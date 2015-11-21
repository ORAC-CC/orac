!-------------------------------------------------------------------------------
! Name: process_broadband_fluxes.F90
!
! Purpose:
! Main program for the radiative flux driver of ORAC-CC4CL. Calls subordinate
! functions to read in data and process.
!
! Subroutines:
!
!   preprocess_bugsrad.F90
!   driver_for_bugsrad.F90
!   interpolate_meteorology.F90
!
! History:
! 2015/10/14, MC: Inital developement
! 2015/11/10, MC: Put into repository
! 2015/11/16, MC: Added compression to NETDF output
! 2015/11/16, MC: Changed NetCDF output to include more digits by using nc_def_var_float_packed_float
! 2015/11/17, MC: TOASWUP can now = 0 without triggering the skipflag.
! 2015/11/18, MC: Output ASCII file with flux profile for the single pixel-optional argument.
! 2015/11/21, GM: Fix, retrflag long_name, standard_name, and units.
!
! $Id$
!
! Bugs:
! None known.
!
! example
! bsub -q lotus -W 24:00 -R "order[-r15s:pg]" -o bugsrad.out -e bugsrad.err -J BUGSrad 
!
! Examples
!
! MODIS
!./process_broadband_fluxes /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/postproc/TEST-L2-CLOUD-CLD-MODIS_ORAC_AQUA_200803200710_V1.0.primary.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/preproc/TEST-L2-CLOUD-CLD-MODIS_ORAC_AQUA_200803200710_V1.0.prtm.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/preproc/TEST-L2-CLOUD-CLD-MODIS_ORAC_AQUA_200803200710_V1.0.alb.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/data/tsi/tsi.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/derived_products/TEST-L2-CLOUD-CLD-MODIS_ORAC_AQUA_200803200710_V1.0.bugsrad.nc
!
! AATSR
!./process_broadband_fluxes /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/postproc/TEST-L2-CLOUD-CLD-AATSR_ORAC_Envisat_200806200846_V1.0.primary.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/preproc/TEST-L2-CLOUD-CLD-AATSR_ORAC_Envisat_200806200846_V1.0.prtm.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/preproc/TEST-L2-CLOUD-CLD-AATSR_ORAC_Envisat_200806200846_V1.0.alb.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/data/tsi_soho_sorce_1978_2015.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/derived_products/TEST-L2-CLOUD-CLD-AATSR_ORAC_Envisat_200806200846_V1.0.bugsrad.nc 182 13487
!
! SEVIRI
!./process_broadband_fluxes /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/postproc/TEST-L2-CLOUD-CLD-SEVIRI_ORAC_MSG2_201004161312_V1.0.primary.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/preproc/TEST-L2-CLOUD-CLD-SEVIRI_ORAC_MSG2_201004161312_V1.0.prtm.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/preproc/TEST-L2-CLOUD-CLD-SEVIRI_ORAC_MSG2_201004161312_V1.0.alb.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/data/tsi_soho_sorce_1978_2015.nc /group_workspaces/cems/cloud_ecv/mchristensen/orac/workspace/output/derived_products/TEST-L2-CLOUD-CLD-SEVIRI_ORAC_MSG2_201004161312_V1.0.bugsradTEST.nc 1500 1500
!
!-------------------------------------------------------------------------------

program process_broadband_fluxes

   use interpol
   use common_constants
   use orac_ncdf
   use global_attributes
   use source_attributes
   use netcdf

   implicit none

   character(path_length) :: Fprimary,FPRTM,FTSI,FALB,fname
   integer :: ncid, i, j, k, dims_var(2)
   logical, parameter :: verbose=.true.
   type(global_attributes_s) :: global_atts
   type(source_attributes_s) :: source_atts

   !Constants
   real(kind=8), parameter :: Runiv = 8314. !universal gas constant
   real(kind=8), parameter :: Rdryair = 287.05 !dry air gas constant
   real(kind=8), parameter :: Rwetair = Runiv / 18. !moist air gas constant
   real(kind=8), parameter :: Rozone = Runiv / 48. !ozone gas constant

   !BUGSrad setup
!   integer, parameter :: NL = 59  !# of vertical layers in radiative flux code must be odd
   integer, parameter :: NL = 29
   integer, parameter :: NLS = NL+1
   real, dimension(NLS) :: pxZ, pxP, pxT, pxQ, pxO3 !pixel level PRTM profiles

   !PRTM FILE
   real, allocatable :: P(:,:,:)  ! Pressure - grid
   real, allocatable :: T(:,:,:)  ! Temperature - grid
   real, allocatable :: H(:,:,:)  ! Height - grid
   real, allocatable :: Q(:,:,:)  ! Humidity - grid
   real, allocatable :: O3(:,:,:) ! Ozone - grid
   real, allocatable :: inP(:)  ! Pressure - interpolated
   real, allocatable :: inT_(:)  ! Temperature - interpolated
   real, allocatable :: inH(:)  ! Height - interpolated
   real, allocatable :: inQ(:)  ! Humidity - interpolated
   real, allocatable :: inO3(:) ! Ozone - interpolated
   real, allocatable :: lon_prtm(:,:)  ! Longitude values         
   real, allocatable :: lat_prtm(:,:)  ! Latitude values
   real, allocatable :: tlon_prtm(:)  ! Longitude values         
   real, allocatable :: tlat_prtm(:)  ! Latitude values
   real, allocatable :: dummy1d(:)
   integer(kind=lint) :: xdim_prtm,ydim_prtm,levdim_prtm !PRTM dimensions
   integer(kind=lint) :: xN,yN !Satellite 1-km dimensions
   integer, dimension(NLS) :: mask_vres !to match PRTM vertical resolution to BUGSrad

   !Albedo FILE
   real, allocatable :: rho_dd(:,:,:)  ! Albedo
   real, allocatable :: alb_abs_ch_numbers(:) !channels used in albedo product
   integer :: ch1ID,ch2ID,ch3ID,ch4ID,ch5ID,ch6ID
   real, parameter :: ch1WT=0.65,ch2WT=0.15,ch3WT=0.05,ch4WT=0.03,ch5WT=0.015,ch6WT=0.005
   integer(kind=lint) :: nc_alb

   !PRIMARY FILE
   real, allocatable :: COT(:,:)  ! Cloud Optical Depth (xp,yp)
   real, allocatable :: REF(:,:)  ! Cloud Effective Radius (xp,yp)
   real, allocatable :: LAT(:,:)  ! Latitude Satellite (xp,yp)
   real, allocatable :: LON(:,:)  ! Longitude Satellite (xp,yp)
   real, allocatable :: CC_TOT(:,:)  ! Cloud Mask (xp,yp)
   real, allocatable :: CTH(:,:)  ! Cloud Top Height (xp,yp)
   real, allocatable :: SOLZ(:,:)  ! Solar zenith angle (xp,yp)
   real, allocatable :: PHASE(:,:)  ! Cloud phase (xp,yp)
   real, allocatable :: CTT(:,:)  ! Cloud top temperature (xp,yp)

   !Total Solar Irriadiance File
   real, allocatable :: TSI_tsi_true_earth(:) !TOTAL SOLAR IRRADIANCE AT EARTH-SUN DISTANCE
   real, allocatable :: TSI_tsi_1au(:) !TOTAL SOLAR IRRADIANCE AT 1-AU
   real, allocatable :: TSI_year(:) !TSI INDEX YEAR
   real, allocatable :: TSI_jday(:) !TSI INDEX Julian Day
   integer(kind=lint)  :: nTSI = 13425

   !Local Pixel-Scale Variables
   integer :: pxYear  !Year
   integer :: pxMonth !Month
   integer :: pxDay   !Day
   real :: pxJday  !JULIAN DAY FROM 1 - 365
   real :: pxTSI   !Total incoming solar irradiance (true-earth)
   real :: pxAsfc  !Surface albedo
   real :: pxAsfcSWR !visible surface albedo channels 1,2,3
   real :: pxAsfcNIR !near-infrared surface albedo channels 4,5,6
   real :: pxTheta !cosine of solar zenith angle
   real :: pxPhase !cloud phase
   real :: pxMask  !cloud mask
   real :: pxCTT   !cloud top temperature
   real :: pxaREF  !aerosol effective radius
   real :: pxAOD  !aerosol optical depth
   real :: pxREF   !cloud effective radius
   real :: pxCOT   !cloud optical depth
   real :: pxCTH   !cloud top height
   real :: pxHctop !input cloud top height
   real :: pxHcbase!input cloud base height
   real :: pxLayerType !aerosol type
   real :: pxPhaseFlag !cloud phase type
   real :: pxts   !land/sea surface temperature
   real pxregime,pxcomputationFlag
   integer pxHctopID(1),pxHcbaseID(1)

   !radiation flux profiles
   real (kind=8), dimension(1,NL) ::  &
      ulwfx   ,&  !all-sky pward longwave flux
      dlwfx   ,&  !all-sky downward longwave flux
      uswfx   ,&  !all-sky upward shortwave flux
      dswfx   ,&  !all-sky downward shortwave flux
      ulwfxclr,&  !clear-sky upward longwave flux
      dlwfxclr,&  !clear-sky downward longwave flux
      uswfxclr,&  !clear-sky upward shortwave flux
      dswfxclr    !clear-sky downward shortwave flux

   !Local Flux & PAR variables
   real ::    &
      pxtoalwup,pxtoaswdn,pxtoaswup ,& !All-sky TOA fluxes
      pxtoalwupclr,pxtoaswdnclr,pxtoaswupclr, & !Clear-Sky TOA fluxes
      pxboalwup,pxboalwdn,pxboaswdn,pxboaswup ,& !All-sky BOA fluxes
      pxboalwupclr,pxboalwdnclr,pxboaswdnclr,pxboaswupclr,& !clear-sky BOA fluxes
      tpar   ,&    !toa par total
      bpardif,&    !boa par diffuse
      bpar   ,&    !boa par total
      bpardir      !boa par direct



   !NETCDF Output geolocation data
   real, allocatable :: lat_data(:,:), lon_data(:,:) !latitude & longitude

   !NETCDF Output TOA & BOA radiation flux data
   real, allocatable :: toa_lwup(:,:) !TOA outgoing LW flux
     integer toa_lwup_vid

   real, allocatable :: toa_swup(:,:) !TOA outgoing SW flux
     integer toa_swup_vid

   real, allocatable :: toa_swdn(:,:) !TOA incoming SW flux
     integer toa_swdn_vid

   real, allocatable :: boa_lwup(:,:) !BOA outgoing LW flux
     integer boa_lwup_vid

   real, allocatable :: boa_lwdn(:,:) !BOA incoming LW flux
     integer boa_lwdn_vid

   real, allocatable :: boa_swup(:,:) !BOA outgoing SW flux
     integer boa_swup_vid

   real, allocatable :: boa_swdn(:,:) !BOA incoming SW flux
     integer boa_swdn_vid

   !NETCDF Output CLEAR-SKY TOA & BOA radiation flux data
   real, allocatable :: toa_lwup_clr(:,:) !TOA outgoing LW flux clear-sky condition
     integer toa_lwup_clr_vid

   real, allocatable :: toa_lwdn_clr(:,:) !TOA incoming LW flux clear-sky condition
     integer toa_lwdn_clr_vid

   real, allocatable :: toa_swup_clr(:,:) !TOA outgoing SW flux clear-sky condition
     integer toa_swup_clr_vid

   real, allocatable :: boa_lwup_clr(:,:) !BOA outgoing LW flux clear-sky condition
     integer boa_lwup_clr_vid

   real, allocatable :: boa_lwdn_clr(:,:) !BOA incoming LW flux clear-sky condition
     integer boa_lwdn_clr_vid

   real, allocatable :: boa_swup_clr(:,:) !BOA outgoing SW flux clear-sky condition
     integer boa_swup_clr_vid

   real, allocatable :: boa_swdn_clr(:,:) !BOA incoming SW flux clear-sky condition
     integer boa_swdn_clr_vid

   !NETCDF Output TOA & BOA PAR radiation flux data
   real, allocatable :: toa_par_tot(:,:) !TOA PAR - total
     integer toa_par_tot_vid

   real, allocatable :: boa_par_tot(:,:) !BOA PAR - total
     integer boa_par_tot_vid

   real, allocatable :: boa_par_dif(:,:) !BOA PAR - diffuse
     integer boa_par_dif_vid

   !NETCDF OUTPUT (lat/lon/time)
      integer LAT_vid
      integer LON_vid
      integer TIME_vid

   integer(kind=1), allocatable :: retrflag(:,:) !regime flag
      integer retrflag_vid

   integer, parameter :: deflate_lv = 9
   logical, parameter :: shuffle_flag = .false.

   !To compress data
   real(kind=sreal) :: temp_real_var
   integer(kind=sint) :: vmin = 0
   integer(kind=sint) :: vmax = 32000
   real(kind=sreal) :: var_scale = 10.0
   real(kind=sreal) :: var_scale_geo = 100.0
   real(kind=sreal) :: var_offset = 0.0

   !NetCDF output dimensions
   integer :: &
      ixstart,ixstop,xstep  ,& ! First and last super-pixel X locations
      iystart,iystop,ystep  ,& ! First and last super-pixel Y locations  
      n_x, n_y

   !debugging
   integer :: nanFlag

   !Pixel selection option
   character(path_length) :: cpxX0,cpxY0,cpxX1,cpxY1
   integer :: pxX0,pxY0,pxX1,pxY1
   integer value
   integer px_processing_mode !=0 all pixels make netCDF, =1 multiple pixels (print to screen), =2 one pixel (print to screen)

   !ECMWF-PRTM for comparing my interpolation scheme to ORAC scheme
   integer :: tlatid,tlonid

   !For reading time from input string
   integer :: index1
   character(len=4) :: cyear
   character(len=2) :: cmonth
   character(len=2) :: cday

!-------------------------------------------------------------------------------
!Get File Names

   !Read manditory arguments (file names)
   call get_command_argument(1, Fprimary)  
   call get_command_argument(2, FPRTM)  
   call get_command_argument(3, FALB)  
   call get_command_argument(4, FTSI)
   call get_command_argument(5, fname)
    print*,'primary file: ',trim(Fprimary)
    print*,'prtm file : ',trim(FPRTM)
    print*,'albedo file: ',trim(FALB)
    print*,'total solar irradiance file: ',trim(FTSI)
    print*,'output file: ',trim(fname)

   !Read optional arguments
   call get_command_argument(6, cpxX0)
   call get_command_argument(7, cpxY0)
   call get_command_argument(8, cpxX1)
   call get_command_argument(9, cpxY1)
    !x-y range of selected pixels
    if(len(trim(cpxX0)) .ne. 0 .and. len(trim(cpxX1)) .ne. 0 .and. &
       len(trim(cpxY0)) .ne. 0 .and. len(trim(cpxY1)) .ne. 0) then
      print*,'PROCESS MULTIPLE SATELLITE PIXELS'
      px_processing_mode=1
      read(cpxX0,*) value
      pxX0=value
      read(cpxX1,*) value
      pxX1=value
      read(cpxY0,*) value
      pxY0=value
      read(cpxY1,*) value
      pxY1=value
    endif

    !single pixel
    if(len(trim(cpxX0)) .ne. 0 .and. len(trim(cpxX1)) .eq. 0 .and. &
       len(trim(cpxY0)) .ne. 0 .and. len(trim(cpxY1)) .eq. 0) then
      print*,'PROCESS SINGLE SATELLITE PIXEL'
      px_processing_mode=2
      read(cpxX0,*) value
      pxX0=value
      pxX1=value
      read(cpxY0,*) value
      pxY0=value
      pxY1=value
    endif

!-------------------------------------------------------------------------------
   !Read time string from file
   index1=index(trim(adjustl(Fprimary)),'.primary',back=.true.)
   cyear=trim(adjustl(Fprimary(index1-17:index1-14)))
   cmonth=trim(adjustl(Fprimary(index1-13:index1-11)))
   cday=trim(adjustl(Fprimary(index1-11:index1-9)))
   read(cyear,'(I4)') value
   pxYear = value
   read(cmonth,'(I2)') value
   pxMonth = value
   read(cday,'(I2)') value
   pxDay = value
   !Get calendar day
   call greg2jul(pxYear,pxMonth,pxDay,pxJday)   
!-------------------------------------------------------------------------------
   ! Open TSI file
   call nc_open(ncid,FTSI)
   
    !Allocate arrays
    allocate(TSI_tsi_true_earth(nTSI))
    allocate(TSI_tsi_1au(nTSI))
    allocate(TSI_year(nTSI))
    allocate(TSI_jday(nTSI))

    !Read primary data
    call nc_read_array(ncid, "tsi_true_earth", TSI_tsi_true_earth, verbose)
    call nc_read_array(ncid, "tsi_1au", TSI_tsi_1au, verbose)
    call nc_read_array(ncid, "year", TSI_year, verbose)
    call nc_read_array(ncid, "jday", TSI_jday, verbose)

    ! Close file
    if (nf90_close(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: read_input_dimensions_lwrtm(): Error closing ' // &
                  'LWRTM file: ', Fprimary
       stop error_stop_code
    end if

    !Get TSI that coincides with input date
    do i=1,nTSI
     if(TSI_year(i) .eq. pxYear .and. TSI_jday(i) .eq. pxJday) pxTSI=TSI_tsi_true_earth(i)
    end do
    print*,'TSI data on date: '
    print*,'YEAR: ',pxYear
    print*,'Calendar Day: ',pxJday
    print*,'TSI = ',pxTSI

!-------------------------------------------------------------------------------

   ! Open PRIMARY file
   call nc_open(ncid,Fprimary)
   
    !Get satellite dimensions
    xN = nc_dim_length(ncid, 'across_track', verbose)
    yN = nc_dim_length(ncid, 'along_track', verbose)    

    !Allocate arrays
    allocate(LAT(xN,yN))
    allocate(LON(xN,yN))
    allocate(COT(xN,yN))
    allocate(REF(xN,yN))
    allocate(CC_TOT(xN,yN))
    allocate(CTH(xN,yN))
    allocate(SOLZ(xN,yN))
    allocate(CTT(xN,yN))
    allocate(PHASE(xN,yN))

    !Read primary data
    call nc_read_array(ncid, "lat", LAT, verbose)
    call nc_read_array(ncid, "lon", LON, verbose)
    call nc_read_array(ncid, "cot", COT, verbose)
    call nc_read_array(ncid, "ref", REF, verbose)
    call nc_read_array(ncid, "cc_total", CC_TOT, verbose)
    call nc_read_array(ncid, "phase", PHASE, verbose)
    call nc_read_array(ncid, "ctt", CTT, verbose)
    call nc_read_array(ncid, "cth", CTH, verbose)
    call nc_read_array(ncid, "solar_zenith_view_no1", SOLZ, verbose)
   
    ! Close file
    if (nf90_close(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: read_input_dimensions_lwrtm(): Error closing ' // &
                  'LWRTM file: ', Fprimary
       stop error_stop_code
    end if
!   PRINT*,'ACROSS_TRACK PIXELS = ',xN
!   PRINT*,'ALONG_TRACK PIXELS = ',yN
!   PRINT*,'LATITUDE ',LAT(pxX,pxY)
!   PRINT*,'LONGITUDE ',LON(pxX,pxY)
!   PRINT*,'CLOUD OPTICAL THICKNESS ',COT(pxX,pxY)
!   PRINT*,'CLOUD EFFECTIVE RADIUS ',REF(pxX,pxY)
!   PRINT*,'CLOUD MASK ',CC_TOT(pxX,pxY)
!   PRINT*,'PHASE ',PHASE(pxX,pxY)
!   PRINT*,'CTT = ',CTT(pxX,pxY)
!   PRINT*,'CTH = ',CTH(pxX,pxY)
!   PRINT*,'SOLZ = ',SOLZ(pxX,pxY)

!-------------------------------------------------------------------------------


   ! Open PRTM file
   call nc_open(ncid,FPRTM)

    !Get PRTM dimensions
    xdim_prtm = nc_dim_length(ncid, 'nlon_rtm', verbose)
    ydim_prtm = nc_dim_length(ncid, 'nlat_rtm', verbose)
    levdim_prtm = nc_dim_length(ncid, 'nlevels_rtm', verbose)
   
    !Allocate arrays
    allocate(P(levdim_prtm, xdim_prtm, ydim_prtm))
    allocate(T(levdim_prtm, xdim_prtm, ydim_prtm))
    allocate(H(levdim_prtm, xdim_prtm, ydim_prtm))
    allocate(Q(levdim_prtm, xdim_prtm, ydim_prtm))
    allocate(O3(levdim_prtm, xdim_prtm, ydim_prtm))
    allocate(lon_prtm(xdim_prtm, ydim_prtm))
    allocate(lat_prtm(xdim_prtm, ydim_prtm))    
    allocate(tlon_prtm(xdim_prtm))
    allocate(tlat_prtm(ydim_prtm))    

    !Read PRTM data
    call nc_read_array(ncid, "pprofile_rtm", P, verbose)
    call nc_read_array(ncid, "tprofile_rtm", T, verbose)
    call nc_read_array(ncid, "hprofile_rtm", H, verbose)
    call nc_read_array(ncid, "qprofile_rtm", Q, verbose)
    call nc_read_array(ncid, "o3profile_rtm", O3, verbose)
    call nc_read_array(ncid, "lon_rtm", tlon_prtm, verbose)
    call nc_read_array(ncid, "lat_rtm", tlat_prtm, verbose)

    !Fill longitude array
    allocate(dummy1d(xdim_prtm))
    call nc_read_array(ncid, "lon_rtm", dummy1d, verbose)
    do i=1,xdim_prtm
       lon_prtm(i,:) = dummy1d(i)
    end do
    deallocate(dummy1d)

    !Fill latitude array
    allocate(dummy1d(ydim_prtm))
    call nc_read_array(ncid, "lat_rtm", dummy1d, verbose)
    do i=1,ydim_prtm
       lat_prtm(i,:) = dummy1d(i)
    end do
    deallocate(dummy1d)

    ! Close file
    if (nf90_close(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: read_input_dimensions_lwrtm(): Error closing ' // &
                  'LWRTM file: ', FPRTM
       stop error_stop_code
    end if

    !Set PRTM units
    H  = (H/9.81)/1000. !to put to km
    Q  = Q*(Rdryair/Rwetair)*(1e-6) !ppmv --> kg/kg
    O3 = O3*(Rdryair/Rozone)*(1e-6) !ppmv --> kg/kg
 
    !call collocate_prtm_profile(LON(pxX0,pxY0),LAT(pxX0,pxY0),&
    !          xdim_prtm,ydim_prtm,tlon_prtm,tlat_prtm,tlonid,tlatid)
    !print*,tlonid,tlatid
    !print*,xdim_prtm,ydim_prtm,levdim_prtm
    !print*,'PRESSURE: ',P(:,tlonid,tlatid)
    !print*,'TEMPERATURE: ',T(:,tlonid,tlatid)
    !print*,'HEIGHT: ',H(:,tlonid,tlatid)
    !print*,'HUMIDITY: ',Q(:,tlonid,tlatid)
    !print*,'Ozone: ',O3(:,tlonid,tlatid)
    !print*,'longitude: ',tlon_prtm(tlonid)
    !print*,'latitude: ',tlat_prtm(tlatid)
    !print*,'satellite longitude: ',LON(pxX0,pxY0)
    !print*,'satellite latitude: ',LAT(pxX0,pxY0)
!-------------------------------------------------------------------------------

   ! Open ALB file
   call nc_open(ncid,FALB)

    !Get #Channels
    nc_alb = nc_dim_length(ncid, 'nc_alb', verbose)
   
    !Allocate arrays
    allocate(rho_dd(xN, yN, nc_alb))
    allocate(alb_abs_ch_numbers(nc_alb))

    !Read ALB data
    call nc_read_array(ncid, "rho_dd_data", rho_dd, verbose)
    call nc_read_array(ncid, "alb_abs_ch_numbers", alb_abs_ch_numbers, verbose)

    ! Close file
    if (nf90_close(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: read_input_dimensions_lwrtm(): Error closing ' // &
                  'LWRTM file: ', FPRTM
       stop error_stop_code
    end if
    
    !Channel indices
    do i=1,nc_alb
      if(alb_abs_ch_numbers(i) .eq. 1) ch1ID=i
      if(alb_abs_ch_numbers(i) .eq. 2) ch2ID=i
      if(alb_abs_ch_numbers(i) .eq. 3) ch3ID=i
      if(alb_abs_ch_numbers(i) .eq. 4) ch4ID=i
      if(alb_abs_ch_numbers(i) .eq. 5) ch5ID=i
      if(alb_abs_ch_numbers(i) .eq. 6) ch6ID=i
    enddo

!-------------------------------------------------------------------------------
! Allocate arrays
!-------------------------------------------------------------------------------

   !Interpolated PRTM arrays
   allocate(inP(levdim_prtm))
   allocate(inT_(levdim_prtm))
   allocate(inH(levdim_prtm))
   allocate(inQ(levdim_prtm))
   allocate(inO3(levdim_prtm))

   !Allocate OUTPUT variables
   allocate(lat_data(xN,yN))
   allocate(lon_data(xN,yN))
   allocate(toa_lwup(xN,yN))
   allocate(toa_swup(xN,yN))
   allocate(toa_swdn(xN,yN))
   allocate(boa_lwup(xN,yN))
   allocate(boa_lwdn(xN,yN))
   allocate(boa_swup(xN,yN))
   allocate(boa_swdn(xN,yN))
   allocate(toa_lwup_clr(xN,yN))
   allocate(toa_lwdn_clr(xN,yN))
   allocate(toa_swup_clr(xN,yN))
   allocate(boa_lwup_clr(xN,yN))
   allocate(boa_lwdn_clr(xN,yN))
   allocate(boa_swup_clr(xN,yN))
   allocate(boa_swdn_clr(xN,yN))
   allocate(toa_par_tot(xN,yN))
   allocate(boa_par_tot(xN,yN))
   allocate(boa_par_dif(xN,yN))
   allocate(retrflag(xN,yN))

    !Fill OUTPUT with missing
    lat_data(:,:) = sint_fill_value
    lon_data(:,:) = sint_fill_value
    toa_lwup(:,:)=sint_fill_value
    toa_swup(:,:) = sint_fill_value
    toa_swdn(:,:) = sint_fill_value
    boa_lwup(:,:) = sint_fill_value
    boa_lwdn(:,:) = sint_fill_value
    boa_swup(:,:) = sint_fill_value
    boa_swdn(:,:) = sint_fill_value
    toa_lwup_clr(:,:) = sint_fill_value
    toa_lwdn_clr(:,:) = sint_fill_value
    toa_swup_clr(:,:) = sint_fill_value
    boa_lwup_clr(:,:) = sint_fill_value
    boa_lwdn_clr(:,:) = sint_fill_value
    boa_swup_clr(:,:) = sint_fill_value
    boa_swdn_clr(:,:) = sint_fill_value
    toa_par_tot(:,:) = sint_fill_value
    boa_par_tot(:,:) = sint_fill_value
    boa_par_dif(:,:) = sint_fill_value
   

   !re-grid PRTM vertical profile to match bugsrad resolution (NLS)
   do i=1,NLS 
    mask_vres(i)=floor(i*(levdim_prtm/(NLS*1.)))
   enddo
    !top and bottom of BUGSrad profile need to be at the same level as PRTM
    mask_vres(1)=1
    mask_vres(NLS)=levdim_prtm
    print*,mask_vres

   !OPTION - PROCESS all pixels in granule if range not specified
   if(len(trim(cpxX0)) .eq. 0 .and. len(trim(cpxX1)) .eq. 0 .and. &
      len(trim(cpxY0)) .eq. 0 .and. len(trim(cpxY1)) .eq. 0) then
      print*,'PROCESS ALL SATELLITE PIXELS'
      px_processing_mode=0
      pxX0=1
      pxX1=xN
      pxY0=1
      pxY1=yN
    endif

!-------------------------------------------------------------------------------
! Loop over each satellite pixel
!-------------------------------------------------------------------------------
    print*,'Processing Pixel Range:'
    print*,'x-start = ',pxX0
    print*,'y-start = ',pxY0
    print*,'x-end = ',pxX1
    print*,'y-end = ',pxY1
    print*,'Across Track # = ',xN
    print*,'Along Track #  = ',yN

   !loop over cross-track dimension
    do i=pxX0,pxX1
      print*,'complete: ',i*100./(xN*1.),'%   i=',i

      !loop over along-track dimension
      do j=pxY0,pxY1
!      do j=pxY0,pxY0+1 !for testing

      !Valid lat/lon required to run (needed for SEVIRI)
      if(LAT(i,j) .ne. -999.0 .and. LON(i,j) .ne. -999.0) then

       !surface albedo
       pxAsfcSWR = (rho_dd(i,j,ch1ID)*ch1WT+rho_dd(i,j,ch2ID)*ch2WT)/(ch1WT+ch2WT)
       pxAsfcNIR = (rho_dd(i,j,ch3ID)*ch3WT+rho_dd(i,j,ch4ID)*ch4WT)/(ch3WT+ch4WT)
         !*note*
         !weights are based on the fraction of planck radiation integrated over the band width
         !based on heritage channels 1,2, shortwave & 3,4 NIR

       !solar zenith angle
       pxTheta = COS( SOLZ(i,j) * Pi/180.)
 
       !meteorology
       call interpolate_meteorology(lon_prtm,lat_prtm,levdim_prtm,&
                               xdim_prtm,ydim_prtm,P,T,H,Q,O3,&
                               LON(i,j),LAT(i,j),inP,inT_,inH,inQ,inO3)

       !use to debug current interpolation scheme
       !call collocate_prtm_profile(LON(i,j),LAT(i,j),&
       !          xdim_prtm,ydim_prtm,tlon_prtm,tlat_prtm,tlonid,tlatid)
        !print*,tlonid,tlatid
        !set value
       ! inH(:)= H(:,tlonid,tlatid)
       ! inT_(:)= T(:,tlonid,tlatid)
       ! inP(:)= P(:,tlonid,tlatid)
       ! inQ(:)= Q(:,tlonid,tlatid)
       ! inO3(:) = O3(:,tlonid,tlatid)
       !call midlatsum1(pxZ,pxP,pxT,pxQ,pxO3,NLS) !standard profile if wanted

       !collocate PRTM vertical resolution to BUGSrad profile resolution (31 levels)
       pxZ = inH(mask_vres)
       pxP = inP(mask_vres)
       pxT = inT_(mask_vres)
       pxQ = inQ(mask_vres)
       pxO3 = inO3(mask_vres)

       !Surface temperature
       pxts = pxT(NLS)

!         print*,'latitude: ',LAT(i,j)
!         print*,'longitude: ',LON(i,j)
!         print*,'Sat Phase: ',PHASE(i,j)
!         print*,'Sat retr. CTH = ',CTH(i,j)
!         print*,'Sat retr. CTT = ',CTT(i,j)
!         print*,'SAT retr. REF = ',REF(i,j)
!         print*,'SAT retr. COT = ',COT(i,j)
!         print*,'SAT retr. cc_tot = ',cc_tot(i,j)

       !cloud base & top height calculation
       call preprocess_bugsrad(cc_tot(i,j),0.,0.,phase(i,j),&
                        CTT(i,j),REF(i,j),COT(i,j),CTH(i,j),&
                        NLS,pxZ,pxREF,pxCOT,pxHctop,pxHcbase,&
                        pxPhaseFlag,pxLayerType,&
                        pxregime,pxcomputationFlag,pxHctopID,pxHcbaseID)

       !for detecting NaN's produced by BUGSrad
       nanFlag=0

       !Run BUGSrad over valid retrieval
!      if(pxcomputationFlag == 1 .and. nanFlag == 0) then

         !debugging (print statements)
!         print*,'phase: ',pxPhaseFlag
!         print*,'re :',pxREF
!         print*,'tau :',pxCOT
!         print*,'Hctop = ',pxHctop,' HctopID: ',pxHctopID
!         print*,'Hcbase = ',pxHcbase,' HcbaseID: ',pxHcbaseID
!         print*,'Regime: ',pxregime      

         call driver_for_bugsrad(NL,pxTSI,pxtheta,pxAsfcSWR,pxAsfcNIR,pxts,&
                          pxPhaseFlag,pxREF,pxCOT,pxHctop,pxHcbase,&
                          pxHctopID,pxHcbaseID,&
                          pxZ,pxP,pxT,pxQ,pxO3,&
                          pxtoalwup,pxtoaswdn,pxtoaswup,&
                          pxboalwup,pxboalwdn,pxboaswdn,pxboaswup,&
                          pxtoalwupclr,pxtoaswupclr,&
                          pxboalwupclr,pxboalwdnclr,pxboaswupclr,pxboaswdnclr,&
                          bpar,bpardif,tpar,&
                          ulwfx,dlwfx,uswfx,dswfx,&
                          ulwfxclr,dlwfxclr,uswfxclr,dswfxclr)

!           print*,pxtoalwup,pxtoaswdn,pxtoaswup
!           print*,pxtoalwupclr,pxtoaswupclr
!           print*,pxboalwup,pxboalwdn,pxboaswdn,pxboaswup
!           print*,pxboalwupclr,pxboalwdnclr,pxboaswdnclr,pxboaswupclr
!           print*,bpar,bpardif
           !if(j .eq. 10) stop

         !catch NaN
         if(pxtoalwup .le. 0. .or. pxtoalwup .gt. 1500.) nanFlag=1
         if(pxtoaswup .lt. 0. .or. pxtoaswup .gt. 1500.) nanFlag=1
         if(pxtoalwupclr .le. 0. .or. pxtoalwupclr .gt. 1500.) nanFlag=1
         if(pxtoaswupclr .lt. 0. .or. pxtoaswupclr .gt. 1500.) nanFlag=1

         !regime type
         retrflag(i,j) = pxregime

         !valid data only
         if(nanFlag == 0) then
          !netCDF output arrays
          !Observed
          lat_data(i,j) = LAT(i,j)
          lon_data(i,j) = LON(i,j)

          toa_lwup(i,j) = pxtoalwup
          toa_swup(i,j) = pxtoaswup
          toa_swdn(i,j) = pxtoaswdn
          boa_lwup(i,j) = pxboalwup
          boa_lwdn(i,j) = pxboalwdn
          boa_swup(i,j) = pxboaswup
          boa_swdn(i,j) = pxboaswdn

          !Clear-sky retrieval
          toa_lwup_clr(i,j) = pxtoalwupclr
          toa_swup_clr(i,j) = pxtoaswupclr
          boa_lwup_clr(i,j) = pxboalwupclr
          boa_lwdn_clr(i,j) = pxboalwdnclr
          boa_swup_clr(i,j) = pxboaswupclr
          boa_swdn_clr(i,j) = pxboaswdnclr

          !PAR
          toa_par_tot(i,j) = tpar
          boa_par_tot(i,j) = bpar
          boa_par_dif(i,j) = bpardif

         endif !valid data
!       endif !user-defined
      endif   !valid geolocation data
    enddo !j-loop
   enddo !i-loop


!OPTIONAL - print to screen for selected satellite pixel/s
if(px_processing_mode .eq. 1 .or. px_processing_mode .eq. 2) then
 do i=pxX0,pxX1
 do j=pxY0,pxY1
  print*,'i = ',i,' j = ',j
  print*,'toa_swdn    : ',toa_swdn(i,j)
  print*,'toa_lwup    : ',toa_lwup(i,j)
  print*,'toa_lwup_clr: ',toa_lwup_clr(i,j)
  print*,'toa_swup    : ',toa_swup(i,j)
  print*,'toa_swup_clr: ',toa_swup_clr(i,j)
  print*,'toa_albedo  : ',toa_swup(i,j)/toa_swdn(i,j)
  print*,'boa_par_tot : ',boa_par_tot(i,j)
  print*,'boa_par_dif : ',boa_par_dif(i,j)

  !write profile of last pixel to ascii file 
  !(to write out more than 1 pixel would take tremendous memory)
  open(unit=1,file=trim(fname),status="new",action="write")
   write(unit=1, fmt=*) " Fluxes   Plev       SW_DN       SW_UP       LW_DN       LW_UP"
   write(unit=1, fmt=*) "            Pa       W/m^2       W/m^2       W/m^2       W/m^2"
  do k=1,NLS
    write(unit=1, fmt="(I4,5(F12.3))") k,pxP(k),dswfx(1,k),uswfx(1,k),dlwfx(1,k),ulwfx(1,k)
  enddo
  close(unit=1)

 enddo
 enddo
endif

!-------------------------------------------------------------------------
!Make output netcdf file
!-------------------------------------------------------------------------
!make only if processing all satellite-pixels
if(px_processing_mode .eq. 0) then

call nc_open(ncid,Fprimary)

!get common attributes from primary file
call nc_get_common_attributes(ncid, global_atts, source_atts)

if (nf90_close(ncid) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_close()'
         stop error_stop_code
end if


   !dimensions
   ixstart = 1
   ixstop = xN
   iystart = 1
   iystop = yN
   n_x = ixstop - ixstart + 1
   n_y = iystop - iystart + 1


  ! create netcdf file
   call nc_create(trim(fname), ncid, ixstop-ixstart+1, &
      iystop-iystart+1, dims_var, 1, global_atts, source_atts)

      !Need this to exit data mode to define variables
      if (nf90_redef(ncid) .ne. NF90_NOERR) then
        write(*,*) 'ERROR: nf90_redef()'
        stop error_stop_code
      end if

      !-------------------------------------------------------------------------
      ! latitude
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'lat', &
               LAT_vid, &
               verbose, &
               long_name     = 'latitude', &
               standard_name = 'latitude', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(-90., sreal), &
               valid_max     = real(90., sreal), &
               units         = 'degrees', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)


      !-------------------------------------------------------------------------
      ! longitude
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'lon', &
               LON_vid, &
               verbose, &
               long_name     = 'longitude', &
               standard_name = 'longitude', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(-180, sreal), &
               valid_max     = real(180, sreal), &
               units         = 'degrees', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! retrieval flag
      !-------------------------------------------------------------------------
       call nc_def_var_short_packed_float( &
               ncid, &
               dims_var, &
               'retrflag', &
               retrflag_vid, &
               verbose, &
               long_name     = 'retrflag', &
               standard_name = 'retrflag', &
               fill_value    = sint_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = int(1, sint), &
               valid_max     = int(4, sint), &
               units         = '1', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! toa_incoming_shortwave_flux
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'toa_swdn', &
               toa_swdn_vid, &
               verbose, &
               long_name     = 'top of atmosphere incident solar radiation', &
               standard_name = 'toa_incoming_shortwave_flux', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! toa_outgoing_shortwave_flux
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'toa_swup', &
               toa_swup_vid, &
               verbose, &
               long_name     = 'top of atmosphere upwelling solar radiation', &
               standard_name = 'toa_outgoing_shortwave_flux', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! toa_outgoing_longwave_flux
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'toa_lwup', &
               toa_lwup_vid, &
               verbose, &
               long_name     = 'top of atmosphere upwelling thermal radiation', &
               standard_name = 'toa_outgoing_longwave_flux', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)



      !-------------------------------------------------------------------------
      ! surface_downwelling_shortwave_flux_in_air
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_swdn', &
               boa_swdn_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere downwelling solar radiation', &
               standard_name = 'surface_downwelling_shortwave_flux_in_air', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! surface_upwelling_shortwave_flux_in_air
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_swup', &
               boa_swup_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere upwelling solar radiation', &
               standard_name = 'surface_upwelling_shortwave_flux_in_air', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! surface_upwelling_longwave_flux_in_air
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_lwup', &
               boa_lwup_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere upwelling thermal radiation', &
               standard_name = 'surface_upwelling_longwave_flux_in_air', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)


      !-------------------------------------------------------------------------
      ! surface_downwelling_longwave_flux_in_air
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_lwdn', &
               boa_lwdn_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere downwelling thermal radiation', &
               standard_name = 'surface_downwelling_longwave_flux_in_air', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)



      !-------------------------------------------------------------------------
      ! toa_outgoing_shortwave_flux_assuming_clear_sky
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'toa_swup_clr', &
               toa_swup_clr_vid, &
               verbose, &
               long_name     = 'top of atmosphere upwelling solar radiation', &
               standard_name = 'toa_outgoing_shortwave_flux_assuming_clear_sky', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! toa_outgoing_longwave_flux_assuming_clear_sky
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'toa_lwup_clr', &
               toa_lwup_clr_vid, &
               verbose, &
               long_name     = 'top of atmosphere upwelling thermal radiation', &
               standard_name = 'toa_outgoing_longwave_flux_assuming_clear_sky', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)


      !-------------------------------------------------------------------------
      ! boa_downwelling_shortwave_flux_in_air_assuming_clear_sky
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_swdn_clr', &
               boa_swdn_clr_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere downwelling solar radiation', &
               standard_name = 'surface_downwelling_shortwave_flux_in_air_assuming_clear_sky', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! boa_upwelling_shortwave_flux_in_air_assuming_clear_sky
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_swup_clr', &
               boa_swup_clr_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere upwelling solar radiation', &
               standard_name = 'surface_upwelling_shortwave_flux_in_air_assuming_clear_sky', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! boa_upwelling_longwave_flux_in_air_assuming_clear_sky
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_lwup_clr', &
               boa_lwup_clr_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere upwelling thermal radiation', &
               standard_name = 'surface_upwelling_longwave_flux_in_air_assuming_clear_sky', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)


      !-------------------------------------------------------------------------
      ! boa_downwelling_longwave_flux_in_air_assuming_clear_sky
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_lwdn_clr', &
               boa_lwdn_clr_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere downwelling thermal radiation', &
               standard_name = 'surface_downwelling_longwave_flux_in_air_assuming_clear_sky', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)



      !-------------------------------------------------------------------------
      ! surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_par_dif', &
               boa_par_dif_vid, &
               verbose, &
               long_name     = 'surface diffuse par', &
               standard_name = 'surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)


      !-------------------------------------------------------------------------
      ! surface_downwelling_photosynthetic_radiative_flux_in_air
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_par_tot', &
               boa_par_tot_vid, &
               verbose, &
               long_name     = 'surface total par', &
               standard_name = 'surface_downwelling_photosynthetic_radiative_flux_in_air', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! toa_incoming_photosynthetic_radiative_flux
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'toa_par_tot', &
               toa_par_tot_vid, &
               verbose, &
               long_name     = 'top of atmosphere incident par', &
               standard_name = 'toa_incoming_photosynthetic_radiative_flux', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'W m-2', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)



     !Need to exit define mode to write data
      if (nf90_enddef(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: nf90_enddef()'
       stop error_stop_code
      end if


     !write the array to the netcdf file
     call nc_write_array(ncid,'lat',LAT_vid,&
             lat_data(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'lon',LON_vid,&
             lon_data(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'retrflag',retrflag_vid,&
             retrflag(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'toa_swdn',toa_swdn_vid,&
             toa_swdn(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'toa_swup',toa_swup_vid,&
             toa_swup(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'toa_lwup',toa_lwup_vid,&
             toa_lwup(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_swdn',boa_swdn_vid,&
             boa_swdn(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_swup',boa_swup_vid,&
             boa_swup(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_lwup',boa_lwup_vid,&
             boa_lwup(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_lwdn',boa_lwdn_vid,&
             boa_lwdn(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'toa_swup_clr',toa_swup_clr_vid,&
             toa_swup_clr(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'toa_lwup_clr',toa_lwup_clr_vid,&
             toa_lwup_clr(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_swdn_clr',boa_swdn_clr_vid,&
             boa_swdn_clr(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_swup_clr',boa_swup_clr_vid,&
             boa_swup_clr(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_lwup_clr',boa_lwup_clr_vid,&
             boa_lwup_clr(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_lwdn_clr',boa_lwdn_clr_vid,&
             boa_lwdn_clr(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_par_dif',boa_par_dif_vid,&
             boa_par_dif(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_par_tot',boa_par_tot_vid,&
             boa_par_tot(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'toa_par_tot',toa_par_tot_vid,&
             toa_par_tot(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     !close netcdf file
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_close()'
         stop error_stop_code
      end if

print*,'CREATED:'
print*,TRIM(fname)
endif !netCDF mode

end program process_broadband_fluxes
