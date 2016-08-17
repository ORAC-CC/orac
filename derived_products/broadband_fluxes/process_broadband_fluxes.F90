!-------------------------------------------------------------------------------
! Name: process_broadband_fluxes.F90
!
! Purpose:
! Main program for the radiative flux driver of ORAC-CC4CL. Calls subordinate
! functions to read in data and process.
!
! Inputs:
!  Required
!   1) Primary file (post-processed for water & ice)
!   2) PRTM file (pre-processing)
!   3) ALB file (pre-processing)
!   4) TSI file (http://proj.badc.rl.ac.uk/svn/orac/data/tsi_soho_sorce_1978_2015.nc)
!   5) Output filename (user specified)
!   6) Radiation Algorithm (BUGSrad: '1'; FuLiou: '2')
!  Optional inputs:
!   7) FuLiou Solver ('1' 4-stream, '2' 2-Stream Modified gamma, '3' 2-Stream)
!   8) Aerosol CCI file (needs to coincide with cloud file) --> use '' to skip
!   9) Collocated aerosol-cloud netcdf file (user specified) --> use '' to skip
!  Options 10,11,12,13 are to process individual or multiple 1km pixels
!   10) x0, 11) y0, 12) x1, 13) y1
!
! Subroutines:
!   interpolate_meteorology.F90
!   compute_lts.F90
!   compute_fth.F90
!   compute_column_o3.F90
!   preprocess_input.F90
!   preprocess_bugsrad_sfc_albedo.F90
!   preprocess_bugsrad_sfc_emissivity.F90
!   preprocess_fuliou_sfc_albedo.F90
!   driver_for_fuliou.F90
!   driver_for_bugsrad.F90
!
! History:
! 2015/10/14, MC: Initial development
! 2015/11/10, MC: Put into repository
! 2015/11/16, MC: Added compression to NETDF output
! 2015/11/16, MC: Changed NetCDF output to include more digits by using
!    nc_def_var_float_packed_float
! 2015/11/17, MC: TOASWUP can now = 0 without triggering the skipflag.
! 2015/11/18, MC: Output ASCII file with flux profile for the single pixel-
!    optional argument.
! 2015/11/21, GM: Fix retrflag long_name, standard_name, and units.
! 2015/11/21, GM: Range checking is not a valid way to check for nan.  The
!    result was nans getting into the output.  Use Fortran intrinsic isnan().
! 2015/11/22, GM: Fixed retrflag output.  It was defined as a float when it is
!    actually a byte and it was not being initialized leading to garbage output.
!    Also set the flag_values and flag meanings attributes.
! 2015/12/10, MC: Removed nighttime and twilight retrievals (solar zenith angle < 80)
! 2015/12/21, MC: Added optional argument and collocation routine to process
!    radiative fluxes using aerosol cci data.
! 2016/01/12, MC: Added Liang (2000) surface albedo model which converts narrowband
!    albedo radiances for each channel into broadband albedo for direct visible,
!    diffuse visible, direct near-IR, and diffuse-IR as required inputs for BUGSrad.
! 2016/02/19, MC: Added meteorological variables (surface temperature, pressure,
!    humidity, LTS, FTH, & column ozone) to output file.
! 2016/02/19, MC: Fixed bug in reading time from input file name. Index is now based
!    the last underscore in primary file instead of the word primary.
! 2016/03/31, MC: Added time variable to output file.
! 2016/04/06, MC: Modified aerosol_processing option to bypass collocation file creation.
! 2016/07/20, WJ: Added surface temperature from ORAC retrieval and uses this instead of
!    ECMWF skin temperature where possible.
! 2016/08/03, MC: Added the Fu-Liou broadband radiative flux code and adapted procedures
!    to the respository.Code from: www-cave.larc.nasa.gov/ Edition 4 January 12th 2015.
!    New option to run the ORAC brodband flux code using BUGSrad: 1 or Fu_Liou: 2.
! 2016/08/10, MC: Debugged Fu Liou code so that clear-sky pixels can run in the retreival.
!    Set limits to droplet effective radius (re < 30 um) for liquid clouds only in Fu Liou.
! 2016/08/15, MC: Arrays were incorrectly being assigned integer fill value instead of real.
! 2016/08/15, MC: Changed output time array from float to double.
! 2016/08/16, MC: Modified code to restrict size of output netCDF file by the optional input
!                 pixel selection range (pxX0,pxY0,pxX1,pxY1).
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

program process_broadband_fluxes

   use common_constants_m
   use orac_ncdf_m
   use global_attributes_m
   use source_attributes_m
   use system_utils_m
   use netcdf

   implicit none

   character(path_length) :: Fprimary,FPRTM,FTSI,FALB,fname,FLXalgorithm,Faerosol,Fcollocation
   integer algorithm_processing_mode !1-BUGSrad, 2-FuLiou
   integer :: ncid, i, j, k, dims_var(2), dim3d_var(3)
   logical, parameter :: verbose=.true.
   logical there
   type(global_attributes_t) :: global_atts
   type(source_attributes_t) :: source_atts

   !Fu Liou optional input
   character(path_length) :: Fsolver
   integer fu_solver_mode !1=4-stream, 2-stream gamma weighted, 2-stream

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
   real, allocatable :: rho_0d(:,:,:)  ! Albedo direct directional - blacksky albedo
   real, allocatable :: rho_dd(:,:,:)  ! Albedo diffuse directional - whitesky albedo
   real, allocatable :: emis_data(:,:,:)  ! Emissivity
   real, allocatable :: alb_abs_ch_numbers(:) !channels used in albedo product
   real, allocatable :: emis_abs_ch_numbers(:) !channels used in emissivity product
   integer :: ch1ID,ch2ID,ch3ID,ch4ID
   integer(kind=lint) :: nc_alb
   integer(kind=lint) :: nc_emis

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
   real, allocatable :: CTP(:,:)  ! Cloud top pressure (xp,yp)
   real(kind=dreal), allocatable :: TIME(:,:)  ! Time of pixel in Julian Days (xp,yp)
   real, allocatable :: STEMP(:,:)  ! Surface temperature from primary files (xp,yp)

   !Total Solar Irriadiance File
   real, allocatable :: TSI_tsi_true_earth(:) !TOTAL SOLAR IRRADIANCE AT EARTH-SUN DISTANCE
   real, allocatable :: TSI_tsi_1au(:) !TOTAL SOLAR IRRADIANCE AT 1-AU
   real, allocatable :: TSI_year(:) !TSI INDEX YEAR
   real, allocatable :: TSI_jday(:) !TSI INDEX Julian Day
   integer(kind=lint)  :: nTSI = 13425

   !AEROSOL CCI File (optional)
   real, allocatable :: aerLon(:)  ! longitude
   real, allocatable :: aerLat(:)  ! latitude
   real, allocatable :: aerAOD(:)  ! aerosol optical depth
   real, allocatable :: aerREF(:)  ! aerosol effective radius
   real, allocatable :: aerQflag(:)! q-flag
   real, allocatable :: AOD550(:,:)! Aerosol optical depth at 1 km resolution
   real, allocatable :: AREF(:,:)  ! Aerosol Effective Radius 1 km resolution
   integer, allocatable :: aID(:,:)   ! aerosol index for i,jth locations in cloud file
   integer(kind=lint) :: nc_aer
      integer aID_vid


   !Local Pixel-Scale Variables
   integer :: pxYear  !Year
   integer :: pxMonth !Month
   integer :: pxDay   !Day
   real :: pxJday  !JULIAN DAY FROM 1 - 365
   real :: pxTSI   !Total incoming solar irradiance (true-earth)
   real :: pxAsfc  !Surface albedo
   real :: pxAsfcSWRdr !DIRECT visible surface albedo
   real :: pxAsfcNIRdr !DIRECT near-infrared surface albedo
   real :: pxAsfcSWRdf !DIFFUSE visible surface albedo
   real :: pxAsfcNIRdf !DIFFUSE near-infrared surface albedo
   real :: pxTheta !cosine of solar zenith angle
   real :: pxPhase !cloud phase
   real :: pxMask  !cloud mask
   real :: pxCTT   !cloud top temperature
   real :: pxCTP   !cloud top PRESSURE
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
   real pxregime
   integer pxHctopID(1),pxHcbaseID(1)
   real :: pxLTS
   real :: pxFTH
   real :: pxcolO3
   real :: rho_0d_bugsrad(6),rho_dd_bugsrad(6),emis_bugsrad(12)
   real :: rho_0d_fuliou(18),rho_dd_fuliou(18),emis_fuliou(12)

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
   real(kind=dreal), allocatable :: time_data(:,:)
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

   !NETCDF Output METEOROLOGICAL VARIABLES
   real, allocatable :: boa_tsfc(:,:) !BOA temperature
     integer boa_tsfc_vid

   real, allocatable :: boa_psfc(:,:) !BOA pressure
     integer boa_psfc_vid

   real, allocatable :: boa_qsfc(:,:) !BOA vapour pressure
     integer boa_qsfc_vid

   real, allocatable :: lts(:,:) !LTS (LOWER TROPOSPHERE STABILITY)
     integer lts_vid

   real, allocatable :: fth(:,:) !FTH (FREE TROPOSPHERE HUMIDITY 850 hPa)
     integer fth_vid

   real, allocatable :: colO3(:,:) !Column Ozone
     integer colO3_vid

   !NETCDF OUTPUT (lat/lon/time)
      integer LAT_vid
      integer LON_vid
      integer TIME_vid

   integer(kind=byte), allocatable :: retrflag(:,:) !regime flag
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
      n_x, n_y, n_v

   !debugging
   integer :: nanFlag

   !Pixel selection option
   character(path_length) :: cpxX0,cpxY0,cpxX1,cpxY1
   integer :: pxX0,pxY0,pxX1,pxY1
   integer value
   integer aerosol_processing_mode !=0 no processing, =1 collocate aerosol cci file, =2 collocate & save file

   !ECMWF-PRTM for comparing my interpolation scheme to ORAC scheme
   integer :: tlatid,tlonid

   !For reading time from input string
   integer :: index1
   character(len=4) :: cyear
   character(len=2) :: cmonth
   character(len=2) :: cday

   !For CPU processing time
   real :: cpuStart,cpuFinish

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

   call get_command_argument(6, FLXalgorithm)
    read(flxAlgorithm,*) value
    algorithm_processing_mode=value
    if(algorithm_processing_mode .eq. 1) print*,'Algorithm: BUGSrad'
    if(algorithm_processing_mode .eq. 2) print*,'Algorithm: FuLiou'

   !Read optional arguments
   call get_command_argument(7, Fsolver)
    if(len(trim(Fsolver)) .ne. 0) then
      read(Fsolver,*) value
      fu_solver_mode = value
      if(fu_solver_mode .eq. 1) print*,'4-stream solution'
      if(fu_solver_mode .eq. 2) print*,'2-stream Mod. Gamma Solution'
      if(fu_solver_mode .eq. 3) print*,'2-stream solution'
    endif

   call get_command_argument(8, Faerosol)
    aerosol_processing_mode = 0
    if(len(trim(Faerosol)) .gt. 1.) aerosol_processing_mode = 1 !collocate aerosol2cloud

   call get_command_argument(9, Fcollocation)
    if(len(trim(Fcollocation)) .gt. 1.) aerosol_processing_mode = 2 !collocate & save file

   call get_command_argument(10, cpxX0)
   call get_command_argument(11, cpxY0)
   call get_command_argument(12, cpxX1)
   call get_command_argument(13, cpxY1)
    !x-y range of selected pixels
    if(len(trim(cpxX0)) .ne. 0 .and. len(trim(cpxX1)) .ne. 0 .and. &
       len(trim(cpxY0)) .ne. 0 .and. len(trim(cpxY1)) .ne. 0) then
      print*,'PROCESS MULTIPLE SATELLITE PIXELS'
      read(cpxX0,*) value
      pxX0=value*1
      read(cpxX1,*) value
      pxX1=value*1
      read(cpxY0,*) value
      pxY0=value*1
      read(cpxY1,*) value
      pxY1=value*1
      print*,pxX0,pxX1,pxY0,pxY1
    endif

    !single pixel
    if(len(trim(cpxX0)) .ne. 0 .and. len(trim(cpxX1)) .eq. 0 .and. &
       len(trim(cpxY0)) .ne. 0 .and. len(trim(cpxY1)) .eq. 0) then
      print*,'PROCESS SINGLE SATELLITE PIXEL'
      read(cpxX0,*) value
      pxX0=value
      pxX1=value
      read(cpxY0,*) value
      pxY0=value
      pxY1=value
    endif
!-------------------------------------------------------------------------------
   !Read time string from file
   index1=index(trim(adjustl(Fprimary)),'_',back=.true.)
    cyear=trim(adjustl(Fprimary(index1-12:index1-9)))
    cmonth=trim(adjustl(Fprimary(index1-8:index1-6)))
    cday=trim(adjustl(Fprimary(index1-6:index1-4)))
    print*,cyear
    print*,cmonth
    print*,cday
   read(cyear,'(I4)') value
   pxYear = value
   read(cmonth,'(I2)') value
   pxMonth = value
   read(cday,'(I2)') value
   pxDay = value
   !Get calendar day
   call greg2jul(pxYear,pxMonth,pxDay,pxJday)
   print*,pxYear,pxMonth,pxDay,pxJday
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
    allocate(CTP(xN,yN))
    allocate(PHASE(xN,yN))
    allocate(TIME(xN,yN))
    allocate(STEMP(xN,yN))

    !Read primary data
    call nc_read_array(ncid, "time", TIME, verbose)
    call nc_read_array(ncid, "lat", LAT, verbose)
    call nc_read_array(ncid, "lon", LON, verbose)
    call nc_read_array(ncid, "cot", COT, verbose)
    call nc_read_array(ncid, "cer", REF, verbose)
    call nc_read_array(ncid, "cc_total", CC_TOT, verbose)
    call nc_read_array(ncid, "phase", PHASE, verbose)
    call nc_read_array(ncid, "ctt", CTT, verbose)
    call nc_read_array(ncid, "ctp", CTP, verbose)
    call nc_read_array(ncid, "cth", CTH, verbose)
    call nc_read_array(ncid, "solar_zenith_view_no1", SOLZ, verbose)
    call nc_read_array(ncid, "stemp", STEMP, verbose)

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
    nc_emis = nc_dim_length(ncid, 'nc_emis', verbose)

    !Allocate arrays
    allocate(rho_dd(xN, yN, nc_alb))
    allocate(rho_0d(xN, yN, nc_alb))
    allocate(emis_data(xN, yN, nc_emis))
    allocate(alb_abs_ch_numbers(nc_alb))
    allocate(emis_abs_ch_numbers(nc_emis))

    !Read ALB data
    call nc_read_array(ncid, "rho_dd_data", rho_dd, verbose)
    call nc_read_array(ncid, "rho_0d_data", rho_0d, verbose)
    call nc_read_array(ncid, "emis_data", emis_data, verbose)
    call nc_read_array(ncid, "alb_abs_ch_numbers", alb_abs_ch_numbers, verbose)
    call nc_read_array(ncid, "emis_abs_ch_numbers", emis_abs_ch_numbers, verbose)

    ! Close file
    if (nf90_close(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: read_input_dimensions_lwrtm(): Error closing ' // &
                  'LWRTM file: ', FALB
       stop error_stop_code
    end if

!-------------------------------------------------------------------------------

  ! Open Aerosol CCI file (optional)
  if(aerosol_processing_mode .ge. 1) then
   call nc_open(ncid,Faerosol)

    !Get dimension
    nc_aer = nc_dim_length(ncid, 'pixel_number', verbose)

    !Allocate arrays
    allocate(aerLon(nc_aer))
    allocate(aerLat(nc_aer))
    allocate(aerAOD(nc_aer))
    allocate(aerREF(nc_aer))
    allocate(aerQflag(nc_aer))

    !Read Aerosol data
    call nc_read_array(ncid, "longitude", aerLon, verbose)
    call nc_read_array(ncid, "latitude", aerLat, verbose)
    call nc_read_array(ncid, "AOD550", aerAOD, verbose)
    call nc_read_array(ncid, "REFF", aerREF, verbose)
    call nc_read_array(ncid, "quality_flag", aerQflag, verbose)

    ! Close file
    if (nf90_close(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: ',Faerosol
       stop error_stop_code
    end if
  end if

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
   allocate(time_data(xN,yN))
   allocate(lat_data(xN,yN))
   allocate(lon_data(xN,yN))
   allocate(retrflag(xN,yN))
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
   allocate(boa_tsfc(xN,yN))
   allocate(boa_psfc(xN,yN))
   allocate(boa_qsfc(xN,yN))
   allocate(lts(xN,yN))
   allocate(fth(xN,yN))
   allocate(colO3(xN,yN))

    !Fill OUTPUT with missing
    time_data(:,:) = dreal_fill_value
    lat_data(:,:)  = sreal_fill_value
    lon_data(:,:)  = sreal_fill_value
    retrflag(:,:)  = byte_fill_value
    toa_lwup(:,:)  = sreal_fill_value
    toa_swup(:,:)  = sreal_fill_value
    toa_swdn(:,:)  = sreal_fill_value
    boa_lwup(:,:)  = sreal_fill_value
    boa_lwdn(:,:)  = sreal_fill_value
    boa_swup(:,:)  = sreal_fill_value
    boa_swdn(:,:)  = sreal_fill_value
    toa_lwup_clr(:,:) = sreal_fill_value
    toa_lwdn_clr(:,:) = sreal_fill_value
    toa_swup_clr(:,:) = sreal_fill_value
    boa_lwup_clr(:,:) = sreal_fill_value
    boa_lwdn_clr(:,:) = sreal_fill_value
    boa_swup_clr(:,:) = sreal_fill_value
    boa_swdn_clr(:,:) = sreal_fill_value
    toa_par_tot(:,:)  = sreal_fill_value
    boa_par_tot(:,:)  = sreal_fill_value
    boa_par_dif(:,:)  = sreal_fill_value
    boa_tsfc(:,:) = sreal_fill_value
    boa_psfc(:,:) = sreal_fill_value
    boa_qsfc(:,:) = sreal_fill_value

   !re-grid PRTM vertical profile to match bugsrad resolution (NLS)
   do i=1,NLS
    mask_vres(i)=floor(i*(levdim_prtm/(NLS*1.)))
   end do
    !top and bottom of BUGSrad profile need to be at the same level as PRTM
    mask_vres(1)=1
    mask_vres(NLS)=levdim_prtm
    print*,mask_vres

!-------------------------------------------------------------------------
!OPTIONAL INPUTS
!-------------------------------------------------------------------------
   !OPTION - PROCESS all pixels in granule if range not specified
   if(len(trim(cpxX0)) .eq. 0 .and. len(trim(cpxX1)) .eq. 0 .and. &
      len(trim(cpxY0)) .eq. 0 .and. len(trim(cpxY1)) .eq. 0) then
      print*,'PROCESS ALL SATELLITE PIXELS'
      pxX0=1
      pxX1=xN
      pxY0=1
      pxY1=yN
    end if

   allocate(AOD550(xN,yN))
   allocate(AREF(xN,yN))
   !OPTION - PROCESS aerosol
   PRINT*,'aerosol_processing_mode: ',aerosol_processing_mode
   if(aerosol_processing_mode .ge. 1) then
    allocate(aID(xN,yN))
    !determine if netcdf file already exists
    inquire( file=Fcollocation, exist=there )
    if(.not. there) then
     call collocate_aerosol2cloud(nc_aer,aerLon,aerLat,xN,yN,LON,LAT,aID)

     !-------------------------------------------------------------------------
     !Make collocation netcdf file
     !-------------------------------------------------------------------------
      if(aerosol_processing_mode .eq. 2) then
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
        n_v = 1
       ! create netcdf file
        call nc_create(trim(Fcollocation), ncid, ixstop-ixstart+1, &
           iystop-iystart+1, n_v, dim3d_var, 1, global_atts, source_atts)
        dims_var = dim3d_var(1:2)

           !Need this to exit data mode to define variables
           if (nf90_redef(ncid) .ne. NF90_NOERR) then
             write(*,*) 'ERROR: nf90_redef()'
             stop error_stop_code
           end if
         !-------------------------------------------------------------------------
         ! Aerosol Index
         !-------------------------------------------------------------------------
          call nc_def_var_long_packed_long( &
                  ncid, &
                  dims_var, &
                  'aID', &
                  aID_vid, &
                  verbose, &
                  long_name     = 'aerosol 10-km pixel index location', &
                  standard_name = 'aID_standard', &
                  fill_value    = lint_fill_value)

         !Need to exit define mode to write data
          if (nf90_enddef(ncid) .ne. NF90_NOERR) then
           write(*,*) 'ERROR: nf90_enddef()'
           stop error_stop_code
          end if


         !write the array to the netcdf file
         call nc_write_array(ncid,'aID',aID_vid,&
                 aID(ixstart:,iystart:),1,1,n_x,1,1,n_y)

         !close netcdf file
          if (nf90_close(ncid) .ne. NF90_NOERR) then
             write(*,*) 'ERROR: nf90_close()'
             stop error_stop_code
          end if

         print*,'CREATED: '
         print*,Fcollocation
       end if ;aerosol_processing_mode = 2
    end if !file not there

   !Netcdf collocation file exists get aID
    if(there) then
      print*,'Extracting data from:'
      write(*,*) trim(Fcollocation)
     ! Open Collocation file
     call nc_open(ncid,Fcollocation)

     !Read collocation data
     call nc_read_array(ncid, "aID", aID, verbose)

     ! Close file
      if (nf90_close(ncid) .ne. NF90_NOERR) then
        write(*,*) 'ERROR:  ' // &
                   'LWRTM file: ', Fcollocation
        stop error_stop_code
      end if
    end if !file there

    !fill arrays (aerosol index aID must exist by this point)
    do i=1,xN
    do j=1,yN
      AOD550(i,j) = aerAOD(aID(i,j))
      AREF(i,j) = aerREF(aID(i,j))
    end do
    end do

   end if !end aerosol collocation option
   if(aerosol_processing_mode .eq. 0) then
    !fill arrays
    do i=1,xN
    do j=1,yN
      AOD550(i,j) = -999.
      AREF(i,j) = -999.
    end do
    end do
   end if

   if(algorithm_processing_mode .eq. 1) print*,'Algorithm: BUGSrad'
   if(algorithm_processing_mode .eq. 2) print*,'Algorithm: FuLiou'
   if(fu_solver_mode .eq. 1) print*,'4-stream solution'
   if(fu_solver_mode .eq. 2) print*,'2-stream Mod. Gamma Solution'
   if(fu_solver_mode .eq. 3) print*,'2-stream solution'

!-------------------------------------------------------------------------
!END OPTIONAL INPUTS SECTION
!-------------------------------------------------------------------------





!-------------------------------------------------------------------------
!BEGIN MAIN CODE
!-------------------------------------------------------------------------
call cpu_time(cpuStart)

    print*,'Processing Pixel Range:'
    print*,'x-start = ',pxX0
    print*,'y-start = ',pxY0
    print*,'x-end = ',pxX1
    print*,'y-end = ',pxY1
    print*,'Across Track # = ',xN
    print*,'Along Track #  = ',yN

   !loop over cross-track dimension
    do i=pxX0,pxX1
      call cpu_time(cpuFinish)
      print*,'complete: ',i*100./(xN*1.),'%   i=',i, cpuFinish-cpuStart,' seconds elapsed'

      !loop over along-track dimension
      do j=pxY0,pxY1

      !Valid lat/lon required to run (needed for SEVIRI)
      if(LAT(i,j) .ne. -999.0 .and. LON(i,j) .ne. -999.0) then


       !---------------------------------------------------------
       ! Surface albedo
       ! interpolate narrowband BRDF radiances to broadband BUGSrad radiances
       !---------------------------------------------------------
      !BugsRAD Surface Properties
      if(algorithm_processing_mode .eq. 1) then
       call preprocess_bugsrad_sfc_albedo(nc_alb,rho_0d(i,j,:),rho_dd(i,j,:),rho_0d_bugsrad,rho_dd_bugsrad)
       call preprocess_bugsrad_sfc_emissivity(nc_emis,emis_data(i,j,:),emis_bugsrad)
      endif

      !FuLiou Surface Properties
      if(algorithm_processing_mode .eq. 2) then
       call preprocess_fuliou_sfc_albedo(nc_alb,rho_0d(i,j,:),rho_dd(i,j,:),rho_0d_fuliou,rho_dd_fuliou)
       call preprocess_bugsrad_sfc_emissivity(nc_emis,emis_data(i,j,:),emis_fuliou)
      endif

       !print*,'BUGSrad (blacksky) ',rho_0d_bugsrad
       !print*,'BUGSrad (whitesky) ',rho_dd_bugsrad
       !print*,'BUGSrad emissivity ',emis_bugsrad
       !print*,'Fu Liou (blacksky) ',rho_0d_fuliou
       !print*,'Fu Liou (whitesky) ',rho_dd_fuliou
       !print*,'Fu Liou emissivity ',emis_fuliou

       ! solar zenith angle
       pxTheta = COS( SOLZ(i,j) * Pi/180.)
       !print*,i,j,solz(i,j),pxTheta

       ! solar zenith angle condition (remove nighttime & twilight)
!       if( SOLZ(i,j) .lt. 80.) then

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

        !skin temperature - currently bottom level as defined in
        !rttov_driver.F90
        !pxts = inT_(levdim_prtm)
        !check if STEMP is valid, if not use ECMWF value.
        if((STEMP(i,j) .lt. 0) .or. (STEMP(i,j) .gt. 400)) then
           pxts = inT_(levdim_prtm)
        else
           pxts = STEMP(i,j)
        endif

         !print*,'latitude: ',LAT(i,j)
         !print*,'longitude: ',LON(i,j)
         !print*,'solar zenith: ',SOLZ(i,j)
         !print*,'cc_tot:  ',cc_tot(i,j)
         !print*,'Sat Phase: ',PHASE(i,j)
         !print*,'Sat retr. CTH = ',CTH(i,j)
         !print*,'Sat retr. CTT = ',CTT(i,j)
         !print*,'SAT retr. REF = ',REF(i,j)
         !print*,'SAT retr. COT = ',COT(i,j)
         !print*,'SAT retr. cc_tot = ',cc_tot(i,j)
         !print*,'Aerosol Optical Depth: ',aerAOD(aID(i,j))
         !print*,'Aerosol Effective Radius: ',aerREF(aID(i,j))

        !cloud base & top height calculation
        call preprocess_input(cc_tot(i,j),AREF(i,j),AOD550(i,j),phase(i,j),&
                         CTT(i,j),CTP(i,j),REF(i,j),COT(i,j),CTH(i,j),&
                         NLS,pxZ,pxREF,pxCOT,pxHctop,pxHcbase,&
                         pxPhaseFlag,pxLayerType,&
                         pxregime,pxHctopID,pxHcbaseID)

        !for detecting NaN's produced by BUGSrad
        nanFlag=0

         !debugging (print statements)
         !print*,'phase: ',pxPhaseFlag
         !print*,'re :',pxREF
         !print*,'tau :',pxCOT
         !print*,'Hctop = ',pxHctop,' HctopID: ',pxHctopID
         !print*,'Hcbase = ',pxHcbase,' HcbaseID: ',pxHcbaseID
         !print*,'Regime: ',pxregime
         !print*,'TOTAL SOLAR IRRADIANCE: ',pxTSI
         !print*,'Hctop (hPa) = ',pxP(pxHctopID)
         ! print*,'Hcbase (hPa) = ',pxP(pxHcbaseID)

      !Call BUGSrad Algorithm
      if(algorithm_processing_mode .eq. 1) then
!      print*,'starting... BUGSrad'
!      print*,NL,pxTSI,pxtheta,pxAsfcSWRdr,pxAsfcNIRdr,pxAsfcSWRdf,pxAsfcNIRdf,pxts
!      print*,nc_alb,rho_0d(i,j,:),rho_dd(i,j,:)
         call driver_for_bugsrad(NL,pxTSI,pxtheta,pxAsfcSWRdr,pxAsfcNIRdr,pxAsfcSWRdf,pxAsfcNIRdf,pxts,&
                          pxPhaseFlag,pxREF,pxCOT,pxHctop,pxHcbase,&
                          pxHctopID,pxHcbaseID,&
                          pxZ,pxP,pxT,pxQ,pxO3,&
                          pxtoalwup,pxtoaswdn,pxtoaswup,&
                          pxboalwup,pxboalwdn,pxboaswdn,pxboaswup,&
                          pxtoalwupclr,pxtoaswupclr,&
                          pxboalwupclr,pxboalwdnclr,pxboaswupclr,pxboaswdnclr,&
                          bpar,bpardif,tpar,&
                          ulwfx,dlwfx,uswfx,dswfx,&
                          ulwfxclr,dlwfxclr,uswfxclr,dswfxclr,&
                          emis_bugsrad,rho_0d_bugsrad,rho_dd_bugsrad)

!           print*,'BUGSrad'
!           print*,pxtoalwup,pxtoaswdn,pxtoaswup
!           print*,pxtoalwupclr,pxtoaswdn,pxtoaswupclr
!           print*,pxboalwup,pxboalwdn,pxboaswdn,pxboaswup
!           print*,pxboalwupclr,pxboalwdnclr,pxboaswdnclr,pxboaswupclr
!           print*,tpar,bpar,bpardif

       endif

      !Call FuLiou Algorithm
      if(algorithm_processing_mode .eq. 2) then
!      print*,'starting... Fu-Liou'
!      print*,NL,pxTSI,pxtheta,pxAsfcSWRdr,pxAsfcNIRdr,pxAsfcSWRdf,pxAsfcNIRdf,pxts
!      print*,nc_alb
!      print*,rho_0d(i,j,:)
!      print*,rho_dd(i,j,:)
!      print*,''
   call driver_for_fuliou(NL,pxTSI,pxtheta,pxAsfcSWRdr,pxAsfcNIRdr,pxAsfcSWRdf,pxAsfcNIRdf,pxts,&
                          pxPhaseFlag,pxREF,pxCOT,pxHctop,pxHcbase,&
                          pxHctopID,pxHcbaseID,&
                          pxZ,pxP,pxT,pxQ,pxO3,&
                          pxtoalwup,pxtoaswdn,pxtoaswup,&
                          pxboalwup,pxboalwdn,pxboaswdn,pxboaswup,&
                          pxtoalwupclr,pxtoaswupclr,&
                          pxboalwupclr,pxboalwdnclr,pxboaswupclr,pxboaswdnclr,&
                          bpar,bpardif,tpar,&
                          ulwfx,dlwfx,uswfx,dswfx,&
                          ulwfxclr,dlwfxclr,uswfxclr,dswfxclr,&
                          emis_fuliou,rho_0d_fuliou,rho_dd_fuliou,fu_solver_mode)

!           print*,'Fu Liou'
!           print*,i,j,'1',pxtoaswup
!           print*,pxtoalwupclr,pxtoaswdn,pxtoaswupclr
!           print*,pxboalwup,pxboalwdn,pxboaswdn,pxboaswup
!           print*,pxboalwupclr,pxboalwdnclr,pxboaswdnclr,pxboaswupclr
!           print*,tpar,bpar,bpardif

       endif


         !catch NaN
         if(is_nan(pxtoalwup)) nanFlag=1
         if(is_nan(pxtoaswup)) nanFlag=1
         if(is_nan(pxtoalwupclr)) nanFlag=1
         if(is_nan(pxtoaswupclr)) nanFlag=1

         !catch unphysical values
         if(pxtoalwup .lt. 0. .or. pxtoalwup .gt. 1000.) nanFlag=1
         if(pxtoaswup .lt. 0. .or. pxtoaswup .gt. 1600.) nanFlag=1

         !regime type
         retrflag(i,j) = pxregime

         !valid data only
         if(nanFlag == 0) then
          !netCDF output arrays
          !Observed
          time_data(i,j) = TIME(i,j)
          lat_data(i,j)  = LAT(i,j)
          lon_data(i,j)  = LON(i,j)

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

         end if !valid data

         ! meteorology data to output in netCDF file
         boa_tsfc(i,j) = pxT(NLS)
         boa_psfc(i,j) = pxP(NLS)
         boa_qsfc(i,j) = pxQ(NLS)
          call compute_lts(NL,pxP,pxT,pxLTS)
          call compute_fth(NL,pxP,pxT,pxQ,pxFTH)
          call compute_column_o3(NL,pxZ,pxO3,pxcolO3)

         lts(i,j) = pxLTS
         fth(i,j) = pxFTH
         colO3(i,j) = pxcolO3

!       end if   !valid solar zenith angle
      end if   !valid geolocation data
    end do !j-loop
   end do !i-loop
call cpu_time(cpuFinish)
print*,cpuFinish-cpuStart,' seconds elapsed'

!-------------------------------------------------------------------------
!Make output netcdf file
!-------------------------------------------------------------------------
 call nc_open(ncid,Fprimary)

!get common attributes from primary file
 call nc_get_common_attributes(ncid, global_atts, source_atts)

if (nf90_close(ncid) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_close()'
         stop error_stop_code
end if

   !dimensions
   ixstart = pxX0
   ixstop  = pxX1
   iystart = pxY0
   iystop  = pxY1
   n_x = ixstop - ixstart + 1
   n_y = iystop - iystart + 1
   n_v = 1

  ! create netcdf file
   call nc_create(trim(fname), ncid, ixstop-ixstart+1, &
      iystop-iystart+1, n_v, dim3d_var, 1, global_atts, source_atts)
   dims_var = dim3d_var(1:2)

      !Need this to exit data mode to define variables
      if (nf90_redef(ncid) .ne. NF90_NOERR) then
        write(*,*) 'ERROR: nf90_redef()'
        stop error_stop_code
      end if

      !-------------------------------------------------------------------------
      ! time
      !-------------------------------------------------------------------------
       call nc_def_var_double_packed_double( &
               ncid, &
               dims_var, &
               'time', &
               TIME_vid, &
               verbose, &
               long_name     = 'Time in Julian days', &
               standard_name = 'Julian Days', &
               fill_value    = dreal_fill_value, &
               scale_factor  = real(1, dreal), &
               add_offset    = real(0, dreal), &
               valid_min     = real(MINVAL(time_data), dreal), &
               valid_max     = real(MAXVAL(time_data), dreal), &
               units         = 'days since -4712-01-01 12:00:00', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)



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
       call nc_def_var_byte_packed_byte( &
               ncid, &
               dims_var, &
               'retrflag', &
               retrflag_vid, &
               verbose, &
               long_name     = 'retrflag', &
               standard_name = 'retrflag', &
               fill_value    = byte_fill_value, &
               scale_factor  = int(1, byte), &
               add_offset    = int(0, byte), &
               valid_min     = int(1, byte), &
               valid_max     = int(4, byte), &
               units         = '1', &
               flag_values   = '1b 2b 3b 4b 5b 6b', &
               flag_meanings = 'oc_liq-cld oc_ice-cld clear-AOD clear_no_AOD joint_aod-liq_cld joint_aod-ice_cld', &
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

      !-------------------------------------------------------------------------
      ! surface_air_temperature
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_tsfc', &
               boa_tsfc_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere air temperature', &
               standard_name = 'surface_air_temperature', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'K', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! surface_air_pressure
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_psfc', &
               boa_psfc_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere air pressure', &
               standard_name = 'surface_air_pressure', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'hPa', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! surface_specific_humidity
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'boa_qsfc', &
               boa_qsfc_vid, &
               verbose, &
               long_name     = 'bottom of atmosphere specific humidity', &
               standard_name = 'surface_specific_humidity', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'kg/kg', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! lower_troposphere_stability
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'lts', &
               lts_vid, &
               verbose, &
               long_name     = 'lower troposphere stability (theta700 - theta sfc)', &
               standard_name = 'lower_troposphere_stability', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'K', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! free_troposphere_humidity
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'fth', &
               fth_vid, &
               verbose, &
               long_name     = 'free troposphere humidity (RH at 850 hPa)', &
               standard_name = 'free_troposphere_humidity', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = '1', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

      !-------------------------------------------------------------------------
      ! Column_Ozone
      !-------------------------------------------------------------------------
       call nc_def_var_float_packed_float( &
               ncid, &
               dims_var, &
               'colO3', &
               colO3_vid, &
               verbose, &
               long_name     = 'column ozone', &
               standard_name = 'column_ozone', &
               fill_value    = sreal_fill_value, &
               scale_factor  = real(1), &
               add_offset    = real(0), &
               valid_min     = real(0, sreal), &
               valid_max     = real(1500, sreal), &
               units         = 'DU', &
               deflate_level = deflate_lv, &
               shuffle       = shuffle_flag)

     !Need to exit define mode to write data
      if (nf90_enddef(ncid) .ne. NF90_NOERR) then
       write(*,*) 'ERROR: nf90_enddef()'
       stop error_stop_code
      end if


     !write the array to the netcdf file
     call nc_write_array(ncid,'time',TIME_vid,&
             time_data(ixstart:,iystart:),1,1,n_x,1,1,n_y)

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

     call nc_write_array(ncid,'boa_tsfc',boa_tsfc_vid,&
             boa_tsfc(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_psfc',boa_psfc_vid,&
             boa_psfc(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'boa_qsfc',boa_qsfc_vid,&
             boa_qsfc(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'lts',lts_vid,&
             lts(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'fth',fth_vid,&
             fth(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     call nc_write_array(ncid,'colO3',colO3_vid,&
             colO3(ixstart:,iystart:),1,1,n_x,1,1,n_y)

     !close netcdf file
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_close()'
         stop error_stop_code
      end if

print*,'CREATED:'
print*,TRIM(fname)

end program process_broadband_fluxes
