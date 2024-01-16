!*******************************************************************************
!
! Copyright (C) 2000-2018, RAL Space, Science and Technology Facilities Council
! Copyright (C) 2000-2018, University of Oxford
! Copyright (C) 2011-2018, Deutscher Wetterdienst
!
! This file is part of the Optimal Retrieval of Aerosol and Cloud (ORAC).
!
! ORAC is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! ORAC is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE. See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! ORAC. If not, see <http://www.gnu.org/licenses/>.
!
!*******************************************************************************


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
!   4) TSI file
!      (https://github.com/mattchri/broadband_fluxes/blob/master/tsi_soho_sorce_1978_2017.nc)
!   5) Output filename (user specified)
!   6) Radiation Algorithm (BUGSrad: '1'; FuLiou-2Stream Modified Gamma: '2';
!      FuLiou-4stream '3'; FuLiou-2Stream '4')
!   7) x0, 8) y0, 9) x1, 10) y1 {required but can leave blank to specify full
!      range}
!
! Optional inputs: Use equals sign (=) with appropriate designator can be in
! any order
!    cci_aerosol={filename} accepts v3.02 or v4.01 types
!    cci_collocation={filename}
!    modis_aerosol={filename} accepts MOD04 or MYD04 COLLECTION 6
!    modis_cloud={filename} accepts MOD06 or MYD06 COLLECTION 6
!    LUT_mode={filename} this is generated off the repository currently
!
! Subroutines:
!    interpolate_meteorology.F90
!    compute_lts.F90
!    compute_fth.F90
!    compute_column_o3.F90
!    preprocess_input.F90
!    preprocess_bugsrad_sfc_albedo.F90
!    preprocess_bugsrad_sfc_emissivity.F90
!    preprocess_fuliou_sfc_albedo.F90
!    driver_for_fuliou.F90
!    driver_for_bugsrad.F90
!
! History:
! 2015/10/14, MC: Initial implementation
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
! 2015/12/10, MC: Removed nighttime and twilight retrievals (solar zenith angle
!    < 80)
! 2015/12/21, MC: Added optional argument and collocation routine to process
!    radiative fluxes using aerosol cci data.
! 2016/01/12, MC: Added Liang (2000) surface albedo model which converts
!    narrowband albedo radiances for each channel into broadband albedo for
!    direct visible, diffuse visible, direct near-IR, and diffuse-IR as required
!    inputs for BUGSrad.
! 2016/02/19, MC: Added meteorological variables (surface temperature, pressure,
!    humidity, LTS, FTH, & column ozone) to output file.
! 2016/02/19, MC: Fixed bug in reading time from input file name. Index is now
!    based the last underscore in primary file instead of the word primary.
! 2016/03/31, MC: Added time variable to output file.
! 2016/04/06, MC: Modified aerosol_processing option to bypass collocation file
!    creation.
! 2016/07/20, WJ: Added surface temperature from ORAC retrieval and uses this
!    instead of ECMWF skin temperature where possible.
! 2016/08/03, MC: Added the Fu-Liou broadband radiative flux code and adapted
!    procedures to the respository.Code from: www-cave.larc.nasa.gov/ Edition 4
!    January 12th 2015. New option to run the ORAC brodband flux code using
!    BUGSrad: 1 or Fu_Liou: 2.
! 2016/08/10, MC: Debugged Fu Liou code so that clear-sky pixels can run in the
!    retreival. Set limits to droplet effective radius (re < 30 um) for liquid
!    clouds only in Fu Liou.
! 2016/08/15, MC: Arrays were incorrectly being assigned integer fill value
!    instead of real.
! 2016/08/15, MC: Changed output time array from float to double.
! 2016/08/16, MC: Modified code to restrict size of output netCDF file by the
!    optional input pixel selection range (pxX0,pxY0,pxX1,pxY1).
! 2016/09/19, MC: Added option to run code using MODIS C6 aerosol (MOD04/MYD04)
!    & cloud (MOD06/MYD06) instead of CCI. Added hdf reader tools for MODIS.
!    Re-formatted optional arguments for efficiency.
! 2016/09/22, MC: Added conditional statement to input pixel selection range so
!    if values of zero are specified the whole range of the orbit is used.
! 2016/12/09, MC: Added options to examine impact of corrected CTH and
!    infinitely thin clouds.
! 2017/01/12, MC: Added driver code and option to run SW LUT for fast
!    implementation of the broadband flux retrieval. LUT's are currently
!    generated offline using solar zenith angle cloud effective radius, cloud
!    optical depth, and broadband surface albedo applied to the Fu-Liou model to
!    produce LUT_BB_FLUXES.nc. Speed increased by a factor of ~200. Relative
!    accuracy (RMS) is between 4-8% at the TOA and Surface.
! 2017/04/04, MC: Added scale factor for PAR based on solar spectral
!    observations from SORCE. Weights are computed by taking the ratio of the
!    measured solar irradiance in the 400-700 nm wavelength range by the
!    estimated value based on planck radiation law and earth-sun distance. Note,
!    need to update tsi_soho_sorce_1978_2015.nc.
! 2017/06/23, OS: Added WRAPPER subroutine definition and settings
! 2017/06/30, OS: Net nTSI to + 500, so that values of 2016 are read
! 2017/07/27, MC: Removed unit conversion on Q and O3 profiles to be compatible
!    with the output from the PRTM file represented in kg/kg. Also updated the
!    gravitational acceleration to the appropriate value of 9.80665.
! 2017/07/28, MC: Modified code to accept multi-layer cloud primary output
!    files. Note, you must specify 'multi_layer=1' to run in multi-layer mode.
! 2018/01/23, MST: Passing pxYEAR to driver_for_bugsrad to enable time dependent co2 concentration calculation
! 2018/04/23, MC: Added aerosol support from pixel scale retrieval from ORAC
!    combined post primary file. Fixed bug related to multi-layer code not
!    allowing single-layer aerosol retreival in clear sky pixels. Updated
!    spectral total solar irradiance input file. Included option to calculate
!    TSI from earth-sun distance relationship and constant TSI at 1 AU if not provided by LUT.
! 2022/04/28, GT: Added a check for the existence of the "par_weight"
!    parameter in the TSI dataset, as it is not included/needed in the new NOAA
!    TSI CDRs.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#ifndef WRAPPER
program process_broadband_fluxes
#else
subroutine process_broadband_fluxes(Fprimary, FPRTM, FALB, FTSI, fname,&
        FLXalgorithm, status, Faerosol, Fcollocation)
#endif
   use common_constants_m
   use global_attributes_m
   use orac_ncdf_m
   use source_attributes_m
   use system_utils_m

   implicit none

#ifndef WRAPPER
   character(path_length) :: Fprimary, FPRTM, FTSI, FALB, fname, FLXalgorithm, &
                             Faerosol, Fcollocation
#else
   integer :: status
   character(file_length), intent(in) :: Fprimary, FPRTM, FTSI, FALB, fname, &
                                         FLXalgorithm
   character(file_length), intent(inout), optional :: Faerosol, Fcollocation
#endif
   character(path_length) :: FMOD04, FMOD06
   integer :: algorithm_processing_mode ! 1-BUGSrad, 2-FuLiou2G, 3-FuLiou4S, 4-FuLiou 2S
   integer :: ncid, i, j, k, dims_var(2), dim3d_var(3)
   logical, parameter :: verbose=.true.
   logical :: there
   type(global_attributes_t) :: global_atts
   type(source_attributes_t) :: source_atts

   ! Constants
   real(kind=8), parameter :: Runiv = 8314.         ! universal gas constant
   real(kind=8), parameter :: Rdryair = 287.05      ! dry air gas constant
   real(kind=8), parameter :: Rwetair = Runiv / 18. ! moist air gas constant
   real(kind=8), parameter :: Rozone = Runiv / 48.  ! ozone gas constant

   ! BUGSrad setup
!  integer, parameter :: NL = 59 ! # of vertical layers in radiative flux code,
                                  ! must be odd
   integer, parameter :: NL = 29
   integer, parameter :: NLS = NL+1
   real, dimension(NLS) :: pxZ, pxP, pxT, pxQ, pxO3 ! pixel level PRTM profiles

   ! PRTM file
   real, allocatable :: P(:,:,:)      ! Pressure - grid
   real, allocatable :: T(:,:,:)      ! Temperature - grid
   real, allocatable :: H(:,:,:)      ! Height - grid
   real, allocatable :: Q(:,:,:)      ! Humidity - grid
   real, allocatable :: O3(:,:,:)     ! Ozone - grid
   real, allocatable :: inP(:)        ! Pressure - interpolated
   real, allocatable :: inT_(:)       ! Temperature - interpolated
   real, allocatable :: inH(:)        ! Height - interpolated
   real, allocatable :: inQ(:)        ! Humidity - interpolated
   real, allocatable :: inO3(:)       ! Ozone - interpolated
   real, allocatable :: lon_prtm(:,:) ! Longitude values
   real, allocatable :: lat_prtm(:,:) ! Latitude values
   real, allocatable :: tlon_prtm(:)  ! Longitude values
   real, allocatable :: tlat_prtm(:)  ! Latitude values
   real, allocatable :: dummy1d(:)
   integer(kind=lint) :: xdim_prtm, ydim_prtm, levdim_prtm ! PRTM dimensions
   integer(kind=lint) :: xN, yN ! Satellite 1-km dimensions
   integer, dimension(NLS) :: mask_vres ! To match PRTM vertical resolution to BUGSrad

   ! Albedo file
   real, allocatable :: rho_0d(:,:,:)          ! Albedo direct directional - blacksky albedo
   real, allocatable :: rho_dd(:,:,:)          ! Albedo diffuse directional - whitesky albedo
   real, allocatable :: alb_data(:,:,:)        ! Broadband albedo
   real, allocatable :: emis_data(:,:,:)       ! Emissivity
   real, allocatable :: alb_abs_ch_numbers(:)  ! Channels used in albedo product
   real, allocatable :: emis_abs_ch_numbers(:) ! Channels used in emissivity product
   integer(kind=lint) :: nc_alb
   integer(kind=lint) :: nc_emis

   ! Primary file
   real, allocatable :: LAT(:,:)              ! Latitude Satellite (xp,yp)
   real, allocatable :: LON(:,:)              ! Longitude Satellite (xp,yp)
   real, allocatable :: SOLZ(:,:)             ! Solar zenith angle (xp,yp)
   real, allocatable :: PHASE(:,:)            ! Cloud phase (xp,yp)
   real(kind=dreal), allocatable :: TIME(:,:) ! Time of pixel in Julian Days (xp,yp)
   real, allocatable :: STEMP(:,:)            ! Surface temperature from primary files (xp,yp)
   real, allocatable :: COT(:,:)              ! Cloud Optical Depth (xp,yp)
   real, allocatable :: REF(:,:)              ! Cloud Effective Radius (xp,yp)
   real, allocatable :: CC_TOT(:,:)           ! Cloud Mask (xp,yp)
   real, allocatable :: CTH(:,:)              ! Cloud Top Height (xp,yp)
   real, allocatable :: CTT(:,:)              ! Cloud top temperature (xp,yp)
   real, allocatable :: CTP(:,:)              ! Cloud top pressure (xp,yp)
   real, allocatable :: COT2(:,:)             ! Cloud Optical Depth (xp,yp)
   real, allocatable :: REF2(:,:)             ! Cloud Effective Radius (xp,yp)
   real, allocatable :: CC_TOT2(:,:)          ! Cloud Mask (xp,yp)
   real, allocatable :: CTH2(:,:)             ! Cloud Top Height (xp,yp)
   real, allocatable :: CTT2(:,:)             ! Cloud top temperature (xp,yp)
   real, allocatable :: CTP2(:,:)             ! Cloud top pressure (xp,yp)

   ! Total solar irriadiance file
   real, allocatable :: TSI_tsi_true_earth(:) ! Total solar irradiance at earth-sun distance
   real, allocatable :: TSI_tsi_1au(:)        ! Total solar irradiance at 1-au
   real, allocatable :: TSI_year(:)           ! TSI index year
   real, allocatable :: TSI_jday(:)           ! TSI index Julian day
   real, allocatable :: TSI_par_weight(:)     ! TSI weight for PAR based on SORCE data
   integer(kind=lint) :: nTSI = 14256
   real(kind=dreal) :: TSI_time, TSI_sfac

   ! Aerosol CCI file (optional)
   real, allocatable :: aerLon(:)   ! Longitude
   real, allocatable :: aerLat(:)   ! Latitude
   real, allocatable :: aerAOD(:)   ! Aerosol optical depth
   real, allocatable :: aerREF(:)   ! Aerosol effective radius
   real, allocatable :: aerQflag(:) ! q-flag
   real, allocatable :: aerAOD1(:,:)   ! Aerosol optical depth
   real, allocatable :: aerREF1(:,:)   ! Aerosol effective radius
   real, allocatable :: AOD550(:,:) ! Aerosol optical depth at 1 km resolution
   real, allocatable :: AREF(:,:)   ! Aerosol Effective Radius 1 km resolution
   integer, allocatable :: aID(:,:) ! Aerosol index for i,jth locations in cloud file
   integer(kind=lint) :: nc_aer
   integer :: aID_vid

   ! Pixel-scale variables
   integer :: pxYear    ! Year
   integer :: pxMonth   ! Month
   integer :: pxDay     ! Day
   real :: pxJday       ! Julian day from 1 - 365
   real :: pxTSI        ! Total incoming solar irradiance (true-earth)
   real :: pxPAR_WEIGHT ! Total incoming solar irradiance (true-earth)
   real :: pxAsfcSWRdr  ! DIRECT visible surface albedo
   real :: pxAsfcNIRdr  ! DIRECT near-infrared surface albedo
   real :: pxAsfcSWRdf  ! DIFFUSE visible surface albedo
   real :: pxAsfcNIRdf  ! DIFFUSE near-infrared surface albedo
   real :: pxTheta      ! Cosine of solar zenith angle

   integer :: tmp_pxHctopID(1), tmp_pxHcbaseID(1)
   real :: tmp_pxREF       ! cloud effective radius
   real :: tmp_pxCOT       ! cloud optical depth
   real :: tmp_pxHctop     ! input cloud top height
   real :: tmp_pxHcbase    ! input cloud base height
   real :: tmp_pxPhaseFlag ! cloud phase type

   real :: pxREF(2)       ! cloud effective radius
   real :: pxCOT(2)       ! cloud optical depth
   real :: pxHctop(2)     ! input cloud top height
   real :: pxHcbase(2)    ! input cloud base height
   real :: pxPhaseFlag(2) ! cloud phase type

   integer :: pxHctopID(2), pxHcbaseID(2)
   real :: pxLayerType ! aerosol type
   real :: pxts        ! land/sea surface temperature
   real :: pxregime
   real :: pxLTS
   real :: pxFTH
   real :: pxcolO3
   real :: rho_0d_bugsrad(6), rho_dd_bugsrad(6), emis_bugsrad(12)
   real :: rho_0d_fuliou(18), rho_dd_fuliou(18), emis_fuliou(12)

   ! Radiation flux profiles
   real (kind=8), dimension(1,NL) :: &
      ulwfx   ,& ! all-sky upward longwave flux
      dlwfx   ,& ! all-sky downward longwave flux
      uswfx   ,& ! all-sky upward shortwave flux
      dswfx   ,& ! all-sky downward shortwave flux
      ulwfxclr,& ! clear-sky upward longwave flux
      dlwfxclr,& ! clear-sky downward longwave flux
      uswfxclr,& ! clear-sky upward shortwave flux
      dswfxclr   ! clear-sky downward shortwave flux

   ! Flux & PAR variables
   real :: &
      pxtoalwup, pxtoaswdn, pxtoaswup ,&          ! All-sky TOA fluxes
      pxtoalwupclr, pxtoaswupclr,&               ! Clear-Sky TOA fluxes
      pxboalwup, pxboalwdn, pxboaswdn, pxboaswup,& ! All-sky BOA fluxes
      pxboalwupclr, pxboalwdnclr, pxboaswdnclr, pxboaswupclr,& ! clear-sky BOA fluxes
      tpar   ,& ! TOA PAR total
      bpardif,& ! BOA PAR diffuse
      bpar      ! BOA PAR total

   ! NetCDF output geolocation data
   real(kind=dreal), allocatable :: time_data(:,:)
   real, allocatable :: lat_data(:,:), lon_data(:,:) !latitude & longitude

   ! NetCDF output TOA & BOA radiation flux data
   real, allocatable :: toa_lwup(:,:) ! TOA outgoing LW flux
   integer :: toa_lwup_vid

   real, allocatable :: toa_swup(:,:) ! TOA outgoing SW flux
   integer :: toa_swup_vid

   real, allocatable :: toa_swdn(:,:) ! TOA incoming SW flux
   integer :: toa_swdn_vid

   real, allocatable :: boa_lwup(:,:) ! BOA outgoing LW flux
   integer :: boa_lwup_vid

   real, allocatable :: boa_lwdn(:,:) ! BOA incoming LW flux
   integer :: boa_lwdn_vid

   real, allocatable :: boa_swup(:,:) ! BOA outgoing SW flux
   integer :: boa_swup_vid

   real, allocatable :: boa_swdn(:,:) ! BOA incoming SW flux
   integer :: boa_swdn_vid

   ! NetCDF output clear-sky TOA & BOA radiation flux data
   real, allocatable :: toa_lwup_clr(:,:) ! TOA outgoing LW flux clear-sky condition
   integer :: toa_lwup_clr_vid

   real, allocatable :: toa_swup_clr(:,:) ! TOA outgoing SW flux clear-sky condition
   integer :: toa_swup_clr_vid

   real, allocatable :: boa_lwup_clr(:,:) ! BOA outgoing LW flux clear-sky condition
   integer :: boa_lwup_clr_vid

   real, allocatable :: boa_lwdn_clr(:,:) ! BOA incoming LW flux clear-sky condition
   integer :: boa_lwdn_clr_vid

   real, allocatable :: boa_swup_clr(:,:) ! BOA outgoing SW flux clear-sky condition
   integer :: boa_swup_clr_vid

   real, allocatable :: boa_swdn_clr(:,:) ! BOA incoming SW flux clear-sky condition
   integer :: boa_swdn_clr_vid

   ! NetCDF output TOA & BOA PAR radiation flux data
   real, allocatable :: toa_par_tot(:,:) ! TOA PAR - total
   integer :: toa_par_tot_vid

   real, allocatable :: boa_par_tot(:,:) ! BOA PAR - total
   integer :: boa_par_tot_vid

   real, allocatable :: boa_par_dif(:,:) ! BOA PAR - diffuse
   integer :: boa_par_dif_vid

   ! NetCDF output meteorological variables
   real, allocatable :: boa_tsfc(:,:) ! BOA temperature
   integer :: boa_tsfc_vid

   real, allocatable :: boa_psfc(:,:) ! BOA pressure
   integer :: boa_psfc_vid

   real, allocatable :: boa_qsfc(:,:) ! BOA vapour pressure
   integer :: boa_qsfc_vid

   real, allocatable :: lts(:,:) ! LTS (LOWER TROPOSPHERE STABILITY)
   integer :: lts_vid

   real, allocatable :: fth(:,:) ! FTH (FREE TROPOSPHERE HUMIDITY 850 hPa)
   integer :: fth_vid

   real, allocatable :: colO3(:,:) ! Column Ozone
   integer :: colO3_vid

   real, allocatable :: cbh(:,:) ! cloud base height
   integer :: cbh_vid

   ! NetCDF output (lat/lon/time)
   integer :: LAT_vid
   integer :: LON_vid
   integer :: TIME_vid

   integer(kind=byte), allocatable :: retrflag(:,:) ! Regime flag
   integer :: retrflag_vid

   ! More options

   ! Infinitely thin cloud assumption & use corrected cloud top heights
   integer :: InfThnCld, corrected_cth, multi_layer
   integer :: ml_flag

   integer, parameter :: deflate_lv = 9
   logical, parameter :: shuffle_flag = .false.

   ! NetCDF output dimensions
   integer :: ixstart, ixstop,& ! First and last super-pixel X locations
              iystart, iystop,& ! First and last super-pixel Y locations
               n_x, n_y, n_v

   ! Debugging
   integer :: nanFlag

   ! Pixel selection option
   character(path_length) :: cpxX0, cpxY0, cpxX1, cpxY1
   integer :: pxX0, pxY0, pxX1, pxY1
   integer :: value
   !=0 no processing, =1 collocate aerosol cci file, =2 collocate & save file
   integer :: aerosol_processing_mode

   ! For reading time from input string
   integer :: index1, index2
   character(len=4) :: cyear
   character(len=2) :: cmonth
   character(len=2) :: cday

   ! For CPU processing time
   real :: cpuStart, cpuFinish

   integer :: nargs ! Number of command line arguments
   character(path_length) :: argname, tmpname1, tmpname2

   ! Broadband albedo LUT
   real, allocatable :: LUT_toa_sw_albedo(:,:,:,:),&
                        LUT_boa_sw_transmission(:,:,:,:),&
                        LUT_boa_sw_albedo(:,:,:,:)
   character(path_length) :: FtoaSW
   integer :: lut_mode ! =0 no processing, =1 use LUT
   integer(kind=lint) :: nASFC, nRE, nTAU, nSOLZ
   real, allocatable :: LUT_SFC_ALB(:), LUT_REF(:), LUT_COT(:), LUT_SOLZ(:)
   real :: tmpVal(1)


   !----------------------------------------------------------------------------
   ! Read mandatory arguments
   !----------------------------------------------------------------------------
#ifndef WRAPPER
   nargs = command_argument_count()
   call get_command_argument(1, Fprimary)
   call get_command_argument(2, FPRTM)
   call get_command_argument(3, FALB)
   call get_command_argument(4, FTSI)
   call get_command_argument(5, fname)
#endif
   print*, 'primary file: ', trim(adjustl(Fprimary))
   print*, 'prtm file : ', trim(FPRTM)
   print*, 'albedo file: ', trim(FALB)
   print*, 'total solar irradiance file: ', trim(FTSI)
   print*, 'output file: ', trim(fname)

#ifndef WRAPPER
   call get_command_argument(6, FLXalgorithm)
#endif
   read(flxAlgorithm,*) algorithm_processing_mode
   if (algorithm_processing_mode .eq. 1) then
      print*, 'Using algorithm: BUGSrad'
#ifdef INCLUDE_FU_LIOU_SUPPORT
   else if (algorithm_processing_mode .eq. 2) then
      print*, 'Using algorithm: FuLiou 2G'
   else if (algorithm_processing_mode .eq. 3) then
      print*, 'Using algorithm: FuLiou 4S'
   else if (algorithm_processing_mode .eq. 4) then
      print*, 'Using algorithm: FuLiou 2S'
#endif
   else
      write(*,*) 'ERROR: Unsupported algorithm: ', algorithm_processing_mode
   end if

#ifndef WRAPPER
   ! Maybe these should be subroutine arguments; if 0, all pixels will be
   ! processed
   call get_command_argument(7, cpxX0)
   call get_command_argument(8, cpxY0)
   call get_command_argument(9, cpxX1)
   call get_command_argument(10, cpxY1)
#else
   ! Process all pixels if in Wrapper mode
   cpxX0 = "0"; cpxY0 = "0"; cpxX1 = "0"; cpxY1 = "0"
#endif
   ! x-y range of selected pixels
   if (len_trim(cpxX0) .ne. 0 .and. len_trim(cpxX1) .ne. 0 .and. &
       len_trim(cpxY0) .ne. 0 .and. len_trim(cpxY1) .ne. 0) then
      print*, 'PROCESS MULTIPLE SATELLITE PIXELS'
      read(cpxX0,*) value
      pxX0 = value*1
      read(cpxX1,*) value
      pxX1 = value*1
      read(cpxY0,*) value
      pxY0 = value*1
      read(cpxY1,*) value
      pxY1 = value*1
      print*, pxX0, pxX1, pxY0, pxY1
   end if

   ! Read optional arguments
   InfThnCld = 0
   corrected_cth = 0
   multi_layer = 0
   aerosol_processing_mode = 0
   lut_mode = 0
#ifndef WRAPPER
   ! Set Faerosol, Fcollocation, FMOD04, FMOD06, InfThnCld, corrected_cth, FtoaSW
   do i = 11, nargs
      call get_command_argument(i, argname)
      index1 = index(argname,'=', back=.true.)
      index2 = len_trim(argname)
      tmpname1 = trim(adjustl(argname(1:index1-1)))
      tmpname2 = trim(adjustl(argname(index1+1:index2)))

      if (tmpname1 .eq. 'cci_aerosol') then
         Faerosol = trim(tmpname2)
         aerosol_processing_mode = 2
      end if
      if (tmpname1 .eq. 'cci_collocation') then
         Fcollocation = trim(tmpname2)
         aerosol_processing_mode = 3
      end if
      if (tmpname1 .eq. 'cci_aerpix') then
         Faerosol = trim(tmpname2)
         aerosol_processing_mode = 1
      end if
      if (tmpname1 .eq. 'modis_aerosol') FMOD04 = trim(tmpname2)
      if (tmpname1 .eq. 'modis_cloud') FMOD06 = trim(tmpname2)
      if (tmpname1 .eq. 'infinitely_thin_cloud') InfThnCld = 1
      if (tmpname1 .eq. 'corrected_cth') corrected_cth = 1
      if (tmpname1 .eq. 'multi_layer') multi_layer = 1
      if (tmpname1 .eq. 'LUT_mode') then
         FtoaSW = trim(tmpname2)
         lut_mode = 1
      end if
   end do
#endif

   !----------------------------------------------------------------------------
   ! Read time string from file
   !----------------------------------------------------------------------------
#ifndef WRAPPER
   index1 = index(Fprimary,'_', back=.true.)
   ! ORAC filenames without orbit number included
   cyear = trim(adjustl(Fprimary(index1-12:index1-9)))
   cmonth = trim(adjustl(Fprimary(index1-8:index1-7)))
   cday = trim(adjustl(Fprimary(index1-6:index1-4)))
   ! ORAC filenames with orbit number included (e.g. SLSTR)
!   cyear = trim(adjustl(Fprimary(index1-18:index1-15)))
!   cmonth = trim(adjustl(Fprimary(index1-14:index1-12)))
!   cday = trim(adjustl(Fprimary(index1-12:index1-10)))
#else
   index1 = index(Fprimary,'/', back=.true.)
   cyear = trim(adjustl(Fprimary(index1+1:index1+4)))
   cmonth = trim(adjustl(Fprimary(index1+5:index1+6)))
   cday = trim(adjustl(Fprimary(index1+7:index1+8)))
#endif
   print*, cyear
   print*, cmonth
   print*, cday

   read(cyear,'(I4)') value
   pxYear = value
   read(cmonth,'(I2)') value
   pxMonth = value
   read(cday,'(I2)') value
   pxDay = value
   ! Get calendar day
   call greg2jul(pxYear, pxMonth, pxDay, pxJday)
   print*, pxYear, pxMonth, pxDay, pxJday

   !----------------------------------------------------------------------------
   ! Read TSI file
   !----------------------------------------------------------------------------
   call ncdf_open(ncid, FTSI, 'process_broadband_fluxes()')

   ! Allocate arrays
   allocate(TSI_tsi_true_earth(nTSI))
   allocate(TSI_tsi_1au(nTSI))
   allocate(TSI_year(nTSI))
   allocate(TSI_jday(nTSI))
   allocate(TSI_par_weight(nTSI))

   ! Read primary data
   call ncdf_read_array(ncid, "tsi_true_earth", TSI_tsi_true_earth)
   call ncdf_read_array(ncid, "tsi_1au", TSI_tsi_1au)
   call ncdf_read_array(ncid, "year", TSI_year)
   call ncdf_read_array(ncid, "jday", TSI_jday)
   ! Check if we have a PAR weights in the TSI file
   if (nf90_inq_varid(ncid, "par_weight", i) .eq. NF90_NOERR) then
      call ncdf_read_array(ncid, "par_weight", TSI_par_weight)
   else
      TSI_par_weight(:) = 1.0
   endif

   ! Close file
   call ncdf_close(ncid, 'process_broadband_fluxes(FTSI)')

   ! Get TSI that coincides with input date
   pxTSI = 0.0
   do i = 1, nTSI
      if (TSI_year(i) .eq. pxYear .and. TSI_jday(i) .eq. pxJday) &
         pxTSI = TSI_tsi_true_earth(i)
      if (TSI_year(i) .eq. pxYear .and. TSI_jday(i) .eq. pxJday) &
         pxPAR_WEIGHT = TSI_par_weight(i)
   end do

   ! Get TSI if outside of LUT range
   if (pxTSI .eq. 0.) then
      TSI_time = (2.d0 * pxJday * Pi) / 365.d0
      !Earth-sun distance relationship
      TSI_sfac = 1.000110d0 + 0.03341d0*cos(TSI_time) + &
         & .001280d0*sin(TSI_time) + 0.000719d0*cos(2*TSI_time) + &
         & .000077d0*sin(2*TSI_time)
      !Constant value for TSI at 1 AU
      pxTSI = 1361. * TSI_sfac
      pxPAR_WEIGHT = 1.0
   end if

   print*, 'TSI data on date: '
   print*, 'YEAR: ', pxYear
   print*, 'Calendar Day: ', pxJday
   print*, 'TSI = ', pxTSI
   print*, 'PAR WEIGHTS = ', pxPAR_WEIGHT


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   if (lut_mode .eq. 1) then
      ! Open LUT file
      call ncdf_open(ncid, FToaSW, 'process_broadband_fluxes()')

      ! Get LUT dimensions
      nASFC = ncdf_dim_length(ncid, 'n_sfc_albedo', 'process_broadband_fluxes()')
      nSOLZ = ncdf_dim_length(ncid, 'n_solar_zenith', 'process_broadband_fluxes()')
      nRe   = ncdf_dim_length(ncid, 'n_effective_radius', 'process_broadband_fluxes()')
      nTau  = ncdf_dim_length(ncid, 'n_optical_depth', 'process_broadband_fluxes()')

      allocate(LUT_SFC_ALB(nASFC))
      call ncdf_read_array(ncid, "surface_albedo", LUT_SFC_ALB)

      allocate(LUT_SOLZ(nSOLZ))
      call ncdf_read_array(ncid, "solar_zenith_angle", LUT_SOLZ)

      allocate(LUT_REF(nRe))
      call ncdf_read_array(ncid, "cloud_effective_radius", LUT_REF)

      allocate(LUT_COT(nTau))
      call ncdf_read_array(ncid, "cloud_optical_thickness", LUT_COT)

      allocate(LUT_toa_sw_albedo(nASFC,nSOLZ,nRe,nTau))
      call ncdf_read_array(ncid, "toa_sw_albedo", LUT_toa_sw_albedo)

      allocate(LUT_boa_sw_transmission(nASFC,nSOLZ,nRe,nTau))
      call ncdf_read_array(ncid, "boa_sw_transmission", LUT_boa_sw_transmission)

      allocate(LUT_boa_sw_albedo(nASFC,nSOLZ,nRe,nTau))
      call ncdf_read_array(ncid, "boa_sw_albedo", LUT_boa_sw_albedo)

      ! Close file
      call ncdf_close(ncid, 'process_broadband_fluxes(FToaSW)')
   end if


   !----------------------------------------------------------------------------
   ! Read ORAC files
   !----------------------------------------------------------------------------

   ! Open primary file
   call ncdf_open(ncid, Fprimary, 'process_broadband_fluxes()')

   ! Get satellite dimensions
   xN = ncdf_dim_length(ncid, 'across_track', 'process_broadband_fluxes()')
   yN = ncdf_dim_length(ncid, 'along_track', 'process_broadband_fluxes()')

   ! Allocate arrays
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

   ! Read primary data
   call ncdf_read_array(ncid, "time", TIME)
   call ncdf_read_array(ncid, "lat", LAT)
   call ncdf_read_array(ncid, "lon", LON)
   call ncdf_read_array(ncid, "cot", COT)
   call ncdf_read_array(ncid, "cer", REF)
   call ncdf_read_array(ncid, "cc_total", CC_TOT)
   call ncdf_read_array(ncid, "phase", PHASE)
   call ncdf_read_array(ncid, "solar_zenith_view_no1", SOLZ)
   call ncdf_read_array(ncid, "stemp", STEMP)
   if (corrected_cth .eq. 0) then
      call ncdf_read_array(ncid, "ctt", CTT)
      call ncdf_read_array(ncid, "ctp", CTP)
      call ncdf_read_array(ncid, "cth", CTH)
   end if
   if (corrected_cth .eq. 1) then
      call ncdf_read_array(ncid, "ctt_corrected", CTT)
      call ncdf_read_array(ncid, "ctp_corrected", CTP)
      call ncdf_read_array(ncid, "cth_corrected", CTH)
   end if

   if (multi_layer .eq. 1) then
      allocate(COT2(xN,yN))
      allocate(REF2(xN,yN))
      allocate(CC_TOT2(xN,yN))
      allocate(CTH2(xN,yN))
      allocate(CTT2(xN,yN))
      allocate(CTP2(xN,yN))
      call ncdf_read_array(ncid, "cot2", COT2)
      call ncdf_read_array(ncid, "cer2", REF2)
      call ncdf_read_array(ncid, "cc_total2", CC_TOT2)
      call ncdf_read_array(ncid, "ctt2", CTT2)
      call ncdf_read_array(ncid, "ctp2", CTP2)
      call ncdf_read_array(ncid, "cth2", CTH2)
   end if

   ! Close file
   call ncdf_close(ncid, 'process_broadband_fluxes(FPrimary)')

   !----------------------------------------------------------------------------

   ! Open PRTM file
   call ncdf_open(ncid, FPRTM, 'process_broadband_fluxes()')

   ! Get PRTM dimensions
   xdim_prtm = ncdf_dim_length(ncid, 'nlon_rtm', 'process_broadband_fluxes()')
   ydim_prtm = ncdf_dim_length(ncid, 'nlat_rtm', 'process_broadband_fluxes()')
   levdim_prtm = ncdf_dim_length(ncid, 'nlevels_rtm', 'process_broadband_fluxes()')

   ! Allocate arrays
   allocate(P(levdim_prtm, xdim_prtm, ydim_prtm))
   allocate(T(levdim_prtm, xdim_prtm, ydim_prtm))
   allocate(H(levdim_prtm, xdim_prtm, ydim_prtm))
   allocate(Q(levdim_prtm, xdim_prtm, ydim_prtm))
   allocate(O3(levdim_prtm, xdim_prtm, ydim_prtm))
   allocate(lon_prtm(xdim_prtm, ydim_prtm))
   allocate(lat_prtm(xdim_prtm, ydim_prtm))
   allocate(tlon_prtm(xdim_prtm))
   allocate(tlat_prtm(ydim_prtm))

   ! Read PRTM data
   call ncdf_read_array(ncid, "pprofile_rtm", P)
   call ncdf_read_array(ncid, "tprofile_rtm", T)
   call ncdf_read_array(ncid, "hprofile_rtm", H)
   call ncdf_read_array(ncid, "qprofile_rtm", Q)
   call ncdf_read_array(ncid, "o3profile_rtm", O3)
   call ncdf_read_array(ncid, "lon_rtm", tlon_prtm)
   call ncdf_read_array(ncid, "lat_rtm", tlat_prtm)

   ! Fill longitude array
   allocate(dummy1d(xdim_prtm))
   call ncdf_read_array(ncid, "lon_rtm", dummy1d)
   do i = 1, xdim_prtm
      lon_prtm(i,:) = dummy1d(i)
   end do
   deallocate(dummy1d)

   ! Fill latitude array
   allocate(dummy1d(ydim_prtm))
   call ncdf_read_array(ncid, "lat_rtm", dummy1d)
   do i = 1, ydim_prtm
      lat_prtm(i,:) = dummy1d(i)
   end do
   deallocate(dummy1d)

   ! Close file
   call ncdf_close(ncid, 'process_broadband_fluxes(FPRTM)')

   ! Set PRTM units
   H = (H/g_wmo)/1000. ! to put to km

   !----------------------------------------------------------------------------

   ! Open ALB file
   call ncdf_open(ncid, FALB, 'process_broadband_fluxes()')

   ! Get # Channels
   nc_alb = ncdf_dim_length(ncid, 'nc_alb', 'process_broadband_fluxes()')
   nc_emis = ncdf_dim_length(ncid, 'nc_emis', 'process_broadband_fluxes()')

   ! Allocate arrays
   allocate(rho_dd(xN, yN, nc_alb))
   allocate(rho_0d(xN, yN, nc_alb))
   allocate(alb_data(xN, yN, nc_alb))
   allocate(emis_data(xN, yN, nc_emis))
   allocate(alb_abs_ch_numbers(nc_alb))
   allocate(emis_abs_ch_numbers(nc_emis))

   ! Read ALB data
   call ncdf_read_array(ncid, "rho_dd_data", rho_dd)
   call ncdf_read_array(ncid, "rho_0d_data", rho_0d)
   call ncdf_read_array(ncid, "alb_data", alb_data)
   call ncdf_read_array(ncid, "emis_data", emis_data)
   call ncdf_read_array(ncid, "alb_abs_ch_numbers", alb_abs_ch_numbers)
   call ncdf_read_array(ncid, "emis_abs_ch_numbers", emis_abs_ch_numbers)

   ! Close file
   call ncdf_close(ncid, 'process_broadband_fluxes(FALB)')
   ! Replace ALB_DATA WITH mean of rho_0d and rho_dd
   do i = 1, xN
      do j = 1, yN
         do k = 1, nc_alb
            if (alb_data(i,j,k) .gt. 0.) then
                alb_data(i,j,k)=(rho_0d(i,j,k)+rho_dd(i,j,k))/2.
            end if
         end do
      end do
   end do

   !----------------------------------------------------------------------------

   ! Open Aerosol CCI file (optional)
   if (aerosol_processing_mode .ge. 2) then
      call ncdf_open(ncid, Faerosol, 'process_broadband_fluxes()')

      ! Get dimension
      nc_aer = ncdf_dim_length(ncid, 'pixel_number', 'process_broadband_fluxes()')

      ! Allocate arrays
      allocate(aerLon(nc_aer))
      allocate(aerLat(nc_aer))
      allocate(aerAOD(nc_aer))
      allocate(aerREF(nc_aer))
      allocate(aerQflag(nc_aer))

      ! Read Aerosol data
      call ncdf_read_array(ncid, "longitude", aerLon)
      call ncdf_read_array(ncid, "latitude", aerLat)
      call ncdf_read_array(ncid, "AOD550", aerAOD)
      call ncdf_read_array(ncid, "REFF", aerREF)
      call ncdf_read_array(ncid, "quality_flag", aerQflag)

      ! Close file
      call ncdf_close(ncid, 'process_broadband_fluxes(Faerosol)')
   end if

   ! Open Aerosol Primary File (optional)
   if (aerosol_processing_mode .eq. 1) then
      print*, trim(Faerosol)
      call ncdf_open(ncid, Faerosol, 'process_broadband_fluxes()')

      allocate(aerAOD1(xN,yN))
      allocate(aerREF1(xN,yN))
      call ncdf_read_array(ncid, "aot550", aerAOD1)
      call ncdf_read_array(ncid, "aer", aerREF1)

      ! Close file
      call ncdf_close(ncid, 'process_broadband_fluxes(Faerosol)')
   end if



   !----------------------------------------------------------------------------
   ! Allocate arrays
   !----------------------------------------------------------------------------

   ! Interpolated PRTM arrays
   allocate(inP(levdim_prtm))
   allocate(inT_(levdim_prtm))
   allocate(inH(levdim_prtm))
   allocate(inQ(levdim_prtm))
   allocate(inO3(levdim_prtm))

   ! Allocate OUTPUT variables
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
   allocate(AOD550(xN,yN))
   allocate(AREF(xN,yN))
   allocate(cbh(xN,yN))

   ! Fill OUTPUT with missing
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
   cbh(:,:) = sreal_fill_value

   ! Re-grid PRTM vertical profile to match bugsrad resolution (NLS)
   do i = 1, NLS
      mask_vres(i)=floor(i*(levdim_prtm/(NLS*1.)))
   end do
   ! Top and bottom of BUGSrad profile need to be at the same level as PRTM
   mask_vres(1)=1
   mask_vres(NLS)=levdim_prtm
   print*, mask_vres


   !----------------------------------------------------------------------------
   ! Optional inputs
   !----------------------------------------------------------------------------
   ! OPTION - PROCESS all pixels in granule if range not specified
   if (len_trim(cpxX0) .eq. 0 .and. len_trim(cpxX1) .eq. 0 .and. &
       len_trim(cpxY0) .eq. 0 .and. len_trim(cpxY1) .eq. 0) then
      print*, 'PROCESS ALL SATELLITE PIXELS'
      pxX0 = 1
      pxX1 = xN
      pxY0 = 1
      pxY1 = yN
   end if
   ! Override if 0's are given
   if (pxX0 .eq. 0) pxX0 = 1
   if (pxX1 .eq. 0) pxX1 = xN
   if (pxY0 .eq. 0) pxY0 = 1
   if (pxY1 .eq. 0) pxY1 = yN

   ! OPTION - PROCESS aerosol
   print*, 'aerosol_processing_mode: ', aerosol_processing_mode
   if (aerosol_processing_mode .ge. 2) then
      allocate(aID(xN,yN))
      ! determine if netcdf file already exists
      inquire(file=trim(Fcollocation), exist=there)
      if (.not. there) then
         call collocate_aerosol2cloud(nc_aer, aerLon, aerLat, xN, yN, LON, LAT, aID)

         !----------------------------------------------------------------------
         ! Make collocation netcdf file
         !----------------------------------------------------------------------
         if (aerosol_processing_mode .eq. 3) then
            call ncdf_open(ncid, Fprimary, 'process_broadband_fluxes()')

            ! Get common attributes from primary file
            call ncdf_get_common_attributes(ncid, global_atts, source_atts)

            call ncdf_close(ncid, 'process_broadband_fluxes(Fprimary)')
            ! Dimensions
            ixstart = 1
            ixstop = xN
            iystart = 1
            iystop = yN
            n_x = ixstop - ixstart + 1
            n_y = iystop - iystart + 1
            n_v = 1
            ! Create netcdf file
            call ncdf_create(trim(Fcollocation), ncid, ixstop-ixstart+1, &
                 iystop-iystart+1, n_v, dim3d_var, 1, global_atts, source_atts)
            dims_var = dim3d_var(1:2)

            ! Need this to exit data mode to define variables
            if (nf90_redef(ncid) .ne. NF90_NOERR) then
               write(*,*) 'ERROR: nf90_redef()'
               stop error_stop_code
            end if

            !-------------------------------------------------------------------
            ! Aerosol Index
            !-------------------------------------------------------------------
            call ncdf_def_var_long_packed_long( &
                 ncid, &
                 dims_var, &
                 'aID', &
                 aID_vid, &
                 verbose, &
                 long_name     = 'aerosol 10-km pixel index location', &
                 standard_name = 'aID_standard', &
                 fill_value    = lint_fill_value)

            ! Need to exit define mode to write data
            if (nf90_enddef(ncid) .ne. NF90_NOERR) then
               write(*,*) 'ERROR: nf90_enddef()'
               stop error_stop_code
            end if

            ! Write the array to the netcdf file
            call ncdf_write_array(ncid,'aID', aID_vid,&
                 aID(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

            ! Close netcdf file
            call ncdf_close(ncid, 'process_broadband_fluxes()')

            print*, 'CREATED: '
            print*, trim(Fcollocation)
         end if;aerosol_processing_mode = 3
      end if ! file not there

      ! Netcdf collocation file exists get aID
      if (there) then
         print*, 'Extracting data from:'
         write(*,*) trim(Fcollocation)
         ! Open Collocation file
         call ncdf_open(ncid, Fcollocation, 'process_broadband_fluxes()')

         ! Read collocation data
         call ncdf_read_array(ncid, "aID", aID)

         ! Close file
         call ncdf_close(ncid, 'process_broadband_fluxes()')
      end if !file there

      ! Fill arrays (aerosol index aID must exist by this point)
      do i = 1, xN
         do j = 1, yN
            AOD550(i,j) = aerAOD(aID(i,j))
            AREF(i,j) = aerREF(aID(i,j))
         end do
      end do

   end if ! end aerosol collocation option

   if (aerosol_processing_mode .eq. 0) then
      ! Fill arrays
      do i = 1, xN
         do j = 1, yN
            AOD550(i,j) = -999.
            AREF(i,j) = -999.
         end do
      end do
   end if

   if (aerosol_processing_mode .eq. 1) then
      ! Fill arrays
      do i = 1, xN
         do j = 1, yN
            AOD550(i,j) = aerAOD1(i,j)
            AREF(i,j) = aerREF1(i,j)
         end do
      end do
   end if

   if (algorithm_processing_mode .eq. 1) print*, 'Algorithm: BUGSrad'
   if (algorithm_processing_mode .eq. 2) print*, 'Algorithm: FuLiou 2G'
   if (algorithm_processing_mode .eq. 3) print*, 'Algorithm: FuLiou 4S'
   if (algorithm_processing_mode .eq. 4) print*, 'Algorithm: FuLiou 2S'

   ! Read MODIS HDF data
   if (len_trim(FMOD04) .gt. 0 .and. &
       len_trim(FMOD04) .ne. path_length) then
      print*, 'Using MODIS aerosol input --> replacing ORAC'
      call get_modis_aerosol(trim(FMOD04), xN, yN, AREF, AOD550)
   end if

   if (len_trim(FMOD06) .gt. 0 .and. &
       len_trim(FMOD06) .ne. path_length) then
      print*, 'Using MODIS cloud input --> replacing ORAC'
      call get_modis_cloud(trim(FMOD06), xN, yN, CTT, CTP, CTH, phase, REF, COT, cc_tot)
   end if

   if (lut_mode .eq. 0) print*, 'Process using radiation model'
   if (lut_mode .eq. 1) print*, 'Process using LUT'

   !----------------------------------------------------------------------------
   ! End optional inputs section
   !----------------------------------------------------------------------------


   !----------------------------------------------------------------------------
   ! Begin main code
   !----------------------------------------------------------------------------
   call cpu_time(cpuStart)

   print*, 'Processing Pixel Range:'
   print*, 'x-start = ', pxX0
   print*, 'y-start = ', pxY0
   print*, 'x-end = ', pxX1
   print*, 'y-end = ', pxY1
   print*, 'Across Track # = ', pxX1 - pxX0 + 2
   print*, 'Along Track #  = ', pxY1 - pxY0 + 1

   ! loop over cross-track dimension
   do i = pxX0, pxX1
      call cpu_time(cpuFinish)
      if (mod(i, 50) .eq. 0) then
         print*, 'complete: ', i*100./(xN*1.), &
              '%   i=', i, cpuFinish-cpuStart,' seconds elapsed'
      end if

      ! loop over along-track dimension
      do j = pxY0, pxY1

         ! Valid lat/lon required to run (needed for SEVIRI)
         if (LAT(i,j) .ne. -999.0 .and. LON(i,j) .ne. -999.0) then

            !-------------------------------------------------------------------
            ! Surface albedo
            ! Interpolate narrowband BRDF radiances to broadband BUGSrad radiances
            !-------------------------------------------------------------------
            ! BugsRAD surface properties
            if (algorithm_processing_mode .eq. 1) then
               call preprocess_bugsrad_sfc_albedo(nc_alb, rho_0d(i,j,:), &
                    rho_dd(i,j,:), rho_0d_bugsrad, rho_dd_bugsrad)
               call preprocess_bugsrad_sfc_emissivity(nc_emis, emis_data(i,j,:), &
                    emis_bugsrad)
            end if

            ! FuLiou surface properties
            if (algorithm_processing_mode .eq. 2 .or. &
                algorithm_processing_mode .eq. 3 .or. &
                algorithm_processing_mode .eq. 4) then
               call preprocess_fuliou_sfc_albedo(nc_alb, rho_0d(i,j,:), &
                    rho_dd(i,j,:), rho_0d_fuliou, rho_dd_fuliou)
               call preprocess_bugsrad_sfc_emissivity(nc_emis, emis_data(i,j,:), &
                    emis_fuliou)
            end if

!           print*,'BUGSrad (blacksky) ',rho_0d_bugsrad
!           print*,'BUGSrad (whitesky) ',rho_dd_bugsrad
!           print*,'BUGSrad emissivity ',emis_bugsrad
!           print*,'Fu Liou (blacksky) ',rho_0d_fuliou
!           print*,'Fu Liou (whitesky) ',rho_dd_fuliou
!           print*,'Fu Liou emissivity ',emis_fuliou

            ! Solar zenith angle
            pxTheta = COS( SOLZ(i,j) * Pi/180.)

            ! Solar zenith angle condition (remove nighttime & twilight)
!           if ( SOLZ(i,j) .lt. 80.) then

            ! Meteorology
            call interpolate_meteorology(lon_prtm, lat_prtm, levdim_prtm,&
                 xdim_prtm, ydim_prtm, P, T, H, Q, O3,&
                 LON(i,j), LAT(i,j), inP, inT_, inH, inQ, inO3)

            ! Collocate PRTM vertical resolution to BUGSrad profile resolution
            ! (31 levels)
            pxZ = inH(mask_vres)
            pxP = inP(mask_vres)
            pxT = inT_(mask_vres)
            pxQ = inQ(mask_vres)
            pxO3 = inO3(mask_vres)

            ! Skin temperature - currently bottom level as defined in
            ! rttov_driver.F90
!           pxts = inT_(levdim_prtm)

            ! Check if STEMP is valid, if not use ECMWF value.
            if ((STEMP(i,j) .lt. 0) .or. (STEMP(i,j) .gt. 400)) then
               pxts = inT_(levdim_prtm)
            else
               pxts = STEMP(i,j)
            end if

            ! Cloud base & top height calculation
            pxREF(:)       = -999.
            pxCOT(:)       = -999.
            pxHctop(:)     = -999.
            pxHcbase(:)    = -999.
            pxPhaseFlag(:) = -999.
            pxHctopID(:)   = -999.
            pxHcbaseID(:)  = -999.

            ml_flag = 1
            ! cloud base & top height calculation
            call preprocess_input(cc_tot(i,j), AREF(i,j), AOD550(i,j), phase(i,j),&
                    CTT(i,j), CTP(i,j), REF(i,j), COT(i,j), CTH(i,j), InfThnCld,&
                    NLS, pxZ, tmp_pxREF, tmp_pxCOT, tmp_pxHctop, tmp_pxHcbase,&
                    tmp_pxPhaseFlag, pxLayerType,&
                    pxregime, tmp_pxHctopID, tmp_pxHcbaseID)

            pxREF(1)       = tmp_pxREF
            pxCOT(1)       = tmp_pxCOT
            pxHctop(1)     = tmp_pxHctop
            pxHcbase(1)    = tmp_pxHcbase
            pxPhaseFlag(1) = tmp_pxPhaseFlag
            pxHctopID(1)   = tmp_pxHctopID(1)
            pxHcbaseID(1)  = tmp_pxHcbaseID(1)


            if (multi_layer .EQ. 1) then
               if (cot2(i,j) .gt. 0) then
                  ! ML code has -999. for CTP but CTH exists see correction below
                  if (CTP2(i,j) .LT. 0. .AND. CTH2(i,j) .GT. 0.) then
                     print*, CTP2(i,j), CTH2(i,j)
                     print*,(pxP(MINLOC(ABS(CTH2(i,j)-pxZ)))+ &
                          pxP(MINLOC(ABS(CTH2(i,j)-pxZ))+1))/2.
                     ! poor man's interpolation method but gets close...
                     tmpVal = (pxP( MINLOC(ABS(CTH2(i,j)-pxZ)) )+ &
                          pxP( MINLOC(ABS(CTH2(i,j)-pxZ))+1))/2.
                     CTP2(i,j) = tmpVal(1)
                  end if

                  ml_flag = 2
                  call preprocess_input(cc_tot2(i,j), AREF(i,j), AOD550(i,j), phase(i,j),&
                       CTT2(i,j), CTP2(i,j), REF2(i,j), COT2(i,j), CTH2(i,j), InfThnCld,&
                       NLS, pxZ, tmp_pxREF, tmp_pxCOT, tmp_pxHctop, tmp_pxHcbase,&
                       tmp_pxPhaseFlag, pxLayerType,&
                       pxregime, tmp_pxHctopID, tmp_pxHcbaseID)
                  
                  pxREF(2)       = tmp_pxREF
                  pxCOT(2)       = tmp_pxCOT
                  pxHctop(2)     = tmp_pxHctop
                  pxHcbase(2)    = tmp_pxHcbase
                  pxPhaseFlag(2) = tmp_pxPhaseFlag
                  pxHctopID(2)   = tmp_pxHctopID(1)
                  pxHcbaseID(2)  = tmp_pxHcbaseID(1)
                  pxregime = 7
               end if
            end if

            if (pxregime .eq. 4) ml_flag = 0

            ! Run full radiation code (not LUT mode)
            if (lut_mode .eq. 0) then

               !----------------------------------------------------------------
               ! Call BUGSrad algorithm
               !----------------------------------------------------------------
               if (algorithm_processing_mode .eq. 1) then
                  call driver_for_bugsrad(NL, pxTSI, pxtheta, pxAsfcSWRdr,&
                          pxAsfcNIRdr, pxAsfcSWRdf, pxAsfcNIRdf, pxts,&
                          pxPhaseFlag, ml_flag, pxREF, pxCOT, pxHctop, pxHcbase,&
                          pxHctopID, pxHcbaseID,&
                          pxZ, pxP, pxT, pxQ, pxO3,&
                          pxtoalwup, pxtoaswdn, pxtoaswup,&
                          pxboalwup, pxboalwdn, pxboaswdn, pxboaswup,&
                          pxtoalwupclr, pxtoaswupclr,&
                          pxboalwupclr, pxboalwdnclr, pxboaswupclr, pxboaswdnclr,&
                          bpar, bpardif, tpar,&
                          ulwfx, dlwfx, uswfx, dswfx,&
                          ulwfxclr, dlwfxclr, uswfxclr, dswfxclr,&
                          emis_bugsrad, rho_0d_bugsrad, rho_dd_bugsrad, pxYEAR)
               end if ! BUGSrad algorithm

               !----------------------------------------------------------------
               ! Call FuLiou algorithm
               !----------------------------------------------------------------
               if (algorithm_processing_mode .eq. 2 .or. &
                   algorithm_processing_mode .eq. 3 .or. &
                   algorithm_processing_mode .eq. 4) then
                  call driver_for_fuliou(NL, pxTSI, pxtheta, pxAsfcSWRdr, pxAsfcNIRdr,&
                          pxAsfcSWRdf, pxAsfcNIRdf, pxts,&
                          pxPhaseFlag, ml_flag, pxREF, pxCOT, pxHctop, pxHcbase,&
                          pxHctopID, pxHcbaseID,&
                          pxZ, pxP, pxT, pxQ, pxO3,&
                          pxtoalwup, pxtoaswdn, pxtoaswup,&
                          pxboalwup, pxboalwdn, pxboaswdn, pxboaswup,&
                          pxtoalwupclr, pxtoaswupclr,&
                          pxboalwupclr, pxboalwdnclr, pxboaswupclr, pxboaswdnclr,&
                          bpar, bpardif, tpar,&
                          ulwfx, dlwfx, uswfx, dswfx,&
                          ulwfxclr, dlwfxclr, uswfxclr, dswfxclr,&
                          emis_fuliou, rho_0d_fuliou, rho_dd_fuliou,&
                          algorithm_processing_mode)
               end if ! FuLiou algorithm
            end if ! lut_mode = 0

            !-------------------------------------------------------------------
            ! LUT mode
            !-------------------------------------------------------------------
            if (lut_mode .eq. 1) then
               call driver_for_lut(pxTSI, pxregime,&
                       nASFC, LUT_SFC_ALB,&
                       nRE, lut_ref, nTAU, lut_cot, nSOLZ, lut_solz,&
                       LUT_toa_sw_albedo(:,:,:,:),&
                       LUT_boa_sw_transmission(:,:,:,:),&
                       LUT_boa_sw_albedo(:,:,:,:),&
                       alb_data(I,J,1:), REF(I,J), COT(I,J), SOLZ(I,J),&
                       pxtoalwup, pxtoaswdn, pxtoaswup,&
                       pxboalwup, pxboalwdn, pxboaswdn, pxboaswup,&
                       pxtoalwupclr, pxtoaswupclr,&
                       pxboalwupclr, pxboalwdnclr, pxboaswupclr, pxboaswdnclr,&
                       bpar, bpardif, tpar)
            end if

            !-------------------------------------------------------------------
            ! Quality check retrieved data & fill output arrays
            !-------------------------------------------------------------------
            ! Catch NaN
            nanFlag = 0
            if (is_nan(pxtoalwup)) nanFlag = 1
            if (is_nan(pxtoaswup)) nanFlag = 1
            if (is_nan(pxtoalwupclr)) nanFlag = 1
            if (is_nan(pxtoaswupclr)) nanFlag = 1

            ! Catch unphysical values
            if (pxtoalwup .lt. 0. .or. pxtoalwup .gt. 1000.) nanFlag = 1
            if (pxtoaswup .lt. 0. .or. pxtoaswup .gt. 1600.) nanFlag = 1

            ! Regime type
            retrflag(i,j) = pxregime

            ! Valid data only
            if (nanFlag == 0) then
               ! NetCDF output arrays
               ! Observed
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

               ! Clear-sky retrieval
               toa_lwup_clr(i,j) = pxtoalwupclr
               toa_swup_clr(i,j) = pxtoaswupclr
               boa_lwup_clr(i,j) = pxboalwupclr
               boa_lwdn_clr(i,j) = pxboalwdnclr
               boa_swup_clr(i,j) = pxboaswupclr
               boa_swdn_clr(i,j) = pxboaswdnclr

               ! PAR
               toa_par_tot(i,j) = tpar * pxPAR_WEIGHT
               boa_par_tot(i,j) = bpar * pxPAR_WEIGHT
               boa_par_dif(i,j) = bpardif * pxPAR_WEIGHT

               ! Derived properties
               cbh(i,j) = pxHcbase(1)
            end if ! valid data

            ! meteorology data to output in netCDF file
            boa_tsfc(i,j) = pxT(NLS)
            boa_psfc(i,j) = pxP(NLS)
            boa_qsfc(i,j) = pxQ(NLS)
            call compute_lts(NL, pxP, pxT, pxLTS)
            call compute_fth(NL, pxP, pxT, pxQ, pxFTH)
            call compute_column_o3(NL, pxZ, pxO3, pxcolO3)
            lts(i,j) = pxLTS
            fth(i,j) = pxFTH
            colO3(i,j) = pxcolO3

         end if ! valid geolocation data
      end do ! j-loop
   end do ! i-loop

   call cpu_time(cpuFinish)
   print*, cpuFinish-cpuStart,' seconds elapsed'


   !----------------------------------------------------------------------------
   ! Make output NetCDF file
   !----------------------------------------------------------------------------
   call ncdf_open(ncid, Fprimary, 'process_broadband_fluxes()')

   ! Get common attributes from primary file
   call ncdf_get_common_attributes(ncid, global_atts, source_atts)

   call ncdf_close(ncid, 'process_broadband_fluxes()')

   ! Dimensions
   ixstart = pxX0
   ixstop  = pxX1
   iystart = pxY0
   iystop  = pxY1
   n_x = ixstop - ixstart + 1
   n_y = iystop - iystart + 1
   n_v = 1

   ! Create netcdf file
   call ncdf_create(trim(fname), ncid, ixstop-ixstart+1, &
        iystop-iystart+1, n_v, dim3d_var, 1, global_atts, source_atts)
   dims_var = dim3d_var(1:2)

   ! Need this to exit data mode to define variables
   if (nf90_redef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_redef()'
      stop error_stop_code
   end if

   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   call ncdf_def_var_double_packed_double( &
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

   !----------------------------------------------------------------------------
   ! latitude
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! longitude
   !----------------------------------------------------------------------------
    call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! retrieval flag
   !----------------------------------------------------------------------------
   call ncdf_def_var_byte_packed_byte( &
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
        valid_max     = int(6, byte), &
        units         = '1', &
        flag_values   = '1b 2b 3b 4b 5b 6b', &
        flag_meanings = 'oc_liq-cld oc_ice-cld clear-AOD clear_no_AOD joint_aod-liq_cld joint_aod-ice_cld', &
        deflate_level = deflate_lv, &
        shuffle       = shuffle_flag)

   !----------------------------------------------------------------------------
   ! toa_incoming_shortwave_flux
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! toa_outgoing_shortwave_flux
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! toa_outgoing_longwave_flux
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_downwelling_shortwave_flux_in_air
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_upwelling_shortwave_flux_in_air
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_upwelling_longwave_flux_in_air
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_downwelling_longwave_flux_in_air
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! toa_outgoing_shortwave_flux_assuming_clear_sky
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! toa_outgoing_longwave_flux_assuming_clear_sky
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! boa_downwelling_shortwave_flux_in_air_assuming_clear_sky
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! boa_upwelling_shortwave_flux_in_air_assuming_clear_sky
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! boa_upwelling_longwave_flux_in_air_assuming_clear_sky
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! boa_downwelling_longwave_flux_in_air_assuming_clear_sky
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_downwelling_photosynthetic_radiative_flux_in_air
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! toa_incoming_photosynthetic_radiative_flux
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_air_temperature
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_air_pressure
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! surface_specific_humidity
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! lower_troposphere_stability
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! free_troposphere_humidity
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! Column_Ozone
   !----------------------------------------------------------------------------
   write(*,*) ncid, dims_var, 'colO3', colO3_vid, verbose, deflate_lv, shuffle_flag

   call ncdf_def_var_float_packed_float( &
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

   !----------------------------------------------------------------------------
   ! Cloud_Base_Height
   !----------------------------------------------------------------------------
   call ncdf_def_var_float_packed_float( &
        ncid, &
        dims_var, &
        'cbh', &
        cbh_vid, &
        verbose, &
        long_name     = 'derived adiabatic cloud base height', &
        standard_name = 'cloud_base_height', &
        fill_value    = sreal_fill_value, &
        scale_factor  = real(1), &
        add_offset    = real(0), &
        valid_min     = real(0, sreal), &
        valid_max     = real(1500, sreal), &
        units         = 'km', &
        deflate_level = deflate_lv, &
        shuffle       = shuffle_flag)


   ! Need to exit define mode to write data
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

   ! write the array to the netcdf file
   call ncdf_write_array(ncid,'time', TIME_vid,&
        time_data(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'lat', LAT_vid,&
        lat_data(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'lon', LON_vid,&
        lon_data(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'retrflag', retrflag_vid,&
        retrflag(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'toa_swdn', toa_swdn_vid,&
        toa_swdn(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'toa_swup', toa_swup_vid,&
        toa_swup(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'toa_lwup', toa_lwup_vid,&
        toa_lwup(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_swdn', boa_swdn_vid,&
        boa_swdn(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_swup', boa_swup_vid,&
        boa_swup(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_lwup', boa_lwup_vid,&
        boa_lwup(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_lwdn', boa_lwdn_vid,&
        boa_lwdn(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'toa_swup_clr', toa_swup_clr_vid,&
        toa_swup_clr(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'toa_lwup_clr', toa_lwup_clr_vid,&
        toa_lwup_clr(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_swdn_clr', boa_swdn_clr_vid,&
        boa_swdn_clr(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_swup_clr', boa_swup_clr_vid,&
        boa_swup_clr(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_lwup_clr', boa_lwup_clr_vid,&
        boa_lwup_clr(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_lwdn_clr', boa_lwdn_clr_vid,&
        boa_lwdn_clr(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_par_dif', boa_par_dif_vid,&
        boa_par_dif(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_par_tot', boa_par_tot_vid,&
        boa_par_tot(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'toa_par_tot', toa_par_tot_vid,&
        toa_par_tot(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_tsfc', boa_tsfc_vid,&
        boa_tsfc(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_psfc', boa_psfc_vid,&
        boa_psfc(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'boa_qsfc', boa_qsfc_vid,&
        boa_qsfc(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'lts', lts_vid,&
        lts(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'fth', fth_vid,&
        fth(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'colO3', colO3_vid,&
        colO3(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   call ncdf_write_array(ncid,'cbh', cbh_vid,&
        cbh(ixstart:,iystart:), 1, 1, n_x, 1, 1, n_y)

   ! Close netcdf file
   call ncdf_close(ncid, 'process_broadband_fluxes()')

   print*, 'CREATED:'
   print*, TRIM(fname)

#ifdef WRAPPER
end subroutine process_broadband_fluxes
#else
end program process_broadband_fluxes
#endif
