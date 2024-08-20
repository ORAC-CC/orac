!-------------------------------------------------------------------------------
! Name: cloud_typing_pavolonis.F90
!
! Purpose:
! Module holding algorithm to determine cloud type as a function of satellite
! radiances and land surface cover.
!
! History:
! 2014/10/23, CS: Original version.
! 2014/11/04, OS: ecmwf structure containing skin temperature now passed as
!    argument; bilinear interpolation of skin temperature on orbit grid; added
!    snow/ice and skin temperature to NN cloud mask call arguments
! 2014/11/20, OS: BTD_Ch4_Ch3b is now calculated within NN cloud mask; flag
!    ch3a_on_avhrr_flag is passed as an argument to NN call
! 2014/12/01, OS: cloud type now set to PROB_OPAQUE_ICE_TYPE for twilight/night
!    extra-polar regions if Ch3b is missing; significant bug fix: wrong ch3b
!    emissivity and reflectance were used in old code,  which are now correctly
!    calculated and applied
! 2014/12/03, OS: use default coefficients for AATSR until these will be
!    calculated and implemented here; skip pixel if input solzen is negative;
!    pass satzen as argument to NN cloud mask
! 2014/12/03, GM: Added Planck coefficients for AATSR derived from SADChan
!    Planck coefficients.
! 2014/12/03, OS: changed nrows of coefficients and reshape command to include
!    new AATSR coefficients
! 2014/12/01, CP: added check for missing AATSR 12 um channel
! 2014/12/10, GM: Fixed the last change above.
! 2014/12/31, GM: Parallelized the cloud typing loop with OpenMP.
! 2015/01/14, AP: Channel indexing now selected at the start of the routine
!    rather than hardcoded.
! 2015/02/05, CP: added check for missing warm AATSR 11 um channel
! 2015/02/05, CP: fixed bug that removed changed made 1st of December
! 2015/02/19, GM: Added SEVIRI support.
! 2015/04/16, SS: correct NIR thresholds for 3.7 µm channel -> divide by 100
! 2015/04/16, OS+SS: removed setting prob_opaque_ice if both NIR channels are
!    missing and replaced it with IR-only Test.
! 2015/04/20, SS: fixes and changes in cirrus/overlap spatial filter fix
!    missing degree-to-radians factor for coszen, changed coszen from cos(sunZA)
!    to cos(satZA)
! 2015/04/22, SS: introduce solar correction for reflectance channels ch1 and
!    ch3
! 2015/04/29, CP: changed from Env to Envisat
! 2015/05/19, GM: Fixed several race conditions introduced from recent commits
!    occurring when using OpenMP.
! 2015/07/03, OS: Added cldmask_uncertainty; added coefficients to calculate
!    NOAA19 Ch3.7 reflectance + slight update for other platforms
! 2015/07/27, AP: Replaced sym structure with parameters.
! 2015/11/17, OS: Added interpolation of ERA-Interim surface fields snow_depth
!    and sea_ice_cover; data are used for defining snow/ice mask (here called
!    NISE_MASK) input to neural net
! 2015/11/18, OS: Replaced surface%NISE_MASK with local variable snow_ice_mask
! 2015/11/23, OS: Fixed bug that could lead to uninitialised variables when
!    ch3a was available for solzen > 88.
! 2015/11/28  CP: used probabaly clear to deal with saturation of 12um channel
!    over antarctica in ATSR channel
! 2016/01/21  OS: Removed bug due to differences in land/sea masks between
!    ERA-Interim and USGS
! 2016/01/21  OS: Removed bug: test with AATSR flag was also applied to other
!    sensors
! 2016/02/05, OS: Cloud mask now uses albedo for glint correction.
! 2016/02/18, OS: ECMWF snow/ice mask now corrected by USGS land/sea mask
! 2016/03/04, OS: bug fix in setting index when passing surface%albedo
! 2016/04/14, SP: Added support for Himawari/AHI
! 2016/04/09, SP: Add multiple views.
! 2016/05/17, SP: Added support for the VIIRS instrument
! 2016/07/05, SP: Added support for the SLSTR instrument
! 2016/08/04, GM: Add sw1 and sw2 indices for surface%albedo which is indexed
!    only for SW.
! 2017/04/08, SP: New flag to disable VIS processing, saves proc time (ExtWork)
! 2017/06/20, OS: Added correction for differences in spectral reponse
!    functions between NOAA19 and all other platforms; for IR only mode, set
!    Ch3b to fill value during daytime due to solar contamination; added new
!    ANN phase mask; Added Planck inversion coefficients for several sensors.
! 2017/07/07, GM: Put the masking/typing loop contents in to a subroutine so all
!    OpenMP private variables as well as new ones in the future are implicitly
!    private.
! 2017/07/07, GM: Give parameters in pavolonis_fill_coefficients.inc the
!    parameter attribute and take them off the cloud_type_pixel() stack by
!    giving them module scope.
! 2017/07/08, GM: Much needed tidying.
! 2017/12/20, GT: Changed Sentinel-3 platform name to Sentinel3a or Sentinel3b
! 2019/08/14, SP: Add Fengyun-4A support.
! 2021/12/14, DP: Added external SEVIRI ANN
! 2021/01/21, DP: Added spectral response correction for MSG1/MSG2/MSG3/MSG4
!                 SEVIRI and Sentinel-3A/Sentinel-3B SLSTR.
! 2024/03/13, GT: Added a check for missing values in the 11 micron band before
!                 attempting cloud masking/typing. This catches pixels which
!                 exist and are geolocated, but have no data.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-----------------------------------------------------------------------
! **** Upstream header ****
!-----------------------------------------------------------------------
!
! Clouds from AVHRR Extended (CLAVR-x) 1b PROCESSING SOFTWARE
! Version 5.3
!
! NAME: cloud_type.f90 (src)
!       CLOUD_TYPING (program)
!
! PURPOSE: This module performs a cloud typing decision on
!          pixel by pixel basis
!
! DESCRIPTION:
!
! AUTHORS:
!  Andrew Heidinger, Andrew.Heidinger@noaa.gov
!  Michael Pavolonis (NOAA/NESDIS)
!
! COPYRIGHT
! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
! SUPPORT TO USERS.
!
! REVISION HISTORY:
!   October 2006, Added retype routine - Heidinger
!   March 2012 - Added check for solar contamination flag
!
! Subroutines included in module:
!   cloud_type
!   cloud_retype
!
!-----------------------------------------------------------------------

module cloud_typing_pavolonis_m

   use common_constants_m

   implicit none

   private

   public :: CLOUD_TYPE !, CLOUD_RETYPE

   integer, parameter, private:: n_box=3

   ! Load external file containing fill coefficients
   include 'pavolonis_fill_coefficients.inc'

contains

!-----------------------------------------------------------------------
! **** Upstream header ****
!-----------------------------------------------------------------------
!
! SUBROUTINE NAME: CLOUD_TYPE
!-----------------------------------------------------------------------
!
! This subroutine performs a cloud typing decision on pixel by pixel
! basis the resulting cloud mask codes are
!
!    defined in constants_cloud_typing_pavolonis.f90:
!
!    INTEGER(kind=sint) :: CLEAR_TYPE = 0
!    INTEGER(kind=sint) :: PROB_CLEAR_TYPE = 1
!    INTEGER(kind=sint) :: FOG_TYPE = 2
!    INTEGER(kind=sint) :: WATER_TYPE = 3
!    INTEGER(kind=sint) :: SUPERCOOLED_TYPE = 4
!    INTEGER(kind=sint) :: OPAQUE_ICE_TYPE = 6
!    INTEGER(kind=sint) :: CIRRUS_TYPE = 7
!    INTEGER(kind=sint) :: OVERLAP_TYPE = 8
!    INTEGER(kind=sint) :: PROB_OPAQUE = 9
!
!
! INPUTS:
!  j1 - the first scan index to process
!  j2 - the last scan index to process
!
! OUTPUTS:
!
! CALLING SEQUENCE:  call CLOUD_TYPE(j_min, num_scans_read)
!                    (called from ORAC - CC4CL)
!
! SIDE EFFECTS: None
!
! MODIFICATIONS:
!   april 2005 - added spatial filtering to prevent ct =5,6 for
!                isolated pixels in regions where there are no
!                cold pixels
!
!   August 30, 2005 - cleaned-up code and eliminated cirrus
!                     quality-based filter; only Bt_Ch31 filter
!                     is now used - mpav
!
!   April 14, 2006 - Modified to process on an arbitrary range of
!                    scans
!-----------------------------------------------------------------------
!
! LOCAL VARIABLES:
!
! NIR_PHASE_THRES             = 1.6 micron phase threshold
! NIR_CIRRUS_THRES            = 1.6 micron cirrus threshold
! NIR_OVER_THRES              = Minimum 1.6 um reflectance allowed
!                               for cloud overlap over SNOW/ICE.
! BTD3811_PHASE_THRES         = 3.75um - 11um thresholds used for phase determination.
! EMS38_PHASE_THRES           = 3.75 um thresholds used for phase determination.
! BTD1112_DOVERLAP_THRES      = 11um - 12um cloud overlap threshold
! BTD1112_CIRRUS_THRES        = 11um - 12um cirrus threshold
! BTD1112_NOVERLAP_THRES_L    = Split window nighttime low cloud overlap threshold
! BTD1112_NOVERLAP_THRES_H    = Split window nighttime high cloud overlap threshold
! EMS38_NOVERLAP_THRES_L      = EMS38 nighttime low cloud overlap threshold
! EMS38_NOVERLAP_THRES_H      = EMS38 nighttime high cloud overlap threshold
! MIN_BTD1112_DOVERLAP        = The minimum 11um - 12um BTD allowed
!  for overlap detection.
! MIN_BTD1112_NOVERLAP        = The minimum allowed Bt_Ch31 -
!  Bt_Ch32 allowed for nighttime overlap
! A1                          = Coefficient needed to determine the 11um - 12um BTD for cirrus detection
! B1                          = Coefficient needed to determine the 11um -
!                               12um BTD for cirrus detection
! C1                          = Coefficient needed to determine the 11um -
!                               12um BTD for cirrus detection
! D1                          = Coefficient needed to determine the 11um -
!                               12um BTD for cirrus detection
! E1                          = Coefficient needed to determine the 11um -
!                               12um BTD for cirrus detection
! A2                          = Coefficient needed to determine the 3.75um - 11um BTD thresholds that
!                               differentiate between ice and water as a function of 11um - 12um BTD.
! B2                          = Coefficient needed to determine the 3.75um - 11um BTD thresholds that
!                               differentiate between ice and water as a function of 11um - 12um BTD.
! C2                          = Coefficient needed to determine the 3.75um - 11um BTD thresholds that
!                               differentiate between ice and water as a function of 11um - 12um BTD.
! D2                          = Coefficient needed to determine the 3.75um - 11um BTD thresholds that
!                               differentiate between ice and water as a function of 11um - 12um BTD.
! E2                          = Coefficient needed to determine the 3.75um - 11um BTD thresholds that
!                               differentiate between ice and water as a function of 11um - 12um BTD.
! A3                          = Coefficient needed to determine the 11um - 12um BTD used to find cloud
!                               overlap as a function of 0.65 um reflectance.
! B3                          = Coefficient needed to determine the 11um - 12um BTD used to find cloud
!                               overlap as a function of 0.65 um reflectance.
! C3                          = Coefficient needed to determine the 11um - 12um BTD used to find cloud
!                               overlap as a function of 0.65 um reflectance.
! D3                          = Coefficient needed to determine the 11um - 12um BTD used to find cloud
!                               overlap as a function of 0.65 um reflectance.
! E3                          = Coefficient needed to determine the 11um - 12um BTD used to find cloud
!                               overlap as a function of 0.65 um reflectance.
!
! i                           = pixel counter
! j                           = scanline counter
! j1                          = start scanline
! j2                          = ending scanline
! index1                      = viewing zenith angle bin
! index2                      = solar zenith angle bin
! wflg                        = IR window flag
! start_line                  = line to start filtering
! end_line                    = line to end filtering
! start_pix                   = pixel to start filtering
! end_pix                     = pixel to end filtering
! npix                        = number of pixels to filer
!
! day                         = day/night flag
!
! t4_filter_thresh            = BT4 threshold, accounting for atmospheric effects, for filtering
! nir_ref                     = channel 3a/b reflectance
!
! Not used: A4, B4, C4, D4, E4, n
!
!-----------------------------------------------------------------------

subroutine cloud_type(channel_info, sensor, surface, imager_flags, &
     imager_angles, imager_geolocation, imager_measurements, imager_pavolonis, &
     ecmwf, platform, doy, do_ironly, do_spectral_response_correction, &
     use_seviri_ann_cma_cph, use_seviri_ann_ctp_fg, use_seviri_ann_mlay, do_nasa, &
     verbose)

   use channel_structures_m
   use common_constants_m
   use imager_structures_m
   use surface_structures_m
   use neural_net_preproc_m
   use constants_cloud_typing_pavolonis_m
   use interpol_m
   use ecmwf_m, only : ecmwf_t
   use seviri_neural_net_preproc_m
   ! Input variables

   type(channel_info_t),        intent(in)    :: channel_info
   character(len=*),            intent(in)    :: sensor
   type(surface_t),             intent(in)    :: surface
   type(imager_flags_t),        intent(in)    :: imager_flags
   type(imager_angles_t),       intent(in)    :: imager_angles
   type(imager_geolocation_t),  intent(in)    :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_pavolonis_t),    intent(inout) :: imager_pavolonis
   type(ecmwf_t),               intent(in)    :: ecmwf
   character(len=*),            intent(in)    :: platform
   integer(kind=sint),          intent(in)    :: doy
   logical,                     intent(in)    :: do_ironly
   logical,                     intent(inout) :: do_spectral_response_correction
   logical,                     intent(inout) :: use_seviri_ann_cma_cph
   logical,                     intent(inout) :: use_seviri_ann_ctp_fg
   logical,                     intent(inout) :: use_seviri_ann_mlay
   logical,                     intent(in)    :: do_nasa
   logical,                     intent(in)    :: verbose

   ! Local variables

   integer :: i, ii, j, cview, start_line, end_line, start_pix, end_pix, npix, &
              platform_index
   real                                              :: t4_filter_thresh
   real(kind=sreal),   allocatable, dimension(:,:)   :: skint, snow_depth, sea_ice_cover
   real(kind=sreal),   allocatable, dimension(:,:,:) :: imager_data
   integer(kind=byte), allocatable, dimension(:,:)   :: snow_ice_mask
   type(interpol_t),   allocatable, dimension(:)     :: interp

   ! CC4CL requirements

   real(kind=sreal)   :: coszen
   integer            :: ch1, ch2, ch3, ch4, ch5, ch6, sw1, sw2, sw3
   integer            :: mlch1, mlch2, mlch3, mlch4, mlch5, mlch6, mlch7, &
                         mlch9, mlch10, mlch11
   integer            :: ml_channels(10)
   integer            :: legacy_channels(6)

   !--------------------------------------------------------------------
   ! -- Parameters used here
   !
   ! cirrus_quality     = quality of cirrus flag
   ! coszen             = cosine of the solar zenith angle
   ! ch3a_on_avhrr_flag = whether or not AVHRR channel 3a is
   !                      used (NO, YES, INEXISTENT)
   ! BTD                = Brightness Temperature Difference
   ! BTD_Ch3b_Ch4       = BT Ch3b minus BT Ch4
   ! BTD_Ch4_Ch5        = BT Ch4 minus BT Ch5
   ! BTD_Ch4_Ch3b       = BT Ch4 minus BT Ch3b
   !
   !
   ! -- Input
   !                                   wavelength  MODIS = CC4CL = AVHRR
   ! imager_measurements%data(i,j,1)    0.659 um  Ch01 =  1  = 1
   ! imager_measurements%data(i,j,2)    0.865 um  Ch02 =  2  = 2
   ! imager_measurements%data(i,j,3)    1.640 um  Ch06 =  3  = 3a
   ! imager_measurements%data(i,j,4)    3.750 um  Ch20 =  4  = 3b
   ! imager_measurements%data(i,j,5)   11.030 um  Ch31 =  5  = 4
   ! imager_measurements%data(i,j,6)   12.020 um  Ch32 =  6  = 5
   !
   ! imager_geolocation%latitude(i,j)
   ! imager_geolocation%longitude(i,j)
   ! imager_geolocation%dem(i,j)
   ! imager_geolocation%startx
   ! imager_geolocation%starty
   ! imager_geolocation%endx
   ! imager_geolocation%endy
   ! imager_geolocation%nx
   ! imager_geolocation%ny
   !
   ! imager_angles%satzen(i,j,imager_angles%nviews)
   ! imager_angles%solzen(i,j,imager_angles%nviews)
   !
   ! imager_flags%lusflag(i,j)
   ! imager_flags%lsflag(i,j)
   !
   ! surface%emissivity(i,j,k): currently k = 1 (Ch3b)
   ! surface%nise_mask(i,j)
   !
   !
   ! --- Output
   !
   ! imager_pavolonis%sfctype             ... surface type - NISE corrected LUS
   ! imager_pavolonis%cldtype             ... cloud type based on pavolonis
   ! imager_pavolonis%cldmask             ... cloud mask based on L1C
   !                                          thresholding
   ! imager_pavolonis%cldmask_uncertainty ... cloud mask uncertainty
   ! imager_pavolonis%cccot               ... cloud cover COTs
   !
   !--------------------------------------------------------------------

   allocate(skint(imager_geolocation%startx:imager_geolocation%endx, &
                  1:imager_geolocation%ny))
   skint = sreal_fill_value
   allocate(snow_depth(imager_geolocation%startx:imager_geolocation%endx, &
                       1:imager_geolocation%ny))
   snow_depth = sreal_fill_value
   allocate(sea_ice_cover(imager_geolocation%startx:imager_geolocation%endx, &
                          1:imager_geolocation%ny))
   sea_ice_cover = sreal_fill_value
   allocate(snow_ice_mask(imager_geolocation%startx:imager_geolocation%endx, &
                          1:imager_geolocation%ny))
   snow_ice_mask = byte_fill_value
   allocate(interp(1))

   do i = 1, imager_geolocation%ny
      do j = imager_geolocation%startx, imager_geolocation%endx
         call bilinear_coef(ecmwf%lon, ecmwf%xdim, ecmwf%lat, &
              ecmwf%ydim, imager_geolocation%longitude(j,i), &
              imager_geolocation%latitude(j,i), interp(1))

         call interp_field(ecmwf%skin_temp, skint(j,i), interp(1))
         call interp_field(ecmwf%snow_depth, snow_depth(j,i), interp(1))
         call interp_field(ecmwf%sea_ice_cover, sea_ice_cover(j,i), interp(1))

         if ((snow_depth(j,i)    .gt. 0.01 .and. &
              imager_flags%lsflag(j,i)         .eq. 1_byte) .or. &
             (snow_depth(j,i)    .gt. 0.01 .and. &
              imager_geolocation%latitude(j,i) .lt. -60.00) .or. &
             (sea_ice_cover(j,i) .gt. 0.15 .and. &
              imager_flags%lsflag(j,i)         .eq. 0_byte)) then
            snow_ice_mask(j,i) = YES
         else
            snow_ice_mask(j,i) = NO
         end if
      end do
   end do

   deallocate(interp)

   ! Copy land use flag array to Surface TYPE array
   imager_pavolonis%sfctype = imager_flags%lusflag

   ! Correction of sfctype with NISE aux. data
   where (snow_ice_mask .eq. YES)
      imager_pavolonis%sfctype = NISE_FLAG
   end where

   ! Initialize cloud mask as cloudy
!  where (imager_pavolonis%cldmask .eq. byte_fill_value)
!     imager_pavolonis%cldmask = CLOUDY
!  end where

   if (trim(adjustl(sensor)) .eq. 'AATSR' .or. &
       trim(adjustl(sensor)) .eq. 'ATSR2' .or. &
       trim(adjustl(sensor)) .eq. 'AVHRR' .or. &
       trim(adjustl(sensor)) .eq. 'MODIS' .or. &
       trim(adjustl(sensor)) .eq. 'SEVIRI' .or. &
       trim(adjustl(sensor)) .eq. 'SLSTR') &
        do_spectral_response_correction = .true.

   ! Check if all channels, required for the seviri-specific neural network, are used
   mlch1 = 0
   mlch2 = 0
   mlch3 = 0
   mlch4 = 0
   mlch5 = 0
   mlch6 = 0
   mlch7 = 0
   mlch9 = 0
   mlch10 = 0
   mlch11 = 0
   if (trim(adjustl(sensor)) .eq. 'SEVIRI' .and. use_seviri_ann_cma_cph) then
         do i = 1, channel_info%nchannels_total
            select case (channel_info%channel_ids_instr(i))
            case(1)
               mlch1 = i
            case(2)
               mlch2 = i
            case(3)
               mlch3 = i
            case(4)
               mlch4 = i
            case(5)
               mlch5 = i
            case(6)
               mlch6 = i
            case(7)
               mlch7 = i
            case(9)
               mlch9 = i
            case(10)
               mlch10 = i
            case(11)
               mlch11 = i
            end select
         end do
         ml_channels = (/mlch1, mlch2, mlch3, mlch4, mlch5, mlch6, mlch7, mlch9, mlch10, mlch11/)

         if (mlch1 == 0 .or. mlch2 == 0 .or. mlch3 == 0 .or. mlch4 == 0 &
             .or. mlch5 == 0 .or. mlch6 == 0 .or. mlch7 == 0 .or. mlch9 == 0 &
             .or. mlch10 == 0 .or. mlch11 == 0) then
            write(*,*) 'WARNING: Not using correct channels for SEVIRI-specific neural net!', &
                       ' Instead running general ANN!'
            use_seviri_ann_cma_cph = .false.
            use_seviri_ann_ctp_fg = .false.
            use_seviri_ann_mlay = .false.
         end if

      end if




   ! do not apply NOAA19 mimic when using SEVIRI neural network
   if (trim(adjustl(sensor)) .eq. 'SEVIRI' .and. use_seviri_ann_cma_cph) then
      do_spectral_response_correction = .false.
   end if

   if (do_spectral_response_correction) then
      allocate(imager_data(imager_geolocation%startx:imager_geolocation%endx, &
                           1:imager_geolocation%ny,1:channel_info%nchannels_total))
   end if

   v_loop: do cview = 1, imager_angles%nviews
      ! Determine channel indexes based on instrument channel number
      ch1 = 0
      ch2 = 0
      ch3 = 0
      ch4 = 0
      ch5 = 0
      ch6 = 0
      if (trim(adjustl(sensor)) .eq. 'AATSR' .or. &
          trim(adjustl(sensor)) .eq. 'ATSR2') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            ! If we're not looking at the second view then assume it's the first
            ! (just in case somehow cview = 3+)
            if (cview .eq. 1) then
               select case (channel_info%channel_ids_instr(i))
               case(2)
                  ch1 = i
                  sw1 = ii
               case(3)
                  ch2 = i
                  sw2 = ii
               case(4)
                  ch3 = i
                  sw3 = ii
               case(5)
                  ch4 = i
               case(6)
                  ch5 = i
               case(7)
                  ch6 = i
               end select
            else
               select case (channel_info%channel_ids_instr(i))
               case(9)
                  ch1 = i
                  sw1 = ii
               case(10)
                  ch2 = i
                  sw2 = ii
               case(11)
                  ch3 = i
                  sw3 = ii
               case(12)
                  ch4 = i
               case(13)
                  ch5 = i
               case(14)
                  ch6 = i
               end select
            end if
         end do
      else if (trim(adjustl(sensor)) .eq. 'ABI') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(2)
               ch1 = i
               sw1 = ii
            case(3)
               ch2 = i
               sw2 = ii
            case(5)
               ch3 = i
               sw3 = ii
            case(7)
               ch4 = i
            case(14)
               ch5 = i
            case(15)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'AGRI') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(2)
               ch1 = i
               sw1 = ii
            case(3)
               ch2 = i
               sw2 = ii
            case(5)
               ch3 = i
               sw3 = ii
            case(8)
               ch4 = i
            case(12)
               ch5 = i
            case(13)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'AHI') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(3)
               ch1 = i
               sw1 = ii
            case(4)
               ch2 = i
               sw2 = ii
            case(5)
               ch3 = i
               sw3 = ii
            case(7)
               ch4 = i
            case(14)
               ch5 = i
            case(15)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'AVHRR') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(1)
               ch1 = i
               sw1 = ii
            case(2)
               ch2 = i
               sw2 = ii
            case(3)
               ch3 = i
               sw3 = ii
            case(4)
               ch4 = i
            case(5)
               ch5 = i
            case(6)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'FCI') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(3)
               ch1 = i
               sw1 = ii
            case(4)
               ch2 = i
               sw2 = ii
            case(7)
               ch3 = i
               sw3 = ii
            case(9)
               ch4 = i
            case(14)
               ch5 = i
            case(15)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'MODIS') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(1)
               ch1 = i
               sw1 = ii
            case(2)
               ch2 = i
               sw2 = ii
            case(6)
               ch3 = i
               sw3 = ii
            case(20)
               ch4 = i
            case(31)
               ch5 = i
            case(32)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'SEVIRI') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(1)
               ch1 = i
               sw1 = ii
            case(2)
               ch2 = i
               sw2 = ii
            case(3)
               ch3 = i
               sw3 = ii
            case(4)
               ch4 = i
            case(9)
               ch5 = i
            case(10)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'SLSTR') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(2)
               ch1 = i
               sw1 = ii
            case(3)
               ch2 = i
               sw2 = ii
            case(5)
               ch3 = i
               sw3 = ii
            case(7)
               ch4 = i
            case(8)
               ch5 = i
            case(9)
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'VIIRSI') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(1)
               ch1 = i
               sw1 = ii
            case(2)
               ch2 = i
               sw2 = ii
            case(3)
               ch3 = i
               sw3 = ii
            case(4)
               ch4 = i
            case(5)
               ch5 = i
               ch6 = i
            end select
         end do
      else if (trim(adjustl(sensor)) .eq. 'VIIRSM') then
         do i = 1, channel_info%nchannels_total
            ii = channel_info%map_ids_channel_to_sw(i)
            select case (channel_info%channel_ids_instr(i))
            case(5)
               ch1 = i
               sw1 = ii
            case(7)
               ch2 = i
               sw2 = ii
            case(10)
               ch3 = i
               sw3 = ii
            case(12)
               ch4 = i
            case(15)
               ch5 = i
            case(16)
               ch6 = i
            end select
         end do
      end if

      if (do_ironly) then
         ch1 = 99
         ch2 = 99
         ch3 = 99
      end if

      if (ch1 == 0 .or. ch2 == 0 .or. (ch3 == 0 .and. ch4 == 0) &
          .or. ch5 == 0 .or. ch6 == 0) then
         write(*,*) 'WARNING: Pavolonis cloud typing skipped due to an ' // &
              'insufficient channel selection.'
         cycle v_loop
      end if

      legacy_channels(1) = ch1
      legacy_channels(2) = ch2
      legacy_channels(3) = ch3
      legacy_channels(4) = ch4
      legacy_channels(5) = ch5
      legacy_channels(6) = ch6

      ! Correct for differences in spectral responses between NOAA19 (= NN
      ! training data) and all other satellites with linear regression
      ! coefficients.
      !
      ! NB: "false" reflectances = reflectances not corrected by cosine of solar
      ! zenith angle. Pavolonis code converts "false" reflectances to true for
      ! ch1, ch3a, and ch3b, NN works with "false" reflectances. Thus, modify
      ! imager reflectances with linear regression coefficients that have been
      ! derived from "false" reflectances do this prior to cloud typing and NN;
      ! after that step, no further conversion has to be applied first, save
      ! imager data as it was before transformation, as only here transformed
      ! data should be used.
      if (do_spectral_response_correction) then
         imager_data = imager_measurements%data

         platform_index = get_platform_index(platform)
         n19mimic(1:3,2,:,:) = n19mimic(1:3,2,:,:) / 100.
         do i = 1, 6
            ! do not apply correction for channel3b, which is not yet available
            if (i .eq. 4) cycle

            where (imager_angles%solzen(:,:,cview) .lt. 80.)
               imager_measurements%data(:,:,legacy_channels(i)) = &
                    imager_measurements%data(:,:,legacy_channels(i)) * &
                    n19mimic(i,1,1,platform_index) + &
                    n19mimic(i,2,1,platform_index)
            else where
               imager_measurements%data(:,:,legacy_channels(i)) = &
                    imager_measurements%data(:,:,legacy_channels(i)) * &
                    n19mimic(i,1,2,platform_index) + &
                    n19mimic(i,2,2,platform_index)
            end where
         end do
      end if

      ! call SEVIRI neural network (Python) cloud detection and cloud phase
      ! determination for whole SEVIRI disc (outside loop)
      if (trim(adjustl(sensor)) .eq. 'SEVIRI' .and. use_seviri_ann_cma_cph) then
         if (verbose) write(*,*) 'Using SEVIRI-specific neural net'
         call cma_cph_seviri(cview, imager_flags, imager_angles, &
              imager_geolocation, imager_measurements, ml_channels, &
              imager_pavolonis, skint, channel_info, &
              platform, do_nasa, verbose)
      end if


      !-------------------------------------------------------------------------
      ! Begin cloud typing
      !-------------------------------------------------------------------------

      !$OMP PARALLEL &

      !$OMP PRIVATE(i) &
      !$OMP PRIVATE(j)

      !$OMP DO SCHEDULE(GUIDED)
      i_loop: do  i = imager_geolocation%startx, imager_geolocation%endx
         j_loop: do j = 1, imager_geolocation%ny
            ! Check for pixels with no valid data in the 11 micron band,
            ! skipping cloud-typing for these data (usually the case at
            ! the edge of SLSTR swaths, for instance)
            if (imager_measurements%data(i,j,ch5) .lt. 0.0) then
               imager_pavolonis%cldtype(i,j,cview) = BYTE_FILL_VALUE
               imager_pavolonis%cldmask(i,j,cview) = BYTE_FILL_VALUE
               imager_pavolonis%cldmask_uncertainty(i,j,cview) = SREAL_FILL_VALUE
               imager_pavolonis%cccot_pre(i,j,cview) = SREAL_FILL_VALUE
               imager_pavolonis%ann_phase(i,j,cview) = BYTE_FILL_VALUE
               imager_pavolonis%ann_phase_uncertainty(i,j,cview) = SREAL_FILL_VALUE
               imager_pavolonis%cphcot(i,j,cview) = SREAL_FILL_VALUE
            else
               call cloud_type_pixel(cview, i, j, ch1, ch2, ch3, ch4, ch5, ch6, &
                    sw1, sw2, sw3, imager_flags, surface, imager_angles, &
                    imager_geolocation, imager_measurements, imager_pavolonis, &
                    sensor, platform, doy, do_ironly, verbose, skint, snow_ice_mask, &
                    use_seviri_ann_cma_cph)
            end if
         end do j_loop
      end do i_loop

      !$OMP END DO
      !$OMP END PARALLEL


      !-------------------------------------------------------------------------
      ! Start spatial filter used on cirrus and overlap pixels. A 2n_box x
      ! 2n_box pixel filter is employed.
      !-------------------------------------------------------------------------

      i_loop2: do  i = imager_geolocation%startx, imager_geolocation%endx
         j_loop2: do j = 1, imager_geolocation%ny

            if (imager_pavolonis%cldtype(i,j,cview) /= CIRRUS_TYPE .and. &
                imager_pavolonis%cldtype(i,j,cview) /= OVERLAP_TYPE) cycle

            ! coszen = cosine of the solar zenith angle
            coszen = cos(imager_angles%satzen(i,j,cview) * d2r)

            ! Determine box for filtering
            start_line = max(1, j - n_box)
            end_line   = min(imager_geolocation%ny, j + n_box)
            start_pix  = max(imager_geolocation%startx, i - n_box)
            end_pix    = min(imager_geolocation%endx, i + n_box)
            npix       = ((end_line - start_line) + 1)*((end_pix - start_pix) + 1)


            ! At least one pixel in the 2n_box x 2n_box array must have a
            ! Bt_Ch4 < 295 K and the average ems 3.75 um must be < 1.2 for low
            ! quality cirrus; otherwise the pixel is reset to water or mixed.

            if (imager_pavolonis%cldtype(i,j,cview) == CIRRUS_TYPE .and. &
                imager_pavolonis%cirrus_quality(i,j,cview) == 0) then

               ! Account for atmospheric effects
               t4_filter_thresh = 295.0 - 12.0*(1.0-coszen)

               ! Filter pixels in region
               ! Added criterion for missing values in ems_Ch20 - akh 1/07.
               if ((minval(imager_measurements%data(start_pix:end_pix, &
                    start_line:end_line,ch5)) > t4_filter_thresh) .or. &
                   ((sum(imager_pavolonis%emis_ch3b(start_pix:end_pix, &
                     start_line:end_line,cview))/npix < 1.2) .and. &
                   (minval(imager_pavolonis%emis_ch3b(start_pix:end_pix, &
                    start_line:end_line,cview)) > 0.0))) then

                  if (imager_measurements%data(i,j,ch5) <= 273.16) then
                     imager_pavolonis%cldtype(i,j,cview) = SUPERCOOLED_TYPE
                  else
                     imager_pavolonis%cldtype(i,j,cview) = WATER_TYPE
                  end if
               end if
            end if

            ! At least one pixel in the 2n x 2n array must have a Bt_Ch4 < 275 K
            ! for overlap; otherwise the pixel is reset to water or mixed.

            ! Account for atmospheric effects in BT4 threshold for filtering

            if (imager_pavolonis%cldtype(i,j,cview) == OVERLAP_TYPE .and. &
                imager_angles%solzen(i,j,cview) > 90.0) then

               t4_filter_thresh = 273.0 - 12.0*(1.0-coszen)

               if (minval(imager_measurements%data(start_pix:end_pix, &
                   start_line:end_line,ch5)) > t4_filter_thresh) then

                  if (imager_measurements%data(i,j,ch5) <= 273.16) then
                     imager_pavolonis%cldtype(i,j,cview) = SUPERCOOLED_TYPE
                  else
                     imager_pavolonis%cldtype(i,j,cview) = WATER_TYPE
                  end if
               end if
            end if
         end do j_loop2
      end do i_loop2

      ! reset imager data to untransformed values
      if (do_spectral_response_correction) then
         imager_measurements%data = imager_data
      end if

      if (trim(adjustl(sensor)) .eq. 'SEVIRI' .and. use_seviri_ann_ctp_fg) then
         if (verbose) write(*,*) 'Producing SEVIRI ANN-based CTP first guess'
         call ctp_fg_seviri(cview, imager_flags, imager_angles, &
              imager_geolocation, imager_measurements, ml_channels, &
              imager_pavolonis, skint, channel_info, &
              platform, do_nasa, verbose)
      end if

      if (trim(adjustl(sensor)) .eq. 'SEVIRI' .and. use_seviri_ann_mlay) then
         if (verbose) write(*,*) 'Producing SEVIRI Multilayer flag'
         call mlay_seviri(cview, imager_flags, imager_angles, &
              imager_geolocation, imager_measurements, ml_channels, &
              imager_pavolonis, skint, channel_info, &
              platform, do_nasa, verbose)

      end if

   end do v_loop

   if (do_spectral_response_correction) then
      deallocate(imager_data)
   end if

   deallocate(skint)
   deallocate(snow_depth)
   deallocate(sea_ice_cover)

end subroutine cloud_type


subroutine cloud_type_pixel(cview, i, j, ch1, ch2, ch3, ch4, ch5, ch6, &
     sw1, sw2, sw3, imager_flags, surface, imager_angles, imager_geolocation, &
     imager_measurements, imager_pavolonis, sensor, platform, doy, do_ironly, &
     verbose, skint, snow_ice_mask, use_seviri_ann_cma_cph)

   use constants_cloud_typing_pavolonis_m
   use imager_structures_m
   use neural_net_preproc_m
   use surface_structures_m

   ! Input variables

   integer,                        intent(in)    :: cview, i, j
   integer,                        intent(in)    :: ch1, ch2, ch3, ch4, ch5, &
                                                    ch6, sw1, sw2, sw3
   type(surface_t),                intent(in)    :: surface
   type(imager_flags_t),           intent(in)    :: imager_flags
   type(imager_angles_t),          intent(in)    :: imager_angles
   type(imager_geolocation_t),     intent(in)    :: imager_geolocation
   type(imager_measurements_t),    intent(inout) :: imager_measurements
   type(imager_pavolonis_t),       intent(inout) :: imager_pavolonis
   character(len=*),             intent(in)      :: sensor
   character(len=*),             intent(in)      :: platform
   integer(kind=sint),             intent(in)    :: doy
   logical,                        intent(in)    :: do_ironly
   logical,                        intent(in)    :: verbose
   logical,                        intent(in)    :: use_seviri_ann_cma_cph
   real(kind=sreal),               intent(in)    :: &
        skint(imager_geolocation%startx:imager_geolocation%endx, &
              1:imager_geolocation%ny)
   integer(kind=byte),             intent(in)    :: &
        snow_ice_mask(imager_geolocation%startx:imager_geolocation%endx, &
                      1:imager_geolocation%ny)

   ! Local variables

   real (kind=sreal)  :: NIR_PHASE_THRES, NIR_CIRRUS_THRES, NIR_OVER_THRES,  &
                         BTD3811_PHASE_THRES, EMS38_PHASE_THRES,             &
                         BTD1112_DOVERLAP_THRES, BTD1112_CIRRUS_THRES,       &
                         BTD1112_NOVERLAP_THRES_L, BTD1112_NOVERLAP_THRES_H, &
                         EMS38_NOVERLAP_THRES_L, EMS38_NOVERLAP_THRES_H
   real               :: nir_ref
   integer            :: index1, index2, wflg
   integer(kind=sint) :: ch3a_on_avhrr_flag
   integer(kind=sint) :: ch2_on_atsr_flag, ch6_on_atsr_flag, ch7_on_atsr_flag
   logical            :: day, desertflag

   ! CC4CL requirements

   real(kind=sreal)               :: glint_angle
   real(kind=sreal)               :: rad_ch3b, rad_ch3b_emis
   real(kind=sreal)               :: ref_ch1, ref_ch3a, ref_ch3b
   real(kind=sreal)               :: bt_ch3b
   real(kind=sreal)               :: BTD_Ch3b_Ch4, BTD_Ch4_Ch5
   real(kind=sreal), dimension(2) :: plank_inv_out
   real(kind=sreal)               :: solcon_ch3b
   real(kind=sreal)               :: mu0, esd, c_sun
   real(kind=sreal)               :: solzen, ch1v, ch2v, ch3v, alb1, alb2, alb3

   ! --3x3 box stdd of BT 11µm
   integer(kind=lint) :: s_i, e_i, s_j, e_j
   real(kind=sreal)   :: NNN, MN_BT11, SD_BT11
   !-------------------------------

   ! Check if solar zenith angle is < 0
   if (imager_angles%solzen(i,j,cview) .lt. 0.) return

   if (.not. do_ironly) then
      ! Check if Ch3a is available or not (fill_value is negative)
      if (imager_measurements%data(i,j,ch3) .ge. 0 .and. &
           imager_measurements%data(i,j,ch4) .lt. 0) then
         ! Ch3a is used if Ch3b is not avail.
         ch3a_on_avhrr_flag = YES
      else if (imager_measurements%data(i,j,ch4) .ge. 0) then
         ! Ch3b is used if avail.
         ch3a_on_avhrr_flag = NO
      else
         ! Neither Ch3a nor Ch3b avail.
         ch3a_on_avhrr_flag = INEXISTENT
      end if
   else
      if (imager_measurements%data(i,j,ch4) .ge. 0) then
         ! Ch3b is used if avail.
         ch3a_on_avhrr_flag = NO
      else
         ! Neither Ch3a nor Ch3b avail.
         ch3a_on_avhrr_flag = INEXISTENT
      end if

   end if

   if (trim(adjustl(sensor)) .eq. 'AATSR' .or. &
       trim(adjustl(sensor)) .eq. 'ATSR2') then
      ! Changed min value of 3.7 for AATSR
      if (imager_measurements%data(i,j,ch3) .ge. 0 .and. &
          imager_measurements%data(i,j,ch4) .lt. 100) then
         ! Ch3a is used if Ch3b is not avail.
         ch3a_on_avhrr_flag = YES
      else if (imager_measurements%data(i,j,ch4) .ge. 100) then
         ! Ch3b is used if avail.
         ch3a_on_avhrr_flag = NO
      else
         ! Neither Ch3a nor Ch3b avail.
         ch3a_on_avhrr_flag = INEXISTENT
      end if
   end if


   ! Check if ATSR 12um channel is missing
   ch7_on_atsr_flag = YES

   if (imager_measurements%data(i,j,ch5) .ge. 100 .and. &
       imager_measurements%data(i,j,ch6) .lt. 100.) then
      ch7_on_atsr_flag = NO
   end if

   if (.not. do_ironly) then
      ch2_on_atsr_flag = YES
      if (imager_measurements%data(i,j,ch2) .lt. 0.) then
         ch2_on_atsr_flag = NO
      end if
   else
      ch2_on_atsr_flag = NO
   end if

   ! Check if ATSR 11um channel is missing because too warm
!  ch6_on_atsr_flag = YES

   if (imager_measurements%data(i,j,ch6) .ge. 220 .and. &
       imager_measurements%data(i,j,ch5) .lt. 100.) then
      ch6_on_atsr_flag = NO
   end if

   ! Check if ATSR 11um channel is missing because too cold
   ch6_on_atsr_flag = YES

   if (imager_measurements%data(i,j,ch6) .lt. 100 .and. &
       imager_measurements%data(i,j,ch5) .lt. 100.) then
      ch6_on_atsr_flag = NO
      ch7_on_atsr_flag = NO
   end if


   ! Check for sunglint and save result:

   ! In PATMOS sunglint calculation:
   if (imager_angles%solzen(i,j,cview) /= sreal_fill_value .and. &
       imager_angles%satzen(i,j,cview) /= sreal_fill_value .and. &
       imager_angles%RELAZI(i,j,cview) /= sreal_fill_value) then

      glint_angle = cos(imager_angles%solzen(i,j,cview) * d2r) * &
                    cos(imager_angles%satzen(i,j,cview) * d2r) + &
                    sin(imager_angles%solzen(i,j,cview) * d2r) * &
                    sin(imager_angles%satzen(i,j,cview) * d2r) * &
                    cos(imager_angles%RELAZI(i,j,cview) * d2r)

      glint_angle = max(-1.0, min(glint_angle, 1.0))
      glint_angle = acos(glint_angle) / d2r
   else
      glint_angle = sreal_fill_value
   end if

   ! Calculate BT differences

   ! BT(11) minus BT(12)
   BTD_Ch4_Ch5 = imager_measurements%data(i,j,ch5) - &
                 imager_measurements%data(i,j,ch6)

   ! BT(3.75) minus BT(11)
   BTD_Ch3b_Ch4 = imager_measurements%data(i,j,ch4) - &
                  imager_measurements%data(i,j,ch5)

   ! Calculate spatial stddev of BT(11)
   s_i = max(i-1, imager_geolocation%startx)
   e_i = min(i+1, imager_geolocation%endx)
   s_j = max(j-1, 1)
   e_j = min(j+1, imager_geolocation%ny)
   NNN = ( e_i - s_i +1) * ( e_j- s_j +1)

   MN_BT11 = SUM(imager_measurements%DATA(s_i:e_i,s_j:e_j,ch5)  ) / NNN
   SD_BT11 = SQRT (SUM((imager_measurements%DATA(s_i:e_i,s_j:e_j,ch5) - MN_BT11)**2) / (NNN - 1))

   ! Calculate ch3b radiance and emissivity
   plank_inv_out  = plank_inv(platform, imager_measurements%data(i,j,ch4))
   rad_ch3b       = plank_inv_out(1)
   solcon_ch3b    = plank_inv_out(2)
   plank_inv_out  = plank_inv(platform, imager_measurements%data(i,j,ch5))
   rad_ch3b_emis  = plank_inv_out(1)
   mu0            = cos (imager_angles%solzen(i,j,cview) * d2r)
   esd            = 1.0 - 0.0167 * cos(2.0 * pi * (doy - 3) / 365.0)
   c_sun          = 1. / esd**2
   imager_pavolonis%emis_ch3b(i,j,cview) = rad_ch3b / rad_ch3b_emis

   ! Calculate true reflectances for avhrr, modis and aatsr
   if (.not. do_ironly) then
      ref_ch1 = imager_measurements%data(i,j,ch1) / mu0
   else
      ! set ref_ch1 to fillvalue is enough to force pavolonis to use
      ! Ir only at daytime
      ref_ch1 = sreal_fill_value
   end if

   if (ch3a_on_avhrr_flag .eq. YES) then
      ref_ch3a = imager_measurements%data(i,j,ch3) / mu0
   else
      ref_ch3a = sreal_fill_value
   end if
   ref_ch3b = (rad_ch3b - rad_ch3b_emis) / &
              (solcon_ch3b * c_sun * mu0 - rad_ch3b_emis)

   ! Make sure reflectances are positive, else fill value
   if (ref_ch1 .lt. 0.) ref_ch1 = sreal_fill_value
   if (ref_ch3a .lt. 0.) ref_ch3a = sreal_fill_value
   ! dont do this. Neg values are realistic and the ANN can deal with it
!  if (ref_ch3b .lt. 0.) ref_ch3b = sreal_fill_value

   ! nir_ref = channel 3a or channel 3b reflectance
   nir_ref = sreal_fill_value

   solzen  = imager_angles%solzen(i,j,cview)

   !-- Determine the viewing zenith angle bin.
   index1 = min(7, max(1, int(imager_angles%SATZEN(i,j,cview)/10.0) + 1))
   !-- Determine the solar zenith angle bin.
   index2 = min(8, max(1, int(imager_angles%SOLZEN(i,j,cview)/10.0) + 1))


   ! If we're not using any VIS channels then fill ch1 and ch2
   ! Fudge SZA to 120 deg: forces neural net into night mode
   if (do_ironly) then
      ch1v = sreal_fill_value
      ch2v = sreal_fill_value
      ch3v = sreal_fill_value
      alb1 = sreal_fill_value
      alb2 = sreal_fill_value
      alb3 = sreal_fill_value
      ! During daytime we need to set bt37 to fillvalue as well, the nighttime
      ! ANN expects 3.7 without solar component.
      if (solzen .le. 80.) bt_ch3b = sreal_fill_value
      solzen = 120.
      index2 = 8
      ! Otherwise, behave as-before
   else
      ch1v    = imager_measurements%data(i,j,ch1)
      ch2v    = imager_measurements%data(i,j,ch2)
      ch3v    = imager_measurements%data(i,j,ch3)
      bt_ch3b = imager_measurements%data(i,j,ch4)
      alb1    = surface%albedo(i,j,sw1)
      alb2    = surface%albedo(i,j,sw2)
      alb3    = surface%albedo(i,j,sw3)
   end if

   !-- Set 11um - 12um cirrus thresholds.
   !   Absorption of radiation by water vapor and ice crystals in
   !   semitransparent cirrus clouds is greater at 12 than 11 micron

   BTD1112_CIRRUS_THRES = &
        A1(index1) + &
        B1(index1)*imager_measurements%DATA(i,j,ch5) + &
        C1(index1)*imager_measurements%DATA(i,j,ch5)**2 + &
        D1(index1)*imager_measurements%DATA(i,j,ch5)**3 + &
        E1(index1)*imager_measurements%DATA(i,j,ch5)**4

   BTD1112_CIRRUS_THRES = max( 1.0, min(4.0, BTD1112_CIRRUS_THRES) )

   desertflag = .false.
   if (imager_pavolonis%sfctype(i,j) == DESERT_FLAG) desertflag = .true.

   if (.not. use_seviri_ann_cma_cph) then
      ! NEURAL_NET_PREPROC subroutine
      ! If one of the (future) ANN's expects true reflectances this will be handled
      ! internally in ann_cloud_mask.
      call ann_cloud_mask(&
           ch1v, & ! Not "True" reflectances expected
           ch2v, & ! Not "True" reflectances expected
           ch3v, &
           bt_ch3b, &
           ref_ch3b*mu0, & ! Not "True" reflectances expected
           imager_measurements%data(i,j,ch5), &
           imager_measurements%data(i,j,ch6), &
           solzen, &
           imager_angles%satzen(i,j,cview), &
           snow_ice_mask(i,j), &
           imager_flags%lsflag(i,j), &
           desertflag, &
           alb1, &
           alb2, &
           alb3, &
           imager_pavolonis%cccot_pre(i,j,cview), &
           imager_pavolonis%cldmask(i,j,cview) , &
           imager_pavolonis%cldmask_uncertainty(i,j,cview) , &
           skint(i,j) , &
           ch3a_on_avhrr_flag, &
           glint_angle, &
           sensor, &
           platform, &
           verbose)
   end if

   ! Return if clear, as no need to define cloud type
   if ( imager_pavolonis%cldmask(i,j,cview) == CLEAR) then
      if ( ( BTD_Ch4_Ch5 > (BTD1112_CIRRUS_THRES-0.2)  ) .and. &
           ( imager_measurements%DATA(i,j,ch5) < 295.0 ) .and. &
           ( snow_ice_mask(i,j) .eq. NO ) ) then
         imager_pavolonis%CLDMASK(i,j,cview) = CLOUDY
         imager_pavolonis%CLDMASK_UNCERTAINTY(i,j,cview) = 99 !100. - imager_pavolonis%CLDMASK_UNCERTAINTY(i,j,cview)
!          imager_pavolonis%CLDTYPE(i,j,cview) = CIRRUS_TYPE
!          imager_pavolonis%cirrus_quality(i,j,cview) = 1
!          imager_pavolonis%ANN_PHASE(i,j,cview) = ICE
!          imager_pavolonis%ANN_PHASE_UNCERTAINTY(i,j,cview) = 99
!          cycle
      else
         imager_pavolonis%cldtype(i,j,cview) = CLEAR_TYPE
         imager_pavolonis%ANN_PHASE(i,j,cview) = CLEAR_TYPE
         if (trim(adjustl(sensor)) /= 'AATSR' .and. &
             trim(adjustl(sensor)) /= 'ATSR2') return
      end if
   end if

   ! Implement extra tests for AATSR

   if (trim(adjustl(sensor)) .eq. 'AATSR' .or. &
       trim(adjustl(sensor)) .eq. 'ATSR2') then
      ! 11um channel can occasionally be missing particuarly for AATSR if it
      ! gets too warm. Also, when ch6 atsr is fill value, clear type is assigned.
      if ((ch6_on_atsr_flag == NO) .and. (ch7_on_atsr_flag == YES)) then

         imager_pavolonis%cldmask(i,j,cview) = CLEAR
         imager_pavolonis%cldtype(i,j,cview) = CLEAR_TYPE

      end if

      ! 12um channel can occasionally be missing particuarly for AATSR.
      ! Also, when ch7 ATSR is fill value, PROB_OPAQUE_ICE_TYPE is assigned
      if ((ch7_on_atsr_flag == NO) .and. (ch2_on_atsr_flag == YES)) then

         imager_pavolonis%cldmask(i,j,cview) = CLOUDY
         imager_pavolonis%cldtype(i,j,cview) = PROB_OPAQUE_ICE_TYPE

      end if

      ! If both IR channels are missing them likely a very cold opaque cloud.
      ! Enable a CTH retrieval by setting minimum threshold values of IR
      ! channels.
      if  ((ch6_on_atsr_flag == NO) .and. (ch7_on_atsr_flag == NO) .and. &
           (ch2_on_atsr_flag == yes)) then

         imager_pavolonis%cldmask(i,j,cview) = CLOUDY
!        imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE
         imager_pavolonis%cldtype(i,j,cview) = PROB_OPAQUE_ICE_TYPE
         imager_measurements%data(i,j,ch6)   = 210.0
         imager_measurements%data(i,j,ch5)   = 209.0

      end if

      if ((ch7_on_atsr_flag == NO)) then
         ! Check if over very cold atlantic plateau Antarctica in which case
         ! probably clear if 12um is missing.
         if ((imager_pavolonis%sfctype(i,j) == NISE_FLAG) .and. &
             (imager_geolocation%latitude(i,j) < -70.0)) then

            imager_pavolonis%cldmask(i,j,cview) = CLEAR
            imager_pavolonis%cldtype(i,j,cview) = PROB_CLEAR_TYPE

         end if
      end if

      if (imager_pavolonis%cldmask(i,j,cview) == CLEAR) then

         imager_pavolonis%cldtype(i,j,cview) = CLEAR_TYPE

         return
      end if
   end if

   ! introduce 2nd glint test / also possible dust using spatial
   ! variabilty in channel5
   if ( ( BTD_Ch4_Ch5 .ge. 0. ) .and. ( BTD_Ch4_Ch5 .le. 3. ) .and. &
        ( ch2v/max(ch1v, 0.01) .ge. 0.6 ) .and. (ch2v/max(ch1v, 0.01) .le. 1. ) .and. &
        ( imager_measurements%DATA(i,j,ch5) .gt. 290.0 ) .and. &
        ( SD_BT11 .lt. 0.25 ) ) then
      imager_pavolonis%CLDMASK(i,j,cview) = CLEAR
      imager_pavolonis%CLDMASK_UNCERTAINTY(i,j,cview) = 99 !100. - imager_pavolonis%CLDMASK_UNCERTAINTY(i,j,cview)
      imager_pavolonis%CLDTYPE(i,j,cview) = CLEAR_TYPE
      return
   end if

   ! From here all sensors have their decisions made only when cloudy is allowed

   ! Start ANN phase
   if (.not. use_seviri_ann_cma_cph) then
      ! NEURAL_NET_PREPROC subroutine
      ! If one of the (future) ANN's expects true reflectances this will be handled
      ! internally in ann_cloud_phase.
      call ann_cloud_phase(&
           ch1v, & ! Not "True" reflectances expected
           ch2v, & ! Not "True" reflectances expected
           ch3v, &
           bt_ch3b, &
           ref_ch3b*mu0, & ! Not "True" reflectances expected
           imager_measurements%data(i,j,ch5), &
           imager_measurements%data(i,j,ch6), &
           solzen, &
           imager_angles%satzen(i,j,cview), &
           snow_ice_mask(i,j), imager_flags%lsflag(i,j), &
           desertflag , &
           alb1, &
           alb2, &
           alb3, &
           imager_pavolonis%CPHCOT(i,j,cview), &
           imager_pavolonis%ANN_PHASE(i,j,cview) , &
           imager_pavolonis%ANN_PHASE_UNCERTAINTY(i,j,cview) , &
           ch3a_on_avhrr_flag, &
           sensor, &
           platform, &
           skint(i,j) , &
           verbose)
   end if

   ! neither ch3a nor ch3b available
   if (trim(adjustl(sensor)) .eq. 'AVHRR') then
      ! At night, assign probably opaque ice flag as ch3.7 has fill value due to
      ! low S/N.

      ! Only apply to avhrr data as when fill for aatsr it is actaully warm
      if (ch3a_on_avhrr_flag == INEXISTENT) then
!        if ((imager_geolocation%latitude(i,j) < 65.0 .and. &
!             imager_geolocation%latitude(i,j) > -65.0) .and. &
!             (.not. day)) &
!           imager_pavolonis%cldtype(i,j) = PROB_OPAQUE_ICE_TYPE

         if ((imager_measurements%data(i,j,ch5) > 0.) .and. &
              (imager_measurements%data(i,j,ch5) <= 233.16)) then
            imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE
         else if ((imager_measurements%data(i,j,ch5) > 233.16) .and. &
              (imager_measurements%data(i,j,ch5) <= 253.16)) then
            imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE
         else if ((imager_measurements%data(i,j,ch5) > 253.16) .and. &
              (imager_measurements%data(i,j,ch5) <= 273.16)) then
            imager_pavolonis%cldtype(i,j,cview) =SUPERCOOLED_TYPE
         else
            imager_pavolonis%cldtype(i,j,cview) = WATER_TYPE
         end if

         return
      end if
   end if

   ! Initial cirrus quality
   imager_pavolonis%cirrus_quality(i,j,cview) = 0

   ! Check if daytime or nighttime algorithm is to be used.
   day = .false.
   if ((imager_angles%solzen(i,j,cview) < 88.0) .or. (ch3a_on_avhrr_flag .eq. YES)) then
      day = .true.
   end if


   !----------------------------------------------------------------------------
   ! If DAYTIME, use daytime algorithm
   !----------------------------------------------------------------------------

   if ((imager_pavolonis%cldmask(i,j,cview) == CLOUDY) .and. day) then

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Set 11um - 12um overlap thresholds
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!     if ((imager_measurements%data(i,j,ch1) >= 35.0) .and. &
!         (imager_measurements%data(i,j,ch1) <= 60.0)) then
      if ((ref_ch1 >= 0.35) .and. (ref_ch1 <= 0.60)) then

         BTD1112_DOVERLAP_THRES = max(&
              ((A3(index1,index2)+ &
!               B3(index1,index2)* imager_measurements%data(i,j,ch1)*0.01 + &
!               C3(index1,index2)*(imager_measurements%data(i,j,ch1)*0.01)**2 + &
!               D3(index1,index2)*(imager_measurements%data(i,j,ch1)*0.01)**3 + &
!               E3(index1,index2)*(imager_measurements%data(i,j,ch1)*0.01)**4) - 0.1), &
                B3(index1,index2)* ref_ch1 + &
                C3(index1,index2)*(ref_ch1)**2 + &
                D3(index1,index2)*(ref_ch1)**3 + &
                E3(index1,index2)*(ref_ch1)**4) - 0.1), &
                MIN_BTD1112_DOVERLAP(index1,index2) - 0.1)

!     else if (imager_measurements%data(i,j,ch1) > 60.0 .and. &
!              imager_measurements%data(i,j,ch1) < 90.0) then
      else if (ref_ch1 > 0.60 .and. ref_ch1 < 0.90) then

         BTD1112_DOVERLAP_THRES = MIN_BTD1112_DOVERLAP(index1,index2) - 0.1
      else

         BTD1112_DOVERLAP_THRES = 9999.0

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! In the high latitudes, the Ch3b 3.75 um must be less than 20% to prevent
      ! single layer water clouds from being typed as overlap.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((ch3a_on_avhrr_flag == NO) .and. &
          ((imager_geolocation%latitude(i,j) > 65.0)   .or.  &
           (imager_geolocation%latitude(i,j) < -65.0)) .and. &
          (ref_ch3b > 0.20)) then

         BTD1112_DOVERLAP_THRES = 9999.0

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Perform initial IR window brightness temperature-based typing
      ! wflg = IR window flag
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      wflg = 1

      if (imager_measurements%data(i,j,ch5) <= 233.16) then

         imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE
         wflg = 0

      else if ((imager_measurements%data(i,j,ch5) > 233.16) .and. &
               (imager_measurements%data(i,j,ch5) <= 253.16)) then

         imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE

      else if ((imager_measurements%data(i,j,ch5) > 253.16) .and. &
               (imager_measurements%data(i,j,ch5) <= 273.16)) then

         imager_pavolonis%cldtype(i,j,cview) = SUPERCOOLED_TYPE

      else
         ! BT(11) > 273.16 K, the melting point of pure water, the cirrus
         ! detection test is simply applied. If it is passed, then the pixel is
         ! classified as nonopaque ice cloud, otherwise, it is a warm liquid
         ! water cld type. [Pavolonis et al. (2005)]
         imager_pavolonis%cldtype(i,j,cview) = WATER_TYPE

      end if


      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Use 1.6 um algorithm, if available
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((ch3a_on_avhrr_flag == YES) .and. (ref_ch1 > 0.0)) then

         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! Set some 1.6 um thresholds used in phase identification
         ! and cirrus detection.
         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         if ((imager_pavolonis%sfctype(i,j) == WATER_FLAG) .or. &
             (imager_pavolonis%sfctype(i,j) == NISE_FLAG)) then

!           NIR_CIRRUS_THRES = 20.0
!           NIR_PHASE_THRES  = 17.0
            NIR_CIRRUS_THRES = 0.20
            NIR_PHASE_THRES  = 0.17

         else if (imager_pavolonis%sfctype(i,j) == DESERT_FLAG) then

!           NIR_CIRRUS_THRES = 55.0
!           NIR_PHASE_THRES  = 32.0
            NIR_CIRRUS_THRES = 0.55
            NIR_PHASE_THRES  = 0.32

            ! All other surface types
         else

!           NIR_CIRRUS_THRES = 33.0
!           NIR_PHASE_THRES  = 32.0
            NIR_CIRRUS_THRES = 0.33
            NIR_PHASE_THRES  = 0.32

         end if

         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! Set the minimum 1.6 um reflectance allowed for cloud overlap over
         ! SNOW/ICE.
         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         if (imager_pavolonis%sfctype(i,j) == NISE_FLAG) then

!           NIR_OVER_THRES = 17.0
            NIR_OVER_THRES = 0.17

         else

            NIR_OVER_THRES = 0.0

         end if

         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! The reflectance used in the typing tests to 1.65 um  when Ch3a is on.
         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         nir_ref = ref_ch3a

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Use 3.75 um channel (used only if 1.6um is not available)
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      else if ((ch3a_on_avhrr_flag == NO) .and. (ref_ch1 > 0.0)) then

         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! Set some 3.75 um thresholds used in phase identification and cirrus
         ! detection.
         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         if ((imager_pavolonis%sfctype(i,j) == WATER_FLAG) .or. &
             (imager_pavolonis%sfctype(i,j) == NISE_FLAG)) then

!           NIR_CIRRUS_THRES = 12.0
!           NIR_PHASE_THRES  = 6.0
            NIR_CIRRUS_THRES = 0.12
            NIR_PHASE_THRES  = 0.06

         else if (imager_pavolonis%sfctype(i,j) == DESERT_FLAG) then

!           NIR_CIRRUS_THRES = 40.0
!           NIR_PHASE_THRES  = 6.0
            NIR_CIRRUS_THRES = 0.40
            NIR_PHASE_THRES  = 0.06

            ! all other surface types
         else

!           NIR_CIRRUS_THRES = 12.0
!           NIR_PHASE_THRES  = 6.0
            NIR_CIRRUS_THRES = 0.12
            NIR_PHASE_THRES  = 0.06

         end if

         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! Set the minimum 3.75 um reflectance allowed for cloud overlap over
         ! SNOW/ICE.
         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         if (imager_pavolonis%sfctype(i,j) == NISE_FLAG) then

!           NIR_OVER_THRES = 6.0
            NIR_OVER_THRES = 0.06

         else

            NIR_OVER_THRES = 0.0

         end if

         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ! The reflectance used in the typing tests to 3.75 um when Ch3b is on.
         !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         nir_ref = ref_ch3b

      else

         ! In case it is daytime, but ref_ch1 <= 0, skip further Pavolonis
         ! tests.  Algorithm output is simple IR window brightness temperature-
         ! based typing as applied previously.
         return

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Perform the NIR reflectance bulk cloud phase test
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((imager_pavolonis%cldtype(i,j,cview) == SUPERCOOLED_TYPE) .and. &
           (nir_ref <= NIR_PHASE_THRES) .and. &
           (imager_measurements%data(i,j,ch5) < 263.16)) then

         imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE

      end if

      if ((imager_pavolonis%cldtype(i,j,cview) == OPAQUE_ICE_TYPE) .and. &
           (wflg == 1) .and. &
           (nir_ref > NIR_PHASE_THRES)) then

         imager_pavolonis%cldtype(i,j,cview) = SUPERCOOLED_TYPE

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Perform the cloud overlap test
      ! !! not used over DESERT surfaces !!
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((BTD_Ch4_Ch5 > BTD1112_DOVERLAP_THRES) .and. &
           (imager_measurements%data(i,j,ch5) < 270.0) .and. &
           (imager_pavolonis%sfctype(i,j) /= DESERT_FLAG) .and. &
           (nir_ref > NIR_OVER_THRES) .and. &
           (imager_measurements%data(i,j,ch5) > 210.0)) then

         imager_pavolonis%cldtype(i,j,cview) = OVERLAP_TYPE

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Look for cirrus clouds. Note, akh modified so that nir_ref test only
      ! applied when Solzen < 70.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((imager_pavolonis%cldtype(i,j,cview) /= OVERLAP_TYPE) .and. &
           (BTD_Ch4_Ch5 > (BTD1112_CIRRUS_THRES-0.2)) .and. &
           (imager_measurements%data(i,j,ch5) < 295.0)) then

         if (imager_angles%solzen(i,j,cview) < 70.0) then
            if (nir_ref < NIR_CIRRUS_THRES) then

               imager_pavolonis%cldtype(i,j,cview) = CIRRUS_TYPE
               imager_pavolonis%cirrus_quality(i,j,cview) = 1

            end if
         else
            imager_pavolonis%cldtype(i,j,cview) = CIRRUS_TYPE

            imager_pavolonis%cirrus_quality(i,j,cview) = 0
            ! note, this is a low quality

         end if
      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Look for fog - not used over DESERT(=12) surfaces.
      !
      ! The ref3b/ref1 threshold is used to prevent near terminator and sunglint
      ! pixels from being classified as fog.

      ! THERE IS CURRENTLY NO FOG DETECTION WHEN CH3A IS ON!
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((ch3a_on_avhrr_flag == NO) .and. &
          (ref_ch3b >= 0.25) .and. &
           (imager_pavolonis%sfctype(i,j) /= DESERT_FLAG) .and. &
           (imager_measurements%data(i,j,ch5) > 240.0) .and. &
           ((ref_ch3b / ref_ch1) < 0.6)) then

         imager_pavolonis%cldtype(i,j,cview) = FOG_TYPE

      end if


   !----------------------------------------------------------------------------
   ! If nighttime, use tri-spectral nighttime algorithm.
   !----------------------------------------------------------------------------

   else if ((imager_pavolonis%cldmask(i,j,cview) == CLOUDY) .and. &
            (.not. day)) then

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Set 3.75um - 11um thresholds used for phase determination.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      BTD3811_PHASE_THRES = A2(index1) + &
           B2(index1)*BTD_Ch4_Ch5    + &
           C2(index1)*BTD_Ch4_Ch5**2 + &
           D2(index1)*BTD_Ch4_Ch5**3 + &
           E2(index1)*BTD_Ch4_Ch5**4

      BTD3811_PHASE_THRES = min(8.0, max(-2.0, BTD3811_PHASE_THRES))

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Set the 3.75 um thresholds used for phase determination.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if (imager_measurements%data(i,j,ch5) <= 245.0) then

         if (imager_pavolonis%sfctype(i,j) == WATER_FLAG) then
            EMS38_PHASE_THRES = 0.9
         else
            EMS38_PHASE_THRES = 0.9
         end if

      else ! BT11 > 245 (Ch4 BT: 11 micron)

         if (imager_pavolonis%sfctype(i,j) == WATER_FLAG) then
            EMS38_PHASE_THRES = 1.12
         else
            EMS38_PHASE_THRES = 1.12
         end if

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Set the split window and EMS3b thresholds used in nighttime cloud
      ! overlap detection.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if (BTD_Ch3b_Ch4 > 0.0) then

         if (imager_pavolonis%sfctype(i,j) == WATER_FLAG) then

            if ((imager_geolocation%latitude(i,j) > -30.0) .and. &
                (imager_geolocation%latitude(i,j) < 30.0 )) then

               BTD1112_NOVERLAP_THRES_H = 2.5
               BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)+0.2
               EMS38_NOVERLAP_THRES_H = 5.0
               EMS38_NOVERLAP_THRES_L = 1.1

            else

               BTD1112_NOVERLAP_THRES_H = 2.0
               BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)
               EMS38_NOVERLAP_THRES_H = 2.5
               EMS38_NOVERLAP_THRES_L = 1.05

            end if
            ! All other surface types
         else

            if ((imager_geolocation%latitude(i,j) > -30.0) .and. &
                (imager_geolocation%latitude(i,j) < 30.0)) then

               BTD1112_NOVERLAP_THRES_H = 2.5
               BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)+0.2
               EMS38_NOVERLAP_THRES_H = 5.0
               EMS38_NOVERLAP_THRES_L = 1.1

            else

               BTD1112_NOVERLAP_THRES_H = 2.0
               BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)
               EMS38_NOVERLAP_THRES_H = 2.0
               EMS38_NOVERLAP_THRES_L = 1.0

            end if

         end if

         ! These thresholds are not applied if Ch3b minus Ch4
         ! (BTD_Ch3b_Ch4) <= 0
      else

         BTD1112_NOVERLAP_THRES_H = -99.0
         BTD1112_NOVERLAP_THRES_L = 999.0
         EMS38_NOVERLAP_THRES_H = -99.0
         EMS38_NOVERLAP_THRES_L = 999.0

      end if


      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! These thresholds are not applied if the surface type is DESERT.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if (imager_pavolonis%sfctype(i,j) == DESERT_FLAG) then

         BTD1112_NOVERLAP_THRES_H = -99.0
         BTD1112_NOVERLAP_THRES_L = 999.0
         EMS38_NOVERLAP_THRES_H = -99.0
         EMS38_NOVERLAP_THRES_L = 999.0

      end if


      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Perform initial IR window brightness temperature-based typing.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      wflg = 1

      if (imager_measurements%data(i,j,ch5) <= 233.16) then

         imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE
         wflg = 0

      else if ((imager_measurements%data(i,j,ch5) > 233.16) .and. &
               (imager_measurements%data(i,j,ch5) <= 253.16)) then

         imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE

      else if ((imager_measurements%data(i,j,ch5) > 253.16) .and. &
               (imager_measurements%data(i,j,ch5) <= 273.16)) then

         imager_pavolonis%cldtype(i,j,cview) = SUPERCOOLED_TYPE

      else

         imager_pavolonis%cldtype(i,j,cview) = WATER_TYPE

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Perform the EMS 3.75 um test for bulk cloud phase determination.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ! solar contamination check disabled
!     if (SOLAR_CONTAMINATION_MASK(i,j) == NO) then

      if ((imager_pavolonis%cldtype(i,j,cview) == SUPERCOOLED_TYPE) .and. &
           (imager_pavolonis%emis_ch3b(i,j,cview) >= EMS38_PHASE_THRES) .and. &
           (imager_measurements%data(i,j,ch5) < 263.16)) then

         imager_pavolonis%cldtype(i,j,cview) = OPAQUE_ICE_TYPE

      end if


      if ((imager_pavolonis%cldtype(i,j,cview) == OPAQUE_ICE_TYPE) .and. &
           (wflg == 1) .and. &
           (imager_pavolonis%emis_ch3b(i,j,cview) < EMS38_PHASE_THRES)) then

         imager_pavolonis%cldtype(i,j,cview) = SUPERCOOLED_TYPE

      end if

!     end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Nighttime cloud overlap test
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((BTD_Ch4_Ch5 > BTD1112_NOVERLAP_THRES_L) .and. &
           (BTD_Ch4_Ch5 < BTD1112_NOVERLAP_THRES_H) .and. &
           (imager_pavolonis%emis_ch3b(i,j,cview) > EMS38_NOVERLAP_THRES_L) .and. &
           (imager_pavolonis%emis_ch3b(i,j,cview) < EMS38_NOVERLAP_THRES_H) .and. &
           (imager_measurements%data(i,j,ch5) > 210.0) .and. &
           (imager_measurements%data(i,j,ch5) < 283.0)) then

         imager_pavolonis%cldtype(i,j,cview) = OVERLAP_TYPE

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Look for cirrus clouds using the split window test.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((imager_pavolonis%cldtype(i,j,cview) /= OVERLAP_TYPE) .and. &
          (BTD_Ch4_Ch5 > BTD1112_CIRRUS_THRES) .and. &
          (imager_pavolonis%emis_ch3b(i,j,cview) > 1.3)) then

         imager_pavolonis%cldtype(i,j,cview) = CIRRUS_TYPE

      end if

      if ((imager_pavolonis%emis_ch3b(i,j,cview) > 1.6) .and. &
          (imager_measurements%data(i,j,ch5) < 300.0)) then

         imager_pavolonis%cirrus_quality(i,j,cview) = 1

      end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Look for cirrus clouds using the EMS 3.75 um test.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ! solar contamination check disabled
!     if (SOLAR_CONTAMINATION_MASK(i,j) == NO) then

      if ((imager_pavolonis%cldtype(i,j,cview) /= OVERLAP_TYPE) .and. &
           (imager_pavolonis%cldtype(i,j,cview) /= OPAQUE_ICE_TYPE) .and. &
           (imager_pavolonis%emis_ch3b(i,j,cview) > 1.10) .and. &
           (imager_measurements%data(i,j,ch5) < 300.0)) then

         imager_pavolonis%cldtype(i,j,cview) = CIRRUS_TYPE

      end if

      if (((imager_pavolonis%emis_ch3b(i,j,cview) > 1.6)       .and. &
           (imager_measurements%data(i,j,ch5) < 300.0)) .or. &
           ((imager_pavolonis%emis_ch3b(i,j,cview) > 1.4)       .and. &
           (imager_measurements%data(i,j,ch5) < 300.0)   .and. &
           (BTD_Ch4_Ch5 > BTD1112_CIRRUS_THRES))) then

         imager_pavolonis%cirrus_quality(i,j,cview) = 1

      end if

!     end if

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Look for fog - not used over DESERT(=12) surfaces.
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if ((imager_angles%solzen(i,j,cview) >= 90.0) .and. &
           (imager_pavolonis%emis_ch3b(i,j,cview) <= 0.90) .and. &
           (imager_measurements%data(i,j,ch5) > 240.0) .and. &
           (imager_pavolonis%sfctype(i,j) /= DESERT_FLAG)) then

         imager_pavolonis%cldtype(i,j,cview) = FOG_TYPE

      end if

   end if

   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! set cccot to a fake high value as otherwise gets set as clear later on.
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   if  (imager_pavolonis%cldtype(i,j,cview) == PROB_OPAQUE_ICE_TYPE) then
      imager_pavolonis%cldmask(i,j,cview) = CLOUDY
      imager_pavolonis%cccot_pre(i,j,cview) = .99

   end if

end subroutine cloud_type_pixel


! NOT ADAPTED YET for CC4CL!
!
! =====================================================================
!      Beginning of subroutine CLOUD_RETYPE
!      This routine modifies the cloud_type array based on spatial
!      analysis.  Its goal is to reduce the edges of stratus clouds
!      from being typed as cirrus
! =====================================================================

!   subroutine CLOUD_RETYPE(jmin, numj, cld_type_array)

!     use COMMON_CONSTANTS
!     use IMAGER_STRUCTURES
!     use CONSTANTS_CLOUD_TYPING_PAVOLONIS

!     integer (kind=sint), intent(inout), dimension(:,:) :: cld_type_array
!     integer, intent(in):: jmin
!     integer, intent(in):: numj
!     integer:: ilrc
!     integer:: jlrc
!     integer:: i
!     integer:: j

!     type(imager_geolocation_t) :: imager_geolocation


!     !-------------------------------------------------------------------
!     ! ---- loop over pixels
!     i_loop: do i = 1, num_pix
!     i_loop: do i = imager_geolocation%startx, imager_geolocation%endx


!        !-----------------------------------------------------------------
!        ! ---- loop over scanlines
!        j_loop: do j = jmin, numj + jmin - 1
!        j_loop: do j = imager_geolocation%starty, imager_geolocation%endy


!           ilrc = i_lrc(i,j)
!           jlrc = j_lrc(i,j)


!           ! check for ice-phase pixels where the local radiative center
!           ! is a water cloud  - retype these as water

!           if ((cld_type_array(i,j) == CIRRUS_TYPE)  .or. &
!               (cld_type_array(i,j) == OVERLAP_TYPE)) then

!              !ilrc = i_min_Bt_Ch31_3x3(i,j)
!              !jlrc = j_min_Bt_Ch31_3x3(i,j)
!              ilrc = i_lrc(i,j)
!              jlrc = j_lrc(i,j)

!              ! skip this if no lrc is available
!              if (ilrc < 1 .or. jlrc < 1) then
!                 cycle
!              end if

!              if ((cld_type_array(ilrc,jlrc) == FOG_TYPE) .or. &
!                  (cld_type_array(ilrc,jlrc) == WATER_TYPE) .or. &
!                  (cld_type_array(ilrc,jlrc) == SUPERCOOLED_TYPE)) then

!                 cld_type_array(i,j) = cld_type_array(ilrc,jlrc)

!              end if

!           end if



!           ! check for water clouds on the edge of cirrus

!           if ((cld_type_array(i,j) == FOG_TYPE)  .or. &
!               (cld_type_array(i,j) == WATER_TYPE)) then

!              ilrc = i_lrc(i,j)
!              jlrc = j_lrc(i,j)

!              if (ilrc < 1 .or. jlrc < 1) then
!                 cycle
!              end if

!              if ((cld_type_array(ilrc,jlrc) == CIRRUS_TYPE) .or. &
!                  (cld_type_array(ilrc,jlrc) == OVERLAP_TYPE) .or. &
!                  (cld_type_array(ilrc,jlrc) == OPAQUE_ICE_TYPE)) then

!                 cld_type_array(i,j) = CIRRUS_TYPE

!              end if

!           end if


!        end do j_loop
!        !-----------------------------------------------------------------


!     end do i_loop
!     !-------------------------------------------------------------------

! =====================================================================
!   end subroutine CLOUD_RETYPE
! =====================================================================


function plank_inv(input_platform, T)

   use common_constants_m

   implicit none

   ! input variable
   character(len=*), intent(in) :: input_platform
   real(kind=sreal), intent(in) :: T ! Kelvin

   ! return variable
   real(kind=sreal), dimension(2) :: plank_inv !out

   ! local variables
   integer(kind=byte) :: index ! index of row containing platform-specific coefficients
   real(kind=sreal), parameter :: Planck_C1 = 1.19104E-5 ! 2hc^2 in mW m-2 sr-1 (cm-1)-4
   real(kind=sreal), parameter :: Planck_C2 = 1.43877 ! hc/k  in K (cm-1)-1
   real(kind=sreal), dimension(4,27) :: coefficients ! coefficients containing variables

   ! select appropriate row of coefficient values
   select case (input_platform)
   case ("NOAA-5")
      index = 1
   case ("NOAA-6")
      index = 2
   case ("NOAA-7")
      index = 3
   case ("NOAA-8")
      index = 4
   case ("NOAA-9")
      index = 5
   case ("NOAA-10")
      index = 6
   case ("NOAA-11")
      index = 7
   case ("NOAA-12")
      index = 8
   case ("NOAA-14")
      index = 9
   case ("NOAA-15")
      index = 10
   case ("NOAA-16")
      index = 11
   case ("NOAA-17")
      index = 12
   case ("NOAA-18")
      index = 13
   case ("NOAA-19")
      index = 14
   case ("Metop-B")
      index = 15
   case ("Metop-A")
      index = 16
   case ("TERRA")
      index = 17
   case ("AQUA")
      index = 18
   case ("Envisat", "ERS2")
      index = 19
   case ("MSG-1", "MSG-2", "MSG-3", "MSG-4")
      index = 20
   case ("Himawari-8", "Himawari-9")
      index = 21
   case ("GOES-16", "GOES-17", "GOES-18")
      index = 22
   case ("Suomi-NPP", "NOAA-20", "NOAA-21")
      index = 23
   case ("Sentinel-3a", "Sentinel-3b")
      index = 24
   case ("FY-4A", "FY-4B")
      index = 25
   case ("MTG-I1")
      index = 26
   case ("default")
      index = 27
   case default
      write(*,*) "Error: Platform name does not match local string in function plank_inv"
      write(*,*) "Input platform name = ", input_platform
      stop
   end select

   ! v: wave number (cm-1)
   ! a: alpha parameter
   ! b: beta parameter
   ! solcon: solar constant

   ! Conversion from SADChan Planck coefficients to the ones here:
   ! v = (B1 / Planck_C1)^(1/3) = B2 / Planck_C2
   ! a = T2
   ! b = T1
   coefficients = reshape((/ &
      ! v       a         b        solcon     satname
        2655.741, 0.997915, 1.64511, 4.957, & ! noaa05, tirosn
        2671.543, 0.997563, 1.76241, 5.010, & ! noaa06
        2684.523, 0.997083, 1.94314, 5.061, & ! noaa07
        2651.378, 0.997580, 1.77211, 4.939, & ! noaa08
        2690.045, 0.997111, 1.87782, 5.081, & ! noaa09
        2672.616, 0.997374, 1.79397, 5.017, & ! noaa10
        2680.050, 0.996657, 1.73316, 5.077, & ! noaa11
        2651.771, 0.996999, 1.89956, 4.948, & ! noaa12
        2654.250, 0.996176, 1.87812, 4.973, & ! noaa14
        2695.974, 0.998015, 1.62126, 5.088, & ! noaa15
        2681.254, 0.998271, 1.67456, 5.033, & ! noaa16
        2669.141, 0.997335, 1.69576, 5.008, & ! noaa17
        2660.647, 0.997145, 1.71735, 4.981, & ! noaa18
        2670.242, 0.997411, 1.68202, 5.010, & ! noaa19
        2664.338, 0.997016, 1.76585, 4.996, & ! metop01, metopb
        2687.039, 0.996570, 2.05823, 5.077, & ! metop02, metopa
        2641.650, 0.999206, 0.48685, 4.896, & ! terra
        2641.790, 0.999205, 0.48803, 4.896, & ! aqua
        2675.166, 0.996344, 1.72695, 5.030, & ! env,ers2 (aatsr)
        2568.832, 0.995400, 3.43800, 4.660, & ! msg1, msg2, msg3, msg4
        2575.767, 0.999339, 0.46466, 4.688, & ! himawari8, himawari9
        2570.373, 0.999376, 0.43862, 4.672, & ! goes16, goes17
        2707.560, 0.999085, 0.58063, 5.123, & ! viirs SoumiNpp/NOAA20
        2673.797, 0.994884, 2.19600, 5.064, & ! slstr Sentinel-3
        2692.112, 0.996369, 2.64763, 5.093, & ! agri fengyun-4
        2631.579, 0.994040, 1.94224, 4.688, & ! MTG-I1 FCI
        2670.000, 0.998000, 1.75000, 5.000  & ! default
        /), (/ 4, 27 /))

   plank_inv(1) = Planck_C1 * coefficients(1 , index)**3 / &
        (exp(Planck_C2 * coefficients(1 , index) / &
        (coefficients(2 , index) * T &
        + coefficients(3 , index))) - 1.)
   plank_inv(2) = coefficients(4 , index)

end function plank_inv


function get_platform_index(input_platform)

   !use imager_structures_m
   use constants_cloud_typing_pavolonis_m

   implicit none

   ! input variables
   character(len=*), intent(in) :: input_platform

   ! output variable
   integer :: get_platform_index

   ! local variables
   integer(kind=byte) :: index ! index of row containing platform-specific coefficients

   ! select appropriate row of coefficient values
   select case (input_platform)
   case ("NOAA-5")
      index = 1
   case ("NOAA-6")
      index = 2
   case ("NOAA-7")
      index = 3
   case ("NOAA-8")
      index = 4
   case ("NOAA-9")
      index = 5
   case ("NOAA-10")
      index = 6
   case ("NOAA-11")
      index = 7
   case ("NOAA-12")
      index = 8
   case ("NOAA-13")
      index = 9
   case ("NOAA-14")
      index = 10
   case ("NOAA-15")
      index = 11
   case ("NOAA-16")
      index = 12
   case ("NOAA-17")
      index = 13
   case ("NOAA-18")
      index = 14
   case ("NOAA-19")
      index = 15
   case ("NOAA-20")
      index = 16
   case ("Metop-B")
      index = 17
   case ("Metop-A")
      index = 18
   case ("TERRA")
      index = 19
   case ("AQUA")
      index = 20
   case ("ERS2")
      index = 21
   case ("Envisat")
      index = 22
   case ("MSG-1")
      index = 23
   case ("MSG-2")
      index = 24
   case ("MSG-3")
      index = 25
   case ("MSG-4")
      index = 26
   case ("Sentinel-3a")
      index = 27
   case ("Sentinel-3b")
      index = 28
   case ("default")
      ! 15 = NOAA19, the baseline, for which slopes = 1 and intercepts = 0
      index = 15
   case default
      write(*,*) "Error: platform name does not match local string in subroutine scale_to_N19"
      write(*,*) "Input platform name = ", input_platform
      stop
   end select

   get_platform_index = index

end function get_platform_index

!***********************************************************************
end module cloud_typing_pavolonis_m
!***********************************************************************
! End of module CLOUD_TYPING_PAVOLONIS
