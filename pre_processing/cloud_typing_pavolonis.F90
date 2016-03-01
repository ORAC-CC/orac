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
!    calculated and implemented here; skip pixel if input SOLZEN is negative;
!    pass SATZEN as argument to NN cloud mask
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
!    missing degree-to-radians factor for coszen, changed coszen from
!    cos(sunZA) to cos(satZA)
! 2015/04/22, SS: introduce solar correction for reflectance channels ch1 and
!    ch3
! 2015/04/29, CP: changed from Env to Envisat
! 2015/05/19, GM: Fixed several race conditions introduced from recent commits
!    occurring when using OpenMP.
! 2015/07/03, OS: Added cldmask_uncertainty; added coefficients to calculate
!    NOAA19 Ch3.7 reflectance + slight update for other platforms
! 2015/07/27, AP: Replaced sym structure with parameters.
! 2015/11/17, OS: Added interpolation of ERA-Interim surface fields
!    snow_depth and sea_ice_cover; data are used for defining snow/ice
!    mask (here called NISE_MASK) input to neural net
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
!
! $Id$
!
! Bugs:
! None known.
!-----------------------------------------------------------------------

! Subroutines included in module:
!   CLOUD_TYPE
!   CLOUD_RETYPE
!
! DEPENDENCIES:
!   COMMON_CONSTANTS
!   IMAGER_STRUCTURES
!   SURFACE_STRUCTURES
!   NEURAL_NET_PREPROC
!   CONSTANTS_CLOUD_TYPING_PAVOLONIS
!
!
!
!-----------------------------------------------------------------------
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
!   CLOUD_TYPE
!   CLOUD_RETYPE
!
!-----------------------------------------------------------------------


! Begin of module
!***********************************************************************
module CLOUD_TYPING_PAVOLONIS
  !***********************************************************************

  implicit none

  private

  public :: CLOUD_TYPE !, CLOUD_RETYPE

  integer, parameter, private:: n_box=3

contains

  !----------------------------------------------------------------------
  ! SUBROUTINE NAME: CLOUD_TYPE
  !----------------------------------------------------------------------
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
  ! CALLING SEQUENCE:  call CLOUD_TYPE( j_min, num_scans_read )
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
  !----------------------------------------------------------------------
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
  !----------------------------------------------------------------------


  ! =====================================================================
  subroutine CLOUD_TYPE(channel_info, sensor, surface, imager_flags, &
       imager_angles, imager_geolocation, imager_measurements, &
       imager_pavolonis, ecmwf, platform, doy, verbose)
    ! =====================================================================

    !-- load necessary variable fields and constants
    use channel_structures
    use COMMON_CONSTANTS
    use IMAGER_STRUCTURES
    use SURFACE_STRUCTURES
    use NEURAL_NET_PREPROC
    use CONSTANTS_CLOUD_TYPING_PAVOLONIS
    use interpol
    use ecmwf_m, only : ecmwf_s


    !-- parameters to be passed

    type(channel_info_s),           intent(in)    :: channel_info
    character(len=sensor_length),   intent(in)    :: sensor
    type(surface_s),                intent(in)    :: surface
    type(imager_flags_s),           intent(in)    :: imager_flags
    type(imager_angles_s),          intent(in)    :: imager_angles
    type(imager_geolocation_s),     intent(in)    :: imager_geolocation
    type(imager_measurements_s),    intent(inout) :: imager_measurements
    type(imager_pavolonis_s),       intent(inout) :: imager_pavolonis
    type(ecmwf_s),                  intent(in)    :: ecmwf
    character(len=platform_length), intent(in)    :: platform
    integer(kind=sint),             intent(in)    :: doy
    logical,                        intent(in)    :: verbose


    !-- Declare some variables to hold various thresholds.

    real (kind=sreal):: NIR_PHASE_THRES, NIR_CIRRUS_THRES, NIR_OVER_THRES, &
         BTD3811_PHASE_THRES, EMS38_PHASE_THRES,            &
         BTD1112_DOVERLAP_THRES, BTD1112_CIRRUS_THRES,      &
         BTD1112_NOVERLAP_THRES_L, BTD1112_NOVERLAP_THRES_H,&
         EMS38_NOVERLAP_THRES_L, EMS38_NOVERLAP_THRES_H

    real(kind=dreal), dimension(7)   :: A1
    real(kind=dreal), dimension(7)   :: B1
    real(kind=dreal), dimension(7)   :: C1
    real(kind=dreal), dimension(7)   :: D1
    real(kind=dreal), dimension(7)   :: E1
    real(kind=dreal), dimension(7)   :: A2
    real(kind=dreal), dimension(7)   :: B2
    real(kind=dreal), dimension(7)   :: C2
    real(kind=dreal), dimension(7)   :: D2
    real(kind=dreal), dimension(7)   :: E2
    real(kind=dreal), dimension(7,8) :: A3
    real(kind=dreal), dimension(7,8) :: B3
    real(kind=dreal), dimension(7,8) :: C3
    real(kind=dreal), dimension(7,8) :: D3
    real(kind=dreal), dimension(7,8) :: E3
    real(kind=dreal), dimension(7,8) :: MIN_BTD1112_DOVERLAP
    real(kind=dreal), dimension(7)   :: MIN_BTD1112_NOVERLAP


    ! Declare some miscelaneous variables.

    integer :: i, j, index1, index2, wflg, j1, j2, &
         start_line, end_line, start_pix, end_pix, npix
    logical :: day
    real    :: t4_filter_thresh, nir_ref
    real(kind=sreal),allocatable,dimension(:,:) :: skint,snow_depth,sea_ice_cover
    integer(kind=byte), allocatable,dimension(:,:) :: snow_ice_mask
    type(interpol_s), allocatable, dimension(:) :: interp

    ! --------------------------------------------------------------------
    !
    !---- CC4CL requirements and adaptions for Pavolonis alg.

    integer(kind=sint) :: ch3a_on_avhrr_flag

    integer(kind=sint) :: ch2_on_atsr_flag,ch6_on_atsr_flag,ch7_on_atsr_flag

    real(kind=sreal)   :: glint_angle, coszen
    real(kind=sreal)   :: BTD_Ch3b_Ch4
    real(kind=sreal)   :: BTD_Ch4_Ch5
    real(kind=sreal)   :: rad_ch3b
    real(kind=sreal)   :: rad_ch3b_emis
    real(kind=sreal)   :: ref_ch3b
    real(kind=sreal)   :: ref_ch1
    real(kind=sreal)   :: ref_ch3a
    real(kind=sreal)   :: solcon_ch3b
    real(kind=sreal)   :: mu0
    real(kind=sreal)   :: esd
    real(kind=sreal)   :: c_sun
    real(kind=sreal),dimension(2)   :: PlanckInv_out
    integer(kind=sint) :: ch1, ch2, ch3, ch4, ch5, ch6

    ! -- Parameters used here
    !
    ! cirrus_quality      = quality of cirrus flag
    ! coszen              = cosine of the solar zenith angle
    ! ch3a_on_avhrr_flag  = whether or not AVHRR channel 3a is
    !                       used (NO, YES, INEXISTENT)
    ! BTD                 = Brightness Temperature Difference
    ! BTD_Ch3b_Ch4        = BT Ch3b minus BT Ch4
    ! BTD_Ch4_Ch5         = BT Ch4 minus BT Ch5
    ! BTD_Ch4_Ch3b        = BT Ch4 minus BT Ch3b
    !
    !
    ! -- INPUT
    !                                   wavelength  MODIS=CC4CL=AVHRR=AATSR
    ! imager_measurements%DATA(i,j,1)   ! 0.659 um  !Ch01=  1  =1
    ! imager_measurements%DATA(i,j,2)   ! 0.865 um  !Ch02=  2  =2
    ! imager_measurements%DATA(i,j,3)   ! 1.640 um  !Ch06=  3  =3a
    ! imager_measurements%DATA(i,j,4)   ! 3.750 um  !Ch20=  4  =3b
    ! imager_measurements%DATA(i,j,5)   !11.030 um  !Ch31=  5  =4
    ! imager_measurements%DATA(i,j,6)   !12.020 um  !Ch32=  6  =5
    !
    ! imager_geolocation%LATITUDE(i,j)
    ! imager_geolocation%LONGITUDE(i,j)
    ! imager_geolocation%DEM(i,j)
    ! imager_geolocation%STARTX
    ! imager_geolocation%STARTY
    ! imager_geolocation%ENDX
    ! imager_geolocation%ENDY
    ! imager_geolocation%NX
    ! imager_geolocation%NY
    !
    ! imager_angles%SATZEN(i,j,imager_angles%NVIEWS)
    ! imager_angles%SOLZEN(i,j,imager_angles%NVIEWS)
    !
    ! imager_flags%LUSFLAG(i,j)
    ! imager_flags%LSFLAG(i,j)
    !
    ! surface%EMISSIVITY(i,j,k): currently k = 1 (Ch3b)
    ! surface%NISE_MASK(i,j)
    !
    !
    ! --- OUTPUT
    !
    ! imager_pavolonis%SUNGLINT_MASK ... sunglint mask: YES, NO
    ! imager_pavolonis%SFCTYPE ... surface type - NISE corrected LUS
    ! imager_pavolonis%CLDTYPE ... cloud type based on pavolonis
    ! imager_pavolonis%CLDMASK ... cloud mask based on L1c thresholding
    ! imager_pavolonis%CLDMASK_UNCERTAINTY ... cloud mask uncertainty
    ! imager_pavolonis%CCCOT   ... cloud cover COTs
    !
    !---------------------------------------------------------------------


    ! Determine channel indexes based on instrument channel number
    if (trim(adjustl(sensor)) .eq. 'AATSR' .or. trim(adjustl(sensor)) .eq. 'ATSR2' ) then
       do i=1,channel_info%nchannels_total
          select case (channel_info%channel_ids_instr(i))
          case(2)
             ch1=i
          case(3)
             ch2=i
          case(4)
             ch3=i
          case(5)
             ch4=i
          case(6)
             ch5=i
          case(7)
             ch6=i
          end select

       end do

    else if (trim(adjustl(sensor)) .eq. 'AVHRR') then
       do i=1,channel_info%nchannels_total
          select case (channel_info%channel_ids_instr(i))
          case(1)
             ch1=i
          case(2)
             ch2=i
          case(3)
             ch3=i
          case(4)
             ch4=i
          case(5)
             ch5=i
          case(6)
             ch6=i
          end select
       end do
    else if (trim(adjustl(sensor)) .eq. 'MODIS') then
       do i=1,channel_info%nchannels_total
          select case (channel_info%channel_ids_instr(i))
          case(1)
             ch1=i
          case(2)
             ch2=i
          case(6)
             ch3=i
          case(20)
             ch4=i
          case(31)
             ch5=i
          case(32)
             ch6=i
          end select
       end do
    else if (trim(adjustl(sensor)) .eq. 'SEVIRI') then
       do i=1,channel_info%nchannels_total
          select case (channel_info%channel_ids_instr(i))
          case(1)
             ch1=i
          case(2)
             ch2=i
          case(3)
             ch3=i
          case(4)
             ch4=i
          case(9)
             ch5=i
          case(10)
             ch6=i
          end select
       end do
    end if

    allocate(skint(imager_geolocation%startx:imager_geolocation%endx, &
         1:imager_geolocation%ny))
    skint=sreal_fill_value
    allocate(snow_depth(imager_geolocation%startx:imager_geolocation%endx, &
         1:imager_geolocation%ny))
    snow_depth=sreal_fill_value
    allocate(sea_ice_cover(imager_geolocation%startx:imager_geolocation%endx, &
         1:imager_geolocation%ny))
    sea_ice_cover=sreal_fill_value
    allocate(snow_ice_mask(imager_geolocation%startx:imager_geolocation%endx, &
         1:imager_geolocation%ny))
    snow_ice_mask=byte_fill_value
    allocate(interp(1))

    do i=1,imager_geolocation%ny
       do j=imager_geolocation%startx,imager_geolocation%endx

          call bilinear_coef(ecmwf%lon, ecmwf%xdim, ecmwf%lat, &
               ecmwf%ydim, imager_geolocation%longitude(j,i), &
               imager_geolocation%latitude(j,i), interp(1))

          call interp_field (ecmwf%skin_temp, skint(j,i), interp(1))
          call interp_field (ecmwf%snow_depth, snow_depth(j,i), interp(1))
          call interp_field (ecmwf%sea_ice_cover, sea_ice_cover(j,i), interp(1))

          if ( &
               ((snow_depth(j,i) .GT. 0.01)    .AND. (imager_flags%LSFLAG(j,i)         .EQ. 1_byte)) .OR. & 
               ((snow_depth(j,i) .GT. 0.01)    .AND. (imager_geolocation%latitude(j,i) .lt. -60.00)) .OR. &
               ((sea_ice_cover(j,i) .GT. 0.15) .AND. (imager_flags%LSFLAG(j,i)         .EQ. 0_byte))      &
             ) then
             snow_ice_mask(j,i) = YES
          else
             snow_ice_mask(j,i) = NO
          endif

       end do
    end do


    deallocate(interp)
    !-- copy land use flag array to Surface TYPE array
    imager_pavolonis%SFCTYPE = imager_flags%LUSFLAG

    !-- correction of SFCTYPE with NISE aux. data
    where (snow_ice_mask .eq. YES)
       imager_pavolonis%SFCTYPE = NISE_FLAG
    endwhere

    !-- initialize cloud mask as cloudy
    !where (imager_pavolonis%CLDMASK .eq. byte_fill_value)
    !imager_pavolonis%CLDMASK = CLOUDY
    !endwhere

    ! load external file containing fill coefficients
    include 'pavolonis_fill_coefficients.inc'

    !=====================================================================
    !                                                                    !
    !                     Begin cloud typing.                            !
    !                                                                    !
    !=====================================================================

    ! j1      = always 1 (first scanline)
    ! j2      = n_along_track (last scanline)
    ! num_pix = n_across_track (number of pixels per scanline)

    !---------------------------------------------------------------------
    !$OMP PARALLEL &
    !$OMP PRIVATE(i) &
    !$OMP PRIVATE(j) &
    !$OMP PRIVATE(ch3a_on_avhrr_flag) &
    !$OMP PRIVATE(ch2_on_atsr_flag) &
    !$OMP PRIVATE(ch6_on_atsr_flag) &
    !$OMP PRIVATE(ch7_on_atsr_flag) &
    !$OMP PRIVATE(glint_angle) &
    !$OMP PRIVATE(coszen) &
    !$OMP PRIVATE(BTD_Ch4_Ch5) &
    !$OMP PRIVATE(BTD_Ch3b_Ch4) &
    !$OMP PRIVATE(day) &
    !$OMP PRIVATE(PlanckInv_out) &
    !$OMP PRIVATE(rad_ch3b) &
    !$OMP PRIVATE(solcon_ch3b) &
    !$OMP PRIVATE(rad_ch3b_emis) &
    !$OMP PRIVATE(mu0) &
    !$OMP PRIVATE(esd) &
    !$OMP PRIVATE(c_sun) &
    !$OMP PRIVATE(ref_ch3b) &
    !$OMP PRIVATE(ref_ch1) &
    !$OMP PRIVATE(ref_ch3a) &
    !$OMP PRIVATE(nir_ref) &
    !$OMP PRIVATE(index1) &
    !$OMP PRIVATE(index2) &
    !$OMP PRIVATE(BTD1112_CIRRUS_THRES) &
    !$OMP PRIVATE(BTD1112_DOVERLAP_THRES) &
    !$OMP PRIVATE(wflg) &
    !$OMP PRIVATE(NIR_CIRRUS_THRES) &
    !$OMP PRIVATE(NIR_PHASE_THRES) &
    !$OMP PRIVATE(NIR_OVER_THRES) &
    !$OMP PRIVATE(BTD3811_PHASE_THRES) &
    !$OMP PRIVATE(EMS38_PHASE_THRES) &
    !$OMP PRIVATE(BTD1112_NOVERLAP_THRES_H) &
    !$OMP PRIVATE(BTD1112_NOVERLAP_THRES_L) &
    !$OMP PRIVATE(EMS38_NOVERLAP_THRES_H) &
    !$OMP PRIVATE(EMS38_NOVERLAP_THRES_L)

    !$OMP DO SCHEDULE(GUIDED)
    !-- loop over all pixels (x)
    !i_loop: do  i = 1, num_pix

    i_loop: do  i = imager_geolocation%STARTX, imager_geolocation%ENDX

       !-------------------------------------------------------------------
       !-- loop over scanlines (y)
       !j_loop: do j = j1, j2 + j1 - 1
       ! e.g. j1=100, j2=400, i.e. start with 100 and take next 400 lines
       ! j2 = number of scanlines to be read

       j_loop: do j = 1, imager_geolocation%ny !imager_geolocation%STARTY,
          ! imager_geolocation%ENDY

          !-- check if solar zenith angle is > 0

          if ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) .lt. 0. ) cycle


          !-- check if Ch3a is available or not (fill_value is negative)


          if ( imager_measurements%DATA(i,j,ch3) .ge. 0 .and. &
               imager_measurements%DATA(i,j,ch4) .lt. 0) then

             ! Ch3a is used if Ch3b is not avail.
             ch3a_on_avhrr_flag = YES

          elseif ( imager_measurements%DATA(i,j,ch4) .ge. 0 ) then

             ! Ch3b is used if avail.
             ch3a_on_avhrr_flag = NO

          else

             ! neither Ch3a nor Ch3b avail.
             ch3a_on_avhrr_flag = INEXISTENT

          endif

          if (trim(adjustl(sensor)) .eq. 'AATSR' .or. trim(adjustl(sensor)) .eq. 'ATSR2' ) then
             ! changed min value of 3.7 for AATSR
             if ( imager_measurements%DATA(i,j,ch3) .ge. 0 .and. &
                  imager_measurements%DATA(i,j,ch4) .lt. 100) then

                ! Ch3a is used if Ch3b is not avail.
                ch3a_on_avhrr_flag = YES

             elseif ( imager_measurements%DATA(i,j,ch4) .ge. 100 ) then

                ! Ch3b is used if avail.
                ch3a_on_avhrr_flag = NO

             else

                ! neither Ch3a nor Ch3b avail.
                ch3a_on_avhrr_flag = INEXISTENT

             endif
	  endif


   ! check if ATSR 12um channel is missing
          ch7_on_atsr_flag = YES

          if ( imager_measurements%DATA(i,j,ch5) .ge. 100 .and. &
               imager_measurements%DATA(i,j,ch6) .lt. 100. ) then
             ch7_on_atsr_flag = NO
          endif

          ch2_on_atsr_flag = YES
          if ( imager_measurements%DATA(i,j,ch2) .lt. 0. ) then
             ch2_on_atsr_flag = NO
          endif

          ! check if ATSR 11um channel is missing because too warm
          !          ch6_on_atsr_flag = YES


          if ( imager_measurements%DATA(i,j,ch6) .ge. 220 .and. &
               imager_measurements%DATA(i,j,ch5) .lt. 100. ) then
             ch6_on_atsr_flag = NO
	  endif


   ! check if ATSR 11um channel is missing because too cold
          ch6_on_atsr_flag = YES

          if ( imager_measurements%DATA(i,j,ch6) .lt. 100 .and. &
               imager_measurements%DATA(i,j,ch5) .lt. 100. ) then
             ch6_on_atsr_flag = NO
             ch7_on_atsr_flag = NO
	  endif

   !-- check for sunglint and save result:

   ! In PATMOS sunglint calculation:
          if ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) .ne. sreal_fill_value .and. &
               imager_angles%SATZEN(i,j,imager_angles%NVIEWS) .ne. sreal_fill_value .and. &
               imager_angles%RELAZI(i,j,imager_angles%NVIEWS) .ne. sreal_fill_value ) then

             glint_angle = &
                  cos ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) * d2r ) * &
                  cos ( imager_angles%SATZEN(i,j,imager_angles%NVIEWS) * d2r ) + &
                  sin ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) * d2r ) * &
                  sin ( imager_angles%SATZEN(i,j,imager_angles%NVIEWS) * d2r ) * &
                  cos ( imager_angles%RELAZI(i,j,imager_angles%NVIEWS) * d2r )

             glint_angle = max( -1.0, min( glint_angle, 1.0 ) )
             glint_angle = acos(glint_angle) / d2r

          else

             glint_angle = sreal_fill_value

          endif

          !-- calculate BT differences

          ! BT(11) minus BT(12)
          BTD_Ch4_Ch5 = imager_measurements%DATA(i,j,ch5) - &
               imager_measurements%DATA(i,j,ch6)

          ! BT(3.75) minus BT(11)
          BTD_Ch3b_Ch4 = imager_measurements%DATA(i,j,ch4) - &
               imager_measurements%DATA(i,j,ch5)



          !-- NEURAL_NET_PREPROC subroutine
          call ann_cloud_mask( &
               imager_measurements%DATA(i,j,ch1), &
               imager_measurements%DATA(i,j,ch2), &
               imager_measurements%DATA(i,j,ch4), &
               imager_measurements%DATA(i,j,ch5), &
               imager_measurements%DATA(i,j,ch6), &
               imager_angles%SOLZEN(i,j,imager_angles%NVIEWS), &
               imager_angles%SATZEN(i,j,imager_angles%NVIEWS), &
               int(imager_geolocation%DEM(i,j), lint), &
               snow_ice_mask(i,j), imager_flags%LSFLAG(i,j), &
               imager_flags%LUSFLAG(i,j), &
               surface%albedo(i,j,1), &
               surface%albedo(i,j,2), &
               imager_pavolonis%CCCOT_pre(i,j), &
               imager_pavolonis%CLDMASK(i,j) , &
               imager_pavolonis%CLDMASK_UNCERTAINTY(i,j) , &
               imager_geolocation%LATITUDE(i,j) , &
               skint(i,j) , &
               ch3a_on_avhrr_flag, &
               i, j, &
               glint_angle, &
               sensor, &
               platform, &
               verbose )

          ! cycle if clear, as no need to define cloud type
          if ( imager_pavolonis%CLDMASK(i,j) == CLEAR ) then
             imager_pavolonis%CLDTYPE(i,j) = CLEAR_TYPE
             if (trim(adjustl(sensor)) .ne. 'AATSR' .or. trim(adjustl(sensor)) .eq. 'ATSR2') then
                cycle
             endif
	  endif

   ! implement extra tests for AATSR

   !-- First Pavolonis test: clear or cloudy

          if (trim(adjustl(sensor)) .eq. 'AATSR' .or. trim(adjustl(sensor)) .eq. 'ATSR2' ) then

             ! 11um channel can occasionally be missing particuarly for AATSR instrument if it gets too warm
             ! also when ch6 atsr is fill value,clear type is assigned
             if ( ( ch6_on_atsr_flag == NO )  .and.  ( ch7_on_atsr_flag == YES ) ) then
                imager_pavolonis%CLDTYPE(i,j) = CLEAR_TYPE
                imager_pavolonis%CLDMASK(i,j) = CLEAR
             endif

             ! 12um channel can occasionally be missing particuarly for AATSR instrument
             ! also when ch7 atsr is fill value, %PROB_OPAQUE_ICE_TYPE is assigned

             if ( (ch7_on_atsr_flag == NO ) .and. (ch2_on_atsr_flag == YES) ) then
                imager_pavolonis%CLDTYPE(i,j) = PROB_OPAQUE_ICE_TYPE
                imager_pavolonis%CLDMASK(i,j) = CLOUDY

             endif

             ! if both IR channels are missing them likely a very cold opaque
             ! cloud enable a CTH retrieval by setting minimum threshold values
             ! of IR channels
             if  ( ( ch6_on_atsr_flag == NO )  .and. ( ch7_on_atsr_flag == NO ) .and. (ch2_on_atsr_flag == yes)) then
                imager_pavolonis%CLDTYPE(i,j) = PROB_OPAQUE_ICE_TYPE
                !imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE
		imager_measurements%DATA(i,j,ch6)=210.0
		imager_measurements%DATA(i,j,ch5)=209.0
                imager_pavolonis%CLDMASK(i,j) = CLOUDY
             endif

             if ( (ch7_on_atsr_flag == NO )  ) then
                ! check if over very cold atlantic plateau Antarctica in which case probably clear if 12um is missing
                if ((imager_pavolonis%SFCTYPE(i,j) == NISE_FLAG) .and.   (imager_geolocation%LATITUDE(i,j) < -70.0) ) then
                   imager_pavolonis%CLDTYPE(i,j) = PROB_CLEAR_TYPE
                   imager_pavolonis%CLDMASK(i,j) = CLEAR
                endif
             endif

             if ( imager_pavolonis%CLDMASK(i,j) == CLEAR ) then
                imager_pavolonis%CLDTYPE(i,j) = CLEAR_TYPE
                cycle
             endif
	  endif

   !-- neither ch3a nor ch3b available
          if (trim(adjustl(sensor)) .eq. 'AVHRR') then
             !-- at night, assign probably opaque ice flag
             !-- as ch3.7 has fill value due to low S/N
             !only apply to avhrr data as when fill for aatsr it is actaully warm
             if ( ch3a_on_avhrr_flag == INEXISTENT ) then
                !                if ( ( imager_geolocation%LATITUDE(i,j) < 65.0 .and. &
                !                     imager_geolocation%LATITUDE(i,j) > -65.0 ) .and. &
                !                     ( day .eqv. .FALSE. ) ) &
                !     imager_pavolonis%CLDTYPE(i,j) = PROB_OPAQUE_ICE_TYPE
                if ( ( imager_measurements%DATA(i,j,ch5) > 0. ) .and. ( imager_measurements%DATA(i,j,ch5) <= 233.16 ) ) then

                   imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE

                elseif ( (imager_measurements%DATA(i,j,ch5) > 233.16) .and. &
                     (imager_measurements%DATA(i,j,ch5) <= 253.16) ) then

                   imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE

                elseif ( (imager_measurements%DATA(i,j,ch5) > 253.16) .and. &
                     (imager_measurements%DATA(i,j,ch5) <= 273.16)) then

                   imager_pavolonis%CLDTYPE(i,j) =SUPERCOOLED_TYPE

                else

                   imager_pavolonis%CLDTYPE(i,j) = WATER_TYPE

                endif

                cycle

             endif ! end avhrr channel flag

          endif ! end avhrr


          ! calculate ch3b radiance and emissivity
          PlanckInv_out  = PlanckInv( platform, imager_measurements%DATA(i,j,ch4) )
          rad_ch3b       = PlanckInv_out(1)
          solcon_ch3b    = PlanckInv_out(2)
          PlanckInv_out  = PlanckInv( platform, imager_measurements%DATA(i,j,ch5) )
          rad_ch3b_emis  = PlanckInv_out(1)
          mu0 = cos ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) * d2r )
          esd = 1.0 - 0.0167 * cos( 2.0 * pi * ( doy - 3 ) / 365.0 )
          c_sun = 1. / esd**2
          imager_pavolonis%emis_ch3b(i,j) = rad_ch3b / rad_ch3b_emis

          ! calculate true reflectances for avhrr, modis and aatsr
          ref_ch1  = imager_measurements%DATA(i,j,ch1) / mu0
          ref_ch3a = imager_measurements%DATA(i,j,ch3) / mu0
          ref_ch3b = ( rad_ch3b - rad_ch3b_emis ) / ( solcon_ch3b * c_sun * mu0 - rad_ch3b_emis )
          ! make sure reflectances are positive, else fill value
          if (ref_ch1 .lt. 0.) ref_ch1 = sreal_fill_value
          if (ref_ch3a .lt. 0.) ref_ch3a = sreal_fill_value
          if (ref_ch3b .lt. 0.) ref_ch3b = sreal_fill_value

          !-- nir_ref = channel 3a or channel 3b reflectance
          nir_ref = sreal_fill_value


          !-- Determine the viewing zenith angle bin.
          index1 = min(7,max(1,int(imager_angles%SATZEN(i,j,imager_angles &
               %NVIEWS)/10.0) + 1))

          !-- Determine the solar zenith angle bin.
          index2 = min(8,max(1,int(imager_angles%SOLZEN(i,j,imager_angles &
               %NVIEWS)/10.0) + 1))


          !-- Set 11um - 12um cirrus thresholds.
          !   Absorption of radiation by water vapor and ice crystals in
          !   semitransparent cirrus clouds is greater at 12 than 11 micron

          BTD1112_CIRRUS_THRES = &
               A1(index1) + &
               B1(index1)*imager_measurements%DATA(i,j,ch5) + &
               C1(index1)*imager_measurements%DATA(i,j,ch5)**2 + &
               D1(index1)*imager_measurements%DATA(i,j,ch5)**3 + &
               E1(index1)*imager_measurements%DATA(i,j,ch5)**4


          BTD1112_CIRRUS_THRES = max( 1.0, min(4.0,BTD1112_CIRRUS_THRES) )


          !-- initial cirrus quality

          imager_pavolonis%cirrus_quality(i,j) = 0

          !-- Check if daytime or nighttime algorithm is to be used.
          day = .FALSE.
          if ( ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) < 88.0 ) .or. ( ch3a_on_avhrr_flag .eq. YES ) ) then
             day = .TRUE.
          endif

          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          !-- If DAYTIME, use daytime algorithm.
          !
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          if ( (imager_pavolonis%CLDMASK(i,j) == CLOUDY) .and. &
               (day .eqv. .TRUE.) ) then

             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Set 11um - 12um overlap thresholds.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             !if ( (imager_measurements%DATA(i,j,ch1) >= 35.0) .and. &
             !   (imager_measurements%DATA(i,j,ch1) <= 60.0) ) then
             if ( (ref_ch1 >= 0.35) .and. (ref_ch1 <= 0.60) ) then

                BTD1112_DOVERLAP_THRES = max( &
                     ( ( A3(index1,index2)+ &
                                !    B3(index1,index2)* imager_measurements%DATA(i,j,ch1)*0.01 + &
                                !    C3(index1,index2)*(imager_measurements%DATA(i,j,ch1)*0.01)**2 + &
                                !    D3(index1,index2)*(imager_measurements%DATA(i,j,ch1)*0.01)**3 + &
                                !    E3(index1,index2)*(imager_measurements%DATA(i,j,ch1)*0.01)**4 ) - 0.1 ), &
                     B3(index1,index2)* ref_ch1 + &
                     C3(index1,index2)*(ref_ch1)**2 + &
                     D3(index1,index2)*(ref_ch1)**3 + &
                     E3(index1,index2)*(ref_ch1)**4 ) - 0.1 ), &
                     MIN_BTD1112_DOVERLAP(index1,index2) - 0.1 )

                !elseif ( imager_measurements%DATA(i,j,ch1) > 60.0 .and. &
                !       imager_measurements%DATA(i,j,ch1) < 90.0 ) then
             elseif ( ref_ch1 > 0.60 .and. ref_ch1 < 0.90 ) then

                BTD1112_DOVERLAP_THRES = MIN_BTD1112_DOVERLAP(index1,index2) - 0.1

             else

                BTD1112_DOVERLAP_THRES = 9999.0

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- In the high latitudes, the Ch3b 3.75 um must be
             !   less than 20% to prevent single layer water clouds
             !   from being typed as overlap
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             if ( ( ch3a_on_avhrr_flag == NO ) .and. &
                  ( ( imager_geolocation%LATITUDE(i,j) > 65.0 )   .or.  &
                  ( imager_geolocation%LATITUDE(i,j) < -65.0 ) ) .and. &
                  ( ref_ch3b > 0.20 ) ) then

                BTD1112_DOVERLAP_THRES = 9999.0

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform initial IR window brightness temperature-based typing
             !   wflg = IR window flag
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             wflg = 1

             if ( imager_measurements%DATA(i,j,ch5) <= 233.16 ) then

                imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE
                wflg = 0

             elseif ( (imager_measurements%DATA(i,j,ch5) > 233.16) .and. &
                  (imager_measurements%DATA(i,j,ch5) <= 253.16) ) then

                imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE

             elseif ( (imager_measurements%DATA(i,j,ch5) > 253.16) .and. &
                  (imager_measurements%DATA(i,j,ch5) <= 273.16)) then

                imager_pavolonis%CLDTYPE(i,j) =SUPERCOOLED_TYPE

             else

                ! BT(11) > 273.16 K, the melting point of pure water,
                ! the cirrus detection test is simply applied.
                ! If it is passed, then the pixel is classified as nonopaque
                ! ice cloud, otherwise, it is a warm liquid water cld type.
                ! [Pavolonis et al. (2005)]

                imager_pavolonis%CLDTYPE(i,j) = WATER_TYPE

             endif


             !---------------------------------------------!
             !                                             !
             !---- Use 1.6 um algorithm, if available ---- !
             !                                             !
             !---------------------------------------------!

             if ( (ch3a_on_avhrr_flag == YES) .and. ( ref_ch1 > 0.0) ) then


                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                ! Set some 1.6 um thresholds used in phase identification
                ! and cirrus detection.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                if ( (imager_pavolonis%SFCTYPE(i,j) == WATER_FLAG) .or. &
                     (imager_pavolonis%SFCTYPE(i,j) == NISE_FLAG) ) then

                   !NIR_CIRRUS_THRES = 20.0
                   !NIR_PHASE_THRES  = 17.0
                   NIR_CIRRUS_THRES = 0.20
                   NIR_PHASE_THRES  = 0.17

                elseif (imager_pavolonis%SFCTYPE(i,j) == DESERT_FLAG) then

                   !NIR_CIRRUS_THRES = 55.0
                   !NIR_PHASE_THRES  = 32.0
                   NIR_CIRRUS_THRES = 0.55
                   NIR_PHASE_THRES  = 0.32

                   ! all other surface types
                else

                   !NIR_CIRRUS_THRES = 33.0
                   !NIR_PHASE_THRES  = 32.0
                   NIR_CIRRUS_THRES = 0.33
                   NIR_PHASE_THRES  = 0.32

                endif


                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                !-- Set the minimum 1.6 um reflectance allowed for
                !   cloud overlap over SNOW/ICE.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                if (imager_pavolonis%SFCTYPE(i,j) == NISE_FLAG) then
                   !NIR_OVER_THRES = 17.0
                   NIR_OVER_THRES = 0.17
                else
                   NIR_OVER_THRES = 0.0
                endif


                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                !-- The reflectance used in the typing tests to 1.65 um
                !   when Ch3a is on
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                nir_ref = ref_ch3a


                !---------------------------------------!
                !                                       !
                !----      Use 3.75 um channel      ----!
                ! (used only if 1.6um is not available) !
                !                                       !
                !---------------------------------------!

             elseif ( (ch3a_on_avhrr_flag == NO) .and. ( ref_ch1 > 0.0) ) then


                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                ! Set some 3.75 um thresholds used in phase identification
                ! and cirrus detection.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                if ( (imager_pavolonis%SFCTYPE(i,j) == WATER_FLAG) .or. &
                     (imager_pavolonis%SFCTYPE(i,j) == NISE_FLAG) ) then

                   !NIR_CIRRUS_THRES = 12.0
                   !NIR_PHASE_THRES  = 6.0
                   NIR_CIRRUS_THRES = 0.12
                   NIR_PHASE_THRES  = 0.06

                elseif (imager_pavolonis%SFCTYPE(i,j) == DESERT_FLAG) then

                   !NIR_CIRRUS_THRES = 40.0
                   !NIR_PHASE_THRES  = 6.0
                   NIR_CIRRUS_THRES = 0.40
                   NIR_PHASE_THRES  = 0.06

                   ! all other surface types
                else

                   !NIR_CIRRUS_THRES = 12.0
                   !NIR_PHASE_THRES  = 6.0
                   NIR_CIRRUS_THRES = 0.12
                   NIR_PHASE_THRES  = 0.06

                endif



                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                !-- Set the minimum 3.75 um reflectance allowed for
                !   cloud overlap over SNOW/ICE.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                if (imager_pavolonis%SFCTYPE(i,j) == NISE_FLAG) then
                   !NIR_OVER_THRES = 6.0
                   NIR_OVER_THRES = 0.06
                else
                   NIR_OVER_THRES = 0.0
                endif


                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                !-- The reflectance used in the typing tests to 3.75 um
                !   when Ch3b is on.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                nir_ref = ref_ch3b

             else

                ! In case it is daytime, but ref_ch1 <= 0, skip further Pavolonis tests.
                ! Algorithm output is simple IR window brightness temperature-based typing
                ! as applied previously.
                cycle

                ! ----------------------------------------- !
                !                                           !
                ! end of if loop: CH3a or CH3b availability !
                !                                           !
                ! ----------------------------------------- !
             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform the NIR reflectance bulk cloud phase test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( imager_pavolonis%CLDTYPE(i,j) == SUPERCOOLED_TYPE ) .and. &
                  ( nir_ref <= NIR_PHASE_THRES ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 263.16 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE

             endif

             if ( ( imager_pavolonis%CLDTYPE(i,j) == OPAQUE_ICE_TYPE ) .and. &
                  ( wflg == 1 ) .and. &
                  ( nir_ref > NIR_PHASE_THRES ) ) then

                imager_pavolonis%CLDTYPE(i,j) = SUPERCOOLED_TYPE

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform the cloud overlap test
             !   !! not used over DESERT surfaces !!
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( BTD_Ch4_Ch5 > BTD1112_DOVERLAP_THRES ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 270.0) .and. &
                  ( imager_pavolonis%SFCTYPE(i,j) /= DESERT_FLAG ) .and. &
                  ( nir_ref > NIR_OVER_THRES ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) > 210.0 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = OVERLAP_TYPE

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for cirrus clouds.
             !   note, akh modified so that nir_ref test
             !   only applied when Solzen < 70
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( imager_pavolonis%CLDTYPE(i,j) /= OVERLAP_TYPE ) .and. &
                  ( BTD_Ch4_Ch5 > (BTD1112_CIRRUS_THRES-0.2) ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 295.0 ) ) then

                if (imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) < 70.0) then

                   if ( nir_ref < NIR_CIRRUS_THRES ) then
                      imager_pavolonis%CLDTYPE(i,j) = CIRRUS_TYPE
                      imager_pavolonis%cirrus_quality(i,j) = 1
                   endif

                else

                   imager_pavolonis%CLDTYPE(i,j) = CIRRUS_TYPE
                   imager_pavolonis%cirrus_quality(i,j) = 0
                   ! note, this is a low quality

                endif !end of solzen if loop

             endif !end of look for cirrus clouds


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for fog - not used over DESERT(=12) surfaces.
             !   The ref3b/ref1 threshold is used to prevent near terminator
             !   and sunglint pixels from being classified as fog.
             !   THERE IS CURRENTLY NO FOG DETECTION WHEN CH3A IS ON!
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( ch3a_on_avhrr_flag == NO ) .and. &
                  ( ref_ch3b >= 0.25) .and. &
                  ( imager_pavolonis%SFCTYPE(i,j) /= DESERT_FLAG ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) > 240.0 ) .and. &
                  ( ( ref_ch3b / ref_ch1 ) < 0.6 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = FOG_TYPE

             endif


             !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             !
             !-- If nighttime, use tri-spectral nighttime algorithm.
             !
             !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          elseif ( (imager_pavolonis%CLDMASK(i,j) == CLOUDY) .and. &
               (day .eqv. .false.) ) then


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Set 3.75um - 11um thresholds used for phase determination.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             BTD3811_PHASE_THRES = A2(index1) + &
                  B2(index1)*BTD_Ch4_Ch5    + &
                  C2(index1)*BTD_Ch4_Ch5**2 + &
                  D2(index1)*BTD_Ch4_Ch5**3 + &
                  E2(index1)*BTD_Ch4_Ch5**4

             BTD3811_PHASE_THRES = min(8.0,max(-2.0,BTD3811_PHASE_THRES))



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Set the 3.75 um thresholds used for phase determination.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if (imager_measurements%DATA(i,j,ch5) <= 245.0) then

                if (imager_pavolonis%SFCTYPE(i,j) == WATER_FLAG) then
                   EMS38_PHASE_THRES = 0.9
                else
                   EMS38_PHASE_THRES = 0.9
                endif

             else ! BT11 > 245 (Ch4 BT: 11 micron)

                if (imager_pavolonis%SFCTYPE(i,j) == WATER_FLAG) then
                   EMS38_PHASE_THRES = 1.12
                else
                   EMS38_PHASE_THRES = 1.12
                endif

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Set the split window and EMS3b thresholds used in nighttime
             !   cloud overlap detection.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( BTD_Ch3b_Ch4 > 0.0 ) then

                if (imager_pavolonis%SFCTYPE(i,j) == WATER_FLAG) then

                   if ( ( imager_geolocation%LATITUDE(i,j) > -30.0 ) .and. &
                        ( imager_geolocation%LATITUDE(i,j) < 30.0  ) ) then

                      BTD1112_NOVERLAP_THRES_H = 2.5
                      BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)+0.2
                      EMS38_NOVERLAP_THRES_H = 5.0
                      EMS38_NOVERLAP_THRES_L = 1.1

                   else

                      BTD1112_NOVERLAP_THRES_H = 2.0
                      BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)
                      EMS38_NOVERLAP_THRES_H = 2.5
                      EMS38_NOVERLAP_THRES_L = 1.05

                   endif

                   ! all other surface types
                else

                   if ( ( imager_geolocation%LATITUDE(i,j) > -30.0 ) .and. &
                        ( imager_geolocation%LATITUDE(i,j) < 30.0 ) ) then

                      BTD1112_NOVERLAP_THRES_H = 2.5
                      BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)+0.2
                      EMS38_NOVERLAP_THRES_H = 5.0
                      EMS38_NOVERLAP_THRES_L = 1.1

                   else

                      BTD1112_NOVERLAP_THRES_H = 2.0
                      BTD1112_NOVERLAP_THRES_L = MIN_BTD1112_NOVERLAP(index1)
                      EMS38_NOVERLAP_THRES_H = 2.0
                      EMS38_NOVERLAP_THRES_L = 1.0

                   endif

                endif !end of SFCTYPE if loop

                ! These thresholds are not applied if
                ! Ch3b minus Ch4 (BTD_Ch3b_Ch4) <= 0
             else

                BTD1112_NOVERLAP_THRES_H = -99.0
                BTD1112_NOVERLAP_THRES_L = 999.0
                EMS38_NOVERLAP_THRES_H = -99.0
                EMS38_NOVERLAP_THRES_L = 999.0

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- These thresholds are not applied if the surface type
             !   is DESERT
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( imager_pavolonis%SFCTYPE(i,j) == DESERT_FLAG ) then

                BTD1112_NOVERLAP_THRES_H = -99.0
                BTD1112_NOVERLAP_THRES_L = 999.0
                EMS38_NOVERLAP_THRES_H = -99.0
                EMS38_NOVERLAP_THRES_L = 999.0

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform initial IR window brightness temperature-based
             !   typing
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             wflg = 1

             if (imager_measurements%DATA(i,j,ch5) <= 233.16) then

                imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE
                wflg = 0

             elseif ( ( imager_measurements%DATA(i,j,ch5) > 233.16 ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) <= 253.16 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE

             elseif ( ( imager_measurements%DATA(i,j,ch5) > 253.16 ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) <= 273.16 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = SUPERCOOLED_TYPE

             else

                imager_pavolonis%CLDTYPE(i,j) = WATER_TYPE

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform the EMS 3.75 um test for bulk cloud phase
             !   determination
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             ! solar contamination check disabled
             !if (SOLAR_CONTAMINATION_MASK(i,j) == NO) then

             if ( ( imager_pavolonis%CLDTYPE(i,j) == SUPERCOOLED_TYPE ) .and. &
                  ( imager_pavolonis%emis_ch3b(i,j) >= EMS38_PHASE_THRES ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 263.16 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = OPAQUE_ICE_TYPE

             endif


             if ( ( imager_pavolonis%CLDTYPE(i,j) == OPAQUE_ICE_TYPE ) .and. &
                  ( wflg == 1 ) .and. &
                  ( imager_pavolonis%emis_ch3b(i,j) < EMS38_PHASE_THRES ) ) then

                imager_pavolonis%CLDTYPE(i,j) = SUPERCOOLED_TYPE

             endif

             !endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Nighttime cloud overlap test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( BTD_Ch4_Ch5 > BTD1112_NOVERLAP_THRES_L ) .and. &
                  ( BTD_Ch4_Ch5 < BTD1112_NOVERLAP_THRES_H ) .and. &
                  ( imager_pavolonis%emis_ch3b(i,j) > EMS38_NOVERLAP_THRES_L ) .and. &
                  ( imager_pavolonis%emis_ch3b(i,j) < EMS38_NOVERLAP_THRES_H ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) > 210.0 ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 283.0 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = OVERLAP_TYPE

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for cirrus clouds using the split window test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( imager_pavolonis%CLDTYPE(i,j) /= OVERLAP_TYPE ) .and. &
                  ( BTD_Ch4_Ch5 > BTD1112_CIRRUS_THRES ) .and. &
                  ( imager_pavolonis%emis_ch3b(i,j) > 1.3 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = CIRRUS_TYPE

             endif


             if( ( imager_pavolonis%emis_ch3b(i,j) > 1.6 ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 300.0 ) ) then

                imager_pavolonis%cirrus_quality(i,j) = 1

             endif


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for cirrus clouds using the EMS 3.75 um test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             ! solar contamination check disabled
             !if (SOLAR_CONTAMINATION_MASK(i,j) == NO) then

             if ( ( imager_pavolonis%CLDTYPE(i,j) /= OVERLAP_TYPE ) .and. &
                  ( imager_pavolonis%CLDTYPE(i,j) /= OPAQUE_ICE_TYPE ) .and. &
                  ( imager_pavolonis%emis_ch3b(i,j) > 1.10 ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 300.0 ) ) then

                imager_pavolonis%CLDTYPE(i,j) = CIRRUS_TYPE

             endif

             if ( ( ( imager_pavolonis%emis_ch3b(i,j) > 1.6 )       .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 300.0 ) ) .or. &
                  ( ( imager_pavolonis%emis_ch3b(i,j) > 1.4 )       .and. &
                  ( imager_measurements%DATA(i,j,ch5) < 300.0 )   .and. &
                  ( BTD_Ch4_Ch5 > BTD1112_CIRRUS_THRES ) ) ) then

                imager_pavolonis%cirrus_quality(i,j) = 1

             endif

             !endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for fog - not used over DESERT(=12) surfaces.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) >= 90.0 ) .and. &
                  ( imager_pavolonis%emis_ch3b(i,j) <= 0.90 ) .and. &
                  ( imager_measurements%DATA(i,j,ch5) > 240.0 ) .and. &
                  ( imager_pavolonis%SFCTYPE(i,j) /= DESERT_FLAG ) ) then

                imager_pavolonis%CLDTYPE(i,j) = FOG_TYPE

             endif


             !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             !
             ! end of day/night if loop
          endif
          !
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          ! set cccot to a fake high value as otherwise gets set as clear later on

          if  (imager_pavolonis%CLDTYPE(i,j) == PROB_OPAQUE_ICE_TYPE ) then
             imager_pavolonis%CLDMASK(i,j) = CLOUDY
             imager_pavolonis%cccot_pre(i,j) = .99

          endif


          !end scanline loop
       end do j_loop
       !-------------------------------------------------------------------


       !pixel loop
    end do i_loop
    !$OMP END DO
    !$OMP END PARALLEL
    !---------------------------------------------------------------------



    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !+                                                                   +
    !+                      END OF TYPING SPECTRAL TESTS                 +
    !+                                                                   +
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !+                                                                   +
    !+    Start spatial filter used on cirrus and overlap pixels.        +
    !+    A 2n_box x 2n_box pixel filter is employed.                    +
    !+                                                                   +
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !-- loop over pixels
    !i_loop2: do  i = 1,num_pix
    i_loop2: do  i = imager_geolocation%STARTX, imager_geolocation%ENDX


       !-- loop over scanlines
       !j_loop2: do j = j1,j2
       j_loop2: do j = 1, imager_geolocation%NY

          if (imager_pavolonis%CLDTYPE(i,j) .ne. CIRRUS_TYPE .and. &
               imager_pavolonis%CLDTYPE(i,j) .ne. OVERLAP_TYPE) cycle

          !-- coszen = cosine of the solar zenith angle
          !         coszen = cos(imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) * d2r )
          coszen = cos(imager_angles%SATZEN(i,j,imager_angles%NVIEWS) * d2r )

          !-- determine box for filtering
          !start_line = max(j1,j - n_box)
          !end_line   = min(j2,j + n_box)
          !start_pix  = max(1,i - n_box)
          !end_pix    = min(num_pix,i + n_box)
          start_line = max(1,j - n_box)
          end_line   = min(imager_geolocation%NY,j + n_box)
          start_pix  = max(imager_geolocation%STARTX,i - n_box)
          end_pix    = min(imager_geolocation%ENDX,i + n_box)
          npix       = ((end_line - start_line)+1)*((end_pix - start_pix)+1)


          !-- At least one pixel in the 2n_box x 2n_box array must have a
          !   Bt_Ch4 < 295 K and the average ems 3.75 um must be < 1.2
          !   for low quality cirrus;
          !   otherwise the pixel is reset to water or mixed.

          if ( imager_pavolonis%CLDTYPE(i,j) == CIRRUS_TYPE .and. &
               imager_pavolonis%cirrus_quality(i,j) == 0) then

             !-- account for atmospheric effects
             t4_filter_thresh = 295.0 - 12.0*(1.0-coszen)

             !-- filter pixels in region
             !   added criterion for missing values in ems_Ch20 - akh 1/07
             if ( (minval(imager_measurements%DATA(start_pix:end_pix &
                  ,start_line:end_line,ch5)) > t4_filter_thresh) .or. &
                  ( (sum(imager_pavolonis%emis_ch3b(start_pix:end_pix &
                  ,start_line:end_line))/npix < 1.2) .and. &
                  (minval(imager_pavolonis%emis_ch3b(start_pix:end_pix &
                  ,start_line:end_line)) > 0.0) ) ) then

                if ( imager_measurements%DATA(i,j,ch5) <= 273.16 ) then
                   imager_pavolonis%CLDTYPE(i,j) = SUPERCOOLED_TYPE
                else
                   imager_pavolonis%CLDTYPE(i,j) = WATER_TYPE
                endif

             endif

          endif


          !-- At least one pixel in the 2n x 2n array must have a
          !   Bt_Ch4 < 275 K for overlap;
          !   otherwise the pixel is reset to water or mixed.

          !-- account for atmospheric effects in BT4 threshold for filtering

          if ( imager_pavolonis%CLDTYPE(i,j) == OVERLAP_TYPE .and. &
               imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) > 90.0 ) then

             t4_filter_thresh = 273.0 - 12.0*(1.0-coszen)

             if (minval(imager_measurements%DATA(start_pix:end_pix &
                  ,start_line:end_line,ch5)) > t4_filter_thresh) then

                if (imager_measurements%DATA(i,j,ch5) <= 273.16) then
                   imager_pavolonis%CLDTYPE(i,j) = SUPERCOOLED_TYPE
                else
                   imager_pavolonis%CLDTYPE(i,j) = WATER_TYPE
                endif

             endif

          endif


          !-- end of scanline loop
       end do j_loop2


       !-- end of pixel loop
    end do i_loop2

    deallocate(skint)

    ! =====================================================================
  end subroutine CLOUD_TYPE
  ! =====================================================================


  !
  ! NOT ADAPTED YET for CC4CL!
  !
  ! =====================================================================
  !      Beginning of subroutine CLOUD_RETYPE
  !      This routine modifies the cloud_type array based on spatial
  !      analysis.  Its goal is to reduce the edges of stratus clouds
  !      from being typed as cirrus
  ! =====================================================================

!   subroutine CLOUD_RETYPE(jmin,numj,cld_type_array)

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

!     type(imager_geolocation_s) :: imager_geolocation


!     !-------------------------------------------------------------------
!     ! ---- loop over pixels
!     !i_loop: do i = 1, num_pix
!     i_loop: do i = imager_geolocation%STARTX, imager_geolocation%ENDX


!        !-----------------------------------------------------------------
!        ! ---- loop over scanlines
!        !j_loop: do j = jmin, numj + jmin - 1
!        j_loop: do j = imager_geolocation%STARTY, imager_geolocation%ENDY


!           ilrc = i_lrc(i,j)
!           jlrc = j_lrc(i,j)


!           ! check for ice-phase pixels where the local radiative center
!           ! is a water cloud  - retype these as water

!           if ((cld_type_array(i,j) == CIRRUS_TYPE)  .or. &
!                (cld_type_array(i,j) == OVERLAP_TYPE)) then

!              !ilrc = i_min_Bt_Ch31_3x3(i,j)
!              !jlrc = j_min_Bt_Ch31_3x3(i,j)
!              ilrc = i_lrc(i,j)
!              jlrc = j_lrc(i,j)

!              ! skip this if no lrc is available
!              if ( ilrc < 1 .or. jlrc < 1 ) then
!                 cycle
!              endif

!              if ((cld_type_array(ilrc,jlrc) == FOG_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == WATER_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == SUPERCOOLED_TYPE)) then

!                 cld_type_array(i,j) = cld_type_array(ilrc,jlrc)

!              endif

!           endif



!           ! check for water clouds on the edge of cirrus

!           if ((cld_type_array(i,j) == FOG_TYPE)  .or. &
!                (cld_type_array(i,j) == WATER_TYPE)) then

!              ilrc = i_lrc(i,j)
!              jlrc = j_lrc(i,j)

!              if (ilrc < 1 .or. jlrc < 1) then
!                 cycle
!              endif

!              if ((cld_type_array(ilrc,jlrc) == CIRRUS_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == OVERLAP_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == OPAQUE_ICE_TYPE)) then

!                 cld_type_array(i,j) = CIRRUS_TYPE

!              endif

!           endif


!        end do j_loop
!        !-----------------------------------------------------------------


!     end do i_loop
!     !-------------------------------------------------------------------


  ! =====================================================================
!   end subroutine CLOUD_RETYPE
  ! =====================================================================


  function PlanckInv( input_platform, T )

    use COMMON_CONSTANTS

    implicit none

    ! input variable
    character(len=platform_length), intent(in)   :: input_platform
    real(kind=sreal),intent(in) :: T ! Kelvin

    ! return variable
    real(kind=sreal), dimension(2) :: PlanckInv !out

    ! local variables
    integer(kind=byte) :: index ! index of row containing platform-specific coefficients
    real(kind=sreal),parameter :: Planck_C1 = 1.19104E-5 ! 2hc^2 in mW m-2 sr-1 (cm-1)-4
    real(kind=sreal),parameter :: Planck_C2 = 1.43877 ! hc/k  in K (cm-1)-1
    real(kind=sreal), dimension(4,17) :: coefficients ! coefficients containing variables

    ! select approproate row of coefficient values
    select case (input_platform)
    case ("noaa7")
       index = 1
    case ("noaa9")
       index = 2
    case ("noaa11")
       index = 3
    case ("noaa12")
       index = 4
    case ("noaa14")
       index = 5
    case ("noaa15")
       index = 6
    case ("noaa16")
       index = 7
    case ("noaa17")
       index = 8
    case ("noaa18")
       index = 9
    case ("noaa19")
       index = 10
    case ("metop02")
       index = 11
    case ("metopa")
       index = 12
    case ("TERRA")
       index = 13
    case ("AQUA")
       index = 14
    case ("Envisat")
       index = 15
    case ("ERS2")
       index = 15
    case ("MSG1", "MSG2", "MSG3", "MSG4")
       index = 16
    case ("default")
       index = 17
    case default
       write(*,*) "Error: Platform name does not match local string in function PlanckInv"
       write(*,*) "Input platform name = ", input_platform
       stop
    end select

    !   v: wave number (cm-1)
    !   a: alpha parameter
    !   b: beta parameter
    !   solcon: solar constant

    ! Conversion from SADChan Planck coefficients to the ones here:
    ! v = (B1 / Planck_C1)^(1/3) = B2 / Planck_C2
    ! a = T2
    ! b = T1
    coefficients = reshape( (/ &
    !        v          a         b     solcon
         2684.523,  0.997083,  1.94314, 5.061, & !noaa07
         2690.045,  0.997111,  1.87782, 5.081, & !noaa09
         2680.050,  0.996657,  1.73316, 5.077, & !noaa11
         2651.771,  0.996999,  1.89956, 4.948, & !noaa12
         2654.250,  0.996176,  1.87812, 4.973, & !noaa14
         2695.974,  0.998015,  1.62126, 5.088, & !noaa15
         2681.254,  0.998271,  1.67456, 5.033, & !noaa16
         2669.141,  0.997335,  1.69576, 5.008, & !noaa17
         2660.647,  0.997145,  1.71735, 4.981, & !noaa18
         2670.242,  0.997411,  1.68202, 5.010, & !noaa19
         2687.039,  0.996570,  2.05823, 5.077, & !metop02
         2687.039,  0.996570,  2.05823, 5.077, & !metopa
         2641.775,  0.999341,  0.47705, 4.804, & !terra
         2647.409,  0.999336,  0.48184, 4.822, & !aqua
         2675.166,  0.996344,  1.72695, 5.030, & !env (aatsr)
         2568.832,  0.995400,  3.43800, 4.660, & !msg1, msg2
         2670.000,  0.998000,  1.75000, 5.000  & !default
         /), (/ 4, 17 /) )

    PlanckInv(1) = Planck_C1 * coefficients( 1 , index )**3 / &
         ( exp( Planck_C2 * coefficients( 1 , index ) / &
         ( coefficients( 2 , index ) * T &
         + coefficients( 3 , index ) ) ) - 1. )
    PlanckInv(2) = coefficients( 4 , index )

  end function PlanckInv


!***********************************************************************
end module CLOUD_TYPING_PAVOLONIS
!***********************************************************************
! End of module CLOUD_TYPING_PAVOLONIS

