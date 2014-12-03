! NAME: cloud_typing_pavolonis.f90 (src)
!       CLOUD_TYPING_PAVOLONIS (module)
!
! Purpose:
!       Algorithm to determine cloud type as a function of satellite
!        radiances and land surface cover.
!
! History: 
!       23rd Oct 2014, CS: Original version.
!        4th Nov 2014, OS: ecmwf structure containing skin temperature now passed
!                          as argument; bilinear interpolation of skin temperature
!                          on orbit grid; added snow/ice and skin temperature to
!                          NN cloud mask call arguments
!       20th Nov 2014, OS: BTD_Ch4_Ch3b is now calculated within NN cloud mask; flag
!                          ch3a_on_avhrr_flag is passed as an argument to NN call
!        1st Dec 2014, OS: cloud type now set to PROB_OPAQUE_ICE_TYPE for twilight/night
!                          extra-polar regions if Ch3b is missing; significant bug fix:
!                          wrong ch3b emissivity and reflectance were used in old code, 
!                          which are now correctly calculated and applied
!        3rd Dec 2014, OS: use default coefficients for AATSR until these will be calculated
!                          and implemented here; skip pixel if input SOLZEN is negative;
!                          pass SATZEN as argument to NN cloud mask
!        3rd Dec 2014, GM: Added Planck coefficients for AATSR derived from
!                          SADChan Planck coefficients.
!        3rd Dec 2014, OS: changed nrows of coefficients and reshape command to include
!                          new AATSR coefficients
!
! Bugs:
!    None known
!
! Subroutines included in module: 
!	CLOUD_TYPE
!	CLOUD_RETYPE
!
! DEPENDENCIES:
!   COMMON_CONSTANTS
!   IMAGER_STRUCTURES
!   SURFACE_STRUCTURES
!   NEURAL_NET_PREPROC
!   CONSTANTS_CLOUD_TYPING_PAVOLONIS
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!$Id$
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
!	CLOUD_TYPE
!	CLOUD_RETYPE
!
!-----------------------------------------------------------------------


! Begin of module
!***********************************************************************
module CLOUD_TYPING_PAVOLONIS
  !***********************************************************************

  implicit none

  private

  public :: CLOUD_TYPE !, CLOUD_RETYPE

  integer, parameter, private:: n_box=10

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
  !    INTEGER(kind=sint) :: PROB_CLEAR_TYPE = 1 !currently not used
  !    INTEGER(kind=sint) :: FOG_TYPE = 2
  !    INTEGER(kind=sint) :: WATER_TYPE = 3
  !    INTEGER(kind=sint) :: SUPERCOOLED_TYPE = 4
  !    INTEGER(kind=sint) :: OPAQUE_ICE_TYPE = 6
  !    INTEGER(kind=sint) :: CIRRUS_TYPE = 7
  !    INTEGER(kind=sint) :: OVERLAP_TYPE = 8
  !
  !
  ! INPUTS:
  !  j1 - the first scan index to process
  !  j2 - the last scan index to process
  !
  ! OUTPUTS:
  !  
  ! CALLING SEQUENCE:  call CLOUD_TYPE( j_min, num_scans_read )
  !		      (called from ORAC - CC4CL)
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
  ! NIR_PHASE_THRES		= 1.6 micron phase threshold
  ! NIR_CIRRUS_THRES		= 1.6 micron cirrus threshold
  ! NIR_OVER_THRES		= Minimum 1.6 um reflectance allowed 
  !                                for cloud overlap over SNOW/ICE.
  ! BTD3811_PHASE_THRES		= 3.75um - 11um thresholds used for phase determination.
  ! EMS38_PHASE_THRES		= 3.75 um thresholds used for phase determination.
  ! BTD1112_DOVERLAP_THRES	= 11um - 12um cloud overlap threshold
  ! BTD1112_CIRRUS_THRES		= 11um - 12um cirrus threshold
  ! BTD1112_NOVERLAP_THRES_L	= Split window nighttime low cloud overlap threshold
  ! BTD1112_NOVERLAP_THRES_H	= Split window nighttime high cloud overlap threshold
  ! EMS38_NOVERLAP_THRES_L	= EMS38 nighttime low cloud overlap threshold
  ! EMS38_NOVERLAP_THRES_H	= EMS38 nighttime high cloud overlap threshold
  ! MIN_BTD1112_DOVERLAP		= The minimum 11um - 12um BTD allowed
  !  for overlap detection. 
  ! MIN_BTD1112_NOVERLAP		= The minimum allowed Bt_Ch31 -
  !  Bt_Ch32 allowed for nighttime overlap 
  ! A1 = Coefficient needed to determine the 11um - 12um BTD for cirrus detection
  ! B1 = Coefficient needed to determine the 11um -
  !  12um BTD for cirrus detection
  ! C1				= Coefficient needed to determine the 11um -
  !  12um BTD for cirrus detection
  ! D1				= Coefficient needed to determine the 11um -
  !  12um BTD for cirrus detection
  ! E1				= Coefficient needed to determine the 11um -
  !  12um BTD for cirrus detection
  ! A2				= Coefficient needed to determine the 3.75um - 11um BTD thresholds that
  !				  differentiate between ice and water as a function of 11um - 12um BTD.
  ! B2				= Coefficient needed to determine the 3.75um - 11um BTD thresholds that
  !				  differentiate between ice and water as a function of 11um - 12um BTD.
  ! C2				= Coefficient needed to determine the 3.75um - 11um BTD thresholds that
  !				  differentiate between ice and water as a function of 11um - 12um BTD.
  ! D2				= Coefficient needed to determine the 3.75um - 11um BTD thresholds that
  !				  differentiate between ice and water as a function of 11um - 12um BTD.
  ! E2				= Coefficient needed to determine the 3.75um - 11um BTD thresholds that
  !				  differentiate between ice and water as a function of 11um - 12um BTD.
  ! A3				= Coefficient needed to determine the 11um - 12um BTD used to find cloud
  !				  overlap as a function of 0.65 um reflectance.
  ! B3				= Coefficient needed to determine the 11um - 12um BTD used to find cloud
  !				  overlap as a function of 0.65 um reflectance.
  ! C3				= Coefficient needed to determine the 11um - 12um BTD used to find cloud
  !				  overlap as a function of 0.65 um reflectance.
  ! D3				= Coefficient needed to determine the 11um - 12um BTD used to find cloud
  !				  overlap as a function of 0.65 um reflectance.
  ! E3				= Coefficient needed to determine the 11um - 12um BTD used to find cloud
  !				  overlap as a function of 0.65 um reflectance.
  !
  ! i				= pixel counter
  ! j				= scanline counter
  ! j1				= start scanline
  ! j2				= ending scanline
  ! index1			= viewing zenith angle bin
  ! index2			= solar zenith angle bin
  ! wflg				= IR window flag
  ! start_line			= line to start filtering 
  ! end_line			= line to end filtering
  ! start_pix			= pixel to start filtering
  ! end_pix			= pixel to end filtering
  ! npix				= number of pixels to filer
  !
  ! day				= day/night flag
  !
  ! t4_filter_thresh		= BT4 threshold, accounting for atmospheric effects, for filtering
  ! nir_ref			= channel 3a/b reflectance
  !
  ! Not used: A4, B4, C4, D4, E4, n
  !
  !----------------------------------------------------------------------


  ! =====================================================================
  subroutine CLOUD_TYPE(surface, imager_flags, imager_angles,&
       & imager_geolocation, imager_measurements, imager_pavolonis, &
       & ecmwf, platform, doy, verbose)
    ! =====================================================================

    !-- load necessary variable fields and constants
    use COMMON_CONSTANTS
    use IMAGER_STRUCTURES
    use SURFACE_STRUCTURES
    use NEURAL_NET_PREPROC
    use CONSTANTS_CLOUD_TYPING_PAVOLONIS
    use interpol
    use ecmwf_m, only : ecmwf_s


    !-- parameters to be passed

    type(surface_s), intent(in)                  :: surface
    type(imager_flags_s), intent(in)             :: imager_flags
    type(imager_angles_s), intent(in)            :: imager_angles
    type(imager_geolocation_s), intent(in)       :: imager_geolocation
    type(imager_measurements_s), intent(in)      :: imager_measurements
    type(imager_pavolonis_s), intent(inout)      :: imager_pavolonis
    type(ecmwf_s),               intent(in)      :: ecmwf
    character(len=platform_length), intent(in)   :: platform
    integer(kind=sint), intent(in)               :: doy
    logical,                     intent(in)      :: verbose


    !-- Declare some variables to hold various thresholds.

    real (kind=sreal):: NIR_PHASE_THRES, NIR_CIRRUS_THRES, NIR_OVER_THRES, &
         & BTD3811_PHASE_THRES, EMS38_PHASE_THRES,            &
         & BTD1112_DOVERLAP_THRES, BTD1112_CIRRUS_THRES,      &
         & BTD1112_NOVERLAP_THRES_L, BTD1112_NOVERLAP_THRES_H,&
         & EMS38_NOVERLAP_THRES_L, EMS38_NOVERLAP_THRES_H

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
         & start_line, end_line, start_pix, end_pix, npix
    logical :: day
    real    :: t4_filter_thresh, nir_ref
    real(kind=sreal),allocatable,dimension(:,:) :: skint
    type(interpol_s), allocatable, dimension(:) :: interp

    ! --------------------------------------------------------------------
    !
    !---- CC4CL requirements and adaptions for Pavolonis alg.

    integer(kind=sint) :: ch3a_on_avhrr_flag
    real(kind=sreal)   :: glint_angle, coszen
    real(kind=sreal)   :: BTD_Ch3b_Ch4
    real(kind=sreal)   :: BTD_Ch4_Ch5
    real(kind=sreal)   :: rad_ch3b
    real(kind=sreal)   :: rad_ch3b_emis
    real(kind=sreal)   :: ref_ch3b
    real(kind=sreal)   :: solcon_ch3b
    real(kind=sreal)   :: mu0
    real(kind=sreal)   :: esd
    real(kind=sreal)   :: c_sun
    real(kind=sreal),dimension(2)   :: PlanckInv_out

    ! -- Parameters used here
    !
    ! cirrus_quality 	= quality of cirrus flag
    ! coszen	   	= cosine of the solar zenith angle
    ! ch3a_on_avhrr_flag	= whether or not AVHRR channel 3a is 
    !                       used (sym%NO, sym%YES, sym%INEXISTENT)
    ! BTD                 = Brightness Temperature Difference
    ! BTD_Ch3b_Ch4        = BT Ch3b minus BT Ch4
    ! BTD_Ch4_Ch5         = BT Ch4 minus BT Ch5
    ! BTD_Ch4_Ch3b        = BT Ch4 minus BT Ch3b
    !
    !
    ! -- INPUT  
    !                                   wavelength  MODIS=CC4CL=AVHRR
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
    ! imager_pavolonis%SUNGLINT_MASK ... sunglint mask: sym%YES, sym%NO
    ! imager_pavolonis%SFCTYPE ... surface type - NISE corrected LUS
    ! imager_pavolonis%CLDTYPE ... cloud type based on pavolonis
    ! imager_pavolonis%CLDMASK ... cloud mask based on L1c thresholding
    ! imager_pavolonis%CCCOT   ... cloud cover COTs
    !
    !---------------------------------------------------------------------

    allocate(skint(imager_geolocation%startx:imager_geolocation%endx, &
         & 1:imager_geolocation%ny))
    skint=sreal_fill_value
    allocate(interp(1))

    write(*,*) "starting bilinear interpolation of skint on orbit grid"

    do i=1,imager_geolocation%ny
       do j=imager_geolocation%startx,imager_geolocation%endx

          call bilinear_coef(ecmwf%lon, ecmwf%xdim, ecmwf%lat, &
               ecmwf%ydim, imager_geolocation%longitude(j,i), &
               imager_geolocation%latitude(j,i), interp(1))

          call interp_field (ecmwf%skin_temp, skint(j,i), interp(1))

       end do
    end do

    write(*,*) "interpolation finished"

    deallocate(interp)
    !-- copy land use flag array to Surface TYPE array
    imager_pavolonis%SFCTYPE = imager_flags%LUSFLAG

    !-- correction of SFCTYPE with NISE aux. data
    where (surface%NISE_MASK .eq. sym%YES)
       imager_pavolonis%SFCTYPE = sym%NISE_FLAG
    endwhere

    !-- initialize cloud mask as cloudy
    !where (imager_pavolonis%CLDMASK .eq. byte_fill_value)
    !imager_pavolonis%CLDMASK = sym%CLOUDY
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

          if ( imager_measurements%DATA(i,j,3) .ge. 0 .and. &
               & imager_measurements%DATA(i,j,4) .lt. 0) then

             ! Ch3a is used if Ch3b is not avail.
             ch3a_on_avhrr_flag = sym%YES 

          elseif ( imager_measurements%DATA(i,j,4) .ge. 0 ) then

             ! Ch3b is used if avail.
             ch3a_on_avhrr_flag = sym%NO 

          else

             ! neither Ch3a nor Ch3b avail.
             ch3a_on_avhrr_flag = sym%INEXISTENT 

          endif

          !-- check for sunglint and save result: 
          !   imager_pavolonis%SUNGLINT_MASK(i,j)

          !In PATMOS sunglint calculation:
          !glint_angle = cos ( sol_zen * d2r ) * cos ( sen_zen * d2r ) + &
          !            & sin ( sol_zen * d2r ) * sin ( sen_zen * d2r ) * &
          !            & cos ( rel_azi * d2r )
          !glint_angle = (-1.0 > ( glint_angle < 1.0 ))
          !glint_angle = acos(glint_angle) / d2r
          !und dann ist sun glint dort wo glint angle lt. 40 grad.

          if ( imager_pavolonis%SFCTYPE(i,j) .eq. sym%WATER_FLAG) then
             glint_angle = &
                  & cos ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) * d2r ) * &
                  & cos ( imager_angles%SATZEN(i,j,imager_angles%NVIEWS) * d2r ) + &
                  & sin ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) * d2r ) * &
                  & sin ( imager_angles%SATZEN(i,j,imager_angles%NVIEWS) * d2r ) * &
                  & cos ((imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) - &
                  &       imager_angles%SATZEN(i,j,imager_angles%NVIEWS)) * d2r )

             glint_angle = max( -1.0, min( glint_angle, 1.0 ) ) 
             glint_angle = acos(glint_angle) / d2r

             if ( glint_angle .lt. 40.0 ) then
                imager_pavolonis%SUNGLINT_MASK(i,j) = sym%YES
             else
                imager_pavolonis%SUNGLINT_MASK(i,j) = sym%NO
             endif

          else
             ! other surface types, e.g. land
             imager_pavolonis%SUNGLINT_MASK(i,j) = sym%NO

          endif

          !-- calculate BT differences

          ! BT(11) minus BT(12) 
          BTD_Ch4_Ch5 = imager_measurements%DATA(i,j,5) - &
               & imager_measurements%DATA(i,j,6)

          ! BT(3.75) minus BT(11) 
          BTD_Ch3b_Ch4 = imager_measurements%DATA(i,j,4) - &
               & imager_measurements%DATA(i,j,5)

          !-- NEURAL_NET_PREPROC subroutine
          call ann_cloud_mask( &
               & imager_measurements%DATA(i,j,1), &
               & imager_measurements%DATA(i,j,2), &
               & imager_measurements%DATA(i,j,4), &
               & imager_measurements%DATA(i,j,5), &
               & imager_measurements%DATA(i,j,6), &
               & imager_angles%SOLZEN(i,j,imager_angles%NVIEWS), &
               & imager_angles%SATZEN(i,j,imager_angles%NVIEWS), &
               & imager_geolocation%DEM(i,j), &
               & surface%NISE_MASK(i,j), imager_flags%LSFLAG(i,j), &
               & imager_flags%LUSFLAG(i,j), &
               & imager_pavolonis%SFCTYPE(i,j), &
               & imager_pavolonis%CCCOT_pre(i,j), &
               & imager_pavolonis%CLDMASK(i,j) , &
               & imager_geolocation%LATITUDE(i,j) , &
               & skint(i,j) , &
               & ch3a_on_avhrr_flag, &
               & i, j, &
               & verbose )

          !-- First Pavolonis test: clear or cloudy

          if ( imager_pavolonis%CLDMASK(i,j) == sym%CLEAR ) then
             imager_pavolonis%CLDTYPE(i,j) = sym%CLEAR_TYPE
             cycle
          endif

          !-- Check if daytime or nighttime algorithm is to be used.

          day = .FALSE.

          if (imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) < 88.0) then
             day = .TRUE.
          endif

          !-- neither ch3a nor ch3b available

          !-- at night, assign probably opaque ice flag
          !-- as ch3.7 has fill value due to low S/N
          if ( ch3a_on_avhrr_flag == -1 ) then
             if ( ( imager_geolocation%LATITUDE(i,j) < 65.0 .and. &
                  & imager_geolocation%LATITUDE(i,j) > -65.0 ) .and. &
                  & day .eqv. .FALSE. ) & 
                  & imager_pavolonis%CLDTYPE(i,j) = sym%PROB_OPAQUE_ICE_TYPE
             cycle
          endif

          ! calculate ch3b emissivity and reflectance
          PlanckInv_out  = PlanckInv( platform, imager_measurements%DATA(i,j,4) )
          rad_ch3b       = PlanckInv_out(1)
          solcon_ch3b    = PlanckInv_out(2)
          PlanckInv_out  = PlanckInv( platform, imager_measurements%DATA(i,j,5) )
          rad_ch3b_emis  = PlanckInv_out(1)
          mu0 = cos ( imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) * d2r ) 
          esd = 1.0 - 0.0167 * cos( 2.0 * pi * ( doy - 3 ) / 365.0 )
          c_sun = 1. / esd**2
          imager_pavolonis%emis_ch3b(i,j) = rad_ch3b / rad_ch3b_emis
          ref_ch3b = ( rad_ch3b - rad_ch3b_emis ) / &
               ( solcon_ch3b * c_sun * mu0 - rad_ch3b_emis )
         
          !-- nir_ref = channel 3a or channel 3b reflectance

          nir_ref = sreal_fill_value



          !-- Determine the viewing zenith angle bin.

          index1 = min(7,max(1,int(imager_angles%SATZEN(i,j,imager_angles &
               & %NVIEWS)/10.0) + 1))




          !-- Determine the solar zenith angle bin.
 
          index2 = min(8,max(1,int(imager_angles%SOLZEN(i,j,imager_angles &
               & %NVIEWS)/10.0) + 1))




          !-- Set 11um - 12um cirrus thresholds.
          !   Absorption of radiation by water vapor and ice crystals in
          !   semitransparent cirrus clouds is greater at 12 than 11 micron

          BTD1112_CIRRUS_THRES = &
               & A1(index1) + &
               & B1(index1)*imager_measurements%DATA(i,j,5) + &
               & C1(index1)*imager_measurements%DATA(i,j,5)**2 + &
               & D1(index1)*imager_measurements%DATA(i,j,5)**3 + &
               & E1(index1)*imager_measurements%DATA(i,j,5)**4


          BTD1112_CIRRUS_THRES = max( 1.0, min(4.0,BTD1112_CIRRUS_THRES) )


          !-- initial cirrus quality

          imager_pavolonis%cirrus_quality(i,j) = 0 



          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          !-- If DAYTIME, use daytime algorithm. 
          !
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          if ( (imager_pavolonis%CLDMASK(i,j) == sym%CLOUDY) .and. &
               & (day .eqv. .TRUE.) ) then

             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Set 11um - 12um overlap thresholds.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             !if ( (imager_measurements%DATA(i,j,1) >= 35.0) .and. &
             !   & (imager_measurements%DATA(i,j,1) <= 60.0) ) then
             if ( (imager_measurements%DATA(i,j,1) >= 0.35) .and. &
                  & (imager_measurements%DATA(i,j,1) <= 0.60) ) then

                BTD1112_DOVERLAP_THRES = max( &
                     & ( ( A3(index1,index2)+ &
                                !&     B3(index1,index2)* imager_measurements%DATA(i,j,1)*0.01 + &
                                !&     C3(index1,index2)*(imager_measurements%DATA(i,j,1)*0.01)**2 + &
                                !&     D3(index1,index2)*(imager_measurements%DATA(i,j,1)*0.01)**3 + &
                                !&     E3(index1,index2)*(imager_measurements%DATA(i,j,1)*0.01)**4 ) - 0.1 ), &
                     &     B3(index1,index2)* imager_measurements%DATA(i,j,1) + &
                     &     C3(index1,index2)*(imager_measurements%DATA(i,j,1))**2 + &
                     &     D3(index1,index2)*(imager_measurements%DATA(i,j,1))**3 + &
                     &     E3(index1,index2)*(imager_measurements%DATA(i,j,1))**4 ) - 0.1 ), &
                     & MIN_BTD1112_DOVERLAP(index1,index2) - 0.1 )

                !elseif ( imager_measurements%DATA(i,j,1) > 60.0 .and. &
                !       & imager_measurements%DATA(i,j,1) < 90.0 ) then
             elseif ( imager_measurements%DATA(i,j,1) > 0.60 .and. &
                  & imager_measurements%DATA(i,j,1) < 0.90 ) then

                BTD1112_DOVERLAP_THRES = MIN_BTD1112_DOVERLAP(index1,index2) - 0.1

             else

                BTD1112_DOVERLAP_THRES = 9999.0

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- In the high latitudes, the Ch3b 3.75 um must be
             !   less than 20% to prevent single layer water clouds 
             !   from being typed as overlap
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( imager_geolocation%LATITUDE(i,j) > 65.0 .or. &
                  & imager_geolocation%LATITUDE(i,j) < -65.0 ) .and. &
                  & ref_ch3b > 20.0 ) then

                BTD1112_DOVERLAP_THRES = 9999.0

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform initial IR window brightness temperature-based typing
             !   wflg = IR window flag
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             wflg = 1

             if ( imager_measurements%DATA(i,j,5) <= 233.16 ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OPAQUE_ICE_TYPE
                wflg = 0

             elseif ( (imager_measurements%DATA(i,j,5) > 233.16) .and. &
                  & (imager_measurements%DATA(i,j,5) <= 253.16) ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OPAQUE_ICE_TYPE

             elseif ( (imager_measurements%DATA(i,j,5) > 253.16) .and. &
                  & (imager_measurements%DATA(i,j,5) <= 273.16)) then

                imager_pavolonis%CLDTYPE(i,j) =sym%SUPERCOOLED_TYPE

             else

                ! BT(11) > 273.16 K, the melting point of pure water, 
                ! the cirrus detection test is simply applied.
                ! If it is passed, then the pixel is classified as nonopaque 
                ! ice cloud, otherwise, it is a warm liquid water cld type.
                ! [Pavolonis et al. (2005)]

                imager_pavolonis%CLDTYPE(i,j) = sym%WATER_TYPE

             endif



             !---------------------------------------------!
             !                                             !
             !---- Use 1.6 um algorithm, if available ---- !
             !                                             !
             !---------------------------------------------!


             if ( (ch3a_on_avhrr_flag == sym%YES) .and. &
                  & (imager_measurements%DATA(i,j,1) > 0.0) ) then 


                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                ! Set some 1.6 um thresholds used in phase identification 
                ! and cirrus detection.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                if ( (imager_pavolonis%SFCTYPE(i,j) == sym%WATER_FLAG) .or. &
                     & (imager_pavolonis%SFCTYPE(i,j) == sym%NISE_FLAG) ) then

                   !NIR_CIRRUS_THRES = 20.0
                   !NIR_PHASE_THRES  = 17.0
                   NIR_CIRRUS_THRES = 0.20
                   NIR_PHASE_THRES  = 0.17

                elseif (imager_pavolonis%SFCTYPE(i,j) == sym%DESERT_FLAG) then

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

                if (imager_pavolonis%SFCTYPE(i,j) == sym%NISE_FLAG) then
                   !NIR_OVER_THRES = 17.0
                   NIR_OVER_THRES = 0.17
                else
                   NIR_OVER_THRES = 0.0
                endif



                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                !-- The reflectance used in the typing tests to 1.65 um 
                !   when Ch3a is on
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                nir_ref = imager_measurements%DATA(i,j,3)



                !---------------------------------------!
                !                                       !
                !----      Use 3.75 um channel      ----!
                ! (used only if 1.6um is not available) !
                !                                       !
                !---------------------------------------!

             elseif ( (ch3a_on_avhrr_flag == sym%NO) .and. &
                  & (imager_measurements%DATA(i,j,1) > 0.0) ) then


                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                ! Set some 3.75 um thresholds used in phase identification 
                ! and cirrus detection.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                if ( (imager_pavolonis%SFCTYPE(i,j) == sym%WATER_FLAG) .or. &
                     & (imager_pavolonis%SFCTYPE(i,j) == sym%NISE_FLAG) ) then

                   NIR_CIRRUS_THRES = 12.0
                   NIR_PHASE_THRES  = 6.0

                elseif (imager_pavolonis%SFCTYPE(i,j) == sym%DESERT_FLAG) then

                   NIR_CIRRUS_THRES = 40.0
                   NIR_PHASE_THRES  = 6.0

                   ! all other surface types   
                else

                   NIR_CIRRUS_THRES = 12.0
                   NIR_PHASE_THRES  = 6.0

                endif



                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                !-- Set the minimum 3.75 um reflectance allowed for 
                !   cloud overlap over SNOW/ICE.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                if (imager_pavolonis%SFCTYPE(i,j) == sym%NISE_FLAG) then
                   NIR_OVER_THRES = 6.0
                else
                   NIR_OVER_THRES = 0.0
                endif



                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                !-- The reflectance used in the typing tests to 3.75 um 
                !   when Ch3b is on.
                !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                nir_ref = ref_ch3b 


                ! ----------------------------------------- !
                !                                           !
                ! end of if loop: CH3a or CH3b availability !
                !                                           !
                ! ----------------------------------------- !
             endif




             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform the NIR reflectance bulk cloud phase test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( imager_pavolonis%CLDTYPE(i,j) == sym%SUPERCOOLED_TYPE .and. &
                  & nir_ref <= NIR_PHASE_THRES .and. &
                  & imager_measurements%DATA(i,j,5) < 263.16 ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OPAQUE_ICE_TYPE

             endif


             if ( imager_pavolonis%CLDTYPE(i,j) == sym%OPAQUE_ICE_TYPE .and. &
                  & wflg == 1 .and. &
                  & nir_ref > NIR_PHASE_THRES ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%SUPERCOOLED_TYPE

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform the cloud overlap test 
             !   !! not used over DESERT surfaces !!
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( ( BTD_Ch4_Ch5 > BTD1112_DOVERLAP_THRES ) .and. &
                  & (imager_measurements%DATA(i,j,5) < 270.0) .and. & 
                  & imager_pavolonis%SFCTYPE(i,j) /= sym%DESERT_FLAG .and. &
                  & nir_ref > NIR_OVER_THRES .and. &
                  & imager_measurements%DATA(i,j,5) > 210.0 ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OVERLAP_TYPE

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for cirrus clouds.
             !   note, akh modified so that nir_ref test 
             !   only applied when Solzen < 70
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( (imager_pavolonis%CLDTYPE(i,j) /= sym%OVERLAP_TYPE) .and. &
                  & (BTD_Ch4_Ch5 > (BTD1112_CIRRUS_THRES-0.2)) .and. &
                  & imager_measurements%DATA(i,j,5) < 295.0 ) then

                if (imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) < 70.0) then

                   if ( nir_ref < NIR_CIRRUS_THRES ) then
                      imager_pavolonis%CLDTYPE(i,j) = sym%CIRRUS_TYPE
                      imager_pavolonis%cirrus_quality(i,j) = 1
                   endif

                else

                   imager_pavolonis%CLDTYPE(i,j) = sym%CIRRUS_TYPE
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

             if ( ch3a_on_avhrr_flag == sym%NO .and. &
                  & ref_ch3b >= 25.0 .and. &
                  & imager_pavolonis%SFCTYPE(i,j) /= sym%DESERT_FLAG .and. &
                  & imager_measurements%DATA(i,j,5) > 240.0 .and. &
                  & ref_ch3b / imager_measurements%DATA(i,j,1) < 0.006 ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%FOG_TYPE

             endif


             !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             !
             !-- If nighttime, use tri-spectral nighttime algorithm.
             !
             !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          elseif( (imager_pavolonis%CLDMASK(i,j) == sym%CLOUDY) .and. &
               & (day .eqv. .false.) ) then


             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Set 3.75um - 11um thresholds used for phase determination.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             BTD3811_PHASE_THRES = A2(index1) + &
                  & B2(index1)*BTD_Ch4_Ch5    + &
                  & C2(index1)*BTD_Ch4_Ch5**2 + &
                  & D2(index1)*BTD_Ch4_Ch5**3 + &
                  & E2(index1)*BTD_Ch4_Ch5**4

             BTD3811_PHASE_THRES = min(8.0,max(-2.0,BTD3811_PHASE_THRES))



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Set the 3.75 um thresholds used for phase determination.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if (imager_measurements%DATA(i,j,5) <= 245.0) then

                if (imager_pavolonis%SFCTYPE(i,j) == sym%WATER_FLAG) then
                   EMS38_PHASE_THRES = 0.9
                else
                   EMS38_PHASE_THRES = 0.9
                endif

             else ! BT11 > 245 (Ch4 BT: 11 micron)

                if (imager_pavolonis%SFCTYPE(i,j) == sym%WATER_FLAG) then
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

                if (imager_pavolonis%SFCTYPE(i,j) == sym%WATER_FLAG) then

                   if (imager_geolocation%LATITUDE(i,j) > -30.0 .and. &
                        & imager_geolocation%LATITUDE(i,j) < 30.0) then

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

                   if (imager_geolocation%LATITUDE(i,j) > -30.0 .and. &
                        & imager_geolocation%LATITUDE(i,j) < 30.0) then

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

             if ( imager_pavolonis%SFCTYPE(i,j) == sym%DESERT_FLAG ) then

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

             if (imager_measurements%DATA(i,j,5) <= 233.16) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OPAQUE_ICE_TYPE
                wflg = 0

             elseif ( (imager_measurements%DATA(i,j,5) > 233.16) .and. &
                  & (imager_measurements%DATA(i,j,5) <= 253.16)) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OPAQUE_ICE_TYPE

             elseif ( (imager_measurements%DATA(i,j,5) > 253.16) .and. &
                  & (imager_measurements%DATA(i,j,5) <= 273.16)) then

                imager_pavolonis%CLDTYPE(i,j) = sym%SUPERCOOLED_TYPE

             else

                imager_pavolonis%CLDTYPE(i,j) = sym%WATER_TYPE

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Perform the EMS 3.75 um test for bulk cloud phase 
             !   determination
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             ! solar contamination check temporalily disabled                              
             !if (SOLAR_CONTAMINATION_MASK(i,j) == sym%NO) then

             if ( (imager_pavolonis%CLDTYPE(i,j) == sym%SUPERCOOLED_TYPE) .and. &
                  & (imager_pavolonis%emis_ch3b(i,j) >= EMS38_PHASE_THRES) .and. &
                  & (imager_measurements%DATA(i,j,5) < 263.16) ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OPAQUE_ICE_TYPE

             endif


             if ( (imager_pavolonis%CLDTYPE(i,j) == sym%OPAQUE_ICE_TYPE) .and. &
                  & (wflg == 1) .and. &
                  & (imager_pavolonis%emis_ch3b(i,j) < EMS38_PHASE_THRES) ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%SUPERCOOLED_TYPE

             endif

             !endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Nighttime cloud overlap test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( BTD_Ch4_Ch5 > BTD1112_NOVERLAP_THRES_L .and. &
                  & BTD_Ch4_Ch5 < BTD1112_NOVERLAP_THRES_H .and. & 
                  & imager_pavolonis%emis_ch3b(i,j) > EMS38_NOVERLAP_THRES_L .and. &
                  & imager_pavolonis%emis_ch3b(i,j) < EMS38_NOVERLAP_THRES_H .and. &
                  & imager_measurements%DATA(i,j,5) > 210.0 .and. &
                  & imager_measurements%DATA(i,j,5) < 283.0 ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%OVERLAP_TYPE

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for cirrus clouds using the split window test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( (imager_pavolonis%CLDTYPE(i,j) /= sym%OVERLAP_TYPE) .and. &
                  & (BTD_Ch4_Ch5 > BTD1112_CIRRUS_THRES) .and. &
                  & (imager_pavolonis%emis_ch3b(i,j) > 1.3) ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%CIRRUS_TYPE

             endif


             if( (imager_pavolonis%emis_ch3b(i,j) > 1.6) .and. &
                  & (imager_measurements%DATA(i,j,5) < 300.0) ) then

                imager_pavolonis%cirrus_quality(i,j) = 1

             endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for cirrus clouds using the EMS 3.75 um test
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             ! solar contamination check temporalily disabled                              
             !if (SOLAR_CONTAMINATION_MASK(i,j) == sym%NO) then

             if ( (imager_pavolonis%CLDTYPE(i,j) /= sym%OVERLAP_TYPE) .and. &
                  & (imager_pavolonis%CLDTYPE(i,j) /= sym%OPAQUE_ICE_TYPE) .and. &
                  & (imager_pavolonis%emis_ch3b(i,j) > 1.10) .and. &
                  & (imager_measurements%DATA(i,j,5) < 300.0) ) then

                imager_pavolonis%CLDTYPE(i,j) = sym%CIRRUS_TYPE

             endif

             if ( ( imager_pavolonis%emis_ch3b(i,j) > 1.6           .and. &
                  &   imager_measurements%DATA(i,j,5) < 300.0 ) .or. &
                  & ( imager_pavolonis%emis_ch3b(i,j) > 1.4           .and. &
                  &   imager_measurements%DATA(i,j,5) < 300.0   .and. &
                  &   BTD_Ch4_Ch5 > BTD1112_CIRRUS_THRES) ) then

                imager_pavolonis%cirrus_quality(i,j) = 1

             endif

             !endif



             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             !-- Look for fog - not used over DESERT(=12) surfaces.
             !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             if ( (imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) >= 90.0) .and. &
                  & (imager_pavolonis%emis_ch3b(i,j) <= 0.90) .and. &
                  & (imager_measurements%DATA(i,j,5) > 240.0) .and. &
                  & (imager_pavolonis%SFCTYPE(i,j) /= sym%DESERT_FLAG) ) then

                ! imager_pavolonis%CLDTYPE(i,j) = sym%FOG_TYPE vfb stuttgart

             endif


             !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             !
             ! end of day/night if loop
          endif
          !
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


          !end scanline loop
       end do j_loop
       !-------------------------------------------------------------------


       !pixel loop
    end do i_loop
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

          if (imager_pavolonis%CLDTYPE(i,j) .ne. sym%CIRRUS_TYPE .and. &
               & imager_pavolonis%CLDTYPE(i,j) .ne. sym%OVERLAP_TYPE) cycle

          !-- coszen = cosine of the solar zenith angle
          coszen = cos(imager_angles%SOLZEN(i,j,imager_angles%NVIEWS))


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

          if ( imager_pavolonis%CLDTYPE(i,j) == sym%CIRRUS_TYPE .and. & 
               & imager_pavolonis%cirrus_quality(i,j) == 0) then

             !-- account for atmospheric effects
             t4_filter_thresh = 295.0 - 12.0*(1.0-coszen)

             !-- filter pixels in region
             !   added criterion for missing values in ems_Ch20 - akh 1/07
             if ( (minval(imager_measurements%DATA(start_pix:end_pix &
                  & ,start_line:end_line,5)) > t4_filter_thresh) .or. &
                  & ( (sum(imager_pavolonis%emis_ch3b(start_pix:end_pix &
                  & ,start_line:end_line))/npix < 1.2) .and. &
                  &   (minval(imager_pavolonis%emis_ch3b(start_pix:end_pix &
                  & ,start_line:end_line)) > 0.0) ) ) then

                if ( imager_measurements%DATA(i,j,5) <= 273.16 ) then
                   imager_pavolonis%CLDTYPE(i,j) = sym%SUPERCOOLED_TYPE
                else
                   imager_pavolonis%CLDTYPE(i,j) = sym%WATER_TYPE
                endif

             endif

          endif


          !-- At least one pixel in the 2n x 2n array must have a 
          !   Bt_Ch4 < 275 K for overlap;
          !   otherwise the pixel is reset to water or mixed.

          !-- account for atmospheric effects in BT4 threshold for filtering

          if ( imager_pavolonis%CLDTYPE(i,j) == sym%OVERLAP_TYPE .and. &
               & imager_angles%SOLZEN(i,j,imager_angles%NVIEWS) > 90.0 ) then

             t4_filter_thresh = 273.0 - 12.0*(1.0-coszen)

             if (minval(imager_measurements%DATA(start_pix:end_pix &
                  & ,start_line:end_line,5)) > t4_filter_thresh) then

                if (imager_measurements%DATA(i,j,5) <= 273.16) then
                   imager_pavolonis%CLDTYPE(i,j) = sym%SUPERCOOLED_TYPE
                else
                   imager_pavolonis%CLDTYPE(i,j) = sym%WATER_TYPE
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

!           if ((cld_type_array(i,j) == sym%CIRRUS_TYPE)  .or. &
!                (cld_type_array(i,j) == sym%OVERLAP_TYPE)) then

!              !ilrc = i_min_Bt_Ch31_3x3(i,j)
!              !jlrc = j_min_Bt_Ch31_3x3(i,j)
!              ilrc = i_lrc(i,j)
!              jlrc = j_lrc(i,j)

!              ! skip this if no lrc is available
!              if ( ilrc < 1 .or. jlrc < 1 ) then
!                 cycle
!              endif

!              if ((cld_type_array(ilrc,jlrc) == sym%FOG_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == sym%WATER_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == sym%SUPERCOOLED_TYPE)) then

!                 cld_type_array(i,j) = cld_type_array(ilrc,jlrc)

!              endif

!           endif



!           ! check for water clouds on the edge of cirrus

!           if ((cld_type_array(i,j) == sym%FOG_TYPE)  .or. &
!                (cld_type_array(i,j) == sym%WATER_TYPE)) then

!              ilrc = i_lrc(i,j)
!              jlrc = j_lrc(i,j)

!              if (ilrc < 1 .or. jlrc < 1) then
!                 cycle
!              endif

!              if ((cld_type_array(ilrc,jlrc) == sym%CIRRUS_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == sym%OVERLAP_TYPE) .or. &
!                   (cld_type_array(ilrc,jlrc) == sym%OPAQUE_ICE_TYPE)) then

!                 cld_type_array(i,j) = sym%CIRRUS_TYPE

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
    real(kind=sreal), dimension(4,15) :: coefficients ! coefficients containing variables

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
    case ("metop02")
       index = 10
    case ("metopa")
       index = 11
    case ("TERRA")
       index = 12
    case ("AQUA")
       index = 13
    case ("ENV")
       index = 14
    case ("default")
       index = 15
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
         2684.525,  0.997083,  1.94221, 5.061, & !noaa07
         2690.040,  0.997110,  1.87670, 5.081, & !noaa09
         2682.467,  0.997208,  1.77725, 5.055, & !noaa11
         2651.751,  0.997000,  1.89743, 4.949, & !noaa12
         2658.173,  0.996753,  1.98000, 4.974, & !noaa14
         2697.637,  0.998389,  1.65054, 5.088, & !noaa15
         2681.254,  0.998271,  1.67455, 5.033, & !noaa16
         2669.141,  0.997335,  1.69552, 5.008, & !noaa17
         2660.644,  0.997145,  1.71713, 4.981, & !noaa18
         2687.039,  0.996570,  2.05822, 5.076, & !metop02
         2687.039,  0.996570,  2.05822, 5.076, & !metopa
         2641.775,  0.999341,  0.47705, 4.804, & !terra
         2647.409,  0.999336,  0.48184, 4.822, & !aqua
         2675.166,  0.996344,  1.72695, 5.030, & !env (aatsr)
         2670.000,  0.998000,  1.75000, 5.000  & !default
         /), (/ 4, 15 /) )

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

