! Name:
!    snow_ice_mask
!
! Purpose:
!    Apply snow/ice identification mask which uses albedo, cth and Istomina tests to determine 
! if a scene is snow or ice and NOT cloud covered. This should remove some erroneous
! identification of snow/ice of cloud in the polar regions. It will also work over 
! other snow covered surfaces. When these scenes are misidentified erroneous high
! cloud cover and optical depths are detected over the poles. The Istomina tests will
! also work over land but the application here is specific to ice/snow surfaces
!
! Description:
!    
! Arguments:
!    Name       Type    In/Out/Both    Description
!    inputs: primary data, secondary data, instrument type instrument id, index of array
!    outputs: clear snow flag
!
! Algorithm:
!    Operates on a single pixel so should be included in a loop     
!    1. check that snow is present using albedo mask
!    2. calculate a subset of Istomina tests (i.e. test only relevant to heritage channels)
!    3. Apply CTH threshold
!    4. different tests applicable day or night

!
! Local variables:
!    Name       Type    Description
!
! History:
!   28/8/2014 Caroline Poulsen :original version
!   25/09/2014 Caroline Poulsen :updated to include channel translation for AVHRR and MODIS
!   25/09/2014 Caroline Poulsen :tidy up and updated to include night tests
!   29/09/2014 Caroline Poulsen :fixed up flagging at night
!   24/10/2014 Oliver Sus: shortened overly long line
!   20/11/2014 Oliver Sus: deactivated use of module neural_net_constants
!   2014/12/03 CP added in common_constants should eventually remove vartypes_pp
!   2015/01/12 CP added in more comments to make it clearer.
!   2015/01/19 CP added in some more stringent cloud clearing
!   2015/02/09 CP applied mask during night updated to work for AVHRR and MODIS instruments
!   2015/02/09 CP applied mask to remove very thick ice cloud over ice surfaces
!   2015/04/23 OS: some minor edits
!
! Bugs:
!    None known.
!
! currently only applicable to 3.7um retrievals could be easily adapted to 1.6 retrievals.
! does not have AVHRR and MODIS channels in it.
! It would be advantageous to refine the CTH threshold using a DEM
! .67,.87,1.6,3.7,11,12- heritage channels only used at the moment
!
! $Id$
!

!---------------------------------------------------------------------
!---------------------------------------------------
!---------------------------------------------------

subroutine snow_ice_mask(l2_input_2dice_primary,l2_input_2d_secondary,snow_ice_flag,cinst,i,j)
  !---------------------------------------------------
  !---------------------------------------------------

  use netcdf
  use scanline_structure
  use vartypes_pp
  use common_constants
  use structures_pp

  implicit none
  type(l2_input_struct_2d_primary) :: l2_input_2dwat_primary, l2_input_2dice_primary
  type(l2_input_struct_2d_secondary) :: l2_input_2d_secondary
  integer          :: status = 0 ! Status value returned from subroutines
  integer          :: snow_ice_flag,i,j
  real(kind=sreal) :: ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch1alb,ch2alb
  real(kind=sreal) :: eq5,eq6,eq7,eq8
  real(kind=sreal) :: eq5_thres,eq6_thres,eq7_thres,eq8_thres,re_min,re_max
  real(kind=sreal) :: eq5_thres_strict,eq6_thres_strict,eq5_thres_loose,eq6_thres_loose,re_ice
  real(kind=sreal) :: eq5_value,eq6_value,eq7_value,eq8_value,opd_thres,opd_max,eq5_thres_iwp,eq6_thres_iwp
  real(kind=sreal) :: alb1_thres,alb2_thres,cth_thres_sea,cth_thres_land,cth_thres_opd_land,cth_thres_opd_sea
  CHARACTER(len= var_length) :: cinst


  !set default flag
  snow_ice_flag=0
  !Istomina thresholds
  eq5_thres=0.02 
  eq6_thres=0.02
  eq5_thres_strict=0.01 
  eq6_thres_strict=0.015
  eq5_thres_loose=0.02 
  eq6_thres_loose=0.02
  eq5_thres_iwp=0.03 
  eq6_thres_iwp=0.03
  eq7_thres=0.8
  eq8_thres=0.1
  !albedo thresholds

  alb1_thres=0.2
  alb2_thres=0.1

  !CTH/OPD/RE threshold for checking we are not looking at an ice cloud at a low height which is morelikely snow/ice

  cth_thres_sea=2.9!km
  cth_thres_land=4.0!km
  cth_thres_opd_land=2.0!km this should be more strict
  cth_thres_opd_sea=1.0!km this should be more strict
  opd_thres=150.
  opd_max=250.
  re_ice=30
  re_min=40
  re_max=120



  ! check what instrument used


  if (cinst .eq. 'AATSR') then
     ch1alb=l2_input_2d_secondary%albedo_IN_CHANNEL_NO_2(i,j)
     ch2alb=l2_input_2d_secondary%albedo_IN_CHANNEL_NO_3(i,j)        
     ch1=l2_input_2d_secondary%reflectance_in_channel_no_2(i,j) 
     ch2=l2_input_2d_secondary%reflectance_in_channel_no_3(i,j) 
     ch3=l2_input_2d_secondary%brightness_temperature_in_channel_no_5(i,j) 
     ch4=l2_input_2d_secondary%brightness_temperature_in_channel_no_6(i,j) 
     ch5=l2_input_2d_secondary%brightness_temperature_in_channel_no_7(i,j) 

  endif


  if (cinst .eq. 'MODIS') then
     ch1alb=l2_input_2d_secondary%albedo_IN_CHANNEL_NO_1(i,j)
     ch2alb=l2_input_2d_secondary%albedo_IN_CHANNEL_NO_2(i,j)        
     ch1=l2_input_2d_secondary%reflectance_in_channel_no_1(i,j) 
     ch2=l2_input_2d_secondary%reflectance_in_channel_no_2(i,j) 
     ch3=l2_input_2d_secondary%brightness_temperature_in_channel_no_20(i,j) 
     ch4=l2_input_2d_secondary%brightness_temperature_in_channel_no_31(i,j) 
     ch5=l2_input_2d_secondary%brightness_temperature_in_channel_no_32(i,j) 

  endif


  if (cinst .eq. 'AVHRR') then
     ch1alb=l2_input_2d_secondary%albedo_IN_CHANNEL_NO_1(i,j)
     ch2alb=l2_input_2d_secondary%albedo_IN_CHANNEL_NO_2(i,j)        
     ch1=l2_input_2d_secondary%reflectance_in_channel_no_1(i,j) 
     ch2=l2_input_2d_secondary%reflectance_in_channel_no_2(i,j) 
     ch3=l2_input_2d_secondary%brightness_temperature_in_channel_no_4(i,j) 
     ch4=l2_input_2d_secondary%brightness_temperature_in_channel_no_5(i,j) 
     ch5=l2_input_2d_secondary%brightness_temperature_in_channel_no_6(i,j) 

  endif





  !
  !set up the channels for each instrument
  !

  ! Check albedo of scene. This should be the uncorrected (i.e no sza correction) albedo. 
  ! If albedo high enough (i.e. indicates an snow/ice scene), proceed.

  ! Testing output Istomina eq.5 day/night

  eq5_value=abs((ch3-ch4)/ch3)


  ! Istomina eq.6 day/night

  eq6_value=abs((ch3-ch5)/ch3)


  !     write(*,*)  'eq5_value  ',eq5_value,eq6_value  ,l2_input_2dice_primary%cth(i,j)  ,l2_input_2dice_primary%illum(i,j)  ,    l2_input_2d_secondary%albedo_IN_CHANNEL_NO_2(i,j),l2_input_2d_secondary%albedo_IN_CHANNEL_NO_3(i,j),ch3,ch5

  if (ch1alb .gt. alb1_thres .and. ch2alb .gt. alb2_thres) then

     !
     !calculate Istomina equations
     !

     ! Istomina eq.5 day/night

     eq5_value=abs((ch3-ch4)/ch3)


     ! Istomina eq.6 day/night

     eq6_value=abs((ch3-ch5)/ch3)

     !
     !check what illumination eg. day/night
     !
     if(l2_input_2dice_primary%illum(i,j) .eq. 1_byte .or.&
          & l2_input_2dice_primary%illum(i,j) .eq. 4_byte .or. &
          & l2_input_2dice_primary%illum(i,j) .eq. 5_byte .or.&
          & l2_input_2dice_primary%illum(i,j) .eq. 6_byte .or. &
          & l2_input_2dice_primary%illum(i,j) .eq. 7_byte .or.&
          & l2_input_2dice_primary%illum(i,j) .eq. 8_byte .or. &
          & l2_input_2dice_primary%illum(i,j) .eq. 9_byte  ) then

        ! day Istomina eq. 8 day only currently not used.

        eq8_value=(ch2-ch1)/ch2


        if ((eq5_value .lt. eq5_thres) .and. (eq6_value .lt. eq6_thres)) then

           ! apply CTH thresholds different over sea than over land


           !over sea

           if (l2_input_2dice_primary%lsflag(i,j) .eq. 0_byte) then

              if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_sea) then
                 snow_ice_flag=1
              endif!cth



           else
              !over land

              if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_land) then
                 snow_ice_flag=1
              endif !cth


           endif ! land/sea

        endif!istomina tests



        ! possibility to add some more tests based on optical depth/ height/effective radius
        !        if (l2_input_2dice_primary%lsflag(i,j) .eq. 0_byte) then
	!sea
        !           if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_opd_sea) then
        !              if (l2_input_2dice_primary%cot(i,j) .gt. opd_thres .and. l2_input_2dice_primary%ref(i,j) .gt. re_min .and. l2_input_2dice_primary%ref(i,j) .lt. re_max ) then

        !                 snow_ice_flag=1

        !              endif
        !           endif


        !        else
        !land
        !           if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_opd_land) then
        !              if (l2_input_2dice_primary%cot(i,j) .gt. opd_thres .and. l2_input_2dice_primary%ref(i,j) .gt. re_min .and. l2_input_2dice_primary%ref(i,j) .lt. re_max ) then
        !                 snow_ice_flag=1

        !              endif
        !           endif
        !        endif

     else
        ! night tests

        if ((eq5_value .lt. eq5_thres) .and. (eq6_value .lt. eq6_thres)) then

           !over sea
           if (l2_input_2dice_primary%lsflag(i,j) .eq. 0_byte) then

              if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_sea) then
                 snow_ice_flag=1
              endif!cth

           else
              !over land
              ! apply CTH thresholds different over sea than over land


              if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_land) then
                 snow_ice_flag=1
              endif !cth


           endif ! land/sea

        endif!istomina tests

     endif! illumination


     !
     !Thses tests are a bit empirical but seem to do the job, could be optimise further.
     !
     !apply looser Istomina tests that have an albedo test and a test on cod particually effective for grrenalnd and poles
     !

     !
     !this test used to reduce too much iwp over greenland/poles
     !
     if ((eq5_value .lt. eq5_thres_iwp) .and. (eq6_value .lt. eq6_thres_iwp)) then	  

        if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_sea) then
           if (l2_input_2dice_primary%ref(i,j) .gt. re_min .and. l2_input_2dice_primary%cot(i,j) .gt. opd_thres) then
              snow_ice_flag=1
           endif
        endif
     endif

     !
     !this test used to reduce too much iwp over greenland
     !
     if ((eq5_value .lt. eq5_thres_iwp) .and. (eq6_value .lt. eq6_thres_iwp)) then


        if (l2_input_2dice_primary%cot(i,j) .gt. opd_max) then
           snow_ice_flag=1
        endif

     endif

  endif ! albedo


  !!this test does not require albedo test curently removed because it does not work at night
  !apply extra strict Istomina tests that are not dependent on albedo. This test could have implications globally particually for thin cloud put too low
  !

  !  if ((eq5_value .lt. eq5_thres_strict) .and. (eq6_value .lt. eq6_thres_strict)) then

  !  if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_land) then
  !  if  (l2_input_2dice_primary%ref(i,j) .gt. re_min) then
  ! snow_ice_flag=1
  !endif
  !endif
  !endif

end subroutine snow_ice_mask



