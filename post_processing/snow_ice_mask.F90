! Name:
!    snow_ice_mask
!
! Purpose:
!    Apply snow/ice identification mask which uses albedo cth and Istomina tests!          to determine if a scene is snow or ice and NOT cloud covered. This should remove some erroneous identification of snow/ice of cloud in the polar regions. It will also work over other snow covered surfaces. When thses scenes are misidentified erroneous high cloud cover and optical depths are detected over the poles
!
! Description:
!    
! Arguments:
!    Name       Type    In/Out/Both    Description
!    inputs: primary data, secondary data, instrument id,
!    outputs: clear snow flag
!
! Algorithm:
!    Operates on a single pixel so should be included in a loop     
!    1. check that snow is present using albedo mask
!;   2. calculate a subset of Istomina tests
!    3. apply CTH threshold
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
!
!2014/12/03 CP added in common_constants should eventually remove vartypes_pp
!
! Bugs:
!    None known.
!
! currently only applicabsele to 3.7um retrievals could be easily adapted to 1.6 retrievals.
! does not have AVHRR and MODIS channels in it.
! a mask on a DEM would be a better solution
!s
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

  !use neural_net_constants

  implicit none
  type(l2_input_struct_2d_primary) :: l2_input_2dwat_primary, l2_input_2dice_primary
  type(l2_input_struct_2d_secondary) :: l2_input_2d_secondary
  integer          :: status = 0 ! Status value returned from subroutines
  integer          :: snow_ice_flag,i,j
  real(kind=sreal) :: ch1,ch2,ch3,ch4,ch5,ch6,ch7
  real(kind=sreal) :: eq5,eq6,eq7,eq8
  real(kind=sreal) :: eq5_thres,eq6_thres,eq7_thres,eq8_thres
  real(kind=sreal) :: eq5_value,eq6_value,eq7_value,eq8_value,opd_thres
  real(kind=sreal) :: alb1_thres,alb2_thres,cth_thres_sea,cth_thres_land,cth_thres_opd_land,cth_thres_opd_sea
   CHARACTER(len= attribute_length) :: cinst
! .67,.87,1.6,3.7,11,12

!set default flag
  snow_ice_flag=0
  eq5_thres=0.02 
  eq6_thres=0.02
  eq7_thres=0.8
  eq8_thres=0.1
  alb1_thres=0.2
  alb2_thres=0.1
  cth_thres_sea=2.9!km
  cth_thres_land=4.0!km
  cth_thres_opd_land=2.0!km this should be more strict
  cth_thres_opd_sea=1.0!km this should be more strict
  opd_thres=150.



!
!set up the channels for each instrument
!

! check albedo of scene this should be the uncorrected (i.e no sza correction) if albedo high enough then proceed.


!write(*,*)'albedo',l2_input_2d_secondary%albedo_IN_CHANNEL_NO_2(i,j),l2_input_2d_secondary%albedo_IN_CHANNEL_NO_3(i,j),alb1_thres,alb2_thres 

  if (l2_input_2d_secondary%albedo_IN_CHANNEL_NO_2(i,j) .gt. alb1_thres .and. l2_input_2d_secondary%albedo_IN_CHANNEL_NO_3(i,j) .gt. alb2_thres) then

!write(*,*)'albedo',l2_input_2d_secondary%albedo_IN_CHANNEL_NO_2(i,j),l2_input_2d_secondary%albedo_IN_CHANNEL_NO_3(i,j)

     !
     !calculate istomina equations
     !


     
! check what instrument used
     
!     write(*,*)'cinst', cinst
     if (cinst .eq. 'AATSR') then
        
        ch1=l2_input_2d_secondary%reflectance_in_channel_no_2(i,j) 
        ch2=l2_input_2d_secondary%reflectance_in_channel_no_3(i,j) 
        ch3=l2_input_2d_secondary%brightness_temperature_in_channel_no_5(i,j) 
        ch4=l2_input_2d_secondary%brightness_temperature_in_channel_no_6(i,j) 
        ch5=l2_input_2d_secondary%brightness_temperature_in_channel_no_7(i,j) 
        
     endif


     if (cinst .eq. 'MODIS') then
        
        ch1=l2_input_2d_secondary%reflectance_in_channel_no_1(i,j) 
        ch2=l2_input_2d_secondary%reflectance_in_channel_no_2(i,j) 
        ch3=l2_input_2d_secondary%brightness_temperature_in_channel_no_20(i,j) 
        ch4=l2_input_2d_secondary%brightness_temperature_in_channel_no_31(i,j) 
        ch5=l2_input_2d_secondary%brightness_temperature_in_channel_no_32(i,j) 
        
     endif


     if (cinst .eq. 'AVHRR') then
        
        ch1=l2_input_2d_secondary%reflectance_in_channel_no_1(i,j) 
        ch2=l2_input_2d_secondary%reflectance_in_channel_no_2(i,j) 
        ch3=l2_input_2d_secondary%brightness_temperature_in_channel_no_4(i,j) 
        ch4=l2_input_2d_secondary%brightness_temperature_in_channel_no_5(i,j) 
        ch5=l2_input_2d_secondary%brightness_temperature_in_channel_no_6(i,j) 
        
     endif

     
     
     ! Istomina eq.5 day/night
     
     eq5_value=abs((ch3-ch4)/ch3)
     
     
     ! Istomina eq.6 day/night
     
     eq6_value=abs((ch3-ch5)/ch3)
     
!write(*,*)'eq5_value eq6_value',eq5_value,eq6_value
!write(*,*)'l2_input_2dice_primary%illum(i,j) ',l2_input_2dice_primary%illum(i,j) 
     if(l2_input_2dice_primary%illum(i,j) .eq. 1_byte .or.&
          & l2_input_2dice_primary%illum(i,j) .eq. 4_byte .or. &
          & l2_input_2dice_primary%illum(i,j) .eq. 5_byte .or.&
          & l2_input_2dice_primary%illum(i,j) .eq. 6_byte .or. &
          & l2_input_2dice_primary%illum(i,j) .eq. 7_byte .or.&
          & l2_input_2dice_primary%illum(i,j) .eq. 8_byte .or. &
          & l2_input_2dice_primary%illum(i,j) .eq. 9_byte  ) then

        ! day Istomina eq.8 day only currently not used.
        
        eq8_value=(ch2-ch1)/ch2
        
!  write(*,*)'cth',l2_input_2dice_primary%cth(i,j)      
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


        ! possibility to add some more tests based on optical depth/ height/effective radius
        if (l2_input_2dice_primary%lsflag(i,j) .eq. 0_byte) then
           if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_opd_sea) then
              if (l2_input_2dice_primary%cot(i,j) .gt. opd_thres .and. l2_input_2dice_primary%ref(i,j) .gt. 43 .and. l2_input_2dice_primary%ref(i,j) .lt. 60 ) then

                 snow_ice_flag=1
!write(*,*)'snow flag c',snow_ice_flag
!                 write(*,*)' optical depth flag sea',l2_input_2dice_primary%cot(i,j) ,l2_input_2dice_primary%ref(i,j),l2_input_2dice_primary%cth(i,j) 
              endif
           endif
           
           
        else
           !land
           if (l2_input_2dice_primary%cth(i,j) .lt. cth_thres_opd_land) then
              if (l2_input_2dice_primary%cot(i,j) .gt. opd_thres .and. l2_input_2dice_primary%ref(i,j) .gt. 40 .and. l2_input_2dice_primary%ref(i,j) .lt. 60 ) then
                 snow_ice_flag=1
!                 write(*,*)' optical depth flag land', snow_ice_flag,l2_input_2dice_primary%cot(i,j) ,l2_input_2dice_primary%ref(i,j),l2_input_2dice_primary%cth(i,j) 
              endif
           endif
        endif
        
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
        
  endif ! albedo




end subroutine snow_ice_mask



