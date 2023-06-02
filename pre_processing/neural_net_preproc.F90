!-------------------------------------------------------------------------------
! Name: neural_net_preproc.F90
!
! Purpose:
! Module for neural network based cloud mask, applied in preprocessing. Input
! data are satellite radiances and auxiliary data.
!
! History:
! 2014/10/23, SS+OS: Original version.
! 2014/11/04, SS+OS: implemented new cloud mask version, which is now also
!    available for twilight and additionally uses ECMWF skin temperature and
!    flags for snow/ice and land/sea
! 2014/11/20, SS+OS: implemented new temporal fill values for all channels,
!    which was necessary because with these cloud mask classifies pixels with
!    missing Ch3b data (accidentally) correctly as cloud
! 2014/11/20, SS+OS: removed previous implementation; instead, for missing Ch3b
!    nighttime pixels, twilight cloud mask is applied
! 2014/12/03, SS+OS: SATZEN is used for correcting viewing angle effect in NN
!    output for AVHRR - still testing
! 2015/02/04, SS+OS: now using sym%NO to set twilight flag when ch3b missing at
!    night; now using twilight cloud mask for 80 < solzen < 90
! 2015/03/13, SS: glint angle correction of neural net output; removed bug that
!    called neural net even though solzen is negative; added new cloud masks
!    and threshold tests; use surface temperature instead of skin temperature
!    when the latter is negative
! 2015/06/03, SS: Compensates missing channel2 reflectances for MODIS in high
!    reflective clouds (when channel 1 has high reflectance and channel 2 has
!    fillvalue), forcing the cloud mask to be clear
! 2015/06/03, SS: double checks sunglint area, sometimes still misinterpreted
!    as cloudy , checks in sunglint at day over sea if reflectances in ch1 and
!    ch2 is high and also ch3b has values gt 300 Kelvin
! 2015/06/24, SS: added cloud mask uncertainty
! 2015/10/29, CP: functionality for ATSR2 added added SteSta NN calibration
!    corrections
! 2015/11/17, OS: added platform flag and correction coefficients for MODIS and
!    AATSR; removed sunglint double check; minor editing
! 2015/12/17, OS: changed structure of setting thresholds
! 2016/01/21, OS: Added correction for ice-free sea skin temperature - to be
!    tested
! 2016/01/21, OS: Removed offset when correcting AATSR ch1 and ch2 data
! 2016/02/05, OS: Cloud mask now uses albedo for glint correction.
! 2016/02/26, OS: Added different approach for nighttime cloud mask SST
!    correction.
! 2016/03/04, OS: bug fix in albedo correction
! 2016/04/14, SP: Added support for Himawari/AHI
! 2017/05/10, SS: Changed daytime ANN cloudmask, which now uses NIR data
! 2017/05/15, SS: Added ANN trained with 1.6um, to use for AVHRRs with switching
!    channel
! 2017/06/20, SS: due to the lack of proper sunglint correction for early morning
!    NOAA satellites, introduced flag correct_realy_morning_noaas (default true)
!    to use only IR data at daytime
! 2017/06/28, SS: added new 1.6um ANN

! Bugs:
! None known.
!-------------------------------------------------------------------------------

module neural_net_preproc_m

   implicit none

contains

!------------------------------------------------------------------------
subroutine ann_cloud_mask(channel1, channel2, channel3a, channel3b, &
     channel3b_ref, channel4, channel5, &
     solzen, satzen, niseflag, lsflag, desertflag, &
     albedo1, albedo2, albedo3a, cccot_pre, cldflag, cld_uncertainty, skint, &
     ch3a_on_avhrr_flag, glint_angle, sensor_name, platform, verbose)
   !------------------------------------------------------------------------

   use constants_cloud_typing_pavolonis_m
   use common_constants_m
   use neural_net_constants_m

   implicit none

   integer(kind=sint) :: noob     !# of pixels out of bounds
   integer(kind=sint) :: nneurons !# of employed neurons
   integer(kind=sint) :: ninput   !# of input dimensions of ann

   real(kind=sreal), allocatable, dimension(:,:) :: inv, minmax_train, scales
   real(kind=sreal), allocatable, dimension(:)   :: input, outv

   real(kind=sreal)   :: output
   real(kind=sreal)   :: oscales(3)
   real(kind=sreal)   :: temperature, cutoff, bias_i, bias_h
   integer(kind=byte) :: illum_nn ! 0 = undefined, 1 = day, 2 = twilight, 3 = night

   ! INPUT from cloud_type subroutine (module cloud_typing_pavolonis.F90)
   character(len=*),   intent(in)  :: sensor_name
   character(len=*),   intent(in)  :: platform
   integer(kind=byte), intent(in)  :: lsflag, niseflag
   integer(kind=sint), intent(in)  :: ch3a_on_avhrr_flag
   real(kind=sreal),   intent(in)  :: solzen, skint, satzen, glint_angle
   real(kind=sreal),   intent(in)  :: albedo1, albedo2, albedo3a ! cox-munk computed BRDF (bidirectional reflectance) albedo used for sunglint correction
   real(kind=sreal),   intent(in)  :: channel1, channel2, channel3a, channel3b, channel3b_ref, channel4, channel5
   logical,            intent(in)  :: verbose, desertflag

   ! OUTPUT to cloud_type subroutine (module cloud_typing_pavolonis.F90)
   integer(kind=byte), intent(out) :: cldflag
   real(kind=sreal),   intent(out) :: cccot_pre
   real(kind=sreal),   intent(out) :: cld_uncertainty

   ! LOCAL variables
   real(kind=sreal)   :: ch1, ch2, ch3b, ref3a, ref3b, ch4, ch5 , threshold_used, ch1_uc
   real(kind=sreal)   :: btd_ch4_ch5, btd_ch4_ch3b, norm_diff_cc_th, mu0
   integer(kind=sint) :: glint_mask
   logical            :: call_neural_net, do_ref3b_alb_corr, calc_true_refl, desert

   calc_true_refl = .FALSE. ! this is .false. at the moment, change to .true. if
                            ! future ANN's are trained with true reflectances


   mu0 = cos ( solzen * d2r )

   desert = desertflag
   desert = .FALSE. ! as long as Albedo over land includes BRDF

   ! not used anymore; albedo is used instead
   if ( glint_angle .lt. 40.0 .and. glint_angle .ne. sreal_fill_value ) then
      glint_mask = YES
   else
      glint_mask = NO
   end if

   ch1_uc = 0.

   if ( channel1 .eq. sreal_fill_value ) then
      ch1 = channel1
   else
      ch1 = channel1 * 100.
      if ((lsflag .eq. 0_byte .or. desert) .and. (niseflag .eq. NO) .and. (albedo1 .ge. 0.) .and. (correct_glint) ) then
         ch1_uc = ch1
         ch1 = max(ch1 - min(albedo1, 1.) * 100. * 0.4, 0.)
      end if
   end if

   if ( channel2 .eq. sreal_fill_value ) then
      if ( (trim(adjustl(sensor_name)) .eq. 'MODIS') .and. ch1 .gt. 50. ) then
         ch2 = ch1 * 1.03
      else
         ch2 = channel2
      end if
   else
      ch2 = channel2 * 100.
      if ((lsflag .eq. 0_byte .or. desert) .and. (niseflag .eq. NO) .and. (albedo2 .gt. 0.) .and. (correct_glint) ) ch2 = max(ch2 - min(albedo2, 1.) * 100. * 0.4, 0.)
   end if

   ch3b = channel3b
   do_ref3b_alb_corr = .FALSE.

   if ( ch3a_on_avhrr_flag .ne. NO ) then
      if ( channel3a .eq. sreal_fill_value ) then
         ref3a = channel3a
      else
         ! We use a 1.6um trained ANN now
         ref3a = channel3a * 100.
         ! We can also use the Albedo correction of the 1.6um channel
         if ((lsflag .eq. 0_byte .or. desert) .and. (niseflag .eq. NO) .and. (albedo3a .gt. 0.) .and. (correct_glint) ) ref3a = max(ref3a - min(albedo3a, 1.) * 100. * 0.55, 0.)
      end if
   else
      if ( channel3b_ref .eq. sreal_fill_value ) then
         ref3b = channel3b_ref
      else
         if (ch1_uc .ne. 0.) do_ref3b_alb_corr = .TRUE.
         ref3b = channel3b_ref*100.
      end if
   end if

   if ( do_ref3b_alb_corr ) then
      !use same albedo correction as for ch1 avoid correction of high clouds
      !with low BT differences
      if ( ch3b .gt. 290 ) ref3b = ref3b * ch1 / ch1_uc
   end if

   ch4 = channel4
   ch5 = channel5
   btd_ch4_ch5  = ch4 - ch5
   btd_ch4_ch3b = ch4 - ch3b

   ! call neural net unless solzen is negative
   call_neural_net = .TRUE.

   if ( ( solzen .ge. 0. ) .and. (solzen  .le. 84) ) then
      illum_nn = 1                                            ! use ANN with Ref3.7
      if  ( ch3a_on_avhrr_flag .eq. INEXISTENT ) illum_nn = 4 ! use ANN w/o any NIR info
      if  ( ch3a_on_avhrr_flag .eq. YES ) illum_nn = 7        ! use ANN with Ref1.6
  elseif ( (solzen  .gt. 84) .and. (solzen .le. 90) ) then
      illum_nn = 2                                    ! use ANN with BT3.7
      if  ( ch3a_on_avhrr_flag .ne. NO ) illum_nn = 5 ! use ANN w/o BT3.7
      if  ( correct_early_morning_noaas ) then
         !Use Night ANN w/o BT3.7 at daytime
         !The early morning Noaa sats have very strong glint dependencies
         !at high viewing angles which is not captured by any ANN training.
         if ( (trim(adjustl(platform)) .eq. 'noaa6') .or. (trim(adjustl(platform)) .eq. 'noaa8') .or. &
              (trim(adjustl(platform)) .eq. 'noaa10') .or. (trim(adjustl(platform)) .eq. 'noaa12') .or. (trim(adjustl(platform)) .eq. 'noaa15') ) illum_nn = 6
      end if
   elseif (solzen  .gt. 90)  then
      illum_nn = 3                                    ! use ANN with BT3.7
      if  ( channel1 .gt. 0.02 ) illum_nn = 6         ! dont use 3.7µm if sensor detects sunlight (check on ch1 w/o albedo correction!)
      if  ( ch3a_on_avhrr_flag .ne. NO ) illum_nn = 6 ! use ANN w/o BT3.7
   else
      illum_nn = 0
      ! if solzen is negative, do not call neural net
      call_neural_net = .FALSE.
   end if

   threshold_used = sreal_fill_value

   if ( illum_nn .eq. 1 ) then

      ! DAY
      if ( calc_true_refl ) then
         ch1   = ch1/mu0
         ch2   = ch2/mu0
         ref3b = ref3b/mu0
      end if

      nneurons = nneurons_ex27  !set number of neurons
      ninput   = ninput_ex27    !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex27

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex27

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex27

      allocate(scales(ninput,2))
      scales = scales_ex27           !parameters to scale input?
      oscales = oscales_ex27         !parameters to scale output?
      temperature = temperature_ex27 !"temperature" for sigmoid function
      cutoff = cutoff_ex27
      bias_i = bias_i_ex27
      bias_h = bias_h_ex27

      ! input
      allocate(input(ninput+1))

      input(1) = ch1           ! ch1 600nm
      input(2) = ch2           ! ch2 800nm
      input(3) = ref3b         ! ch3b reflectance 3.7 µm
      input(4) = ch4           ! ch4 11 µm
      input(5) = ch5           ! ch5 12 µm
      input(6) = btd_ch4_ch5   ! 11-12 µm
      input(7) = skint         ! ERA-Interim skin temperature
      input(8) = niseflag      ! snow/ice information
      input(9) = lsflag        ! land/sea flag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_THRES_DAY_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_DAY_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_THRES_DAY_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_DAY_LAND_ICE
      end if

   elseif ( illum_nn .eq. 2 )  then

      ! TWILIGHT

      nneurons       = nneurons_ex30   !set number of neurons
      ninput         = ninput_ex30     !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex30

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex30

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex30

      allocate(scales(ninput,2))
      scales = scales_ex30               !parameters to scale input?
      oscales = oscales_ex30             !parameters to scale output?
      temperature = temperature_ex30     !"temperature" for sigmoid function
      cutoff = cutoff_ex30
      bias_i = bias_i_ex30
      bias_h = bias_h_ex30

      !input
      allocate(input(ninput+1))

      input(1) = ch3b          ! ch3b 3.7 µm
      input(2) = ch4           ! ch4 11 µm
      input(3) = btd_ch4_ch3b  ! 11-3.7 µm
      input(4) = ch5           ! ch5 12 µm
      input(5) = btd_ch4_ch5   ! 11-12 µm
      input(6) = skint
      ! exclude negative skin-rad4 temperatures at night/twilight
      if ( (correct_skint) .and. ( skint - ch4 ) .lt. 0 ) input(6) = ch4
      input(7) = niseflag
      input(8) = lsflag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_THRES_TWL_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_TWL_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_THRES_TWL_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_TWL_LAND_ICE
      end if

   elseif ( illum_nn .eq. 3 ) then
      ! --- night

      nneurons       = nneurons_ex32   !set number of neurons
      ninput         = ninput_ex32     !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex32

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex32

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex32

      allocate(scales(ninput,2))
      scales = scales_ex32               !parameters to scale input?
      oscales = oscales_ex32             !parameters to scale output?
      temperature = temperature_ex32     !"temperature" for sigmoid function
      cutoff = cutoff_ex32
      bias_i = bias_i_ex32
      bias_h = bias_h_ex32

      !input
      allocate(input(ninput+1))
      input(1) = ch3b          ! ch3b 3.7µm
      input(2) = ch4           ! ch4 11 µm
      input(3) = btd_ch4_ch3b  ! 11-3.7 µm
      input(4) = ch5           ! ch5 12 µm
      input(5) = btd_ch4_ch5   ! 11-12 µm
      input(6) = skint
      ! exclude negative skin-rad4 temperatures at night/twilight
      if ( (correct_skint) .and. ( skint - ch4 ) .lt. 0 ) input(6) = ch4
      input(7) = niseflag
      input(8) = lsflag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_THRES_NIGHT_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_NIGHT_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_THRES_NIGHT_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_NIGHT_LAND_ICE
      end if

   elseif ( illum_nn .eq. 4 ) then

      ! DAY 2 (No NIR Info avail. )
      if ( calc_true_refl ) then
         ch1   = ch1/mu0
         ch2   = ch2/mu0
      end if

      nneurons = nneurons_ex28  !set number of neurons
      ninput   = ninput_ex28    !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex28

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex28

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex28

      allocate(scales(ninput,2))
      scales = scales_ex28           !parameters to scale input?
      oscales = oscales_ex28         !parameters to scale output?
      temperature = temperature_ex28 !"temperature" for sigmoid function
      cutoff = cutoff_ex28
      bias_i = bias_i_ex28
      bias_h = bias_h_ex28

      ! input
      allocate(input(ninput+1))

      input(1) = ch1           ! ch1 600nm
      input(2) = ch2           ! ch2 800nm
      input(3) = ch4           ! ch4 11 µm
      input(4) = ch5           ! ch5 12 µm
      input(5) = btd_ch4_ch5   ! 11-12 µm
      input(6) = skint         ! ERA-Interim skin temperature
      input(7) = niseflag      ! snow/ice information
      input(8) = lsflag        ! land/sea flag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_THRES_DAY_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_DAY_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_THRES_DAY_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_DAY_LAND_ICE
      end if

   elseif ( illum_nn .eq. 5 )  then

      ! TWILIGHT 2 (no 3.7)

      nneurons       = nneurons_ex31   !set number of neurons
      ninput         = ninput_ex31     !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex31

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex31

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex31

      allocate(scales(ninput,2))
      scales = scales_ex31               !parameters to scale input?
      oscales = oscales_ex31             !parameters to scale output?
      temperature = temperature_ex31     !"temperature" for sigmoid function
      cutoff = cutoff_ex31
      bias_i = bias_i_ex31
      bias_h = bias_h_ex31

      !input
      allocate(input(ninput+1))

      input(1) = ch4           ! ch4 11 µm
      input(2) = ch5           ! ch5 12 µm
      input(3) = btd_ch4_ch5   ! 11-12 µm
      input(4) = skint
      ! exclude negative skin-rad4 temperatures at night/twilight
      if ( (correct_skint) .and. ( skint - ch4 ) .lt. 0 ) input(4) = ch4
      input(5) = niseflag
      input(6) = lsflag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_THRES_TWL_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_TWL_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_THRES_TWL_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_TWL_LAND_ICE
      end if

   elseif ( illum_nn .eq. 6 ) then

      ! --- night 2 (no 3.7)

      nneurons       = nneurons_ex33   !set number of neurons
      ninput         = ninput_ex33     !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex33

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex33

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex33

      allocate(scales(ninput,2))
      scales = scales_ex33               !parameters to scale input?
      oscales = oscales_ex33             !parameters to scale output?
      temperature = temperature_ex33     !"temperature" for sigmoid function
      cutoff = cutoff_ex33
      bias_i = bias_i_ex33
      bias_h = bias_h_ex33

      !input
      allocate(input(ninput+1))
      input(1) = ch4           ! ch4 11 µm
      input(2) = ch5           ! ch5 12 µm
      input(3) = btd_ch4_ch5   ! 11-12 µm
      input(4) = skint
      ! exclude negative skin-rad4 temperatures at night/twilight
      if ( (correct_skint) .and. ( skint - ch4 ) .lt. 0 ) input(4) = ch4
      input(5) = niseflag
      input(6) = lsflag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_THRES_NIGHT_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_NIGHT_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_THRES_NIGHT_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_NIGHT_LAND_ICE
      end if

   elseif ( illum_nn .eq. 7 ) then

      ! DAY 3 ( use 1.6 instead of 3.7 channel)
      if ( calc_true_refl ) then
         ch1   = ch1/mu0
         ch2   = ch2/mu0
         ref3a = ref3a/mu0
      end if

      nneurons = nneurons_ex29  !set number of neurons
      ninput   = ninput_ex29    !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex29

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex29

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex29

      allocate(scales(ninput,2))
      scales = scales_ex29           !parameters to scale input?
      oscales = oscales_ex29         !parameters to scale output?
      temperature = temperature_ex29 !"temperature" for sigmoid function
      cutoff = cutoff_ex29
      bias_i = bias_i_ex29
      bias_h = bias_h_ex29

      ! input
      allocate(input(ninput+1))

      input(1) = ch1           ! ch1 600nm
      input(2) = ch2           ! ch2 800nm
      input(3) = ref3a         ! ch3a reflectance 1.6 µm
      input(4) = ch4           ! ch4 11 µm
      input(5) = ch5           ! ch5 12 µm
      input(6) = btd_ch4_ch5   ! 11-12 µm
      input(7) = skint         ! ERA-Interim skin temperature
      input(8) = niseflag      ! snow/ice information
      input(9) = lsflag        ! land/sea flag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_THRES_DAY_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_DAY_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_THRES_DAY_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_THRES_DAY_LAND_ICE
      end if

   else

      if (verbose) then
         !        write(*,*) "Solar zenith angle < 0 in neural_net_preproc"
      end if

      ! --- end of day/night if loop
   end if

   !this should never happen and is only the case if lsflag is neither 0 nor 1
   if ( threshold_used .eq. sreal_fill_value ) call_neural_net = .false.

   ! --- subroutine which carries out neural network computation

   if ( call_neural_net ) then

      call neural_net(nneurons, ninput, minmax_train, inv, outv, &
           input, scales, oscales, cutoff, bias_i, bias_h, &
           temperature, output, noob)

      ! --- correct for viewing angle effect - test phase for AVHRR
      if (correct_view) output = output - ( 1. / 12. * ( 1. / cos( satzen * d2r) - 1. ) )

      if (correct_sst) then
         ! Testing ice-free sea skin temperature correction
         if  (( lsflag .eq. 0_byte ) .AND. (niseflag .eq. NO) .and. (mod(illum_nn, 3) .eq. 1) ) then
            output = output - ((300.- skint)/30.)*0.30 ! daytime
         end if
         if  (( lsflag .eq. 0_byte ) .AND. (niseflag .eq. NO) .and. (mod(illum_nn, 3) .eq. 2) ) then
            output = output - ((300.- skint)/30.)*0.35 ! twilight
         end if
         if  (( lsflag .eq. 0_byte ) .AND. (niseflag .eq. NO) .and. (mod(illum_nn, 3) .eq. 0) ) then
            output = output - ((300.- skint)/30.)*0.30 ! night
         end if
      end if

      ! --- ensure that CCCOT is within 0 - 1 range
      cccot_pre = max( min( output, 1.0 ), 0.0)

      ! --- get rid of fields
      deallocate(minmax_train)
      deallocate(inv)
      deallocate(outv)
      deallocate(input)
      deallocate(scales)

      ! now apply threshold and create BIT mask: 0=CLEAR, 1=CLOUDY
      if ( cccot_pre .gt. threshold_used ) then
         cldflag = CLOUDY
      else
         cldflag = CLEAR
      end if

      ! calculate Uncertainty with pre calculated calipso scores
      ! depending on normalized difference between cccot_pre and used threshold
      if ( cldflag .eq. CLEAR ) then
         norm_diff_cc_th = ( cccot_pre - threshold_used ) / threshold_used
         cld_uncertainty = ( CLEAR_UNC_MAX - CLEAR_UNC_MIN ) * norm_diff_cc_th + CLEAR_UNC_MAX
      elseif ( cldflag .eq. CLOUDY ) then
         !v2.0 power of 2
         !norm_diff_cc_th = ( cccot_pre - threshold_used ) / ( 1 - threshold_used )
         !cld_uncertainty = ( CLOUDY_UNC_MAX - CLOUDY_UNC_MIN ) * ( abs(norm_diff_cc_th -1) )**2 + CLOUDY_UNC_MIN
         !v3.0 power of 3
         norm_diff_cc_th = ( cccot_pre - threshold_used ) / ( 1 - threshold_used )
         cld_uncertainty = ( CLOUDY_UNC_MAX - CLOUDY_UNC_MIN ) * ( abs(norm_diff_cc_th -1) )**3 + CLOUDY_UNC_MIN
      else
         cld_uncertainty = sreal_fill_value
      end if

      ! noob equals 1 if one or more input parameter is not within trained range
      if (noob .eq. 1_lint) then
         ! give penalty; increase uncertainty because at least 1 ANN input parameter
         ! was not within trained range
         cld_uncertainty = cld_uncertainty * 1.1
         ! set cloudmask to fillvalue only if all channels are negative
         if (ch1 .lt. 0 .and. ch2 .lt. 0 .and. ch3b .lt. 0 .and. &
             ch4 .lt. 0 .and. ch5 .lt. 0) then
            cldflag = byte_fill_value
            cld_uncertainty = sreal_fill_value
         end if
      end if
   else
      cldflag = byte_fill_value
      cld_uncertainty = sreal_fill_value
   end if

   if (correct_w_unc) then
      ! do this at least for noaa12
      if (trim(adjustl(platform)) .eq. 'noaa12') then
         if ( (cldflag .eq. CLOUDY) .and. (cld_uncertainty .gt. 35.) ) cldflag = CLEAR
         !            if   (cld_uncertainty .gt. 35.) cldflag = byte_fill_value
      end if
   end if
   !------------------------------------------------------------------------
end subroutine ann_cloud_mask
!------------------------------------------------------------------------

!------------------------------------------------------------------------
subroutine ann_cloud_phase(channel1, channel2, channel3a, channel3b, &
    channel3b_ref, channel4, channel5, &
    solzen, satzen, niseflag, lsflag, desertflag, albedo1, albedo2, albedo3a, &
    cphcot, phase_flag, phase_uncertainty, &
    ch3a_on_avhrr_flag, sensor_name, platform, skint, verbose)
    !------------------------------------------------------------------------

   use constants_cloud_typing_pavolonis_m
   use common_constants_m
   use neural_net_constants_m

   implicit none

   integer(kind=sint) :: noob     !# of pixels out of bounds
   integer(kind=sint) :: nneurons !# of employed neurons
   integer(kind=sint) :: ninput   !# of input dimensions of ann

   real(kind=sreal), allocatable, dimension(:,:) :: inv, minmax_train, scales
   real(kind=sreal), allocatable, dimension(:)   :: input, outv

   real(kind=sreal)   :: output
   real(kind=sreal)   :: oscales(3)
   real(kind=sreal)   :: temperature, cutoff, bias_i, bias_h
   integer(kind=byte) :: illum_nn ! 0 = undefined, 1 = day, 2 = twilight, 3 = night

   ! INPUT from cloud_type subroutine (module cloud_typing_pavolonis.F90)
   character(len=*),   intent(in)  :: sensor_name
   character(len=*),   intent(in)  :: platform
   integer(kind=byte), intent(in)  :: lsflag, niseflag
   integer(kind=sint), intent(in)  :: ch3a_on_avhrr_flag
   real(kind=sreal),   intent(in)  :: solzen, satzen, albedo1, albedo2, albedo3a, skint
   real(kind=sreal),   intent(in)  :: channel1, channel2, channel3a, channel3b, channel3b_ref, channel4, channel5
   logical,            intent(in)  :: verbose, desertflag

   ! OUTPUT to cloud_type subroutine (module cloud_typing_pavolonis.F90)
   integer(kind=byte), intent(out) :: phase_flag
   real(kind=sreal),   intent(out) :: cphcot, phase_uncertainty

   ! LOCAL variables
   real(kind=sreal)   :: ch1, ch2, ref3a, ch3b, ref3b, ch4, ch5 , threshold_used, ch1_uc
   real(kind=sreal)   :: btd_ch4_ch5, btd_ch4_ch3b, mu0, norm_diff_cc_th
   logical            :: call_neural_net, do_ref3b_alb_corr, calc_true_refl
   integer(kind=byte) :: surface_flag

   calc_true_refl = .FALSE. ! this is .false. at the moment, change to .true. if
                             ! future ANN's are trained with true reflectances

   mu0 = cos ( solzen * d2r )

   surface_flag = lsflag ! land (1) or sea (0)
   if ( desertflag ) surface_flag = 2 ! desert
   if ( niseflag .eq. 1 .and. lsflag .eq. 0 ) surface_flag = 3 ! sea ice
   if ( niseflag .eq. 1 .and. lsflag .eq. 1 ) surface_flag = 4 ! snow

   ch1_uc = 0.

   if ( channel1 .eq. sreal_fill_value ) then
      ch1 = channel1
   else
      ch1 = channel1 * 100.
      ! correct channel reflectance over sea (sunglint) with albedo
      if ((lsflag .eq. 0_byte) .and. (niseflag .eq. NO) .and. (albedo1 .ge. 0.) .and. (correct_glint) ) then
         ch1_uc = ch1
         ch1 = max(ch1 - albedo1 * 100. / 2., 0.)
      end if
   end if

   if ( channel2 .eq. sreal_fill_value ) then
      if ( (trim(adjustl(sensor_name)) .eq. 'MODIS') .and. ch1 .gt. 50. ) then
         ch2 = ch1 * 1.02
      else
         ch2 = channel2
      end if
   else
      ch2 = channel2 * 100.
      ! correct channel reflectance over sea (sunglint) with albedo
      if ((lsflag .eq. 0_byte) .and. (niseflag .eq. NO) .and. (albedo2 .gt. 0.) .and. (correct_glint)) ch2 = max(ch2 - albedo2 * 100. / 2., 0.)
   end if

   ch3b = channel3b
   do_ref3b_alb_corr = .FALSE.

   if ( ch3a_on_avhrr_flag .ne. NO ) then
      if ( channel3a .eq. sreal_fill_value ) then
         ref3a = channel3a
         ref3b = channel3b_ref
      else
         ! We use a 1.6um trained ANN now
         ref3a = channel3a * 100.
         ref3b = channel3b_ref
         ! We can also use the Albedo correction of the 1.6um channel
         if ((lsflag .eq. 0_byte) .and. (niseflag .eq. NO) .and. (albedo3a .gt. 0.) .and. (correct_glint) ) ref3a = max(ref3a - min(albedo3a, 1.) * 100. * 0.55, 0.)
      end if
   else
      if ( channel3b_ref .eq. sreal_fill_value ) then
         ref3b = channel3b_ref
      else
         if (ch1_uc .ne. 0.) do_ref3b_alb_corr = .TRUE.
         ref3b = channel3b_ref*100.
      end if
   end if

   if ( do_ref3b_alb_corr .and. ch3b .gt. 290. ) ref3b = ref3b * ch1 / ch1_uc

   ch4 = channel4
   ch5 = channel5
   btd_ch4_ch5  = ch4 - ch5
   btd_ch4_ch5  = max(0.4, btd_ch4_ch5) ! it seems that often opaque ice clouds
                                        ! are set to liquid by the ANN if the BTD_ch4_ch5 is negative
   btd_ch4_ch3b = ch4 - ch3b

   ! call neural net unless solzen is negative
   call_neural_net = .TRUE.

   if ( ( solzen .ge. 0. ) .and. (solzen  .le. 84) ) then
      illum_nn = 1                                            ! use ANN with Ref3.7
      if  ( ch3a_on_avhrr_flag .eq. YES ) illum_nn = 7        ! use ANN with Ref1.6
   elseif ( (solzen  .gt. 84) .and. (solzen .le. 90) ) then
      illum_nn = 2
   elseif (solzen  .gt. 90)  then
      illum_nn = 3
   else
      illum_nn = 0
      ! if solzen is negative, do not call neural net
      call_neural_net = .FALSE.
   end if

   threshold_used = sreal_fill_value

   ! --- if day
   if ( illum_nn .eq. 1 ) then

      if ( calc_true_refl ) then
         ch1   = ch1/mu0
         ch2   = ch2/mu0
         ref3b = ref3b/mu0
      end if

      nneurons = nneurons_ex101  !set number of neurons
      ninput   = ninput_ex101    !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex101

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex101

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex101

      allocate(scales(ninput,2))
      scales = scales_ex101           !parameters to scale input?
      oscales = oscales_ex101         !parameters to scale output?
      temperature = temperature_ex101 !"temperature" for sigmoid function
      cutoff = cutoff_ex101
      bias_i = bias_i_ex101
      bias_h = bias_h_ex101

      ! input
      allocate(input(ninput+1))

      input(1) = ch1           ! ch1 600nm
      input(2) = ch2           ! ch2 800nm
      input(3) = ref3b         ! ch3b reflectance 3.7 µm
      input(4) = ch4           ! ch4 11 µm
      input(5) = ch5           ! ch5 12 µm
      input(6) = btd_ch4_ch5   ! 11-12 µm
      input(7) = surface_flag  ! surface_flag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_CPH_THRES_DAY_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_DAY_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_CPH_THRES_DAY_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_DAY_LAND_ICE
      end if

   elseif ( illum_nn .eq. 2 )  then

      ! NIGHT

      nneurons       = nneurons_ex103   !set number of neurons
      ninput         = ninput_ex103     !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex103

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex103

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex103

      allocate(scales(ninput,2))
      scales = scales_ex103               !parameters to scale input?
      oscales = oscales_ex103             !parameters to scale output?
      temperature = temperature_ex103     !"temperature" for sigmoid function
      cutoff = cutoff_ex103
      bias_i = bias_i_ex103
      bias_h = bias_h_ex103

      !input
      allocate(input(ninput+1))

      input(1) = ch3b          ! ch3b 3.7 µm
      input(2) = ch4           ! ch4 11 µm
      input(3) = btd_ch4_ch3b  ! 11-3.7 µm
      input(4) = ch5           ! ch5 12 µm
      input(5) = btd_ch4_ch5   ! 11-12 µm
      input(6) = surface_flag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_CPH_THRES_TWL_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_TWL_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_CPH_THRES_TWL_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_TWL_LAND_ICE
      end if

   elseif ( illum_nn .eq. 3 )  then

      ! NIGHT

      nneurons       = nneurons_ex102   !set number of neurons
      ninput         = ninput_ex102     !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex102

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex102

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex102

      allocate(scales(ninput,2))
      scales = scales_ex102               !parameters to scale input?
      oscales = oscales_ex102             !parameters to scale output?
      temperature = temperature_ex102     !"temperature" for sigmoid function
      cutoff = cutoff_ex102
      bias_i = bias_i_ex102
      bias_h = bias_h_ex102

      !input
      allocate(input(ninput+1))

      input(1) = ch3b          ! ch3b 3.7 µm
      input(2) = ch4           ! ch4 11 µm
      input(3) = btd_ch4_ch3b  ! 11-3.7 µm
      input(4) = ch5           ! ch5 12 µm
      input(5) = btd_ch4_ch5   ! 11-12 µm
      input(6) = surface_flag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_CPH_THRES_NIGHT_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_NIGHT_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_CPH_THRES_NIGHT_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_NIGHT_LAND_ICE
      end if

   elseif ( illum_nn .eq. 7 ) then

      if ( calc_true_refl ) then
         ch1   = ch1/mu0
         ch2   = ch2/mu0
         ref3a = ref3a/mu0
      end if

      nneurons = nneurons_ex104  !set number of neurons
      ninput   = ninput_ex104    !set number of input parameter for the neural network

      !ranges variables within training was performed
      allocate(minmax_train(ninput,2))
      minmax_train = minmax_train_ex104

      !"weights" for input
      allocate(inv(ninput+1,nneurons))
      inv = inv_ex104

      !"weights" for output
      allocate(outv(nneurons+1))
      outv = outv_ex104

      allocate(scales(ninput,2))
      scales = scales_ex104           !parameters to scale input?
      oscales = oscales_ex104         !parameters to scale output?
      temperature = temperature_ex104 !"temperature" for sigmoid function
      cutoff = cutoff_ex104
      bias_i = bias_i_ex104
      bias_h = bias_h_ex104

      ! input
      allocate(input(ninput+1))

      input(1) = ch1           ! ch1 600nm
      input(2) = ch2           ! ch2 800nm
      input(3) = ref3a         ! ch3a reflectance 1.6 µm
      input(4) = ch4           ! ch4 11 µm
      input(5) = ch5           ! ch5 12 µm
      input(6) = btd_ch4_ch5   ! 11-12 µm
      input(7) = surface_flag  ! surface_flag

      !set threshold
      if ( lsflag .eq. 0_byte ) then
         threshold_used = COT_CPH_THRES_DAY_SEA
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_DAY_SEA_ICE
      elseif ( lsflag .eq. 1_byte ) then
         threshold_used = COT_CPH_THRES_DAY_LAND
         if ( niseflag .eq. YES  ) threshold_used = COT_CPH_THRES_DAY_LAND_ICE
      end if

   else

      if (verbose) then
         !        write(*,*) "Solar zenith angle < 0 in neural_net_preproc"
      end if

      ! --- end of day/night if loop
   end if

   !this should never happen and is only the case if lsflag is neither 0 nor 1
   if ( threshold_used .eq. sreal_fill_value ) call_neural_net = .false.

   ! --- subroutine which carries out neural network computation

   if ( call_neural_net ) then

      call neural_net(nneurons, ninput, minmax_train, inv, outv, &
           input, scales, oscales, cutoff, bias_i, bias_h, &
           temperature, output, noob)

      ! --- correct for viewing angle effect - test phase for AVHRR
      if (correct_view) output = output - ( 1. / 12. * ( 1. / cos( satzen * d2r) - 1. ) )

      ! --- ensure that CCCOT is within 0 - 1 range
      cphcot = max( min( output, 1.0 ), 0.0)

      ! --- get rid of fields
      deallocate(minmax_train)
      deallocate(inv)
      deallocate(outv)
      deallocate(input)
      deallocate(scales)

      ! now apply threshold and create BIT mask: 1=LIQUID, 2=ICE
      if ( cphcot .gt. threshold_used ) then
         phase_flag = ICE
      else
         phase_flag = LIQUID
      end if

      ! calculate Uncertainty with pre calculated calipso scores
      ! depending on normalized difference between cccot_pre and used threshold
      if ( phase_flag .eq. LIQUID ) then
         norm_diff_cc_th = ( cphcot - threshold_used ) / threshold_used
         phase_uncertainty = ( LIQUID_UNC_MAX - LIQUID_UNC_MIN ) * norm_diff_cc_th + LIQUID_UNC_MAX
      elseif ( phase_flag .eq. ICE ) then
         norm_diff_cc_th = ( cphcot - threshold_used ) / ( threshold_used - 1 )
         phase_uncertainty = ( ICE_UNC_MAX - ICE_UNC_MIN ) * norm_diff_cc_th + ICE_UNC_MAX
      else
         phase_uncertainty = sreal_fill_value
      end if

      ! noob equals 1 if one or more input parameter is not within trained range
      if (noob .eq. 1_lint) then
         ! give penalty; increase uncertainty because at least 1 ANN input parameter
         ! was not within trained range
         phase_uncertainty = phase_uncertainty * 1.1
         if (ch1 .lt. 0 .and. ch2 .lt. 0 .and. ch3b .lt. 0 .and. &
             ch4 .lt. 0 .and. ch5 .lt. 0) then
            phase_flag = byte_fill_value
            phase_uncertainty = sreal_fill_value
         end if
      end if

   else

      phase_flag = byte_fill_value
      phase_uncertainty = sreal_fill_value

   end if

   !------------------------------------------------------------------------
end subroutine ann_cloud_phase
!------------------------------------------------------------------------


!------------------------------------------------------------------------
subroutine neural_net(nneurons, ninput, minmax_train, inv, outv, &
     input, scales, oscales, cutoff, bias_i, bias_h, &
     temperature, output, noob)
   !------------------------------------------------------------------------

   use constants_cloud_typing_pavolonis_m
   use common_constants_m
   use neural_net_constants_m

   implicit none

   integer(kind=sint) :: noob
   integer(kind=sint) :: iinput, ineuron
   integer(kind=sint) :: nneurons
   integer(kind=sint) :: ninput

   real(kind=sreal) :: minmax_train(ninput,2), scales(ninput,2), oscales(3)
   real(kind=sreal) :: inv(ninput+1,nneurons), outv(nneurons+1)
   real(kind=sreal), dimension(:), intent(inout) :: input
   real(kind=sreal) :: sigmoide
   real(kind=sreal) :: intermed(nneurons+1), vector_res1(nneurons), scalar_res2
   real(kind=sreal) :: temperature, bias_i, bias_h, cutoff

   logical :: lbounds

   real(kind=sreal), intent(out) :: output

   !check if pixel has values within training min/max and flag it
   !Just flag it make decision later stapel (09/2014)
   lbounds = .true.

   lbounds = all( (input(1:ninput) .ge. minmax_train(1:ninput,1)) .and. &
        ( input(1:ninput) .le. minmax_train(1:ninput,2) ) )

   if(lbounds) then
      noob = 0_lint
   else
      noob = 1_lint
      if (correct_bounds) then
         do iinput = 1, ninput
            input(iinput) = min(max(input(iinput), minmax_train(iinput,1)), minmax_train(iinput,2))
         end do
      end if
   end if

   !-----------------------------------------------------------------------

   !now do let ANN calculate no matter if input is
   !within bounds or not stapel (09/2014)
   do iinput = 1, ninput
      input(iinput) = scales(iinput,1)+scales(iinput,2)*(input(iinput) - &
           minmax_train(iinput,1))
   end do

   !apply constant to additional input element
   input(ninput+1) = bias_i

   !perform vector*matrix multiplication of input vector with
   !matrix of weights (ninput+1).(ninput+1,nneurons)=(nneurons)
   vector_res1 = matmul(input, inv)

   !apply sigmoidal function to each element of resulting vector vector_res1
   do ineuron = 1, nneurons
      call sigmoide_function(temperature/float(ninput), cutoff, &
           vector_res1(ineuron), sigmoide)
      intermed(ineuron) = sigmoide
   end do

   !extend intermediate result by one element
   intermed(nneurons+1) = bias_h

   !perform scalar product of intermediate result with output vector
   ! weights: (nneurons+1)*(nneurons+1)

   !resulting in a scalar
   scalar_res2 = dot_product(intermed, outv)

   !apply sigmoidal function to scalar result
   call sigmoide_function(temperature/float(nneurons), cutoff, scalar_res2, sigmoide)
   output = sigmoide

   !rescale output
   output = (output-oscales(1))/oscales(2)-oscales(3)

   !-------------------------------------------------------------------------
end subroutine neural_net
!-------------------------------------------------------------------------




!-------------------------------------------------------------------------
subroutine sigmoide_function(temperature, cutoff, input, sigmoide)
   !-------------------------------------------------------------------------

   !this functions evaluates the sigmoidal function
   !temperature and cutoff are constants coming from outside

   use common_constants_m

   implicit none

   real(kind=sreal) :: temperature, cutoff, input
   real(kind=sreal) :: sigmoidein, sigmoidem, sigmoide

   sigmoidein = temperature*input

   !ifs for cutoff
   if(sigmoidein .gt. cutoff) sigmoidein = cutoff
   if(sigmoidein .lt. -1.0*cutoff) sigmoidein = -1.0*cutoff

   sigmoidem = -1.0*sigmoidein
   sigmoide = 1.0/(1.0+exp(sigmoidem))

   !-------------------------------------------------------------------------
end subroutine sigmoide_function
!-------------------------------------------------------------------------



!=========================================================================
end module neural_net_preproc_m
!=========================================================================
