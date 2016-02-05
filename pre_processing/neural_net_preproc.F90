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
! 2015/10/29, CP: functionality for ATSR2 added added SteSta NN calibration corrections
! 2015/11/17, OS: added platform flag and correction coefficients for MODIS and AATSR;
!    removed sunglint double check; minor editing
! 2015/12/17, OS: changed structure of setting thresholds
! 2016/01/21, OS: Added correction for ice-free sea skin temperature - to be tested
! 2016/01/21, OS: Removed offset when correcting AATSR ch1 and ch2 data
! 2016/02/05, OS: Cloud mask now uses albedo for glint correction.

! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!=========================================================================
module NEURAL_NET_PREPROC
  !=========================================================================

  implicit none

contains

  !------------------------------------------------------------------------
  subroutine ann_cloud_mask(channel1, channel2, channel3b, channel4, channel5, &
       & solzen, satzen, dem, niseflag, lsflag, &
       & lusflag, albedo1, albedo2, cccot_pre, cldflag, cld_uncertainty, lat, skint, &
       & ch3a_on_avhrr_flag, i, j, glint_angle, sensor_name, platform, verbose)
    !------------------------------------------------------------------------

    use constants_cloud_typing_pavolonis
    use common_constants
    use neural_net_constants

    implicit none

    integer(kind=sint) :: noob     !# of pixels out of bounds
    integer(kind=sint) :: nneurons !# of employed neurons
    integer(kind=sint) :: ninput   !# of input dimensions of ann

    real(kind=sreal),allocatable, dimension(:,:) :: inv,minmax_train,scales
    real(kind=sreal),allocatable, dimension(:)   :: input,outv

    real(kind=sreal) :: output
    real(kind=sreal) :: oscales(3)
    real(kind=sreal) :: temperature,cutoff,bias_i,bias_h
    integer(kind=byte) :: illum_nn ! 0 = undefined, 1 = day, 2 = twilight,
    ! 3 = night

    ! INPUT from cloud_type subroutine (module cloud_typing_pavolonis.F90)
    character(len=sensor_length),   intent(in)    :: sensor_name
    character(len=platform_length), intent(in)    :: platform
    integer(kind=byte), intent(in) :: lsflag, lusflag, niseflag
    integer(kind=sint), intent(in) :: ch3a_on_avhrr_flag
    integer(kind=lint), intent(in) :: dem
    real(kind=sreal),   intent(in) :: solzen, lat, skint, satzen, glint_angle, albedo1, albedo2
    real(kind=sreal),   intent(in) :: channel1, channel2, channel3b, channel4, channel5
    integer(kind=lint), intent(in) :: i, j
    logical,            intent(in) :: verbose

    ! OUTPUT to cloud_type subroutine (module cloud_typing_pavolonis.F90)
    integer(kind=byte), intent(out) :: cldflag
    real(kind=sreal),   intent(out) :: cccot_pre
    real(kind=sreal),   intent(out) :: cld_uncertainty

    ! LOCAL variables
    real(kind=sreal)   :: ch1, ch2, ch3b, ch4, ch5 , threshold_used
    real(kind=sreal)   :: btd_ch4_ch5, btd_ch4_ch3b, norm_diff_cc_th
    integer(kind=sint) :: glint_mask
    logical            :: call_neural_net

    if ( glint_angle .lt. 40.0 .and. glint_angle .ne. sreal_fill_value ) then
       glint_mask = YES
    else
       glint_mask = NO
    endif

    if ( channel1 .eq. sreal_fill_value ) then
       ch1 = channel1
    else
       ch1 = channel1
       if ((lsflag .eq. 0_byte) .and. (niseflag .eq. NO) .and. (albedo1 .ge. 0. .and. albedo1 .lt. 0.8)) ch1 = max(ch1 - albedo1 / 2., 0.)
       ch1 = ch1 * 100.
       if (trim(adjustl(sensor_name)) .eq. 'MODIS' ) ch1 = 0.8945 * ch1 + 2.217
       if (trim(adjustl(sensor_name)) .eq. 'AATSR' ) ch1 = 0.8542 * ch1
       ch1 = min(106.,ch1) ! Dont allow reflectance to be higher than trained
    endif

    if ( channel2 .eq. sreal_fill_value ) then
       if ( (trim(adjustl(sensor_name)) .eq. 'MODIS') .and. ch1 .gt. 50. ) then
          ch2 = min(104., ch1 * 1.03)
       else
          ch2 = channel2
       endif
    else
       ch2 = channel2 
       if ((lsflag .eq. 0_byte) .and. (niseflag .eq. NO) .and. (albedo2 .gt. 0. .and. albedo2 .lt. 0.8)) ch2 = max(ch2 - albedo2 / 2., 0.)
       ch2 = ch2 * 100.
       if (trim(adjustl(sensor_name)) .eq. 'MODIS' ) ch2 = 0.8336 * ch2 + 1.749
       if (trim(adjustl(sensor_name)) .eq. 'AATSR' ) ch2 = 0.7787 * ch2
       ch2 = min(104.,ch2) ! Dont allow reflectance to be higher than trained
    endif

    if ( channel3b .eq. sreal_fill_value ) then
       ch3b = channel3b
    else
       ch3b = channel3b
       if (trim(adjustl(sensor_name)) .eq. 'MODIS' ) ch3b = 0.9944 * ch3b + 1.152
       if (trim(adjustl(sensor_name)) .eq. 'AATSR' ) ch3b = 1.0626 * ch3b - 15.777
    endif

    if ( channel4 .eq. sreal_fill_value ) then
       ch4 = channel4
    else
       ch4 = channel4
       if (trim(adjustl(sensor_name)) .eq. 'MODIS' ) ch4 = 0.9742 * ch4 + 7.205
       if (trim(adjustl(sensor_name)) .eq. 'AATSR' ) ch4 = 0.9793 * ch4 + 5.366
    endif

    if ( channel5 .eq. sreal_fill_value ) then
       ch5 = channel5
    else
       ch5 = channel5
       if (trim(adjustl(sensor_name)) .eq. 'MODIS' ) ch5 = 0.9676 * ch5 + 8.408
       if (trim(adjustl(sensor_name)) .eq. 'AATSR' ) ch5 = 0.9838 * ch5 + 4.255
       ! use following instead of previous line if AATSR channel 5 (12 um)...
       ! ...is NOT "non-linearity" corrected
       ! if (trim(adjustl(sensor_name)) .eq. 'AATSR' ) ch5 = 0.9901 * ch5 + 2.568
    endif

    btd_ch4_ch5  = ch4 - ch5
    btd_ch4_ch3b = ch4 - ch3b

    ! call neural net unless solzen is negative
    call_neural_net = .TRUE.

    if ( ( solzen .ge. 0. ) .and. (solzen  .le. 80) ) then
       illum_nn = 1
    elseif ( (solzen  .gt. 80) .and. (solzen .le. 90) ) then
       illum_nn = 2
    elseif (solzen  .gt. 90)  then
       illum_nn = 3
       ! use twilight net if ch3b is missing at night/twilight:
       if ( ch3a_on_avhrr_flag .ne. NO ) illum_nn = 2
       !       if ( ( ch3b .lt. NOAA7_9_CH3B_BT_THRES ) .and. ( (trim(adjustl(platform)) .eq. 'noaa7') &
       !            .or. (trim(adjustl(platform)) .eq. 'noaa9') ) ) then
       !          illum_nn = 2
       !       endif
    else
       illum_nn = 0
       ! if solzen is negative, do not call neural net
       call_neural_net = .FALSE.
    endif

    threshold_used = sreal_fill_value

    ! --- if day
    if ( illum_nn .eq. 1 ) then

       nneurons = nneurons_ex9  !set number of neurons
       ninput   = ninput_ex9    !set number of input parameter for the neural network

       !ranges variables within training was performed
       allocate(minmax_train(ninput,2))
       minmax_train=minmax_train_ex9

       !"weights" for input
       allocate(inv(ninput+1,nneurons))
       inv=inv_ex9

       !"weights" for output
       allocate(outv(nneurons+1))
       outv=outv_ex9

       allocate(scales(ninput,2))
       scales=scales_ex9           !parameters to scale input?
       oscales=oscales_ex9         !parameters to scale output?
       temperature=temperature_ex9 !"temperature" for sigmoid function
       cutoff=cutoff_ex9
       bias_i=bias_i_ex9
       bias_h=bias_h_ex9

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
       endif

    elseif ( illum_nn .eq. 2 )  then

       ! TWILIGHT

       nneurons       = nneurons_ex10   !set number of neurons
       ninput         = ninput_ex10     !set number of input parameter for the neural network

       !ranges variables within training was performed
       allocate(minmax_train(ninput,2))
       minmax_train=minmax_train_ex10

       !"weights" for input
       allocate(inv(ninput+1,nneurons))
       inv=inv_ex10

       !"weights" for output
       allocate(outv(nneurons+1))
       outv=outv_ex10

       allocate(scales(ninput,2))
       scales=scales_ex10               !parameters to scale input?
       oscales=oscales_ex10             !parameters to scale output?
       temperature=temperature_ex10     !"temperature" for sigmoid function
       cutoff=cutoff_ex10
       bias_i=bias_i_ex10
       bias_h=bias_h_ex10

       !input
       allocate(input(ninput+1))

       input(1) = ch4           ! ch4 11 µm
       input(2) = ch5           ! ch5 12 µm
       input(3) = btd_ch4_ch5   ! 11-12 µm
       input(4) = skint
       ! exclude negative skin-rad4 temperatures at night/twilight, this needs to be tested!!
       if ( ( skint - ch4 ) .lt. 0 ) input(4) = ch4
       input(5) = niseflag
       input(6) = lsflag

       !set threshold
       if ( lsflag .eq. 0_byte ) then
          threshold_used = COT_THRES_TWL_SEA
          if ( niseflag .eq. YES  ) threshold_used = COT_THRES_TWL_SEA_ICE
       elseif ( lsflag .eq. 1_byte ) then
          threshold_used = COT_THRES_TWL_LAND
          if ( niseflag .eq. YES  ) threshold_used = COT_THRES_TWL_LAND_ICE
       endif

    elseif ( illum_nn .eq. 3 ) then

       ! --- night

       nneurons       = nneurons_ex11   !set number of neurons
       ninput         = ninput_ex11     !set number of input parameter for the neural network

       !ranges variables within training was performed
       allocate(minmax_train(ninput,2))
       minmax_train=minmax_train_ex11

       !"weights" for input
       allocate(inv(ninput+1,nneurons))
       inv=inv_ex11

       !"weights" for output
       allocate(outv(nneurons+1))
       outv=outv_ex11

       allocate(scales(ninput,2))
       scales=scales_ex11               !parameters to scale input?
       oscales=oscales_ex11             !parameters to scale output?
       temperature=temperature_ex11     !"temperature" for sigmoid function
       cutoff=cutoff_ex11
       bias_i=bias_i_ex11
       bias_h=bias_h_ex11

       !input
       allocate(input(ninput+1))
       input(1) = ch3b          ! ch3b 3.7µm
       input(2) = ch4           ! ch4 11 µm
       input(3) = ch5           ! ch5 12 µm
       input(4) = btd_ch4_ch3b  ! 11-3.7 µm
       input(5) = btd_ch4_ch5   ! 11-12 µm
       input(6) = skint
       ! exclude negative skin-rad4 temperatures at night/twilight, this needs to be tested!!
       if ( ( skint - ch4 ) .lt. 0 ) input(6) = ch4
       input(7) = niseflag
       input(8) = lsflag

       !set threshold
       if ( lsflag .eq. 0_byte ) then
          threshold_used = COT_THRES_NIGHT_SEA
          if ( niseflag .eq. YES  ) threshold_used = COT_THRES_NIGHT_SEA_ICE
       elseif ( lsflag .eq. 1_byte ) then
          threshold_used = COT_THRES_NIGHT_LAND
          if ( niseflag .eq. YES  ) threshold_used = COT_THRES_NIGHT_LAND_ICE
       endif

    else

       if (verbose) then
          write(*,*) "Solar zenith angle < 0 in neural_net_preproc"
       endif

       ! --- end of day/night if loop
    endif

    !this should never happen and is only the case if lsflag is neither 0 nor 1
    if ( threshold_used .eq. sreal_fill_value ) call_neural_net=.false. 

    ! --- subroutine which carries out neural network computation

    if ( call_neural_net ) then

       call neural_net(nneurons,ninput,minmax_train,inv,outv, &
            & input,scales,oscales,cutoff,bias_i,bias_h,     &
            & temperature,output,noob)

       ! --- correct for viewing angle effect - test phase for AVHRR
       output = output - ( 1. / 12. * ( 1. / cos( satzen * d2r) - 1. ) )

       ! --- correct for sunglint - test phase for AVHRR
       ! This probably needs to be done in future because the sunglint correction does not
       ! really work for AATSR ; but at the moment wait for the results
       !       if ( (trim(adjustl(sensor_name)) .eq. 'MODIS' ) .or. (trim(adjustl(sensor_name)) .eq. 'AVHRR') ) then
       !       output = output + min( 0., (1./6. * ( (glint_angle / 50. )**2. -1.) ) )
       !       if ((lsflag .eq. 0_byte) .and. (niseflag .eq. NO)) output = output - albedo(1)
       !       output = output + min( 0., (1./6. * ( (glint_angle / 25. ) -1.) ) )

       !       endif

       ! Testing ice-free sea skin temperature correction
       if  (( lsflag .eq. 0_byte ) .AND. (niseflag .eq. NO)) then
          output = output - ((300.- skint)/30.)*0.15
       endif

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
       endif


       ! calculate Uncertainty with pre calculated calipso scores
       ! depending on normalized difference between cccot_pre and used threshold
       if ( cldflag .eq. CLEAR ) then
          norm_diff_cc_th = ( cccot_pre - threshold_used ) / threshold_used
          cld_uncertainty = ( CLEAR_UNC_MAX - CLEAR_UNC_MIN ) * norm_diff_cc_th + CLEAR_UNC_MAX
       elseif ( cldflag .eq. CLOUDY ) then
          norm_diff_cc_th = ( cccot_pre - threshold_used ) / ( 1 - threshold_used )
          cld_uncertainty = ( CLOUDY_UNC_MAX - CLOUDY_UNC_MIN ) * (norm_diff_cc_th -1 )**2 + CLOUDY_UNC_MIN
       else
          cld_uncertainty = sreal_fill_value
       endif

       ! here we go.
       ! What are we doing if at least 1 input parameter is not in trained range
       ! , e.g. fillvalue ?
       ! For now 5 cases are defined to deal with it, choose best one later
       ! noob equals 1 if one or more input parameter is not within trained range
       if (noob .eq. 1_lint) then
          ! give penalty; increase uncertainty because at least 1 ANN input parameter
          ! was not within trained range
          cld_uncertainty = cld_uncertainty * 1.05

          ! Case 1) trust the ann and ...
          !just do nothing
          ! Case 2) set it to clear
          !imager_pavolonis%CCCOT_pre(i,j)= sreal_fill_value
          !imager_pavolonis%CLDMASK(i,j)=CLEAR
          ! Case 3) set it to cloudy
          !imager_pavolonis%CCCOT_pre(i,j)= 1.0
          !imager_pavolonis%CLDMASK(i,j)=CLOUDY
          ! Case 4) set it to fillvalue
          !imager_pavolonis%CCCOT_pre(i,j)=sreal_fill_value
          !imager_pavolonis%CLDMASK(i,j)=sint_fill_value
          ! Case 5) trust ann, set cldflag to fillvalue only if all channels are
          !         below 0. (=fillvalue)
          if (ch1 .lt. 0 .and. ch2 .lt. 0 .and. ch3b .lt. 0 .and. ch4 .lt. 0 &
               & .and. ch5 .lt. 0) then
             cldflag = byte_fill_value
             cld_uncertainty = sreal_fill_value
          endif
          ! end of noob if-loop
       endif

    else

       cldflag = byte_fill_value
       cld_uncertainty = sreal_fill_value

    endif

    !------------------------------------------------------------------------
  end subroutine ann_cloud_mask
  !------------------------------------------------------------------------


  !------------------------------------------------------------------------
  subroutine neural_net(nneurons,ninput,minmax_train,inv,outv, &
       & input,scales,oscales,cutoff,bias_i,bias_h,&
       & temperature,output,noob)
    !------------------------------------------------------------------------

    use common_constants
    use neural_net_constants

    implicit none

    integer(kind=sint) :: noob
    integer(kind=sint) :: iinput,ineuron
    integer(kind=sint) :: nneurons
    integer(kind=sint) :: ninput

    real(kind=sreal) :: minmax_train(ninput,2),scales(ninput,2),oscales(3),&
         & inv(ninput+1,nneurons),outv(nneurons+1)
    real(kind=sreal),dimension(:),intent(inout) :: input
    real(kind=sreal) :: sigmoide
    real(kind=sreal) :: intermed(nneurons+1),vector_res1(nneurons),scalar_res2
    real(kind=sreal) ::temperature,bias_i,bias_h,cutoff

    logical :: lbounds

    real(kind=sreal),intent(out) :: output

    !check if pixel has values within training min/max and flag it
    !Just flag it make decision later stapel (09/2014)
    lbounds=.true.

    lbounds=all( (input(1:ninput) .ge. minmax_train(1:ninput,1)) .and. &
         & ( input(1:ninput) .le. minmax_train(1:ninput,2) ) )

    if(lbounds) then
       noob=0_lint
    else
       noob=1_lint
    endif

    !-----------------------------------------------------------------------

    !now do let ANN calculate no matter if input is
    !within bounds or not stapel (09/2014)
    do iinput=1,ninput
       input(iinput)=scales(iinput,1)+scales(iinput,2)*(input(iinput) &
            & -minmax_train(iinput,1))
    enddo

    !apply constant to additional input element
    input(ninput+1)=bias_i

    !perform vector*matrix multiplication of input vector with
    !matrix of weights (ninput+1).(ninput+1,nneurons)=(nneurons)
    vector_res1=matmul(input,inv)

    !apply sigmoidal function to each element of resultinf vector vector_res1
    do ineuron=1,nneurons
       call sigmoide_function(temperature/float(ninput),cutoff &
            & ,vector_res1(ineuron),sigmoide)
       intermed(ineuron)=sigmoide
    enddo

    !extend intermediate result by one element
    intermed(nneurons+1)=bias_h

    !perform scalar product of intermediate result with output vector
    ! weights: (nneurons+1)*(nneurons+1)

    !resulting in a scalar
    scalar_res2=dot_product(intermed,outv)

    !apply sigmoidal function to scalar result
    call sigmoide_function(temperature/float(nneurons),cutoff,scalar_res2,sigmoide)
    output=sigmoide

    !rescale output
    output=(output-oscales(1))/oscales(2)-oscales(3)

    !-------------------------------------------------------------------------
  end subroutine neural_net
  !-------------------------------------------------------------------------




  !-------------------------------------------------------------------------
  subroutine sigmoide_function(temperature,cutoff,input,sigmoide)
    !-------------------------------------------------------------------------

    !this functions evaluates the sigmoidal function
    !temperature and cutoff are constants coming from outside

    use common_constants

    implicit none

    real(kind=sreal) :: temperature,cutoff,input
    real(kind=sreal) :: sigmoidein,sigmoidem,sigmoide

    sigmoidein=temperature*input

    !ifs for cutoff
    if(sigmoidein .gt. cutoff) sigmoidein=cutoff
    if(sigmoidein .lt. -1.0*cutoff) sigmoidein=-1.0*cutoff

    sigmoidem=-1.0*sigmoidein
    sigmoide=1.0/(1.0+exp(sigmoidem))

    !-------------------------------------------------------------------------
  end subroutine sigmoide_function
  !-------------------------------------------------------------------------



  !=========================================================================
end module NEURAL_NET_PREPROC
!=========================================================================
