!-------------------------------------------------------------------------------
! Name:
!    neural_net_preproc.F90
!
! Purpose:
!    Neural network based cloud mask, applied in preprocessing. Input data
!     are satellite radiances and auxiliary data.
!
! History:
!    23th Oct 2014, SteSta + OS: Original version.
!     4th Nov 2014, SteSta + OS: implemented new cloud mask version, which is now
!         also available for twilight and additionally uses ECMWF skin 
!         temperature and flags for snow/ice and land/sea 
!
! Bugs:
!    None known
!-------------------------------------------------------------------------------

!=========================================================================
module NEURAL_NET_PREPROC
  !=========================================================================

  implicit none

contains

  !------------------------------------------------------------------------
  subroutine ann_cloud_mask(ch1, ch2, ch3b, ch4, ch5,  &
       & btd_ch4_ch5, btd_ch4_ch3b, solzen, dem, niseflag, lsflag, &
       & lusflag, sfctype, cccot_pre, cldflag, lat, skint, verbose)
    !------------------------------------------------------------------------

    use constants_cloud_typing_pavolonis
    use common_constants
    use neural_net_constants

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
    integer(kind=byte), intent(in) :: lsflag, lusflag, niseflag
    integer(kind=sint), intent(in) :: sfctype
    integer(kind=lint), intent(in) :: dem
    real(kind=sreal),   intent(in) :: solzen, lat, skint
    real(kind=sreal),   intent(in) :: ch1, ch2, ch3b, ch4, ch5
    real(kind=sreal),   intent(in) :: btd_ch4_ch5, btd_ch4_ch3b
    logical,            intent(in) :: verbose

    ! OUTPUT to cloud_type subroutine (module cloud_typing_pavolonis.F90)
    integer(kind=byte), intent(out) :: cldflag
    real(kind=sreal),   intent(out) :: cccot_pre

    if ( ( solzen .gt. 0 ) .and. (solzen  .lt. 80) ) then
       illum_nn = 1
    elseif (solzen  .ge. 80. .and. solzen .lt. 95.)  then       
       illum_nn = 2
    elseif (solzen  .ge. 95.) then
       illum_nn = 3
    else
       illum_nn = 0
    endif

    ! --- if day
    if ( illum_nn .eq. 1 ) then

       nneurons = nneurons_ex3 		!set number of neurons
       ninput   = ninput_ex3    	!set number of input parameter for the neural network

       !ranges variables within training was performed
       allocate(minmax_train(ninput,2))
       minmax_train=minmax_train_ex3  

       !"weights" for input
       allocate(inv(ninput+1,nneurons))
       inv=inv_ex3

       !"weights" for output
       allocate(outv(nneurons+1))
       outv=outv_ex3

       allocate(scales(ninput,2))
       scales=scales_ex3 		!parameters to scale input?
       oscales=oscales_ex3 		!parameters to scale output?
       temperature=temperature_ex3	!"temperature" for sigmoid function
       cutoff=cutoff_ex3
       bias_i=bias_i_ex3
       bias_h=bias_h_ex3

       ! input
       allocate(input(ninput+1)) 
       input(1) = ch1 *100.	! ch1 600nm
       input(2) = ch2 *100.	! ch2 800nm
       input(3) = ch4 	        ! ch4 11 µm
       input(4) = ch5 	        ! ch5 12 µm
       input(5) = btd_ch4_ch5   ! 11-12 µm
       input(6) = skint         ! ERA-Interim skin temperature
       input(7) = niseflag      ! snow/ice information
       input(8) = lsflag        ! land/sea flag

    elseif ( illum_nn .eq. 2 )  then       

       ! TWILIGHT

       nneurons       = nneurons_ex4 	!set number of neurons
       ninput         = ninput_ex4 	!set number of input parameter for the neural network

       !ranges variables within training was performed
       allocate(minmax_train(ninput,2))
       minmax_train=minmax_train_ex4

       !"weights" for input
       allocate(inv(ninput+1,nneurons))
       inv=inv_ex4

       !"weights" for output
       allocate(outv(nneurons+1))
       outv=outv_ex4

       allocate(scales(ninput,2))
       scales=scales_ex4 		!parameters to scale input?
       oscales=oscales_ex4		!parameters to scale output?
       temperature=temperature_ex4	!"temperature" for sigmoid function
       cutoff=cutoff_ex4
       bias_i=bias_i_ex4
       bias_h=bias_h_ex4

       !input
       allocate(input(ninput+1)) 
       input(1) = ch4 	        ! ch4 11 µm
       input(2) = ch5 	        ! ch5 12 µm
       input(3) = btd_ch4_ch5 	! 11-12 µm
       input(4) = skint
       input(5) = niseflag
       input(6) = lsflag

    elseif ( illum_nn .eq. 3 ) then

       ! --- night

       nneurons       = nneurons_ex5 	!set number of neurons
       ninput         = ninput_ex5 	!set number of input parameter for the neural network

       !ranges variables within training was performed
       allocate(minmax_train(ninput,2))
       minmax_train=minmax_train_ex5

       !"weights" for input
       allocate(inv(ninput+1,nneurons))
       inv=inv_ex5

       !"weights" for output
       allocate(outv(nneurons+1))
       outv=outv_ex5

       allocate(scales(ninput,2))
       scales=scales_ex5 		!parameters to scale input?
       oscales=oscales_ex5		!parameters to scale output?
       temperature=temperature_ex5	!"temperature" for sigmoid function
       cutoff=cutoff_ex5
       bias_i=bias_i_ex5
       bias_h=bias_h_ex5

       !input
       allocate(input(ninput+1)) 
       input(1) = ch3b 	        ! ch3b 3.7µm
       input(2) = ch4 	        ! ch4 11 µm
       input(3) = ch5 	        ! ch5 12 µm
       input(4) = btd_ch4_ch3b	! 11-3.7 µm
       input(5) = btd_ch4_ch5 	! 11-12 µm
       input(6) = skint
       input(7) = niseflag
       input(8) = lsflag

    else

       if (verbose) write(*,*) "Solar zenith angle < 0 in neural_net_preproc"

       ! --- end of day/night if loop
    endif


    ! --- subroutine which carries out neural network computation
    call neural_net(nneurons,ninput,minmax_train,inv,outv, &
         & input,scales,oscales,cutoff,bias_i,bias_h,     &
         & temperature,output,noob)

    ! --- ensure that CCCOT is within 0 - 1 range
    cccot_pre = max( min( output, 1.0 ), 0.0)

    ! --- get rid of fields
    deallocate(minmax_train)
    deallocate(inv)
    deallocate(outv)
    deallocate(input)
    deallocate(scales)

    ! now create BIT mask: 0=CLEAR, 1=CLOUDY, fill_value=unknown

    ! apply sea threshold
    if ( lsflag .eq. 0_byte ) then

       if ( cccot_pre .gt. sym%COT_THRES_SEA ) then
          cldflag = sym%CLOUDY
       else
          cldflag = sym%CLEAR
       endif

       ! apply land threshold
    elseif ( lsflag .eq. 1_byte ) then

       if( cccot_pre .gt. sym%COT_THRES_LAND ) then
          cldflag = sym%CLOUDY
       else
          cldflag = sym%CLEAR
       endif

    endif

    ! here we go. 
    ! What are we doing if at least 1 input parameter is not in trained range
    ! , e.g. fillvalue ?
    ! For now 6 cases are defined to deal with it, choose best one later
    ! noob equals 1 if one or more input parameter is not within trained range 
    if (noob .eq. 1_lint) then

       ! Case 1) trust the ann and ... 
       !just do nothing
       ! Case 2) set it to clear
       !imager_pavolonis%CCCOT_pre(i,j)= sreal_fill_value
       !imager_pavolonis%CLDMASK(i,j)=sym%CLEAR
       ! Case 3) set it to cloudy
       !imager_pavolonis%CCCOT_pre(i,j)= 1.0
       !imager_pavolonis%CLDMASK(i,j)=sym%CLOUDY
       ! Case 4) set it to fillvalue
       !imager_pavolonis%CCCOT_pre(i,j)=sreal_fill_value
       !imager_pavolonis%CLDMASK(i,j)=sint_fill_value
       ! Case 5) only during nighttime! set it to cloudy if 3.7µm is fillvalue (saturated) 
       !but 11µm is below 230 K; cloud holes; fixes at least avhrr, dont know about aatsr 
       !if ( (solzen > 80) .and. (ch3b .lt. 0) .and. &
       !   & (ch4 .gt. 100) .and. (ch4 .lt. 230) ) then
       !  cccot_pre   = 1.0
       !  cldflag = sym%CLOUDY
       !else
       !  cccot_pre   = sreal_fill_value
       !  cldflag = sint_fill_value
       !endif
       ! Case 6) trust ann, set cldflag to fillvalue only if all channels are
       !  below 0. (=fillvalue)

       if (ch1 .lt. 0 .and. ch2 .lt. 0 .and. ch3b .lt. 0 .and. ch4 .lt. 0 &
            & .and. ch5 .lt. 0) cldflag = byte_fill_value
       if (lat .lt. -65. .AND. lat .gt. -90. .AND. lsflag .eq. 1 .AND. &
            & niseflag .eq. 1 .AND. illum_nn .eq. 3) &
            & cldflag = byte_fill_value ! for cold land surfaces (Antarctica)
       !  ch3b saturates and NN by default masks all pixels as cloudy; here
       !  set to fill value because no information available

       ! end of noob if loop
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
