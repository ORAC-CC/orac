!-------------------------------------------------------------------------------
! Name:
!    neural_net.F90
!
! Purpose:
!    Neural network based cloud mask, applied in postprocessing. Input data
!     are retrieval output values.
!
! History:
!    20th Nov 2914, SteSta + OS: implemented bounds check (noob)
!    2014/12/03 CP added in common_constants should eventually remove postproc_constants
!    2015/02/05 OS changed nint to lint
!
! $Id$
!
! Bugs:
!    None known
!-------------------------------------------------------------------------------

subroutine neural_net(nneurons,ninput,noutput,minmax_train,inv,outv,input,scales,oscales,&
     & cutoff,bias_i,bias_h,temperature,output,noob)

  use common_constants
  use postproc_constants

  !use neural_net_constants

  implicit none

  integer(kind=lint) :: noob !number of pixels out of bounds

  integer(kind=lint) :: iinput,ineuron
  integer(kind=lint) :: nneurons !number of employed neurons
  integer(kind=lint) :: ninput !number of criterias (input dimensions of nn)
  integer(kind=lint) :: noutput !number of output dimensions (1 as we only have cm)

  real(kind=sreal) :: minmax_train(ninput,2),scales(ninput,2),oscales(3),&
       & inv(ninput+1,nneurons),input(ninput+1),outv(nneurons+1),output

  real(kind=sreal) :: sigmoide !sigmoide function, s.b.

  real(kind=sreal) :: intermed(nneurons+1),vector_res1(nneurons),scalar_res2
  real(kind=sreal) ::temperature,bias_i,bias_h,cutoff

  logical :: lbounds

  !check if pixel has values within training min/max
  !if yes do actual cloudmasking
  !if no set pixel to cloud free and do nothing.
  lbounds=.true.

  lbounds=all( (input(1:ninput) .ge. minmax_train(1:ninput,1)) .and. &
       & (input(1:ninput) .le. minmax_train(1:ninput,2)))

  if(lbounds) then
     noob=0_lint
  else
     noob=1_lint
  endif

  !rescale input
  do iinput=1,ninput
     input(iinput)=scales(iinput,1)+scales(iinput,2)*(input(iinput)-minmax_train(iinput,1))
  enddo
  !apply constant to additional input element
  input(ninput+1)=bias_i

  !perform vector*matrix multiplication of input vector with matrix of weights 
  !(ninput+1).(ninput+1,nneurons)=(nneurons)
  vector_res1=matmul(input,inv)
  
  !apply sigmoidal function to each element of resultinf vector vector_res1
  do ineuron=1,nneurons
     call sigmoide_function(temperature/float(ninput),cutoff,vector_res1(ineuron),sigmoide)
     intermed(ineuron)=sigmoide
  enddo
  !extend intermediate result by one element
  intermed(nneurons+1)=bias_h  

  !perform scalar product of intermediate result with output vector weights:
  !(nneurons+1)*(nneurons+1)
  !resulting in a scalar 
  scalar_res2=dot_product(intermed,outv)

  !apply sigmoidal function to scalar result 
  call sigmoide_function(temperature/float(nneurons),cutoff,scalar_res2,sigmoide)
  output=sigmoide

  !rescale output 
  output=(output-oscales(1))/oscales(2)-oscales(3)
  !write(*,*) 'output_scaled',output
  !else
  !write(*,*) 'in lbounds=.false. part'     
  !if values not in training ranges set output to fill value (meaning this pixel will be cloud free later)
  !and increment counter by one.
  !   noob=noob+1_lint
  !   output=real_fill_value
  !endif

end subroutine neural_net





!#######################################
!#######################################
subroutine sigmoide_function(temperature,cutoff,input,sigmoide)
!#######################################
!#######################################

  !this functions evaluates the sigmoidal function
  !temperature and cutoff are constants coming from outside

  use postproc_constants
  use common_constants

  implicit none

  real(kind=sreal) :: temperature,cutoff,input,sigmoidein,sigmoidem,sigmoide

  sigmoidein=temperature*input

  !ifs for cutoff
  if(sigmoidein .gt. cutoff) sigmoidein=cutoff
  if(sigmoidein .lt. -1.0*cutoff) sigmoidein=-1.0*cutoff
  sigmoidem=-1.0*sigmoidein
  sigmoide=1.0/(1.0+exp(sigmoidem)) 
!  write(*,*) 'inside sigmoide',sigmoidem,sigmoide

end subroutine sigmoide_function









