real function sigmoide_old(temperature,cutoff,input)

  implicit none

  real(kind=sreal) :: temperature,cutoff,input

  sigmoide=temperature*input

  !ifs for cutoff
  if(sigmoide .gt. cutoff) sigmoide=cutoff
  if(sigmoide .lt. -cutoff) sigmoide=-cutoff

  sigmoide=1.0/(1.0+exp(sigmoide)) !performance?

end function sigmoide_old
