! Name: extrap_into_tropopause.f90
!
!
! Purpose:
! find troppuaes given temperature and height profile
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!	T0	Temperature profile / K
;	Z0	Corresponding altitudes / km
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/05/24:Original code C. Poulsen based on idl t_extrap_tp.pro
!
! $Id$
!
! Bugs:
!not tested bfull of bugs
!none known

subroutine extrap_into_tropopause(z,t)
  
  
  implicit none
  
  real (kind=sreal),allocatable ::  t(:),z(:),t2(:)
  real ::  lr,ztp,ttp,min_t
  integer  :: nt,zdim,t2dim
    
  lr=-5 !lapse rate
  if (z(1) .le. z(2)) then
     write(*,*),'z must be in descending order !'
  end if
  
  call find_tropopause(t,z,ztp)
  
  if (ztp .eq. -999) then
     write(*,*),'tropopause not found !'
  end if
  !
  ! insert interpol routine in here
  !

  !this is supposed to find the 
  ttp=interpol(t,z,ztp)
  
  t2=(z-ztp)*lr+ttp! generate T profile with given lapse rate, passing through TP
  
  write(*,*),'t',t
  where(z .lt. ztp)	! insert actual T below TP
     t2=t
  end where
  
  write(*,*),'t2',t2
  
  ! minimum temperature allowed - 
  !below this weak lapse rate assumed to avoid non-zero KN and help convergence
  min_t=100. 
  where(t2 .gt. min_t)
     t2a=t2
  end where
  
  t2adim=size(t2a)
  zdim=size(z)
  
  
  if (t2adim .ne. zdim) then 
     ztp2=interpol_rs(z,t2,min_t)
     t2=t_extrap_tp1(z,t2,ztp2,lr=-50./(max(z)-ztp2))
  end if
  
  write(*,*),'t2 extrapped',t2
end subroutine  extrap_into_tropopause
