! Name: find_tropopause.f90
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
! 2012/05/24:Original code C. Poulsen based on idl find_tp.pro
!
! $Id$
!
! Bugs:
!
!none known


subroutine find_tropopause(t0,z0,ztp)

  implicit none
  real (kind=sreal),allocatable ::  t(:),z(:)
  real ::  gp,zt,ft,thr,ztp,dt
  integer  ::  tdim,dim,nt,i,i0


  where(z0 .gt. 5 .and. z0 .lt. 25)
     t0w=t0
     z0w=z0
  end where
  write(*,*)'t',t
  write(*,*)'t0',t0

  pause
  size(t0w,tdim)
  write(*,*)'tdim',tdim
  if (tdim .eq. 0) then
     write(*,*)' no tropopause'
     return,-999.
  end if

  t=t0w
  z=z0w
  write(*,*)' before z',z
  !
  !not sure sort shell works 
  call sort_shell(tdim,z)
  write(*,*)' after z ',z
  
  pause
  call sort_shell(tdim,t)
  z=zs
  t=ts
  
  size(t,dim)
  write(*,*),size(t,dim)
  write(*,*),'dim',dim
  
  pause
  nt=dim
  !
  !calculate lapse rate
  !
  dt=(t(2:nt)-t(1:nt-1))/(z(2:nt)-z(1:nt-1))
  dt=([dt(1),dt]+[dt,dt(nt-1)])/2.
  
  gp=dt(1)
  i0=2
  ft=0
  zt=-999.
  thr=-2. ! K/km - tropopause is where lapse rate drops below this.
  
  do i=i0,nt
     if( ft .eq. 0) then
        if (dt(i-1) .le. thr .and. dt(i) .gt. thr) then
           ztp=interpol([z(i-1),z(i)],[dt(i-1),dt(i)],thr)
           ft=1
        end if
     end if
     !		i=i+1
  end do
  write(*,*)'ztp',ztp
  
end subroutine find_tropopause
