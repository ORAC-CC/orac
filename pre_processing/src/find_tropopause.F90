!-------------------------------------------------------------------------------
! Name: find_tropopause.f90
!
! Purpose:
! find troppuaes given temperature and height profile
! 
! Description and Algorithm details:
!
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! T0    real    in  Temperature profile / K
! Z0    real    in  Corresponding altitudes / km
! ztp   real    out Tropopause height
! nlevs integer in  Number of elements in T0 and Z0.
!
! History:
! 2012/05/24, CP: Original code based on idl find_tp.pro
! 2014/03/14, CP: debugged and tested
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine find_tropopause(t0,z0,ztp,nlevs)

   implicit none
   integer, intent(in)                   :: nlevs
   real,    intent(in), dimension(nlevs) :: t0,z0
   real,    intent(out)                  :: ztp

   real              :: missing,xout,yout,datout,hh,gg,kk
   real, allocatable :: dt(:),t(:),z(:)
   real              :: gp,zt,ft,thr,sf
   integer           :: tdim,dim,nt,i,i0,dts,zdim,ncount,n

   sf=10000.0
   missing=-999.
   zdim=size(z0(:))


   ncount=0

   do i=1,zdim
      if (z0(i)/sf .gt. 5 .and. z0(i)/sf .lt. 25) then
         ncount=ncount+1
      endif
   enddo

   allocate(t(ncount))
   allocate(z(ncount))
   ncount=0
   do i=1,zdim
      if (z0(i)/sf .gt. 5 .and. z0(i)/sf .lt. 25) then
         ncount=ncount+1
         z(ncount)=z0(i)/sf
         t(ncount)=t0(i)

      endif
   enddo

   tdim=size(t)


   if (tdim .eq. 0) then
      write(*,*)' no tropopause'
      ztp=-999.
   end if


   !
   !not sure sort shell works 
   call hpsort(tdim,z)

   !reverse the t profile
   n = size(t)

   t = t(n:1:-1)

   nt=size(t)

   !
   !calculate lapse rate
   !
   dts=size(t(2:nt))
   allocate(dt(dts))
   dt=(t(2:nt)-t(1:nt-1))/(z(2:nt)-z(1:nt-1))

   dt=([dt(1),dt]+[dt,dt(nt-1)])/2.

   gp=dt(1)
   i0=2
   ft=0
   zt=-999.
   thr=-2. ! K/km - tropopause is where lapse rate drops below this.


   do i=i0,nt-1
      if( ft .eq. 0) then

         if (dt(i-1) .le. thr .and. dt(i) .gt. thr) then

            hh=abs(dt(i-1)-thr)
            gg=abs(dt(i)-thr)
            write(*,*)'hh gg',hh,gg
            kk=hh+gg


            ! looks for first inflection
            ztp=(gg/kk)*z(i-1)+ (hh/kk)*z(i)

            ft=1
         end if
      end if

   end do

   deallocate(t)
   deallocate(z)  

end subroutine find_tropopause
