! Name: interpol_nearest_neighbour.F90
!
! Purpose:
! Does a nearest neighbour on regularly gridded data for a vector of locations
!
! Description and algorithm details
!
! Arguments:
! Name            Type     In/Out/Both Description
! xin             real        in       X-coordinate of the gridded data
! yin             real        in       Y-coordinate of the gridded data
! datain          real        in       The gridded datafield to be interpolated
! xout            real        in       A vector of x-coordintes for the desired values
! yout            real        in       A vecotr of y-coordintes for the desired values
! dataout         real        out      On return, will contain the interpolated
!                                      values coorespoinding to xout & yout.
! Local variables:
! Name Type Description
!
! History:
! 27/06/2012 Gareth Thomas. Finished first version
!
! Bugs:
!

module interpol_nearest_neighbour_def
  interface
     subroutine interpol_nearest_neighbour(xin, yin, datin, xout, yout, datout)
       use preproc_constants
       implicit none
       real(kind=sreal), dimension(:)          :: xin,yin
       real(kind=sreal), dimension(:,:)        :: datin
       real(kind=sreal), dimension(:)          :: xout, yout
       real(kind=sreal), dimension(:)          :: datout
     end subroutine interpol_nearest_neighbour
  end interface
end module interpol_nearest_neighbour_def

subroutine interpol_nearest_neighbour(xin, yin, datin, xout, yout, datout)
  
  use preproc_constants

  implicit none

  ! Arguments
  real(kind=sreal), dimension(:), intent(in)   :: xin,yin
  real(kind=sreal), dimension(:,:), intent(in) :: datin
  real(kind=sreal), dimension(:), intent(in)   :: xout, yout
  real(kind=sreal), dimension(:), intent(out)  :: datout
  ! Local variables
  real(kind=sreal), allocatable, dimension(:)  :: dx, dy
!MST
  integer(kind=lint)                           :: nxin, nyin, nout
  integer(kind=lint)                           :: ii, jj, i, j, out
!MST
  integer(kind=lint)                           :: ii2, jj2, ii2_old,jj2_old!,kk0
  integer(kind=lint)                           :: ii2bot,ii2top,jj2bot,jj2top

  nxin = size(xin)
  nyin = size(yin)

  nout = size(xout)

  ! Temporary "distance" vectors
  allocate(dx(nxin))
  allocate(dy(nyin))

  ! For each desired point, find the nearest location in x & y
  ! directions and extract the corresponding data value.

  do out=1,nout
     ! Run through the x-coordinte: if the abs difference between the input value
     ! and output value starts increasing, we've found the closet point, so
     ! exit the do loop at that point.


!MST folling IF is by MST. routine below is way faster
if(1 .eq. 2) then
     dx(1) = abs(xin(1) - xout(out))
     ii = nxin
     do i=2,nxin
        dx(i) = abs(xin(i) - xout(out))
        if (dx(i) .gt. dx(i-1)) then
           ii = i-1
           exit
        end if
     end do
     ! Repeat the above process with the y-coordinate.
     dy(1) = abs(yin(1) - yout(out))
     jj = nyin
     do j=2,nyin
        dy(j) = abs(yin(j) - yout(out))
        if (dy(j) .gt. dy(j-1)) then
           jj = j-1
           exit
        end if
     end do
     
     ! Extract the corresponding data value
     datout(out) = datin(ii,jj)

!MST  next line is by MST
endif

!MST
if(1 .eq. 1) then

ii2bot=1
ii2top=nxin
jj2bot=1
jj2top=nyin
ii2_old=ii2bot
jj2_old=jj2bot
!MST I believe y axis is upsite down...
do !kk0=1,15 
 ii2=(ii2bot+ii2top)/2
 jj2=(jj2bot+jj2top)/2
 if(xout(out) .gt. xin(ii2)) ii2bot = ii2
 if(xout(out) .lt. xin(ii2)) ii2top = ii2
 if(yout(out) .gt. yin(jj2)) jj2top = jj2
 if(yout(out) .lt. yin(jj2)) jj2bot = jj2
 if(ii2 .eq. ii2_old .and. jj2 .eq. jj2_old) exit
 ii2_old = ii2
 jj2_old = jj2
enddo
     datout(out) = datin(ii2,jj2)
endif
!MST end

  end do

  deallocate(dx)
  deallocate(dy)

end subroutine interpol_nearest_neighbour

