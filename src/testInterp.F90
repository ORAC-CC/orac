!-------------------------------------------------------------------------------
! Name: testInterp.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

program testInterp

   use test_int_def

   implicit none

   integer    :: i, j
   real   :: xGrid(5), yGrid(5)
   real   :: FInt, dFdx, dFdy
   integer  :: nx, ny
   real   :: xstep, ystep
   real   :: curX, CurY
   real   :: F(1,5,5)

   xstep = 1.
   ystep = 2.
   nx = 5
   ny = 5

   do i = 1, 5
      xGrid(i) = real(i)
      yGrid(i) = real(2.0*i)
      do j = 1, 5
         F(1,i,j) = real(i*j)
      end do
   end do

   write(*,'(a,5(f4.1, 1x))') 'X grid ',(xGrid(i), i=1,5)
   write(*,'(a,5(f4.1, 1x))') 'Y grid ',(yGrid(i), i=1,5)

   CurX = xGrid(2)
   CurY = yGrid(2)

   call Interp3dLUT(1, F, xGrid, yGrid, xstep, ystep, CurX, CurY, &
      FInt, dFdx, dFdy)
   write(*,*)' Interpolated values: ',  FInt, dFdx, dFdy
   write(*,*)

   CurX = xGrid(3)
   CurY = yGrid(3)

   call Interp3dLUT(1, F, xGrid, yGrid, xstep, ystep, CurX, CurY, &
      FInt, dFdx, dFdy)
   write(*,*)' Interpolated values: ',  FInt, dFdx, dFdy
   write(*,*)

   CurX = xGrid(3) + xstep/2
   CurY = yGrid(3) + ystep/2

   call Interp3dLUT(1, F, xGrid, yGrid, xstep, ystep, CurX, CurY, &
      FInt, dFdx, dFdy)
   write(*,*)' Interpolated values: ',  FInt, dFdx, dFdy

end program testInterp
