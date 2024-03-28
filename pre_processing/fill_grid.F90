!-------------------------------------------------------------------------------
! Name: fill_grid.F90
!
! Purpose:
! Use a form of nearest neighbour sampling to fill in missing data
! in a 2D grid. Based on the IDL routine of the same name by Don
! Grainger (U. of Oxford)
!
! Description and Algorithm details:
!
! Arguments:
! Name         Type     In/Out/Both Description
! ------------------------------------------------------------------------------
! grid(:,:)    real*4       both    gridded data to be filled. Note that
!                                   filling is done in-place.
! fillvalue    real*4       in      The value that marks missing data in
!                                   the grid array
! mask(:,:)    integer*1    in      A mask which defines which elements
!                                   should be filled. This must have the
!                                   same dimensions as grid and elements
!                                   .ne. to 0 will have the filling
!                                   algorithm applied
!
! History:
! 2012/04/23, GT: Original - based on the fill_grid.pro IDL code originally
!    written by Don Grainger.
! 2014/06/11, AP: extend range of interpolation
! 2014/08/01, AP: Restricted it again.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module fill_grid_m

   implicit none

contains

subroutine fill_grid(grid, fillval, mask)

   use preproc_constants_m

   implicit none

   ! Input/output variables
   real(kind=sreal), dimension(:,:),   intent(inout) :: grid
   real(kind=sreal),                   intent(in)    :: fillval
   integer(kind=byte), dimension(:,:), intent(in)    :: mask

   ! Local variables
   integer(kind=lint)                                :: nx, ny, i, j, k, m
   integer(kind=lint)                                :: a1, b1, count
   integer(kind=byte)                                :: flag
   real(kind=sreal), dimension(16)                   :: isearch, jsearch
   real(kind=sreal), allocatable, dimension(:,:)     :: Z

   nx = size(grid, 1)
   ny = size(grid, 2)

   isearch = (/ -1.0,  1.0,  0.0,  0.0,  1.0,  1.0, -1.0, -1.0,  1.0, &
        1.0,  0.5,  0.5, -0.5, -0.5, -1.0, -1.0 /)
   jsearch = (/  0.0,  0.0,  1.0, -1.0,  1.0, -1.0,  1.0, -1.0,  0.5, &
        -0.5,  1.0, -1.0,  1.0, -1.0, -0.5,  0.5 /)

   allocate(Z(nx,ny))
   Z = grid

   do j = 1, ny
      do i = 1, nx
         if (mask(i,j) .ne. 0) then
            if (grid(i,j) .eq. fillval) then
               m = 0
               flag = 0
               count=8
               do while (flag .eq. 0 .and. m .lt. 4)
                  m = m+1
                  do k = 1, count
                     a1 = i + int(real(m)*isearch(k))
                     b1 = j + int(real(m)*jsearch(k))
                     if (a1 .lt. 1.0) a1 = 1
                     if (a1 .gt. nx) a1 = nx
                     if (b1 .lt. 1) b1 = 1
                     if (b1 .gt. ny) b1 = ny
                     if (z(a1,b1) .ne. fillval) then
                        grid(i,j) = z(a1,b1)
                        flag = 1
                        exit
                     end if
                  end do ! k loop
                  count = 16
               end do ! do while
            end if ! if (grid(i,j) eq fillval)
         end if ! if (skip(i,j) eq 0)
      end do ! i loop
   end do ! j loop

   deallocate(Z)

end subroutine fill_grid

end module fill_grid_m
