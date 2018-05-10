!-------------------------------------------------------------------------------
! Name: gauss_leg_quad.F90
!
! Purpose:
! Calculate the abscissas and weights of the Gauss-Legendre n-point quadrature
! formula.
!
! Taken from LMie (http://reef.atmos.colostate.edu/~gregm/lmie/) and translated
! from C. Original source is:
!
! Philip J. Davis and Philip Rabinowitz. Methods of Numerical Integration. Dover
! Publications Inc., 31 East 2nd Street, Mineola, Ney York 11501, second edition,
! 1984. ISBN 0486453391.
!
! History:
! 2017/08/09, GM: First version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module gauss_leg_quad_m

   implicit none

   private

   public :: gauss_leg_quadx

contains

!-------------------------------------------------------------------------------
! Arguments:
! Name Type          In/Out/Both Description
! ------------------------------------------------------------------------------
! n    integer       in          Number of points
! x1   real          in          Lower limit
! x2   real          in          Upper limit
! x    real array(n) both        Abscissas
! w    real array(n) both        Weights
!-------------------------------------------------------------------------------
subroutine gauss_leg_quadx(n, x1, x2, x, w)

   use preproc_constants_m

   implicit none

   integer, intent(in)  :: n
   real,    intent(in)  :: x1
   real,    intent(in)  :: x2
   real,    intent(out) :: x(0:n-1)
   real,    intent(out) :: w(0:n-1)

   integer     :: i
   integer     :: ii
   integer     :: nn
   integer     :: ia
   integer     :: ib
   real(dreal) :: da
   real(dreal) :: db
   real(dreal) :: dc
   real(dreal) :: dd
   real(dreal) :: de
   real(dreal) :: y1
   real(dreal) :: p2

   nn = (n + 1) / 2

   ia = 4 * n + 2
   ib = n * (n + 1)

   da = (n - 1) / (8.d0 * n**3)

   db = (x1 + x2) / 2.d0
   dc = (x2 - x1) / 2.d0

   ii = n - 1
   do i = 0, nn - 1
      call gauss_leg_point(n, i, ia, ib, da, y1, p2)

      dd = dc * y1

      x( i) = db - dd
      x(ii) = db + dd

      de = n * p2
      de = 2.d0 / (de * de)
      w( i) = (dc - dd*y1) * de

      w(ii) = w( i)

      ii = ii - 1
   end do

end subroutine gauss_leg_quadx


subroutine gauss_leg_point(n, i, ia, ib, da, y1_, p2_)

   use preproc_constants_m

   implicit none

   integer,     intent(in)  :: n
   integer,     intent(in)  :: i
   integer,     intent(in)  :: ia
   integer,     intent(in)  :: ib
   real(dreal), intent(in)  :: da
   real(dreal), intent(out) :: y1_
   real(dreal), intent(out) :: p2_

   integer     :: j
   real(dreal) :: db
   real(dreal) :: y2
   real(dreal) :: y1
   real(dreal) :: p3
   real(dreal) :: p2
   real(dreal) :: p1
   real(dreal) :: p1p
   real(dreal) :: p1pp

   db = (4.d0 * i + 3.d0) / ia * pi
   y1 = cos(db + da / tan(db))

   do
      p2 = 0.d0
      p1 = 1.d0
      do j = 0, n - 1
           p3 = p2
           p2 = p1

           db = y1 * p2
           p1 = j / (j + 1.d0) * (db - p3) + db
      end do

      db = 1.d0 - y1*y1
      p1p = n * (p2 - y1 * p1) / db

      p1pp = (2.d0 * y1 * p1p - ib * p1) / db

      y2 = y1
      db = p1 / p1p
      y1 = y2 - db * (1.d0 + db * p1pp / (2.d0 * p1p))

      if (abs(y1 - y2) < 3.d-14) exit
   end do

   y1_ = y1
   p2_ = p2

end subroutine gauss_leg_point

end module gauss_leg_quad_m
