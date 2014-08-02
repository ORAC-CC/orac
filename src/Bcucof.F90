!-------------------------------------------------------------------------------
! Bcucof.F90
!
! Purpose:
!	Calculates the coefficients for a bicubic interpolation
!
! Description:
!	Given arrays y, y1, y2 and y12, each of length 4, containing
!	the function, gradients and cross derivative at the four points
!	of a rectangular grid cell (numbered counter clockwise from the
!	lower left), and given d1 and d2, the length of the grid cell
!	in the 1- and 2- directions, this routine returns the 4x4 table
!	c that is used by the routine BCuInt_CA for bicubic interpolation.
!
! Arguments:
!    Name        Type    In/Out/Both    Description
!    y           real array  In         Values of functions at each of
!                                       the four vertices around the
!                                       point to be interpolated to.
!                                       Values should be listed in
!                                       anti-clockwise order, starting
!                                       at the bottom left.
!    y1          real array  In         Derivatives wrt to the
!                                       "x-coordinate" at the vertices.
!    y2          real array  In         Derivatives wrt to the
!                                       "y-coordinate" at the vertices.
!    y12         real array  In         Second order cross-derivatives
!                                       at the vertices.
!    d1,d2       real        In         The spaces of the grid in the x
!                                       and y dimensions, respectively.
!
! Algorithm
!    From Numerical Recipes in Fortran 90 [Press, Flannery]
!
! History
!    22 April 2009 - Written by C. Arnold
!    23 December 2014, Greg McGarragh:
!       Performance improvements gained by unrolling the sparse matrix-vector
!       multiplication and eliminating some un-needed memory copies plus some
!       code cleanup.
!
! Bugs
!    None known
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine bcucof(y,y1,y2,y12,d1,d2,c)

    implicit none

    real, dimension(4),  intent(in)  :: y,y1,y2,y12
    real,                intent(in)  :: d1,d2
    real, dimension(16), intent(out) :: c

    real                             :: a
    real, dimension(16)              :: x
    real, dimension(16,16)           :: wt

    data wt /1,0,-3,2,4*0,-3,0,9,-6,2,0,-6,4,&
       8*0,3,0,-9,6,-2,0,6,-4,10*0,9,-6,2*0,-6,4,2*0,3,-2,6*0,-9,6,&
      2*0,6,-4,4*0,1,0,-3,2,-2,0,6,-4,1,0,-3,2,8*0,-1,0,3,-2,1,0,-3,&
      2,10*0,-3,2,2*0,3,-2,6*0,3,-2,2*0,-6,4,2*0,3,-2,0,1,-2,1,5*0,&
      -3,6,-3,0,2,-4,2,9*0,3,-6,3,0,-2,4,-2,10*0,-3,3,2*0,2,-2,2*0,&
      -1,1,6*0,3,-3,2*0,-2,2,5*0,1,-2,1,0,-2,4,-2,0,1,-2,1,9*0,-1,2,&
      -1,0,1,-2,1,10*0,1,-1,2*0,-1,1,6*0,-1,1,2*0,2,-2,2*0,-1,1/

if (.false.) then
    c(1:4)   = y
    c(5:8)   = y1*d1
    c(9:12)  = y2*d2
    c(13:16) = y12*d1*d2

    c=matmul(wt,c) ! Matrix multiply by the stored table.
else
    x(1:4)   = y
    x(5:8)   = y1*d1
    x(9:12)  = y2*d2
    x(13:16) = y12*d1*d2

    a = 0.
    a = a + wt(1,1) * x(1)
    c(1) = a

    a = 0.
    a = a + wt(2,9) * x(9)
    c(2) = a

    a = 0.
    a = a + wt(3,1) * x(1)
    a = a + wt(3,4) * x(4)
    a = a + wt(3,9) * x(9)
    a = a + wt(3,12) * x(12)
    c(3) = a

    a = 0.
    a = a + wt(4,1) * x(1)
    a = a + wt(4,4) * x(4)
    a = a + wt(4,9) * x(9)
    a = a + wt(4,12) * x(12)
    c(4) = a

    a = 0.
    a = a + wt(5,5) * x(5)
    c(5) = a

    a = 0.
    a = a + wt(6,13) * x(13)
    c(6) = a

    a = 0.
    a = a + wt(7,5) * x(5)
    a = a + wt(7,8) * x(8)
    a = a + wt(7,13) * x(13)
    a = a + wt(7,16) * x(16)
    c(7) = a

    a = 0.
    a = a + wt(8,5) * x(5)
    a = a + wt(8,8) * x(8)
    a = a + wt(8,13) * x(13)
    a = a + wt(8,16) * x(16)
    c(8) = a

    a = 0.
    a = a + wt(9,1) * x(1)
    a = a + wt(9,2) * x(2)
    a = a + wt(9,5) * x(5)
    a = a + wt(9,6) * x(6)
    c(9) = a

    a = 0.
    a = a + wt(10,9) * x(9)
    a = a + wt(10,10) * x(10)
    a = a + wt(10,13) * x(13)
    a = a + wt(10,14) * x(14)
    c(10) = a

    a = 0.
    a = a + wt(11,1) * x(1)
    a = a + wt(11,2) * x(2)
    a = a + wt(11,3) * x(3)
    a = a + wt(11,4) * x(4)
    a = a + wt(11,5) * x(5)
    a = a + wt(11,6) * x(6)
    a = a + wt(11,7) * x(7)
    a = a + wt(11,8) * x(8)
    a = a + wt(11,9) * x(9)
    a = a + wt(11,10) * x(10)
    a = a + wt(11,11) * x(11)
    a = a + wt(11,12) * x(12)
    a = a + wt(11,13) * x(13)
    a = a + wt(11,14) * x(14)
    a = a + wt(11,15) * x(15)
    a = a + wt(11,16) * x(16)
    c(11) = a

    a = 0.
    a = a + wt(12,1) * x(1)
    a = a + wt(12,2) * x(2)
    a = a + wt(12,3) * x(3)
    a = a + wt(12,4) * x(4)
    a = a + wt(12,5) * x(5)
    a = a + wt(12,6) * x(6)
    a = a + wt(12,7) * x(7)
    a = a + wt(12,8) * x(8)
    a = a + wt(12,9) * x(9)
    a = a + wt(12,10) * x(10)
    a = a + wt(12,11) * x(11)
    a = a + wt(12,12) * x(12)
    a = a + wt(12,13) * x(13)
    a = a + wt(12,14) * x(14)
    a = a + wt(12,15) * x(15)
    a = a + wt(12,16) * x(16)
    c(12) = a

    a = 0.
    a = a + wt(13,1) * x(1)
    a = a + wt(13,2) * x(2)
    a = a + wt(13,5) * x(5)
    a = a + wt(13,6) * x(6)
    c(13) = a

    a = 0.
    a = a + wt(14,9) * x(9)
    a = a + wt(14,10) * x(10)
    a = a + wt(14,13) * x(13)
    a = a + wt(14,14) * x(14)
    c(14) = a

    a = 0.
    a = a + wt(15,1) * x(1)
    a = a + wt(15,2) * x(2)
    a = a + wt(15,3) * x(3)
    a = a + wt(15,4) * x(4)
    a = a + wt(15,5) * x(5)
    a = a + wt(15,6) * x(6)
    a = a + wt(15,7) * x(7)
    a = a + wt(15,8) * x(8)
    a = a + wt(15,9) * x(9)
    a = a + wt(15,10) * x(10)
    a = a + wt(15,11) * x(11)
    a = a + wt(15,12) * x(12)
    a = a + wt(15,13) * x(13)
    a = a + wt(15,14) * x(14)
    a = a + wt(15,15) * x(15)
    a = a + wt(15,16) * x(16)
    c(15) = a

    a = 0.
    a = a + wt(16,1) * x(1)
    a = a + wt(16,2) * x(2)
    a = a + wt(16,3) * x(3)
    a = a + wt(16,4) * x(4)
    a = a + wt(16,5) * x(5)
    a = a + wt(16,6) * x(6)
    a = a + wt(16,7) * x(7)
    a = a + wt(16,8) * x(8)
    a = a + wt(16,9) * x(9)
    a = a + wt(16,10) * x(10)
    a = a + wt(16,11) * x(11)
    a = a + wt(16,12) * x(12)
    a = a + wt(16,13) * x(13)
    a = a + wt(16,14) * x(14)
    a = a + wt(16,15) * x(15)
    a = a + wt(16,16) * x(16)
    c(16) = a
endif

end subroutine bcucof
