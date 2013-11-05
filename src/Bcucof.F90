! BCuCof.F90
! 
!	Calculates the coefficients for a bicubic interpolation
!
! Description:
!
!	Given arrays y, y1, y2 and y12, each of length 4, containing
!	the function, gradients and cross derivative at the four points 
!	of a rectangular grid cell (numbered counterclockwise from the 
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
!
! Bugs
!    None known
!
!---------------------------------------------------------------------

SUBROUTINE bcucof(y,y1,y2,y12,d1,d2,c)

    implicit none

    real, intent(in) :: d1,d2
    real, dimension(4), intent(in) :: y,y1,y2,y12
    real, dimension(4,4), intent(out) :: c
    real, dimension(16) :: x
    real, dimension(16,16) :: wt

data wt /1,0,-3,2,4*0,-3,0,9,-6,2,0,-6,4,&
   8*0,3,0,-9,6,-2,0,6,-4,10*0,9,-6,2*0,-6,4,2*0,3,-2,6*0,-9,6,&
   2*0,6,-4,4*0,1,0,-3,2,-2,0,6,-4,1,0,-3,2,8*0,-1,0,3,-2,1,0,-3,&
   2,10*0,-3,2,2*0,3,-2,6*0,3,-2,2*0,-6,4,2*0,3,-2,0,1,-2,1,5*0,&
   -3,6,-3,0,2,-4,2,9*0,3,-6,3,0,-2,4,-2,10*0,-3,3,2*0,2,-2,2*0,&
   -1,1,6*0,3,-3,2*0,-2,2,5*0,1,-2,1,0,-2,4,-2,0,1,-2,1,9*0,-1,2,&
   -1,0,1,-2,1,10*0,1,-1,2*0,-1,1,6*0,-1,1,2*0,2,-2,2*0,-1,1/

    x(1:4)   = y
    x(5:8)   = y1*d1
    x(9:12)  = y2*d2
    x(13:16) = y12*d1*d2

    x=matmul(wt,x)			!Matrix multiply by the stored table.
    c=reshape(x,(/4,4/),order=(/2,1/))	!Unpack the result into output table. 

END SUBROUTINE bcucof

