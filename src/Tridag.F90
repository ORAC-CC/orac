! tridag.90
!
! Purpose:
!
!    Given input vectors a,b,c,r solves the tridiagonal equations
!    shown for output vector u
!
!    |b(  1) c(  1)    0    ..              |   |u(  1)|   |n(  1)|
!    |a(  2) b(  2) c(  2)  ..              |   |u(  2)|   |n(  2)|
!    |   ..     ..     ..   ..   ..     ..  | . |  ..  | = |  ..  |
!    |                      .. b(N-1) c(N-1)|   |u(N-1)|   |n(N-1)|
!    |                      .. a(  N) b(  N)|   |u(  N)|   |n(  N)|
!
!    Called by spline.f90
!
! Arguments:
!    Name        Type    in/Out/Both    Description
!    a           real array  in         Vectors a,b,c form a
!                                       tridiagonal set
!    b           real array  in
!    c           real array  in
!    r           real array  in		
!    u		 real array  Out	The solution
!
! Algorithm
!    From Numerical Recipes in Fortran 90 [Press, Flannery]
!    
! History
!    31st April 2011 - Adapted for ORAC F90 by C. Arnold 
!
! Bugs
!    None known
!
!---------------------------------------------------------------------

subroutine tridag_ser(a,b,c,r,u)

    implicit none

!   Define arguments

    real, dimension(:), intent(in) :: a,b,c,r
    real, dimension(:), intent(out) :: u

!   Define local variables

    real, dimension(size(b)) :: gam
    integer :: n,j
    real :: bet

    n = size(b)
    bet=b(1)
    if (bet == 0.0) write(*,*) 'tridag_ser: Error at stage 1'
        u(1)=r(1)/bet
	do j=2,n
	    gam(j)=c(j-1)/bet
	    bet=b(j)-a(j-1)*gam(j)
	    if (bet == 0.0) &
	            write(*,*) 'tridag_ser: Error at stage 2'
	    u(j)=(r(j)-a(j-1)*u(j-1))/bet
	end do
	do j=n-1,1,-1
	    u(j)=u(j)-gam(j+1)*u(j+1)
	end do
end subroutine tridag_ser

recursive subroutine tridag_par(a,b,c,r,u)

    implicit none

!   Define arguments

    real, dimension(:), intent(in) :: a,b,c,r
    real, dimension(:), intent(out) :: u

!   Define local variables

    integer, parameter :: NPAR_TRIDAG=4
    integer :: n,n2,nm,nx
    real, dimension(size(b)/2) :: y,q,piva
    real, dimension(size(b)/2-1) :: x,z
    real, dimension(size(a)/2) :: pivc

!   interface for tridag_ser (only called here)

    interface
	subroutine tridag_ser(a,b,c,r,u)

            real, dimension(:), intent(in) :: a,b,c,r
	    real, dimension(:), intent(out) :: u

	end subroutine tridag_ser
    end interface

    n = size(b)
    if (n < NPAR_TRIDAG) then
        call tridag_ser(a,b,c,r,u)
    else
	if (maxval(abs(b(1:n))) == 0.0) &
	    write(*,*) 'tridag_par: possible singular matrix'
	n2=size(y)
	nm=size(pivc)
	nx=size(x)
	piva = a(1:n-1:2)/b(1:n-1:2)
	pivc = c(2:n-1:2)/b(3:n:2)
	y(1:nm) = b(2:n-1:2)-piva(1:nm)*c(1:n-2:2)-pivc*a(2:n-1:2)
	q(1:nm) = r(2:n-1:2)-piva(1:nm)*r(1:n-2:2)-pivc*r(3:n:2)
	if (nm < n2) then
	    y(n2) = b(n)-piva(n2)*c(n-1)
	    q(n2) = r(n)-piva(n2)*r(n-1)
	end if
	x = -piva(2:n2)*a(2:n-2:2)
	z = -pivc(1:nx)*c(3:n-1:2)
	call tridag_par(x,y,z,q,u(2:n:2))
	u(1) = (r(1)-c(1)*u(2))/b(1)
	u(3:n-1:2) = (r(3:n-1:2)-a(2:n-2:2)*u(2:n-2:2) &
	             -c(3:n-1:2)*u(4:n:2))/b(3:n-1:2)
	if (nm == n2) u(n)=(r(n)-a(n-1)*u(n-1))/b(n)
    end if
end subroutine tridag_par

