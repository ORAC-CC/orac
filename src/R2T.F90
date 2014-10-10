!-------------------------------------------------------------------------------
! Name:
!    R2T
!
! Purpose:
!    Converts radiances to brightness temperatures.
!
! Arguments:
!    Name     Type       In/Out/Both Description
!    NChan    int        In          Number of required channels
!    SAD_Chan struct     In          SAD channel structure
!    R        real array In          Radiances
!    T        real array Out         Temperatures
!    d_T_d_R  real array Out         Gradients in temperature w.r.t. radiance
!    status   int        Out         Error status
!
! Algorithm:
!    Calculate temperatures from radiances
!    Calculate change in temperature w.r.t. radiance (for each channel)
!
! Local variables:
!    Name Type Description
!
! History:
!    22nd Nov 2000, Kevin M. Smith: Original version
!    24th Nov 2000, Kevin M. Smith:
!       Modified to be more general and pass back d_T_d_R instead of d_BT.
!       Removed log10 error checking
!     5th Feb 2001, Kevin M. Smith:
!       Removed Ctrl from argument list.
!       Added NChan (number of required channels). This was done to enable R2T
!       to be called from FM for the mixed channel calculations.
!    27th Feb 2001, Andy Smith:
!       Bug fix: Following previous change, SAD_Chan needs an array index when
!       used: SAD_Chan(:)%X, not SAD_Chan%X
!    25th Nov 2013, Matthias Jerg:
!       Fixed division by zero in log by simple workaround. Should possibly
!       avoided altogether.
!     4th Aug 2014, Greg McGarragh: Cleaned up the code.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine R2T(NChan, SAD_Chan, R, T, d_T_d_R, status)

   use ECP_constants
   use SAD_Chan_def

   implicit none

   ! Define arguments

   integer,          intent(in)    :: NChan
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(NChan)
   real(4),          intent(inout) :: R(NChan)
   real(4),          intent(out)   :: T(NChan)
   real(4),          intent(out)   :: d_T_d_R(NChan)
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i
   real(4) :: C(NChan)
   real(4) :: T_eff(NChan)

   ! Set error status to zero

   status = 0

   ! Begin calculating temperatures

   do i = 1, NChan
      if (R(i) .le. ditherm6) R(i) = max(R(i),ditherm6)
   end do

   C = log( ( SAD_Chan(:)%Thermal%B1 / R ) + 1.0 )
   T_eff = SAD_Chan(:)%Thermal%B2 / C

   T = (T_eff-SAD_Chan(:)%Thermal%T1) / SAD_Chan(:)%Thermal%T2

   ! Calculate the change in brightness temperature w.r.t. radiance
   ! ( SAD_Chan(:)%Thermal%T2 * C * C * R * ( R + SAD_Chan(:)%Thermal%B1 ))
   d_T_d_R = ( SAD_Chan(:)%Thermal%B1 * SAD_Chan(:)%Thermal%B2 ) / &
             ( SAD_Chan(:)%Thermal%T2 * C * C * R * ( R + SAD_Chan(:)%Thermal%B1 ) )

end subroutine R2T
