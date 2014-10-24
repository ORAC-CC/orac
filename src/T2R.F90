!-------------------------------------------------------------------------------
! Name:
!    T2R
!
! Purpose:
!    Converts temperatures to radiances
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    NChan    int          In          Number of required channels
!    SAD_Chan array struct In          SAD channel structure
!    T        real array   In          Temperatures
!    R        real array   Out         Radiances
!    d_R_d_T  real array   Out         Gradients in radiance. w.r.t. temperature
!    status   int          Out         Error status
!
! Algorithm:
!    Calculate radiances
!    Calculate change in radiances w.r.t. temperature (for each channel)
!
! Local variables:
!   Name Type Description
!
! History:
!    24th November, 2000, Kevin M. Smith: original version
!     5th February, 2001, Kevin M. Smith:
!       Removed Ctrl from argument list. Added NChan (number of required
!       channels). This was done to be consistent with changed to R2T.
!    21st Feb 2001, Andy Smith: comments on arguments corrected
!    28th Jan 2014, Matthias Jerg: Fixes some overflow: however, only symptoms
!       are cured here not the actual reason for the overflow (unknown) nor is
!       this condition reported as status.
!     4th Feb 2014, Matthias Jerg: Fixes bug in above fix.
!    24th Oct 2014, Oliver Sus: Some minor changes to deal with float under/overflow issues.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine T2R(NChan, SAD_Chan, T, R, d_R_d_T, status)

   use SAD_Chan_def

   implicit none

   ! Define arguments

   integer,          intent(in)    :: NChan
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(NChan)
   real(4),          intent(inout) :: T(NChan)
   real(4),          intent(out)   :: R(NChan)
   real(4),          intent(out)   :: d_R_d_T(NChan)
   integer,          intent(out)   :: status

   ! Define local variables

   real(8) :: BB(NChan)
   real(8) :: C(NChan)
   real(8) :: T_eff(NChan)
   real(4) :: huge_value, log_huge_value
   real(8) :: dummy_nominator(NChan)
   real(8) :: dummy_denominator(NChan)
   real(8) :: dummy_result(NChan)

   ! Set status to zero

   status = 0

   huge_value=huge(1.0)
   log_huge_value=log(huge_value)

   ! Begin calculating radiances

   T_eff = ( T * SAD_Chan%Thermal%T2 ) + SAD_Chan%Thermal%T1

   BB = min(( SAD_Chan%Thermal%B2 / T_eff ),log_huge_value)

   C = exp( BB )

   R = SAD_Chan%Thermal%B1 / (C-1.0)

   ! Calculate change in radiances with temperature

   dummy_nominator = SAD_Chan%Thermal%B1 * BB * C * SAD_Chan%Thermal%T2
   dummy_denominator =  T_eff * (C-1.0) * (C-1.0)
   dummy_result = dummy_nominator / dummy_denominator
   dummy_result = min(dummy_result, huge_value)
   d_R_d_T = dummy_result

end subroutine T2R
