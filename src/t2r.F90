!-------------------------------------------------------------------------------
! Name: t2r.F90
!
! Purpose:
! Converts temperatures to radiances
!
! Description and Algorithm details:
! Calculate radiances
! Calculate change in radiances w.r.t. temperature (for each channel)
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! NChan    int          In          Number of required channels
! SAD_Chan array struct In          SAD channel structure
! T        real array   In          Temperatures
! R        real array   Out         Radiances
! d_R_d_T  real array   Out         Gradients in radiance. w.r.t. temperature
! status   int          Out         Error status
!
! History:
! 2000/11/24, KS: original version
! 2001/02/05, KS: Removed Ctrl from argument list. Added NChan (number of
!    required channels). This was done to be consistent with changed to R2T.
! 2001/02/21, AS: comments on arguments corrected
! 2014/01/28, MJ: Fixes some overflow: however, only symptoms are cured here not
!    the actual reason for the overflow (unknown) nor is this condition
!    reported as status.
! 2014/02/04, MJ: Fixes bug in above fix.
! 2014/10/24, OS: Some minor changes to deal with float under/overflow issues.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine T2R(NChan, SAD_Chan, T, R, d_R_d_T, status)

   use ORAC_Constants_m
   use SAD_Chan_m

   implicit none

   ! Define arguments

   integer,          intent(in)  :: NChan
   type(SAD_Chan_t), intent(in)  :: SAD_Chan(:)
   real,             intent(in)  :: T(:)
   real,             intent(out) :: R(:)
   real,             intent(out) :: d_R_d_T(:)
   integer,          intent(out) :: status

   ! Define local variables

   real(dreal) :: BB(NChan)
   real(dreal) :: C(NChan)
   real(dreal) :: T_eff(NChan)
   real        :: huge_value, log_huge_value
   real(dreal) :: dummy_nominator(NChan)
   real(dreal) :: dummy_denominator(NChan)
   real(dreal) :: dummy_result(NChan)


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
