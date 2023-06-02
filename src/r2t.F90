!-------------------------------------------------------------------------------
! Name: r2t.F90
!
! Purpose:
! Converts radiances to brightness temperatures.
!
! Description and Algorithm details:
! Calculate temperatures from radiances
! Calculate change in temperature w.r.t. radiance (for each channel)
!
! Arguments:
! Name     Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! NChan    int        In          Number of required channels
! SAD_Chan struct     In          SAD channel structure
! R        real array In          Radiances
! T        real array Out         Temperatures
! d_T_d_R  real array Out         Gradients in temperature w.r.t. radiance
! status   int        Out         Error status
!
! History:
! 2000/11/22, KS: Original version
! 2000/11/24, KS: Modified to be more general and pass back d_T_d_R instead of
!    d_BT. Removed log10 error checking
! 2001/02/05, KS: Removed Ctrl from argument list. Added NChan (number of
!    required channels). This was done to enable R2T to be called from FM for
!    the mixed channel calculations.
! 2001/02/27, AS: Bug fix: Following previous change, SAD_Chan needs an array
!    index when used: SAD_Chan(:)%X, not SAD_Chan%X
! 2013/11/25, MJ: Fixed division by zero in log by simple workaround. Should
!    possibly avoided altogether.
! 2014/08/04, GM: Cleaned up the code.
! 2014/10/24, OS: Avoid division by zero.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine R2T(NChan, SAD_Chan, R, T, d_T_d_R, status)

   use ORAC_Constants_m
   use SAD_Chan_m

   implicit none

   ! Define arguments

   integer,          intent(in)    :: NChan
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   real,             intent(inout) :: R(:)
   real,             intent(out)   :: T(:)
   real,             intent(out)   :: d_T_d_R(:)
   integer,          intent(out)   :: status

   ! Define local variables

   integer     :: i
   real        :: C(NChan)
   real(dreal) :: T_eff(NChan)
   real        :: tiny_value, huge_value

   ! Set error status to zero

   status = 0

   tiny_value = tiny(1.0)
   huge_value = huge(1.0)

   ! Begin calculating temperatures

   do i = 1, NChan
      if (R(i) .le. ditherm6) R(i) = max(R(i),ditherm6)
   end do

   C = max( log( (SAD_Chan(:)%Thermal%B1 / R ) + 1.0 ), tiny_value)
   T_eff = min( SAD_Chan(:)%Thermal%B2 / C, huge_value)
   T = (T_eff-SAD_Chan(:)%Thermal%T1) / SAD_Chan(:)%Thermal%T2

   ! Calculate the change in brightness temperature w.r.t. radiance
   ! ( SAD_Chan(:)%Thermal%T2 * C * C * R * ( R + SAD_Chan(:)%Thermal%B1 ))
   d_T_d_R = ( SAD_Chan(:)%Thermal%B1 * SAD_Chan(:)%Thermal%B2 ) / &
             ( SAD_Chan(:)%Thermal%T2 * C * C * R * ( R + SAD_Chan(:)%Thermal%B1 ) )

end subroutine R2T
