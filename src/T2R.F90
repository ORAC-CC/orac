! Name:
!   T2R
!
! Purpose:
!   Converts temperatures to radiances
!
! Arguments:
!   Name         Type          In/Out    Description
!   NChan        int           In        Number of required channels
!   SAD_Chan     array struct  In        SAD channel structure
!   T            real array    In        Temperatures
!   R            real array    Out       Radiances
!   d_R_d_T      real array    Out       Gradients in radiances. w.r.t. temperature
!   status       int           Out       Error status
!                        
! Algorithm:
!   Calculate radiances
!   Calculate change in radiances w.r.t. temperature (for each channel)
!   
! Local variables:
!   Name      Type          Description
!   BB        real array
!   C         real array
!   T_eff     real array    Effective temperature    
!
! History:
!   24th November, 2000, Kevin M. Smith : original version
!    5th February, 2001, KMS: removed Ctrl from argument list. Added NChan
!                             (number of required channels). This was done
!                             to be consistent with changed to R2T.
!   21st Feb 2001, Andy Smith: comments on arguments corrected
! 20140128 MJ fixes some overflow:
! however, only symptoms are cured here not the actual reason for the overflow (unknown)
!nor is this condition reported as status.
!20140204 MJ fixes bug in above fix.
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine T2R(NChan, SAD_Chan, T, R, d_R_d_T, status)

    use SAD_Chan_def

    implicit none

!   Define arguments

    integer :: NChan

    type(SAD_Chan_t) :: SAD_Chan(NChan)

    real(4) :: T(NChan)
    real(4) :: R(NChan)
    real(4) :: d_R_d_T(NChan)
    integer :: status

    real huge_value,log_huge_value

!   Define local variables

    real(4) :: BB(NChan)
    real(4) :: C(NChan)
    real(4) :: T_eff(NChan)
    
!   Set status to zero

    status = 0

    huge_value=huge(1.0)
    log_huge_value=log(huge_value)

!   Begin calculating radiances

    T_eff = ( T * SAD_Chan%Thermal%T2 ) + SAD_Chan%Thermal%T1

    BB = min(( SAD_Chan%Thermal%B2 / T_eff ),log_huge_value)

    C = exp( BB )

    R = SAD_Chan%Thermal%B1 / (C-1.0)

!   Calculate change in radiances with temperature

    d_R_d_T = min(( SAD_Chan%Thermal%B1 * BB * C * SAD_Chan%Thermal%T2) / &
         & ( T_eff * (C-1.0) * (C-1.0) ),huge_value)

end subroutine T2R
