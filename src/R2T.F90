! Name:
!   R2T
!
! Purpose:
!   Converts radiances to brightness temperatures
!
! Arguments:
!   Name         Type          In/Out    Description
!   NChan        int           In        Number of required channels
!   SAD_Chan     struct        In        SAD channel structure
!   R            real array    In        Radiances
!   T            real array    Out       Temperatures
!   d_T_d_R      real array    In        Changes in temperature w.r.t. radiance
!   status       int           Out       Error status
!                        
! Algorithm:
!   Calculate temperatures from radiances
!   Calculate change in temperature w.r.t. radiance (for each channel)
!   
! Local variables:
!   Name      Type         Description
!   C         real array   Dummy variable
!   T_eff     real array   Effective temperature
!
! History:
!   22nd November, 2000, Kevin M. Smith : original version
!   24th November, 2000, KMS: modified to be more general and pass back d_T_d_R
!                             instead of d_BT. Removed log10 error checking
!    5th February, 2001, KMS: removed Ctrl from argument list. Added NChan
!                             (number of required channels). This was done
!                             to enable R2T to be called from FM for the
!                             mixed channel calculations.
!   27th Feb 2001, andy Smith:
!      Bug fix: following previous change, SAD_Chan needs an array 
!      index when used: SAD_Chan(:)%X, not SAD_Chan%X
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine R2T(NChan, SAD_Chan, R, T, d_T_d_R, status)

    use SAD_Chan_def

    implicit none

!   Define arguments

    integer :: NChan

    type(SAD_Chan_t) :: SAD_Chan(NChan)

    real(4) :: R(NChan)
    real(4) :: T(NChan)
    real(4) :: d_T_d_R(NChan)
    integer :: status

!   Define local variables

    real(4) :: C(NChan)
    real(4) :: T_eff(NChan)

!   set error status to zero

    status = 0

!   Begin calculating temperatures

    C = log( ( SAD_Chan(:)%Thermal%B1 / R ) + 1.0 )
!    write(*,*) C, SAD_Chan(:)%Thermal%T2
    T_eff = SAD_Chan(:)%Thermal%B2 / C

    T = (T_eff-SAD_Chan(:)%Thermal%T1) / SAD_Chan(:)%Thermal%T2

!   Calculate the change in brightness temperature w.r.t. radiance
!    write(*,*)    ( SAD_Chan(:)%Thermal%T2 * C * C * R * ( R + SAD_Chan(:)%Thermal%B1 ))

    d_T_d_R = ( SAD_Chan(:)%Thermal%B1 * SAD_Chan(:)%Thermal%B2 ) / &
              ( SAD_Chan(:)%Thermal%T2 * C * C * R * ( R + SAD_Chan(:)%Thermal%B1 ) )

end subroutine R2T
