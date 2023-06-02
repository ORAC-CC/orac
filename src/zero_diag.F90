!-------------------------------------------------------------------------------
! Name: zero_diag.F90
!
! Purpose:
! Sets values in the Diag structure to zero. Called when a given super-pixel
! is not to be processed, so that the structure can be output for every
! SPixel. Zeroing the structure clears out values left from the previous
! inversion. Only diagnostic outputs requested are zeroed. Ctrl structure
! contains flags used to switch on or off setting or output of each
! diagnostic value.
!
! Description and Algorithm details:
! for each diagnostic variable
!    check the flag in the Ctrl%Diagl array: if requested
!       set the diagnostic value to zero
!
! N.B. 0 may be an inappropriate value for some quantities (errors, fit etc).
!
! See ORAC_Constants for definitions of the flag indices DiFlagQC etc.
!
! Arguments:
! Name   Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct  In          Control structure. Contains parameter limits for
!                            quality control.
! Diag   struct  Both        Structure containing diagnostic values to be
!                            set. Error arrays St and Ss are already set by
!                            Invert_Marquardt.
!
! History:
! 2001/07/10, AS: Original version
! 2001/10/22, AS: Bug fix: costs were not zeroed.
! 2012/01/12, CP: Changed zeroing to missing rather than 0
! 2014/05/21, GM: Cleaned up the code.
! 2015/05/25, GM: Got rid of flags Diagl and removed obvious comments.
! 2015/07/28, AP: Removed status argument.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2016/01/27, GM: Add cloud emissivity and cloud emissivity uncertainty.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Zero_Diag(Ctrl, Diag)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)    :: Ctrl
   type(Diag_t), intent(inout) :: Diag

   Diag%Converged          = .false.
   Diag%QCFlag             = -1
   Diag%LimitHit           = 0
   Diag%Iterations         = 0
   Diag%Jm                 = MissingSn
   Diag%Ja                 = MissingSn
   Diag%St                 = 0
   Diag%Ss                 = 0
   Diag%Y0                 = MissingXn
   Diag%YmFit              = MissingXn
   Diag%AK                 = MissingXn
   Diag%cloud_albedo       = sreal_fill_value
   Diag%cloud_albedo_s     = sreal_fill_value
   Diag%cloud_emissivity   = sreal_fill_value
   Diag%cloud_emissivity_s = sreal_fill_value
   Diag%diffuse_frac       = sreal_fill_value
   Diag%diffuse_frac_s     = sreal_fill_value
   Diag%aot870             = MissingXn
   Diag%aot870_uncertainty = MissingSn

end subroutine Zero_Diag
