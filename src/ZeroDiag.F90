!-------------------------------------------------------------------------------
! Name: ZeroDiag.F90
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
! See ECP_Constants for definitions of the flag indices DiFlagQC etc.
!
! Arguments:
! Name   Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct  In          ECP control structure. Contains parameter limits
!                            for quality control.
! Diag   struct  Both        Structure containing diagnostic values to be
!                            set. Error arrays St and Ss are already set by
!                            Invert_Marquardt.
! status integer Out         Standard error/status value. Not currently set
!                            (no error conditions identified).
!
! History:
! 2001/07/10, AS: Original version
! 2001/10/22, AS: Bug fix: costs were not zeroed.
! 2012/01/12, CP: Changed zeroing to missing rather than 0
! 2014/05/21, GM: Cleaned up the code.
! 2015/05/25, GM: Got rid of flags Diagl and removed obvious comments.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Zero_Diag(Ctrl, Diag, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)    :: Ctrl
   type(Diag_t), intent(inout) :: Diag
   integer,      intent(out)   :: status

   status = 0

   Diag%QCFlag       = 0
   Diag%Iterations   = 0
   Diag%Jm           = 0
   Diag%Ja           = 0
   Diag%St           = 0
   Diag%Ss           = 0
   Diag%Y0           = MissingXn
   Diag%YmFit        = MissingXn
   Diag%YError       = 0
   Diag%APFit        = MissingXn
   Diag%AK           = 0
   Diag%cloud_albedo = sreal_fill_value

end subroutine Zero_Diag
