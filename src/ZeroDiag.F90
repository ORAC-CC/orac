!-------------------------------------------------------------------------------
! Name:
!    Zero_Diag
!
! Description:
!    Sets values in the Diag structure to zero. Called when a given super-pixel
!    is not to be processed, so that the structure can be output for every
!    SPixel. Zeroing the structure clears out values left from the previous
!    inversion. Only diagnostic outputs requested are zeroed. Ctrl structure
!    contains flags used to switch on or off setting or output of each
!    diagnostic value.
!
! Arguments:
!    Name   Type    In/Out/Both Description
!    Ctrl   struct  In          ECP control structure. Contains parameter limits
!                               for quality control.
!    Diag   struct  Both        Structure containing diagnostic values to be
!                               set. Error arrays St and Ss are already set by
!                               Invert_Marquardt.
!    status integer Out         Standard error/status value. Not currently set
!                               (no error conditions identified).
!
! Algorithm:
!    for each diagnostic variable
!       check the flag in the Ctrl%Diagl array: if requested
!          set the diagnostic value to zero
!
!    N.B. 0 may be an inappropriate value for some quantities (errors, fit etc).
!
!    See ECP_Constants for definitions of the flag indices DiFlagQC etc.
!
! Local variables:
!    Name Type Description
!
! History:
!    10th Jul 2001, Andy Smith: Original version
!    22nd Oct 2001, Andy Smith:
!       Bug fix: costs were not zeroed.
!    12th Jan 2012, Caroline Poulsen:
!       Changed zeroing to missing rather than 0
!    21th May 2014, Greg McGarragh:
!       Cleaned up the code.
!    25th May 2015, Greg McGarragh:
!       Got rid of flags Diagl and removed obvious comments.
!
! Bugs:
!    None known.
!
! $Id$
!
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
   Diag%PhaseChanges = 0
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
