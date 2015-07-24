!-------------------------------------------------------------------------------
! Name: SetDiag.F90
!
! Purpose:
! Sets values in the Diag structure, containing diagnostic outputs requested
! from the Inversion. Ctrl structure contains flags used to switch on or off
! setting or output of each diagnostic value.
!
! Description and Algorithm details:
! For each diagnostic variable
!    Check the flag in the Ctrl%Diagl array
!       Set the diagnostic value if requested
! The QCflag is used for quality control on the solution.
! This test also checks whether the solution X and it's errors should be
! saved in SPixel for use in state-dependent setting of first guess/a priori
! values.
!
! See ECP_Constants for definitions of the flag indices DiFlagQC etc.
!
! Some values are simply the existing SPixel values (e.g. the first guess and
! a priori state vectors). These are not copied to Diag. The values can be
! taken from SPixel when required.
!
! Arguments:
! Name          Type       In/Out/Both  Description
! ------------------------------------------------------------------------------
! Ctrl          struct     In           ECP control structure. Contains
!                                       parameter limits for quality control.
! SPixel        struct     Both         Super-pixel structure. Needed for active
!                                       channels and state variables, latest
!                                       state and error covariance.
! convergence   logical    In           Indicates whether inversion converged.
! J, Jm, Ja     real       In           Costs (total, measurement and a priori)
!                                       from latest inversion iteration.
! iter          integer    In           Inversion iteration counter
! NPhaseChanges integer    In           Number of phase changes during inversion
! Y             real array In           Calculated "measurements" from last
!                                       iteration of inversion.
! Sy            real array In           Error covariance in Y.
! Diag          struct     Both         Structure containing diagnostic values to
!                                       be set. Error arrays St and Ss are
!                                       already set by Invert_Marquardt.
! status        int        Out          Standard error/status value. Not
!                                       currently set.
!
! History:
! 2001/07/05, AS: Original version
! 2001/07/06, AS: Sets SPixel values if solution state is good enough to save 
!    for SDAD setting.
! 2001/07/09, AS: Converted QCFlag setting to use F90 bit setting intrinsic 
!    functions.
!    **************** ECV work starts here *************************************
! 2011/02/22, AS: Re-applying changes made in late 2001/2002.
! 2001/12/06, AS: Change to bit setting for QC flag: new definitions. If no 
!    convergence, only bit (MaxStateVar+1) is set. If cost exceeds max, only bit
!    (MaxStateVar+2 is set). Previously all "state variable" bits were set as 
!    well. State variable bits are now set independently of the other two bits. 
!    Moved setting of SPixel%SnSav to this routine.
! 2011/11/18, CP: Commented out Diag%y0 so it could be written to the output file
! 2012/08/10, CP: Added in measurement array indices
! 2014/05/21, GM: Cleaned up the code.
! 2015/05/25, GM: Got rid of flags Diagl and removed obvious comments.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_Diag(Ctrl, SPixel, convergence, J, Jm, Ja, iter, &
                    NPhaseChanges, Y, Sy, Diag, status)

   use Ctrl_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   logical,        intent(in)    :: convergence   ! Indicates whether the
                                                  ! inversion converged
   real,           intent(in)    :: J, Jm, Ja     ! Cost at final state (plus
                                	          ! contributions to cost from
			        	          ! measurements and a priori)
   integer,        intent(in)    :: iter	  ! Inversion iteration counter
   integer,        intent(in)    :: NPhaseChanges ! Phase change counter
   real,           intent(in)    :: Y(:)          ! Calculated "measurements" at
                                                  ! final state.
   real,           intent(in)    :: Sy(:,:)       ! Error covariance in
                                                  ! measurements
   type(Diag_t),   intent(inout) :: Diag          ! Diagnostic structure
   integer,        intent(out)   :: status

   ! Local variables

   integer :: m


   status = 0

   if (convergence) then
      if (J > Ctrl%QC%MaxJ) then
         Diag%QCFlag = ibset(Diag%QCFlag, MaxStateVar+2)
      else
         SPixel%XnSav      = SPixel%Xn
         SPixel%SnSav      = SPixel%Sn
         SPixel%Loc%LastX0 = SPixel%Loc%X0
         SPixel%Loc%LastY0 = SPixel%Loc%Y0
      end if
   else
      Diag%QCFlag = ibset(Diag%QCFlag, MaxStateVar+1)
   end if

   do m = 1, SPixel%Nx
      if (sqrt(SPixel%Sn(SPixel%X(m),SPixel%X(m))) > Ctrl%QC%MaxS(SPixel%X(m))) &
         Diag%QCFlag = ibset(Diag%QCFlag, SPixel%X(m))
   end do

   Diag%Iterations = iter

   if (.not. convergence .and. NPhaseChanges == Ctrl%Invpar%MaxPhase) then
      Diag%PhaseChanges = -1
   else
      Diag%PhaseChanges = NPhaseChanges
   end if

   Diag%Jm = Jm
   Diag%Ja = Ja

   do m = 1, SPixel%Ind%Ny
      Diag%YError(m) = sqrt(Sy(m,m))
   end do

   Diag%APFit = SPixel%Xb - SPixel%Xn

end subroutine Set_Diag
