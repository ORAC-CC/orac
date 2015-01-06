!-------------------------------------------------------------------------------
! Name:
!    Set_Diag
!
! Description:
!    Sets values in the Diag structure, containing diagnostic outputs requested
!    from the Inversion. Ctrl structure contains flags used to switch on or off
!    setting or output of each diagnostic value.
!
! Arguments:
!    Name          Type       In/Out/Both  Description
!    Ctrl          struct     In           ECP control structure. Contains
!                                          parameter limits for quality control.
!    SPixel        struct     Both         Super-pixel structure. Needed for active
!                                          channels and state variables, latest
!                                          state and error covariance.
!    convergence   logical    In           Indicates whether inversion converged.
!    J, Jm, Ja     real       In           Costs (total, measurement and a priori)
!                                          from latest inversion iteration.
!    iter          integer    In           Inversion iteration counter
!    NPhaseChanges integer    In           Number of phase changes during inversion
!    Y             real array In           Calculated "measurements" from last
!                                          iteration of inversion.
!    Sy            real array In           Error covariance in Y.
!    Diag          struct     Both         Structure containing diagnostic values to
!                                          be set. Error arrays St and Ss are already
!                                          set by Invert_Marquardt.
!    status        int        Out          Standard error/status value. Not currently
!                                          set (no error conditions identified).
!
! Algorithm:
!    For each diagnostic variable
!       Check the flag in the Ctrl%Diagl array
!          Set the diagnostic value if requested
!    The QCflag is used for quality control on the solution.
!    This test also checks whether the solution X and it's errors should be
!    saved in SPixel for use in state-dependent setting of first guess/a priori
!    values.
!
!    See ECP_Constants for definitions of the flag indices DiFlagQC etc.
!
!    Some values are simply the existing SPixel values (e.g. the first guess and
!    a priori state vectors). These are not copied to Diag. The values can be
!    taken from SPixel when required.
!
! Local variables:
!    Name Type Description
!
! History:
!     5th Jul 2001, Andy Smith: Original version
!     6th Jul 2001, Andy Smith:
!       Sets SPixel values if solution state is good enough to save for SDAD
!       setting.
!     9th Jul 2001, Andy Smith:
!       Converted QCFlag setting to use F90 bit setting intrinsic functions.
!    **************** ECV work starts here *************************************
!    22nd Feb 2011, Andy Smith:
!       Re-applying changes made in late 2001/2002.
!     6th Dec 2001, Andy Smith:
!       Change to bit setting for QC flag: new definitions.
!       If no convergence, only bit (MaxStateVar+1) is set.
!       If cost exceeds max, only bit (MaxStateVar+2 is set).
!       Previously all "state variable" bits were set as well. State variable
!       bits are now set independently of the other two bits.
!       Moved setting of SPixel%SnSav to this routine.
!    18th Nov 2011, Caroline Poulsen:
!       Commented out Diag%y0 so it could be written to the output file.
!    10th Aug 2012, Caroline Poulsen:
!       Added in measurement array indices
!    21th May 2014, Greg McGarragh:
!       Cleaned up the code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Set_Diag(Ctrl, SPixel, convergence, J, Jm, Ja, iter, &
                    NPhaseChanges, Y, Sy, Diag, status)

   use Ctrl_def
   use Diag_def
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
   real,           intent(in)    :: Y(SPixel%Ind%Ny)
                                                  ! Calculated "measurements" at
                                                  ! final state.
   real,           intent(in)    :: Sy(SPixel%Ind%Ny, SPixel%Ind%Ny)
                                                  ! Error covariance in
                                                  ! measurements
   type(Diag_t),   intent(inout) :: Diag          ! Diagnostic structure
   integer,        intent(out)   :: status

   ! Local variables

   integer :: m

   status = 0

   ! Set diagnostic values
   ! 1) Quality control flag: bits 1-5 represent state variables (bit set to 1
   !    indicates retrieval error > allowed max).
   !    bit MaxStateVar+1 set to 1 if no convergence
   !    bit MaxStateVar+2 set to 1 if the cost at the solution > allowed max.

   Diag%QCFlag = 0
   if (Ctrl%Diagl(DiFlagQC) > 0) then
      if (convergence) then
         ! Does the solution cost exceed the allowed max?
         ! Cost too great: set bit 7.

         if (J > Ctrl%QC%MaxJ) then
	    Diag%QCFlag = ibset(Diag%QCFlag, MaxStateVar+2)
         else
            ! Solution converged and cost was ok. Save for use in SDAD first
            ! guess and a priori setting.

	    SPixel%XnSav      = SPixel%Xn
            SPixel%SnSav      = SPixel%Sn
	    SPixel%Loc%LastX0 = SPixel%Loc%X0
	    SPixel%Loc%LastY0 = SPixel%Loc%Y0
         end if
      else
         ! No convergence, set bit 6

	 Diag%QCFlag = ibset(Diag%QCFlag, MaxStateVar+1)
      end if

      ! Check retrieval errors. Take square root of SPixel%Sn: check in units
      ! rather than units squared.

      do m = 1, SPixel%Nx
	 if (sqrt(SPixel%Sn(SPixel%X(m),SPixel%X(m))) &
	     > Ctrl%QC%MaxS(SPixel%X(m)) ) &
            Diag%QCFlag = ibset(Diag%QCFlag, SPixel%X(m))
      end do
   end if

   !  2), 3) and 4) Iterations, phase changes and costs

   if (Ctrl%Diagl(DiFlagIter) > 0) then
      Diag%Iterations = iter
   end if

   if (Ctrl%Diagl(DiFlagPhCh) > 0) then
      if (.not. convergence .and. NPhaseChanges == Ctrl%Invpar%MaxPhase) then
         Diag%PhaseChanges = -1
      else
         Diag%PhaseChanges = NPhaseChanges
      end if
   end if

   if (Ctrl%Diagl(DiFlagCost) > 0) then
      Diag%Jm = Jm
      Diag%Ja = Ja
   end if

   ! 5) or 7) State expected error from measurements (square roots of diagonals
   ! or full matrix). Set by Invert_Marquardt.

   ! 6) or 8) State expected error from model parameter noise (square roots of
   ! diagonals or full matrix). Set by Invert_Marquardt.

   ! 9) and 10) Measurement and a priori fit

   if (Ctrl%Diagl(DiFlagYFit) > 0) then
!     Diag%YmFit(1:SPixel%Ind%Ny) = Y(1:SPixel%Ind%Ny)-SPixel%Ym(1:SPixel%Ind%Ny)
   end if

!  if (Ctrl%Diagl(DiFlagYFit) > 0) then
!     Diag%Y0(1:SPixel%Ind%Ny) = Y(1:SPixel%Ind%Ny)
!  end if

   if (Ctrl%Diagl(DiFlagXFit) > 0) then
      Diag%APFit = SPixel%Xb - SPixel%Xn
   end if

   ! 11) and 12) A priori and first guess values. No need to store in Diag as
   ! these are available in SPixel.

   ! 13) and 14) A priori and measurement errors (roots of Sy and Sx)
   ! Sx available from SPixel, just set YError.

   if (Ctrl%Diagl(DiFlagSy) > 0) then
      do m = 1, SPixel%Ind%Ny
         Diag%YError(m) = sqrt(Sy(m,m))
      end do
   end if

end subroutine Set_Diag
