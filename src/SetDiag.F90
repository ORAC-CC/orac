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
! Y             real array In           Calculated "measurements" from last
!                                       iteration of inversion.
! Sy            real array In           Error covariance in Y.
! Diag          struct     Both         Structure containing diagnostic values to
!                                       be set. Error arrays St and Ss are
!                                       already set by Invert_Marquardt.
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
! 2015/07/28, AP: Remove status argument (as routine cannot feasibly fail).
! 2015/07/29, AP: Remove NPhaseChanges argument.
! 2015/07/31, AP: Rejig QCFlag for much longer state vector.
! 2015/11/19, GM: Add Legacy channels used to QCFlag (6 bits).
!
! $Id$
!
! Bugs:
! If there are state vector elements in X_Twi or X_Night that are not in X_Dy,
! the setting of QCFlag will fail.
!-------------------------------------------------------------------------------

subroutine Set_Diag(Ctrl, SPixel, convergence, J, Jm, Ja, iter, Y, Sy, Diag)

   use Ctrl_m
   use ECP_Constants_m
   use Int_Routines_m, only : find_in_array
   use SPixel_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   logical,        intent(in)    :: convergence   ! Indicates whether the
                                                  ! inversion converged
   real,           intent(in)    :: J, Jm, Ja     ! Cost at final state (plus
                                                  ! contributions to cost from
                                                  ! measurements and a priori)
   integer,        intent(in)    :: iter          ! Inversion iteration counter
   real,           intent(in)    :: Y(:)          ! Calculated "measurements" at
                                                  ! final state.
   real,           intent(in)    :: Sy(:,:)       ! Error covariance in
                                                  ! measurements
   type(Diag_t),   intent(inout) :: Diag          ! Diagnostic structure

   ! Local variables

   integer :: m


   if (convergence) then
      Diag%Converged = 1

      if (J > Ctrl%QC%MaxJ) then
         ! Flag high cost retrievals
         Diag%QCFlag = ibset(Diag%QCFlag, CostBit)
      else
         SPixel%XnSav      = SPixel%Xn
         SPixel%SnSav      = SPixel%Sn
         SPixel%Loc%LastX0 = SPixel%Loc%X0
         SPixel%Loc%LastY0 = SPixel%Loc%Y0
      end if
   else
      Diag%Converged = 0
   end if

   ! Flag retrieved variables with excessive error (bit of flag set is the index
   ! of that state vector element in Ctrl%X_Dy; it is assumed that X_Tw and
   ! X_Ni are subsets of X_Dy).
   do m = 1, SPixel%Nx
      if (sqrt(SPixel%Sn(SPixel%X(m),SPixel%X(m))) > Ctrl%QC%MaxS(SPixel%X(m))) &
         Diag%QCFlag = ibset(Diag%QCFlag, &
                             find_in_array(Ctrl%X(1:Ctrl%Nx(IDay),IDay), &
                                           SPixel%X(m)))
   end do

   ! Flag legacy channels used *6 bits).
   do m = 1, N_Legacy
      if (find_in_array(Ctrl%Ind%Y_Id(SPixel%spixel_y_to_ctrl_y_index(1:SPixel%Ind%Ny)), &
                        Ctrl%Ind%Y_Id_legacy(m)) .gt. 0) then
         Diag%QCFlag = ibset(Diag%QCFlag, Ctrl%Nx(IDay) + m)
      end if
   end do

   Diag%Iterations = iter
   Diag%Jm = Jm
   Diag%Ja = Ja

!  do m = 1, SPixel%Ind%Ny
!     Diag%YError(m) = sqrt(Sy(m,m))
!  end do

!   Diag%APFit = SPixel%Xb - SPixel%Xn

end subroutine Set_Diag
