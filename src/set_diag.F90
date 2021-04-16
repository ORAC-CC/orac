!-------------------------------------------------------------------------------
! Name: set_diag.F90
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
! See ORAC_Constants for definitions of the flag indices DiFlagQC etc.
!
! Some values are simply the existing SPixel values (e.g. the first guess and
! a priori state vectors). These are not copied to Diag. The values can be
! taken from SPixel when required.
!
! Arguments:
! Name          Type       In/Out/Both  Description
! ------------------------------------------------------------------------------
! Ctrl          struct     In           Control structure. Contains parameter
!                                       limits for quality control.
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
! 2011/11/18, CP: Commented out Diag%y0 so it could be written to the output
!    file
! 2012/08/10, CP: Added in measurement array indices
! 2014/05/21, GM: Cleaned up the code.
! 2015/05/25, GM: Got rid of flags Diagl and removed obvious comments.
! 2015/07/28, AP: Remove status argument (as routine cannot feasibly fail).
! 2015/07/29, AP: Remove NPhaseChanges argument.
! 2015/07/31, AP: Rejig QCFlag for much longer state vector.
! 2015/11/19, GM: Add Legacy channels used to QCFlag (6 bits).
! 2017/07/12, AP: Completely new QC flags. Moved setting of Jm/Ja/iter to
!    InvertMarquadt. Removed MaxS constraint on setting last retrieval.
!
! Bugs:
! None known
!-------------------------------------------------------------------------------

subroutine Set_Diag(Ctrl, SPixel, MSI_Data, Diag)

   use Ctrl_m
   use Data_m
   use ORAC_Constants_m
   use SPixel_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(Data_t),   intent(in)    :: MSI_Data
   type(Diag_t),   intent(inout) :: Diag

   ! Local variables
   real    :: J, dof
   integer :: i


   Diag%QCFlag = 0
   J = Diag%Jm + Diag%Ja

   ! Degrees of freedom for noise
   dof = real(SPixel%Nx)
   do i = 1, SPixel%Nx
      dof = dof - Diag%AK(SPixel%X(i),SPixel%X(i))
   end do

   ! Only save retrievals for GetX SelmSAD if converged to low cost
   if (Diag%Converged .and. J < Ctrl%QC%MaxJ) then
      SPixel%XnSav      = SPixel%Xn
      SPixel%SnSav      = SPixel%Sn
      SPixel%Loc%LastX0 = SPixel%Loc%X0
      SPixel%Loc%LastY0 = SPixel%Loc%Y0
   end if

   ! Set quality control flags
   if (.not. Diag%Converged)         Diag%QCFlag = ibset(Diag%QCFlag, ConvBit)
   if (J > Ctrl%QC%MaxJ)             Diag%QCFlag = ibset(Diag%QCFlag, CostBit)
   if (MSI_Data%nisemask(SPixel%Loc%X0, SPixel%Loc%Y0) == 1) &
                                     Diag%QCFlag = ibset(Diag%QCFlag, IceBit)
   if (any(MSI_Data%cldmask(SPixel%Loc%X0, SPixel%Loc%Y0, :) == 1) .neqv. &
        (Ctrl%Approach == AppCld1L .or. Ctrl%Approach == AppCld2L)) &
                                     Diag%QCFlag = ibset(Diag%QCFlag, MaskBit)
   if (dof > Ctrl%QC%MaxDoFN)        Diag%QCFlag = ibset(Diag%QCFlag, DoFNBit)
   if (MSI_Data%dem(SPixel%Loc%X0, SPixel%Loc%Y0) > Ctrl%QC%MaxElevation) &
                                     Diag%QCFlag = ibset(Diag%QCFlag, ElevBit)
   if (any(SPixel%Geom%RelAzi < Ctrl%MinRelAzi) .and. &
        .not. (Ctrl%Approach == AppCld1L .or. Ctrl%Approach == AppCld2L)) &
                                     Diag%QCFlag = ibset(Diag%QCFlag, GlintBit)
   ! Cirrus contamination
   ! Case 2 waters
   ! Shadows


end subroutine Set_Diag
