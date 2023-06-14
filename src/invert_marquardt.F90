!-------------------------------------------------------------------------------
! Name: invert_marquardt.F90
!
! Purpose:
! Find the "best" fitting state vector for a set of measurements in the ORAC.
!
! Description and Algorithm details:
! Uses a Marquardt descent algorithm to find a state vector that gives a good
! match to the measurements (Y values across all selected channels) for a
! particular image super-pixel.
!
! The best fit is identified by minimising the "cost".
!
! The full cost function J is defined as:
!   J(x) = (y(x) - ym) * inverse(Sy) * transpose(y(x) - ym) +
!            (x - xb)  * inverse(Sx) * transpose(x - xb) +
!            (bt - b)  * inverse(Sb) * transpose(bt - b)
! J is minimised with respect to x, hence the third component (the model
! parameter vector b) is omitted from this routine.
!
! The state vector X is set to some initial (first guess) values X0, and the
! forward model FM is run to calculate Y(X0).  Y(X0) is compared with the
! measured Y to give the initial "cost", J0
!
! Set Xn = X0
!
! calculate 1st and 2nd derivatives of J
! set starting Marquardt parameters (starts with heaviest weighting on
!    steepest descent)
!
! for a number of iterations (up to a maximum or until convergence is
! reached)
!    calculate step delta_X
!    set new X = Xn + delta_X
!    Check bounds: if any variable in X reaches a limit it is fixed at
!       that limit
!
!    call FM to give Y(X), K(X)
!
!    calculate J(X)
!    if (J(X) < J0)    (i.e. "good" step)
!       set Xn = X     (i.e. old Xn + dX)
!       calculate new 2nd derivative of J
!       if (J - J0) < (limit from Ctrl struct)
!          convergence: drop out of loop
!       else
!          decrease steepest descent
!          calculate new 1st derivatives of J
!          save current J as J0 for checking next iteration
!    else ("bad" step, don't keep X)
!       increase weighting on steepest descent for next iteration
! end of iteration loop
!
! Calculate the two terms contributing to the overall retrieval error Sn:
!    Sn = St + Ss
!    St is the inverse Hessian (d2J_dX2) for the active state variables
!    Ss is the state expected error from model parameter noise (inactive
!       state variables and surface reflectance Rs - no other parameters
!       available at present).  Ss depends whether there are inactive
!       state variables for the SPixel and whether Equivalent Model
!       Parameter Noise from Rs is selected.
!
! Set diagnostics for output
!
! Note, for output, costs are divided by Ny for the SPixel. Hence convergence
! criteria on delta_J are multiplied up by Ny for the inversion.  Otherwise,
! if cost was divided by Ny at each iteration this would add more processing
! per step and would need to be taken into account in the derivatives of J as
! well as J.
!
! Errors in calculations terminate processing this pixel, managed by ORAC.
! If a non-zero status occurs during an inversion, the Diag%QC flag is set to
! indicate non-convergence and uncertainty outputs are set to fill values.
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure, contains Marquardt
!                              parameters, scaling factors etc.
! SPixel   struct  Both        Super pixel data. Contains current
!                              measurements, active state variables, first
!                              guess and a priori info.
! SAD_Chan array   In          Measurement channel characteristics for each
!          of structs          channel.
! SAD_LUT  struct  In          Cloud radiative property look up tables for each
!          of structs          layer.
! RTM_Pc   struct  In          Radiative transfer model data interpolated to
!          of structs          the current cloud pressure Pc for each layer.
! Diag     struct  In          Diagnostic structure
! status   integer Both        ORAC status/error flag.
!
! History:
! 2001/04/05, AS: original version
! 2001/06/12, AS: First complete version. Compiles but not fully debugged.
! 2001/06/29, AS: Passes simple tests. Added breakpoint outputs. Found a problem
!    with handling of phase changes (checking into RCS before attempting fix).
! 2001/06/29, AS: Phase change section updated. Added Diag structure as output.
!    Added error analysis (in Diag setting).
! 2001/10/12, AS: Costs are now divided by SPixel%Ny for output. Allows better
!    comparison between SPixels (since Ny may vary). Since costs are not divided
!    during calculation for each iteration, the convergence criteria are
!    multiplied by Ny to compensate. Notes added in code and Algorithm section.
!    Also added brief description of error analysis to Algorithm section.
!    **************** ECV work starts here *************************************
! 2011/02/22, AS: Re-applying changes made in late 2001/2002.
! 2001/12/05, AS: Introduced error checking throughout. See comments above for
!    details of the scheme implemented.
! 2011/12/21, AS: Changing use of iteration counter so that only "good" steps
!    are counted as "iterations" (experimentally). The number of steps is still
!    checked against the Max allowed no. of iterations at present. (Subsequently
!    removed, so ignored for 2011 update).
! 2011/02/24, AS: stat now initialised (was done in ORAC code somewhere along
!    the way).
! 2011/03/17, AS: Removal of phase change. Code commented out for now, prior to
!    eventual removal. SAD_LUT dimension reduced to 1, since only 1 cloud class
!    in use.
! 2011/03/22, AS: Removal of phase change, phase 2. SAD_CloudClass now has 1
!    dimension rather than N cloud classes, SAD_LUT also 1 since only 1
!    phase/cloud class is used for the whole retrieval.
! 2011/04/06, AS: Removal of redundant selection methods SAD and SDAD for limits
!    checking. SADCloudClass argument to Set_Limits no longer required.
!    SADCloudClass argument to this function InvertMarquardt also redundant.
!    Mopping up from removal of phase change. Removed setting of SPixel%FGPhase
!    as it is redundant.
! 2011/06/08, AS: Removed logging of errors from Invert_Cholesky and related
!    functions, to prevent text file output slowing down execution (use ifdef
!    DEBUG).
! 2011/08/08, CP: remove ref to cloud class
! 2011/10/03, CP: fixed debug syntax error
! 2011/10/18, CP: added lat/lon info to bkp output
! 2011/12/13, CP: change format statement to make g95 compatible
! 2012/01/19, CP: stored first guess measurement. add variable minusdJ_dX=-dJ_dX.
!    changed calls to Invert_cholesky
! 2012/02/24, CP: changed error_matrix call to Invert_cholesky
! 2012/08/10, CP: defined measurement arrays more explicitly other wise crashed
!    for night measurements
! 2012/09/14, CP: bug fix defined Diag%ss to size of nx elements (was ny)
!    initialised Y
! 2012/10/02, CP: initialised variables bug fix defined Diag%ss changed how
!    SPixel%sn calculated
! 2013/05/08, CP: set Diag%Y0
! 2014/01/15, GM: Changed Invert_Cholesky() argument Diag%St to
!    Diag%St(1:SPixel%Nx, 1:SPixel%Nx) when inverting d2J_dX2 to get Diag%St.
!    Using just Diag%St results in use of garbage when there are inactive state
!    variables.
! 2014/01/15, GM: Corrected the dimensions of the assignment for the calculation
!    of Diag%Ss from Diag%Ss(1:SPixel%Ind%Ny,1:SPixel%Ind%Ny) to
!    Diag%Ss(1:SPixel%Nx,1:SPixel%Nx).
! 2014/01/15, GM: Set values of J, Jm, and Ja to MissingSn in the case of failed
!    retrievals as is done with the other values in Diag.
! 2014/01/15, GM: Moved dynamic setting of the upper limit for CTP to the
!    highest pressure in the profile from FM() to this routine.
! 2014/01/17, GM: Cleaned up code.
! 2014/01/29, MJ: Fixed case where alpha can get out of bounds.
! 2014/02/27, CP: Added declaration of J.
! 2014/04/02, CP: Fixed bug where temp was not reassigned.
! 2014/04/02, MJ: Fixed bug where Diag%ss was not initialized.
! 2014/07/24, AP: Removed unused status variable.
! 2014/07/24, CP: Added in cloud albedo
! 2014/11/20, AP: Multiply X|Ydiff by -1, making minusJ_dX redundant.
! 2015/01/09, AP: Patch memory leak with cloud_albedo.
! 2015/07/28, AP: Add multiple of unit matrix with add_unit function. Put
!    calculation of Hessian into it's own routine.
! 2015/07/28, AP: Replace if status blocks with go to 99 to terminate
!    processing. Reorganise main iteration loop to minimise code repetition.
! 2015/07/29, AP: Remove mentions of phase changes. Clean algorithm description.
!    Add RAL's false convergence test (as it's used by the aerosol retrieval).
! 2015/08/21, AP: Variables to include in Jacobian (but not retrieve) now listed
!    in SPixel%XJ. Uncertainty due to inactive state elements tidied.
! 2015/10/21, GM: Pulled evaluation of cloud albedo out of the FM and into here
!    after the retrieval iteration.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/11/18, GM: Add call to Calc_Corrected_CTX().
! 2016/01/05, AP: The convergence test should re-evaluate dJ_dX when failed.
! 2016/01/07, AP: Add output of diffuse fraction of illumination and AOT870.
! 2016/01/27, GM: Compute ecc and ecc uncertainty.
! 2016/01/27, GM: Compute corrected CTH for both day and night.
! 2016/02/18, OS: Making sure CEE is within 0-1.
! 2016/02/24, OS: Avoid negative CEE uncertainties.
! 2016/05/03, AP: Add output of AOD at a second wavelength.
! 2016/07/27, GM: Changes related to the multilayer retrieval support.
! 2017/07/12, AP: New QC.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Invert_Marquardt(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, Diag, stat)

   use Cholesky_m
   use Ctrl_m
   use Diag_m
   use FM_Routines_m
   use GZero_m
   use Int_LUT_Routines_m
   use ORAC_Constants_m
   use RTM_Pc_m
   use SAD_Chan_m
   use SAD_LUT_m
   use SPixel_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT(:)
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc(:)
   type(Diag_t),     intent(out)   :: Diag
   integer,          intent(out)   :: stat

   ! Local variable declarations

   integer :: m, l                ! counter for short/implied do loops
                                  ! (can't use J: it's the cost function)
   real    :: Y(SPixel%Ind%Ny)    ! TOA reflectances, radiances etc for partly-
                                  ! cloudy conditions. Returned by FM
   real    :: dY_dX(SPixel%Ind%Ny,MaxStateVar)
                                  ! Derivatives d[ref]/d[tau,Re,pc,f,Ts,Rs]t
   real    :: Kx(SPixel%Ind%Ny, SPixel%Nx)
                                  ! Scaled, active part of dY_dX
   real    :: Kj(SPixel%Ind%Ny, SPixel%NXJ)
                                  ! Scaled, surface reflectance part of dY_dX
   real    :: Sj(Spixel%NXJ, SPixel%NXJ)
                                  ! Error covariance in parameters
   real    :: Sy(SPixel%Ind%Ny, SPixel%Ind%Ny)
                                  ! Error covariance in measurements
   real    :: SyInv(SPixel%Ind%Ny, SPixel%Ind%Ny)
                                  ! Inverse of Sy
   real    :: error_matrix(SPixel%Nx, SPixel%Nx)
   real    :: SxInv(SPixel%Nx, SPixel%Nx)
                                  ! Inverse of Sx. Sx is the error covariance
                                  ! in the a priori state vector, part of SPixel
   real    :: Ydiff(SPixel%Ind%Ny)
                                  ! Difference between measured and calculated Y
   real    :: Xdiff(SPixel%Nx)
                                  ! Difference between current state and a
                                  ! priori
   real    :: J, Jm, Ja           ! Cost at a given state (plus contributions to
                                  ! cost from measurements and a priori)
   real    :: J0                  ! Cost at first guess state
   real    :: Ccj_Ny              ! Product of Cost convergence criteria and
                                  ! number of active channels.
   integer :: iter                ! Inversion iteration counter
   real    :: KxT_SyI(SPixel%Nx, SPixel%Ind%Ny)
                                  ! Product of the transpose of Kx with SyInv
   real    :: dJ_dX(SPixel%Nx)    ! 1st derivative of J wrt state variables
   real    :: d2J_dX2(SPixel%Nx, SPixel%Nx)
                                  ! 2nd derivative of J wrt state variables
   real    :: alpha               ! Marquardt control variable
   real    :: huge_value          ! largest value of real data type
   real    :: J2plus_A(SPixel%Nx, SPixel%Nx)
                                  ! Temporary array to hold sum of d2J_dX2 and
                                  ! alpha * unit.
   real    :: delta_X(SPixel%Nx)
                                  ! Step to be applied to state variables
   real    :: Xplus_dX(MaxStateVar)
                                  ! State vector being checked in a given
                                  ! iteration (Xn + delta_X)
   real    :: delta_J             ! Change in cost between operations
   real    :: Dy(SPixel%Nx, SPixel%Ind%Ny)
                                  ! "Inversion operator" used in error analysis
   real    :: Kb(SPixel%Ind%Ny, SPixel%NxI)
                                  ! "Model parameter" gradients from inactive
                                  ! state variables and surface reflectance.
   real    :: Dy_Kb(SPixel%Nx, SPixel%NxI)
                                  ! Product of Dy and Kb.
   real    :: Sb(SPixel%NxI, SPixel%NxI)
                                  ! "Model parameter" error covariance.
   real    :: St_temp(SPixel%Nx, SPixel%Nx)
                                  ! Array temporary for Diag%St
   integer :: limit_hit           ! Indicates if state vector hits a limit
   real, dimension(SPixel%Ind%NSolar)    :: CRP, T_00, T_0d, T_all
   real, dimension(SPixel%Ind%NSolar, 2) :: d_CRP, d_T_00, d_T_0d
   real    :: CRP_thermal(SPixel%Ind%NThermal)
   real    :: d_CRP_thermal(SPixel%Ind%NThermal, 2)
   real    :: temp(SPixel%Ind%NSolar,SPixel%Ind%NSolar)
   real    :: temp_thermal(SPixel%Ind%NThermal,SPixel%Ind%NThermal)
   real    :: BextRat(1), d_BextRat(1)
   type(GZero_t) :: GZero

   ! Initialise
   stat      = 0

   Y         = 0.
   dY_dX     = 0.
   Kx        = 0.
   Kj        = 0.
   KxT_SyI   = 0.
   J         = 0.
   d2J_dX2   = 0.
   Diag%St   = 0.
   Diag%Ss   = 0.
   SPixel%Sn = 0.

   huge_value = huge(1.0)/Ctrl%Invpar%MqStep

   ! Set state variable limits
   call Set_Limits(Ctrl, SPixel, SAD_LUT, stat)
   if (stat /= 0) go to 99 ! Terminate processing this pixel

   ! Check for logarithmic LUT axes
   if (SAD_LUT(1)%Grid%Tau%log) then
      SPixel%XB(ITau) = log10(SPixel%XB(ITau))
      SPixel%X0(ITau) = log10(SPixel%X0(ITau))
   end if
   if (Ctrl%Approach == AppCld2L .and. SAD_LUT(2)%Grid%Tau%log) then
      SPixel%XB(ITau2) = log10(SPixel%XB(ITau2))
      SPixel%X0(ITau2) = log10(SPixel%X0(ITau2))
   end if
   if (SAD_LUT(1)%Grid%Re%log) then
      SPixel%XB(IRe) = log10(SPixel%XB(IRe))
      SPixel%X0(IRe) = log10(SPixel%X0(IRe))
   end if
   if (Ctrl%Approach == AppCld2L .and. SAD_LUT(2)%Grid%Re%log) then
      SPixel%XB(IRe2) = log10(SPixel%XB(IRe2))
      SPixel%X0(IRe2) = log10(SPixel%X0(IRe2))
   end if

   ! Invert a priori covariance matrix
   error_matrix = SPixel%Sx(SPixel%X, SPixel%X)
   call Invert_Cholesky(error_matrix, SxInv, SPixel%Nx, stat)
   if (stat /= 0) then
#ifdef DEBUG
      write(*, *) 'ERROR: Invert_Marquardt(): Error in Invert_Cholesky'
#endif
      stat = InvMarquardtSxErr
      go to 99 ! Terminate processing this pixel
   end if

   ! Set parameter covariance matrix
   if (SPixel%NXJ > 0) Sj = SPixel%Sx(SPixel%XJ, SPixel%XJ)


   ! ************ START EVALUATING FORWARD MODEL ************

   ! Evaluate forward model for the first guess and initialise the various
   ! arrays. X should be unscaled when passed into FM.
   call FM(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, SPixel%X0, Y, dY_dX, stat)
   if (stat /= 0) then
      stat = InvMarquardtFMX0Err
      go to 99 ! Terminate processing this pixel
   end if

   ! Store measurement first guess
   Diag%Y0(1:SPixel%Ind%Ny) = Y

   ! Convert dY_dX to Kx and Kj.
   call Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kj)

   ! Set covariance matrix Sy. Always call Set_Kx before Set_Sy. Needs Kj.
   call Set_Sy(Ctrl, SPixel, Kj, Sj, Sy)

   ! Calculate SyInv and SxInv
   call Invert_Cholesky(Sy, SyInv, SPixel%Ind%Ny, stat)
   if (stat /= 0) then
      stat = InvMarquardtSyX0Err
      go to 99 ! Terminate processing this pixel
   end if

   ! Xdiff, the difference between the current state vector X and
   ! the a priori state vector SPixel%Xb for the active state variables.
   ! Requires scaling to produce a properly balanced matrix
   Xdiff = (SPixel%Xb(SPixel%X) - SPixel%X0(SPixel%X)) &
           * Ctrl%Invpar%XScale(SPixel%X)

   ! Calculate Ydiff, the difference between the measurements and calculated
   ! values for X.
   Ydiff = SPixel%Ym - Y
   Diag%YmFit(1:SPixel%Ind%Ny) = -Ydiff

   ! Calculate cost at X0.
   Ja = dot_product(Xdiff, matmul(SxInv, Xdiff))
   Jm = dot_product(Ydiff, matmul(SyInv, Ydiff))
   J0 = Jm + Ja

   ! ************* END EVALUATING FORWARD MODEL *************


   ! ************* START MAIN ITERATION LOOP *************

   ! Set Xn = X0 for 1st iteration
   SPixel%Xn = SPixel%X0

   ! Adjust cost convergence criteria for the number of active channels in the
   ! super-pixel, since final costs will be divided by Ny for output.
   Ccj_Ny = Ctrl%Invpar%Ccj * SPixel%Ind%Ny

   ! Initialise loop control variables
   Diag%Converged   = .false.
   iter = 1

   ! Calculate matrix operators
   ! [matmul(SxInv, Xdiff) is effectively SxInv * transpose(Xdiff)]
   KxT_SyI = matmul(transpose(Kx), SyInv)
   dJ_dX   = matmul(KxT_SyI, Ydiff) + matmul(SXInv, Xdiff)
   d2J_dX2 = matmul(KxT_SyI, Kx) + SxInv

   ! Set Marquardt parameter (initial weighting favours steepest descent) with
   ! average of leading diagonal of Hessian d2J_dX2.
   alpha = average_hessian(SPixel%Nx, d2J_dX2, Ctrl%Invpar%MqStart)

   do
      ! Use alpha and d2J_dX2 to set delta_X. Solve_Cholesky is used to find
      ! delta_X. The equation from the ATBD is
      !    dX = -(J'' + alphaI)^-1 * J' or delta_X = - (inv(J2plus_A) * dJ_dX).
      ! Multiplying through by J2_plusA we get:
      !    J2_plusA * delta_X = -dJ_dX,
      ! which we can solve for delta_X using Solve_Cholesky.
      call add_unit(SPixel%Nx, d2J_dX2, alpha, J2plus_A)
      call Solve_Cholesky(J2plus_A, dJ_dX, delta_X, SPixel%Nx, stat)
      if (stat /= 0) then
#ifdef DEBUG
         write(*, *) 'ERROR: Invert_Marquardt(): Error in Solve_Cholesky'
#endif
         stat = InvMarquardtDJErr
         go to 99 ! Terminate processing this pixel
      end if
      ! De-scale delta_X so that the X passed to FM can be kept un-scaled
      delta_X = delta_X / Ctrl%Invpar%XScale(SPixel%X)

      ! Apply step delta_x to the active state variables. Assumes Xn and
      ! delta_X are both unscaled.
      Xplus_dX(SPixel%X)  = SPixel%Xn(SPixel%X) + delta_X
      if (SPixel%NXJ > 0) Xplus_dX(SPixel%XJ) = SPixel%Xn(SPixel%XJ)
      if (SPixel%NXI > 0) Xplus_dX(SPixel%XI) = SPixel%Xn(SPixel%XI)

      ! Check bounds for active state variables - does delta_X take any state
      ! variable outside it's range? (If so, freeze it at the boundary).
      call Check_Limits(Ctrl, Xplus_dX, SPixel, limit_hit)
      if (limit_hit /= 0) Diag%LimitHit = limit_hit

      ! ************ START EVALUATE FORWARD MODEL ************
      call FM(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, Xplus_dX, Y, dY_dX, stat)
      if (stat /= 0) then
         stat = InvMarquardtFMErr
         go to 99 ! Terminate processing this pixel
      end if

      ! Set new Kx, Kj, Sy and SyInv
      call Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kj)
      call Set_Sy(Ctrl, SPixel, Kj, Sj, Sy)
      call Invert_Cholesky(Sy, SyInv, SPixel%Ind%Ny, stat)
      if (stat /= 0) then
#ifdef DEBUG
         write(*, *) 'ERROR: Invert_Marquardt(): Error in Invert_Cholesky'
#endif
         stat = InvMarquardtSyErr
         go to 99 ! Terminate processing this pixel
      end if

      ! Calculate new cost, J.
      Xdiff = (SPixel%Xb(SPixel%X) - Xplus_dX(SPixel%X)) &
              * Ctrl%Invpar%XScale(SPixel%X)
      Ydiff = SPixel%Ym - Y
      Diag%YmFit(1:SPixel%Ind%NY) = -Ydiff

      Ja = dot_product(Xdiff, matmul(SxInv, Xdiff))
      Jm = dot_product(Ydiff, matmul(SyInv, Ydiff))
      J  = Jm + Ja
      delta_J = J-J0
      ! ************* END EVALUATE FORWARD MODEL *************

      ! Update Levenberg-Marquadt constant (alpha) based on cost
      if (alpha == 0.) then
         ! Impliment Caroline and Richard's method of dealing with false
         ! convergences:
         ! (i)  If Marquadt converged, do another pure Gauss-Newton step.
         ! (ii) If that changes the cost by less than one, we've converged.
         !      Otherwise, re-initialise the Marquadt parameter and continue.
         if (abs(delta_J) < 1) then ! ACP: ccj_ny?
            if (delta_J < 0 .or. Ctrl%Invpar%always_take_GN) then
               ! If Newton step improved the cost, use it's solution.
               SPixel%Xn = Xplus_dX
               KxT_SyI   = matmul(transpose(Kx), SyInv)
               d2J_dX2   = matmul(KxT_SyI, Kx) + SxInv
            end if

            Diag%Converged = .true.
            exit
         else
            ! Otherwise, reset Levenberg constant and continue iterating
            alpha = average_hessian(SPixel%Nx, d2J_dX2, Ctrl%Invpar%MqStart)
         end if
      else if (J <= J0) then
         ! Improvement in cost. Store current solution
         SPixel%Xn = Xplus_dX

         ! Recalculate matrix operators
         KxT_SyI = matmul(transpose(Kx), SyInv)
         d2J_dX2 = matmul(KxT_SyI, Kx) + SxInv

         ! Check for convergence.
         if (abs(delta_J) <= Ccj_Ny) then
            if (Ctrl%Invpar%ConvTest) then
               ! Use CP/RS convergence test and perform Gauss-Newton iteration
               alpha = 0.
               if (Ctrl%Invpar%dont_iter_convtest) iter = iter - 1
            else
               ! Retrieval converged. Exit loop
               Diag%Converged = .true.
               exit
             end if
         else
            ! Not converged so decrease steepest descent part for next iteration
            alpha = alpha / Ctrl%Invpar%MqStep
         end if

         J0 = J
         dJ_dX = matmul(KxT_SyI, Ydiff) + matmul(SXInv, Xdiff)
      else
         ! No improvement in cost. Reject solution

         ! Increase steepest descent part for next iteration.
         ! "if" inserted to catch (academic?) case of overflow if alpha gets huge
         if (alpha .lt. huge_value) alpha = alpha * Ctrl%Invpar%MqStep
      end if

      if (iter == Ctrl%Invpar%MaxIter .or. Diag%Converged) exit

      iter = iter + 1
   end do

   ! ************* END MAIN ITERATION LOOP *************


   ! ************* START ERROR ANALYSIS *************

   ! State expected error from measurements (inverse of Hessian)
   call Invert_Cholesky(d2J_dX2, St_temp, SPixel%Nx, stat)
   Diag%St(1:SPixel%Nx, 1:SPixel%Nx) = St_temp
   if (stat /= 0) then
#ifdef DEBUG
      write(*, *) 'ERROR: Invert_Marquardt(): Error in Invert_Cholesky'
#endif
      stat = InvMarquardtD2JErr
      go to 99 ! Terminate processing this pixel
   end if

   ! Ss is the state expected error from model parameter noise (inactive state
   ! variables and surface reflectance Rs - this is the only model parameter for
   ! which gradient information is returned). Rs part is set only if Eqmpn for
   ! surface reflectance was not used.

   ! Combine the inactive state variables part of Kx and surface reflectance
   ! gradients (if required) to get Kb.

   ! Set the error covariance Sb. Sx is full size (MaxStateVar): pick out the
   ! inactive variables parts. Write into "top left corner" of Sb rather than
   ! use active/inactive sections. Initialise the whole array to 0 so that
   ! the terms for correlation between the Rs/XI terms are 0.

   if (SPixel%NXI > 0) then
      Dy = matmul(St_temp, KxT_SyI) ! The gain matrix

      ! Kx isn't actually set for inactive vars so we use a scaled dY_dX.
      do m = 1, SPixel%NXI
         Kb(:,m) = dY_dX(:,SPixel%XI(m)) / Ctrl%Invpar%XScale(SPixel%XI(m))
      end do
      Dy_Kb = matmul(Dy, Kb)

      Sb = SPixel%Sx(SPixel%XI,SPixel%XI)
      Diag%Ss(1:SPixel%Nx,1:SPixel%Nx) = matmul(Dy_Kb, &
                                                matmul(Sb, transpose(Dy_Kb)))
   end if

   ! Assign St+Ss to the parts of SPixel%Sn where state vars are active. For
   ! inactive variables, Sn is set to the a priori error (squared). De-scale
   ! (by XScale squared, i.e. the product of the XScale factors for the two
   ! state variables contributing to each element).
   SPixel%Sn = 0.0
   if (SPixel%NXI > 0 .and. .not. Ctrl%Invpar%disable_Ss) then
      SPixel%Sn(SPixel%XI, SPixel%XI) = SPixel%Sx(SPixel%XI, SPixel%XI)

      SPixel%Sn(SPixel%X, SPixel%X)   = Diag%St(1:SPixel%Nx, 1:SPixel%Nx) + &
                                        Diag%Ss(1:SPixel%Nx, 1:SPixel%Nx)
   else
      SPixel%Sn(SPixel%X, SPixel%X)   = Diag%St(1:SPixel%Nx, 1:SPixel%Nx)
   end if

   do m = 1, MaxStateVar
      do l = 1, MaxStateVar
         SPixel%Sn(l,m) = SPixel%Sn(l,m) / &
                          (Ctrl%Invpar%XScale(l) * Ctrl%Invpar%XScale(m))
      end do
      if (SPixel%Sn(m,m) < 0) then
         write(*, fmt='(a,i1,a,i1,a,e11.4,a,2(i4,1x))') &
              'WARNING: Invert_Marquardt(): Negative error value in Sn(', m, &
              ',', m, '), value: ', SPixel%Sn(m,m), ' location x,y ', &
              SPixel%Loc%X0, SPixel%Loc%Y0
         SPixel%Sn(m,m) = 0.0
      end if
   end do

   ! ************* END ERROR ANALYSIS *************

   call Allocate_GZero(GZero, SPixel%Ind%Ny)
   call Set_GZero(SPixel%Xn(ITau), SPixel%Xn(IRe), Ctrl, SPixel, SAD_LUT(1), &
        GZero, stat)
   if (stat /= 0) go to 99 ! Terminate processing this pixel

   ! Output diffuse fraction so surface reflectance can be calculated by users
   if (Ctrl%Approach == AppAerSw) then
      call Int_LUT_TauSolRe(SAD_LUT(1)%Tfbd, SPixel%Ind%NSolar, SAD_LUT(1)%Grid, &
           GZero, Ctrl, T_0d, d_T_0d, SPixel%spixel_y_solar_to_ctrl_y_index, &
           SPixel%Ind%YSolar, stat)
      if (stat /= 0) go to 99 ! Terminate processing this pixel
      call Int_LUT_TauSolRe(SAD_LUT(1)%Tb, SPixel%Ind%NSolar, SAD_LUT(1)%Grid, &
           GZero, Ctrl, T_00, d_T_00, SPixel%spixel_y_solar_to_ctrl_y_index, &
           SPixel%Ind%YSolar, stat)
      if (stat /= 0) go to 99 ! Terminate processing this pixel

      T_all = T_0d + T_00
      Diag%diffuse_frac(1:SPixel%Ind%NSolar) = T_0d / T_all
      call d_derivative_wrt_crp_parameter(ITau, ITauCRP, T_00, T_0d, d_T_0d, &
           d_T_00, T_all, d_CRP)
      call d_derivative_wrt_crp_parameter(IRe,  IReCRP,  T_00, T_0d, d_T_0d, &
           d_T_00, T_all, d_CRP)

      temp = matmul(matmul(d_CRP, SPixel%Sn((/ITau,IRe/), (/ITau,IRe/))), &
           transpose(d_CRP))
      do m = 1, SPixel%Ind%NSolar
         Diag%diffuse_frac_s(m) = temp(m, m)
      end do
   end if

   ! Evaluate cloud_albedo
   if ((Ctrl%Approach == AppCld1L .or. Ctrl%Approach == AppCld2L) .and. &
       SPixel%Ind%NSolar > 0) then
      call Int_LUT_TauSolRe(SAD_LUT(1)%Rfbd, SPixel%Ind%NSolar, SAD_LUT(1)%Grid, &
           GZero, Ctrl, CRP, d_CRP, SPixel%spixel_y_solar_to_ctrl_y_index, &
           SPixel%Ind%YSolar, stat)
      if (stat /= 0) go to 99 ! Terminate processing this pixel

      Diag%cloud_albedo(1:SPixel%Ind%NSolar) = CRP

      temp = matmul(matmul(d_CRP, SPixel%Sn((/ITau,IRe/), (/ITau,IRe/))), &
                    transpose(d_CRP))
      do m = 1, SPixel%Ind%NSolar
         Diag%cloud_albedo_s(m) = temp(m, m)
      end do
   end if

   ! Evaluate cloud effective emissivity
   if ((Ctrl%Approach == AppCld1L .or. Ctrl%Approach == AppCld2L) .and. &
       SPixel%Ind%NThermal > 0 .and. any(SPixel%X == ITau) .and. any(SPixel%X == IRe)) then
      call Int_LUT_TauSatRe(SAD_LUT(1)%Em, SPixel%Ind%NThermal, SAD_LUT(1)%Grid, &
         GZero, Ctrl, CRP_thermal, d_CRP_thermal, SPixel%spixel_y_thermal_to_ctrl_y_index, &
         SPixel%Ind%YThermal, stat)
      if (stat /= 0) go to 99 ! Terminate processing this pixel

      where (CRP_thermal .lt. 0.)
         CRP_thermal = 0.
      else where (CRP_thermal .gt. 1.)
         CRP_thermal = 1.
      end where

      Diag%cloud_emissivity(1:SPixel%Ind%NThermal) = CRP_thermal

      temp_thermal = matmul(matmul(d_CRP_thermal, SPixel%Sn((/ITau,IRe/), &
                            (/ITau,IRe/))), transpose(d_CRP_thermal))
      do m = 1, SPixel%Ind%NThermal
         Diag%cloud_emissivity_s(m) = max(temp_thermal(m, m), 0.)
      end do
   end if

   ! Evaluate corrected CTX
   if (Ctrl%do_CTX_correction .and. &
      (Ctrl%Approach == AppCld1L .or. Ctrl%Approach == AppCld2L)) then
      call Calc_Corrected_CTX(Ctrl, SPixel, SAD_Chan, SAD_LUT(1), RTM_Pc(1), Sy)
   else
      SPixel%CTP_corrected             = MissingXn
      SPixel%CTP_corrected_uncertainty = MissingSn
      SPixel%CTH_corrected             = MissingXn
      SPixel%CTH_corrected_uncertainty = MissingSn
      SPixel%CTT_corrected             = MissingXn
      SPixel%CTT_corrected_uncertainty = MissingSn
   end if

   ! Evaluate AOD at 870 nm
   if (Ctrl%Approach == AppAerOx .or. Ctrl%Approach == AppAerSw .or. &
       Ctrl%Approach == AppAerO1) then
      call Int_LUT_Re(SAD_LUT(1)%BextRat, 1, SAD_LUT(1)%Grid, GZero, Ctrl, &
           BextRat, d_BextRat, stat)
      if (stat /= 0) go to 99

      Diag%aot870 = SPixel%Xn(ITau) + log10(BextRat(1))

      Diag%aot870_uncertainty = Spixel%Sn(ITau,ITau) + &
           (d_BextRat(1) / (BextRat(1) * log(10.0)))**2 * SPixel%Sn(IRe,IRe)
   end if

   call Deallocate_GZero(GZero)

   ! Costs are divided by number of active instrument channels before output.
   Jm = Jm / SPixel%Ind%Ny
   Ja = Ja / SPixel%Ind%Ny

   ! Set averaging kernel [d2J_dX2-SxInv = matmul(KxT_SyI, Kx)]
   d2J_dX2 = d2J_dX2 - SxInv
   Diag%AK(SPixel%X, SPixel%X) = matmul(St_temp, d2J_dX2)


   ! Void the outputs for failed superpixels
99 if (stat /= 0) then
      Diag%Converged = .false.
      Diag%St        = MissingSn
      Diag%Ss        = MissingSn
      SPixel%Sn      = MissingSn
      Jm             = MissingSn
      Ja             = MissingSn
   else
      Diag%Iterations = iter
      Diag%Jm         = Jm
      Diag%Ja         = Ja
   end if

end subroutine Invert_Marquardt


subroutine add_unit(n, a, b, c)

   implicit none

   integer, intent(in)    :: n
   real,    intent(in)    :: a(:,:)
   real,    intent(in)    :: b
   real,    intent(inout) :: c(:,:)
   integer                :: i

   c = a
   do i = 1, n
      c(i,i) = c(i,i) + b
   end do

end subroutine add_unit


function average_hessian(n, Hessian, scale) result(alpha)

   implicit none

   integer, intent(in) :: n
   real,    intent(in) :: Hessian(:,:)
   real,    intent(in) :: scale
   real                :: alpha

   real    :: Av_Hess
   integer :: i

   Av_Hess = 0.0
   do i = 1, n
      Av_Hess = Av_Hess + Hessian(i,i)
   end do
   Av_Hess = Av_Hess / n
   alpha   = scale * Av_Hess

end function average_hessian
