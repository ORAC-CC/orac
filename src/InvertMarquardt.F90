!-------------------------------------------------------------------------------
! Name: InvertMarquardt.F90
!
! Purpose:
! Find the "best" fitting state vector for a set of measurements in the ECP.
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
! until (convergence) or (number of phase changes > max allowed):
!    calculate 1st and 2nd derivatives of J
!    set starting Marquardt parameters (starts with heaviest weighting on
!       steepest descent)
!    calculate step dX
!
!    for a number of iterations (up to a maximum or until convergence is
!    reached)
!       set new X = Xn + dX
!       Check bounds: if any variable in X reaches a limit it is fixed at
!          that limit
!       if the change in X constitutes a phase change, i.e. Re crosses some
!          limit, drop out of the iteration loop
!
!       call FM to give Y(X), K(X)
!
!       calculate J(X)
!       if (J(X) < J0)    (i.e. "good" step)
!          set Xn = X     (i.e. old Xn + dX)
!          Convergence? if (J - J0) < (limit from Ctrl struct)
!             convergence: drop out of both loops
!          else
!             decrease steepest descent
!             calculate new 1st and 2nd derivatives of J
!             save current J as J0 for checking next iteration
!       else ("bad" step, don't keep X)
!          increase weighting on steepest descent for next iteration
!
!       set new dX for next iteration
!    end of iteration loop
!
!    if (phase change occurred) ! phase change removed Mar 2011
!       Set limits for new phase
!       call FM to give Y in new state
!       re-calculate starting cost J0
!
! (end of "until" loop above)
!
! Error analysis:
!    Calculate the two terms contributing to the overall retrieval error Sn:
!       Sn = St + Ss
!       St is the inverse Hessian (d2J_dX2) for the active state variables
!       Ss is the state expected error from model parameter noise (inactive
!          state variables and surface reflectance Rs - no other parameters
!          available at present).  Ss depends whether there are inactive
!          state variables for the SPixel and whether Equivalent Model
!          Parameter Noise from Rs is selected.
!
! Set diagnostics for output
!
! Note, for output, costs are divided by Ny for the SPixel. Hence convergence
! criteria on delta_J are multiplied up by Ny for the inversion.  Otherwise,
! if cost was divide by Ny at each iteration this would add more processing
! per step and would need to be taken into account in the derivatives of J as
! well as J. Costs in Breakpoint output are also divided by Ny during
! inversion; gives consistency with the final outputs for ease of comparison
! and the impact on performance should not be a problem since breakpoints
! will only be used during debugging.
!
! Note on error handling: checking is done after each subroutine call. Errors
! in the inversion should be treated as fatal for the super-pixel rather than
! the program as a whole. Local variable stat is used instead of status for
! flagging such errors. Status is used to flag serious errors such as
! breakpoint file open failure.
!
! N.B. the value returned by routines called from this routine is held in
! stat rather than status. It is assumed that these routines only flag
! inversion type errors: this is not a very good assumption (file open errors
! etc could occur in these routines, these errors will be logged but the
! program won't exit as a result of them).
!
! If a non-zero status occurs during an inversion, the Diag%QC flag is set to
! indicate non-convergence. In order to do this the convergence logical is
! set false, retrieved state and errors are set to missing and Set_Diag is
! called regardless of whether errors have occurred.
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure, contains Marquardt
!                              parameters, scaling factors etc.
! SPixel   struct  Both        Super pixel data. Contains current
!                              measurements, active state variables, first
!                              guess and a priori info.
! SAD_Chan array   In          Measurement channel characteristics
!          of structs
! SAD_LUT  struct  In          Cloud radiative property look up tables.
! RTM_Pc   struct  In          Radiative transfer model data interpolated to
!                              the current cloud pressure Pc.
! Diag     struct  In          Diagonstic structure
! status   integer Both        ECP status/error flag.
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
!    functions, to prevent text file output slowing down execution (use
!    ifdef DEBUG).
! 2011/08/08, CP: remove ref to cloud class
! 2011/10/03, CP: fixed debug syntax error
! 2011/10/18, CP: added lat/lon info to bkp output
! 2011/12/13, CP: change format statement to make g95 compatible
! 2012/01/19, CP: stored first guess measurement. add variable minusdJ_dX=-dJ_dX.
!    changed calls to Invert_cholesky
! 2012/02/24, CP: changed error_matrix call to Invert_cholesky
! 2012/08/10, CP: defined measurement arrays more explicitly
!    other wise crashed for night measurements
! 2012/09/14, CP: bug fix defined Diag%ss to size of nx elements (was ny) 
!    initialised Y
! 2012/10/02, CP: initialised variables bug fix defined Diag%ss changed how
!    SPixel%sn calculated
! 2013/05/08, CP: set Diag%Y0
! 2014/01/15, GM: Changed Invert_Cholesky() argument Diag%St
!    to Diag%St(1:SPixel%Nx, 1:SPixel%Nx) when inverting d2J_dX2 to get
!    Diag%St. Using just Diag%St results in use of garbage when there are
!    inactive state variables.
! 2014/01/15, GM: Corrected the dimensions of the assignment
!    for the calculation of Diag%Ss from Diag%Ss(1:SPixel%Ind%Ny,1:SPixel%Ind%Ny)
!    to Diag%Ss(1:SPixel%Nx,1:SPixel%Nx).
! 2014/01/15, GM: Set values of J, Jm, and Ja to MissingSn in the case 
!    of failed retrievals as is done with the other values in Diag.
! 2014/01/15, GM: Moved dynamic setting of the upper limit
!    for CTP to the highest pressure in the profile from FM() to this routine.
! 2014/01/17, GM: Cleaned up code.
! 2014/01/29, MJ: Fixed case where alpha can get out of bounds.
! 2014/02/27, CP: Added declaration of J.
! 2014/04/02, CP: Fixed bug where temp was not reassigned.
! 2014/04/02, MJ: Fixed bug where Diag%ss was not initialized.
! 2014/07/24, AP: Removed unused status variable.
! 2014/07/24, CP: Added in cloud albedo
! 2015/01/09, AP: Patch memory leak with cloud_albedo.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Invert_Marquardt(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, Diag, status)

   use Cholesky
   use Ctrl_def
   use Diag_def
   use ECP_Constants
   use FM_Routines_def
   use RTM_Pc_def
   use SAD_Chan_def
   use SAD_LUT_def
   use SPixel_def

   implicit none

   ! Argument declarations

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc
   type(Diag_t),     intent(out)   :: Diag
   integer,          intent(out)   :: status

   ! Local variable declarations

   integer :: m, l                ! counter for short/implied do loops
                                  ! (can't use J: it's the cost function)
   integer :: stat                ! Local error status flag
   real    :: Y(SPixel%Ind%Ny)    ! TOA reflectances, radiances etc for partly-
                                  ! cloudy conditions. Returned by FM
   real    :: dY_dX(SPixel%Ind%Ny,MaxStateVar+1)
                                  ! Derivatives d[ref]/d[tau,Re,pc,f,Ts,Rs]t
   real    :: cloud_albedo(SPixel%Ind%NSolar)
                                  ! cloud albedo Returned by FM
   real    :: Kx(SPixel%Ind%Ny, SPixel%Nx)
                                  ! Scaled, active part of dY_dX
   real    :: Kbj(SPixel%Ind%Ny, SPixel%Ind%NSolar)
                                  ! Scaled, surface reflectance part of dY_dX
                                  ! Set by Set_Kx if Ctrl%Eqmpn%Rs is set.
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
   real    :: unit(SPixel%Nx, SPixel%Nx)
                                  ! Unit matrix (size matches no of active state
                                  ! variables).
   real    :: Ccj_Ny              ! Product of Cost convergence criteria and
                                  ! number of active channels.
   logical :: convergence         ! Indicates whether convergence has occurred
   logical :: phase_change        ! Indicates phase change has occurred
   integer :: NPhaseChanges       ! Phase change counter
   integer :: iter                ! Inversion iteration counter
   real    :: KxT_SyI(SPixel%Nx, SPixel%Ind%Ny)
                                  ! Product of the transpose of Kx with SyInv
   real    :: dJ_dX(SPixel%Nx)    ! 1st derivative of J wrt state variables
   real    :: d2J_dX2(SPixel%Nx, SPixel%Nx)
                                  ! 2nd derivative of J wrt state variables
   real    :: Av_Hess             ! Average of Hessian (d2J_dX2) diagonal
   real    :: alpha               ! Marquardt control variable
   real    :: huge_value          ! largest value of real data type
   real    :: J2plus_A(SPixel%Nx, SPixel%Nx)
                                  ! Temporary array to hold sum of d2J_dX2 and
                                  ! alpha * unit.
   real    :: minusdJ_dX(SPixel%Nx)
   real    :: delta_X(SPixel%Nx)
                                  ! Step to be applied to state variables
   real    :: Xplus_dX(MaxStateVar)
                                  ! State vector being checked in a given
                                  ! iteration (Xn + delta_X)
   real    :: delta_J             ! Change in cost between operations
   real    :: Dy(SPixel%Nx, SPixel%Ind%Ny)
                                  ! "Inversion operator" used in error analysis
   real    :: Kb(SPixel%Ind%Ny, SPixel%NxI+SPixel%Ind%NSolar)
                                  ! "Model parameter" gradients from inactive
                                  ! state variables and surface reflectance.
   real    :: Dy_Kb(SPixel%Nx, SPixel%NxI+SPixel%Ind%NSolar)
                                  ! Product of Dy and Kb.
   real    :: Sb(SPixel%NxI+SPixel%Ind%NSolar, SPixel%NxI+SPixel%Ind%NSolar)
                                  ! "Model parameter" error covariance.
   real    :: temp(SPixel%Nx, SPixel%Nx)
                                  ! work around "array temporary" warning
#ifdef BKP
   integer :: bkp_lun             ! Unit number for breakpoint file
   integer :: ios                 ! I/O status for breakpoint file
#endif

   ! Initialise
   stat      = 0
   status    = 0

   Y         = 0.
   dY_dX     = 0.
   Kx        = 0.
   Kbj       = 0.
   KxT_SyI   = 0.
   J         = 0.
   d2J_dX2   = 0.
   Diag%st   = 0.
   Diag%ss   = 0.
   SPixel%Sn = 0.

   huge_value = huge(1.0)/Ctrl%InvPar%MqStep

   ! Set state variable limits for initial phase
   call Set_Limits(Ctrl, SPixel, stat)

   ! Dynamically set upper limit of cloud top pressure to lowest profile
   ! pressure of current pixel.
   SPixel%XULim(iPc)=SPixel%RTM%LW%p(SPixel%RTM%LW%Np)

   ! Calculate measurements at first-guess state vector X0 (SPixel%X0. X0
   ! should be provided un-scaled. Only used in the FM call and Xdiff(?)
   ! AS Mar 2011 assume only 1 cloud class in use so SAD_LUT dimension is 1
   if (stat == 0) &
      call FM(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, SPixel%X0, Y, dY_dX, &
              cloud_albedo, stat)

   Diag%Y0(1:SPixel%Ind%Ny)=Y

   ! Convert dY_dX to Kx and Kbj.
   if (stat == 0) &
      call Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kbj, stat)

   ! Set covariance matrix Sy and calculate the inverses SyInv and SxInv,
   ! required for initial cost calculation. (Sx is already stored in SPixel).
   ! Always call Set_Kx before Set_Sy. Needs Kbj.
   if (stat == 0) &
      call Set_Sy(Ctrl, SPixel, Kbj, Sy, stat)

   ! Calculate SyInv and SxInv
   if (stat == 0) &
      call Invert_Cholesky(Sy, SyInv, SPixel%Ind%Ny, stat)

   if (stat == 0) then
      error_matrix=SPixel%Sx(SPixel%X, SPixel%X)

      call Invert_Cholesky(error_matrix, SxInv, SPixel%Nx, stat)
#ifdef DEBUG
      if (stat /= 0) &
         write(*, *) 'ERROR: Invert_Marquardt(): Error in Invert_Cholesky'
#endif
   end if

   ! Calculate cost at X0. Requires inverse of Sx and Sy.
   !
   ! Calculate Xdiff, the difference between the current state vector X and
   ! the a priori state vector SPixel%Xb for the active state variables.
   !
   ! Calculate Ydiff, the difference between the measurements and calculated
   ! values for X. Xdiff requires scaling since X is in "natural" units for use
   ! by FM.
   !
   ! Ja is 0 if X0 is the a priori (not generally true).
   !
   ! Xb is assumed to be full-length and un-scaled.
   ydiff=0.0
   if (stat == 0) then
      Xdiff = (SPixel%X0(SPixel%X) - SPixel%Xb(SPixel%X)) &
              * Ctrl%Invpar%XScale(SPixel%X)

      Ydiff(1:SPixel%Ind%Ny) = Y(1:SPixel%Ind%Ny) - SPixel%Ym(1:SPixel%Ind%Ny)
      Diag%YmFit(1:SPixel%Ind%NY)= Ydiff

      Ja = dot_product(Xdiff, matmul(SxInv, Xdiff))
      Jm = dot_product(Ydiff, matmul(SyInv, Ydiff))
      J0 = Jm + Ja
   end if

   ! Initial breakpoint output (Open breakpoint file if required). Note the
   ! breakpoint file must be closed before any call to a routine that will try
   ! to write to it.
#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_InvertMarquardt_1) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: Invert_Marquardt(): Error opening breakpoint file'
         stop BkpFileOpenErr
      else
         write(bkp_lun,'(/,a)')'Invert_Marquardt:'
         if (Ctrl%Bkpl >= BkpL_InvertMarquardt_2) then
            write(bkp_lun,'(2x,a,2(f9.3,1x))') 'lat/lon: ',SPixel%Loc%Lat, &
                                                           SPixel%Loc%Lon
            write(bkp_lun,'(2x,a,11(f9.3,1x))') 'Measurements: ',SPixel%Ym
         end if
         if (Ctrl%Bkpl >= BkpL_InvertMarquardt_1) then
            write(bkp_lun,'(2x,a)')'First guess: ' ,SPixel%Y0
            write(bkp_lun,'(2x,a,5(f9.3,1x))')  'X0:           ',SPixel%X0
            write(bkp_lun,'(2x,a,5(f9.3,1x))')  'Xb:           ',SPixel%Xb
            write(bkp_lun,*)
            Diag%Y0(1:SPixel%Ind%Ny)=Y

            if (Ctrl%Bkpl >= BkpL_InvertMarquardt_4) then
               ! SPixel%Sx output takes account of the active state variables.
               ! Sx array is full size. SPixel%X(m) selects a row, SPixel%X
               ! selects the active parts of the row. Remove scaling for output.
               write(bkp_lun,'(2x,a)')'Sx:'
               do m=1,SPixel%Nx
                  write(bkp_lun,'(2x,5(e9.2,1x))') &
                        SPixel%Sx(SPixel%X(m),SPixel%X) / &
                        (Ctrl%Invpar%XScale(SPixel%X) * &
                        Ctrl%Invpar%XScale(SPixel%X))
               end do
               write(bkp_lun,*)
            end if
         end if

         close(bkp_lun)
      end if
   end if
#endif

   ! Derivatives of J0 are calculated inside the do loop (same code used for
   ! phase change)

   ! Set Xn = X0 for 1st iteration
   SPixel%Xn = SPixel%X0

   ! Set up unit matrix (size matches number of active state variables) for
   ! Marquardt calculations.
   call Set_Unit(SPixel, unit)

   ! Adjust cost convergence criteria for the number of active channels in the
   ! super-pixel, since final costs will be divided by Ny for output.
   Ccj_Ny = Ctrl%Invpar%Ccj * SPixel%Ind%Ny

   ! Initialise loop control variables
   convergence   = .false.
   phase_change  = .false.
   NPhaseChanges = 0

   iter = 1

   do ! On phase change, re-start from here

      if (status /= 0 .or. stat /= 0) exit

      ! Calculate derivatives of starting cost (required for setting Marquardt
      ! parameters). Needs Kx, the scaled version of dY_dX.

      ! Calculate product of transpose(Kx) and inverse of Sy (used twice below).
      KxT_SyI = matmul(transpose(Kx), SyInv)

      ! The ATBD shows the 2nd term of dJ_dX (or J') as SxInv * Xdiff. The code
      ! here can either have matmul(SxInv, Xdiff), which is effectively SxInv *
      ! transpose(Xdiff), or matmul(Xdiff, SxInv). It looks impossible to
      ! reproduce the calculation in the ATBD.
      !
      ! Although it's generally true that matrix product AB != BA, if A is 1-d
      ! and B is square and symmetric, AB = transpose(BA) and as far as Fortran
      ! is concerned the transpose of a 1-d array is no different from the
      ! original array (at least when adding).
      dJ_dX   = matmul(KxT_SyI, Ydiff) + matmul(SXInv, Xdiff)

      d2J_dX2 = matmul(KxT_SyI, Kx) + SxInv

      ! Set Marquardt parameters (initial weighting favours steepest descent)
      ! Unit matrix and inverse function required.

      ! Average leading diagonal of Hessian d2J_dX2 in order to set alpha.  Then
      ! use alpha and d2J_dX2 to set delta_X. Solve_Cholesky is used to find
      ! delta_X. The equation from the ATBD is
      !
      ! dX = -(J'' + alphaI)^-1 * J' or delta_X = - (inv(J2plus_A) * dJ_dX).
      !
      ! Multiplying through by J2_plusA we get:
      !
      ! J2_plusA * delta_X = -dJ_dX,
      !
      ! which we can solve for delta_X using Solve_Cholesky.
      Av_Hess = 0.0
      do m=1,SPixel%Nx
         Av_Hess = Av_Hess + d2J_dX2(m,m)
      end do
      Av_Hess  = Av_Hess / SPixel%Nx
      alpha    = Ctrl%Invpar%MqStart * Av_Hess
      J2plus_A = d2J_dX2 + (alpha * unit)
      minusdJ_dX=-dJ_dX

      call Solve_Cholesky(J2plus_A, minusdJ_dX, delta_X, SPixel%Nx, stat)
#ifdef DEBUG
      if (stat /= 0) &
         write(*, *) 'ERROR: Invert_Marquardt(): Error in Solve_Cholesky'
#endif
      ! De-scale delta_X so that the X passed to FM can be kept un-scaled
      delta_X = delta_X / Ctrl%Invpar%XScale(SPixel%X)

      Diag%Y0(1:SPixel%Ind%Ny) = Y(1:SPixel%Ind%Ny)

      ! Write starting parameter breakpoints and close breakpoint file so that
      ! FM can write to it.
#ifdef BKP
      if (Ctrl%Bkpl >= BkpL_InvertMarquardt_2) then
         open(unit=bkp_lun,      &
              file=Ctrl%FID%Bkp, &
              status='old',      &
              position='append', &
              iostat=ios)
         if (ios /= 0) then
            write(*,*) 'ERROR: Invert_Marquardt(): Error opening breakpoint file'
            stop BkpFileOpenErr
         else
            write(bkp_lun,'(a)')'Invert_Marquardt:'
            write(bkp_lun,'(2x,a,11(f9.3,1x))') 'Y:            ',Y

            if (Ctrl%Bkpl >= BkpL_InvertMarquardt_3) then
               write(bkp_lun,'(/,2x,a)')'Sy:'
               do m=1,SPixel%Ind%Ny
                  write(bkp_lun,'(2x,11(e11.3,1x))')Sy(m,:)
               end do

               write(bkp_lun,'(/,2x,a)') &
                     'Kx (columns are channels, rows active state variables)'
               do m=1,SPixel%Nx
                  write(bkp_lun,'(2x,11(e11.4,1x))') KX(:,m)
               end do

               if (Ctrl%Bkpl >= BkpL_InvertMarquardt_4) then
                  write(bkp_lun,'(/,2x,a,5(e11.3,1x))')  'Xdiff:        ', &
                        Xdiff / Ctrl%Invpar%XScale(SPixel%X)

                  write(bkp_lun,'(2x,a,11(e11.3,1x))')   'Ydiff:        ',Ydiff
               end if

               write(bkp_lun,'(/,2x,a)') 'dJ_dX:        '
               write(bkp_lun,'(2x,5(e11.4,1x))')dJ_dX

               write(bkp_lun,'(/,2x,a)')'d2J_dX2:'
               do m=1,SPixel%Nx
                  write(bkp_lun,'(2x,5(e11.4,1x))') d2J_dX2(m,:)
               end do
               write(bkp_lun,*)
               write(bkp_lun,'(2x,a,e11.3)')   'alpha:        ', alpha
            end if

            write(bkp_lun,'(2x,a,3(f9.3,1x))')  'Jm, Ja, J0 /Ny', &
                  Jm/SPixel%Ind%Ny, Ja/SPixel%Ind%Ny, J0/SPixel%Ind%Ny
            write(bkp_lun,'(2x,a,5(f9.3,1x))')  'delta_X:      ',delta_X

            close(unit=bkp_lun)
         end if
      end if
#endif

      ! Main iteration loop
      !
      ! The iteration counter is not re-set on phase change. A phase change
      ! counter is also used to avoid danger of oscillation between phases.
      do

         ! N.B. Jumps out of the loop if convergence or phase change occurs, or
         ! if Max no. of iterations is reached, or if status not zero
         if (status /= 0 .or. stat /= 0) exit

         ! Apply step delta_x to the active state variables. Assumes Xn and
         ! delta_X are both unscaled.
         Xplus_dX(SPixel%X)  = SPixel%Xn(SPixel%X) + delta_X

         Xplus_dX(SPixel%XI) = SPixel%Xn(SPixel%XI)

         ! Check bounds for active state variables - does delta_X take any state
         ! variable outside it's range? (If so, freeze it at the boundary). Also
         ! check for possible phase change.
         call Check_Limits(Xplus_dX, SPixel, stat)

         ! Calculate Y for Xn + delta_X. Xplus_dX is currently un-scaled.
         if (stat == 0) &
            call FM(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, Xplus_dX, Y, &
                    dY_dX, cloud_albedo, stat)
         if (SPixel%Ind%NSolar > 0) then
            Diag%cloud_albedo(1:SPixel%Ind%NSolar) = &
                 cloud_albedo(1:SPixel%Ind%NSolar)
         else
            Diag%cloud_albedo = 0
         end if

         ! Set new Kx, Kbj, Sy and SyInv
         if (stat == 0) call Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kbj, stat)
         if (stat == 0) call Set_Sy(Ctrl, SPixel, Kbj, Sy, stat)
         if (stat == 0) then
            call Invert_Cholesky(Sy, SyInv, SPixel%Ind%Ny, stat)
            if (stat /= 0) then
#ifdef DEBUG
               write(*, *) 'ERROR: Invert_Marquardt(): Error in Invert_Cholesky'
#endif
            else
               ! Calculate new cost, J.
               Xdiff = (Xplus_dX(SPixel%X) - SPixel%Xb(SPixel%X)) &
                       * Ctrl%Invpar%XScale(SPixel%X)

               Ydiff = Y - SPixel%Ym
               Diag%YmFit(1:SPixel%Ind%NY)= Ydiff

               Ja = dot_product(Xdiff, matmul(SxInv, Xdiff))
               Jm = dot_product(Ydiff, matmul(SyInv, Ydiff))
               J  = Jm + Ja

               ! Check J vs previous value. Lower value means progress:
               ! "accept" the step delta_X and reset the Marquardt values.

               delta_J = J-J0
            end if
         end if

         if (stat == 0 .and. J <= J0) then
            ! Improvement in cost. Add delta_X to Xn
            SPixel%Xn = Xplus_dX

            ! Calculate the Hessian (J'') as this is required for error analysis
            ! if convergence is reached or for setting new delta_X.
            KxT_SyI   = matmul(transpose(Kx), SyInv)
            d2J_dX2   = matmul(KxT_SyI, Kx) + SxInv

            ! Check for convergence.
            ! Set SPixel%Xn: value is the solution X
            ! Save the pixel location if convergence occurs, for SDAD first
            ! guess/a priori setting.
            if (abs(delta_J) <= Ccj_Ny) then
               convergence = .true.
            else
               ! Decrease steepest descent part for next iteration
               alpha = alpha / Ctrl%InvPar%MqStep

               ! Save new J as J0 for checking next time round
               J0 = J

               ! Calculate values required to set the new step delta_X based on
               ! derivatives of J (delta_X itself is set after this "if")
               dJ_dX      = matmul(KxT_SyI, Ydiff) + matmul(SXInv, Xdiff)
               J2plus_A   = d2J_dX2 + (alpha * unit)
               minusdJ_dX = -dJ_dX
               call Solve_Cholesky(J2plus_A, minusdJ_dX, delta_X, SPixel%Nx, stat)
            end if
         else
            ! No improvement in cost

            ! Increase steepest descent part for next iteration and set values
            ! required for setting new deltaX using old cost derivatives.
            ! "if" inserted to catch (academic?) case of overflow if alpha gets huge.
            if (alpha .lt. huge_value) then
               alpha = alpha * Ctrl%InvPar%MqStep
            end if
            J2plus_A = d2J_dX2 + (alpha * unit)
            minusdJ_dX=-dJ_dX
            call Solve_Cholesky(J2plus_A, minusdJ_dX, delta_X, SPixel%Nx, stat)
         end if

         if (convergence) exit ! Drops out of the iteration do loop

         ! Warn of errors in Solve_Cholesky (assumes no routine that can
         ! overwrite status has been called since).
#ifdef DEBUG
         if (stat /= 0) &
            write(*, *) 'ERROR: Invert_Marquardt(): Error in Solve_Cholesky'
#endif
         ! Set and de-scale the new delta_X
         if (stat == 0) delta_X = delta_X / Ctrl%Invpar%XScale(SPixel%X)

         ! Main iteration loop breakpoint outputs. The file is opened and closed
         ! each time since (a) FM might also write to it and (b) the loop may
         ! exit early if convergence is reached, leaving the file state uncertain.
#ifdef BKP
         if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_1) then
            call Find_Lun(bkp_lun)
            open(unit=bkp_lun,      &
                 file=Ctrl%FID%Bkp, &
                 status='old',      &
                 position='append', &
                 iostat=ios)
            if (ios /= 0) then
               write(*,*) 'ERROR: Invert_Marquardt(): Error opening breakpoint file'
               stop BkpFileOpenErr
            else
               write(bkp_lun,'(/,a,i2)')'Invert_Marquardt Iteration ',iter
               write(bkp_lun,'(2x,a,5(f9.3,1x))')     'State:        ',Xplus_dX

               if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_2) then
                  write(bkp_lun,'(2x,a,11(f9.3,1x))') 'Y:            ',Y
                  write(bkp_lun,'(2x,a,e11.3)')      'alpha:          ', alpha
                  write(bkp_lun,'(2x,a,5(f9.3,1x))')  'delta_X:      ', delta_X
                  write(bkp_lun,'(2x,a,3(f9.3,1x))')  'Jm, Ja, J:    ',&
                        Jm/SPixel%Ind%Ny, Ja/SPixel%Ind%Ny, J/SPixel%Ind%Ny
                  write(bkp_lun,'(2x,a,f9.3,11x,f9.3)') 'delta J, J0:  ', &
                        delta_J/SPixel%Ind%Ny, J0/SPixel%Ind%Ny

                  if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_3) then
                     write(bkp_lun,'(2x,a,5(e11.3,1x))')  'Xdiff:        ',&
                           Xdiff  / Ctrl%Invpar%XScale(SPixel%X)
                     write(bkp_lun,'(2x,a,11(e11.3,1x))') 'Ydiff:        ',Ydiff
                     write(bkp_lun,'(/,2x,a)')'Sy:'
                     do m=1,SPixel%Ind%Ny
                        write(bkp_lun,'(2x,11(e11.3,1x))') Sy(m,:)
                     end do

                     write(bkp_lun,'(/,2x,a)') &
                          'Kx (columns are channels, rows active state variables)'
                     do m=1,SPixel%Nx
                        write(bkp_lun,'(2x,11(e11.4,1x))') KX(:,m)
                     end do
                     write(bkp_lun,'(/,2x,a)') 'dJ_dX:        '
                     write(bkp_lun,'(2x,5(e11.4,1x))')dJ_dX
                     write(bkp_lun,'(/,2x,a)')'d2J_dX2:'
                     do m=1,SPixel%Nx
                        write(bkp_lun,'(2x,5(e11.4,1x))') d2J_dX2(m,:)
                     end do
                     write(bkp_lun,*)
                  end if
               end if
            end if

            close(unit=bkp_lun)
         end if
#endif
         ! Check whether the maximum iteration has been passed.
         if (iter == Ctrl%Invpar%MaxIter) exit

         ! Increment iteration counter.
         iter = iter + 1
      end do ! End of "main iteration" loop

      ! Check for convergence
      if (convergence) exit

      ! Check for too many iterations
      if (iter == Ctrl%Invpar%MaxIter) exit

   end do ! End of "phase change" do loop


   ! End of Marquardt inversion.

   ! Error analysis:
   ! Error values are required for setting SPixel%Sn (used for SDAD FG/AP
   ! setting) and for setting diagnostics St and Ss and QCFlag (if Diag flags
   ! set). St is the inverse Hessian (d2J_dX2) for the active state variables
   ! d2J_dX2 is already sized to match the number of active variables.


   if (stat == 0) then
      ! State expected error from measurements=Diag%St(1:SPixel%Nx, 1:SPixel%Nx))
      temp = Diag%St(1:SPixel%Nx, 1:SPixel%Nx)
      call Invert_Cholesky(d2J_dX2, temp, SPixel%Nx, stat)
      Diag%St(1:SPixel%Nx, 1:SPixel%Nx) = temp
#ifdef DEBUG
      if (stat /= 0) &
         write(*, *) 'ERROR: Invert_Marquardt(): Error in Invert_Cholesky'
#endif
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

   if (stat == 0) then
      if (SPixel%NxI > 0 .or. Ctrl%Eqmpn%Rs == 0) then
         Dy = matmul(Diag%St(1:SPixel%Nx,1:SPixel%Nx), KxT_SyI)

         ! Kx isn't actually set for inactive vars so we use a scaled dY_dX).
         do m=1,SPixel%NxI
            Kb(:,m) = dY_dX(:,SPixel%XI(m)) / Ctrl%Invpar%XScale(SPixel%XI(m))
         end do

         Sb(1:SPixel%NxI,1:SPixel%NxI) = SPixel%Sx(SPixel%XI,SPixel%XI)

         if (Ctrl%Eqmpn%Rs == 0 .and. SPixel%Ind%NSolar > 0) then
            ! Add Rs terms to Kb and Sb. Use the full Dy_Kb.
            Kb(:, SPixel%NxI+1:SPixel%NxI+SPixel%Ind%NSolar) = Kbj

            Dy_Kb   = matmul(Dy, Kb)

            Sb(SPixel%NxI+1:, SPixel%NxI+1:) = SPixel%SRs

            Diag%Ss = matmul(Dy_Kb, matmul(Sb, transpose(Dy_Kb)) )
         else
            ! No Rs terms: don't use all of Dy_Kb this is a thermal only
            ! retrieval
            Dy_Kb(:,1:SPixel%NxI) = matmul(Dy, Kb(:,1:SPixel%NxI))
            Diag%Ss(1:SPixel%Nx,1:SPixel%Nx) = matmul(Dy_Kb(:,1:SPixel%NxI), &
               matmul(Sb(1:SPixel%NxI,1:SPixel%NxI), transpose(Dy_Kb(:,1:SPixel%NxI))) )
         end if
      else
         ! There are no inactive state vars and Eqmpn has been used for Rs, i.e.
         ! no info to set Ss. Needs some values.
         Diag%Ss = 0
      end if

      ! Assign St+Ss to the parts of SPixel%Sn where state vars are active. For
      ! inactive variables, Sn is set to the a priori error (squared). De-scale
      ! (by XScale squared, i.e. the product of the XScale factors for the two
      ! state variables contributing to each element).
      SPixel%Sn = 0.0
      if (SPixel%Nx > 0 .and. SPixel%NxI == 0 ) then
         SPixel%Sn(SPixel%X, SPixel%X) = &
            Diag%St(1:SPixel%Nx, 1:SPixel%Nx)
      end if

      if (SPixel%NxI > 0) then
         SPixel%Sn(SPixel%XI, SPixel%XI) = SPixel%Sx(SPixel%XI, SPixel%XI)

         SPixel%Sn(SPixel%X, SPixel%X)   = Diag%St(1:SPixel%Nx, 1:SPixel%Nx) + &
                                           Diag%Ss(1:SPixel%Nx, 1:SPixel%Nx)
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
   else     ! stat is non-zero, inversion failed
      convergence = .false.
      Diag%St     = MissingSn
      Diag%Ss     = MissingSn
      SPixel%Sn   = MissingSn
   end if   ! End of stat check before Ss setting.

   ! Set remaining diagnostic values. Set_Diag also checks whether the solution
   ! state was good enough to use in SDAD first guess and a priori setting, and
   ! if so saves Xn and Sn in SPixel (XnSav and SnSav). Costs are divided by
   ! number of active instrument channels before output.

   if (stat == 0) then
      J  = J  / SPixel%Ind%Ny
      Jm = Jm / SPixel%Ind%Ny
      Ja = Ja / SPixel%Ind%Ny
   else
      J  = MissingSn
      Jm = MissingSn
      Ja = MissingSn
   end if

   call Set_Diag(Ctrl, SPixel, convergence, J, Jm, Ja, iter, &
        NPhaseChanges, Y, Sy, Diag, stat)

   ! Write final solution and close breakpoint output file

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_InvertMarquardt_1) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: Invert_Marquardt(): Error opening breakpoint file'
         stop BkpFileOpenErr
      else
         if (convergence) then
            write(bkp_lun,'(/,2x,a,i2,a)')'Invert_marquardt: convergence after ',&
                  iter, ' iterations'
         else
            write(bkp_lun,'(2x,a,i2,a)')&
                  'Invert_marquardt: no convergence after ',iter,' iterations'
         end if
         write(bkp_lun,'(2x,a,5(f9.3,1x))') 'State:        ',SPixel%Xn

         if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_2) then
            write(bkp_lun,*)'Y:            ', Y
            write(bkp_lun,'(2x,a,11(f9.3,1x))')'Y-Ym:         ', &
                  Diag%YmFit(1:SPixel%Ind%Ny)
            write(bkp_lun,'(2x,a,3(f9.3,1x))') 'Jm, Ja, J /Ny:', Jm, Ja, J
            write(bkp_lun,'(2x,a,f9.3)')      'delta J:      ', delta_J
         end if

         write(bkp_lun, '(/,a,/)') 'Invert_Marquardt: end'

         close(unit=bkp_lun)
      end if
   end if
#endif

end subroutine Invert_Marquardt
