! Name:
!    Invert_Marquardt
!
! Description:
!    Find the "best" fitting state vector for a set of measurements in the
!    ECP.
!    
!    Uses a Marquardt descent algorithm to find a state vector that gives a 
!    good match to the measurements (Y values across all selected channels)
!    for a particular image super-pixel.
!
!    The best fit is identified by minimising the "cost".   
!    The full cost function J is defined as:
!      J(x) = (y(x) - ym) * inverse(Sy) * transpose(y(x) - ym) +
!               (x - xb)  * inverse(Sx) * transpose(x - xb) + 
!               (bt - b)  * inverse(Sb) * transpose(bt - b) 
!
!    J is minimised with respect to x, hence the third component (the model 
!    parameter vector b) is omitted from this routine. 
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    Ctrl       struct   In          Control structure, contains Marquardt 
!                                    parameters, scaling factors etc.
!    SPixel     struct   Both        Super pixel data. Contains current 
!                                    measurements, active state variables, 
!                                    first guess and a priori info.  
!    SAD_Chan            In          Measurement channel characteristics
!               array of structs
!    SAD_LUT             In          Cloud radiative property look up tables.
!               struct
!    RTM_Pc     struct   In          Radiative transfer model data interpolated
!                                    to the current cloud pressure Pc.
!    status     integer  Both        ECP status/error flag.
!
! Algorithm:
!    The state vector X is set to some initial (first guess) values X0, and
!    the forward model FM is run to calculate Y(X0). 
!    Y(X0) is compared with the measured Y to give the initial "cost", J0
!
!    Set Xn = X0 
!
!    until (convergence) or (number of phase changes > max allowed):
!       calculate 1st and 2nd derivatives of J
!       set starting Marquardt parameters (starts with heaviest weighting on 
!          steepest descent)
!       calculate step dX
!
!       for a number of iterations 
!       (up to a maximum or until convergence is reached)
!          set new X = Xn + dX
!          Check bounds: if any variable in X reaches a limit it is fixed at 
!             that limit
!          if the change in X constitutes a phase change, i.e. Re crosses some
!             limit, drop out of the iteration loop 
!
!          call FM to give Y(X), K(X)
!          calculate J(X)
!
!          if (J(X) < J0)    (i.e. "good" step)
!             set Xn = X     (i.e. old Xn + dX)
!             Convergence? if (J - J0) < (limit from Ctrl struct)
!                convergence: drop out of both loops 
!             else
!                decrease steepest descent
!                calculate new 1st and 2nd derivatives of J
!                save current J as J0 for checking next iteration 
!          else ("bad" step, don't keep X)
!             increase weighting on steepest descent for next iteration
!
!          set new dX for next iteration
!       end of iteration loop
!
!       if (phase change occurred) ! phase change removed Mar 2011
!          Set limits for new phase
!          call FM to give Y in new state
!          re-calculate starting cost J0
!
!    (end of "until" loop above) 
!
!    Error analysis:
!       Calculate the two terms contributing to the overall retrieval error Sn:
!       Sn = St + Ss
!       St is the inverse Hessian (d2J_dX2) for the active state variables
!
!       Ss is the state expected error from model parameter noise (inactive 
!          state variables and surface reflectance Rs - no other parameters
!          available at present).
!          Ss depends whether there are inactive state variables for the SPixel
!          and whether Equivalent Model Parameter Noise from Rs is selected.
!
!    Set diagnostics for output
!    Note, for output, costs are divided by Ny for the SPixel. Hence convergence
!    criteria on delta_J are multiplied up by Ny for the inversion. 
!    Otherwise, if cost was divide by Ny at each iteration this would 
!    add more processing per step and would need to be taken into account in the
!    derivatives of J as well as J. Costs in Breakpoint output are also  
!    divided by Ny during inversion; gives consistency with the final outputs 
!    for ease of comparison and the impact on performance should not be a 
!    problem since breakpoints will only be used during debugging.
!
!    Note on error handling: no status checking is done at present, except 
!    after calls to Invert_Cholesky. Since the status checking is not thorough
!    (i.e. does not lead to exit by the quickest possible route) there is a
!    danger of operations following a failed inversion setting arrays with
!    unknown values.
!
!  **** Error handling: checking is now done after each subroutine call. 
!    Errors in the inversion should be treated as fatal for the super-pixel
!    rather than the program as a whole. Local variable stat is used instead 
!    of status for flagging such errors. Status is used to flag serious errors 
!    such as breakpoint file open failure. 
!    N.B. the value returned by routines called from this routine is held in 
!    stat rather than status. It is assumed that these routines only flag 
!    inversion type errors: this is not a very good assumption (file open 
!    errors etc could occur in these routines, these errors will be logged but
!    the program won't exit as a result of them). A better method might be to
!    make these routines set the Diag%QC flag to indicate "data" errors, leaving
!    status free to indicate serious errors.
!    If a non-zero status occurs during an inversion, the Diag%QC flag
!    is set to indicate non-convergence. In order to do this the convergence 
!    logical is set false, retrieved state and errors are set to missing and 
!    Set_Diag is called regardless of whether errors have occurred.
!
! Local variables:
!    Name       Type        Description
!    Xplus_dX   real array  State vector with perturbation by delta X.
!    Y          real array  TOA measurements calculated by FM.
!    dY_dX      real array  Derivatives of Y w.r.t each state variable.
!    Kx         real array  Scaled, active part of dY_dX
!    Kbj        real array  Scaled, surface reflectance part of dY_dX
!                           Set by Set_Kx if Ctrl%Eqmpn%Rs is set.
!    Ydiff      real array  Difference between measured and calculated Y
!    Xdiff      real array  Difference between current state and a priori
!    Sy         real array  Error covariance in measurements
!    SyInv      real array  Inverse of Sy
!    SxInv      real array  Inverse of Sx. Sx is the error covariance 
!                           in the a priori state vector, part of SPixel.
!    J0         real        Cost at first guess state
!    J, Jm, Ja  real        Cost at a given state (plus contributions
!                           to cost from measurements and a priori)
!    delta_J    real        Change in cost between iterations
!    Ccj_Ny     real        Product of the cost convergence criteria and the
!                           number of active channels in the super-pixel.
!    KxT_SyI    real array  Product of the transpose of Kx with SyInv
!    dJ_dX      real array  1st derivative of J wrt state variables
!    d2J_dX2    real array  2nd derivative of J wrt state variables
!    delta_X    real array  Step to be applied to state variables in a given
!                           iteration of the inversion process
!    unit       real array  Unit matrix (size matches no of active state
!                           variables).
!    J2plus_A   real array  Temporary array to hold sum of d2J_dX2 and
!                           alpha * unit.
!    Av_Hess    real        Average of Hessian (d2J_dX2) diagonal
!    alpha      real        Marquardt control variable
!    iter       int         Inversion iteration counter
!    phase_change  logical  Indicates phase change has occurred
!    NPhaseChanges int      Phase change counter
!    convergence   logical  Indicates whether convergence has occurred 
!    Kb         real array  "Model parameter" gradients from inactive state
!                           variables. Analogous to Kx. Size Ny by NxI.
!    Sb         real array  "Model parameter" error covariance. Size NxI square.
!    Dy         real array  The "inversion operator". Used in error analysis.
!                           Size Nx (active) by Ny.
!    Dy_Kb      real array  Temporary array to hold product of Dy and Kb.
!    bkp_lun    int         Logical unit number for breakpoint file.
!    ios        int         I/O status value returned on file operations.
!    m, l       int         Counter for short/implied do loops (use l, m instead
!                           of the usual i or j: J is the cost function)
!
! History:
!     5th Apr 2001, Andy Smith: original version
!    12th Jun 2001, Andy Smith: 
!      First complete version. Compiles but not fully debugged. 
!    29th Jun 2001, Andy Smith: 
!      Passes simple tests. Added breakpoint outputs. Found a problem with
!      handling of phase changes (checking into RCS before attempting fix).
!    29th Jun 2001, Andy Smith:
!      Phase change section updated. Added Diag structure as output.
!      Added error analysis (in Diag setting).
!    12th Oct 2001, Andy Smith:
!      Costs are now divided by SPixel%Ny for output. Allows better comparison
!      between SPixels (since Ny may vary). Since costs are not divided during 
!      calculation for each iteration, the convergence criteria are multiplied
!      by Ny to compensate. Notes added in code and Algorithm section.
!      Also added brief description of error analysis to Algorithm section.
!    ***************************** ECV work starts here *****************
!    22nd Feb 2011, Andy Smith:
!      Re-applying changes made in late 2001/2002. 
!     5th Dec 2001, Andy Smith:
!      Introduced error checking throughout. See comments above for details
!      of the scheme implemented.
!    21st Dec 2001, Andy Smith:
!      Changing use of iteration counter so that only "good" steps are counted
!      as "iterations" (experimentally). The number of steps is still checked
!      against the Max allowed no. of iterations at present.
!      (Subsequently removed, so ignored for 2011 update). 
!    24th Feb 2011, Andy Smith:
!      stat now initialised (was done in ORAC code somewhere along the way). 
!    17th Mar 2011, Andy Smith: 
!      Removal of phase change. Code commented out for now, prior to eventual 
!      removal. 
!      SAD_LUT dimension reduced to 1, since only 1 cloud class in use. 
!    22nd Mar 2011, Andy Smith:
!     Removal of phase change, phase 2. SAD_CloudClass now has 1 dimension 
!     rather than N cloud classes, SAD_LUT also 1 since only 1 phase/cloud 
!     class is used for the whole retrieval. 
!    6th Apr 2011, Andy Smith:
!      Removal of redundant selection methods SAD and SDAD for limits checking.
!      SADCloudClass argument to Set_Limits no longer required. 
!      SADCloudClass argument to this function InvertMarquardt also redundant. 
!      Mopping up from removal of phase change. Removed setting of Spixel%FGPhase
!      as it is redundant. 
!  8th Jun 2011, Andy Smith:
!      Removed logging of errors from Invert_Cholesky and related functions, to !
!      prevent text file output slowing down execution (use ifdef DEBUG).
!
!      8th Aug 2011, Caroline Poulsen: remove ref to cloud class
!      3rd Oct 2011, Caroline Poulsen: fixed debug syntax error
!      18 Oct 2011, Caroline Poulsen: added lat/lon info to bkp output
!   13th Dec 2011 Caroline poulsen change format statement to make
!                g95 compatible 
!   19th Jan 2012 stored first guesss measurement
!   19th Jan 2012 C. Poulsen add variable  minusdJ_dX=-dJ_dX
!   19th Jan 2012 C. Poulsen changed calls to Invert_cholesky
!   24th feb 2012 C. Poulsen changed error_matrix call to Invert_cholesky
!  10th Aug 2012 C. Poulsen defined neasurement arays more explicitly
!                 other wise crashed for night measurements
!  14th Sep 2012 C. Poulsen bug fix defined diag%ss to size of nx elements (was ny) initialised Y
!  2nd Oct 2012 C. Poulsen initialised variables bug fix defined diag%ss changed how SPixel%sn calculated
!  8th May 2013 C. Poulsen set Diag%Y0
! Bugs: 
!    None known
!
! $Id: InvertMarquardt.f90 170 2011-10-03 08:05:16Z capoulse $
!
!---------------------------------------------------------------------

Subroutine Invert_Marquardt (Ctrl, SPixel, SAD_Chan, SAD_LUT, &
     & RTM_Pc, Diag, status)

   use ECP_Constants
   use Ctrl_def
   use SPixel_def
   use SAD_Chan_def
   use SAD_LUT_def
   use RTM_Pc_def
   use Diag_def

   Implicit none

!  Argument declarations

   type(Ctrl_t), intent(inout)           :: Ctrl
   type(SPixel_t), intent(inout)      :: SPixel
   type(SAD_Chan_t), intent(in)       :: SAD_Chan(Ctrl%Ind%Ny)
   type(SAD_LUT_t), intent(in)        :: SAD_LUT  
   type(RTM_Pc_t), intent(in)         :: RTM_Pc
   type(Diag_t), intent(out)          :: Diag
   integer, intent(inout)             :: status

!  Local variable declarations   

   real     :: Xplus_dX(MaxStateVar)
                                  ! State vector being checked in a given 
                                  ! iteration (Xn + delta_X)
   real     :: Y(SPixel%Ind%Ny)   ! TOA reflectances, radiances etc for 
                                  ! part-cloudy conditions. Returned by FM
   real     :: dY_dX(SPixel%Ind%Ny,MaxStateVar+1)
                                  ! Derivatives d[ref]/d[tau,Re,pc,f,Ts,Rs]t
   real     :: Kx(SPixel%Ind%Ny, SPixel%Nx)
                                  ! Scaled, active part of dY_dX
   real     :: Kbj(SPixel%Ind%Ny, SPixel%Ind%NSolar)
                                  ! Scaled, surface reflectance part of dY_dX
                                  ! Set by Set_Kx if Ctrl%Eqmpn%Rs is set.
   real     :: Ydiff(SPixel%Ind%Ny)
                                  ! Difference between measured and calculated Y
   real     :: Xdiff(SPixel%Nx)
                                  ! Difference between current state and a
                                  ! priori
   real     :: Sy(SPixel%Ind%Ny, SPixel%Ind%Ny)  
                                  ! Error covariance in measurements
   real     :: SyInv(SPixel%Ind%Ny, SPixel%Ind%Ny) 
                                  ! Inverse of Sy 
   real     :: SxInv(SPixel%Nx, SPixel%Nx) 
                                  ! Inverse of Sx. Sx is the error covariance 
                                  ! in the a priori state vector, part of
                                  ! SPixel.
   real     :: J0                 ! Cost at first guess state
   real     :: J, Jm, Ja          ! Cost at a given state (plus contributions
                                  ! to cost from measurements and a priori)
   real     :: delta_J            ! Change in cost between operations
   real     :: Ccj_Ny             ! Product of Cost convergence criteria and 
                                  ! number of active channels. 
   real     :: KxT_SyI(SPixel%Nx, SPixel%Ind%Ny)
                                  ! Product of the transpose of Kx with SyInv
   real     :: dJ_dX(SPixel%Nx)   ! 1st derivative of J wrt state     variables
   real     :: minusdJ_dX(SPixel%Nx)   ! 1st derivative of J wrt state variables
   real     :: d2J_dX2(SPixel%Nx, SPixel%Nx) 
                                  ! 2nd derivative of J wrt state variables
   real     :: delta_X(SPixel%Nx)
                                  ! Step to be applied to state variables 
   real     :: unit(SPixel%Nx, SPixel%Nx)
                                  ! Unit matrix (size matches no of active state
                                  ! variables).
   real     :: error_matrix(SPixel%Nx, SPixel%Nx)
   real     :: J2plus_A(SPixel%Nx, SPixel%Nx) 
                                  ! Temporary array to hold sum of d2J_dX2 and
                                  ! alpha * unit.
   real     :: Av_Hess            ! Average of Hessian (d2J_dX2) diagonal
   real     :: alpha              ! Marquardt control variable
   integer  :: iter               ! Inversion iteration counter
   logical  :: phase_change       ! Indicates phase change has occurred
   integer  :: NPhaseChanges      ! Phase change counter
   logical  :: convergence        ! Indicates whether convergence has occurred
   real     :: Kb(SPixel%Ind%Ny, SPixel%NxI+SPixel%Ind%NSolar)
                                  ! "Model parameter" gradients from inactive
                                  ! state variables and surface reflectance.
   real     :: Sb(SPixel%NxI+SPixel%Ind%NSolar, SPixel%NxI+SPixel%Ind%NSolar)
                                  ! "Model parameter" error covariance.
   real     :: Dy(SPixel%Nx, SPixel%Ind%Ny)
                                  ! "Inversion operator" used in error analysis
   real     :: Dy_Kb(SPixel%Nx, SPixel%NxI+SPixel%Ind%NSolar)
                                  ! Product of Dy and Kb.
!sstapelb unused variables
!   integer  :: bkp_lun, ios       ! Unit number and IO status value for 
                                  ! breakpoint output file
   integer  :: m, l               ! counter for short/implied do loops
                                  ! (can't use J: it's the cost function)
   character(120) :: message       ! String for error messages.
   integer  :: stat               ! Local error status flag

!  Initialise class indicator, Kbj and Kx

   Kx           = 0
   Y           = 0
   Kbj          = 0
   stat         = 0
   SPixel%Sn = 0   
   Diag%st=0
   d2J_dX2=0  
   kxt_syi=0
   dy_dx=0

   !  Set state variable limits for initial phase 
   
   call Set_Limits(Ctrl, SPixel, stat)
   



   !  Calculate measurements at first-guess state vector X0 (SPixel%X0) 
   !  X0 should be provided un-scaled. Only used in the FM call and Xdiff(?)
   ! AS Mar 2011 assume only 1 cloud class in use so SAD_LUT dimension is 1
   
   !   if (stat == 0) Call FM (Ctrl, SPixel, SAD_Chan, SAD_LUT(SPixel%Class), & 
   !      RTM_Pc,SPixel%X0, Y, dY_dX, stat) 

   if (stat == 0) Call FM (Ctrl, SPixel, SAD_Chan, SAD_LUT, & 
        RTM_Pc,SPixel%X0, Y, dY_dX, stat) 

   Diag%Y0(1:SPixel%Ind%Ny)=Y 

   !  Convert dY_dX to Kx and Kbj. 

   if (stat == 0) Call Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kbj, stat)


   !  Set covariance matrix Sy and calculate the inverses SyInv and SxInv,
   !  required for initial cost calculation. (Sx is already stored in SPixel).
   !  Always call Set_Kx before Set_Sy. Needs Kbj.

   if (stat == 0) Call Set_Sy(Ctrl, SPixel, Kbj, Sy, stat) 

   !  Calculate SyInv and SxInv
   
   if (stat == 0) then
      
      Call Invert_Cholesky(Sy, SyInv, SPixel%Ind%Ny, stat)

      if (stat == 0) &
           error_matrix=SPixel%Sx(SPixel%X,SPixel%X)

           Call Invert_Cholesky(error_matrix, SxInv, SPixel%Nx, &
           stat)


#ifdef DEBUG
      if (stat /= 0) &
           Call Write_Log(Ctrl, 'Invert_Marquardt: Error in Invert_Cholesky', &
           & stat)
#endif
   end if



   !  Calculate cost at X0. Requires inverse of Sx and Sy.
   !  Calculate Xdiff, the difference between the current state vector X and
   !  the a priori state vector SPixel%Xb for the active state variables, and 
   !  Ydiff, the difference between the measurements and calculated values for X.
   !  Xdiff requires scaling since X is in "natural" units for use by FM.
   !  Ja is 0 if X0 is the a priori (not generally true).
   !  Sx will be set by GetAPriori and is fixed for a given SPixel.
   !  Xb is assumed to be full-length and un-scaled.

   ydiff=0.0
   if (stat == 0) then
      Xdiff = (SPixel%X0(SPixel%X) - SPixel%Xb(SPixel%X)) &
           & * Ctrl%Invpar%XScale(SPixel%X)

      Ydiff(1:SPixel%Ind%Ny) = Y(1:SPixel%Ind%Ny) - SPixel%Ym(1:SPixel%Ind%Ny)
! sstapelb changed : error happens at change from day to night (5 to 2 channels) dont know if this is really right
!      Diag%YmFit= Ydiff
      Diag%YmFit(1:SPixel%Ind%NY)= Ydiff
      Ja    = dot_product(Xdiff, matmul(SxInv, Xdiff))
      Jm    = dot_product(Ydiff, matmul(SyInv, Ydiff)) 
      J0    = Jm + Ja
   end if

   !  Initial breakpoint output (Open breakpoint file if required)
   !  Note the breakpoint file must be closed before any call to a routine that
   !  will try to write to it.

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_InvertMarquardt_1) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      & 
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
         call Write_Log(Ctrl, &
              &       'Invert_Marquardt: Error opening breakpoint file', status)
      else
         write(bkp_lun,'(/,a)')'Invert_Marquardt:'
         if (Ctrl%Bkpl >= BkpL_InvertMarquardt_2) then
            write(bkp_lun,'(2x,a,2(f9.3,1x))') 'lat/lon: ',SPixel%Loc%Lat,SPixel%Loc%Lon
            write(bkp_lun,'(2x,a,11(f9.3,1x))') 'Measurements: ',Spixel%Ym
         end if
         if (Ctrl%Bkpl >= BkpL_InvertMarquardt_1) then
            write(bkp_lun,'(2x,a)')'First guess: ' ,SPixel%Y0
            write(bkp_lun,'(2x,a,5(f9.3,1x))')  'X0:           ',SPixel%X0
            write(bkp_lun,'(2x,a,5(f9.3,1x))')  'Xb:           ',SPixel%Xb
            write(bkp_lun,*)
            Diag%Y0(1:SPixel%Ind%Ny)=Y
            
            if (Ctrl%Bkpl >= BkpL_InvertMarquardt_4) then
               !              SPixel%Sx output takes account of the active state variables.
               !              Sx array is full size. SPixel%X(m) selects a row, SPixel%X
               !              selects the active parts of the row. Remove scaling for output.

               write(bkp_lun,'(2x,a)')'Sx:'
               do m=1,SPixel%Nx        
                  write(bkp_lun,'(2x,5(e9.2,1x))') &
                       & Spixel%Sx(SPixel%X(m),SPixel%X) / & 
                       & (Ctrl%Invpar%XScale(SPixel%X) * &
                       & Ctrl%Invpar%XScale(SPixel%X))
               end do
               write(bkp_lun,*)
            end if
         end if    ! End of bkp level 1 actions      
         close(bkp_lun)
      end if       ! End of actions on successful file open
   end if          ! End of bkp level 1 actions
#endif

   !  Derivatives of J0 are calculated inside the do loop (same code used for 
   !  phase change)
   
   !  Set Xn = X0 for 1st iteration

   SPixel%Xn = SPixel%X0


   !  Set up unit matrix (size matches number of active state variables) for 
   !  Marquardt calculations.

   call Set_Unit(SPixel, unit, stat)

   !  Adjust cost convergence criteria for the number of active channels in the
   !  super-pixel, since final costs will be divided by Ny for output.

   Ccj_Ny = Ctrl%Invpar%Ccj * SPixel%Ind%Ny

   !  Initialise loop control variables
   
!MJORG   convergence = .false.
   convergence = .false.
   phase_change = .false.
   NPhaseChanges = 0
   iter = 1

   do !  On phase change, re-start from here
      if (status /= 0 .or. stat /= 0) exit   

      !     Calculate derivatives of starting cost (required for setting Marquardt 
      !     parameters). Needs Kx, the scaled version of dY_dX.
      !     Calculate product of transpose(Kx) and inverse of Sy (used twice below).


      KxT_SyI = matmul(transpose(Kx), SyInv)

      !     The ATBD shows the 2nd term of dJ_dX (or J') as SxInv * Xdiff.
      !     The code here can either have matmul(SxInv, Xdiff), which is effectively
      !     SxInv * transpose(Xdiff), or matmul(Xdiff, SxInv). It looks impossible
      !     to reproduce the calculation in the ATBD. 
      !     Although it's generally true that matrix product AB != BA, if 
      !     A is 1-d and B is square and symmetric, AB = transpose(BA) and as far
      !     as Fortran is concerned the transpose of a 1-d array is no different
      !     from the original array (at least when adding).

      dJ_dX   = matmul(KxT_SyI, Ydiff) + matmul(SXInv, Xdiff)

      d2J_dX2 = matmul(KxT_SyI, Kx) + SxInv

      !     Set Marquardt parameters (initial weighting favours steepest descent)
      !     Unit matrix and inverse function required. 
      
      !     Average leading diagonal of Hessian d2J_dX2 in order to set alpha.
      !     Then use alpha and d2J_dX2 to set delta_X.
      !     Solve_Cholesky is used to find delta_X. The equation from the ATBD is
      !     dX = -(J'' + alphaI)^-1 * J' or delta_X = - (inv(J2plus_A) * dJ_dX)
      !     Multiplying through by J2_plusA we get:
      !       J2_plusA * delta_X = -dJ_dX
      !     which we can solve for delta_X using Solve_Cholesky.

      Av_Hess = 0.0
      do m=1,SPixel%Nx
         Av_Hess = Av_Hess + d2J_dX2(m,m)
      end do
      Av_Hess  = Av_Hess / SPixel%Nx
      alpha    = Ctrl%Invpar%MqStart * Av_Hess
      J2plus_A = d2J_dX2 + (alpha * unit)
      minusdJ_dX=-dJ_dX
      delta_X=0.0 !MJ!!!!

      Call Solve_Cholesky(J2plus_A, minusdJ_dX, delta_X, SPixel%Nx, stat)

#ifdef DEBUG 
      if (stat /= 0) &
           Call Write_Log(Ctrl, 'Invert_Marquardt: Error in Solve_Cholesky', &
           & stat)
#endif     
      !     De-scale delta_X so that the X passed to FM can be kept un-scaled

      delta_X = delta_X / Ctrl%Invpar%XScale(SPixel%X)
      
      !     Write starting parameter breakpoints and close breakpoint file so that 
      !     FM can write to it.

      Diag%Y0(1:SPixel%Ind%Ny) = Y(1:SPixel%Ind%Ny)

#ifdef BKP
      if (Ctrl%Bkpl >= BkpL_InvertMarquardt_2) then
         open(unit=bkp_lun,      & 
              & file=Ctrl%FID%Bkp, &
              & status='old',      &
              & position='append', &
              & iostat=ios)
         if (ios /= 0) then
            status = BkpFileOpenErr
            call Write_Log(Ctrl, &
                 & 'Invert_Marquardt: Error opening breakpoint file', status)
         else
            write(bkp_lun,'(a)')'Invert_Marquardt:'
            write(bkp_lun,'(2x,a,11(f9.3,1x))') 'Y:            ',Y
 
            if (Ctrl%Bkpl >= BkpL_InvertMarquardt_3) then
               write(bkp_lun,'(/,2x,a)')'Sy:'
               do m=1,SPixel%Ind%Ny
                  write(bkp_lun,'(2x,11(e11.3,1x))')Sy(m,:)
               end do

               write(bkp_lun,'(/,2x,a)') &
                    & 'Kx (columns are channels, rows active state variables)'
               do m=1,SPixel%Nx
                  write(bkp_lun,'(2x,11(e11.4,1x))') KX(:,m)
               end do

               if (Ctrl%Bkpl >= BkpL_InvertMarquardt_4) then
                  write(bkp_lun,'(/,2x,a,5(e11.3,1x))')  'Xdiff:        ', &
                       & Xdiff / Ctrl%Invpar%XScale(SPixel%X)

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
            end if   ! End of level 3 outputs

            !           Level 2 outputs - note costs are divided by Ny for output

            write(bkp_lun,'(2x,a,3(f9.3,1x))')  'Jm, Ja, J0 /Ny', &
                 & Jm/SPixel%Ind%Ny, Ja/SPixel%Ind%Ny, J0/SPixel%Ind%Ny
            write(bkp_lun,'(2x,a,5(f9.3,1x))')  'delta_X:      ',delta_X
            close(unit=bkp_lun)
         end if  ! End of actions on successful bkp file open
      end if
#endif

      !     Main iteration loop
      !     The iteration counter is not re-set on phase change. 
      !     A phase change counter is also used to avoid danger of oscillation between 
      !     phases.


      do 

         !        N.B. Jumps out of the loop if convergence or phase change occurs, 
         !        or if Max no. of iterations is reached, or if status not zero

         if (status /= 0 .or. stat /= 0) exit

         !        Apply step delta_x to the active state variables
         !        Assumes Xn and delta_X are both unscaled.

         Xplus_dX(SPixel%X)  = SPixel%Xn(SPixel%X) + delta_X

         Xplus_dX(SPixel%XI) = SPixel%Xn(SPixel%XI)

         !        Check bounds for active state variables - does delta_X take any state 
         !        variable outside it's range? (If so, freeze it at the boundary). 
         !        Also check for possible phase change. 

         call Check_Limits(Ctrl, Xplus_dX, SPixel, RTM_Pc, phase_change, stat)

         ! AS Mar 2011, Phase change
         !         if (phase_change) exit ! drop out of main iteration loop and start 
         ! again in the new phase.
         
         !        Calculate Y for Xn + delta_X. Xplus_dX is currently un-scaled.
         ! AS Mar 2011 assume only 1 cloud class in use so SAD_LUT dimension is 1
         

         if (stat == 0) Call FM (Ctrl, SPixel, SAD_Chan, &
              & SAD_LUT, RTM_Pc, Xplus_dX, Y, dY_dX, stat)

         !        Set new Kx, Kbj, Sy and SyInv

         if (stat == 0) Call Set_Kx(Ctrl, SPixel, dY_dX, Kx, Kbj, stat)
         if (stat == 0) Call Set_Sy(Ctrl, SPixel, Kbj, Sy, stat) 
         if (stat == 0) then

            Call Invert_Cholesky(Sy, SyInv, SPixel%Ind%Ny, stat)

            if (stat /= 0) then
#ifdef DEBUG
               Call Write_Log(Ctrl, &
                    & 'Invert_Marquardt: Error in Invert_Cholesky', stat)
#endif
            else
               !              Calculate new cost, J. 

               Xdiff = (Xplus_dX(SPixel%X) - SPixel%Xb(SPixel%X)) &
                    & * Ctrl%Invpar%XScale(SPixel%X)
               Ydiff = Y - SPixel%Ym
               Diag%YmFit= Ydiff
               Ja    = dot_product(Xdiff, matmul(SxInv, Xdiff))
               Jm    = dot_product(Ydiff, matmul(SyInv, Ydiff)) 
               J     = Jm + Ja

               !              Check J vs previous value. Lower value means progress:
               !              "accept" the step delta_X and reset the Marquardt values.

               delta_J = J-J0
            end if
         end if

         if (stat == 0 .and. J <= J0) then
            !           Good step. Add delta_X to Xn
            !           Calculate the Hessian (J'') as this is required for error analysis
            !           if convergence is reached or for setting new delta_X.
            
            SPixel%Xn = Xplus_dX
            KxT_SyI   = matmul(transpose(Kx), SyInv)
            d2J_dX2   = matmul(KxT_SyI, Kx) + SxInv

            !           Check for convergence. 
            !           Set Spixel%Xn: value is the solution X
            !           Save the pixel location if convergence occurs, for SDAD 
            !           first guess/a priori setting.

            ! Experimental change to include absolute cost check
            ! if (J < Ctrl%QC%MaxJ .or. abs(delta_J) <= Ctrl%Invpar%Ccj) then            

            if (abs(delta_J) <= Ccj_Ny) then
               convergence       = .true.
            else
               !              Decrease steepest descent part for next iteration            
               !              Save new J as J0 for checking next time round

               alpha = alpha / Ctrl%InvPar%MqStep
               J0 = J

               !              Calculate values required to set the new step delta_X based on 
               !              derivatives of J (delta_X itself is set after this "if")
               
               dJ_dX    = matmul(KxT_SyI, Ydiff) + matmul(SXInv, Xdiff)
               J2plus_A = d2J_dX2 + (alpha * unit)
               minusdJ_dX=-dJ_dX
               Call Solve_Cholesky(J2plus_A, minusdJ_dX, delta_X, SPixel%Nx, stat)

               !              Check stat retruned by Solve_Chol later - safe as long as no
               !              other routine is called.
            end if
         else ! No improvement in cost
            !           Increase steepest descent part for next iteration and set values
            !           required for setting new deltaX using old cost derivatives. 

            alpha    = alpha * Ctrl%InvPar%MqStep
            J2plus_A = d2J_dX2 + (alpha * unit)
            minusdJ_dX=-dJ_dX
            Call Solve_Cholesky(J2plus_A, minusdJ_dX, delta_X, SPixel%Nx, stat)

            !           Check stat retruned by Solve_Chol later - safe as long as no
            !           other routine is called.
         end if

           
            
         if (convergence) exit  ! Drops out of the iteration do loop

         !        Warn of errors in Solve_Cholesky (assumes no routine that can 
         !        overwrite status has been called since).
#ifdef DEBUG
         if (stat /= 0) &
              & Call Write_Log(Ctrl, 'Invert_Marquardt: Error in Solve_Cholesky', &
              & stat)
#endif
         !        Set and de-scale the new delta_X
         if (stat == 0) delta_X = delta_X / Ctrl%Invpar%XScale(SPixel%X)
         

         !        Main iteration loop breakpoint outputs. The file is opened and closed
         !        each time since (a) FM might also write to it and (b) the loop may 
         !        exit early if convergence is reached, leaving the file state uncertain.
         
#ifdef BKP
         if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_1) then
            call Find_Lun(bkp_lun)
            open(unit=bkp_lun,      & 
                 & file=Ctrl%FID%Bkp, &
                 & status='old',      &
                 & position='append', &
                 & iostat=ios)
            if (ios /= 0) then
               status = BkpFileOpenErr
               call Write_Log(Ctrl, &
                    &  'Invert_Marquardt: Error opening breakpoint file', status)
            else
        !              Level 1 outputs
        
               write(bkp_lun,'(/,a,i2)')'Invert_Marquardt Iteration ',iter
               write(bkp_lun,'(2x,a,5(f9.3,1x))')     'State:        ',Xplus_dX

               !              Level 2 outputs - note costs are divided by Ny for output

               if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_2) then
                  write(bkp_lun,'(2x,a,11(f9.3,1x))') 'Y:            ',Y
                  write(bkp_lun,'(2x,a,e11.3)')      'alpha:          ', alpha
                  write(bkp_lun,'(2x,a,5(f9.3,1x))')  'delta_X:      ', delta_X
                  write(bkp_lun,'(2x,a,3(f9.3,1x))')  'Jm, Ja, J:    ',&
                &Jm/SPixel%Ind%Ny, Ja/SPixel%Ind%Ny, J/SPixel%Ind%Ny
           write(bkp_lun,'(2x,a,f9.3,11x,f9.3)') 'delta J, J0:  ', &
                delta_J/SPixel%Ind%Ny, J0/SPixel%Ind%Ny

           if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_3) then
              write(bkp_lun,'(2x,a,5(e11.3,1x))')  'Xdiff:        ',&
                   & Xdiff  / Ctrl%Invpar%XScale(SPixel%X)
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
        end if  ! End of bkp level 2 actions
        
        close(unit=bkp_lun)
     end if     ! End of actions on successful file open
  end if        ! End of bkp level 1 actions
#endif
  
  !        Increment iteration counter and check whether the maximum has been
  !        passed. (Increment after breakpoint output so that counter value 
  !        is correct in the output).
  
  if (iter == Ctrl%Invpar%MaxIter) exit
  iter = iter + 1

end do !end do loop

!     End of the main iteration loop. Check for convergence, phase change
!     or too many iterations.

if (convergence) exit     ! Drops out of the "phase change" do loop
if (iter == Ctrl%Invpar%MaxIter) exit

! Phase change handling removed Mar 2011

end do      ! End of "phase change" do loop 

!  End of Marquardt inversion. 

!  Error analysis.
!  Error values are required for setting SPixel%Sn (used for SDAD FG/AP setting)
!  and for setting diagnostics St and Ss and QCFlag (if Diagl flags set).
!  St is the inverse Hessian (d2J_dX2) for the active state variables
!  d2J_dX2 is already sized to match the number of active variables.


if (stat == 0) then
   !state expected error from measurements=Diag%St(1:SPixel%Nx, 1:SPixel%Nx))

   Call Invert_Cholesky(d2J_dX2, Diag%St, &
        SPixel%Nx, stat)
#ifdef DEBUG
   if (stat /= 0) &
        Call Write_Log(Ctrl, 'Invert_Marquardt: Error in Invert_Cholesky', &
        stat)
#endif
end if

!  Ss is the state expected error from model parameter noise (inactive state
!  variables and surface reflectance Rs - this is the only model parameter 
!  for which gradient information is returned). Rs part is set only if 
!  Eqmpn for surface reflectance was not used.
!  Combine the inactive state variables part of Kx and surface reflectance 
!  gradients (if required) to get Kb. 
!  Set the error covariance Sb. Sx is full size (MaxStateVar): pick out the 
!  inactive variables parts. Write into "top left corner" of Sb rather than
!  use active/inactive sections. Initialise the whole array to 0 so that
!  the terms for correlation between the Rs/XI terms are 0.
!if (SPixel%Ind%NSolar .eq. 0) then 
!Ctrl%Eqmpn%Rs=0
!end if


if (stat == 0) then
   if (SPixel%NxI > 0 .or. Ctrl%Eqmpn%Rs == 0) then
      Dy = matmul(Diag%St(1:SPixel%Nx,1:SPixel%Nx), KxT_SyI)
      
      !        Kb(:, 1:SPixel%NxI) = Kx(:,SPixel%XI)
      !        Kx isn't actually set for inactive vars so we use a scaled dY_dX).
      
      do m=1,SPixel%NxI
         Kb(:,m) = dY_dX(:,SPixel%XI(m)) / Ctrl%Invpar%XScale(SPixel%XI(m))
      end do
      
      Sb = 0. 

      Sb(1:SPixel%NxI,1:SPixel%NxI) = Spixel%Sx(SPixel%XI,SPixel%XI)

     if (Ctrl%Eqmpn%Rs == 0 .and. SPixel%Ind%NSolar > 0) then
         !           Add Rs terms to Kb and Sb. Use the full Dy_Kb.
! Kbj(SPixel%Ind%Ny, SPixel%Ind%NSolar)         
         Kb(:, SPixel%NxI+1:SPixel%NxI+SPixel%Ind%NSolar) = Kbj
         Sb(SPixel%NxI+1:, SPixel%NxI+1:) = &
              SPixel%SRs(1:SPixel%Ind%NSolar, 1:SPixel%Ind%NSolar)

         Dy_Kb   = matmul(Dy, Kb)
         Diag%Ss = matmul(Dy_Kb, matmul(Sb, transpose(Dy_Kb)) ) 


      else
         !           No Rs terms: don't use all of Dy_Kb this is a thermal only retrieval

         Dy_Kb(:,1:Spixel%NxI) = matmul(Dy, Kb(:,1:SPixel%NxI))
!write(*,*)'a',Dy_Kb(:,1:Spixel%NxI)
!write(*,*)'b',(Sb(1:Spixel%NxI,1:Spixel%NxI)
!write(*,*)'c',transpose(Dy_Kb(:,1:Spixel%NxI))
         !MJ ORG Diag%Ss(1:SPixel%Nx,1:SPixel%Nx) = matmul(Dy_Kb(:,1:Spixel%NxI), &
         !MJ ORG matmul(Sb(1:Spixel%NxI,1:Spixel%NxI), &
         !MJ ORG transpose(Dy_Kb(:,1:Spixel%NxI))) ) 

         !this is a workaround otherwise a 3x3 matrix is written into a 2x2 matrix
         !MJ: temp. commented out
         Diag%Ss(1:SPixel%Ind%Ny+1,1:SPixel%Ind%Ny+1) = matmul(Dy_Kb(:,1:Spixel%NxI), &
              matmul(Sb(1:Spixel%NxI,1:Spixel%NxI), transpose(Dy_Kb(:,1:Spixel%NxI))) )

      end if
   else
      !        There are no inactive state vars and Eqmpn has been used for Rs, i.e.
      !        no info to set Ss. Needs some values.
      
      Diag%Ss = 0
   end if

   !     Assign St+Ss to the parts of SPixel%Sn where state vars are active.
   !     For inactive variables, Sn is set to the a priori error (squared). 
   !     De-scale (by XScale squared, i.e. the product of the XScale factors for
   !     the two state variables contributing to each element).
   
   Spixel%Sn = 0.0
   if (SPixel%Nx > 0  .and. SPixel%NxI == 0 ) then
      Spixel%Sn(SPixel%X, SPixel%X)   = &
           Diag%St(1:SPixel%Nx, 1:SPixel%Nx) 
!write(*,*)'prep prim uncertaintiesbb',sqrt(Spixel%Sn(1,1)),sqrt(Spixel%Sn(2,2)),sqrt(Spixel%Sn(3,3)),sqrt(Spixel%Sn(4,4)),sqrt(Spixel%Sn(5,5)),Spixel%Sn(4,4)
   endif

   if (SPixel%NxI > 0 ) then
      SPixel%Sn(SPixel%XI, SPixel%XI) = SPixel%Sx(SPixel%XI, SPixel%XI)
!      Spixel%Sn(SPixel%X, SPixel%X)   = &
!           Diag%St(1:SPixel%Nx, 1:SPixel%Nx)

!write(*,*)'prep prim uncertaintiesaa',sqrt(Spixel%Sn(1,1)),sqrt(Spixel%Sn(2,2)),sqrt(Spixel%Sn(3,3)),sqrt(Spixel%Sn(4,4)),sqrt(Spixel%Sn(5,5))
       Spixel%Sn(SPixel%X, SPixel%X)   = &
            Diag%St(1:SPixel%Nx, 1:SPixel%Nx) +Diag%Ss(1:SPixel%Nx, 1:SPixel%Nx) 
!changed sstapelb: 1:SPixel%Nx -> SPixel%X --> changed back again because of ctp_uncertainty at night
!       Spixel%Sn(SPixel%X, SPixel%X)   = &
!            Diag%St(SPixel%X, SPixel%X) +Diag%Ss(SPixel%X, SPixel%X) 

! write(*,*)'st',Diag%St(1:SPixel%Nx, 1:SPixel%Nx)
! write(*,*)'ss',Diag%Ss(1:SPixel%Nx, 1:SPixel%Nx)
!      write(*,*)'prep prim uncertaintiesbb',sqrt(Spixel%Sn(1,1)),sqrt(Spixel%Sn(2,2)),sqrt(Spixel%Sn(3,3)),sqrt(Spixel%Sn(4,4)),sqrt(Spixel%Sn(5,5)),Spixel%Sn(4,4)

  endif

   do m = 1, MaxStateVar
      do l = 1, MaxStateVar
         SPixel%Sn(l,m) = SPixel%Sn(l,m) / &
              (Ctrl%Invpar%XScale(l) * Ctrl%Invpar%XScale(m))
      end do
      if (SPixel%Sn(m,m) < 0) then
         write(unit=message, fmt='(a,i1,a,i1,a,e11.4,a,2(i4,1x))') &
              'Invert_Marquardt: warning: negative error value in Sn(', m, &
              ',', m, '), value: ',&
              SPixel%Sn(m,m), ' location x,y ', SPixel%Loc%X0, Spixel%Loc%Y0
         call Write_Log(Ctrl, message, stat)
         SPixel%Sn(m,m) = 0.0
      end if
   end do
   
else     ! stat is non-zero, inversion failed
   convergence = .false.
   Diag%Ss     = MissingSn
   Diag%St     = MissingSn
   Spixel%Sn   = MissingSn
end if   ! End of stat check before Ss setting.

!  Set remaining diagnostic values. Set_Diag also checks whether the solution
!  state was good enough to use in SDAD first guess and a priori setting, and
!  if so saves Xn and Sn in Spixel (XnSav and SnSav). Costs are divided by 
!  number of active instrument channels before output.

J  = J  / SPixel%Ind%Ny 
Jm = Jm / SPixel%Ind%Ny
Ja = Ja / SPixel%Ind%Ny

Call Set_Diag(Ctrl, SPixel, convergence, J, Jm, Ja, iter, &
     NPhaseChanges, Y, Sy, Diag, stat)


!  Write final solution and close breakpoint output file

#ifdef BKP
if (Ctrl%Bkpl >= BkpL_InvertMarquardt_1) then
   call Find_Lun(bkp_lun)
   open(unit=bkp_lun,      & 
        file=Ctrl%FID%Bkp, &
        status='old',      &
        position='append', &
        iostat=ios)
   if (ios /= 0) then
      status = BkpFileOpenErr
      call Write_Log(Ctrl, &
           'Invert_Marquardt: Error opening breakpoint file', status)
   else
      if (convergence) then      
         
         write(bkp_lun,'(/,2x,a,i2,a)')'Invert_marquardt: convergence after ',&
              iter, ' iterations'
      else
         write(bkp_lun,'(2x,a,i2,a)')&
              'Invert_marquardt: no convergence after ',iter,' iterations'
      end if
      write(bkp_lun,'(2x,a,5(f9.3,1x))') 'State:        ',SPixel%Xn
      
      !        Level 2 outputs - note costs output here have already been divided by
      !        Ny
      
      if (Ctrl%Bkpl >= Bkpl_InvertMarquardt_2) then
         !            write(bkp_lun,'(2x,a,11(f15.3,1x))')'Y:            ', Y
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

End Subroutine Invert_Marquardt
