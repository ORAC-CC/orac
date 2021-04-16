!-------------------------------------------------------------------------------
! Name: interpol_solar_spline.F90
!
! Purpose:
! Interpolates SW RTM transmittances onto current cloud pressure level
! NOTE: It is assumed that the pressure levels are ordered in decreasing
! pressure (increasing altitude).
!
! Description and Algorithm details:
! Check to see if Pc is outwith the RTM pressure level range
! (Warn if extrapolation beyond 50 hPa past the first/last RTM level is
!  necessary)
!    If not, search sequentially through the RTM pressure levels to find the
!       pair that are bounding Pc
!    Return an error if none of these conditions are satisfied (e.g. Pc=NaN)
! Determine the change in p and trans. between bounding RTM levels
! Determine transmittance gradients (d_T_d_p) around Pc
! Calculate change in trans. between RTM level i and Pc
! Calculate absolute trans. at Pc
!
! Arguments:
! Name   Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct In          Control structure
! SPixel struct Both        Super-pixel structure
! Pc     float  In          Current cloud pressure
! RTM_Pc struct Out         Contains Tac, Tbc (interpolated transmittances
!                           above and below cloud) and gradients wrt cloud
!                           pressure.
! status int    Out         Standard status value not set here
!
! History:
! 2000/11/15, KM: Original version
! 2001/02/01, AS: Pressure level indices reversed in SPixel RTM arrays, i.e.
!    values now increase with index.
! 2001/02/09, AS: First fully completed and (informally) tested version.
! 2001/02/15, AS: Channel ranges used for array size setting and indexing now
!    picked up from SPixel.
! 2001/02/16, AS: Checked through Pressure level tests and updated comments.
!    Pressure levels were originally intended to be stored from highest
!    pressure to lowest (i.e. ground upwards), but are now in order of pressure.
! 2001/02/20, AS: Removing excess underscores from RTM_Pc variable names.
! 2001/02/26, AS: Changed test for P levels bounding Pc. Using (Pc >= p(j) and
!    Pc <  p(j+1)) instead of (Pc >  p(j) and Pc <= p(j+1)) because P levels
!    now go in order of increasing pressure. Previously, if Pc was equal to
!    P(1) it was not matched (and if Pc < P(1) it's P index is set to 1!)
! 2011/09/05, CA: Added d2X_dP2 variables s/t a cubic spline interpolation can
!    be used. The second derivatives of the tabulated RTM data are calculated
!    using the routine 'spline'. These are used to calculate the interpolates/
!    interpolated gradients using the cubic spline method described in
!    'Numerical Recipes for fortran 90' [Press, Flannery, Teukolsky, Vetterling]
! 2012/02/06, CA: Explicit indexing when spline routine is called ie call
!    spline(SPixel%RTM%P(1:SPixel%RTM%Np), &
!           SPixel%RTM%SW%Tac(k,1:SPixel%RTM%Np), &
!           d2Tac_dP2(k,:))
!    replaced with -> call
!    spline(SPixel%RTM%P(1:SPixel%RTM%Np), &
!           SPixel%RTM%SW%Tbc(k,1:SPixel%RTM%Np), &
!           d2Tbc_dP2(k,1:SPixel%RTM%Np))
! 2014/07/08, CP: Small bug fix to BKP readout.
! 2014/08/05, GM: Cleaned up the code.
! 2014/08/05, GM: Put Interpol_* common code into subroutine find_Pc().
! 2015/01/21, AP: Updated channel indexing to array-based form.
! 2017/10/24, GM: Switch to official NR cubic spline code and make optional
!    through conditional compilation.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Interpol_Solar_spline(Ctrl, SPixel, Pc, RTM_Pc, status)

   use Ctrl_m
   use Int_Routines_m
   use ORAC_Constants_m
   use RTM_Pc_m
   use SPixel_m

   implicit none

   ! Define arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(in)    :: SPixel
   real,           intent(in)    :: Pc
   type(RTM_Pc_t), intent(inout) :: RTM_Pc
   integer,        intent(out)   :: status

   ! Define local variables

   integer :: i
   integer :: j
   integer :: Solar(SPixel%Ind%NSolar) ! Indices of solar channels for RTM_Pc%SW
   real    :: dP ! Fractional distance of Pc from bottom of interval
   real    :: P1 ! Fractional distance of Pc from top of interval
   real    :: k0 ! Spline coefficient
   real    :: k1 ! Spline coefficient
   real    :: delta_p
   real    :: delta_Tac(SPixel%Ind%NSolar)
   real    :: delta_Tbc(SPixel%Ind%NSolar)
   real    :: d2Tac_dP2(SPixel%Ind%NSolar,SPixel%RTM%Np)
   real    :: d2Tbc_dP2(SPixel%Ind%NSolar,SPixel%RTM%Np)

   ! Set initial value of error status equal to zero (i.e. no error)
   status = 0

   ! Subscripts for solar channels in RTM arrays
   Solar = SPixel%spixel_y_solar_to_ctrl_y_solar_index(:SPixel%Ind%NSolar)

   ! Search for Pc in the SW RTM pressure levels. If Pc lies outwith the RTM
   ! pressure levels avoid search and set index to 1 or the penultimate RTM level.
   call find_Pc(Ctrl, SPixel%RTM%Np, SPixel%RTM%P, Pc, i, status)

   if (status /= 0) then
      ! If none of the above conditions are met (e.g. Pc = NaN) then return with
      ! a fatal error
#ifdef DEBUG
      write(*, *) 'ERROR: Interpol_Solar_spline(): Interpolation failure, SPixel ' // &
         'starting at: ',SPixel%Loc%X0, SPixel%Loc%Y0, ', P(1), P(Np), Pc: ', &
         SPixel%RTM%P(1), SPixel%RTM%P(SPixel%RTM%Np), Pc
#endif
      status = IntTransErr
   else
      ! Start the interpolation or extrapolation calculations
      ! Note: Implicit looping over instrument channels from here onwards

      do j = 1, SPixel%Ind%NSolar
#ifdef INCLUDE_NR
         call spline(SPixel%RTM%P,SPixel%RTM%SW%Tac(Solar(j),:), &
                     SPixel%RTM%Np,1.e30,1.e30,d2Tac_dP2(j,:))
         call spline(SPixel%RTM%P,SPixel%RTM%SW%Tbc(Solar(j),:), &
                     SPixel%RTM%Np,1.e30,1.e30,d2Tbc_dP2(j,:))
#else
         write(*, *) 'ERROR: Interpol_Solar_spline(): Numerical Recipes is ' // &
            'not available for cubic spline interpolation'
         stop error_stop_code
#endif
      end do

      ! Change in pressure between RTM levels i and i+1
      ! (delta_p is negative for decreasing pressure with increasing i)
      delta_p =  SPixel%RTM%P(i+1) - SPixel%RTM%P(i)

      dP = (SPixel%RTM%P(i+1)-Pc)/delta_p
      p1 = 1.0 - dP

      k0 = (((3.0*dP*dP)-1.0)/6.0) * delta_p
      k1 = (((3.0*p1*p1)-1.0)/6.0) * delta_p

      ! Change in transmittances between RTM levels i and i+1
      ! (delta_Tac/bc are positive for increasing trans. with increasing i)
      delta_Tac = SPixel%RTM%SW%Tac(Solar,i+1) - SPixel%RTM%SW%Tac(Solar,i)
      delta_Tbc = SPixel%RTM%SW%Tbc(Solar,i+1) - SPixel%RTM%SW%Tbc(Solar,i)

      ! Gradients of transmittance w.r.t. pressure (around Pc)
      do j = 1, SPixel%Ind%NSolar
         RTM_Pc%SW%dTac_dPc(Solar(j)) = (delta_Tac(j) / delta_p) - &
              (k0 * d2Tac_dP2(j,i)) + (k1 * d2Tac_dP2(j,i+1))
         RTM_Pc%SW%dTbc_dPc(Solar(j)) = (delta_Tbc(j) / delta_p) - &
              (k0 * d2Tbc_dP2(j,i)) + (k1 * d2Tbc_dP2(j,i+1))
      end do

      ! Interpolated transmittances
      ! (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
      ! levels then extrapolation takes place using the same equations as for
      ! interpolation. Note: The sign of delta_Pc will change for Pc greater than
      ! the pressure of the lowest altitude RTM pressure level)

      k0 = (((dP*dP*dP)-dP) * (delta_p*delta_p))/6.0
      k1 = (((p1*p1*p1)-p1) * (delta_p*delta_p))/6.0

      do j = 1, SPixel%Ind%NSolar
         RTM_Pc%SW%Tac(Solar(j)) = (dP * SPixel%RTM%SW%Tac(Solar(j),i)) + &
             (p1 * SPixel%RTM%SW%Tac(Solar(j),i+1)) + &
             (k0 * d2Tac_dP2(j,i)) + (k1 * d2Tac_dP2(j,i+1))

         RTM_Pc%SW%Tbc(Solar(j)) = (dP * SPixel%RTM%SW%Tbc(Solar(j),i)) + &
             (p1 * SPixel%RTM%SW%Tbc(Solar(j),i+1)) + &
             (k0 * d2Tbc_dP2(j,i)) + (k1 * d2Tbc_dP2(j,i+1))
      end do
   end if

end subroutine Interpol_Solar_spline
