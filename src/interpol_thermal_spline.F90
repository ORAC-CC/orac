!-------------------------------------------------------------------------------
! Name: interpol_thermal_spline.F90
!
! Purpose:
! Interpolates LW transmittances and radiances to the cloud pressure level.
!
! Description and Algorithm details:
!
! Arguments:
! Name     Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct      In          Control structure
! SPixel   struct      In          Super-pixel structure
! Pc       float       In          Cloud pressure (from state vector X)
! SAD_Chan float array In          Channel characteristics.
! RTM_Pc   struct      Out         Contains Tac, Tbc (interpolated
!                                  transmittances above and below cloud) and
!                                  gradients wrt cloud pressure.
! status   int         Out         Standard status value not set here
!
! History:
! 2000/11/21, KM: Original version
! 2001/02/14, KM: Corrected the position of the end of the first main if block.
! 2001/02/16, AS: Using RTM_Pc struct to pass Tac, Tbc etc.
! 2001/02/20, AS: Converting calculations from B to T, since B is not available
!    from the SPixel RTM input. SAD_Chan is now required as an input argument.
! 2001/02/26, AS: Changed test for P levels bounding Pc. Using (Pc >= p(j) and
!    Pc < p(j+1)) instead of (Pc > p(j) and Pc <= p(j+1)) because P levels now
!    go in order of increasing pressure. Previously, if Pc was equal to P(1) it
!    was not matched (and if Pc < P(1) it's P index is set to 1!)
! 2001/03/01, AS: Fixed dB_dT declaration. Was scalar, should have been array
!    (NThermal).
! 2001/03/15, AS: Added ThF and ThL indices for RTM_Pc%LW arrays. Required
!    because these arrays are allocated to match the no. of thermal channels
!    requested, but in twilight not all of the requested thermal channels may
!    be used.
! 2001/05/11, AS: Added setting of RTM_Pc%Tc.
! 2001/07/20, AS: Fixed setting of ThL. Last thermal channel should always be
!    the one specified in Ctrl, not SPixel, since the RTM_Pc and RTM arrays are
!    of fixed size for the whole run, i.e are not re-allocated to match the no.
!    of thermal channels used in each SPixel, and it is the lower numbered
!    thermal channels that are not used in certain conditions. Added breakpoint
!    outputs.
! 2001/12/07, AS: Added more information to interpolation failure warning
!    message. Changed first test to locate Pc in the RTM pressure levels, now
!    uses >= rather than > in the test of Pc vs. SPixel...P(...Np). Previously,
!    Pc = max RTM P level was flagged as an interpolation failure.
! 2002/12/12, CP: Added in GPH followed the example of temperature
! 2002/12/23, AS: Added height gradient dHc_dPC to RTM_Pc structure. Required
!    later for ascribing an error value to the height.
! 2003/05/21, SD: Added temperature gradient dTc_dPC to RTM_Pc structure for
!    same reason
! 2011/09/05, CA: Added d2X_dP2 variables s/t a cubic spline interpolation can
!    be used. The second derivatives of the tabulated RTM data are calculated
!    using the routine 'spline'. These are used to calculate the interpolates/
!    interpolated gradients using the cubic spline method described in
!    'Numerical Recipes for fortran 90' [Press, Flannery, Teukolsky, Vetterling]
! 2011/09/05, CA: Added surf_pressure and surf_index variables - RTM data is
!    only interpolated in range surf > TOA.
! 2011/11/02, CP: Added 'spline' into debug output.
! 2012/02/06, CA: Removed surf_pressure/surf_index - bug fix to deal with
!    compiler issues.
! 2012/02/07, CA: Added intent() to argument declarations.
! 2013/01/17, MJ: Adds code to extract RTM_Pc%dHc_dPc and RTM_Pc%dTc_dPc.
! 2014/08/05, GM: Cleaned up the code.
! 2014/08/05, GM: Put Interpol_* common code into subroutine find_Pc().
! 2015/01/07, AP: Use SPixel index arrays rather than ThF,ThL.
! 2015/01/21, AP: Finishing the previous commit.
! 2015/05/07, CP: Removed the stop IntTransErr.
! 2017/10/24, GM: Switch to official NR cubic spline code and make optional
!    through conditional compilation.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Interpol_Thermal_spline(Ctrl, SPixel, Pc, SAD_Chan, RTM_Pc, status)

   use Ctrl_m
   use Int_Routines_m
   use ORAC_Constants_m
   use planck_m
   use RTM_Pc_m
   use SAD_Chan_m
   use SPixel_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(in)    :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc
   real,             intent(in)    :: Pc
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i
   integer :: j
   real    :: dP                         ! Fractional distance of Pc from bottom of interval
   real    :: P1                         ! Fractional distance of Pc from top of interval
   real    :: k0                         ! Spline coefficient
   real    :: k1                         ! Spline coefficient
   real    :: delta_p
   real    :: delta_Tac(SPixel%Ind%NThermal)
   real    :: delta_Tbc(SPixel%Ind%NThermal)
   real    :: delta_Rac_up(SPixel%Ind%NThermal)
   real    :: delta_Rac_dwn(SPixel%Ind%NThermal)
   real    :: delta_Rbc_up(SPixel%Ind%NThermal)
   real    :: delta_T                    ! Step in Temp between pressure levels
   real    :: delta_H                    ! Step in GPH between pressure levels
   real    :: dT_dPc                     ! Gradient of T wrt Pc
   real    :: dH_dPc                     ! Gradient of GPH wrt Pc
   real    :: T(SPixel%Ind%NThermal)     ! Calculated Temp at Pc. T2R wants array
   real    :: H(SPixel%Ind%NThermal)     ! Calculated GPH at Pc.
   real    :: d2T_dP2(SPixel%RTM%Np)
   real    :: d2H_dP2(SPixel%RTM%Np)
   real    :: d2Tac_dP2(SPixel%Ind%NThermal,SPixel%RTM%Np)
   real    :: d2Tbc_dP2(SPixel%Ind%NThermal,SPixel%RTM%Np)
   real    :: d2Rac_up_dP2(SPixel%Ind%NThermal,SPixel%RTM%Np)
   real    :: d2Rac_dwn_dP2(SPixel%Ind%NThermal,SPixel%RTM%Np)
   real    :: d2Rbc_up_dP2(SPixel%Ind%NThermal,SPixel%RTM%Np)
   real    :: delta_Pc
   real    :: R(SPixel%Ind%NThermal)
   real    :: dB_dT(SPixel%Ind%NThermal) ! Gradient of Planck function wrt Temp.
   integer :: Thermal(SPixel%Ind%NThermal) ! Indices of thermal channel for RTM_Pc%LW arrays

   ! Set initial value of error status equal to zero (i.e. no error)
   status = 0

   Thermal = SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(:SPixel%Ind%NThermal)

   ! Search for Pc in the LW RTM pressure levels. If Pc lies outwith the RTM
   ! pressure levels avoid search and set index to 1 or the penultimate RTM level
   call find_Pc(Ctrl, SPixel%RTM%Np, SPixel%RTM%P, Pc, i, status)

   if (status /= 0) then
      ! If none of the above conditions are met (e.g. Pc = NaN) then return with
      ! a fatal error
#ifdef DEBUG
      write(*, *) 'ERROR: Interpol_Thermal_spline(): Interpolation failure, SPixel ' // &
         'starting at: ',SPixel%Loc%X0, SPixel%Loc%Y0, ', P(1), P(Np), Pc: ', &
         SPixel%RTM%P(1), SPixel%RTM%P(SPixel%RTM%Np), Pc
#endif
      status = IntTransErr
   else
      ! Start the interpolation or extrapolation calculations
      ! Note: Implicit looping over instrument channels from here onwards

      do j = 1,SPixel%Ind%NThermal
#ifdef INCLUDE_NR
         call spline(SPixel%RTM%P,SPixel%RTM%LW%Tac(Thermal(j),:), &
                     SPixel%RTM%Np,1.e30,1.e30,d2Tac_dP2(j,:))
         call spline(SPixel%RTM%P,SPixel%RTM%LW%Tbc(Thermal(j),:), &
                     SPixel%RTM%Np,1.e30,1.e30,d2Tbc_dP2(j,:))
         call spline(SPixel%RTM%P,SPixel%RTM%LW%Rac_up(Thermal(j),:), &
                     SPixel%RTM%Np,1.e30,1.e30,d2Rac_up_dP2(j,:))
         call spline(SPixel%RTM%P,SPixel%RTM%LW%Rac_dwn(Thermal(j),:), &
                     SPixel%RTM%Np,1.e30,1.e30,d2Rac_dwn_dP2(j,:))
         call spline(SPixel%RTM%P,SPixel%RTM%LW%Rbc_up(Thermal(j),:), &
                     SPixel%RTM%Np,1.e30,1.e30,d2Rbc_up_dP2(j,:))
#else
      write(*, *) 'ERROR: Interpol_Solar_spline(): Numerical Recipes is ' // &
         'not available for cubic spline interpolation'
      stop error_stop_code
#endif
      end do
#ifdef INCLUDE_NR
      call spline(SPixel%RTM%P,SPixel%RTM%T,SPixel%RTM%Np,1.e30,1.e30,d2T_dP2)
      call spline(SPixel%RTM%P,SPixel%RTM%H,SPixel%RTM%Np,1.e30,1.e30,d2H_dP2)
#else
      write(*, *) 'ERROR: Interpol_Solar_spline(): Numerical Recipes is ' // &
         'not available for cubic spline interpolation'
      stop error_stop_code
#endif
      ! Change in pressure between RTM levels i and i+1
      ! (delta_p is negative for decreasing pressure with increasing i)

      delta_p = SPixel%RTM%P(i+1) - SPixel%RTM%P(i)

      dP = (SPixel%RTM%P(i+1)-Pc)/delta_p
      p1 = 1.0 - dP

      k0 = (((3.0*dP*dP)-1.0)/6.0) * delta_p
      k1 = (((3.0*p1*p1)-1.0)/6.0) * delta_p

      ! Change in transmittances between RTM levels i and i+1
      ! (delta_Tac/bc are positive for increasing trans. with increasing i)
      delta_Tac = SPixel%RTM%LW%Tac(Thermal,i+1) - SPixel%RTM%LW%Tac(Thermal,i)
      delta_Tbc = SPixel%RTM%LW%Tbc(Thermal,i+1) - SPixel%RTM%LW%Tbc(Thermal,i)

      ! Gradients of transmittance w.r.t. pressure (around Pc)
      do j = 1,SPixel%Ind%NThermal
         RTM_Pc%LW%dTac_dPc(Thermal(j)) = (delta_Tac(j) / delta_p) - &
              (k0 * d2Tac_dP2(j,i)) + (k1 * d2Tac_dP2(j,i+1))
         RTM_Pc%LW%dTbc_dPc(Thermal(j)) = (delta_Tbc(j) / delta_p) - &
              (k0 * d2Tbc_dP2(j,i)) + (k1 * d2Tbc_dP2(j,i+1))
      end do

      ! Change in radiances between RTM levels i and i+1
      delta_Rac_up  = &
           SPixel%RTM%LW%Rac_up(Thermal,i+1) - SPixel%RTM%LW%Rac_up(Thermal,i)
      delta_Rac_dwn = &
           SPixel%RTM%LW%Rac_dwn(Thermal,i+1) - SPixel%RTM%LW%Rac_dwn(Thermal,i)
      delta_Rbc_up  = &
           SPixel%RTM%LW%Rbc_up(Thermal,i+1) - SPixel%RTM%LW%Rbc_up(Thermal,i)

      ! Gradients of radiances w.r.t. pressure (around Pc)
      do j = 1,SPixel%Ind%NThermal
          RTM_Pc%LW%dRac_up_dPc(Thermal(j)) = (delta_Rac_up(j) / delta_p) - &
              (k0 * d2Rac_up_dP2(j,i)) + (k1 * d2Rac_up_dP2(j,i+1))
          RTM_Pc%LW%dRac_dwn_dPc(Thermal(j)) = (delta_Rac_dwn(j) / delta_p) - &
              (k0 * d2Rac_dwn_dP2(j,i)) + (k1 * d2Rac_dwn_dP2(j,i+1))
          RTM_Pc%LW%dRbc_up_dPc(Thermal(j)) = (delta_Rbc_up(j) / delta_p) - &
              (k0 * d2Rbc_up_dP2(j,i)) + (k1 * d2Rbc_up_dP2(j,i+1))
      end do

      ! Change in temperature between RTM levels i and i+1
      delta_T = SPixel%RTM%T(i+1) - SPixel%RTM%T(i)

      ! Change in GPH between RTM levels i and i+1
      delta_H = SPixel%RTM%H(i+1) - SPixel%RTM%H(i)

      ! Gradient of Planck functions w.r.t. pressure (around Pc)
      dT_dPc = (delta_T / delta_p) - (k0 * d2T_dP2(i)) + (k1 * d2T_dP2(i+1))
      RTM_Pc%dTc_dPc = dT_dPc

      ! Gradient of delta GPH w.r.t. pressure (around Pc)
      dH_dPc = (delta_H / delta_p) - (k0 * d2H_dP2(i)) + (k1 * d2H_dP2(i+1))
      RTM_Pc%dHc_dPc = dH_dPc

      ! Interpolated transmittances
      ! (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
      ! levels then extrapolation takes place using the same equations as for
      ! interpolation. Note: The sign of delta_Pc will change for Pc greater than
      ! the pressure of the lowest altitude RTM pressure level)

      k0 = (((dP*dP*dP)-dP) * (delta_p*delta_p))/6.0
      k1 = (((p1*p1*p1)-p1) * (delta_p*delta_p))/6.0

      delta_Pc = Pc - SPixel%RTM%P(i) ! Diff. between Pc and lower RTM level

      ! Diff. in trans from gradient
      do j = 1,SPixel%Ind%NThermal
         RTM_Pc%LW%Tac(Thermal(j)) = (dP * SPixel%RTM%LW%Tac(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Tac(Thermal(j),i+1)) + &
              (k0 * d2Tac_dP2(j,i)) + (k1 * d2Tac_dP2(j,i+1))

      ! Diff. in trans from gradient
         RTM_Pc%LW%Tbc(Thermal(j)) = (dP * SPixel%RTM%LW%Tbc(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Tbc(Thermal(j),i+1)) + &
              (k0 * d2Tbc_dP2(j,i)) + (k1 * d2Tbc_dP2(j,i+1))

      ! Interpolated radiances
         RTM_Pc%LW%Rac_up(Thermal(j)) = (dP * SPixel%RTM%LW%Rac_up(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Rac_up(Thermal(j),i+1)) + &
              (k0 * d2Rac_up_dP2(j,i)) + (k1 * d2Rac_up_dP2(j,i+1))

         RTM_Pc%LW%Rac_dwn(Thermal(j)) = (dP * SPixel%RTM%LW%Rac_dwn(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Rac_dwn(Thermal(j),i+1)) + &
              (k0 * d2Rac_dwn_dP2(j,i)) + (k1 * d2Rac_dwn_dP2(j,i+1))

         RTM_Pc%LW%Rbc_up(Thermal(j))  = (dP * SPixel%RTM%LW%Rbc_up(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Rbc_up(Thermal(j),i+1)) + &
              (k0 * d2Rbc_up_dP2(j,i)) + (k1 * d2Rbc_up_dP2(j,i+1))
      end do

      ! Interpolated Planck functions: calculate T and convert to B using T2R
      ! delta_T here is the step from the next lowest level to the current Pc,
      ! unlike above where it is the step between RTM levels.
      delta_T = delta_Pc * dT_dPc

      ! Set up GPH
      delta_H = delta_Pc * dH_dPc

      ! Set current temperature RTM_Pc%Tc and calculate equivalent radiance.
      ! (T2R requires T to be an array).
      RTM_Pc%Tc = (dP * SPixel%RTM%T(i)) + &
                  (p1 * SPixel%RTM%T(i+1)) + &
                  (k0 * d2T_dP2(i)) + (k1 * d2T_dP2(i+1))
      T = RTM_Pc%Tc

      call T2R(SPixel%Ind%NThermal, SAD_Chan, T, R, dB_dT, status)
      RTM_Pc%LW%B(Thermal) = R
      RTM_Pc%LW%dB_dPc(Thermal) = dT_dPc * dB_dT

      ! Set current GPH and save the rate of change w.r.t. Pc for use later
      RTM_Pc%Hc = (dP * SPixel%RTM%H(i)) + &
                  (p1 * SPixel%RTM%H(i+1)) + &
                  (k0 * d2H_dP2(i)) + (k1 * d2H_dP2(i+1))
      H = RTM_Pc%Hc
      RTM_Pc%dHc_dPc = dH_dPc
   end if

end subroutine Interpol_Thermal_spline
