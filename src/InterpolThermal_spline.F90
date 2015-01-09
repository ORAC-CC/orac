!-------------------------------------------------------------------------------
! Name:
!    Interpol_Thermal_spline
!
! Purpose:
!    Interpolates LW transmittances and radiances to the cloud pressure level.
!
! Arguments:
!    Name     Type        In/Out/Both Description
!    Ctrl     struct      In          Control structure
!    SPixel   struct      In          Super-pixel structure
!    Pc       float       In          Cloud pressure (from state vector X)
!    SAD_Chan float array In          Channel characteristics.
!    RTM_Pc   struct      Out         Contains Tac, Tbc (interpolated
!                                     transmittances above and below cloud) and
!                                     gradients wrt cloud pressure.
!    status   int         Out         Standard status value not set here
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    21st November, 2000, Kevin M. Smith: Original version
!    14th February, 2001, Kevin M. Smith:
!       Corrected the position of the end of the first main if block.
!    16th Feb 2001, Andy Smith:
!       Using RTM_Pc struct to pass Tac, Tbc etc.
!    20th Feb 2001, Andy Smith:
!       Converting calculations from B to T, since B is not available from the
!       SPixel RTM input. SAD_Chan is now required as an input argument.
!    26th Feb 2001, Andy Smith:
!       Changed test for P levels bounding Pc. Using
!       (Pc >= p(j) and Pc < p(j+1)) instead of
!       (Pc > p(j) and Pc <= p(j+1)) because P levels now go in order of
!       increasing pressure. Previously, if Pc was equal to P(1) it was not
!       matched (and if Pc < P(1) it's P index is set to 1!)
!     1st March 2001, Andy Smith:
!       Fixed dB_dT declaration. Was scalar, should have been array (Nthermal).
!    15th Mar 2001, Andy Smith:
!       Added ThF and ThL indices for RTM_Pc%LW arrays. Required because these
!       arrays are allocated to match the no. of thermal channels requested, but
!       in twilight not all of the requested thermal channels may be used.
!    11th May 2001, Andy Smith:
!       Added setting of RTM_Pc%Tc.
!    20th Jul 2001, Andy Smith:
!       Fixed setting of ThL. Last thermal channel should always be the one
!       specified in Ctrl, not SPixel, since the RTM_Pc and RTM arrays are of
!       fixed size for the whole run, i.e are not re-allocated to match the no.
!       of thermal channels used in each SPixel, and it is the lower numbered
!       thermal channels that are not used in certain conditions.
!       Added breakpoint outputs.
!     7th Dec 2001, Andy Smith:
!       Added more information to interpolation failure warning message.
!       Changed first test to locate Pc in the RTM pressure levels, now uses
!       >= rather than > in the test of Pc vs. SPixel...P(...Np). Previously,
!       Pc = max RTM P level was flagged as an interpolation failure.
!     12th Dec 2002, Caroline Poulsen:
!       Added in GPH followed the example of temperature
!     23rd Dec 2002, Andy Smith:
!       Added height gradient dHc_dPC to RTM_Pc structure. Required later for
!       ascribing an error value to the height.
!     21st May 2003, Sam Dean:
!       Added temperature gradient dTc_dPC to RTM_Pc structure for same reason
!     5th Sep 2011, Chris Arnold:
!       Added d2X_dP2 variables s/t a cubic spline interpolation can be used.
!       The second derivatives of the tabulated RTM data are calculated using
!       the routine 'spline'. These are used to calculate the interpolates/
!       interpolated gradients using the cubic spline method described in
!       'Numerical Recipes for fortran 90' [Press, Flannery, Teukolsky,
!       Vetterling]
!     5th Sep 2011, Chris Arnold:
!       Added surf_pressure and surf_index variables - RTM data is only
!       interpolated in range surf > TOA.
!     2nd Nov 2011, Caroline Poulsen:
!       Added 'spline' into debug output.
!     6th Feb 2012, Chris Arnold:
!       Removed surf_pressure/surf_index - bug fix to deal with compiler issues.
!     7th Feb 2012, Chris Arnold:
!       Added intent() to argument declarations.
!    17th Jan 2013, Matthias Jerg:
!       Adds code to extract RTM_Pc%dHc_dPc and RTM_Pc%dTc_dPc.
!     5th Aug 2014, Greg McGarragh:
!       Cleaned up the code.
!     5th Aug 2014, Greg McGarragh:
!       Put Interpol_* common code into subroutine find_Pc().
!     7th Jan 2015, Adam Povey:
!       Use SPixel index arrays rather than ThF,ThL.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Interpol_Thermal_spline(Ctrl, SPixel, Pc, SAD_Chan, RTM_Pc, status)

   use CTRL_def
   use ECP_Constants
   use Int_Routines_def
   use RTM_Pc_def
   use SAD_Chan_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(in)    :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(SPixel%Ind%Nthermal)
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
   real    :: delta_Tac(SPixel%Ind%Nthermal)
   real    :: delta_Tbc(SPixel%Ind%Nthermal)
   real    :: delta_Rac_up(SPixel%Ind%Nthermal)
   real    :: delta_Rac_dwn(SPixel%Ind%Nthermal)
   real    :: delta_Rbc_up(SPixel%Ind%Nthermal)
   real    :: delta_T                    ! Step in Temp between pressure levels
   real    :: delta_H                    ! Step in GPH between pressure levels
   real    :: dT_dPc                     ! Gradient of T wrt Pc
   real    :: dH_dPc                     ! Gradient of GPH wrt Pc
   real    :: T(SPixel%Ind%Nthermal)     ! Calculated Temp at Pc. T2R wants array
   real    :: H(SPixel%Ind%Nthermal)     ! Calculated GPH at Pc.
   real    :: d2T_dP2(SPixel%RTM%LW%Np)
   real    :: d2H_dP2(SPixel%RTM%LW%Np)
   real    :: d2Tac_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
   real    :: d2Tbc_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
   real    :: d2Rac_up_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
   real    :: d2Rac_dwn_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
   real    :: d2Rbc_up_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
   real    :: delta_Pc
   real    :: R(SPixel%Ind%NThermal)
   real    :: dB_dT(SPixel%Ind%Nthermal) ! Gradient of Planck function wrt Temp.
   integer :: Thermal(SPixel%Ind%NThermal) ! Indices of thermal channel for RTM_Pc%LW arrays
#ifdef BKP
   integer :: bkp_lun ! Unit number for breakpoint file
   integer :: ios     ! I/O status for breakpoint file
#endif

   ! Set initial value of error status equal to zero (i.e. no error)
   status = 0

   Thermal = SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(:SPixel%Ind%NThermal)

   ! Search for Pc in the LW RTM pressure levels. If Pc lies outwith the RTM
   ! pressure levels avoid search and set index to 1 or the penultimate RTM level
   call find_Pc(Ctrl, SPixel%RTM%LW%Np, SPixel%RTM%LW%P, Pc, i, status)

   if (status /= 0) then
      ! If none of the above conditions are met (e.g. Pc = NaN) then return with
      ! a fatal error
      write(*, *) 'ERROR: Interpol_Thermal(): Interpolation failure, SPixel ' // &
         'starting at: ',SPixel%Loc%X0, SPixel%Loc%Y0, ', P(1), P(Np), Pc: ', &
         SPixel%RTM%SW%P(1), SPixel%RTM%SW%P(SPixel%RTM%SW%Np), Pc
      status = IntTransErr
!     stop IntTransErr
   else
      ! Start the interpolation or extrapolation calculations
      ! Note: Implicit looping over instrument channels from here onwards

      do j = 1,SPixel%Ind%NThermal
         call spline(SPixel%RTM%LW%P, &
            SPixel%RTM%LW%Tac(Thermal(j),:),d2Tac_dP2(Thermal(j),:))
         call spline(SPixel%RTM%LW%P, &
            SPixel%RTM%LW%Tbc(Thermal(j),:),d2Tbc_dP2(Thermal(j),:))
         call spline(SPixel%RTM%LW%P, &
            SPixel%RTM%LW%Rac_up(Thermal(j),:),d2Rac_up_dP2(Thermal(j),:))
         call spline(SPixel%RTM%LW%P, &
            SPixel%RTM%LW%Rac_dwn(Thermal(j),:),d2Rac_dwn_dP2(Thermal(j),:))
         call spline(SPixel%RTM%LW%P, &
            SPixel%RTM%LW%Rbc_up(Thermal(j),:),d2Rbc_up_dP2(Thermal(j),:))
      end do

      call spline(SPixel%RTM%LW%P,SPixel%RTM%LW%T,d2T_dP2)
      call spline(SPixel%RTM%LW%P,SPixel%RTM%LW%H,d2H_dP2)

      ! Change in pressure between RTM levels i and i+1
      ! (delta_p is negative for decreasing pressure with increasing i)

      delta_p = SPixel%RTM%LW%P(i+1) - SPixel%RTM%LW%P(i)

      dP = (SPixel%RTM%LW%P(i+1)-Pc)/delta_p
      p1 = 1.0 - dP

      k0 = (((3.0*dP*dP)-1.0)/6.0) * delta_p
      k1 = (((3.0*p1*p1)-1.0)/6.0) * delta_p

      ! Change in transmittances between RTM levels i and i+1
      ! (delta_Tac/bc are positive for increasing trans. with increasing i)
      delta_Tac = SPixel%RTM%LW%Tac(Thermal,i+1) - SPixel%RTM%LW%Tac(Thermal,i)
      delta_Tbc = SPixel%RTM%LW%Tbc(Thermal,i+1) - SPixel%RTM%LW%Tbc(Thermal,i)

      ! Gradients of transmittance w.r.t. pressure (around Pc)
      do j = 1,SPixel%Ind%NThermal
         RTM_Pc%LW%dTac_dPc(Thermal(j)) = (delta_Tac(Thermal(j)) / delta_p) - &
              (k0 * d2Tac_dP2(Thermal(j),i)) + (k1 * d2Tac_dP2(Thermal(j),i+1))
         RTM_Pc%LW%dTbc_dPc(Thermal(j)) = (delta_Tbc(Thermal(j)) / delta_p) - &
              (k0 * d2Tbc_dP2(Thermal(j),i)) + (k1 * d2Tbc_dP2(Thermal(j),i+1))
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
          RTM_Pc%LW%dRac_up_dPc(Thermal(j)) = &
              (delta_Rac_up(Thermal(j)) / delta_p) - &
              (k0 * d2Rac_up_dP2(Thermal(j),i)) + &
              (k1 * d2Rac_up_dP2(Thermal(j),i+1))
          RTM_Pc%LW%dRac_dwn_dPc(Thermal(j)) = &
              (delta_Rac_dwn(Thermal(j)) / delta_p) - &
              (k0 * d2Rac_dwn_dP2(Thermal(j),i)) + &
              (k1 * d2Rac_dwn_dP2(Thermal(j),i+1))
          RTM_Pc%LW%dRbc_up_dPc(Thermal(j)) = &
              (delta_Rbc_up(Thermal(j)) / delta_p) - &
              (k0 * d2Rbc_up_dP2(Thermal(j),i)) + &
              (k1 * d2Rbc_up_dP2(Thermal(j),i+1))
      end do

      ! Change in temperature between RTM levels i and i+1
      delta_T = SPixel%RTM%LW%T(i+1) - SPixel%RTM%LW%T(i)

      ! Change in GPH between RTM levels i and i+1
      delta_H = SPixel%RTM%LW%H(i+1) - SPixel%RTM%LW%H(i)

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

      delta_Pc = Pc - SPixel%RTM%LW%P(i) ! Diff. between Pc and lower RTM level

      ! Diff. in trans from gradient
      do j = 1,SPixel%Ind%NThermal
         RTM_Pc%LW%Tac(Thermal(j)) = (dP * SPixel%RTM%LW%Tac(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Tac(Thermal(j),i+1)) + &
              (k0 * d2Tac_dP2(Thermal(j),i)) + (k1 * d2Tac_dP2(Thermal(j),i+1))

      ! Diff. in trans from gradient
         RTM_Pc%LW%Tbc(Thermal(j)) = (dP * SPixel%RTM%LW%Tbc(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Tbc(Thermal(j),i+1)) + &
              (k0 * d2Tbc_dP2(Thermal(j),i)) + (k1 * d2Tbc_dP2(Thermal(j),i+1))

      ! Interpolated radiances
         RTM_Pc%LW%Rac_up(Thermal(j)) = (dP * SPixel%RTM%LW%Rac_up(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Rac_up(Thermal(j),i+1)) + &
              (k0 * d2Rac_up_dP2(Thermal(j),i)) + (k1 * d2Rac_up_dP2(Thermal(j),i+1))

         RTM_Pc%LW%Rac_dwn(Thermal(j)) = (dP * SPixel%RTM%LW%Rac_dwn(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Rac_dwn(Thermal(j),i+1)) + &
              (k0 * d2Rac_dwn_dP2(Thermal(j),i)) + (k1 * d2Rac_dwn_dP2(Thermal(j),i+1))

         RTM_Pc%LW%Rbc_up(Thermal(j))  = (dP * SPixel%RTM%LW%Rbc_up(Thermal(j),i)) + &
              (p1 * SPixel%RTM%LW%Rbc_up(Thermal(j),i+1)) + &
              (k0 * d2Rbc_up_dP2(Thermal(j),i)) + (k1 * d2Rbc_up_dP2(Thermal(j),i+1))
      end do

      ! Interpolated Planck functions: calculate T and convert to B using T2R
      ! delta_T here is the step from the next lowest level to the current Pc,
      ! unlike above where it is the step between RTM levels.
      delta_T = delta_Pc * dT_dPc

      ! Set up GPH
      delta_H = delta_Pc * dH_dPc

      ! Set current temperature RTM_Pc%Tc and calculate equivalent radiance.
      ! (T2R requires T to be an array).
      RTM_Pc%Tc = (dP * SPixel%RTM%LW%T(i)) + &
                  (p1 * SPixel%RTM%LW%T(i+1)) + &
                  (k0 * d2T_dP2(i)) + (k1 * d2T_dP2(i+1))
      T = RTM_Pc%Tc

      call T2R(SPixel%Ind%Nthermal, SAD_Chan, T, R, dB_dT, status)
      RTM_Pc%LW%B(Thermal) = R
      RTM_Pc%LW%dB_dPc(Thermal) = dT_dPc * dB_dT

      ! Set current GPH and save the rate of change w.r.t. Pc for use later
      RTM_Pc%Hc = (dP * SPixel%RTM%LW%H(i)) + &
                  (p1 * SPixel%RTM%LW%H(i+1)) + &
                  (k0 * d2H_dP2(i)) + (k1 * d2H_dP2(i+1))
      H = RTM_Pc%Hc
      RTM_Pc%dHc_dPc = dH_dPc
   end if

   ! Open breakpoint file if required, and write our reflectances and gradients.

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_Interpol_Thermal) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: Interpol_Thermal_spline(): Error opening breakpoint file'
         stop BkpFileOpenErr
      else
         write(bkp_lun,'(/,a,/)')'Interpol_Thermal_spline:'
      end if

      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,5(a,f9.4))') 'Channel index: ', i, &
            ' Tac: ', RTM_Pc%LW%Tac(Thermal(i)), &
            ' Tbc: ', RTM_Pc%LW%Tbc(Thermal(i)), &
            ' Rac up: ', RTM_Pc%LW%Rac_up(Thermal(i)), &
            ' Rac dwn: ', RTM_Pc%LW%Rac_dwn(Thermal(i)),&
            ' Rbc up: ', RTM_Pc%LW%Rbc_up(Thermal(i))
      end do

      write(bkp_lun, '(a,/)') 'Interpol_Thermal_spline: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine Interpol_Thermal_spline
