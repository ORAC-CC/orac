!-------------------------------------------------------------------------------
! Name:
!    Interpol_Thermal
!
! Purpose:
!    Interpolates LW transmittances and radiances to the cloud pressure level.
!
! Arguments:
!    Name      Type        In/Out/Both Description
!    Ctrl      struct      In          Control structure
!    SPixel    struct      In          Super-pixel structure
!    Pc        float       In          Cloud pressure (from state vector X)
!    SAD_Chan  float array In          Channel characteristics
!    RTM_Pc    struct      Out         Contains Tac, Tbc (interpolated
!                                      transmittances above and below cloud) and
!                                      gradients wrt cloud pressure.
!    status    int         Out         Standard status value not set here
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    21st Nov 2000, Kevin M. Smith: Original version
!    14th Feb 2001, Kevin M. Smith: Corrected the position of the end of the
!       first main if block.
!    16th Feb 2001, Andy Smith:
!       Using RTM_Pc struct to pass Tac, Tbc etc.
!    20th Feb 2001, Andy Smith:
!       Converting calculations from B to T, since B is not available from the
!       SPixel RTM input. SAD_Chan is now required as an input argument.
!    26th Feb 2001, Andy Smith:
!       Changed test for P levels bounding Pc. Using
!       (Pc >= p(j) and Pc <  p(j+1)) instead of
!       (Pc >  p(j) and Pc <= p(j+1)) because P levels now go in order of
!       increasing pressure. Previously, if Pc was equal to P(1) it was not
!       matched (and if Pc < P(1) it's P index is set to 1!)
!     1st Mar 2001, Andy Smith:
!       Fixed dB_dT declaration. Was scalar, should have been array (NThermal).
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
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    12th Dec 2002, Caroline Poulsen:
!       Added in GPH follwed the example of temperature
!    23rd Dec 2002, Andy Smith:
!       Added height gradient dHc_dPC to RTM_Pc structure. Required later for
!       ascribing an error value to the height.
!     7th Feb 2012, C. Arnold:
!       Added intent() to argument declarations
!    17th Jan 2013, Matthias Jerg:
!       Adds code to extract RTM_Pc%dHc_dPc and RTM_Pc%dTc_dPc
!    15th Jan 2014, Greg McGarragh:
!       Deal with the case when Pc is equal to the pressure of the last level.
!    16th May 2014, Greg McGarragh:
!       Cleaned up the code.
!
! Bugs:
!    None known.
!
! $Id$
!
!------------------------------------------------------------------------------------

subroutine Interpol_Thermal(Ctrl, SPixel, Pc, SAD_Chan, RTM_Pc, status)

   use CTRL_def
   use RTM_Pc_def
   use SPixel_def
   use SAD_Chan_def

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(in)    :: SPixel
   real,             intent(in)    :: Pc
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(SPixel%Ind%Nthermal)
   type(RTM_Pc_t) ,  intent(inout) :: RTM_Pc
   integer,          intent(inout) :: status

   ! Define local variables

   integer :: i
   integer :: j
   integer :: ThF, ThL                           ! First, last thermal channel
                                                 ! indices for RTM_Pc%LW arrays
   real    :: delta_p                            ! Difference in pressure between
                                                 ! consecutive RTM levels
   real    :: delta_Tac(SPixel%Ind%Nthermal)     ! Difference in Tac between
                                                 ! consecutive RTM levels
   real    :: delta_Tbc(SPixel%Ind%Nthermal)     ! Difference in Tbc between
                                                 ! consecutive RTM levels
   real    :: delta_Rac_up(SPixel%Ind%Nthermal)  ! Difference in Rac_up between
                                                 ! consecutive RTM levels
   real    :: delta_Rac_dwn(SPixel%Ind%Nthermal) ! Difference in Rac_dwn between
                                                 ! consecutive RTM levels
   real    :: delta_Rbc_up(SPixel%Ind%Nthermal)  ! Difference in Rbc_up between
                                                 ! consecutive RTM levels
   real    :: delta_T                            ! Difference in temp between
                                                 ! consecutive RTM levels
   real    :: delta_H                            ! Difference in GPH between
                                                 ! consecutive RTM levels
   real    :: dT_dPc                             ! Change in Temp w.r.t. Pc
   real    :: dH_dPc                             ! Change in GPH w.r.t. Pc
   real    :: T(SPixel%Ind%Nthermal)             ! Temp at Pc
   real    :: H(SPixel%Ind%Nthermal)             ! GPH at Pc
   real    :: delta_Pc                           ! Difference in pressure between
                                                 ! Pc and lower RTM level
   real    :: delta_Tc(SPixel%Ind%Nthermal)      ! Difference in trans. between
                                                 ! Pc and lower RTM level
   real    :: delta_Rc(SPixel%Ind%Nthermal)      ! Difference in radiance between
                                                 ! Pc and lower RTM level
   real    :: dB_dT(SPixel%Ind%Nthermal)         ! Gradient of Planck function
                                                 ! w.r.t surface T
   character(ECPLogReclen) :: message            ! Warning or error message to
                                                 ! pass to Write_Log
#ifdef BKP
   integer   :: bkp_lun ! Unit number for breakpoint file
   integer   :: ios     ! I/O status for breakpoint file
#endif

   ! Set initial value of error status equal to zero (i.e. no error)
   status = 0

   ! Use ThF and ThL to access the first and last required thermal channels from
   ! RTM_Pc and SPixel %LW arrays, since these are always allocated to size
   ! Ctrl%Ind%NThermal, but not all thermal channels are used in all pixels
   ! hence SPixel%Ind%ThermalFirst may not equal Ctrl%Ind%ThermalFirst.

   ! The above is case in twilight condition for that given pixel where the
   ! mixed channels is excluded.
   ThF = 1 + SPixel%Ind%ThermalFirst - Ctrl%Ind%ThermalFirst
   ThL = Ctrl%Ind%NThermal

   ! Search for Pc in the LW RTM pressure levels. If Pc lies outwith the RTM
   ! pressure levels avoid search and set index to 1 or the penultimate RTM level.

   if (Pc > SPixel%RTM%LW%P(SPixel%RTM%LW%Np)) then
      ! When Pc above pressure at highest level in RTM
      i = SPixel%RTM%LW%Np-1
      if (abs(Pc-SPixel%RTM%LW%P(SPixel%RTM%LW%Np)) > 50.0) then
         ! When there is a difference of more than 50 hPa between Pc and RTM level
         write(unit=message, fmt=*) &
            'WARNING: Interpol_Thermal(), Extrapolation, high, P(1), P(Np), Pc: ', &
            SPixel%RTM%LW%P(1), SPixel%RTM%LW%P(SPixel%RTM%LW%Np), Pc
         call Write_Log(Ctrl, trim(message), status) ! Write to log
      end if
   else if (Pc < SPixel%RTM%LW%P(1)) then
      ! When Pc below lowest in RTM
      i = 1
      if (abs(Pc-SPixel%RTM%LW%P(1)) > 50.0) then
         ! When there is a difference of more than 50 hPa between Pc and RTM level
         write(unit=message, fmt=*) &
            'WARNING: Interpol_Thermal(), Extrapolation, low, P(1), P(Np), Pc: ', &
            SPixel%RTM%LW%P(1), SPixel%RTM%LW%P(SPixel%RTM%LW%Np), Pc
         call Write_Log(Ctrl, trim(message), status) ! Write to log
      end if
   else if (Pc == SPixel%RTM%LW%P(SPixel%RTM%LW%Np)) then
      i = SPixel%RTM%LW%Np-1
   else
      ! Search through RTM levels sequentially to find those bounding Pc
      do j = 1, SPixel%RTM%LW%Np-1
         if (Pc >= SPixel%RTM%LW%P(j) .and. Pc < SPixel%RTM%LW%P(j+1)) then
            i = j ! Set index equal to the lower bounding RTM level
            status = 0
            exit
         end if
         status = 1 ! Bounding levels not found
      end do
   end if

   if (status /= 0) then
      ! If none of the above conditions are met (e.g. Pc = NaN) then return with
      ! a fatal error
      status = IntTransErr ! Set status to indicate failure of interpolation
      write(unit=message, fmt=*) 'ERROR: Interpol_Thermal(), Interpolation failure, ', &
         'SPixel starting at: ',SPixel%Loc%X0, SPixel%Loc%Y0, ', P(1), P(Np), Pc: ', &
         SPixel%RTM%LW%P(1), SPixel%RTM%LW%P(SPixel%RTM%LW%Np), Pc
      call Write_Log(Ctrl, trim(message), status) ! Write to log
!     stop
   else
      ! Start the interpolation or extrapolation calculations
      ! Note: Implicit looping over instrument channels from here onwards

      ! Change in pressure between RTM levels i and i+1
      ! (delta_p is negative for decreasing pressure with increasing i)
      delta_p = SPixel%RTM%LW%P(i+1) - SPixel%RTM%LW%P(i)

      ! Change in transmittances between RTM levels i and i+1
      ! (delta_Tac/bc are positive for increasing trans. with increasing i)
      delta_Tac = SPixel%RTM%LW%Tac(ThF:ThL,i+1) - SPixel%RTM%LW%Tac(ThF:ThL,i)
      delta_Tbc = SPixel%RTM%LW%Tbc(ThF:ThL,i+1) - SPixel%RTM%LW%Tbc(ThF:ThL,i)

      ! Gradients of transmittance w.r.t. pressure (around Pc)
      RTM_Pc%LW%dTac_dPc(ThF:ThL) = delta_Tac / delta_p
      RTM_Pc%LW%dTbc_dPc(ThF:ThL) = delta_Tbc / delta_p

      ! Change in radiances between RTM levels i and i+1
      delta_Rac_up  = SPixel%RTM%LW%Rac_up(ThF:ThL,i+1) - &
                      SPixel%RTM%LW%Rac_up(ThF:ThL,i)
      delta_Rac_dwn = SPixel%RTM%LW%Rac_dwn(ThF:ThL,i+1) - &
                      SPixel%RTM%LW%Rac_dwn(ThF:ThL,i)
      delta_Rbc_up  = SPixel%RTM%LW%Rbc_up(ThF:ThL,i+1) - &
                      SPixel%RTM%LW%Rbc_up(ThF:ThL,i)

      ! Gradients of radiances w.r.t. pressure (around Pc)
      RTM_Pc%LW%dRac_up_dPc(ThF:ThL)  = delta_Rac_up  / delta_p
      RTM_Pc%LW%dRac_dwn_dPc(ThF:ThL) = delta_Rac_dwn / delta_p
      RTM_Pc%LW%dRbc_up_dPc(ThF:ThL)  = delta_Rbc_up  / delta_p

      ! Change in temperature between RTM levels i and i+1
      delta_T = SPixel%RTM%LW%T(i+1) - SPixel%RTM%LW%T(i)

      ! Change in GPH between RTM levels i and i+1
      delta_H = SPixel%RTM%LW%H(i+1) - SPixel%RTM%LW%H(i)

      ! Gradient of Planck functions w.r.t. pressure (around Pc)
      dT_dPc = delta_T / delta_p
      RTM_Pc%dTc_dPc = dT_dPc

      ! Gradient of delta GPH w.r.t. pressure (around Pc)
      dH_dPc = delta_H / delta_p
      RTM_Pc%dHc_dPc = dH_dPc

      ! Interpolated transmittances
      ! (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
      ! levels then extrapolation takes place using the same equations as for
      ! interpolation. Note: The sign of delta_Pc will change for Pc greater than
      ! the pressure of the lowest altitude RTM pressure level)

      ! Diff. between Pc and lower RTM level
      delta_Pc = Pc - SPixel%RTM%LW%P(i)

      ! Diff. in trans. from gradient
      delta_Tc = delta_Pc * RTM_Pc%LW%dTac_dPc(ThF:ThL)
      ! Abs. above cloud trans.
      RTM_Pc%LW%Tac(ThF:ThL) = SPixel%RTM%LW%Tac(ThF:ThL,i) + delta_Tc

      ! Diff. in trans. from gradient
      delta_Tc = delta_Pc * RTM_Pc%LW%dTbc_dPc(ThF:ThL)
      ! Abs. below cloud trans.
      RTM_Pc%LW%Tbc(ThF:ThL) = SPixel%RTM%LW%Tbc(ThF:ThL,i) + delta_Tc

      ! Interpolated radiances
      delta_Rc = delta_Pc * RTM_Pc%LW%dRac_up_dPc(ThF:ThL)
      RTM_Pc%LW%Rac_up(ThF:ThL) = SPixel%RTM%LW%Rac_up(ThF:ThL,i) + delta_Rc

      delta_Rc = delta_Pc * RTM_Pc%LW%dRac_dwn_dPc(ThF:ThL)
      RTM_Pc%LW%Rac_dwn(ThF:ThL) = SPixel%RTM%LW%Rac_dwn(ThF:ThL,i) + delta_Rc

      delta_Rc = delta_Pc * RTM_Pc%LW%dRbc_up_dPc(ThF:ThL)
      RTM_Pc%LW%Rbc_up(ThF:ThL) = SPixel%RTM%LW%Rbc_up(ThF:ThL,i) + delta_Rc

      ! Interpolated Planck functions: calculate T and convert to B using T2R
      ! delta_T here is the step from the next lowest level to the current Pc,
      ! unlike above where it is the step between RTM levels.
      delta_T = delta_Pc * dT_dPc

      ! Set up GPH
      delta_H = delta_Pc * dH_dPc

      ! Set current temperature RTM_Pc%Tc and calculate equivalent radiance.
      ! (T2R requires T to be an array).
      RTM_Pc%Tc = SPixel%RTM%LW%T(i) + delta_T
      T = RTM_Pc%Tc

      call T2R(SPixel%Ind%Nthermal, SAD_Chan, T, RTM_Pc%LW%B(ThF:ThL), dB_dT, status)
      RTM_Pc%LW%dB_dPc(ThF:ThL) = dT_dPc * dB_dT

      ! Set current GPH and save the rate of change w.r.t. Pc for use later
      RTM_Pc%Hc = SPixel%RTM%LW%H(i) + delta_H
      H = RTM_Pc%Hc
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
         status = BkpFileOpenErr
         call Write_Log(Ctrl, 'Interpol_Thermal: Error opening breakpoint file', status)
      else
         write(bkp_lun,'(/,a,/)')'Interpol_Thermal:'
      end if

      do i=ThF,ThL
         write(bkp_lun,'(a,i2,5(a,f9.4))') 'Channel index: ', i, &
            ' Tac: ', RTM_Pc%LW%Tac(i), ' Tbc: ', RTM_Pc%LW%Tbc(i), &
            ' Rac up: ', RTM_Pc%LW%Rac_up(i), ' Rac dwn: ', RTM_Pc%LW%Rac_dwn(i),&
            ' Rbc up: ', RTM_Pc%LW%Rbc_up(i)
      end do

      write(bkp_lun, '(a,/)') 'Interpol_Thermal: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine Interpol_Thermal
