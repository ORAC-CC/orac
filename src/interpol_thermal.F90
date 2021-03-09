!-------------------------------------------------------------------------------
! Name: interpol_thermal.F90
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
! SAD_Chan float array In          Channel characteristics
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
!    Pc <  p(j+1)) instead of (Pc >  p(j) and Pc <= p(j+1)) because P levels
!    now go in order of increasing pressure. Previously, if Pc was equal to
!    P(1) it was not matched (and if Pc < P(1) it's P index is set to 1!)
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
!    **************** ECV work starts here *************************************
! 2011/02/21, AS: Re-introducing changes made in late 2001/2002.
! 2002/12/12, CP: Added in GPH followed the example of temperature
! 2002/12/23, AS: Added height gradient dHc_dPC to RTM_Pc structure. Required
!    later for ascribing an error value to the height.
! 2012/02/07, CA: Added intent() to argument declarations
! 2013/01/17, MJ: Adds code to extract RTM_Pc%dHc_dPc and RTM_Pc%dTc_dPc
! 2014/01/15, GM: Deal with the case when Pc is equal to the pressure of the
!    last level.
! 2014/05/16, GM: Cleaned up the code.
! 2014/08/05, GM: Put Interpol_* common code into subroutine find_Pc().
! 2015/01/07, AP: Use SPixel index arrays rather than ThF,ThL.
! 2015/05/07, CP: Removed the stop IntTransErr.
!
! Bugs:
! call find_Pc(Ctrl, SPixel%RTM%Np, SPixel%RTM%P, Pc, i, status) occasionally
! returns status 0 particually with ice retreivals
!-------------------------------------------------------------------------------

subroutine Interpol_Thermal(Ctrl, SPixel, Pc, SAD_Chan, RTM_Pc, status)

   use Ctrl_m
   use planck_m
   use RTM_Pc_m
   use SAD_Chan_m
   use SPixel_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(in)    :: SPixel
   real,             intent(in)    :: Pc
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(RTM_Pc_t) ,  intent(inout) :: RTM_Pc
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i
   integer :: Thermal(SPixel%Ind%NThermal)       ! Indices of thermal channel
                                                 ! for RTM_Pc%LW arrays
   real    :: delta_p                            ! Difference in pressure between
                                                 ! consecutive RTM levels
   real    :: delta_Tac(SPixel%Ind%NThermal)     ! Difference in Tac between
                                                 ! consecutive RTM levels
   real    :: delta_Tbc(SPixel%Ind%NThermal)     ! Difference in Tbc between
                                                 ! consecutive RTM levels
   real    :: delta_Rac_up(SPixel%Ind%NThermal)  ! Difference in Rac_up between
                                                 ! consecutive RTM levels
   real    :: delta_Rac_dwn(SPixel%Ind%NThermal) ! Difference in Rac_dwn between
                                                 ! consecutive RTM levels
   real    :: delta_Rbc_up(SPixel%Ind%NThermal)  ! Difference in Rbc_up between
                                                 ! consecutive RTM levels
   real    :: delta_T                            ! Difference in temp between
                                                 ! consecutive RTM levels
   real    :: delta_H                            ! Difference in GPH between
                                                 ! consecutive RTM levels
   real    :: T(SPixel%Ind%NThermal)             ! Temp at Pc
   real    :: delta_Pc                           ! Difference in pressure between
                                                 ! Pc and lower RTM level
   real    :: delta_Tc(SPixel%Ind%NThermal)      ! Difference in trans. between
                                                 ! Pc and lower RTM level
   real    :: delta_Rc(SPixel%Ind%NThermal)      ! Difference in radiance between
                                                 ! Pc and lower RTM level
   real    :: R(SPixel%Ind%NThermal)             ! Radiances
   real    :: dB_dT(SPixel%Ind%NThermal)         ! Gradient of Planck function
                                                 ! w.r.t surface T
   ! Set initial value of error status equal to zero (i.e. no error)
   status = 0

   ! Subscripts for thermal channels in RTM arrays
   Thermal = SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(:SPixel%Ind%NThermal)

   ! Search for Pc in the LW RTM pressure levels. If Pc lies outside the RTM
   ! pressure levels avoid search and set index to 1 or the penultimate RTM level
   call find_Pc(Ctrl, SPixel%RTM%Np, SPixel%RTM%P, Pc, i, status)

   if (status /= 0) then
      ! If none of the above conditions are met (e.g. Pc = NaN) then return with
      ! a fatal error
#ifdef DEBUG
      write(*, *) 'ERROR: Interpol_Thermal(): Interpolation failure, SPixel ' // &
         'starting at: ',SPixel%Loc%X0, SPixel%Loc%Y0, ', P(1), P(Np), Pc: ', &
         SPixel%RTM%P(1), SPixel%RTM%P(SPixel%RTM%Np), Pc
#endif
      status = IntTransErr
   else
      ! Start the interpolation or extrapolation calculations
      ! Note: Implicit looping over instrument channels from here onwards

      ! Change in pressure between RTM levels i and i+1
      ! (delta_p is negative for decreasing pressure with increasing i)
      delta_p = SPixel%RTM%P(i+1) - SPixel%RTM%P(i)

      ! Change in transmittances between RTM levels i and i+1
      ! (delta_Tac/bc are positive for increasing trans. with increasing i)
      delta_Tac = SPixel%RTM%LW%Tac(Thermal,i+1) - SPixel%RTM%LW%Tac(Thermal,i)
      delta_Tbc = SPixel%RTM%LW%Tbc(Thermal,i+1) - SPixel%RTM%LW%Tbc(Thermal,i)

      ! Gradients of transmittance w.r.t. pressure (around Pc)
      RTM_Pc%LW%dTac_dPc(Thermal) = delta_Tac / delta_p
      RTM_Pc%LW%dTbc_dPc(Thermal) = delta_Tbc / delta_p

      ! Change in radiances between RTM levels i and i+1
      delta_Rac_up  = SPixel%RTM%LW%Rac_up(Thermal,i+1) - &
                      SPixel%RTM%LW%Rac_up(Thermal,i)
      delta_Rac_dwn = SPixel%RTM%LW%Rac_dwn(Thermal,i+1) - &
                      SPixel%RTM%LW%Rac_dwn(Thermal,i)
      delta_Rbc_up  = SPixel%RTM%LW%Rbc_up(Thermal,i+1) - &
                      SPixel%RTM%LW%Rbc_up(Thermal,i)

      ! Gradients of radiances w.r.t. pressure (around Pc)
      RTM_Pc%LW%dRac_up_dPc(Thermal)  = delta_Rac_up  / delta_p
      RTM_Pc%LW%dRac_dwn_dPc(Thermal) = delta_Rac_dwn / delta_p
      RTM_Pc%LW%dRbc_up_dPc(Thermal)  = delta_Rbc_up  / delta_p

      ! Change in temperature between RTM levels i and i+1
      delta_T = SPixel%RTM%T(i+1) - SPixel%RTM%T(i)

      ! Gradient of Planck functions w.r.t. pressure (around Pc)
      RTM_Pc%dTc_dPc = delta_T / delta_p

      ! Change in GPH between RTM levels i and i+1
      delta_H = SPixel%RTM%H(i+1) - SPixel%RTM%H(i)

      ! Gradient of delta GPH w.r.t. pressure (around Pc)
      RTM_Pc%dHc_dPc = delta_H / delta_p

      ! Interpolated transmittances
      ! (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
      ! levels then extrapolation takes place using the same equations as for
      ! interpolation. Note: The sign of delta_Pc will change for Pc greater than
      ! the pressure of the lowest altitude RTM pressure level)

      ! Diff. between Pc and lower RTM level
      delta_Pc = Pc - SPixel%RTM%P(i)

      ! Diff. in trans. from gradient
      delta_Tc = delta_Pc * RTM_Pc%LW%dTac_dPc(Thermal)
      ! Abs. above cloud trans.
      RTM_Pc%LW%Tac(Thermal) = SPixel%RTM%LW%Tac(Thermal,i) + delta_Tc

      ! Diff. in trans. from gradient
      delta_Tc = delta_Pc * RTM_Pc%LW%dTbc_dPc(Thermal)
      ! Abs. below cloud trans.
      RTM_Pc%LW%Tbc(Thermal) = SPixel%RTM%LW%Tbc(Thermal,i) + delta_Tc

      ! Interpolated radiances
      delta_Rc = delta_Pc * RTM_Pc%LW%dRac_up_dPc(Thermal)
      RTM_Pc%LW%Rac_up(Thermal) = SPixel%RTM%LW%Rac_up(Thermal,i) + delta_Rc

      delta_Rc = delta_Pc * RTM_Pc%LW%dRac_dwn_dPc(Thermal)
      RTM_Pc%LW%Rac_dwn(Thermal) = SPixel%RTM%LW%Rac_dwn(Thermal,i) + delta_Rc

      delta_Rc = delta_Pc * RTM_Pc%LW%dRbc_up_dPc(Thermal)
      RTM_Pc%LW%Rbc_up(Thermal) = SPixel%RTM%LW%Rbc_up(Thermal,i) + delta_Rc

      ! Set current temperature RTM_Pc%Tc and calculate equivalent radiance.
      ! (T2R requires T to be an array).
      delta_T = delta_Pc * RTM_Pc%dTc_dPc
      RTM_Pc%Tc = SPixel%RTM%T(i) + delta_T
      T = RTM_Pc%Tc

      ! Interpolated Planck functions: calculate T and convert to B using T2R
      ! delta_T here is the step from the next lowest level to the current Pc,
      ! unlike above where it is the step between RTM levels.
      call T2R(SPixel%Ind%NThermal, SAD_Chan, T, R, dB_dT, status)
      RTM_Pc%LW%B(Thermal) = R
      RTM_Pc%LW%dB_dPc(Thermal) = RTM_Pc%dTc_dPc * dB_dT

      ! Set current GPH
      delta_H = delta_Pc * RTM_Pc%dHc_dPc
      RTM_Pc%Hc = SPixel%RTM%H(i) + delta_H
   end if

end subroutine Interpol_Thermal
