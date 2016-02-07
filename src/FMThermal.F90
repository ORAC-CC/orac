!-------------------------------------------------------------------------------
! Name: FMThermal.F90
!
! Purpose:
! Radiance forward model (thermal channels) for a defined pixel and pressure
! level.
!
! Description and Algorithm details:
! Update clear radiances at each RTM pressure level (Ts = Ts_0 + delta_Ts).
! Set up long-wave cloud properties.
! Update the below cloud upward radiance (this can be done at Pc only to save
!    time)
! Calculate overcast and part-cloudy radiances.
! Calculate part-cloudy radiance gradients w.r.t. each of the state variables.
! Convert the radiances and radiance gradients to brightness temperatures.
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct In          Control structure
! SAD_LUT  struct In          SAD look up table
! SPixel   struct Both        Super-pixel structure
! SAD_Chan array of structs   SAD channel structures: only the
!                 In          thermal channel part of the array is passed in.
! RTM_Pc   struct In          Contains transmittances, radiances
!                             and their derivatives
! X        real array In      State vector
! GZero
! BT       real array Out     Part cloudy brightness temperatures
! d_BT     real array Out     Gradients in part cloudy brightness temps.
! R        real array Out     Part cloudy radiance calculated at Pc
! d_R      real array Out     Part cloudy radiance gradients calculated at Pc.
! status   int    Out         Error status
!
! History:
! 2000/11/16, KS: original version
! 2000/11/21, KS: corrections
! 2000/11/24, KS: brought calculation of d_T up from R2T
! 2001/01/19, AS: Use FM_Routines_def: contains interface definition for
!    SetCRPThermal
! 2001/02/09, AS: Updating to match recent changes in arguments and change in
!    call sequence. Using constants to select parts of BT and d_BT arrays
!    referring to different state variables.
! 2001/02/20, AS: CRP, d_CRP are required as arguments. Passed to solar routine
!    later to populate the remaining channels.
! 2001/03/01, AS: delta_Ts made local.
! 2001/03/08, AS: dB_dTs argument removed. Now part of SPixel.
! 2001/03/15, AS: Added ThF and ThL indices for RTM_Pc%LW arrays. Required
!    because these arrays are allocated to match the no. of thermal channels
!    requested, but in twilight not all of the requested thermal channels may
!    be used.
! 2001/03/22, AS: Replaced pressure level index in RTM Tac value in clear
!    radiance calculation. Was Np, now 1.
! 2001/07/20, AS: Fixed setting of ThL. Last thermal channel should always be
!    the one specified in Ctrl, not SPixel, since the RTM_Pc and RTM arrays are
!    of fixed size for the whole run, i.e are not re-allocated to match the no.
!    of thermal channels used in each SPixel, and it is the lower numbered
!    thermal channels that are not used in certain conditions.
! 2014/12/20, GM: Cleaned up code.
! 2014/12/24, GM: Some intent changes.
! 2015/01/09, AP: Replacing ThF:ThL with SPixel index array. Eliminate
!    RTM_Pc%Tac, Tbc.
! 2015/01/12, AP: Remove CRP arguments.
! 2015/01/21, GM: Fixed some cases were the required 'Thermal' subscripts were
!    missing.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine FM_Thermal(Ctrl, SAD_LUT, SPixel, SAD_Chan, RTM_Pc, X, GZero, &
                      BT, d_BT, R, d_R, status)

   use CTRL_def
   use ECP_Constants
   use GZero_def
   use planck
   use RTM_Pc_def
   use SAD_Chan_def
   use SAD_LUT_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT
   type(SPixel_t),   intent(in)    :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc
   real,             intent(in)    :: X(:)
   type(GZero_t),    intent(in)    :: GZero
   real,             intent(out)   :: BT(:)
   real,             intent(out)   :: d_BT(:,:)
   real,             intent(out)   :: R(:)
   real,             intent(out)   :: d_R(:,:)
   integer,          intent(out)   :: status

   ! Define local variables

   real    :: CRP(SPixel%Ind%NThermal, MaxCRProps)
   real    :: d_CRP(SPixel%Ind%NThermal, MaxCRProps, 2)
   integer :: i
   integer :: Thermal(SPixel%Ind%NThermal)
   real    :: delta_Ts
   real    :: R_clear(SPixel%Ind%NThermal)
   real    :: R_over(SPixel%Ind%NThermal)
   real    :: Es_dB_dTs(SPixel%Ind%NThermal)
   real    :: fTac(SPixel%Ind%NThermal)
   real    :: dT_dR(SPixel%Ind%NThermal)
#ifdef BKP
   integer :: j       ! For breakpoint output loops
   integer :: bkp_lun ! Unit number for breakpoint file
   integer :: ios     ! I/O status for breakpoint file
#endif
   status = 0
   d_R = 0

   ! Subscripts for thermal channels in RTM arrays
   Thermal = SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(:SPixel%Ind%NThermal)

   ! Set up the LW cloud radiative properties
   call Set_CRP_Thermal(Ctrl, SPixel%Ind, &
        SPixel%spixel_y_thermal_to_ctrl_y_index, &
        GZero, SAD_LUT, CRP, d_CRP, status)

   ! Calculate delta_Ts
   delta_Ts = X(ITs) - SPixel%RTM%T(SPixel%RTM%Np)

   ! Calculate product dB_dTs * SPixel%RTM%LW%Ems (for efficiency)
   Es_dB_dTs = SPixel%RTM%LW%dB_dTs(Thermal) * SPixel%RTM%LW%Ems(Thermal)

   ! Update clear radiances at each RTM pressure level
   R_clear = SPixel%RTM%LW%R_clear(Thermal) + &
      (delta_Ts * Es_dB_dTs * SPixel%RTM%LW%Tac(Thermal,1))

   ! Update below cloud radiance after interpolation to Pc
   RTM_Pc%LW%Rbc_up(Thermal) = RTM_Pc%LW%Rbc_up(Thermal) + &
      (delta_Ts * Es_dB_dTs * RTM_Pc%LW%Tbc(Thermal))

   ! Calculate overcast radiances at cloud pressure level
   R_over = (RTM_Pc%LW%Rbc_up(Thermal)  * CRP(:,IT_dv)  + &
             RTM_Pc%LW%B(Thermal)       * CRP(:,IEm)  + &
             RTM_Pc%LW%Rac_dwn(Thermal) * CRP(:,IR_dv)) * &
             RTM_Pc%LW%Tac(Thermal) + RTM_Pc%LW%Rac_up(Thermal)

   ! Calculate part cloudy radiances (a linear combination of R_clear and R_over)
   R = (X(IFr) * R_over) + ((1.0 - X(IFr)) * R_clear)

   ! Calculate product X%frac*Tac (for efficiency)
   fTac = X(IFr) * RTM_Pc%LW%Tac(Thermal)

   ! Calculate radiance gradients

   ! Gradient w.r.t. cloud optical depth, tau
   d_R(:,ITau) = fTac * (RTM_Pc%LW%Rbc_up(Thermal)  * d_CRP(:,IT_dv,ITau) + &
                         RTM_Pc%LW%B(Thermal)       * d_CRP(:,IEm,ITau) + &
                         RTM_Pc%LW%Rac_dwn(Thermal) * d_CRP(:,IR_dv,ITau))

   ! Gradient w.r.t. effective radius, re
   d_R(:,IRe) = fTac * (RTM_Pc%LW%Rbc_up(Thermal)  * d_CRP(:,IT_dv,IRe) + &
                        RTM_Pc%LW%B(Thermal)       * d_CRP(:,IEm,IRe) + &
                        RTM_Pc%LW%Rac_dwn(Thermal) * d_CRP(:,IR_dv,IRe))

   ! Gradient w.r.t. cloud pressure, Pc
   d_R(:,IPc) = X(IFr) * (RTM_Pc%LW%dTac_dPc(Thermal) * &
                          (RTM_Pc%LW%Rbc_up(Thermal)  * CRP(:,IT_dv) + &
                           RTM_Pc%LW%B(Thermal)       * CRP(:,IEm) + &
                           RTM_Pc%LW%Rac_dwn(Thermal) * CRP(:,IR_dv)) + &
                          RTM_Pc%LW%dRac_up_dPc(Thermal)) &
              + fTac * (RTM_Pc%LW%dRbc_up_dPc(Thermal)  * CRP(:,IT_dv) + &
                        RTM_Pc%LW%dB_dPc(Thermal)       * CRP(:,IEm) + &
                        RTM_Pc%LW%dRac_dwn_dPc(Thermal) * CRP(:,IR_dv))

   ! Gradient w.r.t. cloud fraction, f
   d_R(:,IFr) = R_over - R_clear

   ! Gradient w.r.t. surface temperature, Ts
   d_R(:,ITs) = fTac * Es_dB_dTs * RTM_Pc%LW%Tbc(Thermal) * CRP(:,IT_dv) + &
      (1.0 - X(IFr)) * Es_dB_dTs * SPixel%RTM%LW%Tac(Thermal,1)

   ! Convert radiances to brightness temperatures
   call R2T(SPixel%Ind%NThermal, SAD_Chan, R, BT, dT_dR, status)

   ! Calculate the change in brightness temperatures w.r.t. state parameters
   ! using dBT_dX = dT_dR * dR_dX (loop through each state parameter)
   do i = 1, MaxStateVar
      d_BT(:,i) = dT_dR * d_R(:,i)
   end do

   ! Open breakpoint file if required, and write our reflectances and gradients.
#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_FM_Thermal) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: FM_Thermal(): Error opening breakpoint file'
         stop BkpFileOpenErr
      else
         write(bkp_lun,'(/,a)')'FM_Thermal:'
      end if
      write(bkp_lun,'(/)')

      write(bkp_lun,'(a,f9.4)') 'delta_Ts: ',delta_Ts
      write(bkp_lun,'(a)') 'SPixel Lw RTM contributions to R_Clear:'
      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,3(a,f9.4))') 'Channel index: ', i, &
              ' R_Clear: ', SPixel%RTM%LW%R_Clear(Thermal(i)), &
              ' dB_dTs : ', SPixel%RTM%LW%dB_dTs(Thermal(i)), &
              ' Ems: ', SPixel%RTM%LW%Ems(Thermal(i))
      end do

      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,3(a,f9.4))') 'Channel index: ', i, &
              ' CRP Td: ', CRP(i,IT_dv), '  CRP Em: ', CRP(i,IEm), &
              '  CRP Rd: ', CRP(i,IR_dv)
      end do

      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,3(a,f9.4))') 'Channel index: ', i, &
              ' R_Clear: ', R_Clear(i), '  Rbc_up: ', RTM_Pc%LW%Rbc_up(Thermal(i)), &
              '  R_Over: ',R_Over(i)
      end do
      write(bkp_lun,'(/)')

      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,a,2f9.4)') 'Channel index: ', i, &
              ' BT, R: ', BT(i), R(i)
      end do
      write(bkp_lun,'(/)')

      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,a,5f10.3)') 'Channel index: ', i, &
              ' dBT: ', (d_BT(i,j),j=1,MaxStateVar)
         write(bkp_lun,'(a,5f10.3)') &
              '                   dR: ',  (d_R(i,j),j=1,MaxStateVar)
      end do
      write(bkp_lun,'(/)')

      write(bkp_lun, '(a,/)') 'FM_Thermal: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine FM_Thermal
