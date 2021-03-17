!-------------------------------------------------------------------------------
! Name: fm_thermal.F90
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
! Name     Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct           In    Control structure
! SAD_LUT  struct           In    SAD look up table
! SPixel   struct           Both  Super-pixel structure
! SAD_Chan array of structs In    SAD channel structures. Only the thermal
!                                 channel part of the array is passed in.
! RTM_Pc   array of structs In    Contains transmittances, radiances and their
!                                 derivatives for each layer
! X        real array       In    State vector
! GZero    array of structs In    "Zero'th point" grid info for SAD_LUT CRP
!                                 array interpolation for each layer
! BT       real array       Out   Part cloudy brightness temperatures
! d_BT     real array       Out   Gradients in part cloudy brightness temps.
! R        real array       Out   TOA partly cloudy radiances
! d_R      real array       Out   TOA partly cloudy radiance gradients
! status   int              Out   Error status
!
! History:
! 2000/11/16, KS: original version
! 2000/11/21, KS: corrections
! 2000/11/24, KS: brought calculation of d_T up from R2T
! 2001/01/19, AS: Use FM_Routines_m: contains interface definition for
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
! 2015/01/21, GM: Fixed some cases where the required 'Thermal' subscripts were
!    missing.
! 2016/07/22, GM: Extensive changes for multilayer support.
! 2016/12/08, GM: Computation of thermal clear-sky radiance and its associated
!    derivative wrt surface temperature was using the top level to TOA
!    transmittance when it should have been using the surface (bottom level) to
!    TOA transmittance.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine derivative_wrt_crp_parameter_layer_1(i_p_crp, Thermal, SPixel, RTM_Pc, &
   CRP, d_CRP, d_CRP2, CRP2, f, Tmc, Rmc_up, Rmc_dwn, d_R)

   use RTM_Pc_m
   use SPixel_m

   implicit none

   integer,        intent(in)  :: i_p_crp
   integer,        intent(in)  :: Thermal(:)
   type(SPixel_t), intent(in)  :: SPixel
   type(RTM_Pc_t), intent(in)  :: RTM_Pc(:)
   real,           intent(in)  :: CRP(:, :)
   real,           intent(in)  :: CRP2(:, :)
   real,           intent(in)  :: d_CRP(:, :, :)
   real,           intent(in)  :: d_CRP2(:, :, :)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tmc(:)
   real,           intent(in)  :: Rmc_up(:)
   real,           intent(in)  :: Rmc_dwn(:)
   real,           intent(out) :: d_R(:)

   real :: a(SPixel%Ind%NThermal)
   real :: b(SPixel%Ind%NThermal)
   real :: c(SPixel%Ind%NThermal)

   a = (((RTM_Pc(2)%LW%Rbc_up(Thermal) * CRP2(:,IT_dv) + &
          RTM_Pc(2)%LW%B(Thermal) * CRP2(:,IEm)) * Tmc + &
         Rmc_up) * d_CRP(:,IT_dv,i_p_crp) + &
        RTM_Pc(1)%LW%B(Thermal) * d_CRP(:,IEm,i_p_crp) &
       ) * RTM_Pc(1)%LW%Tac(Thermal)

   b = Tmc * RTM_Pc(1)%LW%Tac(Thermal) * CRP2(:,IR_dv) * &
       (d_CRP(:,IT_dv,i_p_crp) * &
          (Rmc_dwn + (RTM_Pc(1)%LW%B(Thermal) * CRP(:,IEm) + &
                      RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IT_dv)) * Tmc) + &
        CRP(:,IT_dv) * &
                    ((RTM_Pc(1)%LW%B(Thermal) * d_CRP(:,IEm, i_p_crp) + &
                      RTM_Pc(1)%LW%Rac_dwn(Thermal) * d_CRP(:,IT_dv,i_p_crp)) * Tmc))

   c = RTM_Pc(1)%LW%Rac_dwn(Thermal) * d_CRP(:,IR_dv,i_p_crp) * RTM_Pc(1)%LW%Tac(Thermal)

   d_R = f * (a + b + c)

end subroutine derivative_wrt_crp_parameter_layer_1


subroutine derivative_wrt_crp_parameter_layer_2(i_p_crp, Thermal, SPixel, RTM_Pc, &
   CRP, d_CRP, d_CRP2, CRP2, f, Tmc, Rmc_up, Rmc_dwn, d_R)

   use RTM_Pc_m
   use SPixel_m

   implicit none

   integer,        intent(in)  :: i_p_crp
   integer,        intent(in)  :: Thermal(:)
   type(SPixel_t), intent(in)  :: SPixel
   type(RTM_Pc_t), intent(in)  :: RTM_Pc(:)
   real,           intent(in)  :: CRP(:, :)
   real,           intent(in)  :: CRP2(:, :)
   real,           intent(in)  :: d_CRP(:, :, :)
   real,           intent(in)  :: d_CRP2(:, :, :)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tmc(:)
   real,           intent(in)  :: Rmc_up(:)
   real,           intent(in)  :: Rmc_dwn(:)
   real,           intent(out) :: d_R(:)

   real :: a(SPixel%Ind%NThermal)
   real :: b(SPixel%Ind%NThermal)
   real :: c(SPixel%Ind%NThermal)

   a = (RTM_Pc(2)%LW%Rbc_up(Thermal) * d_CRP2(:,IT_dv,i_p_crp) + &
        RTM_Pc(2)%LW%B(Thermal) * d_CRP2(:,IEm,i_p_crp)) * &
       Tmc * CRP(:,IT_dv) * RTM_Pc(1)%LW%Tac(Thermal)

   b = Tmc * RTM_Pc(1)%LW%Tac(Thermal) * CRP(:,IT_dv) * d_CRP2(:,IR_dv,i_p_crp) * &
       (Rmc_dwn + (RTM_Pc(1)%LW%B(Thermal) * CRP(:,IEm) + &
        RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IT_dv)) * Tmc)

   c = 0.

   d_R = f * (a + b + c)

end subroutine derivative_wrt_crp_parameter_layer_2


subroutine FM_Thermal(Ctrl, SAD_LUT, SPixel, SAD_Chan, RTM_Pc, X, GZero, BT, &
                      d_BT, R, d_R, status)

   use CTRL_m
   use GZero_m
   use Int_LUT_Routines_m
   use ORAC_Constants_m
   use planck_m
   use RTM_Pc_m
   use SAD_Chan_m
   use SAD_LUT_m
   use SPixel_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT(:)
   type(SPixel_t),   intent(in)    :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc(:)
   real,             intent(in)    :: X(:)
   type(GZero_t),    intent(in)    :: GZero(:)
   real,             intent(out)   :: BT(:)
   real,             intent(out)   :: d_BT(:,:)
   real,             intent(out)   :: R(:)
   real,             intent(out)   :: d_R(:,:)
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i
   integer :: Thermal(SPixel%Ind%NThermal)
   real    :: delta_Ts
   real    :: f2
   real    :: a(SPixel%Ind%NThermal)
   real    :: b(SPixel%Ind%NThermal)
   real    :: c(SPixel%Ind%NThermal)
   real    :: CRP(SPixel%Ind%NThermal, MaxCRProps)
   real    :: CRP2(SPixel%Ind%NThermal, MaxCRProps)
   real    :: d_CRP(SPixel%Ind%NThermal, MaxCRProps, MaxCRPParams)
   real    :: d_CRP2(SPixel%Ind%NThermal, MaxCRProps, MaxCRPParams)
   real    :: Es_dB_dTs(SPixel%Ind%NThermal)
   real    :: R_clear(SPixel%Ind%NThermal)
   real    :: R_over(SPixel%Ind%NThermal)
   real    :: fTac(SPixel%Ind%NThermal)
   real    :: dT_dR(SPixel%Ind%NThermal)
   real    :: Tmc(SPixel%Ind%NThermal)
   real    :: Rmc_up(SPixel%Ind%NThermal)
   real    :: Rmc_dwn(SPixel%Ind%NThermal)
   real    :: Tmc_l(SPixel%Ind%NThermal)
   real    :: Rmc_up_l(SPixel%Ind%NThermal)
   real    :: Rmc_dwn_l(SPixel%Ind%NThermal)

   status = 0
   d_R    = 0

   ! Subscripts for thermal channels in RTM arrays
   Thermal = SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(:SPixel%Ind%NThermal)

   ! Set up the LW cloud radiative properties
   ! Layer 1
   call Set_CRP_Thermal(Ctrl, SPixel%Ind, &
        SPixel%spixel_y_thermal_to_ctrl_y_index, &
        GZero(1), SAD_LUT(1), CRP, d_CRP, status)
   if (Ctrl%Approach == AppCld2L) then
      ! Layer 2
      call Set_CRP_Thermal(Ctrl, SPixel%Ind, &
           SPixel%spixel_y_thermal_to_ctrl_y_index, &
           GZero(2), SAD_LUT(2), CRP2, d_CRP2, status)
   end if

   ! Calculate delta_Ts
   delta_Ts = X(ITs) - SPixel%RTM%T(SPixel%RTM%Np)

   ! Calculate product dB_dTs * SPixel%RTM%LW%Ems (for efficiency)
   Es_dB_dTs = SPixel%RTM%LW%dB_dTs(Thermal) * SPixel%RTM%LW%Ems(Thermal)

   ! Update clear radiances at each RTM pressure level
   R_clear = SPixel%RTM%LW%R_clear(Thermal) + &
      (delta_Ts * Es_dB_dTs * SPixel%RTM%LW%Tsf(Thermal))

   ! Update below cloud radiance after interpolation to Pc
   RTM_Pc(1)%LW%Rbc_up(Thermal) = RTM_Pc(1)%LW%Rbc_up(Thermal) + &
      (delta_Ts * Es_dB_dTs * RTM_Pc(1)%LW%Tbc(Thermal))
if (Ctrl%Approach .ne. AppCld2L) then
   ! Calculate overcast radiances at TOA
   R_over = (RTM_Pc(1)%LW%Rbc_up(Thermal)  * CRP(:,IT_dv)  + &
             RTM_Pc(1)%LW%B(Thermal)       * CRP(:,IEm)  + &
             RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IR_dv)) * &
             RTM_Pc(1)%LW%Tac(Thermal) + RTM_Pc(1)%LW%Rac_up(Thermal)

   ! Calculate partly cloudy radiances at TOA (a linear combination of R_clear
   ! and R_over)
   R = X(IFr) * R_over + (1.0 - X(IFr)) * R_clear

   ! Calculate product X%frac*Tac (for efficiency)
   fTac = X(IFr) * RTM_Pc(1)%LW%Tac(Thermal)

   ! Calculate radiance gradients

   ! Gradient w.r.t. cloud optical depth, tau
   d_R(:,ITau) = fTac * (RTM_Pc(1)%LW%Rbc_up(Thermal)  * d_CRP(:,IT_dv,ITau) + &
                         RTM_Pc(1)%LW%B(Thermal)       * d_CRP(:,IEm,ITau) + &
                         RTM_Pc(1)%LW%Rac_dwn(Thermal) * d_CRP(:,IR_dv,ITau))

   ! Gradient w.r.t. effective radius, re
   d_R(:,IRe) = fTac * (RTM_Pc(1)%LW%Rbc_up(Thermal)  * d_CRP(:,IT_dv,IRe) + &
                        RTM_Pc(1)%LW%B(Thermal)       * d_CRP(:,IEm,IRe) + &
                        RTM_Pc(1)%LW%Rac_dwn(Thermal) * d_CRP(:,IR_dv,IRe))

   ! Gradient w.r.t. cloud pressure, Pc
   d_R(:,IPc) = X(IFr) * (RTM_Pc(1)%LW%dTac_dPc(Thermal) * &
                          (RTM_Pc(1)%LW%Rbc_up(Thermal)  * CRP(:,IT_dv) + &
                           RTM_Pc(1)%LW%B(Thermal)       * CRP(:,IEm) + &
                           RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IR_dv)) + &
                          RTM_Pc(1)%LW%dRac_up_dPc(Thermal)) &
              + fTac * (RTM_Pc(1)%LW%dRbc_up_dPc(Thermal)  * CRP(:,IT_dv) + &
                        RTM_Pc(1)%LW%dB_dPc(Thermal)       * CRP(:,IEm) + &
                        RTM_Pc(1)%LW%dRac_dwn_dPc(Thermal) * CRP(:,IR_dv))

   ! Gradient w.r.t. cloud fraction, f
   d_R(:,IFr) = R_over - R_clear

   ! Gradient w.r.t. surface temperature, Ts
   d_R(:,ITs) = fTac * Es_dB_dTs * RTM_Pc(1)%LW%Tbc(Thermal) * CRP(:,IT_dv) + &
      (1.0 - X(IFr)) * Es_dB_dTs * SPixel%RTM%LW%Tsf(Thermal)
else
   ! Update below cloud radiance after interpolation to Pc
   RTM_Pc(2)%LW%Rbc_up(Thermal) = RTM_Pc(2)%LW%Rbc_up(Thermal) + &
      (delta_Ts * Es_dB_dTs * RTM_Pc(2)%LW%Tbc(Thermal))

   ! Transmittance from the lower layer to the upper layer
   Tmc = RTM_Pc(2)%LW%Tac(Thermal) / RTM_Pc(1)%LW%Tac(Thermal)

   ! Radiance from the atmopshere between the layers
   Rmc_up = (RTM_Pc(2)%LW%Rac_up (Thermal) - RTM_Pc(1)%LW%Rac_up (Thermal)) / &
             RTM_Pc(1)%LW%Tac(Thermal)
   Rmc_dwn = RTM_Pc(2)%LW%Rac_dwn(Thermal) - RTM_Pc(1)%LW%Rac_dwn(Thermal) * Tmc

   a = (((RTM_Pc(2)%LW%Rbc_up(Thermal) * CRP2(:,IT_dv) + &
          RTM_Pc(2)%LW%B(Thermal) * CRP2(:,IEm)) * Tmc + &
         Rmc_up) * CRP(:,IT_dv) + &
        RTM_Pc(1)%LW%B(Thermal) * CRP (:,IEm) &
       ) * RTM_Pc(1)%LW%Tac(Thermal) + &
        RTM_Pc(1)%LW%Rac_up(Thermal)

   b = Tmc * CRP(:,IT_dv) * RTM_Pc(1)%LW%Tac(Thermal) * CRP2(:,IR_dv) * &
       (Rmc_dwn + (RTM_Pc(1)%LW%B(Thermal) * CRP(:,IEm) + &
        RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IT_dv)) * Tmc)

   c = RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IR_dv) * RTM_Pc(1)%LW%Tac(Thermal)

   ! Calculate overcast radiances at TOA
   R_over = a + b + c

   ! Calculate partly cloudy radiances at TOA (a linear combination of R_clear
   ! and R_over)
   f2 = X(IFr) + X(IFr2) - X(IFr) * X(IFr2)
   R = f2 * R_over + (1.0 - f2) * R_clear

   ! Gradient w.r.t. cloud optical depth, tau in layer 1
   call derivative_wrt_crp_parameter_layer_1(ITauCRP, Thermal, SPixel, RTM_Pc, &
        CRP, d_CRP, d_CRP2, CRP2, f2, Tmc, Rmc_up, Rmc_dwn, d_R(:,ITau))

   ! Gradient w.r.t. effective radius, re in layer 1
   call derivative_wrt_crp_parameter_layer_1(IReCRP,  Thermal, SPixel, RTM_Pc, &
        CRP, d_CRP, d_CRP2, CRP2, f2, Tmc, Rmc_up, Rmc_dwn, d_R(:,IRe))

   ! Gradient w.r.t. cloud pressure, Pc in layer 1
   Tmc_l = - Tmc * RTM_Pc(1)%LW%dTac_dPc(Thermal) / RTM_Pc(1)%LW%Tac(Thermal)

   Rmc_up_l = (- RTM_Pc(1)%LW%dRac_up_dPc(Thermal) - &
      Rmc_up * RTM_Pc(1)%LW%dTac_dPc(Thermal)) / RTM_Pc(1)%LW%Tac(Thermal)

   Rmc_dwn_l = - RTM_Pc(1)%LW%dRac_dwn_dPc(Thermal) * Tmc

   a = RTM_Pc(2)%LW%Rbc_up(Thermal) * CRP2(:,IT_dv) * CRP(:,IT_dv) * &
          (Tmc_l * RTM_Pc(1)%LW%Tac(Thermal) + &
           Tmc * RTM_Pc(1)%LW%dTac_dPc(Thermal)) + &
       RTM_Pc(2)%LW%B(Thermal) * CRP2(:,IEm) * CRP(:,IT_dv) * &
          (Tmc_l * RTM_Pc(1)%LW%Tac(Thermal) + &
           Tmc * RTM_Pc(1)%LW%dTac_dPc(Thermal)) + &
       CRP(:,IEm) * (RTM_Pc(1)%LW%dB_dPc(Thermal) * RTM_Pc(1)%LW%Tac(Thermal)  + &
                     RTM_Pc(1)%LW%B(Thermal) * RTM_Pc(1)%LW%dTac_dPc(Thermal)) + &
       CRP(:,IT_dv) * (Rmc_up_l * RTM_Pc(1)%LW%Tac(Thermal) + &
                       Rmc_up * RTM_Pc(1)%LW%dTac_dPc(Thermal)) + &
       RTM_Pc(1)%LW%dRac_up_dPc(Thermal)

   b = CRP(:,IT_dv) * CRP2(:,IR_dv) * &
          ((Tmc_l * RTM_Pc(1)%LW%Tac(Thermal) + &
            Tmc * RTM_Pc(1)%LW%dTac_dPc(Thermal)) * &
             (Rmc_dwn + &
              (RTM_Pc(1)%LW%B(Thermal) * CRP(:,IEm) + &
               RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IT_dv)) * Tmc) + &
           Tmc * RTM_Pc(1)%LW%Tac(Thermal) * &
             (Rmc_dwn_l + &
              (RTM_Pc(1)%LW%dB_dPc(Thermal) * Tmc + &
               RTM_Pc(1)%LW%B(Thermal) * Tmc_l) * CRP(:,IEm) + &
              (RTM_Pc(1)%LW%dRac_dwn_dPc(Thermal) * Tmc + &
               RTM_Pc(1)%LW%Rac_dwn(Thermal) * Tmc_l) * CRP(:,IT_dv)))

   c = CRP(:,IR_dv) * &
          (RTM_Pc(1)%LW%dRac_dwn_dPc(Thermal) * RTM_Pc(1)%LW%Tac(Thermal) + &
           RTM_Pc(1)%LW%Rac_dwn(Thermal) * RTM_Pc(1)%LW%dTac_dPc(Thermal))

   d_R(:,IPc) = f2 * (a + b + c)

   ! Gradient w.r.t. cloud fraction, f in layer 1
   d_R(:,IFr) = (1. - X(IFr2)) * (R_over - R_clear)

   ! Gradient w.r.t. cloud optical depth, tau in layer 2
   call derivative_wrt_crp_parameter_layer_2(ITauCRP, Thermal, SPixel, RTM_Pc, &
        CRP, d_CRP, d_CRP2, CRP2, f2, Tmc, Rmc_up, Rmc_dwn, d_R(:,ITau2))

   ! Gradient w.r.t. effective radius, re in layer 2
   call derivative_wrt_crp_parameter_layer_2(IReCRP,  Thermal, SPixel, RTM_Pc, &
        CRP, d_CRP, d_CRP2, CRP2, f2, Tmc, Rmc_up, Rmc_dwn, d_R(:,IRe2))

   ! Gradient w.r.t. cloud pressure, Pc in layer 2
   Tmc_l = RTM_Pc(2)%LW%dTac_dPc(Thermal) / RTM_Pc(1)%LW%Tac(Thermal)

   Rmc_up_l = RTM_Pc(2)%LW%dRac_up_dPc(Thermal) / RTM_Pc(1)%LW%Tac(Thermal)

   Rmc_dwn_l = - RTM_Pc(2)%LW%dRac_dwn_dPc(Thermal)

   a = RTM_Pc(2)%LW%dRbc_up_dPc(Thermal) * CRP2(:,IT_dv) * Tmc * &
          CRP(:,IT_dv) * RTM_Pc(1)%LW%Tac(Thermal) + &
       RTM_Pc(2)%LW%dB_dPc(Thermal)      * CRP2(:,IEm)   * Tmc * &
          CRP(:,IT_dv) * RTM_Pc(1)%LW%Tac(Thermal)

   b = CRP(:,IT_dv) * RTM_Pc(1)%LW%Tac(Thermal) * CRP2(:,IR_dv) * &
      (Tmc_l * (Rmc_dwn   + (RTM_Pc(1)%LW%B(Thermal) * CRP(:,IEm) + &
                             RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IT_dv)) * Tmc) + &
       Tmc   * (Rmc_dwn_l + (RTM_Pc(1)%LW%B(Thermal) * CRP(:,IEm) + &
                             RTM_Pc(1)%LW%Rac_dwn(Thermal) * CRP(:,IT_dv)) * Tmc_l))

   c = 0.

   d_R(:,IPc2) = f2 * (a + b + c)

   ! Gradient w.r.t. cloud fraction, f in layer 2
   d_R(:,IFr2) = (1. - X(IFr)) * (R_over - R_clear)

   ! Gradient w.r.t. surface temperature, Ts
   d_R(:,ITs) = f2 * Es_dB_dTs * RTM_Pc(2)%LW%Tbc(Thermal) * CRP2(:,IT_dv) * &
                   Tmc * CRP(:,IT_dv) * RTM_Pc(1)%LW%Tac(Thermal) + &
                (1. - f2) * Es_dB_dTs * SPixel%RTM%LW%Tsf(Thermal)
end if
   ! Convert radiances to brightness temperatures
   call R2T(SPixel%Ind%NThermal, SAD_Chan, R, BT, dT_dR, status)

   ! Calculate the change in brightness temperatures w.r.t. state parameters
   ! using dBT_dX = dT_dR * dR_dX (loop through each state parameter)
   do i = 1, MaxStateVar
      d_BT(:,i) = dT_dR * d_R(:,i)
   end do

end subroutine FM_Thermal
