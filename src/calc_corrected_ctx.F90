!-------------------------------------------------------------------------------
! Name: calc_corrected_ctx.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
!
! History:
! 2015/11/18, GM: Original version.
! 2015/12/15, GM: Some cleanup.  Make use of find_in_array().
! 2016/01/25, GM: Output uncorrected values when no correction is applied.
! 2016/01/28, GM: Output CTH and CTT corrected and corrected_uncertianty.
! 2016/02/12, GM: Apply a lower CTP correction limit that also affects CTP and
!    CTH.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

function get_corrected_bt(Ctrl, SPixel, SAD_Chan, RTM_Pc, i_spixel_y_thermal, Y) &
         result(bt)

   use Ctrl_m
   use planck_m
   use RTM_Pc_m
   use SAD_Chan_m
   use SPixel_m

   implicit none

   type(Ctrl_t),     intent(in) :: Ctrl
   type(SPixel_t),   intent(in) :: SPixel
   type(SAD_Chan_t), intent(in) :: SAD_Chan(:)
   type(RTM_Pc_t),   intent(in) :: RTM_Pc
   integer,          intent(in) :: i_spixel_y_thermal
   real,             intent(in) :: Y(:)

   real                         :: bt

   integer :: i_ctrl_y
   integer :: i_spixel_y
   integer :: status

   real    :: a(1)
   real    :: b(1)
   real    :: R(1)

   i_spixel_y = SPixel%Ind%YThermal(i_spixel_y_thermal)
   i_ctrl_y   = SPixel%spixel_y_to_ctrl_y_index(i_spixel_y)

   call T2R(1, SAD_Chan(i_ctrl_y:i_ctrl_y), Y(i_spixel_y:i_spixel_y), R, a, status)

   R = R / RTM_Pc%LW%Tac(SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(i_spixel_y_thermal))

   call R2T(1, SAD_Chan(i_ctrl_y:i_ctrl_y), R, a, b, status)
   bt = a(1)

end function get_corrected_bt


subroutine Calc_Corrected_CTX(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, Sy)

   use Ctrl_m
   use GZero_m
   use Int_Routines_m
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
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT
   type(RTM_Pc_t),   intent(in)    :: RTM_Pc
   real,             intent(in)    :: Sy(:,:)

   ! Local variable declarations

   integer       :: status
   integer       :: i_spixel_11
   integer       :: i_spixel_12
   integer       :: i_spixel_11_thermal
   integer       :: i_spixel_12_thermal
   integer       :: Y_Id(SPixel%Ind%NThermal)
   real          :: T_11
   real          :: T_12
   real          :: T_11_l
   real          :: T_12_l
   real          :: beta_11
   real          :: beta_12
   real          :: beta_11_l_r_e
   real          :: beta_12_l_r_e
   real          :: delta_beta
   real          :: ctt_new_l_r_e
   real          :: ctt_new_l_t11
   real          :: ctt_new_l_t12
   real          :: ctt_new_sigma
   real          :: ctt_new
   real          :: ctp_new
   real          :: ctp_new_sigma
   real          :: cth_new
   real          :: cth_new_sigma
   real          :: delta_ctp
   real          :: CRP_thermal(SPixel%Ind%NThermal)
   real          :: d_CRP_thermal(SPixel%Ind%NThermal, 2)
   type(GZero_t) :: GZero

   ! Set the corrected CTH to the retrieved CTH by default
   SPixel%CTP_corrected = SPixel%Xn(IPc)
   SPixel%CTH_corrected = RTM_Pc%Hc
   SPixel%CTT_corrected = RTM_Pc%Tc
   if (SPixel%Sn(IPc,IPc) .eq. MissingSn) then
      SPixel%CTP_corrected_uncertainty = MissingSn
      SPixel%CTH_corrected_uncertainty = MissingSn
      SPixel%CTT_corrected_uncertainty = MissingSn
   else
      SPixel%CTP_corrected_uncertainty = sqrt(SPixel%Sn(IPc,IPc))
      SPixel%CTH_corrected_uncertainty = abs(RTM_Pc%dHc_dPc) * &
                                         SPixel%CTP_corrected_uncertainty
      SPixel%CTT_corrected_uncertainty = abs(RTM_Pc%dTc_dPc) * &
                                         SPixel%CTP_corrected_uncertainty
   end if

   ! Only correct for ice
   if (Ctrl%Class .ne. ClsCldIce) then
      return
   end if

   ! Find 11 and 12 micron indexes
   Y_Id = Ctrl%Ind%Y_Id(SPixel%spixel_y_thermal_to_ctrl_y_index(1:SPixel%Ind%NThermal))
   i_spixel_11_thermal = find_in_array(Y_Id, Ctrl%Ind%Y_Id_legacy(I_legacy_11_x))
   i_spixel_12_thermal = find_in_array(Y_Id, Ctrl%Ind%Y_Id_legacy(I_legacy_12_x))

   ! Need both 11 and 12 micron to perform the correction
   if (i_spixel_11_thermal .gt. 0 .and. i_spixel_12_thermal .gt. 0) then
      i_spixel_11 = SPixel%Ind%YThermal(i_spixel_11_thermal)
      i_spixel_12 = SPixel%Ind%YThermal(i_spixel_12_thermal)

      ! Interpolate Bext for the LUTs as function of Tau and Re.  Note that in
      ! fact Bext is in variant with Tau but this LUT has redundancy with Tau so
      ! that existing routines can be used.
      call Allocate_GZero(GZero, SPixel%Ind%Ny)
      call Set_GZero(SPixel%Xn(ITau), SPixel%Xn(IRe), Ctrl, SPixel, SAD_LUT, &
              GZero, status)
      call Int_LUT_TauRe(SAD_LUT%Bext, SPixel%Ind%NThermal, SAD_LUT%Grid, &
              GZero, Ctrl, CRP_thermal, d_CRP_thermal, &
              SPixel%spixel_y_thermal_to_ctrl_y_index, SPixel%Ind%YThermal, status)
      call Deallocate_GZero(GZero)

      ! Correct the BT11 and BT12 to that which will be observed through a
      ! transparent atmosphere.
      T_11 = get_corrected_bt(Ctrl, SPixel, SAD_Chan, RTM_Pc, &
                              i_spixel_11_thermal, SPixel%Ym)
      T_12 = get_corrected_bt(Ctrl, SPixel, SAD_Chan, RTM_Pc, &
                              i_spixel_12_thermal, SPixel%Ym)

      ! For now we assume these are zero.  In fact the are not zero w.r.t. both
      ! the measurements and Pc.
      T_11_l = 1.
      T_12_l = 1.

      ! Extinction coefficients and associated derivatives
      beta_11       = CRP_thermal(i_spixel_11_thermal)
      beta_12       = CRP_thermal(i_spixel_12_thermal)

      beta_11_l_r_e = d_CRP_thermal(i_spixel_11_thermal, IRe)
      beta_12_l_r_e = d_CRP_thermal(i_spixel_12_thermal, IRe)

      delta_beta    = beta_11 - beta_12

      ! The corrected CTT equation and associated derivatives
      ctt_new       = (beta_11 * T_11 - beta_12 * T_12) / delta_beta

      ctt_new_l_r_e = (beta_11_l_r_e * T_11 - beta_12_l_r_e  * T_12 - &
                       ctt_new * (beta_11_l_r_e - beta_12_l_r_e)) / delta_beta
      ctt_new_l_t11 =   beta_11  * T_11_l / delta_beta
      ctt_new_l_t12 = - beta_12  * T_12_l / delta_beta

      ! Bext is not a function of Tau so SPixel%Sn(ITau, ITau) and Tau cross
      ! terms need not be included.  Still not including measurement and Pc
      ! uncertainty.
      ctt_new_sigma = &
         sqrt((ctt_new_l_r_e * SPixel%Sn(IRe, IRe)) * ctt_new_l_r_e + &
              (ctt_new_l_t11 * Sy(i_spixel_11,i_spixel_11) + &
               ctt_new_l_t12 * Sy(i_spixel_11,i_spixel_12)) * ctt_new_l_t11 + &
              (ctt_new_l_t11 * Sy(i_spixel_12,i_spixel_11) + &
               ctt_new_l_t12 * Sy(i_spixel_12,i_spixel_12)) * ctt_new_l_t12)

      delta_ctp     = (ctt_new - RTM_Pc%Tc) / RTM_Pc%dTc_dPc

      ! Propagate from CTT to CTP
      ctp_new       = SPixel%Xn(IPc) + delta_ctp
      ctp_new_sigma = abs(ctt_new_sigma / RTM_Pc%dTc_dPc)

      ! Propagate from CTP to CTH
      cth_new       = RTM_Pc%Hc      + delta_ctp * RTM_Pc%dHc_dPc
      cth_new_sigma = abs(ctp_new_sigma * RTM_Pc%dHc_dPc)

      ! Ignore corrections less than a thresold or in the downward direction
      if (ctp_new .gt. Ctrl%CTP_correction_limit .and. ctp_new .lt. SPixel%Xn(IPc)) then
         SPixel%CTP_corrected             = ctp_new
         SPixel%CTP_corrected_uncertainty = ctp_new_sigma
         SPixel%CTH_corrected             = cth_new
         SPixel%CTH_corrected_uncertainty = cth_new_sigma
         SPixel%CTT_corrected             = ctt_new
         SPixel%CTT_corrected_uncertainty = ctt_new_sigma
      end if
   end if

end subroutine Calc_Corrected_CTX
