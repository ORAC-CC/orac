!-------------------------------------------------------------------------------
! Name: fm_solar.F90
!
! Purpose:
! Reflectance forward model (solar channels) for a defined pixel and
! pressure level.
!
! Description and Algorithm details:
!   Note: FMThermal must have been called prior to this routine, to populate
!      CRP(ThermalFirst:SolarLast, ITd), i.e. for the part-thermal channels.
!   Get radiance functions.
!   Calculate transmittances for current geometry.
!   Calculate top of the atmosphere reflectances for clear, overcast, and part
!      cloudy conditions.
!   Calculate derivatives of reflectance w.r.t. all other variables.
!
! Arguments:
! Name     Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct      In          Control structure
! SAD_LUT  struct      In          SAD look up table
! SPixel   struct      In          Super-pixel structure
! i_layer  int         In          0: 1-layer model
!                                  1: layer 1 (upper) call of two-layer model
!                                  2: layer 2 (lower) call of two-layer model
! RTM_Pc   struct      In          Contains transmittances, radiances and their
!                                  derivatives for the layer of the call
! RTM_Pc2  struct      In          RTM_Pc of the other layer of the two-layer
!                                  model, i.e. if i_layer=2 the this is the
!                                  RTM_Pc for layer 1
! X        real array  In          State vector
! GZero    struct      In          "Zero'th point" grid info for SAD_LUT CRP
!                                  array interpolation.
! CRP      float array In          Interpolated cloud radiative properties
!                                  (calculated by SetCRPSolar, but an input
!                                  argument because CRP for Td for the mixed
!                                  channels is set by SetCRPThermal. Only the
!                                  solar channels should be passed by the
!                                  calling routine).
! d_CRP    float array In          Grads. of interpolated cloud rad. properties
!                                  (see comment for CRP).
! Ref      float array Out         TOA bidirectional reflectances for partly
!                                  cloudy conditions
! d_Ref    float array Out         Derivatives d[ref]/d[x]
! status   int         Out         Status from Set_CRP_Solar/breakpoint file
!                                  open
! Ref_0d   float array Out         Optional TOA directional-diffuse reflectances
!                                  for partly cloudy conditions
! d_Ref_0d float array Out         Optional Derivatives d[ref_0d]/d[x]
! Ref_dv   float array Out         Optional TOA diffuse-directional reflectances
!                                  for partly cloudy conditions
! d_Ref_dv float array Out         Optional Derivatives d[ref_dv]/d[x]
! Ref_dd   float array Out         Optional TOA diffuse-diffuse reflectances for
!                                  partly cloudy conditions
! d_Ref_dd float array Out         Optional Derivatives d[ref_dd]/d[x]
! RsX      float array In          Optional overriding bidirectional surface
!                                  reflectance
! RsX_0d   float array In          Optional overriding directional-diffuse
!                                  surface reflectance
! RsX_dv   float array In          Optional overriding diffuse-directional
!                                  surface reflectance
! RsX_dd   float array In          Optional overriding diffuse-diffuse surface
!                                  reflectance
! d_Ref_dRs2 float array Out       Optional derivatives w.r.t surface BRDF
!                                  parameters
!
! History:
! 2000/11/07, KS: original version
! 2000/11/21, KS: added X structure (state vector)
! 2001/01/17, AS: Changed indexing of CRP arrays to use constants to reference
!    the different LUT values (IRbd etc) Using FM_Routines_m: contains interface
!    definition for SetCRPSolar.
! 2001/01/23, AS: Added GZero argument, interface to SetCRPSolar changed.
!    Updated CRP, d_CRP and Ref, d_Ref array indexing to use constants to pick
!    out the values depending on Tau, Re etc.
! 2001/02/02, AS: Transmittance values Tac etc made arguments, Interpol_Solar
!    is now called before this function rather than from this function.
! 2001/02/07, AS: Picks up Tac values etc from RTM_Pc structure.
! 2001/02/09, AS: First fully completed and (informally) tested version.
! 2001/02/15, AS: Array sizes changed: Ctrl%Ind%NSolar to SPixel%Ind%NSolar
!    Indices changed where whole dimension is used, replaced array(1:
!    SPixel%Ind%NSolar) with array(:).
! 2001/02/16, AS: Array sizes changed again: only the purely solar channels are
!    required. Use Ny-NThermal instead of NSolar. Calculate a local NSolar
!    variable (with the same value) for indexing SPixel%Rs. SetCRPSolar now
!    requires SPixel as an argument.
! 2001/02/19, AS: Error in previous revision. Arrays must hold all solar
!    channels, not just purely solar. CRP, d_CRP required as arguments
!    (partially populated by FMThermal). SetCRPSolar now takes SPixel%Ind as an
!    argument instead of SPixel.
! 2001/03/02, AS: Updates to cope with transmittances etc as fractions instead
!    of percentages.
! 2001/03/06, AS: Calculation of Ref_clear and dRef_clear_dRs moved to
!    Get_SPixel. (Values are now part of SPixel%RTM).
! 2001/03/07, AS: Tac, Tbc etc now picked up from the overall RTM_Pc struct
!    rather than the SW sub-structure, so that all solar channels are selected,
!    and not just the purely solar.
! 2001/03/13, AS: Changed some of the "compound" variables: S, TBTD etc can go
!    to 0 and are used in division operations. New variable Sp (S prime,
!    replaces S / TBTD).
! 2001/03/22, AS: Corrected equations for d_Ref wrt Tau and Re
! 2001/03/23, AS: Updated calculation of dRef wrt Rs to include divide by
!    SPixel%Geom%SEC_o since SPixel Rs values now include this factor.
! 2001/10/15, AS: Gradient w.r.t Rs fixed. Previously only the second term was
!    divided by sec_o, whereas the whole expression should have been divided.
!    Also the bracketing was incorrect: instead of a "f" term and a "1-f" term,
!    everything was multiplied by f. (For "f" read "X(IFr)" in the code).
! 2011/05/05, AS: Extension to multiple viewing angles. Some whole-array
!    assignments replaced by loops over channels, where appropriate viewing
!    geometry must be selected. Added some breakpoint outputs.
! 2012/01/20, CP: Fixed bug with rtm_pc%tbc array allocation.
! 2014/12/20, GM: Cleaned up code.
! 2014/12/24, GM: Some intent changes.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/12/11, GM: Some small mathematical bug fixes in the BRDF derivative
!    equations, mathematical refactoring to reduce computation, and some
!    cleanup.
! 2015/01/09, AP: Eliminate write to RTM_Pc%Tac, Tbc.
! 2015/01/15, AP: Facilitate channel indexing in arbitrary order.
! 2015/01/21, AP: Finishing the last commit.
! 2015/07/31, GM: Remove i_equation 2 as an invalid derivation.
! 2015/08/21, AP: Massive overhaul to include both aerosol forward models:
!    Transmittance calculations now optional. Swansea FM integrated with
!    use_full_brdf=.false. Aerosol FM added as equations_forms 2 and 3.
!    Calculation of derivatives wrt surface reflectance generalised.
! 2016/01/07, AP: Bug fix in BRDF Jacobians. IRs terms were not being divided by
!    the solar factor. The uncertainty in surface reflectance describes the un-
!    corrected value, so derivatives need to be scaled.
! 2016/06/06, GM: Remove i_equation 1 as an invalid derivation.  Shift equations
!    2-3 to be 1-2.  Add a new equation 3 to be the same as equation 4 but sun
!    normalized like equation 1 whereas equation 4 is still sun normalized like
!    equation 3.  See detailed comments which also indicate that equations 2 and
!    4 are incorrectly sun normalized and will be removed in the future.
! 2016/07/27, GM: Extensive changes for multilayer support with several new
!    optional inputs and outputs to perform multilayer simulations with two
!    consecutive calls.
! 2016/09/23, AP: Add Swansea G to the state vector.
! 2016/09/24. AP: Add ClsAerBR, a BRDF form of the Swansea surface. It maps the
!    three terms of the Swansea equation to the BRDF terms.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define USE_REVISED_REF_0D .true.

subroutine d_derivative_wrt_crp_parameter(i_param, i_p_crp, T_0d, T_00, &
   d_T_0d, d_T_00, T_all, d_D)

   implicit none

   integer, intent(in)  :: i_param
   integer, intent(in)  :: i_p_crp
   real,    intent(in)  :: T_0d(:)
   real,    intent(in)  :: T_00(:)
   real,    intent(in)  :: d_T_0d(:,:)
   real,    intent(in)  :: d_T_00(:,:)
   real,    intent(in)  :: T_all(:)
   real,    intent(out) :: d_D(:,:)

   d_D(:,i_param) = (d_T_0d(:,i_p_crp) * T_00 - d_T_00(:,i_p_crp) * T_0d) / &
                    (T_all * T_all)

end subroutine d_derivative_wrt_crp_parameter


subroutine rs_derivative_wrt_crp_parameter(i_param, i_p_crp, gamma, s, p, CRP, &
   d_CRP, T_all, d_Rs)

   implicit none

   integer, intent(in)  :: i_param
   integer, intent(in)  :: i_p_crp
   real,    intent(in)  :: gamma
   real,    intent(in)  :: s(:)
   real,    intent(in)  :: p(:)
   real,    intent(in)  :: CRP(:,:)
   real,    intent(in)  :: d_CRP(:,:,:)
   real,    intent(in)  :: T_all(:)
   real,    intent(out) :: d_Rs(:,:)

   call d_derivative_wrt_crp_parameter(i_param, i_p_crp, CRP(:,IT_0d), &
        CRP(:,IT_00), d_CRP(:,IT_0d,:), d_CRP(:,IT_00,:), T_all, d_Rs)
   d_Rs(:,i_param) = s * (gamma - p) * d_Rs(:,i_p_crp)

end subroutine rs_derivative_wrt_crp_parameter


subroutine derivative_wrt_crp_parameter(i_p_crp, Rs, CRP, d_CRP, f, Tac_0v, &
   Tbc2, T_all, S_dnom, d_Rs, d_Ref)

   implicit none

   integer, intent(in)  :: i_p_crp
   real,    intent(in)  :: Rs(:)
   real,    intent(in)  :: CRP(:,:)
   real,    intent(in)  :: d_CRP(:,:,:)
   real,    intent(in)  :: f
   real,    intent(in)  :: Tac_0v(:)
   real,    intent(in)  :: Tbc2(:)
   real,    intent(in)  :: T_all(:)
   real,    intent(in)  :: S_dnom(:)
   real,    intent(in)  :: d_Rs(:)
   real,    intent(out) :: d_Ref(:)

   d_Ref = f * Tac_0v * ( &
      d_CRP(:,IR_0v,i_p_crp) + &
      Tbc2 / S_dnom * Rs * ( &
         T_all * d_CRP(:,IT_dv,i_p_crp) + &
         CRP(:,IT_dv) * (d_CRP(:,IT_00,i_p_crp) + d_CRP(:,IT_0d,i_p_crp))) + &
      T_all * CRP(:,IT_dv) * Tbc2 / (S_dnom * S_dnom) * &
         (d_Rs(:) + Rs * Rs * Tbc2 * d_CRP(:,IR_dd,i_p_crp)))

end subroutine derivative_wrt_crp_parameter


subroutine derivative_wrt_crp_parameter_brdf_0v(SPixel, i_p_crp, i_equation_form, &
   CRP, d_CRP, f, Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, &
   Rs2, d_Ref, a, b, c)

   use SPixel_m

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_p_crp
   integer,        intent(in)  :: i_equation_form
   real,           intent(in)  :: CRP(:,:)
   real,           intent(in)  :: d_CRP(:,:,:)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_0v(:)
   real,           intent(in)  :: Tbc_0(:)
   real,           intent(in)  :: Tbc_v(:)
   real,           intent(in)  :: Tbc_d(:)
   real,           intent(in)  :: Tbc_0v(:)
   real,           intent(in)  :: Tbc_0d(:)
   real,           intent(in)  :: Tbc_dv(:)
   real,           intent(in)  :: Tbc_dd(:)
   real,           intent(in)  :: Rs2(:,:)
   real,           intent(out) :: d_Ref(:)
   real,           intent(in)  :: a(:)
   real,           intent(in)  :: b(:)
   real,           intent(in)  :: c(:)

   real :: e_l(SPixel%Ind%NSolar)
   real :: Ref_over_l(SPixel%Ind%NSolar)

   if (i_equation_form == 1) then
      e_l = d_CRP(:,IR_0v,i_p_crp) + &
           ((d_CRP(:,IT_00,i_p_crp) * (Rs2(:,IRho_0V) - &
               Rs2(:,IRho_0D)) * CRP(:,IT_vv) + CRP(:,IT_00) * &
               (Rs2(:,IRho_0V) - Rs2(:,IRho_0D)) * &
               d_CRP(:,IT_vv,i_p_crp)) * Tbc_0v + &
            ((d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_DD) * Tbc_d) * c + &
               b * d_CRP(:,IT_dv,i_p_crp) * Tbc_d) / a + &
            b * c * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd / (a*a)) &
           / SPixel%Geom%SEC_o(1)
   else if (i_equation_form == 2) then
      e_l = d_CRP(:,IR_0v,i_p_crp) + &
            (d_CRP(:,IT_00,i_p_crp) * (Rs2(:,IRho_0V) - &
               Rs2(:,IRho_0D)) * CRP(:,IT_vv) + CRP(:,IT_00) * &
               (Rs2(:,IRho_0V) - Rs2(:,IRho_0D)) * &
               d_CRP(:,IT_vv,i_p_crp)) * Tbc_0v + &
            ((d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_DD) * Tbc_d) * c + &
               b * d_CRP(:,IT_dv,i_p_crp) * Tbc_d) / a + &
            b * c * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd / (a*a)
   else if (i_equation_form == 3) then
      e_l = d_CRP(:,IR_0v,i_p_crp) + &
           ((d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0V) * CRP(:,IT_vv) + &
               CRP(:,IT_00) * Rs2(:,IRho_0V) * d_CRP(:,IT_vv,i_p_crp)) * &
               Tbc_0v + &
            (d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_DV) * CRP(:,IT_vv) + &
               CRP(:,IT_0d) * Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_p_crp)) * &
               Tbc_dv + &
            ((d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_DD) * Tbc_d) * c + &
               b * (d_CRP(:,IT_dv,i_p_crp) * Tbc_d + d_CRP(:,IR_dd,i_p_crp) * &
               Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dd * Tbc_v + &
               CRP(:,IR_dd) * Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_p_crp) * &
               Tbc_dd * Tbc_v)) / a + &
            b * c * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd / (a*a)) &
           / SPixel%Geom%SEC_o(1)
   else
      e_l = d_CRP(:,IR_0v,i_p_crp) + &
            (d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0V) * CRP(:,IT_vv) + &
               CRP(:,IT_00) * Rs2(:,IRho_0V) * d_CRP(:,IT_vv,i_p_crp)) * &
               Tbc_0v + &
            (d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_DV) * CRP(:,IT_vv) + &
               CRP(:,IT_0d) * Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_p_crp)) * &
               Tbc_dv + &
            ((d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_DD) * Tbc_d) * c + &
               b * (d_CRP(:,IT_dv,i_p_crp) * Tbc_d + d_CRP(:,IR_dd,i_p_crp) * &
               Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dd * Tbc_v + &
               CRP(:,IR_dd) * Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_p_crp) * &
               Tbc_dd * Tbc_v)) / a + &
            b * c * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd / (a*a)
   end if

   Ref_over_l = Tac_0v * e_l

   d_Ref = f * Ref_over_l

end subroutine derivative_wrt_crp_parameter_brdf_0v


subroutine derivative_wrt_crp_parameter_brdf_0d(SPixel, i_p_crp, i_equation_form, &
   CRP, d_CRP, f, Tac_0d, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, &
   Rs2, d_Ref, a, b, c)

   use SPixel_m

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_p_crp
   integer,        intent(in)  :: i_equation_form
   real,           intent(in)  :: CRP(:,:)
   real,           intent(in)  :: d_CRP(:,:,:)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_0d(:)
   real,           intent(in)  :: Tbc_0(:)
   real,           intent(in)  :: Tbc_v(:)
   real,           intent(in)  :: Tbc_d(:)
   real,           intent(in)  :: Tbc_0v(:)
   real,           intent(in)  :: Tbc_0d(:)
   real,           intent(in)  :: Tbc_dv(:)
   real,           intent(in)  :: Tbc_dd(:)
   real,           intent(in)  :: Rs2(:,:)
   real,           intent(out) :: d_Ref(:)
   real,           intent(in)  :: a(:)
   real,           intent(in)  :: b(:)
   real,           intent(in)  :: c(:)

   real :: e_l(SPixel%Ind%NSolar)
   real :: Ref_over_l(SPixel%Ind%NSolar)

   if (i_equation_form == 1 .or. i_equation_form == 3) then
if (USE_REVISED_REF_0D) then
      e_l = d_CRP(:,IR_0d,i_p_crp) + &
            ((d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0d) * Tbc_0 + &
              d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_dd) * Tbc_d) * c + &
             b * d_CRP(:,IT_dd,i_p_crp) * Tbc_d) / a + &
            (b * c * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd) / (a*a)
else
      e_l = d_CRP(:,IR_0d,i_p_crp) + &
            (d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0V) * CRP(:,IT_vd) + &
             CRP(:,IT_00) * Rs2(:,IRho_0V) * d_CRP(:,IT_vd,i_p_crp)) * Tbc_0v + &
            ((d_CRP(:,IT_00,i_p_crp) * Rs2(:,IRho_0d) * Tbc_0 + &
              d_CRP(:,IT_0d,i_p_crp) * Rs2(:,IRho_dd) * Tbc_d) * c + &
             b * d_CRP(:,IT_dd,i_p_crp) * Tbc_d) / a + &
            (b * c * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd) / (a*a)
end if
   else
      write (*,*) 'ERROR: FM_Solar(), directional-diffuse reflectance not' // &
         ' supported with i_equation_form = ', i_equation_form
      stop error_stop_code
   end if

   Ref_over_l = Tac_0d * e_l

   d_Ref = f * Ref_over_l

end subroutine derivative_wrt_crp_parameter_brdf_0d


subroutine derivative_wrt_crp_parameter_brdf_dv(SPixel, i_p_crp, i_equation_form, &
   CRP, d_CRP, f, Tac_dv, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, &
   Rs2, d_Ref, a, b, c, d)

   use SPixel_m

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_p_crp
   integer,        intent(in)  :: i_equation_form
   real,           intent(in)  :: CRP(:,:)
   real,           intent(in)  :: d_CRP(:,:,:)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_dv(:)
   real,           intent(in)  :: Tbc_0(:)
   real,           intent(in)  :: Tbc_v(:)
   real,           intent(in)  :: Tbc_d(:)
   real,           intent(in)  :: Tbc_0v(:)
   real,           intent(in)  :: Tbc_0d(:)
   real,           intent(in)  :: Tbc_dv(:)
   real,           intent(in)  :: Tbc_dd(:)
   real,           intent(in)  :: Rs2(:,:)
   real,           intent(out) :: d_Ref(:)
   real,           intent(in)  :: a(:)
   real,           intent(in)  :: b(:)
   real,           intent(in)  :: c(:)
   real,           intent(in)  :: d(:)

   real :: e_l(SPixel%Ind%NSolar)
   real :: Ref_over_l(SPixel%Ind%NSolar)

   if (i_equation_form == 1 .or. i_equation_form == 3) then
      e_l = d_CRP(:,IR_dv,i_p_crp) + &
            (d_CRP(:,IT_dd,i_p_crp) * Tbc_d * d + &
             c * (Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_p_crp) * Tbc_v + &
                  Rs2(:,IRho_DD) * d_CRP(:,IT_dv,i_p_crp) * Tbc_d)) / a + &
            c * d * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd / (a*a)
   else
      write (*,*) 'ERROR: FM_Solar(), diffuse-directional reflectance not' // &
         ' supported with i_equation_form = ', i_equation_form
      stop error_stop_code
   end if

   Ref_over_l = Tac_dv * e_l

   d_Ref = f * Ref_over_l

end subroutine derivative_wrt_crp_parameter_brdf_dv


subroutine derivative_wrt_crp_parameter_brdf_dd(SPixel, i_p_crp, i_equation_form, &
   CRP, d_CRP, f, Tac_dd, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, &
   Rs2, d_Ref, a, b)

   use SPixel_m

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_p_crp
   integer,        intent(in)  :: i_equation_form
   real,           intent(in)  :: CRP(:,:)
   real,           intent(in)  :: d_CRP(:,:,:)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_dd(:)
   real,           intent(in)  :: Tbc_0(:)
   real,           intent(in)  :: Tbc_v(:)
   real,           intent(in)  :: Tbc_d(:)
   real,           intent(in)  :: Tbc_0v(:)
   real,           intent(in)  :: Tbc_0d(:)
   real,           intent(in)  :: Tbc_dv(:)
   real,           intent(in)  :: Tbc_dd(:)
   real,           intent(in)  :: Rs2(:,:)
   real,           intent(out) :: d_Ref(:)
   real,           intent(in)  :: a(:)
   real,           intent(in)  :: b(:)

   real :: e_l(SPixel%Ind%NSolar)
   real :: Ref_over_l(SPixel%Ind%NSolar)

   if (i_equation_form == 1 .or. i_equation_form == 3) then
      e_l = d_CRP(:,IR_dd,i_p_crp) + &
            (2. * CRP(:,IT_dd) * d_CRP(:,IT_dd,i_p_crp) * &
             Rs2(:,IRho_dd) * Tbc_dd) / a + &
            CRP(:,IT_dd)**2 * Rs2(:,IRho_dd) * Tbc_dd * &
            Rs2(:,IRho_dd) * d_CRP(:,IR_dd,i_p_crp) * Tbc_dd / (a*a)
   else
      write (*,*) 'ERROR: FM_Solar(), diffuse-diffuse reflectance not' // &
         ' supported with i_equation_form = ', i_equation_form
      stop error_stop_code
   end if

   Ref_over_l = Tac_dd * e_l

   d_Ref = f * Ref_over_l

end subroutine derivative_wrt_crp_parameter_brdf_dd


subroutine derivative_wrt_rho_parameters_brdf(SPixel, i_equation_form, CRP, f, &
   Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, rho_l, d_Ref, &
   a, b, c)

   use SPixel_m

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_equation_form
   real,           intent(in)  :: CRP(:,:)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_0v(:)
   real,           intent(in)  :: Tbc_0(:)
   real,           intent(in)  :: Tbc_v(:)
   real,           intent(in)  :: Tbc_d(:)
   real,           intent(in)  :: Tbc_0v(:)
   real,           intent(in)  :: Tbc_0d(:)
   real,           intent(in)  :: Tbc_dv(:)
   real,           intent(in)  :: Tbc_dd(:)
   real,           intent(in)  :: rho_l(:,:)
   real,           intent(out) :: d_Ref(:)
   real,           intent(in)  :: a(:)
   real,           intent(in)  :: b(:)
   real,           intent(in)  :: c(:)

   real :: e_l(SPixel%Ind%NSolar)
   real :: Ref_over_l(SPixel%Ind%NSolar)

   if (i_equation_form == 1) then
      e_l =( CRP(:,IT_00) * (rho_l(:,IRho_0V) - rho_l(:,IRho_0D)) * CRP(:,IT_vv) * Tbc_0v &
          + (CRP(:,IT_00) * rho_l(:,IRho_0D) * Tbc_0 + &
             CRP(:,IT_0d) * rho_l(:,IRho_DD) * Tbc_d) * c / a &
          +  b * c * rho_l(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd / (a*a)) &
          / SPixel%Geom%SEC_o(1)
   else if (i_equation_form == 2) then
      e_l =  CRP(:,IT_00) * (rho_l(:,IRho_0V) - rho_l(:,IRho_0D)) * CRP(:,IT_vv) * Tbc_0v &
          + (CRP(:,IT_00) * rho_l(:,IRho_0D) * Tbc_0 + &
             CRP(:,IT_0d) * rho_l(:,IRho_DD) * Tbc_d) * c / a &
          +  b * c * rho_l(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd / (a*a)
   else if (i_equation_form == 3) then
      e_l =( CRP(:,IT_00) * rho_l(:,IRho_0V) * CRP(:,IT_vv) * Tbc_0v &
          +  CRP(:,IT_0d) * rho_l(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dv &
          + (CRP(:,IT_00) * rho_l(:,IRho_0D) * Tbc_0 + &
             CRP(:,IT_0d) * rho_l(:,IRho_DD) * Tbc_d) * c / a &
          +  b * CRP(:,IR_dd) * rho_l(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dd * Tbc_v / a &
          +  b * c * rho_l(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd / (a*a)) &
          / SPixel%Geom%SEC_o(1)
   else
      e_l =  CRP(:,IT_00) * rho_l(:,IRho_0V) * CRP(:,IT_vv) * Tbc_0v &
          +  CRP(:,IT_0d) * rho_l(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dv &
          + (CRP(:,IT_00) * rho_l(:,IRho_0D) * Tbc_0 + &
             CRP(:,IT_0d) * rho_l(:,IRho_DD) * Tbc_d) * c / a &
          +  b * CRP(:,IR_dd) * rho_l(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dd * Tbc_v / a &
          +  b * c * rho_l(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd / (a*a)
   end if

   Ref_over_l = Tac_0v * e_l

   d_Ref = f * Ref_over_l + (1.0-f) * SPixel%RTM%dRef_clear_dRs * rho_l(:,IRho_DD)

end subroutine derivative_wrt_rho_parameters_brdf


subroutine FM_Solar(Ctrl, SAD_LUT, SPixel, i_layer, RTM_Pc, RTM_Pc2, X, GZero, &
   Ref, d_Ref, status, Ref_0d, d_Ref_0d, Ref_dv, d_Ref_dv, Ref_dd, d_Ref_dd, &
   RsX, RsX_0d, RsX_dv, RsX_dd, d_Ref_dRs2)

   use Ctrl_m
   use GZero_m
   use Int_LUT_Routines_m
   use ORAC_Constants_m
   use RTM_Pc_m
   use SAD_LUT_m
   use SPixel_m

   implicit none

   ! Define arguments

   type(Ctrl_t),    intent(in)    :: Ctrl
   type(SAD_LUT_t), intent(in)    :: SAD_LUT
   type(SPixel_t),  intent(in)    :: SPixel
   integer,         intent(in)    :: i_layer
   type(RTM_Pc_t),  intent(in)    :: RTM_Pc
   type(RTM_Pc_t),  intent(in)    :: RTM_Pc2
   real,            intent(in)    :: X(:)
   type(GZero_t),   intent(in)    :: GZero
   real,            intent(inout) :: Ref(:)
   real,            intent(inout) :: d_Ref(:,:)
   integer,         intent(out)   :: status
   real, optional,  intent(inout) :: Ref_0d(:)
   real, optional,  intent(inout) :: d_Ref_0d(:,:)
   real, optional,  intent(inout) :: Ref_dv(:)
   real, optional,  intent(inout) :: d_Ref_dv(:,:)
   real, optional,  intent(inout) :: Ref_dd(:)
   real, optional,  intent(inout) :: d_Ref_dd(:,:)
   real, optional,  intent(inout) :: RsX(:)
   real, optional,  intent(inout) :: RsX_0d(:)
   real, optional,  intent(inout) :: RsX_dv(:)
   real, optional,  intent(inout) :: RsX_dd(:)
   real, optional,  intent(inout) :: d_Ref_dRs2(:,:)

   ! Referencing of different properties stored in CRP and d_CRP: the last index
   ! of the CRP array refers to the property. Hence ITb is the index of TB, etc.

   ! Define local variables
   integer                            :: i, ii, j
   integer                            :: ITauX, IReX, IPcX, IFrX
   real                               :: dif_trans_fac
   integer                            :: Solar(SPixel%Ind%NSolar)
   real                               :: CRP(SPixel%Ind%NSolar, MaxCRProps)
   real                               :: d_CRP(SPixel%Ind%NSolar, MaxCRProps, &
                                               MaxCRPParams)
   real                               :: Ref_over(SPixel%Ind%NSolar)
   real                               :: Ref_over_0d(SPixel%Ind%NSolar)
   real                               :: Ref_over_dv(SPixel%Ind%NSolar)
   real                               :: Ref_over_dd(SPixel%Ind%NSolar)
   real                               :: Ref_over_l(SPixel%Ind%NSolar)

   ! Atmospheric transmittances
   real, dimension(SPixel%Ind%NSolar) :: Tac_0, Tac_v, Tac_d
   real, dimension(SPixel%Ind%NSolar) :: Tac_0v, Tac_0d, Tac_dv, Tac_dd
   real, dimension(SPixel%Ind%NSolar) :: Tbc_0, Tbc_v, Tbc_d
   real, dimension(SPixel%Ind%NSolar) :: Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd
   real, dimension(SPixel%Ind%NSolar) :: Tsf_0v, Tsf_0d, Tsf_dv, Tsf_dd

   ! Derivatives of those wrt Pc
   real, dimension(SPixel%Ind%NSolar) :: Tac_0_l, Tac_v_l, Tac_0v_l
   real, dimension(SPixel%Ind%NSolar) :: Tbc_0_l, Tbc_v_l, Tbc_d_l, Tbc_dd_l
   real, dimension(SPixel%Ind%NSolar) :: Tsf_0v_l

   ! Lambertian surface model terms
   real, dimension(SPixel%Ind%NSolar) :: T_all

   ! Swansea surface model terms
   real, dimension(SPixel%Ind%NSolar) :: Ss, Sp, Sd, Sa
   real                               :: Sg
   real                               :: Rs(SPixel%Ind%NSolar)
   real                               :: Rs2(SPixel%Ind%NSolar,MaxRho_XX)
   real                               :: d_Rs(SPixel%Ind%NSolar,5)

   ! Auxilary variables
   real, dimension(SPixel%Ind%NSolar) :: a, b, c, c_0d, c_dv, d_dv, e, &
                                         e_0d, e_dv, e_dd, e_l
   real                               :: g, h

   ! Used to determine d_Ref(:,IRs); gives ratio between element and the term
   ! that is retrieved.
   real                               :: rho_l(SPixel%Ind%NSolar, MaxRho_XX)


   status = 0.

   CRP    = 0.
   d_CRP  = 0.
   Rs     = 0.
   d_Rs   = 0.
   Rs2    = 0.

   ! Choose the appropriate layer specific parameter indices.
   if (i_layer < 2) then
      ITauX = ITau
      IReX  = IRe
      IPcX  = IPc
      IFrX  = IFr
   else
      ITauX = ITau2
      IReX  = IRe2
      IPcX  = IPc2
      IFrX  = IFr2
   end if

   ! Diffuse transmittance mass path
   dif_trans_fac = 1. ! 1. / cos(66. * d2r)

   ! Subscripts for solar channels in RTM arrays
   Solar = SPixel%spixel_y_solar_to_ctrl_y_solar_index(:SPixel%Ind%NSolar)

   ! Interpolate cloud radiative property LUT data to the current Tau, Re values.
   call Set_CRP_Solar(Ctrl, SPixel%Ind, SPixel%spixel_y_solar_to_ctrl_y_index, &
        GZero, SAD_LUT, CRP, d_CRP, status)

   ! Fetch surface reflectance
   if (i_layer == 1) then
      ! Surface BRDF parameters have been input directly by the caller.
      Rs2(:,IRho_0V) = RsX
      Rs2(:,IRho_0D) = RsX_0d
      Rs2(:,IRho_DV) = RsX_dv
      Rs2(:,IRho_DD) = RsX_dd
   else if (Ctrl%Approach == AppAerSw) then
      T_all = CRP(:,IT_00) + CRP(:,IT_0d) ! Direct + diffuse transmission

      ! Copy terms from X into tidier local variables
      Ss = X(SPixel%Surface%XIndex(:,ISwan_S))
      Sp = X(SPixel%Surface%XIndex(:,ISwan_P))
      Sg = X(ISG)
      Sa = 1.0 - (1.0 - Sg) * Ss ! Denominator

      if (Ctrl%RS%use_full_brdf) then
         ! Associate Swansea model terms to BRDF
         Rs2(:,IRho_0V) = Sp * Ss
         Rs2(:,IRho_DD) = Sg * Ss / Sa
         Rs2(:,IRho_0D) = (1.0 - Sg) * Ss * Rs2(:,IRho_DD)
      else
         ! D is proportion of the downward transmission which is diffuse
         Sd = CRP(:,IT_0d) / T_all

         ! Calculate surface reflectance with Swansea surface
         Rs = &
              ! Forward model as written in ATBD
              (1.0 - Sd) * (Sp * Ss + Sg * (1.0 - Sg) * Ss * Ss / Sa) + &
              Sd * Sg * Ss / Sa
              ! More efficient expression factorised in D
!             Ss * (Sp + Sd * (Sg - Sp) + Sg * (1.0 - Sg) * Ss / Sa)

         ! Derivatives of Rs wrt state vector terms
         call rs_derivative_wrt_crp_parameter(ITauX, ITauCRP, Sg, Ss, Sp, &
              CRP, d_CRP, T_all, d_Rs)
         call rs_derivative_wrt_crp_parameter(IReX,  IReCRP,  Sg, Ss, Sp, &
              CRP, d_CRP, T_all, d_Rs)
         ! Derivative wrt s vector (Using hardcoded second index for d_Rs as we
         ! only need to reference the s vector in this routine)
         d_Rs(:,ISv) = Sp + Sd * (Sg - Sp) + &
              Sg * (1.0 - Sg) * Ss * (1.0 + Sa) / (Sa * Sa)
         ! Derivative wrt p vector
         d_Rs(:,IPv) = Ss * (1.0 - Sd)
         ! Derivative wrt gamma
         d_Rs(:,IGv) = Ss * (Sd + Ss * (1.0 - 2.0*Sg) * (1.0 - Ss) / (Sa * Sa))
      end if
   else if (Ctrl%RS%use_full_brdf) then
      ! Copy surface reflectances from X to SPixel%Surface
      do j = 1, MaxRho_XX
         Rs2(:,j) = X(SPixel%Surface%XIndex(:,j)) * &
                      SPixel%Surface%Ratios(:,j)
      end do
   else
      T_all = CRP(:,IT_00) + CRP(:,IT_0d) ! Direct + diffuse transmission

      ! Copy Lambertian surface reflectances from X to SPixel%Surface
      Rs = X(SPixel%Surface%XIndex(:,IRho_DD)) * &
           SPixel%Surface%Ratios(:,IRho_DD)
   end if

   d_Ref = 0.
   if (present(d_Ref_0d)) d_Ref_0d = 0.
   if (present(d_Ref_dv)) d_Ref_dv = 0.
   if (present(d_Ref_dd)) d_Ref_dd = 0.

   if (Ctrl%RTMIntSelm == RTMIntMethNone) then
      ! Above/below cloud transmittances are 1 for infinite extent cloud/aerosol
      Tac_0  = 1.0
      Tac_v  = 1.0
      Tac_0v = 1.0
      Tac_0d = 1.0
      Tac_dd = 1.0
      Tbc_0  = 1.0
      Tbc_v  = 1.0
      Tbc_d  = 1.0
      Tbc_0v = 1.0
      Tbc_0d = 1.0
      Tbc_dv = 1.0
      Tbc_dd = 1.0
   else
      do i = 1, SPixel%Ind%NSolar
         ! Calculate above cloud (ac) beam transmittances.
         if (i_layer == 2) then
            ! In this case this is the first call of two-layer use called for
            ! the lower layer where the outputs are BRDF parameters representing
            ! the combination of the lower layer and the atmosphere and surface
            ! below and should not include above cloud extinction.

            ! At solar zenith angle:
            Tac_0(i) = 1.
            ! At sensor viewing angle:
            Tac_v(i) = 1.
            ! Diffuse:
            Tac_d(i) = 1.
         else
            ! At solar zenith angle:
            Tac_0(i) = RTM_Pc%SW%Tac(Solar(i)) ** &
                 SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
            ! At sensor viewing angle:
            Tac_v(i) = RTM_Pc%SW%Tac(Solar(i)) ** &
                 SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
            ! Diffuse:
            Tac_d(i) = RTM_Pc%SW%Tac(Solar(i)) ** dif_trans_fac
         end if

         ! Calculate below cloud (bc) beam transmittances.
         if (i_layer == 1) then
            ! In this case this is the second call of two-layer use called for
            ! upper layer where below cloud transmittances should not include
            ! the atmosphere below the lower layer.
            g = RTM_Pc%SW%Tbc(Solar(i)) / RTM_Pc2%SW%Tbc(Solar(i))

            ! At solar zenith angle:
            Tbc_0(i) = g ** &
                 SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
            ! At sensor viewing angle:
            Tbc_v(i) = g ** &
                 SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
            ! Diffuse:
            Tbc_d(i) = g ** dif_trans_fac
         else
            ! At solar zenith angle:
            Tbc_0(i) = RTM_Pc%SW%Tbc(Solar(i)) ** &
                 SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
            ! At sensor viewing angle:
            Tbc_v(i) = RTM_Pc%SW%Tbc(Solar(i)) ** &
                 SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
            ! Diffuse:
            Tbc_d(i) = RTM_Pc%SW%Tbc(Solar(i)) ** dif_trans_fac
         end if
      end do

      ! Calculate transmittance from TOA to cloud times transmittance from
      ! cloud to TOA.
      Tac_0v = Tac_0 * Tac_v
      Tac_0d = Tac_0 * Tac_d
      Tac_dv = Tac_d * Tac_v
      Tac_dd = Tac_d * Tac_d

      ! Calculate transmittances from cloud to surface times transmittance from
      ! surface to cloud.
      Tbc_0v = Tbc_0 * Tbc_v
      Tbc_0d = Tbc_0 * Tbc_d
      Tbc_dv = Tbc_d * Tbc_v
      Tbc_dd = Tbc_d * Tbc_d

      ! Calculate transmittances from TOA to surface times transmittance from
      ! surface to TOA.
      Tsf_0v = Tac_0 * Tbc_0 * Tac_v * Tbc_v
      Tsf_0d = Tac_0 * Tbc_0 * Tac_d * Tbc_d
      Tsf_dv = Tac_d * Tbc_d * Tac_v * Tbc_v
      Tsf_dd = Tac_d * Tbc_d * Tac_d * Tbc_d
   end if


   !----------------------------------------------------------------------------
   ! The old albedo only implementation
   !----------------------------------------------------------------------------
if (.not. Ctrl%RS%use_full_brdf) then
   ! Calculate auxillary quantities used for the reflectance and its
   ! derivatives

   a = 1.0 - Rs * CRP(:,IR_dd) * Tbc_dd

   b = T_all * Rs * CRP(:,IT_dv) * Tbc_dd / a

   ! Calculate overcast reflectance
   Ref_over = Tac_0v * (CRP(:,IR_0v) + b)

   ! Calculate top of atmosphere reflectance for fractional cloud cover
   Ref = X(IFrX) * Ref_over + (1.0-X(IFrX)) * SPixel%RTM%Ref_clear

   ! Derivative w.r.t. cloud optical depth, Tau
   call derivative_wrt_crp_parameter(ITauCRP, Rs, CRP, d_CRP, X(IFrX), Tac_0v, &
        Tbc_dd, T_all, a, d_Rs(:,ITauX), d_Ref(:,ITauX))

   ! Derivative w.r.t. effective radius, r_e
   call derivative_wrt_crp_parameter(IReCRP,  Rs, CRP, d_CRP, X(IFrX), Tac_0v, &
        Tbc_dd, T_all, a, d_Rs(:,IReX),  d_Ref(:,IReX))

   ! Derivative w.r.t. cloud-top pressure, P_c
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) then
      do i = 1, SPixel%Ind%NSolar
         d_Ref(i,IPcX) = X(IFrX) * &
            (1.0 * RTM_Pc%SW%dTac_dPc(Solar(i)) * &
                   (SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) + &
                    SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))) &
                 * Ref_over(i) / RTM_Pc%SW%Tac(Solar(i))) + &
            (2.0 * RTM_Pc%SW%dTbc_dPc(Solar(i)) * Tac_0v(i) * b(i) * &
                   RTM_Pc%SW%Tbc(Solar(i)) * &
                   (1.0/Tbc_dd(i) + CRP(i,IR_dd) * Rs(i) / a(i)))
      end do
   end if

   ! Derivative w.r.t. cloud fraction, f
   d_Ref(:,IFrX) = Ref_over - SPixel%RTM%Ref_clear

   ! Derivative w.r.t. surface temperature, T_s
!  d_Ref(:,ITs) = 0.0 ! Constant and zero

   ! Derivative w.r.t. surface reflectance, R_s
   do i = 1, SPixel%Ind%NSolar
      ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)
      d_Ref(i,IRs(ii,IRho_DD)) = ( &
           X(IFrX) * Tac_0v(i) * Tbc_dd(i) / a(i) * (T_all(i) * CRP(i,IT_dv) + &
                                                     b(i) * CRP(i,IR_dd)) + &
           (1.0-X(IFrX)) * SPixel%RTM%dRef_clear_dRs(i))
   end do

   if (Ctrl%Approach == AppAerSw) then
      ! Derivatives wrt Swansea surface parameters
      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

         d_Ref(i,SPixel%Surface%XIndex(i,ISwan_S)) = &
              d_Ref(i,IRs(ii,IRho_DD)) * d_Rs(i,ISv)
         d_Ref(i,SPixel%Surface%XIndex(i,ISwan_P)) = &
              d_Ref(i,IRs(ii,IRho_DD)) * d_Rs(i,IPv)
         d_Ref(i,ISG) = &
              d_Ref(i,IRs(ii,IRho_DD)) * d_Rs(i,IGv)
      end do
   end if

   !----------------------------------------------------------------------------
   ! The new BRDF implementation
   !----------------------------------------------------------------------------
else
   ! Calculate auxillary quantities used for the reflectance and its
   ! derivatives

   a = 1. - Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd

   b = CRP(:,IT_00) * Rs2(:,IRho_0D) * Tbc_0 + CRP(:,IT_0d) * &
          Rs2(:,IRho_DD) * Tbc_d

   ! Calculate overcast bidirectional reflectance
   !
   ! This reflectance should be sun normalized [scaled by cos(theta_0)].  The
   ! bidirectional reflectance of the cloud/aerosol layer CRP(:,IR_0v) is in the
   ! LUTs already sun normalized.  The rest of the reflectance and transmission
   ! operators from the LUTs are not sun normalized.  This can be corrected by
   ! multiplying the rest of the terms in these equations by cos(theta_0) (or
   ! dividing by sec(theta_0).  This is what is done for equations 1 and 3.
   ! Unfortunately, this was also done incorrectly in other versions of the code
   ! by instead pre-multiplying the surface reflectance terms by cos(theta_0),
   ! controlled by the switch Ctrl%RS%solar_factor, resulting in equations 2 and
   ! 4.  These equations should be avoided and will be removed in the future.

   ! Equations 1 and 2 are the traditional equations with the simplification
   ! that surface reflectance does not show any dependence on viewing angle
   ! under diffuse illumination.  This equation does not obey the principal of
   ! reciprocity.

   ! Equations 3 and 4 obey the principal of reciprocity and produce slightly
   ! better results.

   if (Ctrl%i_equation_form == 1) then
      c = CRP(:,IT_dv) * Tbc_d

      e = CRP(:,IR_0v) + &
         (CRP(:,IT_00) * (Rs2(:,IRho_0V) - Rs2(:,IRho_0D)) * &
             CRP(:,IT_vv) * Tbc_0v + &
          b * c / a) / SPixel%Geom%SEC_o(1) ! Only nadir zenith used
   else if (Ctrl%i_equation_form == 2) then
      c = CRP(:,IT_dv) * Tbc_d

      e = CRP(:,IR_0v) + &
          CRP(:,IT_00) * (Rs2(:,IRho_0V) - Rs2(:,IRho_0D)) * &
             CRP(:,IT_vv) * Tbc_0v + &
          b * c / a
   else if (Ctrl%i_equation_form == 3) then
      c = CRP(:,IT_dv) * Tbc_d + CRP(:,IR_dd) * Rs2(:,IRho_DV) * &
             CRP(:,IT_vv) * Tbc_dd * Tbc_v
      e = CRP(:,IR_0v) + &
         (CRP(:,IT_00) * Rs2(:,IRho_0V) * CRP(:,IT_vv) * Tbc_0v + &
          CRP(:,IT_0d) * Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dv + &
          b * c / a) / SPixel%Geom%SEC_o(1) ! Only nadir zenith used
   else
      c = CRP(:,IT_dv) * Tbc_d + CRP(:,IR_dd) * Rs2(:,IRho_DV) * &
             CRP(:,IT_vv) * Tbc_dd * Tbc_v

      e = CRP(:,IR_0v) + &
          CRP(:,IT_00) * Rs2(:,IRho_0V) * CRP(:,IT_vv) * Tbc_0v + &
          CRP(:,IT_0d) * Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dv + &
          b * c / a
   end if

   Ref_over = Tac_0v * e

   ! Calculate top of atmosphere bidirectional reflectance for fractional cloud
   ! cover
   if (i_layer == 0) then
      Ref = X(IFrX) * Ref_over + (1.0-X(IFrX)) * SPixel%RTM%Ref_clear
   else
      Ref = X(IFrX) * Ref_over + (1.0-X(IFrX)) * Rs2(:,IRho_0V) * Tsf_0v
   end if

   ! Calculate overcast directional-diffuse reflectance
   if (present(Ref_0d)) then
      if (Ctrl%i_equation_form == 1 .or. Ctrl%i_equation_form == 3) then
         c_0d = CRP(:,IT_dd) * Tbc_d
if (USE_REVISED_REF_0D) then
         e_0d = CRP(:,IR_0d) + b * c_0d / a
else
         e_0d = CRP(:,IR_0d) + &
            CRP(:,IT_00) * Rs2(:,IRho_0V) * CRP(:,IT_vd) * Tbc_0v + &
            b * c_0d / a
end if
         Ref_over_0d = Tac_0d * e_0d
      else
         write (*,*) 'ERROR: FM_Solar(), directional-diffuse reflectance not' // &
            ' supported with i_equation_form = ', Ctrl%i_equation_form
         stop error_stop_code
      end if

      ! Calculate top of atmosphere directional-diffuse reflectance for
      ! fractional cloud cover
      Ref_0d = X(IFrX) * Ref_over_0d + (1.0-X(IFrX)) * Rs2(:,IRho_0D) * Tsf_0d
   end if

   ! Calculate overcast diffuse-directional reflectance
   if (present(Ref_dv)) then
      if (Ctrl%i_equation_form == 1 .or. Ctrl%i_equation_form == 3) then
         c_dv = CRP(:,IT_dd) * Tbc_d

         d_dv = Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_v + &
                Rs2(:,IRho_DD) * CRP(:,IT_dv) * Tbc_d

         e_dv = CRP(:,IR_dv) + c_dv * d_dv / a

         Ref_over_dv = Tac_0d * e_dv
      else
         write (*,*) 'ERROR: FM_Solar(), diffuse-directional reflectance not' // &
            ' supported with i_equation_form = ', Ctrl%i_equation_form
         stop error_stop_code
      end if

      ! Calculate top of atmosphere diffuse-directional reflectance for
      ! fractional cloud cover
      Ref_dv = X(IFrX) * Ref_over_dv + (1.0-X(IFrX)) * Rs2(:,IRho_DV) * Tsf_dv
   end if

   ! Calculate overcast diffuse-diffuse reflectance
   if (present(Ref_dd)) then
      if (Ctrl%i_equation_form == 1 .or. Ctrl%i_equation_form == 3) then
         e_dd = CRP(:,IR_dd) + &
            (CRP(:,IT_dd)**2 * Rs2(:,IRho_dd) * Tbc_dd) / a

         Ref_over_dd = Tac_dd * e_dd
      else
         write (*,*) 'ERROR: FM_Solar(), diffuse-diffuse reflectance not' // &
            ' supported with i_equation_form = ', Ctrl%i_equation_form
         stop error_stop_code
      end if

      ! Calculate top of atmosphere diffuse-diffuse reflectance for fractional
      ! cloud cover
      Ref_dd = X(IFrX) * Ref_over_dd + (1.0-X(IFrX)) * Rs2(:,IRho_DD) * Tsf_dd
   end if

   ! Calculate derivatives of reflectance w.r.t. all other variables or part
   ! cloudy conditions

   ! Derivative of bidirectional reflectance w.r.t. cloud optical depth
   call derivative_wrt_crp_parameter_brdf_0v(SPixel, ITauCRP, &
        Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_0v, Tbc_0, Tbc_v, &
        Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref(:,ITauX), a, b, c)

   ! Derivative of bidirectional reflectance w.r.t. effective radius
   call derivative_wrt_crp_parameter_brdf_0v(SPixel, IReCRP, &
        Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_0v, Tbc_0, Tbc_v, &
        Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref(:,IReX), a, b, c)

   if (present(d_Ref_0d)) then
      ! Derivative of directional-diffuse reflectance w.r.t. cloud optical depth
      call derivative_wrt_crp_parameter_brdf_0d(SPixel, ITauCRP, &
           Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_0d, Tbc_0, Tbc_v, &
           Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref_0d(:,ITauX), &
           a, b, c_0d)

      ! Derivative of directional-diffuse reflectance w.r.t. effective radius
      call derivative_wrt_crp_parameter_brdf_0d(SPixel, IReCRP, &
           Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_0d, Tbc_0, Tbc_v, &
           Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref_0d(:,IReX), &
           a, b, c_0d)
   end if

   if (present(d_Ref_dv)) then
      ! Derivative of diffuse-directional reflectance w.r.t. cloud optical depth
      call derivative_wrt_crp_parameter_brdf_dv(SPixel, ITauCRP, &
           Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_dv, Tbc_0, Tbc_v, &
           Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref_dv(:,ITauX), &
           a, b, c_dv, d_dv)

      ! Derivative of diffuse-directional reflectance w.r.t. effective radius
      call derivative_wrt_crp_parameter_brdf_dv(SPixel, IReCRP, &
           Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_dv, Tbc_0, Tbc_v, &
           Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref_dv(:,IReX), &
           a, b, c_dv, d_dv)
   end if

   if (present(d_Ref_dd)) then
      ! Derivative of diffuse-diffuse reflectance w.r.t. cloud optical depth
      call derivative_wrt_crp_parameter_brdf_dd(SPixel, ITauCRP, &
           Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_dd, Tbc_0, Tbc_v, &
           Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref_dd(:,ITauX), a, b)

      ! Derivative of diffuse-diffuse reflectance w.r.t. effective radius
      call derivative_wrt_crp_parameter_brdf_dd(SPixel, IReCRP, &
           Ctrl%i_equation_form, CRP, d_CRP, X(IFrX), Tac_dd, Tbc_0, Tbc_v, &
           Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, Rs2, d_Ref_dd(:,IReX), a, b)
   end if

   ! Calculate the derivatives of the above cloud (ac) beam transmittances
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) then
      do i = 1, SPixel%Ind%NSolar
         if (i_layer == 2) then
            ! Above cloud at solar zenith angle:
            Tac_0_l(i) = 0.
            ! Above cloud at viewing angle:
            Tac_v_l(i) = 0.
         else
            ! Above cloud at solar zenith angle:
            Tac_0_l(i) = RTM_Pc%SW%dTac_dPc(Solar(i)) * &
               SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
               RTM_Pc%SW%Tac(Solar(i)) ** &
                  (SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
            ! Above cloud at viewing angle:
            Tac_v_l(i) = RTM_Pc%SW%dTac_dPc(Solar(i)) * &
               SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
               RTM_Pc%SW%Tac(Solar(i)) ** &
                  (SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
         end if
      end do

      ! Calculate the derivative of the solar transmittance from TOA to cloud-
      ! top times the viewing transmittance from cloud-top to TOA
      Tac_0v_l = Tac_0_l * Tac_v + Tac_0 * Tac_v_l

      ! Calculate the derivatives of the below cloud (bc) beam transmittances
      do i = 1, SPixel%Ind%NSolar
         if (i_layer == 1) then
            h = RTM_Pc%SW%dTbc_dPc(Solar(i)) / RTM_Pc2%SW%Tbc(Solar(i))

            ! At solar zenith angle:
            Tbc_0_l(i) = h * &
               SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
               g ** (SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
            ! At sensor viewing angle:
            Tbc_v_l(i) = h * &
               SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
               g ** (SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
            ! Diffuse
            Tbc_d_l(i) = h * dif_trans_fac * g ** (dif_trans_fac - 1.)
         else
            ! At solar zenith angle:
            Tbc_0_l(i) = RTM_Pc%SW%dTbc_dPc(Solar(i)) * &
               SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
               RTM_Pc%SW%Tbc(Solar(i)) ** &
                    (SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
            ! At sensor viewing angle:
            Tbc_v_l(i) = RTM_Pc%SW%dTbc_dPc(Solar(i)) * &
               SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
               RTM_Pc%SW%Tbc(Solar(i)) ** &
                    (SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
            ! Diffuse
            Tbc_d_l(i) = RTM_Pc%SW%dTbc_dPc(Solar(i)) * dif_trans_fac * &
               RTM_Pc%SW%Tbc(i) ** (dif_trans_fac - 1.)
         end if
      end do

      ! Calculate the derivative of the diffuse transmittance from cloud to
      ! surface times transmittance from surface to cloud
      Tbc_dd_l = 2. * Tbc_d_l * Tbc_d

      Tsf_0v_l = Tac_0_l * Tbc_0   * Tac_v   * Tbc_v + &
                 Tac_0   * Tbc_0_l * Tac_v   * Tbc_v + &
                 Tac_0   * Tbc_0   * Tac_v_l * Tbc_v + &
                 Tac_0   * Tbc_0   * Tac_v   * Tbc_v_l

      ! Derivative of bidirectionals reflectance w.r.t. cloud-top pressure, P_c
      if (Ctrl%i_equation_form == 1) then
         e_l = (CRP(:,IT_00) * (Rs2(:,IRho_0V) - Rs2(:,IRho_0D)) * &
                  CRP(:,IT_vv) * (Tbc_0_l * Tbc_v + Tbc_0 * Tbc_v_l) + &
               ((CRP(:,IT_00) * Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
                  Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (                   &
                  CRP(:,IT_dv) * Tbc_d_l)) / a + &
               b * c * Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)) / &
               SPixel%Geom%SEC_o(1)
      else if (Ctrl%i_equation_form == 2) then
         e_l = CRP(:,IT_00) * (Rs2(:,IRho_0V) - Rs2(:,IRho_0D)) * &
                  CRP(:,IT_vv) * (Tbc_0_l * Tbc_v + Tbc_0 * Tbc_v_l) + &
               ((CRP(:,IT_00) * Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
                  Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (                   &
                  CRP(:,IT_dv) * Tbc_d_l)) / a + &
               b * c * Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)
      else if (Ctrl%i_equation_form == 3) then
         e_l = (CRP(:,IT_00) * Rs2(:,IRho_0V) * CRP(:,IT_vv) * (Tbc_0_l * &
                  Tbc_v + Tbc_0 * Tbc_v_l) + &
                CRP(:,IT_0d) * Rs2(:,IRho_DV) * CRP(:,IT_vv) * (Tbc_d_l * &
                  Tbc_v + Tbc_d * Tbc_v_l) + &
               ((CRP(:,IT_00) * Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
                  Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (CRP(:,IT_dv) * &
                  Tbc_d_l + CRP(:,IR_dd) * Rs2(:,IRho_DV) * CRP(:,IT_vv) * &
                  Tbc_dd_l * Tbc_v + CRP(:,IR_dd) * Rs2(:,IRho_DV) * &
                  CRP(:,IT_vv) * Tbc_dd * Tbc_v_l)) / a + &
               b * c * Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)) / &
               SPixel%Geom%SEC_o(1)
      else
         e_l = CRP(:,IT_00) * Rs2(:,IRho_0V) * CRP(:,IT_vv) * (Tbc_0_l * &
                  Tbc_v + Tbc_0 * Tbc_v_l) + &
               CRP(:,IT_0d) * Rs2(:,IRho_DV) * CRP(:,IT_vv) * (Tbc_d_l * &
                  Tbc_v + Tbc_d * Tbc_v_l) + &
               ((CRP(:,IT_00) * Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
                  Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (CRP(:,IT_dv) * &
                  Tbc_d_l + CRP(:,IR_dd) * Rs2(:,IRho_DV) * CRP(:,IT_vv) * &
                  Tbc_dd_l * Tbc_v + CRP(:,IR_dd) * Rs2(:,IRho_DV) * &
                  CRP(:,IT_vv) * Tbc_dd * Tbc_v_l)) / a + &
               b * c * Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)
      end if

      Ref_over_l = Tac_0v_l * e + Tac_0v * e_l

      if (i_layer /= 2) then
            d_Ref(:,IPcX) = X(IFrX) * Ref_over_l
      else
            ! In the case of the lower layer in the two-layer case the clear-sky
            ! reflectance is only for the lower layer down and is therefore dep-
            ! endent on the cloud-top pressure, P_c of the lower layer.
            d_Ref(:,IPcX) = X(IFrX) * Ref_over_l + &
                            (1.0-X(IFrX)) * Rs2(:,IRho_0V) * Tsf_0v_l
      end if

      if (present(d_Ref_0d)) then
         ! Derivative of directional-diffuse reflectance w.r.t. cloud-top
         ! pressure, P_c
         if (Ctrl%i_equation_form == 1 .or. Ctrl%i_equation_form == 3) then
if (USE_REVISED_REF_0D) then
            e_l = ((CRP(:,IT_00) * Rs2(:,IRho_0D) * Tbc_0_l + &
                    CRP(:,IT_0d) * Rs2(:,IRho_DD) * Tbc_d_l) * c_0d + &
                   b * CRP(:,IT_dd) * Tbc_d_l) / a + &
                  b * c_0d * Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)
else
            e_l = CRP(:,IT_00) * Rs2(:,IRho_0V) * CRP(:,IT_vd) * &
                  (Tbc_0_l * Tbc_v + Tbc_0 * Tbc_v_l) + &
                  ((CRP(:,IT_00) * Rs2(:,IRho_0D) * Tbc_0_l + &
                    CRP(:,IT_0d) * Rs2(:,IRho_DD) * Tbc_d_l) * c_0d + &
                   b * CRP(:,IT_dd) * Tbc_d_l) / a + &
                  b * c_0d * Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)
end if
         else
            write (*,*) 'ERROR: FM_Solar(), directional-diffuse reflectance not' // &
               ' supported with i_equation_form = ', Ctrl%i_equation_form
            stop error_stop_code
         end if

         Ref_over_l = Tac_0v_l * e_0d + Tac_0v * e_l

         d_Ref_0d(:,IPcX) = X(IFrX) * Ref_over_l
      end if

      if (present(d_Ref_dv)) then
         ! Derivative of diffuse-directional reflectance w.r.t. cloud-top
         ! pressure, P_c
         if (Ctrl%i_equation_form == 1 .or. Ctrl%i_equation_form == 3) then
            e_l = (CRP(:,IT_dd) * Tbc_d_l * d_dv + &
                   c_dv * (Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_v_l + &
                           Rs2(:,IRho_DD) * CRP(:,IT_dv) * Tbc_d_l)) / a + &
                  c_dv * d_dv * Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)
         else
            write (*,*) 'ERROR: FM_Solar(), diffuse-diffuse reflectance not' // &
               ' supported with i_equation_form = ', Ctrl%i_equation_form
            stop error_stop_code
         end if

         Ref_over_l = Tac_0v_l * e_dv + Tac_0v * e_l

         d_Ref_dv(:,IPcX) = X(IFrX) * Ref_over_l
      end if

      if (present(d_Ref_dd)) then
         ! Derivative of diffuse-diffuse reflectance w.r.t. cloud-top
         ! pressure, P_c
         if (Ctrl%i_equation_form == 1 .or. Ctrl%i_equation_form == 3) then
            e_l = CRP(:,IT_dd)**2 * Rs2(:,IRho_dd) * Tbc_dd_l / a + &
                 (CRP(:,IT_dd)**2 * Rs2(:,IRho_dd) * Tbc_dd * &
                  Rs2(:,IRho_dd) * CRP(:,IR_dd) * Tbc_dd_l) / (a*a)
         else
            write (*,*) 'ERROR: FM_Solar(), diffuse-diffuse reflectance not' // &
               ' supported with i_equation_form = ', Ctrl%i_equation_form
            stop error_stop_code
         end if

         Ref_over_l = Tac_0v_l * e_dd + Tac_0v * e_l

         d_Ref_dd(:,IPcX) = X(IFrX) * Ref_over_l
      end if
   end if

   ! Derivative w.r.t. cloud fraction, f
   if (i_layer == 0) then
      d_Ref(:,IFrX) = Ref_over - SPixel%RTM%Ref_clear
   else
         d_Ref(:,IFrX) = Ref_over - Rs2(:,IRho_0V) * Tsf_0v
         if (present(d_Ref_0d)) &
            d_Ref_0d(:,IFrX) = Ref_over_0d - Rs2(:,IRho_0D) * Tsf_0d
         if (present(d_Ref_dv)) &
            d_Ref_dv(:,IFrX) = Ref_over_dv - Rs2(:,IRho_DV) * Tsf_dv
         if (present(d_Ref_dd)) &
            d_Ref_dd(:,IFrX) = Ref_over_dd - Rs2(:,IRho_DD) * Tsf_dd
   end if

   ! Derivative w.r.t. surface temperature, T_s (no dependence)
   d_Ref(:,ITs) = 0.0
   if (present(d_Ref_0d)) d_Ref_0d(:,ITs) = 0.0
   if (present(d_Ref_dv)) d_Ref_dv(:,ITs) = 0.0
   if (present(d_Ref_dd)) d_Ref_dd(:,ITs) = 0.0

   ! Derivative w.r.t. BRDF reflectance parameters, rho_xx
   if (i_layer /= 1) then
      if (Ctrl%Approach == AppAerSw) then
         rho_l = 0.0

         ! Derivative wrt P
         do j = 1, MaxNumViews
            do i = 1, SPixel%Ind%NSolar
               if (SPixel%Surface%XIndex(i,ISwan_P) == ISP(j)) then
                  rho_l(i,IRho_0V) = Ss(i)
               else
                  rho_l(i,IRho_0V) = 0.0
               end if
            end do
            call derivative_wrt_rho_parameters_brdf(SPixel, &
                 Ctrl%i_equation_form, CRP, X(IFrX), Tac_0v, Tbc_0, Tbc_v, &
                 Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, rho_l, &
                 d_Ref(:,ISP(j)), a, b, c)
         end do

         ! Derivative wrt S
         do j = 1, SPixel%Ind%NSolar
            do i = 1, SPixel%Ind%NSolar
               if (SPixel%Surface%XIndex(i,ISwan_S) == ISS(j)) then
                  rho_l(i,IRho_0V) = Sp(i)
                  rho_l(i,IRho_0D) = Sg * (1.0 - Sg) * Ss(i) * &
                       (2.0 - (1.0 - Sg) * Ss(i)) / (Sa(i) * Sa(i))
                  rho_l(i,IRho_DD) = Sg / (Sa(i) * Sa(i))
               else
                  rho_l(i,:) = 0.0
               end if
            end do
            call derivative_wrt_rho_parameters_brdf(SPixel, &
                 Ctrl%i_equation_form, CRP, X(IFrX), Tac_0v, Tbc_0, Tbc_v, &
                 Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, rho_l, &
                 d_Ref(:,ISS(j)), a, b, c)
         end do

         ! Derivative wrt G
         rho_l(:,IRho_0V) = 0.0
         rho_l(:,IRho_0D) = Ss * Ss * (1.0 - Sg * (2.0 + Ss - Sg * Ss)) / &
              (Sa * Sa)
         rho_l(:,IRho_DD) = Ss * (1.0 - Ss) / (Sa * Sa)
         call derivative_wrt_rho_parameters_brdf(SPixel, &
              Ctrl%i_equation_form, CRP, X(IFrX), Tac_0v, Tbc_0, Tbc_v, &
              Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, rho_l, &
              d_Ref(:,ISG), a, b, c)
      else
         do j = MaxRho_XX, MaxRho_XX ! NOTE: Only evaluate Rho_DD for now
            do i = 1, SPixel%Ind%NSolar
               ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

               ! Set rho_l for all BRDF terms proportional to this state vector
               ! element
               where (SPixel%Surface%XIndex == IRs(ii,j))
                  rho_l = SPixel%Surface%Ratios
               else where
                  rho_l = 0.0
               end where

               ! If no dependence, skip this derivative
               if (sum(rho_l) == 0.0) cycle

               call derivative_wrt_rho_parameters_brdf(SPixel, &
                    Ctrl%i_equation_form, &
                    CRP, X(IFrX), Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, &
                    Tbc_dv, Tbc_dd, rho_l, d_Ref(:,IRs(ii,j)), a, b, c)

               if (present(d_Ref_0d)) d_Ref_0d(:,IRs(ii,j)) = 0.0
               if (present(d_Ref_dv)) d_Ref_dv(:,IRs(ii,j)) = 0.0
               if (present(d_Ref_dd)) d_Ref_dd(:,IRs(ii,j)) = 0.0
            end do
         end do
      end if
   end if

   ! Optionally provide derivatives w.r.t surface BRDF parameters useful in the
   ! two-layer case to propagate derivatives w.r.t lower layer parameters and
   ! surface parameters through the upper layer.
   if (present(d_Ref_dRs2)) then
      do j = 1, MaxRho_XX
         rho_l      = 0.
         rho_l(:,j) = 1.

         call derivative_wrt_rho_parameters_brdf(SPixel, Ctrl%i_equation_form, &
              CRP, X(IFrX), Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, &
              Tbc_dv, Tbc_dd, rho_l, d_Ref_dRs2(:,j), a, b, c)
      end do
   end if
end if

   ! Account for solar factor scaling applied in GetSurface()
   if (Ctrl%RS%solar_factor) then
      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)
         d_Ref(i,IRs(ii,IRho_DD)) = d_Ref(i,IRs(ii,IRho_DD)) / &
              SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
      end do
   end if

end subroutine FM_Solar
