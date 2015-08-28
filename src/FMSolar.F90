!-------------------------------------------------------------------------------
! Name: FMSolar.F90
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
! Name    Type        In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl    struct      In          Control structure
! SAD_LUT struct      In          SAD look up table
! SPixel  struct      In          Super-pixel structure
! RTM_Pc  struct      In          Contains transmittances, radiances and their
!                                 derivatives
! X       real array  In          State vector
! GZero   struct      In          "Zero'th point" grid info for SAD_LUT CRP
!                                 array interpolation.
! CRP     float array In          Interpolated cloud radiative properties
!                                 (calculated by SetCRPSolar, but an input
!                                 argument because CRP for Td for the mixed
!                                 channels is set by SetCRPThermal. Only the
!                                 solar channels should be passed by the
!                                 calling routine).
! d_CRP   float array In          Grads. of interpolated cloud rad. properties
!                                 (see comment for CRP).
! REF     float array Out         TOA reflectances for part cloudy conditions
! d_REF   float array Out         Derivatives d[ref]/d[tau,Re,pc,f,Ts,Rs]t
! status  int         Out         Status from Set_CRP_Solar/breakpoint file
!                                 open
!
! History:
! 2000/11/07, KS: original version
! 2000/11/21, KS: added X structure (state vector)
! 2001/01/17, AS: Changed indexing of CRP arrays to use constants to reference
!    the different LUT values (IRBd etc) Using FM_Routines_def: contains
!    interface definition for SetCRPSolar.
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
! 2014/12/11, GM: Some small mathmatical bug fixes in the BRDF derivative
!    equations, mathematical refactoring to reduce computation, and some
!    cleanup.
! 2015/01/09, AP: Eliminate write to RTM_Pc%Tac, Tbc.
! 2015/01/15, AP: Facilitate channel indexing in arbitrary order.
! 2015/01/21, AP: Finishing the last commit.
! 2015/07/31, GM: Remove i_equation 2 as an invalid derivation.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine derivative_wrt_crp_parameter(SPixel, i_param, CRP, d_CRP, f, Tac_0v, &
   Tbc2, T_all, S, S_dnom, Sp, d_REF)

   use SPixel_def

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_param
   real,           intent(in)  :: CRP(:,:)
   real,           intent(in)  :: d_CRP(:,:,:)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_0v(:)
   real,           intent(in)  :: Tbc2(:)
   real,           intent(in)  :: Sp(:)
   real,           intent(in)  :: T_all(:)
   real,           intent(in)  :: S(:)
   real,           intent(in)  :: S_dnom(:)
   real,           intent(out) :: d_REF(:,:)

   d_REF(:,i_param) = f * Tac_0v * &
      (d_CRP(:,IRBd,i_param) + &
      Sp * SPixel%Surface%Rs * &
      (T_all * d_CRP(:,ITd,i_param) + &
      CRP(:,ITd) * (d_CRP(:,ITB,i_param) + d_CRP(:,ITFBd,i_param)) &
      ) + &
      S * SPixel%Surface%Rs * Tbc2 * d_CRP(:,IRFd,i_param) / S_dnom)

end subroutine derivative_wrt_crp_parameter



subroutine derivative_wrt_crp_parameter_brdf(SPixel, i_param, i_equation_form, &
   CRP, d_CRP, f, Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, &
   Rs2, d_REF, a, b, c)

   use SPixel_def

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_param
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
   real,           intent(out) :: d_REF(SPixel%Ind%NSolar)
   real,           intent(in)  :: a(:)
   real,           intent(in)  :: b(:)
   real,           intent(in)  :: c(:)

   real :: d_l(SPixel%Ind%NSolar)
   real :: REF_over_l(SPixel%Ind%NSolar)

   if (i_equation_form .eq. 1) then
      d_l = d_CRP(:,IR_0v,i_param) + &
            (d_CRP(:,IT_00,i_param) * (Rs2(:,IRho_0V) - &
               Rs2(:,IRho_DD)) * CRP(:,IT_dv) + CRP(:,IT_00) * &
               (Rs2(:,IRho_0V) - Rs2(:,IRho_DD)) * &
               d_CRP(:,IT_dv,i_param)) * Tbc_0d + &
            ((d_CRP(:,IT_00,i_param) * Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_param) * Rs2(:,IRho_DD) * Tbc_d) * c + b * &
               d_CRP(:,IT_dv,i_param) * Tbc_d)  / a + &
            b * c  * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_param) * Tbc_dd / (a*a)
   else
      d_l = d_CRP(:,IR_0v,i_param) + &
            (d_CRP(:,IT_00,i_param) * Rs2(:,IRho_0V) * CRP(:,IT_vv) + &
               CRP(:,IT_00) * Rs2(:,IRho_0V) * d_CRP(:,IT_vv,i_param)) * &
               Tbc_0v + &
            (d_CRP(:,IT_0d,i_param) * Rs2(:,IRho_DV) * CRP(:,IT_vv) + &
               CRP(:,IT_0d) * Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_param)) * &
               Tbc_dv + &
            ((d_CRP(:,IT_00,i_param) * Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_param) * Rs2(:,IRho_DD) * Tbc_d) * c + &
               b * (d_CRP(:,IT_dv,i_param) * Tbc_d + d_CRP(:,IR_dd,i_param) * &
               Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dd * Tbc_v + &
               CRP(:,IR_dd) * Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_param) * &
               Tbc_dd * Tbc_v)) / a + &
            b * c * Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_param) * Tbc_dd / (a*a)
   end if

   REF_over_l = Tac_0v * d_l

   d_REF = f * REF_over_l

end subroutine derivative_wrt_crp_parameter_brdf


subroutine FM_Solar(Ctrl, SAD_LUT, SPixel, RTM_Pc, X, GZero, CRP, d_CRP, REF, &
                    d_REF, status)

   use CTRL_def
   use ECP_Constants
   use GZero_def
   use RTM_Pc_def
   use SAD_LUT_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),    intent(in)    :: Ctrl
   type(SAD_LUT_t), intent(in)    :: SAD_LUT
   type(SPixel_t),  intent(in)    :: SPixel
   type(RTM_Pc_t),  intent(inout) :: RTM_Pc
   real,            intent(in)    :: X(:)
   type(GZero_t),   intent(in)    :: GZero
   real,            intent(inout) :: CRP(:,:)
   real,            intent(inout) :: d_CRP(:,:,:)
   real,            intent(out)   :: REF(:)
   real,            intent(out)   :: d_REF(:,:)
   integer,         intent(out)   :: status
   ! Referencing of different properties stored in CRP and d_CRP: the last index
   ! of the CRP array refers to the property. Hence ITB is the index of TB, etc.

   ! Define local variables
   integer, parameter :: i_equation_form = 1

   integer :: i, ii
   integer :: Solar(SPixel%Ind%NSolar)
   real    :: a(SPixel%Ind%NSolar)
   real    :: b(SPixel%Ind%NSolar)
   real    :: c(SPixel%Ind%NSolar)
   real    :: d(SPixel%Ind%NSolar)

   real    :: Tac_0(SPixel%Ind%NSolar)
   real    :: Tac_v(SPixel%Ind%NSolar)
   real    :: Tac_0v(SPixel%Ind%NSolar)
   real    :: Tbc_0(SPixel%Ind%NSolar)
   real    :: Tbc_v(SPixel%Ind%NSolar)
   real    :: Tbc_d(SPixel%Ind%NSolar)
   real    :: Tbc_0v(SPixel%Ind%NSolar)
   real    :: Tbc_0d(SPixel%Ind%NSolar)
   real    :: Tbc_dv(SPixel%Ind%NSolar)
   real    :: Tbc_dd(SPixel%Ind%NSolar)

   real    :: REF_over(SPixel%Ind%NSolar)

   real    :: T_all(SPixel%Ind%NSolar)
   real    :: S_dnom(SPixel%Ind%NSolar)
   real    :: S(SPixel%Ind%NSolar)

   real    :: Sp(SPixel%Ind%NSolar)
   real    :: TBTD(SPixel%Ind%NSolar)

   real    :: d_l(SPixel%Ind%NSolar)

   real    :: Tac_0_l(SPixel%Ind%NSolar)
   real    :: Tac_v_l(SPixel%Ind%NSolar)
   real    :: Tac_0v_l(SPixel%Ind%NSolar)
   real    :: Tbc_0_l(SPixel%Ind%NSolar)
   real    :: Tbc_v_l(SPixel%Ind%NSolar)
   real    :: Tbc_d_l(SPixel%Ind%NSolar)
   real    :: Tbc_dd_l(SPixel%Ind%NSolar)
   real    :: rho_0v_l(SPixel%Ind%NSolar)
   real    :: rho_0d_l(SPixel%Ind%NSolar)
   real    :: rho_dv_l(SPixel%Ind%NSolar)
   real    :: rho_dd_l(SPixel%Ind%NSolar)

   real    :: REF_over_l(SPixel%Ind%NSolar)
#ifdef BKP
   integer :: j       ! For breakpoint output loops
   integer :: bkp_lun ! Unit number for breakpoint file
   integer :: ios     ! I/O status for breakpoint file
#endif

   status = 0
   d_REF = 0

   ! Subscripts for solar channels in RTM arrays
   Solar = SPixel%spixel_y_solar_to_ctrl_y_solar_index(:SPixel%Ind%NSolar)

   ! Interpolate cloud radiative property LUT data to the current Tau, Re values.
   call Set_CRP_Solar(Ctrl, SPixel%Ind, SPixel%spixel_y_solar_to_ctrl_y_index, &
        GZero, SAD_LUT, CRP, d_CRP, status)


   if (Ctrl%RTMIntSelm == RTMIntMethNone) then
      ! Above/below cloud transmittances are 1 for infinite extent cloud/aerosol
      Tac_0  = 1.0
      Tac_v  = 1.0
      Tac_0v = 1.0
      Tbc_0  = 1.0
      Tbc_v  = 1.0
      Tbc_d  = 1.0
      Tbc_0v = 1.0
      Tbc_0d = 1.0
      Tbc_dv = 1.0
      Tbc_dd = 1.0
   else
      do i = 1, SPixel%Ind%NSolar
         ! Calculate above cloud (ac) beam transmittances
         ! At solar zenith angle:
         Tac_0(i) = RTM_Pc%SW%Tac(Solar(i)) ** &
              SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
         ! At sensor viewing angle:
         Tac_v(i) = RTM_Pc%SW%Tac(Solar(i)) ** &
              SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))

         ! Calculate below cloud (bc) beam transmittances
         ! At solar zenith angle:
         Tbc_0(i) = RTM_Pc%SW%Tbc(Solar(i)) ** &
              SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
         ! At sensor viewing angle:
         Tbc_v(i) = RTM_Pc%SW%Tbc(Solar(i)) ** &
              SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))

         ! Calculate below cloud (bc) diffuse transmittances
         Tbc_d(i) = RTM_Pc%SW%Tbc(Solar(i)) ! ** (1. / cos(66. * d2r))
      end do

      ! Calculate solar transmittance from TOA to cloud-top times the viewing
      ! transmittance from cloud-top to TOA
      Tac_0v = Tac_0 * Tac_v

      ! Calculate transmittance from cloud to surface times transmittance from
      ! surface to cloud
      Tbc_0v = Tbc_0 * Tbc_v
      Tbc_0d = Tbc_0 * Tbc_d
      Tbc_dv = Tbc_d * Tbc_v
      Tbc_dd = Tbc_d * Tbc_d
   end if


   !----------------------------------------------------------------------------
   ! The old albedo only implementation
   !----------------------------------------------------------------------------
if (.not. Ctrl%RS%use_full_brdf) then
   ! Calculate auxillary quantitities used for the reflectance and its
   ! derivatives

   ! Sum of direct and diffuse beam transmissions
   T_all = (CRP(:,ITB) + CRP(:,ITFBd))

   S_dnom = 1.0 - (SPixel%Surface%Rs * CRP(:,IRFd) * Tbc_dd)

   S = (T_all * SPixel%Surface%Rs * CRP(:,ITd) * Tbc_dd) / S_dnom

   ! Calculate overcast reflectance
   REF_over = Tac_0v * (CRP(:,IRBd) + S)

   ! Calculate top of atmosphere reflectance for fractional cloud cover
   REF = X(IFr) * REF_over + (1.0-X(IFr)) * SPixel%RTM%REF_clear

   ! Calculate auxillary quantitities used for reflectance derivatives
   Sp = Tbc_dd / S_dnom

   TBTD = T_all * CRP(:,ITd)

   ! Calculate derivatives of reflectance w.r.t. all other variables or part
   ! cloudy conditions

   ! Derivative w.r.t. cloud optical depth, Tau
   call derivative_wrt_crp_parameter(SPixel, ITau, CRP, d_CRP, X(IFr), Tac_0v, &
      Tbc_dd, T_all, S, S_dnom, Sp, d_REF)

   ! Derivative w.r.t. effective radius, r_e
   call derivative_wrt_crp_parameter(SPixel, IRe,  CRP, d_CRP, X(IFr), Tac_0v, &
      Tbc_dd, T_all, S, S_dnom, Sp, d_REF)

   ! Derivative w.r.t. cloud-top pressure, P_c
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) then
      do i=1, SPixel%Ind%NSolar
         d_REF(i,IPc) = X(IFr) * &
            (1.0 * RTM_Pc%SW%dTac_dPc(Solar(i)) * &
                   (SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) + &
                    SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))) &
                 * REF_over(i) / RTM_Pc%SW%Tac(Solar(i))) + &
            (2.0 * RTM_Pc%SW%dTbc_dPc(Solar(i)) * Tac_0v(i) * S(i) * &
                   RTM_Pc%SW%Tbc(Solar(i)) * &
                   (1.0/Tbc_dd(i) + CRP(i,IRFd) * SPixel%Surface%Rs(i) / S_dnom(i)))
      end do
   end if

   ! Derivative w.r.t. cloud fraction, f
   d_REF(:,IFr) = (REF_over - SPixel%RTM%REF_clear)

   ! Derivative w.r.t. surface temperature, T_s
!  d_REF(:,ITs) = 0.0 ! Constant and zero

   ! Derivative w.r.t. surface reflectance, R_s
   do i=1, SPixel%Ind%NSolar
      ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)
      d_REF(i,IRs(ii,IRho_DD)) = &
         (X(IFr) * Tac_0v(i) * &
             (Sp(i) * TBTD(i) + (S(i) * CRP(i,IRFd) * Tbc_dd(i) / S_dnom(i))) + &
             (SPixel%RTM%dREF_clear_dRs(i) * (1.0-X(IFr)))) / &
         SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
   end do


   !----------------------------------------------------------------------------
   ! The new BRDF implementation
   !----------------------------------------------------------------------------
else
   ! Calculate auxillary quantitities used for the reflectance and its
   ! derivatives

   a = 1. - SPixel%Surface%Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd

   b = CRP(:,IT_00) * SPixel%Surface%Rs2(:,IRho_0D) * Tbc_0 + CRP(:,IT_0d) * &
          SPixel%Surface%Rs2(:,IRho_DD) * Tbc_d

   ! Calculate overcast reflectance
   if (i_equation_form .eq. 1) then ! Traditional ORAC forward model
      c = CRP(:,IT_dv) * Tbc_d

      d = CRP(:,IR_0v) + &
          CRP(:,IT_00) * (SPixel%Surface%Rs2(:,IRho_0V) - SPixel%Surface%Rs2(:,IRho_DD)) * &
             CRP(:,IT_dv) * Tbc_0d + &
          b * c / a

      REF_over = Tac_0v * d
   else ! GT's reciprocity-obeying forward model
      c = CRP(:,IT_dv) * Tbc_d + CRP(:,IR_dd) * SPixel%Surface%Rs2(:,IRho_DV) * &
             CRP(:,IT_vv) * Tbc_dd * Tbc_v

      d = CRP(:,IR_0v) + &
          CRP(:,IT_00) * SPixel%Surface%Rs2(:,IRho_0V) * CRP(:,IT_vv) * Tbc_0v + &
          CRP(:,IT_0d) * SPixel%Surface%Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dv + &
          b * c / a

      REF_over = Tac_0v * d
   end if

   ! Calculate top of atmosphere reflectance for fractional cloud cover
   REF = X(IFr) * REF_over + (1.0-X(IFr)) * SPixel%RTM%REF_clear

   ! Calculate derivatives of reflectance w.r.t. all other variables or part
   ! cloudy conditions

   ! Derivative w.r.t. cloud optical depth, Tau
   call derivative_wrt_crp_parameter_brdf(SPixel, ITau, i_equation_form, CRP, &
      d_CRP, X(IFr), Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, &
      Tbc_dd, SPixel%Surface%Rs2, d_REF(:,ITau), a, b, c)

   ! Derivative w.r.t. effective radius, r_e
   call derivative_wrt_crp_parameter_brdf(SPixel, IRe,  i_equation_form, CRP, &
      d_CRP, X(IFr), Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, &
      Tbc_dd, SPixel%Surface%Rs2, d_REF(:,IRe), a, b, c)

   ! Calculate the derivatives of the above cloud (ac) beam transmittances
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) then
      do i = 1, SPixel%Ind%NSolar
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
      end do

      ! Calculate the derivative of the solar transmittance from TOA to cloud-top
      ! times the viewing transmittance from cloud-top to TOA
      Tac_0v_l = Tac_0_l * Tac_v + Tac_0 * Tac_v_l

      ! Calculate the derivatives of the below cloud (bc) beam transmittances
      do i = 1, SPixel%Ind%NSolar
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
         Tbc_d_l(i) = RTM_Pc%SW%dTbc_dPc(Solar(i)) ! &
!        * (1. / cos(66. * d2r)) * RTM_Pc%SW%Tbc(i) ** (1. / cos(66. * d2r) - 1.)
      end do

      ! Calculate the derivative of the diffuse transmittance from cloud to
      ! surface times transmittance from surface to cloud
      Tbc_dd_l = 2. * Tbc_d_l * Tbc_d

      ! Derivative w.r.t. cloud-top pressure, P_c
      if (i_equation_form .eq. 1) then
         d_l = CRP(:,IT_00) * (SPixel%Surface%Rs2(:,IRho_0V) - SPixel%Surface%Rs2(:,IRho_DD)) * &
                  CRP(:,IT_dv) * (Tbc_0_l * Tbc_d + Tbc_0 * Tbc_d_l) + &
               ((CRP(:,IT_00) * SPixel%Surface%Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
                  SPixel%Surface%Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (                   &
                  CRP(:,IT_dv) * Tbc_d_l)) / a + &
               b * c * SPixel%Surface%Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)
      else
         d_l = CRP(:,IT_00) * SPixel%Surface%Rs2(:,IRho_0V) * CRP(:,IT_vv) * (Tbc_0_l * &
                  Tbc_v + Tbc_0 * Tbc_v_l) + &
               CRP(:,IT_0d) * SPixel%Surface%Rs2(:,IRho_DV) * CRP(:,IT_vv) * (Tbc_d_l * &
                  Tbc_v + Tbc_d * Tbc_v_l) + &
               ((CRP(:,IT_00) * SPixel%Surface%Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
                  SPixel%Surface%Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (CRP(:,IT_dv) * &
                  Tbc_d_l + CRP(:,IR_dd) * SPixel%Surface%Rs2(:,IRho_DV) * CRP(:,IT_vv) * &
                  Tbc_dd_l * Tbc_v + CRP(:,IR_dd) * SPixel%Surface%Rs2(:,IRho_DV) * &
                  CRP(:,IT_vv) * Tbc_dd * Tbc_v_l)) / a + &
               b * c * SPixel%Surface%Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / (a*a)
      end if

      REF_over_l = Tac_0v_l * d + Tac_0v * d_l

      d_REF(:,IPc) = X(IFr) * REF_over_l
   end if

   ! Derivative w.r.t. cloud fraction, f
   d_REF(:,IFr) = REF_over - SPixel%RTM%REF_clear

   ! Derivative w.r.t. surface temperature, T_s
   d_REF(:,ITs) = 0.0 ! Constant and zero

   ! Derivative w.r.t. BRDF reflectance parameters, rho_xx

   ! For this the rho_xx values are start point of the linearization but are
   ! left in the equation and set to one here in case derivatives w.r.t. BRDF
   ! input parameters are required
   rho_0v_l = 1.0
   rho_0d_l = 1.0
   rho_dv_l = 1.0
   rho_dd_l = 1.0

   if (i_equation_form .eq. 1) then
      d_l = -CRP(:,IT_00) * rho_dd_l * CRP(:,IT_DV) * Tbc_0d &
          +  CRP(:,IT_0D) * rho_dd_l * Tbc_d * c / a &
          +  b * c * rho_dd_l * CRP(:,IR_dd) * Tbc_dd / a**2
   else
      d_l =  CRP(:,IT_0D) * rho_dd_l * Tbc_d * c / a &
          +  b * c * rho_dd_l * CRP(:,IR_dd) * Tbc_dd / a**2
   end if

   REF_over_l = Tac_0v * d_l

   ! NOTE: This neglects the / sec_o used in the Lambertian model
   do i = 1, SPixel%Ind%NSolar
      ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)
      d_REF(i,IRs(ii,IRho_DD)) = X(IFr) * REF_over_l(i) + &
                                 (1.0-X(IFr)) * SPixel%RTM%dREF_clear_dRs(i)
   end do
end if
   ! Open breakpoint file if required, and write out reflectances and gradients.
#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_FM_Solar) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: FM_Solar(): Error opening breakpoint file'
         stop BkpFileOpenErr
      else
         write(bkp_lun,*)'FM_Solar:'
      end if

      do i=1, SPixel%Ind%NSolar
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' Ref:         ', Ref(i)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' Ref_over:    ', Ref_over(i)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' T:           ', T(i)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' T_all:       ', T_all(i)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' S:           ', S(i)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' CRP(:,IRBd): ',CRP(i,IRBd)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' CRP(:,ITd):  ',CRP(i,ITd)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' CRP(:,ITB):  ',CRP(i,ITB)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' CRP(:,ITFBd):',CRP(i,ITFBd)
         write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
              ' CRP(:,IRFd): ',CRP(i,IRFd)
      end do

      do i=1, SPixel%Ind%NSolar
         write(bkp_lun,'(a,i2,a,6f9.4)') 'Channel index: ', i, &
              ' dRef: ', (d_Ref(i,j),j=1,MaxStateVar)
      end do

      write(bkp_lun, '(a,/)') 'FM_Solar: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine FM_Solar
