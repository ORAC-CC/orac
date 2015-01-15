!-------------------------------------------------------------------------------
! Name:
!   FM_Solar
!
! Description:
!   Reflectance forward model (solar channels) for a defined pixel and
!   pressure level.
!
! Arguments:
!   Name    Type        In/Out/Both Description
!   Ctrl    struct      In          Control structure
!   SAD_LUT struct      In          SAD look up table
!   SPixel  struct      In          Super-pixel structure
!   RTM_Pc  struct      In          Contains transmittances, radiances and their
!                                   derivatives
!   X       real array  In          State vector
!   GZero   struct      In          "Zero'th point" grid info for SAD_LUT CRP
!                                   array interpolation.
!   CRP     float array In          Interpolated cloud radiative properties
!                                   (calculated by SetCRPSolar, but an input
!                                   argument because CRP for Td for the mixed
!                                   channels is set by SetCRPThermal. Only the
!                                   solar channels should be passed by the
!                                   calling routine).
!   d_CRP   float array In          Grads. of interpolated cloud rad. properties
!                                   (see comment for CRP).
!   REF     float array Out         TOA reflectances for part cloudy conditions
!   d_REF   float array Out         Derivatives d[ref]/d[tau,Re,pc,f,Ts,Rs]t
!   status  int         Out         Status from Set_CRP_Solar/breakpoint file
!                                   open
!
! Algorithm:
!   Note: FMThermal must have been called prior to this routine, to populate
!      CRP(ThermalFirst:SolarLast, ITd), i.e. for the part-thermal channels.
!   Get radiance functions.
!   Calculate transmittances for current geometry.
!   Calculate top of the atmosphere reflectances for clear, overcast, and part
!      cloudy conditions.
!   Calculate derivatives of reflectance w.r.t. all other variables.
!
! Local variables:
!    Name Type Description
!
! History:
!     7th November, 2000, Kevin M. Smith : original version
!    21st November, 2000: added X structure (state vector)
!    17th Jan 2001, Andy Smith:
!       Changed indexing of CRP arrays to use constants to reference the
!       different LUT values (IRBd etc)
!       Using FM_Routines_def: contains interface definition for SetCRPSolar.
!    23rd Jan 2001, Andy Smith:
!       Added GZero argument, interface to SetCRPSolar changed.
!       Updated CRP, d_CRP and Ref, d_Ref array indexing to use constants to
!       pick out the values depending on Tau, Re etc.
!     2nd Feb 2001, Andy Smith:
!       Transmittance values Tac etc made arguments, Interpol_Solar is now
!       called before this function rather than from this function.
!     7th Feb 2001, Andy Smith:
!       Picks up Tac values etc from RTM_Pc structure.
!     9th Feb 2001, Andy Smith:
!       First fully completed and (informally) tested version.
!    15th Feb 2001, Andy Smith:
!       Array sizes changed: Ctrl%Ind%NSolar to SPixel%Ind%NSolar
!       Indices changed where whole dimension is used, replaced
!       array(1: SPixel%Ind%NSolar) with array(:).
!    16th Feb 2001, Andy Smith:
!       Array sizes changed again: only the purely solar channels are required.
!       Use Ny-NThermal instead of NSolar. Calculate a local
!       NSolar variable (with the same value) for indexing SPixel%Rs.
!       SetCRPSolar now requires SPixel as an argument.
!    19th Feb 2001, Andy Smith:
!       Error in previous revision. Arrays must hold all solar channels, not
!       just purely solar.
!       CRP, d_CRP required as arguments (partially populated by FMThermal).
!       SetCRPSolar now takes SPixel%Ind as an argument instead of SPixel.
!     2nd Mar 2001, Andy Smith:
!       Updates to cope with transmittances etc as fractions instead of
!       percentages.
!     6th Mar 2001, Andy Smith:
!       Calculation of Ref_clear and dRef_clear_dRs moved to Get_SPixel.
!       (Values are now part of SPixel%RTM).
!     7th Mar 2001, Andy Smith:
!       Tac, Tbc etc now picked up from the overall RTM_Pc struct rather than
!       the SW sub-structure, so that all solar channels are selected, and not
!       just the purely solar.
!    13th Mar 2001, Andy Smith:
!       Changed some of the "compound" variables: S, TBTD etc can go to 0 and
!       are used in division operations. New variable Sp (S prime, replaces S /
!       TBTD).
!    22nd Mar 2001, Andy Smith:
!       Corrected equations for d_Ref wrt Tau and Re
!    23rd Mar 2001, Andy Smith:
!       Updated calculation of dRef wrt Rs to include divide by SPixel%Geom%SEC_o
!       since SPixel Rs values now include this factor.
!    15th Oct 2001, Andy Smith:
!       Gradient w.r.t Rs fixed. Previously only the second term was divided by
!       sec_o, whereas the whole expression should have been divided. Also the
!       bracketing was incorrect: instead of a "f" term and a "1-f" term,
!       everything was multiplied by f. (For "f" read "X(IFr)" in the code).
!     5th May 2011, Andy Smith:
!       Extension to multiple viewing angles. Some whole-array assignments
!       replaced by loops over channels, where appropriate viewing geometry must
!       be selected.
!       Added some breakpoint outputs.
!    20th Jan 2012, C Poulsen:
!       Fixed bug with rtm_pc%tbc array allocation.
!    20th Dec 2014, Greg McGarragh:
!       Cleaned up code.
!    24th Dec 2014, Greg McGarragh:
!       Some intent changes.
!     9th Sep 2014, Greg McGarragh:
!       Changes related to new BRDF support.
!    11th Dec 2014, Greg McGarragh:
!       Some small mathmatical bug fixes in the BRDF derivative equations,
!       mathematical refactoring to reduce computation, and some cleanup.
!     9th Jan 2015, Adam Povey:
!       Eliminate write to RTM_Pc%Tac, Tbc.
!    15th Jan 2015, Adam Povey:
!       Facilitate channel indexing in arbitrary order.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine derivative_wrt_crp_parameter(SPixel, i_param, CRP, d_CRP, f, Tac_0v, &
   Tbc2, T_all, S, S_dnom, Sp, d_REF)

   use SPixel_def

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_param
   real,           intent(in)  :: CRP(SPixel%Ind%NSolar,MaxCRProps)
   real,           intent(in)  :: d_CRP(SPixel%Ind%NSolar,MaxCRProps,2)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_0v(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc2(SPixel%Ind%NSolar)
   real,           intent(in)  :: Sp(SPixel%Ind%NSolar)
   real,           intent(in)  :: T_all(SPixel%Ind%NSolar)
   real,           intent(in)  :: S(SPixel%Ind%NSolar)
   real,           intent(in)  :: S_dnom(SPixel%Ind%NSolar)
   real,           intent(out) :: d_REF(SPixel%Ind%NSolar,MaxCRProps)

   d_REF(:,i_param) = f * Tac_0v * &
      (d_CRP(:,IRBd,i_param) + &
      Sp * SPixel%Rs * &
      (T_all * d_CRP(:,ITd,i_param) + &
      CRP(:,ITd) * (d_CRP(:,ITB,i_param) + d_CRP(:,ITFBd,i_param)) &
      ) + &
      S * SPixel%Rs * Tbc2 * d_CRP(:,IRFd,i_param) / S_dnom)

end subroutine derivative_wrt_crp_parameter



subroutine derivative_wrt_crp_parameter_brdf(SPixel, i_param, i_equation_form, &
   CRP, d_CRP, f, Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, Tbc_dd, &
   Rs2, d_REF, a, b, c)

   use SPixel_def

   implicit none

   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: i_param
   integer,        intent(in)  :: i_equation_form
   real,           intent(in)  :: CRP(SPixel%Ind%NSolar,MaxCRProps)
   real,           intent(in)  :: d_CRP(SPixel%Ind%NSolar,MaxCRProps,2)
   real,           intent(in)  :: f
   real,           intent(in)  :: Tac_0v(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc_0(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc_v(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc_d(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc_0v(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc_0d(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc_dv(SPixel%Ind%NSolar)
   real,           intent(in)  :: Tbc_dd(SPixel%Ind%NSolar)
   real,           intent(in)  :: Rs2(SPixel%Ind%NSolar,MaxRho_XX)
   real,           intent(out) :: d_REF(SPixel%Ind%NSolar)
   real,           intent(in)  :: a(SPixel%Ind%NSolar)
   real,           intent(in)  :: b(SPixel%Ind%NSolar)
   real,           intent(in)  :: c(SPixel%Ind%NSolar)

   real :: d_l(SPixel%Ind%NSolar)
   real :: REF_over_l(SPixel%Ind%NSolar)

   if (i_equation_form .eq. 1) then
      d_l = d_CRP(:,IR_0v,i_param) + &
            (d_CRP(:,IT_00,i_param) * (SPixel%Rs2(:,IRho_0V) - &
               SPixel%Rs2(:,IRho_DD)) * CRP(:,IT_dv) + CRP(:,IT_00) * &
               (SPixel%Rs2(:,IRho_0V) - SPixel%Rs2(:,IRho_DD)) * &
               d_CRP(:,IT_dv,i_param)) * Tbc_0d + &
            ((d_CRP(:,IT_00,i_param) * SPixel%Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_param) * SPixel%Rs2(:,IRho_DD) * Tbc_d) * c + b * &
               d_CRP(:,IT_dv,i_param) * Tbc_d)  / a + &
            b * (c)  * SPixel%Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_param) * Tbc_dd / a**2
   else if (i_equation_form .eq. 2) then
      d_l = d_CRP(:,IR_0v,i_param) + &
            (d_CRP(:,IT_00,i_param) * (SPixel%Rs2(:,IRho_0V) - &
               SPixel%Rs2(:,IRho_0D)) * CRP(:,IT_vv) + CRP(:,IT_00) * &
               (SPixel%Rs2(:,IRho_0V) - SPixel%Rs2(:,IRho_0D)) * &
               d_CRP(:,IT_vv,i_param)) * Tbc_0v + &
            ((d_CRP(:,IT_00,i_param) * SPixel%Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_param) * SPixel%Rs2(:,IRho_DD) * Tbc_d) * c + &
               b * (d_CRP(:,IT_vv,i_param) * Tbc_v + d_CRP(:,IT_dv,i_param) * &
               Tbc_d)) / a + &
            b * (c) * SPixel%Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_param) * Tbc_dd / a**2
   else
      d_l = d_CRP(:,IR_0v,i_param) + &
            (d_CRP(:,IT_00,i_param) * SPixel%Rs2(:,IRho_0V) * CRP(:,IT_vv) + &
               CRP(:,IT_00) * SPixel%Rs2(:,IRho_0V) * d_CRP(:,IT_vv,i_param)) * &
               Tbc_0v + &
            (d_CRP(:,IT_0d,i_param) * SPixel%Rs2(:,IRho_DV) * CRP(:,IT_vv) + &
               CRP(:,IT_0d) * SPixel%Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_param)) * &
               Tbc_dv + &
            ((d_CRP(:,IT_00,i_param) * SPixel%Rs2(:,IRho_0D) * Tbc_0 + &
               d_CRP(:,IT_0d,i_param) * SPixel%Rs2(:,IRho_DD) * Tbc_d) * c + &
               b * (d_CRP(:,IT_dv,i_param) * Tbc_d + d_CRP(:,IR_dd,i_param) * &
               SPixel%Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dd * Tbc_v + &
               CRP(:,IR_dd) * SPixel%Rs2(:,IRho_DV) * d_CRP(:,IT_vv,i_param) * &
               Tbc_dd * Tbc_v)) / a + &
            b * (c) * SPixel%Rs2(:,IRho_DD) * d_CRP(:,IR_dd,i_param) * Tbc_dd / a**2
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
   real,            intent(in)    :: X(MaxStateVar)
   type(GZero_t),   intent(in)    :: GZero
   real,            intent(inout) :: CRP(SPixel%Ind%NSolar,MaxCRProps)
   real,            intent(inout) :: d_CRP(SPixel%Ind%NSolar,MaxCRProps,2)
   real,            intent(out)   :: REF(SPixel%Ind%NSolar)
   real,            intent(out)   :: d_REF(SPixel%Ind%NSolar, MaxStateVar+1)
   integer,         intent(out)   :: status

   ! Define local variables
   integer, parameter :: i_equation_form = 1

   integer :: i
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

   real    :: REF_cld(SPixel%Ind%NSolar)
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

   ! Interpolate cloud radiative property LUT data to the current Tau, Re values.
   ! Note that Set_CRP_Solar interpolates values for all solar channels, except
   ! in the case of Td. This is interpolated by SetCRPThermal, which is called
   ! by FMThermal prior to this routine.
   call Set_CRP_Solar(Ctrl, SPixel%Ind, SPixel%spixel_y_solar_to_ctrl_y_index, &
        GZero, SAD_LUT, CRP, d_CRP, status)

   ! Calculate above cloud (ac) beam transmittances
   do i=1,SPixel%Ind%NSolar
      ! At solar zenith angle:
      Tac_0(i) = RTM_Pc%SW%Tac(i) ** &
           SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
      ! At sensor viewing angle:
      Tac_v(i) = RTM_Pc%SW%Tac(i) ** &
      SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
   end do

   ! Calculate solar transmittance from TOA to cloud-top times the viewing
   ! transmittance from cloud-top to TOA
   Tac_0v = Tac_0 * Tac_v

   ! Calculate below cloud (bc) beam transmittances
   do i=1,SPixel%Ind%NSolar
      ! At solar zenith angle:
      Tbc_0(i) = RTM_Pc%SW%Tbc(i) ** &
           SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
      ! At sensor viewing angle:
      Tbc_v(i) = RTM_Pc%SW%Tbc(i) ** &
           SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
      ! At sensor viewing angle:
      Tbc_d(i) = RTM_Pc%SW%Tbc(i) ! ** (1. / cos(66. * d2r))
   end do

   ! Calculate transmittance from cloud to surface times transmittance from
   ! surface to cloud
   Tbc_0v = Tbc_0 * Tbc_v
   Tbc_0d = Tbc_0 * Tbc_d
   Tbc_dv = Tbc_d * Tbc_v
   Tbc_dd = Tbc_d * Tbc_d


   ! Referencing of different properties stored in CRP and d_CRP: the last index
   ! of the CRP array refers to the property. Hence ITB is the index of TB, etc.


   !----------------------------------------------------------------------------
   ! The old albedo only implementation
   !----------------------------------------------------------------------------
if (.not. Ctrl%RS%use_full_brdf) then
   ! Calculate auxillary quantitities used for the reflectance and its
   ! derivatives

   ! Sum of direct and diffuse beam transmissions
   T_all = (CRP(:,ITB) + CRP(:,ITFBd))

   S_dnom = 1.0 - (SPixel%Rs * CRP(:,IRFd) * Tbc_dd)

   S = (T_all * SPixel%Rs * CRP(:,ITd) * Tbc_dd) / S_dnom

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
   do i=1,SPixel%Ind%NSolar
      d_REF(i,IPc) = X(IFr) * &
         (1.0 * RTM_Pc%SW%dTac_dPc(i) * &
                (SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) + &
                 SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))) &
              * REF_over(i) / RTM_Pc%SW%Tac(i)) + &
         (2.0 * RTM_Pc%SW%dTbc_dPc(i) * Tac_0v(i) * S(i) * RTM_Pc%SW%Tbc(i) * &
              ((1.0/Tbc_dd(i)) + (CRP(i,IRFd) * SPixel%Rs(i) / S_dnom(i))))
   end do

   ! Derivative w.r.t. cloud fraction, f
   d_REF(:,IFr) = (REF_over - SPixel%RTM%REF_clear)

   ! Derivative w.r.t. surface temperature, T_s
   d_REF(:,ITs) = 0.0 ! Constant and zero

   ! Derivative w.r.t. surface reflectance, R_s
   do i=1,SPixel%Ind%NSolar
      d_REF(i,IRs) = &
         (X(IFr) * Tac_0v(i) * &
             (Sp(i) * TBTD(i) + (S(i) * CRP(i,IRFd) * Tbc_dd(i) / S_dnom(i))) + &
             (SPixel%RTM%dREF_clear_dRs(i) * (1.0-X(IFr))))/ &
         SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
   end do


   !----------------------------------------------------------------------------
   ! The new BRDF implementation
   !----------------------------------------------------------------------------
else
   ! Calculate auxillary quantitities used for the reflectance and its
   ! derivatives

   a = 1. - SPixel%Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd

   b = CRP(:,IT_00) * SPixel%Rs2(:,IRho_0D) * Tbc_0 + CRP(:,IT_0d) * &
          SPixel%Rs2(:,IRho_DD) * Tbc_d

   ! Calculate overcast reflectance
   if (i_equation_form .eq. 1) then
      c = CRP(:,IT_dv) * Tbc_d

      d = CRP(:,IR_0v) + &
          CRP(:,IT_00) * (SPixel%Rs2(:,IRho_0V) - SPixel%Rs2(:,IRho_DD)) * &
             CRP(:,IT_dv) * Tbc_0d + &
          b * c / a

      REF_over = Tac_0v * d
   else if (i_equation_form .eq. 2) then
      c = CRP(:,IT_vv) * Tbc_v + CRP(:,IT_dv) * Tbc_d

      d = CRP(:,IR_0v) + &
          CRP(:,IT_00) * (SPixel%Rs2(:,IRho_0V) - SPixel%Rs2(:,IRho_0D)) * &
             CRP(:,IT_vv) * Tbc_0v + &
          b * c / a

      REF_over = Tac_0v * d
   else
      c = CRP(:,IT_dv) * Tbc_d + CRP(:,IR_dd) * SPixel%Rs2(:,IRho_DV) * &
             CRP(:,IT_vv) * Tbc_dd * Tbc_v

      d = CRP(:,IR_0v) + &
          CRP(:,IT_00) * SPixel%Rs2(:,IRho_0V) * CRP(:,IT_vv) * Tbc_0v + &
          CRP(:,IT_0d) * SPixel%Rs2(:,IRho_DV) * CRP(:,IT_vv) * Tbc_dv + &
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
      Tbc_dd, SPixel%Rs2, d_REF(:,ITau), a, b, c)

   ! Derivative w.r.t. effective radius, r_e
   call derivative_wrt_crp_parameter_brdf(SPixel, IRe,  i_equation_form, CRP, &
      d_CRP, X(IFr), Tac_0v, Tbc_0, Tbc_v, Tbc_d, Tbc_0v, Tbc_0d, Tbc_dv, &
      Tbc_dd, SPixel%Rs2, d_REF(:,IRe), a, b, c)

   ! Calculate the derivatives of the above cloud (ac) beam transmittances
   do i=1,SPixel%Ind%NSolar
      ! Above cloud at solar zenith angle:
      Tac_0_l(i) = RTM_Pc%SW%dTac_dPc(i) * &
         SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
         RTM_Pc%SW%Tac(i) ** &
            (SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
      ! Above cloud at viewing angle:
      Tac_v_l(i) = RTM_Pc%SW%dTac_dPc(i) * &
         SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
         RTM_Pc%SW%Tac(i) ** &
            (SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) - 1.)
   end do

   ! Calculate the derivative of the solar transmittance from TOA to cloud-top
   ! times the viewing transmittance from cloud-top to TOA
   Tac_0v_l = Tac_0_l * Tac_v + Tac_0 * Tac_v_l

   ! Calculate the derivatives of the below cloud (bc) beam transmittances
   do i=1,SPixel%Ind%NSolar
      ! At solar zenith angle:
      Tbc_0_l(i) = RTM_Pc%SW%dTbc_dPc(i) * &
         SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
         RTM_Pc%SW%Tbc(i) ** (SPixel%Geom%SEC_o(SPixel%ViewIdx(i)) - 1.)
      ! At sensor viewing angle:
      Tbc_v_l(i) = RTM_Pc%SW%dTbc_dPc(i) * &
         SPixel%Geom%SEC_v(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * &
         RTM_Pc%SW%Tbc(i) ** (SPixel%Geom%SEC_v(SPixel%ViewIdx(i)) - 1.)
      ! At sensor viewing angle:
      Tbc_d_l(i) = RTM_Pc%SW%dTbc_dPc(i) ! * (1. / cos(66. * d2r)) * RTM_Pc%SW%Tbc(i) ** (1. / cos(66. * d2r) - 1.)
   end do

   ! Calculate the derivative of the diffuse transmittance from cloud to
   ! surface times transmittance from surface to cloud
   Tbc_dd_l = 2. * Tbc_d_l * Tbc_d

   ! Derivative w.r.t. cloud-top pressure, P_c
   if (i_equation_form .eq. 1) then
      d_l = CRP(:,IT_00) * (SPixel%Rs2(:,IRho_0V) - SPixel%Rs2(:,IRho_DD)) * &
               CRP(:,IT_dv) * (Tbc_0_l * Tbc_d + Tbc_0 * Tbc_d_l) + &
            ((CRP(:,IT_00) * SPixel%Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
               SPixel%Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (                   &
               CRP(:,IT_dv) * Tbc_d_l)) / a + &
            b * c * SPixel%Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / a**2
   else if (i_equation_form .eq. 2) then
      d_l = CRP(:,IT_00) * (SPixel%Rs2(:,IRho_0V) - SPixel%Rs2(:,IRho_0D)) * &
               CRP(:,IT_vv) * (Tbc_0_l * Tbc_v + Tbc_0 * Tbc_v_l) + &
            ((CRP(:,IT_00) * SPixel%Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
               SPixel%Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (CRP(:,IT_vv) * &
               Tbc_v_l + CRP(:,IT_dv) * Tbc_d_l)) / a + &
            b * c * SPixel%Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / a**2
   else
      d_l = CRP(:,IT_00) * SPixel%Rs2(:,IRho_0V) * CRP(:,IT_vv) * (Tbc_0_l * &
               Tbc_v + Tbc_0 * Tbc_v_l) + &
            CRP(:,IT_0d) * SPixel%Rs2(:,IRho_DV) * CRP(:,IT_vv) * (Tbc_d_l * &
               Tbc_v + Tbc_d * Tbc_v_l) + &
            ((CRP(:,IT_00) * SPixel%Rs2(:,IRho_0D) * Tbc_0_l + CRP(:,IT_0d) * &
               SPixel%Rs2(:,IRho_DD) * Tbc_d_l) * c + b * (CRP(:,IT_dv) * &
               Tbc_d_l + CRP(:,IR_dd) * SPixel%Rs2(:,IRho_DV) * CRP(:,IT_vv) * &
               Tbc_dd_l * Tbc_v + CRP(:,IR_dd) * SPixel%Rs2(:,IRho_DV) * &
               CRP(:,IT_vv) * Tbc_dd * Tbc_v_l)) / a + &
            b * c * SPixel%Rs2(:,IRho_DD) * CRP(:,IR_dd) * Tbc_dd_l / a**2
   end if

   REF_over_l = Tac_0v_l * d + Tac_0v * d_l

   d_REF(:,IPc) = X(IFr) * REF_over_l

   ! Derivative w.r.t. cloud fraction, f
   d_REF(:,IFr) = (REF_over - SPixel%RTM%REF_clear)

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
   else if (i_equation_form .eq. 2) then
      d_l =  CRP(:,IT_0D) * rho_dd_l * Tbc_d * c / a &
          +  b * c * rho_dd_l * CRP(:,IR_dd) * Tbc_dd / a**2
   else
      d_l =  CRP(:,IT_0D) * rho_dd_l * Tbc_d * c / a &
          +  b * c * rho_dd_l * CRP(:,IR_dd) * Tbc_dd / a**2
   end if

   REF_over_l = Tac_0v * d_l

   d_REF(:,IRs) = X(IFr) * REF_over_l + (1.0-X(IFr)) * SPixel%RTM%dREF_clear_dRs
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

      do i=SPixel%Ind%SolarFirst, SPixel%Ind%SolarLast
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

      do i=SPixel%Ind%SolarFirst, SPixel%Ind%SolarLast
      	 write(bkp_lun,'(a,i2,a,6f9.4)') 'Channel index: ', i, &
	    ' dRef: ', (d_Ref(i,j),j=1,MaxStateVar+1)
      end do

      write(bkp_lun, '(a,/)') 'FM_Solar: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine FM_Solar
