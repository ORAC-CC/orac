# 1 "../common/struct_parser.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "../common/struct_parser.F90"
!
! NAME:
! struct_parser.F90
! PURPOSE:
! Bison grammar to read the contents of a driver file and write its
! contents into a Fortran structure.
! HISTORY:
! 09 Jun 2016, ACP: Initial version
!
# 1 "../common/struct_parser.h" 1
# 11 "../common/struct_parser.F90" 2

module read_ctrl_m
   implicit none

contains

subroutine read_ctrl(filename, strct)
   use Ctrl_m; use ECP_constants_m
   use iso_c_binding
   implicit none

   interface
      subroutine read_ctrl_fc( &
# 1 "../src/obj/read_ctrl.f_arg.inc" 1
         COMMON_FILE_FLAGS_T__do_cloud, &
         COMMON_FILE_FLAGS_T__do_aerosol, &
         COMMON_FILE_FLAGS_T__do_rho, &
         COMMON_FILE_FLAGS_T__do_swansea, &
         COMMON_FILE_FLAGS_T__do_indexing, &
         COMMON_FILE_FLAGS_T__do_phase_pavolonis, &
         COMMON_FILE_FLAGS_T__do_cldmask, &
         COMMON_FILE_FLAGS_T__do_cldmask_uncertainty, &
         COMMON_FILE_FLAGS_T__do_phase, &
         COMMON_FILE_FLAGS_T__do_covariance, &
         COMMON_INDICES_T__Ny, &
         COMMON_INDICES_T__Y_Id, &
         COMMON_INDICES_T__Y_Id_dim0, &
         COMMON_INDICES_T__NSolar, &
         COMMON_INDICES_T__YSolar, &
         COMMON_INDICES_T__YSolar_dim0, &
         COMMON_INDICES_T__NThermal, &
         COMMON_INDICES_T__YThermal, &
         COMMON_INDICES_T__YThermal_dim0, &
         COMMON_INDICES_T__NViews, &
         COMMON_INDICES_T__View_Id, &
         COMMON_INDICES_T__View_Id_dim0, &
         COMMON_INDICES_T__Ch_Is, &
         COMMON_INDICES_T__Ch_Is_dim0, &
         COMMON_INDICES_T__Nx, &
         COMMON_INDICES_T__rho_terms, &
         COMMON_INDICES_T__rho_terms_dim0, &
         COMMON_INDICES_T__rho_terms_dim1, &
         COMMON_INDICES_T__Xdim, &
         COMMON_INDICES_T__X0, &
         COMMON_INDICES_T__X1, &
         COMMON_INDICES_T__Ydim, &
         COMMON_INDICES_T__Y0, &
         COMMON_INDICES_T__Y1, &
         FID_T__Data_Dir, &
         FID_T__Out_Dir, &
         FID_T__SAD_Dir, &
         FID_T__Filename, &
         FID_T__Config, &
         FID_T__MSI, &
         FID_T__LWRTM, &
         FID_T__SWRTM, &
         FID_T__PRTM, &
         FID_T__LS, &
         FID_T__CF, &
         FID_T__Geo, &
         FID_T__Loc, &
         FID_T__Alb, &
         FID_T__BkP, &
         FID_T__L2_primary, &
         FID_T__L2_secondary, &
         IND_T__NAvail, &
         IND_T__ICh, &
         IND_T__ICh_dim0, &
         IND_T__Y_Id_legacy, &
         IND_T__NMixed, &
         IND_T__YMixed, &
         IND_T__YMixed_dim0, &
         IND_T__NWvl, &
         IND_T__WvlIdx, &
         IND_T__WvlIdx_dim0, &
         IND_T__XMax, &
         IND_T__YMax, &
         IND_T__channel_proc_flag, &
         IND_T__channel_proc_flag_dim0, &
         SURFREF_T__RsSelm, &
         SURFREF_T__SRsSelm, &
         SURFREF_T__use_full_brdf, &
         SURFREF_T__allow_a_default_surface, &
         SURFREF_T__B, &
         SURFREF_T__B_dim0, &
         SURFREF_T__B_dim1, &
         SURFREF_T__Sb, &
         SURFREF_T__Sb_dim0, &
         SURFREF_T__Sb_dim1, &
         SURFREF_T__Cb, &
         SURFREF_T__add_fractional, &
         SURFREF_T__diagonal_SRs, &
         SURFREF_T__solar_factor, &
         EQMPN_T__SySelm, &
         EQMPN_T__Homog, &
         EQMPN_T__CoReg, &
         INVPAR_T__ConvTest, &
         INVPAR_T__MqStart, &
         INVPAR_T__MqStep, &
         INVPAR_T__MaxIter, &
         INVPAR_T__Ccj, &
         INVPAR_T__XScale, &
         INVPAR_T__XLLim, &
         INVPAR_T__XULim, &
         INVPAR_T__always_take_GN, &
         INVPAR_T__dont_iter_convtest, &
         INVPAR_T__disable_Ss, &
         QC_T__MaxJ, &
         QC_T__MaxS, &
         CTRL_T__InstName, &
         CTRL_T__LUTClass, &
         CTRL_T__Approach, &
         CTRL_T__DOY, &
         CTRL_T__Run_ID, &
         CTRL_T__MaxSolZen, &
         CTRL_T__MaxSatZen, &
         CTRL_T__MinRelAzi, &
         CTRL_T__Sunset, &
         CTRL_T__i_equation_form, &
         CTRL_T__get_T_dv_from_T_0d, &
         CTRL_T__LUTIntSelm, &
         CTRL_T__RTMIntSelm, &
         CTRL_T__CloudType, &
         CTRL_T__Bkpl, &
         CTRL_T__Max_SDAD, &
         CTRL_T__sabotage_inputs, &
         CTRL_T__process_cloudy_only, &
         CTRL_T__NTypes_to_process, &
         CTRL_T__Types_to_process, &
         CTRL_T__Surfaces_to_skip, &
         CTRL_T__second_aot_ch, &
         CTRL_T__verbose, &
         CTRL_T__tau_chans, &
         CTRL_T__tau_chans_dim0, &
         CTRL_T__r_e_chans, &
         CTRL_T__r_e_chans_dim0, &
         CTRL_T__ir_chans, &
         CTRL_T__ir_chans_dim0, &
         CTRL_T__ReChans, &
         CTRL_T__ReChans_dim0, &
         CTRL_T__do_new_night_retrieval, &
         CTRL_T__do_CTX_correction, &
         CTRL_T__CTP_correction_limit, &
         CTRL_T__Ap, &
         CTRL_T__Fg, &
         CTRL_T__Xb, &
         CTRL_T__X0, &
         CTRL_T__Sx, &
         CTRL_T__Sy, &
         CTRL_T__Sy_dim0, &
         CTRL_T__Sy_dim1, &
         CTRL_T__Nx, &
         CTRL_T__NxJ, &
         CTRL_T__X, &
         CTRL_T__XJ, &
# 25 "../common/struct_parser.F90" 2
         stat, filename) bind(C,name="read_ctrl_c")
         use Ctrl_m; use ECP_constants_m
         use iso_c_binding
         implicit none

# 1 "../src/obj/read_ctrl.f_def.inc" 1
# 34 "../src/obj/read_ctrl.f_def.inc"
         logical :: COMMON_FILE_FLAGS_T__do_cloud
         logical :: COMMON_FILE_FLAGS_T__do_aerosol
         logical :: COMMON_FILE_FLAGS_T__do_rho
         logical :: COMMON_FILE_FLAGS_T__do_swansea
         logical :: COMMON_FILE_FLAGS_T__do_indexing
         logical :: COMMON_FILE_FLAGS_T__do_phase_pavolonis
         logical :: COMMON_FILE_FLAGS_T__do_cldmask
         logical :: COMMON_FILE_FLAGS_T__do_cldmask_uncertainty
         logical :: COMMON_FILE_FLAGS_T__do_phase
         logical :: COMMON_FILE_FLAGS_T__do_covariance
         integer :: COMMON_INDICES_T__Ny
         integer :: COMMON_INDICES_T__NSolar
         integer :: COMMON_INDICES_T__NThermal
         integer :: COMMON_INDICES_T__NViews
         integer :: COMMON_INDICES_T__Nx
         integer :: COMMON_INDICES_T__Xdim
         integer :: COMMON_INDICES_T__X0
         integer :: COMMON_INDICES_T__X1
         integer :: COMMON_INDICES_T__Ydim
         integer :: COMMON_INDICES_T__Y0
         integer :: COMMON_INDICES_T__Y1
# 312 "../src/obj/read_ctrl.f_def.inc"
         character(c_char) :: FID_T__Data_Dir(*)
         character(c_char) :: FID_T__Out_Dir(*)
         character(c_char) :: FID_T__SAD_Dir(*)
         character(c_char) :: FID_T__Filename(*)
         character(c_char) :: FID_T__Config(*)
         character(c_char) :: FID_T__MSI(*)
         character(c_char) :: FID_T__LWRTM(*)
         character(c_char) :: FID_T__SWRTM(*)
         character(c_char) :: FID_T__PRTM(*)
         character(c_char) :: FID_T__LS(*)
         character(c_char) :: FID_T__CF(*)
         character(c_char) :: FID_T__Geo(*)
         character(c_char) :: FID_T__Loc(*)
         character(c_char) :: FID_T__Alb(*)
         character(c_char) :: FID_T__BkP(*)
         character(c_char) :: FID_T__L2_primary(*)
         character(c_char) :: FID_T__L2_secondary(*)

         integer :: IND_T__NAvail
         integer :: IND_T__Y_Id_legacy(6)
         integer :: IND_T__NMixed
         integer :: IND_T__NWvl
         integer :: IND_T__XMax
         integer :: IND_T__YMax
         integer :: SURFREF_T__RsSelm
         integer :: SURFREF_T__SRsSelm
         logical :: SURFREF_T__use_full_brdf
         logical :: SURFREF_T__allow_a_default_surface
         real :: SURFREF_T__Cb
         logical :: SURFREF_T__add_fractional
         logical :: SURFREF_T__diagonal_SRs
         logical :: SURFREF_T__solar_factor
         integer :: EQMPN_T__SySelm
         logical :: EQMPN_T__Homog
         logical :: EQMPN_T__CoReg
         logical :: INVPAR_T__ConvTest
         real :: INVPAR_T__MqStart
         real :: INVPAR_T__MqStep
         integer :: INVPAR_T__MaxIter
         real :: INVPAR_T__Ccj
         real :: INVPAR_T__XScale(5 + 24*4 + 2)
         real :: INVPAR_T__XLLim(5 + 24*4 + 2)
         real :: INVPAR_T__XULim(5 + 24*4 + 2)
         logical :: INVPAR_T__always_take_GN
         logical :: INVPAR_T__dont_iter_convtest
         logical :: INVPAR_T__disable_Ss
         real :: QC_T__MaxJ
         real :: QC_T__MaxS(5 + 24*4 + 2)

         character(c_char) :: CTRL_T__InstName(*)
         character(c_char) :: CTRL_T__LUTClass(*)
         integer :: CTRL_T__Approach

         integer :: CTRL_T__DOY
         character(c_char) :: CTRL_T__Run_ID(*)




         real :: CTRL_T__MaxSolZen
         real :: CTRL_T__MaxSatZen
         real :: CTRL_T__MinRelAzi
         real :: CTRL_T__Sunset
         integer :: CTRL_T__i_equation_form
         logical :: CTRL_T__get_T_dv_from_T_0d
         integer :: CTRL_T__LUTIntSelm
         integer :: CTRL_T__RTMIntSelm
         integer :: CTRL_T__CloudType
         integer :: CTRL_T__Bkpl
         real :: CTRL_T__Max_SDAD
         logical :: CTRL_T__sabotage_inputs
         logical :: CTRL_T__process_cloudy_only
         integer :: CTRL_T__NTypes_to_process
         integer(byte) :: CTRL_T__Types_to_process(10)
         integer :: CTRL_T__Surfaces_to_skip
         integer :: CTRL_T__second_aot_ch(1)
         logical :: CTRL_T__verbose
         logical :: CTRL_T__do_new_night_retrieval
         logical :: CTRL_T__do_CTX_correction
         real :: CTRL_T__CTP_correction_limit
         integer :: CTRL_T__Ap(5 + 24*4 + 2, 3)
         integer :: CTRL_T__Fg(5 + 24*4 + 2, 3)
         real :: CTRL_T__Xb(5 + 24*4 + 2)
         real :: CTRL_T__X0(5 + 24*4 + 2)
         real :: CTRL_T__Sx(5 + 24*4 + 2)
         integer :: CTRL_T__Nx(3)
         integer :: CTRL_T__NxJ(3)
         integer :: CTRL_T__X(5 + 24*4 + 2, 3)
         integer :: CTRL_T__XJ(5 + 24*4 + 2, 3)
# 31 "../common/struct_parser.F90" 2
# 1 "../src/obj/read_ctrl.f_arr.inc" 1
         type(c_ptr) :: COMMON_INDICES_T__Y_Id
         integer :: COMMON_INDICES_T__Y_Id_dim0
         type(c_ptr) :: COMMON_INDICES_T__YSolar
         integer :: COMMON_INDICES_T__YSolar_dim0
         type(c_ptr) :: COMMON_INDICES_T__YThermal
         integer :: COMMON_INDICES_T__YThermal_dim0
         type(c_ptr) :: COMMON_INDICES_T__View_Id
         integer :: COMMON_INDICES_T__View_Id_dim0
         type(c_ptr) :: COMMON_INDICES_T__Ch_Is
         integer :: COMMON_INDICES_T__Ch_Is_dim0
         type(c_ptr) :: COMMON_INDICES_T__rho_terms
         integer :: COMMON_INDICES_T__rho_terms_dim0
         integer :: COMMON_INDICES_T__rho_terms_dim1
         type(c_ptr) :: IND_T__ICh
         integer :: IND_T__ICh_dim0
         type(c_ptr) :: IND_T__YMixed
         integer :: IND_T__YMixed_dim0
         type(c_ptr) :: IND_T__WvlIdx
         integer :: IND_T__WvlIdx_dim0
         type(c_ptr) :: IND_T__channel_proc_flag
         integer :: IND_T__channel_proc_flag_dim0
         type(c_ptr) :: SURFREF_T__B
         integer :: SURFREF_T__B_dim0
         integer :: SURFREF_T__B_dim1
         type(c_ptr) :: SURFREF_T__Sb
         integer :: SURFREF_T__Sb_dim0
         integer :: SURFREF_T__Sb_dim1
         type(c_ptr) :: CTRL_T__tau_chans
         integer :: CTRL_T__tau_chans_dim0
         type(c_ptr) :: CTRL_T__r_e_chans
         integer :: CTRL_T__r_e_chans_dim0
         type(c_ptr) :: CTRL_T__ir_chans
         integer :: CTRL_T__ir_chans_dim0
         type(c_ptr) :: CTRL_T__ReChans
         integer :: CTRL_T__ReChans_dim0
         type(c_ptr) :: CTRL_T__Sy
         integer :: CTRL_T__Sy_dim0
         integer :: CTRL_T__Sy_dim1
# 32 "../common/struct_parser.F90" 2
         integer :: stat
         character(c_char) :: filename(path_length)
      end subroutine read_ctrl_fc
   end interface

   character(len=*), intent(in) :: filename
   type(Ctrl_t), intent(inout) :: strct

   ! Declarations of variables
# 1 "../src/obj/read_ctrl.f_arr.inc" 1
         type(c_ptr) :: COMMON_INDICES_T__Y_Id
         integer :: COMMON_INDICES_T__Y_Id_dim0
         type(c_ptr) :: COMMON_INDICES_T__YSolar
         integer :: COMMON_INDICES_T__YSolar_dim0
         type(c_ptr) :: COMMON_INDICES_T__YThermal
         integer :: COMMON_INDICES_T__YThermal_dim0
         type(c_ptr) :: COMMON_INDICES_T__View_Id
         integer :: COMMON_INDICES_T__View_Id_dim0
         type(c_ptr) :: COMMON_INDICES_T__Ch_Is
         integer :: COMMON_INDICES_T__Ch_Is_dim0
         type(c_ptr) :: COMMON_INDICES_T__rho_terms
         integer :: COMMON_INDICES_T__rho_terms_dim0
         integer :: COMMON_INDICES_T__rho_terms_dim1
         type(c_ptr) :: IND_T__ICh
         integer :: IND_T__ICh_dim0
         type(c_ptr) :: IND_T__YMixed
         integer :: IND_T__YMixed_dim0
         type(c_ptr) :: IND_T__WvlIdx
         integer :: IND_T__WvlIdx_dim0
         type(c_ptr) :: IND_T__channel_proc_flag
         integer :: IND_T__channel_proc_flag_dim0
         type(c_ptr) :: SURFREF_T__B
         integer :: SURFREF_T__B_dim0
         integer :: SURFREF_T__B_dim1
         type(c_ptr) :: SURFREF_T__Sb
         integer :: SURFREF_T__Sb_dim0
         integer :: SURFREF_T__Sb_dim1
         type(c_ptr) :: CTRL_T__tau_chans
         integer :: CTRL_T__tau_chans_dim0
         type(c_ptr) :: CTRL_T__r_e_chans
         integer :: CTRL_T__r_e_chans_dim0
         type(c_ptr) :: CTRL_T__ir_chans
         integer :: CTRL_T__ir_chans_dim0
         type(c_ptr) :: CTRL_T__ReChans
         integer :: CTRL_T__ReChans_dim0
         type(c_ptr) :: CTRL_T__Sy
         integer :: CTRL_T__Sy_dim0
         integer :: CTRL_T__Sy_dim1
# 42 "../common/struct_parser.F90" 2

   integer :: stat
   character(kind=c_char,len=path_length) :: c_filename

   ! Copy pointers to structure into variables passed to wrapper
# 1 "../src/obj/read_ctrl.f_cpy.inc" 1
   if (associated(strct%Ind%common_indices_t%Y_Id)) then
      COMMON_INDICES_T__Y_Id = c_loc(strct%Ind%common_indices_t%Y_Id(1))
      COMMON_INDICES_T__Y_Id_dim0 = size(strct%Ind%common_indices_t%Y_Id,1)
   else
      COMMON_INDICES_T__Y_Id = C_NULL_PTR
      COMMON_INDICES_T__Y_Id_dim0 = 0
   end if
   if (associated(strct%Ind%common_indices_t%YSolar)) then
      COMMON_INDICES_T__YSolar = c_loc(strct%Ind%common_indices_t%YSolar(1))
      COMMON_INDICES_T__YSolar_dim0 = size(strct%Ind%common_indices_t%YSolar,1)
   else
      COMMON_INDICES_T__YSolar = C_NULL_PTR
      COMMON_INDICES_T__YSolar_dim0 = 0
   end if
   if (associated(strct%Ind%common_indices_t%YThermal)) then
      COMMON_INDICES_T__YThermal = c_loc(strct%Ind%common_indices_t%YThermal(1))
      COMMON_INDICES_T__YThermal_dim0 = size(strct%Ind%common_indices_t%YThermal,1)
   else
      COMMON_INDICES_T__YThermal = C_NULL_PTR
      COMMON_INDICES_T__YThermal_dim0 = 0
   end if
   if (associated(strct%Ind%common_indices_t%View_Id)) then
      COMMON_INDICES_T__View_Id = c_loc(strct%Ind%common_indices_t%View_Id(1))
      COMMON_INDICES_T__View_Id_dim0 = size(strct%Ind%common_indices_t%View_Id,1)
   else
      COMMON_INDICES_T__View_Id = C_NULL_PTR
      COMMON_INDICES_T__View_Id_dim0 = 0
   end if
   if (associated(strct%Ind%common_indices_t%Ch_Is)) then
      COMMON_INDICES_T__Ch_Is = c_loc(strct%Ind%common_indices_t%Ch_Is(1))
      COMMON_INDICES_T__Ch_Is_dim0 = size(strct%Ind%common_indices_t%Ch_Is,1)
   else
      COMMON_INDICES_T__Ch_Is = C_NULL_PTR
      COMMON_INDICES_T__Ch_Is_dim0 = 0
   end if
   if (associated(strct%Ind%common_indices_t%rho_terms)) then
      COMMON_INDICES_T__rho_terms = c_loc(strct%Ind%common_indices_t%rho_terms(1,1))
      COMMON_INDICES_T__rho_terms_dim0 = size(strct%Ind%common_indices_t%rho_terms,1)
      COMMON_INDICES_T__rho_terms_dim1 = size(strct%Ind%common_indices_t%rho_terms,2)
   else
      COMMON_INDICES_T__rho_terms = C_NULL_PTR
      COMMON_INDICES_T__rho_terms_dim0 = 0
      COMMON_INDICES_T__rho_terms_dim1 = 0
   end if
   if (associated(strct%Ind%ICh)) then
      IND_T__ICh = c_loc(strct%Ind%ICh(1))
      IND_T__ICh_dim0 = size(strct%Ind%ICh,1)
   else
      IND_T__ICh = C_NULL_PTR
      IND_T__ICh_dim0 = 0
   end if
   if (associated(strct%Ind%YMixed)) then
      IND_T__YMixed = c_loc(strct%Ind%YMixed(1))
      IND_T__YMixed_dim0 = size(strct%Ind%YMixed,1)
   else
      IND_T__YMixed = C_NULL_PTR
      IND_T__YMixed_dim0 = 0
   end if
   if (associated(strct%Ind%WvlIdx)) then
      IND_T__WvlIdx = c_loc(strct%Ind%WvlIdx(1))
      IND_T__WvlIdx_dim0 = size(strct%Ind%WvlIdx,1)
   else
      IND_T__WvlIdx = C_NULL_PTR
      IND_T__WvlIdx_dim0 = 0
   end if
   if (associated(strct%Ind%channel_proc_flag)) then
      IND_T__channel_proc_flag = c_loc(strct%Ind%channel_proc_flag(1))
      IND_T__channel_proc_flag_dim0 = size(strct%Ind%channel_proc_flag,1)
   else
      IND_T__channel_proc_flag = C_NULL_PTR
      IND_T__channel_proc_flag_dim0 = 0
   end if
   if (associated(strct%RS%B)) then
      SURFREF_T__B = c_loc(strct%RS%B(1,1))
      SURFREF_T__B_dim0 = size(strct%RS%B,1)
      SURFREF_T__B_dim1 = size(strct%RS%B,2)
   else
      SURFREF_T__B = C_NULL_PTR
      SURFREF_T__B_dim0 = 0
      SURFREF_T__B_dim1 = 0
   end if
   if (associated(strct%RS%Sb)) then
      SURFREF_T__Sb = c_loc(strct%RS%Sb(1,1))
      SURFREF_T__Sb_dim0 = size(strct%RS%Sb,1)
      SURFREF_T__Sb_dim1 = size(strct%RS%Sb,2)
   else
      SURFREF_T__Sb = C_NULL_PTR
      SURFREF_T__Sb_dim0 = 0
      SURFREF_T__Sb_dim1 = 0
   end if
   if (associated(strct%tau_chans)) then
      CTRL_T__tau_chans = c_loc(strct%tau_chans(1))
      CTRL_T__tau_chans_dim0 = size(strct%tau_chans,1)
   else
      CTRL_T__tau_chans = C_NULL_PTR
      CTRL_T__tau_chans_dim0 = 0
   end if
   if (associated(strct%r_e_chans)) then
      CTRL_T__r_e_chans = c_loc(strct%r_e_chans(1))
      CTRL_T__r_e_chans_dim0 = size(strct%r_e_chans,1)
   else
      CTRL_T__r_e_chans = C_NULL_PTR
      CTRL_T__r_e_chans_dim0 = 0
   end if
   if (associated(strct%ir_chans)) then
      CTRL_T__ir_chans = c_loc(strct%ir_chans(1))
      CTRL_T__ir_chans_dim0 = size(strct%ir_chans,1)
   else
      CTRL_T__ir_chans = C_NULL_PTR
      CTRL_T__ir_chans_dim0 = 0
   end if
   if (associated(strct%ReChans)) then
      CTRL_T__ReChans = c_loc(strct%ReChans(1))
      CTRL_T__ReChans_dim0 = size(strct%ReChans,1)
   else
      CTRL_T__ReChans = C_NULL_PTR
      CTRL_T__ReChans_dim0 = 0
   end if
   if (associated(strct%Sy)) then
      CTRL_T__Sy = c_loc(strct%Sy(1,1))
      CTRL_T__Sy_dim0 = size(strct%Sy,1)
      CTRL_T__Sy_dim1 = size(strct%Sy,2)
   else
      CTRL_T__Sy = C_NULL_PTR
      CTRL_T__Sy_dim0 = 0
      CTRL_T__Sy_dim1 = 0
   end if
# 48 "../common/struct_parser.F90" 2

   ! Call C wrapper function
   c_filename = trim(filename)//C_NULL_CHAR
   call read_ctrl_fc( &
# 1 "../src/obj/read_ctrl.f_arg2.inc" 1
         strct%Ind%common_indices_t%flags%do_cloud, &
         strct%Ind%common_indices_t%flags%do_aerosol, &
         strct%Ind%common_indices_t%flags%do_rho, &
         strct%Ind%common_indices_t%flags%do_swansea, &
         strct%Ind%common_indices_t%flags%do_indexing, &
         strct%Ind%common_indices_t%flags%do_phase_pavolonis, &
         strct%Ind%common_indices_t%flags%do_cldmask, &
         strct%Ind%common_indices_t%flags%do_cldmask_uncertainty, &
         strct%Ind%common_indices_t%flags%do_phase, &
         strct%Ind%common_indices_t%flags%do_covariance, &
         strct%Ind%common_indices_t%Ny, &
         COMMON_INDICES_T__Y_Id, &
         COMMON_INDICES_T__Y_Id_dim0, &
         strct%Ind%common_indices_t%NSolar, &
         COMMON_INDICES_T__YSolar, &
         COMMON_INDICES_T__YSolar_dim0, &
         strct%Ind%common_indices_t%NThermal, &
         COMMON_INDICES_T__YThermal, &
         COMMON_INDICES_T__YThermal_dim0, &
         strct%Ind%common_indices_t%NViews, &
         COMMON_INDICES_T__View_Id, &
         COMMON_INDICES_T__View_Id_dim0, &
         COMMON_INDICES_T__Ch_Is, &
         COMMON_INDICES_T__Ch_Is_dim0, &
         strct%Ind%common_indices_t%Nx, &
         COMMON_INDICES_T__rho_terms, &
         COMMON_INDICES_T__rho_terms_dim0, &
         COMMON_INDICES_T__rho_terms_dim1, &
         strct%Ind%common_indices_t%Xdim, &
         strct%Ind%common_indices_t%X0, &
         strct%Ind%common_indices_t%X1, &
         strct%Ind%common_indices_t%Ydim, &
         strct%Ind%common_indices_t%Y0, &
         strct%Ind%common_indices_t%Y1, &
         strct%FID%Data_Dir, &
         strct%FID%Out_Dir, &
         strct%FID%SAD_Dir, &
         strct%FID%Filename, &
         strct%FID%Config, &
         strct%FID%MSI, &
         strct%FID%LWRTM, &
         strct%FID%SWRTM, &
         strct%FID%PRTM, &
         strct%FID%LS, &
         strct%FID%CF, &
         strct%FID%Geo, &
         strct%FID%Loc, &
         strct%FID%Alb, &
         strct%FID%BkP, &
         strct%FID%L2_primary, &
         strct%FID%L2_secondary, &
         strct%Ind%NAvail, &
         IND_T__ICh, &
         IND_T__ICh_dim0, &
         strct%Ind%Y_Id_legacy, &
         strct%Ind%NMixed, &
         IND_T__YMixed, &
         IND_T__YMixed_dim0, &
         strct%Ind%NWvl, &
         IND_T__WvlIdx, &
         IND_T__WvlIdx_dim0, &
         strct%Ind%XMax, &
         strct%Ind%YMax, &
         IND_T__channel_proc_flag, &
         IND_T__channel_proc_flag_dim0, &
         strct%RS%RsSelm, &
         strct%RS%SRsSelm, &
         strct%RS%use_full_brdf, &
         strct%RS%allow_a_default_surface, &
         SURFREF_T__B, &
         SURFREF_T__B_dim0, &
         SURFREF_T__B_dim1, &
         SURFREF_T__Sb, &
         SURFREF_T__Sb_dim0, &
         SURFREF_T__Sb_dim1, &
         strct%RS%Cb, &
         strct%RS%add_fractional, &
         strct%RS%diagonal_SRs, &
         strct%RS%solar_factor, &
         strct%EqMPN%SySelm, &
         strct%EqMPN%Homog, &
         strct%EqMPN%CoReg, &
         strct%Invpar%ConvTest, &
         strct%Invpar%MqStart, &
         strct%Invpar%MqStep, &
         strct%Invpar%MaxIter, &
         strct%Invpar%Ccj, &
         strct%Invpar%XScale, &
         strct%Invpar%XLLim, &
         strct%Invpar%XULim, &
         strct%Invpar%always_take_GN, &
         strct%Invpar%dont_iter_convtest, &
         strct%Invpar%disable_Ss, &
         strct%QC%MaxJ, &
         strct%QC%MaxS, &
         strct%InstName, &
         strct%LUTClass, &
         strct%Approach, &
         strct%DOY, &
         strct%Run_ID, &
         strct%MaxSolZen, &
         strct%MaxSatZen, &
         strct%MinRelAzi, &
         strct%Sunset, &
         strct%i_equation_form, &
         strct%get_T_dv_from_T_0d, &
         strct%LUTIntSelm, &
         strct%RTMIntSelm, &
         strct%CloudType, &
         strct%Bkpl, &
         strct%Max_SDAD, &
         strct%sabotage_inputs, &
         strct%process_cloudy_only, &
         strct%NTypes_to_process, &
         strct%Types_to_process, &
         strct%Surfaces_to_skip, &
         strct%second_aot_ch, &
         strct%verbose, &
         CTRL_T__tau_chans, &
         CTRL_T__tau_chans_dim0, &
         CTRL_T__r_e_chans, &
         CTRL_T__r_e_chans_dim0, &
         CTRL_T__ir_chans, &
         CTRL_T__ir_chans_dim0, &
         CTRL_T__ReChans, &
         CTRL_T__ReChans_dim0, &
         strct%do_new_night_retrieval, &
         strct%do_CTX_correction, &
         strct%CTP_correction_limit, &
         strct%Ap, &
         strct%Fg, &
         strct%Xb, &
         strct%X0, &
         strct%Sx, &
         CTRL_T__Sy, &
         CTRL_T__Sy_dim0, &
         CTRL_T__Sy_dim1, &
         strct%Nx, &
         strct%NxJ, &
         strct%X, &
         strct%XJ, &
# 53 "../common/struct_parser.F90" 2
      stat, c_filename)

   ! Copy arrays allocated in wrapper into structure
# 1 "../src/obj/read_ctrl.f_cpy2.inc" 1
   if (size(strct%Ind%common_indices_t%Y_Id,1) /= COMMON_INDICES_T__Y_Id_dim0) call c_f_pointer(COMMON_INDICES_T__Y_Id, strct%Ind%common_indices_t%Y_Id, [COMMON_INDICES_T__Y_Id_dim0])
   if (size(strct%Ind%common_indices_t%YSolar,1) /= COMMON_INDICES_T__YSolar_dim0) call c_f_pointer(COMMON_INDICES_T__YSolar, strct%Ind%common_indices_t%YSolar, [COMMON_INDICES_T__YSolar_dim0])
   if (size(strct%Ind%common_indices_t%YThermal,1) /= COMMON_INDICES_T__YThermal_dim0) call c_f_pointer(COMMON_INDICES_T__YThermal, strct%Ind%common_indices_t%YThermal, [COMMON_INDICES_T__YThermal_dim0])
   if (size(strct%Ind%common_indices_t%View_Id,1) /= COMMON_INDICES_T__View_Id_dim0) call c_f_pointer(COMMON_INDICES_T__View_Id, strct%Ind%common_indices_t%View_Id, [COMMON_INDICES_T__View_Id_dim0])
   if (size(strct%Ind%common_indices_t%Ch_Is,1) /= COMMON_INDICES_T__Ch_Is_dim0) call c_f_pointer(COMMON_INDICES_T__Ch_Is, strct%Ind%common_indices_t%Ch_Is, [COMMON_INDICES_T__Ch_Is_dim0])
   if (size(strct%Ind%common_indices_t%rho_terms,1) /= COMMON_INDICES_T__rho_terms_dim0 .or. size(strct%Ind%common_indices_t%rho_terms,2) /= COMMON_INDICES_T__rho_terms_dim1) call c_f_pointer(COMMON_INDICES_T__rho_terms, strct%Ind%common_indices_t%rho_terms, [COMMON_INDICES_T__rho_terms_dim0,COMMON_INDICES_T__rho_terms_dim1])
   call strip_c_nulls(strct%FID%Data_Dir)
   call strip_c_nulls(strct%FID%Out_Dir)
   call strip_c_nulls(strct%FID%SAD_Dir)
   call strip_c_nulls(strct%FID%Filename)
   call strip_c_nulls(strct%FID%Config)
   call strip_c_nulls(strct%FID%MSI)
   call strip_c_nulls(strct%FID%LWRTM)
   call strip_c_nulls(strct%FID%SWRTM)
   call strip_c_nulls(strct%FID%PRTM)
   call strip_c_nulls(strct%FID%LS)
   call strip_c_nulls(strct%FID%CF)
   call strip_c_nulls(strct%FID%Geo)
   call strip_c_nulls(strct%FID%Loc)
   call strip_c_nulls(strct%FID%Alb)
   call strip_c_nulls(strct%FID%BkP)
   call strip_c_nulls(strct%FID%L2_primary)
   call strip_c_nulls(strct%FID%L2_secondary)
   if (size(strct%Ind%ICh,1) /= IND_T__ICh_dim0) call c_f_pointer(IND_T__ICh, strct%Ind%ICh, [IND_T__ICh_dim0])
   if (size(strct%Ind%YMixed,1) /= IND_T__YMixed_dim0) call c_f_pointer(IND_T__YMixed, strct%Ind%YMixed, [IND_T__YMixed_dim0])
   if (size(strct%Ind%WvlIdx,1) /= IND_T__WvlIdx_dim0) call c_f_pointer(IND_T__WvlIdx, strct%Ind%WvlIdx, [IND_T__WvlIdx_dim0])
   if (size(strct%Ind%channel_proc_flag,1) /= IND_T__channel_proc_flag_dim0) call c_f_pointer(IND_T__channel_proc_flag, strct%Ind%channel_proc_flag, [IND_T__channel_proc_flag_dim0])
   if (size(strct%RS%B,1) /= SURFREF_T__B_dim0 .or. size(strct%RS%B,2) /= SURFREF_T__B_dim1) call c_f_pointer(SURFREF_T__B, strct%RS%B, [SURFREF_T__B_dim0,SURFREF_T__B_dim1])
   if (size(strct%RS%Sb,1) /= SURFREF_T__Sb_dim0 .or. size(strct%RS%Sb,2) /= SURFREF_T__Sb_dim1) call c_f_pointer(SURFREF_T__Sb, strct%RS%Sb, [SURFREF_T__Sb_dim0,SURFREF_T__Sb_dim1])
   call strip_c_nulls(strct%InstName)
   call strip_c_nulls(strct%LUTClass)
   call strip_c_nulls(strct%Run_ID)
   if (size(strct%tau_chans,1) /= CTRL_T__tau_chans_dim0) call c_f_pointer(CTRL_T__tau_chans, strct%tau_chans, [CTRL_T__tau_chans_dim0])
   if (size(strct%r_e_chans,1) /= CTRL_T__r_e_chans_dim0) call c_f_pointer(CTRL_T__r_e_chans, strct%r_e_chans, [CTRL_T__r_e_chans_dim0])
   if (size(strct%ir_chans,1) /= CTRL_T__ir_chans_dim0) call c_f_pointer(CTRL_T__ir_chans, strct%ir_chans, [CTRL_T__ir_chans_dim0])
   if (size(strct%ReChans,1) /= CTRL_T__ReChans_dim0) call c_f_pointer(CTRL_T__ReChans, strct%ReChans, [CTRL_T__ReChans_dim0])
   if (size(strct%Sy,1) /= CTRL_T__Sy_dim0 .or. size(strct%Sy,2) /= CTRL_T__Sy_dim1) call c_f_pointer(CTRL_T__Sy, strct%Sy, [CTRL_T__Sy_dim0,CTRL_T__Sy_dim1])
# 57 "../common/struct_parser.F90" 2

end subroutine read_ctrl

! Routines called from C to allocate arrays



# 1 "../common/fort_alloc.inc" 1
subroutine fort_alloc_bool_1d(ptr, n0, m0) bind(C,name="fort_alloc_bool_1d")
   use iso_c_binding
   implicit none

   type(c_ptr), intent(inout) :: ptr
   integer, intent(inout) :: n0
   integer, intent(in) :: m0
   logical, pointer :: arr(:)

   if (n0 /= m0) then
      if (n0 /= 0) then
         call c_f_pointer(ptr, arr, [n0])
         deallocate(arr)
      end if
      allocate(arr(m0))
      n0 = m0
      ptr = c_loc(arr(1))
   end if

end subroutine fort_alloc_bool_1d

subroutine fort_alloc_bool_2d(ptr, n0, n1, m0, m1) bind(C,name="fort_alloc_bool_2d")
   use iso_c_binding
   implicit none

   type(c_ptr), intent(inout) :: ptr
   integer, intent(inout) :: n0, n1
   integer, intent(in) :: m0, m1
   logical, pointer :: arr(:,:)

   if (n0 /= m0 .or. n1 /= m1) then
      if (n0 /= 0 .or. n1 /= 0) then
         call c_f_pointer(ptr, arr, [n0,n1])
         deallocate(arr)
      end if
      allocate(arr(m0,m1))
      n0 = m0
      n1 = m1
      ptr = c_loc(arr(1,1))
   end if

end subroutine fort_alloc_bool_2d
# 65 "../common/struct_parser.F90" 2







# 1 "../common/fort_alloc.inc" 1
subroutine fort_alloc_int_1d(ptr, n0, m0) bind(C,name="fort_alloc_int_1d")
   use iso_c_binding
   implicit none

   type(c_ptr), intent(inout) :: ptr
   integer, intent(inout) :: n0
   integer, intent(in) :: m0
   integer, pointer :: arr(:)

   if (n0 /= m0) then
      if (n0 /= 0) then
         call c_f_pointer(ptr, arr, [n0])
         deallocate(arr)
      end if
      allocate(arr(m0))
      n0 = m0
      ptr = c_loc(arr(1))
   end if

end subroutine fort_alloc_int_1d

subroutine fort_alloc_int_2d(ptr, n0, n1, m0, m1) bind(C,name="fort_alloc_int_2d")
   use iso_c_binding
   implicit none

   type(c_ptr), intent(inout) :: ptr
   integer, intent(inout) :: n0, n1
   integer, intent(in) :: m0, m1
   integer, pointer :: arr(:,:)

   if (n0 /= m0 .or. n1 /= m1) then
      if (n0 /= 0 .or. n1 /= 0) then
         call c_f_pointer(ptr, arr, [n0,n1])
         deallocate(arr)
      end if
      allocate(arr(m0,m1))
      n0 = m0
      n1 = m1
      ptr = c_loc(arr(1,1))
   end if

end subroutine fort_alloc_int_2d
# 73 "../common/struct_parser.F90" 2







# 1 "../common/fort_alloc.inc" 1
subroutine fort_alloc_float_1d(ptr, n0, m0) bind(C,name="fort_alloc_float_1d")
   use iso_c_binding
   implicit none

   type(c_ptr), intent(inout) :: ptr
   integer, intent(inout) :: n0
   integer, intent(in) :: m0
   real, pointer :: arr(:)

   if (n0 /= m0) then
      if (n0 /= 0) then
         call c_f_pointer(ptr, arr, [n0])
         deallocate(arr)
      end if
      allocate(arr(m0))
      n0 = m0
      ptr = c_loc(arr(1))
   end if

end subroutine fort_alloc_float_1d

subroutine fort_alloc_float_2d(ptr, n0, n1, m0, m1) bind(C,name="fort_alloc_float_2d")
   use iso_c_binding
   implicit none

   type(c_ptr), intent(inout) :: ptr
   integer, intent(inout) :: n0, n1
   integer, intent(in) :: m0, m1
   real, pointer :: arr(:,:)

   if (n0 /= m0 .or. n1 /= m1) then
      if (n0 /= 0 .or. n1 /= 0) then
         call c_f_pointer(ptr, arr, [n0,n1])
         deallocate(arr)
      end if
      allocate(arr(m0,m1))
      n0 = m0
      n1 = m1
      ptr = c_loc(arr(1,1))
   end if

end subroutine fort_alloc_float_2d
# 81 "../common/struct_parser.F90" 2





subroutine strip_c_nulls(str)
   use iso_c_binding, only: C_NULL_CHAR
   implicit none

   character(*), intent(inout) :: str
   integer :: i

   do i=1,len(str)
      if (str(i:i) == C_NULL_CHAR) str(i:i) = ' '
   end do
end subroutine strip_c_nulls

end module read_ctrl_m
