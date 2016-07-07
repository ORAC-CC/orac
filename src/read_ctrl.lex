 /*
 NAME:
    struct_parser.lex
 PURPOSE:
    The skeleton of a flex parser to read the contents of a text file into a
    Fortran structure. The start conditions and rules to identify variables are
    imported from the output of generate_parser to produce the final parser for
    a specific Fortran structure.
 HISTORY:
    07 Apr 2016, ACP: Initial version
    19 Apr 2016, ACP: Converted into C++ as structures were ungainly.
    09 Jun 2016, ACP: Final version
 */
%{
#include "struct_parser.hh"
#include "struct_parser.tab.hh"
#include XSTR(DIM_DEFINITIONS)

#define YY_USER_ACTION if (yytext[0] == '\n') { yylloc->lines(); } \
                       else { yylloc->columns (yyleng); }

typedef yy::CLASS_NAME::token token;

extern "C" {
    void WRAPPER_NAME_C(
#include XCAT3(INC_PATH, c_arg, inc)
        int* stat, const char* filename);

    void fort_alloc_bool_1d(bool** , int* , int* );
    void fort_alloc_bool_2d(bool** , int* , int* , int* , int* );
    void fort_alloc_int_1d(int** , int* , int* );
    void fort_alloc_int_2d(int** , int* , int* , int* , int* );
    void fort_alloc_float_1d(float** , int* , int* );
    void fort_alloc_float_2d(float** , int* , int* , int* , int* );
}
%}

%option warn nodefault
%option noyywrap
%option yylineno
%option nounput

     /* ---------- Token definitions ---------- */
%x COMMON_FILE_FLAGS_T_SC
%x COMMON_INDICES_T_SC
%x FID_T_SC
%x IND_T_SC
%x SURFREF_T_SC
%x EQMPN_T_SC
%x INVPAR_T_SC
%x QC_T_SC
%x CTRL_T_SC

DELIM   [%\.]
QUOTE   [\"\']
VAL     [\+\-]?[0-9]+

%%

%{
     // Step location counter for next token
     yylloc->step ();
%}

     /* ---------- Parsing rules ---------- */
(?i:"byte") {
      yylval->val = BYTE_TOKEN;
      return(token::NUM); }
(?i:"sint") {
      yylval->val = SINT_TOKEN;
      return(token::NUM); }
(?i:"lint") {
      yylval->val = LINT_TOKEN;
      return(token::NUM); }
(?i:"sreal") {
      yylval->val = SREAL_TOKEN;
      return(token::NUM); }
(?i:"dreal") {
      yylval->val = DREAL_TOKEN;
      return(token::NUM); }
(?i:"cmd_arg_length") {
      yylval->val = CMD_ARG_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"file_length") {
      yylval->val = FILE_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"path_length") {
      yylval->val = PATH_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"date_length") {
      yylval->val = DATE_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"platform_length") {
      yylval->val = PLATFORM_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"sensor_length") {
      yylval->val = SENSOR_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"attribute_length") {
      yylval->val = ATTRIBUTE_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"attribute_length_long") {
      yylval->val = ATTRIBUTE_LENGTH_LONG_TOKEN;
      return(token::NUM); }
(?i:"unitlength") {
      yylval->val = UNITLENGTH_TOKEN;
      return(token::NUM); }
(?i:"var_length") {
      yylval->val = VAR_LENGTH_TOKEN;
      return(token::NUM); }
(?i:"MAX_NC_NAME") {
      yylval->val = MAX_NC_NAME_TOKEN;
      return(token::NUM); }
(?i:"MAX_VAR_DIMS") {
      yylval->val = MAX_VAR_DIMS_TOKEN;
      return(token::NUM); }
(?i:"error_stop_code") {
      yylval->val = ERROR_STOP_CODE_TOKEN;
      return(token::NUM); }
(?i:"byte_fill_value") {
      yylval->val = BYTE_FILL_VALUE_TOKEN;
      return(token::NUM); }
(?i:"sint_fill_value") {
      yylval->val = SINT_FILL_VALUE_TOKEN;
      return(token::NUM); }
(?i:"lint_fill_value") {
      yylval->val = LINT_FILL_VALUE_TOKEN;
      return(token::NUM); }
(?i:"sreal_fill_value") {
      yylval->val = SREAL_FILL_VALUE_TOKEN;
      return(token::NUM); }
(?i:"dreal_fill_value") {
      yylval->val = DREAL_FILL_VALUE_TOKEN;
      return(token::NUM); }
(?i:"pi") {
      yylval->val = PI_TOKEN;
      return(token::NUM); }
(?i:"IRho_0V") {
      yylval->val = IRHO_0V_TOKEN;
      return(token::NUM); }
(?i:"IRho_0D") {
      yylval->val = IRHO_0D_TOKEN;
      return(token::NUM); }
(?i:"IRho_DV") {
      yylval->val = IRHO_DV_TOKEN;
      return(token::NUM); }
(?i:"IRho_DD") {
      yylval->val = IRHO_DD_TOKEN;
      return(token::NUM); }
(?i:"MaxRho_XX") {
      yylval->val = MAXRHO_XX_TOKEN;
      return(token::NUM); }
(?i:"SolarBit") {
      yylval->val = SOLARBIT_TOKEN;
      return(token::NUM); }
(?i:"ThermalBit") {
      yylval->val = THERMALBIT_TOKEN;
      return(token::NUM); }
(?i:"MaxNumMeas") {
      yylval->val = MAXNUMMEAS_TOKEN;
      return(token::NUM); }
(?i:"MaxNumViews") {
      yylval->val = MAXNUMVIEWS_TOKEN;
      return(token::NUM); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_cloud") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_cloud);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_aerosol") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_aerosol);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_rho") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_rho);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_swansea") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_swansea);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_indexing") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_indexing);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_phase_pavolonis") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_phase_pavolonis);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_cldmask") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_cldmask);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_cldmask_uncertainty") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_cldmask_uncertainty);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_phase") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_phase);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_FILE_FLAGS_T_SC>(?i:"do_covariance") {
      yylval->bool_v = new Target<bool> (COMMON_FILE_FLAGS_T_VARIABLE.do_covariance);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_INDICES_T_SC>(?i:"Ny") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Ny);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"Y_Id") {
      fort_alloc_int_1d(&COMMON_INDICES_T_VARIABLE.Y_Id, &COMMON_INDICES_T_VARIABLE.Y_Id_dim0, COMMON_INDICES_T_Y_Id_DIMS);
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Y_Id, COMMON_INDICES_T_VARIABLE.Y_Id_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"NSolar") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.NSolar);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"YSolar") {
      fort_alloc_int_1d(&COMMON_INDICES_T_VARIABLE.YSolar, &COMMON_INDICES_T_VARIABLE.YSolar_dim0, COMMON_INDICES_T_YSolar_DIMS);
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.YSolar, COMMON_INDICES_T_VARIABLE.YSolar_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"NThermal") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.NThermal);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"YThermal") {
      fort_alloc_int_1d(&COMMON_INDICES_T_VARIABLE.YThermal, &COMMON_INDICES_T_VARIABLE.YThermal_dim0, COMMON_INDICES_T_YThermal_DIMS);
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.YThermal, COMMON_INDICES_T_VARIABLE.YThermal_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"NViews") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.NViews);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"View_Id") {
      fort_alloc_int_1d(&COMMON_INDICES_T_VARIABLE.View_Id, &COMMON_INDICES_T_VARIABLE.View_Id_dim0, COMMON_INDICES_T_View_Id_DIMS);
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.View_Id, COMMON_INDICES_T_VARIABLE.View_Id_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"Ch_Is") {
      fort_alloc_int_1d(&COMMON_INDICES_T_VARIABLE.Ch_Is, &COMMON_INDICES_T_VARIABLE.Ch_Is_dim0, COMMON_INDICES_T_Ch_Is_DIMS);
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Ch_Is, COMMON_INDICES_T_VARIABLE.Ch_Is_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"Nx") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Nx);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"rho_terms") {
      fort_alloc_bool_2d(&COMMON_INDICES_T_VARIABLE.rho_terms, &COMMON_INDICES_T_VARIABLE.rho_terms_dim0, &COMMON_INDICES_T_VARIABLE.rho_terms_dim1, COMMON_INDICES_T_rho_terms_DIMS);
      yylval->bool_v = new Target<bool> (COMMON_INDICES_T_VARIABLE.rho_terms, COMMON_INDICES_T_VARIABLE.rho_terms_dim0, COMMON_INDICES_T_VARIABLE.rho_terms_dim1);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<COMMON_INDICES_T_SC>(?i:"Xdim") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Xdim);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"X0") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.X0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"X1") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.X1);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"Ydim") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Ydim);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"Y0") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Y0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"Y1") {
      yylval->int_v = new Target<int> (COMMON_INDICES_T_VARIABLE.Y1);
      BEGIN(INITIAL); return(token::INT_VAR); }
<COMMON_INDICES_T_SC>(?i:"flags"){DELIM} {
      BEGIN(COMMON_FILE_FLAGS_T_SC); }
(?i:"cloud_bit") {
      yylval->val = CLOUD_BIT_TOKEN;
      return(token::NUM); }
(?i:"aerosol_bit") {
      yylval->val = AEROSOL_BIT_TOKEN;
      return(token::NUM); }
(?i:"rho_bit") {
      yylval->val = RHO_BIT_TOKEN;
      return(token::NUM); }
(?i:"swansea_bit") {
      yylval->val = SWANSEA_BIT_TOKEN;
      return(token::NUM); }
(?i:"indexing_bit") {
      yylval->val = INDEXING_BIT_TOKEN;
      return(token::NUM); }
(?i:"pavolonis_bit") {
      yylval->val = PAVOLONIS_BIT_TOKEN;
      return(token::NUM); }
(?i:"cldmask_bit") {
      yylval->val = CLDMASK_BIT_TOKEN;
      return(token::NUM); }
(?i:"cldmask_u_bit") {
      yylval->val = CLDMASK_U_BIT_TOKEN;
      return(token::NUM); }
(?i:"phase_bit") {
      yylval->val = PHASE_BIT_TOKEN;
      return(token::NUM); }
(?i:"covariance_bit") {
      yylval->val = COVARIANCE_BIT_TOKEN;
      return(token::NUM); }
(?i:"FilenameLen") {
      yylval->val = FILENAMELEN_TOKEN;
      return(token::NUM); }
(?i:"InstNameLen") {
      yylval->val = INSTNAMELEN_TOKEN;
      return(token::NUM); }
(?i:"MaxNumSolar") {
      yylval->val = MAXNUMSOLAR_TOKEN;
      return(token::NUM); }
(?i:"MaxNumThermal") {
      yylval->val = MAXNUMTHERMAL_TOKEN;
      return(token::NUM); }
(?i:"MaxCloudType") {
      yylval->val = MAXCLOUDTYPE_TOKEN;
      return(token::NUM); }
(?i:"MaxPLevels") {
      yylval->val = MAXPLEVELS_TOKEN;
      return(token::NUM); }
(?i:"MaxCRProps") {
      yylval->val = MAXCRPROPS_TOKEN;
      return(token::NUM); }
(?i:"MaxTypes") {
      yylval->val = MAXTYPES_TOKEN;
      return(token::NUM); }
(?i:"ditherm3") {
      yylval->val = DITHERM3_TOKEN;
      return(token::NUM); }
(?i:"ditherm6") {
      yylval->val = DITHERM6_TOKEN;
      return(token::NUM); }
(?i:"ditherm15") {
      yylval->val = DITHERM15_TOKEN;
      return(token::NUM); }
(?i:"FlagMin") {
      yylval->val = FLAGMIN_TOKEN;
      return(token::NUM); }
(?i:"FlagMax") {
      yylval->val = FLAGMAX_TOKEN;
      return(token::NUM); }
(?i:"TypeMin") {
      yylval->val = TYPEMIN_TOKEN;
      return(token::NUM); }
(?i:"TypeMax") {
      yylval->val = TYPEMAX_TOKEN;
      return(token::NUM); }
(?i:"SolZenMin") {
      yylval->val = SOLZENMIN_TOKEN;
      return(token::NUM); }
(?i:"SatZenMin") {
      yylval->val = SATZENMIN_TOKEN;
      return(token::NUM); }
(?i:"SatZenMax") {
      yylval->val = SATZENMAX_TOKEN;
      return(token::NUM); }
(?i:"RelAziMin") {
      yylval->val = RELAZIMIN_TOKEN;
      return(token::NUM); }
(?i:"RelAziMax") {
      yylval->val = RELAZIMAX_TOKEN;
      return(token::NUM); }
(?i:"LatMin") {
      yylval->val = LATMIN_TOKEN;
      return(token::NUM); }
(?i:"LatMax") {
      yylval->val = LATMAX_TOKEN;
      return(token::NUM); }
(?i:"LonMin") {
      yylval->val = LONMIN_TOKEN;
      return(token::NUM); }
(?i:"LonMax") {
      yylval->val = LONMAX_TOKEN;
      return(token::NUM); }
(?i:"RefMin") {
      yylval->val = REFMIN_TOKEN;
      return(token::NUM); }
(?i:"RefMax") {
      yylval->val = REFMAX_TOKEN;
      return(token::NUM); }
(?i:"BTMin") {
      yylval->val = BTMIN_TOKEN;
      return(token::NUM); }
(?i:"BTMax") {
      yylval->val = BTMAX_TOKEN;
      return(token::NUM); }
(?i:"RhoMin") {
      yylval->val = RHOMIN_TOKEN;
      return(token::NUM); }
(?i:"RhoMax") {
      yylval->val = RHOMAX_TOKEN;
      return(token::NUM); }
(?i:"RhoErrMin") {
      yylval->val = RHOERRMIN_TOKEN;
      return(token::NUM); }
(?i:"RhoErrMax") {
      yylval->val = RHOERRMAX_TOKEN;
      return(token::NUM); }
(?i:"CorrMin") {
      yylval->val = CORRMIN_TOKEN;
      return(token::NUM); }
(?i:"CorrMax") {
      yylval->val = CORRMAX_TOKEN;
      return(token::NUM); }
(?i:"TxcMin") {
      yylval->val = TXCMIN_TOKEN;
      return(token::NUM); }
(?i:"TxcMax") {
      yylval->val = TXCMAX_TOKEN;
      return(token::NUM); }
(?i:"RxcMin") {
      yylval->val = RXCMIN_TOKEN;
      return(token::NUM); }
(?i:"RxcMax") {
      yylval->val = RXCMAX_TOKEN;
      return(token::NUM); }
(?i:"EmsMin") {
      yylval->val = EMSMIN_TOKEN;
      return(token::NUM); }
(?i:"EmsMax") {
      yylval->val = EMSMAX_TOKEN;
      return(token::NUM); }
(?i:"MissingXn") {
      yylval->val = MISSINGXN_TOKEN;
      return(token::NUM); }
(?i:"MissingSn") {
      yylval->val = MISSINGSN_TOKEN;
      return(token::NUM); }
(?i:"rhowat") {
      yylval->val = RHOWAT_TOKEN;
      return(token::NUM); }
(?i:"rhoice") {
      yylval->val = RHOICE_TOKEN;
      return(token::NUM); }
(?i:"qextwat") {
      yylval->val = QEXTWAT_TOKEN;
      return(token::NUM); }
(?i:"qextice") {
      yylval->val = QEXTICE_TOKEN;
      return(token::NUM); }
(?i:"g_wmo") {
      yylval->val = G_WMO_TOKEN;
      return(token::NUM); }
(?i:"MDADErrTau") {
      yylval->val = MDADERRTAU_TOKEN;
      return(token::NUM); }
(?i:"MDADErrPc") {
      yylval->val = MDADERRPC_TOKEN;
      return(token::NUM); }
(?i:"MDADErrF") {
      yylval->val = MDADERRF_TOKEN;
      return(token::NUM); }
(?i:"AUXErrTsLand") {
      yylval->val = AUXERRTSLAND_TOKEN;
      return(token::NUM); }
(?i:"AUXErrTsSea") {
      yylval->val = AUXERRTSSEA_TOKEN;
      return(token::NUM); }
(?i:"RTMIntMethLinear") {
      yylval->val = RTMINTMETHLINEAR_TOKEN;
      return(token::NUM); }
(?i:"RTMIntMethSpline") {
      yylval->val = RTMINTMETHSPLINE_TOKEN;
      return(token::NUM); }
(?i:"RTMIntMethNone") {
      yylval->val = RTMINTMETHNONE_TOKEN;
      return(token::NUM); }
(?i:"LUTIntMethLinear") {
      yylval->val = LUTINTMETHLINEAR_TOKEN;
      return(token::NUM); }
(?i:"LUTIntMethBicubic") {
      yylval->val = LUTINTMETHBICUBIC_TOKEN;
      return(token::NUM); }
(?i:"N_legacy") {
      yylval->val = N_LEGACY_TOKEN;
      return(token::NUM); }
(?i:"I_legacy_0_6x") {
      yylval->val = I_LEGACY_0_6X_TOKEN;
      return(token::NUM); }
(?i:"I_legacy_0_8x") {
      yylval->val = I_LEGACY_0_8X_TOKEN;
      return(token::NUM); }
(?i:"I_legacy_1_6x") {
      yylval->val = I_LEGACY_1_6X_TOKEN;
      return(token::NUM); }
(?i:"I_legacy_3_xx") {
      yylval->val = I_LEGACY_3_XX_TOKEN;
      return(token::NUM); }
(?i:"I_legacy_11_x") {
      yylval->val = I_LEGACY_11_X_TOKEN;
      return(token::NUM); }
(?i:"I_legacy_12_x") {
      yylval->val = I_LEGACY_12_X_TOKEN;
      return(token::NUM); }
(?i:"IBext") {
      yylval->val = IBEXT_TOKEN;
      return(token::NUM); }
(?i:"IRBd") {
      yylval->val = IRBD_TOKEN;
      return(token::NUM); }
(?i:"IRFBd") {
      yylval->val = IRFBD_TOKEN;
      return(token::NUM); }
(?i:"IRd") {
      yylval->val = IRD_TOKEN;
      return(token::NUM); }
(?i:"IRFd") {
      yylval->val = IRFD_TOKEN;
      return(token::NUM); }
(?i:"ITB") {
      yylval->val = ITB_TOKEN;
      return(token::NUM); }
(?i:"ITB_u") {
      yylval->val = ITB_U_TOKEN;
      return(token::NUM); }
(?i:"ITBd") {
      yylval->val = ITBD_TOKEN;
      return(token::NUM); }
(?i:"ITFBd") {
      yylval->val = ITFBD_TOKEN;
      return(token::NUM); }
(?i:"ITd") {
      yylval->val = ITD_TOKEN;
      return(token::NUM); }
(?i:"ITFd") {
      yylval->val = ITFD_TOKEN;
      return(token::NUM); }
(?i:"IEm") {
      yylval->val = IEM_TOKEN;
      return(token::NUM); }
(?i:"IBextRat") {
      yylval->val = IBEXTRAT_TOKEN;
      return(token::NUM); }
(?i:"ISwan_S") {
      yylval->val = ISWAN_S_TOKEN;
      return(token::NUM); }
(?i:"ISwan_P") {
      yylval->val = ISWAN_P_TOKEN;
      return(token::NUM); }
(?i:"MaxSwan_X") {
      yylval->val = MAXSWAN_X_TOKEN;
      return(token::NUM); }
(?i:"ISea") {
      yylval->val = ISEA_TOKEN;
      return(token::NUM); }
(?i:"ILand") {
      yylval->val = ILAND_TOKEN;
      return(token::NUM); }
(?i:"MaxSurf") {
      yylval->val = MAXSURF_TOKEN;
      return(token::NUM); }
(?i:"ITau") {
      yylval->val = ITAU_TOKEN;
      return(token::NUM); }
(?i:"IRe") {
      yylval->val = IRE_TOKEN;
      return(token::NUM); }
(?i:"IPc") {
      yylval->val = IPC_TOKEN;
      return(token::NUM); }
(?i:"IFr") {
      yylval->val = IFR_TOKEN;
      return(token::NUM); }
(?i:"ITs") {
      yylval->val = ITS_TOKEN;
      return(token::NUM); }
(?i:"IDay") {
      yylval->val = IDAY_TOKEN;
      return(token::NUM); }
(?i:"ITwi") {
      yylval->val = ITWI_TOKEN;
      return(token::NUM); }
(?i:"INight") {
      yylval->val = INIGHT_TOKEN;
      return(token::NUM); }
(?i:"MaxIllum") {
      yylval->val = MAXILLUM_TOKEN;
      return(token::NUM); }
(?i:"SelmCtrl") {
      yylval->val = SELMCTRL_TOKEN;
      return(token::NUM); }
(?i:"SelmMeas") {
      yylval->val = SELMMEAS_TOKEN;
      return(token::NUM); }
(?i:"SelmAux") {
      yylval->val = SELMAUX_TOKEN;
      return(token::NUM); }
(?i:"SelmPrev") {
      yylval->val = SELMPREV_TOKEN;
      return(token::NUM); }
(?i:"CostBit") {
      yylval->val = COSTBIT_TOKEN;
      return(token::NUM); }
(?i:"CldWat") {
      yylval->val = CLDWAT_TOKEN;
      return(token::NUM); }
(?i:"CldIce") {
      yylval->val = CLDICE_TOKEN;
      return(token::NUM); }
(?i:"AerOx") {
      yylval->val = AEROX_TOKEN;
      return(token::NUM); }
(?i:"AerSw") {
      yylval->val = AERSW_TOKEN;
      return(token::NUM); }
(?i:"AshEyj") {
      yylval->val = ASHEYJ_TOKEN;
      return(token::NUM); }
(?i:"BkpL_FM") {
      yylval->val = BKPL_FM_TOKEN;
      return(token::NUM); }
(?i:"BkpL_FM_Solar") {
      yylval->val = BKPL_FM_SOLAR_TOKEN;
      return(token::NUM); }
(?i:"BkpL_FM_Thermal") {
      yylval->val = BKPL_FM_THERMAL_TOKEN;
      return(token::NUM); }
(?i:"BkpL_Get_SPixel") {
      yylval->val = BKPL_GET_SPIXEL_TOKEN;
      return(token::NUM); }
(?i:"BkpL_Interpol_Solar") {
      yylval->val = BKPL_INTERPOL_SOLAR_TOKEN;
      return(token::NUM); }
(?i:"BkpL_Interpol_Thermal") {
      yylval->val = BKPL_INTERPOL_THERMAL_TOKEN;
      return(token::NUM); }
(?i:"BkpL_InvertMarquardt_1") {
      yylval->val = BKPL_INVERTMARQUARDT_1_TOKEN;
      return(token::NUM); }
(?i:"BkpL_InvertMarquardt_2") {
      yylval->val = BKPL_INVERTMARQUARDT_2_TOKEN;
      return(token::NUM); }
(?i:"BkpL_InvertMarquardt_3") {
      yylval->val = BKPL_INVERTMARQUARDT_3_TOKEN;
      return(token::NUM); }
(?i:"BkpL_InvertMarquardt_4") {
      yylval->val = BKPL_INVERTMARQUARDT_4_TOKEN;
      return(token::NUM); }
(?i:"BkpL_Read_LUT_1") {
      yylval->val = BKPL_READ_LUT_1_TOKEN;
      return(token::NUM); }
(?i:"BkpL_Read_LUT_2") {
      yylval->val = BKPL_READ_LUT_2_TOKEN;
      return(token::NUM); }
(?i:"DriverFileOpenErr") {
      yylval->val = DRIVERFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"DriverFileReadErr") {
      yylval->val = DRIVERFILEREADERR_TOKEN;
      return(token::NUM); }
(?i:"DriverFileNotFound") {
      yylval->val = DRIVERFILENOTFOUND_TOKEN;
      return(token::NUM); }
(?i:"DriverFileDataErr") {
      yylval->val = DRIVERFILEDATAERR_TOKEN;
      return(token::NUM); }
(?i:"AMethInvalid") {
      yylval->val = AMETHINVALID_TOKEN;
      return(token::NUM); }
(?i:"LimitMethInvalid") {
      yylval->val = LIMITMETHINVALID_TOKEN;
      return(token::NUM); }
(?i:"SegSizeInvalid") {
      yylval->val = SEGSIZEINVALID_TOKEN;
      return(token::NUM); }
(?i:"DriverFileIncompat") {
      yylval->val = DRIVERFILEINCOMPAT_TOKEN;
      return(token::NUM); }
(?i:"BadLUTClass") {
      yylval->val = BADLUTCLASS_TOKEN;
      return(token::NUM); }
(?i:"ICFileOpenErr") {
      yylval->val = ICFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"ICFileReadErr") {
      yylval->val = ICFILEREADERR_TOKEN;
      return(token::NUM); }
(?i:"InstIDInvalid") {
      yylval->val = INSTIDINVALID_TOKEN;
      return(token::NUM); }
(?i:"CtrlDataInvalid") {
      yylval->val = CTRLDATAINVALID_TOKEN;
      return(token::NUM); }
(?i:"ChanFileOpenErr") {
      yylval->val = CHANFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"ChanFileReadErr") {
      yylval->val = CHANFILEREADERR_TOKEN;
      return(token::NUM); }
(?i:"ChanFileDataErr") {
      yylval->val = CHANFILEDATAERR_TOKEN;
      return(token::NUM); }
(?i:"CCFileOpenErr") {
      yylval->val = CCFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"CCFileReadErr") {
      yylval->val = CCFILEREADERR_TOKEN;
      return(token::NUM); }
(?i:"CCNClassErr") {
      yylval->val = CCNCLASSERR_TOKEN;
      return(token::NUM); }
(?i:"CCSelectError") {
      yylval->val = CCSELECTERROR_TOKEN;
      return(token::NUM); }
(?i:"CCDefaultError") {
      yylval->val = CCDEFAULTERROR_TOKEN;
      return(token::NUM); }
(?i:"LUTFileOpenErr") {
      yylval->val = LUTFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"LUTFileReadErr") {
      yylval->val = LUTFILEREADERR_TOKEN;
      return(token::NUM); }
(?i:"LUTFileDataErr") {
      yylval->val = LUTFILEDATAERR_TOKEN;
      return(token::NUM); }
(?i:"MSIFileOpenErr") {
      yylval->val = MSIFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"MSIFileReadHeadErr") {
      yylval->val = MSIFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"MSIFileReadDataErr") {
      yylval->val = MSIFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"MSIFileEOFErr") {
      yylval->val = MSIFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"MSIFileCloseErr") {
      yylval->val = MSIFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"CfFileOpenErr") {
      yylval->val = CFFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"CfFileReadHeadErr") {
      yylval->val = CFFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"CfFileReadDataErr") {
      yylval->val = CFFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"CfFileEOFErr") {
      yylval->val = CFFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"CfFileCloseErr") {
      yylval->val = CFFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"LsFileOpenErr") {
      yylval->val = LSFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"LsFileReadHeadErr") {
      yylval->val = LSFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"LsFileReadDataErr") {
      yylval->val = LSFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"LsFileEOFErr") {
      yylval->val = LSFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"LsFileCloseErr") {
      yylval->val = LSFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"GeomFileOpenErr") {
      yylval->val = GEOMFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"GeomFileReadHeadErr") {
      yylval->val = GEOMFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"GeomFileReadDataErr") {
      yylval->val = GEOMFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"GeomFileEOFErr") {
      yylval->val = GEOMFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"GeomFileCloseErr") {
      yylval->val = GEOMFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"IntTransErr") {
      yylval->val = INTTRANSERR_TOKEN;
      return(token::NUM); }
(?i:"LocFileOpenErr") {
      yylval->val = LOCFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"LocFileReadHeadErr") {
      yylval->val = LOCFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"LocFileReadDataErr") {
      yylval->val = LOCFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"LocFileEOFErr") {
      yylval->val = LOCFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"LocFileCloseErr") {
      yylval->val = LOCFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMRTMFileOpenErr") {
      yylval->val = LWRTMRTMFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMRTMInstErr") {
      yylval->val = LWRTMRTMINSTERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMRTMDateErr") {
      yylval->val = LWRTMRTMDATEERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMChanErr") {
      yylval->val = LWRTMCHANERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMReadErr") {
      yylval->val = LWRTMREADERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMPFileOpenErr") {
      yylval->val = LWRTMPFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMProfDateErr") {
      yylval->val = LWRTMPROFDATEERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMProfNLatErr") {
      yylval->val = LWRTMPROFNLATERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMProfNLonErr") {
      yylval->val = LWRTMPROFNLONERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMProfErr") {
      yylval->val = LWRTMPROFERR_TOKEN;
      return(token::NUM); }
(?i:"LwRTMProfReadErr") {
      yylval->val = LWRTMPROFREADERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMRTMFileOpenErr") {
      yylval->val = SWRTMRTMFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMRTMInstErr") {
      yylval->val = SWRTMRTMINSTERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMRTMDateErr") {
      yylval->val = SWRTMRTMDATEERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMChanErr") {
      yylval->val = SWRTMCHANERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMReadErr") {
      yylval->val = SWRTMREADERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMPFileOpenErr") {
      yylval->val = SWRTMPFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMProfDateErr") {
      yylval->val = SWRTMPROFDATEERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMProfNLatErr") {
      yylval->val = SWRTMPROFNLATERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMProfNLonErr") {
      yylval->val = SWRTMPROFNLONERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMProfErr") {
      yylval->val = SWRTMPROFERR_TOKEN;
      return(token::NUM); }
(?i:"SwRTMProfReadErr") {
      yylval->val = SWRTMPROFREADERR_TOKEN;
      return(token::NUM); }
(?i:"SPixelIndexing") {
      yylval->val = SPIXELINDEXING_TOKEN;
      return(token::NUM); }
(?i:"SPixelMixed") {
      yylval->val = SPIXELMIXED_TOKEN;
      return(token::NUM); }
(?i:"SPixelCloudPix") {
      yylval->val = SPIXELCLOUDPIX_TOKEN;
      return(token::NUM); }
(?i:"SPixelAmeth") {
      yylval->val = SPIXELAMETH_TOKEN;
      return(token::NUM); }
(?i:"SPixelInvalid") {
      yylval->val = SPIXELINVALID_TOKEN;
      return(token::NUM); }
(?i:"SPixelType") {
      yylval->val = SPIXELTYPE_TOKEN;
      return(token::NUM); }
(?i:"SPixelSkip") {
      yylval->val = SPIXELSKIP_TOKEN;
      return(token::NUM); }
(?i:"SPixelGeomSol") {
      yylval->val = SPIXELGEOMSOL_TOKEN;
      return(token::NUM); }
(?i:"SPixelGeomSat") {
      yylval->val = SPIXELGEOMSAT_TOKEN;
      return(token::NUM); }
(?i:"SPixelGeomRel") {
      yylval->val = SPIXELGEOMREL_TOKEN;
      return(token::NUM); }
(?i:"SPixelSurfglint") {
      yylval->val = SPIXELSURFGLINT_TOKEN;
      return(token::NUM); }
(?i:"SPixelLocLat") {
      yylval->val = SPIXELLOCLAT_TOKEN;
      return(token::NUM); }
(?i:"SPixelLocLon") {
      yylval->val = SPIXELLOCLON_TOKEN;
      return(token::NUM); }
(?i:"SPixelSurfErr") {
      yylval->val = SPIXELSURFERR_TOKEN;
      return(token::NUM); }
(?i:"BkpFileOpenErr") {
      yylval->val = BKPFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"GetRTMLwMaxLat") {
      yylval->val = GETRTMLWMAXLAT_TOKEN;
      return(token::NUM); }
(?i:"GetRTMLwMinLat") {
      yylval->val = GETRTMLWMINLAT_TOKEN;
      return(token::NUM); }
(?i:"GetRTMLwMaxLon") {
      yylval->val = GETRTMLWMAXLON_TOKEN;
      return(token::NUM); }
(?i:"GetRTMLwMinLon") {
      yylval->val = GETRTMLWMINLON_TOKEN;
      return(token::NUM); }
(?i:"GetSurfaceMeth") {
      yylval->val = GETSURFACEMETH_TOKEN;
      return(token::NUM); }
(?i:"GetSurfaceNDNoCh") {
      yylval->val = GETSURFACENDNOCH_TOKEN;
      return(token::NUM); }
(?i:"GetRsCentPix") {
      yylval->val = GETRSCENTPIX_TOKEN;
      return(token::NUM); }
(?i:"GetRsAvMeth") {
      yylval->val = GETRSAVMETH_TOKEN;
      return(token::NUM); }
(?i:"GetLwSwRTMLat") {
      yylval->val = GETLWSWRTMLAT_TOKEN;
      return(token::NUM); }
(?i:"GetLwSwRTMLon") {
      yylval->val = GETLWSWRTMLON_TOKEN;
      return(token::NUM); }
(?i:"APMethErr") {
      yylval->val = APMETHERR_TOKEN;
      return(token::NUM); }
(?i:"FGMethErr") {
      yylval->val = FGMETHERR_TOKEN;
      return(token::NUM); }
(?i:"CloudClassMethErr") {
      yylval->val = CLOUDCLASSMETHERR_TOKEN;
      return(token::NUM); }
(?i:"XMDADMeth") {
      yylval->val = XMDADMETH_TOKEN;
      return(token::NUM); }
(?i:"XMDADBounds") {
      yylval->val = XMDADBOUNDS_TOKEN;
      return(token::NUM); }
(?i:"XSDADMeth") {
      yylval->val = XSDADMETH_TOKEN;
      return(token::NUM); }
(?i:"InvCholNotPosDef") {
      yylval->val = INVCHOLNOTPOSDEF_TOKEN;
      return(token::NUM); }
(?i:"OutFileOpenErr") {
      yylval->val = OUTFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"DiagFileWriteErr") {
      yylval->val = DIAGFILEWRITEERR_TOKEN;
      return(token::NUM); }
(?i:"AlbFileOpenErr") {
      yylval->val = ALBFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"AlbFileReadHeadErr") {
      yylval->val = ALBFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"AlbFileReadDataErr") {
      yylval->val = ALBFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"AlbFileEOFErr") {
      yylval->val = ALBFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"ScanFileOpenErr") {
      yylval->val = SCANFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"ScanFileReadHeadErr") {
      yylval->val = SCANFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"ScanFileReadDataErr") {
      yylval->val = SCANFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"ScanFileEOFErr") {
      yylval->val = SCANFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"ScanFileCloseErr") {
      yylval->val = SCANFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"LUTIntflagErr") {
      yylval->val = LUTINTFLAGERR_TOKEN;
      return(token::NUM); }
(?i:"RTMIntflagErr") {
      yylval->val = RTMINTFLAGERR_TOKEN;
      return(token::NUM); }
(?i:"CWP_Calcerror") {
      yylval->val = CWP_CALCERROR_TOKEN;
      return(token::NUM); }
(?i:"IllumFileOpenErr") {
      yylval->val = ILLUMFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"IllumFileReadHeadErr") {
      yylval->val = ILLUMFILEREADHEADERR_TOKEN;
      return(token::NUM); }
(?i:"IllumFileReadDataErr") {
      yylval->val = ILLUMFILEREADDATAERR_TOKEN;
      return(token::NUM); }
(?i:"IllumFileEOFErr") {
      yylval->val = ILLUMFILEEOFERR_TOKEN;
      return(token::NUM); }
(?i:"PrimaryFileOpenErr") {
      yylval->val = PRIMARYFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"SecondaryFileOpenErr") {
      yylval->val = SECONDARYFILEOPENERR_TOKEN;
      return(token::NUM); }
(?i:"PrimaryFileDefinitionErr") {
      yylval->val = PRIMARYFILEDEFINITIONERR_TOKEN;
      return(token::NUM); }
(?i:"SecondaryFileDefinitionErr") {
      yylval->val = SECONDARYFILEDEFINITIONERR_TOKEN;
      return(token::NUM); }
(?i:"PrimaryFileWriteErr") {
      yylval->val = PRIMARYFILEWRITEERR_TOKEN;
      return(token::NUM); }
(?i:"SecondaryFileWriteErr") {
      yylval->val = SECONDARYFILEWRITEERR_TOKEN;
      return(token::NUM); }
(?i:"PrimaryFileCloseErr") {
      yylval->val = PRIMARYFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"SecondaryFileCloseErr") {
      yylval->val = SECONDARYFILECLOSEERR_TOKEN;
      return(token::NUM); }
(?i:"deflate_level") {
      yylval->val = DEFLATE_LEVEL_TOKEN;
      return(token::NUM); }
(?i:"shuffle_flag") {
      yylval->val = SHUFFLE_FLAG_TOKEN;
      return(token::NUM); }
<FID_T_SC>(?i:"Data_Dir") {
      yylval->str_v = FID_T_VARIABLE.Data_Dir;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"Out_Dir") {
      yylval->str_v = FID_T_VARIABLE.Out_Dir;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"SAD_Dir") {
      yylval->str_v = FID_T_VARIABLE.SAD_Dir;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"Filename") {
      yylval->str_v = FID_T_VARIABLE.Filename;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"Config") {
      yylval->str_v = FID_T_VARIABLE.Config;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"MSI") {
      yylval->str_v = FID_T_VARIABLE.MSI;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"LWRTM") {
      yylval->str_v = FID_T_VARIABLE.LWRTM;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"SWRTM") {
      yylval->str_v = FID_T_VARIABLE.SWRTM;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"PRTM") {
      yylval->str_v = FID_T_VARIABLE.PRTM;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"LS") {
      yylval->str_v = FID_T_VARIABLE.LS;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"CF") {
      yylval->str_v = FID_T_VARIABLE.CF;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"Geo") {
      yylval->str_v = FID_T_VARIABLE.Geo;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"Loc") {
      yylval->str_v = FID_T_VARIABLE.Loc;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"Alb") {
      yylval->str_v = FID_T_VARIABLE.Alb;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"BkP") {
      yylval->str_v = FID_T_VARIABLE.BkP;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"L2_primary") {
      yylval->str_v = FID_T_VARIABLE.L2_primary;
      BEGIN(INITIAL); return(token::STR_VAR); }
<FID_T_SC>(?i:"L2_secondary") {
      yylval->str_v = FID_T_VARIABLE.L2_secondary;
      BEGIN(INITIAL); return(token::STR_VAR); }
<IND_T_SC>(?i:"NAvail") {
      yylval->int_v = new Target<int> (IND_T_VARIABLE.NAvail);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"ICh") {
      fort_alloc_int_1d(&IND_T_VARIABLE.ICh, &IND_T_VARIABLE.ICh_dim0, IND_T_ICh_DIMS);
      yylval->int_v = new Target<int> (IND_T_VARIABLE.ICh, IND_T_VARIABLE.ICh_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"Y_Id_legacy") {
      yylval->int_v = new Target<int> (IND_T_VARIABLE.Y_Id_legacy, 6);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"NMixed") {
      yylval->int_v = new Target<int> (IND_T_VARIABLE.NMixed);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"YMixed") {
      fort_alloc_int_1d(&IND_T_VARIABLE.YMixed, &IND_T_VARIABLE.YMixed_dim0, IND_T_YMixed_DIMS);
      yylval->int_v = new Target<int> (IND_T_VARIABLE.YMixed, IND_T_VARIABLE.YMixed_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"NWvl") {
      yylval->int_v = new Target<int> (IND_T_VARIABLE.NWvl);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"WvlIdx") {
      fort_alloc_int_1d(&IND_T_VARIABLE.WvlIdx, &IND_T_VARIABLE.WvlIdx_dim0, IND_T_WvlIdx_DIMS);
      yylval->int_v = new Target<int> (IND_T_VARIABLE.WvlIdx, IND_T_VARIABLE.WvlIdx_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"XMax") {
      yylval->int_v = new Target<int> (IND_T_VARIABLE.XMax);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"YMax") {
      yylval->int_v = new Target<int> (IND_T_VARIABLE.YMax);
      BEGIN(INITIAL); return(token::INT_VAR); }
<IND_T_SC>(?i:"channel_proc_flag") {
      fort_alloc_int_1d(&IND_T_VARIABLE.channel_proc_flag, &IND_T_VARIABLE.channel_proc_flag_dim0, IND_T_channel_proc_flag_DIMS);
      yylval->int_v = new Target<int> (IND_T_VARIABLE.channel_proc_flag, IND_T_VARIABLE.channel_proc_flag_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<SURFREF_T_SC>(?i:"RsSelm") {
      yylval->int_v = new Target<int> (SURFREF_T_VARIABLE.RsSelm);
      BEGIN(INITIAL); return(token::INT_VAR); }
<SURFREF_T_SC>(?i:"SRsSelm") {
      yylval->int_v = new Target<int> (SURFREF_T_VARIABLE.SRsSelm);
      BEGIN(INITIAL); return(token::INT_VAR); }
<SURFREF_T_SC>(?i:"use_full_brdf") {
      yylval->bool_v = new Target<bool> (SURFREF_T_VARIABLE.use_full_brdf);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<SURFREF_T_SC>(?i:"allow_a_default_surface") {
      yylval->bool_v = new Target<bool> (SURFREF_T_VARIABLE.allow_a_default_surface);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<SURFREF_T_SC>(?i:"B") {
      fort_alloc_float_2d(&SURFREF_T_VARIABLE.B, &SURFREF_T_VARIABLE.B_dim0, &SURFREF_T_VARIABLE.B_dim1, SURFREF_T_B_DIMS);
      yylval->float_v = new Target<float> (SURFREF_T_VARIABLE.B, SURFREF_T_VARIABLE.B_dim0, SURFREF_T_VARIABLE.B_dim1);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<SURFREF_T_SC>(?i:"Sb") {
      fort_alloc_float_2d(&SURFREF_T_VARIABLE.Sb, &SURFREF_T_VARIABLE.Sb_dim0, &SURFREF_T_VARIABLE.Sb_dim1, SURFREF_T_Sb_DIMS);
      yylval->float_v = new Target<float> (SURFREF_T_VARIABLE.Sb, SURFREF_T_VARIABLE.Sb_dim0, SURFREF_T_VARIABLE.Sb_dim1);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<SURFREF_T_SC>(?i:"Cb") {
      yylval->float_v = new Target<float> (SURFREF_T_VARIABLE.Cb);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<SURFREF_T_SC>(?i:"add_fractional") {
      yylval->bool_v = new Target<bool> (SURFREF_T_VARIABLE.add_fractional);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<SURFREF_T_SC>(?i:"diagonal_SRs") {
      yylval->bool_v = new Target<bool> (SURFREF_T_VARIABLE.diagonal_SRs);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<SURFREF_T_SC>(?i:"solar_factor") {
      yylval->bool_v = new Target<bool> (SURFREF_T_VARIABLE.solar_factor);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<EQMPN_T_SC>(?i:"SySelm") {
      yylval->int_v = new Target<int> (EQMPN_T_VARIABLE.SySelm);
      BEGIN(INITIAL); return(token::INT_VAR); }
<EQMPN_T_SC>(?i:"Homog") {
      yylval->bool_v = new Target<bool> (EQMPN_T_VARIABLE.Homog);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<EQMPN_T_SC>(?i:"CoReg") {
      yylval->bool_v = new Target<bool> (EQMPN_T_VARIABLE.CoReg);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<INVPAR_T_SC>(?i:"ConvTest") {
      yylval->bool_v = new Target<bool> (INVPAR_T_VARIABLE.ConvTest);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<INVPAR_T_SC>(?i:"MqStart") {
      yylval->float_v = new Target<float> (INVPAR_T_VARIABLE.MqStart);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<INVPAR_T_SC>(?i:"MqStep") {
      yylval->float_v = new Target<float> (INVPAR_T_VARIABLE.MqStep);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<INVPAR_T_SC>(?i:"MaxIter") {
      yylval->int_v = new Target<int> (INVPAR_T_VARIABLE.MaxIter);
      BEGIN(INITIAL); return(token::INT_VAR); }
<INVPAR_T_SC>(?i:"Ccj") {
      yylval->float_v = new Target<float> (INVPAR_T_VARIABLE.Ccj);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<INVPAR_T_SC>(?i:"XScale") {
      yylval->float_v = new Target<float> (INVPAR_T_VARIABLE.XScale, MAXSTATEVAR_TOKEN);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<INVPAR_T_SC>(?i:"XLLim") {
      yylval->float_v = new Target<float> (INVPAR_T_VARIABLE.XLLim, MAXSTATEVAR_TOKEN);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<INVPAR_T_SC>(?i:"XULim") {
      yylval->float_v = new Target<float> (INVPAR_T_VARIABLE.XULim, MAXSTATEVAR_TOKEN);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<INVPAR_T_SC>(?i:"always_take_GN") {
      yylval->bool_v = new Target<bool> (INVPAR_T_VARIABLE.always_take_GN);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<INVPAR_T_SC>(?i:"dont_iter_convtest") {
      yylval->bool_v = new Target<bool> (INVPAR_T_VARIABLE.dont_iter_convtest);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<INVPAR_T_SC>(?i:"disable_Ss") {
      yylval->bool_v = new Target<bool> (INVPAR_T_VARIABLE.disable_Ss);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<QC_T_SC>(?i:"MaxJ") {
      yylval->float_v = new Target<float> (QC_T_VARIABLE.MaxJ);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<QC_T_SC>(?i:"MaxS") {
      yylval->float_v = new Target<float> (QC_T_VARIABLE.MaxS, MAXSTATEVAR_TOKEN);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"FID"){DELIM} {
      BEGIN(FID_T_SC); }
<CTRL_T_SC>(?i:"InstName") {
      yylval->str_v = CTRL_T_VARIABLE.InstName;
      BEGIN(INITIAL); return(token::STR_VAR); }
<CTRL_T_SC>(?i:"LUTClass") {
      yylval->str_v = CTRL_T_VARIABLE.LUTClass;
      BEGIN(INITIAL); return(token::STR_VAR); }
<CTRL_T_SC>(?i:"Approach") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.Approach);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"Ind"){DELIM} {
      BEGIN(IND_T_SC); }
<CTRL_T_SC>(?i:"DOY") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.DOY);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"Run_ID") {
      yylval->str_v = CTRL_T_VARIABLE.Run_ID;
      BEGIN(INITIAL); return(token::STR_VAR); }
<CTRL_T_SC>(?i:"RS"){DELIM} {
      BEGIN(SURFREF_T_SC); }
<CTRL_T_SC>(?i:"EqMPN"){DELIM} {
      BEGIN(EQMPN_T_SC); }
<CTRL_T_SC>(?i:"Invpar"){DELIM} {
      BEGIN(INVPAR_T_SC); }
<CTRL_T_SC>(?i:"QC"){DELIM} {
      BEGIN(QC_T_SC); }
<CTRL_T_SC>(?i:"MaxSolZen") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.MaxSolZen);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"MaxSatZen") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.MaxSatZen);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"MinRelAzi") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.MinRelAzi);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"Sunset") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.Sunset);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"i_equation_form") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.i_equation_form);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"get_T_dv_from_T_0d") {
      yylval->bool_v = new Target<bool> (CTRL_T_VARIABLE.get_T_dv_from_T_0d);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<CTRL_T_SC>(?i:"LUTIntSelm") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.LUTIntSelm);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"RTMIntSelm") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.RTMIntSelm);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"CloudType") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.CloudType);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"Bkpl") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.Bkpl);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"Max_SDAD") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.Max_SDAD);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"sabotage_inputs") {
      yylval->bool_v = new Target<bool> (CTRL_T_VARIABLE.sabotage_inputs);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<CTRL_T_SC>(?i:"process_cloudy_only") {
      yylval->bool_v = new Target<bool> (CTRL_T_VARIABLE.process_cloudy_only);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<CTRL_T_SC>(?i:"NTypes_to_process") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.NTypes_to_process);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"Types_to_process") {
      yylval->char_v = new Target<char> (CTRL_T_VARIABLE.Types_to_process, MAXTYPES_TOKEN);
      BEGIN(INITIAL); return(token::CHAR_VAR); }
<CTRL_T_SC>(?i:"Surfaces_to_skip") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.Surfaces_to_skip);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"second_aot_ch") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.second_aot_ch, 1);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"verbose") {
      yylval->bool_v = new Target<bool> (CTRL_T_VARIABLE.verbose);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<CTRL_T_SC>(?i:"tau_chans") {
      fort_alloc_int_1d(&CTRL_T_VARIABLE.tau_chans, &CTRL_T_VARIABLE.tau_chans_dim0, CTRL_T_tau_chans_DIMS);
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.tau_chans, CTRL_T_VARIABLE.tau_chans_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"r_e_chans") {
      fort_alloc_int_1d(&CTRL_T_VARIABLE.r_e_chans, &CTRL_T_VARIABLE.r_e_chans_dim0, CTRL_T_r_e_chans_DIMS);
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.r_e_chans, CTRL_T_VARIABLE.r_e_chans_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"ir_chans") {
      fort_alloc_int_1d(&CTRL_T_VARIABLE.ir_chans, &CTRL_T_VARIABLE.ir_chans_dim0, CTRL_T_ir_chans_DIMS);
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.ir_chans, CTRL_T_VARIABLE.ir_chans_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"ReChans") {
      fort_alloc_int_1d(&CTRL_T_VARIABLE.ReChans, &CTRL_T_VARIABLE.ReChans_dim0, CTRL_T_ReChans_DIMS);
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.ReChans, CTRL_T_VARIABLE.ReChans_dim0);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"do_new_night_retrieval") {
      yylval->bool_v = new Target<bool> (CTRL_T_VARIABLE.do_new_night_retrieval);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<CTRL_T_SC>(?i:"do_CTX_correction") {
      yylval->bool_v = new Target<bool> (CTRL_T_VARIABLE.do_CTX_correction);
      BEGIN(INITIAL); return(token::BOOL_VAR); }
<CTRL_T_SC>(?i:"CTP_correction_limit") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.CTP_correction_limit);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"Ap") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.Ap, MAXSTATEVAR_TOKEN, MAXILLUM_TOKEN);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"Fg") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.Fg, MAXSTATEVAR_TOKEN, MAXILLUM_TOKEN);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"Xb") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.Xb, MAXSTATEVAR_TOKEN);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"X0") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.X0, MAXSTATEVAR_TOKEN);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"Sx") {
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.Sx, MAXSTATEVAR_TOKEN);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"Sy") {
      fort_alloc_float_2d(&CTRL_T_VARIABLE.Sy, &CTRL_T_VARIABLE.Sy_dim0, &CTRL_T_VARIABLE.Sy_dim1, CTRL_T_Sy_DIMS);
      yylval->float_v = new Target<float> (CTRL_T_VARIABLE.Sy, CTRL_T_VARIABLE.Sy_dim0, CTRL_T_VARIABLE.Sy_dim1);
      BEGIN(INITIAL); return(token::FLOAT_VAR); }
<CTRL_T_SC>(?i:"Nx") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.Nx, MAXILLUM_TOKEN);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"NxJ") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.NxJ, MAXILLUM_TOKEN);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"X") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.X, MAXSTATEVAR_TOKEN, MAXILLUM_TOKEN);
      BEGIN(INITIAL); return(token::INT_VAR); }
<CTRL_T_SC>(?i:"XJ") {
      yylval->int_v = new Target<int> (CTRL_T_VARIABLE.XJ, MAXSTATEVAR_TOKEN, MAXILLUM_TOKEN);
      BEGIN(INITIAL); return(token::INT_VAR); }

 /* Rule for parent structure */
<INITIAL>(?i:"Ctrl"){DELIM} {
    BEGIN(CTRL_T_SC); }

 /* Manually coded rules */

 /* Things to ignore */
<*>[ \t]                    ;
<*>#[^\n]*                  ;

 /* Syntactic punctuation */
\n                          return(token::ENDL);   // One command per line
=                           return(token::EQ);     // Delimit variable from value
[\[\(]                      return(token::OPENB);  // Begin array slice
[\]\)]                      return(token::CLOSEB); // End array slice
,                           return(token::COMMA);  // 1D array delimiter
;                           return(token::SEMI);   // 2D array delimiter
:                           return(token::COLON);  // Dimension delimiter

 /* Data */
[\+\-0-9\.e]+             { yylval->val = atof(yytext);
                            return(token::NUM); }  // Float or ints
\".+\"                    { yylval->str = new std::string(yytext, yyleng-1);
                            yylval->str->erase(0, 1);
                            return(token::WORD); } // Quoted string
[^ \t%\.#\n=\[\(\]\),;:]+ { yylval->str = new std::string(yytext, yyleng);
                            return(token::WORD); } // String

%%

void WRAPPER_NAME_C(
#include XCAT3(INC_PATH, c_arg, inc)
    int* stat, const char* filename) {
    // Open file and parser
    yyin = fopen(filename, "r");
    PARENT_STRUCT_TYPE strct;
    yy::CLASS_NAME parser(strct);

#include XCAT3(INC_PATH, c_cpy, inc)

    // Parse file
    *stat = parser.parse();

#include XCAT3(INC_PATH, c_cpy2, inc)

    // Tidying
    fclose(yyin);
    yylex_destroy();
    return;
}

