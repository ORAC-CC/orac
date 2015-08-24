!-------------------------------------------------------------------------------
! Name: ECPConstants.F90
!
! Purpose:
! Module defining constants used by the ECP code, e.g. maximum array sizes.
!
! History:
! 2000/08/03, AS: Original version.
! 2000/11/23, AS: SADChanForm updated.
! 2000/11/23, AS: Added MaxPLevels.
! 2000/12/01, AS: Now using an environment variable for driver file name. Hard-
!    coded path removed. New error code added. Renamed MaxNumSunZen MaxNumSolZen
! 2001/01/17, AS: Added index for Em and increased MaxCRProps to allow for Em in
!    CRPOut array in FM Solar and Thermal routines.
! 2001/01/24, AS: Added index constants for the state vector array.
! 2001/02/09, AS: Started using constants to denote breakpoint levels for
!    individual subroutines.
! 2001/05/22, AS: Checked into RCS for safety. Continuing to make changes/
!    additions in all areas.
! 2001/06/24, AS: Checked in again. Many more changes.
! 2001/08/15, AS: ECP main program working. More changes in all areas.
!    **************** ECV work starts here *************************************
! 2011/02/08, AS: Re-introducing changes made in late 2001/2002.
! 2001/11/28, AS: New constants for upper and lower limits used in checking in
!    GetSPixel: flag values, lat, long, geometry, reflectances and brightness
!    temps.
! 2002/01/17, AS: New LwRTM error code. Re-numbered higher error codes to
!    make room.
! 2011/02/23, AS: Added ECPLogReclen, fed up of irritating line breaks.
! 2011/03/09, AS: New error codes for albedo files. Updated XMDAD error on
!    a priori F.
! 2011/04/05, AS: Removed selection methods SAD and SDAD. Codes SlmSAD, SelmSDAD
!    removed, other Selm codes re-numbered. SelmMDAD renamed SelmMeas to
!    improve clarity.
! 2011/04/14, AS: Extension to handle multiple views.
!    Extended FilenameLen to allow for long paths.
! 2011/06/14, CP: remove maximum sizes for LUT arrays
! 2011/06/28, CP: added in scanline file error values
! 2011/08/01, CP: new swrtm error values
! 2011/08/08, CP: changed format of LUTS
! 2011/09/22, CP: changed getLeRTM to GetSWrtm
! 2011/10/04, CA: added LUT/RTM Intflag errors
! 2011/10/07, CP: added in variables to calculate CWP rho and qext
! 2011/11/04, CP: changed values of AUXErrTsSea/land
! 2011/11/25, CP: changed values maxnummeas
! 2011/12/08, MJ: added data type definitions for netcdf output
!    and filenamelengths
! 2012/01/30, MJ: added ditherm3 as parameter.
! 2012/06/15, CP: added iluum error flags
! 2012/06/22, CP: added sacura option
! 2012/10/01, CP: added case where 1.6 of 3.7 channel is missing during the day
! 2013/xx/xx, MJ: changes lengths of filenames and some formatting,
!    adds fill value for double precision.
! 2013/11/19, MJ: changes refmax to 1.5 from 1.2 and btmin to 140.0 from 150.0
! 2014/01/26, GM: Cleaned up code.
! 2014/04/03, MJ: adds some fill value definitions
! 2014/05/22, GM: Added RTMIntMeth and LUTIntMeth constants.
! 2014/07/24, AP: made FlagMin|Max type byte
! 2014/08/01, GM: Rename illumination conditions for missing conditions.
! 2014/08/15, GM: d2r is a derived constant. It should be computed which will be
!    done at compile time since both operands are constants.
! 2014/08/30, GM: Use common_constants and remove pi and d2r as
!    they are in common_constants.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/11/20, OS: increased BTMax from 330 to 350 K, thus providing
!    retrieval results for warm land surfaces (e.g. Sahara, Namib)
! 2015/01/09, CP: Added IRFBd for cloud albedo calculations.
! 2015/01/12, AP: Added bit positions for Ctrl%Ind%Ch_Is.
! 2015/03/03, AP: Added terms for aerosol retrieval.
! 2015/03/11, GM: Increase MaxNumMeas and MaxNumSolar to 36 and 20, respectively.
! 2015/07/14, AP: Add aerosol terms.
! 2015/07/26, GM: Added deflate_level and shuffle_flag.
! 2015/07/27, AP: Removed unused variables ECPLogReclen, MaxDiagFlags,
!    MaxCloudClass, AMeth..., DiFlag..., CCFileName. Added MaxNumViews, MaxTypes.
!    Renamed CloudFlag terms to Type.
! 2015/08/05, AP: Extend state vector to include BRDF terms.
! 2015/08/18, GM: Added LW and SW RTM minimum and maximum values.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module ECP_constants

   use common_constants

   implicit none

   integer            :: i_ECP                      ! Loop variable

   ! Maximum string lengths
   integer, parameter :: FilenameLen      = 2048    ! Max. length of filenames
   integer, parameter :: InstNameLen      = 16      ! Max. length of instrument name

   ! Maximum array lengths
   integer, parameter :: MaxNumMeas       = 36      ! Max no. of measurement channels
   integer, parameter :: MaxNumViews      = 2       ! Max no. of measurement views
   integer, parameter :: MaxNumSolar      = 20      ! Max no. of solar channels
   integer, parameter :: MaxCloudType     = 5       ! Max. no of cloud types to be
   integer, parameter :: MaxPLevels       = 50      ! Max. no. of pressure levels (in SPixel RTM arrays)
   integer, parameter :: MaxCRProps       = 11      ! Max no. of properties in SAD_LUT arrays
   integer, parameter :: MaxRho_XX        = 4       ! Max no. of BRDF parameters
   integer, parameter :: MaxTypes         = 10      ! Number of possible cloud/aerosol types


   ! Tolerance values
   real, parameter    :: ditherm3         = 1.0E-3  ! Some small value
   real, parameter    :: ditherm6         = 1.0E-6  ! Some even smaller value
   real, parameter    :: ditherm15        = 1.0E-15 ! Tiny value


   ! Parameters for range checking of data values (used in Get_SPixel)
   integer(byte), parameter :: FlagMin    = 0       ! Land/sea flag
   integer(byte), parameter :: FlagMax    = 1
   integer(byte), parameter :: TypeMin    = 0       ! Cloud/aerosol type flag
   integer(byte), parameter :: TypeMax    = 9
   real, parameter    :: SolZenMin        = 0.0     ! Solar zenith angle
   real, parameter    :: SatZenMin        = 0.0     ! Satellite zenith angle
   real, parameter    :: SatZenMax        = 90.0
   real, parameter    :: RelAziMin        = 0.0     ! Relative azimuth angle
   real, parameter    :: RelAziMax        = 180.0
   real, parameter    :: LatMin           = -90.0   ! Latitude
   real, parameter    :: LatMax           =  90.0
   real, parameter    :: LonMin           = -180.0  ! Longitude
   real, parameter    :: LonMax           =  180.0
   real, parameter    :: RefMin           =  0.0    ! Reflectance
   real, parameter    :: RefMax           =  1.5
   real, parameter    :: BTMin            =  140.0  ! Brightness temperature
   real, parameter    :: BTMax            =  350.0

   real, parameter    :: TxcMin           = 0.0
   real, parameter    :: TxcMax           = 1.5
   real, parameter    :: RxcMin           = 0.
   real, parameter    :: RxcMax           = 300.    ! T2R at 13.5um and 350K
                                                    ! up to nearest 100th
   real, parameter    :: EmsMin           = 0.0
   real, parameter    :: EmsMax           = 1.0

   ! Missing data (fill) values
   real, parameter    :: MissingXn        = -999.   ! Value for "missing data" used as output when a SPixel is not processed.
   real, parameter    :: MissingSn        = 1.0e+08 ! Value for "missing data" used as error output when a SPixel is not processed.

   ! Physical constants/parameters
   real, parameter    :: rhowat           = 1.0     ! Density of water
   real, parameter    :: rhoice           = 0.9167  ! Density of ice
   real, parameter    :: qextwat          = 2.0     ! Extinction coefficient water
   real, parameter    :: qextice          = 2.1     ! Extinction coefficient ice

   real, parameter    :: g_wmo            = 9.80665 ! Gravity

   ! Error values
   real, parameter    :: MDADErrTau       = 1.0e+08 ! Error on a priori Tau if set by MDAD method.
   real, parameter    :: MDADErrPc        = 1.0e+08 ! Error on a priori Pc if set by MDAD method.
   real, parameter    :: MDADErrF         = 0.1     ! Error on a priori F if set by MDAD method.
   real, parameter    :: AUXErrTsLand     = 5.0     ! Error on a priori Ts if set by AUX method (land value).
   real, parameter    :: AUXErrTsSea      = 2.0     ! Error on a priori Ts if set by AUX method (sea value).

   ! For conversion from month/day to day number within year.
   integer, parameter, dimension(12) :: days_in_month = &
      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

   ! Codes for RTM interpolation (to Pc) methods
   integer, parameter :: RTMIntMethLinear = 0
   integer, parameter :: RTMIntMethSpline = 1
   integer, parameter :: RTMIntMethNone   = 2

   ! Codes for FM LUT interpolation methods
   integer, parameter :: LUTIntMethLinear  = 0
   integer, parameter :: LUTIntMethBicubic = 1

   ! Index of CRP array parameter in interpolated arrays (e.g. CRPOut in
   ! functions Set_CRP_Solar and Set_CRP_Thermal).
   integer, parameter :: IRBd             = 1       ! Index of RBd data in array
   integer, parameter :: IRFBd            = 2       !  "    "  RFBd "   "   "
   integer, parameter :: IRd              = 3       !  "    "  Rd   "   "   "
   integer, parameter :: IRFd             = 4       !  "    "  RFd  "   "   "
   integer, parameter :: ITB              = 5       !  "    "  TB   "   "   "
   integer, parameter :: ITB_u            = 6       !  "    "  TB   "   "   "
   integer, parameter :: ITBd             = 7       !  "    "  TBd  "   "   "
   integer, parameter :: ITd              = 8       !  "    "  Td   "   "   "
   integer, parameter :: ITFBd            = 9       !  "    "  TFBd "   "   "
   integer, parameter :: ITFd             = 10      !  "    "  TFd  "   "   "
   integer, parameter :: IEm              = 11      !  "    "  Em   "   "   "

   ! Index of CRP array parameter in interpolated arrays (e.g. CRPOut in
   ! functions Set_CRP_Solar and Set_CRP_Thermal). These are also used to index
   ! IRs in FM_Solar for cloud retrievals (so ensure they don't exceed NIRs).
   integer, parameter :: IRho_0V          = 1       ! Index of rho_0v data in array
   integer, parameter :: IRho_0D          = 2       !  "    "  rho_0d  "   "   "
   integer, parameter :: IRho_DV          = 3       !  "    "  rho_dv  "   "   "
   integer, parameter :: IRho_DD          = 4       !  "    "  rho_dd  "   "   "

   ! 3rd index of d_CRP array in functions FM_Thermal, FM_Solar and CRP LUT
   ! interpolation functions. Also used as index of state vector array X in FM
   ! and Invert_Marquardt
   integer, parameter :: ITau             = 1       ! Index of tau, cloud optical depth
   integer, parameter :: IRe              = 2       ! Index of re, effective radius

   ! Index of values in X (state vector) array - use ITau, IRe from above, plus
   integer, parameter :: IPc              = 3       ! Index of Pc, cloud pressure
   integer, parameter :: IFr              = 4       ! Index of F, cloud fraction (can't
                                                    ! use "If", unfortunately)
   integer, parameter :: ITs              = 5       ! Index of Ts, surface temperature

   ! Oxford surface reflectance parameters. Though we're unlikely to retrieve
   ! all the BRDF parameters, space needs to be made in the Jacobian.
   integer, parameter :: IRs(MaxNumSolar, MaxRho_XX) = &
        reshape([(i_ECP+iTs, i_ECP = 1, MaxNumSolar*MaxRho_XX)], &
                [MaxNumSolar, MaxRho_XX])
   ! Swansea surface reflectance parameters. ISS is the wavelength-dependent
   ! s parameter and ISP is the directionally-dependent P parameter.
   integer, parameter :: ISS(MaxNumSolar) = IRs(:, 1)
   integer, parameter :: ISP(MaxNumViews) = &
        [(i_ECP+IRs(MaxNumSolar, MaxRho_XX), i_ECP = 1, MaxNumViews)]

   ! Determine max. no. of state vector elements from indices
   integer, parameter :: MaxStateVar      = ISP(MaxNumViews)

   ! Illumination conditions (day/twilight/night) for arrays Ctrl%FG and AP
   integer, parameter :: IDay             = 1
   integer, parameter :: ITwi             = 2
   integer, parameter :: INight           = 3
   integer, parameter :: MaxIllum         = 3

   ! Constant values used for selection method
   integer, parameter :: SelmCtrl         = 1
   integer, parameter :: SelmMeas         = 2
   integer, parameter :: SelmAux          = 3

   ! Bit positions used in Ctrl%Ind%Ch_Is flag
   integer, parameter :: SolarBit         = 0
   integer, parameter :: ThermalBit       = 1

   ! Bits used with Diag%QCFlag
   integer, parameter :: CostBit          = 0


   ! Retrieval approaches (for Ctrl%Approach)
   integer, parameter :: CldWat = 1
   integer, parameter :: CldIce = 2
   integer, parameter :: AerOx  = 3
   integer, parameter :: AerSw  = 4
   integer, parameter :: AshEyj = 5

   ! General format statements
   character(len=*), parameter :: FNForm       = '(a2048)' ! Format for I/O of filenames
   character(len=*), parameter :: LUTArrayForm = '(10E14.6)'
   ! Used in outputting SAD_chan structs (for debugging only)
   character(len=*), parameter :: SADChanForm  = &
      '(/, 2A, f10.2, /, i1, 1x, 15(f9.2, 1x), /, i1, 1x, 14(f9.2, 1x))'

   ! Breakpoint levels for individual subroutines.  The parameter name is
   ! BkpL_<subroutine name>. Some routines may require multiple breakpoint
   ! levels. Use "_1", "_2" etc.
   integer, parameter :: BkpL_FM                = 2
   integer, parameter :: BkpL_FM_Solar          = 2
   integer, parameter :: BkpL_FM_Thermal        = 2
   integer, parameter :: BkpL_Get_SPixel        = 2
   integer, parameter :: BkpL_Interpol_Solar    = 2
   integer, parameter :: BkpL_Interpol_Thermal  = 2
   integer, parameter :: BkpL_InvertMarquardt_1 = 1
   integer, parameter :: BkpL_InvertMarquardt_2 = 2
   integer, parameter :: BkpL_InvertMarquardt_3 = 3
   integer, parameter :: BkpL_InvertMarquardt_4 = 4
   integer, parameter :: BkpL_Read_LUT_1        = 1
   integer, parameter :: BkpL_Read_LUT_2        = 2

   ! Error conditions: starting from 1000, a range of 10 error conditions is
   ! allowed for each function. So error values 1000 - 10009 relate to the read
   ! driver file function, 1010 - 1019 to the next function, etc.
   integer, parameter :: DriverFileOpenErr          = 1000
   integer, parameter :: DriverFileReadErr          = 1001
   integer, parameter :: DriverFileNotFound         = 1002
   integer, parameter :: DriverFileDataErr          = 1003
   integer, parameter :: AMethInvalid               = 1004 ! SPixel averaging method
   integer, parameter :: LimitMethInvalid           = 1005
   integer, parameter :: SegSizeInvalid             = 1006 ! Image segment size
   integer, parameter :: DriverFileIncompat         = 1007
   integer, parameter :: BadLUTClass                = 1008
   integer, parameter :: ICFileOpenErr              = 1010
   integer, parameter :: ICFileReadErr              = 1011
   integer, parameter :: InstIDInvalid              = 1012
   integer, parameter :: CtrlDataInvalid            = 1013
   integer, parameter :: ChanFileOpenErr            = 1020
   integer, parameter :: ChanFileReadErr            = 1021
   integer, parameter :: ChanFileDataErr            = 1022
   integer, parameter :: CCFileOpenErr              = 1030
   integer, parameter :: CCFileReadErr              = 1031
   integer, parameter :: CCNClassErr                = 1032
   integer, parameter :: CCSelectError              = 1033
   integer, parameter :: CCDefaultError             = 1034
   integer, parameter :: LUTFileOpenErr             = 1040
   integer, parameter :: LUTFileReadErr             = 1041
   integer, parameter :: LUTFileDataErr             = 1042
   integer, parameter :: MSIFileOpenErr             = 1050
   integer, parameter :: MSIFileReadHeadErr         = 1051
   integer, parameter :: MSIFileReadDataErr         = 1052
   integer, parameter :: MSIFileEOFErr              = 1053
   integer, parameter :: MSIFileCloseErr            = 1054
   integer, parameter :: CfFileOpenErr              = 1060
   integer, parameter :: CfFileReadHeadErr          = 1061
   integer, parameter :: CfFileReadDataErr          = 1062
   integer, parameter :: CfFileEOFErr               = 1063
   integer, parameter :: CfFileCloseErr             = 1064
   integer, parameter :: LsFileOpenErr              = 1070
   integer, parameter :: LsFileReadHeadErr          = 1071
   integer, parameter :: LsFileReadDataErr          = 1072
   integer, parameter :: LsFileEOFErr               = 1073
   integer, parameter :: LsFileCloseErr             = 1070
   integer, parameter :: GeomFileOpenErr            = 1080
   integer, parameter :: GeomFileReadHeadErr        = 1081
   integer, parameter :: GeomFileReadDataErr        = 1082
   integer, parameter :: GeomFileEOFErr             = 1083
   integer, parameter :: GeomFileCloseErr           = 1084
   integer, parameter :: IntTransErr                = 1090
   integer, parameter :: LocFileOpenErr             = 1100
   integer, parameter :: LocFileReadHeadErr         = 1101
   integer, parameter :: LocFileReadDataErr         = 1102
   integer, parameter :: LocFileEOFErr              = 1103
   integer, parameter :: LocFileCloseErr            = 1104
   integer, parameter :: LwRTMRTMFileOpenErr        = 1110
   integer, parameter :: LwRTMRTMInstErr            = 1111
   integer, parameter :: LwRTMRTMDateErr            = 1112
   integer, parameter :: LwRTMChanErr               = 1113
   integer, parameter :: LwRTMReadErr               = 1114
   integer, parameter :: LwRTMPFileOpenErr          = 1120
   integer, parameter :: LwRTMProfDateErr           = 1121
   integer, parameter :: LwRTMProfNLatErr           = 1122
   integer, parameter :: LwRTMProfNLonErr           = 1123
   integer, parameter :: LwRTMProfErr               = 1124
   integer, parameter :: LwRTMProfReadErr           = 1125
   integer, parameter :: SwRTMRTMFileOpenErr        = 1130
   integer, parameter :: SwRTMRTMInstErr            = 1131
   integer, parameter :: SwRTMRTMDateErr            = 1132
   integer, parameter :: SwRTMChanErr               = 1133
   integer, parameter :: SwRTMReadErr               = 1134
   integer, parameter :: SwRTMPFileOpenErr          = 1135
   integer, parameter :: SwRTMProfDateErr           = 1136
   integer, parameter :: SwRTMProfNLatErr           = 1137
   integer, parameter :: SwRTMProfNLonErr           = 1138
   integer, parameter :: SwRTMProfErr               = 1139
   integer, parameter :: SwRTMProfReadErr           = 1129
   integer, parameter :: SPixelIndexing             = 1140
   integer, parameter :: SPixelMixed                = 1141
   integer, parameter :: SPixelCloudPix             = 1142
   integer, parameter :: SPixelAmeth                = 1143
   integer, parameter :: SPixelInvalid              = 1144
   integer, parameter :: SPixelType                 = 1145
   integer, parameter :: SPixelGeomSol              = 1150
   integer, parameter :: SPixelGeomSat              = 1151
   integer, parameter :: SPixelGeomRel              = 1152
   integer, parameter :: SPixelLocLat               = 1154
   integer, parameter :: SPixelLocLon               = 1155
   integer, parameter :: SPixelSurfglint            = 1153
   integer, parameter :: BkpFileOpenErr             = 1160
   integer, parameter :: GetRTMLwMaxLat             = 1170
   integer, parameter :: GetRTMLwMinLat             = 1171
   integer, parameter :: GetRTMLwMaxLon             = 1172
   integer, parameter :: GetRTMLwMinLon             = 1173
   integer, parameter :: GetSurfaceMeth             = 1180
   integer, parameter :: GetRsCentPix               = 1190
   integer, parameter :: GetRsAvMeth                = 1191
   integer, parameter :: GetLwSwRTMLat              = 1190
   integer, parameter :: GetLwSwRTMLon              = 1191
   integer, parameter :: APMethErr                  = 1200
   integer, parameter :: FGMethErr                  = 1201
   integer, parameter :: CloudClassMethErr          = 1210
   integer, parameter :: XMDADMeth                  = 1220
   integer, parameter :: XMDADBounds                = 1221
   integer, parameter :: XSDADMeth                  = 1230
   integer, parameter :: InvCholNotPosDef           = 1240
   integer, parameter :: OutFileOpenErr             = 1250
   integer, parameter :: DiagFileWriteErr           = 1251
   integer, parameter :: AlbFileOpenErr             = 1270
   integer, parameter :: AlbFileReadHeadErr         = 1271
   integer, parameter :: AlbFileReadDataErr         = 1272
   integer, parameter :: AlbFileEOFErr              = 1273
   integer, parameter :: ScanFileOpenErr            = 1280
   integer, parameter :: ScanFileReadHeadErr        = 1281
   integer, parameter :: ScanFileReadDataErr        = 1282
   integer, parameter :: ScanFileEOFErr             = 1283
   integer, parameter :: ScanFileCloseErr           = 1280
   integer, parameter :: LUTIntflagErr              = 1284
   integer, parameter :: RTMIntflagErr              = 1285
   integer, parameter :: CWP_Calcerror              = 1300
   integer, parameter :: IllumFileOpenErr           = 1310
   integer, parameter :: IllumFileReadHeadErr       = 1311
   integer, parameter :: IllumFileReadDataErr       = 1312
   integer, parameter :: IllumFileEOFErr            = 1313
   integer, parameter :: PrimaryFileOpenErr         = 1400
   integer, parameter :: SecondaryFileOpenErr       = 1401
   integer, parameter :: PrimaryFileDefinitionErr   = 1402
   integer, parameter :: SecondaryFileDefinitionErr = 1403
   integer, parameter :: PrimaryFileWriteErr        = 1404
   integer, parameter :: SecondaryFileWriteErr      = 1405
   integer, parameter :: PrimaryFileCloseErr        = 1406
   integer, parameter :: SecondaryFileCloseErr      = 1407

   ! NetCDF deflate level
   integer, parameter :: deflate_level = 0

   ! Shuffling to improve compression
   logical, parameter :: shuffle_flag  = .False.

end module ECP_constants
