!-------------------------------------------------------------------------------
! Name: orac_constants.F90
!
! Purpose:
! Module defining constants used by the ORAC code, e.g. maximum array sizes.
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
! 2002/01/17, AS: New LwRTM error code. Re-numbered higher error codes to make
!    room.
! 2011/02/23, AS: Added ECPLogReclen, fed up of irritating line breaks.
! 2011/03/09, AS: New error codes for albedo files. Updated XMDAD error on a
!    priori F.
! 2011/04/05, AS: Removed selection methods SAD and SDAD. Codes SlmSAD, SelmSDAD
!    removed, other Selm codes re-numbered. SelmMDAD renamed SelmMeas to improve
!    clarity.
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
! 2011/12/08, MJ: added data type definitions for netcdf output and filename
!    lengths
! 2012/01/30, MJ: added ditherm3 as parameter.
! 2012/06/15, CP: added iluum error flags
! 2012/06/22, CP: added sacura option
! 2012/10/01, CP: added case where 1.6 of 3.7 channel is missing during the day
! 2013/xx/xx, MJ: changes lengths of filenames and some formatting,
!    adds fill value for double precision.
! 2013/11/19, MJ: changes RefMax to 1.5 from 1.2 and BTMin to 140.0 from 150.0
! 2014/01/26, GM: Cleaned up code.
! 2014/04/03, MJ: adds some fill value definitions
! 2014/05/22, GM: Added RTMIntMeth and LUTIntMeth constants.
! 2014/07/24, AP: made FlagMin|Max type byte
! 2014/08/01, GM: Rename illumination conditions for missing conditions.
! 2014/08/15, GM: d2r is a derived constant. It should be computed which will be
!    done at compile time since both operands are constants.
! 2014/08/30, GM: Use common_constants and remove pi and d2r as they are in
!    common_constants.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/11/20, OS: increased BTMax from 330 to 350 K, thus providing retrieval
!    results for warm land surfaces (e.g. Sahara, Namib)
! 2015/01/09, CP: Added IRfbd for cloud albedo calculations.
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
! 2015/08/20, AP: Removed AUXErr terms. Added indices for XIndex, B, and
!    Approach along with SelmPrev.
! 2015/10/19, GM: Added index for Bext LUT.
! 2015/11/19, GM: Added index values for the Ctrl%Ind%Y_Id_legacy index.
! 2016/07/27, GM: Added Class constants and renamed Approach constants to
!    distinguish them from the Class constants to support the multilayer
!    retrieval.
! 2017/03/16, GT: Changes for single-view aerosol retrieval mode.
! 2017/07/05, AP: Move ISS before ISP so that the wavelength-dependent values
!    are last (to simlify the variables_retrieved flag).
! 2018/09/20, SP: Removed MaxPLevels.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module ORAC_constants_m

   use common_constants_m

   implicit none

   integer            :: i_ORAC                      ! Loop variable

   ! Maximum string lengths
   integer, parameter :: FilenameLen      = path_length
                                                    ! Max length of filenames
   integer, parameter :: InstNameLen      = 24      ! Max length of instrument name

   ! Maximum array lengths
   integer, parameter :: MaxNumSolar      = 24      ! Max no. of solar channels
   integer, parameter :: MaxNumThermal    = 16      ! Max no. of thermal channels
   integer, parameter :: MaxCloudType     = 5       ! Max no. of cloud types to be
   integer, parameter :: MaxCRProps       = 14      ! Max no. of properties in SAD_LUT arrays
   integer, parameter :: MaxTypes         = 11      ! Number of possible cloud/aerosol types


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
   real, parameter    :: RhoMin           =  0.0    ! BRDF parameters
   real, parameter    :: RhoMax           =  1.0
   real, parameter    :: RhoErrMin        =  0.0    ! BRDF uncertainties
   real, parameter    :: RhoErrMax        =  1.0
   real, parameter    :: CorrMin          = -1.0
   real, parameter    :: CorrMax          =  1.0

   real, parameter    :: TxcMin           = 0.0
   real, parameter    :: TxcMax           = 1.5
   real, parameter    :: RxcMin           = 0.
   real, parameter    :: RxcMax           = 300.    ! T2R at 13.5um and 350K
                                                    ! up to nearest 100th
   real, parameter    :: EmsMin           = 0.0
   real, parameter    :: EmsMax           = 1.0
   real, parameter    :: MinPriorCTP      = 10.0

   ! Missing data (fill) values
   real, parameter    :: MissingXn        = -999.   ! Value for "missing data" used as output when a SPixel is not processed.
   real, parameter    :: MissingSn        = 1.0e+08 ! Value for "missing data" used as error output when a SPixel is not processed.

   ! Physical constants/parameters
   real, parameter    :: rhowat           = 1.0     ! Density of water
   real, parameter    :: rhoice           = 0.9167  ! Density of ice
   real, parameter    :: qextwat          = 2.0     ! Extinction efficiency for water
   real, parameter    :: qextice          = 2.1     ! Extinction efficiency for ice

   ! Error values
   real, parameter    :: MDADErrTau       = 1.0e+08 ! Error on a priori Tau if set by MDAD method.
   real, parameter    :: MDADErrPc        = 1.0e+08 ! Error on a priori Pc if set by MDAD method.
   real, parameter    :: MDADErrF         = 0.1     ! Error on a priori F if set by MDAD method.
   real, parameter    :: AUXErrTsLand     = 5.0     ! Error on a priori Ts if set by AUX method (land value).
   real, parameter    :: AUXErrTsSea      = 2.0     ! Error on a priori Ts if set by AUX method (sea value).

   ! For conversion from month/day to day number within year.
   integer, parameter :: days_in_month(12) = &
      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

   ! Codes for RTM interpolation (to Pc) methods
   integer, parameter :: RTMIntMethLinear = 0
   integer, parameter :: RTMIntMethSpline = 1
   integer, parameter :: RTMIntMethNone   = 2

   ! Codes for FM LUT interpolation methods
   integer, parameter :: LUTIntMethLinear  = 0
   integer, parameter :: LUTIntMethBicubic = 1

   ! Index of legacy channels in Ctrl%Ind%Y_Id_legacy
   integer, parameter :: N_legacy         = 6

   integer, parameter :: I_legacy_0_6x    = 1
   integer, parameter :: I_legacy_0_8x    = 2
   integer, parameter :: I_legacy_1_6x    = 3
   integer, parameter :: I_legacy_3_xx    = 4
   integer, parameter :: I_legacy_11_x    = 5
   integer, parameter :: I_legacy_12_x    = 6

   ! Index of CRP array parameter in interpolated arrays (e.g. CRPOut in
   ! functions Set_CRP_Solar and Set_CRP_Thermal).
   integer, parameter :: IBext            = 1       ! Index of Bext data in array
   integer, parameter :: IBextRat         = 2       ! "     "  BextRat " "  "
   integer, parameter :: IRbd             = 3       ! "     "  Rbd     " "  "
   integer, parameter :: IRfbd            = 4       ! "     "  Rfbd    " "  "
   integer, parameter :: IRd              = 5       ! "     "  Rd      " "  "
   integer, parameter :: IRfd             = 6       ! "     "  Rfd     " "  "
   integer, parameter :: ITb              = 7       ! "     "  Tb      " "  "
   integer, parameter :: ITb_u            = 8       ! "     "  Tb_u    " "  "
   integer, parameter :: ITbd             = 9       ! "     "  Tbd     " "  "
   integer, parameter :: ITfbd            = 10      ! "     "  Tfbd    " "  "
   integer, parameter :: ITfbd_u          = 11      ! "     "  Tfbd_u  " "  "
   integer, parameter :: ITd              = 12      ! "     "  Td      " "  "
   integer, parameter :: ITfd             = 13      ! "     "  Tfd     " "  "
   integer, parameter :: IEm              = 14      ! "     "  Em      " "  "

   ! Index of XIndex array in GetSurface
   integer, parameter :: ISwan_S          = 1
   integer, parameter :: ISwan_P          = 2
   integer, parameter :: MaxSwan_X        = 2

   ! Index of B in GetSurface and ReadDriver
   integer, parameter :: ISea             = 1
   integer, parameter :: ILand            = 2
   integer, parameter :: MaxSurf          = 2

   ! 3rd index of d_CRP array in functions FM_Thermal, FM_Solar and CRP LUT
   ! interpolation functions. Also used as index of state vector array X in FM
   ! and Invert_Marquardt
   integer, parameter :: ITau             = 1       ! Index of tau, cloud optical depth
   integer, parameter :: IRe              = 2       ! Index of re, effective radius
   integer, parameter :: IPc              = 3       ! Index of Pc, cloud pressure
   integer, parameter :: IFr              = 4       ! Index of F, cloud fraction (can't
                                                    ! use "If", unfortunately)
   integer, parameter :: ITau2            = 5
   integer, parameter :: IRe2             = 6
   integer, parameter :: IPc2             = 7
   integer, parameter :: IFr2             = 8
   integer, parameter :: ISG              = 9
   integer, parameter :: ITs              = 10      ! Index of Ts, surface temperature

   ! Swansea surface reflectance parameters. ISS is the wavelength-dependent
   ! s parameter and ISP is the directionally-dependent P parameter.
   integer, parameter :: ISP(MaxNumViews) = &
        [(i_ORAC+ITs, i_ORAC = 1, MaxNumViews)]
   ! Oxford surface reflectance parameters. Though we're unlikely to retrieve
   ! all the BRDF parameters, space needs to be made in the Jacobian.
   integer, parameter :: IRs(MaxNumSolar, MaxRho_XX) = &
        reshape([(i_ORAC+ISP(MaxNumViews), i_ORAC = 1, MaxNumSolar*MaxRho_XX)], &
                [MaxNumSolar, MaxRho_XX])
   integer, parameter :: ISS(MaxNumSolar) = IRs(:, 1)
   ! NOTE: MaxNumSolar arrays should be treated as if of length Ctrl%Ind%NSolar

   ! Determine max. no. of state vector elements from indices
   integer, parameter :: MaxStateVar      = IRs(MaxNumSolar, MaxRho_XX)

   ! Illumination conditions (day/twilight/night) for arrays Ctrl%FG and AP
   integer, parameter :: IDay             = 1
   integer, parameter :: ITwi             = 2
   integer, parameter :: INight           = 3
   integer, parameter :: MaxIllum         = 3

   ! Constant values used for selection method
   integer, parameter :: SelmCtrl         = 1
   integer, parameter :: SelmMeas         = 2
   integer, parameter :: SelmAux          = 3
   integer, parameter :: SelmPrev         = 4

   ! Bits used with Diag%QCFlag
   integer, parameter :: ConvBit          = 0
   integer, parameter :: CostBit          = 1
   integer, parameter :: IceBit           = 2
   integer, parameter :: MaskBit          = 3
   integer, parameter :: DoFNBit          = 4
   integer, parameter :: ElevBit          = 5
   integer, parameter :: GlintBit         = 6
   integer, parameter :: LimitBit         = 7

   ! Retrieval classes (for Ctrl%Class)
   integer, parameter :: ClsCldWat        = 1
   integer, parameter :: ClsCldIce        = 2
   integer, parameter :: ClsAerOx         = 3
   integer, parameter :: ClsAerSw         = 4
   integer, parameter :: ClsAerBR         = 5
   integer, parameter :: ClsAshEyj        = 6

   ! Retrieval approaches (for Ctrl%Approach)
   integer, parameter :: AppCld1L         = 1 ! Single layer cloud
   integer, parameter :: AppCld2L         = 2 ! 2-layer cloud
   integer, parameter :: AppAerOx         = 3 ! Multi-view aeroosl with Ox FM
   integer, parameter :: AppAerSw         = 4 ! Mulit-view aeroosl with Swansea FM
   integer, parameter :: AppAerO1         = 5 ! Single-view aerosol (like GlobAEROSOL)


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
   integer, parameter :: SPixelSkip                 = 1146
   integer, parameter :: SPixelGeomSol              = 1150
   integer, parameter :: SPixelGeomSat              = 1151
   integer, parameter :: SPixelGeomRel              = 1152
   integer, parameter :: SPixelSurfglint            = 1153
   integer, parameter :: SPixelLocLat               = 1154
   integer, parameter :: SPixelLocLon               = 1155
   integer, parameter :: SPixelSurfErr              = 1156
   integer, parameter :: GetRTMLwMaxLat             = 1170
   integer, parameter :: GetRTMLwMinLat             = 1171
   integer, parameter :: GetRTMLwMaxLon             = 1172
   integer, parameter :: GetRTMLwMinLon             = 1173
   integer, parameter :: GetSurfaceMeth             = 1180
   integer, parameter :: GetSurfaceNDNoCh           = 1181
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
   integer, parameter :: InvMarquardtSxErr          = 2000
   integer, parameter :: InvMarquardtFMX0Err        = 2001
   integer, parameter :: InvMarquardtSyX0Err        = 2002
   integer, parameter :: InvMarquardtDJErr          = 2003
   integer, parameter :: InvMarquardtFMErr          = 2004
   integer, parameter :: InvMarquardtSyErr          = 2005
   integer, parameter :: InvMarquardtD2JErr         = 2006

   ! NetCDF deflate level
   integer, parameter :: deflate_level = 0

   ! Shuffling to improve compression
   logical, parameter :: shuffle_flag  = .False.

   ! Indicator that the retrieval hit a limit
   integer, parameter :: LimitHitUpper = 1
   integer, parameter :: LimitHitLower = 2
   integer, parameter :: LimitHitClose = 3

end module ORAC_constants_m
