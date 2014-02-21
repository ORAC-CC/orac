!-------------------------------------------------------------------------------
! Name:
!    ECP_constants
!
! Purpose:
!    Module defining constants used by the ECP code, e.g. maximum array sizes.
!
! Description:
!    Contains a set of declarations and parameter statements for constant
!    values.
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!     3rd Aug 2000, Andy Smith : Original version.
!    23rd Nov 2000, Andy Smith :
!       SADChanForm updated.
!    24th Nov 2000, Andy Smith :
!       Added MaxPLevels.
!     1st Dec 2000, Andy Smith :
!       Now using an environment variable for driver file name.
!       Hard-coded path removed. New error code added.
!       Renamed MaxNumSunZen MaxNumSolZen
!    17th Jan 2001, Andy Smith :
!       Added index for Em and increased MaxCRProps to allow for Em in CRPOut
!       array in FM Solar and Thermal routines.
!    24th Jan 2001, Andy Smith :
!       Added index constants for the state vector array.
!     9th Feb 2001, Andy Smith:
!       Started using constants to denote breakpoint levels for individual
!       subroutines.
!    22nd May 2001, Andy Smith:
!       Checked into RCS for safety. Continuing to make changes/additions in all
!       areas.
!    24th July 2001, Andy Smith:
!       Checked in again. Many more changes.
!    15th Aug 2001, Andy Smith:
!       ECP main program working. More changes in all areas.
!    **************** ECV work starts here *************************************
!     8th Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    28th Nov 2001, Andy Smith:
!       New constants for upper and lower limits used in checking in
!       GetSPixel: flag values, lat, long, geometry, reflectances and brightness
!       temps.
!    (17th Jan 2001 - should read 2002?, Andy Smith:)
!       New LwRTM error code. Re-numbered higher error codes to make room.
!    23rd Feb 2011, Andy Smith:
!       Added ECPlogReclen, fed up of irritating line breaks.
!     9th Mar 2011, Andy Smith:
!       New error codes for albedo files. Updated XMDAD error on a priori F.
!     5th Apr 2011, Andy Smith:
!       Removed selection methods SAD and SDAD. Codes SlmSAD, SelmSDAD removed,
!       other Selm codes re-numbered.
!       SelmMDAD renamed SelmMeas to improve clarity.
!    14th Apr 2011, Andy Smith:
!       Extension to handle multiple views.
!       Extended FilenameLen to allow for long paths.
!    14th Jun 2011, Caroline Poulsen: remove maximum sizes for LUT arrays
!    28th Jul 2011, Caroline Poulsen: added in scanline file error values
!     1st Aug 2011, Caroline Poulsen: newswrtm error values
!     8th Aug 2011, Caroline Poulsen: changed format of LUTS
!    22nd Sep 2011, Caroline Poulsen: changed getLeRTM to GetSWrtm
!     4th Oct 2011, Chris Arnold: added LUT/RTM Intflag errors
!     7th Oct 2011, Caroline Poulsen: added in variables to calulate CWP rho and
!       qext
!     4th Nov 2011, Caroline Poulsen: changed values of AUXErrTsSea/land
!    25th Nov 2011, Caroline Poulsen: changed values maxnummeas
!     8th Dec 2011, Matthias Jerg: added data type definitions for netcdf output
!       and filenamelengths
!    2012/01/30, Matthias Jerg: added ditherm3 as parameter.
!    2012/06/15, C. Poulsen: added iluum error flags
!    2012/06/22, C. Poulsen: added sacura option
!    2012/10/01, C. Poulsen: added case where 1.6 of 3.7 channel is missing
!       during the day
!    2013/xx/xx, MJ: changes lengths of filenames and soem formatting, adds fill
!       value for double precision.
!    2013/11/19, MJ: changes refmax to 1.5 from 1.2 and btmin to 140.0 from 150.0
!    2014/01/26, Greg McGarragh: Cleaned up code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module ECP_constants

   ! Maximum string lengths
   integer, parameter :: ECPLogReclen     = 132     ! Record length in ECP log file,
                                                    ! to avoid annoying line breaks.
   integer, parameter :: FilenameLen      = 2048    ! Max. length of filenames
   integer, parameter :: InstNameLen      = 16      ! Max. length of instrument name
   integer, parameter :: NetcdfVarLength  = 150
   integer, parameter :: NetcdfUnitLength = 150

   ! Maximum array lengths
   integer, parameter :: MaxDiagFlags     = 14      ! Max no. of flags in diagnostic flag
   integer, parameter :: MaxNumMeas       = 6       ! Max no. of measurement channels
   integer, parameter :: MaxNumSolar      = 4       ! Max no. of solar channels(ATSR -specfic)
   integer, parameter :: MaxCloudClass    = 3       ! Max no. of cloud classes
   integer, parameter :: MaxCloudType     = 5       ! Max. no of cloud types to be
   integer, parameter :: MaxPLevels       = 50      ! Max. no. of pressure levels (in SPixel RTM arrays)
   integer, parameter :: MaxStateVar      = 5       ! Max. number of state variables processed

   ! Tolerance values
   real, parameter    :: ditherm3         = 1.0E-3  ! Some small value
   real, parameter    :: ditherm6         = 1.0E-6  ! Some even smaller value
   real, parameter    :: ditherm15        = 1.0E-15 ! Tiny value

   ! Parameters for range checking of data values (used in Get_SPixel)
   integer, parameter :: FlagMin          = 0       ! Checking for flag values (land/sea or cloud flags)                                    !
   integer, parameter :: FlagMax          = 1       ! Max. and min. values used in range
   real, parameter    :: CloudMin         = 0.0
   real, parameter    :: CloudMax         = 1.0     ! Max. and min. values used in range checking for cloud flag values
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
   real, parameter    :: BTMax            =  330.0

   ! Missing data (fill) values
   real, parameter    :: MissingXn        = -999.   ! Value for "missing data" used as output when a SPixel is not processed.
   real, parameter    :: MissingSn        = 1.0e+08 ! Value for "missing data" used as error output when a SPixel is not processed.

   ! Mathematical constants
   real, parameter    :: Pi               = 3.1415927 ! Pi required for solar constant

   ! Physical constants/parameters
   real, parameter    :: rhowat           = 1.0     ! Density of water
   real, parameter    :: rhoice           = 0.9167  ! Density of ice
   real, parameter    :: qextwat          = 2.0     ! Extinction coefficient water
   real, parameter    :: qextice          = 2.1     ! Extinction coefficient ice

   ! Error values
   real, parameter    :: MDADErrTau       = 1.0e+08 ! Error on a priori Tau if set by MDAD method.
   real, parameter    :: MDADErrPc        = 1.0e+08 ! Error on a priori Pc if set by MDAD method.
   real, parameter    :: MDADErrF1        = 0.1
   real, parameter    :: MDADErrF2        = 0.01
   real, parameter    :: ErrFthreshold    = 9.0     ! Error on a priori F if set by MDAD method.
   real, parameter    :: AUXErrTsLand     = 5.0     ! Error on a priori Ts if set by ! AUX method (land value).
   real, parameter    :: AUXErrTsSea      = 2.0     ! Error on a priori Ts if set by ! AUX method (sea value).

   character(len=*), parameter :: months  = &
      'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'        ! String used to convert month name to number.

   integer, parameter, dimension(12) :: days_in_month = &
      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For conversion from month, day to day number within year.

   ! The following max sizes apply to the SAD_LUT arrays
   integer, parameter :: MaxCRProps       = 9

   ! Codes for super-pixel averaging methods
   integer, parameter :: AMethAll         = 0       ! All pixels in super-pixel included
   integer, parameter :: AMethCloudy      = 1       ! Cloudy pixels only
   integer, parameter :: AMethCentral     = 2       ! Central pixel only

   ! Index of CRP array parameter in interpolated arrays (e.g. CRPOut in
   ! functions Set_CRP_Solar and Set_CRP_Thermal).

   integer, parameter :: IRBd             = 1       ! Index of RBd data in array
   integer, parameter :: ITB              = 2       !  "    "  TB   "   "   "
   integer, parameter :: ITBd             = 3       !  "    "  TBd  "   "   "
   integer, parameter :: ITFBd            = 4       !  "    "  TFBd "   "   "
   integer, parameter :: ITd              = 5       !  "    "  Td   "   "   "
   integer, parameter :: ITFd             = 6       !  "    "  TFd  "   "   "
   integer, parameter :: IRd              = 7       !  "    "  Rd   "   "   "
   integer, parameter :: IRFd             = 8       !  "    "  RFd  "   "   "
   integer, parameter :: IEm              = 9       !  "    "  Em   "   "   "

   ! 3rd index of d_CRP array in functions FM_Thermal, FM_Solar and CRP LUT
   ! interpolation functions. Also used as index of state vector array X in FM
   ! and Invert_Marquardt
   integer, parameter :: Itau             = 1       ! Index of tau, cloud optical depth
   integer, parameter :: Ire              = 2       ! Index of re, effective radius

   ! Index of values in X (state vector) array - use ITau, IRe from above, plus:
   ! (also used for the Ref and d_Ref arrays, which include an Rs component).
   ! Phase can also be included, e.g. in first guess arrays from Ctrl, in which
   ! case it occupies the 6th position (same as Rs in other cases). N.B. could
   ! be dangerous!
   integer, parameter :: IPc              = 3       ! Index of Pc, cloud pressure
   integer, parameter :: IFr              = 4       ! Index of F, cloud fraction (can't
                                                    ! use "If", unfortunately)
   integer, parameter :: ITs              = 5       ! Index of Ts, surface temperature
   integer, parameter :: IRs              = 6       ! Index of Rs, surface reflectance (in Reflectance arrays).
   integer, parameter :: IPhase           = 6       ! Index of phase when included in arrays with state vector info. N.B. same as IRs.

   ! Cloud phase (water, ice)
   integer, parameter :: IPhaseWat        = 1       ! Water
   integer, parameter :: IPhaseIce        = 2       ! Ice

   ! Illumination conditions (day/twilight/night) for arrays Ctrl%FG and AP
   integer, parameter :: IDay             = 1
   integer, parameter :: ITwi             = 2
   integer, parameter :: INight           = 3
   integer, parameter :: IDaynore         = 4

   ! Constant values used for selection method
   integer, parameter :: SelmCtrl         = 1
   integer, parameter :: SelmMeas         = 2
   integer, parameter :: SelmAux          = 3
   integer, parameter :: Sacura           = 4

   ! Super-pixel quality control flag: bit numbers for flagging problems with
   ! different super-pixel data

   ! Non-fatal values: indicate problems not necessarily affecting the whole
   ! super-pixel.
   integer, parameter :: SPixCloudFl      = 0       ! 1 or more cloud flags out of range
   integer, parameter :: SPixLandFl       = 1       !   "   "   land sea flags " " "
   integer, parameter :: SPixSolZen       = 2       !   "   "   solar zenith angles " " "
   integer, parameter :: SPixSatZen       = 3       !   "   "   Sat zenith angles " " "
   integer, parameter :: SPixRelAzi       = 4       !   "   "   Relative azimuth " " "
   integer, parameter :: SPixLat          = 5       !   "   "   Latitudes  " " "
   integer, parameter :: SPixLon          = 6       !   "   "   Longitudes " " "
   integer, parameter :: SPixRef          = 7       !   "   "   Reflectances " " "
   integer, parameter :: SPixTemp         = 8       !   "   "   Temperatures " " "

   ! Fatal values: whole Spixel affected
   integer, parameter :: SPixAll          = 9       ! All pixels in SPixel out of range (because of 1 or more of the above)
   integer, parameter :: SPixNoCloud      = 10      ! All cloud flags 0.
   integer, parameter :: SPixNoAvge       = 11      ! Can't get SPixel cloud average
   integer, parameter :: SPixGeom         = 12      ! Problem from Get_Geometry routine
   integer, parameter :: SPixLoc          = 13      ! Problem from Get_Location routine
   integer, parameter :: SPixRTM          = 14      ! Problem from Get_RTM routine
   integer, parameter :: SPixMeas         = 15      ! Problem from Get_Measurements
   integer, parameter :: SPixSurf         = 16      ! Problem from Get_Surface
   integer, parameter :: SPixFGAP         = 17      ! Problem from Get_X (First Guess/A Priori setting)
   integer, parameter :: SPixIllum        = 18      ! Problem from Get_illum routine
   integer, parameter :: SpixNoProc       = 31      ! Do not process super-pixel. Earlier flags indicate why not.

   ! Indices of diagnostic flags in array Ctrl%Diagl
   ! See structure definition for Diag to get full meaning of diagnostic params.
   integer, parameter :: DiFlagQC         = 1
   integer, parameter :: DiFlagIter       = 2
   integer, parameter :: DiFlagPhCh       = 3
   integer, parameter :: DiFlagCost       = 4
   integer, parameter :: DiFlagSt1        = 5
   integer, parameter :: DiFlagSs1        = 6
   integer, parameter :: DiFlagSt2        = 7
   integer, parameter :: DiFlagSs2        = 8
   integer, parameter :: DiFlagYFit       = 9
   integer, parameter :: DiFlagXFit       = 10
   integer, parameter :: DiFlagAP         = 11
   integer, parameter :: DiFlagFG         = 12
   integer, parameter :: DiFlagSx         = 13
   integer, parameter :: DiFlagSy         = 14

   ! Config file names:

   ! Instrument configuration
   character(len=*), parameter :: ICFileName   = 'ECP_inst_config.sad'

   ! Cloud Class config file
   character(len=*), parameter :: CCFileName   = 'ECP_CloudClass_config.sad'

   ! General format statements
   character(len=*), parameter :: FNForm       = '(a2048)' ! Format for I/O of filenames
   character(len=*), parameter :: LUTArrayForm = '(10E14.6)'
   ! Used in outputting SAD_chan structs (for debugging only)
   character(len=*), parameter :: SADChanForm  = &
      '(/, 2A, f10.2, /, i1, 1x, 15(f9.2, 1x), /, i1, 1x, 14(f9.2, 1x))'

   ! Breakpoint levels for individual subroutines.  The parameter name is
   ! BkpL_<subroutine name>. Some routines may require multiple breakpoint
   ! levels. Use "_1", "_2" etc.
   integer, parameter :: BkpL_Read_LUT_1        = 1
   integer, parameter :: BkpL_Read_LUT_2        = 2
   integer, parameter :: BkpL_Get_SPixel        = 2
   integer, parameter :: BkpL_Interpol_Solar    = 2
   integer, parameter :: BkpL_FM_Solar          = 2
   integer, parameter :: BkpL_Interpol_Thermal  = 2
   integer, parameter :: BkpL_FM_Thermal        = 2
   integer, parameter :: BkpL_FM                = 2
   integer, parameter :: BkpL_InvertMarquardt_1 = 1
   integer, parameter :: BkpL_InvertMarquardt_2 = 2
   integer, parameter :: BkpL_InvertMarquardt_3 = 3
   integer, parameter :: BkpL_InvertMarquardt_4 = 4


   ! Error conditions: starting from 1000, a range of 10 error conditions is
   ! allowed for each function. So errorvalues 1000 - 10009 relate to the read
   ! driver file function, 1010 - 1019 to the next function, etc.
   integer, parameter :: DriverFileOpenErr          = 1000
   integer, parameter :: DriverFileReadErr          = 1001
   integer, parameter :: DriverFileNotFound         = 1002
   integer, parameter :: DriverFileDataErr          = 1003
   integer, parameter :: AMethInvalid               = 1004 ! SPixel averaging method
   integer, parameter :: LimitMethInvalid           = 1005
   integer, parameter :: SegSizeInvalid             = 1006 ! Image segment size
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
   integer, parameter :: SPixelCentPix              = 1140
   integer, parameter :: SPixelAllPix               = 1141
   integer, parameter :: SPixelCloudPix             = 1142
   integer, parameter :: SPixelAmeth                = 1143
   integer, parameter :: SPixelInvalid              = 1144
   integer, parameter :: SPixelCloudFrac            = 1145
   integer, parameter :: SPixelGeomSol              = 1150
   integer, parameter :: SPixelGeomSat              = 1151
   integer, parameter :: SPixelGeomRel              = 1152
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
   integer, parameter :: Spixelillum                = 1290
   integer, parameter :: CWP_Calcerror              = 1300
   integer, parameter :: illumFileOpenErr           = 1310
   integer, parameter :: illumFileReadHeadErr       = 1311
   integer, parameter :: illumFileReadDataErr       = 1312
   integer, parameter :: illumFileEOFErr            = 1313
   integer, parameter :: PrimaryFileOpenErr         = 1400
   integer, parameter :: SecondaryFileOpenErr       = 1401
   integer, parameter :: PrimaryFileDefinitionErr   = 1402
   integer, parameter :: SecondaryFileDefinitionErr = 1403
   integer, parameter :: PrimaryFileWriteErr        = 1404
   integer, parameter :: SecondaryFileWriteErr      = 1405
   integer, parameter :: PrimaryFileCloseErr        = 1406
   integer, parameter :: SecondaryFileCloseErr      = 1407


   real, parameter    :: real_fill_value            = -999.0
   double precision, parameter :: double_fill_value = -999.0

   real, parameter    :: d2r                        = 0.017453292 ! Pi / 180.0

   integer, parameter :: sint                       = 2
   integer, parameter :: byte                       = 1
   integer, parameter :: nint                       = 4

   integer, parameter :: sreal                      = 4
   integer, parameter :: dreal                      = 8

end module ECP_constants
