!-------------------------------------------------------------------------------
! Name:
!   Ctrl
!
! Purpose:
!    Module defining control structure (Ctrl) for the ECP
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
!
! History:
!    17th Aug 2000, Andy Smith: original version
!    22nd Nov 2000, Andy Smith:
!       Added Out_Dir, Ysolar and Ythermal.
!       Added Run_ID - type may be wrong (we had to guess)
!       Added maxsatzen and maxsunzen
!       Added LimitsFlag to InvPar
!    15th Dec 2000, Kevin Smith:
!       Added Inst%Date and Inst%Time
!       Added ThermalFirst, ThermalLast, SolarFirst, SolarLast
!    10th Jan 2001, Andy Smith:
!       Added comments to clarify meaning of (Ctrl%Ind%) Y, YSolar and YThermal.
!       Ctrl%Ind%Y renamed Y_Id for the same reason.
!       Added Threshold to SPixel
!     5th Feb 2001, Kevin Smith:
!       Added NMixed, number of mixed channels.
!    12th Feb 2001, Kevin Smith:
!       Added MDAD_LW and MDAD_SW, indices for channels to determine FG values
!       of Pc, phase and Tau using the MDAD method.
!    24th April 2001, Andy Smith:
!       New variable for max number of phase changes in inversion
!       (Ctrl%Invpar%MaxPhase).
!       New arrays for specifying active state variables in day/twilight/night
!       conditions (1 array of X and 1 array of Nx for each). Replaces the old
!       X and Nx arrays, which were for all conditions.
!    17th May 2001, Andy Smith:
!       Added CloudType parameter and measurement covariance matrix Sy.
!     6th June 2001, Andy Smith:
!       FG and AP options arrays extended to two dimensions. 2nd dim allows
!       different choices depending on the conditions (day/twilight/night).
!    25th Jun 2001, Andy Smith:
!       New variable Sunset for solar zenith angle that denotes sunset. (Was
!       previously hard-coded).
!     2nd July 2001, Andy Smith:
!       Diagnostic level parameter Diagl changed to an array of flags.
!       New parameters MaxJ and MaxS in new sub-structure QC (quality control)
!       Also Max_SDAD, for limit on distance (in pixels) at which the last
!       retrieved solution is valid for FG/AP setting.
!     6th July 2001, Andy Smith:
!       Removing pointer arrays to allow output of the whole struct in a single
!       statement. Arrays affected: Ctrl%Ind%X_Dy/Tw/Ni, Ctrl%Sy.
!    11th July 2001, Andy Smith:
!       Fixes in CloudClass and Phaset structs. CloudClass arrays were fixed at
!       size 2: replaced by MaxCloudClass constant. CloudClass data removed from
!       Phaset struct: couldn't see any reason why it was there, must have been
!       a typo.
!       Moved Date and Time from the Inst struct to the main struct. Added
!       Day of Year (DOY) required for setting solar constants in ReadChan.
!     3rd Aug 2001, Andy Smith:
!       Added Ctrl%Resoln%SegSize. Size of image segment.
!       Updated Date from character length 8 to 11 (format dd-mmm-yyyy rather
!       than yyyymmdd) and converted time from real to character length 12
!       (hh:mm:ss.sss).
!    23rd Aug 2001, Andy Smith:
!       New parameter in Ind for number of instrument channels available (as
!       opposed to selected by the user). Set by Read_Inst_Config.
!    **************** ECV work starts here *************************************
!    21st Mar 2011, Andy Smith:
!       Removal of phase change functionality. Only 1 cloud class is required
!       now. Cloud class on phase change becomes redundant.
!       State variables for first guess and a priori for ice and water phase
!       become single state vars, limits for ice and water phase replaced by
!       single set of limits.
!       Cloud class selection method now redundant.
!    31st Mar 2011, Andy Smith:
!       Removal of phase change: re-size X0 and FG flags arrays since phase no
!       longer needed.
!     6th Apr 2011, Andy Smith:
!       Removed selection methods SAD and SDAD.
!       Limits flag removed from Ctrl struct as only 1 method remains supported.
!    14th Apr 2011, Andy Smith:
!       Extension to handle multiple views. Number of channels/measurements
!       become allocatable since a specific channel ID can be used in several
!       views. New index for nviews and set of view indices.
!    12th May 2011, Andy Smith:
!       Extension to handle multiple views. Number of surface reflectance values
!       becomes allocatable: number of solar channels can be increased my having
!       multiple views.
!    18th May 2011, Andy Smith:
!       Multiple views(2). Added new Ctrl value NInstViews, to store the number
!       of possible viewing angles for the instrument.
!     1st July 2011, Caroline Poulsen: added in extra output files.
!    28th July 2011, Caroline Poulsen: added in scan line files files.
!     5th Sep 2011, Chris Arnold: added LUT/RTM interpolation switches.
!    25th Nov 2011, Caroline Poulsen add ChI channel indice variable.
!    2011/12/13, Matthias Jerg: added netcdf filenames to type FID_t
!    2012/05/23, C. Poulsen: removed threshold def
!    2012/06/18, C. Poulsen: added illum
!    2012/08/22, Matthias Jerg: adds Nyp
!    2012/10/12, C. Poulsen: added defaultctrl%sx category!
!    2014/01/15, Greg McGarragh: No need for Ctrl%DefaultSx any more.
!    2014/01/25, Greg McGarragh: Cleaned up the code.
!    2014/12/19, Adam Povey: Removing unneccessary fields.
!    2015/01/13, Adam Povey: Adding Ch_Is, YMixed. Removing First:Last indexes.
!    2015/01/30, Adam Povey: Remove Ws, Xstart, Ystart as depreciated.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module CTRL_def

   use ECP_constants

   implicit none

   ! Define a type to hold File names used by the ECP

   type FID_t
      character(FilenameLen) :: Config	           ! Configuration
      character(FilenameLen) :: MSI	           ! Multi-Spectral Image
      character(FilenameLen) :: LWRTM	           ! LW Rad Trans Model results
      character(FilenameLen) :: SWRTM	           ! SW Rad Trans Model results
      character(FilenameLen) :: PRTM               ! Profile RTM
      character(FilenameLen) :: LS	           ! Land/sea map
      character(FilenameLen) :: CF	           ! Cloud flag
      character(FilenameLen) :: Geo                ! Geometry (sun-satellite)
      character(FilenameLen) :: Loc                ! Location (latitudes/longs)
      character(FilenameLen) :: uv                 ! scan lines u/v
      character(FilenameLen) :: Alb	           ! Surface albedo, emissivity
      character(FilenameLen) :: Diag	           ! ECP o/p diagnostics
      character(FilenameLen) :: BkP	           ! ECP Break-Point file
      character(FilenameLen) :: Log	           ! ECP log file
      character(FilenameLen) :: L2_primary         ! Primary output file
      character(FilenameLen) :: L2_secondary       ! Secondary output file
   end type FID_t

   ! Define a type for instrument info
   type Inst_t
      integer                :: ID
      character(InstNameLen) :: Name
   end type Inst_t

   ! Resolution info
   type Resoln_t
      integer                :: Space              ! Deg. of spatial resolution
                                                   ! (pixels)
      integer                :: AMeth              ! Averaging method
      integer                :: SegSize            ! Image segment size (no. of
                                                   ! rows of super-pixels that
                                                   ! make up a segment).
      integer                :: Time               ! Temporal av'ging required
                                                   ! (slots)
   end type Resoln_t

   ! Indices: includes pixels to be used, channel and state variable indices and
   ! "warm-start" pixel indices.
   type Ind_t
      ! Channel indexing variables
      integer                :: Ny                 ! No. of instrument channels
                                                   ! actually used
      integer                :: NAvail             ! No. of instrument channels
                                                   ! available.
      integer, pointer       :: Y_Id(:)            ! Instrument IDs for used chs
      integer, pointer       :: ICh(:)             ! Array indices for used chs
      integer                :: NSolar             ! No. of chs with solar source
      integer                :: NThermal           ! No. of chs w/ thermal source
      integer                :: NMixed             ! Number of mixed solar/
                                                   ! thermal channels
      integer, pointer       :: YSolar(:)          ! Array indices for solar chs
                                                   ! i.e. indices in Y array, not
                                                   ! channel IDs.
      integer, pointer       :: YThermal(:)        ! Array indices for thermal ch
      integer, pointer       :: YMixed(:)          ! Array indices for mixed chs
      integer, pointer       :: Ch_Is(:)           ! A bit flag of ch properties
                                                   ! such as thermal, solar

      ! View indexing variables
      integer                :: NInstViews         ! No. of instrument views
                                                   ! available
      integer                :: NViews             ! Number of instrument views
                                                   ! (forward,
                                                   ! nadir etc) selected
      integer, pointer       :: ViewIdx(:)         ! Array of view values, 1 per
                                                   ! channel

      ! Spatial grid indexing variables
      integer                :: XMax               ! Max no. of pixels in x
                                                   ! direction
      integer                :: YMax               ! Max no. of pixels in y
                                                   ! direction
      integer                :: X0                 ! Lower left pixel x coord
      integer                :: Y0                 ! Lower left pixel y coord
      integer                :: X1                 ! Upper right pixel x coord
      integer                :: Y1                 ! Upper right pixel y coord

      ! State vector indexing variables
      integer                :: Nx_Dy              ! Number of active state
                                                   ! variables for daylight
      integer                :: Nx_Tw              ! Number of active state
                                                   ! variables for twilight
      integer                :: Nx_Ni              ! Number of active state
                                                   ! variables for night
      integer                :: NxI_Dy             ! Number of inactive state
                                                   ! for daylight conditions
      integer                :: NxI_Tw             ! Number of inactive state
                                                   ! variables for twilight
      integer                :: NxI_Ni             ! Number of inactive state
                                                   ! variables for night
      integer                :: X_Dy(MaxStateVar)  ! Active state variable
                                                   ! indices for daylight
      integer                :: X_Tw(MaxStateVar)  ! Active state variable
                                                   ! indices for twilight
      integer                :: X_Ni(MaxStateVar)  ! Active state variable
                                                   ! indices for night
      integer                :: XI_Dy(MaxStateVar) ! Inactive state variable
                                                   ! indices for daylight
      integer                :: XI_Tw(MaxStateVar) ! Inactive state variable
                                                   ! indices for twilight
      integer                :: XI_Ni(MaxStateVar) ! Inactive state variable
                                                   ! indices for night
      integer                :: MDAD_LW            ! Index of channel at (or
                                                   ! nearest to) 11 um used in
                                                   ! MDAD method for setting FG
                                                   ! (AP) cloud pressure and
                                                   ! phase
      integer                :: MDAD_SW            ! Index of channel at (or
                                                   ! nearest to) 0.67 used in
                                                   ! MDAD method for setting FG
                                                   ! (AP) cloud optical depth
   end type Ind_t

   ! Surface Reflectance parameters
   ! Arrays are set by Ctrl%ind%nsolar - presumably max possible is total no
   ! of channels since all could be solar? - Sb, Cb NOT ARRAYS?
   type SurfRef_t
      integer                :: Flag               ! Surface reflectance flag (1=Ctrl,2=SAD,...)
      logical                :: use_full_brdf
      real, pointer          :: B(:,:)             ! Model parameter surface reflectance values
                                                   ! (chans, sea/land)
      real                   :: Sb                 ! % error in B
      real                   :: Cb                 ! Correlation in Sb between channels
   end type SurfRef_t

   ! Equivalent model parameter noise flags
   type EqMPN_t
      integer                :: Rs                 ! Flag to use EqMPN from Rs errors
      integer                :: TH                 ! Flag to use Eqmpn from T/H(z) errors
      integer                :: Homog              ! Flag to use Eqmpn from homog errors
      integer                :: CoReg              ! Flag to use Eqmpn from coReg errors
   end type EqMPN_t

   ! Inversion parameters
   type Invpar_t
      real                   :: MqStart            ! Marquardt starting parameter
      real                   :: MqStep             ! Marquardt step parameter
      integer                :: MaxIter            ! Maximum number of iterations
      integer                :: MaxPhase           ! Maximum number oif phase changes
      real                   :: Ccj                ! Cost convergence criteria
      real                   :: XScale(MaxStateVar)! Scaling parameters (Tau,Re,Pc,F,Ts)
      real                   :: XLLim(MaxStateVar) ! Lower limit on state
      real                   :: XULim(MaxStateVar) ! Upper limit on state
   end type Invpar_t

   ! Quality control parameters (apply to inversion)
   type QC_t
      real                   :: MaxJ               ! Maximum acceptable value of cost function
      real                   :: MaxS(MaxStateVar)  ! Maximum acceptable error in state variable
				                   ! at solution.
   end type QC_t

   ! Main Ctrl structure.
   ! Note that the FG and AP arrays are 2-d, to allow separate options for
   ! day, twilight and night. A 2-d array is used (as opposed to the 3 separate
   ! 1-d arrays used in the case of X_Dy, X_Ni etc) to allow checking via a
   ! simple nested loop.
   type CTRL_t
      character(FilenameLen) :: version
      character(FilenameLen) :: Data_Dir
      character(FilenameLen) :: Out_Dir
      character(FilenameLen) :: SAD_Dir
      character(FilenameLen) :: Run_ID
      type (FID_t)           :: FID
      integer                :: Bkpl	           ! Break-point level
      integer                :: Diagl(MaxDiagFlags)
      integer		     :: LUTIntflag         ! LUT Interpolation flag
                                                   ! 0 = linear interp
                                                   ! 1 = bicubic
      integer		     :: RTMIntflag         ! RTM Interpolation flag
                                                   ! 0 = linear interp
                                                   ! 1 = bicubic
      character(16)          :: Date               ! Date of MSI data
      character(16)          :: Time               ! Time of MSI data (not used?)
      integer                :: DOY                ! Day number in year of MSI data.
      real                   :: MaxSatZen
      real                   :: MaxSolZen          ! Daynight/twilight boundary
      real                   :: Sunset             ! SolZen angle for sunset
      type (Inst_t)          :: Inst
      type (Resoln_t)        :: Resoln
      type (Ind_t)           :: Ind
      character(3)           :: CloudClass         ! Name of LUT to use
      integer                :: CloudType          ! Cloud type flag, distinct from
                                                   ! cloud class, used to define
                                                   ! the homog/coreg noise.
      integer                :: Ap(MaxStateVar,3)  ! A Priori options
      integer                :: Fg(MaxStateVar,3)  ! First-guess options for
                                                   ! state vector plus phase.
      real                   :: Xb(MaxStateVar)    ! A Priori values
      real                   :: X0(MaxStateVar)    ! First-guess values
      real                   :: Sx(MaxStateVar)    ! Error covariance of Xb
      integer                :: Max_SDAD           ! Max distance (no of pixels) that
                                                   ! last retrieved solution can
                                                   ! be used for FG/AP setting by SDAD method.
      real, pointer          :: Sy(:,:)            ! Measurement error covariance.
      type (SurfRef_t)       :: RS
      type (EqMPN_t)         :: EqMPN
      type (Invpar_t)        :: Invpar
      type (QC_t)            :: QC                 ! Quality control structure
   end type CTRL_t

contains

include 'DeallocCtrl.F90'

end module CTRL_def
