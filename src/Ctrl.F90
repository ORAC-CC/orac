!-------------------------------------------------------------------------------
! Name:
!   Ctrl
!
! Purpose:
!   Module defining control structure (Ctrl) for the ECP
!
! Description:
!   Defines the types and sub-types used in the Ctrl structure.
!
! Arguments:
!   Name Type In/Out/Both Description
!   N/A
!
! Algorithm:
!   N/A
!
! Local variables:
!    Name Type Description
!    N/A
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
!       Removing pointer arays to allow output of the whole struct in a single
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
!       mulitple views.
!    18th May 2011, Andy Smith:
!       Multiple views(2). Added new Ctrl value NInstViews, to store the number
!       of possible viewing angles for the instrument.
!     1st July 2011, Caroline Poulsen: added in extra output files.
!    28th July 2011, Caroline Poulsen: added in scan line files files.
!     5th Sep 2011, Chris Arnold: added LUT/RTM interpolation switches.
!    25th Nov 2011, Caroline Poulsen add ChI channel indice variable.
!    2011/12/13, Matthias Jerg: added netcdf filenames to type FID_t
!    2012/05/23, C. Poulsen: removed threhold def
!    2012/06/18, C. Poulsen: added illum
!    2012/08/22, Matthias Jerg: adds Nyp
!    2012/10/12, C. Poulsen: added defaultctrl%sx category!
!    2014/01/15, Greg McGarragh: No need for Ctrl%DefaultSx anymore.
!    2014/01/25, Greg McGarragh: Cleaned up code.
!
! Bugs:
!   None known
!
! $Id: Ctrl.f90 191 2011-10-05 10:53:20Z carnold $
!
!---------------------------------------------------------------------

module CTRL_def

   use ECP_constants

   implicit none

   ! Define a type to hold File names used by the ECP

   type FID_t
      character(FilenameLen) :: CONFIG	           ! Multi-Spectral Image
      character(FilenameLen) :: MSI	           ! Multi-Spectral Image
      character(FilenameLen) :: LWRTM	           ! LW Rad Trans Model results
      character(FilenameLen) :: SWRTM	           ! SW Rad Trans Model results
      character(FilenameLen) :: PRTM               ! Profile RTM
      character(FilenameLen) :: LS	           ! Land/sea map
      character(FilenameLen) :: CF	           ! Cloud flag
      character(FilenameLen) :: Geo                ! Geometry (sun-satellite)
      character(FilenameLen) :: illum              ! illumination
      character(FilenameLen) :: Loc                ! Location (latitudes/longs)
      character(FilenameLen) :: uv                 ! scan lines u/v
      character(FilenameLen) :: Aux	           ! ??? Look-Up Tables ???
      character(FilenameLen) :: Out	           ! ECP results output
      character(FilenameLen) :: Diag	           ! ECP o/p diagnostics
      character(FilenameLen) :: BkP	           ! ECP Break-Point file
      character(FilenameLen) :: Log	           ! ECP log file
      character(FilenameLen) :: res	           ! ECP log file
      character(FilenameLen) :: qc	           ! ECP log file
      character(FilenameLen) :: geoout	           ! ECP log file
      character(FilenameLen) :: runinfo	           ! ECP log file
      character(FilenameLen) :: input	           ! ECP log file
      character(FilenameLen) :: apfg	           ! ECP log file
      character(FilenameLen) :: scan	           ! ECP log file
      character(FilenameLen) :: input_filename     ! Multi-Spectral Image
      character(FilenameLen) :: L2_primary_outputpath_and_file
      character(FilenameLen) :: L2_secondary_outputpath_and_file
      character(FilenameLen) :: L1_input_outputpath_and_file
   end type FID_t

   ! Define a type for instrument info
   type Inst_t
      integer                :: ID
      character(InstNameLen) :: Name
   end type Inst_t

   ! Resolution info
   type Resoln_t
      integer                :: Space              ! Deg. of spatial resolution (pixels)
      integer                :: AMeth              ! Averaging method
      integer                :: SegSize            ! Image segment size (no. of
                                                   ! rows of super-pixels that
                                                   ! make up a segment).
      integer                :: Time               ! Teporal av'ging required (slots)
   end type Resoln_t

   ! Indices: includes pixels to be used, channel and state variable indices and
   ! "warm-start" pixel indices.
   !
   ! Notes on channel IDs and indices:
   !
   ! The Y array holds the channel identifier numbers (IDs) for the channels
   ! used. (These are FIXED for each instrument and allow the system to link
   ! to channel dependent SAD data sets).
   !
   ! YSolar and YThermal hold the indices of the Solar and Thermal channels in
   ! the Y array (so YSolar always runs from 1 to NSolar and YThermal runs from
   ! the first channel with a thermal component to the last element of Y).
   type Ind_t
      integer                :: XMax               ! Max no. of pixels in x direction
      integer                :: YMax               ! Max no. of pixels in y direction
      integer                :: NChans             ! No. of instrument channels available.
      integer                :: Navail             ! No. of instrument channels actually
                                                   ! used
      integer                :: NInstViews         ! No. of instrument views available
      integer                :: NViews             ! Number of instrument views (forward,
                                                   ! nadir etc) selected
      integer, pointer       :: ViewIdx(:)         ! Array of view values, 1 per channel
                                                   ! / Y_ID
      integer                :: X0                 ! Lower left pixel x co-ordinate
      integer                :: Y0                 ! Lower left pixel y co-ordinate
      integer                :: X1                 ! Upper right pixel x co-ordinate
      integer                :: Y1                 ! Upper right pixel y co-ordinate
      integer                :: Ws                 ! Warm start flag
      integer                :: Xstart             ! Warm start X
      integer                :: Ystart             ! Warm start Y
      integer                :: Ny                 ! Number of channels to use
      integer                :: Nyp                ! Number of channels in preprocessing
                                                   ! file
      integer, pointer       :: Y_Id(:)            ! Array of channel IDs
      integer, pointer       :: ChI(:)             ! Array of channel IDs
      integer                :: Nx_Dy              ! Number of active state variables
                                                   ! for daylight conditions
      integer                :: Nx_Tw              ! Number of active state variables
                                                   ! for twilight conditions
      integer                :: Nx_Ni              ! Number of active state variables
                                                   ! for night conditions
      integer                :: NxI_Dy             ! Number of inactive state variables
                                                   ! for daylight conditions
      integer                :: NxI_Tw             ! Number of inactive state variables
                                                   ! for twilight conditions
      integer                :: NxI_Ni             ! Number of inactive state variables
                                                   ! for night conditions
      integer                :: X_Dy(MaxStateVar)  ! Array of active state variable
                                                   ! indices for daylight conditions
      integer                :: X_Tw(MaxStateVar)  ! Array of active state variable
                                                   ! indices for twilight conditions
      integer                :: X_Ni(MaxStateVar)  ! Array of active state variable
                                                   ! indices for night conditions
      integer                :: XI_Dy(MaxStateVar) ! Array of inactive state variable
                                                   ! indices for daylight conditions
      integer                :: XI_Tw(MaxStateVar) ! Array of inactive state variable
                                                   ! indices for twilight conditions
      integer                :: XI_Ni(MaxStateVar) ! Array of inactive state variable
                                                   ! indices for night conditions
      integer                :: Nsolar             ! No. of channels with solar source
      integer                :: Nthermal           ! No. of chan with thermal source
      integer, pointer       :: Ysolar(:)          ! Array of solar channel indices
                                                   ! i.e. indices in Y array, not
                                                   ! channel IDs.
      integer, pointer       :: Ythermal(:)        ! Array of thermal channel indices
      integer, pointer       :: ysolar_msi(:)
      integer, pointer       :: ythermal_msi(:)
      integer                :: ThermalFirst       ! Index of first thermal ID
      integer                :: ThermalLast        ! Index of last thermal ID
      integer                :: SolarFirst         ! Index of first solar ID
      integer                :: SolarLast          ! Index of last thermal ID
      integer                :: NMixed             ! Number of mixed solar/thermal chans
      integer                :: MDAD_LW            ! Index of channel at (or nearest
                                                   ! to) 11 um used in MDAD method for
                                                   ! setting FG (AP)
                                                   ! cloud pressure and phase
      integer                :: MDAD_SW            ! Index of channel at (or nearest
                                                   ! to) 0.67 used in MDAD method for
                                                   ! setting FG (AP) cloud optical depth
   end type Ind_t

   ! Cloud class info
   type CloudClass_t
      integer                :: N                  ! No. of classes available
      integer                :: Id                 ! Identifier
      character(3)           :: Name               ! Class name

   end type CloudClass_t

   ! Phase (water/ice) info
   type Phaset_t
      real                   :: Ice                ! Temperature below which first guess
                                                   ! phase switches to ice
      real                   :: Water              ! Temp. above which first guess phase
                                                   ! switches to water
   end type Phaset_t

   ! Surface Reflectance parameters
   ! Arrays are set by ctrl%ind%nsolar - presumably max possible is total no
   ! of channels since all could be solar? - Sb, Cb NOT ARRAYS?
   type SurfRef_t
      integer                :: Flag               ! Surface reflectance flag (1=Ctrl,2=SAD,...)
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

   ! Noise
   type Noise_t
      real                   :: NEFR               ! Noise equivalent fractional reflection
      real                   :: NEBT               ! Noise equivalent brightness temperature
   end type Noise_t

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

   ! Super pixel parameters (hidden)
   type SPix_t
      integer                :: NPixel             ! Number of pixels in super pixel
      integer                :: Xc                 ! Relative x coordinate of centre pixel
      integer                :: Yc                 ! Relative y coordinate of centre pixel
   end type SPix_t

   ! Main Ctrl structure.
   ! Note that the FG and AP arrays are 2-d, to allow separate options for
   ! day, twilight and night. A 2-d array is used (as oppoesed to the 3 separate
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
      character(11)          :: Date               ! Date of MSI data
      character(12)          :: Time               ! Time of MSI data (not used?)
      integer                :: DOY                ! Day number in year of MSI data.
      real                   :: MaxSatZen
      real                   :: MaxSolZen          ! Daynight/twilight boundary
      real                   :: Sunset             ! SolZen angle for sunset
      type (Inst_t)          :: Inst
      type (Resoln_t)        :: Resoln
      type (Ind_t)           :: Ind
      type (CloudClass_t)    :: CloudClass
      integer                :: CloudType          ! Cloud type flag, distinct from
                                                   ! cloud class, used to define
                                                   ! the homog/coreg noise.
      type (Phaset_t)        :: Phaset
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
      type (Noise_t)         :: Noise
      type (Invpar_t)        :: Invpar
      type (QC_t)            :: QC                 ! Quality control structure
      type (SPix_t)          :: Spix
    end type CTRL_t

end module CTRL_def
