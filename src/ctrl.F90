!-------------------------------------------------------------------------------
! Name: ctrl.F90
!
! Purpose:
! Module defining control structure (Ctrl) for ORAC
!
! History:
! 2000/08/17, AS: Original version
! 2000/11/22, AS: Added Out_Dir, Ysolar and Ythermal. Added Run_ID - type may be
!    wrong (we had to guess). Added maxsatzen and maxsunzen.
!    Added LimitsFlag to InvPar
! 2000/12/15, KS: Added Inst%Date and Inst%Time.
!    Added ThermalFirst, ThermalLast, SolarFirst, SolarLast
! 2001/01/10, AS: Added comments to clarify meaning of (Ctrl%Ind%) Y, YSolar and
!    YThermal. Ctrl%Ind%Y renamed Y_Id for the same reason.
!    Added Threshold to SPixel
! 2001/02/05, KS: Added NMixed, number of mixed channels.
! 2001/02/12, KS: Added MDAD_LW and MDAD_SW, indices for channels to determine
!    FG values of Pc, phase and Tau using the MDAD method.
! 2001/04/24, AS: New variable for max number of phase changes in inversion
!    (Ctrl%Invpar%MaxPhase).
!    New arrays for specifying active state variables in day/twilight/night
!    conditions (1 array of X and 1 array of Nx for each). Replaces the old X
!    and Nx arrays, which were for all conditions.
! 2001/05/17, AS: Added CloudType parameter and measurement covariance matrix Sy.
! 2001/06/06, AS: FG and AP options arrays extended to two dimensions. 2nd dim
!    allows different choices depending on the conditions (day/twilight/night).
! 2001/06/25, AS: New variable Sunset for solar zenith angle that denotes
!    sunset. (Was previously hard-coded).
! 2001/07/02, AS: Diagnostic level parameter Diagl changed to an array of flags.
!    New parameters MaxJ and MaxS in new sub-structure QC (quality control)
!    Also Max_SDAD, for limit on distance (in pixels) at which the last
!    retrieved solution is valid for FG/AP setting.
! 2001/07/06, AS: Removing pointer arrays to allow output of the whole struct in
!    a single statement. Arrays affected: Ctrl%Ind%X_Dy/Tw/Ni, Ctrl%Sy.
! 2001/07/11, AS: Fixes in CloudClass and Phaset structs. CloudClass arrays were
!    fixed at size 2: replaced by MaxCloudClass constant. CloudClass data
!    removed from Phaset struct: couldn't see any reason why it was there, must
!    have been a typo. Moved Date and Time from the Inst struct to the main
!    struct. Added Day of Year (DOY) required for setting solar constants
!    in ReadChan.
! 2001/08/03, AS: Added Ctrl%Resoln%SegSize. Size of image segment.
!    Updated Date from character length 8 to 11 (format dd-mmm-yyyy rather
!    than yyyymmdd) and converted time from real to character length 12
!    (hh:mm:ss.sss).
! 2001/08/23, AS: New parameter in Ind for number of instrument channels
!    available (as opposed to selected by the user). Set by Read_Inst_Config.
!    **************** ECV work starts here *************************************
! 2011/03/21, AS: Removal of phase change functionality. Only 1 cloud class is
!    required now. Cloud class on phase change becomes redundant.
!    State variables for first guess and a priori for ice and water phase
!    become single state vars, limits for ice and water phase replaced by
!    single set of limits. Cloud class selection method now redundant.
! 2011/03/31, AS: Removal of phase change: re-size X0 and FG flags arrays since
!    phase no longer needed.
! 2011/04/06, AS: Removed selection methods SAD and SDAD.
!    Limits flag removed from Ctrl struct as only 1 method remains supported.
! 2011/04/14, AS: Extension to handle multiple views. Number of channels/
!    measurements become allocatable since a specific channel ID can be used in
!    several views. New index for nviews and set of view indices.
! 2011/05/12, AS: Extension to handle multiple views. Number of surface
!    reflectance values becomes allocatable: number of solar channels can be
!    increased my having multiple views.
! 2011/05/18, AS: Multiple views(2). Added new Ctrl value NInstViews, to store
!    the number of possible viewing angles for the instrument.
! 2011/07/01, CP: added in extra output files.
! 2011/07/28, CP: added in scan line files files.
! 2011/09/05, CA: added LUT/RTM interpolation switches.
! 2011/11/25, CP: add ChI channel indice variable.
! 2011/12/13, MJ: added netcdf filenames to type FID_t
! 2012/05/23, CP: removed threshold def
! 2012/06/18, CP: added illum
! 2012/08/22, MJ: adds Nyp
! 2012/10/12, CP: added defaultctrl%sx category!
! 2014/01/15, GM: No need for Ctrl%DefaultSx any more.
! 2014/01/25, GM: Cleaned up the code.
! 2014/12/19, AP: Removing unneccessary fields.
! 2015/01/13, AP: Adding Ch_Is, YMixed. Removing First:Last indexes.
! 2015/01/30, AP: Remove Ws, Xstart, Ystart as depreciated.
!    Remove Resoln structure as superpixeling only in preprocessing.
! 2015/02/04, GM: Add sabotage_inputs flag and retrieval channel requirements
!    arrays.
! 2015/02/04, GM: Add ReChans array.
! 2015/03/02, AP: Adding terms for aerosol retrieval.
! 2015/05/25, GM: Get rid of filename Diag and flags Diagl.  Neither was being
!    used and have been rotting.
! 2015/07/27, AP: Convert Homog/Coreg into logicals. Remove Ind%Log and
!    NInstViews. Replace process_one_phase_only with Types_to_process.
! 2015/08/21, AP: Tidying comments; Ordering variables logically; Adding WvlIdx;
!    Renaming Flags as Selm; Introducing XJ;
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2015/11/18, GM: Add Y_Id_legacy(6).
! 2016/02/02, GM: Add allow_a_default_surface.
! 2016/05/31, GT: Added Ctrl%process_aerosol_only flag.
! 2016/06/06, GM: Add get_T_dv_from_T_0d.
! 2016/07/11, GM: Add Nullify_Ctrl().
! 2016/07/27, GM: Add variables Class, Class2, LUTClass2, FID%SAD_Dir2 to
!    support the multilayer retrieval.
! 2016/08/11, SP: Add logical flag for processing when using only 1 view from a
!    multiangular sensor. Prevents post-processor problems.
! 2017/01/09, CP: Clarified definitions of ML layer definition.
! 2017/07/05, AP: Add NAll to track the total number of channels. New QC.
! 2017/10/04, GM: Add use_ann_phase.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module Ctrl_m

   use ORAC_Constants_m
   use orac_indexing_m, only: common_indices_t

   implicit none

   ! Define a type to hold File names
   type FID_t
      character(FilenameLen) :: Data_Dir           ! Directory of input files
      character(FilenameLen) :: Out_Dir            ! Directory for output files
      character(FilenameLen) :: SAD_Dir            ! Directory of SAD files
      character(FilenameLen) :: SAD_Dir2           ! Directory of SAD files
      character(FilenameLen) :: Filename           ! Basename of input files
      character(FilenameLen) :: Config             ! Configuration
      character(FilenameLen) :: MSI                ! Multi-Spectral Image
      character(FilenameLen) :: LWRTM              ! LW Rad Trans Model results
      character(FilenameLen) :: SWRTM              ! SW Rad Trans Model results
      character(FilenameLen) :: PRTM               ! Profile RTM
      character(FilenameLen) :: LS                 ! Land/sea map
      character(FilenameLen) :: CF                 ! Cloud flag
      character(FilenameLen) :: Geo                ! Geometry (sun-satellite)
      character(FilenameLen) :: Loc                ! Location (latitudes/longs)
      character(FilenameLen) :: Alb                ! Surface albedo, emissivity
      character(FilenameLen) :: L2_primary         ! Primary output file
      character(FilenameLen) :: L2_secondary       ! Secondary output file
   end type FID_t

   ! Indices: channel indexing and spatial grid definitions
   type, extends(common_indices_t) :: Ind_t
      ! Channel indexing variables
      integer                :: NAll               ! No. of channels on inst
      integer                :: NAvail             ! No. of instrument channels
                                                   ! available in file
      integer, pointer       :: ICh(:)             ! Index used chs out of those
                                                   ! available in file
      integer                :: Y_Id_legacy(6)     ! The closest available legacy
                                                   ! channels or 0 otherwise.
      integer                :: NMixed             ! Number of mixed solar/
                                                   ! thermal channels
      integer, pointer       :: YMixed(:)          ! Array indices for mixed chs

      ! Wavelength indexing variables (used to identify different views of the
      ! same filter; numbered from 1 in order found in file)
      integer                :: NWvl               ! Number of wavelenths avail
      integer, pointer       :: WvlIdx(:)          ! Array of wavelength values

      ! Spatial grid indexing variables
      integer                :: XMax               ! Max no. of pixels in x
                                                   ! direction
      integer                :: YMax               ! Max no. of pixels in y
                                                   ! direction

      integer, pointer       :: channel_proc_flag(:)
   end type Ind_t

   ! Surface Reflectance parameters
   type SurfRef_t
      integer                :: RsSelm             ! Surface reflectance
                                                   ! selection method
      integer                :: SRsSelm            ! Surface reflectance
                                                   ! uncertainty selection method
      logical                :: use_full_brdf
      logical                :: read_full_brdf
      logical                :: allow_a_default_surface
      real, pointer          :: B(:,:)             ! Prescribed surface reflect
                                                   ! (chs, sea/land)
      real, pointer          :: Sb(:,:)            ! % error in B
      real                   :: Cb                 ! Correlation in error between
                                                   ! chs: constant for all chs
      logical                :: add_fractional     ! When using SRsSelm /= Meas,
                                                   ! add Sb*Rs to the uncertainty
      logical                :: diagonal_SRs       ! If true SRs must be diagonal
      logical                :: solar_factor       ! In GetSurface divide surface
                                                   ! reflectances by SEC_o
      real                   :: SS_Xb(MaxStateVar) ! A priori used by Swansea
                                                   ! mode to guess S from meas
      real                   :: SS_Sx(MaxStateVar) ! Uncertainy used by Swansea
                                                   ! mode to guess S from meas
   end type SurfRef_t

   ! Equivalent model parameter noise flags
   type EqMPN_t
      integer                :: SySelm             ! Measurement covariance
                                                   ! selection method
      logical                :: Homog              ! Use homogogeneity errors
      logical                :: CoReg              ! Use coregistration errors
   end type EqMPN_t

   ! Inversion parameters
   type Invpar_t
      logical                :: ConvTest           ! Apply false convergence test
      real                   :: MqStart            ! Marquardt starting parameter
      real                   :: MqStep             ! Marquardt step parameter
      integer                :: MaxIter            ! Maximum number of iterations
      real                   :: Ccj                ! Cost convergence criteria
      real                   :: XScale(MaxStateVar)! Scaling parameters
      real                   :: XLLim(MaxStateVar) ! Lower limit on state
      real                   :: XULim(MaxStateVar) ! Upper limit on state
      real                   :: Pc_dmz             ! The closest the upper and
                                                   ! lower layer clouds are
                                                   ! allowed to get to each other
      logical                :: always_take_GN     ! When using ConvTest, always
                                                   ! return the Gauss-Newton
                                                   ! result, even if worse.
      logical                :: dont_iter_convtest ! When using ConvTest, don't
                                                   ! count tests towards iter
      logical                :: disable_Ss         ! Don't include inactive state
                                                   ! elements in uncertainty calc
   end type Invpar_t

   ! Quality control parameters (used to set SPixel%QC flag)
   type QC_t
      real                   :: MaxJ               ! Maximum acceptable value of
                                                   ! cost function
      real                   :: MaxDoFN            ! Maximum acceptable degrees
                                                   ! of freedom for noise
      real                   :: MaxElevation       ! Elevation above which
                                                   ! retrievals are suspect
   end type QC_t

   ! Main Ctrl structure.
   type Ctrl_t
      ! Terms drawn from mandatory part of driver file and config file
      type(FID_t)            :: FID
      character(InstNameLen) :: InstName           ! Instrument name
      integer                :: Class              ! Controls details related to
                                                   ! class (liquid water, ice,
                                                   ! ash, aerosol, etc.)
      integer                :: Class2             ! Class for layer 2 (lower)
      character(3)           :: LUTClass           ! Name of LUT to use
      character(3)           :: LUTClass2          ! LUTClass for layer 2 (lower)
      character(7)           :: LUTClassLayers     ! LUTClasses combined with '_'
      integer                :: Approach           ! Controls manner of retrieval
                                                   ! performed. See ORACConstants.

      ! Terms that aren't controlled by the driver file
      type(Ind_t)            :: Ind
      integer                :: DOY                ! Day number in year of data.

      ! Remaining terms are set in Read_Driver
      character(FilenameLen) :: Run_ID
      type(SurfRef_t)        :: RS
      type(EqMPN_t)          :: EqMPN
      type(Invpar_t)         :: Invpar
      type(QC_t)             :: QC

      ! Boundaries of illumination conditions
      real                   :: MaxSolZen          ! Daynight/twilight boundary
      real                   :: MaxSatZen
      real                   :: MinRelAzi          ! Sun glint boundary
      real                   :: Sunset             ! SolZen angle for sunset

      ! Switches and flags controlling optional behaviours
      integer                :: i_equation_form    ! Selects equation used in
                                                   ! FMSolar if Surface%
                                                   ! use_full_brdf = .true.
      logical                :: get_T_dv_from_T_0d ! See detailed description in
                                                   ! ReadDriver.F90
      integer                :: LUTIntSelm         ! LUT Interpolation flag,
                                                   ! See LUTIntMeth variables.
      integer                :: RTMIntSelm         ! RTM Interpolation flag,
                                                   ! See RTMIntMeth variables.
      integer                :: CloudType          ! Defines the homog/coreg
                                                   ! noise estimates used
      real                   :: Max_SDAD           ! Max # of pixels since
                                                   ! sucessful retrieval
                                                   ! acceptable for using SDAD.
      logical                :: sabotage_inputs    ! See sabotage_input_data.F90
      logical                :: process_cloudy_only
      logical                :: process_aerosol_only
      logical                :: all_channels_same_view
      logical                :: use_ann_phase
      integer                :: NTypes_to_process  ! # of valid values in above
      integer(byte)          :: Types_to_process(MaxTypes) ! Pavolonis (or other)
                                                   ! type codes for pixels to
                                                   ! run the retrieval
      integer                :: Surfaces_to_skip   ! Setting to ILand or ISea
                                                   ! skips all respective pixels
      integer                :: second_aot_ch(1)   ! Output second AOT value at
                                                   ! wavelength of this channel
      logical                :: verbose            ! Controls output to terminal

      ! Variables used by cloud_indexing_logic (first three ignore driver)
      integer, pointer       :: tau_chans(:)       ! Channels that meet the
                                                   ! Tau retrieval requirement
      integer, pointer       :: r_e_chans(:)       ! Same thing but for Re
      integer, pointer       :: ir_chans(:)        ! Same thing but for Pc, Fr,
                                                   ! and Ts
      integer, pointer       :: ReChans(:)         ! A list of effective radius
                                                   ! sensitive channels to use
                                                   ! in an order of decreasing
                                                   ! priority.
      logical                :: do_new_night_retrieval
      logical                :: do_CTX_correction
      real                   :: CTP_correction_limit

      ! State vector selection methods (see Selm variables in ORAC_Constants_m)
      integer                :: Ap(MaxStateVar,MaxIllum) ! A Priori options
      integer                :: Fg(MaxStateVar,MaxIllum) ! First-guess options

      ! Prescribed retrieval vectors
      real                   :: Xb(MaxStateVar)    ! A Priori values
      real                   :: X0(MaxStateVar)    ! First-guess values
      real                   :: Sx(MaxStateVar)    ! Error covariance of Xb
      real, pointer          :: Sy(:,:)            ! Measurement error covariance

      ! State vector indexing variables (defined for each illumination condition)
      integer                :: Nx(MaxIllum)       ! # of active state variables
      integer                :: NxJ(MaxIllum)      ! # of Jacobian parameters
      integer                :: X(MaxStateVar,MaxIllum)  ! State vector elements
                                                         !   to be retrieved
      integer                :: XJ(MaxStateVar,MaxIllum) ! " included in Jacobian
   end type Ctrl_t

contains

subroutine Nullify_Ctrl(Ctrl)

   use orac_indexing_m, only: nullify_common_indices

   implicit none

   ! Declare arguments

   type(Ctrl_t), intent(inout) :: Ctrl

   call nullify_common_indices(Ctrl%Ind%common_indices_t)

   nullify(Ctrl%Ind%WvlIdx)
   nullify(Ctrl%Ind%ICh)
   nullify(Ctrl%Ind%YMixed)
   nullify(Ctrl%Ind%channel_proc_flag)

   nullify(Ctrl%RS%B)
   nullify(Ctrl%RS%Sb)

   nullify(Ctrl%Sy)

   nullify(Ctrl%tau_chans)
   nullify(Ctrl%r_e_chans)
   nullify(Ctrl%ir_chans)

   nullify(Ctrl%ReChans)

end subroutine Nullify_Ctrl


#include "dealloc_ctrl.F90"

end module Ctrl_m
