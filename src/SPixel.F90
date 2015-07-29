!-------------------------------------------------------------------------------
! Name: SPixel.F90
!
! Purpose:
! Defines the SPixel, or "super pixel" module. This structure holds
! geometry, Radiative Transfer Model and other information relating to the
! pixel (or "super pixel") being processed.
!
! Also encapsulates various useful routines and defines check_value.
!
! Note: Most allocatable SPixel arrays are allocated in the subroutine
! Alloc_SPixel. However, it is necessary to allocate the Ym array size in
! Get_Measurements (as the number of channels is dependent on the geometry of
! the super pixel).
!
! History:
! 2000/11/24, AS: Original version
! 2000/12/01, AS: Removed underscores from Geom_t variable names and changed
!    Sunzen to Solzen.
! 2000/12/20, AS: Changed to an allocatable structure
! 2001/01/29, KS: Added transmittance at surface (Tsf), air mas factors SEC_o
!    and SEC_v, and transmittance along a slant path Tsf_o and Tsf_v.
!    Added Xb, X0 and Xn state vectors.
! 2001/02/15, KS: Added Ind substructure (numbers and indicies of channels)
!    Note: These have the same definitions as the corresponding Ctrl variables,
!    however they have been modified to account for the geometry (i.e. day,
!    twilight, night). These get calculated in Get_Geometry.
! 2001/02/21, AS: Added Tbc to LW structure. Previously missing from model data.
!    Removed Tsf parameters from the LW structure.
! 2001/03/06, AS: Change in use of Tsf parameters. RTM%SW and LW now both
!    contain Tsf, for appropriate channels. RTM overall struct contains Tsf_o and
!    Tsf_v, for all solar channels (mixed channel values supplied from the LW RTM
!    data). New quantities Ref_Clear and dRef_Clear_dRS added to RTM struct.
! 2001/03/07, AS: New LW value dB_dTs.
! 2001/04/11, AS: Changes arising from development of Invert_Marquardt. Added
!    solar constant f0. Not strictly a SPixel value as it is the same for all
!    SPixels in the image, although there may be pixels for which it does not
!    apply (night, twilight). Also using named constant for arrays of size
!    MaxStateVar. Corrected Sx to 2-d, was previously 1-d. Removed SPixel%Xn
!    array (current state vector for inversion). Added index of cloud class for
!    first guess state vector: FGCloudClass, also First Guess phase FGPhase, and
!    current phase and cloud class  SPixel%Phase and SPixel%Class. New arrays
!    for current upper and lower limits on state variables, XLLim and XULim.
! 2001/05/17, AS: Added measurement error covariance Sy.
! 2001/06/05, AS: Added Xn and Sn to main SPixel structure. New arrays for FG and
!    AP selection options. New indices MDAD_LW/SW and illumination flag Illum.
! 2001/07/04, AS: Comments updated.  LastX0 and LastY0 added to Loc structure.
!    Used to record the location of the last successfully retrieved solution
!    SPixel%XnSav, so that validity of that solution at a given location can be
!    checked. XnSav and SnSav introduced. These hold the last "good" state
!    vector while Xn and Sn hold the most recent.
! 2001/08/03, AS: Updated Loc structure to include YSeg0, YSegC, YSegn, for
!    referencing pixels within a data segment as opposed to the whole image.
!    **************** ECV work starts here *************************************
! 2001/02/23, AS: Re-applying some changes from 2001/2.
! 2002/08/14, CP: Add in a new variable that tells you about the cloud
!    characteristics around the pixel of concern
! 2002/12/13, CP: Add in the geopotential height )
! 2011/02/23, AS: Cloud flags converted to real, rather than byte, to match
!    current ORAC data.
! 2011/03/23, AS: Removal of super-pixel averaging. No need to allocate mask and
!    cloud or surface flags to Ctrl%Resoln%Space. Assume 1 pixel processed at a
!    time so only 1 flag needed. Removed Fracnext (was used to set a priori /
!    first guess error on fraction). Removed redundant struct members YSegc,
!    YSegn. Re-size FG array, phase removed (follow-up from phase change removal)
! 2011/04/08, AS: Phase change removal. On checking code in GetX and
!    InvertMarquardt, SPixel%Phase, Class and FGPhase are no longer required.
! 2011/04/20, AS:  Extension to handle multiple instrument views. The viewing
!    geometry becomes a set of arrays, e.g. 1 value of sat. zen angle per view.
!    New to Ind struct: Nviews and View_Idx.
! 2012/10/07, CP: Added in CWP variable and CWP error
! 2012/12/08, MJ: Added data types and structures for netcdf output.
! 2012/01/08, CP: Added channel info  for netcdfoutput.
! 2012/01/15, CP: Changed netcdf definitions. Added albedo
! 2012/06/15, CP: Changed illum into an array of ny variables
! 2012/10/04, CP: Added in new variables
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt
!    and cth
! 2013/10/02, CP/GT: Added in variable to calculate degrees of freedom of signal
! 2014/01/16, GM: Added spixel_y_to_ctrl_y_index.
! 2014/05/27, GM: Removed unused structure members and code cleanup.
! 2014/08/01, GM: Added more SPixel to Ctrl map indexes.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2015/01/13, AP: Adding YThermal, YSolar. Removing First:Last indexes
! 2015/01/18, GM: Put all related Get*() subroutines and
!    check_value() subroutines into this module.
! 2015/01/19, GM: Put XAUX.F90, XMDAD.F90, and XSDAD.F90
!    into this module.
! 2015/01/30, AP: Remove redundant fields.
! 2015/02/04, GM: Changes related to the new missing channel, illumination, and
!    channel selection code.
! 2015/03/02, AP: Remove Ctrl argument from
!    check_value routines.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module SPixel_def

   use ECP_Constants

   implicit none

   private

   public :: RTM_SW_t, &
             RTM_LW_t, &
             SPixel_RTM_t, &
             SPixel_Geom_t, &
             Loc_t, &
             Surface_t, &
             SPixel_Ind_t, &
             SPixel_t, &
             Alloc_SPixel, &
             Dealloc_SPixel, &
             Get_SPixel

   ! Define a type for the Short Wave RTM parameters

   type RTM_SW_t
      integer             :: Np           ! Number of pressure levels
      real, pointer       :: P(:)         ! Array of pressure levels (1:SW%Np)
      real, pointer       :: Tbc(:,:)     ! Surface to P channel transmittances
      real, pointer       :: Tac(:,:)     ! P to TOA channel transmittances
      real, pointer       :: Tsf(:)       ! Transmittance at the surface to TOA
   end type RTM_SW_t


   ! Define a type for the Long Wave RTM parameters

   type RTM_LW_t
      integer             :: Np           ! Number of pressure levels
      real                :: Lat          ! Latitude
      real                :: Lon          ! Longitude
      real, pointer       :: P(:)         ! Array of pressure levels
      real, pointer       :: Ems(:)       ! Channel emissivities
      real, pointer       :: Tac(:,:)     ! P to space channel transmittances
      real, pointer       :: Tbc(:,:)     ! Surface to P channel transmittances
      real, pointer       :: Tsf(:)       ! Transmittance at the surface to TOA
      real, pointer       :: Rac_Up(:,:)  ! TOA radiances from atmosphere at P
      real, pointer       :: Rac_Dwn(:,:) ! Downwelling radiances from atmosphere at P
      real, pointer       :: Rbc_Up(:,:)  ! Upwelling radiances from atmosphere at P
      real, pointer       :: R_clear(:)   ! Clear upwelling TOA radiance
      real, pointer       :: T(:)         ! Temperature at P. (1:LW%Np)
      real, pointer       :: H(:)         ! geopotential height at P. (1:LW%Np)
      real, pointer       :: dB_dTs(:)    ! Gradient of Bs w.r.t surface temp.
   end type RTM_LW_t


   ! Define the overall RTM structure

   type SPixel_RTM_t
      type(RTM_SW_t)      :: SW           ! Short wave RTM parameters
      type(RTM_LW_t)      :: LW           ! Long wave RTM parameters
      real, pointer       :: Tsf_o(:)     ! Transmittance along solar slant path
      real, pointer       :: Tsf_v(:)     ! Transmittance along view slant path
      real, pointer       :: REF_clear(:) ! TOA reflectance for clear conditions
      real, pointer       :: dREF_clear_dRs(:)
                                          ! Gradient of clear ref. w.r.t.
                                          ! surface albedo
   end type SPixel_RTM_t


   ! Define a type for the geometrical parameters

   type SPixel_Geom_t
      real, pointer       :: SolZen(:)    ! Solar zenith angle
      real, pointer       :: SatZen(:)    ! Satellite zenith angle
      real, pointer       :: RelAzi(:)    ! Relative azimuth
      real, pointer       :: SEC_o(:)     ! Air mass factor at solar zenith angle
      real, pointer       :: SEC_v(:)     ! Air mass factor at viewing angle
   end type SPixel_Geom_t


   ! Define a type for the locational parameters

   type Loc_t
      integer             :: X0           ! Pixel x co-ordinate
      integer             :: Y0           ! Pixel y co-ordinate
      real                :: Lat          ! Super-pixel mean latitude
      real                :: Lon          ! Super-pixel mean longitude
      integer             :: LastX0       ! Value of X0 for last successful
                                          ! retrieval.
      integer             :: LastY0       ! Value of Y0 for last successful
                                          ! retrieval.
   end type Loc_t


   type Surface_t
      logical             :: Land         ! Flag pixels containing land surface
   end type Surface_t


   type SPixel_Ind_t                      ! Modified Ctrl variables, depending on geometry
      integer             :: Ny           ! Number of viable measurement channels
      integer             :: NSolar       ! Number of viable solar channels
      integer             :: NThermal     ! Number of viable thermal channels
      integer             :: NMixed       ! Number of mixed solar/thermal channels
      integer, pointer    :: YSolar(:)    ! Array indices wrt Ctrl%Ind%ICh for
                                          ! solar channels
      integer, pointer    :: YThermal(:)  ! Array indices for thermal channels
      integer, pointer    :: YMixed(:)    ! Array indices for mixed channels
      integer             :: MDAD_LW      ! Index of channel at (or nearest to)
                                          ! 11 um. Used in MDAD method for setting
                                          ! FG (AP) cloud pressure and phase
      integer             :: MDAD_SW      ! Index of channel at (or nearest to)
                                          ! 0.67. Used in MDAD method for setting
                                          ! FG (AP) cloud optical depth
      integer             :: NViews       ! Number of instrument views
   end type SPixel_Ind_t


   type SPixel_t
      type(SPixel_RTM_t)  :: RTM          ! Super-pixel radiative transfer model
                                          ! parameters
      type(SPixel_Geom_t) :: Geom         ! Super-pixel geometry values
      type(Loc_t)         :: Loc          ! Super-pixel location info
      integer(byte)       :: Type         ! Particle type in pixel
      type(Surface_t)     :: Surface      ! Super-pixel surface flag info
      type(SPixel_Ind_t)  :: Ind          ! Numbers and indices of channels
      integer, pointer    :: Illum(:)     ! Illumination flag (day,twi, night)
      integer             :: FG(MaxStateVar)
                                          ! Methods for setting first guess
                                          ! state vector, matched to
                                          ! SPixel conditions.
      integer             :: AP(MaxStateVar)
                                          ! Methods for setting a priori state
                                          ! vector, matched to SPixel conditions.
      real,    pointer    :: Ym(:)        ! Measurements (** Allocated in
                                          ! Get_Measurements **)
      real,    pointer    :: Sy(:,:)      ! Measurement error covariance
                                          ! (alloc in Get_Measurements).
      integer, pointer    :: ViewIdx(:)   ! View index for each active measurement
      integer             :: Nx           ! Number of active state variables
                                          ! for the super-pixel
      integer             :: NxI          ! Number of inactive state variables
                                          ! for the super-pixel
      integer, pointer    :: X(:)         ! Array of active state variable
                                          ! indices for the super-pixel
                                          ! Allocated to Nx for the SPixel
      integer, pointer    :: XI(:)        ! Array of inactive state variable
                                          ! indices for the super-pixel
                                          ! Allocated to NxI for the SPixel
      real                :: Xb(MaxStateVar)
                                          ! A priori state vector values
      real                :: Sx(MaxStateVar, MaxStateVar)
                                          ! A priori state vector errors
                                          ! Not allocatable to no. of active
                                          ! vars - inactive parts required for
                                          ! error analysis.
      real                :: X0(MaxStateVar)
                                          ! First guess state vector values.
      real                :: XLLim(MaxStateVar)
                                          ! Array of lower limits on state
                                          ! variables.
      real                :: XULim(MaxStateVar)
                                          ! Array of upper limits on state
                                          ! variables.
      real                :: Xn(MaxStateVar)
                                          ! State vector from last inversion.
      real                :: Sn(MaxStateVar, MaxStateVar)
                                          ! Error values from last inversion.
      real                :: XnSav(MaxStateVar)
                                          ! "Saved" state vector from last
                                          ! inversion that converged.
      real                :: SnSav(MaxStateVar, MaxStateVar)
                                          ! Error values for XnSav.
      integer             :: QC           ! Quality control indicator
      real                :: CWP          ! Cloud water path
      real                :: CWP_Error    ! Cloud water path error
      real, pointer       :: Rs(:)        ! Super pixel surface reflectance
      real, pointer       :: SRs(:,:)     ! Super pixel surface reflectance
                                          ! covariances
      real, pointer       :: Rs2(:,:)     ! Super pixel surface reflectance
      real, pointer       :: SRs2(:,:,:)  ! Super pixel surface reflectance
                                          ! covariances
      real,    pointer    :: f0(:)        ! Solar constant
      integer, pointer    :: spixel_y_to_ctrl_y_index(:)
                                          ! Map SPixel measurement index space to
                                          !     CRTL   measurement index space
      integer, pointer    :: spixel_y_solar_to_ctrl_y_index(:)
      integer, pointer    :: spixel_y_thermal_to_ctrl_y_index(:)
      integer, pointer    :: spixel_y_solar_to_ctrl_y_solar_index(:)
      integer, pointer    :: spixel_y_thermal_to_ctrl_y_thermal_index(:)
      integer, pointer    :: spixel_y_mixed_to_spixel_y_solar(:)
      integer, pointer    :: spixel_y_mixed_to_spixel_y_thermal(:)
   end type SPixel_t


   interface check_value
      module procedure check_value_float0, &
           check_value_float1, check_value_float1_l, &
           check_value_float2, check_value_float2_l, &
           check_value_byte0, &
           check_value_byte1, check_value_byte1_l, &
           check_value_byte2, check_value_byte2_l
   end interface check_value

contains

#include "AllocSPixel.F90"
#include "DeallocSPixel.F90"

#include "GetGeometry.F90"
#include "GetIllum.F90"
#include "GetIndexing.F90"
#include "GetLwSwRTM.F90"
#include "GetMeasurements.F90"
#include "GetRTM.F90"
#include "GetSurface.F90"
#include "XMDAD.F90"
#include "XSDAD.F90"
#include "GetX.F90"

#include "GetSPixel.F90"
#include "IntCTP.F90"


!-------------------------------------------------------------------------------
! Name: check_value
!
! Purpose:
! Flag values outside their prescribed range. Accepts scalars and arrays up to
! two dimensions of type float or byte (though there is no reason this range
! couldn't be extended).
!
! Description and Algorithm details:
! 1) If the value is outside its range, set the appropriate bit of the QC flag.
! 2) If in DEBUG mode, write a warning to stdout.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! val         real   In   Value(s) to be considered.
! max         real   In   Minimum acceptable value.
! min         real   In   Maximum acceptable value
! SPixel      struct Both Super-pixel structure containing QC flag.
! name        string In   Identifying string to print.
! flag_bit    int    In   Bit of QC to set in event of out-of-range value.
! limit       real   In   (Optional) Rather than require all elements of val
!                         fall within the min-max, require no more than LIMIT
!                         elements fall outside than range.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine check_value_float0(val, max, min, SPixel, name, flag_bit)
   implicit none

   real,           intent(in)    :: val
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit

   if (val > max .or. val < min) then
#include "check_value.inc"
   end if

end subroutine check_value_float0

subroutine check_value_float1(val, max, min, SPixel, name, flag_bit)
   implicit none

   real,           intent(in)    :: val(:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_float1

subroutine check_value_float1_l(val, max, min, SPixel, name, flag_bit, limit)
   implicit none

   real,           intent(in)    :: val(:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_float1_l

subroutine check_value_float2(val, max, min, SPixel, name, flag_bit)
   implicit none

   real,           intent(in)    :: val(:,:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_float2

subroutine check_value_float2_l(val, max, min, SPixel, name, flag_bit, limit)
   implicit none

   real,           intent(in)    :: val(:,:)
   real,           intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_float2_l

!-----------------------------------------------------------------------------

subroutine check_value_byte0(val, max, min, SPixel, name, flag_bit)
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit

   if (val > max .or. val < min) then
#include "check_value.inc"
   end if

end subroutine check_value_byte0

subroutine check_value_byte1(val, max, min, SPixel, name, flag_bit)
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_byte1

subroutine check_value_byte1_l(val, max, min, SPixel, name, flag_bit, limit)
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_byte1_l

subroutine check_value_byte2(val, max, min, SPixel, name, flag_bit)
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:,:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit

   if (any(val > max .or. val < min)) then
#include "check_value.inc"
   end if

end subroutine check_value_byte2

subroutine check_value_byte2_l(val, max, min, SPixel, name, flag_bit, limit)
   use ECP_constants, only: byte
   implicit none

   integer(byte),  intent(in)    :: val(:,:)
   integer(byte),  intent(in)    :: max, min
   type(SPixel_t), intent(inout) :: SPixel
   character(*),   intent(in)    :: name
   integer,        intent(in)    :: flag_bit
   integer,        intent(in)    :: limit

   if (count(val > max .or. val < min) > limit) then
#include "check_value.inc"
   end if

end subroutine check_value_byte2_l

end module SPixel_def
