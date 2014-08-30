!-------------------------------------------------------------------------------
! Name:
!    SPixel
!
! Purpose:
!    Defines the SPixel, or "super pixel" structure. This structure holds
!    geometry, Radiative Transfer Model and other information relating to the
!    pixel (or "super pixel") being processed.
!
!    Note: Most allocatable SPixel arrays are allocated in the subroutine
!    Alloc_SPixel. However, it is necessary to allocate the Ym array size in
!    Get_Measurements (as the number of channels is dependent on the geometry of
!    the super pixel).
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
!    24th Nov 2000, Andy Smith: Original version
!     1st Dec 2000, Andy Smith:
!       Removed underscores from Geom_t variable names and changed Sunzen to
!       Solzen.
!    20th Dec 2000, Kevin Smith:
!       Changed to an allocatable structure
!    29th Jan 2001, Kevin Smith :
!       Added transmittance at surface (Tsf), air mas factors SEC_o and SEC_v,
!       and transmittance along a slant path Tsf_o and Tsf_v.
!       Added Xb, X0 and Xn state vectors.
!    15th Feb 2001, Kevin Smith:
!       Added Ind substructure (numbers and indicies of channels)
!       Note: These have the same definitions as the corresponding Ctrl
!       variables, however they have been modified to account for the geometry
!       (i.e. day, twilight, night). These get calculated in Get_Geometry.
!    21st Feb 2001, Andy Smith:
!       Added Tbc to LW structure. Previously missing from model data.
!       Removed Tsf parameters from the LW structure.
!     6th Mar 2001, Andy Smith:
!       Change in use of Tsf parameters.
!       RTM%SW and LW now both contain Tsf, for appropriate channels.
!       RTM overall struct contains Tsf_o and Tsf_v, for all solar channels
!       (mixed channel values supplied from the LW RTM data).
!       New quantities Ref_Clear and dRef_Clear_dRS added to RTM struct.
!     7th Mar 2001, Andy Smith:
!       New LW value dB_dTs.
!    11th Apr 2001, Andy Smith:
!       Changes arising from development of Invert_Marquardt.
!       Added solar constant f0. Not strictly a SPixel value as it is the same
!       for all SPixels in the image, although there may be pixels for which it
!       does not apply (night, twilight).
!       Also using named constant for arrays of size MaxStateVar.
!       Corrected Sx to 2-d, was previously 1-d.
!       Removed SPixel%Xn array (current state vector for inversion).
!       Added index of cloud class for first guess state vector: FGCloudClass,
!       also First Guess phase FGPhase, and current phase and cloud class
!       SPixel%Phase and SPixel%Class.
!       New arrays for current upper and lower limits on state variables,
!       XLLim and XULim.
!    17th May 2001, Andy Smith:
!       Added measurement error covariance Sy.
!     5th Jun 2001, Andy Smith:
!       Added Xn and Sn to main SPixel structure.
!       New arrays for FG and AP selection options. New indices MDAD_LW/SW and
!       illumination flag Illum.
!     4th July 2001, Andy Smith:
!       Comments updated.
!       LastX0 and LastY0 added to Loc structure. Used to record the location of
!       the last successfully retrieved solution SPixel%XnSav, so that validity
!       of that solution at a given location can be checked.
!       XnSav and SnSav introduced. These hold the last "good" state vector
!       while Xn and Sn hold the most recent.
!     3rd Aug 2001, Andy Smith:
!       Updated Loc structure to include YSeg0, YSegC, YSegn, for referencing
!       pixels within a data segment as opposed to the whole image.
!    **************** ECV work starts here *************************************
!    23rd Feb 2011, Andy Smith:
!       Re-applying some changes from 2001/2.
!    14th Aug 2002, Caroline Poulsen:
!       Add in a new variable that tells you about the cloud characteristics
!       around the pixel of concern
!    13th Dec 2002, Caroline Poulsen:
!       Add in the geopotential height )
!    23rd Feb 2011, Andy Smith:
!       Cloud flags converted to real, rather than byte, to match current ORAC
!       data.
!    23rd Mar 2011, Andy Smith:
!       Removal of super-pixel averaging. No need to allocate mask and cloud or
!       surface flags to Ctrl%Resoln%Space. Assume 1 pixel processed at a time
!       so only 1 flag needed.
!       Removed Fracnext (was used to set a priori / first guess error on
!       fraction).
!       Removed redundant struct members YSegc, YSegn.
!       Re-size FG array, phase removed (follow-up from phase change removal).
!     8th Apr 2011, Andy Smith:
!       Phase change removal. On checking code in GetX and InvertMarquardt,
!       SPixel%Phase, Class and FGPhase are no longer required.
!    20th Apr 2011, Andy Smith:
!       Extension to handle multiple instrument views. The viewing geometry
!       becomes a set of arrays, e.g. 1 value of sat. zen angle per view.
!       New to Ind struct: Nviews and View_Idx.
!     7th Oct 2012, Caroline Poulsen: Added in CWP variable and CWP error
!     8th Dec 2012, Matthias Jerg: Added data types and structures for netcdf
!       output.
!     8th Jan 2012, Caroline Poulsen: Added channel info  for netcdfoutput.
!    15th Jan 2012, Caroline Poulsen: Changed netcdf definitions
!    15th Jan 2012, Caroline Poulsen: Added albedo
!    15th Jun 2012, Caroline Poulsen: Changed illum into an array of ny
!       variables
!     4th Oct 2012, Caroline Poulsen: Added in new variables
!    2013/01/17, Matthias Jerg: Adds code to accommodate uncertainties of ctt
!       and cth
!    2013/10/02, CP/GT: Added in variable to calculate degrees of freedom of
!       signal
!    2014/01/16, Greg McGarragh: Added spixel_y_to_ctrl_y_index.
!    2014/05/27, Greg McGarragh: Removed unused structure members and code
!       cleanup.
!    2014/08/01, Greg McGarragh: Added more SPixel to Ctrl map indexes.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module SPixel_def

   use ECP_Constants

   implicit none

   ! Define a type for the Short Wave RTM parameters

   type RTM_SW_t
      integer             :: Np           ! Number of pressure levels
      real                :: Lat          ! Latitude
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
      real                :: skint        ! skin temperature
      real                :: sp           ! Surface pressure
      real, pointer       :: P(:)         ! Array of pressure levels
      real, pointer       :: Ems(:)       ! Channel emissivities
      real, pointer       :: Tac(:,:)     ! P to space channel transmittances
      real, pointer       :: Tbc(:,:)     ! Surface to P channel transmittances
      real, pointer       :: Tsf(:)       ! Transmittance at the surface to TOA
      real, pointer       :: Rac_Up(:,:)  ! TOA radiances from atmosphere at P
      real, pointer       :: Rac_Dwn(:,:) ! Downwelling radiances from atmosphere at P
      real, pointer       :: Rbc_Up(:,:)  ! Upwelling radiances from atmosphere at P
      real, pointer       :: R_clear(:)   ! Clear upwelling TOA radiance
      real, pointer       :: Bs(:)        ! Planck radiances at surface.
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
      ! X0, Xn, Xc, Y0, Yn, Yc are co-ordinates within the whole image
      ! AS, Apr 2011: Xc, Yc, Xn, Yn can probably be removed now that
      ! super-pixel averaging has gone.
      integer             :: X0           ! Super-pixel 'left' x co-ordinate
      integer             :: Xn           ! Super-pixel 'right' x co-ordinate
      integer             :: Xc           ! Super-pixel central x co-ordinate
      integer             :: Y0           ! Super-pixel 'bottom' y co-ordinate
      integer             :: Yn           ! Super-pixel 'top' y co-ordinate
      integer             :: Yc           ! Super-pixel central y co-ordinate
      integer             :: YSeg0        ! Super-pixel 'bottom' y co-ordinate
                                          ! within an image segment
      real                :: Lat          ! Super-pixel mean latitude
      real                :: Lon          ! Super-pixel mean longitude
      integer             :: LastX0       ! Value of X0 for last successful
                                          ! retrieval.
      integer             :: LastY0       ! Value of Y0 for last successful
                                          ! retrieval.
   end type Loc_t


   type Cloud_t
!     integer             :: NCloudy      ! Number of cloudy pixels in super pixel
      real                :: Fraction     ! Cloud fraction for super pixel
      real                :: Flags        ! Cloud flags
   end type Cloud_t


   type Surface_t
      integer             :: NLand        ! Number of land surface pixels in super pixel
      integer             :: NSea         ! Number of sea surface pixels in super pixel
      integer(kind=byte)  :: Land         ! Super-pixel land surface flag
      integer(kind=byte)  :: Sea          ! Super-pixel sea surface flag
      integer(kind=byte)  :: Flags        ! Surface flags
   end type Surface_t


   type SPixel_Ind_t                      ! Modified Ctrl variables, depending on geometry
      integer             :: Ny           ! Number of viable measurement channels
      integer             :: NSolar       ! Number of viable solar channels
      integer             :: NThermal     ! Number of viable thermal channels
      integer             :: ThermalFirst ! Index of first channel with a thermal component
      integer             :: ThermalLast  ! Index of last channel with a thermal component
      integer             :: SolarFirst   ! Index of first channel with a solar component
      integer             :: SolarLast    ! Index of last channel with a solar component
      integer             :: NMixed       ! Number of mixed solar/thermal channels
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
      type(Cloud_t)       :: Cloud        ! Super-pixel cloud flag info
      type(Surface_t)     :: Surface      ! Super-pixel surface flag info
      type(SPixel_Ind_t)  :: Ind          ! Numbers and indices of channels
      integer ,pointer    :: Illum(:)     ! Illumination flag (day,twi, night)
      integer             :: FG(MaxStateVar)
                                          ! Methods for setting first guess
                                          ! state vector, matched to
                                          ! SPixel conditions.
      integer             :: AP(MaxStateVar)
                                          ! Methods for setting a priori state
                                          ! vector, matched to SPixel conditions.
      real, pointer       :: Ym(:)        ! Measurements (** Allocated in
                                          ! Get_Measurements **)
      real, pointer       :: Sy(:,:)      ! Measurement error covariance
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
      real                :: CWP_ERROR    ! Cloud water path error
      integer             :: Mask         ! Averaging mask
      integer             :: NMask        ! Number of 'good' pixels in mask
      integer             :: NAverage     ! Number of pixels used to calculate
                                          ! super-pixel averages
      real, pointer       :: Rs(:)        ! Super pixel surface reflectance
      real, pointer       :: SRs(:,:)     ! Super pixel surface reflectance
                                          ! covariances
      real, pointer       :: f0(:)        ! Solar constant
      integer, pointer    :: spixel_y_to_ctrl_y_index(:)
                                          ! Map SPixel measurement index space to
                                          !     CRTL   measurement index space
      integer, pointer    :: spixel_y_solar_to_ctrl_y_index(:)
      integer, pointer    :: spixel_y_thermal_to_ctrl_y_index(:)
      integer, pointer    :: spixel_y_solar_to_ctrl_y_solar_index(:)
      integer, pointer    :: spixel_y_thermal_to_ctrl_y_thermal_index(:)
   end type SPixel_t

contains

include 'AllocSPixel.F90'
include 'DeallocSPixel.F90'

end module SPixel_def
