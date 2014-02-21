! Name:
!    SPixel
!
! Purpose:
!    Defines the SPixel, or "super pixel" structure. This structure holds
!    geometry, Radiative Transfer Model and other information relating to
!    the pixel (or "super pixel") being processed.
!    Note: Most allocatable SPixel arrays are allocated in the subroutine
!          Alloc_SPixel. However, it is necessary to allocate the Ym array
!          size in Get_Measurements (as the number of channels is dependent
!          on the geometry of the super pixel).
!
! Local variables:
!    Name       Type    Description
!    See structure definitions.
!
! History:
!    24th Nov 2000, Andy Smith : original version
!     1st Dec 2000, Andy Smith :
!       Removed underscores from Geom_t variable names and changed Sunzen 
!       to Solzen.
!    20th Dec 2000, Kevin Smith : Changed to an allocatable structure 
!    29th Jan 2001, KMS : Added transmittance at surface (Tsf), air mas factors
!                         SEC_o and SEC_v, and transmittance along a slant path
!                         Tsf_o and Tsf_v.
!                         Added Xb, X0 and Xn state vectors.
!    15th Feb 2001, KMS : Added Ind substructure (numbers and indicies of channels)
!                         Note: These have the same definitions as the corresponding
!                         Ctrl variables, however they have been modified to
!                         account for the geometry (i.e. day, twilight, night).
!                         These get calculated in Get_Geometry.
!    21st Feb 2001, andy Smith:
!       Added Tbc to LW structure. Previously missing from model data.
!       Removed Tsf parameters from the LW structure.
!     6th Mar 2001, Andy Smith:
!       Change in use of Tsf parameters.
!       RTM%SW and LW now both contain Tsf, for appropriate channels.
!       RTM overall struct contains Tsf_o and Tsf_v, for all solar
!       channels (mixed channel values supplied from the LW RTM data).
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
!       New arrays for FG and AP selection options. New indices MDAD_LW/SW
!       and illumination flag Illum.
!     4th July 2001, Andy Smith:
!       Comments updated.
!       LastX0 and LastY0 added to Loc structure. Used to record the location of
!       the last successfully retrieved solution Spixel%XnSav, so that validity 
!       of that solution at a given location can be checked.
!       XnSav and SnSav introduced. These hold the last "good" state vector
!       while Xn and Sn hold the most recent.
!     3rd Aug 2001, Andy Smith:
!       Updated Loc structure to include YSeg0, YSegC, YSegn, for referencing
!       pixels within a data segment as opposed to the whole image.
!    ******************************** ECV work starts here *****************
!    23rd Feb 2011, Andy Smith:
!       Re-applying some changes from 2001/2.
!     (14th Aug 2002, Caroline Poulsen:
!       Add in a new variable that tells you about the cloud characteristics
!       around the pixel of concern
!     13th December Caroline Poulsen add in the geopotential height )
!    23rd Feb 2011, Andy Smith:  
!       Cloud flags converted to real, rather than byte, to match current ORAC data. 
!    23rd Mar 2011, Andy Smith:
!       Removal of super-pixel averaging. No need to allocate mask and cloud
!       or surface flags to Ctrl%Resoln%Space. Assume 1 pixel processed at a time 
!       so only 1 flag needed.
!       Removed Fracnext (was used to set a priori / first guess error on fraction).
!       Removed redundant struct members YSegc, YSegn. 
!       Re-size FG array, phase removed (follow-up from phase change removal). 
!     8th Apri 2011, Andy Smith:
!       Phase change removal. On checking code in GetX and InvertMarquardt, 
!       SPixel%Phase, Class and FGPhase are no longer required. 
!    20th Apr 2011, Andy Smith:
!       Extension to handle multiple instrument views. The viewing geometry 
!       becomes a set of arrays, e.g. 1 value of sat. zen angle per view. 
!       New to Ind struct: Nviews and View_Idx.
!    7th October caroline Poulsen added in CWP variable and CWP error
!   8th December Matthias Jerg added data types and structures for netcdf output.
!   8th Jan 2012 Caroline Poulsen added channel info  for netcdfoutput.
!   15th Jan 2012 Caroline Poulsen changed netcdf definitions
!   15th Jan 2012 Caroline Poulsen added albedo
!   15th Jun 2012 Caroline Poulsen changed illum into an array of ny variables
!   4th Oct 2012 Caroline Poulsen added in new variables
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
! 2013/10/02 CP/GT added in variable to calculate degrees of freedom of signal
! 2014/01/16 Greg MCgarragh: Added spixel_y_to_ctrl_y_index.
!
! Bugs:
!    None known.
! 
! $Id$
!
!---------------------------------------------------------------------

module SPixel_def

   use ECP_Constants

!  Define a type for the Short Wave RTM parameters

   type RTM_SW_t
      integer             :: Np           ! Number of presure levels
      real                :: Lat          ! Latitude
      real, pointer       :: P(:)         ! Array of pressure levels (1:SW%Np)
      real, pointer       :: Tbc(:,:)     ! Surface to P channel transmittances 
      real, pointer       :: Tac(:,:)     ! P to TOA channel transmittances
      real, pointer       :: Tsf(:)       ! Transmittance at the surface to TOA
   end type RTM_SW_t

!  Define a type for the Long Wave RTM parameters

   type RTM_LW_t
      integer             :: Np           ! Number of presure levels
      real                :: Lat          ! Latitude
      real                :: Lon          ! Longitude
      real                :: skint          ! skin temperature
      real                :: sp          ! Longitude
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

!  Define the overall RTM structure

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

   type SPixel_Geom_t
      real, pointer       :: SolZen(:)    ! Solar zenith angle
      real, pointer       :: SatZen(:)    ! Satellite zenith angle
      real, pointer       :: RelAzi(:)    ! Relative azimuth
      real, pointer       :: SEC_o(:)     ! Air mass factor at solar zenith angle
      real, pointer       :: SEC_v(:)     ! Air mass factor at viewing angle     
   end type SPixel_Geom_t
   
   type Loc_t
!     X0, Xn, Xc, Y0, Yn, Yc are co-ordinates within the whole image
! AS, Apr 2011: Xc, Yc, Xn, Yn can probably be removed now that super-pixel averaging has gone.
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
!      integer             :: NCloudy      ! Number of cloudy pixels in super pixel
      real                :: Fraction     ! Cloud fraction for super pixel
      real                :: Flags        ! Cloud flags
   end type Cloud_t
   
   type Surface_t
      integer             :: NLand        ! Number of land surface pixels in super pixel
      integer             :: NSea         ! Number of sea surface pixels in super pixel
      integer(kind=byte)  :: Land         ! Super-pixel land surface flag
      integer(kind=byte)  :: Sea          ! Super-pixel sea surface flag
      integer(kind=byte)  :: Flags        ! Surface flags
!       byte                :: Land         ! Super-pixel land surface flag
!       byte                :: Sea          ! Super-pixel sea surface flag
!       byte                :: Flags        ! Surface flags
   end type Surface_t
   
   type SPixel_Ind_t           ! Modified Ctrl variables, depending on geometry 
      integer  :: Ny               ! Number of viable measurement channels
      integer  :: NSolar       ! Number of viable solar channels
      integer  :: NThermal     ! Number of viable thermal channels
      integer  :: ThermalFirst ! Index of first channel with a thermal component
      integer  :: ThermalLast  ! Index of last channel with a thermal component
      integer  :: SolarFirst   ! Index of first channel with a solar component
      integer  :: SolarLast    ! Index of last channel with a solar component
      integer  :: NMixed       ! Number of mixed solar/thermal channels
      integer  :: MDAD_LW      ! Index of channel at (or nearest to)
                               ! 11 um used in MDAD method for setting FG (AP)
                               ! cloud pressure and phase
      integer  :: MDAD_SW      ! Index of channel at (or nearest to) 0.67
                               ! used in MDAD method for setting FG (AP)
                               ! cloud optical depth
      integer  :: NViews       ! Number of instrument views
   end type SPixel_Ind_t
  
   type SPixel_t
      type(SPixel_RTM_t)  :: RTM          ! Super-pixel radiative transfer model parameters
      type(SPixel_Geom_t) :: Geom         ! Super-pixel geometry values
      type(Loc_t)         :: Loc          ! Super-pixel location info
      type(Cloud_t)       :: Cloud        ! Super-pixel cloud flag info
      type(Surface_t)     :: Surface      ! Super-pixel surface flag info
      type(SPixel_Ind_t)  :: Ind          ! Numbers and indices of channels
      integer ,pointer       :: Illum(:)        ! Illumination flag (day,twi, night)
      integer             :: FG(MaxStateVar)
                                          ! Methods for setting first guess 
                                          ! state vector, matched to
                                          ! SPixel conditions.
      integer             :: AP(MaxStateVar)
                                          ! Methods for setting a priori state 
                                          ! vector, matched to SPixel conditions.
      real, pointer       :: Ym(:)        ! Measurements (** Allocated in Get_Measurements **)
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
      real                :: CWP           ! Cloud water path
      real                :: CWP_ERROR           ! Cloud water path error
      integer             :: Mask         ! Averaging mask
      integer             :: NMask        ! Number of 'good' pixels in mask
      integer             :: NAverage     ! Number of pixels used to calculate super-pixel averages
      real, pointer       :: Rs(:)        ! Super pixel surface reflectance
      real, pointer       :: SRs(:,:)     ! Super pixel surface reflectance covariances
      real, pointer       :: f0(:)        ! Solar constant
      integer, pointer    :: spixel_y_to_ctrl_y_index(:) ! Index SPixel measurment
                                                         ! space to CRTL measuement space
      integer, pointer    :: spixel_y_thermal_to_ctrl_y(:) ! Other useful indexes
      integer, pointer    :: spixel_y_solar_to_spixel_y(:)
      integer, pointer    :: spixel_y_thermal_to_spixel_y(:)
    end type SPixel_t
    
    type spixel_scanline_primary_output

       integer :: vidlat, vidlon,vidtime

       integer,dimension(:), pointer :: vidsat_zen,vidsol_zen,vidrel_azi

       integer :: vidcot,vidref, vidctp,vidcct,vidstemp, vidcth
       integer :: vidctt, vidcwp
       integer :: vidcoterror,vidreferror, vidctperror,vidccterror,vidctherror,vidctterror
       integer :: vidstemperror,vidcwperror

       integer :: vidconvergence,vidniter, vidlsflag,vidpchange

       integer :: vidcostja,vidcostjm, vidqcflag, vidillum

       integer :: vidradiance1

       real(kind=dreal) :: time_scale,time_offset,time_vmin,time_vmax,time_fv
       real :: lat_scale,lat_offset,lat_vmin,lat_vmax
       real :: lon_scale,lon_offset,lon_vmin,lon_vmax
       real :: real_fill_value_lat_lon=-999.0
       real :: real_fill_value=-999.0
	!       real :: real_fill_value_lat_lon=-32767.0
	!       real :: real_fill_value=-32767.0
       integer(kind=sint) :: int_fill_value=-32767
       integer(kind=byte) :: byte_fill_value=-127

       real :: sat_scale,sat_offset,sat_vmin,sat_vmax
       real :: sol_scale,sol_offset,sol_vmin,sol_vmax
       real :: azi_scale,azi_offset,azi_vmin,azi_vmax

       real :: cot_scale,cot_offset
       integer(kind=sint) :: cot_vmin,cot_vmax
       
       real :: cot_error_scale,cot_error_offset
       integer(kind=sint) :: cot_error_vmin,cot_error_vmax

       real :: ref_scale,ref_offset
       integer(kind=sint) :: ref_vmin,ref_vmax

       real :: ref_error_scale,ref_error_offset
       integer(kind=sint) :: ref_error_vmin,ref_error_vmax

       real :: ctp_scale,ctp_offset
       integer(kind=sint) :: ctp_vmin,ctp_vmax

       real :: ctp_error_scale,ctp_error_offset
       integer(kind=sint) :: ctp_error_vmin,ctp_error_vmax

       real :: cct_scale,cct_offset
       integer(kind=sint) :: cct_vmin,cct_vmax

       real :: cct_error_scale,cct_error_offset
       integer(kind=sint) :: cct_error_vmin,cct_error_vmax

       real :: stemp_scale, stemp_offset
       integer(kind=sint) ::  stemp_vmin, stemp_vmax

       real :: stemp_error_scale, stemp_error_offset
       integer(kind=sint) ::  stemp_error_vmin, stemp_error_vmax

       real :: albedo_scale, albedo_offset
       integer(kind=sint) ::  albedo_vmin, albedo_vmax

       
       real :: cth_scale, cth_offset
       integer(kind=sint) ::  cth_vmin, cth_vmax

       real :: cth_error_scale,cth_error_offset
       integer(kind=sint) :: cth_error_vmin,cth_error_vmax

       real :: ctt_scale, ctt_offset
       integer(kind=sint) ::  ctt_vmin, ctt_vmax

       real :: ctt_error_scale,ctt_error_offset
       integer(kind=sint) :: ctt_error_vmin,ctt_error_vmax


       real :: cwp_scale,cwp_offset
       integer(kind=sint) :: cwp_vmin,cwp_vmax

       real :: cwp_error_scale,cwp_error_offset
       integer(kind=sint) :: cwp_error_vmin,cwp_error_vmax



       real(kind=sreal) :: costja_scale, costja_offset
       real(kind=sreal) ::  costja_vmin, costja_vmax

       real(kind=sreal) :: costjm_scale, costjm_offset
       real(kind=sreal) ::  costjm_vmin, costjm_vmax

       integer(kind=byte) :: con_scale, con_offset, con_vmin, con_vmax
       integer(kind=byte) :: ls_scale, ls_offset, ls_vmin, ls_vmax

       integer(kind=byte) :: niter_scale, niter_offset, niter_vmin, niter_vmax
       integer(kind=byte) :: pchange_scale, pchange_offset, pchange_vmin, pchange_vmax

       integer(kind=byte) :: illum_scale, illum_offset, illum_vmin, illum_vmax

       integer(kind=sint) :: qc_scale,qc_offset,qc_vmin,qc_vmax

       real(kind=dreal),  dimension(:,:), pointer :: time

       real,  dimension(:,:), pointer :: lon
       real,  dimension(:,:), pointer :: lat

       real,  dimension(:,:,:), pointer :: sat_zen
       real,  dimension(:,:,:), pointer :: sol_zen
       real,  dimension(:,:,:), pointer :: rel_azi

       integer(kind=sint), dimension(:,:), pointer :: cct,cct_error

       integer(kind=sint), dimension(:,:), pointer :: cot,cot_error
       integer(kind=sint), dimension(:,:), pointer :: ref,ref_error

       integer(kind=sint), dimension(:,:), pointer :: ctp,ctp_error

       integer(kind=sint), dimension(:,:), pointer :: stemp,stemp_error

       integer(kind=sint), dimension(:,:), pointer :: albedo

       integer(kind=sint), dimension(:,:), pointer :: ctt,ctt_error
       integer(kind=sint), dimension(:,:), pointer :: cth,cth_error
       integer(kind=sint), dimension(:,:), pointer :: cwp,cwp_error

       integer(kind=byte), dimension(:,:), pointer :: convergence

       integer(kind=byte), dimension(:,:), pointer :: niter
       integer(kind=byte), dimension(:,:), pointer :: pchange

       real(kind=sreal), dimension(:,:), pointer :: costja
       real(kind=sreal), dimension(:,:), pointer :: costjm

       integer(kind=byte), dimension(:,:), pointer :: lsflag

       integer(kind=sint),  dimension(:,:), pointer :: qcflag

       integer(kind=byte), dimension(:,:), pointer :: illum

    end type spixel_scanline_primary_output

    type spixel_scanline_secondary_output

       integer :: vidscanline_u, vidscanline_v

       integer :: vidcotap,vidcotfg
       integer :: vidrefap,vidreffg
       integer :: vidctpap,vidctpfg
       integer :: vidstempfg
       integer :: vidds

       integer, dimension(:,:), pointer :: vidcovar

       integer, dimension(:),pointer :: vidres
       integer, dimension(:),pointer :: vidchans	
       integer, dimension(:),pointer :: vidalb
       integer, dimension(:),pointer :: vidy0
       real :: real_fill_value=-32767.0
       integer :: lint_fill_value=-32767
       integer(kind=sint) :: int_fill_value=-32767
       integer(kind=byte) :: byte_fill_value=-127

       real :: scanline_u_scale,scanline_u_offset
       real :: scanline_v_scale,scanline_v_offset
       integer :: scanline_u_vmin,scanline_u_vmax
       integer :: scanline_v_vmin,scanline_v_vmax
      
       integer, dimension(:,:), pointer :: scanline_u, scanline_v


       real :: cot_ap_scale,cot_ap_offset
       real :: cot_fg_scale,cot_fg_offset
       integer(kind=sint) :: cot_ap_vmin,cot_ap_vmax
       integer(kind=sint) :: cot_fg_vmin,cot_fg_vmax

       integer(kind=sint), dimension(:,:), pointer :: cot_ap,cot_fg

       real :: ref_ap_scale,ref_ap_offset
       real :: ref_fg_scale,ref_fg_offset
       integer(kind=sint) :: ref_ap_vmin,ref_ap_vmax
       integer(kind=sint) :: ref_fg_vmin,ref_fg_vmax

       integer(kind=sint), dimension(:,:), pointer :: ref_ap,ref_fg


       real :: ctp_ap_scale,ctp_ap_offset
       real :: ctp_fg_scale,ctp_fg_offset
       integer(kind=sint) :: ctp_ap_vmin,ctp_ap_vmax
       integer(kind=sint) :: ctp_fg_vmin,ctp_fg_vmax

       integer(kind=sint), dimension(:,:), pointer :: ctp_ap,ctp_fg


       real :: stemp_fg_offset,stemp_fg_scale
       integer(kind=sint) :: stemp_fg_vmin,stemp_fg_vmax
       integer(kind=sint), dimension(:,:), pointer :: stemp_fg

       real, dimension(:), pointer :: res_scale,res_offset
       real, dimension(:), pointer :: chans_scale,chans_offset        
       integer(kind=sint), dimension(:), pointer :: res_vmin,res_vmax
       integer(kind=sint), dimension(:), pointer ::  chans_vmin,chans_vmax

       integer(kind=sint), dimension(:), pointer :: alb_vmin,alb_vmax
       real, dimension(:), pointer :: alb_scale,alb_offset        


       real, dimension(:), pointer :: y0_scale,y0_offset        
       integer(kind=sint), dimension(:), pointer :: y0_vmin,y0_vmax


       integer(kind=sint), dimension(:,:,:), pointer :: residuals
       integer(kind=sint), dimension(:,:,:), pointer :: albedo
       integer(kind=sint), dimension(:,:,:), pointer :: y0
       integer(kind=sint), dimension(:,:,:), pointer :: channels

       real(kind=sreal), dimension(:,:,:,:), pointer :: covariance

       ! Degrees of freedom for signal
       real :: ds_offset, ds_scale
       integer(kind=sint) :: ds_vmin, ds_vmax
       integer(kind=sint), dimension(:,:), pointer :: ds

    end type spixel_scanline_secondary_output


    type spixel_scanline_input

       integer, dimension(:),pointer :: vidinput,viderror
       real :: real_fill_value=-32767.0
       integer(kind=sint) :: int_fill_value=-32767
       integer(kind=byte) :: byte_fill_value=-127
       real, dimension(:), pointer :: input_scale,input_offset
       integer(kind=sint), dimension(:), pointer :: input_vmin,input_vmax

       real, dimension(:), pointer :: error_scale,error_offset
       integer(kind=sint), dimension(:), pointer :: error_vmin,error_vmax

       integer(kind=sint), dimension(:,:), pointer :: input,error
       
    end type spixel_scanline_input


end module SPixel_def
   
