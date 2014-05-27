!-------------------------------------------------------------------------------
! Name:
!   RTM
!
! Purpose:
!   Module defining radiative transfer model structure (RTM) for the ECP
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!   Name Type Description
!
! History:
!    15th Dec 2000, Kevin Smith: Original version
!    11th Jan 2001, Kevin Smith: Added Grid substructures for use in Get_RTM
!    26th Jan 2001, Andy Smith:
!       Added comments to explain variable names.
!    21st Feb 2001, Andy Smith:
!       Added Tbc to LW struct. Previously missing from RTM model data file.
!     1st Mar 2001, Andy Smith:
!       Removed R_Clear from LW struct. Not available from RTM data file.
!    30th Mar 2001, Andy Smith:
!       Added MaxLat, MinLat, MaxLon, MinLon to Grid structure, so that these
!       can be calculated once and stored (otherwise checked many times in
!       GetRTM). These are different from Lat0, LatN etc as they make no
!       assumption about the order in which lat/lon values are supplied
!       (lowest to highest or vice-versa).
!    22nd Jun 2001, Andy Smith:
!       Improved commenting for Lw arrays.
!    **************** ECV work starts here *************************************
!     8th Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    13th Dec 2001, Caroline Poulsen:
!       Added in geopotential height.
!    22th Sep 2011, Caroline Poulsen:
!       Modified SWRTM to vary with lat/lon not just latitude, remove variables
!       that are the same as LWRTM
!     4th Sep 2012, Caroline Poulsen:
!       Added skipt and sp variable.
!    27th May 2014, Greg McGarragh:
!       Some cleanup.
!
! Bugs:
!   None known
!
! $Id$
!
!-------------------------------------------------------------------------------

module RTM_def

   type RTM_Grid_t
      real             :: Lat0             ! Starting latitude value
      real             :: LatN             ! End latitude value
      integer          :: NLat             ! No. of latitude values
      integer          :: NLatLon          ! No. of latitude values
      real             :: delta_Lat        ! Lat grid step size
      real             :: inv_delta_Lat    ! 1/delta_Lat
      real             :: Lon0             ! Starting longitude
      real             :: LonN             ! Ending longitude
      integer          :: NLon             ! No. of longitude values
      real             :: delta_Lon        ! Lon grid step size
      real             :: inv_delta_Lon    ! 1/delta_Lon
      real             :: MinLat           ! Minimum lat value in grid
      real             :: MinLon           ! Minimum lon value in grid
      real             :: MaxLat           ! Maximum lat value in grid
      real             :: MaxLon           ! Maximum lon value in grid
   end type RTM_Grid_t

   ! Define short-wave sub-structure

   type SW_t
      integer          :: NP               ! No of pressure levels
      integer          :: NPLAY            ! No of pressure layers
      integer          :: NSWF             ! Number of solar channels in SWRTM File
      integer          :: NV               ! Number of views in SWRTM File
      real,    pointer :: Lat(:,:)         ! Latitude values in grid (lat,lon)
      real,    pointer :: Lon(:,:)         ! Longitude values (lat,lon)
      real,    pointer :: Tac(:,:,:,:)     ! Transmittance above cloud      ! (lat,lon, channel, pressure)
      real,    pointer :: Tbc(:,:,:,:)     ! Transmittance below cloud
                                           ! (lat, lon, channel, pressure)
      type(RTM_Grid_t) :: Grid
   end type SW_t

   ! Define long-wave sub-structure

   type LW_t
      integer          :: NP               ! No of pressure levels
      integer          :: NPLAY            ! No of pressure layers
      integer          :: NLWF             ! Number of thermal channels in LWRTM File
      integer          :: NV               ! Number of views in LWRTM File
      real,    pointer :: Lat(:,:)         ! Latitude values in grid (lat,lon)
      real,    pointer :: Lon(:,:)         ! Longitude values (lat,lon)
      real,    pointer :: skint(:,:)       ! skin temperature
      real,    pointer :: sp(:,:)          ! surface pressure
      real,    pointer :: P(:,:,:)         ! Pressure values (lat,lon,pressure)
      real,    pointer :: T(:,:,:)         ! Temperatures (lat,lon,pressure)
      real,    pointer :: H(:,:,:)         ! geopotential height
      real,    pointer :: Bs(:,:,:)        ! Planck radiances at surface
                                           ! (lat,lon,channels)
      real,    pointer :: Ems(:,:,:)       ! Emissivity (lat,lon,channels)
      real,    pointer :: Tac(:,:,:,:)     ! Transmittance above cloud
                                           ! (lat,lon,channels,pressure)
      real,    pointer :: Tbc(:,:,:,:)     ! Transmittance below cloud
                                           ! (lat,lon,channels,pressure)
      real,    pointer :: Rac_up(:,:,:,:)  ! Upwelling radiance above cloud
                                           ! (lat,lon,channels,pressure)
      real,    pointer :: Rac_dwn(:,:,:,:) ! Downwelling radiance above cloud
                                           ! (lat,lon,channels,pressure)
      real,    pointer :: Rbc_up(:,:,:,:)  ! Upwelling radiance below cloud
                                           ! (lat,lon,channels,pressure)
!     real,    pointer :: R_clear(:,:,:)
      type(RTM_Grid_t) :: Grid
   end type LW_t

   type RTM_t
      type(LW_t)       :: LW
      type(SW_t)       :: SW
   end type RTM_t

contains

   include 'DeallocRTM.F90'

end module RTM_def
