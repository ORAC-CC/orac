!-------------------------------------------------------------------------------
! Name:
!    RTM
!
! Purpose:
!    Module defining radiative transfer model structure (RTM) for the ECP
!
! Arguments:
!     Name Type In/Out/Both Description
!     N/A
!
! Algorithm:
!     N/A
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
!    13th Aug 2014, Adam Povey:
!       Adding Wrap flag to save repeated calculation in GetLwSwRTM.
!    19th Jan 2015, Greg McGarragh:
!       Put ReadSwRTM_nc.F90 and ReadLwRTM_nc.F90 into this module.
!    30th Jan 2015, Adam Povey:
!       Eliminate redundant fields.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module RTM_def

   type RTM_Grid_t
      real(8)          :: Lat0             ! Starting latitude
      real(8)          :: LatN             ! Ending latitude
      integer          :: NLat             ! No. of latitude values
      real(8)          :: delta_Lat        ! Lat grid step size
      real(8)          :: inv_delta_Lat    ! 1/delta_Lat
      real(8)          :: Lon0             ! Starting longitude
      real(8)          :: LonN             ! Ending longitude
      integer          :: NLon             ! No. of longitude values
      real(8)          :: delta_Lon        ! Lon grid step size
      real(8)          :: inv_delta_Lon    ! 1/delta_Lon
      integer          :: NLatLon          ! NLat * NLon
      real             :: MinLat           ! Minimum lat value in grid
      real             :: MinLon           ! Minimum lon value in grid
      real             :: MaxLat           ! Maximum lat value in grid
      real             :: MaxLon           ! Maximum lon value in grid
      logical          :: Wrap             ! Does this grid wrap in lon?
   end type RTM_Grid_t

   ! Define short-wave sub-structure

   type SW_t
      integer          :: NP               ! No of pressure levels
      integer          :: NSWF             ! Number of solar channels in SWRTM File
      integer          :: NV               ! Number of views in SWRTM File
      real,    pointer :: Tac(:,:,:,:)     ! Transmittance above cloud
                                           ! (lat,lon, channel, pressure)
      real,    pointer :: Tbc(:,:,:,:)     ! Transmittance below cloud
                                           ! (lat, lon, channel, pressure)
      type(RTM_Grid_t) :: Grid
   end type SW_t

   ! Define long-wave sub-structure

   type LW_t
      integer          :: NP               ! No of pressure levels
      integer          :: NLWF             ! Number of thermal channels in LWRTM File
      integer          :: NV               ! Number of views in LWRTM File
      real,    pointer :: Lat(:,:)         ! Latitude values in grid (lat,lon)
      real,    pointer :: Lon(:,:)         ! Longitude values (lat,lon)
      real,    pointer :: P(:,:,:)         ! Pressure values (lat,lon,pressure)
      real,    pointer :: T(:,:,:)         ! Temperatures (lat,lon,pressure)
      real,    pointer :: H(:,:,:)         ! geopotential height
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
      type(RTM_Grid_t) :: Grid
   end type LW_t

   type RTM_t
      type(LW_t)       :: LW
      type(SW_t)       :: SW
   end type RTM_t

contains

#include "DeallocRTM.F90"
#include "ReadSwRTM_nc.F90"
#include "ReadLwRTM_nc.F90"

end module RTM_def
