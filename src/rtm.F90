!-------------------------------------------------------------------------------
! Name: rtm.F90
!
! Purpose:
! Module defining radiative transfer model structure (RTM) for the ORAC
!
! History:
! 2000/12/15, KS: Original version
! 2001/01/11, KS: Added Grid substructures for use in Get_RTM
! 2001/01/26, AS: Added comments to explain variable names.
! 2001/02/21, AS: Added Tbc to LW struct. Previously missing from RTM model data
!    file.
! 2001/03/01, AS: Removed R_Clear from LW struct. Not available from RTM data
!    file.
! 2001/03/30, AS: Added MaxLat, MinLat, MaxLon, MinLon to Grid structure, so
!    that these can be calculated once and stored (otherwise checked many times
!    in GetRTM). These are different from Lat0, LatN etc as they make no
!    assumption about the order in which lat/lon values are supplied (lowest to
!    highest or vice-versa).
! 2001/06/22, AS: Improved commenting for Lw arrays.
!    **************** ECV work starts here *************************************
! 2011/02/08, AS: Re-introducing changes made in late 2001/2002.
! 2001/12/13, CP: Added in geopotential height.
! 2011/09/22, CP: Modified SWRTM to vary with lat/lon not just latitude, remove
!    variables that are the same as LWRTM
! 2012/09/04, CP: Added skipt and sp variable.
! 2014/05/27, GM: Some cleanup.
! 2014/08/13, AP: Adding Wrap flag to save repeated calculation in GetLwSwRTM.
! 2015/01/19, GM: Put ReadSwRTM_nc.F90 and ReadLwRTM_nc.F90 into this module.
! 2015/01/30, AP: Eliminate redundant fields.
! 2015/04/27, AP: Moved PRTM code into its own routine.
! 2016/01/20, GM: Move fields in LW_t and SW_t that are common to both up one
!    level into RTM_t.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module RTM_m

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


   type SW_t
      integer          :: NSWF             ! Number of solar channels in SWRTM File
      real,    pointer :: Tac(:,:,:,:)     ! Transmittance above cloud
                                           ! (lat,lon, channel, pressure)
      real,    pointer :: Tbc(:,:,:,:)     ! Transmittance below cloud
                                           ! (lat, lon, channel, pressure)
   end type SW_t


   type LW_t
      integer          :: NLWF             ! Number of thermal channels in LWRTM File
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
   end type LW_t


   type RTM_t
      integer          :: NP               ! No of pressure levels
      integer          :: NV               ! Number of views in SWRTM File
      real,    pointer :: Lat(:,:)         ! Latitude values in grid (lat,lon)
      real,    pointer :: Lon(:,:)         ! Longitude values (lat,lon)
      real,    pointer :: P(:,:,:)         ! Pressure values (lat,lon,pressure)
      real,    pointer :: T(:,:,:)         ! Temperatures (lat,lon,pressure)
      real,    pointer :: H(:,:,:)         ! geopotential height
      type(RTM_Grid_t) :: Grid
      type(LW_t)       :: LW
      type(SW_t)       :: SW
   end type RTM_t

contains

#include "dealloc_rtm.F90"
#include "read_prtm.F90"
#include "read_swrtm.F90"
#include "read_lwrtm.F90"

end module RTM_m
