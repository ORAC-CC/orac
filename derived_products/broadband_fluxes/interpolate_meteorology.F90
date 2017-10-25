!-------------------------------------------------------------------------------
! Name: interpolate_meteorology.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/10/14, MC: Initial implementation
! 2015/11/21, GM: Wrap was being passed to bilinear_coef_reg_reg() without being
!    set to anything.  Added the proper setting.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine interpolate_meteorology(lon,lat,nlev,xdim,ydim, &
                                   P,T,H,Q,O3, &
                                   ilon,ilat,iP,iT,iH,iQ,iO3)

   use interpol_m

   implicit none

   ! Input arguments
   integer, intent(in) :: nlev,xdim,ydim
   real, intent(in), dimension(xdim,ydim) :: lon,lat         ! PRTM longitude and latitude arrays
   real, intent(in), dimension(nlev,xdim,ydim) :: P,T,H,Q,O3 ! PRTM 3D variables
   real, intent(in) :: ilon,ilat                             ! satellite longitude and latitude

   ! Output arguments
   real, intent(out), dimension(nlev) :: iP,iT,iH,iQ,iO3 ! interpolated profiles

   ! Local variables
   type(interpol_t) :: interp
   logical :: Wrap
   integer :: NLat,NLon ! # of lat/lon indices
   real(8) :: Lat0,LatN,Lon0,LonN,delta_Lat,inv_delta_Lat,delta_Lon,inv_delta_Lon ! inputs to bilinear
   real(8) :: MinLon,MaxLon

   NLat = ydim
   NLon = xdim

   Lat0 = real(lat(1,1), kind=8)
   LatN = real(lat(NLat,1), kind=8)
   Lon0 = real(lon(1,1), kind=8)
   LonN = real(lon(NLon,1), kind=8)

   delta_Lat = (LatN - Lat0) / (NLat-1)
   inv_delta_Lat = 1. / delta_Lat
   delta_Lon = (LonN - Lon0) / (NLon-1)
   inv_delta_Lon = 1. / delta_Lon

   ! Max and min lon values
   MinLon = min(Lon0-0.5*delta_Lon, LonN+0.5*delta_Lon)
   MaxLon = max(Lon0-0.5*delta_Lon, LonN+0.5*delta_Lon)

   ! Does the grid wrap around the international date-line?
   Wrap = MinLon <= -180. .and. MaxLon >=  180.

   ! Compute bilinear interpolation coefficients common to all interpolations
   call bilinear_coef_reg_reg(Lon0, inv_delta_Lon, &
        NLon, Lat0, inv_delta_Lat, &
        NLat, ilon, ilat, interp, &
        Wrap)

   call interp_field2(P, iP, interp)
   call interp_field2(T, iT, interp)
   call interp_field2(H, iH, interp)
   call interp_field2(Q, iQ, interp)
   call interp_field2(O3, iO3, interp)

end subroutine interpolate_meteorology
