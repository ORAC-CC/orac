   subroutine interpolate_meteorology(lon,lat,nlev,xdim,ydim,&
                                      P,T,H,Q,O3,&
                                      ilon,ilat,iP,iT,iH,iQ,iO3)

   use interpol

   implicit none

   !Input arguments
   integer, intent(in) :: nlev,xdim,ydim
   real, dimension(xdim,ydim) :: lon,lat !prtm longitud and latitude arrays
   real, dimension(nlev,xdim,ydim) :: P,T,H,Q,O3 ! prtm 3D variables
   real ilon,ilat !satellite longitude and latitude

   !Output arguments - interpolated values
   real, intent(out), dimension(nlev) :: iP,iT,iH,iQ,iO3 !interpolated profiles

   !Local variables
   type(interpol_s) :: interp
   logical :: Wrap
   integer  NLat, NLon !#of lat/lon indices
   real(8) :: Lat0, LatN, Lon0, LonN, delta_Lat, inv_delta_Lat, delta_Lon, inv_delta_Lon !inputs to bilinear




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

   ! Bilinear interpolation (method taken from Numerical Recipes p96, 1987)
   call bilinear_coef_reg_reg(Lon0, inv_delta_Lon, &
        NLon, Lat0, inv_delta_Lat, &
        NLat, ilon, ilat, interp, &
        Wrap)
   
   call interp_field2(P, iP, interp)
   call interp_field2(T, iT, interp)
   call interp_field2(H, iH, interp)
   call interp_field2(Q, iQ, interp)
   call interp_field2(O3, iO3, interp)

   return
end subroutine interpolate_meteorology
