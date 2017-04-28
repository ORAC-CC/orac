!-------------------------------------------------------------------------------
! Name: cloud_emis.F90
!
! Purpose:
! Module for computing cloud emissivity.
! For details see: Pavolonis, M. (2010), "Advanced in Extracting Cloud
!   Composition Information from Spacebourne Infrared Radiances - A robust
!   Alternative to Brightness Temperatures. Part I: Theory, JAMC(49)
!
! History:
! 2017/03/29, SP: First version (EKWork)
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module cloud_emis_m

   implicit none

contains

subroutine get_cloud_emis(channel_info,imager_measurements,imager_geolocation,&
     preproc_dims,preproc_geoloc,preproc_cld,imager_cloud,ecmwf,sensor,verbose)

   use channel_structures_m
   use imager_structures_m
   use interpol_m
   use preproc_constants_m
   use preproc_structures_m
   use ecmwf_m, only : ecmwf_t

   implicit none

   type(channel_info_t),         intent(in)    :: channel_info
   type(imager_measurements_t),  intent(in)   :: imager_measurements
   type(imager_geolocation_t),   intent(in)    :: imager_geolocation
   type(preproc_dims_t),         intent(in)    :: preproc_dims
   type(preproc_geoloc_t),       intent(in)    :: preproc_geoloc
   type(preproc_cld_t),          intent(in)    :: preproc_cld
   type(imager_cloud_t),         intent(out)   :: imager_cloud
   type(ecmwf_t),                intent(in)    :: ecmwf
   character(len=sensor_length), intent(in)    :: sensor
   logical,                      intent(in)    :: verbose

   real(kind=sreal), allocatable, dimension(:,:) :: cldbt,clrbt
   type(interpol_t), allocatable, dimension(:)   :: interp
   integer :: i, j, chan_n, good_chan_lw, good_chan_all

   ! Interpolation variables
   real           :: Lat0,Lon0,LatN,LonN, MinLat, MinLon, MaxLat, MaxLon
   real           :: delta_lat,delta_lon,inv_delta_Lat,inv_delta_Lon
   real           :: rad_clr,rad_cld,rad_obs,t1,t2,emis
   real,parameter :: c1   =   1.191042e8
   real,parameter :: c2   =   1.4387752e4
   real,parameter :: lam=   10.8
   integer        :: NLat,NLon
   logical        :: Wrap

   good_chan_lw = -1
   good_chan_all = -1

   allocate(cldbt(imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   cldbt=sreal_fill_value
   allocate(clrbt(imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   clrbt=sreal_fill_value
   allocate(interp(1))

   ! Needed for grid interpolation, adapted from ../src/ReadPRTM_nc.F90
   NLat = abs(preproc_dims%min_lat - preproc_dims%max_lat)
   NLon = abs(preproc_dims%min_lon - preproc_dims%max_lon)
   Lat0 = real(preproc_geoloc%latitude(preproc_dims%min_lat), kind=8)
   LatN = real(preproc_geoloc%latitude(preproc_dims%max_lat), kind=8)
   Lon0 = real(preproc_geoloc%longitude(preproc_dims%min_lon), kind=8)
   LonN = real(preproc_geoloc%longitude(preproc_dims%max_lon), kind=8)

   ! Grid spacing and inverse
   delta_Lat = (LatN - Lat0) / (NLat-1)
   delta_Lon = (LonN - Lon0) / (NLon-1)

   ! Max and Min grid values
   MinLon = min(Lon0-0.5*delta_Lon, LonN+0.5*delta_Lon)
   MaxLon = max(Lon0-0.5*delta_Lon, LonN+0.5*delta_Lon)

   ! Does the grid wrap around the international date-line?
   Wrap = MinLon <= -180. .and. MaxLon >=  180.

   if (trim(adjustl(sensor)) .eq. 'AATSR' .or. &
       trim(adjustl(sensor)) .eq. 'ATSR2') then
      chan_n = 6
   else if (trim(adjustl(sensor)) .eq. 'AHI') then
      chan_n = 13
   else if (trim(adjustl(sensor)) .eq. 'AVHRR') then
      chan_n = 5
   else if (trim(adjustl(sensor)) .eq. 'MODIS') then
      chan_n = 31
   else if (trim(adjustl(sensor)) .eq. 'SEVIRI') then
      chan_n = 9
   else if (trim(adjustl(sensor)) .eq. 'SLSTR') then
      chan_n = 8
   else if (trim(adjustl(sensor)) .eq. 'VIIRS') then
      chan_n = 15
   end if

   do i=1,channel_info%nchannels_total
      if (channel_info%channel_ids_instr(i) .eq. chan_n) then
         good_chan_all = i
         good_chan_lw = channel_info%map_ids_channel_to_lw(i)
      end if
   end do

   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx

         call bilinear_coef(preproc_geoloc%longitude, NLon, &
              preproc_geoloc%latitude, NLat, imager_geolocation%longitude(j,i), &
              imager_geolocation%latitude(j,i), interp(1),Wrap)

         call interp_field (preproc_cld%cloud_bt(:,:,good_chan_lw), cldbt(j,i), interp(1))
         call interp_field (preproc_cld%clear_bt(:,:,good_chan_lw), clrbt(j,i), interp(1))
      end do
   end do
   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx
         t1      = exp(c2/(lam*cldbt(j,i)))
         t2      = (t1-1)
         rad_cld = c1/(t2*(lam**5))
         t1      = exp(c2/(lam*clrbt(j,i)))
         t2      = (t1-1)
         rad_clr = c1/(t2*(lam**5))
         t1      = exp(c2/(lam*imager_measurements%data(j,i,good_chan_all)))
         t2      = (t1-1)
         rad_obs = c1/(t2*(lam**5))

         emis    = (rad_obs-rad_clr)/(rad_cld-rad_clr)
         if (emis .lt. 0) emis=0.
         imager_cloud%cloud_emis(j,i) = emis
        end do
   end do

   deallocate(cldbt)
   deallocate(clrbt)
   deallocate(interp)

end subroutine get_cloud_emis

end module cloud_emis_m
