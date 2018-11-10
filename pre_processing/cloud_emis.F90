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
! 2017/03/29, SP: First version (ExtWork)
! 2018/04/29, SP: Add cloud emissivity support for ECMWF profiles (ExtWork)
! 2018/05/24, SP: Updates to better calculate tropopause pressure (from IntCTP.F90)
! 2018/07/18, DE: Add tropopause temperature
! 2018/11/05, SP: Add CAPE
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module cloud_emis_m

   implicit none

contains

subroutine get_trop_tp(preproc_prtm,preproc_dims)

   use preproc_constants_m
   use preproc_structures_m
   use omp_lib

   type(preproc_prtm_t),           intent(inout)    :: preproc_prtm
   type(preproc_dims_t),           intent(in)    :: preproc_dims

   real,    parameter :: min_tropopause = 40.0  ! Heighest p allowed for trop
   real,    parameter :: max_tropopause = 450.0 ! Lowest p allowed for trop
   integer, parameter :: depth          = 2     ! # layers added to inversions

   integer                            :: nx,ny,nz ! Number of vertical levels, pixels
   integer                            :: x,y      ! Looping variables over preproc
   integer                            :: k, l     ! Indexing variables
   integer                            :: step     ! Direction of search
   real, dimension(preproc_dims%kdim) :: t        ! Temperature profile
   real, dimension(preproc_dims%kdim) :: p        ! Pressure profile
   real, dimension(preproc_dims%kdim) :: h        ! Height profile


   nx = preproc_dims%xdim
   ny = preproc_dims%ydim
   nz = preproc_dims%kdim


   do x=preproc_dims%min_lon,preproc_dims%max_lon
      do y=preproc_dims%min_lat,preproc_dims%max_lat

         k = nz
         t  = preproc_prtm%temperature(x,y,:)
         p  = preproc_prtm%pressure(x,y,:)/100.
         h  = (0.001 / g_wmo) * preproc_prtm%phi_lev(x,y,1:preproc_dims%kdim)

         do while (p(k) > max_tropopause .and. k > 1)
            k = k-1
         end do
         step = nz
         if (x .eq. 262 .and. y .eq. 161) then
            do while (step > 1)
               step = step - 1
            end do
         end if

         ! Locate the tropopause
         do while (p(k) > min_tropopause .and. k > 1)
            ! The tropopause is defined as the lowest level with a lapse rate
            ! less than 2 K km^-1
            if ((t(k) - t(k-1)) / (h(k-1) - h(k)) < 2.) then
               ! Find the first level at least 2 km above the identified level
               l = k-1
               do while (h(l) - h(k) < 2. .and. l > 1)
                  l = l-1
               end do

               ! We also require that the lapse rate remain this low in the 2km
               ! above the tropopause
               if ((t(k) - t(l)) / (h(l) - h(k)) < 2.) exit
            end if

            ! Continue to the next level up
            k = k-1
         end do
         if (k .lt. nz) then
            preproc_prtm%trop_p(x,y) = p(k)
            preproc_prtm%trop_t(x,y) = t(k)
         else
            preproc_prtm%trop_p(x,y) = sreal_fill_value
            preproc_prtm%trop_t(x,y) = sreal_fill_value
         end if
      end do
   end do
   return

end subroutine get_trop_tp

subroutine get_cloud_emis(channel_info,imager_measurements,imager_geolocation, &
     preproc_dims,preproc_geoloc,preproc_cld,preproc_prtm,imager_cloud,ecmwf,&
     sensor, verbose)

   use channel_structures_m
   use ecmwf_m, only : ecmwf_t
   use imager_structures_m
   use interpol_m
   use preproc_constants_m
   use preproc_structures_m

#ifdef INCLUDE_SATWX
	use satwx_emis_funcs
#endif

   implicit none

   type(channel_info_t),         intent(in)    :: channel_info
   type(imager_measurements_t),  intent(in)    :: imager_measurements
   type(imager_geolocation_t),   intent(in)    :: imager_geolocation
   type(preproc_dims_t),         intent(in)    :: preproc_dims
   type(preproc_geoloc_t),       intent(in)    :: preproc_geoloc
   type(preproc_cld_t),          intent(in)    :: preproc_cld
   type(preproc_prtm_t),         intent(in)    :: preproc_prtm
   type(imager_cloud_t),         intent(out)   :: imager_cloud
   type(ecmwf_t),                intent(in)    :: ecmwf
   character(len=sensor_length), intent(in)    :: sensor
   logical,                      intent(in)    :: verbose

   real(kind=sreal), allocatable, dimension(:,:,:) :: indata
   real(kind=sreal), allocatable, dimension(:,:) :: cldbt,clrbt
   real(kind=sreal), allocatable, dimension(:,:) :: cldbtwv,clrbtwv
   type(interpol_t), allocatable, dimension(:)   :: interp
   integer :: i, j
   integer :: chan_n, good_chan_lw, good_chan_all
   integer :: chanwv_n, good_chanwv_lw, good_chanwv_all
   integer :: extent(4)

   ! Interpolation variables
   real           :: Lat0,Lon0,LatN,LonN, MinLon, MaxLon
   real           :: delta_lat,delta_lon
   real           :: rad_clr,rad_cld,rad_obs,t1,t2,emis
   real,parameter :: c1  = 1.191042e8
   real,parameter :: c2  = 1.4387752e4
   real,parameter :: lam1 = 10.8
   real,parameter :: lam2 = 6.2
   integer        :: NLat,NLon
   logical        :: Wrap, do_wv

#ifdef INCLUDE_SATWX

   good_chan_lw = -1
   good_chan_all = -1
   good_chanwv_lw = -1
   good_chanwv_all = -1

   do_wv = .true.

   extent(1) = imager_geolocation%startx
   extent(2) = imager_geolocation%endx
   extent(3) = 1
   extent(4) = imager_geolocation%ny

   allocate(cldbt(extent(1):extent(2),extent(3):extent(4)))
   allocate(clrbt(extent(1):extent(2),extent(3):extent(4)))
   allocate(cldbtwv(extent(1):extent(2),extent(3):extent(4)))
   allocate(clrbtwv(extent(1):extent(2),extent(3):extent(4)))

   allocate(indata(extent(1):extent(2),extent(3):extent(4),2))

   cldbt=sreal_fill_value
   clrbt=sreal_fill_value
   cldbtwv=sreal_fill_value
   clrbtwv=sreal_fill_value

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
      if (do_wv) then
         if (verbose)write(*,*)"WARNING: Cannot process WV cloud emis for (A)ATSR."
         do_wv=.false.
      end if
   else if (trim(adjustl(sensor)) .eq. 'ABI') then
      chan_n = 14
      chanwv_n = 8
   else if (trim(adjustl(sensor)) .eq. 'AHI') then
      chan_n = 14
      chanwv_n = 8
   else if (trim(adjustl(sensor)) .eq. 'AVHRR') then
      chan_n = 5
      if (do_wv) then
         if (verbose)write(*,*)"WARNING: Cannot process WV cloud emis for AVHRR."
         do_wv=.false.
      end if
   else if (trim(adjustl(sensor)) .eq. 'MODIS') then
      chan_n = 31
      chanwv_n = 27
   else if (trim(adjustl(sensor)) .eq. 'SEVIRI') then
      chan_n = 9
      chanwv_n = 5
   else if (trim(adjustl(sensor)) .eq. 'SLSTR') then
      chan_n = 8
      if (do_wv) then
         if (verbose)write(*,*)"WARNING: Cannot process WV cloud emis for SLSTR."
         do_wv=.false.
      end if
   else if (trim(adjustl(sensor)) .eq. 'VIIRS') then
      chan_n = 15
      if (do_wv) then
         if (verbose)write(*,*)"WARNING: Cannot process WV cloud emis for VIIRS."
         do_wv=.false.
      end if
   end if

   do i=1,channel_info%nchannels_total
      if (channel_info%channel_ids_instr(i) .eq. chan_n) then
         good_chan_all = i
         good_chan_lw = channel_info%map_ids_channel_to_lw(i)
      end if
      if (channel_info%channel_ids_instr(i) .eq. chanwv_n) then
         good_chanwv_all = i
         good_chanwv_lw = channel_info%map_ids_channel_to_lw(i)
      end if
   end do

   if (good_chan_all .lt. 0 .or. good_chan_lw .lt. 0) then
      write(*,*)"ERROR: The longwave channel required for cloud emissivity (",chan_n,") is not available!"
      stop
   end if
   if (do_wv) then
      if (good_chanwv_all .lt. 0 .or. good_chanwv_lw .lt. 0) then
         write(*,*)"ERROR: The longwave channel required for cloud emissivity (",chan_n,") is not available!"
         stop
      end if
   end if

   !$OMP PARALLEL &
   !$OMP PRIVATE(i) &
   !$OMP PRIVATE(j) &
   !$OMP PRIVATE(interp)
   !$OMP DO SCHEDULE(GUIDED)
   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx

         call bilinear_coef(preproc_geoloc%longitude, NLon, &
              preproc_geoloc%latitude, NLat, imager_geolocation%longitude(j,i), &
              imager_geolocation%latitude(j,i), interp(1),Wrap)

         ! Longwave channel (10.8 micron)
         call interp_field (preproc_cld%cloud_bt(:,:,good_chan_lw), &
              cldbt(j,i), interp(1))
         call interp_field (preproc_cld%clear_bt(:,:,good_chan_lw), &
              clrbt(j,i), interp(1))

         ! Interpolate tropopause info onto satellite grid
         call interp_field (preproc_prtm%trop_t(:,:), &
              imager_cloud%trop_t(j,i), interp(1))
         call interp_field (preproc_prtm%trop_p(:,:), &
              imager_cloud%trop_p(j,i), interp(1))
         call interp_field (preproc_prtm%cape(:,:), &
              imager_cloud%cape(j,i), interp(1))

         ! WV channel (~7 micron)
         if (do_wv) then
            call interp_field (preproc_cld%cloud_bt(:,:,good_chanwv_lw), &
                 cldbtwv(j,i), interp(1))
            call interp_field (preproc_cld%clear_bt(:,:,good_chanwv_lw), &
                 clrbtwv(j,i), interp(1))
         end if
      end do
   end do
   !$OMP END DO
   !$OMP END PARALLEL

   indata(:,:,1) = imager_measurements%data(:,:,good_chan_all)
   indata(:,:,2) = imager_measurements%data(:,:,good_chanwv_all)

   call calc_cloud_emis(cldbt,clrbt,cldbtwv,clrbtwv,indata,imager_cloud%cloud_emis,extent,verbose)


	where (imager_geolocation%latitude .eq. sreal_fill_value)
		imager_cloud%trop_t(:,:) = sreal_fill_value
		imager_cloud%trop_p(:,:) = sreal_fill_value
		imager_cloud%cape(:,:) = sreal_fill_value
	end where
	where (imager_geolocation%longitude .eq. sreal_fill_value)
		imager_cloud%trop_t(:,:) = sreal_fill_value
		imager_cloud%trop_p(:,:) = sreal_fill_value
		imager_cloud%cape(:,:) = sreal_fill_value
	end where

   deallocate(cldbt)
   deallocate(clrbt)
   deallocate(cldbtwv)
   deallocate(clrbtwv)
   deallocate(interp)
   deallocate(indata)

#endif

end subroutine get_cloud_emis


subroutine do_cb_detect(channel_info,imager_measurements,imager_geolocation,imager_cloud, imager_pavolonis, sensor, verbose)

   use channel_structures_m
   use ecmwf_m, only : ecmwf_t
   use imager_structures_m
   use interpol_m
   use preproc_constants_m
   use preproc_structures_m

#ifdef INCLUDE_SATWX
	use satwx_conv_funcs
#endif

   implicit none

   type(channel_info_t),         intent(in)    :: channel_info
   type(imager_measurements_t),  intent(in)    :: imager_measurements
   type(imager_geolocation_t),   intent(in)    :: imager_geolocation
   type(imager_cloud_t),   intent(in)    		  :: imager_cloud
   type(imager_pavolonis_t),     intent(inout) :: imager_pavolonis
   character(len=sensor_length), intent(in)    :: sensor
   logical,                      intent(in)    :: verbose

#ifdef INCLUDE_SATWX
   integer 					:: extent(4),n_chans

   real(kind=sreal), pointer, dimension(:,:,:) 		::	sat_data
   real(kind=sreal), pointer, dimension(:)			::	channel_wl_abs

   extent(1) = imager_geolocation%startx
   extent(2) = imager_geolocation%endx
   extent(3) = 1
   extent(4) = imager_geolocation%ny


   n_chans = channel_info%nchannels_total

   sat_data => imager_measurements%data
   channel_wl_abs => channel_info%channel_wl_abs

	call calc_convection(channel_wl_abs, n_chans, sat_data, imager_cloud%cape, imager_pavolonis%cldtype, extent, verbose)

	where (imager_geolocation%latitude .eq. sreal_fill_value)
		imager_pavolonis%cldtype(:,:,1) = byte_fill_value
	end where
	where (imager_geolocation%longitude .eq. sreal_fill_value)
		imager_pavolonis%cldtype(:,:,1) = byte_fill_value
	end where

#else
	write(*,*) "ERROR: Cannot detect convection without linking to SatWx"
	stop
#endif

end subroutine do_cb_detect

end module cloud_emis_m
