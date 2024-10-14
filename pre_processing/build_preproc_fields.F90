!-------------------------------------------------------------------------------
! Name: build_preproc_fields.F90
!
! Purpose:
! Grid and average the imager data to the preprocessing grid.
!
! Description and Algorithm details:
! 1) Create regular lat/lon grids for preprocessor array.
! 2) Sum all imager angles within each preprocessor grid cell.
! 3) Divide each grid cell by the number of measurements in it.
!
! Arguments:
! Name               Type In/Out/Both Description
! ------------------------------------------------------------------------------
! preproc_dims       struct both Summary of preprocessing grid definitions
! preproc_geoloc     struct both Summary of preprocessing lat/lon
! preproc_geo        struct both Summary of preprocessing geometry
! imager_geolocation struct both Summary of satellite grid definitions
! imager_angles      struct both Summary of satellite geometry
!
! History:
! 2012/02/24, MJ: produces initial code version.
! 2012/04/19, GT: Bug fix - relazi was being referenced by idim
!    in both dimensions (rather idim for x and jdim for y)
! 2012/07/30, CP: added in solazi
! 2012/08/24, MJ: separated filtering in lw and sw to avoid issues on night side
!    of orbits
! 2012/12/14, CP: changed how y loop was set changed starty to startyi to loop
!    over a granule
! 2013/05/16, MJ: changed filtering of angles wrt fill value: multi-view is
!    accounted for.
! 2014/05/07, AP: Move contents of make_preproc_grid here. Update structures.
! 2015/21/01, OS: bug fix in setting lon_i/lat_i min/max limits
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine build_preproc_fields(preproc_dims, preproc_geoloc, preproc_geo, &
     imager_geolocation, imager_angles)

   use imager_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   type(preproc_dims_t),       intent(inout) :: preproc_dims
   type(preproc_geoloc_t),     intent(inout) :: preproc_geoloc
   type(preproc_geo_t),        intent(inout) :: preproc_geo
   type(imager_geolocation_t), intent(inout) :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles

   integer(kind=lint)         :: i,j,k,lon_i,lat_j
   real(sreal)                :: fac

   ! build the arrays for the regular grid

   ! create grid resolution lat
   fac = 1. / preproc_dims%dellat
   preproc_geoloc%latitude(preproc_dims%min_lat) = &
        (preproc_dims%min_lat-0.5)*fac - real(preproc_dims%lat_offset,sreal)
   do i = preproc_dims%min_lat+1, preproc_dims%max_lat
      preproc_geoloc%latitude(i) = preproc_geoloc%latitude(i-1) + fac
   end do

   ! create grid resolution lon
   fac = 1. / preproc_dims%dellon
   preproc_geoloc%longitude(preproc_dims%min_lon) = &
        (preproc_dims%min_lon-0.5)*fac - real(preproc_dims%lon_offset,sreal)
   do i = preproc_dims%min_lon+1, preproc_dims%max_lon
      preproc_geoloc%longitude(i) = preproc_geoloc%longitude(i-1) + fac
   end do

   ! imager resolution is always higher than preprocessing resolution
   ! =>average imager properties to this coarser resolution grid.

   preproc_dims%counter_sw = 0
   preproc_dims%counter_lw = 0

   ! loop over imager data
   do j = 1, imager_geolocation%ny
      do i = imager_geolocation%startx, imager_geolocation%endx

         ! if geolocation isn't there, do nothing
         if (imager_geolocation%latitude(i,j) .eq. sreal_fill_value .or. &
              imager_geolocation%longitude(i,j) .eq. sreal_fill_value) cycle

         ! find grid cell coordinates into which L1b pixel falls
         lon_i = floor((imager_geolocation%longitude(i,j) + &
              preproc_dims%lon_offset)*preproc_dims%dellon, kind=lint) + 1
         lat_j = floor((imager_geolocation%latitude(i,j) + &
              preproc_dims%lat_offset)*preproc_dims%dellat, kind=lint) + 1

         if (lon_i .lt. preproc_dims%min_lon) lon_i = preproc_dims%min_lon
         if (lat_j .lt. preproc_dims%min_lat) lat_j = preproc_dims%min_lat
         if (lon_i .gt. preproc_dims%max_lon) lon_i = preproc_dims%max_lon
         if (lat_j .gt. preproc_dims%max_lat) lat_j = preproc_dims%max_lat

         do k = 1, imager_angles%nviews
            if (imager_angles%satzen(i,j,k) .ne. sreal_fill_value) then
               preproc_geo%satza(lon_i,lat_j,k) = &
                    preproc_geo%satza(lon_i,lat_j,k) + &
                    imager_angles%satzen(i,j,k)

               ! count the number of L1 pixels which fall in this pixel
               preproc_dims%counter_lw(lon_i,lat_j,k) = &
                    preproc_dims%counter_lw(lon_i,lat_j,k)+1
            end if

            if (imager_angles%solzen(i,j,k) .gt. sreal_fill_value .and. &
                 imager_angles%solazi(i,j,k) .gt. sreal_fill_value .and. &
                 imager_angles%relazi(i,j,k) .gt. sreal_fill_value) then

               preproc_geo%solza(lon_i,lat_j,k) = &
                    preproc_geo%solza(lon_i,lat_j,k)+imager_angles%solzen(i,j,k)
               preproc_geo%satazi(lon_i,lat_j,k) = &
                    preproc_geo%satazi(lon_i,lat_j,k)+imager_angles%satazi(i,j,k)
               preproc_geo%relazi(lon_i,lat_j,k) = &
                    preproc_geo%relazi(lon_i,lat_j,k)+imager_angles%relazi(i,j,k)
               preproc_geo%solazi(lon_i,lat_j,k) = &
                    preproc_geo%solazi(lon_i,lat_j,k)+imager_angles%solazi(i,j,k)

               ! count the number of L1b pixels which fall in this pixel
               preproc_dims%counter_sw(lon_i,lat_j,k) = &
                    preproc_dims%counter_sw(lon_i,lat_j,k)+1
            end if
         end do
      end do
   end do

   ! loop over preprocessor data i.e reduced resolution
   do j = preproc_dims%min_lat, preproc_dims%max_lat
      do i = preproc_dims%min_lon, preproc_dims%max_lon
         do k = 1, imager_angles%nviews
            if (preproc_dims%counter_lw(i,j,k) .gt. 0) then
               ! if this is a good preprocessing pixel, calculate the average
               preproc_geo%satza(i,j,k) = preproc_geo%satza(i,j,k)/ &
                    preproc_dims%counter_lw(i,j,k)
            else
               ! if not set fill value
               preproc_geo%satza(i,j,k) = sreal_fill_value
            end if

            if (preproc_dims%counter_sw(i,j,k) .gt. 0) then
               ! if this is a good preprocessing pixel, calculate the average
               preproc_geo%solza(i,j,k) = preproc_geo%solza(i,j,k)/ &
                    preproc_dims%counter_sw(i,j,k)
               preproc_geo%satazi(i,j,k) = preproc_geo%satazi(i,j,k)/ &
                    preproc_dims%counter_sw(i,j,k)
               preproc_geo%relazi(i,j,k) = preproc_geo%relazi(i,j,k)/ &
                    preproc_dims%counter_sw(i,j,k)
               preproc_geo%solazi(i,j,k) = preproc_geo%solazi(i,j,k)/ &
                    preproc_dims%counter_sw(i,j,k)
            else
               ! if not set fill value
               preproc_geo%solza(i,j,k) = sreal_fill_value
               preproc_geo%satazi(i,j,k) = sreal_fill_value
               preproc_geo%relazi(i,j,k) = sreal_fill_value
               preproc_geo%solazi(i,j,k) = sreal_fill_value
            end if
         end do
      end do
   end do

end subroutine build_preproc_fields
