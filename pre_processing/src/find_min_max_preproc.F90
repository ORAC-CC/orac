! Name: find_min_max_preproc.F90
!
!
! Purpose:
! Find min/max indices of preproc grid cells. Use this information to speed up
! assigning of ECMWF values.
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/02/27: MJ produces initial code version.
! 2012/12/14: CP changed how loop was set changed starty to startyi to loop over
!                a granule
! 2013/01/03: CP changed int-->nint
! 2013/01/31: CP used floor and ceiling to get better boundaries, not very
!                efficient but it works.
! 2013/01/31: CP changed function to read in ecmwf_dims
! 2013/10/30: AP Replaced function. Now uses array operations rather than a
!                loop, uses preprop_dims rather than ecmwf (in case you ever
!                define a different grid), and properly wraps the longitude
!                (rather than only wrapping large longitudes around).
!
! $Id$
!
! Bugs:
! none known
!

subroutine find_min_max_preproc(preproc_dims,imager_geolocation,verbose)

   use preproc_constants
   use imager_structures
   use preproc_structures

   implicit none

   type(preproc_dims_s)       :: preproc_dims
   type(imager_geolocation_s) :: imager_geolocation
   logical                    :: verbose

   real(sreal), dimension(imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny) :: lat, lon
   logical, dimension(imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny) :: mask


   ! determine which preproc grid points each pixels falls in (start at 1)
   lat = (imager_geolocation%latitude + preproc_dims%lat_offset)* &
        preproc_dims%dellat + 1.
   lon = (imager_geolocation%longitude + preproc_dims%lon_offset)* &
        preproc_dims%dellon + 1.

   ! only consider valid lat/lon values
   mask =  imager_geolocation%latitude.ge.(real_fill_value+1.0) .and. &
        imager_geolocation%longitude.ge.(real_fill_value+1.0)

   ! take one more pixel than is required for interpolation
   preproc_dims%preproc_max_lat = maxval(ceiling(lat), mask) + 1
   if (preproc_dims%preproc_max_lat .ge. preproc_dims%ydim_pre) &
      preproc_dims%preproc_max_lat = preproc_dims%ydim_pre
   
   preproc_dims%preproc_min_lat = minval(floor(lat), mask) - 1
   if (preproc_dims%preproc_min_lat .lt. 1) preproc_dims%preproc_min_lat = 1

   ! if longitude touches the grid edges, wrap around and use the whole grid
   preproc_dims%preproc_max_lon = maxval(ceiling(lon), mask) + 1
   preproc_dims%preproc_min_lon = minval(floor(lon), mask) - 1
   if (preproc_dims%preproc_max_lon.ge.preproc_dims%xdim_pre .or. &
        preproc_dims%preproc_min_lon.le.1) then
      preproc_dims%preproc_max_lon = preproc_dims%xdim_pre
      preproc_dims%preproc_min_lon = 1
   end if

   if (verbose) then
      print*, 'preproc_dims%preproc_min_lat: ', preproc_dims%preproc_min_lat
      print*, 'preproc_dims%preproc_max_lat: ', preproc_dims%preproc_max_lat
      print*, 'preproc_dims%preproc_min_lon: ', preproc_dims%preproc_min_lon
      print*, 'preproc_dims%preproc_max_lon: ', preproc_dims%preproc_max_lon
   end if
   

end subroutine find_min_max_preproc
 
