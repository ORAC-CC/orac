!-------------------------------------------------------------------------------
! Name: get_modis_time.F90
!
! Purpose:
! Read MODIS time data and map to imager pixels
!
! Description and Algorithm details:
! 1) Allocate an array for the time.
! 2) Read time from MODIS.
! 3) Convert that to a format suitable for ORAC.
!
! Arguments:
! Name               Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! geo_id             lint   in   A file ID returned by SFSTART
! imager_geolocation struct both Summary of pixel positions
! imager_time        struct both Summary of pixel observation time
! n_along_track      lint   in   Number of pixels in the direction of travel
!
! History:
! 2011/12/16, MJ: produces draft code
! 2012/06/13, MJ: reworks code.
! 2013/09/11, AP: tidying, added n_along_track, removed tfv
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine get_modis_time(geo_id,imager_geolocation,imager_time,n_along_track)

   use calender
   use imager_structures
   use preproc_constants

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   integer(kind=lint),         intent(in)    :: geo_id
   type(imager_geolocation_s), intent(inout) :: imager_geolocation
   type(imager_time_s),        intent(in)    :: imager_time
   integer(kind=lint),         intent(in)    :: n_along_track

   integer(kind=lint)         :: var_id
   integer(kind=lint)         :: err_code
   integer(kind=lint)         :: dummy_type,dummy_numattrs,dummy_rank,dims_10
   integer(kind=lint)         :: n_along_track_10, along_track_ratio
   character(len=50)          :: dummy_name

   real(kind=dreal), allocatable, dimension(:)   :: ttemp10
   real(kind=dreal), allocatable, dimension(:,:) :: ttemp
   real(kind=dreal)           :: refjulianday=0.00_dreal
   integer(kind=lint)         :: startyy,stopyy

   !reference point of time
   integer(kind=stint) :: refday=1_stint,refyear=1993_stint,refmonth=1_stint

   ! determine size of grid on which time is stored
   var_id=sfselect(geo_id,sfn2index(geo_id,"EV start time"))
   err_code=sfginfo(var_id,dummy_name,dummy_rank,dims_10,dummy_type, &
        dummy_numattrs)
   n_along_track_10=dims_10
   err_code=sfendacc(var_id)

   along_track_ratio=n_along_track/n_along_track_10


   ! find the 10-line block in which the start and stop pixel lies
   ! 1 => lines 1-10, 2 => lines 11-20, ...
   startyy = int((imager_geolocation%starty-1)/along_track_ratio)+1
   stopyy = int((imager_geolocation%endy-1)/along_track_ratio)+1

   !allocate array which holds EV
   allocate(ttemp10(startyy:stopyy))

   !read the EarthView (EV) start time of the scan
   call read_modis_time(geo_id, "EV start time", startyy, stopyy, ttemp10)

   !allocate array which holds final time information for each pixel
   allocate(ttemp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))

   !this converts the reference point of time (epoch) to the julian date
   call GREG2JD(refyear,refmonth,refday,refjulianday)

   call map_time_to_pixel(along_track_ratio, &
        imager_geolocation%startx,imager_geolocation%endx, &
        imager_geolocation%starty,imager_geolocation%endy,startyy,stopyy, &
        ttemp,ttemp10,refjulianday)

   deallocate(ttemp10)

   imager_time%time=ttemp

   deallocate(ttemp)

end subroutine get_modis_time
