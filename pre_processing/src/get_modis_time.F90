! Name: get_modis_time.F90
!
!
! Purpose:
! Read MODIS time data and map to imager pixels
! 
! Description and Algorithm details:
!
!
! Arguments:
! Name               Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! geo_id             lint   in   A file ID returned by SFSTART
! imager_geolocation struct both Summary of pixel positions
! imager_time        struct both Summary of pixel observation time
! n_along_track      lint   in   Number of pixels in the direction of travel
!
! Local variables:
! Name Type Description
!
!
! History:
! 2011/12/16: MJ produces draft code
! 2012/06/13: MJ reworks code.
! 2013/09/11: AP tidying, added n_along_track, removed tfv
!
! $Id$
!
! Bugs:
! none known
!

!---------------------------------------------------
!---------------------------------------------------
subroutine get_modis_time(geo_id,imager_geolocation,imager_time,n_along_track)

   use preproc_constants
   use imager_structures

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   integer(kind=lint)         :: geo_id,ix,jy,startyy,stopyy
   
   integer(kind=lint)         :: var_id

   integer(kind=lint)         :: err_code

   integer(kind=lint)         :: dims(2),dummy_type,dummy_numattrs,dummy_rank, &
        dims_10

   integer(kind=lint)         :: n_along_track,n_along_track_10, &
        along_track_ratio

   character(len=50)          :: dummy_name

   type(imager_geolocation_s) :: imager_geolocation
   type(imager_time_s)        :: imager_time
   
   real(kind=dreal), allocatable, dimension(:)   :: ttemp10
   
   real(kind=dreal), allocatable, dimension(:,:) :: ttemp

   real(kind=dreal)           :: refjulianday=0.00_dreal
   
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


!---------------------------------------------------
!---------------------------------------------------
subroutine get_modis_time_old(geo_id,imager_geolocation,imager_time)
!---------------------------------------------------
!---------------------------------------------------

  use preproc_constants

  use imager_structures

  implicit none

  include "hdf.f90"
  include "dffunc.f90"

  integer(kind=lint) :: geo_id,ix,jy,startyy,stopyy

  integer(kind=lint) :: var_id

  integer(kind=lint) :: err_code, dims(2), dummy_type, dummy_numattrs, dummy_rank,dims_10

  integer(kind=lint) :: n_along_track,n_along_track_10,along_track_ratio

  character(len=50) :: dummy_name

  type(imager_geolocation_s) :: imager_geolocation
  type(imager_time_s) :: imager_time

  real(kind=dreal), allocatable, dimension(:)  :: ttemp10

  real(kind=dreal), allocatable, dimension(:,:)  :: ttemp

  real(kind=dreal) :: tfv

  var_id=sfselect(geo_id,sfn2index(geo_id,"Latitude"))
  err_code=sfginfo(var_id,dummy_name, dummy_rank, dims,dummy_type, dummy_numattrs)
!  write(*,*) err_code
  n_along_track=dims(2)
  err_code=sfendacc(var_id)

!  write(*,*) 'time1'

  var_id=sfselect(geo_id,sfn2index(geo_id,"EV start time"))
  err_code=sfginfo(var_id,dummy_name, dummy_rank, dims_10,dummy_type, dummy_numattrs)
!  write(*,*) err_code
  n_along_track_10=dims_10
  err_code=sfendacc(var_id)

!  write(*,*) 'time2'

  along_track_ratio=n_along_track/n_along_track_10

!  write(*,*) 'ratio',along_track_ratio,n_along_track,n_along_track_10

!  write(*,*) 'time3'
  
!  imager_geolocation%starty=11

  select case(mod(imager_geolocation%starty,along_track_ratio))

  case(0)

     if(imager_geolocation%starty .lt. along_track_ratio) then

        startyy=1

     else

        startyy=int(imager_geolocation%starty/along_track_ratio)

     endif

  case default

  startyy=int(imager_geolocation%starty/(along_track_ratio))+1

  end select



  select case(mod(imager_geolocation%endy,along_track_ratio))

  case(0)

     if(imager_geolocation%endy .lt. along_track_ratio) then

        stopyy=1

     else

        stopyy=int(imager_geolocation%endy/along_track_ratio)

     endif

  case default

     stopyy=int(imager_geolocation%endy/(along_track_ratio))+1

  end select

!  write(*,*) 'startyy,stopyy',imager_geolocation%endy,startyy,stopyy
  

  allocate(ttemp10(startyy:stopyy))
  
! write(*,*) 'hier',geo_id,startyy,stopyy
   call read_modis_time(geo_id, &
       &  "EV start time",startyy,stopyy, ttemp10,tfv)

!  write(*,*) ttemp10,tfv

  allocate(ttemp(imager_geolocation%startx:imager_geolocation%endx,&
       & imager_geolocation%starty:imager_geolocation%endy))

  call map_time_to_pixel(along_track_ratio,&
       & imager_geolocation%startx,imager_geolocation%endx,&
       & imager_geolocation%starty,imager_geolocation%endy,startyy,stopyy,ttemp,ttemp10)

  deallocate(ttemp10)

  imager_time%time=ttemp

!  write(*,*) imager_time%time

  deallocate(ttemp)

end subroutine get_modis_time_old
