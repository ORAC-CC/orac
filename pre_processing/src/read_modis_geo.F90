! Name: read_modis_geo.F90
!
!
! Purpose:
! Open and read MODIS geolocation input files.
!
! Description and Algorithm details:
! 1) Allocate and initialise temporary arrays.
! 2) Read each geolocation field in turn.
! 3) Deallocate arrays and close files.
!
! Arguments:
! Name               Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_geo_file   string in   Full path to geolocation data 
! imager_geolocation struct both Summary of pixel positions
! imager_angles      struct both Summary of sun/satellite viewing angles
! imager_flags       struct both Summary of land/sea/ice flags
! imager_time        struct both Summary of pixel observation time
! n_along_track      lint   in   Number of pixels in the direction of travel
!
! History:
! 2011/12/12: MJ produces draft code which opens and reads MODIS geo hdf files
! 2012/04/24: GT Added a line assigning the solar azimuth angle
!                to imager structure (needed by surface reflectance routines) 
! 2013/05/21: GT Removed 180.0 degree correction to relative
!                azimuth to match the convention used in ORAC lookup table code
!                and surface reflectance calculation.
! 2013/05/21: GT On further investigation, reintroduced the 180
!                degree correction to relative azimuth. This means that forward
!                scattering = 0 relative azimuth.
! 2013/09/11: AP tidying, added n_along_track, removed modis2oraclsflag
!
! $Id$
!
! Bugs:
! none known
!

subroutine read_modis_geo(path_to_geo_file,imager_geolocation,imager_angles, &
     imager_flags,imager_time,n_along_track)

   use preproc_constants
   use imager_structures

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   character(len=pathlength)  :: path_to_geo_file

   integer(kind=lint)         :: geo_id,ix,jy,n_along_track

   type(imager_geolocation_s) :: imager_geolocation
   type(imager_angles_s)      :: imager_angles
   type(imager_flags_s)       :: imager_flags
   type(imager_time_s)        :: imager_time

   real(kind=sreal), allocatable, dimension(:,:)    :: temp,temp2

   integer(kind=sint), allocatable, dimension(:,:)  :: btemp

   integer                    :: err_code

   !allocate temporary data
   allocate(temp(imager_geolocation%startx:imager_geolocation%endx,&
        & imager_geolocation%starty:imager_geolocation%endy))

   !get file id
   geo_id=sfstart(path_to_geo_file,DFACC_READ)

   !read latitude
   call read_modis_lat_lon(geo_id,"Latitude",imager_geolocation%startx, &
        imager_geolocation%endx,imager_geolocation%starty, &
        imager_geolocation%endy,temp)
   imager_geolocation%latitude=temp

   !read longitude
   call read_modis_lat_lon(geo_id,"Longitude",imager_geolocation%startx, &
        imager_geolocation%endx,imager_geolocation%starty, &
        imager_geolocation%endy,temp)
   imager_geolocation%longitude=temp

   !read time
   call get_modis_time(geo_id,imager_geolocation,imager_time,n_along_track)

   !read senszen
   call read_modis_angles(geo_id,"SensorZenith",imager_geolocation%startx, &
        imager_geolocation%endx,imager_geolocation%starty, &
        imager_geolocation%endy,temp)
   imager_angles%satzen(:,:,imager_angles%nviews)=temp

   !read solzen
   call read_modis_angles(geo_id,"SolarZenith",imager_geolocation%startx, &
        imager_geolocation%endx,imager_geolocation%starty, &
        imager_geolocation%endy,temp)
   imager_angles%solzen(:,:,imager_angles%nviews)=temp

   !read sensazi
   call read_modis_angles(geo_id,"SensorAzimuth",imager_geolocation%startx, &
        imager_geolocation%endx,imager_geolocation%starty, &
        imager_geolocation%endy,temp)

   !read solazi
   allocate(temp2(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))
   call read_modis_angles(geo_id,"SolarAzimuth",imager_geolocation%startx, &
        imager_geolocation%endx,imager_geolocation%starty, &
        imager_geolocation%endy,temp2)
   imager_angles%solazi(:,:,imager_angles%nviews)=temp2

   ! make rel azi
   ! Note: Relative azimuth is defined so that if the satellite is looking 
   ! towards the sun (i.e. forward scattering), relative azimuth is zero.
   temp2=180.0-temp2
   imager_angles%relazi(:,:,imager_angles%nviews) = 180.0 - &
        acos(cos((temp-temp2)*d2r))/d2r

   !free temp arrays
   deallocate(temp2)
   deallocate(temp)

   !read modis ls flag
   allocate(btemp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))
   call read_modis_lsflag(geo_id,"Land/SeaMask",imager_geolocation%startx, &
        imager_geolocation%endx,imager_geolocation%starty, &
        imager_geolocation%endy,btemp)

!!$ make orac ls flag by mapping the MODIS L/S definitions to the ones for ORAC
!!$ (approximate). MODIS DEFINITIONS - DN values:
!!$                0:      Shallow Ocean (Ocean <5k from coast OR <50m deep).
!!$                1:      Land (not anything else).
!!$                2:      Ocean Coastlines and Lake Shorelines.
!!$                3:      Shallow Inland Water (Inland Water < 5km from shore
!!$                                OR < 50m deep).
!!$                4:      Ephemeral (intermittent) Water.
!!$                5:      Deep Inland Water (Inland water > 5km from shoreline
!!$                                AND > 50m deep).
!!$                6:      Moderate or Continental Ocean (Ocean > 5km from coast
!!$                                AND > 50m deep AND < 500m deep).
!!$                7:      Deep Ocean (Ocean > 500m deep).
   ! which of these is most efficient is compiler-dependant
!   where(btemp.eq.0 .or. btemp.eq.5 .or. btemp.eq.6 .or. btemp.eq.7)
!      btemp = 0
!   elsewhere
!      btemp = 1
!   end where
   do ix=imager_geolocation%startx,imager_geolocation%endx
      do jy=imager_geolocation%starty,imager_geolocation%endy
         if(btemp(ix,jy).eq.0 .or. &
              (btemp(ix,jy).ge.5 .and. btemp(ix,jy).le.7)) then
            btemp(ix,jy) = 0
         else
            btemp(ix,jy) = 1
         endif
      enddo
   enddo

   imager_flags%lsflag=btemp

   !free temp byte array
   deallocate(btemp)

   !end access to geofile
   err_code=sfend(geo_id)

end subroutine read_modis_geo
