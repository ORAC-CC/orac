! Name: get_surface_emissivity.F90
!
! Purpose:
! Loads land surface emissivity data, from the CIMSS global land surface
! IR emissivity data (University of Wisconsin), and uses it to populate both the
! instrument grid (surface structure) and pre-processing grid (preproc_surf)
!
! Description and algorithm details:
!
! Arguments:
! Name            Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! emis_path    char   in   Path to CIMSS emis_inf10_month data
! imager_flags       struct in   Imager structure containing land/sea flag
! imager_geolocation struct in   Imager structure containing lat/lon points
! preproc_dims       struct in   Preprocessing dimensions, including sw and lw
!                                channel counts
! surface            struct both Surface properties structure
! preproc_surf       struct both Preproc surface properties structure
!
! History:
! 30/05/2012 Gareth Thomas. Finished first version
! 31/05/2012 MJ fixes: sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))) / &
!                      real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))))
!               with:  sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))) / &
!                      real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))))
!               to make code compile with sun compiler.
! 26/06/2012 CP updated preproc_dims with channel_info struture variables
! 08/08/2012 CP changed format of emis array to be consistent with msi order
! 08/08/2012 CP fixed bug channel_info was not in arguments
! 08/08/2012 CP changed bilinear to nearestneighbout interpolation, also
!               swicthed lats and lons around in interpolatio subroutine
!               initialised allocate arrays
! 25/02/2013 GT preproc_geoloc arrays are now used directly in averaging the
!               emissivity onto the preproc (rtm) grid. As a consequence,
!               preproc_gelolc added to the argument list
! 06/03/2013 GT Small tidy up of code.
! 15/03/2013 GT Reinstated bilinear interpolation, as new version is nearly as
!               fast as nearest neighbour.
!               Fixed a bug with indexing of surface%emissivity
! 16/10/2013 GM get_surface_emissivity() was using preproc_dims%dellat for the
!               longitude center to edge transformation.  Changed it to use
!               preproc_dims%dellon and reordered the statements a bit.
! 16/10/2013 GM Fixed indexing bug in get_surface_emissivity() when finding
!               index numbers in the emissivity lat-lon grid that correspond to
!               each of the grid cells in the preproc grid.
! 05/11/2013 GM Removed my commented out original fixes for those of Matthias
!               and otherwise fixed a small memory leak and cleaned up the code.
! 10/02/2014 AP Variable renaming
! 21/04/2014 GM Added logical option assume_full_path.
! 20/06/2014 GM Handle case when imager_geolocation%latitude or
!               imager_geolocation%longitude is equal to fill_value.
! 01/07/2014 AP Tidying. Update to new structures.
!
! $Id$
!
! Bugs:
! none known
!

subroutine get_surface_emissivity(cyear, doy, assume_full_path, emis_path, &
     imager_flags, imager_geolocation, channel_info, preproc_dims, &
     preproc_geoloc, surface, preproc_surf)

   use cimss_emissivity
   use preproc_constants
   use preproc_structures
   use channel_structures
   use imager_structures
   use surface_structures
   use interpol

   implicit none

   ! Input/output variables
   character(len=datelength),  intent(in)    :: cyear
   integer(kind=stint),        intent(in)    :: doy
   logical,                    intent(in)    :: assume_full_path
   character(len=pathlength),  intent(in)    :: emis_path
   type(imager_flags_s),       intent(in)    :: imager_flags
   type(imager_geolocation_s), intent(in)    :: imager_geolocation
   type(channel_info_s),       intent(in)    :: channel_info
   type(preproc_dims_s),       intent(in)    :: preproc_dims
   type(preproc_geoloc_s),     intent(in)    :: preproc_geoloc
   type(surface_s),            intent(inout) :: surface
   type(preproc_surf_s),       intent(inout) :: preproc_surf

   ! Local variables
   character(len=pathlength)                          :: emis_path_file
   type(emis_s)                                       :: emis
   integer(kind=stint),              dimension(3)     :: embands=[1,8,9]
   real(kind=sreal),    allocatable, dimension(:,:,:) :: transemis, summat
   real(kind=sreal),    allocatable, dimension(:,:)   :: counter
   integer(kind=lint)                                 :: i,j,k,lat,lon
   integer(kind=lint)                                 :: nland
   real,                             dimension(1)     :: temp_lat, temp_lon
   real,                             dimension(1)     :: temp_sur

   ! Count the number of land and sea pixels, using the imager land/sea mask
   nland = count(imager_flags%lsflag .eq. 1)
   write(*,*)'nland: ',nland

   ! If there are no land pixels in the scene, we have nothing more to do
   if (nland .eq. 0) return

   ! embands is modis numbering of IR channels where,
   ! channel_info%channel_ids_abs .gt. 3
   write(*,*)'channel_info%channel_ids_abs: ',channel_info%channel_ids_abs
   write(*,*)'embands: ',embands

   ! Select correct modis file
   if (assume_full_path) then
      emis_path_file = emis_path
   else
      call select_modis_emiss_file(cyear,doy,emis_path,emis_path_file)
   endif
   write(*,*)'emis_path_file: ',trim(emis_path_file)

   ! Read the data itself
   if (read_cimss_emissivity(emis_path_file, emis, embands) .ne. 0) &
        STOP 'GET_SURFACE_EMISSIVITY: Bad read of cimss file.'

   ! This emissivity data has very few missing values, but there are some. Set
   ! these to 0.999 (as close to 1 as the emissitivty data itself gets). This is
   ! also a vaguely reasonable value for the water as well....
   allocate(transemis(emis%nbands,emis%nlon,emis%nlat))
   do i=1,emis%nbands
      transemis(i,:,:) = transpose(emis%emissivity(i,:,:))
   end do
   where(transemis .le. 0.0) transemis = 0.999

   ! Now interpolate this data onto the data grid
   do j=1,imager_geolocation%ny
      do i=imager_geolocation%startx,imager_geolocation%endx
         if (imager_flags%lsflag(i,j) .eq. 1 .and. &
              imager_geolocation%latitude(i,j) .ne. real_fill_value .and. &
              imager_geolocation%longitude(i,j) .ne. real_fill_value) then
            temp_lat=imager_geolocation%latitude(i,j)
            temp_lon=imager_geolocation%longitude(i,j)
            do k=1,channel_info%nchannels_lw
               ! Can use either bilinear or nearest neighbour...
               !call interpol_nearest_neighbour(emis%lon, emis%lat, &
               !     transemis(i,:,:), temp_lat, temp_lon, temp_sur)
               call interpol_bilinear(emis%lon, emis%lat, transemis(k,:,:), &
                    temp_lon, temp_lat, temp_sur, real_fill_value)
               
               surface%emissivity(i,j,k)=temp_sur(1)
            end do
         end if
      end do
   end do

   ! calculate the mean emissivity in each preproc grid
   allocate(counter(preproc_dims%min_lon:preproc_dims%max_lon, &
        preproc_dims%min_lat:preproc_dims%max_lat))
   allocate(summat(preproc_dims%min_lon:preproc_dims%max_lon, &
        preproc_dims%min_lat:preproc_dims%max_lat, channel_info%nchannels_lw))
   counter=0
   summat=0.
   do j=1,emis%nlat
      lat=floor((emis%lat(j)+preproc_dims%lat_offset)*preproc_dims%dellat)+1
      if (lat.ge.preproc_dims%min_lat .and. lat.le.preproc_dims%max_lat) then
         do i=1,emis%nlon
            lon=floor((emis%lon(i)+preproc_dims%lon_offset)* &
                 preproc_dims%dellon)+1
            if (lon.ge.preproc_dims%min_lon .and. &
                 lon.le.preproc_dims%max_lon) then
               summat(lon,lat,:)=summat(lon,lat,:)+transemis(:,i,j)
               counter(lon,lat) =counter(lon,lat)+1
            end if
         end do
      end if
   end do

   do j=preproc_dims%min_lat,preproc_dims%max_lat
      do i=preproc_dims%min_lon,preproc_dims%max_lon
         if (counter(i,j) .gt. 0) &
            preproc_surf%emissivity(i,j,:) = summat(i,j,:) / real(counter(i,j))
      end do
   end do

   deallocate(transemis)
   deallocate(counter)
   deallocate(summat)
   call deallocate_emis(emis)

end subroutine get_surface_emissivity
