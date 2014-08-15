!-------------------------------------------------------------------------------
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
! Name               Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! cyear              string  in   Year, as a 4 character string
! cdoy               string  in   DOY,  as a 3 character string
! assume_full_path
!                    logic   in   T: inputs are filenames; F: folder names
! cimss_emis_path    char    in   Path to CIMSS emis_inf10_month data
! imager_flags       struct  in   Imager structure containing land/sea flag
! imager_geolocation struct  in   Imager structure containing lat/lon points
! channel_info    struct  in      Preprocessing dimensions, including sw and lw
!                                 channel counts
! preproc_dims       struct  in   Preprocessing dimensions, including sw and lw
!                                 channel counts
! assume_full_path   logic   in   T: inputs are filenames; F: folder names
! verbose            logic   in   T: print status information; F: don't
! surface            struct  both Surface properties structure
! preproc_surf       struct  both Preproc surface properties structure
!
! History:
! 30/05/2012, GT: Finished first version
! 31/05/2012, MJ: fixes
!   sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))) / &
!   real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))))
!   with:
!   sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))) / &
!   real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))))
!   to make code compile with sun compiler.
! 26/06/2012, CP: updated preproc_dims with channel_info structure variables
! 08/08/2012, CP: changed format of emis array to be consistent with msi order
! 08/08/2012, CP: fixed bug channel_info was not in arguments
! 08/08/2012, CP: changed bilinear to nearest neighbour interpolation, also
!   switched lats and lons around in interpolation subroutine initialised
!   allocate arrays
! 25/02/2013, GT: preproc_geoloc arrays are now used directly in averaging the
!   emissivity onto the preproc (rtm) grid. As a consequence, preproc_gelolc
!   added to the argument list
! 06/03/2013, GT: Small tidy up of code.
! 15/03/2013, GT: Reinstated bilinear interpolation, as new version is nearly as
!   fast as nearest neighbour. Fixed a bug with indexing of surface%emissivity
! 16/10/2013, GM: get_surface_emissivity() was using preproc_dims%dellat for the
!   longitude center to edge transformation.  Changed it to use
!   preproc_dims%dellon and reordered the statements a bit.
! 16/10/2013, GM: Fixed indexing bug in get_surface_emissivity() when finding
!   index numbers in the emissivity lat-lon grid that correspond to each of the
!   grid cells in the preproc grid.
! 05/11/2013, GM: Removed my commented out original fixes for those of Matthias
!   and otherwise fixed a small memory leak and cleaned up the code.
! 10/02/2014, AP: Variable renaming
! 21/04/2014, GM: Added logical option assume_full_path.
! 20/06/2014, GM: Handle case when imager_geolocation%latitude or
!   imager_geolocation%longitude is equal to fill_value.
! 01/07/2014, AP: Tidying. Update to new structures.
! 05/08/2014, AP: New bilinear interpolation routine. Changed how grid is
!   described from emissivity file.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine get_surface_emissivity(cyear, cdoy, cimss_emis_path, imager_flags, &
           imager_geolocation, channel_info, preproc_dims, preproc_geoloc, &
           assume_full_path, verbose, surface, preproc_surf)

   use channel_structures
   use cimss_emissivity
   use imager_structures
   use interpol
   use preproc_constants
   use preproc_structures
   use surface_structures

   implicit none

   ! Input/output variables
   character(len=datelength),  intent(in)    :: cyear
   character(len=datelength),  intent(in)    :: cdoy
   character(len=pathlength),  intent(in)    :: cimss_emis_path
   type(imager_flags_s),       intent(in)    :: imager_flags
   type(imager_geolocation_s), intent(in)    :: imager_geolocation
   type(channel_info_s),       intent(in)    :: channel_info
   type(preproc_dims_s),       intent(in)    :: preproc_dims
   type(preproc_geoloc_s),     intent(in)    :: preproc_geoloc
   logical,                    intent(in)    :: assume_full_path
   logical,                    intent(in)    :: verbose
   type(surface_s),            intent(inout) :: surface
   type(preproc_surf_s),       intent(inout) :: preproc_surf

   ! Local variables
   character(len=pathlength)                          :: cimss_emis_path_file
   type(emis_s)                                       :: emis
   integer(kind=sint),              dimension(3)      :: embands=[1,8,9]
   real(kind=sreal),    allocatable, dimension(:,:,:) :: transemis, summat
   real(kind=sreal),    allocatable, dimension(:,:)   :: counter
   integer(kind=lint)                                 :: i,j,k,lat,lon
   integer(kind=lint)                                 :: nland
   type(interpol_s)                                   :: interp

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_surface_emissivity()'

   if (verbose) write(*,*) 'cyear: ',            trim(cyear)
   if (verbose) write(*,*) 'cdoy: ',             trim(cdoy)
   if (verbose) write(*,*) 'cimss emis_path: ',  trim(cimss_emis_path)
   if (verbose) write(*,*) 'assume_full_path: ', assume_full_path

   ! Count the number of land and sea pixels, using the imager land/sea mask
   nland = count(imager_flags%lsflag .eq. 1)
   if (verbose) write(*,*) 'nland: ', nland

   ! If there are no land pixels in the scene, we have nothing more to do
   if (nland == 0 .or. count(channel_info%channel_ids_abs .eq. 4) == 0) return

   ! embands is modis numbering of IR channels where,
   ! channel_info%channel_ids_abs .gt. 3
   if (verbose) write(*,*) 'n channels for land reflectance: ', &
                           channel_info%channel_ids_abs
   if (verbose) write(*,*) 'modis numbering for these IR channels: ', &
                           embands

   ! Select correct modis file
   if (assume_full_path) then
      cimss_emis_path_file = cimss_emis_path
   else
      call select_modis_emiss_file(cyear,cdoy,cimss_emis_path,cimss_emis_path_file)
   end if
   if (verbose) write(*,*) 'cimss_emis_path_file: ', trim(cimss_emis_path_file)

   ! Read the data itself
   if (read_cimss_emissivity(cimss_emis_path_file, emis, embands, verbose) .ne. 0) then
        write(*,*) 'ERROR: read_cimss_emissivity(), problem reading CIMSS emissivity file: ', &
        cimss_emis_path_file
        stop error_stop_code
   end if

   ! This emissivity data has very few missing values, but there are some. Set
   ! these to 0.999 (as close to 1 as the emissivity data itself gets). This is
   ! also a vaguely reasonable value for the water as well....
   allocate(transemis(emis%nlon,emis%nlat,emis%nbands))
   do i=1,emis%nbands
      transemis(:,:,i) = transpose(emis%emissivity(:,:,i))
   end do
   where(transemis .le. 0.0) transemis = 0.999

   ! Now interpolate this data onto the data grid
   do j=1,imager_geolocation%ny
      do i=imager_geolocation%startx,imager_geolocation%endx
         if (imager_flags%lsflag(i,j) .eq. 1 .and. &
              imager_geolocation%latitude(i,j) .ne. real_fill_value .and. &
              imager_geolocation%longitude(i,j) .ne. real_fill_value) then
            call bilinear_coef(emis%lon0, emis%lon_invdel, emis%nlon, &
                 emis%lat0, emis%lat_invdel, emis%nlat, &
                 imager_geolocation%longitude(i,j), &
                 imager_geolocation%latitude(i,j), interp)

            do k=1,channel_info%nchannels_lw
               call interp_field(transemis(:,:,k), surface%emissivity(i,j,k), &
                    interp)
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
      lat=floor((emis%lat0+(j-1)*emis%lat_del+preproc_dims%lat_offset)* &
           preproc_dims%dellat)+1
      if (lat.ge.preproc_dims%min_lat .and. lat.le.preproc_dims%max_lat) then
         do i=1,emis%nlon
            lon=floor((emis%lon0+(i-1)*emis%lon_del+preproc_dims%lon_offset)* &
                 preproc_dims%dellon)+1
            if (lon.ge.preproc_dims%min_lon .and. &
                 lon.le.preproc_dims%max_lon) then
               summat(lon,lat,:)=summat(lon,lat,:)+transemis(i,j,:)
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

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_surface_emissivity()'

end subroutine get_surface_emissivity
