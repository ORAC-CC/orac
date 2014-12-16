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
! 19/08/2014, AP: Comment out reading of unused emissivity fields. They will be
!   used eventually; hence why the code remains.
! 15/10/2014, GM: Changes related to supporting an arbitrary set of LW channels.
! 1/12/2014,  CP: Added source attributes.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define ONLY_GET_MIXED_CHANNELS

subroutine get_surface_emissivity(cyear, cdoy, cimss_emis_path, imager_flags, &
           imager_geolocation, channel_info, preproc_dims, preproc_geoloc, &
           assume_full_path, verbose, surface, preproc_surf, source_atts)

   use channel_structures
   use cimss_emissivity
   use imager_structures
   use interpol
   use preproc_constants
   use preproc_structures
   use source_attributes
   use surface_structures

   implicit none

   ! Input/output variables
   character(len=date_length),  intent(in)    :: cyear
   character(len=date_length),  intent(in)    :: cdoy
   character(len=path_length),  intent(in)    :: cimss_emis_path
   type(imager_flags_s),        intent(in)    :: imager_flags
   type(imager_geolocation_s),  intent(in)    :: imager_geolocation
   type(channel_info_s),        intent(in)    :: channel_info
   type(preproc_dims_s),        intent(in)    :: preproc_dims
   type(preproc_geoloc_s),      intent(in)    :: preproc_geoloc
   logical,                     intent(in)    :: assume_full_path
   logical,                     intent(in)    :: verbose
   type(surface_s),             intent(inout) :: surface
   type(preproc_surf_s),        intent(inout) :: preproc_surf
    type(source_attributes_s),  intent(inout) :: source_atts
   ! Local variables
   character(len=path_length)                         :: cimss_emis_path_file
   type(emis_s)                                       :: emis
   integer                                            :: i_lw_chans
   integer                                            :: n_mixed_chans
   real(kind=sreal),    allocatable, dimension(:,:,:) :: transemis, summat
   real(kind=sreal),    allocatable, dimension(:,:)   :: counter
   integer                                            :: i,j,k,lat,lon
   integer                                            :: nland
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
   if (nland == 0 .or. channel_info%nchannels_lw == 0) return

   i_lw_chans = channel_info%nchannels_total - channel_info%nchannels_lw + 1

   ! embands is modis numbering of IR channels where,
   ! channel_info%channel_lw_flag .ne. 0
   if (verbose) write(*,*) 'n channels for land emissivity: ', &
                           channel_info%nchannels_lw

   if (verbose) write(*,*) 'intrument bands for land emissivity: ', &
                           channel_info%channel_ids_instr(i_lw_chans:)

   ! Select correct modis file
   if (assume_full_path) then
      cimss_emis_path_file = cimss_emis_path
   else
      call select_modis_emiss_file(cyear,cdoy,cimss_emis_path,cimss_emis_path_file)
   end if
   if (verbose) write(*,*) 'cimss_emis_path_file: ', trim(cimss_emis_path_file)

   source_atts%emissivity_file=trim(cimss_emis_path_file)

   ! Read the data itself
#ifdef ONLY_GET_MIXED_CHANNELS
   if (read_cimss_emissivity(cimss_emis_path_file, emis, &
       channel_info%channel_wl_abs(i_lw_chans:channel_info%nchannels_sw), &
       verbose) .ne. 0) then
#else
   if (read_cimss_emissivity(cimss_emis_path_file, emis, &
       channel_info%channel_wl_abs(i_lw_chans:channel_info%nchannels_total), &
       verbose) .ne. 0) then
#endif
        write(*,*) 'ERROR: read_cimss_emissivity(), problem reading CIMSS ' // &
                   'emissivity file: ', cimss_emis_path_file
        stop error_stop_code
   end if

   ! This emissivity data has very few missing values, but there are some. Set
   ! these to 0.999 (as close to 1 as the emissivity data itself gets). This is
   ! also a vaguely reasonable value for the water as well....

   n_mixed_chans = channel_info%nchannels_sw - i_lw_chans + 1

   allocate(transemis(emis%nlon,emis%nlat,emis%nbands))
   do i=1,emis%nbands
      transemis(:,:,i) = transpose(emis%emissivity(:,:,i))
   end do
   where(transemis .le. 0.0) transemis = 0.999

   ! Now interpolate this data onto the data grid
   do j=1,imager_geolocation%ny
      do i=imager_geolocation%startx,imager_geolocation%endx
         if (imager_flags%lsflag(i,j) .eq. 1 .and. &
              imager_geolocation%latitude(i,j) .ne. sreal_fill_value .and. &
              imager_geolocation%longitude(i,j) .ne. sreal_fill_value) then
            call bilinear_coef(emis%lon0, emis%lon_invdel, emis%nlon, &
                 emis%lat0, emis%lat_invdel, emis%nlat, &
                 imager_geolocation%longitude(i,j), &
                 imager_geolocation%latitude(i,j), interp)
#ifdef ONLY_GET_MIXED_CHANNELS
             do k=1,n_mixed_chans
#else
             do k=1,channel_info%nchannels_lw
#endif
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
#ifdef ONLY_GET_MIXED_CHANNELS
        preproc_dims%min_lat:preproc_dims%max_lat, n_mixed_chans))
#else
        preproc_dims%min_lat:preproc_dims%max_lat, channel_info%nchannels_lw))
#endif
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
         if (counter(i,j) .gt. 0) then
#ifdef ONLY_GET_MIXED_CHANNELS
            preproc_surf%emissivity(i,j,1:n_mixed_chans) = summat(i,j,:) / &
               real(counter(i,j))
#else
            preproc_surf%emissivity(i,j, :             ) = summat(i,j,:) / &
               real(counter(i,j))
#endif
         endif
      end do
   end do

   deallocate(transemis)
   deallocate(counter)
   deallocate(summat)
   call deallocate_emis(emis)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_surface_emissivity()'

end subroutine get_surface_emissivity
