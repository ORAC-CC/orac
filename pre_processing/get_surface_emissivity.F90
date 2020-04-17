!-------------------------------------------------------------------------------
! Name: get_surface_emissivity.F90
!
! Purpose:
! Loads land surface emissivity data, from the CIMSS or CAMEL global land surface
! IR emissivity data (University of Wisconsin), and uses it to populate both the
! instrument grid (surface structure) and pre-processing grid (preproc_surf)
!
! Description and Algorithm details:
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
! 2012/05/30, GT: Finished first version
! 2012/05/31, MJ: fixes
!    sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))) / &
!    real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))))
!    with:
!    sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))) / &
!    real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))))
!    to make code compile with sun compiler.
! 2012/06/26, CP: updated preproc_dims with channel_info structure variables
! 2012/08/08, CP: changed format of emis array to be consistent with msi order
! 2012/08/08, CP: fixed bug channel_info was not in arguments
! 2012/08/08, CP: changed bilinear to nearest neighbour interpolation, also
!    switched lats and lons around in interpolation subroutine initialised
!    allocate arrays
! 2012/02/25, GT: preproc_geoloc arrays are now used directly in averaging the
!    emissivity onto the preproc (rtm) grid. As a consequence, preproc_gelolc
!    added to the argument list
! 2013/03/06, GT: Small tidy up of code.
! 2013/03/15, GT: Reinstated bilinear interpolation, as new version is nearly as
!    fast as nearest neighbour. Fixed a bug with indexing of surface%emissivity
! 2013/10/16, GM: get_surface_emissivity() was using preproc_dims%dellat for the
!    longitude center to edge transformation.  Changed it to use
!    preproc_dims%dellon and reordered the statements a bit.
! 2013/10/16, GM: Fixed indexing bug in get_surface_emissivity() when finding
!    index numbers in the emissivity lat-lon grid that correspond to each of the
!    grid cells in the preproc grid.
! 2013/11/05, GM: Removed my commented out original fixes for those of Matthias
!    and otherwise fixed a small memory leak and cleaned up the code.
! 2014/02/10, AP: Variable renaming
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/06/20, GM: Handle case when imager_geolocation%latitude or
!    imager_geolocation%longitude is equal to fill_value.
! 2014/07/01, AP: Tidying. Update to new structures.
! 2014/08/05, AP: New bilinear interpolation routine. Changed how grid is
!    described from emissivity file.
! 2014/08/19, AP: Comment out reading of unused emissivity fields. They will be
!    used eventually; hence why the code remains.
! 2014/10/15, GM: Changes related to supporting an arbitrary set of LW channels.
! 2014/12/01, CP: Added source attributes.
! 2015/01/13, AP: Alter channel indexing to allow channels in arbitrary order.
! 2015/10/19, GM: Turn back on reading of unused emissivity fields which are now
!    optionally required.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine get_surface_emissivity(cyear, cdoy, cimss_emis_path, imager_flags, &
           imager_geolocation, channel_info, preproc_dims, &
           assume_full_path, verbose, surface, preproc_surf, source_atts)

   use channel_structures_m
   use cimss_emissivity_m
   use imager_structures_m
   use interpol_m
   use preproc_constants_m
   use preproc_structures_m
   use source_attributes_m
   use surface_structures_m

   implicit none

   ! Input/output variables
   character(len=*),           intent(in)    :: cyear
   character(len=*),           intent(in)    :: cdoy
   character(len=*),           intent(in)    :: cimss_emis_path
   type(imager_flags_t),       intent(in)    :: imager_flags
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(channel_info_t),       intent(in)    :: channel_info
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   logical,                    intent(in)    :: assume_full_path
   logical,                    intent(in)    :: verbose
   type(surface_t),            intent(inout) :: surface
   type(preproc_surf_t),       intent(inout) :: preproc_surf
   type(source_attributes_t),  intent(inout) :: source_atts

   ! Local variables
   character(len=path_length)                         :: cimss_emis_path_file
   type(emis_t)                                       :: emis
   integer                                            :: n_chans
   integer,             allocatable, dimension(:)     :: ch_total_index
   integer,             allocatable, dimension(:)     :: ch_lw_index
   real(kind=sreal),    allocatable, dimension(:,:,:) :: transemis, summat
   real(kind=sreal),    allocatable, dimension(:,:)   :: counter
   integer                                            :: i, j, k, lat, lon
   integer                                            :: nland
   type(interpol_t)                                   :: interp

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_surface_emissivity()'

   if (verbose) write(*,*) 'cyear: ',            trim(cyear)
   if (verbose) write(*,*) 'cdoy: ',             trim(cdoy)
   if (verbose) write(*,*) 'cimss emis_path: ',  trim(cimss_emis_path)
   if (verbose) write(*,*) 'assume_full_path: ', assume_full_path


   ! Count the number of land and sea pixels, using the imager land/sea mask
   nland = count(imager_flags%lsflag .eq. 1)
   if (verbose) write(*,*) 'nland: ', nland

   source_atts%emissivity_file = 'Not used (no LW channels or no pixels of land)'

   ! If there are no lw channels or no land pixels in the scene, we have nothing
   ! more to do.
   if (channel_info%nchannels_lw == 0 .or. nland == 0) return

   n_chans = channel_info%nchannels_lw

   allocate(ch_total_index(n_chans))
   allocate(ch_lw_index(n_chans))
   j = 1
   k = 1
   do i = 1, channel_info%nchannels_total
      if (channel_info%channel_lw_flag(i) == 1) then
         if (.true.) then
            ! Indices of desired channels wrt all channels
            ch_total_index(j) = i
            ! Indices of desired channels wrt longwave channels
            ch_lw_index(j)    = k
            j = j + 1
         end if
         k = k + 1
      end if
   end do

   if (verbose) write(*,*) 'n channels for land emissivity: ', &
                            channel_info%nchannels_lw
#ifndef __INTEL_COMPILER
   if (verbose) write(*,*) 'instrument bands for land emissivity: ', &
                            channel_info%channel_ids_instr(ch_total_index)
   if (verbose) write(*,*) 'instument wavelengths for land emissivity: ', &
                            channel_info%channel_wl_abs(ch_total_index)
#endif

   ! Select correct modis file
   if (assume_full_path) then
      cimss_emis_path_file = cimss_emis_path
   else
      call select_modis_emiss_file(cyear, cdoy, cimss_emis_path, &
           cimss_emis_path_file)
   end if
   if (verbose) write(*,*) 'cimss_emis_path_file: ', trim(cimss_emis_path_file)

   source_atts%emissivity_file = trim(cimss_emis_path_file)

   ! Read the data itself
   if (read_cimss_emissivity(cimss_emis_path_file, emis, &
       channel_info%channel_wl_abs(ch_total_index), verbose) .ne. 0) then
        write(*,*) 'ERROR: read_cimss_emissivity(), problem reading CIMSS ' // &
                   'emissivity file: ', cimss_emis_path_file
        stop error_stop_code
   end if

   ! This emissivity data has very few missing values, but there are some. Set
   ! these to 0.999 (as close to 1 as the emissivity data itself gets). This is
   ! also a vaguely reasonable value for the water as well....

   allocate(transemis(emis%nlon,emis%nlat,emis%nbands))
   do i = 1, emis%nbands
      transemis(:,:,i) = transpose(emis%emissivity(:,:,i))
   end do
   where(transemis .le. 0.0) transemis = 0.999

   ! Now interpolate this data onto the data grid
   do j = 1, imager_geolocation%ny
      do i = imager_geolocation%startx, imager_geolocation%endx
         if (imager_flags%lsflag(i,j) .eq. 1 .and. &
              imager_geolocation%latitude(i,j) .ne. sreal_fill_value .and. &
              imager_geolocation%longitude(i,j) .ne. sreal_fill_value) then
            call bilinear_coef(emis%lon0, emis%lon_invdel, emis%nlon, &
                 emis%lat0, emis%lat_invdel, emis%nlat, &
                 imager_geolocation%longitude(i,j), &
                 imager_geolocation%latitude(i,j), interp)
            do k = 1, n_chans
               call interp_field(transemis(:,:,k), &
                   surface%emissivity(i,j,ch_lw_index(k)), interp)
            end do
         end if
      end do
   end do

   ! calculate the mean emissivity in each preproc grid
   allocate(counter(preproc_dims%min_lon:preproc_dims%max_lon, &
        preproc_dims%min_lat:preproc_dims%max_lat))
   allocate(summat(preproc_dims%min_lon:preproc_dims%max_lon, &
        preproc_dims%min_lat:preproc_dims%max_lat, n_chans))

   counter = 0
   summat  = 0.
   do j = 1, emis%nlat
      lat = floor((emis%lat0+(j-1)*emis%lat_del+preproc_dims%lat_offset)* &
            preproc_dims%dellat)+1
      if (lat.ge.preproc_dims%min_lat .and. lat.le.preproc_dims%max_lat) then
         do i = 1, emis%nlon
            lon = floor((emis%lon0+(i-1)*emis%lon_del+preproc_dims%lon_offset)* &
                  preproc_dims%dellon)+1
            if (lon.ge.preproc_dims%min_lon .and. &
                 lon.le.preproc_dims%max_lon) then
               summat(lon,lat,:) = summat(lon,lat,:)+transemis(i,j,:)
               counter(lon,lat)  = counter(lon,lat)+1
            end if
         end do
      end if
   end do

   do j = preproc_dims%min_lat, preproc_dims%max_lat
      do i = preproc_dims%min_lon, preproc_dims%max_lon
         if (counter(i,j) .gt. 0) then
            preproc_surf%emissivity(i,j,ch_lw_index) = summat(i,j,:) / &
               real(counter(i,j))
         end if
      end do
   end do

   deallocate(ch_total_index)
   deallocate(ch_lw_index)
   deallocate(transemis)
   deallocate(counter)
   deallocate(summat)
   call deallocate_emis(emis)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_surface_emissivity()'

end subroutine get_surface_emissivity


!-------------------------------------------------------------------------------
! This routine is similar to above, but uses the CAMEL database rather than CIMSS
!-------------------------------------------------------------------------------

subroutine get_camel_emissivity(cyear, cmonth, camel_emis_path, imager_flags, &
           imager_geolocation, channel_info, preproc_dims, &
           assume_full_path, verbose, surface, preproc_surf, source_atts)

   use channel_structures_m
   use camel_emissivity_m
   use imager_structures_m
   use interpol_m
   use preproc_constants_m
   use preproc_structures_m
   use source_attributes_m
   use surface_structures_m

   implicit none

   ! Input/output variables
   character(len=*),           intent(in)    :: cyear
   character(len=*),           intent(in)    :: cmonth
   character(len=*),           intent(in)    :: camel_emis_path
   type(imager_flags_t),       intent(in)    :: imager_flags
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(channel_info_t),       intent(in)    :: channel_info
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   logical,                    intent(in)    :: assume_full_path
   logical,                    intent(in)    :: verbose
   type(surface_t),            intent(inout) :: surface
   type(preproc_surf_t),       intent(inout) :: preproc_surf
   type(source_attributes_t),  intent(inout) :: source_atts

   ! Local variables
   character(len=path_length)                         :: camel_emis_path_file
   type(emis_t)                                       :: emis
   integer                                            :: n_chans
   integer,             allocatable, dimension(:)     :: ch_total_index
   integer,             allocatable, dimension(:)     :: ch_lw_index
   real(kind=sreal),    allocatable, dimension(:,:,:) :: summat
   real(kind=sreal),    allocatable, dimension(:,:)   :: counter
   integer                                            :: i, j, k, lat, lon
   integer                                            :: nland
   type(interpol_t)                                   :: interp

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_camel_emissivity()'

   if (verbose) write(*,*) 'cyear: ',           trim(cyear)
   if (verbose) write(*,*) 'cmonth: ',          trim(cmonth)
   if (verbose) write(*,*) 'camel emis_path: ', trim(camel_emis_path)
   if (verbose) write(*,*) 'assume_full_path: ', assume_full_path


   ! Count the number of land and sea pixels, using the imager land/sea mask
   nland = count(imager_flags%lsflag .eq. 1)
   if (verbose) write(*,*) 'nland: ', nland

   source_atts%emissivity_file = 'Not used (no LW channels or no pixels of land)'

   ! If there are no lw channels or no land pixels in the scene, we have nothing
   ! more to do.
   if (channel_info%nchannels_lw == 0 .or. nland == 0) return

   n_chans = channel_info%nchannels_lw

   allocate(ch_total_index(n_chans))
   allocate(ch_lw_index(n_chans))
   j = 1
   k = 1
   do i = 1, channel_info%nchannels_total
      if (channel_info%channel_lw_flag(i) == 1) then
         if (.true.) then
            ! Indices of desired channels wrt all channels
            ch_total_index(j) = i
            ! Indices of desired channels wrt longwave channels
            ch_lw_index(j)    = k
            j = j + 1
         end if
         k = k + 1
      end if
   end do

   if (verbose) write(*,*) 'n channels for land emissivity: ', &
                            channel_info%nchannels_lw
#ifndef __INTEL_COMPILER
   if (verbose) write(*,*) 'instrument bands for land emissivity: ', &
                            channel_info%channel_ids_instr(ch_total_index)
   if (verbose) write(*,*) 'instument wavelengths for land emissivity: ', &
                            channel_info%channel_wl_abs(ch_total_index)
#endif

   ! Select correct modis file
   if (assume_full_path) then
      camel_emis_path_file = camel_emis_path
   else
      call select_camel_emiss_file(cyear, cmonth, camel_emis_path, &
           camel_emis_path_file)
   end if
   if (verbose) write(*,*) 'camel_emis_path_file: ', trim(camel_emis_path_file)

   source_atts%emissivity_file = trim(camel_emis_path_file)

   ! Read the data itself
   if (read_camel_emissivity(camel_emis_path_file, emis, &
       channel_info%channel_wl_abs(ch_total_index), verbose) .ne. 0) then
        write(*,*) 'ERROR: read_camel_emissivity(), problem reading camel ' // &
                   'emissivity file: ', camel_emis_path_file
        stop error_stop_code
   end if

   ! This emissivity data has very few missing values, but there are some. Set
   ! these to 0.999 (as close to 1 as the emissivity data itself gets). This is
   ! also a vaguely reasonable value for the water as well....

   where(emis%emissivity .le. 0.0) emis%emissivity = 0.999

   ! Now interpolate this data onto the data grid
   do j = 1, imager_geolocation%ny
      do i = imager_geolocation%startx, imager_geolocation%endx
         if (imager_flags%lsflag(i,j) .eq. 1 .and. &
              imager_geolocation%latitude(i,j) .ne. sreal_fill_value .and. &
              imager_geolocation%longitude(i,j) .ne. sreal_fill_value) then
            call bilinear_coef(emis%lon0, emis%lon_invdel, emis%nlon, &
                 emis%lat0, emis%lat_invdel, emis%nlat, &
                 imager_geolocation%longitude(i,j), &
                 imager_geolocation%latitude(i,j), interp)
            do k = 1, n_chans
               call interp_field(emis%emissivity(:,:,k), &
                   surface%emissivity(i,j,ch_lw_index(k)), interp)
            end do
         end if
      end do
   end do

   ! calculate the mean emissivity in each preproc grid
   allocate(counter(preproc_dims%min_lon:preproc_dims%max_lon, &
        preproc_dims%min_lat:preproc_dims%max_lat))
   allocate(summat(preproc_dims%min_lon:preproc_dims%max_lon, &
        preproc_dims%min_lat:preproc_dims%max_lat, n_chans))

   counter = 0
   summat  = 0.
   do j = 1, emis%nlat
      lat = floor((emis%lat0+(j-1)*emis%lat_del+preproc_dims%lat_offset)* &
            preproc_dims%dellat)+1
      if (lat.ge.preproc_dims%min_lat .and. lat.le.preproc_dims%max_lat) then
         do i = 1, emis%nlon
            lon = floor((emis%lon0+(i-1)*emis%lon_del+preproc_dims%lon_offset)* &
                  preproc_dims%dellon)+1
            if (lon.ge.preproc_dims%min_lon .and. &
                 lon.le.preproc_dims%max_lon) then
               summat(lon,lat,:) = summat(lon,lat,:)+emis%emissivity(i,j,:)
               counter(lon,lat)  = counter(lon,lat)+1
            end if
         end do
      end if
   end do

   do j = preproc_dims%min_lat, preproc_dims%max_lat
      do i = preproc_dims%min_lon, preproc_dims%max_lon
         if (counter(i,j) .gt. 0) then
            preproc_surf%emissivity(i,j,ch_lw_index) = summat(i,j,:) / &
               real(counter(i,j))
         end if
      end do
   end do

   deallocate(ch_total_index)
   deallocate(ch_lw_index)
   deallocate(counter)
   deallocate(summat)
   call deallocate_emis(emis)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_camel_emissivity()'

end subroutine get_camel_emissivity
