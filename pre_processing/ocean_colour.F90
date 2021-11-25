!-------------------------------------------------------------------------------
! Name: ocean_colour_m
!
! Purpose:
! Module in this file defines the structures for containing ocean colour data,
! a reader for OceanColour_cci data (L3S IOP MONTHLY GEO) and a routine for
! interpolating a gridded ocean colour product onto the required wavelength
! bands and pixel locations.
!
! History:
! 2016/07/12, GT: First version
! 2016/11/03, GT: Bug fix - if processing data outside coverage of
!                 OceanColour_cci, the code now correctly looks for climatology
!                 data with year='XXXX'
! Bugs:
! None known.
!-------------------------------------------------------------------------------
module ocean_colour_m
   use preproc_constants_m

   implicit none

   private

   public :: ocean_colour_t, &
             occci_t, &
             read_oceancolour_cci, &
             get_ocean_colour

   type ocean_colour_t
      logical                       :: have_data
      real(kind=sreal)              :: wl
      real(kind=sreal)              :: totabs
      real(kind=sreal)              :: totbsc
   end type ocean_colour_t

   type occci_t
      integer                       :: nlon
      integer                       :: nlat
      real(kind=dreal)              :: lon0, lon_invdel
      real(kind=dreal)              :: lat0, lat_invdel
      integer                       :: nwavelength
      integer, allocatable          :: iwavelength(:,:)
      real(kind=sreal), allocatable :: wavelength(:)
      real(kind=sreal), allocatable :: atot(:,:,:)
      real(kind=sreal), allocatable :: bbs(:,:,:)
   end type occci_t

contains

!-------------------------------------------------------------------------------
! Name: deallocate_occci()
!
! Purpose:
! Deallocates the occci data structure, created by read_oceancolour_cci, and
! defined above.
!
! Arguments:
! Name  Type In/Out/Both Description
! occci occci_t In       Structure to be dellocated
!
! Local variables:
!
! Bugs:
! None known
!-------------------------------------------------------------------------------
subroutine deallocate_occci(occci)

   implicit none

   type(occci_t), intent(inout) :: occci

   if (allocated(occci%wavelength)) then
      deallocate(occci%iwavelength)
      deallocate(occci%wavelength)
      deallocate(occci%atot)
      deallocate(occci%bbs)
   end if

end subroutine deallocate_occci

!-------------------------------------------------------------------------------
! Name: read_oceancolour_cci()
!
! Purpose:
! Read L3S OceanColour_cci IOP products (writen for v2.0 monthly, 4 km GEO
! files, but should work for any geo-projection IOP data).
!
! Description and Algorithm details:
! Opens the file, the name of which is passed as an argument, and checks the
! dimensions. Provided these seem ok, the geo-grid is then constructed from
! the files global attributes (we don't bother reading the lat/lon coordinates
! included in the file. The grid is then stored in the output structure as a
! starting value, 1/grid-separation and number of cells, for the grid cell
! centres.
! The requested band wavelengths are then compared to those provided by OCCCI;
! if the bands match those available, these bands will be returned; however,
! if the wavelengths do not match, a pair of OCCCI bands are returned for each
! requested wavelength:
! - if the requested wavelength is less than the lowest OCCCI wavelength
!   (412 nm), the lowest two wavelength bands will be returned.
! - if the requested wavelength is larger then the highest OCCCI wavelength
!   (665 nm), the highest two wavelength bands will be returned.
! - For bandss between these two extremes, the pair of bands the bracket the
!   requested wavelength will be choosen.
! The required OCCCI bands are marked for reading with a boolean flag, and the
! index numbers of the matching OCCCI bands for each requested wavelength are
! stored in the iwavelength array in the output structure.
! Once the list of required OCCCI bands has been completed the total absorption
! and total backscatter coefficient data are read from the file and placed in
! the output structure, and the file is closed.
!
! Arguments:
! Name  Type In/Out/Both Description
! path_to_file    In     Fully qualified path to the required OCCCI data.
!     Character
! occci occci_t   Out    Output data structure
! wavelengths     In     An array of wavelengths of the bands for which the
!      sreal             ocean colour is needed.
! verbose logical In     If set, the routine will print out messages
!
! Local variables:
!
! Bugs:
! None known
!-------------------------------------------------------------------------------
function read_oceancolour_cci(path_to_file, occci, wavelengths, verbose) &
     result (stat)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   ! Input variables
   character(len=*),  intent(in)  :: path_to_file
   real,              intent(in)  :: wavelengths(:)
   logical,           intent(in)  :: verbose

   ! Output variables
   type(occci_t),     intent(out) :: occci
   integer(kind=sint)             :: stat


   ! Local variables
   integer,          parameter :: occci_nwl = 6
   real(kind=sreal), parameter :: occci_wl(occci_nwl) = &
        (/ 0.412, 0.443, 0.490, 0.510, 0.560, 0.665 /)
   character(len=8), parameter :: occci_atotvar(occci_nwl) = &
        (/ 'atot_412', 'atot_443', 'atot_490', 'atot_510', &
           'atot_560', 'atot_665' /)
   character(len=7), parameter :: occci_bbpvar(occci_nwl) = &
        (/ 'bbp_412', 'bbp_443', 'bbp_490', 'bbp_510', 'bbp_560', 'bbp_665' /)

   integer                       :: i, j
   integer                       :: nwl
   logical                       :: occci_rd(occci_nwl)
   integer                       :: fid
   integer                       :: ntime, nlon , nlat
   integer, allocatable          :: iwavelength(:,:)
   real(kind=dreal)              :: lonmin, latmin
   real(kind=dreal)              :: lonres, latres
   character(len=21)             :: slonres, slatres
   real(kind=sreal), allocatable :: cache(:,:)


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_oceancolour_cci()'

   if (verbose) write(*,*) 'path_to_file: ', trim(path_to_file)
   if (verbose) write(*,*) 'wavelengths: ',  wavelengths

   nwl = size(wavelengths)
   ! Allocate the array which holds the index numbers of the occci band
   ! pairs which bracket each input band
   allocate(iwavelength(2,nwl))      ! Holds index nos. wrt all OCCCI bands
   allocate(occci%iwavelength(2,nwl))! Holds index nos. wrt to OCCCI band actually read

   ! Open NetCDF file and extract information about it
   call ncdf_open(fid, path_to_file, 'read_oceancolour_cci()')

   ! Variables needed: atot (total absorption), bbp (particulate backscatter)
   ! Available wavelengths: 412, 443, 490, 510, 560, 665 nm.
   if (verbose) write(*,*) 'Extracting dimension IDs'
   ! Extract the array dimensions
   ntime = ncdf_dim_length(fid, 'time', 'read_oceancolour_cci()')
   nlon = ncdf_dim_length(fid, 'lon', 'read_oceancolour_cci()')
   nlat = ncdf_dim_length(fid, 'lat', 'read_oceancolour_cci()')
   if (verbose) write(*,*) 'Dimensions are (time, lon, lat): ', ntime, nlon, nlat
   if (ntime .gt. 1) then
      write(*,*) 'Error: read_oceancolour_cci(): Time dimension is not 1. ' // &
           'Not expecting multi-temporal data. Filename: ', &
           trim(path_to_file), ' dimtime: ', ntime
      stop error_stop_code
   end if

   ! Define the lat/lon arrays in the output structure
   occci%nlon = nlon
   occci%nlat = nlat
   !allocate(occci%lon(nlon))
   !allocate(occci%lat(nlat))
   ! Read the data into these arrays
   !call ncdf_read_array(fid, lon, occci%lon)
   !call ncdf_read_array(fid, lat, occci%lat)
   ! Rather than reading (and storing) the entire lat/lon arrays, we
   ! assume we are dealing with a regular grid and simply calculate the
   ! grid spacing
   if (verbose) &
        write(*,*) 'Extracting grid attributes and building lat/lon grid:'
   ! Read the grid attributes needed...
   stat = nf90_get_att(fid, NF90_GLOBAL, "geospatial_lon_min", lonmin)
   stat = nf90_get_att(fid, NF90_GLOBAL, 'geospatial_lon_resolution', slonres)
   read(slonres,*) lonres
   stat = nf90_get_att(fid, NF90_GLOBAL, 'geospatial_lat_min', latmin)
   stat = nf90_get_att(fid, NF90_GLOBAL, 'geospatial_lat_resolution', slatres)
   read(slatres,*) latres
   ! Populate the output structure... note that we store the mid-point of
   ! each grid cell
   occci%lon0       = lonmin + lonres/2.0
   occci%lon_invdel = 1.0 / lonres
   occci%lat0       = latmin + latres/2.0
   occci%lat_invdel = 1.0 / latres

   if (verbose) then
      write(*,*) 'lon0, lon_invdel: ', occci%lon0, occci%lon_invdel
      write(*,*) 'lat0, lat_invdel: ', occci%lat0, occci%lat_invdel
   end if

   ! Now deal with the wavelengths requested. The approach here is to
   ! provide the extact wavelength if there is a match with what is
   ! provided by OceanColour_cci, or provide the bracketting wavelengths
   ! otherwise
   ! Store the wavelength pairs which correspond to each input wavelength
   ! so the calling routine can do the interpolation
   if (verbose) write(*,*) 'Selecting bands to read based on wavelength'
   occci_rd = .false.
   do i = 1, nwl
      j = 1
      do while ( (occci_wl(j) .le. wavelengths(i)) .and. (j .lt. occci_nwl) )
         j = j+1
      end do
      j = j-1
      occci_rd(j) = .true.
      iwavelength(:,i) = j
      if (j .eq.  occci_nwl) then
         occci_rd(j-1) = .true.
         iwavelength(1,i) = j-1
      else if (occci_wl(1) .gt. wavelengths(i)) then
         occci_rd(2) = .true.
         iwavelength(2,i) = 2
      else if (occci_wl(j) .ne. wavelengths(i)) then
         occci_rd(j+1) = .true.
         iwavelength(2,i) = j+1
      end if
   end do

   ! Now allocate the storage for the required data in the output structures
   occci%nwavelength = count(occci_rd)
   allocate(occci%wavelength(occci%nwavelength))
   allocate(occci%atot(nlon,nlat,occci%nwavelength))
   allocate(occci%bbs(nlon,nlat,occci%nwavelength))
   allocate(cache(nlon,nlat))

   ! Loop over the required data fields and read into the output structure
   j = 1
   do i = 1, occci_nwl
      if (occci_rd(i)) then
         occci%wavelength(j) = occci_wl(i)
         where(iwavelength .eq. i)
            occci%iwavelength = j
         end where
         if (verbose) write(*,*) 'Reading data for Wvl: ', occci%wavelength(j)
         call ncdf_read_array(fid, occci_atotvar(i), cache)
         occci%atot(:,:,j) = cache
         call ncdf_read_array(fid, occci_bbpvar(i), cache)
         occci%bbs(:,:,j) = cache
         j = j+1
      end if
   end do

   ! Close the data file
   call ncdf_close(fid, 'read_oceancolour_cci()')

   deallocate(cache)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_oceancolour_cci()'

end function read_oceancolour_cci

!-------------------------------------------------------------------------------
! Name: get_ocean_colour()
!
! Purpose:
! Deals with selecting the required OCCCI data file, determining which
! instrument bands require OCCCI data, calling read_oceancolour_cci() and
! interpolating the data onto the instrument bands and spatial grid.
!
! Description and Algorithm details:
! The instrument sw bands are first checked to see if they lie within the
! wavelength range where OCCCI data is relevant - any bands outside this range
! (currently limited to under 800 nm) use the default water absorption and
! backscatter values defined in the cox_munk module.
! Unless the "assume_full_path" switch is set, the routine first determines
! the path and OCCCI filename to read, based on the year and month. If the
! request year/month lie outside the coverage of OCCCI, the year will be set to
! 'XXXX', which is expected to contain a climatology product.
! read_oceancolour_cci() is then called, using these wavelengths and path.
! The coordinates and coefficients for bilinear interpolation of the OCCCI data
! onto the instrument grid are then determined using bilinear_coef(), and a mask
! indicating which OCCCI data points cover the instrument swath is created.
! Data gaps in the OCCCI data are then filled using fill_grid, using the swath
! mask to limit the filling to only relevant pixels.
! The OCCCI data is then linearly interpolated in wavelength, to match the
! required instrument bands, before interp_field() is used to apply the bilinear
! spatial interpolation set-up earlier.
! I final check for missing data is then done on  the output array, with missing
! values being replaced with the cox_munk defaults.
!
! Arguments:
! Name  Type In/Out/Both Description
! cyear character   In   'YYYY' year for which data is required.
! cmonth character  In   'MM' month for which data is requred.
! occci_path        In   The path to the OCCCI data. If assume_full_path is set
!       character        this is treated as the fully qualified path to the
!                        required data file, otherwise the code assumes the data
!                        is stored in yearly directories below occci_path, and
!                        that the file match the pattern:
!                        "ESACCI-OC-L3S-IOP-MERGED-1M_MONTHLY_4km_GEO_PML_OC".
! lat   sreal       In   Latitude points of the satellite grid.
! lon   sreal       In   Longitude points of the satellite grid.
! channel_info      In   ORAC common structure containing satellite channel
!     channel_info_t     indices and wavelengths.
! ocean_colour      Out  An array of structures, as defined above, containing
!                        the output ocean colour data on the satellite grid.
! assume_full_path  In   If set, the occci_path is assumed to be fully
!       logical          qualified.
! verbose logical   In   If set, additional output will be printed to std-out.
!
! Local variables:
!
! Bugs:
! None known
!-------------------------------------------------------------------------------
subroutine get_ocean_colour(cyear, cmonth, occci_path, lat, lon, &
     channel_info, ocean_colour, assume_full_path, verbose)

   use preproc_constants_m
   use cox_munk_constants_m
   use channel_structures_m
   use interpol_m
   use fill_grid_m
   use system_utils_m
   use netcdf, only: NF90_NOERR

   implicit none

   ! Input arguments
   character(len=*),     intent(in)  :: cyear
   character(len=*),     intent(in)  :: cmonth
   character(len=*),     intent(in)  :: occci_path
   real(kind=sreal),     intent(in)  :: lat(:)
   real(kind=sreal),     intent(in)  :: lon(:)
   type(channel_info_t), intent(in)  :: channel_info
   logical,              intent(in)  :: assume_full_path
   logical,              intent(in)  :: verbose

   ! Output arguments
   type(ocean_colour_t), allocatable, intent(out) :: ocean_colour(:,:)

   ! Local variables
   integer(kind=lint)              :: i, j, k, ii
   integer                         :: nbands, nbands_occci
   integer(kind=lint)              :: nsea
   real(kind=sreal),   allocatable :: wavelengths(:)
   integer(kind=lint), allocatable :: have_data_idx(:)
   integer                         :: iyear, imonth
   character(len=4)                :: cyear2
   character(len=path_length)      :: occci_path_file
   character(len=path_length)      :: occci_path_full
   character(len=path_length)      :: occci_file_regex
   character(len=path_length)      :: occci_file
   logical                         :: occci_file_exist = .false.
   character(len=7)                :: occci_file_read
   type(occci_t)                   :: occci
   real(kind=sreal),   allocatable :: tmp_data(:,:), tmp_data2(:,:)
   real(kind=sreal)                :: tmp_val
   integer(kind=byte), allocatable :: fg_mask(:,:)
   type(interpol_t),   allocatable :: interp(:)
   real(kind=sreal)                :: dwl

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_ocean_colour()'

   ! Generate the list of wavelengths required. We ignore multiple views
   ! here, as the read_oceancolour_cci ensures that the required data is
   ! only read once
   nbands = channel_info%nchannels_sw
   nsea = size(lat)

   ! Allocate the output structure
   allocate(ocean_colour(nbands,nsea))
   allocate(interp(nsea))

   ! Check which bands lie within the OCCCI wavelength coverage.
   ! Any bands which lie outside this range are assumed to have the base
   ! absorption and backscatter values from cox_munk_constants_m.
   nbands_occci = count(channel_info%channel_wl_abs .lt. 0.8)
   allocate(wavelengths(nbands_occci))
   allocate(have_data_idx(nbands_occci))
   ii = 1
   do i = 1, nbands
      if (channel_info%channel_wl_abs(channel_info%map_ids_sw_to_channel(i)) &
           .lt. 0.8) then
         wavelengths(ii) = &
              channel_info%channel_wl_abs(channel_info%map_ids_sw_to_channel(i))
         have_data_idx(ii) = i
         ocean_colour(i,:)%wl = wavelengths(ii)
         ii = ii+1
      else
         ocean_colour(i,:)%have_data = .false.
         ocean_colour(i,:)%wl = &
              channel_info%channel_wl_abs(channel_info%map_ids_sw_to_channel(i))
         ocean_colour(i,:)%totabs = &
              totalabs(channel_info%map_ids_abs_to_ref_band_sea(i))
         ocean_colour(i,:)%totbsc = &
              totalbsc(channel_info%map_ids_abs_to_ref_band_sea(i))
      end if
   end do

   read(cyear, *) iyear
   read(cmonth, *) imonth
   ! Check that our data is within the OceanColour_cci data record
   if ((iyear .lt. 1997) .or. &
       ((iyear .eq. 1997) .and. (imonth .lt. 9)) .or. &
       (iyear .gt. 2020)) then
      cyear2 = 'XXXX'
   else
      cyear2 = cyear
   end if

   ! Determine the path to the OceanColour_cci data file needed
   if (assume_full_path) then
      occci_path_file = occci_path
   else
      occci_path_full = trim(adjustl(occci_path))//'/'//trim(adjustl(cyear2))
      occci_file_regex = 'ESACCI-OC-L3S-IOP-MERGED-1M_MONTHLY_4km_GEO_..._OCx_QAA-'// &
           trim(adjustl(cyear2))//trim(adjustl(cmonth))//'-fv.\..\.nc'
      if (match_file(trim(occci_path_full), trim(occci_file_regex), occci_file) .ne. 0) then
         occci_file_regex = 'ESACCI-OC-L3S-OC_PRODUCTS-MERGED-1M_MONTHLY_4km_GEO_..._OC._QAA-'//&
                            trim(adjustl(cyear2))//trim(adjustl(cmonth))//'-fv.\..\.nc'
         if (match_file(trim(occci_path_full), trim(occci_file_regex), occci_file) .ne. 0) then
            write(*,*) 'ERROR: get_ocean_colour(): Unable to locate ', 'OceanColour_cci data: ', &
                       trim(occci_path_full)//'/'//trim(occci_file_regex)
            stop error_stop_code
         end if
      end if
      occci_path_file = trim(occci_path_full)//'/'//trim(occci_file)
   end if
   call c_to_fortran_str(occci_path_file)
   if (verbose) write(*,*) 'OCCCI data file: ', trim(occci_path_file)

   ! Check that this file exists and is readable
   inquire(file=trim(occci_path_file), exist=occci_file_exist, &
        read=occci_file_read)
   if (.not. occci_file_exist) then
      write(*,*) "ERROR: get_ocean_colour(): OceanColour_cci data "// &
           "file  doesn't exist: ", trim(occci_path_file)
      stop error_stop_code
   end if
   if (trim(occci_file_read) .eq. 'NO') then
      write(*,*) "ERROR: get_ocean_colour(): OceanColour_cci data "// &
           "file isn\'t readable: ", trim(occci_path_file)
      stop error_stop_code
   end if

   ! Read the data
   if (read_oceancolour_cci(occci_path_file, occci, wavelengths, verbose) .ne. &
       NF90_NOERR) then
      write(*,*) 'ERROR: read_oceancolour_cci: Problem encountered '// &
           'reading OceanColour_cci file: ', trim(occci_path_file)
      stop error_stop_code
   end if

   ! Use nearest neighbour to fill missing data in the Ocean Colour data
   ! Note we can't allocate the tmp_data array until we've created the
   ! occci structure
   allocate(tmp_data(occci%nlon,occci%nlat))
   allocate(tmp_data2(occci%nlon,occci%nlat))
   tmp_data  = 0.
   tmp_data2 = 0.

   ! Also create a bit mask that controls where we apply the data filling,
   ! limiting it to the region covered by the imager data
   allocate(fg_mask(occci%nlon,occci%nlat))
   fg_mask = 0

   ! Locate pixels surrounding each sea pixel from the imager structures
   ! Also flag required pixels from the occci data, assuming a regular
   ! grid
   do i = 1, nsea
      call bilinear_coef(occci%lon0, occci%lon_invdel, occci%nlon, &
           occci%lat0, occci%lat_invdel, occci%nlat, lon(i), lat(i), &
           interp(i))
      fg_mask(interp(i)%x0, interp(i)%y0) = 1
      fg_mask(interp(i)%x1, interp(i)%y0) = 1
      fg_mask(interp(i)%x0, interp(i)%y1) = 1
      fg_mask(interp(i)%x1, interp(i)%y1) = 1
   end do

   ! Now we fill in the gaps in the OC_cci data. We do this before
   ! interpolating in wavelength because the missing pixels are not
   ! necessarily the same for all the OC_cci bands
   do i = 1, occci%nwavelength
      tmp_data = occci%atot(:,:,i)
      call fill_grid(tmp_data, sreal_fill_value, fg_mask)

      tmp_data2 = occci%bbs(:,:,i)
      call fill_grid(tmp_data2, sreal_fill_value, fg_mask)

      occci%atot(:,:,i) = tmp_data
      occci%bbs(:,:,i) = tmp_data2
   end do

   ! Now we interpolate in wavelength, again using the fg_mask to only
   ! apply the calculation to the pixels we're actually interested in
   do i = 1, nbands_occci
      j = occci%iwavelength(1,i)
      k = occci%iwavelength(2,i)
      if (j .eq. k) then
         where(fg_mask .eq. 1)
            tmp_data  = occci%atot(:,:,j)
            tmp_data2 = occci%bbs(:,:,j)
         end where
      else
         dwl = (wavelengths(i)-occci%wavelength(j))/ &
              (occci%wavelength(k)-occci%wavelength(j))
         where(fg_mask .eq. 1)
            tmp_data  = occci%atot(:,:,j) + &
                 (occci%atot(:,:,k)-occci%atot(:,:,j)) * dwl
            tmp_data2 = occci%bbs(:,:,j) + &
                 (occci%bbs(:,:,k)-occci%bbs(:,:,j)) * dwl
         end where
      end if

      ! Finally, copy the data into the output structure, ready for
      ! use in cox_munk
      do j = 1, nsea
         ocean_colour(have_data_idx(i),j)%have_data = .true.
         call interp_field(tmp_data, tmp_val, interp(j))
         ocean_colour(have_data_idx(i),j)%totabs = tmp_val
         call interp_field(tmp_data2, tmp_val, interp(j))
         ocean_colour(have_data_idx(i),j)%totbsc = tmp_val
      end do
      ! Do a final check for fill values in this data, and replace with
      ! the defaults defined in cox-munk
      where(ocean_colour(have_data_idx(i),:)%totabs .eq. sreal_fill_value)
         ocean_colour(have_data_idx(i),:)%totabs = &
              totalabs(channel_info%map_ids_abs_to_ref_band_sea(have_data_idx(i)))
         ocean_colour(have_data_idx(i),:)%totbsc = &
              totalbsc(channel_info%map_ids_abs_to_ref_band_sea(have_data_idx(i)))
      end where
   end do

   deallocate(wavelengths)
   deallocate(have_data_idx)
   deallocate(interp)
   deallocate(tmp_data)
   deallocate(tmp_data2)
   deallocate(fg_mask)
   call deallocate_occci(occci)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_ocean_colour()'

end subroutine get_ocean_colour

end module ocean_colour_m
