!-------------------------------------------------------------------------------
! Name: get_surface_reflectance.F90
!
! Purpose:
! Populates the surface albedo part of the surface structure, using MODIS MCD43C
! spectral albedo over the land and the cox_munk ocean surface reflectance model
! over the sea.
!
! Description and Algorithm details:
!
! Arguments:
! Name            Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! cyear           string  in          Year, as a 4 character string.
! cdoy            string  in          DOY,  as a 3 character string.
! modis_surf_path char    in          Path to MODIS MCD43C data
! occci_path      char    in          Path to OceanColour_cci L3S IOP data
! imager_flags    struct  in          Imager structure containing land/sea flag
! imager_geolocation      in          Imager structure containing lat/lon points
!                 struct
! imager_angles   struct  in          Imager structure containing viewing geom.
! channel_info    struct  in          Preprocessing dimensions, including sw and
!                                     lw channel counts
! ecmwf           struct  in          ECMWF fields
! assume_full_path
!                 logic   in          T: inputs are filenames; F: folder names
! use_occci       logic   in          T: read and use OceanColour_cci data;
!                                     F: Use defaults defined in cox_munk module
! verbose         logic   in          T: print status information; F: don't
! surface         struct  both        Surface properties structure
! source_atts     struct  both        Source attributes
!
! History:
! 2012/04/30, GT: Finished first version
! 2012/05/30, GT: Bug fixes:
!    - Changed limit on do loops for populating tmp_WSA and wsalnd from
!      imager_measurements%nchannels to preproc_dims%nchan_sw
!    - Reversed index order of nested do loops to make efficient use of
!      column-major indexing
!    - lndcount and seacount reset to 1 before populating surface%albedo output
!      array.
! 2012/06/26, CP: Bug fixes
!    - Changed indexing
!    - Defined preproc_dims%channels this is hardwired so should really be
!      changed along with band info
!    - Changed count conditions may have to change back after channel info
!      correctly implemented.
!    - Changed code to use channel structure information
! 2012/06/27, GT: Changed from bilinear interpolation of MODIS land
!    surface albedo to nearest neighbour, hopefully with a resulting significant
!    speed improvement
! 2012/06/27, CP: changed modbands from 1,2,6 to 1,2,3
! 2012/07/02, GT: Bug fix. Fill grid was attempting to use the imager
!    grid to mask the MCD grid. The code now generates a mask for fill_grid
!    based on the lat-lon limits of the imager_geolocation arrays.
! 2012/07/04, CP: removed nview dependence
! 2012/07/30, CP: initialised allocated arrays
! 2012/08/07, CP: reformatted albedo array to be consistent with msi file
!    routine added that automatically selects MCD file added doy
! 2012/08/08, CP: Changed modband to 12,6 and created coxbands  2 3 4
! 2012/08/15, MJ: Speeds up code.
! 2012/08/15, GT: Added code for inserting (1.0 - emissivity) for the land
!    surface reflectance at 3.7 microns (done at the point where surface
!    reflectance is copied into the surface structures).
!    Bug fix. Temporary ocean surface reflectance arrays were being defined
!    using nswchannels (local value for land-surface reflectance) rather then
!    channel_info%nchannels_sw.
!    Also, coxbands now set from the channel_info%channel_ids_abs and
!    channel_info%channel_sw_flag (and is dynamic). Note: this will need
!    updating for multi-views.
! 2012/08/16, MJ: Fixed two bugs in bugfix from 15.8.
! 2012/08/20, MJ: Fixed  bugs in coxband assignment
! 2012/08/20, MJ: Changed read_mcd43c3 from function to subroutine in order to
!    iron out bugs
! 2012/08/22, MJ: Implements flexible x and y dimensions start and end indices
! 2012/12/14, CP: Changed howy loop was set changed starty to startyi to loop
!    over a granule!
! 2013/03/06, CP: fixed bug in imager_angles that was picked up when comiling in
!    gfortran: This bug had no effect on results.
! 2013/03/15, GT: Reinstated bilinear interpolation, as new version is nearly as
!    fast as nearest neighbour.
!    Fixed longitude bug in interpolation ECMWF wind fields (ECMWF longitude
!    runs from 0-360 degrees)
! 2013/09/02, AP: Removed startyi, endye.
! 2014/01/17, MJ: Fixed doy data type from byte to stint to comply with other
!    defs
! 2014/04/20, GM: Cleaned up the code.
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/06/20, GM: Handle case when imager_geolocation%latitude or
!    imager_geolocation%longitude is equal to fill_value.
! 2014/06/11, AP: Use standard fill value rather than unique one. Use new
!    ecmwf structures.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/08/11, AP: New bilinear interpolation routines. Data now copied directly
!    into surface%albedo for the surface.
! 2014/08/13, GM: Fixed bug where the length of array arguments is larger than
!    the amount of valid data within which affects routines that get the length
!    to be processed from the size() intrinsic.
! 2014/10/15, GM: Changes related to supporting an arbitrary set of SW channels.
!    Still limited by availability from the land and ocean reflectance sources.
! 2014/10/23, OS: added support for reading full BRDF file path
! 2014/12/02, GM: Fixed handling of night.  Values that are not dependent on
!    solar zenith angle (albedo, rho_dv, and rho_dd) are computed at night.
!    Values that are (rho_0v and rho_0d) are not computed and set to fill.
! 2014/12/01, CP: Added source attributes.
! 2015/01/14, AP: Allow the code to accept channels in arbitrary order.
! 2015/10/03, GM: Changes to support AATSR channel 1 and MODIS channels 3, 4, 5,
!    and 7 for sea surface reflectance.
! 2015/10/27, CP: Changed path for brdf files to be modis_brdf_path.
! 2015/11/21, GM: Fixed use of channel_info%nchannels_sw where it should have
!    been n_bands.
! 2016/05/02, AP: Added multiple views.
! 2016/07/07, SP: Corrected to run if only land or only sea pixels are present.
! 2016/07/12, GT: Added OceanColour_cci data support.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine get_surface_reflectance(cyear, cdoy, cmonth, modis_surf_path, &
     modis_brdf_path, occci_path, imager_flags, imager_geolocation, &
     imager_angles, channel_info, ecmwf, assume_full_path, include_full_brdf, &
     use_occci, use_swansea_climatology, swan_g, verbose, surface, source_atts)

   use channel_structures_m
   use cox_munk_m
   use cox_munk_constants_m
   use ecmwf_m, only : ecmwf_t
   use fill_grid_m
   use imager_structures_m
   use interpol_m
   use mcd43c_m
   use ocean_colour_m
   use system_utils_m
   use preproc_constants_m
   use preproc_structures_m
   use ross_thick_li_sparse_r_m
   use source_attributes_m
   use surface_structures_m

   implicit none

   ! Input variables
   character(len=*),           intent(in)    :: cyear
   character(len=*),           intent(in)    :: cdoy
   character(len=*),           intent(in)    :: cmonth
   character(len=*),           intent(in)    :: modis_surf_path
   character(len=*),           intent(in)    :: modis_brdf_path
   character(len=*),           intent(in)    :: occci_path
   type(imager_flags_t),       intent(in)    :: imager_flags
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(imager_angles_t),      intent(in)    :: imager_angles
   type(channel_info_t),       intent(in)    :: channel_info
   type(ecmwf_t),              intent(in)    :: ecmwf
   logical,                    intent(in)    :: assume_full_path
   logical,                    intent(in)    :: include_full_brdf
   logical,                    intent(in)    :: use_occci
   logical,                    intent(in)    :: use_swansea_climatology
   real,                       intent(in)    :: swan_g
   logical,                    intent(in)    :: verbose
   type(surface_t),            intent(inout) :: surface
   type(source_attributes_t),  intent(inout) :: source_atts

   ! Local variables

   ! General
   integer                           :: i, j,k, ii, jj, kk, i_oc, i_view
   logical                           :: flag
   integer                           :: nsea, nland
   integer                           :: seacount
   integer                           :: lndcount
   logical,            allocatable   :: mask(:,:)
   integer,            allocatable   :: bands(:)
   integer,            allocatable   :: band_to_sw_index(:)
   real,               allocatable   :: solza(:), satza(:)
   real,               allocatable   :: solaz(:), relaz(:)
   type(interpol_t),   allocatable   :: interp(:)

   ! Band definitions
   integer, parameter :: n_modbands = 7
   integer, target    :: modbands(n_modbands) = [1, 2, 3, 4, 5, 6, 7]
   integer, parameter :: n_swanbands = 4
   integer, target    :: swanbands(n_swanbands) = [4, 1, 2, 6]
   integer, parameter :: n_coxbands = 9
   integer, parameter :: coxbands(n_coxbands) = [1, 2, 3, 4, 5, 6, 7, 8, 9]

   ! Land surface reflectance
   character(len=path_length)        :: modis_surf_path_file
   character(len=path_length)        :: modis_brdf_path_file
   type(mcd43c1_t)                   :: mcdc1
   type(mcd43c3_t)                   :: mcdc3
   integer                           :: n_inbands
   integer,            pointer       :: in_bands(:)
   integer                           :: n_bands
   real,               allocatable   :: tmp_data(:,:)
   integer(kind=byte), allocatable   :: fg_mask(:,:)
   real,               allocatable   :: wsalnd(:,:)
   real,               allocatable   :: wgtlnd(:,:,:)
   real,               allocatable   :: rholnd(:,:,:), tmprho(:,:,:)
   real                              :: tmp_val, tmp_p(2)
   integer                           :: stat

   ! Sea surface reflectance
   real,               allocatable   :: u10sea(:), v10sea(:)
   real,               allocatable   :: latsea(:), lonsea(:)
   real,               allocatable   :: refsea(:,:)
   real,               allocatable   :: rhosea(:,:,:)
   type(cox_munk_shared_geo_wind_t)  :: cox_munk_shared_geo_wind
   type(ocean_colour_t), allocatable :: ocean_colour(:,:)
#ifdef __INTEL_COMPILER
   type(ocean_colour_t), allocatable :: ocean_colour2(:,:)
#endif


   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_surface_reflectance()'

   if (verbose) write(*,*) 'cyear: ',             trim(cyear)
   if (verbose) write(*,*) 'cdoy: ',              trim(cdoy)
   if (verbose) write(*,*) 'modis_surf_path: ',   trim(modis_surf_path)
   if (verbose) write(*,*) 'modis_brdf_path: ',   trim(modis_brdf_path)
   if (verbose) write(*,*) 'occci_path:      ',   trim(occci_path)
   if (verbose) write(*,*) 'assume_full_path: ',  assume_full_path
   if (verbose) write(*,*) 'include_full_brdf: ', include_full_brdf
   if (verbose) write(*,*) 'use_occci: ',         use_occci
   if (verbose) write(*,*) 'use_swansea_clim: ',  use_swansea_climatology, swan_g


   ! Mask out pixels set to fill_value and out of BRDF angular range
   allocate(mask(imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))

   mask = .false.

   do k = 1, imager_angles%nviews
      mask = mask .or. &
             imager_geolocation%latitude  .ne. sreal_fill_value .and. &
             imager_geolocation%longitude .ne. sreal_fill_value .and. &
             imager_angles%solzen(:,:,k)  .ne. sreal_fill_value .and. &
             imager_angles%satzen(:,:,k)  .ne. sreal_fill_value .and. &
             imager_angles%solazi(:,:,k)  .ne. sreal_fill_value .and. &
             imager_angles%relazi(:,:,k)  .ne. sreal_fill_value
   end do

   ! Albedo, rho_dv, and rho_dd are all valid at night rho_0v and rho_0d are
   ! not.  At night we have two choices: (1) compute what we can (albedo,
   ! rho_dv, and rho_dd) at the expense of some processing time and set rho_0v
   ! and rho_0d to fill, or (2) set all reflectance values to fill since the
   ! main processor won't use them anyway.  The code below should be uncommented
   ! for option 2. Otherwise, the case of solzen > maxsza_twi is checked at the
   ! BRDF code level.
   do k = 1, imager_angles%nviews
      mask = mask .and. imager_angles%solzen(:,:,k) .lt. maxsza_twi
   end do

   ! Count the number of land and sea pixels, using the imager land/sea mask
   nsea  = count(mask .and. imager_flags%lsflag .eq. 0)
   nland = count(mask .and. imager_flags%lsflag .eq. 1)
   if (verbose) write(*,*) 'nsea, nland: ', nsea, nland

   source_atts%albedo_file = 'Not used (no SW channels or no pixels of land)'
   source_atts%brdf_file   = 'Not used (no SW channels or no pixels of land)'

   if (nland .eq. 0 .and. nsea .eq. 0) then
      if (verbose) write(*,*) 'WARNING: No land or sea pixels to process; ' // &
           'something is wrong if daytime processing was expected.'
      return
   end if

   ! If there are no sw channels, we have nothing more to do.
   if (channel_info%nchannels_sw == 0) return


   !----------------------------------------------------------------------------
   ! Extract land surface reflectance
   !----------------------------------------------------------------------------
   if (nland .gt. 0) then
      if (verbose) write(*,*) &
         'get_surface_reflectance(): Beginning LAND SURFACE REFLECTANCE ', &
         'calculation'

      ! Allocate and populate the local arrays required for land pixels
      allocate(wsalnd(channel_info%nchannels_sw,nland))
      wsalnd(:,:) = sreal_fill_value
      allocate(interp(nland))
      if (include_full_brdf) then
         if (.not. use_swansea_climatology) then
            allocate(solza(nland))
            allocate(satza(nland))
            allocate(solaz(nland))
            allocate(relaz(nland))
         end if
         allocate(rholnd(channel_info%nchannels_sw,nland,4))
      end if

      ! Read MODIS MCD43C3 surface reflectance data
      ! Need to set the band numbers in terms of MODIS band numbering. These are
      ! the 0.65, 0.87(ish) and 1.6 micron bands

      ! Bands that are available.
      if (use_swansea_climatology) then
         n_inbands = n_swanbands
         in_bands => swanbands
      else
         n_inbands = n_modbands
         in_bands => modbands
      end if

      ! Count number of bands required (a 1-to-many mapping for multiple views)
      ii = 0
      do i = 1, n_inbands
         do j = 1, channel_info%nchannels_sw
            if (channel_info%map_ids_abs_to_ref_band_land(j) .eq. &
                in_bands(i)) then
               ii = ii + 1
               exit
            end if
         end do
      end do
      n_bands = ii

      if (verbose) write(*,*) 'n channels required for land reflectance: ', &
                              n_bands

      allocate(bands(n_bands))
      if (include_full_brdf) then
         allocate(wgtlnd(3,n_bands,nland))
      end if

      ! Identify bands required
      ii = 1
      do i = 1, n_inbands
         do j = 1, channel_info%nchannels_sw
            if (channel_info%map_ids_abs_to_ref_band_land(j) .eq. &
                in_bands(i)) then
               ! Indices of bands to be read
               bands(ii) = in_bands(i)
               ii = ii + 1
               exit
            end if
         end do
      end do

      ! Check all pure SW channels have a band
      do i = 1, channel_info%nchannels_sw
         ii = channel_info%map_ids_sw_to_channel(i)

         if (channel_info%channel_sw_flag(ii) == 0) cycle
         if (channel_info%channel_lw_flag(ii) /= 0) cycle
         if (channel_info%map_ids_abs_to_ref_band_land(i) < 0) cycle

         flag = .true.
         do j = 1, n_bands
            if (channel_info%map_ids_abs_to_ref_band_land(i) .eq. &
                 bands(j)) then
               flag = .false.
               exit
            end if
         end do

         ! Flag channels without a MODIS band unless they are mixed
         if (flag) then
            write(*,*) 'ERROR: get_surface_reflectance(), instrument ' // &
                 'channel ', channel_info%channel_ids_instr(ii), ' does ' // &
                 'not have a corresponding land reflectance product channel'
            stop error_stop_code
         end if
      end do

      if (verbose) print *, 'intrument bands: ', channel_info%channel_ids_instr
      if (verbose) print *, 'MCD43CX bands selected: ', bands

      ! Select correct modis file
      if (verbose) write(*,*) 'find appropriate files:'
      if (verbose) write(*,*) 'modis_surf_path: ', trim(modis_surf_path)
      if (verbose) write(*,*) 'modis_brdf_path: ', trim(modis_brdf_path)
      if (assume_full_path) then
         modis_surf_path_file = modis_surf_path
         modis_brdf_path_file = modis_brdf_path
      else
         if (use_swansea_climatology) then
            write(*,*) 'ERROR: get_surface_reflectance(): ' // &
                 'USE_SWANSEA_CLIMATOLOGY requires ASSUME_FULL_PATH.'
            stop error_stop_code
         end if
         call select_modis_albedo_file(cyear, cdoy, modis_surf_path, &
              .false., modis_surf_path_file)
         if (include_full_brdf) then
            call select_modis_albedo_file(cyear, cdoy, modis_brdf_path, &
                 .true., modis_brdf_path_file)
         end if
      end if
      call c_to_fortran_str(modis_surf_path_file)
      call c_to_fortran_str(modis_brdf_path_file)
      if (verbose) then
         write(*,*) 'modis_surf_path_file: ', trim(modis_surf_path_file)
         if (include_full_brdf) then
            write(*,*) 'modis_brdf_path_file: ', trim(modis_brdf_path_file)
         end if
      end if

      source_atts%albedo_file = trim(modis_surf_path_file)
      source_atts%brdf_file   = trim(modis_brdf_path_file)

      ! Read the data itself
      if (use_swansea_climatology) then
         call read_swansea_climatology(modis_surf_path_file, n_bands, bands, &
              mcdc3)
      else
         call read_mcd43c3(modis_surf_path_file, mcdc3, n_bands, bands, &
              .true., .false., .false., verbose, stat)

         if (include_full_brdf) then
            call read_mcd43c1(modis_brdf_path_file, mcdc1, n_bands, bands, &
                 .true., .false., verbose, stat)
         end if
      end if

      ! Fill missing data in the MODIS surface reflectance using a nearest
      ! neighbour technique. Note we cannot allocate tmp_data until we've
      ! created the mcd structure
      allocate(tmp_data(mcdc3%nlon,mcdc3%nlat))
      tmp_data = 0.

      ! Also create a bit mask that controls where we apply the data filling,
      ! limiting it to the region covered by the imager data
      allocate(fg_mask(mcdc3%nlon,mcdc3%nlat))
      fg_mask = 0

      ! Locate pixels surrounding each land pixel from the imager structures
      ! Also flag required pixels from MCD array, assuming a regular grid
      lndcount = 1
      do i = 1, imager_geolocation%ny
         do j = imager_geolocation%startx, imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 1 .and. mask(j,i)) then
               call bilinear_coef(mcdc3%lon0, mcdc3%lon_invdel, mcdc3%nlon, &
                    mcdc3%lat0, mcdc3%lat_invdel, mcdc3%nlat, &
                    imager_geolocation%longitude(j,i), &
                    imager_geolocation%latitude(j,i), interp(lndcount))

               ! Flag pixels for which there will be albedo data
               if (abs(imager_geolocation%latitude(j,i)) < 80.2) then
                  fg_mask(interp(lndcount)%x0, interp(lndcount)%y0) = 1
                  fg_mask(interp(lndcount)%x1, interp(lndcount)%y0) = 1
                  fg_mask(interp(lndcount)%x0, interp(lndcount)%y1) = 1
                  fg_mask(interp(lndcount)%x1, interp(lndcount)%y1) = 1
               end if

               lndcount = lndcount + 1
            end if
         end do
      end do

      do i = 1, n_bands
         tmp_data = mcdc3%WSA(:,:,i)

         call fill_grid(tmp_data, sreal_fill_value, fg_mask)

         do lndcount = 1, nland
            call interp_field(tmp_data, tmp_val, interp(lndcount))

            do j = 1, channel_info%nchannels_sw
               jj = channel_info%map_ids_abs_to_ref_band_land(j)
               if (jj .lt. 0) then
                  wsalnd(j,lndcount) = 0.
               else if (jj .eq. bands(i)) then
                  wsalnd(j,lndcount) = tmp_val
               end if
            end do
         end do

         if (use_swansea_climatology) then
            if (include_full_brdf) then
               ! mcd4%WSA was used for the Swansea S parameter and BSA the P
               call fill_grid(mcdc3%BSA(:,:,1), sreal_fill_value, fg_mask)
               call fill_grid(mcdc3%BSA(:,:,2), sreal_fill_value, fg_mask)

               do lndcount = 1, nland
                  call interp_field(mcdc3%BSA, tmp_p, interp(lndcount))

                  do j = 1, channel_info%nchannels_sw
                     jj = channel_info%map_ids_abs_to_ref_band_land(j)
                     kk = channel_info%map_ids_sw_to_channel(j)
                     i_view = channel_info%channel_view_ids(kk)

                     if (jj == bands(i)) then
                        ! rho_0V = s * p
                        rholnd(j,lndcount,1) = wsalnd(j,lndcount) * tmp_p(i_view)
                        ! rho_DD = g * s / (1 - (1-g) * s)
                        rholnd(j,lndcount,4) = swan_g * wsalnd(j,lndcount) / &
                             (1.0 - (1.0 - swan_g) * wsalnd(j,lndcount))
                        ! rho_0D = g * (1-g) *s^2 / (1 - (1-g) * s)
                        rholnd(j,lndcount,2) = (1.0 - swan_g) * &
                             wsalnd(j,lndcount) * rholnd(j,lndcount,4)
                        ! rho_DV is neglected in the Swansea model
                        rholnd(j,lndcount,3) = sreal_fill_value
                        ! albedo = rho_DD
                        wsalnd(j,lndcount) = rholnd(j,lndcount,4)
                     end if
                  end do
               end do
            else
               do j = 1, channel_info%nchannels_sw
                  jj = channel_info%map_ids_abs_to_ref_band_land(j)
                  if (jj == bands(i)) wsalnd(j,:) = swan_g * wsalnd(j,:) / &
                       (1.0 - (1.0 - swan_g) * wsalnd(j,:))
               end do
            end if

         else if (include_full_brdf) then
            do j = 1, 3
               tmp_data = mcdc1%brdf_albedo_params(:,:,j,i)

               call fill_grid(tmp_data, sreal_fill_value, fg_mask)

               do lndcount = 1, nland
                  call interp_field(tmp_data, wgtlnd(j,i,lndcount), &
                       interp(lndcount))
               end do
            end do
         end if
      end do

      if (verbose) then
         do i = 1, channel_info%nchannels_sw
               write(*,*) 'Land WSA: channel, min, max = ', &
                    i, minval(wsalnd(i,:)), maxval(wsalnd(i,:))
         end do
      end if

      if (include_full_brdf .and. .not. use_swansea_climatology) then
         do i_view = 1, imager_angles%nviews
            lndcount = 1
            do i = 1, imager_geolocation%ny
               do j = imager_geolocation%startx, imager_geolocation%endx
                  if (imager_flags%lsflag(j,i) .eq. 1 .and. mask(j,i)) then
                     solza(lndcount) = imager_angles%solzen(j,i,i_view)
                     satza(lndcount) = imager_angles%satzen(j,i,i_view)
                     solaz(lndcount) = imager_angles%solazi(j,i,i_view)
                     relaz(lndcount) = imager_angles%relazi(j,i,i_view)

                     lndcount = lndcount+1
                  end if
               end do
            end do

            allocate(tmprho(n_bands,nland,4))
            call ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd(n_bands, &
                 solza, satza, solaz, relaz, wgtlnd, sreal_fill_value, &
                 tmprho(:,:,1), tmprho(:,:,2), tmprho(:,:,3), tmprho(:,:,4), &
                 verbose)

            do i = 1, channel_info%nchannels_sw
               if (channel_info%sw_view_ids(i) .ne. i_view) cycle

               ii = channel_info%map_ids_abs_to_ref_band_land(i)
               if (ii .lt. 0) then
                  rholnd(i,:,:) = 0.
               else
                  do j = 1, n_bands
                     if (ii .eq. bands(j)) then
                        rholnd(i,:,:) = tmprho(j,:,:)
                        exit
                     end if
                  end do
               end if
            end do
            deallocate(tmprho)
         end do
      end if

      ! end do ! End of instrument view loop

      ! Do some clean-up
      deallocate(bands)
      deallocate(tmp_data)
      deallocate(fg_mask)
      deallocate(interp)
      call deallocate_mcd43c3(mcdc3)
      if (include_full_brdf .and. .not. use_swansea_climatology) then
         deallocate(solza)
         deallocate(satza)
         deallocate(solaz)
         deallocate(relaz)
         deallocate(wgtlnd)
         call deallocate_mcd43c1(mcdc1)
      end if

   end if ! End of land surface reflectance setting


   !----------------------------------------------------------------------------
   ! Compute sea surface reflectance
   !----------------------------------------------------------------------------
   if (nsea .gt. 0) then
      if (verbose) write(*,*) &
         'get_surface_reflectance(): Beginning SEA SURFACE REFLECTANCE ', &
         'calculation'

      ! Allocate and populate the local arrays required for sea pixels
      allocate(solza(nsea))
      allocate(satza(nsea))
      allocate(solaz(nsea))
      allocate(relaz(nsea))
      allocate(u10sea(nsea))
      allocate(v10sea(nsea))
      allocate(refsea(channel_info%nchannels_sw,nsea))
      if (include_full_brdf) then
         allocate(rhosea(channel_info%nchannels_sw,nsea,4))
      end if
      if (use_occci) then
         allocate(latsea(nsea))
         allocate(lonsea(nsea))
      end if

      ! Extract and flatten out angles and lat/lon pixels over ocean.
      seacount = 1
      do i = 1, imager_geolocation%ny
         do j = imager_geolocation%startx, imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 0 .and. mask(j,i)) then
               if (use_occci) then
                  latsea(seacount) = imager_geolocation%latitude(j,i)
                  lonsea(seacount) = imager_geolocation%longitude(j,i)
               end if
               seacount = seacount+1
            end if
         end do
      end do

      ! Interpolate the ECMWF wind fields onto the instrument grid
      seacount = 1
      allocate(interp(1))
      do i = 1, imager_geolocation%ny
         do j = imager_geolocation%startx, imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 0 .and. mask(j,i)) then
               call bilinear_coef(ecmwf%lon, ecmwf%xdim, ecmwf%lat, &
                    ecmwf%ydim, imager_geolocation%longitude(j,i), &
                    imager_geolocation%latitude(j,i), interp(1))

               call interp_field(ecmwf%u10, u10sea(seacount), interp(1))
               call interp_field(ecmwf%v10, v10sea(seacount), interp(1))

               seacount = seacount+1
            end if
         end do
      end do
      deallocate(interp)

      ! If requested, use OceanColour_cci data to set backscatter and extinction
      ! of the sea water.
      if (use_occci) then
         if (verbose) write(*,*) 'Calling get_ocean_colour...'
         call get_ocean_colour(cyear, cmonth, occci_path, latsea, &
              lonsea, channel_info, ocean_colour, assume_full_path, verbose)
         if (verbose) write(*,*) 'get_ocean_colour done'
      else
         allocate(ocean_colour(channel_info%nchannels_sw,1))
         do i = 1, channel_info%nchannels_sw
            ii = channel_info%map_ids_abs_to_ref_band_sea(i)

            ocean_colour(i,1)%have_data = .false.
            ocean_colour(i,1)%totabs = totalabs(ii)
            ocean_colour(i,1)%totbsc = totalbsc(ii)
         end do
      end if

      ! Process views separately
      do i_view = 1, imager_angles%nviews
         ii = 0
         do i = 1, channel_info%nchannels_sw
            ! Check that the channel is in the current view
            if (channel_info%sw_view_ids(i) .ne. i_view) cycle

            flag = .true.
            do j = 1, n_coxbands
               if (channel_info%map_ids_abs_to_ref_band_sea(i) .eq. &
                   coxbands(j)) then
                  flag = .false.
                  ii = ii + 1
                  exit
               end if
            end do

            if (flag) then
               ii = channel_info%map_ids_sw_to_channel(i)
               write(*,*) 'ERROR: get_surface_reflectance(), instrument ' // &
                    'channel ', channel_info%channel_ids_instr(ii), ' does ' // &
                    'not have a corresponding ocean reflectance product channel'
               stop error_stop_code
            end if
         end do
         n_bands = ii

         if (verbose) write(*,*) 'n channels required for sea reflectance: ', &
                                 n_bands

         allocate(bands(n_bands))
         allocate(band_to_sw_index(n_bands))

         ii = 1
         do i = 1, channel_info%nchannels_sw
            ! Check that the channel is in the current view
            if (channel_info%sw_view_ids(i) .ne. i_view) cycle

            do j = 1, n_coxbands
               if (channel_info%map_ids_abs_to_ref_band_sea(i) .eq. &
                    coxbands(j)) then
                  ! Indices of bands to be computed
                  bands(ii) = j
                  ! A mapping of bands to be read to sw channel
                  band_to_sw_index(ii) = i
                  ii = ii + 1
                  exit
               end if
            end do
         end do

         seacount = 1
         do i = 1, imager_geolocation%ny
            do j = imager_geolocation%startx, imager_geolocation%endx
               if (imager_flags%lsflag(j,i) .eq. 0 .and. mask(j,i)) then
                  solza(seacount) = imager_angles%solzen(j,i,i_view)
                  satza(seacount) = imager_angles%satzen(j,i,i_view)
                  solaz(seacount) = imager_angles%solazi(j,i,i_view)
                  relaz(seacount) = imager_angles%relazi(j,i,i_view)
                  seacount = seacount+1
               end if
            end do
         end do

         do j = 1, nsea
            call cox_munk3_calc_shared_geo_wind(solza(j), satza(j), solaz(j), &
                 relaz(j), u10sea(j), v10sea(j), cox_munk_shared_geo_wind)

            if (use_occci) then
               i_oc = j
            else
               i_oc = 1
            end if
            do i = 1, n_bands
               ii = band_to_sw_index(i)

               call cox_munk3(bands(i), cox_munk_shared_geo_wind, &
                    ocean_colour(ii,i_oc), refsea(ii, j))
            end do
         end do

         if (verbose) then
            do i = 1, n_bands
               ii = band_to_sw_index(i)
               write(*,*) 'Sea refl: sw_index, wvl, min, max = ', ii, &
                    channel_info%channel_wl_abs( &
                        channel_info%map_ids_sw_to_channel(ii) &
                    ), &
                    minval(refsea(ii,:)), maxval(refsea(ii,:))
            end do
         end if

         if (include_full_brdf) then
            allocate(tmprho(n_bands,nsea,4))
#ifdef __INTEL_COMPILER
            if (use_occci) then
               allocate(ocean_colour2(n_bands,nsea))
            else
               allocate(ocean_colour2(n_bands,1))
            end if
            do i = 1, n_bands
               ocean_colour2(i,:) = ocean_colour(band_to_sw_index(i),:)
            end do
            call cox_munk_rho_0v_0d_dv_and_dd(bands, solza(:), satza(:), &
                 solaz(:), relaz(:), ocean_colour2(:,:), &
                 u10sea, v10sea, sreal_fill_value, tmprho(:,:,1), &
                 tmprho(:,:,2), tmprho(:,:,3), tmprho(:,:,4), verbose)
            deallocate(ocean_colour2)
#else
            call cox_munk_rho_0v_0d_dv_and_dd(bands, solza(:), satza(:), &
                 solaz(:), relaz(:), ocean_colour(band_to_sw_index,:), &
                 u10sea, v10sea, sreal_fill_value, tmprho(:,:,1), &
                 tmprho(:,:,2), tmprho(:,:,3), tmprho(:,:,4), verbose)
#endif

            do i = 1, n_bands
               ii = band_to_sw_index(i)
               rhosea(ii,:,:) = tmprho(i,:,:)
            end do
            deallocate(tmprho)
         end if

         deallocate(bands)
         deallocate(band_to_sw_index)
      end do

      ! Tidy up cox_munk input arrays
      deallocate(solza)
      deallocate(satza)
      deallocate(solaz)
      deallocate(relaz)
      deallocate(u10sea)
      deallocate(v10sea)
   end if ! End of sea surface reflectance setting


   !----------------------------------------------------------------------------
   ! Copy the reflectance values from land and sea into the output surface
   ! reflectance structure.
   !
   ! Note that for surface%albedo used in the Lambertian retrieval we use the
   ! white-sky albedo for the land surface reflectance in, while the bi-direc-
   ! tional ocean reflectance (in the first view in a multiview situation) is
   ! used as an approximation.
   !----------------------------------------------------------------------------
   lndcount = 1
   seacount = 1
   do i = 1, imager_geolocation%ny
      do j = imager_geolocation%startx, imager_geolocation%endx
         if (mask(j,i)) then
            if (imager_flags%lsflag(j,i) .ne. 0) then

               surface%albedo(j,i,:) = wsalnd(:,lndcount)

               if (include_full_brdf) then
                  surface%rho_0v(j,i,:) = rholnd(:,lndcount,1)
                  surface%rho_0d(j,i,:) = rholnd(:,lndcount,2)
                  surface%rho_dv(j,i,:) = rholnd(:,lndcount,3)
                  surface%rho_dd(j,i,:) = rholnd(:,lndcount,4)
               end if

               ! The MCD43CX products do not provide reflectance in the mixed
               ! channnels therefore we use 1-emissivity.
               ii = 0
               kk = 0
               do k = 1, channel_info%nchannels_total
                  if (channel_info%channel_sw_flag(k) .ne. 0) ii = ii + 1
                  if (channel_info%channel_lw_flag(k) .ne. 0) kk = kk + 1
                  if (channel_info%channel_sw_flag(k) .ne. 0 .and. &
                      channel_info%channel_lw_flag(k) .ne. 0) then
                     surface%albedo(j,i,ii) = 1.0 - surface%emissivity(j,i,kk)

                     if (include_full_brdf) then
                        surface%rho_0v(j,i,ii) = 1.0 - surface%emissivity(j,i,kk)
                        surface%rho_0d(j,i,ii) = 1.0 - surface%emissivity(j,i,kk)
                        surface%rho_dv(j,i,ii) = 1.0 - surface%emissivity(j,i,kk)
                        surface%rho_dd(j,i,ii) = 1.0 - surface%emissivity(j,i,kk)
                     end if
                  end if
               end do

               lndcount = lndcount+1

            else

               surface%albedo(j,i,:) = refsea(:,seacount)

               if (include_full_brdf) then
                  surface%rho_0v(j,i,:) = rhosea(:,seacount,1)
                  surface%rho_0d(j,i,:) = rhosea(:,seacount,2)
                  surface%rho_dv(j,i,:) = rhosea(:,seacount,3)
                  surface%rho_dd(j,i,:) = rhosea(:,seacount,4)
               end if

               seacount = seacount+1
            end if
         end if
      end do
   end do


   ! Check to see if the land and sea reflectance arrays have been created
   ! before deallocating them
   deallocate(mask)

   if (allocated(wsalnd)) deallocate(wsalnd)
   if (allocated(rholnd)) deallocate(rholnd)

   if (allocated(refsea)) deallocate(refsea)
   if (allocated(rhosea)) deallocate(rhosea)


   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_surface_reflectance()'

end subroutine get_surface_reflectance
