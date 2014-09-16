!-------------------------------------------------------------------------------
! Name: netcdf_output_create.F90
!
! Purpose:
! Define the fields and attributes of the NetCDF files output by the
! preprocessor. Three versions exist: RTM, SWATH, and CONFIG.
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! global_atts    struct  in   Structure detailing NCDF header contents.
! cyear          string  in   Year, as a 4 character string.
! cmonth         string  in   Month of year, as a 2 character string.
! cday           string  in   Day of month, as a 2 character string.
! chour          string  in   Hour of day, as a 2 character string.
! cminute        string  in   Minute of day, as a 2 character string.
! platform       string  in   Name of satellite platform.
! sensor         string  in   Name of sensor.
! path           string  in   Name of file to create.
! type           integer in   File type to produce. 1: LWRTM; 2: SWRTM; 3: PRTM.
! preproc_dims   struct  in   Summary of preprocessing grid definitions.
! imager_angles  struct  in   Summary of satellite geometry.
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed
! use_chunking   logic   in   T: Chunk output file; F: Don't.
!
! History:
! 2011/12/11, MJ: Original version.
! 2012/05/15, MJ: modifies file to write pixels in one loop successively one
!    after the other
! 2012/05/24, MJ: adds some commenting.
! 2012/08/02, MJ: adds some more code for writing of RTTOV output to netcdf file.
! 2012/08/02, CP: bug fix in outputting of msi files removed nviews dimension
! 2012/09/20, CP: added solzen and satzen tp prtm output
! 2012/11/29, CP: changed variables names from layers to levels
! 2013/02/26, CP: inserted missing comment for adding noaa platform
! 2013/03/07, CP: added in some diagnostics q and albedo
! 2013/xx/xx, MJ: adds PLATFORMUP variable and output to comply with
!    nomenclature
! 2013/10/14, MJ: fixed bug with writing of albedo and emissivity.
! 2013/11/06, MJ: adds config file to preprocessing output which holds all
!    relevant dimensional information.
! 2013/11/27, MJ: changes output from netcdf3 to netcdf4.
! 2014/01/30, MJ: implements chunking for the large variables which are actually
!    read in later.
! 2014/02/02, GM: adds chunking on/off option and cleans up code.
! 2014/02/02, GM: puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
! 2014/02/02, GM: Changed the nlat x nlon 'unlimited' dimension size to a fixed
!    dimension size.  The 'unlimited' dimension size is not required and results
!    in a significant performance hit.
! 2014/02/03, GM: A small reordering of the variables in the SW RTM output to be
!    consistent with the LW RTM output.
! 2014/02/10, AP: variable renaming
! 2014/03/11, MJ: some modifications for chunking (only used when turned on)
! 2014/03/11, MJ: Commented out chunking completely in routines as I/O
!    performance issues persisted.
! 2014/05/01, GM: Reordered data/time arguments into a logical order.
! 2014/05/26, GM: Fixes/improvements to error reporting.
! 2014/08/01, AP: Remove unused counter fields.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/08/31, GM: Make the global attribute list consistent with CF-1.4.
! 2014/09/02, GM: Replaced use of the derived dimensions id arrays in
!    netcdf_info with local arrays here. There was no reason for them to be in
!    that structure.
! 2014/09/16, GM: Use the nc_def_var routine from the orac_ncdf module in the
!    common library.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_create_rtm(global_atts,cyear,cmonth,cday,chour,cminute, &
     platform,sensor,path,type,preproc_dims,imager_angles,netcdf_info, &
     channel_info,use_chunking,verbose)

   use netcdf

   use channel_structures
   use global_attributes
   use imager_structures
   use orac_ncdf
   use preproc_constants
   use preproc_structures

   implicit none

   ! Input
   type(global_attributes_s),      intent(in)    :: global_atts
   character(len=date_length),     intent(in)    :: cyear
   character(len=date_length),     intent(in)    :: cmonth
   character(len=date_length),     intent(in)    :: cday
   character(len=date_length),     intent(in)    :: chour
   character(len=date_length),     intent(in)    :: cminute
   character(len=platform_length), intent(in)    :: platform
   character(len=sensor_length),   intent(in)    :: sensor
   character(len=*),               intent(in)    :: path
   integer,                        intent(in)    :: type
   type(preproc_dims_s),           intent(in)    :: preproc_dims
   type(imager_angles_s),          intent(in)    :: imager_angles
   type(netcdf_output_info_s),     intent(inout) :: netcdf_info
   type(channel_info_s),           intent(in)    :: channel_info
   logical,                        intent(in)    :: use_chunking
   logical,                        intent(in)    :: verbose

   ! Local
   integer                    :: ierr
   integer                    :: nlon_x_nlat
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer                    :: dimids_1d(1)
   integer                    :: dimids_2d(2)
   integer                    :: dimids_3d(3)
   integer                    :: chunksize1d(1)
   integer                    :: chunksize2d(2)
   integer                    :: chunksize3d(3)


   nlon_x_nlat=(preproc_dims%max_lon-preproc_dims%min_lon+1) * &
        (preproc_dims%max_lat-preproc_dims%min_lat+1)


   ! open lwrtm file
   if (type .eq. NETCDF_OUTPUT_FILE_LWRTM) then

      ctitle='ORAC Preprocessing lwrtm output file'

      ! create file
      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_lwrtm)
      if (ierr.ne.NF90_NOERR) stop 'error: lw creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_lwrtm)


      ! define view dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nviews', &
           imager_angles%nviews, netcdf_info%dimid_v_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nv lw'

      ! define channel dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlw_channels', &
           channel_info%nchannels_lw, netcdf_info%dimid_lw_channels)
      if (ierr.ne.NF90_NOERR) stop 'error: create nchan lw'

      ! define lon dimension for reference
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlon_lwrtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, netcdf_info%dimid_x_lw)
      if (ierr.ne.NF90_NOERR) stop 'error:create x-d'

      ! define lat dimension for reference
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlat_lwrtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, netcdf_info%dimid_y_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d'

      ! define horizontal dimension as lon x lat
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlon_x_nlat_lwrtm', &
           nlon_x_nlat, netcdf_info%dimid_xy_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 2'

      ! define layer dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlayers_lwrtm', &
           preproc_dims%kdim-1, netcdf_info%dimid_layers_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlay lw'

      ! define level dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlevels_lwrtm', &
           preproc_dims%kdim, netcdf_info%dimid_levels_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlev lw'


      dimids_1d(1) = netcdf_info%dimid_lw_channels

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_lwrtm, &
              dimids_1d, &
              'lw_channel_abs_ids', &
              netcdf_info%vid_lw_channel_abs_ids, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_lwrtm, &
              dimids_1d, &
              'lw_channel_instr_ids', &
              netcdf_info%vid_lw_channel_instr_ids, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_1d, &
              'lw_channel_wvl', &
              netcdf_info%vid_lw_channel_wvl, &
              verbose, ierr, &
              fill_value = sreal_fill_value)


      ! combines viewing xy dims
      dimids_2d(1)=netcdf_info%dimid_v_lw
      dimids_2d(2)=netcdf_info%dimid_xy_lw

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize1d(1)=min(nlon_x_nlat,max_chunk_latlon)

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define solar zenith
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_2d, &
              'solza_lw', &
              netcdf_info%vid_solza_lw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define satellite zenith
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_2d, &
              'satza_lw', &
              netcdf_info%vid_satza_lw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define relative azimuth
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_2d, &
              'relazi_lw', &
              netcdf_info%vid_relazi_lw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


      ! set up the combined dimensions for 3D fields (spatial+channel)
      dimids_2d(1)=netcdf_info%dimid_lw_channels
      dimids_2d(2)=netcdf_info%dimid_xy_lw

      if (.not. use_chunking) then
         chunksize2d(1)=channel_info%nchannels_lw
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize2d(1)=1
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define emissivity 3D
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_2d, &
              'emiss_lw', &
              netcdf_info%vid_emiss_lw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


      ! set up the combined dimensions for 4D fields (c-z-xy)
      dimids_3d(1)=netcdf_info%dimid_lw_channels
      dimids_3d(2)=netcdf_info%dimid_levels_lw
      dimids_3d(3)=netcdf_info%dimid_xy_lw

      if (.not. use_chunking) then
         chunksize3d(1)=channel_info%nchannels_lw
         chunksize3d(2)=preproc_dims%kdim
         chunksize3d(3)=nlon_x_nlat
      else
         chunksize3d(1)=1
         chunksize3d(2)=1
         chunksize3d(3)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define tac
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_3d, &
              'tac_lw', &
              netcdf_info%vid_tac_lw, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define tbc
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_3d, &
              'tbc_lw', &
              netcdf_info%vid_tbc_lw, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define rbc_up
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_3d, &
              'rbc_up_lw', &
              netcdf_info%vid_rbc_up_lw, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define rac_up
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_3d, &
              'rac_up_lw', &
              netcdf_info%vid_rac_up_lw, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define rac_down
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_3d, &
              'rac_down_lw', &
              netcdf_info%vid_rac_down_lw, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


   ! open swrtm file
   else if (type .eq. NETCDF_OUTPUT_FILE_SWRTM) then

      ctitle='ORAC Preprocessing swrtm output file'


      ! create file
      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_swrtm)
      if (ierr.ne.NF90_NOERR) stop 'error: creating sw file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_swrtm)


      ! define view dimension
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nviews', &
           imager_angles%nviews, netcdf_info%dimid_v_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nv sw'

      ! define channel dimension
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nsw_channels', &
           channel_info%nchannels_sw, netcdf_info%dimid_sw_channels)
      if (ierr.ne.NF90_NOERR) stop 'error: create nchan sw'

      ! define lon dimension for reference
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlon_swrtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, netcdf_info%dimid_x_sw)
      if (ierr.ne.NF90_NOERR) stop 'error:create x-d'

      ! define lat dimension for reference
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlat_swrtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, netcdf_info%dimid_y_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d'

      ! define horizontal dimension as lon x lat
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlon_x_nlat_swrtm', &
           nlon_x_nlat, netcdf_info%dimid_xy_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 2'

      ! define layer dimension
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlayers_swrtm', &
           preproc_dims%kdim-1, netcdf_info%dimid_layers_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlay sw'

      ! define level dimension
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlevels_swrtm', &
           preproc_dims%kdim, netcdf_info%dimid_levels_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlev sw'

      dimids_1d(1) = netcdf_info%dimid_sw_channels

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_swrtm, &
              dimids_1d, &
              'sw_channel_abs_ids', &
              netcdf_info%vid_sw_channel_abs_ids, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_swrtm, &
              dimids_1d, &
              'sw_channel_instr_ids', &
              netcdf_info%vid_sw_channel_instr_ids, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_1d, &
              'sw_channel_wvl', &
              netcdf_info%vid_sw_channel_wvl, &
              verbose, ierr, &
              fill_value = sreal_fill_value)


      ! combines viewing xy dims
      dimids_2d(1)=netcdf_info%dimid_v_sw
      dimids_2d(2)=netcdf_info%dimid_xy_sw

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize1d(1)=min(nlon_x_nlat,max_chunk_latlon)

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define solar zenith
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_2d, &
              'solza_sw', &
              netcdf_info%vid_solza_sw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define satellite zenith
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_2d, &
              'satza_sw', &
              netcdf_info%vid_satza_sw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define relative azimuth
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_2d, &
              'relazi_sw', &
              netcdf_info%vid_relazi_sw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


      ! set up the combined dimensions for 4D fields
      dimids_3d(1)=netcdf_info%dimid_sw_channels
      dimids_3d(2)=netcdf_info%dimid_levels_sw
      dimids_3d(3)=netcdf_info%dimid_xy_sw

      if (.not. use_chunking) then
         chunksize3d(1)=channel_info%nchannels_sw
         chunksize3d(2)=preproc_dims%kdim
         chunksize3d(3)=nlon_x_nlat
      else
         chunksize3d(1)=1
         chunksize3d(2)=1
         chunksize3d(3)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define tac
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_3d, &
              'tac_sw', &
              netcdf_info%vid_tac_sw, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define tbc
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_3d, &
              'tbc_sw', &
              netcdf_info%vid_tbc_sw, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


   ! open prtm file
   else if (type .eq. NETCDF_OUTPUT_FILE_PRTM) then

      ctitle='ORAC Preprocessing prtm output file'


      ! create file
      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_prtm)
      if (ierr.ne.NF90_NOERR) stop 'error: creating p file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_prtm)


      ! define lon dimension for reference
      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlon_prtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, netcdf_info%dimid_x_pw)
      if (ierr.ne.NF90_NOERR) stop 'error:create x-d'

      ! define lat dimension for reference
      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlat_prtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, netcdf_info%dimid_y_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d'

      ! define horizontal dimension as lon x lat
      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlon_x_nlat_prtm', &
           nlon_x_nlat, netcdf_info%dimid_xy_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 2'

      ! define layer dimension
      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlayers_prtm', &
           preproc_dims%kdim-1, netcdf_info%dimid_layers_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlay pw'

      ! define level dimension
      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlevels_prtm', &
           preproc_dims%kdim, netcdf_info%dimid_levels_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlev pw'


      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat
      else
         chunksize1d(1)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      dimids_1d(1) = netcdf_info%dimid_xy_pw

      ! define longitude variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_1d, &
              'lon_pw', &
              netcdf_info%vid_lon_pw, &
              verbose, ierr, &
              chunksizes = chunksize1d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define latitude variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_1d, &
              'lat_pw', &
              netcdf_info%vid_lat_pw, &
              verbose, ierr, &
              chunksizes = chunksize1d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define skint variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_1d, &
              'skint_pw', &
              netcdf_info%vid_skint_pw, &
              verbose, ierr, &
              chunksizes = chunksize1d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define exp(lnsp) variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_1d, &
              'explnsp_pw', &
              netcdf_info%vid_lnsp_pw, &
              verbose, ierr, &
              chunksizes = chunksize1d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define exp(lsf) variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_1d, &
              'lsf_pw', &
              netcdf_info%vid_lsf_pw, &
              verbose, ierr, &
              chunksizes = chunksize1d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define satzen variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_1d, &
              'satzen_pw', &
              netcdf_info%vid_satzen_pw, &
              verbose, ierr, &
              chunksizes = chunksize1d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define solzen variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_1d, &
              'solzen_pw', &
              netcdf_info%vid_solzen_pw, &
              verbose, ierr, &
              chunksizes = chunksize1d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


      ! set up the combined dimensions for 3D fields
      dimids_2d(1)=netcdf_info%dimid_levels_pw
      dimids_2d(2)=netcdf_info%dimid_xy_pw

      if (.not. use_chunking) then
         chunksize2d(1)=preproc_dims%kdim
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize2d(1)=1
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define pressure profile at level centers as variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_2d, &
              'pprofile_lev', &
              netcdf_info%vid_pprofile_lev_pw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define temperature profile at lever centers as variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_2d, &
              'tprofile_lev', &
              netcdf_info%vid_tprofile_lev_pw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define geopotential height profile at lever centers as variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_prtm, &
              dimids_2d, &
              'gphprofile_lev', &
              netcdf_info%vid_gphprofile_lev_pw, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)
   end if


   ! set up attributes common to all output files
   if (type .eq. NETCDF_OUTPUT_FILE_PRTM)  ncid=netcdf_info%ncid_prtm
   if (type .eq. NETCDF_OUTPUT_FILE_LWRTM) ncid=netcdf_info%ncid_lwrtm
   if (type .eq. NETCDF_OUTPUT_FILE_SWRTM) ncid=netcdf_info%ncid_swrtm

   call netcdf_put_common_attributes(ncid,global_atts,ctitle,platform,sensor, &
        path,cyear,cmonth,cday,chour,cminute)


   ! close definition section
   ierr = nf90_enddef(ncid)
   if (ierr.ne.NF90_NOERR) stop 'error: enddef rtm'


   return

end subroutine netcdf_create_rtm


!-------------------------------------------------------------------------------
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! global_atts    struct  in   Structure detailing NCDF header contents.
! cyear          string  in   Year, as a 4 character string.
! cmonth         string  in   Month of year, as a 2 character string.
! cday           string  in   Day of month, as a 2 character string.
! chour          string  in   Hour of day, as a 2 character string.
! cminute        string  in   Minute of day, as a 2 character string.
! platform       string  in   Name of satellite platform.
! sensor         string  in   Name of sensor.
! path           string  in   Name of file to create.
! type           integer in   File type to produce. 1: MSI; 2: CF; 3: LSF;
!                             4: GEO; 5: LOC; 6: ALB; 7: SCAN.
! imager_geolocation
!                struct  in   Summary of pixel positions
! imager_angles  struct  in   Summary of satellite geometry.
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed.
! use_chunking   logic   in   T: Chunk output file; F: Don't.
! include_full_brdf
!                logic   in   T: Output BRDF fields; F: Don't.
!
! History:
! 2012/05/31, MJ: initial routine version.
! 2012/07/04, CP: removed nviews dimension of data
! 2014/02/02, GM: adds chunking on/off option and cleans up code.
! 2014/02/02, GM: puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
! 2014/09/16, GM: Use the nc_def_var routine from the orac_ncdf module in the
!    common library.
!
!-------------------------------------------------------------------------------

subroutine netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute, &
   platform,sensor,path,type,imager_geolocation,imager_angles,netcdf_info, &
   channel_info,use_chunking,include_full_brdf,verbose)

   use netcdf

   use channel_structures
   use global_attributes
   use imager_structures
   use orac_ncdf
   use preproc_constants

   implicit none

   ! Input
   type(global_attributes_s),      intent(in)    :: global_atts
   character(len=date_length),     intent(in)    :: cyear
   character(len=date_length),     intent(in)    :: cmonth
   character(len=date_length),     intent(in)    :: cday
   character(len=date_length),     intent(in)    :: chour
   character(len=date_length),     intent(in)    :: cminute
   character(len=platform_length), intent(in)    :: platform
   character(len=sensor_length),   intent(in)    :: sensor
   character(len=*),               intent(in)    :: path
   integer,                        intent(in)    :: type
   type(imager_geolocation_s),     intent(in)    :: imager_geolocation
   type(imager_angles_s),          intent(in)    :: imager_angles
   type(netcdf_output_info_s),     intent(inout) :: netcdf_info
   type(channel_info_s),           intent(in)    :: channel_info
   logical,                        intent(in)    :: use_chunking
   logical,                        intent(in)    :: include_full_brdf
   logical,                        intent(in)    :: verbose

   ! Local
   integer                    :: ierr
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer                    :: dimids_1d(1)
   integer                    :: dimids_2d(2)
   integer                    :: dimids_3d(3)
   integer                    :: chunksize2d(2)
   integer                    :: chunksize3d(3)


   ! open alb file
   if (type .eq. NETCDF_OUTPUT_FILE_ABL) then

      ctitle='ORAC Preprocessing alb output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: alb creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_alb)


      ! define channel dimension for albedo
      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'nc_alb', &
           channel_info%nchannels_sw, netcdf_info%dimid_c_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: create c-d alb'

      ! define channel dimension for emissivity
      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'nc_emis', &
           channel_info%nchannels_lw, netcdf_info%dimid_c_emis)
      if (ierr.ne.NF90_NOERR) stop 'error: create c-d emis'

      ! define x dimension
      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'nx_alb', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%dimid_x_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d alb'

      ! define y dimension
      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'ny_alb', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%dimid_y_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d alb'


      dimids_1d(1) = netcdf_info%dimid_c_alb

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_alb, &
              dimids_1d, &
              'alb_abs_ch_numbers', &
              netcdf_info%vid_alb_abs_ch_numbers, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      dimids_1d(1) = netcdf_info%dimid_c_emis

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_alb, &
              dimids_1d, &
              'emis_abs_ch_numbers', &
              netcdf_info%vid_emis_abs_ch_numbers, &
              verbose, ierr, &
              fill_value = lint_fill_value)


      dimids_3d(1)=netcdf_info%dimid_x_alb
      dimids_3d(2)=netcdf_info%dimid_y_alb
      dimids_3d(3)=netcdf_info%dimid_c_alb

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=channel_info%nchannels_sw
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      end if

      ! define alb variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_alb, &
              dimids_3d, &
              'alb_data', &
              netcdf_info%vid_alb_data, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


      dimids_3d(1)=netcdf_info%dimid_x_alb
      dimids_3d(2)=netcdf_info%dimid_y_alb
      dimids_3d(3)=netcdf_info%dimid_c_emis

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=channel_info%nchannels_lw
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      end if

      ! define emis variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_alb, &
              dimids_3d, &
              'emis_data', &
              netcdf_info%vid_emis_data, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


      if (include_full_brdf) then
         dimids_3d(1)=netcdf_info%dimid_x_alb
         dimids_3d(2)=netcdf_info%dimid_y_alb
         dimids_3d(3)=netcdf_info%dimid_c_alb

         if (.not. use_chunking) then
            chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
            chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
            chunksize3d(3)=channel_info%nchannels_sw
         else
            chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
            chunksize3d(2)=imager_geolocation%ny
            chunksize3d(3)=1
         end if

         call nc_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_0v_data', &
                 netcdf_info%vid_rho_0v_data, &
                 verbose, ierr, &
                 chunksizes = chunksize3d, &
                 deflate_level = deflate_level_sreal, &
                 shuffle = shuffle_sreal, &
                 fill_value = sreal_fill_value)

         call nc_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_0d_data', &
                 netcdf_info%vid_rho_0d_data, &
                 verbose, ierr, &
                 chunksizes = chunksize3d, &
                 deflate_level = deflate_level_sreal, &
                 shuffle = shuffle_sreal, &
                 fill_value = sreal_fill_value)

         call nc_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_dv_data', &
                 netcdf_info%vid_rho_dv_data, &
                 verbose, ierr, &
                 chunksizes = chunksize3d, &
                 deflate_level = deflate_level_sreal, &
                 shuffle = shuffle_sreal, &
                 fill_value = sreal_fill_value)

         call nc_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_dd_data', &
                 netcdf_info%vid_rho_dd_data, &
                 verbose, ierr, &
                 chunksizes = chunksize3d, &
                 deflate_level = deflate_level_sreal, &
                 shuffle = shuffle_sreal, &
                 fill_value = sreal_fill_value)
      end if


   ! open clf file
   else if (type .eq. NETCDF_OUTPUT_FILE_CLF) then

      ctitle='ORAC Preprocessing cf output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_clf)
      if (ierr.ne.NF90_NOERR) stop 'error: cf creating file'


      !start defining things
      ierr = nf90_redef(netcdf_info%ncid_clf)


      ! define x dimension
      ierr = nf90_def_dim(netcdf_info%ncid_clf, 'nx_cf', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%dimid_x_cf)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d cf'

      ! define y dimension
      ierr = nf90_def_dim(netcdf_info%ncid_clf, 'ny_cf', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%dimid_y_cf)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d cf'


      dimids_2d(1)=netcdf_info%dimid_x_cf
      dimids_2d(2)=netcdf_info%dimid_y_cf

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define cf variable
      call nc_def_var_byte_packed_byte( &
              netcdf_info%ncid_clf, &
              dimids_2d, &
              'cflag', &
              netcdf_info%vid_cflag, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_byte, &
              shuffle = shuffle_byte, &
              fill_value = byte_fill_value)


   ! open geo file
   else if (type .eq. NETCDF_OUTPUT_FILE_GEO) then

      ctitle='ORAC Preprocessing geo output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: geo creating file'


      !start defining things
      ierr = nf90_redef(netcdf_info%ncid_geo)


      ! define view dimension
      ierr = nf90_def_dim(netcdf_info%ncid_geo, 'nv_geo', &
           imager_angles%nviews, netcdf_info%dimid_v_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: create v-d geo'

      ! define x dimension
      ierr = nf90_def_dim(netcdf_info%ncid_geo, 'nx_geo', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%dimid_x_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d geo'

      ! define y dimension
      ierr = nf90_def_dim(netcdf_info%ncid_geo, 'ny_geo', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%dimid_y_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d geo'


      dimids_3d(1)=netcdf_info%dimid_x_geo
      dimids_3d(2)=netcdf_info%dimid_y_geo
      dimids_3d(3)=netcdf_info%dimid_v_geo

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=imager_angles%nviews
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      end if

      ! define solzen variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_geo, &
              dimids_3d, &
              'solzen', &
              netcdf_info%vid_solzen, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define satzen variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_geo, &
              dimids_3d, &
              'satzen', &
              netcdf_info%vid_satzen, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define solaz variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_geo, &
              dimids_3d, &
              'solaz', &
              netcdf_info%vid_solaz, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define relazi variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_geo, &
              dimids_3d, &
              'relazi', &
              netcdf_info%vid_relazi, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


   ! open loc file
   else if (type .eq. NETCDF_OUTPUT_FILE_LOC) then

      ctitle='ORAC Preprocessing loc output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_loc)
      if (ierr.ne.NF90_NOERR) stop 'error: loc creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_loc)


      ! define x dimension
      ierr = nf90_def_dim(netcdf_info%ncid_loc, 'nx_loc', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%dimid_x_loc)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d loc'

      ! define y dimension
      ierr = nf90_def_dim(netcdf_info%ncid_loc, 'ny_loc', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%dimid_y_loc)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d loc'


      dimids_2d(1)=netcdf_info%dimid_x_loc
      dimids_2d(2)=netcdf_info%dimid_y_loc

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define lat variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_loc, &
              dimids_2d, &
              'lat', &
              netcdf_info%vid_lat, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)

      ! define lon variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_loc, &
              dimids_2d, &
              'lon', &
              netcdf_info%vid_lon, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


   ! open lsf file
   else if (type .eq. NETCDF_OUTPUT_FILE_LSF) then
      ctitle='ORAC Preprocessing lsf output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_lsf)
      if (ierr.ne.NF90_NOERR) stop 'error: lsf creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_lsf)


      ! define x dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lsf, 'nx_lsf', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%dimid_x_lsf)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d lsf'

      ! define y dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lsf, 'ny_lsf', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%dimid_y_lsf)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d lsf'


      dimids_2d(1)=netcdf_info%dimid_x_lsf
      dimids_2d(2)=netcdf_info%dimid_y_lsf

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define cf variable
      call nc_def_var_byte_packed_byte( &
              netcdf_info%ncid_lsf, &
              dimids_2d, &
              'lsflag', &
              netcdf_info%vid_lsflag, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_byte, &
              shuffle = shuffle_byte, &
              fill_value = byte_fill_value)


   ! open msi file
   else if (type .eq. NETCDF_OUTPUT_FILE_MSI) then

      ctitle='ORAC Preprocessing msi output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: msi creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_msi)

if (.false.) then
      ! define view dimension
      ierr = nf90_def_dim(netcdf_info%ncid_msi, 'nv_msi', &
           imager_angles%nviews, netcdf_info%dimid_v_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: create v-d msi'
endif
      ! define channel dimension
      ierr = nf90_def_dim(netcdf_info%ncid_msi, 'nc_msi', &
           channel_info%nchannels_total, netcdf_info%dimid_c_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: create c-d msi'

      ! define x dimension
      ierr = nf90_def_dim(netcdf_info%ncid_msi, 'nx_msi', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%dimid_x_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d msi'

      ! define y dimension
      ierr = nf90_def_dim(netcdf_info%ncid_msi, 'ny_msi', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%dimid_y_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d msi'


      ! define some channel variables
      dimids_1d(1) = netcdf_info%dimid_c_msi

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_msi, &
              dimids_1d, &
              'msi_instr_ch_numbers', &
              netcdf_info%vid_msi_instr_ch_numbers, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_msi, &
              dimids_1d, &
              'msi_abs_ch_numbers', &
              netcdf_info%vid_msi_abs_ch_numbers, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_msi, &
              dimids_1d, &
              'msi_abs_ch_wl', &
              netcdf_info%vid_msi_abs_ch_wl, &
              verbose, ierr, &
              fill_value = sreal_fill_value)

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_msi, &
              dimids_1d, &
              'msi_ch_swflag', &
              netcdf_info%vid_msi_ch_swflag, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_msi, &
              dimids_1d, &
              'msi_ch_lwflag', &
              netcdf_info%vid_msi_ch_lwflag, &
              verbose, ierr, &
              fill_value = lint_fill_value)

      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_msi, &
              dimids_1d, &
              'msi_ch_procflag', &
              netcdf_info%vid_msi_ch_procflag, &
              verbose, ierr, &
              fill_value = lint_fill_value)


      dimids_2d(1)=netcdf_info%dimid_x_msi
      dimids_2d(2)=netcdf_info%dimid_y_msi

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define time variable
      call nc_def_var_double_packed_double( &
              netcdf_info%ncid_msi, &
              dimids_2d, &
              'time_data', &
              netcdf_info%vid_time, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_dreal, &
              shuffle = shuffle_dreal, &
              fill_value = dreal_fill_value)


      dimids_3d(1)=netcdf_info%dimid_x_msi
      dimids_3d(2)=netcdf_info%dimid_y_msi
      dimids_3d(3)=netcdf_info%dimid_c_msi

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=channel_info%nchannels_total
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      end if

      ! define msi variable
      call nc_def_var_float_packed_float( &
              netcdf_info%ncid_msi, &
              dimids_3d, &
              'msi_data', &
              netcdf_info%vid_msi_data, &
              verbose, ierr, &
              chunksizes = chunksize3d, &
              deflate_level = deflate_level_sreal, &
              shuffle = shuffle_sreal, &
              fill_value = sreal_fill_value)


   ! open uv file
   else if (type .eq. NETCDF_OUTPUT_FILE_UV) then

      ctitle='ORAC Preprocessing scan output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_scan)
      if (ierr.ne.NF90_NOERR) stop 'error: scan creating file'


      !start defining things
      ierr = nf90_redef(netcdf_info%ncid_scan)


      ! define x dimension
      ierr = nf90_def_dim(netcdf_info%ncid_scan, 'nx_scan', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%dimid_x_scan)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d scan'

      ! define y dimension
      ierr = nf90_def_dim(netcdf_info%ncid_scan, 'ny_scan', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%dimid_y_scan)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d scan'


      dimids_2d(1)=netcdf_info%dimid_x_scan
      dimids_2d(2)=netcdf_info%dimid_y_scan

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define u variable
      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_scan, &
              dimids_2d, &
              'uscan', &
              netcdf_info%vid_uscan, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_lint, &
              shuffle = shuffle_lint, &
              fill_value = lint_fill_value)

      ! define v variable
      call nc_def_var_long_packed_long( &
              netcdf_info%ncid_scan, &
              dimids_2d, &
              'vscan', &
              netcdf_info%vid_vscan, &
              verbose, ierr, &
              chunksizes = chunksize2d, &
              deflate_level = deflate_level_lint, &
              shuffle = shuffle_lint, &
              fill_value = lint_fill_value)
   endif

   ! set up attributes common to all output files
   if (type .eq. NETCDF_OUTPUT_FILE_ABL) ncid=netcdf_info%ncid_alb
   if (type .eq. NETCDF_OUTPUT_FILE_CLF) ncid=netcdf_info%ncid_clf
   if (type .eq. NETCDF_OUTPUT_FILE_GEO) ncid=netcdf_info%ncid_geo
   if (type .eq. NETCDF_OUTPUT_FILE_LOC) ncid=netcdf_info%ncid_loc
   if (type .eq. NETCDF_OUTPUT_FILE_LSF) ncid=netcdf_info%ncid_lsf
   if (type .eq. NETCDF_OUTPUT_FILE_MSI) ncid=netcdf_info%ncid_msi
   if (type .eq. NETCDF_OUTPUT_FILE_UV)  ncid=netcdf_info%ncid_scan

   call netcdf_put_common_attributes(ncid,global_atts,ctitle,platform,sensor, &
        path,cyear,cmonth,cday,chour,cminute)


   ! close definition section
   ierr = nf90_enddef(ncid)
   if (ierr.ne.NF90_NOERR) stop 'error: enddef swath'


   return

end subroutine netcdf_create_swath

!-------------------------------------------------------------------------------
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! global_atts    struct  in   Structure detailing NCDF header contents.
! cyear          string  in   Year, as a 4 character string.
! cmonth         string  in   Month of year, as a 2 character string.
! cday           string  in   Day of month, as a 2 character string.
! chour          string  in   Hour of day, as a 2 character string.
! cminute        string  in   Minute of day, as a 2 character string.
! platform       string  in   Name of satellite platform.
! sensor         string  in   Name of sensor.
! path           string  in   Name of file to create.
! preproc_dims   struct  in   Summary of preprocessing grid definitions.
! imager_geolocation
!                struct  in   Summary of pixel positions
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed.
!
! History:
! 2013/11/06, MJ: initial routine version
! 2014/02/02, GM: adds chunking on/off option and cleans up code.
! 2014/02/02, GM: puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
! 2014/09/16, GM: Use the nc_def_var routine from the orac_ncdf module in the
!    common library.
!
!-------------------------------------------------------------------------------

subroutine netcdf_create_config(global_atts,cyear,cmonth,cday,chour,cminute, &
     platform,sensor,path,preproc_dims,imager_geolocation,netcdf_info, &
     channel_info,verbose)

   use netcdf

   use channel_structures
   use global_attributes
   use imager_structures
   use orac_ncdf
   use preproc_constants
   use preproc_structures

   implicit none

   ! Input
   type(global_attributes_s),      intent(in)    :: global_atts
   character(len=date_length),     intent(in)    :: cyear
   character(len=date_length),     intent(in)    :: cmonth
   character(len=date_length),     intent(in)    :: cday
   character(len=date_length),     intent(in)    :: chour
   character(len=date_length),     intent(in)    :: cminute
   character(len=platform_length), intent(in)    :: platform
   character(len=sensor_length),   intent(in)    :: sensor
   character(len=*),               intent(in)    :: path
   type(preproc_dims_s),           intent(in)    :: preproc_dims
   type(imager_geolocation_s),     intent(in)    :: imager_geolocation
   type(netcdf_output_info_s),     intent(inout) :: netcdf_info
   type(channel_info_s),           intent(in)    :: channel_info
   logical,                        intent(in)    :: verbose

   ! Local
   integer                    :: ierr
   integer                    :: nlon_x_nlat
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer                    :: dimids_1d(1)


   ctitle='ORAC Preprocessing config file'


   ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
        netcdf_info%ncid_config)
   if (ierr.ne.NF90_NOERR) stop 'error: config creating file'


   ! start defining things
   ierr = nf90_redef(netcdf_info%ncid_config)

if (.false.) then
   ! define view dimension
!  ierr = nf90_def_dim(netcdf_info%ncid_config, 'nv_conf', &
!       imager_angles%nviews, netcdf_info%dimid_v_config)
!  if (ierr.ne.NF90_NOERR) stop 'error: create v-d conf'
endif
   ! define channel dimension
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nc_conf', &
        channel_info%nchannels_total, netcdf_info%dimid_c_config)
   if (ierr.ne.NF90_NOERR) stop 'error: create c-d conf'

   ! define channel dimension for albedo
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nc_alb', &
        channel_info%nchannels_sw, netcdf_info%dimid_c_config_alb)
   if (ierr.ne.NF90_NOERR) stop 'error: create c-d alb'

   ! define channel dimension for emissivity
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nc_emis', &
        channel_info%nchannels_lw, netcdf_info%dimid_c_config_emis)
   if (ierr.ne.NF90_NOERR) stop 'error: create c-d emis'

if (.false.) then
   ! define lon dimension for reference
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlon_conf', &
        preproc_dims%max_lon-preproc_dims%min_lon+1, netcdf_info%dimid_x_lw)
   if (ierr.ne.NF90_NOERR) stop 'error:create x-d'

   ! define lat dimension for reference
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlat_conf', &
        preproc_dims%max_lat-preproc_dims%min_lat+1, netcdf_info%dimid_y_lw)
   if (ierr.ne.NF90_NOERR) stop 'error: create y-d'

   ! define horizontal dimension as lon x lat
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlon_x_nlat_conf', &
        nlon_x_nlat, netcdf_info%dimid_xy_lw)
   if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 2'

   ! define layer dimension
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlayers_conf', &
        preproc_dims%kdim-1, netcdf_info%dimid_layers_lw)
   if (ierr.ne.NF90_NOERR) stop 'error: create nlay lw'

   ! define level dimension
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlevels_conf', &
        preproc_dims%kdim, netcdf_info%dimid_levels_lw)
   if (ierr.ne.NF90_NOERR) stop 'error: create nlev lw'
endif

   ! define x dimension
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nx_conf', &
        imager_geolocation%endx-imager_geolocation%startx+1, &
        netcdf_info%dimid_x_config)
   if (ierr.ne.NF90_NOERR) stop 'error: create x-d conf'

   ! define y dimension
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'ny_conf', &
        imager_geolocation%endy-imager_geolocation%starty+1, &
        netcdf_info%dimid_y_config)
   if (ierr.ne.NF90_NOERR) stop 'error: create y-d conf'


   ! define some channel variables

   dimids_1d(1) = netcdf_info%dimid_c_config

   call nc_def_var_long_packed_long( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'msi_instr_ch_numbers', &
           netcdf_info%vid_msi_instr_ch_numbers_config, &
           verbose, ierr, &
           fill_value = lint_fill_value)

   call nc_def_var_long_packed_long( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'msi_abs_ch_numbers', &
           netcdf_info%vid_msi_abs_ch_numbers_config, &
           verbose, ierr, &
           fill_value = lint_fill_value)

   call nc_def_var_float_packed_float( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'msi_abs_ch_wl', &
           netcdf_info%vid_msi_abs_ch_wl_config, &
           verbose, ierr, &
           fill_value = sreal_fill_value)

   call nc_def_var_long_packed_long( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'msi_ch_swflag', &
           netcdf_info%vid_msi_ch_swflag_config, &
           verbose, ierr, &
           fill_value = lint_fill_value)

   call nc_def_var_long_packed_long( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'msi_ch_lwflag', &
           netcdf_info%vid_msi_ch_lwflag_config, &
           verbose, ierr, &
           fill_value = lint_fill_value)

   call nc_def_var_long_packed_long( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'msi_ch_procflag', &
           netcdf_info%vid_msi_ch_procflag_config, &
           verbose, ierr, &
           fill_value = lint_fill_value)

   dimids_1d(1) = netcdf_info%dimid_c_config_alb

   call nc_def_var_long_packed_long( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'alb_abs_ch_numbers', &
           netcdf_info%vid_alb_abs_ch_numbers_config, &
           verbose, ierr, &
           fill_value = lint_fill_value)

   dimids_1d(1) = netcdf_info%dimid_c_config_emis

   call nc_def_var_long_packed_long( &
           netcdf_info%ncid_config, &
           dimids_1d, &
           'emis_abs_ch_numbers', &
           netcdf_info%vid_emis_abs_ch_numbers_config, &
           verbose, ierr, &
           fill_value = lint_fill_value)


   ncid=netcdf_info%ncid_config

   ! set up attributes common to all output files
   call netcdf_put_common_attributes(ncid,global_atts,ctitle,platform,sensor, &
        path, cyear,cmonth,cday,chour,cminute)


   ! close definition section
   ierr = nf90_enddef(ncid)
   if (ierr.ne.NF90_NOERR) stop 'error: enddef swath'


   return

end subroutine netcdf_create_config


subroutine netcdf_put_common_attributes(ncid,global_atts,title,platform,sensor, &
                                        path,cyear,cmonth,cday,chour,cminute)

   use netcdf

   use global_attributes
   use orac_ncdf

   implicit none

   integer,                        intent(in) :: ncid
   type(global_attributes_s),      intent(in) :: global_atts
   character(len=file_length),     intent(in) :: title
   character(len=platform_length), intent(in) :: platform
   character(len=sensor_length),   intent(in) :: sensor
   character(len=*),               intent(in) :: path
   character(len=date_length),     intent(in) :: cyear
   character(len=date_length),     intent(in) :: cmonth
   character(len=date_length),     intent(in) :: cday
   character(len=date_length),     intent(in) :: chour
   character(len=date_length),     intent(in) :: cminute

   character(len=platform_length) :: PLATFORM_UPPER_CASE
   integer                        :: position,length
   type(global_attributes_s)      :: global_atts2

   global_atts2 = global_atts

   global_atts2%title  = trim(title)
   global_atts2%source = 'source!!!'

   position=index(trim(path),'/',back=.true.)
   length=len_trim(path)
   global_atts2%File_Name    = trim(path(position+1:length))

   global_atts2%Product_Name = 'Product_Name!!!'

   global_atts2%Product_Date = trim(cyear)//trim(cmonth)//trim(cday)// &
                               trim(chour)//trim(cminute)

   PLATFORM_UPPER_CASE=platform
   if (platform(1:4) .eq. 'noaa') PLATFORM_UPPER_CASE(1:4)='NOAA'
   global_atts2%platform = trim(PLATFORM_UPPER_CASE)
   global_atts2%sensor   = trim(sensor)

   global_atts2%AATSR_Processing_Version = ' '
   if (sensor .eq. 'ATSR' .or. sensor .eq. 'AATSR') then
      global_atts2%AATSR_Processing_Version = '3.01'
   endif

   call nc_put_common_attributes(ncid, global_atts2)

end subroutine netcdf_put_common_attributes
