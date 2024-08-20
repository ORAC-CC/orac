!-------------------------------------------------------------------------------
! Name: netcdf_output_create_file.F90
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
! source_atts    struct  in   Structure detailing source attributes
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
! 2014/09/10, AP: Tidying. Removed angles from RTM outputs. Now outputs RTTOV
!    over two spatial dimensions rather than one, which come first (as used in
!    the main processor).
! 2014/09/16, GM: Use the nc_def_var routine from the orac_ncdf module in the
!    common library.
! 2014/09/19, AP: Rearranged the rtm variable dimensions to work with the new
!    RTTOV driver.
! 2014/09/28, GM: Fixed a significant performance regression from the new RTTOV
!    driver by rearranging the rtm variable dimensions and fixing the chunking
!    sizes for the use_chunking = .false. case.
! 2014/09/28, GM: Removed the option for chunking. Chunking will not help
!    any more as the variables are written by the preprocessor and read by the
!    main processor in the order in which they are stored.
! 2014/09/28, GM: Remove layer dimensions as they were not used any more.
! 2014/09/28, GM: Make the rest of the error messaging consistent with the new
!    format.
! 2014/10/23, OS: Added various variables due to implementation of USGS data,
!    Pavolonies cloud typing, and NN cloud mask: cldtype, cldmask, cccot_pre,
!    lusflag, dem, nisemask
! 2014/12/01, CP: Added in source attributes.
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/01/30, AP: Eliminate skint, sp, and lsf field for PRTM.
!    Remove uscan and vscan as unnecessary.
! 2015/07/03, OS: added cldmask_uncertainty
! 2015/07/23, GM: Added specific humidity and ozone PRTM fields.
! 2016/03/31, GM: Changes to support processing only SW or only LW channels.
! 2017/02/07, SP: Added support for NOAA GFS atmosphere data (ExtWork)
! 2017/04/11, SP: Added nwp_flag=6, for working with GFS analysis files.
! 2017/06/21, OS: Added ANN phase variables
! 2018/04/26, SP: Add code to save satellite azimuth
! 2018/04/29, SP: Add cloud emissivity support for ECMWF profiles (ExtWork)
! 2018/07/18, DE: Add tropoopause temperature
! 2018/11/05, SP: Add CAPE
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_create_rtm(global_atts, source_atts, cyear, cmonth, cday, chour, &
     cminute, platform, sensor, path, type, preproc_dims, netcdf_info, channel_info, &
     nwp_flag, verbose)

   use netcdf

   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use orac_ncdf_m
   use preproc_constants_m
   use preproc_structures_m
   use source_attributes_m

   implicit none

   ! Input
   type(global_attributes_t),  intent(in)    :: global_atts
   type(source_attributes_t),  intent(in)    :: source_atts
   character(len=*),           intent(in)    :: cyear
   character(len=*),           intent(in)    :: cmonth
   character(len=*),           intent(in)    :: cday
   character(len=*),           intent(in)    :: chour
   character(len=*),           intent(in)    :: cminute
   character(len=*),           intent(in)    :: platform
   character(len=*),           intent(in)    :: sensor
   character(len=*),           intent(in)    :: path
   integer,                    intent(in)    :: type
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(netcdf_output_info_t), intent(inout) :: netcdf_info
   type(channel_info_t),       intent(in)    :: channel_info
   integer,                    intent(in)    :: nwp_flag
   logical,                    intent(in)    :: verbose

   ! Local
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer                    :: dimids_1d(1)
!  integer                    :: dimids_2d(2)
   integer                    :: dimids_3d(3)
   integer                    :: dimids_4d(4)
   integer(lint)              :: nlon, nlat, kdim


   nlon = preproc_dims%max_lon-preproc_dims%min_lon+1
   nlat = preproc_dims%max_lat-preproc_dims%min_lat+1


   ! Set number of vertical levels/layers here, as GFS is different to ECMWF
   kdim = preproc_dims%kdim+1
   if (nwp_flag .eq. 0) kdim = kdim-1

   if (type .eq. NETCDF_OUTPUT_FILE_LWRTM) then

      ctitle = 'ORAC Preprocessing lwrtm output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_lwrtm) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_rtm(1), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      if (channel_info%nchannels_lw .ne. 0) then
         ! define dimensions
         if (nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlat_rtm', &
                          nlat, netcdf_info%dimid_y_lw) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(1), nf90_create(), ' // &
                 'dimension name: nlat_rtm'
            stop error_stop_code
         end if

         if (nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlon_rtm', &
                          nlon, netcdf_info%dimid_x_lw) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(1), nf90_create(), ' // &
                 'dimension name: nlon_rtm'
            stop error_stop_code
         end if

         if (nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlevels_rtm', kdim, &
                          netcdf_info%dimid_levels_lw) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(1), nf90_create(), ' // &
                 'dimension name: nlevels_rtm'
            stop error_stop_code
         end if

         if (nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlw_channels', &
                          channel_info%nchannels_lw, &
                          netcdf_info%dimid_lw_channels) &
              .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(1), nf90_create(), ' // &
                 'dimension name: nlw_channels'
            stop error_stop_code
         end if


         ! define 1-D variables
         dimids_1d(1) = netcdf_info%dimid_lw_channels

         call ncdf_def_var_long_packed_long( &
              netcdf_info%ncid_lwrtm, &
              dimids_1d, &
              'lw_channel_abs_ids', &
              netcdf_info%vid_lw_channel_abs_ids, &
              verbose, &
              fill_value = lint_fill_value)

         call ncdf_def_var_long_packed_long( &
              netcdf_info%ncid_lwrtm, &
              dimids_1d, &
              'lw_channel_instr_ids', &
              netcdf_info%vid_lw_channel_instr_ids, &
              verbose, &
              fill_value = lint_fill_value)

         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_1d, &
              'lw_channel_wvl', &
              netcdf_info%vid_lw_channel_wvl, &
              verbose, &
              fill_value = sreal_fill_value)


         ! define 3-D variables
         dimids_3d(1) = netcdf_info%dimid_lw_channels
         dimids_3d(2) = netcdf_info%dimid_x_lw
         dimids_3d(3) = netcdf_info%dimid_y_lw

         ! define emiss_lw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_3d, &
              'emiss_lw', &
              netcdf_info%vid_emiss_lw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)


         ! define 4-D variables
         dimids_4d(1) = netcdf_info%dimid_lw_channels
         dimids_4d(2) = netcdf_info%dimid_levels_lw
         dimids_4d(3) = netcdf_info%dimid_x_lw
         dimids_4d(4) = netcdf_info%dimid_y_lw

         ! define tac_lw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_4d, &
              'tac_lw', &
              netcdf_info%vid_tac_lw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

         ! define tbc_lw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_4d, &
              'tbc_lw', &
              netcdf_info%vid_tbc_lw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

         ! define rbc_up_lw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_4d, &
              'rbc_up_lw', &
              netcdf_info%vid_rbc_up_lw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

         ! define rac_up_lw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_4d, &
              'rac_up_lw', &
              netcdf_info%vid_rac_up_lw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

         ! define rac_down_lw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_lwrtm, &
              dimids_4d, &
              'rac_down_lw', &
              netcdf_info%vid_rac_down_lw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
      end if


   else if (type .eq. NETCDF_OUTPUT_FILE_SWRTM) then

      ctitle = 'ORAC Preprocessing swrtm output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_swrtm) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_rtm(2), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      if (channel_info%nchannels_sw .ne. 0) then
         ! define dimensions
         if (nf90_def_dim(netcdf_info%ncid_swrtm, 'nlat_rtm', &
                          nlat, netcdf_info%dimid_y_sw) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(2), nf90_create(), ' // &
                 'dimension name: nlat_rtm'
            stop error_stop_code
         end if

         if (nf90_def_dim(netcdf_info%ncid_swrtm, 'nlon_rtm', &
                          nlon, netcdf_info%dimid_x_sw) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(2), nf90_create(), ' // &
                 'dimension name: nlon_rtm'
            stop error_stop_code
         end if

         if (nf90_def_dim(netcdf_info%ncid_swrtm, 'nlevels_rtm', kdim, &
                          netcdf_info%dimid_levels_sw) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(2), nf90_create(), ' // &
                 'dimension name: nlevels_rtm'
            stop error_stop_code
         end if

         if (nf90_def_dim(netcdf_info%ncid_swrtm, 'nsw_channels', &
                          channel_info%nchannels_sw, &
                          netcdf_info%dimid_sw_channels) &
              .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_rtm(2), nf90_create(), ' // &
                 'dimension name: nsw_channels'
            stop error_stop_code
         end if


         ! define 1-D variables
         dimids_1d(1) = netcdf_info%dimid_sw_channels

         call ncdf_def_var_long_packed_long( &
              netcdf_info%ncid_swrtm, &
              dimids_1d, &
              'sw_channel_abs_ids', &
              netcdf_info%vid_sw_channel_abs_ids, &
              verbose, &
              fill_value = lint_fill_value)

         call ncdf_def_var_long_packed_long( &
              netcdf_info%ncid_swrtm, &
              dimids_1d, &
              'sw_channel_instr_ids', &
              netcdf_info%vid_sw_channel_instr_ids, &
              verbose, &
              fill_value = lint_fill_value)

         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_1d, &
              'sw_channel_wvl', &
              netcdf_info%vid_sw_channel_wvl, &
              verbose, &
              fill_value = sreal_fill_value)


         ! define 4-D variables
         dimids_4d(1) = netcdf_info%dimid_sw_channels
         dimids_4d(2) = netcdf_info%dimid_levels_sw
         dimids_4d(3) = netcdf_info%dimid_x_sw
         dimids_4d(4) = netcdf_info%dimid_y_sw

         ! define tac_sw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_4d, &
              'tac_sw', &
              netcdf_info%vid_tac_sw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

         ! define tbc_sw
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_swrtm, &
              dimids_4d, &
              'tbc_sw', &
              netcdf_info%vid_tbc_sw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
      end if


   else if (type .eq. NETCDF_OUTPUT_FILE_PRTM) then

      ctitle = 'ORAC Preprocessing prtm output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_prtm) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_rtm(3), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      ! define dimensions
      if (nf90_def_dim(netcdf_info%ncid_prtm, 'nlat_rtm', &
                       nlat, netcdf_info%dimid_y_pw) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_rtm(3), nf90_create(), dimension ' // &
              'name: nlat_rtm'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_prtm, 'nlon_rtm', &
                       nlon, netcdf_info%dimid_x_pw) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_rtm(2), nf90_create(), dimension ' // &
              'name: nlon_rtm'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_prtm, 'nlevels_rtm', &
                       kdim, netcdf_info%dimid_levels_pw) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_rtm(3), nf90_create(), dimension ' // &
              'name: nlevels_rtm'
         stop error_stop_code
      end if


      ! define 1-D variables
      dimids_1d(1) = netcdf_info%dimid_x_pw

      ! define lon_rtm
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_prtm, &
           dimids_1d, &
           'lon_rtm', &
           netcdf_info%vid_lon_pw, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define lat_rtm
      dimids_1d(1) = netcdf_info%dimid_y_pw
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_prtm, &
           dimids_1d, &
           'lat_rtm', &
           netcdf_info%vid_lat_pw, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define 3-D variables
      dimids_3d(1) = netcdf_info%dimid_levels_pw
      dimids_3d(2) = netcdf_info%dimid_x_pw
      dimids_3d(3) = netcdf_info%dimid_y_pw

      ! define pprofile_rtm
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_prtm, &
           dimids_3d, &
           'pprofile_rtm', &
           netcdf_info%vid_pprofile_lev_pw, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define tprofile_rtm
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_prtm, &
           dimids_3d, &
           'tprofile_rtm', &
           netcdf_info%vid_tprofile_lev_pw, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define hprofile_rtm
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_prtm, &
           dimids_3d, &
           'hprofile_rtm', &
           netcdf_info%vid_hprofile_lev_pw, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define qprofile_rtm
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_prtm, &
           dimids_3d, &
           'qprofile_rtm', &
           netcdf_info%vid_qprofile_lev_pw, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define o3profile_rtm
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_prtm, &
           dimids_3d, &
           'o3profile_rtm', &
           netcdf_info%vid_o3profile_lev_pw, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)
   end if


   ! set up attributes common to all output files
   if (type .eq. NETCDF_OUTPUT_FILE_PRTM)  ncid = netcdf_info%ncid_prtm
   if (type .eq. NETCDF_OUTPUT_FILE_LWRTM) ncid = netcdf_info%ncid_lwrtm
   if (type .eq. NETCDF_OUTPUT_FILE_SWRTM) ncid = netcdf_info%ncid_swrtm

   call netcdf_put_common_attributes(ncid, global_atts, source_atts, ctitle, &
                                     platform, sensor, path, cyear, cmonth, cday, &
                                     chour, cminute)


   ! close definition section
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_rtm(): nf90_enddef()'
      stop error_stop_code
   end if

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
! 2014/09/09, AP: Remove procflag as that's controlled by ORAC driver file.
! 2014/09/16, GM: Use the nc_def_var routine from the orac_ncdf module in the
!    common library.
! 2014/09/28, GM: Removed the option for chunking. Chunking will not help
!    any more as the variables are written by the preprocessor and read by the
!    main processor in the order in which they are stored.
! 2016/04/28, AP: Make multiple views mandatory.
! 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2018/11/05, SP: Add CAPE

!
!-------------------------------------------------------------------------------

subroutine netcdf_create_swath(global_atts, source_atts, cyear, cmonth, cday, chour, &
     cminute, platform, sensor, path, type, imager_geolocation, imager_angles, &
     netcdf_info, channel_info, include_full_brdf, do_cloud_emis, use_seviri_ann_mlay, &
     verbose)

   use netcdf

   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use orac_ncdf_m
   use preproc_constants_m
   use source_attributes_m

   implicit none

   ! Input
   type(global_attributes_t),  intent(in)    :: global_atts
   type(source_attributes_t),  intent(in)    :: source_atts
   character(len=*),           intent(in)    :: cyear
   character(len=*),           intent(in)    :: cmonth
   character(len=*),           intent(in)    :: cday
   character(len=*),           intent(in)    :: chour
   character(len=*),           intent(in)    :: cminute
   character(len=*),           intent(in)    :: platform
   character(len=*),           intent(in)    :: sensor
   character(len=*),           intent(in)    :: path
   integer,                    intent(in)    :: type
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(imager_angles_t),      intent(in)    :: imager_angles
   type(netcdf_output_info_t), intent(inout) :: netcdf_info
   type(channel_info_t),       intent(in)    :: channel_info
   logical,                    intent(in)    :: include_full_brdf
   logical,                    intent(in)    :: do_cloud_emis
   logical,                    intent(in)    :: verbose
   logical,                    intent(in)    :: use_seviri_ann_mlay

   ! Local
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer                    :: dimids_1d(1)
   integer                    :: dimids_2d(2)
   integer                    :: dimids_3d(3)

   if (type .eq. NETCDF_OUTPUT_FILE_ABL) then

      ctitle = 'ORAC Preprocessing alb output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_alb) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(1), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      ! define dimensions

      if (nf90_def_dim(netcdf_info%ncid_alb, 'nc_alb', &
                       channel_info%nchannels_sw, netcdf_info%dimid_c_alb) &
           .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(1), nf90_create(), ' // &
              'dimension name: nc_alb'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_alb, 'nc_emis', &
                       channel_info%nchannels_lw, netcdf_info%dimid_c_emis) &
           .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(1), nf90_create(), ' // &
              'dimension name: nc_emis'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_alb, 'nx_alb', &
                       imager_geolocation%endx-imager_geolocation%startx+1, &
                       netcdf_info%dimid_x_alb) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(1), nf90_create(), ' // &
              'dimension name: nx_alb'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_alb, 'ny_alb', &
                       imager_geolocation%endy-imager_geolocation%starty+1, &
                       netcdf_info%dimid_y_alb) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(1), nf90_create(), ' // &
              'dimension name: ny_alb'
         stop error_stop_code
      end if


      if (channel_info%nchannels_sw .ne. 0) then
         dimids_1d(1) = netcdf_info%dimid_c_alb

         call ncdf_def_var_long_packed_long( &
              netcdf_info%ncid_alb, &
              dimids_1d, &
              'alb_abs_ch_numbers', &
              netcdf_info%vid_alb_abs_ch_numbers, &
              verbose, &
              fill_value = lint_fill_value)
      end if

      if (channel_info%nchannels_lw .ne. 0) then
         dimids_1d(1) = netcdf_info%dimid_c_emis

         call ncdf_def_var_long_packed_long( &
              netcdf_info%ncid_alb, &
              dimids_1d, &
              'emis_abs_ch_numbers', &
              netcdf_info%vid_emis_abs_ch_numbers, &
              verbose, &
              fill_value = lint_fill_value)
      end if


      if (channel_info%nchannels_sw .ne. 0) then
         dimids_3d(1) = netcdf_info%dimid_x_alb
         dimids_3d(2) = netcdf_info%dimid_y_alb
         dimids_3d(3) = netcdf_info%dimid_c_alb

         ! define alb
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_alb, &
              dimids_3d, &
              'alb_data', &
              netcdf_info%vid_alb_data, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

         if (include_full_brdf) then
            call ncdf_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_0v_data', &
                 netcdf_info%vid_rho_0v_data, &
                 verbose, &
                 deflate_level = deflate_level, &
                 shuffle = shuffle_flag, &
                 fill_value = sreal_fill_value)

            call ncdf_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_0d_data', &
                 netcdf_info%vid_rho_0d_data, &
                 verbose, &
                 deflate_level = deflate_level, &
                 shuffle = shuffle_flag, &
                 fill_value = sreal_fill_value)

            call ncdf_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_dv_data', &
                 netcdf_info%vid_rho_dv_data, &
                 verbose, &
                 deflate_level = deflate_level, &
                 shuffle = shuffle_flag, &
                 fill_value = sreal_fill_value)

            call ncdf_def_var_float_packed_float( &
                 netcdf_info%ncid_alb, &
                 dimids_3d, &
                 'rho_dd_data', &
                 netcdf_info%vid_rho_dd_data, &
                 verbose, &
                 deflate_level = deflate_level, &
                 shuffle = shuffle_flag, &
                 fill_value = sreal_fill_value)
         end if
      end if

      if (channel_info%nchannels_lw .ne. 0) then
         dimids_3d(1) = netcdf_info%dimid_x_alb
         dimids_3d(2) = netcdf_info%dimid_y_alb
         dimids_3d(3) = netcdf_info%dimid_c_emis

         ! define emis
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_alb, &
              dimids_3d, &
              'emis_data', &
              netcdf_info%vid_emis_data, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
      end if


   else if (type .eq. NETCDF_OUTPUT_FILE_CLF) then

      ctitle = 'ORAC Preprocessing cf output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_clf) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(2), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      ! define dimensions
      if (nf90_def_dim(netcdf_info%ncid_clf, 'nx_cf', &
                       imager_geolocation%endx-imager_geolocation%startx+1, &
                       netcdf_info%dimid_x_cf) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(2), nf90_create(), ' // &
              'dimension name: nx_cf'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_clf, 'ny_cf', &
                       imager_geolocation%endy-imager_geolocation%starty+1, &
                       netcdf_info%dimid_y_cf) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(2), nf90_create(), ' // &
              'dimension name: ny_cf'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_clf, 'nv_cf', &
                       imager_angles%nviews, &
                       netcdf_info%dimid_v_cf) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(2), nf90_create(), ' // &
              'dimension name: nvcf'
         stop error_stop_code
      end if


      dimids_2d(1) = netcdf_info%dimid_x_cf
      dimids_2d(2) = netcdf_info%dimid_y_cf

      dimids_3d(1) = netcdf_info%dimid_x_cf
      dimids_3d(2) = netcdf_info%dimid_y_cf
      dimids_3d(3) = netcdf_info%dimid_v_cf

      ! define cflag
      call ncdf_def_var_byte_packed_byte( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'cflag', &
           netcdf_info%vid_cflag, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = byte_fill_value)

      ! define cldemis variable
#ifdef INCLUDE_SATWX
      if (do_cloud_emis) then
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_2d, &
              'cldemis_lw', &
              netcdf_info%vid_cemis_lw, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_2d, &
              'cldemis_wv_1', &
              netcdf_info%vid_cemis_wv1, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_2d, &
              'cldemis_wv_2', &
              netcdf_info%vid_cemis_wv2, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_2d, &
              'tropopause_pres', &
              netcdf_info%vid_tropop_pr, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_2d, &
              'tropopause_temp', &
              netcdf_info%vid_tropop_te, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_2d, &
              'cape', &
              netcdf_info%vid_cape, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
      end if
#endif

      ! define cldtype variable
      call ncdf_def_var_byte_packed_byte( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'cldtype', &
           netcdf_info%vid_cldtype, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = byte_fill_value)

      ! define cldmask variable
      call ncdf_def_var_byte_packed_byte( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'cldmask', &
           netcdf_info%vid_cldmask, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = byte_fill_value)

      ! define cldmask_uncertainty variable
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'cldmask_uncertainty', &
           netcdf_info%vid_cldmask_unc, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define cccot_pre variable
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'cccot_pre', &
           netcdf_info%vid_cccot_pre, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define ann cloud_phase variable
      call ncdf_def_var_byte_packed_byte( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'ann_phase', &
           netcdf_info%vid_ann_phase, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = byte_fill_value)

      ! define ann cloud phase_uncertainty variable
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'ann_phase_uncertainty', &
           netcdf_info%vid_ann_phase_unc, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define cphcot variable
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_clf, &
           dimids_3d, &
           'cphcot', &
           netcdf_info%vid_cphcot, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

#ifdef INCLUDE_SEVIRI_NEURALNET
      if (use_seviri_ann_mlay) then
         ! define multilayer probability
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_3d, &
              'mlay_prob', &
              netcdf_info%vid_mlay_prob, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)
                  ! define multilayer probability
         call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_clf, &
              dimids_3d, &
              'mlay_unc', &
              netcdf_info%vid_mlay_unc, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

         call ncdf_def_var_byte_packed_byte( &
              netcdf_info%ncid_clf, &
              dimids_3d, &
              'mlay_flag', &
              netcdf_info%vid_mlay_flag, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = byte_fill_value)
      end if

#endif

   else if (type .eq. NETCDF_OUTPUT_FILE_GEO) then

      ctitle = 'ORAC Preprocessing geo output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_geo) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(1), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      ! define dimensions
      if (nf90_def_dim(netcdf_info%ncid_geo, 'nv_geo', imager_angles%nviews, &
                       netcdf_info%dimid_v_geo) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(3), nf90_create(), ' // &
              'dimension name: nv_geo'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_geo, 'nx_geo', &
                       imager_geolocation%endx-imager_geolocation%startx+1, &
                       netcdf_info%dimid_x_geo) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(3), nf90_create(), ' // &
              'dimension name: nx_geo'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_geo, 'ny_geo', &
                       imager_geolocation%endy-imager_geolocation%starty+1, &
                       netcdf_info%dimid_y_geo) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(3), nf90_create(), ' // &
              'dimension name: ny_geo'
         stop error_stop_code
      end if


      dimids_3d(1) = netcdf_info%dimid_x_geo
      dimids_3d(2) = netcdf_info%dimid_y_geo
      dimids_3d(3) = netcdf_info%dimid_v_geo

      ! define solzen
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_geo, &
           dimids_3d, &
           'solzen', &
           netcdf_info%vid_solzen, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define satzen
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_geo, &
           dimids_3d, &
           'satzen', &
           netcdf_info%vid_satzen, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define solaz
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_geo, &
           dimids_3d, &
           'solaz', &
           netcdf_info%vid_solaz, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define sataz
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_geo, &
           dimids_3d, &
           'sataz', &
           netcdf_info%vid_sataz, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define relazi
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_geo, &
           dimids_3d, &
           'relazi', &
           netcdf_info%vid_relazi, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)


   else if (type .eq. NETCDF_OUTPUT_FILE_LOC) then

      ctitle = 'ORAC Preprocessing loc output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_loc) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(4), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      ! define dimensions
      if (nf90_def_dim(netcdf_info%ncid_loc, 'nx_loc', &
                       imager_geolocation%endx-imager_geolocation%startx+1, &
                       netcdf_info%dimid_x_loc) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(4), nf90_create(), ' // &
              'dimension name: nx_loc'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_loc, 'ny_loc', &
                       imager_geolocation%endy-imager_geolocation%starty+1, &
                       netcdf_info%dimid_y_loc) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(4), nf90_create(), ' // &
              'dimension name: ny_loc'
         stop error_stop_code
      end if


      dimids_2d(1) = netcdf_info%dimid_x_loc
      dimids_2d(2) = netcdf_info%dimid_y_loc

      ! define lat
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_loc, &
           dimids_2d, &
           'lat', &
           netcdf_info%vid_lat, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define lon
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_loc, &
           dimids_2d, &
           'lon', &
           netcdf_info%vid_lon, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)


   else if (type .eq. NETCDF_OUTPUT_FILE_LSF) then
      ctitle = 'ORAC Preprocessing lsf output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_lsf) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(5), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      ! define dimensions
      if (nf90_def_dim(netcdf_info%ncid_lsf, 'nx_lsf', &
                       imager_geolocation%endx-imager_geolocation%startx+1, &
                       netcdf_info%dimid_x_lsf) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(5), nf90_create(), ' // &
              'dimension name: nx_lsf'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_lsf, 'ny_lsf', &
                       imager_geolocation%endy-imager_geolocation%starty+1, &
                       netcdf_info%dimid_y_lsf) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(5), nf90_create(), ' // &
              'dimension name: ny_lsf'
         stop error_stop_code
      end if


      dimids_2d(1) = netcdf_info%dimid_x_lsf
      dimids_2d(2) = netcdf_info%dimid_y_lsf

      ! define lsflag
      call ncdf_def_var_byte_packed_byte( &
           netcdf_info%ncid_lsf, &
           dimids_2d, &
           'lsflag', &
           netcdf_info%vid_lsflag, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = byte_fill_value)

      ! define lusflag variable
      call ncdf_def_var_byte_packed_byte( &
           netcdf_info%ncid_lsf, &
           dimids_2d, &
           'lusflag', &
           netcdf_info%vid_lusflag, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = byte_fill_value)

      ! define dem variable
      call ncdf_def_var_long_packed_long( &
           netcdf_info%ncid_lsf, &
           dimids_2d, &
           'dem', &
           netcdf_info%vid_dem, &
           verbose, &
           fill_value = lint_fill_value)

      ! define nise_mask variable
      call ncdf_def_var_byte_packed_byte( &
           netcdf_info%ncid_lsf, &
           dimids_2d, &
           'nisemask', &
           netcdf_info%vid_nisemask, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = byte_fill_value)


   else if (type .eq. NETCDF_OUTPUT_FILE_MSI) then

      ctitle = 'ORAC Preprocessing msi output file'


      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_msi) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(6), nf90_create(), ' // &
              'filename: ', path
         stop error_stop_code
      end if


      ! define dimensions
      if (.false.) then
         if (nf90_def_dim(netcdf_info%ncid_msi, 'nv_msi', imager_angles%nviews, &
                          netcdf_info%dimid_v_msi) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: netcdf_create_swath(6), nf90_create(), ' // &
                 'dimension name: nv_msi'
            stop error_stop_code
         end if
      end if
      if (nf90_def_dim(netcdf_info%ncid_msi, 'nc_msi', &
                       channel_info%nchannels_total, &
                       netcdf_info%dimid_c_msi) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(6), nf90_create(), ' // &
              'dimension name: nc_msi'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_msi, 'nx_msi', &
                       imager_geolocation%endx-imager_geolocation%startx+1, &
                       netcdf_info%dimid_x_msi) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(6), nf90_create(), ' // &
              'dimension name: nx_msi'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_msi, 'ny_msi', &
                       imager_geolocation%endy-imager_geolocation%starty+1, &
                       netcdf_info%dimid_y_msi) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(6), nf90_create(), ' // &
              'dimension name: ny_msi'
         stop error_stop_code
      end if


      ! define some channel variables
      dimids_1d(1) = netcdf_info%dimid_c_msi

      call ncdf_def_var_long_packed_long( &
           netcdf_info%ncid_msi, &
           dimids_1d, &
           'msi_instr_ch_numbers', &
           netcdf_info%vid_msi_instr_ch_numbers, &
           verbose, &
           fill_value = lint_fill_value)

      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_msi, &
           dimids_1d, &
           'msi_abs_ch_wl', &
           netcdf_info%vid_msi_abs_ch_wl, &
           verbose, &
           fill_value = sreal_fill_value)

      call ncdf_def_var_long_packed_long( &
           netcdf_info%ncid_msi, &
           dimids_1d, &
           'msi_ch_swflag', &
           netcdf_info%vid_msi_ch_swflag, &
           verbose, &
           fill_value = lint_fill_value)

      call ncdf_def_var_long_packed_long( &
           netcdf_info%ncid_msi, &
           dimids_1d, &
           'msi_ch_lwflag', &
           netcdf_info%vid_msi_ch_lwflag, &
           verbose, &
           fill_value = lint_fill_value)

      call ncdf_def_var_long_packed_long( &
           netcdf_info%ncid_msi, &
           dimids_1d, &
           'msi_ch_view', &
           netcdf_info%vid_msi_ch_view, &
           verbose, &
           fill_value = lint_fill_value)


      dimids_2d(1) = netcdf_info%dimid_x_msi
      dimids_2d(2) = netcdf_info%dimid_y_msi

      ! define time_data
      call ncdf_def_var_double_packed_double( &
           netcdf_info%ncid_msi, &
           dimids_2d, &
           'time_data', &
           netcdf_info%vid_time, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = dreal_fill_value)


      dimids_3d(1) = netcdf_info%dimid_x_msi
      dimids_3d(2) = netcdf_info%dimid_y_msi
      dimids_3d(3) = netcdf_info%dimid_c_msi

      ! define msi_data
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_msi, &
           dimids_3d, &
           'msi_data', &
           netcdf_info%vid_msi_data, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define sd_data
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_msi, &
           dimids_3d, &
           'sd_data', &
           netcdf_info%vid_sd_data, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

      ! define cal_gain data
      call ncdf_def_var_float_packed_float( &
           netcdf_info%ncid_msi, &
           dimids_1d, &
           'cal_data', &
           netcdf_info%vid_cal_data, &
           verbose, &
           deflate_level = deflate_level, &
           shuffle = shuffle_flag, &
           fill_value = sreal_fill_value)

   else if (type .eq. NETCDF_OUTPUT_FILE_CTP) then

       ctitle = 'ORAC Preprocessing ctp output file'

      ! create file
      if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                      netcdf_info%ncid_ctp) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(5), nf90_create(), filename: ', &
              path
         stop error_stop_code
      end if


      ! define dimensions
      if (nf90_def_dim(netcdf_info%ncid_ctp, 'nx_ctp', &
                       imager_geolocation%endx-imager_geolocation%startx+1, &
                       netcdf_info%dimid_x_ctp) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(5), nf90_create(), ' // &
              'dimension name: nx_ctp'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_ctp, 'ny_ctp', &
                       imager_geolocation%endy-imager_geolocation%starty+1, &
                       netcdf_info%dimid_y_ctp) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(5), nf90_create(), ' // &
              'dimension name: ny_ctp'
         stop error_stop_code
      end if

      if (nf90_def_dim(netcdf_info%ncid_ctp, 'nv_ctp', imager_angles%nviews, &
                       netcdf_info%dimid_v_ctp) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: netcdf_create_swath(3), nf90_create(), ' // &
              'dimension name: nv_ctp'
         stop error_stop_code
      end if

      dimids_3d(1) = netcdf_info%dimid_x_ctp
      dimids_3d(2) = netcdf_info%dimid_y_ctp
      dimids_3d(3) = netcdf_info%dimid_v_ctp

      ! define ctp_fg variable
      call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_ctp, &
              dimids_3d, &
              'ctp', &
              netcdf_info%vid_ctp_fg, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

      ! define ctp_fg_unc variable
      call ncdf_def_var_float_packed_float( &
              netcdf_info%ncid_ctp, &
              dimids_3d, &
              'ctp_var', &
              netcdf_info%vid_ctp_fg_unc, &
              verbose, &
              deflate_level = deflate_level, &
              shuffle = shuffle_flag, &
              fill_value = sreal_fill_value)

   end if

   ! set up attributes common to all output files
   if (type .eq. NETCDF_OUTPUT_FILE_ABL) ncid = netcdf_info%ncid_alb
   if (type .eq. NETCDF_OUTPUT_FILE_CLF) ncid = netcdf_info%ncid_clf
   if (type .eq. NETCDF_OUTPUT_FILE_GEO) ncid = netcdf_info%ncid_geo
   if (type .eq. NETCDF_OUTPUT_FILE_LOC) ncid = netcdf_info%ncid_loc
   if (type .eq. NETCDF_OUTPUT_FILE_LSF) ncid = netcdf_info%ncid_lsf
   if (type .eq. NETCDF_OUTPUT_FILE_MSI) ncid = netcdf_info%ncid_msi
   if (type .eq. NETCDF_OUTPUT_FILE_CTP) ncid = netcdf_info%ncid_ctp

   call netcdf_put_common_attributes(ncid, global_atts, source_atts, ctitle, &
        platform, sensor, path, cyear, cmonth, cday, &
        chour, cminute)


   ! close definition section
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_swath(): nf90_enddef()'
      stop error_stop_code
   end if


   return

end subroutine netcdf_create_swath

!-------------------------------------------------------------------------------
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! global_atts    struct  in   Structure detailing NCDF header contents.
! source_atts    struct  in   Structure detailing source file information
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
! 2014/09/09, AP: Remove procflag as that's controlled by ORAC driver file.
! 2014/09/16, GM: Use the nc_def_var routine from the orac_ncdf module in the
!    common library.
! 2014/12/01, CP: Added in source attributes.
! 2017/07/05, AP: Added all_nchannels_total.
!
!-------------------------------------------------------------------------------

subroutine netcdf_create_config(global_atts, source_atts, cyear, cmonth, cday, &
     chour, cminute, platform, sensor, path, preproc_dims, imager_geolocation, &
     netcdf_info, channel_info, verbose)

   use netcdf

   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use orac_ncdf_m
   use preproc_constants_m
   use preproc_structures_m
   use source_attributes_m

   implicit none

   ! Input
   type(global_attributes_t),  intent(in)    :: global_atts
   type(source_attributes_t),  intent(in)    :: source_atts
   character(len=*),           intent(in)    :: cyear
   character(len=*),           intent(in)    :: cmonth
   character(len=*),           intent(in)    :: cday
   character(len=*),           intent(in)    :: chour
   character(len=*),           intent(in)    :: cminute
   character(len=*),           intent(in)    :: platform
   character(len=*),           intent(in)    :: sensor
   character(len=*),           intent(in)    :: path
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(netcdf_output_info_t), intent(inout) :: netcdf_info
   type(channel_info_t),       intent(in)    :: channel_info
   logical,                    intent(in)    :: verbose

   ! Local
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer                    :: dimids_1d(1)


   ctitle = 'ORAC Preprocessing config file'


   ! create file
   if (nf90_create(path, IOR(NF90_HDF5, NF90_CLASSIC_MODEL), &
                   netcdf_info%ncid_config) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), filename: ', &
           path
      stop error_stop_code
   end if


   ! define dimensions
   if (nf90_def_dim(netcdf_info%ncid_config, 'nc_conf', &
                    channel_info%nchannels_total, netcdf_info%dimid_c_config) &
        .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: nc_conf'
      stop error_stop_code
   end if

   if (nf90_def_dim(netcdf_info%ncid_config, 'nc_alb', &
                    channel_info%nchannels_sw, netcdf_info%dimid_c_config_alb) &
        .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: nc_alb'
      stop error_stop_code
   end if

   if (nf90_def_dim(netcdf_info%ncid_config, 'nc_emis', &
                    channel_info%nchannels_lw, netcdf_info%dimid_c_config_emis) &
        .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: nc_emis'
      stop error_stop_code
   end if

   if (nf90_def_dim(netcdf_info%ncid_config, 'nlat_conf', &
                    preproc_dims%max_lat-preproc_dims%min_lat+1, &
                    netcdf_info%dimid_y_lw) &
        .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: nlat_conf'
      stop error_stop_code
   end if

   if (nf90_def_dim(netcdf_info%ncid_config, 'nlon_conf', &
                    preproc_dims%max_lon-preproc_dims%min_lon+1, &
                    netcdf_info%dimid_x_lw) &
        .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: nlon_conf'
      stop error_stop_code
   end if

   if (nf90_def_dim(netcdf_info%ncid_config, 'nlevels_conf', &
                    preproc_dims%kdim+1, &
                    netcdf_info%dimid_levels_lw) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: nlevels_conf'
      stop error_stop_code
   end if

   if (nf90_def_dim(netcdf_info%ncid_config, 'nx_conf', &
                    imager_geolocation%endx-imager_geolocation%startx+1, &
                    netcdf_info%dimid_x_config) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: ny_conf'
      stop error_stop_code
   end if

   if (nf90_def_dim(netcdf_info%ncid_config, 'ny_conf', &
                    imager_geolocation%endy-imager_geolocation%starty+1, &
                    netcdf_info%dimid_y_config) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(), nf90_create(), dimension ' // &
           'name: ny_conf'
      stop error_stop_code
   end if


   ! define some channel variables

   dimids_1d(1) = netcdf_info%dimid_c_config

   call ncdf_def_var_long_packed_long( &
        netcdf_info%ncid_config, &
        dimids_1d, &
        'msi_instr_ch_numbers', &
        netcdf_info%vid_msi_instr_ch_numbers_config, &
        verbose, &
        fill_value = lint_fill_value)

   call ncdf_def_var_float_packed_float( &
        netcdf_info%ncid_config, &
        dimids_1d, &
        'msi_abs_ch_wl', &
        netcdf_info%vid_msi_abs_ch_wl_config, &
        verbose, &
        fill_value = sreal_fill_value)

   call ncdf_def_var_long_packed_long( &
        netcdf_info%ncid_config, &
        dimids_1d, &
        'msi_ch_swflag', &
        netcdf_info%vid_msi_ch_swflag_config, &
        verbose, &
        fill_value = lint_fill_value)

   call ncdf_def_var_long_packed_long( &
        netcdf_info%ncid_config, &
        dimids_1d, &
        'msi_ch_lwflag', &
        netcdf_info%vid_msi_ch_lwflag_config, &
        verbose, &
        fill_value = lint_fill_value)

   call ncdf_def_var_long_packed_long( &
        netcdf_info%ncid_config, &
        dimids_1d, &
        'msi_ch_view', &
        netcdf_info%vid_msi_ch_view_config, &
        verbose, &
        fill_value = lint_fill_value)

   dimids_1d(1) = netcdf_info%dimid_c_config_alb

   call ncdf_def_var_long_packed_long( &
        netcdf_info%ncid_config, &
        dimids_1d, &
        'alb_abs_ch_numbers', &
        netcdf_info%vid_alb_abs_ch_numbers_config, &
        verbose, &
        fill_value = lint_fill_value)

   dimids_1d(1) = netcdf_info%dimid_c_config_emis

   call ncdf_def_var_long_packed_long( &
        netcdf_info%ncid_config, &
        dimids_1d, &
        'emis_abs_ch_numbers', &
        netcdf_info%vid_emis_abs_ch_numbers_config, &
        verbose, &
        fill_value = lint_fill_value)

   ! Put number of instrument channels (for ch_flag) in attribute
   if (nf90_put_att(netcdf_info%ncid_config, NF90_GLOBAL, &
                    'all_nchannels_total', channel_info%all_nchannels_total) &
       .ne. NF90_NOERR) then
      write(*,*) 'ERROR: netcdf_create_config(1), nf90_put_att(), ', &
           ', name: all_nchannels_total'
      stop error_stop_code
   end if

   ncid = netcdf_info%ncid_config

   ! set up attributes common to all output files
   call netcdf_put_common_attributes(ncid, global_atts, source_atts, ctitle, &
        platform, sensor, path, cyear, cmonth, cday, &
        chour, cminute)


   ! close definition section
   if (nf90_enddef(ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_enddef()'
      stop error_stop_code
   end if


   return

end subroutine netcdf_create_config


subroutine netcdf_put_common_attributes(ncid, global_atts, source_atts, title, &
     platform, sensor, path, cyear, cmonth, cday, &
     chour, cminute)

   use netcdf

   use global_attributes_m
   use orac_ncdf_m
   use source_attributes_m

   implicit none

   integer,                   intent(in) :: ncid
   type(global_attributes_t), intent(in) :: global_atts
   type(source_attributes_t), intent(in) :: source_atts
   character(len=*),          intent(in) :: title
   character(len=*),          intent(in) :: platform
   character(len=*),          intent(in) :: sensor
   character(len=*),          intent(in) :: path
   character(len=*),          intent(in) :: cyear
   character(len=*),          intent(in) :: cmonth
   character(len=*),          intent(in) :: cday
   character(len=*),          intent(in) :: chour
   character(len=*),          intent(in) :: cminute

   integer                        :: position, length
   type(global_attributes_t)      :: global_atts2
   type(source_attributes_t)      :: source_atts2

   global_atts2 = global_atts
   source_atts2 = source_atts

   global_atts2%title  = trim(title)

   global_atts2%source = trim(source_atts%level1b_version)

   position = index(trim(path), '/', back=.true.)
   length = len_trim(path)
   global_atts2%File_Name    = trim(path(position+1:length))

   ! product_name should be the base filename of the product
   position = index(trim(global_atts2%File_Name), '_', back=.true.)
   global_atts2%Product_Name = trim(global_atts2%File_Name(1:position-1))

   global_atts2%Date_Created = trim(cyear)//trim(cmonth)//trim(cday)// &
        trim(chour)//trim(cminute)

   global_atts2%Platform = trim(platform)
   global_atts2%Sensor   = trim(sensor)

   global_atts2%AATSR_Processing_Version = ' '
   if (sensor .eq. 'ATSR' .or. sensor .eq. 'AATSR') then
      global_atts2%AATSR_Processing_Version = '3.01'
   end if

   global_atts2%SVN_Version = 'xxx'

   call ncdf_put_common_attributes(ncid, global_atts2, source_atts2)


end subroutine netcdf_put_common_attributes
