!-------------------------------------------------------------------------------
! Name: read_imager.F90
!
! Purpose:
! A wrapper to call the routine appropriate for reading L1B and geolocation
! data for this sensor.
!
! Description and Algorithm details:
! 1) Call read_L1B routine.
! 2) Call read_geolocation routine.
!
! Arguments:
! Name                      Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! sensor                    string in   Name of instrument
! platform                  string in   Name of satellite
! path_to_l1b_file          string in   Full path to level 1B data
! path_to_geo_file          string in   Full path to geolocation data
! path_to_aatsr_drift_table string in   Full path to the AATSR calibration file
! imager_geolocation        struct both Summary of pixel positions
! imager_angles             struct both Summary of sun/satellite viewing angles
! imager_flags              struct both Summary of land/sea/ice flags
! imager_time               struct both Summary of pixel observation time
! imager_measurements       struct both Satellite observations
! channel_info              struct in   Summary of channel information
! n_along_track             lint   in   Number of pixels available in the
!                                       direction of travel
! do_gsics                  logic  in   T: apply GSICS correction; F: don't
! global_atts               struct both Attribute information, needed for satpos
! verbose                   logic  in   T: print status information; F: don't
!
! History:
! 2011/12/12, MJ: produces draft code which opens and reads MODIS L1b file.
! 2012/01/24, MJ: includes code to read AVHRR L1b file.
! 2012/06/22, GT: Added code to read AATSR L1b file.
! 2012/08/28, ??: Set imager_flags%cflag = 1 for MODIS, AVHHR, and AATSR as we
!    don't have proper masks.
! 2012/09/04, GT: Corrected AATSR sensor name
! 2013/09/06, AP: tidying
! 2014/12/01, OS: removed call to read_avhrr_land_sea_mask, which was obsolete
!    as land/sea information is now read in SR read_USGS_file
! 2015/02/19, GM: Added SEVIRI support.
! 2015/08/08, CP: Added ATSR2 functionality
! 2016/04/08, SP: Added Himawari support.
! 2016/05/16, SP: Added Suomi-NPP support.
! 2016/06/28, SP: Added SLSTR-Sentinel3 support.
! 2016/07/24, AP: Put back call to read_avhrr_land_sea_mask for
!                 use_l1_land_mask
! 2017/04/26, SP: Support for loading geoinfo (lat/lon/vza/vaa) from an
!                 external file. Supported by AHI, not yet by SEVIRI (ExtWork)
! 2017/08/10, GT: Added a check on the existence of a geo_file before
!                 printing path
! 2018/06/03, SP: GSICS calibration is now supported for SEVIRI. The default
!                 setting is ON, meaning that GSICS coefficients will be used
!                 instead of IMPF (as previous). The new driver file option
!                 USE_GSICS enables this to be disabled.
! 2018/06/08, SP: New global attribute to store satellite position information
! 2019/08/14, SP: Add Fengyun-4A support.
! 2021/03/09, AP: Add radiance bias corrections.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_imager_m

implicit none

contains

subroutine read_imager(granule, opts, path_to_aatsr_drift_table, &
     imager_geolocation, imager_angles, imager_flags, imager_time, &
     imager_measurements, channel_info, global_atts, verbose)

   use channel_structures_m
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use preproc_structures_m, only: preproc_opts_t, setup_args_t
   use read_aatsr_m
   use read_abi_m
   use read_agri_m
   use read_avhrr_m
   use read_himawari_m
   use read_modis_m
   use read_seviri_m
   use read_slstr_m
   use read_viirs_iband_m
   use read_viirs_mband_m

   implicit none

   type(setup_args_t),          intent(in)    :: granule
   type(preproc_opts_t),        intent(in)    :: opts
   character(len=*),            intent(in)    :: path_to_aatsr_drift_table
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_flags_t),        intent(inout) :: imager_flags
   type(imager_time_t),         intent(inout) :: imager_time
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(channel_info_t),        intent(in)    :: channel_info
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   character(len=file_length), allocatable    :: abi_filenames(:)

   integer :: i, j, k
   real    :: meas_unc, fm_unc

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_imager()'

   if (verbose) write(*,*) 'sensor: ',           trim(granule%sensor)
   if (verbose) write(*,*) 'platform: ',         trim(granule%platform)
   if (verbose) write(*,*) 'path_to_l1b_file: ', trim(granule%l1b_file)
   if (verbose) write(*,*) 'path_to_geo_file: ', trim(granule%geo_file)
   ! Not all instruments provide a geo file, and if a string is nothing by
   ! blank characters, then trim doesn't work
   if ((verbose) .and. (len_trim(opts%ext_geo_path) .ne. path_length)) &
        write(*,*) 'geo_file_path:    ', trim(opts%ext_geo_path)

   ! Set satellite position metadata to blank by default
   ! Currently this is only used by geosats for parallax correction (postproc)
   global_atts%Satpos_Metadata = ""

   !branches for the sensors
   select case (trim(granule%sensor))
   case('AATSR', 'ATSR2')
      if (verbose) write(*,*) 'path_to_aatsr_drift_table: ', &
                              trim(path_to_aatsr_drift_table)

      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation

      call read_aatsr_l1b(granule%l1b_file, path_to_aatsr_drift_table, &
           imager_geolocation, imager_measurements, imager_angles, &
           imager_flags, imager_time, channel_info, granule%sensor, verbose)

   case('ABI')
      ! Assemble the filenames required for ABI data
      allocate(abi_filenames(channel_info%nchannels_total))
      call get_abi_path(granule%l1b_file, granule%platform, abi_filenames, &
           channel_info%nchannels_total, channel_info%channel_ids_instr)

      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_abi_bin(abi_filenames, imager_geolocation, &
           imager_measurements, imager_angles, imager_time, channel_info, &
           opts%use_predef_geo, opts%ext_geo_path, global_atts, verbose)

      !in absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

      deallocate(abi_filenames)

   case('AGRI')
      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_agri_data(granule%l1b_file, imager_geolocation, &
           imager_measurements, imager_angles, imager_time, channel_info, &
           global_atts, verbose)
      !in absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case('AHI')
      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_himawari_bin(granule%l1b_file, imager_geolocation, &
           imager_measurements, imager_angles, imager_time, channel_info, &
           opts%use_predef_geo, opts%ext_geo_path, global_atts, verbose)

      !in absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case('AVHRR')
      !  Read the angles and lat/lon info of the orbit
      call read_avhrr_time_lat_lon_angles(granule%geo_file, imager_geolocation, &
           imager_angles, imager_time, granule%n_along_track, verbose)

      if (opts%use_l1_land_mask) &
           call read_avhrr_land_sea_mask(granule%geo_file, imager_geolocation, &
                imager_flags)

      ! Read the (subset) of the orbit etc. SW:reflectances, LW:brightness temp
      call read_avhrr_l1b_radiances(granule%sensor, granule%platform, granule%l1b_file, &
           imager_geolocation, imager_measurements, channel_info, verbose)

      ! In absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case('MODIS')
      call read_modis_time_lat_lon_angles(granule%geo_file, imager_geolocation, &
           imager_angles, imager_flags, imager_time, granule%n_along_track, verbose)

      ! Read MODIS L1b data. SW:reflectances, LW:brightness temperatures
      call read_modis_l1b_radiances(granule%sensor, granule%platform, granule%l1b_file, &
           imager_geolocation, imager_measurements, channel_info, verbose)

      ! In absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case('SEVIRI')
      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_seviri_l1_5(granule%l1b_file, &
           imager_geolocation, imager_measurements, imager_angles, &
           imager_time, channel_info, opts%do_gsics, global_atts, verbose)

      ! Temporary function for use until seviri_util predef geo is fixed. INEFFICIENT.
      if (opts%use_predef_geo) call SEV_Retrieve_Predef_Geo(imager_geolocation, &
           imager_angles, opts%ext_geo_path, verbose)

      ! In absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case('SLSTR')
      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_slstr(granule%l1b_file, &
           imager_geolocation, imager_measurements, imager_angles, imager_time, &
           imager_flags, channel_info, verbose)

      ! In absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case('VIIRSI')
      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_viirs_iband(granule%l1b_file, granule%geo_file, &
           imager_geolocation, imager_measurements, imager_angles, &
           imager_time, channel_info, verbose)

      ! In absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case('VIIRSM')
      ! Read the L1B data, according to the dimensions and offsets specified in
      ! imager_geolocation
      call read_viirs_mband(granule%l1b_file, granule%geo_file, &
           imager_geolocation, imager_measurements, imager_angles, &
           imager_time, channel_info, verbose)

      ! In absence of proper mask set everything to "1" for cloud mask
      imager_flags%cflag = 1

   case default
      write(*,*) 'ERROR: read_imager(): Invalid sensor: ', trim(adjustl(granule%sensor))
      stop error_stop_code
   end select

   ! Bias correction (these are defined in the appropriate setup.F90 routine)
   do k = 1, channel_info%nchannels_total
      imager_measurements%data(:,:,k) = channel_info%channel_absolute_bias(k) + &
           channel_info%channel_relative_bias(k) * &
           imager_measurements%data(:,:,k)
   end do

   ! Estimate measurement uncertainty
   ! NOTE: Doing this here as instruments will eventually provide L1 uncs
   do k = 1, channel_info%nchannels_total
      do j = 1, imager_geolocation%ny
         do i = imager_geolocation%startx, imager_geolocation%endx
            ! Measurement uncertainty is a constant fraction
            meas_unc = imager_measurements%data(i,j,k) * &
                 channel_info%channel_fractional_uncertainty(k)

            ! There is a digitization limit on that uncertainty
            if (meas_unc < channel_info%channel_minimum_uncertainty(k)) &
                 meas_unc = channel_info%channel_minimum_uncertainty(k)

            ! Forward model is a function of surface (see A Sayer's thesis)
            if (imager_flags%lsflag(i,j) .eq. 1) then
               fm_unc = imager_measurements%data(i,j,k) * &
                    channel_info%channel_fm_lnd_uncertainty(k)
            else
               fm_unc = imager_measurements%data(i,j,k) * &
                    channel_info%channel_fm_sea_uncertainty(k)
            end if

            ! Combine uncertainties in quadrature
            imager_measurements%uncertainty(i,j,k) = &
                 sqrt(meas_unc*meas_unc + fm_unc*fm_unc)
         end do
      end do
   end do

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_imager()'

end subroutine read_imager

end module read_imager_m
