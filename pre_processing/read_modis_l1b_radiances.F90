!-------------------------------------------------------------------------------
! Name: read_modis_l1b_radiances.F90
!
! Purpose:
! Open and read MODIS input files
!
! Description and Algorithm details:
! 1) Allocate temporary array.
! 2) Open file.
! 3) Call L1B read routine.
! 4) If necessary, convert radiances to brightness temperatures.
! 5) Close file.
!
! Arguments:
! Name                Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! sensor              string in   Name of instrument
! platform            string in   Name of satellite
! path_to_l1b_file    string in   Full path to level 1B data
! imager_geolocation  struct in   Summary of pixel positions
! imager_measurements struct both Satellite observations
! channel_info        struct in   Summary of channel information
! verbose             logic  in   F: minimise information printed to screen;
!                                 T: don't
!
! History:
! 2011/12/21, MJ: produces draft code which opens and reads MODIS L1b hdf files
! 2013/09/06, AP: tidying
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_modis_l1b_radiances(sensor, platform, path_to_l1b_file, &
     imager_geolocation, imager_measurements, channel_info, verbose)

   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use hdf_m, only: DFACC_READ

   implicit none

   integer(kind=lint) :: l1b_id, ix, jy, ich, err_code

   character(len=*),            intent(in)     :: sensor
   character(len=*),            intent(in)     :: platform
   character(len=*),            intent(in)     :: path_to_l1b_file
   type(imager_geolocation_t),  intent(in)     :: imager_geolocation
   type(imager_measurements_t), intent(inout)  :: imager_measurements
   type(channel_info_t),        intent(in)     :: channel_info
   logical,                     intent(in)     :: verbose

   logical                       :: lrefl
   real(kind=sreal), allocatable :: temp(:,:)

   integer(kind=4), external     :: sfstart, sfend

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_modis_l1b_radiances()'

   if (verbose) write(*,*) 'sensor: ',           trim(sensor)
   if (verbose) write(*,*) 'platform: ',         trim(platform)
   if (verbose) write(*,*) 'path_to_l1b_file: ', trim(path_to_l1b_file)

   allocate(temp(imager_geolocation%startx:imager_geolocation%endx, &
                 imager_geolocation%starty:imager_geolocation%endy))

   ! get file id
   l1b_id = sfstart(path_to_l1b_file, DFACC_READ)

   do ich = 1, channel_info%nchannels_total
      if (verbose) write(*,*) 'Read MODIS band: ', &
           channel_info%channel_ids_instr(ich)

      ! use channel_info from setup.F90
      lrefl = channel_info%channel_ids_instr(ich).lt.20 .or. &
           channel_info%channel_ids_instr(ich).eq.26

      call read_modis_l1b_radiances_2(l1b_id, &
           channel_info%channel_ids_instr(ich), lrefl, &
           imager_geolocation%startx, imager_geolocation%endx, &
           imager_geolocation%starty, imager_geolocation%endy, temp, verbose)

      if (verbose) write(*,*) 'Band minimum and maximum values: ', &
           minval(temp), maxval(temp)

      if (.not. lrefl) then
         do ix = imager_geolocation%startx, imager_geolocation%endx
            do jy = imager_geolocation%starty, imager_geolocation%endy

               temp(ix,jy) = modis_bright(platform, temp(ix,jy), &
                    channel_info%channel_ids_instr(ich), 1)

            end do
         end do
      end if

      imager_measurements%data(:,:,ich) = temp(:,:)
      where(imager_measurements%data(:,:,ich) .lt. 0) &
         imager_measurements%data(:,:,ich) = sreal_fill_value
   end do

   deallocate(temp)

   ! end access to l1b file
   err_code = sfend(l1b_id)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_modis_l1b_radiances()'

end subroutine read_modis_l1b_radiances
