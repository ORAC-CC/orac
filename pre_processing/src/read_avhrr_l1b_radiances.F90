!-------------------------------------------------------------------------------
! Name: read_avhrr_l1b_radiances.F90
!
! Purpose:
! Read the L1b AVHRR HDF5 file.
!
! Description and Algorithm details:
! 1) Allocate temporary array.
! 2) Open file.
! 3) Call L1B read routine.
! 4) Sort channels into ascending wavelength and apply scale factors.
! 5) Close file.
!
! Arguments:
! Name                Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! sensor              string in   Name of instrument
! platform            string in   Name of satellite
! path_to_l1b_file    string in   Full path to level 1B data
! imager_geolocation  struct both Summary of pixel positions
! imager_measurements struct both Satellite observations
! channel_info        struct in   Summary of channel information
! verbose             logic  in   T: print status information; F: don't
!
! History:
! 2012/02/01, MJ: writes initial code for reading the L1B AVHRR file.
! 2012/03/13, MJ: fixes AVHRR read bug.
! 2012/07/04, CP: removed iangle dependence of %data array
! 2012/07/05, CP: changed nmax channels to channel_info%nchannels_total
! 2013/09/06, AP: tidying, changed channel identification to use 'channel'
!   attribute within the file
! 2014/07/23, AP: don't apply 0.01 scale factor to missing values.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_avhrr_l1b_radiances(sensor,platform,path_to_l1b_file,imager_geolocation, &
     imager_measurements,channel_info,verbose)

   use channel_structures
   use hdf5
   use imager_structures
   use preproc_constants

   implicit none

   character(len=sensor_length),   intent(in)    :: sensor
   character(len=platform_length), intent(in)    :: platform
   character(len=path_length),     intent(in)    :: path_to_l1b_file
   type(imager_geolocation_s),     intent(inout) :: imager_geolocation
   type(imager_measurements_s),    intent(inout) :: imager_measurements
   type(channel_info_s),           intent(in)    :: channel_info
   logical,                        intent(in)    :: verbose

   integer                                       :: ichannel,err_code
   integer(kind=lint)                            :: l1b_id
   real(kind=sreal), allocatable, dimension(:,:) :: temp
   character(len=6)                              :: cimage,cich
   character(len=10)                             :: channel_number

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_avhrr_l1b_radiances()'

   if (verbose) write(*,*) 'sensor: ',           trim(sensor)
   if (verbose) write(*,*) 'platform: ',         trim(platform)
   if (verbose) write(*,*) 'path_to_l1b_file: ', trim(path_to_l1b_file)

   allocate(temp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))

   !initialize the f90 interface for hdf5
   call h5open_f(err_code)

   !open the geo file
   call h5fopen_f(path_to_l1b_file,h5f_acc_rdonly_f,l1b_id,err_code)

   do ichannel=1,channel_info%nchannels_total
      if (verbose) write(*,*) 'Read AVHRR channel: ', ichannel

      write(cich,'(i1)') ichannel
      cimage='image'//trim(adjustl(cich))

      call read_avhrr_l1b_radiances_2(l1b_id, &
           cimage,"data",cimage//'/what', &
           imager_geolocation%startx,imager_geolocation%endx, &
           imager_geolocation%starty,imager_geolocation%endy, &
           channel_number,temp,verbose)

      ! copy arrays over and bring in 1,2,3a,3b,4,5 order,
      ! storage in the imager_measurements%data array is strictLy with
      ! increasing wavelength. apply division by 100.0 to convert from percent
      ! to fraction  representation for SW reflectances. All LW channels have
      ! BTs assigned to them already.
      select case(channel_number)
      case('1') !image 1 in the file is channel 1 of AVHRR
         where (temp .ne. sreal_fill_value)
            imager_measurements%data(:,:,1)=temp/100.0
         end where
      case('2') !image 2 in the file is channel 2 of AVHRR
         where (temp .ne. sreal_fill_value)
            imager_measurements%data(:,:,2)=temp/100.0
         end where
      case('3b') !image 3 in the file is channel 3B of AVHRR
         imager_measurements%data(:,:,4)=temp
      case('4') !image 4 in the file is channel 4 of AVHRR
         imager_measurements%data(:,:,5)=temp
      case('5') !image 5 in the file is channel 5 of AVHRR
         imager_measurements%data(:,:,6)=temp
      case('3a') !image 6 in the file is channel 3A of AVHRR
         where (temp .ne. sreal_fill_value)
            imager_measurements%data(:,:,3)=temp/100.0
         end where
      case('3B') ! just in case
         imager_measurements%data(:,:,4)=temp
      case('3A') ! just in case
         where (temp .ne. sreal_fill_value)
            imager_measurements%data(:,:,3)=temp/100.0
         end where
      case default
         write(*,*) 'ERROR: read_avhrr_l1b_radiances(): invalid AVHRR channel: ', &
                    channel_number
         stop error_stop_code
      end select
   end do

   deallocate(temp)

   !close the file
   call h5fclose_f(l1b_id, err_code)

   !close access to hdf5 interface
   call h5close_f(err_code)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_avhrr_l1b_radiances()'

end subroutine read_avhrr_l1b_radiances
