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
!    attribute within the file
! 2014/07/23, AP: don't apply 0.01 scale factor to missing values.
! 2015/01/15, AP: Permit arbitrary ordering of the channels.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_avhrr_l1b_radiances(sensor,platform,path_to_l1b_file, &
     imager_geolocation,imager_measurements,channel_info,verbose)

   use channel_structures_m
   use hdf5
   use imager_structures_m
   use preproc_constants_m

   implicit none

   character(len=*),            intent(in)    :: sensor
   character(len=*),            intent(in)    :: platform
   character(len=*),            intent(in)    :: path_to_l1b_file
   type(imager_geolocation_t),     intent(inout) :: imager_geolocation
   type(imager_measurements_t),    intent(inout) :: imager_measurements
   type(channel_info_t),           intent(in)    :: channel_info
   logical,                        intent(in)    :: verbose

   integer                                       :: ichannel,err_code
   integer(kind=HID_T)                           :: l1b_id
   real(kind=sreal), allocatable, dimension(:,:) :: temp
   character(len=1)                              :: cich
   character(len=6)                              :: cimage
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
      ! Translate our instrument channel number into the actual instrument number
      ! We give inst ch numbers in increasing wavelength. The AVHRR numbers do
      ! that except that the 1.6um channel was added later - hence it's #6.
      select case (channel_info%channel_ids_instr(ichannel))
      case(1)
         cich='1'
      case(2)
         cich='2'
      case(3)
         cich='6'
      case(4)
         cich='3'
      case(5)
         cich='4'
      case(6)
         cich='5'
      case default
         write(*,*) 'ERROR: read_avhrr_l1b_radiances(): Invalid channel number.'
         stop error_stop_code
      end select
      cimage='image'//cich

      if (verbose) write(*,*) 'Read AVHRR channel: '//cich

      call read_avhrr_l1b_radiances_2(l1b_id, &
           cimage,"data",cimage//'/what', &
           imager_geolocation%startx,imager_geolocation%endx, &
           imager_geolocation%starty,imager_geolocation%endy, &
           channel_number,temp,verbose)

      ! copy arrays over. apply division by 100.0 to convert from percent
      ! to fraction  representation for SW reflectances. All LW channels have
      ! BTs assigned to them already.
      ! image1,2,3,4,5,6 => Ch 1,2,3B,4,5,3A
      select case(channel_number)
      case('1','2','3a','3A')
         where (temp .ne. sreal_fill_value)
            imager_measurements%data(:,:,ichannel)=temp/100.0
         end where
      case('3b','3B','4','5')
         imager_measurements%data(:,:,ichannel)=temp
      case default
         write(*,*) 'ERROR: read_avhrr_l1b_radiances(): invalid AVHRR ', &
                    'channel: ', channel_number
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
