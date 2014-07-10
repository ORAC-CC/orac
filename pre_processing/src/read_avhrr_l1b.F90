!-------------------------------------------------------------------------------
! Name: read_avhrr_l1b.F90
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
!
! History:
! 2012/02/01, MJ: writes initial code for reading the L1B AVHRR file.
! 2012/03/13, MJ: fixes AVHRR read bug.
! 2012/07/04, CP: removed iangle dependance of %data array
! 2012/07/05, CP: changed nmax channels to channel_info%nchannels_total
! 2013/09/06, AP: tidying, changed channel identification to use 'channel'
!   attribute within the file
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine read_avhrr_l1b(sensor,platform,path_to_l1b_file,imager_geolocation, &
     imager_measurements,channel_info)

   use hdf5
   use preproc_constants
   use imager_structures
   use channel_structures

   implicit none

   character(len=sensorlength),   intent(in)    :: sensor
   character(len=platformlength), intent(in)    :: platform
   character(len=pathlength),     intent(in)    :: path_to_l1b_file
   type(imager_geolocation_s),    intent(inout) :: imager_geolocation
   type(imager_measurements_s),   intent(inout) :: imager_measurements
   type(channel_info_s),          intent(in)    :: channel_info

   integer                       :: ichannel,err_code

   integer(kind=lint)            :: l1b_id

   real(kind=sreal), allocatable, dimension(:,:) :: temp

   character(len=6)              :: cimage,cich

   character(len=10)             :: channel_number

   allocate(temp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))

   !initialize the f90 interface for hdf5
   call h5open_f(err_code)

   !open the geo file
   call h5fopen_f(path_to_l1b_file,h5f_acc_rdonly_f,l1b_id,err_code)

   do ichannel=1,channel_info%nchannels_total
      write(cich,'(i1)') ichannel
      cimage='image'//trim(adjustl(cich))

      call read_L1B_avhrr_reflectances_radiances(l1b_id, &
           cimage,"data",cimage//'/what', &
           imager_geolocation%startx,imager_geolocation%endx, &
           imager_geolocation%starty,imager_geolocation%endy, &
           channel_number,temp)

      ! copy arrays over and bring in 1,2,3a,3b,4,5 order,
      ! storage in the imager_measurements%data array is strictLy with
      ! increasing wavelength. apply division by 100.0 to convert from percent
      ! to fraction  representation for SW reflectances. All LW channels have
      ! BTs assigned to them already.
      select case(channel_number)
      case('1') !image 1 in the file is channel 1 of AVHRR
         imager_measurements%data(:,:,1)=temp(:,:)/100.0
      case('2') !image 2 in the file is channel 2 of AVHRR
         imager_measurements%data(:,:,2)=temp(:,:)/100.0
      case('3b') !image 3 in the file is channel 3B of AVHRR
         imager_measurements%data(:,:,4)=temp(:,:)
      case('4') !image 4 in the file is channel 4 of AVHRR
         imager_measurements%data(:,:,5)=temp(:,:)
      case('5') !image 5 in the file is channel 5 of AVHRR
         imager_measurements%data(:,:,6)=temp(:,:)
      case('3a') !image 6 in the file is channel 3A of AVHRR
         imager_measurements%data(:,:,3)=temp(:,:)/100.0
      case('3B') ! just in case
         imager_measurements%data(:,:,4)=temp(:,:)
      case('3A') ! just in case
         imager_measurements%data(:,:,3)=temp(:,:)/100.0
      case default
         write(*,*) 'AVHRR channel '//channel_number//' not expected.'
         stop
      end select
   enddo

   deallocate(temp)

   !close the file
   call h5fclose_f(l1b_id, err_code)

   !close access to hdf5 interface
   call h5close_f(err_code)

end subroutine read_avhrr_l1b
