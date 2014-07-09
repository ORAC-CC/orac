! Name: read_imager.F90
!
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
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! sensor              string in   Name of instrument
! platform            string in   Name of satellite
! path_to_l1b_file    string in   Full path to level 1B data
! imager_geolocation  struct both Summary of pixel positions
! imager_measurements struct both Satellite observations
! channel_info        struct in   Summary of channel information
! verbose             logic  in   F: minimise information printed to screen;
!                                 T: don't
!
! History:
! 2011/12/21: MJ produces draft code which opens and reads MODIS L1b hdf files
! 2013/09/06: AP tidying
!
! $Id$
!
! Bugs:
! none known
!

subroutine read_modis_l1b(sensor,platform,path_to_l1b_file,imager_geolocation, &
     imager_measurements,channel_info,verbose)

   use preproc_constants
   use imager_structures
   use channel_structures

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   integer(kind=lint) :: l1b_id, ix, jy,ich, err_code

   character(len=sensorlength), intent(in)    :: sensor
   character(len=platformlength), intent(in)  :: platform
   character(len=pathlength), intent(in)      :: path_to_l1b_file
   type(imager_geolocation_s), intent(inout)  :: imager_geolocation
   type(imager_measurements_s), intent(inout) :: imager_measurements
   type(channel_info_s), intent(in)           :: channel_info
   logical, intent(in)                        :: verbose

   logical                                       :: lrefl

   real(kind=sreal), allocatable, dimension(:,:) :: temp

   allocate(temp(imager_geolocation%startx:imager_geolocation%endx,&
        & imager_geolocation%starty:imager_geolocation%endy))

   !get file id
   l1b_id=sfstart(path_to_l1b_file,DFACC_READ)
   write(*,*) 'L1B FILE:',trim(path_to_l1b_file),l1b_id

   do ich=1,channel_info%nchannels_total
      ! use channel_info from SETUP.F90
      lrefl = channel_info%channel_ids_instr(ich).lt.20 .or. &
           channel_info%channel_ids_instr(ich).eq.26

      call read_L1B_modis_reflectances_radiances(l1b_id, &
           channel_info%channel_ids_instr(ich),lrefl, &
           imager_geolocation%startx,imager_geolocation%endx, &
           imager_geolocation%starty,imager_geolocation%endy,temp,verbose)
      if(verbose) write(*,*) 'MODIS band ', &
           channel_info%channel_ids_instr(ich),': ',minval(temp),maxval(temp)

      if(.not. lrefl) then
         do ix=imager_geolocation%startx,imager_geolocation%endx
            do jy=imager_geolocation%starty,imager_geolocation%endy

               temp(ix,jy)=MODIS_BRIGHT(platform,temp(ix,jy), &
                    channel_info%channel_ids_instr(ich),1)

            enddo
         enddo
      endif
      imager_measurements%data(:,:,ich)=temp(:,:)

   enddo

   deallocate(temp)

   !end access to l1b file
   err_code=sfend(l1b_id)

end subroutine read_modis_l1b
