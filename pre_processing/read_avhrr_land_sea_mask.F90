!-------------------------------------------------------------------------------
! Name: read_avhrr_land_sea_mask.F90
!
! Purpose:
! Open and read AVHRR pixel based land/sea mask information.
!
! Description and Algorithm details:
! 1) Determine filename for physiography file from geolocation name.
! 2) Allocate arrays and open HDF file.
! 3) Call read routine.
! 4) Convert AVHRR flag to binary ORAC flag.
!
! Arguments:
! Name                Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_geo_file    string in   Full path to geolocation data
! imager_geolocation  struct both Summary of pixel positions
! imager_angles       struct both Summary of sun/satellite viewing angles
! imager_flags        struct both Summary of land/sea/ice flags
! imager_time         struct both Summary of pixel observation time
!
! History:
! 2012/05/15, MJ: writes code to read land/sea information for AVHRR.
! 2013/09/12, AP: tidying, added check that file exists before opening
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_avhrr_land_sea_mask(path_to_geo_file,imager_geolocation, &
     imager_angles,imager_flags,imager_time)

   use hdf5
   use imager_structures_m
   use preproc_constants_m

   implicit none

   character(len=path_length), intent(in)    :: path_to_geo_file
   type(imager_geolocation_t), intent(inout) :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles
   type(imager_flags_t),       intent(inout) :: imager_flags
   type(imager_time_t),        intent(inout) :: imager_time

   character(len=path_length)                      :: path_to_lsmask_file

   integer(kind=lint)                              :: geo_id,ix,jy,iunderscore

   integer(kind=lint), allocatable, dimension(:,:) :: btemp

   integer                                         :: err_code
   logical                                         :: check

   iunderscore=scan(trim(adjustl(path_to_geo_file)),'_',back=.true.)
   path_to_lsmask_file=trim(adjustl(path_to_geo_file))
   path_to_lsmask_file=path_to_lsmask_file(1:iunderscore)//'physiography.h5'
   inquire(file=path_to_lsmask_file,exist=check)
   if (.not. check) then
      write(*,*) 'ERROR: read_avhrr_land_sea_mask(): AVHRR physiography ' // &
                & 'file does not exist, filename: ', trim(path_to_lsmask_file)
      stop error_stop_code
   endif

   !allocate temporary data
   allocate(btemp(imager_geolocation%startx:imager_geolocation%endx, &
        imager_geolocation%starty:imager_geolocation%endy))

   !initialize the f90 interface for hdf5
   call h5open_f(err_code)

   !open the geo file
   call h5fopen_f(path_to_lsmask_file,h5f_acc_rdonly_f,geo_id,err_code)

   !read lsmask
   call read_avhrr_land_sea_mask_2(geo_id,"/","1kmLanduse", &
        imager_geolocation%startx,imager_geolocation%endx, &
        imager_geolocation%starty,imager_geolocation%endy,btemp)


!!$ make orac ls flag by mapping the MODIS L/S definitions to the ones for ORAC
!!$ (approximate). AVHRR DEFINITIONS:
!!$                16:      water
!!$             != 16:      various land types
!   where(btemp .eq. 16)
!      btemp = 0
!   elsewhere
!      btemp = 1
!   end where
   do ix=imager_geolocation%startx,imager_geolocation%endx
      do jy=imager_geolocation%starty,imager_geolocation%endy
         if (btemp(ix,jy) .eq. 16) then
            btemp(ix,jy) = 0
         else
            btemp(ix,jy) = 1
         end if
      end do
   end do

   imager_flags%lsflag=int(btemp,kind=byte)

   !free temp arrays
   deallocate(btemp)

   !close the file
   call h5fclose_f(geo_id, err_code)

   !close access to hdf5 interface
   call h5close_f(err_code)

end subroutine read_avhrr_land_sea_mask
