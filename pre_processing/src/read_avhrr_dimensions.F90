! Name: read_avhrr_dimensions.f90
!
!
! Purpose:
! Open geo input file to determine the size of the data array for the purposes
! of dynamic array allocation.
!
! Description and Algorithm details:
! 1) Open geolocation file
! 2) Access the Longitude field and output its dimensions
! 3) Close file
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_geo_file string in  Full path to geolocation file
! n_across_track   lint   out Number of pixels available perpendicular to the
!                             direction of travel
! n_along_track    lint   out Number of pixels in the direction of travel
!
! History:
! 2012/01/24: MJ writes initial code for AVHRR.
! 2013/09/11: AP tidying, removed path_to_l1b_file
!
! $Id$
!
! Bugs:
! none known
!

subroutine read_avhrr_dimensions(path_to_geo_file,n_across_track,n_along_track)

   use HDF5
   use preproc_constants

   implicit none

   character(len=pathlength), intent(in) :: path_to_geo_file
   integer(kind=lint), intent(out)       :: n_across_track, n_along_track

   integer(kind=HID_T)       :: gr_id,dset_id,dspace_id

   integer(kind=lint)        :: rank
  
   integer(kind=lint)        :: err_code

   integer(kind=HSIZE_T)     :: dims(2),maxdims(2)

   integer(kind=HID_T)       :: geo_id

   !initialize the f90 interface for hdf5
   call h5open_f(err_code)

   !open the geo file
   call h5fopen_f(path_to_geo_file,h5f_acc_rdonly_f,geo_id,err_code)

   !open the data group
   call h5gopen_f(geo_id,'where/lon',gr_id,err_code)

   !open the dataset
   call h5dopen_f(gr_id,'data',dset_id,err_code)

   !get dataspace id
   call h5dget_space_f(dset_id,dspace_id,err_code)

   !get rank
   call h5sget_simple_extent_ndims_f(dspace_id,rank,err_code)

   !get dimensions
   call h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,err_code)

   n_across_track=int(dims(1),kind=lint)
   n_along_track=int(dims(2),kind=lint)

   !close dataset
   call  h5dclose_f(dset_id, err_code)

   !close data group
   call h5gclose_f(gr_id, err_code)

   !close the file
   call h5fclose_f(geo_id, err_code) 

   !close access to hdf5 interface
   call h5close_f(err_code)

end subroutine read_avhrr_dimensions
