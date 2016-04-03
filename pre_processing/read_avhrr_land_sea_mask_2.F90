!-------------------------------------------------------------------------------
! Name: read_avhrr_land_sea_mask_2.F90
!
! Purpose:
! Read land/sea mask for AVHRR from physiography data
!
! Description and Algorithm details:
! 1) Set start, end, and stride of data read.
! 2) Open data group, data set, and allocate data space. Read data.
! 3) Close file.
!
! Arguments:
! Name      Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! fid       HID_T  in   HDF file ID from H5Fopen_f
! group     string in   Name of the group to open
! dataset   string in   Name of the dataset within that group to open
! attrgroup string in   Name of the group where the attributes are stored
! startx    lint   in   First pixel to read across track
! stopx     lint   in   Last pixel to read across track
! starty    lint   in   First pixel to read along track
! stopy     lint   in   Last pixel to read along track
! btemp     lint   both Array into which data will be stored
!
! History:
! 2012/05/15, MJ: adds code to read AVHRR HDF5 land sea mask file.
! 2013/09/11, AP: tidying
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_avhrr_land_sea_mask_2(fid,group,dataset,startx,stopx,starty,stopy, &
     btemp)

   use hdf5
   use preproc_constants_m

   implicit none

   integer(kind=HID_T), intent(in)  :: fid
   character(len=*),    intent(in)  :: dataset, group
   integer(kind=lint),  intent(in)  :: startx,stopx,starty,stopy
   integer(kind=lint),  intent(out) :: btemp(startx:stopx,starty:stopy)

   integer               :: err_code

   integer(kind=HID_T)   :: gr_id,dset_id,dspace_id,mem_id

   integer(kind=HSIZE_T) :: start(2), stride(2), edge(2)

   !open the data group
   call h5gopen_f(fid,group,gr_id,err_code)
   !open the dataset
   call h5dopen_f(gr_id,dataset,dset_id,err_code)
   !get dataspace id
   call h5dget_space_f(dset_id,dspace_id,err_code)
   !define parameters for size of subset and convert to appropriate datatype
   start(1) = int(startx-1,kind=HSIZE_T)
   start(2) = int(starty-1,kind=HSIZE_T)
   stride = int(1,kind=HSIZE_T)
   edge(1) = int(stopx-startx+1,kind=HSIZE_T)
   edge(2) = int(stopy-starty+1,kind=HSIZE_T)

   !select subset of data
   call h5sselect_hyperslab_f(dspace_id,H5S_SELECT_SET_F,start,edge,err_code, &
        stride)

   !create memory dataspace
   call h5screate_simple_f(2,edge,mem_id,err_code)
   !now finally read the data
   btemp = -1
   call h5dread_f(dset_id,H5T_NATIVE_INTEGER,btemp,edge,err_code,mem_id, &
        dspace_id)

   !close dataspace
   call h5sclose_f(mem_id,err_code)

   !close memspace
   call h5sclose_f(dspace_id,err_code)

   !close dataset
   call h5dclose_f(dset_id, err_code)

   !close data group
   call h5gclose_f(gr_id, err_code)

end subroutine read_avhrr_land_sea_mask_2
