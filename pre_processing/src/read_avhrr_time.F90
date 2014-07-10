!-------------------------------------------------------------------------------
! Name: read_avhrr_lat_lon.F90
!
! Purpose:
! Read AVHRR time geolocation data
!
! Description and Algorithm details:
! 1) Open the attribute group.
! 2) Read the attributes.
! 3) Close the file.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! fid         HID_T  in   HDF file ID from H5Fopen_f
! attrgroup   string in   Group in which
! startepochs lint   out  Start time for the orbit, in seconds
! endepochs   lint   out  End time for the orbit, in seconds
!
! History:
! 2012/01/24, MJ: adds code to read AVHRR HDF5 file.
! 2013/09/06, AP: tidying
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine read_avhrr_time(fid, attrgroup, startepochs, endepochs)

   use hdf5
   use preproc_constants

   implicit none

   integer(kind=HID_T), intent(in)  :: fid
   character(len=*),    intent(in)  :: attrgroup
   integer(kind=lint),  intent(out) :: startepochs,endepochs

   integer               :: err_code

   integer(kind=HID_T)   :: dset_id2,attr_id

   integer(kind=HSIZE_T) :: adims(1)

   !open data group where attributes are stored
   call h5gopen_f(fid,attrgroup,dset_id2,err_code)
   !because those are all scalar atributes
   adims(1)=1

   !startepochs
   !get attribute id
   call h5aopen_name_f(dset_id2,'startepochs',attr_id,err_code)
   !read now the attribute
   call h5aread_f(attr_id,H5T_NATIVE_INTEGER,startepochs,adims,err_code)
   !close attribute
   call h5aclose_f(attr_id,err_code)

   !startepochs
   !get attribute id
   call h5aopen_name_f(dset_id2,'endepochs',attr_id,err_code)
   !read now the attribute
   call h5aread_f(attr_id,H5T_NATIVE_INTEGER,endepochs,adims,err_code)
   !close attribute
   call h5aclose_f(attr_id,err_code)

   !close attribute group
   call h5gclose_f(dset_id2, err_code)

end subroutine read_avhrr_time
