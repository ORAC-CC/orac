!-------------------------------------------------------------------------------
! Name: read_modis_lat_lon.F90
!
! Purpose:
! Read latitude and longitude MODIS geolocation data
!
! Description and Algorithm details:
! 1) Set start, end, and stride of data read.
! 2) Read data with SFRDATA.
! 3) Set all values outside the valid range to the file's fill value.
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! fid      int    in   A file ID returned by SFSTART
! SDS_name string in   Name of the data field to read
! startx   lint   in   First pixel to read across track
! stopx    lint   in   Last pixel to read across track
! starty   lint   in   First pixel to read along track
! stopy    lint   in   Last pixel to read along track
! temp     real   out  Initialised array into which data is stored
!
! History:
! 2011/12/13, MJ: produces draft code which reads latitude and longitude modis
!    geolocation data
! 2013/09/06, AP: tidying, added code for WHERE statement (if desired), use
!    sreal_fill_value rather than file's value
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_modis_lat_lon(fid,SDS_name,startx,stopx,starty,stopy,temp)

   use preproc_constants_m

   implicit none

   integer,            intent(in)  :: fid
   character(len=*),   intent(in)  :: SDS_name
   integer(kind=lint), intent(in)  :: startx, stopx, starty, stopy
   real(kind=sreal),   intent(out) :: temp(startx:stopx,starty:stopy)

!  integer(kind=lint) :: ix,jy
   integer            :: err_code
   integer            :: var_id, attr_id
   integer            :: start(2), stride(2), edge(2)
   real(kind=sreal)   :: fv, vr(2)

   integer(kind=4), external :: sfselect, sfn2index, sffattr, sfrattr, sfrdata
   integer(kind=4), external :: sfendacc

   start(1) = startx-1
   start(2) = starty-1
   stride = 1
   edge(1) = stopx-startx+1
   edge(2) = stopy-starty+1

   var_id = sfselect(fid, sfn2index(fid, SDS_name))

   ! read data into array
   err_code = sfrdata(var_id, start, stride, edge, temp)

   attr_id = sffattr(var_id, "_FillValue")
   err_code = sfrattr(var_id, attr_id, fv)

   attr_id = sffattr(var_id, "valid_range")
   err_code = sfrattr(var_id, attr_id, vr)

   ! overwrite fill values with the ORAC value
   where (temp.lt.vr(1) .or. temp.gt.vr(2)) temp = sreal_fill_value

   err_code = sfendacc(var_id)

end subroutine read_modis_lat_lon
