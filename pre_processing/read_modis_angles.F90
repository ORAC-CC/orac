!-------------------------------------------------------------------------------
! Name: read_modis_angles.F90
!
! Purpose:
! Read angular information from MODIS geolocation data
!
! Description and Algorithm details:
! 1) Set start, end, and stride of data read.
! 2) Read data with SFRDATA.
! 3) Set all values outside the valid range to the file's fill value. Multiply
!    those inside the range by the scale factor. Because of the if/then/else, it
!    is not necessary to initialise the input array.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! fid      int    in   A file ID returned by SFSTART
! SDS_name string in   Name of the data field to read
! ixstart  lint   in   First pixel to read across track
! ixstop   lint   in   Last pixel to read across track
! iystart  lint   in   First pixel to read along track
! iystop   lint   in   Last pixel to read along track
! rtemp    real   out  Initialised array into which data is stored
!
! History:
! 2011/12/14, MJ: produces draft code which reads modis angles
! 2013/09/06, AP: tidying, switched to using MODIS file's fill value to be
!    consistent with read_modis_lat_lon.F90, use sreal_fill_value rather than
!    file's own value
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_modis_angles(fid,SDS_name,ixstart,ixstop,iystart,iystop,rtemp)

   use preproc_constants_m

   implicit none

   integer,            intent(in)  :: fid
   character(len=*),   intent(in)  :: SDS_name
   integer(kind=lint), intent(in)  :: ixstart, ixstop, iystart, iystop
   real(kind=sreal),   intent(out) :: rtemp(ixstart:ixstop,iystart:iystop)

   integer            :: err_code
   integer            :: file_id, var_id, attr_id
   integer            :: start(2), stride(2), edge(2)
   real(kind=dreal)   :: sf
   integer(kind=sint) :: vr(2), fv
   integer(kind=sint), allocatable :: stemp(:,:)

   integer(kind=4), external :: sfselect, sfn2index, sffattr, sfrattr, sfrdata, &
                                sfendacc

   start(1) = ixstart-1
   start(2) = iystart-1
   stride = 1
   edge(1) = ixstop-ixstart+1
   edge(2) = iystop-iystart+1

   allocate(stemp(ixstart:ixstop,iystart:iystop))

   file_id = fid
   var_id = sfselect(file_id, sfn2index(file_id, SDS_name))

   err_code = sfrdata(var_id, start, stride, edge, stemp)

   attr_id = sffattr(var_id, "scale_factor")
   err_code = sfrattr(var_id, attr_id, sf)

   attr_id = sffattr(var_id, "_FillValue")
   err_code = sfrattr(var_id, attr_id, fv)

   attr_id = sffattr(var_id, "valid_range")
   err_code = sfrattr(var_id, attr_id, vr)

   ! overwrite fill values with the ORAC value
   where(stemp .ge. vr(1) .and. stemp .le. vr(2))
      rtemp = real(stemp*sf,kind=sreal)
   else where
      rtemp = sreal_fill_value
   end where

   err_code=sfendacc(var_id)

   deallocate(stemp)

end subroutine read_modis_angles
