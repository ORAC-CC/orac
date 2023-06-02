!-------------------------------------------------------------------------------
! Name: read_modis_time.F90
!
! Purpose:
! Read MODIS time data from hdf file
!
! Description and Algorithm details:
! 1) Set start, end, and stride of data read.
! 2) Read data with SFRDATA.
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! fid      int    in   A file ID returned by SFSTART
! SDS_name string in   Name of the data field to read
! startyy  lint   in   First pixel to read across track
! stopyy   lint   in   Last pixel to read across track
! temp     dreal  both Initialised array into which data is stored
!
! History:
! 2011/12/16, MJ: produces draft code
! 2013/09/11, AP: tidying, removed fv
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_modis_time(fid,SDS_name,startyy,stopyy,temp)

   use preproc_constants_m

   implicit none

   integer,            intent(in)  :: fid
   character(len=*),   intent(in)  :: SDS_name
   integer(kind=lint), intent(in)  :: startyy, stopyy
   real(kind=dreal),   intent(out) :: temp(startyy:stopyy-1)

   integer :: err_code

   integer :: file_id, var_id !, attr_id
   integer :: start(1), stride(1), edge(1)

   integer(kind=4), external  :: sfselect, sfn2index, sfrdata, sfendacc

   start(1) = startyy-1
   stride = 1
   edge(1) = stopyy-startyy+1

   file_id = fid
   var_id = sfselect(file_id, sfn2index(file_id, SDS_name))

   err_code = sfrdata(var_id, start, stride, edge, temp)

!  no need for fill value so commented out (assumes all data fine)
!   attr_id=sffattr(var_id, "_FillValue")
!   err_code=sfrattr(var_id, attr_id, fv)

   err_code=sfendacc(var_id)

end subroutine read_modis_time
