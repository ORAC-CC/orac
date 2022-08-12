!-------------------------------------------------------------------------------
! Name: read_hdf_sd_dims.F90
!
! Purpose:
!
! Inputs:
!
! Output:
!
! History:
! xxxx/xx/xx, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_hdf_sd_dims(filename,SDS_name,nX,nY)

   use common_constants_m
   use hdf_m, only: DFACC_READ

   implicit none

   ! Input arguments
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: SDS_name

   ! Output arguments
   integer(kind=lint), intent(out) :: nX,nY

   ! Local variables
   integer :: fid
   integer :: err_code
   integer :: var_id
   integer(kind=lint) :: dummy_type,dummy_numattrs,dummy_rank
   integer, dimension(2) :: dimsizes
   character(len=MAX_NC_NAME) :: dummy_name

   integer(kind=4), external :: sfstart,sfselect,sfginfo,sfn2index
   integer(kind=4), external :: sfendacc,sfend

   ! Get file id
   fid = sfstart(filename,DFACC_READ)

   var_id = sfselect(fid, sfn2index(fid, SDS_name))

   err_code = sfginfo(var_id,dummy_name,dummy_rank,dimsizes,dummy_type, &
                      dummy_numattrs)

   ! Read from netCDF file
   nX=int(dimsizes(1))
   nY=int(dimsizes(2))

   err_code=sfendacc(var_id)

   ! End access to hdfile
   err_code=sfend(fid)

!  print*,'var_id: ',var_id
!  print*,'name: ',trim(dummy_name)
!  print*,'rank: ',dummy_rank
!  print*,'dimensions: ',dimsizes
!  print*,'type: ',dummy_type
!  print*,'numattrs: ',dummy_numattrs

end subroutine read_hdf_sd_dims
