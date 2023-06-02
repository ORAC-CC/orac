!-------------------------------------------------------------------------------
! Name: read_hdf_sd_data.F90
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

subroutine read_hdf_sd_data(filename,SDS_name,elm3d,nX,nY,temp_out)

   use common_constants_m
   use hdf_m, only: DFACC_READ

   implicit none

   ! Input arguments
   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: SDS_name
   integer, intent(in) :: elm3d ! index of 3d array to output
   integer(kind=lint), intent(in) :: nX,nY

   ! Output arguments
   real(kind=sreal), intent(out) :: temp_out(1:nX,1:nY)

   ! Local variables
   integer :: fid
   integer(kind=lint) :: startx,starty
   integer(kind=lint) :: stopx,stopy
   integer(kind=lint) :: n3D
   real(kind=sreal), allocatable :: temp(:,:)

   ! Specifying type here is problematic - cannot figure this out...
   ! Integer type - dummy_type = 22
   integer(kind=sint), allocatable :: temp2D_int(:,:)
   integer(kind=sint), allocatable :: temp3D_int(:,:,:)

   ! Float type - dummy_type = 5
   real(kind=sreal), allocatable :: temp2D_flt(:,:)
   real(kind=sreal), allocatable :: temp3D_flt(:,:,:)

   ! Byte type - dummy_type = 20
   integer(kind=1), allocatable :: temp2D_byt(:,:)
   integer(kind=1), allocatable :: temp3D_byt(:,:,:)

   integer :: err_code
   integer :: var_id,attr_id
   integer, allocatable :: start(:),stride(:),edge(:)

   integer(kind=sreal) :: fv
   real(kind=dreal) :: scale,offset
   integer(kind=lint) :: dummy_type,dummy_numattrs,dummy_rank
   integer, dimension(3) :: dimsizes
   character(len=MAX_NC_NAME) :: dummy_name
   integer(kind=lint) :: ix, jy

   real(kind=sreal) :: fv_flt
   integer(kind=sint) :: fv_int
   integer(kind=1) :: fv_byt

   integer(kind=4), external :: sfstart,sfselect,sfginfo,sfn2index,sffattr
   integer(kind=4), external :: sfrattr,sfrdata,sfendacc,sfend

   ! Read whole granule
   startx=int(1) ! start x at 1
   starty=int(1) ! start y at 1

   ! Get file id
   fid=sfstart(filename,DFACC_READ)

   var_id = sfselect(fid, sfn2index(fid, SDS_name))

   err_code=sfginfo(var_id,dummy_name,dummy_rank,dimsizes,dummy_type, &
        dummy_numattrs)

   print*,'Fetching: ',trim(dummy_name)
   print*,'Rank    : ',dummy_rank
   print*,'Dimensions: ',dimsizes

   ! Read from netCDF file
   stopx=int(dimsizes(1))
   stopy=int(dimsizes(2))

   ! Allocate output arrays
   allocate(temp(startx:stopx,starty:stopy))

   ! 2D variable
   if (dummy_rank .eq. 2) then
      allocate(temp2D_int(startx:stopx,starty:stopy))
      allocate(temp2D_flt(startx:stopx,starty:stopy))
      allocate(temp2D_byt(startx:stopx,starty:stopy))
      allocate(start(dummy_rank))
      allocate(stride(dummy_rank))
      allocate(edge(dummy_rank))

      ! Granule dimensions
      start(1) = startx-1
      start(2) = starty-1
      stride = 1
      edge(1) = stopx-startx+1
      edge(2) = stopy-starty+1

      ! Read data into array
      if (dummy_type .eq. 20) then
         err_code = sfrdata(var_id, start, stride, edge, temp2D_byt)
         temp = temp2D_byt
         attr_id  = sffattr(var_id, "_FillValue")
         err_code = sfrattr(var_id, attr_id, fv_byt)
         !fv = fv_byt
         fv = -1 ! there is no fill value for some reason attr_id is found??
      end if
      if (dummy_type .eq. 22) then
         err_code = sfrdata(var_id, start, stride, edge, temp2D_int)
         temp = temp2D_int
         attr_id  = sffattr(var_id, "_FillValue")
         err_code = sfrattr(var_id, attr_id, fv_int)
         fv = fv_int
      end if
      if (dummy_type .eq. 5) then
         err_code = sfrdata(var_id, start, stride, edge, temp2D_flt)
         temp = temp2D_flt
         attr_id  = sffattr(var_id, "_FillValue")
         err_code = sfrattr(var_id, attr_id, fv_flt)
         fv = fv_flt
      end if

      attr_id  = sffattr(var_id, "scale_factor")
      err_code = sfrattr(var_id, attr_id, scale)

      attr_id  = sffattr(var_id, "add_offset")
      err_code = sfrattr(var_id, attr_id, offset)
   end if

   ! 3D variable
   if (dummy_rank .eq. 3) then
      n3D = dimsizes(3)
      allocate(temp3D_byt(startx:stopx,starty:stopy,1:n3D))
      allocate(temp3D_flt(startx:stopx,starty:stopy,1:n3D))
      allocate(temp3D_int(startx:stopx,starty:stopy,1:n3D))
      allocate(start(dummy_rank))
      allocate(stride(dummy_rank))
      allocate(edge(dummy_rank))

      ! Granule dimensions
      start(1) = startx-1
      start(2) = starty-1
      start(3) = n3D-1
      stride = 1
      edge(1) = stopx-startx+1
      edge(2) = stopy-starty+1
      edge(3) = 1

      ! Read data into array
      if (dummy_type .eq. 20) then
         err_code = sfrdata(var_id, start, stride, edge, temp3D_byt)
         temp = temp3D_byt(:,:,elm3d)
         attr_id  = sffattr(var_id, "_FillValue")
         err_code = sfrattr(var_id, attr_id, fv_byt)
!        fv = fv_byt
         fv = -1 ! there is no fill value for some reason attr_id is found??
      end if
      if (dummy_type .eq. 22) then
         err_code = sfrdata(var_id, start, stride, edge, temp3D_int)
         temp = temp3D_int(:,:,elm3d)
         attr_id  = sffattr(var_id, "_FillValue")
         err_code = sfrattr(var_id, attr_id, fv_int)
         fv = fv_int
      end if
      if (dummy_type .eq. 5) then
         err_code = sfrdata(var_id, start, stride, edge, temp3D_flt)
         temp = temp3D_flt(:,:,elm3d)
         attr_id  = sffattr(var_id, "_FillValue")
         err_code = sfrattr(var_id, attr_id, fv_flt)
         fv = fv_flt
      end if

      attr_id  = sffattr(var_id, "scale_factor")
      err_code = sfrattr(var_id, attr_id, scale)

      attr_id  = sffattr(var_id, "add_offset")
      err_code = sfrattr(var_id, attr_id, offset)
   end if

   err_code=sfendacc(var_id)

   ! End access to hdfile
   err_code=sfend(fid)

   ! Overwrite fill values with the ORAC value
   do jy=1,stopy
      do ix=1,stopx
         if (temp(ix,jy) .ne. fv) then
            temp_out(ix,jy) = (real(temp(ix,jy),kind=sreal) - &
                              offset) * scale
         else
            temp_out(ix,jy) = sreal_fill_value
         end if
      end do
   end do

end subroutine read_hdf_sd_data
