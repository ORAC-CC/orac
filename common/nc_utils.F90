!-------------------------------------------------------------------------------
! Name: nc_create.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2015/07/16, GM: Original version.
!
! $Id: nc_create.F90 2355 2014-09-09 23:16:38Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_create(path, ncid, nx, ny, dims_var, type, global_atts, source_atts)

   use netcdf

   use common_constants
   use global_attributes
   use source_attributes

   implicit none

   ! Input
   character(len=*),          intent(in)    :: path
   integer,                   intent(in)    :: nx, ny
   integer,                   intent(in)    :: type

   ! Output
   integer,                   intent(out)   :: ncid
   integer,                   intent(out)   :: dims_var(2)

   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts

   ! Local
   integer :: ierr


   !----------------------------------------------------------------------------
   ! Create new file
   !----------------------------------------------------------------------------
   ierr = nf90_create(path, IOR(IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL), &
                      NF90_CLOBBER), ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_create(): filename = ', trim(path)
      stop error_stop_code
   end if

   ! Define the 2 dimensions: lat / lon
   ierr = nf90_def_dim(ncid, 'across_track', nx, dims_var(1))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(): dim_name = across_track, xdim = ', dims_var(1)
      stop error_stop_code
   end if

   ierr = nf90_def_dim(ncid, 'along_track', ny, dims_var(2))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(): dim_name = along_track,  ydim = ', dims_var(2)
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Write global attributes to the netcdf output
   !----------------------------------------------------------------------------
   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   if (type .eq. 1) then
      global_atts%title = 'ESA Cloud CCI Retrieval Products L2 Primary File'
   else if (type .eq. 2) then
      global_atts%title = 'ESA Cloud CCI Retrieval Products L2 Secondary File'
   else
      write(*,*) 'ERROR: nf90_create(): invalid file type: ', type
      stop error_stop_code
   end if

   call nc_put_common_attributes(ncid, global_atts,source_atts)


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   ierr = nf90_enddef(ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

end subroutine nc_create


subroutine prepare_short_packed_float(value_in, value_out, &
                                      scale_factor, add_offset, &
                                      fill_value_in, fill_value_out, &
                                      valid_min, valid_max, bound_max_value)

   implicit none

   real(kind=sreal),   intent(in)  :: value_in
   integer(kind=sint), intent(out) :: value_out
   real(kind=sreal),   intent(in)  :: scale_factor
   real(kind=sreal),   intent(in)  :: add_offset
   real(kind=sreal),   intent(in)  :: fill_value_in
   integer(kind=sint), intent(in)  :: fill_value_out
   integer(kind=sint), intent(in)  :: valid_min
   integer(kind=sint), intent(in)  :: valid_max
   integer(kind=sint), intent(in)  :: bound_max_value

   real(kind=sreal)                 :: temp

   temp = (value_in - add_offset) / scale_factor

   if (value_in .ne. sreal_fill_value) then
      if (temp .lt. real(valid_min,kind=sreal)) then
         value_out=sint_fill_value
      else if (temp .gt. real(valid_max,kind=sreal)) then
         value_out=bound_max_value
      else
         value_out=int(temp, kind=sint)
      end if
   else
      value_out=sint_fill_value
   end if

end subroutine prepare_short_packed_float


subroutine prepare_float_packed_float(value_in, value_out, &
                                      scale_factor, add_offset, &
                                      fill_value_in, fill_value_out, &
                                      valid_min, valid_max, bound_max_value)

   implicit none

   real(kind=sreal), intent(in)  :: value_in
   real(kind=sreal), intent(out) :: value_out
   real(kind=sreal), intent(in)  :: scale_factor
   real(kind=sreal), intent(in)  :: add_offset
   real(kind=sreal), intent(in)  :: fill_value_in
   real(kind=sreal), intent(in)  :: fill_value_out
   real(kind=sreal), intent(in)  :: valid_min
   real(kind=sreal), intent(in)  :: valid_max
   real(kind=sreal), intent(in)  :: bound_max_value

   real(kind=sreal)              :: temp

   temp = (value_in - add_offset) / scale_factor

   if (temp .ge. real(valid_min,kind=sreal) .and. &
       temp .le. real(valid_max,kind=sreal)) then
      value_out=temp
   else if (temp .lt. real(valid_min,kind=sreal)) then
      value_out=sreal_fill_value
   else if (temp .gt. real(valid_max,kind=sreal)) then
      value_out=bound_max_value
   end if

end subroutine prepare_float_packed_float
