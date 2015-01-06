!-------------------------------------------------------------------------------
! Name: nc_create.F90
!
! Purpose:
! A netcdf output file is opened/created for writing.
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
! 2011/12/19, Matthias Jerg: Creates initial file.
! 2012/11/16, Caroline Poulsen: Added calibration file version.
! 2014/08/04, Greg McGarragh: Cleaned up the code.
! 2014/08/31, Greg McGarragh: Make the global attribute list consistent with
!    CF-1.4.
! 2014/09/01, Greg McGarragh: Make use of the general shared routine
!    nc_put_common_attributes().
! 2014/12/01, CP: Added remove global attributes read out directly now in
!    read_config file
!
! $Id: nc_create.F90 2355 2014-09-09 23:16:38Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_create(path, ncid, nx, ny, dims_var, inst_name, type, &
     global_atts, source_atts)

   use netcdf

   use common_constants
   use global_attributes
   use source_attributes

   use orac_ncdf

   implicit none

   ! Input
   character(len=*),          intent(in)    :: path
   integer,                   intent(in)    :: nx, ny
   character(len=*),          intent(in)    :: inst_name
   integer,                   intent(in)    :: type

   ! Output
   integer,                   intent(out)   :: ncid
   integer,                   intent(out)   :: dims_var(2)

   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts

   ! Local
   integer :: ierr, xdim, ydim


   !----------------------------------------------------------------------------
   ! Create new file
   !----------------------------------------------------------------------------
   ierr = nf90_create(path, NF90_CLOBBER, ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_create(): filename = ', trim(path)
      stop error_stop_code
   end if

   ! Define the 3 dimensions: time / lat / lon
   ierr = nf90_def_dim(ncid, 'across_track', nx, xdim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(): dim_name = across_track, xdim = ', xdim
      stop error_stop_code
   end if

   ierr = nf90_def_dim(ncid, 'along_track', ny, ydim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(): dim_name = along_track,  ydim = ', ydim
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Write global attributes to the netcdf output
   !----------------------------------------------------------------------------
   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   if (type .eq. 1) then
      global_atts%title = 'ESA CCI Cloud Retrieval Products L2 Primary File'
   else if (type .eq. 2) then
      global_atts%title = 'ESA CCI Cloud Retrieval Products L2 Secondary File'
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


   dims_var(1) = xdim
   dims_var(2) = ydim

end subroutine nc_create


subroutine prepare_short_packed_float(value_in, value_out, &
                                      scale_factor, add_offset, &
                                      fill_value_in, fill_value_out, &
                                      valid_min, valid_max, bound_max_value)

   use ECP_Constants

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

   use ECP_Constants

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
