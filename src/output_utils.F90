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
!
! $Id: nc_create.F90 2355 2014-09-09 23:16:38Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_create(path, ncid, nx, ny, dims_var, inst_name, type, status)

   use netcdf

   use common_constants
   use global_attributes
   use orac_ncdf

   implicit none

   ! Input
   character(len=*),intent(in)  :: path
   integer,         intent(in)  :: nx, ny
   character(len=*),intent(in)  :: inst_name
   integer,         intent(in)  :: type

   ! Output
   integer,         intent(out) :: ncid
   integer,         intent(out) :: dims_var(2)
   integer,         intent(out) :: status

   ! Local
   integer                   :: ierr, xdim, ydim
   character(len=128)        :: temp_string
   type(global_attributes_s) :: global_atts


   !----------------------------------------------------------------------------
   ! Create new file
   !----------------------------------------------------------------------------
   ierr = nf90_create(path, NF90_CLOBBER, ncid)
   if (ierr .ne. NF90_NOERR) then
      status = -1
      write(*,*) 'ERROR: nf90_create(), filename = ', trim(path)
      stop
   end if

   ! Define the 3 dimensions: time / lat / lon
   ierr = nf90_def_dim(ncid, 'across_track', nx, xdim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(), dim_name = across_track, xdim = ', xdim
      stop
   end if

   ierr = nf90_def_dim(ncid, 'along_track', ny, ydim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(), dim_name = along_track,  ydim = ', ydim
      stop
   end if


   !----------------------------------------------------------------------------
   ! Set global_attributes structure
   !----------------------------------------------------------------------------

   ! Global attribute 'Conventions' as defined by CF-1.4, section 2.6.1.
   global_atts%Conventions               = 'Conventions!!!'

   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   if (type .eq. 1) then
      global_atts%title = 'ESA CCI Cloud Retrieval Products L2 Primary File'
   else if (type .eq. 2) then
      global_atts%title = 'ESA CCI Cloud Retrieval Products L2 Secondary File'
   else
      write(*,*) 'ERROR: nf90_create(), invalid file type: ', type
      stop
   endif

   global_atts%institution               = 'institution!!!'
   global_atts%source                    = 'source!!!'
   global_atts%history                   = 'history!!!'
   global_atts%references                = 'references!!!'
   global_atts%comment                   = 'comment!!!'

   ! Extra global attributes defined by ORAC
   global_atts%Project                   = 'Project!!!'
   global_atts%File_Name                 = 'File_Name!!!'
   global_atts%UUID                      = 'UUID!!!'
   global_atts%NetCDF_Version            = 'NetCDF_Version!!!'
   global_atts%Product_Name              = 'Product_Name!!!'

   temp_string=trim('year!!!')//trim('month!!!')//trim('day!!!')
   global_atts%Product_Date              = trim(temp_string)

   global_atts%Production_Time           = 'Production_Time!!!'
   global_atts%L2_Processor              = 'ORAC'
   global_atts%L2_Processor_Version      = 'L2_Processor_Version!!!'
   global_atts%Platform                  = 'Platform!!!'
   global_atts%Sensor                    = 'Sensor!!!'

   global_atts%AATSR_Processing_Version = ' '
   if (inst_name .eq. 'ATSR' .or. inst_name .eq. 'AATSR') then
      global_atts%AATSR_Processing_Version = '3.01'
   endif

   global_atts%Contact_Email             = 'Contact_Email!!!'
   global_atts%Contact_Website           = 'Contact_Website!!!'
   global_atts%Keywords                  = 'Keywords!!!'
   global_atts%Summary                   = 'Summary!!!'
   global_atts%License                   = 'GNU General Public License (GPL), Version 3'


   !----------------------------------------------------------------------------
   ! Write global attributes to the netcdf output
   !----------------------------------------------------------------------------
   call nc_put_common_attributes(ncid, global_atts)


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   ierr = nf90_enddef(ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop
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
