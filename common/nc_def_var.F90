!-------------------------------------------------------------------------------
! Name: nc_def_var.F90
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for
! different attribute/variable type combinations. Subroutines names are self
! descriptive.
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
! 2014/08/04, Greg McGarragh: Cleaned up the code.
! 2014/08/07, Greg McGarragh: Removed nf90_redef() and nf90_enddef() from the
!    subroutines.  It is extremely inefficient to do this for each variable
!    defined.  It should be done at the start and end of defining all the
!    variables in a file, i.e. in the code calling the subroutines in this file.
! 2014/08/31, Greg McGarragh: Significant refactoring and moved into the
!    common library.
! 2014/09/01, Greg McGarragh: Make use of the general routine
!     nc_put_common_attributes().
!
!
! $Id: nc_def_var.F90 2290 2014-08-12 08:24:01Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_def_var_byte_packed_byte(ncid,dimids,name,varid,long_name, &
   standard_name,fill_value,scale_factor,add_offset,valid_min,valid_max, &
   verbose,ierr,units,flag_values,flag_meanings)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)           :: ncid
   integer,            intent(in)           :: dimids(2)
   character(len=*),   intent(in)           :: name
   character(len=*),   intent(in)           :: long_name
   character(len=*),   intent(in)           :: standard_name
   integer(kind=byte), intent(in)           :: fill_value
   integer(kind=byte), intent(in)           :: scale_factor
   integer(kind=byte), intent(in)           :: add_offset
   integer(kind=byte), intent(in)           :: valid_min
   integer(kind=byte), intent(in)           :: valid_max
   logical,            intent(in)           :: verbose
   character(len=*),   intent(in), optional :: units
   character(len=*),   intent(in), optional :: flag_values
   character(len=*),   intent(in), optional :: flag_meanings

   ! Output
   integer,            intent(out) :: varid
   integer,            intent(out) :: ierr

   ierr = nf90_def_var(ncid, name, NF90_BYTE, dimids, varid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), name = ', name
      stop
   end if

   include 'nc_def_var.inc'

end subroutine nc_def_var_byte_packed_byte


subroutine nc_def_var_short_packed_short(ncid,dimids,name,varid,long_name, &
   standard_name,fill_value,scale_factor,add_offset,valid_min,valid_max, &
   verbose,ierr,units,flag_values,flag_meanings)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)           :: ncid
   integer,            intent(in)           :: dimids(2)
   character(len=*),   intent(in)           :: name
   character(len=*),   intent(in)           :: long_name
   character(len=*),   intent(in)           :: standard_name
   integer(kind=sint), intent(in)           :: fill_value
   integer(kind=sint), intent(in)           :: scale_factor
   integer(kind=sint), intent(in)           :: add_offset
   integer(kind=sint), intent(in)           :: valid_min
   integer(kind=sint), intent(in)           :: valid_max
   logical,            intent(in)           :: verbose
   character(len=*),   intent(in), optional :: units
   character(len=*),   intent(in), optional :: flag_values
   character(len=*),   intent(in), optional :: flag_meanings

   ! Output
   integer,            intent(out) :: varid
   integer,            intent(out) :: ierr

   ierr = nf90_def_var(ncid, name, NF90_SHORT, dimids, varid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), name = ', name
      stop
   end if

   include 'nc_def_var.inc'

end subroutine nc_def_var_short_packed_short


subroutine nc_def_var_short_packed_float(ncid,dimids,name,varid,long_name, &
   standard_name,fill_value,scale_factor,add_offset,valid_min,valid_max, &
   verbose,ierr,units,flag_values,flag_meanings)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)           :: ncid
   integer,            intent(in)           :: dimids(2)
   character(len=*),   intent(in)           :: name
   character(len=*),   intent(in)           :: long_name
   character(len=*),   intent(in)           :: standard_name
   integer(kind=sint), intent(in)           :: fill_value
   real(kind=sreal),   intent(in)           :: scale_factor
   real(kind=sreal),   intent(in)           :: add_offset
   integer(kind=sint), intent(in)           :: valid_min
   integer(kind=sint), intent(in)           :: valid_max
   logical,            intent(in)           :: verbose
   character(len=*),   intent(in), optional :: units
   character(len=*),   intent(in), optional :: flag_values
   character(len=*),   intent(in), optional :: flag_meanings

   ! Output
   integer,            intent(out) :: varid
   integer,            intent(out) :: ierr

   ierr = nf90_def_var(ncid, name, NF90_SHORT, dimids, varid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), name = ', name
      stop
   end if

   include 'nc_def_var.inc'

end subroutine nc_def_var_short_packed_float


subroutine nc_def_var_long_packed_long(ncid,dimids,name,varid,long_name, &
   standard_name,fill_value,scale_factor,add_offset,valid_min,valid_max, &
   verbose,ierr,units,flag_values,flag_meanings)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)           :: ncid
   integer,            intent(in)           :: dimids(2)
   character(len=*),   intent(in)           :: name
   character(len=*),   intent(in)           :: long_name
   character(len=*),   intent(in)           :: standard_name
   integer,            intent(in)           :: fill_value
   integer(kind=lint), intent(in)           :: scale_factor
   integer(kind=lint), intent(in)           :: add_offset
   integer(kind=lint), intent(in)           :: valid_min
   integer(kind=lint), intent(in)           :: valid_max
   logical(kind=lint), intent(in)           :: verbose
   character(len=*),   intent(in), optional :: units
   character(len=*),   intent(in), optional :: flag_values
   character(len=*),   intent(in), optional :: flag_meanings

   ! Output
   integer,          intent(out) :: varid
   integer,          intent(out) :: ierr

   ierr = nf90_def_var(ncid, name, NF90_INT, dimids, varid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), name = ', name
      stop
   end if

   include 'nc_def_var.inc'

end subroutine nc_def_var_long_packed_long


subroutine nc_def_var_float_packed_float(ncid,dimids,name,varid,long_name, &
   standard_name,fill_value,scale_factor,add_offset,valid_min,valid_max, &
   verbose,ierr,units,flag_values,flag_meanings)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)           :: ncid
   integer,          intent(in)           :: dimids(2)
   character(len=*), intent(in)           :: name
   character(len=*), intent(in)           :: long_name
   character(len=*), intent(in)           :: standard_name
   real(kind=sreal), intent(in)           :: fill_value
   real(kind=sreal), intent(in)           :: scale_factor
   real(kind=sreal), intent(in)           :: add_offset
   real(kind=sreal), intent(in)           :: valid_min
   real(kind=sreal), intent(in)           :: valid_max
   logical,          intent(in)           :: verbose
   character(len=*), intent(in), optional :: units
   character(len=*), intent(in), optional :: flag_values
   character(len=*), intent(in), optional :: flag_meanings

   ! Output
   integer,          intent(out) :: varid
   integer,          intent(out) :: ierr

   ierr = nf90_def_var(ncid, name, NF90_FLOAT, dimids, varid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), name = ', name
      stop
   end if

   include 'nc_def_var.inc'

end subroutine nc_def_var_float_packed_float


subroutine nc_def_var_double_packed_double(ncid,dimids,name,varid,long_name, &
   standard_name,fill_value,scale_factor,add_offset,valid_min,valid_max, &
   verbose,ierr,units,flag_values,flag_meanings)

   use common_constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)           :: ncid
   integer,          intent(in)           :: dimids(2)
   character(len=*), intent(in)           :: name
   character(len=*), intent(in)           :: long_name
   character(len=*), intent(in)           :: standard_name
   real(kind=dreal), intent(in)           :: fill_value
   real(kind=dreal), intent(in)           :: scale_factor
   real(kind=dreal), intent(in)           :: add_offset
   real(kind=dreal), intent(in)           :: valid_min
   real(kind=dreal), intent(in)           :: valid_max
   logical,          intent(in)           :: verbose
   character(len=*), intent(in), optional :: units
   character(len=*), intent(in), optional :: flag_values
   character(len=*), intent(in), optional :: flag_meanings

   ! Output
   integer,          intent(out) :: varid
   integer,          intent(out) :: ierr

   ierr = nf90_def_var(ncid, name, NF90_double, dimids, varid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), name = ', name
      stop
   end if

   include 'nc_def_var.inc'

end subroutine nc_def_var_double_packed_double
