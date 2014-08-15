!-------------------------------------------------------------------------------
! Name: orac_ncdf.F90
!
! Purpose:
! A module containing the following routines for reading NetCDF files:
!    NC_READ_ARRAY - Read specified data field from open file into an array.
!    NC_OPEN       - Open a NetCDF file with error handling.
!    NC_ERROR      - Translate a NetCDF error code into an error message.
!    NC_DIM_LENGTH - Return the length of a dimension in a NetCDF file.
!
! History:
! 2014/02/10, AP: Original version, combining the original files:
!    nc_read_file.F90, nc_open.F90
!
! $Id$
!-------------------------------------------------------------------------------

module orac_ncdf

   use netcdf

   implicit none

   interface nc_read_array
      module procedure double_1d, double_2d, double_3d, double_4d, double_5d, &
           float_1d, float_2d, float_3d, float_4d, float_5d, &
           lint_1d, lint_2d, lint_3d, lint_4d, lint_5d, &
           int_1d, int_2d, int_3d, int_4d, int_5d, &
           byte_1d, byte_2d, byte_3d, byte_4d, byte_5d
   end interface nc_read_array

contains

!-------------------------------------------------------------------------------
! Name: nc_read_array
!
! Purpose:
! A module procedure for reading arrays of various sizes and types from a
! NetCDF file. It currently supports reading arrays of 1 to 5 dimensions of
! type real or integer.
!
! Description and Algorithm details:
! 1) Write array dimensions into "counter" array.
! 2) Execute code in "ncdf_read.inc". This:
!   - Locate named variable in file.
!   - Read data from file and replace fill value as required.
!   - If necessary, apply scale factor and/or offset to data.
!   - If requested, print out units, valid_min, and valid_max fields.
!
! Arguments:
! Name    Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid    integer In  ID number returned by a call to nf90_open
! name    string  In  Name of the data field to be returned
! val     real    Out Array into which the data will be written. The type and
!                     size of this array determine the call used.
! verbose logical In  T: print additional information; F: don't
!
! History:
! 2014/02/10, AP: Original version, replacing nc_read_file.F90
! 2014/08/12, AP: Adding routines for all expected data types.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine double_1d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(8),          intent(inout) :: val(:)

   real(8)                         :: fill=double_fill_value
   integer                         :: ierr, vid
   real(8)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(1)  :: start, counter, stride

   start = 1
   counter = size(val,1)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine double_1d

subroutine double_2d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(8),          intent(inout) :: val(:,:)

   real(8)                         :: fill=double_fill_value
   integer                         :: ierr, vid
   real(8)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(2)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine double_2d

subroutine double_3d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(8),          intent(inout) :: val(:,:,:)

   real(8)                         :: fill=double_fill_value
   integer                         :: ierr, vid
   real(8)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(3)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine double_3d

subroutine double_4d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(8),          intent(inout) :: val(:,:,:,:)

   real(8)                         :: fill=double_fill_value
   integer                         :: ierr, vid
   real(8)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(4)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine double_4d

subroutine double_5d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(8),          intent(inout) :: val(:,:,:,:,:)

   real(8)                         :: fill=double_fill_value
   integer                         :: ierr, vid
   real(8)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(5)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine double_5d
   
subroutine float_1d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(4),          intent(inout) :: val(:)

   real(4)                         :: fill=real_fill_value
   integer                         :: ierr, vid
   real(4)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(1)  :: start, counter, stride

   start = 1
   counter = size(val,1)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine float_1d

subroutine float_2d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(4),          intent(inout) :: val(:,:)

   real(4)                         :: fill=real_fill_value
   integer                         :: ierr, vid
   real(4)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(2)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine float_2d

subroutine float_3d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(4),          intent(inout) :: val(:,:,:)

   real(4)                         :: fill=real_fill_value
   integer                         :: ierr, vid
   real(4)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(3)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine float_3d

subroutine float_4d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(4),          intent(inout) :: val(:,:,:,:)

   real(4)                         :: fill=real_fill_value
   integer                         :: ierr, vid
   real(4)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(4)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine float_4d

subroutine float_5d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(4),          intent(inout) :: val(:,:,:,:,:)

   real(4)                         :: fill=real_fill_value
   integer                         :: ierr, vid
   real(4)                         :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(5)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   stride = 1
   ierr = 0
   sf = 1.0
   of = 0.0
   flag = .false.

   include "ncdf_read.inc"

end subroutine float_5d
   
subroutine lint_1d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(4),       intent(inout) :: val(:)

   integer(4)                      :: fill=long_int_fill_value
   integer                         :: ierr, vid
   integer(4)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(1)  :: start, counter, stride

   start = 1
   counter = size(val,1)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine lint_1d
   
subroutine lint_2d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(4),       intent(inout) :: val(:,:)

   integer(4)                      :: fill=long_int_fill_value
   integer                         :: ierr, vid
   integer(4)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(2)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine lint_2d
   
subroutine lint_3d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(4),       intent(inout) :: val(:,:,:)

   integer(4)                      :: fill=long_int_fill_value
   integer                         :: ierr, vid
   integer(4)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(3)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine lint_3d
   
subroutine lint_4d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(4),       intent(inout) :: val(:,:,:,:)

   integer(4)                      :: fill=long_int_fill_value
   integer                         :: ierr, vid
   integer(4)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(4)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine lint_4d
   
subroutine lint_5d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(4),       intent(inout) :: val(:,:,:,:,:)

   integer(4)                      :: fill=long_int_fill_value
   integer                         :: ierr, vid
   integer(4)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(5)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine lint_5d
   
subroutine int_1d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:)

   integer(2)                      :: fill=int_fill_value
   integer                         :: ierr, vid
   integer(2)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(1)  :: start, counter, stride

   start = 1
   counter = size(val,1)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine int_1d

subroutine int_2d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:,:)

   integer(2)                      :: fill=int_fill_value
   integer                         :: ierr, vid
   integer(2)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(2)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine int_2d

subroutine int_3d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:,:,:)

   integer(2)                      :: fill=int_fill_value
   integer                         :: ierr, vid
   integer(2)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(3)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine int_3d

subroutine int_4d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:,:,:,:)

   integer(2)                      :: fill=int_fill_value
   integer                         :: ierr, vid
   integer(2)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(4)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine int_4d

subroutine int_5d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:,:,:,:,:)

   integer(2)                      :: fill=int_fill_value
   integer                         :: ierr, vid
   integer(2)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(5)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine int_5d

subroutine byte_1d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(1),       intent(inout) :: val(:)

   integer(1)                      :: fill=byte_fill_value
   integer                         :: ierr, vid
   integer(1)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(1)  :: start, counter, stride

   start = 1
   counter = size(val,1)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine byte_1d

subroutine byte_2d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(1),       intent(inout) :: val(:,:)

   integer(1)                      :: fill=byte_fill_value
   integer                         :: ierr, vid
   integer(1)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(2)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine byte_2d

subroutine byte_3d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(1),       intent(inout) :: val(:,:,:)

   integer(1)                      :: fill=byte_fill_value
   integer                         :: ierr, vid
   integer(1)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(3)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine byte_3d

subroutine byte_4d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(1),       intent(inout) :: val(:,:,:,:)

   integer(1)                      :: fill=byte_fill_value
   integer                         :: ierr, vid
   integer(1)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(4)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine byte_4d

subroutine byte_5d(ncid, name, val, verbose)
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(1),       intent(inout) :: val(:,:,:,:,:)

   integer(1)                      :: fill=byte_fill_value
   integer                         :: ierr, vid
   integer(1)                      :: fv, sf, of
   character(len=unitlength)       :: unit
   logical                         :: flag
   integer,          dimension(5)  :: start, counter, stride

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   stride = 1
   ierr = 0
   sf = 1
   of = 0
   flag = .false.

   include "ncdf_read.inc"

end subroutine byte_5d

!-------------------------------------------------------------------------------
! Name: nc_open
!
! Purpose:
! Wrapper for nf90_open with error handling.
!
! Description and Algorithm details:
! 1) Call nf90_open. If error, print message.
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid  integer Out File ID number returned by nf90_open
! fname string  In  Name of the file to be opened
!
! History:
! 2014/02/10, AP: Original version, replacing nc_open.F90.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine nc_open(ncid, fname)
   implicit none

   integer,          intent(out) :: ncid
   character(len=*), intent(in)  :: fname

   integer                       :: ierr

   ierr=nf90_open(path=trim(adjustl(fname)),mode=NF90_NOWRITE,ncid=ncid)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_OPEN: Error opening file ',trim(fname),'. ',nc_error(ierr)
      stop
   end if
end subroutine nc_open

!-------------------------------------------------------------------------------
! Name: nc_error
!
! Purpose:
! Function that translates an error code returned by a NetCDF routine into a
! string that can be understood by a user.
!
! Description and Algorithm details:
! 1) A CASE statement derived from
!    http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-c/Error-Codes.html
!
! Arguments:
! Name  Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ierr integer In  Error code returned by NetCDF routine
! out  string  Out Descriptive string for that code
!
! History:
! 2014/02/10, AP: Original version
! 2014/08/05, GM: 'out' was being truncated. Extended length to 64.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function nc_error(ierr) result(out)
   implicit none

   integer, intent(in) :: ierr
   character(len=64)   :: out

   select case (ierr)
   case(-33)
      out='Not a netcdf id.'
   case(-34)
      out='Too many netcdfs open.'
   case(-35)
      out='netcdf file exists && NC_NOCLOBBER.'
   case(-36)
      out='Invalid Argument.'
   case(-37)
      out='Write to read only.'
   case(-38)
      out='Operation not allowed in data mode.'
   case(-39)
      out='Operation not allowed in define mode.'
   case(-40)
      out='Index exceeds dimension bound.'
   case(-41)
      out='NC_MAX_DIMS exceeded.'
   case(-42)
      out='String match to name in use.'
   case(-43)
      out='Attribute not found.'
   case(-44)
      out='NC_MAX_ATTRS exceeded.'
   case(-45)
      out='Not a netcdf data type.'
   case(-46)
      out='Invalid dimension id or name.'
   case(-47)
      out='NC_UNLIMITED in the wrong index.'
   case(-48)
      out='NC_MAX_VARS exceeded.'
   case(-49)
      out='Variable not found.'
   case(-50)
      out='Action prohibited on NC_GLOBAL varid.'
   case(-51)
      out='Not a netcdf file.'
   case(-52)
      out='In Fortran, string too short.'
   case(-53)
      out='NC_MAX_NAME exceeded.'
   case(-54)
      out='NC_UNLIMITED size already in use.'
   case(-55)
      out='nc_rec op when there are no record vars.'
   case(-56)
      out='Attempt to convert between text & numbers.'
   case(-57)
      out='Edge+start exceeds dimension bound.'
   case(-58)
      out='Illegal stride.'
   case(-59)
      out='Attribute or variable name contains illegal characters.'
   case(-60)
      out='Math result not representable.'
   case(-61)
      out='Memory allocation case(malloc) failure.'
   case(-62)
      out='One or more variable sizes violate format constraints.'
   case(-63)
      out='Invalid dimension size.'
   case(-64)
      out='File likely truncated or possibly corrupted.'
   case default
      out='Unknown NetCDF error code.'
   end select

end function nc_error

!-------------------------------------------------------------------------------
! Name: nc_dim_length
!
! Purpose:
! Read length of a named dimension of NCDF file.
!
! Description and Algorithm details:
! 1) Obtain ID of requested dimension.
! 2) Retrieve its length and output.
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid  integer Out File ID number returned by nf90_open
! name  string  In  Name of dimension to read
! len   integer Out Length of desired dimenison
!
! History:
! 2014/08/06, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function nc_dim_length(ncid, name, verbose) result(len)
   implicit none

   integer,          intent(in) :: ncid, verbose
   character(len=*), intent(in) :: name

   integer :: did, ierr, len
   character(len=NF90_MAX_NAME) :: dname

   ierr = nf90_inq_dimid(ncid, name, did)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_DIM_LENGTH: Could not locate dimension ',trim(name),'. ', &
           nc_error(ierr)
      stop
   end if
   
   ierr = nf90_inquire_dimension(ncid, did, dname, len)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_DIM_LENGTH: Could not read dimension ',trim(name),'. ', &
           nc_error(ierr)
      stop
   end if

   if (verbose.ne.0) print*, trim(name),' n: ',len
   
end function nc_dim_length

end module orac_ncdf
