!-------------------------------------------------------------------------------
! Name: orac_ncdf.F90
!
! Purpose:
! A module containing the following routines for reading NetCDF files:
!    NC_READ_ARRAY - Read specified data field from open file into an array.
!    NC_OPEN       - Open a NetCDF file with error handling.
!    NC_ERROR      - Translate a NetCDF error code into an error message.
!
! History:
! 2014/02/10, AP: Original version, combining the original files:
!    nc_read_file.F90, nc_open.F90
!
! $Id$
!-------------------------------------------------------------------------------

module orac_ncdf

   implicit none

   interface nc_read_array
      module procedure float_1d, float_2d, float_3d, float_4d, float_5d, &
           int_1d, int_2d, int_3d, int_4d, int_5d, sint_1d, sint_2d, sint_3d
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
! ncid    integer In  ID number returned by a call to NF90_OPEN
! name    string  In  Name of the data field to be returned
! val     real    Out Array into which the data will be written. The type and
!                     size of this array determine the call used.
! verbose logical In  T: print additional information; F: don't
!
! History:
! 2014/02/10, AP: Original version, replacing nc_read_file.F90
!
! Bugs:
! - does not yet cover all possible data types
!-------------------------------------------------------------------------------

SUBROUTINE float_1d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(sreal),      intent(inout) :: val(:)

   integer                         :: ierr, vid
   real(sreal)                     :: fv, sf, of
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

end SUBROUTINE float_1d

SUBROUTINE float_2d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(sreal),      intent(inout) :: val(:,:)

   integer                         :: ierr, vid
   real(sreal)                     :: fv, sf, of
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

end SUBROUTINE float_2d

SUBROUTINE float_3d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(sreal),      intent(inout) :: val(:,:,:)

   integer                         :: ierr, vid
   real(sreal)                     :: fv, sf, of
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

end SUBROUTINE float_3d

SUBROUTINE float_4d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(sreal),      intent(inout) :: val(:,:,:,:)

   integer                         :: ierr, vid
   real(sreal)                     :: fv, sf, of
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

end SUBROUTINE float_4d

SUBROUTINE float_5d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   real(sreal),      intent(inout) :: val(:,:,:,:,:)

   integer                         :: ierr, vid
   real(sreal)                     :: fv, sf, of
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

end SUBROUTINE float_5d
   
SUBROUTINE int_1d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(lint),    intent(inout) :: val(:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE int_1d
   
SUBROUTINE int_2d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(lint),    intent(inout) :: val(:,:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE int_2d
   
SUBROUTINE int_3d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(lint),    intent(inout) :: val(:,:,:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE int_3d
   
SUBROUTINE int_4d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(lint),    intent(inout) :: val(:,:,:,:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE int_4d
   
SUBROUTINE int_5d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(lint),    intent(inout) :: val(:,:,:,:,:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE int_5d
   
SUBROUTINE sint_1d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE sint_1d

SUBROUTINE sint_2d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:,:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE sint_2d

SUBROUTINE sint_3d(ncid, name, val, verbose)
   use netcdf
   use preproc_constants

   implicit none

   integer,          intent(in)    :: ncid, verbose
   character(len=*), intent(in)    :: name
   integer(2),       intent(inout) :: val(:,:,:)

   integer                         :: ierr, vid
   integer(lint)                   :: fv, sf, of
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

end SUBROUTINE sint_3d

!-------------------------------------------------------------------------------
! Name: nc_open
!
! Purpose:
! Wrapper for NF90_OPEN with error handling.
!
! Description and Algorithm details:
! 1) Call NF90_OPEN. If error, print message.
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid  integer Out File ID number returned by NF90_OPEN
! fname string  In  Name of the file to be opened
!
! History:
! 2014/02/10, AP: Original version, replacing nc_open.F90.
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------
   
SUBROUTINE nc_open(ncid, fname)
   use netcdf

   implicit none

   integer,          intent(out) :: ncid
   character(len=*), intent(in)  :: fname

   integer                       :: ierr

   ierr=NF90_OPEN(path=trim(adjustl(fname)),mode=NF90_NOWRITE,ncid=ncid)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_OPEN: Error opening file ',trim(fname),'. ',NC_ERROR(ierr)
      STOP
   end if
end SUBROUTINE nc_open

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
!
! Bugs:
! none known
!-------------------------------------------------------------------------------
FUNCTION nc_error(ierr) result(out)
   implicit none

   integer, intent(in) :: ierr
   character(len=40)   :: out

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

end FUNCTION nc_error
end module orac_ncdf
