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
   use common_constants, only: dreal_fill_value, sreal_fill_value, &
        lint_fill_value, sint_fill_value, byte_fill_value, &
        error_stop_code, unitlength

   implicit none

   interface nc_read_array
      module procedure dreal_1d, dreal_2d, dreal_3d,  dreal_4d, &
           sreal_1d, sreal_2d, sreal_3d, sreal_4d, &
           lint_1d, lint_2d, lint_3d, lint_4d, &
           sint_1d, sint_2d, sint_3d, sint_4d, &
           byte_1d, byte_2d, byte_3d, byte_4d
   end interface nc_read_array

contains

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
      print*,'ERROR: nc_open(): Error opening file ',trim(fname)
      print*,nc_error(ierr)
      stop error_stop_code
   end if

end subroutine nc_open

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

   integer,          intent(in) :: ncid
   character(len=*), intent(in) :: name
   logical,          intent(in) :: verbose

   integer :: did, ierr, len
   character(len=NF90_MAX_NAME) :: dname

   ierr = nf90_inq_dimid(ncid, name, did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: nc_dim_length(): Could not locate dimension ',trim(name)
      print*,nc_error(ierr)
      stop error_stop_code
   end if
   
   ierr = nf90_inquire_dimension(ncid, did, dname, len)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: nc_dim_length():: Could not read dimension ',trim(name)
      print*,nc_error(ierr)
      stop error_stop_code
   end if

   if (verbose) print*, trim(name),' dim length: ',len
   
end function nc_dim_length

!-------------------------------------------------------------------------------
! Name: nc_read_array
!
! Purpose:
! A module procedure for reading arrays of various sizes and types from a
! NetCDF file. It currently supports reading arrays of 1 to 4 dimensions of
! type 1, 2, or 4 byte integer and 4 or 8 byte real.
!
! An array can be partially read using the dim and ind arguments to specifiy
! that dimension and the elements that should be read.
!
! Description and Algorithm details:
! 1) Write array dimensions into "counter" array.
! 2) Execute code in "ncdf_open_field.inc". It locates the named variable and
!    reads its fill value, scale factor and/or offset.
! 3) If partially reading the array, loop over the elements to be read.
!    Otherwise, consider the entire array.
! 4) Execute code in "ncdf_read_field.inc" to:
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
! dim     integer In  Optional. If set, specifies the index of a dimension of
!                     the field to be read that will only be partially read.
! ind     integer In  Optional. If set, specifies the indices of the dimension
!                     specified above to be read.
!
! History:
! 2014/02/10, AP: Original version, replacing nc_read_file.F90
! 2014/08/12, AP: Adding routines for all expected data types.
! 2014/08/15, AP: Adding partial read procedure. Homogenizing use of verbose.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine dreal_1d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(8), target,   intent(inout) :: val(:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(1)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(8)                   :: fv, sf, of
   real(8), pointer          :: arr(:)
   real(8)                   :: fill=dreal_fill_value
 
   start = 1
   counter = size(val,1)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         arr => val(i:i)
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine dreal_1d

subroutine dreal_2d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(8), target,   intent(inout) :: val(:,:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(2)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(8)                   :: fv, sf, of
   real(8), pointer          :: arr(:,:)
   real(8)                   :: fill=dreal_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine dreal_2d

subroutine dreal_3d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(8), target,   intent(inout) :: val(:,:,:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(3)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(8)                   :: fv, sf, of
   real(8), pointer          :: arr(:,:,:)
   real(8)                   :: fill=dreal_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine dreal_3d

subroutine dreal_4d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(8), target,   intent(inout) :: val(:,:,:,:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(4)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(8)                   :: fv, sf, of
   real(8), pointer          :: arr(:,:,:,:)
   real(8)                   :: fill=dreal_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:)
         case(2)
            arr => val(:,i:i,:,:)
         case(3)
            arr => val(:,:,i:i,:)
         case(4)
            arr => val(:,:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine dreal_4d
!-------------------------------------------------------------------------------
subroutine sreal_1d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(4), target,   intent(inout) :: val(:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(1)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(4)                   :: fv, sf, of
   real(4), pointer          :: arr(:)
   real(4)                   :: fill=sreal_fill_value
 
   start = 1
   counter = size(val,1)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         arr => val(i:i)
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sreal_1d

subroutine sreal_2d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(4), target,   intent(inout) :: val(:,:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(2)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(4)                   :: fv, sf, of
   real(4), pointer          :: arr(:,:)
   real(4)                   :: fill=sreal_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sreal_2d

subroutine sreal_3d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(4), target,   intent(inout) :: val(:,:,:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(3)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(4)                   :: fv, sf, of
   real(4), pointer          :: arr(:,:,:)
   real(4)                   :: fill=sreal_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sreal_3d

subroutine sreal_4d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,           intent(in)    :: ncid
   character(len=*),  intent(in)    :: name
   real(4), target,   intent(inout) :: val(:,:,:,:)
   logical,           intent(in)    :: verbose
   integer, optional, intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(4)     :: start, counter, stride
   character(len=unitlength) :: unit
   real(4)                   :: fv, sf, of
   real(4), pointer          :: arr(:,:,:,:)
   real(4)                   :: fill=sreal_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:)
         case(2)
            arr => val(:,i:i,:,:)
         case(3)
            arr => val(:,:,i:i,:)
         case(4)
            arr => val(:,:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sreal_4d
!-------------------------------------------------------------------------------
subroutine lint_1d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(4), target, intent(inout) :: val(:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(1)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(4)                :: fv, sf, of
   integer(4), pointer       :: arr(:)
   integer(4)                :: fill=lint_fill_value
 
   start = 1
   counter = size(val,1)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         arr => val(i:i)
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine lint_1d

subroutine lint_2d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(4), target, intent(inout) :: val(:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(2)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(4)                :: fv, sf, of
   integer(4), pointer       :: arr(:,:)
   integer(4)                :: fill=lint_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine lint_2d

subroutine lint_3d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(4), target, intent(inout) :: val(:,:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(3)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(4)                :: fv, sf, of
   integer(4), pointer       :: arr(:,:,:)
   integer(4)                :: fill=lint_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine lint_3d

subroutine lint_4d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(4), target, intent(inout) :: val(:,:,:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(4)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(4)                :: fv, sf, of
   integer(4), pointer       :: arr(:,:,:,:)
   integer(4)                :: fill=lint_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:)
         case(2)
            arr => val(:,i:i,:,:)
         case(3)
            arr => val(:,:,i:i,:)
         case(4)
            arr => val(:,:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine lint_4d
!-------------------------------------------------------------------------------
subroutine sint_1d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(2), target, intent(inout) :: val(:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(1)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(2)                :: fv, sf, of
   integer(2), pointer       :: arr(:)
   integer(2)                :: fill=sint_fill_value
 
   start = 1
   counter = size(val,1)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         arr => val(i:i)
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sint_1d

subroutine sint_2d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(2), target, intent(inout) :: val(:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(2)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(2)                :: fv, sf, of
   integer(2), pointer       :: arr(:,:)
   integer(2)                :: fill=sint_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sint_2d

subroutine sint_3d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(2), target, intent(inout) :: val(:,:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(3)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(2)                :: fv, sf, of
   integer(2), pointer       :: arr(:,:,:)
   integer(2)                :: fill=sint_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sint_3d

subroutine sint_4d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(2), target, intent(inout) :: val(:,:,:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(4)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(2)                :: fv, sf, of
   integer(2), pointer       :: arr(:,:,:,:)
   integer(2)                :: fill=sint_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:)
         case(2)
            arr => val(:,i:i,:,:)
         case(3)
            arr => val(:,:,i:i,:)
         case(4)
            arr => val(:,:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine sint_4d
!-------------------------------------------------------------------------------
subroutine byte_1d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(1), target, intent(inout) :: val(:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(1)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(1)                :: fv, sf, of
   integer(1), pointer       :: arr(:)
   integer(1)                :: fill=byte_fill_value
 
   start = 1
   counter = size(val,1)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         arr => val(i:i)
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine byte_1d

subroutine byte_2d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(1), target, intent(inout) :: val(:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(2)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(1)                :: fv, sf, of
   integer(1), pointer       :: arr(:,:)
   integer(1)                :: fill=byte_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine byte_2d

subroutine byte_3d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(1), target, intent(inout) :: val(:,:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(3)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(1)                :: fv, sf, of
   integer(1), pointer       :: arr(:,:,:)
   integer(1)                :: fill=byte_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine byte_3d

subroutine byte_4d(ncid, name, val, verbose, dim, ind)
   implicit none

   integer,            intent(in)    :: ncid
   character(len=*),   intent(in)    :: name
   integer(1), target, intent(inout) :: val(:,:,:,:)
   logical,            intent(in)    :: verbose
   integer, optional,  intent(in)    :: dim, ind(:)

   integer                   :: ierr, vid, i
   integer, dimension(4)     :: start, counter, stride
   character(len=unitlength) :: unit
   integer(1)                :: fv, sf, of
   integer(1), pointer       :: arr(:,:,:,:)
   integer(1)                :: fill=byte_fill_value
 
   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (verbose) print*,'Reading variable ',trim(name)
   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:)
         case(2)
            arr => val(:,i:i,:,:)
         case(3)
            arr => val(:,:,i:i,:)
         case(4)
            arr => val(:,:,:,i:i)
         end select
         
         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine byte_4d

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

end module orac_ncdf
