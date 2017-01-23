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
! 2014/09/01, GM: Add nc_write_array and associated routines. For now for 2d
!    only.
! 2014/09/02, GM: Added 1d, 3d, and 4d nc_write_array capability.
! 2014/09/08, GM: Used poor man's C-preprocessor based templates for write
!    nc_write_array routines.
! 2014/09/16, GM: Added nc_put_common_attributes() and nc_def_var_*() routines.
! 2014/10/10, GM: Use nc_error() in nc_put_common_attributes() and the
!    nc_def_var_*() routines.
! 2014/10/24, OS: removed superfluous commata in write statements, causing
!    CRAY-ftn compiler to exit
! 2014/12/01, CP: Added new source attributes
! 2014/12/16, GM: Added nc_get_common_attributes().
! 2014/12/31, GM: Removed ierr output argument from nc_def_var_*() routines as
!    it severed no purpose since errors are handled to program exit within the
!    routines themselves.
! 2015/03/20, CP: changed to creator url and website
! 2015/07/10, OS: added optional error_status return argument
! 2015/07/16, GM: Added support to read packed data to nc_read routines.
! 2016/07/11, SP: Added new variable to read functions: startp
!
! $Id$
!-------------------------------------------------------------------------------

module orac_ncdf_m

   use netcdf
   use common_constants_m

   implicit none

   interface nc_read_array
      module procedure &
         read_byte_1d,  read_byte_2d,  read_byte_3d,  read_byte_4d, &
         read_sint_1d,  read_sint_2d,  read_sint_3d,  read_sint_4d, &
         read_lint_1d,  read_lint_2d,  read_lint_3d,  read_lint_4d, &
         read_sreal_1d, read_sreal_2d, read_sreal_3d, read_sreal_4d, &
         read_dreal_1d, read_dreal_2d, read_dreal_3d, read_dreal_4d
   end interface nc_read_array

   interface nc_write_array
      module procedure &
         write_byte_1d, write_byte_2d, write_byte_3d, write_byte_4d, &
         write_sint_1d, write_sint_2d, write_sint_3d, write_sint_4d, &
         write_lint_1d, write_lint_2d, write_lint_3d, write_lint_4d, &
         write_sreal_1d, write_sreal_2d, write_sreal_3d, write_sreal_4d, &
         write_dreal_1d, write_dreal_2d, write_dreal_3d, write_dreal_4d
   end interface nc_write_array

   interface nc_read_packed_array
      module procedure &
         read_packed_sreal_1d, read_packed_sreal_2d, read_packed_sreal_3d, &
         read_packed_sreal_4d
   end interface nc_read_packed_array

contains


subroutine c_to_f_str(str)
   use iso_c_binding, only: C_NULL_CHAR

   implicit none

   character(*), intent(inout) :: str
   integer :: i

   do i=1,len(str)
      if (str(i:i) == C_NULL_CHAR) exit
   end do

   str(i:len(str)) = ' '
end subroutine c_to_f_str

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
! 2015/07/03, OS: Added ierr return argument.
! 2015/07/25, GM: Got rid of conditional compilation around the error_status
!   argument.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine nc_open(ncid, fname, error_status)
   implicit none

   integer,          intent(out) :: ncid
   character(len=*), intent(in)  :: fname
   integer, intent(out),optional :: error_status
   integer                       :: ierr

   ierr=nf90_open(path=trim(adjustl(fname)),mode=NF90_NOWRITE,ncid=ncid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: nc_open(): Error opening file ',trim(fname)
      print*,'error flag:',trim(nf90_strerror(ierr))
      if (present(error_status)) then
         error_status = error_stop_code
      else
         stop error_stop_code
      end if
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
      print*,trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   ierr = nf90_inquire_dimension(ncid, did, dname, len)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: nc_dim_length():: Could not read dimension ',trim(name)
      print*,trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   if (verbose) print*, trim(name),' dim length: ',len

end function nc_dim_length

!-------------------------------------------------------------------------------
! Name: nc_def_var
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
!
! History:
! 2014/08/31, GM: Original version
! 2014/09/16, GM: Used poor man's C-preprocessor based templates for the
!    nc_def_var_* routines and made several arguments optional.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define NC_DEF_VAR_NAME nc_def_var_byte_packed_byte
#define NC_DEF_VAR_TYPE_PACKED integer
#define NC_DEF_VAR_KIND_PACKED byte
#define NC_DEF_VAR_TYPE_UNPACKED integer
#define NC_DEF_VAR_KIND_UNPACKED byte
#define NC_DEF_VAR_NF90_TYPE NF90_BYTE
#include "nc_def_var_template.inc"
#undef NC_DEF_VAR_NAME
#undef NC_DEF_VAR_TYPE_PACKED
#undef NC_DEF_VAR_KIND_PACKED
#undef NC_DEF_VAR_TYPE_UNPACKED
#undef NC_DEF_VAR_KIND_UNPACKED
#undef NC_DEF_VAR_NF90_TYPE

#define NC_DEF_VAR_NAME nc_def_var_short_packed_float
#define NC_DEF_VAR_TYPE_PACKED integer
#define NC_DEF_VAR_KIND_PACKED sint
#define NC_DEF_VAR_TYPE_UNPACKED real
#define NC_DEF_VAR_KIND_UNPACKED sreal
#define NC_DEF_VAR_NF90_TYPE NF90_SHORT
#include "nc_def_var_template.inc"
#undef NC_DEF_VAR_NAME
#undef NC_DEF_VAR_TYPE_PACKED
#undef NC_DEF_VAR_KIND_PACKED
#undef NC_DEF_VAR_TYPE_UNPACKED
#undef NC_DEF_VAR_KIND_UNPACKED
#undef NC_DEF_VAR_NF90_TYPE

#define NC_DEF_VAR_NAME nc_def_var_short_packed_short
#define NC_DEF_VAR_TYPE_PACKED integer
#define NC_DEF_VAR_KIND_PACKED sint
#define NC_DEF_VAR_TYPE_UNPACKED integer
#define NC_DEF_VAR_KIND_UNPACKED sint
#define NC_DEF_VAR_NF90_TYPE NF90_SHORT
#include "nc_def_var_template.inc"
#undef NC_DEF_VAR_NAME
#undef NC_DEF_VAR_TYPE_PACKED
#undef NC_DEF_VAR_KIND_PACKED
#undef NC_DEF_VAR_TYPE_UNPACKED
#undef NC_DEF_VAR_KIND_UNPACKED
#undef NC_DEF_VAR_NF90_TYPE

#define NC_DEF_VAR_NAME nc_def_var_long_packed_long
#define NC_DEF_VAR_TYPE_PACKED integer
#define NC_DEF_VAR_KIND_PACKED lint
#define NC_DEF_VAR_TYPE_UNPACKED integer
#define NC_DEF_VAR_KIND_UNPACKED lint
#define NC_DEF_VAR_NF90_TYPE NF90_INT
#include "nc_def_var_template.inc"
#undef NC_DEF_VAR_NAME
#undef NC_DEF_VAR_TYPE_PACKED
#undef NC_DEF_VAR_KIND_PACKED
#undef NC_DEF_VAR_TYPE_UNPACKED
#undef NC_DEF_VAR_KIND_UNPACKED
#undef NC_DEF_VAR_NF90_TYPE

#define NC_DEF_VAR_NAME nc_def_var_float_packed_float
#define NC_DEF_VAR_TYPE_PACKED real
#define NC_DEF_VAR_KIND_PACKED sreal
#define NC_DEF_VAR_TYPE_UNPACKED real
#define NC_DEF_VAR_KIND_UNPACKED sreal
#define NC_DEF_VAR_NF90_TYPE NF90_FLOAT
#include "nc_def_var_template.inc"
#undef NC_DEF_VAR_NAME
#undef NC_DEF_VAR_TYPE_PACKED
#undef NC_DEF_VAR_KIND_PACKED
#undef NC_DEF_VAR_TYPE_UNPACKED
#undef NC_DEF_VAR_KIND_UNPACKED
#undef NC_DEF_VAR_NF90_TYPE

#define NC_DEF_VAR_NAME nc_def_var_double_packed_double
#define NC_DEF_VAR_TYPE_PACKED real
#define NC_DEF_VAR_KIND_PACKED dreal
#define NC_DEF_VAR_TYPE_UNPACKED real
#define NC_DEF_VAR_KIND_UNPACKED dreal
#define NC_DEF_VAR_NF90_TYPE NF90_DOUBLE
#include "nc_def_var_template.inc"
#undef NC_DEF_VAR_NAME
#undef NC_DEF_VAR_TYPE_PACKED
#undef NC_DEF_VAR_KIND_PACKED
#undef NC_DEF_VAR_TYPE_UNPACKED
#undef NC_DEF_VAR_KIND_UNPACKED
#undef NC_DEF_VAR_NF90_TYPE

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
! startp  integer In  Optional. If set, specifies the position in the netcdf
!                     file to start reading data
!
! History:
! 2014/02/10, AP: Original version, replacing nc_read_file.F90
! 2014/08/12, AP: Adding routines for all expected data types.
! 2014/08/15, AP: Adding partial read procedure. Homogenizing use of verbose.
! 2014/09/03, GM: Added vl to handle valid_<limit> temporary auxiliary values.
! 2015/07/09, GM: Used poor man's C-preprocessor based templates.
! 2015/07/16, GM: Added support to read packed data.
! 2016/07/11, SP: Addednew variable: startp
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define NC_READ_TYPE integer
#define NC_READ_KIND byte
#define NC_READ_FILL_VALUE byte_fill_value
#define NC_READ_NAME_1D read_byte_1d
#define NC_READ_NAME_2D read_byte_2d
#define NC_READ_NAME_3D read_byte_3d
#define NC_READ_NAME_4D read_byte_4d
#include "nc_read_template.inc"
#undef NC_READ_TYPE
#undef NC_READ_KIND
#undef NC_READ_FILL_VALUE
#undef NC_READ_NAME_1D
#undef NC_READ_NAME_2D
#undef NC_READ_NAME_3D
#undef NC_READ_NAME_4D

#define NC_READ_TYPE integer
#define NC_READ_KIND sint
#define NC_READ_FILL_VALUE sint_fill_value
#define NC_READ_NAME_1D read_sint_1d
#define NC_READ_NAME_2D read_sint_2d
#define NC_READ_NAME_3D read_sint_3d
#define NC_READ_NAME_4D read_sint_4d
#include "nc_read_template.inc"
#undef NC_READ_TYPE
#undef NC_READ_KIND
#undef NC_READ_FILL_VALUE
#undef NC_READ_NAME_1D
#undef NC_READ_NAME_2D
#undef NC_READ_NAME_3D
#undef NC_READ_NAME_4D

#define NC_READ_TYPE integer
#define NC_READ_KIND lint
#define NC_READ_FILL_VALUE lint_fill_value
#define NC_READ_NAME_1D read_lint_1d
#define NC_READ_NAME_2D read_lint_2d
#define NC_READ_NAME_3D read_lint_3d
#define NC_READ_NAME_4D read_lint_4d
#include "nc_read_template.inc"
#undef NC_READ_TYPE
#undef NC_READ_KIND
#undef NC_READ_FILL_VALUE
#undef NC_READ_NAME_1D
#undef NC_READ_NAME_2D
#undef NC_READ_NAME_3D
#undef NC_READ_NAME_4D

#define NC_READ_TYPE real
#define NC_READ_KIND sreal
#define NC_RD_P_TYPE real
#define NC_RD_P_KIND sreal
#define NC_READ_FILL_VALUE sreal_fill_value
#define NC_READ_NAME_1D read_sreal_1d
#define NC_READ_NAME_2D read_sreal_2d
#define NC_READ_NAME_3D read_sreal_3d
#define NC_READ_NAME_4D read_sreal_4d
#define NC_READ_PACKED_NAME_1D read_packed_sreal_1d
#define NC_READ_PACKED_NAME_2D read_packed_sreal_2d
#define NC_READ_PACKED_NAME_3D read_packed_sreal_3d
#define NC_READ_PACKED_NAME_4D read_packed_sreal_4d
#include "nc_read_template.inc"
#undef NC_READ_TYPE
#undef NC_READ_KIND
#undef NC_RD_P_TYPE
#undef NC_RD_P_KIND
#undef NC_READ_FILL_VALUE
#undef NC_READ_NAME_1D
#undef NC_READ_NAME_2D
#undef NC_READ_NAME_3D
#undef NC_READ_NAME_4D
#undef NC_READ_PACKED_NAME_1D
#undef NC_READ_PACKED_NAME_2D
#undef NC_READ_PACKED_NAME_3D
#undef NC_READ_PACKED_NAME_4D

#define NC_READ_TYPE real
#define NC_READ_KIND dreal
#define NC_READ_FILL_VALUE dreal_fill_value
#define NC_READ_NAME_1D read_dreal_1d
#define NC_READ_NAME_2D read_dreal_2d
#define NC_READ_NAME_3D read_dreal_3d
#define NC_READ_NAME_4D read_dreal_4d
#include "nc_read_template.inc"
#undef NC_READ_TYPE
#undef NC_READ_KIND
#undef NC_READ_FILL_VALUE
#undef NC_READ_NAME_1D
#undef NC_READ_NAME_2D
#undef NC_READ_NAME_3D
#undef NC_READ_NAME_4D

!-------------------------------------------------------------------------------
! Name: nc_write_array
!
! Purpose:
! A module procedure for writing arrays of various sizes and types to a
! NetCDF file. It currently supports writing arrays of 1 to 4 dimensions of
! type 1, 2, or 4 byte integer and 4 or 8 byte real.
!
! Description and Algorithm details:
!
! Arguments:
! Name    Type    In/Out/Both Description
! ------------------------------------------------------------------------------
!
! History:
! 2014/09/01, GM: Original version.
! 2014/09/02, GM: Added 1d, 3d, and 4d nc_write_array capability.
! 2014/09/08, GM: Used poor man's C-preprocessor based templates.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define NC_WRITE_TYPE integer
#define NC_WRITE_KIND byte
#define NC_WRITE_NAME_1D write_byte_1d
#define NC_WRITE_NAME_2D write_byte_2d
#define NC_WRITE_NAME_3D write_byte_3d
#define NC_WRITE_NAME_4D write_byte_4d
#include "nc_write_template.inc"
#undef NC_WRITE_TYPE
#undef NC_WRITE_KIND
#undef NC_WRITE_NAME_1D
#undef NC_WRITE_NAME_2D
#undef NC_WRITE_NAME_3D
#undef NC_WRITE_NAME_4D

#define NC_WRITE_TYPE integer
#define NC_WRITE_KIND sint
#define NC_WRITE_NAME_1D write_sint_1d
#define NC_WRITE_NAME_2D write_sint_2d
#define NC_WRITE_NAME_3D write_sint_3d
#define NC_WRITE_NAME_4D write_sint_4d
#include "nc_write_template.inc"
#undef NC_WRITE_TYPE
#undef NC_WRITE_KIND
#undef NC_WRITE_NAME_1D
#undef NC_WRITE_NAME_2D
#undef NC_WRITE_NAME_3D
#undef NC_WRITE_NAME_4D

#define NC_WRITE_TYPE integer
#define NC_WRITE_KIND lint
#define NC_WRITE_NAME_1D write_lint_1d
#define NC_WRITE_NAME_2D write_lint_2d
#define NC_WRITE_NAME_3D write_lint_3d
#define NC_WRITE_NAME_4D write_lint_4d
#include "nc_write_template.inc"
#undef NC_WRITE_TYPE
#undef NC_WRITE_KIND
#undef NC_WRITE_NAME_1D
#undef NC_WRITE_NAME_2D
#undef NC_WRITE_NAME_3D
#undef NC_WRITE_NAME_4D

#define NC_WRITE_TYPE real
#define NC_WRITE_KIND sreal
#define NC_WRITE_NAME_1D write_sreal_1d
#define NC_WRITE_NAME_2D write_sreal_2d
#define NC_WRITE_NAME_3D write_sreal_3d
#define NC_WRITE_NAME_4D write_sreal_4d
#include "nc_write_template.inc"
#undef NC_WRITE_TYPE
#undef NC_WRITE_KIND
#undef NC_WRITE_NAME_1D
#undef NC_WRITE_NAME_2D
#undef NC_WRITE_NAME_3D
#undef NC_WRITE_NAME_4D

#define NC_WRITE_TYPE real
#define NC_WRITE_KIND dreal
#define NC_WRITE_NAME_1D write_dreal_1d
#define NC_WRITE_NAME_2D write_dreal_2d
#define NC_WRITE_NAME_3D write_dreal_3d
#define NC_WRITE_NAME_4D write_dreal_4d
#include "nc_write_template.inc"
#undef NC_WRITE_TYPE
#undef NC_WRITE_KIND
#undef NC_WRITE_NAME_1D
#undef NC_WRITE_NAME_2D
#undef NC_WRITE_NAME_3D
#undef NC_WRITE_NAME_4D


#include "orac_nc_utils.F90"


end module orac_ncdf_m
