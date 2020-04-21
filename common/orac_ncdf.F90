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
! 2015/03/20, CP: Changed to creator url and website
! 2015/07/10, OS: added optional error_status return argument
! 2015/07/16, GM: Added support to read packed data to nc_read routines.
! 2016/07/11, SP: Added new variable to read functions: start
! 2017/07/10, AP: Add int64 fields.
! 2020/02/02, AP: Added nc_close().
! 2020/04/21, AP: Renamed routines ncdf_ to avoid clobber of library routines.
!-------------------------------------------------------------------------------

module orac_ncdf_m

   use netcdf
   use common_constants_m

   implicit none

   interface ncdf_read_array
      module procedure &
         read_byte_0d,  read_byte_1d,  read_byte_2d,  read_byte_3d,  read_byte_4d, &
         read_sint_0d,  read_sint_1d,  read_sint_2d,  read_sint_3d,  read_sint_4d, &
         read_lint_0d,  read_lint_1d,  read_lint_2d,  read_lint_3d,  read_lint_4d, &
         read_dint_0d,  read_dint_1d,  read_dint_2d,  read_dint_3d,  read_dint_4d, &
         read_sreal_0d, read_sreal_1d, read_sreal_2d, read_sreal_3d, read_sreal_4d, &
         read_dreal_0d, read_dreal_1d, read_dreal_2d, read_dreal_3d, read_dreal_4d
   end interface ncdf_read_array

   interface ncdf_write_array
      module procedure &
         write_byte_1d, write_byte_2d, write_byte_3d, write_byte_4d, &
         write_sint_1d, write_sint_2d, write_sint_3d, write_sint_4d, &
         write_lint_1d, write_lint_2d, write_lint_3d, write_lint_4d, &
         write_dint_1d, write_dint_2d, write_dint_3d, write_dint_4d, &
         write_sreal_1d, write_sreal_2d, write_sreal_3d, write_sreal_4d, &
         write_dreal_1d, write_dreal_2d, write_dreal_3d, write_dreal_4d
   end interface ncdf_write_array

   interface ncdf_read_packed_array
      module procedure &
         read_packed_sreal_1d, read_packed_sreal_2d, read_packed_sreal_3d, &
         read_packed_sreal_4d
   end interface ncdf_read_packed_array

contains


subroutine c_to_f_str(str)
   use iso_c_binding, only: C_NULL_CHAR

   implicit none

   character(len=*), intent(inout) :: str
   integer :: i

   do i=1,len(str)
      if (str(i:i) == C_NULL_CHAR) exit
   end do

   str(i:len(str)) = ' '
end subroutine c_to_f_str

!-------------------------------------------------------------------------------
! Name: ncdf_open
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
! ncid           integer Out File ID number returned by nf90_open
! fname          string  In  Name of the file to be opened
! source_routine string  In  Name of the routine that called this
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
subroutine ncdf_open(ncid, fname, source_routine)
   implicit none

   integer,          intent(out) :: ncid
   character(len=*), intent(in)  :: fname
   character(len=*), intent(in)  :: source_routine
   integer                       :: ierr

   ierr = nf90_open(path=trim(adjustl(fname)), mode=NF90_NOWRITE, ncid=ncid)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: ncdf_open(): ', source_routine, &
           ': Error opening file ', trim(fname)
      print*, trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

end subroutine ncdf_open

!-------------------------------------------------------------------------------
! Name: ncdf_close
!
! Purpose:
! Wrapper for nf90_close with error handling.
!
! Description and Algorithm details:
! 1) Call nf90_close. If error, print message.
!
! Arguments:
! Name  Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid           integer Out File ID number returned by nf90_open
! source_routine string  In  Name of the routine that called this
!
! History:
! 2020/02/03, AP: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine ncdf_close(ncid, source_routine)
   implicit none

   integer,          intent(in) :: ncid
   character(len=*), intent(in) :: source_routine
   integer                      :: ierr

   ierr = nf90_close(ncid)
   if (ierr .ne. NF90_NOERR) then
      print*, 'ERROR: ncdf_close(): ', source_routine, &
           ': Error closing file'
      print*, trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

end subroutine ncdf_close

!-------------------------------------------------------------------------------
! Name: ncdf_dim_length
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
! ncid           integer Out File ID number returned by nf90_open
! name           string  In  Name of dimension to read
! source_routine string  In  Name of the routine that called this
! len            integer Out Length of desired dimenison
!
! History:
! 2014/08/06, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function ncdf_dim_length(ncid, name, source_routine, verbose) result(len)
   implicit none

   integer,          intent(in) :: ncid
   character(len=*), intent(in) :: name
   character(len=*), intent(in) :: source_routine
   logical,          intent(in) :: verbose

   integer :: did, ierr, len
   character(len=NF90_MAX_NAME) :: dname

   ierr = nf90_inq_dimid(ncid, name, did)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: ncdf_dim_length(): ', source_routine, &
           ': Could not locate dimension ', trim(name)
      print*, trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   ierr = nf90_inquire_dimension(ncid, did, dname, len)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: ncdf_dim_length(): ', source_routine, &
           ': Could not read dimension ', trim(name)
      print*, trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   if (verbose) print*, trim(name),' dim length: ',len

end function ncdf_dim_length

!-------------------------------------------------------------------------------
! Name: ncdf_def_var
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

#define NCDF_DEF_VAR_NAME ncdf_def_var_byte_packed_byte
#define NCDF_DEF_VAR_TYPE_PACKED integer
#define NCDF_DEF_VAR_KIND_PACKED byte
#define NCDF_DEF_VAR_TYPE_UNPACKED integer
#define NCDF_DEF_VAR_KIND_UNPACKED byte
#define NCDF_DEF_VAR_NF90_TYPE NF90_BYTE
#include "ncdf_def_var_template.inc"
#undef NCDF_DEF_VAR_NAME
#undef NCDF_DEF_VAR_TYPE_PACKED
#undef NCDF_DEF_VAR_KIND_PACKED
#undef NCDF_DEF_VAR_TYPE_UNPACKED
#undef NCDF_DEF_VAR_KIND_UNPACKED
#undef NCDF_DEF_VAR_NF90_TYPE

#define NCDF_DEF_VAR_NAME ncdf_def_var_short_packed_float
#define NCDF_DEF_VAR_TYPE_PACKED integer
#define NCDF_DEF_VAR_KIND_PACKED sint
#define NCDF_DEF_VAR_TYPE_UNPACKED real
#define NCDF_DEF_VAR_KIND_UNPACKED sreal
#define NCDF_DEF_VAR_NF90_TYPE NF90_SHORT
#include "ncdf_def_var_template.inc"
#undef NCDF_DEF_VAR_NAME
#undef NCDF_DEF_VAR_TYPE_PACKED
#undef NCDF_DEF_VAR_KIND_PACKED
#undef NCDF_DEF_VAR_TYPE_UNPACKED
#undef NCDF_DEF_VAR_KIND_UNPACKED
#undef NCDF_DEF_VAR_NF90_TYPE

#define NCDF_DEF_VAR_NAME ncdf_def_var_short_packed_short
#define NCDF_DEF_VAR_TYPE_PACKED integer
#define NCDF_DEF_VAR_KIND_PACKED sint
#define NCDF_DEF_VAR_TYPE_UNPACKED integer
#define NCDF_DEF_VAR_KIND_UNPACKED sint
#define NCDF_DEF_VAR_NF90_TYPE NF90_SHORT
#include "ncdf_def_var_template.inc"
#undef NCDF_DEF_VAR_NAME
#undef NCDF_DEF_VAR_TYPE_PACKED
#undef NCDF_DEF_VAR_KIND_PACKED
#undef NCDF_DEF_VAR_TYPE_UNPACKED
#undef NCDF_DEF_VAR_KIND_UNPACKED
#undef NCDF_DEF_VAR_NF90_TYPE

#define NCDF_DEF_VAR_NAME ncdf_def_var_long_packed_long
#define NCDF_DEF_VAR_TYPE_PACKED integer
#define NCDF_DEF_VAR_KIND_PACKED lint
#define NCDF_DEF_VAR_TYPE_UNPACKED integer
#define NCDF_DEF_VAR_KIND_UNPACKED lint
#define NCDF_DEF_VAR_NF90_TYPE NF90_INT
#include "ncdf_def_var_template.inc"
#undef NCDF_DEF_VAR_NAME
#undef NCDF_DEF_VAR_TYPE_PACKED
#undef NCDF_DEF_VAR_KIND_PACKED
#undef NCDF_DEF_VAR_TYPE_UNPACKED
#undef NCDF_DEF_VAR_KIND_UNPACKED
#undef NCDF_DEF_VAR_NF90_TYPE

#define NCDF_DEF_VAR_NAME ncdf_def_var_dlong_packed_dlong
#define NCDF_DEF_VAR_TYPE_PACKED integer
#define NCDF_DEF_VAR_KIND_PACKED dint
#define NCDF_DEF_VAR_TYPE_UNPACKED integer
#define NCDF_DEF_VAR_KIND_UNPACKED dint
#define NCDF_DEF_VAR_NF90_TYPE NF90_INT64
#include "ncdf_def_var_template.inc"
#undef NCDF_DEF_VAR_NAME
#undef NCDF_DEF_VAR_TYPE_PACKED
#undef NCDF_DEF_VAR_KIND_PACKED
#undef NCDF_DEF_VAR_TYPE_UNPACKED
#undef NCDF_DEF_VAR_KIND_UNPACKED
#undef NCDF_DEF_VAR_NF90_TYPE

#define NCDF_DEF_VAR_NAME ncdf_def_var_float_packed_float
#define NCDF_DEF_VAR_TYPE_PACKED real
#define NCDF_DEF_VAR_KIND_PACKED sreal
#define NCDF_DEF_VAR_TYPE_UNPACKED real
#define NCDF_DEF_VAR_KIND_UNPACKED sreal
#define NCDF_DEF_VAR_NF90_TYPE NF90_FLOAT
#include "ncdf_def_var_template.inc"
#undef NCDF_DEF_VAR_NAME
#undef NCDF_DEF_VAR_TYPE_PACKED
#undef NCDF_DEF_VAR_KIND_PACKED
#undef NCDF_DEF_VAR_TYPE_UNPACKED
#undef NCDF_DEF_VAR_KIND_UNPACKED
#undef NCDF_DEF_VAR_NF90_TYPE

#define NCDF_DEF_VAR_NAME ncdf_def_var_double_packed_double
#define NCDF_DEF_VAR_TYPE_PACKED real
#define NCDF_DEF_VAR_KIND_PACKED dreal
#define NCDF_DEF_VAR_TYPE_UNPACKED real
#define NCDF_DEF_VAR_KIND_UNPACKED dreal
#define NCDF_DEF_VAR_NF90_TYPE NF90_DOUBLE
#include "ncdf_def_var_template.inc"
#undef NCDF_DEF_VAR_NAME
#undef NCDF_DEF_VAR_TYPE_PACKED
#undef NCDF_DEF_VAR_KIND_PACKED
#undef NCDF_DEF_VAR_TYPE_UNPACKED
#undef NCDF_DEF_VAR_KIND_UNPACKED
#undef NCDF_DEF_VAR_NF90_TYPE

!-------------------------------------------------------------------------------
! Name: ncdf_read_array
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
! start   integer In  Optional. If set, specifies the position in the netcdf
!                     file to start reading data
!
! History:
! 2014/02/10, AP: Original version, replacing nc_read_file.F90
! 2014/08/12, AP: Adding routines for all expected data types.
! 2014/08/15, AP: Adding partial read procedure. Homogenizing use of verbose.
! 2014/09/03, GM: Added vl to handle valid_<limit> temporary auxiliary values.
! 2015/07/09, GM: Used poor man's C-preprocessor based templates.
! 2015/07/16, GM: Added support to read packed data.
! 2016/07/11, SP: Added new variable: startp
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define NCDF_READ_TYPE integer
#define NCDF_READ_KIND byte
#define NCDF_READ_FILL_VALUE byte_fill_value
#define NCDF_READ_NAME_0D read_byte_0d
#define NCDF_READ_NAME_1D read_byte_1d
#define NCDF_READ_NAME_2D read_byte_2d
#define NCDF_READ_NAME_3D read_byte_3d
#define NCDF_READ_NAME_4D read_byte_4d
#include "ncdf_read_template.inc"
#undef NCDF_READ_TYPE
#undef NCDF_READ_KIND
#undef NCDF_READ_FILL_VALUE
#undef NCDF_READ_NAME_0D
#undef NCDF_READ_NAME_1D
#undef NCDF_READ_NAME_2D
#undef NCDF_READ_NAME_3D
#undef NCDF_READ_NAME_4D

#define NCDF_READ_TYPE integer
#define NCDF_READ_KIND sint
#define NCDF_READ_FILL_VALUE sint_fill_value
#define NCDF_READ_NAME_0D read_sint_0d
#define NCDF_READ_NAME_1D read_sint_1d
#define NCDF_READ_NAME_2D read_sint_2d
#define NCDF_READ_NAME_3D read_sint_3d
#define NCDF_READ_NAME_4D read_sint_4d
#include "ncdf_read_template.inc"
#undef NCDF_READ_TYPE
#undef NCDF_READ_KIND
#undef NCDF_READ_FILL_VALUE
#undef NCDF_READ_NAME_0D
#undef NCDF_READ_NAME_1D
#undef NCDF_READ_NAME_2D
#undef NCDF_READ_NAME_3D
#undef NCDF_READ_NAME_4D

#define NCDF_READ_TYPE integer
#define NCDF_READ_KIND lint
#define NCDF_READ_FILL_VALUE lint_fill_value
#define NCDF_READ_NAME_0D read_lint_0d
#define NCDF_READ_NAME_1D read_lint_1d
#define NCDF_READ_NAME_2D read_lint_2d
#define NCDF_READ_NAME_3D read_lint_3d
#define NCDF_READ_NAME_4D read_lint_4d
#include "ncdf_read_template.inc"
#undef NCDF_READ_TYPE
#undef NCDF_READ_KIND
#undef NCDF_READ_FILL_VALUE
#undef NCDF_READ_NAME_0D
#undef NCDF_READ_NAME_1D
#undef NCDF_READ_NAME_2D
#undef NCDF_READ_NAME_3D
#undef NCDF_READ_NAME_4D

#define NCDF_READ_TYPE integer
#define NCDF_READ_KIND dint
#define NCDF_READ_FILL_VALUE dint_fill_value
#define NCDF_READ_NAME_0D read_dint_0d
#define NCDF_READ_NAME_1D read_dint_1d
#define NCDF_READ_NAME_2D read_dint_2d
#define NCDF_READ_NAME_3D read_dint_3d
#define NCDF_READ_NAME_4D read_dint_4d
#include "ncdf_read_template.inc"
#undef NCDF_READ_TYPE
#undef NCDF_READ_KIND
#undef NCDF_READ_FILL_VALUE
#undef NCDF_READ_NAME_0D
#undef NCDF_READ_NAME_1D
#undef NCDF_READ_NAME_2D
#undef NCDF_READ_NAME_3D
#undef NCDF_READ_NAME_4D

#define NCDF_READ_TYPE real
#define NCDF_READ_KIND sreal
#define NCDF_RD_P_TYPE real
#define NCDF_RD_P_KIND sreal
#define NCDF_READ_FILL_VALUE sreal_fill_value
#define NCDF_READ_NAME_0D read_sreal_0d
#define NCDF_READ_NAME_1D read_sreal_1d
#define NCDF_READ_NAME_2D read_sreal_2d
#define NCDF_READ_NAME_3D read_sreal_3d
#define NCDF_READ_NAME_4D read_sreal_4d
#define NCDF_READ_PACKED_NAME_1D read_packed_sreal_1d
#define NCDF_READ_PACKED_NAME_2D read_packed_sreal_2d
#define NCDF_READ_PACKED_NAME_3D read_packed_sreal_3d
#define NCDF_READ_PACKED_NAME_4D read_packed_sreal_4d
#include "ncdf_read_template.inc"
#undef NCDF_READ_TYPE
#undef NCDF_READ_KIND
#undef NCDF_RD_P_TYPE
#undef NCDF_RD_P_KIND
#undef NCDF_READ_FILL_VALUE
#undef NCDF_READ_NAME_0D
#undef NCDF_READ_NAME_1D
#undef NCDF_READ_NAME_2D
#undef NCDF_READ_NAME_3D
#undef NCDF_READ_NAME_4D
#undef NCDF_READ_PACKED_NAME_1D
#undef NCDF_READ_PACKED_NAME_2D
#undef NCDF_READ_PACKED_NAME_3D
#undef NCDF_READ_PACKED_NAME_4D

#define NCDF_READ_TYPE real
#define NCDF_READ_KIND dreal
#define NCDF_READ_FILL_VALUE dreal_fill_value
#define NCDF_READ_NAME_0D read_dreal_0d
#define NCDF_READ_NAME_1D read_dreal_1d
#define NCDF_READ_NAME_2D read_dreal_2d
#define NCDF_READ_NAME_3D read_dreal_3d
#define NCDF_READ_NAME_4D read_dreal_4d
#include "ncdf_read_template.inc"
#undef NCDF_READ_TYPE
#undef NCDF_READ_KIND
#undef NCDF_READ_FILL_VALUE
#undef NCDF_READ_NAME_0D
#undef NCDF_READ_NAME_1D
#undef NCDF_READ_NAME_2D
#undef NCDF_READ_NAME_3D
#undef NCDF_READ_NAME_4D

!-------------------------------------------------------------------------------
! Name: ncdf_write_array
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

#define NCDF_WRITE_TYPE integer
#define NCDF_WRITE_KIND byte
#define NCDF_WRITE_NAME_1D write_byte_1d
#define NCDF_WRITE_NAME_2D write_byte_2d
#define NCDF_WRITE_NAME_3D write_byte_3d
#define NCDF_WRITE_NAME_4D write_byte_4d
#include "ncdf_write_template.inc"
#undef NCDF_WRITE_TYPE
#undef NCDF_WRITE_KIND
#undef NCDF_WRITE_NAME_1D
#undef NCDF_WRITE_NAME_2D
#undef NCDF_WRITE_NAME_3D
#undef NCDF_WRITE_NAME_4D

#define NCDF_WRITE_TYPE integer
#define NCDF_WRITE_KIND sint
#define NCDF_WRITE_NAME_1D write_sint_1d
#define NCDF_WRITE_NAME_2D write_sint_2d
#define NCDF_WRITE_NAME_3D write_sint_3d
#define NCDF_WRITE_NAME_4D write_sint_4d
#include "ncdf_write_template.inc"
#undef NCDF_WRITE_TYPE
#undef NCDF_WRITE_KIND
#undef NCDF_WRITE_NAME_1D
#undef NCDF_WRITE_NAME_2D
#undef NCDF_WRITE_NAME_3D
#undef NCDF_WRITE_NAME_4D

#define NCDF_WRITE_TYPE integer
#define NCDF_WRITE_KIND lint
#define NCDF_WRITE_NAME_1D write_lint_1d
#define NCDF_WRITE_NAME_2D write_lint_2d
#define NCDF_WRITE_NAME_3D write_lint_3d
#define NCDF_WRITE_NAME_4D write_lint_4d
#include "ncdf_write_template.inc"
#undef NCDF_WRITE_TYPE
#undef NCDF_WRITE_KIND
#undef NCDF_WRITE_NAME_1D
#undef NCDF_WRITE_NAME_2D
#undef NCDF_WRITE_NAME_3D
#undef NCDF_WRITE_NAME_4D

#define NCDF_WRITE_TYPE integer
#define NCDF_WRITE_KIND dint
#define NCDF_WRITE_NAME_1D write_dint_1d
#define NCDF_WRITE_NAME_2D write_dint_2d
#define NCDF_WRITE_NAME_3D write_dint_3d
#define NCDF_WRITE_NAME_4D write_dint_4d
#include "ncdf_write_template.inc"
#undef NCDF_WRITE_TYPE
#undef NCDF_WRITE_KIND
#undef NCDF_WRITE_NAME_1D
#undef NCDF_WRITE_NAME_2D
#undef NCDF_WRITE_NAME_3D
#undef NCDF_WRITE_NAME_4D

#define NCDF_WRITE_TYPE real
#define NCDF_WRITE_KIND sreal
#define NCDF_WRITE_NAME_1D write_sreal_1d
#define NCDF_WRITE_NAME_2D write_sreal_2d
#define NCDF_WRITE_NAME_3D write_sreal_3d
#define NCDF_WRITE_NAME_4D write_sreal_4d
#include "ncdf_write_template.inc"
#undef NCDF_WRITE_TYPE
#undef NCDF_WRITE_KIND
#undef NCDF_WRITE_NAME_1D
#undef NCDF_WRITE_NAME_2D
#undef NCDF_WRITE_NAME_3D
#undef NCDF_WRITE_NAME_4D

#define NCDF_WRITE_TYPE real
#define NCDF_WRITE_KIND dreal
#define NCDF_WRITE_NAME_1D write_dreal_1d
#define NCDF_WRITE_NAME_2D write_dreal_2d
#define NCDF_WRITE_NAME_3D write_dreal_3d
#define NCDF_WRITE_NAME_4D write_dreal_4d
#include "ncdf_write_template.inc"
#undef NCDF_WRITE_TYPE
#undef NCDF_WRITE_KIND
#undef NCDF_WRITE_NAME_1D
#undef NCDF_WRITE_NAME_2D
#undef NCDF_WRITE_NAME_3D
#undef NCDF_WRITE_NAME_4D


#include "orac_ncdf_utils.F90"


end module orac_ncdf_m
