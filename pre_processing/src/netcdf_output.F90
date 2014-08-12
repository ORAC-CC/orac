!-------------------------------------------------------------------------------
! Name: netcdf_output.F90
!
! Purpose:
! Container for NCDF output write routines.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2014/05/23, GM: First version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module netcdf_output

   implicit none

contains

include 'netcdf_output_create_file.F90'
include 'netcdf_output_open.F90'
include 'netcdf_output_close.F90'
include 'netcdf_output_write_swath.F90'

end module netcdf_output
