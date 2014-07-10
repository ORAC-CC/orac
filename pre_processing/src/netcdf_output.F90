!-------------------------------------------------------------------------------
! Name: ncdf_output.F90
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
! None known
!-------------------------------------------------------------------------------

module netcdf_output

implicit none

contains

include 'nc_create_file.F90'
include 'open_netcdf_output.F90'
include 'close_netcdf_output.F90'
include 'write_swath_to_netcdf.F90'

end module netcdf_output
