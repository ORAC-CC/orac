!-------------------------------------------------------------------------------
! Name: ecmwf.F90
!
! Purpose:
! Define variables types which hold the ecmwf input data.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2012/01/10, MJ: writes sample code for ERA Interim data.
! 2012/08/02, CP: changed to accommodate badc netcdf files with different
!   dimensions
! 2013/10/23, AP: Tidying
! 2014/02/10, AP: removed _nc_ structures as redundant. Shortened DIM names.
! 2014/05/06, AP: Simplified to just one structure.
! 2014/11/04, OS: added skin temperature to ecmwf structure
! 2014/11/19, GM: C #includes should use double quotes.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module ecmwf_m

   use preproc_constants

   implicit none

   type ecmwf_s
      integer(kind=lint)                        :: xdim,ydim,kdim
      real(kind=sreal), dimension(:),   pointer :: lat,lon
      real(kind=sreal), dimension(:),   pointer :: avec,bvec
      real(kind=sreal), dimension(:,:), pointer :: u10,v10,skin_temp
   end type ecmwf_s

contains

#include "deallocate_ecmwf_structures.F90"
#include "compute_geopot_coordinate.F90"
#include "read_ecmwf_wind_nc.F90"
#include "read_ecmwf_wind_grib.F90"
#include "read_ecmwf_wind_badc.F90"
#include "read_ecmwf_nc.F90"
#include "read_ecmwf_grib.F90"
#include "rearrange_ecmwf.F90"

end module ecmwf_m
