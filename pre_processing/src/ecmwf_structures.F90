! Name: ecmwf_structures.F90
!
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
! 2012/01/10: MJ writes sample code for ERA Interim data.
! 2012/08/02: CP changed to accomodate badc netcdf files
!                with different dimensions
! 2013/10/23: AP Tidying
! 2014/02/10: AP removed _nc_ structures as redundant. Shortened DIM names.
!
! $Id$
!
! Bugs:
! none known
!

module ecmwf_structures

  use preproc_constants

  implicit none
  
  type ecmwf_dims_s
     integer (kind=lint) :: xdim,ydim,kdim

  end type ecmwf_dims_s

  type ecmwf_3d_s
     real(kind=sreal), dimension(:,:,:), pointer ::  temperature,spec_hum,ozone

  end type ecmwf_3d_s

  type ecmwf_2d_s
     real(kind=sreal), dimension(:,:), pointer ::  latitude,longitude

     real(kind=sreal), dimension(:,:), pointer ::  sst,geopot,lnsp, &
          sea_ice_cover,snow_albedo,totcolwv,snow_depth

     real(kind=sreal), dimension(:,:), pointer ::  u10,v10,temp2, &
          land_sea_mask,skin_temp

  end type ecmwf_2d_s

contains

  include 'allocate_ecmwf_structures.F90'
  include 'deallocate_ecmwf_structures.F90'

end module ecmwf_structures
