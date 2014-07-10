!-------------------------------------------------------------------------------
! Name: imager_structures.f90
!
! Purpose:
! Define variables types which hold the imager input data. should be common to
! all imagers.
! 
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2011/12/19, MJ: writes sample code for MODIS L1b data.
! 2012/02/03, MJ: adds uncertainty array to imager_measurements_s
! 2012/04/24, GT: Added solar azimuth angle to the
!   imager_angles structure (needed by surface reflectance routines)
! 2012/04/24, GT: Added solar azimuth angle to the
!   imager_angles structure (needed by surface reflectance routines)
! 2012/07/04, CP: removed nviews for arrays!
! 2012/07/05, CP: nmaxchannels and nchannels info is now in channel_infor array
! 2012/12/13, CP: added startyi and endye
! 2013/09/02, AP: Removed startyi, endye.
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

module imager_structures

   use preproc_constants

   implicit none

   type imager_measurements_s

      integer(kind=lint) :: nchannels
      integer(kind=lint) :: nviews
      integer(kind=lint) :: nobservations

      real(kind=sreal), dimension(:,:,:), pointer ::  data
      real(kind=sreal), dimension(:,:,:), pointer ::  uncertainty

   end type imager_measurements_s

   type imager_geolocation_s

      integer(kind=lint) :: nx,ny,startx,starty,endx,endy

      real(kind=sreal), dimension(:,:), pointer ::  latitude
      real(kind=sreal), dimension(:,:), pointer ::  longitude

      integer(kind=lint), dimension(:,:), pointer ::  uscan
      integer(kind=lint), dimension(:,:), pointer ::  vscan

   end type imager_geolocation_s

   type imager_angles_s

      integer(kind=lint) :: nviews

      real(kind=sreal), dimension(:,:,:), pointer ::  solzen
      real(kind=sreal), dimension(:,:,:), pointer ::  satzen
      real(kind=sreal), dimension(:,:,:), pointer ::  solazi
      real(kind=sreal), dimension(:,:,:), pointer ::  relazi

   end type imager_angles_s

   type imager_flags_s

      integer(kind=sint), dimension(:,:), pointer :: lsflag
      integer(kind=sint), dimension(:,:), pointer :: cflag

   end type imager_flags_s

   type imager_time_s

      real(kind=dreal), dimension(:,:), pointer ::  time

   end type imager_time_s

contains

   include 'allocate_imager_structures.F90'
   include 'deallocate_imager_structures.F90'

end module imager_structures
