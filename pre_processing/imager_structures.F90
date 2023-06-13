!-------------------------------------------------------------------------------
! Name: imager_structures.F90
!
! Purpose:
! Module defining variables types which hold the imager input data. Should be
! common to all imagers.
!
! History:
! 2011/12/19, MJ: writes sample code for MODIS L1b data.
! 2012/02/03, MJ: adds uncertainty array to imager_measurements_t
! 2012/04/24, GT: Added solar azimuth angle to the
!    imager_angles structure (needed by surface reflectance routines)
! 2012/04/24, GT: Added solar azimuth angle to the
!    imager_angles structure (needed by surface reflectance routines)
! 2012/07/04, CP: removed nviews for arrays!
! 2012/07/05, CP: nmaxchannels and nchannels info is now in channel_infor array
! 2012/12/13, CP: added startyi and endye
! 2013/09/02, AP: Removed startyi, endye.
! 2014/09/17, CS: Added imager_pavolonis, imager_geolocation%usgs_dem and
!    imager_flags%lusflag
! 2014/12/01, OS: added variable emis_ch3b to Pavolonis imager structure
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2015/07/02, OS: added cldmask_uncertainty
! 2016/04/09, SP: Added multiple views
! 2017/03/29, SP: Add new variable for tropopause cloud emissivity (ExtWork)
! 2017/06/21, OS: added ann phase variables
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2018/02/02, GT: Added total number of pixels across and along track to
!    imager_geolocation (in addition to the nx and ny values, which reflect
!    user specified start and end coordinates)
! 2018/11/05, SP: Add CAPE
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module imager_structures_m

   use preproc_constants_m

   implicit none

   type imager_measurements_t

      integer(kind=lint) :: nchannels
      integer(kind=lint) :: nviews

      real(kind=sreal), dimension(:,:,:), pointer :: data
      real(kind=sreal), dimension(:,:,:), pointer :: uncertainty
      real(kind=sreal), dimension(:),     pointer :: cal_gain

   end type imager_measurements_t

   type imager_geolocation_t

      integer(kind=lint) :: nx,ny,startx,starty,endx,endy

      real(kind=sreal), dimension(:,:), pointer :: latitude
      real(kind=sreal), dimension(:,:), pointer :: longitude

      integer(kind=sint), dimension(:,:), pointer :: dem

   end type imager_geolocation_t

   ! IMPORTANT NOTE
   ! The code expects relative azimuth = 0 when sun and sensor are on OPPOSITE
   ! sides of the pixel. It expects relative azimuth = 180 when sun and sensor
   ! are on the SAME side of the pixel.
   ! THIS IS NOT THE STANDARD DEFINITION, BEWARE WHEN ADDING NEW SENSORS
   ! /IMPORTANT NOTE

   type imager_angles_t

      integer(kind=lint) :: nviews

      real(kind=sreal), dimension(:,:,:), pointer :: solzen
      real(kind=sreal), dimension(:,:,:), pointer :: satzen
      real(kind=sreal), dimension(:,:,:), pointer :: solazi
      real(kind=sreal), dimension(:,:,:), pointer :: satazi
      real(kind=sreal), dimension(:,:,:), pointer :: relazi

   end type imager_angles_t

   type imager_flags_t

      integer(kind=byte), dimension(:,:),   pointer :: lusflag
      integer(kind=byte), dimension(:,:),   pointer :: lsflag
      integer(kind=byte), dimension(:,:,:), pointer :: cflag

   end type imager_flags_t

   type imager_time_t

      real(kind=dreal), dimension(:,:), pointer :: time

   end type imager_time_t

   type imager_pavolonis_t

      integer(kind=sint), dimension(:,:),   pointer :: sfctype
      integer(kind=byte), dimension(:,:,:), pointer :: cldtype
      integer(kind=byte), dimension(:,:,:), pointer :: cldmask
      real(kind=sreal),   dimension(:,:,:), pointer :: cldmask_uncertainty
      real(kind=sreal),   dimension(:,:,:), pointer :: cccot_pre
      integer(kind=byte), dimension(:,:,:), pointer :: ann_phase
      real(kind=sreal),   dimension(:,:,:), pointer :: ann_phase_uncertainty
      real(kind=sreal),   dimension(:,:,:), pointer :: cphcot
      integer(kind=byte), dimension(:,:,:), pointer :: cirrus_quality
      real(kind=sreal),   dimension(:,:,:), pointer :: emis_ch3b

   end type imager_pavolonis_t

   type imager_cloud_t

      real(kind=sreal), dimension(:,:,:),   pointer :: cloud_emis
      real(kind=sreal), dimension(:,:),     pointer :: trop_t
      real(kind=sreal), dimension(:,:),     pointer :: trop_p
      real(kind=sreal), dimension(:,:),     pointer :: cape

   end type imager_cloud_t

contains

#include "allocate_imager_structures.F90"
#include "deallocate_imager_structures.F90"

end module imager_structures_m
