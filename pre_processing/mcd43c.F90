!-------------------------------------------------------------------------------
! Name: mcd43c.F90
!
! Purpose:
! Defines the MCD structure for holding MODIS L3 surface data and the
! deallocation routine for deleting the same structure.
!
! Description and Algorithm details:
! 1) Deallocate all fields of the structure.
!
! Arguments deallocate_mcd43c :
! Name Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! mcd  type(mcd) Both        The mcd structure to be deallocated
!
! History:
! 11 Apr 2012, GT: Original
! 11 Jun 2014, AP: Remove unique fill value
! 10 Aug 2014, GM: Changes related to new BRDF support.
! 11 Aug 2014, AP: Lat/lon grid now defined with start and division rather than
!   array.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module mcd43c_m

   implicit none


   type mcd43c1
      ! Data dimensions
      integer*4                  :: nlon
      integer*4                  :: nlat
      integer*4                  :: nbands

      ! Quality information
      integer*1, allocatable     :: quality(:,:)
      integer*1, allocatable     :: local_solar_noon(:,:)
      integer*1, allocatable     :: percent_inputs(:,:)
      integer*1, allocatable     :: percent_snow(:,:)

      ! Band identifiers
      character(len=10), pointer :: bandids(:)
      integer, allocatable       :: bands(:)

      ! Data
      real*8                     :: lat0, lat_invdel
      real*8                     :: lon0, lon_invdel
      real, allocatable          :: brdf_albedo_params(:,:,:,:)
   end type mcd43c1


   type mcd43c3
      ! Data dimensions
      integer*4                  :: nlon
      integer*4                  :: nlat
      integer*4                  :: nbands

      ! Quality information
      integer*1, allocatable     :: quality(:,:)
      integer*1, allocatable     :: local_solar_noon(:,:)
      integer*1, allocatable     :: percent_inputs(:,:)
      integer*1, allocatable     :: percent_snow(:,:)

      ! Band identifiers
      character(len=10), pointer :: bandids(:)
      integer, allocatable       :: bands(:)

      ! Data
      real*8                     :: lat0, lat_invdel
      real*8                     :: lon0, lon_invdel
      real, allocatable          :: WSA(:,:,:)
      real, allocatable          :: BSA(:,:,:)
   end type mcd43c3

contains

subroutine deallocate_mcd43c1(mcd)

   implicit none

   type(mcd43c1), intent(inout) :: mcd

   deallocate(mcd%quality)
   deallocate(mcd%local_solar_noon)
   deallocate(mcd%percent_inputs)
   deallocate(mcd%percent_snow)
   deallocate(mcd%bandids)
   deallocate(mcd%bands)
   deallocate(mcd%brdf_albedo_params)

end subroutine deallocate_mcd43c1


subroutine deallocate_mcd43c3(mcd)

   implicit none

   type(mcd43c3), intent(inout) :: mcd

   deallocate(mcd%quality)
   deallocate(mcd%local_solar_noon)
   deallocate(mcd%percent_inputs)
   deallocate(mcd%percent_snow)
   deallocate(mcd%bandids)
   deallocate(mcd%bands)
   deallocate(mcd%WSA)
   deallocate(mcd%BSA)

end subroutine deallocate_mcd43c3


include 'read_mcd43c1.F90'
include 'read_mcd43c3.F90'

end module mcd43c_m
