! Name: mcd43c.f90 (module)
!
!
! Purpose:
! Defines the MCD structure for holding MODIS L3 surface data and the
! deallocation routine for deleting the same structure.
! 
!
! Description and Algorithm details:
!
!
! Arguments deallocate_mcd43c :
! Name Type   In/Out/Both Description
! mcd  type(mcd) Both     The mcd structure to be deallocated
!
! Local variables:
! Name Type Description
!
!
! History:
! 11 Apr 2012 Gareth Thomas: Orginal
!
! $Id$
!
! Bugs:
!
!none known
module mcd43c_def

  type mcd43c
!    Data dimensions
     integer*4                  :: nlon
     integer*4                  :: nlat
     integer*4                  :: nbands
!    Quality information
     integer*1, allocatable     :: quality(:,:)
     integer*1, allocatable     :: local_solar_noon(:,:)
     integer*1, allocatable     :: percent_snow(:,:)
     integer*1, allocatable     :: percent_inputs(:,:)
!    Band identifiers
     character(len=10), pointer :: bandids(:)
     integer, allocatable       :: bands(:)
!    Data
     real, allocatable          :: lat(:)
     real, allocatable          :: lon(:)
     real, allocatable          :: WSA(:,:,:)
     real, allocatable          :: BSA(:,:,:)
!    Missing data value
     real                       :: fill=-999.0
  end type mcd43c

  contains

  subroutine deallocate_mcd43c(mcd)

    implicit none

    type(mcd43c), intent(inout) :: mcd
    
    deallocate(mcd%quality)
    deallocate(mcd%local_solar_noon)
    deallocate(mcd%percent_snow)
    deallocate(mcd%percent_inputs)
    deallocate(mcd%bandids)
    deallocate(mcd%bands)
    deallocate(mcd%WSA)
    deallocate(mcd%BSA)
    deallocate(mcd%lat)
    deallocate(mcd%lon)
    
  end subroutine deallocate_mcd43c

end module mcd43c_def
