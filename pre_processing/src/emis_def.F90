module emis_def

  type emis_s
!    Data dimensions
     integer(kind=4)                      :: nlon
     integer(kind=4)                      :: nlat
     integer(kind=4)                      :: nbands
!    Quality information
     integer(kind=2), allocatable, dimension(:,:) :: flag
!    Bands
     real, allocatable, dimension(:)      :: wavenumber
     integer, allocatable, dimension(:)   :: bands
!    Data
     real, allocatable, dimension(:)      :: lat
     real, allocatable, dimension(:)      :: lon
     real, allocatable, dimension(:,:,:)  :: emissivity
!    Missing data value
     real                                 :: fill=-999.0
  end type emis_s

  contains

  subroutine deallocate_emis(emis)

    implicit none

    type(emis_s), intent(inout) :: emis
    
    if (allocated(emis%wavenumber)) deallocate(emis%wavenumber)
    if (allocated(emis%flag)) deallocate(emis%flag)
    if (allocated(emis%bands)) deallocate(emis%bands)
    if (allocated(emis%lat)) deallocate(emis%lat)
    if (allocated(emis%lon)) deallocate(emis%lon)
    if (allocated(emis%emissivity)) deallocate(emis%emissivity)

  end subroutine deallocate_emis

end module emis_def
