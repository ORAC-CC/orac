!-------------------------------------------------------------------------------
! Name: read_camel_emissivity.F90
!
! Purpose:
! Reads land surface emissivity from the CAMEL database
!
! Description and Algorithm details:
!
! Arguments:
! Name            Type     In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_file    char         In      Full path to data file
! emis           struct        Out     The Emis data structure, defined in
!                                      the emis_def module, which on return
!                                      will contain the read data.
! bands          integer       In      Index numbers of bands to read, wrt
!                                      to the numbering used by CAMEL
! flag           integer       In      OPTIONAL argument. If > 0, the "flag"
!                                      data will be read from the file
! wavenumber     integer       In      OPTIONAL argument. If > 0, the band
!                                      wavenumbers will be read from the file
! loc             char         In      OPTIONAL argument. If this is set to a
!                                      non-null string, it is assumed to define
!                                      the path to the lat-lon grid data file
!                                      for the CAMEL data (otherwise the code
!                                      generates its own grid).
! stat           integer       Out     (Return value) Status for the routine
!
! History:
! 2012/05/31, SRP: Initial version, based on existing read_cimss_emissivity.F90
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module camel_emissivity_m

   use preproc_constants_m, only : sreal_fill_value

   implicit none

   type emis_t
      !    Data dimensions
      integer(4)                                :: nlon, nlat, nbands
      !    Quality information
      integer(2), allocatable, dimension(:,:)   :: flag
      !    Bands
      integer,    allocatable, dimension(:)     :: bands
      real(8),    allocatable, dimension(:)     :: wavenumber
      !    Data
      real(8)                                   :: lon0, lon_invdel, lon_del
      real(8)                                   :: lat0, lat_invdel, lat_del
      real,       allocatable, dimension(:,:,:) :: emissivity
      !    Missing data value
      real                                      :: fill=sreal_fill_value
   end type emis_t

contains

! Used to emulate legacy behavoir for 3.7, 11, and 12 um.
#define USE_NEAREST_CHANNEL .false.

function read_camel_emissivity(path_to_file, emis, wavelengths, verbose, flag, &
   wavenumber) result (stat)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   ! Input variables
   character(len=*), intent(in)               :: path_to_file
   real,             intent(in), dimension(:) :: wavelengths
   logical,          intent(in)               :: verbose
   integer,          intent(in), optional     :: flag
   integer,          intent(in), optional     :: wavenumber

   ! Output variables
   type(emis_t),     intent(out)              :: emis
   integer(kind=sint)                         :: stat

   ! Local variables
   integer            :: i, j
   real               :: a
   integer, parameter :: nBands = 13
   integer            :: n_wavelengths
   integer            :: fid
   integer            :: xdim, ydim, zdim
   integer            :: nDim
   real, dimension(nBands) :: camel_wvl

   real, allocatable, dimension(:,:,:) :: cache

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_camel_emissivity()'

   if (verbose) write(*,*) 'path_to_file: ', trim(path_to_file)
   if (verbose) write(*,*) 'wavelengths: ',  wavelengths

   n_wavelengths = size(wavelengths)

   camel_wvl = (/ 3.6, 4.3, 5.0, 5.8, 7.6, 8.3, 8.6, 9.1, 10.6, 10.8, 11.3, 12.1, 14.3 /)

   ! Open NetCDF file
   call ncdf_open(fid, path_to_file, 'read_camel_emissivity()')

   ! Extract information about the file
   stat = nf90_inquire(fid, nDim)

   ! Now extract dimensions - should be three!
   if (nDim .gt. 3) then
      write(*,*) 'ERROR: read_camel_emissivity(): File has more than ' // &
           'three dimensions, filename: ', trim(path_to_file), ' nDim: ', nDim
      stop error_stop_code
   end if

   ! Extract the array dimensions
   xdim = ncdf_dim_length(fid, 'latitude', 'read_camel_emissivity()', verbose)
   ydim = ncdf_dim_length(fid, 'longitude', 'read_camel_emissivity()', verbose)
   zdim = ncdf_dim_length(fid, 'spectra', 'read_camel_emissivity()', verbose)

   ! Begin to populate the emis structure
   emis%nlat   = xdim
   emis%nlon   = ydim
   emis%nbands = n_wavelengths

   ! Likewise the flag
   if (present(flag)) then
      if (flag .gt. 0) then
         allocate(emis%flag(ydim,xdim))
         call ncdf_read_array(fid, 'camel_qflag', emis%flag, verbose)
      end if
   end if

   allocate(emis%emissivity(ydim,xdim,n_wavelengths))


   allocate(cache(13,7200,3600))
   call ncdf_read_array(fid, 'camel_emis', cache, verbose)

   ! Extract the data for each of the requested bands
   do i = 1, n_wavelengths
      if (wavelengths(i) .le. camel_wvl(1)) then
         emis%emissivity(:,:,i) = cache(1,:,:)
      else if (wavelengths(i) .ge. camel_wvl(nBands)) then
         emis%emissivity(:,:,i) = cache(nBands,:,:)
      else
         j = 1
         do while (.true.)
            if (wavelengths(i) .ge. camel_wvl(j) .and. &
                wavelengths(i) .lt. camel_wvl(j + 1)) exit
            j = j + 1
         end do

         a = (wavelengths(i) - camel_wvl(j)) / &
             (camel_wvl(j + 1) - camel_wvl(j))
if (USE_NEAREST_CHANNEL) then
         if (a < .5) then
            emis%emissivity(:,:,i) = cache(j,:,:)
         else
            emis%emissivity(:,:,i) = cache(j+1,:,:)
         end if
else
         emis%emissivity(:,:,i) = (1. - a) * cache(j,:,:) + a * cache(j+1,:,:)
end if
      end if
   end do

   deallocate(cache)

   ! We are now finished with the main data file
   call ncdf_close(fid, 'read_camel_emissivity()')

   ! Commented out read/generation of lat/lon arrays. As the grid is regular,
   ! simply output its starting point and the inverse of the spacing
   emis%lon0 = -179.975
   emis%lon_del = 0.05
   emis%lon_invdel = 20.
   emis%lat0 = 89.975
   emis%lat_del = -0.05
   emis%lat_invdel = -20.

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_camel_emissivity()'

end function read_camel_emissivity

subroutine deallocate_emis(emis)

   implicit none

   type(emis_t), intent(inout) :: emis

   if (allocated(emis%wavenumber)) deallocate(emis%wavenumber)
   if (allocated(emis%flag))       deallocate(emis%flag)
   if (allocated(emis%bands))      deallocate(emis%bands)
   if (allocated(emis%emissivity)) deallocate(emis%emissivity)

end subroutine deallocate_emis

end module camel_emissivity_m
