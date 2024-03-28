!-------------------------------------------------------------------------------
! Name: read_cimss_emissivity.F90
!
! Purpose:
! Reads land surface emissivity from the CIMSS database (produced by Eva
! Borbas and Suzanne Seemann in Wisconsin)
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
!                                      to the numbering used by CIMSS
! flag           integer       In      OPTIONAL argument. If > 0, the "flag"
!                                      data will be read from the file
! wavenumber     integer       In      OPTIONAL argument. If > 0, the band
!                                      wavenumbers will be read from the file
! loc             char         In      OPTIONAL argument. If this is set to a
!                                      non-null string, it is assumed to define
!                                      the path to the lat-lon grid data file
!                                      for the CIMSS data (otherwise the code
!                                      generates its own grid).
! stat           integer       Out     (Return value) Status for the routine
!
! History:
! 2012/05/31, GT: First version
! 2012/07/02, GT: Bug fix: Added if statements to check if the optional
!    arguments have been supplied before testing their values
! 2012/08/08, CP: initialised variables
! 2013/02/25, GT: Added an error check after opening the file. An
!    error will result in the code stopping.
! 2013/10/16, GM: Changed the name of the fill value for the CIMSS emissivity
!    product to the correct name: 'FillValue' to 'fill_value'.
! 2013/11/05, GM: It turns out some files use 'FillValue' and others
!    use 'fill_value'.  So now we handle both cases.
! 2014/05/12, AP: Uses orac_ncdf to avoid duplication of NCDF routines. Made
!    into a module.
! 2014/08/04, AP: Put bands as last index of emissivity array. Replaced lat|lon
!    fields with grid definitions to be compatible with new interpolation
!    routine.
! 2014/10/15, GM: Changes related to supporting an arbitrary set of LW channels.
! 2015/03/04, GM: Changes related to supporting channels in arbitrary order.
! 2015/07/03, OS: added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module cimss_emissivity_m

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

function read_cimss_emissivity(path_to_file, emis, wavelengths, verbose, flag, &
   wavenumber) result (stat)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   ! Input variables
   character(len=*), intent(in)               :: path_to_file
   real,                       intent(in), dimension(:) :: wavelengths
   logical,                    intent(in)               :: verbose
   integer,                    intent(in), optional     :: flag
   integer,                    intent(in), optional     :: wavenumber

   ! Output variables
   type(emis_t),               intent(out)              :: emis
   integer(kind=sint)                                   :: stat

   ! Local variables
   integer            :: i, j
   real               :: a
   integer, parameter :: nBands = 10
   character(len=6)   :: bandList(nBands)
   integer            :: n_wavelengths
   integer            :: fid
   integer            :: xdim, ydim, zdim
   integer            :: nDim

   type cache_element_t
      real, pointer, dimension(:,:) :: a
   end type cache_element_t

   type(cache_element_t), allocatable, dimension(:) :: cache

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_cimss_emissivity()'

   if (verbose) write(*,*) 'path_to_file: ', trim(path_to_file)
   if (verbose) write(*,*) 'wavelengths: ',  wavelengths

   ! List of the band variable names in the data files
   bandList = (/ 'emis1 ', 'emis2 ', 'emis3 ', 'emis4 ', 'emis5 ', &
                 'emis6 ', 'emis7 ', 'emis8 ', 'emis9 ', 'emis10' /)

   n_wavelengths = size(wavelengths)

   ! Open NetCDF file
   call ncdf_open(fid, path_to_file, 'read_cimss_emissivity()')

   ! Extract information about the file
   stat = nf90_inquire(fid, nDim)

   ! Now extract dimensions - should be three!
   if (nDim .gt. 3) then
      write(*,*) 'ERROR: read_cimss_emissivity(): File has more than ' // &
           'three dimensions, filename: ', trim(path_to_file), ' nDim: ', nDim
      stop error_stop_code
   end if

   ! Three dimensions should be:
   ! xdim = latitude (strange!)
   ! ydim = longitude
   ! zdim = wavelength = number of band variables
   xdim = ncdf_dim_length(fid, 'xdim', 'read_cimss_emissivity()')
   ydim = ncdf_dim_length(fid, 'ydim', 'read_cimss_emissivity()')
   zdim = ncdf_dim_length(fid, 'zdim', 'read_cimss_emissivity()')

   ! Begin to populate the emis structure
   emis%nlat   = xdim
   emis%nlon   = ydim
   emis%nbands = n_wavelengths

   ! If required, read the wavenumber data from the file
!  if (present(wavenumber)) then
!     if (wavenumber .gt. 0) then
         allocate(emis%wavenumber(nBands))
         call ncdf_read_array(fid, 'wavenumber', emis%wavenumber)
!     end if
!  end if

   ! Likewise the flag
   if (present(flag)) then
      if (flag .gt. 0) then
         allocate(emis%flag(xdim,ydim))
         call ncdf_read_array(fid, 'emis_flag', emis%flag)
      end if
   end if

   ! Now define the emissivity array itself
   allocate(cache(nBands))
   do i = 1, nBands
      nullify(cache(i)%a)
   end do

   allocate(emis%emissivity(xdim,ydim,n_wavelengths))

   ! Extract the data for each of the requested bands
   do i = 1, n_wavelengths
      if (wavelengths(i) .le. 1.e4 / emis%wavenumber(1)) then
         if (.not. associated(cache(1)%a)) then
            allocate(cache(1)%a(xdim,ydim))
            call ncdf_read_array(fid, bandList(1), cache(1)%a)
         end if
         emis%emissivity(:,:,i) = cache(1)%a
      else if (wavelengths(i) .ge. 1.e4 / emis%wavenumber(nBands)) then
         if (.not. associated(cache(nBands)%a)) then
            allocate(cache(nBands)%a(xdim,ydim))
            call ncdf_read_array(fid, bandList(nBands), cache(nBands)%a)
         end if
         emis%emissivity(:,:,i) = cache(nBands)%a
      else
         j = 1
         do while (.true.)
            if (wavelengths(i) .ge. 1.e4 / emis%wavenumber(j) .and. &
                wavelengths(i) .lt. 1.e4 / emis%wavenumber(j + 1)) exit
            j = j + 1
         end do

         if (.not. associated(cache(j)%a)) then
            allocate(cache(j)%a(xdim,ydim))
            call ncdf_read_array(fid, bandList(j), cache(j)%a)
         end if

         if (.not. associated(cache(j+1)%a)) then
            allocate(cache(j+1)%a(xdim,ydim))
            call ncdf_read_array(fid, bandList(j+1), cache(j+1)%a)
         end if

         a = (wavelengths(i) - 1.e4 / emis%wavenumber(j)) / &
             (1.e4 / emis%wavenumber(j + 1) - 1.e4 / emis%wavenumber(j))
if (USE_NEAREST_CHANNEL) then
         if (a < .5) then
            emis%emissivity(:,:,i) = cache(j)%a
         else
            emis%emissivity(:,:,i) = cache(j + 1)%a
         end if
else
         emis%emissivity(:,:,i) = (1. - a) * cache(j)%a + a * cache(j + 1)%a
end if
      end if
   end do

   do i = 1, nBands
      if (associated(cache(i)%a)) deallocate(cache(i)%a)
   end do
   deallocate(cache)

   ! We are now finished with the main data file
   call ncdf_close(fid, 'read_cimss_emissivity()')

   ! Commented out read/generation of lat/lon arrays. As the grid is regular,
   ! simply output its starting point and the inverse of the spacing
   emis%lon0 = -179.975
   emis%lon_del = 0.05
   emis%lon_invdel = 20.
   emis%lat0 = 89.975
   emis%lat_del = -0.05
   emis%lat_invdel = -20.

!   If the loc string is non-null, then we attempt to read the data
!   lat/lon grid from this file
!  allocate(emis%lat(xdim))
!  allocate(emis%lon(ydim))
!  if (present(loc)) then
!     if (len_trim(loc) .gt. 1) then
!        gen_loc = 0
!        call ncdf_read_array(fid, 'lat', emis%lat)
!        call ncdf_read_array(fid, 'lon', emis%lon)
!     end if
!  end if
!   If the loc variable is null, or hasn't been specified,
!   then we generate our own lat-lon grid.
!  if (gen_loc .eq. 1) then
!     do i = 1, 7200
!        emis%lon(i) = -180.025 + real(i)*0.05
!     end do
!     do i = 1, 3600
!        emis%lat(i) =   90.025 - real(i)*0.05
!     end do
!  end if

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_cimss_emissivity()'

end function read_cimss_emissivity

subroutine deallocate_emis(emis)

   implicit none

   type(emis_t), intent(inout) :: emis

   if (allocated(emis%wavenumber)) deallocate(emis%wavenumber)
   if (allocated(emis%flag))       deallocate(emis%flag)
   if (allocated(emis%bands))      deallocate(emis%bands)
   if (allocated(emis%emissivity)) deallocate(emis%emissivity)

end subroutine deallocate_emis

end module cimss_emissivity_m
