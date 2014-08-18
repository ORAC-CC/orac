module cimss_emissivity

   use preproc_constants, only : sreal_fill_value

   implicit none

   type emis_s
      !    Data dimensions
      integer(4)                                :: nlon, nlat, nbands
      !    Quality information
      integer(2), allocatable, dimension(:,:)   :: flag
      !    Bands
      integer,    allocatable, dimension(:)     :: wavenumber,bands
      !    Data
      real(8)                                   :: lon0, lon_invdel, lon_del
      real(8)                                   :: lat0, lat_invdel, lat_del
      real,       allocatable, dimension(:,:,:) :: emissivity
      !    Missing data value
      real                                      :: fill=sreal_fill_value
   end type emis_s

contains

! Name: read_cimss_emissivity.F90
!
! Purpose:
! Reads land surface emissivity from the CIMSS database (produced by Eva
! Borbas and Suzanne Seemann in Wisconsin)
!
! Description and algorithm details
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
!
! Return value:
! Name Type    Description
! stat integer Status value for the routine
!
! History:
! 31/05/2012, GT: First version
! 02/07/2012, GT: Bug fix: Added if statements to check if the optional
!   arguments have been supplied before testing their values
! 08/08/2012, CP: initialised variables
! 25/02/2013, GT: Added an error check after opening the file. An
!   error will result in the code stopping.
! 16/10/2013, GM: Changed the name of the fill value for the CIMSS emissivity
!   product to the correct name: 'FillValue' to 'fill_value'.
! 05/11/2013, GM: It turns out some files use 'FillValue' and others
!   use 'fill_value'.  So now we handle both cases.
! 12/05/2014, AP: Uses orac_ncdf to avoid duplication of NCDF routines. Made into
!   a module.
! 04/08/2014, AP: Put bands as last index of emissivity array. Replaced lat|lon
!   fields with grid definitions to be compatible with new interpolation routine.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

function read_cimss_emissivity(path_to_file, emis, bands, verbose, flag, &
     wavenumber) result (stat)

   use orac_ncdf
   use preproc_constants

   implicit none

   ! Input variables
   character(len=pathlength), intent(in)               :: path_to_file
   integer(kind=sint),        intent(in), dimension(:) :: bands
   logical,                   intent(in)               :: verbose
   integer(kind=sint),        intent(in), optional     :: flag, wavenumber


   ! Output variables
   type(emis_s),              intent(out)              :: emis
   integer(kind=sint)                                  :: stat

   ! Local variables
   integer          :: fid, xid, yid, zid
   integer          :: nbands
   integer          :: xdim, ydim, zdim
   integer          :: nDim, nVar, nAtt
   integer          :: uDimID, ForNM
   character(len=6) :: BandList(10)
   integer          :: i
   integer(kind=1)  :: gen_loc=1

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_cimss_emissivity()'

   if (verbose) write(*,*) 'path_to_file: ', trim(path_to_file)
   if (verbose) write(*,*) 'bands: ',        bands

   ! List of the band variable names in the data files
   BandList = (/ 'emis1 ', 'emis2 ', 'emis3 ', 'emis4 ', 'emis5 ', &
                 'emis6 ', 'emis7 ', 'emis8 ', 'emis9 ', 'emis10' /)

   nbands = size(bands)
   ! Open NetCDF file
   call nc_open(fid, path_to_file)

   ! Extract information about the file
   stat = nf90_inquire(fid, nDim, nVar, nAtt, uDimID, ForNM)

   ! Now extract dimensions - should be three!
   if (nDim .gt. 3) then
      write(*,*) 'ERROR: read_cimss_emissitivty(): File has more than three '//&
               & 'dimensions, filename: ', trim(path_to_file), ' nDim: ', nDim
      stop error_stop_code
   endif
   ! Three dimensions should be:
   ! xdim = latitude (strange!)
   ! ydim = longitude
   ! zdim = wavelength = number of band variables
   stat = nf90_inq_dimid(fid, 'xdim', xid)
   stat = nf90_inq_dimid(fid, 'ydim', yid)
   stat = nf90_inq_dimid(fid, 'zdim', zid)
   ! Extract the array dimensions
   stat = nf90_inquire_dimension(fid, xid, len=xdim)
   stat = nf90_inquire_dimension(fid, yid, len=ydim)
   stat = nf90_inquire_dimension(fid, zid, len=zdim)
   ! Begin to populate the emis structure
   emis%nlat   = xdim
   emis%nlon   = ydim
   emis%nbands = nbands
   ! If required, read the wavenumber data from the file
   if (present(wavenumber)) then
      if (wavenumber .gt. 0) then
         allocate(emis%wavenumber(nbands))
         call nc_read_array(fid,'wavenumber',emis%wavenumber,verbose)
      end if
   end if

   ! Likewise the flag
   if (present(flag)) then
      if (flag .gt. 0) then
         allocate(emis%flag(xdim,ydim))
         call nc_read_array(fid,'emis_flag',emis%flag,verbose)
      end if
   end if

   ! Now define the emissivity array itself, based on the bands argument
   allocate(emis%emissivity(xdim,ydim,nbands))
   ! Extract the data for each of the requested bands
   do i=1,nbands
      if (verbose) write(*,*) 'Reading band:  ', BandList(bands(i))
      call nc_read_array(fid,BandList(bands(i)),emis%emissivity(:,:,i),verbose)
   end do

   ! We are now finished with the main data file
   stat = nf90_close(fid)

   ! Commented out read/generation of lat/lon arrays. As the grid is regular,
   ! simply output its starting point and the inverse of the spacing
   emis%lon0 = -179.975
   emis%lon_del = 0.05
   emis%lon_invdel = 20.
   emis%lat0 = 89.975
   emis%lat_del = -0.05
   emis%lat_invdel = -20.

   ! If the loc string is non-null, then we attempt to read the data
   ! lat/lon grid from this file
   !allocate(emis%lat(xdim))
   !allocate(emis%lon(ydim))
   !if (present(loc)) then
   !   if (len(trim(loc)) .gt. 1) then
   !      gen_loc = 0
   !      call nc_read_array(fid,'lat',emis%lat,verbose)
   !      call nc_read_array(fid,'lon',emis%lon,verbose)
   !   end if
   !end if
   ! If the loc variable is null, or hasn't been specified,
   ! then we generate our own lat-lon grid.
   !if (gen_loc .eq. 1) then
   !   do i=1,7200
   !      emis%lon(i) = -180.025 + real(i)*0.05
   !   end do
   !   do i=1,3600
   !      emis%lat(i) =   90.025 - real(i)*0.05
   !   end do
   !end if

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_cimss_emissivity()'

end function read_cimss_emissivity

subroutine deallocate_emis(emis)

   implicit none

   type(emis_s), intent(inout) :: emis

   if (allocated(emis%wavenumber)) deallocate(emis%wavenumber)
   if (allocated(emis%flag))       deallocate(emis%flag)
   if (allocated(emis%bands))      deallocate(emis%bands)
   if (allocated(emis%emissivity)) deallocate(emis%emissivity)

end subroutine deallocate_emis

end module cimss_emissivity
