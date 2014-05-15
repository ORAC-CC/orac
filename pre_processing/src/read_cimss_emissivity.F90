module cimss_emissivity

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
! Local variables:
! Name Type Description
!
! History:
! 31/05/2012 Gareth Thomas. First version
! 02/07/2012 Gareth Thomas. Bug fix: Added if statements to check if the optional
!                           arguments have been supplied before testing their
!                           values
! 08/08/2012 Caroline Poulsen: initialised variables
! 25/02/2013 Gareth Thomas. Added an error check after openning the file. An
!                           error will result in the code stopping.
! 16/10/2013 Greg McGarragh: Changed the name of the fill value for the CIMSS
!                            emissivity product to the correct name: 'FillValue'
!                            to 'fill_value'.
! 05/11/2013 Greg McGarragh: It turns out some files use 'FillValue' and others
!                            use 'fill_value'.  So now we handle both cases.
!
! $Id$
!
! Bugs:
! None known
!
!could be a problem will deallocation of some arrays in this routine
!

function read_cimss_emissivity(path_to_file, emis, bands, flag, wavenumber, loc) &
     result (stat)

  use preproc_constants
  use emis_def
  use netcdf

  implicit none

  ! Input variables
  character(len=pathlength), intent(in)         :: path_to_file
  integer(kind=stint), intent(in), dimension(:) :: bands
  integer(kind=stint), optional, intent(in)     :: flag, wavenumber
  character(len=pathlength), optional, intent(in) :: loc
  ! Output variables
  type(emis_s), intent(out)                     :: emis
  integer(kind=stint)                           :: stat
  ! Local variables
  integer                                       :: fid, xid, yid, zid, did
  integer                                       :: nbands
  integer                                       :: xdim, ydim, zdim
  integer                                       :: nDim, nVar, nAtt, uDimID, ForNM
  integer(kind=2), allocatable, dimension(:)    :: tmpwavenumber
  real, allocatable, dimension(:,:)             :: fdata
  real                                          :: scale_factor, offset
  character(len=6)                              :: BandList(10)
  integer                                       :: i

  integer, dimension(2) :: start, cnt, stride
  integer(kind=1) :: gen_loc=1

  ! List of the band variable names in the data files
  BandList = (/ 'emis1 ', 'emis2 ', 'emis3 ', 'emis4 ', 'emis5 ', &
             &  'emis6 ', 'emis7 ', 'emis8 ', 'emis9 ', 'emis10' /)

  nbands = size(bands)
  ! Open NetCDF file
  stat = nf90_open(path_to_file, nf90_nowrite, fid)
  if (stat .ne. NF90_NOERR) then
     write(*,*) 'Error in read_cimss_emissivity! Cannot open file: ',trim(path_to_file)
     stop
  end if

  ! Extract information about the file
  stat = nf90_inquire(fid, nDim, nVar, nAtt, uDimID, ForNM)

  ! Now extract dimensions - should be three!
  if (nDim .gt. 3) write(*,*) &
       'read_cimss_emissitivty: Warning ',trim(path_to_file), &
       &' has more than three dimensions! nDim = ',nDim
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
        allocate(tmpwavenumber(zdim))
	emis%wavenumber=real_fill_value
	tmpwavenumber=real_fill_value
        stat = nf90_inq_varid(fid, 'wavenumber', did)
        stat = nf90_get_var(fid, did, tmpwavenumber)
        ! Now convert the temporary integer wavenumber variable into a correctly
        ! scaled real
        stat = nf90_get_att(fid, did, 'scale_factor', scale_factor)
        ! If we didn't sucessfully retrieve the scaling factor, assume there is none
        if (stat .ne. NF90_NOERR) scale_factor = 1.0
        ! Likewise the offset... 
        stat = nf90_get_att(fid, did, 'add_offset', offset)
        if (stat .ne. NF90_NOERR) offset = 0.0
        ! Apply and copy into the data array, but only those bands that have been
        ! requested
        emis%wavenumber(:) = real(tmpwavenumber(bands))*scale_factor + offset
        deallocate(tmpwavenumber)
     end if
  end if

  ! Likewise the flag 
  if (present(flag)) then
     if (flag .gt. 0) then

        stat = nf90_inq_varid(fid, 'emis_flag', did)
        allocate(emis%flag(xdim,ydim))
	emis%flag=real_fill_value
        ! Don't know why, but this call requires the start, count and stride
        ! arguments to be defined.
        start(:) = 1
        cnt(1) = 3600
        cnt(2) = 7200
        stride(:) = 1
        stat = nf90_get_var(fid, did, emis%flag)!, start, cnt, stride)
     endif
  end if
  
  ! Now define the emissivity array itself, based on the bands argument
  allocate(emis%emissivity(nbands,xdim,ydim))
  emis%emissivity=real_fill_value
  ! Extract the data for each of the requested bands
  do i=1,nbands
     write(*,*) 'read_cimss_emissivity: Extracting band ',BandList(bands(i)),bands(i)
     stat = nf90_inq_varid(fid, BandList(bands(i)), did)
     stat = read_cimss_2d(fid, did, emis%fill, emis%emissivity(i,:,:))
  end do

  ! We are now finished with the main data file
  stat = nf90_close(fid)

  ! If the loc string is non-null, then we attempt to read the data
  ! lat/lon grid from this file
  allocate(emis%lat(xdim))
  allocate(emis%lon(ydim))
  emis%lat=real_fill_value
  emis%lon=real_fill_value
  if (present(loc)) then
     if (len(trim(loc)) .gt. 1) then

        gen_loc = 0

        allocate(fdata(xdim,ydim))
	fdata=real_fill_value
        stat = nf90_open(loc, nf90_nowrite, fid)
        
        stat = nf90_inq_varid(fid, 'lat', did)
        ! stat = read_cimss_2d(fid, did, emis%fill, fdata)
        stat = nf90_get_var(fid, did, fdata)
        emis%lat = fdata(:,1)
        
        stat = nf90_inq_varid(fid, 'lon', did)
        ! stat = read_cimss_2d(fid, did, emis%fill, fdata)
        stat = nf90_get_var(fid, did, fdata)
        emis%lon = fdata(1,:)
        
        stat = nf90_close(fid)
        deallocate(fdata)
     end if
  end if
  ! If the loc variable is null, or hasn't been specified, 
  ! then we generate our own lat-lon grid.
  if (gen_loc .eq. 0) then
     do i=1,7200
        emis%lon(i) = -180.025 + real(i)*0.05
     end do
     do i=1,3600
        emis%lat(i) =   90.025 - real(i)*0.05
     end do
  end if
  
end function read_cimss_emissivity


function read_cimss_2d(fid, did, fill, data) result(stat)

  use netcdf

  implicit none

  integer, intent(in)                   :: fid, did
  real, intent(in)                      :: fill
  real, dimension(:,:), intent(inout)   :: data
  integer                               :: stat
  integer                               :: x, y
  integer(kind=2), allocatable, dimension(:,:)  :: idata
  real                                  :: scale_factor, offset, ifill
  
  ! Allocate the array for the integer data and extract the data from the file
  x = size(data, 1)
  y = size(data, 2)
  allocate(idata(x,y))

  stat = nf90_get_var(fid, did, idata)

  ! Now convert the temporary integer wavenumber variable into a correctly
  ! scaled real
  stat = nf90_get_att(fid, did, 'scale_factor', scale_factor)
  ! If we didn't sucessfully retrieve the scaling factor, assume there is none
  if (stat .ne. NF90_NOERR) scale_factor = 1.0
  ! Likewise the offset... 
  stat = nf90_get_att(fid, did, 'add_offset', offset)
  if (stat .ne. NF90_NOERR) offset = 0.0
  ! What is the fill value?  Some files use 'FillValue' and others use
  ! 'fill_value'
  stat = nf90_get_att(fid, did, 'FillValue', ifill)
  if (stat .ne. NF90_NOERR) then
     stat = nf90_get_att(fid, did, 'fill_value', ifill)
  endif

  ! Apply and copy into the data array
  data = real(idata)*scale_factor + offset
!  write(*,*) data(750:760,4900)

  ! Check for missing data in the unscaled array, and apply the correct fill
  ! value to the output array
  where(idata .eq. int(ifill)) data = fill
  
  deallocate(idata)

end function read_cimss_2d

end module cimss_emissivity
