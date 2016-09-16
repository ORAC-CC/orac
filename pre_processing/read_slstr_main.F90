!-------------------------------------------------------------------------------
! Name: read_slstr_main.F90
!
! Purpose:
! Module for SLSTR I/O routines.
! To run the preprocessor with SLSTR use:
!    SLSTR as the sensor name (1st line of driver)
!    Any S-band radiance file (eg: S4_radiance_an.nc) as the data file (2nd line)
!    The corresponding tx file (eg: geodetic_tx.nc) as the geo file (3rd line)
!
! All M-band filenames to be read must have the same filename format!
!
! History:
! 2016/06/14, SP: Initial version.
! 2016/07/08, SP: Bug fixes.
! 2016/07/22, SP: Implement the second view (oblique).
!
! $Id$
!
! Bugs:
! Currently this will only work with the nadir view. Oblique view not supported.
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: read_slstr_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! geo_file       string  in   Full path to one slstr image file
! n_across_track lint    out  Number columns in the slstr image (usually constant)
! n_along_track  lint    out  Number lines   in the slstr image (usually constant)
! startx         lint    both First column desired by the caller
! endx           lint    both First line desired by the caller
! starty         lint    both Last column desired by the caller
! endy           lint    both Last line desired by the caller
! verbose        logical in   If true then print verbose information.
!
! Note: startx,endx,starty,endy currently ignored.
! It will always process the full scene. This will be fixed.
!-------------------------------------------------------------------------------
subroutine read_slstr_dimensions(img_file, n_across_track, n_along_track, &
                                 startx, endx, starty, endy, verbose)

   use iso_c_binding
   use preproc_constants_m
   use orac_ncdf_m

   implicit none

   character(path_length), intent(in)    :: img_file
   integer(lint),          intent(out)   :: n_across_track, n_along_track
   integer(lint),          intent(inout) :: startx, endx, starty, endy
   logical,                intent(in)    :: verbose

   integer :: fid,ierr

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_slstr_dimensions()'

   ! Open the file.
   call nc_open(fid, img_file, ierr)

   startx=1
   starty=1

   endy=nc_dim_length(fid,'rows',verbose)
   endx=nc_dim_length(fid,'columns',verbose)

   n_across_track = endx
   n_along_track = endy

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_slstr_dimensions()'

end subroutine read_slstr_dimensions


!-------------------------------------------------------------------------------
! Name: read_slstr_bin
!
! Purpose:
! To read the requested VIIRS data from HDF5-format files.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! infile              string  in   Full path to any M-band image file
! imager_geolocation  struct  both Members within are populated
! imager_measurements struct  both Members within are populated
! imager_angles       struct  both Members within are populated
! imager_time         struct  both Members within are populated
! channel_info        struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_slstr(infile,imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, verbose)

   use iso_c_binding
   use calender_m
   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m
   use netcdf
   use hdf5


   implicit none

   character(len=path_length),  intent(in)    :: infile
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: verbose

   integer                      :: i,j
   integer(c_int)               :: n_bands
   integer(c_int), allocatable  :: band_ids(:)
   integer(c_int), allocatable  :: band_units(:)
   integer                      :: startx, nx
   integer                      :: starty, ny
   integer(c_int)               :: line0, line1
   integer(c_int)               :: column0, column1
   integer                      :: matchcol

   character(len=path_length)   :: indir

   real(kind=sreal),allocatable :: txlats(:,:)
   real(kind=sreal),allocatable :: txlons(:,:)
   real(kind=sreal),allocatable :: oblats(:,:)
   real(kind=sreal),allocatable :: oblons(:,:)
   real(kind=sreal),allocatable :: interp(:,:,:)

   integer                      :: txnx,txny
   integer                      :: obnx,obny,obl_off

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_slstr()'

   ! Figure out the channels to process
   n_bands = channel_info%nchannels_total
   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr
   allocate(band_units(n_bands))

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   line0   = starty - 1
   line1   = starty - 1 + ny - 1
   column0 = startx - 1
   column1 = startx - 1 + nx - 1

   ! Sort out the start and end times, place into the time array
   call get_slstr_startend(imager_time,infile,ny)

   j=index(infile,'/',.true.)
   indir=infile(1:j)

   if (verbose) write(*,*)'Reading geoinformation data for SLSTR grids'

   ! Find size of tx grid. Should be 130,1200 but occasionally isn't
   call get_slstr_txgridsize(indir,txnx,txny)

   ! Then allocate arrays for tx data
   allocate(txlats(txnx,txny))
   allocate(txlons(txnx,txny))

   ! Find size of oblique view grid.
   call get_slstr_obgridsize(indir,obnx,obny)

   ! Then allocate arrays for oblique data
   if (imager_angles%nviews .eq. 2) then
      allocate(oblats(obnx,obny))
      allocate(oblons(obnx,obny))
   endif
   ! And one for the interpolation results
   allocate(interp(nx,ny,3))

   ! Read primary dem, lat and lon
   call read_slstr_demdata(indir,imager_geolocation%dem,nx,ny)
   call read_slstr_lldata(indir,imager_geolocation%latitude,nx,ny,.true.,'in')
   call read_slstr_lldata(indir,imager_geolocation%longitude,nx,ny,.false.,'in')

   ! Read reduced grid lat/lon
   call read_slstr_lldata(indir,txlats,txnx,txny,.true.,'tx')
   call read_slstr_lldata(indir,txlons,txnx,txny,.false.,'tx')

   ! Read oblique view lat/lon
   if (imager_angles%nviews .eq. 2) then
      call read_slstr_lldata(indir,oblats,obnx,obny,.true.,'io')
      call read_slstr_lldata(indir,oblons,obnx,obny,.false.,'io')
      ! Get alignment factor between oblique and nadir views
      call slstr_get_alignment(nx,ny,obnx,obny,imager_geolocation%longitude,oblons,obl_off)
   endif

   ! Get interpolation factors between reduced and TIR grid for each pixel
   call slstr_get_interp(imager_geolocation%longitude,txlons,txnx,txnx,nx,ny,interp)

   deallocate(txlats)
   deallocate(txlons)

   if (verbose) write(*,*)'Reading geometry data for SLSTR geo grid'

   ! Read satellite and solar angles for the nadir viewing geometry
   call read_slstr_satsol(indir,imager_angles,interp,txnx,txny,nx,ny,startx,1)
   if (imager_angles%nviews .eq. 2) then
      ! Read satellite and solar angles for the nadir viewing geometry
      call read_slstr_satsol(indir,imager_angles,interp,txnx,txny,nx,ny,startx,2)
   endif

   deallocate(interp)

   ! This bit reads all the data.
   do i=1,n_bands
      if (verbose) write(*,*)'Reading SLSTR data for band',band_ids(i)
      if (band_ids(i) .lt. 7) then
         call read_slstr_visdata(indir,band_ids(i),imager_measurements%data(:,:,i),&
            imager_angles,startx,starty,nx,ny,nx,ny,0,1)
      else if (band_ids(i) .le. 9) then
         call read_slstr_tirdata(indir,band_ids(i),imager_measurements%data(:,:,i),&
            startx,starty,nx,ny,nx,ny,1)
      else if (band_ids(i) .lt. 16) then
         call read_slstr_visdata(indir,band_ids(i),imager_measurements%data(:,:,i),&
            imager_angles,startx,starty,obnx,obny,nx,ny,obl_off-1,2)
      else if (band_ids(i) .le. 18) then
         call read_slstr_tirdata(indir,band_ids(i),imager_measurements%data(:,:,i),&
            startx,starty,obnx,obny,nx,ny,obl_off)
      else
         write(*,*)'Invalid band_id! Must be in range 1->18',band_ids(i)
         stop
      end if
   end do
   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_slstr()'

end subroutine read_slstr
