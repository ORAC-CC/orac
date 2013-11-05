! Name: read_mcd43c3.f90
!
! Purpose:
! Open and read MODIS MCD43C3 16-day gridded surface albedo files
!
! Description and algorithm details
!
! Return value:
! Name  Type       Description
! stat  integer*4  Status value returned by the various hdf-eos API
!                  routines. If an error occurs, it will be returned
!                  with the value -1, otherwise returned as 0.
! Arguments:
! Name         Type     In/Out/Both Description
! path_to_file character    in      File name (and path) to read
! mcd          type(mcd43c) out     MCD output structure
! nbands       integer      in      Number of bands to read
! bands        integer(nbands) in   The band numbers of the required
!                                   data
! white_sky    integer      in      If not zero, white sky albedo
!                                   will be read
! black_sky    integer      in      If not zero, black sky albedo
!                                   will be read
! QC integer,  intent       in      If not zero, QC and auxillary data
!                                   will be read
!
! Local variables:
! Name Type Description
!
! History:
! 11 Apr 2012 Gareth Thomas: Original
! 23 Apr 2012 Gareth Thomas: Replaced where statements for dealing with
!                            fill values in input data (as they seem to
!                            cause seg-faults with ifort) with do if loops
! 26th June 2012 Caroline Poulsen commented out gdprojinfo as causes unknown crash.
! 16 Aug 2012 Gareth Thomas: Commented out check on projection type, as
!                            Carolines previous change means the variable is not
!                            defined.
! 2012/08/20 MJ changed read_mcd43c3 from function to subroutine in order to iron out bugs and some slight change
! Bugs:
!
!

subroutine read_mcd43c3(path_to_file, mcd, nbands, bands, white_sky, black_sky, QC,stat)
  use mcd43c_def
  use preproc_constants

  implicit none

  include "hdf.f90"
  include "dffunc.f90"

 
! Input variables
  character(len=300), intent(in)  :: path_to_file 
  integer(kind=stint), intent(in) :: nbands
  integer(kind=stint), intent(in) :: bands(:)
  integer(kind=sint), intent(in)  :: white_sky
  integer(kind=sint), intent(in)  :: black_sky
  integer(kind=sint), intent(in)  :: QC

! Output variables
  type(mcd43c), intent(out)    :: mcd
  integer*4                    :: stat      ! Function return

! Local variables
  integer                      :: i,j,k
  integer*4                    :: fid, gid
  character(len=300)           :: gridlist
  integer*4                    :: gridlistlen
  character(len=21)            :: dataname

  integer*4                    :: proj, zone, sphere
  real, pointer                :: param(:)
  integer*4                    :: xdim, ydim
  real                         :: xres, yres
  real*8                       :: upleft(2), lowright(2)
  integer*4                    :: start(2), stride(2), edge(2)
  
  integer*2, allocatable       :: tmpdata(:,:)

  integer*2                    :: fill
  real*8                       :: offset, scale

! External functions (the hdf-eos library)
  integer*4, external          :: gdprojinfo
  integer*4, external          :: gdgridinfo
  integer*4, external          :: gdinqgrid
  integer*4, external          :: gdopen
  integer*4, external          :: gdattach
  integer*4, external          :: gdrdfld
  integer*4, external          :: gdgetfill
  integer*4, external          :: gddetach
  integer*4, external          :: gdclose

  integer*4, external          :: gdinqflds
  integer*4, external          :: gdinqdims
  integer*4, external          :: gdnentries
  integer*4, external          :: gdfldinfo

  

  ! The list of "bands" contained in the MCD43c3 files. Note that these
  ! actually appear as "Albedo_XXX_Band1" etc in the data file, where XXX
  ! is either BSA (Black-Sky Albedo) or WSA (White-Sky Albedo)
  character(len=10)            :: BandList(10)
  BandList = (/ 'Band1    ', 'Band2    ', 'Band3    ', 'Band4    ', &
                'Band5    ', 'Band6    ', 'Band7    ', 'vis      ', &
                'nir      ', 'shortwave' /)

		write(*,*)'BandList',BandList
  ! Allocate and populate the band and bandid arrays in the output structure
  allocate(mcd%bands(nbands))
  allocate(mcd%bandids(nbands))
  mcd%nbands  = nbands
  mcd%bands   = bands
  mcd%bandids = BandList(bands)

  ! Rather bizzarely, the HDF-EOS API doesn't provide a way of reading
  ! grid field attributes (ie. HDF4 SDS attributes), although it does appear
  ! to provide a way of writing them (the one exception is the _FillValue,
  ! which has its own function). Unfortunately, the MODIS surface people
  ! appear to have ignored their own standards and defined the scale and
  ! and offset factors for the albedo data as HDF4 SDS attributes!
  ! We could either re-open the file with the standard HDF4 API, or define
  ! the scale and offsets here... which is messy, but easy...
  scale  = 0.001
  offset = 0.0

  ! First off, find out what grids are in the file - there should only be
  ! one. We'll need it's name to "attach" to it and extract the data
  write(*,*) 'Reading ',trim(path_to_file)
  stat = gdinqgrid(path_to_file, gridlist, gridlistlen)

  if (stat .ne. 1) write(*,*) 'Problem with number of grids: ',stat
  write(*,*) 'gridlist = ',trim(gridlist),' length = ',gridlistlen

  ! Open the datafile and get a file descriptor, and then attach to the 
  ! grid (using the name returned above)
  !MJ ORG fid = gdopen(path_to_file, 1)
  fid = gdopen(path_to_file,DFACC_READ)

  gid = gdattach(fid, trim(gridlist))

  write(*,*) 'File and grid IDs are: ',fid, gid

  ! Extract the projection and grid information from the supplied grid
  ! for some reason C. Poulsen cannot run with this file called it does
  ! not seem critical so I have commented it out.
  !stat = gdprojinfo(gid, proj, zone, sphere, param)

  !write(*,*) 'proj, zone, sphere, param',proj, zone, sphere, param
  stat = gdgridinfo(gid, xdim, ydim, upleft, lowright)

  
  ! Check the grid-type and then use the grid-info to reproduce the
  ! lat-lon coordinates
  !if (proj.ne.0) write(*,*) 'Read_MCD43C3: Warning only "Geographic" grid type supported'
  ! Start populating the mcd data structure and allocating the arrays that
  ! we can
  mcd%nlon = xdim
  mcd%nlat = ydim
    
  allocate(mcd%lon(xdim))
  allocate(mcd%lat(ydim))
    
  ! Now populate the newly allocated lat-lon coordinates....
  ! There might be a more efficient (or at least tidier) way to do the
  ! following with intrinsic functions...
  xres = (lowright(1) - upleft(1)) / real(xdim)
  yres = (upleft(2) - lowright(2)) / real(ydim)
  do i = 1,xdim
     mcd%lon(i) = upleft(1) + real(i)*xres - xres/2.0
  end do
  do i = 1,ydim
     mcd%lat(ydim-i+1) = lowright(2) + real(i)*yres - yres/2.0
  end do

  ! Allocate the other variables, based on which bands have been requested
  if (white_sky .ne. 0) then
     allocate(mcd%WSA(nbands, mcd%nlon, mcd%nlat))
  else
     allocate(mcd%WSA(1,1,1))
     mcd%WSA(1,1,1) = -1.0
  end if
  if (black_sky .ne. 0) then
     allocate(mcd%BSA(nbands, mcd%nlon, mcd%nlat))
  else
     allocate(mcd%BSA(1,1,1))
     mcd%BSA(1,1,1) = -1.0
  end if

  ! Set-up the parameters which control the data reading
  ! start : starting x,y coordinates within the data arrays (starting at 0)
  ! stride: x,y stride (spacing)
  ! edge  : maximum x,y coordinates to read
  start(:)  = 0
  stride(:) = 1
  edge(1) = xdim
  edge(2) = ydim

  ! If it's required, the QC data is read straight off (no need to loop
  ! over the bands like with the data itself)
  if (QC .ne. 0) then
     allocate(mcd%quality(mcd%nlon,mcd%nlat))
     allocate(mcd%local_solar_noon(mcd%nlon,mcd%nlat))
     allocate(mcd%percent_snow(mcd%nlon,mcd%nlat))
     allocate(mcd%percent_inputs(mcd%nlon,mcd%nlat))
     stat = gdrdfld(gid, 'BRDF_Quality', start, stride, edge, &
          mcd%quality)
     if (stat .ne. 0) write(*,*) 'Error reading BRDF_Quality', stat

     stat = gdrdfld(gid, 'Local_Solar_Noon', start, stride, edge, &
          mcd%local_solar_noon)
     if (stat .ne. 0) write(*,*) 'Error reading Local_Solar_Noon', stat

     stat = gdrdfld(gid, 'Percent_Snow', start, stride, edge,     &
          mcd%percent_snow)
     if (stat .ne. 0) write(*,*) 'Error reading Percent_Snow', stat

     stat = gdrdfld(gid, 'Percent_Inputs', start, stride, edge,   &
          mcd%percent_inputs)
     if (stat .ne. 0) write(*,*) 'Error reading Percent_Inputs', stat
  else
     allocate(mcd%quality(1,1))
     allocate(mcd%local_solar_noon(1,1))
     allocate(mcd%percent_snow(1,1))
     allocate(mcd%percent_inputs(1,1))
     mcd%quality(1,1) = 127! 255
     mcd%local_solar_noon(1,1) = 127 !255
     mcd%percent_snow(1,1) = 127 !255
     mcd%percent_inputs(1,1) = 127 !255
  end if

  allocate(tmpdata(mcd%nlon,mcd%nlat))

  fill = 32767

  do i = 1,nbands
     ! Read the white sky albedo
     if (white_sky .ne. 0) then
        dataname = 'Albedo_WSA_' // trim(BandList(bands(i)))
        write(*,*) 'Reading variable ', trim(dataname)
        stat = gdrdfld(gid, trim(dataname), start, stride, edge, tmpdata)

        ! Extract the fill value
        ! stat = gdgetfill(gid, trim(dataname), fill)
        ! Use the scale and offset values defined (read?) above to convert
        ! the integer data into sensible floating point numbers, and
        ! replace the fill value
        do j = 1,ydim
           do k = 1,xdim
              if (tmpdata(k,j) .eq. fill) then
                 mcd%WSA(i,k,j) = mcd%fill
              else
                 mcd%WSA(i,k,j) = tmpdata(k,j)*scale + offset

              end if
           end do
        end do

        ! Note, the following works with gfortran, but causes a segmentation
        ! fault if used with ifort (v11)
        !where (tmpdata .eq. fill)
        !   mcd%WSA(i,:,:) = mcd%fill
        !elsewhere
        !   mcd%WSA(i,:,:) = real(tmpdata)*scale + offset
        !endwhere
     end if

     ! Read the black sky albedo
     if (black_sky .ne. 0) then
        dataname = 'Albedo_BSA_' // trim(BandList(bands(i)))
        write(*,*) 'Reading variable ', dataname
        stat = gdrdfld(gid, dataname, start, stride, edge, tmpdata)
        ! Extract the fill value
        stat = gdgetfill(gid, trim(dataname), fill)
        ! Use the scale and offset values defined (read?) above to convert
        ! the integer data into sensible floating point numbers
        do j = 1,ydim
           do k = 1,xdim
              if (tmpdata(k,j) .eq. fill) then
                 mcd%BSA(i,k,j) = mcd%fill
              else
                 mcd%BSA(i,k,j) = tmpdata(k,j)*scale + offset
              end if
           end do
        end do
        !where (tmpdata .eq. fill)
        !   mcd%BSA(i,:,:) = mcd%fill
        !elsewhere
        !   mcd%BSA(i,:,:) = real(tmpdata)*scale + offset
        !endwhere
      end if
  end do

  ! Detach from the grid, and close the hdf file
  stat = gddetach(gid)

  stat = gdclose(fid)

  !write(*,*) 'end reading read_mcd43c3'

end subroutine read_mcd43c3



