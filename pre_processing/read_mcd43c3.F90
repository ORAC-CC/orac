!-------------------------------------------------------------------------------
! Name: read_mcd43c3.F90
!
! Purpose:
! Open and read MODIS MCD43C3 16-day gridded surface albedo files
!
! Description and Algorithm details:
!
! Arguments:
! Name         Type     In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_file character    in      File name (and path) to read
! mcd          type(mcd43c) out     MCD output structure
! nbands       integer      in      Number of bands to read
! bands        integer(nbands) in   The band numbers of the required data
! read_ws      logical      in      If true white sky albedo will be read
! read_bs      logical      in      If true black sky albedo will be read
! QC           logical      in      If true QC and axillary data will be read
! stat         integer*4    out     Status value returned by the various hdf-eos
!                                   API routines. If an error occurs, it will be
!                                   returned with the value -1, otherwise
!                                   returned as 0.
!
! History:
! 2012/04/11, GT: Original
! 2012/04/23, GT: Replaced where statements for dealing with fill values in
!    input data (as they seem to cause seg-faults with ifort) with do if loops
! 2012/06/26, CP: commented out gdprojinfo as causes unknown crash.
! 2012/08/16, GT: Commented out check on projection type, as Carolines previous
!    change means the variable is not defined.
! 2012/08/20, MJ: changed read_mcd43c3 from function to subroutine in order to
!    iron out bugs and some slight change
! 2014/01/24, MJ: corrects length of "path_to_file"
! 2014/06/11, AP: use standard fill value rather than unique one
! 2014/08/05, AP: Moved channel dimension to end of array for efficiency.
!    Lat/lon grid now defined with start & division rather than array.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_mcd43c3(path_to_file, mcd, nbands, bands, read_ws, read_bs, &
                        read_QC, verbose, stat)

   use preproc_constants_m

   implicit none

   include "hdf.f90"

   ! Input variables
   character(len=*), intent(in) :: path_to_file
   integer,          intent(in) :: nbands
   integer,          intent(in) :: bands(:)
   logical,          intent(in) :: read_ws
   logical,          intent(in) :: read_bs
   logical,          intent(in) :: read_QC
   logical,          intent(in) :: verbose

   ! Output variables
   type(mcd43c3_t), intent(out) :: mcd
   integer*4,       intent(out) :: stat

   ! Local variables
   integer                    :: i, j, k
   integer*4                  :: fid, gid
   character(len=300)         :: gridlist
   integer*4                  :: gridlistlen
   character(len=MAX_NC_NAME) :: dataname

!  integer*4                  :: proj, zone, sphere
!  real, pointer              :: param(:)
   integer*4                  :: xdim, ydim
   real*8                     :: upleft(2), lowright(2)
   integer*4                  :: start(2), stride(2), edge(2)

   integer*2, allocatable     :: tmpdata(:,:)

   integer*2                  :: fill
   real*8                     :: offset, scale

   ! External functions (the hdf-eos library)
   integer*4, external        :: gdprojinfo
   integer*4, external        :: gdgridinfo
   integer*4, external        :: gdinqgrid
   integer*4, external        :: gdopen
   integer*4, external        :: gdattach
   integer*4, external        :: gdrdfld
   integer*4, external        :: gdgetfill
   integer*4, external        :: gddetach
   integer*4, external        :: gdclose

   integer*4, external        :: gdinqflds
   integer*4, external        :: gdinqdims
   integer*4, external        :: gdnentries
   integer*4, external        :: gdfldinfo

   ! The list of "bands" contained in the MCD43c3 files. Note that these
   ! actually appear as "Albedo_XXX_Band1" etc in the data file, where XXX
   ! is either BSA (Black-Sky Albedo) or WSA (White-Sky Albedo)
   character(len=10) :: BandList(10)
   BandList = (/ 'Band1    ', 'Band2    ', 'Band3    ', 'Band4    ', &
                 'Band5    ', 'Band6    ', 'Band7    ', 'vis      ', &
                 'nir      ', 'shortwave' /)

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_mcd43c3()'

   if (verbose) write(*,*) 'path_to_file: ', trim(path_to_file)
   if (verbose) write(*,*) 'nbands: ',       nbands
   if (verbose) write(*,*) 'bands: ',        bands

   ! Allocate and populate the band and bandid arrays in the output structure
   allocate(mcd%bands(nbands))
   allocate(mcd%bandids(nbands))
   mcd%nbands  = nbands
   mcd%bands   = bands
   mcd%bandids = BandList(bands)

   ! Rather bizarrely, the HDF-EOS API doesn't provide a way of reading grid
   ! field attributes (ie. HDF4 SDS attributes), although it does appear to
   ! provide a way of writing them (the one exception is the _FillValue, which
   ! has its own function). Unfortunately, the MODIS surface people appear to
   ! have ignored their own standards and defined the scale and and offset
   ! factors for the albedo data as HDF4 SDS attributes! We could either re-open
   ! the file with the standard HDF4 API, or define the scale and offsets here...
   ! which is messy, but easy...
   scale  = 0.001
   offset = 0.0

   ! First off, find out what grids are in the file - there should only be one.
   ! We'll need it's name to "attach" to it and extract the data
   if (verbose) write(*,*) 'Reading: ', trim(path_to_file)
   stat = 0
   stat = gdinqgrid(path_to_file, gridlist, gridlistlen)
   if (stat .ne. 1) then
      write(*,*) 'ERROR: read_mcd43c3(), problem with gdinqgrid(): ', stat
      stop error_stop_code
   end if

   if (verbose) write(*,*) 'gridlist = ', trim(gridlist), ' length = ', gridlistlen

   ! Open the datafile and get a file descriptor, and then attach to the grid
   ! (using the name returned above)
   fid = gdopen(path_to_file, DFACC_READ)

   gid = gdattach(fid, trim(gridlist))

   if (verbose) write(*,*) 'File and grid IDs are: ', fid, gid

   ! Extract the projection and grid information from the supplied grid. For some
   ! reason C. Poulsen cannot run with this file called it does not seem critical
   ! so I have commented it out.
!  stat = gdprojinfo(gid, proj, zone, sphere, param)
!  if (stat .ne. 0) then
!     write(*,*) 'ERROR: read_mcd43c3(), problem with gdprojinfo(): ', stat
!     stop error_stop_code
!  end if

   ! Check the grid-type and then use the grid-info to reproduce the lat-lon
   ! coordinates
!  if (proj.ne.0) then
!     write(*,*) 'ERROR: read_mcd43c3: only "Geographic" grid type supported'
!     stop error_stop_code
!  end if

   stat = gdgridinfo(gid, xdim, ydim, upleft, lowright)
   if (stat .ne. 0) then
      write(*,*) 'ERROR: read_mcd43c3(), problem with gdgridinfo(): ', stat
      stop error_stop_code
   end if

   ! Start populating the mcd data structure and allocating the arrays that we
   ! can
   mcd%nlon = xdim
   mcd%nlat = ydim
   mcd%lon_invdel = real(xdim, kind=8) / (lowright(1) - upleft(1))
   mcd%lat_invdel = real(ydim, kind=8) / (lowright(2) - upleft(2))
   mcd%lon0 = upleft(1) + 0.5/mcd%lon_invdel
   mcd%lat0 = upleft(2) + 0.5/mcd%lat_invdel

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
   if (read_QC) then
      allocate(mcd%quality(mcd%nlon,mcd%nlat))
      allocate(mcd%local_solar_noon(mcd%nlon,mcd%nlat))
      allocate(mcd%percent_inputs(mcd%nlon,mcd%nlat))
      allocate(mcd%percent_snow(mcd%nlon,mcd%nlat))

      stat = gdrdfld(gid, 'BRDF_Quality', start, stride, edge, &
                     mcd%quality)
      if (stat .ne. 0) then
         write(*,*) 'ERROR: read_mcd43c3(), Error reading BRDF_Quality: ', stat
         stop error_stop_code
      end if

      stat = gdrdfld(gid, 'Local_Solar_Noon', start, stride, edge, &
                     mcd%local_solar_noon)
      if (stat .ne. 0) then
         write(*,*) 'ERROR: read_mcd43c3(), Error reading Local_Solar_Noon: ', &
              stat
         stop error_stop_code
      end if

      stat = gdrdfld(gid, 'Percent_Inputs', start, stride, edge, &
                     mcd%percent_inputs)
      if (stat .ne. 0) then
         write(*,*) 'ERROR: read_mcd43c3(), Error reading Percent_Inputs: ', stat
         stop error_stop_code
      end if

      stat = gdrdfld(gid, 'Percent_Snow', start, stride, edge, &
                     mcd%percent_snow)
      if (stat .ne. 0) then
         write(*,*) 'ERROR: read_mcd43c3(), Error reading Percent_Snow: ', stat
         stop error_stop_code
      end if
   else
      allocate(mcd%quality(1,1))
      allocate(mcd%local_solar_noon(1,1))
      allocate(mcd%percent_snow(1,1))
      allocate(mcd%percent_inputs(1,1))

      mcd%quality(1,1) = 127
      mcd%local_solar_noon(1,1) = 127
      mcd%percent_snow(1,1) = 127
      mcd%percent_inputs(1,1) = 127
   end if

   allocate(tmpdata(mcd%nlon,mcd%nlat))

   ! Allocate the other variables, based on which bands have been requested
   if (read_ws) then
      allocate(mcd%WSA(mcd%nlon, mcd%nlat, nbands))
   else
      allocate(mcd%WSA(1,1,1))
      mcd%WSA(1,1,1) = -1.0
   end if
   if (read_bs) then
      allocate(mcd%BSA(mcd%nlon, mcd%nlat, nbands))
   else
      allocate(mcd%BSA(1,1,1))
      mcd%BSA(1,1,1) = -1.0
   end if

   fill = 32767

   do i = 1, nbands
      ! Read the white sky albedo
      if (read_ws) then
         dataname = 'Albedo_WSA_' // trim(BandList(bands(i)))

         if (verbose) write(*,*) 'Reading variable: ', trim(dataname)
         stat = gdrdfld(gid, trim(dataname), start, stride, edge, tmpdata)
         if (stat .ne. 0) then
            write(*,*) 'ERROR: read_mcd43c3(), gdrdfld(): ', stat
            stop error_stop_code
         end if

         ! Extract the fill value
!        stat = gdgetfill(gid, trim(dataname), fill)
!        if (stat .ne. 0) then
!           write(*,*) 'ERROR: read_mcd43c3(), gdgetfill(): ', stat
!           stop error_stop_code
!        end if

         ! Use the scale and offset values defined (read?) above to convert the
         ! integer data into sensible floating point numbers, and replace the
         ! fill value
         do j = 1, ydim
            do k = 1, xdim
               if (tmpdata(k,j) .eq. fill) then
                  mcd%WSA(k,j,i) = sreal_fill_value
               else
                  mcd%WSA(k,j,i) = tmpdata(k,j)*scale + offset
               end if
            end do
         end do

      end if

      ! Read the black sky albedo
      if (read_bs) then
         dataname = 'Albedo_BSA_' // trim(BandList(bands(i)))

         write(*,*) 'Reading parameter: ', dataname
         stat = gdrdfld(gid, dataname, start, stride, edge, tmpdata)
         if (stat .ne. 0) then
            write(*,*) 'ERROR: read_mcd43c3(), gdrdfld(): ', stat
            stop error_stop_code
         end if

         ! Extract the fill value
!        stat = gdgetfill(gid, trim(dataname), fill)
!        if (stat .ne. 0) then
!           write(*,*) 'ERROR: read_mcd43c3(), gdgetfill(): ', stat
!           stop error_stop_code
!        end if

         ! Use the scale and offset values defined (read?) above to convert the
         ! integer data into sensible floating point numbers, and replace the
         ! fill value
         do j = 1, ydim
            do k = 1, xdim
               if (tmpdata(k,j) .eq. fill) then
                  mcd%BSA(k,j,i) = sreal_fill_value
               else
                  mcd%BSA(k,j,i) = tmpdata(k,j)*scale + offset
               end if
            end do
         end do

      end if
   end do

   deallocate(tmpdata)

   ! Detach from the grid, and close the hdf file
   stat = gddetach(gid)

   stat = gdclose(fid)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_mcd43c3()'

end subroutine read_mcd43c3
