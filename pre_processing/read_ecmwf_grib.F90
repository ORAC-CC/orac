!-------------------------------------------------------------------------------
! Name: read_ecmwf_grib.F90
!
! Purpose:
! Read ECMWF data from a GRIB file, having interpolated it onto the
! preprocessing grid. Replaces read_ecmwf.F90.
!
! Description and Algorithm details:
! The ECMWF EMOS library is used to perform the interpolation from the native
! grid of the input file to the desired preprocessor grid. That library can
! only produce regular or semi-regular grids and they must contain -180W and
! 0N. Also, the files report quantity values *at that point* rather than an
! area average as considered by the preprocessor grid.
!
! Hence, the definition of the preprocessor grid means we desire the ECMWF value
! at the centre of a grid cell but the interpolation software will only report
! at cell edges. To get around this, we request the ECMWF data at twice the
! desired resolution and then only use every other line of lat/lon. The final
! values are identical to what would have resulted from requesting the correct
! resolution.
!
! There is also a slight workaround involving the production of a scratch
! file. Though the EMOS library will output an unpacked array, that does not
! include the header information required to identify the field. By copying
! the interpolated field to a scratch file retains that.
!
! 1) Open file. Create scratch file.
! 2) Set output grid to limits and spacing of preprocessing grid.
! 3) Loop over fields in GRIB file.
!    a) Read data field.
!    b) Interpolate it with INTF.
!    c) Write new GRIB field to scratch file.
!    d) Read scratch file with GRIB API routines. Check for reduced Gauss grid.
!    e) Select correct output array from parameter #.
!    f) Write data into preproc structures.
! 4) Close files.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_file     string in   Full path to a ECMWF GRIB file to read.
! preproc_dims   struct out  Summary of preprocessing grid definitions
! preproc_geoloc struct out  Summary of lat/lon values
! preproc_prtm   struct out  Summary of profiles and surface fields
! verbose        logic  in   T: Print min/max of each field; F: Don't.
!
! History:
! 2014/05/07, AP: First version.
! 2014/08/20, OS: Adaptations made for cray-fortran compiler specific issues
! 2014/12/04, OS: include job ID in scratch file name - only for wrapper
! 2015/12/17, OS: Added some wrapper specific variables.
! 2016/01/13, GM: Allocate old_data and new_data on the heap explicitly rather
!    than declaring them automatic. In this case, as automatic, gfortran, and
!    maybe other compilers, were allocating the arrays on the stack overflowing
!    the stack. These arrays are big enough that they should be explicitly
!    allocated on the heap anyway.
! 2016/01/13, GM: Eliminate the need for a scratch file by reading the grib
!    output from INTF() directly into the grib_api library code from memory with
!    grib_new_from_message().
! 2016/12/07, GT: Replaced call to INTF with INTF2, as INTF requires the GRIBEX
!    subroutine from EMOSLIB when interpolating from or to a GRIB field, which
!    is no-longer supported (or it appears functional) from v4.1.1 of the EMOSLIB
! 2024/07/01, DH: Change indexing to use preproc_dims for all dimensions
!
! Bugs:
! - If you're having problems with INTF, set the environment variable JDCNDBG=1
! for additional debugging output.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_grib(ecmwf_file, preproc_dims, preproc_geoloc, &
     preproc_prtm, verbose)

   use grib_api
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),       intent(in)    :: ecmwf_file
   type(preproc_dims_t),   intent(in)    :: preproc_dims
   type(preproc_geoloc_t), intent(in)    :: preproc_geoloc
   type(preproc_prtm_t),   intent(inout) :: preproc_prtm
   logical,                intent(in)    :: verbose

   integer(lint), parameter                 :: BUFFER = 3000000
   integer(lint), external                  :: INTIN, INTOUT, INTF2
   integer(lint)                            :: fu, stat, nbytes
!  integer(lint)                            :: in_words, out_words
   integer(lint)                            :: out_bytes, out_words
   integer(lint), allocatable, dimension(:) :: in_data, out_data
   integer(lint)                            :: iblank(4)
!  real(dreal)                              :: zni(1), zno(1), grid(2), area(4)
   real(dreal)                              :: grid(2), area(4)
   character(len=20)                        :: charv(1)

   integer(lint)                            :: gid, level, param
   integer(lint)                            :: n, ni, nj, i,j, plpresent
   real(sreal), dimension(:),   allocatable :: pl, val
   real(sreal), dimension(:,:), pointer     :: array

   ! open the ECMWF file
   call PBOPEN(fu, ecmwf_file, 'r', stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to read file.')

   ! select appropriate grid definition
   charv(1) = 'grib'
   if (INTIN('form', iblank, grid, charv) .ne. 0) &
        call h_e_e('grib', 'INTIN form failed.')
   if (INTOUT('form', iblank, grid, charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT form failed.')

   ! input details of new grid (see note in header)
   grid(1) = 0.5 / preproc_dims%dellon
   grid(2) = 0.5 / preproc_dims%dellat
   if (INTOUT('grid', iblank, grid, charv) .ne. 0) &
      call h_e_e('grib', 'INTOUT grid failed.')
   area(1) = preproc_geoloc%latitude(preproc_dims%ydim) + 0.01*grid(2)
   area(2) = preproc_geoloc%longitude(1) + 0.01*grid(1)
   area(3) = preproc_geoloc%latitude(1) + 0.01*grid(2)
   area(4) = preproc_geoloc%longitude(preproc_dims%xdim) + 0.01*grid(1)
   if (INTOUT('area', iblank, area, charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT area failed.')

   allocate(in_data(BUFFER))
   allocate(out_data(BUFFER))

   ! interpolate ECMWF products to preproc grid
   do
      ! read GRIB data field
      call PBGRIB(fu, in_data, BUFFER*lint, nbytes, stat)
      if (stat .eq. -1) exit
      if (stat .ne. 0) call h_e_e('grib', 'Failure to read product.')

      ! interpolate GRIB field (into another GRIB field)
      ! in_words = nbytes / lint
      ! out_words = BUFFER
      out_bytes = BUFFER * lint
      !if (INTF(in_data, in_words, zni, out_data, out_words, zno) .ne. 0) &
      !     call h_e_e('grib', &
      !       'INTF failed. Check if 1/dellon 1/dellat are muliples of 0.001.')
      if (INTF2(in_data, nbytes, out_data, out_bytes) .ne. 0) &
         call h_e_e('grib', 'INTF2 failed.')
      out_words = out_bytes/lint
      ! load grib data into grib_api
      call grib_new_from_message(gid, out_data(1:out_words), stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting GRIB_ID.')

      call grib_get(gid, 'parameter', param, stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting parameter #.')
      call grib_get(gid, 'level', level, stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
      call grib_get(gid, 'Nj', nj, stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting nj.')

      ! check if this is a reduced Gaussian grid
      call grib_get(gid, 'PLPresent', plpresent, stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting PLPresent.')
      if (plpresent .eq. 1) then
         ! determine total number of points from PL array
         allocate(pl(nj))
         call grib_get(gid, 'pl', pl, stat)
         if (stat .ne. 0) call h_e_e('grib', 'Error getting PL.')
         n = sum(pl)
         deallocate(pl)
      else
         ! regular grid
         call grib_get(gid, 'Ni', ni, stat)
         if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
         n = ni*nj
      end if
      if (.not.allocated(val)) allocate(val(n))

      call grib_get(gid, 'values', val, stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error reading data.')
      call grib_release(gid, stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error releasing GRIB_ID.')

      ! select correct output array
      select case (param)
      case(130)
         array => preproc_prtm%temperature( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,level)
      case(133)
         array => preproc_prtm%spec_hum( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,level)
      case(203)
         array => preproc_prtm%ozone( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,level)
      case(129)
         array => preproc_prtm%geopot( &
                 1:preproc_dims%xdim, & 
                 1:preproc_dims%ydim)
      case(152)
         array => preproc_prtm%lnsp( &
                 1:preproc_dims%xdim, & 
                 1:preproc_dims%ydim)
      case(31)
         array => preproc_prtm%sea_ice_cover(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(32)
         array => preproc_prtm%snow_albedo(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(34)
         array => preproc_prtm%sst(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(137)
         array => preproc_prtm%totcolwv(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(141)
         array => preproc_prtm%snow_depth(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(165)
         array => preproc_prtm%u10(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(166)
         array => preproc_prtm%v10(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(167)
         array => preproc_prtm%temp2(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(172)
         array => preproc_prtm%land_sea_mask(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(235)
         array => preproc_prtm%skin_temp(  &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case default
         cycle
      end select

      ! copy data into preprocessing grid
      ! a) we're inverting the y axis as ECMWF write 90->-90
      ! b) we're only taking every other point to read cell centres
      ! c) there will be an odd # of lats as it contains 0N but an even # of
      !    lons as 180 wraps to -180
      do j = 1, nj, 2
         do i = 1, ni, 2
            array(1+i/2,1+(nj-j)/2) = val(i+(j-1)*ni)
         end do
      end do
      where (array .eq. 9999) array = sreal_fill_value
      if (verbose) print*, param, ') Min: ', minval(array), &
              ', Max: ', maxval(array)
   end do

   deallocate(val)

   deallocate(in_data)
   deallocate(out_data)

   ! close ECMWF file
   call PBCLOSE(fu, stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to close file.')

end subroutine read_ecmwf_grib
