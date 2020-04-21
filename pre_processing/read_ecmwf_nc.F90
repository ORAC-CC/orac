!-------------------------------------------------------------------------------
! Name: read_ecmwf_nc.F90
!
! Purpose:
! Reads NetCDF format ECMWF ERA interim data. It is interpolated onto the
! preprocessing grid using the EMOS package
!
! Description and Algorithm details:
! 1) Prepare interpolation.
! 2) Open file.
! 3) Loop over variables:
!    a) Identify variable with desired data field.
!    b) Read data and interpolate.
!    c) Copy data into preprocessor structure.
! 4) Close file.
!
! Arguments:
! Name            Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path     string  In   NetCDF ECMWF file to be opened.
! ecmwf          struct  both Structure summarising contents of ECMWF files.
! preproc_dims   struct  In   Dimensions of the preprocessing grid.
! preproc_prtm   struct  Both Pressure-level information for RTTOV.
! verbose        logic  in   T: Print min/max of each field; F: Don't.
!
! History:
! 2012/08/06, ??: Initial version ecmwf code
! 2012/08/06, CP: modified to write data into preprocessing structures
! 2012/08/07, CP: added in reading of surface data pressure level data, added in
!    ozone profile and geopotential height,
! 2012/08/13, CP: totally rewrote program to cope with multiple netcdf file read
! 2012/11/13, CP: added in surface pressure and pressure
! 2012/11/29, CP: added ecmwf_2d definitions for u10 and v10
! 2013/01/29, CP: changed how geopotential was read out
! 2013/03/05, CP: small change to work in gfortran
! 2013/03/06, CP: tidy up and rearrange badc files
! 2013/03/07, CP: tidied up allocations and changed code to read in q and 03
!    form a netcdf file because grib code did not work for badc style grb files
!    also added computation of geopot because was previously dome in grib read
! 2013/03/18, GT: Altered the allocation of temporary arrays to hold the various
!    ECMWF variable to avoid the compiler complaining of possible use of
!    unallocated arrays.
! 2013/03/19, GT: Fixed the reading of the gpam file (containing specific
!    humidity and O3 data). Moved the rearranging of the ECMWF arrays into the
!    same if statements as the reading commands, and changed the generation of
!    the pressure profile array so that it is created on the rearranged grid.
!    Removed quite a few debugging print statements
! 2013/03/20, GT: Fixed a bug introduced in yesterday's changes (10 m wind
!    components were not being written to ECMWF structures)
! 2013/10/29, ??: Changed array allocation of phi_lay and phi_lev
! 2014/02/10, AP: Extreme tidying. Made all allocatable arrays definite size.
!    Removed check of file dimensions. Made a,bvector global. Added nearest
!    neighbour functionality. Made geopotential calculation external. Removed
!    surface flag.
! 2014/05/08, AP: Complete rewrite, vastly tidying the original and updating to
!    the new ecmwf structure.
! 2014/12/30, GM: Allocate old_data and new_data on the heap explicitly rather
!    than declaring them automatic. In this case, as automatic, gfortran, and
!    maybe other compilers, were allocating the arrays on the stack overflowing
!    the stack. These arrays are big enough that they should be explicitly
!    allocated on the heap anyway.
! 2014/02/04, MS+OS: Implemented nearest neighbour interpolation of ECMWF data;
!    only activated when WRAPPER flag is set; preliminary approach which will
!    be made obsolete when ECMWF data will be retrieved on preproc grid
!    resolution.
! 2015/07/03, OS: added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
! 2016/04/04, SP: Added option to process ECMWF forecast/analysis data that's
!    stored in a single NetCDF file.
! 2018/11/05, SP: Add CAPE

!
! Bugs:
! - you need to be careful with parameter naming as the variable names are not
!   consistent across files for example the variable name could be lnsp or LNSP
!-------------------------------------------------------------------------------

subroutine read_ecmwf_nc(ecmwf_path, ecmwf, preproc_dims, preproc_geoloc, &
     preproc_prtm, verbose, ecmwf_flag)

   use orac_ncdf_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),        intent(in)    :: ecmwf_path
   type(ecmwf_t),           intent(in)    :: ecmwf
   type(preproc_dims_t),    intent(in)    :: preproc_dims
   type(preproc_geoloc_t),  intent(in)    :: preproc_geoloc
   type(preproc_prtm_t),    intent(inout) :: preproc_prtm
   logical,                 intent(in)    :: verbose
   integer,                 intent(in)    :: ecmwf_flag

   integer(lint),     external            :: INTIN, INTOUT, INTF
   integer(lint),     parameter           :: BUFFER = 2000000

   integer(lint),            dimension(1) :: intv, old_grib, new_grib
   real(dreal)                            :: grid(2), area(4)
   real(dreal), allocatable, dimension(:) :: old_data, new_data
   character(len=20),        dimension(1) :: charv

   real(sreal),       pointer             :: array2d(:,:), array3d(:,:,:)
   integer(4)                             :: n, ni, nj, i, j, k, ivar
   integer(4)                             :: fid, nvar
   integer(4)                             :: old_len, new_len
   character(len=20)                      :: name
   logical                                :: three_d
   real(sreal)   :: dummy2d(ecmwf%xdim,ecmwf%ydim,1,1)
   real(sreal)   :: dummy3d(ecmwf%xdim,ecmwf%ydim,ecmwf%kdim,1)
   real(sreal)   :: dummy3d_2(ecmwf%xdim,ecmwf%ydim,ecmwf%kdim)
#ifdef WRAPPER
   real(sreal)   :: ecmwf_lon(ecmwf%xdim)
   real(sreal)   :: ecmwf_lat(ecmwf%ydim)
   integer(lint) :: pointer_x(preproc_dims%min_lon:preproc_dims%max_lon)
   integer(lint) :: pointer_y(preproc_dims%min_lat:preproc_dims%max_lat)
   real(sreal)   :: diff_lon(ecmwf%xdim), diff_lat(ecmwf%ydim)
#endif



   n = ecmwf%xdim*ecmwf%ydim

   ! input details of new grid (see note in read_ecmwf_grib)
   charv(1) = 'yes'
   grid(1) = sreal_fill_value
   if (INTIN('missingvalue', intv, grid, charv) .ne. 0) &
        call h_e_e('nc', 'INTIN missingvalue failed.')
   charv(1) = 'unpacked'
   if (INTIN('form', intv, grid, charv) .ne. 0) &
        call h_e_e('nc', 'INTIN form failed.')
   if (INTOUT('form', intv, grid, charv) .ne. 0) &
        call h_e_e('nc', 'INTOUT form failed.')
   intv(1) = ecmwf%ydim/2
   if (INTIN('regular', intv, grid, charv) .ne. 0) &
        call h_e_e('nc', 'INTIN reg failed.')
   grid(1) = 0.5 / preproc_dims%dellon
   grid(2) = 0.5 / preproc_dims%dellat
   if (INTOUT('grid', intv, grid, charv) .ne. 0) &
        call h_e_e('nc', 'INTOUT grid failed.')
   area(1) = preproc_geoloc%latitude(preproc_dims%max_lat) + 0.01*grid(2)
   area(2) = preproc_geoloc%longitude(preproc_dims%min_lon) + 0.01*grid(1)
   area(3) = preproc_geoloc%latitude(preproc_dims%min_lat) + 0.01*grid(2)
   area(4) = preproc_geoloc%longitude(preproc_dims%max_lon) + 0.01*grid(1)
   if (INTOUT('area', intv, area, charv) .ne. 0) &
        call h_e_e('nc', 'INTOUT area failed.')
   ni = ceiling((area(4)+180.)/grid(1)) - floor((area(2)+180.)/grid(1)) + 1
   nj = ceiling((area(1)+90.)/grid(2)) - floor((area(3)+90.)/grid(2)) + 1

   ! open file
   call ncdf_open(fid, ecmwf_path, 'read_ecmwf_nc()')
   if (nf90_inquire(fid, nVariables=nvar) .ne. 0) &
        call h_e_e('nc', 'NF INQ failed.')

   allocate(old_data(BUFFER))
   allocate(new_data(BUFFER))

   ! loop over variables
   do ivar = 1, nvar

      ! determine nearest ecmwf neighbour of preproc pixel
#ifdef WRAPPER
      if(ivar .eq. 1) then

        call ncdf_read_array(fid, 'lon', ecmwf_lon, verbose)
        call ncdf_read_array(fid, 'lat', ecmwf_lat, verbose)

        where(ecmwf_lon .gt. 180.) ecmwf_lon = ecmwf_lon-360.

        do i = preproc_dims%min_lon, preproc_dims%max_lon
          diff_lon = abs(ecmwf_lon - preproc_geoloc%longitude(i))
          pointer_x(i) = minloc(diff_lon, 1)
        end do

        do j = preproc_dims%min_lat, preproc_dims%max_lat
          diff_lat = abs(ecmwf_lat - preproc_geoloc%latitude(j))
          pointer_y(j) = minloc(diff_lat, 1)
        end do

      end if
#endif

      ! determine if field should be read
      if (nf90_inquire_variable(fid, ivar, name) .ne. 0) &
           call h_e_e('nc', 'NF VAR INQUIRE failed.')
      select case (name)
      case('Z', 'z')
         three_d = .false.
         array2d => preproc_prtm%geopot
      case('Q', 'q')
         three_d = .true.
         array3d => preproc_prtm%spec_hum
      case('T')
         three_d = .true.
         array3d => preproc_prtm%temperature
      case('O3', 'o3')
         three_d = .true.
         array3d => preproc_prtm%ozone
      case('LNSP', 'lnsp')
         three_d = .false.
         array2d => preproc_prtm%lnsp
      case('CI', 'ci')
         three_d = .false.
         array2d => preproc_prtm%sea_ice_cover
      case('ASN', 'asn')
         three_d = .false.
         array2d => preproc_prtm%snow_albedo
      case('TCWV', 'tcwv')
         three_d = .false.
         array2d => preproc_prtm%totcolwv
      case('SD', 'sd')
         three_d = .false.
         array2d => preproc_prtm%snow_depth
      case('U10', 'u10', 'U10M')
         three_d = .false.
         array2d => preproc_prtm%u10
      case('V10', 'v10', 'V10M')
         three_d = .false.
         array2d => preproc_prtm%v10
      case('T2', 't2', 'T2M')
         three_d = .false.
         array2d => preproc_prtm%temp2
      case('SKT', 'skt')
         three_d = .false.
         array2d => preproc_prtm%skin_temp
      case('SSTK', 'sstk')
         three_d = .false.
         array2d => preproc_prtm%sst
      case('AL', 'al', 'LSM')
         three_d = .false.
         array2d => preproc_prtm%land_sea_mask
#ifdef INCLUDE_SATWX
      case('cape', 'CAPE')
         three_d = .false.
         array2d => preproc_prtm%cape
#endif
      case default
         cycle
      end select
      if (nf90_inquire_variable(fid, ivar, name) .ne. 0) &
           call h_e_e('nc', 'NF VAR INQUIRE failed.')
      if (three_d) then
         if (ecmwf_flag.ne.4 .and. ecmwf_flag .ne. 5) then
            call ncdf_read_array(fid, name, dummy3d, verbose)
         else
            call ncdf_read_array(fid, name, dummy3d_2, verbose)
         end if
         do k = 1, ecmwf%kdim
            old_len = n
            if (ecmwf_flag.ne.4 .and. ecmwf_flag .ne. 5) then
               old_data(1:n) = reshape(real(dummy3d(:,:,k,1), kind=8), [n])
            else
               old_data(1:n) = reshape(real(dummy3d_2(:,:,k), kind=8), [n])
            end if

            new_len = BUFFER

#ifndef WRAPPER
            if (INTF(old_grib, old_len, old_data, new_grib, new_len, new_data).ne.0)&
                  call h_e_e('nc', 'INTF failed.')
            if (new_len .ne. ni*nj) print*, '3D Interpolation grid wrong.'

            ! copy data into preprocessing grid
            do j = 1, nj, 2
               do i = 1, ni, 2
                  array3d(preproc_dims%min_lon+i/2, &
                     preproc_dims%min_lat+(nj-j)/2, k) = &
                     real(new_data(i+(j-1)*ni), kind=4)
               end do
            end do
#else
            ! copy data into preprocessing grid
            do i = preproc_dims%min_lon, preproc_dims%max_lon
               do j = preproc_dims%min_lat, preproc_dims%max_lat
                  array3d(i,j,k) = real(dummy3d(pointer_x(i), pointer_y(j), k, 1))
               end do
            end do
#endif
         end do
         if (verbose) print*, trim(name), ') Min: ', minval(array3d), &
              ', Max: ', maxval(array3d)
      else
         call ncdf_read_array(fid, name, dummy2d, verbose)
         old_len = n
         old_data(1:n) = reshape(real(dummy2d, kind=8), [n])

         new_len = BUFFER

#ifndef WRAPPER
         if (INTF(old_grib, old_len, old_data, new_grib, new_len, new_data) .ne. 0) &
               call h_e_e('nc', 'INTF failed.')
         if (new_len .ne. ni*nj) print*, '2D Interpolation grid wrong.'

         ! copy data into preprocessing grid
         do j = 1, nj, 2
            do i = 1, ni, 2
               array2d(preproc_dims%min_lon+i/2, &
                  preproc_dims%min_lat+(nj-j)/2) = &
                  real(new_data(i+(j-1)*ni), kind=4)
            end do
         end do
#else
         ! copy data into preprocessing grid
         do i = preproc_dims%min_lon, preproc_dims%max_lon
            do j = preproc_dims%min_lat, preproc_dims%max_lat
               array2d(i,j) = real(dummy2d(pointer_x(i), pointer_y(j), 1, 1))
            end do
         end do
#endif
         if (verbose) print*, trim(name), ') Min: ', minval(array2d), &
              ', Max: ', maxval(array2d)
      end if
   end do

   deallocate(old_data)
   deallocate(new_data)

   call ncdf_close(fid, 'read_ecmwf_nc()')

end subroutine read_ecmwf_nc
