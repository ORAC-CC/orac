!-------------------------------------------------------------------------------
! Name: read_ecmwf_wind_grib.F90
!
! Purpose:
! Read surface wind components and lat/lon from ECMWF file. Intended for use
! with the single files from the MARS system (filename
! ERA_Interim_an_YYYYMMDD_HH+00.grb). Successor to read_ecmwf_dimensions.F90.
!
! Description and Algorithm details:
! 1) Open file.
! 2) Check file grid is as expected (rectangular).
! 3) Read field dimensions and allocate ecmwf structure.
! 4) Loop through GRIB fields, checking dimensions are consistent.
! 5) Read U10, V10, lat, lon into ecmwf structure.
! 6) Close file.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! nwp_path string in   Full path to ECMWF NCDF file to read.
! ecmwf      struct both Structure summarising contents of ECMWF files.
!
! History:
! 2014/05/07, AP: First version.
! 2014/11/04, OS: Added reading of skin temperature.
! 2016/01/27, GM: Check and trim filename length for grib_open_file().
! 2016/02/02, OS: Now also reads sea-ice cover and snow depth from HR ERA file.
! 2016/02/03, GM: Changes/fixes to read the HR ERA GRIB file: no vertical
!    coordinate and no wind.
! 2016/04/26, AP: For high res files, abvec now set with init routine.
! 2016/05/26, GT: Moved ecmwf%kdim=nk statement inside if (.not. high_res) block,
!    as nk is undefined in the high_res case
! 2017/02/07, SP: Added support for NOAA GFS atmosphere data (ExtWork)
! 2017/04/11, SP: Added nwp_flag=6, for working with GFS analysis files.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_grib(nwp_path, ecmwf, nwp_flag)

   use grib_api
   use preproc_constants_m

   implicit none

   character(len=*), intent(in)    :: nwp_path
   type(ecmwf_t),    intent(inout) :: ecmwf
   integer,          intent(in)    :: nwp_flag

   real, allocatable, dimension(:) :: pv, lat, lon, val
   integer                         :: fid, gid, stat
   integer                         :: PVPresent, PLPresent
   integer                         :: i, n, ni, nj, nk, npv, ni_, nj_
   integer                         :: param, level, nlevels
   character(len=100)              :: ltype, lname

   if (len_trim(nwp_path) .gt. 1024) call h_e_e('wind_grib', &
         'Filename argument string nwp_path is too long.  It must be ' // &
         'limited to a length of 1024 due to a bug in grib_api.')

   ! open file
   call grib_open_file(fid, trim(nwp_path), 'r', stat)
   if (stat .ne. 0) call h_e_e('wind_grib', 'Error opening GRIB file. '// &
        trim(nwp_path))
   call grib_new_from_file(fid, gid, stat)
   if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting GRIB_ID. '// &
        trim(nwp_path))
   if (gid .eq. GRIB_END_OF_FILE) call h_e_e('wind_grib', 'Empty GRIB file.')

   if (nwp_flag .ne. 0) then
      ! ensure it contains the expected fields
      call grib_get(gid, 'PVPresent', PVPresent)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting PVPresent.')
      if (PVPresent .eq. 0) &
           call h_e_e('wind_grib', 'Incorrect file format. Check ECMWF_FLAG.')
      call grib_get(gid, 'PLPresent', PLPresent)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting PLPresent.')
      if (PLPresent .eq. 1) &
           call h_e_e('wind_grib', &
                      'Incorrect file formatting. Check ECMWF_FLAG.')

      ! fetch vertical coordinate
      call grib_get_size(gid, 'pv', npv, stat)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error checking PV.')
      allocate(pv(npv))
      call grib_get(gid, 'pv', pv, stat)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting PV.')
      nk = npv/2-1
   end if

   ! read dimensions
   call grib_get(gid, 'Ni', ni, stat)
   if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting Ni.')
   call grib_get(gid, 'Nj', nj, stat)
   if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting Nj.')
   n = ni*nj

   ! allocate temporary arrays
   allocate(lon(n))
   allocate(lat(n))
   allocate(val(n))

   ! allocate permanent arrays
   allocate(ecmwf%lon(ni))
   allocate(ecmwf%lat(nj))
   if (nwp_flag .gt. 0) then
      allocate(ecmwf%avec(nk+1))
      allocate(ecmwf%bvec(nk+1))
   else
      allocate(ecmwf%avec(ecmwf%kdim))
      allocate(ecmwf%bvec(ecmwf%kdim))
   end if
   allocate(ecmwf%u10(ni,nj))
   allocate(ecmwf%v10(ni,nj))
   allocate(ecmwf%skin_temp(ni,nj))
   allocate(ecmwf%snow_depth(ni,nj))
   allocate(ecmwf%sea_ice_cover(ni,nj))

   ! Initialise
   ecmwf%lon(:) = sreal_fill_value
   ecmwf%lat(:) = sreal_fill_value

   nlevels = 0
   do while (stat .ne. GRIB_END_OF_FILE)
      ! check for consistent dimensions
      call grib_get(gid, 'Ni', ni_, stat)
      if (stat .ne. 0 .or. ni_ .ne. ni) call h_e_e('wind_grib', 'Error with Ni.')
      call grib_get(gid, 'Nj', nj_, stat)
      if (stat .ne. 0 .or. nj_ .ne. nj) call h_e_e('wind_grib', 'Error with Nj.')
      call grib_get(gid, 'level', level, stat)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting level.')
      call grib_get(gid, 'typeOfLevel', ltype, stat)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting level type.')
      call grib_get(gid, 'name', lname, stat)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting name.')

      ! count level number
      if (level .gt. nlevels) nlevels = level

      ! if wind field, read and output
      call grib_get(gid, 'parameter', param, stat)
      if (stat .ne. 0) call h_e_e('wind_grib', 'Error getting parameter.')

      select case (param)
      case(165)
         ! 10m zonal wind component, latitude, and longitude
         call grib_get(gid, 'values', val, stat)
         if (stat .ne. 0) call h_e_e('wind_grib', 'Error reading U10.')

         ecmwf%u10 = reshape(val, (/ni, nj/)) 
      case(166)
         ! 10 m meriodional wind component
         call grib_get(gid, 'values', val, stat)
         if (stat .ne. 0) call h_e_e('wind_grib', 'Error reading V10.')

         ecmwf%v10 = reshape(val, (/ni, nj/))
      case(235)
         ! skin temperature
         call grib_get_data(gid, lat, lon, val, stat)
         if (stat .ne. 0) call h_e_e('wind_grib', 'Error reading skin_temp.')

         ecmwf%skin_temp = reshape(val, (/ni, nj/))
         ecmwf%lon = lon(1:ni)
         do i = 1, n,ni
            ecmwf%lat(1+i/ni) = lat(i)
         end do
      case(31)
         ! sea-ice cover
         call grib_get(gid, 'values', val, stat)
         if (stat .ne. 0) call h_e_e('wind_grib', 'Error reading sea-ice cover.')

         ecmwf%sea_ice_cover = reshape(val, (/ni, nj/))
      case(141)
         ! snow depth (ECMWF)
         call grib_get(gid, 'values', val, stat)
         if (stat .ne. 0) call h_e_e('wind_grib', 'Error reading snow_depth.')

         ecmwf%snow_depth = reshape(val, (/ni, nj/))
      case(3066)
         ! snow depth (GFS)
         call grib_get(gid, 'values', val, stat)
         if (stat .ne. 0) call h_e_e('wind_grib', 'Error reading snow_depth.')

         ecmwf%snow_depth = reshape(val, (/ni, nj/))
         where (ecmwf%snow_depth .eq. 9999.) ecmwf%snow_depth = sreal_fill_value
      case(130)
         if (trim(ltype) .eq. 'surface') then
            if (nwp_flag .eq. 0) then
               ! skin temp (GFS) - proxied by surface temp
               call grib_get_data(gid, lat, lon, val, stat)
               if (stat .ne. 0) call h_e_e('wind_grib', &
                                           'Error reading skin_temp.')

               ecmwf%skin_temp = reshape(val, (/ni, nj/))
               ecmwf%lon = lon(1:ni)
               do i = 1, n,ni
                  ecmwf%lat(1+i/ni) = lat(i)
               end do
            end if
         end if
      end select

      ! advance file position
      call grib_release(gid)
      call grib_new_from_file(fid, gid, stat)
   end do

   call grib_close_file(fid)

   if (all(ecmwf%lat .eq. sreal_fill_value) .eqv. .true.) &
        call h_e_e('wind_grib', 'No geoinfo, check skin/sfc temp in GRB.')

   ! set ECMWF dimensions

   ecmwf%xdim = ni
   ecmwf%ydim = nj
   if (nwp_flag .gt. 0) then
      ecmwf%kdim = nk
      if (nk .ne. nlevels) &
           call h_e_e('wind_grib', 'Inconsistent vertical levels.')
      ecmwf%avec = pv(1:nk+1)
      ecmwf%bvec = pv(nk+2:)
      deallocate(pv)
   end if

   ! clean-up
   deallocate(lat)
   deallocate(lon)
   deallocate(val)

end subroutine read_ecmwf_wind_grib
