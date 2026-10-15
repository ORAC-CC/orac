!-------------------------------------------------------------------------------
! Name: read_gfs_grib.F90
!
! Purpose:
! Read GFS data from a GRIB file, having interpolated it onto the
! preprocessing grid.
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
! ecmwf_file     string in   Full path to a GFS GRIB file to read.
! preproc_dims   struct out  Summary of preprocessing grid definitions
! preproc_geoloc struct out  Summary of lat/lon values
! preproc_prtm   struct out  Summary of profiles and surface fields
! verbose        logic  in   T: Print min/max of each field; F: Don't.
!
! History:
! 2017/02/04, SP: Initial version (ExtWork)
! 2017/02/25, SP: Update to RTTOV v12.1 (ExtWork)
! 2017/03/27, SP: New technique for computing profile levels. Improves retrievals
!                 over high altitude land regions (Tibet, f.ex) (ExtWork)
! 2017/03/30, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2017/06/21, OS: line continuation symbol set to &
! 2024/07/01, DH: Change indexing to use preproc_dims for all dimensions
!
! Bugs:
! - If you're having problems with INTF, set the environment variable JDCNDBG=1
! for additional debugging output.
!-------------------------------------------------------------------------------

subroutine read_gfs_grib(ecmwf_file,preproc_dims,preproc_geoloc, &
     preproc_prtm, ecmwf, nwp_flag, verbose)

   use grib_api
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),       intent(in)    :: ecmwf_file
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(preproc_geoloc_t),     intent(in)    :: preproc_geoloc
   type(preproc_prtm_t),       intent(inout) :: preproc_prtm
   type(ecmwf_t),           intent(inout)    :: ecmwf
   integer,       intent(in)                 :: nwp_flag
   logical,                    intent(in)    :: verbose

#ifdef INCLUDE_EMOS
   integer(lint), parameter                 :: BUFFER = 3000000
   integer(lint), external                  :: INTIN,INTOUT,INTF2
#endif
   integer(lint)                            :: fu,stat,nbytes
   integer(lint)                            :: out_bytes, out_words
   integer(lint), allocatable, dimension(:) :: in_data,out_data
   integer(lint)                            :: iblank(4)
   real(dreal)                              :: grid(2),area(4)
   character(len=20)                        :: charv(1)
   character(len=100)                       :: ltype,lname

   integer(lint)                            :: gid,level,param
   integer                                  :: tlev,qlev,olev,glev

   integer(lint)                            :: n,ni,nj,i,j
   real(sreal), dimension(:),   allocatable :: val
   real(sreal), dimension(:,:), pointer     :: array

   integer(lint),dimension(41)              :: gfs_levlist

#ifdef INCLUDE_EMOS
   gfs_levlist = (/1,2,4,7,10,20,40,70,100,&
                   200,300,500,700,1000,&
                   1500,2000,3000,4000,5000,7000,10000,&
                   15000,20000,25000,30000,35000,40000,45000,&
                   50000,55000,60000,65000,70000,75000,80000,&
                   85000,90000,92500,95000,97500,100000/)

   ! Initialise level count, needed for GFS files
   tlev = 1
   qlev = 1
   olev = 1
   glev = 1

   ! Initialise some arrays, prevents issues with missing GFS values
   ! (only some levels)
   preproc_prtm%spec_hum(:,:,:) = 0.
   preproc_prtm%ozone(:,:,:) = 1e-10

   ! open the GFS file
   call PBOPEN(fu, ecmwf_file, 'r', stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to read file.')

   ! select appropriate grid definition
   charv(1)='grib'
   if (INTIN('form',iblank,grid,charv) .ne. 0) &
        call h_e_e('grib', 'INTIN form failed.')
   if (INTOUT('form',iblank,grid,charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT form failed.')

   ! input details of new grid (see note in header)
   grid(1) = 0.5 / preproc_dims%dellon
   grid(2) = 0.5 / preproc_dims%dellat
   if (INTOUT('grid',iblank,grid,charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT grid failed.')
   area(1) = preproc_geoloc%latitude(preproc_dims%ydim) + 0.01*grid(2)
   area(2) = preproc_geoloc%longitude(1) + 0.01*grid(1)
   area(3) = preproc_geoloc%latitude(1) + 0.01*grid(2)
   area(4) = preproc_geoloc%longitude(preproc_dims%xdim) + 0.01*grid(1)
   if (INTOUT('area',iblank,area,charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT area failed.')

   allocate(in_data(BUFFER))
   allocate(out_data(BUFFER))
   allocate(ecmwf%temperature(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%pressure(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%spec_hum(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%phi_lev(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%ozone(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))

   ! interpolate ECMWF products to preproc grid
   do
      ! read GRIB data field
      call PBGRIB(fu, in_data, BUFFER*lint, nbytes, stat)
      if (stat .eq. -1) exit
      if (stat .ne. 0) call h_e_e('grib', 'Failure to read product.')

      ! Check if this is something we want to read
      call grib_new_from_message(gid,in_data,stat)
      call grib_get(gid,'parameter',param,stat)
      if (param .ne. 130 .and. param .ne. 157 .and. &
          param .ne. 133 .and. param .ne. 260131 .and. &
          param .ne. 134 .and. param .ne. 31 .and. &
          param .ne. 3066 .and. param .ne. 165 .and. &
          param .ne. 166 .and. param .ne. 167 .and. &
          param .ne. 172 .and. param .ne. 54 .and. &
          param .ne. 156 .and. param .ne. 228002) then
         call grib_release(gid,stat)
         cycle
      end if

      call grib_release(gid,stat)

      ! interpolate GRIB field (into another GRIB field)
      ! in_words = nbytes / lint
      ! out_words = BUFFER
      out_bytes = BUFFER * lint
      !if (INTF(in_data,in_words,zni,out_data,out_words,zno) .ne. 0) &
      !     call h_e_e('grib', &
      !       'INTF failed. Check if 1/dellon 1/dellat are muliples of 0.001.')
      if (INTF2(in_data,nbytes,out_data,out_bytes) .ne. 0) &
           call h_e_e('grib', 'INTF2 failed.')
      out_words = out_bytes/lint

!      stop
      ! load grib data into grib_api
      call grib_new_from_message(gid,out_data(1:out_bytes),stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting GRIB_ID.')

      call grib_get(gid,'parameter',param,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting parameter #.')
      call grib_get(gid,'level',level,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
      call grib_get(gid,'Nj',nj,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting nj.')

      call grib_get(gid,'typeOfLevel',ltype,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level type.')
      call grib_get(gid,'name',lname,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting name.')

      ! regular grid
      call grib_get(gid,'Ni',ni,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
      n=ni*nj

      if (.not.allocated(val)) allocate(val(n))

      call grib_get(gid,'values',val,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error reading data.')
      call grib_release(gid,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error releasing GRIB_ID.')

      ! Normalize level to hPa if needed
      if (trim(ltype) == 'isobaricInhPa') then
          level = level * 100
          ltype = 'isobaricInPa'
      end if

      ! select correct output array
      select case (param)
      case(130)
         ! Temperature
         if ((any(abs(level - gfs_levlist) < 1e-2)) .and. &
             trim(ltype) .eq. 'isobaricInPa') then
            array => ecmwf%temperature( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,tlev)
            ecmwf%pressure(:,:,tlev)=level
            tlev=tlev+1
         else if (trim(ltype) .eq. 'surface') then
            array => preproc_prtm%skin_temp( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
         else
            cycle
         end if
      case(133)
         if ((all(abs(level - gfs_levlist) < 1e-2)) .or. &
             trim(ltype) .ne. 'isobaricInPa') cycle
         ! Specific humidity
         array => ecmwf%spec_hum( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,qlev)
         qlev=qlev+1
      case(156)
         if ((all(abs(level - gfs_levlist) < 1e-2)) .or. &
             trim(ltype) .ne. 'isobaricInPa') cycle
         ! Geopotential
         array => ecmwf%phi_lev( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,glev)
         glev=glev+1
      case(228002)
         if ((all(abs(level - gfs_levlist) < 1e-2)) .or. &
             trim(ltype) .ne. 'surface') cycle
         ! Geopotential
         array => preproc_prtm%geopot(1:preproc_dims%xdim, 1:preproc_dims%ydim)
      case(260131)
         ! Ozone
         if ((all(abs(level - gfs_levlist) < 1e-2)) .or. &
             trim(ltype) .ne. 'isobaricInPa') cycle
         array => preproc_prtm%ozone( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,olev)
         olev=olev+1
      case(134)
         array => preproc_prtm%lnsp( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(31)
         array => preproc_prtm%sea_ice_cover( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(3066)
         array => preproc_prtm%snow_depth( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(165)
         array => preproc_prtm%u10( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(166)
         array => preproc_prtm%v10( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(167)
         array => preproc_prtm%temp2( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(172)
         array => preproc_prtm%land_sea_mask( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
      case(54)
         if (trim(ltype) .ne. 'tropopause') cycle
         array => preproc_prtm%trop_p( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim)
         preproc_prtm%trop_p = preproc_prtm%trop_p * pa2hpa
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
      if (verbose) print*,param,') Min: ',minval(array), &
              ', Max: ',maxval(array)
   end do

   ! GFS has no skin temperature variable
   ecmwf%phi_lev   = ecmwf%phi_lev*g_wmo

   ! convert from pressure to log pressure
   preproc_prtm%lnsp = log(preproc_prtm%lnsp)

   deallocate(val)

   ! GFS has no snow mask, so use snow depth instead. 0.1m threshold arbitrary
   where(preproc_prtm%snow_depth .gt. 0.1) preproc_prtm%snow_albedo = 0.98
   deallocate(in_data)
   deallocate(out_data)

   ! close ECMWF file
   call PBCLOSE(fu,stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to close file.')

   ! Refactor all the GFS levels so that below-surface contributions are removed.
   call interpolate_gfs_levels(preproc_prtm, preproc_dims, ecmwf, nwp_flag, verbose)
#else
   write(*,*) 'ERROR: read_gfs_grib(): LIBEMOS is required for ' // &
        'use_ecmwf_preproc_grid = .false.'
#endif

end subroutine read_gfs_grib

subroutine read_gfs_grib_for_preproc_structures(ecmwf_file,preproc_dims,preproc_geoloc, &
     preproc_prtm,verbose, ecmwf, date, ind, nwp_flag)

   use grib_api
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),       intent(in)    :: ecmwf_file
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(preproc_geoloc_t),     intent(in)    :: preproc_geoloc
   type(preproc_prtm_t),       intent(inout) :: preproc_prtm
   logical,                    intent(in)    :: verbose
   type(ecmwf_t),           intent(inout)    :: ecmwf
   integer,          intent(in)              :: date, ind
   integer,       intent(in)                 :: nwp_flag

   integer(lint), parameter                 :: BUFFER = 3000000
   integer(lint), external                  :: INTIN,INTOUT,INTF2
   integer(lint)                            :: fu,stat,nbytes
   integer(lint)                            :: out_bytes, out_words
   integer(lint), allocatable, dimension(:) :: in_data
   integer(lint)                            :: iblank(4)
   real(dreal)                              :: grid(2),area(4)
   character(len=20)                        :: charv(1)
   character(len=100)                       :: ltype,lname

   integer(lint)                            :: gid,level,param
   integer                                  :: tlev,qlev,olev,glev

   integer(lint)                            :: n,ni,nj,i,j
   real(sreal), dimension(:),   allocatable :: val
   real(sreal), dimension(:,:), pointer     :: array

   integer(lint),dimension(41)              :: gfs_levlist
   real(sreal)   :: dummy2d(ecmwf%xdim,ecmwf%ydim)
   real(sreal), allocatable   :: dummy3d(:,:,:)


   gfs_levlist = (/1,2,4,7,10,20,40,70,100,&
                   200,300,500,700,1000,&
                   1500,2000,3000,4000,5000,7000,10000,&
                   15000,20000,25000,30000,35000,40000,45000,&
                   50000,55000,60000,65000,70000,75000,80000,&
                   85000,90000,92500,95000,97500,100000/)

   ! Initialise level count, needed for GFS files
   tlev = 1
   qlev = 1
   olev = 1
   glev = 1

   ! Initialise some arrays, prevents issues with missing GFS values
   ! (only some levels)
   preproc_prtm%spec_hum(:,:,:) = 0.
   preproc_prtm%ozone(:,:,:) = 1e-10

   ! open the GFS file
   call PBOPEN(fu, ecmwf_file, 'r', stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to read file.')

   allocate(in_data(BUFFER))
   allocate(ecmwf%temperature(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%pressure(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%spec_hum(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%phi_lev(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   allocate(ecmwf%ozone(ecmwf%xdim, ecmwf%ydim,ecmwf%kdim))
   ! interpolate ECMWF products to preproc grid
   do
      ! read GRIB data field
      call PBGRIB(fu, in_data, BUFFER*lint, nbytes, stat)
      if (stat .eq. -1) exit
      if (stat .ne. 0) call h_e_e('grib', 'Failure to read product.')

      ! Check if this is something we want to read
      call grib_new_from_message(gid,in_data,stat)
      call grib_get(gid,'parameter',param,stat)
      if (param .ne. 130 .and. param .ne. 157 .and. &
          param .ne. 133 .and. param .ne. 260131 .and. &
          param .ne. 134 .and. param .ne. 31 .and. &
          param .ne. 3066 .and. param .ne. 165 .and. &
          param .ne. 166 .and. param .ne. 167 .and. &
          param .ne. 172 .and. param .ne. 54 .and. &
          param .ne. 156 .and. param .ne. 228002) then
         call grib_release(gid,stat)
         cycle
      end if

      call grib_release(gid,stat)

      ! load grib data into grib_api
      call grib_new_from_message(gid,in_data,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting GRIB_ID.')

      call grib_get(gid,'parameter',param,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting parameter #.')
      call grib_get(gid,'level',level,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
      call grib_get(gid,'Nj',nj,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting nj.')

      call grib_get(gid,'typeOfLevel',ltype,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level type.')
      call grib_get(gid,'name',lname,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting name.')

      ! regular grid
      call grib_get(gid,'Ni',ni,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
      n=ni*nj

      if (.not.allocated(val)) allocate(val(n))

      call grib_get(gid,'values',val,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error reading data.')
      call grib_release(gid,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error releasing GRIB_ID.')
      ! select correct output array
      ! Normalize level to hPa if needed
      if (trim(ltype) == 'isobaricInhPa') then
          level = level * 100
          ltype = 'isobaricInPa'
      end if
      dummy2d(:,:) = reshape(val, shape(dummy2d))
      where (dummy2d .eq. 9999) dummy2d = sreal_fill_value
      call rearrange_ecmwf_var2d(ecmwf, dummy2d, date, ind)

      select case (param)
      case(130)
         ! Temperature
         if ((any(abs(level - gfs_levlist) < 1e-2)) .and. &
             trim(ltype) .eq. 'isobaricInPa') then
            ecmwf%temperature(1:preproc_dims%xdim, 1:preproc_dims%ydim,tlev) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
            ecmwf%pressure(:,:,tlev)=level
            tlev=tlev+1
         else if (trim(ltype) .eq. 'surface') then
            preproc_prtm%skin_temp(1:preproc_dims%xdim, 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
         else
            cycle
         end if
      case(133)
         if ((all(abs(level - gfs_levlist) < 1e-2)) .or. &
             trim(ltype) .ne. 'isobaricInPa') cycle
         ! Specific humidity
         ecmwf%spec_hum( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,qlev) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
         qlev=qlev+1
      case(156)
         if ((all(abs(level - gfs_levlist) < 1e-2)) .or. &
             trim(ltype) .ne. 'isobaricInPa') cycle
         ! Geopotential
         ecmwf%phi_lev( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,glev) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
         glev=glev+1
      case(228002)
         if ((all(abs(level - gfs_levlist) < 1e-2)) .or. &
             trim(ltype) .ne. 'surface') cycle
         preproc_prtm%geopot(1:preproc_dims%xdim, 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)*g_wmo

      case(260131)
         ! Ozone
         if (all(level .ne. gfs_levlist) .or. &
             trim(ltype) .ne. 'isobaricInPa') cycle
         ecmwf%ozone( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim,olev) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
         olev=olev+1
      case(134)
         preproc_prtm%lnsp( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
      case(31)
         preproc_prtm%sea_ice_cover( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
      case(3066)
         preproc_prtm%snow_depth( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
      case(165)
         preproc_prtm%u10( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
      case(166)
         preproc_prtm%v10( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
      case(167)
         preproc_prtm%temp2( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
      case(172)
         preproc_prtm%land_sea_mask( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
      case(54)
         if (trim(ltype) .ne. 'tropopause') cycle
         preproc_prtm%trop_p( &
                 1:preproc_dims%xdim, &
                 1:preproc_dims%ydim) = dummy2d(preproc_dims%min_lon_ind:preproc_dims%max_lon_ind, &
                                         preproc_dims%min_lat_ind:preproc_dims%max_lat_ind)
         preproc_prtm%trop_p = preproc_prtm%trop_p * pa2hpa
      case default
         cycle
      end select
      
      !if (verbose) print*,param,') Min: ',minval(array), &
      !        ', Max: ',maxval(array)
      call grib_release(gid, stat)
   end do
   ecmwf%phi_lev   = ecmwf%phi_lev*g_wmo

   ! convert from pressure to log pressure
   preproc_prtm%lnsp = log(preproc_prtm%lnsp)

   deallocate(val)

   ! GFS has no snow mask, so use snow depth instead. 0.1m threshold arbitrary
   where(preproc_prtm%snow_depth .gt. 0.1) preproc_prtm%snow_albedo = 0.98
   deallocate(in_data)

   ! close ECMWF file
   call PBCLOSE(fu,stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to close file.')

   ! Refactor all the GFS levels so that below-surface contributions are removed.
   call interpolate_gfs_levels(preproc_prtm, preproc_dims, ecmwf, nwp_flag, verbose)

end subroutine read_gfs_grib_for_preproc_structures


! This function transforms the GFS fixed pressure levels into surface-relative
! levels that are more similar to those from ECMWF. Needed to prevent below-
! surface contributions to the transmission and radiances.
subroutine interpolate_gfs_levels(preproc_prtm, preproc_dims, ecmwf, nwp_flag, verbose)

   use preproc_constants_m
   use preproc_structures_m

   implicit none

   type(preproc_prtm_t), intent(inout) :: preproc_prtm
   type(preproc_dims_t), intent(in)    :: preproc_dims
   type(ecmwf_t),    intent(inout)     :: ecmwf
   integer,          intent(in)        :: nwp_flag
   logical,              intent(in)    :: verbose

   integer          :: sh(3), lb(3), ub(3), i_0, i_1, j_0, j_1, nl
   integer          :: i, j, l, stoplev

   real(dreal)      :: surfp, interp
   real,allocatable :: p(:), t(:), q(:), o(:), pl(:)
   logical          :: stopper


   if (verbose)write(*,*)">>>>>>Interpolate_gfs_levels>>>>>>"
   call ecmwf_abvec_init(ecmwf, nwp_flag)
   call compute_geopot_coordinate_gfs(preproc_prtm, preproc_dims, ecmwf)
   
   if (verbose) write(*,*)"<<<<<<Interpolate_gfs_levels<<<<<<"

end subroutine interpolate_gfs_levels