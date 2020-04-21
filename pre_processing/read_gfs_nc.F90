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
! 2017/07/20, SP: Initial version, cloned from read_ecmwf_nc.F90
!
! Bugs:
! - you need to be careful with parameter naming as the variable names are not
!   consistent across files for example the variable name could be lnsp or LNSP
!-------------------------------------------------------------------------------

subroutine read_gfs_nc(ecmwf_path, ecmwf, preproc_dims, preproc_geoloc, &
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
   integer(lint),     parameter           :: BUFFER = 3000000

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
   real(sreal)   :: dummy3d_2(ecmwf%xdim,ecmwf%ydim,ecmwf%kdim)

   integer(lint), dimension(31)           :: gfs_levlist

   gfs_levlist = (/1,2,3,5,7,10,20,30,50,70,100,150,200,250,300,350, &
                   400,450,500,550,600,650,700,750,800,850,900,925,950,975,1000/)

   do i = 1, 31
      preproc_prtm%pressure(:,:,i) = gfs_levlist(i)
   end do

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
   call ncdf_open(fid, ecmwf_path, 'read_gfs_nc()')
   if (nf90_inquire(fid, nVariables=nvar) .ne. 0) &
        call h_e_e('nc', 'NF INQ failed.')

   allocate(old_data(BUFFER))
   allocate(new_data(BUFFER))

   ! loop over variables
   do ivar = 1, nvar

      ! determine if field should be read
      if (nf90_inquire_variable(fid, ivar, name) .ne. 0) &
           call h_e_e('nc', 'NF VAR INQUIRE failed.')
      select case (name)
      case('Z', 'z')
         three_d = .true.
         array3d => preproc_prtm%phi_lev
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
      case('p_trop', 'P_trop', 'P_Trop', 'P_TROP')
         three_d = .false.
         array2d => preproc_prtm%trop_p
      case default
         cycle
      end select
      if (nf90_inquire_variable(fid, ivar, name) .ne. 0) &
           call h_e_e('nc', 'NF VAR INQUIRE failed.')

      if (three_d) then
         call ncdf_read_array(fid, name, dummy3d_2, verbose)
         do k = 1, ecmwf%kdim
            old_len = n
            old_data(1:n) = reshape(real(dummy3d_2(:,:,k), kind=8), [n])

            new_len = BUFFER

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
         end do
         if (verbose) print*, trim(name), ') Min: ', minval(array3d), &
              ', Max: ', maxval(array3d)
      else
         call ncdf_read_array(fid, name, dummy2d, verbose)
         old_len = n
         old_data(1:n) = reshape(real(dummy2d, kind=8), [n])

         new_len = BUFFER

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
         if (verbose) print*, trim(name), ') Min: ', minval(array2d), &
              ', Max: ', maxval(array2d)
      end if
   end do

   ! GFS pressures are in Pa rather than hPa
   preproc_prtm%trop_p = preproc_prtm%trop_p * pa2hpa

   preproc_prtm%phi_lev   = preproc_prtm%phi_lev*g_wmo

   ! GFS provides humidity as relative humidity (%), we need specific humidity
   ! (kg/kg). This will convert from one to the other.
   call conv_rh_q(preproc_prtm%spec_hum, preproc_prtm%temperature, &
                  preproc_prtm%pressure, verbose)

   ! GFS has no snow mask, so use snow depth instead. 0.1m threshold arbitrary
   where(preproc_prtm%snow_depth .gt. 0.1) preproc_prtm%snow_albedo = 0.98

   deallocate(old_data)
   deallocate(new_data)

   ! Refactor all the GFS levels so that below-surface contributions are removed.
   call sort_gfs_levels(preproc_prtm, verbose)

   call ncdf_close(fid, 'read_gfs_nc()')

end subroutine read_gfs_nc
