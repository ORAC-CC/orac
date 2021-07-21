!-------------------------------------------------------------------------------
! Name: read_era5_jasmin.F90
!
! Purpose:
! Deals with reading data from the JASMIN ERA5 archive, which has each variable
! in a separate file. Therefore, we have to construct a lot of filenames first
! (see: determine_jasmin_filenames_era5) before we can read the data.
!
!-------------------------------------------------------------------------------

subroutine read_era5_jasmin_wind_nc(ecmwf, nwp_fnames, idx, nwp_flag)

   use preproc_constants_m
   use preproc_structures_m, only: preproc_nwp_fnames_t

   implicit none

   type(ecmwf_t),    intent(inout)        :: ecmwf
   type(preproc_nwp_fnames_t), intent(in) :: nwp_fnames
   integer,          intent(in)           :: idx
   integer,          intent(in)           :: nwp_flag

   call ecmwf_wind_init(ecmwf)
   call ecmwf_abvec_init(ecmwf)

   ! loop over given files
   call read_ecmwf_wind_nc_file(nwp_fnames%ci_f(idx), ecmwf)
   call read_ecmwf_wind_nc_file(nwp_fnames%u10_f(idx), ecmwf)
   call read_ecmwf_wind_nc_file(nwp_fnames%v10_f(idx), ecmwf)
   call read_ecmwf_wind_nc_file(nwp_fnames%skt_f(idx), ecmwf)
   call read_ecmwf_wind_nc_file(nwp_fnames%sd_f(idx), ecmwf)

end subroutine read_era5_jasmin_wind_nc


subroutine read_era5_jasmin_nc(nwp_fnames, idx, ecmwf, preproc_dims, preproc_geoloc, &
     preproc_prtm, verbose, nwp_flag)

    use orac_ncdf_m
    use preproc_constants_m
    use preproc_structures_m

    implicit none

    type(preproc_nwp_fnames_t), intent(in)    :: nwp_fnames
    integer,                    intent(in)    :: idx
    type(ecmwf_t),              intent(in)    :: ecmwf
    type(preproc_dims_t),       intent(in)    :: preproc_dims
    type(preproc_geoloc_t),     intent(in)    :: preproc_geoloc
    type(preproc_prtm_t),       intent(inout) :: preproc_prtm
    logical,                    intent(in)    :: verbose
    integer,                    intent(in)    :: nwp_flag

    integer(lint),     external            :: INTIN, INTOUT, INTF
    integer(lint),     parameter           :: BUFFER = 2000000

    integer(lint),            dimension(1) :: intv, old_grib, new_grib
    real(dreal)                            :: grid(2), area(4)
    character(len=20),        dimension(1) :: charv

    real(sreal),       pointer             :: array2d(:,:), array3d(:,:,:)
    integer(4)                             :: n, ni, nj, i, j, k, ivar
    integer(4)                             :: fid, nvar
    integer(4)                             :: old_len, new_len
    character(len=20)                      :: name
    logical                                :: three_d
    real(sreal)   :: dummy2d(ecmwf%xdim,ecmwf%ydim)
    real(sreal)   :: dummy3d(ecmwf%xdim,ecmwf%ydim,ecmwf%kdim,1)
    real(sreal)   :: dummy3d_2(ecmwf%xdim,ecmwf%ydim,ecmwf%kdim)

    ! open file
    ! Do temperature on model levels
    array3d => preproc_prtm%temperature
    call ncdf_open(fid, nwp_fnames%t_f(idx), 'read_era5_jasmin_nc_t()')
    call ncdf_read_array(fid, 't', dummy3d_2)
    call preproc_3d_var(dummy3d_2, ecmwf, preproc_dims, preproc_geoloc, array3d)
    if (verbose) print*, 'T)     Min: ', minval(array3d), ', Max: ', maxval(array3d)
    call ncdf_close(fid, 'read_era5_jasmin_t()')
    
    ! Do specific humidity on model levels
    array3d => preproc_prtm%spec_hum
    call ncdf_open(fid, nwp_fnames%q_f(idx), 'read_era5_jasmin_nc_q()')
    call ncdf_read_array(fid, 'q', dummy3d_2)
    call preproc_3d_var(dummy3d_2, ecmwf, preproc_dims, preproc_geoloc, array3d)
    if (verbose) print*, 'Q)     Min: ', minval(array3d), ', Max: ', maxval(array3d)
    call ncdf_close(fid, 'read_era5_jasmin_q()')
    
    ! Do ozone on model levels
    array3d => preproc_prtm%ozone
    call ncdf_open(fid, nwp_fnames%o3_f(idx), 'read_era5_jasmin_nc_o3()')
    call ncdf_read_array(fid, 'o3', dummy3d_2)
    call preproc_3d_var(dummy3d_2, ecmwf, preproc_dims, preproc_geoloc, array3d)
    if (verbose) print*, 'O3)    Min: ', minval(array3d), ', Max: ', maxval(array3d)
    call ncdf_close(fid, 'read_era5_jasmin_o3()')
    
    ! Do logarithm of surface pressure
    array2d => preproc_prtm%lnsp
    call ncdf_open(fid, nwp_fnames%lnsp_f(idx), 'read_era5_jasmin_nc_lnsp()')
    call ncdf_read_array(fid, 'lnsp', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'LNSP)  Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_lnsp()')
    
    ! Do geopotential
    array2d => preproc_prtm%geopot
    call ncdf_open(fid, nwp_fnames%z_f(idx), 'read_era5_jasmin_nc_z()')
    call ncdf_read_array(fid, 'z', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'GEOP)  Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_z()')
    
    ! Do sea ice fraction
    array2d => preproc_prtm%sea_ice_cover
    call ncdf_open(fid, nwp_fnames%ci_f(idx), 'read_era5_jasmin_nc()')
    call ncdf_read_array(fid, 'siconc', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'CI)    Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin()')
    
    ! Do snow albedo
    array2d => preproc_prtm%snow_albedo
    call ncdf_open(fid, nwp_fnames%asn_f(idx), 'read_era5_jasmin_nc_asn()')
    call ncdf_read_array(fid, 'asn', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'ASN)   Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_asn()')
    
    ! Do total column water vapour
    array2d => preproc_prtm%totcolwv
    call ncdf_open(fid, nwp_fnames%tcwv_f(idx), 'read_era5_jasmin_nc_tcwv()')
    call ncdf_read_array(fid, 'tcwv', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'TCWV)  Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_tcwv()')
    
    ! Do snow depth
    array2d => preproc_prtm%snow_depth
    call ncdf_open(fid, nwp_fnames%sd_f(idx), 'read_era5_jasmin_nc_sd()')
    call ncdf_read_array(fid, 'sd', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'SD)    Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_sd()')
    
    ! Do U-component of 10m wind
    array2d => preproc_prtm%u10
    call ncdf_open(fid, nwp_fnames%u10_f(idx), 'read_era5_jasmin_nc_10u()')
    call ncdf_read_array(fid, 'u10', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'U10)    Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_nc_10u()')
    
    ! Do V-component of 10m wind
    array2d => preproc_prtm%v10
    call ncdf_open(fid, nwp_fnames%v10_f(idx), 'read_era5_jasmin_nc_10v()')
    call ncdf_read_array(fid, 'v10', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'V10)    Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_nc_10v()')
    
    ! Do 2m temperature
    array2d => preproc_prtm%temp2
    call ncdf_open(fid, nwp_fnames%t2_f(idx), 'read_era5_jasmin_nc_t2()')
    call ncdf_read_array(fid, 't2m', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'T2)     Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_nc_t2()')
    
    ! Do skin temperature
    array2d => preproc_prtm%skin_temp
    call ncdf_open(fid, nwp_fnames%skt_f(idx), 'read_era5_jasmin_nc_skt()')
    call ncdf_read_array(fid, 'skt', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'SKT)     Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_nc_skt()')
    
    ! Do sea surface temperature
    array2d => preproc_prtm%sst
    call ncdf_open(fid, nwp_fnames%sstk_f(idx), 'read_era5_jasmin_nc_sst()')
    call ncdf_read_array(fid, 'sst', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'SST)     Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_nc_sst()')
    
#ifdef INCLUDE_SATWX
    ! Do convective available potential energy
    array2d => preproc_prtm%cape
    call ncdf_open(fid, nwp_fnames%cape_f(idx), 'read_era5_jasmin_nc_cape()')
    call ncdf_read_array(fid, 'cape', dummy2d)
    call preproc_2d_var(dummy2d, ecmwf, preproc_dims, preproc_geoloc, array2d)
    if (verbose) print*, 'CAPE)    Min: ', minval(array2d), ', Max: ', maxval(array2d)
    call ncdf_close(fid, 'read_era5_jasmin_nc_cape()')
#endif
end subroutine read_era5_jasmin_nc

subroutine preproc_3d_var(dummy_in, ecmwf, preproc_dims, preproc_geoloc, out_arr3d)

    use preproc_constants_m
    use preproc_structures_m

    real(sreal), dimension(:,:,:), intent(in)    :: dummy_in
    type(ecmwf_t),                 intent(in)    :: ecmwf
    type(preproc_dims_t),          intent(in)    :: preproc_dims
    type(preproc_geoloc_t),        intent(in)    :: preproc_geoloc
    real(sreal), pointer,          intent(inout) :: out_arr3d(:,:, :)

    integer(lint),     dimension(1)        :: intv, old_grib, new_grib
    integer(lint),     external            :: INTIN, INTOUT, INTF
    integer(lint),     parameter           :: BUFFER = 2000000
    character(len=20), dimension(1)        :: charv
    real(dreal)                            :: grid(2), area(4)
    real(dreal), allocatable, dimension(:) :: old_data, new_data

    integer(4)                             :: n, ni, nj, i, j, k, ivar
    integer(4)                             :: old_len, new_len

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

    allocate(old_data(BUFFER))
    allocate(new_data(BUFFER))
   
    do k = 1, ecmwf%kdim
        old_len = n
        old_data(1:n) = reshape(real(dummy_in(:,:,k), kind=8), [n])

        new_len = BUFFER

        if (INTF(old_grib, old_len, old_data, new_grib, new_len, new_data).ne.0)&
            call h_e_e('nc', 'INTF failed.')
        
        if (new_len .ne. ni*nj) print*, '3D Interpolation grid wrong, '

        ! copy data into preprocessing grid
        do j = 1, nj, 2
            do i = 1, ni, 2
                out_arr3d(preproc_dims%min_lon+i/2, preproc_dims%min_lat+(nj-j)/2, k) = real(new_data(i+(j-1)*ni), kind=4)
            end do
        end do
    end do   

    deallocate(old_data)
    deallocate(new_data)

end subroutine preproc_3d_var


subroutine preproc_2d_var(dummy_in, ecmwf, preproc_dims, preproc_geoloc, out_arr2d)

    use preproc_constants_m
    use preproc_structures_m

    real(sreal), dimension(:,:),   intent(in)    :: dummy_in
    type(ecmwf_t),                 intent(in)    :: ecmwf
    type(preproc_dims_t),          intent(in)    :: preproc_dims
    type(preproc_geoloc_t),        intent(in)    :: preproc_geoloc
    real(sreal), pointer,          intent(inout) :: out_arr2d(:,:)

    integer(lint),     dimension(1)        :: intv, old_grib, new_grib
    integer(lint),     external            :: INTIN, INTOUT, INTF
    integer(lint),     parameter           :: BUFFER = 2000000
    character(len=20), dimension(1)        :: charv
    real(dreal)                            :: grid(2), area(4)
    real(dreal), allocatable, dimension(:) :: old_data, new_data

    integer(4)                             :: n, ni, nj, i, j, ivar
    integer(4)                             :: old_len, new_len

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

    allocate(old_data(BUFFER))
    allocate(new_data(BUFFER))

    old_len = n
    old_data(1:n) = reshape(real(dummy_in(:,:), kind=8), [n])

    new_len = BUFFER

    if (INTF(old_grib, old_len, old_data, new_grib, new_len, new_data).ne.0)&
        call h_e_e('nc', 'INTF failed.')
    
    if (new_len .ne. ni*nj) print*, '2D Interpolation grid wrong, '

    ! copy data into preprocessing grid
         do j = 1, nj, 2
            do i = 1, ni, 2
               out_arr2d(preproc_dims%min_lon+i/2, preproc_dims%min_lat+(nj-j)/2) = real(new_data(i+(j-1)*ni), kind=4)
            end do
         end do

    deallocate(old_data)
    deallocate(new_data)

end subroutine preproc_2d_var
