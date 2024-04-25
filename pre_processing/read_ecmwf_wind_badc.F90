!-------------------------------------------------------------------------------
! Name: read_ecmwf_wind_badc.F90
!
! Purpose:
! Read surface wind components and lat/lon from ECMWF file. Intended for use
! with BADC files in NCDF and GRIB formats (filename ggasYYYYMMDDHH00.nc and
! [sgam|spam]YYYYMMDDHH00.grb). Successor to read_ecmwf_dimensions_nc.F90.
!
! Description and Algorithm details:
! 1) Loop through given files.
! 2) If NCDF file, process using read_ecmwf_wind_file.
! 3) Otherwise, open GRIB file and check for expected grid (reduced gaussian).
! 4) Read vertical dimension.
! 5) Close file.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! nwp_path string in   Full path to a ECMWF NCDF file to read.
! ecmwf2path string in   "
! ecmwf3path string in   "
! ecmwf      struct both Structure summarising contents of ECMWF files.
!
! History:
! 2014/05/07, AP: First version.
! 2016/01/29, GM: read_ecmwf_wind_file() expects values to be initilized, must
!    call ecmwf_wind_init().
! 2016/01/27, GM: Check and trim filename length for grib_open_file().
! 2016/02/02, OS: Now reads into HR ERA structure if flag is set.
! 2016/04/04, SP: Add option to process ECMWF forecast in single NetCDF4 file
!    Note: This should work with either the OPER or FCST streams from ECMWF.
! 2016/04/26, AP: Moved refences to nwp_flag and high_res up a level.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_badc(nwp_path, ecmwf2path, ecmwf3path, ecmwf)

   use grib_api
   use preproc_constants_m

   implicit none

   character(len=*), intent(in)    :: nwp_path
   character(len=*), intent(in)    :: ecmwf2path, ecmwf3path
   type(ecmwf_t),    intent(inout) :: ecmwf

   integer                         :: i,fid,gid,stat
   integer                         :: PVPresent,PLPresent,level
   integer                         :: npv,nk,nlevels
   real, allocatable               :: pv(:)
   character(len=1024)             :: paths(2)

   if (len_trim(ecmwf2path) .gt. 1024 .or. len_trim(ecmwf3path) .gt. 1024) &
      call h_e_e('wind_badc', 'Filename argument strings ecmwf2path and ' // &
           'ecmwf3path are too long.  They must be limited to a length of ' // &
           '1024 due to a bug in grib_api.')

   paths=(/ trim(ecmwf2path), trim(ecmwf3path) /)

   call ecmwf_wind_init(ecmwf)

   ! ggas NCDF file, giving U10,V10,lat,lon
   call read_ecmwf_wind_nc_file(nwp_path,ecmwf)

   ! loop over GRIB files for vertical coordinate
   do i = 1, 2
      call grib_open_file(fid,paths(i),'r',stat)
      if (stat .ne. 0) call h_e_e('wind_badc', 'Error opening GRIB file. ' // &
           trim(paths(i)))
      call grib_new_from_file(fid,gid,stat)
      if (stat .ne. 0) call h_e_e('wind_badc', 'Error getting GRIB_ID.')
      if (gid .eq. GRIB_END_OF_FILE) call h_e_e('wind_badc', 'Empty GRIB file.')

      ! ensure it contains the expected fields
      call grib_get(gid,'PVPresent',PVPresent)
      if (stat .ne. 0) call h_e_e('wind_badc', 'Error getting PVPresent.')
      call grib_get(gid,'PLPresent',PLPresent)
      if (stat .ne. 0) call h_e_e('wind_badc', 'Error getting PVPresent.')
      if (PVPresent .eq. 1 .and. PLPresent .eq. 1) then
         ! fetch vertical coordinate
         call grib_get_size(gid,'pv',npv,stat)
         if (stat .ne. 0) call h_e_e('wind_badc', 'Error checking PV.')
         allocate(pv(npv))
         call grib_get(gid,'pv',pv,stat)
         if (stat .ne. 0) call h_e_e('wind_badc', 'Error getting PV.')
         nk=npv/2-1

         nlevels=0
         do while (stat .ne. GRIB_END_OF_FILE)
            call grib_get(gid,'level',level,stat)
            if (stat .ne. 0) call h_e_e('wind_badc', 'Error getting level.')
            if (level .gt. nlevels) nlevels=level

            ! advance file position
            call grib_release(gid)
            call grib_new_from_file(fid,gid,stat)
         end do
      else
         call grib_release(gid)
      end if

      call grib_close_file(fid)
   end do

   if (.not. allocated(pv)) &
        call h_e_e('wind_badc', 'Could not find vertical field.')

   ! set ECMWF dimensions
   if (nk .ne. nlevels) call h_e_e('wind_badc', 'Inconsistent vertical levels.')
   ecmwf%kdim=nk
   allocate(ecmwf%avec(nk+1))
   allocate(ecmwf%bvec(nk+1))
   ecmwf%avec=pv(1:nk+1)
   ecmwf%bvec=pv(nk+2:)

   ! clean-up
   deallocate(pv)

end subroutine read_ecmwf_wind_badc
