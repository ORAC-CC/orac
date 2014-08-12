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
! ecmwf_path string in   Full path to a ECMWF NCDF file to read.
! ecmwf2path string in   "
! ecmwf3path string in   "
! ecmwf      struct both Structure summarising contents of ECMWF files.
!
! History:
! 2014/05/07, AP: First version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_badc(ecmwf_path, ecmwf2path, ecmwf3path, ecmwf)

   use grib_api
   use preproc_constants

   implicit none

   character(len=*), intent(in)    :: ecmwf_path
   character(len=*), intent(in)    :: ecmwf2path, ecmwf3path
   type(ecmwf_s),    intent(inout) :: ecmwf

   integer                         :: i,fid,gid,stat
   integer                         :: PVPresent,PLPresent,level
   integer                         :: npv,nk,nlevels
   real,              allocatable  :: pv(:)
   character(len=pathlength)       :: paths(2)
   paths=(/ ecmwf2path, ecmwf3path /)

   ! ggas NCDF file, giving U10,V10,lat,lon
   call read_ecmwf_wind_file(ecmwf_path,ecmwf)

   ! loop over GRIB files for vertical coordinate
   do i=1,2
      call grib_open_file(fid,paths(i),'r',stat)
      if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error opening GRIB field.'
      call grib_new_from_file(fid,gid,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting GRIB_ID.'
      if (gid .eq. GRIB_END_OF_FILE) stop 'READ_ECMWF_WIND: Empty GRIB file.'

      ! ensure it contains the expected fields
      call grib_get(gid,'PVPresent',PVPresent)
      if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting PVPresent.'
      call grib_get(gid,'PLPresent',PLPresent)
      if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting PVPresent.'
      if (PVPresent .eq. 1 .and. PLPresent .eq. 1) then
         ! fetch vertical coordinate
         call grib_get_size(gid,'pv',npv,stat)
         if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error checking PV.'
         allocate(pv(npv))
         call grib_get(gid,'pv',pv,stat)
         if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting PV.'
         nk=npv/2-1

         nlevels=0
         do while (stat .ne. GRIB_END_OF_FILE)
            call grib_get(gid,'level',level,stat)
            if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting level.'
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
        stop 'READ_ECMWF_WIND: Could not find vertical field.'

   ! set ECMWF dimensions
   if (nk .ne. nlevels) stop 'READ_ECMWF_WIND: Inconsistent vertical levels.'
   ecmwf%kdim=nk
   allocate(ecmwf%avec(nk+1))
   allocate(ecmwf%bvec(nk+1))
   ecmwf%avec=pv(1:nk+1)
   ecmwf%bvec=pv(nk+2:)

   ! clean-up
   deallocate(pv)

end subroutine read_ecmwf_wind_badc
