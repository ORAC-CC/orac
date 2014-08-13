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
! ecmwf_path string in   Full path to ECMWF NCDF file to read.
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

subroutine read_ecmwf_wind_grib(ecmwf_path, ecmwf)

   use grib_api
   use preproc_constants

   implicit none

   character(len=*), intent(in)    :: ecmwf_path
   type(ecmwf_s),    intent(inout) :: ecmwf

   real, allocatable, dimension(:) :: pv,lat,lon,val
   integer                         :: fid,gid,stat
   integer                         :: PVPresent,PLPresent
   integer                         :: i,n,ni,nj,nk,npv,ni_,nj_
   integer                         :: param,level,nlevels

   ! open file
   call grib_open_file(fid,ecmwf_path,'r',stat)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error opening GRIB field.'
   call grib_new_from_file(fid,gid,stat)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting GRIB_ID.'
   if (gid .eq. GRIB_END_OF_FILE) stop 'READ_ECMWF_WIND: Empty GRIB file.'

   ! ensure it contains the expected fields
   call grib_get(gid,'PVPresent',PVPresent)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting PVPresent.'
   if (PVPresent .eq. 0) &
        stop 'READ_ECMWF_WIND: Incorrect file format. Check ECMWF_FLAG.'
   call grib_get(gid,'PLPresent',PLPresent)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting PVPresent.'
   if (PLPresent .eq. 1) &
        stop 'READ_ECMWF_WIND: Incorrect file formatting. Check ECMWF_FLAG.'

   ! fetch vertical coordinate
   call grib_get_size(gid,'pv',npv,stat)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error checking PV.'
   allocate(pv(npv))
   call grib_get(gid,'pv',pv,stat)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting PV.'
   nk=npv/2-1

   ! read dimensions
   call grib_get(gid,'Ni',ni,stat)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting Ni.'
   call grib_get(gid,'Nj',nj,stat)
   if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting Nj.'
   n=ni*nj

   ! allocate temporary arrays
   allocate(lon(n))
   allocate(lat(n))
   allocate(val(n))

   ! allocate permanent arrays
   allocate(ecmwf%lon(ni))
   allocate(ecmwf%lat(nj))
   allocate(ecmwf%avec(nk+1))
   allocate(ecmwf%bvec(nk+1))
   allocate(ecmwf%u10(ni,nj))
   allocate(ecmwf%v10(ni,nj))

   nlevels=0
   do while (stat .ne. GRIB_END_OF_FILE)
      ! check for consistent dimensions
      call grib_get(gid,'Ni',ni_,stat)
      if (stat .ne. 0 .or. ni_ .ne. ni) stop 'READ_ECMWF_WIND: Error with Ni.'
      call grib_get(gid,'Nj',nj_,stat)
      if (stat .ne. 0 .or. nj_ .ne. nj) stop 'READ_ECMWF_WIND: Error with Nj.'
      call grib_get(gid,'level',level,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting level.'

      ! count level number
      if (level .gt. nlevels) nlevels=level

      ! if wind field, read and output
      call grib_get(gid,'parameter',param,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error getting parameter.'
      select case (param)
      case(165)
         ! 10m zonal wind component, latitude, and longitude
         call grib_get_data(gid,lat,lon,val,stat)
         if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error reading U10.'

         ecmwf%u10=reshape(val, (/ni,nj/))
         ecmwf%lon=lon(1:ni)
         do i=1,n,ni
            ecmwf%lat(1+i/ni)=lat(i)
         end do
      case(166)
         ! 10 m meriodional wind component
         call grib_get(gid,'values',val,stat)
         if (stat .ne. 0) stop 'READ_ECMWF_WIND: Error reading V10.'

         ecmwf%v10=reshape(val, (/ni,nj/))
      end select

      ! advance file position
      call grib_release(gid)
      call grib_new_from_file(fid,gid,stat)
   end do

   call grib_close_file(fid)

   ! set ECMWF dimensions
   if (nk .ne. nlevels) stop 'READ_ECMWF_WIND: Inconsistent vertical levels.'
   ecmwf%xdim=ni
   ecmwf%ydim=nj
   ecmwf%kdim=nk
   ecmwf%avec=pv(1:nk+1)
   ecmwf%bvec=pv(nk+2:)

   ! clean-up
   deallocate(pv)
   deallocate(lat)
   deallocate(lon)
   deallocate(val)

end subroutine read_ecmwf_wind_grib
