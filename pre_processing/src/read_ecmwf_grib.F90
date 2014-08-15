!-------------------------------------------------------------------------------
! Name: read_ecmwf_grib.F90
!
! Purpose:
! Read ECMWF data from a GRIB file, having interpolated it onto the
! preprocessing grid. Replaces read_ecmwf.F90.
!
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
! Description and Algorithm details:
! 1) Open file. Create scratch file.
! 2) Set output grid to limits and spacing of preprocessing grid.
! 3) Loop over fields in GRIB file.
!    a) Read data field.
!    b) Interpolate it with INTF.
!    c) Write new GRIB field to scratch file.
!    d) Read scratch file with GRIB API routines. Check for reduced Gaus grid.
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
!
! $Id$
!
! Bugs:
! - If you're having problems with INTF, set the environment variable JDCNDBG=1
! for additional debugging output.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_grib(ecmwf_file,preproc_dims,preproc_geoloc, &
     preproc_prtm,verbose)

   use grib_api
   use preproc_constants
   use preproc_structures

   implicit none

   character(len=pathlength), intent(in)    :: ecmwf_file
   type(preproc_dims_s),      intent(inout) :: preproc_dims
   type(preproc_geoloc_s),    intent(inout) :: preproc_geoloc
   type(preproc_prtm_s),      intent(inout) :: preproc_prtm
   logical,                   intent(in)    :: verbose

   integer(lint),             parameter     :: BUFFER = 2000000
   integer(lint),             external      :: INTIN,INTOUT,INTF

   integer(lint)                            :: fu,stat,lun,nbytes
   integer(lint)                            :: in_words,out_words
   logical                                  :: lun_exists, lun_used
   integer(lint),         dimension(BUFFER) :: in_data,out_data
   integer(lint)                            :: iblank(4)
   real(dreal)                              :: zni(1),zno(1),grid(2),area(4)
   character(len=20)                        :: charv(1)
   character(len=filelength)                :: name

   integer(lint)                            :: fid,gid,level,param
   integer(lint)                            :: n,ni,nj,i,j,plpresent
   real(sreal), dimension(:),   allocatable :: pl,val
   real(sreal), dimension(:,:), pointer     :: array

   ! open the ECMWF file
   call PBOPEN(fu, ecmwf_file, 'r', stat)
   if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Failed to read file.'

   ! find an available file unit for scratch file
   do lun=97,1,-1
      inquire(unit=lun, exist=lun_exists, opened=lun_used)
      if (lun_exists .and. (.not. lun_used)) exit
   end do

   ! select appropriate grid definition
   charv(1)='grib'
   if (INTIN('form',iblank,grid,charv).ne.0) &
        stop 'READ_ECMWF_GRIB: INTIN form failed.'
   if (INTOUT('form',iblank,grid,charv).ne.0) &
        stop 'READ_ECMWF_GRIB: INTOUT form failed.'

   ! input details of new grid (see note in header)
   grid(1) = 0.5 / preproc_dims%dellon
   grid(2) = 0.5 / preproc_dims%dellat
   if (INTOUT('grid',iblank,grid,charv).ne.0) stop 'INTOUT grid failed.'
   area(1) = preproc_geoloc%latitude(preproc_dims%max_lat) + 0.01*grid(2)
   area(2) = preproc_geoloc%longitude(preproc_dims%min_lon) + 0.01*grid(1)
   area(3) = preproc_geoloc%latitude(preproc_dims%min_lat) + 0.01*grid(2)
   area(4) = preproc_geoloc%longitude(preproc_dims%max_lon) + 0.01*grid(1)
   if (INTOUT('area',iblank,area,charv).ne.0) stop 'INTOUT area failed.'

   ! interpolate ECMWF products to preproc grid
   do
      ! read GRIB data field
      call PBGRIB(fu, in_data, BUFFER*lint, nbytes, stat)
      if (stat .eq. -1) exit
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Failure to read product.'

      ! interpolate GRIB field (into another GRIB field)
      in_words = nbytes / lint
      out_words = BUFFER
      if (INTF(in_data,in_words,zni,out_data,out_words,zno).ne.0) &
           stop 'INTF failed. Check if 1/dellon 1/dellat are muliples of 0.001.'

      ! open a scratch file to store the interpolated product
      open(unit=lun,status='SCRATCH',iostat=stat,form='UNFORMATTED')
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Failed to open scratch file.'
      inquire(unit=lun,name=name)

      ! write interpolated field to scratch file
      write(lun) out_data(1:out_words)

      ! read scratch file
      call grib_open_file(fid,name,'r',stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error opening GRIB field.'
      call grib_new_from_file(fid,gid,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error getting GRIB_ID.'

      call grib_get(gid,'parameter',param,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error getting parameter #.'
      call grib_get(gid,'level',level,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error getting level #.'
      call grib_get(gid,'Nj',nj,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error getting nj.'

      ! check if this is a reduced Gaussian grid
      call grib_get(gid,'PLPresent',plpresent,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error getting PLPresent.'
      if (plpresent .eq. 1) then
         ! determine total number of points from PL array
         allocate(pl(nj))
         call grib_get(gid,'pl',pl,stat)
         if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error getting PL.'
         n=sum(pl)
         deallocate(pl)
      else
         ! regular grid
         call grib_get(gid,'Ni',ni,stat)
         if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error getting level #.'
         n=ni*nj
      end if
      if (.not.allocated(val)) allocate(val(n))

      call grib_get(gid,'values',val,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error reading data.'
      call grib_release(gid,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error releasing GRIB_ID.'
      call grib_close_file(fid,stat)
      if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Error closing GRIB field.'

      ! close scratch file
      close(unit=lun)

      ! select correct output array
      select case (param)
      case(130)
         array => preproc_prtm%temperature( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat,level)
      case(133)
         array => preproc_prtm%spec_hum( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat,level)
      case(203)
         array => preproc_prtm%ozone( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat,level)
      case(129)
         array => preproc_prtm%geopot( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(152)
         array => preproc_prtm%lnsp( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(31)
         array => preproc_prtm%sea_ice_cover( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(32)
         array => preproc_prtm%snow_albedo( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(34)
         array => preproc_prtm%sst( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(137)
         array => preproc_prtm%totcolwv( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(141)
         array => preproc_prtm%snow_depth( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(165)
         array => preproc_prtm%u10( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(166)
         array => preproc_prtm%v10( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(167)
         array => preproc_prtm%temp2( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(172)
         array => preproc_prtm%land_sea_mask( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(235)
         array => preproc_prtm%skin_temp( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case default
         cycle
      end select

      ! copy data into preprocessing grid
      ! a) we're inverting the y axis as ECMWF write 90->-90
      ! b) we're only taking every other point to read cell centres
      ! c) there will be an odd # of lats as it contains 0N but an even # of
      !    lons as 180 wraps to -180
      do j=1,nj,2
         do i=1,ni,2
            array(1+i/2,1+(nj-j)/2) = val(i+(j-1)*ni)
         end do
      end do
      where (array .eq. 9999) array = sreal_fill_value
      if (verbose) print*,param,') Min: ',minval(array), &
              ', Max: ',maxval(array)
   end do

   deallocate(val)

   ! close ECMWF file
   call PBCLOSE(fu,stat)
   if (stat .ne. 0) stop 'READ_ECMWF_GRIB: Failed to close file.'

end subroutine read_ecmwf_grib
