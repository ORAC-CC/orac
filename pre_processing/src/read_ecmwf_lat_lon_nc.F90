! Name: read_ecmwf_lat_lon_nc.F90
!
!
! Purpose:
! Read ECMWF ERA Interim grib lat/lon grid.
! 
! Description and Algorithm details:
! 1) Read dimension lengths.
! 2) Read through all variables in search of latitude and longitude. Read them.
! 3) Copy information into structure.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in   Full path to ECMWF NCDF file to read.
! ecmwf_dims struct in  Structure summarising dimensions of ECMWF files.
! ecmwf_2d   struct out Structure containing 2-D ECMWF fields.
!
! History:
! 2012/02/22: MJ produces code which reads rlat/lon grid from ERA INTERIM
!                GRIB files
! 2012/10/22: CP modified file for netcdf
! 2013/10/22: AP Tidying. Removed allocated arrays as unnecessary. Added
!                nc_close. Removed unused variables.
! 2013/11/06: GM wo was unititialized, set to 0
! 2014/02/06: AP Further tidying. Removed unnecessary components.
!
! $Id$
!
! Bugs:
! none known
!

subroutine read_ecmwf_lat_lon_nc(ecmwf_path,ecmwf_dims,ecmwf_2d)

   implicit none

   character(len=pathlength) :: ecmwf_path
   type(ecmwf_dims_s)        :: ecmwf_dims
   type(ecmwf_2d_s)          :: ecmwf_2d

   real(kind=sreal), allocatable, dimension(:) :: lon,lat
   character(len=varlength)  :: available_names

   integer(kind=lint)        :: ncid,ierr,verb
   integer(kind=lint)        :: ivar,ndim,nvar,nattr,idim,jdim

   verb=0
   
   call nc_open(ncid,ecmwf_path)
   ierr=NF90_INQUIRE(ncid,ndim,nvar,nattr)

   do ivar=1,nvar
      ierr=NF90_INQUIRE_VARIABLE(ncid, ivar, available_names)

      if (available_names .eq. 'longitude') then
         allocate(lon(ecmwf_dims%xdim))
         lon=real_fill_value
         call nc_read_array(ncid,'longitude',lon,verb) 
         !Latitude is counted from northpole to southpole 90 -> -90

         !Longitude is eastward from 0 to 358.875
      endif

      if (available_names .eq. 'latitude') then
         allocate(lat(ecmwf_dims%ydim))
         lat=real_fill_value
         call nc_read_array(ncid,'latitude',lat,verb)  
      endif
   enddo

   do jdim=1,ecmwf_dims%xdim
      ecmwf_2d%longitude(jdim,:)=lon(jdim)
   enddo
   do idim=1,ecmwf_dims%ydim
      ecmwf_2d%latitude(:,idim)=lat(idim)
   enddo

   ierr=NF90_CLOSE(ncid)
   if (allocated(lon)) deallocate(lon)
   if (allocated(lat)) deallocate(lat)

end subroutine read_ecmwf_lat_lon_nc
