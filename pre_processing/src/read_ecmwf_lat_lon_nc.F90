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
! Local variables:
! Name Type Description
!
!
! History:
! 2012/02/22: MJ produces code which reads rlat/lon grid from ERA INTERIM
!                GRIB files
! 2012/10/22: CP modified file for netcdf
! 2013/10/22: AP Tidying. Removed allocated arrays as unnecessary. Added
!                nc_close. Removed unused variables.
! 2013/11/06: GM wo was unititialized, set to 0
!
! $Id$
!
! Bugs:
! none known
!

!---------------------------------------------------
!---------------------------------------------------
subroutine read_ecmwf_lat_lon_nc(ecmwf_path,ecmwf_dims,ecmwf_2d)

   use netcdf
   use preproc_constants
   use preproc_structures
   use ecmwf_structures

   implicit none

   character(len=pathlength) :: ecmwf_path

   type(ecmwf_dims_s)        :: ecmwf_dims
   type(ecmwf_2d_s)          :: ecmwf_2d
   character(len=varlength)  :: dname
   real(kind=sreal), allocatable, dimension(:) :: lon,lat
   character(len=varlength)  :: available_names

   character(len=varlength)  :: name
   integer(kind=lint)        :: ncid,ierr,wo
   integer(kind=lint)        :: ivar,ndim,nvar,nattr,dummyint,jdim,idim
   integer                   :: dimid

   integer (kind=lint)       :: xdim,ydim,levlistdim,levtypedim,timedim


   ! open netcdf file
   wo = 0

   call nc_open(ncid,ecmwf_path,ierr,wo)

   call nc_info(ncid,ndim,nvar,nattr,wo)

   do idim=1,ndim
      if(idim .eq. 1 ) name='longitude'
      if(idim .eq. 2 ) name='latitude'
      if(idim .eq. 3 ) name='hybrid_1'
      if(idim .eq. 4 ) name='hybrid'
      if(idim .eq. 5 ) name='t'

      call nc_dim_id(ncid,name,dimid,wo)

      call nc_dim_length(ncid,dname,dimid,dummyint,wo)

      if(idim .eq. 1 ) xdim=int(dummyint,kind=lint)
      if(idim .eq. 2 ) ydim=int(dummyint,kind=lint)
      if(idim .eq. 3 ) levlistdim=int(dummyint,kind=lint)
      if(idim .eq. 4 ) levtypedim=int(dummyint,kind=lint)
      if(idim .eq. 5 ) timedim=int(dummyint,kind=lint)
   enddo


   do ivar=1,nvar
      ierr=nf90_inquire_variable(ncid, ivar, available_names)

      if (available_names .eq. 'longitude') then
         allocate(lon(xdim))
         lon=real_fill_value
         call nc_read_array_float_1d(ncid,xdim,'longitude',lon,wo) 
         !Latitude is counted from northpole to southpole 90 -> -90

         !Longitude is eastward from 0 to 358.875
      endif

      if (available_names .eq. 'latitude') then
         allocate(lat(ydim))
         lat=real_fill_value
         call nc_read_array_float_1d(ncid,ydim,'latitude',lat,wo)  
      endif
   enddo

   do jdim=1,ecmwf_dims%xdim_ec
      ecmwf_2d%longitude(jdim,:)=lon(jdim)
   enddo
   do idim=1,ecmwf_dims%ydim_ec
      ecmwf_2d%latitude(:,idim)=lat(idim)
   enddo

   ierr=nf90_close(ncid)
   if (allocated(lon)) deallocate(lon)
   if (allocated(lat)) deallocate(lat)

end subroutine read_ecmwf_lat_lon_nc
