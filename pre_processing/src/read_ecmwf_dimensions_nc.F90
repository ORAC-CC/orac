! Name: read_ecmwf_dimensions_nc.F90
!
!
! Purpose:
! Extract dimensions of arrays of variables stored in ERA Interim NCDF files
! for further use in dynamic array allocation.
!
! Description and Algorithm details:
! 1) Open file.
! 2) Read the length of the first 3 dimensions and store into structure.
! 3) Close file.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in   Full path to ECMWF NCDF file to read.
! ecmwf_dims struct both Structure summarising dimensions of ECMWF files.
!
! History:
! 2012/01/13: MJ writes original code version.
! 2012/01/13: CP changed to accomodate badc netcdf files
!                with different dimensions
! 2012/11/13: CP removed hardwiring of levels kdim_ec
! 2013/10/22: AP Tidying
!
! Local variables:
! Name       Type   In/Out/Both Description
!
!
! $Id$
!
! Bugs:
! none known
!

!---------------------------------------------------
!---------------------------------------------------
subroutine read_ecmwf_dimensions_nc(ecmwf_path, ecmwf_dims)

   use netcdf
   use preproc_constants
   use ecmwf_structures

   implicit none

   character(len=pathlength) :: ecmwf_path
   type(ecmwf_dims_s)        :: ecmwf_dims

   integer(kind=lint)        :: ncid,ierr,wo,idim,ndim,nvar,nattr,dummyint
   integer(kind=stint)       :: dimid
   character(len=varlength)  :: dname,name
   integer(kind=lint)        :: levtypedim,timedim


   wo=0 ! do not print NCDF details

   call nc_open(ncid,ecmwf_path,ierr,wo)

   call nc_info(ncid,ndim,nvar,nattr,wo)
   ndim=3 ! ignore 'hybrid' and 't'

   do idim=1,ndim
      !NB note that the dimension names are different for different netcdf files
      !so need to be modified if different netcdf files are read
      !i.e values are different for pressure level netcdf files.
      if(idim .eq. 1) name='longitude'
      if(idim .eq. 2) name='latitude'
      if(idim .eq. 3) name='hybrid_1'
      !     if(idim .eq. 3) name='p' ! netcdf pressure levels
      !     if(idim .eq. 4) name='hybrid'
      !     if(idim .eq. 5) name='time'

      call nc_dim_id(ncid,name,dimid,wo)

      call nc_dim_length(ncid,dname,dimid,dummyint,wo)

      if(idim .eq. 1) ecmwf_dims%xdim_ec=int(dummyint,kind=lint)
      if(idim .eq. 2) ecmwf_dims%ydim_ec=int(dummyint,kind=lint)
      if(idim .eq. 3) ecmwf_dims%kdim_ec=int(dummyint,kind=lint)
      if(idim .eq. 4) levtypedim=int(dummyint,kind=lint)
      if(idim .eq. 5) timedim=int(dummyint,kind=lint)
   enddo

   ierr=nf90_close(ncid)


end subroutine read_ecmwf_dimensions_nc
      
