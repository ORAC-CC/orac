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
! ecmwf_path string in   Full path to ECMWF gpam NCDF file to read.
! ecmwf_dims struct both Structure summarising dimensions of ECMWF files.
!
! History:
! 2012/01/13: MJ writes original code version.
! 2012/01/13: CP changed to accomodate badc netcdf files
!                with different dimensions
! 2012/11/13: CP removed hardwiring of levels kdim_ec
! 2013/10/22: AP Tidying
! 2014/02/06: AP Further tidying. Removed unnecessary components.
!
! $Id$
!
! Bugs:
! none known
!

subroutine read_ecmwf_dimensions_nc(ecmwf_path, ecmwf_dims)

   implicit none

   character(len=pathlength) :: ecmwf_path
   type(ecmwf_dims_s)        :: ecmwf_dims

   integer(kind=lint)        :: ncid,ierr,idim,ndim,nvar,nattr,dummyint
   character(len=varlength)  :: dname


   call nc_open(ncid,ecmwf_path)

   ierr=NF90_INQUIRE(ncid,ndim,nvar,nattr)

   do idim=1,ndim
      ierr=NF90_INQUIRE_DIMENSION(ncid,idim,dname,dummyint)
      if (ierr.ne.NF90_NOERR) &
           STOP 'READ_ECMWF_DIMENSIONS_NC: Error inspecting NCDF dimension.'

      if (dname == 'longitude') then
         ecmwf_dims%xdim=int(dummyint,kind=lint)
      else if (dname == 'latitude') then
         ecmwf_dims%ydim=int(dummyint,kind=lint)
      else if ((dname=='hybrid' .and. ndim.eq.4) .or. dname=='hybrid_1') then
         ecmwf_dims%kdim=int(dummyint,kind=lint)
      endif
      !if (dname == 't') timedim=int(dummyint,kind=lint)
   enddo

   ierr=NF90_CLOSE(ncid)


end subroutine read_ecmwf_dimensions_nc
      
