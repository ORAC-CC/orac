!-------------------------------------------------------------------------------
! Name: read_input_dimensions.F90
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for
! different attribute/variable type combinations.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/06/04, MJ: Changes routine names to "*_pp" to avoid confusion when
!    building libraries.
! 2014/12/03, CP: Added in common_constants should eventually remove
!    postproc_constants
! 2015/02/05, OS: Changed nint to lint
! 2015/02/05, CP: Updated constants file
! 2015/07/10, GM: Major cleanup and made use of the NetCDF interface in the
!    common library.
! 2016/03/03, AP: Homogenisation of I/O modules.
! 2017/01/02, AP: filename read more robust
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_dimensions(fname, indexing, verbose)

   use orac_ncdf_m

   implicit none

   character(len=*),      intent(in)  :: fname
   type(input_indices_t), intent(out) :: indexing
   logical,               intent(in)  :: verbose

   integer :: ncid, ierr

   ! Open file

   call ncdf_open(ncid, fname, 'read_input_dimensions()')
   call nullify_indexing(indexing)

   indexing%Xdim   = ncdf_dim_length(ncid, 'across_track', 'read_input_dimensions()')
   indexing%Ydim   = ncdf_dim_length(ncid, 'along_track', 'read_input_dimensions()')
   indexing%Ny     = ncdf_dim_length(ncid, 'channels', 'read_input_dimensions()')
   indexing%NViews = ncdf_dim_length(ncid, 'views', 'read_input_dimensions()')

   ! Read attributes that should eventually be dimensions
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'NState', indexing%Nx)
   if (ierr /= NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions(), ', trim(nf90_strerror(ierr)), &
           ', name: NState'
      stop error_stop_code
   end if

   ! Close msi file
   call ncdf_close(ncid, 'read_input_dimensions()')

end subroutine read_input_dimensions
