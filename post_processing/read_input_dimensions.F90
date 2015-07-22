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
! 2014/06/04, MJ: changes routine names to "*_pp" to avoid confusion when
!    building libraries.
! 2014/12/03, CP: added in common_constants should eventually remove vartypes_pp
! 2015/02/05, OS: changed nint to lint
! 2015/02/05, CP: updated constants file
! 2015/07/10, GM: Major cleanup and made use of the NetCDF interface in the
!    common library.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_dimensions(fname,xdim,ydim,verbose)

   use common_constants
   use netcdf
   use structures_pp
   use vartypes_pp
   use orac_ncdf

   implicit none

   character(len=path_length), intent(in)  :: fname
   integer(kind=lint),         intent(out) :: xdim,ydim
   logical,                    intent(in)  :: verbose

   integer :: ncid

   ! Open file
   call nc_open(ncid,fname)

   xdim = nc_dim_length(ncid, 'across_track', verbose)
   ydim = nc_dim_length(ncid, 'along_track',  verbose)

   ! Close msi file
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions(): Error closing file: ', fname
      stop error_stop_code
   end if

end subroutine read_input_dimensions
