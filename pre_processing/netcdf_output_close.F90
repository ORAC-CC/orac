!-------------------------------------------------------------------------------
! Name: netcdf_output_close.F90
!
! Purpose:
! Close netcdf output files
!
! Description and Algorithm details:
! 1) Call NF90_CLOSE several times.
!
! Arguments:
! Name        Type In/Out/Both Description
! ------------------------------------------------------------------------------
! netcdf_info struct In Summary of output file details
!
! History:
! 2011/12/12, MJ: produces draft code which creates dummy output files
! 2013/11/06, MJ: adds config file to preprocessing output which holds all
!    relevant dimensional information.
! 2014/02/06, AP: removing unnecessary NCDF routines
! 2015/02/04, GM: Terminate on error like all the other NetCDF calls.
! 2015/07/03, OS: Corrected error message for clf.nc file
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_close(netcdf_info)

   use netcdf
   use preproc_constants

   implicit none

   type(netcdf_output_info_s), intent(in) :: netcdf_info

   if (nf90_close(netcdf_info%ncid_alb) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".alb.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_clf) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".clf.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_config) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".config.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_geo) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".geo.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_loc) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".loc.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_lsf) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".lsf.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_lwrtm) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".lwrtm.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_msi) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".msi.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_prtm) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".prtm.nc"'
      stop error_stop_code
   endif

   if (nf90_close(netcdf_info%ncid_swrtm) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".swrtm.nc"'
      stop error_stop_code
   endif

end subroutine netcdf_output_close
