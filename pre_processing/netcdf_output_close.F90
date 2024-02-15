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
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_close(netcdf_info, use_seviri_ann_ctp_fg)

   use orac_ncdf_m, only: ncdf_close
   use preproc_constants_m

   implicit none

   type(netcdf_output_info_t), intent(in) :: netcdf_info
   logical, intent(in) :: use_seviri_ann_ctp_fg

   call ncdf_close(netcdf_info%ncid_alb, 'netcdf_create_config(): ".alb.nc"')
   call ncdf_close(netcdf_info%ncid_clf, 'netcdf_create_config(): ".clf.nc"')
   call ncdf_close(netcdf_info%ncid_config, 'netcdf_create_config(): ".config.nc"')
   call ncdf_close(netcdf_info%ncid_geo, 'netcdf_create_config(): ".geo.nc"')
   call ncdf_close(netcdf_info%ncid_loc, 'netcdf_create_config(): ".loc.nc"')
   call ncdf_close(netcdf_info%ncid_lsf, 'netcdf_create_config(): ".lsf.nc"')
   call ncdf_close(netcdf_info%ncid_lwrtm, 'netcdf_create_config(): ".lwrtm.nc"')
   call ncdf_close(netcdf_info%ncid_msi, 'netcdf_create_config(): ".msi.nc"')
   call ncdf_close(netcdf_info%ncid_prtm, 'netcdf_create_config(): ".prtm.nc"')
   call ncdf_close(netcdf_info%ncid_swrtm, 'netcdf_create_config(): ".swrtm.nc"')
#ifdef INCLUDE_SEVIRI_NEURALNET
   if (use_seviri_ann_ctp_fg) then
       call ncdf_close(netcdf_info%ncid_ctp, 'netcdf_create_config(): ".ctp.nc"')
   end if
#endif

end subroutine netcdf_output_close
