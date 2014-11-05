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
!   relevant dimensional information.
! 2014/02/06, AP: removing unnecessary NCDF routines
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_close(netcdf_info)

   use netcdf

   implicit none

   type(netcdf_output_info_s), intent(in) :: netcdf_info

   if (nf90_close(netcdf_info%ncid_alb).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF ALB.'
   if (nf90_close(netcdf_info%ncid_clf).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF CLF.'
   if (nf90_close(netcdf_info%ncid_config).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF CONFIG.'
   if (nf90_close(netcdf_info%ncid_geo).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF GEO.'
   if (nf90_close(netcdf_info%ncid_loc).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF LOC.'
   if (nf90_close(netcdf_info%ncid_lsf).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF LSF.'
   if (nf90_close(netcdf_info%ncid_lwrtm).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF LWRTM.'
   if (nf90_close(netcdf_info%ncid_msi).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF MSI.'
   if (nf90_close(netcdf_info%ncid_prtm).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF PRTM.'
   if (nf90_close(netcdf_info%ncid_swrtm).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF SWRTM.'
   if (nf90_close(netcdf_info%ncid_scan).ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF UV.'

end subroutine netcdf_output_close
