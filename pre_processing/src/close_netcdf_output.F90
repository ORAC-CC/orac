! Name: close_netcdf_output.f90
!
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
! 2011/12/12: MJ produces draft code which creates dummy output files
! 2013/11/06: MJ adds config file to preprocessing output which holds all
!                relevant dimensional information.
! 2014/02/06: AP removing unnecessary NCDF routines
!
! $Id$
!
! Bugs:
! none known
!

subroutine close_netcdf_output(netcdf_info)

   use netcdf
   use netcdf_structures

   implicit none

   type(netcdf_info_s), intent(in) :: netcdf_info

   integer :: ierr

   ierr=nf90_close(netcdf_info%ncid_lwrtm)
   if (ierr.ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF LWRTM.'
   ierr=nf90_close(netcdf_info%ncid_swrtm)
   if (ierr.ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF SWRTM.'
   ierr=nf90_close(netcdf_info%ncid_prtm)
   if (ierr.ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF PRTM.'
   ierr=nf90_close(netcdf_info%ncid_config)
   if (ierr.ne.NF90_NOERR) &
        print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF CONFIG.'
   ierr=nf90_close(netcdf_info%ncid_msi)
   if (ierr.ne.NF90_NOERR) print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF MSI.'
   ierr=nf90_close(netcdf_info%ncid_geo)
   if (ierr.ne.NF90_NOERR) print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF GEO.'
   ierr=nf90_close(netcdf_info%ncid_lsf)
   if (ierr.ne.NF90_NOERR) print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF LSF.'
   ierr=nf90_close(netcdf_info%ncid_cf)
   if (ierr.ne.NF90_NOERR) print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF CF.'
   ierr=nf90_close(netcdf_info%ncid_loc)
   if (ierr.ne.NF90_NOERR) print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF LOC.'
   ierr=nf90_close(netcdf_info%ncid_alb)
   if (ierr.ne.NF90_NOERR) print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF ALB.'
   ierr=nf90_close(netcdf_info%ncid_scan)
   if (ierr.ne.NF90_NOERR) print*,'CLOSE_NETCDF_OUTPUT: Error closing NCDF UV.'

end subroutine close_netcdf_output
