!-------------------------------------------------------------------------------
! Name: read_input_dimensions.F90
!
! Purpose:
! Determine dimensions of data before allocating arrays.
!
! Description and Algorithm details:
! Use nc_dim_length a few times.
!
! Arguments:
! Name      Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! fname_msi string  In          Path for MSI (imager data) file
! fname_geo string  In          Path for GEO (geolocation) file
! xdim      int     Out         Across-track dimension
! ydim      int     Out         Along-track dimension
! vdim      int     Out         Number of available views
! verbose   logical In          If set, print results to screen
!
! History:
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/15, AP: Use preprocessor NCDF functions.
! 2014/10/07, AP: Removed read of layer dimensions.
! 2014/12/19, AP: Number of channels read in ReadDriver. Removed from here.
! 2014/01/30, AP: Remove NLayer as redundant.
! 2015/07/03, OS: Added error status variable to nc_open call.
! 2015/07/10, OS: Undo previous commit.
! 2016/01/20, GM: Added read_input_dimensions_rtm(). Removed _lwrtm() and
!    swrtm() versions.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_dimensions_msi(fname_msi, fname_geo, xdim, ydim, vdim, &
     verbose)

   use ECP_Constants
   use orac_ncdf

   implicit none

   character(len=FilenameLen), intent(in)  :: fname_msi
   character(len=FilenameLen), intent(in)  :: fname_geo
   integer(kind=lint),         intent(out) :: xdim,ydim,vdim
   logical,                    intent(in)  :: verbose

   integer                                 :: ncid


   ! Open msi file
   call nc_open(ncid,fname_msi)

   xdim = nc_dim_length(ncid, 'nx_msi', verbose)
   ydim = nc_dim_length(ncid, 'ny_msi', verbose)
   !cdim = nc_dim_length(ncid, 'nc_msi', verbose)

   ! Close msi file
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions_msi(): Error closing MSI ' // &
                 'file: ', fname_msi
      stop error_stop_code
   end if

   ! Open geo file
   call nc_open(ncid,fname_geo)

   !xdim = nc_dim_length(ncid, 'nx_geo', verbose)
   !ydim = nc_dim_length(ncid, 'ny_geo', verbose)
   vdim = nc_dim_length(ncid, 'nv_geo', verbose)

   ! Close geo file
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions_msi(): Error closing GEO ' // &
                 'file: ', fname_geo
      stop error_stop_code
   end if

end subroutine read_input_dimensions_msi


subroutine read_input_dimensions_rtm(fname_prtm,fname_lwrtm,fname_swrtm, &
     xdim,ydim,levdim,channeldim_lw,channeldim_sw,verbose)

   use ECP_Constants
   use orac_ncdf

   implicit none

   character(len=FilenameLen), intent(in)  :: fname_prtm
   character(len=FilenameLen), intent(in)  :: fname_lwrtm
   character(len=FilenameLen), intent(in)  :: fname_swrtm
   integer(kind=lint),         intent(out) :: xdim,ydim,levdim, &
                                              channeldim_lw,channeldim_sw
   logical,                    intent(in)  :: verbose

   integer                                 :: ncid


   ! Open PRTM file
   call nc_open(ncid,fname_prtm)

   xdim = nc_dim_length(ncid, 'nlon_rtm', verbose)
   ydim = nc_dim_length(ncid, 'nlat_rtm', verbose)
   levdim = nc_dim_length(ncid, 'nlevels_rtm', verbose)

   ! Close PRTM file
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions_rtm(): Error closing ' // &
                 'PRTM file: ', fname_prtm
      stop error_stop_code
   end if


   ! Open LWRTM file
   call nc_open(ncid,fname_lwrtm)

   channeldim_lw = nc_dim_length(ncid, 'nlw_channels', verbose)

   ! Close LWRTM file
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions_rtm(): Error closing ' // &
                 'LWRTM file: ', fname_lwrtm
      stop error_stop_code
   end if


   ! Open SWRTM file
   call nc_open(ncid,fname_swrtm)

   channeldim_sw = nc_dim_length(ncid, 'nsw_channels', verbose)

   ! Close SWRTM file
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions_rtm(): Error closing ' // &
                 'SWRTM file: ', fname_swrtm
      stop error_stop_code
   end if

end subroutine read_input_dimensions_rtm
