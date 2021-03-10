!-------------------------------------------------------------------------------
! Name: read_input_dimensions.F90
!
! Purpose:
! Determine dimensions of data before allocating arrays.
!
! Description and Algorithm details:
! Use ncdf_dim_length a few times.
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
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_dimensions_msi(fname_msi, fname_geo, xdim, ydim, vdim, &
     verbose)

   use ORAC_Constants_m
   use orac_ncdf_m

   implicit none

   character(len=*),   intent(in)  :: fname_msi
   character(len=*),   intent(in)  :: fname_geo
   integer(kind=lint), intent(out) :: xdim,ydim,vdim
   logical,            intent(in)  :: verbose

   integer                         :: ncid


   ! Open msi file
   call ncdf_open(ncid, fname_msi, 'read_input_dimensions_msi()')

   xdim = ncdf_dim_length(ncid, 'nx_msi', 'read_input_dimensions_msi()')
   ydim = ncdf_dim_length(ncid, 'ny_msi', 'read_input_dimensions_msi()')
   !cdim = ncdf_dim_length(ncid, 'nc_msi', 'read_input_dimensions_msi()')

   ! Close msi file
   call ncdf_close(ncid, 'read_input_dimensions_msi(MSI)')

   ! Open geo file
   call ncdf_open(ncid, fname_geo, 'read_input_dimensions_msi()')

   !xdim = ncdf_dim_length(ncid, 'nx_geo', 'read_input_dimensions_msi()')
   !ydim = ncdf_dim_length(ncid, 'ny_geo', 'read_input_dimensions_msi()')
   vdim = ncdf_dim_length(ncid, 'nv_geo', 'read_input_dimensions_msi()')

   ! Close geo file
   call ncdf_close(ncid, 'read_input_dimensions_msi(GEO)')

end subroutine read_input_dimensions_msi


subroutine read_input_dimensions_rtm(fname_prtm,fname_lwrtm,fname_swrtm, &
     xdim,ydim,levdim,channeldim_lw,channeldim_sw,n_solar,verbose)

   use ORAC_Constants_m
   use orac_ncdf_m

   implicit none

   character(len=*),   intent(in)  :: fname_prtm
   character(len=*),   intent(in)  :: fname_lwrtm
   character(len=*),   intent(in)  :: fname_swrtm
   integer(kind=lint), intent(out) :: xdim,ydim,levdim, &
                                      channeldim_lw,channeldim_sw
   integer(kind=lint), intent(in)  :: n_solar
   logical,            intent(in)  :: verbose

   integer                         :: ncid


   ! Open PRTM file
   call ncdf_open(ncid, fname_prtm, 'read_input_dimensions_rtm()')

   xdim = ncdf_dim_length(ncid, 'nlon_rtm', 'read_input_dimensions_rtm()')
   ydim = ncdf_dim_length(ncid, 'nlat_rtm', 'read_input_dimensions_rtm()')
   levdim = ncdf_dim_length(ncid, 'nlevels_rtm', 'read_input_dimensions_rtm()')

   ! Close PRTM file
   call ncdf_close(ncid, 'read_input_dimensions_rtm(PRTM)')


   ! Open LWRTM file
   call ncdf_open(ncid, fname_lwrtm, 'read_input_dimensions_rtm()')

   channeldim_lw = ncdf_dim_length(ncid, 'nlw_channels', 'read_input_dimensions_rtm()')

   ! Close LWRTM file
   call ncdf_close(ncid, 'read_input_dimensions_rtm(LWRTM)')


   ! Open SWRTM file
   call ncdf_open(ncid, fname_swrtm, 'read_input_dimensions_rtm()')
   if (n_solar > 0) then
      channeldim_sw = ncdf_dim_length(ncid, 'nsw_channels', 'read_input_dimensions_rtm()')
   else
      channeldim_sw = 0
   end if

   ! Close SWRTM file
    call ncdf_close(ncid, 'read_input_dimensions_rtm(SWRTM)')

end subroutine read_input_dimensions_rtm
