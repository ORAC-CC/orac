!-------------------------------------------------------------------------------
! Name: read_input_dimensions.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/15, AP: Use preprocessor NCDF functions.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_dimensions_msi(fname_msi,fname_geo,xdim,ydim,cdim,vdim, &
     verbose)

   use ECP_Constants
   use orac_ncdf

   implicit none

   character(len=FilenameLen), intent(in)  :: fname_msi
   character(len=FilenameLen), intent(in)  :: fname_geo
   integer(kind=lint),         intent(out) :: xdim,ydim,cdim,vdim
   logical,                    intent(in)  :: verbose

   integer                                 :: ncid


   ! Open msi file
   call nc_open(ncid,fname_msi)

   xdim = nc_dim_length(ncid, 'nx_msi', verbose)
   ydim = nc_dim_length(ncid, 'ny_msi', verbose)
   cdim = nc_dim_length(ncid, 'nc_msi', verbose)

   ! Close msi file
   if (nf90_close(ncid) .ne. NF90_NOERR) &
        stop 'READ_INPUT_DIM: Failure to close MSI file.'


   ! Open geo file
   call nc_open(ncid,fname_geo)

   !xdim = nc_dim_length(ncid, 'nx_geo', verbose)
   !ydim = nc_dim_length(ncid, 'ny_geo', verbose)
   vdim = nc_dim_length(ncid, 'nv_geo', verbose)

   ! Close geo file
   if (nf90_close(ncid) .ne. NF90_NOERR) &
        stop 'READ_INPUT_DIM: Failure to close GEO file.'

end subroutine read_input_dimensions_msi


subroutine read_input_dimensions_lwrtm(Ctrl,fname,xydim,xdim,ydim,levdim, &
     laydim,channeldim,viewdim,verbose)

   use CTRL_def
   use ECP_Constants
   use orac_ncdf

   implicit none

   type(CTRL_t),              intent(in)  :: Ctrl
   character(len=FilenameLen),intent(in)  :: fname
   integer(kind=lint),        intent(out) :: xydim,xdim,ydim,levdim,laydim, &
                                             channeldim,viewdim
   logical,                   intent(in)  :: verbose

   integer                                :: ncid

   ! Open file
   call nc_open(ncid,fname)

   xydim = nc_dim_length(ncid, 'nlon_x_nlat_lwrtm', verbose)
   xdim = nc_dim_length(ncid, 'nlon_lwrtm', verbose)
   ydim = nc_dim_length(ncid, 'nlat_lwrtm', verbose)
   laydim = nc_dim_length(ncid, 'nlayers_lwrtm', verbose)
   levdim = nc_dim_length(ncid, 'nlevels_lwrtm', verbose)
   channeldim = nc_dim_length(ncid, 'nlw_channels', verbose)
   viewdim = nc_dim_length(ncid, 'nviews', verbose)

   ! Close file
   if (nf90_close(ncid) .ne. NF90_NOERR) &
        stop 'READ_INPUT_DIM: Failure to close LWRTM file.'

end subroutine read_input_dimensions_lwrtm


subroutine read_input_dimensions_swrtm(fname,xydim,xdim,ydim,levdim,laydim, &
     channeldim,viewdim,verbose)

   use ECP_Constants
   use orac_ncdf

   implicit none

   character(len=FilenameLen), intent(in)  :: fname
   integer(kind=lint),         intent(out) :: xydim,xdim,ydim,levdim,laydim, &
                                              channeldim,viewdim
   logical,                    intent(in)  :: verbose

   integer                                 :: ncid

   ! Open file
   call nc_open(ncid,fname)
   
   xydim = nc_dim_length(ncid, 'nlon_x_nlat_swrtm', verbose)
   xdim = nc_dim_length(ncid, 'nlon_swrtm', verbose)
   ydim = nc_dim_length(ncid, 'nlat_swrtm', verbose)
   laydim = nc_dim_length(ncid, 'nlayers_swrtm', verbose)
   levdim = nc_dim_length(ncid, 'nlevels_swrtm', verbose)
   channeldim = nc_dim_length(ncid, 'nsw_channels', verbose)
   viewdim = nc_dim_length(ncid, 'nviews', verbose)

   ! Close file
   if (nf90_close(ncid) .ne. NF90_NOERR) &
        stop 'READ_INPUT_DIM: Failure to close SWRTM file.'

end subroutine read_input_dimensions_swrtm
