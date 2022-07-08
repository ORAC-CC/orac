!-------------------------------------------------------------------------------
! Name: get_modis_aerosol.F90
!
! Purpose:
!
! Inputs:
!
! Output:
!
! History:
! xxxx/xx/xx, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine get_modis_aerosol(fileIN,Nx,Ny,AREF,AOD550)

   use common_constants_m

   implicit none

   ! Input arguments
   character(len=*), intent(in) :: fileIN  ! MODIS aerosol file
   integer(kind=lint), intent(in) :: Nx,Ny ! Satellite 1-km dimensions

   ! Output arguments
   real(kind=sreal), intent(inout) :: AREF(1:Nx,1:Ny)   ! MOD04 Aerosol Effective Radius - regrid
   real(kind=sreal), intent(inout) :: AOD550(1:Nx,1:Ny) ! MOD04 Aerosol Optical Depth - regrid

   ! Local variables
   integer(kind=lint) :: tmpNx,tmpNy
   integer(kind=lint) :: ix,jy
   integer(kind=lint) :: tmp_ix,tmp_jy
   real(kind=sreal) :: xFac,yFac

   real(kind=sreal), allocatable :: pre_AREF(:,:)   ! MOD04 Aerosol Effective Radius - orig
   real(kind=sreal), allocatable :: pre_AOD550(:,:) ! MOD04 Aerosol Optical Depth - orig

   ! Replace ORAC Aerosol with fill value
   AREF = sreal_fill_value
   AOD550 = sreal_fill_value

   ! Read dimensions of mod04
   call read_hdf_sd_dims(fileIN,'Optical_Depth_Land_And_Ocean',tmpNx,tmpNy)

   !----------------------------------------------------------------------------
   !  MOD04, get MODIS aerosol
   !----------------------------------------------------------------------------
   print*,'Fetching MOD04 AEROSOL PROPERTIES'
   allocate(pre_AREF(tmpNx,tmpNy))
   allocate(pre_AOD550(tmpNx,tmpNy))
   call read_hdf_sd_data(fileIN,'Optical_Depth_Land_And_Ocean',-1,tmpNx,tmpNy,pre_AOD550)
   call read_hdf_sd_data(fileIN,'Effective_Radius_Ocean',1,tmpNx,tmpNy,pre_AREF)

   ! Rebin MOD04 data to match MOD06 resolution
   xFac = Nx / (tmpNx-1.)
   yFac = Ny / (tmpNy-1.)
   do jy=1,nY
      do ix=1,nX
         tmp_ix=floor((ix+5.)/xFac)+1
         tmp_jy=floor((jy+5.)/yFac)+1
         AREF(ix,jy)=pre_AREF(tmp_ix,tmp_jy)
         AOD550(ix,jy)=pre_AOD550(tmp_ix,tmp_jy)
         if (AOD550(ix,jy) .gt. 0. .and. AREF(ix,jy) .le. 0.) AREF(ix,jy)=0.5
      end do
    end do

end subroutine get_modis_aerosol
