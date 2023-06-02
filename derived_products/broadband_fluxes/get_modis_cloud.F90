!-------------------------------------------------------------------------------
! Name: get_modis_cloud.F90
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

subroutine get_modis_cloud(fileIN,Nx,Ny,CTT,CTP,CTH,phase,REF,COT,cc_tot)

   use common_constants_m

   implicit none

   ! Input arguments
   character(len=*), intent(in) :: fileIN  ! MODIS Cloud File
   integer(kind=lint), intent(in) :: Nx,Ny ! Satellite 1-km dimensions

   ! Output arguments
   real(kind=sreal), intent(inout) :: CTT(1:Nx,1:Ny)    ! MOD06 Cloud Top Temperature
   real(kind=sreal), intent(inout) :: CTP(1:Nx,1:Ny)    ! MOD06 Cloud Top Pressure
   real(kind=sreal), intent(inout) :: CTH(1:Nx,1:Ny)    ! MOD06 Cloud Top Height
   real(kind=sreal), intent(inout) :: phase(1:Nx,1:Ny)  ! MOD06 Cloud Phase
   real(kind=sreal), intent(inout) :: REF(1:Nx,1:Ny)    ! MOD06 Cloud Effective Radius
   real(kind=sreal), intent(inout) :: COT(1:Nx,1:Ny)    ! MOD06 Cloud Optical Thickness
   real(kind=sreal), intent(inout) :: cc_tot(1:Nx,1:Ny) ! MOD06 Cloud Mask

   ! Local variables
   integer(kind=lint) :: tmpNx,tmpNy
   integer(kind=lint) :: ix,jy

   real(kind=sreal), allocatable :: pre_phase(:,:)   ! MOD06 Cloud Phase
   real(kind=sreal), allocatable :: pre_REF(:,:)     ! MOD06 Cloud Effective Radius
   real(kind=sreal), allocatable :: pre_REF_PCL(:,:) ! MOD06 Cloud Effective Radius
   real(kind=sreal), allocatable :: pre_COT(:,:)     ! MOD06 Cloud Effective Radius
   real(kind=sreal), allocatable :: pre_COT_PCL(:,:) ! MOD06 Cloud Effective Radius

   ! Replace ORAC cloud with fill value
   CTT = sreal_fill_value
   CTP = sreal_fill_value
   CTH = sreal_fill_value
   phase = sreal_fill_value
   REF = sreal_fill_value
   COT = sreal_fill_value
   cc_tot = 0.0

   ! Read dimensions of MOD06
   call read_hdf_sd_dims(fileIN,'Cloud_Effective_Radius',tmpNx,tmpNy)

   if (Nx .ne. tmpNx .or. Ny .ne. tmpNy) then
    print*,'MOD06 dimensions do not match ORAC'
    stop
   end if

   !----------------------------------------------------------------------------
   !  MOD06 Get MODIS CLOUD
   !----------------------------------------------------------------------------
   print*,'Fetching MOD06 CLOUD PROPERTIES'

   allocate(pre_REF(tmpNx,tmpNy))
   allocate(pre_REF_PCL(tmpNx,tmpNy))
   allocate(pre_COT(tmpNx,tmpNy))
   allocate(pre_COT_PCL(tmpNx,tmpNy))
   allocate(pre_phase(tmpNx,tmpNy))

   call read_hdf_sd_data(fileIN,'cloud_top_temperature_1km',-1,tmpNx,tmpNy,CTT)
   call read_hdf_sd_data(fileIN,'cloud_top_pressure_1km',-1,tmpNx,tmpNy,CTP)
   call read_hdf_sd_data(fileIN,'cloud_top_height_1km',-1,tmpNx,tmpNy,CTH)
   call read_hdf_sd_data(fileIN,'Cloud_Effective_Radius',-1,tmpNx,tmpNy,pre_REF)
   call read_hdf_sd_data(fileIN,'Cloud_Effective_Radius_PCL',-1,tmpNx,tmpNy,pre_REF_PCL)
   call read_hdf_sd_data(fileIN,'Cloud_Optical_Thickness',-1,tmpNx,tmpNy,pre_COT)
   call read_hdf_sd_data(fileIN,'Cloud_Optical_Thickness_PCL',-1,tmpNx,tmpNy,pre_COT_PCL)
   call read_hdf_sd_data(fileIN,'Cloud_Phase_Optical_Properties',-1,tmpNx,tmpNy,pre_phase)

   ! Combine PCL quantities
   REF(:,:) = sreal_fill_value
   COT(:,:) = sreal_fill_value
   do jy=1,nY
      do ix=1,nX
         if (pre_REF(ix,jy) .gt. 0 .and. pre_REF_PCL(ix,jy) .le. 0) &
            REF(ix,jy)=pre_REF(ix,jy)
         if (pre_REF(ix,jy) .le. 0 .and. pre_REF_PCL(ix,jy) .gt. 0) &
            REF(ix,jy)=pre_REF_PCL(ix,jy)
         if (pre_REF(ix,jy) .gt. 0 .and. pre_REF_PCL(ix,jy) .gt. 0) &
           REF(ix,jy)=( pre_REF(ix,jy)+pre_REF_PCL(ix,jy) ) / 2.

         if (pre_COT(ix,jy) .gt. 0 .and. pre_COT_PCL(ix,jy) .le. 0) &
            COT(ix,jy)=pre_COT(ix,jy)
         if (pre_COT(ix,jy) .le. 0 .and. pre_COT_PCL(ix,jy) .gt. 0) &
            COT(ix,jy)=pre_COT_PCL(ix,jy)
         if (pre_COT(ix,jy) .gt. 0 .and. pre_COT_PCL(ix,jy) .gt. 0) &
           COT(ix,jy)=( pre_COT(ix,jy)+pre_COT_PCL(ix,jy) ) / 2.
      end do
   end do

   ! Cloud Phase Consistency with CCI
   ! MODIS
   ! 0 -- cloud mask undetermined
   ! 1 -- clear sky
   ! 2 -- liquid water cloud
   ! 3 -- ice cloud
   ! 4 -- undetermined phase cloud (but retrieval is attempted as liquid water)
   !
   ! CCI
   ! 0 -- clear
   ! 1 -- water
   ! 2 -- ice
   ! Consistency:
   !  0 -- clear sky=1
   !  1 -- cloud mask water = 2 or cloud mask undetermined categories 0 & 4
   !  2 -- cloud mask = 3
   ! Note, the undetermined MODIS cloud phase means that the cloud optical
   ! properties retrieval algorithm could not make a determination of the cloud
   ! phase (liquid water or ice). This may have been caused by viewing anomalies
   ! in the retrieval (sunglint), contamination by aerosol, or a multi-layer
   ! cloud with mixed-phases. These undetermined retrievals use water look up
   ! tables, therefore we are assigning this category to liquid cloud retrieval.
   phase(:,:) = sreal_fill_value
   do jy=1,nY
      do ix=1,nX

      ! Clear-sky
      if (pre_phase(ix,jy) .eq. 1) phase(ix,jy) = 0

      ! Water Cloud
      if (pre_phase(ix,jy) .eq. 0. .or. pre_phase(ix,jy) .eq. 2 .or. &
          pre_phase(ix,jy) .eq. 4) phase(ix,jy) = 1

      ! Ice Cloud
      if (pre_phase(ix,jy) .eq. 3) phase(ix,jy) = 2
      end do
   end do

   ! Assume cloud if not clear
   do jy=1,nY
      do ix=1,nX
      if (phase(ix,jy) .gt. 0) cc_tot(ix,jy)=1.0
      end do
   end do

end subroutine get_modis_cloud
