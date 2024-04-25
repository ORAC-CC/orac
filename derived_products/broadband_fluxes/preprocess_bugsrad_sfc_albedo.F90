!-------------------------------------------------------------------------------
! Name: preprocess_bugsrad_sfc_albedo
!
! Purpose:
! Interpolate spectral surface black and white sky albedos from MODIS to BUGSrad
! bands. For more accurate results pass 8 spectral channels into the program.
! For less accurate results use heritage channels (4). It will run using both.
!
! Inputs:
! nc_alb: number of input spectral bands
! rho_0d: black sky albedo for each spectral band
! rho_dd: white sky albedo for each spectral band
!
! Outputs:
! rho0d_bugsrad: interpolated albedo to bugsrad band
! rhodd_bugsrad: interpolated albedo to bugsrad band
!
! History:
! xxxx/xx/xx, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine preprocess_bugsrad_sfc_albedo(nc_alb,rho_0d,rho_dd,rho0d_bugsrad, &
      rhodd_bugsrad)

   implicit none

   ! Input arguments
   integer, intent(in) :: nc_alb
   real, intent(in) :: rho_0d(nc_alb)
   real, intent(in) :: rho_dd(nc_alb)

   ! Output arguments
   real, intent(out) :: rho0d_bugsrad(6) ! rho_0d for each BUGSrad band
   real, intent(out) :: rhodd_bugsrad(6) ! rho_dd for each BUGSrad band

   ! Local variables
   real, dimension(nc_alb) :: modBand
   real :: bugsBand(6) ! center of each BUGSrad band (um)
   real(kind=8) :: m,b
   integer :: I

   integer :: modID1(6),modID2(6)

   ! Center location of each BUGsrad band (um)
   data bugsBand/0.4445,0.994,1.602,1.8995,3.0045,3.7545/

   rho0d_bugsrad(:) = 0.
   rhodd_bugsrad(:) = 0.

   ! Full MODIS spectral channel range
   if (nc_alb .EQ. 8) then
      ! center location of each MODIS band
      modBand = [0.67,0.87,0.47,0.55,1.24,1.6,2.13,3.7]

      ! Neighboring MODIS bands to interpolate to BUGSrad
      modID1 = [3,2,5,6,7,7]
      modID2 = [1,5,6,7,8,8]

      ! rho_0d
      do I = 2, 6
         m = (rho_0d(modID2(I)) - rho_0d(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_0d(modID1(I)) - m*modBand(modID1(I))
         rho0d_bugsrad(I) = m*bugsBand(I) + b
         if (rho0d_bugsrad(I) .ge. 1.) rho0d_bugsrad(I) = 1.0
      end do
      rho0d_bugsrad(1) = ( rho_0d(3)+rho_0d(4)+rho_0d(1) ) / 3.
!     print*,'rho_0d = ',rho_0d_bugs_sfc_albedo

      ! rho_dd
      do I = 2, 6
         m = (rho_dd(modID2(I)) - rho_dd(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_dd(modID1(I)) - m*modBand(modID1(I))
         rhodd_bugsrad(I) = m*bugsBand(I) + b
         if (rhodd_bugsrad(I) .ge. 1.) rhodd_bugsrad(I) = 1.0
      end do
      rhodd_bugsrad(1) = ( rho_dd(3)+rho_dd(4)+rho_dd(1) ) / 3.
!     print*,'rho_dd = ',rho_dd_bugs_sfc_albedo
   end if

   ! Heritage spectral channel range
   if (nc_alb .EQ. 4) then
      modBand = [0.67,0.87,1.6,3.7]

      ! Neighboring MODIS bands to interpolate to BUGSrad
      modID1 = [1,2,2,3,3,3]
      modID2 = [1,3,3,4,4,4]
      ! rho_0d
      do I = 2, 6
         m = (rho_0d(modID2(I)) - rho_0d(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_0d(modID1(I)) - m*modBand(modID1(I))
         rho0d_bugsrad(I) = m*bugsBand(I) + b
         if (rho0d_bugsrad(I) .ge. 1.) rho0d_bugsrad(I) = 1.0
      end do
      rho0d_bugsrad(1) = rho_0d(1)

      ! rho_dd
      do I = 2, 6
         m = (rho_dd(modID2(I)) - rho_dd(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_dd(modID1(I)) - m*modBand(modID1(I))
         rhodd_bugsrad(I) = m*bugsBand(I) + b
         if (rhodd_bugsrad(I) .ge. 1.) rhodd_bugsrad(I) = 1.0
      end do
      rhodd_bugsrad(1) = rho_dd(1)
   end if

end subroutine preprocess_bugsrad_sfc_albedo
