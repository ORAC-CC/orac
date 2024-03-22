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

subroutine preprocess_fuliou_sfc_albedo(nc_alb,rho_0d,rho_dd,rho0d_fuliou, &
      rhodd_fuliou)

   implicit none

   ! Input arguments
   integer, intent(in) :: nc_alb
   real, intent(in) :: rho_0d(nc_alb)
   real, intent(in) :: rho_dd(nc_alb)

   ! Output arguments
   real, intent(out) :: rho0d_fuliou(18) ! rho_0d for each Fu-Liou band
   real, intent(out) :: rhodd_fuliou(18) ! rho_dd for each Fu-Liou band

   ! Local variables
   real, dimension(nc_alb) :: modBand
   real :: fuBand(18) ! center of each FU Liou band (um)
   real(kind=8) :: m,b
   integer :: I

   integer :: modID1(18),modID2(18)

   ! Center location of each BUGsrad band (um)
   data fuBand/0.20005,0.2343,0.2648,0.2921,0.3105,0.34,0.3975,0.4675,0.54625, &
               0.6425,0.742,0.8415,0.9655,1.226,1.6574,2.2024,3.0044,3.75440/

   rho0d_fuliou(:) = 0.
   rhodd_fuliou(:) = 0.

   ! Full MODIS spectral channel range
   if (nc_alb .EQ. 8) then
      ! center location of each MODIS band
      modBand = [0.67,0.87,0.47,0.55,1.24,1.6,2.13,3.7]

      ! neighboring MODIS bands to interpolate to BUGSrad
      modID1 = [1,1,1,1,1,1,1,3,3,4,1,1,2,2,5,6,7,7]
      modID2 = [1,1,1,1,1,1,1,4,4,1,2,2,5,5,6,7,8,8]

      ! rho_0d
      rho0d_fuliou(1:7) = rho_0d(3)
      do I = 8, 18
         m = (rho_0d(modID2(I)) - rho_0d(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_0d(modID1(I)) - m*modBand(modID1(I))
         rho0d_fuliou(I) = m*fuBand(I) + b
         if (rho0d_fuliou(I) .ge. 1.) rho0d_fuliou(I) = 1.0
!        print*,I,m,b,modID1(I),modID2(I),rho_0d(modID2(I)),rho_0d(modID1(I))
      end do

      ! rho_dd
      rhodd_fuliou(1:7) = rho_dd(3)
      do I = 8, 18
         m = (rho_dd(modID2(I)) - rho_dd(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_dd(modID1(I)) - m*modBand(modID1(I))
         rhodd_fuliou(I) = m*fuBand(I) + b
         if (rhodd_fuliou(I) .ge. 1.) rhodd_fuliou(I) = 1.0
      end do
   end if

   ! Heritage spectral channel range
   if (nc_alb .EQ. 4) then
      modBand = [0.67,0.87,1.6,3.7]

      ! Neighboring MODIS bands to interpolate to Fu-Liou
      modID1 = [1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,3]
      modID2 = [1,1,1,1,1,1,1,1,1,1,2,2,3,3,3,4,4,4]
      ! rho_0d
      rho0d_fuliou(1:10) = rho_0d(1)
      do I = 11, 18
         m = (rho_0d(modID2(I)) - rho_0d(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_0d(modID1(I)) - m*modBand(modID1(I))
         rho0d_fuliou(I) = m*fuBand(I) + b
         if (rho0d_fuliou(I) .ge. 1.) rho0d_fuliou(I) = 1.0
!        print*,I,m,b,modID1(I),modID2(I),rho_0d(modID2(I)),rho_0d(modID1(I))
      end do

      ! rho_dd
      rhodd_fuliou(1:10) = rho_dd(1)
      do I = 11, 18
         m = (rho_dd(modID2(I)) - rho_dd(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = rho_dd(modID1(I)) - m*modBand(modID1(I))
         rhodd_fuliou(I) = m*fuBand(I) + b
         if (rhodd_fuliou(I) .ge. 1.) rhodd_fuliou(I) = 1.0
      end do
   end if

end subroutine preprocess_fuliou_sfc_albedo
