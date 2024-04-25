!-------------------------------------------------------------------------------
! Name: preprocess_bugsrad_sfc_emissivity
!
! Purpose:
! Interpolate spectral surface black and white sky albedos from MODIS to BUGSrad
! bands. For more accurate results pass 8 spectral channels into the program.
! For less accurate results use heritage channels (4). It will run using both.
!
! Inputs:
! nc_alb: number of input spectral bands
! emis_data: emissivity for each spectral band
!
! Outputs:
! emis_bugsrad: interpolated emissivity to BUGSrad channel
!
! History:
! xxxx/xx/xx, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine preprocess_bugsrad_sfc_emissivity(nc_emis,emis_data,emis_bugsrad)

   implicit none

   ! Input arguments
   integer, intent(in) :: nc_emis
   real, intent(in) :: emis_data(nc_emis)

   ! Output arguments
   real, intent(out) :: emis_bugsrad(12)

   ! Local variables
   real, dimension(nc_emis) :: modBand
   real :: bugsBand(12) ! center of each BUGSrad band (um)
   real(kind=8) :: m,b
   integer :: I

   integer :: modID1(12),modID2(12)

   ! Center location of each BUGsrad band (um)
   data bugsBand/4.85,5.5,6.45,7.55,8.54,9.65,11.35,13.70,16.70,21.75,30.5,5000/
   emis_bugsrad(:) = 1.

   ! Full MODIS spectral channel range
   if (nc_emis .EQ. 13) then
      ! Center location of each MODIS band
      modBand = [3.7,4.46,4.516,6.715,7.325,8.55,9.73,11.017,12.032,13.34,13.63, &
               13.94,14.23]

      ! Neighboring MODIS bands to interpolate to the BUGSrad
      modID1 = [3,3,3,5,5,6,8,11,12,13,13,13]
      modID2 = [4,4,4,6,6,7,9,12,13,13,13,13]

      ! Interpolate emissivity
      do I = 1, 8
         m = (emis_data(modID2(I)) - emis_data(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = emis_data(modID1(I)) - m*modBand(modID1(I))
         emis_bugsrad(I) = m*bugsBand(I) + b
      end do
      ! Bands 8 - 12 fill with MODIS band 36
      emis_bugsrad(9)  = 1.0!emis_data(13)
      emis_bugsrad(10) = 1.0!emis_data(13)
      emis_bugsrad(11) = 1.0!emis_data(13)
      emis_bugsrad(12) = 1.0!emis_data(13)
   end if

   ! Heritage spectral channel range
   if (nc_emis .EQ. 3) then
      ! Center location of each MODIS band
      modBand = [3.7,11.017,12.032]

      ! Neighboring MODIS bands to interpolate to the BUGSrad
      modID1 = [1,1,1,1,1,1,2,3,3,3,3,3]
      modID2 = [2,2,2,2,2,2,3,3,3,3,3,3]

!     print*,emis_data

      ! Interpolate emissivity
      do I = 1, 7
         m = (emis_data(modID2(I)) - emis_data(modID1(I))) / &
             (modBand(modID2(I)) - modBand(modID1(I)))
         b = emis_data(modID1(I)) - m*modBand(modID1(I))
         emis_bugsrad(I) = m*bugsBand(I) + b
!        print*,I,emis_bugsrad(I),m,b,emis_data(modID2(I)), &
!               emis_data(modID1(I)),modBand(modID1(I)), &
!               modBand(modID2(I)),bugsBand(I)
      end do
      ! Bands 8 - 12 fill with MODIS band 36
      emis_bugsrad(8)  = 1.0!emis_data(3)
      emis_bugsrad(9)  = 1.0!emis_data(3)
      emis_bugsrad(10) = 1.0!emis_data(3)
      emis_bugsrad(11) = 1.0!emis_data(3)
      emis_bugsrad(12) = 1.0!emis_data(3)
   end if

   ! Ocean surface
   if ( emis_data(1) .LE. 0.) emis_bugsrad(:) = 1.0

end subroutine preprocess_bugsrad_sfc_emissivity
