!-------------------------------------------------------------------------------
! Name: compute_column_o3.F90
!
! Purpose:
! Compute column ozone (colO3) from input profile of geopotential height and
! ozone. The computation is a simple summation of integrated ozone over each
! layer in the atmosphere.
!
! Inputs:
! geopotential height (km)
! Ozone (kg/kg)
!
! Output:
! column ozone (DU)
!
! History:
! 2016/02/18, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine compute_column_o3(nlm,H,O3,colO3)

   implicit none

   ! Input arguments
   integer, intent(in) :: nlm
   real, intent(in), dimension(nlm+1) :: &
      H, & ! height profile at SAT. pixel      (hPa).
      O3   ! temperature profile at SAT. pixel (kg/kg).

   ! Output arguments
   real, intent(out) :: colO3

   ! Local variables
   integer :: i

   colO3 = 0.
   ! Integrate over each level
   do i = 1, nlm-1
!     print*,H(i),H(i)-H(i+1)
      colO3 = colO3+((O3(i)+O3(i+1))/2.) * (H(i)-H(i+1))*1000000.
!     print*,((O3(i)+O3(i+1))/2.) * (H(i)-H(i+1)),colO3
   end do

end subroutine compute_column_o3
