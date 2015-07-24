!-------------------------------------------------------------------------------
! Name: SetUnit.F90
!
! Purpose:
! Create a unit matrix of the appropriate size, as defined by the number of
! active state variables SPixel%Nx.
!
! Description and Algorithm details:
! Zero array, then write unity to the diagonal.
!
! Arguments:
! Name   Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! SPixel struct In          Current super-pixel info.
! Unit   real   Out         Unit matrix
!
! History:
! 2001/01/29, KS: Original version
! 2001/05/10, AS: Changed from allocatable array to automatically sized.
!    Size value now comes from SPixel rather than Ctrl.
! 2014/05/21, GM: Cleaned up the code.
! 2014/07/24, AP: Removed unused status variable.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_Unit(SPixel, Unit)

   use SPixel_def

   implicit none

   ! Declare arguments

   type(SPixel_t), intent(in)    :: SPixel
   real,           intent(inout) :: Unit(:,:)

   ! Declare local variables

   integer :: i

   ! Initialise all elements to be zero

   Unit = 0.0

   ! Set diagonals to unity

   do i = 1, SPixel%Nx
      Unit(i,i) = 1.0
   end do

end subroutine Set_Unit
