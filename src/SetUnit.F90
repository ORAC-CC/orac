!-------------------------------------------------------------------------------
! Name:
!    Set_Unit
!
! Purpose:
!    Create a unit matrix of the appropriate size, as defined by the number of
!    active state variables SPixel%Nx.
!
! Arguments:
!    Name   Type   In/Out/Both Description
!    SPixel struct In          Current super-pixel info.
!    Unit   real   Out         Unit matrix
!    status int    Out         Error status
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    29th Jan 2001, Kevin M. Smith: Original version
!    10th May 2001, Andy Smith:
!       Changed from allocatable array to automatically sized.
!       Size value now comes from SPixel rather than Ctrl.
!    21th May 2014, Greg McGarragh:
!       Cleaned up the code.
!    24th Jul 2014, Adam Povey:
!       Removed unused status variable.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Set_Unit(SPixel, Unit)

   use SPixel_def

   implicit none

   ! Declare arguments

   type(SPixel_t), intent(in)    :: SPixel
   real,           intent(inout) :: Unit(SPixel%Nx, SPixel%Nx)

   ! Declare local variables

   integer :: i

   ! Initialise all elements to be zero

   Unit(:,:) = 0.0

   ! Set diagonals to unity

   do i = 1, SPixel%Nx
      Unit(i,i) = 1.0
   end do

end subroutine Set_Unit
