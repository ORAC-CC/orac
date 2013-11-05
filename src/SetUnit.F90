! Name:
!    Set_Unit
!
! Purpose:
!    Create a unit matrix of the appropriate size, as defined by the number 
!    of active state variables SPixel%Nx.
!
! Arguments:
!    Name        Type           In/Out   Description
!    Spixel      struct         In       Current super-pixel info.
!    Unit        real           Out      Unit matrix
!    status      int            Out      Error status
!    
! Algorithm:
!
! Local variables:
!    Name   Type   Description
!    None
!
! History:
!  29th January, 2001, Kevin M. Smith : original version
!  10th May 2001, Andy Smith:
!     Changed from allocatable array to automatically sized.
!     Size value now comes from SPixel rather than Ctrl.
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Set_Unit(Spixel, Unit, status)

   use Spixel_def

   implicit none
   
!  Declare arguments

   type(SPixel_t), intent(in) :: SPixel
   real, intent(inout)        :: Unit(SPixel%Nx, SPixel%Nx)
   integer                    :: status

!  Declare local variables 

   integer        :: i        ! Loop counter

!  Initialise all elements to be zero
   
   Unit(:,:) = 0.0
   
!  Set diagonals to unity
   
   do i = 1, Spixel%Nx   
      Unit(i,i) = 1.0      
   end do

end subroutine Set_Unit
