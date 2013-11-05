! Name:
!    Check_ByteArray
!
! Purpose:
!    Determines whether all elements in a byte array are within specified
!    limits.  Sets elements of a mask to zero, when an 'out of range' array
!    element is found.
!
! Arguments
!    Name     Type       In/Out   Description
!    array    byte arr   In       Input array to be examined
!    Nx       int        In       Number of columns to be examined
!    Ny       int        In       Number of rows to be examined
!    mask     int arr    Out      Mask (1 = good, 0 = out of range)
!    ulim     int        In       Upper limit
!    llim     int        In       Lower limit
!    status   int        Out      Status flag 
!
! Local variables:
!    Name   Type   Description
!    i, j   int    Loop counters
!
! History:
!    Dec 2000, Kevin M Smith : Original version 
!     2nd April 2001, Andy Smith:
!        Improved efficiency by using else if and reversing the order of the 
!        do loops.
! 2013 MJ explicitly defines array type
!
! Bugs:
!    None known.
! 
!---------------------------------------------------------------------
subroutine Check_ByteArray(Nx, Ny, array, mask, ulim, llim, status)

   use ECP_Constants

   implicit none

!  define arguments

   integer :: Nx
   integer :: Ny
!   byte    :: array(Nx, Ny)
   integer(kind = byte) :: array(Nx, Ny)
   integer :: mask(Nx, Ny)
   integer :: ulim
   integer :: llim
   integer :: status

!  define local variables

   integer :: i, j
   
   status = 0

   do j = 1, Ny
      do i = 1, Nx
         if (int(array(i,j)) > ulim) then
            mask(i,j) = 0
            status    = 1
         else if (int(array(i,j)) < llim) then
            mask(i,j) = 0
            status    = 1
         end if
      end do
   end do
   
end subroutine Check_ByteArray
