!-------------------------------------------------------------------------------
! Name:
!    Find_Lun
!
! Purpose:
!    Subroutine to find a free logical unit number for file handling
!
! Description:
!    Uses the inquire function to find a free logical unit number in the range
!    20 to 100.
!
! Arguments:
!    Name Type In/Out/Both Description
!    lun  int  Out         Free unit number
!
! Algorithm:
!    for i=20, 200 (until  free unit is found)
!       inquire whether unit number exists and is in use
!
! Local variables:
!    Name Type Description
!
! History:
!    17th Aug 2000, Andy Smith : Original version
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Find_Lun(lun)

   implicit none

   integer, intent(out) :: lun

   integer              :: i
   logical              :: found_lun
   logical              :: lun_exists
   logical              :: lun_used

   found_lun = .false.

   do i = 20, 100
      if (found_lun) exit
      inquire(unit=i, exist=lun_exists, opened=lun_used)
      if (lun_exists .and. (.not. lun_used)) then
         found_lun = .true.
	 lun = i
      end if
   end do

   if (.not. found_lun) then
      write(*,*) 'Find_Lun: could not find free LUN in range 20 to 100'
      lun = -1
   end if

end subroutine Find_Lun
