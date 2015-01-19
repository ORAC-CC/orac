!-------------------------------------------------------------------------------
! Name:
!    sad_util
!
! Purpose:
!
! Description:
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!    19th Jan 2015, Greg McGarragh: Original version.
!
! Bugs:
!    None known.
!
! $Id: sad_util.F90 2856 2015-01-12 18:50:33Z acpovey $
!
!---------------------------------------------------------------------

module sad_util

   implicit none

contains

!-------------------------------------------------------------------------------
! Name:
!    make_sad_chan_num
!
! Purpose:
!
! Arguments:
!    Name Type In/Out/Both Description
!
! Algorithm:
!
! History:
!    19th Jan 2015, Greg McGarragh: Original version.
!
! Bugs:
!    None known.
!
!-------------------------------------------------------------------------------

subroutine make_sad_chan_num(Ctrl, i_chan, chan_num)

   use CTRL_def

   implicit none

   type(CTRL_t),     intent(in)  :: Ctrl
   integer,          intent(in)  :: i_chan
   character(len=*), intent(out) :: chan_num

   if (Ctrl%Ind%Y_Id(i_chan) < 10) then
      if (Ctrl%Inst%Name(1:5) .ne. 'AVHRR') then
         write(chan_num, '(a2,i1)') 'Ch',Ctrl%Ind%Y_Id(i_chan)
      else
         select case (Ctrl%Ind%Y_Id(i_chan))
         case(1)
            chan_num='Ch1'
         case(2)
            chan_num='Ch2'
         case(3)
            chan_num='Ch3a'
         case(4)
            chan_num='Ch3b'
         case(5)
            chan_num='Ch4'
         case(6)
            chan_num='Ch5'
         end select
      end if
   else
      write(chan_num, '(a2,i2)') 'Ch',Ctrl%Ind%Y_Id(i_chan)
   end if

end subroutine make_sad_chan_num

end module sad_util
