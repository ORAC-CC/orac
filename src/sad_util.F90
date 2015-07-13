!-------------------------------------------------------------------------------
! Name: sad_util.F90
!
! Purpose:
! Module of routines used by the SAD reading routines.
!
! History:
! 2015/01/19, GM: Original version.
!
! $Id: sad_util.F90 2856 2015-01-12 18:50:33Z acpovey $
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module sad_util

   implicit none

contains

!-------------------------------------------------------------------------------
! Name: make_sad_chan_num
!
! Purpose:
! Returns a string label for a numbered channel.
!
! Algorithm:
! 1) If AVHRR: 1->Ch1, 2->Ch2, 3->Ch3a, 4->Ch3b, 5->Ch4, 6->Ch5
! 2) Otherwise, X->Ch0X, YY->ChYY
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure defined in Ctrl_def
! i_chan   integer In          Number of channel
! chan_num string  Out         String equivalent of channel number
!
! History:
! 2015/01/19, GM: Original version.
!
! Bugs:
! None known.
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
