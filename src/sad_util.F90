!-------------------------------------------------------------------------------
! Name: sad_util.F90
!
! Purpose:
! Module of routines used by the SAD reading routines.
!
! History:
! 2015/01/19, GM: Original version.
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module sad_util_m

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
! Ctrl     struct  In          Control structure defined in Ctrl_m
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

   use Ctrl_m

   implicit none

   type(Ctrl_t),     intent(in)  :: Ctrl
   integer,          intent(in)  :: i_chan
   character(len=*), intent(out) :: chan_num

   if (Ctrl%Ind%Y_Id(i_chan) < 10) then
      if (Ctrl%InstName(1:5) .ne. 'AVHRR') then
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

!-------------------------------------------------------------------------------
! Name: map_ch_indices
!
! Purpose:
! Returns the indices of provided_ind that have the same value as each
! element of required_ind.
!
! Algorithm:
! 1) For an element of required_ind, loop over provided_ind looking for
!    the same value.
! 2) Store that element in map.
! 3) Repeat for all elements of required_ind.
!
! Arguments:
! Name         Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! nreq         integer In      Length of required_ind
! required_ind integer In      Array of desired values
! npro         integer In      Length of provided_ind
! provided_ind integer In      Array of available values
! map          integer Out     Indices that map provided onto required
!
! History:
! 2020/08/18, AP: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine map_ch_indices(nreq, required_ind, npro, provided_ind, map)
   use common_constants_m, only: error_stop_code

   implicit none

   integer, intent(in)    :: nreq
   integer, intent(in)    :: required_ind(:)
   integer, intent(in)    :: npro
   integer, intent(in)    :: provided_ind(:)
   integer, intent(inout) :: map(:)
   integer :: i, j

   map = 0
   do i = 1, nreq
      do j = 1, npro
         if (required_ind(i) == provided_ind(j)) then
            map(i) = j
            exit
         end if
      end do

      if (map(i) == 0) then
         print*, 'ERROR: map_ch_indices(): Cannot locate channel ', &
              required_ind(i), ' in ', provided_ind
         stop error_stop_code
      end if
   end do

end subroutine map_ch_indices

end module sad_util_m
