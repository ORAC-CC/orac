!-------------------------------------------------------------------------------
! Name: prepare_output.F90
!
! Purpose:
! Structure definition for output_data and module for output routines.
!
! History:
! 2015/09/06, GM: Original version created for the parts of output_routines.F90
!    that were not moved into common/ including the prepare routines and
!    string_description_of_state().
! 2015/09/06, GM: Added build_qc_flag_meanings().
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module prepare_output

   use ECP_Constants

   implicit none

contains

#include "prepare_primary.F90"
#include "prepare_secondary.F90"


!-------------------------------------------------------------------------------
! Name: string_description_of_state
!
! Purpose:
! Given an index of the state vector, this returns a string describing that
! element that a user can understand.
!
! Description and Algorithm details:
! Many if statements. The expectation is that this function will be extended to
! return multiple strings when ORAC needs to output elements from an unknown
! state vector.
!
! Arguments:
! Name   Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! state  integer In          Index of state vector to consider
! string string  Out         Descriptive string
! status integer Out         (Return value) Non-zero value indicates the given
!                            index was not known
!
! History:
! 2015/07/31, AP: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function string_description_of_state(state, str) result(status)
   implicit none

   integer,          intent(in)  :: state
   character(len=*), intent(out) :: str
   integer                       :: status

   integer                       :: i, j
   character(len=6), parameter   :: rho_labels(MaxRho_XX) = &
        ['RHO_OV', 'RHO_0D', 'RHO_DV', 'RHO_DD']
   character(len=2)              :: temp_str

   status = 0

   if (state == ITau) then
      str = 'COT'
      return
   else if (state == IRe) then
      str = 'REF'
      return
   else if (state == IPc) then
      str = 'CTP'
      return
   else if (state == IFr) then
      str = 'CCT'
      return
   else if (state == ITs) then
      str = 'STEMP'
      return
   end if
if (.false.) then
   do j = 1, MaxRho_XX
      do i = 1, MaxNumSolar
         if (state == IRs(i,j)) then
            write(temp_str, '(I2)') i
            str = rho_labels(j)//trim(adjustl(temp_str))
            return
         end if
      end do
   end do

   do i = 1, MaxNumViews
      if (state == ISP(i)) then
         write(temp_str, '(I2)') i
         str = 'P'//trim(adjustl(temp_str))
         return
      end if
   end do
end if
   status = 1

end function string_description_of_state


!-------------------------------------------------------------------------------
! Name: build_qc_flag_meanings
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type    In/Out/Both Description
! ------------------------------------------------------------------------------
!
! History:
! 2015/09/05, GM: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine build_qc_flag_meanings(Ctrl, str)
   use Ctrl_def

   implicit none

   type(Ctrl_t),     intent(in)  :: Ctrl
   character(len=*), intent(out) :: str

   integer          :: i
   character(len=2) :: temp_str
   character(len=8) :: state_label

   write(temp_str, '(I2)') Ctrl%Nx(IDay)
   str = 'Bit 0 set to 1 if cost too large, ' // &
         'Bits 1-' // trim(adjustl(temp_str)) // &
         ' set to 1 if state variable error out of bounds, ('
   do i = 1, Ctrl%Nx(IDay)
      write(temp_str, '(I2)') i
      if (string_description_of_state(Ctrl%X(i,IDay), state_label) == 0) &
           str = trim(str) // ' Bit ' // &
                 trim(adjustl(temp_str)) // '=' // trim(state_label)
   end do
   str = trim(str ) // ').'

end subroutine build_qc_flag_meanings

end module prepare_output
