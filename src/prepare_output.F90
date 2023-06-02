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
! 2015/11/14, GM: Added build_qc_flag_masks().
! 2015/11/19, GM: Added support for x_xx_um_legacy_channel_used masks.
! 2015/01/07, AP: Make QCFlag long to accomodate longer state vectors. Count
!    state vector elements rather than assuming there are four.
! 2017/07/05, AP: Add channels_used, variables_retrieved.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module prepare_output_m

   use ORAC_Constants_m

   implicit none

contains

#include "prepare_output_primary.F90"
#include "prepare_output_secondary.F90"


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
      str = 'OT'
      return
   else if (state == IRe) then
      str = 'ER'
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

   status = 1

end function string_description_of_state


!-------------------------------------------------------------------------------
! Name: build_flag_masks
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
! 2017/07/05, AP: Merged with build_flag_meanings.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine build_flag_masks(Ctrl, data)
   use Ctrl_m
   use orac_output_m, only: output_data_primary_t

   implicit none

   type(Ctrl_t),                intent(in)  :: Ctrl
   type(output_data_primary_t), intent(inout) :: data

   integer(lint)     :: i
   character(len=14) :: temp_str
   character(len=8)  :: state_label

   data%qc_flag_masks    = '1b'
   do i = 1, 7
      write(temp_str, '(I14)') 2_dint**i
      data%qc_flag_masks = trim(data%qc_flag_masks) // ' ' // &
           trim(adjustl(temp_str)) // 'b'
   end do

   write(temp_str,"(f14.1)") Ctrl%QC%MaxJ
   data%qc_flag_meanings = '1: Retrieval did not converge ' // &
                           '2: Total cost > ' // &
                               trim(adjustl(temp_str)) // ' ' // &
                           '4: Snow/ice surface ' // &
                           '8: Particle type inconsistent with cloud mask'
   write(temp_str,"(f14.1)") Ctrl%QC%MaxDoFN
   data%qc_flag_meanings = trim(data%qc_flag_meanings) // ' ' // &
                           '16: Degrees of freedom for noise > ' // &
                                trim(adjustl(temp_str))
   write(temp_str,"(f14.1)") Ctrl%QC%MaxElevation * 1e-3
   data%qc_flag_meanings = trim(data%qc_flag_meanings) // ' ' // &
                           '32: Surface elevation > ' // ' ' // &
                                trim(adjustl(temp_str)) // ' km'
   write(temp_str,"(f14.1)") Ctrl%MinRelAzi
   data%qc_flag_meanings = trim(data%qc_flag_meanings) // ' ' // &
                           '64: Sun glint (relative azimuth < ' // &
                                trim(adjustl(temp_str)) // ' deg) ' // &
                           '128: Retrieval hit an imposed limit'

   data%ch_flag_masks    = '2b'
   data%ch_flag_meanings = 'Ch1_used'
   do i = 2, Ctrl%Ind%NAll
      write(temp_str, '(I14)') 2_dint**i
      write(state_label, '(I8)') i

      data%ch_flag_masks = trim(data%ch_flag_masks) // ' ' // &
           trim(adjustl(temp_str)) // 'b'
      data%ch_flag_meanings = trim(data%ch_flag_meanings) // ' Ch' // &
           trim(adjustl(state_label)) // '_used'
   end do

   ! Bits 0-2 give the approach, bits 3-6 the class, and bits after that flag
   ! state vector elements in positions 1 to IRs(MaxNumSolar,1)
   data%vr_flag_masks ='0b 1b 2b 3b 8b 16b 24b 32b 40b 48b'
   do i = 1, IRs(MaxNumSolar,1)
      write(temp_str,"(i14)") 2_dint**(i + VarRetrievedBitOffset)
      data%vr_flag_masks = trim(data%vr_flag_masks) // ' ' // &
           trim(adjustl(temp_str)) // 'b'
   end do
   data%vr_flag_meanings = '0: Approach = 1 layer cloud ' // &
                           '1: Approach = 2 layer cloud ' // &
                           '2: Approach = Oxford surface aerosol ' // &
                           '3: Approach = Swansea surface aerosol ' // &
                           '4: Approach = Single-view aerosol ' // &
                           '5-7: Reserved ' // &
                           '8:  Class = water cloud ' // &
                           '16: Class = ice cloud ' // &
                           '24: Class = Oxford surface aerosol ' // &
                           '32: Class = Lambertian Swansea aerosol ' // &
                           '40: Class = BRDF Swansea aerosol ' // &
                           '48: Class = ash cloud ' // &
                           '56-120: Reserved ' // &
                           '128: optical depth retrieved ' // &
                           '256: effective radius retrieved ' // &
                           '512: cloud-top pressure retrieved ' // &
                           '1024: cloud fraction retrieved ' // &
                           '2048: 2nd optical depth retrieved ' // &
                           '4096: 2nd effective radius retrieved ' // &
                           '8192: 2nd cloud-top pressure retrieved ' // &
                           '16384: 2nd cloud fraction retrieved ' // &
                           '32768: Swansea gamma retrieved ' // &
                           '65536: surface temperature retrieved ' // &
                           '2^(16+i): surface reflectance in ith solar ' // &
                                     'channel retrieved'

end subroutine build_flag_masks


end module prepare_output_m
