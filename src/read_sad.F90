!-------------------------------------------------------------------------------
! Name: read_sad.F90
!
! Purpose:
! Controlling subroutine for reading Static Application Data (SAD) files.
!
! Description and Algorithm details:
! This subroutine calls all the subroutines that read SAD files. It also
! sets the sizes of all allocatable arrays of SAD data structures, and
! carries out checking of the returned status from each subroutine called.
!
! Arguments:
! Name       Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl       struct  In          Control structure. Passed to all subordinate
!                                routines.
! SAD_Chan   array of structs    Instrument channel info for each channel.
!                    Out         Populated by=Read_Chan
! SAD_LUT    array of structs    Cloud Radiative Property Look Up Tables for
!                    Out         layer. Populated by Read_LUT.
!
! History:
! 2000/09/06, AS: Original version.
! 2000/11/23, AS: Fixed debugging output statements: only executed if status is 0
! 2001/06/25, AS: Tidied up. Header comments completed. Removed debug code.
!    **************** ECV work starts here *************************************
! 2011/03/22, AS: Removal of phase change. Only 1 cloud class required for each
!    retrieval run. SADCloudClass and SAD_LUT array dimensions changed.
! 2011/08/05, CP: Removed routine that read the cloudclass config file removed
!    from function name.
! 2011/12/05, CP: Removed instrument config file.
! 2014/05/23, GM: Cleaned up code.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2016/07/27, GM: Read SAD stuff for layer 2 when the multilayer retrieval is
!    active.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module Read_SAD_m

implicit none

contains

subroutine Read_SAD(Ctrl, SAD_Chan, SAD_LUT)

   use Ctrl_m
   use ORAC_Constants_m
   use SAD_Chan_m
   use SAD_LUT_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(:)
   type(SAD_LUT_t),  intent(inout) :: SAD_LUT(:)

   if (Ctrl%verbose) write(*,*) 'Reading SAD files'

   ! Read channel sad files
   call Read_SAD_Chan(Ctrl, SAD_Chan)

   ! Read Look up tables
   call Read_SAD_LUT (Ctrl, SAD_Chan, SAD_LUT(1), 1)
   if (Ctrl%Approach == AppCld2L) then
      call Read_SAD_LUT (Ctrl, SAD_Chan, SAD_LUT(2), 2)
   end if

end subroutine Read_SAD

end module Read_SAD_m
