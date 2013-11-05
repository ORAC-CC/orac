! Name:
!    SAD_Routines_def
!
! Purpose:
!    Interface definition for ECP SAD read routines
!
! Description:
!    This module contains a set of interface defintions for ECP
!    subroutines. Not all subroutines are included. These interface
!    definitions are required in order that passed-length arrays can
!    be used as subroutine arguments.
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name       Type    Description
!    N/A
!
! History:
!    10th Nov 2000, Andy Smith : original version
!    ***************** ECV work starts here *****************
!   22nd Mar 2011, Andy Smith:
!      Removal of phase change. Only 1 cloud class required for each retrieval 
!      run. SADCloudClass and SAD_LUT array dimensions changed. 
!   30th June 2011, Caroline Poulsen: remove cloud class from read_lut
!   8th Aug 2011, Caroline Poulsen: removed ref to cloudclass
! Bugs:
!    None known
!
! $Id: SADRoutines.f90 74 2011-08-16 16:11:53Z capoulse $
!
!---------------------------------------------------------------------

module SAD_Routines_def

   interface
      subroutine Read_Chan (Ctrl, SAD_Chan, status)
	 use ECP_Constants
	 use Ctrl_def
	 use SAD_Chan_def
      
	 type(Ctrl_t), intent(in)      :: Ctrl
	 type(SAD_Chan_t), dimension(:), intent(inout) :: SAD_Chan
	 integer, intent(out)          :: status       
      end subroutine Read_Chan
   end interface  
   
   

   interface
      Subroutine Read_LUT (Ctrl, SAD_Chan, SAD_LUT, status)

	 use CTRL_def
         use SAD_Chan_def
	 use SAD_LUT_def

	 implicit none

      !  argument declarations 

	 type(CTRL_t), intent(inout)                      :: Ctrl
         type(SAD_Chan_t), dimension(:), intent(in)       :: SAD_Chan
	 type(SAD_LUT_t), intent(inout)     :: SAD_LUT
	 integer, intent(out)                             :: status
      end Subroutine Read_LUT
    end interface

end module SAD_Routines_def
