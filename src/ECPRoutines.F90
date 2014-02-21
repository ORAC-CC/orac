! Name:
!    ECP_Routines_def
!
! Purpose:
!    Interface definition for subroutines called from the ECP main program
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
!    24th Nov 2000, Andy Smith :
!       SetCRPSolar updated to use SPixel Geom structure.
!    19th Jan 2001, Andy Smith :
!       SetCRPSolar removed. Will not be called directly from ECP main 
!       program.
!    ***************** ECV work starts here *****************
!   22nd Mar 2011, Andy Smith:
!      Removal of phase change. Only 1 cloud class required for each retrieval 
!      run. SADCloudClass and SAD_LUT array dimensions changed. 
!   5th Aug 2011 Caroline Poulsen remove ref to cloudclass
! Bugs:
!    None known
!
! $Id$
!
!---------------------------------------------------------------------

module ECP_Routines_def

   interface
      subroutine Read_SAD(Ctrl, SAD_Chan, SAD_LUT, status)

	 use ECP_Constants
	 use CTRL_def
	 use SAD_Chan_def
	 use SAD_LUT_def

	 type(CTRL_t), intent(inout) :: Ctrl
	 type(SAD_Chan_t), dimension(:), intent(inout) :: SAD_Chan
         type(SAD_LUT_t), intent(inout)  :: SAD_LUT
	 integer, intent(out)        :: status
      end subroutine Read_SAD
   end interface

end module ECP_Routines_def
    
