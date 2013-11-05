! Name:
!    FM_Routines_def
!
! Purpose:
!    Interface definition for subroutines called from the ECP FM routine
!    and routines below it.
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
!    19th Jan 2001, Andy Smith : original version
!    23rd Jan 2001, Andy Smith :
!       SetCRPSolar/Thermal interfaces updated to cope with new routine
!       SetGZero.
!    16th Feb 2001, Andy Smith :
!       New argument SPixel for SetCRPSolar.
!    20th Feb 2001, Andy Smith:
!       SetCRPSolar Spixel argument replaced by Ind (sub-struct of SPixel).
!       Same change applied to SetCRPThermal.
!    27th Feb 2001, Andy Smith:
!       Set_CRP_Thermal no longer requires argument First.
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

module FM_Routines_def

   interface
      Subroutine Set_CRP_Solar (Ctrl, Ind, GZero, SAD_LUT, CRPOut, dCRPOut, &
         status)

	 use Int_Routines_def
	 use Ctrl_def
         use SPixel_def
	 use SAD_LUT_def
	 use GZero_def

	 implicit none

      !  argument declarations 
	 type(Ctrl_t), intent(in)              :: Ctrl
         type(Spixel_Ind_t), intent(in)        :: Ind
	 type(GZero_t), intent(inout)          :: GZero  ! Struct containing 
                                                	 ! "zero'th" grid points
	 type(SAD_LUT_t), intent(inout)        :: SAD_LUT
	 real, dimension(:,:), intent(inout)   :: CRPOut  
                        			 ! Interpolated values returned
                        			 ! (CRPOut(1)=RBD, (2)=TB, ...
	 real, dimension(:,:,:), intent(inout) :: dCRPOut
                                		 ! Interpolated gradients of CRPOut in 
						 ! Tau and Re
	 integer, intent(out)                  :: status
      End Subroutine Set_CRP_Solar
   end interface

   interface
      Subroutine Set_CRP_Thermal (Ctrl, Ind, GZero, SAD_LUT, &
	 CRPOut, dCRPOut, status)

	 use Int_Routines_def
	 use Ctrl_def
         use SPixel_def
	 use SAD_LUT_def
	 use GZero_def

	 implicit none

      !  Argument declarations 
	 type(Ctrl_t), intent(in)              :: Ctrl
         type(Spixel_Ind_t), intent(in)        :: Ind
	 type(GZero_t), intent(inout)          :: GZero  ! Struct containing 
                                                	 ! "zero'th" grid points
	 type(SAD_LUT_t), intent(inout)        :: SAD_LUT
	 real, dimension(:,:), intent(inout)   :: CRPOut  
                        			 ! Interpolated values returned
                        			 ! (CRPOut(1)=RBD, (2)=TB, ...
	 real, dimension(:,:,:), intent(inout) :: dCRPOut
                                		 ! Interpolated gradients of CRPOut in 
						 ! Tau and Re
	 integer, intent(out)                  :: status
      End Subroutine Set_CRP_Thermal
   end interface

end module FM_Routines_def
    
