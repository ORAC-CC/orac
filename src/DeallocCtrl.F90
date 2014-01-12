! Name:
!    Dealloc_Ctrl
!
! Purpose:
!    Deallocate the Ctrl at end of ECP execution.
!
! Arguments:
!    Name        Type           In/Out   Description
!    Ctrl        struct         In       Control structure
!    RTM         alloc struct   In       RTM structure
!    status      int            Out      Error status
!    
! Algorithm:
!    Deallocates all arrays in the Ctrl structure
!
! Local variables:
!    Name   Type   Description
!    None
!
! History:
!    13th Dec 2011, Caroline Poulsen: original version
!    20th Dec 2011, Caroline Poulsen: changed Ctrl to be inout (from in)
!    12th Jan 2014, Greg McGarragh: Added some missing deallocates.
!
! Bugs:
!    None known.
!
!------------------------------------------------------------------------------------
subroutine Dealloc_Ctrl(Ctrl, status)

   use Ctrl_def
   implicit none
   
!  Declare arguments

   type(Ctrl_t), intent(inout)   :: Ctrl

   integer, intent(inout)     :: status

      deallocate(Ctrl%Ind%ViewIdx)
      deallocate(Ctrl%Ind%Y_Id)
      deallocate(Ctrl%Ind%ChI)
      deallocate(Ctrl%Ind%Ysolar)
      deallocate(Ctrl%Ind%Ythermal)
      deallocate(Ctrl%Ind%Ysolar_msi)
      deallocate(Ctrl%Ind%Ythermal_msi)
      deallocate(Ctrl%Rs%B)
      deallocate(Ctrl%Sy)

end subroutine Dealloc_Ctrl
