!-------------------------------------------------------------------------------
! Name:
!    Get_Illum
!
! Purpose:
!
! Arguments:
!    Name     Type    In/Out/Both Description
!    Ctrl     struct  In          Control structure
!    SPixel   struct  Both        Super-pixel structure
!    MSI_Data struct  In          Data structure. Contains the multi-spectral
!                                 image measurements, location values, geometry
!                                 etc for the current image segment, from which
!                                 the current SPixel values will be extracted.
!    status   integer Out         Error status
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!     4th Feb 2015, Greg McGarragh:
!       Original version.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Get_Illum(Ctrl, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(Data_t),   intent(in)    :: MSI_Data
   integer,        intent(out)   :: status

   ! Define local variables

   integer :: i_view

   ! Set status to zero
   status = 0

   ! Determine whether super pixel geometry corresponds to day, twilight or
   ! night and set up SPixel%Ind values accordingly.

   do i_view=1,Ctrl%Ind%NViews
      SPixel%illum(i_view) = MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%Y0, i_view)
   end do

end subroutine Get_Illum
