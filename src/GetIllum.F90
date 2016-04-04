!-------------------------------------------------------------------------------
! Name: GetIllum.F90
!
! Purpose:
! Copy illumination condition into SPixel structure. REDUNDANT to GetIndexing.
!
! Description and Algorithm details:
!
! Arguments:
! Name     Type   In/Out/Both  Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure
! SPixel   struct  Both        Super-pixel structure
! MSI_Data struct  In          Data structure. Contains the multi-spectral image
!                              measurements, location values, geometry etc for
!                              the current image segment, from which the current
!                              SPixel values will be extracted.
! status   integer Out         Error status
!
! History:
! 2015/02/04, GM: Original version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_Illum(Ctrl, SPixel, MSI_Data, status)

   use Ctrl_m
   use Data_m
   use ECP_Constants_m

   implicit none

   ! Define arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
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
