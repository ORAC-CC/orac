!-------------------------------------------------------------------------------
! Name: get_lsf.F90
!
! Purpose:
! Sets the land sea flag if no visible channels are present.
! REDUNDANT due to GetSurface.
!
! Description and Algorithm details:
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       In          Control structure
! SAD_Chan struct       In          SAD channel structure
! SPixel   alloc struct Both        Super-pixel structure
! MSI_Data struct       In          Data structure. Contains the multi-
!                                   spectral image measurements, location
!                                   values, geometry etc for the current
!                                   image segment, from which the current
!                                   SPixel values will be extracted.
! status   integer      Out         Error status
!
! History:
! 2012/02/08, CP: Original version
! 2014/07/30, GM: Cleaned up the code.
! 2015/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/12/30, AP: Add surfaces_to_skip.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_LSF(Ctrl, SPixel, MSI_Data, status)

   use Ctrl_m
   use Data_m
   use ORAC_Constants_m

   implicit none

   ! Define arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(Data_t),   intent(in)    :: MSI_Data
   integer,        intent(out)   :: status


   status = 0

   select case (MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%Y0))
   case(1)
      SPixel%Surface%Land = .true.
   case(0)
      SPixel%Surface%Land = .false.
   case default
#ifdef DEBUG
      write(*, *) 'WARNING: Get_Surface(): pixel contains mixed surface types'
#endif
      status = SPixelInvalid
   end select

   if (SPixel%Surface%Land .and. Ctrl%Surfaces_to_skip == ILand .or. &
       .not. SPixel%Surface%Land .and. Ctrl%Surfaces_to_skip == ISea) &
       status = SPixelSkip

end subroutine Get_LSF
