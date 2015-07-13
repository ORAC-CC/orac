!-------------------------------------------------------------------------------
! Name: GetLSF.F90
!
! Purpose:
! Sets the land sea flag if no visible channels are present
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_LSF(Ctrl, SPixel, MSI_Data, status)

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

   ! Initialise
   status = 0

   SPixel%Surface%Land  = 0
   SPixel%Surface%Sea   = 0
   SPixel%Surface%NLand = 0
   SPixel%Surface%NSea  = 0


   SPixel%Surface%Flags = MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%Y0)
   SPixel%Surface%NLand = SPixel%Surface%Flags
   SPixel%Surface%NSea  = SPixel%NMask - SPixel%Surface%NLand

   if (SPixel%Surface%NLand > 0) SPixel%Surface%Land = 1
   if (SPixel%Surface%NSea  > 0) SPixel%Surface%Sea  = 1
#ifdef DEBUG
   if (SPixel%Surface%Land + SPixel%Surface%Sea > 1) then
      ! Write warning to log file that the surface pixel contains mixed surface
      ! types
      write(*,*) 'WARNING: Get_LSF() pixel contains mixed surface types'
      status = -1
   end if
#endif

end subroutine Get_LSF
