!-------------------------------------------------------------------------------
! Name:
!    Get_LSF
!
! Purpose:
!    Sets the land sea flag if no visible channels are present
!
! Arguments:
!    Name     Type         In/Out Description
!    Ctrl     struct       In     Control structure
!    SAD_Chan struct       In     SAD channel structure
!    SPixel   alloc struct Both   Super-pixel structure
!    MSI_Data struct       In     Data structure. Contains the multi-spectral
!                                 image measurements, location values,
!                                 geometry etc for the current image segment,
!                                 from which the current SPixel values will be
!                                 extracted.
!    status   integer      Out    Error status
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    08/02/2012, Caroline Poulsen: Original version
!    30/07/2014, Greg McGarragh: Cleaned up the code.
!
! Bugs:
!  none known
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Get_lsf(Ctrl, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(Data_t),   intent(in)    :: MSI_Data
   integer,        intent(out)   :: status

   ! Define local variables
#ifdef DEBUG
    character(180)   :: message
#endif

   ! Initialise
   status = 0

   SPixel%Surface%Land  = 0
   SPixel%Surface%Sea   = 0
   SPixel%Surface%NLand = 0
   SPixel%Surface%NSea  = 0


   SPixel%Surface%Flags = MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0)
   SPixel%Surface%NLand = SPixel%Surface%Flags
   SPixel%Surface%NSea  = SPixel%NMask - SPixel%Surface%NLand

   if (SPixel%Surface%NLand > 0) SPixel%Surface%Land = 1
   if (SPixel%Surface%NSea  > 0) SPixel%Surface%Sea  = 1
#ifdef DEBUG
    if (SPixel%Surface%Land + SPixel%Surface%Sea > 1) then
       ! Write warning to log file that the surface pixel contains mixed surface
       ! types
       write(unit=message, fmt=*) &
          'Get_Surface: WARNING pixel contains mixed surface types'
       call Write_log(Ctrl, trim(message), status)
    end if
#endif

end subroutine Get_lsf
