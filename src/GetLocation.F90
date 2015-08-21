!-------------------------------------------------------------------------------
! Name: GetLocation.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name     Type   In/Out/Both  Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure
! SPixel   struct  Both        Super-pixel structure
! MSI_Data struct  In          Data structure. Contains the multi-spectral
!                              image measurements, location values, geometry
!                              etc for the current image segment, from which
!                              the current SPixel values will be extracted.
! status   integer Out         Error status
!
! History:
! 2015/08/04, GM: Original version.
!
! $Id: GetLocation.F90 3539 2015-07-30 16:32:59Z acpovey $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_Location(Ctrl, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(Data_t),   intent(in)    :: MSI_Data
   integer,        intent(out)   :: status


   status = 0

   SPixel%Loc%Lat = MSI_Data%Location%Lat(SPixel%Loc%X0, SPixel%Loc%Y0)
   SPixel%Loc%Lon = MSI_Data%Location%Lon(SPixel%Loc%X0, SPixel%Loc%Y0)

   ! Set status non-zero if lat/lon is outside the allowed range specified in
   ! Ctrl.
   if (SPixel%Loc%Lat < LatMin .or. SPixel%Loc%Lat > LatMax) then
#ifdef DEBUG
      write(*, *) 'Get_Location: Latitude out of range at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelLocLat
   end if
   if (SPixel%Loc%Lon < LonMin .or. SPixel%Loc%Lon > LonMax) then
#ifdef DEBUG
      write(*, *) 'Get_Location: Longitude out of range at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelLocLon
   end if

end subroutine Get_Location
