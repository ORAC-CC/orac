!-------------------------------------------------------------------------------
! Name: get_rtm.F90
!
! Purpose:
! Controls the interpolation of short and long wave RTM data to the current
! super pixel coordinates.
!
! Description and Algorithm details:
!    Check that the image data lies within the bounds of the LW RTM data
!    (SW RTM data has global coverage, so no need to check).
!    Call Get_LwRTM and Get_SwRTM subroutines if no errors have been found.
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       In          Control structure
! SAD_Chan struct array In          Channel description structures
! RTM      alloc struct Both        RTM structure
! SPixel   struct       Both        Super-pixel structure
! status   integer      Out         Error status
!
! History:
! 2001/01/11, KM: Original version
! 2001/03/08, AS: New argument SAD_Chan required for use in Get_LW_RTM. Added
!    intent to arguments and changed argument order (inputs first)
!    Get_LW/SW_RTM argument order changed.
! 2001/03/30, AS: Replaced all lat and lon tests that use maxval or minval of
!    the lat or lon arrays with a simple check vs. the new RTM grid values
!    MaxLat, MaxLon etc since in practice these tests are done many times on
!    the same data (this routine is called once per super-pixel).
! 2001/06/15, AS: Changed error message string assignments/writes. Long message
!    strings were wrapped over two lines with only one set of quotes around the
!    whole string (including the line continuation marker). Original code works
!    on DEC but not Linux.
! 2011/09/22, CP: Remove get GetLwRTM and GetSwRTM and replace with GetLwSwRTM
! 2014/07/30, GM: Cleaned up the code.
!
! Bugs:
! None known.
!------------------------------------------------------------------------------

subroutine Get_RTM(Ctrl, SAD_Chan, RTM, SPixel, status)

   use Ctrl_m
   use ORAC_Constants_m
   use RTM_m
   use SAD_Chan_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(RTM_t),      intent(in)    :: RTM
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(out)   :: status


   status = 0

   ! Check that the current super pixel location lies within the range of the
   ! LW and SW RTM data

   ! Note: All super pixel locations must lie within the RTM grid - currently
   ! no extrapolation)

   ! Longwave
   if (SPixel%Loc%Lat > RTM%Grid%MaxLat) then
#ifdef DEBUG
      write(*,*) 'WARNING: Get_RTM(): Maximum LW RTM latitude exceeded ' // &
         'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0, SPixel%Loc%Lat, &
         RTM%Grid%MaxLat
#endif
      status = GetRTMLwMaxLat
   end if
   if (SPixel%Loc%Lat < RTM%Grid%MinLat) then
#ifdef DEBUG
      write(*,*) 'WARNING: Get_RTM(): Minimum LW RTM latitude exceeded ', &
         'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0, SPixel%Loc%Lat, &
         RTM%Grid%MinLat
#endif
      status = GetRTMLwMinLat
   end if
   if (SPixel%Loc%Lon > RTM%Grid%MaxLon) then
#ifdef DEBUG
      write(*,*) 'WARNING: Get_RTM(): Maximum LW RTM longitude exceeded ' // &
         'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0, SPixel%Loc%Lon, &
         RTM%Grid%MaxLon
#endif
      status = GetRTMLwMaxLon
   end if
   if (SPixel%Loc%Lon < RTM%Grid%MinLon) then
#ifdef DEBUG
      write(*,*) 'WARNING: Get_RTM(): Minimum LW RTM longitude exceeded ', &
         'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0, SPixel%Loc%Lon, &
         RTM%Grid%MinLon
#endif
      status = GetRTMLwMinLon
   end if

   ! Shortwave

   ! Currently there is no error checking for the SW RTM. This is because the
   ! RTM is so simple (latitude bands) that there can be no exceptions, provided
   ! the location data is within physical bounds - and this is checked in
   ! Get_Location. Should the SW RTM be upgraded to a lat,lon grid then error
   ! checking similar to the above will be required.

   ! Call LW and SW subroutines if status is zero
   if (status == 0) call Get_LwSwRTM(Ctrl, SAD_Chan, RTM, SPixel, status)

end subroutine Get_RTM
