!-------------------------------------------------------------------------------
! Name:
!    Get_RTM
!
! Purpose:
!    Controls the interpolation of short and long wave RTM data to the current
!    super pixel coordinates.
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    Ctrl     struct       In          Control structure
!    SAD_Chan struct array In          Channel description structures
!    SPixel   struct       Both        Super-pixel structure
!    RTM      alloc struct Both        RTM structure
!    status   integer      Out         Error status
!
! Algorithm:
!    Check that the image data lies within the bounds of the LW RTM data
!    (SW RTM data has global coverage, so no need to check).
!    Call Get_LwRTM and Get_SwRTM subroutines if no errors have been found.
!
! Local variables:
!    Name Type Description
!
! History:
!    11th Jan 2001, Kevin M. Smith: Original version
!     8th Mar 2001, Andy Smith:
!       New argument SAD_Chan required for use in Get_LW_RTM.
!       Added intent to arguments and changed argument order (inputs first)
!       Get_LW/SW_RTM argument order changed.
!    30th Mar 2001, Andy Smith:
!       Replaced all lat and lon tests that use maxval or minval of the lat or
!       lon arrays with a simple check vs. the new RTM grid values MaxLat,
!       MaxLon etc since in practice these tests are done many times on the same
!       data (this routine is called once per super-pixel).
!    15th Jun 2001, Andy Smith:
!       Changed error message string assignments/writes.
!       Long message strings were wrapped over two lines with only one set of
!       quotes around the whole string (including the line continuation marker).
!       Original code works on DEC but not Linux.
!    22nd Sep xxxx, Somebody:
!       Remove get GetLwRTM and GetSwRTM and replace with GetLwSwRTM
!    30th Jul 2014, Greg McGarragh:
!       Cleaned up the code.
!
! Bugs:
!    None known.
!
! $Id$
!
!------------------------------------------------------------------------------

subroutine Get_RTM(Ctrl, SAD_Chan, RTM, SPixel, status)

   use Ctrl_def
   use ECP_Constants
   use Get_LwSwRTM_m
   use RTM_def
   use SAD_Chan_def
   use SPixel_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(RTM_t),      intent(in)    :: RTM
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(out)   :: status

   ! Declare local variables

   character(180) :: message

   ! Set status to zero

   status = 0

   ! Check that the current super pixel location lies within the range of the
   ! LW and SW RTM data

   ! Note: All super pixel locations must lie within the RTM grid - currently
   ! no extrapolation)

   ! Longwave
   if (SPixel%Loc%Lat > RTM%Lw%Grid%MaxLat) then
      status = GetRTMLwMaxLat
      write(unit=message, fmt=*) &
         'Get_RTM: Maximum LW RTM latitude exceeded by super pixel ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0,SPixel%Loc%Lat, RTM%Lw%Grid%MaxLat
      write(*, *) &
         'Get_RTM: Maximum LW RTM latitude exceeded by super pixel ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0,SPixel%Loc%Lat, RTM%Lw%Grid%MaxLat
      call Write_log(Ctrl, trim(message), status)
   end if
   if (SPixel%Loc%Lat < RTM%Lw%Grid%MinLat) then
      status = GetRTMLwMinLat
      write(unit=message, fmt=*) &
         'Get_RTM: Minimum LW RTM latitude exceeds super pixel latitude ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0
      write(*, fmt=*) &
         'Get_RTM: Minimum LW RTM latitude exceeds super pixel latitude ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0,SPixel%Loc%Lat < RTM%Lw%Grid%MinLat
      call Write_log(Ctrl, trim(message), status)
   end if
   if (SPixel%Loc%Lon > RTM%Lw%Grid%MaxLon) then
      status = GetRTMLwMaxLon
      write(unit=message, fmt=*) &
         'Get_RTM: Maximum LW RTM longitude exceeded in super pixel ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0
      write(*, *) &
         'Get_RTM: Maximum LW RTM longitude exceeded in super pixel ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0, SPixel%Loc%X0, SPixel%Loc%Y0
      call Write_log(Ctrl, trim(message), status)
   end if
   if (SPixel%Loc%Lon < RTM%Lw%Grid%MinLon) then
      status = GetRTMLwMinLon
      write(unit=message, fmt=*) &
         'Get_RTM: Minimum LW RTM longitude exceeds super pixel longitude ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0
      write(*, *) &
         'Get_RTM: Minimum LW RTM longitude exceeds super pixel longitude ' &
         // 'starting at:', SPixel%Loc%X0, SPixel%Loc%Y0
      call Write_log(Ctrl, trim(message), status)
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
