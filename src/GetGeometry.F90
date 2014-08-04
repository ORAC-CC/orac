!-------------------------------------------------------------------------------
! Name:
!    Get_Geometry
!
! Purpose:
!    Gets the 'average' observation geometry for the current super-pixel.
!    Also calculates mean airmass factors for use in the forward model.
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
!    Assign the SPixel geometry values using the corresponding pixel from the
!    MSI_Data struct.
!    Determine the "illumination" (day, twi, night) based on solar zenith angle.
!    Ensure that illumination is consistent in all views for this pixel
!      (if not, return an error code)
!    Assign the active solar, thermal channels and state variables depending on
!    illumination.
!    Calculate the airmass factors for the observation and viewing geometry
!
! Local variables:
!    Name Type Description
!
! History:
!    27th Nov 2000, Kevin M. Smith: Original version
!    30th Nov 2000, Kevin M. Smith:
!       Added SPixel%NCloudy and SPixel%Ind%Xn and Yn
!    19th Dec 2000, Kevin M. Smith:
!       Replaced Data_Geometry_* arrays with Data structure
!     8th Jan 2001, Kevin M. Smith:
!       Included quality control mask
!    26th Jan 2001, Kevin M. Smith:
!       Added calculation of air mass factors
!    14th Mar 2001, Andy Smith:
!       Temporary change to setting of SPixel%Ind%Ny
!    16th Mar 2001, Andy Smith:
!       Using named constants for averaging methods.
!       Removed check for invalid averaging method (now in ReadDriver)
!    29th Mar 2001, Andy Smith:
!       Removed out-of-date comment on channel setting
!    27th Apr 2001, Andy Smith:
!       Added allocation and setting of active state variable arrays for the
!       current SPixel conditions.
!     6th Jun 2001, Andy Smith:
!       Added setting of new SPixel variables MDAD_LW, MDAD_SW and FG, AP arrays
!    15th Jun 2001, Andy Smith:
!       Changed error message string assignments/writes.
!       Long message strings were wrapped over two lines with only one set of
!       quotes around the whole string (including the line continuation marker).
!       Original code works on DEC but not Linux.
!     6th Jul 2001, Andy Smith:
!       Ctrl%X_Dy/Ni/Tw arrays are no longer allocatable. Now need explicit
!       indices supplied.
!     7th Aug 2001, Andy Smith:
!       Updates for image segmentation. Super-pixel y locations need to refer to
!       the current data segment in the MSI data arrays rather than the whole
!       image.
!       Renamed structure Data to MSI_Data since Data is a reserved word (hasn't
!       caused any problems so far but it might).
!       Added argument intent.
!    18th Sep 2001, Andy Smith:
!       Removed Write_Log call when sat zenith angle > max Ctrl value. No log
!       message necessary.
!    21st Sep 2001, Andy Smith:
!       Memory leak fix. Now deallocates SPixel%Ym and Sy before each allocation.
!       It is assumed that an initial allocation was made (in ECP main) otherwise
!       the first deallocate will fail.
!    **************** ECV work starts here *************************************
!    23rd Feb 2011, Andy Smith:
!       Cloud flags converted to real to match current ORAC data.
!    29th Mar 2011, Andy Smith:
!       Removal of super-pixel averaging. Process single pixels.
!    21st Apr 2011, Andy Smith:
!       Extension to handle multiple instrument views. MSI Geometry values have
!       extra dimension for view. SolZen are are now arrays of size NViews.
!       When checking pixel illumination, ensure that all views have the same
!       value (day, twilight, night).
!    13th Dec 2011, C. Poulsen:
!       Convert cosd to cos to make g95 compatible.
!    xxxx xxx xxxx, C. Poulsen:
!       Remove illumination calculation. Illum is now an array of values
!    30th Jul 2014, Greg McGarragh:
!       Cleaned up the code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Get_Geometry(Ctrl, SPixel, MSI_Data, status)

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

   integer :: view


   ! Set status to zero
   status = 0

   ! Assign SPixel values for satellite geometry
   SPixel%Geom%SolZen = MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%YSeg0, :)
   SPixel%Geom%SatZen = MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%YSeg0, :)
   SPixel%Geom%RelAzi = MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%YSeg0, :)

   ! Set status non-zero if satellite zenith angle is outside the allowed range
   ! specified in Ctrl. (Use absolute y location in the error message rather
   ! than segment value).
   do view = 1,Ctrl%Ind%NViews
      if (SPixel%Geom%SatZen(view) > Ctrl%MaxSatZen) then
         status = SPixelGeomSat
!        write(unit=message, fmt=*) &
!           'Get_Geometry: Satellite zenith angle exceeds maximum allowed ' &
!           // 'in super pixel starting at:', SPixel%Loc%X0, SPixel%Loc%Y0
!        call Write_log(Ctrl, trim(message), status)
      end if
   end do

   ! Calculate the mean air mass factors.

   ! Note: This is an approximation because the AMF of the mean angle is not the
   ! same as mean AMF taken over all the pixels in the super pixel. Also the
   ! path through the atmosphere deviates from SEC at larger paths. These
   ! approximations are valid for small super pixels and small angles.

   SPixel%Geom%SEC_o = 1.0 / COS(d2r*SPixel%Geom%Solzen)

   SPixel%Geom%SEC_v = 1.0 / COS(d2r*SPixel%Geom%Satzen)

end subroutine Get_Geometry
