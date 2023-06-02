!-------------------------------------------------------------------------------
! Name: get_geometry.F90
!
! Purpose:
! Gets the 'average' observation geometry for the current super-pixel.
! Also calculates mean airmass factors for use in the forward model.
!
! Description and Algorithm details:
! Assign the SPixel geometry values using the corresponding pixel from the
! MSI_Data struct.
! Determine the "illumination" (day, twi, night) based on solar zenith angle.
! Ensure that illumination is consistent in all views for this pixel
!   (if not, return an error code)
! Assign the active solar, thermal channels and state variables depending on
! illumination.
! Calculate the airmass factors for the observation and viewing geometry
!
! Arguments:
! Name     Type    In/Out/Both Description
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
! 2000/11/27, KS: Original version
! 2000/11/30, KS: Added SPixel%NCloudy and SPixel%Ind%Xn and Yn
! 2000/12/19, KS: Replaced Data_Geometry_* arrays with Data structure
! 2001/01/08, KS: Included quality control mask
! 2001/01/26, KS: Added calculation of air mass factors
! 2001/03/14, AS: Temporary change to setting of SPixel%Ind%Ny
! 2001/03/16, AS: Using named constants for averaging methods.
!    Removed check for invalid averaging method (now in ReadDriver)
! 2001/03/29, AS: Removed out-of-date comment on channel setting
! 2200/04/07, AS: Added allocation and setting of active state variable arrays
!    for the current SPixel conditions.
! 2001/06/06, AS: Added setting of new SPixel variables MDAD_LW, MDAD_SW and
!    FG, AP arrays
! 2001/06/15, AS: Changed error message string assignments/writes. Long message
!    strings were wrapped over two lines with only one set of quotes around the
!    whole string (including the line continuation marker). Original code works
!    on DEC but not Linux.
! 2001/07/07, AS: Ctrl%X_Dy/Ni/Tw arrays are no longer allocatable. Now need
!    explicit indices supplied.
! 2001/08/07, AS: Updates for image segmentation. Super-pixel y locations need
!    to refer to the current data segment in the MSI data arrays rather than the
!    whole image. Renamed structure Data to MSI_Data since Data is a reserved
!    word (hasn't caused any problems so far but it might). Added argument intent
! 2001/09/18, AS: Removed Write_Log call when sat zenith angle > max Ctrl value.
!    No log message necessary.
! 2001/09/21, AS: Memory leak fix. Now deallocates SPixel%Ym and Sy before each
!    allocation. It is assumed that an initial allocation was made (in ECP main)
!    otherwise the first deallocate will fail.
!    **************** ECV work starts here *************************************
! 2011/02/23, AS: Cloud flags converted to real to match current ORAC data.
! 2011/03/29, AS: Removal of super-pixel averaging. Process single pixels.
! 2011/04/24, AS: Extension to handle multiple instrument views. MSI Geometry
!    values have extra dimension for view. SolZen are are now arrays of size
!    NViews. When checking pixel illumination, ensure that all views have the
!    same value (day, twilight, night).
! 2011/12/13, CP: Convert cosd to cos to make g95 compatible.
! 2011/06/15, CP: Remove illumination calculation. Illum now an array of values
! 2014/07/30, GM: Cleaned up the code.
! 2015/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/07/14, AP: Replace loop with any() statement.
! 2015/08/04, GM: Added range checking.
! 2015/08/21, AP: Add sunglint test from aerosol (currently off as set to 0).
! 2016/08/11, SP: Add logical flag for processing when using only 1 view from a
!                 multiangular sensor. Prevents post-processor problems.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_Geometry(Ctrl, SPixel, MSI_Data, status)

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

   ! Assign SPixel values for satellite geometry
   SPixel%Geom%SolZen = MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%Y0, :)
   SPixel%Geom%SatZen = MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%Y0, :)
   SPixel%Geom%RelAzi = MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%Y0, :)
   SPixel%Geom%SatAzi = MSI_Data%Geometry%Saz(SPixel%Loc%X0, SPixel%Loc%Y0, :)

   ! Set status non-zero if one of the angles is outside the allowed range
   ! specified in Ctrl.
   if ((any(SPixel%Geom%SolZen < SolZenMin)) .and. (.not. Ctrl%all_channels_same_view)) then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: 1Solar zenith angle exceeds maximum at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if
   if (all(SPixel%Geom%SolZen < SolZenMin))  then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: 2Solar zenith angle exceeds maximum at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if
   if ((any(SPixel%Geom%SatZen < SatZenMin .or. &
      SPixel%Geom%SatZen > Ctrl%MaxSatZen)) .and. (.not. Ctrl%all_channels_same_view)) then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: 1Satellite zenith angle out of range at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if
   if (all(SPixel%Geom%SatZen < SatZenMin .or. SPixel%Geom%SatZen > Ctrl%MaxSatZen)) then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: 2Satellite zenith angle out of range at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if
   if ((any(SPixel%Geom%RelAzi < RelAziMin .or. &
           SPixel%Geom%RelAzi > RelAziMax)) .and. (.not. Ctrl%all_channels_same_view)) then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: 1Relative azimuth angle out of range at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if
   if (all(SPixel%Geom%RelAzi < RelAziMin .or. &
           SPixel%Geom%RelAzi > RelAziMax)) then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: 2Relative azimuth angle out of range at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if

   ! Aerosol sunglint test
   if ((any(SPixel%Geom%RelAzi < Ctrl%MinRelAzi)) .and. (.not. Ctrl%all_channels_same_view)) then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: Relative azimuth indicates sunglint at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if
   if (all(SPixel%Geom%RelAzi < Ctrl%MinRelAzi)) then
#ifdef DEBUG
      write(*, *) 'Get_Geometry: Relative azimuth indicates sunglint at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      status = SPixelGeomSat
   end if

   ! Calculate the mean air mass factors.
   SPixel%Geom%SEC_o = 1.0 / cos(d2r*SPixel%Geom%Solzen)
   SPixel%Geom%SEC_v = 1.0 / cos(d2r*SPixel%Geom%Satzen)

end subroutine Get_Geometry
