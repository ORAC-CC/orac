!-------------------------------------------------------------------------------
! Name:
!    Get_LwSwRTM
!
! Purpose:
!    Performs the bilinear interpolation of the long wave RTM data to the
!    current super pixel coordinates.
!    Also assigns the surface to TOA transmittances for the current super pixel.
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    Ctrl     struct       In          Control structure
!    SAD_Chan struct array In          Channel description structures
!    RTM      alloc struct In          RTM structure
!    SPixel   struct       Both        Super-pixel structure
!    status   integer      Out         Error status
!
! Algorithm:
! 1) Determine the interpolation coefficients for this pixel.
! 2) Interpolate all of the required RTM fields using those coefficients.
! 3) Set the surface properties using the lowest RTM level.
! 4) Set dB_dTs using the surface temperature.
!
! History:
!    12th Jan 2001, Kevin M. Smith: Original version.
!    25th Jan 2001, Kevin M. Smith:
!       Added checks to see if super pixel lat,lon are covered by the RTM
!       ranges. Simplified calculation of XN and YN
!    21st Feb 2001, Andy Smith:
!       Added Tbc to LW structure. Previously missing from model data.
!       Tsf removed from LW structure.
!     1st Mar 2001, Andy Smith:
!       Changed setting of R_Clear values. These are not present in the
!       RTM struct as they are not read from the RTM data file.
!     6th Mar 2001, Andy Smith:
!       Tsf required in RTM struct again.
!     8th Mar 2001, Andy Smith:
!       Now sets dB_dTs in LW sub-struct.
!       Added SAD_Chan argument (required by T2R for dB_dTs calculation).
!    30th Mar 2001, Andy Smith:
!       Using whole array operations in the interpolations across all pressure
!       levels and channels to improve performance.
!       Bs calculation removed. Bs is not used by subsequent code.
!    20th Jul 2001, Andy Smith:
!       dB_dTs calculation fixed: was using no. of thermal channels from SPixel,
!       should have used Ctrl values as the RTM Lw arrays are all allocated to
!       size Ctrl%Ind%NThermal and left at this size for the whole image.
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    12th Dec 2003, Caroline Poulsen:
!       Added geopotential height.
!    22nd Sep 2011, Caroline Poulsen:
!       Added in swrtm variables.
!     7th Nov 2011, Caroline Poulsen:
!       Tidied up comments. no actual change.
!    15/09/2012, M. Stengel: Puts in coefs fudge to account for missing values.
!    03/11/2012, MJ: Changed Coeffs if block.
!    23/07/2014, AP: Grid no longer assumed to defined points rather than the
!       cells centres (as is actually the case).
!    23/07/2014, CP: Added in extrapolation into stratosphere.
!    30/07/2014, GM: Cleaned up the code.
!    30/07/2014, GM: Put the duplicate interpolation code into subroutines.
!    19/08/2014, AP: Using the preprocessor's interpolation routines.
!    28/09/2014, GM: Updated to conform with a new arrangement of dimensions.
!    11/11/2014, CP: Updated to include boundary layer cth correction to
!       temperature profile
!    12/01/2015, AP: Replace ThermalFirst:ThermalLast indexing with YThermal.
!    30/01/2015, AP: Remove skint and sp as redundant. Use bottom of T and P
!       arrays in SPixel%RTM instead.
!    06/02/2015, AP: Move T profile alteration to Int_CTP.
!    11/03/2015, GM: Do not interpolate wavelength dependent fields if the
!       number of measurements is equal to zero.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Get_LwSwRTM(Ctrl, SAD_Chan, RTM, SPixel, status)

   use Ctrl_def
   use ECP_Constants
   use interpol
   use planck
   use RTM_def
   use SAD_Chan_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(RTM_t),      intent(in)    :: RTM
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(out)   :: status

   type(interpol_s) :: interp
   real             :: R(Ctrl%Ind%NThermal)
   real             :: T_Array(Ctrl%Ind%NThermal)
   type(SAD_Chan_t) :: SAD_temp(Ctrl%Ind%NThermal)

   ! Set status to zero
   status = 0

   ! Bilinear interpolation (method taken from Numerical Recipes p96, 1987)
   call bilinear_coef(RTM%LW%Grid%Lon0, RTM%LW%Grid%inv_delta_Lon, &
        RTM%LW%Grid%NLon, RTM%LW%Grid%Lat0, RTM%LW%Grid%inv_delta_Lat, &
        RTM%LW%Grid%NLat, SPixel%Loc%Lon, SPixel%Loc%Lat, interp, &
        RTM%LW%Grid%Wrap)

   call interp_field2(RTM%LW%P, SPixel%RTM%LW%P, interp)
   call interp_field2(RTM%LW%T, SPixel%RTM%LW%T, interp)
   call interp_field2(RTM%LW%H, SPixel%RTM%LW%H, interp)
   if (Ctrl%Ind%NThermal .gt. 0) then
      call interp_field2(RTM%LW%Ems,     SPixel%RTM%LW%Ems,     interp)
      call interp_field2(RTM%LW%Tac,     SPixel%RTM%LW%Tac,     interp)
      call interp_field2(RTM%LW%Tbc,     SPixel%RTM%LW%Tbc,     interp)
      call interp_field2(RTM%LW%Rac_up,  SPixel%RTM%LW%Rac_up,  interp)
      call interp_field2(RTM%LW%Rac_dwn, SPixel%RTM%LW%Rac_dwn, interp)
      call interp_field2(RTM%LW%Rbc_up,  SPixel%RTM%LW%Rbc_up,  interp)
   end if

   ! Set surface level to TOA transmittances
   SPixel%RTM%LW%Tsf = SPixel%RTM%LW%Tac(:,RTM%LW%Np)

   ! Set R_Clear using Rbc_up at the TOA
   SPixel%RTM%LW%R_clear = SPixel%RTM%LW%Rbc_up(:,1)

   call interp_field2(RTM%LW%P, SPixel%RTM%SW%P, interp)
   if (Ctrl%Ind%NSolar .gt. 0) then
      call interp_field2(RTM%SW%Tac, SPixel%RTM%SW%Tac, interp)
      call interp_field2(RTM%SW%Tbc, SPixel%RTM%SW%Tbc, interp)
   end if

   ! Set surface level to TOA transmittances
   SPixel%RTM%SW%Tsf = SPixel%RTM%SW%Tac(:,RTM%SW%Np)

#ifdef LEGACY_CTP_MODE
   ! Modify profile in boundary layer inversion not implemented yet
   call Blmodification(SPixel)

   ! Extrapolate temperature profile into stratosphere to deal with deep
   ! convective clouds primarily in tropics that push through the trop.
   call extrap_into_tropopause(SPixel)
#endif

   ! Set dB_dTs using the surface temperature. (T2R needs an array of T values,
   ! one per channel, to convert).

   T_Array = SPixel%RTM%LW%T(SPixel%RTM%LW%Np)
   SAD_temp = SAD_Chan(Ctrl%Ind%YThermal)
   call T2R(Ctrl%Ind%NThermal, SAD_temp, &
        T_Array, R, SPixel%RTM%LW%dB_dTs, status)

end subroutine Get_LwSwRTM
