!-------------------------------------------------------------------------------
! Name: GetLwSwRTM.F90
!
! Purpose:
! Performs the bilinear interpolation of the long wave RTM data to the
! current super pixel coordinates.
! Also assigns the surface to TOA transmittances for the current super pixel.
!
! Description and Algorithm details:
! 1) Determine the interpolation coefficients for this pixel.
! 2) Interpolate all of the required RTM fields using those coefficients.
! 3) Set the surface properties using the lowest RTM level.
! 4) Set dB_dTs using the surface temperature.
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       In          Control structure
! SAD_Chan struct array In          Channel description structures
! RTM      alloc struct In          RTM structure
! SPixel   struct       Both        Super-pixel structure
! status   integer      Out         Error status
!
! History:
! 2001/01/12, KM: Original version.
! 2001/01/25, KM: Added checks to see if super pixel lat,lon are covered by the
!    RTM ranges. Simplified calculation of XN and YN
! 2001/02/21, AS: Added Tbc to LW structure. Previously missing from model
!    data. Tsf removed from LW structure.
! 2001/03/01, AS: Changed setting of R_Clear values. These are not present in
!    the RTM struct as they are not read from the RTM data file.
! 2001/03/06, AS: Tsf required in RTM struct again.
! 2001/03/08, AS: Now sets dB_dTs in LW sub-struct. Added SAD_Chan argument
!    (required by T2R for dB_dTs calculation).
! 2001/03/30, AS: Using whole array operations in the interpolations across all
!    pressure levels and channels to improve performance. Bs calculation
!    removed. Bs is not used by subsequent code.
! 2001/07/20, AS: dB_dTs calculation fixed: was using no. of thermal channels
!    from SPixel, should have used Ctrl values as the RTM Lw arrays are all
!    allocated to size Ctrl%Ind%NThermal and left at this size for the whole
!    image.
!    **************** ECV work starts here *************************************
! 2011/02/21, AS: Re-introducing changes made in late 2001/2002.
! 2003/12/12, CP: Added geopotential height.
! 2011/09/22, CP: Added in swrtm variables.
! 2011/11/07, CP: Tidied up comments. no actual change.
! 2012/09/15, MS: Puts in coefs fudge to account for missing values.
! 2012/11/03, MJ: Changed Coeffs if block.
! 2014/07/23, AP: Grid no longer assumed to defined points rather than the
!    cells centres (as is actually the case).
! 2014/07/23, CP: Added in extrapolation into stratosphere.
! 2014/07/30, GM: Cleaned up the code.
! 2014/07/30, GM: Put the duplicate interpolation code into subroutines.
! 2014/08/19, AP: Using the preprocessor's interpolation routines.
! 2014/09/28, GM: Updated to conform with a new arrangement of dimensions.
! 2014/11/11, CP: Updated to include boundary layer cth correction to
!    temperature profile
! 2015/01/12, AP: Replace ThermalFirst:ThermalLast indexing with YThermal.
! 2015/01/30, AP: Remove skint and sp as redundant. Use bottom of T and P
!    arrays in SPixel%RTM instead.
! 2015/02/06, AP: Move T profile alteration to Int_CTP.
! 2015/03/11, GM: Do not interpolate wavelength dependent fields if the
!    number of measurements is equal to zero.
!
! $Id$
!
! Bugs:
! None known.
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
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
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
