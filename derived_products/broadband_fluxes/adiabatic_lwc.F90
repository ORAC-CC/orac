!-------------------------------------------------------------------------------
! Name: adiabatic_lwc.F90
!
! Purpose:
! Theoretical calculation of the adiabatic rate of increase of liquid water
! content with respect to height. This quantity is a function of temperature and
! pressure. It can be used to derive the adiabatic liquid water path/content in
! clouds, whereby the general form is: LWP = 1/2 * fad * gamma_ad * h^2, where
! fad is the degree of adiabaticity, gamma_ad is the adiabatic rate of increase
! of liquid water content with respect to height (calculated here) and h is the
! cloud thickness. Robert Wood provides a nice description on his website:
! 'Relationships between optical depth, liquid water path, droplet concentration
! and effective radius in an adiabatic layer cloud.
!
! Inputs:
! temperature (K)
! pressure (hPa)
!
! Output:
! Adiabatic rate of increase of liquid water content with respect to height
! (g/m3/km).
!
! History:
! 2016/01/13, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine adiabatic_lwc(T,P,dw_dz)

   use common_constants_m

   implicit none

   real, intent(in) :: t,p
   real, intent(out) :: dw_dz

   real, parameter :: &
      g=g_wmo , & ! gravitational acceleration [m/s2]
      cp=1005., & ! specific heat of air [J/kgK]
      Lv=2.5e6, & ! latent heat of vaporization [J/kg]
      Rd=287. , & ! dry air gas constant [J/kgK]
      Rv=461.5    ! moist air gas constant [J/kgK]

   real :: &
      ep         , & ! ratio of dry air to moist air constants
      gamma_d    , & ! dry air temperature lapse rate [K/km]
      Es         , & ! saturation vapor pressure
      rho        , & ! density of air
      ws         , & ! water vapor mixing ratio [g/kg]
      numerator  , & ! numerator part of Clausias Clapyeron Equation
      denominator, & ! denominator part of Clausias Clapyeron Equation
      gamma_m        ! moist adiabatic lapse rate

   ep=Rd/Rv
   gamma_d=g/Cp*1000.

   ! Saturation vapor pressure at temperature T
   Es=6.112*EXP( 17.67*(T-273.) / ( (T-273.)+243.5 ) ) ![hPa]
   rho = P*100./(Rd*T)
   ws = ep*Es/(P-Es)

   ! Calculate Saturated Adiabatic Lapse Rate
   numerator   = ( 1 + (Lv*ws)/(Rd*T) )
   denominator = ( 1 + ( (Lv*Lv)*ep*ws)/(Rd*Cp*(T*T)) )
   gamma_m = gamma_d * (numerator/denominator)
   dw_dz = (Cp/Lv) * (gamma_d - gamma_m) * rho * 1000. ! [g/m3/km]

end subroutine adiabatic_lwc
