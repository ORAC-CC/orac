!-------------------------------------------------------------------------------
! Name: compute_fth.F90
!
! Purpose:
! Compute FTH (free troposphere relative humidity) from input profile of
! pressure, temperature, and specific humidity. FTH is computed at the 850 hPa
! level. Vapour pressures for air and saturation are computed from temperature
! and humidity interpolated to this level.
!
! Inputs:
! temperature (K)
! pressure (hPa)
! specific humidity (kg/kg)
!
! Output:
! free troposphere humidity
!
! History:
! 2016/02/18, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine compute_fth(nlm,P,T,Q,FTH)

   implicit none

   ! Input arguments
   integer, intent(in) :: nlm
   real, intent(in), dimension(nlm+1) :: &
      P, & ! pressure profile at SAT. pixel          (hPa).
      T, & ! temperature profile at SAT. pixel       (K).
      Q    ! specific humidity profile at SAT. pixel (kg/kg).

   ! Output arguments
   real, intent(out) :: FTH

   ! Local variables
   real :: m(1),T850(1),Q850(1),Es850(1),E850(1)
   integer :: id1(1),id2(1)
   real, parameter :: Ep=0.622

   ! Determine Temperature at 850 hPa level
   ! Check that the lowest layer pressure > 850
   if (maxval(P) .gt. 850.) then
      id1 = MINLOC(P,MASK=(P>800))
      id2 = MAXLOC(P,MASK=(P<900))

      ! Interpolate Temperature
      m = (T(id2)-T(id1))/(P(id2)-P(id1))
      T850 = m*850.+T(id1)-m*P(id1)

      ! Interpolate Humidity
      m = (Q(id2)-Q(id1))/(P(id2)-P(id1))
      Q850 = m*850.+Q(id1)-m*P(id1)

      E850 = (Q850*850.)/(Ep*(1-Q850)+Q850)
      Es850=6.112*EXP( 17.67*(T850-273.) / ( (T850-273.)+243.5 ) ) ![hPa]
      FTH = E850(1) / ES850(1)
!     print*,'FTH = ',FTH
   end if

end subroutine compute_fth
