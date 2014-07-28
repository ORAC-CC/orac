! Name: cox_munk.f90
!
! See below for main cox_munk subroutine....
!
! zeisse_ba function
!
! Purpose:
! Calculates the B/A value defined in the paper:
! Zeisse, C.R.: Radiance of the ocean horizon, J. Opt. Soc. Am. A, 12,
!    2022-2030, doi:10.1364/JOSAA.12.002022, 1995.
! This value is a correction to the cos(theta) value used in the
! Cox-Munk glint-calculations.
!
! Return value:
! Name Type    Description
! ba   real*4  The so-called "area of the ergodic cap"
!
! Arguments:
! Name Type    In/Out/Both Description
! theta real*4     In      view-angle (in radians)
! ws    real*4     In      wind-speed (in m/s)
! Note: Both input arrays should be vectors with an equal number of
!       elements
!
! Local variables:
! Name Type Description
!
!
! History:
! 16 Apr 2012 Gareth Thomas: Adapted from the cox_munk.pro and
!                            zeisse_ba.pro IDL functions
! 24 Apr 2012 Gareth Thomas: Added estimates for coefficients (refractive
!                            index, absorption coefficient etc) at 3.7 um
! 30 Jul 2012 C. Poulsen     Initialised all allocated variables
! 30 Aug 2012 Gareth Thomas: Made some corrections to underlight calculations
! 30 Aug 2012 Gareth Thomas: Changed call to zeisse_ba function so that
!                            theta is passed in Radians as is expected by the
!                            function
! 14 Mar 2013 Gareth Thomas: Altered calculation of wind direction to be
!                            asin(v10/ws). Under the assumption that v10
!                            North pointing and u10 is East pointing, this
!                            should give angle of wind from North.
! 20 Mar 2013 Gareth Thomas: The above (14/03) change is clearly wrong. Fixed
!                            (i.e. wd=acos(v10/ws)). Also put wd onto the
!                            -180 to 180 degree interval (rather than 0 to 360)
!                            as this is what the satellites use.
! 21 May 2013 Gareth Thomas: Altered the relative azimuth used in within
!                            this routine to be the reverse (i.e. 180-azi)
!                            to that used elsewhere. ORAC (and RT in general)
!                            assumes that forward scatter equates to azi=0,
!                            while the Cox and Munk expressions treat
!                            backscatter as azi=0.
! 30 Oct 2013 Matthias Jerg  Corrects datatypes for variable initilizations and
!                            implements quick fix to avoid div by 0
! 28 Jan 2014 Matthias Jerg  Fixes div by zero
! 17 Jun 2014 Greg McGarragh Fixed a subtle indexing bug that would only show up
!                            if the 0.65um channel was *not* being used.
! 12 Jun 2014 Adam Povey     Tidying and fixing div by 0 coding bugs.
! 27 Jul 2014 Greg McGarragh Bug fix: rsolaz was being used uninitialized.  Now
!                            it is appropriately set to zero.
! 27 Jul 2014 Greg McGarragh Bug fix: w was not initialized for the computation
!                            of the coefficients of Fresnel's equation.
!
!
! $Id$
!
! Bugs:
!
!none known

module cox_munk_m

implicit none

contains

! Main cox_munk subroutine
!
! Purpose:
! Calculate ocean surface reflectance, including white-caps, surface
! glint and under-light.
! At present Chlorophyll-A and CDOM concentrations are not taken into
! account - something for the future.
!
! Description and Algorithm details:
! For details of the algorithm used here, see Andrew Sayer's DPhil thesis
! (University of Oxford, 2010), or:
! Sayer et al., Atmos. Meas. Tech., 3:813-838, 2010.
!
! Arguments:
! Name Type    In/Out/Both Description
! bands integer*2  In      Wavelength band index numbers
! solza real*4     In      Array of solar zenith angles (in degrees)
! satza real*4     In      Array of satellite zenith angles
! solaz real*4     In      Array of solar azimuth anlges (from north)
! relaz real*4     In      Array of relative azimuth angles (between sun
!                          and satellite)
! u10   real*4     In      Near surface (10m) East-West wind component
! v10   real*4     In      South-North wind component (both in m/s)
! rho   real*4     Out     The nbands x npoints output array of
!                          bi-directional reflectances
!
! Local variables:
! Name Type Description
!
subroutine cox_munk(bands, solza, satza, solaz, relaz, u10, v10, rho)

   use preproc_constants

   implicit none

   ! Input arguments
   integer(kind=stint), dimension(:), intent(in) :: bands
   real(kind=sreal), dimension(:), intent(in)    :: solza, satza
   real(kind=sreal), dimension(:), intent(in)    :: solaz, relaz
   real(kind=sreal), dimension(:), intent(in)    :: u10, v10

   ! Output arguments
   real(kind=sreal), dimension(:,:), intent(out) :: rho

   ! Local arguments
   ! Wavelength dependent constants
   real(kind=sreal)               :: lambda(4)
   real(kind=sreal)               :: nr(4), ni(4)
   real(kind=sreal)               :: baseabs(4), basebsc(4)
   real(kind=sreal)               :: chlabs(4), chlbsc(4)
   real(kind=sreal)               :: eta_oc(4), totbsc(4)
   real(kind=sreal)               :: Rwc(4)
   ! Other constants
   real(kind=sreal)               :: n_air, chlconc
   integer(kind=lint)             :: npts, nbands
   ! Wind speed and direction
   real(kind=sreal), allocatable  :: ws(:), wd(:)
   ! White cap reflection variables
   real(kind=sreal), allocatable  :: wcfrac(:)
   real(kind=sreal), allocatable  :: rhowc(:,:)
   ! Under-light reflection variables
   real(kind=sreal), allocatable  :: f(:)
   real(kind=sreal), allocatable  :: R_wb(:)
   real(kind=sreal)               :: t_u, r_u
   real(kind=sreal), allocatable  :: t_d(:)
   real(kind=sreal), allocatable  :: rhoul(:,:)
   ! Snell's law and Fresnel's equation
   real(kind=sreal), allocatable  :: w(:), wprime(:)
   real(kind=sreal), allocatable  :: a1(:), b1(:), c1(:), d1(:)
   ! Sun-glint/Cox and Munk variables
   real(kind=sreal), allocatable  :: rsolza(:), rsatza(:)
   real(kind=sreal), allocatable  :: rrelaz(:), rsolaz(:)
   real(kind=sreal), allocatable  :: dangle(:)
   real(kind=sreal), allocatable  :: Zx(:), Zy(:)
   real(kind=sreal), allocatable  :: Zxprime(:), Zyprime(:)
   real(kind=sreal), allocatable  :: sigx(:), sigy(:)
   real(kind=sreal), allocatable  :: zeta(:), eta(:)
   real(kind=sreal), allocatable  :: p(:)
   real(kind=sreal), allocatable  :: cosomega(:), cosbeta(:)
   real(kind=sreal), allocatable  :: R_sf(:)
   real(kind=sreal), allocatable  :: ergodic(:)
   real(kind=sreal), allocatable  :: rhogl(:,:)

   integer(kind=lint)             :: i

   ! Coefficients for needed by the model at "heritage wavelengths"
   ! Values for 0.65, 0.87 and 1.6 microns are taken from Sayer et al.
   ! while values for 3.7 microns are taken from references there-in
   ! or estimated from values at lower wavelengths
   lambda  = (/ 0.65,    0.87,    1.60,    3.7     /)
   ! Clean sea water refractive indices
   nr      = (/ 1.338,   1.334,   1.323,   1.374   /)
   ni      = (/ 2.23e-8, 3.91e-7, 8.55e-5, 3.60e-3 /)
   ! Water absorption at these wavelengths
   baseabs = (/ 0.410,   5.65,    672.0,   1.22e4  /)
   ! Water back-scattering coefficients
   basebsc = (/ 8.87e-4, 2.65e-4, 1.91e-5, 5.00e-7 /)
   ! White cap reflectance at same wavelengths
   Rwc     = (/ 0.4,     0.24,    0.06,    0.0      /)
   ! For future reference, Andy Sayer's values for 0.55 microns are:
   ! nr = 1.341, ni = 1.96e-9, baseabs = 0.064, basebsc = 1.93e-3
   ! Rwc = 0.4
   ! Approximate median chlorophyll-A concentration from GlobCOLOUR
   chlconc = 0.18 ! (mg/m3)
   ! Calculate the chl-a absorption coeffecient from the concentration
   ! value. This is only relevant at 0.67 (and 0.55) microns. If 0.55
   ! is needed, the two coefficients are 2.79e-3 & 0.0064
   ! The equation comes from Sathyendranath, while the coefficients are
   ! taken  from Devred et al. 2006
   chlabs = (/ 5.46e-3,  0.0,     0.0,     0.0 /)*(1.0-exp(-1.61*chlconc)) + &
        (/ 8.50e-3,  0.0,     0.0,     0.0 /)*chlconc
   ! Next define the back-scattering coefficients for chl-a
   chlbsc = 0.02*(0.5-0.25*log10(chlconc))* 0.55/lambda + 0.002

   ! Approximate median CDOM absorption from GlobCOLOUR. Note that it is
   ! only significant at 0.55 microns, so can be neglected unless that
   ! channel is incorporated
   !cdomabs = (/ 0.013,   0.0,     0.0,     0.0,     0.0 /)

   ! **** End of model constants ****

   ! Number of points and wavelength bands passed to the subroutine
   nbands = size(bands)
   npts   = size(solza)

   ! Now allocate the local variables
   ! Reflectance components
   allocate(rhowc(nbands,npts))
   allocate(rhoul(nbands,npts))
   allocate(rhogl(nbands,npts))
   ! Local variables
   allocate(ws(npts))
   allocate(wd(npts))
   allocate(wcfrac(npts))
   allocate(f(npts))
   allocate(R_wb(npts))
   allocate(t_d(npts))
   allocate(w(npts))
   allocate(wprime(npts))
   allocate(a1(npts))
   allocate(b1(npts))
   allocate(c1(npts))
   allocate(d1(npts))
   allocate(rsolza(npts))
   allocate(rsatza(npts))
   allocate(rrelaz(npts))
   allocate(rsolaz(npts))
   allocate(dangle(npts))
   allocate(Zx(npts))
   allocate(Zy(npts))
   allocate(Zxprime(npts))
   allocate(Zyprime(npts))
   allocate(sigx(npts))
   allocate(sigy(npts))
   allocate(zeta(npts))
   allocate(eta(npts))
   allocate(p(npts))
   allocate(cosomega(npts))
   allocate(cosbeta(npts))
   allocate(R_sf(npts))
   allocate(ergodic(npts))

   ! Calculate windspeed and direction from wind components
   ws = sqrt(u10*u10 + v10*v10) ! Wind speed in m/s
   wd = acos(v10/ws) ! Wind angle in radians from north
   where(u10 .lt. 0.0) wd = -wd ! Azimuth angle on -180 - 180 degree interval

   ! Calculate white-cap fraction (and ensure it is no greater than 1)
   wcfrac = 2.951e-6 * (ws**3.52)
   where(wcfrac .gt. 1.0) wcfrac = 1.0

   ! White-cap contribution to the reflectance
   do i=1,nbands
      rhowc(i,:) = wcfrac*Rwc(bands(i))
   end do

   ! Generate versions of the viewing geometry in radians
   rsolza = d2r * solza
   rsatza = d2r * satza

   rsolaz = 0.

   ! Next, calculate the reflectance of the water body, using the average
   ! CHL concentration and CDOM absorption defined above.
   ! Combine the various scattering coefficients defined above to give the
   ! total
   totbsc = 0.5*basebsc + chlbsc * 0.3*chlconc**0.62
   eta_oc = 0.5*basebsc / totbsc
   ! Upward transmission and reflectance can be taken as constant at
   ! wavelengths where underlight is significant
   t_u = 0.52
   r_u = 1.0 - t_u
   n_air = 1.00029 ! Refractive index of air
   do i=1,nbands
      ! The so-called "coefficient of R"
      f = 0.6279 - (0.2227*eta_oc(bands(i))) - &
           ( 0.00513*eta_oc(bands(i)) * eta_oc(bands(i)) ) + &
           ( 0.2465 *eta_oc(bands(i)) - 0.3119 )*cos(rsolza)
      ! Now calculate the water body reflectance, which is the coefficient
      ! of R * the backscatter, devided by the absorption (note if CDOM is
      ! included, it will appear on the bottom line of this equation)
      R_wb = f*totbsc(bands(i)) / (baseabs(bands(i)) + chlabs(bands(i)))
      ! Now we need to use the Fresnel equation and Snell's Law to calculate
      ! how much light actually enters the water body through the surface (t_d)
      ! Snell's law
      w = satza(:)*d2r
      wprime = asin(n_air*sin(rsatza) / nr(bands(i)))
      ! Coefficients of Fresnel's equation
      a1 = sin(w - wprime)
      b1 = sin(w + wprime)
      c1 = tan(w - wprime)
      d1 = tan(w + wprime)
      ! Fresnel's equation (checking for divide by 0)
      where (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more)
         t_d = 1.0 - 0.5*( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )
      elsewhere
         t_d = 0.0
      end where

      ! Combine the surface transmission terms with the underlight
      ! reflectance to give total underlight contribution
      rhoul(i,:) = (t_u * t_d * R_wb(:)) / (1.0 - r_u*R_wb(:))
   enddo

   ! Now calculate the reflectance from the wind-roughened surface. This
   ! is the actual Cox and Munk bit
   ! Note that the relative azimuth used in Cox and Munk is the other
   ! way round from the convention used in the rest of ORAC. Here
   ! backscattering equates to a zero relative azimuth (i.e. if the
   ! satellite is looking away from the sun, azi = 0). Hence the 180
   ! degree correction.
   rrelaz = d2r * (180. - relaz) ! relative azimuth

   ! Convert wind direction to be relative to solar azimuth
   wd = rsolaz - wd
   where(wd .lt. 0.0) wd = 2.0*pi + wd

   ! Define surface slopes
   dangle = cos(rsolza) + cos(rsatza)
   where (abs(dangle) .gt. dither_more)
      Zx = (-1.0*sin(rsatza) * sin(rrelaz)) / dangle
      Zy = (sin(rsolza) + sin(rsatza) * cos(rrelaz)) / dangle
   elsewhere
      Zx = 0.0
      Zy = 0.0
   end where
   Zxprime = cos(wd)*Zx + sin(wd)*Zy
   Zyprime = -sin(wd)*Zx + cos(wd)*Zy

   ! Cox and Munk (1954) (Statistics of...) coefficients
   sigx = sqrt(0.003 + 0.00192*ws)
   sigy = sqrt(0.00316*ws)

   zeta = Zxprime / sigx
   eta  = Zyprime / sigy

   ! Slope distribution
   p = exp(-0.5*(zeta*zeta + eta*eta)) / (2.0*pi*sigx*sigy)

   ! Cox and Munk (1954) (Measurements of...)
   ! 2*omega = angle between incident light and intstrument, wrt the
   ! sloping sea surface
   cosomega = cos(rsatza)*cos(rsolza) + sin(rsatza)*sin(rsolza)*cos(rrelaz)
   where (abs(cosomega+1.0) .gt. dither_more)
      cosbeta  = (cos(rsolza) + cos(rsatza)) / sqrt(2.0 + 2.0*cosomega)
   elsewhere
      cosbeta = 0.0
   end where
   w = 0.5*acos(cosomega)

   ! Apply the correction of Zeisse (1995) for zenith angles greater
   ! than 70 degrees. This replaces cos(satza) in the glint calculation
   ! by the area of the "ergodic cap"
   ergodic = zeisse_ba(rsatza, ws)

   ! Now, again cycling through the wavelengths, use Fresnel to determine
   ! the reflectance of the roughned surface
   do i=1,nbands
      wprime = asin(n_air*sin(w) / nr(bands(i)))
      a1     = sin(w - wprime)
      b1     = sin(w + wprime)
      c1     = tan(w - wprime)
      d1     = tan(w + wprime)

      where (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more)
         R_sf = 0.5*((a1*a1)/(b1*b1) + (c1*c1)/(d1*d1))
      elsewhere
         R_sf = 0.0
      end where

      ! Calculate glint reflectance contribution
      dangle = 4.0 * cos(rsolza) * ergodic * (cosbeta**4)
      where (abs(dangle) .gt. dither_more)
         rhogl(i,:) = pi * p * R_sf / dangle
      elsewhere
         rhogl(i,:) = 0.0
      end where
   end do

   ! Add all the reflectance terms to produce the full surface reflectance
   do i=1,npts
      rho(:,i) = rhowc(:,i) + (1+wcfrac(i))*(rhogl(:,i) + rhoul(:,i))
   end do

   ! Deallocate local dynamic arrays
   deallocate(rhowc)
   deallocate(rhoul)
   deallocate(rhogl)
   deallocate(ws)
   deallocate(wd)
   deallocate(wcfrac)
   deallocate(f)
   deallocate(R_wb)
   deallocate(t_d)
   deallocate(w)
   deallocate(wprime)
   deallocate(a1)
   deallocate(b1)
   deallocate(c1)
   deallocate(d1)
   deallocate(rsolza)
   deallocate(rsatza)
   deallocate(rrelaz)
   deallocate(rsolaz)
   deallocate(dangle)
   deallocate(Zx)
   deallocate(Zy)
   deallocate(Zxprime)
   deallocate(Zyprime)
   deallocate(sigx)
   deallocate(sigy)
   deallocate(zeta)
   deallocate(eta)
   deallocate(p)
   deallocate(cosomega)
   deallocate(cosbeta)
   deallocate(R_sf)
   deallocate(ergodic)

end subroutine cox_munk


function zeisse_ba(theta, ws) result (ba)

   use  preproc_constants

   implicit none

   ! Input variables
   real(kind=sreal), dimension(:), intent(in) :: theta
   real(kind=sreal), dimension(:), intent(in) :: ws
   ! Return value
   real(kind=sreal), allocatable, dimension(:) :: ba
   ! Local variables
   integer(kind=lint)                         :: npoints
   real(kind=sreal), allocatable              :: tp(:),wp(:)
   real(kind=sreal), allocatable              :: tp2(:),wp2(:)
   real(kind=sreal), allocatable              :: delta(:)
   real(kind=sreal)                           :: C(3,3)
   ! C defines the coefficients of the delta polynomial as given by
   ! the Zeisse paper
   C(:,1) = (/  1.67530e-3, -1.66517e-4,  2.03068e-5 /)
   C(:,2) = (/ -6.96112e-3, -5.55537e-3,  2.60686e-3 /)
   C(:,3) = (/  2.86324e-3,  1.86059e-3, -4.69589e-4 /)

   ! Allocate the output and local arrays, based on the size of the
   ! input arrays
   npoints = size(theta)
   allocate(ba(npoints)) ! Must be deallocated by calling routine
   allocate(tp(npoints))
   allocate(wp(npoints))
   allocate(tp2(npoints))
   allocate(wp2(npoints))
   allocate(delta(npoints))

   ! The delta correction is only applied at high solar zenith angles
   ! and non-neglibible wind strength, otherwise just use cos(theta)
   where((theta .ge. 70.0*d2r) .and. (ws .gt. 1.0))
      tp = (theta - 70.0*d2r) / 5.0
      wp = 4.0 * alog10(ws) / 1.30103

      wp2 = wp*wp
      tp2 = tp*tp

      delta =      C(1,1) + C(2,1)*wp + C(3,1)*wp2 +  &
              tp* (C(1,2) + C(2,2)*wp + C(3,2)*wp2) + &
              tp2*(C(1,3) + C(2,3)*wp + C(3,3)*wp2)
      ba = delta + cos(theta)
   elsewhere
      ba = cos(theta)
   end where

   ! Clean up local arrays
   deallocate(tp)
   deallocate(wp)
   deallocate(tp2)
   deallocate(wp2)
   deallocate(delta)

end function zeisse_ba

end module cox_munk_m
