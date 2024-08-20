!-------------------------------------------------------------------------------
! Name: cox_munk.F90
!
! Purpose:
! Module of routines in this file calculate ocean BRDF parameters, including
! white-caps, surface glint and under-light. The code makes use of total water
! absorption and backscatter values from an ocean_colour_t structure (provided
! by the ocean_colour_m module), which can either contain actual data, or just
! makes use of the default parameters provided by cox_monk_constants_m
!
! Description and Algorithm details:
! For details of the algorithm used here, see Andrew Sayer's DPhil thesis
! (University of Oxford, 2010), or:
! Sayer, A. M., G. E. Thomas, and R. G. Grainger. 2010. "A Sea Surface
! Reflectance Model for (A)ATSR, and Application to Aerosol Retrievals."
! Atmospheric Measurement Techniques 3: 813–838. doi:10.5194/amt-3-813-2010.
!
! Presently four different versions exist:
!
! cox_munk(): This is the legacy version that takes as input an array of bands
!    and arrays of properties for any number of points and returns a 2d array
!    of bidirectional reflectances for each band and point. This version is
!    extremely memory inefficient as all computations are vectorized and many
!    local variables are arrays of length n_points.
!
! cox_munk2(): This version is identical to the cox_munk() but only computes
!    bidirectional reflectance for one band and one point. This version is
!    memory efficient as it is only scaler but slightly slower when used for
!    many points due to function overhead and no vectorization.
!
! cox_munk3(): This version is scaler like cox_munk2() but isolates computations
!    related to wavelength from computations related to geometry and wind. This
!    is efficient for computing bidirectional reflectance for many bands and is
!    ideal for calculating rho_0d and rho_dv. It requires as input the
!    structures
!       cox_munk_shared_geo_wind_t,
!    computed with
!       cox_munk3_calc_shared_geo_wind(),
!    respectively.
!
! cox_munk4() This version is scaler like cox_munk2() but isolates computations
!    related to wind from computations related to wavelength and geometry. This
!    is efficient for computing bidirectional reflectance for many wind
!    conditions and is *very* ideal for calculating rho_dd where the geometry
!    is the same for each pixel as a consistent set of quadrature points. It
!    requires as input the structures
!       cox_munk_shared_wind_t,
!       cox_munk_shared_band_geo_t,
!    computed with
!       cox_munk4_calc_shared_wind(),
!       cox_munk4_calc_shared_band_geo(),
!    respectively.
!
! In addition, the higher level subroutine cox_munk_rho_0v_0d_dv_and_dd()
! computes rho_0v, rho_0d, rho_dv, and, rho_dd for any number of points.
!
! History:
! 2012/04/16, GT: Adapted from the cox_munk.pro and zeisse_ba.pro IDL functions.
! 2012/04/24, GT: Added estimates for coefficients (refractive index, absorption
!    coefficient etc) at 3.7 um.
! 2012/07/30, CP: Initialised all allocated variables.
! 2012/08/30, GT: Made some corrections to underlight calculations
! 2012/08/30, GT: Changed call to zeisse_ba function so that theta is passed in
!    Radians as is expected by the function.
! 2013/03/14, GT: Altered calculation of wind direction to be asin(v10/ws).
!    Under the assumption that v10 is north pointing and u10 is east pointing,
!    this should give angle of wind from North.
! 2013/03/20, GT: The above (14/03) change is clearly wrong. Fixed (i.e.
!    wd=acos(v10/ws)). Also put wd onto the -180 to 180 degree interval (rather
!    than 0 to 360) as this is what the satellites use.
! 2013/05/21, GT: Altered the relative azimuth used in within this routine to
!    be the reverse (i.e. 180-azi) to that used elsewhere. ORAC (and RT in
!    general) assumes that forward scatter equates to azi=0, while the Cox and
!    Munk expressions treat backscatter as azi=0.
! 2013/10/30, MJ: Corrects data types for variable initializations and
!    implements quick fix to avoid division by 0
! 2014/01/28, MJ: Fixes division by 0
! 2014/06/17, GM: Fixed a subtle indexing bug that would only show up if the
!    0.65um channel was *not* being used.
! 2014/06/12, AP: Tidying and fixing div by 0 coding bugs.
! 2014/07/27, GM: Bug fix: rsolaz was being used uninitialized. Now it is
!    appropriately set to zero.
! 2014/07/27, GM: Bug fix: w was not initialized for computation of the
!    coefficients of Fresnel's equation.
! 2014/08/10, GM: An extensive refactoring for speed and memory efficiency
!    adding subroutines cox_munk2(), cox_munk3(), cox_munk4(), and the higher
!    level routine cox_munk_rho_0v_0d_dv_and_dd().
! 2014/08/13, GM: Improve performance in calculating rho_0d and rho_dv with a
!    dynamically created look-up-table (LUT). This is initial. Performance can
!    probably be increased further using a higher dimension LUT while
!    performance for calculating rho_dd can be improved using a similar
!    technique.
! 2014/12/31, GM: Parallelized the main loops in the interface subroutine with
!    OpenMP.
! 2015/01/06, GM: Fixed a couple of bugs in the OpenMP parallelization that
!    come out when compiling with ifort.
! 2015/01/15, AP: Bug fix. Passing channel index into cox_munk3 wasn't
!    compatible with arbitrary channel ordering.
! 2015/03/10, GM: Added support for 4.69, 0.55, 1.24, and 2.13 um. Now AATSR
!    channel 1 and MODIS channels 3, 4, 5, and 7 will work. Also, corrected
!    some of the wavelength dependent constants.
! 2016/04/08, SP: Corrected 4.69 micron to 0.47 micron.
! 2016/07/12, GT: Made significant changes to enable the use of
!    OceanColour_cci data in determining the underlight contribution. Note this
!    required undoing some of the optimisation previously done by GM. In
!    particular:
!    - Removal of the shared_band structure and cox_munk3_shared_band()
!      subroutine. Calculation of underlight terms now carried out in cox_munk3
!      and cox_munk4 directly.
!    - Have moved the definition of the old constants used by cox_munk into
!      their own module: cox_munk_constants_m
!    - Calculation of underlight has been simplified, as we are now reading
!      total absorption and backscatter directly from the OCCCI data, rather
!      than chlorophyll concentration and cdom absorption
! 2017/08/09, GM: Switch from the NR routine gauleg() to the GPL compatible
!    gauss_leg_quadx() for computing Gauss-Legendre quadrature.
! 2020/07/16, AP: Invert the direction of Zy and the relative wd. The previous
!    expressions oriented the y-axis with sunlight rather than point it towards
!    the sun as ORAC would typically do. Details can be found at
!    http://eodg.atm.ox.ac.uk/eodg/gray/2020Povey_relative-azimuth-surface.pdf
!
! Bugs:
! As they aren't actually used by the code any more, the cox_munk() and
! cox_munk2() subroutines aren't fully up-to-date. Something on the to-do list.
!-------------------------------------------------------------------------------


#define COMPATIBILITY_MODE


module cox_munk_m

   use cox_munk_constants_m
   use preproc_constants_m
   use ocean_colour_m

   implicit none

   private

   public :: cox_munk_shared_geo_wind_t, &
             cox_munk_shared_wind_t, &
             cox_munk_shared_band_geo_t, &
             cox_munk, &
             cox_munk2, &
             cox_munk3_calc_shared_geo_wind, &
             cox_munk3, &
             cox_munk4_calc_shared_wind, &
             cox_munk4_calc_shared_band_geo, &
             cox_munk4, &
             cox_munk_rho_0v_0d_dv_and_dd

   ! Note that the cox_munk band definitions and default optical
   ! properties are now in a separate module cox_munk_constants_m

   type cox_munk_shared_geo_wind_t
      real(kind=sreal) :: satza
      real(kind=sreal) :: cos_solza
      real(kind=sreal) :: sin_solza
      real(kind=sreal) :: cos_satza
      real(kind=sreal) :: sin_satza
      real(kind=sreal) :: cos_relaz
      real(kind=sreal) :: sin_relaz
      real(kind=sreal) :: wcfrac
      real(kind=sreal) :: p
      real(kind=sreal) :: cosbeta
      real(kind=sreal) :: w
      real(kind=sreal) :: sin_w
      real(kind=sreal) :: ergodic
      real(kind=sreal) :: a
   end type cox_munk_shared_geo_wind_t

   type cox_munk_shared_wind_t
      real(kind=sreal) :: ws
      real(kind=sreal) :: wd
      real(kind=sreal) :: wcfrac
      real(kind=sreal) :: rhowc
      real(kind=sreal) :: cos_wd
      real(kind=sreal) :: sin_wd
      real(kind=sreal) :: sigx
      real(kind=sreal) :: sigy

   end type cox_munk_shared_wind_t

   type cox_munk_shared_band_geo_t
      real(kind=sreal) :: satza
      real(kind=sreal) :: cos_solza
      real(kind=sreal) :: sin_solza
      real(kind=sreal) :: cos_satza
      real(kind=sreal) :: sin_satza
      real(kind=sreal) :: cos_relaz
      real(kind=sreal) :: sin_relaz
      real(kind=sreal) :: Zx
      real(kind=sreal) :: Zy
      real(kind=sreal) :: cosbeta
      real(kind=sreal) :: R_sf
   end type cox_munk_shared_band_geo_t

contains

!-------------------------------------------------------------------------------
! Name: cox_munk()
!
! Purpose:
! Calculate ocean surface reflectance, including white-caps, surface glint and
! under-light. Previous version used a climatological average of chlorophyll-a
! concentration and CDOM absorption for underlight calculation, which has been
! replaced by usage of total absorption and backscatter from Ocean-colour cci.
!
! Description and Algorithm details:
! For details of the algorithm used here, see Andrew Sayer's DPhil thesis
! (University of Oxford, 2010), or:
! Sayer, A. M., G. E. Thomas, and R. G. Grainger. 2010. "A Sea Surface
! Reflectance Model for (a)ATSR, and Application to Aerosol Retrievals."
! Atmospheric Measurement Techniques 3: 813–838. doi:10.5194/amt-3-813-2010.
!
! Arguments:
! Name  Type    In/Out/Both Description
! bands integer In          Wavelength band index numbers
! solza sreal   In          Array of solar zenith angles (in degrees)
! satza sreal   In          Array of satellite zenith angles
! solaz sreal   In          Array of solar azimuth angles (from north)
! relaz sreal   In          Array of relative azimuth angles (between sun and
!                           satellite)
! totbsc sreal  In          nbands x npoints array of total water backscatter
!                           coefficients (1/m)
! totabs sreal  In          nbands x npoints array of total water absorption
!                           coefficients (1/m)
! u10   sreal   In          Near surface (10m) East-West wind component (m/s)
! v10   sreal   In          Near surface (10m) South-North wind component (m/s)
! rho   sreal   Out         The nbands x npoints output array of bi-directional
!                           reflectances
!
! Local variables:
! Name Type Description
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine cox_munk(bands, solza, satza, solaz, relaz, totbsc, totabs, u10, v10, rho)

   use preproc_constants_m

   implicit none

   ! Input arguments
   integer,          intent(in)     :: bands(:)
   real(kind=sreal), intent(in)     :: solza(:), satza(:)
   real(kind=sreal), intent(in)     :: solaz(:), relaz(:)
   real(kind=sreal), intent(in)     :: totbsc(:,:), totabs(:,:)
   real(kind=sreal), intent(in)     :: u10(:), v10(:)

   ! Output arguments
   real(kind=sreal), intent(out)    :: rho(:,:)

   ! Local variables
   integer                          :: i

   integer                          :: npts, nbands

   ! Wavelength dependent constants (derived)
!  real(kind=sreal)                 :: chlabs(n_lambda), chlbsc(n_lambda)
!  real(kind=sreal)                 :: totbsc(n_lambda), eta_oc(n_lambda)

   ! Wind speed and direction
   real(kind=sreal), allocatable    :: ws(:), wd(:)

   ! White cap reflection variables
   real(kind=sreal), allocatable    :: wcfrac(:)
   real(kind=sreal), allocatable    :: rhowc(:,:)

   ! Under-light reflection variables
   real(kind=sreal), allocatable    :: eta_oc(:)
   real(kind=sreal), allocatable    :: f(:)
   real(kind=sreal), allocatable    :: R_wb(:)
   real(kind=sreal)                 :: t_u, r_u
   real(kind=sreal), allocatable    :: t_d(:)
   real(kind=sreal), allocatable    :: rhoul(:,:)

   ! Snell's law and Fresnel's equation
   real(kind=sreal), allocatable    :: w(:), wprime(:)
   real(kind=sreal), allocatable    :: a1(:), b1(:), c1(:), d1(:)

   ! Sun-glint/Cox and Munk variables
   real(kind=sreal), allocatable    :: rsolza(:), rsatza(:)
   real(kind=sreal), allocatable    :: rsolaz(:), rrelaz(:)

   real(kind=sreal), allocatable    :: dangle(:)
   real(kind=sreal), allocatable    :: Zx(:), Zy(:)
   real(kind=sreal), allocatable    :: Zxprime(:), Zyprime(:)
   real(kind=sreal), allocatable    :: sigx(:), sigy(:)
   real(kind=sreal), allocatable    :: zeta(:), eta(:)
   real(kind=sreal), allocatable    :: p(:)
   real(kind=sreal), allocatable    :: cosomega(:), cosbeta(:)
   real(kind=sreal), allocatable    :: ergodic(:)
   real(kind=sreal), allocatable    :: R_sf(:)
   real(kind=sreal), allocatable    :: rhogl(:,:)


   !----------------------------------------------------------------------------
   ! Calculation of the absroption and backscatter no-longer needed with use
   ! of Ocean Colour cci data...
   !----------------------------------------------------------------------------
   ! Calculate the chl-a absorption coefficient from the concentration value.
!  chlabs = coef1*(1.0-exp(-1.61*chlconc)) + coef2*chlconc

   ! Next define the back-scattering coefficients for chl-a
!  chlbsc = 0.02*(0.5-0.25*log10(chlconc))* 0.55/lambda + 0.002

   ! Approximate median CDOM absorption from GlobCOLOUR. Note that it is only
   ! significant at 0.55 microns, so can be neglected unless that channel is
   ! incorporated
!  cdomabs = (/ 0.013,   0.0,     0.0,     0.0,     0.0 /)


   ! Number of points and wavelength bands passed to the subroutine
   nbands = size(bands)
   npts   = size(solza)


   !----------------------------------------------------------------------------
   ! Now allocate the local variables
   !----------------------------------------------------------------------------

   ! Local variables
   allocate(ws(npts))
   allocate(wd(npts))
   allocate(wcfrac(npts))
   allocate(eta_oc(npts))
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
   allocate(rsolaz(npts))
   allocate(rrelaz(npts))
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

   ! Reflectance components
   allocate(rhowc(nbands,npts))
   allocate(rhoul(nbands,npts))
   allocate(rhogl(nbands,npts))


   !----------------------------------------------------------------------------
   ! Calculate wind speed and direction from wind components
   !----------------------------------------------------------------------------
   ws = sqrt(u10*u10 + v10*v10) ! Wind speed in m/s
   wd = acos(v10/ws)            ! Wind angle in radians from north
   where(u10 .lt. 0.0) wd = -wd ! Azimuth angle on -180 - 180 degree interval


   !----------------------------------------------------------------------------
   ! Calculate white-cap fraction (and ensure it is no greater than 1)
   !----------------------------------------------------------------------------
   wcfrac = 2.951e-6 * (ws**3.52)
   where(wcfrac .gt. 1.0) wcfrac = 1.0

   do i = 1, nbands
      ! White-cap contribution to the reflectance
      rhowc(i,:) = wcfrac*Rwc(bands(i))
   end do


   !----------------------------------------------------------------------------
   ! Next, calculate the reflectance of the water body, using the average CHL
   ! concentration and CDOM absorption defined above. Combine the various
   ! scattering coefficients defined above to give the total
   !----------------------------------------------------------------------------
   !----------------------------------------------------------------------------
   ! Ocean colour cci provides spectrally resolved total backscatter and
   ! absorption values, so we no-longer need to calculate it from chlorophyll
   ! concentration
   !----------------------------------------------------------------------------
!  totbsc = 0.5*basebsc + chlbsc * 0.3*chlconc**0.62

   do i = 1, nbands
      ! The so-called "coefficient of R"
      eta_oc = 0.5*basebsc(bands(i)) / totbsc(i,:)
      f = 0.6279 - (0.2227*eta_oc(bands(i))) - &
          ( 0.00513*eta_oc * eta_oc ) + &
          ( 0.2465 *eta_oc - 0.3119 )*cos(solza*d2r)

      ! Now calculate the water body reflectance, which is the coefficient of
      ! R * the backscatter, divided by the absorption (note if CDOM is
      ! included, it will appear on the bottom line of this equation)
!     R_wb = f*totbsc(bands(i)) / (baseabs(bands(i)) + chlabs(bands(i)))
      R_wb = f*totbsc(i,:) / totabs(i,:)

      ! Now we need to use the Fresnel equation and Snell's Law to calculate how
      ! much light actually enters the water body through the surface (t_d)
      ! Upward transmission and reflectance can be taken as constant at
      ! wavelengths where underlight is significant
      t_u = 0.52
      r_u = 1.0 - t_u

      ! Snell's law
      w = satza(:)*d2r
      wprime = asin(n_air*sin(w) / nr(bands(i)))

      ! Coefficients of Fresnel's equation
      a1 = sin(w - wprime)
      b1 = sin(w + wprime)
      c1 = tan(w - wprime)
      d1 = tan(w + wprime)

      ! Fresnel's equation
!     t_d = 1.0 - 0.5 * ( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )

      ! This version explicitly catches division by 0
      where (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more)
         t_d = 1.0 - 0.5*( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )
      else where
         t_d = 0.0
      end where

      ! Combine the surface transmission terms with the underlight reflectance
      ! to give total underlight contribution
      rhoul(i,:) = (t_u * t_d * R_wb(:)) / (1.0 - r_u*R_wb(:))
   end do


   !----------------------------------------------------------------------------
   ! Now calculate the reflectance from the wind-roughened surface. This is the
   ! actual Cox and Munk bit
   !----------------------------------------------------------------------------

   ! Generate versions of the viewing geometry in radians
   rsolza = d2r * solza
   rsatza = d2r * satza

   rsolaz = d2r * solaz

   rrelaz = d2r * relaz ! relative azimuth

   ! Convert wind direction to be relative to solar azimuth
   wd(:) = wd(:) - rsolaz(:)

   where(wd .lt. 0.0) wd = 2.0*pi + wd

   ! Define surface slopes
   dangle = cos(rsolza) + cos(rsatza)

   where (abs(dangle(:)) .gt. dither_more)
      Zx(:) = (-1.0*sin(rsatza(:)) * sin(rrelaz(:))) / dangle(:)
      Zy(:) = -1.0 * (sin(rsolza(:)) + sin(rsatza(:)) * cos(rrelaz)) / dangle(:)
   else where
      Zx(:) = 0.0
      Zy(:) = 0.0
   end where

   Zxprime(:) = cos(wd(:))*Zx(:) + sin(wd(:))*Zy(:)
   Zyprime(:) = -sin(wd(:))*Zx(:) + cos(wd(:))*Zy(:)

   ! Cox and Munk (1954) (Statistics of...) coefficients
   sigx(:) = sqrt(0.003 + 0.00192*ws(:))
   sigy(:) = sqrt(0.00316*ws(:))

   zeta(:) = Zxprime(:) / sigx(:)
   eta(:)  = Zyprime(:) / sigy(:)

   ! Slope distribution
   p(:) = exp(-0.5*(zeta(:)*zeta(:) + eta(:)*eta(:))) / (2.0*pi*sigx(:)*sigy(:))

   ! Cox and Munk (1954) (Measurements of...)
   ! 2*omega = angle between incident light and instrument, wrt the sloping sea
   ! surface
   cosomega(:) = cos(rsatza(:))*cos(rsolza(:)) + &
                 sin(rsatza(:))*sin(rsolza(:))*cos(rrelaz(:))

   where (abs(cosomega(:)+1.0) .gt. dither_more)
      cosbeta(:)  = (cos(rsolza(:)) + cos(rsatza(:))) / sqrt(2.0 + 2.0*cosomega(:))
   else where
      cosbeta(:) = 0.0
   end where

   w = 0.5*acos(cosomega(:))

   ! Apply the correction of Zeisse (1995) for zenith angles greater than 70
   ! degrees. This replaces cos(satza) in the glint calculation by the area of
   ! the "ergodic cap"
   ergodic(:) = zeisse_ba(rsatza(:), ws(:))

   do i = 1, nbands
      ! Snell's law
      wprime(:) = asin(n_air*sin(w(:)) / nr(bands(i)))

      ! Calculate Fresnel reflection coefficient R_sf
      a1(:)     = sin(w(:) - wprime(:))
      b1(:)     = sin(w(:) + wprime(:))
      c1(:)     = tan(w(:) - wprime(:))
      d1(:)     = tan(w(:) + wprime(:))

      where (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more)
         R_sf(:) = 0.5*((a1(:)*a1(:))/(b1(:)*b1(:)) + (c1(:)*c1(:))/(d1(:)*d1(:)))
      else where
         R_sf(:) = 0.0
      end where

      ! Calculate glint reflectance contribution
      dangle = 4.0 * cos(rsolza(:)) * ergodic(:) * (cosbeta(:)**4)
      where (abs(dangle(:)) .gt. dither_more)
         rhogl(i,:) = pi * p(:) * R_sf(:) / dangle(:)
      else where
         rhogl(i,:) = 0.0
      end where
   end do


   !----------------------------------------------------------------------------
   ! Add all the reflectance terms to produce the full surface reflectance
   !----------------------------------------------------------------------------
   do i = 1, npts
      rho(:,i) = rhowc(:,i) + (1-wcfrac(i))*(rhogl(:,i) + rhoul(:,i))
   end do


   ! Deallocate local dynamic arrays
   deallocate(rhowc)
   deallocate(rhoul)
   deallocate(rhogl)
   deallocate(ws)
   deallocate(wd)
   deallocate(wcfrac)
   deallocate(eta_oc)
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


!-------------------------------------------------------------------------------
! Name: zeisse_ba()
!
! Purpose:
! Calculates the B/A value, a correction to the cos(theta) value used in the
! Cox-Munk glint-calculations.
!
! Description and Algorithm details:
! Zeisse, C. R. 1995. "Radiance of the Ocean Horizon." Journal of the Optical
! Society of America A 12 (9): 2022–2030. doi:10.1364/JOSAA.12.002022.
!
! Arguments:
! Name  Type   In/Out/Both Description
! theta sreal  In          view-angle (in radians)
! ws    sreal  In          wind-speed (in m/s)
! Note: Both input arrays should be vectors with an equal number of elements.
!
! Return value:
! Name Type    Description
! ba   sreal   The so-called "area of the ergodic cap"
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
function zeisse_ba(theta, ws) result (ba)

   use preproc_constants_m

   implicit none

   ! Input arguments
   real(kind=sreal), dimension(:), intent(in) :: theta
   real(kind=sreal), dimension(:), intent(in) :: ws

   ! Return value
   real(kind=sreal), allocatable, dimension(:) :: ba

   ! Local variables
   integer                                     :: npoints
   real(kind=sreal)                            :: C(3,3)
   real(kind=sreal), allocatable               :: tp(:),wp(:)
!  real(kind=sreal), allocatable               :: tp2(:),wp2(:)
   real(kind=sreal), allocatable               :: delta(:)

   ! C defines the coefficients of the delta polynomial as given by the Zeisse
   ! paper
   C(:,1) = (/  1.67530e-3, -1.66517e-4,  2.03068e-5 /)
   C(:,2) = (/ -6.96112e-3, -5.55537e-3,  2.60686e-3 /)
   C(:,3) = (/  2.86324e-3,  1.86059e-3, -4.69589e-4 /)

   ! Allocate the output and local arrays, based on the size of the input arrays
   npoints = size(theta)

   allocate(ba(npoints)) ! Must be deallocated by calling routine
   allocate(tp(npoints))
   allocate(wp(npoints))
!  allocate(tp2(npoints))
!  allocate(wp2(npoints))
   allocate(delta(npoints))

   ! The delta correction is only applied at high solar zenith angles and
   ! non-negligible wind strength, otherwise just use cos(theta)
   where((theta .ge. 70.0*d2r) .and. (ws .gt. 1.0))
      tp = (theta - 70.0*d2r) / 5.0
      wp = 4.0 * alog10(ws) / 1.30103

!     wp2 = wp*wp
!     tp2 = tp*tp

!     delta = C(1,1) + &
!             C(2,1)*wp     + C(3,1)*wp2    + &
!             C(1,2)*tp     + C(1,3)*tp2    + &
!             C(2,2)*wp*tp  + C(3,2)*wp2*tp + &
!             C(2,3)*wp*tp2 + C(3,3)*wp2*tp2

      delta = C(1,1) + (C(2,1) + C(3,1)*wp)*wp  + &
            ((C(1,2) + (C(2,2) + C(3,2)*wp)*wp) + &
             (C(1,3) + (C(2,3) + C(3,3)*wp)*wp)*tp)*tp

      ba = delta + cos(theta)
   else where
      ba = cos(theta)
   end where

   ! Clean up local arrays
   deallocate(tp)
   deallocate(wp)
 ! deallocate(tp2)
 ! deallocate(wp2)
   deallocate(delta)

end function zeisse_ba


!-------------------------------------------------------------------------------
! Name: cox_munk2()
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type    In/Out/Both Description
! i_band integer In          Wavelength band index
! solza  sreal   In          Solar zenith angle (in degrees)
! satza  sreal   In          Satellite zenith angle
! solaz  sreal   In          Solar azimuth angle (from north)
! relaz  sreal   In          Relative azimuth angle (between sun and satellite)
! totbsc sreal   In          Total warer backscatter coefficient (1/m)
! totabs sreal   In          Total water absorption coefficient (1/m)
! u10    sreal   In          Near surface (10m) East-West wind component (m/s)
! v10    sreal   In          Near surface (10m) South-North wind component (m/s)
! rho    sreal   Out         Bi-directional reflectance
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine cox_munk2(i_band, solza, satza, solaz, relaz, totbsc, totabs, u10, v10, rho)

   use preproc_constants_m

   implicit none

   ! Input arguments
   integer,          intent(in)     :: i_band
   real(kind=sreal), intent(in)     :: solza, satza
   real(kind=sreal), intent(in)     :: solaz, relaz
   real(kind=sreal), intent(in)     :: totbsc, totabs
   real(kind=sreal), intent(in)     :: u10, v10

   ! Output arguments
   real(kind=sreal), intent(out)    :: rho

   ! Wavelength dependent constants (derived)
!  real(kind=sreal)                 :: chlabs(n_lambda), chlbsc(n_lambda)
!  real(kind=sreal)                 :: totbsc(n_lambda), eta_oc(n_lambda)
   real(kind=sreal)                  :: eta_oc

   ! Wind speed and direction
   real(kind=sreal)                 :: ws, wd

   ! White cap reflection variables
   real(kind=sreal)                 :: wcfrac
   real(kind=sreal)                 :: rhowc

   ! Under-light reflection variables
   real(kind=sreal)                 :: f
   real(kind=sreal)                 :: R_wb
   real(kind=sreal)                 :: t_u, r_u
   real(kind=sreal)                 :: t_d
   real(kind=sreal)                 :: rhoul

   ! Snell's law and Fresnel's equation
   real(kind=sreal)                 :: w, wprime
   real(kind=sreal)                 :: a1, b1, c1, d1

   ! Sun-glint/Cox and Munk variables
   real(kind=sreal)                 :: rsolza, rsatza
   real(kind=sreal)                 :: rsolaz, rrelaz

   real(kind=sreal)                 :: dangle
   real(kind=sreal)                 :: Zx, Zy
   real(kind=sreal)                 :: Zxprime, Zyprime
   real(kind=sreal)                 :: sigx, sigy
   real(kind=sreal)                 :: zeta, eta
   real(kind=sreal)                 :: p
   real(kind=sreal)                 :: cosomega, cosbeta
   real(kind=sreal)                 :: ergodic
   real(kind=sreal)                 :: R_sf
   real(kind=sreal)                 :: rhogl

   ! Calculate the chl-a absorption coefficient from the concentration value.
!  chlabs = coef1*(1.0-exp(-1.61*chlconc)) + coef2*chlconc

   ! Next define the back-scattering coefficients for chl-a
!  chlbsc = 0.02*(0.5-0.25*log10(chlconc))* 0.55/lambda + 0.002

   ! Approximate median CDOM absorption from GlobCOLOUR. Note that it is only
   ! significant at 0.55 microns, so can be neglected unless that channel is
   ! incorporated
!  cdomabs = (/ 0.013,   0.0,     0.0,     0.0,     0.0 /)


   !----------------------------------------------------------------------------
   ! Calculate wind speed and direction from wind components
   !----------------------------------------------------------------------------
   ws = sqrt(u10*u10 + v10*v10) ! Wind speed in m/s
   wd = acos(v10/ws)            ! Wind angle in radians from north
   if (u10 .lt. 0.0) wd = -wd   ! Azimuth angle on -180 - 180 degree interval


   !----------------------------------------------------------------------------
   ! Calculate white-cap fraction (and ensure it is no greater than 1)
   !----------------------------------------------------------------------------
   wcfrac = 2.951e-6 * (ws**3.52)
   if (wcfrac .gt. 1.0) wcfrac = 1.0

   ! White-cap contribution to the reflectance
   rhowc = wcfrac*Rwc(i_band)


   !----------------------------------------------------------------------------
   ! Next, calculate the reflectance of the water body, using the average CHL
   ! concentration and CDOM absorption defined above. Combine the various
   ! scattering coefficients defined above to give the total
   !----------------------------------------------------------------------------
!  totbsc = 0.5*basebsc + chlbsc * 0.3*chlconc**0.62
   eta_oc = 0.5*basebsc(i_band) / totbsc

   ! The so-called "coefficient of R"
   f = 0.6279 - (0.2227*eta_oc) - &
       ( 0.00513*eta_oc * eta_oc ) + &
       ( 0.2465 *eta_oc - 0.3119 )*cos(solza*d2r)

   ! Now calculate the water body reflectance, which is the coefficient of
   ! R * the backscatter, divided by the absorption (note if CDOM is
   ! included, it will appear on the bottom line of this equation)
!  R_wb = f*totbsc(i_band) / (baseabs(i_band) + chlabs(i_band))
   R_wb = f*totbsc / totabs

   ! Now we need to use the Fresnel equation and Snell's Law to calculate how
   ! much light actually enters the water body through the surface (t_d)
   ! Upward transmission and reflectance can be taken as constant at
   ! wavelengths where underlight is significant
   t_u = 0.52
   r_u = 1.0 - t_u

   ! Snell's law
   w = satza*d2r
   wprime = asin(n_air*sin(w) / nr(i_band))

   ! Coefficients of Fresnel's equation
   a1 = sin(w - wprime)
   b1 = sin(w + wprime)
   c1 = tan(w - wprime)
   d1 = tan(w + wprime)

   ! Fresnel's equation
!  t_d = 1.0 - 0.5 * ( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )

   ! This version explicitly catches division by 0
   if (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more) then
      t_d = 1.0 - 0.5*( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )
   else
      t_d = 0.0
   end if

   ! Combine the surface transmission terms with the underlight reflectance
   ! to give total underlight contribution
   rhoul = (t_u * t_d * R_wb) / (1.0 - r_u*R_wb)


   !----------------------------------------------------------------------------
   ! Now calculate the reflectance from the wind-roughened surface. This is the
   ! actual Cox and Munk bit
   !----------------------------------------------------------------------------

   ! Generate versions of the viewing geometry in radians
   rsolza = d2r * solza
   rsatza = d2r * satza

   rsolaz = d2r * solaz

   rrelaz = d2r * relaz ! relative azimuth

   ! Convert wind direction to be relative to solar azimuth
   wd = wd - rsolaz

   if (wd .lt. 0.0) wd = 2.0*pi + wd

   ! Define surface slopes
   dangle=cos(rsolza) + cos(rsatza)

   if (abs(dangle) .gt. dither_more) then
      Zx = (-1.0*sin(rsatza) * sin(rrelaz)) / dangle
      Zy = -1.0 * (sin(rsolza) + sin(rsatza) * cos(rrelaz)) / dangle
   else
      Zx = 0.0
      Zy = 0.0
   end if

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
   ! 2*omega = angle between incident light and instrument, wrt the sloping sea
   ! surface
   cosomega = cos(rsatza)*cos(rsolza) + sin(rsatza)*sin(rsolza)*cos(rrelaz)

   if (abs(cosomega+1.0) .gt. dither_more) then
      cosbeta = (cos(rsolza) + cos(rsatza)) / sqrt(2.0 + 2.0*cosomega)
   else
      cosbeta = 0.0
   end if

   w = 0.5*acos(cosomega)

   ! Apply the correction of Zeisse (1995) for zenith angles greater than 70
   ! degrees. This replaces cos(satza) in the glint calculation by the area of
   ! the "ergodic cap"
   ergodic = zeisse_ba2(rsatza, ws)

   ! Snell's law
   wprime = asin(n_air*sin(w) / nr(i_band))

   ! Calculate Fresnel reflection coefficient R_sf
   a1     = sin(w - wprime)
   b1     = sin(w + wprime)
   c1     = tan(w - wprime)
   d1     = tan(w + wprime)

   if (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more) then
      R_sf = 0.5*((a1*a1)/(b1*b1) + (c1*c1)/(d1*d1))
   else
      R_sf = 0.0
   end if

   ! Calculate glint reflectance contribution
   dangle = 4.0 * cos(rsolza) * ergodic * (cosbeta**4)
   if (abs(dangle) .gt. dither_more) then
      rhogl = pi * p * R_sf / dangle
   else
      rhogl = 0.0
   end if


   !----------------------------------------------------------------------------
   ! Add all the reflectance terms to produce the full surface reflectance
   !----------------------------------------------------------------------------
   rho = rhowc + (1+wcfrac)*(rhogl + rhoul)

end subroutine cox_munk2


!-------------------------------------------------------------------------------
! Name: zeisse_ba2()
!
! Purpose:
! Calculates the B/A value, a correction to the cos(theta) value used in the
! Cox-Munk glint-calculations.
!
! Description and Algorithm details:
! Zeisse, C. R. 1995. "Radiance of the Ocean Horizon." Journal of the Optical
! Society of America A 12 (9): 2022–2030. doi:10.1364/JOSAA.12.002022.
!
! Arguments:
! Name  Type  In/Out/Both Description
! theta sreal In          view-angle (in radians)
! ws    sreal In          wind-speed (in m/s)
!
! Return value:
! Name Type  Description
! ba   sreal The so-called "area of the ergodic cap"
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
function zeisse_ba2(theta, ws) result (ba)

   use preproc_constants_m

   implicit none

   ! Input arguments
   real(kind=sreal), intent(in) :: theta
   real(kind=sreal), intent(in) :: ws

   ! Return value
   real(kind=sreal) :: ba

   ! Local variables
   real(kind=sreal) :: C(3,3)
   real(kind=sreal) :: tp,wp
   real(kind=sreal) :: delta

   ! C defines the coefficients of the delta polynomial as given by the Zeisse
   ! paper
   C(:,1) = (/  1.67530e-3, -1.66517e-4,  2.03068e-5 /)
   C(:,2) = (/ -6.96112e-3, -5.55537e-3,  2.60686e-3 /)
   C(:,3) = (/  2.86324e-3,  1.86059e-3, -4.69589e-4 /)

   ! The delta correction is only applied at high solar zenith angles and
   ! non-negligible wind strength, otherwise just use cos(theta)
   if ((theta .ge. 70.0*d2r) .and. (ws .gt. 1.0)) then
      tp = (theta - 70.0*d2r) / 5.0
      wp = 4.0 * alog10(ws) / 1.30103

      delta = C(1,1) + (C(2,1) + C(3,1)*wp)*wp  + &
            ((C(1,2) + (C(2,2) + C(3,2)*wp)*wp) + &
             (C(1,3) + (C(2,3) + C(3,3)*wp)*wp)*tp)*tp

      ba = delta + cos(theta)
   else
      ba = cos(theta)
   end if

end function zeisse_ba2


!-------------------------------------------------------------------------------
! Name: cox_munk3_calc_shared_band()
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type                   In/Out/Both Description
! i_band integer                In          Wavelength band index
! totbsc real                   In          Total water backscatter coeff (1/m)
! totabs real                   In          Total water absorption coeff (1/m)
!
! shared cox_munk_shared_band_t Out         Structure with shared band values
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
#ifdef MULTI_LINE_COMMENT

subroutine cox_munk3_calc_shared_band(bands, totbsc, totabs, shared)

   implicit none

   ! Input arguments
   integer,                      intent(in)  :: bands(:)
   real(kind=sreal),             intent(in)  :: totbsc(:)
   real(kind=sreal),             intent(in)  :: totabs(:)

   ! Output arguments
   type(cox_munk_shared_band_t), intent(out) :: shared(:)

   ! Wavelength dependent constants (derived)
   real(kind=sreal) :: chlbsc


   !----------------------------------------------------------------------------
   ! Computations related to coefficients
   !----------------------------------------------------------------------------

   ! Calculate the chl-a absorption coefficient from the concentration value.
   shared%chlabs = coef1(i_band)*(1.0-exp(-1.61*chlconc)) + coef2(i_band)*chlconc

   ! Next define the back-scattering coefficients for chl-a
   chlbsc = 0.02*(0.5-0.25*log10(chlconc))* 0.55/lambda(i_band) + 0.002

end subroutine cox_munk3_calc_shared_band

#endif

!-------------------------------------------------------------------------------
! Name: cox_munk3_calc_shared_geo_wind()
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type   In/Out/Both Description
! solza  sreal  In          Solar zenith angle (in degrees)
! satza  sreal  In          Satellite zenith angle
! solaz  sreal  In          Solar azimuth angle (from north)
! relaz  sreal  In          Relative azimuth angle (between sun and satellite)
! u10    sreal  In          Near surface (10m) East-West wind component (m/s)
! v10    sreal  In          Near surface (10m) South-North wind component (m/s)
! shared cox_munk_shared_geo_wind_t Out Structure with shared geometry and
!                                   wind values
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine cox_munk3_calc_shared_geo_wind(solza, satza, solaz, relaz, u10, v10, shared)

   implicit none

   ! Input arguments
   real(kind=sreal),                 intent(in)  :: solza, satza
   real(kind=sreal),                 intent(in)  :: solaz, relaz
   real(kind=sreal),                 intent(in)  :: u10, v10

   ! Output arguments
   type(cox_munk_shared_geo_wind_t), intent(out) :: shared

   ! Wind speed and direction
   real(kind=sreal) :: ws, wd

   ! Sun-glint/Cox and Munk variables
   real(kind=sreal) :: dangle
   real(kind=sreal) :: Zx, Zy
   real(kind=sreal) :: cos_wd, sin_wd
   real(kind=sreal) :: Zxprime, Zyprime
   real(kind=sreal) :: sigx, sigy
   real(kind=sreal) :: zeta, eta
   real(kind=sreal) :: cosomega

   !----------------------------------------------------------------------------
   ! Precalculate trigonometric functions
   !----------------------------------------------------------------------------
   shared%satza = satza

   shared%cos_solza = cos(solza * d2r)
#ifdef COMPATIBILITY_MODE
   shared%sin_solza = sin(solza * d2r)
#else
   shared%sin_solza = sqrt(1. - shared%cos_solza * shared%cos_solza)
#endif
   shared%cos_satza = cos(satza * d2r)
#ifdef COMPATIBILITY_MODE
   shared%sin_satza = sin(satza * d2r)
#else
   shared%sin_satza = sqrt(1. - shared%cos_satza * shared%cos_satza)
#endif
   shared%cos_relaz = cos(relaz * d2r)
#ifdef COMPATIBILITY_MODE
   shared%sin_relaz = sin(relaz * d2r)
#else
   shared%sin_relaz = sqrt(1. - shared%cos_relaz * shared%cos_relaz)
#endif

   !----------------------------------------------------------------------------
   ! Calculate wind speed and direction from wind components
   !----------------------------------------------------------------------------
   ws = sqrt(u10*u10 + v10*v10) ! Wind speed in m/s
   wd = acos(v10/ws)            ! Wind angle in radians from north
   if (u10 .lt. 0.0) wd = -wd   ! Azimuth angle on -180 - 180 degree interval

   if (ws .le. 0.) then
      ws = 0.
      wd = 0.
   end if


   !----------------------------------------------------------------------------
   ! Calculate white-cap fraction (and ensure it is no greater than 1)
   !----------------------------------------------------------------------------
   shared%wcfrac = 2.951e-6 * (ws**3.52)
   if (shared%wcfrac .gt. 1.0) shared%wcfrac = 1.0


   !----------------------------------------------------------------------------
   ! Now calculate the reflectance from the wind-roughened surface. This is the
   ! actual Cox and Munk bit
   !----------------------------------------------------------------------------

   ! Convert wind direction to be relative to solar azimuth
   wd = wd - d2r * solaz
   if (wd .lt. 0.0) wd = 2.0*pi + wd

   ! Define surface slopes
   dangle=shared%cos_solza + shared%cos_satza

   if (abs(dangle) .gt. dither_more) then
      Zx = (-1.0*shared%sin_satza * shared%sin_relaz) / dangle
      Zy = -1.0 * (shared%sin_solza + shared%sin_satza*shared%cos_relaz) / dangle
   else
      Zx = 0.0
      Zy = 0.0
   end if
   cos_wd = cos(wd)
#ifdef COMPATIBILITY_MODE
   sin_wd = sin(wd)
#else
   sin_wd = sqrt(1. - cos_wd * cos_wd)
#endif
   Zxprime =  cos_wd*Zx + sin_wd*Zy
   Zyprime = -sin_wd*Zx + cos_wd*Zy

   ! Cox and Munk (1954) (Statistics of...) coefficients
   sigx = sqrt(0.003 + 0.00192*ws)
   sigy = sqrt(0.00316*ws)

   zeta = Zxprime / sigx
   eta  = Zyprime / sigy

   ! Slope distribution
   shared%p = exp(-0.5*(zeta*zeta + eta*eta)) / (2.0*pi*sigx*sigy)

   ! Cox and Munk (1954) (Measurements of...)
   ! 2*omega = angle between incident light and instrument, wrt the sloping sea
   ! surface
   cosomega = shared%cos_satza*shared%cos_solza + &
              shared%sin_satza*shared%sin_solza*shared%cos_relaz

   if (abs(cosomega+1.0) .gt. dither_more) then
      shared%cosbeta = (shared%cos_solza + shared%cos_satza) / sqrt(2.0 + 2.0*cosomega)
   else
      shared%cosbeta = 0.0
   end if

   shared%w = 0.5*acos(cosomega)

   shared%sin_w = sin(shared%w)

   ! Apply the correction of Zeisse (1995) for zenith angles greater than 70
   ! degrees. This replaces cos(satza) in the glint calculation by the area of
   ! the "ergodic cap"
   shared%ergodic = zeisse_ba3(d2r * satza, shared%cos_satza, ws)

   shared%a = 4.0*shared%cos_solza*shared%ergodic*(shared%cosbeta**4)

end subroutine cox_munk3_calc_shared_geo_wind


!-------------------------------------------------------------------------------
! Name: cox_munk3()
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type                       In/Out/Both Description
! i_band integer                    In          Wavelength band index
! shared_geo_wind
!        cox_munk_shared_geo_wind_t In          Structure with shared geometry
!                                               and wind values
! ocean_colour
!        ocean_colour_t             In          Structure containing ocean water
!                                               absorption and backscatter values
! rho    sreal                      Out         Bi-directional reflectance
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine cox_munk3(i_band, shared_geo_wind, ocean_colour, rho)

   use preproc_constants_m

   implicit none

   ! Input arguments
   integer,                          intent(in)  :: i_band
   type(ocean_colour_t),             intent(in)  :: ocean_colour
   type(cox_munk_shared_geo_wind_t), intent(in)  :: shared_geo_wind

   ! Output arguments
   real(kind=sreal),                 intent(out) :: rho

   ! White cap reflection variables
   real(kind=sreal)            :: rhowc

   ! Under-light reflection variables
   real(kind=sreal)            :: f
   real(kind=sreal)            :: R_wb
   real(kind=sreal)            :: eta_oc
   real(kind=sreal)            :: t_u, r_u
   real(kind=sreal)            :: t_d
   real(kind=sreal)            :: rhoul

   ! Snell's law and Fresnel's equation
   real(kind=sreal)            :: w, wprime
   real(kind=sreal)            :: a1, b1, c1, d1

   ! Sun-glint/Cox and Munk variables
   real(kind=sreal)            :: R_sf
   real(kind=sreal)            :: rhogl

   !----------------------------------------------------------------------------
   ! Calculate white-cap fraction (and ensure it is no greater than 1)
   !----------------------------------------------------------------------------

   ! White-cap contribution to the reflectance
   rhowc = shared_geo_wind%wcfrac*Rwc(i_band)


   !----------------------------------------------------------------------------
   ! Next, calculate the reflectance of the water body, using the total
   ! backscatter and absorption values derived from the ocean colour cci data.
   ! Combine the various scattering coefficients defined above to give the total
   !----------------------------------------------------------------------------
   eta_oc = 0.5*basebsc(i_band) / ocean_colour%totbsc
   ! The so-called "coefficient of R"
   f = 0.6279 - (0.2227*eta_oc) - ( 0.00513*eta_oc * eta_oc ) + &
       ( 0.2465 *eta_oc - 0.3119 )*shared_geo_wind%cos_solza

   ! Now calculate the water body reflectance, which is the coefficient of
   ! R * the backscatter, divided by the absorption (note if CDOM is
   ! included, it will appear on the bottom line of this equation)
!  R_wb = f*shared_band%totbsc / (baseabs(i_band) + shared_band%chlabs)
   R_wb = f*ocean_colour%totbsc / ocean_colour%totabs

   ! Now we need to use the Fresnel equation and Snell's Law to calculate how
   ! much light actually enters the water body through the surface (t_d)
   ! Upward transmission and reflectance can be taken as constant at
   ! wavelengths where underlight is significant
   t_u = 0.52
   r_u = 1.0 - t_u

   ! Snell's law
   w      = shared_geo_wind%satza*d2r
   wprime = asin(n_air*shared_geo_wind%sin_satza / nr(i_band))

   ! Coefficients of Fresnel's equation
   a1 = sin(w - wprime)
   b1 = sin(w + wprime)
#ifdef COMPATIBILITY_MODE
   c1 = tan(w - wprime)
   d1 = tan(w + wprime)
#else
   c1 = a1 / sqrt(1. - a1 * a1)
   d1 = b1 / sqrt(1. - b1 * b1)
#endif
   ! Fresnel's equation
!  t_d = 1.0 - 0.5 * ( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )

   ! This version explicitly catches division by 0
   if (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more) then
      t_d = 1.0 - 0.5*( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )
   else
      t_d = 0.0
   end if

   ! Combine the surface transmission terms with the underlight reflectance
   ! to give total underlight contribution
   rhoul = (t_u * t_d * R_wb) / (1.0 - r_u*R_wb)


   !----------------------------------------------------------------------------
   ! Now calculate the reflectance from the wind-roughened surface. This is the
   ! actual Cox and Munk bit
   !----------------------------------------------------------------------------

   ! Snell's law
   wprime = asin(n_air*shared_geo_wind%sin_w / nr(i_band))

   ! Calculate Fresnel reflection coefficient R_sf
   a1     = sin(shared_geo_wind%w - wprime)
   b1     = sin(shared_geo_wind%w + wprime)
#ifdef COMPATIBILITY_MODE
   c1     = tan(shared_geo_wind%w - wprime)
   d1     = tan(shared_geo_wind%w + wprime)
#else
   c1     = a1 / sqrt(1. - a1 * a1)
   d1     = b1 / sqrt(1. - b1 * b1)
#endif
   if (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more) then
      R_sf = 0.5*((a1*a1)/(b1*b1) + (c1*c1)/(d1*d1))
   else
      R_sf = 0.0
   end if

   ! Calculate glint reflectance contribution
   if (abs(shared_geo_wind%a) .gt. dither_more) then
      rhogl = pi * shared_geo_wind%p * R_sf / shared_geo_wind%a
   else
      rhogl = 0.0
   end if


   !----------------------------------------------------------------------------
   ! Add all the reflectance terms to produce the full surface reflectance
   !----------------------------------------------------------------------------
   rho = rhowc + (1-shared_geo_wind%wcfrac)*(rhogl + rhoul)

end subroutine cox_munk3


!-------------------------------------------------------------------------------
! Name: cox_munk4_calc_shared_wind()
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type    In/Out/Both Description
! i_band integer In          Wavelength band index number
! u10    sreal   In          Near surface (10m) East-West wind component (m/s)
! v10    sreal   In          Near surface (10m) South-North wind component (m/s)
! shared cox_munk_shared_wind_t Out Structure with shared band values
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine cox_munk4_calc_shared_wind(i_band, solaz, u10, v10, shared)

   use preproc_constants_m

   implicit none

   ! Input arguments
   integer,                      intent(in)  :: i_band
   real(kind=sreal),             intent(in)  :: solaz, u10, v10

   ! Output arguments
   type(cox_munk_shared_wind_t), intent(out) :: shared


   !----------------------------------------------------------------------------
   ! Calculate wind speed and direction from wind components
   !----------------------------------------------------------------------------
   shared%ws = sqrt(u10*u10 + v10*v10)      ! Wind speed in m/s
   shared%wd = acos(v10/shared%ws)          ! Wind angle in radians from north
   if (u10 .lt. 0.0) shared%wd = -shared%wd ! Azimuth angle on -180 - 180 degree interval

   ! Convert wind direction to be relative to solar azimuth
   shared%wd = shared%wd - d2r * solaz

   if (shared%wd .lt. 0.0) shared%wd = 2.0*pi + shared%wd


   !----------------------------------------------------------------------------
   ! Calculate white-cap fraction (and ensure it is no greater than 1)
   !----------------------------------------------------------------------------
   shared%wcfrac = 2.951e-6 * (shared%ws**3.52)
   if (shared%wcfrac .gt. 1.0) shared%wcfrac = 1.0

   ! White-cap contribution to the reflectance
   shared%rhowc = shared%wcfrac*Rwc(i_band)


   !----------------------------------------------------------------------------
   ! Now calculate the reflectance from the wind-roughened surface. This is the
   ! actual Cox and Munk bit
   !----------------------------------------------------------------------------
   shared%cos_wd = cos(shared%wd)
#ifdef COMPATIBILITY_MODE
   shared%sin_wd = sin(shared%wd)
#else
   shared%sin_wd = sqrt(1. - shared%cos_wd * shared%cos_wd)
#endif
   ! Cox and Munk (1954) (Statistics of...) coefficients
   shared%sigx = sqrt(0.003 + 0.00192*shared%ws)
   shared%sigy = sqrt(0.00316*shared%ws)

end subroutine cox_munk4_calc_shared_wind


!-------------------------------------------------------------------------------
! Name: cox_munk4_calc_shared_band_geo()
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type    In/Out/Both Description
! i_band integer In          Wavelength band index number
! solza  sreal   In          Solar zenith angle (in degrees)
! satza  sreal   In          Satellite zenith angle
! solaz  sreal   In          Solar azimuth angle (from north)
! relaz  sreal   In          Relative azimuth angle (between sun and satellite)
! totbsc real    In          Total water backscatter coeff (1/m)
! totabs real    In          Total water absorption coeff (1/m)
!
! shared cox_munk_shared_wind_t Out Structure with shared band values
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine cox_munk4_calc_shared_band_geo(i_band, solza, satza, solaz, relaz, &
     shared)

   implicit none

   ! Input arguments
   integer,                          intent(in)  :: i_band
   real(kind=sreal),                 intent(in)  :: solza, satza
   real(kind=sreal),                 intent(in)  :: solaz, relaz

   ! Output arguments
   type(cox_munk_shared_band_geo_t), intent(out) :: shared

   ! Snell's law and Fresnel's equation
   real(kind=sreal)            :: w, wprime
   real(kind=sreal)            :: a1, b1, c1, d1

   ! Sun-glint/Cox and Munk variables
   real(kind=sreal)            :: dangle
   real(kind=sreal)            :: cosomega


   !----------------------------------------------------------------------------
   ! Precalculate trigonometric functions
   !----------------------------------------------------------------------------
   shared%satza = satza * d2r

   shared%cos_solza = cos(solza * d2r)
#ifdef COMPATIBILITY_MODE
   shared%sin_solza = sin(solza * d2r)
#else
   shared%sin_solza = sqrt(1. - shared%cos_solza * shared%cos_solza)
#endif
   shared%cos_satza = cos(satza * d2r)
#ifdef COMPATIBILITY_MODE
   shared%sin_satza = sin(satza * d2r)
#else
   shared%sin_satza = sqrt(1. - shared%cos_satza * shared%cos_satza)
#endif
   shared%cos_relaz = cos(relaz * d2r)
#ifdef COMPATIBILITY_MODE
   shared%sin_relaz = sin(relaz * d2r)
#else
   shared%sin_relaz = sqrt(1. - shared%cos_relaz * shared%cos_relaz)
#endif


   !----------------------------------------------------------------------------
   ! Now calculate the reflectance from the wind-roughened surface. This is the
   ! actual Cox and Munk bit
   !----------------------------------------------------------------------------

   ! Define surface slopes
   dangle=shared%cos_solza + shared%cos_satza

   if (abs(dangle) .gt. dither_more) then
      shared%Zx = (-1.0*shared%sin_satza * shared%sin_relaz) / dangle
      shared%Zy = -1.0 * (shared%sin_solza + shared%sin_satza*shared%cos_relaz) / dangle
   else
      shared%Zx = 0.0
      shared%Zy = 0.0
   end if

   ! Cox and Munk (1954) (Measurements of...)
   ! 2*omega = angle between incident light and instrument, wrt the sloping sea
   ! surface
   cosomega = shared%cos_satza*shared%cos_solza + &
              shared%sin_satza*shared%sin_solza*shared%cos_relaz

   if (abs(cosomega+1.0) .gt. dither_more) then
      shared%cosbeta = (shared%cos_solza + shared%cos_satza) / sqrt(2.0 + 2.0*cosomega)
   else
      shared%cosbeta = 0.0
   end if

   w = 0.5*acos(cosomega)

   ! Snell's law
   wprime = asin(n_air*sin(w) / nr(i_band))

   ! Calculate Fresnel reflection coefficient R_sf
   a1     = sin(w - wprime)
   b1     = sin(w + wprime)
#ifdef COMPATIBILITY_MODE
   c1     = tan(w - wprime)
   d1     = tan(w + wprime)
#else
   c1     = a1 / sqrt(1. - a1 * a1)
   d1     = b1 / sqrt(1. - b1 * b1)
#endif
   if (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more) then
      shared%R_sf = 0.5*((a1*a1)/(b1*b1) + (c1*c1)/(d1*d1))
   else
      shared%R_sf = 0.0
   end if

end subroutine cox_munk4_calc_shared_band_geo


subroutine cox_munk4_interp_shared_band_geo(d_theta, theta, shared_lut, shared_out)

   implicit none

   ! Input arguments
   real(kind=sreal),                 intent(in) :: d_theta
   real(kind=sreal),                 intent(in) :: theta
   type(cox_munk_shared_band_geo_t), intent(in) :: shared_lut(:)

   ! Output arguments
   type(cox_munk_shared_band_geo_t), intent(out) :: shared_out

   integer :: i
   integer :: ii
   real    :: a
   real    :: b

   if (theta .lt. 0. .or. theta .gt. 2. * pi) then
        write(*,*) 'ERROR: cox_munk4_interp_shared_band_geo(), theta = ', &
                 theta, 'is out of range'
   end if

   i  = int(theta / d_theta) + 1
   ii = i + 1

   a  = (theta - (i - 1) * d_theta) / d_theta
   b  = 1. - a

   shared_out%satza     = b * shared_lut(i)%satza     + a * shared_lut(ii)%satza
   shared_out%cos_solza = b * shared_lut(i)%cos_solza + a * shared_lut(ii)%cos_solza
   shared_out%sin_solza = b * shared_lut(i)%sin_solza + a * shared_lut(ii)%sin_solza
   shared_out%cos_satza = b * shared_lut(i)%cos_satza + a * shared_lut(ii)%cos_satza
   shared_out%sin_satza = b * shared_lut(i)%sin_satza + a * shared_lut(ii)%sin_satza
   shared_out%cos_relaz = b * shared_lut(i)%cos_relaz + a * shared_lut(ii)%cos_relaz
   shared_out%sin_relaz = b * shared_lut(i)%sin_relaz + a * shared_lut(ii)%sin_relaz
   shared_out%Zx        = b * shared_lut(i)%Zx        + a * shared_lut(ii)%Zx
   shared_out%Zy        = b * shared_lut(i)%Zy        + a * shared_lut(ii)%Zy
   shared_out%cosbeta   = b * shared_lut(i)%cosbeta   + a * shared_lut(ii)%cosbeta
   shared_out%R_sf      = b * shared_lut(i)%R_sf      + a * shared_lut(ii)%R_sf

end subroutine cox_munk4_interp_shared_band_geo


!-------------------------------------------------------------------------------
! Name: cox_munk4()
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name   Type                       In/Out/Both Description
! i_band integer                    In          Wavelength band index
! shared_wind
!        cox_munk_shared_wind_t     In          Structure with shared wind
!                                               values
! shared_band_geo
!        cox_munk_shared_band_geo_t In          Structure with shared band and
!                                               geometry values
! solza  sreal                      In          Solar zenith angle (radians)
! satza  sreal                      In          Satellite zenith angle (radians)
! ocean_colour                      In          Structure containing water
!        ocean_colour_t                         absorption and backscatter
! rho    sreal                      Out         Bi-directional reflectance
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine cox_munk4(i_band, shared_wind, shared_band_geo, solza, satza, &
     ocean_colour, rho)

   use preproc_constants_m

   implicit none

   ! Input arguments
   integer,                          intent(in) :: i_band
   type(cox_munk_shared_wind_t),     intent(in) :: shared_wind
   type(cox_munk_shared_band_geo_t), intent(in) :: shared_band_geo
   real(kind=sreal),                 intent(in) :: solza
   real(kind=sreal),                 intent(in) :: satza
   type(ocean_colour_t),             intent(in) :: ocean_colour

   ! Output arguments
   real(kind=sreal),                 intent(out) :: rho

   ! Underlight variables
   real(kind=sreal) :: eta_oc, f
   real(kind=sreal) :: R_wb, t_u, t_d, r_u
   real(kind=sreal) :: wprime
   real(kind=sreal) :: a1, b1, c1, d1
   real(kind=sreal) :: rhoul

   ! Sun-glint/Cox and Munk variables
   real(kind=sreal) :: dangle
   real(kind=sreal) :: Zxprime, Zyprime
   real(kind=sreal) :: zeta, eta
   real(kind=sreal) :: p
   real(kind=sreal) :: ergodic
   real(kind=sreal) :: rhogl

   !----------------------------------------------------------------------------
   ! Calculate the reflectance of the water body, using the average CHL
   ! concentration and CDOM absorption defined above. Combine the various
   ! scattering coefficients defined above to give the total
   !----------------------------------------------------------------------------
   eta_oc = 0.5*basebsc(i_band) / ocean_colour%totbsc

   ! The so-called "coefficient of R"
   f = 0.6279 - (0.2227*eta_oc) - ( 0.00513*eta_oc * eta_oc ) + &
        ( 0.2465 *eta_oc - 0.3119 )*cos(solza)

   ! Now calculate the water body reflectance, which is the coefficient of
   ! R * the backscatter, divided by the absorption (note if CDOM is
   ! included, it will appear on the bottom line of this equation)
!  R_wb = f*totbsc / (baseabs(i_band) + chlabs)
   R_wb = f*ocean_colour%totbsc / ocean_colour%totabs

   ! Now we need to use the Fresnel equation and Snell's Law to calculate how
   ! much light actually enters the water body through the surface (t_d)
   ! Upward transmission and reflectance can be taken as constant at
   ! wavelengths where underlight is significant
   t_u = 0.52
   r_u = 1.0 - t_u

   ! Snell's law
   wprime = asin(n_air*sin(satza) / nr(i_band))

   ! Coefficients of Fresnel's equation
   a1 = sin(satza - wprime)
   b1 = sin(satza + wprime)
#ifdef COMPATIBILITY_MODE
   c1 = tan(satza - wprime)
   d1 = tan(satza + wprime)
#else
   c1 = a1 / sqrt(1. - a1 * a1)
   d1 = b1 / sqrt(1. - b1 * b1)
#endif
   ! Fresnel's equation
!  t_d = 1.0 - 0.5 * ( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )

   ! This version explicitly catches division by 0
   if (abs(b1) .gt. dither_more .and. abs(d1) .gt. dither_more) then
      t_d = 1.0 - 0.5*( (a1*a1)/(b1*b1) + (c1*c1)/(d1*d1) )
   else
      t_d = 0.0
   end if

   ! Combine the surface transmission terms with the underlight reflectance
   ! to give total underlight contribution
   rhoul = (t_u * t_d * R_wb) / (1.0 - r_u*R_wb)

   !----------------------------------------------------------------------------
   ! Now calculate the reflectance from the wind-roughened surface. This is the
   ! actual Cox and Munk bit
   !----------------------------------------------------------------------------

   ! Define surface slopes
   Zxprime =  shared_wind%cos_wd*shared_band_geo%Zx + shared_wind%sin_wd*shared_band_geo%Zy
   Zyprime = -shared_wind%sin_wd*shared_band_geo%Zx + shared_wind%cos_wd*shared_band_geo%Zy

   zeta = Zxprime / shared_wind%sigx
   eta  = Zyprime / shared_wind%sigy

   ! Slope distribution
   p = exp(-0.5*(zeta*zeta + eta*eta)) / (2.0*pi*shared_wind%sigx*shared_wind%sigy)

   ! Apply the correction of Zeisse (1995) for zenith angles greater than 70
   ! degrees. This replaces cos(satza) in the glint calculation by the area of
   ! the "ergodic cap"
   ergodic = zeisse_ba3(shared_band_geo%satza, shared_band_geo%cos_satza, shared_wind%ws)

   ! Calculate glint reflectance contribution
   dangle = 4.0 * shared_band_geo%cos_solza * ergodic * (shared_band_geo%cosbeta**4)
   if (abs(dangle) .gt. dither_more) then
      rhogl = pi * p * shared_band_geo%R_sf / dangle
   else
      rhogl = 0.0
   end if


   !----------------------------------------------------------------------------
   ! Add all the reflectance terms to produce the full surface reflectance
   !----------------------------------------------------------------------------
   rho = shared_wind%rhowc + (1-shared_wind%wcfrac)*(rhogl + rhoul)

end subroutine cox_munk4


!-------------------------------------------------------------------------------
! Name: zeisse_ba3()
!
! Purpose:
! Calculates the B/A value, a correction to the cos(theta) value used in the
! Cox-Munk glint-calculations.
!
! Description and Algorithm details:
! Zeisse, C. R. 1995. "Radiance of the Ocean Horizon." Journal of the Optical
! Society of America A 12 (9): 2022–2030. doi:10.1364/JOSAA.12.002022.
!
! Arguments:
! Name      Type  In/Out/Both Description
! theta     sreal In          view-angle (in radians)
! cos_theta sreal In          cosine of the view-angle
! ws        sreal In          wind-speed (in m/s)
!
! Return value:
! Name Type  Description
! ba   sreal The so-called "area of the ergodic cap"
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
function zeisse_ba3(theta, cos_theta, ws) result (ba)

   use preproc_constants_m

   implicit none

   ! Input arguments
   real(kind=sreal), intent(in) :: theta
   real(kind=sreal), intent(in) :: cos_theta
   real(kind=sreal), intent(in) :: ws

   ! Return value
   real(kind=sreal) :: ba

   ! Local variables
   real(kind=sreal) :: tp,wp
   real(kind=sreal) :: delta
   real(kind=sreal) :: C(3,3)

   ! C defines the coefficients of the delta polynomial as given by the Zeisse
   ! paper
   C(:,1) = (/  1.67530e-3, -1.66517e-4,  2.03068e-5 /)
   C(:,2) = (/ -6.96112e-3, -5.55537e-3,  2.60686e-3 /)
   C(:,3) = (/  2.86324e-3,  1.86059e-3, -4.69589e-4 /)

   ! The delta correction is only applied at high solar zenith angles and
   ! non-negligible wind strength, otherwise just use cos(theta)
   if ((theta .ge. 70.0*d2r) .and. (ws .gt. 1.0)) then
      tp = (theta - 70.0*d2r) / 5.0
      wp = 4.0 * alog10(ws) / 1.30103

      delta = C(1,1) + (C(2,1) + C(3,1)*wp)*wp  + &
            ((C(1,2) + (C(2,2) + C(3,2)*wp)*wp) + &
             (C(1,3) + (C(2,3) + C(3,3)*wp)*wp)*tp)*tp

      ba = delta + cos_theta
   else
      ba = cos_theta
   end if

end function zeisse_ba3


!-------------------------------------------------------------------------------
! Name: cox_munk_rho_0v_0d_dv_and_dd()
!
! Purpose:
! Compute quantities used in the ORAC fast forward model.
!
! Description and Algorithm details:
! This is mostly just Gauss-Legendre integrations over hemispheres.
!
! Arguments:
! Name   Type    In/Out/Both Description
! bands  integer In          Wavelength band index numbers
! solza  sreal   In          Array of solar zenith angles (in degrees)
! satza  sreal   In          Array of satellite zenith angles
! solaz  sreal   In          Array of solar azimuth anlges (from north)
! relaz  sreal   In          Array of relative azimuth angles (between sun and
!                            satellite)
! ocean_colour   In          Array of structures containing ocean colour data
!        ocean_colour_t      from the ocean_colour_cci, or default values.
! u10    sreal   In          Near surface (10m) East-West wind component (m/s)
! v10    sreal   In          Near surface (10m) South-North wind component (m/s)
! rho_0v sreal   Out         The nbands x npoints output array of solar beam to
!                            satellite view reflectances
! rho_0d sreal   Out         The nbands x npoints output array of solar beam to
!                            diffuse reflectances
! rho_dv sreal   Out         The nbands x npoints output array of diffuse to
!                            satellite view reflectances
! rho_dd sreal   Out         The nbands x npoints output array of diffuse to
!                            diffuse reflectances
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine cox_munk_rho_0v_0d_dv_and_dd(bands, solza, satza, solaz, relaz, &
   ocean_colour, u10, v10, fill_value, rho_0v, rho_0d, rho_dv, rho_dd, verbose)

   use preproc_constants_m
   use gauss_leg_quad_m

   implicit none

   ! Input arguments
   integer,              intent(in) :: bands(:)
   real(kind=sreal),     intent(in) :: solza(:), satza(:)
   real(kind=sreal),     intent(in) :: solaz(:), relaz(:)
   type(ocean_colour_t), intent(in) :: ocean_colour(:,:)
   real(kind=sreal),     intent(in) :: u10(:), v10(:)
   real(kind=sreal),     intent(in) :: fill_value
   logical,              intent(in) :: verbose

   ! Output arguments
   real(kind=sreal), intent(inout) :: rho_0v(:,:)
   real(kind=sreal), intent(inout) :: rho_0d(:,:)
   real(kind=sreal), intent(inout) :: rho_dv(:,:)
   real(kind=sreal), intent(inout) :: rho_dd(:,:)

   ! Local variables
   integer                       :: i
   integer                       :: j
   integer                       :: k
   integer                       :: l
   integer                       :: m
   integer                       :: i_oc = 1
   integer                       :: n_bands
   integer                       :: n_points
   integer, parameter            :: n_quad_theta = 4
   integer, parameter            :: n_quad_phi   = 4
   integer, parameter            :: lut_n_theta  = 181

   real(kind=sreal), allocatable :: qx_theta(:)
   real(kind=sreal), allocatable :: qw_theta(:)

   real(kind=sreal), allocatable :: qx_phi(:)
   real(kind=sreal), allocatable :: qw_phi(:)

   real(kind=sreal), allocatable :: qx_cos_sin_qw_theta(:)

   real(kind=sreal)              :: lut_d_theta

   real(kind=sreal)              :: a
   real(kind=sreal), allocatable :: aa(:)
   real(kind=sreal)              :: a2
   real(kind=sreal)              :: a3
   real(kind=sreal)              :: solza2
   real(kind=sreal)              :: satza2
   real(kind=sreal)              :: relaz2

   type(cox_munk_shared_geo_wind_t)              :: shared_geo_wind
   type(cox_munk_shared_wind_t)                  :: shared_wind
   type(cox_munk_shared_band_geo_t), allocatable :: shared_band_geo(:,:,:,:)
   type(cox_munk_shared_band_geo_t)              :: shared_band_geo2


   n_bands  = size(bands)
   n_points = size(solza)


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   allocate(qx_theta(n_quad_theta))
   allocate(qw_theta(n_quad_theta))

   allocate(qx_phi  (n_quad_phi  ))
   allocate(qw_phi  (n_quad_phi  ))

   allocate(qx_cos_sin_qw_theta(n_quad_theta))

   call gauss_leg_quadx(n_quad_theta, 0., pi / 2., qx_theta, qw_theta)
   call gauss_leg_quadx(n_quad_phi,   0., 2. * pi, qx_phi,   qw_phi)

   do i = 1, n_quad_theta
      qx_cos_sin_qw_theta(i) = cos(qx_theta(i)) * sin(qx_theta(i)) * qw_theta(i)
   end do


   !----------------------------------------------------------------------------
   if (verbose) write(*,*) 'cox_munk_rho_0v_0d_dv_and_dd(): computing rho_0v'
   !----------------------------------------------------------------------------
!$OMP PARALLEL PRIVATE(i, j, i_oc, shared_geo_wind)
!$OMP DO SCHEDULE(GUIDED)
   do i = 1, n_points
      if (solza(i) .lt. 0. .or. solza(i) .gt. maxsza_twi .or. satza(i) .lt. 0. .or. &
          u10(i) .eq. fill_value .or. v10(i) .eq. fill_value) then
         rho_0v(:, i) = fill_value
         cycle
      end if

      call cox_munk3_calc_shared_geo_wind(solza(i), satza(i), solaz(i), relaz(i), &
                                          u10(i), v10(i), shared_geo_wind)

      do j = 1, n_bands
         ! If we have the Ocean_colour_cci data, then use it. Otherwise
         ! ocean_colour will just contain the default values for our bands.
         if (ocean_colour(j,1)%have_data) then
            i_oc = i
         else
            i_oc = 1
         end if
         call cox_munk3(bands(j), shared_geo_wind, ocean_colour(j,i_oc), rho_0v(j,i))
      end do
   end do
!$OMP END DO
!$OMP END PARALLEL

   !----------------------------------------------------------------------------
   if (verbose) write(*,*) 'cox_munk_rho_0v_0d_dv_and_dd(): computing rho_0d'
   !----------------------------------------------------------------------------
! The full computation
if (.true.) then
   rho_0d = 0.
!$OMP PARALLEL PRIVATE(i, j, k, l, i_oc, aa, a2, satza2, relaz2, shared_geo_wind)
   allocate(aa(n_bands))
!$OMP DO SCHEDULE(GUIDED)
   do i = 1, n_points
      if (solza(i) .lt. 0. .or. solza(i) .gt. maxsza_twi .or. &
          u10(i) .eq. fill_value .or. v10(i) .eq. fill_value) then
         rho_0d(:, i) = fill_value
         cycle
      end if

      do j = 1, n_quad_theta
         satza2 = qx_theta(j) / d2r
         aa = 0.

         do k = 1, n_quad_phi
            relaz2 = qx_phi(k) / d2r
            call cox_munk3_calc_shared_geo_wind(solza(i), satza2, solaz(i), relaz2, &
                                                u10(i), v10(i), shared_geo_wind)
            do l = 1, n_bands
               ! If we have the Ocean_colour_cci data, then use it.
               ! Otherwise ocean_colour will just contain the default
               ! values for our bands.
               if (ocean_colour(l,1)%have_data) then
                  i_oc = i
               else
                  i_oc = 1
               end if

               call cox_munk3(bands(l), shared_geo_wind, ocean_colour(l,i_oc), a2)
               aa(l) = aa(l) + a2 * qw_phi(k)
            end do
         end do
         rho_0d(:, i) = rho_0d(:, i) + aa * qx_cos_sin_qw_theta(j)
      end do

      rho_0d(:, i) = rho_0d(:, i) / pi
   end do
!$OMP END DO
   deallocate(aa)
!$OMP END PARALLEL

! Fast LUT version
else
   lut_d_theta = 2. * pi / (lut_n_theta - 1.)

   allocate(shared_band_geo(n_bands, lut_n_theta, n_quad_theta, n_quad_phi))

   do i = 1, n_bands
      do j = 1, lut_n_theta
         solza2 = (j - 1) * lut_d_theta / d2r
         do k = 1, n_quad_theta
            satza2 = qx_theta(k) / d2r
            do l = 1, n_quad_phi
               relaz2 = qx_phi(l) / d2r
               call cox_munk4_calc_shared_band_geo(bands(i), solza2, satza2, &
                  0., relaz2, shared_band_geo(i, j, k, l))
            end do
         end do
      end do
   end do

   rho_0d = 0.

   do i = 1, n_bands
!$OMP PARALLEL PRIVATE(j, l, m, i_oc, a, a2, solza2, shared_wind, shared_band_geo2)
!$OMP DO SCHEDULE(GUIDED)
      do j = 1, n_points
         if (solza(j) .lt. 0. .or. solza(j) .gt. maxsza_twi .or. &
             u10(j) .eq. fill_value .or. v10(j) .eq. fill_value) then
            rho_0d(:, j) = fill_value
            cycle
         end if

         ! If we have the Ocean_colour_cci data, then use it. Otherwise
         ! ocean_colour will just contain the default values for our bands.
         if (ocean_colour(i,1)%have_data) then
            i_oc = j
         else
            i_oc = 1
         end if

         solza2 = solza(j) * d2r
         call cox_munk4_calc_shared_wind(bands(i), solaz(j), u10(j), v10(j), shared_wind)
         do l = 1, n_quad_theta
            a = 0.
            do m = 1, n_quad_phi
               call cox_munk4_interp_shared_band_geo(lut_d_theta, solza2, &
                    shared_band_geo(i,:,l,m), shared_band_geo2)
               call cox_munk4(bands(i), shared_wind, shared_band_geo2, &
                    solza2, qx_theta(l), ocean_colour(i,i_oc), a2)
               a = a + a2 * qw_phi(m)
            end do
            rho_0d(i, j) = rho_0d(i, j) + a * qx_cos_sin_qw_theta(l)
         end do

         rho_0d(i, j) = rho_0d(i, j) / pi
      end do
!$OMP END DO
!$OMP END PARALLEL
   end do

   deallocate(shared_band_geo)
end if

   !----------------------------------------------------------------------------
   if (verbose) write(*,*) 'cox_munk_rho_0v_0d_dv_and_dd(): computing rho_dv'
   !----------------------------------------------------------------------------
! The full computation
if (.true.) then
   rho_dv = 0.
!$OMP PARALLEL PRIVATE(i, j, k, l, i_oc, aa, a2, solza2, relaz2, shared_geo_wind)
   allocate(aa(n_bands))
!$OMP DO SCHEDULE(GUIDED)
   do i = 1, n_points
      if (satza(i) .lt. 0. .or. &
          u10(i) .eq. fill_value .or. v10(i) .eq. fill_value) then
         rho_dv(:, i) = fill_value
         cycle
      end if

      do j = 1, n_quad_theta
         solza2 = qx_theta(j) / d2r
         aa = 0.
         do k = 1, n_quad_phi
            relaz2 = qx_phi(k) / d2r
            call cox_munk3_calc_shared_geo_wind(solza2, satza(i), solaz(i), relaz2, &
                                                u10(i), v10(i), shared_geo_wind)
            do l = 1, n_bands
               ! If we have the Ocean_colour_cci data, then use it. Otherwise ocean_colour
               ! will just contain the default values for our bands.
               if (ocean_colour(l,1)%have_data) then
                  i_oc = i
               else
                  i_oc = 1
               end if

               call cox_munk3(bands(l), shared_geo_wind, ocean_colour(l,i_oc), a2)
               aa(l) = aa(l) + a2 * qw_phi(k)
            end do
         end do
         rho_dv(:, i) = rho_dv(:, i) + aa * qx_cos_sin_qw_theta(j)
      end do

      rho_dv(:, i) = rho_dv(:, i) / pi
   end do
!$OMP END DO
   deallocate(aa)
!$OMP END PARALLEL

! Fast LUT version
else
   lut_d_theta = 2. * pi / (lut_n_theta - 1.)

   allocate(shared_band_geo(n_bands, n_quad_theta, lut_n_theta, n_quad_phi))

   do i = 1, n_bands
      do j = 1, n_quad_theta
         solza2 = qx_theta(j) / d2r
         do k = 1, lut_n_theta
            satza2 = (k - 1) * lut_d_theta / d2r
            do l = 1, n_quad_phi
               relaz2 = qx_phi(l) / d2r
               call cox_munk4_calc_shared_band_geo(bands(i), solza2, satza2, &
                  0., relaz2, shared_band_geo(i, j, k, l))
            end do
         end do
      end do
   end do

   rho_dv = 0.

   do i = 1, n_bands
!$OMP PARALLEL PRIVATE(j, l, m, i_oc, a, a2, satza2, shared_wind, shared_band_geo2)
!$OMP DO SCHEDULE(GUIDED)
      do j = 1, n_points
         if (satza(j) .lt. 0. .or. &
             u10(j) .eq. fill_value .or. v10(j) .eq. fill_value) then
            rho_dv(:, j) = fill_value
            cycle
         end if

         satza2 = satza(j) * d2r
         call cox_munk4_calc_shared_wind(bands(i), solaz(j), u10(j), v10(j), shared_wind)

         ! If we have the Ocean_colour_cci data, then use it. Otherwise ocean_colour
         ! will just contain the default values for our bands.
         if (ocean_colour(i,1)%have_data) then
            i_oc = j
         else
            i_oc = 1
         end if

         do l = 1, n_quad_theta
            a = 0.
            do m = 1, n_quad_phi
               call cox_munk4_interp_shared_band_geo(lut_d_theta, satza2, &
                    shared_band_geo(i,l,:,m), shared_band_geo2)
               call cox_munk4(bands(i), shared_wind, shared_band_geo2, &
                    qx_theta(l), satza2, ocean_colour(i,i_oc), a2)
               a = a + a2 * qw_phi(m)
            end do
            rho_dv(i, j) = rho_dv(i, j) + a * qx_cos_sin_qw_theta(l)
         end do

         rho_dv(i, j) = rho_dv(i, j) / pi
      end do
!$OMP END DO
!$OMP END PARALLEL
   end do

   deallocate(shared_band_geo)
end if

   !----------------------------------------------------------------------------
   if (verbose) write(*,*) 'cox_munk_rho_0v_0d_dv_and_dd(): computing rho_dd'
   !----------------------------------------------------------------------------
   allocate(shared_band_geo(n_quad_phi, n_quad_theta, n_quad_theta, n_bands))

   do i = 1, n_bands
      do j = 1, n_quad_theta
         solza2 = qx_theta(j) / d2r
         do k = 1, n_quad_theta
            satza2 = qx_theta(k) / d2r
            do l = 1, n_quad_phi
               relaz2 = qx_phi(l) / d2r
               call cox_munk4_calc_shared_band_geo(bands(i), solza2, satza2, &
                  0., relaz2, shared_band_geo(l, k, j, i))
            end do
         end do
      end do
   end do

   rho_dd = 0.

   do i = 1, n_bands
!$OMP PARALLEL PRIVATE(j, k, l, m, i_oc, a, a2, a3, shared_wind)
!$OMP DO SCHEDULE(GUIDED)
      do j = 1, n_points
         if (u10(j) .eq. fill_value .or. v10(j) .eq. fill_value) then
            rho_dd(:, j) = fill_value
            cycle
         end if

         call cox_munk4_calc_shared_wind(int(bands(i), kind=lint), &
                                         solaz(j), u10(j), v10(j), shared_wind)

         ! If we have the Ocean_colour_cci data, then use it. Otherwise ocean_colour
         ! will just contain the default values for our bands.
         if (ocean_colour(i,1)%have_data) then
            i_oc = j
         else
            i_oc = 1
         end if
         do k = 1, n_quad_theta
            a = 0.
            do l = 1, n_quad_theta
               a2 = 0.
               do m = 1, n_quad_phi
                  call cox_munk4(bands(i), shared_wind, &
                       shared_band_geo(m, l, k, i), qx_theta(l), &
                       qx_theta(k), ocean_colour(i,i_oc), a3)
                  a2 = a2 + a3 * qw_phi(m)
               end do
               a = a + a2 * qx_cos_sin_qw_theta(l)
            end do
            rho_dd(i, j) = rho_dd(i, j) + a * qx_cos_sin_qw_theta(k)
         end do

         rho_dd(i, j) = rho_dd(i, j) * 2. / pi
      end do
!$OMP END DO
!$OMP END PARALLEL
   end do

   deallocate(shared_band_geo)


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   deallocate(qx_theta)
   deallocate(qw_theta)

   deallocate(qx_phi)
   deallocate(qw_phi)

   deallocate(qx_cos_sin_qw_theta)

end subroutine cox_munk_rho_0v_0d_dv_and_dd

end module cox_munk_m
