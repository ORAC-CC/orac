!-----------------------------------------------------------------------
! This code is part of the International MODIS/AIRS Processing Package
! (IMAPP) licensed under the GNU General Public License (GPL), Version
! 3, and available at http://cimss.ssec.wisc.edu/imapp/.
!
! History (relative to upstream):
! 2016/12/08, CP: For ifort compilation add end function statements.
!-----------------------------------------------------------------------


MODULE BRIGHT

IMPLICIT NONE

CONTAINS

      REAL FUNCTION BRIGHT_M(W, R)

!-----------------------------------------------------------------------
!!F77
!
!!DESCRIPTION:
!    Compute brightness temperature given monochromatic Planck radiance
!    (Radiance units: Watts per square meter per steradian per micron)
!
!!INPUT PARAMETERS:
!    W (REAL)           Wavelength (microns)
!    R (REAL)           Monochromatic Planck radiance (Watts per
!                       square meter per steradian per micron)
!
!!OUTPUT PARAMETERS:
!    BRIGHT_M (REAL)    Brightness temperature (Kelvin)
!
!!REVISION HISTORY:
!
!!TEAM-UNIQUE HEADER:
!    Liam.Gumley@ssec.wisc.edu
!
!!END
!-----------------------------------------------------------------------

      IMPLICIT NONE

! ... Include files
      include 'fundamental_constants.inc'

! ... Arguments
      real w, r

! ... Local variables
      double precision ws

! ... Set default return value
      bright_m = -1.0

! ... Check input parameters and return if they are bad
      if (w .le. 0.0 .or. r .le. 0.0) return

! ... Convert wavelength to meters
      ws = 1.0d-6 * dble(w)

! ... Compute brightness temperature
      bright_m = sngl(c2 / &
     &  (ws * log(c1 / (1.0d+6 * dble(r) * ws**5) + 1.0d+0)))

      END FUNCTION BRIGHT_M


      REAL FUNCTION BRITE_M(V, R)

!-----------------------------------------------------------------------
!!F77
!
!!DESCRIPTION:
!    Compute brightness temperature given monochromatic Planck radiance
!    (Radiance units: milliWatts per square meter per steradian per
!    inverse centimeter)
!
!!INPUT PARAMETERS:
!    V (REAL)          Wavenumber (inverse centimeters)
!    R (REAL)          Monochromatic Planck radiance (milliWatts per
!                      square meter per steradian per
!                      inverse centimeter)
!
!!OUTPUT PARAMETERS:
!    BRITE_M (REAL)    Brightness temperature (Kelvin)
!
!!REVISION HISTORY:
!
!!TEAM-UNIQUE HEADER:
!    Liam.Gumley@ssec.wisc.edu
!
!!END
!-----------------------------------------------------------------------

      IMPLICIT NONE

! ... Include files
      include 'fundamental_constants.inc'

! ... Arguments
      real v, r

! ... Local variables
      double precision vs

! ... Set default return value
      brite_m = -1.0

! ... Check input parameters and return if they are bad
      if (v .le. 0.0 .or. r .le. 0.0) return

! ... Convert wavenumber to inverse meters
      vs = 1.0d+2 * dble(v)

! ... Compute brightness temperature
      brite_m = sngl(c2 * &
     &  vs / log(c1 * vs**3 / (1.0d-5 * dble(r)) + 1.0d+0))

      END FUNCTION BRITE_M

END MODULE BRIGHT
