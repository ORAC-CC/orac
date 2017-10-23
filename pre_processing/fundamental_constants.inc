!-----------------------------------------------------------------------
! This code is part of the International MODIS/AIRS Processing Package
! (IMAPP) licensed under the GNU General Public License (GPL), Version
! 3, and available at http://cimss.ssec.wisc.edu/imapp/.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! !F77-INC
!
! !DESCRIPTION:
!    Fundamental constants for Planck functions.
!
! !INPUT PARAMETERS:
!    None
!
! !OUTPUT PARAMETERS:
!    None
!
! !REVISION HISTORY:
!
! !TEAM-UNIQUE HEADER:
!    Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
!
! !END
!-----------------------------------------------------------------------

!  Fundamental constants required for the monochromatic
!  Planck function routines PLANCK_M, PLANC_M, BRIGHT_M, BRITE_M
!
!  Taken from the NIST Reference on Constants, Units, and Uncertainty
!
!  http://physics.nist.gov/cuu/Constants/
!
!  See also:
!
!  Mohr, P.J. and B.N. Taylor, "CODATA recommended values of the
!    fundamental physical constants: 1998", Reviews of Modern Physics,
!    Vol.72, No.2, 2000.

! ... Planck constant (Joule second)
      double precision h
      parameter (h = 6.62606876d-34)

! ... Speed of light in vacuum (meters per second)
      double precision c
      parameter (c = 2.99792458d+08)

! ... Boltzmann constant (Joules per Kelvin)
      double precision k
      parameter (k = 1.3806503d-23)

! ... Derived constants
      double precision c1, c2
      parameter (c1 = 2.0d+0 * h * c * c)
      parameter (c2 = (h * c) / k)
