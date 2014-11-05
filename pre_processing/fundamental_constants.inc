!C-----------------------------------------------------------------------
!C !F77-INC
!C
!C !DESCRIPTION:
!C    Fundamental constants for Planck functions.
!C
!C !INPUT PARAMETERS:
!C    None
!C
!C !OUTPUT PARAMETERS:
!C    None
!C
!C !REVISION HISTORY:
!C
!C !TEAM-UNIQUE HEADER:
!C    Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
!C
!C !END
!C-----------------------------------------------------------------------

!c  Fundamental constants required for the monochromatic
!c  Planck function routines PLANCK_M, PLANC_M, BRIGHT_M, BRITE_M
!c
!c  Taken from the NIST Reference on Constants, Units, and Uncertainty
!c
!c  http://physics.nist.gov/cuu/Constants/
!c
!c  See also:
!c
!c  Mohr, P.J. and B.N. Taylor, "CODATA recommended values of the
!c    fundamental physical constants: 1998", Reviews of Modern Physics,
!c    Vol.72, No.2, 2000.

  !c ... Planck constant (Joule second)
  real(kind=dreal) :: h
  parameter (h = 6.62606876d-34)

  !c ... Speed of light in vacuum (meters per second)
  real(kind=dreal) :: c
  parameter (c = 2.99792458d+08)

  !c ... Boltzmann constant (Joules per Kelvin)
  real(kind=dreal) :: k
  parameter (k = 1.3806503d-23)

  !c ... Derived constants
  real(kind=dreal) :: c1, c2
  parameter (c1 = 2.0d+0 * h * c * c)
  parameter (c2 = (h * c) / k)
