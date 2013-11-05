C--------------------------------------------------------------------
C  Copyright (C) 2002,  Space Science and Engineering Center, 
C  University C  of Wisconsin-Madison, Madison WI.
C      
C  This program is free software; you can redistribute it 
C  and/or modify it under the terms of the GNU General 
C  Public License as published by the Free Software Foundation; 
C  either version 2 of the License, or (at your option) any 
C  later version.
C
C  This program is distributed in the hope that it will be 
C  useful, but WITHOUT ANY WARRANTY; without even the implied 
C  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
C  See the  GNU General Public License for more details.
C
C  You should have received a copy of the GNU General Public 
C  License along with this program; if not, write to the Free 
C  Software Foundation, Inc., 59 Temple Place, Suite 330, 
C  Boston, MA  02111-1307 USA
C--------------------------------------------------------------------
C
C
      REAL FUNCTION BRITE_M(V, R)

c-----------------------------------------------------------------------
c!F77
c
c!DESCRIPTION:
c    Compute brightness temperature given monochromatic Planck radiance
c    (Radiance units: milliWatts per square meter per steradian per
c    inverse centimeter)
c
c!INPUT PARAMETERS:
c    V (REAL)          Wavenumber (inverse centimeters)
c    R (REAL)          Monochromatic Planck radiance (milliWatts per
c                      square meter per steradian per
c                      inverse centimeter)
c
c!OUTPUT PARAMETERS:
c    BRITE_M (REAL)    Brightness temperature (Kelvin)
c
c!REVISION HISTORY:
c
c!TEAM-UNIQUE HEADER:
c    Liam.Gumley@ssec.wisc.edu
c
c!END
c-----------------------------------------------------------------------

      IMPLICIT NONE

c ... Include files
      include 'fundamental_constants.inc'

c ... Arguments
      real v, r

c ... Local variables
      double precision vs

c ... Set default return value
      brite_m = -1.0
      
c ... Check input parameters and return if they are bad
      if (v .le. 0.0 .or. r .le. 0.0) return
                  
c ... Convert wavenumber to inverse meters
      vs = 1.0d+2 * dble(v)
      
c ... Compute brightness temperature
      brite_m = sngl(c2 *
     &  vs / log(c1 * vs**3 / (1.0d-5 * dble(r)) + 1.0d+0))
      
      END
