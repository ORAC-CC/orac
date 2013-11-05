!!$C--------------------------------------------------------------------
!!$C  Copyright (C) 2002,  Space Science and Engineering Center, 
!!$C  University C  of Wisconsin-Madison, Madison WI.
!!$C      
!!$C  This program is free software; you can redistribute it 
!!$C  and/or modify it under the terms of the GNU General 
!!$C  Public License as published by the Free Software Foundation; 
!!$C  either version 2 of the License, or (at your option) any 
!!$C  later version.
!!$C
!!$C  This program is distributed in the hope that it will be 
!!$C  useful, but WITHOUT ANY WARRANTY; without even the implied 
!!$C  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!!$C  See the  GNU General Public License for more details.
!!$C
!!$C  You should have received a copy of the GNU General Public 
!!$C  License along with this program; if not, write to the Free 
!!$C  Software Foundation, Inc., 59 Temple Place, Suite 330, 
!!$C  Boston, MA  02111-1307 USA
!!$C--------------------------------------------------------------------
!!$C
!!$C
      REAL FUNCTION BRIGHT_M(W, R)

!!$c-----------------------------------------------------------------------
!!$c!F77
!!$c
!!$c!DESCRIPTION:
!!$c    Compute brightness temperature given monochromatic Planck radiance
!!$c    (Radiance units: Watts per square meter per steradian per micron)
!!$c
!!$c!INPUT PARAMETERS:
!!$c    W (REAL)           Wavelength (microns)
!!$c    R (REAL)           Monochromatic Planck radiance (Watts per
!!$c                       square meter per steradian per micron)
!!$c
!!$c!OUTPUT PARAMETERS:
!!$c    BRIGHT_M (REAL)    Brightness temperature (Kelvin)
!!$c
!!$c!REVISION HISTORY:
!!$c
!!$c!TEAM-UNIQUE HEADER:
!!$c    Liam.Gumley@ssec.wisc.edu
!!$c
!!$c!END
!!$c-----------------------------------------------------------------------

        use preproc_constants

        IMPLICIT NONE

! ... Include files
      include 'fundamental_constants.inc'

! ... Arguments
      real w, r

! ... Local variables
      real(kind=dreal) ::  ws

! ... Set default return value
      bright_m = -1.0
      
! ... Check input parameters and return if they are bad
      if (w .le. 0.0 .or. r .le. 0.0) return
                  
! ... Convert wavelength to meters
      ws = 1.0d-6 * real(w,kind=dreal)
      
! ... Compute brightness temperature
      bright_m = real(c2 / &
           &  (ws * log(c1 / (1.0d+6 * real(r,kind=dreal) * ws**5) + 1.0d+0)),kind=sreal)

   END FUNCTION BRIGHT_M
