!C--------------------------------------------------------------------
!  Copyright (C) 2002,  Space Science and Engineering Center,
!  University !  of Wisconsin-Madison, Madison WI.
!C
!  This program is free software; you can redistribute it
!  and/or modify it under the terms of the GNU General
!  Public License as published by the Free Software Foundation;
!  either version 2 of the License, or (at your option) any
!  later version.
!C
!  This program is distributed in the hope that it will be
!  useful, but WITHOUT ANY WARRANTY; without even the implied
!  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!  See the  GNU General Public License for more details.
!C
!  You should have received a copy of the GNU General Public
!  License along with this program; if not, write to the Free
!  Software Foundation, Inc., 59 Temple Place, Suite 330,
!  Boston, MA  02111-1307 USA
!C--------------------------------------------------------------------
!C
!C
REAL FUNCTION MODIS_BRIGHT(platform_name,RAD, BAND, UNITS)
  
  !C-----------------------------------------------------------------------
  !C!F90
  !C
  !C!DESCRIPTION:
  !    Compute brightness temperature for a MODIS infrared band
  !    on Terra or Aqua.
  !C
  !    Spectral responses for each IR detector were obtained from MCST:
  !    ftp://ftp.mcst.ssai.biz/pub/permanent/MCST in directories
  !    PFM_L1B_LUT_4-30-99 (Terra) and FM1_RSR_LUT_07-10-01 (Aqua).
  !C
  !    An average spectral response for all detectors in each band was
  !    computed. The detector-averaged spectral response data were used
  !    to compute the effective central wavenumbers and temperature
  !    correction coefficients included in this module.
  !C
  !    NOTE: The plaform name ('Terra' or 'Aqua') is passed to this
  !    function via the common block defined in 'platform_name.inc'.
  !C
  !C!INPUT PARAMETERS:
  !    RAD (REAL)      Planck radiance (units are determined by UNITS)
  !    BAND (LONG)     MODIS IR band number (20-25, 27-36)
  !    UNITS (LONG)    Flag defining radiance units
  !                    0 => milliWatts per square meter per
  !                         steradian per inverse centimeter
  !                    1 => Watts per square meter per
  !                         steradian per micron
  !C
  !C!OUTPUT PARAMETERS:
  !    MODIS_BRIGHT  Brightness temperature (Kelvin)
  !                  Note that a value of -1.0 is returned if
  !                  RAD .LE. 0.0, or BAND is not in range 20-25, 27-36.
  !C
  !C!REVISION HISTORY:
  !    date: 2004/01/03 17:02:24;  author: gumley
  !    Updated the spectral response files and temperature ranges
  !C
  !    Liam.Gumley@ssec.wisc.edu
  !C
  !C!TEAM-UNIQUE HEADER:
  !    Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
  !C
  !C!END
  !C-----------------------------------------------------------------------

  IMPLICIT NONE
  
  ! ... Include files
!  include 'platform_name.inc'
  
  ! ... Arguments
  real rad
  integer band, units
  
  ! ... Local variables
  real cwn_terra(16), tcs_terra(16), tci_terra(16)
  real cwn_aqua(16), tcs_aqua(16), tci_aqua(16)
  real cwn, tcs, tci
  integer index

  character*5 platform_name

  ! ... External functions
  real bright_m, brite_m
  external bright_m, brite_m
  
  ! ... Data statements

!!$c-----------------------------------------------------------------------
!!$
!!$c     TERRA MODIS DETECTOR-AVERAGED SPECTRAL RESPONSE
!!$c     (LIAM GUMLEY 2003/06/05)
!!$
!!$c     BAND 20 TEMPERATURE RANGE WAS  180.00 K TO  350.00 K
!!$c     BAND 21 TEMPERATURE RANGE WAS  180.00 K TO  400.00 K
!!$c     BAND 22 TEMPERATURE RANGE WAS  180.00 K TO  350.00 K
!!$c     BAND 23 TEMPERATURE RANGE WAS  180.00 K TO  350.00 K
!!$c     BAND 24 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 25 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 27 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 28 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 29 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 30 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 31 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 32 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 33 TEMPERATURE RANGE WAS  180.00 K TO  330.00 K
!!$c     BAND 34 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 35 TEMPERATURE RANGE WAS  180.00 K TO  310.00 K
!!$c     BAND 36 TEMPERATURE RANGE WAS  180.00 K TO  310.00 K
!!$
!!$c     BANDS
!!$c      20,  21,  22,  23,
!!$c      24,  25,  27,  28,
!!$c      29,  30,  31,  32,
!!$c      33,  34,  35,  36,

  ! ... Effective central wavenumbers (inverse centimeters)
  data cwn_terra/ &
       &  2.641767E+03, 2.505274E+03, 2.518031E+03, 2.465422E+03, &
       &  2.235812E+03, 2.200345E+03, 1.478026E+03, 1.362741E+03, &
       &  1.173198E+03, 1.027703E+03, 9.081998E+02, 8.315149E+02, &
       &  7.483224E+02, 7.309089E+02, 7.188677E+02, 7.045309E+02/ 
  
  ! ... Temperature correction slopes (no units)
  data tcs_terra/  &
       &  9.993487E-01, 9.998699E-01, 9.998604E-01, 9.998701E-01, &
       &  9.998825E-01, 9.998849E-01, 9.994942E-01, 9.994937E-01, &
       &  9.995643E-01, 9.997499E-01, 9.995880E-01, 9.997388E-01, &
       &  9.999192E-01, 9.999171E-01, 9.999174E-01, 9.999264E-01/

  ! ... Temperature correction intercepts (Kelvin)
  data tci_terra/ &
       &  4.744530E-01, 9.091094E-02, 9.694298E-02, 8.856134E-02, &
       &  7.287017E-02, 7.037161E-02, 2.177889E-01, 2.037728E-01, & 
       &  1.559624E-01, 7.989879E-02, 1.176660E-01, 6.856633E-02, &
       &  1.903625E-02, 1.902709E-02, 1.859296E-02, 1.619453E-02/

!c-----------------------------------------------------------------------

!!$c     AQUA MODIS DETECTOR-AVERAGED SPECTRAL RESPONSE
!!$c     (LIAM GUMLEY 2003/06/05)
!!$
!!$c     BAND 20 TEMPERATURE RANGE WAS  180.00 K TO  350.00 K
!!$c     BAND 21 TEMPERATURE RANGE WAS  180.00 K TO  400.00 K
!!$c     BAND 22 TEMPERATURE RANGE WAS  180.00 K TO  350.00 K
!!$c     BAND 23 TEMPERATURE RANGE WAS  180.00 K TO  350.00 K
!!$c     BAND 24 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 25 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 27 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 28 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 29 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 30 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 31 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 32 TEMPERATURE RANGE WAS  180.00 K TO  340.00 K
!!$c     BAND 33 TEMPERATURE RANGE WAS  180.00 K TO  330.00 K
!!$c     BAND 34 TEMPERATURE RANGE WAS  180.00 K TO  320.00 K
!!$c     BAND 35 TEMPERATURE RANGE WAS  180.00 K TO  310.00 K
!!$c     BAND 36 TEMPERATURE RANGE WAS  180.00 K TO  310.00 K
!!$
!!$c     BANDS
!!$c       20,  21,  22,  23,
!!$c       24,  25,  27,  28,
!!$c       29,  30,  31,  32,
!!$c       33,  34,  35,  36,

  ! ... Effective central wavenumbers (inverse centimeters)
  data cwn_aqua/ &
           &  2.647418E+03, 2.511763E+03, 2.517910E+03, 2.462446E+03, &
           &  2.248296E+03, 2.209550E+03, 1.474292E+03, 1.361638E+03, &
           &  1.169637E+03, 1.028715E+03, 9.076808E+02, 8.308397E+02, &
           &  7.482977E+02, 7.307761E+02, 7.182089E+02, 7.035020E+02/

! ... Temperature correction slopes (no units)
      data tcs_aqua/ &
           &  9.993438E-01, 9.998680E-01, 9.998649E-01, 9.998729E-01, &
           &  9.998738E-01, 9.998774E-01, 9.995732E-01, 9.994894E-01, &
           &  9.995439E-01, 9.997496E-01, 9.995483E-01, 9.997404E-01, &
           &  9.999194E-01, 9.999071E-01, 9.999176E-01, 9.999211E-01/ 

! ... Temperature correction intercepts (Kelvin)
      data tci_aqua/ &
           &  4.792821E-01, 9.260598E-02, 9.387793E-02, 8.659482E-02, &
           &  7.854801E-02, 7.521532E-02, 1.833035E-01, 2.053504E-01, &
           &  1.628724E-01, 8.003410E-02, 1.290129E-01, 6.810679E-02, &
           &  1.895925E-02, 2.128960E-02, 1.857071E-02, 1.733782E-02/

      !c-----------------------------------------------------------------------

      ! ... Set default return value
      modis_bright = -1.0
      
      ! ... Check input parameters and return if they are bad
      if (rad .le. 0.0 .or. &
           &    band .lt. 20 .or. &
           &    band .gt. 36 .or. &
           &    band .eq. 26) return

! ... Get index into coefficient arrays
      if (band .le. 25) then
         index = band - 19
      else
         index = band - 20
      endif
      
      ! ... Get the coefficients for Terra or Aqua
      if (platform_name(1:5) .eq. 'Terra' .or. &
           &    platform_name(1:5) .eq. 'terra' .or. &
           &    platform_name(1:5) .eq. 'TERRA') then
         cwn = cwn_terra(index)
         tcs = tcs_terra(index)
         tci = tci_terra(index)
      else if (platform_name(1:4) .eq. 'Aqua' .or. &
           &         platform_name(1:4) .eq. 'aqua' .or. &
           &         platform_name(1:4) .eq. 'AQUA') then
         cwn = cwn_aqua(index)
         tcs = tcs_aqua(index)
         tci = tci_aqua(index)
      endif
     
      ! ... Compute brightness temperature
      if (units .eq. 1) then

         ! ...   Radiance units are
         ! ...   Watts per square meter per steradian per micron
         modis_bright = (bright_m(1.0e+4 / cwn, rad) - tci) / tcs

         !MJ    else
         
         ! ...   Radiance units are
         ! ...   milliWatts per square meter per steradian per wavenumber
         !MJ modis_bright = (brite_m(cwn, rad) - tci) / tcs
         
      endif

    END FUNCTION MODIS_BRIGHT
