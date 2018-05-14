!-------------------------------------------------------------------------------
! Name: calender.F90
!
! Purpose:
! Calender/time routines.
!
! Description and Algorithm details:
! The original routines were provided as public domain by:
!    David G. Simpson
!    NASA Goddard Space Flight Center
!    Greenbelt, Maryland  20771
! and are available at
!   http://caps.gsfc.nasa.gov/simpson/software
!
! The code within has been modified for use in ORAC including conversion from
! stand-alone programs to subroutines.
!
! The original source of the algorithms is:
! "Astronomical Algorithms" (2nd ed.) by Jean Meeus (Willmann-Bell, Richmond,
! Virginia, USA, 2009).
!
! History:
! 2012/06/15, MJ: Collect routines and adapt them for ORAC use from
!    http://caps.gsfc.nasa.gov/simpson/software
! 2015/11/20, GM: Refreshed with upstream and put into subroutines.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module calender_m

   private

   public :: GREG2JD, &
             JD2GREG, &
             GREG2DOY, &
             DOY2GREG

   TYPE :: DATE_TYPE_GREG2JD
      INTEGER :: YEAR_J                                                             ! year of end of Julian calendar
      INTEGER :: MONTH_J                                                            ! month of end of Julian calendar
      INTEGER :: DAY_J                                                              ! day of end of Julian calendar
      INTEGER :: YEAR_G                                                             ! year of start of Gregorian calendar
      INTEGER :: MONTH_G                                                            ! month of start of Gregorian calendar
      INTEGER :: DAY_G                                                              ! day of start of Gregorian calendar
   END TYPE DATE_TYPE_GREG2JD

   TYPE :: DATE_TYPE_GREG2DOY
      INTEGER :: YEAR_J                                                             ! year of end of Julian calendar
      INTEGER :: MONTH_J                                                            ! month of end of Julian calendar
      INTEGER :: DAY_J                                                              ! day of end of Julian calendar
      INTEGER :: YEAR_G                                                             ! year of start of Gregorian calendar
      INTEGER :: MONTH_G                                                            ! month of start of Gregorian calendar
      INTEGER :: DAY_G                                                              ! day of start of Gregorian calendar
      INTEGER :: NDAYS                                                              ! number of days dropped from calendar at switch
   END TYPE DATE_TYPE_GREG2DOY

   TYPE :: DATE_TYPE_DOY2GREG
      INTEGER :: YEAR_J                                                             ! year of end of Julian calendar
      INTEGER :: DOY_J                                                              ! day of year of end of Julian calendar
      INTEGER :: NDAYS                                                              ! num of days dropped from calendar at switch
      INTEGER :: TTLDAYS                                                            ! number of days in year of switch
   END TYPE DATE_TYPE_DOY2GREG

contains

!***********************************************************************************************************************************
!
!                                                          G R E G 2 J D
!
!
!  Program:      GREG2JD
!
!  Programmer:   David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  20771
!
!  Date:         November 20, 2001
!
!  Language:     ANSI Standard Fortran-90
!
!  Version:      1.00b  (October 25, 2004)
!
!  Description:  This program converts a date on the Gregorian or Julian calendars to a Julian day.
!
!  Note:         Structure 'gregorian_start' defines the end dates of the Julian calendar and start dates of the Gregorian calendar.
!                Set the parameter 'gregorian_choice' to indicate the desired start date of the Gregorian calendar, as listed in
!                array 'gregorian_start'.
!
!***********************************************************************************************************************************

subroutine GREG2JD(y,m,d,jd)

      use preproc_constants_m

      IMPLICIT NONE

      INTEGER(kind=sint), intent(in) :: Y                                           ! year
      INTEGER(kind=sint), intent(in) :: M                                           ! month
!     DOUBLE PRECISION,   intent(in) :: D                                           ! day of month
      INTEGER(kind=sint), intent(in) :: D                                           ! day of month
      DOUBLE PRECISION,   intent(out) :: JD                                         ! Julian day

      INTEGER :: A, B                                                               ! intermediate variables
      INTEGER :: Y2
      INTEGER :: M2
      LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian

      TYPE (DATE_TYPE_GREG2JD), DIMENSION (3) :: GREGORIAN_START =   &
         (/ DATE_TYPE_GREG2JD (1582, 10,  4, 1582, 10, 15),          &              ! 1: Decree by Pope Gregory XIII
            DATE_TYPE_GREG2JD (1752,  9,  2, 1752,  9, 14),          &              ! 2: Great Britain
            DATE_TYPE_GREG2JD (1918,  1, 31, 1918,  2, 14)  /)                      ! 3: Russia

      INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                    ! set to 1 for 1582 date, 2 for 1752 date, etc.


!-----------------------------------------------------------------------------------------------------------------------------------
!  Main program code
!-----------------------------------------------------------------------------------------------------------------------------------

      GREGORIAN_FLAG = GREGORIAN_GREG2JD(Y, M, int(D, kind=sint), GREGORIAN_START(GREGORIAN_CHOICE))   ! test for Gregorian calendar

      Y2 = Y
      M2 = M

      IF (M .LE. 2) THEN
         Y2 = Y2 - 1
         M2 = M2 + 12
      END IF

      IF (GREGORIAN_FLAG) THEN                                                      ! Gregorian calendar
         A = Y2/100
         B = 2 - A + A/4
      ELSE                                                                          ! Julian calendar
         B = 0
      END IF

      JD = INT(365.25D0*(Y2+4716)) + INT(30.6001D0*(M2+1)) + D + B - 1524.5D0

end subroutine GREG2JD


!***********************************************************************************************************************************
!  GREGORIAN
!
!  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar
!  (return value of .FALSE.).
!***********************************************************************************************************************************

      FUNCTION GREGORIAN_GREG2JD (YEAR, MONTH, DAY, GREG_START) RESULT (GREG_FLAG)

      use preproc_constants_m

      IMPLICIT NONE

      LOGICAL :: GREG_FLAG                                                          ! result flag (.TRUE. for Gregorian)

      INTEGER(kind=sint), INTENT(IN) :: YEAR                                        ! input year
      INTEGER(kind=sint), INTENT(IN) :: MONTH                                       ! input month
      INTEGER(kind=sint), INTENT(IN) :: DAY                                         ! input day of month
      TYPE (DATE_TYPE_GREG2JD), INTENT(IN) :: GREG_START                            ! contains Julian stop/Gregorian start dates

      INTEGER :: CALTYPE = 0                                                        ! 0=unknown, 1=Julian, 2=Gregorian

!
!     Start of code.
!

      IF (YEAR .LT. GREG_START%YEAR_J) THEN                                         ! if year before end of Julian calendar..
         CALTYPE = 1                                                                ! ..then this is a Julian date
      ELSE IF (YEAR .EQ. GREG_START%YEAR_J) THEN                                    ! if this is the last year of the Julian cal..
         IF (MONTH .LT. GREG_START%MONTH_J) THEN                                    ! ..then if this is before the ending month..
            CALTYPE = 1                                                             ! ..then this is a Julian date
         ELSE IF (MONTH .EQ. GREG_START%MONTH_J) THEN                               ! if this is the ending month..
            IF (DAY .LE. GREG_START%DAY_J) THEN                                     ! ..then if this is before/at the ending date..
               CALTYPE = 1                                                          ! ..then this is a Julian date
            END IF
         END IF
      END IF

      IF (YEAR .GT. GREG_START%YEAR_G) THEN                                         ! if year after start of Gregorian calendar..
         CALTYPE = 2                                                                ! ..then this is a Gregorian date
      ELSE IF (YEAR .EQ. GREG_START%YEAR_G) THEN                                    ! if this is the first year of the Greg. cal..
         IF (MONTH .GT. GREG_START%MONTH_G) THEN                                    ! ..then if this is after the starting month..
            CALTYPE = 2                                                             ! ..then this is a Gregorian date
         ELSE IF (MONTH .EQ. GREG_START%MONTH_G) THEN                               ! if this is the starting month..
            IF (DAY .GE. GREG_START%DAY_G) THEN                                     ! ..then if this is at/after the starting date..
               CALTYPE = 2                                                          ! ..then this is a Gregorian date
            END IF
         END IF
      END IF

      SELECT CASE (CALTYPE)                                                         ! check calendar type
         CASE (0)                                                                   ! if unknown, we have an invalid date
            WRITE (UNIT=*, FMT='(A)') ' No such date.'                              ! print error message
            STOP                                                                    ! stop program
         CASE (1)                                                                   ! if Julian date..
            GREG_FLAG = .FALSE.                                                     ! ..set return value to .false.
         CASE (2)                                                                   ! if Gregorian date..
            GREG_FLAG = .TRUE.                                                      ! ..set return value to .true.
      END SELECT

      END FUNCTION GREGORIAN_GREG2JD



!***********************************************************************************************************************************
!
!                                                          J D 2 G R E G
!
!
!  Program:      JD2GREG
!
!  Programmer:   Dr. David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  20771
!
!  Date:         November 20, 2001
!
!  Language:     ANSI Standard Fortran-90
!
!  Version:      1.00b  (October 25, 2004)
!
!  Description:  This program converts a Julian day to a date on the Gregorian calendar.
!
!***********************************************************************************************************************************

subroutine JD2GREG(jd,y,m,day)

      use preproc_constants_m

      IMPLICIT NONE

      DOUBLE PRECISION,   intent(in) :: JD                                          ! Julian day
      INTEGER(kind=sint), intent(out) :: Y                                          ! year
      INTEGER(kind=sint), intent(out) :: M                                          ! month
      DOUBLE PRECISION,   intent(out) :: DAY                                        ! day of month (+ fraction)

      INTEGER :: A, B, C, E, Z, ALPHA                                               ! intermediate variables
      INTEGER :: D                                                                  ! day of month
      DOUBLE PRECISION :: JD2
      DOUBLE PRECISION :: F                                                         ! fractional part of jd
      CHARACTER(LEN=9), DIMENSION(12), PARAMETER :: MONTH_NAME =               &    ! month names
         (/ 'January  ', 'February ', 'March    ', 'April    ', 'May      ',   &
            'June     ', 'July     ', 'August   ', 'September', 'October  ',   &
            'November ', 'December ' /)



!-----------------------------------------------------------------------------------------------------------------------------------
!  Main program code
!-----------------------------------------------------------------------------------------------------------------------------------

      JD2 = JD + 0.5D0                                                               ! begin algorithm

      Z = INT(JD2)
      F = JD2 - Z

      IF (Z .LT. 2299161) THEN
         A = Z
      ELSE
         ALPHA = INT((Z-1867216.25D0)/36524.25D0)
         A = Z + 1 + ALPHA - ALPHA/4
      END IF

      B = A + 1524
      C = INT((B-122.1D0)/365.25D0)
      D = INT(365.25D0*C)
      E = INT((B-D)/30.6001D0)

      DAY = B - D - INT(30.6001D0*E) + F

      IF (E .LT. 14) THEN
         M = E - 1
      ELSE
         M = E - 13
      END IF

      IF (M .GT. 2) THEN
         Y = C - 4716
      ELSE
         Y = C - 4715
      END IF

end subroutine JD2GREG


!***********************************************************************************************************************************
!
!                                                         G R E G 2 D O Y
!
!
!  Program:      GREG2DOY
!
!  Programmer:   David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  20771
!
!  Date:         November 20, 2001
!
!  Language:     ANSI Standard Fortran-90
!
!  Version:      1.00b  (October 25, 2004)
!
!  Description:  This program converts a date on the Gregorian or Julian calendars to a day of year.
!
!  Note:         Array GREGORIAN_START defines the end dates of the Julian calendar and start dates of the Gregorian calendar.
!                Set the parameter GREGORIAN_CHOICE to indicate the desired start date of the Gregorian calendar, as listed in
!                array GREGORIAN_START.
!
!***********************************************************************************************************************************

subroutine GREG2DOY(y,m,d,doy)

      use preproc_constants_m

      IMPLICIT NONE

      INTEGER(kind=sint), intent(in) :: Y                                           ! year
      INTEGER(kind=sint), intent(in) :: M                                           ! month
      INTEGER(kind=sint), intent(in) :: D                                           ! day of month
      INTEGER(kind=sint), intent(out) :: DOY                                        ! Julian day

      INTEGER :: K
      LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian
      LOGICAL :: LEAP

      TYPE (DATE_TYPE_GREG2DOY), DIMENSION (3) :: GREGORIAN_START =   &
         (/ DATE_TYPE_GREG2DOY (1582, 10,  4, 1582, 10, 15, 10),      &             ! 1: Decree by Pope Gregory XIII
            DATE_TYPE_GREG2DOY (1752,  9,  2, 1752,  9, 14, 11),      &             ! 2: Great Britain
            DATE_TYPE_GREG2DOY (1918,  1, 31, 1918,  2, 14, 13)  /)                 ! 3: Russia

      INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                    ! set to 1 for 1582 date, 2 for 1752 date, etc.



!-----------------------------------------------------------------------------------------------------------------------------------
!  Main program code
!-----------------------------------------------------------------------------------------------------------------------------------

      GREGORIAN_FLAG = GREGORIAN_GREG2DOY(Y, M, int(D, kind=sint), GREGORIAN_START(GREGORIAN_CHOICE))   ! test for Gregorian calendar

      LEAP = .FALSE.
      IF (MOD(Y,4) .EQ. 0) LEAP = .TRUE.

      IF (GREGORIAN_FLAG) THEN
         IF (MOD(Y,100) .EQ. 0) LEAP = .FALSE.
         IF (MOD(Y,400) .EQ. 0) LEAP = .TRUE.
      END IF

      IF (LEAP) THEN
         K = 1
      ELSE
         K = 2
      END IF

      DOY = ((275*M)/9) - K*((M+9)/12) + D - 30

      IF (GREGORIAN_FLAG .AND. (Y .EQ. GREGORIAN_START(GREGORIAN_CHOICE)%YEAR_G)) THEN
         DOY = DOY - GREGORIAN_START(GREGORIAN_CHOICE)%NDAYS
      END IF

end subroutine GREG2DOY


!***********************************************************************************************************************************
!  GREGORIAN
!
!  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar
!  (return value of .FALSE.).
!***********************************************************************************************************************************

      FUNCTION GREGORIAN_GREG2DOY (YEAR, MONTH, DAY, GREG_START) RESULT (GREG_FLAG)

      use preproc_constants_m

      IMPLICIT NONE

      TYPE :: DATE_TYPE
         INTEGER :: YEAR_J                                                          ! year of end of Julian calendar
         INTEGER :: MONTH_J                                                         ! month of end of Julian calendar
         INTEGER :: DAY_J                                                           ! day of end of Julian calendar
         INTEGER :: YEAR_G                                                          ! year of start of Gregorian calendar
         INTEGER :: MONTH_G                                                         ! month of start of Gregorian calendar
         INTEGER :: DAY_G                                                           ! day of start of Gregorian calendar
         INTEGER :: NDAYS                                                           ! number of days dropped from calendar at switch
      END TYPE DATE_TYPE

      INTEGER(kind=sint), INTENT(IN) :: YEAR                                        ! input year
      INTEGER(kind=sint), INTENT(IN) :: MONTH                                       ! input month
      INTEGER(kind=sint), INTENT(IN) :: DAY                                         ! input day of month
      TYPE (DATE_TYPE_GREG2DOY), INTENT(IN) :: GREG_START                           ! contains Julian stop/Gregorian start dates

      LOGICAL :: GREG_FLAG                                                          ! result flag (.TRUE. for Gregorian)

      INTEGER :: CALTYPE = 0                                                        ! 0=unknown, 1=Julian, 2=Gregorian


      IF (YEAR .LT. GREG_START%YEAR_J) THEN                                         ! if year before end of Julian calendar..
         CALTYPE = 1                                                                ! ..then this is a Julian date
      ELSE IF (YEAR .EQ. GREG_START%YEAR_J) THEN                                    ! if this is the last year of the Julian cal..
         IF (MONTH .LT. GREG_START%MONTH_J) THEN                                    ! ..then if this is before the ending month..
            CALTYPE = 1                                                             ! ..then this is a Julian date
         ELSE IF (MONTH .EQ. GREG_START%MONTH_J) THEN                               ! if this is the ending month..
            IF (DAY .LE. GREG_START%DAY_J) THEN                                     ! ..then if this is before/at the ending date..
               CALTYPE = 1                                                          ! ..then this is a Julian date
            END IF
         END IF
      END IF

      IF (YEAR .GT. GREG_START%YEAR_G) THEN                                         ! if year after start of Gregorian calendar..
         CALTYPE = 2                                                                ! ..then this is a Gregorian date
      ELSE IF (YEAR .EQ. GREG_START%YEAR_G) THEN                                    ! if this is the first year of the Greg. cal..
         IF (MONTH .GT. GREG_START%MONTH_G) THEN                                    ! ..then if this is after the starting month..
            CALTYPE = 2                                                             ! ..then this is a Gregorian date
         ELSE IF (MONTH .EQ. GREG_START%MONTH_G) THEN                               ! if this is the starting month..
            IF (DAY .GE. GREG_START%DAY_G) THEN                                     ! ..then if this is at/after the starting date..
               CALTYPE = 2                                                          ! ..then this is a Gregorian date
            END IF
         END IF
      END IF

      SELECT CASE (CALTYPE)                                                         ! check calendar type
         CASE (0)                                                                   ! if unknown, we have an invalid date
            WRITE (UNIT=*, FMT='(A)') ' No such date.'                              ! print error message
            STOP                                                                    ! stop program
         CASE (1)                                                                   ! if Julian date..
            GREG_FLAG = .FALSE.                                                     ! ..set return value to .false.
         CASE (2)                                                                   ! if Gregorian date..
            GREG_FLAG = .TRUE.                                                      ! ..set return value to .true.
      END SELECT

      END FUNCTION GREGORIAN_GREG2DOY



!***********************************************************************************************************************************
!
!                                                         D O Y 2 G R E G
!
!
!  Program:      DOY2GREG
!
!  Programmer:   David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  20771
!
!  Date:         November 20, 2001
!
!  Language:     ANSI Standard Fortran-90
!
!  Version:      1.00b (October 25, 2004)
!
!  Description:  This program converts a date on the Gregorian or Julian calendars to a day of year.
!
!  Note:         Array GREGORIAN_START defines the end dates of the Julian calendar and start dates of the Gregorian calendar.
!                Set the parameter GREGORIAN_CHOICE to indicate the desired start date of the Gregorian calendar, as listed in
!                array GREGORIAN_START.
!
!***********************************************************************************************************************************

subroutine DOY2GREG(doy,y,m,d)

      use preproc_constants_m

      IMPLICIT NONE

      INTEGER(kind=sint), intent(inout) :: DOY                                      ! Julian day
      INTEGER(kind=sint), intent(in)    :: Y                                        ! year
      INTEGER(kind=sint), intent(out)   :: M                                        ! month
      INTEGER(kind=sint), intent(out)   :: D                                        ! day of month

      TYPE :: DATE_TYPE                                                             ! DATE_TYPE definition
         INTEGER :: YEAR_J                                                          ! year of end of Julian calendar
         INTEGER :: DOY_J                                                           ! day of year of end of Julian calendar
         INTEGER :: NDAYS                                                           ! num of days dropped from calendar at switch
         INTEGER :: TTLDAYS                                                         ! number of days in year of switch
      END TYPE DATE_TYPE

      INTEGER :: K
      LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian
      LOGICAL :: LEAP

      CHARACTER(LEN=9), DIMENSION(12), PARAMETER :: MONTH_NAME =               &    ! month names
         (/ 'January  ', 'February ', 'March    ', 'April    ', 'May      ',   &
            'June     ', 'July     ', 'August   ', 'September', 'October  ',   &
            'November ', 'December ' /)

      TYPE (DATE_TYPE_DOY2GREG), DIMENSION (3) :: GREGORIAN_START =        &
         (/ DATE_TYPE_DOY2GREG (1582, 277, 10, 355),                       &        ! 1: Decree by Pope Gregory XIII
            DATE_TYPE_DOY2GREG (1752, 246, 11, 355),                       &        ! 2: Great Britain
            DATE_TYPE_DOY2GREG (1918,  31, 13, 352)  /)                             ! 3: Russia

      INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                    ! set to 1 for 1582 date, 2 for 1752 date, etc.


!-----------------------------------------------------------------------------------------------------------------------------------
!  Main program code
!-----------------------------------------------------------------------------------------------------------------------------------

      GREGORIAN_FLAG = GREGORIAN_DOY2GREG(Y, DOY, GREGORIAN_START(GREGORIAN_CHOICE))         ! test for Gregorian calendar

      LEAP = .FALSE.                                                                ! test for leap year
      IF (MOD(Y,4) .EQ. 0) LEAP = .TRUE.

      IF (GREGORIAN_FLAG) THEN                                                      ! additional Gregorian leap year tests
         IF (MOD(Y,100) .EQ. 0) LEAP = .FALSE.
         IF (MOD(Y,400) .EQ. 0) LEAP = .TRUE.
      END IF

      IF (LEAP) THEN                                                                ! set K based on calendar type
         K = 1
      ELSE
         K = 2
      END IF

      IF ((Y .EQ. GREGORIAN_START(GREGORIAN_CHOICE)%YEAR_J) .AND.   &               ! if this is the year we switch calendars..
          (DOY .GT. GREGORIAN_START(GREGORIAN_CHOICE)%DOY_J)) THEN                  ! ..and we're on the Gregorian calendar..
         DOY = DOY + GREGORIAN_START(GREGORIAN_CHOICE)%NDAYS                        ! ..then adjust for dropped days
      END IF

      M = INT(9.0D0*(K+DOY)/275.0D0 + 0.98D0)                                       ! compute month
      IF (DOY .LT. 32) M = 1

      D = DOY - ((275*M)/9) + K*((M+9)/12) + 30                                     ! compute day of month

end subroutine DOY2GREG


!***********************************************************************************************************************************
!  GREGORIAN
!
!  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar
!  (return value of .FALSE.).
!***********************************************************************************************************************************

      FUNCTION GREGORIAN_DOY2GREG (YEAR, DOY, GREG_START) RESULT (GREG_FLAG)

      use preproc_constants_m

      IMPLICIT NONE

      TYPE :: DATE_TYPE                                                             ! DATE_TYPE definition
         INTEGER :: YEAR_J                                                          !  year of end of Julian calendar
         INTEGER :: DOY_J                                                           !  day of year of end of Julian calendar
         INTEGER :: NDAYS                                                           !  num of days dropped from calendar at switch
         INTEGER :: TTLDAYS                                                         !  number of days in year of switch
      END TYPE DATE_TYPE

      INTEGER(kind=sint), INTENT(IN) :: YEAR                                        ! input year
      INTEGER(kind=sint), INTENT(IN) :: DOY                                         ! input day of month
      TYPE (DATE_TYPE_DOY2GREG), INTENT(IN) :: GREG_START                           ! contains Julian stop/Gregorian start dates

      LOGICAL :: GREG_FLAG                                                          ! result flag (.TRUE. for Gregorian)

      INTEGER :: CALTYPE = 0                                                        ! 0=unknown, 1=Julian, 2=Gregorian
      INTEGER :: TOTAL_DAYS
      LOGICAL :: LEAP_FLAG


      LEAP_FLAG = .FALSE.
      IF (MOD(YEAR,4) .EQ. 0) LEAP_FLAG = .TRUE.

      IF (YEAR .LT. GREG_START%YEAR_J) THEN                                         ! if year before end of Julian calendar..
         CALTYPE = 1                                                                ! ..then this is a Julian date
         IF (LEAP_FLAG) THEN
            TOTAL_DAYS = 366
         ELSE
            TOTAL_DAYS = 365
         END IF
      ELSE IF (YEAR .EQ. GREG_START%YEAR_J) THEN                                    ! if this is the last year of the Julian cal..
         IF (DOY .LE. GREG_START%DOY_J) THEN                                        ! ..then if this is before the ending month..
            CALTYPE = 1                                                             ! ..then this is a Julian date
         END IF
         TOTAL_DAYS = GREG_START%TTLDAYS
      END IF

      IF (YEAR .GT. GREG_START%YEAR_J) THEN                                         ! if year after start of Gregorian calendar..
         CALTYPE = 2                                                                ! ..then this is a Gregorian date
         IF (MOD(YEAR,100) .EQ. 0) LEAP_FLAG = .FALSE.
         IF (MOD(YEAR,400) .EQ. 0) LEAP_FLAG = .TRUE.
         IF (LEAP_FLAG) THEN
            TOTAL_DAYS = 366
         ELSE
            TOTAL_DAYS = 365
         END IF
      ELSE IF (YEAR .EQ. GREG_START%YEAR_J) THEN                                    ! if this is the first year of the Greg. cal..
         IF (DOY .GT. GREG_START%DOY_J) THEN                                        ! ..then if this is after the starting month..
            CALTYPE = 2                                                             ! ..then this is a Gregorian date
         END IF
         TOTAL_DAYS = GREG_START%TTLDAYS
      END IF

      IF (DOY .GT. TOTAL_DAYS) CALTYPE = 0

      SELECT CASE (CALTYPE)                                                         ! check calendar type
         CASE (0)                                                                   ! if unknown, we have an invalid date
            WRITE (UNIT=*, FMT='(A)') ' No such day of year.'                       ! print error message
            STOP                                                                    ! stop program
         CASE (1)                                                                   ! if Julian date..
            GREG_FLAG = .FALSE.                                                     ! ..set return value to .false.
         CASE (2)                                                                   ! if Gregorian date..
            GREG_FLAG = .TRUE.                                                      ! ..set return value to .true.
      END SELECT

      END FUNCTION GREGORIAN_DOY2GREG


end module calender_m
