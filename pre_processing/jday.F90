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

!***********************************************************************************************************************************
!  Main program
!***********************************************************************************************************************************

      PROGRAM GREG2JD

      IMPLICIT NONE

      TYPE :: DATE_TYPE
         INTEGER :: YEAR_J                                                          ! year of end of Julian calendar
         INTEGER :: MONTH_J                                                         ! month of end of Julian calendar
         INTEGER :: DAY_J                                                           ! day of end of Julian calendar
         INTEGER :: YEAR_G                                                          ! year of start of Gregorian calendar
         INTEGER :: MONTH_G                                                         ! month of start of Gregorian calendar
         INTEGER :: DAY_G                                                           ! day of start of Gregorian calendar
      END TYPE DATE_TYPE

      INTEGER :: A, B                                                               ! intermediate variables
      DOUBLE PRECISION :: D                                                         ! day of month (+ fraction)
      DOUBLE PRECISION :: JD                                                        ! Julian day
      INTEGER :: M                                                                  ! month (1-12)
      INTEGER :: Y                                                                  ! year
      LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian

      TYPE (DATE_TYPE), DIMENSION (3) :: GREGORIAN_START =   &
         (/ DATE_TYPE (1582, 10,  4, 1582, 10, 15),          &                      ! 1: Decree by Pope Gregory XIII
            DATE_TYPE (1752,  9,  2, 1752,  9, 14),          &                      ! 2: Great Britain
            DATE_TYPE (1918,  1, 31, 1918,  2, 14)  /)                              ! 3: Russia

      INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                    ! set to 1 for 1582 date, 2 for 1752 date, etc.

      LOGICAL :: GREGORIAN



!-----------------------------------------------------------------------------------------------------------------------------------
!  Main program code
!-----------------------------------------------------------------------------------------------------------------------------------

      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter month (1-12):  '              ! prompt for month
      READ (UNIT=*, FMT=*) M

      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter day:  '                       ! prompt for day of month
      READ (UNIT=*, FMT=*) D

      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter year:  '                      ! prompt for year
      READ (UNIT=*, FMT=*) Y

      GREGORIAN_FLAG = GREGORIAN(Y, M, INT(D), GREGORIAN_START(GREGORIAN_CHOICE))   ! test for Gregorian calendar

      IF (M .LE. 2) THEN
         Y = Y - 1
         M = M + 12
      END IF

      IF (GREGORIAN_FLAG) THEN                                                      ! Gregorian calendar
         A = Y/100
         B = 2 - A + A/4
      ELSE                                                                          ! Julian calendar
         B = 0
      END IF

      JD = INT(365.25D0*(Y+4716)) + INT(30.6001D0*(M+1)) + D + B - 1524.5D0

      IF (.NOT. GREGORIAN_FLAG) THEN                                                ! print msg if Julian calendar in effect
         WRITE (UNIT=*, FMT='(/,A)') ' Julian calendar.'
      END IF

      WRITE (UNIT=*, FMT='(/,A, F15.6)') ' Julian day = ', JD                       ! print result

      END PROGRAM GREG2JD






!***********************************************************************************************************************************
!  GREGORIAN
!
!  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar
!  (return value of .FALSE.).
!***********************************************************************************************************************************

      FUNCTION GREGORIAN (YEAR, MONTH, DAY, GREG_START) RESULT (GREG_FLAG)

      IMPLICIT NONE

      TYPE :: DATE_TYPE
         INTEGER :: YEAR_J                                                          ! year of end of Julian calendar
         INTEGER :: MONTH_J                                                         ! month of end of Julian calendar
         INTEGER :: DAY_J                                                           ! day of end of Julian calendar
         INTEGER :: YEAR_G                                                          ! year of start of Gregorian calendar
         INTEGER :: MONTH_G                                                         ! month of start of Gregorian calendar
         INTEGER :: DAY_G                                                           ! day of start of Gregorian calendar
      END TYPE DATE_TYPE

      LOGICAL :: GREG_FLAG                                                          ! result flag (.TRUE. for Gregorian)

      INTEGER, INTENT(IN) :: YEAR                                                   ! input year
      INTEGER, INTENT(IN) :: MONTH                                                  ! input month
      INTEGER, INTENT(IN) :: DAY                                                    ! input day of month
      TYPE (DATE_TYPE), INTENT(IN) :: GREG_START                                    ! contains Julian stop/Gregorian start dates

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

    END FUNCTION GREGORIAN


