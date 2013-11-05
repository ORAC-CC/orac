! Name: calender.F90 
! 
! 
! Purpose: 
! This file contains a number of subroutines and functions 
! for time/calender computations which are necessary at various  
! steps in the code. 
!  
! 
! Description and Algorithm details: 
! 
! 
! Arguments: 
! Name Type In/Out/Both Description 
! 
! 
! Local variables: 
! Name Type Description 
! 
! 
! History: 
!2012/06/15: Matthias Jerg collects routines and adapts them for ORAC use. 
!2012/09/11:Caroline Poulsen downloaded code from 
!http://caps.gsfc.nasa.gov/simpson/software/greg2doy_f90.txt 
! 
! $Id$ 
! 
! Bugs: 
! 
!none known 
 
 
!******************************************************* 
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
!******************************************************************************* 
!  Main subroutine 
!******************************************************************************* 
 
subroutine DOY2GREG(doy,y,m,d) 
 
  use preproc_constants 
   
  use date_type_structures 
   
    IMPLICIT NONE 
        
    INTEGER(kind=stint) :: D                                                                  ! day of month (+ fraction) 
    INTEGER(kind=stint) :: DOY 
    INTEGER(kind=stint) :: K 
    INTEGER(kind=stint) :: M                                                                  ! month (1-12) 
    INTEGER(kind=stint) :: Y                                                                  ! year 
    LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian 
    LOGICAL :: LEAP 
       
    CHARACTER(LEN=9), DIMENSION(12), PARAMETER :: MONTH_NAME =               &    ! month names 
         (/ 'January  ', 'February ', 'March    ', 'April    ', 'May      ',   & 
         'June     ', 'July     ', 'August   ', 'September', 'October  ',   & 
         'November ', 'December ' /) 
     
    TYPE (DATE_TYPE_DOY2GREG), DIMENSION (3) :: GREGORIAN_START =        & 
         (/ DATE_TYPE_DOY2GREG (1582, 277, 10, 355),                       &                 ! 1: Decree by Pope Gregory XIII 
         DATE_TYPE_DOY2GREG (1752, 246, 11, 355),                       &                 ! 2: Great Britain 
         DATE_TYPE_DOY2GREG (1918,  31, 13, 352)  /)                                      ! 3: Russia 
     
    INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                    ! set to 1 for 1582 date, 2 for 1752 date, etc. 
     
    LOGICAL :: GREGORIAN_DOY2GREG 
     
     
    !----------------------------------------------------------------------------------------------------------------------------------- 
    !  Main program code 
    !----------------------------------------------------------------------------------------------------------------------------------- 
    !      write(*,*) doy,y 
!      pause 
!!$      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter day of year:  '               ! prompt for day of year 
!!$      READ (UNIT=*, FMT=*) DOY 
!!$ 
!!$      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter year:  '                      ! prompt for year 
 !!$      READ (UNIT=*, FMT=*) Y 
        ! test for Gregorian calendar: 
     GREGORIAN_FLAG = GREGORIAN_DOY2GREG(Y, DOY, GREGORIAN_START(GREGORIAN_CHOICE))   
  
       LEAP = .FALSE.                                                                ! test for leap year 
       IF (MOD(Y,4_stint) .EQ. 0) LEAP = .TRUE. 
  
       IF (GREGORIAN_FLAG) THEN                                                      ! additional Gregorian leap year tests 
          IF (MOD(Y,100_stint) .EQ. 0) LEAP = .FALSE. 
          IF (MOD(Y,400_stint) .EQ. 0) LEAP = .TRUE. 
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
  
       M = INT(9.0D0*(K+DOY)/275.0D0 + 0.98D0,kind=stint)                                       ! compute month 
       IF (DOY .LT. 32) M = 1 
  
       D = DOY - ((275*M)/9) + K*((M+9)/12) + 30                                     ! compute day of month 
  
 !!$      IF (.NOT. GREGORIAN_FLAG) THEN                                                ! print msg if Julian calendar in effect 
 !!$         WRITE (UNIT=*, FMT='(/,A)') ' Julian calendar.' 
 !!$      END IF 
 !!$ 
 !!$      IF (Y .GE. 1) THEN                                                            ! print results (AD) 
 !!$         WRITE (UNIT=*, FMT='(/,1X,A,1X,I2,", ",I7, " AD")')   & 
 !!$                TRIM(MONTH_NAME(M)), D, Y 
 !!$      ELSE                                                                          ! print results (BC) 
 !!$         WRITE (UNIT=*, FMT='(/,1X,A,1X,I2,", ",I7, " BC")')   & 
 !!$                TRIM(MONTH_NAME(M)), D, -Y+1 
 !!$      END IF 
  
     END subroutine DOY2GREG 
  
  
  
 !******************************************************************************* 
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
 !    !*********************************************************************************************************************************** 
     !  Main subroutine 
     !*************************************************************************** 
  
     subroutine GREG2JD(y,m,d,jd) 
        
       use preproc_constants 
        
       use date_type_structures 
        
       IMPLICIT NONE 
  
       !MJ: some modifications where necessary here to make it work 
       integer(kind=stint) :: D                                                                  ! day of month 
       INTEGER(kind=stint) :: dayint 
       INTEGER(kind=stint) :: M                                                                  ! month 
       INTEGER(kind=stint) :: Y,dummy                                                            ! year 
  
       real(kind=dreal) :: JD                                                                  ! Julian day 
  
       INTEGER(kind=stint) :: A, B                                                               ! intermediate variables 
       !DOUBLE PRECISION :: D                                                         ! day of month (+ fraction) 
 !      DOUBLE PRECISION :: JD                                                        ! Julian day 
       !INTEGER :: M                                                                  ! month (1-12) 
       !INTEGER :: Y                                                                  ! year 
       LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian 
        
       TYPE (DATE_TYPE_GREG2JD), DIMENSION (3) :: GREGORIAN_START =   & 
            & (/ DATE_TYPE_GREG2JD (1582, 10,  4, 1582, 10, 15),          &                      ! 1: Decree by Pope Gregory XIII 
            & DATE_TYPE_GREG2JD (1752,  9,  2, 1752,  9, 14),          &                      ! 2: Great Britain 
            & DATE_TYPE_GREG2JD (1918,  1, 31, 1918,  2, 14)  /)                              ! 3: Russia 
  
       INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                   ! set to 1 for 1582 date, 2 for 1752 date, etc. 
  
       LOGICAL :: GREGORIAN_GREG2JD 
  
 !----------------------------------------------------------------------------------------------------------------------------------- 
 !  Main program code 
 !----------------------------------------------------------------------------------------------------------------------------------- 
  
 !!$      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter month (1-12):  '              ! prompt for month 
 !!$      READ (UNIT=*, FMT=*) M 
 !!$       
 !!$      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter day:  '                       ! prompt for day of month 
 !!$      READ (UNIT=*, FMT=*) D 
 !!$ 
 !!$      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter year:  '                      ! prompt for year 
 !!$      READ (UNIT=*, FMT=*) Y 
  
       dayint=int(d,kind=stint) 
       ! test for Gregorian calendar 
 !MJ ORG      GREGORIAN_FLAG = GREGORIAN_GREG2JD(Y, M, INT(D,kind=stint), GREGORIAN_START(GREGORIAN_CHOICE))  
       GREGORIAN_FLAG = GREGORIAN_GREG2JD(Y, M, dayint, GREGORIAN_START(GREGORIAN_CHOICE))  
  
       IF (M .LE. 2_stint) THEN 
          Y=Y-1_stint 
          M=M+12_stint 
       END IF 
        
       IF (GREGORIAN_FLAG) THEN                                                      ! Gregorian calendar 
          A = Y/100 
          B = 2 - A + A/4 
       ELSE                                                                          ! Julian calendar 
          B = 0 
       END IF 
        
       JD = INT(365.25D0*(Y+4716)) + INT(30.6001D0*(M+1),kind=stint) + D + B - 1524.5D0 
        
       IF (.NOT. GREGORIAN_FLAG) THEN                                                ! print msg if Julian calendar in effect 
          WRITE (UNIT=*, FMT='(/,A)') ' Julian calendar.' 
       END IF 
        
 !      WRITE (UNIT=*, FMT='(/,A, F15.6)') ' Julian day = ', JD                       ! print result 
  
     END subroutine GREG2JD 
  
  
 !******************************************************************************* 
  
 !  GREGORIAN(S) 
 ! 
 !  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar 
 !  (return value of .FALSE.). 
 !******************************************************************************* 
  
     FUNCTION GREGORIAN_DOY2GREG (YEAR, DOY, GREG_START) RESULT (GREG_FLAG) 
  
       use preproc_constants 
  
       use date_type_structures 
  
       IMPLICIT NONE 
  
  
       INTEGER(kind=stint), INTENT(IN) :: YEAR                                                   ! input year 
       INTEGER(kind=stint), INTENT(IN) :: DOY                                                    ! input day of month 
       TYPE (DATE_TYPE_DOY2GREG), INTENT(IN) :: GREG_START                                    ! contains Julian stop/Gregorian start dates 
  
       LOGICAL :: GREG_FLAG                                                          ! result flag (.TRUE. for Gregorian) 
  
       INTEGER(kind=stint) :: CALTYPE = 0                                                        ! 0=unknown, 1=Julian, 2=Gregorian 
       INTEGER(kind=stint) :: TOTAL_DAYS 
       LOGICAL :: LEAP_FLAG 
  
       LEAP_FLAG = .FALSE. 
       IF (MOD(YEAR,4_stint) .EQ. 0) LEAP_FLAG = .TRUE. 
  
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
          IF (MOD(YEAR,100_stint) .EQ. 0) LEAP_FLAG = .FALSE. 
          IF (MOD(YEAR,400_stint) .EQ. 0) LEAP_FLAG = .TRUE. 
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
  
 !******************************************************************************* 
 !  GREGORIAN 
 ! 
 !  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar 
 !  (return value of .FALSE.). 
 !******************************************************************************* 
  
     FUNCTION GREGORIAN_GREG2JD(YEAR, MONTH, DAY, GREG_START) RESULT (GREG_FLAG) 
  
       use preproc_constants 
  
       use date_type_structures 
  
       IMPLICIT NONE 
  
       LOGICAL :: GREG_FLAG                                                          ! result flag (.TRUE. for Gregorian) 
  
       INTEGER(kind=stint), INTENT(IN) :: YEAR                                                   ! input year 
       INTEGER(kind=stint), INTENT(IN) :: MONTH                                                  ! input month 
       INTEGER(kind=stint), INTENT(IN) :: DAY                                                    ! input day of month 
       TYPE (DATE_TYPE_GREG2JD), INTENT(IN) :: GREG_START          ! contains Julian stop/Gregorian start dates 
  
       INTEGER(kind=stint) :: CALTYPE = 0                                                        ! 0=unknown, 1=Julian, 2=Gregorian 
  
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
 !************************************************ 
  
  
  
 !**************************************************** 
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
 !******************************************************************************* 
 !******************************************************************************* 
 !  Main program 
 !******************************************************************************* 
  
       subroutine GREG2DOY(Y, M, D, DOY)
  
       use preproc_constants 

       use date_type_structures

       IMPLICIT NONE 

!        TYPE :: DATE_TYPE 
!           INTEGER :: YEAR_J                                                          ! year of end of Julian calendar 
!           INTEGER :: MONTH_J                                                         ! month of end of Julian calendar 
!           INTEGER :: DAY_J                                                           ! day of end of Julian calendar 
!           INTEGER :: YEAR_G                                                          ! year of start of Gregorian calendar 
!           INTEGER :: MONTH_G                                                         ! month of start of Gregorian calendar 
!           INTEGER :: DAY_G                                                           ! day of start of Gregorian calendar 
!           INTEGER :: NDAYS                                                           ! number of days dropped from calendar at switch 
!        END TYPE DATE_TYPE 
  

!       INTEGER(kind=2), INTENT(IN) :: Y                                                   ! input year 
!       INTEGER(kind=2), INTENT(IN) :: M                                                  ! input month 
!       INTEGER(kind=2), INTENT(IN) :: D 
  
!       INTEGER(kind=2), INTENT(out) :: DOY 


       INTEGER(kind=stint), INTENT(IN) :: Y                                                   ! input year 
       INTEGER(kind=stint), INTENT(IN) :: M                                                  ! input month 
       INTEGER(kind=stint), INTENT(IN) :: D 
       INTEGER(kind=stint), INTENT(out) :: DOY 



       !MJ: some modifications where necessary here to make it work 
!        INTEGER(kind=stint) :: D                                                                  ! day of month 
        INTEGER(kind=stint) :: dayint 
!        INTEGER(kind=stint) :: M                                                                  ! month 
!        INTEGER(kind=stint) :: Y,dummy                                                            ! year 


       INTEGER :: K 
  
       LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian 
       LOGICAL :: LEAP 
  
       TYPE (DATE_TYPE_GREG2DOY), DIMENSION (3) :: GREGORIAN_START =   & 
          (/ DATE_TYPE_GREG2DOY (1582, 10,  4, 1582, 10, 15, 10),      &                      ! 1: Decree by Pope Gregory XIII 
             DATE_TYPE_GREG2DOY (1752,  9,  2, 1752,  9, 14, 11),      &                      ! 2: Great Britain 
             DATE_TYPE_GREG2DOY (1918,  1, 31, 1918,  2, 14, 13)  /)                          ! 3: Russia 
  
       INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                    ! set to 1 for 1582 date, 2 for 1752 date, etc. 
  
       LOGICAL :: GREGORIAN_GREG2DOY
  
  
 !----------------------------------------------------------------------------------------------------------------------------------- 
 !  Main program code 
 !----------------------------------------------------------------------------------------------------------------------------------- 

       dayint=int(d,kind=stint) 
!       GREGORIAN_FLAG = GREGORIAN(Y, M, INT(D), GREGORIAN_START(GREGORIAN_CHOICE))   ! test for Gregorian calendar 
       GREGORIAN_FLAG = GREGORIAN_GREG2DOY(Y, M, dayint, GREGORIAN_START(GREGORIAN_CHOICE))  

       LEAP = .FALSE. 
       IF (MOD(Y,4_stint) .EQ. 0) LEAP = .TRUE. 
  
       IF (GREGORIAN_FLAG) THEN 
          IF (MOD(Y,100_stint) .EQ. 0) LEAP = .FALSE. 
          IF (MOD(Y,400_stint) .EQ. 0) LEAP = .TRUE. 
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
  
       IF (.NOT. GREGORIAN_FLAG) THEN                                                ! print msg if Julian calendar in effect 
          WRITE (UNIT=*, FMT='(/,A)') ' Julian calendar.' 
       END IF 
  
!       WRITE (UNIT=*, FMT='(/,A, I3)') ' Day of year = ', DOY                        ! print result 
  
  
       END subroutine GREG2DOY 
  
  
  
  
  
  
 !******************************************************************************* 
 !  GREGORIAN 
 ! 
 !  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar 
 !  (return value of .FALSE.). 
 !******************************************************************************* 
  
       FUNCTION GREGORIAN_GREG2DOY (YEAR, MONTH, DAY, GREG_START) RESULT (GREG_FLAG) 

       use date_type_structures

       IMPLICIT NONE 

!        TYPE :: DATE_TYPE 
!           INTEGER :: YEAR_J                                                          ! year of end of Julian calendar 
!           INTEGER :: MONTH_J                                                         ! month of end of Julian calendar 
!           INTEGER :: DAY_J                                                           ! day of end of Julian calendar 
!           INTEGER :: YEAR_G                                                          ! year of start of Gregorian calendar 
!           INTEGER :: MONTH_G                                                         ! month of start of Gregorian calendar 
!           INTEGER :: DAY_G                                                           ! day of start of Gregorian calendar 
!           INTEGER :: NDAYS                                                           ! number of days dropped from calendar at switch 
!        END TYPE DATE_TYPE 

       INTEGER, INTENT(IN) :: YEAR                                                   ! input year 
       INTEGER, INTENT(IN) :: MONTH                                                  ! input month 
       INTEGER, INTENT(IN) :: DAY                                                    ! input day of month 
       TYPE (DATE_TYPE_GREG2DOY), INTENT(IN) :: GREG_START                                    ! contains Julian stop/Gregorian start dates 
  
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
  
 
