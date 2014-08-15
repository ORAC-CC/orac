!-------------------------------------------------------------------------------
! Name: date_type_structure.F90
!
! Purpose:
! Definition of date structures used by calendar.F90
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
!     /  /  ,   : First version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

MODULE DATE_TYPE_STRUCTURES

   USE PREPROC_CONSTANTS

   IMPLICIT NONE

   TYPE DATE_TYPE_DOY2GREG          ! DATE_TYPE definition
      INTEGER(kind=sint) :: YEAR_J  ! year of end of Julian calendar
      INTEGER(kind=sint) :: DOY_J   ! day of year of end of Julian calendar
      INTEGER(kind=sint) :: NDAYS   ! num of days dropped from calendar at switch
      INTEGER(kind=sint) :: TTLDAYS ! number of days in year of switch
   END TYPE DATE_TYPE_DOY2GREG

   TYPE :: DATE_TYPE_GREG2JD
      INTEGER(kind=sint) :: YEAR_J  ! year of end of Julian calendar
      INTEGER(kind=sint) :: MONTH_J ! month of end of Julian calendar
      INTEGER(kind=sint) :: DAY_J   ! day of end of Julian calendar
      INTEGER(kind=sint) :: YEAR_G  ! year of start of Gregorian calendar
      INTEGER(kind=sint) :: MONTH_G ! month of start of Gregorian calendar
      INTEGER(kind=sint) :: DAY_G   ! day of start of Gregorian calendar
   END TYPE DATE_TYPE_GREG2JD

   TYPE :: DATE_TYPE_GREG2DOY
      INTEGER(kind=sint) :: YEAR_J  ! year of end of Julian calendar
      INTEGER(kind=sint) :: MONTH_J ! month of end of Julian calendar
      INTEGER(kind=sint) :: DAY_J   ! day of end of Julian calendar
      INTEGER(kind=sint) :: YEAR_G  ! year of start of Gregorian calendar
      INTEGER(kind=sint) :: MONTH_G ! month of start of Gregorian calendar
      INTEGER(kind=sint) :: DAY_G   ! day of start of Gregorian calendar
      INTEGER(kind=sint) :: NDAYS   ! num of days dropped from calendar at switch
   END TYPE DATE_TYPE_GREG2DOY

END MODULE DATE_TYPE_STRUCTURES
