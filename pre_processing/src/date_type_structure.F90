module date_type_structures

  use preproc_constants

    implicit none

    TYPE DATE_TYPE_DOY2GREG                                                            ! DATE_TYPE definition
       INTEGER(kind=stint) :: YEAR_J                                                          !  year of end of Julian calendar
       INTEGER(kind=stint) :: DOY_J                                                           !  day of year of end of Julian calendar
       INTEGER(kind=stint) :: NDAYS                                                           !  num of days dropped from calendar at switch
       INTEGER(kind=stint) :: TTLDAYS                                                         !  number of days in year of switch
    END TYPE DATE_TYPE_DOY2GREG

    TYPE :: DATE_TYPE_GREG2JD
       INTEGER(kind=stint) :: YEAR_J                                                          ! year of end of Julian calendar
       INTEGER(kind=stint) :: MONTH_J                                                         ! month of end of Julian calendar
       INTEGER(kind=stint) :: DAY_J                                                           ! day of end of Julian calendar
       INTEGER(kind=stint) :: YEAR_G                                                          ! year of start of Gregorian calendar
       INTEGER(kind=stint) :: MONTH_G                                                         ! month of start of Gregorian calendar
       INTEGER(kind=stint) :: DAY_G                                                           ! day of start of Gregorian calendar
    END TYPE DATE_TYPE_GREG2JD

    TYPE :: DATE_TYPE_GREG2DOY
       INTEGER(kind=stint) :: YEAR_J                                                          ! year of end of Julian calendar
       INTEGER(kind=stint) :: MONTH_J                                                         ! month of end of Julian calendar
       INTEGER(kind=stint) :: DAY_J                                                           ! day of end of Julian calendar
       INTEGER(kind=stint) :: YEAR_G                                                          ! year of start of Gregorian calendar
       INTEGER(kind=stint) :: MONTH_G                                                         ! month of start of Gregorian calendar
       INTEGER(kind=stint) :: DAY_G                                                           ! day of start of Gregorian calendar
       INTEGER(kind=stint) :: NDAYS                                                           !  num of days dropped from calendar at switch
    END TYPE DATE_TYPE_GREG2DOY


  end module date_type_structures
