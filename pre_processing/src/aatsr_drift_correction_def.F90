! Name: aatsr_drift_correction_def
!
! Purpose:
! Module which provides the definition of the aatsr_drift_lut structure
! and prototypes of the various AATSR drift correction rountines (the
! routines themselves, with documentation, are in
! aatsr_apply_corrections.F90)
!
! History:
! 20 Jun 2012 Gareth Thomas: Original
!
! $Id$
!
! Bugs:
!
module aatsr_drift_structure
  use preproc_constants
  
  implicit none
  
  type aatsr_drift_lut
     integer(kind=lint)                   :: n
     integer(kind=stint), dimension(4000) :: year, month, day
     integer(kind=stint), dimension(4000) :: hour, minute, second
     real(kind=dreal), dimension(4000)    :: julday
     real(kind=sreal), dimension(4,4000)  :: ch
     real(kind=sreal), dimension(4,4000)  :: er
  end type aatsr_drift_lut

  ! Month strings used in the LUT files
  character(3), dimension(12), parameter  :: monthname &
       = (/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
            'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /)

end module aatsr_drift_structure
