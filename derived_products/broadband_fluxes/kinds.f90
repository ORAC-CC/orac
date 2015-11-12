

! CVS:  $Id: kinds.F90,v 1.1 2004/01/26 16:24:44 norm Exp $
! CVS:  $Name:  $

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module kinds

!***********************************************************************
!
!     This module defines variable precision for all common data
!     types.
!
!-----------------------------------------------------------------------

implicit none

!-----------------------------------------------------------------------

integer, parameter :: char_len  = 80,                       &
                         int_kind  = kind(1),               &
                         log_kind  = kind(.true.),          &
                         real_kind = selected_real_kind(6), &
                         dbl_kind  = selected_real_kind(13)    !13 for dble

end module kinds

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


