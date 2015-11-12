

! CVS:  $Id: newexp.F90,v 1.1 2003/11/11 21:55:13 norm Exp $
! CVS:  $Name:  $

module newexp
      use kinds
      contains

      function exp(x) result(val)
         !Fast replacement for standard exp() function
         implicit none
         real (kind = dbl_kind), intent(in) :: x
         real (kind=dbl_kind):: &
           y1 , y2 , y4 , val 
   
         y1 = 1._dbl_kind - x*(0.2507213_dbl_kind - (x*(0.0292732_dbl_kind - x*0.0038278_dbl_kind)))
         y2 = y1*y1
         y4 = y2*y2
         val = 1._dbl_kind/y4
         return
      end function exp

end module newexp
