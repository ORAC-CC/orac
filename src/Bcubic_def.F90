! Name:
!    Bicubic_def
!
! Purpose:
!    Provides interfaces for the 2D interpolation routines bcuint 
!    and linint (linint is the linear analogue of bcuint).
!    
! History:
!    22nd April 2009 - Written by C. Arnold
!    (bcuint routine from Numerical Recipes in Fortran 90)
!
! Bugs:
!    None known.
! 
!---------------------------------------------------------------------
module bcubic_def

   Interface 
      Subroutine bcuint(y,y1,y2,y12,x1l,x1u,x2l,x2u,x1,x2,ansy,ansy1,ansy2)
       
         implicit none

         real, dimension(4), intent(in)		:: y,y1,y2,y12
         real, intent(in)			:: x1l,x1u,x2l,x2u,x1,x2
         real, intent(out)			:: ansy,ansy1,ansy2

      End Subroutine bcuint
   End Interface

   Interface 
      Subroutine linint(y,x1l,x1u,x2l,x2u,x1,x2,ansy,ansy1,ansy2)
       
         implicit none

         real, dimension(4), intent(in)		:: y
         real, intent(in)			:: x1l,x1u,x2l,x2u,x1,x2
         real, intent(out)			:: ansy,ansy1,ansy2

      End Subroutine linint
   End Interface

end module bcubic_def
