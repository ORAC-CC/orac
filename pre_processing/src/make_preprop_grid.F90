! Name: make_preprop_grid.F90
!
!
! Purpose:
! Build the preprocessing grid
! 
! Description and Algorithm details:
! 1) The latitude grid is defined by, from an initial index of 1,
!       lat(i) = (i-1)/dellat - lat_offset + 1/(2dellat)
! 2) The longitude grid is defined equivalently.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! preproc_dims   struct in  Summary of preprocessing grid definitions
! preproc_geoloc struct out Summary of lat/lon values
!
! History:
! 2012/02/21: MJ produces initial code version.
! 2012/05/02: GT Bug fixes: -Implicit none statement moved to correct location
!                           -float(x) statements replaced with real(x) 
! 2013/10/23: AP Replaced maths with equivalent and more accurate form
!
! $Id$
!
! Bugs:
! none known
! 


subroutine make_preprop_grid(preproc_dims,preproc_geoloc)

   use preproc_constants
   use preproc_structures

   implicit none

   type(preproc_geoloc_s) :: preproc_geoloc
   type(preproc_dims_s)   :: preproc_dims

   integer(kind=lint)     :: i
   real(sreal)            :: fac

   !build the arrays for the regular grid
   !create grid resolution lat
   fac = 1. / preproc_dims%dellat
   preproc_geoloc%latitude(1) = 0.5*fac - real(preproc_dims%lat_offset,sreal)
   DO i=2,preproc_dims%ydim
      preproc_geoloc%latitude(i) = preproc_geoloc%latitude(i-1) + fac
   ENDDO

   !create grid resolution lon
   fac = 1. / preproc_dims%dellon
   preproc_geoloc%longitude(1) = 0.5*fac - real(preproc_dims%lon_offset,sreal)
   DO i=2,preproc_dims%xdim
      preproc_geoloc%longitude(i) = preproc_geoloc%longitude(i-1) + fac      
   ENDDO


end subroutine make_preprop_grid
