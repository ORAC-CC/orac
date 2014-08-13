!-------------------------------------------------------------------------------
! Name: rearrange_ecmwf.F90
!
! Purpose:
! Rearrange ecmwf data because grid is not consistent, i.e australia is in the
! centre of the map and 0 lat is at index 180. Implemented as a module called
! "rearrange" to create a generic routine.
!
! Description and Algorithm details:
! 1) Swap left and right halves.
! 2) Invert the y-axis.
!
! Arguments:
! Name       Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf      struct  Both   Summary of contents of ECMWF files
!
! History:
! 2013/03/06, CP: Original code
! 2013/03/07, CP: fixed latitudinal bug switch!
! 2013/03/19, GT: Commented debugging write statements
! 2013/11/01, GM: Cleaned up code and removed the use of several auxiliary arrays
! 2013/11/05, GT: Bug fix. Moved declaration of dim1 & dim2 to before they
!   are used in the definition of var.
! 2014/05/07, AP: Restructuring for smaller ECMWF structure.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine rearrange_ecmwf(ecmwf)

   implicit none

   type(ecmwf_s), intent(inout) :: ecmwf

   integer                      :: date, ind, i
   real(kind=sreal)             :: utemp(ecmwf%xdim,ecmwf%ydim)
   real(kind=sreal)             :: vtemp(ecmwf%xdim,ecmwf%ydim)
   real(kind=sreal)             :: lontemp(ecmwf%xdim), lattemp(ecmwf%ydim)

   ! find dateline
   date=1
   do while (ecmwf%lon(date) .lt. 180.)
      date = date + 1
   end do
   ind = ecmwf%xdim + 1 - date

   ! swap left and right halfs into a temp array
   utemp(1:ind,:) = ecmwf%u10(date:,:)
   vtemp(1:ind,:) = ecmwf%v10(date:,:)
   lontemp(1:ind) = ecmwf%lon(date:) - 360.
   utemp(date:,:) = ecmwf%u10(1:ind,:)
   vtemp(date:,:) = ecmwf%v10(1:ind,:)
   lontemp(date:) = ecmwf%lon(1:ind)

   ecmwf%lon = lontemp

   ! flip in the y direction from the temp to the original
   lattemp=ecmwf%lat
   do i=1,ecmwf%ydim
      ecmwf%u10(:,ecmwf%ydim+1-i) = utemp(:,i)
      ecmwf%v10(:,ecmwf%ydim+1-i) = vtemp(:,i)
      ecmwf%lat(ecmwf%ydim+1-i)   = lattemp(i)
   end do

end subroutine rearrange_ecmwf
