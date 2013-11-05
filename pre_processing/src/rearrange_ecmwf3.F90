! Name: rearrange_ecmwf3.F90
!
! Purpose:
! Rearrange ecmwf data because grid is not consistent, i.e australia is in the
! centre of the map and 0 lat is at index 180.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
!
! Local variables:
! Name Type Description
! ------------------------------------------------------------------------------
!
! History:
! 2013/03/06: Original code C. Poulsen
! 2013/03/07: Cp fixed latitudinal bug switch!
! 2013/03/19: G Thomas Commented debugging write statements
! 2013/11/01: Greg McGarragh Cleaned up code and removed the use of several
!             auxiliary arrays.
!
! $Id$
!
! Bugs:
! none known

subroutine rearrange_ecmwf3(var,ecmwf_dims,dim1,dim2,dim3)

   use ecmwf_structures
   use preproc_structures

   implicit none

   real(kind=sreal),   intent(inout) :: var(dim1,dim2,dim3)
   type(ecmwf_dims_s), intent(in)    :: ecmwf_dims
   integer,            intent(in)    :: dim1,dim2,dim3

   integer          :: i
   integer          :: x_half,y_half
   real(kind=sreal) :: temp(dim1,dim2,dim3)

   x_half=int(ecmwf_dims%xdim_ec/2)
   y_half=int(ecmwf_dims%ydim_ec/2)

   ! swap left and right halfs into a temp array
   temp(1:x_half,:,:) =var(x_half+1:,:,:)
   temp(x_half+1:,:,:)=var(1:x_half,:,:)

   ! flip in the y direction from the temp to the original
   do i=1,dim2
      var(:,dim2+1-i,:)=temp(:,i,:)
   enddo

end subroutine rearrange_ecmwf3
