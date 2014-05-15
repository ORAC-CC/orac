! Name: rearrange_ecmwf.F90
!
! Purpose:
! Rearrange ecmwf data because grid is not consistent, i.e australia is in the
! centre of the map and 0 lat is at index 180. Implimented as a module called
! "rearrange" to create a generic routine.
!
! Description and Algorithm details:
! 1) Swap left and right halves.
! 2) Invert the y-axis.
!
! Arguments:
! Name       Type In/Out/Both Description
! ------------------------------------------------------------------------------
! var        real    Both Array to be rearranged. The specific function called
!                         determined by if this has 2 or 3 dimensions.
! ecmwf_dims struct  In   Summary of dimensions of ECMWF fields
! dim1,2,3   integer In   Size of the dimensions of the array. Dim3 should only
!                         be included for 3 dimensional arrays.
!
! History:
! 2013/03/06: CP Original code
! 2013/03/07: CP fixed latitudinal bug switch!
! 2013/03/19: GT Commented debugging write statements
! 2013/11/01: GM Cleaned up code and removed the use of several auxiliary arrays
! 2013/11/05: GT Bug fix. Moved declaration of dim1 & dim2 to before they
!                are used in the definition of var.
!
! $Id$
!
! Bugs:
! none known
!

module rearrange

   interface rearrange_ecmwf
      module procedure rearrange_ecmwf_2d, rearrange_ecmwf_3d
   end interface rearrange_ecmwf

contains

   subroutine rearrange_ecmwf_2d(var,ecmwf_dims,dim1,dim2)

      use ecmwf_structures, only: ecmwf_dims_s
      use preproc_constants, only: sreal

      implicit none

      integer,            intent(in)    :: dim1,dim2
      real(kind=sreal),   intent(inout) :: var(dim1,dim2)
      type(ecmwf_dims_s), intent(in)    :: ecmwf_dims

      integer          :: i
      integer          :: x_half,y_half
      real(kind=sreal) :: temp(dim1,dim2)

      x_half=int(ecmwf_dims%xdim/2.)
      y_half=int(ecmwf_dims%ydim/2.)

      ! swap left and right halfs into a temp array
      temp(1:x_half,:) =var(x_half+1:,:)
      temp(x_half+1:,:)=var(1:x_half,:)

      ! flip in the y direction from the temp to the original
      do i=1,dim2
         var(:,dim2+1-i)=temp(:,i)
      enddo
   end subroutine rearrange_ecmwf_2d

   subroutine rearrange_ecmwf_3d(var,ecmwf_dims,dim1,dim2,dim3)

      use ecmwf_structures, only: ecmwf_dims_s
      use preproc_constants, only: sreal

      implicit none

      integer,            intent(in)    :: dim1,dim2,dim3
      real(kind=sreal),   intent(inout) :: var(dim1,dim2,dim3)
      type(ecmwf_dims_s), intent(in)    :: ecmwf_dims

      integer          :: i
      integer          :: x_half,y_half
      real(kind=sreal) :: temp(dim1,dim2,dim3)

      x_half=int(ecmwf_dims%xdim/2)
      y_half=int(ecmwf_dims%ydim/2)

      ! swap left and right halfs into a temp array
      temp(1:x_half,:,:) =var(x_half+1:,:,:)
      temp(x_half+1:,:,:)=var(1:x_half,:,:)

      ! flip in the y direction from the temp to the original
      do i=1,dim2
         var(:,dim2+1-i,:)=temp(:,i,:)
      enddo
   end subroutine rearrange_ecmwf_3d

end module rearrange
