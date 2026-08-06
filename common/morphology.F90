!-------------------------------------------------------------------------------
! Name: morphology.F90
!
! Purpose:
! Module which provides simple morphological image transformation functions.
!
! Description and Algorithm details:
! Four standard mathematical morphology functions are provided:
! erode:  "Shrinks" brighter (more positive) objects in an image, by an amount
!         defined by some kernel matrix (completely removing objects smaller
!         than the kernel)
! dilate: "Grows" brighter objects by an amount defined by a kernel matrix.
! open:   Apply erode and dilate sequentially to the same image, using the same
!         kernel. This will have the effect of removing any bright features
!         smaller than the kernel - i.e. can be used to remove "salt-noise" or
!         spikes in a image.
! close:  The opposite of open. This has the effect of removing any dark
!         features smaller than the kernel - i.e. can be used to remove
!         "pepper-noise" or holes in an image.
! All four functions accept two input arguments and return an integer matrix
! with the same dimension as the first input array:
! A(:,:) : The image to be transformed. Must be a 2-dimensional array (matrix)
!          of integers. Note that binary data will work (i.e. bolean
!          transformations will be done) but the output will still be integer
!          type.
! B(:,:) : The transform kernel to apply. This should also be a 2-dimensional
!          integer array, smaller than A, with an odd number of elements in
!          both dimensions.
!
! Arguments:
! None (see above)
!
! History:
! 2024/03/08, GT: First version
!
! Bugs:
! None known
!-------------------------------------------------------------------------------

module morphology_m

   implicit none

!!$  interface
!!$     function morph_erode(A, B) result(Ae)
!!$       ! Input variables
!!$       integer, dimension(:,:), intent(in)        :: A
!!$       integer, dimension(:,:), intent(in)        :: B
!!$       ! Returned variable
!!$       integer, dimension( size(A,1), size(A,2) ) :: Ae
!!$     end function morph_erode
!!$     function morph_dilate(A, B) result(Ad)
!!$       ! Input variables
!!$       integer, dimension(:,:), intent(in)        :: A
!!$       integer, dimension(:,:), intent(in)        :: B
!!$       ! Returned variable
!!$       integer, dimension( size(A,1), size(A,2) ) :: Ad
!!$     end function morph_dilate
!!$     function morph_open(A, B) result(Ao)
!!$       ! Input variables
!!$       integer, dimension(:,:), intent(in)        :: A
!!$       integer, dimension(:,:), intent(in)        :: B
!!$       ! Returned variable
!!$       integer, dimension( size(A,1), size(A,2) ) :: Ao
!!$     end function morph_open
!!$     function morph_close(A, B) result(Ac)
!!$       ! Input variables
!!$       integer, dimension(:,:), intent(in)        :: A
!!$       integer, dimension(:,:), intent(in)        :: B
!!$       ! Returned variable
!!$       integer, dimension( size(A,1), size(A,2) ) :: Ac
!!$     end function morph_close
!!$  end interface

contains

   subroutine morph_check_range(x, dx, minx, maxx, xmdx, xpdx, o0, o1)

      implicit none

      ! Input variables
      integer, intent(in)  :: x, dx, minx, maxx
      ! Output variables
      integer, intent(out) :: xmdx, xpdx, o0, o1

      if (x-dx .lt. minx) then
         xmdx = minx
         o0 = 1 + minx - (x-dx)
      else
         xmdx = x-dx
         o0 = 1
      end if
      if (x+dx .gt. maxx) then
         xpdx = maxx
         o1 = 2*dx + 1 - (x+dx) + maxx
      else
         xpdx = x+dx
         o1 = 2*dx + 1
      end if

   end subroutine morph_check_range

   function morph_erode(A, B) result(Ae)

      implicit none

      ! Input variables
      integer, dimension(:,:), intent(in)        :: A
      integer, dimension(:,:), intent(in)        :: B
      ! Returned variable
      integer, dimension( size(A,1), size(A,2) ) :: Ae
      ! Local variables
      ! Array dimensions
      integer                                    :: nx, ny, mx, my
      ! Indices in x-direction
      integer                                    :: i, i0, i1, bi0, bi1
      ! Indices in y-direction
      integer                                    :: j, j0, j1, bj0, bj1
      ! Temporary array combining input and kernel
      integer, dimension( size(B,1), size(B,2) ) :: Ab

      ! Check that B has odd dimensions
      if ((MOD(size(B,1), 2) .ne. 1) .or. (MOD(size(B,2), 2) .ne. 1)) then
         write(*,*) 'ERROR: morph_erode(): Kernel array, B, must have odd dimensions'
         stop 1
      end if

      ! Define dimension variables
      nx = size(A,1)
      ny = size(A,2)
      mx = size(B,1) / 2
      my = size(B,2) / 2

      ! Now step through the A array, applying the erode transform
      do i = 1, nx
         do j = 1, ny
            call morph_check_range(i, mx, 1, nx, i0, i1, bi0, bi1)
            call morph_check_range(j, my, 1, ny, j0, j1, bj0, bj1)
            Ab(:,:) = 0
            Ab(bi0:bi1, bj0:bj1) = A(i0:i1, j0:j1) * B(bi0:bi1, bj0:bj1)
            Ae(i, j) = minval(Ab, mask = B .gt. 0)
         end do
      end do

   end function morph_erode

   function morph_dilate(A, B) result(Ad)

      implicit none
      ! Input variables
      integer, dimension(:,:), intent(in)        :: A
      integer, dimension(:,:), intent(in)        :: B
      ! Returned variable
      integer, dimension( size(A,1), size(A,2) ) :: Ad
      ! Local variables
      ! Array dimensions
      integer                                    :: nx, ny, mx, my
      ! Indices in x-direction
      integer                                    :: i, i0, i1, bi0, bi1
      ! Indices in y-direction
      integer                                    :: j, j0, j1, bj0, bj1
      ! Temporary array combining input and kernel
      integer, dimension( size(B,1), size(B,2) ) :: Ab

      ! Check that B has odd dimensions
      if ((MOD(size(B,1), 2) .ne. 1) .or. (MOD(size(B,2), 2) .ne. 1)) then
         write(*,*) 'ERROR: morph_dilate(): Kernel array, B, must have odd dimensions'
         stop 1
      end if

      ! Define dimension variables
      nx = size(A,1)
      ny = size(A,2)
      mx = size(B,1) / 2
      my = size(B,2) / 2

      ! Now step through the A array, applying the dilate transform
      do i = 1, nx
         do j = 1, ny
            call morph_check_range(i, mx, 1, nx, i0, i1, bi0, bi1)
            call morph_check_range(j, my, 1, ny, j0, j1, bj0, bj1)
            Ab(:,:) = 0
            Ab(bi0:bi1, bj0:bj1) = A(i0:i1, j0:j1) * B(bi0:bi1, bj0:bj1)
            Ad(i, j) = maxval(Ab, mask = B .gt. 0)
         end do
      end do

   end function morph_dilate

   function morph_open(A, B) result(Ao)
      implicit none
      ! Input variables
      integer, dimension(:,:), intent(in)        :: A
      integer, dimension(:,:), intent(in)        :: B
      ! Returned variable
      integer, dimension( size(A,1), size(A,2) ) :: Ao

      ! Simply call dilate on the erode of A, using the kernel B
      Ao = morph_dilate( morph_erode(A, B), B)

   end function morph_open

   function morph_close(A, B) result(Ac)
      implicit none
      ! Input variables
      integer, dimension(:,:), intent(in)        :: A
      integer, dimension(:,:), intent(in)        :: B
      ! Returned variable
      integer, dimension( size(A,1), size(A,2) ) :: Ac

      ! Simply call dilate on the erode of A, using the kernel B
      Ac = morph_erode( morph_dilate(A, B), B)

   end function morph_close


end module morphology_m
