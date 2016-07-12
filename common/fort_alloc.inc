subroutine FORT_ALLOC_NAME_1D(ptr, n0, m0) bind(C,name=XSTR(FORT_ALLOC_NAME_1D))
   use iso_c_binding
   implicit none

   type(c_ptr),    intent(inout) :: ptr
   integer(c_int), intent(inout) :: n0
   integer(c_int), intent(in)    :: m0
   FORT_ALLOC_TYPE, pointer   :: arr(:)

   if (n0 /= m0) then
      if (n0 /= 0) then
         call c_f_pointer(ptr, arr, [n0])
         deallocate(arr)
      end if
      allocate(arr(m0))
      n0 = m0
      ptr = c_loc(arr(1))
   end if

end subroutine FORT_ALLOC_NAME_1D

subroutine FORT_ALLOC_NAME_2D(ptr, n0, n1, m0, m1) bind(C,name=XSTR(FORT_ALLOC_NAME_2D))
   use iso_c_binding
   implicit none

   type(c_ptr),    intent(inout) :: ptr
   integer(c_int), intent(inout) :: n0, n1
   integer(c_int), intent(in)    :: m0, m1
   FORT_ALLOC_TYPE, pointer   :: arr(:,:)

   if (n0 /= m0 .or. n1 /= m1) then
      if (n0 /= 0 .or. n1 /= 0) then
         call c_f_pointer(ptr, arr, [n0,n1])
         deallocate(arr)
      end if
      allocate(arr(m0,m1))
      n0 = m0
      n1 = m1
      ptr = c_loc(arr(1,1))
   end if

end subroutine FORT_ALLOC_NAME_2D
