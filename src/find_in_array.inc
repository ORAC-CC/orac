integer function FIND_IN_ARRAY_NAME(arr, val) result(i)
   implicit none
   FIND_IN_ARRAY_TYPE(kind=FIND_IN_ARRAY_KIND), intent(in) :: arr(:) ! Array to be searched
   FIND_IN_ARRAY_TYPE(kind=FIND_IN_ARRAY_KIND), intent(in) :: val    ! Value to find in that array
   integer :: n
   n = size(arr)
   do i = 1, n
      if (arr(i) == val) return
   end do
   ! Failed to locate value
   i=0
   return
end function FIND_IN_ARRAY_NAME
