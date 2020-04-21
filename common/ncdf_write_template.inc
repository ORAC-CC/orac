subroutine NCDF_WRITE_NAME_1D(ncid, name, varid, values, i1, start1, n1)

   implicit none

   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: name
   integer,          intent(in)  :: varid
   integer,          intent(in)  :: i1, start1, n1
   NCDF_WRITE_TYPE(kind=NCDF_WRITE_KIND), dimension(:), intent(in) :: values

   integer               :: ierr
   integer, dimension(1) :: start, counter, stride

   start(1) = start1

   counter(1) = n1

   stride = 1

   ierr = nf90_put_var(ncid, varid, values(i1:i1 + n1 - 1), &
                       start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      print *, 'ERROR: nf90_put_var(), ', trim(nf90_strerror(ierr)), ' name = ', name
      stop error_stop_code
   end if

end subroutine NCDF_WRITE_NAME_1D


subroutine NCDF_WRITE_NAME_2D(ncid, name, varid, values, i1, start1, n1, &
                            i2, start2, n2)

   implicit none

   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: name
   integer,          intent(in)  :: varid
   integer,          intent(in)  :: i1, start1, n1
   integer,          intent(in)  :: i2, start2, n2
   NCDF_WRITE_TYPE(kind=NCDF_WRITE_KIND), dimension(:,:), intent(in) :: values

   integer               :: ierr
   integer, dimension(2) :: start, counter, stride

   start(1) = start1
   start(2) = start2

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = nf90_put_var(ncid, varid, values(i1:i1 + n1 - 1, i2:i2 + n2 - 1), &
                       start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      print *, 'ERROR: nf90_put_var(), ', trim(nf90_strerror(ierr)), ' name = ', name
      stop error_stop_code
   end if

end subroutine NCDF_WRITE_NAME_2D


subroutine NCDF_WRITE_NAME_3D(ncid, name, varid, values, i1, start1, n1, &
                            i2, start2, n2, i3, start3, n3)

   implicit none

   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: name
   integer,          intent(in)  :: varid
   integer,          intent(in)  :: i1, start1, n1
   integer,          intent(in)  :: i2, start2, n2
   integer,          intent(in)  :: i3, start3, n3
   NCDF_WRITE_TYPE(kind=NCDF_WRITE_KIND), dimension(:,:,:), intent(in) :: values

   integer               :: ierr
   integer, dimension(3) :: start, counter, stride

   start(1) = start1
   start(2) = start2
   start(3) = start3

   counter(1) = n1
   counter(2) = n2
   counter(3) = n3

   stride = 1

   ierr = nf90_put_var(ncid, varid, values(i1:i1 + n1 - 1, i2:i2 + n2 - 1, &
                       i3:i3 + n3 - 1), start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      print *, 'ERROR: nf90_put_var(), ', trim(nf90_strerror(ierr)), ', name = ', name
      stop error_stop_code
   end if

end subroutine NCDF_WRITE_NAME_3D


subroutine NCDF_WRITE_NAME_4D(ncid, name, varid, values, i1, start1, n1, &
                            i2, start2, n2, i3, start3, n3, i4, start4, n4)

   implicit none

   integer,          intent(in)  :: ncid
   character(len=*), intent(in)  :: name
   integer,          intent(in)  :: varid
   integer,          intent(in)  :: i1, start1, n1
   integer,          intent(in)  :: i2, start2, n2
   integer,          intent(in)  :: i3, start3, n3
   integer,          intent(in)  :: i4, start4, n4
   NCDF_WRITE_TYPE(kind=NCDF_WRITE_KIND), dimension(:,:,:,:), intent(in) :: values

   integer               :: ierr
   integer, dimension(4) :: start, counter, stride

   start(1) = start1
   start(2) = start2
   start(3) = start3
   start(4) = start4

   counter(1) = n1
   counter(2) = n2
   counter(3) = n3
   counter(4) = n4

   stride = 1

   ierr = nf90_put_var(ncid, varid, values(i1:i1 + n1 - 1, i2:i2 + n2 - 1, &
                       i3:i3 + n3 - 1, i4:i4 + n4 - 1), start, counter, stride)
   if (ierr .ne. NF90_NOERR) then
      print *, 'ERROR: nf90_put_var(), ', trim(nf90_strerror(ierr)), ', name = ', name
      stop error_stop_code
   end if

end subroutine NCDF_WRITE_NAME_4D
