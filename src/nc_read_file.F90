!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
! Name: nc_read_file.F90
!
! Purpose:
! File contains subroutines to read netcdf files for various variable types.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2012/02/03, Matthias Jerg: Cleans out prototype code to prepare repository
!    upload.
! 2012/02/09, Matthias Jerg: Adds routines for ORAC.
! 2012/07/06, Matthias Jerg: Extensively overhauls and restructures the code
! 2012/08/21, Matthias Jerg: Builds this code collection based on pre- and
!    postprocessing code collections.
! 2014/04/20, Greg McGarragh: Cleaned up the code.
! 2014/04/20, Greg McGarragh: Added nc_read_array_2d_float_to_float_orac() and
!                                   nc_read_array_2d_float_to_float_orac2().
! 2014/08/04, Greg McGarragh: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
! ------------------------------------------------------------------------------

subroutine nc_read_array_1d_int_to_int_orac(ncid,n1,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)  :: ncid
   integer,            intent(in)  :: n1
   integer,            intent(in)  :: wo
   character(len=*),   intent(in)  :: cv ! variable name

   ! Output
   integer(kind=nint), intent(out) :: v(1:n1) ! the variable read in

   ! Local
   integer :: ierr,vid,start(1),counter(1),stride(1)
   real,parameter :: miss=-9999.
   integer(kind=nint) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   counter(1) = n1
   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

end subroutine nc_read_array_1d_int_to_int_orac



subroutine nc_read_array_1d_float_to_float_orac(ncid,n1,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in) :: ncid
   integer,          intent(in) :: n1
   integer,          intent(in) :: wo
   character(len=*), intent(in) :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1) ! the variable read in

   ! Local
   integer :: ierr,vid,start(1),counter(1),stride(1)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   counter(1) = n1
   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

end subroutine nc_read_array_1d_float_to_float_orac



subroutine nc_read_array_2d_float_to_float_orac(ncid,n1,n2,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: n2
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   integer :: ierr,vid,start(2),counter(2),stride(2)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start = 1

   counter(1) = n2
   counter(2) = n1

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

end subroutine nc_read_array_2d_float_to_float_orac



subroutine nc_read_array_2d_float_to_float_orac2(ncid,n1,n2,i3,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: n2
   integer,          intent(in)  :: i3
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   integer :: ierr,vid,start(3),counter(3),stride(3)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = i3
   start(2) = 1
   start(3) = 1

   counter(1) = 1
   counter(2) = n2
   counter(3) = n1

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

end subroutine nc_read_array_2d_float_to_float_orac2



subroutine nc_read_array_3d_float_to_float_orac(ncid,n1,n2,n3,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: n2
   integer,          intent(in)  :: n3
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1,1:n2,1:n3) ! the variable read in

   ! Local
   integer :: ierr,vid,start(3),counter(3),stride(3)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start = 1

   counter(1) = n3
   counter(2) = n2
   counter(3) = n1

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

end subroutine nc_read_array_3d_float_to_float_orac



subroutine nc_read_array_3d_float_orac(ncid,n1,n2,ichan,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: n2
   integer,          intent(in)  :: ichan
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1,1:n2,1) ! the variable read in

   ! Local
   integer :: ierr,vid,start(3),counter(3),stride(3)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1
   start(3) = ichan

   counter(1) = n1
   counter(2) = n2
   counter(3) = 1

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
      fv=real_fill_value
   end if

end subroutine nc_read_array_3d_float_orac



subroutine nc_read_array_1p1_float_orac(ncid,n1,ichan,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: ichan
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1,1) ! the variable read in

   ! Local
   integer :: ierr,vid,start(2),counter(2),stride(2)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = ichan
   start(2) = 1

   counter(1) = 1
   counter(2) = n1

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
      fv=real_fill_value
   end if

end subroutine nc_read_array_1p1_float_orac



subroutine nc_read_array_1p2_float_orac(ncid,n1,ichan,ilay,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: ichan
   integer,          intent(in)  :: ilay
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1,1,1) ! the variable read in

   ! Local
   integer :: ierr,vid,start(3),counter(3),stride(3)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = ichan
   start(2) = ilay
   start(3) = 1

   counter(1) = 1
   counter(2) = 1
   counter(3) = n1

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
      fv=real_fill_value
   end if

end subroutine nc_read_array_1p2_float_orac



subroutine nc_read_array_2d_double_orac(ncid,n1,n2,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: n2
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=Dreal), intent(out) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   integer :: ierr,vid,start(2),counter(2),stride(2)
   real,parameter :: miss=-9999.
   real(kind=Dreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
      fv=real_fill_value
   end if

end subroutine nc_read_array_2d_double_orac



subroutine nc_read_array_2d_float_orac(ncid,n1,n2,cv,v,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: n2
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v(1:n1,1:n2) ! the variable read in

   ! Local
   integer :: ierr,vid,start(2),counter(2),stride(2)
   real,parameter :: miss=-9999.
   real(kind=sreal) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
      fv=real_fill_value
   end if

end subroutine nc_read_array_2d_float_orac



subroutine nc_read_array_2d_byte_to_real_orac(ncid,n1,n2,cv,v_out,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in) :: ncid
   integer,          intent(in) :: n1
   integer,          intent(in) :: n2
   integer,          intent(in) :: wo
   character(len=*), intent(in) :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v_out(1:n1,1:n2) ! the variable read in

   ! Local
   integer :: ierr,vid,i,j,start(2),counter(2),stride(2)
   real,parameter :: miss=-9999.
   integer(kind=byte) :: v(1:n1,1:n2),fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'first data value: ', v(1,1)
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

   do i=1,n1
      do j=1,n2
         if ( v(i,j) .eq. fv ) then
            v_out(i,j)=real_fill_value
         else
            v_out(i,j)=real(v(i,j),kind=sreal)
         end if
      end do
   end do

end subroutine nc_read_array_2d_byte_to_real_orac



subroutine nc_read_array_2d_int_to_real_orac(ncid,n1,n2,cv,v_out,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in) :: ncid
   integer,          intent(in) :: n1
   integer,          intent(in) :: n2
   integer,          intent(in) :: wo
   character(len=*), intent(in) :: cv ! variable name

   ! Output
   real(kind=sreal), intent(out) :: v_out(1:n1,1:n2) ! the variable read in

   ! Local
   integer :: ierr,vid,i,j,start(2),counter(2),stride(2)
   real,parameter :: miss=-9999.
   integer(kind=nint) :: v(1:n1,1:n2),fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'first data value: ', v(1,1)
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

   do i=1,n1
      do j=1,n2
         if ( v(i,j) .eq. fv ) then
            v_out(i,j)=real_fill_value
         else
            v_out(i,j)=real(v(i,j),kind=sreal)
         end if
      end do
   end do

end subroutine nc_read_array_2d_int_to_real_orac



subroutine nc_read_array_2d_byte_to_byte_orac(ncid,n1,n2,cv,v_out,wo)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: n1
   integer,          intent(in)  :: n2
   integer,          intent(in)  :: wo
   character(len=*), intent(in)  :: cv ! variable name

   ! Output
   integer(kind=byte), intent(out) :: v_out(1:n1,1:n2) ! the variable read in

   ! Local
   integer :: ierr,vid,start(2),counter(2),stride(2)
   real,parameter :: miss=-9999.
   integer(kind=byte) :: fv

   ! Fortran is column-major order but NetCDF stores in row-major order
   start(1) = 1
   start(2) = 1

   counter(1) = n1
   counter(2) = n2

   stride = 1

   ierr = 0

   if (wo .eq. 1) then
      write(*,*) 'read variable: ', cv
   end if

   ierr = nf90_inq_varid(ncid, cv, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid()'
      stop
   end if

   ierr = nf90_get_var(ncid,vid,v_out,start,counter,stride)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_var()'
      stop
   end if

   ierr = nf90_get_att(ncid, vid, '_FillValue', fv)
   if (wo .eq. 1) then
      write(*,*) 'read _FillVallue: ', fv
   end if
   if (ierr .ne. NF90_NOERR) then
      if (wo .eq. 1) then
         write(*,*) 'ERROR: nf90_get_att(), att_name = _FillVallue, value = ', fv
      end if
   end if

end subroutine nc_read_array_2d_byte_to_byte_orac
