!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: nc_defdata.F90
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for
! different attribute/variable type combinations. Subroutines names are self
! descriptive.
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
! 2011/12/19, Matthias Jerg: Creates initial file.
! 2014/08/04, Greg McGarragh: Cleaned up the code.
! 2014/08/07, Greg McGarragh: Removed nf90_redef() and nf90_enddef() from the
!    subroutines.  It is extremely inefficient to do this for each variable
!    defined.  It should be done at the start and end of defining all the
!    variables in a file, i.e. in the code calling the subroutines in this file.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_defdata_float(ncid,dims,var_name,vid,var_lname,var_sname, &
                            var_unit,var_fill,scale,offset,vmin,vmax,wo,ierr)

   use netcdf

   implicit none

   ! Input
   integer,          intent(in) :: ncid
   integer,          intent(in) :: dims(2)
   character(len=*), intent(in) :: var_name
   character(len=*), intent(in) :: var_lname
   character(len=*), intent(in) :: var_sname
   character(len=*), intent(in) :: var_unit
   real,             intent(in) :: var_fill
   real,             intent(in) :: scale
   real,             intent(in) :: offset
   real,             intent(in) :: vmin
   real,             intent(in) :: vmax
   integer,          intent(in) :: wo

   ! Output
   integer,          intent(out) :: vid
   integer,          intent(out) :: ierr

   ierr = nf90_def_var(ncid, var_name, NF90_FLOAT, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'units', var_unit)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = units, value = ', var_unit
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_float


subroutine nc_defdata_float_no_att(ncid,dims,var_name,vid,var_lname,var_sname, &
                                   var_fill,wo,ierr)

   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: dims(2)
   character(len=*), intent(in)  :: var_name
   character(len=*), intent(in)  :: var_lname
   character(len=*), intent(in)  :: var_sname
   real,             intent(in)  :: var_fill
   integer,          intent(in)  :: wo

   ! Output
   integer,          intent(out) :: vid
   integer,          intent(out) :: ierr

   ierr = nf90_def_var(ncid, var_name, NF90_FLOAT, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_float_no_att


subroutine nc_defdata_double(ncid,dims,var_name,vid,var_lname,var_sname, &
                             var_unit,var_fill,scale,offset,vmin,vmax,wo,ierr)

   use ECP_constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: dims(2)
   character(len=*), intent(in)  :: var_name
   character(len=*), intent(in)  :: var_lname
   character(len=*), intent(in)  :: var_sname
   character(len=*), intent(in)  :: var_unit
   real(kind=dreal), intent(in)  :: var_fill
   real(kind=dreal), intent(in)  :: scale
   real(kind=dreal), intent(in)  :: offset
   real(kind=dreal), intent(in)  :: vmin
   real(kind=dreal), intent(in)  :: vmax
   integer,          intent(in)  :: wo

   ! Output
   integer,          intent(out) :: vid
   integer,          intent(out) :: ierr

   ierr = nf90_def_var(ncid, var_name, NF90_double, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'units', var_unit)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = units, value = ', var_unit
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_double


subroutine nc_defdata_short(ncid,dims,var_name,vid,var_lname,var_sname, &
                            var_unit,var_fill,scale,offset,vmin,vmax,wo,ierr)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)  :: ncid
   integer,            intent(in)  :: dims(2)
   character(len=*),   intent(in)  :: var_name
   character(len=*),   intent(in)  :: var_lname
   character(len=*),   intent(in)  :: var_sname
   character(len=*),   intent(in)  :: var_unit
   integer(kind=sint), intent(in)  :: var_fill
   real,               intent(in)  :: scale
   real,               intent(in)  :: offset
   integer(kind=sint), intent(in)  :: vmin
   integer(kind=sint), intent(in)  :: vmax
   integer,            intent(in)  :: wo

   ! Output
   integer,            intent(out) :: vid
   integer,            intent(out) :: ierr

   ierr = nf90_def_var(ncid, var_name, NF90_SHORT, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'units', var_unit)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = units, value = ', var_unit
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_short


subroutine nc_defdata_short_no_units(ncid,dims,var_name,vid,var_lname,var_sname, &
                                     var_fill,scale,offset,vmin,vmax,wo,ierr)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)  :: ncid
   integer,            intent(in)  :: dims(2)
   character(len=*),   intent(in)  :: var_name
   character(len=*),   intent(in)  :: var_lname
   character(len=*),   intent(in)  :: var_sname
   integer(kind=sint), intent(in)  :: var_fill
   real,               intent(in)  :: scale
   real,               intent(in)  :: offset
   integer(kind=sint), intent(in)  :: vmin
   integer(kind=sint), intent(in)  :: vmax
   integer,            intent(in)  :: wo

   ! Output
   integer,            intent(out) :: vid
   integer,            intent(out) :: ierr

   ierr = nf90_def_var(ncid, var_name, NF90_SHORT, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_short_no_units


subroutine nc_defdata_long(ncid,dims,var_name,vid,var_lname,var_sname, &
                           var_unit,var_fill,scale,offset,vmin,vmax,wo,ierr)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)  :: ncid
   integer,          intent(in)  :: dims(2)
   character(len=*), intent(in)  :: var_name
   character(len=*), intent(in)  :: var_lname
   character(len=*), intent(in)  :: var_sname
   character(len=*), intent(in)  :: var_unit
   integer,          intent(in)  :: var_fill
   real,             intent(in)  :: scale
   real,             intent(in)  :: offset
   integer,          intent(in)  :: vmin
   real,             intent(in)  :: vmax
   integer,          intent(in)  :: wo

   ! Output
   integer,          intent(out) :: vid
   integer,          intent(out) :: ierr

   integer :: var_ln, var_sn,var_un

   var_ln=len_trim(adjustl(var_lname))
   var_sn=len_trim(adjustl(var_sname))
   var_un=len_trim(adjustl(var_unit))

   ierr = nf90_def_var(ncid, var_name, NF90_INT, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'units', var_unit)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = units, value = ', var_unit
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_long


subroutine nc_defdata_byte(ncid,dims,var_name,vid,var_lname,var_sname, &
                           var_unit,var_fill,scale,offset,vmin,vmax,wo,ierr)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)  :: ncid
   integer,            intent(in)  :: dims(2)
   character(len=*),   intent(in)  :: var_name
   character(len=*),   intent(in)  :: var_lname
   character(len=*),   intent(in)  :: var_sname
   character(len=*),   intent(in)  :: var_unit
   integer(kind=byte), intent(in)  :: var_fill
   integer(kind=byte), intent(in)  :: scale
   integer(kind=byte), intent(in)  :: offset
   integer(kind=byte), intent(in)  :: vmin
   integer(kind=byte), intent(in)  :: vmax
   integer,            intent(in)  :: wo

   ! Output
   integer,            intent(out) :: vid
   integer,            intent(out) :: ierr

   integer :: var_ln, var_sn,var_un

   var_ln=len_trim(adjustl(var_lname))
   var_sn=len_trim(adjustl(var_sname))
   var_un=len_trim(adjustl(var_unit))

   ierr = nf90_def_var(ncid, var_name, NF90_BYTE, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'units', var_unit)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = units, value = ', var_unit
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_byte


subroutine nc_defdata_byte_flag_value(ncid,dims,var_name,vid,var_lname,var_sname, &
                                      var_unit,var_mean,var_fill,scale,offset,vmin,vmax,wo,ierr)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in)  :: ncid
   integer,            intent(in)  :: dims(2)
   character(len=*),   intent(in)  :: var_name
   character(len=*),   intent(in)  :: var_lname
   character(len=*),   intent(in)  :: var_sname
   character(len=*),   intent(in)  :: var_unit
   character(len=*),   intent(in)  :: var_mean
   integer(kind=byte), intent(in)  :: var_fill
   integer(kind=byte), intent(in)  :: scale
   integer(kind=byte), intent(in)  :: offset
   integer(kind=byte), intent(in)  :: vmin
   integer(kind=byte), intent(in)  :: vmax
   integer,            intent(in)  :: wo

   ! Output
   integer,            intent(out) :: vid
   integer,            intent(out) :: ierr

   integer :: var_ln, var_sn,var_un

   var_ln=len_trim(adjustl(var_lname))
   var_sn=len_trim(adjustl(var_sname))
   var_un=len_trim(adjustl(var_unit))

   ierr = nf90_def_var(ncid, var_name, NF90_BYTE, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'flag_values', var_unit)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = flag_values, value = ', var_unit
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'flag_meanings', var_mean)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = flag_meanings, value = ', var_mean
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_byte_flag_value


subroutine nc_defdata_short_flag_value(ncid,dims,var_name,vid,var_lname,var_sname, &
                                       var_mean,var_fill,scale,offset,vmin,vmax,wo,ierr)

   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   integer,            intent(in) :: ncid
   integer,            intent(in) :: dims(2)
   character(len=*),   intent(in) :: var_name
   character(len=*),   intent(in) :: var_lname
   character(len=*),   intent(in) :: var_sname
   character(len=*),   intent(in) :: var_mean
   integer(kind=sint), intent(in) :: var_fill
   integer(kind=sint), intent(in) :: scale
   integer(kind=sint), intent(in) :: offset
   integer(kind=sint), intent(in) :: vmin
   integer(kind=sint), intent(in) :: vmax
   integer,            intent(in) :: wo

   ! Output
   integer,            intent(out) :: vid
   integer,            intent(out) :: ierr

   integer :: var_ln, var_sn

   var_ln=len_trim(adjustl(var_lname))
   var_sn=len_trim(adjustl(var_sname))

   ierr = nf90_def_var(ncid, var_name, NF90_SHORT, dims, vid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), var_name = ', var_name
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'long_name', var_lname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', var_lname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'standard_name', var_sname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', var_sname
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'flag_meanings', var_mean)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = flag_meanings, value = ', var_mean
      stop
   end if

   ierr = nf90_put_att(ncid, vid, '_FillValue', var_fill)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', var_fill
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'scale_factor', scale)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'add_offset', offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', offset
      stop
   end if

   ierr = nf90_put_att(ncid, vid, 'valid_min', vmin)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', vmin
      stop
   end if
   ierr = nf90_put_att(ncid, vid, 'valid_max', vmax)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', vmax
      stop
   end if

   if (wo .eq. 1) then
      write(*,*) 'defined variable: ', trim(var_name)
   end if

end subroutine nc_defdata_short_flag_value
