!-------------------------------------------------------------------------------
! Name: nc_def_var.inc
!
! Purpose:
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

   ierr = nf90_put_att(ncid, varid, 'long_name', long_name)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = long_name, value = ', long_name
      stop
   end if

   ierr = nf90_put_att(ncid, varid, 'standard_name', standard_name)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = standard_name, value = ', standard_name
      stop
   end if

   if (present(units)) then
      ierr = nf90_put_att(ncid, varid, 'units', units)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), att_name = units, value = ', units
         stop
      end if
   end if

   if (present(flag_values)) then
      ierr = nf90_put_att(ncid, varid, 'flag_values', flag_values)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), att_name = flag_values, value = ', flag_values
         stop
      end if
   end if

   if (present(flag_meanings)) then
      ierr = nf90_put_att(ncid, varid, 'flag_meanings', flag_meanings)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), att_name = flag_meanings, value = ', flag_meanings
         stop
      end if
   end if

   ierr = nf90_put_att(ncid, varid, '_FillValue', fill_value)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = _FillValue, value = ', fill_value
      stop
   end if

   ierr = nf90_put_att(ncid, varid, 'scale_factor', scale_factor)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = scale_factor, value = ', scale_factor
      stop
   end if
   ierr = nf90_put_att(ncid, varid, 'add_offset', add_offset)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = add_offset, value = ', add_offset
      stop
   end if

   ierr = nf90_put_att(ncid, varid, 'valid_min', valid_min)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_min, value = ', valid_min
      stop
   end if
   ierr = nf90_put_att(ncid, varid, 'valid_max', valid_max)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), att_name = valid_max, value = ', valid_max
      stop
   end if

   if (verbose) then
      write(*,*) 'defined variable: ', trim(name)
   end if
