subroutine NCDF_DEF_VAR_NAME(ncid,dimids,name,varid,verbose,chunksizes, &
   deflate_level,shuffle,long_name,standard_name,fill_value,scale_factor, &
   add_offset,valid_min,valid_max,units,flag_values,flag_masks,flag_meanings)

   use common_constants_m
   use netcdf

   implicit none

   ! Input
   integer,          intent(in)           :: ncid
   integer,          intent(in)           :: dimids(:)
   character(len=*), intent(in)           :: name
   logical,          intent(in)           :: verbose
   integer,          intent(in), optional :: chunksizes(:)
   integer,          intent(in), optional :: deflate_level
   logical,          intent(in), optional :: shuffle
   character(len=*), intent(in), optional :: long_name
   character(len=*), intent(in), optional :: standard_name
   NCDF_DEF_VAR_TYPE_PACKED(kind=NCDF_DEF_VAR_KIND_PACKED), &
                     intent(in), optional :: fill_value
   NCDF_DEF_VAR_TYPE_UNPACKED(kind=NCDF_DEF_VAR_KIND_UNPACKED), &
                     intent(in), optional :: scale_factor
   NCDF_DEF_VAR_TYPE_UNPACKED(kind=NCDF_DEF_VAR_KIND_UNPACKED), &
                     intent(in), optional :: add_offset
   NCDF_DEF_VAR_TYPE_PACKED(kind=NCDF_DEF_VAR_KIND_PACKED), &
                     intent(in), optional :: valid_min
   NCDF_DEF_VAR_TYPE_PACKED(kind=NCDF_DEF_VAR_KIND_PACKED), &
                     intent(in), optional :: valid_max
   character(len=*), intent(in), optional :: units
   character(len=*), intent(in), optional :: flag_values
   character(len=*), intent(in), optional :: flag_masks
   character(len=*), intent(in), optional :: flag_meanings

   ! Output
   integer,          intent(out)          :: varid

   integer :: ierr
   integer :: deflate_level2 = 0
   logical :: shuffle2       = .false.

   if (present(deflate_level)) then
      deflate_level2 = deflate_level
   end if

   if (present(shuffle)) then
      shuffle2 = shuffle
   end if

   if (.not. present(chunksizes)) then
      ierr = nf90_def_var(ncid, name, NCDF_DEF_VAR_NF90_TYPE, dimids, varid, &
                                deflate_level=deflate_level2, shuffle=shuffle2)
   else
      ierr = nf90_def_var(ncid, name, NCDF_DEF_VAR_NF90_TYPE, dimids, varid, &
         chunksizes=chunksizes, deflate_level=deflate_level2, shuffle=shuffle2)
   end if
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_var(), ', trim(nf90_strerror(ierr)), &
         ', name = ', name
      stop error_stop_code
   end if

   if (present(long_name)) then
      ierr = nf90_put_att(ncid, varid, 'long_name', long_name)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = long_name, value = ', long_name
         stop error_stop_code
      end if
   end if

   if (present(standard_name)) then
      ierr = nf90_put_att(ncid, varid, 'standard_name', standard_name)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = standard_name, value = ', standard_name
         stop error_stop_code
      end if
   end if

   if (present(units)) then
      ierr = nf90_put_att(ncid, varid, 'units', units)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = units, value = ', units
         stop error_stop_code
      end if
   end if

   if (present(flag_values)) then
      ierr = nf90_put_att(ncid, varid, 'flag_values', flag_values)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = flag_values, value = ', flag_values
         stop error_stop_code
      end if
   end if

   if (present(flag_masks)) then
      ierr = nf90_put_att(ncid, varid, 'flag_masks', flag_masks)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = flag_masks, value = ', flag_masks
         stop error_stop_code
      end if
   end if

   if (present(flag_meanings)) then
      ierr = nf90_put_att(ncid, varid, 'flag_meanings', flag_meanings)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = flag_meanings, value = ', flag_meanings
         stop error_stop_code
      end if
   end if

   if (present(fill_value)) then
      ierr = nf90_put_att(ncid, varid, '_FillValue', fill_value)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = _FillValue, value = ', fill_value
         stop error_stop_code
      end if
   end if

   if (present(scale_factor)) then
      ierr = nf90_put_att(ncid, varid, 'scale_factor', scale_factor)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = scale_factor, value = ', scale_factor
         stop error_stop_code
      end if
   end if

   if (present(add_offset)) then
      ierr = nf90_put_att(ncid, varid, 'add_offset', add_offset)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = add_offset, value = ', add_offset
         stop error_stop_code
      end if
   end if

   if (present(valid_min)) then
      ierr = nf90_put_att(ncid, varid, 'valid_min', valid_min)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = valid_min, value = ', valid_min
         stop error_stop_code
      end if
   end if

   if (present(valid_max)) then
      ierr = nf90_put_att(ncid, varid, 'valid_max', valid_max)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
              ', name = ', trim(name), &
              ', att_name = valid_max, value = ', valid_max
         stop error_stop_code
      end if
   end if

   if (verbose) then
      write(*,*) 'defining variable: ', trim(name)
   end if

end subroutine NCDF_DEF_VAR_NAME
