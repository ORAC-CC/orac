!-------------------------------------------------------------------------------
! Name: ncdf_read_field.inc
!
! Purpose:
! Code shared by all version of ncdf_read_array function. See orac_ncdf.F90 for
! detailed header information.
!
! Description and Algorithm details:
! 1) Locate variable in file
! 2) Read fill values, apply scale factor, add offset
!
! History:
! 2014/02/10, AP: Original version.
! 2014/08/15, AP: Fixed bug in management of scale factor/offset. Homogenizing
!   verbose and error printing formats. Split into open field/read field.
! 2014/09/03, GM: Fixed bug: fv should not be used as a loop local auxillary
!   variable.
! 2015/07/16, GM: Modifications to properly read packed data and check ranges.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

   if (verbose) then
      print*, 'Reading variable: ', trim(name)
      print*, 'Start:  ', start_pos
      print*, 'Count:  ', counter
      print*, 'Stride: ', stride
   end if

   ! read data
   ierr = nf90_get_var(ncid, vid, arr, start_pos, counter, stride)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: ncdf_read_file(): Could not read variable ', trim(name)
      print*, trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   ! replace file's fill value and out of range values with our own and apply
   ! scale factor/offset
   if (fv_flag) &
      where (arr.eq.fv) arr = fill
   if (vr_flag) then
      where (arr.ne.fill .and. arr.lt.vmin) arr = fill
      where (arr.ne.fill .and. arr.gt.vmax) arr = fill
   end if
   if (sf .ne. 1.0 .or. of .ne. 0.0) then
      where (arr.ne.fill) arr = sf*arr + of
   end if

   ! additional information for print out
   if (verbose) then
      if (nf90_get_att(ncid, vid, 'units', unit).eq.NF90_NOERR) then
           call c_to_f_str(unit)
           print*, 'Field units: ', trim(unit)
      end if
      if (vr_flag) then
           print*, 'Field valid min: ', sf*vmin + of
           print*, 'Field valid max: ', sf*vmax + of
      end if
   end if
