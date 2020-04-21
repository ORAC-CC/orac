!-------------------------------------------------------------------------------
! Name: ncdf_open_field.inc
!
! Purpose:
! Code shared by all version of ncdf_read_array function. See orac_ncdf.F90 for
! detailed header information.
!
! Description and Algorithm details:
! 1) Read data
! 3) Replace fill values, apply scale factor, add offset
!
! History:
! 2014/02/10, AP: Original version.
! 2014/08/15, AP: Fixed bug in management of scale factor/offset. Homogenizing
!   verbose and error printing formats. Split into open field/read field.
! 2015/07/16, GM: Modifications to properly read packed data and check ranges.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

   if (verbose) print*,'Opening variable: ',trim(name)

   ! locate variable in file
   ierr = nf90_inq_varid(ncid, name, vid)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: ncdf_read_file(): Could not locate variable ', trim(name)
      print*, trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   ! read field attributes
   fv_flag = .false.
   vr_flag = .false.

   vmin = -huge(vmin)
   vmax =  huge(vmax)

   if (nf90_get_att(ncid,vid,'_FillValue',fv).eq.NF90_NOERR) fv_flag = .true.
   if (nf90_get_att(ncid,vid,'scale_factor',sf).ne.NF90_NOERR) sf = 1.0
   if (nf90_get_att(ncid,vid,'add_offset',of).ne.NF90_NOERR) of = 0.0
   if (nf90_get_att(ncid,vid,'valid_min',vmin).eq.NF90_NOERR) vr_flag = .true.
   if (nf90_get_att(ncid,vid,'valid_max',vmax).eq.NF90_NOERR) vr_flag = .true.
