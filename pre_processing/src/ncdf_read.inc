!-------------------------------------------------------------------------------
! Name: ncdf_read.inc
!
! Purpose:
! Code shared by all version of nc_read_array function. See orac_ncdf.F90 for
! detailed header information.
!
! Description and Algorithm details:
! 1) Locate variable in file
! 2) Read data
! 3) Replace fill values, apply scale factor, add offset
!
! History:
! 2014/02/10, AP: Original version
!
! $Id$
!
! Bugs:
! None known
!-------------------------------------------------------------------------------
   
   ! locate variable in file
   ierr = NF90_INQ_VARID(ncid, name, vid)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_READ_FILE: Could not locate variable ',trim(name),'. ',NC_ERROR(ierr)
      STOP
   endif

   ! read data
   ierr = NF90_GET_VAR(ncid,vid,val,start,counter,stride)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_READ_FILE: Could not read variable ',trim(name),'. ',NC_ERROR(ierr)
      STOP
   else
      if (verbose.eq.1) print*,'Reading variable ',trim(name)
   endif

   ! replace file's fill value with our own
   ierr = NF90_GET_ATT(ncid,vid,'_FillValue',fv)
   if (ierr.eq.NF90_NOERR) then
      where (val.eq.fv) val=real_fill_value
   else
      fv=real_fill_value
   end if

   ! check for scale factor or offset of data
   ierr = NF90_GET_ATT(ncid,vid,'scale_factor',sf)
   if (ierr.eq.NF90_NOERR) flag = .true.
   ierr = NF90_GET_ATT(ncid,vid,'add_offset',of)
   if (ierr.eq.NF90_NOERR) flag = .true.
   if (flag) then
      where (val.ne.fv) val = sf*val + of
   end if

   ! additional information for print out
   if (verbose.eq.1) then
      ierr = NF90_GET_ATT(ncid,vid,'units',unit)
      if (ierr.eq.NF90_NOERR) print*,'unit: ',trim(unit)

      ierr = NF90_GET_ATT(ncid,vid,'valid_min',fv)
      if (ierr.eq.NF90_NOERR) print*,'valid_min: ',fv

      ierr = NF90_GET_ATT(ncid,vid,'valid_max',fv)
      if (ierr.eq.NF90_NOERR) print*,'valid_max: ',fv
   end if
