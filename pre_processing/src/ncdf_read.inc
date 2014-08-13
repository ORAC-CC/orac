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
   ierr = nf90_inq_varid(ncid, name, vid)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_READ_FILE: Could not locate variable ',trim(name),'. ', &
           nc_error(ierr)
      stop
   endif

   ! read data
   ierr = nf90_get_var(ncid,vid,val,start,counter,stride)
   if (ierr.ne.NF90_NOERR) then
      print*,'NC_READ_FILE: Could not read variable ',trim(name),'. ', &
           nc_error(ierr)
      stop
   else
      if (verbose.eq.1) print*,'Reading variable ',trim(name)
   endif

   ! replace file's fill value with our own
   if (nf90_get_att(ncid,vid,'_FillValue',fv).eq.NF90_NOERR) then
      where (val.eq.fv) val=fill
   else
      fv=fill
   end if

   ! check for scale factor or offset of data
   if (nf90_get_att(ncid,vid,'scale_factor',sf).eq.NF90_NOERR) flag = .true.
   if (nf90_get_att(ncid,vid,'add_offset',of).eq.NF90_NOERR) flag = .true.
   if (flag) then
      where (val.ne.fv) val = sf*val + of
   end if

   ! additional information for print out
   if (verbose.eq.1) then
      if (nf90_get_att(ncid,vid,'units',unit).eq.NF90_NOERR) &
           print*,'unit: ',trim(unit)

      if (nf90_get_att(ncid,vid,'valid_min',fv).eq.NF90_NOERR) &
           print*,'valid_min: ',fv

      if (nf90_get_att(ncid,vid,'valid_max',fv).eq.NF90_NOERR) &
           print*,'valid_max: ',fv
   end if
