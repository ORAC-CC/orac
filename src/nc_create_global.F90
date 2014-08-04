!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: nc_create_global.F90
!
! Purpose:
! A netcdf output file is opened/created for writing.
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
! 2012/11/16, Caroline Poulsen: Added calibration file version.
! 2014/08/04, Greg McGarragh: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_create_global_l2(Ctrl, path, ncid, nx, ny, dims_var, wo, type, status)

   use CTRL_def
   use ECP_Constants
   use netcdf

   implicit none

   ! Input
   type(Ctrl_t),    intent(in)  :: Ctrl
   character(len=*),intent(in)  :: path
   integer,         intent(in)  :: nx, ny
   integer,         intent(in)  :: wo
   integer,         intent(in)  :: type

   ! Output
   integer,         intent(out) :: ncid
   integer,         intent(out) :: dims_var(2)
   integer,         intent(out) :: status

   ! Local
   integer :: ierr, xdim, ydim

   character(len= 75) :: cncver,ccon,cinst,csname, csid, cuuid, &
        instname, fname, contact, website, prodtime, ctitle, cproc, cprocver, &
        prod_name, year, month,day,cal_file_ver

   ! Create new file
   ierr = nf90_create(path, NF90_CLOBBER, ncid)

   if (ierr .ne. NF90_NOERR) then
      if (type .eq. 1 ) then
         status = PrimaryFileOpenErr
         write(*,*) 'ERROR: nf90_create(), filename = ', trim(path)
         call Write_Log(Ctrl,'ERROR: nf90_create(), status = ', status)
         stop
      else if(type .eq. 2 ) then
         status = SecondaryFileOpenErr
         write(*,*) 'ERROR: nf90_create(), filename = ', trim(path)
         call Write_Log(Ctrl,'ERROR: nf90_create(), status = ', status)
         stop
      endif
   endif

   ! Define the 3 dimensions: time / lat / lon
   ierr = nf90_def_dim(ncid, 'across_track', nx, xdim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(), dim_name = across_track, xdim = ', xdim
      stop
   endif

   ierr = nf90_def_dim(ncid, 'along_track', ny, ydim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(), dim_name = along_track,  ydim = ', ydim
      stop
   endif

   if (type .eq. 1) ctitle='ESA CCI Cloud Retrieval Products L2 Primary Output File'
   if (type .eq. 2) ctitle='ESA CCI Cloud Retrieval Products L2 Secondary File'
   if (type .eq. 3) ctitle='ESA CCI Cloud Retrieval Products L2 Input/Output File'

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'File_Title', ctitle)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = File_Title', ctitle
      stop
   endif

   cncver='3.6.3'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'NetCDF_Version', cncver)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = NetCDF_Version', cncver
      stop
   endif

   ccon='CF-1.4'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'CF_Convention_Version', ccon)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = CF_Convention_Version', ccon
      stop
   endif

   cinst='CMSAF!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Processing_Institution', cinst)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Processing_Institution', cinst
      stop
   endif

   cproc='ORAC!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Processed_with', cproc)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Processed_with', cproc
      stop
   endif

   cprocver='1.0!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Processor_Version', cprocver)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Processor_Version', cprocver
      stop
   endif

   if (Ctrl%Inst%Name .eq. 'AATSR' .or. Ctrl%Inst%Name .eq. 'ATSR') then
      cal_file_ver='3.01'
      ierr = nf90_put_att(ncid, NF90_GLOBAL, 'AATSR_Calibration_Version', cal_file_ver)
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), name = AATSR_Calibration_Version', cal_file_ver
         stop
      endif
   end if

   csname='satellite name!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Satellite_Name', csname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Satellite_Name', csname
      stop
   endif

   csid='satellite id!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Satellite_ID', csid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Satellite_ID', csid
      stop
   endif

   cuuid='uuid tag!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'uuid', cuuid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = uuid', cuuid
      stop
   endif

   instname='instrument name!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Instrument_Name', instname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Instrument_Name', instname
      stop
   endif

   fname='file name!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'File_Name', fname)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = File_Name', fname
      stop
   endif

   contact='contact email!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Contact_Email', contact)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Contact_Email', contact
      stop
   endif

   website='contact website!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Contact_Website', website)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Contact_Website', website
      stop
   endif

   prodtime='production time!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Production_Time', prodtime)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Production_Time', prodtime
      stop
   endif

   prod_name='prod_name!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Product_Name', trim(adjustl(prod_name)))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Product_Name', trim(adjustl(prod_name))
      stop
   endif

   year='year!!!'
   month='month!!!'
   day='day!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Product_Date',&
      trim(adjustl(trim(adjustl(year))//trim(adjustl(month))//trim(adjustl(day)))))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Product_Date', &
         trim(adjustl(trim(adjustl(year))//trim(adjustl(month))//trim(adjustl(day))))
      stop
   endif

   ierr = nf90_enddef(ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop
   endif

   dims_var(1) = xdim
   dims_var(2) = ydim

   if (wo .eq. 1) then
      write(*,*) 'new file created: ',trim(path)
   endif

end subroutine nc_create_global_l2
