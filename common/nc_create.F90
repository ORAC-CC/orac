!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: nc_create.F90
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
! 2014/08/31, Greg McGarragh: Make the global attribute list consistent with
!    CF-1.4.
!
! $Id: nc_create_global.F90 2290 2014-08-12 08:24:01Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_create(path, ncid, nx, ny, dims_var, inst_name, wo, type, status)

   use common_constants
   use netcdf

   implicit none

   ! Input
   character(len=*),intent(in)  :: path
   integer,         intent(in)  :: nx, ny
   character(len=*),intent(in)  :: inst_name
   integer,         intent(in)  :: wo
   integer,         intent(in)  :: type

   ! Output
   integer,         intent(out) :: ncid
   integer,         intent(out) :: dims_var(2)
   integer,         intent(out) :: status

   ! Local
   integer :: ierr, xdim, ydim

   character(len = 128) :: temp_string


   !----------------------------------------------------------------------------
   ! Create new file
   !----------------------------------------------------------------------------
   ierr = nf90_create(path, NF90_CLOBBER, ncid)
   if (ierr .ne. NF90_NOERR) then
      status = -1
      write(*,*) 'ERROR: nf90_create(), filename = ', trim(path)
      stop
   end if

   ! Define the 3 dimensions: time / lat / lon
   ierr = nf90_def_dim(ncid, 'across_track', nx, xdim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(), dim_name = across_track, xdim = ', xdim
      stop
   end if

   ierr = nf90_def_dim(ncid, 'along_track', ny, ydim)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(), dim_name = along_track,  ydim = ', ydim
      stop
   end if


   !----------------------------------------------------------------------------
   ! Global attribute 'Conventions' as defined by CF-1.4, section 2.6.1.
   !----------------------------------------------------------------------------
   temp_string='Conventions!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Conventions', trim(temp_string)
      stop
   end if


   !----------------------------------------------------------------------------
   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   !----------------------------------------------------------------------------
   if (type .eq. 1) then
      temp_string = 'ESA CCI Cloud Retrieval Products L2 Primary File'
   else if (type .eq. 2) then
      temp_string = 'ESA CCI Cloud Retrieval Products L2 Secondary File'
   else
      write(*,*) 'ERROR: nf90_create(), invalid file type: ', type
      stop
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'title', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = title', trim(temp_string)
      stop
   end if

   temp_string='institution!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'institution', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = institution', trim(temp_string)
      stop
   end if

   temp_string='source!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'source', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = source', trim(temp_string)
      stop
   end if

   temp_string='history!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'history', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = history', trim(temp_string)
      stop
   end if

   temp_string='references!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'references', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = references', trim(temp_string)
      stop
   end if

   temp_string='comment!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'comment', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = comment', trim(temp_string)
      stop
   end if


   !----------------------------------------------------------------------------
   ! Extra global attributes defined by Orac
   !----------------------------------------------------------------------------
   temp_string='Project!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Project', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Project', trim(temp_string)
      stop
   end if

   temp_string='File_Name!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'File_Name', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = File_Name', trim(temp_string)
      stop
   end if

   temp_string='UUID!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'UUID', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = UUID', trim(temp_string)
      stop
   end if

   temp_string='NetCDF_Version!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'NetCDF_Version', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = NetCDF_Version', trim(temp_string)
      stop
   end if

   temp_string='Product_Name!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Product_Name', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Product_Name', trim(temp_string)
      stop
   end if

   temp_string=trim('year!!!')//trim('month!!!')//trim('day!!!')
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Product_Date', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Product_Date', trim(temp_string)
      stop
   end if

   temp_string='Production_Time!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Production_Time', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Production_Time', trim(temp_string)
      stop
   end if

   temp_string='ORAC'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'L2_Processor', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = L2_Processor', trim(temp_string)
      stop
   end if

   temp_string='L2_Processor_Version!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'L2_Processor_Version', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = L2_Processor_Version', trim(temp_string)
      stop
   end if

   temp_string='Platform!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Platform', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Platform', trim(temp_string)
      stop
   end if

   temp_string='Sensor!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Sensor', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Sensor', trim(temp_string)
      stop
   end if

   if (inst_name .eq. 'AATSR' .or. inst_name .eq. 'ATSR') then
      temp_string='3.01'
      ierr = nf90_put_att(ncid, NF90_GLOBAL, 'AATSR_Calibration_Version', trim(temp_string))
      if (ierr .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), name = AATSR_Calibration_Version', trim(temp_string)
         stop
      end if
   end if

   temp_string='Contact_Email!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Contact_Email', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Contact_Email', trim(temp_string)
      stop
   end if

   temp_string='Contact_Website!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Contact_Website', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Contact_Website', trim(temp_string)
      stop
   end if

   temp_string='Keywords!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Keywords', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Keywords', trim(temp_string)
      stop
   end if

   temp_string='Summary!!!'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Summary', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = Summary', trim(temp_string)
      stop
   end if

   temp_string='GNU General Public License (GPL), Version 3'
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'License', trim(temp_string))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), name = License', trim(temp_string)
      stop
   end if


   ierr = nf90_enddef(ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop
   end if


   dims_var(1) = xdim
   dims_var(2) = ydim


   if (wo .eq. 1) then
      write(*,*) 'new file created: ',trim(path)
   end if

end subroutine nc_create
