!-------------------------------------------------------------------------------
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid           integer in   ID number for open NCDF file.
! global_atts    struct  in   Structure detailing NCDF header contents.
!
! History:
! 2014/02/03, GM: Original version
! 2014/08/31, GM: Make the global attribute list consistent with CF-1.4.
! 2014/08/31, GM: Moved to the orac common tree.
! 2014/09/01, GM: Generalize for use in both the main and preprocessors.
!
!-------------------------------------------------------------------------------

subroutine nc_put_common_attributes(ncid, global_atts)

   use netcdf

   use global_attributes

   implicit none

   integer,                   intent(in) :: ncid
   type(global_attributes_s), intent(in) :: global_atts

   integer :: ierr


   !----------------------------------------------------------------------------
   ! Global attribute 'Conventions' as defined by CF-1.4, section 2.6.1.
   !----------------------------------------------------------------------------
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', &
        trim(global_atts%Conventions))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Conventions'
      stop error_stop_code
   endif


   !----------------------------------------------------------------------------
   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   !----------------------------------------------------------------------------
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'title', &
        trim(global_atts%title))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: title'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'institution', &
        trim(global_atts%institution))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: institution'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'source', &
        trim(global_atts%source))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: source'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'history', &
        trim(global_atts%history))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: history'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'references', &
        trim(global_atts%references))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: references'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'comment', &
        trim(global_atts%comment))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: comment'
      stop error_stop_code
   endif


   !----------------------------------------------------------------------------
   ! Extra global attributes defined by Orac
   !----------------------------------------------------------------------------
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Project', &
        trim(global_atts%Project))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Project'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'File_Name', &
        trim(global_atts%File_Name))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: File_Name'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'UUID', &
        trim(global_atts%UUID))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: UUID'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'NetCDF_Version', &
        trim(global_atts%NetCDF_Version))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: NetCDF_Version'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Product_Name', &
        trim(global_atts%Product_Name))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Product_Name'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Product_Date', &
        trim(global_atts%Product_Date))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Product_Date'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Production_Time', &
        trim(global_atts%Production_Time))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Production_Time'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'L2_Processor', &
        trim(global_atts%L2_Processor))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: L2_Processor'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'L2_Processor_Version', &
        trim(global_atts%L2_Processor_Version))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: L2_Processor_Version'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Platform', &
        trim(global_atts%Platform))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Platform'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Sensor', &
        trim(global_atts%Sensor))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Sensor'
      stop error_stop_code
   endif

   if (global_atts%AATSR_Processing_Version .ne. ' ') then
      ierr = nf90_put_att(ncid, NF90_GLOBAL, 'AATSR_Processing_Version', &
           trim(global_atts%AATSR_Processing_Version))
      if (ierr.ne.NF90_NOERR) then
         write(*,*), 'ERROR: nf90_put_att(), name: AATSR_Processing_Version'
         stop error_stop_code
      endif
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Contact_Email', &
        trim(global_atts%Contact_Email))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Contact_Email'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Contact_Website', &
        trim(global_atts%Contact_Website))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Contact_Website'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Keywords', &
        trim(global_atts%Keywords))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Keywords'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Summary', &
        trim(global_atts%Summary))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: Summary'
      stop error_stop_code
   endif

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'License', &
        trim(global_atts%License))
   if (ierr.ne.NF90_NOERR) then
      write(*,*), 'ERROR: nf90_put_att(), name: License'
      stop error_stop_code
   endif

end subroutine nc_put_common_attributes
