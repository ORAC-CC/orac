!-------------------------------------------------------------------------------
! Name: orac_nc_utils.F90
!
! Purpose:
! File with ORAC specific NetCDF i/o convience routines not appropriate for the
! more general nc_* files.
!
! $Id$
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: nc_create
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
! 2015/07/16, GM: Original version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_create(path, ncid, nx, ny, dims_var, type, global_atts, source_atts)

   use netcdf

   use common_constants
   use global_attributes
   use source_attributes

   implicit none

   ! Input
   character(len=*),          intent(in)    :: path
   integer,                   intent(in)    :: nx, ny
   integer,                   intent(in)    :: type

   ! Output
   integer,                   intent(out)   :: ncid
   integer,                   intent(out)   :: dims_var(2)

   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts

   ! Local
   integer :: ierr


   !----------------------------------------------------------------------------
   ! Create new file
   !----------------------------------------------------------------------------
   ierr = nf90_create(path, IOR(IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL), &
                      NF90_CLOBBER), ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_create(): filename = ', trim(path)
      stop error_stop_code
   end if

   ! Define the 2 dimensions: lat / lon
   ierr = nf90_def_dim(ncid, 'across_track', nx, dims_var(1))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(): dim_name = across_track, xdim = ', dims_var(1)
      stop error_stop_code
   end if

   ierr = nf90_def_dim(ncid, 'along_track', ny, dims_var(2))
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_def_dim(): dim_name = along_track,  ydim = ', dims_var(2)
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Write global attributes to the netcdf output
   !----------------------------------------------------------------------------
   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   if (type .eq. 1) then
      global_atts%title = 'ESA Cloud CCI Retrieval Products L2 Primary File'
   else if (type .eq. 2) then
      global_atts%title = 'ESA Cloud CCI Retrieval Products L2 Secondary File'
   else
      write(*,*) 'ERROR: nf90_create(): invalid file type: ', type
      stop error_stop_code
   end if

   call nc_put_common_attributes(ncid, global_atts,source_atts)


   !----------------------------------------------------------------------------
   !
   !----------------------------------------------------------------------------
   ierr = nf90_enddef(ncid)
   if (ierr .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_enddef()'
      stop error_stop_code
   end if

end subroutine nc_create


!-------------------------------------------------------------------------------
! Name: nc_put_common_attributes
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid           integer in   ID number for open NCDF file.
! global_atts    struct  both Structure of the common attributes.
! source_atts    struct  both Structure of the global attributes.
!
! History:
! 2014/02/03, GM: Original version
! 2014/08/31, GM: Make the global attribute list consistent with CF-1.4.
! 2014/08/31, GM: Moved to the orac common tree.
! 2014/09/01, GM: Generalize for use in both the main and preprocessors.
! 2014/12/01, CP: updated to include source attributes

! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_put_common_attributes(ncid, global_atts, source_atts)

   use netcdf

   use global_attributes
   use source_attributes

   implicit none

   integer,                   intent(in) :: ncid
   type(global_attributes_s), intent(in) :: global_atts
   type(source_attributes_s), intent(in) :: source_atts

   integer :: ierr


   !----------------------------------------------------------------------------
   ! Global attribute 'Conventions' as defined by CF-1.4, section 2.6.1.
   !----------------------------------------------------------------------------
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', &
        trim(global_atts%Conventions))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
         ', name: Conventions'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   !----------------------------------------------------------------------------
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'title', &
        trim(global_atts%title))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: title'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'institution', &
        trim(global_atts%institution))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: institution'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'source', &
        trim(global_atts%source))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: source'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'history', &
        trim(global_atts%history))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: history'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'references', &
        trim(global_atts%references))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: references'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'comment', &
        trim(global_atts%comment))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: comment'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Extra global attributes defined by Orac
   !----------------------------------------------------------------------------
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Project', &
        trim(global_atts%Project))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Project'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'File_Name', &
        trim(global_atts%File_Name))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: File_Name'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'UUID', &
        trim(global_atts%UUID))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: UUID'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'NetCDF_Version', &
        trim(global_atts%NetCDF_Version))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: NetCDF_Version'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Product_Name', &
        trim(global_atts%Product_Name))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Product_Name'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Date_Created', &
        trim(global_atts%Date_Created))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Date_Created'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Production_Time', &
        trim(global_atts%Production_Time))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Production_Time'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'L2_Processor', &
        trim(global_atts%L2_Processor))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: L2_Processor'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'L2_Processor_Version', &
        trim(global_atts%L2_Processor_Version))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: L2_Processor_Version'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Platform', &
        trim(global_atts%Platform))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Platform'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Sensor', &
        trim(global_atts%Sensor))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Sensor'
      stop error_stop_code
   end if

   if (global_atts%AATSR_Processing_Version .ne. ' ') then
      ierr = nf90_put_att(ncid, NF90_GLOBAL, 'AATSR_Processing_Version', &
           trim(global_atts%AATSR_Processing_Version))
      if (ierr.ne.NF90_NOERR) then
         write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: AATSR_Processing_Version'
         stop error_stop_code
      end if
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Creator_Email', &
        trim(global_atts%Creator_Email))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Creator_Email'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Creator_url', &
        trim(global_atts%Creator_url))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Creator_url'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Keywords', &
        trim(global_atts%Keywords))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Keywords'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Summary', &
        trim(global_atts%Summary))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Summary'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'License', &
        trim(global_atts%License))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: License'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'SVN_Version', &
        trim(global_atts%SVN_Version))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: SVN_Version'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'ECMWF_Version', &
        trim(global_atts%ECMWF_Version))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: ECMWF_Version'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'RTTOV_Version', &
        trim(global_atts%RTTOV_Version))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: RTTOV_Version'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Source attributes
   !----------------------------------------------------------------------------
   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Albedo_File', &
        trim(source_atts%albedo_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: albedo_file'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'BRDF_File', &
        trim(source_atts%brdf_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: brdf_file'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Emissivity_File', &
        trim(source_atts%emissivity_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: emissivity_file'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'USGS_File', &
        trim(source_atts%usgs_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: usgs_file'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Snow_File', &
        trim(source_atts%snow_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: snow_file'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Sea_Ice_File', &
        trim(source_atts%sea_ice_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: sea_ice_file'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Level1b_File', &
        trim(source_atts%level1b_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Level1b_file'
      stop error_stop_code
   end if

   ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Geo_File', &
        trim(source_atts%geo_file))
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_put_att(), ', trim(nf90_strerror(ierr)), &
          ', name: geo_file'
      stop error_stop_code
   end if

end subroutine nc_put_common_attributes


!-------------------------------------------------------------------------------
! Name: nc_get_common_attributes
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid           integer in   ID number for open NCDF file.
! global_atts    struct  both Structure of the common attributes.
! source_atts    struct  both Structure of the global attributes.
!
! History:
! 2014/12/16, GM: Original version

! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine nc_get_common_attributes(ncid, global_atts, source_atts)

   use netcdf

   use global_attributes
   use source_attributes

   implicit none

   integer,                   intent(in)    :: ncid
   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts

   integer :: ierr


   !----------------------------------------------------------------------------
   ! Global attribute 'Conventions' as defined by CF-1.4, section 2.6.1.
   !----------------------------------------------------------------------------
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Conventions', &
        global_atts%Conventions)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
         ', name: Conventions'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Global attributes for the 'Description of file contents' as defined by
   ! CF-1.4, section 2.6.2.
   !----------------------------------------------------------------------------
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'title', &
        global_atts%title)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: title'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'institution', &
        global_atts%institution)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: institution'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'source', &
        global_atts%source)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: source'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'history', &
        global_atts%history)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: history'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'references', &
        global_atts%references)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: references'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'comment', &
        global_atts%comment)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: comment'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Extra global attributes defined by Orac
   !----------------------------------------------------------------------------
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Project', &
        global_atts%Project)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Project'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'File_Name', &
        global_atts%File_Name)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: File_Name'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'UUID', &
        global_atts%UUID)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: UUID'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'NetCDF_Version', &
        global_atts%NetCDF_Version)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: NetCDF_Version'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Product_Name', &
        global_atts%Product_Name)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Product_Name'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Date_Created', &
        global_atts%Date_Created)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Date_Created'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Production_Time', &
        global_atts%Production_Time)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Production_Time'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'L2_Processor', &
        global_atts%L2_Processor)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: L2_Processor'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'L2_Processor_Version', &
        global_atts%L2_Processor_Version)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: L2_Processor_Version'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Platform', &
        global_atts%Platform)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Platform'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Sensor', &
        global_atts%Sensor)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Sensor'
      stop error_stop_code
   end if

   global_atts%AATSR_Processing_Version = ' '
!  if (global_atts%AATSR_Processing_Version .ne. ' ') then
      ierr = nf90_get_att(ncid, NF90_GLOBAL, 'AATSR_Processing_Version', &
           global_atts%AATSR_Processing_Version)
!     if (ierr.ne.NF90_NOERR) then
!        write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
!         ', name: AATSR_Processing_Version'
!        stop error_stop_code
!     end if
!  end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Creator_Email', &
        global_atts%Creator_Email)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Creator_Email'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Creator_url', &
        global_atts%Creator_url)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Creator_url'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Keywords', &
        global_atts%Keywords)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Keywords'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Summary', &
        global_atts%Summary)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Summary'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'License', &
        global_atts%License)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: License'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'SVN_Version', &
        global_atts%SVN_Version)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: SVN_Version'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'ECMWF_Version', &
        global_atts%ECMWF_Version)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: ECMWF_Version'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'RTTOV_Version', &
        global_atts%RTTOV_Version)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: RTTOV_Version'
      stop error_stop_code
   end if


   !----------------------------------------------------------------------------
   ! Source attributes
   !----------------------------------------------------------------------------
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Albedo_File', &
        source_atts%albedo_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: albedo_file'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'BRDF_File', &
        source_atts%brdf_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: brdf_file'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Emissivity_File', &
        source_atts%emissivity_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: emissivity_file'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'USGS_File', &
        source_atts%usgs_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: usgs_file'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Snow_File', &
        source_atts%snow_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: snow_file'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Sea_Ice_File', &
        source_atts%sea_ice_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: sea_ice_file'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Level1b_File', &
        source_atts%level1b_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: Level1b_file'
      stop error_stop_code
   end if

   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'Geo_File', &
        source_atts%geo_file)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
          ', name: geo_file'
      stop error_stop_code
   end if

end subroutine nc_get_common_attributes


!-------------------------------------------------------------------------------
! Name: prepare_<type>_packed_<type>
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
!
! History:
! 2014/12/16, GM: Original version
! 2016/01/02, AP: Round values rather than simply casting.
! 2016/03/04, AP: Tidy prepare_*_packed_float.
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine prepare_short_packed_float(value_in, value_out, scale_factor, &
                                      add_offset, valid_min, valid_max, &
                                      fill_value_in, bound_max_value, control)

   implicit none

   real(kind=sreal),   intent(in)  :: value_in
   integer(kind=sint), intent(out) :: value_out
   real(kind=sreal),   intent(in)  :: scale_factor
   real(kind=sreal),   intent(in)  :: add_offset
   integer(kind=sint), intent(in)  :: valid_min
   integer(kind=sint), intent(in)  :: valid_max
   real(kind=sreal),   intent(in)  :: fill_value_in
   integer(kind=sint), intent(in)  :: bound_max_value
   real(kind=sreal),   intent(in), optional :: control

   real(kind=sreal)                :: temp, value_compare

   if (present(control)) then
      value_compare = control
   else
      value_compare = value_in
   end if

   if (value_compare .ne. fill_value_in) then
      temp = (value_in - add_offset) / scale_factor

      if (temp .lt. real(valid_min,kind=sreal)) then
         value_out=sint_fill_value
      else if (temp .gt. real(valid_max,kind=sreal)) then
         value_out=bound_max_value
      else
         value_out=nint(temp, kind=sint)
      end if
   else
      value_out=sint_fill_value
   end if

end subroutine prepare_short_packed_float


subroutine prepare_float_packed_float(value_in, value_out, scale_factor, &
                                      add_offset, valid_min, valid_max, &
                                      fill_value_in, bound_max_value)

   implicit none

   real(kind=sreal), intent(in)  :: value_in
   real(kind=sreal), intent(out) :: value_out
   real(kind=sreal), intent(in)  :: scale_factor
   real(kind=sreal), intent(in)  :: add_offset
   real(kind=sreal), intent(in)  :: valid_min
   real(kind=sreal), intent(in)  :: valid_max
   real(kind=sreal), intent(in)  :: fill_value_in
   real(kind=sreal), intent(in)  :: bound_max_value

   real(kind=sreal)              :: temp

   if (value_in .ne. fill_value_in) then
      temp = (value_in - add_offset) / scale_factor

      if (temp .lt. real(valid_min,kind=sreal)) then
         value_out=sreal_fill_value
      else if (temp .gt. real(valid_max,kind=sreal)) then
         value_out=bound_max_value
      else
         value_out=temp
      end if
   else
      value_out=sreal_fill_value
   end if

end subroutine prepare_float_packed_float
