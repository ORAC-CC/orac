!-------------------------------------------------------------------------------
! Name: netcdf_output_check.F90
!
! Purpose:
! Check whether netcdf output files are corrupt
!
! Description and Algorithm details:
! 1) Call NC_OPEN/NF90_CLOSE several times.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
!
! History:
! 2015/05/05, OS: Initial version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_check(output_path, lwrtm_file, swrtm_file, prtm_file, &
   config_file, msi_file, cf_file, lsf_file, geo_file, loc_file, alb_file, corrupt, &
   verbose)

   use netcdf, only: nf90_close, nf90_open, NF90_NOERR, NF90_NOWRITE
   use preproc_constants_m

   implicit none

   character(len=*), intent(in)    :: output_path
   character(len=*), intent(in)    :: lwrtm_file, swrtm_file, &
                                      prtm_file, config_file, &
                                      msi_file, cf_file, lsf_file, &
                                      geo_file, loc_file, alb_file
   logical,          intent(inout) :: corrupt
   logical,          intent(in)    :: verbose
   integer                         :: ncid

   if (verbose) write(*,*) 'Albedo file: ', trim(alb_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(alb_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".alb.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".alb.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'Cloud flag file: ', trim(cf_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(cf_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".clf.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".clf.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'Config file: ', trim(config_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(config_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".config.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".config.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'Geometry file: ', trim(geo_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(geo_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".geo.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".geo.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'Location file: ', trim(loc_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(loc_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".loc.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".loc.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'Land/sea file: ', trim(lsf_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(lsf_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".lsf.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".lsf.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'LwRTM file: ', trim(lwrtm_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(lwrtm_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".lwrtm.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".lwrtm.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'Imagery file: ', trim(msi_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(msi_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".msi.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".msi.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'Prtm file: ', trim(prtm_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(prtm_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".prtm.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".prtm.nc"'
         stop error_stop_code
      end if
   end if

   if (verbose) write(*,*) 'SwRTM file: ', trim(swrtm_file)
   if (nf90_open(path=trim(adjustl(output_path))//'/'//trim(adjustl(swrtm_file)), &
                 mode=NF90_NOWRITE, ncid=ncid) .ne. NF90_NOERR) then
      write (*,*) 'ERROR: netcdf_output_check(): nf90_open(): ".swrtm.nc"'
      corrupt = .true.
      return
   else
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write (*,*) 'ERROR: netcdf_create_config(): nf90_close(): ".swrtm.nc"'
         stop error_stop_code
      end if
   end if

 end subroutine netcdf_output_check
