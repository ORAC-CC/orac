!-------------------------------------------------------------------------------
! Name: read_input_primary.F90
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for
! different attribute/variable type combinations.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/20, CP: Creates initial file.
! 2012/03/18, CP: Modified to add cloud flag
! 2012/07/06, MJ: Extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/06/04, MJ: Changes routine names to "*_pp" to avoid confusion when
!    building libraries.
! 2014/10/24, OS: Added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
! 2014/11/26, CP: Added cloud_albedo
!    (currently deactivated), and nisemask; commented out reading of variables
!    for water within if condition iphase = 2 (never true for water)
! 2014/12/02, CP: Reads in global and source attributes from file
! 2015/01/26, CP: Add in ml from IR only option changed to common constants
! 2015/07/06, OS: Changed some nf90_attributes to their naming convention in
!    common/orac_ncdf.F90; changed cct to cc_total
! 2015/07/16, GM: Major cleanup and made use of the NetCDF interface in the
!    common library.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2016/01/27, GM: Add cee and cee_uncertainty.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_primary_common(ncid, input_data, xdim, ydim, indexing, &
                                     global_atts, verbose)

   use global_attributes
   use netcdf
   use orac_ncdf
   use postproc_constants

   implicit none

   integer,                   intent(in)    :: ncid
   type(input_data_primary),  intent(inout) :: input_data
   integer(kind=lint),        intent(in)    :: xdim,ydim
   type(counts_and_indexes),  intent(in)    :: indexing
   type(global_attributes_s), intent(in)    :: global_atts
   logical,                   intent(in)    :: verbose

   integer            :: i, ii
   integer            :: ierr
   integer            :: varid
   character(len=32)  :: input_num
   character(len=512) :: input_dummy

   call nc_read_packed_array(ncid, "cot", input_data%cot, verbose)
   call nc_read_packed_array(ncid, "cot_uncertainty", input_data%cot_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cer", input_data%cer, verbose)
   call nc_read_packed_array(ncid, "cer_uncertainty", input_data%cer_uncertainty, verbose)

   call nc_read_packed_array(ncid, "ctp", input_data%ctp, verbose)
   call nc_read_packed_array(ncid, "ctp_uncertainty", input_data%ctp_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cc_total", input_data%cc_total, verbose)
   call nc_read_packed_array(ncid, "cc_total_uncertainty", input_data%cc_total_uncertainty, verbose)

   call nc_read_packed_array(ncid, "stemp", input_data%stemp, verbose)
   call nc_read_packed_array(ncid, "stemp_uncertainty", input_data%stemp_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cth", input_data%cth, verbose)
   call nc_read_packed_array(ncid, "cth_uncertainty", input_data%cth_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cth_corrected", input_data%cth_corrected, verbose)
   call nc_read_packed_array(ncid, "cth_corrected_uncertainty", input_data%cth_corrected_uncertainty, verbose)

   call nc_read_packed_array(ncid, "ctt", input_data%ctt, verbose)
   call nc_read_packed_array(ncid, "ctt_uncertainty", input_data%ctt_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cwp", input_data%cwp, verbose)
   call nc_read_packed_array(ncid, "cwp_uncertainty", input_data%cwp_uncertainty, verbose)

   ii = 1
   do i=1,indexing%Ny
      if (btest(indexing%Ch_Is(i), SolarBit)) then
         write(input_num,"(i4)") indexing%Y_Id(i)
         input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
         call nc_read_packed_array(ncid, input_dummy, input_data%cloud_albedo(:,:,ii), verbose)

         write(input_num,"(i4)") indexing%Y_Id(i)
         input_dummy='cloud_albedo_uncertainty_in_channel_no_'//trim(adjustl(input_num))
         call nc_read_packed_array(ncid, input_dummy, input_data%cloud_albedo_uncertainty(:,:,ii), verbose)

         ii = ii + 1
      end if
   end do

   ii = 1
   do i=1,indexing%Ny
      if (btest(indexing%Ch_Is(i), ThermalBit)) then
         write(input_num,"(i4)") indexing%Y_Id(i)
         input_dummy='cee_in_channel_no_'//trim(adjustl(input_num))
         call nc_read_packed_array(ncid, input_dummy, input_data%cee(:,:,ii), verbose)

         write(input_num,"(i4)") indexing%Y_Id(i)
         input_dummy='cee_uncertainty_in_channel_no_'//trim(adjustl(input_num))
         call nc_read_packed_array(ncid, input_dummy, input_data%cee_uncertainty(:,:,ii), verbose)

         ii = ii + 1
      end if
   end do

   call nc_read_array(ncid, "convergence", input_data%convergence, verbose)
   call nc_read_array(ncid, "niter", input_data%niter, verbose)
   call nc_read_array(ncid, "costja", input_data%costja, verbose)
   call nc_read_array(ncid, "costjm", input_data%costjm, verbose)

   ierr = nf90_inq_varid(ncid, "qcflag", varid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: nc_read_file(): Could not locate variable ', trim("qcflag")
      print*,trim(nc_error(ierr))
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_masks', input_data%qc_flag_masks)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nc_error(ierr)), &
          ', variable: qcflag, name: flag_masks'
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_meanings', input_data%qc_flag_meanings)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nc_error(ierr)), &
          ', variable: qcflag, name: flag_meanings'
      stop error_stop_code
   end if
   call nc_read_array(ncid, "qcflag", input_data%qcflag, verbose)
   where(input_data%qcflag .eq. sint_fill_value) input_data%qcflag = -1

end subroutine read_input_primary_common


subroutine read_input_primary_all(fname, input_data, xdim, ydim, indexing, &
                                  global_atts, source_atts, verbose)

   use global_attributes
   use netcdf
   use orac_ncdf
   use source_attributes
   use postproc_constants

   implicit none

   character(len=path_length), intent(in)    :: fname
   type(input_data_primary),   intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim,ydim
   type(counts_and_indexes),   intent(in)    :: indexing
   type(global_attributes_s),  intent(inout) :: global_atts
   type(source_attributes_s),  intent(inout) :: source_atts
   logical,                    intent(in)    :: verbose

   integer :: ncid

   write(*,*) 'Opening primary input file: ', trim(fname)
   call nc_open(ncid,fname)

   call nc_get_common_attributes(ncid, global_atts, source_atts)

   call read_input_primary_common(ncid, input_data, xdim, ydim, indexing, &
      global_atts, verbose)

   call nc_read_array(ncid, "time", input_data%time, verbose)

   call nc_read_array(ncid, "lon", input_data%lon, verbose)
   call nc_read_array(ncid, "lat", input_data%lat, verbose)

   call nc_read_array(ncid, "solar_zenith_view_no1", input_data%solar_zenith_view_no1, verbose)
   call nc_read_array(ncid, "satellite_zenith_view_no1", input_data%satellite_zenith_view_no1, verbose)
   call nc_read_array(ncid, "rel_azimuth_view_no1", input_data%rel_azimuth_view_no1, verbose)

   call nc_read_array(ncid, "lsflag", input_data%lsflag, verbose)
   call nc_read_array(ncid, "lusflag", input_data%lusflag, verbose)
   call nc_read_array(ncid, "dem", input_data%dem, verbose)
   call nc_read_array(ncid, "nisemask", input_data%nisemask, verbose)

   call nc_read_array(ncid, "illum", input_data%illum, verbose)
   call nc_read_array(ncid, "cldtype", input_data%cldtype, verbose)
   call nc_read_array(ncid, "cldmask", input_data%cldmask, verbose)
   call nc_read_packed_array(ncid, "cldmask_uncertainty", input_data%cldmask_uncertainty, verbose)
   call nc_read_packed_array(ncid, "cccot_pre", input_data%cccot_pre, verbose)

   write(*,*) 'Closing primary input file.'
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

!  call nc_read_array(ncid, "phase", input_data%phase, verbose)

end subroutine read_input_primary_all


subroutine read_input_primary_class(fname, input_data, xdim, ydim, indexing, &
                                    global_atts, verbose)

   use global_attributes
   use netcdf
   use orac_ncdf
   use postproc_constants

   implicit none

   character(len=path_length), intent(in)    :: fname
   type(input_data_primary),   intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim,ydim
   type(counts_and_indexes),   intent(in)    :: indexing
   type(global_attributes_s),  intent(in)    :: global_atts
   logical,                    intent(in)    :: verbose

   integer :: ncid

   call nc_open(ncid,fname)

   call read_input_primary_common(ncid, input_data, xdim, ydim, indexing, &
      global_atts, verbose)

   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_input_primary_class
