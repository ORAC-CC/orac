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
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/04, AP: Homogenisation of I/O modules.
! 2016/06/10, SP: Updates for bayesian selection without huge memory usage.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_primary_cost_only(ncid, input_data, indexing, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                    intent(in)    :: ncid
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   call nc_read_array(ncid, "costja", input_data%costja, verbose, startp = [1, sval])
   call nc_read_array(ncid, "costjm", input_data%costjm, verbose, startp = [1, sval])

end subroutine read_input_primary_cost_only


subroutine read_input_primary_common(ncid, input_data, indexing, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                    intent(in)    :: ncid
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer            :: i, j
   integer            :: ierr
   integer            :: varid
   character(len=32)  :: input_num
   character(len=512) :: input_dummy


if (indexing%flags%do_aerosol) then
   call nc_read_packed_array(ncid, "aot550", input_data%aot550, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "aot550_uncertainty", &
        input_data%aot550_uncertainty, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "aot870", input_data%aot870, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "aot870_uncertainty", &
        input_data%aot870_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "aer", input_data%aer, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "aer_uncertainty", &
        input_data%aer_uncertainty, verbose, startp = [1, sval])
end if

if (indexing%flags%do_rho) then
   do i=1,indexing%NSolar
      write(input_num, "(i4)") indexing%Y_Id(indexing%YSolar(i))

      do j=1,MaxRho_XX
         if (indexing%rho_terms(i,j)) then
            call create_rho_field_name(j, 1, input_num, input_dummy)
            call nc_read_packed_array(ncid, input_dummy, &
                 input_data%rho(:,:,i,j), verbose, startp = [1, sval, 1, 1])

            call create_rho_field_name(j, 2, input_num, input_dummy)
            call nc_read_packed_array(ncid, input_dummy, &
                 input_data%rho_uncertainty(:,:,i,j), verbose, startp = [1, sval, 1, 1])
         end if
      end do
   end do
end if

if (indexing%flags%do_swansea) then
   do i=1,indexing%NSolar
      write(input_num, "(i4)") indexing%Y_Id(indexing%YSolar(i))

      input_dummy='swansea_s_in_channel_no_'//trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%swansea_s(:,:,i), verbose, startp = [1, sval, 1])
      input_dummy='swansea_s_uncertainty_in_channel_no_'// &
           trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%swansea_s_uncertainty(:,:,i), verbose, startp = [1, sval, 1])

      input_dummy='diffuse_frac_in_channel_no_'//trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%diffuse_frac(:,:,i), verbose, startp = [1, sval, 1])
      input_dummy='diffuse_frac_uncertainty_in_channel_no_'// &
           trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%diffuse_frac_uncertainty(:,:,i), verbose, startp = [1, sval, 1])
   end do

   do i=1,indexing%NViews
      write(input_num, "(i4)") i

      input_dummy='swansea_p_in_view_no_'//trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%swansea_p(:,:,i), verbose, startp = [1, sval, 1])
      input_dummy='swansea_p_uncertainty_in_view_no_'// &
           trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%swansea_p_uncertainty(:,:,i), verbose, startp = [1, sval, 1])
   end do
end if

if (indexing%flags%do_cloud) then
   call nc_read_packed_array(ncid, "cot", input_data%cot, verbose)
   call nc_read_packed_array(ncid, "cot_uncertainty", &
        input_data%cot_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "cer", input_data%cer, verbose)
   call nc_read_packed_array(ncid, "cer_uncertainty", &
        input_data%cer_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "ctp", input_data%ctp, verbose)
   call nc_read_packed_array(ncid, "ctp_uncertainty", &
        input_data%ctp_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "ctp_corrected", &
        input_data%ctp_corrected, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "ctp_corrected_uncertainty", &
        input_data%ctp_corrected_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "cc_total", input_data%cc_total, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "cc_total_uncertainty", &
        input_data%cc_total_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "stemp", input_data%stemp, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "stemp_uncertainty", &
        input_data%stemp_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "cth", input_data%cth, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "cth_uncertainty", &
        input_data%cth_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "cth_corrected", &
        input_data%cth_corrected, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "cth_corrected_uncertainty", &
        input_data%cth_corrected_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "ctt", input_data%ctt, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "ctt_uncertainty", &
        input_data%ctt_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "ctt_corrected", &
        input_data%ctt_corrected, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "ctt_corrected_uncertainty", &
        input_data%ctt_corrected_uncertainty, verbose, startp = [1, sval])

   call nc_read_packed_array(ncid, "cwp", input_data%cwp, verbose, startp = [1, sval])
   call nc_read_packed_array(ncid, "cwp_uncertainty", &
        input_data%cwp_uncertainty, verbose, startp = [1, sval])

   do i=1,indexing%NSolar
      write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%cloud_albedo(:,:,i), verbose, startp = [1, sval, 1])

      input_dummy='cloud_albedo_uncertainty_in_channel_no_'// &
           trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%cloud_albedo_uncertainty(:,:,i), verbose, startp = [1, sval, 1])
   end do

   do i=1,indexing%NThermal
      write(input_num,"(i4)") indexing%Y_Id(indexing%YThermal(i))

      input_dummy='cee_in_channel_no_'//trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%cee(:,:,i), verbose, startp = [1, sval, 1])

      input_dummy='cee_uncertainty_in_channel_no_'//trim(adjustl(input_num))
      call nc_read_packed_array(ncid, input_dummy, &
           input_data%cee_uncertainty(:,:,i), verbose, startp = [1, sval, 1])
   end do
end if

   call nc_read_array(ncid, "convergence", input_data%convergence, verbose, startp = [1, sval])
   call nc_read_array(ncid, "niter", input_data%niter, verbose, startp = [1, sval])
   call nc_read_array(ncid, "costja", input_data%costja, verbose, startp = [1, sval])
   call nc_read_array(ncid, "costjm", input_data%costjm, verbose, startp = [1, sval])

   ierr = nf90_inq_varid(ncid, "qcflag", varid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: nc_read_file(): Could not locate variable ', trim("qcflag")
      print*,trim(nf90_strerror(ierr))
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_masks', input_data%qc_flag_masks)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
           ', variable: qcflag, name: flag_masks'
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_meanings', input_data%qc_flag_meanings)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
           ', variable: qcflag, name: flag_meanings'
      stop error_stop_code
   end if
   call nc_read_array(ncid, "qcflag", input_data%qcflag, verbose, startp = [1, sval])
   where(input_data%qcflag .eq. sint_fill_value) input_data%qcflag = -1

end subroutine read_input_primary_common


subroutine read_input_primary_optional(ncid, input_data, indexing, &
     read_flags, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                    intent(in)    :: ncid
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   type(common_file_flags_t),  intent(inout) :: read_flags
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer            :: i
   character(len=32)  :: input_num
   character(len=512) :: input_dummy

   do i=1,indexing%NViews
      if (indexing%read_optional_view_field(i)) then
         write(input_num,"(i1)") i

         input_dummy = "solar_zenith_view_no"//trim(adjustl(input_num))
         call nc_read_array(ncid, trim(adjustl(input_dummy)), &
              input_data%sol_zen(:,:,i), verbose, startp = [1, sval, 1])

         input_dummy ="satellite_zenith_view_no"//trim(adjustl(input_num))
         call nc_read_array(ncid, trim(adjustl(input_dummy)), &
              input_data%sat_zen(:,:,i), verbose, startp = [1, sval, 1])

         input_dummy ="rel_azimuth_view_no"//trim(adjustl(input_num))
         call nc_read_array(ncid, trim(adjustl(input_dummy)), &
              input_data%rel_azi(:,:,i), verbose, startp = [1, sval, 1])
      end if
   end do
   if (indexing%flags%do_cloud .and. read_flags%do_cloud) then
      call nc_read_packed_array(ncid, "cccot_pre", input_data%cccot_pre, verbose, startp = [1, sval, 1])
      read_flags%do_cloud = .false.
   end if

   if (indexing%flags%do_cldmask .and. read_flags%do_cldmask) then
      call nc_read_array(ncid, "cldmask", input_data%cldmask, verbose, startp = [1, sval, 1])
      read_flags%do_cldmask = .false.
   end if

   if (indexing%flags%do_cldmask_uncertainty .and. &
        read_flags%do_cldmask_uncertainty) then
      call nc_read_packed_array(ncid, "cldmask_uncertainty", &
           input_data%cldmask_uncertainty, verbose, startp = [1, sval, 1])
      read_flags%do_cldmask_uncertainty = .false.
   end if

   if (indexing%flags%do_phase .and. read_flags%do_phase) then
      call nc_read_array(ncid, "phase", input_data%phase, verbose, startp = [1, sval])
      read_flags%do_phase = .false.
   end if

   if (indexing%flags%do_phase_pavolonis .and. &
        read_flags%do_phase_pavolonis) then
      call nc_read_array(ncid, "phase_pavolonis", &
           input_data%phase_pavolonis, verbose, startp = [1, sval])
      read_flags%do_phase_pavolonis = .false.
   end if

end subroutine read_input_primary_optional


subroutine read_input_primary_once(nfile, fname, input_data, indexing, &
     loop_ind, global_atts, source_atts, sval, verbose)

   use global_attributes_m
   use orac_ncdf_m
   use source_attributes_m

   implicit none

   integer,                    intent(in)    :: nfile
   character(len=path_length), intent(in)    :: fname(:)
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   type(input_indices_t),      intent(in)    :: loop_ind(:)
   type(global_attributes_t),  intent(inout) :: global_atts
   type(source_attributes_t),  intent(inout) :: source_atts
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer                   :: ncid, i
   type(common_file_flags_t) :: read_flags

   ! Flag which optional fields to be read. Set false as they are read.
   read_flags = indexing%flags

   ! Read universally common fields from first input file
   call nc_open(ncid, fname(1))

   call nc_get_common_attributes(ncid, global_atts, source_atts)

   call nc_read_array(ncid, "time", input_data%time, verbose, startp = [1, sval])

   call nc_read_array(ncid, "lon", input_data%lon, verbose, startp = [1, sval])
   call nc_read_array(ncid, "lat", input_data%lat, verbose, startp = [1, sval])

   call nc_read_array(ncid, "lsflag", input_data%lsflag, verbose, startp = [1, sval])
   call nc_read_array(ncid, "lusflag", input_data%lusflag, verbose, startp = [1, sval])
   call nc_read_array(ncid, "dem", input_data%dem, verbose, startp = [1, sval])
   call nc_read_array(ncid, "nisemask", input_data%nisemask, verbose, startp = [1, sval])

   call nc_read_array(ncid, "illum", input_data%illum, verbose, startp = [1, sval])
   call nc_read_array(ncid, "cldtype", input_data%cldtype, verbose, startp = [1, sval, 1])

   call read_input_primary_optional(ncid, input_data, loop_ind(1), &
        read_flags, sval, verbose)

   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

   do i=2,nfile
      call nc_open(ncid,fname(i))
      call read_input_primary_optional(ncid, input_data, loop_ind(i), &
           read_flags, sval, verbose)
      if (nf90_close(ncid) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_close()'
         stop error_stop_code
      end if
   end do

end subroutine read_input_primary_once


subroutine read_input_primary_class(fname, input_data, indexing, costonly, sval, verbose)

   use orac_ncdf_m

   implicit none

   character(len=path_length), intent(in)    :: fname
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   logical,                    intent(in)    :: costonly
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer :: ncid

   if (verbose) write(*,*) 'Opening primary input file: ', trim(fname)
   call nc_open(ncid,fname)
   if (.not. costonly) then
      call read_input_primary_common(ncid, input_data, indexing, sval, verbose)
   else
      call read_input_primary_cost_only(ncid, input_data, indexing, sval, verbose)
   end if

   if (verbose) write(*,*) 'Closing primary input file.'
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_input_primary_class
