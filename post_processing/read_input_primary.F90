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
! 2017/01/02, SP: Updates for multi layer cloud
! 2017/01/09, CP: ML additions.
! 2017/04/28, CP: ML bug fix added ctp2
! 2017/06/22, OS: Added phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
! 2021/11/21, GT: Added read_input_primary_classify subroutine
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_input_primary_cost_only(ncid, input_data, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                    intent(in)    :: ncid
   type(input_data_primary_t), intent(inout) :: input_data
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   call ncdf_read_array(ncid, "costja", input_data%costja, start = [1, sval])
   call ncdf_read_array(ncid, "costjm", input_data%costjm, start = [1, sval])

end subroutine read_input_primary_cost_only


subroutine read_input_primary_common(ncid, input_data, indexing, sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                    intent(in)    :: ncid
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer            :: i, j, i_rho
   character(len=32)  :: input_num
   character(len=512) :: input_dummy


if (indexing%flags%do_aerosol) then
   call ncdf_read_packed_array(ncid, "aot550", input_data%aot550, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "aot550_uncertainty", &
        input_data%aot550_uncertainty, start = [1, sval])
   call ncdf_read_packed_array(ncid, "aot870", input_data%aot870,  &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "aot870_uncertainty", &
        input_data%aot870_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "aer", input_data%aer, start = [1, sval])
   call ncdf_read_packed_array(ncid, "aer_uncertainty", &
        input_data%aer_uncertainty, start = [1, sval])
end if

if (indexing%flags%do_rho) then
   i_rho = 0
   do i=1,indexing%NSolar
      write(input_num, "(i4)") indexing%Y_Id(indexing%YSolar(i))

      do j=1,MaxRho_XX
         if (indexing%rho_terms(i,j)) then
            i_rho = i_rho + 1

            call create_rho_field_name(j, 1, input_num, input_dummy)
            call ncdf_read_packed_array(ncid, input_dummy, &
                 input_data%rho(:,:,i_rho), start = [1, sval])

            call create_rho_field_name(j, 2, input_num, input_dummy)
            call ncdf_read_packed_array(ncid, input_dummy, &
                 input_data%rho_uncertainty(:,:,i_rho), start = [1, sval])
         end if
      end do
   end do
end if

if (indexing%flags%do_swansea) then
   i_rho = 0
   do i=1,indexing%NSolar
      if (indexing%ss_terms(i)) then
         i_rho = i_rho + 1

         write(input_num, "(i4)") indexing%Y_Id(indexing%YSolar(i))

         input_dummy='swansea_s_in_channel_no_'//trim(adjustl(input_num))
         call ncdf_read_packed_array(ncid, input_dummy, &
              input_data%swansea_s(:,:,i_rho), start = [1, sval])
         input_dummy='swansea_s_uncertainty_in_channel_no_'// &
              trim(adjustl(input_num))
         call ncdf_read_packed_array(ncid, input_dummy, &
              input_data%swansea_s_uncertainty(:,:,i_rho), &
              start = [1, sval])

         input_dummy='diffuse_frac_in_channel_no_'//trim(adjustl(input_num))
         call ncdf_read_packed_array(ncid, input_dummy, &
              input_data%diffuse_frac(:,:,i_rho), start = [1, sval])
         input_dummy='diffuse_frac_uncertainty_in_channel_no_'// &
              trim(adjustl(input_num))
         call ncdf_read_packed_array(ncid, input_dummy, &
              input_data%diffuse_frac_uncertainty(:,:,i_rho), &
              start = [1, sval])
      end if
   end do

   do i=1,indexing%NViews
      write(input_num, "(i4)") i

      input_dummy='swansea_p_in_view_no_'//trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, input_dummy, &
           input_data%swansea_p(:,:,i), start = [1, sval])
      input_dummy='swansea_p_uncertainty_in_view_no_'// &
           trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, input_dummy, &
           input_data%swansea_p_uncertainty(:,:,i), start = [1, sval])
   end do
end if

if (indexing%flags%do_cloud) then
   call ncdf_read_packed_array(ncid, "cot", input_data%cot, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cot_uncertainty", &
        input_data%cot_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cer", input_data%cer, start = [1, sval])
   call ncdf_read_packed_array(ncid, "cer_uncertainty", &
        input_data%cer_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "ctp", input_data%ctp, start = [1, sval])
   call ncdf_read_packed_array(ncid, "ctp_uncertainty", &
        input_data%ctp_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "ctp_corrected", &
        input_data%ctp_corrected, start = [1, sval])
   call ncdf_read_packed_array(ncid, "ctp_corrected_uncertainty", &
        input_data%ctp_corrected_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cc_total", input_data%cc_total, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cc_total_uncertainty", &
        input_data%cc_total_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "stemp", input_data%stemp, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "stemp_uncertainty", &
        input_data%stemp_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cth", input_data%cth, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cth_uncertainty", &
        input_data%cth_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cth_corrected", &
        input_data%cth_corrected, start = [1, sval])
   call ncdf_read_packed_array(ncid, "cth_corrected_uncertainty", &
        input_data%cth_corrected_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "ctt", input_data%ctt, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "ctt_uncertainty", &
        input_data%ctt_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "ctt_corrected", &
        input_data%ctt_corrected, start = [1, sval])
   call ncdf_read_packed_array(ncid, "ctt_corrected_uncertainty", &
        input_data%ctt_corrected_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cwp", input_data%cwp, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cwp_uncertainty", &
        input_data%cwp_uncertainty, start = [1, sval])

   do i=1,indexing%NSolar
      write(input_num,"(i4)") indexing%Y_Id(indexing%YSolar(i))

      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, input_dummy, &
           input_data%cloud_albedo(:,:,i), start = [1, sval])

      input_dummy='cloud_albedo_uncertainty_in_channel_no_'// &
           trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, input_dummy, &
           input_data%cloud_albedo_uncertainty(:,:,i), &
           start = [1, sval])
   end do

   do i=1,indexing%NThermal
      write(input_num,"(i4)") indexing%Y_Id(indexing%YThermal(i))

      input_dummy='cee_in_channel_no_'//trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, input_dummy, &
           input_data%cee(:,:,i), start = [1, sval])

      input_dummy='cee_uncertainty_in_channel_no_'//trim(adjustl(input_num))
      call ncdf_read_packed_array(ncid, input_dummy, &
           input_data%cee_uncertainty(:,:,i), start = [1, sval])
   end do
end if

if (indexing%flags%do_cloud_layer_2) then
   call ncdf_read_packed_array(ncid, "cot2", input_data%cot2, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cot2_uncertainty", &
        input_data%cot2_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cer2", input_data%cer2, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cer2_uncertainty", &
        input_data%cer2_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cth2", input_data%cth2, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cth2_uncertainty", &
        input_data%cth2_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "ctp2", input_data%ctp2, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "ctp2_uncertainty", &
        input_data%ctp2_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "ctt2", input_data%ctt2, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "ctt2_uncertainty", &
        input_data%ctt2_uncertainty, start = [1, sval])

   call ncdf_read_packed_array(ncid, "cwp2", input_data%cwp2, &
        start = [1, sval])
   call ncdf_read_packed_array(ncid, "cwp2_uncertainty", &
        input_data%cwp2_uncertainty, start = [1, sval])
end if

   call ncdf_read_array(ncid, "niter", input_data%niter, &
        start = [1, sval])
   call ncdf_read_array(ncid, "costja", input_data%costja, &
        start = [1, sval])
   call ncdf_read_array(ncid, "costjm", input_data%costjm, &
        start = [1, sval])

   call ncdf_read_array(ncid, "qcflag", input_data%qcflag, &
        start = [1, sval])
   where(input_data%qcflag .eq. sint_fill_value) input_data%qcflag = -1

   call ncdf_read_array(ncid, "channels_used", input_data%channels_used, &
        start = [1, sval])
   call ncdf_read_array(ncid, "variables_retrieved", &
        input_data%variables_retrieved, start = [1, sval])

end subroutine read_input_primary_common


subroutine read_input_primary_optional(ncid, input_data, indexing, read_flags, &
     sval, verbose)

   use orac_ncdf_m

   implicit none

   integer,                    intent(in)    :: ncid
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   type(common_file_flags_t),  intent(inout) :: read_flags
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer            :: i, ii
   character(len=32)  :: input_num
   character(len=512) :: input_dummy

   do i=1,indexing%NViews
      if (indexing%read_optional_view_field(i)) then
         write(input_num,"(i1)") i
         ii = indexing%view_loop_to_main_index(i)

         input_dummy = "solar_zenith_view_no"//trim(adjustl(input_num))
         call ncdf_read_array(ncid, trim(adjustl(input_dummy)), &
              input_data%sol_zen(:,:,ii), start = [1, sval])

         input_dummy ="satellite_zenith_view_no"//trim(adjustl(input_num))
         call ncdf_read_array(ncid, trim(adjustl(input_dummy)), &
              input_data%sat_zen(:,:,ii), start = [1, sval])

         input_dummy ="rel_azimuth_view_no"//trim(adjustl(input_num))
         call ncdf_read_array(ncid, trim(adjustl(input_dummy)), &
              input_data%rel_azi(:,:,ii), start = [1, sval])

         input_dummy ="sat_azimuth_view_no"//trim(adjustl(input_num))
         call ncdf_read_array(ncid, trim(adjustl(input_dummy)), &
              input_data%sat_azi(:,:,ii), start = [1, sval])
      end if
   end do

   if (indexing%flags%do_cloud .and. read_flags%do_cloud) then
      call ncdf_read_packed_array(ncid, "cccot_pre", input_data%cccot_pre, &
        start = [1, sval, 1])
      read_flags%do_cloud = .false.
   end if

   if (indexing%flags%do_cldmask .and. read_flags%do_cldmask) then
      call ncdf_read_array(ncid, "cldmask", input_data%cldmask, &
        start = [1, sval, 1])
      read_flags%do_cldmask = .false.
   end if

   if (indexing%flags%do_cldmask_uncertainty .and. &
        read_flags%do_cldmask_uncertainty) then
      call ncdf_read_packed_array(ncid, "cldmask_uncertainty", &
           input_data%cldmask_uncertainty, start = [1, sval, 1])
      read_flags%do_cldmask_uncertainty = .false.
   end if

   if (indexing%flags%do_ann_phase .and. read_flags%do_ann_phase) then
      call ncdf_read_packed_array(ncid, "cphcot", input_data%cphcot, &
        start = [1, sval, 1])
      call ncdf_read_array(ncid, "ann_phase", input_data%ann_phase, &
        start = [1, sval, 1])
      read_flags%do_ann_phase = .false.
   end if

   if (indexing%flags%do_ann_phase_uncertainty .and. &
        read_flags%do_ann_phase_uncertainty) then
      call ncdf_read_packed_array(ncid, "ann_phase_uncertainty", &
           input_data%ann_phase_uncertainty, start = [1, sval, 1])
      read_flags%do_ann_phase_uncertainty = .false.
   end if

   if (indexing%flags%do_phase .and. read_flags%do_phase) then
      call ncdf_read_array(ncid, "phase", input_data%phase, &
        start = [1, sval])
      read_flags%do_phase = .false.
   end if

   if (indexing%flags%do_phase_pavolonis .and. &
        read_flags%do_phase_pavolonis) then
      call ncdf_read_array(ncid, "phase_pavolonis", &
           input_data%phase_pavolonis, start = [1, sval])
      read_flags%do_phase_pavolonis = .false.
   end if

end subroutine read_input_primary_optional


subroutine read_input_primary_once(nfile, fname, input_data, indexing, &
     loop_ind, global_atts, source_atts, sval, use_ml, verbose)

   use global_attributes_m
   use orac_ncdf_m
   use source_attributes_m

   implicit none

   integer,                    intent(in)    :: nfile
   character(len=*),           intent(in)    :: fname(:)
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   type(input_indices_t),      intent(in)    :: loop_ind(:)
   type(global_attributes_t),  intent(inout) :: global_atts
   type(source_attributes_t),  intent(inout) :: source_atts
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose
   logical,                    intent(in)    :: use_ml

   integer                   :: i
   integer                   :: ierr
   integer                   :: ncid, varid
   type(common_file_flags_t) :: read_flags

   ! Flag which optional fields to be read. Set false as they are read.
   read_flags = indexing%flags

   ! Read universally common fields from first input file
   if (.not. use_ml) then
      call ncdf_open(ncid, fname(1), 'read_input_primary_once()')
   else
      call ncdf_open(ncid, fname(3), 'read_input_primary_once()')
   end if

   call ncdf_get_common_attributes(ncid, global_atts, source_atts)

   call ncdf_read_array(ncid, "time", input_data%time, start = [1, sval])

   call ncdf_read_array(ncid, "lon", input_data%lon, start = [1, sval])
   call ncdf_read_array(ncid, "lat", input_data%lat, start = [1, sval])

   call ncdf_read_array(ncid, "lsflag", input_data%lsflag, &
        start = [1, sval])
   call ncdf_read_array(ncid, "lusflag", input_data%lusflag, &
        start = [1, sval])
   call ncdf_read_array(ncid, "dem", input_data%dem, start = [1, sval])

   call ncdf_read_array(ncid, "illum", input_data%illum, &
        start = [1, sval])
   call ncdf_read_array(ncid, "cldtype", input_data%cldtype, &
        start = [1, sval, 1])

   ierr = nf90_inq_varid(ncid, "qcflag", varid)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid(), ', trim(nf90_strerror(ierr)), &
           ', variable: qcflag'
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

   ierr = nf90_inq_varid(ncid, "channels_used", varid)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid(), ', trim(nf90_strerror(ierr)), &
           ', variable: channels_used'
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_masks', input_data%ch_flag_masks)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
           ', variable: channels_used, name: flag_masks'
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_meanings', input_data%ch_flag_meanings)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
           ', variable: channels_used, name: flag_meanings'
      stop error_stop_code
   end if

   ierr = nf90_inq_varid(ncid, "variables_retrieved", varid)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_inq_varid(), ', trim(nf90_strerror(ierr)), &
           ', variable: variables_retrieved'
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_masks', input_data%vr_flag_masks)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
           ', variable: variables_retrieved, name: flag_masks'
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, varid, 'flag_meanings', input_data%vr_flag_meanings)
   if (ierr.ne.NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), ', trim(nf90_strerror(ierr)), &
           ', variable: variables_retrieved, name: flag_meanings'
      stop error_stop_code
   end if

   call read_input_primary_optional(ncid, input_data, loop_ind(1), &
        read_flags, sval, verbose)

   call ncdf_close(ncid, 'read_input_primary_once()')

   do i=2,nfile
      call ncdf_open(ncid, fname(i), 'read_input_primary_once()')
      call read_input_primary_optional(ncid, input_data, loop_ind(i), &
           read_flags, sval, verbose)
      call ncdf_close(ncid, 'read_input_primary_once()')
   end do

end subroutine read_input_primary_once


subroutine read_input_primary_class(fname, input_data, indexing, costonly, &
     sval, verbose)

   use orac_ncdf_m

   implicit none

   character(len=*),           intent(in)    :: fname
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   logical,                    intent(in)    :: costonly
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer :: ncid

   if (verbose) write(*,*) 'Opening primary input file: ', trim(fname)
   call ncdf_open(ncid, fname, 'read_input_primary_class()')
   if (.not. costonly) then
      call read_input_primary_common(ncid, input_data, indexing, sval, verbose)
   else
      
      call read_input_primary_cost_only(ncid, input_data, sval, verbose)
   end if

   if (verbose) write(*,*) 'Closing primary input file.'
   call ncdf_close(ncid, 'read_input_primary_class()')

end subroutine read_input_primary_class

subroutine read_input_primary_classify(fname, input_data, indexing, read_cost, &
     read_ctt, sval, verbose)

   use orac_ncdf_m

   implicit none

   character(len=*),           intent(in)    :: fname
   type(input_data_primary_t), intent(inout) :: input_data
   type(input_indices_t),      intent(in)    :: indexing
   logical,                    intent(in)    :: read_cost
   logical,                    intent(in)    :: read_ctt
   integer,                    intent(in)    :: sval
   logical,                    intent(in)    :: verbose

   integer :: ncid

   if (verbose) write(*,*) 'Opening primary input file: ', trim(fname)
   call ncdf_open(ncid, fname, 'read_input_primary_classify()')
   
   if (read_cost) then
      call ncdf_read_array(ncid, "costja", input_data%costja, start = [1, sval])
      call ncdf_read_array(ncid, "costjm", input_data%costjm, start = [1, sval])
   end if

   if (read_ctt) then
      call ncdf_read_array(ncid, "ctt", input_data%ctt, start = [1, sval])
   end if
   
   if (verbose) write(*,*) 'Closing primary input file.'
   call ncdf_close(ncid, 'read_input_primary_class()')

end subroutine read_input_primary_classify


