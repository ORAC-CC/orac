!-------------------------------------------------------------------------------
! Name: write_output_primary.F90
!
! Purpose:
! Actual writing of the primary output data to the netcdf file is carried out.
!
! Description and Algorithm details:
! Call ncdf_write_array many times.
!
! Arguments:
! Name        Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid        integer In          File ID for open output file
! ind         struct  In          Channel indexing information
! output_data struct  Both        Data to be written to output
!
! History:
! 2011/12/19, MJ: Creates initial version
! 2012/01/06, CP: Added in CWP
! 2012/06/18, CP: Changed cost to a float
! 2012/11/03, MJ: Bug fix in writing of qc flag and illum
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/09/01, GM: Start using the common/orac_ncdf.F90 write_array interface.
! 2014/09/17, GM: Bug fix, forgot to offset y dimension of output.
! 2014/10/24, OS: added variables cldtype, cloudmask, cccot_pre, lusflags, dem,
!    and nisemask
! 2014/12/01, CP: added cloud albedo
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, ather than the
!    MSI file.
! 2015/07/04, CP: Added corrected cth
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2016/01/05, AP: The cloud albedo field name is now properly subscripted with
!    YSolar, rather than just counting up Y_Id.
! 2016/01/06, AP: Add do_cldmask checks from output_flags structure.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/04, AP: Homogenisation of I/O modules.
! 2016/04/28, AP: Add multiple views.
! 2016/07/08, GM: Add fields for cloud layer 2.
! 2017/05/17, OS: Added ann phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_output_primary(ncid, ind, output_data)

   use orac_ncdf_m

   implicit none

   integer,                     intent(in)    :: ncid
   type(common_indices_t),      intent(in)    :: ind
   type(output_data_primary_t), intent(inout) :: output_data

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   integer            :: i

   call ncdf_write_array(ncid,'time', output_data%vid_time, &
        output_data%time(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'lat', output_data%vid_lat, &
        output_data%lat(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'lon', output_data%vid_lon, &
        output_data%lon(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   do i = 1, ind%NViews

      write(input_num,"(i4)") i

      input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_sol_zen(i), output_data%sol_zen(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)

      input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_sat_zen(i), output_data%sat_zen(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)

      input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_rel_azi(i), output_data%rel_azi(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
      input_dummy='sat_azimuth_view_no'//trim(adjustl(input_num))
      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_sat_azi(i), output_data%sat_azi(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)

   end do

if (ind%flags%do_aerosol) then
   call ncdf_write_array(ncid,'aot550', output_data%vid_aot550, &
        output_data%aot550(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'aot550_uncertainty', &
        output_data%vid_aot550_uncertainty, &
        output_data%aot550_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'aot870', output_data%vid_aot870, &
        output_data%aot870(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'aot870_uncertainty', &
        output_data%vid_aot870_uncertainty, &
        output_data%aot870_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'aer', output_data%vid_aer, &
        output_data%aer(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'aer_uncertainty', &
        output_data%vid_aer_uncertainty, &
        output_data%aer_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)
end if

if (ind%flags%do_rho) then
   do i = 1, ind%Nrho
      call ncdf_write_array(ncid,'rho', &
           output_data%vid_rho(i), &
           output_data%rho(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'rho_uncertainty', &
           output_data%vid_rho_uncertainty(i), &
           output_data%rho_uncertainty(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do
end if

if (ind%flags%do_swansea) then
   do i = 1, ind%Nss
      call ncdf_write_array(ncid,'swansea_s', &
           output_data%vid_swansea_s(i), &
           output_data%swansea_s(ind%X0:,ind%Y0:,i), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'swansea_s_uncertainty', &
           output_data%vid_swansea_s_uncertainty(i), &
           output_data%swansea_s_uncertainty(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'diffuse_frac', &
           output_data%vid_diffuse_frac(i), &
           output_data%diffuse_frac(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'diffuse_frac_uncertainty', &
           output_data%vid_diffuse_frac_uncertainty(i), &
           output_data%diffuse_frac_uncertainty(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do

   do i = 1, ind%NViews
      call ncdf_write_array(ncid,'swansea_p', &
           output_data%vid_swansea_p(i), &
           output_data%swansea_p(ind%X0:,ind%Y0:,i), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'swansea_p_uncertainty', &
           output_data%vid_swansea_p_uncertainty(i), &
           output_data%swansea_p_uncertainty(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do
end if

if (ind%flags%do_cloud) then
   call ncdf_write_array(ncid,'cot', output_data%vid_cot, &
        output_data%cot(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cot_uncertainty', &
        output_data%vid_cot_uncertainty, &
        output_data%cot_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cer', output_data%vid_cer, &
        output_data%cer(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cer_uncertainty', &
        output_data%vid_cer_uncertainty, &
        output_data%cer_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctp', output_data%vid_ctp, &
        output_data%ctp(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctp_uncertainty', &
        output_data%vid_ctp_uncertainty, &
        output_data%ctp_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctp_corrected', output_data%vid_ctp_corrected, &
        output_data%ctp_corrected(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctp_corrected_uncertainty', &
        output_data%vid_ctp_corrected_uncertainty, &
        output_data%ctp_corrected_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cc_total', output_data%vid_cc_total, &
        output_data%cc_total(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cc_total_uncertainty', &
        output_data%vid_cc_total_uncertainty, &
        output_data%cc_total_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'stemp', output_data%vid_stemp, &
        output_data%stemp(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'stemp_uncertainty', &
        output_data%vid_stemp_uncertainty, &
        output_data%stemp_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cth', output_data%vid_cth, &
        output_data%cth(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cth_uncertainty', &
        output_data%vid_cth_uncertainty, &
        output_data%cth_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cth_corrected', output_data%vid_cth_corrected, &
        output_data%cth_corrected(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cth_corrected_uncertainty', &
        output_data%vid_cth_corrected_uncertainty, &
        output_data%cth_corrected_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctt', output_data%vid_ctt, &
        output_data%ctt(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctt_uncertainty', &
        output_data%vid_ctt_uncertainty, &
        output_data%ctt_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctt_corrected', output_data%vid_ctt_corrected, &
        output_data%ctt_corrected(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctt_corrected_uncertainty', &
        output_data%vid_ctt_corrected_uncertainty, &
        output_data%ctt_corrected_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cwp', output_data%vid_cwp, &
        output_data%cwp(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cwp_uncertainty', &
        output_data%vid_cwp_uncertainty, &
        output_data%cwp_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   do i = 1, ind%Nalb
      call ncdf_write_array(ncid,'cloud_albedo', &
           output_data%vid_cloud_albedo(i), &
           output_data%cloud_albedo(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)

      call ncdf_write_array(ncid,'cloud_albedo_uncertainty', &
           output_data%vid_cloud_albedo_uncertainty(i), &
           output_data%cloud_albedo_uncertainty(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do

   do i = 1, ind%Ncee
      call ncdf_write_array(ncid,'cee', &
           output_data%vid_cee(i), &
           output_data%cee(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)

      call ncdf_write_array(ncid,'cee_uncertainty', &
           output_data%vid_cee_uncertainty(i), &
           output_data%cee_uncertainty(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do

   call ncdf_write_array(ncid,'cccot_pre', output_data%vid_cccot_pre, &
        output_data%cccot_pre(ind%X0:,ind%Y0:,:), 1, 1, ind%Xdim, 1, 1, ind%Ydim, &
        1, 1, ind%NViews)

   call ncdf_write_array(ncid,'ann_phase', output_data%vid_ann_phase, &
        output_data%ann_phase(ind%X0:,ind%Y0:,:), 1, 1, ind%Xdim, 1, 1, ind%Ydim, &
        1, 1, ind%NViews)
   call ncdf_write_array(ncid,'ann_phase_uncertainty', &
        output_data%vid_ann_phase_uncertainty, &
        output_data%ann_phase_uncertainty(ind%X0:,ind%Y0:,:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim, 1, 1, ind%NViews)
   call ncdf_write_array(ncid,'cphcot', output_data%vid_cphcot, &
        output_data%cphcot(ind%X0:,ind%Y0:,:), 1, 1, ind%Xdim, 1, 1, ind%Ydim, &
        1, 1, ind%NViews)

end if

if (ind%flags%do_cloud_layer_2) then
   call ncdf_write_array(ncid,'cot2', output_data%vid_cot2, &
        output_data%cot2(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cot2_uncertainty', &
        output_data%vid_cot2_uncertainty, &
        output_data%cot2_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cer2', output_data%vid_cer2, &
        output_data%cer2(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cer2_uncertainty', &
        output_data%vid_cer2_uncertainty, &
        output_data%cer2_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctp2', output_data%vid_ctp2, &
        output_data%ctp2(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctp2_uncertainty', &
        output_data%vid_ctp2_uncertainty, &
        output_data%ctp2_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cc_total2', output_data%vid_cc_total2, &
        output_data%cc_total2(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cc_total2_uncertainty', &
        output_data%vid_cc_total2_uncertainty, &
        output_data%cc_total2_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cth2', output_data%vid_cth2, &
        output_data%cth2(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cth2_uncertainty', &
        output_data%vid_cth2_uncertainty, &
        output_data%cth2_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctt2', output_data%vid_ctt2, &
        output_data%ctt2(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctt2_uncertainty', &
        output_data%vid_ctt2_uncertainty, &
        output_data%ctt2_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cwp2', output_data%vid_cwp2, &
        output_data%cwp2(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cwp2_uncertainty', &
        output_data%vid_cwp2_uncertainty, &
        output_data%cwp2_uncertainty(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)
end if

   call ncdf_write_array(ncid,'niter', output_data%vid_niter, &
        output_data%niter(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'costja', output_data%vid_costja, &
        output_data%costja(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'costjm', output_data%vid_costjm, &
        output_data%costjm(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'qcflag', output_data%vid_qcflag, &
        output_data%qcflag(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'channels_used', output_data%vid_channels_used, &
        output_data%channels_used(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'variables_retrieved', &
        output_data%vid_variables_retrieved, &
        output_data%variables_retrieved(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'lsflag', output_data%vid_lsflag, &
        output_data%lsflag(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'lusflag', output_data%vid_lusflag, &
        output_data%lusflag(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'dem',output_data%vid_dem, &
        output_data%dem(ind%X0:, ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'illum',output_data%vid_illum, &
        output_data%illum(ind%X0:, ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cldtype', output_data%vid_cldtype, &
        output_data%cldtype(ind%X0:,ind%Y0:,:), 1, 1, ind%Xdim, 1, 1, ind%Ydim, &
        1, 1, ind%NViews)

if (ind%flags%do_cldmask) then
   call ncdf_write_array(ncid,'cldmask', output_data%vid_cldmask, &
        output_data%cldmask(ind%X0:,ind%Y0:,:), 1, 1, ind%Xdim, 1, 1, ind%Ydim, &
        1, 1, ind%NViews)
end if
if (ind%flags%do_cldmask_uncertainty) then
   call ncdf_write_array(ncid,'cldmask_uncertainty', &
        output_data%vid_cldmask_uncertainty, &
        output_data%cldmask_uncertainty(ind%X0:,ind%Y0:,:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim, 1, 1, ind%NViews)
end if

if (ind%flags%do_phase) then
   call ncdf_write_array(ncid,'phase', output_data%vid_phase, &
        output_data%phase(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
end if

if (ind%flags%do_phase_pavolonis) then
   call ncdf_write_array(ncid,'phase_pavolonis', &
        output_data%vid_phase_pavolonis, &
        output_data%phase_pavolonis(ind%X0:,ind%Y0:), &
        1, 1, ind%Xdim, 1, 1, ind%Ydim)
end if

   if (ind%flags%do_indexing) then
      call ncdf_write_array(ncid,'y_id', output_data%vid_y_id, &
           output_data%y_id, 1, 1, ind%Ny)
      call ncdf_write_array(ncid,'view_id', output_data%vid_view_id, &
           output_data%view_id, 1, 1, ind%NViews)
      call ncdf_write_array(ncid,'ch_is', output_data%vid_ch_is, &
           output_data%ch_is, 1, 1, ind%Ny)
      call ncdf_write_array(ncid,'rho_flags', output_data%vid_rho_flags, &
           output_data%rho_flags, 1, 1, ind%Ny)
   end if

end subroutine write_output_primary
