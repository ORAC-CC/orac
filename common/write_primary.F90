!-------------------------------------------------------------------------------
! Name: write_primary.F90
!
! Purpose:
! Actual writing of the primary output data to the netcdf file is carried out.
!
! Description and Algorithm details:
! Call nc_write_array many times.
!
! Arguments:
! Name        Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl        struct  In          Control parameters for the retrieval
! ncid        integer In          File ID for open output file
! ixstart     integer In          Starting index on first dimension of output
! ixstop      integer In          Ending index on first dimension of output
! iystart     integer In          Starting index on second dimension of output
! iystop      integer In          Ending index on second dimension of output
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
! 2014/10/24, OS: added variables cldtype, cloudmask, cccot_pre, lusflags,
!    dem, and nisemask
! 2014/12/01, CP: added cloud albedo
! 2014/12/19, AP: YSolar and YThermal now contain the index of
!    solar/thermal channels with respect to the channels actually processed,
!    rather than the MSI file.
! 2015/07/04, CP: Added corrected cth
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_primary(ncid, ixstart, ixstop, iystart, iystop, output_data, NViews, NSolar, Y_Id, do_phase_pavolonis, do_cldmask, do_cloudmask_pre, do_dem)

   use orac_ncdf

   implicit none

   integer,                   intent(in)    :: ncid
   integer,                   intent(in)    :: ixstart
   integer,                   intent(in)    :: ixstop
   integer,                   intent(in)    :: iystart
   integer,                   intent(in)    :: iystop
   type(output_data_primary), intent(inout) :: output_data
   integer,                   intent(in)    :: NViews
   integer,                   intent(in)    :: NSolar
   integer,                   intent(in)    :: Y_Id(:)
   logical,                   intent(in)    :: do_phase_pavolonis
   logical,                   intent(in)    :: do_cldmask
   logical,                   intent(in)    :: do_cloudmask_pre
   logical,                   intent(in)    :: do_dem

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   integer            :: i
   integer            :: n_x
   integer            :: n_y

   n_x = ixstop - ixstart + 1
   n_y = iystop - iystart + 1

   call nc_write_array(ncid,'time',output_data%vid_time,&
           output_data%time(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'lat',output_data%vid_lat,&
           output_data%lat(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'lon',output_data%vid_lon,&
           output_data%lon(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   do i=1,NViews

      write(input_num,"(i4)") i

      input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_sol_zen(i),output_data%sol_zen(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)

      input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_sat_zen(i),output_data%sat_zen(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)

      input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_rel_azi(i),output_data%rel_azi(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)

   end do

   call nc_write_array(ncid,'cot',output_data%vid_cot,&
           output_data%cot(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cot_uncertainty',output_data%vid_cot_error,&
           output_data%cot_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ref',output_data%vid_ref,&
           output_data%ref(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ref_uncertainty',output_data%vid_ref_error,&
           output_data%ref_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ctp',output_data%vid_ctp,&
           output_data%ctp(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ctp_uncertainty',output_data%vid_ctp_error,&
           output_data%ctp_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cc_total',output_data%vid_cct,&
           output_data%cct(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cc_total_uncertainty',output_data%vid_cct_error,&
           output_data%cct_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'stemp',output_data%vid_stemp,&
           output_data%stemp(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'stemp_uncertainty',output_data%vid_stemp_error,&
           output_data%stemp_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cth',output_data%vid_cth,&
           output_data%cth(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cth_uncertainty',output_data%vid_cth_error,&
           output_data%cth_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cth_corrected',output_data%vid_cth_corrected,&
           output_data%cth_corrected(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cth_corrected_uncertainty',output_data%vid_cth_corrected_error,&
           output_data%cth_corrected_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ctt',output_data%vid_ctt,&
           output_data%ctt(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ctt_uncertainty',output_data%vid_ctt_error,&
           output_data%ctt_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cwp',output_data%vid_cwp,&
           output_data%cwp(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cwp_uncertainty',output_data%vid_cwp_error,&
           output_data%cwp_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'convergence',output_data%vid_convergence,&
           output_data%convergence(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'niter',output_data%vid_niter,&
           output_data%niter(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'phase',output_data%vid_phase,&
           output_data%phase(ixstart:,iystart:),1,1,n_x,1,1,n_y)
if (do_phase_pavolonis) then
   call nc_write_array(ncid,'phase_pavolonis',output_data%vid_phase_pavolonis,&
           output_data%phase_pavolonis(ixstart:,iystart:),1,1,n_x,1,1,n_y)
end if
   call nc_write_array(ncid,'costja',output_data%vid_costja,&
           output_data%costja(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'costjm',output_data%vid_costjm,&
           output_data%costjm(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'lsflag',output_data%vid_lsflag,&
           output_data%lsflag(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'qcflag',output_data%vid_qcflag,&
           output_data%qcflag(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'illum',output_data%vid_illum,&
           output_data%illum(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cldtype',output_data%vid_cldtype,&
           output_data%cldtype(ixstart:,iystart:),1,1,n_x,1,1,n_y)
if (do_cldmask) then
   call nc_write_array(ncid,'cldmask',output_data%vid_cldmask,&
           output_data%cldmask(ixstart:,iystart:),1,1,n_x,1,1,n_y)
end if
if (do_cloudmask_pre) then
   call nc_write_array(ncid,'cloudmask_pre',output_data%vid_cldmask,&
           output_data%cldmask(ixstart:,iystart:),1,1,n_x,1,1,n_y)
end if
   call nc_write_array(ncid,'cccot_pre',output_data%vid_cccot_pre,&
           output_data%cccot_pre(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'lusflag',output_data%vid_lusflag,&
           output_data%lusflag(ixstart:,iystart:),1,1,n_x,1,1,n_y)
if (do_dem) then
   call nc_write_array(ncid,'dem',output_data%vid_dem,&
           output_data%dem(ixstart:,iystart:),1,1,n_x,1,1,n_y)
end if
   call nc_write_array(ncid,'nisemask',output_data%vid_nisemask,&
           output_data%nisemask(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   do i=1,NSolar
      write(input_num,"(i4)") Y_Id(i)

      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_cloud_albedo(i),output_data%cloud_albedo(ixstart:,iystart:,i), &
              1,1,n_x,1,1,n_y)
   end do

end subroutine write_primary
