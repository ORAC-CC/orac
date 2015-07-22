!-------------------------------------------------------------------------------
! Name: write_primary_pp.F90
!
! Purpose:
! Actual writing of the primary output data to the netcdf file is carried out.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! xxxx/xx/xx, Caroline Poulsen: Original version copied from ORAC main processor
! 2012/03/18, Caroline Poulsen: modified to add cloud flag
! 2012/07/06, Matthias Jerg: extensively overhauls and restructures the code
! 2013/01/17, Matthias Jerg: Adds code to accommodate uncertainties of ctt and
!    cth
! 2014/06/04, MJ: changes routine names to "*_pp" to avoid confusion when
!    building libraries.
! 2014/10/24, OS: added variables lusflag, cldtype, cloudmask, DEM (currently
!    deactivated), and nisemask
! 2014/11/20, OS: added Pavolonis cloud phase variable
! 2014/11/25, CP: added in cloud albedo
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_primary_pp(ncid, ixstart, ixstop, iystart, iystop, indexing, &
                            output_data, global_atts)

   use global_attributes
   use input_routines
   use orac_ncdf
   use postproc_constants

   implicit none

   integer,                      intent(in) :: ncid
   integer,                      intent(in) :: ixstart, ixstop, &
                                               iystart, iystop
   type(counts_and_indexes),     intent(in) :: indexing
   type(output_data_primary_pp), intent(in) :: output_data
   type(global_attributes_s),    intent(in) :: global_atts

   character(len=32)  :: input_num
   character(len=512) :: input_dummy
   integer            :: i
   integer            :: n_x, n_y

   n_x = ixstop - ixstart + 1
   n_y = iystop - iystart + 1


   output_data%time = output_data%time + 2451758.0

   call nc_write_array(ncid,'time',output_data%vid_time,&
           output_data%time(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'lat',output_data%vid_lat,&
           output_data%lat(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'lon',output_data%vid_lon,&
           output_data%lon(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   do i=1,indexing%NViews

      write(input_num,"(i4)") i

      input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_sol_zen(i),output_data%sol_zen(ixstart:,:), &
              1,1,n_x,1,1,n_y)

      input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_sat_zen(i),output_data%sat_zen(ixstart:,:), &
              1,1,n_x,1,1,n_y)

      input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_rel_azi(i),output_data%rel_azi(ixstart:,:), &
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

   call nc_write_array(ncid,'cldmask',output_data%vid_cldmask,&
           output_data%cldmask(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cccot_pre',output_data%vid_cccot_pre,&
           output_data%cccot_pre(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'lusflag',output_data%vid_lusflag,&
           output_data%lusflag(ixstart:,iystart:),1,1,n_x,1,1,n_y)

!  call nc_write_array(ncid,'dem',output_data%vid_dem,&
!          output_data%dem(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'nisemask',output_data%vid_nisemask,&
           output_data%nisemask(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   do i=1,indexing%NSolar
      write(input_num,"(i4)") indexing%Y_Id(i)

      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_cloud_albedo(i),output_data%cloud_albedo(ixstart:,iystart:,i), &
              1,1,n_x,1,1,n_y)
   end do

end subroutine write_primary_pp
