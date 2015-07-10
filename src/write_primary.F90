!-------------------------------------------------------------------------------
! Name: write_primary.F90
!
! Purpose:
! Actual writing of the primary output data to the netcdf file is carried out.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Return value:
! Name Type Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: Creates initial version
! 2012/01/06, Caroline Poulsen: Added in CWP
! 2012/06/18, Caroline Poulsen: Changed cost to a float
! 2012/11/03, Matthias Jerg: Bug fix in writing of qc flag and illum
! 2013/01/17, Matthias Jerg: Adds code to accommodate uncertainties of ctt and
!    cth
! 2014/06/13, Greg McGarragh: Put the code into a subroutine.
! 2014/06/13, Greg McGarragh: Cleaned up the code.
! 2014/09/01, Greg McGarragh: Start using the common/orac_ncdf.F90 write_array
!    interface.
! 2014/09/17, Greg McGarragh: Bug fix, forgot to offset y dimension of output.
! 2014/10/24, Oliver Sus: added variables cldtype, cloudmask, cccot_pre, lusflags,
!    dem, and nisemask
! 2014/12/01, Caroline Poulsen: added cloud albedo
! 2014/12/19, Adam Povey: YSolar and YThermal now contain the index of
!     solar/thermal channels with respect to the channels actually processed,
!     rather than the MSI file.
! 2015/07/04, Caroline Poulsen: Added corrected cth
!

! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_primary(Ctrl, ncid, ixstart, ixstop, iystart, iystop, &
                         output_data)

   use CTRL_def
   use orac_ncdf
   use SPixel_def

   implicit none

   type(CTRL_t),              intent(in)    :: Ctrl
   integer,                   intent(in)    :: ncid
   integer,                   intent(in)    :: ixstart
   integer,                   intent(in)    :: ixstop
   integer,                   intent(in)    :: iystart
   integer,                   intent(in)    :: iystop
   type(output_data_primary), intent(inout) :: output_data

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

   do i=1,Ctrl%Ind%NViews

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
   call nc_write_array(ncid,'cot_uncertainty',output_data%vid_coterror,&
           output_data%cot_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ref',output_data%vid_ref,&
           output_data%ref(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ref_uncertainty',output_data%vid_referror,&
           output_data%ref_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ctp',output_data%vid_ctp,&
           output_data%ctp(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ctp_uncertainty',output_data%vid_ctperror,&
           output_data%ctp_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cc_total',output_data%vid_cct,&
           output_data%cct(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cc_total_uncertainty',output_data%vid_ccterror,&
           output_data%cct_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'stemp',output_data%vid_stemp,&
           output_data%stemp(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'stemp_uncertainty',output_data%vid_stemperror,&
           output_data%stemp_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cth',output_data%vid_cth,&
           output_data%cth(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cth_uncertainty',output_data%vid_ctherror,&
           output_data%cth_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cth_corrected',output_data%vid_cth_corrected,&
           output_data%cth_corrected(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cth_corrected_uncertainty',output_data%vid_cth_correctederror,&
           output_data%cth_corrected_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ctt',output_data%vid_ctt,&
           output_data%ctt(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ctt_uncertainty',output_data%vid_ctterror,&
           output_data%ctt_error(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cwp',output_data%vid_cwp,&
           output_data%cwp(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cwp_uncertainty',output_data%vid_cwperror,&
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

   call nc_write_array(ncid,'dem',output_data%vid_dem,&
           output_data%dem(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'nisemask',output_data%vid_nisemask,&
           output_data%nisemask(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   do i=1,Ctrl%Ind%NSolar
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(i)

      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_cloud_albedo(i),output_data%cloud_albedo(ixstart:,iystart:,i), &
              1,1,n_x,1,1,n_y)
   end do

end subroutine write_primary
