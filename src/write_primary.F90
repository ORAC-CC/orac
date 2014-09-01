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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_primary(Ctrl, ncid, ixstart, ixstop, iystart, iystop, &
                         output_data, status)

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
   integer,                   intent(inout) :: status

   character(len=20)  :: input_num
   character(len=500) :: input_dummy
   integer            :: iviews

   status = 0

   call nc_write_array(ncid,'time',output_data%vid_time,&
           output_data%time(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'lat',output_data%vid_lat,&
           output_data%lat(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'lon',output_data%vid_lon,&
           output_data%lon(:,:),ixstart,ixstop,iystart,iystop)

   do iviews=1,Ctrl%Ind%NViews

      write(input_num,"(i4)") iviews

      input_dummy='solar_zenith_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)),output_data%vid_sol_zen(iviews),&
              output_data%sol_zen(:,:,iviews),ixstart,ixstop,iystart,iystop)

      input_dummy='satellite_zenith_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)),output_data%vid_sat_zen(iviews),&
              output_data%sat_zen(:,:,iviews),ixstart,ixstop,iystart,iystop)

      input_dummy='rel_azimuth_view_no'//trim(adjustl(input_num))
      call nc_write_array(ncid,trim(adjustl(input_dummy)),output_data%vid_rel_azi(iviews),&
              output_data%rel_azi(:,:,iviews),ixstart,ixstop,iystart,iystop)

   end do

   call nc_write_array(ncid,'cot',output_data%vid_cot,&
           output_data%cot(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'cot_uncertainty',output_data%vid_coterror,&
           output_data%cot_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'ref',output_data%vid_ref,&
           output_data%ref(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'ref_uncertainty',output_data%vid_referror,&
           output_data%ref_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'ctp',output_data%vid_ctp,&
           output_data%ctp(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'ctp_uncertainty',output_data%vid_ctperror,&
           output_data%ctp_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'cc_total',output_data%vid_cct,&
           output_data%cct(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'cc_total_uncertainty',output_data%vid_ccterror,&
           output_data%cct_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'stemp',output_data%vid_stemp,&
           output_data%stemp(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'stemp_uncertainty',output_data%vid_stemperror,&
           output_data%stemp_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'cth',output_data%vid_cth,&
           output_data%cth(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'cth_uncertainty',output_data%vid_ctherror,&
           output_data%cth_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'ctt',output_data%vid_ctt,&
           output_data%ctt(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'ctt_uncertainty',output_data%vid_ctterror,&
           output_data%ctt_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'cwp',output_data%vid_cwp,&
           output_data%cwp(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'cwp_uncertainty',output_data%vid_cwperror,&
           output_data%cwp_error(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'convergence',output_data%vid_convergence,&
           output_data%convergence(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'niter',output_data%vid_niter,&
           output_data%niter(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'phase',output_data%vid_phase,&
           output_data%phase(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'costja',output_data%vid_costja,&
           output_data%costja(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'costjm',output_data%vid_costjm,&
           output_data%costjm(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'lsflag',output_data%vid_lsflag,&
           output_data%lsflag(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'qcflag',output_data%vid_qcflag,&
           output_data%qcflag(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'illum',output_data%vid_illum,&
           output_data%illum(:,:),ixstart,ixstop,iystart,iystop)

end subroutine write_primary
