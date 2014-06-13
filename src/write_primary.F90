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
! 2014/06/13, GM: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known
!-------------------------------------------------------------------------------

subroutine write_primary(Ctrl, ncid, ixstart, ixstop, iystart, iystop, &
                         spixel_scan_out, status)

   use CTRL_def
   use SPixel_def

   implicit none

   type(CTRL_t),                         intent(in)    :: Ctrl
   integer,                              intent(in)    :: ncid
   integer,                              intent(in)    :: ixstart
   integer,                              intent(in)    :: ixstop
   integer,                              intent(in)    :: iystart
   integer,                              intent(in)    :: iystop
   type(spixel_scanline_primary_output), intent(inout) :: spixel_scan_out
   integer,                              intent(inout) :: status

   character(len=20)  :: input_num
   character(len=500) :: input_dummy
   integer            :: ierr
   integer            :: iviews
   integer            :: wo = 0

   call nc_write_L2_double(ncid,'time',spixel_scan_out%vidtime,&
           spixel_scan_out%time(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_float(ncid,'lon',spixel_scan_out%vidlon,&
           spixel_scan_out%lon(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_float(ncid,'lat',spixel_scan_out%vidlat,&
           spixel_scan_out%lat(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   do iviews=1,Ctrl%Ind%NViews

      write(input_num,"(i4)") iviews

      input_dummy='satellite_zenith_view'//trim(adjustl(input_num))
      call nc_write_L2_float(ncid,trim(adjustl(input_dummy)),spixel_scan_out%vidsat_zen(iviews),&
              spixel_scan_out%sat_zen(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)

      input_dummy='solar_zenith_view'//trim(adjustl(input_num))
      call nc_write_L2_float(ncid,trim(adjustl(input_dummy)),spixel_scan_out%vidsol_zen(iviews),&
              spixel_scan_out%sol_zen(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)

      input_dummy='rel_azimuth_view'//trim(adjustl(input_num))
      call nc_write_L2_float(ncid,trim(adjustl(input_dummy)),spixel_scan_out%vidrel_azi(iviews),&
              spixel_scan_out%rel_azi(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)

   enddo

   call nc_write_L2_short(ncid,'cot',spixel_scan_out%vidcot,&
           spixel_scan_out%cot(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'cot_uncertainty',spixel_scan_out%vidcoterror,&
           spixel_scan_out%cot_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'ref',spixel_scan_out%vidref,&
           spixel_scan_out%ref(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'ref_uncertainty',spixel_scan_out%vidreferror,&
           spixel_scan_out%ref_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'ctp',spixel_scan_out%vidctp,&
           spixel_scan_out%ctp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'ctp_uncertainty',spixel_scan_out%vidctperror,&
           spixel_scan_out%ctp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'cc_total',spixel_scan_out%vidcct,&
           spixel_scan_out%cct(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'cc_total_uncertainty',spixel_scan_out%vidccterror,&
           spixel_scan_out%cct_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'stemp',spixel_scan_out%vidstemp,&
           spixel_scan_out%stemp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'stemp_uncertainty',spixel_scan_out%vidstemperror,&
           spixel_scan_out%stemp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'cth',spixel_scan_out%vidcth,&
           spixel_scan_out%cth(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'cth_uncertainty',spixel_scan_out%vidctherror,&
           spixel_scan_out%cth_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'ctt',spixel_scan_out%vidctt,&
           spixel_scan_out%ctt(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'ctt_uncertainty',spixel_scan_out%vidctterror,&
           spixel_scan_out%ctt_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'cwp',spixel_scan_out%vidcwp,&
           spixel_scan_out%cwp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'cwp_uncertainty',spixel_scan_out%vidcwperror,&
           spixel_scan_out%cwp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_byte(ncid,'convergence',spixel_scan_out%vidconvergence,&
           spixel_scan_out%convergence(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_byte(ncid,'niter',spixel_scan_out%vidniter,&
           spixel_scan_out%niter(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_byte(ncid,'phase',spixel_scan_out%vidpchange,&
           spixel_scan_out%pchange(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_float(ncid,'costja',spixel_scan_out%vidcostja,&
           spixel_scan_out%costja(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_float(ncid,'costjm',spixel_scan_out%vidcostjm,&
           spixel_scan_out%costjm(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_byte(ncid,'lsflag',spixel_scan_out%vidlsflag,&
           spixel_scan_out%lsflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'qcflag',spixel_scan_out%vidqcflag,&
           spixel_scan_out%qcflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_byte(ncid,'illum',spixel_scan_out%vidillum,&
           spixel_scan_out%illum(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   if (ierr .ne. 0 ) then
      status=PrimaryFileWriteErr
      write(*,*) 'write_primary.inc: netcdf primary file write error: ', status
      call Write_Log(Ctrl,'write_primary.inc: netcdf primary file write error: ', status)
      stop
   endif

end subroutine write_primary
