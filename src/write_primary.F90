! Name: write_primary.inc
!
!
! Purpose:
! Actual writing of the primary output data to the netcdf file is carried out.
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2011/12/19: Matthias Jerg creates initial output for main output variables.
!2012/01/06: Caroline Poulsen added in CWP
!2012/06/18: Caroline Poulsen changed cost to a float
!2012/11/03 MJ bug fix in writing of qc flag and illum
!2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
!2014/06/13, Greg McGarragh: Put the code into a subroutine.
!
! $Id$
!
! Bugs:
!
!none known

subroutine write_primary(Ctrl, ncid, ixstart, ixstop, iystart, iystop, spixel_scan_out, status)

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

!          write(*,*) 'hier_a'
         CALL nc_write_L2_double(ncid,'time',spixel_scan_out%vidtime,&
         & spixel_scan_out%time(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
!         write(*,*) 'hier_ab'


         CALL nc_write_L2_float(ncid,'lon',spixel_scan_out%vidlon,&
                                & spixel_scan_out%lon(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
!         write(*,*) 'hier_ac'
         CALL nc_write_L2_float(ncid,'lat',spixel_scan_out%vidlat,&
                        & spixel_scan_out%lat(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
!          pause

         do iviews=1,Ctrl%Ind%NViews

            write(input_num,"(i4)") iviews
            input_dummy='satellite_zenith_view'//trim(adjustl(input_num))
            CALL nc_write_L2_float(ncid,trim(adjustl(input_dummy)),spixel_scan_out%vidsat_zen(iviews),&
               & spixel_scan_out%sat_zen(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)
            !         write(*,*) 'hier_b'
            input_dummy='solar_zenith_view'//trim(adjustl(input_num))
            CALL nc_write_L2_float(ncid,trim(adjustl(input_dummy)),spixel_scan_out%vidsol_zen(iviews),&
               & spixel_scan_out%sol_zen(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)
            !         write(*,*) 'hier_c'
            input_dummy='rel_azimuth_view'//trim(adjustl(input_num))
            CALL nc_write_L2_float(ncid,trim(adjustl(input_dummy)),spixel_scan_out%vidrel_azi(iviews),&
           & spixel_scan_out%rel_azi(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)
            
         enddo

         CALL nc_write_L2_short(ncid,'cot',spixel_scan_out%vidcot,&
                 & spixel_scan_out%cot(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'cot_uncertainty',spixel_scan_out%vidcoterror,&
                & spixel_scan_out%cot_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         
         CALL nc_write_L2_short(ncid,'ref',spixel_scan_out%vidref,&
                & spixel_scan_out%ref(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid,'ref_uncertainty',spixel_scan_out%vidreferror,&
                & spixel_scan_out%ref_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'ctp',spixel_scan_out%vidctp,&
                & spixel_scan_out%ctp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

        CALL nc_write_L2_short(ncid,'ctp_uncertainty',spixel_scan_out%vidctperror,&
                & spixel_scan_out%ctp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'cc_total',spixel_scan_out%vidcct,&
                & spixel_scan_out%cct(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid,'cc_total_uncertainty',spixel_scan_out%vidccterror,&
                & spixel_scan_out%cct_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'stemp',spixel_scan_out%vidstemp,&
                & spixel_scan_out%stemp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid,'stemp_uncertainty',spixel_scan_out%vidstemperror,&
                & spixel_scan_out%stemp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'cth',spixel_scan_out%vidcth,&
                & spixel_scan_out%cth(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

        CALL nc_write_L2_short(ncid,'cth_uncertainty',spixel_scan_out%vidctherror,&
                & spixel_scan_out%cth_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'ctt',spixel_scan_out%vidctt,&
                & spixel_scan_out%ctt(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

        CALL nc_write_L2_short(ncid,'ctt_uncertainty',spixel_scan_out%vidctterror,&
                & spixel_scan_out%ctt_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid,'cwp',spixel_scan_out%vidcwp,&
                & spixel_scan_out%cwp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid,'cwp_uncertainty',spixel_scan_out%vidcwperror,&
                & spixel_scan_out%cwp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         
         CALL nc_write_L2_byte(ncid,'convergence',spixel_scan_out%vidconvergence,&
               & spixel_scan_out%convergence(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_byte(ncid,'niter',spixel_scan_out%vidniter,&
               & spixel_scan_out%niter(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_byte(ncid,'phase',spixel_scan_out%vidpchange,&
               & spixel_scan_out%pchange(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!         CALL nc_write_L2_short(ncid,'costja',spixel_scan_out%vidcostja,&
!        & spixel_scan_out%costja(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!         CALL nc_write_L2_short(ncid,'costjm',spixel_scan_out%vidcostjm,&
!        & spixel_scan_out%costjm(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_float(ncid,'costja',spixel_scan_out%vidcostja,&
        & spixel_scan_out%costja(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_float(ncid,'costjm',spixel_scan_out%vidcostjm,&
        & spixel_scan_out%costjm(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_byte(ncid,'lsflag',spixel_scan_out%vidlsflag,&
               & spixel_scan_out%lsflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!         CALL nc_write_L2_float(ncid,'qcflag',spixel_scan_out%vidqcflag,&
 !              & spixel_scan_out%qcflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!MST         CALL nc_write_L2_short(ncid,'qcflag',spixel_scan_out%vidqcflag,&
!MST              spixel_scan_out%qcflag(:,j),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid,'qcflag',spixel_scan_out%vidqcflag,&
              spixel_scan_out%qcflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!MST         CALL nc_write_L2_byte(ncid,'illum',spixel_scan_out%vidillum,&
!MST              spixel_scan_out%illum(:,j),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_byte(ncid,'illum',spixel_scan_out%vidillum,&
              spixel_scan_out%illum(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

print*,'mnimaxi'
print*,shape(spixel_scan_out%illum)
print*,'illum',spixel_scan_out%vidillum,&
              ixstart,ixstop,iystart,iystop
print*,minval(spixel_scan_out%illum),maxval(spixel_scan_out%illum)
     if(ierr .ne. 0 ) then 
       status=PrimaryFileWriteErr
       write(*,*) 'write_primary.inc: netcdf primary file write error:', status
       call Write_Log(Ctrl,'write_primary.inc: netcdf primary file write error:', status)
       stop
       
       endif

end subroutine write_primary
