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
!
! $Id$
!
! Bugs:
!
!none known

!          write(*,*) 'hier_a'
         CALL nc_write_L2_double(ncid_primary,'time',spixel_scan_out%vidtime,&
         & spixel_scan_out%time(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
!         write(*,*) 'hier_ab'


         CALL nc_write_L2_float(ncid_primary,'lon',spixel_scan_out%vidlon,&
                                & spixel_scan_out%lon(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
!         write(*,*) 'hier_ac'
         CALL nc_write_L2_float(ncid_primary,'lat',spixel_scan_out%vidlat,&
                        & spixel_scan_out%lat(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
!          pause

         do iviews=1,Ctrl%Ind%NViews

            write(input_num,"(i4)") iviews
            input_dummy='satellite_zenith_view'//trim(adjustl(input_num))
            CALL nc_write_L2_float(ncid_primary,trim(adjustl(input_dummy)),spixel_scan_out%vidsat_zen(iviews),&
               & spixel_scan_out%sat_zen(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)
            !         write(*,*) 'hier_b'
            input_dummy='solar_zenith_view'//trim(adjustl(input_num))
            CALL nc_write_L2_float(ncid_primary,trim(adjustl(input_dummy)),spixel_scan_out%vidsol_zen(iviews),&
               & spixel_scan_out%sol_zen(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)
            !         write(*,*) 'hier_c'
            input_dummy='rel_azimuth_view'//trim(adjustl(input_num))
            CALL nc_write_L2_float(ncid_primary,trim(adjustl(input_dummy)),spixel_scan_out%vidrel_azi(iviews),&
           & spixel_scan_out%rel_azi(:,:,iviews),ixstart,ixstop,iystart,iystop,wo,ierr)
            
         enddo

         CALL nc_write_L2_short(ncid_primary,'cot',spixel_scan_out%vidcot,&
                 & spixel_scan_out%cot(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_primary,'cot_uncertainty',spixel_scan_out%vidcoterror,&
                & spixel_scan_out%cot_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         
         CALL nc_write_L2_short(ncid_primary,'ref',spixel_scan_out%vidref,&
                & spixel_scan_out%ref(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid_primary,'ref_uncertainty',spixel_scan_out%vidreferror,&
                & spixel_scan_out%ref_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_primary,'ctp',spixel_scan_out%vidctp,&
                & spixel_scan_out%ctp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

        CALL nc_write_L2_short(ncid_primary,'ctp_uncertainty',spixel_scan_out%vidctperror,&
                & spixel_scan_out%ctp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_primary,'cc_total',spixel_scan_out%vidcct,&
                & spixel_scan_out%cct(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid_primary,'cc_total_uncertainty',spixel_scan_out%vidccterror,&
                & spixel_scan_out%cct_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_primary,'stemp',spixel_scan_out%vidstemp,&
                & spixel_scan_out%stemp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid_primary,'stemp_uncertainty',spixel_scan_out%vidstemperror,&
                & spixel_scan_out%stemp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_primary,'cth',spixel_scan_out%vidcth,&
                & spixel_scan_out%cth(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

        CALL nc_write_L2_short(ncid_primary,'cth_uncertainty',spixel_scan_out%vidctherror,&
                & spixel_scan_out%cth_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_primary,'ctt',spixel_scan_out%vidctt,&
                & spixel_scan_out%ctt(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

        CALL nc_write_L2_short(ncid_primary,'ctt_uncertainty',spixel_scan_out%vidctterror,&
                & spixel_scan_out%ctt_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid_primary,'cwp',spixel_scan_out%vidcwp,&
                & spixel_scan_out%cwp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid_primary,'cwp_uncertainty',spixel_scan_out%vidcwperror,&
                & spixel_scan_out%cwp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
         
         CALL nc_write_L2_byte(ncid_primary,'convergence',spixel_scan_out%vidconvergence,&
               & spixel_scan_out%convergence(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_byte(ncid_primary,'niter',spixel_scan_out%vidniter,&
               & spixel_scan_out%niter(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_byte(ncid_primary,'phase',spixel_scan_out%vidpchange,&
               & spixel_scan_out%pchange(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!         CALL nc_write_L2_short(ncid_primary,'costja',spixel_scan_out%vidcostja,&
!        & spixel_scan_out%costja(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!         CALL nc_write_L2_short(ncid_primary,'costjm',spixel_scan_out%vidcostjm,&
!        & spixel_scan_out%costjm(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_float(ncid_primary,'costja',spixel_scan_out%vidcostja,&
        & spixel_scan_out%costja(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_float(ncid_primary,'costjm',spixel_scan_out%vidcostjm,&
        & spixel_scan_out%costjm(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_byte(ncid_primary,'lsflag',spixel_scan_out%vidlsflag,&
               & spixel_scan_out%lsflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!         CALL nc_write_L2_float(ncid_primary,'qcflag',spixel_scan_out%vidqcflag,&
 !              & spixel_scan_out%qcflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!MST         CALL nc_write_L2_short(ncid_primary,'qcflag',spixel_scan_out%vidqcflag,&
!MST              spixel_scan_out%qcflag(:,j),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_short(ncid_primary,'qcflag',spixel_scan_out%vidqcflag,&
              spixel_scan_out%qcflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!MST         CALL nc_write_L2_byte(ncid_primary,'illum',spixel_scan_out%vidillum,&
!MST              spixel_scan_out%illum(:,j),ixstart,ixstop,iystart,iystop,wo,ierr)
         CALL nc_write_L2_byte(ncid_primary,'illum',spixel_scan_out%vidillum,&
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
