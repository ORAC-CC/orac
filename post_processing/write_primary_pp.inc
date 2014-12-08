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
  !original version copied from ORAC main processor adapted by C. Poulsen
  ! to write out post processed files
  ! 2012/03/18 Caroline Poulsen modified to add cloud flag
  ! 2012/07/06 MJ extensively overhauls and restructures the code
  ! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
  ! 2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when
  !  building libraries.
  ! 2014/10/24 OS: added variables lusflag, cldtype, cloudmask, DEM
  !  (currently deactivated), and nisemask
  ! 2014/11/20 OS: added Pavolonis cloud phase variable
  ! 2014/11/25 CP added in cloud albedo 
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known
  
  
  
  CALL nc_write_L2_double_pp(ncid_primary,'time',spixel_scan_out%vidtime,&
       spixel_scan_out%time(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_float_pp(ncid_primary,'lon',spixel_scan_out%vidlon,&
       spixel_scan_out%lon(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)



  CALL nc_write_L2_float_pp(ncid_primary,'lat',spixel_scan_out%vidlat,&
       spixel_scan_out%lat(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


  !         do iviews=1,Ctrl%Ind%NViews
  input_num='1'
  !            write(input_num,"(i4)") 1 !one view
  input_dummy='satellite_zenith_view'//trim(adjustl(input_num))
  CALL nc_write_L2_float_pp(ncid_primary,trim(adjustl(input_dummy)),spixel_scan_out%vidsat_zen(1),&
       spixel_scan_out%sat_zen(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  input_dummy='solar_zenith_view'//trim(adjustl(input_num))
  CALL nc_write_L2_float_pp(ncid_primary,trim(adjustl(input_dummy)),spixel_scan_out%vidsol_zen(1),&
       spixel_scan_out%sol_zen(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  input_dummy='rel_azimuth_view'//trim(adjustl(input_num))
  CALL nc_write_L2_float_pp(ncid_primary,trim(adjustl(input_dummy)),spixel_scan_out%vidrel_azi(1),&
       spixel_scan_out%rel_azi(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  !         enddo

  CALL nc_write_L2_short_pp(ncid_primary,'cot',spixel_scan_out%vidcot,&
       spixel_scan_out%cot(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
  CALL nc_write_L2_short_pp(ncid_primary,'cot_uncertainty',spixel_scan_out%vidcoterror,&
       spixel_scan_out%cot_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'ref',spixel_scan_out%vidref,&
       spixel_scan_out%ref(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
  CALL nc_write_L2_short_pp(ncid_primary,'ref_uncertainty',spixel_scan_out%vidreferror,&
       spixel_scan_out%ref_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'ctp',spixel_scan_out%vidctp,&
       spixel_scan_out%ctp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
  CALL nc_write_L2_short_pp(ncid_primary,'ctp_uncertainty',spixel_scan_out%vidctperror,&
       spixel_scan_out%ctp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


  CALL nc_write_L2_short_pp(ncid_primary,'cc_total',spixel_scan_out%vidcct,&
       spixel_scan_out%cct(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
  CALL nc_write_L2_short_pp(ncid_primary,'cc_total_uncertainty', &
       & spixel_scan_out%vidccterror,spixel_scan_out%cct_error(:,:),ixstart, &
       & ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'cccot',spixel_scan_out%vidcccot,&
       spixel_scan_out%cccot(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'cccot_pre',spixel_scan_out%vidcccot_pre,&
       spixel_scan_out%cccot_pre(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'stemp',spixel_scan_out%vidstemp,&
       spixel_scan_out%stemp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'stemp_uncertainty',spixel_scan_out%vidstemperror,&
       spixel_scan_out%stemp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'cth',spixel_scan_out%vidcth,&
       spixel_scan_out%cth(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
  CALL nc_write_L2_short_pp(ncid_primary,'cth_uncertainty',spixel_scan_out%vidctherror,&
       spixel_scan_out%cth_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'ctt',spixel_scan_out%vidctt,&
       spixel_scan_out%ctt(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
  CALL nc_write_L2_short_pp(ncid_primary,'ctt_uncertainty',spixel_scan_out%vidctterror,&
       spixel_scan_out%ctt_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


  CALL nc_write_L2_short_pp(ncid_primary,'cwp',spixel_scan_out%vidcwp,&
       spixel_scan_out%cwp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
  CALL nc_write_L2_short_pp(ncid_primary,'cwp_uncertainty',spixel_scan_out%vidcwperror,&
       spixel_scan_out%cwp_error(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'convergence',spixel_scan_out%vidconvergence,&
       spixel_scan_out%convergence(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'niter',spixel_scan_out%vidniter,&
       spixel_scan_out%niter(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'phase',spixel_scan_out%vidphase,&
       spixel_scan_out%phase(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'phase_pavolonis', &
       spixel_scan_out%vidphase_pavolonis, spixel_scan_out%phase_pavolonis(:,:), &
       ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_float_pp(ncid_primary,'costja',spixel_scan_out%vidcostja,&
       spixel_scan_out%costja(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_float_pp(ncid_primary,'costjm',spixel_scan_out%vidcostjm,&
       spixel_scan_out%costjm(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  !MJCALL nc_write_L2_short(ncid_primary,'costja',spixel_scan_out%vidcostja,&
  !MJ     spixel_scan_out%costja(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  !MJCALL nc_write_L2_short(ncid_primary,'costjm',spixel_scan_out%vidcostjm,&
  !MJ     spixel_scan_out%costjm(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


  CALL nc_write_L2_byte_pp(ncid_primary,'lsflag',spixel_scan_out%vidlsflag,&
       spixel_scan_out%lsflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'lusflag',spixel_scan_out%vidlusflag,&
       spixel_scan_out%lusflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'cldtype',spixel_scan_out%vidcldtype,&
       spixel_scan_out%cldtype(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'cldmask',spixel_scan_out%vidcldmask,&
       spixel_scan_out%cldmask(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  !CALL nc_write_L2_short_pp(ncid_primary,'dem',spixel_scan_out%viddem,&
  !     spixel_scan_out%dem(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'nisemask',spixel_scan_out%vidnisemask,&
       spixel_scan_out%nisemask(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_short_pp(ncid_primary,'qcflag',spixel_scan_out%vidqcflag,&
       spixel_scan_out%qcflag(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

  CALL nc_write_L2_byte_pp(ncid_primary,'illum',spixel_scan_out%vidillum,&
       spixel_scan_out%illum(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


  CALL nc_write_L2_short_pp(ncid_primary,'cwp',spixel_scan_out%vidcwp,&
       spixel_scan_out%cwp(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!
!This is not a bug but this assumes 5 channel retrieval with 3.7 channel so only 2 solar channels)
!This should ideally be read in from the primary file somehow

   if (global_atts%sensor .eq. 'AATSR') then 
      solar_chan_id=(/ 2,3 /)
      endif

   if (global_atts%sensor .eq. 'MODIS') then 
      solar_chan_id=(/ 1,2 /)
   endif

   if (global_atts%sensor .eq. 'AVHRR') then 
      solar_chan_id=(/ 1,2 /)
   endif

   nchan_solar=2
   write(*,*)'solar_chanid',solar_chan_id


   do iinput=1,nchan_solar
 
      write(input_num,"(i4)") solar_chan_id(iinput)
 write(*,*)' cloud albedo',spixel_scan_out%cloud_albedo(:,:,iinput)
  
      input_dummy=trim(adjustl(input_num))

      write(*,*)'write_primary cloud albedo','cloud_albedo_in_channel_no_'//input_dummy

           CALL nc_write_L2_short_pp(ncid_primary,'cloud_albedo_in_channel_no_'//input_dummy,spixel_scan_out%vidcloud_albedo,&
       spixel_scan_out%cloud_albedo(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
           end do


  if(ierr .ne. 0 ) then 
     status=PrimaryFileWriteErr
     write(*,*) 'write_primary.inc: netcdf primary file write error:', status
     stop

  endif
