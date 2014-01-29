! Name: write_swath_to_netcdf.F90
!
!
! Purpose:
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
! 2012/06/01: MJ created innitial file version.
! 2012/06/12: CP bug fix to imager_time%time
! 2012/07/04: CP removed nviews from data
! 2012/08/07: CP changes 4 d array to 3d msi as no views selected
! 2012/08/07: CP added in albedo write
! 2012/08/29: CP added in solaz
! 2012/08/29: CP changed starty to stayi and endy to endye
! 2013/05/23: GT 3.7 micron albedo is now ouput in the alb file.
! 2013/09/03: AP Removed startyi, endye.
! 2013/10/14: MJ fixed bug with writing of albedo and emissivity.
! 2013/11/05: MJ fixed bug with writing of albedo and emissivity (channel numbers).
! 2013/11/05: GM Fixed a copy-and-paste-and-forgot-to-change bug from the above
!                fix.
!
! $Id$
!
subroutine write_swath_to_netcdf(imager_flags,imager_angles, &
     & imager_geolocation,imager_measurements,imager_time,&
     & netcdf_info,channel_info,surface)

   use netcdf
   use preproc_constants
   use attribute_structures
   use imager_structures
   use surface_structures
   use netcdf_structures
   use channel_structures

   implicit none

   type(imager_angles_s)       :: imager_angles
   type(imager_geolocation_s)  :: imager_geolocation
   type(imager_flags_s)        :: imager_flags
   type(imager_time_s)         :: imager_time
   type(netcdf_info_s)         :: netcdf_info
   type(channel_info_s)        :: channel_info
   type(surface_s)             :: surface
   type(imager_measurements_s) :: imager_measurements
   integer                     :: ierr
   integer(kind=lint)          :: start1d(1),counter1d(1),stride1d(1),start2d(2),&
                                & counter2d(2),stride2d(2),start3d(3),counter3d(3),&
                                & stride3d(3)

   integer(kind=lint), allocatable, dimension(:) :: dummy_chan_vec1d
   integer(kind=lint) :: ichan,ic

   ! everything has the same start and stride
   start1d = 1
   start2d = 1
   start3d = 1
   stride1d = 1
   stride2d = 1
   stride3d = 1

   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny

   !write lat/lon
   ierr = NF90_PUT_VAR(netcdf_info%ncid_loc, netcdf_info%lonid,&
        & imager_geolocation%longitude(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny), start2d, counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) then
      write(*,*) 'err write lon', ierr
      stop 
   endif

   ierr = NF90_PUT_VAR(netcdf_info%ncid_loc, netcdf_info%latid,&
        & imager_geolocation%latitude(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny), start2d, counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'err write lat'

   !write cflag
   ierr = NF90_PUT_VAR(netcdf_info%ncid_cf, netcdf_info%cfid,&
        & imager_flags%cflag(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny), start2d, counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'err write cflag'

   !write lsflag
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lsf, netcdf_info%lsfid,&
        & imager_flags%lsflag(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny), start2d, counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'err write lsflag'

   !write scans
   ierr = NF90_PUT_VAR(netcdf_info%ncid_scan, netcdf_info%uscanid,&
        & imager_geolocation%uscan(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny), start2d, counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'err write u scan'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_scan, netcdf_info%vscanid,&
        & imager_geolocation%vscan(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny), start2d, counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'err write v scan'

   counter3d(1) = imager_geolocation%nx
   counter3d(2) = imager_geolocation%ny
   counter3d(3) = imager_angles%nviews

   !solzen
   ierr = NF90_PUT_VAR(netcdf_info%ncid_geo, netcdf_info%solzenid,&
        & imager_angles%solzen(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny,1:imager_angles%nviews),start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'err write solzen'

   !satzen
   ierr = NF90_PUT_VAR(netcdf_info%ncid_geo, netcdf_info%satzenid,&
        & imager_angles%satzen(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny,1:imager_angles%nviews),start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'err write satzen'

   !solaz
   ierr = NF90_PUT_VAR(netcdf_info%ncid_geo, netcdf_info%solazid,&
        & imager_angles%solazi(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny,1:imager_angles%nviews),start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'err write solaz'

   !realzi
   ierr = NF90_PUT_VAR(netcdf_info%ncid_geo, netcdf_info%relazid,&
        & imager_angles%relazi(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny,1:imager_angles%nviews),start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'err write relazi'

   counter1d=channel_info%nchannels_total

   !write msi
   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%channelninid,&
        & channel_info%channel_ids_instr(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi channels'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%channelnabsid,&
        & channel_info%channel_ids_abs(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi channels abs'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%channelwlabsid,&
        & channel_info%channel_wl_abs(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi wls'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%channellwflag,&
        & channel_info%channel_lw_flag(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi lw flag'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%channelswflag,&
        & channel_info%channel_sw_flag(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi sw flag'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%channelprocflag,&
        & channel_info%channel_proc_flag(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi proc flag'

   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny

   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%timeid,&
        & imager_time%time(imager_geolocation%startx:imager_geolocation%endx,1:imager_geolocation%ny),&
        & start2d, counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi'

   counter3d(1) = imager_geolocation%nx
   counter3d(2) = imager_geolocation%ny
   counter3d(3) = channel_info%nchannels_total

   ierr = NF90_PUT_VAR(netcdf_info%ncid_msi, netcdf_info%msid,&
        & imager_measurements%data(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny,1:channel_info%nchannels_total),&
        & start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi'

   ! write albedo and emissivity
   counter1d=channel_info%nchannels_sw

   allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if(channel_info%channel_sw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      endif
   enddo
!MJ OLD   ierr = NF90_PUT_VAR(netcdf_info%ncid_alb, netcdf_info%albid,&
   ierr = NF90_PUT_VAR(netcdf_info%ncid_alb,  netcdf_info%channelnalbid,&
        !MJ OLD & channel_info%channel_ids_abs(1:channel_info%nchannels_sw),&
        & dummy_chan_vec1d(1:channel_info%nchannels_sw),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write alb channels alb'
   deallocate(dummy_chan_vec1d)


   counter1d=channel_info%nchannels_lw
   allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if(channel_info%channel_lw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      endif
   enddo
!MJOLD  ierr = NF90_PUT_VAR(netcdf_info%ncid_alb, netcdf_info%emisid,&
   ierr = NF90_PUT_VAR(netcdf_info%ncid_alb, netcdf_info%channelnemisid,&
        !MJ OLD& channel_info%channel_ids_abs(1:channel_info%nchannels_lw),&
        & dummy_chan_vec1d(1:channel_info%nchannels_lw),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write alb channels emis'
   deallocate(dummy_chan_vec1d)

   counter3d(1) = imager_geolocation%nx
   counter3d(2) = imager_geolocation%ny
   counter3d(3) = channel_info%nchannels_sw

   ierr = NF90_PUT_VAR(netcdf_info%ncid_alb, netcdf_info%albid,&
        & surface%albedo(imager_geolocation%startx:imager_geolocation%endx,&
!MJ OLD        & 1:imager_geolocation%ny,1:channel_info%nchannels_sw-1),&
        & 1:imager_geolocation%ny,1:channel_info%nchannels_sw),&
        & start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'err write alb'

   counter3d(3) = channel_info%nchannels_lw
   ierr = NF90_PUT_VAR(netcdf_info%ncid_alb, netcdf_info%emisid,&
        & surface%emissivity(imager_geolocation%startx:imager_geolocation%endx,&
        & 1:imager_geolocation%ny,1:channel_info%nchannels_lw),&
        & start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'err write emiss'

!--------------------
!--------------------
!   write config file
!--------------------
!--------------------
   start1d = 1
   stride1d = 1
   counter1d=channel_info%nchannels_total

   ierr = NF90_PUT_VAR(netcdf_info%ncid_config, netcdf_info%channelninid_config,&
        & channel_info%channel_ids_instr(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi channels'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_config, netcdf_info%channelnabsid_config,&
        & channel_info%channel_ids_abs(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi channels abs'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_config, netcdf_info%channelwlabsid_config,&
        & channel_info%channel_wl_abs(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi wls'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_config, netcdf_info%channellwflag_config,&
        & channel_info%channel_lw_flag(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi lw flag'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_config, netcdf_info%channelswflag_config,&
        & channel_info%channel_sw_flag(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi sw flag'

   ierr = NF90_PUT_VAR(netcdf_info%ncid_config, netcdf_info%channelprocflag_config,&
        & channel_info%channel_proc_flag(1:channel_info%nchannels_total),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write msi proc flag'

   counter1d=channel_info%nchannels_sw

   allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if(channel_info%channel_sw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      endif
   enddo
!MJ OLD   ierr = NF90_PUT_VAR(netcdf_info%ncid_alb, netcdf_info%albid,&
   ierr = NF90_PUT_VAR(netcdf_info%ncid_config,  netcdf_info%channelnalbid_config,&
        !MJ OLD & channel_info%channel_ids_abs(1:channel_info%nchannels_sw),&
        & dummy_chan_vec1d(1:channel_info%nchannels_sw),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write alb channels alb'
   deallocate(dummy_chan_vec1d)


   counter1d=channel_info%nchannels_lw
   allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if(channel_info%channel_lw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      endif
   enddo
!MJOLD  ierr = NF90_PUT_VAR(netcdf_info%ncid_alb, netcdf_info%emisid,&
   ierr = NF90_PUT_VAR(netcdf_info%ncid_config, netcdf_info%channelnemisid_config,&
        !MJ OLD& channel_info%channel_ids_abs(1:channel_info%nchannels_lw),&
        & dummy_chan_vec1d(1:channel_info%nchannels_lw),&
        & start1d, counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'err write alb channels emis'
   deallocate(dummy_chan_vec1d)

   

end subroutine write_swath_to_netcdf
