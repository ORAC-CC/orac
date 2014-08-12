!-------------------------------------------------------------------------------
! Name: netcdf_output_write_swath.F90
!
! Purpose:
! Write outputs of ORAC into already opened NCDF files.
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! imager_flags        struct  in  Summary of land/sea/ice flags.
! imager_angles       struct  in  Summary of satellite geometry.
! imager_geolocation  struct  in  Summary of pixel positions.
! imager_measurements struct  in  Summary of satellite imagery.
! imager_time         struct  in  Summary of pixel observation time.
! netcdf_info         struct  in  Summary of NCDF file properties.
! channel_info        struct  in  Summary of imager channel parameters.
! surface             struct  in  Summary of surface properties.
!
! History:
! 2012/06/01, MJ: created innitial file version.
! 2012/06/12, CP: bug fix to imager_time%time
! 2012/07/04, CP: removed nviews from data
! 2012/08/07, CP: changes 4 d array to 3d msi as no views selected
! 2012/08/07, CP: added in albedo write
! 2012/08/29, CP: added in solaz
! 2012/08/29, CP: changed starty to stayi and endy to endye
! 2013/05/23, GT: 3.7 micron albedo is now ouput in the alb file.
! 2013/09/03, AP: Removed startyi, endye.
! 2013/10/14, MJ: fixed bug with writing of albedo and emissivity.
! 2013/11/05, MJ: fixed bug with writing of albedo and emissivity (channel
!    numbers).
! 2013/11/05, GM: Fixed a copy-and-paste-and-forgot-to-change bug from the above
!    fix.
! 2014/05/26, GM: Some code clean up.
! 2014/08/10, GM: Changes related to new BRDF support.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_output_write_swath(imager_flags,imager_angles,imager_geolocation, &
   imager_measurements,imager_time,netcdf_info,channel_info,surface,include_full_brdf)

   use attribute_structures
   use channel_structures
   use imager_structures
   use netcdf
   use netcdf_structures
   use preproc_constants
   use surface_structures

   implicit none

   type(imager_flags_s),        intent(in) :: imager_flags
   type(imager_angles_s),       intent(in) :: imager_angles
   type(imager_geolocation_s),  intent(in) :: imager_geolocation
   type(imager_measurements_s), intent(in) :: imager_measurements
   type(imager_time_s),         intent(in) :: imager_time
   type(netcdf_info_s),         intent(in) :: netcdf_info
   type(channel_info_s),        intent(in) :: channel_info
   type(surface_s),             intent(in) :: surface
   logical,                     intent(in) :: include_full_brdf

   integer            :: ierr
   integer(kind=lint) :: start1d(1),counter1d(1),stride1d(1)
   integer(kind=lint) :: start2d(2),counter2d(2),stride2d(2)
   integer(kind=lint) :: start3d(3),counter3d(3),stride3d(3)

   integer(kind=lint)                            :: ichan,ic
   integer(kind=lint), allocatable, dimension(:) :: dummy_chan_vec1d


   ! everything has the same start and stride
   start1d = 1
   start2d = 1
   start3d = 1
   stride1d = 1
   stride2d = 1
   stride3d = 1


   ! config file

   start1d = 1
   stride1d = 1

   counter1d=channel_info%nchannels_total

   ierr = nf90_put_var(netcdf_info%ncid_config, netcdf_info%channelninid_config,&
        channel_info%channel_ids_instr(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi channels'

   ierr = nf90_put_var(netcdf_info%ncid_config, &
        netcdf_info%channelnabsid_config,&
        channel_info%channel_ids_abs(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi channels abs'

   ierr = nf90_put_var(netcdf_info%ncid_config, &
        netcdf_info%channelwlabsid_config,&
        channel_info%channel_wl_abs(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi wls'

   ierr = nf90_put_var(netcdf_info%ncid_config, &
         netcdf_info%channellwflag_config,&
        channel_info%channel_lw_flag(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi lw flag'

   ierr = nf90_put_var(netcdf_info%ncid_config, &
         netcdf_info%channelswflag_config,&
        channel_info%channel_sw_flag(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi sw flag'

   ierr = nf90_put_var(netcdf_info%ncid_config, &
         netcdf_info%channelprocflag_config,&
        channel_info%channel_proc_flag(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi proc flag'

   counter1d=channel_info%nchannels_sw

   allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if (channel_info%channel_sw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      end if
   end do
   ierr = nf90_put_var(netcdf_info%ncid_config,netcdf_info%channelnalbid_config,&
        dummy_chan_vec1d(1:channel_info%nchannels_sw),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write alb channels alb'
   deallocate(dummy_chan_vec1d)

   counter1d=channel_info%nchannels_lw
   allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if (channel_info%channel_lw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      end if
   end do
   ierr = nf90_put_var(netcdf_info%ncid_config, &
        netcdf_info%channelnemisid_config,&
        dummy_chan_vec1d(1:channel_info%nchannels_lw),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write alb channels emis'
   deallocate(dummy_chan_vec1d)
   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny


   ! alb file (albedo and emissivity)
   counter1d=channel_info%nchannels_sw

   allocate(dummy_chan_vec1d(channel_info%nchannels_sw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if (channel_info%channel_sw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      end if
   end do
   ierr = nf90_put_var(netcdf_info%ncid_alb,netcdf_info%channelnalbid,&
        dummy_chan_vec1d(1:channel_info%nchannels_sw),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write alb channels alb'
   deallocate(dummy_chan_vec1d)


   counter1d=channel_info%nchannels_lw
   allocate(dummy_chan_vec1d(channel_info%nchannels_lw))
   dummy_chan_vec1d=0_lint
   ic=1
   do ichan=1,channel_info%nchannels_total
      if (channel_info%channel_lw_flag(ichan) .eq. 1) then
         dummy_chan_vec1d(ic)=channel_info%channel_ids_abs(ichan)
         ic=ic+1
      end if
   end do
   ierr = nf90_put_var(netcdf_info%ncid_alb, netcdf_info%channelnemisid,&
        dummy_chan_vec1d(1:channel_info%nchannels_lw),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write alb channels emis'
   deallocate(dummy_chan_vec1d)


   counter3d(1) = imager_geolocation%nx
   counter3d(2) = imager_geolocation%ny
   counter3d(3) = channel_info%nchannels_sw

   ierr = nf90_put_var(netcdf_info%ncid_alb, netcdf_info%albid,&
        surface%albedo(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,1:channel_info%nchannels_sw),&
        start3d, counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'error: write alb'


   counter3d(1) = imager_geolocation%nx
   counter3d(2) = imager_geolocation%ny
   counter3d(3) = channel_info%nchannels_lw

   ierr = nf90_put_var(netcdf_info%ncid_alb, netcdf_info%emisid,&
        surface%emissivity(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,1:channel_info%nchannels_lw),&
        start3d,counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'error: write emiss'


   if (include_full_brdf) then
        counter3d(1) = imager_geolocation%nx
        counter3d(2) = imager_geolocation%ny
        counter3d(3) = channel_info%nchannels_sw

        ierr = nf90_put_var(netcdf_info%ncid_alb, netcdf_info%rho_0v_id,&
               surface%rho_0v(imager_geolocation%startx:imager_geolocation%endx,&
               1:imager_geolocation%ny,1:channel_info%nchannels_sw),&
               start3d,counter3d,stride3d)
        if (ierr.NE.NF90_NOERR) stop 'error: write rho_0v'

        ierr = nf90_put_var(netcdf_info%ncid_alb, netcdf_info%rho_0d_id,&
               surface%rho_0d(imager_geolocation%startx:imager_geolocation%endx,&
               1:imager_geolocation%ny,1:channel_info%nchannels_sw),&
               start3d,counter3d,stride3d)
        if (ierr.NE.NF90_NOERR) stop 'error: write rho_0d'

        ierr = nf90_put_var(netcdf_info%ncid_alb, netcdf_info%rho_dv_id,&
               surface%rho_dv(imager_geolocation%startx:imager_geolocation%endx,&
               1:imager_geolocation%ny,1:channel_info%nchannels_sw),&
               start3d,counter3d,stride3d)
        if (ierr.NE.NF90_NOERR) stop 'error: write rho_dv'

        ierr = nf90_put_var(netcdf_info%ncid_alb, netcdf_info%rho_dd_id,&
               surface%rho_dd(imager_geolocation%startx:imager_geolocation%endx,&
               1:imager_geolocation%ny,1:channel_info%nchannels_sw),&
               start3d,counter3d,stride3d)
        if (ierr.NE.NF90_NOERR) stop 'error: write rho_dd'
   end if


   ! clf file (cflag)
   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny

   ierr = nf90_put_var(netcdf_info%ncid_cf, netcdf_info%cfid,&
        imager_flags%cflag(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny),start2d,counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'error: write cflag'


   ! geo file (solzen, satzen, solaz, relazi)

   counter3d(1) = imager_geolocation%nx
   counter3d(2) = imager_geolocation%ny
   counter3d(3) = imager_angles%nviews

   ! solzen
   ierr = nf90_put_var(netcdf_info%ncid_geo, netcdf_info%solzenid,&
        imager_angles%solzen(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,1:imager_angles%nviews), &
        start3d,counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'error: write solzen'

   ! satzen
   ierr = nf90_put_var(netcdf_info%ncid_geo, netcdf_info%satzenid,&
        imager_angles%satzen(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,1:imager_angles%nviews), &
        start3d,counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'error: write satzen'

   ! solaz
   ierr = nf90_put_var(netcdf_info%ncid_geo, netcdf_info%solazid,&
        imager_angles%solazi(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,1:imager_angles%nviews), &
        start3d,counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'error: write solaz'

   ! relazi
   ierr = nf90_put_var(netcdf_info%ncid_geo, netcdf_info%relazid,&
        imager_angles%relazi(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,1:imager_angles%nviews), &
        start3d,counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'error: write relazi'


   ! loc file (lat, lon)

   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny

   ierr = nf90_put_var(netcdf_info%ncid_loc, netcdf_info%lonid,&
        imager_geolocation%longitude( &
        imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny),start2d,counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'err write lon'

   ierr = nf90_put_var(netcdf_info%ncid_loc, netcdf_info%latid,&
        imager_geolocation%latitude( &
        imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny),start2d,counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'error: write lat'


   ! lsf file (lsflag)
   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny

   ierr = nf90_put_var(netcdf_info%ncid_lsf, netcdf_info%lsfid,&
        imager_flags%lsflag(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny),start2d,counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'error: write lsflag'


   ! msi file (indexes, time, measurements)
   counter1d=channel_info%nchannels_total

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%channelninid,&
        channel_info%channel_ids_instr(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi channels'

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%channelnabsid,&
        channel_info%channel_ids_abs(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi channels abs'

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%channelwlabsid,&
        channel_info%channel_wl_abs(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi wls'

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%channellwflag,&
        channel_info%channel_lw_flag(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi lw flag'

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%channelswflag,&
        channel_info%channel_sw_flag(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi sw flag'

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%channelprocflag,&
        channel_info%channel_proc_flag(1:channel_info%nchannels_total),&
        start1d,counter1d,stride1d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi proc flag'


   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%timeid,&
        imager_time%time(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny),start2d,counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi'


   counter3d(1) = imager_geolocation%nx
   counter3d(2) = imager_geolocation%ny
   counter3d(3) = channel_info%nchannels_total

   ierr = nf90_put_var(netcdf_info%ncid_msi, netcdf_info%msid,&
        imager_measurements%data( &
        imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,1:channel_info%nchannels_total),&
        start3d,counter3d,stride3d)
   if (ierr.NE.NF90_NOERR) stop 'error: write msi'


   ! uv file, (uscan, vscan)
   counter2d(1) = imager_geolocation%nx
   counter2d(2) = imager_geolocation%ny

   ierr = nf90_put_var(netcdf_info%ncid_scan, netcdf_info%uscanid,&
        imager_geolocation%uscan( &
        imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny),start2d,counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'error: write u scan'

   ierr = nf90_put_var(netcdf_info%ncid_scan, netcdf_info%vscanid,&
        imager_geolocation%vscan( &
        imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny),start2d,counter2d,stride2d)
   if (ierr.NE.NF90_NOERR) stop 'error: write v scan'

end subroutine netcdf_output_write_swath
