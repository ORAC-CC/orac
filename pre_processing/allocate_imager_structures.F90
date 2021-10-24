!-------------------------------------------------------------------------------
! Name: allocate_imager_structures.F90
!
! Purpose:
! Allocate the array parts of the types defined in imager_structures.F90
!
! Description and Algorithm details:
! 1) Allocate arrays to have appropriate size
! 2) Initialise to the appropriate fill value
!
! Arguments:
! Name                Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! imager_geolocation  struct both Structure with arrays that need allocating.
! imager_angles       struct both "
! imager_flags        struct both "
! imager_time         struct both "
! imager_measurements struct both "
! channel_info        struct in   Structure giving the number of channels.
!
! History:
! 2011/12/19, MJ: produces draft code for MODIS L1b data
! 2012/02/03, MJ: adds uncertainty to measurements
! 2012/04/24, GT: Added solar azimuth angle to the imager_angles structure
!    (needed by surface reflectance routines)
! 2012/07/04, CP: removed nviews from data
! 2012/12/13, CP: changed ydimension to imager_geolocation%ny
! 2013/09/11, AP: Removed startyi, endye.
! 2014/09/17, CS: Added
!    imager_pavolonis%CLDTYPE/CLDMASK/CCCOT/SFCTYPE/SUNGLINT_MASK,
!    imager_geolocation%USGS_DEM, and imager_flags%LUSFLAG
! 2014/12/01, OS: added imager_pavolonis%emis_ch3b
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2015/07/02, OS: added allocation of cldmask_uncertainty
! 2016/04/09, SP: Added multiple views
! 2017/03/29, SP: Add new variable for tropopause cloud emissivity (ExtWork)
! 2017/06/21, OS: allocated ann phase variables; imager_angles are now inout
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2018/11/05, SP: Add CAPE
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine allocate_imager_structures(imager_geolocation,imager_angles, &
     imager_flags,imager_time,imager_measurements,imager_pavolonis, &
     imager_cloud, channel_info)


   use channel_structures_m
   use preproc_constants_m

   implicit none

   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_flags_t),        intent(out)   :: imager_flags
   type(imager_time_t),         intent(out)   :: imager_time
   type(imager_measurements_t), intent(out)   :: imager_measurements
   type(imager_pavolonis_t),    intent(out)   :: imager_pavolonis
   type(imager_cloud_t),        intent(out)   :: imager_cloud
   type(channel_info_t),        intent(in)    :: channel_info

   allocate(imager_measurements%data( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,1:channel_info%nchannels_total))
   imager_measurements%data=sreal_fill_value

   allocate(imager_measurements%uncertainty( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,1:channel_info%nchannels_total))
   imager_measurements%uncertainty=sreal_fill_value

   allocate(imager_measurements%cal_gain(1:channel_info%nchannels_total))
   imager_measurements%cal_gain=sreal_fill_value

   allocate(imager_geolocation%latitude( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_geolocation%latitude=sreal_fill_value

   allocate(imager_geolocation%longitude( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_geolocation%longitude=sreal_fill_value

   allocate(imager_geolocation%dem( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_geolocation%dem=lint_fill_value

   allocate(imager_angles%solzen( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%solzen=sreal_fill_value

   allocate(imager_angles%satzen( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%satzen=sreal_fill_value

   allocate(imager_angles%solazi( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%solazi=sreal_fill_value

   allocate(imager_angles%satazi( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%satazi=sreal_fill_value

   allocate(imager_angles%relazi( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%relazi=sreal_fill_value

   allocate(imager_flags%lusflag( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_flags%lusflag=byte_fill_value

   allocate(imager_flags%lsflag( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_flags%lsflag=byte_fill_value

   allocate(imager_flags%cflag( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_flags%cflag=byte_fill_value

   allocate(imager_time%time( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_time%time=dreal_fill_value

   allocate(imager_pavolonis%sfctype( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_pavolonis%sfctype=sint_fill_value

   allocate(imager_pavolonis%cldtype( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%cldtype=byte_fill_value

   allocate(imager_pavolonis%cldmask( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%cldmask=byte_fill_value

   allocate(imager_pavolonis%cldmask_uncertainty( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%cldmask_uncertainty=sreal_fill_value

   allocate(imager_pavolonis%cccot_pre( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%cccot_pre=sreal_fill_value

   allocate(imager_pavolonis%ann_phase( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%ann_phase=byte_fill_value

   allocate(imager_pavolonis%ann_phase_uncertainty( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%ann_phase_uncertainty=sreal_fill_value

   allocate(imager_pavolonis%cphcot( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%cphcot=sreal_fill_value

   allocate(imager_pavolonis%cirrus_quality( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%cirrus_quality=byte_fill_value

   allocate(imager_pavolonis%emis_ch3b( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_pavolonis%emis_ch3b=sreal_fill_value

#ifdef INCLUDE_SATWX
   allocate(imager_cloud%cloud_emis( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,3))
   imager_cloud%cloud_emis=sreal_fill_value

   allocate(imager_cloud%trop_t( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_cloud%trop_t=sreal_fill_value

   allocate(imager_cloud%trop_p( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_cloud%trop_p=sreal_fill_value

   allocate(imager_cloud%cape( &
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_cloud%cape=sreal_fill_value
#endif

end subroutine allocate_imager_structures
