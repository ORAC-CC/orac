!-------------------------------------------------------------------------------
! Name: deallocate_imager_structures.F90
!
! Purpose:
! Deallocate the array parts of the types defined in imager_structures.F90
!
! Description and Algorithm details:
! 1) Deallocate all fields passed.
!
! Arguments:
! Name                Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! imager_geolocation  struct both Structure with arrays that need deallocating.
! imager_angles       struct both "
! imager_flags        struct both "
! imager_time         struct both "
! imager_measurements struct both "
!
! History:
! 2012/01/13, MJ: produces draft code for MODIS L1b data
! 2012/02/03, MJ: adds uncertainty to measurements
! 2012/02/03, CP: deallocated solazi
! 2012/12/13, CP: deallocated uscan and vscan
! 2013/11/08, GM: added missing deallocate statements.
! 2014/09/17, CS: added deallocation statements for image_pavolonis,
!    imager_geolocation%usgs_dem, imager_flags%lusflag
! 2014/12/01, OS: added imager_pavolonis%emis_ch3b
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2015/07/03, OS: Added cldmask_uncertainty
! 2017/03/29, SP: Add new variable for tropopause cloud emissivity (ExtWork)
! 2017/06/21, OS: deallocated ann phase variables
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2018/11/05, SP: Add CAPE
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine deallocate_imager_structures(imager_geolocation, imager_angles, &
     imager_flags, imager_time, imager_measurements, imager_pavolonis, &
     imager_cloud)

   use preproc_constants_m

   implicit none

   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_flags_t),        intent(inout) :: imager_flags
   type(imager_time_t),         intent(inout) :: imager_time
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_pavolonis_t),    intent(inout) :: imager_pavolonis
   type(imager_cloud_t),        intent(inout) :: imager_cloud

   deallocate(imager_geolocation%latitude)
   deallocate(imager_geolocation%longitude)
   deallocate(imager_geolocation%dem)
   deallocate(imager_angles%solzen)
   deallocate(imager_angles%satzen)
   deallocate(imager_angles%satazi)
   deallocate(imager_angles%relazi)
   deallocate(imager_angles%solazi)
   deallocate(imager_flags%lusflag)
   deallocate(imager_flags%lsflag)
   deallocate(imager_flags%cflag)
   deallocate(imager_time%time)
   deallocate(imager_measurements%data)
   deallocate(imager_measurements%uncertainty)
   deallocate(imager_pavolonis%cldtype)
   deallocate(imager_pavolonis%sfctype)
   deallocate(imager_pavolonis%cldmask)
   deallocate(imager_pavolonis%cldmask_uncertainty)
   deallocate(imager_pavolonis%cccot_pre)
   deallocate(imager_pavolonis%ann_phase)
   deallocate(imager_pavolonis%ann_phase_uncertainty)
   deallocate(imager_pavolonis%cphcot)
   deallocate(imager_pavolonis%cirrus_quality)
   deallocate(imager_pavolonis%emis_ch3b)

#ifdef INCLUDE_SEVIRI_NEURALNET
   if (associated(imager_pavolonis%ctp_fg)) &
        deallocate(imager_pavolonis%ctp_fg)
   if (associated(imager_pavolonis%ctp_fg_unc)) &
        deallocate(imager_pavolonis%ctp_fg_unc)

   if (associated(imager_pavolonis%mlay_prob)) &
        deallocate(imager_pavolonis%mlay_prob)
   if (associated(imager_pavolonis%mlay_flag)) &
        deallocate(imager_pavolonis%mlay_flag)
   if (associated(imager_pavolonis%mlay_unc)) &
        deallocate(imager_pavolonis%mlay_unc)
#endif

   deallocate(imager_measurements%cal_gain)

#ifdef INCLUDE_SATWX
   deallocate(imager_cloud%cloud_emis)
   deallocate(imager_cloud%trop_p)
   deallocate(imager_cloud%trop_t)
   deallocate(imager_cloud%cape)
#endif

end subroutine deallocate_imager_structures
