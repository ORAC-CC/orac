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
!   (needed by surface reflectance routines)
! 2012/07/04, CP: removed nviews from data
! 2012/12/13, CP: changed ydimension to imager_geolocation%ny
! 2013/09/11, AP: Removed startyi, endye.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine allocate_imager_structures(imager_geolocation,imager_angles, &
     imager_flags,imager_time,imager_measurements,channel_info)

   use channel_structures
   use preproc_constants

   implicit none

   type(imager_geolocation_s),  intent(inout) :: imager_geolocation
   type(imager_angles_s),       intent(out)   :: imager_angles
   type(imager_flags_s),        intent(out)   :: imager_flags
   type(imager_time_s),         intent(out)   :: imager_time
   type(imager_measurements_s), intent(out)   :: imager_measurements
   type(channel_info_s),        intent(inout) :: channel_info

   allocate(imager_geolocation%latitude(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_geolocation%latitude=sreal_fill_value

   allocate(imager_geolocation%longitude(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_geolocation%longitude=sreal_fill_value

   allocate(imager_geolocation%uscan(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_geolocation%uscan=lint_fill_value

   allocate(imager_geolocation%vscan(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_geolocation%vscan=lint_fill_value

   allocate(imager_angles%solzen(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%solzen=sreal_fill_value

   allocate(imager_angles%satzen(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%satzen=sreal_fill_value

   allocate(imager_angles%solazi(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%solazi=sreal_fill_value

   allocate(imager_angles%relazi(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,imager_angles%nviews))
   imager_angles%relazi=sreal_fill_value

   allocate(imager_flags%lsflag(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_flags%lsflag=byte_fill_value

   allocate(imager_flags%cflag(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_flags%cflag=byte_fill_value

   allocate(imager_time%time(imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny))
   imager_time%time=dreal_fill_value

   allocate(imager_measurements%data(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,1:channel_info%nchannels_total))
   imager_measurements%data=sreal_fill_value

   allocate(imager_measurements%uncertainty(&
        imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny,1:channel_info%nchannels_total))
   imager_measurements%uncertainty=sreal_fill_value

end subroutine allocate_imager_structures
