! Name: allocate_surface_structures.F90
!
!
! Purpose:
! Allocate the array parts of the types defined in surface_structures.F90
! 
! Description and Algorithm details:
! 1) Allocate arrays to have appropriate size
! 2) Initialise to the appropriate fill value
!
! Arguments:
! Name               Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! surface            struct both Structure with arrays that need allocating.
! imager_geolocation struct both "
! channel_info       struct in   Structure giving the number of channels.
!
! History:
! 2012/05/01: GT First version
! 2012/07/29: CP inialised variables
! 2012/08/07: CP changed how arrays were formated to be consistent with msi
! 2012/08/22: MJ implements flexible x and y dimensions start and end indices
! 2012/12/13: CP changed ydimension to imager_geolocation%ny
! 2013/09/06: AP tidying
!
! $Id$
!
! Bugs:
! none known
!

subroutine allocate_surface_structures(surface,imager_geolocation,channel_info)

   use channel_structures
   use imager_structures
   use preproc_constants
   use preproc_structures
!  use surface_structures

   implicit none

   type(surface_s), intent(inout)         :: surface
   type(imager_geolocation_s), intent(in) :: imager_geolocation
   type(channel_info_s)                   :: channel_info


   allocate(surface%albedo_chan(channel_info%nchannels_sw))
   surface%albedo_chan=real_fill_value
   allocate(surface%emissivity_chan(channel_info%nchannels_lw))
   surface%emissivity_chan=real_fill_value
   allocate(surface%albedo(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,channel_info%nchannels_sw))
   surface%albedo=real_fill_value
   allocate(surface%emissivity(imager_geolocation%startx:imager_geolocation%endx,&
        1:imager_geolocation%ny,channel_info%nchannels_lw))
   surface%emissivity=real_fill_value

end subroutine allocate_surface_structures
