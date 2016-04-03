!-------------------------------------------------------------------------------
! Name: allocate_surface_structures.F90
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
! imager_geolocation struct both Structure giving the imager dimensions.
! channel_info       struct in   Structure giving the number of channels.
!
! History:
! 2012/05/01, GT: First version
! 2012/07/29, CP: initialised variables
! 2012/08/07, CP: changed how arrays were formatted to be consistent with msi
! 2012/08/22, MJ: implements flexible x and y dimensions start and end indices
! 2012/12/13, CP: changed ydimension to imager_geolocation%ny
! 2013/09/06, AP: tidying
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/09/17, CS: Added surface%nise_mask
! 2014/12/02, GM: Remove unused surface%albedo_chan and surface%emissivity_chan.
! 2016/03/31, GM: Changes to support processing only SW or only LW channels.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine allocate_surface_structures(surface,imager_geolocation,channel_info, &
                                       include_full_brdf)

   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   type(surface_t),            intent(out) :: surface
   type(imager_geolocation_t), intent(in)  :: imager_geolocation
   type(channel_info_t),       intent(in)  :: channel_info
   logical,                    intent(in)  :: include_full_brdf

   allocate(surface%nise_mask(&
            imager_geolocation%startx:imager_geolocation%endx, &
            1:imager_geolocation%ny))
   surface%nise_mask=byte_fill_value

   if (channel_info%nchannels_sw .ne. 0) then
      allocate(surface%albedo(imager_geolocation%startx:imager_geolocation%endx, &
               1:imager_geolocation%ny,channel_info%nchannels_sw))
      surface%albedo=sreal_fill_value

      if (include_full_brdf) then
         allocate(surface%rho_0v(imager_geolocation%startx:imager_geolocation%endx,&
                  1:imager_geolocation%ny,channel_info%nchannels_sw))
         surface%rho_0v=sreal_fill_value

         allocate(surface%rho_0d(imager_geolocation%startx:imager_geolocation%endx,&
                  1:imager_geolocation%ny,channel_info%nchannels_sw))
         surface%rho_0d=sreal_fill_value

         allocate(surface%rho_dv(imager_geolocation%startx:imager_geolocation%endx,&
                  1:imager_geolocation%ny,channel_info%nchannels_sw))
         surface%rho_dv=sreal_fill_value

         allocate(surface%rho_dd(imager_geolocation%startx:imager_geolocation%endx,&
                  1:imager_geolocation%ny,channel_info%nchannels_sw))
         surface%rho_dd=sreal_fill_value
      end if
   end if

   if (channel_info%nchannels_lw .ne. 0) then
      allocate(surface%emissivity( &
               imager_geolocation%startx:imager_geolocation%endx, &
               1:imager_geolocation%ny,channel_info%nchannels_lw))
      surface%emissivity=sreal_fill_value
   end if

end subroutine allocate_surface_structures
