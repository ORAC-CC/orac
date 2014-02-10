! Name: build_preproc_fields.f90
!
!
! Purpose:
! Grid and average the imager data to the preprocessing grid.
! 
! Description and Algorithm details:
! 1) Average imager angles over preprocessor grid cells.
! 2)
!
! Arguments:
! Name               Type In/Out/Both Description
! ------------------------------------------------------------------------------
! preproc_dims       struct both Summary of preprocessing grid definitions
! preproc_geo        struct both Summary of preprocessing geometry
! imager_geolocation struct both Summary of satellite grid definitions
! imager_angles      struct both Summary of satellite geometry
!
! History:
! 2012/02/24: MJ produces initial code version.
! 2012/04/19: GT Bug fix - relazi was being refereneced by idim
!                in both dimensions (rather idim for x and jdim for y)
! 2012/07/30: CP added in solazi
! 2012/08/24: MJ seperated filtering in lw and sw to avoid issues on night side
!                of orbits
! 2012/12/14: CP changed howy loop was set changed starty to startyi to loop
!                over a granule
! 2013/05/16: MJ changed filtering of angles wrt fill value: multi-view is
!                accounted for.
!
! $Id$
!
! Bugs:
! none known
!
  
subroutine build_preproc_fields(preproc_dims, preproc_geo, imager_geolocation, &
     imager_angles)

   use preproc_constants
   use imager_structures
   use preproc_structures

   implicit none

   type(preproc_dims_s)       :: preproc_dims
   type(preproc_geo_s)        :: preproc_geo
   type(imager_geolocation_s) :: imager_geolocation
   type(imager_angles_s)      :: imager_angles

   integer(kind=lint)         :: idim,jdim,maxi,maxj,lon_i,lat_j,iangle

   !imager resolution is always higher than preprocessing resolution
   !=>average imager properties to this coarser resolution grid.

   preproc_dims%counter_sw=0 
   preproc_dims%filter_array_sw=0 
   preproc_dims%counter_lw=0 
   preproc_dims%filter_array_lw=0  
   !average the properties to low resolution grid
   maxi=preproc_dims%xdim
   maxj=preproc_dims%ydim

   !loop over imager data
   do idim=imager_geolocation%startx,imager_geolocation%endx
      do jdim=1,imager_geolocation%ny

         imager_geolocation%uscan(idim,jdim)=idim
         imager_geolocation%vscan(idim,jdim)=jdim

         !look if geolocation is there, if not do nothing
         if (imager_geolocation%latitude(idim,jdim) .ge. real_fill_value .and. &
            imager_geolocation%longitude(idim,jdim) .ge. real_fill_value) then

            !find grid cell coordinates into which L1b pixel falls
            lon_i=floor((imager_geolocation%longitude(idim,jdim) + &
                 preproc_dims%lon_offset)*preproc_dims%dellon, kind=lint) + 1
            lat_j=floor((imager_geolocation%latitude(idim,jdim) + &
                 preproc_dims%lat_offset)*preproc_dims%dellat, kind=lint) + 1
            if (lon_i .gt. maxi) lon_i=1
            if (lat_j .gt. maxj) lat_j=maxj    
         else
            cycle
         endif

         if (all(imager_angles%satzen(idim,jdim,:) .gt. real_fill_value)) then
            preproc_geo%satza(lon_i,lat_j,:)=preproc_geo%satza(lon_i,lat_j,:)+ &
                 imager_angles%satzen(idim,jdim,:)

            ! indicate this is a preprocessing pixel which has valid values
            preproc_dims%filter_array_lw(lon_i,lat_j)=1

            ! count the number of L1 pixels which fall in this pixel
            preproc_dims%counter_lw(lon_i,lat_j)= &
                 preproc_dims%counter_lw(lon_i,lat_j)+1
         endif

         if (all(imager_angles%solzen(idim,jdim,:) .gt. real_fill_value) .and. &
             all(imager_angles%solazi(idim,jdim,:) .gt. real_fill_value) .and. &
             all(imager_angles%relazi(idim,jdim,:) .gt. real_fill_value)) then

            preproc_geo%solza(lon_i,lat_j,:)=preproc_geo%solza(lon_i,lat_j,:)+ &
                 imager_angles%solzen(idim,jdim,:)
            preproc_geo%relazi(lon_i,lat_j,:)= &
                 preproc_geo%relazi(lon_i,lat_j,:)+&
                 imager_angles%relazi(idim,jdim,:)
            preproc_geo%solazi(lon_i,lat_j,:)=&
                 preproc_geo%solazi(lon_i,lat_j,:)+&
                 imager_angles%solazi(idim,jdim,:)

            ! indicate this is a preprocessing pixel which has valid values
            preproc_dims%filter_array_sw(lon_i,lat_j)=1

            ! count the number of L1 pixels which fall in this pixel
            preproc_dims%counter_sw(lon_i,lat_j)= &
                 & preproc_dims%counter_sw(lon_i,lat_j)+1
         endif
      enddo
   enddo

   ! loop over preprocessor data i.e reduced resolution
   do idim=1,preproc_dims%xdim
      do jdim=1,preproc_dims%ydim
         if(preproc_dims%filter_array_lw(idim,jdim) .eq. 1) then
            ! if this is a good preprocessing pixel, calculate the average
            preproc_geo%satza(idim,jdim,:)=preproc_geo%satza(idim,jdim,:)/ &
                 preproc_dims%counter_lw(idim,jdim)
         else
            ! if not set fill value
            preproc_geo%satza(idim,jdim,:)=real_fill_value
         endif

         if(preproc_dims%filter_array_sw(idim,jdim) .eq. 1) then
            ! if this is a good preprocessing pixel, calculate the average
            preproc_geo%solza(idim,jdim,:)=preproc_geo%solza(idim,jdim,:)/ &
                 preproc_dims%counter_sw(idim,jdim)
            preproc_geo%relazi(idim,jdim,:)=preproc_geo%relazi(idim,jdim,:)/ &
                 preproc_dims%counter_sw(idim,jdim)
            preproc_geo%solazi(idim,jdim,:)=preproc_geo%solazi(idim,jdim,:)/ &
                 preproc_dims%counter_sw(idim,jdim)	
         else
            ! if not set fill value
            preproc_geo%solza(idim,jdim,:)=real_fill_value
            preproc_geo%relazi(idim,jdim,:)=real_fill_value
            preproc_geo%solazi(idim,jdim,:)=real_fill_value	
         endif
      enddo
   enddo

end subroutine build_preproc_fields
