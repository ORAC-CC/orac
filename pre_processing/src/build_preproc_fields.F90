! Name: build_preproc_fields.f90
!
!
! Purpose:
! Grid and average the imager data to the preprocessing grid.
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
! 2012/02/24: Matthias Jerg produces initial code version.
! 2012/04/19: Gareth Thomas. Bug fix - relazi was being refereneced by idim
!             in both dimensions (rather idim for x and jdim for y)
! 2012/07/30: C. Poulsen added in solazi
!2012/08/24 MJ seperated filtering in lw and sw to avoid issues on night side of orbits
!14/12/2012 Cp changed howy loop was set changed starty to startyi to loop over
!            a granule
!20130516 MJ changed filtering of angles wrt fill value: multi-view is accounted for.
! $Id$
!
! Bugs:
!
!none known
  
  
subroutine build_preproc_fields(preproc_dims,preproc_geo,imager_geolocation,imager_angles)

  use preproc_constants

  use imager_structures

  use preproc_structures

  implicit none

  type(preproc_dims_s) :: preproc_dims
  type(preproc_geo_s) :: preproc_geo

  type(imager_geolocation_s) :: imager_geolocation
  type(imager_angles_s) :: imager_angles

  integer(kind=lint) :: idim,jdim,maxi,maxj,lon_i,lat_j,iangle

  !imager resolution is always higher than preprocessing resolution
  !=>average imager properties to this coarser resolution grid.

  preproc_dims%counter_sw=0 
  preproc_dims%filter_array_sw=0 
  preproc_dims%counter_lw=0 
  preproc_dims%filter_array_lw=0  
  !average the properties to low resolution grid
  maxi=preproc_dims%xdim_pre!2*preproc_dims%lon_offset*preproc_dims%dellon
  maxj=preproc_dims%ydim_pre!2*preproc_dims%lat_offset*preproc_dims%dellat
!
!loop over imager data
!  
  do idim=imager_geolocation%startx,imager_geolocation%endx
     do jdim=1,imager_geolocation%ny

        imager_geolocation%uscan(idim,jdim)=idim
        imager_geolocation%vscan(idim,jdim)=jdim

        !look if geolocation is there, if not do nothing and go to next L1b pixel
        if(imager_geolocation%latitude(idim,jdim) .ge.  &
              & (real_fill_value+1.0) .and. &
             & imager_geolocation%longitude(idim,jdim) .ge. &
              & (real_fill_value+1.0)) then
           
           !find grid cell coordinates into which L1b pixel falls
           lon_i=int((imager_geolocation%longitude(idim,jdim)+ &
                 & preproc_dims%lon_offset)*preproc_dims%dellon)+1
           lat_j=int((imager_geolocation%latitude(idim,jdim)+ &
                 & preproc_dims%lat_offset)*preproc_dims%dellat)+1
           if(lon_i .gt. maxi) lon_i=1
           if(lat_j .gt. maxj) lat_j=maxj    
        else
           lon_i=long_int_fill_value
           lat_j=long_int_fill_value
           cycle
        endif


        !MJ ORG if((imager_angles%satzen(idim,jdim,1) .gt. real_fill_value)) then  
        if(all(imager_angles%satzen(idim,jdim,:) .gt. real_fill_value)) then
           do iangle=1,imager_angles%nviews
              
              preproc_geo%satza(lon_i,lat_j,iangle)=&
                   & preproc_geo%satza(lon_i,lat_j,iangle)+ &
                    & imager_angles%satzen(idim,jdim,iangle)

           enddo

           !set a "1" if this is a preprocessing pixel which has valid values on it
           preproc_dims%filter_array_lw(lon_i,lat_j)=1

           !count the number of L1 pixels which fall into this preprocessing pixel
           preproc_dims%counter_lw(lon_i,lat_j)= &
                preproc_dims%counter_lw(lon_i,lat_j)+1

        endif

        if(all(imager_angles%solzen(idim,jdim,:) .gt. real_fill_value) .and. &
             & all(imager_angles%solazi(idim,jdim,:) .gt. real_fill_value) .and. &
             & all(imager_angles%relazi(idim,jdim,:) .gt. real_fill_value)) then

           !MJ ORG if((imager_angles%solzen(idim,jdim,1) .gt. real_fill_value) .and. &
           !MJ ORG & (imager_angles%solazi(idim,jdim,1) .gt. real_fill_value) .and. &
           !MJ ORG & (imager_angles%relazi(idim,jdim,1) .gt. real_fill_value)) then

           do iangle=1,imager_angles%nviews

              preproc_geo%solza(lon_i,lat_j,iangle)=&
                   & preproc_geo%solza(lon_i,lat_j,iangle)+ &
                    & imager_angles%solzen(idim,jdim,iangle)

              preproc_geo%relazi(lon_i,lat_j,iangle)=&
                   & preproc_geo%relazi(lon_i,lat_j,iangle)+ &
                    & imager_angles%relazi(idim,jdim,iangle)

              preproc_geo%solazi(lon_i,lat_j,iangle)=&
                   & preproc_geo%solazi(lon_i,lat_j,iangle)+ &
                    & imager_angles%solazi(idim,jdim,iangle)
              
           enddo

           !set a "1" if this is a preprocessing pixel which has valid values on it
           preproc_dims%filter_array_sw(lon_i,lat_j)=1

           !count the number of L1 pixels which fall into this preprocessing pixel
           preproc_dims%counter_sw(lon_i,lat_j)= &
                 & preproc_dims%counter_sw(lon_i,lat_j)+1

        endif


     enddo
  enddo

!
!loop over preprocessor data i.e reduced resolution
!

  do idim=1,preproc_dims%xdim_pre
     do jdim=1,preproc_dims%ydim_pre
        !if this is a good preprocessing pixel do the actual average
        if(preproc_dims%filter_array_lw(idim,jdim) .eq. 1) then

           !LW
           do iangle=1,imager_angles%nviews

              preproc_geo%satza(idim,jdim,iangle)= &
                   &  preproc_geo%satza(idim,jdim,iangle)/preproc_dims%counter_lw(idim,jdim)

           enddo
           
           !if not set fill value
        else

           preproc_geo%satza(idim,jdim,:)=real_fill_value

        endif


        !if this is a good preprocessing pixel do the actual average
        if(preproc_dims%filter_array_sw(idim,jdim) .eq. 1) then

           do iangle=1,imager_angles%nviews

              preproc_geo%solza(idim,jdim,iangle)= &
                  &   preproc_geo%solza(idim,jdim,iangle)/preproc_dims%counter_sw(idim,jdim)
              preproc_geo%relazi(idim,jdim,iangle)= &
                  &   preproc_geo%relazi(idim,jdim,iangle)/preproc_dims%counter_sw(idim,jdim)
              preproc_geo%solazi(idim,jdim,iangle)= &
                  &   preproc_geo%solazi(idim,jdim,iangle)/preproc_dims%counter_sw(idim,jdim)	

           enddo
           
           !if not set fill value
        else

           preproc_geo%solza(idim,jdim,:)=real_fill_value
           preproc_geo%relazi(idim,jdim,:)=real_fill_value
           preproc_geo%solazi(idim,jdim,:)=real_fill_value	

        endif



!!$        !if this is a good preprocessing pixel do the actual average
!!$        if(preproc_dims%filter_array(idim,jdim) .eq. 1) then
!!$
!!$           !write(*,*) 'relazi4',idim,jdim
!!$
!!$           do iangle=1,imager_angles%nviews
!!$
!!$              preproc_geo%satza(idim,jdim,iangle)=preproc_geo%satza(idim,jdim,iangle)/preproc_dims%counter(idim,jdim)
!!$              preproc_geo%solza(idim,jdim,iangle)=preproc_geo%solza(idim,jdim,iangle)/preproc_dims%counter(idim,jdim)
!!$              preproc_geo%relazi(idim,jdim,iangle)= preproc_geo%relazi(idim,jdim,iangle)/preproc_dims%counter(idim,jdim)
!!$              preproc_geo%solazi(idim,jdim,iangle)= preproc_geo%solazi(idim,jdim,iangle)/preproc_dims%counter(idim,jdim)	
!!$
!!$           enddo
!!$           
!!$           !if not set fill value
!!$        else
!!$
!!$           preproc_geo%satza(idim,jdim,:)=real_fill_value
!!$           preproc_geo%solza(idim,jdim,:)=real_fill_value
!!$           preproc_geo%relazi(idim,jdim,:)=real_fill_value
!!$           preproc_geo%solazi(idim,jdim,:)=real_fill_value	
!!$
!!$        endif
!!$

     enddo
  enddo


!
!interpolate preprocessor data onto a slightly larger grid
!





end subroutine build_preproc_fields
