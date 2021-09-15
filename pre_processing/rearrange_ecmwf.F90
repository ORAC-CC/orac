!-------------------------------------------------------------------------------
! Name: rearrange_ecmwf.F90
!
! Purpose:
! Rearrange ecmwf data because grid is not consistent, i.e australia is in the
! centre of the map and 0 lat is at index 180. Implemented as a module called
! "rearrange" to create a generic routine.
!
! Description and Algorithm details:
! 1) Swap left and right halves.
! 2) Invert the y-axis.
!
! Arguments:
! Name       Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf      struct  Both   Summary of contents of ECMWF files
!
! History:
! 2013/03/06, CP: Original code
! 2013/03/07, CP: fixed latitudinal bug switch!
! 2013/03/19, GT: Commented debugging write statements
! 2013/11/01, GM: Cleaned up code and removed the use of several auxiliary arrays
! 2013/11/05, GT: Bug fix. Moved declaration of dim1 & dim2 to before they
!    are used in the definition of var.
! 2014/05/07, AP: Restructuring for smaller ECMWF structure.
! 2014/11/04, OS: Added skin temperature.
! 2015/11/17, OS: Added rearrangement of high resolution ERA-Interim data.
! 2018/07/26, AP: Switch to dynamic allocation to reduce stack requirements.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine rearrange_ecmwf(ecmwf)

   implicit none

   type(ecmwf_t), intent(inout) :: ecmwf

   integer                      :: date, ind, i
   real(kind=sreal), allocatable, dimension(:,:) :: u, v
   real(kind=sreal), allocatable, dimension(:,:) :: skint, snow_depth
   real(kind=sreal), allocatable, dimension(:,:) :: sea_ice_cover
   real(kind=sreal), allocatable, dimension(:)   :: lon,lat

   ! find dateline
   date=1
   do while (ecmwf%lon(date) .lt. 180.)
      date = date + 1
   end do
   ind = ecmwf%xdim + 1 - date

   ! Swap the left and right halfs into a temp array
   ! Wind fields are not contained in high resolution data
   allocate(u(ecmwf%xdim,ecmwf%ydim))
   u(1:ind,:)  = ecmwf%u10(date:,:)
   u(ind+1:,:) = ecmwf%u10(1:date-1,:)
 
   allocate(v(ecmwf%xdim,ecmwf%ydim))
   v(1:ind,:)  = ecmwf%v10(date:,:)
   v(ind+1:,:) = ecmwf%v10(1:date-1,:)

   allocate(skint(ecmwf%xdim,ecmwf%ydim))
   skint(1:ind,:)  = ecmwf%skin_temp(date:,:)
   skint(ind+1:,:) = ecmwf%skin_temp(1:date-1,:)

   allocate(snow_depth(ecmwf%xdim,ecmwf%ydim))
   snow_depth(1:ind,:) = ecmwf%snow_depth(date:,:)
   snow_depth(ind+1:,:)= ecmwf%snow_depth(1:date-1,:)

   allocate(sea_ice_cover(ecmwf%xdim,ecmwf%ydim))
   sea_ice_cover(1:ind,:)  = ecmwf%sea_ice_cover(date:,:)
   sea_ice_cover(ind+1:,:) = ecmwf%sea_ice_cover(1:date-1,:)

   allocate(lon(ecmwf%xdim))
   allocate(lat(ecmwf%ydim))
   lon(1:ind)  = ecmwf%lon(date:) - 360.
   lon(ind+1:) = ecmwf%lon(1:date-1)

   ecmwf%lon = lon

   ! flip in the y direction from the temp to the original
   do i=1,ecmwf%ydim
      ecmwf%u10(:,ecmwf%ydim+1-i) = u(:,i)
      ecmwf%v10(:,ecmwf%ydim+1-i) = v(:,i)
      ecmwf%skin_temp(:,ecmwf%ydim+1-i)     = skint(:,i)
      ecmwf%snow_depth(:,ecmwf%ydim+1-i)    = snow_depth(:,i)
      ecmwf%sea_ice_cover(:,ecmwf%ydim+1-i) = sea_ice_cover(:,i)
      lat(ecmwf%ydim+1-i)                   = ecmwf%lat(i)
   end do

   ecmwf%lon = lon
   ecmwf%lat = lat

   deallocate(u)
   deallocate(v)
   deallocate(skint)
   deallocate(snow_depth)
   deallocate(sea_ice_cover)
   deallocate(lon)
   deallocate(lat)

end subroutine rearrange_ecmwf
