module correct_for_ice_snow_m

implicit none

contains

!-------------------------------------------------------------------------------
! Name: correct_for_ice_snow.F90
!
! Purpose:
! Opens sea/snow cover data (NSIDC NISE data at present) and applies a
! correction to the surface albedo.
!
! Description and algorithm details
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! assume_full_path      in  T: inputs are filenames; F: folder names
!                logic  
! nise_path      string in  Path to NSIDC NISE data file
! imager_geolocation    in   Geolocation data for satellite data
!                struct      (defined in imager_structures)
! preproc_dims   struct in  Preprocessing dimensions, including sw and
!                            lw channel counts
! surface        struct both Surface properties structure
! cyear          string in   Year, as a 4 character string.
! cmonth         string in   Month of year, as a 2 character string.
! cday           string in   Day of month, as a 2 character string.
! channel_info   struct in   Structure summarising the channels to be processed
!
! History:
! 30/04/2012, GT: Finished first version
! 15/05/2012, MJ: replaced
!       if (count(imager_geolocation%latitude .gt. 0)) north=1
!       if (count(imager_geolocation%latitude .le. 0)) south=1
!    with
!       if (any(imager_geolocation%latitude .gt. 0)) north=1
!       if (any(imager_geolocation%latitude .le. 0)) south=1
!    as sun compiler complained about it being not a logical expression in the
!   if statement.
! 26/06/2012, CP: added channel structure removed preproc_dims variables
! 04/07/2012, GT: Added (1) array index to pixel_snow and pixel_ice in
!   expressions. They are one element arrays (for compatibility with
!   interpol_bilinear) not scalars.
! 10/07/2012, GT: Moved reflectance correction in apply_ice_correction inside
!   the channel do loop to correctindexing problem
! 12/07/2012, GT: Bux fix to previous bug fix.
! 30/07/2012, CP: added in year month day input
! 2012/08/03, MJ: removes trailing "/" from path and explicitly
!   includes it in this subroutine.
! 2012/08/22, MJ: implements flexible x and y dimensions start and end indices
! 2012/09/14, GT: Changed convertion of fraction index numbers to integers from
!   int(x) to nint(x) (i.e. rounding), based on EASE grid documentation, and
!   added 1, so that indices run from 1-721, rather than 0-720. 
! 16/11/2012, CP: modified how nise_path_file called no
!   longer need year in path name
! 14/12/2012, CP: changed howy loop was set changed starty to startyi to loop
!   over a granule
! 25/02/2012, CP: changed name of ice file becsue it is different for 2020
! 2013/03/07, GT; Reverted change made by CP on 2012/11/16, and added code to
!   check that the MODIS file exists and is readable.
! 2013/04/08, GT/CP: fixed bug in which no snow and albedo information was being
!   read out because channel_info structure was not being passed though the
!   routine
! 2013/05/13, GT: Fixed bugs with the calculation of nise grid coordinates
! 2013/05/14, GT: Fixed bugs with the calculation of nise grid in southern
!   hemisphere
! 2013/05/17, GT: Added code do deal with missing data along coast lines
!    and at the poles themselves.
! 2013/05/20, CP: visual inspection of scenes showed that ice had an albedo
!   closer to that of snow than of bare ice so albedo was modified accordingly,
!   could be modified in the future
! 2013/05/23, GT: Small bug fix to coast line-check (was causing snow to be
!   flagged along coastlines without any snow).
! 2013/09/02, AP: Removed startyi, endye.
! 2013/10/02, CP: added fmonth so correct ice snow emissivity files are read 
!   in 2009/2010
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/05/26, MJ: Added "FAILED" to error output.
! 2014/06/20, GM: Handle case when imager_geolocation%latitude or
!   imager_geolocation%longitude is equal to fill_value.
! 2014/07/01, AP: New apply_ice_correction algorithm that attempts to avoid
!   returning albedo > 1
!
! $Id$
!
! Bugs:
! none known
!-------------------------------------------------------------------------------

subroutine correct_for_ice_snow(assume_full_path,nise_path,imager_geolocation, &
     preproc_dims,surface,cyear,cmonth,cday, &
     channel_info)

   use preproc_constants
   use preproc_structures
   use imager_structures
   use surface_structures
   use nise_m
   use channel_structures

   implicit none

   ! Arguments
   logical,                    intent(in)    :: assume_full_path
   character(len=300),         intent(in)    :: nise_path
   type(imager_geolocation_s), intent(in)    :: imager_geolocation
   type(preproc_dims_s),       intent(in)    :: preproc_dims
   type(surface_s),            intent(inout) :: surface
   character(len=datelength),  intent(in)    :: cyear,cmonth,cday
   type(channel_info_s),       intent(in)    :: channel_info

   ! Local variables
   real(kind=sreal), dimension(4)   :: snow_albedo, ice_albedo
   integer(kind=1)                  :: north=0, south=0
   integer(kind=4)                  :: stat,i,j,k,l,m,n,xi,yi,count
   real(kind=dreal)                 :: easex, easey
   type(nise_s)                     :: nise
   real(kind=sreal), dimension(2,2) :: nise_tmp
   real(kind=sreal), dimension(8)   :: nise_tmp2
   real                             :: fmonth
   character(len=300)               :: nise_path_file
   logical                          :: nise_file_exist
   character(len=7)                 :: nise_file_read

   ! Define the ice and snow albedo values
   ! Snow - from the ASTER spectral library
   ! Bare sea ice - from Brandt et al 2005 (0.67 & 0.87 microns); 
   ! Grenfell and Perovich 1984 (1.6 microns); assumed 0 reflectance
   ! (Emissivity = 1) at 3.7 microns.
   ! lambda         0.67   0.87   1.6     3.7
   snow_albedo = (/ 0.958, 0.868, 0.0364, 0.0 /)
   ice_albedo = (/ 0.958, 0.868, 0.0364, 0.0 /)
   !  ice_albedo  = (/ 0.497, 0.289, 0.070,  0.0 /)
   !  convert a string to a float
   read (cmonth,*) fmonth

   ! Do we need to load both Northern and Southern Hemisphere data?
   if (any(imager_geolocation%latitude .gt. 0)) north=1
   if (any(imager_geolocation%latitude .le. 0)) south=1

   ! Load ice/snow data
   if (assume_full_path) then
      nise_path_file = nise_path
   else
      if ((trim(adjustl(cyear)) .eq. '2010') .or. &
           (trim(adjustl(cyear)) .eq. '2009' .and. fmonth .gt. 8 )) then
         nise_path_file=trim(adjustl(nise_path))//'/'//'NISE_SSMISF17_'//&
              & trim(adjustl(cyear))//trim(adjustl(cmonth))//&
              & trim(adjustl(cday))//'.HDFEOS'
      else
         nise_path_file=trim(adjustl(nise_path))//'/'//'NISE_SSMIF13_'//&
              & trim(adjustl(cyear))//trim(adjustl(cmonth))//&
              & trim(adjustl(cday))//'.HDFEOS'
      endif
   endif
   write(*,*)'nise_path_file: ', trim(nise_path_file)

   ! Check that the defined file exists and is readable
   inquire(file=trim(nise_path_file), exist=nise_file_exist, &
        read=nise_file_read)
   if (.not.nise_file_exist) then
      write(*,*) 'FAILED: STOP: NISE ice/snow file does not exist'
      stop
   else if (trim(nise_file_read).eq.'NO') then
      write(*,*) 'FAILED: STOP: NISE ice/snow file exists but is not readable'
      stop
   end if


   stat = read_nsidc_nise(nise_path_file, nise, north, south)

   do i=imager_geolocation%startx,imager_geolocation%endx
      do j=1,imager_geolocation%ny

         if (imager_geolocation%latitude (i,j) .eq. real_fill_value .or. &
              imager_geolocation%longitude(i,j) .eq. real_fill_value) &
              cycle

         ! Northern and Southern hemispheres are handled separately:
         ! - NISE data provides them as separate gridded products
         ! - Sea ice is handled differently for the North and South
         if (imager_geolocation%latitude(i,j) .gt. 0) then
            ! The NSIDC provide simple arithmetic functions for converting
            ! lat-lon points into the EASE-Grid used by NISE

            easex = 2.0 * nise%north%REarth / nise%north%res *              &
                 cos(d2r*(imager_geolocation%longitude(i,j)-90.0)) *        &
                 sin(pi/4.0 - d2r*imager_geolocation%latitude(i,j) / 2.0) + &
                 nise%north%nx/2 + 1.0
            easey = 2.0 * nise%north%REarth / nise%north%res *              &
                 sin(d2r*(imager_geolocation%longitude(i,j)+90.0)) *        &
                 sin(pi/4.0 - d2r*imager_geolocation%latitude(i,j) / 2.0) + &
                 nise%north%ny/2 + 1.0

            ! Create integer indexes of the pixel "below" and "left" of this
            ! fractional index.
            xi = floor(easex)
            yi = floor(easey)
            ! Extract the NISE extent index for the four grid cells surrounding
            ! our location and convert to a floating point value

            nise_tmp(:,1) = (/ real(nise%north%extent(xi,yi)),      &
                 real(nise%north%extent(xi+1,yi)) /)
            nise_tmp(:,2) = (/ real(nise%north%extent(xi,yi+1)),    &
                 real(nise%north%extent(xi+1,yi+1)) /)

            ! Check for coastline or the region of missing data at the pole
            ! in the selected points and correct:
            ! * Assume the pole is always covered in ice
            ! * Interpolate the surrounding sea ice/snow cover for each coast
            !   point
            do l=0,1 
               do k=0,1
                  ! Check if the current NISE pixel is flagged as coast
                  ! and that it is not at the edge of the data array.
                  if ((nise_tmp(k+1,l+1) .eq. 252.0) .and.           &
                       (xi+k .gt. 1) .and.                           &
                       (xi+k .lt. nise%north%nx-1) .and.             &
                       (yi+l .gt. 1) .and.                           &
                       (yi+l .lt. nise%north%ny-1)) then
                     count = 0
                     ! Check the value of each neighbouring nise extent pixel
                     ! to the current coast one. If they aren't bad pixels
                     ! themselves, include them in the values to average.
                     do m=-1,1
                        do n=-1,1
                           if (nise%north%extent(xi+k+n,yi+l+m) .lt. 250.0) then
                              count = count+1
                              nise_tmp2(count) = &
                                   real(nise%north%extent(xi+k+n,yi+l+m))
                           endif
                        end do
                     end do
                     ! If we've found some valid pixels surrounding our
                     ! coastal value, replace the coast value with their
                     ! average
                     if (count .gt. 0) then
                        nise_tmp(k+1,l+1) = sum(nise_tmp2(1:count)) / real(count)
                        if (nise_tmp(k+1,l+1) .gt. 100) nise_tmp(k+1,l+1) = 101.0
                     end if
                  else if (nise_tmp(k+1,l+1) .eq. 253.0) then
                     ! If the data is marked as "253", it is a missing
                     ! polar value, which we can fairly safely assume to
                     ! be snow.
                     nise_tmp(k+1,l+1) = 102.0
                  end if
               end do
            end do

            call apply_ice_correction(real(easex-real(xi)), &
                 real(easey-real(yi)), nise_tmp, ice_albedo, snow_albedo, &
                 preproc_dims, surface%albedo(i,j,:),channel_info)

         else ! Repeat for the Southern Hemisphere

            easex = 2.0 * nise%south%REarth / nise%south%res *              &
                 cos(d2r*(imager_geolocation%longitude(i,j)-90.0)) *        &
                 sin(pi/4.0 + d2r*imager_geolocation%latitude(i,j) / 2.0) + &
                 nise%south%nx/2 + 1.0
            easey = 2.0 * nise%south%REarth / nise%south%res *              &
                 sin(d2r*(imager_geolocation%longitude(i,j)-90.0)) *        &
                 sin(pi/4.0 + d2r*imager_geolocation%latitude(i,j) / 2.0) + &
                 nise%south%ny/2 + 1.0

            ! Create integer indexes of the pixel "below" and "left" of this
            ! fractional index.
            xi = floor(easex)
            yi = floor(easey)
            ! Extract the NISE extent index for the four grid cells surrounding
            ! our location and convert to a floating point value

            nise_tmp(:,1) = (/ real(nise%south%extent(xi,yi)),      &
                 real(nise%south%extent(xi+1,yi)) /)
            nise_tmp(:,2) = (/ real(nise%south%extent(xi,yi+1)),    &
                 real(nise%south%extent(xi+1,yi+1)) /)

            ! Check for coastline or the region of missing data at the pole
            ! in the selected points and correct:
            ! * Assume the pole is always covered in snow
            ! * Interpolate the surrounding sea ice/snow cover for each coast
            !   point
            do l=0,1 
               do k=0,1
                  ! Check if the current NISE pixel is flagged as coast
                  ! and that it is not at the edge of the data array.
                  if ((nise_tmp(k+1,l+1) .eq. 252.0) .and.           &
                       (xi+k .gt. 1) .and.                           &
                       (xi+k .lt. nise%south%nx-1) .and.             &
                       (yi+l .gt. 1) .and.                           &
                       (yi+l .lt. nise%south%ny-1)) then
                     count = 0
                     ! Check the value of each neighbouring nise extent pixel
                     ! to the current coast one. If they aren't bad pixels
                     ! themselves, include them in the values to average.
                     do m=-1,1
                        do n=-1,1
                           if (nise%south%extent(xi+k+n,yi+l+m) .lt. 250.0) then
                              count = count+1
                              nise_tmp2(count) = &
                                   real(nise%south%extent(xi+k+n,yi+l+m))
                           endif
                        end do
                     end do
                     ! If we've found some valid pixels surrounding our
                     ! coastal value, replace the coast value with their
                     ! average
                     if (count .gt. 0) then
                        nise_tmp(k+1,l+1) = sum(nise_tmp2(1:count)) / real(count)
                        if (nise_tmp(k+1,l+1) .gt. 100) nise_tmp(k+1,l+1) = 101.0
                     end if
                  else if (nise_tmp(k+1,l+1) .eq. 253.0) then
                     ! If the data is marked as "253", it is a missing
                     ! polar value, which we can fairly safely assume to
                     ! be snow.
                     nise_tmp(k+1,l+1) = 102.0
                  end if
               end do
            end do

            call apply_ice_correction(real(easex-real(xi)),  &
                 real(easey-real(yi)), nise_tmp, ice_albedo, snow_albedo,  &
                 preproc_dims, surface%albedo(i,j,:), channel_info)
         endif
      end do
   end do
   ! Tidy up the nise structure created by the read_nsidc_snow call
   call deallocate_nise(nise)

end subroutine correct_for_ice_snow

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

subroutine apply_ice_correction(x, y, nise, ice_albedo, snow_albedo, &
     preproc_dims, pixel_ref,channel_info)

   use preproc_constants
   use preproc_structures
   use interpol
   use channel_structures

   implicit none

   real,                             intent(in)    :: x, y
   real,             dimension(2,2), intent(in)    :: nise
   real,             dimension(:),   intent(in)    :: ice_albedo, snow_albedo
   type(preproc_dims_s),             intent(in)    :: preproc_dims
   real(kind=sreal), dimension(:),   intent(inout) :: pixel_ref
   type(channel_info_s),             intent(in)    :: channel_info
   
   real,             dimension(2,2)                :: ice_frac
   real,             dimension(1)                  :: pixel_ice, pixel_snow
   integer,          dimension(:), allocatable     :: chanidx
   integer(kind=lint)                              :: i
   logical                                         :: snw_frac

   snw_frac=.false.

   ! Snow is denoted by a value of 102 or 103 in the NISE product
   !   where((nise .gt. 101) .and. (nise .lt. 105))
   !      snw_frac = 1.0
   !   elsewhere
   !      snw_frac = 0.0
   !   end where
   ! Note that interpol_bilinear expects array values for the output
   ! x and y coordinates, as well as the output function value
   !   call interpol_bilinear( (/ 0.0, 1.0 /), (/ 0.0, 1.0 /),  &
   !        snw_frac, (/x/),(/y/), pixel_snow )
   if (x.gt.0.5) then
      if (y.gt.0.5) then
         if (nise(2,2).eq.102 .or. nise(2,2).eq.103) snw_frac=.true.
      else
         if (nise(2,1).eq.102 .or. nise(2,2).eq.103) snw_frac=.true.
      end if
   else
      if (y.gt.0.5) then
         if (nise(1,2).eq.102 .or. nise(2,2).eq.103) snw_frac=.true.
      else
         if (nise(1,1).eq.102 .or. nise(2,2).eq.103) snw_frac=.true.
      end if
   end if

   ! Sea and permanent ice is handled more complexly, and is  denoted by 
   ! values of 1-101, with 1-100 denoting a percentage cover of sea ice and
   ! 101 denoting permanent ice. We set permanent ice to a fraction of 1.0,
   ! and convert the percentage cover to a fraction
   where(nise .eq. 101) 
      ice_frac = 1.0
   elsewhere
      ice_frac = nise / 100.
   end where
   where(nise .gt. 101) ice_frac = 0.0
   call interpol_bilinear( (/ 0.0, 1.0 /), (/ 0.0, 1.0 /), &
        ice_frac, (/x/), (/y/), pixel_ice )

   if (snw_frac) then
      ! snow adjacent pixel assumed completely snowy
      pixel_ref = snow_albedo
   else if (pixel_ice(1).eq.1.) then
      ! completely icy
      pixel_ref = ice_albedo
   else if (pixel_ice(1).gt.0.) then
      ! somewhat icy
      do i=1,channel_info%nchannels_sw
         if (pixel_ref(i) .ne. real_fill_value) pixel_ref(i) = &
              (1. - pixel_ice(1))*pixel_ref(i) + pixel_ice(1)*ice_albedo(i)
      end do
   end if

   ! Sort out the channel indexing.
   !   allocate(chanidx(channel_info%nchannels_sw))
   !   chanidx=-999.
   !   do i=1,channel_info%nchannels_sw
   !      chanidx(i) = channel_info%channel_ids_abs(i)
   ! Now, if the current pixel has a snow and/or ice fraction greater
   ! than zero, alter the surface albedo in accordance with the total
   ! fraction
   !      if ((pixel_ice(1) .gt. 0.0) .or. (pixel_snow(1) .gt. 0.0)) then
   !         pixel_ref(i) = (1.0 - pixel_ice(1) - pixel_snow(1)) *  &
   !              pixel_ref(i) + pixel_ice(1)*ice_albedo(chanidx(i)) + &
   !              pixel_snow(1)*snow_albedo(chanidx(i))
   !      end if
   !   end do

end subroutine apply_ice_correction

end module correct_for_ice_snow_m
