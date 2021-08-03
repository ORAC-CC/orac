!-------------------------------------------------------------------------------
! Name: correct_for_ice_snow.F90
!
! Purpose:
! Opens sea/snow cover data (NSIDC NISE data at present) and applies a
! correction to the surface albedo.
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! nise_path      string in   Path to NSIDC NISE data file
! imager_geolocation    in   Geolocation data for satellite data
!                struct      (defined in imager_structures)
! preproc_dims   struct in   Preprocessing dimensions, including sw and
!                            lw channel counts
! surface        struct both Surface properties structure
! cyear          string in   Year, as a 4 character string.
! cmonth         string in   Month of year, as a 2 character string.
! cday           string in   Day of month, as a 2 character string.
! channel_info   struct in   Structure summarising the channels to be processed
! assume_full_path      in   T: inputs are filenames; F: folder names
! source_atts    struct in   Source attributes
!                logic
! verbose        logic  in   T: print status information; F: don't
!
! History:
! 2012/04/30, GT: Finished first version
! 2012/05/15, MJ: replaced
!       if (count(imager_geolocation%latitude .gt. 0)) north=1
!       if (count(imager_geolocation%latitude .le. 0)) south=1
!    with
!       if (any(imager_geolocation%latitude .gt. 0)) north=1
!       if (any(imager_geolocation%latitude .le. 0)) south=1
!    as sun compiler complained about it being not a logical expression in the
!    if statement.
! 2012/06/26, CP: added channel structure removed preproc_dims variables
! 2012/07/04, GT: Added (1) array index to pixel_snow and pixel_ice in
!    expressions. They are one element arrays (for compatibility with
!    interpol_bilinear) not scalars.
! 2012/07/10, GT: Moved reflectance correction in apply_ice_correction inside
!    the channel do loop to correct indexing problem
! 2012/07/12, GT: Bux fix to previous bug fix.
! 2012/07/30, CP: added in year month day input
! 2012/08/03, MJ: removes trailing "/" from path and explicitly includes it in
!    this subroutine.
! 2012/08/22, MJ: implements flexible x and y dimensions start and end indices
! 2012/09/14, GT: Changed conversion of fraction index numbers to integers from
!    int(x) to nint(x) (i.e. rounding), based on EASE grid documentation, and
!    added 1, so that indices run from 1-721, rather than 0-720.
! 2012/11/16, CP: modified how nise_path_file called no longer need year in path
!    name
! 2012/12/14, CP: changed howy loop was set changed starty to startyi to loop
!    over a granule
! 2012/02/25, CP: changed name of ice file because it is different for 2020
! 2013/03/07, GT; Reverted change made by CP on 2012/11/16, and added code to
!    check that the MODIS file exists and is readable.
! 2013/04/08, GT/CP: fixed bug in which no snow and albedo information was being
!    read out because channel_info structure was not being passed though the
!    routine
! 2013/05/13, GT: Fixed bugs with the calculation of nise grid coordinates
! 2013/05/14, GT: Fixed bugs with the calculation of nise grid in southern
!    hemisphere
! 2013/05/17, GT: Added code do deal with missing data along coast lines and at
!    the poles themselves.
! 2013/05/20, CP: visual inspection of scenes showed that ice had an albedo
!    closer to that of snow than of bare ice so albedo was modified accordingly,
!    could be modified in the future
! 2013/05/23, GT: Small bug fix to coast line-check (was causing snow to be
!    flagged along coastlines without any snow).
! 2013/09/02, AP: Removed startyi, endye.
! 2013/10/02, CP: added fmonth so correct ice snow emissivity files are read
!    in 2009/2010
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/05/26, MJ: Added "FAILED" to error output.
! 2014/06/20, GM: Handle case when imager_geolocation%latitude or
!    imager_geolocation%longitude is equal to fill_value.
! 2014/07/01, AP: New apply_ice_correction algorithm that attempts to avoid
!    returning albedo > 1
! 2014/08/05, AP: Explicit bilinear interpolation, rather than function call.
!    Removing old code.
! 2014/09/17, CS: nise_mask added in surface_structures.F90, i.e.
!    surface%NISE_MASK(i,j) needed in cloud_typing_pavolonis.F90 for correcting
!    the land use map (currently USGS)
! 2014/12/01, CP: Added source attributes.
! 2014/12/16, GM: Added support for 2011 to 2014.
! 2015/02/04, MS+OS: Implemented correction for ice/snow based on ERA-Interim
!    data in SR correct_for_ice_snow_ecmwf
! 2015/03/04, GM: Changes related to supporting channels in arbitrary order.
! 2015/04/10, GM: Fixed the use of snow/ice albedo for the BRDF parameters.
! 2016/02/18, OS: ECMWF snow/ice mask now corrected by USGS land/sea mask
! 2016/02/23, OS: previous commit on ECMWF snow/ice mask was incomplete
! 2018/10/01, SP: Introduce a more comprehensive snow albedo dataset
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module correct_for_ice_snow_m

implicit none

contains

! This subroutine will calculate a snow albedo for a given wavelength via
! interpolation between the closest two suitable wavelengths in the database.
! Values are taken from the ASTER spectral library and are a combination of three
! snow types: Fine, Medium and Course. Here we simply average all three.
! If the requested wavelength is outside the range supported by this function then
! a warning is displayed and a reflectance value of 0.0 is supplied.
! Acceptable wavelength range: 350 nm -> 14,000 nm.
real function get_snow_albedo(wvl)

   implicit none

   real, intent(in) :: wvl

   integer          :: prevp, nextp, i
   real             :: slo, wvldif1, wvldif2
   real, parameter  :: start_wvl = 0.35
   real, parameter  :: end_wvl = 13.9
   real, parameter  :: snow_wvl(40) = (/ &
        0.35,  0.40,  0.45,  0.50,  0.60, 0.65, 0.70, 0.75, &
        0.80,  0.90,  1.00,  1.10,  1.20, 1.30, 1.40, 1.50, 1.60, &
        1.70,  1.80,  1.90,  2.00,  2.10, 2.20, 2.30, 2.40, 2.50, &
        2.60,  2.70,  2.80,  2.90,  3.00, 3.10, 3.20, 3.30, 3.70, &
        3.80, 10.91, 11.92, 12.90, 13.90/)

   real, parameter  :: snow_alb(40) = (/ &
        0.97, 0.98, 0.99, 0.98, 0.98, 0.96, 0.95, 0.93, 0.90, &
        0.84, 0.72, 0.73, 0.54, 0.47, 0.42, 0.04, 0.07, 0.14, &
        0.20, 0.07, 0.01, 0.03, 0.11, 0.08, 0.04, 0.03, 0.03, &
        0.01, 0.00, 0.01, 0.02, 0.04, 0.03, 0.02, 0.02, 0.01, &
        0.01, 0.02, 0.03, 0.03/)

   if (wvl .lt. start_wvl) then
      write(*,*) "WARNING: Cannot interpolate snow albedo, wavelength of ", wvl, &
           "is below minimum acceptable wavelength of ", start_wvl
      get_snow_albedo = 0.0
      return
   end if
   if (wvl .gt. end_wvl) then
      write(*,*) "WARNING: Cannot interpolate snow albedo, wavelength of ", wvl, &
           "is above maximum acceptable wavelength of ", end_wvl
      get_snow_albedo = 0.0
      return
   end if
   prevp = 1
   nextp = 2
   do i = 1, 39
      if (snow_wvl(i) .le. wvl .and. snow_wvl(i+1) .ge. wvl) then
         prevp = i
         nextp = i+1
      end if
   end do

   wvldif1 = wvl - snow_wvl(prevp)
   wvldif2 = snow_wvl(nextp) - snow_wvl(prevp)

   slo = (snow_alb(nextp) - snow_alb(prevp))/wvldif2

   get_snow_albedo = wvldif1 * slo + snow_alb(prevp)

end function get_snow_albedo

! This subroutine is the same as above, but for ice albedo.
! Acceptable wavelength range: 350 nm -> 14,000 nm.
real function get_ice_albedo(wvl)

   implicit none

   real, intent(in) :: wvl

   integer          :: prevp, nextp, i
   real             :: slo, wvldif1, wvldif2
   real, parameter  :: start_wvl = 0.35
   real, parameter  :: end_wvl = 13.9
   real, parameter  :: snow_wvl(40) = (/ &
        0.35,  0.40,  0.45,  0.50,  0.60, 0.65, 0.70, 0.75, &
        0.80,  0.90,  1.00,  1.10,  1.20, 1.30, 1.40, 1.50, 1.60, &
        1.70,  1.80,  1.90,  2.00,  2.10, 2.20, 2.30, 2.40, 2.50, &
        2.60,  2.70,  2.80,  2.90,  3.00, 3.10, 3.20, 3.30, 3.70, &
        3.80, 10.91, 11.92, 12.90, 13.90/)

   real, parameter  :: snow_alb(40) = (/ &
        0.97, 0.98, 0.99, 0.98, 0.98, 0.96, 0.95, 0.93, 0.90, &
        0.84, 0.72, 0.73, 0.54, 0.47, 0.42, 0.04, 0.07, 0.14, &
        0.20, 0.07, 0.01, 0.03, 0.11, 0.08, 0.04, 0.03, 0.03, &
        0.01, 0.00, 0.01, 0.02, 0.04, 0.03, 0.02, 0.02, 0.01, &
        0.01, 0.02, 0.03, 0.03/)

   if (wvl .lt. start_wvl) then
      write(*,*) "WARNING: Cannot interpolate ice albedo, wavelength of ", wvl, &
           "is below minimum acceptable wavelength of ", start_wvl
      get_ice_albedo = 0.0
      return
   end if
   if (wvl .gt. end_wvl) then
      write(*,*) "WARNING: Cannot interpolate ice albedo, wavelength of ", wvl, &
           "is above maximum acceptable wavelength of ", end_wvl
      get_ice_albedo = 0.0
      return
   end if
   prevp = 1
   nextp = 2
   do i = 1, 39
      if (snow_wvl(i) .le. wvl .and. snow_wvl(i+1) .ge. wvl) then
         prevp = i
         nextp = i+1
      end if
   end do

   wvldif1 = wvl - snow_wvl(prevp)
   wvldif2 = snow_wvl(nextp) - snow_wvl(prevp)

   slo = (snow_alb(nextp) - snow_alb(prevp))/wvldif2

   get_ice_albedo = wvldif1 * slo + snow_alb(prevp)

end function get_ice_albedo

subroutine correct_for_ice_snow(nise_path, imager_geolocation, surface, cyear, &
      cmonth, cday, channel_info, assume_full_path, include_full_brdf, source_atts, &
      verbose)

   use channel_structures_m
   use imager_structures_m
   use nsidc_nise_m
   use preproc_constants_m
   use preproc_structures_m
   use source_attributes_m
   use surface_structures_m

   implicit none

   ! Arguments
   character(len=*),           intent(in)    :: nise_path
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(surface_t),            intent(inout) :: surface
   character(len=*),           intent(in)    :: cyear, cmonth, cday
   type(channel_info_t),       intent(in)    :: channel_info
   logical,                    intent(in)    :: assume_full_path
   logical,                    intent(in)    :: include_full_brdf
   type(source_attributes_t),  intent(inout) :: source_atts
   logical,                    intent(in)    :: verbose

   ! Local variables
   real(kind=sreal), dimension(4)   :: snow_albedo, ice_albedo
   integer(kind=1)                  :: north = 0, south = 0
   integer(kind=4)                  :: stat, i,j, k,l, m,n, xi, yi, count
   real(kind=dreal)                 :: easex, easey
   type(nise_t)                     :: nise
   real(kind=sreal), dimension(2,2) :: nise_tmp
   real(kind=sreal), dimension(8)   :: nise_tmp2
   integer                          :: iyear
   integer                          :: imonth
   character(len=path_length)       :: nise_path_file
   logical                          :: nise_file_exist
   character(len=7)                 :: nise_file_read
   logical                          :: applied_flag

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering correct_for_ice_snow()'

   if (verbose) write(*,*) 'nise_path: ',        trim(nise_path)
   if (verbose) write(*,*) 'cyear: ',            trim(cyear)
   if (verbose) write(*,*) 'cmonth: ',           trim(cmonth)
   if (verbose) write(*,*) 'cday: ',             trim(cday)
   if (verbose) write(*,*) 'assume_full_path: ', assume_full_path

   ! Define the ice and snow albedo values
   ! Snow - from the ASTER spectral library
   ! Bare sea ice - from Brandt et al 2005 (0.67 & 0.87 microns);
   ! Grenfell and Perovich 1984 (1.6 microns); assumed 0 reflectance
   ! (Emissivity = 1) at 3.7 microns.
   ! lambda         0.67   0.87   1.6     3.7
   snow_albedo = (/ 0.958, 0.868, 0.0364, 0.0 /)
!  ice_albedo  = (/ 0.497, 0.289, 0.070,  0.0 /)
   ice_albedo  = (/ 0.958, 0.868, 0.0364, 0.0 /)

   read (cyear,*) iyear
   read (cmonth,*) imonth

   ! Do we need to load both Northern and Southern Hemisphere data?
   if (any(imager_geolocation%latitude .gt. 0)) north = 1
   if (any(imager_geolocation%latitude .le. 0)) south = 1

   ! Load ice/snow data
   if (assume_full_path) then
      nise_path_file = nise_path
   else
      if ((iyear .le. 2008) .or. (iyear .eq. 2009 .and. imonth .le. 8)) then
         nise_path_file = trim(adjustl(nise_path))//'/'//'NISE_SSMIF13_'// &
                          trim(adjustl(cyear))//trim(adjustl(cmonth))// &
                          trim(adjustl(cday))//'.HDFEOS'
      else
         nise_path_file = trim(adjustl(nise_path))//'/'//'NISE_SSMISF17_'// &
                          trim(adjustl(cyear))//trim(adjustl(cmonth))// &
                          trim(adjustl(cday))//'.HDFEOS'
      end if
   end if
   if (verbose) write(*,*)'nise_path_file: ', trim(nise_path_file)

   source_atts%snow_file = trim(nise_path_file)
   source_atts%sea_ice_file = trim(nise_path_file)

   ! Check that the defined file exists and is readable
   inquire(file=trim(nise_path_file), exist=nise_file_exist, &
        read=nise_file_read)
   if (.not.nise_file_exist) then
      write(*,*) 'ERROR: correct_for_ice_snow(): NISE ice/snow file does ' // &
           'not exist: ', trim(nise_path_file)
      stop error_stop_code
   else if (trim(nise_file_read).eq.'NO') then
      write(*,*) 'ERROR: correct_for_ice_snow(): NISE ice/snow file exists ' // &
           'but is not readable: ', trim(nise_path_file)
      stop error_stop_code
   end if

   stat = read_nsidc_nise(nise_path_file, nise, north, south, verbose)

   do i = imager_geolocation%startx, imager_geolocation%endx
      do j = 1, imager_geolocation%ny

         if (imager_geolocation%latitude (i,j) .eq. sreal_fill_value .or. &
              imager_geolocation%longitude(i,j) .eq. sreal_fill_value) &
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

            nise_tmp(:,1) = (/ real(nise%north%extent(xi,yi)),   &
                 real(nise%north%extent(xi+1,yi)) /)
            nise_tmp(:,2) = (/ real(nise%north%extent(xi,yi+1)), &
                 real(nise%north%extent(xi+1,yi+1)) /)

            ! Check for coastline or the region of missing data at the pole
            ! in the selected points and correct:
            ! * Assume the pole is always covered in ice
            ! * Interpolate the surrounding sea ice/snow cover for each coast
            !   point
            do l = 0, 1
               do k = 0, 1
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
                     do m=-1, 1
                        do n=-1, 1
                           if (nise%north%extent(xi+k+n,yi+l+m) .lt. 250.0) then
                              count = count+1
                              nise_tmp2(count) = &
                                   real(nise%north%extent(xi+k+n,yi+l+m))
                           end if
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
                 surface%albedo(i,j,:), channel_info, surface%nise_mask(i,j), &
                 applied_flag)

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

            nise_tmp(:,1) = (/ real(nise%south%extent(xi,yi)),   &
                 real(nise%south%extent(xi+1,yi)) /)
            nise_tmp(:,2) = (/ real(nise%south%extent(xi,yi+1)), &
                 real(nise%south%extent(xi+1,yi+1)) /)

            ! Check for coastline or the region of missing data at the pole
            ! in the selected points and correct:
            ! * Assume the pole is always covered in snow
            ! * Interpolate the surrounding sea ice/snow cover for each coast
            !   point
            do l = 0, 1
               do k = 0, 1
                  ! Check if the current NISE pixel is flagged as coast
                  ! and that it is not at the edge of the data array.
                  if ((nise_tmp(k+1,l+1) .eq. 252.0) .and. &
                       (xi+k .gt. 1) .and.                 &
                       (xi+k .lt. nise%south%nx-1) .and.   &
                       (yi+l .gt. 1) .and.                 &
                       (yi+l .lt. nise%south%ny-1)) then
                     count = 0
                     ! Check the value of each neighbouring nise extent pixel
                     ! to the current coast one. If they aren't bad pixels
                     ! themselves, include them in the values to average.
                     do m=-1, 1
                        do n=-1, 1
                           if (nise%south%extent(xi+k+n,yi+l+m) .lt. 250.0) then
                              count = count+1
                              nise_tmp2(count) = &
                                   real(nise%south%extent(xi+k+n,yi+l+m))
                           end if
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
                 surface%albedo(i,j,:), channel_info, surface%nise_mask(i,j), &
                 applied_flag)
         end if

         if (include_full_brdf .and. applied_flag) then
            surface%rho_0v(i,j,:) = surface%albedo(i,j,:)
            surface%rho_0d(i,j,:) = surface%albedo(i,j,:)
            surface%rho_dv(i,j,:) = surface%albedo(i,j,:)
            surface%rho_dd(i,j,:) = surface%albedo(i,j,:)
         end if
      end do
   end do
   ! Tidy up the nise structure created by the read_nsidc_snow call
   call deallocate_nise(nise)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving correct_for_ice_snow()'

end subroutine correct_for_ice_snow

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

subroutine apply_ice_correction(x, y, nise, ice_albedo, snow_albedo, &
     pixel_ref, channel_info, nise_mask_flag, applied_flag)

   use channel_structures_m
   use constants_cloud_typing_pavolonis_m
   use interpol_m
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   real,                             intent(in)    :: x, y
   real,             dimension(2,2), intent(in)    :: nise
   real,             dimension(:),   intent(in)    :: ice_albedo, snow_albedo
   real(kind=sreal), dimension(:),   intent(inout) :: pixel_ref
   type(channel_info_t),             intent(in)    :: channel_info
   integer(kind=byte),               intent(out)   :: nise_mask_flag
   logical,                          intent(out)   :: applied_flag

   integer(kind=lint)               :: i
   real,             dimension(2,2) :: ice_frac
   real,             dimension(1)   :: pixel_ice
   logical                          :: snw_frac

   snw_frac=.false.

   ! Snow is denoted by a value of 102 or 103 in the NISE product
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

   ! Sea and permanent ice is handled more complexly, and is denoted by
   ! values of 1-101, with 1-100 denoting a percentage cover of sea ice and
   ! 101 denoting permanent ice. We set permanent ice to a fraction of 1.0,
   ! and convert the percentage cover to a fraction
   where(nise .eq. 101)
      ice_frac = 1.0
   else where
      ice_frac = nise / 100.
   end where
   where(nise .gt. 101) ice_frac = 0.0
   ! bilinear interpolation
   pixel_ice = (1.-x)*(1.-y)*ice_frac(1,1) +     x *(1.-y)*ice_frac(2,1) + &
               (1.-x)*    y *ice_frac(1,2) +     x *    y *ice_frac(2,2)

   applied_flag = .false.

   if (snw_frac) then
      ! snow adjacent pixel assumed completely snowy
      applied_flag = .true.
      do i = 1, channel_info%nchannels_sw
         pixel_ref(i) = snow_albedo(channel_info%map_ids_abs_to_snow_and_ice(i))
      end do
      nise_mask_flag = YES
   else if (pixel_ice(1).eq.1.) then
      ! completely icy
      applied_flag = .true.
      do i = 1, channel_info%nchannels_sw
         pixel_ref(i) = ice_albedo (channel_info%map_ids_abs_to_snow_and_ice(i))
      end do
      nise_mask_flag = YES
   else if (pixel_ice(1).gt.0.) then
      ! somewhat icy
      applied_flag = .true.
      do i = 1, channel_info%nchannels_sw
         if (pixel_ref(i) .ne. sreal_fill_value) pixel_ref(i) = &
              (1. - pixel_ice(1))*pixel_ref(i) + pixel_ice(1) * &
              ice_albedo(channel_info%map_ids_abs_to_snow_and_ice(i))
      end do
      if (pixel_ice(1) .ge. 0.15) then
         nise_mask_flag = YES
      else
         nise_mask_flag = NO
      end if
   else
      nise_mask_flag = NO
   end if

end subroutine apply_ice_correction

!-------------------------------------------------------------------------------
!------------------------------------------------------------------------------

subroutine correct_for_ice_snow_nwp(nwp_path, imager_geolocation, &
     channel_info, imager_flags, preproc_dims, preproc_prtm, surface, &
     include_full_brdf, source_atts, verbose)

   use channel_structures_m
   use constants_cloud_typing_pavolonis_m
   use imager_structures_m
   use preproc_constants_m
   use preproc_structures_m
   use source_attributes_m
   use surface_structures_m

   implicit none

   ! Arguments
   character(len=*),           intent(in)    :: nwp_path
   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(channel_info_t),       intent(in)    :: channel_info
   type(imager_flags_t),       intent(in)    :: imager_flags
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(preproc_prtm_t),       intent(in)    :: preproc_prtm
   logical,                    intent(in)    :: include_full_brdf
   type(source_attributes_t),  intent(inout) :: source_atts
   logical,                    intent(in)    :: verbose

   type(surface_t),            intent(inout) :: surface

   ! Local variables
   logical                        :: flag
   integer(kind=4)                :: i, j,lon_i, lat_j
   real(kind=sreal), dimension(:), allocatable :: tmp_albedo, tmp_snow, tmp_ice
   real(kind=sreal), dimension(4) :: snow_albedo, ice_albedo
   real(kind=sreal)               :: snow_threshold, ice_threshold

   allocate(tmp_albedo(channel_info%nchannels_sw))
   allocate(tmp_snow(channel_info%nchannels_sw))
   allocate(tmp_ice(channel_info%nchannels_sw))

   ! lambda         0.67   0.87   1.6     3.7
   snow_albedo = (/ 0.958, 0.868, 0.0364, 0.0 /)
   ice_albedo = (/ 0.958, 0.868, 0.0364, 0.0 /)

   snow_threshold = 0.01 ! I belive this is 1cm
   ice_threshold = 0.15 ! I believe this is 15%

   source_atts%snow_file = trim(nwp_path)
   source_atts%sea_ice_file = trim(nwp_path)

   do i = 1, channel_info%nchannels_total
      if (channel_info%map_ids_abs_to_snow_and_ice(i) .le. 0) cycle
      tmp_snow(i) = get_snow_albedo(channel_info%channel_wl_abs(i))
      if (verbose) write(*,*) "Calculated snow albedo for", &
           channel_info%channel_wl_abs(i), "micron is", tmp_snow(i)
      tmp_ice(i) = ice_albedo(channel_info%map_ids_abs_to_snow_and_ice(i))
   end do

   do j = 1, imager_geolocation%ny
      do i = imager_geolocation%startx, imager_geolocation%endx

         ! if geolocation isn't there, do nothing
         if (imager_geolocation%latitude(i,j) .eq. sreal_fill_value .or. &
              imager_geolocation%longitude(i,j) .eq. sreal_fill_value) cycle

         ! find grid cell coordinates into which L1b pixel falls
         lon_i = floor((imager_geolocation%longitude(i,j) + &
              preproc_dims%lon_offset)*preproc_dims%dellon, kind=lint) + 1
         lat_j = floor((imager_geolocation%latitude(i,j) + &
              preproc_dims%lat_offset)*preproc_dims%dellat, kind=lint) + 1

         if (lon_i .lt. preproc_dims%min_lon) lon_i = preproc_dims%min_lon
         if (lat_j .lt. preproc_dims%min_lat) lat_j = preproc_dims%min_lat
         if (lon_i .gt. preproc_dims%max_lon) lon_i = preproc_dims%max_lon
         if (lat_j .gt. preproc_dims%max_lat) lat_j = preproc_dims%max_lat

         tmp_albedo = surface%albedo(i,j,:)

         if ( &
              ((preproc_prtm%snow_depth(lon_i,lat_j)    .gt. snow_threshold) .and. &
               (imager_flags%lsflag(i,j)         .eq. 1_byte)) .or. &
              ((preproc_prtm%snow_depth(lon_i,lat_j)    .gt. snow_threshold) .and. &
                (imager_geolocation%latitude(i,j) .lt. -60.00)) .or. &
              ((preproc_prtm%sea_ice_cover(lon_i,lat_j) .gt. ice_threshold)  .and. &
                (imager_flags%lsflag(i,j)         .eq. 0_byte)) &
            ) then
           surface%nise_mask(i,j) = YES
         else
           surface%nise_mask(i,j) = NO
         end if

         flag = .false.

         ! calculate albedo according to fraction of sea ice
         if ((surface%nise_mask(i,j) .eq. YES) .and. &
             (preproc_prtm%snow_depth(lon_i,lat_j) .lt. snow_threshold)) then
             flag = .true.
             surface%albedo(i,j,:) = &
             tmp_ice*preproc_prtm%sea_ice_cover(lon_i,lat_j) + &
             tmp_albedo*(1.-preproc_prtm%sea_ice_cover(lon_i,lat_j))

         ! same as before but with snow cover on sea ice part
         else if ((preproc_prtm%sea_ice_cover(lon_i,lat_j) .gt. ice_threshold) .and. &
             (preproc_prtm%snow_depth(lon_i,lat_j) .gt. snow_threshold) .and. &
             (surface%nise_mask(i,j) .eq. YES)) then
             flag = .true.
             surface%albedo(i,j,:) = &
             tmp_snow*preproc_prtm%sea_ice_cover(lon_i,lat_j) + &
             tmp_albedo*(1.-preproc_prtm%sea_ice_cover(lon_i,lat_j))

         ! calculate albedo for snowy pixels in absence of sea ice
         else if ((preproc_prtm%sea_ice_cover(lon_i,lat_j) .lt. ice_threshold) .and. &
             (surface%nise_mask(i,j) .eq. YES)) then
             flag = .true.
             surface%albedo(i,j,:) = tmp_snow

         end if

         if (include_full_brdf .and. flag) then
            surface%rho_0v(i,j,:) = surface%albedo(i,j,:)
            surface%rho_0d(i,j,:) = surface%albedo(i,j,:)
            surface%rho_dv(i,j,:) = surface%albedo(i,j,:)
            surface%rho_dd(i,j,:) = surface%albedo(i,j,:)
         end if
      end do
   end do

   deallocate(tmp_albedo)
   deallocate(tmp_snow)
   deallocate(tmp_ice)

end subroutine correct_for_ice_snow_nwp

end module correct_for_ice_snow_m
