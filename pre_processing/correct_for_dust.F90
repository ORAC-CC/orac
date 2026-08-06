!-------------------------------------------------------------------------------
! Name: correct_for_dust.F90
!
! Purpose:
! This is a very simple correction for detecting areas masked as cloud
! which are actually dust. Might also be effective for volcanic ash.
! It selects pixels which have been flagged as cloud, but with a high
! uncertainty, and then uses the 11-12 micron BTD to flag possible
! dust. Then, a morphological opening transform is applied to the
! resulting dust mask to remove random false detections.
!
! History:
! 2024/03/11, GT: Initial version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine correct_for_dust(channel_info, imager_measurements, imager_angles, &
     imager_geolocation, imager_pavolonis, verbose)

   use common_constants_m
   use channel_structures_m
   use imager_structures_m
   use morphology_m

   ! Input (and output) variables
   type(channel_info_t), intent(in)        :: channel_info
   type(imager_measurements_t), intent(in) :: imager_measurements
   type(imager_angles_t), intent(in)       :: imager_angles
   type(imager_geolocation_t), intent(in)  :: imager_geolocation
   type(imager_pavolonis_t), intent(inout) :: imager_pavolonis
   logical, intent(in)                     :: verbose
   ! Local variables
   integer(kind=lint)                      :: i, j
   integer(kind=lint)                      :: bt11=-1, bt12=-1
   integer, dimension( imager_geolocation%startx:imager_geolocation%endx, &
        1:imager_geolocation%ny )          :: dustmask
   integer, dimension(5,5) :: kernel=reshape( (/ 0, 1, 1, 1, 0, &
                                                 1, 1, 1, 1, 1, &
                                                 1, 1, 1, 1, 1, &
                                                 1, 1, 1, 1, 1, &
                                                 0, 1, 1, 1, 0 /), &
                                             shape(kernel) )

   ! First we need to determine if we have the required channels:
   do i = 1, channel_info%nchannels_total
      if (ANINT(channel_info%channel_wl_abs(i)) .eq. 11.0) bt11 = i
      if (ANINT(channel_info%channel_wl_abs(i)) .eq. 12.0) bt12 = i
      if (bt11 .gt. 0 .and. bt12 .gt. 0) exit
   end do

   ! If we do have the requisite brightness temperatures, then do our test
   ! The tests are:
   ! * Is the result from the NN cloud-mask uncertain?
   ! * Do we have valid BTs?
   ! * BT difference test
   ! * Limit on solar-zenith. Deals with false positives at high latitudes
   !   in LEO instruments (need to check if this is suitable for Geo).
   if (bt11 .gt. 0 .and. bt12 .gt. 0) then
      where(imager_pavolonis%cldmask_uncertainty(:,:,1) .gt. 10 .and. &
           imager_measurements%data(:,:,bt11) .gt. 0 .and. &
           (imager_measurements%data(:,:,bt11) - &
           imager_measurements%data(:,:,bt12)) .lt. 0.3 .and. &
           (imager_angles%solzen(:,:,1) .lt. 50 .or. &
           abs(imager_geolocation%latitude) .lt. 40))
         dustmask = 1
      elsewhere
         dustmask = 0
      end where
      ! Now do perform an opening transform on our new mask, which should
      ! remove scattered false positives
      if (verbose) write(*,*) count(dustmask .eq. 1),' pixels flagged as dust before open'
      dustmask = morph_open(dustmask, kernel)

      if (verbose) write(*,*) count(dustmask .eq. 1),' pixels flagged as dust'
      ! Finally, correct the cldmask with the detected dust pixels and add
      ! two new values to the Pavalonis cloud-type mask, one indicating where
      ! we think a clear pixel is dust, and where previously flagged cloud
      ! has been switched to dust
      do i = imager_geolocation%startx, imager_geolocation%endx
         do j = 1, size(imager_pavolonis%cldmask,2)
            if (dustmask(i,j) .eq. 1)  then
               if (maxval(imager_pavolonis%cldmask(i,j,:)) .eq. 1) then
                  imager_pavolonis%cldtype(i,j,1) = DUST_SWITCHED_FROM_CLOUD_TYPE
               else
                  imager_pavolonis%cldtype(i,j,1) = DUST_CLEAR_TYPE
               end if
               imager_pavolonis%cldmask(i,j,:) = CLEAR_TYPE
            end if
         end do
      end do
      if (verbose) write(*,*) &
           count(imager_pavolonis%cldtype(:,:,1) .eq. DUST_CLEAR_TYPE), &
           ' clear pixels set as dust'
      if (verbose) write(*,*) &
           count(imager_pavolonis%cldtype(:,:,1) .eq. DUST_SWITCHED_FROM_CLOUD_TYPE), &
           ' cloud pixels set as dust'
   else
      ! We don't have the required BTs, so issue a warning and return without
      ! doing anything
      write(*,*) 'WARNING: correct_for_dust(): Need 11 and 12 micron BTs for dust correction. No correction performed.'
   end if

end subroutine correct_for_dust
