!-------------------------------------------------------------------------------
! Name: get_surface_reflectance_lam.F90
!
! Purpose:
! Populates the surface albedo part of the surface structure, using MODIS MCD43C
! spectral albedo over the land and the cox_munk ocean surface reflectance model
! over the sea.
!
! Description and algorithm details:
!
! Arguments:
! Name            Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! cyear           string  in          Year, as a 4 character string.
! doy             integer in          Day of year.
! assume_full_path
!                 logic   in          T: inputs are filenames; F: folder names
! modis_surf_path char    in          Path to MODIS MCD43C data
! imager_flags    struct  in          Imager structure containing land/sea flag
! imager_geolocation      in          Imager structure containing lat/lon points
!                 struct
! imager_angles   struct  in          Imager structure containing viewing geom.
! channel_info    struct  in          Preprocessing dimensions, including sw and
!                                     lw channel counts
! ecmwf           struct  in          ECMWF fields
! surface         struct  both        Surface properties structure
!
! History:
! 30/04/2012, GT: Finished first version
! 30/05/2012, GT: Bug fixes:
!    - Changed limit on do loops for populating tmp_WSA and wsalnd from
!      imager_measurements%nchannels to preproc_dims%nchan_sw
!    - Reversed index order of nested do loops to make efficient use of
!      column-major indexing
!    - lndcount and seacount reset to 1 before populating surface%albedo output
!      array.
! 26/06/2012, CP: Bug fixes
!    - Changed indexing
!    - Defined preproc_dims%channels this is hardwired so should really be
!      changed along with band info
!    - Changed count conditions may have to change back after channel info
!      correctly implemented.
!    - Changed code to use chahnel structure information
! 27/06/2012, GT: Changed from bilinear interpolation of MODIS land
!    surface albedo to nearest neighbour, hopefully with a resulting significant
!    speed improvement
! 27/06/2012, CP: changed modbands from 1,2,6 to 1,2,3
! 02/07/2012, GT: Bug fix. Fill grid was attempting to use the imager
!    grid to mask the MCD grid. The code now generates a mask for fill_grid based
!    on the lat-lon limits of the imager_geolocation arrays.
! 2012/07/04, CP: removed nview dependance
! 2012/07/30, CP: initialised allocated arrays
! 2012/08/07, CP: reformated albedo array to be consistent with msi file
!    routine added that automatically selects MCD file added doy
! 2012/08/08, CP: Changed modband to 12,6 and created coxbands  2 3 4
! 2012/08/15, MJ: Speeds up code.
! 2012/08/15, GT: Added code for inserting (1.0 - emssivity) for the land
!    surface reflectance at 3.7 microns (done at the point where surface
!    reflectance is copied into the surface structures).
!    Bug fix. Temporary ocean surface reflectance arrays were being defined
!    using nswchannels (local value for land-surface reflectance) rather then
!    channel_info%nchannels_sw.
!    Also, coxbands now set from the channel_info%channel_ids_abs and
!    channel_info%channel_sw_flag (and is dynamic). Note: this will need updating
!    for multi-views.
! 2012/08/16, MJ: Fixed two bugs in bugfix from 15.8.
! 2012/08/20, MJ: Fixed  bugs in coxband assignment
! 2012/08/20, MJ: Changed read_mcd43c3 from function to subroutine in order to
!    iron out bugs
! 2012/08/22, MJ: Implements flexible x and y dimensions start and end indices
! 14/12/2012, CP: Changed howy loop was set changed starty to startyi to loop
!    over a granule!
! 06/03/2013, CP: fixed bug in imager_angles that was picked up when comiling in
!    gfortran: This bug had no effect on results.
! 15/03/2013, GT: Reinstated bilinear interpolation, as new version is nearly as
!    fast as nearest neighbour.
!    Fixed longitude bug in interpolation ECMWF wind fields (ECMWF longitude runs
!    from 0-360 degrees)
! 2013/09/02, AP: Removed startyi, endye.
! 2014/01/17, MJ: Fixed doy datatype from sint to stint to comply with other defs
! 2014/04/20, GM: Cleaned up the code.
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/06/20, GM: Handle case when imager_geolocation%latitude or
!    imager_geolocation%longitude is equal to fill_value.
! 2014/06/11, AP: Use standard fill value rather than unique one. Use new 
!    ecmwf structures.
!
! $Id$
!
! Bugs:
! - NITAVHRR hangs somewhere in here.
! NB channels are hardwired in this code and not selected automatically
!-------------------------------------------------------------------------------

subroutine get_surface_reflectance_lam(cyear, doy, assume_full_path, &
   modis_surf_path, imager_flags, imager_geolocation, imager_angles, &
   imager_measurements, channel_info, ecmwf, surface)

   use channel_structures
   use cox_munk_m
   use ecmwf_m, only : ecmwf_s
   use fill_grid_m
   use imager_structures
   use interpol
   use mcd43c_m
   use preproc_constants
   use preproc_structures
   use surface_structures

   implicit none

   ! Input variables
   character(len=datelength),   intent(in)         :: cyear
   integer(kind=stint),         intent(in)         :: doy
   logical,                     intent(in)         :: assume_full_path
   character(len=pathlength),   intent(in)         :: modis_surf_path
   type(imager_flags_s),        intent(in)         :: imager_flags
   type(imager_geolocation_s),  intent(in)         :: imager_geolocation
   type(imager_angles_s),       intent(in)         :: imager_angles
   type(imager_measurements_s), intent(in)         :: imager_measurements
   type(channel_info_s),        intent(in)         :: channel_info
   type(ecmwf_s),               intent(in)         :: ecmwf
   type(surface_s),             intent(inout)      :: surface

   ! Local variables

   ! Land surface reflectance
   character(len=pathlength)                       :: modis_surf_path_file
   real(kind=sreal), allocatable, dimension(:)     :: latlnd, lonlnd
   type(mcd43c)                                    :: mcd
   integer(kind=stint), dimension(3)               :: modbands
   integer(kind=stint), allocatable, dimension(:)  :: coxbands
   integer(kind=stint), allocatable, dimension(:)  :: bands
   integer(kind=stint)                             :: nswchannels
   real(kind=sreal), allocatable, dimension(:,:)   :: tmp_WSA
   integer(kind=sint), allocatable, dimension(:,:) :: fg_mask
   real(kind=sreal), allocatable, dimension(:,:)   :: wsalnd
   integer(kind=sint)                              :: ws = 1 ! 1 = read white-sky albedo
   integer(kind=sint)                              :: bs = 0 ! 1 = read black-sky albedo
   integer(kind=sint)                              :: qc = 0 ! 1 = read QC/ancillaty data
   integer*4                                       :: stat

   ! Sea surface reflectance
   real(kind=sreal), allocatable, dimension(:)     :: latsea, lonsea
   real(kind=sreal), allocatable, dimension(:)     :: solzasea, satzasea
   real(kind=sreal), allocatable, dimension(:)     :: relazsea, solazsea
   real(kind=sreal), allocatable, dimension(:)     :: u10sea, v10sea
   real(kind=sreal), allocatable, dimension(:,:)   :: refsea_tmp
   real(kind=sreal), allocatable, dimension(:,:)   :: refsea

   ! General
   integer(kind=lint)                              :: i,j,k
   integer(kind=lint)                              :: nsea=0, nland=0
   integer(kind=lint)                              :: seacount=1
   integer(kind=lint)                              :: lndcount=1

   real                                            :: minlat,maxlat,minlon,maxlon

   logical                                         :: include_full_brdf_sea

   logical, allocatable, dimension(:,:)            :: mask

   write(*,*) 'In get_surface_reflectance_lam()'

   ! Count the number of land and sea pixels, using the imager land/sea mask
   nsea  = count(imager_flags%lsflag .eq. 0)
   nland = count(imager_flags%lsflag .eq. 1)

   allocate(mask(imager_geolocation%startx:imager_geolocation%endx, &
                 1:imager_geolocation%ny))

   mask = imager_geolocation%latitude  .ne. real_fill_value .and. &
          imager_geolocation%longitude .ne. real_fill_value


   ! Extract land surface reflectance
   if (nland .gt. 0) then

      ! Allocate and populate the local arrays required for land pixels
      allocate(latlnd(nland))
      allocate(lonlnd(nland))
      allocate(wsalnd(channel_info%nchannels_sw,nland))

      latlnd = 0
      lonlnd = 0
      wsalnd = 0

      ! Extract only the land pixels from the imager structures
      lndcount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 1) then
               if (mask(j,i)) then
                  latlnd(lndcount) = imager_geolocation%latitude(j,i)
                  lonlnd(lndcount) = imager_geolocation%longitude(j,i)
                  lndcount = lndcount+1
               end if
            end if
         end do
      end do

      ! Read MODIS MCD43C3 surface reflectance data
      ! Need to set the band numbers in terms of MODIS band numbering. These are
      ! the 0.65, 0.87(ish) and 1.6 micron bands
      modbands = (/ 1, 2, 6 /)

      ! Which of these bands have been selected
      nswchannels = count(channel_info%channel_ids_abs .gt. 0 .and. &
                          channel_info%channel_ids_abs .lt. 4)
      write(*,*)'nswchannels: ',nswchannels
      allocate(bands(nswchannels))
      bands = 0

      where(channel_info%channel_ids_abs .lt. 4 .and. &
            channel_info%channel_ids_abs .gt. 0) bands(:) = modbands

      ! Select correct modis file
      if (assume_full_path) then
         modis_surf_path_file = modis_surf_path
      else
         call select_modis_albedo_file(cyear,doy,modis_surf_path, &
                                       modis_surf_path_file)
      endif
      write(*,*)'modis_surf_path_file: ',trim(modis_surf_path_file)

      ! Read the data itself
      call read_mcd43c3(modis_surf_path_file, mcd, nswchannels, bands, ws, bs, &
                        qc,stat)

      ! Fill missing data in the MODIS surface reflectance using a nearest
      ! neighbour technique. Note we cannot allocate tmp_WSA until we've created
      ! the mcd structure
      allocate(tmp_WSA(mcd%nlon,mcd%nlat))
      tmp_WSA=0.

      ! Also create a bit mask that controls where we apply the data filling,
      ! limiting it to the region covered by the imager data
      allocate(fg_mask(mcd%nlon,mcd%nlat))
      fg_mask(:,:) = 0

      minlat = minval(imager_geolocation%latitude,  mask)
      maxlat = maxval(imager_geolocation%latitude,  mask)
      minlon = minval(imager_geolocation%longitude, mask)
      maxlon = maxval(imager_geolocation%longitude, mask)
      do j=1, mcd%nlat
         if ((mcd%lat(j) .ge. minlat) .and. &
              (mcd%lat(j) .le. maxlat)) then
            do i=1, mcd%nlon
               if ((mcd%lon(i) .ge. minlon) .and. &
                    (mcd%lon(i) .le. maxlon)) then
                  fg_mask(i,j) = 1
               end if
            end do
         end if
      end do

      do i=1,nswchannels
         tmp_WSA = mcd%WSA(i,:,:)
         call fill_grid(tmp_WSA, real_fill_value, fg_mask)
         mcd%WSA(i,:,:) = tmp_WSA
      end do

      ! Use bilinear interpolation to put the MODIS surface reflectances onto
      ! the preprocessing grid (uses the numerical recipes polin2 subroutine)
      do i=1,nswchannels

!        call interpol_nearest_neighbour(mcd%lon, mcd%lat, mcd%WSA(i,:,:), &
!                                        lonlnd, latlnd, wsalnd(i,:))

         call interpol_bilinear(mcd%lon, mcd%lat, mcd%WSA(i,:,:), &
                                lonlnd, latlnd, wsalnd(i,:), real_fill_value)

         write(*,*) 'Channel ',i,' land: ',minval(wsalnd(i,:)), &
              maxval(wsalnd(i,:))
      end do

      ! Do some clean-up
      deallocate(latlnd)
      deallocate(lonlnd)

      deallocate(tmp_WSA)
      deallocate(fg_mask)
      call deallocate_mcd43c(mcd)

   end if ! End of land surface reflectance setting

   ! Now check for sea pixels and use the cox_munk routine to model their
   ! reflectance
   if (nsea .gt. 0) then
      ! Allocate and populate the local arrays required for sea pixels
      allocate(latsea(nsea))
      allocate(lonsea(nsea))
      allocate(solzasea(nsea))
      allocate(satzasea(nsea))
      allocate(relazsea(nsea))
      allocate(solazsea(nsea))
      allocate(u10sea(nsea))
      allocate(v10sea(nsea))
      allocate(refsea_tmp(channel_info%nchannels_sw,nsea))
      allocate(refsea(channel_info%nchannels_sw,nsea))
      allocate(coxbands(channel_info%nchannels_sw))

      latsea     = 0.
      lonsea     = 0.
      solzasea   = 0.
      satzasea   = 0.
      relazsea   = 0.
      solazsea   = 0.
      u10sea     = 0.
      v10sea     = 0.
      refsea_tmp = 0.
      refsea     = 0.

      ! Which channels do we need?
      i=0
      do k=1,channel_info%nchannels_total
         if (channel_info%channel_sw_flag(k) .eq. 1  ) then
            i=i+1
            coxbands(i) = channel_info%channel_ids_abs(k)
         endif
      enddo

      ! Extract the location data for the sea pixels
      seacount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 0) then
               if (mask(j,i)) then
                  lonsea(seacount) = imager_geolocation%longitude(j,i)
                  latsea(seacount) = imager_geolocation%latitude(j,i)
                  seacount = seacount+1
               end if
            end if
         end do
      end do

      ! Interpolate the ECMWF wind fields onto the instrument grid

      ! ECMWF longitude runs from 0-360 unstead of -180-180. We have to convert
      ! lonsea to match the ECMWF format (rather than the other way around) as we
      ! need longitude to be monotonically increasing for the interpolation
      ! routine
      where(lonsea .lt. 0.0) lonsea = lonsea + 360

!     call interpol_nearest_neighbour(ecmwf%lon, ecmwf%lat, &
!                                     ecmwf%u10, lonsea, latsea, u10sea)

!     call interpol_nearest_neighbour(ecmwf%lon, ecmwf%lat, &
!                                     ecmwf%v10, lonsea, latsea, v10sea)

      call interpol_bilinear(ecmwf%lon, ecmwf%lat, &
           ecmwf%u10, lonsea, latsea, u10sea, real_fill_value)

      call interpol_bilinear(ecmwf%lon, ecmwf%lat, &
           ecmwf%v10, lonsea, latsea, v10sea, real_fill_value)

      ! Code for looping over instrument views is disabled for now: we are using
      ! the lambertian forward model, for which there is no angular dependance
      ! in the surface reflectance by definition
!     do k=1,imager_angles%nviews
      k=1
      ! Extract only the sea pixels from the imager structures
      seacount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 0) then
               solzasea(seacount) = imager_angles%solzen(j,i,k)

               satzasea(seacount) = imager_angles%satzen(j,i,k)

               solazsea(seacount) = imager_angles%solazi(j,i,k)

               relazsea(seacount) = imager_angles%relazi(j,i,k)

               seacount = seacount+1
            end if
         end do
      end do

      ! Note that cox_munk returns the bi-directional reflectance
      call cox_munk(coxbands,solzasea, satzasea, solazsea, relazsea, u10sea, &
                    v10sea, refsea_tmp)

      refsea(:,:) = refsea_tmp

      write(*,*) 'Sea: ',minval(refsea), maxval(refsea)

!     end do ! End of instrument view loop

      ! Tidy up cox_munk input arrays
      deallocate(latsea)
      deallocate(lonsea)
      deallocate(solzasea)
      deallocate(satzasea)
      deallocate(solazsea)
      deallocate(relazsea)
      deallocate(u10sea)
      deallocate(v10sea)
      deallocate(refsea_tmp)
      deallocate(coxbands)
   end if ! End of sea surface reflectance setting

   ! Copy the reflectance values from sea and land into the output surface
   ! reflectance structure
   ! Note that we use the white-sky albedo for the land surface reflectance in
   ! the lambertian-surface version of the retrieval, while the bi-directional
   ! ocean reflectance (in the first view in a multiview situation) is used.
   lndcount = 1
   seacount = 1
   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx
         if (mask(j,i)) then
            if (imager_flags%lsflag(j,i) .ne. 0) then
               surface%albedo(j,i,:) = wsalnd(:,lndcount)

               ! The MCD43c3 product is of no use for the 3.7 micron channel. If
               ! it has been selected, we use 1-emissivity as the land surface
               ! reflectance
               if (count(channel_info%channel_ids_abs .eq. 4) .gt. 0) then
                  surface%albedo(j,i,4) = 1.0 - surface%emissivity(j,i,1)
               end if

               lndcount = lndcount+1
            else
               surface%albedo(j,i,:) = refsea(:,seacount)

               seacount = seacount+1
            end if
         end if
      end do
   end do

   ! Check to see if the land and sea reflectance arrays have been created
   ! before deallocating them
   deallocate(mask)

   if (allocated(refsea)) deallocate(refsea)
   if (allocated(wsalnd)) deallocate(wsalnd)
   if (allocated(bands))  deallocate(bands)

end subroutine get_surface_reflectance_lam
