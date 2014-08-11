!-------------------------------------------------------------------------------
! Name: get_surface_reflectance.F90
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
!    grid to mask the MCD grid. The code now generates a mask for fill_grid
!    based on the lat-lon limits of the imager_geolocation arrays.
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
!    channel_info%channel_sw_flag (and is dynamic). Note: this will need
!    updating for multi-views.
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
!    Fixed longitude bug in interpolation ECMWF wind fields (ECMWF longitude
!    runs from 0-360 degrees)
! 2013/09/02, AP: Removed startyi, endye.
! 2014/01/17, MJ: Fixed doy datatype from sint to stint to comply with other
!    defs
! 2014/04/20, GM: Cleaned up the code.
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/06/20, GM: Handle case when imager_geolocation%latitude or
!    imager_geolocation%longitude is equal to fill_value.
! 2014/06/11, AP: Use standard fill value rather than unique one. Use new
!    ecmwf structures.
! 2014/08/10, GM: Changes related to new BRDF support.
!
! $Id$
!
! Bugs:
! NITAVHRR hangs somewhere in here.
! NB channels are hardwired in this code and not selected automatically
!-------------------------------------------------------------------------------

subroutine get_surface_reflectance(cyear, doy, assume_full_path, &
   modis_surf_path, imager_flags, imager_geolocation, imager_angles, &
   imager_measurements, channel_info, ecmwf, include_full_brdf, surface)

   use channel_structures
   use cox_munk_m
   use ecmwf_m, only : ecmwf_s
   use fill_grid_m
   use imager_structures
   use interpol
   use mcd43c_m
   use preproc_constants
   use preproc_structures
   use ross_thick_li_sparse_r_m
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
   logical,                     intent(in)         :: include_full_brdf
   type(surface_s),             intent(inout)      :: surface

   ! Local variables

   ! Land surface reflectance
   character(len=pathlength)                       :: modis_surf_path_file
   character(len=pathlength)                       :: modis_brdf_path_file
   real(kind=sreal), allocatable, dimension(:)     :: latlnd, lonlnd
   real(kind=sreal), allocatable, dimension(:)     :: solzalnd, satzalnd
   real(kind=sreal), allocatable, dimension(:)     :: solazlnd, relazlnd
   type(mcd43c1)                                   :: mcdc1
   type(mcd43c3)                                   :: mcdc3
   integer, dimension(3)                           :: modbands
   integer, allocatable, dimension(:)              :: coxbands
   integer, allocatable, dimension(:)              :: bands
   integer                                         :: nswchannels
   real(kind=sreal), allocatable, dimension(:,:)   :: tmp_data
   integer(kind=sint), allocatable, dimension(:,:) :: fg_mask
   real(kind=sreal), allocatable, dimension(:,:)   :: wsalnd
   real(kind=sreal), allocatable, dimension(:,:,:) :: wgtlnd
   real(kind=sreal), allocatable, dimension(:,:,:) :: rholnd
   integer                                         :: read_ws   = 1 ! 1 = read white-sky albedo
   integer                                         :: read_bs   = 0 ! 1 = read black-sky albedo
   integer                                         :: read_brdf = 1 ! 1 = read brdf parameters
   integer                                         :: read_qc   = 0 ! 1 = read QC/ancillaty data
   integer                                         :: stat

   ! Sea surface reflectance
   real(kind=sreal), allocatable, dimension(:)     :: latsea, lonsea
   real(kind=sreal), allocatable, dimension(:)     :: solzasea, satzasea
   real(kind=sreal), allocatable, dimension(:)     :: solazsea, relazsea
   real(kind=sreal), allocatable, dimension(:)     :: u10sea, v10sea
   real(kind=sreal), allocatable, dimension(:,:)   :: refsea
   real(kind=sreal), allocatable, dimension(:,:,:) :: rhosea
   real(kind=dreal)                                :: time
   type(cox_munk_shared_band_type)                 :: cox_munk_shared_band(4)
   type(cox_munk_shared_geo_wind_type)             :: cox_munk_shared_geo_wind

   ! General
   integer(kind=lint)                              :: i,j,k,i_mcd,j_mcd
   integer(kind=lint)                              :: nsea=0, nland=0
   integer(kind=lint)                              :: seacount=1
   integer(kind=lint)                              :: lndcount=1

   real                                            :: lat_res,lon_res

   logical, allocatable, dimension(:,:)            :: mask


   write(*,*) 'In get_surface_reflectance_lam()'


   ! Count the number of land and sea pixels, using the imager land/sea mask
   nsea  = count(imager_flags%lsflag .eq. 0)
   nland = count(imager_flags%lsflag .eq. 1)

   allocate(mask(imager_geolocation%startx:imager_geolocation%endx, &
                 1:imager_geolocation%ny))

   mask = imager_geolocation%latitude  .ne. real_fill_value .and. &
          imager_geolocation%longitude .ne. real_fill_value

   if (include_full_brdf) then
      do k=1,imager_angles%nviews
         mask = mask .and. &
                imager_angles%solzen(:,:,k) .ne. real_fill_value .and. &
                imager_angles%solzen(:,:,k) .lt. maxsza_twi      .and. &
                imager_angles%satzen(:,:,k) .ne. real_fill_value .and. &
                imager_angles%solazi(:,:,k) .ne. real_fill_value .and. &
                imager_angles%relazi(:,:,k) .ne. real_fill_value
      end do
   endif


   !----------------------------------------------------------------------------
   ! Extract land surface reflectance
   !----------------------------------------------------------------------------
   if (nland .gt. 0) then
      ! Allocate and populate the local arrays required for land pixels
      allocate(latlnd(nland))
      allocate(lonlnd(nland))
      allocate(wsalnd(channel_info%nchannels_sw,nland))
      if (include_full_brdf) then
         allocate(solzalnd(nland))
         allocate(satzalnd(nland))
         allocate(solazlnd(nland))
         allocate(relazlnd(nland))
         allocate(wgtlnd(3,channel_info%nchannels_sw,nland))
         allocate(rholnd (channel_info%nchannels_sw,nland,4))
      endif

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
      write(*,*)'nswchannels: ', nswchannels
      allocate(bands(nswchannels))
      bands = 0

      where(channel_info%channel_ids_abs .lt. 4 .and. &
            channel_info%channel_ids_abs .gt. 0) bands(:) = modbands

      ! Select correct modis file
      if (assume_full_path) then
         modis_surf_path_file = modis_surf_path
      else
         call select_modis_albedo_file(cyear,doy,modis_surf_path, &
                                       .false.,modis_surf_path_file)
         if (include_full_brdf) then
            call select_modis_albedo_file(cyear,doy,modis_surf_path, &
                                          .true.,modis_brdf_path_file)

         endif
      endif
      write(*,*)'modis_surf_path_file: ', trim(modis_surf_path_file)
      if (include_full_brdf) then
         write(*,*)'modis_brdf_path_file: ', trim(modis_brdf_path_file)
      endif

      ! Read the data itself
      call read_mcd43c3(modis_surf_path_file, mcdc3, nswchannels, bands, &
                        read_ws, read_bs, read_qc, stat)

      if (include_full_brdf) then
         call read_mcd43c1(modis_brdf_path_file, mcdc1, nswchannels, bands, &
                           read_brdf, read_qc, stat)
      endif

      ! Fill missing data in the MODIS surface reflectance using a nearest
      ! neighbour technique. Note we cannot allocate tmp_data until we've created
      ! the mcd structure
      allocate(tmp_data(mcdc3%nlon,mcdc3%nlat))
      tmp_data=0.

      ! Also create a bit mask that controls where we apply the data filling,
      ! limiting it to the region covered by the imager data
      allocate(fg_mask(mcdc3%nlon,mcdc3%nlat))
      fg_mask(:,:) = 0

      ! Code for looping over instrument views is disabled for now
!     do k=1,imager_angles%nviews
      k=1

      ! Extract only the land pixels from the imager structures
      ! Also flag required pixels from MCD array assuming a regular grid
      lat_res=mcdc3%lat(2)-mcdc3%lat(1)
      lon_res=mcdc3%lon(2)-mcdc3%lon(1)
      lndcount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 1) then
               if (mask(j,i)) then
                  latlnd(lndcount) = imager_geolocation%latitude(j,i)
                  lonlnd(lndcount) = imager_geolocation%longitude(j,i)

                  ! flag pixels for which there will be albedo data
                  if (abs(latlnd(lndcount)) < 80.2) then
                     i_mcd=floor((latlnd(lndcount) - mcdc3%lat(1)) / lat_res + 1.5)
                     j_mcd=floor((lonlnd(lndcount) - mcdc3%lon(1)) / lon_res + 1.5)
                     if (j_mcd > mcdc3%nlon-1) j_mcd = mcdc3%nlon-1
                     fg_mask(j_mcd:j_mcd+1,i_mcd:i_mcd+1)=1
                  end if

                  lndcount = lndcount+1
               end if
            end if
         end do
      end do

      do i=1,nswchannels
         tmp_data = mcdc3%WSA(i,:,:)

         call fill_grid(tmp_data, real_fill_value, fg_mask)

!        call interpol_nearest_neighbour(mcdc3%lon, mcdc3%lat, tmp_data, &
!                                        lonlnd, latlnd, wsalnd(i,:))
         call interpol_bilinear         (mcdc3%lon, mcdc3%lat, tmp_data, &
                                         lonlnd, latlnd, wsalnd(i,:), &
                                         real_fill_value)

         write(*,*) 'Land WSA: channel, min, max = ', i, minval(wsalnd(i,:)), &
                                                         maxval(wsalnd(i,:))

         if (include_full_brdf) then
            do j = 1, 3
               tmp_data = mcdc1%brdf_albedo_params(i,j,:,:)

               call fill_grid(tmp_data, real_fill_value, fg_mask)

!              call interpol_nearest_neighbour(mcdc3%lon, mcdc3%lat, tmp_data, &
!                                              lonlnd, latlnd, wgtlnd(j,i,:))
               call interpol_bilinear         (mcdc3%lon, mcdc3%lat, tmp_data, &
                                               lonlnd, latlnd, wgtlnd(j,i,:), &
                                               real_fill_value)
            enddo
         endif
      end do

      if (include_full_brdf) then
         lndcount = 1
         do i=1,imager_geolocation%ny
            do j=imager_geolocation%startx,imager_geolocation%endx
               if (imager_flags%lsflag(j,i) .eq. 1) then
                  if (mask(j,i)) then
                     solzalnd(lndcount) = imager_angles%solzen(j,i,k)
                     satzalnd(lndcount) = imager_angles%satzen(j,i,k)
                     solazlnd(lndcount) = imager_angles%solazi(j,i,k)
                     relazlnd(lndcount) = imager_angles%relazi(j,i,k)

                     lndcount = lndcount+1
                  end if
               end if
            end do
         end do

         call ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd &
            (3, solzalnd, satzalnd, solazlnd, relazlnd, wgtlnd, real_fill_value, &
             rholnd(:,:,1), rholnd(:,:,2), rholnd(:,:,3), rholnd(:,:,4))
      endif

!     end do ! End of instrument view loop

      ! Do some clean-up
      deallocate(bands)
      deallocate(latlnd)
      deallocate(lonlnd)
      deallocate(tmp_data)
      deallocate(fg_mask)
      call deallocate_mcd43c3(mcdc3)
      if (include_full_brdf) then
         deallocate(solzalnd)
         deallocate(satzalnd)
         deallocate(solazlnd)
         deallocate(relazlnd)
         deallocate(wgtlnd)
         call deallocate_mcd43c1(mcdc1)
      endif

   end if ! End of land surface reflectance setting


   !----------------------------------------------------------------------------
   ! Compute sea surface reflectance
   !----------------------------------------------------------------------------
   if (nsea .gt. 0) then
      ! Allocate and populate the local arrays required for sea pixels
      allocate(latsea(nsea))
      allocate(lonsea(nsea))
      allocate(solzasea(nsea))
      allocate(satzasea(nsea))
      allocate(solazsea(nsea))
      allocate(relazsea(nsea))
      allocate(u10sea(nsea))
      allocate(v10sea(nsea))
      allocate(refsea(channel_info%nchannels_sw,nsea))
      allocate(coxbands(channel_info%nchannels_sw))
      if (include_full_brdf) then
         allocate(rhosea(channel_info%nchannels_sw,nsea,4))
      endif

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
                  latsea(seacount) = imager_geolocation%latitude(j,i)
                  lonsea(seacount) = imager_geolocation%longitude(j,i)
                  seacount = seacount+1
               end if
            end if
         end do
      end do

      ! Interpolate the ECMWF wind fields onto the instrument grid

!     call interpol_nearest_neighbour(ecmwf%lon, ecmwf%lat, &
!                                     ecmwf%u10, lonsea, latsea, u10sea)

!     call interpol_nearest_neighbour(ecmwf%lon, ecmwf%lat, &
!                                     ecmwf%v10, lonsea, latsea, v10sea)

      call interpol_bilinear(ecmwf%lon, ecmwf%lat, &
           ecmwf%u10, lonsea, latsea, u10sea, real_fill_value)

      call interpol_bilinear(ecmwf%lon, ecmwf%lat, &
           ecmwf%v10, lonsea, latsea, v10sea, real_fill_value)

      ! Code for looping over instrument views is disabled for now
!     do k=1,imager_angles%nviews
      k=1
      ! Extract only the sea pixels from the imager structures
      seacount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            if (imager_flags%lsflag(j,i) .eq. 0) then
               if (mask(j,i)) then

                  solzasea(seacount) = imager_angles%solzen(j,i,k)
                  satzasea(seacount) = imager_angles%satzen(j,i,k)
                  solazsea(seacount) = imager_angles%solazi(j,i,k)
                  relazsea(seacount) = imager_angles%relazi(j,i,k)

                  seacount = seacount+1
               end if
            end if
         end do
      end do

      do i = 1, channel_info%nchannels_sw
         call cox_munk3_calc_shared_band(i, cox_munk_shared_band(i))
      end do

      do j = 1, nsea
         call cox_munk3_calc_shared_geo_wind &
            (solzasea(j), satzasea(j), solazsea(j), relazsea(j), &
             u10sea(j), v10sea(j), cox_munk_shared_geo_wind)

         do i = 1, channel_info%nchannels_sw
            call cox_munk3(i, cox_munk_shared_band(i), &
                              cox_munk_shared_geo_wind, refsea(i, j))
         end do
      end do

      write(*,*) 'Sea reflectance: min, max = ', minval(refsea), maxval(refsea)

      if (include_full_brdf) then
         call cox_munk_rho_0v_0d_dv_and_dd &
            (coxbands, solzasea, satzasea, solazsea, relazsea, u10sea, v10sea, &
             real_fill_value, rhosea(:,:,1), rhosea(:,:,2), rhosea(:,:,3), rhosea(:,:,4))
      endif

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
      deallocate(coxbands)
   end if ! End of sea surface reflectance setting


   !----------------------------------------------------------------------------
   ! Copy the reflectance values from land and sea into the output surface
   ! reflectance structure.
   !
   ! Note that for surface%albedo used in the Lambertian retrieval we use the
   ! white-sky albedo for the land surface reflectance in, while the bi-direc-
   ! tional ocean reflectance (in the first view in a multiview situation) is
   ! used as an approximation.
   !----------------------------------------------------------------------------
   lndcount = 1
   seacount = 1
   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx
         if (mask(j,i)) then
            if (imager_flags%lsflag(j,i) .ne. 0) then

               surface%albedo(j,i,:) = wsalnd(:,lndcount)

               if (include_full_brdf) then
                  surface%rho_0v(j,i,:) = rholnd(:,lndcount,1)
                  surface%rho_0d(j,i,:) = rholnd(:,lndcount,2)
                  surface%rho_dv(j,i,:) = rholnd(:,lndcount,3)
                  surface%rho_dd(j,i,:) = rholnd(:,lndcount,4)
               endif

               ! The MCD43c3 product is of no use for the 3.7 micron channel. If
               ! it has been selected, we use 1-emissivity as the land surface
               ! reflectance
               if (count(channel_info%channel_ids_abs .eq. 4) .gt. 0) then
                  surface%albedo(j,i,4) = 1.0 - surface%emissivity(j,i,1)

                  if (include_full_brdf) then
                     surface%rho_0v(j,i,4) = 0.
                     surface%rho_0d(j,i,4) = 0.
                     surface%rho_dv(j,i,4) = 0.
                     surface%rho_dd(j,i,4) = 1.0 - surface%emissivity(j,i,1)
                  endif
               end if

               lndcount = lndcount+1

            else

               surface%albedo(j,i,:) = refsea(:,seacount)

               if (include_full_brdf) then
                  surface%rho_0v(j,i,:) = rhosea(:,seacount,1)
                  surface%rho_0d(j,i,:) = rhosea(:,seacount,2)
                  surface%rho_dv(j,i,:) = rhosea(:,seacount,3)
                  surface%rho_dd(j,i,:) = rhosea(:,seacount,4)
               endif

               seacount = seacount+1
            end if
         endif
      end do
   end do


   ! Check to see if the land and sea reflectance arrays have been created
   ! before deallocating them
   deallocate(mask)

   if (allocated(wsalnd)) deallocate(wsalnd)
   if (allocated(rholnd)) deallocate(rholnd)

   if (allocated(refsea)) deallocate(refsea)
   if (allocated(rhosea)) deallocate(rhosea)

end subroutine get_surface_reflectance
