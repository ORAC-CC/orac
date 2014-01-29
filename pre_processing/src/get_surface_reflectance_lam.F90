! Name: get_surface_reflectance_lam.F90
!
! Purpose:
! Populates the surface albedo part of the surface structure, using MODIS
! MCD43C spectral albedo over the land the cox_munk ocean surface reflectance
! model over the sea.
!
! Description and algorithm details
!
! Arguments:
! Name            Type     In/Out/Both Description
! modis_surf_path char     in     Path to MODIS MCD43C data
! imager_flags    struct.  in     Imager structure containing land/sea flag
! imager_geolocation       in     Imager structure containing lat/lon points
!                 struct.
! imager_angles   struct.  in     Imager structure containing viewing geom.
! channel_info    struct.  in     Preprocessing dimensions, including sw and
!                                 lw channel counts
! ecmwf_2d        struct.  in     ECMWF 2D (near-surface) fields
! surface         struct.  both   Surface properties structure
! Local variables:
! Name Type Description
!
! History:
! 30/04/2012 Gareth Thomas. Finished first version
! 30/05/2012 Gareth Thomas. Bug fixes:
!                           - Changed limit on do loops for populating tmp_WSA
!                             and wsalnd from imager_measurements%nchannels to
!                             preproc_dims%nchan_sw
!                           - Reversed index order of nested do loops to make
!                             efficient use of column-major indexing
!                           - lndcount and seacount reset to 1 before
!                             populating surface%albedo output array.
! 26/06/2012 Caroline Poulsen Bug fixes
!                           - changed indexing
!                           - defined preproc_dims%channels this is hardwired so should
!                             really be changed alonf with band info
!                           - changed count conditions may have to change back after
!                             channel info correctly implemented.
!                           - changed code to use chahnel structure information
! 27/06/2012 Gareth Thomas Changed from bilinear interpolation of MODIS land surface albedo
!                          to nearest neighbour, hopefully with a resulting significant speed
!                          improvement
! 27/06/2012 Caroline Poulsen changed modbands from 1,2,6 to 1,2,3
! 02/07/2012 Gareth Thomas Bug fix. Fill grid was attempting to use the imager grid to mask
!                          the MCD grid. The code now generates a mask for fill_grid based on
!                          the lat-lon limits of the imager_geolocation arrays.
!
! 2012/07/04 C. Poulsen removed nview dependance
! 2012/07/30 C. Poulsen initialised allocated arrays
! 2012/08/07 C. Poulsen reformated albedo array to be consistent with msi
!              file routine added that automatically selects MCD file added doy
! 2012/08/08 CP changed modband to 12,6 and created coxbands  2 3 4
! 2012/08/15 MJ speeds up code.
! 2012/08/15 Gareth Thomas Added code for inserting (1.0 - emssivity) for the 
!                          land surface reflectance at 3.7 microns (done at the point
!                          where surface reflectance is copied into the surface
!                          structures).
!                          Bug fix. Temporary ocean surface reflectance arrays were being
!                          defined using nswchannels (local value for land-surface
!                          reflectance) rather then channel_info%nchannels_sw.
!                          Also, coxbands now set from the channel_info%channel_ids_abs
!                          and channel_info%channel_sw_flag (and is dynamic). Note:
!                          this will need updating for multi-views.
! 2012/08/16 MJ fixed two bugs in bugfix from 15.8.
! 2012/08/20 MJ fixed  bugs in coxband assignment
! 2012/08/20 MJ changed read_mcd43c3 from function to subroutine in order to iron out bugs
! 2012/08/22 Matthias Jerg implements flexible x and y dimensions start and end indices
! 14/12/2012 Cp changed howy loop was set changed starty to startyi to loop over
!              a granule!
! 06/03/2013 CP fixed bug in imager_angles that was picked up when comiling in gfortran:
!              This bug had no effect on results.
! 15/03/2013 GT Reinstated bilinear interpolation, as new version is nearly as fast
!                 as nearest neighbour.
!               Fixed longitude bug in interpolation ECMWF wind fields (ECMWF
!                 longitude runs from 0-360 degrees)
! 2013/09/02: AP Removed startyi, endye.
!2014/01/17 MJ fixed doy datatype from sint to stint to comply with other defs.
!
! $Id$
!
! Bugs:
! NB channels are hardwired in this code and not selected automatically
!
subroutine get_surface_reflectance_lam(modis_surf_path, imager_flags, &
     & imager_geolocation, imager_angles, imager_measurements, channel_info, &
     & ecmwf_2d, surface,doy,cyear)
!!$      call get_surface_reflectance_lam(albedo_path_file, imager_flags, &
!!$           & imager_geolocation, imager_angles, imager_measurements,    &
!!$           & channel_info, ecmwf_2d, surface,doy,cyear)

   use preproc_constants
   use preproc_structures
   use imager_structures
   use surface_structures
   use ecmwf_structures
   use mcd43c_def
   use fill_grid_def
   use interpol_bilinear_def
   use interpol_nearest_neighbour_def
   use channel_structures

   implicit none

   interface
      subroutine read_mcd43c3(path_to_file, mcd, nbands, bands, white_sky, black_sky, QC,stat)
         use mcd43c_def
         use preproc_constants
         integer*4                                  :: stat
         character(len=pathlength), intent(in)      :: path_to_file 
         type(mcd43c)                               :: mcd
         integer(kind=stint)                        :: nbands,doy
         integer(kind=stint)                        :: bands(:)
         integer(kind=sint)                         :: white_sky
         integer(kind=sint)                         :: black_sky
         integer(kind=sint)                         :: QC
      end subroutine read_mcd43c3

      subroutine cox_munk(bands, solza, satza, solaz, relaz, u10, v10, rho)
         use preproc_constants
         integer(kind=stint), dimension(:)          :: bands
         real(kind=sreal), dimension(:)             :: solza, satza
         real(kind=sreal), dimension(:)             :: solaz, relaz
         real(kind=sreal), dimension(:)             :: u10, v10
         real(kind=sreal), dimension(:,:)           :: rho
      end subroutine cox_munk
   end interface

   ! Input variables
   character(len=pathlength), intent(in)           :: modis_surf_path
   character(len=pathlength)         :: modis_surf_path_file
   character(len=datelength), intent(in)           :: cyear
   integer(kind=stint)                              :: doy
   type(imager_flags_s), intent(in)                :: imager_flags
   type(imager_geolocation_s), intent(in)          :: imager_geolocation
   type(imager_measurements_s), intent(in)         :: imager_measurements
   type(imager_angles_s), intent(in)               :: imager_angles
   type(ecmwf_2d_s), intent(in)                    :: ecmwf_2d
   type(surface_s), intent(inout)                  :: surface
   type(channel_info_s)  , intent(in)              :: channel_info
   ! Local variables
   ! Land surface reflectance
   real(kind=sreal), allocatable, dimension(:)     :: latlnd, lonlnd
   type(mcd43c)                                    :: mcd
   integer(kind=stint), dimension(3)               :: modbands
   integer(kind=stint), allocatable, dimension(:)  :: coxbands
   integer(kind=stint), allocatable, dimension(:)  :: bands
   integer(kind=stint)                             :: nswchannels
   real(kind=sreal), allocatable, dimension(:,:)   :: tmp_WSA
   integer(kind=sint), allocatable, dimension(:,:) :: fg_mask
   real(kind=sreal), allocatable, dimension(:,:)   :: wsalnd
   integer(kind=sint)   :: ws = 1 ! 1 = read white-sky albedo
   integer(kind=sint)   :: bs = 0 ! 1 = read black-sky albedo
   integer(kind=sint)   :: qc = 0 ! 1 = read QC/ancillaty data
   integer*4            :: stat
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

   real :: minlat,maxlat,minlon,maxlon

   write(*,*) 'in get surface'

   ! Count the number of land and sea pixels, using the imager
   ! land/sea mask
   nsea  = count(imager_flags%lsflag .eq. 0)
   nland = count(imager_flags%lsflag .eq. 1)

   ! Extract land surface reflectance
   if (nland .gt. 0) then

      ! Allocate and populate the local arrays required for land pixels
      allocate(latlnd(nland))
      allocate(lonlnd(nland))
      allocate(wsalnd(channel_info%nchannels_sw,nland))

      latlnd=0
      lonlnd=0
      wsalnd=0

      ! Extract only the land pixels from the imager structures
      lndcount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            !do i=1,imager_geolocation%ny
            !do j=1,imager_geolocation%nx
            if (imager_flags%lsflag(j,i) .eq. 1) then
               latlnd(lndcount) = imager_geolocation%latitude(j,i)
               lonlnd(lndcount) = imager_geolocation%longitude(j,i)
               lndcount = lndcount+1
            end if
         end do
      end do

      ! Read MODIS MCD43C3 surface reflectance data
      ! Need to set the band numbers in terms of MODIS band numbering
      ! These are the 0.65, 0.87(ish) and 1.6 micron bands
      modbands = (/ 1, 2, 6 /)
      ! Which of these bands have been selected in the

      nswchannels = count(channel_info%channel_ids_abs .lt. 4 .and. channel_info%channel_ids_abs .gt. 0) 
      write(*,*)'nswchannels',nswchannels
      allocate(bands(nswchannels))
      bands=0


      where(channel_info%channel_ids_abs .lt. 4 .and. channel_info%channel_ids_abs .gt. 0) bands(:) = modbands

      ! Read the data itself

      !
      !select correct modis file
      !

      call select_modis_albedo_file(cyear,doy,modis_surf_path,modis_surf_path_file)

      write(*,*) 'after select_modis_albedo_file'

      !write(*,*) 'before read'

      call read_mcd43c3(modis_surf_path_file, mcd, nswchannels, &
           & bands, ws, bs, qc,stat)
      !write(*,*) 'stat',stat
      !write(*,*) 'after read'

      ! Fill missing data in the MODIS surface reflectance using a nearest
      ! neighbour technique. Note we cannot allocate tmp_WSA until we've
      ! created the mcd structure
      allocate(tmp_WSA(mcd%nlon,mcd%nlat))
      tmp_WSA=0.
      ! Also create a bit mask that controls where we apply the data
      ! filling, limiting it to the region covered by the imager data
      allocate(fg_mask(mcd%nlon,mcd%nlat))
      fg_mask(:,:) = 0 
      write(*,*) 'before minmaxvals'
      minlat=minval(imager_geolocation%latitude)
      maxlat=maxval(imager_geolocation%latitude)
      minlon= minval(imager_geolocation%longitude)
      maxlon=maxval(imager_geolocation%longitude)
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
         !write(*,*) '2',i,j
      end do

      !call itime(now)
      !write(*,*)' fill_grid itime a',now
      do i=1,nswchannels
         tmp_WSA = mcd%WSA(i,:,:)
         call fill_grid(tmp_WSA, mcd%fill, fg_mask)
         mcd%WSA(i,:,:) = tmp_WSA
      end do

      !call itime(now)
      !write(*,*)'itime b',now
      ! Use bilinear interpolation to put the MODIS surface reflectances onto
      ! the preprocessing grid (uses the numerical recipes polin2 subroutine)

      ! this takes a while to run
      write(*,*) 'before channel loop'
      do i=1,nswchannels
         !call itime(now)
         !write(*,*)'citime nearest_neighbour ',now

         !call interpol_nearest_neighbour(mcd%lon, mcd%lat, mcd%WSA(i,:,:), &
         !     lonlnd, latlnd, wsalnd(i,:))
         !write(*,*)'citime bilinear ',now

         call interpol_bilinear(mcd%lon, mcd%lat, mcd%WSA(i,:,:), &
              lonlnd, latlnd, wsalnd(i,:))

         !call itime(now)
         !write(*,*)'itimed bilinear',now
         print*,'Channel ',i,' land: ',minval(wsalnd(i,:)), maxval(wsalnd(i,:))
      end do
      write(*,*) 'after channel loop'

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

      latsea=0.
      lonsea=0.
      solzasea=0.
      satzasea=0.
      relazsea=0.
      solazsea=0.
      u10sea=0.
      v10sea=0.
      refsea_tmp=0.
      refsea=0.

      ! Which channels do we need?
      !write(*,*) size(coxbands)
      !write(*,*) size(channel_info%channel_ids_abs)
      i=0
      do k=1,channel_info%nchannels_total
         if(channel_info%channel_sw_flag(k) .eq. 1  ) then
            i=i+1
            coxbands(i) = channel_info%channel_ids_abs(k)
         endif
      enddo

      ! Extract the location data for the sea pixels
      seacount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            !do i=1,imager_geolocation%ny
            !do j=1,imager_geolocation%nx
            if (imager_flags%lsflag(j,i) .eq. 0) then
               lonsea(seacount) = imager_geolocation%longitude(j,i)
               latsea(seacount) = imager_geolocation%latitude(j,i)
               seacount = seacount+1
            end if
         end do
      end do
      ! Interpolate the ECMWF wind fields onto the instrument grid
      write(*,*) 'bilinear interpol start'

      ! ECMWF longitude is runs from 0-360 unstead of -180-180. We have to convert
      ! lonsea to match the ECMWF format (rather than the other way around) as we
      ! need longitude to be monotonically increasing for the interpolation routine
      where(lonsea .lt. 0.0) lonsea = lonsea + 360

      !call interpol_nearest_neighbour(ecmwf_2d%longitude(:,1), ecmwf_2d%latitude(1,:), &
      !      ecmwf_2d%u10, lonsea, latsea, u10sea)

      !call interpol_nearest_neighbour(ecmwf_2d%longitude(:,1), ecmwf_2d%latitude(1,:), &
      !      ecmwf_2d%v10, lonsea, latsea, v10sea)


      call interpol_bilinear(ecmwf_2d%longitude(:,1), ecmwf_2d%latitude(1,:), &
           ecmwf_2d%u10, lonsea, latsea, u10sea)

      call interpol_bilinear(ecmwf_2d%longitude(:,1), ecmwf_2d%latitude(1,:), &
           ecmwf_2d%v10, lonsea, latsea, v10sea)


      ! Code for looping over instrument views is disabled for now:
      ! we are using the lambertian forward model, for which there is
      ! no angular dependance in the surface reflectance by definition
      !    do k=1,imager_angles%nviews
      k=1
      ! Extract only the sea pixels from the imager structures
      seacount = 1
      do i=1,imager_geolocation%ny
         do j=imager_geolocation%startx,imager_geolocation%endx
            !do i=1,imager_geolocation%ny
            !do j=1,imager_geolocation%nx
            if (imager_flags%lsflag(j,i) .eq. 0) then
               solzasea(seacount)  = imager_angles%solzen(j,i,k)

               satzasea(seacount) = imager_angles%satzen(j,i,k)

               solazsea(seacount) = imager_angles%solazi(j,i,k)

               relazsea(seacount) = imager_angles%relazi(j,i,k)

               seacount = seacount+1
            end if
         end do
      end do

      ! Note that cox_munk returns the bi-directional reflectance

      call cox_munk(coxbands,solzasea, satzasea, solazsea, relazsea, &
           u10sea, v10sea, refsea_tmp)

      refsea(:,:) = refsea_tmp

      print*,'Sea: ',minval(refsea), maxval(refsea)

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
   endif ! End of sea surface reflectance setting

   ! Copy the reflectance values from sea and land into the output surface
   ! reflectance structure
   ! Note that we use the white-sky albedo for the land surface reflectance
   ! in the lambertian-surface version of the retrieval, while the
   ! bi-directional ocean reflectance (in the first view in a multiview
   ! situation) is used.
   lndcount=1
   seacount=1
   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx
         !do i=1,imager_geolocation%ny
         !do j=1,imager_geolocation%nx
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
      end do
   end do

   ! Check to see if the land and sea reflectance arrays have been created
   ! before deallocating them
   if (allocated(refsea)) deallocate(refsea)
   if (allocated(wsalnd)) deallocate(wsalnd)
   if (allocated(bands))   deallocate(bands)
end subroutine get_surface_reflectance_lam
