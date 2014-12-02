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
! cdoy            string  in          DOY,  as a 3 character string.
! modis_surf_path char    in          Path to MODIS MCD43C data
! imager_flags    struct  in          Imager structure containing land/sea flag
! imager_geolocation      in          Imager structure containing lat/lon points
!                 struct
! imager_angles   struct  in          Imager structure containing viewing geom.
! channel_info    struct  in          Preprocessing dimensions, including sw and
!                                     lw channel counts
! ecmwf           struct  in          ECMWF fields
! assume_full_path
!                 logic   in          T: inputs are filenames; F: folder names
! verbose         logic   in          T: print status information; F: don't
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
!    - Changed code to use channel structure information
! 27/06/2012, GT: Changed from bilinear interpolation of MODIS land
!    surface albedo to nearest neighbour, hopefully with a resulting significant
!    speed improvement
! 27/06/2012, CP: changed modbands from 1,2,6 to 1,2,3
! 02/07/2012, GT: Bug fix. Fill grid was attempting to use the imager
!    grid to mask the MCD grid. The code now generates a mask for fill_grid
!    based on the lat-lon limits of the imager_geolocation arrays.
! 2012/07/04, CP: removed nview dependence
! 2012/07/30, CP: initialised allocated arrays
! 2012/08/07, CP: reformatted albedo array to be consistent with msi file
!    routine added that automatically selects MCD file added doy
! 2012/08/08, CP: Changed modband to 12,6 and created coxbands  2 3 4
! 2012/08/15, MJ: Speeds up code.
! 2012/08/15, GT: Added code for inserting (1.0 - emissivity) for the land
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
! 2014/01/17, MJ: Fixed doy data type from byte to stint to comply with other
!    defs
! 2014/04/20, GM: Cleaned up the code.
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/06/20, GM: Handle case when imager_geolocation%latitude or
!    imager_geolocation%longitude is equal to fill_value.
! 2014/06/11, AP: Use standard fill value rather than unique one. Use new
!    ecmwf structures.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/08/11, AP: New bilinear interpolation routines. Data now copied directly
!    into surface%albedo for the surface.
! 2014/08/13, GM: Fixed bug where the length of array arguments is larger than
!    the amount of valid data within which affects routines that get the length
!    to be processed from the size() intrinsic.
! 15/10/2014, GM: Changes related to supporting an arbitrary set of SW channels.
!    Still limited by availability from the land and ocean reflectance sources.
! 23/10/2014, OS: added support for reading full BRDF file path
! 02/12/2014, GM: Fixed handling of night.  Values that are not dependent on
!    solar zenith angle (albedo, rho_dv, and rho_dd) are computed at night.
!    Values that are (rho_0v and rho_0d) are not computed and set to fill.
!
! $Id$
!
! Bugs:
! NB channels are hardwired in this code and not selected automatically
!-------------------------------------------------------------------------------

subroutine get_surface_reflectance(cyear, cdoy, modis_surf_path, modis_brdf_path, &
     imager_flags, imager_geolocation, imager_angles, channel_info, ecmwf, &
     assume_full_path, include_full_brdf, verbose, surface)

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
  character(len=date_length),   intent(in)         :: cyear
  character(len=date_length),   intent(in)         :: cdoy
  character(len=path_length),   intent(in)         :: modis_surf_path
  character(len=path_length),   intent(in)         :: modis_brdf_path
  type(imager_flags_s),         intent(in)         :: imager_flags
  type(imager_geolocation_s),   intent(in)         :: imager_geolocation
  type(imager_angles_s),        intent(in)         :: imager_angles
  type(channel_info_s),         intent(in)         :: channel_info
  type(ecmwf_s),                intent(in)         :: ecmwf
  logical,                      intent(in)         :: assume_full_path
  logical,                      intent(in)         :: include_full_brdf
  logical,                      intent(in)         :: verbose
  type(surface_s),              intent(inout)      :: surface

  ! Local variables

  ! Land surface reflectance
  character(len=path_length)                      :: modis_surf_path_file
  character(len=path_length)                      :: modis_brdf_path_file
  real(kind=sreal), allocatable, dimension(:)     :: solzalnd, satzalnd
  real(kind=sreal), allocatable, dimension(:)     :: solazlnd, relazlnd
  type(mcd43c1)                                   :: mcdc1
  type(mcd43c3)                                   :: mcdc3
  integer, parameter                              :: n_modbands = 7
  integer, dimension(n_modbands)                  :: modbands
  integer, parameter                              :: n_coxbands = 4
  integer, dimension(n_coxbands)                  :: coxbands
  integer, allocatable, dimension(:)              :: bands
  integer                                         :: n_ref_chans
  real(kind=sreal), allocatable, dimension(:,:)   :: tmp_data
  integer(kind=byte), allocatable, dimension(:,:) :: fg_mask
  real(kind=sreal), allocatable, dimension(:,:)   :: wsalnd
  real(kind=sreal), allocatable, dimension(:,:,:) :: wgtlnd
  real(kind=sreal), allocatable, dimension(:,:,:) :: rholnd
  integer                                         :: read_ws   = 1 ! 1 = read white-sky albedo
  integer                                         :: read_bs   = 0 ! 1 = read black-sky albedo
  integer                                         :: read_brdf = 1 ! 1 = read brdf parameters
  integer                                         :: read_qc   = 0 ! 1 = read QC/ancillaty data
  integer                                         :: stat

  ! Sea surface reflectance
  real(kind=sreal), allocatable, dimension(:)     :: solzasea, satzasea
  real(kind=sreal), allocatable, dimension(:)     :: solazsea, relazsea
  real(kind=sreal), allocatable, dimension(:)     :: u10sea, v10sea
  real(kind=sreal), allocatable, dimension(:,:)   :: refsea
  real(kind=sreal), allocatable, dimension(:,:,:) :: rhosea
  type(cox_munk_shared_band_type)                 :: cox_munk_shared_band(4)
  type(cox_munk_shared_geo_wind_type)             :: cox_munk_shared_geo_wind

  ! General
  integer                                         :: i,j,k,ii,kk
  logical                                         :: flag
  integer                                         :: nsea=0, nland=0
  integer                                         :: seacount=1
  integer                                         :: lndcount=1

  type(interpol_s), allocatable, dimension(:)     :: interp

  logical,          allocatable, dimension(:,:)   :: mask

  if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_surface_reflectance()'

  if (verbose) write(*,*) 'cyear: ',             trim(cyear)
  if (verbose) write(*,*) 'cdoy: ',              trim(cdoy)
  if (verbose) write(*,*) 'modis_surf_path: ',   trim(modis_surf_path)
  if (verbose) write(*,*) 'modis_brdf_path: ',   trim(modis_brdf_path)
  if (verbose) write(*,*) 'assume_full_path: ',  assume_full_path
  if (verbose) write(*,*) 'include_full_brdf: ', include_full_brdf


  ! Mask out pixels set to fill_value and out of BRDF angular range
  allocate(mask(imager_geolocation%startx:imager_geolocation%endx, &
       1:imager_geolocation%ny))

  mask = imager_geolocation%latitude  .ne. sreal_fill_value .and. &
         imager_geolocation%longitude .ne. sreal_fill_value

  do k=1,imager_angles%nviews
     mask = mask .and. &
        imager_angles%solzen(:,:,k) .ne. sreal_fill_value .and. &
        imager_angles%satzen(:,:,k) .ne. sreal_fill_value .and. &
        imager_angles%solazi(:,:,k) .ne. sreal_fill_value .and. &
        imager_angles%relazi(:,:,k) .ne. sreal_fill_value
  end do

  ! Albedo, rho_dv, and rho_dd are all valid at night rho_0v and rho_0d are
  ! not.  At night we have two choices: (1) compute what we can (albedo, rho_dv,
  ! and rho_dd) at the expense of some processing time and set rho_0v and rho_0d
  ! to fill, or (2) set all reflectance values to fill since the main processor
  ! won't use them anyway.  The code below should be uncommented for option 2.
  ! Otherwise, the case of solzen > maxsza_twi is checked at the BRDF code level.
! do k=1,imager_angles%nviews
!    mask = mask .and. imager_angles%solzen(:,:,k) .lt. maxsza_twi
! end do

  ! Count the number of land and sea pixels, using the imager land/sea mask
  nsea  = count(mask .and. imager_flags%lsflag .eq. 0)
  nland = count(mask .and. imager_flags%lsflag .eq. 1)
  if (verbose) write(*,*) 'nsea, nland: ', nsea, nland


  !----------------------------------------------------------------------------
  ! Extract land surface reflectance
  !----------------------------------------------------------------------------
  if (nland .gt. 0) then
     ! Allocate and populate the local arrays required for land pixels
     allocate(wsalnd(channel_info%nchannels_sw,nland))
     allocate(interp(nland))
     if (include_full_brdf) then
        allocate(solzalnd(nland))
        allocate(satzalnd(nland))
        allocate(solazlnd(nland))
        allocate(relazlnd(nland))
        allocate(wgtlnd(3,channel_info%nchannels_sw,nland))
        allocate(rholnd (channel_info%nchannels_sw,nland,4))
     end if

     ! Read MODIS MCD43C3 surface reflectance data
     ! Need to set the band numbers in terms of MODIS band numbering. These are
     ! the 0.65, 0.87(ish) and 1.6 micron bands

     ! Which of these bands have been selected
     modbands = (/ 1, 2, 3, 4, 5, 6, 7 /)

     ii = 0
     do i = 1, channel_info%nchannels_sw
        flag = .true.
        do j = 1, n_modbands
           if (channel_info%map_ids_abs_to_ref_band_land(i) .eq. modbands(j)) then
              flag = .false.
              ii = ii + 1
              exit
           endif
        enddo

        if (flag .and. channel_info%channel_lw_flag(i) .eq. 0) then
           write(*,*) 'ERROR: get_surface_reflectance(), instrument ' // &
              'channel ', channel_info%channel_ids_instr(i), ' does not ' // &
              'have a corresponding land reflectance product channel'
           stop error_stop_code
        endif
     enddo
     n_ref_chans = ii

     if (verbose) write(*,*) 'n channels for land reflectance: ', n_ref_chans

     allocate(bands(n_ref_chans))

     ii = 1
     do i = 1, channel_info%nchannels_sw
        do j = 1, n_modbands
           if (channel_info%map_ids_abs_to_ref_band_land(i) .eq. modbands(j)) then
              bands(ii) = modbands(j)
              ii = ii + 1
              exit
           endif
        enddo
     enddo

     if (verbose) print *, 'intrument bands: ', channel_info%channel_ids_instr
     if (verbose) print *, 'MCD43CX bands selected: ', bands

     ! Select correct modis file
     if (verbose) write(*,*) 'find appropriate files'
     if (assume_full_path) then
       modis_surf_path_file = modis_surf_path
       modis_brdf_path_file = modis_brdf_path
     else
        call select_modis_albedo_file(cyear,cdoy,modis_surf_path, &
                                      .false.,modis_surf_path_file)
        if (include_full_brdf) then
           call select_modis_albedo_file(cyear,cdoy,modis_surf_path, &
                                         .true.,modis_brdf_path_file)
        end if
     end if
     if (verbose) write(*,*) 'modis_surf_path_file: ', trim(modis_surf_path_file)
     if (include_full_brdf) then
        if (verbose) write(*,*) 'modis_brdf_path_file: ', trim(modis_brdf_path_file)
     end if

     ! Read the data itself
     call read_mcd43c3(modis_surf_path_file, mcdc3, n_ref_chans, bands, &
                       read_ws, read_bs, read_qc, verbose, stat)

     if (include_full_brdf) then
        call read_mcd43c1(modis_brdf_path_file, mcdc1, n_ref_chans, bands, &
                          read_brdf, read_qc, verbose, stat)
     end if

     ! Fill missing data in the MODIS surface reflectance using a nearest
     ! neighbour technique. Note we cannot allocate tmp_data until we've created
     ! the mcd structure
     allocate(tmp_data(mcdc3%nlon,mcdc3%nlat))
     tmp_data=0.

     ! Also create a bit mask that controls where we apply the data filling,
     ! limiting it to the region covered by the imager data
     allocate(fg_mask(mcdc3%nlon,mcdc3%nlat))
     fg_mask = 0

     ! Locate pixels surrounding each land pixel from the imager structures
     ! Also flag required pixels from MCD array, assuming a regular grid
     lndcount = 1
     do i=1,imager_geolocation%ny
        do j=imager_geolocation%startx,imager_geolocation%endx
           if (imager_flags%lsflag(j,i) .eq. 1) then
              if (mask(j,i)) then
                 call bilinear_coef(mcdc3%lon0, mcdc3%lon_invdel, mcdc3%nlon, &
                      mcdc3%lat0, mcdc3%lat_invdel, mcdc3%nlat, &
                      imager_geolocation%longitude(j,i), &
                      imager_geolocation%latitude(j,i), interp(lndcount))

                 ! flag pixels for which there will be albedo data
                 if (abs(imager_geolocation%latitude(j,i)) < 80.2) then
                    fg_mask(interp(lndcount)%x0, interp(lndcount)%y0) = 1
                    fg_mask(interp(lndcount)%x1, interp(lndcount)%y0) = 1
                    fg_mask(interp(lndcount)%x0, interp(lndcount)%y1) = 1
                    fg_mask(interp(lndcount)%x1, interp(lndcount)%y1) = 1
                 end if

                 lndcount = lndcount + 1
              end if
           end if
        end do
     end do

     do i=1,n_ref_chans
        tmp_data = mcdc3%WSA(:,:,i)

        call fill_grid(tmp_data, sreal_fill_value, fg_mask)

        do lndcount=1,nland
           call interp_field(tmp_data, wsalnd(i,lndcount), interp(lndcount))
        end do

        if (verbose) write(*,*) 'Land WSA: channel, min, max = ', &
             i, minval(wsalnd(i,:)), maxval(wsalnd(i,:))

        if (include_full_brdf) then
           do j = 1, 3
              tmp_data = mcdc1%brdf_albedo_params(:,:,j,i)

              call fill_grid(tmp_data, sreal_fill_value, fg_mask)

              do lndcount=1,nland
                 call interp_field(tmp_data, wgtlnd(j,i,lndcount), &
                      interp(lndcount))
              end do
           end do
        end if
     end do

     if (include_full_brdf) then
        lndcount = 1
        do i=1,imager_geolocation%ny
           do j=imager_geolocation%startx,imager_geolocation%endx
              if (imager_flags%lsflag(j,i) .eq. 1) then
                 if (mask(j,i)) then
                    ! if using multiple views, require a loop over last index
                    solzalnd(lndcount) = imager_angles%solzen(j,i,1)
                    satzalnd(lndcount) = imager_angles%satzen(j,i,1)
                    solazlnd(lndcount) = imager_angles%solazi(j,i,1)
                    relazlnd(lndcount) = imager_angles%relazi(j,i,1)

                    lndcount = lndcount+1
                 end if
              end if
           end do
        end do

        call ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd &
           (n_ref_chans, solzalnd, satzalnd, solazlnd, relazlnd, wgtlnd, &
            sreal_fill_value, rholnd(:,:,1), rholnd(:,:,2), rholnd(:,:,3), &
            rholnd(:,:,4), verbose)
     end if

     ! end do ! End of instrument view loop

     ! Do some clean-up
     deallocate(bands)
     deallocate(tmp_data)
     deallocate(fg_mask)
     deallocate(interp)
     call deallocate_mcd43c3(mcdc3)
     if (include_full_brdf) then
        deallocate(solzalnd)
        deallocate(satzalnd)
        deallocate(solazlnd)
        deallocate(relazlnd)
        deallocate(wgtlnd)
        call deallocate_mcd43c1(mcdc1)
     end if

  end if ! End of land surface reflectance setting

  !----------------------------------------------------------------------------
  ! Compute sea surface reflectance
  !----------------------------------------------------------------------------
  if (nsea .gt. 0) then
     ! Allocate and populate the local arrays required for sea pixels
     allocate(solzasea(nsea))
     allocate(satzasea(nsea))
     allocate(solazsea(nsea))
     allocate(relazsea(nsea))
     allocate(u10sea(nsea))
     allocate(v10sea(nsea))
     allocate(refsea(channel_info%nchannels_sw,nsea))
     if (include_full_brdf) then
        allocate(rhosea(channel_info%nchannels_sw,nsea,4))
     end if

     ! Which channels do we need?

     coxbands = (/ 1, 2, 6, 20/)

     ii = 0
     do i = 1, channel_info%nchannels_sw
        flag = .true.
        do j = 1, n_coxbands
           if (channel_info%map_ids_abs_to_ref_band_sea(i) .eq. coxbands(j)) then
              flag = .false.
              ii = ii + 1
              exit
           endif
        enddo

         if (flag) then
           write(*,*) 'ERROR: get_surface_reflectance(), instrument ' // &
             'channel ', channel_info%channel_ids_instr(i), ' does not ' // &
              'have a corresponding ocean reflectance product channel'
           stop error_stop_code
        endif
     enddo
     n_ref_chans = ii

     if (verbose) write(*,*) 'n channels for sea reflectance: ', n_ref_chans

     allocate(bands(n_ref_chans))

     ii = 1
     do i = 1, channel_info%nchannels_sw
        do j = 1, n_coxbands
           if (channel_info%map_ids_abs_to_ref_band_sea(i) .eq. coxbands(j)) then
              bands(ii) = j
              ii = ii + 1
              exit
           endif
        enddo
     enddo

     ! Interpolate the ECMWF wind fields onto the instrument grid
     seacount = 1
     allocate(interp(1))
     do i=1,imager_geolocation%ny
        do j=imager_geolocation%startx,imager_geolocation%endx
           if (imager_flags%lsflag(j,i) .eq. 0) then
              if (mask(j,i)) then
                 call bilinear_coef(ecmwf%lon, ecmwf%xdim, ecmwf%lat, &
                      ecmwf%ydim, imager_geolocation%longitude(j,i), &
                      imager_geolocation%latitude(j,i), interp(1))

                 call interp_field(ecmwf%u10, u10sea(seacount), interp(1))
                 call interp_field(ecmwf%v10, v10sea(seacount), interp(1))

                 ! Extract only the sea pixels from the imager structures
                 solzasea(seacount) = imager_angles%solzen(j,i,1)
                 satzasea(seacount) = imager_angles%satzen(j,i,1)
                 solazsea(seacount) = imager_angles%solazi(j,i,1)
                 relazsea(seacount) = imager_angles%relazi(j,i,1)

                 seacount = seacount+1
              end if
           end if
        end do
     end do
     deallocate(interp)

     do i = 1, n_ref_chans
        call cox_munk3_calc_shared_band(i, cox_munk_shared_band(i))
     end do

     do j = 1, nsea
        call cox_munk3_calc_shared_geo_wind &
           (solzasea(j), satzasea(j), solazsea(j), relazsea(j), &
            u10sea(j), v10sea(j), cox_munk_shared_geo_wind)

        do i = 1, n_ref_chans
           call cox_munk3(i, cox_munk_shared_band(i), &
                             cox_munk_shared_geo_wind, refsea(i, j))
        end do
     end do

     if (verbose) then
        do i = 1, n_ref_chans
           write(*,*) 'Sea reflectance: channel, min, max = ', &
                      i, minval(refsea(i,:)), maxval(refsea(i,:))
        end do
     end if

     if (include_full_brdf) then
        call cox_munk_rho_0v_0d_dv_and_dd(bands, solzasea, satzasea, &
           solazsea, relazsea, u10sea, v10sea, sreal_fill_value, &
           rhosea(:,:,1), rhosea(:,:,2), rhosea(:,:,3), rhosea(:,:,4), verbose)
     end if

     ! Tidy up cox_munk input arrays
     deallocate(solzasea)
     deallocate(satzasea)
     deallocate(solazsea)
     deallocate(relazsea)
     deallocate(u10sea)
     deallocate(v10sea)
     deallocate(bands)
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
              end if

              ! The MCD43CX products do not provide reflectance in the mixed
              ! channnels therefore we use 1-emissivity as the land surface
              ! albedo or rho_dd
              kk = 1
              do k = 1, channel_info%nchannels_sw
                 if (channel_info%channel_sw_flag(k) .ne. 0 .and. &
                     channel_info%channel_lw_flag(k) .ne. 0) then
                    surface%albedo(j,i,k) = 1.0 - surface%emissivity(j,i,kk)

                    if (include_full_brdf) then
                       surface%rho_0v(j,i,k) = 0.
                       surface%rho_0d(j,i,k) = 0.
                       surface%rho_dv(j,i,k) = 0.
                       surface%rho_dd(j,i,k) = 1.0 - surface%emissivity(j,i,kk)
                    end if
                    kk = kk + 1
                 endif
              enddo

              lndcount = lndcount+1

           else

              surface%albedo(j,i,:) = refsea(:,seacount)

              if (include_full_brdf) then
                 surface%rho_0v(j,i,:) = rhosea(:,seacount,1)
                 surface%rho_0d(j,i,:) = rhosea(:,seacount,2)
                 surface%rho_dv(j,i,:) = rhosea(:,seacount,3)
                 surface%rho_dd(j,i,:) = rhosea(:,seacount,4)
              end if

              seacount = seacount+1
           end if
        end if
     end do
  end do


  ! Check to see if the land and sea reflectance arrays have been created
  ! before deallocating them
  deallocate(mask)

  if (allocated(wsalnd)) deallocate(wsalnd)
  if (allocated(rholnd)) deallocate(rholnd)

  if (allocated(refsea)) deallocate(refsea)
  if (allocated(rhosea)) deallocate(rhosea)


  if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_surface_reflectance()'

end subroutine get_surface_reflectance
