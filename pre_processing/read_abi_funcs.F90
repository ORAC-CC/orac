!-------------------------------------------------------------------------------
! Name: read_abi_funcs.F90
!
! Purpose:
! Contains functions for processing data from GOES-16 onwards
! Here we have functions to:
!    1) Find the filenames for each GOES channel
!    2) Compute the lat/lon for each pixel in the image
!    3) Compute the viewing geometry
!    4) Compute the solar geometry
!    5) Resample VIS bands to TIR pixel size (0.5/1km to 2km)
!
! History:
! 2018/02/10, SP: First version.
! 2018/04/04, SP: Geolocation updates.
! 2018/06/08, SP: New global attribute to store satellite position information
! 2018/10/26, SP: Rename to prevent clashes with GOES-Imager reader
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------


! This function computes the solar geometry for a given GOES scene
subroutine ABI_Solpos(year, doy, hour, minute, lat, lon, sza, saa)
!$acc routine seq

   use preproc_constants_m
   use solar_position_m

   implicit none

   real(kind=sreal), intent(in)  :: year
   real(kind=sreal), intent(in)  :: doy
   real(kind=sreal), intent(in)  :: hour
   real(kind=sreal), intent(in)  :: minute
   real(kind=sreal), intent(in)  :: lat
   real(kind=sreal), intent(in)  :: lon
   real(kind=sreal), intent(out) :: sza
   real(kind=sreal), intent(out) :: saa

   saa    = 0.
   sza    = 0.
   call sun_pos_calc(year, doy, hour+minute/60., lat, lon, sza, saa)

   if (saa .gt. 360.0) then
      saa = sreal_fill_value
   end if
   if (saa .lt. 0.0) then
      saa = sreal_fill_value
   end if
!   sza = abs(sza)
   if (sza .gt. 180.0) then
      sza = sreal_fill_value
   end if
   if (sza .lt. -180.0) then
      sza = sreal_fill_value
   end if

   return

end subroutine ABI_Solpos

! This subroutine works out, and checks the existence of, each GOES file that
! needs to be read. GOES data is stored as one file per band, so the l1_5_file
! variable stores only one out of (up to) 16 channel filenames.
! The output array abi_filenames contains each filename for reading.
subroutine get_abi_path(l1_5_file, platform, abi_filenames, n_chans, channel_ids)

   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*), intent(in)  :: l1_5_file
   character(len=*), intent(in)  :: platform
   character(len=*), intent(out) :: abi_filenames(:)
   integer,          intent(in)  :: n_chans
   integer, pointer, intent(in)  :: channel_ids(:)

   character(len=file_length)    :: tmp_file
   character(len=path_length)    :: regex

   integer           :: i, index1, index3, success
   character(len=2)  :: band
   character(len=3)  :: shplat
   character(len=16) :: dtstr

   ! Determine which GOES platform we're using. Currently -16 or -17 are acceptable
   if (platform .eq. "GOES-16") then
      shplat = "G16"
   elseif (platform .eq. "GOES-17") then
      shplat = "G17"
   else
      write(*,*) "Unsupported GOES platform: ", platform
      stop
   end if

   ! Some useful positions in the file
   ! Location of the datestring
   index1 = index(l1_5_file, '_s', back=.true.)
   ! Starting location of the actual filename
   index3 = index(l1_5_file, 'OR_', back=.true.)

   ! Extract the datestring (including '_s' prefix)
   dtstr  = l1_5_file(index1:index1+16)

   ! Loop over all bands to read
   do i = 1, n_chans
      write(band, '(i2.2)') channel_ids(i)

      ! Find channel filenames. This isn't as simple as dropping 'C01' etc into the existing filename
      ! As each file contains a timestamp of creation, which differs between channels. Similar to VIIRS.
      regex = "OR_ABI-L1b-RadF-M.C"//band//"_"//shplat//dtstr//"................................\.nc"
      success = match_file(l1_5_file(1:index3-1), regex, tmp_file)
      if (success .eq. 0) then
         abi_filenames(i) = l1_5_file(1:index3-1)//trim(tmp_file)
      else
         regex = "OR_ABI-L1b-RadC-M.C"//band//"_"//shplat//dtstr//"................................\.nc"
         success = match_file(l1_5_file(1:index3-1), regex, tmp_file)
         if (success .eq. 0) then
            abi_filenames(i) = l1_5_file(1:index3-1)//trim(tmp_file)
         else
            regex = "OR_ABI-L1b-RadM.-M.C"//band//"_"//shplat//dtstr//"................................\.nc"
            success = match_file(l1_5_file(1:index3-1), regex, tmp_file)
            if (success .eq. 0) then
               abi_filenames(i) = l1_5_file(1:index3-1)//trim(tmp_file)
            else
               write(*,*) "Cannot find the GOES file for band ", band
               write(*,*) "Regex:", trim(regex)
               stop
            end if
         end if
      end if
   end do

end subroutine get_abi_path


subroutine get_abi_geoloc(infile, imager_geolocation, imager_angles, &
     global_atts, verbose)

   use channel_structures_m
   use global_attributes_m
   use iso_c_binding
   use imager_structures_m
   use orac_ncdf_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*),           intent(in)    :: infile
   type(imager_geolocation_t), intent(inout) :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles
   type(global_attributes_t),  intent(inout) :: global_atts
   logical,                    intent(in)    :: verbose

   integer :: fid, ierr
   integer :: gimpid
   integer :: i, j

   real    :: sma, smi, invf, e, hproj, h, lon0, lat0
   real    :: a, b, c, rs, sx, sy, sz, tlat, tlon, tx, ty

   character(len=10) :: satlat, satlon, sathei, eqrrad, polrad

   real, dimension(:), allocatable :: x, y

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_abi_geoloc()'

   allocate(x(imager_geolocation%startx:imager_geolocation%endx))
   allocate(y(imager_geolocation%starty:imager_geolocation%endy))

   e = 0.081819191

   if (verbose) write(*,*) "Computing latitude and longitude for each pixel"

   ! Open the netCDF4 file for reading
   call ncdf_open(fid, infile, 'get_abi_geoloc()')

   call ncdf_read_array(fid, "x", x, verbose, start=[imager_geolocation%startx])
   call ncdf_read_array(fid, "y", y, verbose, start=[imager_geolocation%starty])

   ! Read the various attributes required for building the geolocation model
   ierr = nf90_inq_varid(fid, "goes_imager_projection", gimpid)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_geoloc(): Error reading goes_imager_projection variable id', trim(infile)
      stop error_stop_code
   end if

   ! Read the relevant attributes, see GOES PUG, Volume 4: GRB section 7.1.2.8.1 for details of what
   ! these are.
   ierr = nf90_get_att(fid, gimpid, "semi_major_axis", sma)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_geoloc(): Error reading semi_major_axis attribute', trim(infile)
      stop error_stop_code
   end if
   ierr = nf90_get_att(fid, gimpid, "semi_minor_axis", smi)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_geoloc(): Error reading semi_minor_axis attribute', trim(infile)
      stop error_stop_code
   end if
   ierr = nf90_get_att(fid, gimpid, "inverse_flattening", invf)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_geoloc(): Error reading inverse_flattening attribute', trim(infile)
      stop error_stop_code
   end if
   ierr = nf90_get_att(fid, gimpid, "perspective_point_height", hproj)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_geoloc(): Error reading perspective_point_height attribute', trim(infile)
      stop error_stop_code
   end if
   ierr = nf90_get_att(fid, gimpid, "longitude_of_projection_origin", lon0)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_geoloc(): Error reading longitude_of_projection_origin attribute', trim(infile)
      stop error_stop_code
   end if
   ierr = nf90_get_att(fid, gimpid, "latitude_of_projection_origin", lat0)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_geoloc(): Error reading latitude_of_projection_origin attribute', trim(infile)
      stop error_stop_code
   end if

   ! Close the netCDF file, we have all we need
   call ncdf_close(fid, 'get_abi_geoloc()')

   ! We need the height above geoid centre, so sat altitude + earth radius
   h = sma + hproj

   ! The 'c' parameter (P52 of PUG) can be pre-computed
   c = h*h - sma*sma

   ! Convert lon_0 into radians
   lon0 = lon0 * pi / 180.
   lat0 = lat0 * pi / 180.

   ! Generate the satellite position string for global_atts
   write(satlat, '(f10.7)') lat0
   write(satlon, '(f10.7)') lon0
   write(sathei, '(f10.0)') h
   write(eqrrad, '(f10.0)') sma
   write(polrad, '(f10.0)') sma * (1.-(1./invf))
   global_atts%Satpos_Metadata = satlat//","//satlon//","//sathei//','// &
        eqrrad//","//polrad

   !$OMP PARALLEL PRIVATE(i, j, a, b, rs, sx, sy, sz, tlat, tlon, tx, ty)
   !$OMP DO SCHEDULE(GUIDED)
   do i = imager_geolocation%startx, imager_geolocation%endx
      do j = imager_geolocation%starty, imager_geolocation%endy
         ! These are not needed really, but help make code readable
         tx = x(i)
         ty = y(j)
         ! These are ugly, all from PUG page 52
         a = ( sin(tx) * sin(tx)) + (cos(tx) * cos(tx) * ((cos(ty) * cos(ty)) + &
              (((sma*sma) / (smi*smi)) * sin(ty) * sin(ty))))

         b = -2 * h * cos(tx) * cos(ty)

         rs = (-b - sqrt(b*b - 4*a*c)) / (2*a)

         sx = rs * cos(tx) * cos(ty)
         sy = -rs * sin(tx)
         sz = rs * cos(tx) * sin(ty)

         tlat = atan( ((sma*sma) / (smi * smi)) * sz / (sqrt((h-sx)*(h-sx) + &
              sy*sy)))
         tlon = lon0 - atan( sy / (h-sx) )

         tlat = tlat * 180. / pi
         tlon = tlon * 180. / pi

         if (tlat .lt. -90 .or. tlat .gt. 90) tlat = sreal_fill_value
         if (tlon .lt. -180 .or. tlon .gt. 180) tlon = sreal_fill_value
         if (is_nan(tlat)) tlat = sreal_fill_value
         if (is_nan(tlon)) tlon = sreal_fill_value

         imager_geolocation%latitude(i, j-imager_geolocation%starty+1) = tlat
         imager_geolocation%longitude(i, j-imager_geolocation%starty+1) = tlon
      end do
   end do
   !$OMP END DO
   !$OMP END PARALLEL

   ! Now we compute the viewing geometry
   call get_abi_viewing_geom(imager_geolocation, imager_angles, sma, smi, &
        hproj, lon0, verbose)

   ! Deallocate temporary variables
   deallocate(x)
   deallocate(y)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_abi_geoloc()'

end subroutine get_abi_geoloc

subroutine get_abi_viewing_geom(imager_geolocation, imager_angles, sma, smi, &
     hproj, l0, verbose)

   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   type(imager_geolocation_t), intent(inout) :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles
   real,                       intent(in)    :: sma
   real,                       intent(in)    :: smi
   real,                       intent(in)    :: hproj
   real,                       intent(inout) :: l0
   logical,                    intent(in)    :: verbose

   real :: a, b, e2

   real, dimension(imager_geolocation%startx:imager_geolocation%endx, 1:imager_geolocation%ny) :: N, xp, yp, zp, cos_lat, sin_lat, cos_lon, sin_lon, qv1, qv2, qv3, u1, u2, u3

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_abi_viewing_geom()'

   a = sma
   b = smi

   cos_lat = cos(imager_geolocation%latitude * pi / 180.)
   sin_lat = sin(imager_geolocation%latitude * pi / 180.)
   cos_lon = cos(imager_geolocation%longitude * pi / 180. - l0)
   sin_lon = sin(imager_geolocation%longitude * pi / 180. - l0)

   e2 = 1. - (b * b) / (a * a)

   N = a / sqrt(1. - e2 * sin_lat * sin_lat)

   xp = N * cos_lat * cos_lon
   yp = N * cos_lat * sin_lon
   zp = ((1-e2) * N) * sin_lat

   qv1 = (sma + hproj) * cos(0.)
   qv2 = (sma + hproj) * sin(0.)
   qv3 = 0

   qv1 = qv1 - xp
   qv2 = qv2 - yp
   qv3 = qv3 - zp

   !s, e, z
   u1 = (-sin_lat * cos_lon * qv1) + (-sin_lat * sin_lon * qv2) + (cos_lat * qv3)
   u2 = (-sin_lon *           qv1) + ( cos_lon           * qv2)
   u3 = ( cos_lat * cos_lon * qv1) + (-cos_lat * sin_lon * qv2) + (sin_lat * qv3)

   imager_angles%satzen(:, :, 1) = acos(u3 / sqrt(u1*u1 + u2*u2 + u3*u3)) * 180. / pi
   imager_angles%satazi(:, :, 1) = atan2(-u2, u1) * 180. / pi
   imager_angles%satazi(:, :, 1) = imager_angles%satazi(:, :, 1) + 180.

   where (imager_angles%satazi .le. 180 .and. imager_angles%satazi  .ge. 0)
      imager_angles%satazi = abs(180. - imager_angles%satazi)
   end where

   where (imager_angles%satazi .le. 360 .and. imager_angles%satazi  .ge. 180)
      imager_angles%satazi = (360. - imager_angles%satazi) + 180.
   end where

   where (imager_angles%satazi .gt. 360)
      imager_angles%satazi = 360 - imager_angles%satazi
   end where

   where (imager_angles%satazi .lt. 0)
      imager_angles%satazi = abs(imager_angles%satazi)
   end where

!   where (imager_angles%satzen .gt. 180)
!      imager_angles%satzen = sreal_fill_value
!   end where

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_abi_viewing_geom()'

end subroutine get_abi_viewing_geom

! This computes the solar geometry for each pixel
subroutine get_abi_solgeom(imager_time, imager_angles, imager_geolocation, verbose)

   use imager_structures_m
   use preproc_constants_m
   use solar_position_m
   use calender_m

   implicit none

   type(imager_geolocation_t), intent(in)    :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles
   type(imager_time_t),        intent(in)    :: imager_time
   logical,                    intent(in)    :: verbose

   real(kind=sreal)   :: sza, saa, doy
   integer            :: x, y, line0, line1, column0, column1
   integer            :: mid_c, mid_l
   integer(kind=sint) :: iye, mon, idy, ihr, minu
   real(kind=dreal)   :: dfr, tmphr
   real(kind=sreal)   :: rye, rhr, rminu

   real(kind=sreal), dimension(:,:), allocatable :: tlat, tlon, tsza, tsaa

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_abi_solgeom()'

   line0  = imager_geolocation%startx
   line1  = imager_geolocation%endx
   column0 = imager_geolocation%starty
   column1 = imager_geolocation%endy

   mid_l  = line0 + (line1 - line0) / 2
   mid_c  = (column1 - column0) / 2

   allocate(tlat(imager_geolocation%startx:imager_geolocation%endx,1:imager_geolocation%ny))
   allocate(tlon(imager_geolocation%startx:imager_geolocation%endx,1:imager_geolocation%ny))
   allocate(tsza(imager_geolocation%startx:imager_geolocation%endx,1:imager_geolocation%ny))
   allocate(tsaa(imager_geolocation%startx:imager_geolocation%endx,1:imager_geolocation%ny))

   tlat = imager_geolocation%latitude(:,:)
   tlon = imager_geolocation%longitude(:,:)

   ! Here we compute the time for each pixel, GOES scans S-N so each line has a different time
   call JD2GREG(imager_time%time(mid_l, mid_c), iye, mon, dfr)
   idy = int(dfr)
   tmphr = (dfr-idy)*24.
   ihr = int(tmphr)
   tmphr = (tmphr-ihr)*60.
   minu = int(tmphr)

   call get_day_of_year(float(idy), float(mon), float(iye), doy)

   rye = float(iye)
   rhr = float(ihr)
   rminu = float(minu)

   ! This section computes the solar geometry for each pixel in the image
#ifdef _OPENMP
   if (verbose) write(*,*) "Computing solar geometry using OpenMP"
   !$OMP PARALLEL DO PRIVATE(y, x, iye, mon, dfr, idy, tmphr, ihr, minu, sza, saa)
#endif
#ifdef __ACC
   if (verbose) write(*,*) "Computing solar geometry using PGI_ACC"
!$acc data copyin(tlat) copyin(tlon) copyout(tsza) copyout(tsaa)
!$acc parallel
!$acc loop collapse(2) independent private(y, x, iye, mon, dfr, idy, tmphr, ihr, minu, sza, saa)
#endif
   do y = 1, imager_geolocation%ny
      do x = imager_geolocation%startx, imager_geolocation%endx

         ! We can now use this time to retrieve the actual solar geometry
         if (tlat(x,y) .gt. -90. .and. tlon(x,y) .gt. -180.) then
             call ABI_Solpos(rye, doy, rhr, rminu, tlat(x,y), tlon(x,y), sza, saa)
             tsza(x,y) = sza
             tsaa(x,y) = saa
         else
            tsza(x,y) = sreal_fill_value
            tsaa(x,y) = sreal_fill_value
         end if
      end do
   end do
#ifdef _OPENMP
   !$OMP END PARALLEL DO
#endif
#ifdef __ACC
!$acc end parallel
!$acc end data
#endif

   imager_angles%solzen(:, :, 1) = tsza
   imager_angles%solazi(:, :, 1) = tsaa

   where(imager_angles%solazi(:, :, 1) .ne. sreal_fill_value .and. &
        imager_angles%relazi(:, :, 1) .ne. sreal_fill_value)
!      imager_angles%solazi(:, :, 1) = imager_angles%solazi(:, :, 1) - 180.
!      where(imager_angles%solazi(:, :, 1) .lt. 0.)
!         imager_angles%solazi(:, :, 1) = imager_angles%solazi(:, :, 1) + 360.
!      end where
   end where

   deallocate(tlat)
   deallocate(tlon)
   deallocate(tsza)
   deallocate(tsaa)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_abi_solgeom()'

end subroutine get_abi_solgeom



! Resamples VIS data onto the TIR grid. Assumes that VIS resolution is twice
! or four times that of the TIR images (0.5km vs 1km).
! This is not efficient, but works!
!
! To resample we simply average a NxN region of VIS into a 1x1 pixel of TIR.
subroutine goes_resample_vis_to_tir(inarr, outarr, nx, ny, fill, scl, verbose)

   use omp_lib
   use preproc_constants_m

   implicit none

   integer,          intent(in)  :: nx
   integer,          intent(in)  :: ny
   real,             intent(in)  :: fill
   integer,          intent(in)  :: scl
   logical,          intent(in)  :: verbose
   real(kind=sreal), intent(in)  :: inarr(:,:)
   real(kind=sreal), intent(out) :: outarr(:,:)

#ifdef _OPENMP
   integer :: n_threads
#endif
   integer :: x, y
   integer :: outx, outy

   integer :: i, j
   real    :: val
   integer :: inpix

   outarr(:, :) = 0
#ifdef _OPENMP
   if (verbose) write(*,*) "Resampling VIS grid to IR grid using OpenMP"
   !$OMP PARALLEL DO PRIVATE(y, x, outx, outy, val, inpix)
#endif
#ifdef __ACC
   if (verbose) write(*,*) "Resampling VIS grid to IR grid using PGI_ACC"
!$acc data copyin(inarr(1:nx*scl,1:ny*scl))  copyout(outarr(1:nx,1:ny))
!$acc parallel
!$acc loop collapse(2) independent private(x, y, outx, outy, val, inpix,i,j)
#endif
   do x = 1, (nx*scl)-scl
      do y = 1, (ny*scl)-scl
         outx = int(x/scl)+1
         outy = int(y/scl)+1
         val = 0
         inpix= 0
         !$acc loop collapse(2)
         do i = 1, scl
            do j = 1, scl
               if (inarr(x+i, y+j) .gt. sreal_fill_value) then
                  val = val + inarr(x+i, y+j)
                  inpix= inpix + 1
               end if
            end do
         end do
         val = val/inpix
         if (outx .le. 0 .or. outx .ge. nx .or. &
              outy .le. 0 .or. outy .ge. ny) then
            cycle
         end if

         if (outarr(outx, outy).le. 0) then
            outarr(outx, outy) = val
         end if
      end do

   end do

#ifdef _OPENMP
   !$OMP END PARALLEL DO
#endif
#ifdef __ACC
!$acc end parallel
!$acc end data
#endif

end subroutine goes_resample_vis_to_tir



! This reads data for one band from its individual netcdf file
subroutine load_abi_band(infile, imager_geolocation, rad, kappa, bc1, bc2, fk1, fk2, scl, verbose)

   use orac_ncdf_m
   use imager_structures_m
   use preproc_constants_m

   implicit none

   character(len=*),           intent(in) :: infile
   type(imager_geolocation_t), intent(in) :: imager_geolocation
   real,       intent(out) :: rad(:, :)
   real,       intent(out) :: kappa
   real,       intent(out) :: bc1
   real,       intent(out) :: bc2
   real,       intent(out) :: fk1
   real,       intent(out) :: fk2
   integer,    intent(in)  :: scl
   logical,    intent(in)  :: verbose

   byte, allocatable       :: dqf(:,:)


   integer  :: fid
   integer  :: x0, x1, y0, y1, nx, ny

   rad(:, :) = sreal_fill_value
   kappa  = sreal_fill_value
   bc1  = sreal_fill_value
   bc2  = sreal_fill_value
   fk1  = sreal_fill_value
   fk2  = sreal_fill_value

   x0   = (imager_geolocation%startx)*scl-scl+1
   x1   = (imager_geolocation%endx)*scl
   y0   = (imager_geolocation%starty)*scl-scl+1
   y1   = (imager_geolocation%endy)*scl

   nx   = x1-x0+1
   ny   = y1-y0+1

   allocate(dqf(nx,ny))

   ! Open the netCDf4 file for access
   call ncdf_open(fid, infile, 'load_abi_band()')

   call ncdf_read_array(fid, 'Rad', rad, verbose, start=[x0, y0])
   call ncdf_read_array(fid, 'DQF', dqf, verbose, start=[x0, y0])
   call ncdf_read_array(fid, 'kappa0', kappa, verbose)
   call ncdf_read_array(fid, 'planck_bc1', bc1, verbose)
   call ncdf_read_array(fid, 'planck_bc2', bc2, verbose)
   call ncdf_read_array(fid, 'planck_fk1', fk1, verbose)
   call ncdf_read_array(fid, 'planck_fk2', fk2, verbose)

   call ncdf_close(fid, 'load_abi_band()')

   where(dqf .lt. 0 .or. dqf .gt. 1)
      rad = sreal_fill_value
   end where

   deallocate(dqf)

end subroutine load_abi_band

! This gets the timestamp associated with each line of the image
subroutine get_abi_time(infile, imager_time, ny, verbose)

   use imager_structures_m
   use calender_m
   use orac_ncdf_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   character(len=*),    intent(in)    :: infile
   type(imager_time_t), intent(inout) :: imager_time
   integer,             intent(in)    :: ny
   logical,             intent(in)    :: verbose

   integer                    :: fid, ierr, j
   character(len=21)          :: start_coverage
   character(len=21)          :: end_coverage

   integer(kind=sint)         :: year1, month1, day1
   integer(kind=sint)         :: year2, month2, day2
   integer(kind=sint)         :: hour1, minute1, second1
   integer(kind=sint)         :: hour2, minute2, second2
   real(kind=dreal)           :: dfrac1, dfrac2, jd1, jd2, slo

   character(len=var_length), parameter :: date_format = &
        '(I4, T1, I2, T1, I2, T1, I2, T1, I2, T1, I2)'

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering get_abi_time()'

   ! Read the time boundaries from the input file
   call ncdf_open(fid, infile, 'get_abi_time()')

   ierr = nf90_get_att(fid, NF90_GLOBAL, "time_coverage_start", start_coverage)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_time(): Error reading time_coverage_start attribute', trim(infile)
      stop error_stop_code
   end if
   ierr = nf90_get_att(fid, NF90_GLOBAL, "time_coverage_end", end_coverage)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_abi_time(): Error reading time_coverage_end attribute', trim(infile)
      stop error_stop_code
   end if

   ! Close the netCDF file, we have all we need
   call ncdf_close(fid, 'get_abi_time()')

   ! Get year, doy, hour and minute as integers
   read(start_coverage, date_format) year1, month1, day1, hour1, minute1, second1
   read(end_coverage, date_format) year2, month2, day2, hour2, minute2, second2

   call GREG2JD(year1, month1, day1, jd1)
   call GREG2JD(year2, month2, day2, jd2)

   ! Add on a fraction to account for the start / end times
   dfrac1 = (float(hour1)/24.0) + (float(minute1)/(24.0*60.0)) + &
        (float(second1)/(24.0*60.0*60.0))
   dfrac2 = (float(hour2)/24.0) + (float(minute2)/(24.0*60.0)) + &
        (float(second2)/(24.0*60.0*60.0))
   jd1    = jd1 + dfrac1
   jd2    = jd2 + dfrac2

   ! Compute linear regression slope
   slo = (jd2-jd1)/ny

   ! Put correct julian date into each location in the time array
   do j = 1, ny
      imager_time%time(:, j) = jd1+(slo*float(j))
   end do

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving get_abi_time()'

end subroutine get_abi_time
