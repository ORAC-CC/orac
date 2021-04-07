!-------------------------------------------------------------------------------
! Name: read_slstr_funcions.F90
!
! Purpose:
! Contains functions for processing SLSTR images
! Here we have functions to:
!    1) Figure out the start and end times from the input image file
!    2) Choose filenames for reading
!    3) Downsample VIS data to TIR grid
!    4) Compute the zenith/azimuth angles on the TIR grid
!
! History:
! 2016/06/14, SP: First version.
! 2016/07/07, SP: Updated to make the code more sensible, some efficiency
!    improvements too, should be more accurate for VIS resampling
! 2016/07/13, SP: Fix so that code doesn't crash when dealing with SLSTR's
!    occasionally nonsensical array sizes.
! 2016/07/20, SP: Read the solar irradiance data from file rather than assuming
!    pre-launch default values.
! 2016/07/22, SP: Implement second (oblique) view. Also, correct initial version
!    of the geometry resampling. Now operates more accurately.
! 2016/08/01, SP: Changed zenith angle bounds, fixed RAA for second view
! 2016/09/14, SP: Corrections for night-time processing
! 2016/09/16, SP: Added openMP for a small performance boost
! 2016/11/23, SP: Fixed interpolation issue for azimuth angles
! 2016/11/24, SP: Added fudge factor to correct for bad SLSTR colocation
! 2016/11/26, SP: Disable fudge factor, awaiting EUMETSAT fix
! 2017/05/12, SP: Added 'correction' to bypass problems with S7 channel.
!                 Now bad data (T>305K) is replaced by values from F1 channel.
!                 This is not elegant due to F1 issues, but better than no data.
! 2017/11/15, SP: Add feature to give access to sensor azimuth angle
! 2020/02/24, SP: Remove the S7/F1 correction, as new processing baseline makes
!                 this harder to implement, but also makes it less necessary.
!                 The S7 band now saturates at 312K rather than 305K.
! 2020/04/14, AP: Add subsetting to the read functions.
! 2020/16/07, AP: Relative azimuth should be 180 when looking into the sun.
!
! Bugs:
! SLSTR colocation is poor. Aerosol retrieval unusable. Cloud retrieval suspect.
!-------------------------------------------------------------------------------

! This function retrieves the start and end times for an SLSTR scene, then
! computes the per-pixel sensing time before saving the results in the
! imager_time%time array.
subroutine get_slstr_startend(imager_time, fname, starty, ny)

   use orac_ncdf_m
   use calender_m
   use imager_structures_m

   implicit none

   type(imager_time_t), intent(inout) :: imager_time
   character(len=*),    intent(in)    :: fname
   integer,             intent(in)    :: starty
   integer,             intent(in)    :: ny


   integer(kind=sint)         :: year1, month1, day1
   integer(kind=sint)         :: year2, month2, day2
   integer(kind=sint)         :: hour1, minute1, second1
   integer(kind=sint)         :: hour2, minute2, second2
   real(kind=dreal)           :: dfrac1, dfrac2, jd1, jd2, slo

   integer                    :: fid, ierr, j
   integer                    :: n_across_track, n_along_track
   character(len=path_length) :: l1b_start, l1b_end

   ! This ignores the 6dp on seconds and Z suffix
   character(len=var_length), parameter :: date_format = &
        '(I4, X, I2, X, I2, X, I2, X, I2, X, I2)'

   call read_slstr_dimensions(fname, n_across_track, n_along_track, .false.)

   ! Convert start and end times to julian

   call ncdf_open(fid, fname, 'get_slstr_startend()')
   ierr = nf90_get_att(fid, NF90_GLOBAL, 'start_time', l1b_start)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_slstr_startend(): Error getting start_time from file ', &
           trim(fname), ierr
      stop error_stop_code
   end if
   ierr = nf90_get_att(fid, NF90_GLOBAL, 'stop_time', l1b_end)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: get_slstr_startend(): Error getting end_time from file ', &
           trim(fname), ierr
      stop error_stop_code
   end if
   call ncdf_close(fid, 'read_slstr_startend()')

   read(l1b_start, date_format) year1, month1, day1, hour1, minute1, second1
   read(l1b_end, date_format) year2, month2, day2, hour2, minute2, second2

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
   slo = (jd2 - jd1) / n_along_track

   ! Put correct julian date into each location in the time array
   do j = 1, ny
      imager_time%time(:,j) = jd1 + (slo * float(j + starty - 1))
   end do

end subroutine get_slstr_startend


! Computes the name of the input file based upon the band name.
subroutine get_slstr_imnames(indir, inband, fname, fname_qa, bname, irradname)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: indir
   integer,          intent(in)  :: inband
   character(len=*), intent(out) :: fname
   character(len=*), intent(out) :: fname_qa
   character(len=*), intent(out) :: bname
   character(len=*), intent(out) :: irradname

   character(len=3) :: band
   character(len=3) :: vid

   integer          :: mod_inb

   if (inband .le. 9) then
      vid = 'n'
      mod_inb = inband
   else
      vid = 'o'
      mod_inb = inband-9
   end if

   write (band, '(I1)') mod_inb
   if (mod_inb .le. 6) then
      fname       = trim(indir)//'S'//trim(band)//'_radiance_a'//trim(vid)//'.nc'
      fname_qa    = trim(indir)//'S'//trim(band)//'_quality_a'//trim(vid)//'.nc'
      bname       = 'S'//trim(band)//'_radiance_a'//trim(vid)
      irradname   = 'S'//trim(band)//'_solar_irradiance_a'//trim(vid)
   else if (mod_inb .le. 9) then
      fname       = trim(indir)//'S'//trim(band)//'_BT_i'//trim(vid)//'.nc'
      fname_qa    = trim(indir)//'S'//trim(band)//'_quality_i'//trim(vid)//'.nc'
      bname       = 'S'//trim(band)//'_BT_i'//trim(vid)
      irradname   = 'NONE'
   else
      print*, 'Incorrect band:', inband, &
           '. This is not supported for SLSTR (1-18 only).'
      stop error_stop_code
   end if

end subroutine get_slstr_imnames

! Read the nadir-view thermal grid data
subroutine read_slstr_tirdata(indir, inband, outarr, sx, sy)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: indir
   integer,          intent(in)  :: inband
   integer,          intent(in)  :: sx
   integer,          intent(in)  :: sy
   real(kind=sreal), intent(out) :: outarr(:,:)

   character(len=path_length) :: filename
   character(len=path_length) :: filename_qa
   character(len=path_length) :: bandname
   character(len=path_length) :: irradname

   integer                    :: fid

   ! Find the filename required for this channel
   call get_slstr_imnames(indir, inband, filename, filename_qa, bandname, irradname)

   ! Open the netcdf file
   call ncdf_open(fid, filename, 'read_slstr_tirdata()')
   call ncdf_read_array(fid, bandname, outarr, start=[sx, sy])
   call ncdf_close(fid, 'read_slstr_tirdata()')

end subroutine read_slstr_tirdata

! Read the nadir-view visible grid data from SLSTR. Need to update with oblique view
! This assumes that VIS data is twice resolution of TIR (should be correct, 0.5km)
subroutine read_slstr_visdata(indir, inband, outarr, imager_angles, &
     sx, sy, nx, ny)

   use imager_structures_m
   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*),      intent(in)  :: indir
   integer,               intent(in)  :: inband
   integer,               intent(in)  :: sx
   integer,               intent(in)  :: sy
   integer,               intent(in)  :: nx
   integer,               intent(in)  :: ny
   real(kind=sreal),      intent(out) :: outarr(:,:)
   type(imager_angles_t), intent(in)  :: imager_angles

   real, allocatable              :: data1(:,:)
   real(kind=sreal), allocatable  :: irradiances(:)

   character(len=path_length)     :: filename
   character(len=path_length)     :: filename_qa
   character(len=path_length)     :: bandname
   character(len=path_length)     :: irradname

   integer :: fid, ndet

   if (inband .lt. 1 .or. inband .gt. 18) then
      print*, 'SLSTR input band must be in range 1-18. Here we have', inband
      stop error_stop_code
   end if

   ! Find the filename required for this channel
   call get_slstr_imnames(indir, inband, filename, filename_qa, bandname, irradname)

   allocate(data1(nx*2,ny*2))

   data1(:,:) = sreal_fill_value

   ! Open the netcdf file
   call ncdf_open(fid, filename, 'read_slstr_visdata()')
   call ncdf_read_array(fid, bandname, data1, start=[sx*2-1, sy*2-1])
   call ncdf_close(fid, 'read_slstr_visdata()')

   ! Now we deal with the solar irradiance dataset
   ! Open the netcdf file
   call ncdf_open(fid, filename_qa, 'read_slstr_visdata()')
   ! Get number of detectors, should be 4
   ndet = ncdf_dim_length(fid, 'detectors', 'read_slstr_visdata()')
   allocate(irradiances(ndet))

   call ncdf_read_array(fid, irradname, irradiances)

   call ncdf_close(fid, 'read_slstr_visdata()')

   ! Resample the data to the TIR grid size.
   call slstr_resample_vis_to_tir(data1, outarr, nx, ny, sreal_fill_value)

   ! Convert from radiances to reflectances
   where(outarr .ne. sreal_fill_value) &
        outarr = pi / irradiances(1) * outarr

   deallocate(data1)

end subroutine read_slstr_visdata


! Resamples VIS data onto the TIR grid. Assumes that VIS resolution is twice
! that of the TIR images (0.5km vs 1km). This is not efficient, but works!
!
! To resample we simply average a 2x2 region of VIS into a 1x1 pixel of TIR.
subroutine slstr_resample_vis_to_tir(inarr, outarr, nx, ny, fill)

   use preproc_constants_m

   implicit none

   integer,          intent(in)  :: nx
   integer,          intent(in)  :: ny
   real,             intent(in)  :: fill
   real(kind=sreal), intent(in)  :: inarr(:,:) ! (nx*2,ny*2)
   real(kind=sreal), intent(out) :: outarr(:,:) ! (nx,ny)

   real    :: tmpval

   integer :: x, y
   integer :: newx, newy, counter

   newx = 1
   do x = 1, nx*2-1, 2
      newy = 1

      do y = 1, ny*2-1, 2
         counter = 0
         tmpval = 0
         if (inarr(x,y).ne.fill) then
            tmpval = tmpval+inarr(x,y)
            counter = counter+1
         end if
         if (inarr(x+1,y).ne.fill) then
            tmpval = tmpval+inarr(x+1,y)
            counter = counter+1
         end if
         if (inarr(x,y+1).ne.fill) then
            tmpval = tmpval+inarr(x,y+1)
            counter = counter+1
         end if
         if (inarr(x+1,y+1).ne.fill) then
            tmpval = tmpval+inarr(x+1,y+1)
            counter = counter+1
         end if
         if (counter .gt. 0) then
            outarr(newx,newy) = tmpval/counter
         else
            outarr(newx,newy) = fill
         end if
         newy = newy+1
      end do
      newx = newx+1
   end do

end subroutine slstr_resample_vis_to_tir

subroutine slstr_get_alignment(indir, startx, endx, sx_nad, sx_obl, &
     ex_nad, ex_obl)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: indir
   integer,          intent(in)  :: startx
   integer,          intent(in)  :: endx
   integer,          intent(out) :: sx_nad
   integer,          intent(out) :: sx_obl
   integer,          intent(out) :: ex_nad
   integer,          intent(out) :: ex_obl

   integer :: nx, ny, obnx, obny, alignment

#ifndef CONSTANT_OBLIQUE_OFFSET
   real(kind=sreal), allocatable :: ndlons(:,:)
   real(kind=sreal), allocatable :: oblons(:,:)

   integer :: x
   real    :: bdiff, total
#endif

   call get_slstr_gridsize(indir, 'in', nx, ny)
   call get_slstr_gridsize(indir, 'io', obnx, obny)

#ifdef CONSTANT_OBLIQUE_OFFSET
   ! Assume constant offset between nadir and oblique grids.
   alignment = CONSTANT_OBLIQUE_OFFSET
#else
   ! Determine offset between nadir and oblique grids as the point which
   ! minimises the difference between their full longitude fields.
   allocate(ndlons(nx,ny))
   allocate(oblons(obnx,obny))

   call read_slstr_field(indir, 'geodetic', 'in', 'longitude', 1, 1, ndlons)
   call read_slstr_field(indir, 'geodetic', 'io', 'longitude', 1, 1, oblons)

   bdiff = 9e15

   do x = 1, nx-obnx
      total = sum(abs(ndlons(x:x+obnx-1,:) - oblons))
      if (total .lt. bdiff) then
         bdiff = total
         alignment = x
      end if
   end do
   ! There should be a better way of doing this, seeing as 545 < alignment < 551
   ! and the cost function appears to be smooth.

   if (bdiff .eq. 9e15) then
      write(*,*) 'ERROR: slstr_get_alignment(): unable to determine alignment.'
      stop error_stop_code
   end if
   deallocate(ndlons)
   deallocate(oblons)
#endif

   ! Determine start/end of orbit segment in the oblique view
   if (startx .lt. alignment) then
      sx_nad = alignment
      sx_obl = 1
   else if (startx .ge. alignment+obnx) then
      write(*,*) ' WARNING: read_slstr(): Oblique view requested for ', &
           'unavailable segment of orbit. Require startx <', alignment+obnx
      sx_nad = 0
      sx_obl = 0
   else
      sx_nad = startx
      sx_obl = startx - alignment + 1
   end if

   if (endx .lt. alignment) then
      write(*,*) 'WARNING: read_slstr(): Oblique view requested for ', &
           'unavailable segment of orbit. Require endx >', alignment
      ex_nad = 0
      ex_obl = 0
   else if (endx .ge. alignment+obnx) then
      ex_nad = alignment + obnx - 1
      ex_obl = obnx
   else
      ex_nad = endx
      ex_obl = endx - alignment + 1
   end if

#ifdef DEBUG
   print*, 'Nadir start, end: ', sx_nad, ex_nad
   print*, 'Oblique start, end: ', sx_obl, ex_obl
#endif

end subroutine slstr_get_alignment


! Gets the size of an SLSTR grid.
! This is needed for retrieving tx lats/lons, resampling, and to ensure
! correct offset between views.
subroutine get_slstr_gridsize(indir, grid, nx, ny)

   use orac_ncdf_m

   implicit none

   character(len=*), intent(in)  :: indir
   character(len=*), intent(in)  :: grid
   integer,          intent(out) :: nx
   integer,          intent(out) :: ny

   character(len=path_length) :: geofile
   integer                    :: fid

   geofile = trim(adjustl(indir))//'geodetic_'//trim(adjustl(grid))//'.nc'

   call ncdf_open(fid, geofile, 'get_slstr_gridsize()')
   ny = ncdf_dim_length(fid, 'rows', 'get_slstr_gridsize()')
   nx = ncdf_dim_length(fid, 'columns', 'get_slstr_gridsize()')
   call ncdf_close(fid, 'get_slstr_gridsize()')

end subroutine get_slstr_gridsize

! Routine to get slope/prev/next pixel for resampling tx->in grids.
subroutine slstr_get_interp(in_lons, tx_lons, nxt, nyt, nxi, nyi, starty, interp)

   use imager_structures_m
   use preproc_constants_m

   implicit none

   integer,          intent(in)  :: nxt
   integer,          intent(in)  :: nyt
   integer,          intent(in)  :: nxi
   integer,          intent(in)  :: nyi
   integer,          intent(in)  :: starty
   real(kind=sreal), intent(in)  :: in_lons(:,:) ! (nxi,nyi)
   real(kind=sreal), intent(in)  :: tx_lons(:,:) ! (nxt,nyt)
   real(kind=sreal), intent(out) :: interp(:,:,:) ! (nxi,nyi,3)

   integer          :: x, y, y_tx
   real(kind=sreal) :: dists1(nxt), dists2(nxt)
   real(kind=sreal) :: firlo, seclo

!$OMP PARALLEL PRIVATE(x, y, dists1, dists2, firlo, seclo)
!$OMP DO SCHEDULE(GUIDED)
   do y = 1, nyi
      y_tx = starty - 1 + y
      do x = 1, nxi
         dists1 = in_lons(x,y) - tx_lons(:,y_tx)
         dists2 = dists1

         where(dists1 .lt. 0) dists1 = 999
         where(dists2 .gt. 0) dists2 = -999
         interp(x,y,1) = minloc(dists1, dim=1)
         interp(x,y,2) = maxloc(dists2, dim=1)

         firlo = tx_lons(int(interp(x,y,1)),y_tx)
         seclo = tx_lons(int(interp(x,y,2)),y_tx)

         interp(x,y,3) = (in_lons(x,y) - firlo) / (seclo - firlo)
      end do
   end do
!$OMP END DO
!$OMP END PARALLEL

end subroutine slstr_get_interp


! Routine to resample the azmiuth/zenith data from the tx grid to the tir grid.
subroutine slstr_interp_angs(in_angs, out_angs, txnx, txny, nx, ny, &
     startx, starty, interp, view)

   use imager_structures_m
   use preproc_constants_m

   implicit none

   integer,               intent(in)    :: txnx
   integer,               intent(in)    :: txny
   integer,               intent(in)    :: nx
   integer,               intent(in)    :: ny
   integer,               intent(in)    :: startx
   integer,               intent(in)    :: starty
   integer,               intent(in)    :: view
   real(kind=sreal),      intent(in)    :: in_angs(:,:,:) ! (txnx,txny,4)
   real(kind=sreal),      intent(in)    :: interp(:,:,:) ! (nx,ny,3)
   type(imager_angles_t), intent(inout) :: out_angs

   integer          :: x, y, z, prev, next, y_tx
   real(kind=sreal) :: slo, intval(4)

   ! Loop over all pixels
!$OMP PARALLEL PRIVATE(x, y, intval, prev, next, slo)
!$OMP DO SCHEDULE(GUIDED)
   do x = 1, nx
      do y = 1, ny
         prev= interp(x,y,1)
         next= interp(x,y,2)
         slo = interp(x,y,3)
         y_tx = starty - 1 + y

         ! Compute the interpolated angles on the TIR grid
         do z = 1, 4
            if (prev .ne. next) then
               if (in_angs(prev,y_tx,z) .eq. sreal_fill_value) then
                  if (in_angs(next,y_tx,z) .eq. sreal_fill_value) then
                     intval(z) = sreal_fill_value
                  else
                     intval(z) = in_angs(next,y_tx,z)
                  end if
               else
                  if (in_angs(next,y,z) .eq. sreal_fill_value) then
                     intval(z) = in_angs(prev,y_tx,z)
                  else
                     if (z .eq. 1 .or. z .eq. 3) then
                        if (in_angs(prev,y_tx,z) .gt. 315. .and. &
                            in_angs(next,y_tx,z) .lt. 45.) then
                           intval(z) = in_angs(prev,y_tx,z) + &
                                slo*(in_angs(next,y_tx,z)-(in_angs(prev,y_tx,z)-360.))
                        else if (in_angs(next,y_tx,z) .gt. 315. .and. &
                                 in_angs(prev,y_tx,z) .lt. 45.) then
                           intval(z) = in_angs(prev,y_tx,z) + slo*( &
                                (in_angs(next,y_tx,z)-360.)-(in_angs(prev,y_tx,z)))
                        else
                           intval(z) = in_angs(prev,y_tx,z) + slo*( &
                                in_angs(next,y_tx,z)-in_angs(prev,y_tx,z))
                        end if
                     else
                        intval(z) = in_angs(prev,y_tx,z) + slo*( &
                             in_angs(next,y_tx,z)-in_angs(prev,y_tx,z))
                     end if
                     if (z .eq. 1 .or. z .eq. 3) then
                        if (intval(z) .lt. 0.) intval(z) = intval(z)+360.
                     end if
                  end if
               end if
            else
               if (in_angs(prev,y_tx,z) .ne. sreal_fill_value) then
                  intval(z) = in_angs(prev,y_tx,z)
               else
                  intval(z) = sreal_fill_value
               end if
            end if
         end do

         if (intval(4) .lt. 0 .or. intval(4) .gt. 180 ) intval = sreal_fill_value
         if (intval(2) .lt. 0 .or. intval(2) .gt. 180 ) intval = sreal_fill_value

         out_angs%solazi(startx-1+x,y,view) = intval(3)
         out_angs%satazi(startx-1+x,y,view) = intval(1)
         out_angs%solzen(startx-1+x,y,view) = intval(4)
         out_angs%satzen(startx-1+x,y,view) = intval(2)

      end do
   end do
!$OMP END DO
!$OMP END PARALLEL

end subroutine slstr_interp_angs


! Read SZA, SAA, VZA, VAA from the appropriate file. For reasons that make no
! sense these are on a completely different grid to either the TIR or VIS data.
! We therefore also have to convert from this 'tie-point' grid onto something
! that's actually sensible and useful.
subroutine read_slstr_satsol(indir, imager_angles, interp, txnx, txny, nx, ny, &
     startx, starty, view)

   use orac_ncdf_m
   use imager_structures_m
   use preproc_constants_m
   use system_utils_m

   implicit none

   integer,               intent(in)    :: view
   integer,               intent(in)    :: nx
   integer,               intent(in)    :: ny
   integer,               intent(in)    :: txnx
   integer,               intent(in)    :: txny
   integer,               intent(in)    :: startx
   integer,               intent(in)    :: starty
   character(len=*),      intent(in)    :: indir
   real(kind=sreal),      intent(in)    :: interp(:,:,:) ! (nx,ny,3)
   type(imager_angles_t), intent(inout) :: imager_angles

   character(len=path_length) :: geofile, vid
   integer                    :: fid

   ! This stores the angles on the tx (reduced) grid.
   ! In order (1->4): vaa, vza, saa, sza
   real(kind=sreal) :: angles(txnx,txny,4)

   if (view .eq. 2) then
      vid = 'to'
   else
      vid = 'tn'
   end if

   ! Input file, on the geometry grid (tn)
   geofile = trim(indir)//'geometry_'//trim(vid)//'.nc'

   ! First we do the DEM
   call ncdf_open(fid, geofile, 'read_slstr_geodata()')

   ! Retrieve each variable on the tx grid
   call ncdf_read_array(fid, 'sat_azimuth_'//trim(vid), angles(:,:,1))
   call ncdf_read_array(fid, 'sat_zenith_'//trim(vid), angles(:,:,2))
   call ncdf_read_array(fid, 'solar_azimuth_'//trim(vid), angles(:,:,3))
   call ncdf_read_array(fid, 'solar_zenith_'//trim(vid), angles(:,:,4))

   call ncdf_close(fid, 'read_slstr_geodata()')

   ! Check bounds
   where(angles(:,:,4) .lt. -180) &
      angles(:,:,4) = sreal_fill_value
   where(angles(:,:,2) .lt. -180) &
      angles(:,:,2) = sreal_fill_value
   where(angles(:,:,4) .gt. 180) &
      angles(:,:,4) = sreal_fill_value
   where(angles(:,:,2) .gt. 180) &
      angles(:,:,2) = sreal_fill_value
   where(is_nan(angles)) &
        angles = sreal_fill_value

   ! Do the interpolation to full grid
   call slstr_interp_angs(angles, imager_angles, txnx, txny, nx, ny, &
        startx, starty, interp, view)

   ! Rescale zens + azis into correct format
   where(imager_angles%solazi(:,:,view) .ne. sreal_fill_value .and. &
         imager_angles%satazi(:,:,view) .ne. sreal_fill_value)
      !This line converts sol+sat azi to relazi
      imager_angles%relazi(:,:,view) = abs(imager_angles%solazi(:,:,view)-&
                                           imager_angles%satazi(:,:,view))
   end where

   where (imager_angles%relazi(:,:,view) .gt. 180.)
      imager_angles%relazi(:,:,view) = 360. - imager_angles%relazi(:,:,view)
   end where

end subroutine read_slstr_satsol


! Reads an arbitrary variable from a specified SLSTR file
subroutine read_slstr_int_field(indir, file, procgrid, variable, &
     startx, starty, data_arr)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*),   intent(in)  :: indir
   character(len=*),   intent(in)  :: file
   character(len=*),   intent(in)  :: procgrid
   character(len=*),   intent(in)  :: variable
   integer,            intent(in)  :: startx
   integer,            intent(in)  :: starty
   integer(kind=sint), intent(out) :: data_arr(:,:)

   character(len=path_length) :: geofile, var
   integer                    :: fid

   geofile = trim(indir) // trim(file) // '_' // trim(procgrid) // '.nc'
   var = trim(variable) // '_' // trim(procgrid)

   call ncdf_open(fid, geofile, 'read_slstr_field()')
   call ncdf_read_array(fid, var, data_arr, start=[startx, starty])
   call ncdf_close(fid, 'read_slstr_field()')

end subroutine read_slstr_int_field


! Reads an arbitrary variable from a specified SLSTR file
subroutine read_slstr_field(indir, file, procgrid, variable, &
     startx, starty, data_arr)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*), intent(in)  :: indir
   character(len=*), intent(in)  :: file
   character(len=*), intent(in)  :: procgrid
   character(len=*), intent(in)  :: variable
   integer,          intent(in)  :: startx
   integer,          intent(in)  :: starty
   real(kind=sreal), intent(out) :: data_arr(:,:)

   character(len=path_length) :: geofile, var
   integer                    :: fid

   geofile = trim(indir) // trim(file) // '_' // trim(procgrid) // '.nc'
   var = trim(variable) // '_' // trim(procgrid)

   call ncdf_open(fid, geofile, 'read_slstr_field()')
   call ncdf_read_array(fid, var, data_arr, start=[startx, starty])
   call ncdf_close(fid, 'read_slstr_field()')

end subroutine read_slstr_field
