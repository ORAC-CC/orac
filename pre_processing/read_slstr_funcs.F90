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
!
! $Id$
!
! Bugs:
! Fudge factor for some channels, needed to correct for SLSTR's awful coloaction
!-------------------------------------------------------------------------------

! This function retrieves the start and end times for an SLSTR scene, then
! computes the per-pixel sensing time before saving the results in the
! imager_time%time array.
subroutine get_slstr_startend(imager_time,fname,ny)

   use netcdf
   use imager_structures_m
   use calender_m

   type(imager_time_t),        intent(inout) :: imager_time
   character(len=path_length), intent(in)    :: fname
   integer,                    intent(in)    :: ny


   integer(kind=sint)         :: year1,month1,day1
   integer(kind=sint)         :: year2,month2,day2
   integer(kind=sint)         :: hour1,minute1,second1
   integer(kind=sint)         :: hour2,minute2,second2
   reaL(kind=dreal)           :: dfrac1,dfrac2,jd1,jd2,slo

   ! Variables for computing start and end times
   character(len=date_length) :: cyear1,cmonth1,cday1
   character(len=date_length) :: cyear2,cmonth2,cday2
   character(len=date_length) :: chour1,cminute1,csec1
   character(len=date_length) :: chour2,cminute2,csec2

   integer                    :: fid,ierr,index2,j
   character(len=path_length) :: l1b_start, l1b_end

   ! Convert start and end times to julian

   ierr=nf90_open(path=trim(adjustl(fname)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_startend(): Error opening file ',trim(fname)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, nf90_global, 'start_time', l1b_start)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_startend(): Error getting start_time from file ',trim(fname)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, nf90_global, 'stop_time', l1b_end)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_startend(): Error getting end_time from file ',trim(fname)
      stop error_stop_code
   end if
   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr(): Error closing file ',trim(fname)
      stop error_stop_code
   end if

   index2=1

   ! Starting time
   cyear1=trim(adjustl(l1b_start(index2:index2+4)))
   cmonth1=trim(adjustl(l1b_start(index2+5:index2+6)))
   cday1=trim(adjustl(l1b_start(index2+8:index2+9)))
   chour1=trim(adjustl(l1b_start(index2+11:index2+12)))
   cminute1=trim(adjustl(l1b_start(index2+14:index2+15)))
   csec1=trim(adjustl(l1b_start(index2+17:index2+18)))

   ! Ending time
   cyear2=trim(adjustl(l1b_end(index2:index2+4)))
   cmonth2=trim(adjustl(l1b_end(index2+5:index2+6)))
   cday2=trim(adjustl(l1b_end(index2+8:index2+9)))
   chour2=trim(adjustl(l1b_end(index2+11:index2+12)))
   cminute2=trim(adjustl(l1b_end(index2+14:index2+15)))
   csec2=trim(adjustl(l1b_end(index2+17:index2+18)))

   ! Get year, doy, hour and minute as integers
   read(cyear1(1:len_trim(cyear1)), '(I4)') year1
   read(cmonth1(1:len_trim(cmonth1)), '(I2)') month1
   read(cday1(1:len_trim(cday1)), '(I2)') day1
   read(chour1(1:len_trim(chour1)), '(I2)') hour1
   read(cminute1(1:len_trim(cminute1)), '(I2)') minute1
   read(csec1(1:len_trim(csec1)), '(I2)') second1

   read(cyear1(1:len_trim(cyear2)), '(I4)') year2
   read(cmonth1(1:len_trim(cmonth2)), '(I2)') month2
   read(cday1(1:len_trim(cday2)), '(I2)') day2
   read(chour2(1:len_trim(chour2)), '(I2)') hour2
   read(cminute2(1:len_trim(cminute2)), '(I2)') minute2
   read(csec2(1:len_trim(csec2)), '(I2)') second2

   call GREG2JD(year1,month1,day1,jd1)
   call GREG2JD(year2,month2,day2,jd2)

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
   do j=1,ny
      imager_time%time(:,j) = jd1+(slo*float(j))
   end do

end subroutine get_slstr_startend


! Computes the name of the input file based upon the band name.
! This is currently hard-coded for nadir files, need to add oblique.
subroutine get_slstr_imnames(indir,inband,fname,fname_qa,bname,irradname)

   use preproc_constants_m

   character(len=path_length),  intent(in)  :: indir
   integer,                     intent(in)  :: inband
   character(len=path_length),  intent(out) :: fname
   character(len=path_length),  intent(out) :: fname_qa
   character(len=path_length),  intent(out) :: bname
   character(len=path_length),  intent(out) :: irradname

   character(len=3) :: band
   character(len=3) :: vid

   integer          ::   mod_inb

   if (inband .le. 9) then
      vid='n'
      mod_inb=inband
   else
      vid='o'
      mod_inb=inband-9
   end if

   write (band, '(I1.1)') mod_inb
   if (mod_inb .le. 6) then
      fname       = trim(indir)//'S'//trim(band)//'_radiance_a'//trim(vid)//'.nc'
      fname_qa      = trim(indir)//'S'//trim(band)//'_quality_a'//trim(vid)//'.nc'
      bname       = 'S'//trim(band)//'_radiance_a'//trim(vid)
      irradname    = 'S'//trim(band)//'_solar_irradiance_a'//trim(vid)
   else if (mod_inb .le. 9) then
      fname       = trim(indir)//'S'//trim(band)//'_BT_i'//trim(vid)//'.nc'
      fname_qa      = trim(indir)//'S'//trim(band)//'_quality_i'//trim(vid)//'.nc'
      bname       = 'S'//trim(band)//'_BT_i'//trim(vid)
      irradname   = 'NONE'
   else
      print*,'Incorrect band:',inband,'. This is not supported for SLSTR (1-18 only).'
      stop error_stop_code
   end if

end subroutine get_slstr_imnames

! Read the nadir-view thermal grid data from SLSTR. Need to update with oblique view
subroutine read_slstr_tirdata(indir,inband,outarr,sx,sy,nx,ny,inx,iny,offset,view)

   use netcdf
   use preproc_constants_m
   use orac_ncdf_m

   character(len=path_length), intent(in)  :: indir
   integer,                    intent(in)  :: inband
   integer,                    intent(in)  :: sx
   integer,                    intent(in)  :: sy
   integer,                    intent(in)  :: nx
   integer,                    intent(in)  :: ny
   integer,                    intent(in)  :: inx
   integer,                    intent(in)  :: iny
   integer,                    intent(in)  :: offset
   integer,                    intent(in)  :: view
   real(kind=sreal),           intent(out) :: outarr(inx,iny)

   real                       :: data1(nx,ny)
   real                       :: data2(nx,ny)

   character(len=path_length) :: filename
   character(len=path_length) :: filename_qa
   character(len=path_length) :: bandname
   character(len=path_length) :: irradname
   character(len=path_length) :: fillname
   character(len=path_length) :: sclname
   character(len=path_length) :: offname

   real    :: filval,sclval,offval
   integer :: fid,did,ierr
   integer :: endx,endy

   ! Find the filename required for this channel
   call get_slstr_imnames(indir,inband,filename,filename_qa,bandname,irradname)

   ! Open the netcdf file
   ierr=nf90_open(path=trim(adjustl(filename)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error opening file ',trim(filename)
      stop error_stop_code
   end if

   ! Check dimensions so we load the right amount of NetCDF file
   endy=nc_dim_length(fid,'rows',.false.)
   endx=nc_dim_length(fid,'columns',.false.)

   ! Check that the dataset exists
   ierr=nf90_inq_varid(fid, trim(bandname), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error opening dataset ',trim(bandname),' in ',trim(filename)
      stop error_stop_code
   end if

   ! Get anciliary values (fill, scale, offset)
   ierr=nf90_get_att(fid, did, '_FillValue', filval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error getting ',trim(fillname),' from file ',trim(filename)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, did, 'scale_factor', sclval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error getting ',trim(sclname),' from file ',trim(filename)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, did,'add_offset', offval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error getting ',trim(offname),' from file ',trim(filename)
      stop error_stop_code
   end if

   ! Get the actual data
   ierr=nf90_get_var(fid, did, data1)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error reading dataset ',trim(bandname),' in ',trim(filename)
      stop error_stop_code
   end if
   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error closing file ',trim(filename)
      stop error_stop_code
   end if

   ! Apply scale, offset and then fill for bad data
   data2 = data1*sclval + offval
   where(data1 .eq. filval) data2=sreal_fill_value
   outarr(offset:offset+nx-1,:)=data2


   if (inband .eq. 8 .or. inband .eq. 9) then
      outarr(:,1:ny-2)=outarr(:,3:ny)
   endif
   if (inband .eq. 16 .or. inband .eq. 17 .or. inband .eq. 18) then
      outarr(1:nx-1,:)=outarr(2:nx,:)
   endif

end subroutine read_slstr_tirdata

! Read the nadir-view visible grid data from SLSTR. Need to update with oblique view
! This assumes that VIS data is twice resolution of TIR (should be correct, 0.5km)
subroutine read_slstr_visdata(indir,inband,outarr,imager_angles,sx,sy,nx,ny,inx,iny,offset,view)

   use netcdf
   use preproc_constants_m
   use imager_structures_m
   use orac_ncdf_m

   character(len=path_length),  intent(in)  :: indir
   integer,                     intent(in)  :: inband
   integer,                     intent(in)  :: sx
   integer,                     intent(in)  :: sy
   integer,                     intent(in)  :: nx
   integer,                     intent(in)  :: ny
   integer,                     intent(in)  :: inx
   integer,                     intent(in)  :: iny
   integer,                     intent(in)  :: offset
   integer,                     intent(in)  :: view
   real(kind=sreal),            intent(out) :: outarr(inx,iny)
   type(imager_angles_t),       intent(in)  :: imager_angles

   real, allocatable              :: data1(:,:)
   real, allocatable              :: data2(:,:)
   real, allocatable              :: data3(:,:)
   real(kind=sreal), allocatable  :: irradiances(:)

   character(len=path_length)     :: filename
   character(len=path_length)     :: filename_qa
   character(len=path_length)     :: bandname
   character(len=path_length)     :: irradname
   character(len=path_length)     :: fillname
   character(len=path_length)     :: sclname
   character(len=path_length)     :: offname

   real    :: filval,sclval,offval

   integer :: fid,did,ierr,j

   integer :: endx,endy,ndet

   if (inband .lt. 1 .or. inband .gt. 18) then
      print*,'SLSTR input band must be in range 1-18. Here we have',inband
      stop error_stop_code
   end if

   ! Find the filename required for this channel
   call get_slstr_imnames(indir,inband,filename,filename_qa,bandname,irradname)

   allocate(data1(nx*2,ny*2))
   allocate(data2(nx,ny))
   allocate(data3(nx,ny))

   data1(:,:)=sreal_fill_value

   ! Open the netcdf file
   ierr=nf90_open(path=trim(adjustl(filename)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error opening file ',trim(filename)
      stop error_stop_code
   end if

   ! Check dimensions so we load the right amount of NetCDF file
   endy=nc_dim_length(fid,'rows',.false.)
   endx=nc_dim_length(fid,'columns',.false.)

   ! Check that the dataset exists
   ierr=nf90_inq_varid(fid, trim(bandname), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error opening dataset ',trim(bandname),' in ',trim(filename)
      stop error_stop_code
   end if

   ! Get anciliary values (fill, scale, offset)
   ierr=nf90_get_att(fid, did, '_FillValue', filval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error getting ',trim(fillname),' from file ',trim(filename)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, did, 'scale_factor', sclval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error getting ',trim(sclname),' from file ',trim(filename)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, did,'add_offset', offval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error getting ',trim(offname),' from file ',trim(filename)
      stop error_stop_code
   end if

   ! Get the actual data
   ierr=nf90_get_var(fid, did, data1,count=(/ endx,endy /))
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error reading dataset ',trim(bandname),' in ',trim(filename)
      print*,trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   ! Close this file
   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error closing file ',trim(filename)
      stop error_stop_code
   end if

   ! Now we deal with the solar irradiance dataset
   ! Open the netcdf file
   ierr=nf90_open(path=trim(adjustl(filename_qa)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error opening file ',trim(filename_qa)
      stop error_stop_code
   end if
   ! Get number of detectors, should be 4
   ndet=nc_dim_length(fid,'detectors',.false.)
   allocate(irradiances(ndet))

   ierr=nf90_inq_varid(fid, trim(irradname), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error opening dataset ',trim(irradname),' in ',trim(filename_qa)
      stop error_stop_code
   end if
   ierr=nf90_get_var(fid, did, irradiances,count=(/ ndet /))
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error reading dataset ',trim(irradname),' in ',trim(filename_qa)
      print*,trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_visdata(): Error closing file ',trim(filename_qa)
      stop error_stop_code
   end if

   ! Resample the data to the TIR grid size.
   call slstr_resample_vis_to_tir(data1,data2,nx,ny,filval)
   ! Apply scale, offset and then fill for bad data
   data3 = data2*sclval + offval

   ! Apply scale, offset and then fill for bad data
   where(data2 .eq. filval) data3=sreal_fill_value
   outarr(offset:nx+offset-1,:)=data3

   ! Convert from radiances to reflectances
   where(outarr .ne.sreal_fill_value) outarr = (pi*outarr*cos(d2r*imager_angles%solzen(:,:,view)))/irradiances(1)

   ! Fill where sza is bad
   where(imager_angles%solzen(:,:,1) .eq. sreal_fill_value) outarr=sreal_fill_value
   deallocate(data1)
   deallocate(data2)
   deallocate(data3)
   if (inband .eq. 5 .or. inband .eq. 6) then
      outarr(1:nx-1,1:ny-1)=outarr(2:nx,2:ny)
   endif
   if (inband .eq. 10 .or. inband .eq. 11 .or. inband .eq. 12 .or. inband .eq. 13) then
      outarr(1:nx-1,1:ny-1)=outarr(2:nx,2:ny)
   endif
   if (inband .eq. 14 .or. inband .eq. 15) then
      outarr(1:nx-1,:)=outarr(2:nx,:)
   endif



end subroutine read_slstr_visdata


! Resamples VIS data onto the TIR grid. Assumes that VIS resolution is twice
! that of the TIR images (0.5km vs 1km). This is not efficient, but works!
!
! To resample we simply average a 2x2 region of VIS into a 1x1 pixel of TIR.
subroutine slstr_resample_vis_to_tir(inarr,outarr,nx,ny,fill)

   use preproc_constants_m

   integer,          intent(in)  :: nx
   integer,          intent(in)  :: ny
   real,             intent(in)  :: fill
   real(kind=sreal), intent(in)  :: inarr(nx*2,ny*2)
   real(kind=sreal), intent(out) :: outarr(nx,ny)

   real    :: tmpval

   integer :: x,y
   integer :: newx,newy,counter
   newx=1
   do x=1,nx*2-1,2
      newy=1

      do y=1,ny*2-1,2
         counter=0
         tmpval=0
         if (inarr(x,y).ne.fill) then
            tmpval=tmpval+inarr(x,y)
            counter=counter+1
         end if
         if (inarr(x+1,y).ne.fill) then
            tmpval=tmpval+inarr(x+1,y)
            counter=counter+1
         end if
         if (inarr(x,y+1).ne.fill) then
            tmpval=tmpval+inarr(x,y+1)
            counter=counter+1
         end if
         if (inarr(x+1,y+1).ne.fill) then
            tmpval=tmpval+inarr(x+1,y+1)
            counter=counter+1
         end if
         if (counter .gt. 0) then
            outarr(newx,newy) = tmpval/counter
         else
            outarr(newx,newy)=fill
         end if
         newy=newy+1
      end do
      newx=newx+1
   end do

end subroutine slstr_resample_vis_to_tir

! Get the DEM from the geolocation file
subroutine read_slstr_demdata(indir,data_arr,nx,ny)

   use preproc_constants_m
   use imager_structures_m
   use netcdf

   integer,                    intent(in)  :: nx
   integer,                    intent(in)  :: ny
   character(len=path_length), intent(in)  :: indir
   integer(kind=sint),         intent(out) :: data_arr(nx,ny)

   integer(kind=sint), allocatable ::  data(:,:)
   character(len=path_length)      :: geofile
   character(len=path_length)      :: var
   integer                         :: fid,did,ierr
   real                            :: filval,sclval,offval

   allocate(data(nx,ny))

   ! Hardcoded filenames, these shouldn't change for nominal SLSTR data
   var = 'elevation_in'
   geofile = trim(indir)//'geodetic_in.nc'

   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening file ',trim(geofile)
      stop error_stop_code
   end if
   ierr=nf90_inq_varid(fid, trim(var), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening dataset ',trim(var),' in ',trim(geofile)
      stop error_stop_code
   end if

   ierr=nf90_get_att(fid, did, '_FillValue', filval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting FillValue from file ',trim(geofile)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, did, 'scale_factor', sclval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting scale_factor from file ',trim(geofile)
      stop error_stop_code
   end if
   ierr=nf90_get_att(fid, did,'add_offset', offval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting add_offset from file ',trim(geofile)
      stop error_stop_code
   end if
   ierr=nf90_get_var(fid, did, data)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error reading dataset ',trim(var),' in ',trim(geofile)
      stop error_stop_code
   end if
   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error closing file ',trim(geofile)
      stop error_stop_code
   end if

   ! Scale and offset results, then apply the fill value
   data_arr = data*sclval + offval
   where(data .eq. filval) data_arr=sreal_fill_value

   deallocate(data)
end subroutine read_slstr_demdata



! Read the lat/lon data on the TIR grid
subroutine read_slstr_lldata(indir,data_arr,nx,ny,proclat,procgrid)

   use preproc_constants_m
   use imager_structures_m
   use netcdf
   use orac_ncdf_m

   integer,                    intent(in)  :: nx
   integer,                    intent(in)  :: ny
   character(len=path_length), intent(in)  :: indir
   real(kind=sreal),           intent(out) :: data_arr(nx,ny)
   logical,                    intent(in)  :: proclat
   character(len=2),           intent(in)  :: procgrid

   real(kind=sreal), allocatable :: data(:,:)
   character(len=path_length)    :: geofile
   character(len=path_length)    :: var
   integer                       :: fid,did,ierr
   real                          :: filval,sclval,offval
   character(len=2)              :: grid

   sclval = 1.
   offval = 0.

   if (proclat .eqv. .true.) then
      var = 'latitude_'//procgrid
   else
      var = 'longitude_'//procgrid
   end if
   geofile = trim(indir)//'geodetic_'//trim(procgrid)//'.nc'
   allocate(data(nx,ny))

   ! First we do the DEM
   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_lldata(): Error opening file ',trim(geofile)
      stop error_stop_code
   end if

   ierr=nf90_inq_varid(fid, trim(var), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_lldata(): Error opening dataset ',trim(var),' in ',trim(geofile)
      stop error_stop_code
   end if

   ! Only run this bit for non tx-grids. For unknown reasons tx grid does not
   ! contain any metadata for the following values.
   if (procgrid .ne. 'tx') then
      ierr=nf90_get_att(fid, did, '_FillValue', filval)
      if (ierr.ne.NF90_NOERR) then
         print*,'ERROR: get_slstr_lldata(): Error getting FillValue from file ',trim(geofile)
         stop error_stop_code
      end if
      ierr=nf90_get_att(fid, did, 'scale_factor', sclval)
      if (ierr.ne.NF90_NOERR) then
         print*,'ERROR: get_slstr_lldata(): Error getting scale_factor from file ',trim(geofile)
         stop error_stop_code
      end if
      ierr=nf90_get_att(fid, did,'add_offset', offval)
      if (ierr.ne.NF90_NOERR) then
         print*,'ERROR: get_slstr_lldata(): Error getting add_offset from file ',trim(geofile)
         stop error_stop_code
      end if
   end if

   ierr=nf90_get_var(fid, did, data)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_lldata(): Error reading dataset ',trim(var),' in ',trim(geofile)
      stop error_stop_code
   end if

   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_lldata(): Error closing file ',trim(geofile)
      stop error_stop_code
   end if

   data_arr = data*sclval + offval
   where(data .eq. filval) data_arr=sreal_fill_value

   deallocate(data)
end subroutine read_slstr_lldata

subroutine slstr_get_alignment(nx,ny,obnx,obny,tirlons,oblons,aligment)

   use preproc_constants_m

   integer,          intent(in)  :: nx
   integer,          intent(in)  :: ny
   integer,          intent(in)  :: obnx
   integer,          intent(in)  :: obny
   integer,          intent(out) :: aligment
   real(kind=sreal), intent(in)  :: tirlons(nx,ny)
   real(kind=sreal), intent(in)  :: oblons(obnx,obny)

   integer :: x
   real    :: bdiff,summer

   bdiff=9e15

   do x=1,nx-obnx
      summer   =   sum(abs(tirlons(x:x+obnx,:)-oblons))
      if (summer .lt. bdiff) then
         bdiff = summer
         aligment = x
      end if
   end do

end subroutine slstr_get_alignment


! Gets the size of the oblique view TIR grid.
! This is needed to ensure correct offset between views.
subroutine get_slstr_obgridsize(indir,nx,ny)
   use orac_ncdf_m

   integer,                    intent(out) :: nx
   integer,                    intent(out) :: ny
   character(len=path_length), intent(in)  :: indir

   character(len=path_length) :: geofile
   integer                    :: fid,ierr

   geofile = trim(indir)//'geodetic_io.nc'

   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_txgridsize(): Error opening file ',trim(geofile)
      stop error_stop_code
   end if

   ny=nc_dim_length(fid,'rows',.false.)
   nx=nc_dim_length(fid,'columns',.false.)
   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_txgridsize(): Error closing file ',trim(geofile)
      stop error_stop_code
   end if
end subroutine get_slstr_obgridsize

! Gets the size of the reduced (tx) grid.
! This is needed for retrieving tx lats/lons and resampling.
subroutine get_slstr_txgridsize(indir,nx,ny)
   use orac_ncdf_m

   integer,                    intent(out) :: nx
   integer,                    intent(out) :: ny
   character(len=path_length), intent(in)  :: indir

   character(len=path_length) :: geofile
   integer                    :: fid,ierr

   geofile = trim(indir)//'geodetic_tx.nc'

   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_txgridsize(): Error opening file ',trim(geofile)
      stop error_stop_code
   end if

   ny=nc_dim_length(fid,'rows',.false.)
   nx=nc_dim_length(fid,'columns',.false.)
   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_txgridsize(): Error closing file ',trim(geofile)
      stop error_stop_code
   end if
end subroutine get_slstr_txgridsize

! Routine to get slope/prev/next pixel for resampling tx->in grids.
subroutine slstr_get_interp(in_lons,tx_lons,nxt,nyt,nxi,nyi,interp)

   use preproc_constants_m
   use imager_structures_m

   integer,                    intent(in)  :: nxt
   integer,                    intent(in)  :: nyt
   integer,                    intent(in)  :: nxi
   integer,                    intent(in)  :: nyi
   real(kind=sreal),           intent(in)  :: in_lons(nxi,nyi)
   real(kind=sreal),           intent(in)  :: tx_lons(nxt,nyt)
   real(kind=sreal),           intent(out) :: interp(nxi,nyi,3)

   integer          :: x,y
   real(kind=sreal) :: dists1(nxt),dists2(nxt)
   real(kind=sreal) :: firlo,seclo,slo

!$OMP PARALLEL PRIVATE(x, y, dists1, dists2, firlo, seclo)
!$OMP DO SCHEDULE(GUIDED)
   do y=1,nyi
      do x=1,nxi
         dists1 = in_lons(x,y)-tx_lons(:,y)
         dists2 = in_lons(x,y)-tx_lons(:,y)

         where(dists1 .lt. 0)dists1=999
         where(dists2 .gt. 0)dists2=-999
         interp(x,y,1) = minloc(dists1,dim=1)
         interp(x,y,2) = maxloc(dists2,dim=1)

         firlo = tx_lons(minloc(dists1,dim=1),y)
         seclo = tx_lons(maxloc(dists2,dim=1),y)

         interp(x,y,3) = (in_lons(x,y)-firlo)/(seclo-firlo)
      end do
   end do
!$OMP END DO
!$OMP END PARALLEL

end subroutine slstr_get_interp


! Routine to resample the azmiuth/zenith data from the tx grid to the tir grid.
subroutine slstr_interp_angs(in_angs,out_angs,txnx,txny,nx,ny,interp,view)

   use preproc_constants_m
   use imager_structures_m

   integer,               intent(in)    :: txnx
   integer,               intent(in)    :: txny
   integer,               intent(in)    :: nx
   integer,               intent(in)    :: ny
   integer,               intent(in)    :: view
   real(kind=sreal),      intent(in)    :: in_angs(txnx,txny,4)
   real(kind=sreal),      intent(in)    :: interp(nx,ny,3)
   type(imager_angles_t), intent(inout) :: out_angs

   integer          :: x,y,z,prev,next
   real(kind=sreal) :: slo,intval(4)

   ! Loop over all pixels
!$OMP PARALLEL PRIVATE(x, y, intval, prev, next, slo)
!$OMP DO SCHEDULE(GUIDED)
   do x=1,nx
      do y=1,ny
         prev= interp(x,y,1)
         next= interp(x,y,2)
         slo = interp(x,y,3)

         ! Compute the interpolated angles on the TIR grid
         do z=1,4
            if (prev .ne. next) then
               if (in_angs(prev,y,z) .eq. sreal_fill_value) then
                  if (in_angs(next,y,z) .eq. sreal_fill_value) then
                     intval(z) = sreal_fill_value
                  else
                     intval(z) = in_angs(next,y,z)
                  endif
               else
                  if (in_angs(next,y,z) .eq. sreal_fill_value) then
                     intval(z) = in_angs(prev,y,z)
                  else
                     if (z .eq. 1 .or. z .eq. 3) then
                        if (in_angs(prev,y,z) .gt. 315. .and. in_angs(next,y,z) .lt. 45.) then
                           intval(z) = in_angs(prev,y,z) + slo*(in_angs(next,y,z)-(in_angs(prev,y,z)-360.))
                        else if (in_angs(next,y,z) .gt. 315. .and. in_angs(prev,y,z) .lt. 45.) then
                           intval(z) = in_angs(prev,y,z) + slo*((in_angs(next,y,z)-360.)-(in_angs(prev,y,z)))
                        else
                           intval(z) = in_angs(prev,y,z) + slo*(in_angs(next,y,z)-in_angs(prev,y,z))
                        endif
                     else
                        intval(z) = in_angs(prev,y,z) + slo*(in_angs(next,y,z)-in_angs(prev,y,z))
                     endif
                     if (z .eq. 1 .or. z .eq. 3) then
                        if (intval(z) .lt. 0.) intval(z)=intval(z)+360.
                     endif
                  endif
               endif
            else
               if (in_angs(prev,y,z) .ne. sreal_fill_value) then
                    intval(z) = in_angs(prev,y,z)
                 else if (in_angs(next,y,z) .ne. sreal_fill_value) then
                    intval(z) = in_angs(next,y,z)
                 else
                  intval(z) = sreal_fill_value
               endif
            end if
         enddo

         if (intval(4) .lt. 0 .or. intval(4) .gt. 180 ) intval = sreal_fill_value
         if (intval(2) .lt. 0 .or. intval(2) .gt. 180 ) intval = sreal_fill_value

         out_angs%solazi(x,y,view) = intval(3)
         out_angs%relazi(x,y,view) = intval(1)
         out_angs%solzen(x,y,view) = intval(4)
         out_angs%satzen(x,y,view) = intval(2)

      end do
   end do
!$OMP END DO
!$OMP END PARALLEL

end subroutine slstr_interp_angs


! Read SZA, SAA, VZA, VAA from the appropriate file. For reasons that make no
! sense these are on a completely different grid to either the TIR or VIS data.
! We therefore also have to convert from this 'tie-point' grid onto something
! that's actually sensible and useful.
subroutine read_slstr_satsol(indir,imager_angles,interp,txnx,txny,nx,ny,startx,view)

   use preproc_constants_m
   use imager_structures_m
   use system_utils_m
   use netcdf

   integer,                    intent(in)    :: view
   integer,                    intent(in)    :: nx
   integer,                    intent(in)    :: ny
   integer,                    intent(in)    :: txnx
   integer,                    intent(in)    :: txny
   integer,                    intent(in)    :: startx
   character(len=path_length), intent(in)    :: indir
   real(kind=sreal),           intent(in)    :: interp(nx,ny,3)
   type(imager_angles_t),      intent(inout) :: imager_angles

   character(len=path_length) :: geofile,vid
   integer                    :: fid,ierr,bob

   ! This stores the angles on the tx (reduced) grid.
   ! In order (1->4): vaa,vza,saa,sza
   real(kind=sreal) :: angles(txnx,txny,4)

   if (view .eq. 2) then
      vid='to'
   else
      vid='tn'
   end if

   ! Input file, on the geometry grid (tn)
   geofile = trim(indir)//'geometry_'//trim(vid)//'.nc'

   ! First we do the DEM
   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening file ',trim(geofile)
      stop error_stop_code
   end if

   ! Retrieve each variable on the tx grid
   call slstr_get_one_geom(txnx,txny,fid,'sat_azimuth_'//trim(vid)//'',angles(:,:,1),geofile)
   call slstr_get_one_geom(txnx,txny,fid,'sat_zenith_'//trim(vid)//'',angles(:,:,2),geofile)
   call slstr_get_one_geom(txnx,txny,fid,'solar_azimuth_'//trim(vid)//'',angles(:,:,3),geofile)
   call slstr_get_one_geom(txnx,txny,fid,'solar_zenith_'//trim(vid)//'',angles(:,:,4),geofile)

   ierr=nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error closing file ',trim(geofile)
      stop error_stop_code
   end if

   ! Check bounds
   where(angles(:,:,4) .lt. -180) &
      angles(:,:,4)=sreal_fill_value
   where(angles(:,:,2) .lt. -180) &
      angles(:,:,2)=sreal_fill_value
   where(angles(:,:,4) .gt. 180) &
      angles(:,:,4)=sreal_fill_value
   where(angles(:,:,2) .gt. 180) &
      angles(:,:,2)=sreal_fill_value

   where(is_nan(angles)) angles=sreal_fill_value

   ! Do the interpolation to full grid
   call slstr_interp_angs(angles,imager_angles,txnx,txny,nx,ny,interp,view)

   ! Rescale zens + azis into correct format
   where(imager_angles%solazi(startx:,:,view) .ne. sreal_fill_value .and. &
         imager_angles%relazi(startx:,:,view) .ne. sreal_fill_value)
      imager_angles%relazi(:,:,view) = abs(imager_angles%relazi(startx:,:,view) - &
                                        imager_angles%solazi(startx:,:,view))
   end where
   where (imager_angles%solazi(:,:,view) .gt. 180.)
      imager_angles%solazi(:,:,view) = 360. - imager_angles%solazi(:,:,view)
   end where
   where (imager_angles%relazi(:,:,view) .gt. 180.)
      imager_angles%relazi(:,:,view) = 360. - imager_angles%relazi(:,:,view)
   end where
end subroutine read_slstr_satsol

! Get one of the geometry variables, then resample to the correct grid
subroutine slstr_get_one_geom(nx,ny,fid,var,odata,geofile)

   use preproc_constants_m
   use imager_structures_m
   use netcdf

   integer,                    intent(in)  :: nx
   integer,                    intent(in)  :: ny
   integer,                    intent(in)  :: fid
   character(len=*),           intent(in)  :: var
   real(kind=sreal),           intent(out) :: odata(nx,ny)
   character(len=path_length), intent(in)  :: geofile

   integer :: did,ierr

   ierr=nf90_inq_varid(fid, trim(var), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_get_one_geom(): Error opening dataset ',trim(var),' in ',trim(geofile)
      stop error_stop_code
   end if
   ierr=nf90_get_var(fid, did, odata)

   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_get_one_geom(): Error reading dataset ',trim(var),' in ',trim(geofile)
      stop error_stop_code
   end if


end subroutine slstr_get_one_geom
