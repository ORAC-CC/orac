!-------------------------------------------------------------------------------
! Name: read_slstr_funcions.F90
!
! Purpose:
! Contains functions for processing SLSTR images
! Here we have functions to:
!  1) Figure out the start and end times from the input image file
!    2) Choose filenames for reading
!   3) Downsample VIS data to TIR grid
!  4) Compute the zenith/azimuth angles on the TIR grid
!
! History:
! 2016/06/14, SP: First version.
! 2016/07/07, SP: Updated to make the code more sensible, some efficiency
!                 improvements too, should be more accurate for VIS resampling
!
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------


! This function retrieves the start and end times for an SLSTR scene, then
! computes the per-pixel sensing time before saving the results in the
! imager_time%time array.
subroutine get_slstr_startend(imager_time,fname,ny)

   use netcdf
   use imager_structures_m
   use calender_m

   type(imager_time_t),         intent(inout) :: imager_time
   character(len=path_length),  intent(in)    :: fname
   integer,                     intent(in)    :: ny


   integer(kind=sint)               :: year1,month1,day1,doy1
   integer(kind=sint)               :: year2,month2,day2,doy2
   integer(kind=sint)               :: hour1,minute1,second1
   integer(kind=sint)               :: hour2,minute2,second2
   double precision                 :: dfrac1,dfrac2,jd1,jd2,slo

   ! Variables for computing start and end times
   character(len=date_length)       :: cyear1,cmonth1,cday1
   character(len=date_length)       :: cyear2,cmonth2,cday2
   character(len=date_length)       :: chour1,cminute1,csec1
   character(len=date_length)       :: chour2,cminute2,csec2

   integer fid,did,ierr,curband,index2,j
   character(len=path_length) :: l1b_start, l1b_end

   ! Convert start and end times to julian

   ierr=nf90_open(path=trim(adjustl(fname)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_startend(): Error opening file ',trim(fname)
      stop
   endif
   ierr = nf90_get_att(fid, nf90_global, "start_time", l1b_start)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_startend(): Error getting start_time from file ',trim(fname)
      stop
   endif
   ierr = nf90_get_att(fid, nf90_global, "stop_time", l1b_end)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_startend(): Error getting end_time from file ',trim(fname)
      stop
   endif
   ierr = nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr(): Error closing file ',trim(fname)
      stop
   endif

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

   ! get year, doy, hour and minute as integers
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
   dfrac1   =   (float(hour1)/24.0) + (float(minute1)/(24.0*60.0)) + &
                        (float(second1)/(24.0*60.0*60.0))
   dfrac2   =   (float(hour2)/24.0) + (float(minute2)/(24.0*60.0)) + &
                        (float(second2)/(24.0*60.0*60.0))
   jd1   =   jd1 + dfrac1
   jd2   =   jd2 + dfrac2

   ! Compute linear regression slope
   slo   =   (jd2-jd1)/ny

   ! Put correct julian date into each location in the time array
   do j=1,ny
      imager_time%time(:,j) = jd1+(slo*float(j))
   end do

end subroutine get_slstr_startend


! Computes the name of the input file based upon the band name
! This is currently hard-coded for nadir files, need to add oblique
subroutine get_slstr_imnames(indir,inband,fname,bname)

   use preproc_constants_m

   character(len=path_length),  intent(in)    :: indir
   integer,  intent(in)                       :: inband
   character(len=path_length),  intent(out)   :: fname
   character(len=path_length),  intent(out)   :: bname

   character(len=3)                 :: band

   write (band, "(I1.1)") inband
   if (inband.le.6) then
      fname = trim(indir)//'S'//trim(band)//'_radiance_an.nc'
      bname = 'S'//trim(band)//'_radiance_an'
   else if (inband.le.9) then
      fname = trim(indir)//'S'//trim(band)//'_BT_in.nc'
      bname = 'S'//trim(band)//'_BT_in'
   else
      write(*,*)"Incorrect band:",inband,". This is not supported for SLSTR (1-9 only)."
      stop
   endif

end subroutine get_slstr_imnames

! Read the nadir-view thermal grid data from SLSTR. Need to update with oblique view
subroutine read_slstr_tirdata(indir,inband,outarr,sx,sy,nx,ny)

   use netcdf
   use preproc_constants_m

   character(len=path_length),  intent(in)   :: indir
   integer,  intent(in)                      :: inband
   integer,intent(in)                        :: sx
   integer,intent(in)                        :: sy
   integer,intent(in)                        :: nx
   integer,intent(in)                        :: ny
   real(kind=sreal),intent(out)               :: outarr(nx,ny)

   real                                         :: data1(nx,ny)

   character(len=path_length)       :: filename
   character(len=path_length)       :: bandname
   character(len=path_length)       :: fillname
   character(len=path_length)       :: sclname
   character(len=path_length)       :: offname

   real    filval,sclval,offval
   integer tmp,numDims, numAtts, i

   integer fid,did,ierr,curband,index2,j

   ! Find the filename required for this channel
   call get_slstr_imnames(indir,inband,filename,bandname)

   ! Open the netcdf file
   ierr=nf90_open(path=trim(adjustl(filename)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error opening file ',trim(filename)
      stop
   endif
   ! Check that the dataset exists
   ierr=nf90_inq_varid(fid, trim(bandname), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error opening dataset ',trim(filename),"in",trim(bandname)
      stop
   endif

   ! Get anciliary values (fill, scale, offset)
   ierr = nf90_get_att(fid, did, "_FillValue", filval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_tirdata(): Error getting ',trim(fillname),' from file ',trim(filename)
      stop
   endif
   ierr = nf90_get_att(fid, did, "scale_factor", sclval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_tirdata(): Error getting ',trim(sclname),' from file ',trim(filename)
      stop
   endif
   ierr = nf90_get_att(fid, did,"add_offset", offval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_tirdata(): Error getting ',trim(offname),' from file ',trim(filename)
      stop
   endif

   ! Get the actual data
   ierr = nf90_get_var(fid, did, data1)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error reading dataset ',trim(filename),"in",trim(bandname)
      stop
   endif
   ierr = nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr(): Error closing file ',trim(filename)
      stop
   endif

   ! Apply scale, offset and then fill for bad data
   outarr   =   data1*sclval + offval
   where(data1 .eq. filval) outarr=sreal_fill_value

end subroutine read_slstr_tirdata

! Read the nadir-view visible grid data from SLSTR. Need to update with oblique view
! This assumes that VIS data is twice resolution of TIR (should be correct, 0.5km)
subroutine read_slstr_visdata(indir,inband,outarr,imager_angles,sx,sy,nx,ny)

   use netcdf
   use preproc_constants_m
   use imager_structures_m

   character(len=path_length),  intent(in)   :: indir
   integer,  intent(in)                      :: inband
   integer,intent(in)                        :: sx
   integer,intent(in)                        :: sy
   integer,intent(in)                        :: nx
   integer,intent(in)                        :: ny
   real(kind=sreal),intent(out)               :: outarr(nx,ny)
   type(imager_angles_t),       intent(in)   :: imager_angles

   real,allocatable                             :: data1(:,:)
   real,allocatable                             :: data2(:,:)
   real,allocatable                             :: data3(:,:)
   real(kind=sreal), dimension(9)            :: irradiances

   character(len=path_length)       :: filename
   character(len=path_length)       :: bandname
   character(len=path_length)       :: fillname
   character(len=path_length)       :: sclname
   character(len=path_length)       :: offname

   real    filval,sclval,offval
   integer tmp,numDims, numAtts, i

   integer fid,did,ierr,curband,index2,j

   if (inband .lt. 1 .or. inband .gt. 9) then
      write(*,*)'SLSTR input band must be in range 1-9. Here we have',inband
      stop
   endif

   ! Find the filename required for this channel
   call get_slstr_imnames(indir,inband,filename,bandname)

   allocate(data1(nx*2,ny*2))
   allocate(data2(nx,ny))
   allocate(data3(nx,ny))

   ! Preliminary solar irradiances, these should be checked once final values available
   irradiances = (/ 1837.39, 1525.94, 956.17, 365.90, 248.33, 78.33, 0., 0., 0. /)

   ! Open the netcdf file
   ierr=nf90_open(path=trim(adjustl(filename)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error opening file ',trim(filename)
      stop
   endif
   ! Check that the dataset exists
   ierr=nf90_inq_varid(fid, trim(bandname), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error opening dataset ',trim(filename),"in",trim(bandname)
      stop
   endif

   ! Get anciliary values (fill, scale, offset)
   ierr = nf90_get_att(fid, did, "_FillValue", filval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_tirdata(): Error getting ',trim(fillname),' from file ',trim(filename)
      stop
   endif
   ierr = nf90_get_att(fid, did, "scale_factor", sclval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_tirdata(): Error getting ',trim(sclname),' from file ',trim(filename)
      stop
   endif
   ierr = nf90_get_att(fid, did,"add_offset", offval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_tirdata(): Error getting ',trim(offname),' from file ',trim(filename)
      stop
   endif
   ! Get the actual data
   ierr = nf90_get_var(fid, did, data1)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_tirdata(): Error reading dataset ',trim(filename),"in",trim(bandname)
      stop
   endif
   ierr = nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr(): Error closing file ',trim(filename)
      stop
   endif
   ! Resample the data to the TIR grid size.
   call slstr_resample_vis_to_tir(data1,data2,nx,ny,filval)
   ! Apply scale, offset and then fill for bad data
   outarr   =   data2*sclval + offval

   ! convert from radiances to reflectances
   outarr = (pi*outarr*cos(d2r*imager_angles%solzen(:,:,1)))/irradiances(inband)


   ! Apply scale, offset and then fill for bad data
   where(data2 .eq. filval) outarr=sreal_fill_value
   where(imager_angles%solzen(:,:,1) .eq. sreal_fill_value) outarr=sreal_fill_value

   deallocate(data1)
   deallocate(data2)

end subroutine read_slstr_visdata


! Resamples VIS data onto the TIR grid. Assumes that VIS resolution is twice
! that of the TIR images (0.5km vs 1km). This is not efficient, but works!
!
! To resample we simply average a 2x2 region of VIS into a 1x1 pixel of TIR
subroutine slstr_resample_vis_to_tir(inarr,outarr,nx,ny,fill)

   use preproc_constants_m

   integer,intent(in)                        :: nx
   integer,intent(in)                        :: ny
   real,intent(in)                           :: fill
   real(kind=sreal),intent(in)               :: inarr(nx*2,ny*2)
   real(kind=sreal),intent(out)               :: outarr(nx,ny)

   real tmpval

   integer x,y
   integer newx,newy,counter

   newx=1
   do x=1,nx*2-1,2
      newy=1
      do y=1,ny*2-1,2
         counter=0
         tmpval=0
         if (inarr(x,y).ne.fill) then
            tmpval=tmpval+inarr(x,y)
            counter=counter+1
         endif
         if (inarr(x+1,y).ne.fill) then
            tmpval=tmpval+inarr(x+1,y)
            counter=counter+1
         endif
         if (inarr(x,y+1).ne.fill) then
            tmpval=tmpval+inarr(x,y+1)
            counter=counter+1
         endif
         if (inarr(x+1,y+1).ne.fill) then
            tmpval=tmpval+inarr(x+1,y+1)
            counter=counter+1
         endif
         if (counter .gt. 0) then
            outarr(newx,newy) = tmpval/counter
         else
            outarr(newx,newy)=fill
         endif
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

   integer,intent(in)                         :: nx
   integer,intent(in)                         :: ny
   character(len=path_length),  intent(in)    :: indir
   integer(kind=sint),intent(out)             :: data_arr(nx,ny)

   integer(kind=sint),allocatable::   data(:,:)
   character(len=path_length)   ::   geofile
   character(len=path_length)   ::   var
   integer                     ::   fid,did,ierr,curband,index2,j
   real                        ::   filval,sclval,offval

   allocate(data(nx,ny))

   ! Hardcoded filenames, these shouldn't change for nominal SLSTR data
   var = 'elevation_in'
   geofile = trim(indir)//'geodetic_in.nc'

   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening file ',trim(geofile)
      stop
   endif
   ierr=nf90_inq_varid(fid, trim(var), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening dataset ',trim(var)," in ",trim(geofile)
      stop
   endif

   ierr = nf90_get_att(fid, did, "_FillValue", filval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting FillValue from file ',trim(geofile)
      stop
   endif
   ierr = nf90_get_att(fid, did, "scale_factor", sclval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting scale_factor from file ',trim(geofile)
      stop
   endif
   ierr = nf90_get_att(fid, did,"add_offset", offval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting add_offset from file ',trim(geofile)
      stop
   endif
   ierr = nf90_get_var(fid, did, data)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error reading dataset ',trim(var)," in ",trim(geofile)
      stop
   endif
   ierr = nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error closing file ',trim(geofile)
      stop
   endif

   ! Scale and offset results, then apply the fill value
   data_arr   =   data*sclval + offval
   where(data .eq. filval) data_arr=sreal_fill_value

   deallocate(data)
end subroutine read_slstr_demdata

! Read the lat/lon data on the TIR grid
subroutine read_slstr_lldata(indir,data_arr,nx,ny,proclat)

   use preproc_constants_m
   use imager_structures_m
   use netcdf

   integer,intent(in)                         :: nx
   integer,intent(in)                         :: ny
   character(len=path_length),  intent(in)    :: indir
   real(kind=sreal),intent(out)               :: data_arr(nx,ny)
   logical                                    :: proclat

   integer(kind=sreal),allocatable::   data(:,:)
   character(len=path_length)       ::   geofile
   character(len=path_length)     ::   var
   integer                         ::   fid,did,ierr,curband,index2,j
   real                            ::   filval,sclval,offval

   allocate(data(nx,ny))
   if (proclat .eqv. .true.) then
      var = 'latitude_in'
   else
      var = 'longitude_in'
   endif
   geofile = trim(indir)//'geodetic_in.nc'

   ! First we do the DEM
   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening file ',trim(geofile)
      stop
   endif
   ierr=nf90_inq_varid(fid, trim(var), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening dataset ',trim(var)," in ",trim(geofile)
      stop
   endif

   ierr = nf90_get_att(fid, did, "_FillValue", filval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting FillValue from file ',trim(geofile)
      stop
   endif
   ierr = nf90_get_att(fid, did, "scale_factor", sclval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting scale_factor from file ',trim(geofile)
      stop
   endif
   ierr = nf90_get_att(fid, did,"add_offset", offval)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: get_slstr_geodata(): Error getting add_offset from file ',trim(geofile)
      stop
   endif
   ierr = nf90_get_var(fid, did, data)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error reading dataset ',trim(var)," in ",trim(geofile)
      stop
   endif
   ierr = nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error closing file ',trim(geofile)
      stop
   endif

   data_arr   =   data*sclval + offval
   where(data .eq. filval) data_arr=sreal_fill_value

   deallocate(data)
end subroutine read_slstr_lldata

! Read SZA, SAA, VZA, VAA from the appropriate file
! For reasons that make no sense these are on a completely different
! Grid to either the TIR or VIS data. We therefore also have to convert
! from this 'tie-point' grid onto something that's actually sensible and useful.
subroutine read_slstr_satsol(indir,imager_angles,nx,ny,startx)

   use preproc_constants_m
   use imager_structures_m
   use netcdf

   integer,intent(in)                         :: nx
   integer,intent(in)                         :: ny
   integer,intent(in)                         :: startx
   character(len=path_length),  intent(in)    :: indir
   type(imager_angles_t),       intent(inout) :: imager_angles

   character(len=path_length)       ::   geofile
   character(len=path_length)     ::   var
   integer                         ::   fid,did,ierr,curband,index2,j
   real                            ::   filval,sclval,offval

   ! Input file, on the geometry grid (tn)
   geofile = trim(indir)//'geometry_tn.nc'

   ! First we do the DEM
   ierr=nf90_open(path=trim(adjustl(geofile)),mode=NF90_NOWRITE,ncid=fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_geodata(): Error opening file ',trim(geofile)
      stop
   endif

   ! Retrieve each variable and resample from tn to in grid
   call slstr_get_one_geom(nx,ny,fid,'sat_azimuth_tn',imager_angles%relazi,geofile)
   call slstr_get_one_geom(nx,ny,fid,'sat_zenith_tn',imager_angles%satzen,geofile)
   call slstr_get_one_geom(nx,ny,fid,'solar_azimuth_tn',imager_angles%solazi,geofile)
   call slstr_get_one_geom(nx,ny,fid,'solar_zenith_tn',imager_angles%solzen,geofile)

   ierr = nf90_close(fid)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_get_one_geom(): Error closing file ',trim(geofile)
      stop
   endif

   ! Check bounds, compute the raa

   where(imager_angles%solazi(startx:,:,1)       .lt. -360) &
      imager_angles%solazi(startx:,:,1)=sreal_fill_value
   where(imager_angles%solzen(startx:,:,1)       .lt. -90) &
      imager_angles%solzen(startx:,:,1)=sreal_fill_value
   where(imager_angles%satzen(startx:,:,1)       .lt. -90) &
      imager_angles%satzen(startx:,:,1)=sreal_fill_value
   where(imager_angles%relazi(startx:,:,1)       .lt. -360) &
      imager_angles%relazi(startx:,:,1)=sreal_fill_value

   where(imager_angles%solazi(startx:,:,1)       .gt. 360) &
      imager_angles%solazi(startx:,:,1)=sreal_fill_value
   where(imager_angles%solzen(startx:,:,1)       .gt. 90) &
      imager_angles%solzen(startx:,:,1)=sreal_fill_value
   where(imager_angles%satzen(startx:,:,1)       .gt. 90) &
      imager_angles%satzen(startx:,:,1)=sreal_fill_value
   where(imager_angles%relazi(startx:,:,1)       .gt. 360) &
      imager_angles%relazi(startx:,:,1)=sreal_fill_value

   ! Rescale zens + azis into correct format
   where(imager_angles%solazi(startx:,:,1) .ne. sreal_fill_value .and. &
         imager_angles%relazi(startx:,:,1) .ne. sreal_fill_value)
      imager_angles%relazi(:,:,1) = abs(imager_angles%relazi(startx:,:,1) - &
                                        imager_angles%solazi(startx:,:,1))

      where (imager_angles%relazi(:,:,1) .gt. 180.)
         imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
      endwhere
   end where
end subroutine read_slstr_satsol

! Get one of the geometry variables, then resample to the correct grid
subroutine slstr_get_one_geom(nx,ny,fid,var,odata,geofile)


   use preproc_constants_m
   use imager_structures_m
   use netcdf

   integer,intent(in)                         :: nx
   integer,intent(in)                         :: ny
   integer,  intent(in)                       :: fid
   character(len=*),  intent(in)              :: var
   real(kind=sreal),intent(out)               :: odata(nx,ny)
   character(len=path_length),  intent(in)    :: geofile

   real(kind=sreal),allocatable::   tdata(:,:)

   integer did,ierr
   integer xstep,ystep

   ! Hardcoded TX grid. We should really do something more robust here
   xstep=130
   ystep=1200

   allocate(tdata(xstep,ystep))

   ierr=nf90_inq_varid(fid, trim(var), did)
   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_get_one_geom(): Error opening dataset ',trim(var)," in ",trim(geofile)
      stop
   endif
   ierr = nf90_get_var(fid, did, tdata)

   if (ierr.ne.NF90_NOERR) then
      print*,'ERROR: read_slstr_get_one_geom(): Error reading dataset ',trim(var)," in ",trim(geofile)
      stop
   endif

   ! Do the resampling.
   call slstr_resample_geom(tdata,odata,nx,ny,xstep,ystep,12,16)

end subroutine slstr_get_one_geom

! This routing resamples the TX (tie-point) grid data onto the IN (TIR) grid.
subroutine slstr_resample_geom(indata,outdata,nx1,ny1,nx2,ny2,startx,step)

   use preproc_constants_m
   use imager_structures_m

   integer,intent(in)                         :: nx1
   integer,intent(in)                         :: ny1
   integer,intent(in)                         :: nx2
   integer,intent(in)                         :: ny2
   integer,intent(in)                         :: startx
   integer,intent(in)                         :: step

   real(kind=sreal),intent(in)                :: indata(nx2,ny2)
   real(kind=sreal),intent(out)               :: outdata(nx1,ny1)

   integer          ::  i,j,k,xpos
   real(kind=sreal) ::  slo,off,tmp1,tmp2

   ! This section loops over each pixel in the input solar/sat geom file
   ! By default the y-axis is the same (1200px) but the x axis is 16km
   ! instead of 1km. Annoyingly, the geom grid does not start in the same
   ! place as the sat grid, there's an offset. It's also not the same ending
   ! point. Therefore we start at some undefined point in the middle of the
   ! grid and stop at another undefined point. This is a poor-mans way of
   ! doing things but will do just now. Ideally we want to perform this based
   ! on nearest-neighbour between TIR and geom lat/lon grid. (on todo list)

   do j=1,ny2
      do i=startx,nx2-1
         slo=(indata(i+1,j)-indata(i,j))/step
         off=indata(i,j)
         do k=0,step-1
            xpos   =   ((i-startx)*step)+(k)
            if (xpos.lt.nx1) then
               outdata(xpos,j)=off+(slo*k)
            endif
         end do
      end do
   end do

end subroutine slstr_resample_geom

