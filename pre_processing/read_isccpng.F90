!-------------------------------------------------------------------------------
! Name: read_isccpng.F90
!
! Purpose:
! Module for ISCCPNG I/O routines.
!
! History:
! 2015/02/15, GM: First version.
! 2015/07/30, GM: Fixed relative azimuth angle.
! 2015/08/17, GM: Adapt to the newest version of seviri_util.
! 2015/08/19, GM: Modifications to support the SEVIRI HRIT format.
! 2016/12/08, GT: Fixed solar azimuth angle.
! 2017/07/18, SP: Added a (basic) method of subsetting HRIT data
! 2018/06/03, SP: GSICS calibration is now supported for SEVIRI. The default
!                 setting is ON, meaning that GSICS coefficients will be used
!                 instead of IMPF (as previous). The new driver file option
!                 USE_GSICS enables this to be disabled.
! 2021/02/23, DP: Shifted satellite azimuth from [0,360] to [-180,180] range
!                 to match ORAC definition.
! 2025/02/17, DR: Added second subroutine that adds in all the channels without
!                 the spectral adjustment. This relies on using the correct LUTs
!                 for each instrument, rather than just using the SEVIRI LUTs.
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_isccpng_m

   implicit none

   integer, parameter :: SEVIRI_TYPE_METOFF = 1
   integer, parameter :: SEVIRI_TYPE_HRIT   = 2
   integer, parameter :: SEVIRI_TYPE_NAT    = 3

   private

   public :: read_isccpng_dimensions, &
             read_isccpng_l1_5, &
             read_isccpng_geo

contains


!-------------------------------------------------------------------------------
! Name: read_seviri_dimensions
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! l1_5_file      string  in   Full path to the native level 1.5 image data
! n_across_track lint    out  Number columns in the SEVIRI disk image (constant)
! n_along_track  lint    out  Number lines   in the SEVIRI disk image (constant)
! startx         lint    both First column desired by the caller
! endx           lint    both First line desired by the caller
! starty         lint    both Last column desired by the caller
! endy           lint    both Last line desired by the caller
! verbose        logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_isccpng_dimensions(l1_5_file, n_across_track, n_along_track, &
                                  startx, endx, starty, endy, verbose)

   use preproc_constants_m
   use orac_ncdf_m

   implicit none

   character(len=*),   intent(in)    :: l1_5_file
   integer(kind=lint), intent(out)   :: n_across_track, n_along_track
   integer(kind=lint), intent(inout) :: startx, endx, starty, endy
   logical,            intent(in)    :: verbose

   integer :: i_line, i_column
   integer :: n_lines, n_columns
   integer :: ftype
   integer :: ncid

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< read_isccpng_dimensions()'
   ! These are constant for the full disk image.

   !n_along_track  = 3600
   !n_across_track = 7200

   call ncdf_open(ncid, trim(l1_5_file), 'l1_5_file')
    n_along_track  = ncdf_dim_length(ncid, 'latitude', 'read_l1_5_file()')
    n_across_track  = ncdf_dim_length(ncid, 'longitude', 'read_l1_5_file()')
   call ncdf_close(ncid, 'l1_5_file')
   
   if(n_along_track .eq. 0 .and. n_across_track .eq. 0) then
      call ncdf_open(ncid, trim(l1_5_file), 'l1_5_file')
      n_along_track  = ncdf_dim_length(ncid, 'lat', 'read_l1_5_file()')
      n_across_track  = ncdf_dim_length(ncid, 'lon', 'read_l1_5_file()')
      call ncdf_close(ncid, 'l1_5_file')
   endif

print*,trim(l1_5_file)
print*,'AAAA',n_along_track,n_across_track
   i_line    = 0
   i_column  = 0
   n_lines   = n_along_track !3600
   n_columns = n_across_track !7200

   if (startx .le. 0 .or. endx .le. 0 .or. starty .le. 0 .or. endy .le. 0) then
      ! If start and end *are not* being used then set them to the start and end
      ! of the actual image in the file.
      starty = i_line + 1
      endy   = i_line + n_lines
      startx = i_column + 1
      endx   = i_column + n_columns
   else
      ! If start and end *are* being used then check that they fall within the
      ! actual image in the file relative to the full disk image.
      if (starty - 1 .lt.  i_line) then
         write(*,*) 'ERROR: user defined starty: ', starty, ', does not ' // &
                    'fall within the actual ISCCPNG image starting at: ', &
                    i_line + 1
         stop error_stop_code
      end if
      if (endy - 1 .gt. i_line   + n_lines - 1) then
         write(*,*) 'ERROR: user defined endy: ', endy, ', does not ' // &
                    'fall within the actual ISCCPNG image ending at: ', &
                    i_line   + n_lines
         stop error_stop_code
      end if
      if (startx - 1 .lt.  i_column) then
         write(*,*) 'ERROR: user defined startx: ', startx, ', does not ' // &
                    'fall within the actual ISCCPNG image starting at: ', &
                    i_column + 1
         stop error_stop_code
      end if
      if (endx - 1 .gt. i_column + n_columns - 1) then
         write(*,*) 'ERROR: user defined endx: ', endx, ', does not ' // &
                    'fall within the actual ISCCPNG image ending at: ', &
                    i_column + n_columns
         stop error_stop_code
      end if
   end if
   if (index(l1_5_file, trim('RSS'))>0 .and. ftype==SEVIRI_TYPE_HRIT) then
      endy = endy/3
   end if
   print*,"endx: ",endx,"endy: ",endy

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> read_isccpng_dimensions()'

end subroutine read_isccpng_dimensions

! Temporary function for use until seviri_util predef geo is fixed. INEFFICIENT.
subroutine read_isccpng_geo(imager_geolocation, imager_angles, &
     geofile, verbose)

   use orac_ncdf_m
   use imager_structures_m
   implicit none

   type(imager_geolocation_t), intent(inout) :: imager_geolocation
   type(imager_angles_t),      intent(inout) :: imager_angles
   character(len=*),           intent(in)    :: geofile
   logical,                    intent(in)    :: verbose
   
   integer :: ncid, start(2),aaa,bbb
   integer(kind=lint) :: n_across_track, n_along_track

   start(1) = imager_geolocation%startx
   start(2) = imager_geolocation%starty

   print*,'opening geofile',trim(geofile)
   call ncdf_open(ncid, trim(geofile), 'ISCCPNG_Retrieve_Predef_Geo()')

   n_along_track  = ncdf_dim_length(ncid, 'latitude', 'abc')
   n_across_track  = ncdf_dim_length(ncid, 'longitude', 'def')

   if(n_along_track .gt. 1 .and. n_across_track .gt. 1) then
      call ncdf_read_array(ncid, "latitude", imager_geolocation%latitude, start=start)
      call ncdf_read_array(ncid, "longitude", imager_geolocation%longitude, start=start)
   else 
      call ncdf_read_array(ncid, "lat", imager_geolocation%latitude, start=start)
      call ncdf_read_array(ncid, "lon", imager_geolocation%longitude, start=start)
   end if

   !call ncdf_read_array(ncid, "satellite_zenith", imager_angles%satzen(:,:,1), start=start)
   !call ncdf_read_array(ncid, "VAA", imager_angles%satazi(:,:,1), start=start)
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')
   print*,'O',shape(imager_geolocation%longitude)
   print*,'A',minval(imager_geolocation%latitude),maxval(imager_geolocation%latitude)
   print*,'B',minval(imager_geolocation%longitude),maxval(imager_geolocation%longitude)

   

end subroutine read_isccpng_geo


!-------------------------------------------------------------------------------
! Name: read_isccpng_l1_5
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name                Type    In/Out/Both Description
! l1_5_file           string  in   Full path to the native level 1.5 image data
! imager_geolocation  struct  both Members within are populated
! imager_measurements struct  both Members within are populated
! imager_angles       struct  both Members within are populated
! imager_time         struct  both Members within are populated
! channel_info        struct  both Members within are populated
! global_atts         struct  both Members within are populated
! verbose             logical in   If true then print verbose information.
!-------------------------------------------------------------------------------
subroutine read_isccpng_l1_5(l1_5_file, imager_geolocation, imager_measurements, &
   imager_angles, imager_time, channel_info, do_gsics, do_nasa, global_atts, verbose)

   use channel_structures_m
   use global_attributes_m
   use imager_structures_m

   implicit none

   character(len=*),            intent(in)    :: l1_5_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: do_gsics
   logical,                     intent(in)    :: do_nasa
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   integer :: startx

! Spectral Adjustment Method
!   call read_isccpng_l1_5_nc_spectral(l1_5_file, imager_geolocation, &
!           imager_measurements, imager_angles, imager_time, channel_info, &
!           do_gsics, do_nasa, global_atts, verbose)

! Induvidual LUT Method
   call read_isccpng_l1_5_nc(l1_5_file, imager_geolocation, &
           imager_measurements, imager_angles, imager_time, channel_info, &
           do_gsics, do_nasa, global_atts, verbose)
   

   startx = imager_geolocation%startx

   where(imager_angles%solazi(startx:,:,1) .ne. sreal_fill_value .and. &
         imager_angles%satazi(startx:,:,1) .ne. sreal_fill_value)
!      imager_angles%solazi(:,:,1) = imager_angles%solazi(startx:,:,1) - 180.
!      where(imager_angles%solazi(:,:,1) .lt. 0.)
!         imager_angles%solazi(:,:,1) = imager_angles%solazi(:,:,1) + 360.
!      end where

      imager_angles%relazi(:,:,1) = abs(imager_angles%satazi(startx:,:,1) - &
                                        imager_angles%solazi(startx:,:,1))

      where (imager_angles%relazi(:,:,1) .gt. 180.)
         imager_angles%relazi(:,:,1) = 360. - imager_angles%relazi(:,:,1)
      end where
   end where

end subroutine read_isccpng_l1_5


subroutine read_isccpng_l1_5_nc_spectral(l1_5_file, imager_geolocation, &
   imager_measurements, imager_angles, imager_time, channel_info, do_gsics, &
   do_nasa, global_atts, verbose)

   use iso_c_binding
   use channel_structures_m
   use common_constants_m, only: error_stop_code
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use orac_ncdf_m
   use calender_m

   implicit none

   character(len=*),            intent(in)    :: l1_5_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: do_gsics
   logical,                     intent(in)    :: do_nasa
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   integer                     :: i,index1,index2
   integer(c_int)              :: n_bands
   integer(c_int), allocatable :: band_ids(:)
   integer(c_int), allocatable :: band_units(:)
   integer                     :: n_across_track, n_along_track
   integer                     :: startx, nx, nx_full
   integer                     :: starty, ny, ny_full
   integer(c_int)              :: line0, line1
   integer(c_int)              :: column0, column1
   integer(kind=sint)          :: iyear, imonth, iday, ihour, iminute
   real(kind=8)                :: jday
   logical                     :: hrit_proc
   integer                     :: ncid, start(2)
   character(len=300)          :: tmpfile
   character(len=12)           :: chstr
   real(kind=4), allocatable   :: tmplat(:),tmplon(:)
   integer, allocatable        :: wmo_id(:,:)
   real(kind=4), allocatable   :: rad_tmp(:,:)
   character(len=3)            :: sat_ids(4)
   real(kind=4)                :: sba_slope(4,2,11),sba_offset(4,2,11)
 
   start(1) = imager_geolocation%startx
   start(2) = imager_geolocation%starty

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_isccpng_l1_5_nc()'

   ! Fetch image dimensions (startx/y are placeholders and overriden below)
   startx = 0
   nx_full = 7200
   starty = 0
   ny_full = 3600
   call read_isccpng_dimensions(l1_5_file, n_across_track, n_along_track, &
        startx, nx_full, starty, ny_full, .false.)

   ! Setup some arguments to seviri_read_and_preproc_f90()

   n_bands = channel_info%nchannels_total
   print *,'Number of bands available: ',n_bands

   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr

   allocate(band_units(n_bands))
!   do i = 1, n_bands
!      if (channel_info%channel_lw_flag(i) .eq. 0) then
!         band_units(i) = SEVIRI_UNIT_REF
!      else
!         band_units(i) = SEVIRI_UNIT_BT
!      end if
!   end do

   if (verbose) then
      if (do_gsics) then
         write(*,*) 'Applying GSICS calibration coefficients'
      else
         write(*,*) 'Applying IMPF calibration coefficients'
      end if
   end if

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   line0   = starty - 1
   line1   = starty - 1 + ny - 1
   column0 = startx - 1
   column1 = startx - 1 + nx - 1

   ! imager_time%time(startx:,:)             = 200.
   ! imager_geolocation%latitude(startx:,:)  = 200.
   ! imager_geolocation%longitude(startx:,:) = 200.
   !imager_angles%solzen(startx:,:,1)       = 40.
   !imager_angles%solazi(startx:,:,1)       = 40.
   !imager_angles%satzen(startx:,:,1)       = 40.
   !imager_angles%satazi(startx:,:,1)       = 40.
   !imager_measurements%data(startx:,:,:)   = 200.
   !imager_measurements%cal_gain(:)         = 200.


!/cmsaf/cloud_cci/ISCCP_NG/L1g/demo_20211115/20200701T0500/ISCCP-NG_L1g_demo_A1_v1_res_0_10deg__refl_00_65um_20200701T0500.nc
   index1 = index(l1_5_file, 'deg__', back=.true.)
   index2 = index(l1_5_file, 'um_', back=.true.)

! Need to assign time at the pixel level
   tmpfile=l1_5_file(1:index1+4)//'pixel_time'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'pixel_time', imager_time%time(startx:,:), start=[startx, starty])
   ! Need to get into Julian format from simple seconds from start set in filename
   read(l1_5_file(index2+4:index2+7), *) iyear
   read(l1_5_file(index2+8:index2+9), *) imonth
   read(l1_5_file(index2+10:index2+11), *) iday
   read(l1_5_file(index2+13:index2+14), *) ihour
   read(l1_5_file(index2+15:index2+16), *) iminute
   call GREG2JD(iyear, imonth, iday, jday)
   imager_time%time(startx:,:) = jday + iminute + imager_time%time(startx:,:)/60.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

! Then we can assign the 0.65um reflectances
   chstr='refl_00_65um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   allocate(tmplat(ny))
   allocate(tmplon(nx))
   allocate(wmo_id(nx,ny))
   allocate(rad_tmp(nx,ny))


   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   if(n_across_track .eq. 7200 .and. n_along_track .eq. 3600) then
      call ncdf_read_array(ncid, "latitude", tmplat, start=[starty])!imager_geolocation%latitude(1,:), start=[1,starty])
      call ncdf_read_array(ncid, "longitude", tmplon, start=[startx])!imager_geolocation%longitude(:,1), start=[startx,1])
   else 
      call ncdf_read_array(ncid, "lat", tmplat, start=[starty])
      call ncdf_read_array(ncid, "lon", tmplon, start=[startx])
   end if
call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,1), start=[startx, starty])
   imager_measurements%data(:,:,1)=imager_measurements%data(:,:,1)/100.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')
print*,minval(tmplon),maxval(tmplon)
print*,minval(tmplat),maxval(tmplat)

   chstr='refl_00_86um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,2), start=[startx, starty])
   imager_measurements%data(:,:,2)=imager_measurements%data(:,:,2)/100.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='refl_01_60um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,3), start=[startx, starty])
   imager_measurements%data(:,:,3)=imager_measurements%data(:,:,3)/100.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_03_80um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,4), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_06_20um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,5), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_07_30um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,6), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   !chstr='temp_08_70um'
   chstr='temp_08_60um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,7), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   !chstr='temp_08_50um'
   !tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   !print*,tmpfile
   !call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   !call ncdf_read_array(ncid, chstr, rad_tmp, start=[startx, starty])
   !call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   !where (imager_measurements%data(:,:,7) .lt. 0.) imager_measurements%data(:,:,7)=rad_tmp


   chstr='temp_11_00um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,9), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_12_00um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,10), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_13_30um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,11), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')
   
   tmpfile=l1_5_file(1:index1+4)//'wmo_id'//l1_5_file(index2+2:)
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'wmo_id', wmo_id, start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

!sat_ids(4)
!sba_slope(4,2,11),sba_offset(4,2,11)

!wmo_id:satellite_names = "270=GOES-16;271=GOES-17;173=Himawari-8;55=Meteosat-8;70=Meteosat-11" ;

sat_ids(1)='270'
sba_slope(1,1,:)=(/1.00166,0.948021,1.00000,0.926764,0.991329,0.999300,1.01977,1.03453,0.996781,1.02343,0.863748/)
sba_slope(1,2,:)=(/1.00000,1.00000,1.00000,0.940848,0.990431,1.00483,1.01947,1.01835,0.996272,1.02239,0.890045/)
sba_offset(1,1,:)=(/-0.00207069,0.161403,0.00000,18.4795,1.58729,-0.0312020,-4.93224,-7.83331,1.13395,-5.51940,30.5574/)
sba_offset(1,2,:)=(/0.00000,0.00000,0.00000,14.4615,1.80946,-1.50406,-4.94939,-3.61878,1.23287,-5.28908,23.4361/)

sat_ids(2)='271'
sba_slope(2,1,:)=(/1.00313,0.947739,1.00000,0.928358,0.991436,0.999576,1.01939,1.03306,0.996656,1.02405,0.894260/)
sba_slope(2,2,:)=(/1.00000,1.00000,1.00000,0.939285,0.990504,1.00481,1.01907,1.01759,0.996129,1.02298,0.917607/)
sba_offset(2,1,:)=(/-0.0107727,0.161054,0.00000,17.7929,1.57229,-0.0877037,-4.83326,-7.49875,1.17318,-5.66102,23.6271/)
sba_offset(2,2,:)=(/0.00000,0.00000,0.00000,14.7561,1.80263,-1.48189,-4.84116,-3.46801,1.27653,-5.42165,17.3323/)

sat_ids(3)='173'
sba_slope(3,1,:)=(/1.00169,0.945509,1.00000,0.929371,1.00905,1.00090,1.01065,1.02516,0.996015,1.02885,0.871186/)
sba_slope(3,2,:)=(/1.00000,1.00000,1.00000,0.938706,1.00526,1.00413,1.01050,1.01338,0.995399,1.02742,0.896835/)
sba_offset(3,1,:)=(/-0.00100716,0.162379,0.00000,17.3952,-2.21718,-0.347357,-2.62520,-5.68860,1.37321,-6.76384,28.8614/)
sba_offset(3,2,:)=(/0.00000,0.00000,0.00000,14.8503,-1.29354,-1.20664,-2.63734,-2.61762,1.49858,-6.43452,21.9239/)

sat_ids(4)='55'
sba_slope(4,1,:)=(/0.999577,0.999643,1.00000,1.01192,1.00193,1.00093,1.00202,0.998663,0.999412,0.999663,0.945356/)
sba_slope(4,2,:)=(/1.00000,1.00000,1.00000,1.00882,1.00146,1.00121,1.00194,0.999321,0.999513,0.999684,0.955400/)
sba_offset(4,1,:)=(/0.00444712,0.0162967,0.00000,-2.83943,-0.417072,-0.181784,-0.486767,0.304194,0.134653,0.0706959,12.3763/)
sba_offset(4,2,:)=(/0.00000,0.00000,0.00000,-1.94343,-0.302762,-0.259928,-0.471424,0.131567,0.104622,0.0658078,9.71430/)

!print*,minval(wmo_id),maxval(wmo_id)
print*,'shape: ',shape(imager_measurements%data)
!stop
do i=1,11
   where (wmo_id.eq.270) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(1,2,i)+sba_offset(1,2,i)
   where (wmo_id.eq.271) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(2,2,i)+sba_offset(2,2,i)
   where (wmo_id.eq.173) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(3,2,i)+sba_offset(3,2,i)
   where (wmo_id.eq.55) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(4,2,i)+sba_offset(4,2,i)
end do
   tmpfile=l1_5_file(1:index1+4)//'satellite_zenith_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'satellite_zenith_angle', imager_angles%satzen(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   tmpfile=l1_5_file(1:index1+4)//'satellite_azimuth_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'satellite_azimuth_angle', imager_angles%satazi(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   tmpfile=l1_5_file(1:index1+4)//'solar_zenith_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'solar_zenith_angle', imager_angles%solzen(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   tmpfile=l1_5_file(1:index1+4)//'solar_azimuth_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'solar_azimuth_angle', imager_angles%solazi(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')


   !imager_angles%solazi(startx:,:,1)=imager_angles%solazi(startx:,:,1)-90.
   !where(imager_angles%solazi(startx:,:,1) .lt. -180.)
   !   imager_angles%solazi(startx:,:,1)=imager_angles%solazi(startx:,:,1)+360.
   !end where

   where (imager_angles%solazi(startx:,:,1) .gt. 180.)
      imager_angles%solazi(startx:,:,1) = imager_angles%solazi(startx:,:,1) - 360.
   end where

   where (imager_angles%satazi(startx:,:,1) .gt. 180.)
      imager_angles%satazi(startx:,:,1) = imager_angles%satazi(startx:,:,1) - 360.
   end where

   do i=1,nx
      imager_geolocation%latitude(startx+i-1,:)=tmplat(:)
   end do
   do i=1,ny
      imager_geolocation%longitude(startx:,i)=tmplon(:)
   end do

   deallocate(tmplat)
   deallocate(tmplon)
   deallocate(wmo_id)
   deallocate(rad_tmp)

   ! Remove underscores added by seviri_util (easy way of converting c-string to
   ! f-string).
   i = index(global_atts%Satpos_Metadata, '_')
   global_atts%Satpos_Metadata = global_atts%Satpos_Metadata(1:i-1)

   deallocate(band_ids)
   deallocate(band_units)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_isccpng_l1_5_nc()'

end subroutine read_isccpng_l1_5_nc_spectral

! UPDATED SUBROUTINE FOR READING DATA AS SINGLE INSTRUMENT
subroutine read_isccpng_l1_5_nc(l1_5_file, imager_geolocation, &
   imager_measurements, imager_angles, imager_time, channel_info, do_gsics, &
   do_nasa, global_atts, verbose)

   use iso_c_binding
   use channel_structures_m
   use common_constants_m, only: error_stop_code
   use global_attributes_m
   use imager_structures_m
   use preproc_constants_m
   use orac_ncdf_m
   use calender_m

   implicit none

   character(len=*),            intent(in)    :: l1_5_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   logical,                     intent(in)    :: do_gsics
   logical,                     intent(in)    :: do_nasa
   type(global_attributes_t),   intent(inout) :: global_atts
   logical,                     intent(in)    :: verbose

   integer                     :: i,index1,index2
   integer(c_int)              :: n_bands
   integer(c_int), allocatable :: band_ids(:)
   integer(c_int), allocatable :: band_units(:)
   integer                     :: n_across_track, n_along_track
   integer                     :: startx, nx, nx_full
   integer                     :: starty, ny, ny_full
   integer(c_int)              :: line0, line1
   integer(c_int)              :: column0, column1
   integer(kind=sint)          :: iyear, imonth, iday, ihour, iminute
   real(kind=8)                :: jday
   logical                     :: hrit_proc
   integer                     :: ncid, start(2)
   character(len=300)          :: tmpfile
   character(len=12)           :: chstr
   real(kind=4), allocatable   :: tmplat(:),tmplon(:)
   integer, allocatable        :: wmo_id(:,:)
   real(kind=4), allocatable   :: rad_tmp(:,:)
   character(len=3)            :: sat_ids(4)
   real(kind=4)                :: sba_slope(4,2,11),sba_offset(4,2,11)
 
   start(1) = imager_geolocation%startx
   start(2) = imager_geolocation%starty

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_isccpng_l1_5_nc()'

   ! Fetch image dimensions (startx/y are placeholders and overriden below)
   startx = 0
   nx_full = 7200
   starty = 0
   ny_full = 3600
   call read_isccpng_dimensions(l1_5_file, n_across_track, n_along_track, &
        startx, nx_full, starty, ny_full, .false.)

   ! Setup some arguments to seviri_read_and_preproc_f90()

   n_bands = channel_info%nchannels_total
   print *,'Number of bands available: ',n_bands

   allocate(band_ids(n_bands))
   band_ids = channel_info%channel_ids_instr

   allocate(band_units(n_bands))
!   do i = 1, n_bands
!      if (channel_info%channel_lw_flag(i) .eq. 0) then
!         band_units(i) = SEVIRI_UNIT_REF
!      else
!         band_units(i) = SEVIRI_UNIT_BT
!      end if
!   end do

   if (verbose) then
      if (do_gsics) then
         write(*,*) 'Applying GSICS calibration coefficients'
      else
         write(*,*) 'Applying IMPF calibration coefficients'
      end if
   end if

   startx = imager_geolocation%startx
   nx     = imager_geolocation%nx
   starty = imager_geolocation%starty
   ny     = imager_geolocation%ny

   line0   = starty - 1
   line1   = starty - 1 + ny - 1
   column0 = startx - 1
   column1 = startx - 1 + nx - 1

   ! imager_time%time(startx:,:)             = 200.
   ! imager_geolocation%latitude(startx:,:)  = 200.
   ! imager_geolocation%longitude(startx:,:) = 200.
   !imager_angles%solzen(startx:,:,1)       = 40.
   !imager_angles%solazi(startx:,:,1)       = 40.
   !imager_angles%satzen(startx:,:,1)       = 40.
   !imager_angles%satazi(startx:,:,1)       = 40.
   !imager_measurements%data(startx:,:,:)   = 200.
   !imager_measurements%cal_gain(:)         = 200.


!/cmsaf/cloud_cci/ISCCP_NG/L1g/demo_20211115/20200701T0500/ISCCP-NG_L1g_demo_A1_v1_res_0_10deg__refl_00_65um_20200701T0500.nc
   index1 = index(l1_5_file, 'deg__', back=.true.)
   index2 = index(l1_5_file, 'um_', back=.true.)

! Need to assign time at the pixel level
   tmpfile=l1_5_file(1:index1+4)//'pixel_time'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'pixel_time', imager_time%time(startx:,:), start=[startx, starty])
   ! Need to get into Julian format from simple seconds from start set in filename
   read(l1_5_file(index2+4:index2+7), *) iyear
   read(l1_5_file(index2+8:index2+9), *) imonth
   read(l1_5_file(index2+10:index2+11), *) iday
   read(l1_5_file(index2+13:index2+14), *) ihour
   read(l1_5_file(index2+15:index2+16), *) iminute
   call GREG2JD(iyear, imonth, iday, jday)
   imager_time%time(startx:,:) = jday + iminute + imager_time%time(startx:,:)/60.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

! Then we can assign the 0.65um reflectances
   chstr='refl_00_65um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   allocate(tmplat(ny))
   allocate(tmplon(nx))
   allocate(wmo_id(nx,ny))
   allocate(rad_tmp(nx,ny))


   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   if(n_across_track .eq. 7200 .and. n_along_track .eq. 3600) then
      call ncdf_read_array(ncid, "latitude", tmplat, start=[starty]) !imager_geolocation%latitude(1,:), start=[1,starty])
      call ncdf_read_array(ncid, "longitude", tmplon, start=[startx]) !imager_geolocation%longitude(:,1), start=[startx,1])
   else 
      call ncdf_read_array(ncid, "lat", tmplat, start=[starty])
      call ncdf_read_array(ncid, "lon", tmplon, start=[startx])
   end if
call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,1), start=[startx, starty])
   imager_measurements%data(:,:,1)=imager_measurements%data(:,:,1)/100.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')
print*,minval(tmplon),maxval(tmplon)
print*,minval(tmplat),maxval(tmplat)

   chstr='refl_00_86um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,2), start=[startx, starty])
   imager_measurements%data(:,:,2)=imager_measurements%data(:,:,2)/100.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='refl_01_60um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,3), start=[startx, starty])
   imager_measurements%data(:,:,3)=imager_measurements%data(:,:,3)/100.
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_03_80um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,4), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_06_20um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,5), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   chstr='temp_07_30um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,6), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   !chstr='temp_08_70um'
   chstr='temp_08_60um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,7), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   !chstr='temp_08_50um'
   !tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   !print*,tmpfile
   !call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   !call ncdf_read_array(ncid, chstr, rad_tmp, start=[startx, starty])
   !call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   !where (imager_measurements%data(:,:,7) .lt. 0.) imager_measurements%data(:,:,7)=rad_tmp


   chstr='temp_11_00um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,9), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   print*,"We're still reading here..."
   chstr='temp_12_00um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   print*,"Stuck at point 1?"
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,10), start=[startx, starty])
   print*,"Stuck at point 2?"
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   print*,"We're still reading here too..."
   chstr='temp_13_30um'
   tmpfile=l1_5_file(1:index1+4)//chstr//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, chstr, imager_measurements%data(:,:,11), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')
   
   tmpfile=l1_5_file(1:index1+4)//'wmo_id'//l1_5_file(index2+2:)
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'wmo_id', wmo_id, start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

!sat_ids(4)
!sba_slope(4,2,11),sba_offset(4,2,11)

!wmo_id:satellite_names = "270=GOES-16;271=GOES-17;173=Himawari-8;55=Meteosat-8;70=Meteosat-11" ;

!sat_ids(1)='270'
!sba_slope(1,1,:)=(/1.00166,0.948021,1.00000,0.926764,0.991329,0.999300,1.01977,1.03453,0.996781,1.02343,0.863748/)
!sba_slope(1,2,:)=(/1.00000,1.00000,1.00000,0.940848,0.990431,1.00483,1.01947,1.01835,0.996272,1.02239,0.890045/)
!sba_offset(1,1,:)=(/-0.00207069,0.161403,0.00000,18.4795,1.58729,-0.0312020,-4.93224,-7.83331,1.13395,-5.51940,30.5574/)
!sba_offset(1,2,:)=(/0.00000,0.00000,0.00000,14.4615,1.80946,-1.50406,-4.94939,-3.61878,1.23287,-5.28908,23.4361/)

!sat_ids(2)='271'
!sba_slope(2,1,:)=(/1.00313,0.947739,1.00000,0.928358,0.991436,0.999576,1.01939,1.03306,0.996656,1.02405,0.894260/)
!sba_slope(2,2,:)=(/1.00000,1.00000,1.00000,0.939285,0.990504,1.00481,1.01907,1.01759,0.996129,1.02298,0.917607/)
!sba_offset(2,1,:)=(/-0.0107727,0.161054,0.00000,17.7929,1.57229,-0.0877037,-4.83326,-7.49875,1.17318,-5.66102,23.6271/)
!sba_offset(2,2,:)=(/0.00000,0.00000,0.00000,14.7561,1.80263,-1.48189,-4.84116,-3.46801,1.27653,-5.42165,17.3323/)

!sat_ids(3)='173'
!sba_slope(3,1,:)=(/1.00169,0.945509,1.00000,0.929371,1.00905,1.00090,1.01065,1.02516,0.996015,1.02885,0.871186/)
!sba_slope(3,2,:)=(/1.00000,1.00000,1.00000,0.938706,1.00526,1.00413,1.01050,1.01338,0.995399,1.02742,0.896835/)
!sba_offset(3,1,:)=(/-0.00100716,0.162379,0.00000,17.3952,-2.21718,-0.347357,-2.62520,-5.68860,1.37321,-6.76384,28.8614/)
!sba_offset(3,2,:)=(/0.00000,0.00000,0.00000,14.8503,-1.29354,-1.20664,-2.63734,-2.61762,1.49858,-6.43452,21.9239/)

!sat_ids(4)='55'
!sba_slope(4,1,:)=(/0.999577,0.999643,1.00000,1.01192,1.00193,1.00093,1.00202,0.998663,0.999412,0.999663,0.945356/)
!sba_slope(4,2,:)=(/1.00000,1.00000,1.00000,1.00882,1.00146,1.00121,1.00194,0.999321,0.999513,0.999684,0.955400/)
!sba_offset(4,1,:)=(/0.00444712,0.0162967,0.00000,-2.83943,-0.417072,-0.181784,-0.486767,0.304194,0.134653,0.0706959,12.3763/)
!sba_offset(4,2,:)=(/0.00000,0.00000,0.00000,-1.94343,-0.302762,-0.259928,-0.471424,0.131567,0.104622,0.0658078,9.71430/)

!print*,minval(wmo_id),maxval(wmo_id)
print*,'shape: ',shape(imager_measurements%data)
!stop
!do i=1,11
!   where (wmo_id.eq.270) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(1,2,i)+sba_offset(1,2,i)
!   where (wmo_id.eq.271) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(2,2,i)+sba_offset(2,2,i)
!   where (wmo_id.eq.173) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(3,2,i)+sba_offset(3,2,i)
!   where (wmo_id.eq.55) imager_measurements%data(:,:,i)=imager_measurements%data(:,:,i)*sba_slope(4,2,i)+sba_offset(4,2,i)
!end do
   tmpfile=l1_5_file(1:index1+4)//'satellite_zenith_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'satellite_zenith_angle', imager_angles%satzen(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   tmpfile=l1_5_file(1:index1+4)//'satellite_azimuth_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'satellite_azimuth_angle', imager_angles%satazi(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   tmpfile=l1_5_file(1:index1+4)//'solar_zenith_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'solar_zenith_angle', imager_angles%solzen(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')

   tmpfile=l1_5_file(1:index1+4)//'solar_azimuth_angle'//l1_5_file(index2+2:)
   print*,tmpfile
   call ncdf_open(ncid, trim(tmpfile), 'ISCCPNG_Retrieve_Predef_Geo()')
   call ncdf_read_array(ncid, 'solar_azimuth_angle', imager_angles%solazi(startx:,:,1), start=[startx, starty])
   call ncdf_close(ncid, 'ISCCPNG_Retrieve_Predef_Geo()')


   !imager_angles%solazi(startx:,:,1)=imager_angles%solazi(startx:,:,1)-90.
   !where(imager_angles%solazi(startx:,:,1) .lt. -180.)
   !   imager_angles%solazi(startx:,:,1)=imager_angles%solazi(startx:,:,1)+360.
   !end where

   where (imager_angles%solazi(startx:,:,1) .gt. 180.)
      imager_angles%solazi(startx:,:,1) = imager_angles%solazi(startx:,:,1) - 360.
   end where

   where (imager_angles%satazi(startx:,:,1) .gt. 180.)
      imager_angles%satazi(startx:,:,1) = imager_angles%satazi(startx:,:,1) - 360.
   end where

   do i=1,nx
      imager_geolocation%latitude(startx+i-1,:)=tmplat(:)
   end do
   do i=1,ny
      imager_geolocation%longitude(startx:,i)=tmplon(:)
   end do

   deallocate(tmplat)
   deallocate(tmplon)
   deallocate(wmo_id)
   deallocate(rad_tmp)

   ! Remove underscores added by seviri_util (easy way of converting c-string to
   ! f-string).
   i = index(global_atts%Satpos_Metadata, '_')
   global_atts%Satpos_Metadata = global_atts%Satpos_Metadata(1:i-1)

   deallocate(band_ids)
   deallocate(band_units)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_isccpng_l1_5_nc()'

end subroutine read_isccpng_l1_5_nc

end module read_isccpng_m
