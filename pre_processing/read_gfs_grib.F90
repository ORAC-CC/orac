!-------------------------------------------------------------------------------
! Name: read_gfs_grib.F90
!
! Purpose:
! Read GFS data from a GRIB file, having interpolated it onto the
! preprocessing grid.
!
! Description and Algorithm details:
! The ECMWF EMOS library is used to perform the interpolation from the native
! grid of the input file to the desired preprocessor grid. That library can
! only produce regular or semi-regular grids and they must contain -180W and
! 0N. Also, the files report quantity values *at that point* rather than an
! area average as considered by the preprocessor grid.
!
! Hence, the definition of the preprocessor grid means we desire the ECMWF value
! at the centre of a grid cell but the interpolation software will only report
! at cell edges. To get around this, we request the ECMWF data at twice the
! desired resolution and then only use every other line of lat/lon. The final
! values are identical to what would have resulted from requesting the correct
! resolution.
!
! There is also a slight workaround involving the production of a scratch
! file. Though the EMOS library will output an unpacked array, that does not
! include the header information required to identify the field. By copying
! the interpolated field to a scratch file retains that.
!
! 1) Open file. Create scratch file.
! 2) Set output grid to limits and spacing of preprocessing grid.
! 3) Loop over fields in GRIB file.
!    a) Read data field.
!    b) Interpolate it with INTF.
!    c) Write new GRIB field to scratch file.
!    d) Read scratch file with GRIB API routines. Check for reduced Gauss grid.
!    e) Select correct output array from parameter #.
!    f) Write data into preproc structures.
! 4) Close files.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_file     string in   Full path to a GFS GRIB file to read.
! preproc_dims   struct out  Summary of preprocessing grid definitions
! preproc_geoloc struct out  Summary of lat/lon values
! preproc_prtm   struct out  Summary of profiles and surface fields
! verbose        logic  in   T: Print min/max of each field; F: Don't.
!
! History:
! 2017/02/04, SP: Initial version (ExtWork)
! 2017/02/25, SP: Update to RTTOV v12.1 (ExtWork)
! 2017/03/27, SP: New technique for computing profile levels. Improves retrievals
!                 over high altitude land regions (Tibet, f.ex) (ExtWork)
! 2017/03/30, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
! 2017/06/21, OS: line continuation symbol set to &
!
! Bugs:
! - If you're having problems with INTF, set the environment variable JDCNDBG=1
! for additional debugging output.
!-------------------------------------------------------------------------------

subroutine read_gfs_grib(ecmwf_file,preproc_dims,preproc_geoloc, &
     preproc_prtm,verbose)

   use grib_api
   use preproc_constants_m
   use preproc_structures_m

   implicit none

   character(len=*),       intent(in)    :: ecmwf_file
   type(preproc_dims_t),       intent(in)    :: preproc_dims
   type(preproc_geoloc_t),     intent(in)    :: preproc_geoloc
   type(preproc_prtm_t),       intent(inout) :: preproc_prtm
   logical,                    intent(in)    :: verbose

   integer(lint), parameter                 :: BUFFER = 3000000
   integer(lint), external                  :: INTIN,INTOUT,INTF2
   integer(lint)                            :: fu,stat,nbytes
   integer(lint)                            :: out_bytes, out_words
   integer(lint), allocatable, dimension(:) :: in_data,out_data
   integer(lint)                            :: iblank(4)
   real(dreal)                              :: grid(2),area(4)
   character(len=20)                        :: charv(1)
   character(len=100)                       :: ltype,lname

   integer(lint)                            :: gid,level,param
   integer                                  :: tlev,qlev,olev,glev

   integer(lint)                            :: n,ni,nj,i,j
   real(sreal), dimension(:),   allocatable :: val
   real(sreal), dimension(:,:), pointer     :: array

   integer(lint),dimension(31)              :: gfs_levlist


   gfs_levlist = (/1,2,3,5,7,10,20,30,50,70,100,150,200,250,300,350, &
                   400,450,500,550,600,650,700,750,800,850,900,925,950,975,1000/)

   ! Initialise level count, needed for GFS files
   tlev = 1
   qlev = 1
   olev = 1
   glev = 1

   ! Initialise some arrays, prevents issues with missing GFS values
   ! (only some levels)
   preproc_prtm%spec_hum(:,:,:) = 0.
   preproc_prtm%ozone(:,:,:) = 1e-10

   ! open the GFS file
   call PBOPEN(fu, ecmwf_file, 'r', stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to read file.')

   ! select appropriate grid definition
   charv(1)='grib'
   if (INTIN('form',iblank,grid,charv) .ne. 0) &
        call h_e_e('grib', 'INTIN form failed.')
   if (INTOUT('form',iblank,grid,charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT form failed.')

   ! input details of new grid (see note in header)
   grid(1) = 0.5 / preproc_dims%dellon
   grid(2) = 0.5 / preproc_dims%dellat
   if (INTOUT('grid',iblank,grid,charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT grid failed.')
   area(1) = preproc_geoloc%latitude(preproc_dims%max_lat) + 0.01*grid(2)
   area(2) = preproc_geoloc%longitude(preproc_dims%min_lon) + 0.01*grid(1)
   area(3) = preproc_geoloc%latitude(preproc_dims%min_lat) + 0.01*grid(2)
   area(4) = preproc_geoloc%longitude(preproc_dims%max_lon) + 0.01*grid(1)
   if (INTOUT('area',iblank,area,charv) .ne. 0) &
        call h_e_e('grib', 'INTOUT area failed.')

   allocate(in_data(BUFFER))
   allocate(out_data(BUFFER))

   ! interpolate ECMWF products to preproc grid
   do
      ! read GRIB data field
      call PBGRIB(fu, in_data, BUFFER*lint, nbytes, stat)
      if (stat .eq. -1) exit
      if (stat .ne. 0) call h_e_e('grib', 'Failure to read product.')

      ! Check if this is something we want to read
      call grib_new_from_message(gid,in_data,stat)
      call grib_get(gid,'parameter',param,stat)
      if (param .ne. 130 .and. param .ne. 157 .and. &
          param .ne. 156 .and. param .ne. 260131 .and. &
          param .ne. 134 .and. param .ne. 31 .and. &
          param .ne. 3066 .and. param .ne. 165 .and. &
          param .ne. 166 .and. param .ne. 167 .and. &
          param .ne. 172 .and. param .ne. 54) then
         call grib_release(gid,stat)
         cycle
      end if

      call grib_release(gid,stat)

      ! interpolate GRIB field (into another GRIB field)
      ! in_words = nbytes / lint
      ! out_words = BUFFER
      out_bytes = BUFFER * lint
      !if (INTF(in_data,in_words,zni,out_data,out_words,zno) .ne. 0) &
      !     call h_e_e('grib', &
      !       'INTF failed. Check if 1/dellon 1/dellat are muliples of 0.001.')
      if (INTF2(in_data,nbytes,out_data,out_bytes) .ne. 0) &
           call h_e_e('grib', 'INTF2 failed.')
      out_words = out_bytes/lint

!      print*,shape(out_data)
!      print*,out_words,out_bytes,lint
!      stop
      ! load grib data into grib_api
      call grib_new_from_message(gid,out_data(1:out_bytes),stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting GRIB_ID.')

      call grib_get(gid,'parameter',param,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting parameter #.')
      call grib_get(gid,'level',level,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
      call grib_get(gid,'Nj',nj,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting nj.')

      call grib_get(gid,'typeOfLevel',ltype,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level type.')
      call grib_get(gid,'name',lname,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting name.')

      ! regular grid
      call grib_get(gid,'Ni',ni,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error getting level #.')
      n=ni*nj

      if (.not.allocated(val)) allocate(val(n))

      call grib_get(gid,'values',val,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error reading data.')
      call grib_release(gid,stat)
      if (stat .ne. 0) call h_e_e('grib', 'Error releasing GRIB_ID.')

      ! select correct output array
      select case (param)
      case(130)
         ! Temperature
         if (any(level .eq. gfs_levlist) .and. &
             trim(ltype) .eq. 'isobaricInhPa') then
            array => preproc_prtm%temperature( &
                 preproc_dims%min_lon:preproc_dims%max_lon, &
                 preproc_dims%min_lat:preproc_dims%max_lat,tlev)
            preproc_prtm%pressure(:,:,tlev)=level
            tlev=tlev+1
         else
            cycle
         end if
      case(157)
         if (all(level .ne. gfs_levlist) .or. &
             trim(ltype) .ne. 'isobaricInhPa') cycle
         ! Relative humidity
         array => preproc_prtm%spec_hum( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat,qlev)
         qlev=qlev+1
      case(156)
         if (all(level .ne. gfs_levlist) .or. &
             trim(ltype) .ne. 'isobaricInhPa') cycle
         ! Geopotential
         array => preproc_prtm%phi_lev( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat,glev)
         glev=glev+1
      case(260131)
         ! Ozone
         if (all(level .ne. gfs_levlist) .or. &
             trim(ltype) .ne. 'isobaricInhPa') cycle
         array => preproc_prtm%ozone( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat,olev)
         olev=olev+1
      case(134)
         array => preproc_prtm%lnsp( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(31)
         array => preproc_prtm%sea_ice_cover( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(3066)
         array => preproc_prtm%snow_depth( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(165)
         array => preproc_prtm%u10( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(166)
         array => preproc_prtm%v10( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(167)
         array => preproc_prtm%temp2( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(172)
         array => preproc_prtm%land_sea_mask( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case(54)
         if (trim(ltype) .ne. 'tropopause') cycle
         array => preproc_prtm%trop_p( &
              preproc_dims%min_lon:preproc_dims%max_lon, &
              preproc_dims%min_lat:preproc_dims%max_lat)
      case default
         cycle
      end select

      ! copy data into preprocessing grid
      ! a) we're inverting the y axis as ECMWF write 90->-90
      ! b) we're only taking every other point to read cell centres
      ! c) there will be an odd # of lats as it contains 0N but an even # of
      !    lons as 180 wraps to -180
      do j = 1, nj, 2
         do i = 1, ni, 2
            array(1+i/2,1+(nj-j)/2) = val(i+(j-1)*ni)
         end do
      end do
      where (array .eq. 9999) array = sreal_fill_value
      if (verbose) print*,param,') Min: ',minval(array), &
              ', Max: ',maxval(array)
   end do

   ! GFS pressures are in Pa rather than hPa
   preproc_prtm%trop_p = preproc_prtm%trop_p * pa2hpa

   ! GFS has no skin temperature variable
   preproc_prtm%skin_temp = preproc_prtm%temperature(:,:,tlev-1)
   preproc_prtm%phi_lev   = preproc_prtm%phi_lev*g_wmo

   ! convert from pressure to log pressure
   preproc_prtm%lnsp = log(preproc_prtm%lnsp)

   deallocate(val)

   ! GFS provides humidity as relative humidity (%), we need specific humidity
   ! (kg/kg). This will convert from one to the other.
   call conv_rh_q(preproc_prtm%spec_hum, preproc_prtm%temperature, &
                  preproc_prtm%pressure, verbose)

   ! GFS has no snow mask, so use snow depth instead. 0.1m threshold arbitrary
   where(preproc_prtm%snow_depth .gt. 0.1) preproc_prtm%snow_albedo = 0.98
   deallocate(in_data)
   deallocate(out_data)

   ! close ECMWF file
   call PBCLOSE(fu,stat)
   if (stat .ne. 0) call h_e_e('grib', 'Failed to close file.')

   ! Refactor all the GFS levels so that below-surface contributions are removed.
   call sort_gfs_levels(preproc_prtm,verbose)

end subroutine read_gfs_grib


! This function transforms the GFS fixed pressure levels into surface-relative
! levels that are more similar to those from ECMWF. Needed to prevent below-
! surface contributions to the transmission and radiances.
subroutine sort_gfs_levels(preproc_prtm,verbose)

   use preproc_constants_m
   use preproc_structures_m

   implicit none

   type(preproc_prtm_t), intent(inout) :: preproc_prtm
   logical,              intent(in)    :: verbose

   integer          :: sh(3),lb(3),ub(3),i_0,i_1,j_0,j_1,nl
   integer          :: i,j,l,stoplev

   real(dreal)      :: surfp,interp
   real,allocatable :: p(:),t(:),q(:),o(:),pl(:)
   logical          :: stopper

   if (verbose)write(*,*)">>>>>>Sort_gfs_levels>>>>>>"

   ! Get the array bounds
   sh = shape(preproc_prtm%pressure)
   nl = sh(3)
   lb = lbound(preproc_prtm%pressure)
   ub = ubound(preproc_prtm%pressure)
   i_0 = lb(1)
   i_1 = ub(1)
   j_0 = lb(2)
   j_1 = ub(2)

   ! Allocate temporary arrays
   allocate(p(nl))
   allocate(t(nl))
   allocate(q(nl))
   allocate(o(nl))
   allocate(pl(nl))

   ! Loop over each element
   do i = i_0, i_1
      do j = j_0, j_1
         stopper = .false.
         stoplev = 0

         ! Get level arrays for all data, plus surf pres
         surfp = exp(preproc_prtm%lnsp(i,j))*pa2hpa
         p = preproc_prtm%pressure(i,j,:)
         t = preproc_prtm%temperature(i,j,:)
         q = preproc_prtm%spec_hum(i,j,:)
         o = preproc_prtm%ozone(i,j,:)
         pl = preproc_prtm%phi_lev(i,j,:)

         ! Loop over all levels to find last level above surface
         do l = 2, nl
            if (p(l) .gt. surfp) then
               if (.not. stopper) then
                  stopper = .true.
                  ! Compute interp factor to ensure last lev = surface
                  interp = (surfp-p(l-1))/(p(l)-p(l-1))
                  stoplev = l-1
               end if
            end if
         end do

         ! If final level of GFS data is above surface then do this
         ! Skips otherwise (e.g: Sea or high pressure low elevation regions)
         if (stopper) then

            ! Set data for final level equal to interpolated surface data
            p(nl) = p(stoplev) + interp * (p(stoplev+1)-p(stoplev))
            t(nl) = t(stoplev) + interp * (t(stoplev+1)-t(stoplev))
            q(nl) = q(stoplev) + interp * (q(stoplev+1)-q(stoplev))
            o(nl) = o(stoplev) + interp * (o(stoplev+1)-o(stoplev))
            pl(nl) = pl(stoplev) + interp * (pl(stoplev+1)-pl(stoplev))

            ! Move all other data down, so profile is by surface
            p(nl-stoplev:nl) = p(1:stoplev)
            t(nl-stoplev:nl) = t(1:stoplev)
            q(nl-stoplev:nl) = q(1:stoplev)
            o(nl-stoplev:nl) = o(1:stoplev)
            pl(nl-stoplev:nl) = pl(1:stoplev)

            ! Fill upper levels with repeating data from final valid layer
            do l = nl-stoplev-1, 1, -1
               p(l)=p(l+1)-(0.01*p(l+1))
               t(l)=t(l+1)-(0.01*t(l+1))
               q(l)=q(l+1)-(0.01*q(l+1))
               o(l)=o(l+1)-(0.01*o(l+1))
               pl(l)=pl(l+1)-(0.01*pl(l+1))
            end do

            ! Send everything back into appropriate prtm grid array
            preproc_prtm%pressure(i,j,:)=p
            preproc_prtm%temperature(i,j,:)=t
            preproc_prtm%spec_hum(i,j,:)=q
            preproc_prtm%ozone(i,j,:)=o
            preproc_prtm%phi_lev(i,j,:)=pl
         end if
      end do
   end do

   ! Tidy up
   deallocate(p)
   deallocate(t)
   deallocate(q)
   deallocate(o)
   deallocate(pl)

   if (verbose) write(*,*)"<<<<<<Sort_gfs_levels<<<<<<"

end subroutine sort_gfs_levels


subroutine conv_rh_q(rh,t,p,verbose)

   use preproc_constants_m
   use preproc_structures_m

   implicit none

   real(sreal), dimension(:,:,:), intent(inout) :: rh
   real(sreal), dimension(:,:,:), intent(in)    :: t
   real(sreal), dimension(:,:,:), intent(in)    :: p
   logical,                       intent(in)    :: verbose

   real(sreal), allocatable, dimension(:,:,:) :: c1,c2,c3,c4,c5,c6,pc,tc,th, &
                                                 val, pws,pw,ppm
   integer :: sh(3)

   sh=shape(t)

   allocate(c1(sh(1),sh(2),sh(3)))
   allocate(c2(sh(1),sh(2),sh(3)))
   allocate(c3(sh(1),sh(2),sh(3)))
   allocate(c4(sh(1),sh(2),sh(3)))
   allocate(c5(sh(1),sh(2),sh(3)))
   allocate(c6(sh(1),sh(2),sh(3)))
   allocate(pc(sh(1),sh(2),sh(3)))
   allocate(tc(sh(1),sh(2),sh(3)))
   allocate(th(sh(1),sh(2),sh(3)))
   allocate(val(sh(1),sh(2),sh(3)))
   allocate(pws(sh(1),sh(2),sh(3)))
   allocate(pw(sh(1),sh(2),sh(3)))
   allocate(ppm(sh(1),sh(2),sh(3)))

   c1  = -7.85951783
   c2  = 1.84408259
   c3  = -11.7866497
   c4  = 22.6807411
   c5  = -15.9618719
   c6  = 1.80122502
   pc  = 220640
   tc  = 647.096

   th  = 1.   -   (t/tc)
   val = (c1*th) + (c2*(th**1.5)) + (c3*(th**3.0)) + (c4*(th**3.5)) + &
         (c5*(th**4.0)) + (c6*(th**7.5))
   val = val * (tc/t)
   pws = pc * exp(val)
   pw  = pws * (rh/100.)
   rh  = (pw / p * 1e6)/q_mixratio_to_ppmv

   deallocate(c1)
   deallocate(c2)
   deallocate(c3)
   deallocate(c4)
   deallocate(c5)
   deallocate(c6)
   deallocate(pc)
   deallocate(tc)
   deallocate(th)
   deallocate(val)
   deallocate(pws)
   deallocate(pw)
   deallocate(ppm)

end subroutine conv_rh_q
