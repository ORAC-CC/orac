! Name: read_ecmwf_nc.f90
!
!
! Purpose:
! Reads NetCDF format ECMWF ERA interim data
! 
! Description and Algorithm details:
! 1) Read list of available variables in this file.
! 2) Read any desired variables into temporary arrays.
! 3) Copy temporary arrays into preproc_prtm. If not using the ECMWF grid,
!    use nearest neighbour interpolation.
! 4) Compute the geopotential coordinates from humidity, temperature, and
!    surface pressure.
!
! Arguments:
! Name            Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path     string  In   NetCDF ECMWF file to be opened.
! ecmwf_dims     struct  In   Dimensions of that file.
! ecmwf_2d       struct  Both Flat ECMWF fields.
! preproc_dims   struct  In   Dimensions of the preprocessing grid.
! preproc_geoloc struct  In   Lat/lon of the preprocessing grid.
! preproc_prtm   struct  Both Pressure-level information for RTTOV.
! grid_flag      integer In   1:ECMWF grid, 2:L3 grid, 3: own definition
!
! History:
! 2012/08/06:    Initial version ecmwf code
! 2012/08/06: CP modified to write data into preprocessing structures
! 2012/08/07: CP added in reading of surface data pressure level data, added in
!                ozone profile and geopotential height,
! 2012/08/13: CP totally rewrote program to cope with multiple netcdf file read
! 2012/11/13: CP added in surface pressure and pressure
! 2012/11/29: CP added ecmwf_2d definitions for u10 and v10
! 2013/01/29: CP changed how geopotetntial was read out
! 2013/03/05: CP small change to work in gfortran
! 2013/03/06: CP tidy up and rearrange badc files
! 2013/03/07: CP tidied up allocations and changed code to read in q and 03 form
!                a netcdf file because grib code did not work for badc style 
!                grb files also added computation of geopot because was 
!                previously dome in grib read
! 2013/03/18: GT Altered the allocation of temporary arrays to hold the various
!                ECMWF variable to avoid the compiler complaining of possible 
!                use of unallocated arrays.
! 2013/03/19: GT Fixed the reading of the gpam file (containing specific
!                humidity and O3 data)
!                Moved the rearranging of the ECMWF arrays into the same if
!                statements as the reading commands, and changed the generation
!                of the pressure profile array so that it is created on the
!                rearranged grid. Removed quite a few debugging print statements
! 2013/03/20: GT Fixed a bug introduced in yesterday's changes (10 m wind
!                components were not being writen to ECMWF structures)
! 2013/10/29:    Changed array allocation of phi_lay and phi_lev
! 2014/02/10: AP Extreme tidying. Made all allocatable arrays definite size.
!                Removed check of file dimensions. Made a,bvector global.
!                Added nearest neighbour functionality. Made geopotential
!                calculation external. Removed surfaceflag.
!
! $Id$
!
! Bugs:
! - you need to be careful with parameter naming as the variable names are not
!   consistent across files for example the variable name could be lnsp or LNSP
!

subroutine read_ecmwf_nc(ecmwf_path,ecmwf_dims,ecmwf_2d,preproc_dims, &
     preproc_geoloc,preproc_prtm,grid_flag)

   implicit none

   ! arguments
   character(len=pathlength), intent(in)    :: ecmwf_path
   type(ecmwf_dims_s),        intent(in)    :: ecmwf_dims
   type(ecmwf_2d_s),          intent(inout) :: ecmwf_2d
   type(preproc_dims_s),      intent(in)    :: preproc_dims
   type(preproc_geoloc_s),    intent(in)    :: preproc_geoloc
   type(preproc_prtm_s),      intent(inout) :: preproc_prtm
   integer(kind=sint),        intent(in)    :: grid_flag

   ! NCDF variables
   integer                   :: ncid,ierr,verb
   integer(kind=lint)        :: ndim,nvar,nattr,ivar
   character(len=varlength), allocatable, dimension(:) :: available_names

   ! loop variables
   integer                   :: lat1,lat2,lon1,lon2
   integer                   :: jnear,inear,jdiff,idiff
   integer(kind=lint)        :: ik,jdim,idim

   ! flags
   logical                   :: geopot_flag,spec_hum_flag,temperature_flag
   logical                   :: ozone_flag,lnsp_flag,sea_ice_cover_flag
   logical                   :: snow_albedo_flag,totcolwv_flag,snow_depth_flag
   logical                   :: u10_flag,v10_flag,temp2_flag,skin_temp_flag
   logical                   :: sst_flag,surface_p_flag,land_sea_mask_flag
   logical                   :: pressure_flag
   
   ! read temporaries (last dimension = time = 1 as using one-step files)
   real(kind=sreal), dimension(ecmwf_dims%xdim,ecmwf_dims%ydim,1,1) &
                             :: dummy2d
   real(kind=sreal), dimension(ecmwf_dims%xdim,ecmwf_dims%ydim, &
        ecmwf_dims%kdim,1)   :: dummy

   ! transfer arrays
   real(kind=sreal), dimension(ecmwf_dims%xdim,ecmwf_dims%ydim, &
        ecmwf_dims%kdim)     :: pressure,temperature
   real(kind=sreal), dimension(ecmwf_dims%xdim,ecmwf_dims%ydim, &
        ecmwf_dims%kdim)     :: spec_hum,ozone
   real(kind=sreal), dimension(ecmwf_dims%xdim,ecmwf_dims%ydim) &
                             :: sst,lnsp,sea_ice_cover,temp2,totcolwv,u10,v10
   real(kind=sreal), dimension(ecmwf_dims%xdim,ecmwf_dims%ydim) &
                             :: snow_albedo,land_sea_mask,surface_p
   real(kind=sreal), dimension(ecmwf_dims%xdim,ecmwf_dims%ydim) &
                             :: geopot,snow_depth,skin_temp

   ! inputs for compute_geopot_coordinate
   real(kind=sreal)                               :: sp
   real(kind=sreal), dimension(ecmwf_dims%kdim+1) :: phi_lev
   real(kind=sreal), dimension(ecmwf_dims%kdim)   :: phi_lay,spec_hum_g
   real(kind=sreal), dimension(ecmwf_dims%kdim)   :: temperature_g

   ! predefined constants
   real(sreal), dimension(37) :: dummy1d=[ &
             1.,2.,3.,5.,7.,10.,20.,30.,50.,70.,100.,125.,150.,175.,200., &
           225.,250.,300.,350.,400.,450.,500.,550.,600.,650.,700.,750., &
           775.,800.,825.,850.,875.,800.,925.,950.,975.,1000.]


   geopot_flag=.false.
   spec_hum_flag=.false.
   temperature_flag=.false.
   ozone_flag=.false.
   lnsp_flag=.false.
   sea_ice_cover_flag=.false. 
   snow_albedo_flag=.false.
   totcolwv_flag=.false.
   snow_depth_flag=.false.
   u10_flag=.false.
   v10_flag=.false.
   temp2_flag=.false.
   skin_temp_flag=.false.
   sst_flag=.false.
   surface_p_flag=.false.
   land_sea_mask_flag=.false.
   pressure_flag=.false.
   verb=0 ! quiet mode

   ! shorten variable names
   lat1=preproc_dims%min_lat
   lat2=preproc_dims%max_lat
   lon1=preproc_dims%min_lon
   lon2=preproc_dims%max_lon
   
   call nc_open(ncid,ecmwf_path)
   ierr=NF90_INQUIRE(ncid,ndim,nvar,nattr)

   ! Extract all the variable names from the NetCDF file  
   allocate(available_names(nvar))
   available_names=''
   do ivar=1,nvar
      ierr=NF90_INQUIRE_VARIABLE(ncid, ivar, available_names(ivar))
   enddo

   !surface geopotential height
   if (any(available_names .eq. 'Z')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'Z',dummy2d,verb)
      geopot=dummy2d(:,:,1,1)
      call rearrange_ecmwf(geopot,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      geopot_flag=.true.
   endif

   !spec. humidity
   if (any(available_names .eq. 'Q')) then
      dummy=real_fill_value
      call nc_read_array(ncid,'Q',dummy,verb)
      spec_hum=dummy(:,:,:,1)
      call rearrange_ecmwf(spec_hum,ecmwf_dims,ecmwf_dims%xdim, &
           ecmwf_dims%ydim,ecmwf_dims%kdim)
      spec_hum_flag=.true.
   endif

   !temperature
   if (any(available_names .eq. 'T')) then
      dummy=real_fill_value
      call nc_read_array(ncid,'T',dummy,verb)
      temperature=dummy(:,:,:,1)
      call rearrange_ecmwf(temperature,ecmwf_dims,ecmwf_dims%xdim, &
           ecmwf_dims%ydim,ecmwf_dims%kdim)
      temperature_flag=.true.
   endif

   !ozone
   if (any(available_names .eq. 'O3')) then
      dummy=real_fill_value
      call nc_read_array(ncid,'O3',dummy,verb)
      ozone=dummy(:,:,:,1)
      call rearrange_ecmwf(ozone,ecmwf_dims,ecmwf_dims%xdim, &
           ecmwf_dims%ydim,ecmwf_dims%kdim)
      ozone_flag=.true.
   endif

   !Logarithm of surface pressure
   if (any(available_names .eq. 'LNSP')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'LNSP',dummy2d,verb)
      lnsp=dummy2d(:,:,1,1)
      call rearrange_ecmwf(lnsp,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      lnsp_flag=.true.
   endif

   !Sea-ice cover
   if (any(available_names .eq. 'CI')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'CI',dummy2d,verb)
      sea_ice_cover=dummy2d(:,:,1,1)
      call rearrange_ecmwf(sea_ice_cover,ecmwf_dims,ecmwf_dims%xdim, &
           ecmwf_dims%ydim)
      sea_ice_cover_flag=.true. 
    endif

   !snow_albedo
   if (any(available_names .eq. 'ASN')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'ASN',dummy2d,verb)
      snow_albedo=dummy2d(:,:,1,1)
      call rearrange_ecmwf(snow_albedo,ecmwf_dims,ecmwf_dims%xdim, &
           ecmwf_dims%ydim)
      snow_albedo_flag=.true.
   endif

   !totcolwv
   if (any(available_names .eq. 'TCWV')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'TCWV',dummy2d,verb)
      totcolwv=dummy2d(:,:,1,1)
      call rearrange_ecmwf(totcolwv,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      totcolwv_flag=.true.
   endif

   !snow_depth
   if (any(available_names .eq. 'SD')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'SD',dummy2d,verb) 
      snow_depth=dummy2d(:,:,1,1)
      call rearrange_ecmwf(snow_depth,ecmwf_dims,ecmwf_dims%xdim, &
           ecmwf_dims%ydim)
      snow_depth_flag=.true.
   endif

   !u10
   if (any(available_names .eq. 'U10')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'U10',dummy2d,verb)
      u10=dummy2d(:,:,1,1)
      ! 10 m wind components are needed for sea surface reflectance calculations
      ! so copy them into the ecmwf structures
      ecmwf_2d%u10=u10
      call rearrange_ecmwf(u10,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      u10_flag=.true.
   endif

   !v10
   if (any(available_names .eq. 'V10')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'V10',dummy2d,verb)
      v10=dummy2d(:,:,1,1)
      ! 10 m wind components are needed for sea surface reflectance calculations
      ! so copy them into the ecmwf structures
      ecmwf_2d%v10=v10
      call rearrange_ecmwf(v10,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      v10_flag=.true.
   endif

   !temp2
   if (any(available_names .eq. 'T2')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'T2',dummy2d,verb)
      temp2(:,:)=dummy2d(:,:,1,1)
      call rearrange_ecmwf(temp2,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      temp2_flag=.true.
   endif

   !skin_temp
   if (any(available_names .eq. 'SKT')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'SKT',dummy2d,verb)
      skin_temp(:,:)=dummy2d(:,:,1,1)
      call rearrange_ecmwf(skin_temp,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      skin_temp_flag=.true.
   endif

   !sst
   if (any(available_names .eq. 'SSTK')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'SSTK',dummy2d,verb)
      sst(:,:)=dummy2d(:,:,1,1)
      call rearrange_ecmwf(sst,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      sst_flag=.true.
   endif

   !surface_pressure
   if (any(available_names .eq. 'SP')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'SP',dummy2d,verb)
      surface_p(:,:)=dummy2d(:,:,1,1)
      call rearrange_ecmwf(surface_p,ecmwf_dims,ecmwf_dims%xdim,ecmwf_dims%ydim)
      surface_p_flag=.true.
   endif

   !Land-sea mask (actually albedo)
   if (any(available_names .eq. 'AL')) then
      dummy2d=real_fill_value
      call nc_read_array(ncid,'AL',dummy2d,verb)
      land_sea_mask(:,:)=dummy2d(:,:,1,1)
      call rearrange_ecmwf(land_sea_mask,ecmwf_dims, ecmwf_dims%xdim, &
           ecmwf_dims%ydim)
      land_sea_mask_flag=.true.
   endif

   !pressure 
   ! NB for level data pressures are the same
   ! NB#2 We generate pressure on the reordered (preproc) grid, rather than the
   !      ECMWF grid, so x and y dimensions are swapped.
   if (ecmwf_dims%kdim.eq.37 .and. .not.pressure_flag) then
      dummy1d=dummy1d*100.
      do idim=1,ecmwf_dims%xdim
         do jdim=1,ecmwf_dims%ydim
            pressure(jdim,idim,:)=dummy1d
         end do
      end do
      pressure_flag=.true.
   endif

   !now write the data to preprocessing structure
   if (grid_flag .eq. 1) then
      ! preproc on ECMWF grid, so can copy data as is
      if (temperature_flag) preproc_prtm%temperature(lon1:lon2,lat1:lat2, &
           1:ecmwf_dims%kdim) = temperature(lon1:lon2,lat1:lat2,:)
      if (spec_hum_flag) preproc_prtm%spec_hum(lon1:lon2,lat1:lat2, &
           1:ecmwf_dims%kdim) = spec_hum(lon1:lon2,lat1:lat2,:)
      if (ozone_flag) preproc_prtm%ozone(lon1:lon2,lat1:lat2, &
           1:ecmwf_dims%kdim) = ozone(lon1:lon2,lat1:lat2,:)
      if (pressure_flag) preproc_prtm%pressure(lon1:lon2,lat1:lat2, &
           1:ecmwf_dims%kdim) = pressure(lon1:lon2,lat1:lat2,:)
      if (geopot_flag) preproc_prtm%geopot(lon1:lon2,lat1:lat2) = &
           geopot(lon1:lon2,lat1:lat2)
      if (sea_ice_cover_flag) preproc_prtm%sea_ice_cover(lon1:lon2,lat1:lat2)= &
           sea_ice_cover(lon1:lon2,lat1:lat2)
      if (snow_albedo_flag) preproc_prtm%snow_albedo(lon1:lon2,lat1:lat2) = &
           snow_albedo(lon1:lon2,lat1:lat2)
      if (snow_depth_flag) preproc_prtm%snow_depth(lon1:lon2,lat1:lat2) = &
           snow_depth(lon1:lon2,lat1:lat2)
      if (sst_flag) preproc_prtm%sst(lon1:lon2,lat1:lat2) = &
           sst(lon1:lon2,lat1:lat2)
      if (totcolwv_flag) preproc_prtm%totcolwv(lon1:lon2,lat1:lat2) = &
           totcolwv(lon1:lon2,lat1:lat2)
      if (u10_flag) preproc_prtm%u10(lon1:lon2,lat1:lat2) = &
           u10(lon1:lon2,lat1:lat2)
      if (v10_flag) preproc_prtm%v10(lon1:lon2,lat1:lat2) = &
           v10(lon1:lon2,lat1:lat2)
      if (temp2_flag) preproc_prtm%temp2(lon1:lon2,lat1:lat2) = &
           temp2(lon1:lon2,lat1:lat2)
      if (land_sea_mask_flag) preproc_prtm%land_sea_mask(lon1:lon2,lat1:lat2)= &
           land_sea_mask(lon1:lon2,lat1:lat2)
      if (skin_temp_flag) preproc_prtm%skin_temp(lon1:lon2,lat1:lat2) = &
           skin_temp(lon1:lon2,lat1:lat2)
      if (surface_p_flag) preproc_prtm%surface_pressure(lon1:lon2,lat1:lat2) = &
           surface_p(lon1:lon2,lat1:lat2)
      if (lnsp_flag) preproc_prtm%lnsp(lon1:lon2,lat1:lat2) = &
           lnsp(lon1:lon2,lat1:lat2)
   else
      ! Find nearest neighbour ECMWF to preproc grid point:
      ! a) As both grids are regular, we can minimise the distance to each
      !    dimension seperately.
      ! b) As monotonic grid, can search for each neighbour from the last
      ! c) Here, find initial nearest neighbour
      jnear=minval(abs(preproc_geoloc%latitude(lat1)-ecmwf_2d%latitude(1,:)))
      inear=minval(abs(preproc_geoloc%longitude(lon1)-ecmwf_2d%longitude(:,1)))
      jdiff=abs(preproc_geoloc%latitude(lat1)-ecmwf_2d%latitude(1,inear))
      idiff=abs(preproc_geoloc%longitude(lon1)-ecmwf_2d%longitude(jnear,1))
      do jdim=lat1,lat2
         ! d) Iterate to nearest neighbour of next x preproc grid point
         do while (abs(preproc_geoloc%latitude(jdim)- &
              ecmwf_2d%latitude(1,jnear)).lt.jdiff)
            jnear=jnear+1
         end do
         jdiff=abs(preproc_geoloc%latitude(jdim)-ecmwf_2d%latitude(1,jnear))
         
         do idim=lon1,lon2
            ! e) Iterate to nearest neighbour of next y preproc grid point
            do while (abs(preproc_geoloc%longitude(idim)- &
                 ecmwf_2d%longitude(inear,1)).lt.idiff)
               jnear=inear+1
            end do
            idiff=abs(preproc_geoloc%longitude(idim)-ecmwf_2d%longitude(inear,1))

            if (temperature_flag) preproc_prtm%temperature(idim,jdim,&
                 1:ecmwf_dims%kdim) = temperature(idiff,jdiff,:)
            if (spec_hum_flag) preproc_prtm%spec_hum(idim,jdim,&
                 1:ecmwf_dims%kdim) = spec_hum(idiff,jdiff,:)
            if (ozone_flag) preproc_prtm%ozone(idim,jdim,1:ecmwf_dims%kdim) = &
                 ozone(idiff,jdiff,:)
            if (pressure_flag) preproc_prtm%pressure(idim,jdim,&
                 1:ecmwf_dims%kdim) = pressure(idiff,jdiff,:)
            if (geopot_flag) preproc_prtm%geopot(idim,jdim) = &
                 geopot(idiff,jdiff)
            if (sea_ice_cover_flag) preproc_prtm%sea_ice_cover(idim,jdim) = &
                 sea_ice_cover(idiff,jdiff)
            if (snow_albedo_flag) preproc_prtm%snow_albedo(idim,jdim) = &
                 snow_albedo(idiff,jdiff)
            if (snow_depth_flag) preproc_prtm%snow_depth(idim,jdim) = &
                 snow_depth(idiff,jdiff)
            if (sst_flag) preproc_prtm%sst(idim,jdim) = sst(idiff,jdiff)
            if (totcolwv_flag) preproc_prtm%totcolwv(idim,jdim) = &
                 totcolwv(idiff,jdiff)
            if (u10_flag) preproc_prtm%u10(idim,jdim) = u10(idiff,jdiff)
            if (v10_flag) preproc_prtm%v10(idim,jdim) = v10(idiff,jdiff)
            if (temp2_flag) preproc_prtm%temp2(idim,jdim) = &
                 temp2(idiff,jdiff)
            if (land_sea_mask_flag) preproc_prtm%land_sea_mask(idim,jdim) = &
                 land_sea_mask(idiff,jdiff)
            if (skin_temp_flag) preproc_prtm%skin_temp(idim,jdim) = &
                 skin_temp(idiff,jdiff)
            if (surface_p_flag) preproc_prtm%surface_pressure(idim,jdim) = &
                 surface_p(idiff,jdiff)
            if (lnsp_flag) preproc_prtm%lnsp(idim,jdim) = lnsp(idiff,jdiff)
         enddo
      enddo
   end if
   
   ierr=NF90_CLOSE(ncid)

end subroutine read_ecmwf_nc
  
  
