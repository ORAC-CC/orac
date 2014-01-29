! Name: read_ecmwf_nc.f90
!
!
! Purpose:
! Code readin netcdf format ecmwf era interim data
! 
!
! Description and Algorithm details:
!y
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/08/06: Initial version ecmwf code
!2012/08/06: CP modified to write data into preprocessing structures
!2012/08/07: CP added in reading of surface data pressure level data, added in ozone profile and geopotential height,
!2012/08/13: CP totally rewrote program to cope with multiple netcdf file read
!2012/11/13: CP added in surface pressure and pressure
!2012/11/29: CP added ecmwf_2d definitions for u10 and v10
!2013/01/29: CP changed how geopotetntial was read out
!2013/03/05: CP small change to work in gfortran
!2013/03/06: CP tidy up and rearrange badc files
!2013/03/07: CP tidied up allocations and changed code to read in q and 03 form
!                a netcdf file because grib code did not work for badc style grb files 
!                also adde computation of geopot because was previously dome in grib read
! 2013/03/18 Gareth Thomas Altered the allocation of temporary arrays to hold the various
!                ECMWF variable to avoid the compiler complaining of possible use of
!                unallocated arrays.
! 2013/03/19 GT Fixed the reading of the gpam file (containing specific humidity
!                and O3 data)
!               Moved the rearranging of the ECMWF arrays into the same if statements
!                as the reading commands, and changed the generation of the pressure
!                profile array so that it is created on the rearranged grid.
!               Removed quite a few debugging print statements
! 2013/03/20 GT Fixed a bug introduced in yesterday's changes (10 m wind components were
!               not being writen to ECMWF structures)
! 2013/10/29 Changed array allocation of phi_lay and phi_lev
!
! $Id$
!
! Bugs:
!current option for nearest neighbout grid is not implemented
! ecmwf grid is the same as preprocessing grid.
! you need to be careful with parameter naming as the variable names are not con!sistent across files for example the variable name could be lnsp or LNSP
!there is a memory bug when deallocating ozone and spec_hum
!none known
!
!---------------------------------------------------
!---------------------------------------------------
subroutine read_ecmwf_nc(ecmwf_path,ecmwf_dims,ecmwf_3d,ecmwf_2d,preproc_dims,preproc_geoloc,preproc_prtm,surfaceflag)
!---------------------------------------------------
!---------------------------------------------------

  use netcdf

  use preproc_constants

  use preproc_structures

  use ecmwf_structures

  implicit none

  character(len=pathlength) :: ecmwf_path

  type(ecmwf_dims_s) :: ecmwf_dims
  type(ecmwf_3d_s) :: ecmwf_3d
  type(ecmwf_2d_s) :: ecmwf_2d

  type(preproc_geoloc_s) :: preproc_geoloc
  type(preproc_dims_s) :: preproc_dims
  type(preproc_prtm_s) :: preproc_prtm

  integer(kind=lint) :: ncid,ierr,wo
  real(kind=dreal), allocatable, dimension(:) :: shiftlat,shiftlon
  integer(kind=lint) :: ivar,ndim,nvar,nattr,dummyint

  integer, allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)

  character(len=varlength), allocatable :: dname(:)

  character (len=varlength), allocatable, dimension(:) ::  available_names(:)

  character(len=varlength) :: name

  integer (kind=lint) :: xdim,ydim,levlistdim,timedim,k,jdim,idim ! ,levtypedim
  integer (kind=stint) :: surfaceflag
     
  logical :: lnn

  !near neighbor
  !  integer(kind=kindOfInt), allocatable, dimension(:,:) :: nnindex
  
  !interpolation
  !  integer(kind=kindOfInt), allocatable, dimension(:,:,:) :: intindexes
  
  real(kind=sreal),allocatable,dimension(:) :: levelist,levtype,time
  
  real(kind=sreal),allocatable,dimension(:,:,:,:) :: dummy
  real(kind=sreal) ,allocatable,dimension(:) :: dummy1d
  real(kind=sreal),allocatable,dimension(:,:,:,:) :: dummy2d
  
  real(kind=sreal),allocatable,dimension(:,:,:) ::  temperature,spec_hum,ozone,pressure

  real(kind=sreal),allocatable,dimension(:,:) ::   sst,lnsp,sea_ice_cover, &
       snow_albedo,geopot,&
       & totcolwv,snow_depth,u10,v10,temp2,land_sea_mask, &
       skin_temp,surface_pressure,msl
  integer ::   sst_flag,lnsp_flag,sea_ice_cover_flag,snow_albedo_flag, &
       totcolwv_flag,snow_depth_flag,u10_flag,v10_flag,temp2_flag, &
       land_sea_mask_flag, msl_flag,&
       skin_temp_flag,temperature_flag,spec_hum_flag,ozone_flag, &
       geopot_flag,surface_pressure_flag,pressure_flag,ii,jj,dim1,dim2,dim3,ik

 real(kind=sreal), allocatable, dimension(:) :: avector,bvector, &
      &phi_lev,phi_lay,spec_hum_g,temperature_g
  real(kind=sreal) :: sp,geopot_g


  sst_flag=0
  lnsp_flag=0
  msl_flag=0
  sea_ice_cover_flag=0
  snow_albedo_flag=0  
  totcolwv_flag=0
  snow_depth_flag=0
  u10_flag=0
  v10_flag=0
  temp2_flag=0
  land_sea_mask_flag=0  
  skin_temp_flag=0
  surface_pressure_flag=0
  pressure_flag=0
  temperature_flag=0
  spec_hum_flag=0
  ozone_flag=0
  geopot_flag=0
  levlistdim=0

  
  wo=0 ! Produce optional debugging output (from nc_read_file.F90 subroutines)
  lnn=.true. !do nearest neighbor

  call nc_open(ncid,ecmwf_path,ierr,wo)

  call nc_info(ncid,ndim,nvar,nattr,wo)

  allocate(dimids(ndim))
  dimids=0
  allocate(dname(ndim))
  dname=''
  allocate(dimlength(ndim))
  dimlength=0
  allocate(varids(nvar))
  varids=0
  allocate(attrids(nattr))
  attrids=0

  if (surfaceflag .eq. 1) then
     ndim=2
  endif


  do idim=1,ndim

     if(idim .eq. 1 ) name='longitude'
     if(idim .eq. 2 ) name='latitude'
     !     if(idim .eq. 3 ) name='levelist'
     !     if(idim .eq. 4 ) name='levtype'
     !     if(idim .eq. 5 ) name='time'
     ! if surface data (gpas) do not need this
     ! Note that the levels dimension has a different name in the
     ! gpam and gpas files.
     if((ndim .eq. 4) .and. (idim .eq. 3)) name='hybrid'   ! gpam
     if((ndim .eq. 5) .and. (idim .eq. 3)) name='hybrid_1' ! gpas
     if(idim .eq. 4 ) name='t'

     call nc_dim_id(ncid,name,dimids(idim),wo)
     
     call nc_dim_length(ncid,dname(idim),dimids(idim),dummyint,wo)
     dimlength(idim)=dummyint

     if(idim .eq. 1 ) xdim=int(dimlength(1),kind=lint)
     if(idim .eq. 2 ) ydim=int(dimlength(2),kind=lint)
     if(idim .eq. 3 ) then
        levlistdim=int(dimlength(3),kind=lint)
        allocate(dummy1d(levlistdim))
        dummy1d=real_fill_value
     endif
     !     if(idim .eq. 3 )
     !     if(idim .eq. 4 ) levtypedim=int(dimlength(4),kind=lint)
     if(idim .eq. 4 ) timedim=int(dimlength(4),kind=lint)

  enddo

  ! allocate temporary fields to hold the preproc grid in the succession and
  ! ordering of the ecmwf grid
  
  !
  ! these values found from read_ecmwf_dimensions
  !
  allocate(shiftlon(preproc_dims%xdim_pre))
  shiftlon=double_fill_value
  allocate(shiftlat(preproc_dims%ydim_pre))
  shiftlat=double_fill_value
  
  do jdim=preproc_dims%preproc_min_lat,preproc_dims%preproc_max_lat
     do idim=preproc_dims%preproc_min_lon,preproc_dims%preproc_max_lon
        
        if(preproc_geoloc%longitude(idim) .lt. 0.00) then
           shiftlon(idim)=dble(preproc_geoloc%longitude(idim)+360.00)
        else
           shiftlon(idim)=dble(preproc_geoloc%longitude(idim))
        endif
        shiftlat(jdim)=dble(preproc_geoloc%latitude(jdim))

     enddo
  enddo

  ! Extract all the variable names from the NetCDF file  
  allocate(available_names(nvar))
  available_names=''
  
  do ivar=1,nvar
     ierr=nf90_inquire_variable(ncid, ivar, available_names(ivar))
!     write(*,*)'ecmwf var: ',available_names(ivar)
  enddo

  ! Scratch array for holding the output of nc_read_array_short_4d
  timedim=1

  dim1=ecmwf_dims%xdim_ec
  dim2=ecmwf_dims%ydim_ec
  dim3=ecmwf_dims%kdim_ec+1



  allocate(dummy2d(1,1,ydim,xdim))

  !
  !surface geopotential height
  !
  if (any(available_names .eq. 'Z')) then
     dummy2d=real_fill_value
     allocate(geopot(xdim,ydim))
     geopot=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'Z',dummy2d,wo)
     geopot(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(geopot,ecmwf_dims, dim1,dim2)
     geopot_flag=1
  else
     allocate(geopot(1,1))
  endif

  !
  !spec. humidity
  !
  if (any(available_names .eq. 'Q')) then
     allocate(spec_hum(xdim,ydim,levlistdim))
     spec_hum=real_fill_value
     allocate(dummy(timedim,levlistdim,ydim,xdim))
     dummy=real_fill_value
     call nc_read_array_short_4d(ncid,timedim,levlistdim,ydim,xdim,'Q',dummy,wo)
     do k=1,levlistdim
        spec_hum(:,:,k)=transpose(dummy(1,k,:,:))
     enddo
     call rearrange_ecmwf3(spec_hum,ecmwf_dims, dim1,dim2,levlistdim)
     spec_hum_flag=1
     deallocate(dummy)
  else
     allocate(spec_hum(1,1,1))
  endif
  
  !
  !temperature
  !
  if (any(available_names .eq. 'T')) then
     allocate(temperature(xdim,ydim,levlistdim))
     allocate(dummy(timedim,levlistdim,ydim,xdim))
     dummy=real_fill_value
     temperature=real_fill_value
     call nc_read_array_short_4d(ncid,timedim,levlistdim,ydim,xdim,'T',dummy,wo)
     do k=1,levlistdim
        temperature(:,:,k)=transpose(dummy(1,k,:,:))
     enddo


     call rearrange_ecmwf3(temperature,ecmwf_dims, dim1,dim2,levlistdim)
     temperature_flag=1
     deallocate(dummy)
  else
     allocate(temperature(1,1,1))
  endif
  
  !
  !ozone
  !
  if (any(available_names .eq. 'O3')) then
     allocate(ozone(xdim,ydim,levlistdim))
     ozone=real_fill_value
     allocate(dummy(timedim,levlistdim,ydim,xdim))
     dummy=real_fill_value
     call nc_read_array_short_4d(ncid,timedim,levlistdim,ydim,xdim,'O3',dummy,wo)
     do k=1,levlistdim
        ozone(:,:,k)=transpose(dummy(1,k,:,:))
     enddo
     call rearrange_ecmwf3(ozone,ecmwf_dims, dim1,dim2,levlistdim)
     ozone_flag=1
     deallocate(dummy)
  else
     allocate(ozone(1,1,1))
  endif
     
  !
  !Logarithm of surface pressure
  !
  
  if (any(available_names .eq. 'LNSP')) then
     dummy2d=real_fill_value
     allocate(lnsp(xdim,ydim))
     lnsp=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'LNSP',dummy2d,wo)
     lnsp(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(lnsp,ecmwf_dims, dim1,dim2)
     lnsp_flag=1
  else
     allocate(lnsp(1,1))
  endif

  !
  !MSL
  !
  if (any(available_names .eq. 'MSL')) then
     dummy2d=real_fill_value
     allocate(msl(xdim,ydim))
     msl=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'MSL',dummy2d,wo)
     msl(:,:)=transpose(dummy2d(1,1,:,:))
     msl_flag=1
  else
     allocate(msl(1,1))
  endif
   
  !
  !Sea-ice cover
  !
  if (any(available_names .eq. 'CI')) then
     dummy2d=real_fill_value
     allocate(sea_ice_cover(xdim,ydim))
     sea_ice_cover=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'CI',dummy2d,wo)
     sea_ice_cover(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(sea_ice_cover,ecmwf_dims, dim1,dim2)
     sea_ice_cover_flag=1 
  else
     allocate(sea_ice_cover(1,1))
  endif
   
  !
  !snow_albedo
  !
  if (any(available_names .eq. 'ASN')) then
     allocate(snow_albedo(xdim,ydim))
     dummy2d=real_fill_value
     snow_albedo=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'ASN',dummy2d,wo)
     snow_albedo(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(snow_albedo,ecmwf_dims, dim1,dim2)
     snow_albedo_flag=1
  else
     allocate(snow_albedo(1,1))
  endif
   
  ! 
  !totcolwv
  !
  if (any(available_names .eq. 'TCWV')) then
     allocate(totcolwv(xdim,ydim))
     dummy2d=real_fill_value
     totcolwv=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'TCWV',dummy2d,wo)
     totcolwv(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(totcolwv,ecmwf_dims, dim1,dim2)
     totcolwv_flag=1
  else
     allocate(totcolwv(1,1))
  endif

  !
  !snow_depth
  !
  if (any(available_names .eq. 'SD')) then
     allocate(snow_depth(xdim,ydim))
     dummy2d=real_fill_value
     snow_depth=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'SD',dummy2d,wo) 
     snow_depth(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(snow_depth,ecmwf_dims, dim1,dim2)
     snow_depth_flag=1
  else
     allocate(snow_depth(1,1))
  endif
  
  !
  !u10
  !
  if (any(available_names .eq. 'U10')) then
     allocate(u10(xdim,ydim))
     dummy2d=real_fill_value
     u10=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'U10',dummy2d,wo)
     u10(:,:)=transpose(dummy2d(1,1,:,:))
!    10 m wind components are needed for sea surface reflectance calculations
!    so copy them into the ecmwf structures
     ecmwf_2d%u10(:,:)=reshape(u10(:,:), (/ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec/))
     call rearrange_ecmwf(u10,ecmwf_dims, dim1,dim2)
     u10_flag=1
  else
     allocate(u10(1,1))
  endif

  !
  !v10
  !
  if (any(available_names .eq. 'V10')) then
     dummy2d=real_fill_value
     allocate(v10(xdim,ydim))
     v10=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'V10',dummy2d,wo)
     v10(:,:)=transpose(dummy2d(1,1,:,:))
!    10 m wind components are needed for sea surface reflectance calculations
!    so copy them into the ecmwf structures
     ecmwf_2d%v10(:,:)=reshape(v10(:,:), (/ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec/))
     call rearrange_ecmwf(v10,ecmwf_dims, dim1,dim2)
     v10_flag=1
  else
     allocate(v10(1,1))
  endif
   
  !
  !temp2
  !
  if (any(available_names .eq. 'T2')) then
     allocate(temp2(xdim,ydim))
     dummy2d=real_fill_value
     temp2=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'T2',dummy2d,wo)
     temp2(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(temp2,ecmwf_dims, dim1,dim2)
     temp2_flag=1
  else
     allocate(temp2(1,1))
  endif
   
  !
  !skin_temp
  !
  if (any(available_names .eq. 'SKT')) then
     dummy2d=real_fill_value
     allocate(skin_temp(xdim,ydim))
     skin_temp=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'SKT',dummy2d,wo)
     skin_temp(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(skin_temp,ecmwf_dims, dim1,dim2)
     skin_temp_flag=1
  else
     allocate(skin_temp(1,1))
  endif
   
  !
  !sst
  !
  if (any(available_names .eq. 'SSTK')) then
     dummy2d=real_fill_value
     allocate(sst(xdim,ydim))
     sst=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'SSTK',dummy2d,wo)
     sst(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(sst,ecmwf_dims, dim1,dim2)
     sst_flag=1
  else
     allocate(sst(1,1)) 
  endif
   
  !
  !surface_pressure
  !
  if (any(available_names .eq. 'SP')) then
     dummy2d=real_fill_value
     allocate(surface_pressure(xdim,ydim))
     surface_pressure=real_fill_value
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'SP',dummy2d,wo)
     surface_pressure(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(surface_pressure,ecmwf_dims, dim1,dim2)
     surface_pressure_flag=1
  else
     allocate(surface_pressure(1,1))
  endif

  !
  !pressure 
  ! NB for level data pressures are the same
  ! NB#2 We generate pressure on the reordered (preproc) grid, rather than the
  !      ECMWF grid, so x and y dimensions are swapped.
  if (levlistdim .eq. 37 .and. pressure_flag .eq. 0 .and. surfaceflag .eq. 0) then
     
     allocate(pressure(ydim,xdim,levlistdim))
     
     dummy1d=[1.,2.,3.,5.,7.,10.,20.,30.,50.,70.,100.,125.,150.,175.,200., &
          &   225.,250.,300.,350.,400.,450.,500.,550.,600.,650.,700.,750., &
          &   775.,800.,825.,850.,875.,800.,925.,950.,975.,1000.]*100.0
     
     do ii=1,xdim
        do jj=1,ydim
           pressure(jj,ii,:)=dummy1d(:)
        end do
     end do
     pressure_flag=1
  else
     allocate(pressure(1,1,1))
  endif

   !dummy1d=reverse([1000 , 975 , 950 , 925 , 900 , 875 , 850 , 825 , 800 , 775 , 750 , 700 , &
   !    650 , 600 , 550 , 500 , 450 , 400 , 350 , 300 , 250 , 225 , 200 , 175 ,  &
   !    150 , 125 , 100 , 70 , 50 , 30 , 20 , 10 , 7 , 5 , 3 , 2 , 1])
   
   !     if (available_names(ivar) .eq. 'p') then
   !       dummy1d=real_fill_value
   !    	allocate(pressure(xdim,ydim,levlistdim))
   !       call nc_read_array_float_1d(ncid,levlistdim,'p',dummy1d,wo)
   ! now fill global array
   !             pressure(ii,jj,:)=dummy1d(:)
   !    endif
   
   
  !
  !Land-sea mask (actually albedo)
  !
  if (any(available_names .eq. 'AL')) then
     dummy2d=real_fill_value
     allocate(land_sea_mask(xdim,ydim))
     call nc_read_array_short_4d(ncid,1,1,ydim,xdim,'AL',dummy2d,wo)
     land_sea_mask(:,:)=transpose(dummy2d(1,1,:,:))
     call rearrange_ecmwf(land_sea_mask,ecmwf_dims, dim1,dim2)
     land_sea_mask_flag=1
  else
     allocate(land_sea_mask(1,1))
  endif
   
  !   
  !now find nearest neighbour
  !
  
  !datin=temperature(:,:,ilevel)
  !xin=lon
  !yin=lat
  !xout=shiftlon
  !yout=shiftlat
  !
  !if these grids are not the  same then
  !
  
  !call interpol_nearest_neighbour(xin, yin, datin, xout, yout, datout
  
  ! preproc_prtm%temperature(:,:,ilevel)=datout
  
  !if the grids are the same no need to to interpolation just write out the data
  !
  !now write the data to preprocessing structure
  !


!!$   write(*,*)'sst_flag      ',         sst_flag               
!!$  write(*,*)'lnsp_flag    ',             lnsp_flag              
!!$  write(*,*)'sea_ice_cover_flag  ',      sea_ice_cover_flag     
!!$  write(*,*)'snow_albedo_flag ',         snow_albedo_flag       
!!$  write(*,*)'totcolwv_flag   ',          totcolwv_flag          
!!$  write(*,*)'snow_depth_flag  ',         snow_depth_flag        
!!$  write(*,*)'u10_flag      ',            u10_flag               
!!$  write(*,*)'v10_flag     ',             v10_flag               
!!$  write(*,*)'temp2_flag  ',              temp2_flag             
!!$  write(*,*)'land_sea_mask_flag  ',      land_sea_mask_flag     
!!$  write(*,*)'msl_flag      ',            msl_flag               
!!$  write(*,*)'skin_temp_flag  ',          skin_temp_flag         
!!$  write(*,*)'temperature_flag   ',       temperature_flag       
!!$  write(*,*)'spec_hum_flag  ',           spec_hum_flag          
!!$  write(*,*)'ozone_flag   ',             ozone_flag             
!!$  write(*,*)'geopot_flag  ',             geopot_flag            
!!$  write(*,*)'surface_pressure_flag ',    surface_pressure_flag  
!!$  write(*,*)'pressure_flag  ',           pressure_flag                           

! Copy the ECMWF data into the preprocessing structures
  do idim=preproc_dims%preproc_min_lat,preproc_dims%preproc_max_lat
     do jdim=preproc_dims%preproc_min_lon,preproc_dims%preproc_max_lon
        
        !
        !NB note x and y dimensions have swapped
        !

         do k=1,levlistdim
           if (temperature_flag .eq. 1) then 

              preproc_prtm%temperature(jdim,idim,k)=temperature(jdim,idim,k)
          endif
           
           if (spec_hum_flag .eq. 1) then 
              preproc_prtm%spec_hum(jdim,idim,k)=spec_hum(jdim,idim,k)
           endif
           
           if (ozone_flag .eq. 1)  then
              preproc_prtm%ozone(jdim,idim,k)=ozone(jdim,idim,k)
           endif
           
           if (pressure_flag .eq. 1)  then
              preproc_prtm%pressure(jdim,idim,k)=pressure(jdim,idim,k)
           endif
   
           !compute geopotential profile
           if (geopot_flag .eq. 1)  then
              preproc_prtm%geopot(jdim,idim)=geopot(jdim,idim)
           endif
           
           
        enddo
        ! end of loop over levels



        if (lnsp_flag .eq. 1) then 
           preproc_prtm%lnsp(jdim,idim)=lnsp(jdim,idim)

        endif



        if (msl_flag .eq. 1) then 

        endif
        
        if (sea_ice_cover_flag .eq. 1) then	
	    preproc_prtm%sea_ice_cover(jdim,idim)=sea_ice_cover(jdim,idim)
	 endif	    

 	 if (snow_albedo_flag .eq. 1) then 
	    preproc_prtm%snow_albedo(jdim,idim)=snow_albedo(jdim,idim)
	 endif	    

	  if (snow_depth_flag .eq. 1) then
	     preproc_prtm%snow_depth(jdim,idim)=snow_depth(jdim,idim)
	 endif	    

	 if (sst_flag .eq. 1) then  
	   preproc_prtm%sst(jdim,idim)=sst(jdim,idim)
 	 endif	    

	if (totcolwv_flag .eq. 1) then 
	    preproc_prtm%totcolwv(jdim,idim)=totcolwv(jdim,idim)
	 endif	    


         if (u10_flag .eq. 1) then
	   preproc_prtm%u10(jdim,idim)=u10(jdim,idim)
	 endif	    

         if (v10_flag .eq. 1) then  
	   preproc_prtm%v10(jdim,idim)=v10(jdim,idim)
	 endif	    

         if (temp2_flag .eq. 1) then 
	    preproc_prtm%temp2(jdim,idim)=temp2(jdim,idim)
	 endif	    

         if (land_sea_mask_flag .eq. 1) then 
	    preproc_prtm%land_sea_mask(jdim,idim)=land_sea_mask(jdim,idim)
	 endif	    

         if (skin_temp_flag .eq. 1) then 
	    preproc_prtm%skin_temp(jdim,idim)=skin_temp(jdim,idim)
	 endif	    


         if (surface_pressure_flag .eq. 1) then
	    preproc_prtm%surface_pressure(jdim,idim)=surface_pressure(jdim,idim)
	 endif	    

	enddo
    enddo

    if (spec_hum_flag .eq. 1) then 
       allocate(avector(1:ecmwf_dims%kdim_ec+1))
       avector=real_fill_value
       allocate(bvector(1:ecmwf_dims%kdim_ec+1))
       bvector=real_fill_value


       avector=[0.0000000E+00,20.00000,38.42534,63.64780, &
            & 95.63696,134.4833,180.5844,234.7791,298.4958, &
            & 373.9719,464.6182,575.6511,713.2180,883.6604, &
            & 1094.835,1356.475,1680.640,2082.274,2579.889, &
            & 3196.422,3960.292,4906.707,6018.020,7306.633, &
            & 8765.055,10376.12,12077.45,13775.32,15379.80, &
            & 16819.47,18045.18,19027.70,19755.11,20222.20, &
            & 20429.86,20384.48,20097.40,19584.33,18864.75, & 
            & 17961.36,16899.47,15706.45,14411.12,13043.22, &
            & 11632.76,10209.50,8802.355,7438.805,6144.316, &
            & 4941.777,3850.913,2887.697,2063.780,1385.913, &
            & 855.3618,467.3335,210.3939,65.88924,7.367743, &
            & 0.0000000E+00 , 0.0000000E+00]
       bvector= [ 0.0000000E+00,  0.0000000E+00 , 0.0000000E+00,  &
            & 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
            & 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
            & 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
            & 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
            & 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00,  &
            & 0.0000000E+00, 7.5823496E-05, 4.6139490E-04, 1.8151561E-03,  &
            & 5.0811172E-03, 1.1142910E-02, 2.0677876E-02, 3.4121163E-02,  &
            & 5.1690407E-02, 7.3533833E-02, 9.9674702E-02, 0.1300225,      &
            & 0.1643843,     0.2024759,     0.2439331,     0.2883230,      &
            & 0.3351549,     0.3838921,     0.4339629,     0.4847715,      &
            & 0.5357099,     0.5861684,     0.6355475,     0.6832686,      &
            & 0.7287858,     0.7715966,     0.8112534,     0.8473749,      &
            & 0.8796569,     0.9078839,     0.9319403,     0.9518215,      &
            & 0.9676452,     0.9796627,     0.9882701,     0.9940194,      &
            & 0.9976301,1.000000 ]
     
       !build now the pressure coordinate at cell centers from that:
       
       allocate(spec_hum_g(1:preproc_dims%kdim_pre))
       spec_hum_g=0.00
       allocate(temperature_g(1:preproc_dims%kdim_pre))
       temperature_g=0.00
       allocate(phi_lay(1:preproc_dims%kdim_pre-1))
       phi_lay=0.00
       allocate(phi_lev(1:preproc_dims%kdim_pre))
       phi_lev=0.00
       
       !write(*,*)'bvector',bvector
       !write(*,*)'convert ec lnsp',exp(preproc_prtm%lnsp(:,:))
       do jdim=preproc_dims%preproc_min_lat,preproc_dims%preproc_max_lat !1,preproc_dims%ydim_pre
          do idim=preproc_dims%preproc_min_lon,preproc_dims%preproc_max_lon !1,preproc_dims%xdim_pre
             !write(*,*)'convert ec lnsp',exp(preproc_prtm%lnsp(idim,jdim)),preproc_prtm%lnsp(idim,jdim)
             do ik=1,ecmwf_dims%kdim_ec
                !equation to convert model levels to pressure levels.
                !http://www.ecmwf.int/research/ifsdocs/DYNAMICS/Chap2_Discretization4.html#961180
                preproc_prtm%pressure(idim,jdim,ik)=0.5*(avector(ik)+avector(ik+1)+&
                     & (bvector(ik)+bvector(ik+1))*exp(preproc_prtm%lnsp(idim,jdim)))
                
             enddo
             
             !write(*,*)'convert ec',preproc_prtm%pressure(idim,jdim,ik)
             
             !compute geopotential profile
             geopot_g=preproc_prtm%geopot(idim,jdim)
             sp=exp(preproc_prtm%lnsp(idim,jdim))
             spec_hum_g(:)=preproc_prtm%spec_hum(idim,jdim,:)
             temperature_g(:)=preproc_prtm%temperature(idim,jdim,:)
 !            write(*,*)'sie',size(preproc_prtm%temperature(idim,jdim,:))
!             if (preproc_prtm%temperature(idim,jdim,59) .lt. 260) then 
!             write(*,*) 'temp',preproc_prtm%temperature(idim,jdim,:)
!             endif
!              write(*,*) 'pres', preproc_prtm%pressure(idim,jdim,:)
             !write(*,*) idim,jdim,shiftlon(idim),shiftlat(jdim)
             call compute_geopot_coordinate(ecmwf_dims,geopot_g,spec_hum_g,temperature_g,avector,bvector,sp,&
                  & phi_lev,phi_lay)
             preproc_prtm%phi_lay(idim,jdim,:)=phi_lay
             preproc_prtm%phi_lev(idim,jdim,:)=phi_lev
!write(*,*)'phi_lev',phi_lev
!write(*,*)'phi_lay',phi_lay             
             
          enddo
       enddo
       
       deallocate(spec_hum_g)
       deallocate(temperature_g)
       deallocate(phi_lev)
       deallocate(phi_lay)


!  write(*,*) 'PRES',minval(preproc_prtm%pressure),maxval(preproc_prtm%pressure)

       deallocate(avector)
       deallocate(bvector)

    endif

    !deallocate stuff again

    if (allocated(shiftlon)) then
       deallocate(shiftlon)
    endif

    if (allocated(shiftlat)) then
       deallocate(shiftlat)
    endif



    if (allocated(levelist)) then
       deallocate(levelist)
    endif
    if (allocated(levtype)) then
       deallocate(levtype)
    endif
    if (allocated(time)) then
       deallocate(time)
    endif
    
    if (allocated(dummy)) then
       deallocate(dummy)
    endif

    if (allocated(dummy1d)) then
       deallocate(dummy1d)
    endif
    
    if (allocated(dummy2d)) then
       deallocate(dummy2d)
    endif

    if (allocated(dimids)) then
       deallocate(dimids)
    endif

    if (allocated(dname)) then
       deallocate(dname)
    endif
    
    if (allocated(dimlength)) then
       deallocate(dimlength)
    endif
    
    if (allocated(varids)) then
       deallocate(varids)
    endif
    
    if (allocated(attrids)) then
       deallocate(attrids)
    endif
    
    if (allocated(available_names)) then
       deallocate(available_names)
    endif

    if (allocated(temperature)) then
       deallocate(temperature)
    endif

    if (allocated(spec_hum)) then
       deallocate(spec_hum)
    endif

    if (allocated(ozone)) then
       deallocate(ozone) 
    endif
    
    
    if (allocated(geopot)) then 
       deallocate(geopot) 
    endif
    

    if (allocated(sst)) then 
       deallocate(sst)
    endif
    
    if (allocated(lnsp)) then
       deallocate(lnsp)
    endif



    if (allocated(msl)) then
       deallocate(msl)
    endif

    if (allocated(pressure)) then
       deallocate(pressure)
    endif

    if (allocated(surface_pressure)) then
       deallocate(surface_pressure)
    endif
    
    if (allocated(sea_ice_cover)) then
       deallocate(sea_ice_cover)
    endif
    
    if (allocated(snow_albedo)) then
       deallocate(snow_albedo)
    endif
    
    if (allocated(totcolwv)) then
       deallocate(totcolwv)
    endif
    
    if (allocated(snow_depth)) then
       deallocate(snow_depth)
    endif
    
    if (allocated(u10)) then
       deallocate(u10)
    endif
    
    if (allocated(v10)) then
       deallocate(v10)
    endif

    if (allocated(temp2)) then
       deallocate(temp2)
    endif
    
    if (allocated(land_sea_mask)) then
       deallocate(land_sea_mask)
    endif
    
    if (allocated(skin_temp)) then
       deallocate(skin_temp)
    endif
    
    ierr=nf90_close(ncid)
   
  end subroutine read_ecmwf_nc
  
  
