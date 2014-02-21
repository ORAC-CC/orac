! Name:
!
!
! Purpose:
!
!
! Description and Algorithm details:
!
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
!
!
! $Id$
!
! Bugs:
!
!
!-------------------------------------------------------
subroutine open_modis_input(Ctrl,modis_input)
!-------------------------------------------------------

!  use netcdf
  
  use CTRL_def

  use input_structures

  implicit none

  include "hdf.f90"
  include "dffunc.f90"

  integer :: err_code, dims(2), dummy_type, dummy_numattrs, dummy_rank,dims_10

  character(len=50) :: dummy_name

  type(modis_input_d) :: modis_input

  type(CTRL_t)     :: Ctrl

  modis_input%l1b_id=sfstart(Ctrl%FID%nat_input,DFACC_READ)
  modis_input%geo_id=sfstart(Ctrl%FID%nat_geo,DFACC_READ)

  modis_input%dummy_var_id=sfselect(modis_input%geo_id,sfn2index(modis_input%geo_id,"Latitude"))
!  write(*,*) 'var_id',modis_input%dummy_var_id, dummy_type
  err_code=sfginfo(modis_input%dummy_var_id,dummy_name, dummy_rank, dims,dummy_type, dummy_numattrs)

!  write(*,*) dims

  modis_input%n_across_track=dims(1)
  modis_input%n_along_track=dims(2)
  
  modis_input%dummy_var_id=sfselect(modis_input%geo_id,sfn2index(modis_input%geo_id,"EV start time"))
  err_code=sfginfo(modis_input%dummy_var_id,dummy_name, dummy_rank, dims_10,dummy_type, dummy_numattrs)

  modis_input%n_along_track_10=dims_10

  modis_input%along_track_ratio=modis_input%n_along_track/modis_input%n_along_track_10

  err_code=sfendacc(modis_input%dummy_var_id)


  if(Ctrl%Ind%XMax .ne. modis_input%n_across_track .or. &
       & Ctrl%Ind%YMax .ne. modis_input%n_along_track .or. &
       & Ctrl%Ind%YMax/10 .ne. modis_input%n_along_track_10 ) then

     write(*,*)
     write(*,*) '!!!Mismatch of image size and file dimensions!!!'
     write(*,*)
     stop

  endif



!  err_code=sfend(modis_input%geo_id)


!  write(*,*) modis_input%n_across_track
!  write(*,*) modis_input%n_along_track

!  stop

!  write(*,*) modis_input%l1b_id
!  write(*,*) modis_input%geo_id

!  stop

end subroutine open_modis_input

!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_modis_input(Ctrl,modis_input,ixstart,ixstop,iystart,iystop,iread_start,iread_stop,segsize,MSI_Data,tfv)
!-------------------------------------------------------
!-------------------------------------------------------

!  use netcdf

  use ECP_Constants  
  use CTRL_def
  use Data_def

  use input_structures

  implicit none

  include "hdf.f90"
  include "dffunc.f90"

  integer :: ixstart,ixstop,iystart,iystop,iread_start,iread_stop,ix,jy,iread_startt,iread_stopt

  integer :: err_code, dims(2), dummy_type, dummy_numattrs, dummy_rank,segsize

  character(len=50) :: dummy_name

  real(kind=4), allocatable, dimension(:,:)  :: temp,temp2

  real(kind=dreal), allocatable, dimension(:)  :: ttemp10

  real(kind=dreal), allocatable, dimension(:,:)  :: ttemp

  real(kind=dreal) :: tfv

  integer(kind=byte), allocatable, dimension(:,:)  :: btemp,ltemp

  type(modis_input_d) :: modis_input

  type(CTRL_t)     :: Ctrl

  type(Data_t) :: MSI_Data

  integer        :: ChanIdx(Ctrl%Ind%Ny) ! Array of channel indices within the MSI file

  integer :: idummy,i, onecounter,zerocounter

  character*5 platform_name

  logical :: lrefl

  real :: dummy

  real modis_bright
  external modis_bright

  allocate(temp(ixstart:ixstop,iread_start:iread_stop))
  temp=0.00

  call read_modis_lat_lon(modis_input%geo_id, &
       &  "Latitude",ixstart,ixstop,iread_start,iread_stop, temp)

  MSI_Data%Location%Lat(ixstart:ixstop,1:segsize)=real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)

  call read_modis_lat_lon(modis_input%geo_id, &
       &  "Longitude",ixstart,ixstop,iread_start,iread_stop, temp)

  MSI_Data%Location%Lon(ixstart:ixstop,1:segsize)=real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)

  select case(mod(iread_start,modis_input%along_track_ratio))

  case(0)

     iread_startt=int(iread_start/modis_input%along_track_ratio)

  case default

     iread_startt=int(iread_start/modis_input%along_track_ratio)+1

  end select


  select case(mod(iread_stop,modis_input%along_track_ratio))

  case(0)

     iread_stopt=int(iread_stop/modis_input%along_track_ratio)

  case default

     iread_stopt=int(iread_stop/modis_input%along_track_ratio)+1

  end select


!!$  write(*,*) iread_start, iread_stop
!!$  write(*,*) iread_startt, iread_stopt
!!$
!!$  pause

  allocate(ttemp10(iread_startt:iread_stopt))

  call read_modis_time(modis_input%geo_id, &
       &  "EV start time",iread_startt,iread_stopt, ttemp10,tfv)

!  write(*,*) ttemp10,tfv

!  pause

  allocate(ttemp(ixstart:ixstop,iread_start:iread_stop))

  call map_time_to_pixel(modis_input,ixstart,ixstop,iread_start,iread_stop,iread_startt,iread_stopt,ttemp,ttemp10)

!!$  write(*,*) iread_start,iread_stop
!!$
!!$  write(*,*) ttemp(1,203)
!!$  write(*,*) ttemp(1,13)
!!$  write(*,*) ttemp(1,86)
!!$
!!$  pause

  deallocate(ttemp10)

  MSI_Data%Location%Time(ixstart:ixstop,1:segsize)=ttemp(ixstart:ixstop,iread_start:iread_stop)

!!$  write(*,*) ttemp10(4:5)
!!$
!!$  write(*,*)
!!$
!!$  write(*,*) ttemp(:,39:41)

  deallocate(ttemp)
  
  call read_modis_angles(modis_input%geo_id, &
       &  "SensorZenith",ixstart,ixstop,iread_start,iread_stop, temp)

  MSI_Data%Geometry%Sat(ixstart:ixstop,1:segsize,Ctrl%Ind%NViews)=&
       & real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)

  call read_modis_angles(modis_input%geo_id, &
       &  "SolarZenith",ixstart,ixstop,iread_start,iread_stop, temp)

  MSI_Data%Geometry%Sol(ixstart:ixstop,1:segsize,Ctrl%Ind%NViews)=&
       & real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)

  call read_modis_angles(modis_input%geo_id, &
       &  "SensorAzimuth",ixstart,ixstop,iread_start,iread_stop, temp)

  allocate(temp2(ixstart:ixstop,iread_start:iread_stop))
  temp2=0.00

  call read_modis_angles(modis_input%geo_id, &
       &  "SolarAzimuth",ixstart,ixstop,iread_start,iread_stop, temp2)

  temp2=180.0-temp2

!  write(*,*)
!  write(*,*) temp2(ixstart:ixstop,iread_start:iread_stop)
!  write(*,*)
!  write(*,*)
  

!azi=180d0-acos(cos((x.laa-x.saa)*!dtor))/!dtor

!!$  write(*,*) MSI_Data%Geometry%Azi(ixstart:ixstop,1:segsize,Ctrl%Ind%NViews)
!!$  write(*,*)
!!$  write(*,*)
!!$  write(*,*)
!!$  write(*,*) 180.0-&
!!$       & acos(cos((real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)-&
!!$       & real(temp2(ixstart:ixstop,iread_start:iread_stop),kind=sreal))*d2r))/d2r

!!$     write(*,*)
!!$     dummy=-1.0E9
!!$     do ix=ixstart,ixstop
!!$        do jy=1,segsize
!!$!           if(abs(MSI_Data%MSI(ix,jy,i)-temp(ix,jy))/MSI_Data%MSI(ix,jy,i) .gt. 1.0E-6) then
!!$!              write(*,*) ix, jy, MSI_Data%MSI(ix,jy,i),temp(ix,jy)
!!$!           endif
!!$           write(*,*) ix,jy,MSI_Data%Geometry%Azi(ix,jy,Ctrl%Ind%NViews), &
!!$                &180.0-acos(cos((real(temp(ix,jy),kind=sreal)-&
!!$                & real(temp2(ix,jy),kind=sreal))*d2r))/d2r
!!$           pause
!!$
!!$
!!$        enddo
!!$     enddo
!!$
!!$
!!$
!!$  pause


   MSI_Data%Geometry%Azi(ixstart:ixstop,1:segsize,Ctrl%Ind%NViews)=180.0-&
        & acos(cos((real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)-&
        & real(temp2(ixstart:ixstop,iread_start:iread_stop),kind=sreal))*d2r))/d2r

!THERE IS A PROBLEM HERE???!!!SKIPS NOW MORE PIXELS
!MJ  MSI_Data%Geometry%Azi(ixstart:ixstop,1:segsize,Ctrl%Ind%NViews)=&
!MJ       & abs(real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)-&
!MJ       & real(temp2(ixstart:ixstop,iread_start:iread_stop),kind=sreal))

  deallocate(temp2)

!!$  write(*,*)
!!$  write(*,*)
!!$
!!$  write(*,*)  abs(-real(temp(ixstart:ixstop,iread_start:iread_stop),kind=sreal)+&
!!$       & real(temp2(ixstart:ixstop,iread_start:iread_stop),kind=sreal))
!!$
!!$  pause
!!$
!!$  write(*,*)
!!$  write(*,*)
!!$
!!$  write(*,*) MSI_Data%Geometry%Azi
!!$
!!$  write(*,*)
!!$  write(*,*)
!!$
!!$  pause

  allocate(btemp(ixstart:ixstop,iread_start:iread_stop))
  btemp=-1

!THERE IS A PROBLEM HERE???!!! NO, there is just no proper implementation yet!!! done now!
  call read_modis_lsflag(modis_input%geo_id, &
       &  "Land/SeaMask",ixstart,ixstop,iread_start,iread_stop, btemp)
!!       &  "gflags",ixstart,ixstop,iread_start,iread_stop, temp)



  write(*,*) minval(MSI_Data%LSFlags(ixstart:ixstop,1:segsize)),maxval(MSI_Data%LSFlags(ixstart:ixstop,1:segsize))

!!$
!!$  onecounter=0
!!$  zerocounter=0
!!$  do ix=ixstart,ixstop
!!$     do jy=1,segsize
!!$
!!$        if(MSI_Data%LSFlags(ix,jy) .eq. 1 ) then
!!$           
!!$           onecounter=onecounter+1
!!$           
!!$        elseif(MSI_Data%LSFlags(ix,jy) .eq. 0 ) then
!!$           
!!$           zerocounter=zerocounter+1
!!$           
!!$        endif
!!$        
!!$        !        write(*,*) ix,jy,MSI_Data%LSFlags(ix,jy)
!!$        
!!$     enddo
!!$  enddo
!!$  
!!$  write(*,*) 'old','land:',onecounter, 'water:',zerocounter,zerocounter/float(onecounter)



  do ix=ixstart,ixstop
     do jy=iread_start,iread_stop

        call modis2oraclsflag(btemp(ix,jy))

     enddo
  enddo

!!$  onecounter=0
!!$  zerocounter=0
!!$  do ix=ixstart,ixstop
!!$     do jy=iread_start,iread_stop
!!$
!!$        if(btemp(ix,jy) .eq. 1 ) then
!!$           
!!$           onecounter=onecounter+1
!!$           
!!$        elseif(btemp(ix,jy) .eq. 0 ) then
!!$           
!!$           zerocounter=zerocounter+1
!!$           
!!$        endif
!!$        
!!$        !        write(*,*) ix,jy,MSI_Data%LSFlags(ix,jy)
!!$        
!!$     enddo
!!$  enddo
!!$  
!!$  write(*,*) 'new','land:',onecounter, 'water:',zerocounter,zerocounter/float(onecounter)
!!$

  MSI_Data%LSFlags(ixstart:ixstop,1:segsize)=btemp(ixstart:ixstop,iread_start:iread_stop)

!  pause

  deallocate(btemp)


  if(index(Ctrl%Inst%Name,'AQUA') .ge. 1) then
     platform_name='AQUA'
  elseif(index(Ctrl%Inst%Name,'TERRA') .ge. 1) then
     platform_name='TERRA'
  endif



  do i = 1, Ctrl%Ind%Ny

     if(Ctrl%Ind%Y_Id(i) .eq. 1) then
        idummy=1
        lrefl=.true.
     elseif(Ctrl%Ind%Y_Id(i) .eq. 2) then
        idummy=2
        lrefl=.true.
     elseif(Ctrl%Ind%Y_Id(i) .eq. 3) then
        idummy=6
        lrefl=.true.
     elseif(Ctrl%Ind%Y_Id(i) .eq. 4) then
        idummy=20
        lrefl=.true.
     elseif(Ctrl%Ind%Y_Id(i) .eq. 5) then
        idummy=31
        lrefl=.false.
     elseif(Ctrl%Ind%Y_Id(i) .eq. 6) then
        idummy=32
        lrefl=.false.
     endif


     ChanIdx(i) = idummy + (Ctrl%Ind%NChans * (Ctrl%Ind%ViewIdx(i)-1))
!MJORG     ChanIdx(i) = Ctrl%Ind%Y_Id(i) + (Ctrl%Ind%NChans * (Ctrl%Ind%ViewIdx(i)-1))
     write(*,*) 'ChanIdx(i) ',ChanIdx(i) 
     write(*,*) 'y_idn',i,Ctrl%Ind%Y_Id(i),Ctrl%Ind%NChans,Ctrl%Ind%ViewIdx(i)
     write(*,*) 'test',Ctrl%Ind%Y_Id(i) + (Ctrl%Ind%NChans * (Ctrl%Ind%ViewIdx(i)-1))

     call read_L1B_modis_reflectances_radiances(modis_input%l1b_id, &
          & ChanIdx(i), lrefl, ixstart,ixstop,iread_start,iread_stop,temp)

     if(.not. lrefl) then

        do ix=ixstart,ixstop
           do jy=iread_start,iread_stop
              
              temp(ix,jy)=MODIS_BRIGHT(platform_name,temp(ix,jy),ChanIdx(i),1)
              
           enddo
        enddo
        
     endif
        
!!$     write(*,*)
!!$     dummy=-1.0E9
!!$     do ix=ixstart,ixstop
!!$        do jy=1,segsize
!!$!           if(abs(MSI_Data%MSI(ix,jy,i)-temp(ix,jy))/MSI_Data%MSI(ix,jy,i) .gt. 1.0E-6) then
!!$!              write(*,*) ix, jy, MSI_Data%MSI(ix,jy,i),temp(ix,jy)
!!$!           endif
!!$           if(abs(MSI_Data%MSI(ix,jy,i)-temp(ix,jy))/MSI_Data%MSI(ix,jy,i) .gt. dummy) then
!!$              dummy=abs(MSI_Data%MSI(ix,jy,i)-temp(ix,jy))/MSI_Data%MSI(ix,jy,i)
!!$           endif
!!$        enddo
!!$     enddo

     MSI_Data%MSI(ixstart:ixstop,1:segsize,i)=temp(ixstart:ixstop,iread_start:iread_stop)

!     write(*,*) dummy

!     pause

  end do

  deallocate(temp)

!  write(*,*) 'here'
!  write(*,*) temp(ixstart:ixstop,iread_start:iread_stop)
!  pause
!  write(*,*) MSI_Data%Location%Lon(ixstart:ixstop,1:segsize)-temp(ixstart:ixstop,iread_start:iread_stop)

!  pause

end subroutine read_modis_input

!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_L1B_modis (fid, level1b_filename, &
     & band, Cal_type_is_refl, xdimension, ydimension, start_val,&
     & level1b_buffer, uncertainty_buffer, uncertain_spec, uncertain_scale)
!-------------------------------------------------------
!-------------------------------------------------------	

  use CTRL_def
  
  use input_structures

  implicit none
  
  include "hdf.f90"
  include "dffunc.f90"
  
  
  integer, intent(in) :: fid
  character(*), intent(in) :: level1b_filename
  logical, intent(in) ::  Cal_type_is_refl
  integer, intent(in) ::  band, xdimension, ydimension, start_val
  integer*1, dimension(:,:), intent(inout) :: uncertainty_buffer
  real, dimension(:,:), intent(inout) :: level1b_buffer
  real, intent(inout) :: uncertain_spec, uncertain_scale
  
  integer :: file_id, var_id, err_code, start(3), stride(3), edge(3), attr_id
  real*4 :: scale_factors(20), offsets(20)
  real*4 :: spec_unc(20), unc_scale(20)
  character(len=200) :: SDS_name, SDS_unc_name
  integer :: band_index
  integer*2, dimension(:,:), allocatable :: temp
  
  
  if (band >= 1 .and. band <=2) then 
     SDS_name = "EV_250_Aggr1km_RefSB"
     SDS_unc_name = "EV_250_Aggr1km_RefSB_Uncert_Indexes"  
  endif
  if (band >= 3 .and. band <=7) then
     SDS_name = "EV_500_Aggr1km_RefSB" 
     SDS_unc_name = "EV_500_Aggr1km_RefSB_Uncert_Indexes"          
  endif
  if (band >=8 .and. band<=19 .or. band == 26) then
     SDS_name = "EV_1KM_RefSB" 
     SDS_unc_name = "EV_1KM_RefSB_Uncert_Indexes"
  endif
  if (band >= 20 .and. band <= 36 .and. band /=26) then 
     SDS_name = "EV_1KM_Emissive"
     SDS_unc_name = "EV_1KM_Emissive_Uncert_Indexes"
  endif
  ! first we need to find where to start
  if (band >=1 .and. band <= 2) band_index = band-1  
  if (band >=3 .and. band <= 7) band_index = band-3 
  if (band >=8 .and. band <=12) band_index = band-8  
  if (band >=13 .and. band <=15) band_index = 2*band-21 
  if (band >=15 .and. band <=19) band_index = band-6 
  if (band >=20 .and. band <=25) band_index = band-20  
  if (band >=27 .and. band <=36) band_index = band-21 
  if (band == 26) band_index = 14
  
  
  !	start(1) = 0
  !	start(2) = (scan_number-1)*10
  start(2) = 0
  start(1) = start_val !(scan_number-1)*99
  start(3) = band_index
  
  stride = 1
  
  edge(1) = xdimension
  edge(2) = ydimension
  edge(3) = 1
 
 allocate(temp(xdimension, ydimension))
 
 file_id = fid
 var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
 
 err_code = sfrdata(var_id, start, stride, edge, temp)
 
 if (Cal_type_is_refl) then 
    attr_id = sffattr(var_id, "reflectance_scales")
    err_code = sfrattr(var_id, attr_id, scale_factors)
    attr_id = sffattr(var_id, "reflectance_offsets")
    err_code = sfrattr(var_id, attr_id, offsets)
    
    level1b_buffer = (temp*1.0 - offsets(band_index+1)) *scale_factors(band_index+1)
 else
    attr_id = sffattr(var_id, "radiance_scales")
    err_code = sfrattr(var_id, attr_id, scale_factors)
    attr_id = sffattr(var_id, "radiance_offsets")
    err_code = sfrattr(var_id, attr_id, offsets)
    
    level1b_buffer = (temp*1.0 - offsets(band_index+1)) *scale_factors(band_index+1)
    
 endif
 
 deallocate(temp)
 
 err_code = sfendacc(var_id)
 
 var_id = sfselect(file_id, sfn2index(file_id, SDS_unc_name))
 
 attr_id = sffattr(var_id, "specified_uncertainty")
 err_code = sfrattr(var_id, attr_id, spec_unc)
 attr_id = sffattr(var_id, "scaling_factor")
 err_code = sfrattr(var_id, attr_id, unc_scale)
 
 err_code = sfrdata(var_id, start, stride, edge, uncertainty_buffer)
 
 err_code = sfendacc(var_id)
 
 
 uncertain_spec = spec_unc(band_index+1)
 uncertain_scale = unc_scale(band_index+1)				  
					  
end subroutine read_L1B_modis



!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_modis_lat_lon(fid, &
     &  SDS_name,ixstart,ixstop,iread_start,iread_stop,temp)
!-------------------------------------------------------
!-------------------------------------------------------	

  use CTRL_def

  use ECP_Constants  

  use input_structures

  implicit none
  
  include "hdf.f90"
  include "dffunc.f90"
  
  integer :: ixstart, ixstop,iread_start,iread_stop,ix,jy

  integer, intent(in) :: fid
  
  integer :: file_id, var_id, err_code, start(2), stride(2), edge(2), attr_id
  character(len=*) :: SDS_name
  
  real(kind=sreal) :: temp(ixstart:ixstop,iread_start:iread_stop),fv,vr(2)
  
  type(modis_input_d) :: modis_input

  !	start(1) = 0
  !	start(2) = (scan_number-1)*10

  start(1) = ixstart-1 !(scan_number-1)*99
  start(2) = iread_start-1! j-1
  
  stride = 1
  
  edge(1) = ixstop-ixstart+1
  edge(2) = iread_stop-iread_start+1
 
  file_id = fid
  var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
 
  err_code = sfrdata(var_id, start, stride, edge, temp)

  attr_id=sffattr(var_id, "_FillValue")
  err_code=sfrattr(var_id, attr_id, fv)

  attr_id=sffattr(var_id, "valid_range")
  err_code=sfrattr(var_id, attr_id, vr)
	
!  write(*,*) 'range', vr,fv

  do ix=ixstart,ixstop
     do jy=iread_start,iread_stop

        if(temp(ix,jy) .lt. vr(1) .or. temp(ix,jy) .gt. vr(2)) temp(ix,jy)=fv

     enddo
  enddo
  
  err_code=sfendacc(modis_input%dummy_var_id)
				  
end subroutine read_modis_lat_lon



!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_modis_time(fid, &
     &  SDS_name,iread_start,iread_stop,temp,fv)
!-------------------------------------------------------
!-------------------------------------------------------	

  use CTRL_def

  use ECP_Constants  

  use input_structures

  implicit none
  
  include "hdf.f90"
  include "dffunc.f90"
  
  integer :: iread_start,iread_stop,jy

  integer, intent(in) :: fid
  
  integer :: file_id, var_id, err_code, start(1), stride(1), edge(1), attr_id
  character(len=*) :: SDS_name
  
  real(kind=dreal) :: temp(iread_start:iread_stop-1),fv
  
  type(modis_input_d) :: modis_input

  !	start(1) = 0
  !	start(2) = (scan_number-1)*10

  start(1) = iread_start-1! j-1
  
  stride = 1
  
  edge(1) = iread_stop-iread_start+1
 
  file_id = fid
  var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
 
  err_code = sfrdata(var_id, start, stride, edge, temp)

  attr_id=sffattr(var_id, "_FillValue")
  err_code=sfrattr(var_id, attr_id, fv)

  write(*,*) 'range', fv,start,edge,iread_start,iread_stop

!!$  do jy=(int(iread_stop/modis_input%n_along_track_10)+1),(int(iread_start/modis_input%n_along_track_10)+1)
!!$
!!$     if(temp(jy) .lt. vr(1) .or. temp(ix,jy) .gt. vr(2)) temp(ix,jy)=fv
!!$
!!$     enddo
!!$  enddo
  
  err_code=sfendacc(modis_input%dummy_var_id)
				  
end subroutine read_modis_time





!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_modis_angles(fid, &
     &  SDS_name,ixstart,ixstop,iread_start,iread_stop,rtemp)
!-------------------------------------------------------
!-------------------------------------------------------	

  use CTRL_def
  
  use input_structures

  use ECP_Constants  

  implicit none
  
  include "hdf.f90"
  include "dffunc.f90"
  
  integer :: ixstart, ixstop,iread_start,iread_stop,ix,jy

  integer, intent(in) :: fid
  
  integer :: file_id, var_id, err_code, start(2), stride(2), edge(2), attr_id
  character(len=*) :: SDS_name
  
  real(kind=sreal) :: rtemp(ixstart:ixstop,iread_start:iread_stop)

  real(kind=dreal) :: sf
  
  integer(kind=sint) :: stemp(ixstart:ixstop,iread_start:iread_stop), vr(2),fv

  type(modis_input_d) :: modis_input

  !	start(1) = 0
  !	start(2) = (scan_number-1)*10

  start(1) = ixstart-1 !(scan_number-1)*99
  start(2) = iread_start-1! j-1
  
  stride = 1
  
  edge(1) = ixstop-ixstart+1
  edge(2) = iread_stop-iread_start+1
 
  file_id = fid
  var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
 
  err_code = sfrdata(var_id, start, stride, edge, stemp)
	
!  write(*,*) 'code',fid, var_id, SDS_name,err_code

  attr_id=sffattr(var_id, "scale_factor")
  err_code=sfrattr(var_id, attr_id, sf)

  attr_id=sffattr(var_id, "_FillValue")
  err_code=sfrattr(var_id, attr_id, fv)

  attr_id=sffattr(var_id, "valid_range")
  err_code=sfrattr(var_id, attr_id, vr)

!  write(*,*) 'sf',scale_factor , vr, fv
!                 reflectance_sw_1km_raw_2d(idim,jdim,1)= &
!                      & reflectance_scales(select_channel)* &
!                      & (real(dummyreflectance(idim,jdim,select_channel),kind=sreal)-reflectance_offsets(select_channel))
!  pause

!Which one is better? This:
  rtemp=real_fill_value

  do ix=ixstart,ixstop
     do jy=iread_start,iread_stop

        if(stemp(ix,jy) .ge. vr(1) .and. stemp(ix,jy) .le. vr(2)) then

           rtemp(ix,jy)=real(stemp(ix,jy)*sf,kind=sreal)

        endif

     enddo
  enddo

! or:

!!$  do ix=ixstart,ixstop
!!$     do jy=iread_start,iread_stop
!!$
!!$        if(stemp(ix,jy) .lt. vr(1) .or. stemp(ix,jy) .gt. vr(2)) then
!!$
!!$           rtemp(ix,jy)=real_fill_value
!!$
!!$        else
!!$
!!$           rtemp(ix,jy)=real(stemp(ix,jy)*sf,kind=sreal)
!!$
!!$        endif
!!$
!!$     enddo
!!$  enddo

  err_code=sfendacc(modis_input%dummy_var_id)
				  
end subroutine read_modis_angles

!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_modis_lsflag(fid, &
     &  SDS_name,ixstart,ixstop,iread_start,iread_stop,btemp)
!-------------------------------------------------------
!-------------------------------------------------------	

  use CTRL_def
  
  use input_structures

  use ECP_Constants  

  implicit none
  
  include "hdf.f90"
  include "dffunc.f90"
  
  integer :: ixstart, ixstop,iread_start,iread_stop,ix,jy

  integer, intent(in) :: fid
  
  integer :: file_id, var_id, err_code, start(2), stride(2), edge(2), attr_id
  character(len=*) :: SDS_name
  
  integer(kind=byte) :: btemp(ixstart:ixstop,iread_start:iread_stop)

  integer(kind=sint) :: fv

!  byte :: btemp(ixstart:ixstop,iread_start:iread_stop),fv

  type(modis_input_d) :: modis_input

  !	start(1) = 0
  !	start(2) = (scan_number-1)*10

  start(1) = ixstart-1 !(scan_number-1)*99
  start(2) = iread_start-1! j-1
  
  stride = 1
  
  edge(1) = ixstop-ixstart+1
  edge(2) = iread_stop-iread_start+1
  
  file_id = fid
  var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
  
  err_code = sfrdata(var_id, start, stride, edge, btemp)
  
  attr_id=sffattr(var_id, "_FillValue")
  err_code=sfrattr(var_id, attr_id, fv)
  
  do ix=ixstart,ixstop
     do jy=iread_start,iread_stop
        
        if(btemp(ix,jy) .eq. fv ) then
           
           btemp(ix,jy)=-1
                      
        endif
        
     enddo
  enddo

  err_code=sfendacc(modis_input%dummy_var_id)
				  
end subroutine read_modis_lsflag



!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_modis_reflectances(fid, &
     &  SDS_name,ixstart,ixstop,iread_start,iread_stop,rtemp)
!-------------------------------------------------------
!-------------------------------------------------------	

  use CTRL_def
  
  use input_structures

  use ECP_Constants  

  implicit none
  
  include "hdf.f90"
  include "dffunc.f90"
  
  integer :: ixstart, ixstop,iread_start,iread_stop,ix,jy

  integer, intent(in) :: fid
  
  integer :: file_id, var_id, err_code, start(2), stride(2), edge(2), attr_id
  character(len=*) :: SDS_name
  
  real(kind=sreal) :: rtemp(ixstart:ixstop,iread_start:iread_stop)

  real(kind=dreal) :: sf
  
  integer(kind=sint) :: stemp(ixstart:ixstop,iread_start:iread_stop), vr(2),fv

  type(modis_input_d) :: modis_input

  !	start(1) = 0
  !	start(2) = (scan_number-1)*10

  start(1) = ixstart-1 !(scan_number-1)*99
  start(2) = iread_start-1! j-1
  
  stride = 1
  
  edge(1) = ixstop-ixstart+1
  edge(2) = iread_stop-iread_start+1
 
  file_id = fid
  var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
 
  err_code = sfrdata(var_id, start, stride, edge, stemp)
	
!  write(*,*) 'code',fid, var_id, SDS_name,err_code

  attr_id=sffattr(var_id, "scale_factor")
  err_code=sfrattr(var_id, attr_id, sf)

  attr_id=sffattr(var_id, "_FillValue")
  err_code=sfrattr(var_id, attr_id, fv)

  attr_id=sffattr(var_id, "valid_range")
  err_code=sfrattr(var_id, attr_id, vr)

!  write(*,*) 'sf',scale_factor , vr, fv
!                 reflectance_sw_1km_raw_2d(idim,jdim,1)= &
!                      & reflectance_scales(select_channel)* &
!                      & (real(dummyreflectance(idim,jdim,select_channel),kind=sreal)-reflectance_offsets(select_channel))
!  pause

!Which one is better? This:
  rtemp=real_fill_value

  do ix=ixstart,ixstop
     do jy=iread_start,iread_stop

        if(stemp(ix,jy) .ge. vr(1) .and. stemp(ix,jy) .le. vr(2)) then

           rtemp(ix,jy)=real(stemp(ix,jy)*sf,kind=sreal)

        endif

     enddo
  enddo

! or:

!!$  do ix=ixstart,ixstop
!!$     do jy=iread_start,iread_stop
!!$
!!$        if(stemp(ix,jy) .lt. vr(1) .or. stemp(ix,jy) .gt. vr(2)) then
!!$
!!$           rtemp(ix,jy)=real_fill_value
!!$
!!$        else
!!$
!!$           rtemp(ix,jy)=real(stemp(ix,jy)*sf,kind=sreal)
!!$
!!$        endif
!!$
!!$     enddo
!!$  enddo

  err_code=sfendacc(modis_input%dummy_var_id)
				  
end subroutine read_modis_reflectances

!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_L1B_modis_reflectances_radiances(fid, &
     & band, Cal_type_is_refl, ixstart,ixstop,iread_start,iread_stop,level1b_buffer)
  !xdimension, ydimension, start_val,&
  !     & level1b_buffer, uncertainty_buffer, uncertain_spec, uncertain_scale)
!-------------------------------------------------------
!-------------------------------------------------------	

  use CTRL_def
  
  use input_structures

  implicit none
  
  include "hdf.f90"
  include "dffunc.f90"
    
  integer, intent(in) :: fid
  logical, intent(in) ::  Cal_type_is_refl
  integer, intent(in) ::  band
  real(kind=sreal), intent(inout) :: level1b_buffer(ixstart:ixstop,iread_start:iread_stop)
  
  integer :: file_id, var_id, err_code, start(3), stride(3), edge(3), attr_id
  real(kind=sreal) :: scale_factors(20), offsets(20)
  real(kind=sreal) :: spec_unc(20), unc_scale(20)
  character(len=100) :: SDS_name, SDS_unc_name
  integer :: band_index
  integer :: ixstart, ixstop,iread_start,iread_stop,ix,jy

  integer(kind=sint) :: temp(ixstart:ixstop,iread_start:iread_stop), fv, vr(2)
  
  write(*,*) 'band', band

  if (band >= 1 .and. band <=2) then 
     SDS_name = "EV_250_Aggr1km_RefSB"
     SDS_unc_name = "EV_250_Aggr1km_RefSB_Uncert_Indexes"  
  endif
  if (band >= 3 .and. band <=7) then
     SDS_name = "EV_500_Aggr1km_RefSB" 
     SDS_unc_name = "EV_500_Aggr1km_RefSB_Uncert_Indexes"          
  endif
  if (band >=8 .and. band<=19 .or. band == 26) then
     SDS_name = "EV_1KM_RefSB" 
     SDS_unc_name = "EV_1KM_RefSB_Uncert_Indexes"
  endif
  if (band >= 20 .and. band <= 36 .and. band /=26) then 
     SDS_name = "EV_1KM_Emissive"
     SDS_unc_name = "EV_1KM_Emissive_Uncert_Indexes"
  endif
  ! first we need to find where to start
  !MJ THIS REFERS TO THE ORIGINAL FILE WHICH CONTIANS ALL 36 CHANNELS
!!$  if (band >=1 .and. band <= 2) band_index = band-1  
!!$  if (band >=3 .and. band <= 7) band_index = band-3 
!!$  if (band >=8 .and. band <=12) band_index = band-8  
!!$  if (band >=13 .and. band <=15) band_index = 2*band-21 
!!$  if (band >=15 .and. band <=19) band_index = band-6 
!!$  if (band >=20 .and. band <=25) band_index = band-20  
!!$  if (band >=27 .and. band <=36) band_index = band-21 
!!$  if (band == 26) band_index = 14

  !MJ THIS IS CORRECT FOR OUR 6 CHANNEL SUBSET FILE!
  if (band .eq. 1 .or. band .eq. 2) band_index = band-1  
  if (band .eq. 6 ) band_index = band-6 
  if (band .eq. 20 ) band_index = band-20 
  if( band .eq. 31 ) band_index = band-30
  if( band .eq. 32) band_index = band-30
  
  write(*,*) 'band_index',band_index
  
  
  !	start(1) = 0
  !	start(2) = (scan_number-1)*10
  start(2) = iread_start-1
  start(1) = ixstart-1 !(scan_number-1)*99
  start(3) = band_index
  
  stride = 1
  
  edge(1) = ixstop-ixstart+1
  edge(2) = iread_stop-iread_start+1
  edge(3) = 1
  
  ! allocate(temp(xdimension, ydimension))
  
  file_id = fid
  var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
  
  err_code = sfrdata(var_id, start, stride, edge, temp)
  
!!$  if(band .eq. 31 .or. band .eq. 32 .or. band .eq. 6) then
!!$
!!$     write(*,*) band, ixstart,iread_start, temp(ixstart,iread_start)
!!$     pause
!!$
!!$  endif

  attr_id=sffattr(var_id, "_FillValue")
  err_code=sfrattr(var_id, attr_id, fv)
  
  attr_id=sffattr(var_id, "valid_range")
  err_code=sfrattr(var_id, attr_id, vr)
  
  
  if (Cal_type_is_refl) then 
     
     attr_id = sffattr(var_id, "reflectance_scales")
     err_code = sfrattr(var_id, attr_id, scale_factors)
     attr_id = sffattr(var_id, "reflectance_offsets")
     err_code = sfrattr(var_id, attr_id, offsets)
     
  else
    
     attr_id = sffattr(var_id, "radiance_scales")
     err_code = sfrattr(var_id, attr_id, scale_factors)
     attr_id = sffattr(var_id, "radiance_offsets")
     err_code = sfrattr(var_id, attr_id, offsets)
     
  endif
  
  level1b_buffer=real_fill_value
  
!  write(*,*) 'hier'

  do ix=ixstart,ixstop
     do jy=iread_start,iread_stop
        
!        write(*,*) ix,jy

        if(temp(ix,jy) .ge. vr(1) .and. temp(ix,jy) .le. vr(2)) then
           
           level1b_buffer(ix,jy) = (real(temp(ix,jy),kind=sreal) - offsets(band_index+1)) *scale_factors(band_index+1)
           
        endif
        
     enddo
  enddo

!!$ write(*,*) temp(:,1)
!!$ write(*,*) offsets(band_index+1),scale_factors(band_index+1), vr, fv
!!$ pause
 
  err_code = sfendacc(var_id)
 					  
end subroutine read_L1B_modis_reflectances_radiances



!-------------------------------------------------------
!-------------------------------------------------------
subroutine modis2oraclsflag(temp)
!-------------------------------------------------------
!-------------------------------------------------------

  use ECP_Constants  

  implicit none

  integer(kind=byte) :: temp

!!$
!!$  
!!$     DN values:
!!$                0:      Shallow Ocean (Ocean <5k from coast OR <50m deep).
!!$                1:      Land (not anything else).
!!$                2:      Ocean Coastlines and Lake Shorelines.
!!$                3:      Shallow Inland Water (Inland Water < 5km from shore
!!$                                OR < 50m deep).
!!$                4:      Ephemeral (intermittent) Water.
!!$                5:      Deep Inland Water (Inland water > 5km from shoreline
!!$                                AND > 50m deep).
!!$                6:      Moderate or Continental Ocean (Ocean > 5km from coast
!!$                                AND > 50m deep AND < 500m deep).
!!$                7:      Deep Ocean (Ocean > 500m deep).
!!$
!!$                SDS Attributes:
!!$                Attribute Name        Format            Example
!!$                --------------        ------            -------
!!$
!!$                _FillValue            uint8             221
!!$


  lsflag_mapping:  select  case(temp)

  case(0,5,6,7) !water 0
     
     temp=0

  case(1,2,3,4) !land 1

     temp=1
   
  case(-1) !fill value

     write(*,*) 'PROBLEM'
     temp=1

  case default !default land

     temp=1

  end select lsflag_mapping


end subroutine modis2oraclsflag


!-------------------------------------------
!-------------------------------------------
subroutine map_time_to_pixel(modis_input,ixstart,ixstop,iread_start,iread_stop,iread_startt,iread_stopt,ttemp,ttemp10)
!-------------------------------------------
!-------------------------------------------

  use ECP_Constants

  use input_structures

  implicit none

  integer :: jy,jyt, ixstart,ixstop,iread_start,iread_stop,iread_startt,iread_stopt

  real(kind=dreal) :: ttemp10(iread_startt:iread_stopt)

  real(kind=dreal) :: ttemp(ixstart:ixstop,iread_start:iread_stop)

  type(modis_input_d) :: modis_input

  do jy=iread_start,iread_stop

     select case(mod(jy,modis_input%along_track_ratio))
        
     case(0)
        
        jyt=int(jy/modis_input%along_track_ratio)
        
     case default
        
        jyt=int(jy/modis_input%along_track_ratio)+1

     end select

     ttemp(:,jy)=ttemp10(jyt)

  enddo

  
  

end subroutine map_time_to_pixel
