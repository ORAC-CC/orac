! Name: read_inter_file.f90
!
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for different attribute/variable type combinations.
! Subroutines names are selfdescriptive.
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
! 2012/02/20: Caroline Poulsen creates initial file.
! 2012/03/18 Caroline Poulsen modified to add cloud flag
! 2012/07/06 MJ extensively overhauls and restructures the code
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
! 2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when building libraries.
! 2014/10/24 OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
! 2014/11/26 CP: added cloud_albedo
!  (currently deactivated), and nisemask; commented out reading of variables
!   for water within if condition iphase = 2 (never true for water)
! 2014/12/02 CP: reads in global and source attributes from file
! 2015/01/26 CP: add in ml from IR only option changed to common constants
!
!
!
! $Id$
!
! Bugs:
!
!none known


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_inter_file_ice(iphase,fname,l2_input_2dice_primary,xdim,ydim,global_atts,source_atts,wo,ierr)
  !--------------------------------------------------
  use netcdf

  use vartypes_pp

  use structures_pp
  use global_attributes
  use source_attributes

  implicit none

  INTEGER,INTENT(IN) :: wo
  integer :: ivar,idim,ndim,nvar,nattr,dummyint,iphase
  integer :: ncid,ierr,ny=5,nx=5
  character(len=path_length) :: fname,name
  integer (kind=lint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
  character(len=varlength), allocatable :: dname(:)
    character(len=32)  :: input_num
   character(len=512) :: input_dummy
  character (len=varlength), allocatable, dimension(:) ::  available_names(:)
  INTEGER(kind=lint) ::  xdim,ydim,i

  CHARACTER(LEN=unitlength) :: dummy_unit
  type(l2_input_struct_2d_primary) :: l2_input_2dice_primary
   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts
  real (kind=sreal), allocatable, dimension(:,:) :: l2var_dummy
  real (kind=dreal), allocatable, dimension(:,:) :: l2var_dummy_double
  integer(kind=byte), allocatable, dimension(:,:) :: l2var_dummy_byte
  integer(kind=sint), allocatable, dimension(:,:) :: l2var_dummy_short
  integer(kind=lint) :: chan_id(2),nchan
  !  write(*,*) fname
  !pause
  call nc_open_pp(ncid,fname,ierr,wo)
  call nc_info_pp(ncid,ndim,nvar,nattr,wo)

  allocate(l2var_dummy_double(xdim,ydim))
  l2var_dummy_double=dreal_fill_value

  allocate(l2var_dummy(xdim,ydim))
  l2var_dummy=dreal_fill_value

  allocate(l2var_dummy_short(xdim,ydim))
  l2var_dummy_short= sint_fill_value

  allocate(l2var_dummy_byte(xdim,ydim))
  l2var_dummy_byte=byte_fill_value

  if(iphase .eq. 2 ) then

     l2var_dummy=sreal_fill_value
     l2var_dummy_double=dreal_fill_value
     l2var_dummy_short= sint_fill_value
     l2var_dummy_byte=byte_fill_value

     !MJ? time has to be included
     !read time data
     !write(*,*) 'read time',xdim,ydim
     call nc_read_array_2d_double_orac_pp(ncid,xdim,ydim,"time",l2var_dummy_double,wo)
     l2_input_2dice_primary%time=l2var_dummy_double

     !lat/lon
     !write(*,*) 'read latlon'
     call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
          &                 'lon',l2var_dummy,dummy_unit,wo)
     l2_input_2dice_primary%lon=l2var_dummy

     call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
          &          'lat',l2var_dummy,dummy_unit,wo)
     l2_input_2dice_primary%lat=l2var_dummy

     !satzen
     call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
          &          'satellite_zenith_view_no1',l2var_dummy,dummy_unit,wo)
     l2_input_2dice_primary%satellite_zenith_view_no1=l2var_dummy

     !solzen
     call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
          &          'solar_zenith_view_no1',l2var_dummy,dummy_unit,wo)
     l2_input_2dice_primary%solar_zenith_view_no1=l2var_dummy


     !relazi
     call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
          &          'rel_azimuth_view_no1',l2var_dummy,dummy_unit,wo)
     l2_input_2dice_primary%rel_azimuth_view_no1=l2var_dummy

!!$  !cty
!!$  call nc_read_array_2d_byte_orac(ncid,xdim,ydim, &
!!$       &          'phase',l2var_dummy_byte,dummy_unit,wo)            
!!$  l2_input_2d_primary%cty=real(l2var_dummy_byte,kind=sreal)
!!$  

     !lsflag
     call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
          &          'lsflag',l2var_dummy_byte,dummy_unit,wo)            
     l2_input_2dice_primary%lsflag=l2var_dummy_byte

     !illum
     call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
          &          'illum',l2var_dummy_byte,dummy_unit,wo)            
     l2_input_2dice_primary%illum=l2var_dummy_byte

     !cldtype
     call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
          &          'cldtype',l2var_dummy_byte,dummy_unit,wo)            
     l2_input_2dice_primary%cldtype=l2var_dummy_byte

     !cldmask
     call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
          &          'cldmask',l2var_dummy_byte,dummy_unit,wo)            
     l2_input_2dice_primary%cldmask=l2var_dummy_byte

     !lusflag
     call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
          &          'lusflag',l2var_dummy_byte,dummy_unit,wo)            
     l2_input_2dice_primary%lusflag=l2var_dummy_byte

     !dem
!     call nc_read_array_2d_short_to_short_orac_pp(ncid,xdim,ydim, &
!          &          'dem',l2var_dummy_short,dummy_unit,wo)            
!     l2_input_2dice_primary%dem=l2var_dummy_short klhd

     !nisemask
     call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
          &          'nisemask',l2var_dummy_byte,dummy_unit,wo)            
     l2_input_2dice_primary%nisemask=l2var_dummy_byte

     !cccot_pre
     call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
          &                 'cccot_pre',l2var_dummy,dummy_unit,wo)
     l2_input_2dice_primary%cccot_pre=l2var_dummy


  endif


  !ctt           
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctt',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%ctt=l2var_dummy


  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctt_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%ctt_uncertainty=l2var_dummy


  !cth
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cth',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cth=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cth_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cth_uncertainty=l2var_dummy

  !ctp
  !write(*,*) 'read ctp'
  !pause
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%ctp=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%ctp_uncertainty=l2var_dummy

  !cct
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cc_total',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cct=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cc_total_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cct_uncertainty=l2var_dummy

  !cot
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cot',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cot=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cot_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cot_uncertainty=l2var_dummy


  !ref
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ref',l2var_dummy,dummy_unit,wo)      
  l2_input_2dice_primary%ref=l2var_dummy
  !write(*,*) 'ref',maxval(l2var_dummy),maxval(l2_input_2dice_primary%ref)
  !pause

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ref_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%ref_uncertainty=l2var_dummy



  !cwp
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cwp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cwp=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cwp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%cwp_uncertainty=l2var_dummy



   if (nf90_get_att(ncid, NF90_GLOBAL, "Sensor", global_atts%sensor) == &
        NF90_NOERR) then
   endif

write(*,*)' global_atts%sensor',  global_atts%sensor 
  !cloud_albedo
   if ( global_atts%sensor .eq. 'AATSR') then 
   chan_id=(/ 2,3 /)
   endif
   if ( global_atts%sensor .eq. 'MODIS') then
    chan_id=(/ 1,2 /)
    endif

   if ( global_atts%sensor .eq. 'AVHRR') then 
   chan_id=(/ 1,2 /)
   endif
   nchan=2

   do i=1,nchan
   write(input_num,"(i4)") chan_id(i)
write(*,*)'chan_id(i)',chan_id(i)
      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
write(*,*)'input_dummy cloud albedo',input_dummy
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          input_dummy,l2var_dummy,dummy_unit,wo)            
       l2_input_2dice_primary%cloud_albedo(:,:,i)=l2var_dummy
  enddo




  !write(*,*) 'read qc'
  !              pause
  call nc_read_array_2d_short_to_short_orac_pp(ncid,xdim,ydim, &
       &          'qcflag',l2var_dummy_short,dummy_unit,wo)            
  l2_input_2dice_primary%qcflag=l2var_dummy_short

  !stemp
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'stemp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%stemp=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'stemp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%stemp_uncertainty=l2var_dummy



  !niter
  call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
       &          'niter',l2var_dummy_byte,dummy_unit,wo)            
  l2_input_2dice_primary%niter=l2var_dummy_byte


  !convergence
  call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
       &          'convergence',l2var_dummy_byte,dummy_unit,wo)            
  l2_input_2dice_primary%convergence=l2var_dummy_byte

  !costs

  call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
       &          'costja',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%costja=l2var_dummy


  call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
       &          'costjm',l2var_dummy,dummy_unit,wo)            
  l2_input_2dice_primary%costjm=l2var_dummy

  deallocate(l2var_dummy)
  deallocate(l2var_dummy_double)
  deallocate(l2var_dummy_short)
  deallocate(l2var_dummy_byte)
!
!Read global attributes
!
 !----------------------------------------------------------------------------
   ! Set global_attributes structure
   !----------------------------------------------------------------------------


  


   ! Global attribute 'Conventions' as defined by CF-1.4, section 2.6.1.

    if (nf90_get_att(ncid, NF90_GLOBAL, "Conventions", global_atts%Conventions)== &
        NF90_NOERR) then
endif

write(*,*) 'read_inter_file global_atts%Conventions', global_atts%Conventions

   if (nf90_get_att(ncid, NF90_GLOBAL, "Institution", global_atts%institution) == &
        NF90_NOERR) then 
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "Source", global_atts%source) == &
        NF90_NOERR) then 
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "History", global_atts%history) == &
        NF90_NOERR) then
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "References", global_atts%references) == &
        NF90_NOERR) then 
   endif

   if (nf90_get_att(ncid, NF90_GLOBAL, "Comment", global_atts%comment) == &
        NF90_NOERR) then
   endif

   ! Extra global attributes defined by ORAC
   if (nf90_get_att(ncid, NF90_GLOBAL, "Project", global_atts%project) == &
        NF90_NOERR) then 
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "File_Name", global_atts%file_name) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "UUID", global_atts%UUID) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "NetCDF_Version", global_atts%NetCDF_Version) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Product_Name", global_atts%Product_Name) == &
        NF90_NOERR) then
   endif


!   temp_string=trim('year!!!')//trim('month!!!')//trim('day!!!')

   if (nf90_get_att(ncid, NF90_GLOBAL, "Product_date", global_atts%Date_Created) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Production_Time", global_atts%Production_Time) == &
        NF90_NOERR) then 
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "L2_Processor",global_atts%L2_Processor ) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "L2_Processor_Version", global_atts%L2_Processor_Version) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Platform", global_atts%platform) == &
        NF90_NOERR) then
   endif
   write(*,*)'platform',global_atts%platform


   if (nf90_get_att(ncid, NF90_GLOBAL, "AATSR_Processing_Version", global_atts%AATSR_Processing_Version) == &
        NF90_NOERR) then
   endif



   if (nf90_get_att(ncid, NF90_GLOBAL, "Contact_Email", global_atts%Creator_Email) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Contact_Website", global_atts%Creator_url) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Keywords", global_atts%keywords) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "Summary", global_atts%summary) == &
        NF90_NOERR) then
   endif


   if (nf90_get_att(ncid, NF90_GLOBAL, "License", global_atts%license) == &
        NF90_NOERR) then
   endif

write(*,*)'license',NF90_NOERR


  if (nf90_get_att(ncid, NF90_GLOBAL, "RTTOV_version", global_atts%rttov_version) == &
        NF90_NOERR) then
   endif

write(*,*)'rttov_Version', global_atts%rttov_version 


  if (nf90_get_att(ncid, NF90_GLOBAL, "ECMWF_version", global_atts%ecmwf_version) == &
        NF90_NOERR) then
   endif




  if (nf90_get_att(ncid, NF90_GLOBAL, "SVN_version", global_atts%svn_version) == &
        NF90_NOERR) then
   endif




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Read source attributes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (nf90_get_att(ncid, NF90_GLOBAL, "Albedo_file", source_atts%albedo_file) == &
        NF90_NOERR) then 
   endif
write(*,*)'albedo_file',source_atts%albedo_file,NF90_NOERR


  if (nf90_get_att(ncid, NF90_GLOBAL, "BRDF_file", source_atts%brdf_file) == &
        NF90_NOERR) then 
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Emissivity_file", source_atts%emissivity_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "USGS_file", source_atts%usgs_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Sea_Ice_file", source_atts%sea_ice_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Snow_file", source_atts%snow_file) == &
        NF90_NOERR) then
   endif


  if (nf90_get_att(ncid, NF90_GLOBAL, "Level1b_file", source_atts%level1b_file) == &
        NF90_NOERR) then
   endif



  if (nf90_get_att(ncid, NF90_GLOBAL, "Geo_file", source_atts%geo_file) == &
        NF90_NOERR) then
   endif


  !close  input file
  ierr=nf90_close(ncid)



END SUBROUTINE read_inter_file_ice


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_inter_file_water(iphase,fname,l2_input_2dwat_primary,xdim,ydim,global_atts,source_atts,wo,ierr)
!--------------------------------------------------
  use netcdf

  use vartypes_pp
  
  use structures_pp
  use global_attributes
  use source_attributes

  implicit none

   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts

 INTEGER,INTENT(IN) :: wo
  integer :: ivar,idim,ndim,nvar,nattr,dummyint,iphase
  integer :: ncid,ierr,ny=5,nx=5
  character(len=path_length) :: fname,name
   character(len=32)  :: input_num
   character(len=512) :: input_dummy

  INTEGER(kind=lint) ::  xdim,ydim,i

  CHARACTER(LEN=unitlength) :: dummy_unit
  type(l2_input_struct_2d_primary) :: l2_input_2dwat_primary

  real (kind=sreal), allocatable, dimension(:,:) :: l2var_dummy
  integer(kind=byte), allocatable, dimension(:,:) :: l2var_dummy_byte
  integer(kind=sint), allocatable, dimension(:,:) :: l2var_dummy_short
  integer(kind=lint) :: chan_id(2),nchan

  call nc_open_pp(ncid,fname,ierr,wo)
  call nc_info_pp(ncid,ndim,nvar,nattr,wo)

  allocate(l2var_dummy(xdim,ydim))
  l2var_dummy=sreal_fill_value
  
  allocate(l2var_dummy_short(xdim,ydim))
  l2var_dummy_short= sint_fill_value

  allocate(l2var_dummy_byte(xdim,ydim))
  l2var_dummy_byte=byte_fill_value
  

  !ctt           
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctt',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%ctt=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctt_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%ctt_uncertainty=l2var_dummy
  
  
  !cth
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cth',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cth=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cth_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cth_uncertainty=l2var_dummy
  
  !ctp

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%ctp=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%ctp_uncertainty=l2var_dummy
  
  !cct
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cc_total',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cct=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cc_total_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cct_uncertainty=l2var_dummy
  
  !cot
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
                   &          'cot',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cot=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cot_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cot_uncertainty=l2var_dummy
  
  
  !ref
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ref',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%ref=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ref_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%ref_uncertainty=l2var_dummy

  
  
  !cwp
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cwp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cwp=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cwp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cwp_uncertainty=l2var_dummy
 
!cloud_albedo


   if (nf90_get_att(ncid, NF90_GLOBAL, "Sensor", global_atts%sensor) == &
        NF90_NOERR) then
   endif

   if ( global_atts%sensor .eq. 'AATSR') then
    chan_id=(/ 2,3 /)
    endif
   if ( global_atts%sensor .eq. 'MODIS') then
    chan_id=(/ 1,2 /)
    endif
   if ( global_atts%sensor .eq. 'AVHRR') then
    chan_id=(/ 1,2 /)
    endif
   nchan=2

   do i=1,nchan
  write(input_num,"(i4)") chan_id(i)
      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          input_dummy,l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%cloud_albedo(:,:,i)=l2var_dummy
  enddo


  call nc_read_array_2d_short_to_short_orac_pp(ncid,xdim,ydim, &
       &          'qcflag',l2var_dummy_short,dummy_unit,wo)            
  l2_input_2dwat_primary%qcflag=l2var_dummy_short


  !stemp
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'stemp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%stemp=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'stemp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%stemp_uncertainty=l2var_dummy
  

  !niter
  call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
       &          'niter',l2var_dummy_byte,dummy_unit,wo)            
  l2_input_2dwat_primary%niter=l2var_dummy_byte


  !convergence
  call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
       &          'convergence',l2var_dummy_byte,dummy_unit,wo)            
  l2_input_2dwat_primary%convergence=l2var_dummy_byte

  !costs

  call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
       &          'costja',l2var_dummy,dummy_unit,wo)
  l2_input_2dwat_primary%costja=l2var_dummy


  call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
       &          'costjm',l2var_dummy,dummy_unit,wo)            
  l2_input_2dwat_primary%costjm=l2var_dummy

  
  deallocate(l2var_dummy)
  deallocate(l2var_dummy_short)
  deallocate(l2var_dummy_byte)
  
  
  !close  input file
  ierr=nf90_close(ncid)




END SUBROUTINE read_inter_file_water



!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_inter_file_mli(iphase,fname,l2_input_2dmli_primary,xdim,ydim,global_atts,source_atts,wo,ierr)
!--------------------------------------------------
  use netcdf

  use vartypes_pp
  
  use structures_pp
  use global_attributes
  use source_attributes

  implicit none

   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts

 INTEGER,INTENT(IN) :: wo
  integer :: ivar,idim,ndim,nvar,nattr,dummyint,iphase
  integer :: ncid,ierr,ny=5,nx=5
  character(len=path_length) :: fname,name
   character(len=32)  :: input_num
   character(len=512) :: input_dummy

  INTEGER(kind=lint) ::  xdim,ydim,i

  CHARACTER(LEN=unitlength) :: dummy_unit
  type(l2_input_struct_2d_primary) :: l2_input_2dmli_primary

  real (kind=sreal), allocatable, dimension(:,:) :: l2var_dummy
  integer(kind=byte), allocatable, dimension(:,:) :: l2var_dummy_byte
  integer(kind=sint), allocatable, dimension(:,:) :: l2var_dummy_short
  integer(kind=lint) :: chan_id(2),nchan

  call nc_open_pp(ncid,fname,ierr,wo)
  call nc_info_pp(ncid,ndim,nvar,nattr,wo)

  allocate(l2var_dummy(xdim,ydim))
  l2var_dummy=sreal_fill_value
  
  allocate(l2var_dummy_short(xdim,ydim))
  l2var_dummy_short= sint_fill_value

  allocate(l2var_dummy_byte(xdim,ydim))
  l2var_dummy_byte=byte_fill_value
  

  !ctt           
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctt',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%ctt=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctt_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%ctt_uncertainty=l2var_dummy
  
  
  !cth
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cth',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cth=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cth_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cth_uncertainty=l2var_dummy
  
  !ctp

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%ctp=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ctp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%ctp_uncertainty=l2var_dummy
  
  !cct
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cc_total',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cct=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cc_total_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cct_uncertainty=l2var_dummy
  
  !cot
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
                   &          'cot',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cot=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cot_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cot_uncertainty=l2var_dummy
  
  
  !ref
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ref',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%ref=l2var_dummy

  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'ref_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%ref_uncertainty=l2var_dummy

  
  
  !cwp
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cwp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cwp=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'cwp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cwp_uncertainty=l2var_dummy
 
!cloud_albedo


   if (nf90_get_att(ncid, NF90_GLOBAL, "Sensor", global_atts%sensor) == &
        NF90_NOERR) then
   endif

   if ( global_atts%sensor .eq. 'AATSR') then
    chan_id=(/ 2,3 /)
    endif
   if ( global_atts%sensor .eq. 'MODIS') then
    chan_id=(/ 1,2 /)
    endif
   if ( global_atts%sensor .eq. 'AVHRR') then
    chan_id=(/ 1,2 /)
    endif
   nchan=2

   do i=1,nchan
  write(input_num,"(i4)") chan_id(i)
      input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          input_dummy,l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%cloud_albedo(:,:,i)=l2var_dummy
  enddo


  call nc_read_array_2d_short_to_short_orac_pp(ncid,xdim,ydim, &
       &          'qcflag',l2var_dummy_short,dummy_unit,wo)            
  l2_input_2dmli_primary%qcflag=l2var_dummy_short


  !stemp
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'stemp',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%stemp=l2var_dummy
  
  call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
       &          'stemp_uncertainty',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%stemp_uncertainty=l2var_dummy
  

  !niter
  call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
       &          'niter',l2var_dummy_byte,dummy_unit,wo)            
  l2_input_2dmli_primary%niter=l2var_dummy_byte


  !convergence
  call nc_read_array_2d_byte_orac_pp(ncid,xdim,ydim, &
       &          'convergence',l2var_dummy_byte,dummy_unit,wo)            
  l2_input_2dmli_primary%convergence=l2var_dummy_byte

  !costs

  call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
       &          'costja',l2var_dummy,dummy_unit,wo)
  l2_input_2dmli_primary%costja=l2var_dummy


  call nc_read_array_2d_float_orac_pp(ncid,xdim,ydim, &
       &          'costjm',l2var_dummy,dummy_unit,wo)            
  l2_input_2dmli_primary%costjm=l2var_dummy
  
  deallocate(l2var_dummy)
  deallocate(l2var_dummy_short)
  deallocate(l2var_dummy_byte)
  
  
  !close  input file
  ierr=nf90_close(ncid)




END SUBROUTINE read_inter_file_mli


