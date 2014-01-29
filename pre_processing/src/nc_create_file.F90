!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE nc_create_file_rtm(script_input,cyear,chour,cminute,cmonth,cday,platform,sensor,path,&
     & wo,type,preproc_dims,imager_angles,netcdf_info,channel_info)
!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_file.f90
  !
  ! Created on:          12/12/11
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !   
  ! Modifications Log:    
  !2012/05/15: MJ modifies file to write pixels in one loop successievely one after the other
  !2012/05/24: MJ adds some commenting.
  !2012/08/02: MJ adds some more code for writing of RTTOV output to netcdf file.
  !2012/08/02: CP bug fix in outputtting of msi files removed nviews ddimension
  !2012/09/20: CP added solzen and satzen tp prtm output
  !2012/11/29: CP changed variables names from layers to levels
  !2013/02/26: Cp inserted missing comment for adding noaa platform 
  !2013/03/07: Cp added in some diagnostics q and albedo
  !2013 MJ adds PLATFORMUP varaible and output to comply with nomenclature.
! 2013/10/14: MJ fixed bug with writing of albedo and emissivity.
! 2013/11/06: MJ adds config file to preprocessing output which holds all relevant dimensional information.
!20131127 MJ changes output from netcdf3 to netcdf4.
  !
  ! $Id$
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------
  
  USE netcdf

  use preproc_constants

  use attribute_structures

  use preproc_structures

  use imager_structures

  use netcdf_structures

  use channel_structures

  IMPLICIT NONE
  
  ! Input
  INTEGER,INTENT(IN) :: wo!, ryr
  !ORG  INTEGER,INTENT(IN) :: time, nx, ny, grid, dx, dy,wo!, ryr
  CHARACTER(LEN=*),INTENT(IN) :: path
  

  
  ! Local
  INTEGER :: ierr
  
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
!  REAL(KIND=SINGLE):: lon(nx), lat(ny)

  integer :: type,cposition,clength

  character(len=platformlength) :: platform,PLATFORMUP
  character(len=sensorlength) :: sensor
  character(len=filelength) :: fname,ctitle

  character(len=datelength) :: cyear,chour,cminute,cmonth,cday

  type(script_arguments_s) :: script_input

  type(preproc_dims_s) :: preproc_dims

  type(imager_angles_s) :: imager_angles

  type(netcdf_info_s) :: netcdf_info

  type(channel_info_s) :: channel_info

  INTEGER :: ncid

  ! End of header ----------------------------------------------------------

  !open stuff related to LW
  if(type .eq. 1 ) then 

     !MJ ORG ierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_lwrtm)
     !NETCDF4
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL),netcdf_info%ncid_lwrtm)
     write(*,*)'path lw',path
     IF (ierr.NE.NF90_NOERR)  stop 'error lw creating file'

     ctitle='ORAC Preprocessing lwrtm output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_lwrtm)

     !define horizontal dimension as one big vector containing all pixels
     ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlon_x_nlat_lwrtm', NF90_UNLIMITED,  netcdf_info%xydim_lw)
     IF (ierr.NE.NF90_NOERR) STOP 'create xy-d 2'

     !defone lon and lat just for reference
     ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlon_lwrtm',&
          & preproc_dims%preproc_max_lon-preproc_dims%preproc_min_lon+1,&
          & netcdf_info%xdim_lw)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d'
     
     ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlat_lwrtm',&
          & preproc_dims%preproc_max_lat-preproc_dims%preproc_min_lat+1,&
          & netcdf_info%ydim_lw)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d'

     !layer land level dimension
     ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlayers_lwrtm',preproc_dims%kdim_pre-1, netcdf_info%layerdim_lw)
     IF (ierr.NE.NF90_NOERR) STOP 'create nlay lw'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlevels_lwrtm',preproc_dims%kdim_pre, netcdf_info%leveldim_lw)
     IF (ierr.NE.NF90_NOERR) STOP 'create nlev lw'


     !lw channel dimension
     ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlw_channels',channel_info%nchannels_lw, netcdf_info%lwchanneldim)
     IF (ierr.NE.NF90_NOERR) STOP 'create nchan lw'

     !define channel ids abs
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'lw_channel_abs_ids',&
          & NF90_INT, netcdf_info%lwchanneldim, netcdf_info%channels_id_lw)
     IF (ierr.NE.NF90_NOERR) STOP 'def channels lw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%channels_id_lw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue lw channel ids'

     !define channel ids instr
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'lw_channel_instr_ids',&
          & NF90_INT, netcdf_info%lwchanneldim, netcdf_info%channels_id_instr_lw) 
     IF (ierr.NE.NF90_NOERR) STOP 'def channels lw instr'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%channels_id_instr_lw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue lw channel ids instr'


     !define channel wavenumbers
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'lw_channel_wvl', NF90_FLOAT, &
          & netcdf_info%lwchanneldim, netcdf_info%wvn_id_lw) 
     IF (ierr.NE.NF90_NOERR) STOP 'def channels lw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%wvn_id_lw, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var chennel wvn FillValue'


     !lw viewing dimension
     ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nviews',imager_angles%nviews, netcdf_info%viewdim_lw)
     IF (ierr.NE.NF90_NOERR) STOP 'create nv lw'

     !combines viewing xy dims
     netcdf_info%xyvdim_lw(1)=netcdf_info%viewdim_lw
     netcdf_info%xyvdim_lw(2)=netcdf_info%xydim_lw


     !define counter variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'counter_lw', NF90_INT,&
          & netcdf_info%xydim_lw, netcdf_info%counterid_lw,&
          & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def counter lw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%counterid_lw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var clw FillValue'

     !define solar zenith
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'solza_lw', NF90_FLOAT,&
          &  netcdf_info%xyvdim_lw, netcdf_info%solzaid_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def solza_lw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,netcdf_info%solzaid_lw, '_FillValue',real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var solza_lw FillValue'

     !define satellite zenith
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'satza_lw', NF90_FLOAT,&
          &  netcdf_info%xyvdim_lw, netcdf_info%satzaid_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def satza_lw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,netcdf_info%satzaid_lw, '_FillValue',real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var satza_lw FillValue'

     !define rez azimuth
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'relazi_lw', NF90_FLOAT,&
          &  netcdf_info%xyvdim_lw, netcdf_info%relaziid_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def relazi_lw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,netcdf_info%relaziid_lw, '_FillValue',real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var relazi_lw FillValue'
   

     !define longitude variable
!!$     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'lon_lw', NF90_FLOAT, netcdf_info%xydim_lw, netcdf_info%lonid_lw) 
!!$     IF (ierr.NE.NF90_NOERR) STOP 'def lon lw'
!!$     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%lonid_lw, '_FillValue', real_fill_value )
!!$     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue lon'

     !define latitude variable
!!$     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lwrtm, 'lat_lw', NF90_FLOAT, netcdf_info%xydim_lw, netcdf_info%latid_lw) 
!!$     IF (ierr.NE.NF90_NOERR) STOP 'def lat lw'
!!$     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%latid_pw, '_FillValue',  real_fill_value )
!!$     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue lat'


     !set up the combined dimensions for 3D fields (spatial+channel)
     netcdf_info%xycdim_lw(1)=netcdf_info%lwchanneldim
     netcdf_info%xycdim_lw(2)=netcdf_info%xydim_lw

     
     !define emissivity 3D
     ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'emiss_lw',&
          & NF90_FLOAT, netcdf_info%xycdim_lw, netcdf_info%emiss_id_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lw emiss'     
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%emiss_id_lw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !set up the combined dimensions for 4D fields (c-z-xy)
     netcdf_info%xyzcdim_lw(1)=netcdf_info%lwchanneldim
     netcdf_info%xyzcdim_lw(2)=netcdf_info%leveldim_lw
     netcdf_info%xyzcdim_lw(3)=netcdf_info%xydim_lw



     !define tac profile at level centers as variable
     ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'tac_lw',&
          & NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%tac_id_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lw tac'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%tac_id_lw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     !define tbc
     ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'tbc_lw',&
          & NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%tbc_id_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lw tbc'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%tbc_id_lw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     !define radiances
     ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'rbc_up_lw',&
          & NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rbc_up_id_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lw rbc_up'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%rbc_up_id_lw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'rac_up_lw',&
          & NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rac_up_id_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lw rac_up'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%rac_up_id_lw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'rac_down_lw',&
          & NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rac_down_id_lw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lw rac_down'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,  netcdf_info%rac_down_id_lw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !open stuff related to SW
  elseif(type .eq. 2) then

     ctitle='ORAC Preprocessing swrtm output file'
     
     !create file
     !MJ ORG ierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_swrtm)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL),netcdf_info%ncid_swrtm)
     IF (ierr.NE.NF90_NOERR)  stop 'error creating sw file'


     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_swrtm)

     !define horizontal dimension as one big vector containing all pixels
     ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlon_x_nlat_swrtm', NF90_UNLIMITED,  netcdf_info%xydim_sw)
     IF (ierr.NE.NF90_NOERR) STOP 'create xy-d 2'


     !defone lon and lat just for reference
     ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlon_swrtm',&
          & preproc_dims%preproc_max_lon-preproc_dims%preproc_min_lon+1,&
          & netcdf_info%xdim_sw)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d'
     
     ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlat_swrtm',&
          & preproc_dims%preproc_max_lat-preproc_dims%preproc_min_lat+1,&
          & netcdf_info%ydim_sw)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d'


     !layer land level dimension
     ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlayers_swrtm',preproc_dims%kdim_pre-1, netcdf_info%layerdim_sw)
     IF (ierr.NE.NF90_NOERR) STOP 'create sw layer'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlevels_swrtm',preproc_dims%kdim_pre, netcdf_info%leveldim_sw)
     IF (ierr.NE.NF90_NOERR) STOP 'create sw level'


     !sw channel dimension
     ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nsw_channels',channel_info%nchannels_sw, netcdf_info%swchanneldim)
     IF (ierr.NE.NF90_NOERR) STOP 'create sw channel'

     !define channel ids abs
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'sw_channel_abs_ids',&
          & NF90_INT, netcdf_info%swchanneldim, netcdf_info%channels_id_sw) 
     IF (ierr.NE.NF90_NOERR) STOP 'def channels sw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%channels_id_sw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue sw channel ids'

     !define channel ids instr
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'sw_channel_instr_ids',&
          & NF90_INT, netcdf_info%swchanneldim, netcdf_info%channels_id_instr_sw) 
     IF (ierr.NE.NF90_NOERR) STOP 'def channels sw instr'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%channels_id_instr_sw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue sw channel ids instr'


     !define channel wavenumbers
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'sw_channel_wvl', NF90_FLOAT, &
          & netcdf_info%swchanneldim, netcdf_info%wvn_id_sw) 
     IF (ierr.NE.NF90_NOERR) STOP 'def channels sw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%wvn_id_sw, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var chennel wvn FillValue'


     !sw viewing dimension
     ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nviews',imager_angles%nviews, netcdf_info%viewdim_sw)
     IF (ierr.NE.NF90_NOERR) STOP 'create sw views'
    
     !combines viewing xy dims
     netcdf_info%xyvdim_sw(1)=netcdf_info%viewdim_sw
     netcdf_info%xyvdim_sw(2)=netcdf_info%xydim_sw


     !define solar zenith
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'solza_sw', NF90_FLOAT,&
          &  netcdf_info%xyvdim_sw, netcdf_info%solzaid_sw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def solza_sw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,netcdf_info%solzaid_sw, '_FillValue',real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var solza_sw FillValue'

     !define satellite zenith
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'satza_sw', NF90_FLOAT,&
          &  netcdf_info%xyvdim_sw, netcdf_info%satzaid_sw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def satza_sw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,netcdf_info%satzaid_sw, '_FillValue',real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var satza_sw FillValue'

     !define rez azimuth
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'relazi_sw', NF90_FLOAT,&
          &  netcdf_info%xyvdim_sw, netcdf_info%relaziid_sw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def relazi_sw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,netcdf_info%relaziid_sw, '_FillValue',real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var relazi_sw FillValue'
   



     !define counter variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'counter_sw', NF90_INT,&
          & netcdf_info%xydim_sw, netcdf_info%counterid_sw,&
          & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def counter sw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%counterid_sw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var csw FillValue'


     !define longitude variable
!!$     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'lon_sw', NF90_FLOAT, netcdf_info%xydim_sw, netcdf_info%lonid_sw) 
!!$     IF (ierr.NE.NF90_NOERR) STOP 'def lon sw'
!!$     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%lonid_sw, '_FillValue', real_fill_value )
!!$     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue lon'

     !define latitude variable
!!$     ierr = NF90_DEF_VAR ( netcdf_info%ncid_swrtm, 'lat_sw', NF90_FLOAT, netcdf_info%xydim_sw, netcdf_info%latid_sw) 
!!$     IF (ierr.NE.NF90_NOERR) STOP 'def lat sw'
!!$     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%latid_pw, '_FillValue',  real_fill_value )
!!$     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue lat'

     !set up the combined dimensions for 4D fields
     netcdf_info%xyzcdim_sw(1)=netcdf_info%swchanneldim
     netcdf_info%xyzcdim_sw(2)=netcdf_info%leveldim_sw
     netcdf_info%xyzcdim_sw(3)=netcdf_info%xydim_sw

     !define tac profile at level centers as variable
     ierr = NF90_DEF_VAR( netcdf_info%ncid_swrtm, 'tac_sw',&
          & NF90_FLOAT, netcdf_info%xyzcdim_sw, netcdf_info%tac_id_sw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def sw tac'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%tac_id_sw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     ierr = NF90_DEF_VAR( netcdf_info%ncid_swrtm, 'tbc_sw',&
          & NF90_FLOAT, netcdf_info%xyzcdim_sw, netcdf_info%tbc_id_sw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def sw tbc'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,  netcdf_info%tbc_id_sw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     !open stuff related to meteo
  elseif(type .eq. 3) then

     !create file
     !MJ ORGierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_prtm)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL),netcdf_info%ncid_prtm)
     IF (ierr.NE.NF90_NOERR)  stop 'error creating p file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_prtm)
     
     !define horizontal dimension as one big vector containing all pixels
     ierr = NF90_DEF_DIM(netcdf_info%ncid_prtm, 'nlon_x_nlat_prtm', NF90_UNLIMITED,  netcdf_info%xydim_pw)
     IF (ierr.NE.NF90_NOERR) STOP 'create xy-d 3'

     !defone lon and lat just for reference
     ierr = NF90_DEF_DIM(netcdf_info%ncid_prtm, 'nlon_prtm',&
          & preproc_dims%preproc_max_lon-preproc_dims%preproc_min_lon+1,&
          & netcdf_info%xdim_pw)
     IF (ierr.NE.NF90_NOERR) STOP 'create xy-d'
     
     ierr = NF90_DEF_DIM(netcdf_info%ncid_prtm, 'nlat_prtm',&
          & preproc_dims%preproc_max_lat-preproc_dims%preproc_min_lat+1,&
          & netcdf_info%ydim_pw)
     IF (ierr.NE.NF90_NOERR) STOP 'create xy-d'

     !layer land level dimension
     ierr = NF90_DEF_DIM( netcdf_info%ncid_prtm, 'nlayers_prtm',preproc_dims%kdim_pre-1, netcdf_info%layerdim_pw)
     IF (ierr.NE.NF90_NOERR) STOP 'create nlay prtm'

     ierr = NF90_DEF_DIM( netcdf_info%ncid_prtm, 'nlevels_prtm',preproc_dims%kdim_pre, netcdf_info%leveldim_pw)
     IF (ierr.NE.NF90_NOERR) STOP 'create nlev prtm'

!DO THIS RATHER VIA THE GLOBAL ATTRIBUTES, same for instrument
!!$
!!$     !date dimension
!!$     ierr = NF90_DEF_DIM( netcdf_info%ncid_prtm, 'ndate_prtm',1, netcdf_info%datedim_pw)
!!$     IF (ierr.NE.NF90_NOERR) STOP 'create date-d'
!!$
!!$     !define date variable
!!$     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'date', NF90_INT, netcdf_info%datedim_pw, netcdf_info%date_id_pw) 
!!$     IF (ierr.NE.NF90_NOERR) STOP 'def date'
!!$     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%date_id_pw, '_FillValue',long_int_fill_value )
!!$     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     !define i variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'i_pw', NF90_INT, &
          & netcdf_info%xydim_pw, netcdf_info%iid_pw,&
          & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def i'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%iid_pw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var i FillValue'

     !define j variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'j_pw', NF90_INT,&
          & netcdf_info%xydim_pw, netcdf_info%jid_pw,&
          & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def j'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%jid_pw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var j FillValue'

     !define counter variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'counter_pw', NF90_INT,&
          & netcdf_info%xydim_pw, netcdf_info%counterid_pw,&
          & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def counter pw'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%counterid_pw, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var i FillValue'



     !define longitude variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'lon_pw', NF90_FLOAT,&
          & netcdf_info%xydim_pw, netcdf_info%lonid_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)) 
     IF (ierr.NE.NF90_NOERR) STOP 'def lon'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%lonid_pw, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     !define latitude variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'lat_pw', NF90_FLOAT,&
          & netcdf_info%xydim_pw, netcdf_info%latid_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lat'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%latid_pw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'




     !define skint variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'skint_pw', NF90_FLOAT,&
          & netcdf_info%xydim_pw, netcdf_info%skintid_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def skint'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%skintid_pw, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !define exp(lnsp) variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'explnsp_pw', NF90_FLOAT,&
          & netcdf_info%xydim_pw, netcdf_info%lnspid_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def explnsp'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%lnspid_pw, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !define exp(lsf) variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'lsf_pw', NF90_FLOAT,&
          & netcdf_info%xydim_pw, netcdf_info%lsfid_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lsf'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%lsfid_pw, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !define satzen variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'satzen_pw', NF90_FLOAT,&
          & netcdf_info%xydim_pw, netcdf_info%satzenid_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def satzen'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%satzenid_pw, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     !define solzen variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_prtm, 'solzen_pw', NF90_FLOAT,&
          & netcdf_info%xydim_pw, netcdf_info%solzenid_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def solzen'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%solzenid_pw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !set up the combined dimensions for 3D fields
     netcdf_info%xyzdim_pw(1)=netcdf_info%leveldim_pw
     netcdf_info%xyzdim_pw(2)=netcdf_info%xydim_pw

     !define pressure profile at level centers as variable
     ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'pprofile_lev',&
          & NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%pprofile_lev_id_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lat'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%pprofile_lev_id_pw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'

     !define temperature profile at lever centers as variable
     ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'tprofile_lev',&
          & NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%tprofile_lev_id_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lat'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%tprofile_lev_id_pw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !define spec_hum profile at lever centers as variable
!     ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'qprofile_lev',&
!          & NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%qprofile_lev_id_pw) 
!     IF (ierr.NE.NF90_NOERR) STOP 'def lat'
!     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%qprofile_lev_id_pw!, '_FillValue',  real_fill_value )
!     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     !define geopotential height profile at lever centers as variable
     ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'gphprofile_lev',&
          & NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%hprofile_lev_id_pw,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def lat'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm,  netcdf_info%hprofile_lev_id_pw, '_FillValue',  real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'


     ctitle='ORAC Preprocessing prtm output file'
  
  endif


  
    
  !set up attributes common to all output files
  if(type .eq. 1) ncid=netcdf_info%ncid_lwrtm
  if(type .eq. 2) ncid=netcdf_info%ncid_swrtm
  if(type .eq. 3) ncid=netcdf_info%ncid_prtm
  
!  write(*,*) type,ncid,ctitle

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Title',trim(adjustl(ctitle)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions title'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Project',trim(adjustl(script_input%project)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',trim(adjustl(script_input%cncver)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CF_Convention_Version',trim(adjustl(script_input%ccon)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processing_Institution',trim(adjustl(script_input%cinst)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor',trim(adjustl(script_input%l2cproc)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor_Version',trim(adjustl(script_input%l2cprocver)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  
  !MJ ORG if(platform(1:4) .eq. 'noaa') PLATFORM(1:4)='NOAA'
  !MST following line by MST
  PLATFORMUP=platform
  if(platform(1:4) .eq. 'noaa') PLATFORMUP(1:4)='NOAA'


  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Platform',trim(adjustl(platformup)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Sensor_Name',trim(adjustl(sensor)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'uuid',trim(adjustl(script_input%uuid_tag)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cposition=index(trim(adjustl(path)),'/',back=.true.)
  clength=len_trim(adjustl(path))
!  fname=trim(adjustl(path(cposition+1:clength)))
  fname=trim(adjustl(path))
!  write(*,*) cposition,clength
!  write(*,*) trim(adjustl(path))
!  write(*,*) trim(adjustl(fname))
!  write(*,*) F90MAXNCNAM

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Name',trim(adjustl(fname(cposition+1:clength))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Email',trim(adjustl(script_input%contact)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Website',trim(adjustl(script_input%website)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Production_Time',trim(adjustl(script_input%exec_time)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date',&
       & trim(adjustl(trim(adjustl(cyear))//trim(adjustl(cmonth))//&
       & trim(adjustl(cday))//trim(adjustl(chour))//trim(adjustl(cminute)))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Reference',trim(adjustl(script_input%reference)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'History',trim(adjustl(script_input%history)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Summary',trim(adjustl(script_input%summary)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Keywords',trim(adjustl(script_input%keywords)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Comment',trim(adjustl(script_input%comment)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'License',trim(adjustl(script_input%license)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

!!$
!!$
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Title',ctitle)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Project',trim(adjustl( project)))
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  cncver='3.6.3'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',cncver)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  ccon='CF-1.4'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CF_Convention_Version',ccon)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  cinst='CMSAF!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processing_Institution',cinst)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  cproc='ORAC!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processed_with',cproc)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  cprocver='1.0!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processor_Version',cprocver)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  csname='satellite name!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Satellite_Name',csname)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  csid='satellite id!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Satellite_ID',csid)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  cuuid='uuid tag!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'uuid',cuuid)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  instname='instrument name!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Instrument_Name',instname)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  fname='file name!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Name',fname)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  contact='contact email!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Email',contact)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  website='contact website!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Website',website)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  prodtime='production time!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Production_Time',prodtime)
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  prod_name='prod_name!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Name',trim(adjustl(prod_name)))
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$  year='year!!!'
!!$  month='month!!!'
!!$  day='day!!!'
!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date',&
!!$       & trim(adjustl(trim(adjustl(year))//trim(adjustl(month))//trim(adjustl(day)))))
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!!$

  !close definition section
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error enddef rtm'
  !



  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF
  
  RETURN
  
END SUBROUTINE nc_create_file_rtm

!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE nc_create_file_swath(script_input,cyear,chour,cminute,cmonth,cday,platform,sensor,path,&
     & wo,type,imager_geolocation,imager_angles,netcdf_info,channel_info)
!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_file.f90
  !
  ! Created on:          12/12/11
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !   
  ! Modifications Log:    
  !2012/05/31: MJ initial routine version.
  !2012/07/04: CP removed nviews dimension of data

  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------
  
  USE netcdf

  use preproc_constants

  use attribute_structures

  use preproc_structures

  use imager_structures

  use netcdf_structures

  use channel_structures

  IMPLICIT NONE


  
  ! Input
  INTEGER,INTENT(IN) :: wo!, ryr
  !ORG  INTEGER,INTENT(IN) :: time, nx, ny, grid, dx, dy,wo!, ryr
  CHARACTER(LEN=*),INTENT(IN) :: path
  

  INTEGER :: ncid

  
  
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
!  REAL(KIND=SINGLE):: lon(nx), lat(ny)

  integer :: type,cposition,clength,ierr

  integer, dimension(2) :: dims2d
  integer, dimension(3) :: dims3d
  integer, dimension(3) :: dims3dd
  
  character(len=platformlength) :: platform,platformup
  character(len=sensorlength) :: sensor
  character(len=filelength) :: fname,ctitle

  character(len=datelength) :: cyear,chour,cminute,cmonth,cday

  type(script_arguments_s) :: script_input

  type(imager_angles_s) :: imager_angles

  type(imager_geolocation_s) :: imager_geolocation

  type(netcdf_info_s) :: netcdf_info

  type(channel_info_s) :: channel_info

  ! End of header ----------------------------------------------------------

  !open stuff related to msi
  if(type .eq. 1 ) then 

     !MJ ORGierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_msi)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_msi)
     IF (ierr.NE.NF90_NOERR)  stop 'error msi creating file'

     ctitle='ORAC Preprocessing msi output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_msi)


     !defone x and y
     ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'nx_msi',&
          & imager_geolocation%endx-imager_geolocation%startx+1,&
          & netcdf_info%xdim_msi)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d msi'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'ny_msi',&
          & imager_geolocation%endy-imager_geolocation%starty+1,&
          & netcdf_info%ydim_msi)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d msi'

     !define nviews
!     ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'nv_msi',&
!          & imager_angles%nviews,&
!          & netcdf_info%vdim_msi)
!     IF (ierr.NE.NF90_NOERR) STOP 'create v-d msi'

     !define nchannels
     ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'nc_msi',&
          & channel_info%nchannels_total,&
          & netcdf_info%cdim_msi)
     IF (ierr.NE.NF90_NOERR) STOP 'create c-d msi'

     !define some channel variables
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'msi_instr_ch_numbers', NF90_INT, netcdf_info%cdim_msi,&
          netcdf_info%channelninid) 
     IF (ierr.NE.NF90_NOERR) STOP 'def msi channel n'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%channelninid, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue msi channel n'


     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'msi_abs_ch_numbers', NF90_INT, netcdf_info%cdim_msi,&
          netcdf_info%channelnabsid) 
     IF (ierr.NE.NF90_NOERR) STOP 'def msi channel n abs'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%channelnabsid, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue msi channel n abs'

     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'msi_abs_ch_wl', NF90_FLOAT, netcdf_info%cdim_msi,&
          netcdf_info%channelwlabsid) 
     IF (ierr.NE.NF90_NOERR) STOP 'def msi channel wl abs'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%channelwlabsid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue msi channel wl abs'

     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'msi_ch_swflag', NF90_INT, netcdf_info%cdim_msi,&
          netcdf_info%channelswflag) 
     IF (ierr.NE.NF90_NOERR) STOP 'def msi channel swf'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%channelswflag, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue msi channel swf'


     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'msi_ch_lwflag', NF90_INT, netcdf_info%cdim_msi,&
          netcdf_info%channellwflag) 
     IF (ierr.NE.NF90_NOERR) STOP 'def msi channel lwf'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%channellwflag, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue msi channel lwf'

     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'msi_ch_procflag', NF90_INT, netcdf_info%cdim_msi,&
          netcdf_info%channelprocflag) 
     IF (ierr.NE.NF90_NOERR) STOP 'def msi channel proc'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%channelprocflag, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue msi channel proc'

     dims2d(1)=netcdf_info%xdim_msi
     dims2d(2)=netcdf_info%ydim_msi
     !define time variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'time_data', NF90_DOUBLE, dims2d, netcdf_info%timeid,&
          & deflate_level=compress_level_double,shuffle=shuffle_double)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def time'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%timeid, '_FillValue', double_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue time'



     dims3dd(1)=netcdf_info%xdim_msi
     dims3dd(2)=netcdf_info%ydim_msi
     dims3dd(3)=netcdf_info%cdim_msi
 

     !define msi variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_msi, 'msi_data', NF90_FLOAT, dims3dd, netcdf_info%msid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def msi'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_msi,  netcdf_info%msid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue msi'

     !open cloud flag file
  elseif(type .eq. 2) then

     !MJ ORGierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_cf)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_cf)
     IF (ierr.NE.NF90_NOERR)  stop 'error cf creating file'

     ctitle='ORAC Preprocessing cf output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_cf)


     !define x and y
     ierr = NF90_DEF_DIM(netcdf_info%ncid_cf, 'nx_cf',&
          & imager_geolocation%endx-imager_geolocation%startx+1,&
          & netcdf_info%xdim_cf)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d cf'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_cf, 'ny_cf',&
          & imager_geolocation%endy-imager_geolocation%starty+1,&
          & netcdf_info%ydim_cf)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d cf'

     dims2d(1)=netcdf_info%xdim_cf
     dims2d(2)=netcdf_info%ydim_cf

     !define cf variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_cf, 'cflag', NF90_BYTE, dims2d, netcdf_info%cfid) 
     IF (ierr.NE.NF90_NOERR) STOP 'def cf'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_cf,  netcdf_info%cfid, '_FillValue', byte_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue cf'


     !open land/sea flag file
  elseif(type .eq. 3) then

     !MJ ORGierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_lsf)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_lsf)
     IF (ierr.NE.NF90_NOERR)  stop 'error lsf creating file'

     ctitle='ORAC Preprocessing lsf output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_lsf)

     !define x and y
     ierr = NF90_DEF_DIM(netcdf_info%ncid_lsf, 'nx_lsf',&
          & imager_geolocation%endx-imager_geolocation%startx+1,&
          & netcdf_info%xdim_lsf)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d lsf'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_lsf, 'ny_lsf',&
          & imager_geolocation%endy-imager_geolocation%starty+1,&
          & netcdf_info%ydim_lsf)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d lsf'

     dims2d(1)=netcdf_info%xdim_lsf
     dims2d(2)=netcdf_info%ydim_lsf

     !define lsf variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_lsf, 'lsflag', NF90_BYTE, dims2d, netcdf_info%lsfid) 
     IF (ierr.NE.NF90_NOERR) STOP 'def lsf'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_lsf,  netcdf_info%lsfid, '_FillValue', byte_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue lsf'


     !open geometry file
  elseif(type .eq. 4) then

     !MJ ORG ierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_geo)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_geo)
     IF (ierr.NE.NF90_NOERR)  stop 'error geo creating file'

     ctitle='ORAC Preprocessing geo output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_geo)

     !define x and y
     ierr = NF90_DEF_DIM(netcdf_info%ncid_geo, 'nx_geo',&
          & imager_geolocation%endx-imager_geolocation%startx+1,&
          & netcdf_info%xdim_geo)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d geo'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_geo, 'ny_geo',&
          & imager_geolocation%endy-imager_geolocation%starty+1,&
          & netcdf_info%ydim_geo)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d geo'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_geo, 'nv_geo',&
          & imager_angles%nviews,&
          & netcdf_info%vdim_geo)
     IF (ierr.NE.NF90_NOERR) STOP 'create v-d geo'




     dims3d(1)=netcdf_info%xdim_geo
     dims3d(2)=netcdf_info%ydim_geo
     dims3d(3)=netcdf_info%vdim_geo
     !define solzen variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_geo, 'solzen', NF90_FLOAT, dims3d, netcdf_info%solzenid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def solzen'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_geo,  netcdf_info%solzenid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue solzen'

     !define satzen variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_geo, 'satzen', NF90_FLOAT, dims3d, netcdf_info%satzenid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def satzen'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_geo,  netcdf_info%satzenid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue satzen'

     !define solaz variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_geo, 'solaz', NF90_FLOAT, dims3d, netcdf_info%solazid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def solaz'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_geo,  netcdf_info%solazid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue solaz'



!     !define senazi variable
!     ierr = NF90_DEF_VAR ( netcdf_info%ncid_geo, 'senaz', NF90_FLOAT, dims3d, netcdf_info%senazid) 
!     IF (ierr.NE.NF90_NOERR) STOP 'def senaz'
!     ierr = NF90_PUT_ATT(netcdf_info%ncid_geo,  netcdf_info%senazid, '_FillValue', real_fill_value )
!     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue senazi'


     !define relazi variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_geo, 'relazi', NF90_FLOAT, dims3d, netcdf_info%relazid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def relazi'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_geo,  netcdf_info%relazid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue relazi'



     !open location file
  elseif(type .eq. 5) then

     !MJ ORGierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_loc)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_loc)
     IF (ierr.NE.NF90_NOERR)  stop 'error loc creating file'

     ctitle='ORAC Preprocessing loc output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_loc)

     !define x and y
     ierr = NF90_DEF_DIM(netcdf_info%ncid_loc, 'nx_loc',&
          & imager_geolocation%endx-imager_geolocation%startx+1,&
          & netcdf_info%xdim_loc)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d loc'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_loc, 'ny_loc',&
          & imager_geolocation%endy-imager_geolocation%starty+1,&
          & netcdf_info%ydim_loc)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d loc'

     dims2d(1)=netcdf_info%xdim_loc
     dims2d(2)=netcdf_info%ydim_loc

     !define lon variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_loc, 'lon', NF90_FLOAT, dims2d, netcdf_info%lonid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def loc'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_loc,  netcdf_info%lonid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue loc'

     !define lat variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_loc, 'lat', NF90_FLOAT, dims2d, netcdf_info%latid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def loc'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_loc,  netcdf_info%latid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue loc'


  !open stuff related to albedo
  elseif(type .eq. 6 ) then 

     !MJ ORGierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_alb)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_alb)
     IF (ierr.NE.NF90_NOERR)  stop 'error alb creating file'

     ctitle='ORAC Preprocessing alb output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_alb)

     !defone x and y
     ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'nx_alb',&
          & imager_geolocation%endx-imager_geolocation%startx+1,&
          & netcdf_info%xdim_alb)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d alb'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'ny_alb',&
          & imager_geolocation%endy-imager_geolocation%starty+1,&
          & netcdf_info%ydim_alb)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d alb'

     !define nchannels albedo
     ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'nc_alb',&
          & channel_info%nchannels_sw,&
          & netcdf_info%cdim_alb)
     IF (ierr.NE.NF90_NOERR) STOP 'create c-d alb'

     !define nchannels emissivity
     ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'nc_emis',&
          & channel_info%nchannels_lw,&
          & netcdf_info%cdim_emis)
     IF (ierr.NE.NF90_NOERR) STOP 'create c-d emi'


     !MJ OLD ierr = NF90_DEF_VAR ( netcdf_info%ncid_alb, 'alb_abs_ch_numbers', NF90_INT, netcdf_info%cdim_alb,&
     !MJ OLD netcdf_info%channelnabsid) 
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_alb, 'alb_abs_ch_numbers', NF90_INT, netcdf_info%cdim_alb,&
          netcdf_info%channelnalbid) 
     IF (ierr.NE.NF90_NOERR) STOP 'def alb channel n abs'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_alb,  netcdf_info%channelnalbid, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue alb channel n abs'

     ierr = NF90_DEF_VAR ( netcdf_info%ncid_alb, 'emis_abs_ch_numbers', NF90_INT, netcdf_info%cdim_emis,&
          netcdf_info%channelnemisid) 
     IF (ierr.NE.NF90_NOERR) STOP 'def emis channel n abs'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_alb,  netcdf_info%channelnemisid, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue emis channel n abs'



 
     dims3d(1)=netcdf_info%xdim_msi
     dims3d(2)=netcdf_info%ydim_msi
     dims3d(3)=netcdf_info%cdim_alb

     !define alb variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_alb, 'alb_data', NF90_FLOAT, dims3d, netcdf_info%albid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def alb'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_alb,  netcdf_info%albid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue alb'

 
     dims3d(1)=netcdf_info%xdim_msi
     dims3d(2)=netcdf_info%ydim_msi
     dims3d(3)=netcdf_info%cdim_emis

     !define emis variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_alb, 'emis_data', NF90_FLOAT, dims3d, netcdf_info%emisid,&
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def emis'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_alb,  netcdf_info%emisid, '_FillValue', real_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue emis'

  elseif(type .eq. 7) then

     !MJ ORG ierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_scan)
     ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_scan)
     IF (ierr.NE.NF90_NOERR)  stop 'error scan creating file'

     ctitle='ORAC Preprocessing scan output file'

     !start defining things
     ierr = NF90_REDEF(netcdf_info%ncid_scan)

     !define x and y
     ierr = NF90_DEF_DIM(netcdf_info%ncid_scan, 'nx_scan',&
          & imager_geolocation%endx-imager_geolocation%startx+1,&
          & netcdf_info%xdim_scan)
     IF (ierr.NE.NF90_NOERR) STOP 'create x-d scan'

     ierr = NF90_DEF_DIM(netcdf_info%ncid_scan, 'ny_scan',&
          & imager_geolocation%endy-imager_geolocation%starty+1,&
          & netcdf_info%ydim_scan)
     IF (ierr.NE.NF90_NOERR) STOP 'create y-d scan'

     dims2d(1)=netcdf_info%xdim_scan
     dims2d(2)=netcdf_info%ydim_scan

     !define u variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_scan, 'uscan', NF90_INT, dims2d, netcdf_info%uscanid,&
          & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def scan u'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_scan,  netcdf_info%uscanid, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue u scan'


     !define v variable
     ierr = NF90_DEF_VAR ( netcdf_info%ncid_scan, 'vscan', NF90_INT, dims2d, netcdf_info%vscanid,&
          & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
     IF (ierr.NE.NF90_NOERR) STOP 'def scan v'
     ierr = NF90_PUT_ATT(netcdf_info%ncid_scan,  netcdf_info%vscanid, '_FillValue', long_int_fill_value )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue v scan'

     
  endif
  
    
  !set up attributes common to all output files
  if(type .eq. 1) ncid=netcdf_info%ncid_msi
  if(type .eq. 2) ncid=netcdf_info%ncid_cf
  if(type .eq. 3) ncid=netcdf_info%ncid_lsf
  if(type .eq. 4) ncid=netcdf_info%ncid_geo
  if(type .eq. 5) ncid=netcdf_info%ncid_loc
  if(type .eq. 6) ncid=netcdf_info%ncid_alb
  if(type .eq. 7) ncid=netcdf_info%ncid_scan
  

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Title',trim(adjustl(ctitle)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Project',trim(adjustl(script_input%project)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',trim(adjustl(script_input%cncver)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CF_Convention_Version',trim(adjustl(script_input%ccon)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processing_Institution',trim(adjustl(script_input%cinst)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor',trim(adjustl(script_input%l2cproc)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor_Version',trim(adjustl(script_input%l2cprocver)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  

  !MJ ORG if(platform(1:4) .eq. 'noaa') PLATFORM(1:4)='NOAA'
  !MST following line by MST
  PLATFORMUP=platform
  if(platform(1:4) .eq. 'noaa') PLATFORMUP(1:4)='NOAA'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Platform',trim(adjustl(platformup)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Sensor_Name',trim(adjustl(sensor)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'uuid',trim(adjustl(script_input%uuid_tag)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cposition=index(trim(adjustl(path)),'/',back=.true.)
  clength=len_trim(adjustl(path))
!  fname=trim(adjustl(path(cposition+1:clength)))
  fname=trim(adjustl(path))
!  write(*,*) cposition,clength
!  write(*,*) trim(adjustl(path))
!  write(*,*) trim(adjustl(fname))
!  write(*,*) F90MAXNCNAM

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Name',trim(adjustl(fname(cposition+1:clength))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Email',trim(adjustl(script_input%contact)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Website',trim(adjustl(script_input%website)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Production_Time',trim(adjustl(script_input%exec_time)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date',&
       & trim(adjustl(trim(adjustl(cyear))//trim(adjustl(cmonth))//&
       & trim(adjustl(cday))//trim(adjustl(chour))//trim(adjustl(cminute)))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Reference',trim(adjustl(script_input%reference)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'History',trim(adjustl(script_input%history)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Summary',trim(adjustl(script_input%summary)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Keywords',trim(adjustl(script_input%keywords)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Comment',trim(adjustl(script_input%comment)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'License',trim(adjustl(script_input%license)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  !close definition section
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error enddef swath'

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF
  
  RETURN
  
END SUBROUTINE nc_create_file_swath



!new

!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE nc_create_file_config(script_input,cyear,chour,cminute,cmonth,cday,platform,sensor,path,&
     & wo,preproc_dims,imager_geolocation,netcdf_info,channel_info)
!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_file.f90
  !

  !   
  ! Modifications Log:    
  !2013/11/06: MJ initial routine version.

  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------
  
  USE netcdf

  use preproc_constants

  use attribute_structures

  use preproc_structures

  use imager_structures

  use netcdf_structures

  use channel_structures

  IMPLICIT NONE


  
  ! Input
  INTEGER,INTENT(IN) :: wo!, ryr
  !ORG  INTEGER,INTENT(IN) :: time, nx, ny, grid, dx, dy,wo!, ryr
  CHARACTER(LEN=*),INTENT(IN) :: path
  

  INTEGER :: ncid

  
  
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
!  REAL(KIND=SINGLE):: lon(nx), lat(ny)

  integer :: cposition,clength,ierr

  character(len=platformlength) :: platform,platformup
  character(len=sensorlength) :: sensor
  character(len=filelength) :: fname,ctitle

  character(len=datelength) :: cyear,chour,cminute,cmonth,cday

  type(script_arguments_s) :: script_input

  type(preproc_dims_s) :: preproc_dims

! type(imager_angles_s) :: imager_angles

  type(imager_geolocation_s) :: imager_geolocation

  type(netcdf_info_s) :: netcdf_info

  type(channel_info_s) :: channel_info

  ! End of header ----------------------------------------------------------
  !MJ ORG ierr = NF90_CREATE(path, NF90_CLOBBER, netcdf_info%ncid_config)
  ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_config)
  IF (ierr.NE.NF90_NOERR)  stop 'error config creating file'
  
  ctitle='ORAC Preprocessing config  file'
  
  !start defining things
  ierr = NF90_REDEF(netcdf_info%ncid_config)
  
  
  !define x and y
  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nx_conf',&
       & imager_geolocation%endx-imager_geolocation%startx+1,&
       & netcdf_info%xdim_config)
  IF (ierr.NE.NF90_NOERR) then
     write(*,*) 'create x-d conf',ierr
     STOP 
  endif

  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'ny_conf',&
       & imager_geolocation%endy-imager_geolocation%starty+1,&
       & netcdf_info%ydim_config)
  IF (ierr.NE.NF90_NOERR) STOP 'create y-d conf'
  
  !define nviews
  !     ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nv_msi',&
  !          & imager_angles%nviews,&
  !          & netcdf_info%vdim_msi)
  !     IF (ierr.NE.NF90_NOERR) STOP 'create v-d msi'
  
  !define nchannels
  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nc_conf',&
       & channel_info%nchannels_total,&
       & netcdf_info%cdim_config)
  IF (ierr.NE.NF90_NOERR) STOP 'create c-d conf'
  
  !define some channel variables
  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'msi_instr_ch_numbers', NF90_INT, netcdf_info%cdim_config,&
       & netcdf_info%channelninid_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def conf channel n'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channelninid_config, '_FillValue', long_int_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue conf channel n'
  
  
  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'msi_abs_ch_numbers', NF90_INT, netcdf_info%cdim_config,&
       & netcdf_info%channelnabsid_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def conf channel n abs'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channelnabsid_config, '_FillValue', long_int_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue conf channel n abs'

  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'msi_abs_ch_wl', NF90_FLOAT, netcdf_info%cdim_config,&
       & netcdf_info%channelwlabsid_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def conf channel wl abs'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channelwlabsid_config, '_FillValue', real_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue conf channel wl abs'

  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'msi_ch_swflag', NF90_INT, netcdf_info%cdim_config,&
       & netcdf_info%channelswflag_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def conf channel swf'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channelswflag_config, '_FillValue', long_int_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue conf channel swf'
  
  
  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'msi_ch_lwflag', NF90_INT, netcdf_info%cdim_config,&
       & netcdf_info%channellwflag_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def msi channel lwf'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channellwflag_config, '_FillValue', long_int_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue conf channel lwf'

  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'msi_ch_procflag', NF90_INT, netcdf_info%cdim_config,&
       & netcdf_info%channelprocflag_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def conf channel proc'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channelprocflag_config, '_FillValue', long_int_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue conf channel proc'

  !define nchannels albedo
  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nc_alb',&
       & channel_info%nchannels_sw,&
       & netcdf_info%cdim_config_alb)
  IF (ierr.NE.NF90_NOERR) STOP 'create c-d alb conf'
  
  !define nchannels emissivity
  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nc_emis',&
       & channel_info%nchannels_lw,&
       & netcdf_info%cdim_config_emis)
  IF (ierr.NE.NF90_NOERR) STOP 'create c-d emi conf'
  

     !MJ OLD ierr = NF90_DEF_VAR ( netcdf_info%ncid_alb, 'alb_abs_ch_numbers', NF90_INT, netcdf_info%cdim_alb,&
     !MJ OLD netcdf_info%channelnabsid) 
  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'alb_abs_ch_numbers', NF90_INT, netcdf_info%cdim_config_alb,&
       netcdf_info%channelnalbid_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def alb conf channel n abs'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channelnalbid_config, '_FillValue', long_int_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue alb conf channel n abs'
  
  ierr = NF90_DEF_VAR ( netcdf_info%ncid_config, 'emis_abs_ch_numbers', NF90_INT, netcdf_info%cdim_config_emis,&
       netcdf_info%channelnemisid_config) 
  IF (ierr.NE.NF90_NOERR) STOP 'def emis conf channel n abs'
  ierr = NF90_PUT_ATT(netcdf_info%ncid_config,  netcdf_info%channelnemisid_config, '_FillValue', long_int_fill_value )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue emis conf channel n abs'

!!$
!!$  !define horizontal dimension as one big vector containing all pixels
!!$  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlon_x_nlat_lwrtm', NF90_UNLIMITED,  netcdf_info%xydim_lw)
!!$  IF (ierr.NE.NF90_NOERR) STOP 'create xy-d 2'
!!$
!!$  !defone lon and lat just for reference
!!$  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlon_lwrtm',&
!!$       & preproc_dims%preproc_max_lon-preproc_dims%preproc_min_lon+1,&
!!$       & netcdf_info%xdim_lw)
!!$  IF (ierr.NE.NF90_NOERR) STOP 'create x-d'
!!$  
!!$  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlat_lwrtm',&
!!$       & preproc_dims%preproc_max_lat-preproc_dims%preproc_min_lat+1,&
!!$       & netcdf_info%ydim_lw)
!!$  IF (ierr.NE.NF90_NOERR) STOP 'create y-d'
!!$  
!!$  !layer land level dimension
!!$  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlayers_lwrtm',preproc_dims%kdim_pre-1,&
!!$       & netcdf_info%layerdim_lw)
!!$  IF (ierr.NE.NF90_NOERR) STOP 'create nlay lw'
!!$  
!!$  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlevels_lwrtm',preproc_dims%kdim_pre, &
!!$       & netcdf_info%leveldim_lw)
!!$  IF (ierr.NE.NF90_NOERR) STOP 'create nlev lw'
!!$  

  ncid=netcdf_info%ncid_config



  

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Title',trim(adjustl(ctitle)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Project',trim(adjustl(script_input%project)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',trim(adjustl(script_input%cncver)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CF_Convention_Version',trim(adjustl(script_input%ccon)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processing_Institution',trim(adjustl(script_input%cinst)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor',trim(adjustl(script_input%l2cproc)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor_Version',trim(adjustl(script_input%l2cprocver)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  

  !MJ ORG if(platform(1:4) .eq. 'noaa') PLATFORM(1:4)='NOAA'
  !MST following line by MST
  PLATFORMUP=platform
  if(platform(1:4) .eq. 'noaa') PLATFORMUP(1:4)='NOAA'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Platform',trim(adjustl(platformup)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Sensor_Name',trim(adjustl(sensor)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'uuid',trim(adjustl(script_input%uuid_tag)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cposition=index(trim(adjustl(path)),'/',back=.true.)
  clength=len_trim(adjustl(path))
!  fname=trim(adjustl(path(cposition+1:clength)))
  fname=trim(adjustl(path))
!  write(*,*) cposition,clength
!  write(*,*) trim(adjustl(path))
!  write(*,*) trim(adjustl(fname))
!  write(*,*) F90MAXNCNAM

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Name',trim(adjustl(fname(cposition+1:clength))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Email',trim(adjustl(script_input%contact)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Website',trim(adjustl(script_input%website)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Production_Time',trim(adjustl(script_input%exec_time)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date',&
       & trim(adjustl(trim(adjustl(cyear))//trim(adjustl(cmonth))//&
       & trim(adjustl(cday))//trim(adjustl(chour))//trim(adjustl(cminute)))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Reference',trim(adjustl(script_input%reference)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'History',trim(adjustl(script_input%history)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Summary',trim(adjustl(script_input%summary)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Keywords',trim(adjustl(script_input%keywords)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Comment',trim(adjustl(script_input%comment)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'License',trim(adjustl(script_input%license)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  !close definition section
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error enddef swath'

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF
  
  RETURN
  
END SUBROUTINE nc_create_file_config








