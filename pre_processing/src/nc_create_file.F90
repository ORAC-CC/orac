!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
subroutine nc_create_file_rtm(script_input,cyear,chour,cminute,cmonth,cday, &
     platform,sensor,path,wo,type,preproc_dims,imager_angles,netcdf_info, &
     channel_info,use_chunking)
   !-------------------------------------------------------------------------------
   !-------------------------------------------------------------------------------
   ! Description:
   !
   ! Creates new netcdf-file. (Output file)
   !
   !-------------------------------------------------------------------------------
   ! This software was developed within the ESA Cloud CCI Project and is based on
   ! routines developed during the ESA DUE GlobVapour Project.  Copyright 2011, DWD,
   ! All Rights Reserved.
   !-------------------------------------------------------------------------------
   !
   ! Unit Name:  nc_create_file_rtm.f90
   !
   ! Created on: 12/12/11
   !             by Matthias Jerg, DWD/KU22
   !             (matthias.jerg@dwd.de)
   !             based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
   !
   ! Modifications Log:
   ! 2012/05/15: MJ modifies file to write pixels in one loop successievely one
   !    after the other
   ! 2012/05/24: MJ adds some commenting.
   ! 2012/08/02: MJ adds some more code for writing of RTTOV output to netcdf file.
   ! 2012/08/02: CP bug fix in outputtting of msi files removed nviews ddimension
   ! 2012/09/20: CP added solzen and satzen tp prtm output
   ! 2012/11/29: CP changed variables names from layers to levels
   ! 2013/02/26: CP inserted missing comment for adding noaa platform
   ! 2013/03/07: CP added in some diagnostics q and albedo
   ! 2013/xx/xx: MJ adds PLATFORMUP varaible and output to comply with nomenclature.
   ! 2013/10/14: MJ fixed bug with writing of albedo and emissivity.
   ! 2013/11/06: MJ adds config file to preprocessing output which holds all
   !    relevant dimensional information.
   ! 2013/11/27: MJ changes output from netcdf3 to netcdf4.
   ! 2014/01/30: MJ implements chunking for the large variables which are actually
   !    read in later.
   ! 2014/02/02: GM adds chunking on/off option and cleans up code.
   ! 2014/02/02: GM puts setting up of common attributes in a subroutine used by
   !    all the nc_create_file_*() routines.
   ! 2014/02/02, GM: Changed the nlat x nlon 'unlimited' dimension size to a fixed
   !    dimension size.  The 'unlimited' dimension size is not required and results
   !    in a significant performance hit.
   ! 2014/02/03, GM: A small reordering of the variables in the SW RTM output to be
   !    consistent with the LW RTM output.
   ! 2014/02/10, AP: variable renaming
   !
   ! $Id$
   !
   ! Applied SPRs:
   !
   !-------------------------------------------------------------------------------

   use netcdf

   use attribute_structures
   use channel_structures
   use imager_structures
   use netcdf_structures
   use preproc_constants
   use preproc_structures

   implicit none

   ! Input
   type(script_arguments_s),      intent(in)    :: script_input
   character(len=datelength),     intent(in)    :: cyear
   character(len=datelength),     intent(in)    :: chour
   character(len=datelength),     intent(in)    :: cminute
   character(len=datelength),     intent(in)    :: cmonth
   character(len=datelength),     intent(in)    :: cday
   character(len=platformlength), intent(in)    :: platform
   character(len=sensorlength),   intent(in)    :: sensor
   character(len=*),              intent(in)    :: path
   integer,                       intent(in)    :: wo
   integer,                       intent(in)    :: type
   type(preproc_dims_s),          intent(in)    :: preproc_dims
   type(imager_angles_s),         intent(in)    :: imager_angles
   type(netcdf_info_s),           intent(inout) :: netcdf_info
   type(channel_info_s),          intent(in)    :: channel_info
   logical,                       intent(in)    :: use_chunking

   ! Local
   integer                       :: ierr
   integer                       :: nlon_x_nlat
   character(len=filelength)     :: ctitle
   character(len=platformlength) :: PLATFORMUP
   integer                       :: cposition,clength
   integer                       :: ncid
   character(len=filelength)     :: fname
   integer(kind=lint)            :: chunksize1d(1)
   integer(kind=lint)            :: chunksize2d(2)
   integer(kind=lint)            :: chunksize3d(3)


   nlon_x_nlat=(preproc_dims%max_lon-preproc_dims%min_lon+1) * &
        (preproc_dims%max_lat-preproc_dims%min_lat+1)


   ! open stuff related to LW
   if (type .eq. 1) then

      ctitle='ORAC Preprocessing lwrtm output file'

      ! create file
      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_lwrtm)
      if (ierr.ne.NF90_NOERR)  stop 'error lw creating file'


      ! start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_lwrtm)


      ! define horizontal dimension as one big vector containing all pixels
      ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlon_x_nlat_lwrtm', &
           nlon_x_nlat, netcdf_info%xydim_lw)
      if (ierr.ne.NF90_NOERR) stop 'create xy-d 2'

      ! define lon and lat just for reference
      ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlon_lwrtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, &
           netcdf_info%xdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'create x-d'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlat_lwrtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, &
           netcdf_info%ydim_lw)
      if (ierr.ne.NF90_NOERR) stop 'create y-d'

      ! layer land level dimension
      ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlayers_lwrtm', &
           preproc_dims%kdim-1, netcdf_info%layerdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'create nlay lw'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlevels_lwrtm', &
           preproc_dims%kdim, netcdf_info%leveldim_lw)
      if (ierr.ne.NF90_NOERR) stop 'create nlev lw'

      ! lw channel dimension
      ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nlw_channels', &
           channel_info%nchannels_lw, netcdf_info%lwchanneldim)
      if (ierr.ne.NF90_NOERR) stop 'create nchan lw'

      ! define channel ids abs
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'lw_channel_abs_ids', &
           NF90_INT, netcdf_info%lwchanneldim, netcdf_info%channels_id_lw)
      if (ierr.ne.NF90_NOERR) stop 'def channels lw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%channels_id_lw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue lw channel ids'

      ! define channel ids instr
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'lw_channel_instr_ids', &
           NF90_INT, netcdf_info%lwchanneldim, netcdf_info%channels_id_instr_lw)
      if (ierr.ne.NF90_NOERR) stop 'def channels lw instr'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%channels_id_instr_lw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue lw channel ids instr'

      ! define channel wavenumbers
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'lw_channel_wvl', NF90_FLOAT, &
           netcdf_info%lwchanneldim, netcdf_info%wvn_id_lw)
      if (ierr.ne.NF90_NOERR) stop 'def channels lw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%wvn_id_lw, &
           '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var chennel wvn FillValue'


      ! lw viewing dimension
      ierr = NF90_DEF_DIM(netcdf_info%ncid_lwrtm, 'nviews',imager_angles%nviews, &
           netcdf_info%viewdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'create nv lw'


      ! combines viewing xy dims
      netcdf_info%xyvdim_lw(1)=netcdf_info%viewdim_lw
      netcdf_info%xyvdim_lw(2)=netcdf_info%xydim_lw

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      endif

      ! define counter variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'counter_lw', NF90_INT, &
           netcdf_info%xydim_lw, netcdf_info%counterid_lw, &
           deflate_level=compress_level_lint, shuffle=shuffle_lint, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def counter lw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%counterid_lw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var clw FillValue'

      ! define solar zenith
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'solza_lw', NF90_FLOAT, &
           netcdf_info%xyvdim_lw, netcdf_info%solzaid_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def solza_lw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,netcdf_info%solzaid_lw, &
           '_FillValue',real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var solza_lw FillValue'

      ! define satellite zenith
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'satza_lw', NF90_FLOAT, &
           netcdf_info%xyvdim_lw, netcdf_info%satzaid_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def satza_lw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,netcdf_info%satzaid_lw, &
           '_FillValue',real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var satza_lw FillValue'

      ! define rez azimuth
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'relazi_lw', NF90_FLOAT, &
           netcdf_info%xyvdim_lw, netcdf_info%relaziid_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def relazi_lw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm,netcdf_info%relaziid_lw, &
           '_FillValue',real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var relazi_lw FillValue'
      if (.false.) then
         ! define longitude variable
         ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'lon_lw', NF90_FLOAT, &
              netcdf_info%xydim_lw, netcdf_info%lonid_lw)
         if (ierr.ne.NF90_NOERR) stop 'def lon lw'
         ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%lonid_lw, &
              '_FillValue', real_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue lon'

         ! define latitude variable
         ierr = NF90_DEF_VAR(netcdf_info%ncid_lwrtm, 'lat_lw', NF90_FLOAT, &
              netcdf_info%xydim_lw, netcdf_info%latid_lw)
         if (ierr.ne.NF90_NOERR) stop 'def lat lw'
         ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%latid_pw, &
              '_FillValue',  real_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue lat'
      endif

      ! set up the combined dimensions for 3D fields (spatial+channel)
      netcdf_info%xycdim_lw(1)=netcdf_info%lwchanneldim
      netcdf_info%xycdim_lw(2)=netcdf_info%xydim_lw

      if (.not. use_chunking) then
         chunksize2d(1)=channel_info%nchannels_lw
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize2d(1)=1
         chunksize2d(2)=nlon_x_nlat
      endif

      ! define emissivity 3D
      ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'emiss_lw', &
           NF90_FLOAT, netcdf_info%xycdim_lw, netcdf_info%emiss_id_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def lw emiss'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%emiss_id_lw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'


      ! set up the combined dimensions for 4D fields (c-z-xy)
      netcdf_info%xyzcdim_lw(1)=netcdf_info%lwchanneldim
      netcdf_info%xyzcdim_lw(2)=netcdf_info%leveldim_lw
      netcdf_info%xyzcdim_lw(3)=netcdf_info%xydim_lw

      if (.not. use_chunking) then
         chunksize3d(1)=channel_info%nchannels_lw
         chunksize3d(2)=preproc_dims%kdim
         chunksize3d(3)=nlon_x_nlat
      else
         chunksize3d(1)=1
         chunksize3d(2)=1
         chunksize3d(3)=nlon_x_nlat
      endif

      ! define tac profile at level centers as variable
      ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'tac_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%tac_id_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def lw tac'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%tac_id_lw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define tbc
      ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'tbc_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%tbc_id_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def lw tbc'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%tbc_id_lw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define radiances
      ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'rbc_up_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rbc_up_id_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def lw rbc_up'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%rbc_up_id_lw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'rac_up_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rac_up_id_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def lw rac_up'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%rac_up_id_lw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'


      ierr = NF90_DEF_VAR( netcdf_info%ncid_lwrtm, 'rac_down_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rac_down_id_lw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def lw rac_down'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lwrtm, netcdf_info%rac_down_id_lw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'


      ! open stuff related to SW
   else if (type .eq. 2) then

      ctitle='ORAC Preprocessing swrtm output file'


      ! create file
      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_swrtm)
      if (ierr.ne.NF90_NOERR)  stop 'error creating sw file'


      ! start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_swrtm)


      ! define horizontal dimension as one big vector containing all pixels
      ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlon_x_nlat_swrtm', &
           nlon_x_nlat, netcdf_info%xydim_sw)
      if (ierr.ne.NF90_NOERR) stop 'create xy-d 2'

      ! define lon and lat just for reference
      ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlon_swrtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, &
           netcdf_info%xdim_sw)
      if (ierr.ne.NF90_NOERR) stop 'create x-d'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlat_swrtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, &
           netcdf_info%ydim_sw)
      if (ierr.ne.NF90_NOERR) stop 'create y-d'

      ! layer land level dimension
      ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlayers_swrtm', &
           preproc_dims%kdim-1, netcdf_info%layerdim_sw)
      if (ierr.ne.NF90_NOERR) stop 'create sw layer'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nlevels_swrtm', &
           preproc_dims%kdim, netcdf_info%leveldim_sw)
      if (ierr.ne.NF90_NOERR) stop 'create sw level'

      ! sw channel dimension
      ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nsw_channels', &
           channel_info%nchannels_sw, netcdf_info%swchanneldim)
      if (ierr.ne.NF90_NOERR) stop 'create sw channel'

      ! define channel ids abs
      ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'sw_channel_abs_ids', &
           NF90_INT, netcdf_info%swchanneldim, netcdf_info%channels_id_sw)
      if (ierr.ne.NF90_NOERR) stop 'def channels sw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%channels_id_sw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue sw channel ids'

      ! define channel ids instr
      ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'sw_channel_instr_ids', &
           NF90_INT, netcdf_info%swchanneldim, netcdf_info%channels_id_instr_sw)
      if (ierr.ne.NF90_NOERR) stop 'def channels sw instr'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%channels_id_instr_sw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue sw channel ids instr'

      ! define channel wavenumbers
      ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'sw_channel_wvl', NF90_FLOAT, &
           netcdf_info%swchanneldim, netcdf_info%wvn_id_sw)
      if (ierr.ne.NF90_NOERR) stop 'def channels sw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%wvn_id_sw, &
           '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var chennel wvn FillValue'


      ! sw viewing dimension
      ierr = NF90_DEF_DIM(netcdf_info%ncid_swrtm, 'nviews',imager_angles%nviews, &
           netcdf_info%viewdim_sw)
      if (ierr.ne.NF90_NOERR) stop 'create sw views'


      ! combines viewing xy dims
      netcdf_info%xyvdim_sw(1)=netcdf_info%viewdim_sw
      netcdf_info%xyvdim_sw(2)=netcdf_info%xydim_sw

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      endif

      ! define counter variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'counter_sw', NF90_INT, &
           netcdf_info%xydim_sw, netcdf_info%counterid_sw, &
           deflate_level=compress_level_lint, shuffle=shuffle_lint)
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def counter sw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%counterid_sw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var csw FillValue'

      ! define solar zenith
      ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'solza_sw', NF90_FLOAT, &
           netcdf_info%xyvdim_sw, netcdf_info%solzaid_sw, &
           deflate_level=compress_level_float, shuffle=shuffle_float)
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def solza_sw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,netcdf_info%solzaid_sw, &
           '_FillValue',real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var solza_sw FillValue'

      ! define satellite zenith
      ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'satza_sw', NF90_FLOAT, &
           netcdf_info%xyvdim_sw, netcdf_info%satzaid_sw, &
           deflate_level=compress_level_float, shuffle=shuffle_float)
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def satza_sw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,netcdf_info%satzaid_sw, &
           '_FillValue',real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var satza_sw FillValue'

      ! define rez azimuth
      ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'relazi_sw', NF90_FLOAT, &
           netcdf_info%xyvdim_sw, netcdf_info%relaziid_sw, &
           deflate_level=compress_level_float, shuffle=shuffle_float)
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def relazi_sw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm,netcdf_info%relaziid_sw, &
           '_FillValue',real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var relazi_sw FillValue'
      if (.false.) then
         ! define longitude variable
         ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'lon_sw', NF90_FLOAT, &
              netcdf_info%xydim_sw, netcdf_info%lonid_sw)
         if (ierr.ne.NF90_NOERR) stop 'def lon sw'
         ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%lonid_sw, &
              '_FillValue', real_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue lon'

         ! define latitude variable
         ierr = NF90_DEF_VAR(netcdf_info%ncid_swrtm, 'lat_sw', NF90_FLOAT, &
              netcdf_info%xydim_sw, netcdf_info%latid_sw)
         if (ierr.ne.NF90_NOERR) stop 'def lat sw'
         ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%latid_pw, &
              '_FillValue',  real_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue lat'
      endif

      ! set up the combined dimensions for 4D fields
      netcdf_info%xyzcdim_sw(1)=netcdf_info%swchanneldim
      netcdf_info%xyzcdim_sw(2)=netcdf_info%leveldim_sw
      netcdf_info%xyzcdim_sw(3)=netcdf_info%xydim_sw

      if (.not. use_chunking) then
         chunksize3d(1)=channel_info%nchannels_sw
         chunksize3d(2)=preproc_dims%kdim
         chunksize3d(3)=nlon_x_nlat
      else
         chunksize3d(1)=1
         chunksize3d(2)=1
         chunksize3d(3)=nlon_x_nlat
      endif

      ! define tac profile at level centers as variable
      ierr = NF90_DEF_VAR( netcdf_info%ncid_swrtm, 'tac_sw', &
           NF90_FLOAT, netcdf_info%xyzcdim_sw, netcdf_info%tac_id_sw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def sw tac'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%tac_id_sw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ierr = NF90_DEF_VAR( netcdf_info%ncid_swrtm, 'tbc_sw', &
           NF90_FLOAT, netcdf_info%xyzcdim_sw, netcdf_info%tbc_id_sw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def sw tbc'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_swrtm, netcdf_info%tbc_id_sw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'


      ! open stuff related to meteo
   else if (type .eq. 3) then

      ctitle='ORAC Preprocessing prtm output file'


      ! create file
      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_prtm)
      if (ierr.ne.NF90_NOERR)  stop 'error creating p file'


      ! start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_prtm)


      ! define horizontal dimension as one big vector containing all pixels
      ierr = NF90_DEF_DIM(netcdf_info%ncid_prtm, 'nlon_x_nlat_prtm', &
           nlon_x_nlat, netcdf_info%xydim_pw)
      if (ierr.ne.NF90_NOERR) stop 'create xy-d 3'

      ! define lon and lat just for reference
      ierr = NF90_DEF_DIM(netcdf_info%ncid_prtm, 'nlon_prtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, &
           netcdf_info%xdim_pw)
      if (ierr.ne.NF90_NOERR) stop 'create xy-d'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_prtm, 'nlat_prtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, &
           netcdf_info%ydim_pw)
      if (ierr.ne.NF90_NOERR) stop 'create xy-d'

      ! layer land level dimension
      ierr = NF90_DEF_DIM( netcdf_info%ncid_prtm, 'nlayers_prtm', &
           preproc_dims%kdim-1, netcdf_info%layerdim_pw)
      if (ierr.ne.NF90_NOERR) stop 'create nlay prtm'

      ierr = NF90_DEF_DIM( netcdf_info%ncid_prtm, 'nlevels_prtm', &
           preproc_dims%kdim, netcdf_info%leveldim_pw)
      if (ierr.ne.NF90_NOERR) stop 'create nlev prtm'

      if (.false.) then ! DO THIS RATHER VIA THE GLOBAL ATTRIBUTES, same for instrument
         !date dimension
         ierr = NF90_DEF_DIM( netcdf_info%ncid_prtm, 'ndate_prtm',1, &
              netcdf_info%datedim_pw)
         if (ierr.ne.NF90_NOERR) stop 'create date-d'

         ! define date variable
         ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'date', NF90_INT, &
              netcdf_info%datedim_pw, netcdf_info%date_id_pw)
         if (ierr.ne.NF90_NOERR) stop 'def date'
         ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%date_id_pw, &
              '_FillValue',long_int_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'
      endif

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat
      else
         chunksize1d(1)=nlon_x_nlat
      endif

      ! define i variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'i_pw', NF90_INT, &
           netcdf_info%xydim_pw, netcdf_info%iid_pw, &
           deflate_level=compress_level_lint, shuffle=shuffle_lint, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def i'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%iid_pw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var i FillValue'

      ! define j variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'j_pw', NF90_INT, &
           netcdf_info%xydim_pw, netcdf_info%jid_pw, &
           deflate_level=compress_level_lint, shuffle=shuffle_lint, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def j'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%jid_pw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var j FillValue'

      ! define counter variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'counter_pw', NF90_INT, &
           netcdf_info%xydim_pw, netcdf_info%counterid_pw, &
           deflate_level=compress_level_lint, shuffle=shuffle_lint, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def counter pw'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%counterid_pw, &
           '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var i FillValue'

      ! define longitude variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'lon_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%lonid_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def lon'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%lonid_pw, &
           '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define latitude variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'lat_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%latid_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def lat'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%latid_pw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define skint variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'skint_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%skintid_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def skint'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%skintid_pw, &
           '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define exp(lnsp) variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'explnsp_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%lnspid_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def explnsp'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%lnspid_pw, &
           '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define exp(lsf) variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'lsf_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%lsfid_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'def lsf'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%lsfid_pw, &
           '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define satzen variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'satzen_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%satzenid_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float)
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def satzen'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%satzenid_pw, &
           '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define solzen variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_prtm, 'solzen_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%solzenid_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float)
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def solzen'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%solzenid_pw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'


      ! set up the combined dimensions for 3D fields
      netcdf_info%xyzdim_pw(1)=netcdf_info%leveldim_pw
      netcdf_info%xyzdim_pw(2)=netcdf_info%xydim_pw

      if (.not. use_chunking) then
         chunksize2d(1)=preproc_dims%kdim
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize2d(1)=1
         chunksize2d(2)=nlon_x_nlat
      endif

      ! define pressure profile at level centers as variable
      ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'pprofile_lev', &
           NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%pprofile_lev_id_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def lat'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%pprofile_lev_id_pw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

      ! define temperature profile at lever centers as variable
      ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'tprofile_lev', &
           NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%tprofile_lev_id_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def lat'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%tprofile_lev_id_pw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'
      if (.false.) then
         ! define spec_hum profile at lever centers as variable
         ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'qprofile_lev', &
              NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%qprofile_lev_id_pw)
         if (ierr.ne.NF90_NOERR) stop 'def lat'
         ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%qprofile_lev_id_pw, &
              '_FillValue',  real_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'
      endif
      ! define geopotential height profile at lever centers as variable
      ierr = NF90_DEF_VAR( netcdf_info%ncid_prtm, 'gphprofile_lev', &
           NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%hprofile_lev_id_pw, &
           deflate_level=compress_level_float, shuffle=shuffle_float, &
           chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def lat'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_prtm, netcdf_info%hprofile_lev_id_pw, &
           '_FillValue',  real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue'

   endif


   ! set up attributes common to all output files
   if (type .eq. 1) ncid=netcdf_info%ncid_lwrtm
   if (type .eq. 2) ncid=netcdf_info%ncid_swrtm
   if (type .eq. 3) ncid=netcdf_info%ncid_prtm

   call set_common_attributes(ncid,script_input,cyear,chour,cminute,cmonth, &
        cday,ctitle,platform,sensor,path)


   ! close definition section
   ierr = NF90_ENDDEF(ncid)
   if (ierr.ne.NF90_NOERR)  stop 'error enddef rtm'

   if (wo.eq.1) then
      write(*,*) ''
      write(*,*) 'New file created: ',TRIM(path)
   endif

   return

end subroutine nc_create_file_rtm



!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
subroutine nc_create_file_swath(script_input,cyear,chour,cminute,cmonth,cday, &
   platform,sensor,path,wo,type,imager_geolocation,imager_angles,netcdf_info, &
   channel_info, use_chunking)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! Description:
!
! Creates new netcdf-file. (Output file)
!
!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------
!
! Unit Name:  nc_create_file_swath.f90
!
! Created on: 12/12/11
!             by Matthias Jerg, DWD/KU22
!             (matthias.jerg@dwd.de)
!             based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!
! Modifications Log:
! 2012/05/31: MJ initial routine version.
! 2012/07/04: CP removed nviews dimension of dat
! 2014/02/02: GM adds chunking on/off option and cleans up code.
! 2014/02/02: GM puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
!
! Applied SPRs:
!
!-------------------------------------------------------------------------------

   use netcdf

   use attribute_structures
   use preproc_constants
   use channel_structures
   use imager_structures
   use netcdf_structures

   implicit none

   ! Input
   type(script_arguments_s),      intent(in)    :: script_input
   character(len=datelength),     intent(in)    :: cyear
   character(len=datelength),     intent(in)    :: chour
   character(len=datelength),     intent(in)    :: cminute
   character(len=datelength),     intent(in)    :: cmonth
   character(len=datelength),     intent(in)    :: cday
   character(len=platformlength), intent(in)    :: platform
   character(len=sensorlength),   intent(in)    :: sensor
   character(len=*),              intent(in)    :: path
   integer,                       intent(in)    :: wo
   integer,                       intent(in)    :: type
   type(imager_geolocation_s),    intent(in)    :: imager_geolocation
   type(imager_angles_s),         intent(in)    :: imager_angles
   type(netcdf_info_s),           intent(inout) :: netcdf_info
   type(channel_info_s),          intent(in)    :: channel_info
   logical,                       intent(in)    :: use_chunking

   ! Local
   integer                       :: ierr
   character(len=filelength)     :: ctitle
   character(len=platformlength) :: PLATFORMUP
   integer                       :: cposition,clength
   integer                       :: ncid
   character(len=filelength)     :: fname
   integer, dimension(2)         :: dims2d
   integer, dimension(3)         :: dims3d
   integer, dimension(3)         :: dims3dd
   integer(kind=lint)            :: chunksize2d(2)
   integer(kind=lint)            :: chunksize3d(3)


   ! open stuff related to msi
   if (type .eq. 1) then

      ctitle='ORAC Preprocessing msi output file'


      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
                         netcdf_info%ncid_msi)
      if (ierr.ne.NF90_NOERR)  stop 'error msi creating file'


      ! start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_msi)


      ! define x and y
      ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'nx_msi', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_msi)
      if (ierr.ne.NF90_NOERR) stop 'create x-d msi'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'ny_msi', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_msi)
      if (ierr.ne.NF90_NOERR) stop 'create y-d msi'
if (.false.) then
      ! define nviews
      ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'nv_msi', &
           imager_angles%nviews, &
           netcdf_info%vdim_msi)
      if (ierr.ne.NF90_NOERR) stop 'create v-d msi'
endif
      ! define nchannels
      ierr = NF90_DEF_DIM(netcdf_info%ncid_msi, 'nc_msi', &
           channel_info%nchannels_total, &
           netcdf_info%cdim_msi)
      if (ierr.ne.NF90_NOERR) stop 'create c-d msi'

      ! define some channel variables
      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'msi_instr_ch_numbers', NF90_INT, &
             netcdf_info%cdim_msi, netcdf_info%channelninid)
      if (ierr.ne.NF90_NOERR) stop 'def msi channel n'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%channelninid, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue msi channel n'

      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'msi_abs_ch_numbers', NF90_INT, &
             netcdf_info%cdim_msi, netcdf_info%channelnabsid)
      if (ierr.ne.NF90_NOERR) stop 'def msi channel n abs'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%channelnabsid, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue msi channel n abs'

      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'msi_abs_ch_wl', NF90_FLOAT, &
             netcdf_info%cdim_msi, netcdf_info%channelwlabsid)
      if (ierr.ne.NF90_NOERR) stop 'def msi channel wl abs'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%channelwlabsid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue msi channel wl abs'

      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'msi_ch_swflag', NF90_INT, &
             netcdf_info%cdim_msi, netcdf_info%channelswflag)
      if (ierr.ne.NF90_NOERR) stop 'def msi channel swf'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%channelswflag, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue msi channel swf'

      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'msi_ch_lwflag', NF90_INT, &
             netcdf_info%cdim_msi, netcdf_info%channellwflag)
      if (ierr.ne.NF90_NOERR) stop 'def msi channel lwf'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%channellwflag, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue msi channel lwf'

      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'msi_ch_procflag', NF90_INT, &
             netcdf_info%cdim_msi, netcdf_info%channelprocflag)
      if (ierr.ne.NF90_NOERR) stop 'def msi channel proc'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%channelprocflag, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue msi channel proc'


      dims2d(1)=netcdf_info%xdim_msi
      dims2d(2)=netcdf_info%ydim_msi

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      endif

      ! define time variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'time_data', NF90_DOUBLE, &
             dims2d, netcdf_info%timeid, deflate_level=compress_level_double, &
             shuffle=shuffle_double, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def time'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%timeid, &
                          '_FillValue', double_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue time'


      dims3dd(1)=netcdf_info%xdim_msi
      dims3dd(2)=netcdf_info%ydim_msi
      dims3dd(3)=netcdf_info%cdim_msi

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=channel_info%nchannels_total
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      endif

      ! define msi variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_msi, 'msi_data', NF90_FLOAT, dims3dd, &
             netcdf_info%msid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def msi'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_msi, netcdf_info%msid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue msi'


   ! open cloud flag file
   else if (type .eq. 2) then

      ctitle='ORAC Preprocessing cf output file'


      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
                         netcdf_info%ncid_cf)
      if (ierr.ne.NF90_NOERR)  stop 'error cf creating file'


      !start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_cf)


      ! define x and y
      ierr = NF90_DEF_DIM(netcdf_info%ncid_cf, 'nx_cf', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_cf)
      if (ierr.ne.NF90_NOERR) stop 'create x-d cf'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_cf, 'ny_cf', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_cf)
      if (ierr.ne.NF90_NOERR) stop 'create y-d cf'


      dims2d(1)=netcdf_info%xdim_cf
      dims2d(2)=netcdf_info%ydim_cf

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      endif

      ! define cf variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_cf, 'cflag', NF90_BYTE, dims2d, &
             netcdf_info%cfid, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def cf'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_cf, netcdf_info%cfid, &
                          '_FillValue', byte_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue cf'


   ! open land/sea flag file
   else if (type .eq. 3) then
      ctitle='ORAC Preprocessing lsf output file'


      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
                         netcdf_info%ncid_lsf)
      if (ierr.ne.NF90_NOERR)  stop 'error lsf creating file'


      ! start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_lsf)


      ! define x and y
      ierr = NF90_DEF_DIM(netcdf_info%ncid_lsf, 'nx_lsf', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_lsf)
      if (ierr.ne.NF90_NOERR) stop 'create x-d lsf'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_lsf, 'ny_lsf', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_lsf)
      if (ierr.ne.NF90_NOERR) stop 'create y-d lsf'


      dims2d(1)=netcdf_info%xdim_lsf
      dims2d(2)=netcdf_info%ydim_lsf

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      endif

      ! define lsf variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_lsf, 'lsflag', NF90_BYTE, dims2d, &
             netcdf_info%lsfid, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def lsf'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_lsf, netcdf_info%lsfid, &
                          '_FillValue', byte_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue lsf'


   ! open geometry file
   else if (type .eq. 4) then

      ctitle='ORAC Preprocessing geo output file'


      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
                         netcdf_info%ncid_geo)
      if (ierr.ne.NF90_NOERR)  stop 'error geo creating file'


      !start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_geo)


      ! define x and y
      ierr = NF90_DEF_DIM(netcdf_info%ncid_geo, 'nx_geo', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_geo)
      if (ierr.ne.NF90_NOERR) stop 'create x-d geo'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_geo, 'ny_geo', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_geo)
      if (ierr.ne.NF90_NOERR) stop 'create y-d geo'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_geo, 'nv_geo', &
           imager_angles%nviews, &
           netcdf_info%vdim_geo)
      if (ierr.ne.NF90_NOERR) stop 'create v-d geo'


      dims3d(1)=netcdf_info%xdim_geo
      dims3d(2)=netcdf_info%ydim_geo
      dims3d(3)=netcdf_info%vdim_geo

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=imager_angles%nviews
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      endif

      ! define solzen variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_geo, 'solzen', NF90_FLOAT, dims3d, &
             netcdf_info%solzenid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def solzen'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_geo, netcdf_info%solzenid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue solzen'

      ! define satzen variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_geo, 'satzen', NF90_FLOAT, dims3d, &
             netcdf_info%satzenid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def satzen'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_geo, netcdf_info%satzenid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue satzen'

      ! define solaz variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_geo, 'solaz', NF90_FLOAT, dims3d, &
             netcdf_info%solazid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def solaz'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_geo, netcdf_info%solazid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue solaz'
if (.false.) then
      ! define senazi variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_geo, 'senaz', NF90_FLOAT, dims3d, &
             netcdf_info%senazid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def senaz'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_geo, netcdf_info%senazid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue senazi'
endif
      ! define relazi variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_geo, 'relazi', NF90_FLOAT, dims3d, &
             netcdf_info%relazid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def relazi'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_geo, netcdf_info%relazid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue relazi'


  ! open location file
   else if (type .eq. 5) then

      ctitle='ORAC Preprocessing loc output file'


      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
                         netcdf_info%ncid_loc)
      if (ierr.ne.NF90_NOERR)  stop 'error loc creating file'


      !start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_loc)


      ! define x and y
      ierr = NF90_DEF_DIM(netcdf_info%ncid_loc, 'nx_loc', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_loc)
      if (ierr.ne.NF90_NOERR) stop 'create x-d loc'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_loc, 'ny_loc', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_loc)
      if (ierr.ne.NF90_NOERR) stop 'create y-d loc'


      dims2d(1)=netcdf_info%xdim_loc
      dims2d(2)=netcdf_info%ydim_loc

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      endif

      ! define lon variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_loc, 'lon', NF90_FLOAT, dims2d, &
             netcdf_info%lonid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def loc'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_loc, netcdf_info%lonid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue loc'

      ! define lat variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_loc, 'lat', NF90_FLOAT, dims2d, &
             netcdf_info%latid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def loc'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_loc, netcdf_info%latid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue loc'


   ! open stuff related to albedo
   else if (type .eq. 6) then

      ctitle='ORAC Preprocessing alb output file'


      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
                         netcdf_info%ncid_alb)
      if (ierr.ne.NF90_NOERR)  stop 'error alb creating file'


      ! start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_alb)


      ! define x and y
      ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'nx_alb', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_alb)
      if (ierr.ne.NF90_NOERR) stop 'create x-d alb'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'ny_alb', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_alb)
      if (ierr.ne.NF90_NOERR) stop 'create y-d alb'

      ! define nchannels albedo
      ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'nc_alb', &
           channel_info%nchannels_sw, &
           netcdf_info%cdim_alb)
      if (ierr.ne.NF90_NOERR) stop 'create c-d alb'

      ! define nchannels emissivity
      ierr = NF90_DEF_DIM(netcdf_info%ncid_alb, 'nc_emis', &
           channel_info%nchannels_lw, &
           netcdf_info%cdim_emis)
      if (ierr.ne.NF90_NOERR) stop 'create c-d emi'


      ierr = NF90_DEF_VAR(netcdf_info%ncid_alb, 'alb_abs_ch_numbers', NF90_INT, &
             netcdf_info%cdim_alb, netcdf_info%channelnalbid)
      if (ierr.ne.NF90_NOERR) stop 'def alb channel n abs'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_alb, netcdf_info%channelnalbid, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue alb channel n abs'

      ierr = NF90_DEF_VAR(netcdf_info%ncid_alb, 'emis_abs_ch_numbers', NF90_INT, &
             netcdf_info%cdim_emis, netcdf_info%channelnemisid)
      if (ierr.ne.NF90_NOERR) stop 'def emis channel n abs'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_alb, netcdf_info%channelnemisid, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue emis channel n abs'


      dims3d(1)=netcdf_info%xdim_msi
      dims3d(2)=netcdf_info%ydim_msi
      dims3d(3)=netcdf_info%cdim_alb

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=channel_info%nchannels_lw
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      endif

      ! define alb variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_alb, 'alb_data', NF90_FLOAT, dims3d, &
             netcdf_info%albid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def alb'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_alb, netcdf_info%albid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue alb'


      dims3d(1)=netcdf_info%xdim_msi
      dims3d(2)=netcdf_info%ydim_msi
      dims3d(3)=netcdf_info%cdim_emis

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=channel_info%nchannels_lw
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      endif

      ! define emis variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_alb, 'emis_data', NF90_FLOAT, dims3d, &
             netcdf_info%emisid, deflate_level=compress_level_float, &
             shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'def emis'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_alb, netcdf_info%emisid, &
                          '_FillValue', real_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue emis'

   else if (type .eq. 7) then

      ctitle='ORAC Preprocessing scan output file'


      ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
                         netcdf_info%ncid_scan)
      if (ierr.ne.NF90_NOERR)  stop 'error scan creating file'


      !start defining things
      ierr = NF90_REDEF(netcdf_info%ncid_scan)


      ! define x and y
      ierr = NF90_DEF_DIM(netcdf_info%ncid_scan, 'nx_scan', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_scan)
      if (ierr.ne.NF90_NOERR) stop 'create x-d scan'

      ierr = NF90_DEF_DIM(netcdf_info%ncid_scan, 'ny_scan', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_scan)
      if (ierr.ne.NF90_NOERR) stop 'create y-d scan'


      dims2d(1)=netcdf_info%xdim_scan
      dims2d(2)=netcdf_info%ydim_scan

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      endif

      ! define u variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_scan, 'uscan', NF90_INT, dims2d, &
             netcdf_info%uscanid, deflate_level=compress_level_lint, &
             shuffle=shuffle_lint,chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def scan u'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_scan, netcdf_info%uscanid, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue u scan'

      ! define v variable
      ierr = NF90_DEF_VAR(netcdf_info%ncid_scan, 'vscan', NF90_INT, dims2d, &
             netcdf_info%vscanid, deflate_level=compress_level_lint, &
             shuffle=shuffle_lint, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'def scan v'
      ierr = NF90_PUT_ATT(netcdf_info%ncid_scan, netcdf_info%vscanid, &
                          '_FillValue', long_int_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue v scan'

   endif


   ! set up attributes common to all output files
   if (type .eq. 1) ncid=netcdf_info%ncid_msi
   if (type .eq. 2) ncid=netcdf_info%ncid_cf
   if (type .eq. 3) ncid=netcdf_info%ncid_lsf
   if (type .eq. 4) ncid=netcdf_info%ncid_geo
   if (type .eq. 5) ncid=netcdf_info%ncid_loc
   if (type .eq. 6) ncid=netcdf_info%ncid_alb
   if (type .eq. 7) ncid=netcdf_info%ncid_scan

   call set_common_attributes(ncid,script_input,cyear,chour,cminute,cmonth, &
                              cday,ctitle,platform,sensor,path)


   ! close definition section
   ierr = NF90_ENDDEF(ncid)
   if (ierr.ne.NF90_NOERR)  stop 'error enddef swath'

   if (wo.eq.1) then
      write(*,*) ''
      write(*,*) 'New file created: ',TRIM(path)
   endif

   return

end subroutine nc_create_file_swath



!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
subroutine nc_create_file_config(script_input,cyear,chour,cminute,cmonth,cday, &
   platform,sensor,path,wo,preproc_dims,imager_geolocation,netcdf_info,channel_info)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! Description:
!
! Creates new netcdf-file. (Output file)
!
!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project. Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------
!
! Unit Name: nc_create_file_config.f90
!
! Modifications Log:
! 2013/11/06: MJ initial routine version
! 2014/02/02: GM adds chunking on/off option and cleans up code.
! 2014/02/02: GM puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
!
! Applied SPRs:
!
!-------------------------------------------------------------------------------

   use netcdf

   use attribute_structures
   use channel_structures
   use imager_structures
   use netcdf_structures
   use preproc_constants
   use preproc_structures

   implicit none

   ! Input
   type(script_arguments_s),      intent(in)    :: script_input
   character(len=datelength),     intent(in)    :: cyear
   character(len=datelength),     intent(in)    :: chour
   character(len=datelength),     intent(in)    :: cminute
   character(len=datelength),     intent(in)    :: cmonth
   character(len=datelength),     intent(in)    :: cday
   character(len=platformlength), intent(in)    :: platform
   character(len=sensorlength),   intent(in)    :: sensor
   character(len=*),              intent(in)    :: path
   integer,                       intent(in)    :: wo
   type(preproc_dims_s),          intent(in)    :: preproc_dims
   type(imager_geolocation_s),    intent(in)    :: imager_geolocation
   type(netcdf_info_s),           intent(inout) :: netcdf_info
   type(channel_info_s),          intent(in)    :: channel_info

   ! Local
   integer                       :: ierr
   integer                       :: nlon_x_nlat
   character(len=filelength)     :: ctitle
   character(len=platformlength) :: PLATFORMUP
   integer                       :: cposition,clength
   integer                       :: ncid
   character(len=filelength)     :: fname


   ctitle='ORAC Preprocessing config  file'


   ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), netcdf_info%ncid_config)
   if (ierr.ne.NF90_NOERR)  stop 'error config creating file'


   ! start defining things
   ierr = NF90_REDEF(netcdf_info%ncid_config)


   ! define x and y
   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nx_conf', &
        imager_geolocation%endx-imager_geolocation%startx+1, &
        netcdf_info%xdim_config)
   if (ierr.ne.NF90_NOERR) stop 'create x-d conf'

   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'ny_conf', &
        imager_geolocation%endy-imager_geolocation%starty+1, &
        netcdf_info%ydim_config)
   if (ierr.ne.NF90_NOERR) stop 'create y-d conf'

   ! define nviews
!  ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nv_msi', &
!       imager_angles%nviews, &
!       netcdf_info%vdim_msi)
!  if (ierr.ne.NF90_NOERR) stop 'create v-d msi'

   ! define nchannels
   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nc_conf', &
        channel_info%nchannels_total, &
        netcdf_info%cdim_config)
   if (ierr.ne.NF90_NOERR) stop 'create c-d conf'

   ! define some channel variables
   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'msi_instr_ch_numbers', NF90_INT, &
          netcdf_info%cdim_config, netcdf_info%channelninid_config)
   if (ierr.ne.NF90_NOERR) stop 'def conf channel n'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channelninid_config, &
                       '_FillValue', long_int_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue conf channel n'

   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'msi_abs_ch_numbers', NF90_INT, &
          netcdf_info%cdim_config, netcdf_info%channelnabsid_config)
   if (ierr.ne.NF90_NOERR) stop 'def conf channel n abs'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channelnabsid_config, &
                       '_FillValue', long_int_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue conf channel n abs'

   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'msi_abs_ch_wl', NF90_FLOAT, &
          netcdf_info%cdim_config, netcdf_info%channelwlabsid_config)
   if (ierr.ne.NF90_NOERR) stop 'def conf channel wl abs'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channelwlabsid_config, &
                       '_FillValue', real_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue conf channel wl abs'

   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'msi_ch_swflag', NF90_INT, &
          netcdf_info%cdim_config, netcdf_info%channelswflag_config)
   if (ierr.ne.NF90_NOERR) stop 'def conf channel swf'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channelswflag_config, &
                       '_FillValue', long_int_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue conf channel swf'

   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'msi_ch_lwflag', NF90_INT, &
          netcdf_info%cdim_config, netcdf_info%channellwflag_config)
   if (ierr.ne.NF90_NOERR) stop 'def msi channel lwf'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channellwflag_config, &
                       '_FillValue', long_int_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue conf channel lwf'

   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'msi_ch_procflag', NF90_INT, &
          netcdf_info%cdim_config, netcdf_info%channelprocflag_config)
   if (ierr.ne.NF90_NOERR) stop 'def conf channel proc'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channelprocflag_config, &
                       '_FillValue', long_int_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue conf channel proc'

   ! define nchannels albedo
   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nc_alb', &
        channel_info%nchannels_sw, &
        netcdf_info%cdim_config_alb)
   if (ierr.ne.NF90_NOERR) stop 'create c-d alb conf'

   ! define nchannels emissivity
   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nc_emis', &
        channel_info%nchannels_lw, &
        netcdf_info%cdim_config_emis)
   if (ierr.ne.NF90_NOERR) stop 'create c-d emi conf'

   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'alb_abs_ch_numbers', NF90_INT, &
          netcdf_info%cdim_config_alb, netcdf_info%channelnalbid_config)
   if (ierr.ne.NF90_NOERR) stop 'def alb conf channel n abs'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channelnalbid_config, &
                       '_FillValue', long_int_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue alb conf channel n abs'

   ierr = NF90_DEF_VAR(netcdf_info%ncid_config, 'emis_abs_ch_numbers', NF90_INT, &
          netcdf_info%cdim_config_emis, netcdf_info%channelnemisid_config)
   if (ierr.ne.NF90_NOERR) stop 'def emis conf channel n abs'
   ierr = NF90_PUT_ATT(netcdf_info%ncid_config, netcdf_info%channelnemisid_config, &
                       '_FillValue', long_int_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error def var FillValue emis conf channel n abs'

if (.false.) then
   nlon_x_nlat=(preproc_dims%max_lat-preproc_dims%min_lat+1)* &
             (preproc_dims%max_lon-preproc_dims%min_lon+1)

   ! define horizontal dimension as one big vector containing all pixels
   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlon_x_nlat_lwrtm', nlon_x_nlat, &
          netcdf_info%xydim_lw)
   if (ierr.ne.NF90_NOERR) stop 'create xy-d 2'

   ! define lon and lat just for reference
   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlon_lwrtm', &
        preproc_dims%max_lon-preproc_dims%min_lon+1, &
        netcdf_info%xdim_lw)
   if (ierr.ne.NF90_NOERR) stop 'create x-d'

   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlat_lwrtm', &
        preproc_dims%max_lat-preproc_dims%min_lat+1, &
        netcdf_info%ydim_lw)
   if (ierr.ne.NF90_NOERR) stop 'create y-d'

   ! layer land level dimension
   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlayers_lwrtm', preproc_dims%kdim-1, &
        netcdf_info%layerdim_lw)
   if (ierr.ne.NF90_NOERR) stop 'create nlay lw'

   ierr = NF90_DEF_DIM(netcdf_info%ncid_config, 'nlevels_lwrtm', preproc_dims%kdim, &
        netcdf_info%leveldim_lw)
   if (ierr.ne.NF90_NOERR) stop 'create nlev lw'
endif

   ncid=netcdf_info%ncid_config


   ! set up attributes common to all output files
   call set_common_attributes(ncid,script_input,cyear,chour,cminute,cmonth, &
                              cday,ctitle,platform,sensor,path)


   ! close definition section
   ierr = NF90_ENDDEF(ncid)
   if (ierr.ne.NF90_NOERR)  stop 'error enddef swath'

   if (wo.eq.1) then
      write(*,*) ''
      write(*,*) 'New file created: ',TRIM(path)
   endif

   return

end subroutine nc_create_file_config



subroutine set_common_attributes(ncid,script_input,cyear,chour,cminute,cmonth, &
                                 cday,ctitle,platform,sensor,path)

   use netcdf

   use attribute_structures

   implicit none

   integer,                       intent(in) :: ncid
   type(script_arguments_s),      intent(in) :: script_input
   character(len=datelength),     intent(in) :: cyear
   character(len=datelength),     intent(in) :: chour
   character(len=datelength),     intent(in) :: cminute
   character(len=datelength),     intent(in) :: cmonth
   character(len=datelength),     intent(in) :: cday
   character(len=filelength),     intent(in) :: ctitle
   character(len=platformlength), intent(in) :: platform
   character(len=sensorlength),   intent(in) :: sensor
   character(len=*),              intent(in) :: path

   integer                       :: ierr
   character(len=platformlength) :: PLATFORMUP
   integer                       :: cposition,clength
   character(len=filelength)     :: fname

   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Title',trim(adjustl(ctitle)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Project',trim(adjustl(script_input%project)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',trim(adjustl(script_input%cncver)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CF_Convention_Version',trim(adjustl(script_input%ccon)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processing_Institution',trim(adjustl(script_input%cinst)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor',trim(adjustl(script_input%l2cproc)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor_Version',trim(adjustl(script_input%l2cprocver)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'

   PLATFORMUP=platform
   if (platform(1:4) .eq. 'noaa') PLATFORMUP(1:4)='NOAA'

   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Platform',trim(adjustl(platformup)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Sensor_Name',trim(adjustl(sensor)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'uuid',trim(adjustl(script_input%uuid_tag)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'

   cposition=index(trim(adjustl(path)),'/',back=.true.)
   clength=len_trim(adjustl(path))
   fname=trim(adjustl(path))

   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Name',trim(adjustl(fname(cposition+1:clength))))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Email',trim(adjustl(script_input%contact)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Website',trim(adjustl(script_input%website)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Production_Time',trim(adjustl(script_input%exec_time)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date', &
        trim(adjustl(trim(adjustl(cyear))//trim(adjustl(cmonth))//&
        trim(adjustl(cday))//trim(adjustl(chour))//trim(adjustl(cminute)))))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Reference',trim(adjustl(script_input%reference)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'History',trim(adjustl(script_input%history)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Summary',trim(adjustl(script_input%summary)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Keywords',trim(adjustl(script_input%keywords)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Comment',trim(adjustl(script_input%comment)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'License',trim(adjustl(script_input%license)))
   if (ierr.ne.NF90_NOERR) stop 'error def conventions'

end subroutine set_common_attributes
