!-------------------------------------------------------------------------------
! This software was developed within the ESA Cloud CCI Project and is based on
! routines developed during the ESA DUE GlobVapour Project.  Copyright 2011, DWD,
! All Rights Reserved.
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: netcdf_output_create.F90
!
! Purpose:
! Define the fields and attributes of the NetCDF files output by the
! preprocessor. Three versions exist: RTM, SWATH, and CONFIG.
!
! Description and Algorithm details:
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! global_atts    struct  in   Structure detailing NCDF header contents.
! cyear          string  in   Year, as a 4 character string.
! cmonth         string  in   Month of year, as a 2 character string.
! cday           string  in   Day of month, as a 2 character string.
! chour          string  in   Hour of day, as a 2 character string.
! cminute        string  in   Minute of day, as a 2 character string.
! platform       string  in   Name of satellite platform.
! sensor         string  in   Name of sensor.
! path           string  in   Name of file to create.
! wo             integer in   1: Print procedure's success; 0: Don't.
! type           integer in   File type to produce. 1: LWRTM; 2: SWRTM; 3: PRTM.
! preproc_dims   struct  in   Summary of preprocessing grid definitions.
! imager_angles  struct  in   Summary of satellite geometry.
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed
! use_chunking   logic   in   T: Chunk output file; F: Don't.
!
! History:
! 2011/12/11, MJ: Original version.
! 2012/05/15, MJ: modifies file to write pixels in one loop successively one
!    after the other
! 2012/05/24, MJ: adds some commenting.
! 2012/08/02, MJ: adds some more code for writing of RTTOV output to netcdf file.
! 2012/08/02, CP: bug fix in outputting of msi files removed nviews dimension
! 2012/09/20, CP: added solzen and satzen tp prtm output
! 2012/11/29, CP: changed variables names from layers to levels
! 2013/02/26, CP: inserted missing comment for adding noaa platform
! 2013/03/07, CP: added in some diagnostics q and albedo
! 2013/xx/xx, MJ: adds PLATFORMUP variable and output to comply with nomenclature
! 2013/10/14, MJ: fixed bug with writing of albedo and emissivity.
! 2013/11/06, MJ: adds config file to preprocessing output which holds all
!    relevant dimensional information.
! 2013/11/27, MJ: changes output from netcdf3 to netcdf4.
! 2014/01/30, MJ: implements chunking for the large variables which are actually
!    read in later.
! 2014/02/02, GM: adds chunking on/off option and cleans up code.
! 2014/02/02, GM: puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
! 2014/02/02, GM: Changed the nlat x nlon 'unlimited' dimension size to a fixed
!    dimension size.  The 'unlimited' dimension size is not required and results
!    in a significant performance hit.
! 2014/02/03, GM: A small reordering of the variables in the SW RTM output to be
!    consistent with the LW RTM output.
! 2014/02/10, AP: variable renaming
! 2014/03/11, MJ: some modifications for chunking (only used when turned on)
! 2014/03/11, MJ: Commented out chunking completely in routines as I/O
!    performance issues persisted.
! 2014/05/01, GM: Reordered data/time arguments into a logical order.
! 2014/05/26, GM: Fixes/improvements to error reporting.
! 2014/08/01, AP: Remove unused counter fields.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/08/31, GM: Make the global attribute list consistent with CF-1.4.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine netcdf_create_rtm(global_atts,cyear,cmonth,cday,chour,cminute, &
     platform,sensor,path,wo,type,preproc_dims,imager_angles,netcdf_info, &
     channel_info,use_chunking)

   use netcdf

   use channel_structures
   use global_attributes
   use imager_structures
   use netcdf_structures
   use preproc_constants
   use preproc_structures

   implicit none

   ! Input
   type(global_attributes_s),      intent(in)    :: global_atts
   character(len=date_length),     intent(in)    :: cyear
   character(len=date_length),     intent(in)    :: cmonth
   character(len=date_length),     intent(in)    :: cday
   character(len=date_length),     intent(in)    :: chour
   character(len=date_length),     intent(in)    :: cminute
   character(len=platform_length), intent(in)    :: platform
   character(len=sensor_length),   intent(in)    :: sensor
   character(len=*),               intent(in)    :: path
   integer,                        intent(in)    :: wo
   integer,                        intent(in)    :: type
   type(preproc_dims_s),           intent(in)    :: preproc_dims
   type(imager_angles_s),          intent(in)    :: imager_angles
   type(netcdf_info_s),            intent(inout) :: netcdf_info
   type(channel_info_s),           intent(in)    :: channel_info
   logical,                        intent(in)    :: use_chunking

   ! Local
   integer                    :: ierr
   integer                    :: nlon_x_nlat
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer(kind=lint)         :: chunksize1d(1)
   integer(kind=lint)         :: chunksize2d(2)
   integer(kind=lint)         :: chunksize3d(3)


   nlon_x_nlat=(preproc_dims%max_lon-preproc_dims%min_lon+1) * &
        (preproc_dims%max_lat-preproc_dims%min_lat+1)


   ! open stuff related to LW
   if (type .eq. 1) then

      ctitle='ORAC Preprocessing lwrtm output file'

      ! create file
      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_lwrtm)
      if (ierr.ne.NF90_NOERR) stop 'error: lw creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_lwrtm)


      ! define horizontal dimension as one big vector containing all pixels
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlon_x_nlat_lwrtm', &
           nlon_x_nlat, netcdf_info%xydim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 2'

      ! define lon and lat just for reference
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlon_lwrtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, &
           netcdf_info%xdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error:create x-d'

      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlat_lwrtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, &
           netcdf_info%ydim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d'

      ! layer land level dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlayers_lwrtm', &
           preproc_dims%kdim-1, netcdf_info%layerdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlay lw'

      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlevels_lwrtm', &
           preproc_dims%kdim, netcdf_info%leveldim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlev lw'

      ! lw channel dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nlw_channels', &
           channel_info%nchannels_lw, netcdf_info%lwchanneldim)
      if (ierr.ne.NF90_NOERR) stop 'error: create nchan lw'

      ! define channel ids abs
      ierr = nf90_def_var(netcdf_info%ncid_lwrtm, 'lw_channel_abs_ids', &
           NF90_INT, netcdf_info%lwchanneldim, netcdf_info%channels_id_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: def channels lw'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%channels_id_lw, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue lw channel ids'

      ! define channel ids instr
      ierr = nf90_def_var(netcdf_info%ncid_lwrtm, 'lw_channel_instr_ids', &
           NF90_INT, netcdf_info%lwchanneldim, netcdf_info%channels_id_instr_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: def channels lw instr'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, &
           netcdf_info%channels_id_instr_lw, '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue lw channel ids instr'

      ! define channel wavenumbers
      ierr = nf90_def_var(netcdf_info%ncid_lwrtm, 'lw_channel_wvl', NF90_FLOAT, &
           netcdf_info%lwchanneldim, netcdf_info%wvn_id_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: def channels lw'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%wvn_id_lw, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var chennel wvn FillValue'


      ! lw viewing dimension
      ierr = nf90_def_dim(netcdf_info%ncid_lwrtm, 'nviews', &
           imager_angles%nviews, netcdf_info%viewdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nv lw'


      ! combines viewing xy dims
      netcdf_info%xyvdim_lw(1)=netcdf_info%viewdim_lw
      netcdf_info%xyvdim_lw(2)=netcdf_info%xydim_lw

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize1d(1)=min(nlon_x_nlat,max_chunk_latlon)

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define solar zenith
      ierr = nf90_def_var(netcdf_info%ncid_lwrtm, 'solza_lw', NF90_FLOAT, &
           netcdf_info%xyvdim_lw, netcdf_info%solzaid_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def solza_lw'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm,netcdf_info%solzaid_lw, &
           '_FillValue',sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var solza_lw FillValue'

      ! define satellite zenith
      ierr = nf90_def_var(netcdf_info%ncid_lwrtm, 'satza_lw', NF90_FLOAT, &
           netcdf_info%xyvdim_lw, netcdf_info%satzaid_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def satza_lw'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm,netcdf_info%satzaid_lw, &
           '_FillValue',sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var satza_lw FillValue'

      ! define rez azimuth
      ierr = nf90_def_var(netcdf_info%ncid_lwrtm, 'relazi_lw', NF90_FLOAT, &
           netcdf_info%xyvdim_lw, netcdf_info%relaziid_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def relazi_lw'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm,netcdf_info%relaziid_lw, &
           '_FillValue',sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var relazi_lw FillValue'

      ! set up the combined dimensions for 3D fields (spatial+channel)
      netcdf_info%xycdim_lw(1)=netcdf_info%lwchanneldim
      netcdf_info%xycdim_lw(2)=netcdf_info%xydim_lw

      if (.not. use_chunking) then
         chunksize2d(1)=channel_info%nchannels_lw
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize2d(1)=1
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define emissivity 3D
      ierr = nf90_def_var( netcdf_info%ncid_lwrtm, 'emiss_lw', &
           NF90_FLOAT, netcdf_info%xycdim_lw, netcdf_info%emiss_id_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lw emiss'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%emiss_id_lw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'


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
         chunksize3d(3)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define tac profile at level centers as variable
      ierr = nf90_def_var( netcdf_info%ncid_lwrtm, 'tac_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%tac_id_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lw tac'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%tac_id_lw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define tbc
      ierr = nf90_def_var( netcdf_info%ncid_lwrtm, 'tbc_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%tbc_id_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lw tbc'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%tbc_id_lw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define radiances
      ierr = nf90_def_var( netcdf_info%ncid_lwrtm, 'rbc_up_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rbc_up_id_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lw rbc_up'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%rbc_up_id_lw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ierr = nf90_def_var( netcdf_info%ncid_lwrtm, 'rac_up_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rac_up_id_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lw rac_up'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%rac_up_id_lw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'


      ierr = nf90_def_var( netcdf_info%ncid_lwrtm, 'rac_down_lw', &
           NF90_FLOAT, netcdf_info%xyzcdim_lw, netcdf_info%rac_down_id_lw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lw rac_down'
      ierr = nf90_put_att(netcdf_info%ncid_lwrtm, netcdf_info%rac_down_id_lw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'


   ! open stuff related to SW
   else if (type .eq. 2) then

      ctitle='ORAC Preprocessing swrtm output file'


      ! create file
      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_swrtm)
      if (ierr.ne.NF90_NOERR) stop 'error: creating sw file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_swrtm)


      ! define horizontal dimension as one big vector containing all pixels
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlon_x_nlat_swrtm', &
           nlon_x_nlat, netcdf_info%xydim_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 2'

      ! define lon and lat just for reference
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlon_swrtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, &
           netcdf_info%xdim_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d'

      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlat_swrtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, &
           netcdf_info%ydim_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d'

      ! layer land level dimension
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlayers_swrtm', &
           preproc_dims%kdim-1, netcdf_info%layerdim_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create sw layer'

      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nlevels_swrtm', &
           preproc_dims%kdim, netcdf_info%leveldim_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create sw level'

      ! sw channel dimension
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nsw_channels', &
           channel_info%nchannels_sw, netcdf_info%swchanneldim)
      if (ierr.ne.NF90_NOERR) stop 'error: create sw channel'

      ! define channel ids abs
      ierr = nf90_def_var(netcdf_info%ncid_swrtm, 'sw_channel_abs_ids', &
           NF90_INT, netcdf_info%swchanneldim, netcdf_info%channels_id_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: def channels sw'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm, netcdf_info%channels_id_sw, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue sw channel ids'

      ! define channel ids instr
      ierr = nf90_def_var(netcdf_info%ncid_swrtm, 'sw_channel_instr_ids', &
           NF90_INT, netcdf_info%swchanneldim, netcdf_info%channels_id_instr_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: def channels sw instr'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm, &
           netcdf_info%channels_id_instr_sw, '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue sw channel ids instr'

      ! define channel wavenumbers
      ierr = nf90_def_var(netcdf_info%ncid_swrtm, 'sw_channel_wvl', NF90_FLOAT, &
           netcdf_info%swchanneldim, netcdf_info%wvn_id_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: def channels sw'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm, netcdf_info%wvn_id_sw, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var chennel wvn FillValue'


      ! sw viewing dimension
      ierr = nf90_def_dim(netcdf_info%ncid_swrtm, 'nviews', &
           imager_angles%nviews, netcdf_info%viewdim_sw)
      if (ierr.ne.NF90_NOERR) stop 'error: create sw views'


      ! combines viewing xy dims
      netcdf_info%xyvdim_sw(1)=netcdf_info%viewdim_sw
      netcdf_info%xyvdim_sw(2)=netcdf_info%xydim_sw

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize1d(1)=min(nlon_x_nlat,max_chunk_latlon)

         chunksize2d(1)=imager_angles%nviews
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define solar zenith
      ierr = nf90_def_var(netcdf_info%ncid_swrtm, 'solza_sw', NF90_FLOAT, &
           netcdf_info%xyvdim_sw, netcdf_info%solzaid_sw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def solza_sw'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm,netcdf_info%solzaid_sw, &
           '_FillValue',sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var solza_sw FillValue'

      ! define satellite zenith
      ierr = nf90_def_var(netcdf_info%ncid_swrtm, 'satza_sw', NF90_FLOAT, &
           netcdf_info%xyvdim_sw, netcdf_info%satzaid_sw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def satza_sw'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm,netcdf_info%satzaid_sw, &
           '_FillValue',sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var satza_sw FillValue'

      ! define relative azimuth
      ierr = nf90_def_var(netcdf_info%ncid_swrtm, 'relazi_sw', NF90_FLOAT, &
           netcdf_info%xyvdim_sw, netcdf_info%relaziid_sw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def relazi_sw'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm,netcdf_info%relaziid_sw, &
           '_FillValue',sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var relazi_sw FillValue'

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
         chunksize3d(3)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define tac profile at level centers as variable
      ierr = nf90_def_var( netcdf_info%ncid_swrtm, 'tac_sw', &
           NF90_FLOAT, netcdf_info%xyzcdim_sw, netcdf_info%tac_id_sw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def sw tac'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm, netcdf_info%tac_id_sw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ierr = nf90_def_var( netcdf_info%ncid_swrtm, 'tbc_sw', &
           NF90_FLOAT, netcdf_info%xyzcdim_sw, netcdf_info%tbc_id_sw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def sw tbc'
      ierr = nf90_put_att(netcdf_info%ncid_swrtm, netcdf_info%tbc_id_sw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'


   ! open stuff related to meteo
   else if (type .eq. 3) then

      ctitle='ORAC Preprocessing prtm output file'


      ! create file
      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_prtm)
      if (ierr.ne.NF90_NOERR) stop 'error: creating p file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_prtm)


      ! define horizontal dimension as one big vector containing all pixels
      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlon_x_nlat_prtm', &
           nlon_x_nlat, netcdf_info%xydim_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 3'

      ! define lon and lat just for reference
      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlon_prtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, &
           netcdf_info%xdim_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d'

      ierr = nf90_def_dim(netcdf_info%ncid_prtm, 'nlat_prtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, &
           netcdf_info%ydim_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d'

      ! layer land level dimension
      ierr = nf90_def_dim( netcdf_info%ncid_prtm, 'nlayers_prtm', &
           preproc_dims%kdim-1, netcdf_info%layerdim_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlay prtm'

      ierr = nf90_def_dim( netcdf_info%ncid_prtm, 'nlevels_prtm', &
           preproc_dims%kdim, netcdf_info%leveldim_pw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlev prtm'

      if (.not. use_chunking) then
         chunksize1d(1)=nlon_x_nlat
      else
         chunksize1d(1)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define longitude variable
      ierr = nf90_def_var(netcdf_info%ncid_prtm, 'lon_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%lonid_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'error: def lon'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%lonid_pw, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define latitude variable
      ierr = nf90_def_var(netcdf_info%ncid_prtm, 'lat_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%latid_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'error: def lat'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%latid_pw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define skint variable
      ierr = nf90_def_var(netcdf_info%ncid_prtm, 'skint_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%skintid_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'error: def skint'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%skintid_pw, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define exp(lnsp) variable
      ierr = nf90_def_var(netcdf_info%ncid_prtm, 'explnsp_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%lnspid_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'error: def explnsp'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%lnspid_pw, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define exp(lsf) variable
      ierr = nf90_def_var(netcdf_info%ncid_prtm, 'lsf_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%lsfid_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize1d(1))
      if (ierr.ne.NF90_NOERR) stop 'error: def lsf'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%lsfid_pw, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define satzen variable
      ierr = nf90_def_var(netcdf_info%ncid_prtm, 'satzen_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%satzenid_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def satzen'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%satzenid_pw, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define solzen variable
      ierr = nf90_def_var(netcdf_info%ncid_prtm, 'solzen_pw', NF90_FLOAT, &
           netcdf_info%xydim_pw, netcdf_info%solzenid_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def solzen'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%solzenid_pw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'


      ! set up the combined dimensions for 3D fields
      netcdf_info%xyzdim_pw(1)=netcdf_info%leveldim_pw
      netcdf_info%xyzdim_pw(2)=netcdf_info%xydim_pw

      if (.not. use_chunking) then
         chunksize2d(1)=preproc_dims%kdim
         chunksize2d(2)=nlon_x_nlat
      else
         chunksize2d(1)=1
         chunksize2d(2)=min(nlon_x_nlat,max_chunk_latlon)
      end if

      ! define pressure profile at level centers as variable
      ierr = nf90_def_var( netcdf_info%ncid_prtm, 'pprofile_lev', &
           NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%pprofile_lev_id_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lat'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, netcdf_info%pprofile_lev_id_pw, &
           '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define temperature profile at lever centers as variable
      ierr = nf90_def_var( netcdf_info%ncid_prtm, 'tprofile_lev', &
           NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%tprofile_lev_id_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lat'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, &
           netcdf_info%tprofile_lev_id_pw, '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

      ! define geopotential height profile at lever centers as variable
      ierr = nf90_def_var( netcdf_info%ncid_prtm, 'gphprofile_lev', &
           NF90_FLOAT, netcdf_info%xyzdim_pw, netcdf_info%hprofile_lev_id_pw, &
           deflate_level=compress_level_sreal, shuffle=shuffle_float)!, &
      !          chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lat'
      ierr = nf90_put_att(netcdf_info%ncid_prtm, &
           netcdf_info%hprofile_lev_id_pw, '_FillValue',  sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue'

   end if


   ! set up attributes common to all output files
   if (type .eq. 1) ncid=netcdf_info%ncid_lwrtm
   if (type .eq. 2) ncid=netcdf_info%ncid_swrtm
   if (type .eq. 3) ncid=netcdf_info%ncid_prtm

   call netcdf_put_common_attributes(ncid,global_atts,ctitle,platform,sensor, &
        path,cyear,cmonth,cday,chour,cminute)


   ! close definition section
   ierr = nf90_enddef(ncid)
   if (ierr.ne.NF90_NOERR) stop 'error: enddef rtm'

   if (wo.eq.1) then
      write(*,*) ''
      write(*,*) 'New file created: ',TRIM(path)
   end if

   return

end subroutine netcdf_create_rtm


!-------------------------------------------------------------------------------
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! global_atts    struct  in   Structure detailing NCDF header contents.
! cyear          string  in   Year, as a 4 character string.
! cmonth         string  in   Month of year, as a 2 character string.
! cday           string  in   Day of month, as a 2 character string.
! chour          string  in   Hour of day, as a 2 character string.
! cminute        string  in   Minute of day, as a 2 character string.
! platform       string  in   Name of satellite platform.
! sensor         string  in   Name of sensor.
! path           string  in   Name of file to create.
! wo             integer in   1: Print procedure's success; 0: Don't.
! type           integer in   File type to produce. 1: MSI; 2: CF; 3: LSF;
!                             4: GEO; 5: LOC; 6: ALB; 7: SCAN.
! preproc_dims   struct  in   Summary of preprocessing grid definitions.
! imager_geolocation
!                struct  in   Summary of pixel positions
! imager_angles  struct  in   Summary of satellite geometry.
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed.
! use_chunking   logic   in   T: Chunk output file; F: Don't.
!
! History:
! 2012/05/31, MJ: initial routine version.
! 2012/07/04, CP: removed nviews dimension of data
! 2014/02/02, GM: adds chunking on/off option and cleans up code.
! 2014/02/02, GM: puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
!
!-------------------------------------------------------------------------------

subroutine netcdf_create_swath(global_atts,cyear,cmonth,cday,chour,cminute, &
   platform,sensor,path,wo,type,imager_geolocation,imager_angles,netcdf_info, &
   channel_info,use_chunking,include_full_brdf)

   use netcdf

   use channel_structures
   use global_attributes
   use imager_structures
   use netcdf_structures
   use preproc_constants

   implicit none

   ! Input
   type(global_attributes_s),      intent(in)    :: global_atts
   character(len=date_length),     intent(in)    :: cyear
   character(len=date_length),     intent(in)    :: cmonth
   character(len=date_length),     intent(in)    :: cday
   character(len=date_length),     intent(in)    :: chour
   character(len=date_length),     intent(in)    :: cminute
   character(len=platform_length), intent(in)    :: platform
   character(len=sensor_length),   intent(in)    :: sensor
   character(len=*),               intent(in)    :: path
   integer,                        intent(in)    :: wo
   integer,                        intent(in)    :: type
   type(imager_geolocation_s),     intent(in)    :: imager_geolocation
   type(imager_angles_s),          intent(in)    :: imager_angles
   type(netcdf_info_s),            intent(inout) :: netcdf_info
   type(channel_info_s),           intent(in)    :: channel_info
   logical,                        intent(in)    :: use_chunking
   logical,                        intent(in)    :: include_full_brdf

   ! Local
   integer                    :: ierr
   character(len=file_length) :: ctitle
   integer                    :: ncid
   integer, dimension(2)      :: dims2d
   integer, dimension(3)      :: dims3d
   integer, dimension(3)      :: dims3dd
   integer(kind=lint)         :: chunksize2d(2)
   integer(kind=lint)         :: chunksize3d(3)


   ! open stuff related to msi
   if (type .eq. 1) then

      ctitle='ORAC Preprocessing msi output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: msi creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_msi)


      ! define x and y
      ierr = nf90_def_dim(netcdf_info%ncid_msi, 'nx_msi', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d msi'

      ierr = nf90_def_dim(netcdf_info%ncid_msi, 'ny_msi', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d msi'
      if (.false.) then
         ! define nviews
         ierr = nf90_def_dim(netcdf_info%ncid_msi, 'nv_msi', &
              imager_angles%nviews, &
              netcdf_info%vdim_msi)
         if (ierr.ne.NF90_NOERR) stop 'error: create v-d msi'
      end if
      ! define nchannels
      ierr = nf90_def_dim(netcdf_info%ncid_msi, 'nc_msi', &
           channel_info%nchannels_total, &
           netcdf_info%cdim_msi)
      if (ierr.ne.NF90_NOERR) stop 'error: create c-d msi'

      ! define some channel variables
      ierr = nf90_def_var(netcdf_info%ncid_msi, 'msi_instr_ch_numbers', &
           NF90_INT, netcdf_info%cdim_msi, netcdf_info%channelninid)
      if (ierr.ne.NF90_NOERR) stop 'error: def msi channel n'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%channelninid, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue msi channel n'

      ierr = nf90_def_var(netcdf_info%ncid_msi, 'msi_abs_ch_numbers', NF90_INT, &
           netcdf_info%cdim_msi, netcdf_info%channelnabsid)
      if (ierr.ne.NF90_NOERR) stop 'error: def msi channel n abs'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%channelnabsid, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue msi channel n abs'

      ierr = nf90_def_var(netcdf_info%ncid_msi, 'msi_abs_ch_wl', NF90_FLOAT, &
           netcdf_info%cdim_msi, netcdf_info%channelwlabsid)
      if (ierr.ne.NF90_NOERR) stop 'error: def msi channel wl abs'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%channelwlabsid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue msi channel wl abs'

      ierr = nf90_def_var(netcdf_info%ncid_msi, 'msi_ch_swflag', NF90_INT, &
           netcdf_info%cdim_msi, netcdf_info%channelswflag)
      if (ierr.ne.NF90_NOERR) stop 'error: def msi channel swf'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%channelswflag, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue msi channel swf'

      ierr = nf90_def_var(netcdf_info%ncid_msi, 'msi_ch_lwflag', NF90_INT, &
           netcdf_info%cdim_msi, netcdf_info%channellwflag)
      if (ierr.ne.NF90_NOERR) stop 'error: def msi channel lwf'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%channellwflag, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue msi channel lwf'

      ierr = nf90_def_var(netcdf_info%ncid_msi, 'msi_ch_procflag', NF90_INT, &
           netcdf_info%cdim_msi, netcdf_info%channelprocflag)
      if (ierr.ne.NF90_NOERR) stop 'error: def msi channel proc'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%channelprocflag, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue msi channel proc'


      dims2d(1)=netcdf_info%xdim_msi
      dims2d(2)=netcdf_info%ydim_msi

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define time variable
      ierr = nf90_def_var(netcdf_info%ncid_msi, 'time_data', NF90_DOUBLE, &
           dims2d, netcdf_info%timeid, deflate_level=compress_level_dreal)!, &
      !            shuffle=shuffle_double, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def time'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%timeid, &
           '_FillValue', dreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue time'


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
      end if

      ! define msi variable
      ierr = nf90_def_var(netcdf_info%ncid_msi, 'msi_data', NF90_FLOAT, &
           dims3dd, netcdf_info%msid, deflate_level=compress_level_sreal)!, &
      !            shuffle=shuffle_float, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def msi'
      ierr = nf90_put_att(netcdf_info%ncid_msi, netcdf_info%msid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue msi'


   ! open cloud flag file
   else if (type .eq. 2) then

      ctitle='ORAC Preprocessing cf output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_cf)
      if (ierr.ne.NF90_NOERR) stop 'error: cf creating file'


      !start defining things
      ierr = nf90_redef(netcdf_info%ncid_cf)


      ! define x and y
      ierr = nf90_def_dim(netcdf_info%ncid_cf, 'nx_cf', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_cf)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d cf'

      ierr = nf90_def_dim(netcdf_info%ncid_cf, 'ny_cf', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_cf)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d cf'


      dims2d(1)=netcdf_info%xdim_cf
      dims2d(2)=netcdf_info%ydim_cf

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define cf variable
      ierr = nf90_def_var(netcdf_info%ncid_cf, 'cflag', NF90_BYTE, dims2d, &
           netcdf_info%cfid)!, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def cf'
      ierr = nf90_put_att(netcdf_info%ncid_cf, netcdf_info%cfid, &
           '_FillValue', byte_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue cf'


   ! open land/sea flag file
   else if (type .eq. 3) then
      ctitle='ORAC Preprocessing lsf output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_lsf)
      if (ierr.ne.NF90_NOERR) stop 'error: lsf creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_lsf)


      ! define x and y
      ierr = nf90_def_dim(netcdf_info%ncid_lsf, 'nx_lsf', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_lsf)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d lsf'

      ierr = nf90_def_dim(netcdf_info%ncid_lsf, 'ny_lsf', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_lsf)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d lsf'


      dims2d(1)=netcdf_info%xdim_lsf
      dims2d(2)=netcdf_info%ydim_lsf

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define lsf variable
      ierr = nf90_def_var(netcdf_info%ncid_lsf, 'lsflag', NF90_BYTE, dims2d, &
           netcdf_info%lsfid)!, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def lsf'
      ierr = nf90_put_att(netcdf_info%ncid_lsf, netcdf_info%lsfid, &
           '_FillValue', byte_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue lsf'


      ! open geometry file
   else if (type .eq. 4) then

      ctitle='ORAC Preprocessing geo output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: geo creating file'


      !start defining things
      ierr = nf90_redef(netcdf_info%ncid_geo)


      ! define x and y
      ierr = nf90_def_dim(netcdf_info%ncid_geo, 'nx_geo', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d geo'

      ierr = nf90_def_dim(netcdf_info%ncid_geo, 'ny_geo', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d geo'

      ierr = nf90_def_dim(netcdf_info%ncid_geo, 'nv_geo', &
           imager_angles%nviews, &
           netcdf_info%vdim_geo)
      if (ierr.ne.NF90_NOERR) stop 'error: create v-d geo'


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
      end if

      ! define solzen variable
      ierr = nf90_def_var(netcdf_info%ncid_geo, 'solzen', NF90_FLOAT, dims3d, &
           netcdf_info%solzenid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def solzen'
      ierr = nf90_put_att(netcdf_info%ncid_geo, netcdf_info%solzenid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue solzen'

      ! define satzen variable
      ierr = nf90_def_var(netcdf_info%ncid_geo, 'satzen', NF90_FLOAT, dims3d, &
           netcdf_info%satzenid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def satzen'
      ierr = nf90_put_att(netcdf_info%ncid_geo, netcdf_info%satzenid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue satzen'

      ! define solaz variable
      ierr = nf90_def_var(netcdf_info%ncid_geo, 'solaz', NF90_FLOAT, dims3d, &
           netcdf_info%solazid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def solaz'
      ierr = nf90_put_att(netcdf_info%ncid_geo, netcdf_info%solazid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue solaz'

      ! define relazi variable
      ierr = nf90_def_var(netcdf_info%ncid_geo, 'relazi', NF90_FLOAT, dims3d, &
           netcdf_info%relazid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def relazi'
      ierr = nf90_put_att(netcdf_info%ncid_geo, netcdf_info%relazid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue relazi'


   ! open location file
   else if (type .eq. 5) then

      ctitle='ORAC Preprocessing loc output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_loc)
      if (ierr.ne.NF90_NOERR) stop 'error: loc creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_loc)


      ! define x and y
      ierr = nf90_def_dim(netcdf_info%ncid_loc, 'nx_loc', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_loc)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d loc'

      ierr = nf90_def_dim(netcdf_info%ncid_loc, 'ny_loc', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_loc)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d loc'


      dims2d(1)=netcdf_info%xdim_loc
      dims2d(2)=netcdf_info%ydim_loc

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define lon variable
      ierr = nf90_def_var(netcdf_info%ncid_loc, 'lon', NF90_FLOAT, dims2d, &
           netcdf_info%lonid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def loc'
      ierr = nf90_put_att(netcdf_info%ncid_loc, netcdf_info%lonid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue loc'

      ! define lat variable
      ierr = nf90_def_var(netcdf_info%ncid_loc, 'lat', NF90_FLOAT, dims2d, &
           netcdf_info%latid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def loc'
      ierr = nf90_put_att(netcdf_info%ncid_loc, netcdf_info%latid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue loc'


   ! open stuff related to albedo
   else if (type .eq. 6) then

      ctitle='ORAC Preprocessing alb output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: alb creating file'


      ! start defining things
      ierr = nf90_redef(netcdf_info%ncid_alb)


      ! define x and y
      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'nx_alb', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d alb'

      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'ny_alb', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d alb'

      ! define nchannels albedo
      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'nc_alb', &
           channel_info%nchannels_sw, &
           netcdf_info%cdim_alb)
      if (ierr.ne.NF90_NOERR) stop 'error: create c-d alb'

      ! define nchannels emissivity
      ierr = nf90_def_dim(netcdf_info%ncid_alb, 'nc_emis', &
           channel_info%nchannels_lw, &
           netcdf_info%cdim_emis)
      if (ierr.ne.NF90_NOERR) stop 'error: create c-d emi'


      ierr = nf90_def_var(netcdf_info%ncid_alb, 'alb_abs_ch_numbers', NF90_INT, &
           netcdf_info%cdim_alb, netcdf_info%channelnalbid)
      if (ierr.ne.NF90_NOERR) stop 'error: def alb channel n abs'
      ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%channelnalbid, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue alb channel n abs'

      ierr = nf90_def_var(netcdf_info%ncid_alb, 'emis_abs_ch_numbers', &
           NF90_INT, netcdf_info%cdim_emis, netcdf_info%channelnemisid)
      if (ierr.ne.NF90_NOERR) stop 'error: def emis channel n abs'
      ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%channelnemisid, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) &
           write(*,*) 'error: def var FillValue emis channel n abs'


      dims3d(1)=netcdf_info%xdim_msi
      dims3d(2)=netcdf_info%ydim_msi
      dims3d(3)=netcdf_info%cdim_alb

      if (.not. use_chunking) then
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
         chunksize3d(3)=channel_info%nchannels_sw
      else
         chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize3d(2)=imager_geolocation%ny
         chunksize3d(3)=1
      end if

      ! define alb variable
      ierr = nf90_def_var(netcdf_info%ncid_alb, 'alb_data', NF90_FLOAT, dims3d, &
           netcdf_info%albid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def alb'
      ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%albid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue alb'


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
      end if

      ! define emis variable
      ierr = nf90_def_var(netcdf_info%ncid_alb, 'emis_data', NF90_FLOAT, &
           dims3d, netcdf_info%emisid, deflate_level=compress_level_sreal, &
           shuffle=shuffle_float)!, chunksizes=chunksize3d)
      if (ierr.ne.NF90_NOERR) stop 'error: def emis'
      ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%emisid, &
           '_FillValue', sreal_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue emis'


      if (include_full_brdf) then
         dims3d(1)=netcdf_info%xdim_msi
         dims3d(2)=netcdf_info%ydim_msi
         dims3d(3)=netcdf_info%cdim_alb

         if (.not. use_chunking) then
            chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
            chunksize3d(2)=imager_geolocation%endy-imager_geolocation%starty+1
            chunksize3d(3)=channel_info%nchannels_sw
         else
            chunksize3d(1)=imager_geolocation%endx-imager_geolocation%startx+1
            chunksize3d(2)=imager_geolocation%ny
            chunksize3d(3)=1
         end if

         ierr = nf90_def_var(netcdf_info%ncid_alb, 'rho_0v_data', NF90_FLOAT, dims3d, &
                netcdf_info%rho_0v_id, deflate_level=compress_level_sreal, &
                shuffle=shuffle_float)!, chunksizes=chunksize3d)
         if (ierr.ne.NF90_NOERR) stop 'error: def rho_0v_data'
         ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%rho_0v_id, &
                             '_FillValue', sreal_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue rho_0v_data'

         ierr = nf90_def_var(netcdf_info%ncid_alb, 'rho_0d_data', NF90_FLOAT, dims3d, &
                netcdf_info%rho_0d_id, deflate_level=compress_level_sreal, &
                shuffle=shuffle_float)!, chunksizes=chunksize3d)
         if (ierr.ne.NF90_NOERR) stop 'error: def rho_0d_data'
         ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%rho_0d_id, &
                             '_FillValue', sreal_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue rho_0d_data'

         ierr = nf90_def_var(netcdf_info%ncid_alb, 'rho_dv_data', NF90_FLOAT, dims3d, &
                netcdf_info%rho_dv_id, deflate_level=compress_level_sreal, &
                shuffle=shuffle_float)!, chunksizes=chunksize3d)
         if (ierr.ne.NF90_NOERR) stop 'error: def rho_dv_data'
         ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%rho_dv_id, &
                             '_FillValue', sreal_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue rho_dv_data'

         ierr = nf90_def_var(netcdf_info%ncid_alb, 'rho_dd_data', NF90_FLOAT, dims3d, &
                netcdf_info%rho_dd_id, deflate_level=compress_level_sreal, &
                shuffle=shuffle_float)!, chunksizes=chunksize3d)
         if (ierr.ne.NF90_NOERR) stop 'error: def rho_dd_data'
         ierr = nf90_put_att(netcdf_info%ncid_alb, netcdf_info%rho_dd_id, &
                             '_FillValue', sreal_fill_value)
         if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue rho_dd_data'
      end if

   else if (type .eq. 7) then

      ctitle='ORAC Preprocessing scan output file'


      ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
           netcdf_info%ncid_scan)
      if (ierr.ne.NF90_NOERR) stop 'error: scan creating file'


      !start defining things
      ierr = nf90_redef(netcdf_info%ncid_scan)


      ! define x and y
      ierr = nf90_def_dim(netcdf_info%ncid_scan, 'nx_scan', &
           imager_geolocation%endx-imager_geolocation%startx+1, &
           netcdf_info%xdim_scan)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d scan'

      ierr = nf90_def_dim(netcdf_info%ncid_scan, 'ny_scan', &
           imager_geolocation%endy-imager_geolocation%starty+1, &
           netcdf_info%ydim_scan)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d scan'


      dims2d(1)=netcdf_info%xdim_scan
      dims2d(2)=netcdf_info%ydim_scan

      if (.not. use_chunking) then
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%endy-imager_geolocation%starty+1
      else
         chunksize2d(1)=imager_geolocation%endx-imager_geolocation%startx+1
         chunksize2d(2)=imager_geolocation%ny
      end if

      ! define u variable
      ierr = nf90_def_var(netcdf_info%ncid_scan, 'uscan', NF90_INT, dims2d, &
           netcdf_info%uscanid, deflate_level=compress_level_lint, &
           shuffle=shuffle_lint)!,chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def scan u'
      ierr = nf90_put_att(netcdf_info%ncid_scan, netcdf_info%uscanid, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue u scan'

      ! define v variable
      ierr = nf90_def_var(netcdf_info%ncid_scan, 'vscan', NF90_INT, dims2d, &
           netcdf_info%vscanid, deflate_level=compress_level_lint, &
           shuffle=shuffle_lint)!, chunksizes=chunksize2d)
      if (ierr.ne.NF90_NOERR) stop 'error: def scan v'
      ierr = nf90_put_att(netcdf_info%ncid_scan, netcdf_info%vscanid, &
           '_FillValue', lint_fill_value)
      if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue v scan'

   end if


   ! set up attributes common to all output files
   if (type .eq. 1) ncid=netcdf_info%ncid_msi
   if (type .eq. 2) ncid=netcdf_info%ncid_cf
   if (type .eq. 3) ncid=netcdf_info%ncid_lsf
   if (type .eq. 4) ncid=netcdf_info%ncid_geo
   if (type .eq. 5) ncid=netcdf_info%ncid_loc
   if (type .eq. 6) ncid=netcdf_info%ncid_alb
   if (type .eq. 7) ncid=netcdf_info%ncid_scan

   call netcdf_put_common_attributes(ncid,global_atts,ctitle,platform,sensor, &
        path,cyear,cmonth,cday,chour,cminute)


   ! close definition section
   ierr = nf90_enddef(ncid)
   if (ierr.ne.NF90_NOERR) stop 'error: enddef swath'

   if (wo.eq.1) then
      write(*,*) ''
      write(*,*) 'New file created: ',TRIM(path)
   end if

   return

end subroutine netcdf_create_swath

!-------------------------------------------------------------------------------
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! global_atts    struct  in   Structure detailing NCDF header contents.
! cyear          string  in   Year, as a 4 character string.
! cmonth         string  in   Month of year, as a 2 character string.
! cday           string  in   Day of month, as a 2 character string.
! chour          string  in   Hour of day, as a 2 character string.
! cminute        string  in   Minute of day, as a 2 character string.
! platform       string  in   Name of satellite platform.
! sensor         string  in   Name of sensor.
! path           string  in   Name of file to create.
! wo             integer in   1: Print procedure's success; 0: Don't.
! preproc_dims   struct  in   Summary of preprocessing grid definitions.
! imager_geolocation
!                struct  in   Summary of pixel positions
! netcdf_info    struct  both Summary of NCDF file properties.
! channel_info   struct  in   Structure summarising the channels to be processed.
!
! History:
! 2013/11/06, MJ: initial routine version
! 2014/02/02, GM: adds chunking on/off option and cleans up code.
! 2014/02/02, GM: puts setting up of common attributes in a subroutine used by
!    all the nc_create_file_*() routines.
!
!-------------------------------------------------------------------------------

subroutine netcdf_create_config(global_atts,cyear,cmonth,cday,chour,cminute, &
     platform,sensor,path,wo,preproc_dims,imager_geolocation,netcdf_info, &
     channel_info)

   use netcdf

   use channel_structures
   use global_attributes
   use imager_structures
   use netcdf_structures
   use preproc_constants
   use preproc_structures

   implicit none

   ! Input
   type(global_attributes_s),      intent(in)    :: global_atts
   character(len=date_length),     intent(in)    :: cyear
   character(len=date_length),     intent(in)    :: cmonth
   character(len=date_length),     intent(in)    :: cday
   character(len=date_length),     intent(in)    :: chour
   character(len=date_length),     intent(in)    :: cminute
   character(len=platform_length), intent(in)    :: platform
   character(len=sensor_length),   intent(in)    :: sensor
   character(len=*),               intent(in)    :: path
   integer,                        intent(in)    :: wo
   type(preproc_dims_s),           intent(in)    :: preproc_dims
   type(imager_geolocation_s),     intent(in)    :: imager_geolocation
   type(netcdf_info_s),            intent(inout) :: netcdf_info
   type(channel_info_s),           intent(in)    :: channel_info

   ! Local
   integer                    :: ierr
   integer                    :: nlon_x_nlat
   character(len=file_length) :: ctitle
   integer                    :: ncid


   ctitle='ORAC Preprocessing config  file'


   ierr = nf90_create(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), &
        netcdf_info%ncid_config)
   if (ierr.ne.NF90_NOERR) stop 'error: config creating file'


   ! start defining things
   ierr = nf90_redef(netcdf_info%ncid_config)


   ! define x and y
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nx_conf', &
        imager_geolocation%endx-imager_geolocation%startx+1, &
        netcdf_info%xdim_config)
   if (ierr.ne.NF90_NOERR) stop 'error: create x-d conf'

   ierr = nf90_def_dim(netcdf_info%ncid_config, 'ny_conf', &
        imager_geolocation%endy-imager_geolocation%starty+1, &
        netcdf_info%ydim_config)
   if (ierr.ne.NF90_NOERR) stop 'error: create y-d conf'

   ! define nviews
   !  ierr = nf90_def_dim(netcdf_info%ncid_config, 'nv_msi', &
   !       imager_angles%nviews, &
   !       netcdf_info%vdim_msi)
   !  if (ierr.ne.NF90_NOERR) stop 'error: create v-d msi'

   ! define nchannels
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nc_conf', &
        channel_info%nchannels_total, &
        netcdf_info%cdim_config)
   if (ierr.ne.NF90_NOERR) stop 'error: create c-d conf'

   ! define some channel variables
   ierr = nf90_def_var(netcdf_info%ncid_config, 'msi_instr_ch_numbers', &
        NF90_INT, netcdf_info%cdim_config, netcdf_info%channelninid_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def conf channel n'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channelninid_config, '_FillValue', lint_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue conf channel n'

   ierr = nf90_def_var(netcdf_info%ncid_config, 'msi_abs_ch_numbers', NF90_INT, &
        netcdf_info%cdim_config, netcdf_info%channelnabsid_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def conf channel n abs'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channelnabsid_config, '_FillValue', lint_fill_value)
   if (ierr.ne.NF90_NOERR) &
        write(*,*) 'error: def var FillValue conf channel n abs'

   ierr = nf90_def_var(netcdf_info%ncid_config, 'msi_abs_ch_wl', NF90_FLOAT, &
        netcdf_info%cdim_config, netcdf_info%channelwlabsid_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def conf channel wl abs'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channelwlabsid_config, '_FillValue', sreal_fill_value)
   if (ierr.ne.NF90_NOERR) &
        write(*,*) 'error: def var FillValue conf channel wl abs'

   ierr = nf90_def_var(netcdf_info%ncid_config, 'msi_ch_swflag', NF90_INT, &
        netcdf_info%cdim_config, netcdf_info%channelswflag_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def conf channel swf'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channelswflag_config, '_FillValue', lint_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue conf channel swf'

   ierr = nf90_def_var(netcdf_info%ncid_config, 'msi_ch_lwflag', NF90_INT, &
        netcdf_info%cdim_config, netcdf_info%channellwflag_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def msi channel lwf'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channellwflag_config, '_FillValue', lint_fill_value)
   if (ierr.ne.NF90_NOERR) write(*,*) 'error: def var FillValue conf channel lwf'

   ierr = nf90_def_var(netcdf_info%ncid_config, 'msi_ch_procflag', NF90_INT, &
        netcdf_info%cdim_config, netcdf_info%channelprocflag_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def conf channel proc'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channelprocflag_config, '_FillValue', lint_fill_value)
   if (ierr.ne.NF90_NOERR) &
        write(*,*) 'error: def var FillValue conf channel proc'

   ! define nchannels albedo
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nc_alb', &
        channel_info%nchannels_sw, &
        netcdf_info%cdim_config_alb)
   if (ierr.ne.NF90_NOERR) stop 'error: create c-d alb conf'

   ! define nchannels emissivity
   ierr = nf90_def_dim(netcdf_info%ncid_config, 'nc_emis', &
        channel_info%nchannels_lw, &
        netcdf_info%cdim_config_emis)
   if (ierr.ne.NF90_NOERR) stop 'error: create c-d emi conf'

   ierr = nf90_def_var(netcdf_info%ncid_config, 'alb_abs_ch_numbers', NF90_INT, &
        netcdf_info%cdim_config_alb, netcdf_info%channelnalbid_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def alb conf channel n abs'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channelnalbid_config, '_FillValue', lint_fill_value)
   if (ierr.ne.NF90_NOERR) &
        write(*,*) 'error: def var FillValue alb conf channel n abs'

   ierr = nf90_def_var(netcdf_info%ncid_config, 'emis_abs_ch_numbers', &
        NF90_INT, netcdf_info%cdim_config_emis,netcdf_info%channelnemisid_config)
   if (ierr.ne.NF90_NOERR) stop 'error: def emis conf channel n abs'
   ierr = nf90_put_att(netcdf_info%ncid_config, &
        netcdf_info%channelnemisid_config, '_FillValue', lint_fill_value)
   if (ierr.ne.NF90_NOERR) &
        write(*,*) 'error: def var FillValue emis conf channel n abs'

   if (.false.) then
      nlon_x_nlat=(preproc_dims%max_lat-preproc_dims%min_lat+1)* &
           (preproc_dims%max_lon-preproc_dims%min_lon+1)

      ! define horizontal dimension as one big vector containing all pixels
      ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlon_x_nlat_lwrtm', &
           nlon_x_nlat, netcdf_info%xydim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create xy-d 2'

      ! define lon and lat just for reference
      ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlon_lwrtm', &
           preproc_dims%max_lon-preproc_dims%min_lon+1, &
           netcdf_info%xdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create x-d'

      ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlat_lwrtm', &
           preproc_dims%max_lat-preproc_dims%min_lat+1, &
           netcdf_info%ydim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create y-d'

      ! layer land level dimension
      ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlayers_lwrtm', &
           preproc_dims%kdim-1, netcdf_info%layerdim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlay lw'

      ierr = nf90_def_dim(netcdf_info%ncid_config, 'nlevels_lwrtm', &
           preproc_dims%kdim, netcdf_info%leveldim_lw)
      if (ierr.ne.NF90_NOERR) stop 'error: create nlev lw'
   end if

   ncid=netcdf_info%ncid_config


   ! set up attributes common to all output files
   call netcdf_put_common_attributes(ncid,global_atts,ctitle,platform,sensor, &
        path, cyear,cmonth,cday,chour,cminute)


   ! close definition section
   ierr = nf90_enddef(ncid)
   if (ierr.ne.NF90_NOERR) stop 'error: enddef swath'

   if (wo.eq.1) then
      write(*,*) ''
      write(*,*) 'New file created: ',TRIM(path)
   end if

   return

end subroutine netcdf_create_config


subroutine netcdf_put_common_attributes(ncid,global_atts,title,platform,sensor, &
                                        path,cyear,cmonth,cday,chour,cminute)

   use netcdf

   use global_attributes
   use nc_utils

   implicit none

   integer,                        intent(in) :: ncid
   type(global_attributes_s),      intent(in) :: global_atts
   character(len=file_length),     intent(in) :: title
   character(len=platform_length), intent(in) :: platform
   character(len=sensor_length),   intent(in) :: sensor
   character(len=*),               intent(in) :: path
   character(len=date_length),     intent(in) :: cyear
   character(len=date_length),     intent(in) :: cmonth
   character(len=date_length),     intent(in) :: cday
   character(len=date_length),     intent(in) :: chour
   character(len=date_length),     intent(in) :: cminute

   character(len=platform_length) :: PLATFORM_UPPER_CASE
   integer                        :: position,length
   type(global_attributes_s)      :: global_atts2

   global_atts2 = global_atts

   global_atts2%title  = trim(title)
   global_atts2%source = 'source!!!'

   position=index(trim(path),'/',back=.true.)
   length=len_trim(path)
   global_atts2%File_Name    = trim(path(position+1:length))

   global_atts2%Product_Name = 'Product_Name!!!'

   global_atts2%Product_Date = trim(cyear)//trim(cmonth)//trim(cday)// &
                               trim(chour)//trim(cminute)

   PLATFORM_UPPER_CASE=platform
   if (platform(1:4) .eq. 'noaa') PLATFORM_UPPER_CASE(1:4)='NOAA'
   global_atts2%platform = trim(PLATFORM_UPPER_CASE)
   global_atts2%sensor   = trim(sensor)

   global_atts2%AATSR_Processing_Version = ' '
   if (sensor .eq. 'ATSR' .or. sensor .eq. 'AATSR') then
      global_atts2%AATSR_Processing_Version = '3.01'
   endif

   call nc_put_common_attributes(ncid, global_atts2)

end subroutine netcdf_put_common_attributes
