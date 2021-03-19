!-------------------------------------------------------------------------------
! Name: preparation.F90
!
! Purpose:
! Determines the names for the various output files.
!
! Description and Algorithm details:
! 1) Work out limits of ATSR chunks for file name.
! 2) Compile base file name.
! 3) Set paths for ECMWF processing.
! 4) Produce filenames for all outputs.
!
! Arguments:
! Name             Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! paths            struct out  Paths to the ten ORAC preprocessor outputs.
! granule          struct in   Parameters of the swath file
! opts             struct both Processing options
! global_atts      struct in   Attributes for NCDF output
! orbit_number     strint in   Number of SLSTR orbit
! ecmwf_flag       int    in   0: GRIB ECMWF files; 1: BADC NetCDF ECMWF files;
!                              2: BADC GRIB files.
! imager_geolocation struct in Summary of pixel positions
! imager_time      struct in   Time of observations
! i_chunk          stint  in   The number of the current chunk (for AATSR).
! time_int_fac     real   out  Value for interpolating between ECWMF fields
! assume_full_path logic  in   T: inputs are filenames; F: folder names
! verbose          logic  in   T: print status information; F: don't
!
! History:
! 2011/12/12, MJ: produces draft code which sets up output file names
! 2012/01/16, MJ: includes subroutine to determine ERA interim file.
! 2012/02/14, MJ: implements filenames and attributes for netcdf output.
! 2012/07/29, CP: removed old comments
! 2012/08/06, CP: added in badc flag
! 2012/12/06, CP: added in option to break aatsr orbit into chunks for faster
!   processing added imager_structure to input and tidied up the file
! 2012/12/06, CP: changed how ecmwf paths are defined because of looping chunks
! 2012/12/14, CP: changed how file is named if the orbit is broken into
!   granules then the file name is given a latitude range
! 2012/03/05, CP: small change to work for gfortran
! 2013/09/02, AP: Removed startyi, endye.
! 2013/10/21, AP: Removed redundant arguments. Tidying.
! 2014/02/03, AP: made badc a logical variable
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/05/01, GM: Reordered data/time arguments into a logical order.
! 2014/05/02, AP: Made badc into ecmwf_flag.
! 2014/05/02, CP: Changed AATSR file naming
! 2015/08/08, CP: Added functionality for ATSR-2
! 2015/11/17, OS: Building high resolution ERA-Interim file name from low
!   resolution ERA file, appending suffix "_HR":
!   path/to/low_res_era_file.nc = path/to/low_res_era_file_HR.nc
! 2015/11/26, GM: Changes to support temporal interpolation between ecmwf files.
! 2015/12/17, OS: Added write of HR file 2.
! 2015/02/02, CP: Added optional HR path input.
! 2016/04/04, SP: Added option to process ECMWF forecast/analysis data
!    that's stored in a single NetCDF file.
! 2016/05/26, GT: Added code for automatically constructing the filenames
!    of the HR ERA data (copied from changes made, but committed to R3970
!    version of code by CP).
! 2016/07/31, GM: Tidying of the code drop above.
! 2017/04/11, SP: Added ecmwf_flag=6, for working with GFS analysis files.
! 2017/09/14, GT: Added product_name argument, which replaces the
!    '-L2-CLOUD-CLD-' string in the output filenames
! 2018/02/01, GT: If a orbit number has been included in the source_attributes,
!    it is now included in the output file names (between date/time and product
!    version fields)
! 2021/03/09, AP: Consolidate path arguments into structure
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module preparation_m

   implicit none

contains

#include "set_ecmwf.F90"

subroutine preparation(paths, granule, opts, global_atts, orbit_number, &
     ecmwf_flag, imager_geolocation, imager_time, i_chunk, &
     time_int_fac, assume_full_path, verbose)

   use imager_structures_m
   use global_attributes_m
   use preproc_constants_m
   use preproc_structures_m, only: preproc_opts_t, preproc_paths_t
   use setup_m, only: setup_args_t

   implicit none

   type(preproc_paths_t),      intent(out)   :: paths
   type(setup_args_t),         intent(in)    :: granule
   type(preproc_opts_t),       intent(inout) :: opts
   type(global_attributes_t),  intent(in)  :: global_atts
   character(len=*),           intent(in)    :: orbit_number
   integer,                    intent(in)  :: ecmwf_flag
   type(imager_geolocation_t), intent(in)  :: imager_geolocation
   type(imager_time_t),        intent(in)  :: imager_time
   integer,                    intent(in)  :: i_chunk
   real,                       intent(out) :: time_int_fac
   logical,                    intent(in)  :: assume_full_path
   logical,                    intent(in)  :: verbose

   character(len=file_length) :: range_name
   character(len=file_length) :: file_base
   real                       :: startr, endr
   character(len=32)          :: startc, endc, chunkc

   if (verbose) then
      write(*,*) '<<<<<<<<<<<<<<< Entering preparation()'

      write(*,*) 'sensor: ',                trim(granule%sensor)
      write(*,*) 'platform: ',              trim(granule%platform)
      write(*,*) 'cyear: ',                 trim(granule%cyear)
      write(*,*) 'cmonth: ',                trim(granule%cmonth)
      write(*,*) 'cday: ',                  trim(granule%cday)
      write(*,*) 'chour: ',                 trim(granule%chour)
      write(*,*) 'cminute: ',               trim(granule%cminute)
      write(*,*) 'orbit_number: ',          trim(orbit_number)
      write(*,*) 'ecmwf_path(1): ',         trim(opts%ecmwf_path(1))
      write(*,*) 'ecmwf_path_hr(1): ',      trim(opts%ecmwf_path_hr(1))
      write(*,*) 'ecmwf_path2(1): ',        trim(opts%ecmwf_path2(1))
      write(*,*) 'ecmwf_path3(1): ',        trim(opts%ecmwf_path3(1))
      write(*,*) 'ecmwf_path(2): ',         trim(opts%ecmwf_path(2))
      write(*,*) 'ecmwf_path_hr(2): ',      trim(opts%ecmwf_path_hr(2))
      write(*,*) 'ecmwf_path2(2): ',        trim(opts%ecmwf_path2(2))
      write(*,*) 'ecmwf_path3(2): ',        trim(opts%ecmwf_path3(2))
      write(*,*) 'ecmwf_flag: ',            ecmwf_flag
      write(*,*) 'ecmwf_time_int_method: ', opts%ecmwf_time_int_method
      write(*,*) 'i_chunk: ',               i_chunk
      write(*,*) 'assume_full_path: ',      assume_full_path
   end if

   ! determine ecmwf path/filename
   call set_ecmwf(granule, opts, ecmwf_flag, imager_geolocation, imager_time, &
        time_int_fac, assume_full_path)

   if (verbose) then
      write(*,*) 'ecmwf_path_file:  ', trim(opts%ecmwf_path_file(1))
      write(*,*) 'ecmwf_path_file_2:  ', trim(opts%ecmwf_path_file(2))
      write(*,*) 'ecmwf_hr_path_file:  ', trim(opts%ecmwf_hr_path_file(1))
      write(*,*) 'ecmwf_hr_path_file2:  ', trim(opts%ecmwf_hr_path_file(2))
      if (ecmwf_flag .gt. 0.and.ecmwf_flag.lt.4) then
         write(*,*) 'ecmwf_path_file2: ', trim(opts%ecmwf_path_file2(1))
         write(*,*) 'ecmwf_path_file3: ', trim(opts%ecmwf_path_file3(1))
         write(*,*) 'ecmwf_path_file2_2: ', trim(opts%ecmwf_path_file2(2))
         write(*,*) 'ecmwf_path_file3_2: ', trim(opts%ecmwf_path_file3(2))
      end if
   end if

   ! deal with ATSR chunking in filename
   if (granule%sensor .eq. 'AATSR' .or. granule%sensor .eq. 'ATSR2') then
      startr = imager_geolocation%latitude(imager_geolocation%startx, 1)
      endr = imager_geolocation%latitude(imager_geolocation%endx, &
             imager_geolocation%ny)

      ! convert latitudes into strings
      write(chunkc, '( g12.3 )') i_chunk
      write(startc, '( g12.3 )') startr
      write(endc,   '( g12.3 )') endr

      range_name = trim(adjustl(chunkc))//'-'// &
                   trim(adjustl(startc))//'-'//trim(adjustl(endc))//'_'
   else
      range_name = ''
   end if

   if (verbose) write(*,*) 'chunk range_name: ', trim(range_name)

   ! ESACCI-L2-CLOUD-CLD-${sensor}_${product_string}_${platform}_*${YYYY}${MM}${DD}${HH}${II}_${version2}.*.nc

   ! put basic filename together
   file_base = trim(adjustl(global_atts%project))//'-'// &
               trim(adjustl(opts%product_name))//'-'// &
               trim(adjustl(granule%sensor))//'_'// &
               trim(adjustl(global_atts%l2_processor))//'_'// &
               trim(adjustl(granule%platform))//'_'// &
               trim(adjustl(granule%cyear))//trim(adjustl(granule%cmonth))// &
               trim(adjustl(granule%cday))// &
               trim(adjustl(granule%chour))//trim(adjustl(granule%cminute))//'_'
   ! If we have a orbit number attribute, include it in the output file name
   ! As of Feb-18, this only applies to SLSTR.
   if (trim(orbit_number) .ne. 'null') then
      file_base = trim(adjustl(file_base))//trim(orbit_number)//'_'
   end if
   file_base = trim(adjustl(file_base))//trim(adjustl(global_atts%file_version))

   if (verbose) write(*,*) 'output file_base: ', trim(file_base)

   ! get preproc filenames
   paths%lwrtm_file = trim(adjustl(file_base))//'.lwrtm.nc'
   paths%swrtm_file = trim(adjustl(file_base))//'.swrtm.nc'
   paths%prtm_file = trim(adjustl(file_base))//'.prtm.nc'
   paths%config_file = trim(adjustl(file_base))//'.config.nc'
   paths%msi_file = trim(adjustl(file_base))//'.msi.nc'
   paths%cf_file = trim(adjustl(file_base))//'.clf.nc'
   paths%lsf_file = trim(adjustl(file_base))//'.lsf.nc'
   paths%geo_file = trim(adjustl(file_base))//'.geo.nc'
   paths%loc_file = trim(adjustl(file_base))//'.loc.nc'
   paths%alb_file = trim(adjustl(file_base))//'.alb.nc'

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving preparation()'

end subroutine preparation

end module preparation_m
