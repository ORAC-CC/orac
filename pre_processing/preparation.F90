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
! sensor           string in  Name of sensor.
! platform         string in  Name of satellite platform.
! hour             stint  in  Hour of day (0-59)
! cyear            string in  Year, as a 4 character string
! cmonth           string in  Month of year, as a 2 character string
! cday             string in  Day of month, as a 2 character string
! chour            string in  Hour of day, as a 2 character string
! cminute          string in  Minute of day, as a 2 character string
! ecmwf_path       string in  If badc, folder in which to find GGAM files.
!                             Otherwise, folder in which to find GRB files.
! ecmwf_path2      string in  If badc, folder in which to find GGAS files.
! ecmwf_path3      string in  If badc, folder in which to find GPAM files.
! ecmwf_path_file  string out If badc, full path to appropriate GGAM file.
!                             Otherwise, full path to appropriate GRB file.
! ecmwf_path_file2 string out If badc, full path to appropriate GGAS file.
! ecmwf_path_file3 string out If badc, full path to appropriate GPAM file.
! script_input     struct in  Summary of file header information.
! ecmwf_flag       int    in  0: GRIB ECMWF files; 1: BADC NetCDF ECMWF files;
!                             2: BADC GRIB files.
! imager_geolocat  ion        struct both Summary of pixel positions
! i_chunk          stint  in  The number of the current chunk (for AATSR).
! assume_full_pat  h
!                  logic  in  T: inputs are filenames; F: folder names
! verbose          logic  in  T: print status information; F: don't
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

subroutine preparation(paths, sensor, platform, product_name, &
     cyear, cmonth, cday, chour, cminute, orbit_number, ecmwf_path, ecmwf_hr_path, &
     ecmwf_path2, ecmwf_path3, ecmwf_path_file, ecmwf_hr_path_file, &
     ecmwf_path_file2, ecmwf_path_file3, global_atts, ecmwf_flag, &
     ecmwf_time_int_method, imager_geolocation, imager_time, i_chunk, &
     time_int_fac, assume_full_path, verbose)

   use imager_structures_m
   use global_attributes_m
   use preproc_constants_m
   use preproc_structures_m, only: preproc_paths_t

   implicit none

   type(preproc_paths_t),      intent(out)   :: paths
   character(len=*),           intent(in)  :: sensor
   character(len=*),           intent(in)  :: platform
   character(len=*),           intent(in)  :: product_name
   character(len=*),           intent(in)  :: cyear, cmonth, cday, chour, cminute
   character(len=*),           intent(in)  :: orbit_number
   character(len=*),           intent(in)  :: ecmwf_path(:), &
                                              ecmwf_path2(:), &
                                              ecmwf_path3(:)
   character(len=*),           intent(in)  :: ecmwf_hr_path(:)
   character(len=*),           intent(out) :: ecmwf_path_file(:), &
                                              ecmwf_hr_path_file(:), &
                                              ecmwf_path_file2(:), &
                                              ecmwf_path_file3(:)
   type(global_attributes_t),  intent(in)  :: global_atts
   integer,                    intent(in)  :: ecmwf_flag
   integer,                    intent(in)  :: ecmwf_time_int_method
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

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering preparation()'

   if (verbose) write(*,*) 'sensor: ',                trim(sensor)
   if (verbose) write(*,*) 'platform: ',              trim(platform)
   if (verbose) write(*,*) 'cyear: ',                 trim(cyear)
   if (verbose) write(*,*) 'cmonth: ',                trim(cmonth)
   if (verbose) write(*,*) 'cday: ',                  trim(cday)
   if (verbose) write(*,*) 'chour: ',                 trim(chour)
   if (verbose) write(*,*) 'cminute: ',               trim(cminute)
   if (verbose) write(*,*) 'orbit_number: ',          trim(orbit_number)
   if (verbose) write(*,*) 'ecmwf_path(1): ',         trim(ecmwf_path(1))
   if (verbose) write(*,*) 'ecmwf_hr_path(1): ',      trim(ecmwf_hr_path(1))
   if (verbose) write(*,*) 'ecmwf_path2(1): ',        trim(ecmwf_path2(1))
   if (verbose) write(*,*) 'ecmwf_path3(1): ',        trim(ecmwf_path3(1))
   if (verbose) write(*,*) 'ecmwf_path(2): ',         trim(ecmwf_path(2))
   if (verbose) write(*,*) 'ecmwf_hr_path(2): ',      trim(ecmwf_hr_path(2))
   if (verbose) write(*,*) 'ecmwf_path2(2): ',        trim(ecmwf_path2(2))
   if (verbose) write(*,*) 'ecmwf_path3(2): ',        trim(ecmwf_path3(2))
   if (verbose) write(*,*) 'ecmwf_flag: ',            ecmwf_flag
   if (verbose) write(*,*) 'ecmwf_time_int_method: ', ecmwf_time_int_method
   if (verbose) write(*,*) 'i_chunk: ',               i_chunk
   if (verbose) write(*,*) 'assume_full_path: ',      assume_full_path

   ! determine ecmwf path/filename
   call set_ecmwf(cyear, cmonth, cday, chour, &
                  ecmwf_path, ecmwf_path2, ecmwf_path3, &
                  ecmwf_path_file, ecmwf_path_file2, ecmwf_path_file3, &
                  ecmwf_flag, imager_geolocation, imager_time, &
                  ecmwf_time_int_method, time_int_fac, assume_full_path, &
                  ecmwf_hr_path, ecmwf_hr_path_file)

   if (verbose) then
      write(*,*) 'ecmwf_path_file:  ', trim(ecmwf_path_file(1))
      write(*,*) 'ecmwf_path_file_2:  ', trim(ecmwf_path_file(2))
      write(*,*) 'ecmwf_hr_path_file:  ', trim(ecmwf_hr_path_file(1))
      write(*,*) 'ecmwf_hr_path_file2:  ', trim(ecmwf_hr_path_file(2))
      if (ecmwf_flag .gt. 0.and.ecmwf_flag.lt.4) then
         write(*,*) 'ecmwf_path_file2: ', trim(ecmwf_path_file2(1))
         write(*,*) 'ecmwf_path_file3: ', trim(ecmwf_path_file3(1))
         write(*,*) 'ecmwf_path_file2_2: ', trim(ecmwf_path_file2(2))
         write(*,*) 'ecmwf_path_file3_2: ', trim(ecmwf_path_file3(2))
      end if
   end if

   ! deal with ATSR chunking in filename
   if (sensor .eq. 'AATSR' .or. sensor .eq. 'ATSR2') then
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
               trim(adjustl(product_name))//'-'// &
               trim(adjustl(sensor))//'_'// &
               trim(adjustl(global_atts%l2_processor))//'_'// &
               trim(adjustl(platform))//'_'// &
               trim(adjustl(cyear))//trim(adjustl(cmonth))//trim(adjustl(cday))// &
               trim(adjustl(chour))//trim(adjustl(cminute))//'_'
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
