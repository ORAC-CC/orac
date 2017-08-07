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
! lwrtm_file       string out Full path to output file LW RTM.
! swrtm_file       string out Full path to output file SW RTM.
! prtm_file        string out Full path to output file pressure RTM.
! config_file      string out Full path to output file configuration data.
! msi_file         string out Full path to output file multispectral imagery.
! cf_file          string out Full path to output file cloud flag.
! lsf_file         string out Full path to output file land/sea flag.
! geo_file         string out Full path to output file geolocation.
! loc_file         string out Full path to output file location.
! alb_file         string out Full path to output file albedo.
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module preparation_m

   implicit none

contains

#include "set_ecmwf.F90"

subroutine preparation(lwrtm_file,swrtm_file,prtm_file,config_file,msi_file, &
     cf_file,lsf_file,geo_file,loc_file,alb_file,sensor,platform,cyear,cmonth, &
     cday,chour,cminute,ecmwf_path,ecmwf_hr_path,ecmwf_path2,ecmwf_path3, &
     ecmwf_path_file,ecmwf_hr_path_file,ecmwf_path_file2,ecmwf_path_file3, &
     global_atts,ecmwf_flag,ecmwf_time_int_method,imager_geolocation,&
     imager_time,i_chunk,time_int_fac,assume_full_path,verbose)

   use imager_structures_m
   use global_attributes_m
   use preproc_constants_m

   implicit none

   character(len=file_length),     intent(out) :: lwrtm_file,swrtm_file, &
                                                  prtm_file,config_file, &
                                                  msi_file,cf_file,lsf_file, &
                                                  geo_file,loc_file,alb_file
   character(len=sensor_length),   intent(in)  :: sensor
   character(len=platform_length), intent(in)  :: platform
   character(len=date_length),     intent(in)  :: cyear,cmonth,cday,chour,cminute
   character(len=path_length),     intent(in)  :: ecmwf_path(2), &
                                                  ecmwf_path2(2), &
                                                  ecmwf_path3(2)
   character(len=path_length),     intent(in)  :: ecmwf_hr_path(2)
   character(len=path_length),     intent(out) :: ecmwf_path_file(2), &
                                                  ecmwf_hr_path_file(2), &
                                                  ecmwf_path_file2(2), &
                                                  ecmwf_path_file3(2)
   type(global_attributes_t),      intent(in)  :: global_atts
   integer,                        intent(in)  :: ecmwf_flag
   integer,                        intent(in)  :: ecmwf_time_int_method
   type(imager_geolocation_t),     intent(in)  :: imager_geolocation
   type(imager_time_t),            intent(in)  :: imager_time
   integer,                        intent(in)  :: i_chunk
   real,                           intent(out) :: time_int_fac
   logical,                        intent(in)  :: assume_full_path
   logical,                        intent(in)  :: verbose

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
   call set_ecmwf(cyear,cmonth,cday,chour, &
                  ecmwf_path,     ecmwf_path2,     ecmwf_path3, &
                  ecmwf_path_file,ecmwf_path_file2,ecmwf_path_file3, &
                  ecmwf_flag,imager_geolocation,imager_time, &
                  ecmwf_time_int_method,time_int_fac,assume_full_path, &
                  ecmwf_hr_path, ecmwf_hr_path_file)

   if (verbose) then
      write(*,*)'ecmwf_path_file:  ',trim(ecmwf_path_file(1))
      write(*,*)'ecmwf_path_file_2:  ',trim(ecmwf_path_file(2))
      write(*,*)'ecmwf_hr_path_file:  ',trim(ecmwf_hr_path_file(1))
      write(*,*)'ecmwf_hr_path_file2:  ',trim(ecmwf_hr_path_file(2))
      if (ecmwf_flag .gt. 0.and.ecmwf_flag.lt.4) then
         write(*,*)'ecmwf_path_file2: ',trim(ecmwf_path_file2(1))
         write(*,*)'ecmwf_path_file3: ',trim(ecmwf_path_file3(1))
         write(*,*)'ecmwf_path_file2_2: ',trim(ecmwf_path_file2(2))
         write(*,*)'ecmwf_path_file3_2: ',trim(ecmwf_path_file3(2))
      end if
   end if

   ! deal with ATSR chunking in filename
   if (sensor .eq. 'AATSR' .or. sensor .eq. 'ATSR2') then
      startr=imager_geolocation%latitude(imager_geolocation%startx,1)
      endr=imager_geolocation%latitude(imager_geolocation%endx, &
           imager_geolocation%ny)

      ! convert latitudes into strings
      write(chunkc,'( g12.3 )') i_chunk
      write(startc,'( g12.3 )') startr
      write(endc,  '( g12.3 )') endr

      range_name=trim(adjustl(chunkc))//'-'// &
                 trim(adjustl(startc))//'-'//trim(adjustl(endc))//'_'
   else
      range_name=''
   end if

   if (verbose) write(*,*) 'chunk range_name: ', trim(range_name)

   ! ESACCI-L2-CLOUD-CLD-${sensor}_${product_string}_${platform}_*${YYYY}${MM}${DD}${HH}${II}_${version2}.*.nc

   ! put basic filename together
   file_base=trim(adjustl(global_atts%project))// &
             '-L2-CLOUD-CLD-'// &
             trim(adjustl(sensor))//'_'// &
             trim(adjustl(global_atts%l2_processor))//'_'// &
             trim(adjustl(platform))//'_'// &
             trim(adjustl(cyear))//trim(adjustl(cmonth))//trim(adjustl(cday))// &
             trim(adjustl(chour))//trim(adjustl(cminute))//'_'// &
             trim(adjustl(global_atts%file_version))

   if (verbose) write(*,*) 'output file_base: ', trim(file_base)

   ! get preproc filenames
   lwrtm_file=trim(adjustl(file_base))//'.lwrtm.nc'
   swrtm_file=trim(adjustl(file_base))//'.swrtm.nc'
   prtm_file=trim(adjustl(file_base))//'.prtm.nc'
   config_file=trim(adjustl(file_base))//'.config.nc'
   msi_file=trim(adjustl(file_base))//'.msi.nc'
   cf_file=trim(adjustl(file_base))//'.clf.nc'
   lsf_file=trim(adjustl(file_base))//'.lsf.nc'
   geo_file=trim(adjustl(file_base))//'.geo.nc'
   loc_file=trim(adjustl(file_base))//'.loc.nc'
   alb_file=trim(adjustl(file_base))//'.alb.nc'

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving preparation()'

end subroutine preparation

end module preparation_m
