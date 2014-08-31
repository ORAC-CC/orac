!-------------------------------------------------------------------------------
! Name: preprocessing_for_orac.F90
!
! Purpose:
!   Read in data from the variety of files that contain all the information
!   necessary to run the ORAC algorithm, remove any unnecessary details (such as
!   restricting the data to a particular area of the swath), and write out the
!   remainder to a series of NetCDF files. This operates on individual granules
!   and/or orbit segments of MODIS, AVHRR, and AATSR to allow for brute force/
!   poor man's parallelization by running multiple instances of ORAC
!   simultaneously, each ingesting separate files.
!
! Description and Algorithm details:
! 1) Read input parameters from command line (or driver file)
! 2) Determine dimensions of scene, including breaking of AATSR files into more
!    easily processed 'chunks'
! 3) Read in level 1B radiances, brightness temperatures, and geolocation
! 4) Allocate structures to store data
! 5) Identify ECMWF data required
! 6) Set output file names
! 7) Read ecmwf data
! 8) Define processing grid
! 9) Interpolate ecmwf data onto pre processing grid
! 10) Open netcdf output
! 11) RTTOV calculations
! 12) surface calculations
! 13) deallocate structures!
!
! Arguments:
! Name             Type In/Out/Both      Description
! ------------------------------------------------------------------------------
! sensor           string in Name of instrument (only AATSR, MODIS or AVHRR are
!                            valid)
! l1b_path_file    string in Full path to level 1B data
! geo_path_file    string in Full path to geolocation data (identical to above
!                            for AATSR)
! ecmwf_path       string in Folder containing ECMWF files
! rttov_coef_path  string in Folder containing RTTOV coefficient files
! rttov_emiss_path string in Folder containing RTTOV emissivity files
! nise_ice_snow_path
!                  string in Folder containing NISE ice cover files
! modis_albedo_path
!                  string in Folder containing MODIS MCD43C3 albedo files
! cimss_emiss_path string in Folder containing MODIS monthly average emissivity
! dellon           real   in 1 / longitude increment in degrees
! dellat           real   in 1 / latitude increment in degrees
! output_pathin    string in Folder into which outputs should be saved
! startx           int    in First pixel across swath to consider, counting from
!                            1. This and the next four arguments are only
!                            considered if all four are > 0
! endx             int    in Last pixel across swath to consider
! starty           int    in First pixel along swath to consider
! endy             int    in Last pixel along swath to consider
! ncver            string in Version of NetCDF in use. This and all arguments
!                            until exec_time are descriptive information used to
!                            in the headers of the output files
! con              string in The NetCDF convention used
! inst             string in Processing institute
! l2cproc          string in Name of the L2 processor (generally ORAC)
! l2cprocver       string in Version of that processor
! contact          string in Contact email address for these files
! website          string in Web address related to these files
! file_version     string in Version number for these files
! reference        string in A reference (paper, report) for these files
! history          string in Changelog for these files
! summary          string in Brief description of the data
! keywords         string in Short phrases describing the purpose of these files
! comment          string in Any additional information that may be useful
! project          string in Name of the project for which these files were
!                            generated
! license          string in Any necessary licensing information for this data
! uuid_tag         string in A Universally Unique ID for these files
! exec_time        string in Date/time string for when this file was generated
! aatsr_calib_path_file
!                  string in Full path to the AATSR calibration file
! ecmwf_flag       int    in 0: GRIB ECMWF files; 1: BADC NetCDF ECMWF files;
!                            2: BADC GRIB files.
! ecmwf_path2      string in Folder containing ECMWF files (?)
! ecmwf_path3      string in Folder containing ECMWF files (?)
! chunkproc        logic  in T: split AATSR orbit in 4096 row chunks, F: don't
! day_night        int    in 2: process only night data; 1: day (default)
! verbose          logic  in F: minimise information printed to screen; T: don't
! use_chunking     logic  in T: apply chunking when writing NCDF files; F: don't
!
! History:
! 2011/12/09: MJ produces draft code which comprises the main program and the
!                top level subroutines.
! 2011/12/22: MJ includes read routines for MODIS L1B and geolocation HDF4
!                files.
! 2012/01/13: MJ includes read routines for ERA Interim GRIB files.
! 2012/02/01: MJ includes read routines for HDF5 AVHRR L1b and geolocation
!                files.
! 2012/02/14: MJ implements filenames and attributes for netcdf output.
! 2012/02/21: MJ adds code to produce preprocessing grid for RTTOV
!                implementation.
! 2012/02/22: MJ restructures ECMWF reading to prepare RTTOV implementation.
!                Variable naming is corrected as well.
! 2012/02/24: MJ implements code to acquire ECMWF fields on preprocessing grid,
!                to grid imager data to preprocessing grid.
! 2012/03/13: MJ fixes AVHRR read bug.
! 2012/03/26: MJ implements 3D pressure array code.
! 2012/03/26: MJ fixes bug in nearest neighbour assignment.
! 2012/03/27: MJ changes all file suffixes from *.f90 to *.F90 for ease of use
!                of the C preprocessor.
! 2012/05/15: MJ includes code to read AVHRR land/sea mask.
! 2012/05/15: CP includes path to albedo/snow files/emissivity
! 2012/07/15: CP add in AATSR reading including calibration file
! 2012/05/15: MJ removes slight bug in passing of parameters from surrounding
!                shell script.
! 2012/07/23: MJ adds capability to read from command line OR driver file.
! 2012/07/29: CP tidied up code and merged in MJ changes and  skips over
!                external emissivity read, improved readability, removed albedo
!                and icepaths from file these are now called in the running
!                script added in top level description
! 2012/07/29: CP added in year month day for ice file
! 2012/08/02: MJ implements writing of RTTOV output to netcdf file.
! 2012/08/06: CP modified code to accept BADC style ecmwf files
! 2012/08/07: CP modified code write albedo to netcdf file
! 2012/08/08: CP added in emissivity file selection
! 2012/08/13: CP modified reading of badc ecmwf files
! 2012/08/14: MJ some modifications to make code compile at DWD
! 2012/08/15: MJ initializes badc flag and rearranges reading
! 2012/08/16: GT Fixed up indentation of command-line argument reading for
!                clarity
! 2012/08/16: GT Bug-fix: Added "aatsr_calib_file" to the argument list for
!                read_imager
!                Added trim() to write statements of file paths
! 2012/08/20: MJ fixes several bugs with AATSR dimension read
! 2012/08/20: MJ changed read_mcd43c3 from function to subroutine in order to
!                iron out bugs
! 2012/08/22: MJ implements flexible x and y dimensions start and end indices in
!                surface part
! 2012/08/28: CP small mods
! 2012/09/04: GT Corrected calls to setup_aatsr and read_aatsr_dimensions
! 2012/09/13: GT Added write statements for some of the input variables to
!                aid in fixing problems with command arguments
! 2012/11/14: CP modified how netcdf ecmwf files on pressure levels are read
!                clean up of code
! 2012/11/29: CP big tidy up of code formatting writes etc
! 2012/12/06: CP added in option to break aatsr orbit into chunks for faster
!                processing
! 2012/12/06: CP changed how ecmwf paths are defined because of looping chunks!
! 2012/12/16: CP added an extra ecmwf path so ecmwf can be read form separate
!                directories
! 2013/02/26: CP changed how day_night aatsr flag was read in
! 2013/02/25: GT Added preproc_geoloc to the arguments for
!                get_surface_emissivity, un-commented the call to
!                get_surface_emissivity (which now works).
! 2013/03/06: GT Tidied up formatting of file header.
! 2013/03/06: GT Bug fix: default along_track_offset set to 0 rather than 1 and
!                definition of imager_geolocation%endy for whole orbits/granules
!                altered accordingly.
! 2013/03/07: CP changed badc file read options not all files are netcdf
! 2013/03/19: GT Bug fix: When reading BADC ECMWF data (NetCDF files), the ggam
!                files were being treated as surface, rather than profile, files
! 2013/04/08: CP/GT fixed bug where channel_info was not passed through
!                routine to correct for ice and snow.
! 2013/05/21: MJ merges local DWD version with official version.
! 2013/06/03: CP changed function find_min_max_preproc
! 2013/07/24: AP a few bug fixes in the chunking code
! 2013/09/02: AP removed various redundant variables and if statements. Removed
!                startyi, endye.
! 2013/09/03: GT Finished changes required to get AATSR night-time data
!                reading working correctly
! 2013/09/06: AP altering READ routines to take start,n as inputs rather than
!                start,end. Removed channel_flag from and added verbose to
!                command line arguments.
! 2013/10/08: AP altered read_aatsr_dimensions for new call.
! 2013/10/30: AP Continued tidying. Altered call for find_min_max_preproc.
! 2013/11/05: GT Changed type cast of cverbose to verbose variable to use
!                '(I6)', as '(L6)' was causing a buffer overrun.
! 2013/11/06: MJ adds config file to preprocessing output which holds all
!                relevant dimensional information.
! 2013/11/08: GM Added missing call to deallocate_surface_structures().
! 2014/01/24: MJ fixed type mismatch in deallocation of surface structures and
!                of variable nc.
! 2014/01/27: GM Made '1', 't', 'true', 'T', 'True', '0', 'f', 'false', 'F', and
!                'False' all valid values for the preprocessor verbose option.
! 2014/02/02: GM Added NetCDF chunking on/off option.
! 2014/02/03: AP Ensured all arguments that are logical flags are treated
!                identically
! 2014/02/05: MJ corrected data type of chunkproc from character to logical
! 2014/02/10: AP changes to ECMWF routines
! 2014/02/05: MJ adds verbose to argument list of rttov related routines to mute
!                rttov
! 2014/03/11: MJ adds writing out of some flags to gain more information.
! 2014/04/02: GM Get the NetCDF version from the library itself.  Left the
!                obsolete input argument in place for now but it is over
!                written.
! 2014/04/21: GM Added logical option assume_full_path.
! 2014/05/01: GM Cleaned up the code.
! 2014/05/01: GM Move some allocations/deallocations to the proper subroutine.
! 2014/06/04: MJ introduced "WRAPPER" for c-preprocessor and associated variables
! 2014/06/25: GM Rewrote along track chunking code fixing a bug where the second
!                segment in AATSR night processing was not being processed when
!                chunking was off.
! 2014/07/01: AP Update to ECMWF code.
! 2014/08/10: GM Changes related to new BRDF support.
!
! $Id$
!
! Bugs:
! See http://proj.badc.rl.ac.uk/orac/report/1
!-------------------------------------------------------------------------------

#ifndef WRAPPER
program preprocessing
#else
subroutine preprocessing(mytask,ntasks,lower_bound,upper_bound,driver_path_file)
#endif

   use attribute_structures
   use channel_structures
   use correct_for_ice_snow_m
   use ecmwf_m
   use hdf5
   use imager_structures
   use netcdf, only: nf90_inq_libvers
   use netcdf_output
   use netcdf_structures
   use preparation_m
   use preproc_constants
   use preproc_structures
   use read_aatsr
   use read_avhrr
   use read_modis
   use read_imager_m
   use rttov_driver_m
   use setup_instrument
   use surface_emissivity
   use surface_reflectance
   use surface_structures

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   character(len=path_length)       :: driver_path_file
   character(len=path_length)       :: l1b_path_file
   character(len=path_length)       :: geo_path_file
   character(len=path_length)       :: ecmwf_path,ecmwf_path_file
   character(len=path_length)       :: rttov_coef_path
   character(len=path_length)       :: rttov_emiss_path
   character(len=path_length)       :: nise_ice_snow_path
   character(len=path_length)       :: modis_albedo_path
   character(len=path_length)       :: cimss_emiss_path
   character(len=path_length)       :: output_path
   character(len=path_length)       :: aatsr_calib_path_file
   character(len=path_length)       :: ecmwf_path2,ecmwf_path_file2
   character(len=path_length)       :: ecmwf_path3,ecmwf_path_file3
   character(len=attribute_length)  :: cdellon,cdellat
   character(len=cmd_arg_length)    :: cstartx,cendx,cstarty,cendy
   character(len=cmd_arg_length)    :: cecmwf_flag
   character(len=cmd_arg_length)    :: cchunkproc
   character(len=cmd_arg_length)    :: cday_night
   character(len=cmd_arg_length)    :: cverbose
   character(len=cmd_arg_length)    :: cuse_chunking
   character(len=cmd_arg_length)    :: cassume_full_paths
   character(len=cmd_arg_length)    :: cinclude_full_brdf

   type(script_arguments_s)         :: script_input

   integer                          :: ecmwf_flag
   logical                          :: chunkproc
   integer(kind=sint)               :: day_night
   logical                          :: verbose
   logical                          :: use_chunking
   logical                          :: assume_full_paths
   logical                          :: include_full_brdf

   logical                          :: check
   integer                          :: nargs

   integer(kind=lint)               :: startx,endx,starty,endy
   integer(kind=lint)               :: n_across_track,n_along_track
   integer(kind=lint)               :: along_track_offset

   integer                          :: segment_starts(2)
   integer                          :: segment_ends(2)
   integer                          :: n_segments
   integer                          :: chunksize
   integer(kind=sint)               :: i_chunk
   integer                          :: n_chunks
   integer, allocatable             :: chunk_starts(:)
   integer, allocatable             :: chunk_ends(:)
   integer                          :: calc_n_chunks

   real(kind=sreal), dimension(4)   :: loc_limit

   ! The following are for lengths and offsets for the second section of
   ! nighttime data in an (A)ATSR orbit file:
   integer(kind=lint)               :: n_along_track2, along_track_offset2

   character(len=sensor_length)     :: sensor
   character(len=platform_length)   :: platform

   integer(kind=sint)               :: doy,year,month,day,hour,minute

   character(len=date_length)       :: cyear,cmonth,cday,cdoy,chour,cminute

   character(len=file_length)       :: lwrtm_file,swrtm_file,prtm_file
   character(len=file_length)       :: msi_file,cf_file,lsf_file,config_file
   character(len=file_length)       :: geo_file,loc_file,alb_file,scan_file

   type(channel_info_s)             :: channel_info

   type(imager_angles_s)            :: imager_angles
   type(imager_flags_s)             :: imager_flags
   type(imager_geolocation_s)       :: imager_geolocation
   type(imager_measurements_s)      :: imager_measurements
   type(imager_time_s)              :: imager_time

   type(ecmwf_s)                    :: ecmwf

   type(surface_s)                  :: surface

   type(preproc_dims_s)             :: preproc_dims
   type(preproc_geo_s)              :: preproc_geo
   type(preproc_geoloc_s)           :: preproc_geoloc
   type(preproc_prtm_s)             :: preproc_prtm
   type(preproc_lwrtm_s)            :: preproc_lwrtm
   type(preproc_swrtm_s)            :: preproc_swrtm
   type(preproc_surf_s)             :: preproc_surf

   type(netcdf_info_s)              :: netcdf_info

   logical                          :: parse_logical

!  integer, dimension(8)            :: values

   ! this is for the wrapper
#ifdef WRAPPER
   integer :: mytask,ntasks,lower_bound,upper_bound
#endif
!  include "sigtrap.F90"

   ! get number of arguments
#ifndef WRAPPER
   nargs = COMMAND_ARGUMENT_COUNT()
#else
   nargs=-1
#endif
   ! if more than one argument passed, all inputs on command line
   if (nargs .gt. 1) then
      call get_command_argument(1,sensor)
      call get_command_argument(2,l1b_path_file)
      call get_command_argument(3,geo_path_file)
      call get_command_argument(4,ecmwf_path)
      call get_command_argument(5,rttov_coef_path)
      call get_command_argument(6,rttov_emiss_path)
      call get_command_argument(7,nise_ice_snow_path)
      call get_command_argument(8,modis_albedo_path)
      call get_command_argument(9,cimss_emiss_path)
      call get_command_argument(10,cdellon)
      call get_command_argument(11,cdellat)
      call get_command_argument(12,output_path)
      call get_command_argument(13,cstartx)
      call get_command_argument(14,cendx)
      call get_command_argument(15,cstarty)
      call get_command_argument(16,cendy)
      call get_command_argument(17,script_input%cncver)
      call get_command_argument(18,script_input%ccon)
      call get_command_argument(19,script_input%cinst)
      call get_command_argument(20,script_input%l2cproc)
      call get_command_argument(21,script_input%l2cprocver)
      call get_command_argument(22,script_input%contact)
      call get_command_argument(23,script_input%website)
      call get_command_argument(24,script_input%file_version)
      call get_command_argument(25,script_input%reference)
      call get_command_argument(26,script_input%history)
      call get_command_argument(27,script_input%summary)
      call get_command_argument(28,script_input%keywords)
      call get_command_argument(29,script_input%comment)
      call get_command_argument(30,script_input%project)
      call get_command_argument(31,script_input%license)
      call get_command_argument(32,script_input%uuid_tag)
      call get_command_argument(33,script_input%exec_time)
      call get_command_argument(34,aatsr_calib_path_file)
      call get_command_argument(35,cecmwf_flag)
      call get_command_argument(36,ecmwf_path2)
      call get_command_argument(37,ecmwf_path3)
      call get_command_argument(38,cchunkproc)
      call get_command_argument(39,cday_night)
      call get_command_argument(40,cverbose)
      call get_command_argument(41,cuse_chunking)
      call get_command_argument(42,cassume_full_paths)
      call get_command_argument(43,cinclude_full_brdf)
   else
      if (nargs .eq. 1) then
      ! if just one argument => this is driver file which contains everything
         call get_command_argument(1,driver_path_file)
      else if (nargs .eq. -1) then
         write(*,*) 'inside preproc ',trim(adjustl(driver_path_file))
      end if

      open(11,file=trim(adjustl(driver_path_file)),status='old', &
           form='formatted')

      read(11,*) sensor
      read(11,*) l1b_path_file
      read(11,*) geo_path_file
      read(11,*) ecmwf_path
      read(11,*) rttov_coef_path
      read(11,*) rttov_emiss_path
      read(11,*) nise_ice_snow_path
      read(11,*) modis_albedo_path
      read(11,*) cimss_emiss_path
      read(11,*) cdellon
      read(11,*) cdellat
      read(11,*) output_path
      read(11,*) cstartx
      read(11,*) cendx
      read(11,*) cstarty
      read(11,*) cendy
      read(11,*) script_input%cncver
      read(11,*) script_input%ccon
      read(11,*) script_input%cinst
      read(11,*) script_input%l2cproc
      read(11,*) script_input%l2cprocver
      read(11,*) script_input%contact
      read(11,*) script_input%website
      read(11,*) script_input%file_version
      read(11,*) script_input%reference
      read(11,*) script_input%history
      read(11,*) script_input%summary
      read(11,*) script_input%keywords
      read(11,*) script_input%comment
      read(11,*) script_input%project
      read(11,*) script_input%license
      read(11,*) script_input%uuid_tag
      read(11,*) script_input%exec_time
      read(11,*) aatsr_calib_path_file
      read(11,*) cecmwf_flag
      read(11,*) ecmwf_path2
      read(11,*) ecmwf_path3
      read(11,*) cchunkproc
      read(11,*) cday_night
      read(11,*) cverbose
      read(11,*) cuse_chunking
      read(11,*) cassume_full_paths
      read(11,*) cinclude_full_brdf
      close(11)
   end if ! nargs gt 1

   ! get the NetCDF version
   script_input%cncver=nf90_inq_libvers()

   ! get the execution time in UTC
!  call date_and_time(VALUES=values)
!  write(script_input%exec_time, '(i4,i2.2,i2.2,i2.2,i2.2,i2.2)'), &
!        values(1), values(2), values(3), values(5), values(6), values(7)
!  write(*,*) script_input%exec_time
!  stop

   ! cast input strings into appropriate variables
   read(cdellon, '(f10.5)') preproc_dims%dellon
   read(cdellat, '(f10.5)') preproc_dims%dellat
   read(cstartx(1:len_trim(cstartx)), '(I6)') startx
   read(cendx(1:len_trim(cendx)), '(I6)') endx
   read(cstarty(1:len_trim(cstarty)), '(I6)') starty
   read(cendy(1:len_trim(cendy)), '(I6)') endy
   read(cday_night(1:len_trim(cday_night)), '(I6)') day_night
   read(cecmwf_flag(1:len_trim(cecmwf_flag)), '(I6)') ecmwf_flag
   chunkproc=parse_logical(cchunkproc)
   verbose=parse_logical(cverbose)
   use_chunking=parse_logical(cuse_chunking)
   assume_full_paths=parse_logical(cassume_full_paths)
   include_full_brdf=parse_logical(cinclude_full_brdf)

   ! initialise some counts, offset variables...
   along_track_offset=0
   along_track_offset2=0
   n_along_track2=0

   ! allocate array for channel information
   imager_angles%nviews=1
   channel_info%nchannels_total=6
   call allocate_channel_info(channel_info)

   ! determine platform, day, time, check if l1b and geo match
   if (verbose) &
      write(*,*) 'Determine platform, day, time, check if l1b and geo match'
   inquire(file=l1b_path_file,exist=check)
   if (.not. check) then
        write(*,*) 'ERROR: L1B file does not exist: ', trim(l1b_path_file)
        stop error_stop_code
   end if
   inquire(file=geo_path_file,exist=check)
   if (.not. check) then
        write(*,*) 'ERROR: GEO file does not exist: ', trim(geo_path_file)
        stop error_stop_code
   end if
   if (trim(adjustl(sensor)) .eq. 'MODIS') then
      call setup_modis(l1b_path_file,geo_path_file,platform,year,month,day, &
           doy,hour,minute,cyear,cmonth,cday,cdoy,chour,cminute,channel_info, &
           verbose)

      ! get dimensions of the modis granule
      call read_modis_dimensions(geo_path_file,n_across_track,n_along_track)

   else if (trim(adjustl(sensor)) .eq. 'AVHRR') then
      call setup_avhrr(l1b_path_file,geo_path_file,platform,year,month,day, &
           doy,hour,minute,cyear,cmonth,cday,cdoy,chour,cminute,channel_info, &
           verbose)

      ! get dimensions of the avhrr orbit
      call read_avhrr_dimensions(geo_path_file,n_across_track,n_along_track)

   else if (trim(adjustl(sensor)) .eq. 'AATSR') then
      call setup_aatsr(l1b_path_file,geo_path_file,platform,year,month,day, &
           doy,hour,minute,cyear,cmonth,cday,cdoy,chour,cminute,channel_info, &
           verbose)

      ! currently setup to do day only by default
      if (day_night .eq. 0) day_night=1
      loc_limit=(/ -90.0, -180.0, 90.0, 180.0 /)

      ! initialise the second length and offset variables

      ! Get array dimensions and along-track offset for the daylight side. If
      ! we're processing daylight data, we may want to chunk process after this.
      call read_aatsr_dimensions(l1b_path_file, n_across_track, &
           n_along_track, along_track_offset, day_night, loc_limit, &
           n_along_track2, along_track_offset2, verbose)

   end if ! end of sensor selection

   if (verbose) then
      write(*,*) 'WE ARE PROCESSING ',trim(platform),' FOR ORBIT',year,month, &
                                                                  day,hour,minute
      write(*,*) 'File dimensions determined:'
      write(*,*) 'along_track_offset:  ', along_track_offset
      write(*,*) 'n_along_track:       ', n_along_track
      if (trim(adjustl(sensor)) .eq. 'AATSR' .and. day_night .eq. 2) then
         write(*,*) 'along_track_offset2: ', along_track_offset2
         write(*,*) 'n_along_track2:      ', n_along_track2
      end if
   end if

   ! determine processing chunks and their dimensions
   if (verbose) write(*,*) 'Determine processing chunks and their dimensions'
   if (startx.ge.1 .and. endx.ge.1 .and. starty.ge.1 .and. endy.ge.1) then
      ! use specified values
      imager_geolocation%startx=startx
      imager_geolocation%endx=endx

      n_chunks = 1

      allocate(chunk_starts(n_chunks))
      allocate(chunk_ends(n_chunks))

      chunk_starts(1) = starty
      chunk_ends(1)   = endy
   else
      imager_geolocation%startx=1
      imager_geolocation%endx=n_across_track

      if (chunkproc) then
         chunksize = 4096
      else
         chunksize = n_along_track + n_along_track2
      end if

      n_segments = 1
      segment_starts(1) = along_track_offset + 1
      segment_ends(1)   = along_track_offset + n_along_track

      if (trim(adjustl(sensor)) .eq. 'AATSR' .and. day_night .eq. 2) then
         n_segments = n_segments + 1
         segment_starts(n_segments) = along_track_offset2 + 1
         segment_ends(n_segments)   = along_track_offset2 + n_along_track2
      end if

      n_chunks = calc_n_chunks(n_segments, segment_starts, segment_ends, &
                               chunksize)

      allocate(chunk_starts(n_chunks))
      allocate(chunk_ends(n_chunks))

      call chunkify(n_segments, segment_starts, segment_ends, chunksize, &
                    n_chunks, chunk_starts, chunk_ends)

   end if ! end of startx and starty selection

   if (verbose) then
      write(*,*) 'The number of chunks to be processed: ', n_chunks
      write(*,*) 'The chunks to be processed are (i_chunk, chunk_start, chunk_end):'
      do i_chunk = 1, n_chunks
         write(*,*) i_chunk, chunk_starts(i_chunk), chunk_ends(i_chunk)
      end do
   end if

   imager_geolocation%nx=imager_geolocation%endx-imager_geolocation%startx+1

   if (verbose) write(*,*) 'Begin looping over chunks'
   do i_chunk=1,n_chunks
      if (verbose) write(*,*) 'i_chunk: ', i_chunk

      imager_geolocation%starty = chunk_starts(i_chunk)
      imager_geolocation%endy   = chunk_ends(i_chunk)
      imager_geolocation%ny     = chunk_ends(i_chunk) - chunk_starts(i_chunk) + 1

      if (verbose) then
         write(*,*) 'startx: ',imager_geolocation%startx,', endx: ', &
              imager_geolocation%endx
         write(*,*) 'starty: ',imager_geolocation%starty,', endy: ', &
              imager_geolocation%endy
      end if

      ! allocate the structures for the imager: geolocation, angles and
      ! measurements, and surface data
      if (verbose) write(*,*) 'Allocate imager and surface structures'
      call allocate_imager_structures(imager_geolocation,imager_angles, &
           imager_flags,imager_time,imager_measurements,channel_info)
      call allocate_surface_structures(surface,imager_geolocation,channel_info, &
           include_full_brdf)

      ! read imager data:
      if (verbose) write(*,*) 'Read imager data'
      call read_imager(sensor,platform,l1b_path_file,geo_path_file, &
           aatsr_calib_path_file,imager_geolocation,imager_angles,imager_flags, &
           imager_time,imager_measurements,channel_info,n_along_track,verbose)

      ! carry out any preparatory steps: identify required ECMWF and MODIS L3
      ! information,set paths and filenames to those required auxiliary /
      ! ancillary input...
      if (verbose) write(*,*) 'Carry out any preparatory steps'
      call preparation(lwrtm_file,swrtm_file,prtm_file,config_file,msi_file, &
           cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file,sensor, &
           platform,cyear,cmonth,cday,chour,cminute,ecmwf_path,ecmwf_path2, &
           ecmwf_path3,ecmwf_path_file,ecmwf_path_file2,ecmwf_path_file3, &
           script_input,ecmwf_flag,imager_geolocation,i_chunk,assume_full_paths, &
           verbose)

      ! read ECMWF fields and grid information
      if (verbose) then
         write(*,*) 'Start reading ecmwf era interim grib file'
         write(*,*) 'ecmwf_flag: ', ecmwf_flag
         write(*,*) 'ecmwf_path_file: ',trim(ecmwf_path_file)
         if (ecmwf_flag.gt.0) then
            write(*,*) 'ecmwf_path_file2: ',trim(ecmwf_path_file2)
            write(*,*) 'ecmwf_path_file3: ',trim(ecmwf_path_file3)
         end if
      end if

      ! read surface wind fields and ECMWF dimensions
      select case (ecmwf_flag)
      case(0)
         call read_ecmwf_wind_grib(ecmwf_path_file,ecmwf)
         if (verbose) write(*,*)'ecmwf_dims grib: ',ecmwf%xdim,ecmwf%ydim
      case(1)
         call read_ecmwf_wind_nc(ecmwf_path_file,ecmwf_path_file2, &
              ecmwf_path_file3,ecmwf)
         if (verbose) write(*,*)'ecmwf_dims ncdf: ',ecmwf%xdim,ecmwf%ydim
      case(2)
         call read_ecmwf_wind_badc(ecmwf_path_file,ecmwf_path_file2, &
              ecmwf_path_file3,ecmwf)
         if (verbose) write(*,*)'ecmwf_dims badc: ',ecmwf%xdim,ecmwf%ydim
      end select
      if (verbose) then
         write(*,*) 'U10) Min: ',minval(ecmwf%u10),', Max: ',maxval(ecmwf%u10)
         write(*,*),'V10) Min: ',minval(ecmwf%v10),', Max: ',maxval(ecmwf%v10)
      end if
      call rearrange_ecmwf(ecmwf)

      ! define preprocessing grid from user grid spacing and satellite limits
      if (verbose) write(*,*) 'Define preprocessing grid'
      preproc_dims%kdim = ecmwf%kdim + 1
      call define_preprop_grid(imager_geolocation,preproc_dims,verbose)

      ! allocate preprocessing structures
      if (verbose) write(*,*) 'Allocate preprocessing structures'
      call allocate_preproc_structures(imager_angles,preproc_dims, &
           preproc_geoloc,preproc_geo,preproc_prtm,preproc_lwrtm, &
           preproc_swrtm,preproc_surf,channel_info)

      ! now read the actual data and interpolate it to the preprocessing grid
      if (verbose) write(*,*) 'Build preprocessing grid'
      call build_preproc_fields(preproc_dims,preproc_geoloc,preproc_geo, &
           imager_geolocation,imager_angles)

      ! read ecmwf era interim fil
      if (verbose) write(*,*) 'Read ecmwf era interim file'
      select case (ecmwf_flag)
      case(0)
         call read_ecmwf_grib(ecmwf_path_file,preproc_dims, &
              preproc_geoloc,preproc_prtm,verbose)
      case(1)
         if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
         call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims, &
              preproc_geoloc,preproc_prtm,verbose)

         if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file2)
         call read_ecmwf_nc(ecmwf_path_file2,ecmwf,preproc_dims, &
              preproc_geoloc,preproc_prtm,verbose)

         if (verbose) write(*,*) 'Reading ecmwf path:  ',trim(ecmwf_path_file3)
         call read_ecmwf_nc(ecmwf_path_file3,ecmwf,preproc_dims, &
              preproc_geoloc,preproc_prtm,verbose)
      case(2)
         if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file)
         call read_ecmwf_nc(ecmwf_path_file,ecmwf,preproc_dims, &
              preproc_geoloc,preproc_prtm,verbose)

         if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file2)
         call read_ecmwf_grib(ecmwf_path_file2,preproc_dims, &
              preproc_geoloc,preproc_prtm,verbose)

         if (verbose) write(*,*) 'Reading ecmwf path: ',trim(ecmwf_path_file3)
         call read_ecmwf_grib(ecmwf_path_file3,preproc_dims, &
              preproc_geoloc,preproc_prtm,verbose)
      end select
      ! compute geopotential vertical coordinate from pressure coordinate
      call compute_geopot_coordinate(preproc_prtm, preproc_dims, ecmwf)

      ! create output netcdf files.
      if (verbose) write(*,*) 'Create output netcdf files'
      if (verbose) write(*,*) 'output_path: ',trim(output_path)
      call netcdf_output_open(output_path,lwrtm_file, &
           swrtm_file,prtm_file,config_file,msi_file,cf_file,lsf_file, &
           geo_file,loc_file,alb_file,scan_file,platform,sensor,script_input, &
           cyear,cmonth,cday,chour,cminute,preproc_dims,imager_angles, &
           imager_geolocation,netcdf_info,channel_info,use_chunking, &
           include_full_brdf)

      ! perform RTTOV calculations
      if (verbose) write(*,*) 'Perform RTTOV calculations'
      call rttov_driver(rttov_coef_path,rttov_emiss_path,sensor,platform, &
           preproc_dims,preproc_geoloc,preproc_geo,preproc_prtm,preproc_lwrtm, &
           preproc_swrtm,imager_angles,netcdf_info,channel_info,month,verbose)

      ! select correct emissivity file and calculate the emissivity over land
      if (verbose) write(*,*) 'Get surface emissivity'
      call get_surface_emissivity(cyear, cdoy, cimss_emiss_path, imager_flags, &
           imager_geolocation, channel_info, preproc_dims, preproc_geoloc, &
           assume_full_paths, verbose, surface, preproc_surf)

      ! select correct reflectance files and calculate surface reflectance
      ! over land and ocean
      if (verbose) write(*,*) 'Get surface reflectance'
      call get_surface_reflectance(cyear, cdoy, modis_albedo_path, imager_flags, &
           imager_geolocation, imager_angles, channel_info, ecmwf, assume_full_paths, &
           include_full_brdf, verbose, surface)

      ! Use the Near-real-time Ice and Snow Extent (NISE) data from the National
      ! Snow and Ice Data Center to detect ice and snow pixels, and correct the
      ! surface albedo.
      if (verbose) write(*,*) 'Correct for ice and snow'
      call correct_for_ice_snow(nise_ice_snow_path, imager_geolocation, &
           preproc_dims, surface, cyear, cmonth, cday, channel_info, &
           assume_full_paths, verbose)

      if (verbose) write(*,*) 'Write netcdf output files'
      call netcdf_output_write_swath(imager_flags,imager_angles, &
           imager_geolocation,imager_measurements,imager_time,netcdf_info, &
           channel_info,surface,include_full_brdf)

      ! close output netcdf files
      if (verbose) write(*,*)'Close netcdf output files'
      call netcdf_output_close(netcdf_info)

      ! deallocate the array parts of the structures
      if (verbose) write(*,*) 'Deallocate chunk specific structures'
      call deallocate_ecmwf_structures(ecmwf)
      call deallocate_preproc_structures(preproc_dims, preproc_geoloc, &
           preproc_geo,preproc_prtm, preproc_lwrtm, preproc_swrtm, &
           preproc_surf)
      call deallocate_imager_structures(imager_geolocation, imager_angles, &
           imager_flags, imager_time, imager_measurements)
      call deallocate_surface_structures(surface,include_full_brdf)

   end do ! end looping over chunks

   if (verbose) write(*,*)'Deallocate remaining memory'
   deallocate(chunk_starts)
   deallocate(chunk_ends)

   ! deallocate the array parts of the structures
   call deallocate_channel_info(channel_info)

#ifdef WRAPPER
end subroutine preprocessing
#else
end program preprocessing
#endif


function parse_logical(string) result(value)

   use preproc_constants

   implicit none

   logical :: value

   character(len = cmd_arg_length), intent(in) :: string

   if (trim(adjustl(string)) .eq. '1' .or.&
       trim(adjustl(string)) .eq. 't' .or. &
       trim(adjustl(string)) .eq. 'true' .or. &
       trim(adjustl(string)) .eq. 'T' .or. &
       trim(adjustl(string)) .eq. 'True') then
        value = .true.
   else if &
      (trim(adjustl(string)) .eq. '0' .or. &
       trim(adjustl(string)) .eq. 'f' .or. &
       trim(adjustl(string)) .eq. 'false' .or. &
       trim(adjustl(string)) .eq. 'F' .or. &
       trim(adjustl(string)) .eq. 'False') then
        value = .false.
   else
        write(*,*) 'ERROR: invalid logical argument'
        stop error_stop_code
   end if

end function parse_logical



function calc_n_chunks(n_segments, segment_starts, segment_ends, &
                       chunk_size) result (n_chunks)

   implicit none

   integer, intent(in)  :: n_segments
   integer, intent(in)  :: segment_starts(n_segments)
   integer, intent(in)  :: segment_ends(n_segments)
   integer, intent(in)  :: chunk_size
   integer              :: n_chunks

   integer :: i

   n_chunks = 0

   do i = 1, n_segments
      n_chunks = n_chunks + (segment_ends(i) - segment_starts(i)) / chunk_size + 1
   end do

end function calc_n_chunks



subroutine chunkify(n_segments, segment_starts, segment_ends, &
                    chunk_size, n_chunks, chunk_starts, chunk_ends)

   implicit none

   integer, intent(in)  :: n_segments
   integer, intent(in)  :: segment_starts(n_segments)
   integer, intent(in)  :: segment_ends(n_segments)
   integer, intent(in)  :: chunk_size
   integer, intent(out) :: n_chunks
   integer, intent(out) :: chunk_starts(*)
   integer, intent(out) :: chunk_ends(*)

   integer :: i

   n_chunks = 1

   do i = 1, n_segments
      chunk_starts(n_chunks) = segment_starts(i)

      do while (chunk_starts(n_chunks) + chunk_size .lt. segment_ends(i))
         chunk_ends(n_chunks) = chunk_starts(n_chunks) + chunk_size - 1
         n_chunks = n_chunks + 1
         chunk_starts(n_chunks) = chunk_starts(n_chunks - 1) + chunk_size
      end do

      chunk_ends(n_chunks) = segment_ends(i)

      n_chunks = n_chunks + 1
   end do

   n_chunks = n_chunks - 1

end subroutine chunkify
