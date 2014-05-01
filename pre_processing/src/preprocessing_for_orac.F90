!-------------------------------------------------------------------------------
! Name: preprocessing_for_orac.F90
!
! Purpose:
!   Read in data from the variety of files that contain all the information
!   necessary to run the ORAC algorithm, remove any uneccessary details (such as
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
! Name            Type In/Out/Both      Description
! ------------------------------------------------------------------------------
! sensor           string in Name of instrument (only AATSR, MODIS or AVHRR are
!                            valid)
! path_to_l1b_file string in Full path to level 1B data
! path_to_geo_file string in Full path to geolocation data (identical to above
!                            for AATSR)
! ecmwf_path       string in Folder containing ECMWF files
! coef_path        string in Folder containing RTTOV coefficient files
! emiss_path       string in Folder containing RTTOV emissivity files
! ice_path         string in Folder containing NISE ice cover files
! albedo_path      string in Folder containing MODIS MCD43C3 albedo files
! emiss2_path      string in Folder containing MODIS monthly average emissivity
! grid_flag        sint   in 1:ECMWF grid, 2:L3 grid, 3: own definition
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
! aatsr_calib_file string in Full path to the AATSR calibration file
! badc             logic  in T: use of BADC NetCDF ECMWF files; F: GRIB ECMWF
!                            files
! ecmwf_path2      string in Folder containing ECMWF files (?)
! ecmwf_path3      string in Folder containing ECMWF files (?)
! chunkproc        logic  in T: split AATSR orbit in 4096 row chunks, F: don't
! day_night        int    in 2: process only night data; 1: day
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
! 2012/03/26: MJ fixes bug in nearneighbor assigment.
! 2012/03/27: MJ changes all file suffixes from *.f90 to *.F90 for ease of use
!                of the C preprocessor.
! 2012/05/15: MJ includes code to read AVHRR land/sea mask.
! 2012/05/15: CP includes path to albedo/snow files/emissivity
! 2012/07/15: CP add in AATSR reading including calibration file
! 2012/05/15: MJ removes slight bug in passing of parameters from surrounding
!                shell script.
! 2012/07/23: MJ adds capability to read from command line OR driver file.
! 2012/07/29: CP tidied up code and merged in MJ changes and  skips over
!                externalemissivity read, improved readability, removed albedo
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
! 2012/12/16: CP added an extra ecmwf path so ecmwf can be read form seprate
!                directories
! 2013/02/26: CP changed how day_night aatsr flag was read in
! 2013/02/25: GT Added preproc_geoloc to the arguments for
!                get_surface_emissivity, un-commented the call to
!                get_surface_emissivity (which now works).
! 2013/03/06: GT Tidied up formating of file header.
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
! 2014/02/02: GM Added chunking on/off option.
! 2014/02/03: AP Ensured all arguments that are logical flags are treated
!                identically
! 2014/02/05: MJ corrected datatype of chunkproc from character to logical
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
!
! $Id$
!
! Bugs:
! See http://proj.badc.rl.ac.uk/orac/report/1
!
!-------------------------------------------------------------------------------

program preprocessing

   use attribute_structures
   use channel_structures
   use ecmwf_structures
   use imager_structures
   use hdf5
   use netcdf
   use netcdf_structures
   use orac_ecmwf
   use preproc_constants
   use preproc_structures
   use surface_structures
   use preproc_structures

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   character(len=pathlength)       :: driver_path_and_file
   character(len=pathlength)       :: path_to_l1b_file
   character(len=pathlength)       :: path_to_geo_file
   character(len=pathlength)       :: ecmwf_path,ecmwf_pathout
   character(len=pathlength)       :: coef_path
   character(len=pathlength)       :: emiss_path
   character(len=pathlength)       :: ice_path
   character(len=pathlength)       :: albedo_path
   character(len=pathlength)       :: emiss2_path
   character(len=pathlength)       :: output_pathin,output_pathout
   character(len=pathlength)       :: aatsr_calib_file
   character(len=pathlength)       :: ecmwf_path2,ecmwf_path2out
   character(len=pathlength)       :: ecmwf_path3,ecmwf_path3out
   character(len=flaglength)       :: cgrid_flag
   character(len=attribute_length) :: cdellon,cdellat
   character(len=pixellength)      :: cstartx,cendx,cstarty,cendy
   character(len=pixellength)      :: cbadc
   character(len=pixellength)      :: cchunkproc
   character(len=pixellength)      :: cday_night
   character(len=pixellength)      :: cverbose
   character(len=pixellength)      :: cuse_chunking
   character(len=pixellength)      :: cassume_full_paths

   type(script_arguments_s)        :: script_input

   integer(kind=sint)              :: grid_flag
   logical                         :: badc
   logical                         :: chunkproc
   integer(kind=stint)             :: day_night
   logical                         :: verbose
   logical                         :: use_chunking
   logical                         :: assume_full_paths

   integer                         :: ierr
   logical                         :: check
   integer                         :: nargs

   integer(kind=lint)              :: startx,endx,starty,endy
   integer(kind=lint)              :: n_across_track,n_along_track
   integer(kind=lint)              :: along_track_offset

   integer, parameter              :: chunksize=4096
   integer(kind=stint)             :: nc
   integer                         :: nchunks1,leftover_chunk1
   integer                         :: nchunks2,leftover_chunk2
   integer                         :: nchunks_total

   real(kind=sreal), dimension(4)  :: loc_limit

   ! The following are for lengths and offsets for the second section of
   ! nighttime data in an (A)ATSR orbit file:
   integer(kind=lint)              :: n_along_track2, along_track_offset2

   character(len=sensorlength)     :: sensor
   character(len=platformlength)   :: platform

   integer(kind=stint)             :: doy,year,month,day,hour,minute,startyi

   character(len=datelength)       :: cyear,cmonth,cday,chour,cminute

   character(len=filelength)       :: lwrtm_file,swrtm_file,prtm_file
   character(len=filelength)       :: msi_file,cf_file,lsf_file,config_file
   character(len=filelength)       :: geo_file,loc_file,alb_file,scan_file

   type(channel_info_s)            :: channel_info

   type(imager_angles_s)           :: imager_angles
   type(imager_flags_s)            :: imager_flags
   type(imager_geolocation_s)      :: imager_geolocation
   type(imager_measurements_s)     :: imager_measurements
   type(imager_time_s)             :: imager_time

   type(ecmwf_dims_s)              :: ecmwf_dims
   type(ecmwf_2d_s)                :: ecmwf_2d
   type(ecmwf_3d_s)                :: ecmwf_3d

   type(surface_s)                 :: surface

   type(preproc_dims_s)            :: preproc_dims
   type(preproc_geo_s)             :: preproc_geo
   type(preproc_geoloc_s)          :: preproc_geoloc
   type(preproc_prtm_s)            :: preproc_prtm
   type(preproc_lwrtm_s)           :: preproc_lwrtm
   type(preproc_swrtm_s)           :: preproc_swrtm
   type(preproc_surf_s)            :: preproc_surf

   type(netcdf_info_s)             :: netcdf_info

   logical                         :: parse_logical

!  integer, dimension(8)           :: values

!  include "sigtrap.F90"

   ! get number of arguments
   nargs = COMMAND_ARGUMENT_COUNT()
   ! if more than one argument passed, all inputs on command line
   if(nargs .gt. 1) then
      CALL GET_COMMAND_ARGUMENT(1,sensor)
      CALL GET_COMMAND_ARGUMENT(2,path_to_l1b_file)
      CALL GET_COMMAND_ARGUMENT(3,path_to_geo_file)
      CALL GET_COMMAND_ARGUMENT(4,ecmwf_path)
      CALL GET_COMMAND_ARGUMENT(5,coef_path)
      CALL GET_COMMAND_ARGUMENT(6,emiss_path)
      CALL GET_COMMAND_ARGUMENT(7,ice_path)
      CALL GET_COMMAND_ARGUMENT(8,albedo_path)
      CALL GET_COMMAND_ARGUMENT(9,emiss2_path)
      CALL GET_COMMAND_ARGUMENT(10,cgrid_flag)
      CALL GET_COMMAND_ARGUMENT(11,cdellon)
      CALL GET_COMMAND_ARGUMENT(12,cdellat)
      CALL GET_COMMAND_ARGUMENT(13,output_pathin)
      CALL GET_COMMAND_ARGUMENT(14,cstartx)
      CALL GET_COMMAND_ARGUMENT(15,cendx)
      CALL GET_COMMAND_ARGUMENT(16,cstarty)
      CALL GET_COMMAND_ARGUMENT(17,cendy)
      CALL GET_COMMAND_ARGUMENT(18,script_input%cncver)
      CALL GET_COMMAND_ARGUMENT(19,script_input%ccon)
      CALL GET_COMMAND_ARGUMENT(20,script_input%cinst)
      CALL GET_COMMAND_ARGUMENT(21,script_input%l2cproc)
      CALL GET_COMMAND_ARGUMENT(22,script_input%l2cprocver)
      CALL GET_COMMAND_ARGUMENT(23,script_input%contact)
      CALL GET_COMMAND_ARGUMENT(24,script_input%website)
      CALL GET_COMMAND_ARGUMENT(25,script_input%file_version)
      CALL GET_COMMAND_ARGUMENT(26,script_input%reference)
      CALL GET_COMMAND_ARGUMENT(27,script_input%history)
      CALL GET_COMMAND_ARGUMENT(28,script_input%summary)
      CALL GET_COMMAND_ARGUMENT(29,script_input%keywords)
      CALL GET_COMMAND_ARGUMENT(30,script_input%comment)
      CALL GET_COMMAND_ARGUMENT(31,script_input%project)
      CALL GET_COMMAND_ARGUMENT(32,script_input%license)
      CALL GET_COMMAND_ARGUMENT(33,script_input%uuid_tag)
      CALL GET_COMMAND_ARGUMENT(34,script_input%exec_time)
      CALL GET_COMMAND_ARGUMENT(35,aatsr_calib_file)
      CALL GET_COMMAND_ARGUMENT(36,cbadc)
      CALL GET_COMMAND_ARGUMENT(37,ecmwf_path2)
      CALL GET_COMMAND_ARGUMENT(38,ecmwf_path3)
      CALL GET_COMMAND_ARGUMENT(39,cchunkproc)
      CALL GET_COMMAND_ARGUMENT(40,cday_night)
      CALL GET_COMMAND_ARGUMENT(41,cverbose)
      CALL GET_COMMAND_ARGUMENT(42,cuse_chunking)
      CALL GET_COMMAND_ARGUMENT(43,cassume_full_paths)
   else
      ! if just one argument => this is driver file which contains everything
      call get_command_argument(1,driver_path_and_file)
      open(11,file=trim(adjustl(driver_path_and_file)),status='old', &
           form='formatted')

      read(11,*) sensor
      read(11,*) path_to_l1b_file
      read(11,*) path_to_geo_file
      read(11,*) ecmwf_path
      read(11,*) coef_path
      read(11,*) emiss_path
      read(11,*) ice_path
      read(11,*) albedo_path
      read(11,*) emiss2_path
      read(11,*) cgrid_flag
      read(11,*) cdellon
      read(11,*) cdellat
      read(11,*) output_pathin
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
      read(11,*) aatsr_calib_file
      read(11,*) cbadc
      read(11,*) ecmwf_path2
      read(11,*) ecmwf_path3
      read(11,*) cchunkproc
      read(11,*) cday_night
      read(11,*) cverbose
      read(11,*) cuse_chunking
      read(11,*) cassume_full_paths
      close(11)
   endif ! nargs gt 1

   ! get the NetCDF version
   script_input%cncver=nf90_inq_libvers()

   ! get the execution time in UTC
!  call date_and_time(VALUES=values)
!  write (script_input%exec_time, '(i4,i2.2,i2.2,i2.2,i2.2,i2.2)'), &
!         values(1), values(2), values(3), values(5), values(6), values(7)
!  print *, script_input%exec_time
!  stop

   ! cast input strings into appropriate variables
   read(cgrid_flag, '(i1)') grid_flag
   read(cdellon, '(f10.5)') preproc_dims%dellon
   read(cdellat, '(f10.5)') preproc_dims%dellat
   read(cstartx(1:len_trim(cstartx)), '(I6)') startx
   read(cendx(1:len_trim(cendx)), '(I6)') endx
   read(cstarty(1:len_trim(cstarty)), '(I6)') starty
   read(cendy(1:len_trim(cendy)), '(I6)') endy
   read(cday_night(1:len_trim(cday_night)), '(I6)') day_night
   badc=parse_logical(cbadc)
   chunkproc=parse_logical(cchunkproc)
   verbose=parse_logical(cverbose)
   use_chunking=parse_logical(cuse_chunking)
   assume_full_paths=parse_logical(cassume_full_paths)

   ! initialise some counts, offset variables...
   nchunks1=0
   nchunks2=0
   nchunks_total=0
   along_track_offset=0
   along_track_offset2=0
   n_along_track2=0

   ! allocate array for channel information
   imager_angles%nviews=1
   channel_info%nchannels_total=6
   call allocate_channel_info(channel_info)

   ! determine platform, day, time, check if l1b and geo match
   inquire(file=path_to_l1b_file,exist=check)
   if (.not. check) stop 'L1B file does not exist.'
   inquire(file=path_to_geo_file,exist=check)
   if (.not. check) stop 'Geo file does not exist.'
   if (trim(adjustl(sensor)) .eq. 'MODIS') then
      call setup_modis(path_to_l1b_file,path_to_geo_file,platform,doy,year, &
           month,day,hour,minute, cyear,cmonth,cday,chour,cminute,channel_info)

      ! get dimensions of the modis granule
      call read_modis_dimensions(path_to_geo_file,n_across_track,n_along_track)

   elseif (trim(adjustl(sensor)) .eq. 'AVHRR') then
      call setup_avhrr(path_to_l1b_file,path_to_geo_file,platform,doy,year, &
           month,day,hour,minute, cyear,cmonth,cday,chour,cminute,channel_info)

      ! get dimensions of the avhrr orbit
      call read_avhrr_dimensions(path_to_geo_file,n_across_track,n_along_track)

   elseif (trim(adjustl(sensor)) .eq. 'AATSR') then
      call setup_aatsr(path_to_l1b_file,path_to_geo_file,platform,doy,year, &
           month,day,hour,minute, cyear,cmonth,cday,chour,cminute,channel_info)

      ! currently setup to do day only by default
      if (day_night .eq. 0) day_night=1
      loc_limit=(/ -90.0, -180.0, 90.0, 180.0 /)

      ! initialise the second length and offset variables

      ! Get array dimensions and along-track offset for the daylight side. If
      ! we're processing daylight data, we may want to chunk process after this.
      call read_aatsr_dimensions(path_to_l1b_file, n_across_track, &
           n_along_track, along_track_offset, day_night, loc_limit, &
           n_along_track2, along_track_offset2, verbose)

   endif ! end of sensor selection
   if (verbose) then
      write(*,*) 'File dimensions determined -'
      write(*,*) 'n_along_track:       ', n_along_track
      write(*,*) 'along_track_offset:  ', along_track_offset
      if (trim(adjustl(sensor)) .eq. 'AATSR' .and. day_night .eq. 2) then
         write(*,*) 'n_along_track2:      ', n_along_track2
         write(*,*) 'along_track_offset2: ', along_track_offset2
      endif
   endif
   write(*,*) 'WE ARE PROCESSING ',trim(platform),' FOR ORBIT',year,month,day, &
        hour,minute

   ! set processing dimensions
   if (startx.ge.1 .and. endx.ge.1 .and. starty.ge.1 .and. endy.ge.1) then
      ! use specified values
      imager_geolocation%startx=startx
      imager_geolocation%endx=endx

      imager_geolocation%starty=starty
      imager_geolocation%endy=endy
   else
      ! choose to process the whole orbit/granule or break it up
      imager_geolocation%startx=1
      imager_geolocation%endx=n_across_track

      if (trim(adjustl(sensor)) .eq. 'AATSR' .and. chunkproc) then
         ! How many chunks do we have?
         ! For a day-time orbit, there is only one section of orbit to process.
         nchunks1=floor(n_along_track/real(chunksize))
         leftover_chunk1=n_along_track - (nchunks1*chunksize)
         startyi=along_track_offset+1

         ! If we are running the night-side of an orbit, there will be a second
         ! group of chunks.
         if (n_along_track2 .gt. 0) then
            nchunks2=floor(n_along_track2/real(chunksize))
            leftover_chunk2=n_along_track2 - (nchunks2*chunksize)
            nchunks_total=nchunks1 + nchunks2 + 1
         else
            nchunks_total=nchunks1
         endif
      else
         ! process everything
         imager_geolocation%starty=along_track_offset+1
         imager_geolocation%endy=along_track_offset + n_along_track
      endif
   endif ! end of startx and starty selection
   imager_geolocation%nx=imager_geolocation%endx-imager_geolocation%startx+1

   ! begin looping over chunks
   do nc=1,nchunks_total+1
      if (nchunks_total .gt. 0) then
         if (verbose) write(*,*) 'BEGINNING PROCESSING OF CHUNK ',nc,' of ', &
              nchunks_total+1

         ! As each chunk doesn't need to know anything about the others, starty,
         ! endy, and ny are set for each chunk and passed to the routines.
         imager_geolocation%starty=startyi+(nc-1)*chunksize
         if (nc .eq. nchunks1+1) then
            ! last of first block of chunks
            imager_geolocation%endy=imager_geolocation%starty+leftover_chunk1-1
            imager_geolocation%ny=leftover_chunk1

            ! Now check to see if there is a second set of chunks to read
            ! further on in the orbit. If there is, we redefine startyi to
            ! correspond to the start of this second section.
            if (nchunks2 .gt. 0) startyi = along_track_offset2 - nc*chunksize+1
         elseif (nc .eq. nchunks_total+1) then
            ! last of second block of chunks
            imager_geolocation%endy=imager_geolocation%starty+leftover_chunk2-1
            imager_geolocation%ny=leftover_chunk2
         else
            ! normal chunk
            imager_geolocation%endy=imager_geolocation%starty+chunksize-1
            imager_geolocation%ny=chunksize
         endif
      else
         ! no chunks so starty, endy set above
         imager_geolocation%ny=imager_geolocation%endy- &
              imager_geolocation%starty+1
      endif ! end nchunks_total .gt. 0

      if (verbose) then
         write(*,*) 'startx: ',imager_geolocation%startx,', endx: ', &
              imager_geolocation%endx
         write(*,*) 'starty: ',imager_geolocation%starty,', endy: ', &
              imager_geolocation%endy
      endif

      ! allocate the structures for the imager like geolocation, angles and
      ! measurements and surface
      call allocate_imager_structures(imager_geolocation,imager_angles, &
           imager_flags,imager_time,imager_measurements,channel_info)
      call allocate_surface_structures(surface,imager_geolocation,channel_info)

      ! read imager information:
      ! multi spectral data, lat/lon, angles etc.
      call read_imager(sensor,platform,path_to_l1b_file,path_to_geo_file, &
           aatsr_calib_file,imager_geolocation,imager_angles,imager_flags, &
           imager_time,imager_measurements,channel_info,n_along_track,verbose)

      ! carry out any prepatory steps: identify required ECMWF and MODIS L3
      ! information,set paths and filenames to those required auxilliary /
      ! ancilliary input...
      call preparation(lwrtm_file,swrtm_file,prtm_file,config_file,msi_file, &
           cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file, sensor, &
           platform,hour,cyear,cmonth,cday,chour,cminute,assume_full_paths, &
           ecmwf_path,ecmwf_path2,ecmwf_path3,ecmwf_pathout,ecmwf_path2out, &
           ecmwf_path3out,script_input,badc,imager_geolocation,nc,verbose)

      ! read ECMWF fields and grid information
      if (verbose) then
         write(*,*) 'START READING ECMWF ERA INTERIM GRIB FILE:'
         write(*,*) 'badc: ', badc
         write(*,*) 'ecmwf_path:  ',trim(ecmwf_pathout)
         if (badc) then
            write(*,*) 'ecmwf_path2: ',trim(ecmwf_path2out)
            write(*,*) 'ecmwf_path3: ',trim(ecmwf_path3out)
         endif
      endif

      if (badc) then
         call read_ecmwf_dimensions_nc(ecmwf_path3out,ecmwf_dims)
         if (verbose) write(*,*)'ecmwf_dims nc: ',ecmwf_dims%xdim, &
              ecmwf_dims%ydim
      else
         call read_ecmwf_dimensions_grib(ecmwf_pathout,ecmwf_dims)
         if (verbose) write(*,*)'ecmwf_dims grib: ',ecmwf_dims%xdim, &
              ecmwf_dims%ydim
      endif

      ! allocate now the variable arrays in the structure
      if (verbose) write(*,*) 'allocate ecmwf structure'
      call allocate_ecmwf_structures(ecmwf_dims,ecmwf_3d,ecmwf_2d)

      ! read ERA Interim lat/lon grid
      if (verbose) write(*,*) 'read ECMWF lat/lon'
      if (badc) then
         call read_ecmwf_lat_lon_nc(ecmwf_path3out,ecmwf_dims,ecmwf_2d)
      else
         call read_ecmwf_lat_lon(ecmwf_pathout,ecmwf_dims,ecmwf_2d)
      endif

      ! define a preprocessing grid which can be ecmwf grid or user defined.
      if (verbose) write(*,*) 'define preprocessing grid'
      call define_preprop_grid(grid_flag,ecmwf_2d,ecmwf_dims,preproc_dims, &
           verbose)

      ! allocate preprocessing structures
      if (verbose) write(*,*) 'allocate preprocessing structures'
      call allocate_preproc_structures(imager_angles,preproc_geoloc, &
           preproc_geo,preproc_prtm,preproc_dims,preproc_lwrtm,preproc_swrtm, &
           preproc_surf,channel_info)

      ! set up now the preproc grid
      call make_preprop_grid(preproc_dims,preproc_geoloc)

      ! find the bounding indices of the imager data wrt the preprocessing grid
      call find_min_max_preproc(preproc_dims,imager_geolocation,verbose)

      ! now read the actual data and interpolate it to the preprocessing grid
      if (verbose) write(*,*) 'Build preprocessing grid'
      call build_preproc_fields(preproc_dims,preproc_geo,imager_geolocation, &
           imager_angles)
      if (verbose) write(*,*) 'finished preprocessing grid'

      ! read grib files
      if (verbose) write(*,*) 'START READING ECMWF ERA INTERIM FILE'
      if (badc) then
         if (verbose) write(*,*) 'Reading ecmwf path3: ',trim(ecmwf_path3out)
         call read_ecmwf_nc(ecmwf_path3out,ecmwf_dims,ecmwf_2d,preproc_dims, &
              preproc_geoloc,preproc_prtm,grid_flag)

         if (verbose) write(*,*)'reading ecmwf path2: ',trim(ecmwf_path2out)
         call read_ecmwf_nc(ecmwf_path2out,ecmwf_dims,ecmwf_2d,preproc_dims,&
              preproc_geoloc,preproc_prtm,grid_flag)

         if (verbose) write(*,*)'reading ecmwf path:  ',trim(ecmwf_pathout)
         call read_ecmwf_nc(ecmwf_pathout,ecmwf_dims,ecmwf_2d,preproc_dims, &
              preproc_geoloc,preproc_prtm,grid_flag)

         if (verbose) write(*,*) 'FINISHED READING ECMWF ERA INTERIM BADC FILE'
      else
         call read_ecmwf_grib(ecmwf_pathout,ecmwf_dims,ecmwf_3d,ecmwf_2d, &
              preproc_dims,preproc_geoloc,preproc_prtm)

         if (verbose) write(*,*) 'FINISHED READING ECMWF ERA INTERIM GRIB FILE'
      endif ! end badc

      ! compute geopotential vertical coorindate from pressure cooordinate
      call compute_geopot_coordinate(preproc_prtm, preproc_dims, ecmwf_dims)

      ! select correct emissivity file and calculate the emissivity over land
      ! calculates emissivity for rttov
      call get_surface_emissivity(cyear, doy, assume_full_paths, emiss2_path, &
           imager_flags, imager_geolocation, channel_info,preproc_dims, &
           preproc_geoloc, surface, preproc_surf)

      ! create/open output netcdf files.
      write(*,*)'create/open output netcdf files'
      write(*,*)'netcdf output_path: ',trim(output_pathout)
      call open_netcdf_output(imager_geolocation%nx,imager_geolocation%ny, &
           output_pathin,output_pathout,lwrtm_file,swrtm_file,prtm_file, &
           config_file,msi_file,cf_file,lsf_file,geo_file,loc_file,alb_file, &
           scan_file,platform,sensor,script_input,cyear,cmonth,cday,chour,cminute, &
           preproc_dims,imager_angles,imager_geolocation,netcdf_info,channel_info, &
           use_chunking)

      ! perform RTTOV calculations
      write(*,*) 'Start RT calculations'
      call calculate_rt(coef_path,emiss_path,sensor,platform, &
           preproc_dims,preproc_geoloc,preproc_geo, &
           preproc_prtm,preproc_lwrtm,preproc_swrtm,imager_angles, &
           netcdf_info,channel_info,month,ecmwf_dims,verbose)

      ! set the surface reflectance.
      call get_surface_reflectance_lam(cyear, doy, assume_full_paths, albedo_path, &
           imager_flags, imager_geolocation, imager_angles, imager_measurements, &
           channel_info, ecmwf_2d, surface)

      ! Use the Near-real-time Ice and Snow Extent (NISE) data from the National
      ! Snow and Ice Data Center to detect ice and snow pixels, and correct the
      ! surface albedo.
      write(*,*)'ice_path: ',trim(ice_path)
      call correct_for_ice_snow(assume_full_paths, ice_path, imager_geolocation, &
           preproc_dims, surface,cyear,cmonth,cday,channel_info)
      write(*,*)'finished correction for ice'

      write(*,*) 'before write netcdf'
      call write_swath_to_netcdf(imager_flags,imager_angles,imager_geolocation, &
           imager_measurements,imager_time,netcdf_info,channel_info,surface)
      write(*,*) 'after write'

      write(*,*)'finished correction for ice'

      ! close output netcdf files
      write(*,*)'start close netcdf'
      call close_netcdf_output(netcdf_info)
      write(*,*)'end close netcdf'

      ! deallocate the array parts of the structures
      write(*,*)'start deallocate'
      call deallocate_ecmwf_structures(ecmwf_dims,ecmwf_3d,ecmwf_2d)
      call deallocate_preproc_structures(preproc_geoloc,preproc_geo, &
           preproc_dims, preproc_prtm, preproc_lwrtm, preproc_swrtm, &
           preproc_surf)
      call deallocate_imager_structures(imager_geolocation,imager_angles, &
           imager_flags,imager_time,imager_measurements)
      call deallocate_surface_structures(surface)

   enddo ! end looping over chunks

   ! deallocate the array parts of the structures
   call deallocate_channel_info(channel_info)

end program preprocessing



function parse_logical(string) result(value)

   use preproc_constants

   implicit none

   logical :: value

   character(len = pixellength), intent(in) :: string

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
        stop 'Error parsing logical'
   endif

end function parse_logical
