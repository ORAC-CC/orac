! Name: preparation.f90
!
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
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! lwrtm_file     string out Full path to output file LW RTM.
! swrtm_file     string out Full path to output file SW RTM.
! prtm_file      string out Full path to output file pressure RTM.
! msi_file       string out Full path to output file multispectral imagery.
! cf_file        string out Full path to output file cloud flag.
! lsf_file       string out Full path to output file land/sea flag.
! geo_file       string out Full path to output file geolocation.
! loc_file       string out Full path to output file location.
! alb_file       string out Full path to output file albedo.
! scan_file      string out Full path to output file scan position/
! sensor         string in  Name of sensor.
! platform       string in  Name of satellite platform.
! hour           stint  in  Hour of day (0-59)
! cyear          string in  Year, as a 4 character string
! cmonth         string in  Month of year, as a 2 character string
! cday           string in  Day of month, as a 2 character string
! chour          string in  Hour of day, as a 2 character string
! ecmwf_path     string in  If badc, folder in which to find GGAM files.
!                           Otherwise, folder in which to find GRB files.
! ecmwf_path2    string in  If badc, folder in which to find GGAS files.
! ecmwf_path3    string in  If badc, folder in which to find GPAM files.
! ecmwf_pathout  string out If badc, full path to appropriate GGAM file.
!                           Otherwise, full path to appropriate GRB file.
! ecmwf_path2out string out If badc, full path to appropriate GGAS file.
! ecmwf_path3out string out If badc, full path to appropriate GPAM file.
! script_input   struct in  Summary of file header information.
! badc           logic  in  1: Use BADC files as formatted by the BADC in NCDF
!                           format. Otherwise: Assume ERA_Interim GRB format.
! imager_geolocation        struct both Summary of pixel positions
! i_chunk        stint  in  The number of the current chunk (for AATSR).
! verbose        logic  in  T: print status information; F: don't
!
! History:
! 2011/12/12: MJ produces draft code which sets up output file names
! 2012/01/16: MJ includes subroutine to determine ERA interim file.
! 2012/02/14: MJ implements filenames and attributes for netcdf output.
! 2012/07/29: CP removed old comments
! 2012/08/06: CP added in badc flag
! 2012/12/06: CP added in option to break aatsr orbit into chunks for faster
!                processing added imager_structure to input and tidied up the file
! 2012/12/06: CP changed how ecmwf paths are defined because of looping chunks
! 2012/12/14: CP changed how file is named if the orbit is broken into
!                granules then the file name is given a latitude range
! 2012/03/05: CP small change to work for gfortran
! 2013/09/02: AP Removed startyi, endye.
! 2013/10/21: AP Removed redundant arguments. Tidying.
! 2014/02/03: AP made badc a logical variable
! 2014/04/21: GM Added logical option assume_full_path.
!
! $Id$
!
! Bugs:
! none known
!

subroutine preparation(lwrtm_file,swrtm_file,prtm_file,config_file,msi_file,&
   cf_file,lsf_file,geo_file,loc_file,alb_file,scan_file,sensor,platform,&
   hour,cyear,chour,cminute,cmonth,cday,assume_full_path,ecmwf_path,&
   ecmwf_path2,ecmwf_path3,ecmwf_pathout,ecmwf_path2out,ecmwf_path3out,&
   script_input,badc,imager_geolocation,i_chunk,verbose)

   use preproc_constants
   use imager_structures
   use attribute_structures

   implicit none

   character(len=filelength),     intent(out) :: lwrtm_file,swrtm_file,prtm_file, &
                                                 config_file,msi_file,cf_file,&
                                                 lsf_file,geo_file,loc_file,&
                                                 alb_file,scan_file
   character(len=sensorlength),   intent(in)  :: sensor
   character(len=platformlength), intent(in)  :: platform
   integer(kind=stint),           intent(in)  :: hour
   character(len=datelength),     intent(in)  :: cyear,chour,cminute,cmonth,cday
   logical,                       intent(in)  :: assume_full_path
   character(len=pathlength),     intent(in)  :: ecmwf_path,ecmwf_path2,&
                                                 ecmwf_path3
   character(len=pathlength),     intent(out) :: ecmwf_pathout,ecmwf_path2out,&
                                                 ecmwf_path3out
   type(script_arguments_s),      intent(in)  :: script_input
   logical,                       intent(in)  :: badc
   type(imager_geolocation_s),    intent(in)  :: imager_geolocation
   integer(kind=stint),           intent(in)  :: i_chunk
   logical,                       intent(in)  :: verbose

   character(len=filelength) :: range_name
   character(len=filelength) :: file_base
   real                      :: startr,endr
   character(len=30)         :: startc,endc,chunkc

   ! deal with ATSR chunking in filename
   if (sensor .eq. 'AATSR') then
      startr=imager_geolocation%latitude(imager_geolocation%startx,1)
      endr=imager_geolocation%latitude(imager_geolocation%endx, &
           imager_geolocation%ny)

      !convert latitudes into strings
      write(startc,'( g12.3 )') startr
      write(endc,  '( g12.3 )') endr   
      write(chunkc,'( g12.3 )') i_chunk


      range_name=trim(adjustl(chunkc))//'-'//trim(adjustl(startc))//'-'// &
           trim(adjustl(endc))//'_'
   else 
      range_name=''
   endif
   if (verbose) write(*,*) 'range_name: ', trim(range_name)


   !put basic filename together
   file_base=trim(adjustl(script_input%project))//'_'// &
        & trim(adjustl(script_input%cinst))//'_'//trim(adjustl(sensor)) &
        & //'_'// trim(adjustl(range_name))// &
        & trim(adjustl(script_input%l2cproc))//'V'// &
        & trim(adjustl(script_input%l2cprocver))

   !MJ this need to come from outside: script_input%exec_time=''
   file_base=trim(adjustl(file_base))//'_'//trim(adjustl(platform))// &
        & '_'//trim(adjustl(script_input%exec_time))
   file_base=trim(adjustl(file_base))//'_'//trim(adjustl(cyear))// &
        & trim(adjustl(cmonth))//trim(adjustl(cday))
   file_base=trim(adjustl(file_base))//trim(adjustl(chour))// &
        & trim(adjustl(cminute))//'_'//trim(adjustl(script_input%file_version))
   if (verbose) write(*,*) 'file_base: ', trim(file_base)
   
   !determine ecmwf path/filename
   call set_ecmwf(hour,cyear,cmonth,cday,chour,assume_full_path,ecmwf_path,&
                  ecmwf_path2,ecmwf_path3,ecmwf_pathout,ecmwf_path2out,&
                  ecmwf_path3out,badc,verbose)

   !get preproc filenames
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
   scan_file=trim(adjustl(file_base))//'.uv.nc'

end subroutine preparation
