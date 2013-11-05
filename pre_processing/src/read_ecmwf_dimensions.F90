! Name: read_ecmwf_dimensions.F90
!
!
! Purpose:
! Extract dimensions of arrays of variables stored in ERA Interim GRIB files
! for further use in dynamic array allocation.
! 
! Description and Algorithm details:
! 1) Open file.
! 2) Loop through the file, fetching many things (for no apparent reason).
! 3) Copy the last set of values into a structure.
! 4) Close file.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in   Full path to ECMWF NCDF file to read.
! ecmwf_dims struct both Structure summarising dimensions of ECMWF files.
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/01/13: MJ writes original code version.
! 2012/01/16: MJ adds error handling.
! 2013/10/22: AP Tidying.
!
! $Id$
!
! Bugs:
! none known
!

!---------------------------------------------------
!---------------------------------------------------
subroutine read_ecmwf_dimensions_grib(ecmwf_path, ecmwf_dims)

  use grib_api
  use preproc_constants
  use ecmwf_structures

  implicit none

  integer(kind=lint) :: wo,status,gribid,igrib

  character(len=pathlength) :: ecmwf_path

  character(len=errlength) :: err_msg

  integer(kind=lint) :: number_of_values,number_of_points,indik,grid_type,ni,nj
  integer(kind=lint) :: ilevel,nlevels,levels,iret,parameter

  type(ecmwf_dims_s) :: ecmwf_dims

  integer(kind=lint) :: PVPresent,nb_pv


  wo=1

  !open the grib file for reading
  call grib_open_file(gribid,trim(ecmwf_path),'r',status)
  !everything OK?
  if(status .ne. 0 ) then
     call grib_get_error_string(status,err_msg)
     write(*,*) 'GRIB API ERROR OPENING FILE:', trim(ecmwf_path),'msg:', &
          trim(err_msg)
  endif

  !load the grib message?
  call grib_new_from_file(gribid,igrib,iret)
  !everything OK?
  if(iret .ne. 0 ) then
     call grib_get_error_string(status,err_msg)
     write(*,*) 'GRIB API ERROR GETTING GRIB_ID:',  trim(err_msg)
  endif

  call grib_get(igrib,'PVPresent',PVPresent)
  !get the number of entries:
  !REMARK: This should return nb_pv=122=61(= # model levels+1)*2
  !model levels are actually layer centres and there the archived fields are 
  !defined. the pressure and hence A,B are defined at the interfaces.
  !The first 61 entries are the As, the remianing 61 entries are the Bs.
  !Now get the damned figure:
  if(PVPresent .eq. 1) then
     call grib_get_size(igrib,'pv',nb_pv)
  else
     write(*,*) 'FATAL: No vertical coordinate in GRIB header'
     stop
  endif


  nlevels=0

  !this loops over the stored data. data is stored per level and variable, so
  !this loops over levels and variables simultaneously. the variables loop 
  !faster than the levels.in horizontal direction data is stored on a per pixel
  !basis, meaning it is not stored in an 2D array structure but rather as
  !datapoint after datapoint, therefore only 1D arrays with the total number of
  !pixels can be decoded (aka "read").
  do while(iret .ne. grib_end_of_file)
     !get the number of pixels 
     !longitude = 320
     !latitude = 161 ;
     !number_of_values=longitude*latitude (ni*nj, see below)
     call grib_get_size(igrib,'values',number_of_values,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING #values:', trim(ecmwf_path),'msg:', &
             trim(err_msg)
     endif

     !number_of_points=number_of_values???
     call grib_get(igrib,'numberOfPoints',number_of_points,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING #ofpoints:',trim(ecmwf_path),'msg:',&
             trim(err_msg)
     endif

     
     !vertical coordinate type?
     call grib_get(igrib,'indicatorOfTypeOfLevel',indik,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING VERTICAL COORDINATE TYPE:', &
             trim(ecmwf_path),'msg:', trim(err_msg)
     endif
     

     !query the type of grid: "0": latitude/longitude grid, meaning the data is
     !just stored one point after the other
     call grib_get(igrib,'dataRepresentationType',grid_type,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING GRID TYPE:',trim(ecmwf_path),'msg:',&
             trim(err_msg)
     endif

     
     !number of points in longitudinal (x) direction
     call grib_get(igrib,'Ni',ni,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING NI:', trim(ecmwf_path),'msg:', &
             trim(err_msg)
     endif

     !number of points in latitudinal (y) direction
     call grib_get(igrib,'Nj',nj,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING NJ:', trim(ecmwf_path),'msg:', &
             trim(err_msg)
     endif

     call grib_get(igrib,'numberOfVerticalCoordinateValues',levels,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING CURRENT LEVEL INDEX:', &
             trim(ecmwf_path),'msg:', trim(err_msg)
     endif

     !current level
     call grib_get(igrib,'level',ilevel)
     !get the total number of vertical model levels
     if(ilevel .gt. nlevels) nlevels=ilevel
     !read the parameter number of the variable which is just processed by the 
     !loop, levels are arranged from TOA (first level) down to BOA (last level).
     call grib_get(igrib,'parameter',parameter,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR GETTING PARAMETER:',trim(ecmwf_path),'msg:',&
             trim(err_msg)
     endif

     !release the "message" (however, igrib never changed?)
     call grib_release(igrib,status)
     if(status .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR RELEASING GRIB_ID:',  trim(err_msg)
     endif

     !load the (next) message?
     call grib_new_from_file(gribid,igrib,iret)
     if(iret .ne. 0 ) then
        call grib_get_error_string(status,err_msg)
        write(*,*) 'GRIB API ERROR (RE)GETTING GRIB_ID:',  trim(err_msg)
     endif
  enddo

  !close the grib file
  call grib_close_file(gribid)

  !assign the local variables to the ecmwf_dims structure
  ecmwf_dims%xdim_ec=ni
  ecmwf_dims%ydim_ec=nj
  if (nb_pv/2-1 .eq. nlevels) then
     ecmwf_dims%kdim_ec=nlevels
  else
     write(*,*) 'Vertical level information corrupted:',nlevels,nb_pv/2-1
  endif


end subroutine read_ecmwf_dimensions_grib
