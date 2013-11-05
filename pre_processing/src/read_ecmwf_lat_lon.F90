! Name: read_ecmwf_lat_lon.F90
!
!
! Purpose:
! Read ECMWF ERA Interim grib lat/lon grid.
! 
! Description and Algorithm details:
! 1) Open file, load message, and allocate temporary arrays.
! 2) Read latitude and longitude from file. Copy into structure.
! 3) Close file and deallocate arrays.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in  Full path to ECMWF NCDF file to read.
! ecmwf_dims struct in  Structure summarising dimensions of ECMWF files.
! ecmwf_2d   struct out Structure containing 2-D ECMWF fields.
!
! Local variables:
! Name Type Description
!
!
! History:
! 2012/02/22: MJ produces code which reads rlat/lon grid from ERA INTERIM
!                GRIB files
! 2013/10/22: AP Tidying.
!
! $Id$
!
! Bugs:
! none known
!

!---------------------------------------------------
!---------------------------------------------------
subroutine read_ecmwf_lat_lon(ecmwf_path,ecmwf_dims,ecmwf_2d)

  use grib_api
  use preproc_constants
  use ecmwf_structures

  implicit none

  character(len=pathlength) :: ecmwf_path

  character(len=errlength)  :: err_msg

  type(ecmwf_dims_s)        :: ecmwf_dims
  type(ecmwf_2d_s)          :: ecmwf_2d

  real(kind=sreal), allocatable, dimension(:) :: dummyvector_lat, &
       dummyvector_lon,dummyvector_value

  integer(kind=lint)        :: gribid,igrib,iret,parameter,status,ilevel

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


  allocate(dummyvector_value(ecmwf_dims%xdim_ec*ecmwf_dims%ydim_ec))
  dummyvector_value=real_fill_value
  allocate(dummyvector_lat(ecmwf_dims%xdim_ec*ecmwf_dims%ydim_ec))
  dummyvector_lat=real_fill_value
  allocate(dummyvector_lon(ecmwf_dims%xdim_ec*ecmwf_dims%ydim_ec))
  dummyvector_lon=real_fill_value

  ! This will just read the lat,lon grid. In horizontal direction data is stored
  ! on a per pixel basis, meaning it is not stored in an 2D array structure but
  ! rather as datapoint after datapoint, therefore only 1D arrays with the total
  ! number of pixels can be decoded (aka "read").

  !get the current level
  call grib_get(igrib,'level',ilevel,status)
  !everything OK?
  if(status .ne. 0 ) then
     call grib_get_error_string(status,err_msg)
     write(*,*) 'GRIB API ERROR GETTING LEVEL:',  trim(err_msg)
  endif

  !read the parameter number of the variable which is just processed by the loop
  call grib_get(igrib,'parameter',parameter,status)
  !everything OK?
  if(status .ne. 0 ) then
     call grib_get_error_string(status,err_msg)
     write(*,*) 'GRIB API ERROR GETTING PARAMETER:',  trim(err_msg)
  endif

  !read the data
  call grib_get_data(igrib,dummyvector_lat,dummyvector_lon,dummyvector_value, &
       status)
  !everything OK?
  if(status .ne. 0 ) then
     call grib_get_error_string(status,err_msg)
     write(*,*) 'GRIB API ERROR GETTING DATA:',  trim(err_msg)
  endif
  

  !work on lat/lon
  if(ilevel .eq. 1) then
     !Latitude is counted from northpole to southpole 90 -> -90
     ecmwf_2d%latitude=reshape(dummyvector_lat, &
          (/ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec/))
     !Longitude is eastward from 0 to 358.875
     ecmwf_2d%longitude=reshape(dummyvector_lon, &
          (/ecmwf_dims%xdim_ec,ecmwf_dims%ydim_ec/))
  endif

    
     
  !release the "message" (however, igrib never changed?)
  call grib_release(igrib,status)
  !everything OK?
  if(status .ne. 0 ) then
     call grib_get_error_string(status,err_msg)
     write(*,*) 'GRIB API ERROR RELEASING GRIB_ID:',  trim(err_msg)
  endif

  deallocate(dummyvector_value)
  deallocate(dummyvector_lat)
  deallocate(dummyvector_lon)

  !close the grib file
  call grib_close_file(gribid,status)
  !everything OK?
  if(status .ne. 0 ) then
     call grib_get_error_string(status,err_msg)
     write(*,*) 'GRIB API ERROR RELEASING FILE_ID:',  trim(err_msg)
  endif
  

end subroutine read_ecmwf_lat_lon
