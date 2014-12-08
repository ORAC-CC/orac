SUBROUTINE nc_create_file_pp(path, ncid, nx, ny, dims_var, wo,type)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_file.f90
  !
  ! Created on:          12/12/11
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when building libraries.
  !
  !   
  ! Modifications Log:    
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------
  
  USE netcdf

  IMPLICIT NONE
  
  ! Input
  INTEGER,INTENT(IN) :: wo!, ryr
  INTEGER,INTENT(IN) :: nx, ny
  !ORG  INTEGER,INTENT(IN) :: time, nx, ny, grid, dx, dy,wo!, ryr
  integer :: yday
  CHARACTER(LEN=*),INTENT(IN) :: path
  
  ! Output
  INTEGER,INTENT(OUT) :: ncid, dims_var(2)
  
  ! Local
  INTEGER :: ierr, xdim, ydim, tdim

  INTEGER :: xid, yid, tid
  INTEGER :: yrlen(38), diff(2)
  INTEGER :: i
  
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
!  REAL(KIND=SINGLE):: lon(nx), lat(ny)
  
  CHARACTER(LEN=100) :: tunits
  CHARACTER(LEN=  4) :: chryr
    
  integer :: type

  integer :: time

  integer, dimension(1) :: start,counter,stride
  
  CHARACTER(len= 75) :: cncver,ccon,cinst,csname, csid, cuuid, &
       & instname, fname, contact, website, prodtime, ctitle, cproc, cprocver, prod_name, year, month,day

  ! End of header ----------------------------------------------------------
  
!ORG  write(chryr,'(i4)') ryr
  
  ! Create new file
  

  yday=1
  
  write(*,*) path,nx,ny

  ierr = NF90_CREATE(path, NF90_CLOBBER, ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error creating file'
  
  ! Define the 3 dimensions: time / lat / lon
  
  
  ierr = NF90_DEF_DIM(ncid, 'across_track', nx, xdim)
  IF (ierr.NE.NF90_NOERR) STOP 'create x-d 1'
  
!  ierr = NF90_DEF_DIM(ncid, 'along_track', ny, ydim) !ny
  ierr = NF90_DEF_DIM(ncid, 'along_track', NF90_UNLIMITED, ydim) !ny
  IF (ierr.NE.NF90_NOERR) STOP 'create y-d 1'

  if(type .eq. 1 ) ctitle='ORAC Preprocessing lwrtm output file'
  if(type .eq. 2) ctitle='ORAC Preprocessing swrtm output file'
  if(type .eq. 3) ctitle='ORAC Preprocessing prtm output file'
  
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Title',ctitle)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cncver='3.6.3'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',cncver)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ccon='CF-1.4'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CF_Convention_Version',ccon)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cinst='CMSAF!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processing_Institution',cinst)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cproc='ORAC!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processed_with',cproc)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cprocver='1.0!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Processor_Version',cprocver)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  csname='satellite name!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Satellite_Name',csname)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  csid='satellite id!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Satellite_ID',csid)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cuuid='uuid tag!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'uuid',cuuid)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  instname='instrument name!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Instrument_Name',instname)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  fname='file name!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'File_Name',fname)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  contact='contact email!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Email',contact)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  website='contact website!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Contact_Website',website)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  prodtime='production time!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Production_Time',prodtime)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  prod_name='prod_name!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Name',trim(adjustl(prod_name)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  year='year!!!'
  month='month!!!'
  day='day!!!'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date',&
       & trim(adjustl(trim(adjustl(year))//trim(adjustl(month))//trim(adjustl(day)))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'



  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error enddef '
  !



  dims_var(1) = xdim !1
  dims_var(2) = ydim !3
 
!  dims_var(3) = tdim !2
  

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF
  
  RETURN
  
END SUBROUTINE nc_create_file_pp
