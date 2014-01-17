! Name: nc_create_global.F90
!
!
! Purpose:
! A netcdf output file is opened/created for writing.
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2011/12/19: Matthias Jerg creates initial file.
!2012/11/16: CP added calibration file version
!
! $Id$
!
! Bugs:
!
!none known


SUBROUTINE nc_create_global_l2(Ctrl,path, ncid, nx, ny, dims_var, wo,type,status)
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
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
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
  
  use ECP_Constants

  use CTRL_def

  USE netcdf

  IMPLICIT NONE
  
  ! Input
  INTEGER,INTENT(IN) :: wo
  INTEGER,INTENT(IN) :: nx, ny
  integer :: yday
  CHARACTER(LEN=*),INTENT(IN) :: path
  
  ! Output
  INTEGER,INTENT(OUT) :: ncid, dims_var(2)
  
  ! Local
  INTEGER :: ierr, xdim, ydim
  
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8

  integer :: type

  integer :: status

  CHARACTER(len= 75) :: cncver,ccon,cinst,csname, csid, cuuid, &
       & instname, fname, contact, website, prodtime, ctitle, cproc, cprocver, prod_name, year, month,day,cal_file_ver

  type(CTRL_t)  :: Ctrl

  ! End of header ----------------------------------------------------------
  
  
  ! Create new file
  


  yday=1
  
  ierr = NF90_CREATE(path, NF90_CLOBBER, ncid)
!  ierr=1

  IF (ierr.NE.NF90_NOERR)  then
     
     if(type .eq. 1 ) then
        
        status=PrimaryFileOpenErr
        
        write(*,*) 'nc_create_global.F90: Primary netcdf file open error:', status
        call Write_Log(Ctrl,'nc_create_global.F90: Primary netcdf file open error:', status)
        stop
        
     elseif(type .eq. 2 ) then
        
        status=SecondaryFileOpenErr
        write(*,*) 'nc_create_global.F90: Secondary netcdf file open error:', status
        call Write_Log(Ctrl,'nc_create_global.F90: Secondary netcdf file open error:', status)
        stop
        
     elseif(type .eq. 3 ) then

!        status=InputFileOpenErr
!        write(*,*) 'nc_create_global.F90: Input netcdf file open error:', status
!        call Write_Log(Ctrl,'nc_create_global.F90: Input netcdf file open error:', status)
!        stop

     endif

  endif

  !  write(*,*) ierr,NF90_NOERR
  !  pause
  ! Define the 3 dimensions: time / lat / lon
  
  
  ierr = NF90_DEF_DIM(ncid, 'across_track', nx, xdim)
  IF (ierr.NE.NF90_NOERR) then

     STOP 'create x-d'

  endif
  
!  ierr = NF90_DEF_DIM(ncid, 'along_track', NF90_UNLIMITED, ydim) !ny
  ierr = NF90_DEF_DIM(ncid, 'along_track', ny, ydim) !ny
  IF (ierr.NE.NF90_NOERR) then

     STOP 'create y-d'

  endif


  if(type .eq. 1 ) ctitle='ESA CCI Cloud Retrieval Products L2 Primary Output File'
  if(type .eq. 2) ctitle='ESA CCI Cloud Retrieval Products L2 Secondary File'
  if(type .eq. 3) ctitle='ESA CCI Cloud Retrieval Products L2 Input/Output File'
  
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

  if (Ctrl%Inst%Name .eq. 'AATSR' .or. Ctrl%Inst%Name .eq. 'ATSR'  ) then
     cal_file_ver='3.01'
     ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'AATSR_Calibration_Version',cal_file_ver)
  end if

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


  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF
  
  RETURN
  
END SUBROUTINE nc_create_global_l2
