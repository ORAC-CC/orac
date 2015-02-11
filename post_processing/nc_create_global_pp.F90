! Name: nc_create_global_pp.F90
!
!
! Purpose:
! A netcdf output file is opened/created for writing.
! 
!
! Description and Algorithm details:
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90!
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
!2012/07/06 MJ extensively overhauls and restructures the code
!2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when building libraries.
!2014/12/2 CP added in source and new global attributes and big tidy up
!2015/02/07 CP changed to common constants
!
! $Id$
!
! Bugs:
!
!none known



SUBROUTINE nc_create_global_l2_pp(path, ncid, nx, ny, dims_var, wo,type,global_atts,source_atts,status)

 
  !
  ! Declarations:
  !
  
  use vartypes_pp
  use common_constants
  use global_attributes
  use source_attributes
  use orac_ncdf

  USE netcdf

  IMPLICIT NONE
  
  ! Input
   type(global_attributes_s), intent(inout)  :: global_atts
   type(source_attributes_s), intent(inout)  :: source_atts

  INTEGER,INTENT(IN) :: wo
  INTEGER,INTENT(IN) :: nx, ny

!  integer :: yday
  CHARACTER(LEN=*),INTENT(IN) :: path
  
  ! Output
  INTEGER,INTENT(OUT) :: ncid, dims_var(2)
  
  ! Local
  INTEGER :: ierr, xdim, ydim, tdim

!  INTEGER :: xid, yid, tid
!  INTEGER :: yrlen(38), diff(2)
  INTEGER :: i
  
!  INTEGER, PARAMETER :: SINGLE = 4
!  INTEGER, PARAMETER :: DOUBLE = 8

  
!  CHARACTER(LEN=100) :: tunits
!  CHARACTER(LEN=  4) :: chryr
    
  integer :: type

!  integer :: time, cposition,clength

!  integer, dimension(1) :: start,counter,stride
  
  integer :: status
  character(len=attribute_length_long) ::  ctitle

  ! End of header ----------------------------------------------------------
  
  ! Create new file
  
  
  ierr = NF90_CREATE(path, NF90_CLOBBER, ncid)


  IF (ierr.NE.NF90_NOERR)  then
     
     if(type .eq. 1 ) then
        
        status=PrimaryFileOpenErr
        
        write(*,*) 'nc_create_global.F90: Primary netcdf file open error:', status
        !call Write_Log(Ctrl,'nc_create_global.F90: Primary netcdf file open error:', status)
        stop
        
     elseif(type .eq. 2 ) then
        
        status=SecondaryFileOpenErr
        write(*,*) 'nc_create_global.F90: Secondary netcdf file open error:', status
        !call Write_Log(Ctrl,'nc_create_global.F90: Secondary netcdf file open error:', status)
        stop
        
     elseif(type .eq. 3 ) then

!        status=InputFileOpenErr
!        write(*,*) 'nc_create_global.F90: Input netcdf file open error:', status
!        call Write_Log(Ctrl,'nc_create_global.F90: Input netcdf file open error:', status)
!        stop

     endif

  endif


  ! Define the 3 dimensions: time / lat / lon
  
  
  ierr = NF90_DEF_DIM(ncid, 'across_track', nx, xdim)
  IF (ierr.NE.NF90_NOERR) then

     STOP 'create x-d 12'

  endif
  
!  ierr = NF90_DEF_DIM(ncid, 'along_track', NF90_UNLIMITED, ydim) !ny
  ierr = NF90_DEF_DIM(ncid, 'along_track', ny, ydim) !ny
  IF (ierr.NE.NF90_NOERR) then

     STOP 'create y-d 12'

  endif


  if(type .eq. 1 ) ctitle='ESA Cloud CCI Retrieval Products L2 Primary Output File'
  if(type .eq. 2) ctitle='ESA Cloud CCI Retrieval Products L2 Secondary File'
  if(type .eq. 3) ctitle='ESA Cloud CCI Retrieval Products L2 Input/Output File'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'title',trim(adjustl(ctitle)))
  IF (ierr.NE.NF90_NOERR) stop 'error def title'

! write out global and source attributes propagated from pre processing
  call nc_put_common_attributes(ncid, global_atts,source_atts)

  dims_var(1) = xdim !1
  dims_var(2) = ydim !3
 
!  dims_var(3) = tdim !2
  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error enddef '
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF
  
  RETURN
  
END SUBROUTINE nc_create_global_l2_pp


