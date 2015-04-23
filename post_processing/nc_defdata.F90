! Name: nc_defdata.F90
!
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for different attribute/variable type combinations.
! Subroutines names are selfdescriptive.
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
!2012/07/06 MJ extensively overhauls and restructures the code
!2013/06/28 MJ replaces subroutine with locally updated ones.
!2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when building libraries.
!2014/11/20 OS: added calendar attribute to nc_defdata_double_pp
!  (only used for time variable)
!2014/12/03 CP added in common_constants should eventually remove vartypes_pp
!2015/04/23 OS: added deflate_level and shuffle to NETCDF4 variables
!
! $Id$
!
! Bugs:
!
!none known


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_float_pp(ncid,dims,var_name,vid,var_lname,var_sname,std_flag, &
     var_unit,var_fill,scale, offset, vmin, vmax,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

  ! Description:
  !
  ! For definition of data.
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
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

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo,std_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname
  REAL ::  var_fill, scale, offset, vmin, vmax

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_FLOAT, dims, vid, &
     & deflate_level=compress_level_float,shuffle=shuffle_float)
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var float'


  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var long_name'
 

  if(std_flag .eq.1) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var standard_name'
  endif

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_float_pp


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_float_no_att_pp(ncid,dims,var_name,vid,var_lname,var_sname,var_fill,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_sname
  REAL ::  var_fill
! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))


  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_FLOAT, dims, vid, &
     & deflate_level=compress_level_float,shuffle=shuffle_float)

  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var float'


  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var long_name'
 

  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var standard_name'

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var FillValue'
  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_float_no_att_pp

!-----------------------------------------------
!-----------------------------------------------
SUBROUTINE nc_defdata_double_pp(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_fill,scale, offset, vmin, vmax, wo, ierr, calendar)
!-----------------------------------------------
!-----------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

 use vartypes_pp
  use common_constants

  use netcdf

  IMPLICIT NONE


! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname
  CHARACTER(len=*),INTENT(in), optional :: calendar
  REAL(kind=dreal) ::  var_fill, scale, offset, vmin, vmax

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '


  ierr = NF90_DEF_VAR(ncid, var_name, NF90_DOUBLE, dims, vid, &
     & deflate_level=compress_level_double,shuffle=shuffle_double)
! write(*,*) ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var float'

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var long_name'
 

  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var standard_name'

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  IF ( present( calendar )) THEN
    ierr = NF90_PUT_ATT(ncid, vid, 'calendar', calendar)
    IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def calendar'
  ENDIF
  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_double_pp



!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_short_pp(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_fill,scale, offset, vmin, vmax,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !-----------------------------------------------------------------------
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 

! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname
  REAL ::  scale, offset
  integer(kind=sint) :: vmin, vmax
  integer(kind=sint) :: var_fill

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error redef '



  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid, &
     & deflate_level=compress_level_stint,shuffle=shuffle_stint)

  if (ierr.NE.NF90_NOERR)  write(*,*) 'err def var short',ierr
  
  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var long_name'
 

  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var standard_name'

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'

  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_short_pp


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_short_no_units_pp(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_fill,scale, offset, vmin, vmax,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !-----------------------------------------------------------------------
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 

! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_sname
  REAL ::  scale, offset
  integer(kind=sint) :: vmin, vmax
  integer(kind=sint) :: var_fill

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid, &
     & deflate_level=compress_level_stint,shuffle=shuffle_stint)

  ! write(*,*) 'xyz',ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR)  write(*,*) 'err def var short'
  
  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var long_name'
 
  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var standard_name'

20 continue



  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'

  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_short_no_units_pp

!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_long_pp(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_fill,scale, offset, vmin, vmax,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !-----------------------------------------------------------------------
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname
  REAL ::  scale, offset
  integer :: vmin, vmax
  integer :: var_fill

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid, &
     & deflate_level=compress_level_lint,shuffle=shuffle_lint)

  !  write(*,*) ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var long'
  

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var long_name'
 
  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var standard_name'

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'

  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_long_pp



!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_byte_pp(ncid,dims,var_name,vid,var_lname,var_sname,std_flag, &
     var_unit,var_fill,scale, offset, vmin, vmax,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !-----------------------------------------------------------------------
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo,std_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname
  integer(kind=byte) ::  scale, offset, vmin, vmax
  integer(kind=byte) :: var_fill

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_BYTE, dims, vid, &
     & deflate_level=compress_level_byte,shuffle=shuffle_byte)

  !  write(*,*) 'xxx',ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var short'
  

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var long_name'
 
  if(std_flag .eq. 1) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var standard_name'
  endif

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'

  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_byte_pp

!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_byte_flag_value_pp(ncid,dims,var_name,vid,var_lname,var_sname, &
     flag_vals,nflags,std_flag,var_mean,var_fill,scale, offset, vmin, vmax,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !-----------------------------------------------------------------------
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo,nflags,std_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_sname,var_mean
  integer(kind=byte) ::  scale, offset, vmin, vmax
  integer(kind=byte) :: var_fill,flag_vals(nflags)


! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_BYTE, dims, vid, &
     & deflate_level=compress_level_byte,shuffle=shuffle_byte)

!  write(*,*) 'yyy',ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var short'
  

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var long_name'

  if(std_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var standard_name'
  endif

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'flag_values' ,flag_vals )
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, 'flag_meanings' ,var_mean )
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'

  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_byte_flag_value_pp


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_short_flag_value_pp(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_mean,var_fill,scale, offset, vmin, vmax,wo,ierr)
!--------------------------------------------------
!--------------------------------------------------

! Description:
!
! For definition of data.
!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !-----------------------------------------------------------------------
!
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  use vartypes_pp
  use common_constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_sname,var_mean
  integer(kind=sint) ::  scale, offset, vmin, vmax
  integer(kind=sint) :: var_fill

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
  CHARACTER(len=100) :: flag_mea
  INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid, &
     & deflate_level=compress_level_stint_flag,shuffle=shuffle_stint_flag)

!  write(*,*) 'www',ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR)  write(*,*) 'err def var short'
  
  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var long_name'
 
  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var standard_name'

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'flag_meanings' ,var_mean )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'

  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN
  
END SUBROUTINE nc_defdata_short_flag_value_pp
