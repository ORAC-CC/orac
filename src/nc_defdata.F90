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
!
! $Id$
!
! Bugs:
!
!none known


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_float(ncid,dims,var_name,vid,var_lname,var_sname, &
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

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
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

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_FLOAT, dims, vid)
!  write(*,*) ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var float'


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

END SUBROUTINE nc_defdata_float


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_float_no_att(ncid,dims,var_name,vid,var_lname,var_sname,var_fill,wo,ierr)
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

!  write(*,*) 'ncid', ncid
  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_FLOAT, dims, vid)
!  write(*,*) ncid, var_name, dims, vid
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

END SUBROUTINE nc_defdata_float_no_att

!-----------------------------------------------
!-----------------------------------------------
SUBROUTINE nc_defdata_double(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_fill,scale, offset, vmin, vmax,wo,ierr)
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

  use ECP_constants

  use netcdf

  IMPLICIT NONE


! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname
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


  ierr = NF90_DEF_VAR(ncid, var_name, NF90_DOUBLE, dims, vid)
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
  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_double



!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_short(ncid,dims,var_name,vid,var_lname,var_sname, &
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

  use ECP_Constants

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


  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid)
!  write(*,*) ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR)  write(*,*) 'err def var short'
  
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

END SUBROUTINE nc_defdata_short


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_short_no_units(ncid,dims,var_name,vid,var_lname,var_sname, &
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

  use ECP_Constants

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

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid)
!  write(*,*) ncid, var_name, dims, vid
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

END SUBROUTINE nc_defdata_short_no_units

!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_long(ncid,dims,var_name,vid,var_lname,var_sname, &
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

  use ECP_Constants

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

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid)
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

END SUBROUTINE nc_defdata_long



!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_byte(ncid,dims,var_name,vid,var_lname,var_sname, &
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

  use ECP_Constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
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

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_BYTE, dims, vid)
!  write(*,*) ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var short'
  

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

END SUBROUTINE nc_defdata_byte

!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_byte_flag_value(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_mean,var_fill,scale, offset, vmin, vmax,wo,ierr)
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

  use ECP_Constants

  IMPLICIT NONE
 
! Input
  INTEGER,INTENT(in) :: ncid, dims(2),  wo
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname,var_mean
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

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_BYTE, dims, vid)
!  write(*,*) ncid, var_name, dims, vid
  if (ierr.NE.NF90_NOERR) write(*,*) 'err def var short'
  

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var long_name'
 
  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR) write(*,*) 'error def var standard_name'

20 continue


  ierr = NF90_PUT_ATT(ncid, vid, 'flag_values' ,var_unit )
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

END SUBROUTINE nc_defdata_byte_flag_value


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE nc_defdata_short_flag_value(ncid,dims,var_name,vid,var_lname,var_sname, &
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

  use ECP_Constants

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

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid)
!  write(*,*) ncid, var_name, dims, vid
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
  
END SUBROUTINE nc_defdata_short_flag_value
