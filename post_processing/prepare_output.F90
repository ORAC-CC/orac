!-------------------------------------------------------------------------------
! Name: prepare_output.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2013/03/12, CP: changed 32767 to 999
! 2014/10/24, OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM,
!    and nisemask
! 2014/11/20, OS: added Pavolonis cloud phase related variables
! 2014/11/26, CP: added Pavolonis cloud_albedo
! 2015/02/05, OS: deactivated use of postproc_constants to force consistency with
!    common_constants
! 2015/07/06, OS: added vid_cldmaskerror
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module prepare_output

   implicit none

contains

#include "prepare_primary_pp.F90"
#include "prepare_secondary_pp.F90"

end module prepare_output
