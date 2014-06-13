!-------------------------------------------------------------------------------
! Name:
!
! Purpose:
!
! Description:
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!    10th Nov 2000, Greg McGarragh : Original version
!
! Bugs:
!    None known
!
! $Id: output_routines.F90 1963 2014-02-03 11:38:08Z acpovey $
!
!---------------------------------------------------------------------

module output_routines

   implicit none

   contains

   include 'def_vars_primary.F90'
   include 'def_vars_secondary.F90'

   include 'prepare_primary.F90'
   include 'prepare_secondary.F90'

   include 'write_primary.F90'
   include 'write_secondary.F90'

end module output_routines
