!-------------------------------------------------------------------------------
! Name:
!    Int_LUT_Routines_def
!
! Purpose:
!    Module with ECP LUT Interpolation routines
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
!    10th Nov 2000, Andy Smith : Original version
!    16th Nov 2000, Andy Smith :
!       Adding more Int routines: Int_LUT_TauReSun.
!     1st Dec 2000, Andy Smith :
!       Replaced "sun" in routine names with Sol
!    11th Jan 2001, Andy Smith :
!       Chans argument removed from all routines. Redundant since interpolation
!       is done over the entire passed array.
!     5th Sep 2011, Chris Arnold:
!       Added interfaces for spline/locate routines and updated interfaces for
!       Int_LUT routines
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to interpolation routines IntLUT*.f90
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    16th Jan 2014, Greg McGarragh:
!       Added i_chan_to_ctrl_offset and i_chan_to_spixel_offset to subroutine
!       Int_LUT_TauSatRe.
!    20th Dec 2014, Greg McGarragh:
!       Cleaned up code.
!    24th Dec 2014, Greg McGarragh:
!       Some intent changes.
!    23th May 2014, Greg McGarragh:
!       No need for handmade explicit interfaces. Just need to include the
!       subroutines in the module and the interfaces are automatically generated.
!
! Bugs:
!    None known.
!
! $Id: IntRoutines.F90 1963 2014-02-03 11:38:08Z acpovey $
!
!---------------------------------------------------------------------

module Int_LUT_Routines_def

   implicit none

   contains

   include 'Interp3dLUT.F90'

   include 'IntLUTTauRe.F90'
   include 'IntLUTTauSatRe.F90'
   include 'IntLUTTauSatSolAziRe.F90'
   include 'IntLUTTauSolRe.F90'

end module Int_LUT_Routines_def
