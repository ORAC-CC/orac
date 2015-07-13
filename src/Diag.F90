!-------------------------------------------------------------------------------
! Name: Diag.F90
!
! Purpose:
! Module defining retrieval diagnostic structure for the ECP. This structure
! holds information relating to a given retrieval done by Invert_Marquardt,
! such as the number of iterations taken, quality flag etc.
!
! History:
! 2001/07/02, AS: Original version
! 2001/07/18, AS: Corrected error in QC flag comments.
! 2013/05/29, GT: Added averaging kernel to structure
! 2014/05/21, GM: Cleaned up the code.
! 2014/12/01, CP: Added cloud albedo.
! 2015/01/19, GM: Put ZeroDiag.F90 and SetDiag.F90 into this module.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module Diag_def

   use ECP_constants

   implicit none

   type Diag_t
      integer       :: QCFlag       ! Quality control flag. Bits are set as
                                    ! follows:
				    ! - bits 1 to MaxStateVar: set to 1 if
				    !   parameter estimated retrieval error
				    !   too large
				    ! - bit MaxStateVar+1: retrieval did not
				    !   converge
				    ! - bit MaxStateVar+2: retrieval solution
				    !   cost too great
      integer       :: Iterations   ! Number of iterations taken by inversion
                                    ! scheme to reach convergence
      integer       :: PhaseChanges ! Number of phase changes during inversion
      real          :: Jm           ! Cost at solution due to measurements
      real          :: Ja           ! Cost at solution due to a priori
      real          :: St(MaxStateVar,MaxStateVar)
                                    ! State expected error from measurements and
                                    ! "null space" sources. (Size: no. of active
                                    ! state variables square)
      real          :: Ss(MaxStateVar,MaxStateVar)
                                    ! State expected error from model parameter
				    ! noise (includes inactive state variables).
				    ! Size Nx by Nx (active).
      real          :: Y0(MaxNumMeas)
                                    ! Measurements corresponding to first guess
      real          :: YmFit(MaxNumMeas)
                                    ! Difference between measurements
				    ! (SPixel%Ym) and Y at solution X.
				    ! Size: no. of channels used in SPixel.
      real          :: YError(MaxNumMeas)
                                    ! Measurement errors (square root of Sy
				    ! matrix diagonals). Size: no. of channels
				    ! used in SPixel.
      real          :: APFit(MaxStateVar)
                                    ! Difference between a priori X and
				    ! solution X for active state variables.
      real          :: AK(MaxStateVar,MaxStateVar)
                                    ! Averaging kernel matrix
      real          :: cloud_albedo(MaxNumSolar)
                                    ! Cloud albedo
   end type Diag_t

contains

#include "ZeroDiag.F90"
#include "SetDiag.F90"

end module Diag_def
