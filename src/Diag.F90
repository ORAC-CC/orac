!-------------------------------------------------------------------------------
! Name:
!   Diag
!
! Description:
!   Module defining retrieval diagnostic structure for the ECP. This structure
!   holds information relating to a given retrieval done by Invert_Marquardt,
!   such as the number of iterations taken, quality flag etc.
!
! Arguments:
!   Name Type In/Out/Both Description
!   N/A
!
! Algorithm:
!   N/A
!
! Local variables:
!   Name Type Description
!   N/A
!
! History:
!     2nd Jul 2001, Andy Smith: Original version
!    18th Jul 2001, Andy Smith:
!       Corrected error in QC flag comments.
!    29th May 2013, Gareth Thomas:
!       Added averaging kernel to structure
!    21th May 2014, Greg McGarragh:
!       Cleaned up the code.
!    1st Dec. 2014, CP added cloud albedo
!
! Bugs:
!   None known.
!
! $Id$
!
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

end module Diag_def
