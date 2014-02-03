! Name:
!   Diag
!
! Description:
!   Module defining retrieval diagnostic structure for the ECP.
!   This structure holds information relating to a given retrieval done
!   by Invert_Marquardt, such as the number of iterations taken, quality
!   flag etc.
!
!   This source file contains a module Diag_def, which defines a type Diag_t.
!   Where diagnostic information is needed, this module should be used, and
!   a variable (structure) of type Diag_t should be declared.
!
! Arguments:
!   Name       Type    In/Out/Both    Description
!   N/A
!
! Algorithm:
!   N/A
!
! Local variables:
!   Name       Type    Description
!   Diag       struct  
!
! History:
!    2nd Jul 2001, Andy Smith : original version
!   18th Jul 2001, Andy Smith : 
!      Corrected error in QC flag comments.
!   29th May 2013, Gareth Thomas : Added averaging kernel to structure
!      
! Bugs:
!   None known
!
! $Id$
!
!---------------------------------------------------------------------

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
                                    ! State expected error from measurements
				    ! and "null space" sources. (Size: no. of
				    ! active state variables square)
      real          :: Ss(MaxStateVar,MaxStateVar)      
                                    ! State expected error from model parameter
				    ! noise (includes inactive state variables).
				    ! Size Nx by Nx (active).
      real          :: YmFit(MaxNumMeas)     
                                    ! Difference between measurements 
				    ! (SPixel%Ym) and Y at solution X.
				    ! Size: no. of channels used in SPixel.
     real          :: Y0(MaxNumMeas)     
                                    ! maesurements corresponding to first guess
				    !
				    !
      real          :: APFit(MaxStateVar)     
                                    ! Difference between a priori X and  
				    ! solution X for active state variables.
      real          :: YError(MaxNumMeas)     
                                    ! Measurement errors (square root of Sy
				    ! matrix diagonals). Size: no. of channels 
				    ! used in SPixel.
      real          :: AK(MaxStateVar,MaxStateVar)
                                    ! Averaging kernel matrix
   end type Diag_t
   
end module Diag_def
