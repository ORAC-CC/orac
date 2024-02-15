!-------------------------------------------------------------------------------
! Name: diag.F90
!
! Purpose:
! Module defining retrieval diagnostic structure for the ORAC. This structure
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
! 2015/07/31, AP: Rejig QCFlag for much longer state vector.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2015/01/07, AP: Make QCFlag long to accomodate longer state vectors.
! 2016/01/27, GM: Add cloud emissivity and cloud emissivity uncertainty.
! 2017/07/12, AP: New QC
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module Diag_m

   use ORAC_Constants_m

   implicit none

   type Diag_t
      logical       :: Converged    ! Did the retrieval converge?
      integer(lint) :: QCFlag       ! Quality control flag. Bits are set as
                                    ! follows:
                                    ! - bit 0: retrieval solution
                                    !   cost too great
                                    ! - bits 1 to SPixel%Nx: set to 1 if
                                    !   parameter's estimated retrieval
                                    !   uncertainty too large
      integer       :: LimitHit     ! Has the retrieval ever hit a limit?
      integer       :: Iterations   ! Number of iterations taken by inversion
                                    ! scheme to reach convergence
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
      real          :: AK(MaxStateVar,MaxStateVar)
                                    ! Averaging kernel matrix
      real          :: cloud_albedo(MaxNumSolar)
                                    ! Cloud albedo
      real          :: cloud_albedo_s(MaxNumSolar)
                                    ! Cloud albedo variance
      real          :: cloud_emissivity(MaxNumThermal)
                                    ! Cloud emissivity
      real          :: cloud_emissivity_s(MaxNumThermal)
                                    ! Cloud emissivity variance
      real          :: diffuse_frac(MaxNumSolar)
                                    ! Diffuse fraction of radiance
      real          :: diffuse_frac_s(MaxNumSolar)
                                    ! Diffuse fraction variance
      real          :: aot870       ! Aerosol optical depth at 870 nm
      real          :: aot870_uncertainty
                                    ! Uncertainty in aot870
   end type Diag_t

contains

#include "zero_diag.F90"
#include "set_diag.F90"

end module Diag_m
