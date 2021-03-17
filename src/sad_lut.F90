!-------------------------------------------------------------------------------
! Name: sad_lut.F90
!
! Purpose:
! Module defining the Static Application Data Look-Up Table structure and
! supporting routines.
!
! History:
! 2000/10/04, AS: Original version
! 2000/12/01, AS: Renamed Solzen variables to Solzen.
! 2001/01/12, AS: Changing main LUT arrays (Rbd etc) to allocatable.
! 2011/06/11, CP: Removed references to maximum values and changed some
!    variables to pointers values.
! 2013/12/12, MJ: Makes LUTs more flexible wrt channel and properties.
! 2014/01/12, GM: Increase NMaxRe to 23 for the ice LUTs.
! 2014/01/16, GM: Added SAD_LUT%table_use* arrays.
! 2014/01/23, GM: Cleaned up the code.
! 2015/01/09, CP: Added Rfbd.
! 2015/10/19, GM: Added Bext.
! 2017/01/17, GM: Eliminate the unnecessary indexing of the LUT grid wrt LUT
!    type and channel.
! 2017/03/16, GT: Increased maximum size of effective radius LUT dimension.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module SAD_LUT_m

   use ORAC_Constants_m

   private

   public :: LUT_Grid_t, &
             SAD_LUT_t, &
             Alloc_SAD_LUT, &
             Dealloc_SAD_LUT, &
             Read_SAD_LUT

   type LUT_Grid_t
      integer :: NMaxTau    = 20
      integer :: NMaxRe     = 35
      integer :: NMaxSolZen = 20
      integer :: NMaxSatZen = 20
      integer :: NMaxRelAzi = 20

      real     :: MaxTau    ! Optical depth grid max.
      real     :: MinTau    !  - grid min
      real     :: dTau      !  - grid spacing
      integer  :: nTau      !  - no. of gridpoints
      real     :: MaxRe     ! Particle size grid max.
      real     :: MinRe     !  - grid min
      real     :: dRe       !  - grid spacing
      integer  :: nRe       !  - no. of gridpoints
      real     :: MaxSatzen ! Satellite angle grid max.
      real     :: MinSatzen !  - grid min
      real     :: dSatzen   !  - grid spacing
      integer  :: nSatzen   !  - no. of gridpoints
      real     :: MaxSolzen ! Solar angle grid max.
      real     :: MinSolzen !  - grid min
      real     :: dSolzen   !  - grid spacing
      integer  :: nSolzen   !  - no. of gridpoints
      real     :: MaxRelazi ! Relative azimuth grid max.
      real     :: MinRelazi !  - grid min
      real     :: dRelazi   !  - grid spacing
      integer  :: nRelazi   !  - no. of gridpoints

      real, pointer  :: Tau(:)
      real, pointer  :: Re(:)
      real, pointer  :: Solzen(:)
      real, pointer  :: Satzen(:)
      real, pointer  :: Relazi(:)
   end type LUT_Grid_t


   type SAD_LUT_t
      integer          :: Index         ! Reference index
      character(128)   :: Name          ! Optional class name
      real,    pointer :: Wavelength(:) ! Channel wavelengths

      type(LUT_Grid_t) :: Grid

      real, pointer :: Bext(:,:,:)
                       ! Extinction coefficient
                       ! Dimensions: channel, Tau, Re

      real, pointer :: Rbd(:,:,:,:,:,:)
                       ! Bi-directional reflectance
                       ! Dimensions: channel, Tau, SatZen, SolZen, RelAzi, Re

      real, pointer :: Rfbd(:,:,:,:)
                       ! Diffuse part of diffuse reflectance
                       ! Dimensions: channel, Tau, SolZen, Re

      real, pointer :: Rd(:,:,:,:)
                       ! Diffuse reflectance
                       ! Dimensions: channel, Tau, SatZen, Re

      real, pointer :: Rfd(:,:,:)
                       ! Diffuse reflectivity
                       ! Dimensions: channel, Tau, Re

      real, pointer :: Tb(:,:,:,:)
                       ! Direct part of beam transmission
                       ! Dimensions: channel, Tau, SolZen, Re

      real, pointer :: Tbd(:,:,:,:,:,:)
                       ! Bi-directional reflectance
                       ! Dimensions: channel, Tau, SatZen, SolZen, RelAzi, Re

      real, pointer :: Tfbd(:,:,:,:)
                       ! Diffuse part of beam transmission (flux)
                       ! Dimensions: channel, Tau, SolZen, Re

      real, pointer :: Td(:,:,:,:)
                       ! Diffuse transmission
                       ! Dimensions: channel, Tau, SatZen, Re

      real, pointer :: Tfd(:,:,:)
                       ! Diffuse reflectivity
                       ! Dimensions: channel, Tau, Re

      real, pointer :: Em(:,:,:,:)
                       ! Diffuse emissivity
                       ! Dimensions: channel, Tau, SatZen, Re

      real, pointer :: BextRat(:,:)
                       ! Ratio of AOD at some wavelength to that at 550 nm
                       ! Dimensions: Re
   end type SAD_LUT_t

contains

#include "read_sad_lut.F90"
#include "alloc_sad_lut.F90"
#include "dealloc_sad_lut.F90"

end module SAD_LUT_m
