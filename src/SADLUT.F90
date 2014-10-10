!-------------------------------------------------------------------------------
! Name:
!    SAD_LUT_def
!
! Purpose:
!    Module defining the Static Application Data Look-Up Table structure and
!    supporting routines.
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
!
! History:
!     4th Oct 2000, Andy Smith : original version
!     1st Dec 2000, Andy Smith:
!       Renamed Solzen variables to Solzen
!    12th Jan 2001, Andy Smith:
!       Changing main LUT arrays (RBd etc) to allocatable.
!    11th June 2011, Caroline Poulsen: removed references to maximum values and
!       changed some variables to pointers values
!    12th Dec 2013, MJ: makes LUTs more flexible wrt channel and properties
!    12th Jan 2014, Greg McGarragh: Increase nmaxre to 23 for the ice LUTs.
!    16th Jan 2014, Greg McGarragh: Added SAD_LUT%table_use* arrays.
!    23rd Jan 2014, Greg McGarragh: Cleaned up the code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module SAD_LUT_def

   use ECP_Constants

   private

   public :: LUT_Grid_t, &
             SAD_LUT_t, &
             Alloc_SAD_LUT, &
             Dealloc_SAD_LUT, &
             Read_SAD_LUT

   type LUT_Grid_t
      real,    pointer  :: MaxTau(:,:)    ! Optical depth grid max.
      real,    pointer  :: MinTau(:,:)    !  - grid min
      real,    pointer  :: dTau(:,:)      !  - grid spacing
      integer, pointer  :: nTau(:,:)      !  - no. of gridpoints
      real,    pointer  :: MaxRe(:,:)     ! Particle size grid max.
      real,    pointer  :: MinRe(:,:)     !  - grid min
      real,    pointer  :: dRe(:,:)       !  - grid spacing
      integer, pointer  :: nRe(:,:)       !  - no. of gridpoints
      real,    pointer  :: MaxSatzen(:,:) ! Satellite angle grid max.
      real,    pointer  :: MinSatzen(:,:) !  - grid min
      real,    pointer  :: dSatzen(:,:)   !  - grid spacing
      integer, pointer  :: nSatzen(:,:)   !  - no. of gridpoints
      real,    pointer  :: MaxSolzen(:,:) ! Solar angle grid max.
      real,    pointer  :: MinSolzen(:,:) !  - grid min
      real,    pointer  :: dSolzen(:,:)   !  - grid spacing
      integer, pointer  :: nSolzen(:,:)   !  - no. of gridpoints
      real,    pointer  :: MaxRelazi(:,:) ! Relative azimuth grid max.
      real,    pointer  :: MinRelazi(:,:) !  - grid min
      real,    pointer  :: dRelazi(:,:)   !  - grid spacing
      integer, pointer  :: nRelazi(:,:)   !  - no. of gridpoints

      real,    pointer  :: Tau(:,:,:)
      real,    pointer  :: Re(:,:,:)
      real,    pointer  :: Solzen(:,:,:)
      real,    pointer  :: Satzen(:,:,:)
      real,    pointer  :: Relazi(:,:,:)

      integer          :: nmaxtau    = 20
      integer          :: nmaxre     = 23
      integer          :: nmaxsolzen = 20
      integer          :: nmaxsatzen = 20
      integer          :: nmaxrelazi = 20
   end type LUT_Grid_t


   type SAD_LUT_t
      integer          :: Index         ! Reference index
      character(128)   :: Name          ! Optional class name
      real,    pointer :: Wavelength(:) ! Channel wavelengths

      logical, pointer :: table_used_for_channel(:, :)

      logical          :: table_uses_solzen(maxcrprops)
      logical          :: table_uses_relazi(maxcrprops)
      logical          :: table_uses_satzen(maxcrprops)

      type(LUT_Grid_t) :: Grid

      real, pointer :: Rbd(:,:,:,:,:,:)
                       ! Bi-directional reflectance
		       ! Dimensions: channel, Tau, SatZen, SolZen, RelAzi, Re

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

      real, pointer :: Td(:,:,:,:)
                       ! Diffuse transmission
		       ! Dimensions: channel, Tau, SatZen, Re

      real, pointer :: Tfbd(:,:,:,:)
                       ! Diffuse part of beam transmission (flux)
		       ! Dimensions: channel, Tau, SolZen, Re

      real, pointer :: Tfd(:,:,:)
                       ! Diffuse reflectivity
		       ! Dimensions: channel, Tau, Re

      real, pointer :: Em(:,:,:,:)
                       ! Diffuse emissivity
		       ! Dimensions: channel, Tau, SatZen, Re
   end type SAD_LUT_t

contains

! Here we use a C preprocessor include instead of a Fortran include since
! ReadLUT.F90 contains C preprocessor statements but the C preprocessor won't go
! into Fortran included files.
#include "ReadSADLUT.F90"
#include "AllocSADLUT.F90"
#include "DeallocSADLUT.F90"

end module SAD_LUT_def
