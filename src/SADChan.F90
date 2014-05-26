!-------------------------------------------------------------------------------
! Name:
!    SAD_Chan
!
! Purpose:
!    Module containing Static Application Data Channel structure
!
! Description:
!    Defines a set of structures used to hold Static Application Data for
!    measurement channels in the ECP.
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
!     3rd Aug 2000, Andy Smith: Original version
!    23rd Nov 2000, Andy Smith:
!       Rs in Solar is now an array of 2 values rather than a single real.
!    19th Jan 2001, Kevin Smith:
!       Added SRs and CRs fields to the Solar type.
!    29th Jan 2001, Kevin Smith:
!       Removed SRs and CRs fields - now taken from Ctrl.
!    11th Jul 2001, Andy Smith:
!       Added f1 to solar struct. Allows for calculation of f0 based on the day
!       of year rather than just using an annual mean value.
!    23rd May 2014, Greg McGarragh:
!       Cleaned up code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module SAD_Chan_def

   use ECP_constants

   implicit none

   type Solar_t
      integer(1)  :: Flag        ! Value 1 indicates solar source is present
      real        :: F0          ! Solar constant (annual mean).
      real        :: F1          ! Amplitude of variation in solar constant.
      real        :: NeHomog(MaxCloudType) ! Homogeneity noise
      real        :: NeCoreg(MaxCloudType) ! Coregistration noise
      real        :: NedR        ! Noise equivalent delta radiance
      real        :: Rs(2)       ! 'Typical' land/sea reflectance
   end type Solar_t

   type Thermal_t
      integer(1)  :: Flag        ! Value 1 indicates thermal sources present
      real        :: B1          ! Planck function coefficient
      real        :: B2          ! Planck function coefficient
      real        :: T1          ! Planck function coefficient
      real        :: T2          ! Planck function coefficient
      real        :: NeHomog(MaxCloudType) ! Homogeneity noise
      real        :: NeCoreg(MaxCloudType) ! Coregistration noise
      real        :: NEBT        ! Noise equivalent brightness temperature
   end type Thermal_t

   type SAD_Chan_t
      character(10)   :: Desc    ! Descriptor (e.g. 0.6 um)
      character(6)    :: FileID  ! File tag (e.g. CH3)
      real            :: WvN     ! Central wavenumber
      type(Thermal_t) :: Thermal ! Thermal sub-structure
      type(Solar_t)   :: Solar   ! Solar sub-structure
   end type SAD_Chan_t

contains

   include 'ReadChan.F90'

end module SAD_Chan_def
