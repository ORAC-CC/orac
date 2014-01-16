! Name:
!    SAD_LUT
!
! Purpose:
!    Module containing Static Application Data Look-Up Table structure
! 
! Description:
!    Defines a set of structures used to hold Static Application Data 
!    for Look-Up Tables in the ECP. 
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name       Type    Description
!    To be completed once questions are answered
!
! History:
!    4th Oct 2000, Andy Smith : original version
!    1st Dec 2000, Andy Smith:
!       Renamed Solzen variables to Solzen
!    12th Jan 2001, Andy Smith:
!       Changing main LUT arrays (RBd etc) to allocatable. 
!    11th June 2011, Caroline Poulsen: removed refrences to maxnum values 
!       and changed some variables to pointers values
!    12th Dec 2013, MJ: makes LUTs more flexible wrt channel and properties
!    12th Jan 2014, Greg McGarragh: Increase nmaxre to 23 for the ice LUTs.
!    16th Jan 2014, Greg McGarragh: Added SAD_LUT%table_use* arrays.
!
! Bugs:
!    None known.
!
! $Id: SADLUT.f90 182 2011-10-05 10:03:40Z carnold $
!
!---------------------------------------------------------------------

module SAD_LUT_def

   use ECP_Constants

   type LUT_Grid_t
      real, pointer      :: MaxTau(:,:)                    ! Optical depth grid max.
      real,pointer      :: MinTau(:,:)                    !  - grid min
      real, pointer      :: dTau(:,:)                      !  - grid spacing
      integer,pointer   :: nTau(:,:)                      !  - no. of gridpoints
      real,pointer      :: MaxRe(:,:)                     ! Particle size grid max.
      real,pointer      :: MinRe(:,:)                     !  - grid min
      real,pointer      :: dRe(:,:)                       !  - grid spacing
      integer,pointer   :: nRe(:,:)                       !  - no. of gridpoints
      real,pointer      :: MaxSatzen(:,:)                 ! Satellite angle grid max.
      real,pointer      :: MinSatzen(:,:)                 !  - grid min
      real,pointer      :: dSatzen(:,:)                   !  - grid spacing
      integer,pointer   :: nSatzen(:,:)                   !  - no. of gridpoints
      real,pointer      :: MaxSolzen(:,:)                 ! Solar angle grid max.
      real,pointer      :: MinSolzen(:,:)                 !  - grid min
      real,pointer      :: dSolzen(:,:)                   !  - grid spacing
      integer,pointer   :: nSolzen(:,:)                   !  - no. of gridpoints
      real,pointer      :: MaxRelazi(:,:)                 ! Relative azimuth grid max.
      real,pointer      :: MinRelazi(:,:)                 !  - grid min
      real,pointer      :: dRelazi(:,:)                   !  - grid spacing
      integer,pointer   :: nRelazi(:,:)                   !  - no. of gridpoints

      real, pointer :: Tau(:,:,:)
      real, pointer :: Re(:,:,:)
      real, pointer :: Satzen(:,:,:)
      real, pointer :: Solzen(:,:,:)
      real, pointer :: Relazi(:,:,:)

      integer :: nmaxtau=20
      integer :: nmaxre=23
      integer :: nmaxsolzen=20
      integer :: nmaxsatzen=20
      integer :: nmaxrelazi=20

      !integer, parameter :: 
   
   end type LUT_Grid_t



   type SAD_LUT_t
      integer       :: Index  ! Reference index
      character(80) :: Name   ! Optional class name
      real, pointer :: Wavelength(:) ! Channel wavelengths 

      logical, pointer :: table_used_for_channel(:, :)

      logical :: table_uses_satzen(maxcrprops)
      logical :: table_uses_solzen(maxcrprops)
      logical :: table_uses_relazi(maxcrprops)

      real, pointer :: Rbd(:,:,:,:,:,:)
                       ! Bi-directional reflectance
		       ! Dimensions: channel, Tau, SatZen, SolZen, RelAzi, Re

      real, pointer :: Tbd(:,:,:,:,:,:)
                       ! Bi-directional reflectance
		       ! Dimensions: channel, Tau, SatZen, SolZen, RelAzi, Re

	
      real, pointer :: Tb(:,:,:,:)
                       ! Direct part of beam transmission
		       ! Dimensions: channel, Tau, SolZen, Re
	
      real, pointer :: Tfbd(:,:,:,:)
                       ! Diffuse part of beam transmission (flux)
		       ! Dimensions: channel, Tau, SolZen, Re
	
      real, pointer :: Td(:,:,:,:)
                       ! Diffuse transmission
		       ! Dimensions: channel, Tau, SatZen, Re
	
      real, pointer :: Tfd(:,:,:)
                       ! Diffuse reflectivity
		       ! Dimensions: channel, Tau, Re
 
      real, pointer :: Rd(:,:,:,:)
                       ! Diffuse reflectance
		       ! Dimensions: channel, Tau, SatZen, Re
	
      real, pointer :: Rfd(:,:,:)
                       ! Diffuse reflectivity
		       ! Dimensions: channel, Tau, Re
 
      real, pointer :: Em(:,:,:,:)
                       ! Diffuse emissivity
		       ! Dimensions: channel, Tau, SatZen, Re
      
      type(LUT_Grid_t) :: Grid   ! Grid parameters

   end type SAD_LUT_t

end module SAD_LUT_def
