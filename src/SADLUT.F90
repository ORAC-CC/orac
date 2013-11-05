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
      real      :: MaxTau                    ! Optical depth grid max.
      real      :: MinTau                    !  - grid min
      real      :: dTau                      !  - grid spacing
      integer   :: nTau                      !  - no. of gridpoints
       real      :: MaxRe                     ! Particle size grid max.
      real      :: MinRe                     !  - grid min
      real      :: dRe                       !  - grid spacing
      integer   :: nRe                       !  - no. of gridpoints
       real      :: MaxSatzen                 ! Satellite angle grid max.
      real      :: MinSatzen                 !  - grid min
      real      :: dSatzen                   !  - grid spacing
      integer   :: nSatzen                   !  - no. of gridpoints
       real      :: MaxSolzen                 ! Solar angle grid max.
      real      :: MinSolzen                 !  - grid min
      real      :: dSolzen                   !  - grid spacing
      integer   :: nSolzen                   !  - no. of gridpoints
       real      :: MaxRelazi                 ! Relative azimuth grid max.
      real      :: MinRelazi                 !  - grid min
      real      :: dRelazi                   !  - grid spacing
      integer   :: nRelazi                   !  - no. of gridpoints

      real, pointer :: Tau(:)
      real, pointer :: Re(:)
      real, pointer :: Satzen(:)
      real, pointer :: Solzen(:)
      real, pointer :: Relazi(:)

   
   end type LUT_Grid_t



   type SAD_LUT_t
      integer       :: Index  ! Reference index
      character(80) :: Name   ! Optional class name
      real, pointer :: Wavelength(:) ! Channel wavelengths 

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
