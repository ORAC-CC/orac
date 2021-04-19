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

   public :: LUT_Dimension_t, &
             LUT_Grid_t, &
             SAD_LUT_t, &
             Alloc_SAD_LUT, &
             Dealloc_SAD_LUT, &
             Read_SAD_LUT, &
             locate

   type LUT_Dimension_t
      integer :: NMax
      real    :: Max  ! Maximum value on grid
      real    :: Min  ! Minimum value on grid
      real    :: d    ! Grid spacing (0 for irregular)
      integer :: n    ! Grid length

      real, pointer :: x(:)
   end type LUT_Dimension_t

   type LUT_Grid_t
      type(LUT_Dimension_t) :: Tau    ! Optical depth
      type(LUT_Dimension_t) :: Re     ! Particle size
      type(LUT_Dimension_t) :: SatZen ! Satellite angle
      type(LUT_Dimension_t) :: SolZen ! Solar angle
      type(LUT_Dimension_t) :: RelAzi ! Relative azimuth
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
#include "locate.F90"

end module SAD_LUT_m
