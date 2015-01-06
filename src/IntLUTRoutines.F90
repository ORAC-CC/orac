!-------------------------------------------------------------------------------
! Name:
!    Int_LUT_Routines_def
!
! Purpose:
!    Module with ECP LUT Interpolation routines
!
! Description:
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
!    N/A
!
! History:
!    10th Nov 2000, Andy Smith : Original version
!    16th Nov 2000, Andy Smith :
!       Adding more Int routines: Int_LUT_TauReSun.
!     1st Dec 2000, Andy Smith :
!       Replaced "sun" in routine names with Sol
!    11th Jan 2001, Andy Smith :
!       Chans argument removed from all routines. Redundant since interpolation
!       is done over the entire passed array.
!     5th Sep 2011, Chris Arnold:
!       Added interfaces for spline/locate routines and updated interfaces for
!       Int_LUT routines
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to interpolation routines IntLUT*.f90
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    16th Jan 2014, Greg McGarragh:
!       Added i_chan_to_ctrl_offset and i_chan_to_spixel_offset to subroutine
!       Int_LUT_TauSatRe.
!    20th Dec 2014, Greg McGarragh:
!       Cleaned up code.
!    24th Dec 2014, Greg McGarragh:
!       Some intent changes.
!    23th May 2014, Greg McGarragh:
!       No need for handmade explicit interfaces. Just need to include the
!       subroutines in the module and the interfaces are automatically generated.
!     9th Sep 2014, Greg McGarragh:
!       Added IntLUTTauSatReOnSol.F90 for new BRDF support.
!    16th Oct 2014, Greg McGarragh:
!       Moved a large amount of code that was common to all IntLUT* subroutines
!       into Int_LUT_Common()
!
! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------

module Int_LUT_Routines_def

   implicit none

   private

   public :: Interp3dLUT, &
             Int_LUT_TauRe, &
             Int_LUT_TauSatRe, &
             Int_LUT_TauSatReOnSol, &
             Int_LUT_TauSatSolAziRe, &
             Int_LUT_TauSolRe

contains

include 'Interp3dLUT.F90'

include 'IntLUTTauRe.F90'
include 'IntLUTTauSatRe.F90'
include 'IntLUTTauSatReOnSol.F90'
include 'IntLUTTauSatSolAziRe.F90'
include 'IntLUTTauSolRe.F90'


!-------------------------------------------------------------------------------
! Name:
!    Int_LUT_Common
!
! Purpose:
!
! Arguments:
!    Name Type In/Out/Both Description
!
! Algorithm:
!
! History:
!    16th Oct 2014, Greg McGarragh:
!       Removed the multiple instances of this code in the IntLUT* routines and
!       created this one shared subroutine.
!
! $Id$
!
! Bugs:
!    None known.
!
!-------------------------------------------------------------------------------

subroutine Int_LUT_Common(Ctrl, NChans, iCRP, Grid, GZero, G, FInt, FGrads, &
   i_chan_to_ctrl_offset, i_chan_to_spixel_offset, status)

   use CTRL_def
   use GZero_def
   use Int_Routines_def
   use SAD_LUT_def

   implicit none

   type(CTRL_t),                      intent(in)  :: Ctrl
   integer,                           intent(in)  :: NChans
   integer,                           intent(in)  :: iCRP
   type(LUT_Grid_t),                  intent(in)  :: Grid
                                      ! LUT grid data
   type(GZero_t),                     intent(in)  :: GZero
                                      ! Struct containing "zero'th" grid points
   real, dimension(NChans,-1:2,-1:2), intent(in)  :: G
                                      ! A Matrix of dimension NTau,Nre used to
                                      ! store array only interpolated to current
                                      ! viewing geometry
   real, dimension(:),                intent(out) :: FInt
                                      ! Interpolated value of F at the required
                                      ! Tau, SatZen, Re values (1 value per
                                      ! channel).
   real, dimension(:,:),              intent(out) :: FGrads
                                      ! Gradients of F wrt Tau and Re at
                                      ! required Tau, SatZen, Re values (1 value
                                      ! per channel).
   integer,                           intent(in)  :: i_chan_to_ctrl_offset
   integer,                           intent(in)  :: i_chan_to_spixel_offset
   integer,                           intent(out) :: status

   integer            :: i, ii, ii2
   integer, parameter :: iXm1 = -1
   integer, parameter :: iX0  =  0
   integer, parameter :: iX1  =  1
   integer, parameter :: iXp1 =  2
   real, dimension(4) :: Y          ! A vector to contain the values of F at
                                    ! (iT0,iR0), (iT0,iR1), (iT1,iR1) and
                                    ! (iT1,iR0) respectively (i.e. anticlockwise
                                    ! from the bottom left)
   real, dimension(4) :: dYdTau     ! Gradients of F wrt Tau at the same points
                                    ! as Y
   real, dimension(4) :: dYdRe      ! Gradients of F wrt Re  at the same points
                                    ! as Y
   real, dimension(4) :: ddY        ! 2nd order cross derivatives of ! F wrt Tau
                                    ! and Re at the same points
   real               :: a1, a2, a3 ! Temporary store for output of BiCubic
                                    ! subroutine

   status = 0

   ! Calculte the function derivatives at four LUT points around our X
   do i = 1, NChans
      ii = i_chan_to_ctrl_offset + i
      ii2 = i_chan_to_spixel_offset + i

      Y(1) = G(i,iX0,iX0)
      Y(4) = G(i,iX0,iX1)
      Y(3) = G(i,iX1,iX1)
      Y(2) = G(i,iX1,iX0)

      ! Now call linint or the adapted Numerical Recipes BCuInt subroutine to
      ! perform the interpolation to our desired state vector
      if (Ctrl%LUTIntflag .eq. LUTIntMethLinear) then
         call linint(Y,Grid%Tau(ii,GZero%iT0(ii2,iCRP),iCRP), &
                         Grid%Tau(ii,GZero%iT1(ii2,iCRP),iCRP), &
                         Grid%Re (ii,GZero%iR0(ii2,iCRP),iCRP), &
                         Grid%Re (ii,GZero%iR1(ii2,iCRP),iCRP), &
                         GZero%dT(ii2,iCRP),GZero%dR(ii2,iCRP),a1,a2,a3)
      else if (Ctrl%LUTIntflag .eq. LUTIntMethBicubic) then
         ! WRT to Tau
         dYdTau(1) = (G(i,iX1,iX0) - G(i,iXm1,iX0)) / &
                     (Grid%Tau(ii,GZero%iT1 (ii2,iCRP),iCRP) - &
                      Grid%Tau(ii,GZero%iTm1(ii2,iCRP),iCRP))
         dYdTau(2) = (G(i,iXp1,iX0) - G(i,iX0,iX0)) / &
                     (Grid%Tau(ii,GZero%iTp1(ii2,iCRP),iCRP) - &
                      Grid%Tau(ii,GZero%iT0 (ii2,iCRP),iCRP))
         dYdTau(3) = (G(i,iXp1,iX1) - G(i,iX0,iX1)) / &
                     (Grid%Tau(ii,GZero%iTp1(ii2,iCRP),iCRP) - &
                      Grid%Tau(ii,GZero%iT0 (ii2,iCRP),iCRP))
         dYdTau(4) = (G(i,iX1,iX1) - G(i,iXm1,iX1)) / &
                     (Grid%Tau(ii,GZero%iT1 (ii2,iCRP),iCRP) - &
                      Grid%Tau(ii,GZero%iTm1(ii2,iCRP),iCRP))

         ! WRT to Re
         dYDRe(1)  = (G(i,iX0,iX1) - G(i,iX0,iXm1)) / &
                     (Grid%Re (ii,GZero%iR1 (ii2,iCRP),iCRP) - &
                      Grid%Re (ii,GZero%iRm1(ii2,iCRP),iCRP))
         dYDRe(2)  = (G(i,iX1,iX1) - G(i,iX1,iXm1)) / &
                     (Grid%Re (ii,GZero%iR1 (ii2,iCRP),iCRP) - &
                      Grid%Re (ii,GZero%iRm1(ii2,iCRP),iCRP))
         dYDRe(3)  = (G(i,iX1,iXp1) - G(i,iX1,iX0)) / &
                     (Grid%Re (ii,GZero%iRp1(ii2,iCRP),iCRP) - &
                      Grid%Re (ii,GZero%iR0 (ii2,iCRP),iCRP))
         dYDRe(4)  = (G(i,iX0,iXp1) - G(i,iX0,iX0)) / &
                     (Grid%Re (ii,GZero%iRp1(ii2,iCRP),iCRP) - &
                      Grid%Re (ii,GZero%iR0 (ii2,iCRP),iCRP))

         ! Cross derivatives (dY^2/dTaudRe)
         ddY(1) = (G(i,iX1,iX1) - G(i,iX1,iXm1) - &
                   G(i,iXm1,iX1) + G(i,iXm1,iXm1)) / &
                  ((Grid%Tau(ii,GZero%iT1 (ii2,iCRP),iCRP) - &
                    Grid%Tau(ii,GZero%iTm1(ii2,iCRP),iCRP)) * &
                   (Grid%Re (ii,GZero%iR1 (ii2,iCRP),iCRP) - &
                    Grid%Re (ii,GZero%iRm1(ii2,iCRP),iCRP)))
         ddY(2) = (G(i,iXp1,iX1) - G(i,iXp1,iXm1) - &
                   G(i,iX0,iX1) + G(i,iX0,iXm1)) / &
                  ((Grid%Tau(ii,GZero%iTp1(ii2,iCRP),iCRP) - &
                    Grid%Tau(ii,GZero%iT0 (ii2,iCRP),iCRP)) * &
                   (Grid%Re (ii,GZero%iR1 (ii2,iCRP),iCRP) - &
                    Grid%Re (ii,GZero%iRm1(ii2,iCRP),iCRP)))
         ddY(3) = (G(i,iXp1,iXp1) - G(i,iXp1,iX0) - &
                   G(i,iX0,iXp1) + G(i,iX0,iX0)) / &
                  ((Grid%Tau(ii,GZero%iTp1(ii2,iCRP),iCRP) - &
                    Grid%Tau(ii,GZero%iT0 (ii2,iCRP),iCRP)) * &
                   (Grid%Re (ii,GZero%iRp1(ii2,iCRP),iCRP) - &
                    Grid%Re (ii,GZero%iR0 (ii2,iCRP),iCRP)))
         ddY(4) = (G(i,iX1,iXp1) - G(i,iX1,iX0) - &
                   G(i,iXm1,iXp1) + G(i,iXm1,iX0)) / &
                  ((Grid%Tau(ii,GZero%iT1 (ii2,iCRP),iCRP) - &
                    Grid%Tau(ii,GZero%iTm1(ii2,iCRP),iCRP)) * &
                   (Grid%Re (ii,GZero%iRp1(ii2,iCRP),iCRP) - &
                    Grid%Re (ii,GZero%iR0 (ii2,iCRP),iCRP)))

         call bcuint(Y,dYdTau,dYdRe,ddY, &
                     Grid%Tau(ii,GZero%iT0(ii2,iCRP),iCRP), &
                     Grid%Tau(ii,GZero%iT1(ii2,iCRP),iCRP), &
                     Grid%Re(ii,GZero%iR0(ii2,iCRP),iCRP), &
                     Grid%Re(ii,GZero%iR1(ii2,iCRP),iCRP), &
                     GZero%dT(ii2,iCRP),GZero%dR(ii2,iCRP),a1,a2,a3)
      else
         write(*,*) 'ERROR: Int_LUT_Common(): Invalid value for Ctrl%LUTIntflag: ', &
                    Ctrl%LUTIntflag
         status = LUTIntflagErr
         return
      end if

      FInt(i) = a1
      FGrads(i,1) = a2
      FGrads(i,2) = a3
   end do

end subroutine Int_LUT_Common

end module Int_LUT_Routines_def
