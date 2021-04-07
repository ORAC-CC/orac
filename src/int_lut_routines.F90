!-------------------------------------------------------------------------------
! Name: int_lut_routines.F90
!
! Purpose:
! Module with ORAC LUT Interpolation routines
!
! History:
! 2000/11/10, AS: Original version
! 2000/11/16, AS: Adding more Int routines: Int_LUT_TauReSun.
! 2000/12/01, AS: Replaced "sun" in routine names with Sol
! 2001/01/11, AS: Chans argument removed from all routines. Redundant since
!    interpolation is done over the entire passed array.
! 2011/09/05, CA: Added interfaces for spline/locate routines and updated
!    interfaces for Int_LUT routines
! 2012/02/07, CA: Ctrl struct now passed to interpolation routines IntLUT*.f90
! 2013/12/03, MJ: Makes LUTs more flexible wrt channel and properties.
! 2014/01/16, GM: Added i_chan_to_ctrl_offset and i_chan_to_spixel_offset to
!    subroutine Int_LUT_TauSatRe.
! 2014/12/20, GM: Cleaned up code.
! 2014/12/24, GM: Some intent changes.
! 2014/05/23, GM: No need for handmade explicit interfaces. Just need to
!    include the subroutines in the module and the interfaces are automatically
!    generated.
! 2014/09/09, GM: Added IntLUTTauSatReOnSol.F90 for new BRDF support.
! 2014/10/16, GM: Moved a large amount of code that was common to all IntLUT*
!    subroutines into Int_LUT_Common()
! 2016/07/27, GM: Add ITauCRP and IReCRP indices since with the multilayer
!    retrieval ITau and IRe for a particular layer may not be 1 and 2.
! 2017/10/24, GM: Switch to official NR bilinear interpolation code and make
!    optional through conditional compilation.
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Int_LUT_Routines_m

   implicit none

   private

   public :: ITauCRP, IReCRP, &
             MaxCRPParams, &
             Interp3dLUT, &
             Int_LUT_Re, &
             Int_LUT_TauRe, &
             Int_LUT_TauSatRe, &
             Int_LUT_TauSatReOnSol, &
             Int_LUT_TauSatSolAziRe, &
             Int_LUT_TauSolRe

   integer, parameter :: iXm1 = -1
   integer, parameter :: iX0  =  0
   integer, parameter :: iX1  =  1
   integer, parameter :: iXp1 =  2

   integer, parameter :: ITauCRP = 1
   integer, parameter :: IReCRP  = 2

   integer, parameter :: MaxCRPParams  = 2

contains

#include "interp3dlut.F90"

#include "int_lut_re.F90"
#include "int_lut_taure.F90"
#include "int_lut_tausatre.F90"
#include "int_lut_tausatreonsol.F90"
#include "int_lut_tausatsolazire.F90"
#include "int_lut_tausolre.F90"


!-------------------------------------------------------------------------------
! Name: Int_LUT_Common
!
! Purpose:
! Perform gradient calculations common to all IntLUT* routines.
!
! Description and Algorithm details:
!
! Arguments:
!    Name Type In/Out/Both Description
!
! History:
! 2014/10/16, GM: Removed the multiple instances of this code in the IntLUT*
!    routines and created this one shared subroutine.
! 2015/01/13, AP: Switch to array-based channel indexing rather than using
!    offsets.
! 2017/01/17, GM: Changes related to simplification of the indexing of the LUT
!    grid and the GZero parameters.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Int_LUT_Common(Ctrl, NChans, Grid, GZero, G, FInt, FGrads, &
   chan_to_ctrl_index, chan_to_spixel_index, status)

   use Ctrl_m
   use GZero_m
   use Int_Routines_m
   use SAD_LUT_m

   implicit none

   type(Ctrl_t),                      intent(in)  :: Ctrl
   integer,                           intent(in)  :: NChans
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
   integer,                           intent(in)  :: chan_to_ctrl_index(:)
                                      ! Indices for input channels wrt Ctrl
   integer,                           intent(in)  :: chan_to_spixel_index(:)
                                      ! Indices for input channels wrt SPixel
   integer,                           intent(out) :: status

   integer            :: i, ii2
   real               :: a, b
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
      ii2 = chan_to_spixel_index(i)

      Y(1) = G(i,iX0,iX0)
      Y(4) = G(i,iX0,iX1)
      Y(3) = G(i,iX1,iX1)
      Y(2) = G(i,iX1,iX0)

      ! Now call linint or the adapted Numerical Recipes BCuInt subroutine to
      ! perform the interpolation to our desired state vector
      if (Ctrl%LUTIntSelm .eq. LUTIntMethLinear) then
         call linint(Y, Grid%Tau%x(GZero%iT0(ii2)), Grid%Tau%x(GZero%iT1(ii2)), &
                        Grid%Re%x (GZero%iR0(ii2)), Grid%Re%x (GZero%iR1(ii2)), &
                        GZero%dT(ii2), GZero%dR(ii2), a1, a2, a3)
      else if (Ctrl%LUTIntSelm .eq. LUTIntMethBicubic) then
         ! WRT to Tau
         dYdTau(1) = (G(i,iX1,iX0) - G(i,iXm1,iX0)) / &
                     (Grid%Tau%x(GZero%iT1 (ii2)) - Grid%Tau%x(GZero%iTm1(ii2)))
         dYdTau(2) = (G(i,iXp1,iX0) - G(i,iX0,iX0)) / &
                     (Grid%Tau%x(GZero%iTp1(ii2)) - Grid%Tau%x(GZero%iT0 (ii2)))
         dYdTau(3) = (G(i,iXp1,iX1) - G(i,iX0,iX1)) / &
                     (Grid%Tau%x(GZero%iTp1(ii2)) - Grid%Tau%x(GZero%iT0 (ii2)))
         dYdTau(4) = (G(i,iX1,iX1) - G(i,iXm1,iX1)) / &
                     (Grid%Tau%x(GZero%iT1 (ii2)) - Grid%Tau%x(GZero%iTm1(ii2)))

         ! WRT to Re
         dYDRe(1)  = (G(i,iX0,iX1) - G(i,iX0,iXm1)) / &
                     (Grid%Re%x (GZero%iR1 (ii2)) - Grid%Re%x (GZero%iRm1(ii2)))
         dYDRe(2)  = (G(i,iX1,iX1) - G(i,iX1,iXm1)) / &
                     (Grid%Re%x (GZero%iR1 (ii2)) - Grid%Re%x (GZero%iRm1(ii2)))
         dYDRe(3)  = (G(i,iX1,iXp1) - G(i,iX1,iX0)) / &
                     (Grid%Re%x (GZero%iRp1(ii2)) - Grid%Re%x (GZero%iR0 (ii2)))
         dYDRe(4)  = (G(i,iX0,iXp1) - G(i,iX0,iX0)) / &
                     (Grid%Re%x (GZero%iRp1(ii2)) - Grid%Re%x (GZero%iR0 (ii2)))

         ! Cross derivatives (dY^2/dTaudRe)
         ddY(1) = (G(i,iX1,iX1) - G(i,iX1,iXm1) - &
                   G(i,iXm1,iX1) + G(i,iXm1,iXm1)) / &
                  ((Grid%Tau%x(GZero%iT1 (ii2)) - Grid%Tau%x(GZero%iTm1(ii2))) * &
                   (Grid%Re%x (GZero%iR1 (ii2)) - Grid%Re%x (GZero%iRm1(ii2))))
         ddY(2) = (G(i,iXp1,iX1) - G(i,iXp1,iXm1) - &
                   G(i,iX0,iX1) + G(i,iX0,iXm1)) / &
                  ((Grid%Tau%x(GZero%iTp1(ii2)) - Grid%Tau%x(GZero%iT0 (ii2))) * &
                   (Grid%Re%x (GZero%iR1 (ii2)) - Grid%Re%x (GZero%iRm1(ii2))))
         ddY(3) = (G(i,iXp1,iXp1) - G(i,iXp1,iX0) - &
                   G(i,iX0,iXp1) + G(i,iX0,iX0)) / &
                  ((Grid%Tau%x(GZero%iTp1(ii2)) - Grid%Tau%x(GZero%iT0 (ii2))) * &
                   (Grid%Re%x (GZero%iRp1(ii2)) - Grid%Re%x (GZero%iR0 (ii2))))
         ddY(4) = (G(i,iX1,iXp1) - G(i,iX1,iX0) - &
                   G(i,iXm1,iXp1) + G(i,iXm1,iX0)) / &
                  ((Grid%Tau%x(GZero%iT1 (ii2)) - Grid%Tau%x(GZero%iTm1(ii2))) * &
                   (Grid%Re%x (GZero%iRp1(ii2)) - Grid%Re%x (GZero%iR0 (ii2))))
#ifdef INCLUDE_NR
         ! A hack to deal with the fact that upstream NR wants the point and not
         ! the gradient at that point.

         a = GZero%dT(ii2) * (Grid%Tau%x(GZero%iT1(ii2)) - Grid%Tau%x(GZero%iT0(ii2))) + &
             Grid%Tau%x(GZero%iT0(ii2))
         b = GZero%dR(ii2) * (Grid%Re%x (GZero%iR1(ii2)) - Grid%Re%x (GZero%iR0(ii2))) + &
             Grid%Re%x (GZero%iR0(ii2))

         call bcuint(Y, dYdTau, dYdRe, ddY, &
                     Grid%Tau%x(GZero%iT0(ii2)), Grid%Tau%x(GZero%iT1(ii2)), &
                     Grid%Re%x (GZero%iR0(ii2)), Grid%Re%x (GZero%iR1(ii2)), &
                     a, b, a1, a2, a3)
#else
      write(*, *) 'ERROR: Int_LUT_Common(): Numerical Recipes is ' // &
         'not available for bilinear interpolation'
      stop error_stop_code
#endif
      else
         write(*,*) 'ERROR: Int_LUT_Common(): Invalid value for Ctrl%LUTIntSelm: ', &
                    Ctrl%LUTIntSelm
         status = LUTIntflagErr
         return
      end if

      FInt(i) = a1
      FGrads(i,1) = a2
      FGrads(i,2) = a3
   end do

end subroutine Int_LUT_Common

end module Int_LUT_Routines_m
