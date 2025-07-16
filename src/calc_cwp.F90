!-------------------------------------------------------------------------------
! Name: calc_cwp.F90
!
! Purpose:
! This routine calculates the cloud water path and the associated uncertainty on
! the cloud water path.
!
! Description and Algorithm details:
! 1) Evaluate 4/3 * COT * CER * density / extinction_coefficient.
! 2) Propagate COT and CER uncertainty through #1 to get CWP uncertainty.
!
! Arguments:
! Name       Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl       struct  In   Control structure
! SPixel     struct  Both Structure for the pixel currently being retrieved.
!
! History:
! 2011/11/08, CP: Original version adapted from idl version
! 2011/11/28, CP: Remove log write statement
! 2013/11/14, MJ: Makes branch for ICE explicit
! 2013/11/14, GM: Some code cleanup
! 2014/06/11, CP: Removes automatic crash if ice or wat class id not specified
!    so it can cope with other classes.
! 2014/07/23, AP: Added value for al10e2.
! 2014/12/19, AP: Renaming CloudClass field in Ctrl.
! 2016/02/26, GM: Correct computation of CWP uncertainty which was missing a
!    fac**2 factor.  Then reformulated the computation in the form of standard
!    propagation of COT and CER uncertainty through the CWP computation.
! 2016/07/27, GM: Changes for the multilayer retrieval.
! 2025/07/08, DR: COT is actually in linear space when using the new LUTs at this 
!                 point, so if we try to convert it back to linear space through 
!                 10**Tau, we end up with massive values of CWP. Changed to be in 
!                 linear space and corrected the associated uncertainty. Not tested
!                 with old LUTs, but all processing should now be done with new LUTs.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Calc_CWP(Ctrl, SPixel)

   use Ctrl_m
   use ORAC_Constants_m
   use SPixel_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel

   ! Local variable declarations

   call Calc_CWP2(Ctrl, SPixel, Ctrl%Class, ITau, IRe, &
                  SPixel%CWP, SPixel%CWP_uncertainty)

   if (Ctrl%Approach == AppCld2L) then
      call Calc_CWP2(Ctrl, SPixel, Ctrl%Class, ITau2, IRe2, &
                     SPixel%CWP2, SPixel%CWP2_uncertainty)
   end if

end subroutine Calc_CWP


subroutine Calc_CWP2(Ctrl, SPixel, Class, ITauX, IReX, CWP, CWP_uncertainty)

   use Ctrl_m
   use ORAC_Constants_m
   use SPixel_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)  :: Ctrl
   type(SPixel_t), intent(in)  :: SPixel
   integer,        intent(in)  :: Class
   integer,        intent(in)  :: ITauX
   integer,        intent(in)  :: IReX
   real,           intent(out) :: CWP
   real,           intent(out) :: CWP_uncertainty

   ! Local variable declarations

   real :: fac
   real :: dcwp_dtau
   real :: dcwp_dr_e

   if (Class == ClsCldWat) then
      fac = 4./3. * rhowat / qextwat
   else if (Class == ClsCldIce) then
      fac = 4./3. * rhoice / qextice
   else
      CWP             = sreal_fill_value
      CWP_uncertainty = sreal_fill_value
      return
   end if

   CWP = fac * SPixel%Xn(ITauX) * SPixel%Xn(IReX)

   dcwp_dtau = fac * SPixel%Xn(IReX)
   dcwp_dr_e = fac * SPixel%Xn(ITauX)

   CWP_uncertainty = dcwp_dtau * dcwp_dtau * SPixel%Sn(ITauX,ITauX) + &
                     dcwp_dtau * dcwp_dr_e * SPixel%Sn(IReX,ITauX) + &
                     dcwp_dr_e * dcwp_dtau * SPixel%Sn(ITauX,IReX) + &
                     dcwp_dr_e * dcwp_dr_e * SPixel%Sn(IReX,IReX)

end subroutine Calc_CWP2
