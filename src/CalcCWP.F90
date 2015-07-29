!-------------------------------------------------------------------------------
! Name: CalcCWP.F90
!
! Purpose:
! This routine calculates the Cloud water path and the associated error on
! the Cloud waterPath
!
! Description and Algorithm details:
! 1) Evaluate (4/3) * COT * Ref * density / extinction_coefficient.
! 2) Evalute derivatives of that for Jacobian.
!
! Arguments:
! Name       Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl       struct  In   ECP control structure read from driver file
! SPixel     struct  Both Structure for the pixel currently being retrieved.
!
! History:
! 2011/11/08, CP: original version adapted from idl version
! 2011/11/28, CP: remove log write statement
! 2013/11/14, MJ: makes branch for ICE explicit
! 2013/11/14, GM: Some code cleanup
! 2014/06/11, CP: removes automatic crash if ice wat class not specified so
!    can cope with aerosol class
! 2014/07/23, AP: added value for al10e2.
! 2014/12/19, AP: Renaming CloudClass field in Ctrl.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Calc_CWP(Ctrl, SPixel)

   ! Modules used by this program.
   use CTRL_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Argument declarations
   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel

   ! Local variable declarations
   real :: rho             ! liquid or water density
   real :: fac             ! CWP factor
   real :: s_cot_cre       ! co-variance cot, effective radius (from log10 cot,
                           ! effective radius)
   real :: s_cot           ! variance in cot from variance in log10cot
   real :: tenpcot         ! 10^cot convert from log value to linear value
   real :: al10e = .434294 ! i.e  log10(exp(1.))
   real :: al10e2 =.188612 ! =al10e*al10e


   if (trim(Ctrl%CloudClass) == 'WAT') then
      rho=rhowat
      fac=(4./3.)*rho/qextwat
   else if (trim(Ctrl%CloudClass) == 'ICE') then
      rho=rhoice
      fac=(4./3.)*rho/qextice
   else
      ! most likely aerosol but could be some other crash
      SPixel%cwp=sreal_fill_value
      SPixel%cwp_error=sreal_fill_value
      return
   end if

   ! Do not calculate for aerosol class
   if (trim(Ctrl%CloudClass) == 'WAT' .or. trim(Ctrl%CloudClass) == 'ICE') then

      tenpcot=10.**(SPixel%Xn(iTau))

      SPixel%cwp=fac* tenpcot*SPixel%Xn(iRe)

      ! covariance
      s_cot_cre=(SPixel%Sn(iTau,iRe)*tenpcot)/al10e

      ! error on optical depth
      s_cot=(SPixel%Sn(iTau,iTau)*tenpcot*tenpcot)/al10e2

      ! based on
      ! SPixel%cwp_error=fac*sqrt(cre*cre*s_cot+cot*cot*s_cre+2.*cre*cot*s_cot_cre)

      SPixel%cwp_error=SPixel%Xn(iRe)*SPixel%Xn(iRe)*s_cot+ &
                       tenpcot*tenpcot*SPixel%Sn(iRe,iRe)+ &
                       2.* tenpcot*SPixel%Xn(iRe)*s_cot_cre
 end if

end subroutine Calc_CWP
