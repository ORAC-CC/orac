

! CVS:  $Id: continuum.F90,v 1.1 2007/05/30 15:52:47 norm Exp $
! CVS:  $Name:  $ 
module continuum

use kinds
implicit none

integer (kind=int_kind), parameter :: &
  ncoef = 7 &
  , nreg = 2 &
  , nband = 12

real (kind=dbl_kind), dimension(nband) :: &
  h2obnd

real (kind=dbl_kind), dimension(ncoef,nreg,nband):: &
  ck24_3   !weighted by Planck function

data h2obnd /-5,-3.5,-2.0,-2,-1.,-4,-4,-4,-3,-3.5,-3,-2/

data ck24_3/  &    !ckd24fu.fuliou.lin.plnk.out
! band        1
     1.667e+00, 9.421e-01,-7.358e-03, 1.355e+00, &
     2.557e+03, 5.798e+01,-4.570e-01,            &
! band        1
     6.417e+00, 1.002e+00,-6.991e-03, 1.010e+00, &
     1.203e+01, 4.501e-02,-2.428e-02,            &
! band        2
     2.390e+00, 9.528e-01,-6.058e-03, 1.071e+00, &
     2.676e+02, 9.848e+00,-1.459e-01,            &
! band        2
     4.849e+00, 1.002e+00,-6.910e-03, 8.961e-01, &
     1.635e+01, 2.115e-02, 7.243e-02,            &
! band        3
     2.326e+00, 9.720e-01,-6.551e-03, 8.739e-01, &
     6.984e+01, 8.346e-01, 4.824e-02,            &
! band        3
     5.002e+00, 1.005e+00,-9.286e-03, 6.222e-01, &
     1.168e+01, 3.611e-03, 3.148e-01,            &
! band        4
     -4.865e+00, 8.455e-01,-6.911e-03, 1.475e+00,&
     2.905e+02, 7.078e+00,-6.846e-01,            &
! band        4
     4.596e+00, 1.012e+00,-1.152e-02, 5.713e-01, &
     1.270e+01,-1.395e-03, 3.447e-01,            &
! band        5
     -5.396e+00, 8.596e-01,-8.479e-03, 1.619e+00,&
     1.664e+02, 3.236e+00,-7.782e-01,            &
! band        5
     7.478e+00, 1.007e+00,-1.963e-02, 2.771e-01, &
     6.021e+00,-4.489e-03, 6.709e-01,            &
! band        6
     1.262e+00, 2.347e-01,-2.360e-02, 1.655e-01, &
     5.068e+02, 2.462e+01, 3.920e-01,            &
! band        6
     9.334e+00, 1.002e+00,-2.429e-02, 3.575e-02, &
     2.751e-01,-1.189e-03, 9.593e-01,            &
! band        7
     -1.222e+00, 5.423e-01,-2.327e-02, 5.197e-01,&
     6.423e+02, 5.038e+01, 1.502e-01,            &
! band        7
     8.506e+00, 1.000e+00,-2.339e-02, 8.891e-03, &
     -6.805e-01,-1.639e-04, 9.917e-01,           &
! band        8
     -3.638e+00, 8.534e-01,-1.344e-02, 6.816e-01,&
     5.385e+02, 4.428e+01,-6.366e-03,            &
! band        8
     6.921e+00, 1.002e+00,-1.974e-02, 6.350e-02, &
     6.838e-01,-1.121e-03, 9.237e-01,            &
! band        9
     -2.329e+00, 7.893e-01,-2.588e-03, 1.017e+00,&
     1.525e+02, 1.029e+01,-1.486e-01,            &
! band        9
     6.742e-01, 1.008e+00,-3.376e-03, 9.105e-01, &
     1.074e+01,-3.307e-03, 5.741e-02,            &
! band       10
     -1.677e+00, 9.173e-01,-5.780e-03, 1.504e+00,&
     7.886e+02, 2.288e+01,-5.999e-01,            &
! band       10
     3.396e+00, 1.005e+00,-3.433e-03, 1.012e+00, &
     7.635e+00, 3.010e-03,-2.418e-02,            &
! band       11
     7.943e-01, 9.260e-01,-5.050e-03, 1.141e+00, &
     2.221e+02, 1.021e+01,-2.246e-01,            &
! band       11
     3.356e+00, 1.002e+00,-4.719e-03, 9.578e-01, &
     6.164e+00, 1.186e-03, 2.264e-02,            &
! band       12
     -5.874e+00, 7.060e-01,-1.532e-03, 1.141e+00,&
     1.463e+02, 6.534e+00,-4.308e-01,            &
! band       12
     4.709e-01, 1.010e+00,-6.067e-03, 8.513e-01, &
     1.161e+01,-6.629e-03, 8.885e-02             &
     /


contains

subroutine gascon                &
           (ncol, nlm, ib,  pp   &
           ,ppl,  dp, tt, rmix   &
           ,tgm )

   use kinds,             only:  int_kind, dbl_kind
   use bugsrad_physconst, only:  gravity, r_d, f_virt
   implicit none
      
!-----------------------------------------------------------------------
! MODIFICATIONS:
! * changed declarations to adapt the code from BUGS4 to BUGS5.
!   Laura D. Fowler/slikrock (02-01-00).

! NBW - Modified 27/11/2002
! H2O continuum now based on CKD2.4, from Fred Rose and Dave Kurtz

! REFERENCES:
! Parameterized CKD_2.1 continuum absorption.
! adapted from the Fu-Liou 4-stream radiative transfer model original
! code by Fred Rose. use radparams_0898. 
! Phil Partain/graben (04/04/00).

! send comments to partain@atmos.colostate.edu.

! SUBROUTINES CALLED:
!     none.

! FUNCTIONS CALLED:
!     none.

! INCLUDED COMMONS:
!     none.

! ARGUMENT LIST VARIABLES:
! INPUT ARGUMENTS:
! ----------------
   integer (kind=int_kind), intent(in):: &
      ncol & !Length of sub-domain.       
     ,nlm  & !Number of layers.
     ,ib     !Spectral interval.

   real (kind=dbl_kind), intent(in), dimension(:,:):: &
      ppl  &  !Pressure                                          (hPa).
     ,dp   &  !Pressure thickness                                (hPa).
     ,tt   &  !Temperature                                         (K).
     ,rmix &  !Water vapor mixing ratio                        (kg/kg).
     ,pp

! OUTPUT ARGUMENTS:
! -----------------
   real (kind=dbl_kind), intent(out), dimension(:,:):: &
      tgm   !Water vapor continuum optical depth                   (-).

! LOCAL VARIABLES:

   integer (kind=int_kind):: &
      i,l

   integer (kind=int_kind), dimension(18):: &
      iflb
   data iflb /6*0,12,11,10,9,8,7,6,5,4,3,2,1/

   real (kind=dbl_kind)::  &
      dz, amnt, patm, tv
 
   tgm(:,:) = 0. 
   if( iflb(ib) .eq. 0) return
   do i = 1, ncol
      do l = 1, nlm
         if(rmix(i,l).gt.0.0) then
            ! The factor of 10 converts hPa to Pa and kg/m^2 to g/cm^2
            amnt     = 10._dbl_kind*dp(i,l)*rmix(i,l)/gravity
            patm     = ppl(i,l) /1013.25_dbl_kind
            tv       = tt(i,l)*(1._dbl_kind + f_virt*rmix(i,l))
            dz       = (r_d/gravity)*tv*log(pp(i,l+1)/pp(i,l))* &
                          0.001_dbl_kind
            tgm(i,l) = parm_ckd24(iflb(ib),amnt,patm,tt(i,l),dz)
         endif
      enddo
   enddo
 
   return
end subroutine gascon

function parm_ckd24(iband,amnt,patm,temp,dz) result(ckd24_tau)
   use kinds
   use bugsrad_physconst, only:  r_star,mw_h2o
   implicit none
! Parameterization of CKD_2.4 continuum over Fu-Liou Bands
! Input:
! iband  =  integer (1-12) where
!         Band 1 ='  5:280cm-1'
!         Band 2 ='280:400cm-1'
!         Band 3 ='400:540cm-1'
!         Band 4 ='540:670cm-1'
!         Band 5 ='670:800cm-1'
!         Band 6 ='800:980cm-1'
!         Band 7 ='980:1100cm-1'
!         Band 8 ='1100:1250cm-1'
!         Band 9 ='1250:1400cm-1'
!         Band10 ='1400:1700cm-1'
!         Band11 ='1700:1900cm-1'
!         Band12 ='1900:2200cm-1'
! amnt = h2O ammount (g/cm**2)
! patm = pressure (atm)
! temp = temperature (k)
! dz   = pathlength (Km)
! Output:
! parm_ckd24 = parameterized CKD_2.4optical depth for band
!234567890123456789012345678901234567890123456789012345678901234567890



! These Regressions are more sensitive to pathlength
! So accomodations for very Thin or Thick layers are made.

   integer (kind=int_kind), intent(in):: &
      iband

   real (kind=dbl_kind), intent(in):: &
      amnt  &   !Water vapor content                         (g/cm^2).
     ,patm  &   !Pressure                                       (atm).
     ,temp  &   !Temperature                                      (K).
     ,dz        !Path length                                     (km).

! OUTPUT ARGUMENTS:
   real (kind=dbl_kind):: &
      ckd24_tau !Optical depth to water vapor continuum        (-).

! LOCAL VARIABLES:
   integer(kind=int_kind) :: &
      ireg

   real (kind=dbl_kind):: &
      factor, dz1, amnt1, patmx, ph2o, tau_log

   dz1  =dz
   factor=1.000
   if ( dz < 0.25  ) then
      factor = 0.25/dz
      dz1   = 0.25
   elseif (dz > 1.50) then
      factor = 1.50/dz
      dz1    = 1.50
   endif
   amnt1=amnt*factor

! Regression is now broken up into TWO parts one for small
! one for large water vapor ammounts.

   ireg=1
   if (log(amnt1) > h2obnd(iband)) ireg=2

   ph2o = amnt1 *(r_star*1.e4_dbl_kind*temp )/  &
               (dz1*1.0d+05*mw_h2o *1.01325d+06)

   patmx = log(patm)
   tau_log = ck24_3(1,ireg,iband)              +    &
             ck24_3(2,ireg,iband)* log(amnt1)  +    &
             ck24_3(3,ireg,iband)* temp        +    &
             ck24_3(4,ireg,iband)* patmx       +    &
             ck24_3(5,ireg,iband)* (ph2o)      +    &
             ck24_3(6,ireg,iband)* amnt1       +    &
             ck24_3(7,ireg,iband)* log(ph2o)
   ckd24_tau = exp( tau_log )
   ckd24_tau = ckd24_tau/factor
   return
end function parm_ckd24


end module continuum
