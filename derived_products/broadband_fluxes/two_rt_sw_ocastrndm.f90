

! CVS: $Id: two_rt_sw_ocastrndm.F90,v 1.2 2006/11/16 21:11:23 norm Exp $
! CVS: $Name:  $

!NBW This has been modified to apply descaling to the
!NBW delta-M scaled downwelling direct and diffuse fluxes

subroutine two_rt_sw_gsolap                          &
   ( ncol      , nlm       , mbs       , ib        , &
     slr       , amu0      , wc_clr    , asym_clr  , &
     tau_clr   , wc_cld    , asym_cld  , tau_cld   , &
     asdir     , asdif     , fddir_clr , fddif_clr , &
     fudif_clr , fddir_all , fddif_all , fudif_all , &
     cldamt    , c_maximal , cf_max,  cf_random )

   use kinds
   implicit none

!------------------------------------------------------------------------------
! REFERENCES
! Alternate cloud overlap calculation
!
! SUBROUTINES CALLED:
!
! FUNCTIONS CALLED:
!
! INCLUDED COMMONS:
!
! ARGUMENT LIST
!  Inputs
   integer (kind=int_kind), intent(in)::                                     &
      ncol,       &    !Length of sub-domain
      nlm,        &    !Number of layers
      mbs,        &    !Number of SW spectral intervals
      ib               !Index of spectral interval

   real (kind=dbl_kind), intent(in), dimension(ncol)::                       &
      slr,        &    !Fraction of daylight
      amu0,       &    !Cosine of solar zenith angle
      c_maximal        !Maximally overlapped fraction

   real (kind=dbl_kind), intent(in), dimension(ncol,mbs)::                   &
      asdir,      &    !Spectral direct surface albedo
      asdif            !Spectral diffuse serface albedo

   real (kind=dbl_kind), intent(in), dimension(ncol,nlm)::                   &
      wc_clr,     &    !Clear-sky single scattering albedo
      wc_cld,     &    !Cloudy-sky single scattering albedo
      asym_clr,   &    !Clear-sky asymmetry parameter
      asym_cld,   &    !Cloudy-sky asymmetry parameter
      tau_clr,    &    !Clear-sky optical depth
      tau_cld,    &    !Cloudy-sky optical depth
      cldamt,     &    !Cloud fraction
      cf_max,     &    !Maximally overlapped cloud fractions
      cf_random        !Randomly overlapped cloud fractions

!  Outputs
   real(kind=dbl_kind), intent(out), dimension(ncol, nlm+1)::                &
      fddir_clr,  &    !Clear-sky spectral direct downward flux (W/m^2)
      fddir_all,  &    !All-sky spectral direct downward flux (W/m^2)
      fddif_clr,  &    !Clear-sky spectral diffuse downward flux (W/m^2)
      fddif_all,  &    !All-sky spectral diffuse downward flux (W/m^2)
      fudif_clr,  &    !Clear-sky spectral diffuse upward flux (W/m^2)
      fudif_all        !All-sky spectral diffuse upward flux (W/m^2)


! LOCAL VARIABLES
   integer (kind=int_kind) ::                                                &
      i,          &    !Horizontal index
      l                !Vertical index

   real (kind=dbl_kind), parameter ::                                        &
      eps = 1.0e-2

   real (kind=dbl_kind) ::                                                   &
      aa,         &    !
      bb,         &    !
      cc,         &    !
      denom,      &    !
      eggtau,     &    !
      g3,         &    !
      g4,         &    !
      ggtau,      &    !
      kappa,      &    !
      r,          &    !
      rinf,       &    !
      t,          &    !
      oms,        &    !
      taus,       &    !
      fact,       &    !
      asy,        &    !
      prop,       &    !
      fd_total         !

   real (kind=dbl_kind), dimension(nlm) ::                                   &
      exptau_s_clr, &    !
      exptau_s_cld, &    !
      exptau_us_clr,&    !
      exptau_us_cld,&
      rr_clr,     &    !
      rr_cld,     &    !
      rr_tmp,     &    !
      tr_clr,     &    !
      tr_cld,     &    !
      tr_tmp,     &    !
      sigu,       &    !
      sigd,       &    !
      sigu_clr_fract,& !
      sigu_cld_fract,& !
      sigd_clr_fract,& !
      sigd_cld_fract,& !
      sigu_tmp_fract,   &    !
      sigd_tmp_fract,   &    !
      td,     &    !
      vu           !

   real (kind=dbl_kind), dimension(nlm+1) ::                                 &
      direct_s,     &    !
      direct_us,    &
      re,         &    !
      vd               !

   real (kind=dbl_kind), dimension(nlm+1)::                            &
      fddir_max,  &    !Maximum-overlap fraction spectral direct downward flux (W/m^2)
      fddir_rndm,  &    !Random-overlap fraction spectral direct downward flux (W/m^2)
      fddif_max,  &    !Maximum-overlap fraction spectral diffuse downward flux (W/m^2)
      fddif_rndm,  &    !Random-overlap fraction spectral diffuse downward flux (W/m^2)
      fudif_max,  &    !Maximum-overlap fraction spectral diffuse upward flux (W/m^2)
      fudif_rndm        !Random-overlap fraction spectral diffuse upward flux (W/m^2)
      

      fddir_clr = 0.
      fddir_all = 0.
      fddif_clr = 0.
      fddif_all = 0.
      fudif_clr = 0.
      fudif_all = 0.

      fddir_max = 0.
      fddir_rndm = 0.
      fddif_max = 0.
      fddif_rndm = 0.
      fudif_max = 0.
      fudif_rndm = 0.

! Begin loop over all columns
   do i = 1, ncol

      !Clear layer properties 
      direct_s(1) = 1.
      direct_us(1) = 1.
      do l = 1, nlm
         !Delta-M on
         fact = asym_clr(i,l)*asym_clr(i,l)
         asy = asym_clr(i,l)/(1. + asym_clr(i,l))
         !Delta-M off
         !fact = 0.
         !asy = asym_clr(i,l)
         !End delta-M on/off
         oms = ((1. - fact)*wc_clr(i,l))/(1. - fact*wc_clr(i,l))
         taus = (1. - fact*wc_clr(i,l))*tau_clr(i,l)


         exptau_s_clr(l) = exp(-taus/amu0(i))
         direct_s(l+1) = exptau_s_clr(l)*direct_s(l)
         exptau_us_clr(l) = exp(-tau_clr(i,l))
         direct_us(l+1) = exptau_us_clr(l)*direct_us(l)
         ! Local Eddington coefficients
         t =  0.25*(7. - oms*(4. + 3.*asy))
         r = -0.25*(1. - oms*(4. - 3.*asy))
         kappa = sqrt(t*t - r*r)
         rinf = r/(kappa+t)
         ggtau = kappa*taus
         eggtau = exp(-ggtau)
         denom = (1. - rinf*rinf*eggtau*eggtau)
         tr_clr(l) = (1. - rinf*rinf)*eggtau/denom
         rr_clr(l) = rinf*(1. - eggtau*eggtau)/denom

         fact = kappa*kappa - 1./(amu0(i)*amu0(i))
         if (abs(fact) .lt. eps) then
            fact = 1./eps
         else
            fact = 1./fact
         endif

         cc = oms*slr(i)*fact
         g3 = 0.5 - 0.75*asy*amu0(i)
         g4 = 1. - g3
         aa = g3*(t - 1./amu0(i)) + g4*r
         bb = g4*(t + 1./amu0(i)) + g3*r
         sigu_clr_fract(l) = cc*((aa - rr_clr(l)*bb) - aa*tr_clr(l)*exptau_s_clr(l))
         sigd_clr_fract(l) = cc*(-bb*tr_clr(l) + (bb-rr_clr(l)*aa)*exptau_s_clr(l))
      enddo

      !CLEAR-SKY adding
      re(1) = 0.
      vd(1) = 0.
      do l = 1, nlm
         prop = 1./(1. - re(l)*rr_clr(l))
         re(l+1) = rr_clr(l) + tr_clr(l)*tr_clr(l)*re(l)*prop
         vd(l+1) = sigd_clr_fract(l)*direct_s(l) + (tr_clr(l)*vd(l) + tr_clr(l)*re(l)*sigu_clr_fract(l)*direct_s(l))*prop
         vu(l) = (rr_clr(l)*vd(l) + sigu_clr_fract(l)*direct_s(l))*prop
         td(l) = prop
      enddo

      !CLEAR-SKY diffuse flux calculation
      fddif_clr(i,1) = 0.
      fudif_clr(i,nlm+1) = (asdif(i,ib)*vd(nlm+1) +                         &
                            asdir(i,ib)*slr(i)*amu0(i)*direct_s(nlm+1))/      &
                           (1. - asdif(i,ib)*re(nlm+1))
      do l = nlm+1, 2, -1
         fddif_clr(i,l) = re(l)*fudif_clr(i,l) + vd(l)
         fudif_clr(i,l-1) = tr_clr(l-1)*fudif_clr(i,l)*td(l-1) + vu(l-1)
      enddo
 
      !CLEAR-SKY direct flux calculation, with delta-M descaling
      fddir_clr(i,1) = amu0(i)*slr(i)
      do l = 1, nlm
         fd_total = fddif_clr(i,1)+amu0(i)*slr(i)*direct_s(l+1)
         fddir_clr(i,l+1) = amu0(i)*slr(i)*direct_us(l+1)
         fddif_clr(i,l+1) = fd_total - fddir_clr(i,l+1)
      enddo
      !END CLEAR-SKY CALC

      !Cloudy layer properties
      direct_s(1) = 1.
      do l = 1, nlm
         !Delta-M on
         fact = asym_cld(i,l)*asym_cld(i,l)
         asy = asym_cld(i,l)/(1. + asym_cld(i,l))
         !Delta-M off
         !fact = 0.
         !asy = asym_cld(i,l)
         !End Delta-M on/off
         oms = ((1. - fact)*wc_cld(i,l))/(1. - fact*wc_cld(i,l))
         taus = (1. - fact*wc_cld(i,l))*tau_cld(i,l)



         exptau_s_cld(l) = exp(-taus/amu0(i))
         direct_s(l+1) = exptau_s_cld(l)*direct_s(l)
         exptau_us_cld(l) = exp(-tau_cld(i,l)/amu0(i))
         !direct_us_cld is not needed till later
         ! Local eddington coefficients
         t =  0.25*(7. - oms*(4. + 3.*asy))
         r = -0.25*(1. - oms*(4. - 3.*asy))
         kappa = sqrt(t*t - r*r)
         rinf = r/(kappa+t)
         ggtau = kappa*taus
         eggtau = exp(-ggtau)
         denom = (1. - rinf*rinf*eggtau*eggtau)
         tr_cld(l) = (1. - rinf*rinf)*eggtau/denom
         rr_cld(l) = rinf*(1. - eggtau*eggtau)/denom

         fact = kappa*kappa - 1./(amu0(i)*amu0(i))
         if (abs(fact) .lt. eps) then
            fact = 1./eps
         else
            fact = 1./fact
         endif

         cc = oms*slr(i)*fact
         g3 = 0.5 - 0.75*asy*amu0(i)
         g4 = 1. - g3
         aa = g3*(t - 1./amu0(i)) + g4*r
         bb = g4*(t + 1./amu0(i)) + g3*r
         sigu_cld_fract(l) = cc*((aa - rr_cld(l)*bb) - aa*tr_cld(l)*exptau_s_cld(l))
         sigu(l) = sigu_cld_fract(l)*direct_s(l)
         sigd_cld_fract(l) = cc*(-bb*tr_cld(l) + (bb-rr_cld(l)*aa)*exptau_s_cld(l))
         sigd(l) = sigd_cld_fract(l)*direct_s(l)
      enddo

      !BEGIN MAXIMALLY OVERLAPPED CALC
      !For layers where 0. < cf_max < 1., treat as randomly combined
      direct_s(1) = 1.
      direct_us(1) = 1.
      do l = 1,nlm
         rr_tmp(l) = cf_max(i,l)*rr_cld(l) + (1. - cf_max(i,l))*rr_clr(l)
         tr_tmp(l) = cf_max(i,l)*tr_cld(l) + (1. - cf_max(i,l))*tr_clr(l)
         sigu_tmp_fract(l) = cf_max(i,l)*sigu_cld_fract(l) + (1. - cf_max(i,l))*sigu_clr_fract(l)
         sigd_tmp_fract(l) = cf_max(i,l)*sigd_cld_fract(l) + (1. - cf_max(i,l))*sigd_clr_fract(l)
         direct_s(l+1) = (cf_max(i,l)*exptau_s_cld(l) + (1. - cf_max(i,l))*exptau_s_clr(l))*direct_s(l)
         direct_us(l+1) = (cf_max(i,l)*exptau_us_cld(l) + (1. - cf_max(i,l))*exptau_us_clr(l))*direct_us(l)
      enddo
      !MAXIMALLY OVERLAPPED adding
      re(1) = 0.
      vd(1) = 0.
      do l = 1, nlm
         prop = 1./(1. - re(l)*rr_tmp(l))
         re(l+1) = rr_tmp(l) + tr_tmp(l)*tr_tmp(l)*re(l)*prop
         vd(l+1) = sigd_tmp_fract(l)*direct_s(l) + (tr_tmp(l)*vd(l) + tr_tmp(l)*re(l)*sigu_tmp_fract(l)*direct_s(l))*prop
         vu(l) = (rr_tmp(l)*vd(l) + sigu_tmp_fract(l)*direct_s(l))*prop
         td(l) = prop
      enddo

      !MAXIMALLY OVERLAPPED diffuse flux calculation
      fddif_max(1) = 0.
      fudif_max(nlm+1) = (asdif(i,ib)*vd(nlm+1) +                         &
                            asdir(i,ib)*slr(i)*amu0(i)*direct_s(nlm+1))/      &
                           (1. - asdif(i,ib)*re(nlm+1))
      do l = nlm+1, 2, -1
         fddif_max(l) = re(l)*fudif_max(l) + vd(l)
         fudif_max(l-1) = tr_tmp(l-1)*fudif_max(l)*td(l-1) + vu(l-1)
      enddo
 
      !MAXIMALLY OVERLAPPED direct flux calculation
      fddir_max(1) = amu0(i)*slr(i)
      do l = 1, nlm
         fd_total = amu0(i)*slr(i)*direct_s(l+1) + fddif_max(l+1)
         fddir_max(l+1) = amu0(i)*slr(i)*direct_us(l+1)
         fddif_max(l+1) = fd_total - fddir_max(l+1)
      enddo
      !END MAXIMALLY OVERLAPPED CALC

      !BEGIN RANDOMLY-OVERLAPPED CALC
      direct_s(1) = 1.
      direct_us(1) = 1.
      do l = 1, nlm
         rr_tmp(l) = cf_random(i,l)*rr_cld(l) + (1. - cf_random(i,l))*rr_clr(l)
         tr_tmp(l) = cf_random(i,l)*tr_cld(l) + (1. - cf_random(i,l))*tr_clr(l)
         sigu_tmp_fract(l) = cf_random(i,l)*sigu_cld_fract(l) + (1. - cf_random(i,l))*sigu_clr_fract(l)
         sigd_tmp_fract(l) = cf_random(i,l)*sigd_cld_fract(l) + (1. - cf_random(i,l))*sigd_clr_fract(l)
         direct_s(l+1) = (cf_random(i,l)*exptau_s_cld(l) + (1. - cf_random(i,l))*exptau_s_clr(l))*direct_s(l)
         direct_us(l+1) = (cf_random(i,l)*exptau_us_cld(l) + (1. - cf_random(i,l))*exptau_us_clr(l))*direct_us(l)
     enddo
     !RANDOMLY-OVERLAPPED adding
      re(1) = 0.
      vd(1) = 0.
      do l = 1, nlm
         prop = 1./(1. - re(l)*rr_tmp(l))
         re(l+1) = rr_tmp(l) + tr_tmp(l)*tr_tmp(l)*re(l)*prop
         vd(l+1) = sigd_tmp_fract(l)*direct_s(l) + (tr_tmp(l)*vd(l) + tr_tmp(l)*re(l)*sigu_tmp_fract(l)*direct_s(l))*prop
         vu(l) = (rr_tmp(l)*vd(l) + sigu_tmp_fract(l)*direct_s(l))*prop
         td(l) = prop
      enddo

      !RANDOMLY-OVERLAPPED diffuse flux calculation
      fddif_rndm(1) = 0.
      fudif_rndm(nlm+1)= (asdif(i,ib)*vd(nlm+1) +                        &
                           asdir(i,ib)*slr(i)*amu0(i)*direct_s(nlm+1))/     &
                          (1. - asdif(i,ib)*re(nlm+1))
      do l = nlm+1, 2, -1
         fddif_rndm(l) = re(l)*fudif_rndm(l) + vd(l)
         fudif_rndm(l-1) = tr_tmp(l-1)*fudif_rndm(l)*td(l-1) + vu(l-1)
      enddo
      
      !RANDOMLY-OVERLAPPED direct flux calculation
      fddir_rndm(1) = amu0(i)*slr(i)
      do l = 1, nlm
         fd_total = amu0(i)*slr(i)*direct_s(l+1) + fddif_rndm(l+1)
         fddir_rndm(l+1) = amu0(i)*slr(i)*direct_us(l+1) 
         fddif_rndm(l+1) = fd_total - fddir_rndm(l+1)
      enddo
      !END RANDOMLY-OVERLAPPED CALC
      !BEGIN ALL-SKY CALC
      !This is the combined maximally- and randomly overlapped portions of the sky
      fddif_all(i,1) = 0.
      fddir_all(i,1) = amu0(i)*slr(i)
      fudif_all(i,nlm+1) = (1. - c_maximal(i))*fudif_rndm(nlm+1) + c_maximal(i)*fudif_max(nlm+1)
      do l = 1, nlm
         fddif_all(i,l+1) = (1. - c_maximal(i))*fddif_rndm(l+1) + c_maximal(i)*fddif_max(l+1)
         fddir_all(i,l+1) = (1. - c_maximal(i))*fddir_rndm(l+1) + c_maximal(i)*fddir_max(l+1)
         fudif_all(i,l) = (1. - c_maximal(i))*fudif_rndm(l) + c_maximal(i)*fudif_max(l)
      enddo
   enddo ! Loop over columns
   end subroutine 
