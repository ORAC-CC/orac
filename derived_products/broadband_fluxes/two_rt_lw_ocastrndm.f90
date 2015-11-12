

subroutine two_rt_lw_gsolap                &
   (     ncol ,    nlm ,    mbs ,    mbir, &
           ib , cldamt ,  wc_cld,  wc_clr, &
     asym_cld ,asym_clr,tau_cld , tau_clr, &
           es ,     bf , fu_all ,  fu_clr, &
       fd_all , fd_clr , c_maximal, cf_max,&
       cf_random)

 
   use kinds
   implicit none

!-----------------------------------------------------------------------
! REFERENCES:
! Alternate cloud overlap calculations
 

! SUBROUTINES CALLED:
!     none.

! FUNCTIONS CALLED:
!     none.

! INCLUDED COMMONS:
!     none.
 
! ARGUMENT LIST VARIABLES:
!     INPUT ARGUMENTS:
!     ----------------
   integer (kind=int_kind), intent(in):: &
     ncol,      & !Length of sub-domain. 
     nlm,       & !Number of layers.
     mbs,       & !Number of SW spectral intervals.
     mbir,      & !Number of IR spectral intervals.
     ib           !Index of spectral interval.
 
   real (kind=dbl_kind), intent(in), dimension(ncol) :: &
     c_maximal

   real (kind=dbl_kind), intent(in), dimension(ncol,mbir):: &
     es         !Spectral surface emissivity                         (-).

   real (kind=dbl_kind), intent(in), dimension(ncol,nlm):: &
     wc_cld,    & !Single scattering albedo                            (-).
     wc_clr,    & !Clear sky single-scattering albedo
     asym_cld,  & !Asymmetry factor                                    (-).
     asym_clr,  & !Clear sky asymmetry factor
     tau_cld,   & !Optical depth                                       (-).
     tau_clr,   & !Clear sky optical depth
     cldamt,    & !Cloud fraction                                      (-).
     cf_max,    & !Cf in maximally overlapped region                   (-).
     cf_random    !Cf in randomly overlapped region                    (-).

   real (kind=dbl_kind), intent(in), dimension(ncol,nlm+1):: &
     bf           !Planck function                                 (W/m^2).
 
!     OUTPUT ARGUMENTS:
!     -----------------
   real (kind=dbl_kind), intent(out), dimension(ncol,nlm+1):: &
     fd_all,    & !All-sky spectral downward flux                          (W/m^2).
     fd_clr,    & !Clear sky spectral downward flux
     fu_all,    & !All-sky spectral upward flux                            (W/m^2).
     fu_clr       !Clear sky spectral upward flux
 
! LOCAL VARIABLES:
! ----------------
   integer (kind=int_kind) :: &
     i,         & !Horizontal index.
     l,         & !Vertical index.
     ibms         !Index of spectral interval.
 
   real (kind=dbl_kind), dimension(nlm):: &
     rr_clr,    & !
     rr_cld,    & !
     rr_tmp,    & !
     tr_clr,    & !
     tr_cld,    & !
     tr_tmp,    & !
     sigu_clr,  & !
     sigu_cld,  & !
     sigu_tmp,  & !
     sigd_clr,  & !
     sigd_cld,  & !
     sigd_tmp     !

   real (kind=dbl_kind) :: &
     aa,        & !
     bb,        & !
     beta0,     & !
     cc,        & !
     diffac,    & !
     denom,     & !
     fact,      & !
     eggtau,    & !
     ggtau,     & !
     kappa,     & !
     oms,       & !
     prop,      & !
     r,         & !
     rinf,      & !
     t,         & !
     taus         !

   data diffac /2./

   real (kind=dbl_kind), dimension(nlm):: &
     td,        & !
     vu,        & !
     exptau

   real (kind=dbl_kind), dimension(nlm+1):: &
     re,        & !
     vd,        & !
     fd_max,    & !Maximally overlapped cloudy sky spectral downward flux
     fd_rndm,    & !Broken sky (randomly overlapped cloudy sky) spectral downward flux
     fu_max,    & !Maximally overlapped cloud sky spectral upward flux
     fu_rndm       !Broken sky (randomly overlapped cloudy sky) spectral upward flux


   real (kind=dbl_kind) :: &
     cld_ran,   &  !The random overlap fraction of cloud
     cmin          !Minimum cloud fraction in a column

   ibms = ib - mbs
!Begin loop over all columns
   do i = 1, ncol

      !Clear layer properties
      do l = 1, nlm
         fact = asym_clr(i,l)*asym_clr(i,l)
         oms  = ((1.-fact)*wc_clr(i,l))/(1.-fact*wc_clr(i,l))
         taus   = (1.-fact*wc_clr(i,l))*tau_clr(i,l)

         beta0 = (4.+asym_clr(i,l))/(8.*(1.+asym_clr(i,l)))
         t = diffac*(1.-oms*(1.-beta0))     !-0.25
         r = diffac*oms*beta0               !-0.25
         kappa = sqrt(t**2-r**2)
         rinf   = r/(kappa+t)
         ggtau  = kappa*taus
         eggtau = exp(-ggtau)
         denom  = (1.-rinf**2*eggtau**2)
         tr_clr(l) = (1.-rinf**2)*eggtau/denom
         rr_clr(l) = rinf*(1.-eggtau**2)/denom

         if(taus .lt. .8e-2) then
            sigu_clr(l) = 0.5*diffac*(bf(i,l)+bf(i,l+1))*taus
            sigd_clr(l) = sigu_clr(l)
         else
            aa =  (t+r)*(1.-rr_clr(l))-(1.+rr_clr(l)-tr_clr(l))/taus
            bb = -(t+r)*tr_clr(l)+(1.+rr_clr(l)-tr_clr(l))/taus
            cc = diffac*(1.-oms)/kappa**2
            sigu_clr(l) = cc*(aa*bf(i,l)+bb*bf(i,l+1))
            sigd_clr(l) = cc*(bb*bf(i,l)+aa*bf(i,l+1))
         endif
      enddo

      !CLEAR SKY adding
      re(1) = 0.
      vd(1) = 0.
      do l = 1, nlm
         prop = 1. / (1. - re(l)*rr_clr(l))
         re(l+1) = rr_clr(l) + tr_clr(l)**2*re(l)*prop
         vd(l+1) = sigd_clr(l) + (tr_clr(l)*vd(l)           &
                 + tr_clr(l)*re(l)*sigu_clr(l))*prop
         vu(l)   = (rr_clr(l)*vd(l) + sigu_clr(l))*prop
         td(l)   = prop
      enddo

      !CLEAR SKY flux calculation
      fu_clr(i,nlm+1) = es(i,ibms)*bf(i,nlm+1)
      fd_clr(i,1) = 0.
      do l = nlm+1, 2, -1
         fd_clr(i,l)   = re(l)*fu_clr(i,l) + vd(l)
         fu_clr(i,l-1) = tr_clr(l-1)*fu_clr(i,l)*td(l-1) + vu(l-1)
      enddo
      !END CLEAR SKY CALC


      !Cloudy layer properties
      do l = 1, nlm
         fact = asym_cld(i,l)*asym_cld(i,l)
         oms  = ((1.-fact)*wc_cld(i,l))/(1.-fact*wc_cld(i,l))
         taus   = (1.-fact*wc_cld(i,l))*tau_cld(i,l)

         beta0 = (4.+asym_cld(i,l))/(8.*(1.+asym_cld(i,l)))
         t = diffac*(1.-oms*(1.-beta0))     !-0.25
         r = diffac*oms*beta0               !-0.25
         kappa = sqrt(t**2-r**2)
         rinf   = r/(kappa+t)
         ggtau  = kappa*taus
         eggtau = exp(-ggtau)
         denom  = (1.-rinf**2*eggtau**2)
         tr_cld(l) = (1.-rinf**2)*eggtau/denom
         rr_cld(l) = rinf*(1.-eggtau**2)/denom

         if(taus .lt. .8e-2) then
            sigu_cld(l) = 0.5*diffac*(bf(i,l)+bf(i,l+1))*taus
            sigd_cld(l) = sigu_cld(l)
         else
            aa =  (t+r)*(1.-rr_cld(l))-(1.+rr_cld(l)-tr_cld(l))/taus
            bb = -(t+r)*tr_cld(l)+(1.+rr_cld(l)-tr_cld(l))/taus
            cc = diffac*(1.-oms)/kappa**2
            sigu_cld(l) = cc*(aa*bf(i,l)+bb*bf(i,l+1))
            sigd_cld(l) = cc*(bb*bf(i,l)+aa*bf(i,l+1))
         endif
      enddo


      !BEGIN MAXIMALLY OVERLAPPED CALC
      !For layers where 0. < cf_max < 1., treat as randomly combined
      do l = 1, nlm
         rr_tmp(l) = cf_max(i,l)*rr_cld(l) + (1. - cf_max(i,l))*rr_clr(l)
         tr_tmp(l) = cf_max(i,l)*tr_cld(l) + (1. - cf_max(i,l))*tr_clr(l)
         sigu_tmp(l) = cf_max(i,l)*sigu_cld(l) + (1. - cf_max(i,l))*sigu_clr(l)
         sigd_tmp(l) = cf_max(i,l)*sigd_cld(l) + (1. - cf_max(i,l))*sigd_clr(l)
      enddo
      !MAXIMALLY OVERLAPPED adding 
      re(1) = 0.
      vd(1) = 0.
      do l = 1, nlm
         prop = 1. / (1. - re(l)*rr_tmp(l))
         re(l+1) = rr_tmp(l) + tr_tmp(l)**2*re(l)*prop
         vd(l+1) = sigd_tmp(l) + (tr_tmp(l)*vd(l)            &
                   + tr_tmp(l)*re(l)*sigu_tmp(l))*prop
         vu(l)   = (rr_tmp(l)*vd(l) + sigu_tmp(l))*prop
         td(l)   = prop
      enddo
      !MAXIMALLY OVERLAPPED flux calculation
      fu_max(nlm+1) = es(i,ibms)*bf(i,nlm+1)
      do l = nlm+1, 2, -1
         fd_max(l)   = re(l)*fu_max(l) + vd(l)
         fu_max(l-1) = tr_tmp(l-1)*fu_max(l)*td(l-1) + vu(l-1)
      enddo
      !END MAXIMALLY-OVERLAPPED CALC


      !BEGIN RANDOMLY-OVERLAPPED CALC
      do l = 1, nlm
         rr_tmp(l) = cf_random(i,l)*rr_cld(l) + (1. - cf_random(i,l))*rr_clr(l)
         tr_tmp(l) = cf_random(i,l)*tr_cld(l) + (1. - cf_random(i,l))*tr_clr(l)
         sigu_tmp(l) = cf_random(i,l)*sigu_cld(l) + (1. - cf_random(i,l))*sigu_clr(l)
         sigd_tmp(l) = cf_random(i,l)*sigd_cld(l) + (1. - cf_random(i,l))*sigd_clr(l)
      enddo

      !RANDOMLY-OVERLAPPED adding
      re(1) = 0.
      vd(1) = 0.
      do l = 1,nlm
         prop = 1./(1. - re(l)*rr_tmp(l))
         re(l+1) = rr_tmp(l) + tr_tmp(l)*tr_tmp(l)*re(l)*prop
         vd(l+1) = sigd_tmp(l) + (tr_tmp(l)*vd(l) + tr_tmp(l)*re(l)*sigu_tmp(l))*prop
         vu(l) = (rr_tmp(l)*vd(l) + sigu_tmp(l))*prop
         td(l) = prop
      enddo

      !RANDOMLY-OVERLAPPED flux calculation
      fu_rndm(nlm+1) = es(i,ibms)*bf(i,nlm+1)
      do l = nlm+1, 2, -1
         fd_rndm(l)   = re(l)*fu_rndm(l) + vd(l)
         fu_rndm(l-1) = tr_tmp(l-1)*fu_rndm(l)*td(l-1) + vu(l-1)
      enddo
      !END RANDOMLY-OVERLAPPED CALC

      !ALL-SKY: Begin
      !This is the combined maximally- and randomly overlapped portions of the sky
      fd_all(i,1) = 0.
      fu_all(i,nlm+1) = (1. - c_maximal(i))*fu_rndm(nlm+1) + c_maximal(i)*fu_max(nlm+1)
      do l = 1, nlm
         fd_all(i,l+1) = (1. - c_maximal(i))*fd_rndm(l+1) + c_maximal(i)*fd_max(l+1)
         fu_all(i,l) = (1. - c_maximal(i))*fu_rndm(l) + c_maximal(i)*fu_max(l)
      enddo
   
   enddo !Loop over columns 
   return
   end subroutine two_rt_lw_gsolap
