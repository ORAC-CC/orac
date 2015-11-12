

! CVS:  $Id: two_rt_sw.F,v 1.7 2006/11/16 21:19:38 norm Exp $
! CVS:  $Name:  $

!NBW This has been modified to apply delta-M descaling
!NBW to the direct and diffuse downwelling fluxes


!-----------------------------------------------------------------------
 
      subroutine two_rt_sw
     +               ( ncol ,   nlm ,      mbs ,    ib
     +,                 slr ,  amu0 ,       wc ,  asym
     +,                 tau , asdir ,    asdif , fudif
     +,               fddir , fddif ,sel_rules
     +               )
 
      use kinds



      implicit none

!-----------------------------------------------------------------------
! REFERENCES:
! two_rt_sw replaces two_rt and add written by G. Stephens. two_rt_sw
! computes the spectral fluxes using a two-stream approximation method.
! Philip Partain, Philip Gabriel, and Laura D. Fowler/graben (09-08-99).
 
! MODIFICATIONS:
! * changed declarations to adapt the code from BUGS4 to BUGS5.
!   Laura D. Fowler/slikrock (02-01-00).

! SUBROUTINES CALLED:
!     none.

! FUNCTIONS CALLED:
!     none.

! INCLUDED COMMONS:
!     none.
 
! ARGUMENT LIST VARIABLES:
! All arrays indexed as nlm correspond to variables defined in the
! middle of layers. All arrays indexed as nlm+1 correspond to variables
! defined at levels at the top and bottom of layers.
 
!     INPUT ARGUMENTS:
!     ----------------
      logical (kind=log_kind), intent(in)::
     &  sel_rules

      integer (kind=int_kind), intent(in)::
     &  ncol  !Length of sub-domain.
     &, nlm   !Number of layers.
     &, mbs   !Number of SW spectral intervals.
     &, ib    !Index of spectral interval.
 
      real (kind=dbl_kind), intent(in), dimension(ncol)::
     &  slr   !Fraction of daylight                                (-).
     &, amu0  !Cosine of the solar zenith angle                    (-).

      real (kind=dbl_kind), intent(in), dimension(ncol,mbs)::
     &  asdir !Spectral direct surface albedo                      (-).
     &, asdif !Spectral diffuse surface albedo                     (-).

      real (kind=dbl_kind), intent(in), dimension(ncol,nlm)::
     &  wc    !Single scattering albedo                            (-).
     &, asym  !Asymmetry factor                                    (-).
     &, tau   !Optical depth                                       (-).
 
!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(ncol,nlm+1)::
     &  fddir !Spectral direct downward flux                   (W/m^2).
     &, fddif !Spectral diffuse downward flux                  (W/m^2).
     &, fudif !Spectral diffuse upward flux                    (W/m^2).
 
! LOCAL VARIABLES:

      integer(kind=int_kind)
     &  i     ! Horizontal index.
     &, l     ! Vertical index.
 
      real (kind=dbl_kind), dimension(nlm)::
     &  rr      !
     &, tr      !
     &, sigu    !
     &, sigd    !

      real(kind=dbl_kind)
     &  exptau_s(nlm),direct_s(nlm+1),exptau_us(nlm),direct_us(nlm+1)

      real(kind=dbl_kind)
     &      aa ,    bb ,   cc , denom
     &, eggtau ,   eps ,   g3 ,    g4
     &,  ggtau , kappa ,    r ,  rinf
     &,      t ,   oms , taus ,  fact
     &,    asy ,  prop, fd_total
      data eps /1.e-02/

      real (kind=dbl_kind), dimension(nlm)::
     &  td , vu

      real (kind=dbl_kind), dimension(nlm+1)::     
     &  re , vd
 
! SELECTION RULE VARIABLES

      logical (kind=log_kind)::
     &  fail

      real (kind=dbl_kind)::
     &  tausthresh
     &, wcthresh
     &, tauscat


!     data tausthresh / 0.001 /
!     data wcthresh   / 0.975 /
      data tausthresh / 0.01 /
      data wcthresh   / 0.98 /
 
!----------------------------------------------------------------------
 
      do 1000 i = 1, ncol
 
         if(sel_rules) then

            fail = .false.
            tauscat = 0.0
            do l = nlm, 1, -1
               if (wc(i,l).gt.wcthresh) fail = .true.
               tauscat = tauscat + wc(i,l)*tau(i,l)
            enddo
            
            if (fail.and.tauscat.ge.tausthresh) goto 2000
 
           print *,'selection rules'
            fddir(i,1) = amu0(i)*slr(i)
            fddif(i,:) = 0.0
            do l = 1, nlm
               fddir(i,l+1) = fddir(i,l) * exp(-1.*tau(i,l)/amu0(i))
            enddo
            fudif(i,nlm+1) = fddir(i,nlm+1) * asdir(i,ib)
 
            do l = nlm, 1, -1
               fudif(i,l) = fudif(i,l+1) * exp(-2*tau(i,l))
            enddo

            cycle

         endif

2000     direct_s(1) = 1.
         direct_us(1) = 1.
         re(1) = 0.
         vd(1) = 0.
 
         do l = 1, nlm
            !delta-M on
            fact = asym(i,l)*asym(i,l)
            asy = asym(i,l)/(1.+asym(i,l))    !Orig w/ delta M
            !This is the delta-M scaling approx
            !delta-M off
            !fact = 0.
            !asy = asym(i,l)
            !End delta-M on/off
            oms  = ((1.-fact)*wc(i,l))/(1.-fact*wc(i,l))
            taus   = (1.-fact*wc(i,l))*tau(i,l)
            
 
            exptau_s(l) = exp(-taus/amu0(i))         !delta-M scaled
            direct_s(l+1) = exptau_s(l)*direct_s(l)  !delta-M scaled
            exptau_us(l) = exp(-tau(i,l)/amu0(i))            !unscaled
            direct_us(l+1) = exptau_us(l)*direct_us(l)       !unscaled
 
!---- local coefficients:  delta-eddington
            t      = 0.25 * (7. - oms*(4.+3.*asy))
            r      = -0.25 * (1. - oms*(4.-3.*asy))
            kappa  = sqrt(t**2-r**2)
            rinf   = r/(kappa+t)
            ggtau  = kappa*taus
            eggtau = exp(-ggtau)
            denom  = (1.-rinf**2*eggtau**2)
            tr(l)  = (1.-rinf**2)*eggtau/denom
            rr(l)  = rinf*(1.-eggtau**2)/denom
 
            if(abs(kappa**2-1./amu0(i)**2) .lt. eps) then
               fact = 1./eps
            else
               fact = 1./(kappa**2-1./amu0(i)**2)
            endif
            cc = oms*slr(i)*fact
            g3 = 0.5-0.75*asy*amu0(i)
            g4 = 1.-g3
            aa = g3*(t-1./amu0(i))+g4*r
            bb = g4*(t+1./amu0(i))+g3*r
            sigu(l) = cc*((aa-rr(l)*bb)-aa*tr(l)*exptau_s(l))
     +              * direct_s(l)
            sigd(l) = cc*(-bb*tr(l)+(bb-rr(l)*aa)*exptau_s(l))
     +              * direct_s(l)
         enddo
 
!---- 1. do adding, going from top down:

         do l = 1, nlm
            prop = 1. / (1. - re(l)*rr(l))
            re(l+1) = rr(l) + tr(l)**2*re(l)*prop
            vd(l+1) = sigd(l) + (tr(l)*vd(l)
     +              + tr(l)*re(l)*sigu(l))*prop
            vu(l)   = (rr(l)*vd(l) + sigu(l))*prop
            td(l)   = prop
         enddo
 
!---- 2. calculate diffuse fluxes:

         fddif(i,1) = 0.
         fudif(i,nlm+1) = (asdif(i,ib)*vd(nlm+1)
     +                  + asdir(i,ib)*slr(i)*amu0(i)*direct_s(nlm+1))
     +                  / (1.-asdif(i,ib)*re(nlm+1))
 
         do l = nlm+1, 2, -1
            fddif(i,l)   = re(l)*fudif(i,l) + vd(l)
            fudif(i,l-1) = tr(l-1)*fudif(i,l)*td(l-1) + vu(l-1)
         enddo
 
!---- 3. Compute direct flux and descale the direct and diffuse fluxes
         fddir(i,1) = amu0(i)*slr(i)
         do l = 1, nlm
            fd_total = amu0(i)*slr(i)*direct_s(l+1) + fddif(i,l+1)
            fddir(i,l+1) = amu0(i)*slr(i)*direct_us(l+1)
            fddif(i,l+1) = fd_total - fddir(i,l+1)
         enddo
         

 1000 continue
      return
      end subroutine two_rt_sw
 
!------------------------------------------------------------------------
