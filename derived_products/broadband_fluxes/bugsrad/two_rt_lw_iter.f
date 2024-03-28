

! CVS:  $Id: two_rt_lw_iter.F,v 1.2 2003/11/11 21:55:13 norm Exp $
! CVS:  $Name:  $

!-----------------------------------------------------------------------

      subroutine two_rt_lw_iter
     +              (
     +                    ncol ,    nlm ,  mbs ,   mbir
     +,                     ib , cldamt ,   wc ,  wcclr
     +,                   asym , asyclr ,  tau , tauclr
     +,                     es ,     bf ,   fu ,     fd
     +,              sel_rules ,     b1 ,   b2 ,     b3
     +,                     b4
     +              )

      use kinds



      implicit none

!-----------------------------------------------------------------------
! REFERENCES:
! two_rt_lw replaces two_rt and add written by G. Stephens. two_rt_lw
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
     &  ncol   !Length of sub-domain.
     &, nlm    !Number of layers.
     &, mbs    !Number of SW spectral intervals.
     &, mbir   !Number of IR spectral intervals.
     &, ib     !Index of spectral interval.

      real (kind=dbl_kind), intent(in), dimension(ncol,mbir)::
     &  es    !Spectral surface emissivity                         (-).

      real (kind=dbl_kind), intent(in), dimension(ncol,nlm)::
     &  cldamt !Cloud fraction                                     (-).
     &, wc     !All sky single scattering albedo                   (-).
     &, wcclr  !Clear sky single scattering albedo                 (-).
     &, asym   !All sky asymmetry factor                           (-).
     &, asyclr !Clear sky asymmetry factor                         (-).
     &, tau    !All sky optical depth                              (-).
     &, tauclr !Clear sky optical depth                            (-).
     &, b1     !Cloud overlap parameter                            (-).
     &, b2     !Cloud overlap parameter                            (-).
     &, b3     !Cloud overlap parameter                            (-).
     &, b4     !Cloud overlap parameter                            (-).

      real (kind=dbl_kind), intent(in), dimension(ncol,nlm+1)::
     &  bf    !Planck function                                 (W/m^2).

!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(ncol,nlm+1)::
     &  fd     !Spectral downward flux                         (W/m^2).
     &, fu     !Spectral upward flux                           (W/m^2).

! LOCAL VARIABLES:

      integer(kind=int_kind)
     &  i      !Horizontal index.
     &, l      !Vertical index.
     &, ibms   !Index of spectral interval.
     &, j
     &, nsr
     &, nsmx
     &, n
     &, ii
     &, jj
     &, kk
     &, ir
     &, iter
      integer (kind=int_kind), dimension(16*nlm-6)::
     &  idc

      integer (kind=int_kind), dimension(4*nlm+2)::
     &  nir

      real (kind=dbl_kind), dimension(nlm)::
     &  rrcld    !All sky global reflection                        (-).
     &, rrclr    !Clear sky global reflection                      (-).
     &, trcld    !All sky global transmission                      (-).
     &, trclr    !Clear sky global transmission                    (-).
     &, sigucld  !All sky upwelling source                         (-).
     &, siguclr  !Clear sky upwelling source                       (-).
     &, sigdcld  !All sky downwelling source                       (-).
     &, sigdclr  !Clear sky downwelling source                     (-).
     &, exptau   !

      real (kind=dbl_kind), dimension(4*nlm+2)::
     &  b
     &, fvc
     &, error
!     &, old_fvc

      real (kind=dbl_kind), dimension(16*nlm-6)::
     &  smx

      real(kind=dbl_kind)
     &      aa ,    bb , beta0 ,     cc
     &, diffac , denom ,  fact , eggtau
     &,  ggtau
     &,  kappa ,   oms ,  prop ,r, rinf
     &,      t ,  taus , omega
      data diffac /2./

! SELECTION RULE VARIABLES

      logical (kind=log_kind)::
     &  fail

      real (kind=dbl_kind)::
     &  tausthresh
     &, wcthresh
     &, tauscat
      data tausthresh / 0.001 /
      data wcthresh   / 0.975 /

!----------------------------------------------------------------------
      nsr = 4*nlm + 2
      nsmx = 16*nlm - 6

      fd(:,1) = 0.

      ibms = ib - mbs

      do i = 1, ncol

!---- 2. DO LONGWAVE:
         do l = 1, nlm
            !ALL SKY
            fact = asym(i,l)*asym(i,l)
            oms  = ((1.-fact)*wc(i,l))/(1.-fact*wc(i,l))
            taus   = (1.-fact*wc(i,l))*tau(i,l)

            beta0 = (4.+asym(i,l))/(8.*(1.+asym(i,l)))
            t = diffac*(1.-oms*(1.-beta0))     !-0.25
            r = diffac*oms*beta0               !-0.25
            kappa = sqrt(t**2-r**2)
            rinf   = r/(kappa+t)
            ggtau  = kappa*taus

            eggtau = exp(-ggtau)

            denom  = (1.-rinf**2*eggtau**2)
            trcld(l) = (1.-rinf**2)*eggtau/denom
            rrcld(l) = rinf*(1.-eggtau**2)/denom

            if (taus .lt. .8e-2) then
               sigucld(l) = cldamt(i,l)*0.5*diffac*(bf(i,l)+
     *                      bf(i,l+1))*taus
               sigdcld(l) = cldamt(i,l)*sigucld(l)
            else
               aa =  (t+r)*(1.-rrcld(l))-(1.+rrcld(l)-trcld(l))/taus
               bb = -(t+r)*trcld(l)+(1.+rrcld(l)-trcld(l))/taus
               cc = diffac*(1.-oms)/kappa**2
               sigucld(l) = cldamt(i,l)*cc*(aa*bf(i,l)+bb*bf(i,l+1))
               sigdcld(l) = cldamt(i,l)*cc*(bb*bf(i,l)+aa*bf(i,l+1))
            end if

            !CLEAR SKY
            fact = asyclr(i,l)*asyclr(i,l)
            oms  = ((1.-fact)*wcclr(i,l))/(1.-fact*wcclr(i,l))
            taus   = (1.-fact*wcclr(i,l))*tauclr(i,l)

            beta0 = (4.+asyclr(i,l))/(8.*(1.+asyclr(i,l)))
            t = diffac*(1.-oms*(1.-beta0))     !-0.25
            r = diffac*oms*beta0               !-0.25
            kappa = sqrt(t**2-r**2)
            rinf   = r/(kappa+t)
            ggtau  = kappa*taus

            eggtau = exp(-ggtau)

            denom  = (1.-rinf**2*eggtau**2)
            trclr(l) = (1.-rinf**2)*eggtau/denom
            rrclr(l) = rinf*(1.-eggtau**2)/denom

            if (taus .lt. .8e-2) then
               siguclr(l) = (1.0-cldamt(i,l))*0.5*diffac*(bf(i,l)+
     *                      bf(i,l+1))*taus
               sigdclr(l) = (1.0-cldamt(i,l))*siguclr(l)
            else
               aa =  (t+r)*(1.-rrclr(l))-(1.+rrclr(l)-trclr(l))/taus
               bb = -(t+r)*trclr(l)+(1.+rrclr(l)-trclr(l))/taus
               cc = diffac*(1.-oms)/kappa**2
               siguclr(l) = (1.0-cldamt(i,l))*cc*(aa*bf(i,l)+
     *                      bb*bf(i,l+1))
               sigdclr(l) = (1.0-cldamt(i,l))*cc*(bb*bf(i,l)+
     *                      aa*bf(i,l+1))
            end if

         end do


!---- 1. LOAD SMX VECTOR
        nir(:) = 4

        idc(1) = 5
        idc(2) = 6
        smx(1) = -trcld(1) * b4(i,1)
        smx(2) = -trcld(1) * (1.-b2(i,1))
        nir(1) = 2

        idc(3) = 5
        idc(4) = 6
        smx(3) = -trclr(1) * (1.-b4(i,1))
        smx(4) = -trclr(1) * b2(i,1)
        nir(2) = 2

        idc(5) = 5
        idc(6) = 6
        smx(5) = -rrcld(1) * b4(i,1)
        smx(6) = -rrcld(1) * (1.-b2(i,1))
        nir(3) = 2

        idc(7) = 5
        idc(8) = 6
        smx(7) = -rrclr(1) * (1.-b4(i,1))
        smx(8) = -rrclr(1) * b2(i,1)
        nir(4) = 2

        do l = 1, nlm-1
          n = (l-1)*16 + 9
          ir = 4*l

          idc(n)   = ir-1
          idc(n+1) = ir
          idc(n+2) = ir+5
          idc(n+3) = ir+6
          smx(n)   = -rrcld(l+1) * b3(i,l+1)
          smx(n+1) = -rrcld(l+1) * (1.-b1(i,l+1))
          smx(n+2) = -trcld(l+1) * b4(i,l+1)
          smx(n+3) = -trcld(l+1) * (1.-b2(i,l+1))

          idc(n+4) = ir-1
          idc(n+5) = ir
          idc(n+6) = ir+5
          idc(n+7) = ir+6
          smx(n+4) = -rrclr(l+1) * (1.-b3(i,l+1))
          smx(n+5) = -rrclr(l+1) * b1(i,l+1)
          smx(n+6) = -trclr(l+1) * (1.-b4(i,l+1))
          smx(n+7) = -trclr(l+1) * b2(i,l+1)

          idc(n+8) = ir-1
          idc(n+9) = ir
          idc(n+10) = ir+5
          idc(n+11) = ir+6
          smx(n+8) = -trcld(l+1) * b3(i,l+1)
          smx(n+9) = -trcld(l+1) * (1.-b1(i,l+1))
          smx(n+10) = -rrcld(l+1) * b4(i,l+1)
          smx(n+11) = -rrcld(l+1) * (1.-b2(i,l+1))

          idc(n+12) = ir-1
          idc(n+13) = ir
          idc(n+14) = ir+5
          idc(n+15) = ir+6
          smx(n+12) = -trclr(l+1) * (1.-b3(i,l+1))
          smx(n+13) = -trclr(l+1) * b1(i,l+1)
          smx(n+14) = -rrclr(l+1) * (1.-b4(i,l+1))
          smx(n+15) = -rrclr(l+1) * b2(i,l+1)
        end do

        ir = 4*nlm

        idc(16*nlm-7) = 4*nlm-1
        idc(16*nlm-6) = 4*nlm
        smx(16*nlm-7) = 0.0 !1.-es(i,ibms)
        smx(16*nlm-6) = 0.0 !1.-es(i,ibms)
        nir(ir+1) = 1
        nir(ir+2) = 1

        b(:) = 0.0
        do l = 1, nlm
          b(l*4-3) = sigucld(l)
          b(l*4-2) = siguclr(l)
          b(l*4-1) = sigdcld(l)
          b(l*4)   = sigdclr(l)
        end do
        b(nlm*4+1) = cldamt(i,nlm)*es(i,ibms)*bf(i,nlm+1)
        b(nlm*4+2) = (1.-cldamt(i,nlm))*es(i,ibms)*bf(i,nlm+1)




!-------------- GAUSS SEIDEL W/ OVERRELAXATION ------------------
        omega = 1.0

        fvc(:) = b(:)

        do iter = 1, 200
           kk = 1
           do ii = 1, nsr
             t = 0.0
             do j = 1, nir(ii)
               jj = idc(kk)
               t = t + smx(kk) * fvc(jj)
               kk = kk + 1
             end do
             t = t + fvc(ii)
             fvc(ii) = fvc(ii) + omega * (b(ii)-t)
             error(ii) = b(ii) - t
           end do

           if (maxval(abs(error)) .le. 0.05) then
             !print *,omega,iter,' iterations'
             exit
           end if
        end do


!---- 3. SUM CLEAR AND CLOUDY FLUXES
        do l = 1, nlm+1
          fu(i,l) = fvc(l*4-3)+fvc(l*4-2)
        end do
        do l = 1, nlm
          fd(i,l+1) = fvc(l*4-1)+fvc(l*4)
        end do

      end do  !i=1,ncol

      return
      end
