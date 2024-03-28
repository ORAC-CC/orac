

! CVS:  $Id: two_rt_lw_sel.F,v 1.3 2003/11/11 21:55:13 norm Exp $
! CVS:  $Name:  $


!-----------------------------------------------------------------------

      subroutine two_rt_lw_sel
     +                 (
     +                    ncol , nlm ,  mbs , mbir , ib
     +,                 tauclr ,  es ,   bf ,   fu , fd
     +                 )

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
      integer (kind=int_kind), intent(in)::
     &  ncol   !Length of sub-domain.
     &, nlm    !Number of layers.
     &, mbs    !Number of SW spectral intervals.
     &, mbir   !Number of IR spectral intervals.
     &, ib     !Index of spectral interval.

      real (kind=dbl_kind), intent(in), dimension(ncol,mbir)::
     &  es     ! Spectral surface emissivity                       (-).
      real (kind=dbl_kind), intent(in), dimension(ncol,nlm)::
     &  tauclr !Optical depth                                      (-).
      real (kind=dbl_kind), intent(in), dimension(ncol,nlm+1)::
     &  bf     !Planck function                                    (-).

!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(ncol,nlm+1)::
     &  fd    !Spectral downward flux                          (W/m^2).
     &, fu    !Spectral upward flux                            (W/m^2).

! LOCAL VARIABLES:

      integer(kind=int_kind)
     &  i     !Horizontal index.
     &, l     !Vertical index.
     &, ibms  !Index of spectral interval.

      real (kind=dbl_kind), dimension(nlm)::
     &  sigu
     &, sigd
     &, exptau

      real(kind=dbl_kind)
     &  aa
     &, bb
     &, cc
     &, prop

!----------------------------------------------------------------------


      ibms = ib - mbs

      do 1000 i = 1, ncol

          !TOA/BOA initializations
          fu(i,nlm+1) = bf(i,nlm+1)*es(i,ibms)
          fd(i,1) = 0.

          do l = 1, nlm
            exptau(l) = exp(-2*tauclr(i,l))
            if(tauclr(i,l) .lt. .8e-2) then
              sigu(l) = (bf(i,l)+bf(i,l+1))*tauclr(i,l)
              sigd(l) = sigu(l)
            else
              prop = (1.-exptau(l))/tauclr(i,l)
              aa = 2.-prop
              bb = -2.*exptau(l)+prop
              cc = 0.5
              sigu(l) = (aa*bf(i,l)+bb*bf(i,l+1))*cc
              sigd(l) = (bb*bf(i,l)+aa*bf(i,l+1))*cc
            end if
            fd(i,l+1) = sigd(l) + exptau(l) * fd(i,l)
          end do

          do l = nlm, 1, -1
            fu(i,l) = sigu(l) + exptau(l) * fu(i,l+1)
          end do

1000  continue

      return
      end
