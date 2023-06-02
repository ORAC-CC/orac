! CVS:  $Id: comscp2.f,v 1.1 2002/08/28 21:20:20 norm Exp $
! CVS:  $Name:  $


!-----------------------------------------------------------------------

      subroutine comscp2
     +              ( ncol ,    nlm ,     tg , fwcld
     +,              fwclr , tccld1 , tcclr1 , tccld
     +,              tcclr ,  wccld ,  wcclr
     +              )

      use kinds

      implicit none

!-----------------------------------------------------------------------
! REFERENCES:
! comscp2 combines the single scattering properties computed in comscp1
! to the single scattering properties due to non-gray absorption.
! Laura D. Fowler (slikrock. 08-12-97).

! send comments to laura@slikrock.atmos.colostate.edu and
! partain@atmos.colostate.edu

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
! middle of layers. In this subroutine, all the arrays are defined as
! local arrays in BUGSswr.

!     INPUT ARGUMENTS:
!     ----------------
      integer (kind=int_kind), intent(in)::
     &  ncol   !Length of sub-domain..
     &, nlm    !Number of layers.

      real (kind=dbl_kind), dimension(ncol,nlm)::
     &  tg     !Optical depth of non-gray gases                    (-).
     &, fwclr  !Clear-sky single scattering albedo from comscp1    (-).
     &, fwcld  !

!     INPUT/OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), dimension(ncol,nlm)::
     &  tcclr1 !Clear-sky optical depth                            (-).
     &, tccld1 !All-sky optical depth                              (-).
     &, tcclr  !Clear-sky optical depth                            (-).
     &, tccld  !All-sky optical depth                              (-).

!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(ncol,nlm)::
     &  wcclr  !Clear-sky single scattering albedo                 (-).
     &, wccld  !All-sky single scattering albedo                   (-).

! LOCAL LIST VARIABLES:

      integer (kind=int_kind)::
     &  i      !Horizontal index.
     &, l      !Vertical index.

!-----------------------------------------------------------------------

      do l = 1, nlm
         do i = 1, ncol

            tcclr(i,l) = tcclr1(i,l) + tg(i,l)
            tccld(i,l) = tccld1(i,l) + tg(i,l)

            if(tcclr(i,l).gt.0.) then
              wcclr(i,l) = fwclr(i,l)/tcclr(i,l)
            else
              wcclr(i,l) = 0.
            end if
            wcclr(i,l)  = min(.999999_dbl_kind,wcclr(i,l))

            if(tccld(i,l).gt.0.) then
               wccld(i,l) = fwcld(i,l)/tccld(i,l)
            else
               wccld(i,l) = 0.
            end if
            wccld(i,l) = min(.999999_dbl_kind,wccld(i,l))

         end do
      end do

      return
      end subroutine comscp2

c-----------------------------------------------------------------------
