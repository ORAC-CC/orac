

! CVS:  $Id: comscp1.F,v 1.3 2001/04/30 08:48:56 norm Exp $
! CVS:  $Name:  $

!-----------------------------------------------------------------------

      subroutine comscp1
     +              (   ncol ,     nlm ,   taer ,  tcldi
     +,                tcldw ,     tgm ,   tray ,   waer
     +,                wcldi ,   wcldw ,   wray , asyaer
     +,              asycldi , asycldw , tccld1 , tcclr1
     +,               asycld ,  asyclr ,  fwcld ,  fwclr
     +              )

      use kinds

      implicit none
             
!-----------------------------------------------------------------------
! REFERENCES:
! comscp1 combines single scattering properties due to Rayleigh absorp
! tion, aerosols, water continuum, gray gaseous absorption, ice crystals
! and water droplets. 
! Laura D. Fowler/slikrock (08-12-97).

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
     &  ncol    !Length of sub-domain..
     &, nlm     !Number of layers.
     
      real (kind=dbl_kind), intent(in), dimension(ncol,nlm)::
     &  asyaer  !Asymmetry factor of aerosols                      (-).
     &, asycldi !Asymmetry factor of ice clouds                    (-).
     &, asycldw !Asymmetry factor of water clouds                  (-).
     &, taer    !Optical depth of aerosols                         (-).
     &, tcldi   !Optical depth of ice clouds                       (-).
     &, tcldw   !Optical depth of water clouds                     (-).
     &, tgm     !Optical depth of water vapor continuum            (-).
     &, tray    !Optical depth due to Rayleigh absorption          (-).
     &, waer    !Single scattering albedo of aerosols              (-).
     &, wcldi   !Single scattering albedo of ice clouds            (-).
     &, wcldw   !Single scattering albedo of water clouds          (-).
     &, wray    !Single scattering albedo due to Rayleigh
!                absorption                                        (-).

!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(ncol,nlm)::
     &  asyclr  !Clear-sky asymmetry factor                        (-).
     &, asycld  !All-sky asymmetry factor                          (-).
     &, tcclr1  !Clear-sky optical depth                           (-).
     &, tccld1  !All-sky optical depth                             (-).
     &, fwclr   !Total clear-sky single-scattering albedo          (-).
     &, fwcld   !Total cloudy single-scattering albedo             (-).

! LOCAL LIST VARIABLES:    

      integer (kind=int_kind)::
     &  i       !Horizontal index.
     &, l       !Vertical index.

      real (kind=dbl_kind)::
     &  wwray,wwaer,wwcldi,wwcldw
     
!-----------------------------------------------------------------------

      do l = 1, nlm
         do i = 1, ncol

            tcclr1(i,l) = tgm(i,l) + tray(i,l) + taer(i,l)
            tccld1(i,l) = tcclr1(i,l)+ tcldi(i,l) + tcldw(i,l)

            wwray  = wray(i,l)*tray(i,l)
            wwaer  = waer(i,l)*taer(i,l)
            wwcldi = wcldi(i,l)*tcldi(i,l)
            wwcldw = wcldw(i,l)*tcldw(i,l)

            fwclr(i,l)  = wwray+wwaer
            fwcld(i,l)  = fwclr(i,l)+wwcldi+wwcldw
 
            if(fwclr(i,l).gt.1.e-10) then
               asyclr(i,l) = (asyaer(i,l)*wwaer)/fwclr(i,l)
            else
               asyclr(i,l) = 1.
            endif
 
            if(fwcld(i,l).gt.1.e-10) then
               asycld(i,l) = (asyaer(i,l)*wwaer+asycldi(i,l)*wwcldi
     +                     +  asycldw(i,l)*wwcldw)
     +                     / fwcld(i,l)
            else
               asycld(i,l) = 1.
            endif

         enddo
      enddo

      return
      end subroutine comscp1

!-----------------------------------------------------------------------
