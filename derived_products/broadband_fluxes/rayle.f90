

! CVS:  $Id: rayle.F90,v 1.2 2007/05/30 20:26:31 norm Exp $
! CVS:  $Name:  $
module rayleigh
contains
!-----------------------------------------------------------------------

subroutine rayle (&
    nlm          & !Number of layers
   ,ib           & !Index of spectral interval
   ,pp           & !Level pressure, hPa
   ,tray         & !Rayleigh optical depth
   ,wray)          !Rayleigh single-scattering albedo

   use kinds
   use bugsrad_physconst, only:  n_av, gravity, mw_dry_air, ri

   implicit none

!-----------------------------------------------------------------------
! REFERENCES:
! Multitasked version of rayle.f from G. Stephens. Rayle computes the
! optical depth and single scattering albedo due to Rayleigh scattering.
! Laura D. Fowler (slikrock. 08-20-97)

! send comments to laura@slikrock.atmos.colostate.edu and
! partain@atmos.colostate.edu

! MODIFICATIONS:
! * changed declarations to adapt the code from BUGS4 to BUGS5.
!   Laura D. Fowler/slikrock (02-01-00).
! 22 Nov 2005
!   Updated Rayleigh optical depth calculation.  Now uses band-averaged
!   Rayleigh scattering cross-sections following Bodhaine et al. 1999
!   Band average values are weighted by a top-of-atmosphere solar
!   spectrum obtained from Modtran 4.1 (see Anderson et al., 2000),
!   and based on Kurucz, 1995
!


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
      nlm  & !Number of layers.
     ,ib     !Index of spectral interval.

      real (kind=dbl_kind), intent(in), dimension(:,:):: &  !(ncol,nlm+1)
     pp   !Level pressure                                     (hPa).

!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(:,:):: & !(ncol,nlm)
     wray  & !Rayleigh single scattering albedo                    (-).
    ,tray    !Rayleigh optical depth                               (-).

! LOCAL VARIABLES:
      integer (kind=int_kind):: &
     i  & !Horizontal index.
    ,l    !Vertical index.
      real (kind=dbl_kind) :: &
     fact                       !multiplier derived from scattering cross-section
                                !for scaling from pressure to optical depth

!-----------------------------------------------------------------------

!---- computes rayleigh scattering:
!     The factor of 10. adjusts units from g-cm to kg-m and takes into
!     account that pressure is in hPa.
!---- Do top level first
      fact=ri(ib)*n_av/(mw_dry_air*gravity)*10.
      tray(:,1) = pp(:,2)*fact
      !Now remaining levels
      do l = 2, nlm
         tray(:,l) = (pp(:,l+1)-pp(:,l))*fact
      end do
      wray(:,:) = 1.0
      return
      end subroutine rayle
end module rayleigh

!-----------------------------------------------------------------------
