

! CVS: $Id: cloudg.F,v 1.8 2006/11/16 19:54:12 norm Exp $
! CVS: $Name:  $

!-----------------------------------------------------------------------

      subroutine cloudg
     +              (  ncol ,  nlm ,    mb ,   ib
     +,                  pp ,   tt , wcont ,   re
     +,               pdist , cnrw ,  cniw , cnri
     +,                cnii , xlam ,  tcld , wcld
     +,              asycld , flag
     +              )

      use kinds

      implicit none

!-----------------------------------------------------------------------
! REFERENCES:
! Cleaned up version of cloud.f from G. Stephens. Computes the cloud
! optical properties.

! This routine has been modified to include the the corrections to
! ADT for spherical particles based upon the work of David Mitchell
! DRI.  All the derivations have been carried out for the modified
! gamma distribution assuming that m=0.5 (em in program), a 
! parameter in eqn (5) of Mitchell (1994).
 
! tcld, wcld, asycld are the optical depth, single scattering albedo,
! and asymmetry parameter of cloud particles based on the use of
! ADT theory as used by Stephens et al (1990). Effective radius re  
! is input (in microns) and the water content is in g/m3.  The logical
! variable flag is .false. for water and .true. for ice.

! send comments to laura@slikrock.atmos.colostate.edu and
! partain@atmos.colostate.edu.

! MODIFICATIONS:
! * changed declarations to adapt the code from BUGS4 to BUGS5.
!   Laura D. Fowler/slikrock (02-01-00).

! SUBROUTINES CALLED:
!     none.

! FUNCTIONS CALLED:
!     none

! INCLUDED COMMONS:
!     none.

! ARGUMENT LIST VARIABLES:
!     INPUT ARGUMENTS:
!     ----------------
      logical (kind=log_kind), intent(in)::
     &  flag   !If true, computes optical properties of ice clouds, of
!              of water clouds otherwise.

      integer (kind=int_kind), intent(in)::
     &  ncol   !Length of sub-domain.
     &, nlm    !Number of layers.
     &, mb     !Total number of spectral intervals.
     &, ib     !Index of spectral interval.

      real (kind=dbl_kind), intent(in), dimension(mb)::
     &  cnrw   !Real part of refractive index (Water clouds).
     &, cniw   !Imaginary part of refractive index (Water clouds).
     &, cnri   !Real part of refractive index (Ice clouds).
     &, cnii   !Imaginary part of refractive index (Ice clouds).
     &, xlam   !Center of spectral band.

      real (kind=dbl_kind), intent(in)::
     &  pdist  !

      real (kind=dbl_kind), intent(in), dimension(ncol,nlm)::
     &  tt     !Temperature                                        (K).
     &, wcont  !Cloud water/ice content                       (g/m^-3).
     &, re     !Cloud effective radius                            (mu).      

      real (kind=dbl_kind), intent(in), dimension(ncol,nlm+1)::
     &  pp     !Level pressure                                   (hPa).
     
!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(ncol,nlm)::
     &  tcld   !Cloud optical depth                                (-).
     &, wcld   !Cloud single scattering albedo                     (-).
     &, asycld !Cloud asymmetry factor                             (-).

! LOCAL VARIABLES:
      complex (kind=dbl_kind)::
     &  cm,um
          
      integer (kind=int_kind)::
     &  i, l

      real (kind=dbl_kind)::
     &    abs ,  area ,    c0 ,  c1
     &,   cnr ,   cni ,    dz , eps
     &,   ext ,    f2 ,    f3 ,  no
     &,    p0 ,    p1 ,    p2 ,  pi
     &,    rm ,    xm ,    vm ,  rho_water
     
!-----------------------------------------------------------------------

!---- initialize local and output arrays:

      tcld(:,:)   = 0.
      wcld(:,:)   = 0.
      asycld(:,:) = 1.

!--   initialize miscellaneous constants and indices of refraction:

      pi  = acos(-1.)
      eps = 1.e-5
      
      if(flag) then
         !Ice case
         cnr = cnri(ib)
         cni = cnii(ib)
         rho_water = 1.0e6   !gm^-3
      else
         cnr = cnrw(ib)
         cni = cniw(ib)
         rho_water = 1.0e6   !gm^-3
      endif

!--   constants depending upon the characteristic width of the distribu
!     tion.(these may be made to vary with hydrometeor species and thus 
!     pdist could be made to depend upon level and column numbers).

!     p0    = 0.
      p0    = pdist
      p1    = p0 + 1.
      p2    = p0 + 2.
      f2    = p1 * p0
      f3    = p2 * f2

!---- calculate cloud optical properties:

      do l = 1, nlm
         do i = 1, ncol 
            if(wcont(i,l) .gt. eps) then
               dz=29.286*log(pp(i,l+1)/pp(i,l)) * tt(i,l)
               rm = re(i,l)/p2
               no = wcont(i,l) / ( (4.*pi/3.)*f3*rho_water*rm**3 )  !Particles per cubic micrometer
               area = pi*f2*no*rm**2*1.0e6                          !The factor converts inverse micrometers,
                                                                    !i.e., micrometer^2/micrometer^3, to inverse meters
               c0 = 2.*area
               c1 = c0/f2
               xm = 2.*pi*rm/xlam(ib)
               cm = cmplx(cnr,-cni)

               if (ib .eq. 1) then
                  !For band 1 (0.5 um) only compute the extinction.
                  um = 2.*xm*(cnr-1.)*cmplx(0.d0,1.d0)
                  ext = c0 + 2.*c1*real(p0/(um*(um+1.)**p1)
     +                  + 1./(um**2*(um+1.)**p0)-1./um**2)
                  tcld(i,l) = ext*dz
                  wcld(i,l) = 0.999999
                  asycld(i,l) = 0.85
               else
                  !Compute both extinction and absorption coefficients for all other bands.
                  um = 2.*xm*(cm-1.)*cmplx(0.d0,1.d0)
                  ext = c0 + 2.*c1*real( p0/(um*(um+1.)**p1)
     +                  + 1./(um**2*(um+1.)**p0)-1./um**2)
                  vm = 4.*xm*cni
                  abs = area + c1*sngl( p0/(vm*(vm+1.)**p1)
     +                    + 1./(vm**2*(vm+1.)**p0) - 1./vm**2 )
!                 abs = area + c1*sngl(
!     +              p0/(dble(vm)*(dble(vm)+1.)**dble(p1))
!     +            + 1./(dble(vm)**2*(dble(vm)+1.)**dble(p0))
!     +            - 1./dble(vm)**2)
                 tcld(i,l) = ext*dz

                 if (ext.lt.abs) ext = abs
                 wcld(i,l) = (ext-abs)/ext

                 if(wcld(i,l) .lt. 0.) then
                      print *,wcld(i,l),ext,abs,wcont(i,l)
                      print *,pp(i,l),pp(i,l+1)
                      print *,tt(i,l)
                      print *,re(i,l)
                      stop
                 endif
               endif
               asycld(i,l)=0.85  !default, overridden in bugs_swr(), bugs_lwr()
             endif
         enddo
      enddo

      return
      end subroutine cloudg
!-----------------------------------------------------------------------
