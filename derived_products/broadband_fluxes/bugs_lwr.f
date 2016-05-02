

! CVS:  $Id: bugs_lwr.F,v 1.7 2006/11/16 18:45:09 norm Exp $
! CVS:  $Name:  $
! Modified for new version of planck function

!-----------------------------------------------------------------------

      subroutine bugs_lwr
     +                (    ncol ,    nlm ,    pp ,    ppl
     +,                      dp ,     tt ,  rmix ,  cwrho
     +,                   cirho ,  o3mix ,    ts , cldamt
     +,                  cldmax ,     b1 ,    b2 ,     b3
     +,                      b4 ,  umco2 , umch4 ,  umn2o
     +,                    fdlw , fdlwcl ,  fulw , fulwcl
     +,               sel_rules , resat
     +,               emis)

      use kinds
      use bugsrad_planck, only:  planck
      use gases_ckd, only:  gases, stanpir,pscale
      use continuum
      implicit none

!-----------------------------------------------------------------------
! REFERENCES:
! bugs_lwr replaces crclwr written by G. Stephens. bugs_lwr computes the
! downward and upward longwave radiative fluxes, and longwave heating
! rates.
! Laura D. Fowler (slikrock/08-23-96).

! send comments to laura@slikrock.atmos.colostate.edu and
! partain@atmos.colostate.edu.

! MODIFICATIONS:
! * moved the computation of the all-sky and clear-sky radiative heating
!   rates to bugs_rad.
!   Laura D. Fowler and Phil Partain/slikrock (01-27-99).

! * added effective radii of cloud droplets and ice crystals that are
!   dependent on the cloud water and cloud ice contents.
!   Laura D. Fowler/slikrock (06-08-00).

! * cleaned up the argument list to remove variables related to short
!   wave radiative transfer.
!   Laura D. Fowler/slikrock (02-01-00).

! * changed declarations to adapt the code from BUGS4 to BUGS5.
!   Laura D. Fowler/slikrock (02-01-00).

! SUBROUTINES CALLED:

!     pscale         : pressure scaling.
!     cloudg          : computes cloud optical properties of water/ice
!                       clouds.
!     gascon_ckd_parm : water vapor continuum absorption.
!     plank           : computes planck function.
!     comscp1         : combines optical properties for gray absorption
!                       (clouds and water vapor continuum).
!     comscp2         : combines optical properties for non-gray gaseous
!                       absorption.
!     gases           : computes gases absorption.
!     two_rt_lw       : two-stream parameterization.

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

      real (kind=dbl_kind), intent(in)::
     &  umco2 !Concentration of CO2                              (ppm).
     &, umch4 !Concentration of CH4                              (???).
     &, umn2o !Concentration of N2o                              (???).

      real (kind=dbl_kind), intent(in),  dimension(ncol)::
     &  ts    !Surface temperature                                 (K).
     &, cldmax!Maximum cloud fraction                              (-).

      real (kind=dbl_kind), intent(in), dimension(ncol,nlm)::
     &  ppl!Layer pressure                                       (hPa).
     &, dp    !Layer thickness                                   (hPa).
     &, tt    !Temperature                                         (K).
     &, rmix  !Water vapor mixing ratio                        (kg/kg).
     &, cwrho !Cloud water mixing ratio                        (g/m^3).
     &, cirho !Cloud ice mixing ratio                          (g/m^3).
     &, o3mix !Ozone mixing ratio                              (kg/kg).
     &, cldamt!Cloud fraction                                      (-).
     &, b1    !Cloud overlap parameter                             (-).
     &, b2    !Cloud overlap parameter                             (-).
     &, b3    !Cloud overlap parameter                             (-).
     &, b4    !Cloud overlap parameter                             (-).

      real (kind=dbl_kind), intent(in), dimension(ncol,nlm+1)::
     &  pp    !Level pressure                                    (hPa).

      real, intent(in) :: resat !input satellite retrieved cloud effective radius (um)
      real, intent(in) :: emis(12) !Spectral surface emissivity for each LW band

!     OUTPUT ARGUMENTS:
!     -----------------
      real (kind=dbl_kind), intent(out), dimension(ncol,nlm+1)::
     &  fdlw  !Downward LW flux                                (W/m^2).
     &, fdlwcl!Downward clear-ksy LW flux                      (W/m^2).
     &, fulw  !Upward LW flux                                  (W/m^2).
     &, fulwcl!Upward clear-sky LW flux                        (W/m^2).

! LOCAL VARIABLES:

      integer (kind=int_kind)::
     &  mb    !Total number of spectral intervals.
     &, mbs   !Number of shortwave (SW) spectral intervals.
     &, mbir  !Number of shortwave (LW) spectral intervals.
      parameter(mb=18,mbs=6,mbir=12)

      integer (kind=int_kind)::
     &  i     !Horizontal index.
     &, l     !Vertical index.
     &, ib    !Index of spectral interval.
     &, ig    !Index of k-distribution.
     &, ibmbs !Index of LW spectral interval.

      integer (kind=int_kind), dimension(ncol,nlm)::
     &  ip1   !Used in conjunction with pressure weigthing.
     &, ip2   !Used in conjunction with pressure weigthing.

      real (kind=dbl_kind)::
     &  hk       !Weighted spectral solar constant             (W/m^2).  .
     &, tmax     !Temperature threshold                            (K).                        .
     &, eps      !Threshold for cloud optical properties              .
     &, pdist
      data eps,tmax,pdist /1.e-05,340.,2./

      integer (kind=int_kind), dimension(mbir)::
     &  kg       !Nb of k-distributions per spectral intervals.
      data kg /2,3,4,4,3,5,2,10,12,7,7,8/

      real (kind=dbl_kind), dimension(mbir)::
     &  asym_wat !Spectral asymmetry factor of water clouds.
     &, asym_ice !Spectral asymmetry factor of ice clouds.

      real (kind=dbl_kind), dimension(mb)::
     &  cnrw    !Real part of refractive index (Water clouds).
     &, cniw    !Imaginary part of refractive index (Water clouds).
     &, cnri    !Real part of refractive index (Ice clouds).
     &, cnii    !Imaginary part of refractive indec (Ice clouds).
     &, xlam    !Center of spectral band.

      real (kind=dbl_kind), dimension(ncol,mbir)::
     & es       !Spectral surface emissivity                       (-).

      real (kind=dbl_kind), dimension(ncol,nlm)::
     &  rew     !Effective radius for cloud water                 (mu).
     &, rei     !Effective radius for cloud ice                   (mu).
     &, ttem    !Local temperature                                 (K).
     &, pkd     !
     &, tau1    !All-sky optical depth                             (-).
     &, tauclr1 !Clear-sky optical depth                           (-).
     &, tau     !All-sky optical depth                             (-).
     &, tauclr  !Clear-sky optical depth                           (-).
     &, taer    !Aerosol optical depth                             (-).
     &, tray    !Rayley optical depth                              (-).
     &, tg      !Gases optical depth                               (-).
     &, tgm     !WV continuum optical depth                        (-).
     &, tcldi   !Ice cloud optical depth                           (-).
     &, tcldw   !Water cloud optical depth                         (-).
     &, wc      !All-sky single scattering albedo                  (-).
     &, wcclr   !Clear-sky single scattering albedo                (-).
     &, waer    !Aerosol single scattering albedo                  (-).
     &, wray    !Rayley single scattering albedo                   (-).
     &, wcldi   !Ice cloud single scattering albedo                (-).
     &, wcldw   !Water cloud single scattering albedo              (-).
     &, asym    !All-sky asymmetry factor                          (-).
     &, asyclr  !Clear-sky asymmetry factor                        (-).
     &, asyaer  !Aerosol asymmetry factor                          (-).
     &, asycldi !Ice cloud asymmetry factor                        (-).
     &, asycldw !Water cloud asymmetry factor                      (-).
     &, fwclr   !
     &, fwcld   !

      real (kind=dbl_kind), dimension(ncol,nlm+1)::
     &  bf      !Planck function for layers                    (W/m^2).
     &, fdg     !Spectral downward flux                        (W/m^2).
     &, fdgcl   !Spectral clear-sky downward flux              (W/m^2).
     &, fug     !Spectral upward flux                          (W/m^2).
     &, fugcl   !Spectral clear-sky upward flux                (W/m^2).

!     longwave asymmetry parameters:
!     (assumes: re=10 for water; re=30 for ice)
      data asym_wat /0.8200, 0.8547, 0.8619, 0.8683, 0.8723, 0.8703
     +,              0.8566, 0.8040, 0.7463, 0.6579, 0.5103, 0.1279 /
      data asym_ice /0.8524, 0.8791, 0.9022, 0.8797, 0.8637, 0.8722
     +,              0.8609, 0.8168, 0.7663, 0.6584, 0.6172, 0.3585 /

!---  cnrw and cniw (water clouds):
      data cnrw/1.3422,1.3281,1.3174,1.2901,1.3348,1.3700,1.3191,1.2821
     &,         1.3160,1.3030,1.2739,1.2319,1.1526,1.1981,1.3542,1.4917
     &,         1.5463,1.8718/
      data cniw/6.4790e-9,1.3417e-06,1.2521e-4,7.1533e-4,4.2669e-2
     &,         4.3785e-3,1.3239e-2 ,1.5536e-2,5.3894e-2,3.4346e-2
     &,         3.7490e-2,4.7442e-2 ,1.2059e-1,3.3546e-1,4.1698e-1
     &,         4.0674e-1,3.6362e-1 ,5.2930e-1/

!--- cnri and cnii (ice clouds):
      data cnri/1.3266,1.2986,1.2826,1.2556,1.2963,1.3956
     &,         1.3324,1.2960,1.3121,1.3126,1.2903,1.2295
     &,         1.1803,1.5224,1.5572,1.5198,1.4993,1.7026/
      data cnii/7.0696e-9,9.1220e-7,1.2189e-4,5.7648e-4,4.3144e-2
     &,         8.2935e-3,1.5540e-2,2.5594e-2,5.9424e-2,5.1511e-2
     &,         4.0325e-2,4.7994e-2,2.3834e-1,3.0697e-1,1.1852e-1
     &,         4.3048e-2,6.3218e-2,1.5843e-1/

!---- spectral band center:
      data xlam/0.45  ,1.0   ,1.6  ,2.2  ,3.0   ,3.75  ,4.878 ,5.556
     &,         6.452 ,7.547 ,8.511,9.615,11.236,13.605,16.529,21.277
     &,         29.412,71.403/

!-----------------------------------------------------------------------

!---- 0. initialize output arrays:

      fdlw(:,:)   = 0.
      fdlwcl(:,:) = 0.
      fulw(:,:)   = 0.
      fulwcl(:,:) = 0.

      !rew(:,:)    = 10.
      !rei(:,:)    = 30.
      rew(:,:)    = resat
      rei(:,:)    = resat
      !print*,'lwrwat: ',rew
      !print*,'lwrice: ',rei

      do l = 1, nlm
         do i = 1, ncol
            ttem(i,l) = min(tmax,tt(i,l))
          end do
      end do

!---- note: this will be changed to accomodate the spectral dependence
!     the surface emissivity:
! old code that uses surface emissivity value of 1
!      do ib = 1, mbir
!         do i = 1, ncol
!            es(i,ib) = 1.
!         end do
!      end do
! NEW CODE that inputs spectral emissivity
! Matt Christensen 02/24/16
      do i = 1, ncol
        es(i,:) = emis(:)
      end do

!--   pressure scaling:

       call pscale(ncol,nlm,ppl,stanpir,pkd,ip1,ip2)

!---- 1. loop over the mbir spectral intervals starts here:

      do ib = mbs+1, mb
         ibmbs = ib - mbs

         tray(:,:)   = 0.
         wray(:,:)   = 0.
         taer(:,:)   = 0.
         waer(:,:)   = 0.
         asyaer(:,:) = 1.

!---- 1.1 optical properties of water and ice clouds (as in crclwr for
!        now):

         call cloudg
     +           (   ncol ,    nlm  ,    mb ,    ib
     +,                pp ,     tt  , cwrho ,   rew
     +,             pdist ,   cnrw  ,  cniw ,  cnri
     +,              cnii ,   xlam  , tcldw , wcldw
     +,           asycldw , .false.
     +           )

         call cloudg
     +           (   ncol ,   nlm   ,    mb ,    ib
     +,                pp ,    tt   , cirho ,   rei
     +,             pdist ,  cnrw   ,  cniw ,  cnri
     +,              cnii ,  xlam   , tcldi , wcldi
     +,           asycldi , .true.
     +           )

!     the asymmetry factor for water and ice clouds are fixed as for now
!     functions of the spectral intervals:

         do l = 1, nlm
            do i = 1, ncol
               if(cwrho(i,l) .ge. eps) asycldw(i,l) = asym_wat(ibmbs)
               if(cirho(i,l) .ge. eps) asycldi(i,l) = asym_ice(ibmbs)
            end do
         end do

!---- 1.2 water vapor continuum:

         call gascon
     +           (ncol , nlm, ib ,   pp
     +,            ppl ,  dp, tt , rmix
     +,            tgm
     +           )

!---- 1.3 planck function:
         call planck(ncol,nlm,ibmbs,ts,tt,bf)

!---- 1.4 combines single-scattering properties for gray absorption:

         call comscp1
     +           (   ncol ,     nlm ,  taer ,   tcldi
     +,             tcldw ,     tgm ,  tray ,    waer
     +,             wcldi ,   wcldw ,  wray ,  asyaer
     +,           asycldi , asycldw ,  tau1 , tauclr1
     +,              asym ,  asyclr , fwcld ,   fwclr
     +           )

!---- loop over the k-probability distributions starts here:

         do ig = 1, kg(ibmbs)

!---- 1.5 gaseous absorption:

            call gases
     +              ( ncol ,   nlm ,    ib ,    ig
     +,                 pp ,    dp ,    tt ,  rmix
     +,              o3mix , umco2 , umch4 , umn2o
     +,                 hk ,    tg ,   pkd ,   ip1
     +,                ip2
     +              )

!---- 1.6 combines all single-scattering properties:

            call comscp2
     +              (  ncol ,  nlm ,      tg , fwcld
     +,               fwclr , tau1 , tauclr1 ,   tau
     +,              tauclr ,   wc ,   wcclr
     +              )


      !NBW - Minor (?) bug fix
      !With near-zero CO2 and very low water vapor amounts, the
      !correlated-K parameterization can generate negative optical
      !depths in the CO2-H2O overlap bands.  Here's a quick fix:
            where (tau .lt. 0)
               tau = 0.
            endwhere
            where (tauclr .lt. 0)
               tauclr = 0.
            endwhere

!---- 1.7 two-stream approximation:
! No overlap
            call two_rt_lw
     +              (     ncol , nlm ,  mbs , mbir
     +,                     ib ,  wc , asym ,  tau
     +,                     es ,  bf ,  fug ,  fdg
     +,              sel_rules
     +              )

            call two_rt_lw
     +              (     ncol ,   nlm ,   mbs  ,   mbir
     +,                     ib , wcclr , asyclr , tauclr
     +,                     es ,    bf ,  fugcl ,  fdgcl
     +,              sel_rules
     +              )
            fdlw(:,:)   = fdlw(:,:)   + fdg(:,:)*hk
            fulw(:,:)   = fulw(:,:)   + fug(:,:)*hk
            fdlwcl(:,:) = fdlwcl(:,:) + fdgcl(:,:)*hk
            fulwcl(:,:) = fulwcl(:,:) + fugcl(:,:)*hk

         end do ! end k-distribution

      end do ! end spectral interval


      return
      end subroutine bugs_lwr

c-----------------------------------------------------------------------
