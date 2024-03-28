!-------------------------------------------------------------------------------
! Name: driver_for_fuliou.F90
!
! Purpose:
!
! Inputs:
!
! Output:
!
! History:
! xxxx/xx/xx, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine driver_for_fuliou(nlm,tsi,theta, &
                             asfcswrdr,asfcnirdr,asfcswrdf,asfcnirdf,tsfc, &
                             phaseflag,nlayers,cref,ccot,hctop,hcbase, &
                             hctopID,hcbaseID, &
                             pxZ,pxP,pxT,pxQ,pxO3, &
                             toalwup,toaswdn,toaswup, &
                             boalwup,boalwdn,boaswdn,boaswup, &
                             toalwupclr,toaswupclr, &
                             boalwupclr,boalwdnclr,boaswupclr,boaswdnclr, &
                             boapar,boapardif,toapar, &
                             fulw,fdlw,fusw,fdsw, &
                             fulwcl,fdlwcl,fuswcl,fdswcl, &
                             emis,rho0d,rhodd,FuSolverMode)

#ifdef INCLUDE_FU_LIOU_SUPPORT
   use FULIOUMULTI
   use GENERATE_FULIOU_LEVELS, only : gflq, generate_level_scheme
   use EXTRAS, only : getatmosphere, aer_scale_hgt
   use CALIPSO_OUTPUT, only : pack_sky, print_pack_sky, skyp,SKYP_TYPE

   use ICEDIRSFC,only: tau_uc ! Debug Diagnostic

   implicit none

   ! Input arguments

   ! Model setup & cloud locations
   integer, intent(in) :: &
      nlm, &  ! Number of vertical layers. (-).
      nlayers ! Number of cloud layers

   integer, intent(in) :: &
      hctopID(nlayers), & ! vertical level index for Hctop  (-).
      hcbaseID(nlayers)   ! vertical level index for Hcbase (-).

   integer, intent(in) :: FuSolverMode

   real, intent(in) :: &
      phaseflag(nlayers), & ! cloud phase=0 clear, = 1water, =2ice (-).
      cref(nlayers)     , & ! satellite cloud effective radius     (um).
      ccot(nlayers)     , & ! satellite cloud optical depth        (-).
      hctop(nlayers)    , & ! satellite cloud top height           (km).
      hcbase(nlayers)       ! satellite cloud base height          (km).

   ! Column quantities
   real, intent(in) :: &
      tsi      , & ! total solar irradiance              (W/m2).
      theta    , & ! cosine of solar zenith angle         (-).
      asfcswrdr, & ! DIRECT visible surface albedo        (-).
      asfcnirdr, & ! DIRECT near infrared surface albedo  (-).
      asfcswrdf, & ! DIFFUSE visible surface albedo       (-).
      asfcnirdf, & ! DIFFUSE near infrared surface albedo (-).
      tsfc         ! surface temperature

   ! Meteorological profiles
   real, intent(in), dimension(nlm+1) :: &
      pxZ, & ! geopotential height profile at SAT. pixel (km).
      pxP, & ! pressure profile at SAT. pixel            (hPa).
      pxT, & ! temperature profile at SAT. pixel         (K).
      pxQ, & ! specific humidity profile at SAT. pixel   (kg/kg).
      pxO3   ! Ozone mixing ratio at SAT. pixel          (kg/kg).

   real, intent(in) :: emis(12)  ! Spectral surface emissivity for each LW band
   real, intent(in) :: rho0d(18) ! Spectral direct surface albedo for each SW band
   real, intent(in) :: rhodd(18) ! Spectral diffuse surface albedo for each SW band

   ! Output arguments
   real, intent(out) :: &
     toalwup   ,& ! All-sky TOA LW upwelling flux          (W/m2).
     toaswdn   ,& ! All-sky TOA SW upwelling flux          (W/m2).
     toaswup   ,& ! All-sky TOA SW upwelling flux          (W/m2).
     boalwup   ,& ! All-sky BOA LW upwelling flux          (W/m2).
     boalwdn   ,& ! All-sky BOA LW upwelling flux          (W/m2).
     boaswdn   ,& ! All-sky BOA SW upwelling flux          (W/m2).
     boaswup   ,& ! All-sky BOA SW upwelling flux          (W/m2).
     toalwupclr,& ! CLEAR-sky TOA LW upwelling flux        (W/m2).
     toaswupclr,& ! CLEAR-sky TOA SW upwelling flux        (W/m2).
     boalwupclr,& ! CLEAR-sky BOA LW upwelling flux        (W/m2).
     boaswupclr,& ! CLEAR-sky BOA SW upwelling flux        (W/m2).
     boaswdnclr,& ! CLEAR-sky BOA SW upwelling flux        (W/m2).
     boalwdnclr,& ! CLEAR-sky BOA LW upwelling flux        (W/m2).
     boapar    ,& ! All-sky BOA PAR downwelling flux       (W/m2).
     boapardif ,& ! All-sky BOA PAR dowelling diffuse flux (W/m2).
     toapar       ! All-sky TOA PAR downwelling flux       (W/m2).

   real(kind=8), intent(out), dimension(1,nlm+1) :: &
     fulw  ,& ! All-sky LW upwelling flux     (W/m^2).
     fdlw  ,& ! All-sky LW downwelling flux   (W/m^2).
     fusw  ,& ! All-sky SW upwelling flux     (W/m^2).
     fdsw  ,& ! All-sky SW downwelling flux   (W/m^2).
     fulwcl,& ! CLEAR-sky LW upwelling flux   (W/m^2).
     fdlwcl,& ! CLEAR-sky LW downwelling flux (W/m^2).
     fuswcl,& ! CLEAR-sky SW upwelling flux   (W/m^2).
     fdswcl   ! CLEAR-sky SW downwelling flux (W/m^2).

!  real, dimension(19) :: bands_fuliou_sw
!  real, dimension(13) :: bands_fuliou_lw
!  real, dimension(7)  :: bands_bugsrad_sw
!  real, dimension(13) :: bands_bugsrad_lw

   ! Local variables
   type (SKYP_TYPE) :: ut,tu
   integer :: i,k
   real :: psfc

   call set_default_options_fu ! Sets some of the more obscure inputs to
                               ! reasonable values.

   ! BugsRad bands
   ! bands_bugsrad_sw = [0.2,0.689,1.299,1.299,2.50,3.509,4.0]
   ! bands_bugsrad_lw = [4.54,5.26,5.88,7.14,8.,9.09,10.2,12.5,14.9,18.5,25.,35.7,10000.]

   ! Fu-liou bands
   ! bands_fuliou_sw = [0.1754,0.2247,0.2439,0.2857,0.2985,0.3225,0.3575,0.4375, &
   !                    0.4975,0.595,0.69,0.794,0.889,1.042,1.41,1.9048,2.5,3.5088,4.0]
   ! bands_fuliou_lw = [4.54,5.26,5.88,7.14,8.,9.09,10.2,12.5,14.9,18.5,25.,35.7,10000.]

   ! Assign missing values input variables
   do k = 1, 4
      fo(k)%exist      = .false. ! I think this is the only one that is actually needed
      fo(k)%fuwn       =-999.
      fo(k)%fdwn       =-999.
      fo(k)%fds        =-999.
      fo(k)%fus        =-999.
      fo(k)%fdir       =-999.
      fo(k)%fuir       =-999.
      fo(k)%fuwn       =-999.
      fo(k)%fdwn       =-999.
      fo(k)%fdsdr      =-999.
      fo(k)%fdsdf      =-999.
      fo(k)%fiurt      =-999.
      fo(k)%fiurw      =-999.
      fo(k)%fd         =-999.
      fo(k)%fu         =-999.
      fo(k)%dts        =-999.
      fo(k)%dtir       =-999.
      fo(k)%dt         =-999.
      fo(k)%fu_sr      =-999.
      fo(k)%fu_sf      =-999.
      fo(k)%trwn_flt_r =-999.
      fo(k)%trwn_unf_r =-999.
      fo(k)%trwn_f     =-999.
      fo(k)%LW_entropy =-999.
      fo(k)%SW_entropy =-999.
      ftoa(k)%olr      = -999.
      ftoa(k)%swdn     = -999.
      ftoa(k)%swup     = -999.
      ftoa(k)%wnolr    = -999.
      ftoa(k)%totrad   = -999.
      ftoa(k)%winrad   = -999.
      fsfc(k)%lwup     = -999.
      fsfc(k)%lwdn     = -999.
      fsfc(k)%swup     = -999.
      fsfc(k)%swdn     = -999.
      fsfc(k)%swpar    = -999.
      fsfc(k)%swdif    = -999.
      fsfc(k)%swdir    = -999.
      fouv(k)%toa_par  = -999.
   end do

   ! Assign missing values output variables
   toalwup    = -999.
   toaswdn    = -999.
   toaswup    = -999.
   boalwup    = -999.
   boalwdn    = -999.
   boaswup    = -999.
   boaswdn    = -999.
   toalwupclr = -999.
   toaswupclr = -999.
   boalwupclr = -999.
   boalwdnclr = -999.
   boaswdnclr = -999.
   boaswupclr = -999.
   boapar     = -999.
   boapardif  = -999.
   toapar     = -999.

   ! Set Modes (TOTAL-SKY{2-AEROSOL & CLOUD}, PRISTINE{3-NOAEROSOL OR CLOUD})
   fi%lscm(1:4)=(/.false.,.true.,.true.,.false./)

!  fi%HYBRID_SW_SOLVER = .false. !user settings only

   ! 4-STREAM SOLVER
   if (FuSolverMode .eq. 3) then
      fi%isksolve= 0
      fi%fourssl=.true.
   end if

   ! Gamma-weighted 2-stream solver
   if (FuSolverMode .eq. 2) then
      fi%isksolve= 1
   end if

   ! 2-stream solver
   if (FuSolverMode .eq. 4) then
      fi%isksolve= 0
      fi%fourssl=.false.
   end if

   ! Cloud microphysics
   fi%txt=0  ! (Ice 1=smooth, 2=rough, 0=fu1996)
   fi%wp_hgt_flag = 0 ! (0=constant IWC w/height,
                      !  1=linearly increase,
                      !  2=decrease linear,
                      !  3=decrease with height parameterized Calipso profile)

   ! Solar path
   fi%curvedearth= .true.

   ! InPut profile assignment
!  FI%VI%hsfc = 0.00 ! surface geopotential of FI%VI profile

   ! Input CCI profile
   FI%VI%nlev = nlm+1
   FI%VI%pp(1:nlm+1) = pxP(1:nlm+1)
   FI%VI%pt(1:nlm+1) = pxT(1:nlm+1)
   FI%VI%ph(1:nlm+1) = pxQ(1:nlm+1)
   FI%VI%po(1:nlm+1) = pxO3(1:nlm+1)
   FI%pts = tsfc
   gflq%hsfc = pxZ(nlm+1)*1000. ! Meters Surface elev. of ACTUAL FOV... to
                                ! nearest 120m Multiple

   gflq%mode = 'CERES'

   ! Total Solar Irradiance
   fi%ss = tsi ! Solar Constant wm-2
   fi%u0 = theta ! Cosine Solar Zenith Angle
   fi%ur =  0.8 ! Cosine View Zenith Angle (for IR Radiance)


   ! Not being used - original
   if (4 .gt. 5) then
      fi%fc(1)%dpi%ldpi = .false.
      fi%fc(1)%cldfrac  = 1.00000 ! Cloud Fraction (0-1)
      fi%fc(1)%novl     = nlayers

      FI%VD%cldpres(1, 1,1) = pxP(hctopID(1))
      FI%VD%cldpres(2, 1,1) = pxP(hcbaseID(1))
      FI%VD%cldpres(1, 1,2) = pxP(hctopID(2))
      FI%VD%cldpres(2, 1,2) = pxP(hcbaseID(2))

      fi%fc(1)%rphase(1) =  phaseflag(1) ! Cloud Phase 1=Water 2=Ice
      fi%fc(1)%de(1) = cref(1)*2.0
      fi%fc(1)%re(1) = cref(1)

      fi%fc(1)%tau_vis(1) = ccot(1) ! Cloud Visible Optical Depth ( Minnis)
      fi%fc(1)%sc(1)%mn_lin_tau =  fi%fc(1)%tau_vis(1) *1.15


      fi%fc(1)%rphase(2) =  phaseflag(2) ! Cloud Phase 1=Water 2=Ice
      fi%fc(1)%de(2) = cref(2)*2.0
      fi%fc(1)%re(2) = cref(2)

      fi%fc(1)%tau_vis(2)       = ccot(2) ! Cloud Visible Optical Depth ( Minnis)
      fi%fc(1)%sc(2)%mn_lin_tau =  fi%fc(1)%tau_vis(2)


      fi%fc(1)%dpi%ldpi = .false.
      fi%fc(1)%novl     =   nlayers
      fi%fc(1)%cldfrac  = 1.00000 ! Cloud Fraction (0-1)
      do i = i, nlayers

          FI%VD%cldpres(1, 1,i) = pxP(hctopID(i))
          FI%VD%cldpres(2, 1,i) = pxP(hcbaseID(i))

          fi%fc(1)%rphase(i)    =  phaseflag(i) ! Cloud Phase 1=Water 2=Ice
          fi%fc(1)%de(i) = cref(i)*2.0
          fi%fc(1)%re(i) = cref(i)

          fi%fc(1)%tau_vis(i)       = ccot(i) ! Cloud Visible Optical Depth ( Minnis)
          fi%fc(1)%sc(i)%mn_lin_tau =  fi%fc(1)%tau_vis(i) *1.15
      end do
   end if


   ! Clouds
   fi%fc(1)%dpi%ldpi = .false. ! Setting this avoids direct insertion of
                               ! level-by-level IWC/LWC.
   fi%fc(1)%novl     = nlayers ! Number of cloud layers in vertical
   fi%fc(1)%cldfrac  = 1.00000 ! Cloud Fraction (0-1)

   ! Cloud top and bottom pressure
   do i = 1, nlayers
      FI%VD%cldpres(1,1,i) = pxP(hctopID(i)) ! top pressure (hPa)
      FI%VD%cldpres(2,1,i) = pxP(hcbaseID(i)) ! bottom pressure (hPa)
      fi%fc(1)%rphase(i)   =  phaseflag(i) ! Cloud Phase 1=Water 2=Ice
      fi%fc(1)%de(i) = cref(i)*2.
      fi%fc(1)%re(i) = cref(i)
      ! Water clouds cannot have CER > 30. um
      if (phaseflag(i) .eq. 1 .and. cref(i) .ge. 30.) then
         fi%fc(1)%de(i) = 29.9
         fi%fc(1)%re(i) = 29.9
      end if
      fi%fc(1)%asp(i) = exp(0*0.1) ! Fu 20006 Ice AspectRatio !!!!! NEW FOR 20010130
      fi%fc(1)%tau_vis(i)       = ccot(i) ! Cloud Visible Optical Depth (Minnis)
      fi%fc(1)%sc(i)%mn_lin_tau =  fi%fc(1)%tau_vis(i)

      ! No Cloud - need to run despite clear-sky pixel
      if (phaseflag(1) .eq. 0) then
         FI%VD%cldpres(1,1,i) = 100.
         FI%VD%cldpres(2,1,i) = 150.
         fi%fc(1)%rphase(i) =  1
         fi%fc(1)%de(i) = 30.
         fi%fc(1)%re(i) = 15.
         fi%fc(1)%asp(i) = exp(0*0.1)
         fi%fc(1)%tau_vis(i) = 0.00005
         fi%fc(1)%sc(i)%mn_lin_tau = fi%fc(1)%tau_vis(1)
      end if
   end do


!  FI%VD%cldpres(1,1,1) = pxP(hctopID(1)) ! top pressure (hPa)
!  FI%VD%cldpres(2,1,1) = pxP(hcbaseID(1)) ! bottom pressure (hPa)
!  fi%fc(1)%rphase(1)   =  phaseflag(1) ! Cloud Phase 1=Water 2=Ice
!  fi%fc(1)%de(1) = cref(1)*2.
!  fi%fc(1)%re(1) = cref(1)
!  ! Water clouds cannot have CER > 30. um
!  if (phaseflag(1) .eq. 1 .and. cref(1) .ge. 30.) then
!     fi%fc(1)%de(1) = 29.9
!     fi%fc(1)%re(1) = 29.9
!  end if
!  fi%fc(1)%asp(1) = exp(0*0.1) ! Fu 20006 Ice AspectRatio !!!!! NEW FOR 20010130
!  fi%fc(1)%tau_vis(1)       = ccot(1) ! Cloud Visible Optical Depth (Minnis)
!  fi%fc(1)%sc(1)%mn_lin_tau =  fi%fc(1)%tau_vis(1)

   ! No Cloud - need to run despite clear-sky pixel
!  if (phaseflag(1) .eq. 0) then
!     fi%fc(1)%cldfrac   = 1.0
!     FI%VD%cldpres(1,1,1) = 100.
!     FI%VD%cldpres(2,1,1) = 150.
!     fi%fc(1)%rphase(1) =  1
!     fi%fc(1)%de(1) = 30.
!     fi%fc(1)%re(1) = 15.
!     fi%fc(1)%asp(1) = exp(0*0.1)
!     fi%fc(1)%tau_vis(1) = 0.00005
!     fi%fc(1)%sc(1)%mn_lin_tau = fi%fc(1)%tau_vis(1)
!  end if
!  print*,'cloud phase ',fi%fc(1)%rphase(1)
!  print*,'cloud top pressure ',FI%VD%cldpres(1,1,1)
!  print*,'cloud base pressure ',FI%VD%cldpres(2,1,1)
!  print*,'phase flag ',fi%fc(1)%rphase(1)
!  print*,'effective diameter ',fi%fc(1)%de(1)
!  print*,'effective radius ',fi%fc(1)%re(1)
!  print*,'Ice Aspect Ratio ',fi%fc(1)%asp(1)
!  print*,'cloud optical depth ',fi%fc(1)%tau_vis(1)
!  print*,'effective cloud optical depth ',fi%fc(1)%sc(1)%mn_lin_tau


   ! Surface Properties --------------------------------------------------------

   ! Allow different albedos for Aerosol Vs. No aerosol cases, and for each
   ! clear/cloud conditions
   fi%sfcalb(1:18,1,0)  = rho0d(1:18) ! Clear sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,2,0)  = rho0d(1:18) ! Pristine sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,1,1)  = rho0d(1:18)  ! CLOUDY w/AOT  sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,2,1)  = rho0d(1:18)  ! CLOUDY w/o AOT sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,1,2)  = rho0d(1:18)  ! CLOUDY w/AOT  sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,2,2)  = rho0d(1:18)  ! CLOUDY w/o AOT sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,1,3)  = rho0d(1:18)  ! CLOUDY w/AOT  sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,2,3)  = rho0d(1:18)  ! CLOUDY w/o AOT sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,1,4)  = rho0d(1:18)  ! CLOUDY w/AOT  sky -Spectral Surface Albedo SW
   fi%sfcalb(1:18,2,4)  = rho0d(1:18)  ! CLOUDY w/o AOT sky -Spectral Surface Albedo SW

   fi%ee(1:12)  = emis(1:12) ! Spectral Surface Emissivity LW

!  print*,'Surface albedo: ',fi%sfcalb
!  print*,'Surface emissivity ',fi%ee

   ! Aerosols ------------------------------------------------------------------
   if (phaseflag(1) .eq. 0) THEN
      fi%nac              = 1       ! One aerosol constituent is used
      fi%itps(1)          = 2       ! Continental see types (1-18) (SULFATE DROPLETS)
      fi%n_atau           = 1       ! 1 wavelength input for aerosols
      fi%a_wli(1)         = 0.55    ! AOT wavelength(microns) of a_taus
      fi%a_taus(1,1)      = ccot(1) ! AOT for constituent 1
   end if

   if (phaseflag(1) .ne. 0) THEN
      fi%nac              = 1       ! One aerosol constituent is used
      fi%itps(1)          = 2       ! Continental see types (1-18) (SULFATE DROPLETS)
      fi%n_atau           = 1       ! 1 wavelength input for aerosols
      fi%a_wli(1)         = 0.55    ! AOT wavelength(microns) of a_taus
      fi%a_taus(1,1)      = 0.0005  ! AOT for constituent 1
   end if

   call generate_level_scheme ! Define model Fixed layer structure pre-cloud by
                              ! fixed DZ intervals...
!  call print_vla_in
   call prepare_model_profile_fu ! Call After all FI%VD and FI%VI structures are
                                 ! defined.
   call vla_interface_fu ! Uses FI%VO, assign model ATM profile and CLD Levels
!  call print_vla_out

   ! Aerosol profile (after fi%pp is created )----------------------------------
   call aer_scale_hgt(fi%nv,fi%pp,3.0,fi%aprofs(1:fi%nv,1) )
   call aer_scale_hgt(fi%nv,fi%pp,3.0,fi%aprofs(1:fi%nv,2) )

   ! Radiative transfer --------------------------------------------------------
!  call print_in_fu ! prints inputs as ascii
!  fi%swonlycomp=.true.

   call rad_multi_fu  ! call the code

!  call print_out_fu ! prints outputs as ascii
!  call print_out_hr ! prints ascii heating rate profiles

   ! Assign values to specific output variables
   if (ftoa(2)%olr .gt. 0. .and. ftoa(2)%olr .lt. 1000. .and. &
       ftoa(2)%swup .gt. 0. .and. ftoa(2)%swup .lt. 1600. .and. &
       fo(2)%exist) then
      toalwup    = ftoa(2)%olr
      toaswdn    = ftoa(2)%swdn
      toaswup    = ftoa(2)%swup
      boalwup    = fsfc(2)%lwup
      boalwdn    = fsfc(2)%lwdn
      boaswup    = fsfc(2)%swup
      boaswdn    = fsfc(2)%swdn
      toalwupclr = ftoa(3)%olr
      toaswupclr = ftoa(3)%swup
      boalwupclr = fsfc(3)%lwup
      boalwdnclr = fsfc(3)%lwdn
      boaswdnclr = fsfc(3)%swdn
      boaswupclr = fsfc(3)%swup
      boapar     = fsfc(2)%swpar
      boapardif  = fsfc(2)%swpar * (fsfc(2)%swdif/(fsfc(2)%swdir + fsfc(2)%swdif))
      toapar     = fouv(2)%toa_par
   end if

   ! To output profile - model levels are not same as input levels....
!  print*,nlm,fi%nv+1
!  print*,fo(2)%fds(1:nlm+1)
!  fdsw(1,1:nlm+1) = fo(2)%fds(1:nlm+1)
#endif
end subroutine driver_for_fuliou
