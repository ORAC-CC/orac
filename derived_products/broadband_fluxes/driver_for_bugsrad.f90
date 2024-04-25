!-------------------------------------------------------------------------------
! Upstream header
!-------------------------------------------------------------------------------
! driver_read is the main routine for running the CSU radiative transfer
! code offline (that is, apart from the CSU GCM).  It reads a profile
! from a file and also specifies variables that are not read in.  It
! then calls BUGSrad to do the radiative transfer.  This driver is not
! used when the code is compiled online with the CSU GCM.

! REFERENCES:
! Phil Partain /wombat (04-04-00).

! MODIFICATIONS:
! * changed declarations to adapt the code from BUGS4 to BUGS5.
!   Laura D. Fowler/slikrock (02-01-00).

! SUBROUTINES CALLED:
!     bugs_rad : The radiative transfer code

! FUNCTIONS CALLED:
!     none.

! INCLUDED COMMON BLOCKS:
!     none.
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: driver_for_bugsrad.f90
!
! Purpose:
!
! Inputs:
!
! Output:
!
! History:
! 2015/11/15, MC: Initial development.  Changed input/output quantities to
!   accept satellite data changed name of the driver to driver_for_bugsrad.F90.
! 2018/02/27 MST: Using pyYEAR (now passed from parent routine) to calculate time dependent CO2 concentration
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine driver_for_bugsrad(nlm,tsi,theta, &
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
                              emis,rho0d,rhodd,pxYEAR)

   use kinds, only: int_kind, dbl_kind
   use bugsrad_physconst, only: gravity, cp_dry_air, sol_const

   implicit none

   ! Input arguments

   ! Model setup & cloud locations
   integer, intent(in) :: &
      nlm    ,& ! Number of vertical layers. (-).
      nlayers   ! Number of cloud layers

   integer, intent(in) :: &
      hctopID(nlayers) ,& ! vertical level index for Hctop  (-).
      hcbaseID(nlayers)   ! vertical level index for Hcbase (-).

   integer, intent(in) :: &
      pxYEAR

   ! Column quantities
   real, intent(in) :: &
      tsi      ,&  ! total solar irradiance               (W/m2).
      theta    ,&  ! cosine of solar zenith angle         (-).
      asfcswrdr,&  ! DIRECT visible surface albedo        (-).
      asfcnirdr,&  ! DIRECT near infrared surface albedo  (-).
      asfcswrdf,&  ! DIFFUSE visible surface albedo       (-).
      asfcnirdf,&  ! DIFFUSE near infrared surface albedo (-).
      tsfc         ! surface temperature

   real, intent(in) :: &
      phaseflag(nlayers),& ! cloud phase=0 clear, = 1water, =2ice (-).
      cref(nlayers)     ,& ! satellite cloud effective radius     (um).
      ccot(nlayers)     ,& ! satellite cloud optical depth        (-).
      hctop(nlayers)    ,& ! satellite cloud top height           (km).
      hcbase(nlayers)      ! satellite cloud base height          (km).

   ! Meteorological profiles
   real, intent(in), dimension(nlm+1) :: &
      pxZ ,& ! geopotential height profile at SAT. pixel (km).
      pxP ,& ! pressure profile at SAT. pixel            (hPa).
      pxT ,& ! temperature profile at SAT. pixel         (K).
      pxQ ,& ! specific humidity profile at SAT. pixel   (kg/kg).
      pxO3   ! Ozone mixing ratio at SAT. pixel          (kg/kg).

   real, intent(in) :: emis(12) ! Spectral surface emissivity for each LW band
   real, intent(in) :: rho0d(6) ! Spectral direct surface albedo for each SW band
   real, intent(in) :: rhodd(6) ! Spectral diffuse surface albedo for each SW band

   ! Output arguments
   real, intent(out) :: &
      toalwup   ,& ! All-sky TOA LW upwelling flux            (W/m2).
      toaswdn   ,& ! All-sky TOA SW upwelling flux            (W/m2).
      toaswup   ,& ! All-sky TOA SW upwelling flux            (W/m2).
      boalwup   ,& ! All-sky BOA LW upwelling flux            (W/m2).
      boalwdn   ,& ! All-sky BOA LW upwelling flux            (W/m2).
      boaswdn   ,& ! All-sky BOA SW upwelling flux            (W/m2).
      boaswup   ,& ! All-sky BOA SW upwelling flux            (W/m2).
      toalwupclr,& ! CLEAR-sky TOA LW upwelling flux          (W/m2).
      toaswupclr,& ! CLEAR-sky TOA SW upwelling flux          (W/m2).
      boalwupclr,& ! CLEAR-sky BOA LW upwelling flux          (W/m2).
      boaswupclr,& ! CLEAR-sky BOA SW upwelling flux          (W/m2).
      boaswdnclr,& ! CLEAR-sky BOA SW upwelling flux          (W/m2).
      boalwdnclr,& ! CLEAR-sky BOA LW upwelling flux          (W/m2).
      boapar    ,& ! All-sky BOA PAR downwelling flux         (W/m2).
      boapardif ,& ! All-sky BOA PAR downwelling diffuse flux (W/m2).
      toapar       ! All-sky TOA PAR downwelling flux         (W/m2).

   ! Local variables
   integer, parameter :: nlen=1, len=1
   integer (kind=int_kind):: i,l

   real (kind=dbl_kind), dimension(nlen) :: &
      ts   ,& ! Surface temperature            (K).
      amu0 ,& ! Cosine of solar zenith angle   (-).
      slr  ,& ! Fraction of daylight           (-).
      alvdr,& ! Visible direct surface albedo  (-).
      alndr,& ! Near-IR direct surface albedo  (-).
      alvdf,& ! Visible diffuse surface albedo (-).
      alndf,& ! Near-IR diffuse surface albedo (-).
      umco2,& ! Col-avg concentration CO2      (ppm).
      umch4,& ! Col-avg concentration CH4      (ppm).
      umn2o   ! Col-avg concentration N2O      (ppm).
      ! Note that rain mixing ratio is unused by BUGSrad.

   real (kind=dbl_kind), dimension(nlen,nlm) :: &
      pl  ,& ! Layer pressure           (hPa).
      dpl ,& ! Layer thickness          (hPa).
      tl  ,& ! Temperature              (K).
      ql  ,& ! Specific humidity        (kg/kg).
      qcwl,& ! Cloud water mixing ratio (kg/kg).
      qcil,& ! Cloud ice mixing ratio   (kg/kg).
      qrwl,& ! Rain mixing ratio        (kg/kg).
      qril,& ! Snow mixing ratio        (kg/kg).
      o3l ,& ! Ozone mixing ratio       (kg/kg).
      acld   ! Radiative cloud fraction (-).

   real (kind=dbl_kind), dimension(nlen,nlm+1) :: &
      pl2 ! Level pressure (hPa).

   real (kind=dbl_kind), dimension(nlen,nlm+1) :: &
      fulw  ,& ! All-sky LW upwelling flux     (W/m^2).
      fdlw  ,& ! All-sky LW downwelling flux   (W/m^2).
      fusw  ,& ! All-sky SW upwelling flux     (W/m^2).
      fdsw  ,& ! All-sky SW downwelling flux   (W/m^2).
      fulwcl,& ! CLEAR-sky LW upwelling flux   (W/m^2).
      fdlwcl,& ! CLEAR-sky LW downwelling flux (W/m^2).
      fuswcl,& ! CLEAR-sky SW upwelling flux   (W/m^2).
      fdswcl   ! CLEAR-sky SW downwelling flux (W/m^2).

   real (kind=dbl_kind), dimension(nlen,nlm) :: &
      atl,& ! All-sky LW radiative heating rate (K/s).
      asl   ! All-sky SW radiative heating rate (K/s).

   real, parameter :: bugs_solar_constant = 1360. ! incoming solar flux used to
                                                  ! constrain retrieval
   real :: solar_factor ! used to tune shortwave fluxes to match SoHO & SORCE TSI data
   real :: &
      LWP,& ! liquid water path of cloud (g/m2).
      CWC   ! cloud water content        (g/m3).

   !----------------------------------------------------------------------------

   ! Tune incoming solar radiation to TSI measurements
   solar_factor = tsi/bugs_solar_constant
!  print*,tsi,solar_factor
!  pxP(1) = pxP(1)+0.5
!  pxP(2) = pxP(2)+1.5
!  pxP(3) = pxP(3)+3.5

   ! Calculate mid-layer quantities
   do l = 1, nlm
      pl(1,l) = pxP(l+1) - ( pxP(l+1)-pxP(l) ) / 2.
      tl(1,l) = pxT(l+1) - ( pxT(l+1)-pxT(l) ) / 2.
      ql(1,l) = pxQ(l+1) - ( pxQ(l+1)-pxQ(l) ) / 2.
      o3l(1,l) = pxO3(l+1) - ( pxO3(l+1)-pxO3(l) ) / 2.
      dpl(1,l) = pxP(l+1)-pxP(l)
   end do
   pl2(1,:) = pxP

!  print*,pl
!  print*,pl2
!  print*,tl
!  print*,ql
!  print*,o3l
!  print*,dpl

   ! Read surface quantities
   ts(1) = tsfc
   amu0(1) = theta
   alvdr(1) = asfcswrdr
   alndr(1) = asfcnirdr
   alvdf(1) = asfcswrdf
   alndf(1) = asfcnirdf

   ! Read trace gas quantities
   !umco2(1)=380.
   !introducting time dependent co2 with 380 representing the year 2006
   umco2(1) = 380.0+(pxYEAR-2006.)*1.7
   umch4(1) = 1.80
   umn2o(1) = 0.26

   ! Clouds
   ! Initialize profile
   qcwl(1,:) = 0.0 ! cloud water mixing ratio (kg/kg)
   qcil(1,:) = 0.0 ! cloud ice mixing ratio (kg/kg)
   qrwl(1,:) = 0.0 ! rain mixing ratio
   qril(1,:) = 0.0 ! snow mixing ratio
   acld(1,:) = 0.0 ! layer cloud fraction

   do i = 1, nlayers
      ! Assign cloud to vertical layer
      acld(1,hctopID(i):hcbaseID(i)) = 1.0

      ! Compute cloud water mixing ratio
      if (phaseflag(i) .eq. 1) then
         LWP = ((5./9.)*cref(i)*ccot(i)) * (5./6.)
         CWC = LWP/((hctop(i)-hcbase(i))*1000.)
!        mixing ratio = CWC / density of air (density = p/RT)
         qcwl(1,hctopID(i):hcbaseID(i)) = (CWC/(pl(1,hctopID(i):hcbaseID(i)) &
            *100./(287.*tl(1,hctopID(i):hcbaseID(i)))))/1000.
      end if
      if (phaseflag(i) .eq. 2) then
         LWP = ((5./9.)*cref(i)*ccot(i)) * (5./6.)
         CWC = LWP/((hctop(i)-hcbase(i))*1000.)
         qcil(1,hctopID(i):hcbaseID(i)) = (CWC/( pl(1,hctopID(i):hcbaseID(i)) &
            *100./(287.*tl(1,hctopID(i):hcbaseID(i)))))/1000.
      end if
   end do

   ! Read solar factor
   slr(:) = solar_factor

!  print*,CWC
!  print*,phaseflag
!  print*,hctopID(1),hcbaseID(1),hctop,hcbase
!  print*,qcwl
!  print*,''
!  print*,qcil
!  print*,''

!  print*,'nlen = ',nlen
!  print*,'len = ',len
!  print*,'nlm = ',nlm
!  print*,'pl2 = ',pl2
!  print*,'pl = ',pl
!  print*,'dpl = ',dpl
!  print*,'tl = ',tl
!  print*,'ql = ',ql
!  print*,'qcwl = ',qcwl
!  print*,'qcil = ',qcil
!  print*,'qril = ',qril
!  print*,'o3l = ',o3l
!  print*,'ts = ',ts
!  print*,'amu0 = ',amu0
!  print*,'slr = ',slr
!  print*,'alvdf = ',alvdf
!  print*,'alndf = ',alndf
!  print*,'alvdr = ',alvdr
!  print*,'alndr = ',alndr
!  print*,'sol_const = ',sol_const
!  print*,'gravity = ',gravity
!  print*,'cp_dry_air = ',cp_dry_air
!  print*,'asl = ',asl
!  print*,'atl = ',atl

   ! Call the radiative transfer code
   call bugs_rad(nlen,len,nlm,pl2,pl,dpl,tl,ql,qcwl,qcil,qril, &
                 o3l,ts,amu0,slr,alvdf,alndf,alvdr,alndr,sol_const, &
                 gravity,cp_dry_air,asl,atl,fdsw,fusw,fdlw,fulw, &
                 acld, umco2, umch4, umn2o, &
                 fdswcl,fuswcl,fdlwcl,fulwcl,boapar,boapardif,toapar,cref,&
                 emis,rho0d,rhodd)

   ! Output results
!  print fluxes in W/m2, heating rates in K/day.
!  print *, " Fluxes   Plev       SW_DN       SW_UP       LW_DN       LW_UP"
!  print *, "            Pa       W/m^2       W/m^2       W/m^2       W/m^2"
!  do l=1,nlm+1
!     print '(I4,5(F12.3))',l,pl2(1,l),fdsw(1,l),fusw(1,l), &
!                                      fdlw(1,l),fulw(1,l)
!  end do
!
!  print *, "CLR Fluxes   Plev       SW_DN       SW_UP       LW_DN       LW_UP"
!  print *, "            Pa       W/m^2       W/m^2       W/m^2       W/m^2"
!  do l=1,nlm+1
!     print '(I4,5(F12.3))',l,pl2(1,l),fdswcl(1,l),fuswcl(1,l), &
!                                      fdlwcl(1,l),fulwcl(1,l)
!  end do
!
!  print *, 'Heating Rates   Play              SW            LW'
!  print *, '                  Pa           K/day         K/day'
!  do l=1,nlm
!     print '(I4,6X, F12.3,2(F15.5))', &
!           l,pl(1,l),asl(1,l)*86400.,atl(1,l)*86400. !K/day
!  end do

!  Assign values to specific output variables
   toalwup = fulw(1,1)
   toaswdn = fdsw(1,1)
   toaswup = fusw(1,1)
   boalwup = fulw(1,nlm+1)
   boalwdn = fdlw(1,nlm+1)
   boaswup = fusw(1,nlm+1)
   boaswdn = fdsw(1,nlm+1)
   toalwupclr = fulwcl(1,1)
   toaswupclr = fuswcl(1,1)
   boalwupclr = fulwcl(1,nlm+1)
   boalwdnclr = fdlwcl(1,nlm+1)
   boaswdnclr = fdswcl(1,nlm+1)
   boaswupclr = fuswcl(1,nlm+1)

end subroutine driver_for_bugsrad
