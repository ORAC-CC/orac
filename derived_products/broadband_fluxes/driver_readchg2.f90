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
!     bugs_rad   :The radiative transfer code

! FUNCTIONS CALLED:
!     none.

! INCLUDED COMMON BLOCKS:
!     none.
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name: driver_readchg2.f90
!
! Purpose:
!
! Inputs:
!
! Output:
!
! History:
! xxxx/xx/xx, MC: Initial development.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine driver_read(tsi,theta,asfcswr,asfcnir, &
                       cref,ccot,hctop,hcbase, &
                       hctopID,hcbaseID, &
                       pxZ,pxP,pxT,pxQ,pxO3, &
                       toalwup,toaswdn,toaswup, &
                       boalwup,boalwdn,boaswdn,boaswup, &
                       toalwupclr,toaswupclr, &
                       boalwupclr,boalwdnclr,boaswupclr,boaswdnclr, &
                       boapar,boapardif,toapar)

   use kinds, only:  int_kind, dbl_kind
   use bugsrad_physconst, only:  gravity, cp_dry_air, sol_const

   implicit none

   ! Input arguments
   real, intent(in) :: tsi,theta
   real, intent(in) :: cref,ccot,hctop,hcbase
   real, intent(in) :: asfcswr,asfcnir
   integer, intent(in) :: hctopID(1),hcbaseID(1)

   ! Output arguments
   real, intent(out) :: toalwup,toaswdn,toaswup
   real, intent(out) :: boalwup,boalwdn,boaswdn,boaswup
   real, intent(out) :: toalwupclr,toaswupclr
   real, intent(out) :: boalwupclr,boaswupclr,boaswdnclr,boalwdnclr
   real, intent(out) :: boapar,boapardif,toapar

   ! Local variables
   integer (kind=int_kind):: &
      nlen,& ! Length of total domain.
      len ,& ! Length of sub domain.
      nlm ,& ! Number of layers.
      i,l

   real (kind=dbl_kind), dimension(:), allocatable:: &
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

   real (kind=dbl_kind), dimension(:,:), allocatable:: &
      pl  ,& ! Layer pressure           (hPa).
      dpl ,& ! Layer thickness          (hPa).
      tl  ,& ! Temperature              (K).
      ql  ,& ! Specific humidity        (kg/kg).
      qcwl,& ! Cloud water mixing ratio (kg/kg).
      qcil,& ! Cloud ice mixing ratio   (kg/kg).
      qrwl,& ! Rain mixing ratio        (kg/kg).
      qril,& ! Snow mixing ratio        (kg/kg).
      o3l ,& ! Ozone mixing ratio       (kg/kg).
      zl,&   ! height
      acld   ! Radiative cloud fraction  (-).

   ! Note that rain mixing ratio is unused by BUGSrad, but I've put it into the
   ! std_profile.dat file for completeness

   real (kind=dbl_kind), dimension(:,:), allocatable:: &
      pl2 ! Level pressure (hPa).

   real (kind=dbl_kind), dimension(:,:), allocatable:: &
      atl   ,& ! All-sky LW radiative heating rate (K/s).
      asl   ,& ! All-sky SW radiative heating rate (K/s).
      fulw  ,& ! All-sky LW upwelling flux         (W/m^2).
      fdlw  ,& ! All-sky LW downwelling flux       (W/m^2).
      fusw  ,& ! All-sky SW upwelling flux         (W/m^2).
      fdsw  ,& ! All-sky SW downwelling flux       (W/m^2).
      fulwcl,& ! CLEAR-sky LW upwelling flux       (W/m^2).
      fdlwcl,& ! CLEAR-sky LW downwelling flux     (W/m^2).
      fuswcl,& ! CLEAR-sky SW upwelling flux       (W/m^2).
      fdswcl   ! CLEAR-sky SW downwelling flux     (W/m^2).

   real, parameter :: bugs_solar_constant = 1360.
   real :: solar_factor

   ! For timing
   real, dimension(2) :: tarray
   real :: dtime,elapsed

   ! For reading the file
   character(LEN=1)   :: l0
   character(LEN=200) :: line

   real, parameter :: NL = 30
   real, parameter :: NLS = NL+1
   real, dimension(NLS) :: pxZ, pxP,pxT,pxQ,pxO3

   real :: LWP, CWC

   !----------------------------------------------------------------------------

   solar_factor = tsi/bugs_solar_constant
!  print*,tsi,bugs_solar_constant,solar_factor
!  print*,pxP
!  print*,pxZ
!  print*,pxT


   ! Read profile data from file

   nlm = 30
   nlen = 1 ! to do timing tests
   len = nlen

   ! Allocate arrays
   allocate(ts(nlen) , amu0(nlen) , slr(nlen))

   allocate(alvdr(nlen) , alndr(nlen) , alvdf(nlen), alndf(nlen))
   allocate(umco2(nlen), umch4(nlen), umn2o(nlen))
   allocate(pl(nlen,nlm), dpl(nlen,nlm), tl(nlen,nlm), ql(nlen,nlm), &
   qcwl(nlen,nlm) , qcil(nlen,nlm), qrwl(nlen,nlm), qril(nlen,nlm),  &
   o3l(nlen,nlm), acld(nlen,nlm))

   allocate(pl2(nlen,nlm+1))

   allocate(atl(nlen,nlm), asl(nlen,nlm), fulw(nlen,nlm+1), &
            fdlw(nlen,nlm+1), fusw(nlen,nlm+1), fdsw(nlen,nlm+1))

   allocate(fdswcl(nlen,nlm+1), fuswcl(nlen,nlm+1), fdlwcl(nlen,nlm+1), &
            fulwcl(nlen,nlm+1))


!  call midlatsum1(pxZ,pxP,pxT,pxQ,pxO3,NLS)
   pl2(1,:) = pxP(1:)
   do l = 1, nlm
    pl(1,l) = pxP(l) + ( pxP(l+1)-pxP(l) ) / 2.
   end do
   pl2(1,:) = pxP(1:)

   tl(1,1:nlm) = pxT
   ql(1,1:nlm) = pxQ / 1000.
   o3l(1,1:nlm) = pxO3

   qcwl(1,1:nlm) = 0.
   qcil(1,1:nlm) = 0.
   qrwl(1,1:nlm) = 0.
   qril(1,1:nlm) = 0.
   acld(1,1:nlm) = 0.

   ts(1) = 257.
   amu0(1) = theta
   alvdr(1) = asfcswr
   alndr(1) = asfcnir
   alvdf(1) = asfcswr
   alndf(1) = asfcnir
   umco2(1) = 380.
   umch4(1) = 1.80
   umn2o(1) = 0.26

   do l = 1, nlm
     dpl(1,l) = pl2(1,l+1)-pl2(1,l)
   end do

   ! Clouds? Hardcoded here, can read in if you want to.
   LWP = ((5./9.)*cref*ccot)
   CWC = LWP/((hctop-hcbase)*1000.)

   ! Initialize
   qcwl(1,:) = 0.0 ! cloud water mixing ratio (kg/kg)
   qcil(1,:) = 0.0 ! cloud ice mixing ratio (kg/kg)
   qrwl(1,:) = 0.0 ! rain mixing ratio
   qril(1,:) = 0.0 ! snow mixing ratio
   acld(1,:) = 0.0 ! layer cloud fraction

   ! Assign cloud to specific level
   acld(1,hctopID(1):hcbaseID(1)) = 1.0

   qcwl(1,hctopID(1):hcbaseID(1)) = (CWC/(pl(1,hctopID(1):hcbaseID(1))/ &
      (287.*tl(1,hctopID(1):hcbaseID(1)))))/1000.

   if (hctopID(1)-hcbaseID(1) .eq. 0.) then
      print*,cref,ccot,hctop,hcbase,LWP,CWC,hctopID,hcbaseID
      print*,acld
      print*,' '
      print*,qcwl
      stop
   end if


   ! Copy profile to all columns
   ! Copy the same column to all columns (only useful if testing multiple
   ! identical columns for timing, otherwise, it doesn't hurt)
   do i = 1, nlen
      pl2(i,:) = pl2(1,:)
      pl(i,:) = pl(1,:)
      dpl(i,:) = dpl(1,:)
      tl(i,:) = tl(1,:)
      ql(i,:) = ql(1,:)
      o3l(i,:) = o3l(1,:)
      acld(i,:) = acld(1,:)
      qcwl(i,:) = qcwl(1,:)
      qcil(i,:) = qcil(1,:)
      qril(i,:) = qril(1,:)
      amu0(i) = amu0(1)
      alvdr(i) = alvdr(1)
      alvdf(i) = alvdf(1)
      alndr(i) = alndr(1)
      alndf(i) = alndf(1)
   end do


   ! Specify other variables
!  amu0(:) = 1.0
!
!  alvdr(:) = 0.2
!  alvdf(:) = 0.2
!  alndr(:) = 0.2
!  alndf(:) = 0.2

!  slr(:) = 1.0
   slr(:) = solar_factor


   ! Call the radiative transfer code
   elapsed = dtime(tarray)
   call bugs_rad(nlen,len,nlm,pl2,pl,dpl,tl,ql,qcwl,qcil,qril, &
                 o3l,ts,amu0,slr,alvdf,alndf,alvdr,alndr,sol_const, &
                 gravity,cp_dry_air,asl,atl,fdsw,fusw,fdlw,fulw, &
                 acld, umco2, umch4, umn2o, &
                 fdswcl,fuswcl,fdlwcl,fulwcl,boapar,boapardif,toapar)
   elapsed = dtime(tarray)


   ! Output results
!  print fluxes in W/m2, heating rates in K/day.
!  print *, "Dtime: ", elapsed
!  print *, " Fluxes   Plev       SW_DN       SW_UP       LW_DN       LW_UP"
!  print *, "            Pa       W/m^2       W/m^2       W/m^2       W/m^2"
!  do l=1,nlm+1
!     print '(I4,5(F12.3))',l,pl2(1,l),fdsw(1,l),fusw(1,l), &
!                                      fdlw(1,l),fulw(1,l)
!  end do
!  print *, 'Heating Rates   Play              SW            LW'
!  print *, '                  Pa           K/day         K/day'
!  do l=1,nlm
!     print '(I4,6X, F12.3,2(F15.5))', &
!           l,pl(1,l),asl(1,l)*86400.,atl(1,l)*86400. ! K/day
!  end do

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

end subroutine driver_read
