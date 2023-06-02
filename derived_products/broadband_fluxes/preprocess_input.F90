!-------------------------------------------------------------------------------
! Name: preprocess_input.F90
!
! Purpose:
! Prepares pixel-scale retrievals of aerosol & cloud properties for ingestion to
! BUGSrad. Three steps: 1) categorizes phase and type 2) calculates cloud base
! height from adiabatic profile, & 3) ensures accurate placement of layer to
! BUGSrad vertical bin. Step 3 is particularly important as the layer must span
! a complete vertical bin to impose changes on the fluxes.
!
! Inputs:
! cMASK: cloud mask
! aREF: aerosol retrieved 550 nm effective radius
! AOD: aerosol retrieved 550 nm optical depth
! cPHASE: cloud phase
! cCTT: cloud top temperature
! NLS: number of vertical levels used to compute fluxes in BB code
! zz: geopotential height vertical profile
!
! Output:
! REDAT: effective particle radius (cloud or aerosol)
! TAUDAT: optical thickness (cloud or aerosol)
! Hctop: layer top height (cloud or aerosol)
! Hcbase: layer base height (cloud or aerosol)
! phaseFlag: phase of cloud; 0=clear, 1=water, 2=ice
! LayerType: 1=cloud, 2=aerosol
! regime: 1=overcast, 2=joint, 3=clear, 4=no cloud or aerosol
! computationFlag: currently not being used for anything -delete this-
! TopID: layer top location in vertical profile used in BB code
! BaseID: layer base location in vertical profile used in BB code
!
! History:
! 2015/10/14, MC: Initial implementation
! 2015/21/14, MC: Added aerosol input data to code
! 2016/01/13, MC: Added additional categories for cloud phase to regime pixel
! 2016/01/13, MC: Modified adiabatic model to depend on temperature and pressure
!    for cloud base height calculation.
! 2016/01/30, MC: Added separate computation of cloud water path depending on
!    the cloud phase; methods taken from Stephens et al. (1978) and Liou, (1992)
!    - for the estimation of cloud base height.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine preprocess_input(cMASK,aREF,AOD,cPHASE, &
                            cCTT,cCTP,cREF,cTAU,cCTH,InfThnCld, &
                            NLS,zz,REDAT,TAUDAT,Hctop,Hcbase, &
                            phaseFlag,LayerType, &
                            regime,TopID,BaseID)

   use common_constants_m

   implicit none

   ! Input arguments
   integer, intent(in) :: NLS ! # vertical levels

   real, intent(in) :: &
      cMASK ,& ! cloud mask
      aREF  ,& ! aerosol effective radius
      AOD   ,& ! aerosol optical depth
      cPHASE,& ! cloud phase
      cCTH  ,& ! cloud top height
      cTAU  ,& ! cloud optical depth
      cREF  ,& ! cloud effective droplet radius
      cCTT  ,& ! cloud top temperature
      cCTP     ! cloud top pressure

   integer, intent(in) :: InfThnCld !flag to use infinitely thin cloud

   real, intent(in), dimension(NLS) :: zz !height profile

   ! Output arguments
   real, intent(out) :: &
      Hctop    ,& ! cloud top height matched to Hprofile
      Hcbase   ,& ! cloud base height matched to Hprofile
      REDAT    ,& ! effective radius input to BUGSrad
      TAUDAT   ,& ! cloud optical depth input to BUGSrad
      phaseFlag,& ! phase of cloud
      LayerType,& ! type of layer (aerosol or cloud)
      regime      ! regime (aerosol/cloud)

   integer, intent(out) :: &
      TopID(1) ,& ! index of cloud top height in Hprofile
      BaseID(1)   ! index of cloud base height in Hprofile

   ! Local variables
   real :: Hcthick,HcthickG,cwp,cmt,cmp

   real, parameter :: a=-0.00665599, b=3.686, Fad=0.79         ! for adiabatic assumption
   real, parameter :: rhowat = 999.9*1000., rhoice=934.4*1000. ! density of water & ice [g/m3]
   !*note, adiabatic assumption can be improved by using T&P data

   real :: dw_dzG,dw_dz


!  print*,'pre-processor'
!  print*,cMASK,aREF,AOD,cPHASE,cCTT,cCTP,cREF,cTAU,cCTH,InfThnCld

   ! Cloud Phase
   ! Multi-layer
!  if (cPHASE .eq. 0 .or. cPHASE .eq. 3 .and. cMASK .eq. 1 .and. cCTT .gt. 273.) phaseFlag=1 ! WATER
!  if (cPHASE .eq. 0 .or. cPHASE .eq. 3 .and. cMASK .eq. 1 .and. cCTT .le. 273.) phaseFlag=2 ! ICE
   if (cPHASE .eq. 3 .and. cCTT .gt. 273.) phaseFlag=1 ! WATER
   if (cPHASE .eq. 3 .and. cCTT .gt. 0. .and. cCTT .le. 273.) phaseFlag=2 ! ICE

   ! WATER - DEFINITE
   if (cPHASE .eq. 1) phaseFlag=1 ! WATER

   ! ICE - DEFINITE
   if (cPHASE .eq. 2) phaseFlag=2 ! ICE

   ! CLEAR - DEFINITE
!  if (cMASK .eq. 0) phaseFlag=0 ! CLEAR
   if (cPHASE .le. 0) phaseFlag=0 ! CLEAR

!  print*,'phaseFlag = ',phaseFlag

   regime = 0
   ! Determine pixel regime
!  if (cMASK .eq. 1.0 .and. AOD .le. 0. .and. phaseFlag .eq. 1) regime = 1 ! overcast LIQUID cloud
!  if (cMASK .eq. 1.0 .and. AOD .le. 0. .and. phaseFlag .eq. 2) regime = 2 ! overcast ICE cloud
!  if (cMASK .eq. 0.0 .and. AOD .gt. 0. .and. phaseFlag .eq. 0) regime = 3 ! clear with AOD
!  if (cMASK .eq. 0.0 .and. AOD .le. 0. .and. phaseFlag .eq. 0) regime = 4 ! clear no AOD
!  if (cMASK .eq. 1.0 .and. AOD .gt. 0. .and. phaseFlag .eq. 1) regime = 5 ! joint aerosol-LIQUID-cloud
!  if (cMASK .eq. 1.0 .and. AOD .gt. 0. .and. phaseFlag .eq. 2) regime = 6 ! joint aerosol-ICE-cloud
   if (                     AOD .le. 0. .and. phaseFlag .eq. 1) regime = 1 ! overcast LIQUID cloud
   if (                     AOD .le. 0. .and. phaseFlag .eq. 2) regime = 2 ! overcast ICE cloud
   if (                     AOD .gt. 0. .and. phaseFlag .eq. 0) regime = 3 ! clear with AOD
   if (                     AOD .le. 0. .and. phaseFlag .eq. 0) regime = 4 ! clear no AOD
   if (                     AOD .gt. 0. .and. phaseFlag .eq. 1) regime = 5 ! joint aerosol-LIQUID-cloud
   if (                     AOD .gt. 0. .and. phaseFlag .eq. 2) regime = 6 ! joint aerosol-ICE-cloud
   if (regime .eq. 0) then
!     print*,'PROBLEM in pre-processor: pixel regime undefined!'
!     stop
      regime = 4
   end if


   ! Compute cloud top/base height and fit to vertical coordinates

   ! REGIMES 1 & 2
   ! OVERCAST or JOINT Aerosol & CLOUD PIXEL
   if (regime .eq. 1 .or. regime .eq. 2 .or. regime .eq. 5 .or. regime .eq. 6) then
      LayerType=1 !cloud

      ! Valid Cloud Retrieval
      if (cREF .gt. 0. .and. cTAU .gt. 0. .and. cCTH .gt. 0.) then
          REDAT=cREF  !keep value same
          TAUDAT=cTAU !keep value same
          Hctop=cCTH  !keep value same

          ! Cloud base height using dynamic adiabatic model that depends on
          ! cloud top temperature and pressure
          ! call adiabatic_lwc(283.,525.,dw_dz) ! example
          call adiabatic_lwc(cCTT,cCTP,dw_dzG)
          if (dw_dzG .lt. 0.025) dw_dzG=0.025 ! this value gets too small causing problem

          ! Liquid water path
          if (phaseFlag .eq. 1) then
           ! Stephens et al. (1978)
           cwp = (2./3.)*cTAU*cREF ! [g/m2] cREF--> um
          end if

          ! Ice water path
          if (phaseFlag .eq. 2) then
           ! Liou, 1992 Table 6.4
           cwp = cTAU/(a+b/(2*cREF)) ! [g/m2] cREF--> um
          end if

          ! Derived from Meerkotter & Zinner, 2007
          HcthickG = ( sqrt( (2.*cwp)/(Fad*(dw_dzG/1000.)) ) ) / 1000. ![km]

          ! Re-compute cloud thickness based on middle of cloud layer
          ! temperature & pressure from first guess cloud thickness assume 5.5
          ! K/km psuedo adiabatic lapse rate
          cmt = cCTT + 5.5 * (HcthickG/2.)
          cmp = cCTP / EXP(-(HcthickG)/(2.* (287.*cmt/g_wmo/1000.) ))
          ! Corrected adiabatic rate of cwc increase with height based on middle
          ! of cloud layer
          call adiabatic_lwc(cmt,cmp,dw_dz)
          if (dw_dz .lt. 0.025) dw_dz=0.025 ! this value gets too small causing problem

          ! Corrected cloud thickness
          Hcthick = ( sqrt( (2.*cwp)/(Fad*(dw_dz/1000.)) ) ) / 1000. ![km]

          ! Estimated cloud base height
          Hcbase = cCTH - Hcthick

!         if (HcthickG .gt. 0.5 .and. phaseFlag .eq. 2) then
!            print*,'phase: ',phaseFlag
!            print*,'Cloud Top Height: ',cCTH
!            print*,'Cloud Top Temp: ',cCTT
!            print*,'Cloud Top Press: ',cCTP
!            print*,'Cloud MIDDLE Temp: ',cmt
!            print*,'Cloud MIDDLE Press: ',cmp
!            print*,'Cloud water path: ',cwp
!            print*,'Cloud optical depth: ',cTAU
!            print*,'Cloud effective radius: ',cREF
!            print*,'adiabaticity: ',Fad
!            print*,'adiabatic lapse rate 1st guess: ',dw_dzG
!            print*,'adiabatic lapse rate middle cloud: ',dw_dz
!            print*,'Cloud thickness 1st Guess: ',HcthickG
!            print*,'Cloud thickness: ',Hcthick
!            print*,'Cloud base height 1st guess: ',cCTH - HcthickG
!            print*,'Cloud base height: ',Hcbase
!            stop
!         end if
      end if

      ! Invalid Cloud Retrieval
      if (cREF .le. 0. .or. cTAU .le. 0. .or. cCTH .le. 0.) then
         ! Assume the following cloud where CC_TOTAL = 1 but retrieval is bad
         REDAT=10.
         TAUDAT=3.
         Hctop=1.5
         Hcbase = 0.5
      end if
   end if

   ! REGIME 3
   ! CLOUD-FREE with AEROSOL
   if (regime .eq. 3) then
      LayerType=2 !currently does not matter in BUGSrad -no aerosol
      phaseFlag=1 !set the retrieval to do water cloud

      ! Valid AOD retrieval use same values
      if (aREF .gt. 0 .and. aod .gt. 0) then
         REDAT=aREF
         TAUDAT=AOD
         Hctop=1.5
         Hcbase=0.5
      end if

      ! Invalid AOD retrieval
      if (aREF .le. 0 .or. aod .le. 0) then
         REDAT=0.05
         TAUDAT=0.05
         Hctop=1.5
         Hcbase=0.5
      end if
   end if

   ! REGIME 4
   ! Missing Regime
   if (regime .eq. 4) then
      LayerType=1
      !Assume the following properties
      REDAT=0.05
      TAUDAT=0.05
      Hctop=1.5
      Hcbase=0.5
   end if


   ! Match cloud top/base heights to vertical profile
   ! REQUIREMENT: Cloud layer needs to fill an entire bin

   if ( InfThnCld .eq. 1) then
      Hctop = Hcbase
   end if

   ! Cloud thickness
   Hcthick=Hctop-Hcbase

   ! Find bin nearest cloud top
   TopID =minloc( abs( Hctop - zz ) )
   BaseID=minloc( abs( Hcbase-zz ) )
!  print*,'Hcthick = ',Hcthick
!  print*,'Hctop = ',Hctop,' TopID: ',TopID
!  print*,'Hcbase = ',Hcbase,' BaseID: ',BaseID

   ! BaseID cannot equal TopID (if it is make base 1 level lower)
   if (BaseID(1) .eq. TopID(1)) BaseID(1) = BaseID(1)+1

   ! Cloud base is above the surface and at least 1 layer below the top
   if (BaseID(1) .ge. NLS .or. Hcbase .le. zz(NLS)) then
!     print*,'cloud below surface',baseID(1),NLS,zz(NLS)
      BaseID(1) = NLS-1
      if (BaseID(1) .le. TopID(1)) TopID(1) = BaseID(1)-1
   end if

   ! Change cloud top and base heights to match vertical bin height
   Hctop = zz(TopID(1))
   Hcbase = zz(BaseID(1))
!  print*,'Hctop = ',Hctop,' TopID: ',TopID
!  print*,'Hcbase = ',Hcbase,' BaseID: ',BaseID

end subroutine preprocess_input
