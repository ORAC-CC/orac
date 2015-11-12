   subroutine preprocess_bugsrad(cMASK,aREF,AOD,cPHASE,&
                                 cCTT,cREF,cTAU,cCTH,&
                                 NLS,zz,REDAT,TAUDAT,Hctop,Hcbase,&
                                 phaseFlag,LayerType,&
                                 regime,computationFlag,TopID,BaseID)

   implicit none

   !input arguments
    integer, intent(in) :: NLS !#vertical levels

    real, intent(in) :: &
       cMASK  ,&  !cloud mask
       aREF   ,&  !aerosol effective radius
       AOD    ,&  !aerosol optical depth
       cPHASE ,&  !cloud phase
       cCTH   ,&  !cloud top height
       cTAU   ,&  !cloud optical depth
       cREF   ,&  !cloud effective droplet radius
       cCTT       !cloud top temperature

    real, intent(in), dimension(NLS) :: zz !height profile

   !output arguments
    real, intent(out) :: &
       Hctop     ,&    !cloud top height matched to Hprofile
       Hcbase    ,&    !cloud base height matched to Hprofile
       REDAT     ,&    !effective radius input to BUGSrad
       TAUDAT    ,&    !cloud optical depth input to BUGSrad
       phaseFlag ,&    !phase of cloud
       LayerType ,&    !type of layer (aerosol or cloud)
       regime    ,&    !regime (aerosol/cloud)
       computationflag !problem with retrieval

    integer, intent(out) :: &
       TopID(1)  ,&    !index of cloud top height in Hprofile
       BaseID(1)       !index of cloud base height in Hprofile

   !local variables
    real Hcthick,rhocld

   !Constants
    real, parameter :: A=2.24E-4, ALPH = 0.79 !for adiabatic assumption
    real, parameter :: rhowat = 999.9*1000., rhoice = 934.4*1000.  !density of water & ice [g/m3]
     !*note, adiabatic assumption can be improved by using T&P data

!-----------------------------------------------------------------------------
   computationFlag=0

 
   !Cloud Phase
   !Unknown
   IF(cPHASE .eq. 0 .and. cMASK .eq. 1 .and. cCTT .gt. 273.) phaseFlag=1 !WATER
   IF(cPHASE .eq. 0 .and. cMASK .eq. 1 .and. cCTT .le. 273.) phaseFlag=2 !ICE

   !WATER - DEFINITE
   IF(cPHASE .eq. 1) phaseFlag=1 !WATER

   !ICE - DEFINITE
   IF(cPHASE .eq. 2) phaseFlag=2 !ICE

   !CLEAR - DEFINITE
   IF(cMASK .eq. 0) phaseFlag=0 !CLEAR


   !Determine pixel regime
   if(cMASK .eq. 1.0 .and. AOD .le. 0. .and. phaseFlag .ne. 0) regime = 1 !overcast
   if(cMASK .eq. 1.0 .and. AOD .gt. 0. .and. phaseFlag .ne. 0) regime = 2 !joint aerosol-cloud
   if(cMASK .ne. 1.0 .and. AOD .gt. 0. .and. phaseFlag .eq. 0) regime = 3 !clear with AOD
   if(cMASK .ne. 1.0 .and. AOD .le. 0. .and. phaseFlag .eq. 0) regime = 4 !clear no AOD



!Compute cloud top/base height and fit to vertical coordinates

!REGIMES 1 & 2
   !OVERCAST or JOINT Aerosol & CLOUD PIXEL
   if(regime .eq. 1 .or. regime .eq. 2) then
    LayerType=1 !cloud

    !Valid Cloud Retrieval
    if(cREF .gt. 0. .and. cTAU .gt. 0. .and. cCTH .gt. 0.) then
      REDAT=cREF  !keep value same
      TAUDAT=cTAU !keep value same
      Hctop=cCTH  !keep value same
      if(phaseFlag .eq. 1) rhocld=rhowat
      if(phaseFlag .eq. 2) rhocld=rhoice
      Hcbase = cCTH - ( sqrt( ((10./9.)*(cTAU)*(cREF*1e-6)*(rhocld))/(A*ALPH)) ) / 1000. !from Meerkotter & zinner
      computationFlag=1 !valid computation was made
    endif

    !inValid Cloud Retrieval
    if(cREF .le. 0. .or. cTAU .le. 0. .or. cCTH .le. 0.) then
     !Assume the following cloud where CC_TOTAL = 1 but retrieval is bad
      REDAT=10.
      TAUDAT=3.
      Hctop=1.5
      Hcbase = 0.5
    endif
   endif   

!REGIME 3
   !CLOUD-FREE with AEROSOL
   if(regime .eq. 3) then
    LayerType=2 !currently does not matter in BUGSrad -no aerosol
    phaseFlag=1 !set the retrieval to do water cloud

    !Valid AOD retrieval use same values
    if(cREF .gt. 0 .and. aod .gt. 0) then
      REDAT=aREF
      TAUDAT=AOD
      computationFlag=1
    endif

    !Invalid AOD retrieval
    if(cREF .le. 0 .or. aod .le. 0) then
      REDAT=0.05
      TAUDAT=0.05
      Hctop=1.5
      Hcbase=0.5
    endif
   endif

!REGIME 4
   !Missing Regime
   if(regime .eq. 4) then
    LayerType=1
    !Assume the following properties
    REDAT=0.05
    TAUDAT=0.05
    Hctop=1.5
    Hcbase=0.5
   endif




!--Match cloud top/base heights to vertical profile----
!REQUIREMENT: Cloud layer needs to fill an entire bin

   !Cloud thickness
   Hcthick=Hctop-Hcbase

   !Find bin nearest cloud top
   TopID =minloc( abs( Hctop - zz ) )
   BaseID=minloc( abs( Hcbase-zz ) )
   !print*,'Hcthick = ',Hcthick
   !print*,'Hctop = ',Hctop,' TopID: ',TopID
   !print*,'Hcbase = ',Hcbase,' BaseID: ',BaseID

   !BaseID cannot equal TopID (if it is make base 1 level lower)
   if(BaseID(1) .eq. TopID(1)) BaseID(1) = BaseID(1)+1

   !Cloud base is above the surface and at least 1 layer below the top
   if(BaseID(1) .ge. NLS .or. Hcbase .le. zz(NLS)) then
   !print*,'cloud below surface',baseID(1),NLS,zz(NLS)
    BaseID(1) = NLS-1
    if(BaseID(1) .le. TopID(1)) TopID(1) = BaseID(1)-1
   endif

   !Change cloud top and base heights to match vertical bin height
   Hctop = zz(TopID(1))
   Hcbase = zz(BaseID(1))
   !print*,'Hctop = ',Hctop,' TopID: ',TopID
   !print*,'Hcbase = ',Hcbase,' BaseID: ',BaseID
   
   return
end subroutine preprocess_bugsrad
