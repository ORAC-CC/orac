!-------------------------------------------------------------------------------
! Name: driver_for_lut.F90
!
! Purpose:
! Driver program to process the ORAC inputs and convert them to fluxes using generated
! LUTs. The LUT processor is currently located on CEMS in: 
! /home/users/mchristensen/orac/trunk/derived_products/lut
!
! History:
! 2015/01/12, MC: Initial development
!
! Bugs:
! LUT all-sky albedo values of 1.0 and 0. are not processed
!-------------------------------------------------------------------------------

   subroutine driver_for_lut(pxTSI,pxregime,&
                             nASFC,lut_sfc_alb,&
                             nRE,lut_ref,nTAU,lut_cot,nSOLZ,lut_solz,&
                             LUT_toa_sw_albedo,&
                             LUT_boa_sw_transmission,&
                             LUT_boa_sw_albedo,&
                             alb_data,REF,COT,SOLZ,&
                             pxtoalwup,pxtoaswdn,pxtoaswup,&
                             pxboalwup,pxboalwdn,pxboaswdn,pxboaswup,&
                             pxtoalwupclr,pxtoaswupclr,&
                             pxboalwupclr,pxboalwdnclr,pxboaswupclr,pxboaswdnclr,&
                             bpar,bpardif,tpar)

   use common_constants_m

   implicit none

   real, intent(in) :: pxTSI,pxregime
   real, intent(in) :: alb_data(4)
   real, intent(in) :: REF(1),COT(1),SOLZ(1)
   real :: pxASFC  !Surface albedo
   integer(kind=lint), intent(in) :: nASFC,nRE,nTAU,nSOLZ
   real, intent(in) :: lut_sfc_alb(nASFC), &
                       lut_ref(nRE), &
                       lut_cot(nTAU), &
                       lut_solz(nSOLZ)

   real, intent(in) :: LUT_toa_sw_albedo(nASFC,nSOLZ,nRe,nTau)
   real, intent(in) :: LUT_boa_sw_transmission(nASFC,nSOLZ,nRe,nTau)
   real, intent(in) :: LUT_boa_sw_albedo(nASFC,nSOLZ,nRe,nTau)

   !OUTPUT
   real, intent(out) ::  &
      pxtoalwup,pxtoaswdn,pxtoaswup ,& !All-sky TOA fluxes
      pxtoalwupclr,pxtoaswupclr, & !Clear-Sky TOA fluxes
      pxboalwup,pxboalwdn,pxboaswdn,pxboaswup ,& !All-sky BOA fluxes
      pxboalwupclr,pxboalwdnclr,pxboaswdnclr,pxboaswupclr,& !clear-sky BOA fluxes
      tpar   ,&    !toa par total
      bpardif,&    !boa par diffuse
      bpar       !boa par total
   

   !LOCAL
   integer(kind=lint) :: SfcAlbID(1),SolzID(1),RefID(1),CotID(1)
   real, parameter :: b1=0.47, b2=0.28, b3=0.24, b4=-0.158
   real :: pxTOA_AlbedoLUT,pxBOA_TransmissionLUT,pxBOA_AlbedoLUT
   
   pxASFC = b1*alb_data(1) + b2*alb_data(2) + &
            b3*alb_data(3) + b4*alb_data(4)

   SfcAlbID = MINLOC( ABS(pxASFC - lut_sfc_alb))
   SolzID   = MINLOC( ABS(SOLZ(1)-lut_solz))
   RefID    = MINLOC( ABS(REF(1)-lut_ref))
   CotID    = MINLOC( ABS(COT(1)-lut_cot))
    !print*,SOLZ,lut_solz(SolzID),SolzID(1)
    !print*,REF,lut_ref(RefID),RefID(1)
    !print*,COT,lut_cot(CotID),CotID(1)
    !print*,pxASFC,lut_sfc_alb(SfcAlbID),SfcAlbID(1)   

    pxtoaswdn = pxTSI * cos(SOLZ(1)*Pi/180.)


          !With Cloud
          pxTOA_AlbedoLUT = 0.
          pxBOA_TransmissionLUT = 0.
          pxBOA_AlbedoLUT = 0.

          if(pxregime .ne. 4) then 
           pxTOA_AlbedoLUT=LUT_toa_sw_albedo(SfcAlbID(1),SolzID(1),RefID(1),CotID(1))
           pxBOA_TransmissionLUT=LUT_boa_sw_transmission(SfcAlbID(1),SolzID(1),RefID(1),CotID(1))
           pxBOA_AlbedoLUT=LUT_boa_sw_albedo(SfcAlbID(1),SolzID(1),RefID(1),CotID(1))
          endif
          if(pxregime .eq. 4) then 
           pxTOA_AlbedoLUT=LUT_toa_sw_albedo(SfcAlbID(1),SolzID(1),1,1) 
           pxBOA_TransmissionLUT=LUT_boa_sw_transmission(SfcAlbID(1),SolzID(1),1,1)
           pxBOA_AlbedoLUT=LUT_boa_sw_albedo(SfcAlbID(1),SolzID(1),1,1)
          endif

          if(pxTOA_AlbedoLUT .gt. 0. .and. pxTOA_AlbedoLUT .lt. 1.) then
           pxtoaswup = (pxTOA_AlbedoLUT*1.15) * pxtoaswdn
           pxtoalwup = 0.
           pxboaswdn = pxBOA_TransmissionLUT * pxtoaswdn
           pxboaswup = (pxBOA_AlbedoLUT*1.15) * pxBOA_TransmissionLUT * pxtoaswdn
           pxboalwup = 0.
           pxboalwdn = 0.
           bpar = 0.
           bpardif = 0.
           tpar = 0.
          endif
          if(pxTOA_AlbedoLUT .eq. 0. .or. pxTOA_AlbedoLUT .eq. 1.) then
           pxtoaswup = -999.
           pxtoalwup = -999.
           pxboaswdn = -999.
           pxboaswup = -999.
           pxboalwup = -999.
           pxboalwdn = -999.
           bpar = -999.
           bpardif = -999.
           tpar = -999.
          endif

          !Without Cloud
          pxTOA_AlbedoLUT = 0.
          pxBOA_TransmissionLUT = 0.
          pxBOA_AlbedoLUT = 0.

          pxTOA_AlbedoLUT=LUT_toa_sw_albedo(SfcAlbID(1),SolzID(1),1,1)
          pxBOA_TransmissionLUT=LUT_boa_sw_transmission(SfcAlbID(1),SOLZID(1),1,1)
          pxBOA_AlbedoLUT=LUT_boa_sw_albedo(SfcAlbID(1),SolzID(1),1,1)
          if(pxTOA_AlbedoLUT .gt. 0. .and. pxTOA_AlbedoLUT .lt. 1.) then
           pxtoaswupclr = (pxTOA_AlbedoLUT*1.15) * pxtoaswdn
           pxtoalwupclr = 0.
           pxboaswdnclr = pxBOA_TransmissionLUT * pxtoaswdn
           pxboaswupclr = (pxBOA_AlbedoLUT*1.15) * pxBOA_TransmissionLUT * pxtoaswdn
           pxboalwupclr = 0.
           pxboalwdnclr = 0.
          endif
          if(pxTOA_AlbedoLUT .eq. 0. .or. pxTOA_AlbedoLUT .eq. 1.) then
           pxtoaswupclr = -999.
           pxtoalwupclr = -999.
           pxboaswdnclr = -999.
           pxboaswupclr = -999.
           pxboalwupclr = -999.
           pxboalwdnclr = -999.
          endif

!print*,pxtoaswdn,pxtoaswup,pxtoalwup
!print*,pxtoaswupclr,pxtoalwupclr
!print*,pxboaswdn,pxboaswup,pxboalwdn,pxboalwup
!print*,pxboaswdnclr,pxboaswupclr,pxboalwdnclr,pxboalwupclr
!print*,bpar,bpardif,tpar

   return
end subroutine driver_for_lut
