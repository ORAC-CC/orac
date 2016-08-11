!====================================================================
subroutine entropy_interface(eeband,icp,icc)

use FUINPUT ,only : fi , nv1x,nvx,mccx
USE ENTROPY_LW , only: lwe,longwave_entropy_model,shortwave_entropy_model

implicit none
!integer ibt,ig,icc,icp
integer icp,icc
!real hk
!real sumT
real eeband

! Spectral entropy
  real ffu, ffd
  common /dfsout/   ffu(nv1x) , ffd(nv1x)
  
  real tc_t,tc_n,tc_c,tc_p
  common /LWETAUS/ tc_t(nvx,mccx),tc_n(nvx,mccx), tc_c(nvx),tc_p(nvx)
  
 lwe%nlay  = fi%nv
 lwe%nlev  = fi%nv+1
 lwe%Tskin = fi%pts
 lwe%temp(1:lwe%nlev)  = fi%pt(1:lwe%nlev)
 lwe%fup(1:lwe%nlev)   = ffu(1:fi%nv+1)
 lwe%fdn(1:lwe%nlev)   = ffd(1:fi%nv+1)
 
 lwe%fup(lwe%nlev+1)   = 0.0
 lwe%fdn(lwe%nlev+1)   = 0.0
 lwe%eeband =eeband

 if ( icp == 1 .and. icc == 0 )  lwe%tau(1:lwe%nlay) = tc_c(1:fi%nv)
 if ( icp == 1 .and. icc >= 1 )  lwe%tau(1:lwe%nlay) = tc_t(1:fi%nv,icc)
 lwe%tau( 0 ) = 0.0000 ! Insert transmission at above top model layer TOA
 lwe%tau(lwe%nlay+1) = 100.0 ! transmission In pseudo layer Earth Ground layer 

  if ( lwe%swtlwf ) then
   call shortwave_entropy_model
  else
   call longwave_entropy_model 
  endif

  
 RETURN


end subroutine entropy_interface

!========================================================
subroutine lw_spectral_integration(icc,icp,ib,ibt,ig,hk,eeband)

USE FUINPUT
USE FUOUTPUT
USE ENTROPY_LW , only: lwe

implicit none
integer ib,ig,ibt
real hk
integer i ,icc,icp
real ffu, ffd
common /dfsout/   ffu(nv1x) , ffd(nv1x)
real fiur
common /radiance/ fiur(nv1x) 
real bf,bs
common /planci/ bf(nv1x), bs

real eeband
!--------------------------------------------------------------------------------------
          do i = 1, nv1
           focc(icc,icp)%fdir(i)  = focc(icc,icp)%fdir(i) + ffd(i) * hk
           focc(icc,icp)%fuir(i)  = focc(icc,icp)%fuir(i) + ffu(i) * hk
	   focc(icc,icp)%fiurt(i) = focc(icc,icp)%fiurt(i) + fiur(i) * hk * 0.1591549
           end do

!	if ( ibt >= 7 .and. ibt <=20) then
	if ( ibt >= 10 .and. ibt <=23) then
   	 do i = 1, nv1
           foscc(icc,icp)%rlwfu(i,ibt) =  foscc(icc,icp)%rlwfu(i,ibt) + ffu(i) * hk
           foscc(icc,icp)%rlwfd(i,ibt) =  foscc(icc,icp)%rlwfd(i,ibt) + ffd(i) * hk
	   foscc(icc,icp)%sbf(i,ibt) = bf(i)
	   foscc(icc,icp)%sbs(ibt) = bs
  	enddo
	endif

!	if ( ib.eq.11 .or. ib.eq.12 .or. ib.eq.13) then	
	if ( ib.eq.14 .or. ib.eq.15 .or. ib.eq.16) then
		do i=1,nv1
	        focc(icc,icp)%fdwn(i) = focc(icc,icp)%fdwn(i) + ffd(i) * hk
                focc(icc,icp)%fuwn(i) = focc(icc,icp)%fuwn(i) + ffu(i) * hk

           focc(icc,icp)%fiurw(i) = focc(icc,icp)%fiurw(i) + fiur(i) * hk * 0.1591549

		enddo
!TRMM (begin)
! GET Spectral Flux and Radiance for Filtering to TRMM WINDOW
          focc(icc,icp)%fu_sf(ib) = focc(icc,icp)%fu_sf(ib) + ffu(1)  * hk
          focc(icc,icp)%fu_sr(ib) = focc(icc,icp)%fu_sr(ib) + fiur(1) * hk * 0.1591549
!TRMM (end)      
	endif 
!--------------------------------------------------------------------------------------
!print*,'EEE',icc,icp,ibt,ig


 lwe%swtlwf = .false.
 lwe%LL =0
 lwe%spa =0
 if (icp == 1) call entropy_interface(eeband,icp,icc) 
!
!do i = 1, nv1 
! focc(icc,icp)%lwespa(i) = focc(icc,icp)%lwespa(i) + lwe%spa(i) * hk  ! Spectrally Integrate Entropy Production to Space
! focc(icc,icp)%lweatm(i) = focc(icc,icp)%lweatm(i) + lwe%LL(i)  * hk   ! Spectrally Integrate Internal Entropy Exchange
!enddo

 focc(icc,icp)%lw_entropy(1) = focc(icc,icp)%lw_entropy(1) + sum(lwe%spa(1:nv1)) * hk   ! Seiji 20140306
 focc(icc,icp)%lw_entropy(2) = focc(icc,icp)%lw_entropy(2) + sum(lwe%spa(1:nv))  * hk   ! Seiji 20140306
 focc(icc,icp)%lw_entropy(3) = focc(icc,icp)%lw_entropy(3) +     lwe%spa(nv1)    * hk   ! Seiji 20140306
 focc(icc,icp)%lw_entropy(4) = focc(icc,icp)%lw_entropy(4) +     lwe%sfc_up      * hk   ! Seiji 20140306
 focc(icc,icp)%lw_entropy(5) = focc(icc,icp)%lw_entropy(5) +     lwe%sfc_dn      * hk   ! Seiji 20140306

 focc(icc,icp)%lw_entropy(6) = focc(icc,icp)%lw_entropy(6) -  sum(lwe%LL(1:nv))  * hk   ! Seiji 20140306
 focc(icc,icp)%lw_entropy(7) = focc(icc,icp)%lw_entropy(7) -      lwe%LL(nv1)    * hk   ! Seiji 20140306

   
 end subroutine lw_spectral_integration
!========================================================

subroutine sw_spectral_integration(icc,icp,ibseq,hk1,fqu1)

USE FUINPUT
USE FUOUTPUT
USE ENTROPY_LW , only: lwe
implicit none
integer icc,icp
real hk1,hk,fqu1
integer i,ibseq

!Input ---------------------------------------------------
real ffdr, ffdf, ffu, ffd
common /dirdiff/ ffdr(nv1x), ffdf(nv1x)
common /dfsout/   ffu(nv1x) , ffd(nv1x)

hk=hk1*fqu1


!----------------------------------------------------------
!	print*,'SWBand:',ibseq,icc,icp,hk
INTEGRATION : do  i = 1, nv1
!	   print'(3i4,4f10.2)',ib,ig,i,ffd(i),ffu(i),ffdr(i), ffdf(i)
focc(icc,icp)%fds(i)   = focc(icc,icp)%fds(i)   + ffd(i) * hk 
focc(icc,icp)%fus(i)   = focc(icc,icp)%fus(i)   + ffu(i) * hk
focc(icc,icp)%fdsdr(i) = focc(icc,icp)%fdsdr(i) + ffdr(i) * hk
focc(icc,icp)%fdsdf(i) = focc(icc,icp)%fdsdf(i) + ffdf(i) * hk

foscc(icc,icp)%rswfu(i,ibseq)  = foscc(icc,icp)%rswfu(i,ibseq) +ffu(i)  * hk
foscc(icc,icp)%rswfd(i,ibseq)  = foscc(icc,icp)%rswfd(i,ibseq) +ffd(i)  * hk
foscc(icc,icp)%rswdir(i,ibseq) = foscc(icc,icp)%rswdir(i,ibseq)+ffdr(i) * hk
foscc(icc,icp)%rswdif(i,ibseq) = foscc(icc,icp)%rswdif(i,ibseq)+ffdf(i) * hk

enddo INTEGRATION
lwe%swtlwf = .true.
 if (icp == 1) call entropy_interface(0.0,icp,icc) !(icc,icp,ibseq, 0 ,hk) 

!do i = 1, nv1 
! focc(icc,icp)%swespa(i) = focc(icc,icp)%swespa(i) + lwe%SSS(i) * hk  ! Spectrally Integrate Entropy Production to Space
! focc(icc,icp)%sweatm(i) = focc(icc,icp)%sweatm(i) + lwe%SS(i) * hk  ! Spectrally Integrate Entropy Production to Space
!enddo

 focc(icc,icp)%sw_entropy(1) = focc(icc,icp)%sw_entropy(1) + sum(lwe%SSS(1:nv1)) * hk   ! Seiji 20140306
 focc(icc,icp)%sw_entropy(2) = focc(icc,icp)%sw_entropy(2) + sum(lwe%SSS(1:nv))  * hk   ! Seiji 20140306
 focc(icc,icp)%sw_entropy(3) = focc(icc,icp)%sw_entropy(3) +     lwe%SSS(nv1)    * hk   ! Seiji 20140306
 focc(icc,icp)%sw_entropy(4) = focc(icc,icp)%sw_entropy(4) + sum(lwe%SS(1:nv))   * hk   ! Seiji 20140306
 focc(icc,icp)%sw_entropy(5) = focc(icc,icp)%sw_entropy(5) +     lwe%SS(nv1)     * hk   ! Seiji 20140306
 
 

end subroutine sw_spectral_integration
!========================================================
subroutine rapradgw_qftsts(asbs ,icc)
USE FUINPUT
implicit none
real, parameter:: oner4=1.00
real*8, parameter:: oner8=1.00d0
real asbs
!real wc1, wc2, wc3, wc4, wc, tt
real wc1_c, wc2_c, wc3_c, wc4_c, wc_c, tt_c
real w1, w2, w3, w, t, u0a, f0a

real wc1x(nvx),wc1x_clr(nvx)

integer i,jovl
integer icc,ict,icb,iclear
!real oner4
	
! Input
! asbs  surface albedo
! u0    cosine of the solar zenith angle
! Input common blocks
!   wc1 - wc4 : weighted moments 1 thru 4 of the phase function
!   wc : Tau weighted Single Scatter Albedo 
!   tt : summed toa to level(I) optical depth	
! cf_sk(nvx) - cloud fraction at each level
! nu_sk(nvx) - Optical depth variability ( shape parameter 
!              of gamma dist)(Tau/std)**2
! Computed:
! ccf_sk(nvx) - Cumulative cloud fraction at each level ( top to sfc)


real wc1  ,wc2  ,wc3  ,wc4 ,wc   ,tt
common /dfsin/   wc1(nvx), wc2(nvx), wc3(nvx), wc4(nvx), &
                     wc(nvx), tt(nvx)

real wc1_clr  ,wc2_clr  ,wc3_clr  ,wc4_clr ,wc_clr  ,tt_clr
real wc1_tot  ,wc2_tot  ,wc3_tot  ,wc4_tot ,wc_tot  ,tt_tot
 common /toseiji/ wc1_clr(nvx), wc2_clr(nvx), wc3_clr(nvx), wc4_clr(nvx), &
                  wc_clr(nvx),  tt_clr(nvx), &
		 wc1_tot(nvx), wc2_tot(nvx), wc3_tot(nvx), wc4_tot(nvx), &
                  wc_tot(nvx),  tt_tot(nvx)
!-----------------------------------------
! Output common blocks
real ffdr,ffdf,ffu,ffd
common /dirdiff/ ffdr(nv1x), ffdf(nv1x)
common /dfsout/   ffu(nv1x), ffd(nv1x)
!-----------------------------------------

wc1x   = wc1/3.0
wc1x_clr   = wc1_clr/3.0

cf_sk  = 0
ccf_sk = 0
nu_sk  = 0

if ( icc == 0 ) then
 iclear =1 ! CLEAR 
tt  = tt_clr
wc  = wc_clr
wc1x=wc1x_clr

elseif ( fi%fc(icc)%cldfrac > 0.0 .and.  fi%fc(icc)%cldfrac <= 1.0 )then
 iclear=9999 !CLOUD 
 nu_sk (1  :nv)  =  100.
 OVERLAP: do jovl=1,fi%fc(icc)%novl

 ict = fi%fc(icc)%icld_top(jovl)  
 icb = fi%fc(icc)%icld_bot(jovl)
 cf_sk (ict:icb) =   1.0 
 ccf_sk(ict:nv)  =   1.0 
!! nu_sk (1  :nv)  =  shapefactor(icc)
  nu_sk (ict:icb)  = shapefactor(icc,jovl)
enddo OVERLAP

if ( fi%fc(icc)%dpi%ldpi ) then
where ( plwc > 0 .or. piwc > 0 ) 
 cf_sk = 1.0
 ccf_sk = 1.0 
 nu_sk = 100.
endwhere
endif

  
endif


call gwtsa_sw_v20(oner4, nv ,u0 ,wc ,wc1x ,asbs,    	&
              tt ,oner8 ,oner4 ,nu_sk ,cf_sk , 	&
              ccf_sk,				&
	      tt_clr,wc_clr,wc1x_clr,		 &
	      iclear,					&
              ffu  ,ffd  ,ffdr  ,ffdf)


return
end subroutine rapradgw_qftsts

!===============================================================================
     subroutine comscp_0203( isolve_config,icc )
!  *********************************************************************
!  This subroutine is used to  COMbine Single-Scattering Properties  due
!  to  ice crystals,  water droplets, and  Rayleigh molecules along with
!  H2O continuum absorption and nongray gaseous absorption.  See Section
!  3.4 of Fu (1991). wc, wc1, wc2, wc3, and wc4, are total (or combined)
!  single - scattering  albedo,  and   expansion   coefficients  of  the
!  phase function ( 1, 2, 3, and 4 ) in nv layers. tt(nv) are the normal
!  optical depth ( from the top of the atmosphere to a given level ) for
!  level 2 - level nv1( surface ). The single-scattering  properties  of
!  rain and graupel are also incorporated in ( Jan. 19, 1993 ).
!  The single-scattering properties of aerosols are incorporated in 
!  (10/29/96) based on earlier version (5/17/95).
!  *********************************************************************
      USE FUINPUT 
      implicit none
       real,parameter ::taumaxgas = 200.0
      integer isolve_config
 
      integer i,j,iac,icc
      real ,dimension(nvx):: ti,wi,tw,ww
      real ,dimension(nvx,4):: www,wwi

      real tr,wr,wwr,tgm,tg
      real tis,tws,sat
	real wc1  ,wc2  ,wc3  ,wc4 ,wc   ,tt
!	real wc1_c,wc2_c,wc3_c,wc4_c,wc_c,tt_c


         real tc_c,tc_p
         real tc_t,tc_n	 
         common /LWETAUS/ tc_t(nvx,mccx),tc_n(nvx,mccx), tc_c(nvx),tc_p(nvx)
	 
	 
	real fw_t,fw_c,fw_p,fw_n
     
       integer ict,icb
       integer method
       real cftot_wat,cftot_ice
!---------- 10/29/96 (4)
      real tae,wae,wwae,taes(0:4)
      common /aer/ tae(nvx,mxac), wae(nvx,mxac), wwae(nvx,4,mxac)
!---------- 10/29/96 (4)
      
!      common /ic/ ti(nvx), wi(nvx), wwi(nvx,4)
!      common /wat/ tw(nvx), ww(nvx), www(nvx,4)
!      common /rai/ trn(nvx), wrn(nvx), wwrn(nvx,4)
!      common /gra/ tgr(nvx), wgr(nvx), wwgr(nvx,4)

      common /ray/ tr(nvx), wr(nvx), wwr(nvx,4)
      common /con/ tgm(nvx)
      common /gas/ tg(nvx)

!COMMON BLOCKS For Seiji's Solver Routine.

 common /dfsin/   wc1(nvx), wc2(nvx), wc3(nvx), wc4(nvx), &
                     wc(nvx), tt(nvx)

real wc1_clr  ,wc2_clr  ,wc3_clr  ,wc4_clr ,wc_clr  ,tt_clr
real wc1_tot  ,wc2_tot  ,wc3_tot  ,wc4_tot ,wc_tot  ,tt_tot
 common /toseiji/ wc1_clr(nvx), wc2_clr(nvx), wc3_clr(nvx), wc4_clr(nvx), &
                  wc_clr(nvx),  tt_clr(nvx), &
		 wc1_tot(nvx), wc2_tot(nvx), wc3_tot(nvx), wc4_tot(nvx), &
                  wc_tot(nvx),  tt_tot(nvx)

real,dimension(nvx),save:: wc1_p, wc2_p, wc3_p, wc4_p, wc_p,  tt_p
real,dimension(nvx),save:: wc1_c, wc2_c, wc3_c, wc4_c, wc_c,  tt_c
real,dimension(nvx,mccx),save:: wc1_t, wc2_t, wc3_t, wc4_t, wc_t,  tt_t
real,dimension(nvx,mccx),save:: wc1_n, wc2_n, wc3_n, wc4_n, wc_n,  tt_n


! TOTAL SKY 		: GAS + AEROSOL + CLOUD 
! CLEAR SKY 		: GAS + AEROSOL
! PRISTINE SKY		: GAS
! NoAEROSOL CLD SKY	: GAS + CLOUD 

if ( isolve_config == 0 ) then !GENERATE COMBINED PROPERTIES

LEVELS0 : do  i = 1, nv

if ( tg(i) > taumaxgas ) tg(i) = taumaxgas ! Limit Gas Optical depth...
sat = sum ( tae(i,1:nac) ) ! Sum AOT of aerosol constit.


tc_p(i) =  tr(i) + tgm(i) + tg(i)  ! Rayleigh , Continuum ,Gasses
tc_c(i) =  tc_p(i) + sat


taes(0:4) = 0.0
 do iac = 1,nac
  taes(0)=taes(0)+tae(i,iac)*wae(i,iac)
   do j=1,4
    taes(j)=taes(j)+tae(i,iac)*wae(i,iac)*wwae(i,j,iac)
   enddo
 enddo

        fw_c =             tr(i) + taes(0) !+ trns + tgrs 
	fw_p =             tr(i)	   !+ trns + tgrs

       wc_c(i)	     =  fw_c / tc_c(i)
       wc_p(i)	     =  fw_p / tc_p(i)

!----- CLEAR SKY ------------------------------------------------->
       if ( fw_c .lt. 1.0e-20 ) then
      wc1_c(i) = 0.0 ; wc2_c(i) = 0.0 ; wc3_c(i) = 0.0 ; wc4_c(i) = 0.0
       else
         wc1_c(i) = ( tr(i) * wwr(i,1) + taes(1) )/fw_c
         wc2_c(i) = ( tr(i) * wwr(i,2) + taes(2) )/fw_c
         wc3_c(i) = ( tr(i) * wwr(i,3) + taes(3) )/fw_c
         wc4_c(i) = ( tr(i) * wwr(i,4) + taes(4) )/fw_c
       endif
!----- CLEAR SKY -------------------------------------------------<
!----- PRISTINE SKY ------------------------------------------------->
       if ( fw_p .lt. 1.0e-20 ) then
      wc1_p(i) = 0.0 ; wc2_p(i) = 0.0 ; wc3_p(i) = 0.0 ; wc4_p(i) = 0.0
       else
        wc1_p(i) = ( tr(i) * wwr(i,1)  )/fw_p
        wc2_p(i) = ( tr(i) * wwr(i,2)  )/fw_p
        wc3_p(i) = ( tr(i) * wwr(i,3)  )/fw_p
        wc4_p(i) = ( tr(i) * wwr(i,4)  )/fw_p
       endif
!----- PRISTINE SKY -------------------------------------------------<

enddo LEVELS0

tt_c(1)     = tc_c(1)
tt_p(1)     = tc_p(1)


do  i = 2, nv
tt_c(i)     = tt_c(i-1) + tc_c(i)
tt_p(i)     = tt_p(i-1) + tc_p(i)
enddo

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if ( fi%lscm(2) .or. fi%lscm(4)  ) then !! ONLY COMPUTE FOR CLOUDYSKY MODES
else
 return
endif


!---------------------------------------------------------

CLOUD_CONDITIONS : do icc = 1, mccx


if ( fi%fc(icc)%cldfrac > 0.0 ) then  

 tw(1:nv)      =  optwat(1:nv,1,icc) 	   
 ww(1:nv)      =  optwat(1:nv,2,icc)	   
 www(1:nv,1:4) =  optwat(1:nv,3:6,icc) 
  
 ti(1:nv)      =  optice(1:nv,1,icc)  
 wi(1:nv)      =  optice(1:nv,2,icc)	   
 wwi(1:nv,1:4) =  optice(1:nv,3:6,icc) 



LEVELS : do  i = 1, nv

if ( tg(i) > taumaxgas ) tg(i) = taumaxgas ! Limit Gas Optical depth...
sat = sum ( tae(i,1:nac) ) ! Sum AOT of aerosol constit.

tc_n(i,icc) = ti(i) + tw(i) + tr(i) + tgm(i) + tg(i)  
tc_t(i,icc) = tc_n(i,icc)   + sat             
   
!!!
taes(0:4) = 0.0
 do iac = 1,nac
  taes(0)=taes(0)+tae(i,iac)*wae(i,iac)
   do j=1,4
    taes(j)=taes(j)+tae(i,iac)*wae(i,iac)*wwae(i,j,iac)
   enddo
 enddo


tis = ti(i) * wi(i)
tws = tw(i) * ww(i)

fw_t = tis + tws + tr(i) + taes(0) !+ trns + tgrs 	 
fw_n = tis + tws + tr(i)	   !+ trns + tgrs

wc_t(i,icc)   =  fw_t / tc_t(i,icc)
wc_n(i,icc)   =  fw_n / tc_n(i,icc)


!----- CLOUDY SKY ------------------------------------------------->
!23456789012345678901234567890123456789012345678901234567890123456789012
       if ( fw_t .lt. 1.0e-20 ) then
     wc1_t(i,icc) = 0.0 ; wc2_t(i,icc) = 0.0 
     wc3_t(i,icc) = 0.0 ; wc4_t(i,icc) = 0.0
       else
        wc1_t(i,icc) = ( tis * wwi(i,1) + tws * www(i,1) + &
           tr(i) * wwr(i,1) + taes(1) )/fw_t

        wc2_t(i,icc) = ( tis * wwi(i,2) + tws * www(i,2) + &
           tr(i) * wwr(i,2) + taes(2) )/fw_t

        wc3_t(i,icc) = ( tis * wwi(i,3) + tws * www(i,3) + &
           tr(i) * wwr(i,3) + taes(3) )/fw_t

        wc4_t(i,icc) = ( tis * wwi(i,4) + tws * www(i,4) + &
           tr(i) * wwr(i,4) + taes(4) )/fw_t
       endif
!----- CLOUDY SKY -------------------------------------------------<

!----- NO AEROSOL CLOUD  SKY ------------------------------------------------->

       if ( fw_n .lt. 1.0e-20 ) then
       wc1_n(i,icc) = 0.0 ; wc2_n(i,icc) = 0.0 
       wc3_n(i,icc) = 0.0 ; wc4_n(i,icc) = 0.0
       else
        wc1_n(i,icc) = ( tis * wwi(i,1) + tws * www(i,1) +  &
        tr(i) * wwr(i,1)  )/fw_n

        wc2_n(i,icc) = ( tis * wwi(i,2) + tws * www(i,2) +  &
        tr(i) * wwr(i,2)  )/fw_n

        wc3_n(i,icc) = ( tis * wwi(i,3) + tws * www(i,3) +  &
        tr(i) * wwr(i,3)  )/fw_n

        wc4_n(i,icc) = ( tis * wwi(i,4) + tws * www(i,4) +  &
        tr(i) * wwr(i,4)  )/fw_n
       endif
!----- NO AEROSOL CLOUD  SKY -------------------------------------------------<
enddo LEVELS


tt_t(1,icc) = tc_t(1,icc)
tt_n(1,icc) = tc_n(1,icc)

do  i = 2, nv
tt_t(i,icc) = tt_t(i-1,icc) + tc_t(i,icc)
tt_n(i,icc) = tt_n(i-1,icc) + tc_n(i,icc)
!print'(I4,4(4f8.3,3x))',i &
!  ,tc_t(i,icc),tt_t(i),wc_t(i),wc1_t(i)/3.0 	&
!  ,tc_c(i),tt_c(i),wc_c(i),wc1_c(i)/3.0 	&
!  ,tc_p(i),tt_p(i),wc_p(i),wc1_p(i)/3.0		&
!  ,tc_n(i,icc),tt_n(i),wc_n(i),wc1_n(i)/3.0
enddo

endif !cloud Frac >0

enddo CLOUD_CONDITIONS


return

!------------------------------------------------------------------------------
!USE PRECOMPUTED Properties----FuMode------------------------------------------
!------------------------------------------------------------------------------

 elseif ( isolve_config == 1 ) then !CLEAR SKY
  wc(1:nv)  =  wc_c(1:nv)
  wc1(1:nv) = wc1_c(1:nv)
  wc2(1:nv) = wc2_c(1:nv) 
  wc3(1:nv) = wc3_c(1:nv) 
  wc4(1:nv) = wc4_c(1:nv) 
  tt(1:nv)  =  tt_c(1:nv)

!!Clear Sky for Seiji's Solver
 wc_clr(1:nv) =   wc_c(1:nv)
wc1_clr(1:nv) =  wc1_c(1:nv)
wc3_clr(1:nv) =  wc2_c(1:nv)
wc3_clr(1:nv) =  wc3_c(1:nv)
wc4_clr(1:nv) =  wc4_c(1:nv)
 tt_clr(1:nv) =   tt_c(1:nv)

 elseif ( isolve_config == 2 ) then !TOTAL SKY
  wc1(1:nv) = wc1_t(1:nv,icc) 
  wc2(1:nv) = wc2_t(1:nv,icc) 
  wc3(1:nv) = wc3_t(1:nv,icc) 
  wc4(1:nv) = wc4_t(1:nv,icc) 
  wc(1:nv)  =  wc_t(1:nv,icc)
  tt(1:nv)  =  tt_t(1:nv,icc) 
  tt_tot(1:nv)  =  tt_t(1:nv,icc) !!!!!! 20121127 
!!Clear Sky for Seiji's Solver
 wc_clr(1:nv) =   wc_c(1:nv)
wc1_clr(1:nv) =  wc1_c(1:nv)
wc3_clr(1:nv) =  wc2_c(1:nv)
wc3_clr(1:nv) =  wc3_c(1:nv)
wc4_clr(1:nv) =  wc4_c(1:nv)
 tt_clr(1:nv) =   tt_c(1:nv)

!print'(40f8.2)',tt_clr(1:nv)

 elseif ( isolve_config == 3 ) then !PRISTINE SKY
  wc(1:nv)  =  wc_p(1:nv)
  wc1(1:nv) = wc1_p(1:nv) 
  wc2(1:nv) = wc2_p(1:nv) 
  wc3(1:nv) = wc3_p(1:nv) 
  wc4(1:nv) = wc4_p(1:nv) 
  tt(1:nv)  =  tt_p(1:nv)
!!Pristine Sky for Seiji's Solver
 wc_clr(1:nv) =   wc_p(1:nv)
wc1_clr(1:nv) =  wc1_p(1:nv)
wc3_clr(1:nv) =  wc2_p(1:nv)
wc3_clr(1:nv) =  wc3_p(1:nv)
wc4_clr(1:nv) =  wc4_p(1:nv)
 tt_clr(1:nv) =   tt_p(1:nv)
 elseif ( isolve_config == 4 ) then !NOAEROSOL CLOUDY SKY
  wc(1:nv)  =  wc_n(1:nv,icc)
  wc1(1:nv) = wc1_n(1:nv,icc)
  wc2(1:nv) = wc2_n(1:nv,icc)
  wc3(1:nv) = wc3_n(1:nv,icc)
  wc4(1:nv) = wc4_n(1:nv,icc) 
  tt(1:nv)  =  tt_n(1:nv,icc) 


!!Pristine Sky for Seiji's Solver
 wc_clr(1:nv) =   wc_p(1:nv)
wc1_clr(1:nv) =  wc1_p(1:nv)
wc3_clr(1:nv) =  wc2_p(1:nv)
wc3_clr(1:nv) =  wc3_p(1:nv)
wc4_clr(1:nv) =  wc4_p(1:nv)
 tt_clr(1:nv) =   tt_p(1:nv)
 endif


return
end subroutine  comscp_0203
!==========================================================================
!=============================================================

!===========================================================================
real function p_mle_seiji2(avgtau,avg_ltau)

D = log(avgtau) - avg_ltau

if ( D == 0.0) then
  D = 1.0E-06
endif  
if( D <= 0.5772) then
rnu = (0.5000876 + 0.1648852 * D - 0.0544274 * D**2) / D
elseif ( D > 0.5772 .and. d <=17) then
rnu = (8.898919 + 9.059950 * D + 0.9775373 * D** 2) / (17.79728 * D + 11.968477 * D**2 + D**3)
else
rnu= -9999.
endif
p_mle_seiji2 =rnu

end function p_mle_seiji2


