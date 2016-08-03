MODULE FULIOUMULTI
use FUINPUT ,only : fi , set_default_options_fu, check_inputs_fu
use FUOUTPUT,only : fo ,focc,fos,foscc ,ftoa,fsfc , fouv
use  VLA_FU ,only : prepare_model_profile_fu,vla_interface_fu, &
 print_vla_in,print_vla_out,vo

implicit none
contains
!=========================================================================
subroutine rad_multi_fu

USE FUINPUT
USE FUOUTPUT
USE WSU_LUT_INTERFACE , only :  uvco,wsu_lut
USE ICEDIRSFC , only:dircorrect      
implicit none

integer kg(mbx+2)
integer i,j,ib,ig,mbn,iac,icp,icc,iccb,icce,idum,ibcont,mbend
real f0,fuq1,xx,asx
real hk
integer icmf
integer ibt
real eeband
real p_mle_seiji2


real ffdr,ffdf
common /dirdiff/ffdr(nv1x),ffdf(nv1x)

real fiur
common /radiance/ fiur(nv1x) 
real hk1, fk1o3,sol_spect,fk1h2o
 common /band1/ hk1(mby), fk1o3(mby),sol_spect(0:mbsx+1),fk1h2o(mby)

!  ******************************************************************
!  kg(mb+2) is the number of intervals to perform the g-quadrature in
!  each band to consider the nongray gaseous absorption.  In total,
!  we need to perform "sum(kg)" spectral calculations in  the  scattering
!  problem for each atmospheric profile.
!  ******************************************************************
! data kg / 10, 8, 12, 7, 12, 5, &
!            2, 3, 4, 4, 3, 5, 2, 10, 12, 7, 7, 8, &
!            5,5 /
data kg / 10, &
          8,8,8,8, & 
	    12, 7, 12, 5, &
            2, 3, 4, 4, 3, 5, 2, 10, 12, 7, 7, 8, &
            5,5 /
 call check_inputs_fu !! resets error flag!
if (fi%ierror .ne. 0 ) return !! ABORT bad inputs !
 call assigninputs !!! PULL GLOBAL VARIABLES FROM INPUT STRUCTURE

!! Depending on K's option # of Solver loops needed changes...


if ( idkfr == 0) kg(14:16) = (/3,5,2/)
if ( idkfr == 1) kg(14:16) = (/80,40,10/)
if ( idkfr == 2) kg(14:16) = (/5,5,2/)

!   kg(2:mbsx) = (/7,7,7,7, 8,7,8,7/) !ht02 sav SEIJI HITRAN
   if ( fi%nirold(4) ) then
    kg(2:mbsx) = (/7,7,7,7, 8,7,8,7/) !TEST K2C
   else
!    kg(2:mbsx) = (/8,6,8,7, 8,7,8,7/) !K2c
    kg(2:mbsx) = (/8,6,8,7, 9,7,8,7/) !K2c
   endif
!  *********************************************************************
!  The following variables are declared here, instead of in the calling
!  program, since any change in these would require modifications
!  the code anyway.  A check is inserted here, to make sure the 
!  number of layers given by the user, nv, is less than or equal
!  to the number of levels nvx, given in the header file rad_0598.h.
!  nvx is used for dimensioning arrays.  Uncomment to use.
!  *********************************************************************
nv1    = nv+1      ! Number of levels = LAYERS +1
mb     = mbx  ! 18        ! Number of bands
mbs    = mbsx !6         ! number of shortwave bands
mbir   = 12        ! number of longwave bands
nc     = 8         ! number of cloud types
ndfs   = nv        ! number of layers
mdfs   = nv1       ! number of levels
ndfs4  = 4*ndfs    ! number of layers * 4
ndfs2  = 2*ndfs    ! number of layers * 2
f0     = 1.0 / 3.14159

call flx_initalize

call thicks

call rayle2

if ( u0 .le. 1.0e-4 ) then
  mbn = mbs + 1    ! Longwave only if sun is too low
else
  mbn = 1          ! Shortwave and longwave
endif


mbend = mb+nhb
if ( fi%swonlycomp ) then
mbn   = 1
mbend = mbs
endif

 call aerosol_init

 call ckd1_init

if ( lband6a ) then
  fuq1 = ss /  sol_spect(0)  
else
  fuq1 = ss / ( sol_spect(0) - sol_spect(mbsx+1) )
endif


if ( fi%isksolve == 1  ) then
 CLOUD_CONDITION00: do icc = 1,mccx
  if ( fi%fc(icc)%cldfrac > 0.0 ) then

	OVERLAP00: do j=1,fi%fc(icc)%novl
    if( fi%fc(icc)%sc(j)%shapef > 0 ) then 
     shapefactor(icc,j) = fi%fc(icc)%sc(j)%shapef 
    else
       shapefactor(icc,j)=	    &
        p_mle_seiji2( fi%fc(icc)%sc(j)%mn_lin_tau ,&
	       log(fi%fc(icc)%tau_vis(j) ) )
	 if ( shapefactor(icc,j) > 1E+3)  shapefactor(icc,j) = 1E+3
    endif

        enddo OVERLAP00
	call gwtsa_correction(icc)
     
  endif
enddo CLOUD_CONDITION00

endif



BAND_LOOP : do  ibt = mbn, mbend ! mb+nhb

!! WATCH CAREFULLY the use of ib and ibt
! ibt (1-23) band index over added hidden bands 
! ib (1-21) band index over reported bands

ib=ibt

if ( ibt == 22  ) ib =10 !!!! (19)2850:2500cm-1 
if ( ibt == 23  ) ib =10 !!!! (20)2500:2200cm-1


if (ib.ne.1) call aerosolxy (ib,'x')

if (irobckd .eq. 0 ) then
 call gascon_mt_parm ( ib )  !! Ma and Tipping (2002)
elseif (irobckd .eq.1) then !'Set Continuum Model 0= Ma_Tip 1=Roberts 2=CKD_2.1 3=NONE, 4=PARM CKD_2.1 5=PARM CKD_2.4'
 call gascon ( ib  )  !! OLD ROBERTS CONTINUUM
elseif (irobckd .eq.2) then
! call gascon_ckd ( ib  ) !! EXACT CKD_2.1 (SLOW)
elseif (irobckd .eq.3 ) then
 call gascon_off      !! Option to Turn off Continuum Absorbtion
elseif (irobckd .gt. 3 ) then
 call gascon_ckd_parm(ib ) !! Parameterized CKD_2.1 Cont. Abs. OR CKD_2.4

endif

if ( ibt >mbx) 	 call gascon_off 		
if ( ibt .gt. mbs )  call planck ( ibt )


!-------------------------------------------------------------
K_LOOP: do  ig = 1, kg(ibt)
!----------------
!AEROSOLS
if (ib.eq.1) call aerosolxy (ig,'y')

!----------------
!RAYLEIGH
 call rayle ( ib, ig )

!----------------
!GASES        
if ( ( (ibt>=2 .and. ibt<=9) &
   .or. (ibt==1 .and. ig >=9) )  ) then
   call seijifu_ht02a_sav( ibt, ig, hk )
else
  call gases ( ibt, ig, hk )
endif

!----------------
!CLOUDS
if (fi%lscm(2) .or. fi%lscm(4) ) then

 CLOUD_CONDITION1: do icc = 1,mccx
 if ( fi%fc(icc)%cldfrac > 0.0 ) then 
  if (  ig == 1 .or. ib == 1 ) then   !once per band moved here 20140128


 
  if(fi%fc(icc)%dpi%ldpi) then
   call cloud_input_dpi(icc,ib)
  else	
   call cloud_input(icc,ib)
  endif


!if (  ig == 1 .or. ib == 1 ) then   !once per band orig location
   call icenew ( ib,ig,icc) ! 	fl93i = .false. !! False = New 98Ice cld 

! Use Yong Hu's Water cloud optical properties for SW bands
  if ( ib <= mbsx ) then
   call water_hu ( ib,ig,icc ) ! CALL Yong Hu's WATER CLOUD OPTICS For SW bands ib=1:6
  else
   call water ( ib,icc )
  endif

 endif ! once per band


 endif ! CloudFrac >0
enddo CLOUD_CONDITION1
endif
! print *, "idum = ", idum
 call comscp_0203(0,idum) ! 0=construct,
		     ! 1=Clear,2=Total,3=Pristine,4=NoaerCLoud 
		     
 
 call solver_configuration(ib,ibt,ig,f0,hk,fuq1)


!print*,'BAND',ib,ibt,ig
enddo K_LOOP
enddo BAND_LOOP

 call dircorrect

!--------------------------------------------------------------------
! Combine Cloud Condition Componets into Total Sky..	
!--------------------------------------------------------------------

 call aux_flx_calc(0,1)	     ! Heat rate ,Net, Window emmulation ...
 fo(1) = focc(0,1)	     !!Clear
 fos(1) =foscc(0,1)

 call flx_cloudcombine(2,1)  ! CLOUDY With Aerosols
call aux_flx_calc(0,2)
 fo(3) = focc(0,2)	     !!Pristine 
 fos(3) =foscc(0,2)

 call flx_cloudcombine(4,2)  ! CLOUDY NO Aerosols
call only_toa_sfc
call wsu_lut

return

end subroutine rad_multi_fu
!================================================================================

!================================================================================
subroutine solver_configuration(ib,ibt,ig,f0,hk,fuq1)

USE FUINPUT
USE FUOUTPUT
implicit none
integer ib,ibt,ig
real f0,hk,fuq1
real asx,eeband
integer icmf,iccb,icce
integer icc,icp 
integer ibseq
logical iok
SOLVER_CONFIG : do icmf = 1,nscm

if (icmf == 1 .or. icmf == 2 ) icp=1
if (icmf == 3 .or. icmf == 4 ) icp=2

if (fi%lscm(icmf) ) then 

if (  icmf == 1 .or. icmf == 3 ) then
 iccb = 0   ;  icce = 0
else 
 iccb = 1   ;  icce = mccx
endif

 CLOUD_CONDITION2 : do icc=iccb,icce

 iok=.false.
 if ( icc == 0 ) iok= .true.
 if ( icc > 0 ) then
  if( fi%fc(icc)%cldfrac > 0.0 ) iok=.true.
 endif
 
if ( iok)  then

 call comscp_0203(icmf,icc )  
if ( ib .le. mbs ) then  !! SHORTWAVE CALLS

if ( ib == 1 ) then
 ibseq=ig
!elseif ( ib >= 2 .and. ib <=6) then
elseif ( ib >= 2 .and. ib <=9) then
 ibseq=ib+9
endif 

asx = fi%sfcalb(ibseq,icp,icc)





IF (fi%HYBRID_SW_SOLVER) then
!print'(5I5,f8.2)',icmf,ib,ibt,ig,icc
 if ( icc == 0 )then
  call qfts ( ib, asx, f0 )  !! FOUR STREAM SOLIR CLEAR SKY
! if (ib==1 .and. ig ==1) print*, ' HYBRID CLR 4stream'
 else
 quadra = .false. ; hemisp = .false. ; edding = .true.  !! Original

  if ( shapefactor(icc,1) < 10 ) then
  call rapradgw_qftsts(asx,icc ) ! SEIJI_KATO SOLVER
!  if (ib==1 .and. ig ==1)print*, 'HYBRID CLD GWTSA 2str'
  else
! if (ib==1 .and. ig ==1)print*, ' HYBRID CLD Fu 2stream'
!   call qftsts ( ib, asx, f0 ) ! TWO STREAM SW FULIOU
    call qfts ( ib, asx, f0 )  !! FOUR STREAM SOLIR CLEAR SKY
  endif
 endif
  
 
 
ELSE !STANDARD SETUP
   if ( fourssl ) then
    call qfts ( ib, asx, f0 )  !! FOUR STREAM SOLIR	
   else
    quadra = .false. ; hemisp = .false. ; edding = .true.  !! Original
!  quadra = .true. ; hemisp = .false. ; edding = .false.

!   print'(5I5,f8.2)',isksolve,ib,ibt,ig,icc,shapefactor(icc,1) 
   if (isksolve == 0) call qftsts ( ib, asx, f0 ) ! TWO STREAM SW FULIOU
   if (isksolve == 1) call rapradgw_qftsts(asx,icc ) ! SEIJI_KATO SOLVER

   if (fi%ierror .NE. 0) return
  endif
ENDIF !HYBRID_SW_SOLVER






   call sw_spectral_integration(icc,icp,ibseq,hk,fuq1)
else !!!THERMAL
!-------------------------------------------------------------------
 eeband =   ee(ib-mbs)

if (ibt == 22 .or. ibt== 23 ) eeband = ee(1) 


 if ( foursir ) then
  call qfti ( ib, eeband ) !FOUR STREAM IR
 else
  quadra=.false.;edding=.false.;hemisp=.true.;mquadr=.false.
  call qftisf ( ib, eeband) !TWO-FOUR STREAM COMB IR
! call qftits ( ib, eeband ) !TWO-STREAM IR
  if (fi%ierror .NE. 0) return
 endif

 call lw_spectral_integration(icc,icp,ib,ibt,ig,hk,eeband)
 
 
 
endif !SOLAR OR THERMAL

endif
enddo CLOUD_CONDITION2

endif
enddo SOLVER_CONFIG

end subroutine solver_configuration

END MODULE FULIOUMULTI
