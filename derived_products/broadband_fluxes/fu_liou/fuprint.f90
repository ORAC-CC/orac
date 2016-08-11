!=========================================================================================
subroutine print_in_fu
USE FUINPUT
USE EXTRAS ,only : sh_rh
implicit none
integer i,k,j,mxxx,kk
integer icp,icc
real rh(nv1x)
integer iac(mccx)

character*12 lab12
!!!-----
call check_inputs_fu
if ( fi%ierror .ne. 0 ) then
 print*,'message from check_inputs_fu INPUT ERROR : fi%ierror =',fi%ierror
! stop ' from print_in_fu'
endif


print*,('=',k=1,120)
print*,'Fu-Liou Model inputs in structure fi%  Begin'
print*,'# of Model LAYERS      :',fi%nv
print*,'Solver Config Modes    :',fi%lscm(1:nscm)
print*,'Curved Earth Airmass Co:',fi%curvedearth
print*,'nirold Ray,Ice,Wat,Gas,Kwc :',fi%nirold(1:5)
print*,'Solar Constant (wm-2)  :',fi%ss
print*,'Cosine Solar Zenith    :',fi%u0
print*,'Cosine View Zenith     :',fi%ur
print*,'fu%txt                 :',fi%txt 

print'(a25,12f6.3)','Spect Emissivity 	:',fi%ee
print*,'Skin Temperture (k)    :',fi%pts

print*,'Trace Gas Concentration__________________________________________________________'
print*,'CO2 Conc (ppmv)        :',fi%umco2
print*,'CH4 Conc (ppmv)        :',fi%umch4
print*,'N2O Conc (ppmv)        :',fi%umn2o
print*,'CFCs Conc (ppv)        :',fi%cfc_conc

print*,'Option Selection_________________________________________________________________'
print'(a25,l6)','>4 micron solar lband6a:',fi%lband6a
print'(a25,i6)','Continuum option sel	:',fi%irobckd
print'(a25,i6)','# of LW bands >2200cm-1:',fi%nhb
print'(a25,l6)','Hybrid solver option   :',fi%HYBRID_SW_SOLVER
print'(a25,i6)','Solver option          :',fi%isksolve
print'(a25,i6)','Window instrument	:',fi%instrument

print'(a25,l6)','Fourstream Sol fourssl :',fi%fourssl
print'(a25,l6)','Fourstream IR  foursir :',fi%foursir

print'(a25,i6)','Cloud lwc profile flag :',fi%wp_hgt_flag

print*,'Aerosols__________________________________________________________________________'
print*,'#Aerosol Taus		:',fi%n_atau
print*,'#Aerosol Constituents	:',fi%nac

print'(a25,12f8.3)','Aer.Wavelength(s)(micron):',fi%a_wli(1:fi%n_atau)
do i=1, fi%nac
print*,'-Aerosol Type 		:',fi%itps(i)
print'(a25,12f8.5)','Aer. Optical Depth(s)    :',fi%a_taus(1:fi%n_atau,i)
enddo
print*,'Profiles__________________________________________________________________________'
print*,'Level.Pres(hPa).Temp(K).H20(g/g).RH(%)..O3(g/g)...AOT%PROFILES'
do i=1,fi%nv+1
!call ql_rh( rh(i),fi%pt(i),fi%pp(i),fi%ph(i))
rh(i)= sh_rh(fi%pt(i),fi%pp(i),fi%ph(i) )
print'(i6,2f9.2,es9.2,f6.1,es9.2,7f6.2)',	&
 i,fi%pp(i),fi%pt(i),fi%ph(i),rh(i),fi%po(i),fi%aprofs(i,1:fi%nac)

enddo


do icp = 1,2
if ( icp==1) then
print*,'Spectral Surface Albedo WITH AEROSOLS::'
lab12(1:5)='w/AOT'
elseif ( icp==2) then
print*,'Spectral Surface Albedo WITHOUT Aerosol::'
lab12(1:5)='NOAOT'
endif
lab12(6:12)=' CLEAR'
do icc = 0,mccx
if (icc >0)lab12(6:12)=' Cloud'

if ( icc == 0 )&
print'(a25,a12,I2,3x,18f6.3)','Spect Surface albedo:',lab12,icc,fi%sfcalb(1:18,icp,icc)

if (icc > 0 ) then
if( fi%fc(icc)%cldfrac >0 ) &
print'(a25,a12,I2,3x,18f6.3)','Spect Surface albedo:',lab12,icc,fi%sfcalb(1:18,icp,icc)
endif

enddo
enddo

!---------------------------------------------------------------

kk=0
do k=1,mccx

if ( fi%fc(k)%cldfrac > 0 ) then
 kk=kk+1
 iac(kk) = k 
endif 

enddo




print'(80a1)',('=',k=1,80)

! DPI CLOUD Input Mode
 do k = 1,kk
if ( fi%fc(iac(k))%dpi%ldpi ) then
 print'(a32,2I8,f8.3)', 'Direct Lwc/Iwc Cloud Input Mode',k,iac(k),fi%fc(iac(k))%cldfrac
 print*,' Lay#  CC# PresTop PresBot Lwc/Iwc   Re/De   Phase'
  do i=1,fi%nv
   if( fi%fc(iac(k))%dpi%plwc_iwc(i) > 0 ) &
   print'(2i5,2f8.2,f8.4,f8.1,I8)' ,i,iac(k),fi%pp(i),fi%pp(i+1),&
  fi%fc(iac(k))%dpi%plwc_iwc(i),&
  fi%fc(iac(k))%dpi%pre_de(i),&
  fi%fc(iac(k))%dpi%phase(i)
  enddo
  print'(80a1)',('-',i=1,50)
 endif
 enddo

!Standard Cloud Input

print'(a10,I5,30I8)','  CLOUDS::',kk,iac(1:kk)
print'(a15,30f8.3)','Fractions :',(fi%fc(iac(k))%cldfrac,k=1,kk)
print'(a15,30l8)',  'DPI   mode:',(fi%fc(iac(k))%dpi%ldpi,k=1,kk)
print'(a15,30i8)'  ,'#Overlaps :',(fi%fc(iac(k))%novl,k=1,kk)
mxxx = maxval(fi%fc(1:mccx)%novl)
OVERLAP : do j=1,mxxx
print'(25a1)',('-',k=1,25)
print'(a15,30f8.3)','Opt Depth :',(fi%fc(iac(k))%tau_vis(j),k=1,kk)
!print'(a15,30I8)  ','Phase     :',(fi%fc(iac(k))%iphase	(j),k=1,kk)
!print'(a15,30f8.1)','Part.Size :',(fi%fc(iac(k))%part_size(j),k=1,kk)

print'(a15,30f8.2)  ','RPhase    :',(fi%fc(iac(k))%rphase(j),k=1,kk)
print'(a15,30f8.2)  ','Re        :',(fi%fc(iac(k))%re(j),k=1,kk)
print'(a15,30f8.2)  ','De        :',(fi%fc(iac(k))%de(j),k=1,kk)

print'(a15,30(I4,i4))  ','Top:Bot Lay#:',(fi%fc(iac(k))%icld_top(j),fi%fc(iac(k))%icld_bot(j),k=1,kk)
print'(a15,30(I4,I4))','Top:Bot Pres:',( nint( fi%pp(fi%fc(iac(k))%icld_top(j) )), &
                                         nint( fi%pp(fi%fc(iac(k))%icld_bot(j)+1)) ,k=1,kk)


!print'(a15,30I7)  ','Bot Layer :',(fi%fc(iac(k))%icld_bot (j),k=1,kk)

print'(a15,30f8.1)','Nu .......:',(fi%fc(iac(k))%sc(j)%shapef  ,k=1,kk)  
print'(a15,30f8.1)','Mn_lin_tau:',(fi%fc(iac(k))%sc(j)%mn_lin_tau,k=1,kk)
enddo OVERLAP

print*,'Fu-Liou Model inputs in structure fi%  End'
print*,('=',k=1,120)

end subroutine print_in_fu

!============================================================================
subroutine print_out_fu
USE FUINPUT
USE FUOUTPUT
USE EXTRAS, only : sh_rh
implicit none
real pi,enx1,enx0,uu0,uu1,pp0,dd0,dd1,aa0,aa1
real 	rh(nv1x)
integer iprtsw,iprtlw,isl,i,ig,ib,ibseq,is,k
integer icc,icp
character*20 scm_lab(nscm)
data scm_lab /'CLEAR SKY','TOTAL SKY','Pristine Sky','TOTAL_NO_Aerosol'/

isl=fi%nv+1 !!! Local def of sfc level

SOLVER1: do is = 1,nscm ! SOLVER

if ( fi%lscm(is)  ) then 
print*,'Fu-Liou Model outputs in structure fo(',is,')% Begin ',scm_lab(is)

if( ftoa(is)%exist ) then
print*,'TOAONLY TOASWDN TOASWUP  TOAALB     OLR   WNOLR  TOTRad   WNrad FltWnRad UnfWnRad WnFlx '
print'(a7,i1,2f8.2,f8.3,7f8.2)','TOAONLY',is, &
ftoa(is)%swdn, ftoa(is)%swup, ftoa(is)%swalb,&
ftoa(is)%olr, ftoa(is)%wnolr, &
ftoa(is)%totrad, ftoa(is)%winrad,&
ftoa(is)%trwn_flt_r, ftoa(is)%trwn_unf_r, ftoa(is)%trwn_f

print*,'SFCONLY SFCSWDN SFCSWUP  SFCALB  DIRECT DIFFUSE     PAR    LWUP    LWDN    WNUP    WNDN'
print'(a7,i1,2f8.2,f8.3,7f8.2)','SFCONLY',is, &
fsfc(is)%swdn, fsfc(is)%swup, fsfc(is)%swalb, &
fsfc(is)%swdir ,fsfc(is)%swdif ,fsfc(is)%swpar, &
fsfc(is)%lwup, fsfc(is)%lwdn, &
fsfc(is)%wnup, fsfc(is)%wndn 

print'(a7,2x,a63,a60)','UV fouv','TOA_UVB TOA_UVA TOA_PAR SFC_UVB SFC_UVA SFC_PAR UVINDEX UVBDrDf',& 
                                 ' UVADrDf PARDrDf UVB_ERY PAR_PURV PurvDrDf PAR_CHLA ChlADrDf'
print'(a7,i2,15f8.3)','UV fouv',is,fouv(is)

print*, ' '
endif 

if( fo(is)%exist ) then
print*,'FPR________Pres____Temp____RH%____SWDN____SWUP__DIRECT_DIFFUSE_SWHEATR____LWDN___LWUP__LWHEATR_WNDN___WNUP.....'

FPR:do i=1,fi%nv+1
!call ql_rh( rh(i),fi%pt(i),fi%pp(i),fi%ph(i))
rh(i)= sh_rh(fi%pt(i),fi%pp(i),fi%ph(i) )
print '(a3,i1,i3,f8.2,f8.1,f8.1, 5f8.2,1x,5f7.2)',	&
'FPR',is,i,	&
fi%pp(i),fi%pt(i),rh(i),	&
fo(is)%fds(i) ,fo(is)%fus(i) ,fo(is)%fdsdr(i) ,fo(is)%fdsdf(i), &
fo(is)%dts(i),	&
fo(is)%fdir(i),fo(is)%fuir(i), &
fo(is)%dtir(i),	&
fo(is)%fdwn(i),fo(is)%fuwn(i)

enddo FPR
endif


iprtsw =1
iprtlw =1
!-----------------------------------------------------------------------
if(iprtsw==1 .and. fos(is)%exist ) then !SW Spectral
print*,' '
print*,'SSW  Band  SWDNSFC  SWTOAUP    SFCDIR   SFCDIF  TOADIR'
do ibseq=1,18
aa0=-1E36 ; aa1=-1E36
if (fos(is)%rswfd(isl  ,ibseq) >1E-4) &
  aa0= fos(is)%rswfu(isl,ibseq)/fos(is)%rswfd(isl,ibseq) 
if (fos(is)%rswfd(isl-1,ibseq) >1E-4) &
  aa1= fos(is)%rswfu(isl-1,ibseq)/ fos(is)%rswfd(isl-1,ibseq)
print'(a3,i1,I5,4f9.3,2f9.3)','SSW',is,ibseq,	&
fos(is)%rswfd(isl,ibseq), 		&
fos(is)%rswfu(1,ibseq),		&
fos(is)%rswdir(isl,ibseq),		&
fos(is)%rswdif(isl,ibseq), 		&
fos(is)%rswdir(1,ibseq)   
enddo



print'(a3,i1,a5,4f9.2,2f9.3)','SSW',is,'1-18', &
sum( fos(is)%rswfd(isl,1:18)),	&
sum( fos(is)%rswfu(1,1:18)), 	&
sum(fos(is)%rswdir(isl,1:18)),	&
sum(fos(is)%rswdif(isl,1:18)), 	&
sum(fos(is)%rswdir(1,1:18))
endif !iprtsw

!------------------------
if(iprtlw==1 .and. fos(is)%exist ) then       !LW Spectral

print*, ' '
print*,'SLW  Band  LWsfcDN  LWsfcUP  SFC-1Dn  ', &
'SFC-1up EMIS(sfc)(sfc-1)  B(skinT)(sfcT)(sfc-1T)'

pi=3.14159
!do ib=7,20
do ib=10,23
dd0=  fos(is)%rlwfd(isl,ib)
uu0=  fos(is)%rlwfu(isl,ib)
pp0=  pi*fos(is)%sbs(ib)

dd1= fos(is)%rlwfd(isl-1,ib)
uu1= fos(is)%rlwfu(isl-1,ib)

	
enx0= -9.999
if( abs (dd0-uu0 )> 0.01 .and. &
    abs (dd0-pp0 )> 0.01 ) enx0 = ( dd0-uu0 ) / ( dd0-pp0 )
enx1=-9.999
if( abs (dd1-uu1 )> 0.01 .and. &
    abs (dd1-pp0 )> 0.01 ) enx1 = ( dd1-uu1 ) / ( dd1-pp0 )
    
print'(a3,i1,I5,4f9.3,8f8.3)','SLW',is,ib, dd0,uu0,  dd1,uu1, enx0, enx1, &
pi*fos(is)%sbs(ib),	&
pi*fos(is)%sbf(isl,ib),	&
pi*fos(is)%sbf(isl-1,ib)
 enddo

print'(a3,i1,a5,4f9.2,2f7.3)','SLW',is,'10-23', &
sum( fos(is)%rlwfd(isl,10:23)),&
sum( fos(is)%rlwfu(isl,10:23)),&
sum( fos(is)%rlwfd(isl-1,10:23)),&
sum( fos(is)%rlwfu(isl-1,10:23))


!---------------------------------------------------------------
print*,' '
print*,'LWSPEC__band..10...11....12....13....14....15....16....17....18....19....20....21....22....23..10:23Flx.....Rad'
print'(a9,i1,14f6.2,2x,2f8.1)','LWSPECTOA',is,fos(is)%rlwfu( 1,10:23), &
sum(fos(is)%rlwfu(1,10:23)),fo(is)%fiurt(1)

print'(a9,i1,14f6.2,2x,2f8.1)','LWSPECSUP',is,fos(is)%rlwfu(isl,10:23),&
sum(fos(is)%rlwfu(isl,10:23)),fo(is)%fiurt(isl)
print'(a9,i1,14f6.2,2x,2f8.1)','LWSPECSDN',is,fos(is)%rlwfd(isl,10:23),&
sum(fos(is)%rlwfd(isl,10:23))

print*,' '
print*,'WINDOW..RadB11..RadB12..RadB13..FlxB11..FlxB12..FlxB13.Fltwnrad.Unfwnrad.WinFlx'
print'(a6,i1,9f8.2)','WINDOW',is, 		&
fo(is)%fu_sr(14:16), fo(is)%fu_sf(14:16),	&
fo(is)%trwn_flt_r, fo(is)%trwn_unf_r, fo(is)%trwn_f
!---------------------------------------------------------------
endif !iprtlw

print*,'Fu-Liou Model outputs in structure fo(',is,')% END ',scm_lab(is)
print*,('-|',k=1,60)
endif

enddo SOLVER1


!Begin-------------------------CLOUD CONDITION COMPONETS Fu solver only

CLEARORPRIS :do icp =1,2  ! 1= w/Aerosol 2=NO Aerosol
CLOUD_COND: do icc = 0,mccx
if ( focc(icc,icp)%exist ) then
if ( icc == 0  .or. fi%fc(icc)%cldfrac >0  ) then

print*,'FCCXX________Pres____Temp____RH%____SWDN____SWUP__DIRECT_DIFFUSE_SWHEATR____LWDN___LWUP__LWHEATR_WNDN___WNUP......'

FCC:do i=1,fi%nv+1
!call ql_rh( rh(i),fi%pt(i),fi%pp(i),fi%ph(i))
rh(i)= sh_rh(fi%pt(i),fi%pp(i),fi%ph(i) )
print '(a3,2i2.2,i3,f8.2,f8.1,f8.1, 5f8.2,1x,5f7.2)',	&
'FCC',icc,icp,i,	&
fi%pp(i),fi%pt(i),rh(i),	&
focc(icc,icp)%fds(i) ,focc(icc,icp)%fus(i) ,focc(icc,icp)%fdsdr(i) ,focc(icc,icp)%fdsdf(i), &
focc(icc,icp)%dts(i),	&
focc(icc,icp)%fdir(i),focc(icc,icp)%fuir(i), &
focc(icc,icp)%dtir(i),	&
focc(icc,icp)%fdwn(i),focc(icc,icp)%fuwn(i)

enddo FCC

endif
endif
enddo CLOUD_COND
enddo CLEARORPRIS
!END-------------------------CLOUD CONDITION COMPONETS Fu solver only

if ( fi%ierror .ne.0 )then
print*,'OUTPUT ERROR : fi%ierror = ',fi%ierror
stop ' from  subroutine print_out_fu '
endif
end subroutine print_out_fu
!==============================================================================
subroutine print_out_hr
USE FUINPUT
USE FUOUTPUT
implicit none
integer it,is,i,i1,i2,kk
real dp

it=1
is=fi%nv+1

100 format( I10.3,f10.2 ,8f10.3 )
write(6,100) 888, fi%pp(it) ,fo(1:4)%fds(it)-fo(1:4)%fus(it),fo(1:4)%fdir(it)-fo(1:4)%fuir(it)
write(6,100) 999, fi%pp(is) ,fo(1:4)%fds(is)-fo(1:4)%fus(is),fo(1:4)%fdir(is)-fo(1:4)%fuir(is)
do i = 1,FI%VD%nrep-1
i1=vo%ireport(i)
i2=vo%ireport(i+1)
dp= fi%pp(i2)-fi%pp(i1)
write(6,100) i,(FI%VD%report(i)+FI%VD%report(i+1))*0.5, &

( (( fo(kk)%fds (i1) -fo(kk)%fus (i1))- (fo(kk)%fds (i2) -fo(kk)%fus( i2)))* 8.4392/dp , kk=1,4) ,&
( (( fo(kk)%fdir(i1) -fo(kk)%fuir(i1))- (fo(kk)%fdir(i2) -fo(kk)%fuir(i2)))* 8.4392/dp , kk=1,4)


!fo(1:4)%dts(vo%ireport(i) ) ,&
!fo(1:4)%dtir(vo%ireport(i) )

enddo
end subroutine print_out_hr

!
