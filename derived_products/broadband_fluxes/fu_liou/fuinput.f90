MODULE FUINPUT
! LAtest Changes (200609)
! Mixed Phase Clouds allowed ( rphase , de ,re ) replace (iphase, part_size) 
! OPTION :Phase override of Ice Clouds Warmer than 273K
! OPTION :Correction for Curved Earth Aimass at low sun.
! Allow Ice De(5 to 300um ) this previously had a lower limit of 20um because of parameterization issues.
implicit none
! PARAMETERS
integer, parameter :: nvx = 60 ! 40 !60	!! MAX # of Model Levels
integer, parameter :: mccx=  4 !3	!! MAX # of Cloud Conditions
integer, parameter :: mxovl= 4 !2  !! MAX # of Cloud Overlap cases!!!! FU20040901

!integer, parameter :: nvx =160 ! 40 !60	!! MAX # of Model Levels
!integer, parameter :: mccx= 16 !3	!! MAX # of Cloud Conditions
!integer, parameter :: mxovl=6 !2  !! MAX # of Cloud Overlap cases!!!! FU20040901

!integer, parameter :: nvx = 140	!! MAX # of Model Levels CALIPSO
!integer, parameter :: mccx= 4	!! MAX # of Cloud Conditions
!integer, parameter :: mxovl=10  !! MAX # of Cloud Overlap cases!!!! FU20040901
integer, parameter :: mxat= 7  	!! Max # Aerosol Wavelengths to input
integer, parameter :: mxac= 7  	!! Max # Aerosol Constituents to input 

integer, parameter :: nv1x  = nvx + 1 
integer, parameter :: ndfsx = nvx, mdfsx = nvx + 1 
integer, parameter :: ndfs4x= 4*ndfsx, mbx = 21, mbsx = 9 !mbx = 18, mbsx = 6 
integer, parameter :: mbirx = 12 , ncx=8, mby = 10 

!integer, parameter :: naer=25	!! # of aerosol property types
integer, parameter :: naer=29	!! # of aerosol property types

integer, parameter :: nrh=8 	!! # of aerosol Rel Humidity

integer, parameter :: iaform =3 !! Multi Constituent , Multi wavelength 
integer, parameter :: ivd=1 	!! USER PROVIDED AEROSOL VERTICAL PROFILE
integer, parameter :: ifg=0 	!! Aerosol property dependence on layer RH
integer, parameter :: nscm=4	!! # of solver modes(Clr, TOTAL,Pris,TOTALNoaot,Kato w/AOT ,KATO w/o AOT )

integer, parameter :: idkfr=2	!! Window K's scheme Kratz
integer, parameter :: iwtas=4	!! Continuum averaging scheme
real,parameter :: omit = -9999.0!! OMIT FLAG
!VLA parameters
integer, parameter ::  mfix  =  60 !!!! !nvx ! Max # of FIXED/manditory Model levels
integer, parameter ::  mflo  =   10 ! Max # of Floating Model levels
integer, parameter ::  maxin =  mfix !nvx ! Max # of Input Profile levels
integer, parameter ::  maxrep=  mfix !nvx ! Max # of Output Levels to Report.
integer, parameter ::  mri = 1100

!----------------------------------------------------------

integer, save :: nv1,mb,mbs,mbir,nc,ndfs,mdfs,ndfs4,ndfs2 
logical ,save :: edding, quadra, hemisp, mquadr
logical ,save :: fourssl, foursir
logical, save ::  lscm(nscm)
logical, save ::  lband6a

integer, save :: irobckd  ! H20 continuum index suggest set to 5 :ckd2.4
integer, save :: nhb  	! # if hidden bands (0-2) >2200cm-1
integer, save ::   isksolve , instrument

real ,save    ::   cfc_conc(3),umco2,umch4,umn2o 

!----
integer	 nv	! # of MODEL LAYERS
real ee(mbirx),ss ,pts,u0,ur
real dz(nvx) ! thickness of model layer in km

real ,dimension(nv1x):: pp,pt,ph,po
real ,dimension(nvx) :: pre,plwc,pde,piwc ,pdge ,pasp

real, dimension(nvx,6,mccx):: optwat,optice

integer  	n_atau ,nac,itps(mxac)	
real 		a_wli(mxat)
real 		a_taus(mxat,mxac)
real	 	aprofs(nvx,mxac)

real ,dimension(nvx)::cf_sk,ccf_sk,nu_sk
real ,dimension(mccx,mxovl)::shapefactor
real,save :: taucorrection(mccx,mxovl)

!------------------------------------------------------------------------

! Direct Cloud property Insert
TYPE DPI_TYPE 
 sequence
logical ldpi
integer phase(nvx)
real plwc_iwc(nvx)
real   pre_de(nvx)
END TYPE DPI_TYPE 
!-------------------------------------------------------------------------


TYPE FUSUBCLD
 sequence
 real shapef
 real mn_lin_tau
END TYPE FUSUBCLD

TYPE FUCLDCND
sequence
real    cldfrac
integer novl    	!!!! FU20040901
!!integer iphase(mxovl)  !use rphase as of 200609
integer icld_top(mxovl)
integer icld_bot(mxovl)
real    tau_vis(mxovl)
!real    part_size(mxovl) !use re&de as of 200609


real rphase(mxovl) !! NON-Integer Phase
real re(mxovl)	!! NON-Integer Phase
real de(mxovl)	!! NON-Integer Phase

real asp(mxovl) !! Fu2006 Ice Aspect ratio
TYPE (FUSUBCLD) sc(mxovl)

TYPE (DPI_TYPE) dpi
END TYPE FUCLDCND

!===============================================================================
!!Variable  Level Algorithm (VLA)
!-------------------------------------------------------------------------------------

TYPE VLADEFINE
 sequence
 integer nfix		! # of FIXED levels Input
 real pfix(mfix)	! Array of Pressure(hPa) of fixed levels
 integer nflo		! # of FLOATING levels Input
 real pflo(mflo)	! Array of Pressure(hPa) of floating levels
 real cldpres(2,mccx,mxovl) ! Array of Pressure(hPa) Top(1) , Bot(2) for each cloud condition an overlap condition 
 integer nrep		! # of levels to find reporting level index upon output
 real report(maxrep)	! Array of Pressure levels to find reporting level indexes for
END TYPE VLADEFINE

TYPE (VLADEFINE) VD

TYPE VLAIN
 sequence
 integer nlev      ! # of Input levels in input profile
 real	pp(maxin)  !pressure profile (mb)
 real	pt(maxin)  !temperature profile (K)
 real	ph(maxin)  !H2o Mixing ratio profile (g/g)
 real	po(maxin)  !O3 Mixing ratio profile (g/g)
 real   hsfc       ! SURFACE GEOPOTENTIAL of Sounding.
END TYPE VLAIN

TYPE (VLAIN)  VI

TYPE VLAOUT
 sequence
 integer nlev	   ! # of Input levels in output profile
  real	pp(nv1x)  !pressure profile (mb)
 real	pt(nv1x)  !temperature profile (K)
 real	ph(nv1x)  !H2o Mixing ratio profile (g/g)
 real	po(nv1x)  !O3 Mixing ratio profile (g/g)
 integer cldindex(2,mccx,mxovl) ! Top(1)Bot(2) Cloud Layer Indexes for FuModel
 integer ireport(maxrep)	! indexes of reporting pressure levels.
 integer ri_pp(mri)		! reverse index of pressure Layer to nearest 1hpa
 integer ierr	!ERROR FLAG...  <0 warning   ,, >0 ERROR!!
END TYPE VLAOUT

TYPE (VLAOUT) VO
!===============================================================================




!------------------------------------------------------------------------
TYPE FUINPUTTYPE
sequence
integer ioptmethod      ! ( ioptmethod = 1 Dont USe Minnis tau_wp and assume Ice Partice size alread in Dge 
logical swonlycomp
logical curvedearth     ! TRUE use airmass to revise u0 at low sun 
logical nirold(5)	!Rayleigh,WaterCloud,IceCloud,Gases,KdepWatercldOptics
integer	 nv		!# of model LAYERS
logical lscm(nscm)	!Select solver configurations modes
real	u0		!Cosine Solar Zenith angle ( 0-1)
real	ur 		!Cosine View Zenith angle ( 0-1) for LW Radiance
logical lband6a		! Treat Solar > 4um 
integer irobckd		! Continuum Treatment 
integer nhb  		! # of hidden bands (0-2) >2200cm-1
real	sfcalb(18 ,2,0:mccx)	! New Surface albedo array(BAND,w w/oAOT, Clr:CldCnds)
real 	ee(mbirx)	!Surface Spectral Emissivity Longwave
real	ss		!Solar Constant (wm-2)
real	pts		!Skin Temperature (k)
real	pp(nv1x)	!pressure profile (mb)
real	pt(nv1x)	!temperature profile (K)
real	ph(nv1x)	!H2o Mixing ratio profile (g/g)
real	po(nv1x)	!O3 Mixing ratio profile (g/g)
real	umco2 		! Co2 concentration ( LW only)
real	umch4		! CH4 concentration ( LW only)
real	umn2o 		! N2O concentration ( LW only)
real	cfc_conc(3)	! CFC concentrations (LW only)
integer n_atau 		!# of Aerosol wavelength inputs 
integer nac		!# of Aerosol constituents
integer itps(mxac)	!# Aerosol types 
real 	a_wli(mxat)	!Wavelength of AOT input
real	a_taus(mxat,mxac) 	!AOT at Wavelength 
real	aprofs(nvx,mxac)	!fractional profiles of AOT for each constituent
logical  HYBRID_SW_SOLVER     !!= .true.  !200130802 SYNI Ed4 $S Clear , 2S HOMO CLD , GWTSA INHOM Cld
integer isksolve	! Solver option 0=Fu 1=Kato
integer instrument	! Ceres Window emmulation
logical fourssl		! Fu four-stream Solar
logical foursir		! Fu four-stream IR
integer wp_hgt_flag     ! 0 = CONSTANT MIX , 1=INCREASE Linear w/height 2=Decrease Linear w/height 3=SH-Ham ~Decrease Linear w/height
 real   hsfc   		! OPTIONAL: Surface Geopotential Height (meters ) 
 real zz(nv1x)		! NOT AN INPUT COMPUTED BY thicks Geopotential height (meters)
 integer txt             !! Fu2006 Ice Texture(1=smooth) (2==rough) , 0=fu1996
 TYPE(FUCLDCND) 	fc(mccx)

 TYPE (VLADEFINE) VD
 TYPE (VLAIN)  VI
 TYPE (VLAOUT) VO
 
 TYPE (DPI_TYPE) DPI

integer ierror

END TYPE FUINPUTTYPE
	

TYPE(FUINPUTTYPE), save::	fi

CONTAINS 
!======================================================================================
subroutine set_default_options_fu
implicit none

fi%ioptmethod = 1 ! ( ioptmethod = 1 Dont USe Minnis tau_wp and assume Ice Partice size alread in Dge 

fi%swonlycomp   =.false.
!200609 
fi%curvedearth  = .true.
!Trace Gas Concentrations

fi%umco2	     = 380. ! 380 ppmv CO2 Concetration (Affects LW only , SW Fixed)
fi%umch4	     = 1.80 ! 1.80 ppmv CH4 Concetration (Affects LW only , SW Fixed)
fi%umn2o	     = 0.26 ! 0.26 ppmv N2O Concetration (Affects LW only , SW Fixed)
fi%cfc_conc(1:3)     = (/0.268e-09 , 0.503e-09 ,0.105e-09/)

!Options 
fi%nirold	   = .false.

fi%lband6a	   = .true.	!T=Treat >4.0 micron Solar
fi%irobckd	   = 5		!5=CKD2.4 Continuum
fi%nhb		   = 2		!2=Treat 2bands >2200cm-1
fi%isksolve	   = 1		!0=Original Fu Solver 1=Kato Solver

!Full Suite of Calculations
fi%lscm	   = .true. ! USE all  SOLVER MODES ( CLEAR,TOTAL ,Pristine,NoaotCld )

fi%wp_hgt_flag = 3  ! 0 = CONSTANT MIX , 1=INCREASE Linear w/height 2=Decrease Linear w/height 3=SH-Ham ~Decrease Linear w/height

fi%fc%dpi%ldpi =  .false. ! false do not directly insert properties from FI%DPI

fi%fourssl = .false.  ! False =Two Stream Solar ,,True = FOUR stream
fi%foursir = .false.  ! False = Two/four IR     ,,True = FOUR stream
fi%ierror = 0 ! initalize to no error condition
fi%vo%ierr= 0 ! initalize to no error condition vla algorithm

fi%txt = 0 ! FU96 IceOptics ( 0=fu96  1=SmoothFu2006  2=Rough Fu 2006)
end subroutine set_default_options_fu
!=================================================================
subroutine assigninputs
implicit none
real airmass,u0ce

nv	= 	fi%nv
lscm	=	fi%lscm
u0	=	fi%u0
ur	=	fi%ur
lband6a =	fi%lband6a
irobckd = 	fi%irobckd
nhb  	= 	fi%nhb
! surface albedo directly input
ee	=	fi%ee
ss	=	fi%ss
pts	=	fi%pts
pp	=	fi%pp
pt	=	fi%pt
ph	=	fi%ph
po	=	fi%po
umco2	=	fi%umco2
umch4	=	fi%umch4
umn2o	=	fi%umn2o
cfc_conc=	fi%cfc_conc
n_atau	=	fi%n_atau
nac	=	fi%nac
itps	=	fi%itps
a_wli	=	fi%a_wli
a_taus	=	fi%a_taus
aprofs	=	fi%aprofs
isksolve=	fi%isksolve
instrument= 	fi%instrument
fourssl	=	fi%fourssl
foursir	=	fi%foursir

!Secondary...................
nv1	=	nv+1
ndfs 	= 	nv
mdfs 	= 	nv1
ndfs4 	= 	4*ndfs 
ndfs2	=  	2*ndfs
mb   	= mbx    ! 18
mbs  	= mbsx   !6
mbir 	= mbirx  !12 
nc   	= ncx    !8 


if (fi%curvedearth) then !! 200609 Correct for curved earth airmass at low sun

  u0ce     =  1.0/airmass(fi%u0 ,6378. , 8. )    !!! AIRMASS Corrected u0
  ss	   =  fi%ss *     fi%u0/u0ce
  u0       =  u0ce
endif

end subroutine assigninputs

!=================================================================
subroutine check_inputs_fu
implicit none
integer i,j

fi%ierror = 0 !!! SET ERROR FLAG TO ZERO means OK..

! CHECK BASICS
if ( fi%nv > nvx .or. fi%nv < 1 )	  fi%ierror=100 ! #of Model Levels out of range
if ( fi%ierror .ne. 0 ) return
if ( abs(fi%u0) > 1.0 )			  fi%ierror=101 ! Cos Sol out of range
if ( abs(fi%ur) > 1.0 )			  fi%ierror=102 ! Cos View Zen of range
if ( fi%ss < 1000 .or. fi%ss > 1500)	  fi%ierror=103 ! Solar Const out of range


!CHECK ATMOSPHERE....
do i=1,fi%nv+1
if( fi%pp(i)< 1E-5  .or. fi%pp(i)>1100 )  fi%ierror=104 !Pressure(hPa) out of range
if (fi%pt(i)< 100.  .or. fi%pt(i)> 400.)  fi%ierror=105 !Temperature (K) out of range
if (fi%ph(i)<1.E-10 .or. fi%ph(i)> 0.5 )  fi%ierror=106 !H20  Mix Ratio(g/g) out of range
if (fi%po(i)<1.E-10 .or. fi%po(i)>1.E-2)  fi%ierror=107 !O3 Mix Ratio(g/g) out of range
enddo
do i=1,fi%nv
if (fi%pp(i)>fi%pp(i+1) )		  fi%ierror=108 !Pressure Not Monotonic increasing
enddo

if( fi%pts < 100. .or.  fi%pts>500.)	  fi%ierror=109 !Skin Temp out of range


!CHECK AEROSOLS.....
if(fi%nac < 1 .or. fi%nac > mxac )then
	  fi%ierror=110 !#of Aerosol Constituents out of range
print*,fi%nac
endif
if ( fi%ierror .ne. 0 ) return
if(fi%n_atau < 1 .or. fi%n_atau > mxat )  fi%ierror=111 !#of Aerosol Optical Depths out of range
if ( fi%ierror .ne. 0 ) return

do j = 1,fi%nac
if (fi%itps(j) < 1 .or. fi%itps(j) > naer ) fi%ierror=112 !Aerosol Type out of range
do i = 1,fi%n_atau 
if ( fi%a_wli(i) < 0.2 .or. fi%a_wli(i)> 3.0 )   fi%ierror=113 !Aerosol wavelength (um) out of range
if ( fi%a_taus(i,j) < 1E-10 .or. fi%a_taus(i,j)> 10.0 ) fi%ierror=114 !Aerosol Opt depth out of range
enddo; enddo

!!CHECK OPTION SETTINGS
if ( fi%irobckd < 0 .or. fi%irobckd >5 ) fi%ierror=115 ! Continuum option out of range
if ( fi%nhb < 0 .or. fi%nhb >2 ) fi%ierror=116 ! #of hidden thermal bands >2200cm-1 option out of range
if (fi%isksolve<0.or.fi%isksolve>1)  fi%ierror=118 ! Solver Mode 0=Fu 1=Seiji Kato overlap &cld tau shape factor



CLOUDCONDITION :do i =1,mccx
if ( fi%fc(i)%cldfrac < 0 .or. fi%fc(i)%cldfrac > 1.0001 ) fi%ierror=119 ! cloud fraction out of range

if ( fi%fc(i)%cldfrac > 0 .and. fi%fc(i)%cldfrac <= 1.0001 .and. .not. fi%fc(i)%dpi%ldpi) then
if ( fi%fc(i)%novl > mxovl .or. fi%fc(i)%novl < 1 ) fi%ierror=135 ! improper value of # of overlap conditions



if ( fi%fc(i)%cldfrac == 0.0 ) cycle
OVERLAP : do j=1,fi%fc(i)%novl
!if ( fi%fc(i)%iphase(j) < 1 .or. fi%fc(i)%iphase(j) > 2 )then !!! ASSUME MIXED PHASE INPUTS
 if ( fi%fc(i)%rphase(j)<1 .or. fi%fc(i)%rphase(j)>2 ) fi%ierror=120 ! cloud phase out of range
!endif


if ( fi%fc(i)%tau_vis(j) < 0 .or. fi%fc(i)%tau_vis(j) > 1000 ) fi%ierror=121 ! cloud tau_vis out of range
!if ( fi%fc(i)%iphase(j) == 1 .and. (fi%fc(i)%part_size(j) < 4 .or. fi%fc(i)%part_size(j) > 30) ) fi%ierror=122 ! WATER cloud part size out of range
!if ( fi%fc(i)%iphase(j) == 2 .and. (fi%fc(i)%part_size(j) < 20 .or. fi%fc(i)%part_size(j) > 300) ) fi%ierror=123 ! ICE cloud part size out of range

if ( fi%fc(i)%rphase(j) >=1 .and.  fi%fc(i)%rphase(j) <= 2) then 
if ( fi%fc(i)%rphase(j) < 2  .and. (fi%fc(i)%re(j) < 4 .or. fi%fc(i)%re(j) > 30) ) fi%ierror=122 ! WATER cloud part size out of range
if ( fi%fc(i)%rphase(j) > 1  .and. (fi%fc(i)%de(j) < 4 .or. fi%fc(i)%de(j) > 250) ) fi%ierror=123 ! ICE cloud part size out of range

!if ( fi%fc(i)%rphase(j) > 1.0 .and.  fi%fc(i)%rphase(j) < 2.0 )  fi%ierror= 139 ! MIXED PHASE CLOUDS NOT ALLOWED 10-12-2010 had bad logic for Overlapped clouds.

endif

if ( fi%fc(i)%icld_top(j) < 1 .or. fi%fc(i)%icld_bot(j) > fi%nv ) fi%ierror=124 ! Cloud layer placement error

if ( fi%fc(i)%icld_top(j) > fi%fc(i)%icld_bot(j) ) fi%ierror=127 ! Cloud layer assignment order error


if ( fi%fc(i)%rphase(j) > 1 .and. (fi%txt==1 .or. fi%txt == 2 ).and. (fi%fc(i)%asp(j)<0.1 .or. fi%fc(i)%asp(j)>10.) ) fi%ierror=138 ! Fu2006 ASPECT RATIO ERROR
enddo OVERLAP

else
if ( fi%fc(i)%cldfrac > 0 .and. fi%fc(i)%cldfrac <= 1.0001 .and. &
     fi%fc(i)%novl /= 0 .and. fi%fc(i)%dpi%ldpi  ) fi%ierror=136 ! improper value of # of overlap conditions


endif
enddo CLOUDCONDITION

if ( sum(fi%fc(1:mccx)%cldfrac) > 1.0001)  fi%ierror=125 ! Total Cloud Fraction error 

if ( fi%isksolve ==1 ) then
  do i =1,mccx
  if ( fi%fc(i)%cldfrac > 0 .and. .not. fi%fc(i)%dpi%ldpi ) then
	do j=1,fi%fc(i)%novl
       if ( fi%fc(i)%sc(j)%shapef <= 0 ) then
       if ( fi%fc(i)%sc(j)%mn_lin_tau<= 0  ) fi%ierror=126 ! SUBSCALE Cloud Inputs ZERO
       endif
	enddo
  endif
  enddo
endif



if( fi%vo%ierr > 0 ) fi%ierror=136 !! fi%vo%ierr > 0 in VLA algorithm

if ( fi%txt <0 .or. fi%txt > 2)  fi%ierror= 137 ! Ice Cloud Txture incorrect setting


if ( fi%HYBRID_SW_SOLVER ) fi%isksolve =1  ! Insure set up of GWTSA when needed by Hybrid



end subroutine check_inputs_fu
!================================================================================
!=============================================================================================================
subroutine gwtsa_correction(icldcnd)
! "taucorrection" is the d(lntau) needed to correct the cloud optical depth
! for GWTSA algorithm based on biases computed using independent realizations
! of a gamma distribution of tau using the homogeneous solver for various
! Mean Tau, Nu, # of Cloud layers , Cos Sol 
! Corrections are largest for many layer clouds at low NU for mean tau in range 20:80.

USE FU_NU_TAU_NCL, only: taucorrect
implicit none
integer  icldcnd,ncl,ibx
real tau_vis_use,sfcalb_aprox
integer j 
real w_aprox(15) 
data w_aprox /0.000 , 0.000  ,0.000  ,0.000  ,0.002 ,0.027 , 0.099 , 0.114 , 0.169  ,0.145 &
            ,0.377 ,0.055 , 0.012 ,0.000 , 0.000 /


OVERLAP: do  j = 1,fi%fc(icldcnd)%novl
!----
sfcalb_aprox = sum( w_aprox(1:15)* fi%sfcalb ( 1:15 , 1 , icldcnd) ) 

tau_vis_use =  fi%fc(icldcnd)%tau_vis(j)
if ( fi%fc(icldcnd)%sc(j)%mn_lin_tau >0 ) tau_vis_use = fi%fc(icldcnd)%sc(j)%mn_lin_tau

ncl =  1+( fi%fc(icldcnd)%icld_bot(j) - fi%fc(icldcnd)%icld_top(j) )

taucorrection(icldcnd,j) = 0.0
if ( shapefactor(icldcnd,j) > 0 .and. shapefactor(icldcnd,j) < 100. ) &
taucorrection(icldcnd,j) = taucorrect(tau_vis_use,ncl,shapefactor(icldcnd,j),u0,sfcalb_aprox )

! print'(a8,2i5,6f9.3)','TAUCORR',icldcnd,j,taucorrection(icldcnd,j), &
!  tau_vis_use,real(ncl),shapefactor(icldcnd,j),fi%u0,sfcalb_aprox
enddo OVERLAP
 
end subroutine gwtsa_correction
!=============================================================================================================
subroutine cloud_input_dpi(icldcnd,ib)
!INITALIZE TO ZERO....
integer icldcnd,ib
 plwc= 0.0  ; pre= 0.0  ;piwc= 0.0 ;pde= 0.0;pdge= 0.0 !! LAYERS

where ( fi%fc(icldcnd)%dpi%phase(1:fi%nv) == 1 ) !! WATER PHASE
  plwc(1:fi%nv) = fi%fc(icldcnd)%dpi%plwc_iwc(1:fi%nv )
   pre(1:fi%nv) = fi%fc(icldcnd)%dpi%pre_de (1:fi%nv )
 endwhere

where ( fi%fc(icldcnd)%dpi%phase(1:fi%nv) == 2 ) !!  !ICE PHASE
  piwc(1:fi%nv) = fi%fc(icldcnd)%dpi%plwc_iwc(1:fi%nv)
   pde(1:fi%nv) = fi%fc(icldcnd)%dpi%pre_de(1:fi%nv)
!  pdge(1:fi%nv) =  -2.4+ 0.7566*fi%fc(icldcnd)%dpi%pre_de(1:fi%nv) + 9.22E-04* fi%fc(icldcnd)%dpi%pre_de(1:fi%nv)  ** 2
  pdge ( 1:fi%nv ) = &
    2.504655 /(  -1.819921e-04  + 1.0/(   &
    2.453e-1  *  fi%fc(icldcnd)%de(1:fi%nv)     &
   + 1.2196e-3*  fi%fc(icldcnd)%de(1:fi%nv)**2  &
   - 3.4745e-6*  fi%fc(icldcnd)%de(1:fi%nv)**3 )) !! Consistent w/ Minnis Tau_wp extinctions
 endwhere 


!!!
if (fi%ioptmethod == 1 ) then  
 where ( fi%fc(icldcnd)%dpi%phase(1:fi%nv) == 2 ) &!!  !ICE PHASE
 pdge ( 1:fi%nv ) = fi%fc(icldcnd)%de(1:fi%nv)
endif


end subroutine cloud_input_dpi
!=============================================================================================================

subroutine cloud_input(icldcnd,ib)

USE EXTRAS,only :tau_wp

implicit none
integer, parameter:: idirection=1
integer icldcnd,ib,j
integer k,kb,ke

real clc,dzcld,clwp
real lwp1,iwp1
real tau_vis_use
real clc_check
real phasew_wat,phasew_ice
integer imode,wmode
!INITALIZE TO ZERO....
 plwc= 0.0  ; pre= 0.0  ;piwc= 0.0 ;pde= 0.0;pdge= 0.0 !! LAYERS


OVERLAP : do j = 1,fi%fc(icldcnd)%novl  !! OVERLAP CASES 

kb= fi%fc(icldcnd)%icld_top(j)
ke= fi%fc(icldcnd)%icld_bot(j)
if ( ke < kb ) cycle
dzcld = sum( dz(kb:ke) ) *1000.


if ( ib <= 6 .and. fi%isksolve==1 .and. fi%fc(icldcnd)%sc(j)%mn_lin_tau >0 ) then
 tau_vis_use = exp( log( fi%fc(icldcnd)%sc(j)%mn_lin_tau ) + taucorrection(icldcnd,j) )  
else
 tau_vis_use = fi%fc(icldcnd)%tau_vis(j)
endif

!-----MIXED PHASE Clouds--------------------------------------------------------------------------
if (  fi%fc(icldcnd)%rphase(j) >= 1 .and. &
      fi%fc(icldcnd)%rphase(j) <= 2 ) then 


  phasew_wat=  2.0- fi%fc(icldcnd)%rphase(j) 
  phasew_ice=  fi%fc(icldcnd)%rphase(j) -1.0 

  lwp1  = tau_wp(idirection,  &
               tau_vis_use,	  &
	       fi%fc(icldcnd)%re(j),1) * phasew_wat

if ( fi%ioptmethod == 1 ) then 
   iwp1 = tau_vis_use/(1.819921e-04 + 2.504655e+00/ fi%fc(icldcnd)%de(j))* phasew_ice   ! assumed de is as Dge
else !old method
  iwp1  = tau_wp(idirection,  &
               tau_vis_use,	  &
	       fi%fc(icldcnd)%de(j),2)  * phasew_ice
endif

!OVERRIDE  iwp1  = tau_wp(idirection,  tau_vis_use, fi%fc(icldcnd)%de(j),2)  * phasew_ice


 if ( lwp1 > 0  .or. iwp1 > 0 .and. dzcld > 0) then 
 
! print*,'dzcld=',dzcld
!  print*,'iwp1=',iwp1
  do k = kb,ke
  
   if ( lwp1 >  0 ) then
    pre  ( k ) = fi%fc(icldcnd)%re(j)
   endif
   if( iwp1 > 0 ) then 
    pde  ( k ) = fi%fc(icldcnd)%de(j)
    pasp ( k ) = fi%fc(icldcnd)%asp(j)
!   pdge ( k ) =    -2.4+ 0.7566*fi%fc(icldcnd)%de(j) + 9.22E-04*fi%fc(icldcnd)%de(j) ** 2 !! Fu Table II

!!    pdge ( k ) =     2.504655 /(  -1.819921e-04  + 1.0/(  2.453e-1 *  &
!!    fi%fc(icldcnd)%de(j) + 1.2196e-3*  fi%fc(icldcnd)%de(j)**2  &
!!     - 3.4745e-6*  fi%fc(icldcnd)%de(j)**3 )) !! Consistent w/ Minnis Tau_wp extinctions
     
     
if (fi%ioptmethod == 1 ) then  
 pdge ( k ) = fi%fc(icldcnd)%de(j)
else
  pdge ( k ) = &
    2.504655 /(  -1.819921e-04  + 1.0/(   &
    2.453e-1  *  fi%fc(icldcnd)%de(j)     &
   + 1.2196e-3*  fi%fc(icldcnd)%de(j)**2  &
   - 3.4745e-6*  fi%fc(icldcnd)%de(j)**3 )) !! Consistent w/ Minnis Tau_wp extinctions
endif
     
     
  endif
  
  
  if(fi%wp_hgt_flag == 0 ) then 
   if ( fi%fc(icldcnd)%rphase(j) < 2.0 .and. lwp1 > 0) plwc ( k ) = lwp1/dzcld
   if ( fi%fc(icldcnd)%rphase(j) > 1.0 .and. iwp1 > 0) piwc ( k ) = iwp1/dzcld   
   
  elseif(fi%wp_hgt_flag == 1  ) then 
 
  imode = 1  
   if ( fi%fc(icldcnd)%rphase(j) < 2.0 .and. lwp1 > 0) plwc ( k )  = lwp1/dzcld
   if ( fi%fc(icldcnd)%rphase(j) > 1.0 .and. iwp1 > 0) piwc ( k )  = linear_clc(kb,ke,k,imode,iwp1)
  elseif(fi%wp_hgt_flag == 2  ) then 
   imode=2
    if ( fi%fc(icldcnd)%rphase(j) < 2.0 .and. lwp1 > 0) plwc ( k )  = lwp1/dzcld
    if ( fi%fc(icldcnd)%rphase(j) > 1.0 .and. iwp1 > 0) piwc ( k )  = linear_clc(kb,ke,k,imode,iwp1)
  elseif(fi%wp_hgt_flag == 3  ) then 
   imode=3
    if ( fi%fc(icldcnd)%rphase(j) < 2.0 .and. lwp1 > 0) plwc ( k )  = lwp1/dzcld 
    if ( fi%fc(icldcnd)%rphase(j) > 1.0 .and. iwp1 > 0) piwc ( k )  = linear_clc(kb,ke,k,imode,iwp1)    
  endif

!   print*,'MIXPHA' ,fi%fc(icldcnd)%rphase(j),k, plwc ( k ),pre  ( k ),piwc ( k ) ,pde  ( k ), pdge ( k ) 
  enddo  
 endif !!llwp1 iwp1>0
endif 


enddo OVERLAP


end    subroutine cloud_input
!===========================================================
real function linear_clc(kb,ke,k1,mode,wp1)
implicit none
real wp1
integer kb,ke,k,k1,mode,kk
real wpx(nvx),ss0(nvx),zmid(nvx)
integer ibx,iex,idx
real ztot,dkm

if ( mode == 1 ) then
 ibx=ke
 iex=kb
 idx=-1
elseif( mode >= 2 ) then 
 ibx=kb
 iex=ke
 idx=1
endif

ztot=sum( dz(kb:ke) )

 kk=0
 zmid=0
 do k = ibx,iex,idx
  kk=kk+1 
  if ( kk == 1 ) zmid(kk) = dz(k)*0.5
  if ( kk >  1 ) zmid(kk)=zmid(kk-1)+ dz(k)*0.5 +dz(k-(1*idx))*0.5
 enddo

if ( idx == -1 ) zmid(1:kk) = zmid(kk:1:-1)

zmid(1:kk) =   zmid(1:kk) / ztot

!print*,'Zmid',zmid(1:kk)

!!! ss0(1:kk) ! becomes the linearly increasing with depth, fractional water path per layer 

if ( mode <= 2 ) ss0(1:kk) =  float(kk)*zmid(1:kk) /  sum(zmid(1:kk))

if ( mode == 3 ) then 
 DKM=ztot
 if ( DKM > 11.5 ) DKM= 11.5
 if ( DKM < 0.0 )  DKM= 0.0
  ss0(1:kk) =  ( 1.0- ABS( zmid(1:kk) - DKM *0.05-0.425) ) 
  ss0(1:kk) =    float(kk)* ss0(1:kk) / sum( ss0(1:kk) ) 
endif

wpx(kb:ke) = wp1* (ss0(1:kk) ) / (ztot*1000.)

!print*,'lwc1dum' , wp1/(ztot*1000.)

!print*,'ztot',ztot
!print*,'dz',dz(kb:ke)
!print*, 'ss0',DKM,':',ss0(1:kk)
!print*, 'ss0*kk',ss0(1:kk)*kk
!print*, 'nnn',':', (zbar(1:kk) /sum(zbar(1:kk))) / (dz(kb:ke)*1000.)

!print*,"ss0:",  sum (ss0(1:kk)),':', ss0(1:kk)
!print*,'wpx0' , wp1/(ztot*1000.)
!print*,'wpx' , wpx(kb:ke) 
!print*,'RATIO wpx' , wpx(kb:ke) / (wp1/(ztot*1000.))

!print*,'wp1',wp1
!print*,'sumwpx' , sum(wpx(kb:ke)*(dz(kb:ke))*1000.)
!print*,'dumchk' , sum((wp1/ztot) *(dz(kb:ke)))

!print*,'linear_clc',k1,linear_clc

wpx(kb:ke) = wpx(kb:ke)  * wp1/ ( sum(wpx(kb:ke)*(dz(kb:ke))*1000.) ) !! Normalize to insure consistent IWP

linear_clc = wpx(k1)

!if ( ABS(sum(wpx(kb:ke)*(dz(kb:ke))*1000.) - wp1 ) > 0.1 ) print*,'IWCP consistency error1',wp1,sum(wpx(kb:ke)*(dz(kb:ke))*1000.)

!if ( ABS(sum(wpx(kb:ke)*(dz(kb:ke))*1000.) -  sum((wp1/ztot) *(dz(kb:ke))) ) > 0.1 ) then
! print*,'IWCP cons error2',sum(wpx(kb:ke)*(dz(kb:ke))*1000.),sum((wp1/ztot)*(dz(kb:ke))),&
! ABS(sum(wpx(kb:ke)*(dz(kb:ke))*1000.) -  sum((wp1/ztot)*(dz(kb:ke))) )
!endif

!print*, ABS(sum(wpx(kb:ke)*(dz(kb:ke))*1000.) -  sum((wp1/ztot) *(dz(kb:ke))) )

end function linear_clc



END MODULE FUINPUT

