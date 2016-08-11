MODULE FUOUTPUT
USE FUINPUT
implicit none

TYPE FUOUTPUTTYPE
sequence	
real fds(nv1x)		! SW Down 	flux ( W / m **2 )
real fus(nv1x)		! SW Up 	flux ( W / m **2 )
real fdir(nv1x)		! LW Down 	flux ( W / m **2 )
real fuir(nv1x)		! LW Up 	flux ( W / m **2 )
real fdsdr(nv1x)	! SW Direct 	flux ( W / m **2 )
real fdsdf(nv1x)	! SW Diffuse 	flux ( W / m **2 )
real fdwn(nv1x)		! WINDOW Down 	flux ( W / m **2 )
real fuwn(nv1x)	  	! WINDOW Up  	flux ( W / m **2 )

real fiurt(nv1x)	!LW Up Radiance Profile ( W  m-2 sr-1)
real fiurw(nv1x)	!WN Up Radiance	Profile( W  m-2 sr-1)

real fd(nv1x)		! SW+LW down
real fu(nv1x)		! SW+LW up
real dts(nvx)		! SW heating rate
real dtir(nvx)		! LW heating rate
real dt(nvx)		! SW+LW heating rate

real fu_sr(14:16)	!TOA MODEL WINDOW RADIANCEs
real fu_sf(14:16)	!TOA MODEL WINDOW FLUXs
real trwn_flt_r		!emmulated Flt WN Radiance @TOA ( W  m-2 sr-1)
real trwn_unf_r		!emmulated Unflt WN Radiance @TOA ( W  m-2 sr-1)
real trwn_f		!emmulated WN Flux @TOA ( W  m-2 )
logical exist		! Flag to test for presence
!
!real lwespa(nv1x) !
!real lweatm(nv1x) !!
!real swespa(nv1x) !!
!real sweatm(nv1x) !!
real LW_entropy(7) !!! additional entropy outputs 
real SW_entropy(5) !!! additional entropy outputs 
END TYPE FUOUTPUTTYPE
	


TYPE FUOUTPUTSPECTRALTYPE
real rswfu(nv1x,18)	!SW (0.2-4.0) Up
real rswfd(nv1x,18)	!SW (0.2-4.0) Dn
real rswdir(nv1x,18)	!SW (0.2-4.0) Direct
real rswdif(nv1x,18)	!SW (0.2-4.0) Diffuse

real rlwfu(nv1x,10:23)	!LW (4.0-->) Up
real rlwfd(nv1x,10:23)	!LW (4.0-->) Dn
real sbf(nv1x,10:23)	!Level Sig*T**4 (4.0-->)
real sbs(10:23)		!SFC Sig*T**4 (4.0-->)
real swalbtoa(18)
real swalbsfc(18)
real swtran(18)   
real dirsfc(18)  !! CORRECTED FOR FU ICE TAU
real difsfc(18)  !! CORRECTED FOR FU ICE TAU
logical exist		! Flag to test for presence 
END TYPE FUOUTPUTSPECTRALTYPE



TYPE FUTOATYPE
real swdn     ! SW FLUX DOWN INSOLATION TOA
real swup     ! SW FLUX UP TOA
real swalb    ! SW Albedo TOA
real olr      ! Outgoing Longwave TOA
real wnolr    ! Outgoing Window (3band) TOA
real totrad   ! TOA Total Longwave Radiance 
real winrad   ! TOA Window (3band) Radiance
real trwn_flt_r	!emmulated Flt WN Radiance @TOA ( W  m-2 sr-1)
real trwn_unf_r	!emmulated Unflt WN Radiance @TOA ( W  m-2 sr-1)
real trwn_f	!emmulated WN Flux @TOA ( W  m-2 )
real totaniso
real winaniso
logical exist	! Flag to test for presence
END TYPE FUTOATYPE

TYPE FUSFCTYPE
real swdn     ! SW FLUX DOWN INSOLATION SFC
real swup     ! SW FLUX UP SFC
real swalb    ! SW Albedo SFC
real swdir    ! SW Direct SFC
real swdif    ! SW Diffuse SFC
real swpar    ! SW Photosynthetic Active Radiation SFC Dn (3bands)
real lwup     ! Total Longwave Up SFC
real lwdn     ! Total Longwave Down SFC
real wnup     ! Window(3band) Longwave Up SFC
real wndn     ! Window(3band) Longwave Dn SFC
real swtran
real dirsfc  !! CORRECTED FOR FU ICE TAU
real difsfc  !! CORRECTED FOR FU ICE TAU
logical exist	! Flag to test for presence
END TYPE FUSFCTYPE


TYPE FUUVTYPE
real toa_uvb
real toa_uva
real toa_par

real uvb
real uva
real par

real uvindex

real uvb_rdirdif
real uva_rdirdif
real par_rdirdif

real uvb_ery


real par_purv
real par_purv_rdirdif

real par_chla
real par_chla_rdirdif
END TYPE FUUVTYPE




!------------------------------------------------------------------

! Spectral/Profile/Multi/ Cloud Condition 
 TYPE(FUOUTPUTSPECTRALTYPE) 	foscc(0:mccx,2)	 ! (Cldcnd,AOT/NOAOT)

! Spectral/Profile/Multi
 TYPE(FUOUTPUTSPECTRALTYPE) 	fos(nscm)	 ! (1=Clr,2=TOT,3=PRIS,4=TOTnoAOT)


! Profile/Multi/ Cloud Condition 
TYPE(FUOUTPUTTYPE)	    focc(0:mccx,2)	 ! (Cldcnd,AOT/NOAOT)
       
! Profile/Multi/
TYPE(FUOUTPUTTYPE)	    fo(nscm)		 ! (1=Clr,2=TOT,3=PRIS,4=TOTnoAOT)


!TOA / Multi
TYPE (FUTOATYPE) ftoa(nscm)			 ! (1=Clr,2=TOT,3=PRIS,4=TOTnoAOT)

!SFC / Multi
TYPE (FUSFCTYPE) fsfc(nscm)			 ! (1=Clr,2=TOT,3=PRIS,4=TOTnoAOT)

!UV/PAR/ACTINIC LUT Derived
TYPE (FUUVTYPE) fouv(nscm)			 ! (1=Clr,2=TOT,3=PRIS,4=TOTnoAOT)

CONTAINS 
!=================================================================
subroutine only_toa_sfc
implicit none
	real pi
	data pi/3.1415927/

integer i,j
do i=1,nscm
if ( fi%lscm(i)  ) then
ftoa(i)%swdn	     = fo(i)%fds(1)
ftoa(i)%swup	     = fo(i)%fus(1)
 ftoa(i)%swalb	     = omit*0.001
if ( fo(i)%fds(1) > 0 ) ftoa(i)%swalb = fo(i)%fus(1)/fo(i)%fds(1)
ftoa(i)%olr	     = fo(i)%fuir(1)
ftoa(i)%wnolr	     = fo(i)%fuwn(1)
ftoa(i)%totrad	     = fo(i)%fiurt(1)
ftoa(i)%winrad	     = fo(i)%fiurw(1)
ftoa(i)%trwn_flt_r   = fo(i)%trwn_flt_r
ftoa(i)%trwn_unf_r   = fo(i)%trwn_unf_r
ftoa(i)%trwn_f	     = fo(i)%trwn_f
ftoa(i)%exist = .true.


fsfc(i)%swdn	     = fo(i)%fds(nv1)
fsfc(i)%swup	     = fo(i)%fus(nv1)
fsfc(i)%swalb	     = omit*0.001
if ( fo(i)%fds(nv1) > 0 ) fsfc(i)%swalb = fo(i)%fus(nv1)/fo(i)%fds(nv1)
fsfc(i)%swdir	     = fo(i)%fdsdr(nv1)
fsfc(i)%swdif	     = fo(i)%fdsdf(nv1)
fsfc(i)%swpar	     = sum( fos(i)%rswfd(nv1,8:10) )
fsfc(i)%lwup	     = fo(i)%fuir(nv1)
fsfc(i)%lwdn	     = fo(i)%fdir(nv1)
fsfc(i)%wnup	     = fo(i)%fuwn(nv1)
fsfc(i)%wndn	     = fo(i)%fdwn(nv1)

if ( fo(i)%fds(nv1) > 0 ) fsfc(i)%swtran =  fo(i)%fds(nv1)/ fo(i)%fds(1)


	do j = 1,18

	if ( fos(i)%rswfd(1,j) > 0 ) then 
	fos(i)%swalbtoa(j) = fos(i)%rswfu(1,j)/  fos(i)%rswfd(1,j)

	fos(i)%swtran(j)= fos(i)%rswfd(fi%nv+1,j)/ fos(i)%rswfd(1,j)
	endif

	if (fos(i)%rswfd(fi%nv+1,j) > 0 ) &
	fos(i)%swalbsfc(j) = fos(i)%rswfu(fi%nv+1,j)/fos(i)%rswfd(fi%nv+1,j)

	enddo

ftoa(i)%totaniso =   pi *ftoa(i)%totrad / ftoa(i)%olr 
ftoa(i)%winaniso =   pi *ftoa(i)%winrad / ftoa(i)%wnolr


fsfc(i)%dirsfc =   sum( fos(i)%dirsfc(1:18) )
fsfc(i)%difsfc =   sum( fos(i)%difsfc(1:18) )

! FORCE REPLACEMENT WITH CORRECTED VALUES 

 fsfc(i)%swdir = fsfc(i)%dirsfc
 fsfc(i)%swdif = fsfc(i)%difsfc
endif
enddo
end subroutine only_toa_sfc


!=================================================================
subroutine flx_initalize
implicit none
!Initalize
integer i

do i=1,nscm
	fo(i)%fds	=0.0
	fo(i)%fus	=0.0
	fo(i)%fdir	=0.0
	fo(i)%fuir	=0.0
	fo(i)%fuwn	=0.0
	fo(i)%fdwn	=0.0
	fo(i)%fdsdr	=0.0
	fo(i)%fdsdf	=0.0

	fo(i)%fiurt	=0.0
	fo(i)%fiurw	=0.0

	fo(i)%fd	=0.0
	fo(i)%fu	=0.0
	fo(i)%dts	=0.0
	fo(i)%dtir	=0.0
	fo(i)%dt	=0.0

	fo(i)%fu_sr	=0.0
	fo(i)%fu_sf	=0.0
        fo(i)%trwn_flt_r=0.0	
        fo(i)%trwn_unf_r=0.0	
        fo(i)%trwn_f	=0.0
        fo(i)%exist = .true.
!	fo(i)%lwespa = 0.0
!	fo(i)%lweatm = 0.0
!	fo(i)%swespa = 0.0
!	fo(i)%sweatm = 0.0
	
	fo(i)%LW_entropy =0.0
	fo(i)%SW_entropy =0.0
	
	fos(i)%rswfu	=0.0
	fos(i)%rswfd	=0.0
	fos(i)%rswdir	=0.0
	fos(i)%rswdif	=0.0

	fos(i)%rlwfu	=0.0
	fos(i)%rlwfd	=0.0
	fos(i)%sbf	=0.0 
	fos(i)%sbs	=0.0
	fos(i)%swalbtoa	=0.0
	fos(i)%swalbsfc	=0.0
	fos(i)%swtran	=0.0
	fos(i)%dirsfc    =0.0
	fos(i)%difsfc    =0.0
        fos(i)%exist = .true.
	
	
end do
	
	focc=fo(1)  !! Zero componets
	foscc=fos(1)  !! Zero componets
	
end subroutine flx_initalize
!=================================================================
subroutine aux_flx_calc(icc,icp)
implicit none
integer icp,icc,i
real xx,dp

!Net SW+LW
 focc(icc,icp)%fd(1:nv1) = focc(icc,icp)%fds(1:nv1) + focc(icc,icp)%fdir(1:nv1)
 focc(icc,icp)%fu(1:nv1) = focc(icc,icp)%fus(1:nv1) + focc(icc,icp)%fuir(1:nv1)

 HEATRATE: do  i = 1, nv
       dp = ( fi%pp(i+1) - fi%pp(i) )
  xx = focc(icc,icp)%fds(i)   - focc(icc,icp)%fus(i) - 	&
       focc(icc,icp)%fds(i+1) + focc(icc,icp)%fus(i+1)
  focc(icc,icp)%dts(i) = 8.4392 * xx / dp
  xx = focc(icc,icp)%fdir(i)   - focc(icc,icp)%fuir(i) -	&
       focc(icc,icp)%fdir(i+1) + focc(icc,icp)%fuir(i+1)
  focc(icc,icp)%dtir(i) = 8.4392 * xx / dp
  focc(icc,icp)%dt(i) = focc(icc,icp)%dts(i) + focc(icc,icp)%dtir(i)
enddo HEATRATE


!! CERES Window emmulation


 call aqua_wnflt(icc,icp,ur,instrument)


end subroutine aux_flx_calc
!------------------------------------------------------------------
subroutine flx_cloudcombine(icmf,icp)
implicit none
integer icmf,icp,icc
real clrfrac,frac 

! ONLY FOR USE BY FU SOLVER!

clrfrac = 1.0 - sum(fi%fc(1:mccx)%cldfrac)

do icc=0,mccx
 
	if ( icc==0) frac = clrfrac
	if ( icc >0) frac = fi%fc(icc)%cldfrac

	if ( frac > 0) then
!	print'(2i4,f8.3)',icp,i,frac

        call aux_flx_calc(icc,icp) 

	fo(icmf)%fds	= fo(icmf)%fds    + frac *  focc(icc,icp)%fds
	fo(icmf)%fus	= fo(icmf)%fus    + frac *  focc(icc,icp)%fus
	fo(icmf)%fdir	= fo(icmf)%fdir   + frac *  focc(icc,icp)%fdir
	fo(icmf)%fuir	= fo(icmf)%fuir   + frac *  focc(icc,icp)%fuir
	fo(icmf)%fuwn	= fo(icmf)%fuwn   + frac *  focc(icc,icp)%fuwn
	fo(icmf)%fdwn	= fo(icmf)%fdwn   + frac *  focc(icc,icp)%fdwn
	fo(icmf)%fdsdr	= fo(icmf)%fdsdr  + frac *  focc(icc,icp)%fdsdr
	fo(icmf)%fdsdf	= fo(icmf)%fdsdf  + frac *  focc(icc,icp)%fdsdf

	fo(icmf)%fiurt	= fo(icmf)%fiurt  + frac *  focc(icc,icp)%fiurt
	fo(icmf)%fiurw	= fo(icmf)%fiurw  + frac *  focc(icc,icp)%fiurw

	fo(icmf)%fd	= fo(icmf)%fd    + frac *  focc(icc,icp)%fd
	fo(icmf)%fu	= fo(icmf)%fu    + frac *  focc(icc,icp)%fu
	fo(icmf)%dts	= fo(icmf)%dts    + frac *  focc(icc,icp)%dts
	fo(icmf)%dtir	= fo(icmf)%dtir   + frac *  focc(icc,icp)%dtir
	fo(icmf)%dt	= fo(icmf)%dt     + frac *  focc(icc,icp)%dt


	fo(icmf)%fu_sr	= fo(icmf)%fu_sr  + frac *  focc(icc,icp)%fu_sr
	fo(icmf)%fu_sf	= fo(icmf)%fu_sf  + frac *  focc(icc,icp)%fu_sf
        fo(icmf)%trwn_flt_r = fo(icmf)%trwn_flt_r  + frac *  focc(icc,icp)%trwn_flt_r
        fo(icmf)%trwn_unf_r = fo(icmf)%trwn_unf_r  + frac *  focc(icc,icp)%trwn_unf_r
        fo(icmf)%trwn_f     = fo(icmf)%trwn_f      + frac *  focc(icc,icp)%trwn_f

!	fo(icmf)%lwespa= fo(icmf)%lwespa  + frac *  focc(icc,icp)%lwespa
!	fo(icmf)%lweatm= fo(icmf)%lweatm  + frac *  focc(icc,icp)%lweatm

!	fo(icmf)%swespa= fo(icmf)%swespa  + frac *  focc(icc,icp)%swespa
!	fo(icmf)%sweatm= fo(icmf)%sweatm  + frac *  focc(icc,icp)%sweatm
	
	fo(icmf)%LW_entropy= fo(icmf)%LW_entropy  + frac *  focc(icc,icp)%LW_entropy
	fo(icmf)%SW_entropy= fo(icmf)%SW_entropy  + frac *  focc(icc,icp)%SW_entropy

!Spectral
	fos(icmf)%rlwfu	= fos(icmf)%rlwfu  + frac *  foscc(icc,icp)%rlwfu
	fos(icmf)%rlwfd	= fos(icmf)%rlwfd  + frac *  foscc(icc,icp)%rlwfd
	fos(icmf)%sbf	= fos(icmf)%sbf    + frac *  foscc(icc,icp)%sbf
	fos(icmf)%sbs	= fos(icmf)%sbs    + frac *  foscc(icc,icp)%sbs


	fos(icmf)%rswfu	= fos(icmf)%rswfu   + frac *  foscc(icc,icp)%rswfu
	fos(icmf)%rswfd	= fos(icmf)%rswfd   + frac *  foscc(icc,icp)%rswfd
	fos(icmf)%rswdir= fos(icmf)%rswdir  + frac *  foscc(icc,icp)%rswdir
	fos(icmf)%rswdif= fos(icmf)%rswdif  + frac *  foscc(icc,icp)%rswdif

	fos(icmf)%dirsfc= fos(icmf)%dirsfc  + frac *  foscc(icc,icp)%dirsfc
	fos(icmf)%difsfc= fos(icmf)%difsfc  + frac *  foscc(icc,icp)%difsfc
	
	
	endif
end do



	
end subroutine flx_cloudcombine


END MODULE FUOUTPUT
