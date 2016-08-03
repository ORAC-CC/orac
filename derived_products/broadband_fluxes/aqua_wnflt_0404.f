	subroutine aqua_wnflt(icc,icp,COSVZA,instrument)
!! INSTRUMENT  [ -1=OldTrmm 0=TRMM   1=FM1    2=FM2  3=FM3 4=FM4]

!fu_sr      = FU-liou Radiance in each window band
!fu_sf      = FU-liou Flux in each window band

! sat_flt_sr = emmulation of TERRA FILTERED RADIANCE in each window band
! sat_unf_sr = emmulation of TERRA UNFILTERED RADIANCE in each window band

! sat_flt_r = emmulation of TERRA FILTERED RADIANCE 
! sat_unf_r = emmulation of TERRA UNFILTERED RADIANCE 
! sat_f     = emmulation of TERRA Window Flux

       USE FUOUTPUT , only : focc
       implicit none
!! Source {SRBSUN:/usr1/rose/raddb}

        real,dimension(14:16):: fu_sr,fu_sf
	real,dimension(14:16):: adm, sat_flt_sr,sat_unf_sr
	real,dimension(14:16)::      sat_flt_sf,sat_unf_sf
        real :: 	 sat_flt_r,sat_unf_r, sat_f


	integer instrument
        integer icc,icp
       	integer  iwncld
        integer ib
        real ffX,uffX,ff,uff,COSVZA
       real TERRA_FULIOU_FILTER,Trmm_FULIOU_FILTER

      iwncld = 1
       if (icc == 0) iwncld =0 

       fu_sr(14:16)= focc(icc,icp)%fu_sr(14:16)
       fu_sf(14:16)= focc(icc,icp)%fu_sf(14:16)

	 sat_flt_r =0.0
	 sat_unf_r =0.0
	 sat_f     =0.0 
	BAND : do ib = 14,16

	adm(ib) = fu_sf(ib) /(3.14159*fu_sr(ib) )

	if ( instrument >= 0 ) then
	ffX  = TERRA_FULIOU_FILTER(instrument,ib,0,fu_sr(ib),COSVZA)
	uffX = TERRA_FULIOU_FILTER(instrument,ib,1,fu_sr(ib),COSVZA)
	sat_flt_sr(ib) =   ffX
	sat_unf_sr(ib) =  uffX
	sat_flt_r = sat_flt_r + sat_flt_sr(ib)
	sat_unf_r = sat_unf_r + sat_unf_sr(ib)
	
	else
	ff  = Trmm_FULIOU_FILTER(ib,0,iwncld,fu_sr(ib),COSVZA)
	uff = Trmm_FULIOU_FILTER(ib,1,iwncld,fu_sr(ib),COSVZA)
	sat_flt_sr(ib) =  fu_sr(ib) * ff
	sat_unf_sr(ib) = sat_flt_sr(ib) * uff
	sat_flt_r = sat_flt_r + sat_flt_sr(ib)
	sat_unf_r = sat_unf_r + sat_unf_sr(ib)
	endif

! Flux method 1 ( SPECTRAL ADM  )
	sat_f     = sat_f     + sat_unf_sr(ib) * (3.14159*adm(ib))

	enddo BAND
	
       focc(icc,icp)%trwn_flt_r = sat_flt_r
       focc(icc,icp)%trwn_unf_r = sat_unf_r
       focc(icc,icp)%trwn_f     = sat_f

!	print* , 'SCCWN',sum( sat_unf_sr ) / sum( sat_flt_sr )
       
	return
	end subroutine aqua_wnflt

!============================================================
	real function TERRA_FULIOU_FILTER
     &             (instrument,ib,idir,UNFILTERED_RADIANCE,COSVZA)
!!! (844:1227cm-1) 


!!ib - FULIOU BAND !!
! 14 = 1100:1250 cm-1 Fu-Liou band
! 15 = 980:1100 cm-1  Fu-Liou band
! 16 = 800:980 cm-1   Fu-Liou band

!! idir - Filter or UNfilter
! 0 = FU-liou band to FILTERED BAND contribution in [wm-2sr-1] 
! 1 = FU-LIOU BAND to contribution to UNFILTERED WINDOW(844:1227cm-1)in [wm-2sr-1]

! UNFILTERED_RADIANCE - Radiance[wm-2sr-1] corresponding 
!                       to the FU-LIOU band "ib" [14-16]

! COSVZA - Cosine of view zenith angle of toa radiance observation [0-1]

	real ,dimension(4,3,0:1,0:4), save :: coefs
	integer ibt(14:16) 
	data ibt/3,2,1/

	data coefs /
!TRMM 
     &-3.924e-02, 5.021e-01, 1.265e-03,-5.407e-03,
     & 1.970e-03, 7.161e-01,-2.780e-05, 1.325e-03,
     & 2.393e-03, 6.374e-01, 6.574e-04,-1.246e-02,
     &-7.644e-02, 7.167e-01, 1.584e-03,-6.769e-03,
     & 0.000e+00, 1.000e+00, 0.000e+00, 0.000e+00,
     & 3.883e-03, 8.982e-01, 1.379e-03,-2.814e-02,
! FM1 
     &-4.204e-02, 5.016e-01, 1.381e-03,-6.063e-03,
     & 3.479e-03, 7.282e-01,-3.641e-05, 1.969e-03,
     & 1.760e-03, 6.741e-01, 3.709e-04,-7.265e-03,
     &-7.644e-02, 7.167e-01, 1.584e-03,-6.769e-03,
     & 0.000e+00, 1.000e+00, 0.000e+00, 0.000e+00,
     & 3.883e-03, 8.982e-01, 1.379e-03,-2.814e-02,
!FM2 
     &-3.894e-02, 5.014e-01, 1.260e-03,-5.418e-03,
     & 2.979e-03, 7.185e-01,-2.664e-05, 2.024e-03,
     & 2.160e-03, 6.501e-01, 5.066e-04,-9.626e-03,
     &-7.644e-02, 7.167e-01, 1.584e-03,-6.769e-03,
     & 0.000e+00, 1.000e+00, 0.000e+00, 0.000e+00,
     & 3.883e-03, 8.982e-01, 1.379e-03,-2.814e-02,
! FM3 
     &-4.572e-02, 5.581e-01, 1.421e-03,-6.041e-03,
     & 4.007e-03, 7.995e-01,-2.884e-05, 2.217e-03,
     & 2.380e-03, 7.437e-01, 6.103e-04,-1.179e-02,
     &-7.644e-02, 7.167e-01, 1.584e-03,-6.769e-03,
     & 0.000e+00, 1.000e+00, 0.000e+00, 0.000e+00,
     & 3.880e-03, 8.982e-01, 1.379e-03,-2.814e-02,
! FM4 
     &-4.605e-02, 5.620e-01, 1.432e-03,-6.082e-03,
     & 3.899e-03, 8.054e-01,-2.880e-05, 2.155e-03,
     & 2.425e-03, 7.491e-01, 6.156e-04,-1.188e-02,
     &-7.644e-02, 7.167e-01, 1.584e-03,-6.769e-03,
     & 0.000e+00, 1.000e+00, 0.000e+00, 0.000e+00,
     & 3.880e-03, 8.982e-01, 1.379e-03,-2.814e-02/



	if ( ib < 14 .or. ib > 16) then
	 TERRA_FULIOU_FILTER=0.0
	 return
	endif



!	print*,'INSTRUMENT',instrument

	TERRA_FULIOU_FILTER = 
     &				  coefs(1,ibt(ib),idir,instrument) +
     & UNFILTERED_RADIANCE     *  coefs(2,ibt(ib),idir,instrument) + 
     & UNFILTERED_RADIANCE**2  *  coefs(3,ibt(ib),idir,instrument) +
     & COSVZA*UNFILTERED_RADIANCE*coefs(4,ibt(ib),idir,instrument)  


	return
	end function TERRA_FULIOU_FILTER

!============================================================
	real function Trmm_FULIOU_FILTER
     &             (ib,idir,icld,UNFILTERED_RADIANCE,COSVZA)
	parameter (ibw = 1) ! ibw=1 (844:1227cm-1) 
			    ! ibw=2 (847:1219cm-1) 
!!ib - FULIOU BAND !!
! 14 = 1100:1250 cm-1 Fu-Liou band
! 15 = 980:1100 cm-1  Fu-Liou band
! 16 = 800:980 cm-1   Fu-Liou band

!! idir - Filter or UNfilter
! 0 = FU-liou band to FILTERED BAND
! 1 = FILTERED BAND to TERRA WINDOW 

!! icld - Clear or Cloudy Sky
! 0 = Clear
! 1 = Cloudy 

! UNFILTERED_RADIANCE - Radiance[wm-2sr-1] corresponding 
!                       to the FU-LIOU band "ib" [14-16]

! COSVZA - Cosine of view zenith angle of toa radiance observation [0-1]

	real coefs(4,3,0:1,0:1,2)
	integer ibt(14:16) 
	data ibt/3,2,1/
	data coefs/ 
!! (844:1227cm-1)
     x 5.137e-01, 5.002e-04,-4.887e-03, 3.724e-05,
     x 7.156e-01,-2.305e-04, 9.184e-04, 1.272e-05,
     x 6.067e-01, 6.400e-04,-9.066e-03, 3.633e-05,
     x 1.382e+00,-8.756e-05, 7.637e-04,-3.077e-06,
     x 1.398e+00, 4.567e-04,-1.826e-03,-2.524e-05,
     x 1.463e+00,-3.787e-04, 1.142e-03,-4.245e-05,
     x 4.946e-01, 3.123e-03,-2.532e-04,-6.721e-05,
     x 7.187e-01,-1.150e-03, 2.028e-04, 9.825e-05,
     x 6.101e-01,-2.080e-03,-2.730e-03, 2.223e-04,
     x 1.384e+00,-3.153e-04, 1.790e-04, 6.385e-06,
     x 1.391e+00, 2.233e-03,-3.908e-04,-1.896e-04,
     x 1.460e+00, 8.689e-04,-2.329e-04,-1.286e-04,
!!! (847:1219cm-1)
     x 5.137e-01, 5.002e-04,-4.887e-03, 3.724e-05,
     x 7.156e-01,-2.305e-04, 9.184e-04, 1.272e-05,
     x 6.067e-01, 6.400e-04,-9.066e-03, 3.633e-05,
     x 1.346e+00, 3.661e-05, 5.024e-04,-1.379e-06,
     x 1.398e+00, 4.567e-04,-1.826e-03,-2.524e-05,
     x 1.390e+00, 4.193e-04,-8.215e-03, 5.476e-05,
     x 4.946e-01, 3.123e-03,-2.532e-04,-6.721e-05,
     x 7.187e-01,-1.150e-03, 2.028e-04, 9.825e-05,
     x 6.101e-01,-2.080e-03,-2.730e-03, 2.223e-04,
     x 1.344e+00, 2.939e-04, 1.753e-04,-1.015e-05,
     x 1.391e+00, 2.233e-03,-3.908e-04,-1.896e-04,
     x 1.395e+00,-2.624e-03,-2.174e-03, 2.697e-04/

	if ( ib < 14 .or. ib > 16) then
	Trmm_FULIOU_FILTER=1.0000
	return
	endif



	icld_use = icld
	if ( icld_use== 1 )then
	if ( ib == 14 .and. UNFILTERED_RADIANCE >  8.0) icld_use =0
	if ( ib == 15 .and. UNFILTERED_RADIANCE >  8.0) icld_use =0
	if ( ib == 16 .and. UNFILTERED_RADIANCE > 20.0) icld_use =0
	endif

	Trmm_FULIOU_FILTER = 
     &				coefs(1,ibt(ib),idir,icld_use,ibw) +
     & UNFILTERED_RADIANCE    * coefs(2,ibt(ib),idir,icld_use,ibw) + 
     & COSVZA                 * coefs(3,ibt(ib),idir,icld_use,ibw) +
     & UNFILTERED_RADIANCE**2 * coefs(4,ibt(ib),idir,icld_use,ibw)  

	return
	end function Trmm_FULIOU_FILTER

