
      subroutine aerosol_init
c
c                        8/14/95, 4/1/97 , 2/10/2000
c
c  **********************************************************************
c  Subroutine to create aerosol optical properties.  There are several
c  inputs and 6 outputs.  
c
c    INPUTS FROM COMMON BLOCKS OR HEADER FILE:
c
c    a_tau(nwi) :  The input column aerosol optical depth
c    (real)           (common block "aer_tau" - see header file).
c
c    a_wli(nwi) :  Wavelength in microns corresponding to aerosol tau in "a_tau"
c
c    aprof(# layers): The input aerosol optical depth profile - LAYERS
c    (real)           (common block "aer_prof").
c
c    itp:       Aerosol type, given in header file rad_0598.h.
c
c    ifg:       The table will compute vertical distributions based on
c    (integer)  relative humidity (see explanation below).  If ifg is
c               set to 0, each layer will have properties calculated
c               based on the relative humidity of that layer.  If ifg
c               is set equal to another integer (1 through the number of
c               relative humidities given in the block data "aerosol")
c               the routine will calculate a vertical profile of optical
c               properties based on the relative humidity corresponding
c               to the index given.  The indices are: 1: 0%; 2: 50%;
c               3: 70%; 4: 80%; 5:90%; 6: 95%; 7: 98%; and 8: 99%.
c               If the number of relative humidities changes, these
c               numbers will have to be modified.
c
c    ivd:       Vertical tau distribution flag.  If set to zero, the 
c               distribution is based on Jim Spinhirne's marine 
c               distribution formulation, and no user input is required.  
c               If set to one, the user's own vertical distribution is 
c               used, and must be present in the array aprof(nlayers).
c               NOTE: This vertical distribution is used as a weighting 
c               factor ONLY, to distribute input column optical depths!
c
c----------------------------------------------------------------------------
c    a_ssa, a_ext, a_asy:  Input single-scattering albedos, extinction
c           coefficients, and asymmetry parameters.  These variables 
c           are dimensioned (# of bands, # of relative humidities,
c           # of aerosol types). An x or y is appended on these 
c           variable names: if x, the numbers correspond to the 18 
c           original bands.  If y, the numbers are for the 10 
c           sub-intervals in the first shortwave band (.2-.7 microns).  
c           All of these variables come from the block data statements 
c           aerosol# (# corresponds to an integer, eg. aerosol1) and 
c           are in common blocks aer_optx and aer_opty.
c
c    nv,mb,pp,pt,ph,dz: number of layers, number of bands, and the
c           pressure, temperature, humidity and thickness profiles.
c           These are shared by several subroutines.
c
c    OUTPUTS:
c
c    a_tau1,a_ssa1,a_asy1:  The optical depth, single-scattering albedo,
c       and asymmetry parameter vertical profiles for 18 bands.  These
c       are dimensioned (nvx, 18)  These are in the common block
c       aer_initx, which is shared by the subroutine "aerosolx".  
c
c    a_tau2,a_ssa2,a_asy2:  Properties for SW band 1's 10 subintervals.  
c       These are dimensioned (nvx, 10)  These are in the common block
c       aer_inity, which is shared by the subroutine "aerosoly".  
c
c  **********************************************************************
      USE FUINPUT
      USE EXTRAS, only : sh_rh
      implicit none
	
      integer iq,mtop,n,m,ict,ix,iy,irh,krh,iac,itp 
      real, dimension(mbx,nrh,naer) :: a_ssax,a_extx,a_asyx
      real, dimension(mby,nrh,naer) :: a_ssay,a_exty,a_asyy

      real, dimension(nvx) :: tauxxx
      real, dimension(nvx,mbx,mxac) :: a_tau1,a_ext1,a_ssa1,a_asy1
      real, dimension(nvx,mby,mxac) :: a_tau2,a_ext2,a_ssa2,a_asy2

      real ,dimension(nvx)  :: taux1,taux2,rh,ht,rhp
      real sumxxx

      real,dimension(mxat) :: a_tau
      real,dimension(nvx)  :: aprof
	real,dimension(nvx,mbx) :: wvd_x
	real,dimension(nvx,mby) :: wvd_y

      real p1,h1,z,sig,tp 
      real rhx(nrh)
      real wts(4),tau3(2),tau3y(4)
      real aotf,wlf,sump,rirh
      real spinhirne_sig, spinhirne_tau
      real aot_by_band
	
      common /aer_optx/ a_ssax,a_extx,a_asyx
      common /aer_opty/ a_ssay,a_exty,a_asyy
      common /aer_initx/ a_tau1,a_ssa1,a_asy1
      common /aer_inity/ a_tau2,a_ssa2,a_asy2
!      common /atmos/ pp(nv1x), pt(nv1x), ph(nv1x), po(nv1x)

      common /tau_spline_aot/ aotf(18),wlf(18)
      common /aotbyband/ aot_by_band(18)
      
      data rhx /0.,50.,70.,80.,90.,95.,98.,99./
      data wts /.23015,.28274,.25172,.23539/
 
c  Initialize.
	aot_by_band=0.0
	
	 rh     = -9999.
	 a_ssa1 = 0. ; a_ext1 = 0. ; a_asy1 = 0. ; a_tau1 = 0.
	 a_ssa2 = 0. ; a_ext2 = 0. ; a_asy2 = 0. ; a_tau2 = 0.

!	if ( nac < 0 .or. nac > mxac ) fi%ierror= 11 ! print*, 'nac:# Aerosol Constituents'
!	if (n_atau<0 .or.n_atau>mxat) fi%ierror= 12 !print*, 'n_atau:# Aerosol Tau / Wavelengths'
!	if (ifg < 0 .or. ifg > 8)  fi%ierror= 13 ! print*, 'ifg: Aerosol RH% Flag'
	AEROSOL_CONSTITUENTS : do iac = 1,nac

!!!	a_wli(1:n_atau) = a_wlis(1:n_atau,iac)
	a_tau(1:n_atau) = a_taus(1:n_atau,iac)
	aprof(1:nv)  = aprofs(1:nv,iac)
	itp	      = itps(iac)
!	print*, iac,itp
!	if ( itp < 1 .or. itp > naer ) fi%ierror= 14 ! print*, ' itp : Bad Aerosol Type'
!	print*,'CONSTITUENTS',iac,itp

! FOR Aerosol Optical Properties types that are constant with RH	 
	if (itp==1  .or. itp==2 .or. itp==3 .or.
     &      itp==10 .or. itp==12 .or.itp==13 .or. itp==18 ) then
!!       Has already been filled in Block data 
	else
	do krh=2,8
	 a_extx(1:mbx,krh,itp)= a_extx(1:mbx,1,itp)
	 a_ssax(1:mbx,krh,itp)= a_ssax(1:mbx,1,itp)
	 a_asyx(1:mbx,krh,itp)= a_asyx(1:mbx,1,itp)

	 a_exty(1:mby,krh,itp)= a_exty(1:mby,1,itp)
	 a_ssay(1:mby,krh,itp)= a_ssay(1:mby,1,itp)
	 a_asyy(1:mby,krh,itp)= a_asyy(1:mby,1,itp)

	enddo

	endif
!	if ( ifg .ne.0) print*,'CHECK',ifg,itp,a_ssax(1:mbx,ifg,itp)


       

c  ******************************************************************
c  Calculate heights at center of layer - find highest layer to place
c  aerosols (15 km) - calculate relative humidities of each layer as
c  needed.  Values of RH > 99% will be set equal to 99% to make table
c  lookup easier. "mtop" is the highest aerosol layer.
c  ******************************************************************
      z=0.
      m=nv
      iq=0
      do while (iq.eq.0)
       ht(m)=(z*2.+dz(m))/2.
       z=z+dz(m)
c       if (z.gt.15.) then
       if (z.gt.50. .or. m.eq.1) then
         iq=1
         mtop=m
       endif
       p1=(pp(m)+pp(m+1))/2.
       tp=(pt(m)+pt(m+1))/2.
       h1=(ph(m)+ph(m+1))/2.
!       call ql_rh(rh(m),tp,p1,h1)  
            rh(m)= sh_rh(tp,p1,h1)
       if (rh(m).gt.98.9) rh(m)=98.9
       if ((rh(m).lt..01).and.(rh(m).gt.-999.)) rh(m)=0.
       m=m-1
       end do

c  *************************************************************
c  Calculate vertical distribution of asymmetry, ss albedo and
c  extinction, based on aerosol type and relative humidity.  
c  If ifg is not equal to 0, parameters  will corresponds to a 
c  single RH, as described in header file. Loop 31 deals with 
c  the 18 original bands, loop 32 with the 10 band 1 subintervals.
c  *************************************************************
      do 30 m=mtop,nv
       do 31 n=1,mbx
        if (rh(m).eq.-9999.) then
          a_ext1(m,n,iac)=-9999.
          a_ssa1(m,n,iac)=-9999.
          a_asy1(m,n,iac)=-9999.
        else
          if (ifg.eq.0) then          ! Dependence on layer RH.
            ict=2
            do while (rh(m).ge.rhx(ict))
             ict=ict+1
             end do
            a_ext1(m,n,iac)=a_extx(n,ict-1,itp)+(rh(m)-rhx(ict-1))/
     1     (rhx(ict)-rhx(ict-1))*(a_extx(n,ict,itp)-a_extx(n,ict-1,itp))
            a_ssa1(m,n,iac)=a_ssax(n,ict-1,itp)+(rh(m)-rhx(ict-1))/
     1     (rhx(ict)-rhx(ict-1))*(a_ssax(n,ict,itp)-a_ssax(n,ict-1,itp))
            a_asy1(m,n,iac)=a_asyx(n,ict-1,itp)+(rh(m)-rhx(ict-1))/
     1     (rhx(ict)-rhx(ict-1))*(a_asyx(n,ict,itp)-a_asyx(n,ict-1,itp))
	  rhp(m) = rh(m)
          else                        ! Dependence on prescribed RH.
       ict=ifg
            a_ext1(m,n,iac)=a_extx(n,ict,itp)
            a_ssa1(m,n,iac)=a_ssax(n,ict,itp)
            a_asy1(m,n,iac)=a_asyx(n,ict,itp)
          endif
        endif
 31     continue
!-------------------------------------------
       do 32 n=1,mby
        if (rh(m).eq.-9999.) then
          a_ext2(m,n,iac)=-9999.
          a_ssa2(m,n,iac)=-9999.
          a_asy2(m,n,iac)=-9999.
        else
          if (ifg.eq.0) then          ! Dependence on layer RH.
            ict=2
            do while (rh(m).ge.rhx(ict))
             ict=ict+1
             end do
            a_ext2(m,n,iac)=a_exty(n,ict-1,itp)+(rh(m)-rhx(ict-1))/
     1     (rhx(ict)-rhx(ict-1))*(a_exty(n,ict,itp)-a_exty(n,ict-1,itp))
            a_ssa2(m,n,iac)=a_ssay(n,ict-1,itp)+(rh(m)-rhx(ict-1))/
     1     (rhx(ict)-rhx(ict-1))*(a_ssay(n,ict,itp)-a_ssay(n,ict-1,itp))
            a_asy2(m,n,iac)=a_asyy(n,ict-1,itp)+(rh(m)-rhx(ict-1))/
     1     (rhx(ict)-rhx(ict-1))*(a_asyy(n,ict,itp)-a_asyy(n,ict-1,itp))
          else                        ! Dependence on prescribed RH.
       ict=ifg
            a_ext2(m,n,iac)=a_exty(n,ict,itp)
            a_ssa2(m,n,iac)=a_ssay(n,ict,itp)
            a_asy2(m,n,iac)=a_asyy(n,ict,itp)
          endif
        endif
 32     continue

 30    continue

c  ******************************************************************
c  Vertical distribution of aerosol optical depths - CAGEX and CERES.
c       --------------------------------------------------------------
c       Use Spinhirne's vertical distribution of scattering properties 
c       to calculate vertical distribution of optical depths.  The  
c       distribution gives a scattering coefficient ("sig"). Use this,  
c       along with the single-scattering albedo, to produce an  
c       RH-dependent extinction coefficient (extx, exty, etc.), from  
c       which optical depth is calculated (taux, tauy, etc.).  This  
c       optical depth is summed (sum1, sumy2, sum, etc.) to give  
c       column tau for weighting purposes.
c       --------------------------------------------------------------

     	select case (ivd) 
	case default
	 fi%ierror = 15 ! print*, ' ivd : Aerosol Profile flag'
	case (0)  !! DEFAULT VERTICAL DISTRIBUTION Spinhirne

	sumxxx=0.0

        do  m=mtop,nv
 	
	 sig = spinhirne_sig( ht(m))
	 tauxxx(m) = spinhirne_tau(sig,a_ssa2(m,9,iac),dz(m))
	 sumxxx   = sumxxx + tauxxx(m)
!		print*,m,sig,a_ssa2(m,9,iac)
        enddo

	do m=mtop,nv
	 tauxxx(m) = tauxxx(m)  / sumxxx
!!!	 aprofs(m,iac) = tauxxx(m) !! See what the Sphinhirne profiles look like
	enddo

! ----------------------------------------------------------------
	case (1)   ! USER'S OWN VERTICAL DISTRIBUTION IVD=1
         sump =   sum( aprof(mtop:nv) )
	 tauxxx(mtop:nv)= aprof(mtop:nv) / sump 

         if(sump.eq.0.)fi%ierror = 16 !print*, 'No VERTICAL Profile OF AEROSOL TAU '
	 
	end select

c  ********************************************************************
c  IAFORM=2
c
c  Distribute optical depth spectrally into the first 2 Fu-Liou bands.  
c  Band 1 will consist of the first 4 MFRSR bands, weighted with 
c  respect to energy.  Band two will be the fifth MFRSR band. 
c
c  Also, distribute optical depths into 4 of the 10 band 1 subintervals.  
c  Subinterval 7 is directly inserted, since there is one MFRSR 
c  measurement within the range of this band.  Subintervals 7 and 8 
c  straddle the .497 micron MFRSR measurement, so interpolated values 
c  are inserted into these, using .409 and .497 measurements for 7, and 
c  .497 and .606 for 8.  Subinterval 10 contains two MFRSR measurements, 
c  so it is filled using an energy-weighted average.  This is all 
c  hardwired, so we need all of the MFRSR bands (.409, .497, .606, and 
c  .661) for it to work. (The .855 micron band is also needed, but not 
c  for this interval distribution.
c  ********************************************************************
  
	select case ( iaform )
	 case default
	 fi%ierror = 17 ! print*, ' iaform : Bad value of iaform '
	case(1)        ! CERES
!! No operations necessary

	case(2)        ! For CAGEX
      
        tau3(1)=a_tau(1)*wts(1)+a_tau(2)*wts(2)+
     1          a_tau(3)*wts(3)+a_tau(4)*wts(4)
        tau3(2)=a_tau(5)
        tau3y(1)=a_tau(1)      ! For subinterval 7 of 1st band (.409)
        tau3y(2)=a_tau(1)+.6705*(a_tau(2)-a_tau(1)) ! Subi 8 of band 1
        tau3y(3)=a_tau(2)+.4541*(a_tau(3)-a_tau(2)) ! Subi 9 of band 1
        tau3y(4)=a_tau(3)*.5175+a_tau(4)*.4825      ! Subi 10 of band 1
 
	case(3)        ! For AOT_SPLINEFIT
	
	if ( ifg == 0 ) then ! Find Aerosol weighted collumn mean RH index 
	 rirh=0
	 do m =mtop,nv
	 rirh = rirh + rhp(m)* tauxxx(m)  !! Aerosol Profile weighted mean RH
!	 print*,m,rhp(m),tauxxx(m)
	 enddo 
       
	   irh =1
	   do ix= 1,7
	   if( rirh >= rhx(ix) .and. rirh < rhx(ix+1) ) irh=ix
	   enddo 
	   if( rirh >= rhx(8) )irh =8
       
	else  ! Use assigned RH index
	 irh = ifg
	endif
 
! Can't handle ZERO in Log interpolation
	where ( a_tau(1:n_atau) .lt. 1.0E-20) a_tau(1:n_atau) = 1.0E-20

       call atau_spline_iaform3(mxat,n_atau,a_wli,a_tau,itp,irh)

!	write(22,'(a20,15f8.3)') 'AOT in Fu Bands',aotf(1:15)

!!! ACCOUNT FOR VERTICAL EXTINCTION VARIABILITY WITH HUMIDITY ABOUT THE MEAN RH "irh"
!!! ( IAFORM==3) only
	do iy = 1,mby
	wvd_y(mtop:nv,iy)=tauxxx(mtop:nv)
     &             *a_ext2(mtop:nv,iy,iac)/a_exty(iy,irh,itp)
	sump =   sum( wvd_y(mtop:nv,iy) )
	wvd_y(mtop:nv,iy) =  wvd_y(mtop:nv,iy) /sump
	enddo

	do ix = 1,mbx
	wvd_x(mtop:nv,ix)=tauxxx(mtop:nv)
     &             *a_ext1(mtop:nv,ix,iac)/a_extx(ix,irh,itp)
	sump =   sum( wvd_x(mtop:nv,ix) )
	wvd_x(mtop:nv,ix) =  wvd_x(mtop:nv,ix) /sump
	enddo

	end select


! ----------------------------------------------------------------
c       Use weighted optical depths  to distribute our input 
c       column optical depths vertically and spectrally where needed.  
c       For bands with "measured" input, we simply do the weighting.  
c       For the remaining bands, we weight according to our vertically 
c       distributed extinction coefficients (calculated in loop 30), 
c       which carry all the spectral resolution we need.  a_tau1 is for 
c       the 18 original bands, a_tau2 is for the 10 band 1 subintervals.
! ----------------------------------------------------------------
        VERTICAL : do  m=mtop,nv

	select case ( iaform )
        
	case(1)       ! For CERES

           a_tau1(m,1,iac)   = a_tau(1) * tauxxx(m)
           a_tau1(m,2:mbx,iac)= a_tau1(m,1,iac)*
     &                 a_ext1(m,2:mbx,iac)/a_ext1(m,1,iac)

           a_tau2(m,9,iac)  = a_tau(1) * tauxxx(m)

           a_tau2(m,1:mby,iac)=a_tau2(m,9,iac)*
     &                 a_ext2(m,1:mby,iac)/a_ext2(m,9,iac)

        case(2)        ! For CAGEX

	    a_tau1(m,1:2,iac) = tau3(1:2) * tauxxx(m)
            a_tau1(m,3:mbx,iac)=a_tau1(m,2,iac)* 
     &                a_ext1(m,3:mbx,iac)/a_ext1(m,2,iac)

	    a_tau2(m,7:10,iac) = tau3y(1:4) * tauxxx(m)
            a_tau2(m,1:6,iac)  = a_tau2(m,7,iac)* 
     &                a_ext2(m,1:6,iac)/a_ext2(m,7,iac)

	 case(3)       ! For AOT_SPLINEFIT

	 
!	a_tau2(m,1:10,iac) = aotf(1:10)  * tauxxx(m)
	a_tau2(m,1:mby,iac) = aotf(1:mby)  * wvd_y(m,1:mby)
!	a_tau1(m,1,iac)    = aotf(9)     * tauxxx(m)
	a_tau1(m,1,iac)    = aotf(9)     * wvd_x(m,1)
!	a_tau1(m,2:6,iac)  = aotf(11:15) * tauxxx(m)
	a_tau1(m,2:mbsx,iac)  = aotf(11:18) * wvd_x(m,2:mbsx)
            a_tau1(m,10:mbx,iac) =a_tau1(m,2,iac)*
     &                 a_ext1(m,10:mbx,iac)/a_ext1(m,2,iac)



         end select

!	print'(3I4,2f8.2,16f7.3)', m,iac,itp,dz(m),rh(m),
!     & (wvd_y(m,iy),iy=1,10),(wvd_x(m,ix),ix=1,6)

        enddo VERTICAL

	call aot_by_band_out(iac)
	enddo AEROSOL_CONSTITUENTS
	
	
!! BY BAND constituent integrated AOT by band Example...
!!   WAV in Fu Bands:   0.214   0.234   0.270   0.293   0.311   0.341   0.402   0.468   0.546   0.641   0.740   0.839   0.961   1.160   1.549   2.154   2.900   3.734
!!    AOT by band    :   2.216   2.095   1.916   1.816   1.743   1.637   1.439   1.249   1.063   0.883   0.738   0.612   0.511   0.391   0.225   0.170   0.109   0.091
!	
!       print'(a20,18f8.3)', 'WAV in Fu Bands:',wlf(1:18)
!       print'(a20,18f8.3)', 'AOT by band    :',aot_by_band(1:18)
!	stop
 
      return
      end
!===========================================================================
      subroutine aot_by_band_out(iac) 
! Added to get AOT by fuliou band output in ED4 SUNI for Wenying      
      USE FUINPUT
      implicit none
      real ,dimension(nvx,mbx,mxac) :: a_tau1,a_ssa1,a_asy1
      real ,dimension(nvx,mby,mxac) :: a_tau2,a_ssa2,a_asy2
      real aotf,wlf,aot_by_band
      common /aer_initx/ a_tau1,a_ssa1,a_asy1
      common /aer_inity/ a_tau2,a_ssa2,a_asy2
      common /tau_spline_aot/ aotf(18),wlf(18)
  
      integer iac
      real xxx
      integer ii,jj
      
      common /aotbyband/ aot_by_band(18)
      
      
!!!--- Diagnostic Output of Atau
       do ii=1,10
       xxx=0
	do jj=1,nv
	xxx =xxx+ a_tau2(jj,ii,iac)
	enddo
       aotf(ii)=xxx
       enddo

       do ii=2,6
       xxx=0
	do jj=1,nv
	xxx =xxx+ a_tau1(jj,ii,iac)
	enddo
       aotf(9+ii)=xxx
       enddo
       
       aot_by_band(1:18) =aot_by_band(1:18) + aotf(1:18)
       
!       print'(a20,18f8.3)', 'WAV in Fu Bands',wlf(1:18)
!       print'(a20,18f8.3)', 'AOT in Fu Bands',aotf(1:18)
 !      print'(a20,18f8.3)', 'AOT by band int',aot_by_band(1:18)
!	print*,'iac/NAC',iac,nac
      
      
      
      end subroutine aot_by_band_out
!===========================================================================
      subroutine aerosolxy ( ib,cmode )
c *********************************************************************
c                      Modified 2/14/00
c
c tae, wae, and wwae are the optical depth, single scattering albedo,
c and expansion coefficients of the phase function ( 1, 2, 3, and 4 )
c due to the Mie scattering of aerosols for a given layer. 
c
c  This subroutine is called for bands 2 - 18 (ib) 
c  or vis subbands 1-10 (ig)
c *********************************************************************
      USE FUINPUT
      implicit none
	character*1 cmode
      integer i,ib,iac
      real x1,x2,x3,x4,y1,y2,y3,y4,tae,wae,wwae
      real ,dimension(nvx,mbx,mxac) :: a_tau1,a_ssa1,a_asy1
      real ,dimension(nvx,mby,mxac) :: a_tau2,a_ssa2,a_asy2
      common /aer_initx/ a_tau1,a_ssa1,a_asy1
      common /aer_inity/ a_tau2,a_ssa2,a_asy2

      common /aer/ tae(nvx,mxac), wae(nvx,mxac), wwae(nvx,4,mxac)
     
      AEROSOL_CONSTITUENTS  : do iac=1,nac

      LEVELS : do  i = 1, nv
       select case (cmode)
	case ('x')
         tae(i,iac) = a_tau1(i,ib,iac)
         wae(i,iac) = a_ssa1(i,ib,iac)
         x1     = a_asy1(i,ib,iac)
	case ('y')
         tae(i,iac) = a_tau2(i,ib,iac)
         wae(i,iac) = a_ssa2(i,ib,iac)
         x1     = a_asy2(i,ib,iac)
       end select

       x2 = x1 * x1
       x3 = x2 * x1
       x4 = x3 * x1
       y1 = 3.0 * x1
       y2 = 5.0 * x2
       y3 = 7.0 * x3
       y4 = 9.0 * x4
  
       wwae(i,1,iac) = y1
       wwae(i,2,iac) = y2
       wwae(i,3,iac) = y3
       wwae(i,4,iac) = y4

	enddo LEVELS
	enddo AEROSOL_CONSTITUENTS

      return
      end
!----------------------------------------------------------------
	real function spinhirne_sig(ht)

	data sig0,a,ap,b,bp,f/0.025,0.4,2981.0,1.6,2.5,1.5e-7/

         t1=  sig0*(1+a)**2
         t4 = f*(1+ap)**2

         t2 = exp(ht/b)
         t3 = (a+exp(ht/b))**2
         t5 = exp(ht/bp)
         t6 = (a+exp(ht/bp))**2
         spinhirne_sig=t1*t2/t3+t4*t5/t6   ! scattering coefficient

	return
	end
!---------------------------------------------
	real function spinhirne_tau(sig,ssa,dz)
	ext = sig / ssa
	spinhirne_tau = ext / dz
	return
	end
!=========================================================================
	subroutine atau_spline_iaform3(mxat,nwi,wli,aoti,ityp,irh)
	parameter(nsub=5 ,nfuo=18,nwo=nsub*nfuo)
	common /aot_spect_5/  wlo2(5,18) , hkas(5,18) ,sflx (5,18)

	real ,dimension(mxat):: aoti,wli
	real ,dimension(nwo):: aoto,wlo
	real , dimension(nsub,nfuo) :: aoto2

	common /tau_spline_aot/ aotf(18),wlf(18)

!	wlo = reshape(wlo2,(/nwo/))
	kk=0
	do jj=1,nfuo
	do ii=1,nsub
	kk=kk+1
	wlo(kk) = wlo2(ii,jj)
	enddo;enddo


	call aot_ext
     &   (mxat,nwi,aoti,wli,nwo,wlo,aoto,ityp,irh)

!	aoto2 = reshape(aoto,(/5,15/))
	kk=0
	do jj=1,nfuo
	do ii=1,nsub
	kk=kk+1
	aoto2(ii,jj)=aoto(kk)
	enddo;enddo


	wlf=0.0 ; aotf =0.0
	zord = 0.0
	do j=1,nfuo
	do i = 1,nsub
	 wlf(j) =  wlf(j)+  wlo2(i,j) * hkas(i,j)
	 aotf(j)= aotf(j)+ aoto2(i,j) * hkas(i,j)
	 zord = zord + sflx (i,j)*exp(-aoto2(i,j)) 
	enddo
	enddo

!-  WRITE OUT interpolated AOTs
	
!	do i=1,nwo
!	write(11) d1,d2,wlo(i),aoto(i),float(irec),log(wlo(i)),log(aoto(i)),float(ityp)
!	enddo
!	print'(A6,f10.3, 3i4)','FLUX= ',zord ,nsub,ityp,irh
	return
	end
!----------------------------------------------------------------
	subroutine aot_ext
     & (mxat,nwin,aotin,wlin,nwo,wlo,aoto,ityp,irh)
        USE FUINPUT, only:fi
	parameter(ne=24)
      
	real ,dimension(mxat)::aotin,wlin
!	real ,allocatable,dimension(-100:100) :: aoti,wlix
	real ,dimension(-100:100) :: aoti,wlix
	real ,dimension(nwo) :: aoto,wlo
	real ,dimension(ne) :: wlp,extp

	common /dalm_ext/    wld(24) ,datd(24,8,3)
	common /mineral_ext/ wlt(24) ,datt(24,4:8)
 	common /opac_ext/ wlopac(24) ,datopac(24,8,9:18) 
	common /mineral_ext_1004/ datt2(24,19:29)
! Wavelength MICRONS
! wlix,aoti = Monotonicly increasing

	idtl=-1
	if ( ityp >= 1 .and. ityp <=3 )then ! d'Almedia
	 wlp  = wld
	 extp = datd(1:24,irh,ityp)
	 idtl=1
	elseif ( ityp >=4 .and. ityp <= 8 ) then ! Tegen&Lacis
	 wlp  = wlt
	 extp = datt(1:24,ityp)
	 idtl=2
	elseif (  ityp >=19 .and. ityp <= 29 ) then ! Tegen&Lacis_1004
	 wlp  = wlt
	 extp = datt2(1:24,ityp)

	 idtl=2
	elseif ( ityp >=9 .and. ityp <= 18) then ! OPAC 
	 wlp  = wlopac
	  if (ityp==10 .or. ityp==12 .or.ityp==13 .or.ityp==18 ) then
	   extp = datopac(1:24,irh,ityp)
	  else
	   extp = datopac(1:24,  1,ityp)
	  endif



	 idtl=3
	else
	 fi%ierror = 18 !print*, ' Bad Aerosol type'
	endif


	if ( nwin == 1) then
!	 nes=-3 ; nel=19  ! 1 chan @ 500nm
	 if( wlin(1) <= 0.325 .or. wlin(1) >= 0.675) 
     & fi%ierror = 19 ! print*, ' OUT OF ALLOWABLE ARANGE'
	 nes= -(wlin(1)-0.325)/0.05 
	 if( idtl == 3) nes=nes-1  ! OPAC starts at 0.25um instead of 0.30
	 nel = 22+nes

!	 print*,'NES NEL',nes,nel,wlin(1)
   
	else
	 nes=0  
	if(idtl==1) nel=8    !  >= 2um long d'Almedia
	if(idtl==2) nel=11   !  >= 2um long Tegin&Lacis
	if(idtl==3) nel=7    !  >= 2um long OPAC

	endif

	nb = ne+1-nel
	nwi= nwin+nel-nes+1
	iend=nwin+nel
!	print*, icall,'in AOTEXT',nes,iend

!	if ( allocated (aoti) ) deallocate ( aoti )
!	allocate( aoti(nes:iend) )


!	if ( allocated (wlix) ) deallocate ( wlix)
!	allocate(  wlix(nes:iend) )
!	if(icall == 2) print*,

	wlix(1:nwin) =wlin(1:nwin)
	aoti(1:nwin)=aotin(1:nwin)


	LONGSIDE : do i=1,ne
	if( wlix(nwin) >= wlp(i) .and.  wlix(nwin) <= wlp(i+1)) then
!	 print*,i  ,wlp(i),extp(i) ,i+1,wlp(i+1),extp(i+1)

	ext_norm1= rlnintrp( wlp(i),wlp(i+1),
     &        extp(i),extp(i+1),wlix(nwin))

!	 print*,dy,dx,dx1,yy,ext_norm1
	 exit
	 endif
	enddo LONGSIDE
! 	 wlix(nwin+1:nwi) = wlp(nb:ne)
!	 aoti(nwin+1:nwi) = aoti(nwin)*(extp(nb:ne)/ext_norm1)
 	 wlix(nwin+1:nwin+nel) = wlp(nb:ne)
	 aoti(nwin+1:nwin+nel) = aoti(nwin)*(extp(nb:ne)/ext_norm1)

!         print*,'CHECK',nwin+1,nwi,nb,ne,nwin+nel
!         print*,wlp(nb:ne)
!------	
	if (nwin == 1 ) then
!	print*,1,wlix(1),aoti(1)
	SHORTSIDE: do i=1,ne
	if( wlix(1) >= wlp(i) .and.  wlix(1) <= wlp(i+1)) then
!	 print*,i  ,wlp(i),extp(i) , i+1,wlp(i+1),extp(i+1)

	ext_norm0= rlnintrp( wlp(i),wlp(i+1),
     &        extp(i),extp(i+1),wlix(1))

!	 print*,dy,dx,dx1,yy,ext_norm0
	 exit
	 endif
	enddo SHORTSIDE
	   nq = -nes+1
	   wlix(nes:0) = wlp(1:nq)
	  aoti(nes:0) = aoti(1)*(extp(1:nq)/ext_norm0)
	else
	    wlix(0)  = 0.001
	   aoti(0) = 1
	endif
!------------------------------------------------------------------
!	print'(a18,40f7.3)','Wavelength input= ',wlix(nes:iend)
!	print'(a18,40f7.3)','       AOT input= ',aoti(nes:iend)

!	do i=nes,iend
!	write(10) d1,d2,wlix(i),aoti(i),float(irec),log(wlix(i)),log(aoti(i)),float(ityp)
!	enddo

	call aotspline(nwi,aoti(nes:iend),wlix(nes:iend),nwo,wlo,aoto)

!	print'(a18,500f7.3)','Wavelength Out= ',wlo
!	print'(a18,500f7.3)','       AOT Out= ',aoto


	return
	end
!===================================================================
!===================================================================
	real function rlnintrp(x1,x2,y1,y2, x)
	 dx= log(x2) - log(x1)
	 dy= log(y2) - log(y1)
	 dx1=log(x)  - log(x1)
	 yy= (dy/dx) * dx1
	 rlnintrp = exp(log(y1)+yy) 
	return
	end
!====================================================================
	subroutine aotspline(nwi,aoti,wli,nwo,wlo,aoto)

	real ,dimension(nwi)  :: aoti,wli
	
	real ,dimension(nwi+1):: xa,ya,y2a
	real ,dimension(nwo)  :: aoto,wlo,aa

	data yp1,ypn /1.0E+32,1.0E+32/
	
	nwi2=nwi+1

	xa(2:nwi+1)=log(wli(1:nwi))
	ya(2:nwi+1)=log(aoti(1:nwi))
	xa(1)=log(1.0E-6) !; xa(nwi2)=log(1.0E+6) !TENSION
	ya(1)=0   	  !; ya(nwi2)= ya(nwi+1)!TENSION	

	call spline(xa,ya,nwi2,yp1,ypn,y2a)

	do iwo = 1,nwo
	 x=log(wlo(iwo))
	 call splint(xa,ya,y2a,nwi2,x,y)
	 aoto(iwo)=exp(y) 
	enddo	  

	return
	end
!------------------------------------------------------
	real function alphav(aot1,aot2,wl1,wl2)
	ar= aot1/aot2
	wr= wl1/wl2
     	alphav = - log(ar)/ log(wr)
	return
	end
!---------------------------------------------------------------
      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n,NMAX
      REAL yp1,ypn,x(n),y(n),y2(n)
      PARAMETER (NMAX=500)
      INTEGER i,k
      REAL p,qn,sig,un,u(NMAX)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      USE FUINPUT, only:fi
      INTEGER n
      REAL x,y,xa(n),y2a(n),ya(n)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) fi%ierror = 20 ! print*, 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE polint(xa,ya,n,x,y,dy)
        USE FUINPUT, only:fi
      INTEGER n,NMAX
      REAL dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      REAL den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0.) fi%ierror = 21 !print*, 'failure in polint'
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .
!=============================================================
!===========================================================
 	block data opac_extinctions  		  
 	common /opac_ext/ wl(24) ,edat(24,8,9:18) 
       data wl /                                             
     &0.2500E+00,0.3000E+00,0.3500E+00,0.4000E+00,0.4500E+00,0.5000E+00,
     &0.5500E+00,0.6000E+00,0.6500E+00,0.7000E+00,0.7500E+00,0.8000E+00,
     &0.9000E+00,0.1000E+01,0.1250E+01,0.1500E+01,0.1750E+01,0.2000E+01,
     &0.2500E+01,0.3000E+01,0.3200E+01,0.3390E+01,0.3500E+01,0.3750E+01/
 !-----------------------------------------------------------
 !9)  inso	Insoluble                                         
       data (( edat(i,j, 9),i=1,24), j=1,1 ) /               
     &0.9477E+00,0.9572E+00,0.9667E+00,0.9748E+00,0.9839E+00,0.9916E+00,
     &0.1000E+01,0.1008E+01,0.1016E+01,0.1024E+01,0.1031E+01,0.1038E+01,
     &0.1052E+01,0.1064E+01,0.1093E+01,0.1105E+01,0.1088E+01,0.1012E+01,
     &0.7983E+00,0.6625E+00,0.7897E+00,0.8403E+00,0.8668E+00,0.8205E+00/
 !-----------------------------------------------------------
 !10) waso      Water Soluble 			(8 RH%)                     
       data (( edat(i,j,10),i=1,24), j=1,8 ) /               
     &0.2438E+01,0.2095E+01,0.1793E+01,0.1539E+01,0.1326E+01,0.1148E+01,
     &0.1000E+01,0.8739E+00,0.7689E+00,0.6782E+00,0.6032E+00,0.5249E+00,
     &0.4251E+00,0.3497E+00,0.2191E+00,0.1520E+00,0.9186E-01,0.4897E-01,
     &0.3146E-01,0.2746E-01,0.1577E-01,0.1314E-01,0.1167E-01,0.9260E-02,
     &0.2373E+01,0.2044E+01,0.1758E+01,0.1516E+01,0.1314E+01,0.1144E+01,
     &0.1000E+01,0.8785E+00,0.7759E+00,0.6882E+00,0.6135E+00,0.5395E+00,
     &0.4380E+00,0.3605E+00,0.2271E+00,0.1560E+00,0.9755E-01,0.5793E-01,
     &0.3256E-01,0.1309E+00,0.5573E-01,0.2602E-01,0.1914E-01,0.1332E-01,
     &0.2319E+01,0.2006E+01,0.1733E+01,0.1501E+01,0.1306E+01,0.1140E+01,
     &0.1000E+01,0.8812E+00,0.7803E+00,0.6942E+00,0.6199E+00,0.5477E+00,
     &0.4458E+00,0.3676E+00,0.2326E+00,0.1597E+00,0.1014E+00,0.6249E-01,
     &0.3361E-01,0.1628E+00,0.6956E-01,0.3125E-01,0.2246E-01,0.1529E-01,
     &0.2263E+01,0.1967E+01,0.1708E+01,0.1485E+01,0.1298E+01,0.1137E+01,
     &0.1000E+01,0.8840E+00,0.7847E+00,0.7002E+00,0.6264E+00,0.5557E+00,
     &0.4536E+00,0.3748E+00,0.2386E+00,0.1639E+00,0.1054E+00,0.6684E-01,
     &0.3483E-01,0.1857E+00,0.8036E-01,0.3571E-01,0.2545E-01,0.1714E-01,
     &0.2143E+01,0.1884E+01,0.1653E+01,0.1451E+01,0.1279E+01,0.1130E+01,
     &0.1000E+01,0.8897E+00,0.7942E+00,0.7128E+00,0.6405E+00,0.5724E+00,
     &0.4705E+00,0.3910E+00,0.2521E+00,0.1741E+00,0.1146E+00,0.7589E-01,
     &0.3791E-01,0.2186E+00,0.9814E-01,0.4402E-01,0.3134E-01,0.2095E-01,
     &0.2000E+01,0.1785E+01,0.1588E+01,0.1410E+01,0.1257E+01,0.1120E+01,
     &0.1000E+01,0.8968E+00,0.8062E+00,0.7286E+00,0.6585E+00,0.5932E+00,
     &0.4922E+00,0.4122E+00,0.2707E+00,0.1887E+00,0.1272E+00,0.8739E-01,
     &0.4252E-01,0.2441E+00,0.1154E+00,0.5345E-01,0.3848E-01,0.2582E-01,
     &0.1809E+01,0.1650E+01,0.1497E+01,0.1353E+01,0.1224E+01,0.1106E+01,
     &0.1000E+01,0.9072E+00,0.8240E+00,0.7520E+00,0.6856E+00,0.6241E+00,
     &0.5252E+00,0.4454E+00,0.3007E+00,0.2134E+00,0.1481E+00,0.1055E+00,
     &0.5081E-01,0.2683E+00,0.1372E+00,0.6728E-01,0.4955E-01,0.3371E-01,
     &0.1686E+01,0.1563E+01,0.1437E+01,0.1314E+01,0.1202E+01,0.1097E+01,
     &0.1000E+01,0.9142E+00,0.8366E+00,0.7687E+00,0.7051E+00,0.6462E+00,
     &0.5496E+00,0.4702E+00,0.3238E+00,0.2331E+00,0.1648E+00,0.1196E+00,
     &0.5785E-01,0.2815E+00,0.1521E+00,0.7776E-01,0.5825E-01,0.4012E-01/
 !-----------------------------------------------------------
 !11) soot	Soot                                              
       data (( edat(i,j,11),i=1,24), j=1,1 ) /               
     &0.2447E+01,0.2188E+01,0.1837E+01,0.1555E+01,0.1331E+01,0.1153E+01,
     &0.1000E+01,0.8818E+00,0.7906E+00,0.7082E+00,0.6445E+00,0.5904E+00,
     &0.5087E+00,0.4453E+00,0.3412E+00,0.2767E+00,0.2367E+00,0.2055E+00,
     &0.1639E+00,0.1398E+00,0.1284E+00,0.1218E+00,0.1189E+00,0.1105E+00/
 !-----------------------------------------------------------
 !12) ssam	Sea Salt (Accumulation Mode) 	(8 RH%)             
       data (( edat(i,j,12),i=1,24), j=1,8 ) /               
     &0.8801E+00,0.9114E+00,0.9354E+00,0.9580E+00,0.9733E+00,0.9887E+00,
     &0.1000E+01,0.1002E+01,0.1005E+01,0.1003E+01,0.9963E+00,0.9846E+00,
     &0.9618E+00,0.9232E+00,0.8315E+00,0.7230E+00,0.6190E+00,0.5382E+00,
     &0.3864E+00,0.4589E+00,0.3172E+00,0.2808E+00,0.2656E+00,0.2284E+00,
     &0.8883E+00,0.9080E+00,0.9321E+00,0.9508E+00,0.9715E+00,0.9866E+00,
     &0.1000E+01,0.1013E+01,0.1021E+01,0.1026E+01,0.1030E+01,0.1030E+01,
     &0.1023E+01,0.1009E+01,0.9498E+00,0.8710E+00,0.7792E+00,0.6931E+00,
     &0.4881E+00,0.6444E+00,0.6217E+00,0.5226E+00,0.4772E+00,0.4012E+00,
     &0.8884E+00,0.9093E+00,0.9300E+00,0.9493E+00,0.9672E+00,0.9853E+00,
     &0.1000E+01,0.1012E+01,0.1023E+01,0.1031E+01,0.1036E+01,0.1040E+01,
     &0.1041E+01,0.1032E+01,0.9874E+00,0.9212E+00,0.8368E+00,0.7534E+00,
     &0.5404E+00,0.6945E+00,0.6969E+00,0.5975E+00,0.5486E+00,0.4654E+00,
     &0.8932E+00,0.9113E+00,0.9305E+00,0.9508E+00,0.9671E+00,0.9850E+00,
     &0.1000E+01,0.1016E+01,0.1025E+01,0.1035E+01,0.1044E+01,0.1050E+01,
     &0.1054E+01,0.1052E+01,0.1020E+01,0.9637E+00,0.8874E+00,0.8075E+00,
     &0.5909E+00,0.7366E+00,0.7591E+00,0.6629E+00,0.6126E+00,0.5246E+00,
     &0.9051E+00,0.9182E+00,0.9355E+00,0.9528E+00,0.9682E+00,0.9843E+00,
     &0.1000E+01,0.1014E+01,0.1028E+01,0.1040E+01,0.1050E+01,0.1059E+01,
     &0.1072E+01,0.1077E+01,0.1070E+01,0.1036E+01,0.9774E+00,0.9089E+00,
     &0.6961E+00,0.8117E+00,0.8691E+00,0.7870E+00,0.7377E+00,0.6452E+00,
     &0.9158E+00,0.9311E+00,0.9419E+00,0.9586E+00,0.9717E+00,0.9857E+00,
     &0.1000E+01,0.1015E+01,0.1025E+01,0.1037E+01,0.1052E+01,0.1062E+01,
     &0.1080E+01,0.1094E+01,0.1109E+01,0.1098E+01,0.1064E+01,0.1013E+01,
     &0.8218E+00,0.8883E+00,0.9782E+00,0.9207E+00,0.8777E+00,0.7878E+00,
     &0.9361E+00,0.9476E+00,0.9567E+00,0.9668E+00,0.9761E+00,0.9905E+00,
     &0.1000E+01,0.1010E+01,0.1019E+01,0.1030E+01,0.1041E+01,0.1051E+01,
     &0.1075E+01,0.1090E+01,0.1126E+01,0.1144E+01,0.1141E+01,0.1122E+01,
     &0.9894E+00,0.9762E+00,0.1094E+01,0.1079E+01,0.1052E+01,0.9811E+00,
     &0.9487E+00,0.9578E+00,0.9655E+00,0.9729E+00,0.9804E+00,0.9933E+00,
     &0.1000E+01,0.1008E+01,0.1014E+01,0.1023E+01,0.1033E+01,0.1042E+01,
     &0.1064E+01,0.1079E+01,0.1119E+01,0.1148E+01,0.1164E+01,0.1166E+01,
     &0.1091E+01,0.1025E+01,0.1148E+01,0.1162E+01,0.1150E+01,0.1102E+01/
 !-----------------------------------------------------------
 !13) sscm	Sea Salt (Coarse Mode) 		(8 RH%)                  
       data (( edat(i,j,13),i=1,24), j=1,8 ) /               
     &0.9667E+00,0.9741E+00,0.9797E+00,0.9810E+00,0.9885E+00,0.9930E+00,
     &0.1000E+01,0.9993E+00,0.1010E+01,0.1015E+01,0.1013E+01,0.1015E+01,
     &0.1029E+01,0.1039E+01,0.1062E+01,0.1075E+01,0.1107E+01,0.1123E+01,
     &0.1170E+01,0.1175E+01,0.1208E+01,0.1222E+01,0.1228E+01,0.1241E+01,
     &0.9759E+00,0.9817E+00,0.9864E+00,0.9907E+00,0.9941E+00,0.9969E+00,
     &0.1000E+01,0.1001E+01,0.1008E+01,0.1011E+01,0.1014E+01,0.1016E+01,
     &0.1023E+01,0.1027E+01,0.1041E+01,0.1060E+01,0.1073E+01,0.1093E+01,
     &0.1134E+01,0.1094E+01,0.1129E+01,0.1156E+01,0.1167E+01,0.1188E+01,
     &0.9786E+00,0.9834E+00,0.9872E+00,0.9894E+00,0.9929E+00,0.9969E+00,
     &0.1000E+01,0.1003E+01,0.1006E+01,0.1010E+01,0.1013E+01,0.1014E+01,
     &0.1019E+01,0.1028E+01,0.1040E+01,0.1051E+01,0.1070E+01,0.1082E+01,
     &0.1121E+01,0.1083E+01,0.1116E+01,0.1140E+01,0.1151E+01,0.1172E+01,
     &0.9798E+00,0.9855E+00,0.9882E+00,0.9925E+00,0.9953E+00,0.9978E+00,
     &0.1000E+01,0.1005E+01,0.1007E+01,0.1010E+01,0.1015E+01,0.1014E+01,
     &0.1021E+01,0.1023E+01,0.1040E+01,0.1051E+01,0.1064E+01,0.1078E+01,
     &0.1113E+01,0.1079E+01,0.1109E+01,0.1130E+01,0.1141E+01,0.1159E+01,
     &0.9830E+00,0.9866E+00,0.9899E+00,0.9929E+00,0.9965E+00,0.9983E+00,
     &0.1000E+01,0.1003E+01,0.1006E+01,0.1009E+01,0.1012E+01,0.1014E+01,
     &0.1018E+01,0.1024E+01,0.1035E+01,0.1042E+01,0.1054E+01,0.1065E+01,
     &0.1097E+01,0.1069E+01,0.1095E+01,0.1111E+01,0.1120E+01,0.1136E+01,
     &0.9843E+00,0.9864E+00,0.9885E+00,0.9925E+00,0.9954E+00,0.9977E+00,
     &0.1000E+01,0.1002E+01,0.1005E+01,0.1006E+01,0.1010E+01,0.1010E+01,
     &0.1015E+01,0.1016E+01,0.1029E+01,0.1038E+01,0.1044E+01,0.1056E+01,
     &0.1078E+01,0.1060E+01,0.1080E+01,0.1093E+01,0.1100E+01,0.1112E+01,
     &0.9872E+00,0.9898E+00,0.9923E+00,0.9948E+00,0.9966E+00,0.9977E+00,
     &0.1000E+01,0.1002E+01,0.1004E+01,0.1002E+01,0.1007E+01,0.1009E+01,
     &0.1013E+01,0.1016E+01,0.1025E+01,0.1032E+01,0.1040E+01,0.1046E+01,
     &0.1060E+01,0.1052E+01,0.1066E+01,0.1075E+01,0.1079E+01,0.1087E+01,
     &0.9882E+00,0.9875E+00,0.9913E+00,0.9877E+00,0.9953E+00,0.9983E+00,
     &0.1000E+01,0.1001E+01,0.1003E+01,0.1004E+01,0.1006E+01,0.1007E+01,
     &0.1010E+01,0.1013E+01,0.1020E+01,0.1026E+01,0.1032E+01,0.1038E+01,
     &0.1049E+01,0.1044E+01,0.1056E+01,0.1063E+01,0.1066E+01,0.1071E+01/
 !-----------------------------------------------------------
 !14) minm	Mineral Dust (Nucleation Mode)                    
       data (( edat(i,j,14),i=1,24), j=1,1 ) /               
     &0.1000E+01,0.9711E+00,0.9279E+00,0.8725E+00,0.8129E+00,0.7512E+00,
     &0.6916E+00,0.6347E+00,0.5817E+00,0.5327E+00,0.4879E+00,0.4470E+00,
     &0.3760E+00,0.3175E+00,0.2129E+00,0.1474E+00,0.1051E+00,0.7735E-01,
     &0.4556E-01,0.3771E-01,0.2699E-01,0.2199E-01,0.1993E-01,0.1462E-01/
 !-----------------------------------------------------------
 !15) miam	Mineral Dust (Accumulation Mode)                  
       data (( edat(i,j,15),i=1,24), j=1,1 ) /               
     &0.9037E+00,0.9193E+00,0.9354E+00,0.9513E+00,0.9682E+00,0.9837E+00,
     &0.1000E+01,0.1015E+01,0.1031E+01,0.1045E+01,0.1056E+01,0.1069E+01,
     &0.1088E+01,0.1103E+01,0.1117E+01,0.1105E+01,0.1074E+01,0.1030E+01,
     &0.9115E+00,0.7974E+00,0.7466E+00,0.7089E+00,0.6875E+00,0.6277E+00/
 !-----------------------------------------------------------
 !16) micm	Mineral Dust (Coarse Mode)                        
       data (( edat(i,j,16),i=1,24), j=1,1 ) /               
     &0.9742E+00,0.9790E+00,0.9836E+00,0.9878E+00,0.9922E+00,0.9959E+00,
     &0.1000E+01,0.1004E+01,0.1008E+01,0.1012E+01,0.1014E+01,0.1018E+01,
     &0.1026E+01,0.1032E+01,0.1048E+01,0.1065E+01,0.1082E+01,0.1096E+01,
     &0.1128E+01,0.1151E+01,0.1167E+01,0.1178E+01,0.1184E+01,0.1199E+01/
 !-----------------------------------------------------------
 !17) mitr	Mineral Dust (Transported Mode)                   
       data (( edat(i,j,17),i=1,24), j=1,1 ) /               
     &0.9303E+00,0.9420E+00,0.9537E+00,0.9652E+00,0.9773E+00,0.9879E+00,
     &0.1000E+01,0.1010E+01,0.1021E+01,0.1032E+01,0.1042E+01,0.1052E+01,
     &0.1073E+01,0.1089E+01,0.1124E+01,0.1145E+01,0.1158E+01,0.1155E+01,
     &0.1123E+01,0.1056E+01,0.1031E+01,0.1010E+01,0.9970E+00,0.9592E+00/
 !-----------------------------------------------------------
 !18) suso 	Sulfate Droplets		(8 RH%)                        
       data (( edat(i,j,18),i=1,24), j=1,8 ) /               
     &0.1589E+01,0.1522E+01,0.1418E+01,0.1303E+01,0.1190E+01,0.1092E+01,
     &0.1000E+01,0.9143E+00,0.8376E+00,0.7654E+00,0.6996E+00,0.6399E+00,
     &0.5385E+00,0.4523E+00,0.2965E+00,0.1989E+00,0.1377E+00,0.9764E-01,
     &0.4830E-01,0.9007E-01,0.1102E+00,0.1189E+00,0.1158E+00,0.9204E-01,
     &0.1348E+01,0.1324E+01,0.1275E+01,0.1210E+01,0.1140E+01,0.1070E+01,
     &0.1000E+01,0.9327E+00,0.8690E+00,0.8095E+00,0.7523E+00,0.6989E+00,
     &0.6053E+00,0.5239E+00,0.3671E+00,0.2634E+00,0.1894E+00,0.1396E+00,
     &0.6786E-01,0.2235E+00,0.1501E+00,0.1073E+00,0.9444E-01,0.7255E-01,
     &0.1270E+01,0.1260E+01,0.1228E+01,0.1178E+01,0.1121E+01,0.1062E+01,
     &0.1000E+01,0.9397E+00,0.8817E+00,0.8269E+00,0.7733E+00,0.7227E+00,
     &0.6326E+00,0.5533E+00,0.3966E+00,0.2903E+00,0.2118E+00,0.1581E+00,
     &0.7746E-01,0.2542E+00,0.1694E+00,0.1142E+00,0.9794E-01,0.7432E-01,
     &0.1213E+01,0.1214E+01,0.1193E+01,0.1154E+01,0.1107E+01,0.1055E+01,
     &0.1000E+01,0.9457E+00,0.8920E+00,0.8410E+00,0.7904E+00,0.7420E+00,
     &0.6550E+00,0.5774E+00,0.4212E+00,0.3129E+00,0.2309E+00,0.1740E+00,
     &0.8606E-01,0.2758E+00,0.1859E+00,0.1223E+00,0.1035E+00,0.7800E-01,
     &0.1130E+01,0.1144E+01,0.1137E+01,0.1114E+01,0.1082E+01,0.1044E+01,
     &0.1000E+01,0.9549E+00,0.9094E+00,0.8652E+00,0.8201E+00,0.7764E+00,
     &0.6957E+00,0.6218E+00,0.4674E+00,0.3562E+00,0.2682E+00,0.2055E+00,
     &0.1039E+00,0.3110E+00,0.2181E+00,0.1419E+00,0.1185E+00,0.8881E-01,
     &0.1047E+01,0.1070E+01,0.1078E+01,0.1072E+01,0.1055E+01,0.1030E+01,
     &0.1000E+01,0.9671E+00,0.9313E+00,0.8955E+00,0.8577E+00,0.8197E+00,
     &0.7476E+00,0.6795E+00,0.5295E+00,0.4157E+00,0.3210E+00,0.2510E+00,
     &0.1311E+00,0.3533E+00,0.2636E+00,0.1744E+00,0.1455E+00,0.1094E+00,
     &0.9649E+00,0.9946E+00,0.1015E+01,0.1023E+01,0.1023E+01,0.1014E+01,
     &0.1000E+01,0.9820E+00,0.9595E+00,0.9353E+00,0.9080E+00,0.8792E+00,
     &0.8213E+00,0.7633E+00,0.6247E+00,0.5105E+00,0.4084E+00,0.3287E+00,
     &0.1810E+00,0.4156E+00,0.3391E+00,0.2356E+00,0.1991E+00,0.1520E+00,
     &0.9232E+00,0.9524E+00,0.9764E+00,0.9920E+00,0.1001E+01,0.1004E+01,
     &0.1000E+01,0.9924E+00,0.9792E+00,0.9644E+00,0.9463E+00,0.9258E+00,
     &0.8810E+00,0.8331E+00,0.7091E+00,0.5986E+00,0.4932E+00,0.4068E+00,
     &0.2352E+00,0.4729E+00,0.4135E+00,0.3016E+00,0.2589E+00,0.2015E+00/
            end  
!=============================================================
	block data tegen_lacis_ext

	common /mineral_ext/ wl(24) ,dat(24,4:8)

	data wl /
     +   0.30,   0.35,   0.40,   0.45,   0.50,   0.55,
     +   0.60,   0.65,   0.70,   0.80,   1.00,   1.25,
     +   1.50,   2.00,   2.51,   2.61,   2.83,   2.96,
     +   3.04,   3.26,   3.47,   3.69,   3.90,   4.11/

	data dat/
!  4
     1 8.78E-01,9.06E-01,9.36E-01,9.67E-01,1.00E+00,1.03E+00,
     2 1.06E+00,1.08E+00,1.09E+00,1.09E+00,1.02E+00,8.70E-01,
     3 7.04E-01,4.36E-01,2.08E-01,1.95E-01,1.91E-01,1.73E-01,
     4 1.51E-01,1.35E-01,9.41E-02,7.95E-02,6.45E-02,5.36E-02,
!  5
     1 9.41E-01,9.56E-01,9.70E-01,9.85E-01,1.00E+00,1.02E+00,
     2 1.03E+00,1.05E+00,1.07E+00,1.10E+00,1.18E+00,1.25E+00,
     3 1.27E+00,1.19E+00,8.64E-01,8.36E-01,7.68E-01,7.25E-01,
     4 6.93E-01,6.92E-01,5.44E-01,4.96E-01,4.41E-01,3.93E-01,
!  6
     1 9.65E-01,9.75E-01,9.83E-01,9.92E-01,1.00E+00,1.01E+00,
     2 1.02E+00,1.02E+00,1.03E+00,1.05E+00,1.08E+00,1.12E+00,
     3 1.17E+00,1.27E+00,1.34E+00,1.34E+00,1.29E+00,1.29E+00,
     4 1.30E+00,1.33E+00,1.27E+00,1.24E+00,1.21E+00,1.17E+00,
!  7
     1 9.78E-01,9.84E-01,9.89E-01,9.95E-01,1.00E+00,1.00E+00,
     2 1.01E+00,1.01E+00,1.02E+00,1.03E+00,1.05E+00,1.07E+00,
     3 1.09E+00,1.13E+00,1.19E+00,1.20E+00,1.21E+00,1.22E+00,
     4 1.24E+00,1.25E+00,1.29E+00,1.31E+00,1.34E+00,1.36E+00,
!  8
     1 9.86E-01,9.90E-01,9.93E-01,9.97E-01,1.00E+00,1.00E+00,
     2 1.01E+00,1.01E+00,1.01E+00,1.02E+00,1.03E+00,1.04E+00,
     3 1.05E+00,1.08E+00,1.10E+00,1.11E+00,1.11E+00,1.12E+00,
     4 1.12E+00,1.13E+00,1.14E+00,1.15E+00,1.17E+00,1.18E+00
     & /

	end
!============================================================
        block data tegen_lacis_ext_0404
        common /mineral_ext_1004/ dat(24,19:29) 
        data ((dat(i,j),i=1,24),j=19,25)/    
  !  19
     1   0.877,   0.905,   0.935,   0.967,   1.000,   1.031,
     2   1.057,   1.077,   1.090,   1.092,   1.013,   0.837,
     3   0.651,   0.384,   0.208,   0.194,   0.190,   0.172,
     4   0.150,   0.134,   0.094,   0.079,   0.064,   0.053,
 !  20
     1   0.941,   0.953,   0.967,   0.982,   1.000,   1.016,
     2   1.032,   1.047,   1.065,   1.103,   1.179,   1.249,
     3   1.261,   1.131,   0.862,   0.834,   0.766,   0.723,
     4   0.692,   0.690,   0.543,   0.495,   0.440,   0.392,
 !  21
     1   0.966,   0.975,   0.984,   0.992,   1.000,   1.012,
     2   1.014,   1.021,   1.033,   1.047,   1.084,   1.127,
     3   1.179,   1.280,   1.339,   1.341,   1.294,   1.288,
     4   1.301,   1.334,   1.268,   1.245,   1.210,   1.168,
 !  22
     1   0.979,   0.985,   0.990,   0.996,   1.000,   1.006,
     2   1.008,   1.014,   1.020,   1.031,   1.047,   1.078,
     3   1.091,   1.135,   1.192,   1.201,   1.211,   1.223,
     4   1.238,   1.253,   1.294,   1.315,   1.339,   1.361,
 !  23
     1   0.986,   0.990,   0.993,   0.997,   1.000,   1.003,
     2   1.007,   1.009,   1.013,   1.019,   1.029,   1.042,
     3   1.054,   1.078,   1.106,   1.105,   1.112,   1.118,
     4   1.123,   1.131,   1.144,   1.155,   1.165,   1.176,
 !  24
     1   0.900,   0.931,   0.960,   0.983,   1.000,   1.010,
     2   1.014,   1.012,   1.002,   0.969,   0.854,   0.682,
     3   0.522,   0.304,   0.164,   0.153,   0.149,   0.135,
     4   0.118,   0.106,   0.074,   0.062,   0.051,   0.042,
 !  25
     1   0.927,   0.944,   0.963,   0.981,   1.000,   1.020,
     2   1.040,   1.059,   1.077,   1.105,   1.134,   1.125,
     3   1.089,   1.012,   0.927,   0.919,   0.875,   0.862,
     4   0.864,   0.885,   0.833,   0.821,   0.807,   0.790
     & /
     
!#201004 ADD LACIS2004 aerosols consistent with ED3Match dust sizes    
!26
       data (dat(i,26),i=1,24) /
     &  0.8809,  0.9212,  0.9581,  0.9854,   1.000,   1.001,
     &  0.9882,  0.9656,  0.9312,  0.8512,  0.6653,  0.4642,
     &  0.3187,  0.1598, 0.07843, 0.07274, 0.07864, 0.07073,
     & 0.05864, 0.04823, 0.03301, 0.02710, 0.02093, 0.01686/
!27
       data (dat(i,27),i=1,24) /
     &  0.9288,  0.9453,  0.9648,  0.9829,   1.000,   1.021,
     &   1.042,   1.065,   1.088,   1.133,   1.204,   1.224,
     &   1.160,  0.9237,  0.6324,  0.6053,  0.5587,  0.5203,
     &  0.4867,  0.4752,  0.3583,  0.3197,  0.2772,  0.2422/
!28
      data (dat(i,28),i=1,24) /
     &  0.9594,  0.9694,  0.9793,  0.9899,   1.000,  0.9980,
     &   1.018,   1.029,   1.035,   1.050,   1.094,   1.151,
     &   1.214,   1.310,   1.308,   1.300,   1.233,   1.214,
     &   1.216,   1.247,   1.134,   1.094,   1.043,  0.9877/
!29
      data (dat(i,29),i=1,24) /
     &  0.9740,  0.9808,  0.9879,  0.9930,   1.000,   1.006,
     &   1.011,   1.016,   1.022,   1.033,   1.059,   1.081,
     &   1.109,   1.169,   1.246,   1.257,   1.259,   1.272,
     &   1.291,   1.313,   1.348,   1.365,   1.382,   1.391/
  
  
        end block data tegen_lacis_ext_0404
!=============================================================
	block data dalmedia_ext

	common /dalm_ext/ wl(24) ,dat(24,8,3)

	data wl /
     +   0.30,   0.35,   0.40,   0.45,   0.50,   0.55,
     +   0.60,   0.65,   0.70,   0.75,   0.80,   0.90,
     +   1.00,   1.25,   1.50,   1.75,   2.00,   2.50,
     +   3.00,   3.20,   3.39,   3.50,   3.78,   4.00/
	data dat/
!  1
     1 2.07E-04,2.08E-04,2.08E-04,2.07E-04,2.07E-04,2.08E-04,
     2 2.09E-04,2.11E-04,2.12E-04,2.12E-04,2.12E-04,2.09E-04,
     3 2.04E-04,1.90E-04,1.79E-04,1.71E-04,1.67E-04,1.63E-04,
     4 1.72E-04,1.69E-04,1.62E-04,1.66E-04,1.67E-04,1.67E-04,
     1 2.45E-04,2.46E-04,2.45E-04,2.44E-04,2.43E-04,2.43E-04,
     2 2.44E-04,2.45E-04,2.46E-04,2.44E-04,2.43E-04,2.38E-04,
     3 2.31E-04,2.14E-04,2.01E-04,1.91E-04,1.85E-04,1.79E-04,
     4 1.90E-04,1.87E-04,1.83E-04,1.83E-04,1.84E-04,1.84E-04,
     1 3.52E-04,3.50E-04,3.50E-04,3.51E-04,3.50E-04,3.47E-04,
     2 3.47E-04,3.47E-04,3.49E-04,3.50E-04,3.51E-04,3.51E-04,
     3 3.48E-04,3.28E-04,3.10E-04,2.92E-04,2.80E-04,2.63E-04,
     4 2.85E-04,2.82E-04,2.71E-04,2.68E-04,2.65E-04,2.65E-04,
     1 7.98E-04,7.93E-04,7.87E-04,7.86E-04,7.82E-04,7.80E-04,
     2 7.83E-04,7.86E-04,7.86E-04,7.84E-04,7.82E-04,7.83E-04,
     3 7.90E-04,8.10E-04,8.05E-04,7.79E-04,7.47E-04,6.70E-04,
     4 7.23E-04,7.47E-04,7.11E-04,6.94E-04,6.68E-04,6.55E-04,
     1 1.14E-03,1.12E-03,1.12E-03,1.11E-03,1.11E-03,1.11E-03,
     2 1.11E-03,1.11E-03,1.11E-03,1.11E-03,1.11E-03,1.11E-03,
     3 1.11E-03,1.13E-03,1.15E-03,1.14E-03,1.11E-03,9.98E-04,
     4 1.05E-03,1.10E-03,1.06E-03,1.04E-03,9.94E-04,9.67E-04,
     1 1.69E-03,1.67E-03,1.66E-03,1.64E-03,1.64E-03,1.63E-03,
     2 1.63E-03,1.62E-03,1.62E-03,1.61E-03,1.62E-03,1.63E-03,
     3 1.63E-03,1.62E-03,1.65E-03,1.68E-03,1.67E-03,1.54E-03,
     4 1.56E-03,1.65E-03,1.64E-03,1.61E-03,1.53E-03,1.49E-03,
     1 2.88E-03,2.87E-03,2.86E-03,2.83E-03,2.82E-03,2.80E-03,
     2 2.78E-03,2.76E-03,2.74E-03,2.76E-03,2.75E-03,2.74E-03,
     3 2.74E-03,2.76E-03,2.75E-03,2.78E-03,2.83E-03,2.79E-03,
     4 2.69E-03,2.83E-03,2.89E-03,2.88E-03,2.81E-03,2.73E-03,
     1 4.24E-03,4.27E-03,4.26E-03,4.26E-03,4.24E-03,4.21E-03,
     2 4.18E-03,4.16E-03,4.14E-03,4.13E-03,4.11E-03,4.09E-03,
     3 4.09E-03,4.06E-03,4.09E-03,4.08E-03,4.10E-03,4.22E-03,
     4 4.02E-03,4.16E-03,4.26E-03,4.30E-03,4.31E-03,4.25E-03,
!  2
     1 1.76E-05,1.57E-05,1.40E-05,1.25E-05,1.11E-05,9.95E-06,
     2 8.91E-06,8.01E-06,7.21E-06,6.53E-06,5.78E-06,4.79E-06,
     3 4.05E-06,2.66E-06,1.46E-06,1.02E-06,1.33E-06,4.35E-07,
     4 3.49E-07,2.36E-07,2.02E-07,1.90E-07,1.54E-07,1.39E-07,
     1 1.76E-05,1.57E-05,1.40E-05,1.25E-05,1.11E-05,9.95E-06,
     2 8.91E-06,8.01E-06,7.21E-06,6.53E-06,5.79E-06,4.80E-06,
     3 4.05E-06,2.66E-06,1.46E-06,1.02E-06,1.33E-06,4.37E-07,
     4 3.50E-07,2.38E-07,2.03E-07,1.91E-07,1.55E-07,1.40E-07,
     1 1.89E-05,1.69E-05,1.50E-05,1.34E-05,1.19E-05,1.07E-05,
     2 9.57E-06,8.61E-06,7.75E-06,7.02E-06,6.23E-06,5.17E-06,
     3 4.36E-06,2.87E-06,1.59E-06,1.12E-06,1.40E-06,4.73E-07,
     4 5.67E-07,3.34E-07,2.44E-07,2.23E-07,1.77E-07,1.59E-07,
     1 2.54E-05,2.27E-05,2.03E-05,1.81E-05,1.62E-05,1.45E-05,
     2 1.31E-05,1.18E-05,1.06E-05,9.64E-06,8.61E-06,7.17E-06,
     3 6.06E-06,4.01E-06,2.35E-06,1.65E-06,1.81E-06,6.73E-07,
     4 1.72E-06,8.68E-07,4.78E-07,4.04E-07,3.02E-07,2.66E-07,
     1 3.71E-05,3.35E-05,3.01E-05,2.71E-05,2.44E-05,2.20E-05,
     2 1.99E-05,1.80E-05,1.64E-05,1.49E-05,1.35E-05,1.13E-05,
     3 9.58E-06,6.42E-06,4.02E-06,2.84E-06,2.68E-06,1.11E-06,
     4 4.01E-06,2.02E-06,1.01E-06,8.25E-07,5.90E-07,5.07E-07,
     1 4.64E-05,4.22E-05,3.82E-05,3.46E-05,3.13E-05,2.83E-05,
     2 2.57E-05,2.34E-05,2.14E-05,1.95E-05,1.77E-05,1.49E-05,
     3 1.27E-05,8.61E-06,5.57E-06,3.96E-06,3.51E-06,1.52E-06,
     4 5.97E-06,3.08E-06,1.54E-06,1.24E-06,8.75E-07,7.43E-07,
     1 5.89E-05,5.40E-05,4.93E-05,4.50E-05,4.10E-05,3.73E-05,
     2 3.41E-05,3.12E-05,2.86E-05,2.62E-05,2.39E-05,2.03E-05,
     3 1.74E-05,1.20E-05,7.99E-06,5.74E-06,4.85E-06,2.21E-06,
     4 8.84E-06,4.73E-06,2.40E-06,1.94E-06,1.38E-06,1.16E-06,
     1 7.31E-05,6.77E-05,6.22E-05,5.72E-05,5.24E-05,4.80E-05,
     2 4.41E-05,4.05E-05,3.73E-05,3.44E-05,3.15E-05,2.70E-05,
     3 2.32E-05,1.62E-05,1.11E-05,8.06E-06,6.61E-06,3.11E-06,
     4 1.23E-05,6.85E-06,3.56E-06,2.88E-06,2.05E-06,1.72E-06,
!  3
     1 1.16E-05,1.03E-05,9.18E-06,8.17E-06,7.28E-06,6.49E-06,
     2 5.81E-06,5.22E-06,4.70E-06,4.25E-06,3.77E-06,3.13E-06,
     3 2.64E-06,1.74E-06,9.67E-07,6.83E-07,8.71E-07,3.00E-07,
     4 2.42E-07,1.67E-07,1.44E-07,1.35E-07,1.11E-07,1.00E-07,
     1 1.16E-05,1.03E-05,9.20E-06,8.18E-06,7.28E-06,6.50E-06,
     2 5.82E-06,5.23E-06,4.70E-06,4.26E-06,3.78E-06,3.13E-06,
     3 2.64E-06,1.74E-06,9.69E-07,6.85E-07,8.72E-07,3.02E-07,
     4 2.44E-07,1.68E-07,1.45E-07,1.36E-07,1.12E-07,1.01E-07,
     1 1.25E-05,1.11E-05,9.88E-06,8.79E-06,7.83E-06,6.98E-06,
     2 6.25E-06,5.62E-06,5.06E-06,4.58E-06,4.07E-06,3.38E-06,
     3 2.85E-06,1.88E-06,1.06E-06,7.49E-07,9.23E-07,3.28E-07,
     4 3.90E-07,2.32E-07,1.72E-07,1.58E-07,1.28E-07,1.15E-07,
     1 1.68E-05,1.49E-05,1.33E-05,1.19E-05,1.06E-05,9.48E-06,
     2 8.51E-06,7.66E-06,6.92E-06,6.27E-06,5.61E-06,4.67E-06,
     3 3.94E-06,2.61E-06,1.55E-06,1.09E-06,1.18E-06,4.55E-07,
     4 1.14E-06,5.75E-07,3.19E-07,2.71E-07,2.05E-07,1.81E-07,
     1 2.45E-05,2.20E-05,1.97E-05,1.77E-05,1.59E-05,1.43E-05,
     2 1.29E-05,1.17E-05,1.06E-05,9.67E-06,8.71E-06,7.30E-06,
     3 6.19E-06,4.15E-06,2.60E-06,1.85E-06,1.73E-06,7.27E-07,
     4 2.63E-06,1.31E-06,6.55E-07,5.33E-07,3.82E-07,3.28E-07,
     1 3.08E-05,2.78E-05,2.51E-05,2.26E-05,2.05E-05,1.85E-05,
     2 1.67E-05,1.52E-05,1.39E-05,1.26E-05,1.15E-05,9.65E-06,
     3 8.21E-06,5.55E-06,3.60E-06,2.56E-06,2.26E-06,9.87E-07,
     4 3.93E-06,2.00E-06,9.86E-07,7.92E-07,5.59E-07,4.75E-07,
     1 3.98E-05,3.62E-05,3.28E-05,2.98E-05,2.70E-05,2.45E-05,
     2 2.23E-05,2.04E-05,1.86E-05,1.71E-05,1.55E-05,1.32E-05,
     3 1.12E-05,7.70E-06,5.14E-06,3.68E-06,3.10E-06,1.40E-06,
     4 5.86E-06,3.06E-06,1.52E-06,1.22E-06,8.51E-07,7.16E-07,
     1 4.99E-05,4.58E-05,4.18E-05,3.81E-05,3.48E-05,3.17E-05,
     2 2.90E-05,2.66E-05,2.44E-05,2.24E-05,2.06E-05,1.75E-05,
     3 1.51E-05,1.04E-05,7.15E-06,5.15E-06,4.20E-06,1.95E-06,
     4 8.20E-06,4.44E-06,2.24E-06,1.79E-06,1.25E-06,1.05E-06/

	end
!------------------------------------------------------------
	block data aerosol_convolve5

	common /aot_spect_5/  wlo(5,18) , hkas(5,18) ,sflx(5,18)
	data wlo /
     &   0.1794,  0.1878,  0.1970,  0.2073,  0.2186,
     &   0.2265,  0.2301,  0.2339,  0.2378,  0.2418,
     &   0.2475,  0.2551,  0.2632,  0.2717,  0.2809,
     &   0.2869,  0.2894,  0.2920,  0.2946,  0.2972,
     &   0.3008,  0.3053,  0.3100,  0.3149,  0.3199,
     &   0.3257,  0.3323,  0.3391,  0.3462,  0.3537,
     &   0.3642,  0.3783,  0.3935,  0.4100,  0.4279,
     &   0.4429,  0.4539,  0.4656,  0.4778,  0.4908,
     &   0.5059,  0.5233,  0.5419,  0.5620,  0.5836,
     &   0.6033,  0.6206,  0.6389,  0.6583,  0.6789,
     &   0.6988,  0.7179,  0.7380,  0.7593,  0.7819,
     &   0.8022,  0.8200,  0.8386,  0.8580,  0.8783,
     &   0.9021,  0.9298,  0.9592,  0.9906,  1.0241,
     &   1.0627,  1.1074,  1.1561,  1.2092,  1.2674,
     &   1.3414,  1.4358,  1.5444,  1.6708,  1.8198,
     &   1.9512,  2.0513,  2.1622,  2.2857,  2.4242,
     &   2.5740,  2.7360,  2.9197,  3.1299,  3.3727,
     &   3.5524,  3.6430,  3.7383,  3.8388,  3.9448
     & /
	data hkas /
     & 1.9732E-02,8.2461E-03,1.9433E-02,2.5239E-01,7.0020E-01,
     & 1.7698E-01,1.9552E-01,1.8982E-01,2.0086E-01,2.3681E-01,
     & 6.1144E-02,9.1518E-02,2.0913E-01,3.0821E-01,3.3000E-01,
     & 1.1853E-01,1.6684E-01,2.5436E-01,2.2410E-01,2.3617E-01,
     & 1.3174E-01,1.8637E-01,2.1818E-01,2.1883E-01,2.4487E-01,
     & 1.6818E-01,1.9811E-01,1.7183E-01,2.1417E-01,2.4771E-01,
     & 1.3629E-01,1.4266E-01,1.6518E-01,2.7083E-01,2.8504E-01,
     & 1.6885E-01,1.9578E-01,2.0319E-01,2.1598E-01,2.1620E-01,
     & 1.7878E-01,1.8362E-01,1.9993E-01,2.1178E-01,2.2589E-01,
     & 1.9345E-01,1.9568E-01,2.0132E-01,2.0166E-01,2.0789E-01,
     & 1.9618E-01,1.9794E-01,1.9955E-01,2.0186E-01,2.0447E-01,
     & 1.9921E-01,2.0086E-01,2.0087E-01,1.9544E-01,2.0363E-01,
     & 2.0092E-01,2.0061E-01,2.0082E-01,1.9965E-01,1.9800E-01,
     & 2.0090E-01,2.0065E-01,2.0056E-01,2.0019E-01,1.9770E-01,
     & 2.1754E-01,2.1221E-01,2.0669E-01,1.9305E-01,1.7051E-01,
     & 2.3672E-01,2.1723E-01,1.9925E-01,1.8215E-01,1.6466E-01,
     & 2.4692E-01,2.2312E-01,1.9962E-01,1.7636E-01,1.5398E-01,
     & 2.2023E-01,2.0999E-01,1.9844E-01,1.9036E-01,1.8098E-01
     & /
	data sflx /
     & 1.4056E-02,5.8741E-03,1.3843E-02,1.7979E-01,4.9878E-01,
     & 1.5997E-01,1.7673E-01,1.7157E-01,1.8156E-01,2.1404E-01,
     & 4.0480E-01,6.0590E-01,1.3845E+00,2.0405E+00,2.1848E+00,
     & 7.6075E-01,1.0708E+00,1.6325E+00,1.4383E+00,1.5157E+00,
     & 2.1362E+00,3.0220E+00,3.5377E+00,3.5483E+00,3.9706E+00,
     & 5.6607E+00,6.6683E+00,5.7836E+00,7.2089E+00,8.3378E+00,
     & 1.4768E+01,1.5458E+01,1.7899E+01,2.9348E+01,3.0887E+01,
     & 1.9903E+01,2.3078E+01,2.3950E+01,2.5459E+01,2.5484E+01,
     & 3.2273E+01,3.3147E+01,3.6093E+01,3.8232E+01,4.0778E+01,
     & 2.9661E+01,3.0003E+01,3.0869E+01,3.0920E+01,3.1876E+01,
     & 2.6460E+01,2.6698E+01,2.6914E+01,2.7226E+01,2.7578E+01,
     & 1.9702E+01,1.9865E+01,1.9866E+01,1.9329E+01,2.0139E+01,
     & 2.4798E+01,2.4759E+01,2.4785E+01,2.4641E+01,2.4437E+01,
     & 2.7695E+01,2.7661E+01,2.7648E+01,2.7597E+01,2.7254E+01,
     & 3.3902E+01,3.3072E+01,3.2211E+01,3.0086E+01,2.6573E+01,
     & 1.2208E+01,1.1203E+01,1.0275E+01,9.3939E+00,8.4915E+00,
     & 7.0905E+00,6.4071E+00,5.7325E+00,5.0645E+00,4.4219E+00,
     & 1.2380E+00,1.1805E+00,1.1155E+00,1.0701E+00,1.0173E+00
     & /


	end
       block data aerosol2
c                              4/1/97
c  ********************************************************************
c
c  Data statements providing aerosol properties for the 10 
c  subintervals in the first Fu-Liou SW band.
c
c  mb:     Number of bands in code (will always be 10)
c  naer:   Number of aerosol types (will need to be changed here AND in
c          aerosol subroutine.
c  nrh:    Number of different relative humidities (currently 8)
c
c  Optical properties are dimensioned (10,8,naer): Number of 
c  sw sunintervals, number of relative humidities, and number of 
c  aerosol types. Properties were extracted from tables and mapped for 
c  the most part into the Fu-Liou spectral bands.  sub-intervals 1-4, 
c  not available in the tables, were filled with properties from the 
c  5th sub-interval.  Intervals 5-6 were filled by direct insertion 
c  (1 table value per interval). The last two intervals were filled 
c  with 2 table values per interval, which were averaged using 
c  energy weighting.  Tegen and Lacis values are not RH-dependent, 
c  so values are repeated.
c
c  a_ssa:  single-scattering albedo.  One data statement for EACH type
c          of aerosol.
c
c  a_ext:  extinction coefficient.  Normalization is not important.
c          These values are used for spectral weighting only!!  One
c          data statement for EACH type of aerosol.
c
c  a_asy:  Asymmetry parameter.One data statement for EACH type of
c          aerosol.
c
c  ********************************************************************
      USE FUINPUT
      implicit none
c##      include 'rad_0698.h'
      integer i,j
      real a_ssay(mby,nrh,naer),a_exty(mby,nrh,naer)
      real a_asyy(mby,nrh,naer)
      common /aer_opty/ a_ssay,a_exty,a_asyy

c  ****************************************************     
c  Data statements for aerosol type 1 (marine) sw bnd 1     
c  ****************************************************     
      data ((a_ssay(i,j,1),i=1,mby),j=1,nrh) /
     1 .1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,
     1 .1000E+01,.1000E+01,.1000E+01,.1000E+01,
     2 .1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,
     2 .1000E+01,.1000E+01,.1000E+01,.1000E+01,
     3 .1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,
     3 .1000E+01,.1000E+01,.1000E+01,.1000E+01,
     4 .9999E+00,.9999E+00,.9999E+00,.9999E+00,.9999E+00,.1000E+01,
     4 .1000E+01,.1000E+01,.1000E+01,.1000E+01,
     5 .1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,
     5 .1000E+01,.1000E+01,.1000E+01,.1000E+01,
     6 .9993E+00,.9993E+00,.9993E+00,.9993E+00,.9993E+00,.1000E+01,
     6 .1000E+01,.1000E+01,.1000E+01,.1000E+01,
     7 .1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,
     7 .1000E+01,.1000E+01,.1000E+01,.1000E+01,
     8 .1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,.1000E+01,
     8 .1000E+01,.1000E+01,.1000E+01,.1000E+01/
      data ((a_exty(i,j,1),i=1,mby),j=1,nrh) /
     1 .2071E-03,.2071E-03,.2071E-03,.2071E-03,.2071E-03,.2084E-03,
     1 .2081E-03,.2065E-03,.2071E-03,.2101E-03,
     2 .2448E-03,.2448E-03,.2448E-03,.2448E-03,.2448E-03,.2459E-03,
     2 .2452E-03,.2437E-03,.2427E-03,.2447E-03,
     3 .3519E-03,.3519E-03,.3519E-03,.3519E-03,.3519E-03,.3499E-03,
     3 .3503E-03,.3510E-03,.3486E-03,.3468E-03,
     4 .7975E-03,.7975E-03,.7975E-03,.7975E-03,.7975E-03,.7928E-03,
     4 .7874E-03,.7863E-03,.7813E-03,.7843E-03,
     5 .1135E-02,.1135E-02,.1135E-02,.1135E-02,.1135E-02,.1122E-02,
     5 .1120E-02,.1113E-02,.1113E-02,.1106E-02,
     6 .1685E-02,.1685E-02,.1685E-02,.1685E-02,.1685E-02,.1671E-02,
     6 .1656E-02,.1644E-02,.1632E-02,.1626E-02,
     7 .2879E-02,.2879E-02,.2879E-02,.2879E-02,.2879E-02,.2872E-02,
     7 .2855E-02,.2832E-02,.2806E-02,.2770E-02,
     8 .4241E-02,.4241E-02,.4241E-02,.4241E-02,.4241E-02,.4274E-02,
     8 .4256E-02,.4255E-02,.4223E-02,.4171E-02/
      data ((a_asyy(i,j,1),i=1,mby),j=1,nrh) /
     1 .7513E+00,.7513E+00,.7513E+00,.7513E+00,.7513E+00,.7721E+00,
     1 .7842E+00,.7893E+00,.7963E+00,.8072E+00,
     2 .7568E+00,.7568E+00,.7568E+00,.7568E+00,.7568E+00,.7792E+00,
     2 .7907E+00,.7940E+00,.8002E+00,.8113E+00,
     3 .7412E+00,.7412E+00,.7412E+00,.7412E+00,.7412E+00,.7662E+00,
     3 .7783E+00,.7912E+00,.8007E+00,.8096E+00,
     4 .6857E+00,.6857E+00,.6857E+00,.6857E+00,.6857E+00,.7078E+00,
     4 .7249E+00,.7462E+00,.7600E+00,.7868E+00,
     5 .6639E+00,.6639E+00,.6639E+00,.6639E+00,.6639E+00,.6845E+00,
     5 .7070E+00,.7252E+00,.7393E+00,.7655E+00,
     6 .6515E+00,.6515E+00,.6515E+00,.6515E+00,.6515E+00,.6620E+00,
     6 .6810E+00,.6925E+00,.7165E+00,.7380E+00,
     7 .6220E+00,.6220E+00,.6220E+00,.6220E+00,.6220E+00,.6424E+00,
     7 .6525E+00,.6656E+00,.6848E+00,.7081E+00,
     8 .6129E+00,.6129E+00,.6129E+00,.6129E+00,.6129E+00,.6290E+00,
     8 .6397E+00,.6509E+00,.6676E+00,.6865E+00/

c  *********************************************************
c  Data statements for aerosol type 2 (continental) sw bnd 1
c  *********************************************************
      data ((a_ssay(i,j,2),i=1,mby),j=1,nrh) /
     1 .9419E+00,.9419E+00,.9419E+00,.9419E+00,.9419E+00,.9634E+00,
     1 .9640E+00,.9652E+00,.9628E+00,.9566E+00,
     2 .9418E+00,.9418E+00,.9418E+00,.9418E+00,.9418E+00,.9633E+00,
     2 .9640E+00,.9652E+00,.9627E+00,.9565E+00,
     3 .9460E+00,.9460E+00,.9460E+00,.9460E+00,.9460E+00,.9650E+00,
     3 .9667E+00,.9673E+00,.9650E+00,.9595E+00,
     4 .9596E+00,.9596E+00,.9596E+00,.9596E+00,.9596E+00,.9744E+00,
     4 .9754E+00,.9760E+00,.9742E+00,.9703E+00,
     5 .9722E+00,.9722E+00,.9722E+00,.9722E+00,.9722E+00,.9827E+00,
     5 .9833E+00,.9838E+00,.9828E+00,.9805E+00,
     6 .9776E+00,.9776E+00,.9776E+00,.9776E+00,.9776E+00,.9861E+00,
     6 .9869E+00,.9872E+00,.9865E+00,.9846E+00,
     7 .9823E+00,.9823E+00,.9823E+00,.9823E+00,.9823E+00,.9892E+00,
     7 .9895E+00,.9900E+00,.9896E+00,.9883E+00,
     8 .9857E+00,.9857E+00,.9857E+00,.9857E+00,.9857E+00,.9912E+00,
     8 .9917E+00,.9921E+00,.9919E+00,.9907E+00/
      data ((a_exty(i,j,2),i=1,mby),j=1,nrh) /
     1 .1763E-04,.1763E-04,.1763E-04,.1763E-04,.1763E-04,.1574E-04,
     1 .1402E-04,.1248E-04,.1055E-04,.8482E-05,
     2 .1763E-04,.1763E-04,.1763E-04,.1763E-04,.1763E-04,.1574E-04,
     2 .1402E-04,.1249E-04,.1055E-04,.8483E-05,
     3 .1890E-04,.1890E-04,.1890E-04,.1890E-04,.1890E-04,.1689E-04,
     3 .1504E-04,.1339E-04,.1132E-04,.9110E-05,
     4 .2535E-04,.2535E-04,.2535E-04,.2535E-04,.2535E-04,.2270E-04,
     4 .2027E-04,.1811E-04,.1538E-04,.1244E-04,
     5 .3707E-04,.3707E-04,.3707E-04,.3707E-04,.3707E-04,.3347E-04,
     5 .3014E-04,.2714E-04,.2326E-04,.1903E-04,
     6 .4636E-04,.4636E-04,.4636E-04,.4636E-04,.4636E-04,.4215E-04,
     6 .3817E-04,.3459E-04,.2986E-04,.2465E-04,
     7 .5890E-04,.5890E-04,.5890E-04,.5890E-04,.5890E-04,.5402E-04,
     7 .4933E-04,.4501E-04,.3919E-04,.3269E-04,
     8 .7312E-04,.7312E-04,.7312E-04,.7312E-04,.7312E-04,.6769E-04,
     8 .6224E-04,.5721E-04,.5027E-04,.4240E-04/
      data ((a_asyy(i,j,2),i=1,mby),j=1,nrh) /
     1 .6740E+00,.6740E+00,.6740E+00,.6740E+00,.6740E+00,.6635E+00,
     1 .6570E+00,.6507E+00,.6414E+00,.6293E+00,
     2 .6740E+00,.6740E+00,.6740E+00,.6740E+00,.6740E+00,.6635E+00,
     2 .6570E+00,.6507E+00,.6414E+00,.6293E+00,
     3 .6809E+00,.6809E+00,.6809E+00,.6809E+00,.6809E+00,.6740E+00,
     3 .6678E+00,.6616E+00,.6523E+00,.6403E+00,
     4 .7167E+00,.7167E+00,.7167E+00,.7167E+00,.7167E+00,.7097E+00,
     4 .7046E+00,.6988E+00,.6904E+00,.6785E+00,
     5 .7447E+00,.7447E+00,.7447E+00,.7447E+00,.7447E+00,.7407E+00,
     5 .7371E+00,.7325E+00,.7251E+00,.7146E+00,
     6 .7561E+00,.7561E+00,.7561E+00,.7561E+00,.7561E+00,.7534E+00,
     6 .7508E+00,.7468E+00,.7404E+00,.7308E+00,
     7 .7656E+00,.7656E+00,.7656E+00,.7656E+00,.7656E+00,.7643E+00,
     7 .7622E+00,.7589E+00,.7536E+00,.7451E+00,
     8 .7723E+00,.7723E+00,.7723E+00,.7723E+00,.7723E+00,.7715E+00,
     8 .7706E+00,.7678E+00,.7635E+00,.7559E+00/

c  ***************************************************      
c  Data statements for aerosol type 3 (urban) sw bnd 1      
c  ***************************************************      
      data ((a_ssay(i,j,3),i=1,mby),j=1,nrh) /
     1 .9180E+00,.9180E+00,.9180E+00,.9180E+00,.9180E+00,.9394E+00,
     1 .9404E+00,.9417E+00,.9391E+00,.9333E+00,
     2 .9174E+00,.9174E+00,.9174E+00,.9174E+00,.9174E+00,.9388E+00,
     2 .9397E+00,.9411E+00,.9384E+00,.9327E+00,
     3 .9210E+00,.9210E+00,.9210E+00,.9210E+00,.9210E+00,.9400E+00,
     3 .9421E+00,.9428E+00,.9403E+00,.9353E+00,
     4 .9377E+00,.9377E+00,.9377E+00,.9377E+00,.9377E+00,.9527E+00,
     4 .9543E+00,.9551E+00,.9533E+00,.9500E+00,
     5 .9553E+00,.9553E+00,.9553E+00,.9553E+00,.9553E+00,.9663E+00,
     5 .9675E+00,.9685E+00,.9676E+00,.9659E+00,
     6 .9630E+00,.9630E+00,.9630E+00,.9630E+00,.9630E+00,.9722E+00,
     6 .9736E+00,.9743E+00,.9739E+00,.9728E+00,
     7 .9702E+00,.9702E+00,.9702E+00,.9702E+00,.9702E+00,.9776E+00,
     7 .9786E+00,.9795E+00,.9795E+00,.9788E+00,
     8 .9756E+00,.9756E+00,.9756E+00,.9756E+00,.9756E+00,.9816E+00,
     8 .9827E+00,.9836E+00,.9837E+00,.9832E+00/
      data ((a_exty(i,j,3),i=1,mby),j=1,nrh) /
     1 .1160E-04,.1160E-04,.1160E-04,.1160E-04,.1160E-04,.1033E-04,
     1 .9185E-05,.8166E-05,.6890E-05,.5530E-05,
     2 .1161E-04,.1161E-04,.1161E-04,.1161E-04,.1161E-04,.1034E-04,
     2 .9196E-05,.8175E-05,.6897E-05,.5536E-05,
     3 .1248E-04,.1248E-04,.1248E-04,.1248E-04,.1248E-04,.1112E-04,
     3 .9879E-05,.8785E-05,.7413E-05,.5952E-05,
     4 .1675E-04,.1675E-04,.1675E-04,.1675E-04,.1675E-04,.1494E-04,
     4 .1331E-04,.1187E-04,.1005E-04,.8106E-05,
     5 .2446E-04,.2446E-04,.2446E-04,.2446E-04,.2446E-04,.2199E-04,
     5 .1972E-04,.1772E-04,.1514E-04,.1235E-04,
     6 .3079E-04,.3079E-04,.3079E-04,.3079E-04,.3079E-04,.2783E-04,
     6 .2509E-04,.2265E-04,.1948E-04,.1601E-04,
     7 .3977E-04,.3977E-04,.3977E-04,.3977E-04,.3977E-04,.3616E-04,
     7 .3281E-04,.2978E-04,.2579E-04,.2139E-04,
     8 .4994E-04,.4994E-04,.4994E-04,.4994E-04,.4994E-04,.4577E-04,
     8 .4176E-04,.3815E-04,.3329E-04,.2788E-04/
      data ((a_asyy(i,j,3),i=1,mby),j=1,nrh) /
     1 .6710E+00,.6710E+00,.6710E+00,.6710E+00,.6710E+00,.6606E+00,
     1 .6543E+00,.6481E+00,.6390E+00,.6271E+00,
     2 .6711E+00,.6711E+00,.6711E+00,.6711E+00,.6711E+00,.6607E+00,
     2 .6543E+00,.6481E+00,.6389E+00,.6270E+00,
     3 .6811E+00,.6811E+00,.6811E+00,.6811E+00,.6811E+00,.6713E+00,
     3 .6652E+00,.6590E+00,.6498E+00,.6379E+00,
     4 .7143E+00,.7143E+00,.7143E+00,.7143E+00,.7143E+00,.7072E+00,
     4 .7020E+00,.6962E+00,.6878E+00,.6760E+00,
     5 .7425E+00,.7425E+00,.7425E+00,.7425E+00,.7425E+00,.7383E+00,
     5 .7346E+00,.7299E+00,.7225E+00,.7121E+00,
     6 .7541E+00,.7541E+00,.7541E+00,.7541E+00,.7541E+00,.7510E+00,
     6 .7482E+00,.7440E+00,.7375E+00,.7279E+00,
     7 .7637E+00,.7637E+00,.7637E+00,.7637E+00,.7637E+00,.7618E+00,
     7 .7593E+00,.7557E+00,.7501E+00,.7414E+00,
     8 .7707E+00,.7707E+00,.7707E+00,.7707E+00,.7707E+00,.7691E+00,
     8 .7677E+00,.7645E+00,.7598E+00,.7519E+00/

c  ********************************************************
c  Data statements for T&L 0.5 micron dust aerosol sw bnd 1
c  ********************************************************
      data ((a_ssay(i,j,4),i=1,mby),j=1,nrh) /
     1 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     1 .8284E+00,.8779E+00,.9276E+00,.9653E+00,
     2 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     2 .8284E+00,.8779E+00,.9276E+00,.9653E+00,
     3 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     3 .8284E+00,.8779E+00,.9276E+00,.9653E+00,
     4 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     4 .8284E+00,.8779E+00,.9276E+00,.9653E+00,
     5 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     5 .8284E+00,.8779E+00,.9276E+00,.9653E+00,
     6 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     6 .8284E+00,.8779E+00,.9276E+00,.9653E+00,
     7 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     7 .8284E+00,.8779E+00,.9276E+00,.9653E+00,
     8 .7035E+00,.7035E+00,.7035E+00,.7035E+00,.7035E+00,.7798E+00,
     8 .8284E+00,.8779E+00,.9276E+00,.9653E+00/
      data ((a_exty(i,j,4),i=1,mby),j=1,nrh) /
     1 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     1 .9356E+00,.9674E+00,.1015E+01,.1067E+01,
     2 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     2 .9356E+00,.9674E+00,.1015E+01,.1067E+01,
     3 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     3 .9356E+00,.9674E+00,.1015E+01,.1067E+01,
     4 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     4 .9356E+00,.9674E+00,.1015E+01,.1067E+01,
     5 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     5 .9356E+00,.9674E+00,.1015E+01,.1067E+01,
     6 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     6 .9356E+00,.9674E+00,.1015E+01,.1067E+01,
     7 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     7 .9356E+00,.9674E+00,.1015E+01,.1067E+01,
     8 .8783E+00,.8783E+00,.8783E+00,.8783E+00,.8783E+00,.9056E+00,
     8 .9356E+00,.9674E+00,.1015E+01,.1067E+01/
      data ((a_asyy(i,j,4),i=1,mby),j=1,nrh) /
     1 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     1 .6963E+00,.6754E+00,.6626E+00,.6622E+00,
     2 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     2 .6963E+00,.6754E+00,.6626E+00,.6622E+00,
     3 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     3 .6963E+00,.6754E+00,.6626E+00,.6622E+00,
     4 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     4 .6963E+00,.6754E+00,.6626E+00,.6622E+00,
     5 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     5 .6963E+00,.6754E+00,.6626E+00,.6622E+00,
     6 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     6 .6963E+00,.6754E+00,.6626E+00,.6622E+00,
     7 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     7 .6963E+00,.6754E+00,.6626E+00,.6622E+00,
     8 .7678E+00,.7678E+00,.7678E+00,.7678E+00,.7678E+00,.7230E+00,
     8 .6963E+00,.6754E+00,.6626E+00,.6622E+00/
c  ********************************************************
c  Data statements for T&L 1.0 micron dust aerosol sw bnd 1
c  ********************************************************
      data ((a_ssay(i,j,5),i=1,mby),j=1,nrh) /
     1 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     1 .7317E+00,.7920E+00,.8629E+00,.9255E+00,
     2 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     2 .7317E+00,.7920E+00,.8629E+00,.9255E+00,
     3 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     3 .7317E+00,.7920E+00,.8629E+00,.9255E+00,
     4 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     4 .7317E+00,.7920E+00,.8629E+00,.9255E+00,
     5 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     5 .7317E+00,.7920E+00,.8629E+00,.9255E+00,
     6 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     6 .7317E+00,.7920E+00,.8629E+00,.9255E+00,
     7 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     7 .7317E+00,.7920E+00,.8629E+00,.9255E+00,
     8 .6142E+00,.6142E+00,.6142E+00,.6142E+00,.6142E+00,.6812E+00,
     8 .7317E+00,.7920E+00,.8629E+00,.9255E+00/
      data ((a_exty(i,j,5),i=1,mby),j=1,nrh) /
     1 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     1 .9700E+00,.9848E+00,.1008E+01,.1040E+01,
     2 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     2 .9700E+00,.9848E+00,.1008E+01,.1040E+01,
     3 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     3 .9700E+00,.9848E+00,.1008E+01,.1040E+01,
     4 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     4 .9700E+00,.9848E+00,.1008E+01,.1040E+01,
     5 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     5 .9700E+00,.9848E+00,.1008E+01,.1040E+01,
     6 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     6 .9700E+00,.9848E+00,.1008E+01,.1040E+01,
     7 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     7 .9700E+00,.9848E+00,.1008E+01,.1040E+01,
     8 .9410E+00,.9410E+00,.9410E+00,.9410E+00,.9410E+00,.9556E+00,
     8 .9700E+00,.9848E+00,.1008E+01,.1040E+01/
      data ((a_asyy(i,j,5),i=1,mby),j=1,nrh) /
     1 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     1 .7970E+00,.7654E+00,.7285E+00,.6931E+00,
     2 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     2 .7970E+00,.7654E+00,.7285E+00,.6931E+00,
     3 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     3 .7970E+00,.7654E+00,.7285E+00,.6931E+00,
     4 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     4 .7970E+00,.7654E+00,.7285E+00,.6931E+00,
     5 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     5 .7970E+00,.7654E+00,.7285E+00,.6931E+00,
     6 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     6 .7970E+00,.7654E+00,.7285E+00,.6931E+00,
     7 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     7 .7970E+00,.7654E+00,.7285E+00,.6931E+00,
     8 .8661E+00,.8661E+00,.8661E+00,.8661E+00,.8661E+00,.8265E+00,
     8 .7970E+00,.7654E+00,.7285E+00,.6931E+00/
c  ********************************************************
c  Data statements for T&L 2.0 micron dust aerosol sw bnd 1
c  ********************************************************
      data ((a_ssay(i,j,6),i=1,mby),j=1,nrh) /
     1 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     1 .6403E+00,.6988E+00,.7839E+00,.8715E+00,
     2 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     2 .6403E+00,.6988E+00,.7839E+00,.8715E+00,
     3 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     3 .6403E+00,.6988E+00,.7839E+00,.8715E+00,
     4 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     4 .6403E+00,.6988E+00,.7839E+00,.8715E+00,
     5 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     5 .6403E+00,.6988E+00,.7839E+00,.8715E+00,
     6 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     6 .6403E+00,.6988E+00,.7839E+00,.8715E+00,
     7 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     7 .6403E+00,.6988E+00,.7839E+00,.8715E+00,
     8 .5631E+00,.5631E+00,.5631E+00,.5631E+00,.5631E+00,.6011E+00,
     8 .6403E+00,.6988E+00,.7839E+00,.8715E+00/
      data ((a_exty(i,j,6),i=1,mby),j=1,nrh) /
     1 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     1 .9831E+00,.9916E+00,.1004E+01,.1019E+01,
     2 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     2 .9831E+00,.9916E+00,.1004E+01,.1019E+01,
     3 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     3 .9831E+00,.9916E+00,.1004E+01,.1019E+01,
     4 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     4 .9831E+00,.9916E+00,.1004E+01,.1019E+01,
     5 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     5 .9831E+00,.9916E+00,.1004E+01,.1019E+01,
     6 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     6 .9831E+00,.9916E+00,.1004E+01,.1019E+01,
     7 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     7 .9831E+00,.9916E+00,.1004E+01,.1019E+01,
     8 .9650E+00,.9650E+00,.9650E+00,.9650E+00,.9650E+00,.9749E+00,
     8 .9831E+00,.9916E+00,.1004E+01,.1019E+01/
      data ((a_asyy(i,j,6),i=1,mby),j=1,nrh) /
     1 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     1 .8745E+00,.8466E+00,.8097E+00,.7725E+00,
     2 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     2 .8745E+00,.8466E+00,.8097E+00,.7725E+00,
     3 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     3 .8745E+00,.8466E+00,.8097E+00,.7725E+00,
     4 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     4 .8745E+00,.8466E+00,.8097E+00,.7725E+00,
     5 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     5 .8745E+00,.8466E+00,.8097E+00,.7725E+00,
     6 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     6 .8745E+00,.8466E+00,.8097E+00,.7725E+00,
     7 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     7 .8745E+00,.8466E+00,.8097E+00,.7725E+00,
     8 .9183E+00,.9183E+00,.9183E+00,.9183E+00,.9183E+00,.8957E+00,
     8 .8745E+00,.8466E+00,.8097E+00,.7725E+00/
c  ********************************************************
c  Data statements for T&L 4.0 micron dust aerosol sw bnd 1
c  ********************************************************
      data ((a_ssay(i,j,7),i=1,mby),j=1,nrh) /
     1 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     1 .5775E+00,.6141E+00,.6914E+00,.7949E+00,
     2 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     2 .5775E+00,.6141E+00,.6914E+00,.7949E+00,
     3 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     3 .5775E+00,.6141E+00,.6914E+00,.7949E+00,
     4 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     4 .5775E+00,.6141E+00,.6914E+00,.7949E+00,
     5 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     5 .5775E+00,.6141E+00,.6914E+00,.7949E+00,
     6 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     6 .5775E+00,.6141E+00,.6914E+00,.7949E+00,
     7 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     7 .5775E+00,.6141E+00,.6914E+00,.7949E+00,
     8 .5495E+00,.5495E+00,.5495E+00,.5495E+00,.5495E+00,.5603E+00,
     8 .5775E+00,.6141E+00,.6914E+00,.7949E+00/
      data ((a_exty(i,j,7),i=1,mby),j=1,nrh) /
     1 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     1 .9894E+00,.9948E+00,.1002E+01,.1012E+01,
     2 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     2 .9894E+00,.9948E+00,.1002E+01,.1012E+01,
     3 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     3 .9894E+00,.9948E+00,.1002E+01,.1012E+01,
     4 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     4 .9894E+00,.9948E+00,.1002E+01,.1012E+01,
     5 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     5 .9894E+00,.9948E+00,.1002E+01,.1012E+01,
     6 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     6 .9894E+00,.9948E+00,.1002E+01,.1012E+01,
     7 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     7 .9894E+00,.9948E+00,.1002E+01,.1012E+01,
     8 .9779E+00,.9779E+00,.9779E+00,.9779E+00,.9779E+00,.9839E+00,
     8 .9894E+00,.9948E+00,.1002E+01,.1012E+01/
      data ((a_asyy(i,j,7),i=1,mby),j=1,nrh) /
     1 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     1 .9204E+00,.9026E+00,.8702E+00,.8309E+00,
     2 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     2 .9204E+00,.9026E+00,.8702E+00,.8309E+00,
     3 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     3 .9204E+00,.9026E+00,.8702E+00,.8309E+00,
     4 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     4 .9204E+00,.9026E+00,.8702E+00,.8309E+00,
     5 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     5 .9204E+00,.9026E+00,.8702E+00,.8309E+00,
     6 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     6 .9204E+00,.9026E+00,.8702E+00,.8309E+00,
     7 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     7 .9204E+00,.9026E+00,.8702E+00,.8309E+00,
     8 .9364E+00,.9364E+00,.9364E+00,.9364E+00,.9364E+00,.9298E+00,
     8 .9204E+00,.9026E+00,.8702E+00,.8309E+00/
c  ********************************************************
c  Data statements for T&L 8.0 micron dust aerosol sw bnd 1
c  ********************************************************
      data ((a_ssay(i,j,8),i=1,mby),j=1,nrh) /
     1 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     1 .5542E+00,.5663E+00,.6106E+00,.6996E+00,
     2 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     2 .5542E+00,.5663E+00,.6106E+00,.6996E+00,
     3 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     3 .5542E+00,.5663E+00,.6106E+00,.6996E+00,
     4 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     4 .5542E+00,.5663E+00,.6106E+00,.6996E+00,
     5 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     5 .5542E+00,.5663E+00,.6106E+00,.6996E+00,
     6 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     6 .5542E+00,.5663E+00,.6106E+00,.6996E+00,
     7 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     7 .5542E+00,.5663E+00,.6106E+00,.6996E+00,
     8 .5507E+00,.5507E+00,.5507E+00,.5507E+00,.5507E+00,.5512E+00,
     8 .5542E+00,.5663E+00,.6106E+00,.6996E+00/
      data ((a_exty(i,j,8),i=1,mby),j=1,nrh) /
     1 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     1 .9932E+00,.9967E+00,.1002E+01,.1007E+01,
     2 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     2 .9932E+00,.9967E+00,.1002E+01,.1007E+01,
     3 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     3 .9932E+00,.9967E+00,.1002E+01,.1007E+01,
     4 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     4 .9932E+00,.9967E+00,.1002E+01,.1007E+01,
     5 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     5 .9932E+00,.9967E+00,.1002E+01,.1007E+01,
     6 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     6 .9932E+00,.9967E+00,.1002E+01,.1007E+01,
     7 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     7 .9932E+00,.9967E+00,.1002E+01,.1007E+01,
     8 .9859E+00,.9859E+00,.9859E+00,.9859E+00,.9859E+00,.9896E+00,
     8 .9932E+00,.9967E+00,.1002E+01,.1007E+01/
      data ((a_asyy(i,j,8),i=1,mby),j=1,nrh) /
     1 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     1 .9385E+00,.9327E+00,.9136E+00,.8795E+00,
     2 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     2 .9385E+00,.9327E+00,.9136E+00,.8795E+00,
     3 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     3 .9385E+00,.9327E+00,.9136E+00,.8795E+00,
     4 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     4 .9385E+00,.9327E+00,.9136E+00,.8795E+00,
     5 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     5 .9385E+00,.9327E+00,.9136E+00,.8795E+00,
     6 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     6 .9385E+00,.9327E+00,.9136E+00,.8795E+00,
     7 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     7 .9385E+00,.9327E+00,.9136E+00,.8795E+00,
     8 .9401E+00,.9401E+00,.9401E+00,.9401E+00,.9401E+00,.9399E+00,
     8 .9385E+00,.9327E+00,.9136E+00,.8795E+00/

!=====================================================================
!OPAC Y
!-----------------------------------------------------------
 !9)  inso	Insoluble                                         
       data ((a_exty(i,j, 9 ),i=1,mby),  j=1,1 ) /           
     &0.9429E+00,0.9453E+00,0.9513E+00,0.9558E+00,0.9595E+00,0.9650E+00,
     &0.9753E+00,0.9866E+00,0.9992E+00,0.1015E+01/
       data ((a_ssay(i,j, 9 ),i=1,mby),  j=1,1 ) /           
     &0.4624E+00,0.5098E+00,0.6053E+00,0.6511E+00,0.6674E+00,0.6750E+00,
     &0.6918E+00,0.7100E+00,0.7289E+00,0.7486E+00/
       data ((a_asyy(i,j, 9 ),i=1,mby),  j=1,1 ) /           
     &0.9788E+00,0.9585E+00,0.9122E+00,0.8872E+00,0.8773E+00,0.8720E+00,
     &0.8597E+00,0.8458E+00,0.8317E+00,0.8163E+00/
 !-----------------------------------------------------------
 !10) waso      Water Soluble 			(8 RH%)                     
       data ((a_exty(i,j,10 ),i=1,mby),  j=1,8 ) /           
     &0.2636E+01,0.2534E+01,0.2300E+01,0.2142E+01,0.2022E+01,0.1846E+01,
     &0.1535E+01,0.1261E+01,0.1015E+01,0.7892E+00,
     &0.2564E+01,0.2466E+01,0.2240E+01,0.2089E+01,0.1975E+01,0.1808E+01,
     &0.1513E+01,0.1253E+01,0.1015E+01,0.7959E+00,
     &0.2500E+01,0.2407E+01,0.2193E+01,0.2049E+01,0.1940E+01,0.1781E+01,
     &0.1498E+01,0.1246E+01,0.1014E+01,0.8001E+00,
     &0.2434E+01,0.2346E+01,0.2144E+01,0.2008E+01,0.1904E+01,0.1754E+01,
     &0.1482E+01,0.1240E+01,0.1014E+01,0.8042E+00,
     &0.2291E+01,0.2215E+01,0.2039E+01,0.1920E+01,0.1829E+01,0.1694E+01,
     &0.1448E+01,0.1225E+01,0.1013E+01,0.8130E+00,
     &0.2121E+01,0.2059E+01,0.1914E+01,0.1815E+01,0.1738E+01,0.1623E+01,
     &0.1407E+01,0.1208E+01,0.1012E+01,0.8240E+00,
     &0.1896E+01,0.1852E+01,0.1746E+01,0.1673E+01,0.1614E+01,0.1525E+01,
     &0.1350E+01,0.1182E+01,0.1011E+01,0.8404E+00,
     &0.1752E+01,0.1718E+01,0.1638E+01,0.1581E+01,0.1534E+01,0.1460E+01,
     &0.1311E+01,0.1165E+01,0.1010E+01,0.8518E+00/
       data ((a_ssay(i,j,10 ),i=1,mby),  j=1,8 ) /           
     &0.6646E+00,0.7653E+00,0.8981E+00,0.9419E+00,0.9569E+00,0.9666E+00,
     &0.9685E+00,0.9688E+00,0.9633E+00,0.9558E+00,
     &0.7687E+00,0.8417E+00,0.9345E+00,0.9639E+00,0.9735E+00,0.9796E+00,
     &0.9808E+00,0.9810E+00,0.9776E+00,0.9730E+00,
     &0.8036E+00,0.8665E+00,0.9456E+00,0.9704E+00,0.9784E+00,0.9835E+00,
     &0.9844E+00,0.9847E+00,0.9820E+00,0.9782E+00,
     &0.8288E+00,0.8845E+00,0.9538E+00,0.9751E+00,0.9819E+00,0.9861E+00,
     &0.9871E+00,0.9873E+00,0.9850E+00,0.9820E+00,
     &0.8696E+00,0.9126E+00,0.9655E+00,0.9816E+00,0.9868E+00,0.9900E+00,
     &0.9907E+00,0.9909E+00,0.9895E+00,0.9874E+00,
     &0.9009E+00,0.9345E+00,0.9749E+00,0.9869E+00,0.9906E+00,0.9929E+00,
     &0.9935E+00,0.9938E+00,0.9928E+00,0.9915E+00,
     &0.9319E+00,0.9553E+00,0.9831E+00,0.9913E+00,0.9939E+00,0.9954E+00,
     &0.9959E+00,0.9961E+00,0.9956E+00,0.9949E+00,
     &0.9465E+00,0.9651E+00,0.9870E+00,0.9934E+00,0.9954E+00,0.9966E+00,
     &0.9970E+00,0.9972E+00,0.9968E+00,0.9964E+00/
       data ((a_asyy(i,j,10 ),i=1,mby),  j=1,8 ) /           
     &0.7099E+00,0.6998E+00,0.6762E+00,0.6623E+00,0.6551E+00,0.6486E+00,
     &0.6386E+00,0.6267E+00,0.6143E+00,0.5985E+00,
     &0.7400E+00,0.7344E+00,0.7212E+00,0.7131E+00,0.7084E+00,0.7033E+00,
     &0.6946E+00,0.6841E+00,0.6722E+00,0.6570E+00,
     &0.7487E+00,0.7443E+00,0.7339E+00,0.7276E+00,0.7241E+00,0.7201E+00,
     &0.7118E+00,0.7021E+00,0.6904E+00,0.6755E+00,
     &0.7554E+00,0.7517E+00,0.7427E+00,0.7373E+00,0.7343E+00,0.7310E+00,
     &0.7239E+00,0.7151E+00,0.7042E+00,0.6902E+00,
     &0.7629E+00,0.7609E+00,0.7561E+00,0.7529E+00,0.7507E+00,0.7478E+00,
     &0.7426E+00,0.7349E+00,0.7254E+00,0.7123E+00,
     &0.7685E+00,0.7678E+00,0.7659E+00,0.7644E+00,0.7633E+00,0.7615E+00,
     &0.7576E+00,0.7512E+00,0.7433E+00,0.7320E+00,
     &0.7721E+00,0.7725E+00,0.7736E+00,0.7740E+00,0.7738E+00,0.7732E+00,
     &0.7715E+00,0.7669E+00,0.7612E+00,0.7518E+00,
     &0.7749E+00,0.7755E+00,0.7768E+00,0.7777E+00,0.7784E+00,0.7789E+00,
     &0.7779E+00,0.7750E+00,0.7701E+00,0.7619E+00/
 !-----------------------------------------------------------
 !11) soot	Soot                                              
       data ((a_exty(i,j,11 ),i=1,mby),  j=1,1 ) /           
     &0.2564E+01,0.2504E+01,0.2357E+01,0.2233E+01,0.2108E+01,0.1900E+01,
     &0.1552E+01,0.1267E+01,0.1017E+01,0.8078E+00/
       data ((a_ssay(i,j,11 ),i=1,mby),  j=1,1 ) /           
     &0.3009E+00,0.3045E+00,0.3124E+00,0.3138E+00,0.3091E+00,0.2954E+00,
     &0.2666E+00,0.2384E+00,0.2102E+00,0.1789E+00/
       data ((a_asyy(i,j,11 ),i=1,mby),  j=1,1 ) /           
     &0.5324E+00,0.5169E+00,0.4811E+00,0.4589E+00,0.4449E+00,0.4272E+00,
     &0.3957E+00,0.3664E+00,0.3375E+00,0.3079E+00/
 !-----------------------------------------------------------
 !12) ssam	Sea Salt (Accumulation Mode) 	(8 RH%)             
       data ((a_exty(i,j,12 ),i=1,mby),  j=1,8 ) /           
     &0.8629E+00,0.8715E+00,0.8930E+00,0.9073E+00,0.9173E+00,0.9311E+00,
     &0.9576E+00,0.9787E+00,0.9977E+00,0.1004E+01,
     &0.8793E+00,0.8838E+00,0.8954E+00,0.9047E+00,0.9137E+00,0.9279E+00,
     &0.9519E+00,0.9771E+00,0.9989E+00,0.1019E+01,
     &0.8777E+00,0.8831E+00,0.8965E+00,0.9062E+00,0.9141E+00,0.9262E+00,
     &0.9497E+00,0.9737E+00,0.9984E+00,0.1021E+01,
     &0.8839E+00,0.8886E+00,0.9002E+00,0.9086E+00,0.9155E+00,0.9268E+00,
     &0.9508E+00,0.9735E+00,0.9991E+00,0.1024E+01,
     &0.8990E+00,0.9021E+00,0.9098E+00,0.9160E+00,0.9219E+00,0.9322E+00,
     &0.9531E+00,0.9738E+00,0.9984E+00,0.1025E+01,
     &0.9067E+00,0.9113E+00,0.9224E+00,0.9293E+00,0.9335E+00,0.9397E+00,
     &0.9585E+00,0.9766E+00,0.9990E+00,0.1023E+01,
     &0.9296E+00,0.9329E+00,0.9409E+00,0.9461E+00,0.9497E+00,0.9550E+00,
     &0.9668E+00,0.9813E+00,0.9994E+00,0.1018E+01,
     &0.9437E+00,0.9462E+00,0.9524E+00,0.9566E+00,0.9596E+00,0.9641E+00,
     &0.9729E+00,0.9851E+00,0.9998E+00,0.1013E+01/
       data ((a_ssay(i,j,12 ),i=1,mby),  j=1,8 ) /           
     &0.9998E+00,0.9998E+00,0.9998E+00,0.9999E+00,0.9999E+00,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9995E+00,0.9998E+00,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9995E+00,0.9998E+00,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01/
       data ((a_asyy(i,j,12 ),i=1,mby),  j=1,8 ) /           
     &0.7300E+00,0.7254E+00,0.7146E+00,0.7078E+00,0.7037E+00,0.6998E+00,
     &0.6978E+00,0.6951E+00,0.6925E+00,0.6965E+00,
     &0.7850E+00,0.7850E+00,0.7846E+00,0.7830E+00,0.7798E+00,0.7750E+00,
     &0.7740E+00,0.7708E+00,0.7710E+00,0.7724E+00,
     &0.8029E+00,0.8004E+00,0.7945E+00,0.7909E+00,0.7889E+00,0.7866E+00,
     &0.7831E+00,0.7799E+00,0.7783E+00,0.7800E+00,
     &0.8050E+00,0.8040E+00,0.8015E+00,0.7996E+00,0.7980E+00,0.7950E+00,
     &0.7895E+00,0.7869E+00,0.7840E+00,0.7844E+00,
     &0.8178E+00,0.8169E+00,0.8146E+00,0.8127E+00,0.8108E+00,0.8072E+00,
     &0.8002E+00,0.7963E+00,0.7933E+00,0.7912E+00,
     &0.8304E+00,0.8287E+00,0.8246E+00,0.8224E+00,0.8216E+00,0.8198E+00,
     &0.8116E+00,0.8061E+00,0.8009E+00,0.7985E+00,
     &0.8380E+00,0.8380E+00,0.8378E+00,0.8367E+00,0.8345E+00,0.8309E+00,
     &0.8271E+00,0.8189E+00,0.8136E+00,0.8086E+00,
     &0.8448E+00,0.8449E+00,0.8450E+00,0.8444E+00,0.8431E+00,0.8406E+00,
     &0.8373E+00,0.8299E+00,0.8244E+00,0.8185E+00/
 !-----------------------------------------------------------
 !13) sscm	Sea Salt (Coarse Mode) 		(8 RH%)                  
       data ((a_exty(i,j,13 ),i=1,mby),  j=1,8 ) /           
     &0.9630E+00,0.9648E+00,0.9695E+00,0.9730E+00,0.9758E+00,0.9790E+00,
     &0.9821E+00,0.9899E+00,0.9980E+00,0.1007E+01,
     &0.9727E+00,0.9743E+00,0.9783E+00,0.9809E+00,0.9829E+00,0.9856E+00,
     &0.9907E+00,0.9950E+00,0.9993E+00,0.1006E+01,
     &0.9761E+00,0.9773E+00,0.9805E+00,0.9827E+00,0.9844E+00,0.9866E+00,
     &0.9897E+00,0.9943E+00,0.9997E+00,0.1006E+01,
     &0.9762E+00,0.9780E+00,0.9824E+00,0.9849E+00,0.9862E+00,0.9877E+00,
     &0.9924E+00,0.9962E+00,0.1000E+01,0.1007E+01,
     &0.9811E+00,0.9821E+00,0.9844E+00,0.9861E+00,0.9874E+00,0.9893E+00,
     &0.9931E+00,0.9972E+00,0.1000E+01,0.1005E+01,
     &0.9831E+00,0.9837E+00,0.9852E+00,0.9861E+00,0.9868E+00,0.9880E+00,
     &0.9925E+00,0.9962E+00,0.9997E+00,0.1004E+01,
     &0.9858E+00,0.9865E+00,0.9882E+00,0.9894E+00,0.9904E+00,0.9918E+00,
     &0.9948E+00,0.9969E+00,0.9997E+00,0.1003E+01,
     &0.9901E+00,0.9891E+00,0.9872E+00,0.9870E+00,0.9887E+00,0.9911E+00,
     &0.9895E+00,0.9966E+00,0.9997E+00,0.1002E+01/
       data ((a_ssay(i,j,13 ),i=1,mby),  j=1,8 ) /           
     &0.9974E+00,0.9980E+00,0.9990E+00,0.9994E+00,0.9996E+00,0.9998E+00,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9995E+00,0.9995E+00,0.9995E+00,0.9997E+00,0.9999E+00,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9994E+00,0.9995E+00,0.9997E+00,0.9999E+00,0.9999E+00,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9994E+00,0.9995E+00,0.9997E+00,0.9999E+00,0.9999E+00,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9996E+00,0.9997E+00,0.9998E+00,0.9999E+00,0.9999E+00,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9998E+00,0.9998E+00,0.9998E+00,0.9999E+00,0.9999E+00,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9995E+00,0.9998E+00,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.9995E+00,0.9998E+00,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01/
       data ((a_asyy(i,j,13 ),i=1,mby),  j=1,8 ) /           
     &0.8085E+00,0.8092E+00,0.8108E+00,0.8106E+00,0.8085E+00,0.8049E+00,
     &0.8045E+00,0.8017E+00,0.7964E+00,0.7932E+00,
     &0.8452E+00,0.8456E+00,0.8467E+00,0.8476E+00,0.8486E+00,0.8498E+00,
     &0.8505E+00,0.8504E+00,0.8469E+00,0.8464E+00,
     &0.8483E+00,0.8481E+00,0.8480E+00,0.8486E+00,0.8500E+00,0.8530E+00,
     &0.8581E+00,0.8557E+00,0.8506E+00,0.8505E+00,
     &0.8373E+00,0.8422E+00,0.8535E+00,0.8586E+00,0.8582E+00,0.8542E+00,
     &0.8541E+00,0.8559E+00,0.8570E+00,0.8549E+00,
     &0.8441E+00,0.8465E+00,0.8523E+00,0.8548E+00,0.8546E+00,0.8539E+00,
     &0.8614E+00,0.8626E+00,0.8604E+00,0.8586E+00,
     &0.8390E+00,0.8405E+00,0.8443E+00,0.8471E+00,0.8495E+00,0.8536E+00,
     &0.8641E+00,0.8666E+00,0.8629E+00,0.8652E+00,
     &0.8385E+00,0.8383E+00,0.8382E+00,0.8405E+00,0.8455E+00,0.8545E+00,
     &0.8586E+00,0.8622E+00,0.8680E+00,0.8681E+00,
     &0.8070E+00,0.8121E+00,0.8244E+00,0.8326E+00,0.8384E+00,0.8465E+00,
     &0.8575E+00,0.8605E+00,0.8671E+00,0.8673E+00/
 !-----------------------------------------------------------
 !14) minm	Mineral Dust (Nucleation Mode)                    
       data ((a_exty(i,j,14 ),i=1,mby),  j=1,1 ) /           
     &0.1013E+01,0.1007E+01,0.9897E+00,0.9760E+00,0.9624E+00,0.9367E+00,
     &0.8702E+00,0.7911E+00,0.6970E+00,0.5919E+00/
       data ((a_ssay(i,j,14 ),i=1,mby),  j=1,1 ) /           
     &0.7841E+00,0.7925E+00,0.8134E+00,0.8331E+00,0.8543E+00,0.8839E+00,
     &0.9211E+00,0.9491E+00,0.9647E+00,0.9740E+00/
       data ((a_asyy(i,j,14 ),i=1,mby),  j=1,1 ) /           
     &0.7398E+00,0.7359E+00,0.7260E+00,0.7185E+00,0.7118E+00,0.7018E+00,
     &0.6876E+00,0.6759E+00,0.6649E+00,0.6521E+00/
 !-----------------------------------------------------------
 !15) miam	Mineral Dust (Accumulation Mode)                  
       data ((a_exty(i,j,15 ),i=1,mby),  j=1,1 ) /           
     &0.8958E+00,0.8998E+00,0.9097E+00,0.9170E+00,0.9230E+00,0.9325E+00,
     &0.9520E+00,0.9736E+00,0.9984E+00,0.1028E+01/
       data ((a_ssay(i,j,15 ),i=1,mby),  j=1,1 ) /           
     &0.5676E+00,0.5719E+00,0.5844E+00,0.6014E+00,0.6254E+00,0.6655E+00,
     &0.7403E+00,0.8148E+00,0.8711E+00,0.9093E+00/
       data ((a_asyy(i,j,15 ),i=1,mby),  j=1,1 ) /           
     &0.9144E+00,0.9086E+00,0.8938E+00,0.8802E+00,0.8654E+00,0.8411E+00,
     &0.8007E+00,0.7640E+00,0.7372E+00,0.7162E+00/
 !-----------------------------------------------------------
 !16) micm	Mineral Dust (Coarse Mode)                        
       data ((a_exty(i,j,16 ),i=1,mby),  j=1,1 ) /           
     &0.9717E+00,0.9730E+00,0.9761E+00,0.9783E+00,0.9801E+00,0.9828E+00,
     &0.9880E+00,0.9935E+00,0.9996E+00,0.1007E+01/
       data ((a_ssay(i,j,16 ),i=1,mby),  j=1,1 ) /           
     &0.5462E+00,0.5457E+00,0.5448E+00,0.5454E+00,0.5477E+00,0.5523E+00,
     &0.5714E+00,0.6059E+00,0.6601E+00,0.7118E+00/
       data ((a_asyy(i,j,16 ),i=1,mby),  j=1,1 ) /           
     &0.9433E+00,0.9447E+00,0.9478E+00,0.9490E+00,0.9485E+00,0.9466E+00,
     &0.9380E+00,0.9212E+00,0.8973E+00,0.8741E+00/
 !-----------------------------------------------------------
 !17) mitr	Mineral Dust (Transported Mode)                   
       data ((a_exty(i,j,17 ),i=1,mby),  j=1,1 ) /           
     &0.9243E+00,0.9273E+00,0.9348E+00,0.9403E+00,0.9447E+00,0.9516E+00,
     &0.9657E+00,0.9810E+00,0.9986E+00,0.1019E+01/
       data ((a_ssay(i,j,17 ),i=1,mby),  j=1,1 ) /           
     &0.5535E+00,0.5553E+00,0.5615E+00,0.5724E+00,0.5898E+00,0.6205E+00,
     &0.6873E+00,0.7635E+00,0.8289E+00,0.8763E+00/
       data ((a_asyy(i,j,17 ),i=1,mby),  j=1,1 ) /           
     &0.9371E+00,0.9340E+00,0.9257E+00,0.9168E+00,0.9057E+00,0.8860E+00,
     &0.8473E+00,0.8087E+00,0.7784E+00,0.7532E+00/
 !-----------------------------------------------------------
 !18) suso 	Sulfate Droplets		(8 RH%)                        
       data ((a_exty(i,j,18 ),i=1,mby),  j=1,8 ) /           
     &0.1618E+01,0.1603E+01,0.1566E+01,0.1534E+01,0.1500E+01,0.1438E+01,
     &0.1299E+01,0.1155E+01,0.1009E+01,0.8521E+00,
     &0.1357E+01,0.1352E+01,0.1341E+01,0.1329E+01,0.1315E+01,0.1285E+01,
     &0.1207E+01,0.1115E+01,0.1006E+01,0.8812E+00,
     &0.1272E+01,0.1271E+01,0.1268E+01,0.1263E+01,0.1255E+01,0.1235E+01,
     &0.1175E+01,0.1100E+01,0.1006E+01,0.8928E+00,
     &0.1210E+01,0.1211E+01,0.1215E+01,0.1215E+01,0.1211E+01,0.1198E+01,
     &0.1152E+01,0.1089E+01,0.1005E+01,0.9023E+00,
     &0.1120E+01,0.1125E+01,0.1137E+01,0.1143E+01,0.1144E+01,0.1139E+01,
     &0.1112E+01,0.1069E+01,0.1004E+01,0.9180E+00,
     &0.1033E+01,0.1040E+01,0.1057E+01,0.1067E+01,0.1073E+01,0.1077E+01,
     &0.1070E+01,0.1047E+01,0.1002E+01,0.9381E+00,
     &0.9487E+00,0.9569E+00,0.9770E+00,0.9906E+00,0.1000E+01,0.1012E+01,
     &0.1022E+01,0.1020E+01,0.1001E+01,0.9637E+00,
     &0.9078E+00,0.9155E+00,0.9348E+00,0.9483E+00,0.9585E+00,0.9724E+00,
     &0.9917E+00,0.1002E+01,0.1000E+01,0.9817E+00/
       data ((a_ssay(i,j,18 ),i=1,mby),  j=1,8 ) /           
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01,
     &0.1000E+01,0.1000E+01,0.1000E+01,0.1000E+01/
       data ((a_asyy(i,j,18 ),i=1,mby),  j=1,8 ) /           
     &0.6955E+00,0.6978E+00,0.7034E+00,0.7076E+00,0.7112E+00,0.7165E+00,
     &0.7227E+00,0.7231E+00,0.7172E+00,0.7080E+00,
     &0.7408E+00,0.7444E+00,0.7532E+00,0.7586E+00,0.7617E+00,0.7650E+00,
     &0.7706E+00,0.7717E+00,0.7690E+00,0.7634E+00,
     &0.7498E+00,0.7529E+00,0.7606E+00,0.7656E+00,0.7690E+00,0.7729E+00,
     &0.7779E+00,0.7797E+00,0.7779E+00,0.7743E+00,
     &0.7559E+00,0.7590E+00,0.7664E+00,0.7709E+00,0.7734E+00,0.7762E+00,
     &0.7818E+00,0.7840E+00,0.7837E+00,0.7803E+00,
     &0.7601E+00,0.7631E+00,0.7703E+00,0.7748E+00,0.7776E+00,0.7810E+00,
     &0.7867E+00,0.7894E+00,0.7900E+00,0.7891E+00,
     &0.7657E+00,0.7684E+00,0.7749E+00,0.7789E+00,0.7814E+00,0.7842E+00,
     &0.7890E+00,0.7928E+00,0.7948E+00,0.7949E+00,
     &0.7749E+00,0.7765E+00,0.7802E+00,0.7825E+00,0.7836E+00,0.7853E+00,
     &0.7906E+00,0.7940E+00,0.7975E+00,0.7987E+00,
     &0.7795E+00,0.7808E+00,0.7838E+00,0.7856E+00,0.7865E+00,0.7876E+00,
     &0.7909E+00,0.7937E+00,0.7967E+00,0.7995E+00/
!=====================================================================
! 19  1 NSD=1 ABC=  .500  .200  .000 NGP=9925 RR=  .00 15.00   SINYUK 2003 GR
       data ((a_exty(i,j,19),i=1,mby),j=1,1) /
     & .8770E+00,.8770E+00,.8770E+00,.8770E+00,.8770E+00,
     & .8974E+00,.9352E+00,.9755E+00,.1023E+01,.1072E+01/
       data ((a_ssay(i,j,19),i=1,mby),j=1,1) /
     1 .8484E+00,.8484E+00,.8484E+00,.8484E+00,.8484E+00,
     1 .8916E+00,.9422E+00,.9699E+00,.9827E+00,.9919E+00/
       data ((a_asyy(i,j,19),i=1,mby),j=1,1) /
     1 .7110E+00,.7110E+00,.7110E+00,.7110E+00,.7110E+00,
     1 .6853E+00,.6571E+00,.6443E+00,.6456E+00,.6555E+00/
 ! 20  1 NSD=1 ABC= 1.000  .200  .000 NGP=9925 RR=  .00 30.00   SINYUK 2003 GR
       data ((a_exty(i,j,20),i=1,mby),j=1,1) /
     1 .9407E+00,.9407E+00,.9407E+00,.9407E+00,.9407E+00,
     1 .9531E+00,.9685E+00,.9860E+00,.1011E+01,.1043E+01/
       data ((a_ssay(i,j,20),i=1,mby),j=1,1) /
     1 .7644E+00,.7644E+00,.7644E+00,.7644E+00,.7644E+00,
     1 .8214E+00,.8963E+00,.9410E+00,.9629E+00,.9812E+00/
       data ((a_asyy(i,j,20),i=1,mby),j=1,1) /
     1 .8002E+00,.8002E+00,.8002E+00,.8002E+00,.8002E+00,
     1 .7728E+00,.7383E+00,.7130E+00,.6928E+00,.6731E+00/
 ! 21  1 NSD=1 ABC= 2.000  .200  .000 NGP=9925 RR=  .00 60.00   SINYUK 2003 GR
       data ((a_exty(i,j,21),i=1,mby),j=1,1) /
     1 .9660E+00,.9660E+00,.9660E+00,.9660E+00,.9660E+00,
     1 .9715E+00,.9838E+00,.9940E+00,.1007E+01,.1022E+01/
       data ((a_ssay(i,j,21),i=1,mby),j=1,1) /
     1 .6709E+00,.6709E+00,.6709E+00,.6709E+00,.6709E+00,
     1 .7334E+00,.8313E+00,.8987E+00,.9348E+00,.9647E+00/
       data ((a_asyy(i,j,21),i=1,mby),j=1,1) /
     1 .8677E+00,.8677E+00,.8677E+00,.8677E+00,.8677E+00,
     1 .8421E+00,.8028E+00,.7765E+00,.7588E+00,.7422E+00/
 ! 22  1 NSD=1 ABC= 4.000  .200  .000 NGP=9925 RR=  .00120.00   SINYUK 2003 GR
       data ((a_exty(i,j,22),i=1,mby),j=1,1) /
     1 .9790E+00,.9790E+00,.9790E+00,.9790E+00,.9790E+00,
     1 .9832E+00,.9903E+00,.9972E+00,.1005E+01,.1013E+01/
       data ((a_ssay(i,j,22),i=1,mby),j=1,1) /
     1 .5967E+00,.5967E+00,.5967E+00,.5967E+00,.5967E+00,
     1 .6425E+00,.7438E+00,.8321E+00,.8881E+00,.9386E+00/
       data ((a_asyy(i,j,22),i=1,mby),j=1,1) /
     1 .9134E+00,.9134E+00,.9134E+00,.9134E+00,.9134E+00,
     1 .8934E+00,.8550E+00,.8242E+00,.8057E+00,.7862E+00/
 ! 23  1 NSD=1 ABC= 8.000  .200  .000 NGP=9925 RR=  .00240.00   SINYUK 2003 GR
       data ((a_exty(i,j,23),i=1,mby),j=1,1) /
     1 .9858E+00,.9858E+00,.9858E+00,.9858E+00,.9858E+00,
     1 .9886E+00,.9932E+00,.9973E+00,.1002E+01,.1008E+01/
       data ((a_ssay(i,j,23),i=1,mby),j=1,1) /
     1 .5620E+00,.5620E+00,.5620E+00,.5620E+00,.5620E+00,
     1 .5812E+00,.6520E+00,.7416E+00,.8155E+00,.8923E+00/
       data ((a_asyy(i,j,23),i=1,mby),j=1,1) /
     1 .9345E+00,.9345E+00,.9345E+00,.9345E+00,.9345E+00,
     1 .9259E+00,.8980E+00,.8667E+00,.8433E+00,.8192E+00/
 ! 24 Boutique 1 [ Match Log_normal rg=.298 sg=2 weighted (0.1-0.5um) Lacis2004]      
       data ((a_exty(i,j,24),i=1,mby),j=1,1) /
     1 .8998E+00,.8998E+00,.8998E+00,.8998E+00,.8998E+00,
     1 .9237E+00,.9594E+00,.9876E+00,.1007E+01,.1012E+01/
       data ((a_ssay(i,j,24),i=1,mby),j=1,1) /
     1 .8757E+00,.8757E+00,.8757E+00,.8757E+00,.8757E+00,
     1 .9130E+00,.9547E+00,.9766E+00,.9864E+00,.9934E+00/
       data ((a_asyy(i,j,24),i=1,mby),j=1,1) /
     1 .6828E+00,.6828E+00,.6828E+00,.6828E+00,.6828E+00,
     1 .6652E+00,.6502E+00,.6474E+00,.6537E+00,.6629E+00/
 ! 25 Boutique 2[ Match Log_normal rg=.298 sg=2 weighted (0.5-5.um) Lacis2004]        
       data ((a_exty(i,j,25),i=1,mby),j=1,1) /
     1 .9271E+00,.9271E+00,.9271E+00,.9271E+00,.9271E+00,
     1 .9399E+00,.9627E+00,.9855E+00,.1015E+01,.1054E+01/
       data ((a_ssay(i,j,25),i=1,mby),j=1,1) /
     1 .7612E+00,.7612E+00,.7612E+00,.7612E+00,.7612E+00,
     1 .8155E+00,.8898E+00,.9365E+00,.9605E+00,.9797E+00/
       data ((a_asyy(i,j,25),i=1,mby),j=1,1) /
     1 .7960E+00,.7960E+00,.7960E+00,.7960E+00,.7960E+00,
     1 .7692E+00,.7329E+00,.7092E+00,.6944E+00,.6824E+00/
     
!#201004 ADD LACIS2004 aerosols consistent with ED3Match dust sizes      
!  0.18  0.22  0.24  0.29  0.30  0.32  0.36  0.44  0.50  0.60
!  0.22  0.24  0.29  0.30  0.32  0.36  0.44  0.50  0.60  0.69
!#26 Lacis2004 Dust Re=0.35
       data ((a_exty(i,j,26),i=1,mby),j=1,1) /
     &  0.7996,  0.8260,  0.8524,  0.8748,  0.8890,
     &  0.9132,  0.9552,  0.9910,  0.9989,   0.9676/
      data ((a_ssay(i,j,26),i=1,mby),j=1,1) /
     &  0.7842,  0.8179,  0.8516,  0.8801,  0.8982,
     &  0.9250,  0.9596,  0.9811,  0.9894,   0.9947/
      data ((a_asyy(i,j,26),i=1,mby),j=1,1) /
     &  0.7084,  0.6943,  0.6802,  0.6682,  0.6606,
     &  0.6505,  0.6446,  0.6501,  0.6621,   0.6721/
!#27Lacis2004 Dust Re=0.82
      data ((a_exty(i,j,27),i=1,mby),j=1,1) /
     &  0.8921,  0.9040,  0.9160,  0.9261,  0.9325,
     &  0.9425,  0.9633,  0.9891,   1.019,    1.061/
      data ((a_ssay(i,j,27),i=1,mby),j=1,1) /
     &  0.6372,  0.6870,  0.7368,  0.7789,  0.8058,
     &  0.8469,  0.9077,  0.9512,  0.9710,   0.9854/
      data ((a_asyy(i,j,27),i=1,mby),j=1,1) /
     &  0.8558,  0.8304,  0.8051,  0.7836,  0.7699,
     &  0.7489,  0.7163,  0.6879,  0.6705,   0.6563/
!#28Lacis2004 Dust Re=1.70
      data ((a_exty(i,j,28),i=1,mby),j=1,1) /
     &  0.9392,  0.9457,  0.9523,  0.9578,  0.9614,
     &  0.9674,  0.9790,  0.9932,   1.006,    1.025/
      data ((a_ssay(i,j,28),i=1,mby),j=1,1) /
     &  0.5183,  0.5750,  0.6317,  0.6797,  0.7102,
     &  0.7591,  0.8443,  0.9118,  0.9451,   0.9700/
      data ((a_asyy(i,j,28),i=1,mby),j=1,1) /
     &  0.9309,  0.9061,  0.8812,  0.8602,  0.8468,
     &  0.8257,  0.7909,  0.7615,  0.7446,   0.7255/
!#29Lacis2004 Dust Re= 3.2
      data ((a_exty(i,j,29),i=1,mby),j=1,1) /
     &  0.9607,  0.9650,  0.9694,  0.9730,  0.9754,
     &  0.9794,  0.9870,  0.9961,   1.004,    1.016/
      data ((a_ssay(i,j,29),i=1,mby),j=1,1) /
     &  0.4759,  0.5219,  0.5678,  0.6067,  0.6314,
     &  0.6743,  0.7694,  0.8596,  0.9097,   0.9500/
      data ((a_asyy(i,j,29),i=1,mby),j=1,1) /
     &  0.9640,  0.9436,  0.9232,  0.9059,  0.8949,
     &  0.8768,  0.8402,  0.8069,  0.7897,   0.7722/	 
     
     
      end
!=============================================================================
      block data aerosol1		 
      USE FUINPUT , only :nrh,mbx,naer
      integer i,j
!      parameter (nrh=8,mbx=21,naer=29)	 
      real, dimension(mbx,nrh,naer)::a_ssax,a_extx,a_asyx 
        common /aer_optx/ a_ssax,a_extx,a_asyx 	


          data ((a_extx(i,j, 1),i=1,mbx),j=1, 8) / 
     &1.0208E+00,1.0281E+00,1.0247E+00,9.8500E-01,9.1917E-01,8.6738E-01,
     &8.0687E-01,8.3349E-01,8.0687E-01,8.2091E-01,8.3979E-01,8.4076E-01,
     &8.3785E-01,8.1365E-01,8.2430E-01,6.2439E-01,6.2730E-01,6.9990E-01,
     &7.2072E-01,6.3166E-01,5.8374E-01,
     &1.0115E+00,1.0070E+00,1.0016E+00,9.5260E-01,8.8129E-01,8.2729E-01,
     &7.6298E-01,7.8318E-01,7.5763E-01,7.6876E-01,7.8689E-01,7.8854E-01,
     &7.8772E-01,7.6834E-01,7.5185E-01,5.7749E-01,5.8574E-01,6.6447E-01,
     &6.8425E-01,6.0552E-01,5.5070E-01,
     &9.9199E-01,1.0017E+00,1.0049E+00,9.9399E-01,9.3880E-01,8.8533E-01,
     &8.0011E-01,8.1441E-01,7.5808E-01,7.6265E-01,7.7924E-01,7.8153E-01,
     &7.8582E-01,7.6780E-01,7.5150E-01,5.8593E-01,6.3254E-01,6.9831E-01,
     &7.1776E-01,6.6228E-01,5.9365E-01,
     &1.0045E+00,1.0024E+00,9.9949E-01,1.0095E+00,1.0350E+00,1.0291E+00,
     &9.5539E-01,9.2472E-01,8.5340E-01,8.2106E-01,8.1314E-01,8.2055E-01,
     &8.2413E-01,8.1991E-01,8.1915E-01,6.8290E-01,7.5588E-01,8.0445E-01,
     &8.2285E-01,8.0777E-01,7.4668E-01,
     &9.9192E-01,9.9910E-01,1.0009E+00,9.9641E-01,1.0144E+00,1.0341E+00,
     &9.9910E-01,9.4165E-01,8.9237E-01,8.4057E-01,8.1472E-01,8.2280E-01,
     &8.1957E-01,8.1822E-01,8.1930E-01,7.0781E-01,7.7873E-01,8.2136E-01,
     &8.3573E-01,8.3070E-01,7.8312E-01,
     &9.9145E-01,9.8535E-01,9.8657E-01,9.9328E-01,9.9084E-01,1.0092E+00,
     &1.0226E+00,9.5177E-01,9.3651E-01,8.6813E-01,8.2112E-01,8.2906E-01,
     &8.1624E-01,8.1319E-01,8.1197E-01,7.2650E-01,7.9548E-01,8.3333E-01,
     &8.4127E-01,8.4127E-01,8.0769E-01,
     &9.7869E-01,9.7975E-01,9.7833E-01,9.7158E-01,9.8117E-01,9.7762E-01,
     &1.0050E+00,9.5488E-01,9.9680E-01,9.2185E-01,8.5115E-01,8.5471E-01,
     &8.2735E-01,8.1279E-01,8.0178E-01,7.4494E-01,8.1528E-01,8.4902E-01,
     &8.4831E-01,8.4405E-01,8.2700E-01,
     &9.8300E-01,9.7474E-01,9.7144E-01,9.6553E-01,9.5774E-01,9.6624E-01,
     &9.6719E-01,9.4806E-01,1.0172E+00,9.6317E-01,8.8362E-01,8.8149E-01,
     &8.4254E-01,8.1209E-01,7.8848E-01,7.4292E-01,8.1893E-01,8.5246E-01,
     &8.4655E-01,8.3168E-01,8.1988E-01/
          data ((a_ssax(i,j, 1),i=1,mbx),j=1, 8) / 
     &1.0000E+00,1.0000E+00,9.9950E-01,9.9850E-01,9.8700E-01,9.4620E-01,
     &9.0770E-01,4.8510E-01,9.2490E-01,8.3380E-01,8.6780E-01,7.3690E-01,
     &7.7840E-01,7.5930E-01,7.4900E-01,6.0170E-01,3.9480E-01,4.1600E-01,
     &4.5070E-01,4.4260E-01,3.3460E-01,
     &1.0000E+00,1.0000E+00,9.9580E-01,9.9870E-01,9.8320E-01,9.5310E-01,
     &9.1320E-01,4.8730E-01,9.2020E-01,8.2300E-01,8.4470E-01,7.2280E-01,
     &7.6750E-01,7.1810E-01,7.3650E-01,5.8070E-01,3.9980E-01,4.1370E-01,
     &4.4680E-01,4.4270E-01,3.4040E-01,
     &1.0000E+00,1.0000E+00,9.9720E-01,9.9860E-01,9.8780E-01,9.6490E-01,
     &9.2010E-01,4.8900E-01,9.0550E-01,7.8450E-01,8.2010E-01,6.7750E-01,
     &7.2570E-01,6.5320E-01,7.0180E-01,5.3660E-01,3.9880E-01,4.1540E-01,
     &4.4160E-01,4.4710E-01,3.6880E-01,
     &1.0000E+00,9.9990E-01,9.9870E-01,9.9910E-01,9.9150E-01,9.7970E-01,
     &9.2230E-01,4.9920E-01,8.7290E-01,7.1630E-01,7.5830E-01,6.0340E-01,
     &6.4570E-01,5.7200E-01,6.3840E-01,4.9880E-01,4.2000E-01,4.3160E-01,
     &4.4720E-01,4.5710E-01,4.1610E-01,
     &1.0000E+00,9.9990E-01,9.9910E-01,9.9910E-01,9.9170E-01,9.8140E-01,
     &9.1870E-01,5.0560E-01,8.6210E-01,6.9900E-01,7.3510E-01,5.8580E-01,
     &6.2050E-01,5.5200E-01,6.1450E-01,4.9410E-01,4.2850E-01,4.3790E-01,
     &4.5070E-01,4.5990E-01,4.2840E-01,
     &1.0000E+00,1.0000E+00,9.9940E-01,9.9940E-01,9.9110E-01,9.8070E-01,
     &9.1140E-01,5.1100E-01,8.5230E-01,6.8790E-01,7.1430E-01,5.7550E-01,
     &6.0050E-01,5.3610E-01,5.9050E-01,4.9040E-01,4.3630E-01,4.4390E-01,
     &4.5420E-01,4.6200E-01,4.3820E-01,
     &1.0000E+00,9.9960E-01,9.9960E-01,9.9890E-01,9.8960E-01,9.7870E-01,
     &8.9310E-01,5.1720E-01,8.3790E-01,6.8310E-01,6.9510E-01,5.7540E-01,
     &5.8640E-01,5.2470E-01,5.6300E-01,4.8580E-01,4.4640E-01,4.5270E-01,
     &4.5910E-01,4.6420E-01,4.4740E-01,
     &1.0000E+00,1.0000E+00,9.9980E-01,9.9900E-01,9.8870E-01,9.7780E-01,
     &8.9760E-01,5.2000E-01,8.2960E-01,6.8860E-01,6.9540E-01,5.8700E-01,
     &5.8990E-01,5.2460E-01,5.5050E-01,4.8130E-01,4.5020E-01,4.5630E-01,
     &4.6120E-01,4.6340E-01,4.4950E-01/
          data ((a_asyx(i,j, 1),i=1,mbx),j=1, 8) / 
     &8.1080E-01,8.1680E-01,8.1880E-01,8.2170E-01,8.1850E-01,8.2120E-01,
     &8.1940E-01,9.0450E-01,7.6950E-01,8.0180E-01,8.2120E-01,8.3240E-01,
     &8.3910E-01,8.5070E-01,8.2510E-01,8.9260E-01,8.5090E-01,8.0390E-01,
     &7.3730E-01,6.6410E-01,5.5450E-01,
     &8.1460E-01,8.2250E-01,8.2320E-01,8.2250E-01,8.2320E-01,8.2210E-01,
     &8.2100E-01,9.0410E-01,7.7250E-01,8.0840E-01,8.4880E-01,8.3880E-01,
     &8.4440E-01,8.5820E-01,8.5540E-01,9.0060E-01,8.5910E-01,8.0930E-01,
     &7.4570E-01,6.7290E-01,5.6850E-01,
     &8.1380E-01,8.1840E-01,8.2390E-01,8.3030E-01,8.3350E-01,8.3420E-01,
     &8.3490E-01,9.0710E-01,7.8830E-01,8.2640E-01,8.5070E-01,8.5720E-01,
     &8.5980E-01,8.8490E-01,8.8140E-01,9.2270E-01,8.7600E-01,8.3360E-01,
     &7.8010E-01,7.1590E-01,6.3790E-01,
     &7.9090E-01,8.0590E-01,8.1080E-01,8.2580E-01,8.3990E-01,8.4780E-01,
     &8.6110E-01,9.2470E-01,8.2000E-01,8.5910E-01,8.6260E-01,8.8560E-01,
     &8.8350E-01,9.1740E-01,9.1490E-01,9.5290E-01,9.0330E-01,8.7320E-01,
     &8.3940E-01,7.9070E-01,7.4450E-01,
     &7.7020E-01,7.9210E-01,7.9840E-01,8.1620E-01,8.3300E-01,8.4570E-01,
     &8.6700E-01,9.2890E-01,8.3290E-01,8.7190E-01,8.7000E-01,8.9400E-01,
     &8.9180E-01,9.2460E-01,9.2220E-01,9.6040E-01,9.0970E-01,8.8230E-01,
     &8.5610E-01,8.1480E-01,7.7590E-01,
     &7.4060E-01,7.6030E-01,7.7470E-01,8.0200E-01,8.1900E-01,8.3600E-01,
     &8.6810E-01,9.3040E-01,8.4570E-01,8.8350E-01,8.8040E-01,9.0140E-01,
     &8.9960E-01,9.3090E-01,9.2870E-01,9.6620E-01,9.1370E-01,8.8760E-01,
     &8.6680E-01,8.3660E-01,8.0440E-01,
     &7.1800E-01,7.2460E-01,7.3730E-01,7.6480E-01,8.0330E-01,8.1690E-01,
     &8.6010E-01,9.2820E-01,8.5730E-01,8.9700E-01,8.9550E-01,9.1180E-01,
     &9.1110E-01,9.3730E-01,9.3640E-01,9.7170E-01,9.1690E-01,8.8970E-01,
     &8.7710E-01,8.5970E-01,8.3660E-01,
     &6.9820E-01,7.1180E-01,7.1390E-01,7.4640E-01,7.8710E-01,8.0730E-01,
     &8.4950E-01,9.2610E-01,8.5850E-01,9.0210E-01,9.0400E-01,9.1580E-01,
     &9.1610E-01,9.3970E-01,9.3910E-01,9.7360E-01,9.1750E-01,8.8810E-01,
     &8.7350E-01,8.6630E-01,8.5170E-01/
          data ((a_extx(i,j, 2),i=1,mbx),j=1, 8) / 
     &7.1950E-01,5.8634E-01,5.1959E-01,3.6370E-01,2.3872E-01,1.3100E-01,
     &1.1923E-01,3.1330E-02,1.3827E-02,1.2336E-02,9.3351E-03,1.0431E-02,
     &1.7871E-02,4.3792E-02,2.1114E-02,7.6577E-03,8.3711E-03,1.2704E-02,
     &1.0925E-02,1.1824E-02,1.3801E-02,
     &7.1959E-01,5.8643E-01,5.1977E-01,3.6370E-01,2.3881E-01,1.3118E-01,
     &1.1932E-01,3.1473E-02,1.3917E-02,1.2426E-02,9.4070E-03,1.0503E-02,
     &1.7934E-02,4.3845E-02,2.1159E-02,7.6954E-03,8.4160E-03,1.2758E-02,
     &1.0970E-02,1.1860E-02,1.3827E-02,
     &7.2069E-01,5.8777E-01,5.2169E-01,3.6541E-01,2.4012E-01,1.3358E-01,
     &1.1734E-01,4.7462E-02,1.4807E-02,1.3090E-02,9.7152E-03,1.1273E-02,
     &1.7856E-02,4.2479E-02,2.1466E-02,8.7102E-03,1.1993E-02,1.5662E-02,
     &1.2529E-02,1.2647E-02,1.4397E-02,
     &7.2548E-01,5.9457E-01,5.3140E-01,3.7366E-01,2.4725E-01,1.4510E-01,
     &1.1135E-01,1.0598E-01,1.8643E-02,1.5793E-02,1.0901E-02,1.4170E-02,
     &1.7526E-02,3.7483E-02,2.1629E-02,1.2702E-02,2.6101E-02,2.7168E-02,
     &1.8834E-02,1.5540E-02,1.6397E-02,
     &7.3813E-01,6.1088E-01,5.5115E-01,3.9198E-01,2.6285E-01,1.6448E-01,
     &1.0982E-01,1.6399E-01,2.4153E-02,1.9223E-02,1.2295E-02,1.7193E-02,
     &1.7304E-02,3.2819E-02,2.0516E-02,1.6894E-02,4.0998E-02,3.9452E-02,
     &2.5667E-02,1.8372E-02,1.8200E-02,
     &7.4777E-01,6.2324E-01,5.6513E-01,4.0581E-01,2.7503E-01,1.7794E-01,
     &1.1220E-01,1.9058E-01,2.7941E-02,2.1466E-02,1.3250E-02,1.8851E-02,
     &1.7423E-02,3.0948E-02,1.9761E-02,1.8879E-02,4.8020E-02,4.5307E-02,
     &2.8975E-02,1.9690E-02,1.9013E-02,
     &7.5976E-01,6.3902E-01,5.8317E-01,4.2390E-01,2.9146E-01,1.9498E-01,
     &1.1829E-01,2.1554E-01,3.3561E-02,2.5171E-02,1.5449E-02,2.1639E-02,
     &1.8710E-02,3.0439E-02,1.9817E-02,2.1244E-02,5.5098E-02,5.1488E-02,
     &3.2854E-02,2.1624E-02,2.0380E-02,
     &7.7250E-01,6.5542E-01,6.0126E-01,4.4298E-01,3.0854E-01,2.1224E-01,
     &1.2597E-01,2.3513E-01,3.9016E-02,2.8547E-02,1.7321E-02,2.3932E-02,
     &1.9813E-02,3.0225E-02,1.9889E-02,2.3074E-02,6.0564E-02,5.6293E-02,
     &3.5850E-02,2.3551E-02,1.8429E-02/
          data ((a_ssax(i,j, 2),i=1,mbx),j=1, 8) / 
     &9.5330E-01,9.4310E-01,9.3200E-01,8.9170E-01,8.5220E-01,7.5750E-01,
     &3.8250E-01,5.6870E-01,8.3530E-01,5.8320E-01,3.9540E-01,2.0140E-01,
     &1.0340E-01,2.1190E-02,1.2060E-01,1.8780E-01,8.8930E-02,5.5110E-02,
     &4.5120E-02,2.2670E-02,1.6310E-02,
     &9.5320E-01,9.4300E-01,9.3190E-01,8.9170E-01,8.5220E-01,7.5750E-01,
     &3.8280E-01,5.6470E-01,8.3520E-01,5.8430E-01,3.9830E-01,2.0480E-01,
     &1.0520E-01,2.1770E-02,1.2170E-01,1.8850E-01,8.6800E-02,5.4350E-02,
     &4.6090E-02,2.3570E-02,1.6910E-02,
     &9.5630E-01,9.4670E-01,9.3650E-01,8.9850E-01,8.6150E-01,7.7500E-01,
     &4.0680E-01,3.8420E-01,8.3970E-01,5.8560E-01,4.0980E-01,2.1100E-01,
     &1.1170E-01,2.3860E-02,1.2050E-01,1.6230E-01,5.6600E-02,4.2370E-02,
     &4.1270E-02,2.3140E-02,1.6720E-02,
     &9.6840E-01,9.6100E-01,9.5350E-01,9.2520E-01,8.9740E-01,8.4080E-01,
     &5.1400E-01,2.0930E-01,8.6460E-01,6.0220E-01,4.6000E-01,2.3610E-01,
     &1.4270E-01,3.4600E-02,1.1980E-01,9.9470E-02,2.4080E-02,2.4760E-02,
     &3.1030E-02,2.2810E-02,1.6560E-02,
     &9.7930E-01,9.7470E-01,9.7000E-01,9.5140E-01,9.3360E-01,9.0220E-01,
     &6.5170E-01,1.7980E-01,8.9040E-01,6.2980E-01,5.1250E-01,2.5990E-01,
     &1.8070E-01,4.9170E-02,1.2160E-01,6.4500E-02,1.6770E-02,1.9150E-02,
     &2.4770E-02,2.1120E-02,1.5290E-02,
     &9.8350E-01,9.8050E-01,9.7710E-01,9.6260E-01,9.4940E-01,9.2750E-01,
     &7.2360E-01,1.8060E-01,9.0260E-01,6.4890E-01,5.4260E-01,2.7630E-01,
     &2.0500E-01,5.9320E-02,1.2500E-01,5.4400E-02,1.5870E-02,1.8400E-02,
     &2.3320E-02,2.0710E-02,1.4930E-02,
     &9.8750E-01,9.8500E-01,9.8250E-01,9.7260E-01,9.6290E-01,9.4780E-01,
     &7.8970E-01,1.8810E-01,9.1400E-01,6.7580E-01,5.9130E-01,3.1480E-01,
     &2.5740E-01,8.6620E-02,1.5050E-01,5.7930E-02,2.0900E-02,2.4520E-02,
     &3.2280E-02,3.3680E-02,2.5100E-02,
     &9.9010E-01,9.8860E-01,9.8670E-01,9.7860E-01,9.7200E-01,9.6120E-01,
     &8.3810E-01,1.9740E-01,9.2200E-01,6.9630E-01,6.2220E-01,3.4030E-01,
     &2.9320E-01,1.0740E-01,1.6880E-01,6.0540E-02,2.4140E-02,2.8280E-02,
     &3.7290E-02,4.0610E-02,3.7330E-02/
          data ((a_asyx(i,j, 2),i=1,mbx),j=1, 8) / 
     &6.2630E-01,6.1430E-01,6.1250E-01,5.8920E-01,5.6350E-01,5.5680E-01,
     &4.9790E-01,4.4100E-01,4.2060E-01,4.0790E-01,4.0380E-01,3.3530E-01,
     &4.8190E-01,5.0940E-01,3.6070E-01,4.4840E-01,5.5360E-01,5.2240E-01,
     &4.7730E-01,4.6650E-01,3.8690E-01,
     &6.2630E-01,6.1430E-01,6.1250E-01,5.8930E-01,5.6360E-01,5.5690E-01,
     &4.9850E-01,4.3970E-01,4.2310E-01,4.1160E-01,4.1060E-01,3.4860E-01,
     &4.8920E-01,5.1890E-01,3.6660E-01,4.5590E-01,5.5850E-01,5.2760E-01,
     &4.8340E-01,4.7240E-01,3.9360E-01,
     &6.3720E-01,6.2500E-01,6.2290E-01,5.9930E-01,5.7330E-01,5.6530E-01,
     &5.0790E-01,4.3840E-01,4.2890E-01,4.1770E-01,4.2020E-01,3.7230E-01,
     &4.9050E-01,5.2340E-01,3.7470E-01,4.6700E-01,5.5900E-01,5.2670E-01,
     &4.8820E-01,4.8280E-01,4.0860E-01,
     &6.7540E-01,6.6320E-01,6.6010E-01,6.3630E-01,6.0970E-01,5.9730E-01,
     &5.4390E-01,4.4390E-01,4.5040E-01,4.3960E-01,4.4940E-01,4.2740E-01,
     &4.9410E-01,5.3440E-01,4.0950E-01,5.0380E-01,5.4310E-01,5.1230E-01,
     &5.0370E-01,5.1780E-01,4.6020E-01,
     &7.1180E-01,7.0040E-01,6.9670E-01,6.7410E-01,6.4830E-01,6.3260E-01,
     &5.8350E-01,4.5780E-01,4.7240E-01,4.5590E-01,4.5810E-01,4.3420E-01,
     &4.7370E-01,5.0670E-01,4.2960E-01,5.1410E-01,4.9310E-01,4.6870E-01,
     &4.9020E-01,5.3040E-01,4.9280E-01,
     &7.2820E-01,7.1750E-01,7.1370E-01,6.9240E-01,6.6760E-01,6.5080E-01,
     &6.0450E-01,4.6950E-01,4.8670E-01,4.6700E-01,4.6280E-01,4.3540E-01,
     &4.6380E-01,4.8930E-01,4.3780E-01,5.1420E-01,4.6080E-01,4.4080E-01,
     &4.7630E-01,5.3210E-01,5.0790E-01,
     &7.4280E-01,7.3310E-01,7.2940E-01,7.0960E-01,6.8640E-01,6.6940E-01,
     &6.2730E-01,4.8890E-01,5.1180E-01,4.9750E-01,5.0490E-01,4.8090E-01,
     &5.1480E-01,5.5590E-01,5.2090E-01,5.9560E-01,5.3040E-01,5.1070E-01,
     &5.5200E-01,6.0200E-01,5.8180E-01,
     &7.5380E-01,7.4500E-01,7.4160E-01,7.2340E-01,7.0170E-01,6.8480E-01,
     &6.4590E-01,5.0520E-01,5.2950E-01,5.1530E-01,5.2180E-01,4.9620E-01,
     &5.2860E-01,5.7110E-01,5.5770E-01,6.3350E-01,5.4600E-01,5.2750E-01,
     &5.7510E-01,6.2710E-01,6.2500E-01/
          data ((a_extx(i,j, 3),i=1,mbx),j=1, 8) / 
     &7.1719E-01,5.8431E-01,5.1821E-01,3.6306E-01,2.3911E-01,1.3291E-01,
     &1.1964E-01,3.3269E-02,1.5240E-02,1.3430E-02,1.0289E-02,1.1348E-02,
     &1.7810E-02,4.2930E-02,2.0297E-02,7.3272E-03,8.1036E-03,1.2212E-02,
     &1.0304E-02,1.1176E-02,1.3053E-02,
     &7.1723E-01,5.8435E-01,5.1819E-01,3.6307E-01,2.3912E-01,1.3304E-01,
     &1.1974E-01,3.3548E-02,1.5360E-02,1.3554E-02,1.0401E-02,1.1448E-02,
     &1.7900E-02,4.2979E-02,2.0384E-02,7.4180E-03,8.1935E-03,1.2284E-02,
     &1.0351E-02,1.1205E-02,1.3062E-02,
     &7.1775E-01,5.8519E-01,5.1980E-01,3.6437E-01,2.4023E-01,1.3538E-01,
     &1.1785E-01,4.9757E-02,1.6284E-02,1.4317E-02,1.0817E-02,1.2281E-02,
     &1.7969E-02,4.1801E-02,2.0868E-02,8.6679E-03,1.1948E-02,1.5300E-02,
     &1.1990E-02,1.2037E-02,1.3627E-02,
     &7.2196E-01,5.9123E-01,5.2846E-01,3.7154E-01,2.4628E-01,1.4562E-01,
     &1.1122E-01,1.0773E-01,1.9312E-02,1.6400E-02,1.1489E-02,1.4647E-02,
     &1.7484E-02,3.6984E-02,2.1140E-02,1.2818E-02,2.6126E-02,2.6843E-02,
     &1.8285E-02,1.4948E-02,1.5636E-02,
     &7.3446E-01,6.0684E-01,5.4702E-01,3.8864E-01,2.6058E-01,1.6347E-01,
     &1.0891E-01,1.6497E-01,2.3967E-02,1.9134E-02,1.2279E-02,1.7106E-02,
     &1.6987E-02,3.2335E-02,2.0019E-02,1.6930E-02,4.0960E-02,3.9096E-02,
     &2.5116E-02,1.7797E-02,1.7508E-02,
     &7.4291E-01,6.1779E-01,5.6012E-01,4.0117E-01,2.7146E-01,1.7576E-01,
     &1.1056E-01,1.9203E-01,2.7312E-02,2.1051E-02,1.2962E-02,1.8553E-02,
     &1.6975E-02,3.0445E-02,1.9247E-02,1.8935E-02,4.8182E-02,4.5152E-02,
     &2.8548E-02,1.9189E-02,1.8392E-02,
     &7.5287E-01,6.3078E-01,5.7492E-01,4.1583E-01,2.8476E-01,1.9016E-01,
     &1.1458E-01,2.1676E-01,3.1495E-02,2.3489E-02,1.3996E-02,2.0263E-02,
     &1.7292E-02,2.9101E-02,1.8598E-02,2.0836E-02,5.4902E-02,5.0906E-02,
     &3.1883E-02,2.0585E-02,1.9290E-02,
     &7.6408E-01,6.4511E-01,5.9080E-01,4.3247E-01,2.9971E-01,2.0534E-01,
     &1.2075E-01,2.3566E-01,3.6006E-02,2.6075E-02,1.5170E-02,2.1888E-02,
     &1.7799E-02,2.8362E-02,1.8181E-02,2.2273E-02,5.9971E-02,5.5287E-02,
     &3.4454E-02,2.2115E-02,1.7184E-02/
          data ((a_ssax(i,j, 3),i=1,mbx),j=1, 8) / 
     &9.3000E-01,9.1900E-01,9.0710E-01,8.6490E-01,8.2120E-01,7.1550E-01,
     &3.5910E-01,4.9500E-01,6.6440E-01,4.5090E-01,2.8560E-01,1.5840E-01,
     &5.8670E-02,1.1040E-02,8.5090E-02,9.7340E-02,2.7960E-02,1.7140E-02,
     &1.3090E-02,4.0180E-03,2.3830E-03,
     &9.2940E-01,9.1840E-01,9.0640E-01,8.6410E-01,8.2020E-01,7.1420E-01,
     &3.5860E-01,4.9020E-01,6.5890E-01,4.4690E-01,2.8280E-01,1.5730E-01,
     &5.8570E-02,1.1080E-02,8.4840E-02,9.6210E-02,2.7430E-02,1.6950E-02,
     &1.3150E-02,4.1160E-03,2.4540E-03,
     &9.3210E-01,9.2170E-01,9.1050E-01,8.7030E-01,8.2870E-01,7.3050E-01,
     &3.8040E-01,3.4040E-01,6.6320E-01,4.4530E-01,2.8560E-01,1.5670E-01,
     &6.2500E-02,1.2130E-02,8.2880E-02,8.0590E-02,1.8600E-02,1.3870E-02,
     &1.1910E-02,4.0730E-03,2.4450E-03,
     &9.4810E-01,9.4000E-01,9.3190E-01,9.0160E-01,8.6960E-01,8.0340E-01,
     &4.8530E-01,1.9310E-01,7.3160E-01,4.8440E-01,3.3260E-01,1.7280E-01,
     &8.5470E-02,1.8140E-02,8.1050E-02,4.9960E-02,9.4630E-03,9.5720E-03,
     &9.7080E-03,4.2780E-03,2.5340E-03,
     &9.6480E-01,9.5590E-01,9.5480E-01,9.3490E-01,9.1430E-01,8.7710E-01,
     &6.2680E-01,1.7000E-01,8.1150E-01,5.4940E-01,4.0940E-01,2.0520E-01,
     &1.2450E-01,2.9550E-02,8.4480E-02,3.4770E-02,8.1950E-03,9.1210E-03,
     &9.2790E-03,4.6730E-03,2.6190E-03,
     &9.7170E-01,9.6860E-01,9.6490E-01,9.4950E-01,9.3420E-01,9.0810E-01,
     &7.0250E-01,1.7110E-01,8.4550E-01,5.8490E-01,4.5450E-01,2.2680E-01,
     &1.5120E-01,3.8470E-02,8.8510E-02,3.0450E-02,8.6170E-03,9.7160E-03,
     &9.6800E-03,5.1000E-03,2.7650E-03,
     &9.7810E-01,9.7570E-01,9.7300E-01,9.6240E-01,9.5130E-01,9.3340E-01,
     &7.7220E-01,1.7670E-01,8.7400E-01,6.2110E-01,5.0380E-01,2.5420E-01,
     &1.8550E-01,5.1660E-02,9.6860E-02,2.9040E-02,1.0090E-02,1.1460E-02,
     &1.1600E-02,7.2860E-03,4.2450E-03,
     &9.8270E-01,9.8110E-01,9.7930E-01,9.7080E-01,9.6310E-01,9.5050E-01,
     &8.2450E-01,1.8530E-01,8.9410E-01,6.5260E-01,5.4610E-01,2.8070E-01,
     &2.1800E-01,6.5110E-02,1.0580E-01,2.8890E-02,1.1670E-02,1.3250E-02,
     &1.3380E-02,8.8700E-03,6.2060E-03/
          data ((a_asyx(i,j, 3),i=1,mbx),j=1, 8) / 
     &6.2410E-01,6.1220E-01,6.1030E-01,5.8710E-01,5.6080E-01,5.5180E-01,
     &4.8520E-01,4.1580E-01,3.7380E-01,3.3890E-01,3.0060E-01,2.5940E-01,
     &2.6840E-01,2.3560E-01,2.2820E-01,2.3230E-01,2.7340E-01,2.5590E-01,
     &2.3960E-01,3.2170E-01,3.1620E-01,
     &6.2410E-01,6.1220E-01,6.1030E-01,5.8700E-01,5.6080E-01,5.5170E-01,
     &4.8520E-01,4.1560E-01,3.7420E-01,3.3940E-01,3.0180E-01,2.6130E-01,
     &2.7070E-01,2.3930E-01,2.2940E-01,2.3430E-01,2.7310E-01,2.5660E-01,
     &2.4440E-01,3.2980E-01,3.2390E-01,
     &6.3480E-01,6.2270E-01,6.2060E-01,5.9690E-01,5.7030E-01,5.6000E-01,
     &4.9500E-01,4.1660E-01,3.8120E-01,3.4620E-01,3.0890E-01,2.7010E-01,
     &2.7540E-01,2.4530E-01,2.3200E-01,2.3680E-01,2.6430E-01,2.4720E-01,
     &2.4290E-01,3.3220E-01,3.3270E-01,
     &6.7290E-01,6.6080E-01,6.5760E-01,6.3370E-01,6.0660E-01,5.9250E-01,
     &5.3310E-01,4.2540E-01,4.1010E-01,3.7420E-01,3.3740E-01,3.0130E-01,
     &2.9580E-01,2.6790E-01,2.4670E-01,2.4900E-01,2.4010E-01,2.2420E-01,
     &2.3840E-01,3.3620E-01,3.5750E-01,
     &7.0930E-01,6.9790E-01,6.9410E-01,6.7160E-01,6.4550E-01,6.2880E-01,
     &5.7600E-01,4.4460E-01,4.4570E-01,4.0890E-01,3.7000E-01,3.3120E-01,
     &3.1770E-01,2.8390E-01,2.6710E-01,2.6080E-01,2.1600E-01,2.0090E-01,
     &2.1710E-01,3.0290E-01,3.4500E-01,
     &7.2530E-01,7.1460E-01,7.1080E-01,6.8950E-01,6.6460E-01,6.4700E-01,
     &5.9810E-01,4.5820E-01,4.6600E-01,4.2920E-01,3.8930E-01,3.4860E-01,
     &3.3260E-01,2.9530E-01,2.8090E-01,2.6910E-01,2.0830E-01,1.9340E-01,
     &2.0720E-01,2.8170E-01,3.3080E-01,
     &7.3900E-01,7.2930E-01,7.2550E-01,7.0560E-01,6.8220E-01,6.6440E-01,
     &6.1960E-01,4.7440E-01,4.8810E-01,4.5320E-01,4.1630E-01,3.7500E-01,
     &3.6210E-01,3.3150E-01,3.1510E-01,3.0760E-01,2.4070E-01,2.2640E-01,
     &2.5440E-01,3.5800E-01,4.1610E-01,
     &7.4970E-01,7.4070E-01,7.3720E-01,7.1880E-01,6.9690E-01,6.7920E-01,
     &6.3790E-01,4.9010E-01,5.0740E-01,4.7370E-01,4.3730E-01,3.9480E-01,
     &3.8190E-01,3.5170E-01,3.3900E-01,3.3340E-01,2.5280E-01,2.3810E-01,
     &2.6940E-01,3.7580E-01,4.5900E-01/
          data ((a_extx(i,j, 4),i=1,mbx),j=1, 8) / 
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02,
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02,
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02,
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02,
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02,
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02,
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02,
     &1.0000E+00,1.0881E+00,1.0896E+00,1.0224E+00,8.6890E-01,7.0250E-01,
     &4.3540E-01,1.8090E-01,7.2700E-02,3.9900E-02,3.3500E-02,3.5700E-02,
     &3.0400E-02,7.4000E-02,1.0210E-01,5.2200E-02,2.9400E-02,3.9700E-02,
     &3.8400E-02,2.1300E-02,1.7300E-02/
          data ((a_ssax(i,j, 4),i=1,mbx),j=1, 8) / 
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03,
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03,
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03,
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03,
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03,
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03,
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03,
     &8.9310E-01,9.7060E-01,9.7350E-01,9.7280E-01,9.7540E-01,9.7590E-01,
     &9.7370E-01,8.6250E-01,8.9820E-01,8.2730E-01,5.8190E-01,2.3260E-01,
     &9.4700E-02,4.0700E-02,1.7370E-01,7.0300E-02,3.7000E-02,1.4300E-02,
     &1.7700E-02,7.3000E-03,2.5000E-03/
          data ((a_asyx(i,j, 4),i=1,mbx),j=1, 8) / 
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03,
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03,
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03,
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03,
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03,
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03,
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03,
     &6.8320E-01,6.7040E-01,6.8200E-01,6.8760E-01,6.7870E-01,6.5990E-01,
     &6.0790E-01,5.1230E-01,3.7570E-01,2.5710E-01,2.0920E-01,1.4480E-01,
     &1.0520E-01,8.5400E-02,1.2690E-01,6.1500E-02,3.9800E-02,2.8200E-02,
     &2.4400E-02,1.2700E-02,6.4000E-03/
          data ((a_extx(i,j, 5),i=1,mbx),j=1, 8) / 
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02,
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02,
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02,
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02,
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02,
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02,
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02,
     &1.0000E+00,1.0600E+00,1.0974E+00,1.1690E+00,1.2392E+00,1.2663E+00,
     &1.1869E+00,7.7820E-01,4.6830E-01,3.2090E-01,2.3420E-01,1.5850E-01,
     &1.0540E-01,2.1650E-01,5.2670E-01,1.8550E-01,9.2800E-02,1.1020E-01,
     &1.1870E-01,5.7400E-02,4.2800E-02/
          data ((a_ssax(i,j, 5),i=1,mbx),j=1, 8) / 
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02,
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02,
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02,
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02,
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02,
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02,
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02,
     &8.2700E-01,9.3470E-01,9.4070E-01,9.4370E-01,9.5690E-01,9.6490E-01,
     &9.7280E-01,9.0960E-01,9.4990E-01,9.2860E-01,8.0620E-01,5.2540E-01,
     &3.0330E-01,1.4510E-01,3.8840E-01,2.9250E-01,1.9260E-01,8.9500E-02,
     &1.1340E-01,5.3100E-02,1.9300E-02/
          data ((a_asyx(i,j, 5),i=1,mbx),j=1, 8) / 
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02,
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02,
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02,
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02,
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02,
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02,
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02,
     &7.4620E-01,6.8120E-01,6.7320E-01,6.6850E-01,6.7300E-01,6.8120E-01,
     &6.8760E-01,7.0000E-01,6.3540E-01,5.4710E-01,5.0930E-01,4.2620E-01,
     &3.4260E-01,2.7870E-01,2.5290E-01,2.2100E-01,1.5450E-01,1.0950E-01,
     &9.2700E-02,5.1700E-02,2.6000E-02/
          data ((a_extx(i,j, 6),i=1,mbx),j=1, 8) / 
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01,
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01,
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01,
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01,
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01,
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01,
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01,
     &1.0000E+00,1.0248E+00,1.0484E+00,1.0790E+00,1.1217E+00,1.1692E+00,
     &1.2669E+00,1.3199E+00,1.2284E+00,1.0936E+00,8.7440E-01,5.9430E-01,
     &3.6080E-01,5.1800E-01,1.1942E+00,6.3780E-01,3.6290E-01,3.5270E-01,
     &4.9050E-01,2.1840E-01,1.2380E-01/
          data ((a_ssax(i,j, 6),i=1,mbx),j=1, 8) / 
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01,
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01,
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01,
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01,
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01,
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01,
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01,
     &7.5720E-01,8.8600E-01,8.9040E-01,8.9010E-01,9.0850E-01,9.2290E-01,
     &9.4370E-01,8.8720E-01,9.5150E-01,9.4230E-01,8.6960E-01,6.8960E-01,
     &5.0750E-01,2.6450E-01,4.7010E-01,5.0480E-01,4.3810E-01,2.7540E-01,
     &3.2640E-01,2.3680E-01,1.2140E-01/
          data ((a_asyx(i,j, 6),i=1,mbx),j=1, 8) / 
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01,
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01,
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01,
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01,
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01,
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01,
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01,
     &8.2300E-01,7.6200E-01,7.4870E-01,7.2990E-01,7.0180E-01,6.8260E-01,
     &6.6850E-01,7.4190E-01,7.2790E-01,6.8220E-01,6.9460E-01,6.8780E-01,
     &6.5440E-01,5.7740E-01,3.9700E-01,4.7750E-01,4.1940E-01,3.2620E-01,
     &2.1410E-01,1.7070E-01,1.0270E-01/
          data ((a_extx(i,j, 7),i=1,mbx),j=1, 8) / 
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01,
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01,
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01,
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01,
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01,
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01,
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01,
     &1.0000E+00,1.0173E+00,1.0284E+00,1.0455E+00,1.0664E+00,1.0872E+00,
     &1.1293E+00,1.2136E+00,1.3238E+00,1.3957E+00,1.3604E+00,1.2025E+00,
     &8.4140E-01,8.0750E-01,1.3309E+00,1.2281E+00,9.6750E-01,8.4500E-01,
     &1.1659E+00,8.1350E-01,4.9220E-01/
          data ((a_ssax(i,j, 7),i=1,mbx),j=1, 8) / 
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01,
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01,
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01,
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01,
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01,
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01,
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01,
     &6.8630E-01,8.1280E-01,8.2170E-01,8.1960E-01,8.4460E-01,8.6320E-01,
     &8.8960E-01,7.9570E-01,9.0770E-01,9.0370E-01,8.3880E-01,7.1450E-01,
     &6.1520E-01,3.5730E-01,4.9670E-01,5.6070E-01,5.6060E-01,4.2720E-01,
     &4.4590E-01,4.2400E-01,3.3870E-01/
          data ((a_asyx(i,j, 7),i=1,mbx),j=1, 8) / 
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01,
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01,
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01,
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01,
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01,
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01,
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01,
     &8.7490E-01,8.2220E-01,8.1440E-01,8.0450E-01,7.8270E-01,7.6320E-01,
     &7.2890E-01,7.5730E-01,7.1610E-01,6.8730E-01,7.4150E-01,8.0080E-01,
     &8.2080E-01,7.8490E-01,5.9870E-01,6.5580E-01,6.4610E-01,5.8540E-01,
     &3.6610E-01,3.2280E-01,2.6090E-01/
          data ((a_extx(i,j, 8),i=1,mbx),j=1, 8) / 
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00,
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00,
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00,
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00,
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00,
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00,
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00,
     &1.0000E+00,1.0119E+00,1.0176E+00,1.0284E+00,1.0414E+00,1.0533E+00,
     &1.0758E+00,1.1118E+00,1.1586E+00,1.2066E+00,1.2427E+00,1.2777E+00,
     &1.1671E+00,9.6500E-01,1.2530E+00,1.3243E+00,1.3175E+00,1.2112E+00,
     &1.3714E+00,1.3494E+00,1.1744E+00/
          data ((a_ssax(i,j, 8),i=1,mbx),j=1, 8) / 
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01,
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01,
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01,
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01,
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01,
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01,
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01,
     &6.2390E-01,7.1900E-01,7.3010E-01,7.2820E-01,7.5910E-01,7.8350E-01,
     &8.1960E-01,6.9750E-01,8.2660E-01,8.1560E-01,7.2480E-01,6.2310E-01,
     &6.0920E-01,4.2670E-01,5.3580E-01,5.2410E-01,5.4890E-01,4.8530E-01,
     &4.9030E-01,4.7880E-01,4.5400E-01/
          data ((a_asyx(i,j, 8),i=1,mbx),j=1, 8) / 
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01,
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01,
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01,
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01,
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01,
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01,
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01,
     &9.0970E-01,8.7160E-01,8.6630E-01,8.6160E-01,8.4440E-01,8.2960E-01,
     &8.0450E-01,8.4010E-01,7.6860E-01,7.3150E-01,7.7750E-01,8.4430E-01,
     &8.8520E-01,8.8460E-01,7.3260E-01,7.6880E-01,7.6740E-01,7.5550E-01,
     &5.7190E-01,4.9680E-01,4.3220E-01/
          data ((a_extx(i,j, 9),i=1,mbx),j=1, 1) / 
     &9.9922E-01,1.0296E+00,1.0437E+00,1.0594E+00,1.0833E+00,1.0967E+00,
     &8.7210E-01,7.9139E-01,8.1180E-01,6.7573E-01,4.9844E-01,4.2940E-01,
     &4.6489E-01,5.5411E-01,8.5494E-01,6.7740E-01,5.1355E-01,4.9093E-01,
     &4.9517E-01,4.2129E-01,3.5627E-01/
          data ((a_ssax(i,j, 9),i=1,mbx),j=1, 1) / 
     &7.2895E-01,7.6661E-01,7.8282E-01,7.9901E-01,8.2192E-01,8.5524E-01,
     &8.8248E-01,8.5907E-01,8.8197E-01,8.5374E-01,7.5607E-01,5.9142E-01,
     &6.5952E-01,5.2050E-01,5.8114E-01,6.3612E-01,6.3074E-01,6.3475E-01,
     &5.0204E-01,4.0565E-01,3.3524E-01/
          data ((a_asyx(i,j, 9),i=1,mbx),j=1, 1) / 
     &8.3174E-01,8.0280E-01,7.9340E-01,7.8083E-01,7.7725E-01,8.0018E-01,
     &9.0322E-01,8.8246E-01,8.5273E-01,8.5633E-01,8.7776E-01,8.6148E-01,
     &8.2834E-01,7.8919E-01,6.6570E-01,6.8081E-01,6.8858E-01,6.3866E-01,
     &5.7057E-01,4.9732E-01,3.4799E-01/
          data ((a_extx(i,j,10),i=1,mbx),j=1, 8) / 
     &1.0150E+00,6.1791E-01,4.8090E-01,3.7886E-01,2.5920E-01,1.4093E-01,
     &3.1993E-02,1.7177E-02,9.1517E-03,8.2079E-03,8.2556E-03,1.2808E-02,
     &1.8571E-02,3.5896E-02,2.4044E-02,8.1520E-03,8.2731E-03,2.2048E-02,
     &1.2847E-02,1.4265E-02,2.0084E-02,
     &1.0148E+00,6.2871E-01,4.9552E-01,3.9041E-01,2.6814E-01,1.4634E-01,
     &2.8858E-02,5.1701E-02,1.3204E-02,1.0635E-02,1.3074E-02,1.7836E-02,
     &1.6969E-02,3.1896E-02,2.4658E-02,1.8429E-02,3.2423E-02,3.8354E-02,
     &2.4071E-02,1.9470E-02,2.2210E-02,
     &1.0143E+00,6.3523E-01,5.0383E-01,3.9776E-01,2.7425E-01,1.5041E-01,
     &2.9666E-02,6.3147E-02,1.5142E-02,1.1670E-02,1.4461E-02,1.9492E-02,
     &1.6471E-02,2.9653E-02,2.3777E-02,2.1841E-02,4.0486E-02,4.4084E-02,
     &2.7821E-02,2.1010E-02,2.2650E-02,
     &1.0141E+00,6.4178E-01,5.1198E-01,4.0518E-01,2.8074E-01,1.5485E-01,
     &3.0804E-02,7.1958E-02,1.6949E-02,1.2580E-02,1.5502E-02,2.0721E-02,
     &1.6127E-02,2.7692E-02,2.2833E-02,2.4293E-02,4.6331E-02,4.8243E-02,
     &3.0542E-02,2.2067E-02,2.2868E-02,
     &1.0134E+00,6.5574E-01,5.2912E-01,4.2164E-01,2.9534E-01,1.6525E-01,
     &3.3864E-02,8.6285E-02,2.0664E-02,1.4378E-02,1.7210E-02,2.2639E-02,
     &1.5729E-02,2.4362E-02,2.1045E-02,2.7763E-02,5.4686E-02,5.4216E-02,
     &3.4460E-02,2.3502E-02,2.3081E-02,
     &1.0122E+00,6.7340E-01,5.5064E-01,4.4304E-01,3.1507E-01,1.7982E-01,
     &3.8560E-02,1.0004E-01,2.5392E-02,1.6629E-02,1.8984E-02,2.4444E-02,
     &1.5647E-02,2.1541E-02,1.9427E-02,3.0354E-02,6.0997E-02,5.8783E-02,
     &3.7464E-02,2.4543E-02,2.3176E-02,
     &1.0106E+00,6.9979E-01,5.8278E-01,4.7618E-01,3.4655E-01,2.0409E-01,
     &4.7072E-02,1.1737E-01,3.3045E-02,2.0317E-02,2.1563E-02,2.6871E-02,
     &1.6122E-02,1.9250E-02,1.8093E-02,3.2706E-02,6.6680E-02,6.3081E-02,
     &4.0319E-02,2.5570E-02,2.3311E-02,
     &1.0097E+00,7.1872E-01,6.0606E-01,5.0085E-01,3.7049E-01,2.2332E-01,
     &5.4336E-02,1.2934E-01,3.9258E-02,2.3394E-02,2.3630E-02,2.8740E-02,
     &1.6843E-02,1.8548E-02,1.7717E-02,3.3991E-02,6.9669E-02,6.5530E-02,
     &4.1954E-02,2.6236E-02,2.3552E-02/
          data ((a_ssax(i,j,10),i=1,mbx),j=1, 8) / 
     &9.6328E-01,9.4320E-01,9.1732E-01,8.8655E-01,8.4215E-01,7.6880E-01,
     &7.4787E-01,6.1961E-01,7.5020E-01,3.9960E-01,2.0728E-01,9.2011E-02,
     &1.3374E-02,3.5848E-02,4.4068E-02,3.9432E-02,1.4189E-02,4.9775E-03,
     &3.9649E-03,8.3962E-04,1.0665E-04,
     &9.7763E-01,9.6518E-01,9.4924E-01,9.2964E-01,9.0153E-01,8.5395E-01,
     &8.3380E-01,4.5443E-01,7.9269E-01,4.3731E-01,2.2766E-01,9.1515E-02,
     &3.2194E-02,3.6172E-02,4.0941E-02,1.8153E-02,4.2395E-03,3.6683E-03,
     &3.1640E-03,1.0594E-03,1.4848E-04,
     &9.8197E-01,9.7200E-01,9.5931E-01,9.4359E-01,9.2118E-01,8.8288E-01,
     &8.6241E-01,4.5635E-01,8.1051E-01,4.5957E-01,2.4823E-01,9.9772E-02,
     &4.3172E-02,3.8683E-02,4.1196E-02,1.5658E-02,3.9129E-03,3.7795E-03,
     &3.2678E-03,1.2191E-03,1.7490E-04,
     &9.8505E-01,9.7696E-01,9.6661E-01,9.5363E-01,9.3549E-01,9.0412E-01,
     &8.8351E-01,4.6468E-01,8.2512E-01,4.8079E-01,2.6800E-01,1.0883E-01,
     &5.4111E-02,4.2446E-02,4.2088E-02,1.4412E-02,3.9479E-03,4.0307E-03,
     &3.4752E-03,1.3941E-03,2.0307E-04,
     &9.8949E-01,9.8399E-01,9.7698E-01,9.6821E-01,9.5617E-01,9.3486E-01,
     &9.1409E-01,4.8774E-01,8.4980E-01,5.2298E-01,3.0786E-01,1.2971E-01,
     &7.7876E-02,5.2661E-02,4.5369E-02,1.3462E-02,4.4731E-03,4.8079E-03,
     &4.1157E-03,1.8233E-03,2.8235E-04,
     &9.9283E-01,9.8930E-01,9.8478E-01,9.7903E-01,9.7156E-01,9.5779E-01,
     &9.3724E-01,5.1714E-01,8.7284E-01,5.7071E-01,3.5413E-01,1.5790E-01,
     &1.0894E-01,6.8860E-02,5.1458E-02,1.3769E-02,5.5521E-03,6.1020E-03,
     &5.2029E-03,2.4885E-03,4.1027E-04,
     &9.9561E-01,9.9366E-01,9.9110E-01,9.8797E-01,9.8404E-01,9.7607E-01,
     &9.5643E-01,5.5620E-01,8.9723E-01,6.3179E-01,4.1628E-01,2.0224E-01,
     &1.5754E-01,9.8481E-02,6.3885E-02,1.5773E-02,7.7235E-03,8.6017E-03,
     &7.3697E-03,3.7949E-03,6.6765E-04,
     &9.9685E-01,9.9552E-01,9.9380E-01,9.9161E-01,9.8908E-01,9.8340E-01,
     &9.6454E-01,5.8043E-01,9.1016E-01,6.6921E-01,4.5632E-01,2.3519E-01,
     &1.9382E-01,1.2312E-01,7.5135E-02,1.8019E-02,9.6672E-03,1.0839E-02,
     &9.3702E-03,5.0204E-03,9.1518E-04/
          data ((a_asyx(i,j,10),i=1,mbx),j=1, 8) / 
     &6.1426E-01,5.8382E-01,5.7263E-01,5.5378E-01,5.2735E-01,4.8140E-01,
     &4.0542E-01,3.3579E-01,3.0530E-01,2.5707E-01,2.2306E-01,1.9125E-01,
     &1.5387E-01,1.5193E-01,1.5429E-01,1.2512E-01,9.2786E-02,7.6169E-02,
     &6.3628E-02,3.4725E-02,1.4050E-02,
     &6.7224E-01,6.4155E-01,6.2831E-01,6.0982E-01,5.8283E-01,5.3425E-01,
     &4.6252E-01,3.7269E-01,3.4843E-01,2.9686E-01,2.5736E-01,2.2474E-01,
     &1.9054E-01,1.7737E-01,1.6672E-01,1.3275E-01,9.8446E-02,8.1867E-02,
     &6.8653E-02,4.2772E-02,1.8800E-02,
     &6.9044E-01,6.6058E-01,6.4725E-01,6.2941E-01,6.0294E-01,5.5503E-01,
     &4.8437E-01,3.8854E-01,3.6699E-01,3.1427E-01,2.7358E-01,2.3991E-01,
     &2.0619E-01,1.8929E-01,1.7527E-01,1.3922E-01,1.0300E-01,8.6225E-02,
     &7.2570E-02,4.6878E-02,2.1198E-02,
     &7.0416E-01,6.7557E-01,6.6226E-01,6.4440E-01,6.1858E-01,5.7106E-01,
     &5.0366E-01,4.0222E-01,3.8232E-01,3.2936E-01,2.8691E-01,2.5261E-01,
     &2.2047E-01,2.0020E-01,1.8367E-01,1.4541E-01,1.0787E-01,9.0640E-02,
     &7.6535E-02,5.0812E-02,2.3440E-02,
     &7.2538E-01,6.9857E-01,6.8605E-01,6.6947E-01,6.4475E-01,5.9924E-01,
     &5.3632E-01,4.2746E-01,4.1090E-01,3.5776E-01,3.1326E-01,2.7824E-01,
     &2.4588E-01,2.2245E-01,2.0169E-01,1.5968E-01,1.1806E-01,1.0010E-01,
     &8.5098E-02,5.8785E-02,2.8243E-02,
     &7.4328E-01,7.1936E-01,7.0804E-01,6.9263E-01,6.6953E-01,6.2708E-01,
     &5.7012E-01,4.5435E-01,4.4113E-01,3.8883E-01,3.4324E-01,3.0677E-01,
     &2.7553E-01,2.4839E-01,2.2414E-01,1.7770E-01,1.3154E-01,1.1186E-01,
     &9.6225E-02,6.8711E-02,3.4084E-02,
     &7.6122E-01,7.4115E-01,7.3189E-01,7.1826E-01,6.9811E-01,6.5947E-01,
     &6.1157E-01,4.8931E-01,4.8026E-01,4.2948E-01,3.8364E-01,3.4542E-01,
     &3.1467E-01,2.8524E-01,2.5708E-01,2.0453E-01,1.5202E-01,1.2997E-01,
     &1.1262E-01,8.3565E-02,4.3110E-02,
     &7.7014E-01,7.5283E-01,7.4413E-01,7.3247E-01,7.1454E-01,6.7853E-01,
     &6.3608E-01,5.1156E-01,5.0445E-01,4.5587E-01,4.1082E-01,3.7118E-01,
     &3.4122E-01,3.1061E-01,2.8029E-01,2.2422E-01,1.6724E-01,1.4358E-01,
     &1.2478E-01,9.4542E-02,5.0178E-02/
          data ((a_extx(i,j,11),i=1,mbx),j=1, 1) / 
     &1.0172E+00,6.5847E-01,5.5591E-01,4.6966E-01,3.7439E-01,2.7204E-01,
     &1.7278E-01,1.2788E-01,1.0921E-01,8.5554E-02,7.1995E-02,6.0887E-02,
     &5.2130E-02,4.5656E-02,4.0025E-02,3.3922E-02,2.7694E-02,2.2542E-02,
     &1.7022E-02,1.2129E-02,7.0927E-03/
          data ((a_ssax(i,j,11),i=1,mbx),j=1, 1) / 
     &2.1021E-01,1.5088E-01,1.2609E-01,1.0276E-01,7.4536E-02,4.2561E-02,
     &1.5785E-02,7.1827E-03,4.8636E-03,2.7380E-03,1.7953E-03,1.1923E-03,
     &8.1898E-04,6.1160E-04,4.4812E-04,2.9184E-04,1.7606E-04,1.0485E-04,
     &5.4897E-05,2.3084E-05,5.5298E-06/
          data ((a_asyx(i,j,11),i=1,mbx),j=1, 1) / 
     &3.3751E-01,2.8080E-01,2.5707E-01,2.3235E-01,1.9907E-01,1.5419E-01,
     &9.7544E-02,6.6095E-02,5.3799E-02,3.9174E-02,3.0875E-02,2.4148E-02,
     &1.9131E-02,1.5810E-02,1.2891E-02,9.7270E-03,6.9153E-03,4.8580E-03,
     &3.1206E-03,1.7595E-03,1.1410E-03/
          data ((a_extx(i,j,12),i=1,mbx),j=1, 8) / 
     &9.9774E-01,9.9713E-01,9.7636E-01,9.3870E-01,8.6437E-01,7.0488E-01,
     &3.9146E-01,3.2681E-01,2.2649E-01,1.4927E-01,7.9568E-02,7.2471E-02,
     &4.3666E-02,5.5509E-02,4.4742E-02,2.2412E-02,1.3653E-02,2.6535E-02,
     &2.9120E-02,3.1579E-02,1.4724E-01,
     &9.9890E-01,1.0288E+00,1.0279E+00,1.0147E+00,9.7271E-01,8.5240E-01,
     &5.1090E-01,5.6617E-01,3.9389E-01,2.5465E-01,1.6936E-01,1.6445E-01,
     &1.0667E-01,9.2345E-02,7.2837E-02,8.1398E-02,1.4430E-01,1.4946E-01,
     &1.0806E-01,7.1147E-02,1.0541E-01,
     &9.9844E-01,1.0349E+00,1.0413E+00,1.0358E+00,1.0049E+00,9.0325E-01,
     &5.6733E-01,6.3575E-01,4.5667E-01,2.9927E-01,2.0351E-01,1.9812E-01,
     &1.3137E-01,1.0919E-01,8.5142E-02,9.7886E-02,1.7651E-01,1.8275E-01,
     &1.3210E-01,8.4730E-02,1.0360E-01,
     &9.9910E-01,1.0419E+00,1.0522E+00,1.0529E+00,1.0336E+00,9.4696E-01,
     &6.2131E-01,6.9557E-01,5.1476E-01,3.4246E-01,2.3638E-01,2.3028E-01,
     &1.5560E-01,1.2622E-01,9.7507E-02,1.1253E-01,2.0398E-01,2.1182E-01,
     &1.5406E-01,9.7721E-02,1.0555E-01,
     &9.9843E-01,1.0478E+00,1.0648E+00,1.0753E+00,1.0742E+00,1.0219E+00,
     &7.3259E-01,8.0668E-01,6.3359E-01,4.3691E-01,3.0893E-01,3.0089E-01,
     &2.1092E-01,1.6646E-01,1.2647E-01,1.4266E-01,2.5738E-01,2.7023E-01,
     &2.0059E-01,1.2671E-01,1.1551E-01,
     &9.9900E-01,1.0483E+00,1.0692E+00,1.0887E+00,1.1055E+00,1.0886E+00,
     &8.6319E-01,9.2381E-01,7.7502E-01,5.6103E-01,4.0734E-01,3.9656E-01,
     &2.8982E-01,2.2616E-01,1.6963E-01,1.8162E-01,3.2143E-01,3.4261E-01,
     &2.6244E-01,1.6806E-01,1.3628E-01,
     &9.9942E-01,1.0386E+00,1.0607E+00,1.0844E+00,1.1134E+00,1.1407E+00,
     &1.0321E+00,1.0595E+00,9.6858E-01,7.5784E-01,5.7448E-01,5.5993E-01,
     &4.3482E-01,3.4188E-01,2.5515E-01,2.4901E-01,4.2076E-01,4.5854E-01,
     &3.7041E-01,2.4690E-01,1.8297E-01,
     &9.9983E-01,1.0308E+00,1.0508E+00,1.0734E+00,1.1046E+00,1.1492E+00,
     &1.1291E+00,1.1300E+00,1.0914E+00,9.0902E-01,7.1611E-01,7.0005E-01,
     &5.6944E-01,4.5610E-01,3.4296E-01,3.1188E-01,5.0308E-01,5.5652E-01,
     &4.6950E-01,3.2639E-01,2.3429E-01/
          data ((a_ssax(i,j,12),i=1,mbx),j=1, 8) / 
     &1.0000E+00,9.9999E-01,9.9994E-01,9.9901E-01,9.9772E-01,9.9565E-01,
     &9.8217E-01,9.7618E-01,9.8971E-01,9.8251E-01,9.4705E-01,9.2221E-01,
     &8.5193E-01,7.5545E-01,8.1298E-01,7.5374E-01,5.2858E-01,2.9786E-01,
     &1.7493E-01,6.1698E-02,1.3827E-02,
     &1.0000E+00,9.9999E-01,9.9997E-01,9.9960E-01,9.9924E-01,9.9759E-01,
     &9.8703E-01,7.7987E-01,9.7402E-01,9.2075E-01,8.0680E-01,6.9487E-01,
     &7.0719E-01,6.4250E-01,5.4462E-01,2.1003E-01,9.8249E-02,1.2180E-01,
     &1.2715E-01,8.7582E-02,1.9485E-02,
     &1.0000E+00,9.9999E-01,9.9998E-01,9.9972E-01,9.9935E-01,9.9780E-01,
     &9.8711E-01,7.6972E-01,9.7260E-01,9.1676E-01,8.0441E-01,6.8977E-01,
     &7.0925E-01,6.4277E-01,5.3088E-01,1.9956E-01,1.0700E-01,1.3118E-01,
     &1.3830E-01,1.0116E-01,2.4429E-02,
     &1.0000E+00,9.9999E-01,9.9998E-01,9.9978E-01,9.9948E-01,9.9789E-01,
     &9.8709E-01,7.6397E-01,9.7169E-01,9.1498E-01,8.0517E-01,6.9015E-01,
     &7.1445E-01,6.4735E-01,5.2830E-01,1.9840E-01,1.1626E-01,1.4105E-01,
     &1.4952E-01,1.1387E-01,2.9462E-02,
     &1.0000E+00,9.9999E-01,9.9998E-01,9.9983E-01,9.9966E-01,9.9790E-01,
     &9.8673E-01,7.5578E-01,9.7016E-01,9.1339E-01,8.0944E-01,6.9644E-01,
     &7.2788E-01,6.6219E-01,5.3717E-01,2.0717E-01,1.3672E-01,1.6313E-01,
     &1.7409E-01,1.4083E-01,4.1270E-02,
     &1.0000E+00,9.9999E-01,9.9998E-01,9.9983E-01,9.9972E-01,9.9783E-01,
     &9.8573E-01,7.4758E-01,9.6826E-01,9.1235E-01,8.1563E-01,7.0636E-01,
     &7.4378E-01,6.8256E-01,5.5877E-01,2.2695E-01,1.6262E-01,1.9077E-01,
     &2.0478E-01,1.7466E-01,5.8012E-02,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9989E-01,9.9958E-01,9.9750E-01,
     &9.8325E-01,7.3321E-01,9.6419E-01,9.0934E-01,8.2275E-01,7.1857E-01,
     &7.6236E-01,7.1013E-01,5.9610E-01,2.6471E-01,2.0283E-01,2.3314E-01,
     &2.5118E-01,2.2667E-01,9.1535E-02,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9989E-01,9.9958E-01,9.9700E-01,
     &9.8013E-01,7.1865E-01,9.5942E-01,9.0433E-01,8.2510E-01,7.2363E-01,
     &7.7139E-01,7.2664E-01,6.2319E-01,2.9719E-01,2.3506E-01,2.6622E-01,
     &2.8653E-01,2.6699E-01,1.2362E-01/
          data ((a_asyx(i,j,12),i=1,mbx),j=1, 8) / 
     &6.9246E-01,6.9732E-01,7.0235E-01,7.0542E-01,7.0656E-01,7.0369E-01,
     &7.0444E-01,6.2676E-01,6.1863E-01,5.8232E-01,5.7542E-01,5.3044E-01,
     &5.0251E-01,4.6313E-01,4.3437E-01,4.0253E-01,3.5387E-01,3.0690E-01,
     &2.5262E-01,1.7728E-01,5.4750E-02,
     &7.7098E-01,7.7336E-01,7.7714E-01,7.7917E-01,7.8225E-01,7.8443E-01,
     &7.9463E-01,7.2103E-01,7.1112E-01,6.9646E-01,6.8797E-01,6.4584E-01,
     &6.2226E-01,5.9244E-01,5.6446E-01,5.0419E-01,4.0733E-01,3.5148E-01,
     &3.0594E-01,2.4490E-01,1.2813E-01,
     &7.7827E-01,7.8145E-01,7.8331E-01,7.8577E-01,7.9005E-01,7.9280E-01,
     &8.0771E-01,7.3653E-01,7.2645E-01,7.1690E-01,7.1050E-01,6.7055E-01,
     &6.4890E-01,6.2252E-01,5.9624E-01,5.3413E-01,4.3145E-01,3.7289E-01,
     &3.2699E-01,2.6639E-01,1.4848E-01,
     &7.8397E-01,7.8511E-01,7.8682E-01,7.8922E-01,7.9272E-01,7.9784E-01,
     &8.1639E-01,7.4690E-01,7.3741E-01,7.3242E-01,7.2841E-01,6.8932E-01,
     &6.7019E-01,6.4565E-01,6.2145E-01,5.5909E-01,4.5236E-01,3.9211E-01,
     &3.4536E-01,2.8468E-01,1.6579E-01,
     &7.9332E-01,7.9011E-01,7.9137E-01,7.9376E-01,7.9796E-01,8.0350E-01,
     &8.2730E-01,7.6557E-01,7.5369E-01,7.5635E-01,7.5805E-01,7.2185E-01,
     &7.0471E-01,6.8590E-01,6.6556E-01,6.0555E-01,4.9474E-01,4.3032E-01,
     &3.8192E-01,3.2069E-01,2.0024E-01,
     &8.0086E-01,7.9510E-01,7.9515E-01,7.9613E-01,7.9976E-01,8.0643E-01,
     &8.3566E-01,7.8194E-01,7.6744E-01,7.7768E-01,7.8515E-01,7.5260E-01,
     &7.3881E-01,7.2536E-01,7.1038E-01,6.5548E-01,5.4341E-01,4.7557E-01,
     &4.2487E-01,3.6289E-01,2.3892E-01,
     &8.1358E-01,8.0466E-01,8.0206E-01,7.9940E-01,8.0085E-01,8.0673E-01,
     &8.4020E-01,8.0124E-01,7.7925E-01,7.9911E-01,8.1586E-01,7.8826E-01,
     &7.7859E-01,7.7231E-01,7.6510E-01,7.2206E-01,6.1290E-01,5.4244E-01,
     &4.8831E-01,4.2488E-01,2.9727E-01,
     &8.2440E-01,8.1299E-01,8.0943E-01,8.0416E-01,8.0272E-01,8.0654E-01,
     &8.4083E-01,8.1400E-01,7.8437E-01,8.1123E-01,8.3390E-01,8.1057E-01,
     &8.0331E-01,8.0112E-01,7.9951E-01,7.6698E-01,6.6424E-01,5.9321E-01,
     &5.3759E-01,4.7358E-01,3.4486E-01/
          data ((a_extx(i,j,13),i=1,mbx),j=1, 8) / 
     &9.9797E-01,1.0139E+00,1.0203E+00,1.0353E+00,1.0546E+00,1.0836E+00,
     &1.1625E+00,1.2083E+00,1.2417E+00,1.2674E+00,1.2230E+00,1.2226E+00,
     &1.1311E+00,1.1868E+00,1.1927E+00,1.0031E+00,7.7640E-01,8.9815E-01,
     &8.7337E-01,7.0428E-01,1.0874E+00,
     &9.9928E-01,1.0132E+00,1.0187E+00,1.0256E+00,1.0352E+00,1.0617E+00,
     &1.1337E+00,1.1374E+00,1.1898E+00,1.2307E+00,1.1995E+00,1.2114E+00,
     &1.1899E+00,1.1664E+00,1.0884E+00,8.4175E-01,8.7250E-01,1.0017E+00,
     &1.0178E+00,9.0297E-01,8.4246E-01,
     &9.9976E-01,1.0122E+00,1.0155E+00,1.0244E+00,1.0366E+00,1.0555E+00,
     &1.1199E+00,1.1230E+00,1.1741E+00,1.2174E+00,1.1964E+00,1.2102E+00,
     &1.2031E+00,1.1757E+00,1.0937E+00,8.5085E-01,9.0632E-01,1.0298E+00,
     &1.0527E+00,9.5285E-01,8.4841E-01,
     &1.0003E+00,1.0132E+00,1.0163E+00,1.0224E+00,1.0333E+00,1.0539E+00,
     &1.1125E+00,1.1148E+00,1.1614E+00,1.2075E+00,1.1942E+00,1.2095E+00,
     &1.2133E+00,1.1864E+00,1.1051E+00,8.6612E-01,9.3290E-01,1.0522E+00,
     &1.0807E+00,9.9403E-01,8.6639E-01,
     &1.0000E+00,1.0113E+00,1.0154E+00,1.0216E+00,1.0317E+00,1.0449E+00,
     &1.0959E+00,1.0988E+00,1.1383E+00,1.1830E+00,1.1821E+00,1.2000E+00,
     &1.2189E+00,1.2007E+00,1.1298E+00,9.0103E-01,9.7329E-01,1.0837E+00,
     &1.1216E+00,1.0619E+00,9.1508E-01,
     &9.9973E-01,1.0086E+00,1.0118E+00,1.0158E+00,1.0237E+00,1.0390E+00,
     &1.0777E+00,1.0832E+00,1.1141E+00,1.1536E+00,1.1627E+00,1.1805E+00,
     &1.2094E+00,1.2046E+00,1.1534E+00,9.4220E-01,1.0062E+00,1.1068E+00,
     &1.1528E+00,1.1227E+00,9.8032E-01,
     &9.9970E-01,1.0055E+00,1.0105E+00,1.0149E+00,1.0218E+00,1.0337E+00,
     &1.0593E+00,1.0681E+00,1.0885E+00,1.1191E+00,1.1322E+00,1.1498E+00,
     &1.1813E+00,1.1926E+00,1.1720E+00,9.9745E-01,1.0409E+00,1.1254E+00,
     &1.1779E+00,1.1825E+00,1.0763E+00,
     &9.9973E-01,1.0054E+00,1.0081E+00,1.0118E+00,1.0176E+00,1.0272E+00,
     &1.0486E+00,1.0573E+00,1.0726E+00,1.0967E+00,1.1086E+00,1.1241E+00,
     &1.1522E+00,1.1692E+00,1.1665E+00,1.0267E+00,1.0549E+00,1.1274E+00,
     &1.1797E+00,1.2033E+00,1.1304E+00/
          data ((a_ssax(i,j,13),i=1,mbx),j=1, 8) / 
     &1.0000E+00,9.9992E-01,9.9920E-01,9.9194E-01,9.8260E-01,9.7273E-01,
     &9.3647E-01,9.2600E-01,9.7471E-01,9.7096E-01,9.5268E-01,9.2729E-01,
     &9.1586E-01,8.3705E-01,8.7999E-01,9.0689E-01,8.6681E-01,6.9159E-01,
     &5.9059E-01,4.5951E-01,2.5587E-01,
     &1.0000E+00,9.9994E-01,9.9970E-01,9.9686E-01,9.9369E-01,9.8231E-01,
     &9.3173E-01,6.4052E-01,9.1503E-01,8.4984E-01,8.0492E-01,7.1149E-01,
     &7.6857E-01,7.4827E-01,7.3043E-01,5.3098E-01,3.9767E-01,4.2398E-01,
     &4.4671E-01,4.3396E-01,2.6693E-01,
     &1.0000E+00,9.9993E-01,9.9974E-01,9.9751E-01,9.9478E-01,9.8296E-01,
     &9.2694E-01,6.2179E-01,9.0168E-01,8.2865E-01,7.8585E-01,6.8801E-01,
     &7.4743E-01,7.3219E-01,7.0977E-01,5.0868E-01,4.0059E-01,4.2489E-01,
     &4.4499E-01,4.3794E-01,2.8921E-01,
     &1.0000E+00,9.9993E-01,9.9977E-01,9.9787E-01,9.9549E-01,9.8325E-01,
     &9.2143E-01,6.0954E-01,8.9110E-01,8.1185E-01,7.7128E-01,6.7114E-01,
     &7.3140E-01,7.1970E-01,6.9538E-01,4.9798E-01,4.0643E-01,4.2856E-01,
     &4.4608E-01,4.4203E-01,3.0837E-01,
     &1.0000E+00,9.9993E-01,9.9980E-01,9.9838E-01,9.9647E-01,9.8314E-01,
     &9.0911E-01,5.9141E-01,8.7058E-01,7.8122E-01,7.4514E-01,6.4325E-01,
     &7.0291E-01,6.9675E-01,6.7258E-01,4.8926E-01,4.2039E-01,4.3861E-01,
     &4.5149E-01,4.5038E-01,3.4116E-01,
     &1.0000E+00,9.9996E-01,9.9996E-01,9.9864E-01,9.9754E-01,9.8172E-01,
     &8.9248E-01,5.7673E-01,8.4726E-01,7.4800E-01,7.1691E-01,6.1585E-01,
     &6.7230E-01,6.7095E-01,6.5029E-01,4.8850E-01,4.3670E-01,4.5114E-01,
     &4.5949E-01,4.5929E-01,3.7245E-01,
     &1.0000E+00,9.9993E-01,9.9988E-01,9.9913E-01,9.9774E-01,9.7909E-01,
     &8.6380E-01,5.6241E-01,8.1276E-01,7.0242E-01,6.7758E-01,5.8211E-01,
     &6.2973E-01,6.3270E-01,6.1922E-01,4.9259E-01,4.5814E-01,4.6866E-01,
     &4.7142E-01,4.7020E-01,4.0721E-01,
     &1.0000E+00,9.9993E-01,9.9988E-01,9.9913E-01,9.9782E-01,9.7613E-01,
     &8.3854E-01,5.5526E-01,7.8539E-01,6.7030E-01,6.4942E-01,5.6143E-01,
     &6.0007E-01,6.0392E-01,5.9521E-01,4.9590E-01,4.7228E-01,4.8071E-01,
     &4.8010E-01,4.7694E-01,4.2824E-01/
          data ((a_asyx(i,j,13),i=1,mbx),j=1, 8) / 
     &7.9636E-01,7.8815E-01,7.8598E-01,7.8072E-01,7.7372E-01,7.6317E-01,
     &7.6924E-01,7.2047E-01,7.1600E-01,7.1314E-01,7.7024E-01,7.4107E-01,
     &7.6548E-01,7.1858E-01,6.8148E-01,7.1765E-01,7.4503E-01,6.7737E-01,
     &6.2013E-01,5.6800E-01,2.9397E-01,
     &8.4691E-01,8.4262E-01,8.3887E-01,8.3683E-01,8.3288E-01,8.2417E-01,
     &8.2315E-01,8.6514E-01,7.8440E-01,8.1160E-01,8.5632E-01,8.5195E-01,
     &8.5032E-01,8.4737E-01,8.5632E-01,8.8915E-01,8.5564E-01,7.9678E-01,
     &7.3861E-01,6.7890E-01,5.2940E-01,
     &8.5059E-01,8.4829E-01,8.4857E-01,8.4303E-01,8.3862E-01,8.3189E-01,
     &8.3391E-01,8.7907E-01,7.9319E-01,8.2310E-01,8.6477E-01,8.6392E-01,
     &8.6038E-01,8.6182E-01,8.7422E-01,9.0291E-01,8.6587E-01,8.1203E-01,
     &7.5827E-01,7.0138E-01,5.7132E-01,
     &8.5701E-01,8.5213E-01,8.5139E-01,8.5049E-01,8.4630E-01,8.3836E-01,
     &8.4149E-01,8.8848E-01,8.0189E-01,8.3134E-01,8.7036E-01,8.7221E-01,
     &8.6662E-01,8.7116E-01,8.8548E-01,9.1218E-01,8.7436E-01,8.2407E-01,
     &7.7355E-01,7.1900E-01,6.0188E-01,
     &8.6038E-01,8.5820E-01,8.6213E-01,8.5693E-01,8.5076E-01,8.5003E-01,
     &8.5439E-01,9.0498E-01,8.1481E-01,8.4495E-01,8.7962E-01,8.8499E-01,
     &8.7769E-01,8.8517E-01,9.0140E-01,9.2593E-01,8.8823E-01,8.4371E-01,
     &7.9910E-01,7.4884E-01,6.5232E-01,
     &8.6292E-01,8.6425E-01,8.6187E-01,8.6283E-01,8.6197E-01,8.5812E-01,
     &8.6606E-01,9.2109E-01,8.3169E-01,8.5991E-01,8.8888E-01,8.9847E-01,
     &8.8850E-01,8.9682E-01,9.1410E-01,9.3749E-01,9.0147E-01,8.6253E-01,
     &8.2393E-01,7.7852E-01,6.9618E-01,
     &8.6798E-01,8.6813E-01,8.6374E-01,8.6570E-01,8.6556E-01,8.6744E-01,
     &8.8398E-01,9.3877E-01,8.5468E-01,8.8257E-01,9.0383E-01,9.1695E-01,
     &9.0423E-01,9.1113E-01,9.2757E-01,9.5039E-01,9.1800E-01,8.8428E-01,
     &8.5393E-01,8.1571E-01,7.5010E-01,
     &8.6708E-01,8.7028E-01,8.6278E-01,8.6756E-01,8.7025E-01,8.7314E-01,
     &8.9468E-01,9.4954E-01,8.7191E-01,9.0082E-01,9.1657E-01,9.3136E-01,
     &9.1763E-01,9.2214E-01,9.3650E-01,9.5777E-01,9.2770E-01,8.9740E-01,
     &8.7220E-01,8.3953E-01,7.8342E-01/
          data ((a_extx(i,j,14),i=1,mbx),j=1, 1) / 
     &6.9698E-01,4.9756E-01,4.1783E-01,3.4003E-01,2.4671E-01,1.4225E-01,
     &5.0161E-02,2.6827E-02,1.3992E-02,8.2607E-03,1.0044E-02,1.2958E-02,
     &1.6393E-02,3.1302E-02,2.8109E-02,2.5215E-02,1.7030E-02,1.9879E-02,
     &1.6968E-02,9.3761E-03,9.1069E-03/
          data ((a_ssax(i,j,14),i=1,mbx),j=1, 1) / 
     &9.6476E-01,9.7665E-01,9.7649E-01,9.7578E-01,9.7033E-01,9.5511E-01,
     &8.7820E-01,6.7073E-01,7.6614E-01,5.9135E-01,2.4477E-01,1.1046E-01,
     &2.8060E-02,1.2796E-02,9.2663E-02,2.8983E-02,1.5458E-02,4.0086E-03,
     &7.2019E-03,3.9128E-03,4.4039E-04/
          data ((a_asyx(i,j,14),i=1,mbx),j=1, 1) / 
     &6.6487E-01,6.4026E-01,6.2787E-01,6.1266E-01,5.8755E-01,5.4049E-01,
     &4.5271E-01,3.7422E-01,3.3594E-01,2.7628E-01,2.3241E-01,1.9198E-01,
     &1.5106E-01,1.1805E-01,1.5735E-01,1.0891E-01,8.3801E-02,5.3389E-02,
     &5.1486E-02,3.6299E-02,1.2601E-02/
          data ((a_extx(i,j,15),i=1,mbx),j=1, 1) / 
     &9.9838E-01,1.0540E+00,1.0772E+00,1.0972E+00,1.1143E+00,1.0965E+00,
     &9.4325E-01,7.4010E-01,6.2029E-01,4.5560E-01,3.4707E-01,2.8779E-01,
     &1.9961E-01,2.5650E-01,6.0463E-01,3.3909E-01,2.2773E-01,1.7900E-01,
     &2.2335E-01,1.2184E-01,6.9897E-02/
          data ((a_ssax(i,j,15),i=1,mbx),j=1, 1) / 
     &8.7115E-01,9.2702E-01,9.3506E-01,9.4258E-01,9.4546E-01,9.4631E-01,
     &9.2960E-01,8.7278E-01,9.3666E-01,9.1319E-01,7.7964E-01,6.4459E-01,
     &3.7664E-01,1.8829E-01,4.5051E-01,3.7515E-01,3.3984E-01,1.7663E-01,
     &2.6892E-01,2.3450E-01,6.4174E-02/
          data ((a_asyx(i,j,15),i=1,mbx),j=1, 1) / 
     &7.3720E-01,7.0477E-01,6.9780E-01,6.9255E-01,6.8920E-01,6.8754E-01,
     &6.8935E-01,6.8977E-01,6.7345E-01,6.5874E-01,6.5772E-01,6.3564E-01,
     &6.1939E-01,5.5002E-01,3.7336E-01,4.4154E-01,4.2173E-01,3.6776E-01,
     &2.4941E-01,2.3102E-01,1.6118E-01/
          data ((a_extx(i,j,16),i=1,mbx),j=1, 1) / 
     &9.9964E-01,1.0138E+00,1.0214E+00,1.0297E+00,1.0421E+00,1.0682E+00,
     &1.1205E+00,1.1689E+00,1.2014E+00,1.2329E+00,1.2238E+00,1.1909E+00,
     &1.0192E+00,8.5570E-01,1.2583E+00,1.2152E+00,1.1508E+00,9.8920E-01,
     &1.2235E+00,1.1195E+00,8.3446E-01/
          data ((a_ssax(i,j,16),i=1,mbx),j=1, 1) / 
     &6.6011E-01,7.4376E-01,7.5934E-01,7.7635E-01,7.8222E-01,7.8544E-01,
     &7.5867E-01,7.0061E-01,8.2499E-01,8.2126E-01,7.0317E-01,6.4004E-01,
     &5.5809E-01,4.1147E-01,5.2148E-01,4.9518E-01,5.0594E-01,4.3680E-01,
     &4.7540E-01,4.6953E-01,3.9216E-01/
          data ((a_asyx(i,j,16),i=1,mbx),j=1, 1) / 
     &8.9725E-01,8.5905E-01,8.5055E-01,8.3949E-01,8.2909E-01,8.1138E-01,
     &7.9165E-01,8.0335E-01,7.5589E-01,7.5605E-01,8.0607E-01,8.2995E-01,
     &8.7738E-01,8.7537E-01,6.8706E-01,7.4473E-01,7.3452E-01,7.4622E-01,
     &5.5020E-01,4.9306E-01,4.4354E-01/
          data ((a_extx(i,j,17),i=1,mbx),j=1, 1) / 
     &9.9862E-01,1.0399E+00,1.0605E+00,1.0829E+00,1.1122E+00,1.1463E+00,
     &1.1352E+00,1.0268E+00,9.5460E-01,8.0815E-01,6.5354E-01,5.4069E-01,
     &3.4752E-01,3.9212E-01,9.2540E-01,5.9978E-01,4.2181E-01,3.0441E-01,
     &4.3784E-01,2.4166E-01,1.0788E-01/
          data ((a_ssax(i,j,17),i=1,mbx),j=1, 1) / 
     &8.2892E-01,8.9856E-01,9.0801E-01,9.1805E-01,9.2239E-01,9.2475E-01,
     &9.1049E-01,8.5810E-01,9.3325E-01,9.2125E-01,8.1176E-01,7.0048E-01,
     &4.4997E-01,2.3727E-01,4.7420E-01,4.4011E-01,4.1472E-01,2.3907E-01,
     &3.4361E-01,2.8935E-01,6.5795E-02/
          data ((a_asyx(i,j,17),i=1,mbx),j=1, 1) / 
     &7.7844E-01,7.3730E-01,7.2692E-01,7.1664E-01,7.0719E-01,6.9697E-01,
     &6.9693E-01,7.1094E-01,6.9606E-01,7.0348E-01,7.2044E-01,7.0800E-01,
     &6.9031E-01,6.2412E-01,4.3208E-01,5.0882E-01,4.6848E-01,3.8096E-01,
     &2.5301E-01,1.9866E-01,7.5654E-02/
          data ((a_extx(i,j,18),i=1,mbx),j=1, 8) / 
     &1.0085E+00,7.1381E-01,5.9827E-01,4.8560E-01,3.4687E-01,1.9190E-01,
     &5.3387E-02,1.0936E-01,9.1220E-02,6.2530E-02,7.4844E-02,4.9247E-02,
     &9.8567E-02,1.5866E-01,9.8795E-02,4.7001E-02,2.3933E-02,3.0303E-02,
     &5.8921E-03,9.4331E-03,7.8476E-03,
     &1.0063E+00,7.6454E-01,6.6068E-01,5.5541E-01,4.1818E-01,2.5349E-01,
     &6.7425E-02,1.3618E-01,7.1252E-02,4.5265E-02,5.0743E-02,4.0107E-02,
     &5.0885E-02,8.2830E-02,6.4548E-02,4.6187E-02,5.2576E-02,5.2710E-02,
     &2.6653E-02,1.8756E-02,1.6238E-02,
     &1.0056E+00,7.8472E-01,6.8603E-01,5.8402E-01,4.4795E-01,2.7935E-01,
     &7.6964E-02,1.5020E-01,7.2798E-02,4.4786E-02,4.7676E-02,4.0122E-02,
     &4.4062E-02,6.8662E-02,5.5404E-02,4.5577E-02,5.9870E-02,5.8603E-02,
     &3.1970E-02,2.1369E-02,1.8454E-02,
     &1.0049E+00,8.0113E-01,7.0668E-01,6.0750E-01,4.7263E-01,3.0114E-01,
     &8.5732E-02,1.6264E-01,7.6278E-02,4.5872E-02,4.6687E-02,4.0999E-02,
     &4.0640E-02,6.0601E-02,4.9935E-02,4.5485E-02,6.5088E-02,6.2992E-02,
     &3.5753E-02,2.3281E-02,2.0042E-02,
     &1.0037E+00,8.2962E-01,7.4385E-01,6.5051E-01,5.1870E-01,3.4297E-01,
     &1.0425E-01,1.8791E-01,8.6663E-02,5.0507E-02,4.7442E-02,4.4185E-02,
     &3.7526E-02,5.0945E-02,4.3151E-02,4.6193E-02,7.3869E-02,7.0723E-02,
     &4.2083E-02,2.6579E-02,2.2752E-02,
     &1.0023E+00,8.6555E-01,7.9080E-01,7.0602E-01,5.8000E-01,4.0072E-01,
     &1.3291E-01,2.2507E-01,1.0659E-01,6.0775E-02,5.2323E-02,5.0932E-02,
     &3.7669E-02,4.4987E-02,3.8856E-02,4.8675E-02,8.4962E-02,8.1064E-02,
     &5.0025E-02,3.0888E-02,2.6233E-02,
     &1.0008E+00,9.1355E-01,8.5631E-01,7.8597E-01,6.7232E-01,4.9326E-01,
     &1.8606E-01,2.8955E-01,1.4806E-01,8.4133E-02,6.6189E-02,6.6033E-02,
     &4.3873E-02,4.4232E-02,3.8281E-02,5.5043E-02,1.0276E-01,9.8507E-02,
     &6.2865E-02,3.8143E-02,3.1906E-02,
     &1.0001E+00,9.4985E-01,9.0842E-01,8.5189E-01,7.5252E-01,5.7985E-01,
     &2.4412E-01,3.5566E-01,1.9629E-01,1.1304E-01,8.4689E-02,8.4699E-02,
     &5.4408E-02,4.9142E-02,4.1812E-02,6.3055E-02,1.2089E-01,1.1700E-01,
     &7.6311E-02,4.5945E-02,3.7858E-02/
          data ((a_ssax(i,j,18),i=1,mbx),j=1, 8) / 
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9760E-01,
     &9.5117E-01,1.9267E-01,1.7757E-01,1.2322E-01,6.4463E-02,5.6707E-02,
     &1.5273E-02,2.8259E-02,4.0809E-02,4.1007E-02,2.7600E-02,3.5885E-02,
     &7.6457E-02,4.1228E-03,9.6684E-04,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9831E-01,
     &9.6937E-01,4.1168E-01,4.7210E-01,3.4069E-01,1.6575E-01,1.5049E-01,
     &5.4685E-02,4.0884E-02,4.9824E-02,3.3489E-02,1.6804E-02,1.4706E-02,
     &1.4378E-02,6.9697E-03,1.1260E-03,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9847E-01,
     &9.7318E-01,4.7907E-01,5.7853E-01,4.3119E-01,2.2264E-01,1.9453E-01,
     &8.3450E-02,5.2289E-02,5.7847E-02,3.4321E-02,1.7776E-02,1.6799E-02,
     &1.5766E-02,8.1885E-03,1.3822E-03,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9855E-01,
     &9.7553E-01,5.2326E-01,6.5140E-01,4.9846E-01,2.7202E-01,2.3046E-01,
     &1.1147E-01,6.4569E-02,6.6007E-02,3.5725E-02,1.9175E-02,1.8957E-02,
     &1.7499E-02,9.4180E-03,1.6387E-03,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9867E-01,
     &9.7882E-01,5.8489E-01,7.5411E-01,6.0291E-01,3.6348E-01,2.9429E-01,
     &1.7152E-01,9.4783E-02,8.4944E-02,3.9643E-02,2.2733E-02,2.3749E-02,
     &2.1705E-02,1.2237E-02,2.2387E-03,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9875E-01,
     &9.8167E-01,6.3772E-01,8.3832E-01,7.0034E-01,4.6871E-01,3.6753E-01,
     &2.5692E-01,1.4745E-01,1.1634E-01,4.6691E-02,2.8686E-02,3.1272E-02,
     &2.8744E-02,1.6978E-02,3.2846E-03,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9884E-01,
     &9.8426E-01,6.8441E-01,9.0380E-01,7.8804E-01,5.8503E-01,4.5493E-01,
     &3.7882E-01,2.4372E-01,1.7373E-01,6.0405E-02,3.9763E-02,4.4900E-02,
     &4.2217E-02,2.6511E-02,5.5458E-03,
     &1.0000E+00,9.9999E-01,1.0000E+00,9.9999E-01,9.9995E-01,9.9884E-01,
     &9.8581E-01,7.0863E-01,9.3207E-01,8.3208E-01,6.5333E-01,5.1386E-01,
     &4.6855E-01,3.3118E-01,2.2941E-01,7.5084E-02,5.1397E-02,5.9066E-02,
     &5.6877E-02,3.7575E-02,8.3921E-03/
          data ((a_asyx(i,j,18),i=1,mbx),j=1, 8) / 
     &7.1721E-01,6.9739E-01,6.8638E-01,6.7225E-01,6.5073E-01,6.0877E-01,
     &5.2775E-01,4.1059E-01,3.7321E-01,3.1625E-01,2.6611E-01,2.3698E-01,
     &1.7040E-01,1.3527E-01,1.4775E-01,1.5188E-01,1.2578E-01,9.5223E-02,
     &8.5724E-02,5.2565E-02,2.3082E-02,
     &7.6904E-01,7.5650E-01,7.4841E-01,7.3810E-01,7.2054E-01,6.8471E-01,
     &6.2732E-01,5.0683E-01,4.8159E-01,4.2112E-01,3.6731E-01,3.2918E-01,
     &2.7484E-01,2.3254E-01,2.1972E-01,1.9039E-01,1.4800E-01,1.2038E-01,
     &1.0420E-01,7.1214E-02,3.3838E-02,
     &7.7793E-01,7.6850E-01,7.6121E-01,7.5216E-01,7.3628E-01,7.0413E-01,
     &6.5354E-01,5.3295E-01,5.1181E-01,4.5230E-01,3.9896E-01,3.5835E-01,
     &3.0766E-01,2.6449E-01,2.4588E-01,2.0858E-01,1.6011E-01,1.3205E-01,
     &1.1391E-01,7.9742E-02,3.8769E-02,
     &7.8370E-01,7.7528E-01,7.6966E-01,7.6177E-01,7.4764E-01,7.1735E-01,
     &6.7234E-01,5.5131E-01,5.3212E-01,4.7523E-01,4.2259E-01,3.8075E-01,
     &3.3284E-01,2.8886E-01,2.6598E-01,2.2313E-01,1.7053E-01,1.4137E-01,
     &1.2214E-01,8.6936E-02,4.3112E-02,
     &7.9004E-01,7.8542E-01,7.8116E-01,7.7436E-01,7.6256E-01,7.3691E-01,
     &7.0067E-01,5.8086E-01,5.6625E-01,5.1283E-01,4.6197E-01,4.1864E-01,
     &3.7391E-01,3.3075E-01,3.0218E-01,2.5069E-01,1.9084E-01,1.6002E-01,
     &1.3766E-01,1.0079E-01,5.1312E-02,
     &7.9479E-01,7.9322E-01,7.9056E-01,7.8554E-01,7.7727E-01,7.5650E-01,
     &7.3028E-01,6.1293E-01,6.0183E-01,5.5406E-01,5.0701E-01,4.6246E-01,
     &4.2159E-01,3.7958E-01,3.4655E-01,2.8643E-01,2.1756E-01,1.8357E-01,
     &1.5871E-01,1.1929E-01,6.2875E-02,
     &7.9755E-01,7.9901E-01,7.9834E-01,7.9576E-01,7.9090E-01,7.7660E-01,
     &7.6335E-01,6.5093E-01,6.4342E-01,6.0506E-01,5.6471E-01,5.1935E-01,
     &4.8336E-01,4.4409E-01,4.0815E-01,3.3922E-01,2.5845E-01,2.1960E-01,
     &1.9091E-01,1.4769E-01,8.0694E-02,
     &7.9667E-01,8.0114E-01,8.0178E-01,8.0094E-01,7.9898E-01,7.8918E-01,
     &7.8464E-01,6.7897E-01,6.7268E-01,6.4225E-01,6.0842E-01,5.6324E-01,
     &5.3031E-01,4.9508E-01,4.5869E-01,3.8502E-01,2.9506E-01,2.5185E-01,
     &2.2020E-01,1.7378E-01,9.8372E-02/
          data ((a_extx(i,j,19),i=1,mbx),j=1, 1) / 
     &9.9380E-01,1.0900E+00,1.0920E+00,1.0390E+00,8.3660E-01,6.5060E-01,
     &3.8390E-01,1.6330E-01,7.1790E-02,3.9680E-02,3.4320E-02,3.3920E-02,
     &3.0930E-02,7.1380E-02,1.0130E-01,5.5120E-02,2.9280E-02,3.9570E-02,
     &3.8090E-02,2.1020E-02,1.7130E-02/
          data ((a_ssax(i,j,19),i=1,mbx),j=1, 1) / 
     &9.5830E-01,9.9440E-01,9.9450E-01,9.9140E-01,9.8490E-01,9.7480E-01,
     &9.5460E-01,8.2820E-01,9.0020E-01,8.3440E-01,6.1250E-01,2.5370E-01,
     &9.9960E-02,3.7440E-02,1.7560E-01,6.9590E-02,3.7670E-02,1.4250E-02,
     &1.7720E-02,7.0600E-03,2.4120E-03/
          data ((a_asyx(i,j,19),i=1,mbx),j=1, 1) / 
     &6.5950E-01,6.6510E-01,6.7590E-01,6.8690E-01,6.8640E-01,6.7100E-01,
     &6.1720E-01,4.8840E-01,3.7430E-01,2.5990E-01,2.1390E-01,1.4880E-01,
     &1.0660E-01,8.4760E-02,1.2800E-01,6.2120E-02,4.0090E-02,2.8210E-02,
     &2.4390E-02,1.2380E-02,6.2840E-03/
          data ((a_extx(i,j,20),i=1,mbx),j=1, 1) / 
     &1.0000E+00,1.0650E+00,1.1030E+00,1.1600E+00,1.2490E+00,1.2610E+00,
     &1.1310E+00,7.3010E-01,4.6730E-01,3.2030E-01,2.4840E-01,1.5700E-01,
     &1.0970E-01,2.0640E-01,5.2850E-01,1.9560E-01,9.3190E-02,1.1030E-01,
     &1.1850E-01,5.6760E-02,4.2600E-02/
          data ((a_ssax(i,j,20),i=1,mbx),j=1, 1) / 
     &9.2590E-01,9.8640E-01,9.8680E-01,9.8120E-01,9.7400E-01,9.6580E-01,
     &9.5710E-01,8.8960E-01,9.5110E-01,9.3170E-01,8.2280E-01,5.5140E-01,
     &3.1580E-01,1.3520E-01,3.9080E-01,2.8840E-01,1.9550E-01,8.9360E-02,
     &1.1360E-01,5.1450E-02,1.8700E-02/
          data ((a_asyx(i,j,20),i=1,mbx),j=1, 1) / 
     &7.1270E-01,6.6570E-01,6.5820E-01,6.6080E-01,6.8180E-01,7.0220E-01,
     &7.1440E-01,6.9100E-01,6.3460E-01,5.5060E-01,5.1230E-01,4.3350E-01,
     &3.4600E-01,2.7800E-01,2.5500E-01,2.2170E-01,1.5550E-01,1.0960E-01,
     &9.2650E-02,5.0520E-02,2.5380E-02/
          data ((a_extx(i,j,21),i=1,mbx),j=1, 1) / 
     &9.9990E-01,1.0330E+00,1.0470E+00,1.0740E+00,1.1270E+00,1.1790E+00,
     &1.2800E+00,1.3090E+00,1.2280E+00,1.0900E+00,9.1060E-01,5.9870E-01,
     &3.7760E-01,4.8880E-01,1.1960E+00,6.5310E-01,3.6550E-01,3.5150E-01,
     &4.8980E-01,2.1320E-01,1.2210E-01/
          data ((a_ssax(i,j,21),i=1,mbx),j=1, 1) / 
     &8.8240E-01,9.7410E-01,9.7400E-01,9.6030E-01,9.4200E-01,9.2480E-01,
     &9.1830E-01,8.6870E-01,9.5280E-01,9.4500E-01,8.7850E-01,7.0970E-01,
     &5.2020E-01,2.5210E-01,4.7130E-01,4.9740E-01,4.4160E-01,2.7530E-01,
     &3.2670E-01,2.3290E-01,1.1810E-01/
          data ((a_asyx(i,j,21),i=1,mbx),j=1, 1) / 
     &7.7940E-01,7.3210E-01,7.2390E-01,7.1280E-01,6.9730E-01,6.9450E-01,
     &7.0200E-01,7.4560E-01,7.2750E-01,6.8590E-01,6.9020E-01,6.9140E-01,
     &6.5520E-01,5.8060E-01,3.9850E-01,4.7690E-01,4.2060E-01,3.2690E-01,
     &2.1360E-01,1.6870E-01,1.0040E-01/
          data ((a_extx(i,j,22),i=1,mbx),j=1, 1) / 
     &1.0000E+00,1.0200E+00,1.0310E+00,1.0430E+00,1.0780E+00,1.0910E+00,
     &1.1350E+00,1.2300E+00,1.3270E+00,1.3970E+00,1.3700E+00,1.2150E+00,
     &8.7230E-01,7.7220E-01,1.3330E+00,1.2320E+00,9.7440E-01,8.4380E-01,
     &1.1680E+00,8.0150E-01,4.8350E-01/
          data ((a_ssax(i,j,22),i=1,mbx),j=1, 1) / 
     &8.2580E-01,9.5360E-01,9.5300E-01,9.3030E-01,8.9520E-01,8.6420E-01,
     &8.4240E-01,7.7070E-01,9.1010E-01,9.0830E-01,8.4440E-01,7.3040E-01,
     &6.2230E-01,3.4840E-01,4.9680E-01,5.5370E-01,5.6260E-01,4.2750E-01,
     &4.4620E-01,4.2270E-01,3.3440E-01/
          data ((a_asyx(i,j,22),i=1,mbx),j=1, 1) / 
     &8.2700E-01,7.7870E-01,7.7330E-01,7.7420E-01,7.6530E-01,7.6950E-01,
     &7.5370E-01,7.6290E-01,7.1550E-01,6.9040E-01,7.3150E-01,8.0010E-01,
     &8.1850E-01,7.9030E-01,5.9960E-01,6.5660E-01,6.4600E-01,5.8660E-01,
     &3.6470E-01,3.2240E-01,2.5760E-01/
          data ((a_extx(i,j,23),i=1,mbx),j=1, 1) / 
     &9.9960E-01,1.0130E+00,1.0190E+00,1.0260E+00,1.0420E+00,1.0540E+00,
     &1.0780E+00,1.1200E+00,1.1600E+00,1.2060E+00,1.2390E+00,1.2800E+00,
     &1.1820E+00,9.4180E-01,1.2530E+00,1.3210E+00,1.3200E+00,1.2110E+00,
     &1.3720E+00,1.3470E+00,1.1640E+00/
          data ((a_ssax(i,j,23),i=1,mbx),j=1, 1) / 
     &7.5950E-01,9.1730E-01,9.1680E-01,8.8060E-01,8.2940E-01,7.8390E-01,
     &7.5390E-01,6.7000E-01,8.3030E-01,8.2280E-01,7.3160E-01,6.3500E-01,
     &6.0910E-01,4.2270E-01,5.3550E-01,5.2100E-01,5.4940E-01,4.8570E-01,
     &4.9050E-01,4.7860E-01,4.5250E-01/
          data ((a_asyx(i,j,23),i=1,mbx),j=1, 1) / 
     &8.6320E-01,8.1130E-01,8.0880E-01,8.1710E-01,8.2610E-01,8.3540E-01,
     &8.3380E-01,8.4580E-01,7.6700E-01,7.3190E-01,7.7060E-01,8.4030E-01,
     &8.8190E-01,8.8920E-01,7.3330E-01,7.7070E-01,7.6670E-01,7.5640E-01,
     &5.7060E-01,4.9550E-01,4.2910E-01/


!--------------
! 24 Boutique 1 [ Match Log_normal rg=.298 sg=2 weighted (0.1-0.5um) Lacis2004]      
       data ((a_extx(i,j,24),i=1,mbx),j=1,1) /
     & .9812E+00,.1002E+01,.9690E+00,.8861E+00,.6820E+00,
     % .5216E+00,.3038E+00,
     & .1284E+00,.5640E-01,.3107E-01,.2681E-01,
     % .2699E-01,.2504E-01,.5839E-01,
     & .7985E-01,.4469E-01,.2396E-01,.3255E-01,
     % .3122E-01,.1733E-01,.1417E-01/
       data ((a_ssax(i,j,24),i=1,mbx),j=1,1) /
     & .9667E+00,.9952E+00,.9951E+00,.9918E+00,.9845E+00,
     %.9730E+00,.9487E+00,
     & .7999E+00,.8772E+00,.7937E+00,.5488E+00,
     %.2013E+00,.7486E-01,.2757E-01,
     & .1384E+00,.5071E-01,.2702E-01,.1010E-01,
     %.1258E-01,.4974E-02,.1691E-02/
       data ((a_asyx(i,j,24),i=1,mbx),j=1,1) /
     & .6585E+00,.6696E+00,.6745E+00,.6771E+00,.6644E+00,
     %.6422E+00,.5790E+00,
     & .4391E+00,.3311E+00,.2255E+00,.1802E+00,
     %.1192E+00,.8344E-01,.6566E-01,
     & .1046E+00,.4825E-01,.3091E-01,.2166E-01,
     %.1875E-01,.9477E-02,.4803E-02/
 ! 25 Boutique 2[ Match Log_normal rg=.298 sg=2 weighted (0.5-5.um) Lacis2004	     
       data ((a_extx(i,j,25),i=1,mbx),j=1,1) /
     & .1001E+01,.1077E+01,.1105E+01,.1129E+01,.1125E+01,
     %.1089E+01,.1012E+01,
     & .8806E+00,.8139E+00,.7619E+00,.6738E+00,
     %.4943E+00,.3263E+00,.3430E+00,
     & .7998E+00,.5203E+00,.3524E+00,.3035E+00,
     %.4460E+00,.2702E+00,.1536E+00/
       data ((a_ssax(i,j,25),i=1,mbx),j=1,1) /
     & .9224E+00,.9852E+00,.9856E+00,.9786E+00,.9678E+00,
     %.9551E+00,.9413E+00,
     & .8698E+00,.9463E+00,.9334E+00,.8497E+00,
     %.6478E+00,.4520E+00,.1979E+00,
     & .4217E+00,.4234E+00,.3778E+00,.2243E+00,
     %.2779E+00,.2214E+00,.1298E+00/
       data ((a_asyx(i,j,25),i=1,mbx),j=1,1) /
     & .7140E+00,.6797E+00,.6791E+00,.6833E+00,.6920E+00,
     %.6985E+00,.6993E+00,
     & .7013E+00,.6735E+00,.6279E+00,.6273E+00,
     %.6118E+00,.5568E+00,.4353E+00,
     & .3388E+00,.4072E+00,.3733E+00,.2798E+00,
     %.1985E+00,.1675E+00,.1073E+00/
!#201004 ADD LACIS2004 aerosols consistent with ED3Match dust sizes     
!  0.20  0.69  0.79  0.89  1.04  1.41  1.90  2.50  3.50  4.55  5.26  5.88  7.14  8.00  9.09 10.20 12.50 14.93 18.52 25.00 35.71
!  0.69  0.79  0.89  1.04  1.41  1.90  2.50  3.50  4.00  5.26  5.88  7.14  8.00  9.09 10.20 12.50 14.93 18.52 25.00 35.71 99.99
!#26 Lacis2004 Dust Re=0.35
       data ((a_extx(i,j,26),i=1,mbx),j=1,1) /
     &  0.9405,  0.8992,  0.8143,  0.6988,  0.4919,  0.2725,  0.1295,
     & 0.06123, 0.02537, 0.01310, 0.01328, 0.01733, 0.01901, 0.04108,
     & 0.05067, 0.03356, 0.01794, 0.02593, 0.02335, 0.01337,  0.01083/
      data ((a_ssax(i,j,26),i=1,mbx),j=1,1) /
     &  0.9435,  0.9959,  0.9954,  0.9918,  0.9834,  0.9593,  0.9154,
     &  0.7270,  0.8042,  0.6993,  0.3835,  0.1325, 0.04775, 0.01418,
     & 0.07817, 0.02758, 0.01490,0.004869,0.006508,0.002733,0.0007668/
      data ((a_asyx(i,j,26),i=1,mbx),j=1,1) /
     &  0.6640,  0.6762,  0.6740,  0.6657,  0.6328,  0.5599,  0.4545,
     &  0.3180,  0.2226,  0.1418,  0.1093, 0.07824, 0.05639, 0.04316,
     & 0.06717, 0.03158, 0.02026, 0.01350, 0.01206,0.006343, 0.002882/
!#27Lacis2004 Dust Re=0.82
      data ((a_extx(i,j,27),i=1,mbx),j=1,1) /
     &  0.9820,   1.106,   1.149,   1.192,   1.210,   1.086,  0.8094,
     &  0.5113,  0.3071,  0.1922,  0.1393,  0.1003, 0.08200,  0.1410,
     &  0.3403,  0.1392, 0.06527, 0.08416, 0.08227, 0.04257,  0.03230/
      data ((a_ssax(i,j,27),i=1,mbx),j=1,1) /
     &  0.8904,  0.9895,  0.9895,  0.9846,  0.9786,  0.9677,  0.9548,
     &  0.8789,  0.9396,  0.9225,  0.7658,  0.4908,  0.2677, 0.09504,
     &  0.3343,  0.2103,  0.1351, 0.05145, 0.06976, 0.03150, 0.009279/
      data ((a_asyx(i,j,27),i=1,mbx),j=1,1) /
     &  0.7183,  0.6535,  0.6563,  0.6672,  0.6889,  0.7038,  0.6940,
     &  0.6452,  0.5806,  0.4828,  0.4240,  0.3428,  0.2646,  0.2054,
     &  0.2221,  0.1604,  0.1081, 0.07204, 0.06458, 0.03495,  0.01567/
!#28Lacis2004 Dust Re=1.70
      data ((a_extx(i,j,28),i=1,mbx),j=1,1) /
     &  0.9870,   1.041,   1.059,   1.087,   1.146,   1.242,   1.307,
     &   1.234,   1.077,  0.8954,  0.6890,  0.4590,  0.3186,  0.3754,
     &   1.062,  0.5229,  0.2685,  0.2719,  0.3482,  0.1530,  0.08859/
      data ((a_ssax(i,j,28),i=1,mbx),j=1,1) /
     &  0.8339,  0.9772,  0.9764,  0.9641,  0.9505,  0.9359,  0.9339,
     &  0.8793,  0.9519,  0.9496,  0.8658,  0.6990,  0.5066,  0.2204,
     &  0.4595,  0.4597,  0.3967,  0.2147,  0.2786,  0.1812,  0.07110/
      data ((a_asyx(i,j,28),i=1,mbx),j=1,1) /
     &  0.7922,  0.7148,  0.7051,  0.6962,  0.6865,  0.6930,  0.7189,
     &  0.7409,  0.7209,  0.6724,  0.6673,  0.6489,  0.5979,  0.5176,
     &  0.3647,  0.4200,  0.3550,  0.2539,  0.1845,  0.1351,  0.06779/
!#29Lacis2004 Dust Re= 3.2
      data ((a_extx(i,j,29),i=1,mbx),j=1,1) /
     &  0.9917,   1.026,   1.038,   1.054,   1.079,   1.127,   1.200,
     &   1.288,   1.369,   1.398,   1.284,   1.050,  0.7618,  0.6409,
     &   1.324,   1.082,  0.7681,  0.6699,  0.9608,  0.5727,   0.2919/
      data ((a_ssax(i,j,29),i=1,mbx),j=1,1) /
     &  0.7825,  0.9612,  0.9593,  0.9380,  0.9123,  0.8809,  0.8726,
     &  0.8051,  0.9233,  0.9314,  0.8608,  0.7471,  0.6185,  0.3210,
     &  0.4868,  0.5445,  0.5426,  0.3710,  0.4194,  0.3780,   0.2478/
      data ((a_asyx(i,j,29),i=1,mbx),j=1,1) /
     &  0.8378,  0.7637,  0.7578,  0.7560,  0.7498,  0.7383,  0.7288,
     &  0.7489,  0.7231,  0.6987,  0.7372,  0.7772,  0.7773,  0.7447,
     &  0.5420,  0.6073,  0.5861,  0.4971,  0.3070,  0.2726,   0.1916/

	end
