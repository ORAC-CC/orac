

      subroutine gwtsa_sw_v20
     &  (solconst,nlayers,amu0,w0_temp,g0_temp,suralb,
     &   taul_temp,gau_wt,dis2sun,nu_temp,c_frctn_temp,
     &   cum_c_frctn_temp,
     &   taul_clear_temp,w0_clear_temp,g0_clear_temp,
!     &   plankbnd, planklev_temp,
     &   iclear, sol_fluxup,sol_fluxdn,sol_direct,sol_diffuse)

c  The giometry
c
c        ------------------------------
c        In this subroutine, the top layer with taul = 0 is added
c       
c        ------------------------------ 1 st level
c        2 nd layer (this is the 1 st layer in the main code)             
c
c        ------------------------------ 2 nd level
c                    .
c                    .
c                    .
c                    .
c                    .
c
c        -----------------------------  j - 1 th level
c        j th layer
c
c        -----------------------------  j th level
c              | fdn(j)      ^ fup(j)
c              V             |
c        ------------------------------ j th level
c        j + 1 th layer
c
c        ------------------------------- j + 1 th level
c
c
c       The total layer in this code is n + 1 and the bottom of the 
c       n + 1 th layer is n + 1 the level.
c       Then, the irradiance is computed from 1 st to n + 1 levels.
c
c  driver for 2-stream multiple scattering calculation.
c
c All variables are explained in fourstr_vasriables.dat

c Inputs
c dis2sun      The distance to the sun in astronomical units.
c solconst     The direct beam irradiance at the TOA
c nlayers      Total number of layers in the atmosphere
c amu0         Cosine of the solar zenith angle (Radian)
c w0(j)        Single scattering albedo for jth layer (j=1,nlayers)
c g0(j)        Asymmetry parameter (j=1,nalyers)
c suralb       Surface albedo
c taul(j)    Optical depth of jth layer (j=1,nlayers)
c iprint       print output flug =0, no outputs
c nu(j)        prarameter of the gamma distribution, (taul/sigma)^2
c c_frctn(j)  cloud fraction

c Output variables

c sol_fluxdn(j)  Downward shortwave irradinace at j level 
c              (W m^-2) (j=1,ntaujs)
c              j level is the top boundary of jth layer
c              the solar constant at TOA is 1.0 so to obtain W m^-2, need to
c              multiply by the solar constant at TOA
c sol_fluxup(j)  Upwnward shortwave irradinace at j level 
c              (W m^-2) (j=1,ntaujs)
c sol_direct(j)  Direct beam irradiance (W m^-2) (j=1,ntaujs)


c ivert  = maximum number of layers;
      USE FUINPUT ,only: nvx
      implicit none

      integer ivert, ilayer, idbl
      parameter (ivert=nvx)
      parameter (ilayer=ivert+1)  
      parameter (idbl = ilayer*2)                       
                     
      real*8 fup(ilayer), fdn(ilayer), direct(ilayer)

      real sol_fluxup(ilayer), sol_fluxdn(ilayer)
      real sol_direct(ilayer), sol_diffuse(ilayer)


      real*8 e1(ilayer), e2(ilayer), e3(ilayer), e4(ilayer)
      real*8 e5(ilayer), e6(ilayer), e7(ilayer), e8(ilayer)
      real*8 af(idbl), bf(idbl), ef(idbl)
      real*8 c_plus(ilayer), c_minus(ilayer)
      real*8 c_frctn(ilayer), cum_c_frctn(ilayer)
      real*8 ak(ilayer)
      real*8 gami(ilayer)
       real*8 ck1(ilayer), ck2(ilayer)
      
      real w0_temp(*), g0_temp(*), taul_temp(*)
      real taul_clear_temp(*), w0_clear_temp(*), g0_clear_temp(*) 
!      real planklev_temp(*), plankbnd
      real w0(ilayer), g0(ilayer)
      real w0_clear(ilayer), g0_clear(ilayer)
       
      real taul(ilayer)
      real taulnd(ivert), opd(ilayer), opdnd(ilayer)
      
      real cef(ilayer), cefnd(ilayer)
      real amu0, suralb
      real solconst, dis2sun
      real nu_temp(*), c_frctn_temp(*), cum_c_frctn_temp(*)
      real*8 gau_wt, nu(ilayer)
      real*8 slope(ilayer), ptemp(ilayer), ptempg
      real*8 u1i, u1s

!!SEIJI  took out "save"
!! SAVE THESE CLEAR SKY ARRAYS FROM LAST CLEAR SKY CALL		      !FRED
!! THEY ARE NEEDED FOR THE CLOUDY SKY SOLVER			      !FRED
! Calling order becomes important now
      real :: opdnd_clear(ilayer), opd_clear(ilayer)	              !FRED
      real :: taul_clear(ilayer), taulnd_clear(ilayer)	              !FRED

      real*8 ::b1_clear(ilayer),b2_clear(ilayer),b3_clear(ilayer)     !FRED
      real*8 :: el1_clear(ilayer), el2_clear(ilayer)	              !FRED
      real*8 :: em1_clear(ilayer), em2_clear(ilayer)	              !FRED
      real*8 :: gami_clear(ilayer), ee1_clear(ilayer)	              !FRED
      real*8 :: ak_clear(ilayer)				      !FRED
      real*8 :: c_plus_clear(ilayer), c_minus_clear(ilayer)           !FRED
      real*8 :: af_clear(idbl), bf_clear(idbl), ef_clear(idbl)        !FRED

      real epsilon
   
      integer irflag, nlayers, ntaujs, j, iclear

      data epsilon / 1.0e-15 /

c w0max is the maximum allowed asymmetry parameter - values larger 
c than 0.9999 tend to induce model instabilities.

      ntaujs = nlayers + 1
      irflag = 0
      if (amu0 .gt. 0.0) then

c --------------------------------------------------------------------
c Set all flux to be zero and 
c ---------------------------------------------------------------------

        do 20 j = 1,ntaujs
           fup(j) = 0.0
           fdn(j) = 0.0
           direct(j) = 0.0
 20     continue

  


        if (iclear .eq. 1) then 

c computing clear-sky properties ---------------------------------

          call raprad_twostr_sw_clear
     &      (nlayers,w0_clear_temp,g0_clear_temp,
     &       taul_clear_temp,
     &       taul_clear,opd_clear, taulnd_clear,opdnd_clear, 
     &       w0_clear, g0_clear)


c for clearsky computation
          call twostr
     &     (ntaujs,irflag,taul_clear,w0_clear,g0_clear,suralb,
     &      b1_clear,b2_clear,el1_clear,el2_clear,em1_clear,em2_clear,
     &      af_clear,bf_clear,ef_clear,ak_clear,u1i,u1s,
     &      gami_clear,ee1_clear)

          call add
     &      (ntaujs,taul_clear,w0_clear,g0_clear,suralb,
     &       opd_clear,opdnd_clear,
     &       ak_clear,b1_clear,b2_clear,b3_clear,em1_clear,em2_clear,
     &       el1_clear,el2_clear,af_clear,bf_clear,ef_clear,amu0,
     &       slope,ptempg,ptemp,u1i,u1s,
     &       fup,fdn,direct,
     &       irflag,ck1,ck2)
       
        else
c computing clearsky and cloudy sky properties --------------------------------
 
          call raprad_twostr_sw_clear
     &      (nlayers,w0_clear_temp,g0_clear_temp,
     &       taul_clear_temp,
     &       taul_clear,opd_clear, taulnd_clear,opdnd_clear, 
     &       w0_clear, g0_clear)
      
          call raprad_twostr_sw_cloud
     &      (nlayers,w0_temp,g0_temp,
     &       taul_temp,nu_temp,c_frctn_temp, 
     &       cum_c_frctn_temp, taul_clear,taulnd_clear,
     &       taul,opd,taulnd,opdnd,w0, g0,c_frctn,cum_c_frctn,
     &       nu,cef,cefnd)

       
c ---------------------------------------------------------------------
c setting up cloudy conditions and combine with clear_sky with cloud
c fraction

c for all sky computation
          call gwtsa_twostr
     &      (ntaujs,taul,w0,g0,suralb,nu,c_frctn,cum_c_frctn,amu0,
     &       e1,e2,e3,e4,e5,e6,e7,e8,af,bf,ef,cef,
     &       c_plus,c_minus,ak,
     &       gami)

       
          call gwtsa_add
     &       (ntaujs,suralb,amu0,opd,opdnd,taul,taulnd,w0,g0,
     &       nu,c_frctn,cum_c_frctn,taul_clear,taulnd_clear,
     &       e1,e2,e3,e4,e5,e6,e7,e8,af,bf,ef,cef,cefnd,
     &       c_plus,c_minus,ak,
     &       gami,
     &       fup,fdn,direct)
              
        end if

      end if

c correction factor of the sun to the earth distance

c         dist_cor = 1.0 / (dis2sun)**2


c obtain irradiances at the bottom of jth layer
      do 50 j = 1,ntaujs
         sol_fluxup(j) = solconst * fup(j)   

         sol_fluxdn(j) = solconst * fdn(j)   

         sol_direct(j) = solconst * direct(j)   

         sol_diffuse(j)= sol_fluxdn(j) - sol_direct(j)
       
 50   continue

 200  format('layer z(km)  prs(mb)   sw down    sw up')
 220  format(i4,2x,f6.2,2x,f7.1,2x,f9.3,3x,f9.3)
 240  format('mu_0=',f6.4,5x,'radau=',f6.4)

  400 format(i5,3e20.5e3)

      return
      end


      Subroutine raprad_twostr_sw_clear
     &  (nlayers,w0_temp,g0_temp,taul_temp,
     &   taul,opd,taulnd,opdnd,w0,g0)


c
c  driver for 2-stream multiple scattering calculation.
c
c All variables are explained in fourstr_vasriables.dat

c Inputs
c nlayers      Total number of layers in the atmosphere
c w0_temp(j)   Single scattering albedo for jth layer (j=1,nlayers)
c g0_temp(j)   asymmetry paremeter (j=1,nalyers)
c taul_temp(j) Optical depth of jth layer (j=1,nlayers)

c Output variables

c taul(j)      Optical depth of jth layer with delta approximation
c opd(j)       Cumulative optical thickness from TOA with delta
c              approximation
c taulnd(j)    Optical depth of jth layer no delta approximation
c opdnd(j)     Cumulative optical thickness from TOA no delta
c              approximation
c w0(j)        single scattering albedo
c g0(j)        asymmetry parameter

c ivert  = maximum number of layers;
      USE FUINPUT ,only: nvx
      implicit double precision(a-h, o-z)

      parameter (ivert=nvx)
      parameter (ilayer=ivert+1)  
      parameter (idbl = ilayer*2)                       
                     
      real w0_temp(*), g0_temp(*), taul_temp(*)
      real w0(ilayer), g0(ilayer)
      real taul(ilayer), g0l(ilayer), w0l(ilayer)
      real taulnd(ivert), opd(ilayer), opdnd(ilayer)
      real w0max
c      real amu0, suralb

      data epsilon / 1.0D-15 /

c radcorr corrects the solar constant based on the ephemeris derived
c distance to the sun 

c w0max is the maximum allowed asymmetry parameter - values larger 
c than 0.9999 tend to induce model instabilities.

      w0max = 0.9999
      ntaujs = nlayers + 1

      do 1000 j = 1, ntaujs
        if(j .eq. 1) then
          taul(j) = 0.0
          w0l(j)   = 0.0
          g0l(j)   = 0.0
          opdnd(j)  = 0.0
          j1=j
        else
          j1=j-1
          opdnd(j)  = taul_temp(j1)
          taul(j) = opdnd(j) - opdnd(j1)
          w0l(j)   = w0_temp(j1)
          g0l(j)   = g0_temp(j1)


        end if

        if(taul(j).lt.epsilon)  taul(j) = epsilon
           w0t = w0l(j)
        if(w0t .gt. (1.0D0 - epsilon))   w0t=1.0D0 - epsilon
           denom = w0l(j) * taul(j)
        if(denom.le.epsilon)  denom=epsilon

        if(denom.gt.epsilon) then
          g0t = g0l(j)
        else
          g0t = 0.0
        endif


        fo        = g0t**2
        den       = 1.0D0 - w0t*fo
        taulnd(j) = taul(j)
        taul(j)   = taul(j) * SNGL(den)
        w0(j)     = SNGL((1.0D0 - fo)*w0t/den)
        g0(j)     = SNGL(g0t/(1.0D0 + g0t))
        opd(j)    = 0.0
        opd(j)    = opd(j1) + taul(j)

        if(w0(j).gt.w0max) w0(j) = w0max

 1000 continue


c making the delta approximation

      return
      end



      Subroutine raprad_twostr_sw_cloud
     &  (nlayers,w0_temp,g0_temp,taul_temp,nu_temp,c_frctn_temp, 
     &   cum_c_frctn_temp,
     &   taul_clear,taulnd_clear,
     &   taul,opd,taulnd,opdnd,w0,g0,c_frctn,cum_c_frctn,
     &   nu,cef,cefnd)


c
c  driver for 2-stream multiple scattering calculation.
c
c All variables are explained in fourstr_vasriables.dat

c Inputs
c nlayers      Total number of layers in the atmosphere
c w0_temp(j)   Single scattering albedo for jth layer (j=1,nlayers)
c g0_temp(j)   asymmetry paremeter (j=1,nalyers)
c taul_temp(j) Optical depth of jth layer (j=1,nlayers)
c c_frctn_temp Cloud fraction
c nu_temp      (tau/sigma)**2

c Output variables

c taul(j)      Optical depth of jth layer with delta approximation
c opd(j)       Cumulative optical thickness from TOA with delta
c              approximation
c taulnd(j)    Optical depth of jth layer no delta approximation
c opdnd(j)     Cumulative optical thickness from TOA no delta
c              approximation
c w0(j)        single scattering albedo
c g0(j)        asymmetry parameter
c c_frctn(j)   Cloud fraction
c cum_c_frctn(j) Cumulative cloud fraction
c nu(j)        (tau/sigma)**2

c ivert  = maximum number of layers;
      USE FUINPUT ,only: nvx
      implicit double precision(a-h, o-z)

      parameter (ivert=nvx)
      parameter (ilayer=ivert+1)  
      parameter (idbl = ilayer*2)                       
                     
      dimension c_frctn(ilayer), cum_c_frctn(ilayer)
      real*8 nu(ilayer)

      real nu_temp(*), c_frctn_temp(*), cum_c_frctn_temp(*)

      real w0_temp(*), g0_temp(*), taul_temp(*)
      real w0(ilayer), g0(ilayer)
      real taul(ilayer), g0l(ilayer), w0l(ilayer)
      real taulnd(ivert), opd(ilayer), opdnd(ilayer)
      real cef(ilayer), cefnd(ilayer)
      real taul_clear(ilayer), taulnd_clear(ilayer)
      real w0max
c      real amu0, suralb

      data epsilon / 1.0D-15 /

c radcorr corrects the solar constant based on the ephemeris derived
c distance to the sun 

c w0max is the maximum allowed asymmetry parameter - values larger 
c than 0.9999 tend to induce model instabilities.

      w0max = 0.9999
      ntaujs = nlayers + 1

c--------------------------------------------------------------------c
c open an input file to read conditional probability
c input format is
c Given a layer the conditional probability of cloud occurrence 
c at different height goes 
c ------------------------------------------------------------>

      do 2000 j = 1, ntaujs
        cef(j) = 0.0
        cefnd(j) = 0.0
 2000 continue

c----------------------------------------------------------------------c
c End input of conditional probability.                                c
c----------------------------------------------------------------------c

      do 1000 j = 1, ntaujs
        if(j .eq. 1) then
          opdnd(j)  = 0.0
          taul(j) = 0.0
          w0l(j)   = 0.0
          g0l(j)   = 0.0
          nu(j)   = 1.0D4
          c_frctn(j) = 0.0
          cef(j) = 0.0
          cefnd(j) = 0.0
          cum_c_frctn(j) = 0.0

          j1=j
      
        else
          j1=j-1
          opdnd(j)  = taul_temp(j1)
          taul(j) = opdnd(j) - opdnd(j1)
          w0l(j)   = w0_temp(j1)
          g0l(j)   = g0_temp(j1)

!         print*,c_frctn_temp(j1),c_frctn_temp(ntaujs-1)

          c_frctn(j) = c_frctn_temp(j1)
          cum_c_frctn(j) = cum_c_frctn_temp(j1)

c cum_c_frctn test
          if (cum_c_frctn(j1) .ne. 0.0 
     &        .and. cum_c_frctn(j) .eq. 0.0) then
            cum_c_frctn(j) = cum_c_frctn(j1)
          end if
        end if

        if(taul(j).lt.SNGL(epsilon))  taul(j) = SNGL(epsilon)
           w0t = w0l(j)
        if(w0t .gt. (1.0D0 - epsilon))   w0t= 1.0D0 - epsilon
           denom = w0l(j) * DBLE(taul(j))
        if(denom.le.epsilon)  denom=epsilon

        if(denom.gt.epsilon) then
          g0t = g0l(j)
        else
          g0t = 0.0
        endif

        fo        = g0t**2
        den       = 1.0D0 - w0t*fo
        taulnd(j) = taul(j)
        taul(j)   = taul(j) * SNGL(den)
        w0(j)     = SNGL((1.0D0 - fo)*w0t/den)
        g0(j)     = SNGL(g0t/(1.0D0 + g0t))
        opd(j)    = 0.0
        opd(j)    = opd(j1) + taul(j)

        if(w0(j).gt.w0max) w0(j) = w0max

        if (c_frctn_temp(j1) .eq. 0.0) then
          nu(j) = 1.0D4
          cef(j) = 0.0
          cefnd(j) = 0.0
        else
          nu(j)     = nu_temp(j1)

          if (nu(j) .gt. 100.0D0) then
            nu(j) = 100.0D0
          end if

          do 1100 jj = 2, j1
            if (c_frctn(jj) * c_frctn(j) .gt. 0.0) then
!              cef(j) = cef(j) + (taul(jj) - taul_clear(jj))
!     &               / (taul(j) - taul_clear(j))

              cef(j) = cef(j) + taul(jj) / taulnd(j)

              if (taulnd(j) - taulnd_clear(j) .gt. SNGL(epsilon)) then

                cefnd(j) = cefnd(j) + (taulnd(jj) - taulnd_clear(jj))
     &                   / (taulnd(j) - taulnd_clear(j))         
              endif


!              print*, cefnd(j), jj, taulnd(jj), j, taulnd(j),
!     &                cef(j), jj, taul(jj), j, taul(j)
            endif
 1100     continue
        end if

 1000 continue

      return
      end


      subroutine gwtsa_twostr 
     & (nlayer,taul,w0,g0,rsfx,nu,c_frctn,cum_c_frctn,u0,
     &  e1,e2,e3,e4,e5,e6,e7,e8,af,bf,ef,cef,
     &  c_plus,c_minus,ak,
     &  gami)

c
c    ******************************************************************
c    *  purpose             :  defines matrix properties and sets up  *
c    *                         matrix coefficients that do not depend *
c    *                         on zenith angle or temperature.        *
c    *  subroutines called  :  get_ww,                                *
c    *                         get_dif_weight_top, get_dif_weight_btm *
c    *                         get_ww_ee_gwtsa                        *
c    *  input               :  nlayer,taul,w0,g0,rsfx,nu,c_frctn      *
c    *                         cum_c_frctn, u0,                       *
c    *                         u0                                     *
c    *  output              :  e1,e2,e3,e4,e5,e6,e7,e8,af,bf,ef,      *
c    *                         c_plus,c_minus,ak,                     *
c    * ****************************************************************

c define the dimensions used by the radiation model but might be
c specified by an external model
c
c ivert  = maximum number of layers;
c ilayer = maximum number of layer boundaries
c idbl   = twice the maximum number of layer boundaries
       USE FUINPUT ,only: nvx
       implicit none

       integer ivert, irad, ilayer, idbl
       parameter (ivert=nvx)
       parameter (irad=20)
       parameter (ilayer=ivert+1, idbl=2*ilayer)

c  double precision
c       implicit real*8 (a-h, o-z)

       real taul(*), w0(*), g0(*), cef(*)
       real rsfx, u0, x3

       real*8 nu(*), c_frctn(*), cum_c_frctn(*)

       real*8 gami(ilayer), ak(ilayer)
       real*8 b1(ilayer), b2(ilayer), b3(ilayer), b4
       real*8 ee1(ilayer)
       real*8 e1(ilayer), e2(ilayer), e3(ilayer), e4(ilayer)
       real*8 e5(ilayer), e6(ilayer), e7(ilayer), e8(ilayer)
       real*8 af(idbl), bf(idbl), ef(idbl)
       real*8 c_plus(ilayer), c_minus(ilayer)

       real*8 epsilon, x1
       real*8 c1, c2, c1_clear, c2_clear
       real*8 ww13p_r, ww13m_r, ww24p_r, ww24m_r
       real*8 ww13p0(ilayer), ww13m0(ilayer)
       real*8 ww24p0(ilayer), ww24m0(ilayer)
       real*8 ww13p1(ilayer), ww13m1(ilayer)
       real*8 ww24p1(ilayer), ww24m1(ilayer)
       real*8 wd1, wr1, wd2, wr2
       real*8 ww13p0_d, ww13m0_d, ww24p0_d, ww24m0_d 
       real*8 ww13p1_d, ww13m1_d, ww24p1_d, ww24m1_d
       real*8 ww13p0_d_tmp, ww13m0_d_tmp, ww24p0_d_tmp, ww24m0_d_tmp
       real*8 ww13p1_d_tmp, ww13m1_d_tmp, ww24p1_d_tmp, ww24m1_d_tmp
       real*8 c_corr
!       real*8 cf1,cf2, cf3 !!!FRED
       real*8 ref1, trans1, trans_dir1
       real*8 ref_diff1, trans_diff1
       real*8 ref2, trans2, trans_dir2
       real*8 ref_diff2, trans_diff2
       real u1i, pi, tpi, u1s
	   real*8 du0
       integer nlayer, jdble, jd, jn, j, j1


       data epsilon / 1.0d-15 /
c       data u1i     / 1.732050808 /
       data u1i     / 2.0 /
       data pi      / 3.141592654 /
       data tpi     / 6.283185307 /

c for shortwave
       jdble = 2 * nlayer - 2
       jn = jdble - 1

       u1s  =  tpi/u1i
       du0  =  1.0D0/DBLE(u0)


c      here we define layer properties following general scheme
c      of meador and weavor. then we set up layer properties
c      needed for matrix.
       
       do 14 j           = 1,nlayer

         if(j.ne.1) then

           j1=j-1

         else

           j1=j

         endif

                    
c      these are for twostream and hemispheric means
c These b1 and b2 are gamma1 and 2 defined in Table 1 in Toon at al. (1989)
c Quadrature
c         b1(j)    =  0.5*u1i*(2. - w0(j)*(1. + g0(j)))
c         b2(j)    =  0.5*u1i*w0(j)*(1. - g0(j))
c         b3(j)    =  0.5*(1.-u1i*g0(j)*u0)
c         b4       =  1. - b3(j)

c Eddington
         b1(j)    =  DBLE(0.25*(7. - w0(j)*(4. + 3.*g0(j))))
         b2(j)    =  DBLE(-0.25*(1. - w0(j)*(4.-3.*g0(j))))
         b3(j)    =  DBLE(0.25*(2.-3.*g0(j)*u0))
         b4       =  1.0D0 - b3(j)

c equation 21 of Toon et al. (1989)

         ak(j)    =  dsqrt(dabs(b1(j)**2 - b2(j)**2))

c equation 22 of Toon et al. (1989)

         gami(j)  =  b2(j)/(b1(j) + ak(j))
         x1         =  ak(j)*DBLE(taul(j))
       
         if(x1.gt.1000.0D0)    x1=1000.0D0

         ee1(j)   =  dexp(-x1)

c         if( x1.gt. 1000.) ee1(j)= 0.

         c1                =  b1(j) - du0
         c2                =  b1(j) + du0
         c_plus(j)         =  b3(j)*c1+b4*b2(j)
         c_minus(j)        =  b4*c2+b3(j)*b2(j)

         if (c_frctn(j) .eq. 0.0) then
           call get_ww(gami(j), ak(j), taul(j), 
     &                 1.0d0, 1.0d0, 0.0, 
     &                 ww13p_r, ww13m_r, ww24p_r, ww24m_r)

           ww13p0(j) = ww13p_r
           ww13m0(j) = ww13m_r
c           ww24p0(j) = ww24p_r
           ww24p0(j) = 0.0
           ww24m0(j) = ww24m_r

           call get_ww(gami(j), ak(j), taul(j), 
     &                 1.0d0, 1.0d0, 1.0, 
     &                 ww13p_r, ww13m_r, ww24p_r, ww24m_r)

           ww13p1(j) = ww13p_r
           ww13m1(j) = ww13m_r
           ww24p1(j) = ww24p_r
           ww24m1(j) = ww24m_r

         else

           wd1 = 1.0D0
           wr1 = 1.0D0
           wd2 = 1.0D0
           wr2 = 1.0D0
        
           if (nu(j) .lt. 100.0D0) then
c gwtsa 
!             c_corr = 0.0
	       
                if (cum_c_frctn(j1) .gt. 0.0 
     &              .and. c_frctn(j) .gt. 0.0) then
                  c_corr = DBLE(0.063 * u0 * (2.0 - u0) * cef(j))
                else
       		  c_corr=0.0D0
                endif 

             call get_ww_ee_gwtsa
     &           (gami(j), ak(j), taul(j), wd2, wd1, nu(j), u0, 0.0,
     &            c_corr,
     &            ww13p0_d, ww13m0_d, ww24p0_d, ww24m0_d, 
     &            ww13p1_d, ww13m1_d, ww24p1_d, ww24m1_d)           

           else

             call get_ww(gami(j), ak(j), taul(j), wd1, wd2, 0.0, 
     &             ww13p0_d, ww13m0_d, ww24p0_d, ww24m0_d)

             call get_ww(gami(j), ak(j), taul(j), wd1, wd2, 1.0, 
     &             ww13p1_d, ww13m1_d, ww24p1_d, ww24m1_d)
           end if
       
           ww13p0(j) = ww13p0_d 
           ww13m0(j) = ww13m0_d
           ww24p0(j) = ww24p0_d
           ww24m0(j) = ww24m0_d


           ww13p1(j) = ww13p1_d
           ww13m1(j) = ww13m1_d
           ww24p1(j) = ww24p1_d
           ww24m1(j) = ww24m1_d

         endif

c --------------------------------------------------------------------------
         e2(j) = ww13p1(j)

         e4(j) = ww24p1(j)

         e1(j) = ww13p0(j)

         e3(j) = ww24p0(j)

         e5(j) = ww13m0(j)

         e7(j) = ww24m0(j) 

         e6(j) = ww13m1(j)

         e8(j) = ww24m1(j)
c-----------------------------------------------------------------
   14  continue
        
	      	     
c
c     we seek to solve ax(l-1)+bx(l)+ex(l+1) = d.
c     l=2n for even l, l=n+1 for odd l. the mean intensity (tmi/4pi)
c     and the net flux (fnet) are related to x's as noted in add.
c     first we set up the coefficients that are independent of solar
c     angle or temparature: a(i),b(i),e(i). d(i) is defined in add.
c
      j                   =  1
      do 18 jd               =  2,jn,2
        j               =  j + 1

c     here are the even matrix elements eq. 42 of Toon et al.

        af(jd)   = e2(j) * e7(j+1) - e6(j) * e3(j+1)
        bf(jd)   = e4(j) * e7(j+1) - e8(j) * e3(j+1)
        ef(jd)   = e5(j+1) * e3(j+1) - e1(j+1) * e7(j+1)

c     here are the odd matrix elements except for the top.
c     eq. 41 in Toon et al.

        af(jd+1) =  e4(j) * e6(j) - e8(j) * e2(j)
        bf(jd+1) =  e5(j+1) * e2(j) - e1(j+1) * e6(j)
        ef(jd+1) = -e3(j+1) * e6(j) + e7(j+1) * e2(j)

   18  continue
c
c     here are the top and bottom boundary conditions as well as the
c     beginning of the tridiagonal solution definitions. I assume
c     no diffuse radiation is incident at upper boundary.
c

      af(1)     = 0.0
      bf(1)     = -e8(1)
      ef(1)     = e7(2)

      af(2)     = 0.0

      if (rsfx .gt. 0.0) then
        af(jdble) = rsfx * e2(nlayer) - e6(nlayer)
        bf(jdble) = rsfx * e4(nlayer) - e8(nlayer)

      else
       ef(jdble-1) = rsfx
     &   * (-e3(nlayer) * e6(nlayer-1) + e7(nlayer) * e2(nlayer-1))
        af(jdble) = e2(nlayer)
        bf(jdble) = 1.0D0 - rsfx * e4(nlayer)
      end if

      ef(jdble) = 0.0

        
      return
      end



      subroutine get_ww(gami, ak, taul, wm, wp, xi,
     &          ww13p, ww13m, ww24p, ww24m)


c Input
c gami    Gamma defined by Eq. (22) in Toon et al.
c ak      lambda defined by Eq. (21) in Toon et al.
c taul    Optical thickness of the layer
c xi      xi = 0 at the top of the layer
c         xi = 1 at the bottom of the layer
c wp      Weighting function of incident upward diffuse
c         from the bottom of the layer
c wm      Weighting function of incident downward
c         diffuse from the top of the layer
c
c Output
c ww13p
c ww13m
c ww24p
c ww24m
   
      implicit none

      real taul, xi
      real*8 ak, gami, wp, wm
      real*8 l1, l2, l3, l4
      real*8 denom
      real*8 e1wm, e2wp, e3wm, e4wp
      real*8 ww13m, ww24m, ww13p, ww24p


      denom = 1.0D0 - dexp(-2.0D0 * ak * DBLE(taul)) * gami**2
      l1 = gami*dexp(-2.0D0*ak*DBLE(taul)+DBLE(xi)*ak*DBLE(taul))
      l2 = gami * dexp(-ak * DBLE(taul) - DBLE(xi) * ak * DBLE(taul))
      l3 = -dexp(DBLE(-xi) * ak * DBLE(taul))
      l4 = -dexp(-ak * DBLE(taul) + DBLE(xi) * ak * DBLE(taul))

      l1 = l1 / denom
      l2 = l2 / denom
      l3 = l3 / denom
      l4 = l4 / denom

      e1wm = l1 * wm
      e2wp = l2 * wp
      e3wm = l3 * wm
      e4wp = l4 * wp

      ww13m = e1wm + e3wm * gami
      ww24m = e4wp + e2wp * gami
      ww13p = e1wm * gami + e3wm
      ww24p = e4wp * gami + e2wp

      return
      end


      subroutine 
     &  get_ww_ee_gwtsa(gamma, lambda, tau, c_plus, c_minus, 
     &            nu, u0, cu0, c_corr, 
     &            ww13p0, ww13m0, ww24p0, ww24m0, 
     &            ww13p1, ww13m1, ww24p1, ww24m1)

c  JLD opt code logical and integer values follow
      logical g1_active, g2_active
      integer I
c  JLD opt code logical and integer values end
      real tau, u0, cu0
      real*8 gamma, lambda, c_plus, c_minus, nu
      real*8 beta
      real*8 g1_first_term0, g1_second_term0
      real*8 g1_first_term1, g1_second_term1
      real*8 g1_oldfirst_term0, g1_oldsecond_term0
      real*8 g1_oldfirst_term1, g1_oldsecond_term1
      real*8 g2_first_term0, g2_second_term0
      real*8 g2_first_term1, g2_second_term1
      real*8 g2_oldfirst_term0, g2_oldsecond_term0
      real*8 g2_oldfirst_term1, g2_oldsecond_term1
      real*8 ww13p0, ww13m0, ww24p0, ww24m0
      real*8 ww13p1, ww13m1, ww24p1, ww24m1
      real*8 e1wm0, e2wp0, e3wm0, e4wp0
      real*8 e1wm1, e2wp1, e3wm1, e4wp1
      real*8 c_corr
c   JLD opt code data arrays and scalars follow
      real*8 odd_100(100), evn_100(100), gam_odd, gam_evn
      real*8 comdiv0, comdiv1, xi, x, y, z, enorm, ri
      real*8 zcu0p1, oddx, evenx, twoix, zp1
c   JLD opt code data structures end

      real*8 epsilon, delta

      data epsilon / 1.0d-100 /
      data delta / 1.0d-5 /
c   JLD opt code initialization follows:

      data odd_100 /
     + 1.0D0,  3.0D0,  5.0D0,  7.0D0,  9.0D0,
     +11.0D0, 13.0D0, 15.0D0, 17.0D0, 19.0D0,
     +21.0D0, 23.0D0, 25.0D0, 27.0D0, 29.0D0,
     +31.0D0, 33.0D0, 35.0D0, 37.0D0, 39.0D0,
     +41.0D0, 43.0D0, 45.0D0, 47.0D0, 49.0D0,
     +51.0D0, 53.0D0, 55.0D0, 57.0D0, 59.0D0,
     +61.0D0, 63.0D0, 65.0D0, 67.0D0, 69.0D0,
     +71.0D0, 73.0D0, 75.0D0, 77.0D0, 79.0D0,
     +81.0D0, 83.0D0, 85.0D0, 87.0D0, 89.0D0,
     +91.0D0, 93.0D0, 95.0D0, 97.0D0, 99.0D0,
     +101.0D0, 103.0D0, 105.0D0, 107.0D0, 109.0D0,
     +111.0D0, 113.0D0, 115.0D0, 117.0D0, 119.0D0,
     +121.0D0, 123.0D0, 125.0D0, 127.0D0, 129.0D0,
     +131.0D0, 133.0D0, 135.0D0, 137.0D0, 139.0D0,
     +141.0D0, 143.0D0, 145.0D0, 147.0D0, 149.0D0,
     +151.0D0, 153.0D0, 155.0D0, 157.0D0, 159.0D0,
     +161.0D0, 163.0D0, 165.0D0, 167.0D0, 169.0D0,
     +171.0D0, 173.0D0, 175.0D0, 177.0D0, 179.0D0,
     +181.0D0, 183.0D0, 185.0D0, 187.0D0, 189.0D0,
     +191.0D0, 193.0D0, 195.0D0, 197.0D0, 199.0D0 /
	 
      data evn_100 /
     + 0.0D0,  2.0D0,  4.0D0,  6.0D0,  8.0D0,
     +10.0D0, 12.0D0, 14.0D0, 16.0D0, 18.0D0,
     +20.0D0, 22.0D0, 24.0D0, 26.0D0, 28.0D0,
     +30.0D0, 32.0D0, 34.0D0, 36.0D0, 38.0D0,
     +40.0D0, 42.0D0, 44.0D0, 46.0D0, 48.0D0,
     +50.0D0, 52.0D0, 54.0D0, 56.0D0, 58.0D0,
     +60.0D0, 62.0D0, 64.0D0, 66.0D0, 68.0D0,
     +70.0D0, 72.0D0, 74.0D0, 76.0D0, 78.0D0,
     +80.0D0, 82.0D0, 84.0D0, 86.0D0, 88.0D0,
     +90.0D0, 92.0D0, 94.0D0, 96.0D0, 98.0D0,
     +100.0D0, 102.0D0, 104.0D0, 106.0D0, 108.0D0,
     +110.0D0, 112.0D0, 114.0D0, 116.0D0, 118.0D0,
     +120.0D0, 122.0D0, 124.0D0, 126.0D0, 128.0D0,
     +130.0D0, 132.0D0, 134.0D0, 136.0D0, 138.0D0,
     +140.0D0, 142.0D0, 144.0D0, 146.0D0, 148.0D0,
     +150.0D0, 152.0D0, 154.0D0, 156.0D0, 158.0D0,
     +160.0D0, 162.0D0, 164.0D0, 166.0D0, 168.0D0,
     +170.0D0, 172.0D0, 174.0D0, 176.0D0, 178.0D0,
     +180.0D0, 182.0D0, 184.0D0, 186.0D0, 188.0D0,
     +190.0D0, 192.0D0, 194.0D0, 196.0D0, 198.0D0 /

      g1_first_term0 = 0.0
      g1_second_term0 = 0.0
      g1_first_term1 = 0.0
      g1_second_term1 = 0.0
      g1_oldfirst_term0 = 0.0
      g1_oldsecond_term0 = 0.0
      g1_oldfirst_term1 = 0.0
      g1_oldsecond_term1 = 0.0
      g1_active = .TRUE.

      g2_first_term0 = 0.0
      g2_second_term0 = 0.0
      g2_first_term1 = 0.0
      g2_second_term1 = 0.0
      g2_oldfirst_term0 = 0.0
      g2_oldsecond_term0 = 0.0
      g2_oldfirst_term1 = 0.0
      g2_oldsecond_term1 = 0.0
      g2_active = .TRUE.
c
c  JLD opt code - compute the invariants from g_one_n and g_two_n
c  JLD opt code - prior to calling them (halves the computations)
      xi = 1.0D0
      x = (xi * lambda * DBLE(tau)) / nu
      y = (xi * DBLE(cu0 * tau)) / (DBLE(u0) * nu)
      z = (c_corr * DBLE(tau)) / (DBLE(u0) * nu)
      zp1 = z + 1.0D0
      enorm = (1.0D0 / zp1)**(nu)
      zcu0p1 = 1.0D0 + DBLE(cu0) * z

c  JLD opt code initialization ends
c
c  JLD opt code - g_one_n and g_two_n shared 2 common terms that are
c  JLD opt code - invariant.  In g_one_n, the second_term0 divisor
c  JLD opt code - matched the first_term0 divisor in g_two_n.  In
c  JLD opt code - g_one_n, the first_term1 divisor matched the
c  JLD opt code - second_term1 divisor in g_two_n.  Since they are
c  JLD opt code - invariant they can be computed here instead of
c  JLD opt code - 100 times per call in each of g_one_n and g_two_n.
c  JLD opt code
c  JLD opt code - All the terms in g_one_n and g_two_n shared the
c  JLD opt code - even and odd numerators (gamma**even and
c  JLD opt code - gamma**odd where 0 <= even <= 198 by 2 and
c  JLD opt code - 1 <= odd <= 199 by 2)
c  JLD opt code
c  JLD opt code - g_one_n and g_two_n have been eliminated since they
c  JLD opt code - required many common and invariant terms that
c  JLD opt code - involve **, *, -, +, and / operations.  First
c  JLD opt code - analysis revealed that during daytime samples
c  JLD opt code - CPU resource was > 23% for each of the 2 
c  JLD opt code - subroutines.  The CPU resource was concentrated
c  JLD opt code - in the POW intrinsic code due to the usage of the
c  JLD opt code - ** operator.  The revised code minimizes the
c  JLD opt code - the ** usage as well as multiplications.  The
c  JLD opt code - compiler can optimize some of the invariant code
c  JLD opt code - but not too well across subroutines.  Now, let the
c  JLD opt code - compiler do its work on this code.  Test results
c  JLD opt code - indicate that the CPU resource for this SR has
c  JLD opt code - been reduced from > 46.5% to 36.4%.  :>)
c  JLD opt code - The 36.4% CPU resource demand for this SR is
c  JLD opt code - now almost entirely due to the ** operations!

      DO 1000 I = 1, 100
        ri = DBLE(I)
        twoix = 2.0D0 * ri * x
        oddx = odd_100(I) * x
        evenx = evn_100(I) * x
        gam_odd = gamma**odd_100(I)
        gam_evn = gamma**evn_100(I)
        comdiv0 = (zcu0p1 + oddx + y)**nu
        comdiv1 = (zp1 + oddx)**nu 
        
        if(g1_active) then
          g1_first_term0 = g1_first_term0 + gam_odd / ((zp1+twoix)**nu)

          g1_second_term0 = g1_second_term0 + gam_evn / comdiv0
          
          g1_first_term1 = g1_first_term1 + gam_odd / comdiv1

          g1_second_term1 = g1_second_term1 + 
     &    gam_evn / ((zcu0p1 + evenx + y)**nu)

          if (g1_first_term0 - g1_oldfirst_term0 .gt. delta .or.
     &      g1_second_term0 - g1_oldsecond_term0 .gt. delta .or. 
     &      g1_first_term1 - g1_oldfirst_term1   .gt. delta .or.
     &      g1_second_term1 - g1_oldsecond_term1 .gt. delta) then
     
            g1_oldfirst_term0 = g1_first_term0
            g1_oldsecond_term0 = g1_second_term0
            g1_oldfirst_term1 = g1_first_term1
            g1_oldsecond_term1 = g1_second_term1
          else
            g1_active = .FALSE.
          end if
        end if
        
        if(g2_active) then
          g2_first_term0 = g2_first_term0 + gam_odd / comdiv0

          g2_second_term0 = g2_second_term0+gam_evn / ((zp1+evenx)**nu)

          g2_first_term1 = g2_first_term1 + 
     &      gam_odd / ((zcu0p1 + twoix + y)**nu)

          g2_second_term1 = g2_second_term1 + gam_evn / comdiv1

          if (g2_first_term0 - g2_oldfirst_term0 .gt. delta .or.
     &      g2_second_term0 - g2_oldsecond_term0 .gt. delta .or. 
     &      g2_first_term1 - g2_oldfirst_term1   .gt. delta .or.
     &      g2_second_term1 - g2_oldsecond_term1 .gt. delta) then
     
            g2_oldfirst_term0 = g2_first_term0
            g2_oldsecond_term0 = g2_second_term0
            g2_oldfirst_term1 = g2_first_term1
            g2_oldsecond_term1 = g2_second_term1     
          else
            g2_active = .FALSE.
          end if
        end if
        
        if(.NOT.(g1_active .or. g2_active)) exit
 1000 continue

      if (enorm .ne. 0.0) then
        e1wm0 = c_minus * g1_first_term0 / enorm
        e1wm1 = c_minus * g1_first_term1 / enorm
        e3wm0 = -c_minus * g2_second_term0 / enorm
        e3wm1 = -c_minus * g2_second_term1 / enorm
        if (cu0 .eq. 0.0) then
          e4wp0 = -c_plus * g1_second_term0
          e4wp1 = -c_plus * g1_second_term1
          e2wp0 = c_plus * g2_first_term0
          e2wp1 = c_plus * g2_first_term1
        else
          e4wp0 = -c_plus * g1_second_term0 / enorm
          e4wp1 = -c_plus * g1_second_term1 / enorm
          e2wp0 = c_plus * g2_first_term0 / enorm
          e2wp1 = c_plus * g2_first_term1 / enorm
        end if
      else
        e1wm0 = c_minus * g1_first_term0
        e4wp0 = -c_plus * g1_second_term0
        e1wm1 = c_minus * g1_first_term1
        e4wp1 = -c_plus * g1_second_term1
        e2wp0 = c_plus * g2_first_term0
        e3wm0 = -c_minus * g2_second_term0
        e2wp1 = c_plus * g2_first_term1
        e3wm1 = -c_minus * g2_second_term1
      end if

      if (dabs(e4wp0) .lt. epsilon) e4wp0 = epsilon
      if (dabs(e4wp1) .lt. epsilon) e4wp1 = epsilon
      if (dabs(e2wp0) .lt. epsilon) e2wp0 = epsilon
      if (dabs(e2wp1) .lt. epsilon) e2wp1 = epsilon

        ww13m0 = e1wm0 + e3wm0 * gamma
        ww24m0 = e4wp0 + e2wp0 * gamma
        ww13p0 = e1wm0 * gamma + e3wm0
        ww24p0 = e4wp0 * gamma + e2wp0

        ww13m1 = e1wm1 + e3wm1 * gamma
        ww24m1 = e4wp1 + e2wp1 * gamma
        ww13p1 = e1wm1 * gamma + e3wm1
        ww24p1 = e4wp1 * gamma + e2wp1

      return
      end

       subroutine 
     &  g_one_n_int(gamma, lambda, c_plus, c_minus, nu, tau, u0, cu0, 
     &          c_corr,
     &          e1wm0, e4wp0, e1wm1, e4wp1)

      real tau, xi, u0, cu0
      real*8 c_corr
      real*8 gamma, lambda, c_plus, c_minus, nu
      real*8 first_term0, second_term0
      real*8 first_term1, second_term1
      real*8 oldfirst_term0, oldsecond_term0
      real*8 oldfirst_term1, oldsecond_term1
      real*8 x, y, z, e1wm0, e4wp0, e1wm1, e4wp1, enorm
      real*8 epsilon
      real*8 denom1, denom2, denom3, denom4
      real   delta

      integer inu

      data epsilon / 1.0d-100 /
      data delta   / 1.0e-5 /

      first_term0 = 0.0
      second_term0 = 0.0
      first_term1 = 0.0
      second_term1 = 0.0

      oldfirst_term0 = first_term0
      oldsecond_term0 = second_term0
      oldfirst_term1 = first_term0
      oldsecond_term1 = second_term0

      xi = 1.0
c JLD Fixes follow for mixed mode arithmetic
      x = (DBLE(xi) * lambda * DBLE(tau)) / nu
      y = DBLE((cu0 * xi * tau)) / (DBLE(u0) * nu)
      z = (c_corr * DBLE(tau)) / (DBLE(u0) * nu)

      inu = idnint(nu)

      do 1000 i = 1, 100
        ri = real(i)
        denom1 = 1.0D0
        denom2 = 1.0D0
        denom3 = 1.0D0
        denom4 = 1.0D0

        do 1100 j = 1, inu
          denom1 = denom1 * (1.0D0 + z + 2.0D0*DBLE(ri)*x)
          denom2 = denom2 * (1.0D0 + z + (2.0D0*DBLE(ri)-1.0D0)*x + y)
          denom3 = denom3 * (1.0D0 + z + (2.0D0*DBLE(ri)-1.0D0)*x)
          denom4 = denom4 * (1.0D0 + z + (2.0D0*DBLE(ri)-2.0D0)*x + y)
 1100   continue

        first_term0 = first_term0 + 
     &    (gamma**(2.0*ri-1.0)) / denom1

        second_term0 = second_term0 + 
     &    (gamma**(2.0*ri-2.0)) / denom2

        first_term1 = first_term1 + 
     &    (gamma**(2.0*ri-1.0)) / denom3

        second_term1 = second_term1 + 
     &    (gamma**(2.0*ri-2.0)) / denom4

        if (first_term0 - oldfirst_term0 .lt. delta .and.
     &      second_term0 - oldsecond_term0 .lt. delta .and. 
     &      first_term0 - oldfirst_term1 .lt. delta .and.
     &      second_term0 - oldsecond_term1 .lt. delta) goto 2000

        oldfirst_term0 = first_term0
        oldsecond_term0 = second_term0
        oldfirst_term1 = first_term0
        oldsecond_term1 = second_term0

 1000 continue 

c computing normalization factor
 2000 enorm = (1.0D0 / (1.0D0 + z))**(nu)

      if (enorm .ne. 0.0) then
        e1wm0 = c_minus * first_term0 / enorm
        e4wp0 = -c_plus * second_term0 / enorm
        e1wm1 = c_minus * first_term1  / enorm
        e4wp1 = -c_plus * second_term1 / enorm
      else
        e1wm0 = c_minus * first_term0
        e4wp0 = -c_plus * second_term0
        e1wm1 = c_minus * first_term1
        e4wp1 = -c_plus * second_term1
      end if

!      if (abs(e1wp0) .lt. epsilon) 
!     &    e1wp0 = epsilon
!      if (abs(e1wp1) .lt. epsilon) 
!     &    e1wp1 = epsilon
      if (dabs(e4wp0) .lt. epsilon) 
     &    e4wp0 = epsilon
      if (dabs(e4wp1) .lt. epsilon) 
     &    e4wp1 = epsilon

      return
      end

      subroutine 
     &  g_two_n_int(gamma, lambda, c_plus, c_minus, nu, tau, u0, cu0,
     &          c_corr, 
     &          e2wp0, e3wm0, e2wp1, e3wm1)

      real tau, xi, u0, cu0
      real*8 c_corr
      real*8 gamma, lambda, c_plus, c_minus, nu
      real*8 first_term0, second_term0
      real*8 first_term1, second_term1
      real*8 x, y, z, e2wp0, e3wm0, e2wp1, e3wm1, enorm
      real*8 epsilon
      real*8 denom1, denom2, denom3, denom4
      real   delta

      integer inu

      data epsilon / 1.0d-100 /
      data delta   / 1.0e-5 /

      g2 = 0.0
      first_term0 = 0.0
      second_term0 = 0.0
      first_term1 = 0.0
      second_term1 = 0.0

      oldfirst_term0 = first_term0
      oldsecond_term0 = second_term0
      oldfirst_term1 = first_term0
      oldsecond_term1 = second_term0

      xi = 1.0
c JLD Fixes for mixed mode arithmetic follow
      x = (DBLE(xi) * lambda * DBLE(tau)) / nu
      y = DBLE((cu0 * xi * tau)) / (DBLE(u0) * nu)
      z = (c_corr * DBLE(tau)) / (DBLE(u0) * nu)

      inu = idnint(nu)

      do 1000 i = 1, 100
        ri = real(i)
        denom1 = 1.0D0
        denom2 = 1.0D0
        denom3 = 1.0D0
        denom4 = 1.0D0

        do 1100 j = 1, inu
          denom1 = denom1 * (1.0D0 + z + DBLE((2.0*ri-1.0))*x + y)
          denom2 = denom2 * (1.0D0 + z + DBLE((2.0*ri-2.0))*x)
          denom3 = denom3 * (1.0D0 + z + DBLE(2.0*ri)*x + y)
          denom4 = denom4 * (1.0D0 + z + DBLE((2.0*ri-1.0))*x)
 1100   continue

        first_term0 = first_term0 + 
     &    (gamma**(2.0*ri-1.0)) / denom1

        second_term0 = second_term0 + 
     &    (gamma**(2.0*ri-2.0)) / denom2

        first_term1 = first_term1 + 
     &    (gamma**(2.0*ri-1.0)) / denom3

        second_term1 = second_term1 + 
     &    (gamma**(2.0*ri-2.0)) / denom4

        if (first_term0 - oldfirst_term0 .lt. delta .and.
     &      second_term0 - oldsecond_term0 .lt. delta .and. 
     &      first_term0 - oldfirst_term1 .lt. delta .and.
     &      second_term0 - oldsecond_term1 .lt. delta) goto 2000

        oldfirst_term0 = first_term0
        oldsecond_term0 = second_term0
        oldfirst_term1 = first_term0
        oldsecond_term1 = second_term0

 1000 continue

c computing normalization factor
 2000 enorm = (1.0D0 / (1.0D0 + z))**(nu)

      if (enorm .ne. 0.0) then
        e2wp0 = c_plus * first_term0 / enorm
        e3wm0 = -c_minus * second_term0 / enorm
        e2wp1 = c_plus * first_term1 / enorm
        e3wm1 = -c_minus * second_term1 / enorm
      else
        e2wp0 = c_plus * first_term0
        e3wm0 = -c_minus * second_term0
        e2wp1 = c_plus * first_term1
        e3wm1 = -c_minus * second_term1
      end if

      if (dabs(e2wp0) .lt. epsilon) 
     &    e2wp0 = epsilon
      if (dabs(e2wp1) .lt. epsilon) 
     &    e2wp1 = epsilon
!      if (abs(e3wp0) .lt. epsilon) 
!     &    e3wp0 = epsilon
!      if (abs(e3wp1) .lt. epsilon) 
!     &    e3wp1 = epsilon

      return
      end


      subroutine gwtsa_add
     &  (nlayer,rsfx,u0,opd,opdnd,taul,taulnd,w0,g0,
     &     nu,c_frctn,cum_c_frctn,taul_clear,taulnd_clear,
     &     e1,e2,e3,e4,e5,e6,e7,e8,af,bf,ef,cef,cefnd,
     &     c_plus,c_minus,ak,
     &     gami,
     &     sol_fluxup, sol_fluxdn, direct_nd)

c     ***************************************************************
c     *  purpose             :  defines source terms, form matrix   *
c     *                         for multiple layers and solve tri-  *
c     *                         diagnol equations to obtain mean    *
c     *                         intensity and net flux.             *
c     *  subroutines called  :  get_ee, get_ww_ee_gwtsa             *
c     *  input               :  nlayer,rsfx,u0                      *
c     *                      :  opd,opdnd,taul,taulnd,w0,g0         *
c     *                      :  nu,c_frctn,cum_c_frctn              *
c     *                      :  e1,e2,e3,e4,e5,e6,e7,e8,af,bf,ef    *
c     *                      :  c_plus,c_minus,ak                   *
c     *  output              :  fnet,sol_fluxup,sol_fluxdn          *
c     *                      :  direct_nd                           *
c     * *************************************************************

c define the dimensions used by the radiation model but might be
c specified by an external model

c ivert  = maximum number of layers;
c ilayer = maximum number of layer boundaries
c idbl   = twice the maximum number of layer boundaries
      USE FUINPUT ,only: nvx
      implicit none

      integer ivert, ilayer, idbl

      parameter (ivert=nvx)
      parameter (ilayer=ivert+1, idbl=2*ilayer)

c  double precision


      real taul(*), taulnd(*), w0(*), g0(*), opd(*), opdnd(*)
      real cef(*), cefnd(*)
      real rsfx, u0, dicld
      real taul_clear(*),taulnd_clear(*)

      real*8 nu(*), c_frctn(*), cum_c_frctn(*)

      real*8 e1(ilayer), e2(ilayer), e3(ilayer), e4(ilayer)
      real*8 e5(ilayer), e6(ilayer), e7(ilayer), e8(ilayer)
      real*8 c_plus(ilayer), c_minus(ilayer)
      real*8 ak(ilayer)
      real*8 gami(ilayer)
      real*8 el3(ilayer), ee3(ilayer), ee3nd(ilayer)
      real*8 el3nd(ilayer)
      real*8 ee13p_r, ee13m_r, ee24p_r, ee24m_r
      real*8 ee13p0(ilayer), ee13m0(ilayer)
      real*8 ee24p0(ilayer), ee24m0(ilayer)
      real*8 ee13p1(ilayer), ee13m1(ilayer)
      real*8 ee24p1(ilayer), ee24m1(ilayer)
      real*8 cm2_d, cp2_d, cm2_r, cp2_r
      real*8 c_corr

      real*8 sol_fluxup(ilayer), sol_fluxdn(ilayer)
      real*8 direct(ilayer)
      real*8 direct_nd(ilayer)
      real*8 cpb(ilayer), cp(ilayer), cmb(ilayer), cm(ilayer)
      real*8 as(idbl),af(idbl), bf(idbl), df(idbl), ds(idbl)
      real*8 ef(idbl), xk(idbl)

      real*8 epsilon, sfcs
      real*8 x1, x2, x, x2_clear
      real*8 x1nd, x2nd, x2nd_clear
      real*8  xj1, xj2, xj1nd, xj2nd
      real*8 c2, c2_clear
      real*8 cp1, cm1
      real*8 ee13p0_d, ee13m0_d, ee24p0_d, ee24m0_d 
      real*8 ee13p1_d, ee13m1_d, ee24p1_d, ee24m1_d
c JLD fix for making du0 double precision
c      real sol, sq3, du0
      real*8 du0
      real sol, sq3
      integer nlayer, jdble, jn, isl, irs, j, j1, jd
      integer icld
c JLD Fix for single prec literal assigned to dbl prec var
c       data epsilon / 1.0e-15 /
       data epsilon / 1.0D-15 /
       
       jdble = nlayer * 2 - 2
       jn = jdble - 1
       sol = 1.0
       sq3 = 3.0**(1.0/2.0)
       icld = 0 

       c_corr=0.0 !Seiji
c  flag for solar(isl) and IR(irs)
       isl = 1
       irs = 0

c     this subroutine forms the matrix for the multiple layers and
c     uses a tridiagonal routine to find radiation in the entire
c     atmosphere.
c
c     ******************************
c     *   calculations for solar   *
c     ******************************
      if(isl .ne. 0)  then
        du0                =  1.0D0/DBLE(u0)
       
  

        do 10 j            =  1,nlayer

          if(j.ne.1) then

            j1=j-1

          else

            j1=j

          endif
      
c computing direct irradiance ----------------------------------------------
c with delta approximation
c JLD Fixes for mixed mode arithmetic follow
          x1          =  DBLE(opd(j))*du0
          x2          =  DBLE(taul(j))*du0
          x2_clear    =  DBLE(taul_clear(j))*du0
          if(x1.gt.1000.0D0)  x1 = 1000.0D0
          if(x2.gt.1000.0D0)  x2 = 1000.0D0
          if(x2_clear .gt. 1000.0D0) x2_clear = 1000.0D0

          xj1          =  DBLE(opd(j1))*du0
          xj2          =  DBLE(taul(j1))*du0
          if(xj1.gt.1000.0D0)  xj1 = 1000.0D0
          if(xj2.gt.1000.0D0)  xj2 = 1000.0D0

          ee3(j)          =  dexp(-x1)
          el3(j)          =  dexp(-x2)

c without delta approximation
          x1nd          =  DBLE(opdnd(j))*du0
          x2nd          =  DBLE(taulnd(j))*du0
          x2nd_clear    =  DBLE(taulnd_clear(j))*du0
          if(x1nd.gt.1000.0D0)  x1nd = 1000.0D0
          if(x2nd.gt.1000.0D0)  x2nd = 1000.0D0
          if(x2nd_clear.gt.1000.0D0)  x2nd_clear = 1000.0D0

          xj1nd          =  DBLE(opdnd(j1))*du0
          xj2nd          =  DBLE(taulnd(j1))*du0
          if(xj1nd.gt.1000.0D0)  xj1nd = 1000.0D0
          if(xj2nd.gt.1000.0D0)  xj2nd = 1000.0D0

          ee3nd(j)          =  dexp(-x1nd)
          el3nd(j)            =  dexp(-x2nd)

          if (j .eq. 1) then
            direct(j) = DBLE(u0)
            direct_nd(j) = DBLE(u0)

            c2          =  ak(j)*ak(j) - du0*du0

            if(dabs(c2).le.epsilon)          c2=epsilon

c equation 23 and 24 in Toon et al. (1989)

            cp1         =  DBLE(w0(j)) * c_plus(j)  / c2
            cm1         =  DBLE(w0(j)) * c_minus(j) / c2

            cp(j)       =  cp1 * direct(j1) * du0
            cm(j)       =  cm1 * direct(j1) * du0
            cpb(j)      =  cp1 * direct(j) * du0
            cmb(j)      =  cm1 * direct(j) * du0
          else
            if (c_frctn(j) .eq. 0.0) then

              direct(j)    = direct(j1) * el3(j)
              direct_nd(j)  = direct_nd(j1) * el3nd(j)

              c2          =  ak(j)*ak(j) - du0*du0

              if(dabs(c2).le.epsilon)          c2=epsilon

c equation 23 and 24 in Toon et al. (1989)

              cp1         =  DBLE(w0(j)) * c_plus(j)  / c2
              cm1         =  DBLE(w0(j)) * c_minus(j) / c2

              cp(j)       =  cp1 * direct(j1) * du0
              cm(j)       =  cm1 * direct(j1) * du0
              cpb(j)      =  cp1 * direct(j) * du0
              cmb(j)      =  cm1 * direct(j) * du0


	      

              call get_ee(gami(j), ak(j), taul(j), cm(j), cpb(j), 0.0,
     &          ee13p_r, ee13m_r, ee24p_r, ee24m_r)

              ee13p0(j) = ee13p_r
              ee13m0(j) = ee13m_r
c              ee24p0(j) = ee24p_r
              ee24p0(j) = 0.0
              ee24m0(j) = ee24m_r

              call get_ee(gami(j), ak(j), taul(j), cm(j), cpb(j), 1.0,
     &          ee13p_r, ee13m_r, ee24p_r, ee24m_r)

              ee13p1(j) = ee13p_r
              ee13m1(j) = ee13m_r
              ee24p1(j) = ee24p_r
              ee24m1(j) = ee24m_r

            else

              if (nu(j) .ge. 100.0D0) then

                direct(j) = direct(j1) * el3(j)
                direct_nd(j) = direct_nd(j1) * el3nd(j)

              else

                direct(j) = direct(j1)
     &                * (1.0D0 + x2 / (nu(j)))**(-nu(j))

                direct_nd(j) = direct_nd(j1)
     &                * (1.0D0 + x2nd / (nu(j)))**(-nu(j))

!               print*, 'cloudtop', j, 1.0 +x2 / nu(j), 
!     &                 1.0 + x2nd/nu(j), taul(j), nu(j),
!     &                 direct_nd(j), direct(j)

              endif

              if (cum_c_frctn(j1) .gt. 0.0 
     &            .and. c_frctn(j) .gt. 0.0) then

                c_corr = DBLE(cef(j)) 

!                direct(j) = direct(j1) * exp(-x2_clear)
!     &                  * ((1.0 + c_corr 
!     &                  * (x2-x2_clear) / nu(j))
!     &                  / (1.0 + (c_corr * (x2-x2_clear)
!     *                   + (x2-x2_clear)) 
!     &                  / nu(j)))**(nu(j))

                direct(j) = direct(j1)
     &                  * ((1.0D0 + c_corr * x2 / nu(j))
     &                  / (1.0D0 + (c_corr * x2 + x2) 
     &                  / nu(j)))**(nu(j))


!               print*, 'no corr', j, 1.0 + c_corr * x2 / nu(j), 
!     &                 1.0 + (c_corr * x2 + x2)/nu(j), taul(j), nu(j),
!     &                 direct(j)

                c_corr = DBLE(cefnd(j)) 

                direct_nd(j) = direct_nd(j1) * dexp(-x2nd_clear)
     &                  * ((1.0D0 + c_corr 
     &                  * (x2nd-x2nd_clear) / nu(j))
     &                  / (1.0D0 + (c_corr * (x2nd-x2nd_clear)
     &                  + (x2nd-x2nd_clear)) 
     &                  / nu(j)))**(nu(j))

!               print*, 'corr', j, 1.0 + c_corr * x2nd / nu(j), 
!     &                 1.0 + (c_corr * x2nd + x2nd)/nu(j), taulnd(j),
!     &                 direct_nd(j)
              end if

c------------------------------------------------------------------------


              c2          =  ak(j)*ak(j) - du0*du0

              if(dabs(c2).le.epsilon)          c2=epsilon


c equation 23 and 24 in Toon et al. (1989)


              cp1         =  DBLE(w0(j)) * c_plus(j)  / c2
              cm1         =  DBLE(w0(j)) * c_minus(j) / c2


              cp(j)   = direct(j1) * cp1 * du0

              cm(j)   = direct(j1) * cm1 * du0 

              cpb(j)  = direct(j) * cp1 * du0 

              cmb(j)  = direct(j) * cm1 * du0 


              if (nu(j) .lt. 100.0D0) then      
c gwtsa 
	      
                cm2_d = direct(j1) * cm1 * du0 
                cp2_d = direct(j1) * cp1 * du0 


                if (cum_c_frctn(j1) .gt. 0.0 
     &              .and. c_frctn(j) .gt. 0.0) then
                  c_corr = cef(j) 
                else
       		  c_corr=0.0
                endif 

                call get_ww_ee_gwtsa                                    
     &          (gami(j), ak(j), taul(j), cp2_d, cm2_d, nu(j), 
     &           u0, 1.0, c_corr,                                                
     &           ee13p0_d, ee13m0_d, ee24p0_d, ee24m0_d,                
     &           ee13p1_d, ee13m1_d, ee24p1_d, ee24m1_d)                
              else

                cm2_d = direct(j1) * cm1 * du0 
                cp2_d = direct(j1) * cp1 * el3(j) * du0 

                call get_ee(gami(j), ak(j), taul(j), cm2_d, cp2_d, 0.0,
     &            ee13p0_d, ee13m0_d, ee24p0_d, ee24m0_d)

                call get_ee(gami(j), ak(j), taul(j), cm2_d, cp2_d, 1.0,
     &            ee13p1_d, ee13m1_d, ee24p1_d, ee24m1_d)
              end if


              ee13p0(j) = ee13p0_d
              ee13m0(j) = ee13m0_d
              ee24p0(j) = ee24p0_d
              ee24m0(j) = ee24m0_d

              ee13p1(j) = ee13p1_d
              ee13m1(j) = ee13m1_d
              ee24p1(j) = ee24p1_d
              ee24m1(j) = ee24m1_d
            end if  
          end if
c ----------------------------------------------------------------
 10     continue
     
!        print*, 'I am here', direct_nd(nlayer), direct(nlayer)

c       calculate sfcs, the source at the bottom.
        
        sfcs         =  direct(nlayer) * DBLE(rsfx)
      end if
c
c     ******************************
c     * calculations for infrared. *
c     ******************************
c
c      if(irs .ne. 0)  then
c
c        do 30 j           =   1,nlayer
c
c          if(j.eq.1) then
c            kindex = 1
c          else
c            kindex = j-1
c          endif
c
c          b3(j)     = 1.0/(b1(j)+b2(j))
c          cp(j)     = (ptemp(kindex)+slope(j)*b3(j))*u1s
c          cpb(j)    = cp(j) + slope(j)*taul(j)*u1s
c          cm(j)     = (ptemp(kindex)-slope(j)*b3(j))*u1s
c          cmb(j)    = cm(j) + slope(j)*taul(j)*u1s
c          el3(j)    = 0.0
c          direct(j) = 0.0
c          ee3(j)    = 0.0
c
c 30     continue
c
c        sfcs          = emis*ptempg*pi
c
c      end if

      j                =  1

      do 42 jd         =  2,jn,2
        j             =  j + 1

c           here are the even matrix elements
        df(jd) = (ee13m0(j+1) + ee24m0(j+1) + cp(j+1) 
     &         - ee13m1(j) - ee24m1(j) - cpb(j))*e3(j+1)
     &         + (ee13p1(j) + ee24p1(j) + cmb(j)
     &         - ee13p0(j+1) - ee24p0(j+1) - cm(j+1))*e7(j+1)


c           here are the odd matrix elements except for the top.

        df(jd+1) = (ee13m0(j+1) + ee24m0(j+1) + cp(j+1) 
     &         - ee13m1(j) - ee24m1(j) - cpb(j))*e2(j)
     &         + (ee13p1(j) + ee24p1(j) + cmb(j)
     &         - ee13p0(j+1) - ee24p0(j+1) - cm(j+1))*e6(j)

 42   continue

c     here are the top and bottom boundary conditions as well as the
c     beginning of the tridiagonal solution definitions. i assume no
c     diffuse radiation is incident at the top.
c

       ee13m1(1) =0 !! FRED
       ee24m1(1) =0 !! FRED
 
      df(1)     = ee13m0(2) + ee24m0(2) + cp(2)
     &          - ee13m1(1) - ee24m1(1) - cpb(1)
 

      if (rsfx .gt. 0.0) then
        df(jdble) 
     &    = DBLE(rsfx) * (ee13p1(nlayer) + ee24p1(nlayer) + cmb(nlayer))
     &    - ee13m1(nlayer) - ee24m1(nlayer) - cpb(nlayer) + sfcs
      else
        df(jdble) 
     &    = ee13p1(nlayer) + ee24p1(nlayer) + cmb(nlayer) + sfcs
      end if

      ds(jdble) = df(jdble)/bf(jdble)
      as(jdble) = af(jdble)/bf(jdble)

             	       
c
c     ********************************************
c     *     we solve the tridiagonal equations   *
c     ********************************************
c
c This block is following eq. 45, 46, and 47 in Toon et al. (1998)

      do 47 j           = 2, jdble
        if (dabs(bf(jdble+1-j) - ef(jdble+1-j)*as(jdble+2-j))
     &      .lt. 1.0d-308) then
c          x = abs(bf(jdble+1-j) - ef(jdble+1-j)*as(jdble+2-j))
c     &      / (bf(jdble+1-j) - ef(jdble+1-j)*as(jdble+2-j))
          x = 1.0d308
        else
          x               = 1.0D0/(bf(jdble+1-j) -
     1                        ef(jdble+1-j)*as(jdble+2-j))
        end if

        as(jdble+1-j)   = af(jdble+1-j)*x
        ds(jdble+1-j)   = (df(jdble+1-j) - ef(jdble+1-j)
     2                        *ds(jdble+2-j))*x
  47  continue

      xk(1)    = ds(1)

      do 50 j       = 2, jdble
            xk(j) = ds(j) - as(j)*xk(j-1)
  50  continue

c  ***************************************************************
c     calculate layer coefficients, net flux and mean intensity
c  ***************************************************************
      
      do 60 j = 1, nlayer

        sol_fluxdn(j) = 0.0
        sol_fluxup(j) = 0.0
c        direct_nd(j) = 0.0

 60   continue

      do 62 j= 1,nlayer

        if (j .eq. 1) then
          sol_fluxup(j) = xk(1)
          sol_fluxdn(j) = direct(1)
        else if (j .eq. nlayer) then
          if (rsfx .gt. 0.0) then
            sol_fluxup(j) = xk(2*j - 2)
            sol_fluxdn(j) = xk(2*j - 2) / DBLE(rsfx)
          else
            sol_fluxdn(j) = xk(2*j - 2) + direct(j)
            sol_fluxup(j) = xk(2*j - 2) * DBLE(rsfx)
          end if
        else
          sol_fluxup(j) = xk(2*j - 2)
          sol_fluxdn(j) = xk(2*j - 1) + direct(j)
        endif

   62 continue


 400  format (255f8.1)
 401  format (/, ' fnet for ', i5, 'wavelengths. ')
 402  format (' layer  ', i5, ' of ', i5)

 410  format(i5,17e20.5e3)

      return
      end



      subroutine get_ee(gami, ak, taul, cm, cpb, xi,
     &          ee13p, ee13m, ee24p, ee24m)


c Input
c gami    Gamma defined by Eq. (22) in Toon et al.
c ak      lambda defined by Eq. (21) in Toon et al.
c taul    Optical thickness of the layer
c xi      xi = 0 at the top of the layer
c         xi = 1 at the bottom of the layer
c cm      Source function (pi * f0 = 1) defined by eq 24 in
c         Toon et al. at the top of the layer
c cpb     Source function (pi * f0 = 1) defined by eq 23 in
c         Toon et al. at the bottom of the layer
c
c Output
c ee13p
c ee13m
c ee24p
c ee24m

      implicit none

      real taul, xi
      real*8 ak, gami
      real*8 l1, l2, l3, l4
      real*8 denom
      real*8 e1wm, e2wp, e3wm, e4wp
      real*8 ee13p, ee13m, ee24p, ee24m
      real*8 cm, cpb

c JLD Fixes for mixed mode arithmetic follow
      denom = 1.0D0 - dexp(-2.0D0 * ak * DBLE(taul)) * gami**2
      l1 = gami*dexp(-2.0D0*ak*DBLE(taul)+DBLE(xi)*ak*DBLE(taul))
      l2 = gami * dexp(-ak * DBLE(taul) - DBLE(xi) * ak * DBLE(taul))
      l3 = -dexp(-DBLE(xi) * ak * DBLE(taul))
      l4 = -dexp(-ak * DBLE(taul) + DBLE(xi) * ak * DBLE(taul))

      l1 = l1 / denom
      l2 = l2 / denom
      l3 = l3 / denom
      l4 = l4 / denom

      e1wm = l1 * cm
      e2wp = l2 * cpb
      e3wm = l3 * cm
      e4wp = l4 * cpb

      ee13m = e1wm + e3wm * gami
      ee24m = e4wp + e2wp * gami
      ee13p = e1wm * gami + e3wm
      ee24p = e4wp * gami + e2wp

      return
      end






      subroutine twostr 
     & (nlayer,irflag,taul,w0,g0,rsfx,b1,b2,el1,el2,em1,em2,af,bf,ef,ak,
     &  u1i,u1s,gami,ee1)

c
c    ******************************************************************
c    *  purpose             :  defines matrix properties and sets up  *
c    *                         matrix coefficients that do not depend *
c    *                         on zenith angle or temperature.        *
c    *  subroutines called  :  none                                   *
c    *  input               :  w0, g0                                 *
c    *  output              :  b1, b2, el1, el2, em1, em2, af, bf, ef *
c    * ****************************************************************

c define the dimensions used by the radiation model but might be
c specified by an external model
c
c ivert  = maximum number of layers;
c ilayer = maximum number of layer boundaries
c idbl   = twice the maximum number of layer boundaries
      USE FUINPUT ,only: nvx
      parameter (ivert=nvx)
      parameter (irad=20)
      parameter (ilayer=ivert+1, idbl=2*ilayer)


c  double precision
       implicit real*8 (a-h, o-z)

       real taul(*), w0(*), g0(*)
       real rsfx
       dimension gami(ilayer), ak(ilayer)
       dimension b1(ilayer), b2(ilayer)
       dimension ee1(ilayer)
       dimension el1(ilayer), el2(ilayer), em1(ilayer), em2(ilayer)
       dimension af(idbl), bf(idbl), ef(idbl)
       integer   irflag
c JLD Fix to make u1i real*4
       real u1i

c JLD Fixes for mixed mode arithmetic follow
       if(irflag.eq.0) then
c         u1i = sqrt(3.0)
           u1i = 2.0
       else
           u1i = 2.0
       endif

       pi = 4.0D0*datan(1.0D0)
       tpi = 2.0D0 * pi
       jdble = 2 * nlayer
       jn = jdble - 1
       u1s  =  tpi/DBLE(u1i)


c      here we define layer properties following general scheme
c      of meador and weavor. then we set up layer properties
c      needed for matrix.

       do 14 j           = 1,nlayer

c      these are for twostream and hemispheric means
c These b1 and b2 are gamma1 and 2 defined in Table 1 in Toon at al. (1989)
c
       if(irflag.eq.0) then

c Eddington
         b1(j)    =  DBLE(0.25*(7. - w0(j)*(4. + 3.*g0(j))))
         b2(j)    =  DBLE(-0.25*(1. - w0(j)*(4.-3.*g0(j))))
       else
c Quadrature
         b1(j)    =  DBLE(0.5*u1i*(2. - w0(j)*(1. + g0(j))))
         b2(j)    =  DBLE(0.5*u1i*w0(j)*(1. - g0(j)))
       endif


c equation 21 of Toon et al. (1989)

         ak(j)    =  dsqrt(dabs(b1(j)**2 - b2(j)**2))

c equation 22 of Toon et al. (1989)

         gami(j)  =  b2(j)/(b1(j) + ak(j))
         x1         =  ak(j)*DBLE(taul(j))

         if(x1.gt.1000.0D0)    x1=1000.0D0

         ee1(j)   =  dexp(-x1)

         if( x1.gt. 1000.0D0) ee1(j)= 0.0D0

c equation 44 of Toon et al., (1989)

         el1(j)   =  1.0D0 + gami(j) *ee1(j)
         em1(j)   =  1.0D0 - gami(j) * ee1(j)
         el2(j)   =  gami(j) + ee1(j)
         em2(j)   =  gami(j) - ee1(j)

   14  continue
c
c     we seek to solve ax(l-1)+bx(l)+ex(l+1) = d.
c     l=2n for even l, l=n+1 for odd l. the mean intensity (tmi/4pi)
c     and the net flux (fnet) are related to x's as noted in add.
c     first we set up the coefficients that are independent of solar
c     angle or temparature: a(i),b(i),e(i). d(i) is defined in add.
c
      j                   =  0
      do 18 jd               =  2,jn,2
        j               =  j + 1

c     here are the even matrix elements eq. 42 of Toon et al.

        af(jd)   = em1(j+1)*el1(j)-em2(j+1)*el2(j)
        bf(jd)   = em1(j+1)* em1(j)-em2(j+1)*em2(j)
        ef(jd)   = el1(j+1)*em2(j+1) - el2(j+1)*em1(j+1)

c     here are the odd matrix elements except for the top.
c     eq. 41 in Toon et al.

        af(jd+1) =  em1(j)*el2(j)-el1(j)*em2(j)
        bf(jd+1) =  el1(j+1)*el1(j) - el2(j+1)*el2(j)
        ef(jd+1) =  el2(j)*em2(j+1)-el1(j)*em1(j+1)

   18  continue
c
c     here are the top and bottom boundary conditions as well as the
c     beginning of the tridiagonal solution definitions. I assume
c     no diffuse radiation is incident at upper boundary.
c

      jdble = 2 * nlayer

      af(1)     = 0.0
      bf(1) = el1(1)
      ef(1) = -em1(1)
      af(jdble) = el1(nlayer)-rsfx*el2(nlayer)
      bf(jdble) = em1(nlayer)-rsfx*em2(nlayer)
      ef(jdble) = 0.0


      return
      end



      subroutine add
     &  (nlayer,taul,w0,g0,rsfx,opd,opdnd,ak,b1,b2,b3,em1,em2,
     &   el1,el2,af,bf,ef,u0,slope,ptempg,ptemp,u1i,u1s,
     &   sol_fluxup,sol_fluxdn,direct_nd,
     &   irflag,ck1,ck2)

c     ***************************************************************
c     *  purpose             :  defines source terms, form matrix   *
c     *                         for multiple layers and solve tri-  *
c     *                         diagnol equations to obtain mean    *
c     *                         intensity and net flux.             *
c     *  subroutines called  :  none                                *
c     *  input               :  nlayer,taul,w0,g0,rsfx,opd,ak,b1,b2
c     *                         b3,em1,em2,el1,el2,af,bf,ef         *
c     *  output              :  fnet,sol_fluxup,sol_fluxdn          *
c     * *************************************************************

c define the dimensions used by the radiation model but might be
c specified by an external model

c ivert  = maximum number of layers;
c ilayer = maximum number of layer boundaries
c idbl   = twice the maximum number of layer boundaries
      USE FUINPUT ,only: nvx
      parameter (ivert=nvx)
      parameter (ilayer=ivert+1, idbl=2*ilayer)

c  double precision

       implicit real*8 (a-h, o-z)

       real taul(*), w0(*), g0(*)
       real opd(ilayer), opdnd(ilayer)
       real rsfx, u0
       real*8 ptempg
       real*8 u1i,u1s
       dimension ak(ilayer)
       dimension b1(ilayer), b2(ilayer), b3(ilayer)
       dimension el3(ilayer), ee3(ilayer)
       dimension fnet(ilayer), sol_fluxup(ilayer), sol_fluxdn(ilayer)
       dimension direct(ilayer), diffuse(ilayer), tmi(ilayer)
       dimension direct_nd(ilayer)
       dimension cpb(ilayer), cp(ilayer), cmb(ilayer), cm(ilayer)
       dimension slope(ilayer), ptemp(ilayer)
       dimension em1(ilayer), em2(ilayer)
       dimension el1(ilayer), el2(ilayer)
       dimension as(idbl),af(idbl), bf(idbl), df(idbl), ds(idbl)
       dimension ef(idbl), xk(idbl)
       dimension ck1(ilayer), ck2(ilayer)
c JLD Fix for single prec constant stored to double prec var
c       data epsilon / 1.0e-15 /
       data epsilon / 1.0D-15 /
       data pi      / 3.14159265 /
c JLD fixes for mixed mode arithmetic follow
       jdble = nlayer * 2
       jn = jdble - 1
       sol = 1.0D0
       sq3 = 3.0D0**(1.0D0/2.0D0)

c  flag for solar(isl) and IR(irs)
c       isl = 1
c       irs = 0

c     this subroutine forms the matrix for the multiple layers and
c     uses a tridiagonal routine to find radiation in the entire
c     atmosphere.
c
c     ******************************
c     *   calculations for solar   *
c     ******************************
      if(irflag .eq. 0)  then
c JLD Fix for mixed mode arithmetic
c        du0                =  1./u0
        du0                =  1.0D0/DBLE(u0)

        do 10 j            =  1,nlayer

          if(j.ne.1) then

            j1=j-1

          else

            j1=j

          endif

c          b3(j)     =  0.5*(1.-sq3*g0(j)*u0)
c          b4          =  1. - b3(j)

c Eddington
          b3(j)    =  DBLE(0.25*(2.-3.*g0(j)*u0))
          b4       =  1.0D0 - b3(j)

          x2          =  DBLE(taul(j))*du0

          if(x2.gt.1000.0D0)  x2 = 1000.0D0

          ee3(j)    =  dexp(-x2)
          x3          =  DBLE(opd(j))*du0

          if(x3.gt.1000.0D0)  x3 = 1000.0D0

          el3(j)    =  dexp(-x3)*sol

          if(el3(j).ge.1000.0D0)  el3(j)=0.0

          direct(j) = DBLE(u0)*el3(j)
          c1          =  b1(j) - du0
c JLD Fix for c1 == 0.0 to avoid divide by zero
          if(c1 == 0.0D0) then
		    c1 = epsilon
			print*, ' FP:', db_fp, ':SR add, c1 replaced w/epsilon'
		  end if
			
          c2          =  ak(j)*ak(j) - du0*du0
c JLD Fix for mixed mode reference to intrinsic fcn abs
c          if(abs(c2).le.epsilon)   c2=epsilon
          if(dabs(c2).le.epsilon)   c2=epsilon


c equation 23 in Toon et al. (1989)

          cp1         =  DBLE(w0(j))*(b3(j)*c1+b4*b2(j))/c2
          cpb(j)    =  cp1 * el3(j)

          if(j.ne.1) then
            x4 = el3(j1)
          else
            x4 = sol
          endif

          cp(j)     =  cp1 * x4

c equation 24 in Toon et al. (1989)

          cm1         =  ( cp1*b2(j) + DBLE(w0(j))*b4 )/c1
          cmb(j)    =  cm1 * el3(j)
          cm(j)     =  cm1 * x4

 10     continue

c       calculate sfcs, the source at the bottom.

        sfcs         =  direct(nlayer) * DBLE(rsfx)
c
       end if
c
c     ******************************
c     * calculations for infrared. *
c     ******************************

      if(irflag .eq. 1)  then
        emis = 1.0D0 - DBLE(rsfx)

        do 30 j           =   1,nlayer

          if(j.eq.1) then
            kindex = 1
          else
            kindex = j-1
          endif

          b3(j)     = 1.0D0/(b1(j)+b2(j))
          cp(j)     = (ptemp(kindex)+slope(j)*b3(j))*u1s
          cpb(j)    = cp(j) + slope(j)*DBLE(taul(j))*u1s
          cm(j)     = (ptemp(kindex)-slope(j)*b3(j))*u1s
          cmb(j)    = cm(j) + slope(j)*DBLE(taul(j))*u1s
          el3(j)    = 0.0
          direct(j) = 0.0
          ee3(j)    = 0.0

 30     continue

        sfcs          = emis*ptempg*pi

      end if

      j                =  0

      do 42 jd         =  2,jn,2
        j             =  j + 1

c           here are the even matrix elements
        df(jd) = (cp(j+1) - cpb(j))*em1(j+1) -
     $ (cm(j+1) - cmb(j))*em2(j+1)

c           here are the odd matrix elements except for the top.

        df(jd+1) =  el2(j) * (cp(j+1)-cpb(j)) +
     &                    el1(j) * (cmb(j) - cm(j+1))
 42   continue



c     here are the top and bottom boundary conditions as well as the
c     beginning of the tridiagonal solution definitions. i assume no
c     diffuse radiation is incident at the top.
c
      df(1)     = -cm(1)
      df(jdble) = sfcs+DBLE(rsfx)*cmb(nlayer)-cpb(nlayer)
      ds(jdble) = df(jdble)/bf(jdble)
      as(jdble) = af(jdble)/bf(jdble)
c
c     ********************************************
c     *     we solve the tridiagonal equations   *
c     ********************************************
c
c This block is following eq. 45, 46, and 47 in Toon et al. (1998)

      do 47 j           = 2, jdble
        x               = 1./(bf(jdble+1-j) -
     1                        ef(jdble+1-j)*as(jdble+2-j))
        as(jdble+1-j)   = af(jdble+1-j)*x
        ds(jdble+1-j)   = (df(jdble+1-j) - ef(jdble+1-j)
     2                        *ds(jdble+2-j))*x
  47  continue

      xk(1)    = ds(1)

      do 50 j       = 2, jdble
            xk(j) = ds(j) - as(j)*xk(j-1)
  50  continue

c  ***************************************************************
c     calculate layer coefficients, net flux and mean intensity
c  ***************************************************************
      
      do 60 j = 1, nlayer

        sol_fluxdn(j) = 0.0
        sol_fluxup(j) = 0.0
        direct_nd(j) = 0.0

 60   continue

      do 62 j= 1,nlayer

c Yl; l = odd => Yl = Y1n,   l = even => Yl = Y2n

        ck1(j)   = xk(2*j-1)
        ck2(j)   = xk(2*j)

c equation 48 of Toon et al. (1989)

        fnet(j)  = ck1(j)  *( el1(j) -el2(j))   +
     3                 ck2(j) *( em1(j)-em2(j) ) + cpb(j) -
     4                 cmb(j) - direct(j)

c diffuse component of solar radiation

        diffuse(j) = ck1(j)*el2(j) + ck2(j)*em2(j)
     +                                     + cmb(j)
c
        tmi(j)     =  el3(j) + u1i * ( ck1(j)  *
     5                   (el1(j) + el2(j))   +
     6                    ck2(j) * ( em1(j)+em2(j) ) +
     7                    cpb(j) + cmb(j) )

        sol_fluxup(j) = sol_fluxup(j) + ck1(j)*el1(j)
     &                + ck2(j)*em1(j) + cpb(j)
        sol_fluxdn(j) = sol_fluxdn(j) + ck1(j)*el2(j)
     &                + ck2(j)*em2(j)+cmb(j)+direct(j)

        direct_nd(j) = DBLE(u0) * dexp(-DBLE(opdnd(j))*du0)

   62 continue


 400  format (255f8.1)
 401  format (/, ' fnet for ', i5, 'wavelengths. ')
 402  format (' layer  ', i5, ' of ', i5)
      return
      end



      subroutine twostream_ref_trans
     &       (omega0, g, tauc, mu0, ref, trans, trans_dir,
     &        ref_diff, trans_diff)

      real*8 ref, trans, trans_dir, ref_diff, trans_diff
      real*8 mu1,l1,lm1,k1, omega1
      real*8 fw1, fz
      real f0, g, tauc, omega0, mu0
    
      data epsilon / 1.0e-15 /

      pi = 4.0 * atan(1.0)
      f0 = 1.0
      sq3 = sqrt(3.0)
      mu1 = sqrt(1.0/3.0)

      if (omega0 .lt. 1.0 .and. omega0 .gt. 0.0) then
c The notation of omega1 depends on the auther.
c Liou uses the follouing notation.
        omega1 = 3.0d0 * g * omega0
        taun = tauc

        k1 = (1.0d0/mu1) * sqrt((1.0-omega0)*(1.0d0-omega1*(mu1**2)))
        w1plus = fw1(mu1,omega0,omega1,k1)
        w1mins = fw1(-mu1,omega0,omega1,k1)
        zplus = fz(mu1,omega0,omega1,k1,mu0,f0)
        zmins = fz(-mu1,omega0,omega1,k1,mu0,f0)
        aplus = w1mins+w1plus*exp(-k1*taun)
        amins = w1mins-w1plus*exp(-k1*taun)
        cplus = -(zmins+zplus*exp(-taun/mu0))
        cmins = -(zmins-zplus*exp(-taun/mu0))
        l1 = 0.5d0*((cplus/aplus)+(cmins/amins))
        lm1 = 0.5d0*((cplus/aplus)-(cmins/amins))*exp(-k1*taun)

c for reflectivity, set tau = 0.0
        tau = 0.0d0

        ref = 2.0d0*pi*mu1*(l1*w1plus*exp(-k1*tau)
     &      +lm1*w1mins*exp(k1*tau)+zplus*exp(-tau/mu0))

        ref = ref / (pi*mu0*f0) 

        if (ref .lt. epsilon) then
          ref = epsilon
        end if

c for transmissivity, set tau = tauc
        tau = tauc

c When tauc is very large, it cause the problem at exp(k1*tauc)
        if (k1 * tau .gt. 500.0d0) then
          tau = 500.0d0 / k1
        end if

        trans = -2.0d0*pi*mu1*(l1*w1mins*exp(-k1*tau)
     &        +lm1*w1plus*exp(k1*tau)+zmins*exp(-tau/mu0))
     &        -pi*mu0*f0*exp(-tau/mu0)

        trans = -trans / (pi*mu0*f0)

        trans_dir = exp(-tauc/mu0)

        if (trans .lt. epsilon) then
          trans_dir = epsilon
          trans     = epsilon
        end if

c for diffuse illumination
        gamma1 = sq3 * (2.0d0 - omega0 * (1.0d0 + g)) / 2.0d0
        gamma2 = sq3 * omega0 * (1.0d0 - g) / 2.0d0
        gamma3 = (1.0d0 - sq3 * g * mu0) / 2.0d0
        gamma4 = 1.0d0 - gamma3

        ref_diff = gamma2 * (1.0d0 - exp(-2.0d0 * k1 * taun))
     &           / (k1 + gamma1 + (k1 - gamma1) 
     &           * exp(-2.0d0 * k1 * taun))

        trans_diff = 2.0d0 * k1 * exp(-k1 * taun)
     &           / (k1 + gamma1 + (k1 - gamma1) 
     &           * exp(-2.0d0 * k1 * taun))

      else
        gamma1 = sq3 * (2.0d0 - omega0 * (1.0d0 + g)) / 2.0d0
        gamma2 = sq3 * omega0 * (1.0d0 - g) / 2.0d0
        gamma3 = (1.0d0 - sq3 * g * mu0) / 2.0d0
        gamma4 = 1.0d0 - gamma3

        ref = gamma1 * tauc + (gamma3 - gamma1 * mu0) 
     &      * (1.0d0 - exp(-tauc/mu0))
        ref = ref / (1.0d0 + gamma1 * tauc)
        trans = 1.0d0 - ref

        ref_dif = gamma1 * tauc / (1.0d0 + gamma1 * tauc)
        trans_dif = 1.0d0 - ref_dif
      end if
        
      return
      end



      function fw1(x,omega0,omega1,k1)
      
      real*8 x, k1, omega1, fw1
      real   omega0

      data epsilon / 1.0e-15 /

      if (abs(1.0d0+x*k1) .lt. epsilon) then
        fw1 = (1.0d0/epsilon)*(omega0-omega1*(1.0d0-omega0)
     &       *(x/k1))
      else
        fw1 = (1.0d0/(1.0d0+x*k1))*(omega0-omega1*(1.0d0-omega0)
     &       *(x/k1))
      end if
      
      return
      end
     

      function fz(x,omega0,omega1,k1,mu0,f0)

      real*8 x, k1, omega1, fz
      real   omega0, f0, mu0

      data epsilon / 1.0e-15 /

      if (abs(1.0d0-(k1**2)*(mu0**2)) .lt. epsilon) then
        fz = (mu0*f0/4.0d0)*(((x-mu0)*(omega0-omega1*(1.0d0-omega0)
     &      *x*mu0))/((x**2)*epsilon))
      else
        fz = (mu0*f0/4.0)*(((x-mu0)*(omega0-omega1*(1.0d0-omega0)
     &      *x*mu0))/((x**2)*(1.0d0-(k1**2)*(mu0**2))))
      end if
      return
      end
