MODULE ENTROPY_LW
use FUINPUT ,only :  nv1x,nvx,mccx
implicit none
!integer ,parameter :: nlevX =400 , nlayX = nlevX-1 

TYPE LW_ENTROPY_TYPE 
sequence
 logical swtlwf
 integer nlev    ! # of Actual Levels
 integer nlay    ! # of Actual LAYERS
! real hk        ! Fractional contribution of sub-band
! integer icp
! integer icc
! 
 real Tskin
 real temp(0:nv1x)
 real  fup(0:nv1x)
 real  fdn(0:nv1x)
 real  tau(0:nvx)
 real eeband
!output 
 real   LL(nvx+1) ! Lw Entropy Exchange in Atmosphere (single)
 real  spa(nvx+1) ! Lw Entropy Production to SPACE (single)
 !output 
 real   SS(nvx+1) ! SW Entropy Exchange in Atmosphere (single)
 real  SSS(nvx+1) ! Sw Entropy Production From SUN (single)
 
 real sfc_dn, sfc_up
 

END TYPE LW_ENTROPY_TYPE

TYPE (LW_ENTROPY_TYPE) lwe

 CONTAINS
!===================================================================================================
subroutine shortwave_entropy_model
real , parameter :: Tsun = 5860.0 !K
real ,dimension(lwe%nlay+1)               :: tbar,swnet

 tbar(1:lwe%nlay) = 0.50* (lwe%temp(2:lwe%nlev)+lwe%temp(1:lwe%nlay))
 tbar(lwe%nlay+1) = lwe%Tskin

 swnet(1:lwe%nlay+1 ) = (lwe%fdn(1:lwe%nlay+1)-lwe%fup(1:lwe%nlay+1) ) - &
                        (lwe%fdn(2:lwe%nlay+2)-lwe%fup(2:lwe%nlay+2) )
			 
 lwe%SSS(1:lwe%nlay+1) = swnet(1:lwe%nlay+1) * (1.0/Tsun ) *1000.
 lwe%SS(1:lwe%nlay+1)  = swnet(1:lwe%nlay+1) * (1.0/tbar(1:lwe%nlay+1))*1000.
end subroutine shortwave_entropy_model
!====================================================================================================

subroutine longwave_entropy_model

!SK 
 USE FUINPUT
! SK

real   ,parameter :: uu0= 0.577 !!uu0 =0.577 0.5 0.6

integer k,iu,id 
real dneta 
!real ,dimension(lwe%nlay+2)            :: fnt ,fntS
real ,dimension(0:lwe%nlay+1,0:lwe%nlay+1) :: tm 
real ,dimension(0:lwe%nlay+1)          ::  aia !,trs
real ,dimension(lwe%nlay+1)            ::   neta ! DfntS,

real ,dimension(lwe%nlay+1,lwe%nlay+2)     ::  Mfup,Mfdn

real ,dimension(lwe%nlay+1)               :: tbar,lwnet

real ,dimension(0:lwe%nlay+1,0:lwe%nlay+1):: TempM

! SK adding variables required for entropy computation using analytical solution

      integer nq,n,m,i,j,i1, i_seiji
      parameter ( nq = 2 )
      real ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t
      real lamdan,gamman,caddn,cminn,caddn0,cminn0,aa,bb,expn
!      real g1g2n,fkbn,ffu,ffd,u0q,f0,eex,asbs,xx,bf,bs,ugts1
      real g1g2n,fkbn,u0q,f0,eex,asbs,xx,bf,bs,ugts1
      real t0,q1,q2,x,y,z,y1,yy
      real fg(ndfsx), fh(ndfsx), fj(ndfsx), fk(ndfsx)
      real alfa(ndfsx+1), beta(ndfsx)
      real fiu(mdfsx,nq), fid(mdfsx,nq),ub(ndfsx,nq)
      real fiu_source(mdfsx,nq), fid_source(mdfsx,nq)
      real fx(ndfsx,nq), fy(ndfsx), fz1(ndfsx,nq), fz2(ndfsx,nq)
      real aia2(0:lwe%nlay+1,2)

      real fuq1(ndfsx), fuq2(ndfsx)
      real ug(nq), wg(nq), ugwg(nq)
      
      real timt0
      
      real term1, term2, term3, term4, term5, term6, t_lay(ndfsx)
      real pt_slope(ndfsx)
      real ffu(mdfsx), ffd(mdfsx)
      real fiu_space(mdfsx,nq),ffu_space(mdfsx)
      real e_exch, sum_e_exch(mdfsx), sum_temp
      
!      real sfc_dn, sfc_up
      
      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx), &
     &                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), &
     &                      t(ndfsx), u0q(ndfsx), f0(ndfsx)
      common / gtscoe / lamdan(ndfsx), gamman(ndfsx), caddn(ndfsx), &
     &                    cminn(ndfsx), caddn0(ndfsx), cminn0(ndfsx), &
     &                 aa(ndfsx), bb(ndfsx), expn(ndfsx), g1g2n(ndfsx), &
     &                    fkbn(ndfsx)
      
      common /planci/ bf(nv1x), bs

      data ug / 0.2113248, 0.7886752 /
      data wg / 0.5, 0.5 /
      data ugwg / 0.105662, 0.394338 /
            
!SK
eex=lwe%eeband
 tbar(1:lwe%nlay) = 0.50* (lwe%temp(2:lwe%nlev)+lwe%temp(1:lwe%nlay))
 tbar(lwe%nlay+1) = lwe%Tskin


 lwnet(1:lwe%nlay+1 ) = (lwe%fdn(1:lwe%nlay+1)-lwe%fup(1:lwe%nlay+1) ) - &
                        (lwe%fdn(2:lwe%nlay+2)-lwe%fup(2:lwe%nlay+2) ) 


 lwe%LL(1:lwe%nlay+1)  = lwnet(1:lwe%nlay+1) * (  1.0/tbar(1:lwe%nlay+1) ) *1000


goto 1234

! Layer (0) is in Space 100% Transmission
! Layer nlay+1 is a pseudo layer of the SURFACE 
!Transmission_Matrix : do k=0,lwe%nlay+1 
! do iu = 0,k            ; tm(k,iu) = exp ( -1.0* sum( lwe%tau(iu:k))/uu0 )  ; enddo ! Inclusive of to and from layers
! do id = k+1,lwe%nlay+1 ; tm(k,id) = exp ( -1.0* sum( lwe%tau(k:id))/uu0 )  ; enddo   
! aia(k) = (1.0 -tm(k,k) )  ! Absorption in layer
!enddo Transmission_Matrix

! aia(lwe%nlay+1) = 1.0  ! Earth Pseudo-Layer absorbs all.
 
! SK 2014 2 27
! do iu = 1, lwe%nlay
!   do  j = 1, nq
!     aia2(iu,j) = 1.0 - exp(-lwe%tau(iu)/ug(j) )
!     print*, aia2(iu,j), lwe%tau(iu), iu, j, aia(iu)
!   enddo
! enddo
! aia2(lwe%nlay+1,1:2) = eex
!SK 2014 2 27 
 
 
! trs(0:lwe%nlay+1)    =  tm(0,0:lwe%nlay+1) ! Transmission from bottom of layer to space.
  
! fnt(1:lwe%nlev+1)    =  lwe%fup(1:lwe%nlev+1) - lwe%fdn(1:lwe%nlev+1) ! Compute Net from Up minus Down Profiles 
 
! fntS(1:lwe%nlev+1)   =  fnt(1:lwe%nlev+1) * trs(0:lwe%nlay+1) ! Net Flux to Space 

! DfntS(1:lwe%nlay+1)  = fntS(2:lwe%nlev+1) - fntS(1:lwe%nlev) ! Divergence of Net flux to Space

Mfup =0
Mfdn =0
Flux_Source_Matrix : do k=1,lwe%nlay+1 
 do iu=1,lwe%nlay+1

      if ( k >= iu)  Mfdn(k,iu)=   (lwe%fdn(iu+1) - lwe%fdn(iu)*tm(iu,iu) )* tm(k-1,iu+1)    
      if ( k <= iu)  Mfup(k,iu)= - (lwe%fup(iu) - lwe%fup(iu+1)*tm(iu,iu) )* tm(k+1,iu-1) !Transmission of layeres between K and IU
! Emitted Flux from Layer IU ( Total - transmitted component) That is transmitted throught to Layer K    
    
 TempM(k,iu) = 1.0/tbar(k) - 1.0/tbar(iu)  ! This really needs to be one once not for each spectral call
 enddo   

enddo Flux_Source_Matrix


!----- Internal Entropy Exchange ------------------------------------------------------------------------------------------------------

Entropy_INTERNAL : do k= 1,lwe%nlay+1   ! ( nlay+1) is Earth Layer surface

 neta(k) =0 

  do iu = 1,lwe%nlay+1 
  if ( iu .ne. k ) then      

!dneta =   Mfdn(k,iu)*aia(k) * TempM(k,iu)    & !1) 
!        + Mfup(iu,k)*aia(iu)* TempM(iu,k)  & !2)  
!        - Mfdn(iu,k)*aia(iu)* TempM(iu,k)  & !3)
!	- Mfup(k,iu)*aia(k) * TempM(k,iu)    !4)

! 1) Downward Emitted Flux from Layer IU transmitted Through to K  and Absorbed by layer K 
! 2) Upward Emitted Flux from Layer K transmitted Through to IU  and Absorbed by layer IU
! 3) Downward Emitted Flux from Layer K transmitted Through to Iu  and Absorbed by layer IU
! 4) Upward Emitted Flux from Layer IU transmitted Through to K  and Absorbed by layer K
!!! Transmitted Between Layers ::Mfdn(TO, From)  
  
    if (k .gt. iu) then
! k is lower layer    
       dneta = Mfdn(k,iu)*aia(k) * TempM(k,iu)    & !1) 
    &    + Mfup(iu,k)*aia(iu)* TempM(iu,k)   !2)         
          
    elseif (k .lt. iu) then
! k is upper layer    
      dneta = Mfdn(iu,k)*aia(iu)* TempM(iu,k)  & !3)
    &	+ Mfup(k,iu)*aia(k) * TempM(k,iu)    !4)      
    endif
    


    neta(k) = neta(k) + dneta 
    
   endif
  enddo 

enddo Entropy_INTERNAL

! SK this is Fred's output
! lwe%LL(1:lwe%nlay+1)  = neta(1:lwe%nlay+1) * 1000.  ! LW Entropy Production (mWm-1K-1)
 

1234 Continue

i_seiji = 1
if (i_seiji .eq. 1) then 
 !     ib = 1
      eex = lwe%eeband
      
      fid(1:mdfsx,1:nq) = 0.0
      fid_source(1:mdfsx,1:nq) = 0.0
      fiu(1:mdfsx,1:nq) = 0.0
      fiu_source(1:mdfsx,1:nq) = 0.0
!      lwe%LL(1:lwe%nlay+1) = 0.0
      lwe%spa(1:lwe%nlay+1) = 0.0
            
      ugts1 = 0.5
      
      n = ndfs
      m = mdfs
      asbs = bs * eex
      
      
!      print'(40f7.1)',t(1:fi%nv+1)
      call adjust2 
     
      
      t0 = 0.0
!print*, aa(1), aa(2), bb(1), bb(2), x, y, z, gamman(1), gamman(2), ugts1,  expn(1), g1g2n(1)
      do i = 1, n
        q1 = alog ( bf(i+1) / bf(i) )

! Paul Stackhouse: July 5, 98
!          q2 = 1.0 / ( t(i) - t0 )
           timt0 = t(i) - t0 
           if (timt0 .lt. 1.0e-12) timt0 = 1.0e-12
           q2 = 1.0 / timt0
! Paul Stackhouse: July 5, 98

         f0(i) = 2.0 * ( 1.0 - w(i) ) * bf(i)
	 
        if ( abs(q1) .le. 1.0e-10 ) then
          u0q(i) = - 1.0e+10 / q2
        else
          u0q(i) = - 1.0 / ( q1 * q2 )
        endif
       
       t0 = t(i)
       beta(i) = - 1.0 / u0q(i)
      enddo

!      call qccgts ( ib, asbs, eex )
!print*, aa(1), aa(2), bb(1), bb(2), x, y, z, gamman(1), gamman(2), ugts1,  expn(1), g1g2n(1)  
! aa bb are computed in gccgts but they are already computed in the flux computation and passed by the common block
    
      IF (fi%ierror .NE. 0) RETURN 
      do i = 1, n
       x = ( 1.0 - w(i) ) * w(i) / fkbn(i) / ugts1
       y1 = w1(i) / 3.0
       y = g1g2n(i)
       z = -y1 * beta(i)
       fuq1(i) = x * ( y - z ) + 1.0 - w(i)
       fuq2(i) = x * ( y + z ) + 1.0 - w(i)
      enddo

      do i = 1, n + 1
       alfa(i) = 6.2832 * bf(i)
      enddo

      do i = 1, n
       x = lamdan(i) * ugts1
       y = gamman(i) * ( 1.0 + x )
       z = 1.0 - x
       fg(i) = aa(i) * ( z + z )
       fh(i) = bb(i) * ( y + y )
       fj(i) = aa(i) * ( y + y )
       fk(i) = bb(i) * ( z + z )
      enddo
             
!print*, aa(1), aa(2), bb(1), bb(2), x, y, z, gamman(1), gamman(2), ugts1,  expn(1), g1g2n(1) 
     
      
!      do j = 1, nq
!        fid(1,j) = 0.0
!	fid_source(i,j) = 0.0
!        fiu(1,j) = 0.0
!	fiu_source(i,j) = 0.0	
!      enddo

      do j = 1, nq
       t0 = 0.0
       do i = 2, mdfs
        i1 = i - 1
 
! 2-18-2014 SK for upward entropy 	
	t_lay(i1) = t(i1) - t0
	 if ( t_lay(i1) < 1.0E-12 )  t_lay(i1) = 1.0E-12 !FGR 20141103
	 if ( t_lay(i1) > 20.0 )     t_lay(i1) = 20.0    !FGR 20141103
	
	fx(i1,j) = exp ( - ( t_lay(i1) ) / ug(j) )
! 2-18-2014 SK
        fy(i1) = expn(i1)
        xx = lamdan(i1) * ug(j)
!        IF (xx == 1.0) xx = .99999
	if ( ABS(xx-1.0) < 0.000001) xx = .99999 !FGR 20141103
	
	
        fz1(i1,j) = ( 1.0 - fx(i1,j) * fy(i1) ) / ( xx + 1.0 )
        fz2(i1,j) = ( fx(i1,j) - fy(i1) ) / ( xx - 1.0 )

        ub(i1,j) = ug(j) * beta(i1)

        IF (ABS(ub(i1,j) + 1.0) < 0.001 ) ub(i1,j) = -1.001  !FGR 20141103
        IF (ABS(ub(i1,j) - 1.0) < 0.001 ) ub(i1,j) =  1.001  !FGR 20141103


! 2-16-2014 SK
! entropy computation assuming temperature changes linearly with tau
! downward entropy flux
        pt_slope(i1) = (pt(i) - pt(i1)) / t_lay(i1)
	if ( ABS(pt_slope(i1)) < 0.01) pt_slope(i1) =0.01  !FGR 20141103
	
	
        term1 = 1.0/pt(i) + (pt_slope(i1)/pt(i)**2.0) * ug(j) &
     &          / ( xx + 1.0)
        term2 = 1.0/pt(i) - (pt_slope(i1)/pt(i)**2.0) * ug(j) &
     &          / ( xx - 1.0)
        term3 = 1.0/pt(i) + (pt_slope(i1)/pt(i)**2.0) * ug(j)  &
     &          / (ub(i1,j) + 1.0)
        term4 = -fj(i1) * ( t_lay(i1) ) * (pt_slope(i1)/pt(i)**2.0)  & 
     &          / ( xx + 1.0)
        term5 =  fk(i1) * ( t_lay(i1) )     &
     &          * exp(-lamdan(i1) * ( t_lay(i1) ))   &
     &          * (pt_slope(i1)/pt(i)**2.0) / ( xx - 1.0) 
        term6 = -fuq2(i1) * alfa(i) * ( t_lay(i1) )   &
     &          * (pt_slope(i1)/pt(i)**2.0) / ( ub(i1,j) + 1.0 )

        fid_source(i,j) = fj(i1) * fz1(i1,j) * term1 +   &
     &                fk(i1) * fz2(i1,j) * term2 +   &
     &                fuq2(i1) / ( ub(i1,j) + 1.0 ) *   &
     &            ( alfa(i) - alfa(i1) * fx(i1,j) ) * term3   &
     &                + term4 + term5 + term6
     
        fid(i,j) = fid(i1,j) * fx(i1,j) +  fid_source(i,j)

!      print*,i,j,term1,term2,term3,term4,term5,term6
!     print*, fid(i,j), ug(j), lamdan(i1), fj(i1), fk(i1), fuq2(i1)
! 2-16-2014 SK
     
        t0 = t(i1)
        enddo
       enddo

      yy = 0.0
      do j = 1, nq
       yy = yy + ugwg(j) * fid(mdfs,j) 
      enddo

      xx = yy * ( 1.0 - eex ) * 2.0 + 6.2831854 * eex * bs 
      do j = 1, nq
! 2-14-2014 SK 
! entropy computation      
!       fiu(mdfs,j) = xx
       fiu(mdfs,j) = xx /pts
       fiu_source(mdfs,j)= fiu(mdfs,j)
       fiu_space(mdfs,j) = fiu(mdfs,j) * exp(-t(mdfs-1)/ug(j))
! 2-14-2014 SK        
      enddo
! 6-24-98 (11)	
!	fiur(mdfs) = xx
! 6-24-98 (11)
      do j = 1, nq
       do i = mdfs - 1, 1, -1

 	  xx = lamdan(i) * ug(j)

 	  if ( ABS(xx-1.0) < 0.000001) xx = .99999 !FGR 20141103
	     
! 2-16-2014 SK
!          if (i == mdfs-1) print*, t(i+1), t(i), t_lay(1), pt(i+1), 
!     1                          pt(i), pt(1), pts
! entropy computation assuming temperature changes linearly with tau
! upward entropy flux i+1 is lower boundary i is upper boundary
        term1 = 1.0/pt(i) + (pt_slope(i)/pt(i)**2.0) * ug(j) &
     &          / ( xx - 1.0)
        term2 = 1.0/pt(i) - (pt_slope(i)/pt(i)**2.0) * ug(j) &
     &          / ( xx + 1.0)
        term3 = 1.0/pt(i) + (pt_slope(i)/pt(i)**2.0) * ug(j) &
     &          / (ub(i,j) - 1.0)
        term4 = -fg(i) * t_lay(i) &
     &          * exp(-t_lay(i) / ug(j)) &
     &          * (pt_slope(i)/pt(i)**2.0) / ( xx - 1.0)  
        term5 =  fh(i) * t_lay(i) &
     &          * exp(-(lamdan(i) + 1/ug(j)) * t_lay(i)) &
     &          * (pt_slope(i)/pt(i)**2.0) / ( xx + 1.0) 
        term6 = -fuq1(i) * alfa(i+1) * t_lay(i) &
     &          * exp(-t_lay(i) / ug(j)) &	
     &          * (pt_slope(i)/pt(i)**2.0) / ( ub(i,j) - 1.0 )
     
        fiu_source(i,j) = fg(i) * fz2(i,j) * term1 +      &
     &                fh(i) * fz1(i,j) * term2 +     &
     &                fuq1(i) / ( ub(i,j) - 1.0 ) *  &
     &            ( alfa(i+1) * fx(i,j) - alfa(i) ) * term3   &
     &                + term4 + term5 + term6


!      if ( ub(i,j) == 1.01 )  print*,i,j,term1,term2,term3,term4,term5,term6,ub(i,j)-1.0
        fiu(i,j) = fiu(i+1,j) * fx(i,j) + fiu_source(i,j)
     
        if (i .gt. 1) then
          fiu_space(i,j) = fiu_source(i,j) * exp(-t(i-1)/ug(j))
	elseif (i .eq. 1) then
	  fiu_space(i,j) = fiu_source(i,j)
	endif
	
!	if (i .eq. 1) print*, 'upward', fiu(i,j), fiu_space(i,j)
	
! 2-16-2014 SK
        enddo
       enddo

      do i = 1, mdfs
       ffu(i) = 0.0
       ffd(i) = 0.0
       ffu_space(i) = 0.0
       lwe%spa(i) = 0.0
      enddo
      sum_temp = 0.0
      
      do i = 1, mdfs
       do j = 1, nq
        ffu(i) = ffu(i) + ugwg(j) * fiu(i,j) 
        ffd(i) = ffd(i) + ugwg(j) * fid(i,j)
	
	ffu_space(i) = ffu_space(i) + ugwg(j) * fiu_space(i,j)
       enddo

	sum_temp = sum_temp + ffu_space(i)
!	if (sum_temp .gt. 1.0e10) print*, sum_temp, lwe%spa(i), fiu_space(i,j)
      enddo

      lwe%spa(1:mdfs) = ffu_space(1:mdfs) * 1000.0 * (4.0 / 3.0)
      
      lwe%sfc_dn = ffd(mdfs) * 1000.0 * (4.0 / 3.0)
      lwe%sfc_up = ffu(mdfs) * 1000.0 * (4.0 / 3.0)      
      

endif
!print'(40f12.1)',t(1:mdfs)
!print'(40f12.5)',t_lay(1:mdfs)
!print'(40f12.1)',lwe%spa(1:mdfs)
where ( lwe%spa(1:mdfs)  > 1000 .OR. lwe%spa(1:mdfs) < 0.0 ) lwe%spa(1:mdfs)=0.0 ! FGR 20141103
return


end subroutine longwave_entropy_model
END MODULE ENTROPY_LW
!=======================================================================================================================================
