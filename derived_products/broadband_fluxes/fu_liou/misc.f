!============================================================================
	real function airmass(u0,Re,h)
	airmass = Re/h*( sqrt(u0*u0 + 2.0*(h/Re) + (h/Re)* (h/Re))- u0 ) 
	end function airmass
!============================================================================

      subroutine thicks 
c **********************************************
c dz is the thickness of a layer in units of km.
c **********************************************
      USE FUINPUT
     
      implicit none
      integer i
	real pv1,pv2

        do  i = 1, nv
	pv1=pt(i) *(1+.61* ph(i) )
	pv2=pt(i+1) *(1+.61* ph(i+1) )
       dz(i) = 0.0146337 * ( pv1 + pv2 ) 
     1         * alog( pp(i+1) / pp(i) )
       enddo


	fi%zz(nv+1) = fi%hsfc
	do i = nv,1,-1
	fi%zz(i) = fi%zz(i+1)+ dz(i)*1000. !! fi%dz in [Meters]
	enddo

      return
      end
c=======================================================================
      subroutine gascon ( ib )
c  ********************************************************************
c  tgm(nv) are the optical depthes due to water vapor continuum absorp-
c  tion in nv layers for a given band ib. We include continuum absorp-
c  tion in the 280 to 1250 cm**-1 region. vv(11)-vv(17) are the central
c  wavenumbers of each band in this region. 
c *********************************************************************
      USE FUINPUT
      implicit none
      integer i,ib
      real tgm,vv(mbx)

      common /con/ tgm(nvx)

      data vv / 10*0.0, 3*0.0, 1175.0, 1040.0, 890.0, 735.0, 
     1                605.0, 470.0, 340.0, 0.0 /

      if ( ib .gt. 13 .and. ib .lt. 21 ) then
         call qopcon ( vv(ib), tgm )
      else
         do i = 1, nv
          tgm(i) = 0.0
          end do
      endif

      return
      end

      subroutine gases ( ib, ig, hk )
c  *****************************************************************
c  tg(nv) are the optical depthes due to nongray gaseous absorption, 
c  in nv layers for a given band ib and cumulative probability ig. 
c  *****************************************************************
      USE FUINPUT
      implicit none
 
      integer i,ib,ig
!	real pp,pt,ph,po,umco2,umch4,umn2o
      real hk,hk1, fk1o3
      real hk2,c2h2o,hk3,c3h2o,hk4,c4h2o,hk5,c5h2o,hk6,c6h2o
      real hk7,c7h2o,hk8,c8h2o,hk9,c9h2o,hk10,c10h2o,c10ch4,c10n2o
      real hk11,c11h2o,c11ch4,c11n2o,hk12,c12o3,c12h2o,hk13,c13h2o
      real hk14,c14hca,c14hcb,hk15,c15hca,c15hcb,hk16,c16h2o
      real hk17,c17h2o,hk18,c18h2o,tg,fk
      real fkg(nv1x), fkga(nv1x), fkgb(nv1x), pq(nv1x)
      real tg1(nvx), tg2(nvx), tg3(nvx)
      real sol_spect,fk1h2o

      common /band1/ hk1(10), fk1o3(10),sol_spect(0:7),fk1h2o(10)
      common /band2/ hk2(8), c2h2o(3,11,8)
      common /band3/ hk3(12), c3h2o(3,11,12)
      common /band4/ hk4(7), c4h2o(3,11,7)
      common /band5/ hk5(12), c5h2o(3,11,12)
      common /band6/ hk6(5), c6h2o(3,11,5)
      
      common /band7/ hk7(2), c7h2o(3,19,2)
      common /band8/ hk8(3), c8h2o(3,19,3)
      common /band9/ hk9(4), c9h2o(3,19,4)
      common /band10/ hk10(4),c10h2o(3,19,4),c10ch4(3,19),c10n2o(3,19)
      common /band11/ hk11(3),c11h2o(3,19,3),c11ch4(3,19),c11n2o(3,19)
      common /band12/ hk12(5), c12o3(3,19,5), c12h2o(3,19)
      common /band13/ hk13(2), c13h2o(3,19,2)
      common /band14/ hk14(10), c14hca(3,19,10), c14hcb(3,19,10)
      common /band15/ hk15(12), c15hca(3,19,12), c15hcb(3,19,12)
      common /band16/ hk16(7), c16h2o(3,19,7)
      common /band17/ hk17(7), c17h2o(3,19,7)
      common /band18/ hk18(8), c18h2o(3,19,8)
      common /gas/ tg(nvx)


      goto (1, 2,2,2,2, 3,4,5,6,
     &      7,8,9,10,11,12,13,14,15,16,17,18, 19,19)ib 
      fi%ierror = 30 ! print*,
c  ---------------------------------------------------------------------
c  In this band ( 50000 - 14500 cm**-1 ), we have considered the nongray
c  gaseous absorption of O3.    619.618 is the solar energy contained in
c  the band in units of Wm**-2.
c  ---------------------------------------------------------------------
 1    fk = fk1o3(ig)
!!!	fk=1.0E-09 !ZERO OZONE
        call qopo3s ( fk, tg )
	
!	if (lpar) call qoph2o_chou ( fk1h2o(ig)  , tg )
!      hk = 619.618 * hk1(ig)
       hk = sol_spect(1) * hk1(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 14500 - 7700 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.  484.295 is the solar energy contained in
c  the band in units of Wm**-2.
c  ---------------------------------------------------------------------
 2    continue
       call qks ( c2h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
!      hk = 484.295 * hk2(ig)
       hk = sol_spect(2) * hk2(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 7700 - 5250 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O. 149.845 is the solar energy contained in
c  the band in units of Wm**-2.
c  ---------------------------------------------------------------------
 3    call qks ( c3h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
!      hk = 149.845 * hk3(ig)
       hk = sol_spect(3) * hk3(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 5250 - 4000 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O. 48.7302 is the solar energy contained in
c  the band in units of Wm**-2.
c  ---------------------------------------------------------------------
 4    call qks ( c4h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
!      hk = 48.7302 * hk4(ig)
       hk = sol_spect(4) * hk4(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 4000 - 2850 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O. 31.6576 is the solar energy contained in
c  the band in units of Wm**-2.
c  ---------------------------------------------------------------------
 5    call qks ( c5h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
	if ( lband6a ) then
	 hk = ( sol_spect(5) + sol_spect(7) ) * hk5(ig)
	else
!	 hk = 31.6576 * hk5(ig)
	 hk = sol_spect(5) * hk5(ig)
	endif
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 2850 - 2500 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O. 5.79927 is the solar energy contained in
c  the band in units of Wm**-2.
c  ---------------------------------------------------------------------
 6    call qks ( c6h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
!      hk = 5.79927 * hk6(ig)
       hk = sol_spect(6) * hk6(ig)
      goto 20
      
!-THERMAL LONGWAVE .....................................................
c  ---------------------------------------------------------------------
c  In this band ( 2200 - 1900 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.
c  ---------------------------------------------------------------------
 7    call qki ( c7h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
      hk = hk7(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 1900 - 1700 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.
c  ---------------------------------------------------------------------
 8    call qki ( c8h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
      hk = hk8(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 1700 - 1400 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.
c  ---------------------------------------------------------------------
 9    call qki ( c9h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
      hk = hk9(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 1400 - 1250 cm**-1 ), we have considered the 
c  overlapping absorption of H2O, CH4, and N2O by approach one of 
c  Fu(1991).
c  ---------------------------------------------------------------------
 10   call qki ( c10h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg1 )
      call qki ( c10ch4, fkg )
      call qopch4 ( fkg, tg2 )
      call qki ( c10n2o, fkg )
      call qopn2o ( fkg, tg3 )
      do i = 1, nv
       tg(i) = tg1(i) + tg2(i)/1.6*umch4 + tg3(i)/0.28*umn2o
       end do
      hk = hk10(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 1250 - 1100 cm**-1 ), we have considered the 
c  overlapping absorption of H2O, CH4, and N2O by approach one of 
c  Fu(1991).
c  ---------------------------------------------------------------------
 11   continue
 
! 	print*,'band 14',idkfr
	
	if ( idkfr == 0) then

      call qki ( c11h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg1 )

      call qki ( c11ch4, fkg )
      call qopch4 ( fkg, tg2 )
      call qki ( c11n2o, fkg )
      call qopn2o ( fkg, tg3 )
      do i = 1, nv
       tg(i) = tg1(i) + tg2(i)/1.6*umch4 + tg3(i)/0.28*umn2o
       end do

       hk = hk11(ig)
	
	elseif( idkfr == 1) then
	call ck_dkfr_win_all(ib,ig, hk,tg)
	elseif( idkfr == 2) then
	call ck_dkfr_win_hyb(ib,ig, hk,tg)
	endif

      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 1100 - 980 cm**-1 ), we have considered the overlapping
c  absorption of H2O and O3 by approach one of Fu(1991).
c  ---------------------------------------------------------------------
 12   continue
	if ( idkfr == 0) then
      call qkio3 ( c12o3(1,1,ig), fkg )
      call qopo3i ( fkg, tg1 )
      call qki ( c12h2o, fkg )
      call qoph2o ( fkg, tg2 )	

       do i = 1, nv
       tg(i) = tg1(i) + tg2(i)
       end do

      hk = hk12(ig)

	elseif( idkfr == 1) then
	call ck_dkfr_win_all(ib,ig, hk,tg)
	elseif( idkfr == 2) then
	call ck_dkfr_win_hyb(ib,ig, hk,tg)
	endif

      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 980 - 800 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.
c  ---------------------------------------------------------------------
 13   continue
	if ( idkfr == 0) then
      call qki ( c13h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
      hk = hk13(ig)
	elseif( idkfr == 1) then
        call ck_dkfr_win_all(ib,ig, hk,tg)
	elseif( idkfr == 2) then
	call ck_dkfr_win_hyb(ib,ig, hk,tg)
	endif
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 800 - 670 cm**-1), we have considered the overlapping
c  absorption of H2O and CO2 by approach two of Fu(1991).
c  ---------------------------------------------------------------------
 14   do i = 1, nv1
       if ( pp(i) .ge. 63.1 ) then
         pq(i) = ph(i)
       else
         pq(i) = 0.0
       endif
       end do
      call qki ( c14hca(1,1,ig), fkga )
      call qki ( c14hcb(1,1,ig), fkgb )
      do i = 1, nv1
       fkg(i) = fkga(i)/330.0*umco2 + pq(i) * fkgb(i)
       end do
      call qophc ( fkg, tg)
      hk = hk14(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 670 - 540 cm**-1), we have considered the overlapping
c  absorption of H2O and CO2 by approach two of Fu(1991).
c  ---------------------------------------------------------------------
 15   do i = 1, nv1
       if ( pp(i) .ge. 63.1 ) then
         pq(i) = ph(i)
       else
         pq(i) = 0.0
       endif
       end do
      call qki ( c15hca(1,1,ig), fkga )
      call qki ( c15hcb(1,1,ig), fkgb )
      do i = 1, nv1
       fkg(i) = fkga(i)/330.0*umco2 + pq(i) * fkgb(i)
       end do
      call qophc ( fkg, tg)
      hk = hk15(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 540 - 400 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.
c  ---------------------------------------------------------------------
 16   call qki ( c16h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
      hk = hk16(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 400 - 280 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.
c  ---------------------------------------------------------------------
 17   call qki ( c17h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
      hk = hk17(ig)
      goto 20

c  ---------------------------------------------------------------------
c  In this band ( 280 - 000 cm**-1 ), we have considered the nongray
c  gaseous absorption of H2O.
c  ---------------------------------------------------------------------
 18   call qki ( c18h2o(1,1,ig), fkg )
      call qoph2o ( fkg, tg )
      hk = hk18(ig)
	 goto 20

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!Solar / Thermal
c  ---------------------------------------------------------------------
c  In band 19( 2850 - 2500 cm**-1 ),THERMAL we have considered the nongray
c  gaseous absorption of H2O. 
c  ---------------------------------------------------------------------

c  ---------------------------------------------------------------------
c  In band 20 ( 2500 - 2200 cm**-1 ), THERMAL we have considered the nongray
c  gaseous absorption of H2O/CO2/N2.
c  ---------------------------------------------------------------------
 19    continue

      call kratzsolir(ib,ig,hk,tg) ! band 19&20 

 20   continue
      return
      end
c 338kfix
	subroutine qks ( coefks, fkg )
c *********************************************************************
c fkg(nv1) are the gaseous absorption coefficients in units of (cm-atm)
c **-1 for a given cumulative probability in nv1 layers. coefks(3,11)
c are the coefficients to calculate the absorption coefficient at the
c temperature t for the 11 pressures by
c         ln k = a + b * ( t - 245 ) + c * ( t - 245 ) ** 2
c and the absorption coefficient at conditions other than those eleven
c pressures is interpolated linearly with pressure (Fu, 1991).
c *********************************************************************
      USE FUINPUT
	
 
	dimension coefks(3,11)
	dimension fkg(nv1x)
	dimension stanp(11)
	dimension tk(nv1x)
      	data stanp / 10.0, 15.8, 25.1, 39.8, 63.1, 100.0,
     1	             158.0, 251.0, 398.0, 631.0, 1000.0 /
	do i = 1, nv1
           if ( pt(i) .le. 180.0 ) then
              tk(i) = 180.0
           elseif ( pt(i) .ge. 320.0 ) then
              tk(i) = 320.0
           else
              tk(i) = pt(i)
           endif
	enddo
	i1 = 1
	do 5 i = 1, nv1
	   if ( pp(i) .lt. stanp(1) ) then
  	     x1 = exp ( coefks(1,1) + coefks(2,1) * ( tk(i) - 245.0 )
     1       + coefks(3,1) * ( tk(i) - 245.0 ) ** 2 )
	     fkg(i) = x1 * pp(i) / stanp(1)
	   elseif ( pp(i) .ge. stanp(11) ) then
	     y1 = ( tk(i) - 245.0 ) * ( tk(i) - 245.0 )
	     x1 = exp ( coefks(1,10) + coefks(2,10) * ( tk(i) - 245.0 )
     1	     + coefks(3,10) * y1 )
    	     x2 = exp ( coefks(1,11) + coefks(2,11) * ( tk(i) - 245.0 )
     1	     + coefks(3,11) * y1 )
	     fkg(i) = x1 + ( x2 - x1 ) / ( stanp(11) - stanp(10) )
     1	     * ( pp(i) - stanp(10) )
	   else
30	     continue
	     if ( pp(i) .ge. stanp(i1) ) goto 20
	     y1 = ( tk(i) - 245.0 ) * ( tk(i) - 245.0 )
	     x1 = exp ( coefks(1,i1-1) + coefks(2,i1-1) * (tk(i)-245.0)
     1	     + coefks(3,i1-1) * y1 )
	     x2 = exp ( coefks(1,i1) + coefks(2,i1) * ( tk(i) - 245.0 )
     1	     + coefks(3,i1) * y1 )
	     fkg(i) = x1 + ( x2 - x1 ) / ( stanp(i1) - stanp(i1-1) )
     1	     * ( pp(i) - stanp(i1-1) )
	     goto 5
20           i1 = i1 + 1
	     goto 30
	   endif
5  	continue
	return
	end

	subroutine qki ( coefki, fkg )
c *********************************************************************
c fkg(nv1) are the gaseous absorption coefficients in units of (cm-atm)
c **-1 for a given cumulative probability in nv1 layers. coefki(3,19)
c are the coefficients to calculate the absorption coefficient at the
c temperature t for the 19 pressures by
c         ln k = a + b * ( t - 245 ) + c * ( t - 245 ) ** 2
c and the absorption coefficient at  conditions  other  than  those 19
c pressures is interpolated linearly with pressure (Fu, 1991).
c *********************************************************************
      USE FUINPUT

 
	dimension coefki(3,19)
	dimension fkg(nv1x)
	dimension stanp(19)
	dimension tk(nv1x)
	data stanp / 0.251, 0.398, 0.631, 1.000, 1.58, 2.51, 
     1	             3.98, 6.31, 10.0, 15.8, 25.1, 39.8, 63.1,
     1	             100.0, 158.0, 251.0, 398.0, 631.0, 1000.0 /
	do i = 1, nv1
           if ( pt(i) .le. 180.0 ) then
              tk(i) = 180.0
           elseif ( pt(i) .ge. 320.0 ) then
              tk(i) = 320.0
           else
              tk(i) = pt(i)
           endif
	enddo
	i1 = 1
	do 5 i = 1, nv1
	   if ( pp(i) .lt. stanp(1) ) then
  	     x1 = exp ( coefki(1,1) + coefki(2,1) * ( tk(i) - 245.0 )
     1       + coefki(3,1) * ( tk(i) - 245.0 ) ** 2 )
	     fkg(i) = x1 * pp(i) / stanp(1)
	   elseif ( pp(i) .ge. stanp(19) ) then
	     y1 = ( tk(i) - 245.0 ) * ( tk(i) - 245.0 )
	     x1 = exp ( coefki(1,18) + coefki(2,18) * ( tk(i) - 245.0 )
     1	     + coefki(3,18) * y1 )
    	     x2 = exp ( coefki(1,19) + coefki(2,19) * ( tk(i) - 245.0 )
     1	     + coefki(3,19) * y1 )
	     fkg(i) = x1 + ( x2 - x1 ) / ( stanp(19) - stanp(18) )
     1	     * ( pp(i) - stanp(18) )
	   else
30	     continue
	     if ( pp(i) .ge. stanp(i1) ) goto 20
	     y1 = ( tk(i) - 245.0 ) * ( tk(i) - 245.0 )
	     x1 = exp ( coefki(1,i1-1) + coefki(2,i1-1) * (tk(i)-245.0)
     1	     + coefki(3,i1-1) * y1 )
	     x2 = exp ( coefki(1,i1) + coefki(2,i1) * ( tk(i) - 245.0 )
     1	     + coefki(3,i1) * y1 )
	     fkg(i) = x1 + ( x2 - x1 ) / ( stanp(i1) - stanp(i1-1) )
     1	     * ( pp(i) - stanp(i1-1) )
	     goto 5
20           i1 = i1 + 1
	     goto 30
	   endif
5  	continue
	return
	end

	subroutine qkio3 ( coefki, fkg )
c *********************************************************************
c fkg(nv1) are the gaseous absorption coefficients in units of (cm-atm)
c **-1 for a given cumulative probability in nv1 layers. coefki(3,19)
c are the coefficients to calculate the absorption coefficient at the
c temperature t for the 19 pressures by
c         ln k = a + b * ( t - 250 ) + c * ( t - 250 ) ** 2
c and the absorption coefficient at  conditions  other  than  those 19
c pressures is interpolated linearly with pressure (Fu, 1991).
c *********************************************************************
      USE FUINPUT

 
	dimension coefki(3,19)
	dimension fkg(nv1x)
	dimension stanp(19)
	dimension tk(nv1x)
	data stanp / 0.251, 0.398, 0.631, 1.000, 1.58, 2.51, 
     1	             3.98, 6.31, 10.0, 15.8, 25.1, 39.8, 63.1,
     1	             100.0, 158.0, 251.0, 398.0, 631.0, 1000.0 /
	do i = 1, nv1
           if ( pt(i) .le. 180.0 ) then
              tk(i) = 180.0
           elseif ( pt(i) .ge. 320.0 ) then
              tk(i) = 320.0
           else
              tk(i) = pt(i)
           endif
	enddo
	i1 = 1
	do 5 i = 1, nv1
	   if ( pp(i) .lt. stanp(1) ) then
  	     x1 = exp ( coefki(1,1) + coefki(2,1) * ( tk(i) - 250.0 )
     1       + coefki(3,1) * ( tk(i) - 250.0 ) ** 2 )
	     fkg(i) = x1 * pp(i) / stanp(1)
	   elseif ( pp(i) .ge. stanp(19) ) then
	     y1 = ( tk(i) - 250.0 ) * ( tk(i) - 250.0 )
	     x1 = exp ( coefki(1,18) + coefki(2,18) * ( tk(i) - 250.0 )
     1	     + coefki(3,18) * y1 )
    	     x2 = exp ( coefki(1,19) + coefki(2,19) * ( tk(i) - 250.0 )
     1	     + coefki(3,19) * y1 )
	     fkg(i) = x1 + ( x2 - x1 ) / ( stanp(19) - stanp(18) )
     1	     * ( pp(i) - stanp(18) )
	   else
30	     continue
	     if ( pp(i) .ge. stanp(i1) ) goto 20
	     y1 = ( tk(i) - 250.0 ) * ( tk(i) - 250.0 )
	     x1 = exp ( coefki(1,i1-1) + coefki(2,i1-1) * (tk(i)-250.0)
     1	     + coefki(3,i1-1) * y1 )
	     x2 = exp ( coefki(1,i1) + coefki(2,i1) * ( tk(i) - 250.0 )
     1	     + coefki(3,i1) * y1 )
	     fkg(i) = x1 + ( x2 - x1 ) / ( stanp(i1) - stanp(i1-1) )
     1	     * ( pp(i) - stanp(i1-1) )
	     goto 5
20           i1 = i1 + 1
	     goto 30
	   endif
5  	continue
	return
	end

c 338kfix

      subroutine qopo3s ( fk, tg )
      USE FUINPUT
      implicit none

      integer i
      real fk,tg(nvx),fq

 

      fq = 238.08 * fk
      do 10 i = 1, nv
       tg(i) = ( po(i) + po(i+1) ) * ( pp(i+1) - pp(i) ) * fq
10     continue

c      do 20 i = 1, nv
c         tg(i) = tg(i) * 476.16 * fk
c20      continue
c 476.16 = 2.24e4 / M * 10.0 / 9.8, where M = 48 for O3.

      return
      end

      subroutine qoph2o ( fkg, tg )
      USE FUINPUT
      implicit none

      integer i
      real fkg(nv1x),tg(nvx)
 

      do 10 i = 1, nv
       tg(i) = ( fkg(i) * ph(i) + fkg(i+1) * ph(i+1) )
     1         * ( pp(i+1) - pp(i) ) * 634.9205
10     continue

c      do 20 i = 1, nv
c         tg(i) = tg(i) * 1269.841
c20      continue
c 1269.841 = 2.24e4 / M * 10.0 / 9.8, where M = 18 for H2O.

      return
      end

!========================================================================

      subroutine qopch4 ( fkg, tg )
      USE FUINPUT
      implicit none

      integer i
      real fkg(nv1x),tg(nvx)
 

      do 10 i = 1, nv
       tg(i) = ( fkg(i)+fkg(i+1) ) * ( pp(i+1)-pp(i) ) * 6.3119e-4
10     continue

c      do 20 i = 1, nv
c         tg(i) = tg(i) * 1.26238e-3
c20      continue
c 1.26238e-3 = 2.24e4 / M * 10.0 / 9.8 * 1.6e-6 * M / 28.97, where 
c M = 16 for CH4.

      return
      end
!========================================================================
      subroutine qopn2o ( fkg, tg )
      USE FUINPUT
      implicit none

      integer i
      real fkg(nv1x),tg(nvx)
 

      do 10 i = 1, nv
       tg(i) = ( fkg(i)+fkg(i+1) ) * ( pp(i+1)-pp(i) ) * 1.10459e-4
10     continue

c      do 20 i = 1, nv
c         tg(i) = tg(i) * 2.20918e-4
c20      continue
c 2.20918e-4 = 2.24e4 / M * 10.0 / 9.8 * 0.28e-6 * M / 28.97, where
c M = 44 for N2O.

      return
      end
!========================================================================
      subroutine qopo3i ( fkg, tg )
      USE FUINPUT
      implicit none

      integer i
      real fkg(nv1x),tg(nvx)
 

      do 10 i = 1, nv
       tg(i) = ( fkg(i) * po(i) + fkg(i+1) * po(i+1) )
     1         * ( pp(i+1) - pp(i) ) * 238.08
10     continue

c      do 20 i = 1, nv
c         tg(i) = tg(i) * 476.16
c20      continue

      return
      end
!========================================================================
      subroutine qophc ( fkg, tg )
      USE FUINPUT
      implicit none

      integer i
      real fkg(nv1x),tg(nvx)
 

      do 10 i = 1, nv
       tg(i) = ( fkg(i) + fkg(i+1) ) * ( pp(i+1) - pp(i) ) * 0.5
10     continue
c  ------------------------
c  See page 86 of Fu (1991).
c  ------------------------

      return
      end
!========================================================================
      subroutine qopcon ( vv, tg )
      USE FUINPUT
      implicit none

      integer i
      real ff(nv1x),pe(nv1x),tg(nvx),x,y,z,r,s,vv,w

 

      x = 4.18
      y = 5577.8
      z = 0.00787
      r = 0.002
      s = ( x + y * exp ( - z * vv ) ) / 1013.25

      do 3 i = 1, nv1
       pe(i) = pp(i) * ph(i) / ( 0.622 + 0.378 * ph(i) )
       w = exp ( 1800.0 / pt(i) - 6.08108 )
       ff(i) = s * ( pe(i) + r * pp(i) ) * w
3      continue

      do 5 i = 1, nv
       tg(i) = ( ff(i) * ph(i) + ff(i+1) * ph(i+1) )*
     1         ( pp(i+1) - pp(i) ) * 0.5098835
5      continue

c      do 7 i = 1, nv
c       tg(i) = tg(i) * 10.0 / 9.80616
c7      continue

      return
      end
!========================================================================
      subroutine planck ( ib )
c  ******************************************************************
c  bf and bs are the blackbody intensity function integrated over the
c  band ib at the nv1 levels and at the surface, respectively.    The
c  units of bf and bs are W/m**2/Sr. nd*10 is the band width from ve.
c  ******************************************************************
      USE FUINPUT
      implicit none

      integer i,ib,nv11,ibr,j,nd(mbirx+2)
      real bf,bs,ve(mbirx+2),bt(nv1x)
      real bts,v1,v2,w,fq1,fq2,x

 
      common /planci/ bf(nv1x), bs

      data ve / 2200.0, 1900.0, 1700.0, 1400.0, 1250.0, 1100.0,
     1            980.0, 800.0, 670.0, 540.0, 400.0, 280.001,
     &		  2850.0,2500.0 /
      data nd / 30, 20, 30, 15, 15, 12,
     1            18, 13, 13, 14, 12, 28,
     1		  35, 30 /

      nv11 = nv1 + 1
      ibr = ib - mbs
      bts = 0.0

      do 10 i = 1, nv1
       bt(i) = 0.0
10     continue
      v1 = ve(ibr)
      do 20 j = 1, nd(ibr)
       v2 = v1 - 10.0
       w = ( v1 + v2 ) * 0.5
       fq1 = 1.19107e-8 * w * w * w
       fq2 = 1.43884 * w
       do 30 i = 1, nv11
        if ( i .eq. nv11 ) then
          x = fq1 / ( exp ( fq2 / pts ) - 1.0 )
          bts = bts + x
        else
          x = fq1 / ( exp ( fq2 / pt(i) ) - 1.0 )
          bt(i) = bt(i) + x
        endif
30      continue
       v1 = v2
20     continue
      do 40 i = 1, nv1
       bf(i) = bt(i) * 10.0
40     continue
      bs = bts * 10.0

      return
      end
!========================================================================
      subroutine coeff1 
c  *********************************************************************
c  coefficient calculations for four first-order differential equations.
c  *********************************************************************
      USE FUINPUT
      implicit none
      integer i,j,ib
      real a,u,p0d,p1d,p2d,p3d,p11d,p22d,p33d,x,w0w,w1w,w2w,w3w,fw
      real w,w1,w2,w3,t0,t1,u0q,f0,b,c(4,5),q1,q2,q3,fq
      common /dis/ a(4)
      common /point/ u(4)
      common /legen/ p0d(4), p1d(4), p2d(4), p3d(4)
      common /legen1/ p11d(4,4), p22d(4,4), p33d(4,4)
      common /coedfi/ ib, w, w1, w2, w3, t0, t1, u0q, f0
      common /coedf1/ b(4,3)

      x = 0.5 * w
      w0w = x
      w1w = x * w1
      w2w = x * w2
      w3w = x * w3
      if ( ib .le. mbs ) then
        fw = u0q * u0q
        q1 = - w1w * u0q
        q2 = w2w * ( 1.5 * fw - 0.5 )
        q3 = - w3w * ( 2.5 * fw - 1.5 ) * u0q
      endif
      fq = 0.5 * w0w

      do 10 i = 3, 4
       do 20 j = 1, 4
        c(i,j) = fq + w1w * p11d(i,j) +
     1           w2w * p22d(i,j) + w3w * p33d(i,j) 
        if ( i .eq. j ) then 
          c(i,j) = ( c(i,j) - 1.0 ) / u(i)
        else
          c(i,j) = c(i,j) / u(i)
        endif
20     continue
10    continue

      do 30 i = 1, 4
       if ( ib .le. mbs ) then
         c(i,5) = w0w + q1 * p1d(i) +
     1            q2 * p2d(i) + q3 * p3d(i) 
       else
         c(i,5) = 1.0
       endif
       c(i,5) = c(i,5) / u(i)
30     continue

      b(1,1) = c(4,4) - c(4,1)
      b(1,2) = c(4,4) + c(4,1)
      b(2,1) = c(4,3) - c(4,2)
      b(2,2) = c(4,3) + c(4,2)
      b(3,1) = c(3,4) - c(3,1)
      b(3,2) = c(3,4) + c(3,1)
      b(4,1) = c(3,3) - c(3,2)
      b(4,2) = c(3,3) + c(3,2)
      b(1,3) = c(4,5) - c(1,5)
      b(2,3) = c(3,5) - c(2,5)
      b(3,3) = c(3,5) + c(2,5)
      b(4,3) = c(4,5) + c(1,5)
      return
      end
!========================================================================
      subroutine coeff2 
c  *****************************************************************
c  coefficient calculations for second order differential equations.
c  *****************************************************************
      implicit none
      integer ib
      real w,w1,w2,w3,t0,t1,u0q,f0,b,a,d,fw1,fw2,fw3,fw4
      common /coedfi/ ib, w, w1, w2, w3, t0, t1, u0q, f0
      common /coedf1/ b(4,3)
      common /coedf2/ a(2,2,2), d(4)

      fw1 = b(1,1) * b(1,2)
      fw2 = b(2,1) * b(3,2)
      fw3 = b(3,1) * b(2,2)
      fw4 = b(4,1) * b(4,2)
      a(2,2,1) = fw1 + fw2
      a(2,1,1) = b(1,1) * b(2,2) + b(2,1) * b(4,2)
      a(1,2,1) = b(3,1) * b(1,2) + b(4,1) * b(3,2)
      a(1,1,1) = fw3 + fw4
      a(2,2,2) = fw1 + fw3
      a(2,1,2) = b(1,2) * b(2,1) + b(2,2) * b(4,1)
      a(1,2,2) = b(3,2) * b(1,1) + b(4,2) * b(3,1)
      a(1,1,2) = fw2 + fw4
      d(1) = b(3,2) * b(4,3) + b(4,2) * b(3,3) + b(2,3) / u0q
      d(2) = b(1,2) * b(4,3) + b(2,2) * b(3,3) + b(1,3) / u0q
      d(3) = b(3,1) * b(1,3) + b(4,1) * b(2,3) + b(3,3) / u0q
      d(4) = b(1,1) * b(1,3) + b(2,1) * b(2,3) + b(4,3) / u0q

      return
      end
!========================================================================
      subroutine coeff4 
c  *****************************************************************
c  coefficient calculations for fourth-order differential equations.
c  *****************************************************************
      implicit none
      integer ib
      real w,w1,w2,w3,t0,t1,u0q,f0,a,d,b1,c1,z,x
      common /coedfi/ ib, w, w1, w2, w3, t0, t1, u0q, f0
      common /coedf2/ a(2,2,2), d(4)
      common /coedf4/ b1, c1, z(4)
      x = u0q * u0q
      b1 = a(2,2,1) + a(1,1,1)
      c1 = a(2,1,1) * a(1,2,1) - a(1,1,1) * a(2,2,1)
      z(1) = a(2,1,1) * d(3) + d(4) / x - a(1,1,1) * d(4)
      z(2) = a(1,2,1) * d(4) - a(2,2,1) *d(3) + d(3) / x
      z(3) = a(2,1,2) * d(1) + d(2) / x - a(1,1,2) * d(2)
      z(4) = a(1,2,2) * d(2) - a(2,2,2) * d(1) + d(1) / x
      return
      end
!========================================================================
      subroutine coeffl  
c  ********************************
c  fk1 and fk2 are the eigenvalues.
c  ********************************
      USE FUINPUT
      implicit none
      integer ib,i
      real w,w1,w2,w3,t0,t1,u0q,f0,a,d,b,b1,c1,z,x,aa,zz,a1,z1,fk1,fk2
      real dt,fw,a2,b2,fw1,fw2,y,zx,fq0,fq1
      
      common /coedfi/ ib, w, w1, w2, w3, t0, t1, u0q, f0
      common /coedf1/ b(4,3)
      common /coedf2/ a(2,2,2), d(4)
      common /coedf4/ b1, c1, z(4)
      common /coedfl/ aa(4,4,2), zz(4,2), a1(4,4), z1(4), fk1, fk2

      dt = t1 - t0
      x = sqrt ( b1 * b1 + 4.0 * c1 )
      fk1 = sqrt ( ( b1 + x ) * 0.5 )
      fk2 = sqrt ( ( b1 - x ) * 0.5 )
      fw = u0q * u0q
      x = 1.0 / ( fw * fw ) - b1 / fw - c1

c---------- 4/2/97 (4)
      if (abs (x) .lt. 1.0E-16) THEN
        if ( x .lt. 0.0) THEN
          x = -1.0E-6
        else
          x = 1.0E-6
        end if
      end if
c---------- 4/2/97 (4)

      fw = 0.5 * f0 / x
      z(1) = fw * z(1) 
      z(2) = fw * z(2) 
      z(3) = fw * z(3) 
      z(4) = fw * z(4) 
      z1(1) = 0.5 * ( z(1) + z(3) )
      z1(2) = 0.5 * ( z(2) + z(4) )
      z1(3) = 0.5 * ( z(2) - z(4) )
      z1(4) = 0.5 * ( z(1) - z(3) )
      a2 = ( fk1 * fk1 - a(2,2,1) ) / a(2,1,1)
      b2 = ( fk2 * fk2 - a(2,2,1) ) / a(2,1,1)
      x = b(1,1) * b(4,1) - b(3,1) * b(2,1)
      fw1 = fk1 / x
      fw2 = fk2 / x
      y = fw2 * ( b2 * b(2,1) - b(4,1) ) 
      zx = fw1 * ( a2 * b(2,1) - b(4,1) )
      a1(1,1) = 0.5 * ( 1 - y )
      a1(1,2) = 0.5 * ( 1 - zx )
      a1(1,3) = 0.5 * ( 1 + zx )
      a1(1,4) = 0.5 * ( 1 + y )
      y = fw2 * ( b(3,1) - b2 * b(1,1) ) 
      zx = fw1 * ( b(3,1) - a2 * b(1,1) ) 
      a1(2,1) = 0.5 * ( b2 - y )
      a1(2,2) = 0.5 * ( a2 - zx )
      a1(2,3) = 0.5 * ( a2 + zx )
      a1(2,4) = 0.5 * ( b2 + y )
      a1(3,1) = a1(2,4)
      a1(3,2) = a1(2,3)
      a1(3,3) = a1(2,2)
      a1(3,4) = a1(2,1)
      a1(4,1) = a1(1,4)
      a1(4,2) = a1(1,3)
      a1(4,3) = a1(1,2)
      a1(4,4) = a1(1,1)

      if ( ib .le. mbs ) then
        fq0 = exp ( - t0 / u0q )
        fq1 = exp ( - t1 / u0q )
      else
        fq0 = 1.0
        fq1 = exp ( - dt / u0q )
      endif
      x = exp ( - fk1 * dt )
      y = exp ( - fk2 * dt )

      do 40 i = 1, 4
       zz(i,1) = z1(i) * fq0
       zz(i,2) = z1(i) * fq1
       aa(i,1,1) = a1(i,1)
       aa(i,2,1) = a1(i,2)
       aa(i,3,1) = a1(i,3) * x
       aa(i,4,1) = a1(i,4) * y
       aa(i,3,2) = a1(i,3)
       aa(i,4,2) = a1(i,4)
       aa(i,1,2) = a1(i,1) * y
       aa(i,2,2) = a1(i,2) * x
40     continue
      return
      end
!========================================================================
      subroutine coefft 
c  ********************************************************************
c  See the paper by Liou, Fu and Ackerman (1988) for the formulation of
c  the delta-four-stream approximation in a homogeneous layer.
c  ********************************************************************
      call coeff1 
      call coeff2 
      call coeff4 
      call coeffl 
      return
      end
!========================================================================
      subroutine coefft0 
c  *****************************************************************
c  In the limits of no scattering ( Fu, 1991 ), fk1 = 1.0 / u(3) and
c  fk2 = 1.0 / u(4).
c  *****************************************************************
      USE FUINPUT
      implicit none

      integer i,ib,jj,j,k
      real u,w,w1,w2,w3,t0,t1,u0q,f0,aa,zz,a1,z1,fk1,fk2,y,fw,dt,x

      common /point/ u(4)
      common /coedfi/ ib, w, w1, w2, w3, t0, t1, u0q, f0
      common /coedfl/ aa(4,4,2), zz(4,2), a1(4,4), z1(4), fk1, fk2
      fk1 = 4.7320545
      fk2 = 1.2679491
      y = exp ( - ( t1 - t0 ) / u0q )
      fw = 0.5 * f0

      do 10 i = 1, 4
       if ( ib .le. mbs ) then
         z1(i) = 0.0
         zz(i,1) = 0.0
         zz(i,2) = 0.0
       else
         jj = 5 - i
         z1(i) = fw / ( 1.0 + u(jj) / u0q )
         zz(i,1) = z1(i) 
         zz(i,2) = z1(i) * y
       endif

c  ***************************************************************
c  ******************  10/24/96 **********************************
c  ***************************************************************
c  multiple references to 10 continue replaced by loops 11 and 12.
c  ***************************************************************
c  ***************************************************************
c  ***************************************************************
       do 11 j = 1, 4
        a1(i,j) = 0.0
        do 12 k = 1, 2
         aa(i,j,k) = 0.0
12       continue
11      continue
10     continue

      do 20 i = 1, 4
       j = 5 - i
       a1(i,j) = 1.0
20     continue

      dt = t1 - t0
      x = exp ( - fk1 * dt )
      y = exp ( - fk2 * dt )
      aa(1,4,1) = y
      aa(2,3,1) = x
      aa(3,2,1) = 1.0
      aa(4,1,1) = 1.0
      aa(1,4,2) = 1.0
      aa(2,3,2) = 1.0
      aa(3,2,2) = x
      aa(4,1,2) = y

      return
      end
!========================================================================
      subroutine qccfe ( ib, asbs, eex )  
c  ********************************************************************
c  In the solar band  asbs is the surface albedo, while in the infrared
c  band asbs is  blackbody intensity emitted at the surface temperature
c  times surface emissivity.  In this subroutine, the delta-four-stream
c  is applied to nonhomogeneous atmospheres. See comments in subroutine
c  'qcfel' for array AB(13,4*n).
c  ********************************************************************
      USE FUINPUT
      implicit none
 
      integer i,ib,j,k,n,n4,ibn,i8,kf,i1,i2,i3,j1,j2,j3,m1,m2,m18,m28
      real a,u,w1,w2,w3,w,t,u0q,f0,wn,w1n,w2n,w3n,t0n,t1n
      real u0n,f0n,aa,zz,a1,z1,fk1t,fk2t,fk1,fk2,a4,z4,g4,ab,bx,xx
      real fu(4,4), wu(4),v1,v2,v3,asbs,eex,fw1,fw2

      common /dis/ a(4)
      common /point/ u(4)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0q(ndfsx), f0(ndfsx)
      common /coedfi/ ibn, wn, w1n, w2n, w3n, t0n, t1n, u0n, f0n
      common /coedfl/ aa(4,4,2), zz(4,2), a1(4,4), z1(4),
     1                  fk1t, fk2t
      common /qccfeo/ fk1(ndfsx), fk2(ndfsx), a4(4,4,ndfsx), 
     1                      z4(4,ndfsx), g4(4,ndfsx)
      common /qcfelc/ ab(13,ndfs4x), bx(ndfs4x), xx(ndfs4x)

      n = ndfs
      n4 = ndfs4

      do i = 1, n4
       do j = 1, 13
        ab(j,i) = 0.0
        end do
       end do

      ibn = ib
      wn = w(1)
      w1n = w1(1)
      w2n = w2(1)
      w3n = w3(1)
      t0n = 0.0
      t1n = t(1)
      u0n = u0q(1)
      f0n = f0(1)
      if ( wn .ge. 0.999999 ) then
        wn = 0.999999
      endif
      if ( wn .le. 1.0e-4 ) then
        call coefft0 
        fk1(1) = fk1t
        fk2(1) = fk2t
      else
        call coefft 
        fk1(1) = fk1t
        fk2(1) = fk2t
      endif

      do 10 i = 1, 4
       z4(i,1) = z1(i)
       do 11 j = 1, 4
        a4(i,j,1) = a1(i,j)
11      continue
10     continue

      do 20 i = 1, 2
       bx(i) = - zz(i+2,1)
       i8 = i + 8
       do 21 j = 1, 4
        ab(i8-j,j) = aa(i+2,j,1)
21      continue
20     continue

      do 30 i = 1, 4
       wu(i) = zz(i,2)
       do 31 j = 1, 4
        fu(i,j) = aa(i,j,2)
31      continue
30     continue

      do 40 k = 2, n
       wn = w(k)
       w1n = w1(k)
       w2n = w2(k)
       w3n = w3(k)
       t0n = t(k-1)
       t1n = t(k)
       u0n = u0q(k)
       f0n = f0(k)
       if ( wn .ge. 0.999999 ) then
         wn = 0.999999
       endif
       if ( wn .le. 1.0e-4 ) then
         call coefft0 
         fk1(k) = fk1t
         fk2(k) = fk2t
       else
         call coefft 
         fk1(k) = fk1t
         fk2(k) = fk2t
       endif

       do 50 i = 1, 4
        z4(i,k) = z1(i)
        do 51 j = 1, 4
         a4(i,j,k) = a1(i,j)
51       continue
50      continue

       kf = k + k + k + k
       i1 = kf - 5
       i2 = i1 + 3
       j1 = kf - 7
       j2 = j1 + 3
       i3 = 0
       do 60 i = i1, i2
        i3 = i3 + 1
        bx(i) = - wu(i3) + zz(i3,1)
        j3 = 0
        i8 = i + 8
        do 61 j = j1, j2
         j3 = j3 + 1
         ab(i8-j,j) = fu(i3,j3)
61      continue
        j3 = 0
        do 62 j = j2 + 1, j2 + 4
         j3 = j3 + 1
         ab(i8-j,j) = - aa(i3,j3,1)
62       continue
60      continue

       do 70 i = 1, 4
        wu(i) = zz(i,2)
        do 71 j = 1, 4
         fu(i,j) = aa(i,j,2)
71       continue
70      continue

40     continue

      if ( ib .le. mbs ) then
        v1 = 0.2113247 * asbs
        v2 = 0.7886753 * asbs
        v3 = asbs * u0q(1) * f0(1) * exp ( - t(n) / u0q(1) )
        m1 = n4 - 1
        m2 = n4
        m18 = m1 + 8
        m28 = m2 + 8
        fw1 = v1 * wu(3)
        fw2 = v2 * wu(4)
        bx(m1) = - ( wu(1) - fw1 - fw2 - v3 )
        bx(m2) = - ( wu(2) - fw1 - fw2 - v3 )
        do 80 j = 1, 4
         j1 = n4 - 4 + j
         fw1 = v1 * fu(3,j)
         fw2 = v2 * fu(4,j)
         ab(m18-j1,j1) = fu(1,j) - fw1 - fw2
         ab(m28-j1,j1) = fu(2,j) - fw1 - fw2
80       continue
      else
        v1 = 0.2113247 * ( 1.0 - eex )
        v2 = 0.7886753 * ( 1.0 - eex )
        v3 = asbs
        m1 = n4 - 1
        m2 = n4
        m18 = m1 + 8
        m28 = m2 + 8
        fw1 = v1 * wu(3)
        fw2 = v2 * wu(4)
        bx(m1) = - ( wu(1) - fw1 - fw2 - v3 )
        bx(m2) = - ( wu(2) - fw1 - fw2 - v3 )
        do 85 j = 1, 4
         j1 = n4 - 4 + j
         fw1 = v1 * fu(3,j)
         fw2 = v2 * fu(4,j)
         ab(m18-j1,j1) = fu(1,j) - fw1 - fw2
         ab(m28-j1,j1) = fu(2,j) - fw1 - fw2
85       continue
      endif

      call qcfel 
      do 90 k = 1, n
       j = k + k + k + k - 4
       do 91 i = 1, 4
        j = j + 1
        g4(i,k) = xx(j)
91      continue
90     continue
      return
      end
!========================================================================
      subroutine qcfel 
c  **********************************************************************
c  1. `qcfel' is the abbreviation of ` qiu constants for each layer'.
c  2. The inhomogeneous atmosphere is divided into n adjacent homogeneous
c     layers where the  single scattering properties are constant in each
c     layer and allowed to vary from one to another. Delta-four-stream is
c     employed for each homogeneous layer. The boundary conditions at the
c     top and bottom of the atmosphere,  together with  continuity condi-
c     tions  at  layer interfaces lead to a system of algebraic equations
c     from which 4*n unknown constants in the problom can be solved.
c  3. This subroutine is used for solving the 4*n unknowns of A *X = B by
c     considering the fact that the coefficient matrix is a sparse matrix
c     with the precise pattern in this special problom.
c  4. The method is not different in principle from the general scheme of
c     Gaussian elimination with backsubstitution, but carefully optimized
c     so as to minimize arithmetic operations.  Partial  pivoting is used
c     to quarantee  method's numerical stability,  which will  not change
c     the basic pattern of sparsity of the matrix.
c  5. Scaling special problems so as to make  its nonzero matrix elements
c     have comparable magnitudes, which will ameliorate the stability.
c  6. a, b and x present A, B and X in A*X=B, respectively. and n4=4*n.
c  7. AB(13,4*n) is the matrix A in band storage, in rows 3 to 13; rows 1
c     and 2 and other unset elements should be set to zero on entry.
c  8. The jth column of A is stored in the jth column of the array AB  as
c     follows:
c             AB(8+i-j,j) = A(i,j) for max(1,j-5) <= i <= min(4*n,j+5).
c     Reversedly, we have
c            A(ii+jj-8,jj) = AB(ii,jj).
c **********************************************************************
      USE FUINPUT
      implicit none
 
      integer n,n4,k,l,i,j,k44,m1,i0m1,i0,m18,i0f,n44,m,im1,n3,n2,n1
      integer m2,m3,m4,m1f,m28,m38,m48,ifq
      real ab,b,x,p,t,yy,xx

      common /qcfelc/ ab(13,ndfs4x), b(ndfs4x), x(ndfs4x)
      n = ndfs
      n4 = ndfs4

      do 200 k = 1, n - 1
       k44 = 4 * k - 4
       do 201 l= 1, 4
        m1 = k44 + l
        p = 0.0

        do 10 i = 8, 14 - l
         if ( abs ( ab(i,m1) ) .gt. abs ( p ) ) then
           p = ab(i,m1)
           i0 = i
         endif
10       continue
        i0m1 = i0 + m1
        m18 = m1 + 8

c       **********************************************************
c       ****************** 10/24/96 ******************************
c       **********************************************************
c       Replaced "if i0=8 goto 20" statement with if "i0<>8 then".
c       line "20 replaced by the endif.
c       **********************************************************
c       **********************************************************
c       **********************************************************
        if ( i0 .ne. 8 ) then
          do 15 j = m1, m1 + 8 - l
           i0f = i0m1 - j
           m1f = m18 - j
           t = ab(i0f,j)
           ab(i0f,j) = ab(m1f,j)
           ab(m1f,j) = t
15         continue

          i0f = i0m1 - 8
          t = b(i0f)
          b(i0f) = b(m1)
          b(m1) = t
        endif

        yy = ab(8,m1)
        ab(8,m1) = 1.0
        do 25 j = m1 + 1, m1 + 8 - l
         m1f = m18 - j
         ab(m1f,j) = ab(m1f,j) / yy
25       continue
        b(m1) = b(m1) / yy

        do 30 i = 9, 14 - l
         xx = ab(i,m1)
         ab(i,m1) = 0.0
         im1 = i + m1
         do 31 j = m1 + 1, m1 + 8 - l
          ifq = im1 - j
          m1f = m18 - j
          ab(ifq,j) = ab(ifq,j) - ab(m1f,j) * xx
31        continue
         ifq = im1 - 8
         b(ifq) = b(ifq) - b(m1) * xx
30       continue

201     continue
200    continue

      n44 = n4 - 4
      do 300 l = 1, 3
       m1 = n44 + l
       p = 0.0
       do 40 i = 8, 12 - l
        if ( abs ( ab(i,m1) ) .gt. abs ( p ) ) then
          p = ab(i,m1)
          i0 = i
        endif
40      continue
       i0m1 = i0 + m1
       m18 = m1 + 8

c      **********************************************************
c      ****************** 10/24/96 ******************************
c      **********************************************************
c      Replaced "if i0=8 goto 55" statement with if "i0<>8 then".
c      line "55 replaced by the endif.
c      **********************************************************
c      **********************************************************
c      **********************************************************
       if ( i0 .ne. 8 ) then
         do 50 j = m1, m1 + 4 - l
          i0f = i0m1 - j
          m1f = m18 - j
          t = ab(i0f,j)
          ab(i0f,j) = ab(m1f,j)
          ab(m1f,j) = t
50        continue
         i0f = i0m1 - 8
         t = b(i0f)
         b(i0f) = b(m1)
         b(m1) = t
       endif

       yy = ab(8,m1)
       ab(8,m1) = 1.0
       do 60 j = m1 + 1, m1 + 4 - l
        m1f = m18 - j
        ab(m1f,j) = ab(m1f,j) / yy
60      continue
       b(m1) = b(m1) / yy
       do 70 i = 9, 12 - l
        xx = ab(i,m1)
        ab(i,m1) = 0.0
        im1 = i + m1
        do 71 j = m1 + 1, m1 + 4 - l
         ifq = im1 - j
         m1f = m18 - j
         ab(ifq,j) = ab(ifq,j) - ab(m1f,j) * xx
71       continue
        ifq = im1 - 8
        b(ifq) = b(ifq) - b(m1) * xx
70      continue
300    continue

      yy = ab(8,n4)
      ab(8,n4) = 1.0
      b(n4) = b(n4) / yy
      n3 = n4 - 1
      n2 = n3 - 1
      n1 = n2 - 1
      x(n4) = b(n4)
      x(n3) = b(n3) - ab(7,n4) * x(n4)
      x(n2) = b(n2) - ab(7,n3) * x(n3) - ab(6,n4) * x(n4)
      x(n1) = b(n1) - ab(7,n2) * x(n2) - ab(6,n3) * x(n3) -
     1      ab(5,n4) * x(n4)
      do 80 k = 1, n - 1
       m4 = 4 * ( n - k )
       m3 = m4 - 1
       m2 = m3 - 1
       m1 = m2 - 1
       m48 = m4 + 8
       m38 = m3 + 8
       m28 = m2 + 8
       m18 = m1 + 8
       x(m4) = b(m4)
       do 81 m = m4 + 1, m4 + 4
        x(m4) = x(m4) - ab(m48-m,m) * x(m)
81      continue
       x(m3) = b(m3)
       do 82 m = m3 + 1, m3 + 5
        x(m3) = x(m3) - ab(m38-m,m) * x(m)
82      continue
       x(m2) = b(m2)
       do 83 m = m2 + 1, m2 + 6
        x(m2) = x(m2) - ab(m28-m,m) * x(m)
83      continue
       x(m1) = b(m1)
       do 84 m = m1 + 1, m1 + 7
        x(m1) = x(m1) - ab(m18-m,m) * x(m)
84      continue
80     continue
      return
      end
!========================================================================
      subroutine adjust4
c  *****************************************************************
c  In this subroutine, we incorporate a delta-function adjustment to
c  account for the  forward  diffraction  peak in the context of the 
c  four-stream or two stream approximations ( Liou, Fu and Ackerman,
c  1988 ).  The w1(n), w2(n), w3(n), w(n), and t(n) are the adjusted
c  parameters.
c  *****************************************************************
      USE FUINPUT
      implicit none
 
      integer i,n
      real ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t,u0a, f0a
      real dtt(ndfsx), dt(ndfsx),tt0,f,fw

      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx),
     1                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0a(ndfsx), f0a(ndfsx)

      n = ndfs
      tt0 = 0.0
      do i = 1, n
       f = ww4(i) / 9.0
       fw = 1.0 - f * ww(i) 
       w1(i) = ( ww1(i) - 3.0 * f ) / ( 1.0 - f )
       w2(i) = ( ww2(i) - 5.0 * f ) / ( 1.0 - f )
       w3(i) = ( ww3(i) - 7.0 * f ) / ( 1.0 - f )
       w(i) = ( 1.0 - f ) * ww(i) / fw
       dtt(i) = tt(i) - tt0
       tt0 = tt(i)
       dt(i) = dtt(i) * fw
       enddo

      t(1) = dt(1)
      do i = 2, n
       t(i) = dt(i) + t(i-1)
       enddo

      return
      end
!========================================================================
      subroutine qfts ( ib, asx, f0 )
c  *******************************************************************
c  The delta-four-stream approximation for nonhomogeneous atmospheres
c  in the solar wavelengths (Fu, 1991). The input parameters are ndfs,
c  mdfs, and ndfs4 through 'para.file',  ib, asx, u0, f0 for solar and
c  ib, bf, bs, ee for IR through arguments of  'qfts' and 'qfti', and
c  ww1(ndfs), ww2(ndfs), ww3(ndfs), ww4(ndfs), ww(ndfs), and tt(ndfs)
c  through common statement 'dfsin'.
c  *******************************************************************
      USE FUINPUT
      implicit none
 
      integer ib,n,m,i,k,jj,ii
      real a,u,ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t,u0a,f0a
      real fk1,fk2,a4,z4,g4,ffu,ffd,asx,f0,asbs
      real x(4), fiq(4),eex,fw1,fw2,fw3,y,y1,fw4
c---------- 10/28/96 (5)
      real ffdr, ffdf, ydr
      common /dirdiff/ ffdr(nv1x), ffdf(nv1x)
c---------- 10/28/96 (5)

      common /dis/ a(4)
      common /point/ u(4)
      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx),
     1                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0a(ndfsx), f0a(ndfsx)
      common /qccfeo/ fk1(ndfsx), fk2(ndfsx), a4(4,4,ndfsx), 
     1                      z4(4,ndfsx), g4(4,ndfsx)
      common /dfsout/ ffu(mdfsx), ffd(mdfsx)

      n = ndfs
      m = mdfs
      eex = 0.0
      asbs = asx
      call adjust4
      do 5 i = 1, n
       u0a(i) = u0
       f0a(i) = f0
5      continue

      call qccfe ( ib, asbs, eex ) 
      fw1 = 0.6638961
      fw2 = 2.4776962
      fw3 = u0 * 3.14159 * f0 
      do 10 i = 1, m
       if ( i .eq. 1 ) then
         x(1) = 1.0
         x(2) = 1.0
         x(3) = exp ( - fk1(1) * t(1) )
         x(4) = exp ( - fk2(1) * t(1) )
         k = 1
         y = 1.0
c---------- 10/28/96 (6a)
         ydr=1.0
c---------- 10/28/96 (6a)
       elseif ( i .eq. 2 ) then
         x(1) = exp ( - fk2(1) * t(1) )
         x(2) = exp ( - fk1(1) * t(1) )
         x(3) = 1.0
         x(4) = 1.0
         k = 1
         y = exp ( - t(1) / u0 )
c---------- 10/28/96 (6b)
         ydr = exp ( - tt(1) / u0 )
c---------- 10/28/96 (6b)
       else
         k = i - 1
         y1 = t(k) - t(k-1)
         x(1) = exp ( - fk2(k) * y1 )
         x(2) = exp ( - fk1(k) * y1 )
         x(3) = 1.0
         x(4) = 1.0
         y = exp ( - t(k) / u0 )
c---------- 10/28/96 (6c)
         ydr = exp ( - tt(k) / u0 )
c---------- 10/28/96 (6c)
       endif
       do 37 jj = 1, 4
        fiq(jj) = z4(jj,k) * y
37      continue

       do 40 ii = 1, 4
        fw4 = g4(ii,k) * x(ii)
        do 41 jj = 1, 4
         fiq(jj) = fiq(jj) + a4(jj,ii,k) * fw4
41       continue
40      continue

       ffu(i)= fw1 * fiq(2) + fw2 * fiq(1) 
       ffd(i)= fw1 * fiq(3) + fw2 * fiq(4) + fw3 * y
c---------- 10/28/96 (7)
       ffdr(i) = fw3 * ydr
       ffdf(i) = ffd(i) - ffdr(i)
c---------- 10/28/96 (7)
10     continue
      return
      end
!========================================================================
      subroutine qfti ( ib, eex )
c ******************************************************************
c  The exponential approximation for the Planck function in optical 
c  depth is used for the infrared (Fu, 1991). Since the direct solar 
c  radiation source has an exponential function form in terms of 
c  optical depth, the formulation of the delta-four-stream 
c  approximation for infrared wavelengths is the same as that for 
c  solar wavelengths. 
c ******************************************************************
      USE FUINPUT
      implicit none
 
      integer i,ib,n,k,ii,jj,m
      real a,u,ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t,u0q,f0,fk1
      real fk2,a4,z4,g4,ffu,ffd,bf,bs,asbs,q1,q2,t0,fw1,fw2,xy
      real x(4), fiq(4),fw3,eex,y1
	real timt0
      common /dis/ a(4)
      common /point/ u(4)
      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx),
     1                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0q(ndfsx), f0(ndfsx)
      common /qccfeo/ fk1(ndfsx), fk2(ndfsx), a4(4,4,ndfsx), 
     1                      z4(4,ndfsx), g4(4,ndfsx)
      common /dfsout/ ffu(mdfsx), ffd(mdfsx)
      common /planci/ bf(nv1x), bs

      n = ndfs
      m = mdfs
      asbs = bs * eex
      call adjust4 
      t0 = 0.0

      do 3 i = 1, n
       q1 = alog ( bf(i+1) / bf(i) )

c Paul Stackhouse: July 5, 98
c          q2 = 1.0 / ( t(i) - t0 )
           timt0 = t(i) - t0 
           if (timt0 .lt. 1.0e-12) timt0 = 1.0e-12
           q2 = 1.0 / timt0
c Paul Stackhouse: July 5, 98

       f0(i) = 2.0 * ( 1.0 - w(i) ) * bf(i)
       if ( abs(q1) .le. 1.0e-10 ) then
         u0q(i) = - 1.0e+10 / q2
       else
         u0q(i) = - 1.0 / ( q1 * q2 )
       endif

c---------- 4/2/97 (5)
      if (abs(u0q(i)) .gt. 4.25E+09) then
        if (u0q(i) .lt. 0.0) then
          u0q(i) = -4.25E+09
        else
          u0q(i) = 4.25E+09
        end if
      end if
c---------- 4/2/97 (5)

       t0 = t(i)
3       continue

      call qccfe ( ib, asbs, eex ) 
      fw1 = 0.6638958
      fw2 = 2.4776962
      do 10 i = 1, m
       if ( i .eq. 1 ) then
         x(1) = 1.0
         x(2) = 1.0
         x(3) = exp ( - fk1(1) * t(1) )
         x(4) = exp ( - fk2(1) * t(1) )
         k = 1
         xy = 1.0
       elseif ( i .eq. 2 ) then
         x(1) = exp ( - fk2(1) * t(1) )
         x(2) = exp ( - fk1(1) * t(1) )
         x(3) = 1.0
         x(4) = 1.0
         k = 1
         xy =  exp ( - t(1) / u0q(1) )
       else
         k = i - 1
         y1 = t(k) - t(k-1)
         x(1) = exp ( - fk2(k) * y1 )
         x(2) = exp ( - fk1(k) * y1 )
         x(3) = 1.0
         x(4) = 1.0
         xy =  exp ( - y1 / u0q(k) )
       endif
       do 37 jj = 1, 4
        fiq(jj) = z4(jj,k) * xy
37      continue
       do 40 ii = 1, 4
        fw3 = g4(ii,k) * x(ii)
        do 45 jj = 1, 4
         fiq(jj) = fiq(jj) + a4(jj,ii,k) * fw3
45       continue
40      continue
       ffu(i)= fw1 * fiq(2) + fw2 * fiq(1)
       ffd(i)= fw1 * fiq(3) + fw2 * fiq(4)
10     continue

      return
      end
!========================================================================
      subroutine cfgts0 ( gamma1, gamma2, gamma3, gamma4, ugts1 )
c  *********************************************************************
c  This subroutine is used to calculate the Coefficients For Generalized
c  Two-Stream scheme. We can make choices between Eddington, quadrature
c  and  hemispheric  mean  schemes through  logical variables 'edding',
c  'quadra', and 'hemisp'.  The Eddington and quadrature schemes are 
c  discussed in detail by Liou (1992).  The hemispheric mean scheme is 
c  derived by assuming that the phase function is equal to 1 + g in the 
c  forward scattering hemisphere and 1 - g  in the backward scattering 
c  hemisphere where g is the asymmetry factor. The hemispheric mean is
c  only used for infrared wavelengths (Toon et al. 1989).
c
c   11/4/95
c
c  *********************************************************************
      USE FUINPUT
      implicit none
 
      integer ib
      real w,w1,w2,w3,t0,t1,u0q,f0,x,y,gamma1,gamma2
      real gamma3,gamma4,z,ugts1

      common /coedfi / ib, w, w1, w2, w3, t0, t1, u0q, f0
      if ( edding ) then
        x = 0.25 * w1
        y = w * x
        gamma1 = 1.75 - w - y
        gamma2 = - 0.25 + w - y
        gamma3 = 0.0
        gamma4 = 0.0
        if ( ib .le. mbs ) then
          gamma3 = 0.5 - x * u0q
          gamma4 = 1.0 - gamma3
        endif
        ugts1 = 0.5
      endif

      if ( quadra ) then
        x = 0.866 * w
        y = 0.2887 * w1
        z = y * w
        gamma1 = 1.732 - x - z
        gamma2 = x - z
        gamma3 = 0.0
        gamma4 = 0.0
        if ( ib .le. mbs ) then
          gamma3 = 0.5 - y * u0q
          gamma4 = 1.0 - gamma3
        endif
        ugts1 = 0.57735
      endif

      if ( hemisp ) then
        x = w * w1 / 3.0
        gamma1 = 2.0 - w - x
        gamma2 = w - x
        gamma3 = 0.0
        gamma4 = 0.0
        ugts1 = 0.5
      endif

      if ( mquadr ) then
        y = 0.2767 * w
        x = y + y + y
        z = y * w1
        gamma1 = 1.66 - x - z
        gamma2 = x - z
        gamma3 = 0.0
        gamma4 = 0.0
        ugts1 = 0.6024
      endif

      return
      end
!========================================================================
      subroutine cfgts ( lamda, gamma, cadd0, cadd1, cmin0, cmin1,
     1                     g1g2, fkb )
c  *********************************************************
c  This subroutine is used to calculate the Coefficients For
c  Generalized Two-Stream scheme. 
c
c  11/4/95
c
c  ***********************************************************
      USE FUINPUT
      implicit none
 
      integer ib
      real lamda,w,w1,w2,w3,t0,t1,u0q,f0,gamma1,gamma2
      real gamma3,gamma4,ugts1,gamma,g1g2,alfa,beta,cadd0,cmin0
      real fw,cadd1,cmin1,fkb,x,z,fq

      common /coedfi/ ib, w, w1, w2, w3, t0, t1, u0q, f0

      call cfgts0 ( gamma1, gamma2, gamma3, gamma4, ugts1 )
      lamda = sqrt ( ( gamma1 + gamma2 ) * ( gamma1 - gamma2 ) )
      gamma = gamma2 / ( gamma1 + lamda )
      g1g2 = gamma1 + gamma2
      fq = 1.0 / u0q

      x = exp ( - fq * ( t1 - t0 ) )
      z = lamda * lamda - fq * fq

c---------- 4/2/97 (6)
      if (z .eq. 0.0) z = 0.001
c---------- 4/2/97 (6)

      fkb = z

      if ( ib .le. mbs ) then
        alfa = gamma3
        beta = gamma4
        fw = 3.1415927 * f0 * w * exp ( - fq * t0 )
        cadd0 = fw * ( ( gamma1 - fq ) * alfa +
     1             beta * gamma2 ) / z
        cmin0 = fw * ( ( gamma1 + fq ) * beta +
     1             alfa * gamma2 ) / z
      else
        fw = 3.1415927 * f0
        cadd0 = fw * ( g1g2 - fq ) / z
        cmin0 = fw * ( g1g2 + fq ) / z
      endif

      cadd1 = cadd0 * x
      cmin1 = cmin0 * x
      return
      end
!========================================================================
      subroutine qccgts ( ib, asbs, eex )
c  ********************************************************************
c  In the solar band  asbs is the surface albedo, while in the infrared
c  band asbs is  blackbody intensity emitted at the surface temperature
c  times surface emissivity.  In this subroutine,  the generalized two-
c  stream is applied to nonhomogeneous atmospheres. eex is the IR
c  surface emissivity. 
c  ********************************************************************
      USE FUINPUT
      implicit none
 
      integer ndfs2x,ib,k,ibn,k1,k2
      
      parameter ( ndfs2x = ndfsx * 2 )
      real lamdan,w1,w2,w3,w,t,u0q,f0,wn,w1n,w2n
      real w3n,t0n,t1n,u0n, f0n,gamman,caddn,cminn,caddn0,cminn0
      real aa,bb,expn,g1g2n,fkbn,eex,asbs,wm1,wm2,rsfc,ssfc
      real a(ndfs2x), b(ndfs2x), c(ndfs2x), r(ndfs2x), u(ndfs2x)
      real xn(ndfsx), yn(ndfsx), zn(ndfsx),gam(ndfs2x)

      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0q(ndfsx), f0(ndfsx)
      common /coedfi/ ibn, wn, w1n, w2n, w3n, t0n, t1n, u0n, f0n
      common / gtscoe / lamdan(ndfsx), gamman(ndfsx), caddn(ndfsx),
     1                    cminn(ndfsx), caddn0(ndfsx), cminn0(ndfsx),
     1                aa(ndfsx), bb(ndfsx), expn(ndfsx), g1g2n(ndfsx),
     1                    fkbn(ndfsx)

      ibn = ib
      do 40 k = 1, ndfs
       wn = w(k)
       w1n = w1(k)
       if ( k .eq. 1 ) then
         t0n = 0.0
       else
         t0n = t(k-1)
       endif
       t1n = t(k)
       u0n = u0q(k)
       f0n = f0(k)
       if ( wn .ge. 0.999999 ) then
         wn = 0.999999
       endif
       call cfgts ( lamdan(k), gamman(k), caddn0(k), caddn(k),
     1                  cminn0(k), cminn(k), g1g2n(k) , fkbn(k))
       expn(k) = exp ( - lamdan(k) * ( t1n - t0n ) )
       xn(k) = gamman(k) * expn(k)
       yn(k) = ( expn(k) - gamman(k) ) / ( xn(k) - 1.0 )
       zn(k) = ( expn(k) + gamman(k) ) / ( xn(k) + 1.0 )
40     continue

      a(1) = 0.0
      b(1) = xn(1) + 1.0
      c(1) = xn(1) - 1.0
      r(1) = - cminn0(1)
      do 50 k = 1, ndfs - 1
       k1 = k + k
       k2 = k + k + 1
       a(k1) = 1.0 + xn(k) - yn(k+1) * ( gamman(k) + expn(k) )
       b(k1) = 1.0 - xn(k) - yn(k+1) * ( gamman(k) - expn(k) )
       c(k1) = yn(k+1) * ( 1.0 + xn(k+1) ) - expn(k+1) - gamman(k+1)
       r(k1) = caddn0(k+1) - caddn(k) - yn(k+1) *
     1          ( cminn0(k+1) - cminn(k) )
       a(k2) = gamman(k) - expn(k) - zn(k) * ( 1.0 - xn(k) )
       b(k2) = -1.0 - xn(k+1) + zn(k) * ( expn(k+1) + gamman(k+1) )
       c(k2) = zn(k) * ( expn(k+1) - gamman(k+1) ) - xn(k+1) + 1.0
       r(k2) = cminn0(k+1) - cminn(k) - zn(k) *
     1          ( caddn0(k+1) - caddn(k) )
50     continue
      if ( ib .le. mbs ) then
        rsfc = asbs
        ssfc = 3.1415927 * u0q(1) * exp(-t(ndfs)/u0q(1)) * rsfc * f0(1)
      else
        rsfc = 1.0 - eex
        ssfc = 3.1415927 * asbs
      endif

      wm1 = 1.0 - rsfc * gamman(ndfs)
      wm2 = xn(ndfs) - rsfc * expn(ndfs)
      a(ndfs2) = wm1 + wm2
      b(ndfs2) = wm1 - wm2
      c(ndfs2) = 0.0
      r(ndfs2) = rsfc * cminn(ndfs) - caddn(ndfs) + ssfc
      call tridag ( a, b, c, r, u, gam, ndfs2 )
      IF (fi%ierror .NE. 0) RETURN
      do 60 k = 1, ndfs
       k1 = k + k - 1
       k2 = k + k
       aa(k) = u(k1) + u(k2)
       bb(k) = u(k1) - u(k2)
60     continue
      return
      end
!========================================================================
      subroutine tridag ( a, b, c, r, u, gam, n )
c *******************************************************************
c
c   | b1 c1 0  ...                |   | u1   |   | r1   |           
c   | a2 b2 c2 ...                |   | u2   |   | r2   |          
c   |          ...                | . | .    | = | .    |
c   |          ... an-1 bn-1 cn-1 |   | un-1 |   | rn-1 |          
c   |              0    an   bn   |   | un   |   | rn   |        
c
c This  subroutine solves for  a vector U of length N the tridiagonal
c linear set given by above equation. A, B, C and R are input vectors
c and are not modified (Numerical Recipes by Press et al. 1989).
c *******************************************************************
       USE FUINPUT ,only: fi
      implicit none
      integer n,j
      real gam(n), a(n), b(n), c(n), r(n), u(n),bet
      IF ( b(1) .EQ. 0. ) THEN
           fi%ierror = 1
           RETURN
      END IF
c  *********************************************************
c  If this happens then you should rewrite your equations as
c  a set of order n-1, with u2 trivially eliminated.
c  *********************************************************
      bet = b(1)
      u(1) = r(1) / bet

c  ***************************************
c  Decomposition and forward substitution.
c  ***************************************
      do 11 j = 2, n
       gam(j) = c(j-1) / bet
       bet = b(j) - a(j) * gam(j)
       IF ( bet .EQ. 0. ) THEN
           fi%ierror = 2
           RETURN
       END IF
c      ---------------------------------------
c      Algorithm fails; see Numerical Recipes.
c      ---------------------------------------
       u(j) = ( r(j) - a(j) * u(j-1) ) / bet
11     continue

c  ****************
c  Backsubstitution
c  ****************
      do 12 j = n - 1, 1, -1
       u(j) = u(j) - gam(j+1) * u(j+1)
12     continue

      return
      end
!========================================================================
      subroutine qftsts ( ib, asx, f0 )
c **********************************************************************
c The generalized two stream approximation for nonhomgeneous atmospheres
c in  the  solar  wavelengths.  The  input  parameters are those through
c 'para.file', through argument of 'qftsts' and through common statement
c 'dfsin' and 'gtslog'.
c **********************************************************************
      USE FUINPUT
      implicit none
 
      integer ib,n,m,i,k
      real ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t,u0a,f0a
      real lamdan,gamman,caddn,cminn,caddn0,cminn0,aa,bb,expn
      real g1g2n,fkbn,ffu,ffd,asx,f0,eex,asbs,fw3,xx
      real yy(ndfsx)

c---------- 10/28/96 (8)
      real ffdr,ffdf,yydr(ndfsx)
      common /dirdiff/ ffdr(nv1x), ffdf(nv1x)
c---------- 10/28/96 (8)
      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx),
     1                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0a(ndfsx), f0a(ndfsx)
      common / gtscoe / lamdan(ndfsx), gamman(ndfsx), caddn(ndfsx),
     1                    cminn(ndfsx), caddn0(ndfsx), cminn0(ndfsx),
     1                 aa(ndfsx), bb(ndfsx), expn(ndfsx), g1g2n(ndfsx),
     1                    fkbn(ndfsx)
      common /dfsout/ ffu(mdfsx), ffd(mdfsx)

      n = ndfs
      m = mdfs
      eex = 0.0
      asbs = asx
      call adjust2

      do 5 i = 1, n
       u0a(i) = u0
       f0a(i) = f0
5      continue


      call qccgts ( ib, asbs, eex )
      IF (fi%ierror .NE. 0) RETURN
      fw3 = u0 * 3.1415927 * f0
      do k = 1, ndfs
       yy(k) = exp(-t(k)/u0)
c---------- 10/28/96 (9)
       yydr(k) = exp(-tt(k)/u0)
c---------- 10/28/96 (9)
       enddo

      xx = aa(1) * expn(1)
      ffu(1) = xx + gamman(1) * bb(1) + caddn0(1)
      ffd(1) = gamman(1) * xx + bb(1) + cminn0(1) + fw3
c---------- 10/28/96 (10)
      ffdr(1) =  fw3
      ffdf(1) = ffd(1) - ffdr(1)
c---------- 10/28/96 (10)
      do 10 i = 2, m
       k = i - 1
       xx = bb(k) * expn(k)
       ffu(i) = aa(k) + gamman(k) * xx + caddn(k)
       ffd(i) = gamman(k) * aa(k) + xx + cminn(k) + fw3 * yy(k)
c---------- 10/28/96 (11)
       ffdr(i) = fw3 * yydr(k)
       ffdf(i) = ffd(i) - ffdr(i)
c---------- 10/28/96 (11)
10     continue
      
      return
      end
!========================================================================
      subroutine qftits ( ib, eex )
c **********************************************************************
c The exponential approximation for the Planck function in optical depth
c is used for the infrared ( Fu, 1991). Since the direct solar radiation
c source has an exponential function form in terms of optical depth, the
c formulation of generalized two stream approximation for infrared  wave
c lengths is the same as that for solar wavelengths. 
c The generalized two stream approximation for nonhomgeneous atmospheres
c in the infrared wavelengths.  The  input  parameters are those through
c 'para.file', through argument of 'qftits' and through common statement
c 'dfsin', 'gtslog', and 'planci'.
c **********************************************************************
      USE FUINPUT
      implicit none
 
      
      integer ib,n,m,i,k
      real lamdan,ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t
      real gamman,caddn,cminn,caddn0,cminn0,aa,bb,expn
      real g1g2n,fkbn,ffu,ffd,u0q,f0,eex,asbs,xx,q1,q2,bf,bs,t0
	real timt0
      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx),
     1                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0q(ndfsx), f0(ndfsx)
      common / gtscoe / lamdan(ndfsx), gamman(ndfsx), caddn(ndfsx),
     1                    cminn(ndfsx), caddn0(ndfsx), cminn0(ndfsx),
     1                 aa(ndfsx), bb(ndfsx), expn(ndfsx), g1g2n(ndfsx),
     1                    fkbn(ndfsx)
      common /dfsout/ ffu(mdfsx), ffd(mdfsx)
      common /planci/ bf(nv1x), bs
      n = ndfs
      m = mdfs
      asbs = bs * eex
      call adjust2
      t0 = 0.0

      do 3 i = 1, n
       q1 = alog ( bf(i+1) / bf(i) )

c Paul Stackhouse: July 5, 98
c          q2 = 1.0 / ( t(i) - t0 )
           timt0 = t(i) - t0 
           if (timt0 .lt. 1.0e-12) timt0 = 1.0e-12
           q2 = 1.0 / timt0
c Paul Stackhouse: July 5, 98

       if ( mquadr ) then
         f0(i) = 1.66 * ( 1.0 - w(i) ) * bf(i)
       else
         f0(i) = 2.0 * ( 1.0 - w(i) ) * bf(i)
       endif

       if ( abs(q1) .le. 1.0e-10 ) then
         u0q(i) = - 1.0e+10 / q2
       else
         u0q(i) = - 1.0 / ( q1 * q2 )
       endif
       t0 = t(i)
3      continue


      call qccgts ( ib, asbs, eex ) 
      IF (fi%ierror .NE. 0) RETURN
      xx = aa(1) * expn(1)
      ffu(1) = xx + gamman(1) * bb(1) + caddn0(1)
      ffd(1) = gamman(1) * xx + bb(1) + cminn0(1) 

      do 10 i = 2, m
       k = i - 1
       xx = bb(k) * expn(k)
       ffu(i) = aa(k) + gamman(k) * xx + caddn(k)
       ffd(i) = gamman(k) * aa(k) + xx + cminn(k)
10     continue
      return
      end
!========================================================================
	subroutine qftisf ( ib, eex  )
c 6-24-98 (8)	
c *********************************************************************
c In this subroutine, the two- and four- stream combination  scheme  or
c the source function technique (Toon et al. 1989) is used to calculate
c the IR radiative fluxes. The exponential approximation for the Planck
c function in optical depth is used ( Fu, 1991).
c At IR wavelengths, the two-stream results are not exact in the limit 
c of no scattering. It also introduces large error in the case of sca-
c ttering. Since the no-scattering limit is of considerable significance
c at IR wavelengths, we have used  the source function technique  that
c would be exact in the limit of the pure absorption and would also en-
c hance the accuracy of the two-stream approach when scattering occurs
c in the IR wavelengths.
c Here, we use nq Gauss points to obtain the fluxes: when nq=2, we use
c double Gaussian quadrature as in Fu and Liou (1993) for  four-stream
c approximation; when nq = 3, we use the regular Gauss quadrature  but
c u1*w1+u2*w2+u3*w3=1.0.
c The upward radiance at the cosine of zenith angle, ur, in the IR
c atmospheric window 800-980 cm**-1, 980-1100 cm**-1, 1100-1250 cm**-1
c is calculated based on equation (2.25) in Fu et al. (1997). 6-24-98.
c
c *********************************************************************
      USE FUINPUT
      implicit none
 
      
      integer ib,nq,n,m,i,j,i1
      parameter ( nq = 2 )
      real ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t
      real lamdan,gamman,caddn,cminn,caddn0,cminn0,aa,bb,expn
      real g1g2n,fkbn,ffu,ffd,u0q,f0,eex,asbs,xx,bf,bs,ugts1
      real t0,q1,q2,x,y,z,y1,yy
      real fg(ndfsx), fh(ndfsx), fj(ndfsx), fk(ndfsx)
      real alfa(ndfsx+1), beta(ndfsx)
      real fiu(mdfsx,nq), fid(mdfsx,nq),ub(ndfsx,nq)
      real fx(ndfsx,nq), fy(ndfsx), fz1(ndfsx,nq), fz2(ndfsx,nq)

      real fuq1(ndfsx), fuq2(ndfsx)
      real ug(nq), wg(nq), ugwg(nq)

      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx),
     1                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0q(ndfsx), f0(ndfsx)
      common / gtscoe / lamdan(ndfsx), gamman(ndfsx), caddn(ndfsx),
     1                    cminn(ndfsx), caddn0(ndfsx), cminn0(ndfsx),
     1                 aa(ndfsx), bb(ndfsx), expn(ndfsx), g1g2n(ndfsx),
     1                    fkbn(ndfsx)
      common /dfsout/ ffu(mdfsx), ffd(mdfsx)
c 6-24-98 (9/10a)	
      real fiur
	real timt0
      real  fxr(ndfsx), fz1r(ndfsx), fz2r(ndfsx), ubr(ndfsx)
  	common /radiance/ fiur(nv1x)  
c 6-24-98 (9/10a)
      common /planci/ bf(nv1x), bs

      data ug / 0.2113248, 0.7886752 /
      data wg / 0.5, 0.5 /
      data ugwg / 0.105662, 0.394338 /
      if ( mquadr ) then
        ugts1 = 0.60241
      else
        ugts1 = 0.5
      endif
      n = ndfs
      m = mdfs
      asbs = bs * eex
      call adjust2 
      t0 = 0.0

      do i = 1, n
       q1 = alog ( bf(i+1) / bf(i) )

c Paul Stackhouse: July 5, 98
c          q2 = 1.0 / ( t(i) - t0 )
           timt0 = t(i) - t0 
           if (timt0 .lt. 1.0e-12) timt0 = 1.0e-12
           q2 = 1.0 / timt0
c Paul Stackhouse: July 5, 98

       if ( mquadr ) then
         f0(i) = 1.66 * ( 1.0 - w(i) ) * bf(i)
       else
         f0(i) = 2.0 * ( 1.0 - w(i) ) * bf(i)
       endif
       if ( abs(q1) .le. 1.0e-10 ) then
         u0q(i) = - 1.0e+10 / q2
       else
         u0q(i) = - 1.0 / ( q1 * q2 )
       endif
       t0 = t(i)
       beta(i) = - 1.0 / u0q(i)
       enddo

      call qccgts ( ib, asbs, eex )
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

      do j = 1, nq
       fid(1,j) = 0.0
       enddo

      do j = 1, nq
       t0 = 0.0
       do i = 2, mdfs
        i1 = i - 1
        fx(i1,j) = exp ( - ( t(i1) - t0 ) / ug(j) )
        fy(i1) = expn(i1)
        xx = lamdan(i1) * ug(j)
        IF (xx == 1.0) xx = .99999
        fz1(i1,j) = ( 1.0 - fx(i1,j) * fy(i1) ) / ( xx + 1.0 )
        fz2(i1,j) = ( fx(i1,j) - fy(i1) ) / ( xx - 1.0 )
        ub(i1,j) = ug(j) * beta(i1)

c---------- 4/2/97 (7)
        if (ub(i1,j) .eq. -1.0) ub(i1,j) = -1.001
c---------- 4/2/97 (7)

        fid(i,j) = fid(i1,j) * fx(i1,j) + fj(i1) * fz1(i1,j) +
     1                fk(i1) * fz2(i1,j) + 
     1                fuq2(i1) / ( ub(i1,j) + 1.0 ) *
     1                ( alfa(i) - alfa(i1) * fx(i1,j) )
        t0 = t(i1)
        enddo
       enddo

      yy = 0.0
      do j = 1, nq
       yy = yy + ugwg(j) * fid(mdfs,j) 
       enddo

      xx = yy * ( 1.0 - eex ) * 2.0 + 6.2831854 * eex * bs 
      do j = 1, nq
       fiu(mdfs,j) = xx
       enddo
c 6-24-98 (11)	
	fiur(mdfs) = xx
c 6-24-98 (11)
      do j = 1, nq
       do i = mdfs - 1, 1, -1
        IF (ub(i,j) == 1.0) ub(i,j) = 1.0001
        fiu(i,j) = fiu(i+1,j) * fx(i,j) + fg(i) * fz2(i,j) +
     1                fh(i) * fz1(i,j) + 
     1                fuq1(i) / ( ub(i,j) - 1.0 ) *
     1                ( alfa(i+1) * fx(i,j) - alfa(i) )
        enddo
       enddo

      do i = 1, mdfs
       ffu(i) = 0.0
       ffd(i) = 0.0
       enddo

      do i = 1, mdfs
       do j = 1, nq
        ffu(i) = ffu(i) + ugwg(j) * fiu(i,j) 
        ffd(i) = ffd(i) + ugwg(j) * fid(i,j)
        enddo
       enddo
c 6-24-98 (12) 
!!	if ( ib .eq. 11 .or. ib .eq. 12 .or. ib .eq. 13 ) then
	if ( ur .eq. 0.5 ) then
           ur = 0.500001
	endif
	t0 = 0.0
	do i = 2, mdfs
	   i1 = i - 1
	   fxr(i1) = exp ( - ( t(i1) - t0 ) / ur )
       xx = lamdan(i1) * ur
       IF (xx == 1.0) xx = .99999
       fz1r(i1) = ( 1.0 - fxr(i1) * fy(i1) ) / ( xx + 1.0 )
       fz2r(i1) = ( fxr(i1) - fy(i1) ) / ( xx - 1.0 )
	   ubr(i1) = ur * beta(i1)
	   t0 = t(i1)
	enddo
	do i = mdfs - 1, 1, -1
      IF (ubr(i) == 1.0) ubr(i) = 1.0001
      fiur(i) = fiur(i+1) * fxr(i) + fg(i) * fz2r(i) +
     1               fh(i) * fz1r(i) + 
     1               fuq1(i) / ( ubr(i) - 1.0 ) *
     1               ( alfa(i+1) * fxr(i) - alfa(i) ) 
	enddo
!	else
!	do i = 1, mdfs
!           fiur(i) = 0.0
!	enddo
!	endif
c 6-24-98 (12) 
      return
      end
!========================================================================
      subroutine adjust2 
c  *****************************************************************
c  In this subroutine, we incorporate a delta-function adjustment to
c  account for the  forward  diffraction  peak in the context of the 
c  two-stream approximation ( Liou, Fu and Ackerman, 1988 ).  w1(n),
c  w(n), and t(n) are the adjusted parameters.
c  *****************************************************************
      USE FUINPUT
      implicit none
 
      integer i,n
      real ww1,ww2,ww3,ww4,ww,tt,w1,w2,w3,w,t,u0a
      real f0a,dtt(ndfsx), dt(ndfsx),tt0,f,fw
      common /dfsin/ ww1(ndfsx), ww2(ndfsx), ww3(ndfsx), ww4(ndfsx),
     1                     ww(ndfsx), tt(ndfsx)
      common /qccfei/ w1(ndfsx), w2(ndfsx), w3(ndfsx), w(ndfsx), 
     1                      t(ndfsx), u0a(ndfsx), f0a(ndfsx)

      n = ndfs
      tt0 = 0.0
      do 10 i = 1, n
       f = ww2(i) / 5.0
       fw = 1.0 - f * ww(i) 
       w1(i) = ( ww1(i) - 3.0 * f ) / ( 1.0 - f )
       w(i) = ( 1.0 - f ) * ww(i) / fw
       dtt(i) = tt(i) - tt0
       tt0 = tt(i)
       dt(i) = dtt(i) * fw
10     continue
      t(1) = dt(1)

      do 20 i = 2, n
       t(i) = dt(i) + t(i-1)
20     continue

      return
      end
!========================================================================
c---------- 4/2/97 (3) -- NEXT 801 LINES
      subroutine gascon_ckd_parm( ib )
c  *******************************************
c          4/2/97 - F.Rose
c  Parameterized CKD_2.1 continuum absorption.
c  *******************************************
      USE FUINPUT
      implicit none

      integer m,ib
      real tgm
      real dp,amnt,patm,temp,parm_ckd,parm_ckd24
      integer iflb(mbx)
 
      common /con/ tgm(nvx)
      
      data iflb /6*0,3*0,12,11,10,9,8,7,6,5,4,3,2,1/

      do m=1,nv
      tgm(m)=0.0
      end do

      if( iflb(ib) .eq. 0) return

      do m=1,nv
       dp     =   pp(m+1) - pp(m)
       amnt   = 1.02*dp*(ph(m)+ph(m+1) ) *0.5
       patm   = (( pp(m)   + pp(m+1) ) *0.5 ) /1013.25
       temp   = ( pt(m)   + pt(m+1) ) *0.5
!	dz = dz_quick(pp(m),pp(m+1),pt(m),pt(m+1),ph(m),ph(m+1) )
       if ( irobckd ==4) tgm(m)=parm_ckd(iflb(ib),amnt,patm,temp,dz(m))
       if (irobckd ==5)tgm(m)=parm_ckd24(iflb(ib),amnt,patm,temp,dz(m))
       end do

      return
      end

c*********************************************************************
      function parm_ckd(iband,amnt,patm,temp,dz)
c Parameterization of CKD_2.1 continuum over Fu-Liou Bands
c Input:
c iband  =  integer (1-8) where
c	  Band 1 ='  5:280cm-1'
c         Band 2 ='280:400cm-1'
c         Band 3 ='400:540cm-1'
c         Band 4 ='540:670cm-1'
c         Band 5 ='670:800cm-1'
c         Band 6 ='800:980cm-1'
c         Band 7 ='980:1100cm-1'
c         Band 8 ='1100:1250cm-1'
c         Band 9 ='1250:1400cm-1'
c         Band10 ='1400:1700cm-1'
c         Band11 ='1700:1900cm-1'
c         Band12 ='1900:2200cm-1'
c amnt = h2O ammount (g/cm**2)
c patm= pressure (atm)
c temp = temperature (k)
c dz = pathlength (Km)
c Output:
c clough_parm = parameterized CKD_2.1optical depth for band
c234567890123456789012345678901234567890123456789012345678901234567890
	USE FUINPUT ,only: iwtas,fi
        parameter (ncoef=7,nband=12)
	real aa(ncoef,nband) ,aa1,aa2,aa3,aa4
	common /pcont_aa0/ aa0(ncoef,nband) ! ORIGINAL 280:1400
	common /pcont_aa1/ aa1(ncoef,nband)! lin no plank 
	common /pcont_aa2/ aa2(ncoef,nband)! log no plank
	common /pcont_aa3/ aa3(ncoef,nband)! lin Plank wgt
	common /pcont_aa4/ aa4(ncoef,nband)! log Plank wgt
!	common /cont_tas/ iwtas
        if(iwtas <0 .or. iwtas >4)fi%ierror = 31! print*, 'iwtas'
	if ( iwtas == 0) aa= aa0
	if ( iwtas == 1) aa= aa1
	if ( iwtas == 2) aa= aa2
	if ( iwtas == 3) aa= aa3
	if ( iwtas == 4) aa= aa4

       ph2o = amnt *(8.314d+07 *temp )/
     &              (dz*1.0d+05*18.01534 *1.01325d+06)

	if (iwtas == 0) patmx = patm
	if (iwtas >  0) patmx = log(patm)


 	tau_log	    = aa(1,iband)	      +
     $		      aa(2,iband)* log(amnt)  +
     $		      aa(3,iband)* temp       +
     $                aa(4,iband)* patmx       +
     $		      aa(5,iband)* (ph2o)     +
     $		      aa(6,iband)* amnt       +
     $		      aa(7,iband)* log(ph2o) 

    
	parm_ckd = exp ( tau_log )

	return
	end
!========================================================================
      subroutine gascon_off
c  ******************************
c          4/2/97 - F.Rose
c Continuum absorption turned off.
c  ******************************
      USE FUINPUT
      implicit none
 
      integer i
      real tgm
      common /con/ tgm(nvx)

      do i = 1, nv
       tgm(i) = 0.0
       end do

      return
      end



      block data
c **********************************************************************
c Double-Gauss quadratures and weights (Sykes, 1951).
c **********************************************************************
      implicit none
      real a,u
      common /dis/ a(4)
      common /point/ u(4)
      data a / 0.5, 0.5, 0.5, 0.5 /
      data u / -0.7886752, -0.2113247, 0.2113247, 0.7886752 /
      end
      




      block data legend
c **********************************************************************
c p0d(4), p1d(4), p2d(4), and p3d(4) are Legendre polynomials p0(x), 
c p1(x), p2(x), and p3(x) when x = u(1), u(2), u(3), and u(4).
c **********************************************************************
      implicit none
      real p0d, p1d, p2d, p3d
      common /legen/ p0d(4), p1d(4), p2d(4), p3d(4)
      data p0d /  .100000E+01,  .100000E+01,  .100000E+01, .100000E+01 /
      data p1d / -.788675E+00, -.211325E+00,  .211325E+00, .788675E+00 /
      data p2d /  .433013E+00, -.433013E+00, -.433013E+00, .433013E+00 /
      data p3d / -.433940E-01,  .293394E+00, -.293394E+00, .433940E-01 /
      end

      block data legenf
c *********************************************************************
c p11d(4,4), p22d(4,4), and p33d(4,4) are defined as 0.5*p1d(i)*p1d(j),
c 0.5*p2d(i)*p2d(j), and 0.5*p3d(i)*p3d(j), respectively.
c *********************************************************************
      implicit none
      real p11d, p22d, p33d
      common /legen1/ p11d(4,4), p22d(4,4), p33d(4,4)
      data p11d / .311004E+00, .833334E-01,-.833334E-01,-.311004E+00,
     1            .833334E-01, .223291E-01,-.223291E-01,-.833334E-01,
     1           -.833334E-01,-.223291E-01, .223291E-01, .833334E-01,
     1           -.311004E+00,-.833334E-01, .833334E-01, .311004E+00 /
      data p22d / .937501E-01,-.937501E-01,-.937501E-01, .937501E-01,
     1           -.937501E-01, .937501E-01, .937501E-01,-.937501E-01,
     1           -.937501E-01, .937501E-01, .937501E-01,-.937501E-01,
     1            .937501E-01,-.937501E-01,-.937501E-01, .937501E-01 /
      data p33d / .941520E-03,-.636577E-02, .636577E-02,-.941520E-03,
     1           -.636577E-02, .430400E-01,-.430400E-01, .636577E-02,
     1            .636577E-02,-.430400E-01, .430400E-01,-.636577E-02,
     1           -.941520E-03, .636577E-02,-.636577E-02, .941520E-03 /
      end
      
c---------- 4/1/97 (6) -- Replaces old ckd1 block data.

      block data ckd2
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, eleven 
c pressures,  and eight cumulative probabilities  ( Fu,  1991 ). The
c spectral region is from 14500 to 7700 cm**-1.
c *********************************************************************
      common /band2/ hk(8), coeh2o(3,11,8)
      data hk / .71, .11, .06, .06, .04, .016, .0034, .0006 /
c   .343849E+03    .532724E+02    .290577E+02    .290577E+02    .193718E+02
c   .774872E+01    .164660E+01    .290577E+00
      data ( ( ( coeh2o(k,j,i), i = 1, 8 ), j = 1, 11 ), k = 1, 3 ) /
     +-.1735E+02,-.1407E+02,-.1268E+02,-.1131E+02,-.9261E+01,-.6666E+01,
     +-.3937E+01,-.5448E+00,-.1690E+02,-.1365E+02,-.1232E+02,-.1101E+02,
     +-.9058E+01,-.6574E+01,-.3914E+01,-.5529E+00,-.1643E+02,-.1323E+02,
     +-.1195E+02,-.1068E+02,-.8840E+01,-.6475E+01,-.3889E+01,-.6143E+00,
     +-.1598E+02,-.1282E+02,-.1157E+02,-.1035E+02,-.8598E+01,-.6339E+01,
     +-.3848E+01,-.6636E+00,-.1551E+02,-.1241E+02,-.1119E+02,-.1001E+02,
     +-.8342E+01,-.6178E+01,-.3788E+01,-.8181E+00,-.1506E+02,-.1201E+02,
     +-.1082E+02,-.9692E+01,-.8073E+01,-.6017E+01,-.3703E+01,-.9003E+00,
     +-.1446E+02,-.1154E+02,-.1042E+02,-.9332E+01,-.7810E+01,-.5846E+01,
     +-.3576E+01,-.1083E+01,-.1394E+02,-.1112E+02,-.1005E+02,-.8992E+01,
     +-.7548E+01,-.5674E+01,-.3477E+01,-.1266E+01,-.1351E+02,-.1076E+02,
     +-.9722E+01,-.8702E+01,-.7334E+01,-.5531E+01,-.3401E+01,-.1524E+01,
     +-.1311E+02,-.1044E+02,-.9422E+01,-.8423E+01,-.7117E+01,-.5383E+01,
     +-.3410E+01,-.1785E+01,-.1274E+02,-.1015E+02,-.9162E+01,-.8190E+01,
     +-.6949E+01,-.5236E+01,-.3477E+01,-.2082E+01, .2407E-02, .2847E-02,
     + .3768E-02, .4626E-02, .5631E-02, .4542E-02, .3475E-02,-.3085E-02,
     + .2428E-02, .2805E-02, .3412E-02, .3893E-02, .4773E-02, .3998E-02,
     + .2742E-02,-.2556E-02, .2428E-02, .2721E-02, .3077E-02, .3161E-02,
     + .4019E-02, .3224E-02, .2512E-02,-.1884E-02, .2449E-02, .2617E-02,
     + .2763E-02, .2658E-02, .3286E-02, .2617E-02, .1989E-02,-.1740E-02,
     + .2512E-02, .2470E-02, .2470E-02, .2282E-02, .2512E-02, .1926E-02,
     + .1465E-02,-.2612E-02, .2554E-02, .2303E-02, .2303E-02, .1842E-02,
     + .2030E-02, .1340E-02, .1068E-02,-.1413E-02, .2449E-02, .2198E-02,
     + .2030E-02, .1465E-02, .1528E-02, .9838E-03, .1005E-02,-.1099E-02,
     + .2868E-02, .2198E-02, .1968E-02, .1382E-02, .1172E-02, .5652E-03,
     + .6070E-03,-.1662E-02, .3077E-02, .2219E-02, .1800E-02, .1277E-02,
     + .1005E-02, .3349E-03, .2512E-03,-.1195E-02, .3182E-02, .2219E-02,
     + .1758E-02, .1172E-02, .7326E-03, .4815E-03, .6280E-04,-.1880E-02,
     + .3265E-02, .2114E-02, .1696E-02, .1298E-02, .4187E-03, .4187E-03,
     +-.3768E-03,-.1467E-02,-.1180E-04,-.1294E-04,-.1142E-04,-.7232E-05,
     +-.8754E-05,-.1484E-04,-.8373E-05, .1028E-04,-.1218E-04,-.1142E-04,
     +-.9515E-05,-.1522E-05,-.9134E-05,-.1484E-04,-.3425E-05, .1142E-06,
     +-.1294E-04,-.9895E-05,-.7231E-05,-.4187E-05,-.7612E-05,-.3806E-05,
     + .1522E-05,-.3882E-05,-.1256E-04,-.8754E-05,-.7612E-05,-.6470E-05,
     +-.4948E-05,-.3425E-05, .4948E-05,-.1054E-04,-.1370E-04,-.6089E-05,
     +-.8373E-05,-.5709E-05,-.3045E-05,-.3806E-05, .5328E-05, .8678E-05,
     +-.1370E-04,-.6851E-05,-.8373E-05,-.1522E-05,-.3425E-05, .0000E+00,
     + .1256E-04,-.1572E-04,-.1484E-04,-.7231E-05,-.7992E-05,-.4567E-05,
     +-.2664E-05,-.3807E-06,-.1522E-05, .2169E-05,-.1713E-04,-.9515E-05,
     +-.6089E-05,-.6851E-05,-.3045E-05,-.1142E-05, .1903E-05, .9363E-05,
     +-.1560E-04,-.9134E-05,-.5328E-05,-.4948E-05, .0000E+00, .7611E-06,
     +-.6851E-05, .1252E-04,-.1522E-04,-.8373E-05,-.6089E-05,-.6089E-05,
     +-.3805E-06,-.1142E-05,-.3807E-06, .2512E-05,-.1599E-04,-.7231E-05,
     +-.5709E-05,-.4567E-05, .1522E-05,-.2284E-05,-.3941E-10, .5290E-05/
      end
      
      block data ckd3
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, eleven 
c pressures,  and twelve cumulative probabilities ( Fu,  1991 ). The
c spectral region is from 7700 to 5250 cm**-1.
c *********************************************************************
      common /band3/ hk(12), coeh2o(3,11,12)
      data hk / .34, .11, .1, .09, .12, .1,
     1            .06, .04, .026, .01, .0035, .0005 /
c   .509474E+02    .164830E+02    .149845E+02    .134861E+02    .179814E+02
c   .149845E+02    .899071E+01    .599381E+01    .389597E+01    .149845E+01
c   .524458E+00    .749226E-01
      data ( ( ( coeh2o(k,j,i), i = 1, 12 ), j = 1, 11 ), k = 1, 3 ) /
     +-.1900E+02,-.1515E+02,-.1344E+02,-.1224E+02,-.1081E+02,-.9337E+01,
     +-.7965E+01,-.6585E+01,-.4578E+01,-.2247E+01, .1747E+00, .3083E+01,
     +-.1854E+02,-.1471E+02,-.1300E+02,-.1181E+02,-.1039E+02,-.8927E+01,
     +-.7576E+01,-.6238E+01,-.4317E+01,-.2119E+01, .1888E+00, .3033E+01,
     +-.1808E+02,-.1426E+02,-.1257E+02,-.1137E+02,-.9966E+01,-.8513E+01,
     +-.7177E+01,-.5885E+01,-.4053E+01,-.1977E+01, .2245E+00, .3005E+01,
     +-.1763E+02,-.1381E+02,-.1213E+02,-.1094E+02,-.9542E+01,-.8094E+01,
     +-.6779E+01,-.5524E+01,-.3788E+01,-.1796E+01, .2961E+00, .2828E+01,
     +-.1716E+02,-.1337E+02,-.1170E+02,-.1051E+02,-.9116E+01,-.7677E+01,
     +-.6381E+01,-.5153E+01,-.3493E+01,-.1607E+01, .3850E+00, .2660E+01,
     +-.1670E+02,-.1295E+02,-.1127E+02,-.1008E+02,-.8690E+01,-.7265E+01,
     +-.5991E+01,-.4799E+01,-.3212E+01,-.1438E+01, .4582E+00, .2588E+01,
     +-.1596E+02,-.1231E+02,-.1067E+02,-.9501E+01,-.8151E+01,-.6793E+01,
     +-.5588E+01,-.4458E+01,-.2940E+01,-.1257E+01, .4888E+00, .2260E+01,
     +-.1530E+02,-.1184E+02,-.1017E+02,-.8992E+01,-.7661E+01,-.6369E+01,
     +-.5213E+01,-.4145E+01,-.2701E+01,-.1108E+01, .4239E+00, .1974E+01,
     +-.1481E+02,-.1144E+02,-.9756E+01,-.8573E+01,-.7255E+01,-.5994E+01,
     +-.4868E+01,-.3829E+01,-.2485E+01,-.9738E+00, .3343E+00, .1667E+01,
     +-.1439E+02,-.1108E+02,-.9360E+01,-.8183E+01,-.6885E+01,-.5646E+01,
     +-.4559E+01,-.3555E+01,-.2314E+01,-.8904E+00, .2169E+00, .1289E+01,
     +-.1402E+02,-.1073E+02,-.8987E+01,-.7817E+01,-.6551E+01,-.5335E+01,
     +-.4278E+01,-.3316E+01,-.2147E+01,-.8695E+00, .1587E-01, .8658E+00,
     + .1132E-01, .8855E-02, .6698E-02, .5296E-02, .4396E-02, .3370E-02,
     + .3245E-02, .4145E-02, .4731E-02, .4756E-02, .3116E-02,-.2763E-02,
     + .1135E-01, .8917E-02, .6657E-02, .5170E-02, .4207E-02, .3056E-02,
     + .2868E-02, .3433E-02, .3726E-02, .4109E-02, .2836E-02,-.3119E-02,
     + .1135E-01, .8980E-02, .6615E-02, .5045E-02, .4061E-02, .2847E-02,
     + .2491E-02, .2847E-02, .2910E-02, .2671E-02, .2396E-02,-.3245E-02,
     + .1135E-01, .9043E-02, .6594E-02, .4940E-02, .3914E-02, .2638E-02,
     + .2156E-02, .2261E-02, .2051E-02, .1978E-02, .1566E-02,-.3203E-02,
     + .1139E-01, .9085E-02, .6531E-02, .4835E-02, .3768E-02, .2428E-02,
     + .1842E-02, .1612E-02, .1591E-02, .1279E-02, .7201E-03,-.2763E-02,
     + .1143E-01, .9085E-02, .6447E-02, .4752E-02, .3684E-02, .2261E-02,
     + .1570E-02, .1235E-02, .1151E-02, .7243E-03, .6489E-04,-.2240E-02,
     + .1135E-01, .9001E-02, .5694E-02, .4438E-02, .3412E-02, .1968E-02,
     + .1235E-02, .9420E-03, .8792E-03, .5045E-03,-.1821E-03,-.1936E-02,
     + .1174E-01, .9273E-02, .5882E-02, .4689E-02, .3454E-02, .1947E-02,
     + .1151E-02, .6070E-03, .6698E-03, .9420E-04,-.6740E-03,-.2707E-02,
     + .1218E-01, .9336E-02, .6050E-02, .4731E-02, .3475E-02, .1863E-02,
     + .1151E-02, .4605E-03, .3768E-03,-.1214E-03,-.4396E-03,-.1903E-02,
     + .1235E-01, .9294E-02, .6029E-02, .4584E-02, .3370E-02, .1800E-02,
     + .1068E-02, .2303E-03, .1675E-03,-.4501E-03,-.7571E-03,-.1149E-02,
     + .1233E-01, .9315E-02, .6029E-02, .4438E-02, .3203E-02, .1842E-02,
     + .9629E-03, .0000E+00,-.2198E-03,-.5338E-03,-.9721E-03,-.7661E-03,
     +-.3692E-04,-.3844E-04,-.2588E-04,-.1180E-04,-.1066E-04,-.3426E-05,
     +-.2664E-05, .7611E-06, .6089E-05,-.4568E-06,-.2077E-04,-.1142E-04,
     +-.3730E-04,-.3806E-04,-.2360E-04,-.1256E-04,-.1180E-04,-.4567E-05,
     +-.3425E-05,-.2284E-05,-.1522E-05,-.4225E-05,-.9940E-05,-.4187E-05,
     +-.3501E-04,-.3844E-04,-.2131E-04,-.1256E-04,-.9896E-05,-.3806E-05,
     +-.4186E-05, .7612E-06,-.1903E-05, .4110E-05, .1789E-05,-.2169E-04,
     +-.3425E-04,-.3882E-04,-.1941E-04,-.1294E-04,-.9515E-05,-.4567E-05,
     +-.4186E-05, .1522E-05,-.4187E-10, .4605E-05,-.2588E-05, .6470E-05,
     +-.3501E-04,-.3730E-04,-.1751E-04,-.1332E-04,-.1066E-04,-.3806E-05,
     +-.4567E-05,-.1142E-05,-.3045E-05, .1104E-05,-.1058E-04, .2816E-04,
     +-.3578E-04,-.3501E-04,-.1751E-04,-.1332E-04,-.1218E-04,-.3806E-05,
     +-.3425E-05,-.3806E-06,-.4187E-05,-.6090E-06,-.6965E-05,-.3463E-04,
     +-.3578E-04,-.3349E-04,-.1675E-04,-.9895E-05,-.9515E-05,-.6090E-05,
     +-.6470E-05,-.3807E-06,-.5328E-05,-.4186E-06,-.3996E-05, .2074E-04,
     +-.3540E-04,-.3083E-04,-.1789E-04,-.9896E-05,-.1104E-04,-.6470E-05,
     +-.5709E-05, .3425E-05,-.4567E-05, .3463E-05, .5633E-05,-.3159E-05,
     +-.3730E-04,-.2740E-04,-.1484E-04,-.1066E-04,-.1142E-04,-.6470E-05,
     +-.6470E-05, .1522E-05,-.1522E-05,-.3045E-05, .3197E-05,-.1039E-04,
     +-.3425E-04,-.2284E-04,-.1370E-04,-.1028E-04,-.1104E-04,-.8373E-05,
     +-.4948E-05, .1903E-05,-.7612E-06,-.1104E-05, .2455E-05,-.3805E-07,
     +-.3235E-04,-.2093E-04,-.1294E-04,-.1142E-04,-.1180E-04,-.6851E-05,
     +-.3045E-05,-.7611E-06, .1256E-05,-.7231E-06, .9924E-05, .3578E-05/
      end
      
      block data ckd4
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, eleven 
c pressures,  and seven cumulative probabilities ( Fu,  1991 ). The
c spectral region is from 5250 to 4000 cm**-1.
c *********************************************************************
      common /band4/ hk(7), coeh2o(3,11,7)
      data hk / .52, .21, .11, .1, .04, .015, .005 /
c   .253397E+02    .102333E+02    .536032E+01    .487302E+01    .194921E+01
c   .730953E+00    .243651E+00
      data ( ( ( coeh2o(k,j,i), i = 1, 7 ), j = 1, 11 ), k = 1, 3 ) /
     +-.1722E+02,-.1402E+02,-.1202E+02,-.1001E+02,-.7702E+01,-.5273E+01,
     +-.6530E+00,-.1677E+02,-.1359E+02,-.1164E+02,-.9662E+01,-.7419E+01,
     +-.5001E+01,-.6040E+00,-.1630E+02,-.1316E+02,-.1125E+02,-.9303E+01,
     +-.7092E+01,-.4750E+01,-.5715E+00,-.1584E+02,-.1274E+02,-.1086E+02,
     +-.8939E+01,-.6751E+01,-.4458E+01,-.4928E+00,-.1538E+02,-.1232E+02,
     +-.1048E+02,-.8579E+01,-.6399E+01,-.4191E+01,-.4683E+00,-.1493E+02,
     +-.1192E+02,-.1011E+02,-.8241E+01,-.6065E+01,-.3910E+01,-.4310E+00,
     +-.1440E+02,-.1145E+02,-.9643E+01,-.7873E+01,-.5710E+01,-.3668E+01,
     +-.3304E+00,-.1391E+02,-.1104E+02,-.9238E+01,-.7479E+01,-.5367E+01,
     +-.3387E+01,-.3604E+00,-.1348E+02,-.1069E+02,-.8918E+01,-.7122E+01,
     +-.5086E+01,-.3152E+01,-.3030E+00,-.1310E+02,-.1037E+02,-.8626E+01,
     +-.6790E+01,-.4815E+01,-.2945E+01,-.4789E+00,-.1275E+02,-.1011E+02,
     +-.8347E+01,-.6484E+01,-.4584E+01,-.2788E+01,-.5807E+00, .7934E-02,
     + .9231E-02, .1005E-01, .9043E-02, .8164E-02, .8980E-02, .6403E-02,
     + .7954E-02, .9169E-02, .9797E-02, .8687E-02, .7724E-02, .7954E-02,
     + .6652E-02, .7954E-02, .9043E-02, .9608E-02, .8499E-02, .7347E-02,
     + .7473E-02, .6382E-02, .7996E-02, .8980E-02, .9378E-02, .8289E-02,
     + .7264E-02, .6594E-02, .6674E-02, .8059E-02, .8938E-02, .9294E-02,
     + .8227E-02, .7201E-02, .6678E-02, .7032E-02, .8122E-02, .8896E-02,
     + .9189E-02, .8038E-02, .7033E-02, .5987E-02, .5475E-02, .8268E-02,
     + .9064E-02, .8792E-02, .7975E-02, .6573E-02, .5087E-02, .4657E-02,
     + .8541E-02, .8980E-02, .9085E-02, .7996E-02, .6133E-02, .4501E-02,
     + .3860E-02, .8813E-02, .9043E-02, .9294E-02, .8122E-02, .5861E-02,
     + .4354E-02, .3964E-02, .8875E-02, .8834E-02, .9797E-02, .8164E-02,
     + .5463E-02, .4417E-02, .3270E-02, .8938E-02, .8771E-02, .1005E-01,
     + .8247E-02, .5589E-02, .4835E-02, .3033E-02,-.1484E-04,-.2169E-04,
     +-.2436E-04,-.2588E-04,-.1142E-04,-.1142E-05,-.1519E-04,-.1522E-04,
     +-.2055E-04,-.2131E-04,-.2398E-04,-.4948E-05,-.1675E-04,-.3593E-04,
     +-.1522E-04,-.2055E-04,-.1865E-04,-.2207E-04,-.4948E-05,-.1180E-04,
     +-.1237E-04,-.1598E-04,-.2017E-04,-.1903E-04,-.2284E-04,-.1028E-04,
     +-.1865E-04,-.2381E-04,-.1713E-04,-.2017E-04,-.1827E-04,-.2169E-04,
     +-.1218E-04,-.9515E-05,-.2415E-04,-.1827E-04,-.2093E-04,-.1637E-04,
     +-.1827E-04,-.9134E-05,-.8373E-05,-.1243E-04,-.1560E-04,-.1865E-04,
     +-.1599E-04,-.1256E-04,-.1066E-04,-.1142E-05,-.2181E-04,-.1675E-04,
     +-.1560E-04,-.1522E-04,-.1675E-04,-.1865E-04,-.1865E-04,-.9522E-05,
     +-.1332E-04,-.1370E-04,-.1446E-04,-.2055E-04,-.1142E-04,-.2512E-04,
     +-.3343E-04,-.1294E-04,-.1294E-04,-.1751E-04,-.2512E-04,-.1560E-04,
     +-.2854E-04,-.7003E-05,-.8753E-05,-.1028E-04,-.1751E-04,-.2512E-04,
     +-.1713E-04,-.1713E-04,-.1245E-04 /
      end
      
      block data ckd5
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, eleven 
c pressures,  and twelve cumulative probabilities ( Fu,  1991 ). The
c spectral region is from 4000 to 2850 cm**-1.
c *********************************************************************
      common /band5/ hk(12), coeh2o(3,11,12)
      data hk / .13, .14, .13, .16, .18, .14, 
     1                .07, .02, .016, .008, .004, .002 /
c   .411549E+01    .443207E+01    .411549E+01    .506522E+01    .569837E+01
c   .443207E+01    .221603E+01    .633153E+00    .506522E+00    .253261E+00
c   .126631E+00    .633153E-01
      data ( ( ( coeh2o(k,j,i), i = 1, 12 ), j = 1, 11 ), k = 1, 3 ) /
     +-.1499E+02,-.1267E+02,-.1118E+02,-.9696E+01,-.7992E+01,-.6323E+01,
     +-.4414E+01,-.2961E+01,-.1715E+01,-.1406E+00, .1612E+01, .3689E+01,
     +-.1454E+02,-.1223E+02,-.1075E+02,-.9277E+01,-.7576E+01,-.5915E+01,
     +-.4043E+01,-.2630E+01,-.1449E+01, .2314E-01, .1708E+01, .3744E+01,
     +-.1408E+02,-.1178E+02,-.1031E+02,-.8851E+01,-.7154E+01,-.5503E+01,
     +-.3666E+01,-.2288E+01,-.1141E+01, .2772E+00, .1819E+01, .3788E+01,
     +-.1363E+02,-.1134E+02,-.9876E+01,-.8423E+01,-.6733E+01,-.5091E+01,
     +-.3286E+01,-.1938E+01,-.8649E+00, .5349E+00, .1969E+01, .3795E+01,
     +-.1318E+02,-.1091E+02,-.9452E+01,-.8004E+01,-.6309E+01,-.4677E+01,
     +-.2904E+01,-.1595E+01,-.5641E+00, .7592E+00, .2109E+01, .3783E+01,
     +-.1275E+02,-.1048E+02,-.9028E+01,-.7585E+01,-.5892E+01,-.4267E+01,
     +-.2524E+01,-.1274E+01,-.2782E+00, .9376E+00, .2257E+01, .3714E+01,
     +-.1180E+02,-.9887E+01,-.8492E+01,-.7014E+01,-.5390E+01,-.3834E+01,
     +-.2156E+01,-.9775E+00,-.3129E-01, .1151E+01, .2330E+01, .3592E+01,
     +-.1114E+02,-.9367E+01,-.8002E+01,-.6514E+01,-.4928E+01,-.3435E+01,
     +-.1835E+01,-.7064E+00, .2153E+00, .1309E+01, .2422E+01, .3488E+01,
     +-.1074E+02,-.8941E+01,-.7582E+01,-.6116E+01,-.4536E+01,-.3072E+01,
     +-.1521E+01,-.4651E+00, .4053E+00, .1465E+01, .2374E+01, .3260E+01,
     +-.1041E+02,-.8545E+01,-.7180E+01,-.5745E+01,-.4177E+01,-.2735E+01,
     +-.1245E+01,-.2356E+00, .5786E+00, .1516E+01, .2263E+01, .3074E+01,
     +-.1008E+02,-.8149E+01,-.6804E+01,-.5409E+01,-.3855E+01,-.2427E+01,
     +-.9857E+00,-.4939E-01, .7060E+00, .1483E+01, .2159E+01, .2745E+01,
     + .9985E-02, .8373E-02, .7431E-02, .6866E-02, .4584E-02, .2952E-02,
     + .3098E-02, .3768E-02, .4013E-02, .3960E-02, .3228E-02, .3203E-02,
     + .1007E-01, .8436E-02, .7368E-02, .6657E-02, .4375E-02, .2617E-02,
     + .2742E-02, .3286E-02, .3192E-02, .2992E-02, .2612E-02, .1968E-02,
     + .1019E-01, .8457E-02, .7264E-02, .6426E-02, .4187E-02, .2365E-02,
     + .2324E-02, .2614E-02, .2736E-02, .2068E-02, .2085E-02, .1005E-02,
     + .1028E-01, .8478E-02, .7138E-02, .6259E-02, .3998E-02, .2156E-02,
     + .1926E-02, .1953E-02, .2250E-02, .1844E-02, .1869E-02,-.6489E-03,
     + .1030E-01, .8478E-02, .7033E-02, .6112E-02, .3852E-02, .1989E-02,
     + .1716E-02, .1763E-02, .1432E-02, .1193E-02, .1306E-02,-.5861E-03,
     + .1042E-01, .8499E-02, .6887E-02, .5987E-02, .3768E-02, .1800E-02,
     + .1549E-02, .1712E-02, .1287E-02, .7389E-03, .7222E-03,-.1130E-02,
     + .8227E-02, .7201E-02, .6866E-02, .5903E-02, .3412E-02, .1591E-02,
     + .1402E-02, .1346E-02, .1041E-02, .8185E-03, .3349E-03,-.4815E-03,
     + .8268E-02, .6992E-02, .7159E-02, .6384E-02, .3286E-02, .1591E-02,
     + .1271E-02, .1202E-02, .9187E-03, .6531E-03,-.4187E-03,-.7954E-03,
     + .8478E-02, .7159E-02, .7117E-02, .6447E-02, .3349E-02, .1528E-02,
     + .9964E-03, .9210E-03, .6112E-03, .6259E-03,-.3768E-03,-.1298E-02,
     + .8520E-02, .7075E-02, .7096E-02, .6405E-02, .3245E-02, .1528E-02,
     + .1011E-02, .7877E-03, .7536E-03, .9001E-04,-.6719E-03,-.1026E-02,
     + .8561E-02, .6950E-02, .7033E-02, .6280E-02, .2993E-02, .1528E-02,
     + .6698E-03, .5847E-03, .2847E-03,-.6280E-04,-.9420E-03,-.1444E-02,
     +-.1408E-04,-.2664E-04,-.1180E-04,-.1903E-04,-.9515E-05, .3806E-06,
     +-.6851E-05,-.3806E-05,-.4834E-05,-.3239E-05,-.2284E-05,-.1028E-04,
     +-.1484E-04,-.2550E-04,-.1142E-04,-.1827E-04,-.9515E-05, .3805E-06,
     +-.4948E-05, .3806E-06,-.2664E-06, .1058E-04,-.1012E-04,-.1142E-04,
     +-.1560E-04,-.2512E-04,-.1256E-04,-.1865E-04,-.9134E-05, .1142E-05,
     +-.3425E-05, .2474E-05,-.9781E-05,-.1519E-05,-.7916E-05,-.1294E-04,
     +-.1560E-04,-.2474E-04,-.1180E-04,-.2017E-04,-.7992E-05, .3805E-06,
     +-.2283E-05,-.4453E-05,-.1180E-05,-.5138E-05,-.4453E-05,-.3425E-05,
     +-.1522E-04,-.2550E-04,-.9896E-05,-.1903E-04,-.9134E-05,-.1142E-05,
     +-.7611E-06,-.5252E-05,-.4567E-06,-.4643E-05,-.4567E-06,-.4567E-05,
     +-.1294E-04,-.2512E-04,-.1028E-04,-.2055E-04,-.9896E-05,-.4567E-05,
     +-.2284E-05,-.5100E-05,-.4339E-06,-.9515E-06,-.1252E-04,-.7612E-06,
     +-.2246E-04,-.1370E-04,-.1066E-04,-.1598E-04,-.8754E-05,-.5328E-05,
     +-.6622E-05,-.5138E-05,-.8754E-07,-.9515E-06, .6090E-05, .4187E-05,
     +-.3463E-04,-.1599E-04,-.1218E-04,-.2093E-04,-.9515E-05,-.4567E-05,
     +-.1104E-05,-.1903E-05,-.1488E-05,-.3730E-05,-.4567E-05, .3045E-05,
     +-.3463E-04,-.1675E-04,-.1294E-04,-.1979E-04,-.1066E-04,-.4187E-05,
     +-.4034E-05,-.2893E-05,-.2588E-05,-.9401E-05, .2284E-05, .3045E-05,
     +-.2778E-04,-.1522E-04,-.1560E-04,-.1751E-04,-.1256E-04,-.5709E-05,
     +-.2474E-05,-.2577E-05,-.2284E-05,-.4187E-06, .7650E-05,-.3425E-05,
     +-.3083E-04,-.1827E-04,-.1370E-04,-.1751E-04,-.1104E-04,-.9515E-05,
     +-.6318E-05,-.4358E-05,-.7613E-07, .4643E-05, .4415E-05, .1028E-04/
      end
      
      block data ckd6
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, eleven 
c pressures,  and  five  cumulative probabilities ( Fu,  1991 ). The
c spectral region is from 2850 to 2500 cm**-1.
c *********************************************************************
      common /band6/ hk(5), coeh2o(3,11,5)
      data hk / .3, .2, .2, .2, .1 /
c   .173978E+01    .115985E+01    .115985E+01    .115985E+01    .579927E+00
      data ( ( ( coeh2o(k,j,i), i = 1, 5 ), j = 1, 11 ), k = 1, 3 ) /
     +-.1905E+02,-.1602E+02,-.1472E+02,-.1307E+02,-.1024E+02,-.1823E+02,
     +-.1555E+02,-.1427E+02,-.1266E+02,-.9938E+01,-.1749E+02,-.1508E+02,
     +-.1381E+02,-.1225E+02,-.9641E+01,-.1684E+02,-.1462E+02,-.1337E+02,
     +-.1185E+02,-.9367E+01,-.1630E+02,-.1417E+02,-.1294E+02,-.1145E+02,
     +-.9123E+01,-.1578E+02,-.1373E+02,-.1251E+02,-.1108E+02,-.8881E+01,
     +-.1517E+02,-.1327E+02,-.1209E+02,-.1072E+02,-.8653E+01,-.1463E+02,
     +-.1284E+02,-.1169E+02,-.1040E+02,-.8453E+01,-.1421E+02,-.1244E+02,
     +-.1133E+02,-.1014E+02,-.8312E+01,-.1382E+02,-.1207E+02,-.1100E+02,
     +-.9887E+01,-.8220E+01,-.1348E+02,-.1173E+02,-.1071E+02,-.9685E+01,
     +-.8220E+01, .1024E-01, .1842E-02, .6908E-03, .1737E-02, .3517E-02,
     + .8394E-02, .2072E-02, .8164E-03, .1716E-02, .2805E-02, .8143E-02,
     + .2240E-02, .9001E-03, .1570E-02, .1800E-02, .8227E-02, .2386E-02,
     + .9420E-03, .1486E-02, .1068E-02, .8373E-02, .2533E-02, .9210E-03,
     + .1319E-02, .9420E-03, .8394E-02, .2700E-02, .9629E-03, .1026E-02,
     + .5233E-03, .8917E-02, .2575E-02, .8792E-03, .7536E-03, .4187E-03,
     + .9378E-02, .2617E-02, .7955E-03, .6070E-03, .4815E-03, .9797E-02,
     + .2638E-02, .6908E-03, .5233E-03, .6280E-03, .1009E-01, .2638E-02,
     + .4815E-03, .2931E-03, .4815E-03, .1036E-01, .2428E-02, .3140E-03,
     + .3977E-03, .2093E-03,-.5366E-04,-.1522E-04,-.5709E-05,-.2664E-05,
     + .3806E-05,-.4301E-04,-.1484E-04,-.4948E-05,-.7610E-06, .7610E-06,
     +-.3920E-04,-.1484E-04,-.4948E-05, .3804E-06,-.3806E-05,-.3920E-04,
     +-.1522E-04,-.4948E-05, .3425E-05, .1903E-05,-.3806E-04,-.1484E-04,
     +-.3045E-05, .2664E-05, .7993E-05,-.4148E-04,-.1408E-04,-.3806E-05,
     + .4187E-05, .7993E-05,-.5481E-04,-.1180E-04,-.3045E-05, .3045E-05,
     + .2284E-05,-.5709E-04,-.1104E-04,-.2283E-05,-.2664E-05,-.1142E-05,
     +-.6090E-04,-.1218E-04,-.2664E-05, .3804E-06, .3045E-05,-.6698E-04,
     +-.1218E-04,-.2664E-05, .1523E-05,-.1142E-05,-.6508E-04,-.1218E-04,
     +-.3425E-05, .1903E-05, .7612E-06 /
      end

      block data ckd7
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  two  cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 2200 to 1900 cm**-1.
c *********************************************************************
      common /band7/ hk(2), coeh2o(3,19,2)
      data hk / 0.7, 0.3 /
      data ( ( ( coeh2o(k,j,i), i = 1, 2 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2008E+02,-.1467E+02,-.2004E+02,-.1426E+02,-.2001E+02,-.1386E+02,
     +-.1998E+02,-.1345E+02,-.1995E+02,-.1304E+02,-.1992E+02,-.1263E+02,
     +-.1989E+02,-.1223E+02,-.1986E+02,-.1183E+02,-.1984E+02,-.1143E+02,
     +-.1758E+02,-.1038E+02,-.1602E+02,-.9480E+01,-.1469E+02,-.8752E+01,
     +-.1349E+02,-.8218E+01,-.1255E+02,-.7677E+01,-.1174E+02,-.7184E+01,
     +-.1110E+02,-.6735E+01,-.1056E+02,-.6332E+01,-.1019E+02,-.5975E+01,
     +-.9874E+01,-.5644E+01, .2533E-02, .2269E-01, .2575E-02, .2263E-01,
     + .2554E-02, .2267E-01, .2491E-02, .2250E-01, .2449E-02, .2244E-01,
     + .2344E-02, .2234E-01, .2219E-02, .2208E-01, .5694E-02, .2190E-01,
     + .9650E-02, .2162E-01, .3286E-01, .1848E-01, .2987E-01, .1578E-01,
     + .2527E-01, .1465E-01, .2175E-01, .1386E-01, .2056E-01, .1235E-01,
     + .1963E-01, .1116E-01, .1926E-01, .1040E-01, .2014E-01, .1040E-01,
     + .2024E-01, .1042E-01, .1972E-01, .1080E-01,-.8754E-05,-.6698E-04,
     +-.1104E-04,-.6432E-04,-.1142E-04,-.6051E-04,-.1180E-04,-.6128E-04,
     +-.1180E-04,-.6242E-04,-.1218E-04,-.6280E-04,-.1218E-04,-.6204E-04,
     + .5328E-04,-.5709E-04, .1275E-03,-.5214E-04,-.1370E-03,-.4148E-04,
     +-.1100E-03,-.3045E-04,-.9248E-04,-.3197E-04,-.7346E-04,-.2436E-04,
     +-.5100E-04,-.2131E-04,-.5861E-04,-.2550E-04,-.5328E-04,-.3311E-04,
     +-.6090E-04,-.4225E-04,-.5443E-04,-.4415E-04,-.4034E-04,-.4339E-04/
      end

      block data ckd8
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  three cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 1900 to 1700 cm**-1.
c *********************************************************************
      common /band8/ hk(3), coeh2o(3,19,3)
       data hk / 0.2, 0.7, 0.1 /
      data ( ( ( coeh2o(k,j,i), i = 1, 3 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2283E+02,-.1639E+02,-.6155E+01,-.2237E+02,-.1595E+02,-.5775E+01,
     +-.2191E+02,-.1551E+02,-.5381E+01,-.2145E+02,-.1507E+02,-.5004E+01,
     +-.2099E+02,-.1463E+02,-.4617E+01,-.2053E+02,-.1419E+02,-.4218E+01,
     +-.2025E+02,-.1375E+02,-.3806E+01,-.2021E+02,-.1330E+02,-.3403E+01,
     +-.2018E+02,-.1287E+02,-.2993E+01,-.1998E+02,-.1091E+02,-.2586E+01,
     +-.1744E+02,-.9171E+01,-.2162E+01,-.1490E+02,-.7642E+01,-.1763E+01,
     +-.1303E+02,-.6526E+01,-.1373E+01,-.1113E+02,-.5846E+01,-.9699E+00,
     +-.9814E+01,-.5280E+01,-.5955E+00,-.8582E+01,-.4787E+01,-.2510E+00,
     +-.8020E+01,-.4350E+01, .2770E-01,-.7571E+01,-.3942E+01, .2406E+00,
     +-.7140E+01,-.3537E+01, .3567E+00, .3722E-01, .1505E-01, .6615E-02,
     + .3722E-01, .1518E-01, .5840E-02, .3720E-01, .1526E-01, .5170E-02,
     + .3399E-01, .1530E-01, .4773E-02, .3012E-01, .1551E-01, .4333E-02,
     + .2625E-01, .1553E-01, .3956E-02, .2240E-01, .1562E-01, .3454E-02,
     + .1846E-01, .1574E-01, .3161E-02, .1446E-01, .1572E-01, .3098E-02,
     + .5924E-02, .8875E-02, .2658E-02, .2204E-01, .7096E-02, .2504E-02,
     + .1591E-01, .5233E-02, .2292E-02, .8855E-02, .4249E-02, .2190E-02,
     + .5422E-02, .3496E-02, .2041E-02, .4919E-02, .3621E-02, .2200E-02,
     + .6657E-02, .3663E-02, .2248E-02, .8645E-02, .3852E-02, .2118E-02,
     + .8771E-02, .3873E-02, .2176E-02, .9043E-02, .3747E-02, .2079E-02,
     +-.1568E-03,-.4681E-04, .4567E-05,-.1568E-03,-.4605E-04,-.3425E-05,
     +-.1572E-03,-.4605E-04,-.1104E-04,-.2154E-03,-.4453E-04,-.6851E-05,
     +-.2843E-03,-.4225E-04,-.7231E-05,-.3562E-03,-.4110E-04,-.7231E-05,
     +-.3692E-03,-.4110E-04,-.1028E-04,-.3007E-03,-.4263E-04,-.6470E-05,
     +-.2325E-03,-.3996E-04,-.8373E-05,-.5290E-04,-.7612E-05,-.4948E-05,
     +-.7422E-04,-.1256E-04,-.8449E-05,-.3501E-04,-.1446E-04,-.4834E-05,
     + .4529E-04,-.2246E-04,-.2893E-05, .6470E-05,-.1789E-04,-.7498E-05,
     +-.4948E-05,-.1713E-04,-.8183E-05,-.5481E-04,-.1713E-04,-.1447E-04,
     +-.4986E-04,-.1903E-04,-.1353E-04,-.5138E-04,-.1484E-04,-.1147E-04,
     +-.5328E-04,-.1560E-04,-.6588E-05/
      end

      block data ckd9
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  four cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 1700 to 1400 cm**-1.
c *********************************************************************
      common /band9/ hk(4), coeh2o(3,19,4)
       data hk / 0.22, 0.51, 0.22, 0.05 /
      data ( ( ( coeh2o(k,j,i), i = 1, 4 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2066E+02,-.1464E+02,-.8301E+01,-.3548E+01,-.2025E+02,-.1419E+02,
     +-.7905E+01,-.3260E+01,-.2019E+02,-.1374E+02,-.7495E+01,-.2927E+01,
     +-.2013E+02,-.1329E+02,-.7078E+01,-.2584E+01,-.2007E+02,-.1284E+02,
     +-.6675E+01,-.2247E+01,-.2001E+02,-.1239E+02,-.6268E+01,-.1890E+01,
     +-.1996E+02,-.1194E+02,-.5853E+01,-.1530E+01,-.1991E+02,-.1150E+02,
     +-.5441E+01,-.1133E+01,-.1987E+02,-.1105E+02,-.5022E+01,-.7447E+00,
     +-.1575E+02,-.9657E+01,-.4191E+01,-.3728E+00,-.1329E+02,-.8133E+01,
     +-.3638E+01, .1616E-01,-.1181E+02,-.6675E+01,-.3178E+01, .4083E+00,
     +-.1036E+02,-.5655E+01,-.2731E+01, .7953E+00,-.8628E+01,-.4990E+01,
     +-.2303E+01, .1153E+01,-.7223E+01,-.4453E+01,-.1877E+01, .1454E+01,
     +-.6567E+01,-.3974E+01,-.1461E+01, .1663E+01,-.6077E+01,-.3551E+01,
     +-.1071E+01, .1800E+01,-.5651E+01,-.3136E+01,-.7005E+00, .1809E+01,
     +-.5241E+01,-.2726E+01,-.3859E+00, .1781E+01, .1315E-01, .4542E-02,
     + .3496E-02, .4877E-02, .9650E-02, .4542E-02, .3098E-02, .3956E-02,
     + .6154E-02, .4626E-02, .2763E-02, .3077E-02, .2658E-02, .4626E-02,
     + .2512E-02, .2261E-02, .2658E-02, .4689E-02, .2219E-02, .1405E-02,
     + .2700E-02, .4752E-02, .1926E-02, .7473E-03, .2658E-02, .4773E-02,
     + .1737E-02, .5066E-03, .4668E-02, .4815E-02, .1507E-02, .1842E-03,
     + .8541E-02, .4794E-02, .1382E-02,-.2156E-03, .1022E-01, .2198E-02,
     + .3977E-03,-.2910E-03, .5484E-02, .6698E-03, .0000E+00,-.2339E-03,
     + .3349E-02, .1068E-02,-.2512E-03,-.4228E-03, .1884E-02, .2093E-03,
     +-.3977E-03,-.6405E-03,-.8373E-04,-.5233E-03,-.4124E-03,-.5945E-03,
     + .7536E-03,-.6698E-03,-.4919E-03,-.4794E-03, .3600E-02,-.4605E-03,
     +-.4375E-03,-.3517E-03, .3873E-02,-.5861E-03,-.3203E-03,-.4689E-03,
     + .3935E-02,-.7326E-03,-.2072E-03,-.4228E-03, .4124E-02,-.8582E-03,
     +-.4187E-04,-.5945E-03,-.8525E-04, .1865E-04,-.1142E-05, .2664E-05,
     +-.1313E-03, .1865E-04, .0000E+00, .1256E-04,-.6470E-04, .1865E-04,
     +-.3045E-05, .8754E-05, .3805E-06, .1789E-04,-.6851E-05, .5328E-05,
     + .1142E-05, .1827E-04,-.6090E-05, .4148E-05, .1142E-05, .1865E-04,
     +-.3806E-05,-.3768E-05,-.1903E-05, .1751E-04,-.4948E-05, .3121E-05,
     + .3159E-04, .1979E-04,-.3045E-05,-.9896E-06, .1005E-03, .1789E-04,
     +-.6089E-05,-.1865E-05,-.2207E-04, .1941E-04, .1903E-05, .2322E-05,
     +-.1675E-04, .6090E-05,-.7611E-06, .4397E-05, .3425E-04, .3806E-06,
     + .1522E-05, .3806E-05, .4796E-04, .1522E-05,-.3806E-06, .3654E-05,
     +-.6851E-05, .2664E-05,-.3920E-05,-.6850E-06,-.1370E-04, .5328E-05,
     +-.6584E-05,-.8716E-05,-.8374E-10, .1522E-05,-.6356E-05, .1294E-05,
     +-.9515E-05, .7612E-06,-.3235E-05,-.1066E-05,-.7612E-05, .1142E-05,
     +-.4529E-05, .3730E-05,-.2664E-05,-.3806E-06,-.3501E-05,-.5328E-06/
      end

      block data ckd10
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  four cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 1400 to 1250 cm**-1. coech4 and coen2o
c are the coefficients to calculate the CH4 and N2O absorption coe-
c fficients in units of (cm-atm)**-1 at three temperature, nineteen
c pressures, and one cumulative probability (Fu, 1991), respectively.
c *********************************************************************
      common /band10/hk(4), coeh2o(3,19,4), coech4(3,19), coen2o(3,19)
       data hk / 0.28, 0.42, 0.25, 0.05 /
      data ( ( ( coeh2o(k,j,i), i = 1, 4 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2023E+02,-.1641E+02,-.1171E+02,-.6090E+01,-.2016E+02,-.1595E+02,
     +-.1133E+02,-.5867E+01,-.2011E+02,-.1550E+02,-.1095E+02,-.5660E+01,
     +-.2005E+02,-.1504E+02,-.1055E+02,-.5407E+01,-.2001E+02,-.1459E+02,
     +-.1015E+02,-.5137E+01,-.1997E+02,-.1413E+02,-.9749E+01,-.4852E+01,
     +-.1993E+02,-.1367E+02,-.9337E+01,-.4534E+01,-.1990E+02,-.1321E+02,
     +-.8920E+01,-.4211E+01,-.1987E+02,-.1276E+02,-.8506E+01,-.3889E+01,
     +-.1645E+02,-.1179E+02,-.7711E+01,-.3613E+01,-.1442E+02,-.1081E+02,
     +-.6942E+01,-.3316E+01,-.1308E+02,-.9950E+01,-.6344E+01,-.2950E+01,
     +-.1212E+02,-.9217E+01,-.5904E+01,-.2577E+01,-.1131E+02,-.8559E+01,
     +-.5519E+01,-.2256E+01,-.1064E+02,-.7962E+01,-.5183E+01,-.1929E+01,
     +-.1013E+02,-.7447E+01,-.4833E+01,-.1643E+01,-.9712E+01,-.7071E+01,
     +-.4485E+01,-.1410E+01,-.9305E+01,-.6760E+01,-.4145E+01,-.1249E+01,
     +-.8966E+01,-.6477E+01,-.3820E+01,-.1114E+01, .7913E-02, .8206E-02,
     + .1509E-01, .1869E-01, .4228E-02, .8247E-02, .1467E-01, .1783E-01,
     + .2010E-02, .8227E-02, .1442E-01, .1687E-01, .1947E-02, .8289E-02,
     + .1394E-01, .1568E-01, .1863E-02, .8289E-02, .1346E-01, .1484E-01,
     + .1842E-02, .8415E-02, .1310E-01, .1400E-01, .1800E-02, .8457E-02,
     + .1275E-01, .1377E-01, .1696E-02, .8478E-02, .1220E-01, .1321E-01,
     + .1842E-02, .8478E-02, .1189E-01, .1250E-01, .1409E-01, .8624E-02,
     + .1254E-01, .1214E-01, .9043E-02, .1045E-01, .1225E-01, .1260E-01,
     + .8561E-02, .1202E-01, .1181E-01, .1296E-01, .1114E-01, .1235E-01,
     + .1191E-01, .1330E-01, .1199E-01, .1271E-01, .1195E-01, .1371E-01,
     + .1415E-01, .1315E-01, .1218E-01, .1361E-01, .1478E-01, .1338E-01,
     + .1296E-01, .1306E-01, .1518E-01, .1375E-01, .1365E-01, .1334E-01,
     + .1530E-01, .1411E-01, .1392E-01, .1327E-01, .1547E-01, .1507E-01,
     + .1390E-01, .1264E-01,-.1089E-03,-.2740E-04,-.2017E-04,-.5519E-04,
     +-.4491E-04,-.2740E-04,-.1408E-04,-.5937E-04,-.6090E-05,-.2702E-04,
     +-.6470E-05,-.4719E-04,-.7232E-05,-.2740E-04,-.6089E-05,-.4910E-04,
     +-.7231E-05,-.2969E-04,-.4186E-05,-.5366E-04,-.6090E-05,-.3045E-04,
     +-.2284E-05,-.4986E-04,-.4568E-05,-.3121E-04,-.4948E-05,-.5100E-04,
     +-.3426E-05,-.3007E-04,-.7993E-05,-.4910E-04, .1522E-05,-.2931E-04,
     +-.9896E-05,-.5366E-04,-.5823E-04,-.1599E-04,-.1713E-04,-.4110E-04,
     +-.3121E-04,-.1713E-04,-.3159E-04,-.3578E-04,-.3996E-04,-.1598E-04,
     +-.3958E-04,-.4605E-04,-.3349E-04,-.1751E-04,-.3844E-04,-.5576E-04,
     +-.2626E-04,-.2474E-04,-.3920E-04,-.4464E-04,-.1979E-04,-.3045E-04,
     +-.3958E-04,-.5336E-04,-.2893E-04,-.3616E-04,-.3996E-04,-.4754E-04,
     +-.2398E-04,-.3083E-04,-.4415E-04,-.5119E-04,-.2702E-04,-.2664E-04,
     +-.4605E-04,-.4038E-04,-.2398E-04,-.2360E-04,-.4948E-04,-.5149E-04/
      data ( ( coech4(k,j), j = 1, 19 ), k = 1, 3 ) /
     +-.8909E+01,-.8464E+01,-.8018E+01,-.7573E+01,-.7133E+01,-.6687E+01,
     +-.6240E+01,-.5803E+01,-.5377E+01,-.4534E+01,-.3983E+01,-.3502E+01,
     +-.3062E+01,-.2648E+01,-.2265E+01,-.1896E+01,-.1568E+01,-.1234E+01,
     +-.9298E+00, .9629E-03, .9838E-03, .1088E-02, .1172E-02, .1256E-02,
     + .1402E-02, .1528E-02, .1633E-02, .1716E-02, .4815E-03,-.3977E-03,
     +-.5652E-03,-.5024E-03,-.4605E-03,-.4563E-03,-.4438E-03,-.4521E-03,
     +-.4312E-03,-.3789E-03,-.1294E-04,-.1408E-04,-.1522E-04,-.1675E-04,
     +-.1751E-04,-.1941E-04,-.2246E-04,-.2207E-04,-.1827E-04,-.1256E-04,
     +-.9515E-05,-.6470E-05,-.3045E-05,-.3806E-05,-.2055E-05,-.3730E-05,
     +-.7612E-06,-.3806E-05, .1256E-05/
      data ( ( coen2o(k,j), j = 1, 19 ), k = 1, 3 ) /
     +-.7863E+01,-.7412E+01,-.6963E+01,-.6514E+01,-.6065E+01,-.5611E+01,
     +-.5167E+01,-.4720E+01,-.4283E+01,-.3454E+01,-.2858E+01,-.2404E+01,
     +-.1922E+01,-.1491E+01,-.1097E+01,-.7177E+00,-.3548E+00, .1218E-01,
     + .3088E+00, .4459E-02, .4542E-02, .4668E-02, .4752E-02, .4815E-02,
     + .4919E-02, .5087E-02, .5254E-02, .5296E-02, .2324E-02, .2093E-02,
     + .2294E-02, .2125E-02, .2058E-02, .1920E-02, .1786E-02, .1689E-02,
     + .1788E-02, .2144E-02,-.7231E-05,-.7231E-05,-.7231E-05,-.6470E-05,
     +-.6851E-05,-.7231E-05,-.5709E-05,-.6470E-05,-.4186E-05, .8754E-05,
     +-.7612E-05,-.9134E-06,-.8640E-05,-.8487E-05,-.8259E-05,-.9553E-05,
     +-.8107E-05,-.1654E-04,-.1858E-04/
      end

      block data ckd11
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and three cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 1250 to 1100 cm**-1. coech4 and coen2o
c are the coefficients to calculate the CH4 and N2O absorption coe-
c fficients in units of (cm-atm)**-1 at three temperature, nineteen
c pressures, and one cumulative probability (Fu, 1991), respectively.
c *********************************************************************
      common /band11/hk(3), coeh2o(3,19,3), coech4(3,19), coen2o(3,19)
       data hk / 0.80, 0.15, 0.05 /
      data ( ( ( coeh2o(k,j,i), i = 1, 3 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2005E+02,-.1548E+02,-.1021E+02,-.2001E+02,-.1504E+02,-.1001E+02,
     +-.1997E+02,-.1459E+02,-.9814E+01,-.1993E+02,-.1416E+02,-.9595E+01,
     +-.1989E+02,-.1373E+02,-.9349E+01,-.1985E+02,-.1328E+02,-.9072E+01,
     +-.1982E+02,-.1286E+02,-.8833E+01,-.1957E+02,-.1243E+02,-.8566E+01,
     +-.1911E+02,-.1200E+02,-.8276E+01,-.1743E+02,-.1134E+02,-.7958E+01,
     +-.1625E+02,-.1078E+02,-.7629E+01,-.1524E+02,-.1036E+02,-.7334E+01,
     +-.1429E+02,-.9970E+01,-.7051E+01,-.1348E+02,-.9620E+01,-.6749E+01,
     +-.1282E+02,-.9270E+01,-.6505E+01,-.1229E+02,-.8932E+01,-.6277E+01,
     +-.1186E+02,-.8628E+01,-.6120E+01,-.1148E+02,-.8345E+01,-.6049E+01,
     +-.1112E+02,-.8066E+01,-.5906E+01, .1842E-02, .2131E-01, .3033E-01,
     + .1905E-02, .2137E-01, .2841E-01, .1926E-02, .2135E-01, .2696E-01,
     + .1926E-02, .2133E-01, .2514E-01, .1884E-02, .2154E-01, .2401E-01,
     + .5589E-02, .2156E-01, .2321E-01, .9483E-02, .2156E-01, .2210E-01,
     + .1333E-01, .2150E-01, .2133E-01, .1725E-01, .2154E-01, .2074E-01,
     + .2254E-01, .1999E-01, .2005E-01, .2118E-01, .1926E-01, .1978E-01,
     + .1936E-01, .1920E-01, .1963E-01, .1905E-01, .1911E-01, .1934E-01,
     + .1909E-01, .1903E-01, .1920E-01, .1922E-01, .1901E-01, .1899E-01,
     + .1934E-01, .1930E-01, .1974E-01, .1966E-01, .1909E-01, .2014E-01,
     + .1976E-01, .1905E-01, .1984E-01, .1963E-01, .1940E-01, .1897E-01,
     +-.1522E-05,-.6013E-04,-.5062E-04,-.2665E-05,-.6204E-04,-.5519E-04,
     +-.3806E-05,-.6394E-04,-.5633E-04,-.4567E-05,-.6280E-04,-.5214E-04,
     +-.6090E-05,-.6128E-04,-.5290E-04, .6051E-04,-.6242E-04,-.5823E-04,
     + .1313E-03,-.6013E-04,-.5176E-04, .1336E-03,-.5747E-04,-.4072E-04,
     + .6318E-04,-.5671E-04,-.3996E-04,-.5595E-04,-.3996E-04,-.4263E-04,
     +-.3958E-04,-.4719E-04,-.4453E-04,-.3387E-04,-.5138E-04,-.5100E-04,
     +-.5252E-04,-.4986E-04,-.4491E-04,-.5100E-04,-.4453E-04,-.4529E-04,
     +-.5176E-04,-.4795E-04,-.4453E-04,-.5557E-04,-.5176E-04,-.5062E-04,
     +-.5747E-04,-.4795E-04,-.5633E-04,-.5709E-04,-.4643E-04,-.3806E-04,
     +-.5481E-04,-.5671E-04,-.4948E-04/
      data ( ( coech4(k,j), j = 1, 19 ), k = 1, 3 ) /
     +-.1207E+02,-.1162E+02,-.1116E+02,-.1070E+02,-.1024E+02,-.9777E+01,
     +-.9319E+01,-.8858E+01,-.8398E+01,-.7384E+01,-.6643E+01,-.6081E+01,
     +-.5602E+01,-.5188E+01,-.4822E+01,-.4479E+01,-.4184E+01,-.3884E+01,
     +-.3627E+01, .1036E-01, .1036E-01, .1040E-01, .1040E-01, .1045E-01,
     + .1047E-01, .1049E-01, .1055E-01, .1059E-01, .1059E-01, .1026E-01,
     + .1011E-01, .1024E-01, .1049E-01, .1072E-01, .1089E-01, .1109E-01,
     + .1153E-01, .1191E-01,-.4910E-04,-.4834E-04,-.4910E-04,-.4910E-04,
     +-.4910E-04,-.4872E-04,-.4834E-04,-.4948E-04,-.5100E-04,-.5633E-04,
     +-.6166E-04,-.5595E-04,-.5366E-04,-.5366E-04,-.5328E-04,-.5328E-04,
     +-.4948E-04,-.5519E-04,-.5595E-04/
      data ( ( coen2o(k,j), j = 1, 19 ), k = 1, 3 ) /
     +-.9461E+01,-.9003E+01,-.8543E+01,-.8084E+01,-.7629E+01,-.7166E+01,
     +-.6707E+01,-.6249E+01,-.5793E+01,-.5312E+01,-.4847E+01,-.4393E+01,
     +-.3974E+01,-.3587E+01,-.3231E+01,-.2885E+01,-.2602E+01,-.2358E+01,
     +-.2108E+01, .4710E-02, .4752E-02, .4773E-02, .4773E-02, .4815E-02,
     + .4877E-02, .4898E-02, .4982E-02, .5066E-02, .5296E-02, .5149E-02,
     + .5129E-02, .5024E-02, .4752E-02, .4501E-02, .4270E-02, .4019E-02,
     + .3646E-02, .2759E-02,-.1484E-04,-.1408E-04,-.1446E-04,-.1446E-04,
     +-.1522E-04,-.1560E-04,-.1522E-04,-.1522E-04,-.1598E-04,-.1484E-04,
     +-.9895E-05,-.1028E-04,-.7612E-05,-.1903E-05, .1903E-05, .0000E+00,
     + .2283E-05, .6166E-05,-.2740E-05/
      end

      block data ckd12
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeo3 is the coefficient to calculate the ozone absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  five cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 1100 to  980 cm**-1.    coeh2o is the
c coefficient to calculate the H2O absorption coefficient in units
c of (cm-atm)**-1 at three temperature, nineteen pressures, and one
c cumulative probability ( Fu, 1991 ).
c *********************************************************************
      common /band12/ hk(5), coeo3(3,19,5), coeh2o(3,19)
      data hk / 0.45, 0.30, 0.2, 0.04, 0.01 /
      data ( ( ( coeo3(k,j,i), i = 1, 5 ), j = 1, 19 ), k = 1, 3 ) /
     +-.6590E+01,-.3912E+01,-.8513E+00, .2731E+01, .5515E+01,-.6157E+01,
     +-.3583E+01,-.7292E+00, .2740E+01, .5508E+01,-.5731E+01,-.3242E+01,
     +-.5800E+00, .2782E+01, .5485E+01,-.5301E+01,-.2901E+01,-.4131E+00,
     + .2805E+01, .5455E+01,-.4879E+01,-.2551E+01,-.2288E+00, .2878E+01,
     + .5416E+01,-.4449E+01,-.2201E+01,-.2228E-01, .3000E+01, .5374E+01,
     +-.4018E+01,-.1843E+01, .2055E+00, .3143E+01, .5342E+01,-.3615E+01,
     +-.1502E+01, .4561E+00, .3288E+01, .5204E+01,-.3228E+01,-.1172E+01,
     + .7099E+00, .3396E+01, .5077E+01,-.2828E+01,-.8499E+00, .9664E+00,
     + .3463E+01, .4893E+01,-.2480E+01,-.5393E+00, .1229E+01, .3493E+01,
     + .4656E+01,-.2181E+01,-.2653E+00, .1504E+01, .3456E+01, .4398E+01,
     +-.1950E+01,-.1469E-01, .1735E+01, .3387E+01, .4115E+01,-.1788E+01,
     + .2517E+00, .1919E+01, .3251E+01, .3832E+01,-.1677E+01, .5027E+00,
     + .2032E+01, .3088E+01, .3581E+01,-.1637E+01, .7373E+00, .2100E+01,
     + .2910E+01, .3364E+01,-.1650E+01, .9383E+00, .2123E+01, .2793E+01,
     + .3150E+01,-.1658E+01, .1091E+01, .2112E+01, .2683E+01, .3021E+01,
     +-.1654E+01, .1163E+01, .2099E+01, .2602E+01, .2871E+01, .9498E-02,
     + .8894E-02, .1161E-01, .8828E-02,-.1669E-02, .9613E-02, .8347E-02,
     + .1053E-01, .8462E-02,-.1612E-02, .9700E-02, .7829E-02, .9101E-02,
     + .7915E-02,-.1439E-02, .9815E-02, .7167E-02, .7981E-02, .7282E-02,
     +-.1094E-02, .9671E-02, .6764E-02, .6930E-02, .5613E-02,-.8347E-03,
     + .9613E-02, .6312E-02, .6225E-02, .4145E-02,-.1295E-02, .9728E-02,
     + .6099E-02, .5293E-02, .2965E-02,-.1756E-02, .9844E-02, .5915E-02,
     + .4496E-02, .1871E-02,-.2044E-02, .9930E-02, .5817E-02, .3509E-02,
     + .1324E-02,-.2044E-02, .9988E-02, .5535E-02, .2711E-02, .6620E-03,
     +-.1813E-02, .1034E-01, .5247E-02, .1926E-02,-.2303E-03,-.1842E-02,
     + .1058E-01, .4795E-02, .1197E-02,-.9498E-03,-.2216E-02, .1084E-01,
     + .4414E-02, .6188E-03,-.1123E-02,-.2303E-02, .1079E-01, .3926E-02,
     + .1756E-03,-.1497E-02,-.2274E-02, .1039E-01, .3425E-02,-.1900E-03,
     +-.1353E-02,-.2389E-02, .9815E-02, .2769E-02,-.6620E-03,-.1756E-02,
     +-.1785E-02, .9818E-02, .2444E-02,-.1016E-02,-.1410E-02,-.1698E-02,
     + .1074E-01, .3218E-02,-.1235E-02,-.1900E-02,-.2533E-02, .1145E-01,
     + .3684E-02,-.1364E-02,-.1353E-02,-.1957E-02,-.4030E-04,-.2375E-04,
     +-.3814E-05,-.4943E-04,-.3166E-04,-.3742E-04,-.1871E-04,-.1137E-04,
     +-.4317E-04,-.2878E-04,-.3526E-04,-.2015E-04,-.1295E-04,-.4821E-04,
     +-.2303E-04,-.3382E-04,-.2087E-04,-.1519E-04,-.2231E-04,-.1871E-04,
     +-.3454E-04,-.2087E-04,-.8109E-05,-.6476E-05,-.1511E-04,-.3454E-04,
     +-.1820E-04,-.1269E-05,-.1439E-04,-.5037E-05,-.4173E-04,-.2598E-04,
     + .6645E-05,-.1943E-04,-.2087E-04,-.3454E-04,-.2267E-04, .2159E-05,
     +-.2231E-04,-.2159E-05,-.2950E-04,-.2080E-04, .2159E-06,-.4317E-05,
     + .1799E-04,-.3670E-04,-.1590E-04,-.4461E-05,-.9354E-05,-.3598E-05,
     +-.3216E-04,-.1475E-04,-.2231E-05,-.1295E-04,-.2878E-05,-.3576E-04,
     +-.7347E-05,-.1022E-04,-.2159E-05,-.7915E-05,-.3015E-04,-.5230E-05,
     +-.5109E-05,-.6476E-05,-.7196E-05,-.2331E-04,-.1079E-04,-.4102E-05,
     + .1439E-05,-.1223E-04,-.2216E-04,-.1094E-04,-.5325E-05,-.7196E-06,
     +-.1655E-04,-.1036E-04,-.7627E-05,-.2878E-05, .5037E-05,-.1295E-04,
     + .1029E-04,-.1346E-04,-.4821E-05,-.7915E-05, .7915E-05, .2835E-04,
     +-.2893E-04,-.1367E-05,-.7196E-05,-.1871E-04, .3965E-04,-.3310E-04,
     +-.3310E-05,-.7195E-06, .2303E-04/
      data ( ( coeh2o(k,j), j = 1, 19 ), k = 1, 3 ) /
     +-.1984E+02,-.1983E+02,-.1982E+02,-.1981E+02,-.1963E+02,-.1917E+02,
     +-.1871E+02,-.1825E+02,-.1779E+02,-.1639E+02,-.1545E+02,-.1484E+02,
     +-.1433E+02,-.1387E+02,-.1345E+02,-.1305E+02,-.1268E+02,-.1231E+02,
     +-.1196E+02, .6071E-03, .2072E-02, .6196E-02, .1030E-01, .1436E-01,
     + .1846E-01, .2259E-01, .2667E-01, .2993E-01, .2878E-01, .2803E-01,
     + .2851E-01, .2864E-01, .2874E-01, .2862E-01, .2859E-01, .2853E-01,
     + .2868E-01, .2887E-01,-.3808E-06, .2474E-04, .9895E-04, .1728E-03,
     + .1911E-03, .1165E-03, .4225E-04,-.3121E-04,-.8982E-04,-.9553E-04,
     +-.9705E-04,-.9591E-04,-.9287E-04,-.9172E-04,-.9096E-04,-.9134E-04,
     +-.9248E-04,-.1050E-03,-.1031E-03/
      end

      block data ckd13
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  two  cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 980 to 800 cm**-1.
c *********************************************************************
      common /band13/ hk(2), coeh2o(3,19,2)
       data hk / 0.95, 0.05 /
      data ( ( ( coeh2o(k,j,i), i = 1, 2 ), j = 1, 19 ), k = 1, 3 ) /
     +-.1992E+02,-.1446E+02,-.1992E+02,-.1405E+02,-.1991E+02,-.1363E+02,
     +-.1990E+02,-.1322E+02,-.1989E+02,-.1282E+02,-.1989E+02,-.1242E+02,
     +-.1988E+02,-.1201E+02,-.1987E+02,-.1159E+02,-.1986E+02,-.1119E+02,
     +-.1982E+02,-.1079E+02,-.1817E+02,-.1039E+02,-.1659E+02,-.1000E+02,
     +-.1537E+02,-.9623E+01,-.1460E+02,-.9266E+01,-.1406E+02,-.8959E+01,
     +-.1354E+02,-.8676E+01,-.1309E+02,-.8411E+01,-.1267E+02,-.8232E+01,
     +-.1229E+02,-.8094E+01, .5024E-03, .3199E-01, .5652E-03, .3199E-01,
     + .6071E-03, .3211E-01, .6489E-03, .3199E-01, .6699E-03, .3178E-01,
     + .6908E-03, .3157E-01, .6908E-03, .3109E-01, .6698E-03, .3075E-01,
     + .6698E-03, .3054E-01, .1474E-01, .3000E-01, .3085E-01, .2960E-01,
     + .3659E-01, .2935E-01, .3016E-01, .2920E-01, .2834E-01, .2895E-01,
     + .2780E-01, .2870E-01, .2753E-01, .2843E-01, .2755E-01, .2820E-01,
     + .2765E-01, .2732E-01, .2769E-01, .2705E-01, .6299E-09,-.7993E-04,
     +-.3802E-06,-.7992E-04,-.3802E-06,-.8525E-04,-.3808E-06,-.8449E-04,
     +-.7610E-06,-.7764E-04,-.1142E-05,-.7231E-04,-.1142E-05,-.7345E-04,
     +-.2284E-05,-.8259E-04,-.2284E-05,-.8031E-04, .2436E-03,-.7878E-04,
     + .7612E-05,-.8525E-04,-.1248E-03,-.9439E-04,-.9477E-04,-.9172E-04,
     +-.8982E-04,-.8640E-04,-.7916E-04,-.6813E-04,-.7574E-04,-.6090E-04,
     +-.7612E-04,-.7117E-04,-.7498E-04,-.7041E-04,-.7269E-04,-.7992E-04/
      end

      block data ckd14
c **********************************************************************
c hk is the interval in the g (cumulative probability) space from 0
c to one. coehca and coehcb are the coefficients to calculate the
c H2O and CO2 overlapping absorption coefficients in units of (cm-
c atm)**-1 at three temperature, nineteen pressures, and ten cumu-
c lative probabilities (Fu, 1991). The spectral region is from 800
c to 670 cm**-1.
c **********************************************************************
      common /band14/ hk(10), coehca(3,19,10), coehcb(3,19,10)
       data hk / .3,.3,.2,.12,.06,.012,.004,.0025,.0011,.0004 /
      data ( ( ( coehca(k,j,i), i = 1, 10 ), j = 1, 19 ), k = 1, 3 ) /
     +-.1847E+02,-.1399E+02,-.1106E+02,-.8539E+01,-.5852E+01,-.3295E+01,
     +-.1208E+01,-.6272E-01, .2055E+01, .6071E+01,-.1801E+02,-.1357E+02,
     +-.1067E+02,-.8171E+01,-.5562E+01,-.3071E+01,-.1073E+01, .1033E+00,
     + .2055E+01, .6071E+01,-.1755E+02,-.1314E+02,-.1027E+02,-.7798E+01,
     +-.5224E+01,-.2823E+01,-.9280E+00, .2723E+00, .2165E+01, .5969E+01,
     +-.1709E+02,-.1272E+02,-.9868E+01,-.7404E+01,-.4880E+01,-.2569E+01,
     +-.6908E+00, .4453E+00, .2241E+01, .5969E+01,-.1663E+02,-.1230E+02,
     +-.9467E+01,-.7013E+01,-.4535E+01,-.2297E+01,-.4408E+00, .6353E+00,
     + .2359E+01, .5969E+01,-.1617E+02,-.1188E+02,-.9050E+01,-.6619E+01,
     +-.4160E+01,-.1967E+01,-.1687E+00, .8213E+00, .2421E+01, .5969E+01,
     +-.1571E+02,-.1147E+02,-.8629E+01,-.6230E+01,-.3771E+01,-.1648E+01,
     + .1573E+00, .1019E+01, .2511E+01, .5884E+01,-.1525E+02,-.1106E+02,
     +-.8215E+01,-.5841E+01,-.3393E+01,-.1331E+01, .4013E+00, .1198E+01,
     + .2654E+01, .5794E+01,-.1480E+02,-.1066E+02,-.7800E+01,-.5454E+01,
     +-.3032E+01,-.9870E+00, .6323E+00, .1373E+01, .2905E+01, .5647E+01,
     +-.1402E+02,-.9693E+01,-.7206E+01,-.4846E+01,-.2656E+01,-.6540E+00,
     + .8323E+00, .1530E+01, .3211E+01, .5355E+01,-.1343E+02,-.9060E+01,
     +-.6596E+01,-.4399E+01,-.2294E+01,-.3519E+00, .9823E+00, .1673E+01,
     + .3420E+01, .5083E+01,-.1279E+02,-.8611E+01,-.5785E+01,-.4010E+01,
     +-.1936E+01,-.1177E+00, .1134E+01, .1974E+01, .3591E+01, .4770E+01,
     +-.1230E+02,-.8174E+01,-.5298E+01,-.3611E+01,-.1607E+01, .3636E-01,
     + .1433E+01, .2260E+01, .3539E+01, .4439E+01,-.1192E+02,-.7763E+01,
     +-.4946E+01,-.3228E+01,-.1321E+01, .1991E+00, .1720E+01, .2420E+01,
     + .3383E+01, .4041E+01,-.1154E+02,-.7377E+01,-.4576E+01,-.2851E+01,
     +-.1093E+01, .4430E+00, .1896E+01, .2462E+01, .3122E+01, .3620E+01,
     +-.1118E+02,-.7003E+01,-.4210E+01,-.2524E+01,-.8973E+00, .7490E+00,
     + .1966E+01, .2363E+01, .2818E+01, .3182E+01,-.1080E+02,-.6677E+01,
     +-.3872E+01,-.2264E+01,-.6846E+00, .9392E+00, .1867E+01, .2138E+01,
     + .2505E+01, .2738E+01,-.1031E+02,-.6353E+01,-.3596E+01,-.1938E+01,
     +-.4537E+00, .1015E+01, .1659E+01, .1830E+01, .2142E+01, .2287E+01,
     +-.9695E+01,-.5977E+01,-.3427E+01,-.1596E+01,-.1979E+00, .9458E+00,
     + .1363E+01, .1545E+01, .1743E+01, .1832E+01, .3628E-01, .2728E-01,
     + .2213E-01, .1656E-01, .1507E-01, .1564E-01, .1623E-01, .1419E-01,
     + .1455E-01, .1089E-02, .3632E-01, .2740E-01, .2164E-01, .1606E-01,
     + .1369E-01, .1418E-01, .1444E-01, .1275E-01, .1331E-01, .9210E-03,
     + .3636E-01, .2746E-01, .2114E-01, .1557E-01, .1239E-01, .1285E-01,
     + .1237E-01, .1141E-01, .1141E-01, .9210E-03, .3640E-01, .2748E-01,
     + .2064E-01, .1516E-01, .1141E-01, .1125E-01, .1092E-01, .1026E-01,
     + .1011E-01,-.5652E-03, .3646E-01, .2746E-01, .2024E-01, .1478E-01,
     + .1036E-01, .9688E-02, .9610E-02, .9305E-02, .9399E-02,-.6489E-03,
     + .3651E-01, .2734E-01, .1984E-01, .1438E-01, .9436E-02, .8486E-02,
     + .8214E-02, .8995E-02, .7892E-02,-.8582E-03, .3655E-01, .2723E-01,
     + .1951E-01, .1402E-01, .8716E-02, .7433E-02, .7169E-02, .8072E-02,
     + .5443E-02,-.1172E-02, .3659E-01, .2709E-01, .1911E-01, .1379E-01,
     + .8107E-02, .6818E-02, .6818E-02, .7033E-02, .3056E-02,-.1047E-02,
     + .3670E-01, .2698E-01, .1890E-01, .1363E-01, .7502E-02, .6371E-02,
     + .6558E-02, .6489E-02,-.5652E-03,-.1340E-02, .3592E-01, .2238E-01,
     + .1804E-01, .1007E-01, .6730E-02, .5512E-02, .6194E-02, .4375E-02,
     +-.1109E-02,-.3559E-03, .3609E-01, .2242E-01, .1526E-01, .8582E-02,
     + .6284E-02, .5809E-02, .4501E-02, .9420E-03,-.9001E-03,-.1005E-02,
     + .3703E-01, .2196E-01, .1281E-01, .7860E-02, .5861E-02, .5842E-02,
     + .1800E-02,-.1591E-02,-.1235E-02,-.9420E-03, .3728E-01, .2114E-01,
     + .1347E-01, .6678E-02, .5449E-02, .4837E-02,-.1084E-02,-.1361E-02,
     +-.6699E-03,-.1256E-03, .3683E-01, .2061E-01, .1350E-01, .6133E-02,
     + .5449E-02, .2111E-02,-.1386E-02,-.1235E-02,-.5652E-03,-.8373E-04,
     + .3656E-01, .1988E-01, .1348E-01, .5441E-02, .5149E-02,-.8813E-03,
     +-.1116E-02,-.8373E-03,-.3140E-03,-.6280E-04, .3669E-01, .1934E-01,
     + .1363E-01, .5035E-02, .3585E-02,-.1250E-02,-.9357E-03,-.8227E-03,
     +-.3140E-03,-.4187E-04, .3618E-01, .1856E-01, .1390E-01, .3836E-02,
     + .1470E-02,-.1096E-02,-.8080E-03,-.4480E-03,-.2093E-03,-.2093E-04,
     + .3416E-01, .1741E-01, .1431E-01, .1951E-02,-.2923E-04,-.9422E-03,
     +-.4576E-03,-.2395E-03,-.1565E-03,-.2799E-04, .3219E-01, .1674E-01,
     + .1516E-01, .6652E-03,-.5051E-03,-.7052E-03,-.2002E-03,-.2135E-03,
     +-.7633E-04,-.7300E-04,-.1290E-03,-.9934E-04,-.5595E-04,-.3996E-04,
     + .1294E-04,-.9134E-05, .1294E-05,-.3121E-05,-.4757E-04,-.1979E-04,
     +-.1305E-03,-.9629E-04,-.5481E-04,-.4301E-04, .1827E-04,-.9363E-05,
     + .1777E-04,-.2185E-04,-.1903E-04,-.1675E-04,-.1313E-03,-.9439E-04,
     +-.5404E-04,-.4263E-04, .9134E-05,-.1020E-04, .3524E-04,-.2599E-04,
     +-.2093E-04, .1675E-04,-.1313E-03,-.9172E-04,-.5252E-04,-.4567E-04,
     + .4186E-05,-.3920E-05, .2552E-04,-.2059E-04,-.2246E-04,-.1028E-04,
     +-.1324E-03,-.9210E-04,-.5138E-04,-.4491E-04, .6470E-05,-.2131E-05,
     + .1496E-04,-.1572E-04,-.3311E-04,-.8754E-05,-.1324E-03,-.9058E-04,
     +-.5328E-04,-.4225E-04, .1827E-05,-.8411E-06, .4719E-05,-.6813E-05,
     +-.2474E-04,-.1256E-04,-.1340E-03,-.8868E-04,-.5633E-04,-.4187E-04,
     +-.4415E-05, .6055E-05,-.1648E-04,-.1507E-04, .1979E-04,-.2131E-04,
     +-.1340E-03,-.8373E-04,-.5899E-04,-.3920E-04,-.4072E-05, .1491E-04,
     +-.9781E-05,-.5328E-05, .3578E-04,-.1979E-04,-.1321E-03,-.7954E-04,
     +-.5899E-04,-.4072E-04, .1066E-05, .5728E-05,-.5138E-05,-.8373E-05,
     + .2626E-04,-.2436E-04,-.1363E-03,-.6432E-04,-.5176E-04,-.3083E-04,
     + .2169E-05,-.8944E-05, .3159E-05, .6470E-05,-.4187E-05, .4948E-05,
     +-.1302E-03,-.7802E-04,-.3311E-04,-.1903E-04, .5328E-05,-.1884E-04,
     + .1408E-04, .3311E-04, .1142E-05,-.7613E-06,-.1473E-03,-.6737E-04,
     +-.7536E-04,-.1085E-04,-.1903E-05,-.1458E-04, .4034E-04,-.3941E-10,
     +-.7992E-05, .2664E-05,-.1361E-03,-.5709E-04,-.8550E-04,-.5709E-05,
     +-.8640E-05, .6523E-05, .1903E-05,-.8221E-05,-.3045E-05,-.9134E-05,
     +-.1329E-03,-.5529E-04,-.7107E-04, .2664E-05,-.9020E-05, .3320E-04,
     +-.2131E-05,-.4187E-05,-.7231E-05,-.3806E-05,-.1278E-03,-.5247E-04,
     +-.6465E-04, .3806E-05,-.6091E-05, .1245E-04,-.3844E-05,-.6090E-05,
     +-.8754E-05,-.2664E-05,-.1321E-03,-.5632E-04,-.5897E-04, .1012E-04,
     + .1168E-04,-.4196E-06,-.8411E-05,-.8868E-05,-.1484E-04,-.1522E-05,
     +-.1252E-03,-.4907E-04,-.5932E-04, .3245E-04, .1996E-04,-.3325E-05,
     +-.5785E-05,-.6394E-05,-.6851E-05,-.1142E-05,-.1093E-03,-.4731E-04,
     +-.6761E-04, .1808E-04, .1754E-04,-.5079E-05,-.5809E-05,-.5649E-05,
     +-.3988E-05,-.5849E-06,-.1151E-03,-.4965E-04,-.7163E-04, .7839E-05,
     + .5505E-05,-.6084E-05,-.3344E-05,-.3894E-05,-.1391E-05,-.1327E-05/
      data ( ( ( coehcb(k,j,i), i = 1, 10 ), j = 1, 19 ), k = 1, 3 ) /
     +-.9398E+01,-.5678E+01,-.3606E+01,-.2192E+01, .2104E+01, .3044E+01,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.9094E+01,-.5422E+01,
     +-.3448E+01,-.1650E+01, .2046E+01, .2749E+01,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.8760E+01,-.5270E+01,-.3329E+01,-.1147E+01,
     + .2112E+01, .2709E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.8537E+01,-.5152E+01,-.3129E+01,-.9544E+00, .2254E+01, .2771E+01,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.8176E+01,-.4936E+01,
     +-.2680E+01,-.9259E+00, .2247E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.7836E+01,-.4676E+01,-.2378E+01,-.3550E+00,
     + .1396E+01, .1976E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.7419E+01,-.4122E+01,-.2407E+01,-.1204E-01, .1744E+01,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.7124E+01,-.3727E+01,
     +-.2160E+01, .6158E+00, .1953E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.6823E+01,-.3324E+01,-.1748E+01,-.9806E-01,
     + .2319E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.5957E+01,-.3017E+01,-.1647E+01, .1398E+01,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.5115E+01,-.2290E+01,
     +-.5273E+00, .5662E+00, .1459E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.4162E+01,-.1453E+01, .1116E+00,-.4587E+02,
     + .9569E+00,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.3611E+01,-.9744E+00,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.3075E+01,-.4176E+00,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.3469E+01,-.9395E+00, .5092E+00, .6200E+00,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.3808E+01,-.1505E+01, .3901E+00, .6264E+00,-.1155E+01,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4058E+01,-.1818E+01,
     + .2693E+00, .7087E+00, .3820E+00,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.4587E+02,-.4587E+02,-.4262E+01,-.2097E+01,-.5711E-01, .5681E+00,
     + .1310E+01, .7371E+00,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.3997E+01,-.1784E+01, .4388E-01, .5167E+00, .6930E+00,-.6906E+00,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02, .2944E-01, .2723E-01,
     + .1854E-01, .2023E-01, .2254E-01, .3059E-02, .4788E+00, .3059E-02,
     + .3059E-02, .3059E-02, .3080E-01, .2549E-01, .1547E-01, .2225E-01,
     + .2107E-01, .3059E-02, .4737E+00, .3059E-02, .3059E-02, .3059E-02,
     + .3269E-01, .2656E-01, .2125E-01, .2179E-01, .2162E-01, .4589E+00,
     + .4643E+00, .3059E-02, .3059E-02, .3059E-02, .3322E-01, .2476E-01,
     + .2075E-01, .2139E-01, .1907E-01, .4501E+00, .4441E+00, .3059E-02,
     + .3059E-02, .3059E-02, .3387E-01, .2182E-01, .2665E-01, .1841E-01,
     + .2506E-01, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3532E-01, .2091E-01, .1995E-01, .2067E-01, .1949E-01, .4491E+00,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3468E-01, .2075E-01,
     + .2587E-01, .1401E-01, .8646E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3059E-02, .3059E-02, .3666E-01, .2430E-01, .1919E-01, .2007E-01,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3613E-01, .2147E-01, .1892E-01, .1361E-01, .3059E-02, .4506E+00,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3129E-01, .1954E-01,
     + .2442E-01, .1011E-01, .4420E+00, .3059E-02, .3059E-02, .3059E-02,
     + .3059E-02, .3059E-02, .3177E-01, .2101E-01, .1526E-01, .4376E+00,
     + .4379E+00, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .2887E-01, .2044E-01, .1285E-01, .3059E-02,-.4862E-03, .3059E-02,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .2759E-01, .2114E-01,
     + .4303E+00, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3059E-02, .3059E-02, .2880E-01, .1690E-01,-.4187E+00, .3059E-02,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .2852E-01, .2255E-01, .2184E-01, .4334E+00, .4217E+00, .3059E-02,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .2840E-01, .2136E-01,
     + .1644E-01, .2812E-01, .4358E+00, .4288E+00, .3059E-02, .3059E-02,
     + .3059E-02, .3059E-02, .2809E-01, .2173E-01, .1708E-01, .3346E-01,
     + .4225E-01, .4419E+00, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .2702E-01, .2260E-01, .1607E-01, .2720E-01, .3982E-01, .4452E+00,
     + .4365E+00, .4345E+00, .4432E+00, .4623E+00, .2684E-01, .2328E-01,
     + .2099E-01, .3040E-01, .3867E-01, .4389E+00, .3132E-01, .3158E-01,
     + .4083E-01, .4580E+00,-.1581E-03,-.9707E-04,-.1250E-03, .2580E-03,
     + .7378E-04,-.1617E-01, .8646E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.1319E-03,-.9528E-04,-.1710E-03, .7118E-04, .2076E-04,-.1608E-01,
     + .8552E-02,-.4656E-05,-.4656E-05,-.4656E-05,-.1721E-03,-.4680E-04,
     +-.5522E-04,-.6242E-04, .4517E-04,-.7777E-02, .8382E-02,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.1482E-03,-.4208E-04,-.5216E-04,-.6514E-04,
     +-.8378E-04,-.7956E-02, .8013E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.1501E-03,-.4002E-04,-.1664E-03, .2272E-04,-.1888E-03,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.1201E-03,-.4709E-04,
     +-.5371E-04,-.1574E-03, .1854E-03,-.7712E-02,-.4656E-05,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.1333E-03,-.1062E-03, .5785E-04,-.4150E-04,
     +-.5717E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.1212E-03,-.8524E-04,-.5895E-04,-.2884E-03,-.1581E-01,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.8148E-04,-.9361E-04,
     +-.2873E-03, .1883E-03,-.1594E-01, .8133E-02,-.4656E-05,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.1221E-03,-.1430E-04, .6335E-04,-.2581E-03,
     + .7977E-02,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.9257E-04,-.5008E-04, .6389E-04,-.7455E-02,-.7745E-02,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.1186E-03,-.9037E-04,
     +-.7461E-04,-.4656E-05, .1168E-03,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.8513E-04,-.5708E-04, .7763E-02,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.1124E-03,-.1228E-03, .7663E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.1015E-03,-.8369E-04,
     +-.2167E-03,-.7548E-02, .7608E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.4656E-05,-.4656E-05,-.1049E-03,-.6414E-04,-.1384E-03,-.1644E-03,
     +-.6919E-02, .7736E-02,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.1008E-03,-.7047E-04,-.1276E-03,-.2445E-03,-.1860E-03, .7975E-02,
     +-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.9629E-04,-.1007E-03,
     +-.1127E-03,-.1527E-03,-.3238E-03,-.7373E-02, .7877E-02, .7840E-02,
     + .7997E-02, .8345E-02,-.8800E-04,-.1072E-03,-.1046E-03,-.1777E-03,
     +-.2146E-03,-.7016E-02, .1516E-01, .1532E-01, .1509E-01, .8268E-02/
      end

      block data ckd15
c **********************************************************************
c hk is the interval in the g (cumulative probability) space from 0
c to one. coehca and coehcb are the coefficients to calculate the
c H2O and CO2 overlapping absorption coefficients in units of (cm-
c atm)**-1 at three temperatures, nineteen pressures, and 12 cumu-
c lative probabilities (Fu, 1991). The spectral region is from 670
c to 540 cm**-1.
c **********************************************************************
      common /band15/ hk(12), coehca(3,19,12), coehcb(3,19,12)
       data hk /.24,.36,.18,.1,.05,.02,.016,.012,.01,.006,.0039,.0021/
      data ( ( ( coehca(k,j,i), i = 1, 12 ), j = 1, 19 ), k = 1, 2 ) /
     +-.1921E+02,-.1363E+02,-.1080E+02,-.8392E+01,-.6776E+01,-.5696E+01,
     +-.4572E+01,-.3752E+01,-.2382E+01,-.1110E+01, .6803E+00, .3259E+01,
     +-.1875E+02,-.1321E+02,-.1040E+02,-.8026E+01,-.6449E+01,-.5401E+01,
     +-.4316E+01,-.3498E+01,-.2141E+01,-.9439E+00, .8103E+00, .3314E+01,
     +-.1829E+02,-.1278E+02,-.1000E+02,-.7646E+01,-.6089E+01,-.5085E+01,
     +-.4047E+01,-.3217E+01,-.1872E+01,-.7106E+00, .9573E+00, .3390E+01,
     +-.1783E+02,-.1236E+02,-.9596E+01,-.7264E+01,-.5735E+01,-.4740E+01,
     +-.3743E+01,-.2882E+01,-.1587E+01,-.4714E+00, .1120E+01, .3425E+01,
     +-.1737E+02,-.1195E+02,-.9193E+01,-.6877E+01,-.5371E+01,-.4404E+01,
     +-.3405E+01,-.2574E+01,-.1298E+01,-.1747E+00, .1327E+01, .3547E+01,
     +-.1691E+02,-.1153E+02,-.8776E+01,-.6490E+01,-.4993E+01,-.4049E+01,
     +-.3039E+01,-.2256E+01,-.1012E+01, .1103E+00, .1530E+01, .3651E+01,
     +-.1644E+02,-.1112E+02,-.8360E+01,-.6105E+01,-.4623E+01,-.3688E+01,
     +-.2694E+01,-.1915E+01,-.6855E+00, .3993E+00, .1714E+01, .3950E+01,
     +-.1598E+02,-.1073E+02,-.7943E+01,-.5723E+01,-.4236E+01,-.3314E+01,
     +-.2338E+01,-.1596E+01,-.3583E+00, .6963E+00, .1868E+01, .4127E+01,
     +-.1553E+02,-.1034E+02,-.7542E+01,-.5357E+01,-.3856E+01,-.2942E+01,
     +-.1986E+01,-.1299E+01,-.5472E-01, .9443E+00, .2149E+01, .4261E+01,
     +-.1485E+02,-.9661E+01,-.7008E+01,-.4830E+01,-.3458E+01,-.2566E+01,
     +-.1658E+01,-.9639E+00, .2083E+00, .1182E+01, .2458E+01, .4452E+01,
     +-.1427E+02,-.9166E+01,-.6373E+01,-.4404E+01,-.3073E+01,-.2209E+01,
     +-.1349E+01,-.6648E+00, .4023E+00, .1452E+01, .2739E+01, .4466E+01,
     +-.1380E+02,-.8726E+01,-.5772E+01,-.3982E+01,-.2732E+01,-.1874E+01,
     +-.1052E+01,-.4403E+00, .5763E+00, .1792E+01, .2999E+01, .4335E+01,
     +-.1305E+02,-.8270E+01,-.5304E+01,-.3586E+01,-.2392E+01,-.1568E+01,
     +-.8299E+00,-.2650E+00, .8584E+00, .2062E+01, .3141E+01, .4168E+01,
     +-.1269E+02,-.7900E+01,-.4956E+01,-.3205E+01,-.2065E+01,-.1332E+01,
     +-.6415E+00,-.7921E-01, .1170E+01, .2269E+01, .3198E+01, .4066E+01,
     +-.1227E+02,-.7536E+01,-.4576E+01,-.2859E+01,-.1815E+01,-.1139E+01,
     +-.4520E+00, .2272E+00, .1371E+01, .2351E+01, .3150E+01, .3935E+01,
     +-.1186E+02,-.7159E+01,-.4223E+01,-.2538E+01,-.1619E+01,-.9324E+00,
     +-.1566E+00, .5151E+00, .1520E+01, .2339E+01, .3132E+01, .3880E+01,
     +-.1120E+02,-.6777E+01,-.3919E+01,-.2330E+01,-.1387E+01,-.6737E+00,
     + .1108E+00, .6991E+00, .1531E+01, .2163E+01, .3150E+01, .3767E+01,
     +-.9973E+01,-.6279E+01,-.3638E+01,-.2048E+01,-.1098E+01,-.4407E+00,
     + .3043E+00, .7797E+00, .1424E+01, .2002E+01, .3122E+01, .3611E+01,
     +-.8483E+01,-.5607E+01,-.3357E+01,-.1744E+01,-.8884E+00,-.2264E+00,
     + .3800E+00, .7504E+00, .1245E+01, .2032E+01, .3097E+01, .3546E+01,
     + .3762E-01, .2372E-01, .1643E-01, .1208E-01, .1170E-01, .1164E-01,
     + .1214E-01, .1161E-01, .1028E-01, .9185E-02, .7712E-02, .1001E-01,
     + .3762E-01, .2382E-01, .1593E-01, .1145E-01, .1059E-01, .1049E-01,
     + .1080E-01, .1057E-01, .8894E-02, .7807E-02, .7132E-02, .1032E-01,
     + .3764E-01, .2386E-01, .1555E-01, .1080E-01, .9692E-02, .9231E-02,
     + .9585E-02, .9644E-02, .7711E-02, .6443E-02, .6223E-02, .9922E-02,
     + .3764E-01, .2395E-01, .1516E-01, .1028E-01, .8917E-02, .8415E-02,
     + .8457E-02, .8777E-02, .6436E-02, .5428E-02, .5499E-02, .8017E-02,
     + .3768E-01, .2399E-01, .1482E-01, .9692E-02, .8247E-02, .7640E-02,
     + .7582E-02, .7783E-02, .5432E-02, .4482E-02, .4919E-02, .5903E-02,
     + .3770E-01, .2401E-01, .1449E-01, .9252E-02, .7620E-02, .6678E-02,
     + .6845E-02, .6925E-02, .4939E-02, .3471E-02, .4124E-02, .3873E-02,
     + .3776E-01, .2395E-01, .1419E-01, .8959E-02, .7096E-02, .6184E-02,
     + .6110E-02, .6075E-02, .4419E-02, .2891E-02, .3056E-02, .1214E-02,
     + .3780E-01, .2391E-01, .1392E-01, .8687E-02, .6573E-02, .5733E-02,
     + .5359E-02, .5009E-02, .4034E-02, .2755E-02, .1968E-02,-.4187E-04,
     + .3791E-01, .2382E-01, .1373E-01, .8561E-02, .6060E-02, .5120E-02,
     + .4618E-02, .4713E-02, .3965E-02, .2481E-02, .8164E-03,-.1088E-02,
     + .3843E-01, .2148E-01, .1302E-01, .6384E-02, .5256E-02, .4260E-02,
     + .4077E-02, .4181E-02, .4132E-02, .2135E-02,-.2931E-03,-.1151E-02,
     + .3896E-01, .2081E-01, .1097E-01, .5568E-02, .4475E-02, .3795E-02,
     + .3828E-02, .3996E-02, .3766E-02, .1193E-02,-.1089E-02,-.9420E-03,
     + .3973E-01, .2024E-01, .9943E-02, .4815E-02, .3820E-02, .3663E-02,
     + .3568E-02, .3881E-02, .2859E-02, .6698E-03,-.1549E-02,-.6280E-03,
     + .3635E-01, .1963E-01, .1061E-01, .3812E-02, .3509E-02, .3429E-02,
     + .3693E-02, .3316E-02, .1120E-02, .6552E-03,-.1193E-02,-.1109E-02,
     + .3631E-01, .1893E-01, .1056E-01, .3172E-02, .3378E-02, .3164E-02,
     + .2751E-02, .1722E-02, .1112E-02, .4354E-03,-.7327E-03,-.1319E-02,
     + .3500E-01, .1828E-01, .1050E-01, .2831E-02, .2784E-02, .2564E-02,
     + .1469E-02, .7739E-03, .1209E-02, .7913E-03,-.2512E-03,-.1758E-02,
     + .3352E-01, .1763E-01, .1045E-01, .2401E-02, .1928E-02, .1340E-02,
     + .3753E-03, .5794E-03, .9060E-03, .1042E-02, .1465E-03,-.2533E-02,
     + .2880E-01, .1729E-01, .1077E-01, .1347E-02, .1194E-02,-.1191E-03,
     + .2828E-03, .6606E-03, .9743E-03, .1002E-02, .0000E+00,-.3140E-02,
     + .2040E-01, .1585E-01, .1165E-01, .3871E-05, .1509E-04,-.1046E-02,
     + .2444E-03, .4359E-03, .1041E-02, .2429E-02,-.1721E-03,-.2786E-02,
     + .1737E-01, .1560E-01, .1240E-01,-.2139E-03,-.1025E-02,-.1248E-02,
     +-.6934E-04, .1649E-03, .4062E-03, .1554E-02,-.4179E-03,-.7795E-03/
      data ( ( ( coehca(k,j,i), i = 1, 12 ), j = 1, 19 ), k = 3, 3 ) /
     +-.1488E-03,-.9248E-04,-.2322E-04,-.4187E-05, .1104E-04, .9895E-05,
     +-.2283E-05, .2512E-05,-.9058E-05, .8449E-05, .8297E-05,-.3882E-04,
     +-.1488E-03,-.9058E-04,-.2398E-04,-.5709E-05, .1218E-04, .1180E-04,
     + .1522E-05, .6927E-05,-.1161E-04, .1714E-04,-.4948E-06,-.3540E-04,
     +-.1500E-03,-.8830E-04,-.2474E-04,-.8373E-05, .6470E-05, .7992E-05,
     + .9096E-05, .6737E-05,-.1485E-04, .1873E-04,-.4948E-06,-.4491E-04,
     +-.1500E-03,-.8601E-04,-.2664E-04,-.1028E-04, .6851E-05, .6851E-05,
     + .1294E-04,-.2550E-05,-.1520E-04, .2310E-04, .4948E-06,-.2017E-04,
     +-.1507E-03,-.8373E-04,-.2664E-04,-.1256E-04, .4567E-05, .1028E-04,
     + .9210E-05,-.2131E-05,-.6995E-05, .7498E-05,-.1104E-04,-.2284E-05,
     +-.1519E-03,-.8183E-04,-.2816E-04,-.1142E-04, .7611E-06, .7231E-05,
     + .1751E-05,-.7612E-06, .8312E-05, .2436E-05,-.7231E-05, .2398E-04,
     +-.1530E-03,-.7992E-04,-.2893E-04,-.9896E-05, .3806E-06, .8906E-05,
     + .3159E-05,-.5328E-05, .3692E-05,-.2093E-05,-.6851E-05,-.3045E-05,
     +-.1538E-03,-.7536E-04,-.3007E-04,-.8754E-05,-.3045E-05, .5138E-05,
     + .9134E-06,-.1979E-06, .1560E-05,-.1507E-04, .2284E-04, .9895E-05,
     +-.1541E-03,-.7688E-04,-.2969E-04,-.5709E-05,-.3996E-05, .1142E-05,
     +-.8373E-06, .1235E-04,-.7079E-05,-.6737E-05, .1028E-04, .3578E-04,
     +-.1560E-03,-.6851E-04,-.1903E-04,-.4187E-05,-.4605E-05,-.1142E-06,
     + .3878E-05, .3597E-05,-.9591E-05, .5328E-05, .7612E-05,-.4948E-05,
     +-.1587E-03,-.6546E-04,-.2740E-04,-.7612E-06,-.3578E-05, .1713E-05,
     + .6064E-05,-.9781E-05, .1408E-05, .5709E-05, .8373E-05,-.1256E-04,
     +-.1484E-03,-.5823E-04,-.4301E-04,-.1522E-05, .7498E-05,-.5328E-06,
     +-.7855E-05,-.1599E-05, .1964E-04,-.2284E-05, .7882E-10, .5328E-05,
     +-.1238E-03,-.5700E-04,-.5266E-04, .3286E-05, .4910E-05,-.8602E-05,
     + .6090E-06, .8454E-05, .1256E-05,-.4072E-05,-.1903E-05, .6470E-05,
     +-.1155E-03,-.5231E-04,-.4396E-04, .3626E-05,-.7051E-05,-.1743E-05,
     + .9667E-05, .2064E-04,-.2778E-05,-.6546E-05,-.4948E-05, .1903E-05,
     +-.1024E-03,-.5129E-04,-.4506E-04, .7943E-06, .3074E-06, .3243E-05,
     + .2754E-04,-.1479E-05, .1661E-05,-.2969E-05,-.1066E-04, .7612E-06,
     +-.8473E-04,-.5418E-04,-.4674E-04,-.3418E-05, .9460E-05, .1151E-04,
     + .5714E-05,-.1069E-04,-.2022E-05,-.9061E-05,-.1104E-04,-.3083E-04,
     +-.4283E-04,-.5037E-04,-.4476E-04, .1951E-04, .8922E-05, .1296E-04,
     +-.4053E-05,-.4355E-05,-.2355E-05,-.5004E-05,-.1218E-04,-.1522E-04,
     + .6411E-05,-.5937E-04,-.5331E-04, .1934E-04, .5284E-05, .1129E-04,
     +-.2166E-05,-.1484E-06,-.5407E-05,-.1364E-04,-.3115E-05, .3004E-04,
     +-.5074E-04,-.6256E-04,-.5097E-04, .2218E-04, .1228E-04,-.1160E-05,
     +-.1105E-05, .1618E-06,-.6089E-05,-.4216E-06,-.5314E-05, .7903E-05/
      data ( ( ( coehcb(k,j,i), i = 1, 12 ), j = 1, 19 ), k = 1, 2 ) /
     +-.9593E+01,-.4078E+01,-.2812E+01,-.6506E+00,-.4123E+00, .2055E+01,
     + .4097E+01, .4671E+01, .4639E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.9276E+01,-.3757E+01,-.2467E+01,-.5784E+00, .8833E-01, .2232E+01,
     + .3826E+01, .4723E+01, .4942E+01, .5135E+01,-.4587E+02,-.4587E+02,
     +-.8968E+01,-.3508E+01,-.2116E+01,-.1363E+00, .1662E+00, .2424E+01,
     + .4220E+01, .4513E+01, .1375E+01, .4601E+01,-.4587E+02,-.4587E+02,
     +-.8662E+01,-.3164E+01,-.1722E+01, .5178E-01, .7288E+00, .2411E+01,
     + .3805E+01, .4766E+01, .4342E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.8292E+01,-.2799E+01,-.1359E+01, .3271E+00, .1650E+01, .2395E+01,
     + .4192E+01, .4758E+01, .2470E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.7812E+01,-.2404E+01,-.1085E+01, .7167E+00, .2202E+01, .2922E+01,
     + .4322E+01, .4591E+01, .4186E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.7441E+01,-.2066E+01,-.7142E+00, .1057E+01, .2524E+01, .2946E+01,
     + .4220E+01, .3607E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.7191E+01,-.1745E+01,-.3487E+00, .1453E+01, .2739E+01, .3660E+01,
     + .4114E+01, .3245E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.6895E+01,-.1326E+01,-.3500E+00, .1647E+01, .2899E+01, .4023E+01,
     + .3361E+01, .3360E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.5876E+01,-.9573E+00, .2014E+00, .2130E+01, .3493E+01, .4088E+01,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.4429E+01,-.3417E+00, .1204E+01, .2780E+01, .3843E+01, .3099E+01,
     +-.4587E+02, .3605E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.3122E+01, .2697E+00, .1866E+01, .3526E+01, .3569E+01, .1025E+01,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.2284E+01, .8186E+00, .2754E+01, .3206E+01, .3704E+01,-.4587E+02,
     +-.4587E+02, .4625E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.1711E+01, .1220E+01, .3248E+01,-.4587E+02, .2565E+01, .3297E+01,
     +-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.1758E+01, .7970E+00, .2758E+01, .2926E+01, .2613E+01, .1974E+01,
     +-.4587E+02, .2310E+01,-.4587E+02,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.1737E+01, .3499E+00, .2246E+01, .2673E+01, .3308E+01, .3463E+01,
     + .3103E+01, .2611E+01, .2178E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.1559E+01, .2215E+00, .1875E+01, .2500E+01, .3346E+01, .3585E+01,
     + .3946E+01, .3533E+01, .3205E+01,-.4587E+02,-.4587E+02,-.4587E+02,
     +-.1601E+01, .5060E-01, .1275E+01, .2176E+01, .3081E+01, .3649E+01,
     + .3940E+01, .4106E+01, .4112E+01, .4349E+01, .2292E+01,-.4587E+02,
     +-.1222E+01, .3199E+00, .1642E+01, .2380E+01, .3254E+01, .3534E+01,
     + .3687E+01, .3717E+01, .3402E+01, .3868E+01,-.4587E+02,-.4587E+02,
     + .2967E-01, .1697E-01, .1795E-01, .1387E-01, .2032E-01, .1187E-01,
     + .2560E-01, .1044E-01,-.4560E+00, .3059E-02, .3059E-02, .3059E-02,
     + .2998E-01, .1586E-01, .1786E-01, .1521E-01, .1710E-01, .1061E-01,
     + .2030E-01, .1158E-01, .4452E+00, .3059E-02, .3059E-02, .3059E-02,
     + .2993E-01, .1551E-01, .1481E-01, .9846E-02, .2443E-01, .1150E-01,
     + .1865E-01, .1376E-01, .4617E+00, .3059E-02, .3059E-02, .3059E-02,
     + .3035E-01, .1417E-01, .1438E-01, .1511E-01, .1901E-01, .8582E-02,
     + .1746E-01, .1450E-01, .4523E+00, .3059E-02, .3059E-02, .3059E-02,
     + .2970E-01, .1347E-01, .1322E-01, .1252E-01, .1665E-01, .1037E-01,
     + .1320E-01, .1199E-01, .4436E+00, .3059E-02, .3059E-02, .3059E-02,
     + .2949E-01, .1291E-01, .1671E-01, .1111E-01, .1400E-01, .1318E-01,
     + .1060E-01, .1046E-01, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3004E-01, .1300E-01, .1413E-01, .9085E-02, .9764E-02, .2260E-01,
     + .9778E-02, .4671E+00, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3086E-01, .1436E-01, .1205E-01, .1081E-01, .4681E-02, .1479E-01,
     + .1888E-01, .3494E-01, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3094E-01, .1500E-01, .1457E-01, .1060E-01, .8319E-02, .8983E-02,
     + .3791E-01, .2232E-01, .4631E+00, .3059E-02, .3059E-02, .3059E-02,
     + .3158E-01, .1585E-01, .1292E-01, .6531E-02, .1383E-01, .4605E+00,
     + .4662E+00, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .3182E-01, .1586E-01, .8724E-02, .5798E-02, .2454E-01, .4607E+00,
     + .4560E+00, .4511E+00, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .2369E-01, .1606E-01, .5477E-02, .1228E-01, .4579E+00, .4561E+00,
     + .4497E+00, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .2190E-01, .1779E-01, .6267E-02, .4535E+00, .4533E+00, .3059E-02,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .2100E-01, .1653E-01, .7449E-02, .4543E+00, .4472E+00, .4439E+00,
     + .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02, .3059E-02,
     + .1864E-01, .1771E-01, .7040E-02, .2877E-01, .3381E-01, .2691E-01,
     + .4466E+00, .3059E-02, .4613E+00, .3059E-02, .3059E-02, .3059E-02,
     + .1637E-01, .1641E-01, .8424E-02, .1318E-01, .2060E-01, .3426E-01,
     + .4122E-01, .4621E+00, .4555E+00, .4525E+00, .3059E-02, .3059E-02,
     + .1607E-01, .1452E-01, .8013E-02, .1213E-01, .1482E-01, .2125E-01,
     + .3379E-01, .3562E-01, .4619E+00, .4569E+00, .3059E-02, .3059E-02,
     + .1698E-01, .1538E-01, .6616E-02, .1147E-01, .1217E-01, .1696E-01,
     + .1871E-01, .2273E-01, .4513E-01, .4702E+00, .4617E+00, .4553E+00,
     + .1700E-01, .1547E-01, .6456E-02, .1324E-01, .1502E-01, .2095E-01,
     + .2547E-01, .2823E-01, .4107E-01, .4676E+00, .4583E+00, .4498E+00/
      data ( ( ( coehcb(k,j,i), i = 1, 12 ), j = 1, 19 ), k = 3, 3 ) /
     +-.6747E-05,-.2483E-04, .6575E-04, .1026E-03, .3888E-03,-.8519E-04,
     +-.1629E-03,-.1808E-04,-.8355E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.2270E-04,-.3427E-04, .5118E-04, .1218E-03, .1245E-03,-.1245E-03,
     + .3841E-05,-.4151E-04,-.8763E-02,-.1687E-01,-.4656E-05,-.4656E-05,
     +-.4557E-04,-.3023E-04, .2286E-04, .5656E-04, .4113E-04,-.1407E-03,
     +-.1301E-03, .8503E-04,-.7284E-02,-.1669E-01,-.4656E-05,-.4656E-05,
     +-.5325E-04,-.5309E-04,-.1246E-04, .2244E-04, .5136E-04,-.1272E-03,
     + .4217E-04,-.1749E-04,-.8435E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.6857E-04,-.7217E-04, .1740E-05, .3653E-04,-.1490E-03,-.4090E-04,
     +-.2376E-04, .2047E-04,-.7974E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.1232E-03,-.9826E-04,-.2849E-04, .1703E-04,-.1895E-03,-.3363E-03,
     + .7102E-04,-.1838E-05,-.1655E-01,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.9896E-04,-.5127E-04,-.2704E-04,-.1218E-04,-.1207E-03,-.5883E-04,
     + .6893E-04,-.7924E-02,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.7837E-04,-.4980E-04, .6902E-05,-.1072E-03,-.4051E-04,-.1991E-05,
     +-.1173E-03,-.5195E-04,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.8136E-04,-.8102E-04, .1254E-03,-.4658E-04, .3173E-04,-.4461E-05,
     +-.1558E-03,-.2036E-03, .8360E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.2232E-04,-.6411E-04, .9486E-04,-.2322E-03,-.8282E-04,-.8202E-02,
     + .8416E-02,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.1398E-03,-.7165E-04,-.4258E-04,-.3970E-04,-.2839E-03,-.7873E-02,
     + .8231E-02,-.8213E-02,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.6754E-04,-.7469E-04,-.6898E-04,-.1702E-03,-.8079E-02,-.7270E-02,
     + .8116E-02,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.2396E-04,-.2361E-04,-.8664E-04,-.8038E-02,-.8207E-02,-.4656E-05,
     +-.4656E-05,-.1670E-01,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.5479E-04,-.7593E-04,-.1005E-03, .8199E-02,-.7942E-02,-.8244E-02,
     +-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.3806E-04,-.5825E-04,-.1003E-03,-.2925E-03,-.1506E-03, .3148E-04,
     + .8060E-02,-.1593E-01, .8327E-02,-.4656E-05,-.4656E-05,-.4656E-05,
     +-.4706E-04,-.3630E-04,-.7811E-04,-.6881E-04,-.1822E-03,-.3091E-03,
     +-.3033E-03,-.7684E-02,-.7663E-02, .8167E-02,-.4656E-05,-.4656E-05,
     +-.7669E-04,-.4610E-04,-.8063E-04,-.7250E-04,-.1094E-03,-.1241E-03,
     +-.2944E-03,-.1736E-03,-.7886E-02, .8248E-02,-.4656E-05,-.4656E-05,
     +-.7138E-04,-.4545E-04,-.3653E-04,-.6075E-04,-.4528E-04,-.1077E-03,
     +-.1119E-03,-.1657E-03,-.4695E-03,-.8112E-02,-.7587E-02, .8217E-02,
     +-.6812E-04,-.4558E-04,-.6739E-04,-.8861E-04,-.9386E-04,-.1334E-03,
     +-.2007E-03,-.2179E-03,-.1650E-03,-.8001E-02, .8273E-02, .8118E-02/
      end

      block data ckd16
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  seven cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 540 to 400 cm**-1.
c *********************************************************************
      common /band16/ hk(7), coeh2o(3,19,7)
       data hk / .12, .24, .24, .20, .12, .06, .02 /
      data ( ( ( coeh2o(k,j,i), i = 1, 7 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2344E+02,-.2016E+02,-.1986E+02,-.1655E+02,-.1243E+02,-.8437E+01,
     +-.4858E+01,-.2298E+02,-.2014E+02,-.1984E+02,-.1609E+02,-.1198E+02,
     +-.8020E+01,-.4548E+01,-.2252E+02,-.2012E+02,-.1981E+02,-.1564E+02,
     +-.1153E+02,-.7596E+01,-.4239E+01,-.2206E+02,-.2009E+02,-.1957E+02,
     +-.1517E+02,-.1111E+02,-.7161E+01,-.3871E+01,-.2160E+02,-.2007E+02,
     +-.1911E+02,-.1472E+02,-.1065E+02,-.6721E+01,-.3479E+01,-.2113E+02,
     +-.2005E+02,-.1865E+02,-.1426E+02,-.1021E+02,-.6302E+01,-.3081E+01,
     +-.2067E+02,-.2003E+02,-.1819E+02,-.1379E+02,-.9765E+01,-.5883E+01,
     +-.2678E+01,-.2026E+02,-.2001E+02,-.1773E+02,-.1333E+02,-.9332E+01,
     +-.5443E+01,-.2253E+01,-.2024E+02,-.1999E+02,-.1727E+02,-.1288E+02,
     +-.8897E+01,-.5029E+01,-.1858E+01,-.2026E+02,-.1959E+02,-.1481E+02,
     +-.1147E+02,-.7477E+01,-.4555E+01,-.1464E+01,-.2022E+02,-.1632E+02,
     +-.1305E+02,-.9885E+01,-.6689E+01,-.4108E+01,-.1068E+01,-.1936E+02,
     +-.1438E+02,-.1163E+02,-.8499E+01,-.6146E+01,-.3673E+01,-.6816E+00,
     +-.1675E+02,-.1281E+02,-.1020E+02,-.7716E+01,-.5678E+01,-.3256E+01,
     +-.3125E+00,-.1510E+02,-.1124E+02,-.8821E+01,-.7140E+01,-.5243E+01,
     +-.2851E+01,-.2560E-01,-.1334E+02,-.9708E+01,-.8061E+01,-.6611E+01,
     +-.4842E+01,-.2459E+01, .1711E+00,-.1155E+02,-.8798E+01,-.7440E+01,
     +-.6123E+01,-.4439E+01,-.2089E+01, .2480E+00,-.1020E+02,-.8154E+01,
     +-.6945E+01,-.5681E+01,-.4055E+01,-.1737E+01, .2390E+00,-.9464E+01,
     +-.7677E+01,-.6512E+01,-.5284E+01,-.3707E+01,-.1453E+01, .2015E+00,
     +-.9033E+01,-.7246E+01,-.6093E+01,-.4882E+01,-.3346E+01,-.1264E+01,
     + .1033E+00, .4658E-01, .5840E-02, .4626E-02, .2688E-01, .2395E-01,
     + .1804E-01, .2074E-01, .4660E-01, .1884E-02, .8561E-02, .2690E-01,
     + .2403E-01, .1788E-01, .1934E-01, .4660E-01, .1800E-02, .1252E-01,
     + .2694E-01, .2393E-01, .1786E-01, .1825E-01, .4660E-01, .1779E-02,
     + .1649E-01, .2696E-01, .2397E-01, .1779E-01, .1765E-01, .4348E-01,
     + .1758E-02, .2043E-01, .2696E-01, .2393E-01, .1748E-01, .1675E-01,
     + .3944E-01, .1737E-02, .2445E-01, .2698E-01, .2384E-01, .1752E-01,
     + .1549E-01, .3538E-01, .1654E-02, .2847E-01, .2702E-01, .2384E-01,
     + .1714E-01, .1565E-01, .3127E-01, .1570E-02, .3245E-01, .2705E-01,
     + .2374E-01, .1712E-01, .1514E-01, .2715E-01, .1444E-02, .3540E-01,
     + .2711E-01, .2363E-01, .1702E-01, .1446E-01, .2960E-01, .1760E-01,
     + .2977E-01, .2397E-01, .2087E-01, .1618E-01, .1445E-01, .2466E-01,
     + .3039E-01, .2428E-01, .2217E-01, .1821E-01, .1593E-01, .1463E-01,
     + .2640E-01, .2545E-01, .2231E-01, .2060E-01, .1773E-01, .1555E-01,
     + .1473E-01, .3456E-01, .2135E-01, .2030E-01, .1844E-01, .1740E-01,
     + .1559E-01, .1428E-01, .3203E-01, .2047E-01, .1809E-01, .1760E-01,
     + .1725E-01, .1545E-01, .1541E-01, .2137E-01, .1857E-01, .1616E-01,
     + .1698E-01, .1700E-01, .1537E-01, .1636E-01, .1338E-01, .1518E-01,
     + .1580E-01, .1658E-01, .1710E-01, .1518E-01, .1513E-01, .1570E-01,
     + .1614E-01, .1603E-01, .1673E-01, .1706E-01, .1497E-01, .1439E-01,
     + .1987E-01, .1731E-01, .1601E-01, .1675E-01, .1681E-01, .1535E-01,
     + .1425E-01, .2018E-01, .1723E-01, .1597E-01, .1691E-01, .1666E-01,
     + .1509E-01, .1446E-01,-.2873E-03,-.8031E-04, .4225E-04,-.9287E-04,
     +-.6013E-04,-.4339E-04,-.2474E-04,-.2862E-03,-.8372E-05, .1146E-03,
     +-.9248E-04,-.6166E-04,-.3882E-04,-.1827E-04,-.2870E-03,-.6851E-05,
     + .1865E-03,-.9172E-04,-.6128E-04,-.3616E-04,-.7612E-05,-.2877E-03,
     +-.7231E-05, .1880E-03,-.9287E-04,-.5671E-04,-.4110E-04,-.1104E-04,
     +-.3429E-03,-.7612E-05, .1149E-03,-.9287E-04,-.6356E-04,-.4529E-04,
     +-.2436E-04,-.4187E-03,-.7992E-05, .4339E-04,-.9325E-04,-.6280E-04,
     +-.4225E-04,-.3197E-04,-.4925E-03,-.8754E-05,-.2740E-04,-.9477E-04,
     +-.6432E-04,-.3768E-04,-.3361E-04,-.5511E-03,-.8753E-05,-.9972E-04,
     +-.9515E-04,-.6394E-04,-.3806E-04,-.3787E-04,-.4792E-03,-.1028E-04,
     +-.1534E-03,-.9477E-04,-.6356E-04,-.3616E-04,-.2923E-04,-.5070E-03,
     + .1922E-03,-.1028E-03,-.5823E-04,-.7954E-04,-.2550E-04,-.3893E-04,
     +-.3776E-03,-.1043E-03,-.7993E-04,-.7422E-04,-.4948E-04,-.3007E-04,
     +-.3863E-04, .8335E-04,-.5709E-04,-.6090E-04,-.7840E-04,-.3692E-04,
     +-.3007E-04,-.4251E-04,-.6204E-04,-.4872E-04,-.3806E-04,-.4681E-04,
     +-.3463E-04,-.3007E-04,-.4312E-04,-.1142E-04,-.5176E-04,-.5024E-04,
     +-.3007E-04,-.3730E-04,-.3037E-04,-.3888E-04, .2550E-04,-.6508E-04,
     +-.2512E-04,-.3083E-04,-.3197E-04,-.3041E-04,-.3750E-04, .1484E-04,
     +-.1941E-04,-.2626E-04,-.3349E-04,-.3463E-04,-.2896E-04,-.1716E-04,
     +-.7231E-04,-.3920E-04,-.2893E-04,-.3540E-04,-.3311E-04,-.3734E-04,
     +-.2550E-05,-.7650E-04,-.3159E-04,-.2778E-04,-.3121E-04,-.2169E-04,
     +-.4365E-04,-.1546E-04,-.7916E-04,-.2931E-04,-.2854E-04,-.3654E-04,
     +-.1979E-04,-.4811E-04,-.1435E-04/
      end

      block data ckd17
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and  seven cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 400 to 280 cm**-1.
c *********************************************************************
      common /band17/ hk(7), coeh2o(3,19,7)
       data hk / .12, .26, .22, .20, .10, .085, .015 /
      data ( ( ( coeh2o(k,j,i), i = 1, 7 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2255E+02,-.2000E+02,-.1703E+02,-.1282E+02,-.9215E+01,-.5938E+01,
     +-.2009E+01,-.2209E+02,-.1997E+02,-.1657E+02,-.1236E+02,-.8764E+01,
     +-.5499E+01,-.1582E+01,-.2163E+02,-.1993E+02,-.1611E+02,-.1191E+02,
     +-.8324E+01,-.5061E+01,-.1170E+01,-.2117E+02,-.1990E+02,-.1565E+02,
     +-.1146E+02,-.7889E+01,-.4631E+01,-.7737E+00,-.2071E+02,-.1987E+02,
     +-.1519E+02,-.1100E+02,-.7440E+01,-.4179E+01,-.3719E+00,-.2026E+02,
     +-.1985E+02,-.1473E+02,-.1054E+02,-.6995E+01,-.3721E+01, .0000E+00,
     +-.2024E+02,-.1982E+02,-.1426E+02,-.1009E+02,-.6549E+01,-.3284E+01,
     + .4053E+00,-.2022E+02,-.1980E+02,-.1381E+02,-.9639E+01,-.6097E+01,
     +-.2821E+01, .8375E+00,-.2021E+02,-.1933E+02,-.1335E+02,-.9187E+01,
     +-.5653E+01,-.2379E+01, .1272E+01,-.2010E+02,-.1503E+02,-.1125E+02,
     +-.7665E+01,-.4492E+01,-.1893E+01, .1642E+01,-.1747E+02,-.1278E+02,
     +-.9547E+01,-.6120E+01,-.3756E+01,-.1443E+01, .1995E+01,-.1529E+02,
     +-.1095E+02,-.8107E+01,-.5036E+01,-.3182E+01,-.1032E+01, .2429E+01,
     +-.1370E+02,-.9303E+01,-.6691E+01,-.4357E+01,-.2683E+01,-.6173E+00,
     + .2805E+01,-.1150E+02,-.7859E+01,-.5618E+01,-.3843E+01,-.2234E+01,
     +-.2171E+00, .2973E+01,-.9590E+01,-.6537E+01,-.4886E+01,-.3355E+01,
     +-.1805E+01, .1615E+00, .3157E+01,-.7530E+01,-.5699E+01,-.4306E+01,
     +-.2892E+01,-.1388E+01, .5448E+00, .3155E+01,-.6758E+01,-.5112E+01,
     +-.3809E+01,-.2464E+01,-.9947E+00, .8713E+00, .3203E+01,-.6245E+01,
     +-.4610E+01,-.3376E+01,-.2058E+01,-.6166E+00, .1073E+01, .3109E+01,
     +-.5777E+01,-.4175E+01,-.2963E+01,-.1671E+01,-.2556E+00, .1241E+01,
     + .3014E+01, .4264E-01, .1968E-02, .1863E-01, .1436E-01, .1101E-01,
     + .1055E-01, .1281E-01, .4264E-01, .1989E-02, .1861E-01, .1438E-01,
     + .1095E-01, .1030E-01, .1211E-01, .3996E-01, .1968E-02, .1861E-01,
     + .1434E-01, .1103E-01, .1019E-01, .1160E-01, .3600E-01, .1947E-02,
     + .1861E-01, .1442E-01, .1086E-01, .1003E-01, .1157E-01, .3203E-01,
     + .5756E-02, .1861E-01, .1444E-01, .1080E-01, .9922E-02, .1151E-01,
     + .2801E-01, .9713E-02, .1859E-01, .1446E-01, .1070E-01, .9880E-02,
     + .1066E-01, .2393E-01, .1369E-01, .1859E-01, .1451E-01, .1057E-01,
     + .9880E-02, .1072E-01, .1987E-01, .1767E-01, .1863E-01, .1451E-01,
     + .1040E-01, .9880E-02, .1057E-01, .1572E-01, .2169E-01, .1863E-01,
     + .1442E-01, .1022E-01, .9742E-02, .1036E-01, .3391E-02, .1884E-01,
     + .1566E-01, .1105E-01, .1011E-01, .1001E-01, .1017E-01, .1982E-01,
     + .1444E-01, .1189E-01, .1030E-01, .9859E-02, .9861E-02, .1038E-01,
     + .1748E-01, .1321E-01, .9922E-02, .1068E-01, .1013E-01, .9937E-02,
     + .9958E-02, .1346E-01, .9943E-02, .9566E-02, .1097E-01, .9815E-02,
     + .9964E-02, .1059E-01, .9817E-02, .7159E-02, .8687E-02, .1114E-01,
     + .1007E-01, .1014E-01, .1058E-01, .3370E-02, .7264E-02, .9378E-02,
     + .1112E-01, .9767E-02, .1016E-01, .1101E-01, .2993E-02, .8017E-02,
     + .9566E-02, .1116E-01, .9738E-02, .1025E-01, .1086E-01, .8331E-02,
     + .8771E-02, .1001E-01, .1117E-01, .9847E-02, .1076E-01, .1084E-01,
     + .7850E-02, .9378E-02, .1001E-01, .1105E-01, .9964E-02, .1113E-01,
     + .1168E-01, .8038E-02, .9336E-02, .9817E-02, .1096E-01, .1024E-01,
     + .1175E-01, .1107E-01,-.2188E-03,-.2283E-05,-.8069E-04,-.4415E-04,
     +-.2284E-04,-.4491E-04,-.4518E-04,-.2196E-03,-.2665E-05,-.8107E-04,
     +-.4301E-04,-.2398E-04,-.4795E-04,-.4693E-04,-.2683E-03,-.3045E-05,
     +-.8107E-04,-.4301E-04,-.2246E-04,-.4757E-04,-.4152E-04,-.3403E-03,
     +-.4187E-05,-.8031E-04,-.3996E-04,-.1865E-04,-.4301E-04,-.4350E-04,
     +-.4118E-03, .6584E-04,-.8107E-04,-.4034E-04,-.1903E-04,-.4643E-04,
     +-.4834E-04,-.4803E-03, .1378E-03,-.8069E-04,-.4072E-04,-.1713E-04,
     +-.5176E-04,-.3460E-04,-.4099E-03, .2101E-03,-.8069E-04,-.3920E-04,
     +-.1713E-04,-.5024E-04,-.3524E-04,-.3391E-03, .2809E-03,-.7992E-04,
     +-.3616E-04,-.2017E-04,-.5633E-04,-.4886E-04,-.2668E-03, .2078E-03,
     +-.8069E-04,-.3768E-04,-.2131E-04,-.5580E-04,-.5454E-04,-.2207E-04,
     +-.8601E-04,-.4643E-04,-.2436E-04,-.4148E-04,-.5458E-04,-.4579E-04,
     +-.5138E-04,-.2893E-04,-.3273E-04,-.3882E-04,-.3920E-04,-.5035E-04,
     +-.3170E-04,-.2169E-04,-.3007E-04,-.2740E-04,-.5328E-04,-.4491E-04,
     +-.4403E-04,-.6383E-04, .4834E-04,-.2702E-04,-.4453E-04,-.4339E-04,
     +-.4457E-04,-.4551E-04,-.8133E-04, .3768E-04,-.7611E-06,-.2626E-04,
     +-.4643E-04,-.4305E-04,-.4840E-04,-.5149E-04, .7193E-04,-.2169E-04,
     +-.4491E-04,-.3996E-04,-.4483E-04,-.4487E-04,-.6698E-04,-.4834E-04,
     +-.3463E-04,-.4986E-04,-.4377E-04,-.4514E-04,-.5377E-04,-.2626E-04,
     +-.4187E-04,-.3692E-04,-.5100E-04,-.4651E-04,-.4392E-04,-.5386E-04,
     +-.4643E-04,-.4301E-04,-.3578E-04,-.5176E-04,-.4594E-04,-.4551E-04,
     +-.3920E-04,-.3425E-04,-.4491E-04,-.3654E-04,-.5138E-04,-.4377E-04,
     +-.5614E-04,-.5758E-04,-.3600E-04/
      end

      block data ckd18
c *********************************************************************
c hk is the interval in the g (cumulative probability) space from 0 
c to one. coeh2o is the coefficient to calculate the H2O absorption
c coefficient in units of (cm-atm)**-1 at there temperatures, nine-
c teen pressures, and eight cumulative probabilities ( Fu,  1991 ).
c The spectral region is from 280 to 0 cm**-1.
c *********************************************************************
      common /band18/ hk(8), coeh2o(3,19,8)

c       data hk / .07, .1, .2, .25, .2, .1, .03, .02 /
        data hk / .10, .1, .2, .25, .2, .1, .03, .02 /
      data ( ( ( coeh2o(k,j,i), i = 1, 8 ), j = 1, 19 ), k = 1, 3 ) /
     +-.2121E+02,-.2002E+02,-.1676E+02,-.1274E+02,-.8780E+01,-.5167E+01,
     +-.2692E+01,-.6275E+00,-.2075E+02,-.1996E+02,-.1630E+02,-.1228E+02,
     +-.8324E+01,-.4718E+01,-.2260E+01,-.2303E+00,-.2029E+02,-.1990E+02,
     +-.1584E+02,-.1182E+02,-.7868E+01,-.4269E+01,-.1806E+01, .1645E+00,
     +-.2022E+02,-.1985E+02,-.1538E+02,-.1136E+02,-.7417E+01,-.3820E+01,
     +-.1373E+01, .5657E+00,-.2018E+02,-.1981E+02,-.1492E+02,-.1090E+02,
     +-.6965E+01,-.3369E+01,-.9319E+00, .9577E+00,-.2013E+02,-.1937E+02,
     +-.1446E+02,-.1044E+02,-.6512E+01,-.2917E+01,-.4928E+00, .1376E+01,
     +-.2009E+02,-.1891E+02,-.1400E+02,-.9984E+01,-.6063E+01,-.2466E+01,
     +-.6887E-01, .1768E+01,-.2006E+02,-.1845E+02,-.1354E+02,-.9530E+01,
     +-.5618E+01,-.2024E+01, .3615E+00, .2196E+01,-.2003E+02,-.1800E+02,
     +-.1308E+02,-.9075E+01,-.5174E+01,-.1593E+01, .7820E+00, .2600E+01,
     +-.1827E+02,-.1464E+02,-.1097E+02,-.7525E+01,-.3733E+01,-.1077E+01,
     + .1204E+01, .3014E+01,-.1525E+02,-.1210E+02,-.9275E+01,-.5876E+01,
     +-.2768E+01,-.6286E+00, .1622E+01, .3394E+01,-.1298E+02,-.1060E+02,
     +-.7764E+01,-.4462E+01,-.2154E+01,-.2001E+00, .2034E+01, .3756E+01,
     +-.1157E+02,-.8941E+01,-.5984E+01,-.3509E+01,-.1651E+01, .2279E+00,
     + .2422E+01, .4066E+01,-.9986E+01,-.7062E+01,-.4794E+01,-.2818E+01,
     +-.1196E+01, .6394E+00, .2791E+01, .4283E+01,-.8064E+01,-.5512E+01,
     +-.3933E+01,-.2274E+01,-.7559E+00, .1036E+01, .3085E+01, .4444E+01,
     +-.6440E+01,-.4863E+01,-.3219E+01,-.1791E+01,-.3279E+00, .1427E+01,
     + .3304E+01, .4527E+01,-.5902E+01,-.4207E+01,-.2756E+01,-.1350E+01,
     + .7686E-01, .1776E+01, .3475E+01, .4550E+01,-.5439E+01,-.3739E+01,
     +-.2330E+01,-.9233E+00, .4612E+00, .2066E+01, .3564E+01, .4502E+01,
     +-.5006E+01,-.3316E+01,-.1906E+01,-.5066E+00, .8352E+00, .2272E+01,
     + .3587E+01, .4419E+01, .2338E-01, .1968E-02, .9503E-02, .3412E-02,
     + .6280E-03,-.1109E-02,-.1089E-02,-.1026E-02, .1972E-01, .2093E-02,
     + .9503E-02, .3391E-02, .6489E-03,-.1172E-02,-.1164E-02,-.1158E-02,
     + .1603E-01, .3328E-02, .9524E-02, .3391E-02, .6489E-03,-.1277E-02,
     +-.1229E-02,-.1296E-02, .1229E-01, .7138E-02, .9524E-02, .3370E-02,
     + .6070E-03,-.1319E-02,-.1264E-02,-.1610E-02, .8478E-02, .1095E-01,
     + .9566E-02, .3412E-02, .5652E-03,-.1382E-02,-.1266E-02,-.1566E-02,
     + .4563E-02, .1480E-01, .9566E-02, .3412E-02, .5443E-03,-.1423E-02,
     +-.1199E-02,-.1679E-02, .2261E-02, .1865E-01, .9608E-02, .3454E-02,
     + .4815E-03,-.1423E-02,-.1296E-02,-.1555E-02, .2198E-02, .2250E-01,
     + .9671E-02, .3412E-02, .4187E-03,-.1426E-02,-.1472E-02,-.1800E-02,
     + .2072E-02, .2600E-01, .9734E-02, .3433E-02, .3977E-03,-.1428E-02,
     +-.1541E-02,-.1591E-02, .1987E-01, .8645E-02, .6280E-02, .1298E-02,
     +-.1151E-02,-.1509E-02,-.1662E-02,-.1570E-02, .4668E-02, .8373E-02,
     + .3956E-02,-.4187E-04,-.1968E-02,-.1624E-02,-.1700E-02,-.1947E-02,
     + .9231E-02, .5694E-02, .1444E-02,-.2512E-03,-.1827E-02,-.1662E-02,
     +-.1576E-02,-.1633E-02, .8666E-02, .3077E-02,-.1737E-02,-.1277E-02,
     +-.1507E-02,-.1757E-02,-.1612E-02,-.1612E-02, .8164E-03,-.4375E-02,
     +-.1884E-02,-.1277E-02,-.1564E-02,-.1853E-02,-.1591E-02,-.1486E-02,
     +-.1486E-02,-.2596E-02,-.1633E-02,-.1539E-02,-.1662E-02,-.1846E-02,
     +-.1423E-02,-.1277E-02,-.1423E-02,-.2617E-02,-.1005E-02,-.1379E-02,
     +-.1687E-02,-.1905E-02,-.1528E-02,-.1298E-02,-.1675E-03,-.1947E-02,
     +-.5024E-03,-.1325E-02,-.1696E-02,-.1698E-02,-.1486E-02,-.1277E-02,
     + .1047E-03,-.1109E-02,-.5861E-03,-.1363E-02,-.1620E-02,-.1666E-02,
     +-.1507E-02,-.9210E-03, .1047E-03,-.1047E-02,-.8394E-03,-.1342E-02,
     +-.1591E-02,-.1323E-02,-.1340E-02,-.9420E-03,-.1085E-03, .2283E-05,
     +-.4719E-04,-.3807E-06,-.1522E-05,-.3425E-05,-.7612E-06, .1751E-05,
     +-.1766E-03, .1523E-05,-.4719E-04,-.7609E-06,-.3807E-06,-.3045E-05,
     + .1599E-05, .8723E-05,-.2443E-03, .1941E-04,-.4757E-04,-.1522E-05,
     +-.3806E-06,-.1903E-05,-.2778E-05, .1294E-04,-.1838E-03, .8563E-04,
     +-.4757E-04,-.1903E-05, .1142E-05,-.2664E-05,-.6090E-06, .1321E-04,
     +-.1161E-03, .1526E-03,-.4757E-04,-.2664E-05,-.3805E-06,-.3806E-05,
     +-.2093E-05, .2253E-04,-.4795E-04, .9248E-04,-.4757E-04,-.1903E-05,
     + .0000E+00,-.3045E-05,-.7992E-06, .1393E-04,-.9134E-05, .2246E-04,
     +-.4834E-04,-.2664E-05, .3804E-06,-.5328E-05,-.1510E-05, .1465E-04,
     +-.1028E-04,-.4757E-04,-.4948E-04,-.1142E-05, .7614E-06,-.4910E-05,
     +-.5709E-06, .1477E-04,-.1256E-04,-.1066E-03,-.4910E-04,-.1523E-05,
     +-.3805E-06,-.3121E-05,-.2512E-05, .1142E-04,-.7878E-04,-.2664E-05,
     +-.8373E-05,-.7612E-06, .1104E-04,-.3311E-05,-.1979E-05, .5709E-05,
     +-.2626E-04,-.4872E-04,-.3808E-06,-.2283E-05, .2284E-05,-.3349E-05,
     +-.4034E-05, .7231E-05,-.4910E-04, .1599E-04, .1256E-04,-.7612E-05,
     + .1180E-05,-.1815E-05,-.7193E-05, .3045E-05, .1576E-09, .6470E-05,
     +-.1408E-04,-.1903E-05, .1522E-05,-.4746E-05,-.4948E-05, .3806E-06,
     + .9020E-04, .5214E-04, .6090E-05,-.1104E-04, .1180E-05,-.2778E-05,
     +-.6090E-05,-.2664E-05,-.6737E-04,-.1218E-04,-.3806E-05,-.5214E-05,
     +-.1066E-05,-.1294E-05,-.3045E-05,-.2664E-05,-.4643E-04, .1713E-04,
     +-.1218E-04,-.6204E-05,-.2360E-05,-.1979E-05,-.1903E-05,-.3806E-05,
     +-.3045E-04,-.1256E-04,-.9134E-05,-.6508E-05,-.1027E-05,-.7993E-06,
     +-.1142E-05,-.7992E-05,-.3616E-04,-.1028E-04,-.1066E-04,-.6051E-05,
     + .1066E-05,-.1751E-05,-.2284E-05,-.2284E-05,-.3920E-04,-.9895E-05,
     +-.1321E-04,-.3844E-05,-.2055E-05,-.2512E-05,-.3806E-05,-.3425E-05/
      end




c===================================================================
	block data cont_coef     
	parameter (ncoef=7,nband=12)
	real aa0,aa1,aa2,aa3,aa4

	common /pcont_aa0/ aa0(ncoef,nband)
	common /pcont_aa1/ aa1(ncoef,nband)
	common /pcont_aa2/ aa2(ncoef,nband)
	common /pcont_aa3/ aa3(ncoef,nband)
	common /pcont_aa4/ aa4(ncoef,nband)

	data aa0/ !! ORIGINAL !! pres atm
     x -1.00e+01, 0.000e+00, 0.000e+00, 0.000e+00,
     x 0.000e+00, 0.000e+00, 0.000e+00,
     x 3.457e+00, 9.977e-01,-6.202e-03, 1.822e+00,
     x 8.200e+00, 8.343e-03, 1.413e-02,
     x 2.327e+00, 1.011e+00,-7.050e-03, 1.368e+00,
     x 1.508e+01,-1.747e-03, 1.271e-01,
     x 6.139e+00, 1.015e+00,-1.505e-02, 5.499e-01,
     x 6.284e+00,-7.911e-03, 5.933e-01,
     x 9.234e+00, 1.003e+00,-2.254e-02, 1.204e-01,
     x 3.237e-01,-1.697e-03, 9.105e-01,
     x 9.426e+00, 9.999e-01,-2.438e-02, 2.410e-02,
     x-7.441e-01, 4.155e-05, 9.880e-01,
     x 8.408e+00, 1.000e+00,-2.319e-02, 2.111e-02,
     x-8.137e-01, 6.080e-06, 9.933e-01,
     x 7.285e+00, 1.001e+00,-2.049e-02, 5.285e-02,
     x-5.057e-01,-3.719e-04, 9.647e-01,
     x 1.096e+00, 1.008e+00,-6.852e-03, 1.021e+00,
     x 8.523e+00,-5.127e-03, 2.513e-01,
     x -1.00e+01, 0.000e+00, 0.000e+00, 0.000e+00,
     x 0.000e+00, 0.000e+00, 0.000e+00,
     x -1.00e+01, 0.000e+00, 0.000e+00, 0.000e+00,
     x 0.000e+00, 0.000e+00, 0.000e+00,
     x -1.00e+01, 0.000e+00, 0.000e+00, 0.000e+00,
     x 0.000e+00, 0.000e+00, 0.000e+00
     &/

	data aa1/ !ckdfu.fuliou.lin.out
     x 6.762e+00, 9.996e-01,-7.259e-03, 9.977e-01,
     x 1.603e+01, 1.709e-01,-1.394e-03,
     x 5.261e+00, 9.971e-01,-6.219e-03, 9.868e-01,
     x 1.561e+01, 1.275e-01, 2.789e-03,
     x 3.235e+00, 9.872e-01,-6.404e-03, 8.690e-01,
     x 2.107e+01, 1.222e-01, 6.908e-02,
     x 5.942e+00, 9.982e-01,-1.368e-02, 3.916e-01,
     x 9.076e+00, 2.730e-02, 5.256e-01,
     x 9.217e+00, 1.003e+00,-2.221e-02, 8.940e-02,
     x 6.127e-01,-6.341e-04, 8.978e-01,
     x 9.478e+00, 1.000e+00,-2.441e-02, 1.837e-02,
     x-7.174e-01,-8.151e-06, 9.865e-01,
     x 8.451e+00, 1.000e+00,-2.323e-02, 1.559e-02,
     x-8.067e-01, 5.546e-06, 9.932e-01,
     x 7.281e+00, 1.001e+00,-2.035e-02, 4.216e-02,
     x-4.622e-01,-4.440e-04, 9.615e-01,
     x 1.081e+00, 9.983e-01,-4.259e-03, 8.882e-01,
     x 9.883e+00, 1.679e-02, 6.666e-02,
     x 3.738e+00, 9.973e-01,-3.824e-03, 9.842e-01,
     x 8.171e+00, 5.865e-02, 4.520e-03,
     x 2.879e+00, 9.953e-01,-3.841e-03, 9.721e-01,
     x 7.980e+00, 5.344e-02, 1.106e-02,
     x 7.045e-01, 1.008e+00,-5.968e-03, 7.708e-01,
     x 7.788e+00,-5.452e-03, 1.683e-01
     &/
	data aa2/ !ckdfu.fuliou.log.out
     x 6.405e+00, 9.986e-01,-7.277e-03, 9.963e-01,
     x 1.304e+01, 1.588e-01,-3.569e-04,
     x 5.039e+00, 9.967e-01,-6.107e-03, 9.820e-01,
     x 1.819e+01, 1.188e-01, 4.073e-03,
     x 3.333e+00, 9.832e-01,-6.811e-03, 8.141e-01,
     x 2.107e+01, 1.416e-01, 1.102e-01,
     x 6.498e+00, 1.000e+00,-1.486e-02, 3.513e-01,
     x 7.720e+00, 2.083e-02, 5.794e-01,
     x 9.328e+00, 1.003e+00,-2.251e-02, 8.079e-02,
     x 4.519e-01,-5.421e-04, 9.084e-01,
     x 9.448e+00, 9.999e-01,-2.438e-02, 1.711e-02,
     x-7.349e-01, 5.084e-05, 9.879e-01,
     x 8.424e+00, 1.000e+00,-2.318e-02, 1.598e-02,
     x-8.060e-01,-2.452e-05, 9.931e-01,
     x 7.343e+00, 1.001e+00,-2.051e-02, 3.936e-02,
     x-5.032e-01,-3.829e-04, 9.655e-01,
     x 1.962e+00, 1.004e+00,-6.678e-03, 7.064e-01,
     x 9.220e+00, 5.062e-03, 2.399e-01,
     x 3.616e+00, 9.972e-01,-3.827e-03, 9.825e-01,
     x 8.823e+00, 5.525e-02, 5.027e-03,
     x 2.481e+00, 9.947e-01,-3.944e-03, 9.568e-01,
     x 9.687e+00, 5.039e-02, 1.942e-02,
     x 2.141e+00, 1.009e+00,-1.016e-02, 5.728e-01,
     x 5.100e+00,-5.114e-03, 3.688e-01
     &/

	data aa3/ !ckdfu.fuliou.lin.plnk.out
     x 6.899e+00, 9.989e-01,-7.230e-03, 9.979e-01,
     x 1.680e+01, 2.287e-01,-9.470e-04,
     x 5.300e+00, 9.971e-01,-6.528e-03, 9.864e-01,
     x 1.615e+01, 1.283e-01, 2.595e-03,
     x 3.351e+00, 9.867e-01,-6.864e-03, 8.681e-01,
     x 2.138e+01, 1.249e-01, 6.981e-02,
     x 5.981e+00, 9.983e-01,-1.384e-02, 3.944e-01,
     x 9.316e+00, 2.791e-02, 5.220e-01,
     x 9.253e+00, 1.003e+00,-2.230e-02, 9.128e-02,
     x 6.481e-01,-6.383e-04, 8.963e-01,
     x 9.555e+00, 1.000e+00,-2.460e-02, 1.921e-02,
     x-6.885e-01,-3.569e-05, 9.855e-01,
     x 8.495e+00, 1.000e+00,-2.333e-02, 1.484e-02,
     x-7.828e-01,-1.622e-06, 9.927e-01,
     x 7.328e+00, 1.001e+00,-2.046e-02, 3.898e-02,
     x-5.016e-01,-3.945e-04, 9.649e-01,
     x 8.158e-01, 9.974e-01,-3.582e-03, 8.775e-01,
     x 9.817e+00, 1.744e-02, 7.603e-02,
     x 3.482e+00, 9.971e-01,-3.205e-03, 9.827e-01,
     x 8.142e+00, 5.638e-02, 5.275e-03,
     x 3.262e+00, 9.969e-01,-4.671e-03, 9.760e-01,
     x 8.702e+00, 4.119e-02, 8.187e-03,
     x 1.006e+00, 1.008e+00,-6.560e-03, 8.004e-01,
     x 8.480e+00,-5.247e-03, 1.390e-01
     &/
	data aa4/  !ckdfu.fuliou.log.plnk.out
     x 6.883e+00, 1.007e+00,-7.231e-03, 1.006e+00,
     x 1.711e+01, 2.401e-01,-8.842e-04,
     x 5.128e+00, 1.006e+00,-6.531e-03, 9.905e-01,
     x 1.907e+01, 1.219e-01, 4.032e-03,
     x 3.529e+00, 9.903e-01,-7.494e-03, 8.184e-01,
     x 2.150e+01, 1.434e-01, 1.137e-01,
     x 6.623e+00, 1.008e+00,-1.525e-02, 3.557e-01,
     x 7.867e+00, 2.121e-02, 5.826e-01,
     x 9.438e+00, 1.010e+00,-2.281e-02, 8.279e-02,
     x 4.923e-01,-4.263e-04, 9.138e-01,
     x 9.574e+00, 1.005e+00,-2.472e-02, 1.761e-02,
     x-7.078e-01, 1.032e-04, 9.917e-01,
     x 8.530e+00, 1.007e+00,-2.347e-02, 1.612e-02,
     x-7.979e-01, 1.613e-05, 1.000e+00,
     x 7.429e+00, 1.006e+00,-2.076e-02, 3.668e-02,
     x-5.353e-01,-2.511e-04, 9.738e-01,
     x 1.838e+00, 1.013e+00,-6.037e-03, 6.632e-01,
     x 8.190e+00, 1.126e-04, 2.812e-01,
     x 3.284e+00, 9.994e-01,-3.042e-03, 9.830e-01,
     x 8.944e+00, 4.588e-02, 5.890e-03,
     x 2.977e+00, 9.987e-01,-5.050e-03, 9.663e-01,
     x 9.910e+00, 4.875e-02, 1.525e-02,
     x 2.177e+00, 1.010e+00,-1.035e-02, 6.566e-01,
     x 6.613e+00,-5.513e-03, 2.837e-01
     &/
      end


c===================================================================

c=====================================================================
        function parm_ckd24(iband,amnt,patm,temp,dz)
c Parameterization of CKD_2.4 continuum over Fu-Liou Bands
c Input:
c iband  =  integer (1-12) where
c	  Band 1 ='  5:280cm-1'
c         Band 2 ='280:400cm-1'
c         Band 3 ='400:540cm-1'
c         Band 4 ='540:670cm-1'
c         Band 5 ='670:800cm-1'
c         Band 6 ='800:980cm-1'
c         Band 7 ='980:1100cm-1'
c         Band 8 ='1100:1250cm-1'
c         Band 9 ='1250:1400cm-1'
c         Band10 ='1400:1700cm-1'
c         Band11 ='1700:1900cm-1'
c         Band12 ='1900:2200cm-1'
c amnt = h2O ammount (g/cm**2)
c patm = pressure (atm)
c temp = temperature (k)
c dz   = pathlength (Km)
c Output:
c parm_ckd24 = parameterized CKD_2.4optical depth for band
c234567890123456789012345678901234567890123456789012345678901234567890
	parameter (ncoef=7,nreg=2,nband=12)

	real  h2obnd(nband)
	real ck24_3(ncoef,nreg,nband)! lin Plank wgt

	data h2obnd /-5,-3.5,-2.0,-2,-1.,-4,-4,-4,-3,-3.5,-3,-2/

       data ck24_3/  !ckd24fu.fuliou.lin.plnk.out
! band        1
     x 1.667e+00, 9.421e-01,-7.358e-03, 1.355e+00,
     x 2.557e+03, 5.798e+01,-4.570e-01,
! band        1
     x 6.417e+00, 1.002e+00,-6.991e-03, 1.010e+00,
     x 1.203e+01, 4.501e-02,-2.428e-02,
! band        2
     x 2.390e+00, 9.528e-01,-6.058e-03, 1.071e+00,
     x 2.676e+02, 9.848e+00,-1.459e-01,
! band        2
     x 4.849e+00, 1.002e+00,-6.910e-03, 8.961e-01,
     x 1.635e+01, 2.115e-02, 7.243e-02,
! band        3
     x 2.326e+00, 9.720e-01,-6.551e-03, 8.739e-01,
     x 6.984e+01, 8.346e-01, 4.824e-02,
! band        3
     x 5.002e+00, 1.005e+00,-9.286e-03, 6.222e-01,
     x 1.168e+01, 3.611e-03, 3.148e-01,
! band        4
     x-4.865e+00, 8.455e-01,-6.911e-03, 1.475e+00,
     x 2.905e+02, 7.078e+00,-6.846e-01,
! band        4
     x 4.596e+00, 1.012e+00,-1.152e-02, 5.713e-01,
     x 1.270e+01,-1.395e-03, 3.447e-01,
! band        5
     x-5.396e+00, 8.596e-01,-8.479e-03, 1.619e+00,
     x 1.664e+02, 3.236e+00,-7.782e-01,
! band        5
     x 7.478e+00, 1.007e+00,-1.963e-02, 2.771e-01,
     x 6.021e+00,-4.489e-03, 6.709e-01,
! band        6
     x 1.262e+00, 2.347e-01,-2.360e-02, 1.655e-01,
     x 5.068e+02, 2.462e+01, 3.920e-01,
! band        6
     x 9.334e+00, 1.002e+00,-2.429e-02, 3.575e-02,
     x 2.751e-01,-1.189e-03, 9.593e-01,
! band        7
     x-1.222e+00, 5.423e-01,-2.327e-02, 5.197e-01,
     x 6.423e+02, 5.038e+01, 1.502e-01,
! band        7
     x 8.506e+00, 1.000e+00,-2.339e-02, 8.891e-03,
     x-6.805e-01,-1.639e-04, 9.917e-01,
! band        8
     x-3.638e+00, 8.534e-01,-1.344e-02, 6.816e-01,
     x 5.385e+02, 4.428e+01,-6.366e-03,
! band        8
     x 6.921e+00, 1.002e+00,-1.974e-02, 6.350e-02,
     x 6.838e-01,-1.121e-03, 9.237e-01,
! band        9
     x-2.329e+00, 7.893e-01,-2.588e-03, 1.017e+00,
     x 1.525e+02, 1.029e+01,-1.486e-01,
! band        9
     x 6.742e-01, 1.008e+00,-3.376e-03, 9.105e-01,
     x 1.074e+01,-3.307e-03, 5.741e-02,
! band       10
     x-1.677e+00, 9.173e-01,-5.780e-03, 1.504e+00,
     x 7.886e+02, 2.288e+01,-5.999e-01,
! band       10
     x 3.396e+00, 1.005e+00,-3.433e-03, 1.012e+00,
     x 7.635e+00, 3.010e-03,-2.418e-02,
! band       11
     x 7.943e-01, 9.260e-01,-5.050e-03, 1.141e+00,
     x 2.221e+02, 1.021e+01,-2.246e-01,
! band       11
     x 3.356e+00, 1.002e+00,-4.719e-03, 9.578e-01,
     x 6.164e+00, 1.186e-03, 2.264e-02,
! band       12
     x-5.874e+00, 7.060e-01,-1.532e-03, 1.141e+00,
     x 1.463e+02, 6.534e+00,-4.308e-01,
! band       12
     x 4.709e-01, 1.010e+00,-6.067e-03, 8.513e-01,
     x 1.161e+01,-6.629e-03, 8.885e-02
     &/

	

! These Regressions are more sensitive to pathlength
! So accomodations for very Thin or Thick layers are made.
	 dz1  =dz
	 factor=1.000
	   if ( dz < 0.25  ) then
		factor = 0.25/dz
		 dz1   = 0.25
	   elseif (dz > 1.50) then
	        factor = 1.50/dz
	        dz1    = 1.50
	   endif

	amnt1=amnt*factor

! Regression is now broken up into TWO parts one for small
! one for large	water vapor ammounts.

	ireg=1
	if ( log(amnt1) > h2obnd(iband) ) ireg=2

       ph2o = amnt1 *(8.314d+07 *temp )/
     &              (dz1*1.0d+05*18.01534 *1.01325d+06)

	patmx = log(patm)

!	print'(8f8.3)',log(amnt1),temp,patmx,(ph2o),amnt1,log(ph2o),dz1
!	print*, ireg,iband,aa(1:7,ireg,iband)

 	tau_log	    = ck24_3(1,ireg,iband)	      +
     $		      ck24_3(2,ireg,iband)* log(amnt1)  +
     $		      ck24_3(3,ireg,iband)* temp       +
     $                ck24_3(4,ireg,iband)* patmx       +
     $		      ck24_3(5,ireg,iband)* (ph2o)     +
     $		      ck24_3(6,ireg,iband)* amnt1       +
     $		      ck24_3(7,ireg,iband)* log(ph2o) 
!	print*,tau_log
	parm_ckd24 = exp ( tau_log )
	parm_ckd24 = parm_ckd24/factor
	return
	end function parm_ckd24

!==============================================================
	subroutine kratzsolir(ib,ig,hk,tgout)
	USE FUINPUT , only : nvx,nv1x,nv,pp,pt, ph, po,dz
!        common /atmos/ pp(nv1x), pt(nv1x), ph(nv1x), po(nv1x)

	real ,save ::hk19(5),hk20(5)
	real ,save ,dimension(5,nvx):: tau19,tau20
	real ,  dimension(nvx):: u,uco2,taun2
	real tgout(nvx)

!------------------------------------------------SETUP
	
	if ( ig == 1 ) then !SETUP

!--------------
	LEVELS : do i=1,nv
!	 dz=0.0146337*(pt(i)+pt(i+1))*alog( pp(i+1)/pp(i))
	 pbar= (pp(i+1)+pp(i))*0.5
	 pbar_atm = pbar/1013.25

	 tbar= (pt(i+1)+pt(i))*0.5
 
	units =  1.02* (pp(i+1)- pp(i))
        xlog=(log10(ph(i))+log10(ph(i+1)))/2.0
        u(i)= units * 10.0**xlog
! u H2O in g/cm2
!       note that the ratio un2o/uco2 = 0.31/350.0
        uco2(i)=pbar*1000./(1.3805e-16*tbar)*3.40e-04*dz(i)*
     *          1.0e+05/6.023e+23*44.00995
! uco2 CO2 in g/cm2

	call n2cont(u(i),taun2(i),pbar_atm,tbar,dz(i))

!	print*,i, pbar,d(i)z,u(i),uco2(i)
	enddo LEVELS
!-------------------

	if ( ib == 22 ) then
	call ck19(nvx,nv,hk19,pp,pt,u ,tau19)
	endif!19

	if ( ib == 23 ) then

	call ck20(nvx,nv,hk20,pp,pt,uco2 ,tau20)

	do i=1,nv
!	print'(I3,2f8.1,1x,5f9.3,1x,f8.4)',I,pp(i),pt(i),tau20(1:5,i),taun2(i)

	do j=1,5
	tau20(j,i)= tau20(j,i)+taun2(i) !! Add N2 Continuum tau  to  total tau 
	enddo

	enddo
	endif !20

	endif !!! SET UP 
!------------------------------------------------SETUP

	if ( ib == 22 ) then
	 hk    =  hk19(ig)
	 tgout(1:nv) = tau19(ig,1:nv) 
	endif

	if ( ib == 23 ) then
	 hk    =  hk20(ig)
	 tgout(1:nv) = tau20(ig,1:nv) 
	endif

	return
	end

! *********************************************************************
! 2850:2500
      subroutine ck19(nvx,nv,f,pmb,t,u ,tau19)
   
      real  f(5), coefk(5,3,19),stp(19)
      real ,dimension(5,nvx) :: tau19,fkg
      real ,  dimension(nvx) :: k,pmb,t,u

      ni=5
      f(1)=0.540525
      f(2)=0.325386
      f(3)=0.113208
      f(4)=0.018729
      f(5)=0.002152
      k(1)=0.004403799
      do i=2,ni
        k(i)=9.0*k(i-1)
      end do
      data ( (coefk(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00289,0.00289,0.00289,0.00289,0.00289,0.00289,
     *0.00289,0.00289,0.00289,0.00951,0.02582,0.05945,
     *0.11560,0.20518,0.33740,0.53335,0.81968,1.21183,
     * 1.72638,
     * 1.650E-05, 1.650E-05, 1.650E-05, 1.650E-05, 1.650E-05, 1.650E-05,
     * 1.650E-05, 1.650E-05, 1.650E-05, 5.188E-05, 1.369E-04, 2.896E-04,
     * 5.031E-04, 9.030E-04, 1.522E-03, 2.397E-03, 3.587E-03, 5.331E-03,
     * 7.600E-03,
     *-3.750E-08,-3.750E-08,-3.750E-08,-3.750E-08,-3.750E-08,-3.750E-08,
     *-3.750E-08,-3.750E-08,-3.750E-08,-4.063E-08,-1.219E-07,-3.781E-07,
     *-3.719E-07,-5.875E-07,-4.094E-07, 2.813E-07,-5.437E-07,-4.625E-07,
     *-4.156E-06/
      data ( (coefk(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.02847,0.02847,0.02847,0.02847,0.02847,0.02847,
     *0.02847,0.02847,0.02847,0.05115,0.08641,0.13784,
     *0.21072,0.31333,0.45442,0.63664,0.86713,1.14853,
     * 1.47973,
     * 1.586E-04, 1.586E-04, 1.586E-04, 1.586E-04, 1.586E-04, 1.586E-04,
     * 1.586E-04, 1.586E-04, 1.586E-04, 2.589E-04, 4.210E-04, 6.576E-04,
     * 9.754E-04, 1.372E-03, 1.881E-03, 2.501E-03, 3.177E-03, 4.101E-03,
     * 5.093E-03,
     * 1.281E-07, 1.281E-07, 1.281E-07, 1.281E-07, 1.281E-07, 1.281E-07,
     * 1.281E-07, 1.281E-07, 1.281E-07, 3.031E-07, 6.375E-07, 9.656E-07,
     * 1.559E-06, 2.259E-06, 2.956E-06, 4.541E-06, 5.334E-06, 6.506E-06,
     * 6.975E-06/
      data ( (coefk(3,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.13965,0.13965,0.13965,0.13965,0.13965,0.13965,
     *0.13965,0.13965,0.13965,0.17707,0.22719,0.29287,
     *0.37869,0.48593,0.61628,0.76703,0.92621,1.06693,
     * 1.17271,
     * 1.014E-03, 1.014E-03, 1.014E-03, 1.014E-03, 1.014E-03, 1.014E-03,
     * 1.014E-03, 1.014E-03, 1.014E-03, 1.138E-03, 1.303E-03, 1.535E-03,
     * 1.864E-03, 2.309E-03, 2.901E-03, 3.596E-03, 4.255E-03, 4.870E-03,
     * 5.700E-03,
     * 3.050E-06, 3.050E-06, 3.050E-06, 3.050E-06, 3.050E-06, 3.050E-06,
     * 3.050E-06, 3.050E-06, 3.050E-06, 3.347E-06, 3.656E-06, 4.281E-06,
     * 4.462E-06, 5.175E-06, 5.766E-06, 5.350E-06, 4.356E-06, 4.947E-06,
     * 1.191E-05/
      data ( (coefk(4,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.57831,0.57831,0.57831,0.57831,0.57831,0.57831,
     *0.57831,0.57831,0.57831,0.61983,0.67292,0.73455,
     *0.79400,0.84206,0.88828,0.93195,0.97935,1.02072,
     * 1.05737,
     * 3.927E-03, 3.927E-03, 3.927E-03, 3.927E-03, 3.927E-03, 3.927E-03,
     * 3.927E-03, 3.927E-03, 3.927E-03, 3.873E-03, 3.848E-03, 3.878E-03,
     * 4.043E-03, 4.551E-03, 5.600E-03, 6.955E-03, 8.298E-03, 9.810E-03,
     * 1.106E-02,
     * 1.002E-05, 1.002E-05, 1.002E-05, 1.002E-05, 1.002E-05, 1.002E-05,
     * 1.002E-05, 1.002E-05, 1.002E-05, 8.528E-06, 6.963E-06, 5.544E-06,
     * 7.431E-06, 1.512E-05, 1.966E-05, 2.328E-05, 2.517E-05, 3.052E-05,
     * 2.740E-05/
      data ( (coefk(5,jt,jp), jp = 1, 19), jt = 1, 3)/
     *2.17420,2.17420,2.17420,2.17420,2.17420,2.17420,
     *2.17420,2.17420,2.17420,2.10770,2.02271,1.91526,
     *1.78961,1.65009,1.48700,1.29866,1.09915,0.89800,
     * 0.71186,
     * 1.878E-02, 1.878E-02, 1.878E-02, 1.878E-02, 1.878E-02, 1.878E-02,
     * 1.878E-02, 1.878E-02, 1.878E-02, 1.869E-02, 1.860E-02, 1.841E-02,
     * 1.798E-02, 1.712E-02, 1.555E-02, 1.353E-02, 1.149E-02, 9.609E-03,
     * 7.682E-03,
     * 4.196E-05, 4.196E-05, 4.196E-05, 4.196E-05, 4.196E-05, 4.196E-05,
     * 4.196E-05, 4.196E-05, 4.196E-05, 4.297E-05, 4.334E-05, 4.307E-05,
     * 4.059E-05, 3.324E-05, 2.752E-05, 2.324E-05, 1.872E-05, 1.642E-05,
     * 1.367E-05/
	data stp / 0.251, 0.398, 0.631, 1.000, 1.58, 2.51, 
     *	             3.98, 6.31, 10.0, 15.8, 25.1, 39.8, 63.1,
     *	             100.0, 158.0, 251.0, 398.0, 631.0, 1000.0/
      do i=1,ni
      do m=1,nv
        ml=1
!        pmb(m)=p(m)
        if(pmb(m).lt.stp(1))then
         x1=coefk(i,1,1)+coefk(i,2,1)*(t(m)-250.0)
     *   +coefk(i,3,1)*(t(m)-250.0)**2
         fkg(i,m)=x1*pmb(m)/stp(1)
        else if (pmb(m).ge.stp(19)) then  !!! org .gt.
         x1=coefk(i,1,18)+coefk(i,2,18)*(t(m)-250.0)
     *   +coefk(i,3,18)*(t(m)-250.0)**2
         x2=coefk(i,1,19)+coefk(i,2,19)*(t(m)-250.0)
     *   +coefk(i,3,19)*(t(m)-250.0)**2
         fkg(i,m)=x1+(x2-x1)/(stp(19)-stp(18))
     *  *(pmb(m)-stp(18))
        else
         do while(pmb(m).ge.stp(ml))
           ml=ml+1
         end do
         x1=coefk(i,1,ml-1)+coefk(i,2,ml-1)*(t(m)-250.0)
     *   + coefk(i,3,ml-1)*(t(m)-250.0)**2
         x2=coefk(i,1,ml)+coefk(i,2,ml)*(t(m)-250.0)
     *   + coefk(i,3,ml)*(t(m)-250.0)**2
         fkg(i,m)=x1+(x2-x1)/(stp(ml)-stp(ml-1))
     *   *(pmb(m)-stp(ml-1))
        end if
      end do
      end do

      do i=1,ni
        do m=1,nv
          tau19(i,m)=k(i)*u(m)*fkg(i,m)
        end do
      end do

      return
      end
!====================================================================
!     co2/n2o 2200--2500 cm^{-1}
      subroutine ck20(nvx,nv,f,pmb,t,u,tau20)
     
      real  f(5), coefk(5,3,19),stp(19)
      real,dimension(5,nvx) :: tau20,fkg
      real ,dimension(nvx) :: k,pmb,t,u

      ni=5
      f(1)=0.458869
      f(2)=0.136554
      f(3)=0.218715
      f(4)=0.167336
      f(5)=0.018526
      k(1)=0.536896
      do i=2,ni
        k(i)=24.0*k(i-1)
      end do
      data ( (coefk(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00002,0.00007,
     *0.00027,0.00096,0.00354,0.01207,0.03663,0.08552,
     *0.15861,0.25167,0.38168,0.56692,0.82581,1.20863,
     * 1.88788,
     * 0.000E+00, 0.000E+00, 0.000E+00, 1.000E-07, 5.750E-07, 2.150E-06,
     * 6.813E-06, 2.009E-05, 5.225E-05, 1.396E-04, 4.111E-04, 7.851E-04,
     * 1.309E-03, 1.845E-03, 2.548E-03, 3.657E-03, 4.920E-03,-8.227E-03,
     * 1.188E-03,
     * 0.000E+00, 0.000E+00, 0.000E+00, 1.250E-09, 6.250E-09, 2.375E-08,
     * 6.844E-08, 1.828E-07, 2.937E-07, 8.594E-07, 2.878E-06, 5.178E-06,
     * 8.306E-06, 1.251E-05, 1.484E-05, 1.843E-05, 2.581E-05, 3.784E-04,
     * 6.279E-05/
      data ( (coefk(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00001,0.00002,0.00008,
     *0.00032,0.00134,0.00427,0.01262,0.03899,0.08948,
     *0.16530,0.26665,0.41049,0.59994,0.84701,1.15356,
     * 1.42926,
     * 0.000E+00, 0.000E+00, 0.000E+00, 3.375E-07, 9.625E-07, 2.813E-06,
     * 8.712E-06, 2.591E-05, 6.738E-05, 1.731E-04, 4.314E-04, 8.889E-04,
     * 1.428E-03, 2.146E-03, 2.979E-03, 4.081E-03, 5.356E-03,-4.375E-03,
     * 6.029E-03,
     * 0.000E+00, 0.000E+00, 0.000E+00, 4.688E-09, 1.281E-08, 3.469E-08,
     * 9.656E-08, 2.203E-07, 4.406E-07, 1.109E-06, 2.878E-06, 6.241E-06,
     * 9.200E-06, 1.479E-05, 1.835E-05, 2.261E-05, 2.854E-05, 2.972E-04,
     * 2.707E-05/
      data ( (coefk(3,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00001,0.00004,0.00012,
     *0.00036,0.00115,0.00337,0.00944,0.02921,0.08402,
     *0.16244,0.26291,0.40574,0.59750,0.84592,1.15582,
     * 1.43050,
     * 0.000E+00, 0.000E+00, 2.875E-07, 6.000E-07, 1.325E-06, 3.100E-06,
     * 7.375E-06, 1.784E-05, 4.312E-05, 1.009E-04, 2.573E-04, 5.689E-04,
     * 1.087E-03, 1.749E-03, 2.657E-03, 4.042E-03, 5.860E-03,-5.549E-03,
     * 9.583E-03,
     * 0.000E+00, 0.000E+00, 4.688E-09, 8.125E-09, 1.563E-08, 3.375E-08,
     * 6.750E-08, 1.347E-07, 2.406E-07, 4.281E-07, 1.394E-06, 1.109E-06,
     * 1.344E-06, 2.600E-06, 2.409E-06, 3.591E-06, 5.947E-06, 3.445E-04,
     * 1.157E-05/
      data ( (coefk(4,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00033,0.00032,0.00037,0.00058,0.00109,0.00216,
     *0.00429,0.00845,0.01634,0.03141,0.06305,0.12326,
     *0.21023,0.32031,0.45933,0.63241,0.85736,1.14841,
     * 1.43591,
     * 1.216E-05, 1.174E-05, 1.131E-05, 1.576E-05, 2.326E-05, 3.485E-05,
     * 5.263E-05, 7.912E-05, 1.175E-04, 1.620E-04, 2.965E-04, 4.337E-04,
     * 5.642E-04, 7.377E-04, 9.435E-04, 9.828E-04, 7.706E-04,-3.713E-03,
     *-1.563E-03,
     * 1.503E-07, 1.484E-07, 1.297E-07, 1.716E-07, 1.978E-07, 2.481E-07,
     * 3.219E-07, 4.281E-07, 5.625E-07, 4.750E-07, 1.225E-06, 1.625E-06,
     * 2.156E-06, 3.250E-06, 4.331E-06, 6.050E-06, 7.384E-06, 1.036E-04,
     * 1.005E-05/
      data ( (coefk(5,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.17603,0.15775,0.15166,0.15930,0.18058,0.21548,
     *0.27159,0.35027,0.45092,0.58099,0.78768,1.05426,
     *1.21613,1.26776,1.26045,1.19035,1.06528,0.91460,
     * 0.72367,
     * 1.507E-03, 1.481E-03, 1.325E-03, 1.343E-03, 1.360E-03, 1.377E-03,
     * 1.388E-03, 1.331E-03, 1.238E-03, 9.580E-04, 1.403E-03, 1.144E-03,
     * 6.321E-04, 1.536E-04,-9.625E-06, 3.740E-04, 5.853E-04, 2.204E-03,
     *-2.916E-04,
     * 6.019E-06, 6.538E-06, 6.510E-06, 6.969E-06, 5.497E-06, 5.913E-06,
     * 4.419E-06, 3.403E-06, 9.906E-07,-4.681E-06, 2.534E-06, 2.653E-06,
     * 5.597E-06, 8.784E-06, 9.064E-08,-5.625E-06,-3.094E-06,-4.828E-05,
     *-2.031E-07/
	data stp / 0.251, 0.398, 0.631, 1.000, 1.58, 2.51, 
     *	             3.98, 6.31, 10.0, 15.8, 25.1, 39.8, 63.1,
     *	             100.0, 158.0, 251.0, 398.0, 631.0, 1000.0/
      do i=1,ni
      do m=1,nv
        ml=1
!        pmb(m)=p(m)*1013.25
        if(pmb(m).lt.stp(1))then
         x1=coefk(i,1,1)+coefk(i,2,1)*(t(m)-250.0)
     *   +coefk(i,3,1)*(t(m)-250.0)**2
         fkg(i,m)=x1*pmb(m)/stp(1)
        else if (pmb(m).ge.stp(19)) then !! org .gt.
         x1=coefk(i,1,18)+coefk(i,2,18)*(t(m)-250.0)
     *   +coefk(i,3,18)*(t(m)-250.0)**2
         x2=coefk(i,1,19)+coefk(i,2,19)*(t(m)-250.0)
     *   +coefk(i,3,19)*(t(m)-250.0)**2
         fkg(i,m)=x1+(x2-x1)/(stp(19)-stp(18))
     *  *(pmb(m)-stp(18))
        else
         do while(pmb(m).ge.stp(ml))
           ml=ml+1
         end do
         x1=coefk(i,1,ml-1)+coefk(i,2,ml-1)*(t(m)-250.0)
     *   + coefk(i,3,ml-1)*(t(m)-250.0)**2
         x2=coefk(i,1,ml)+coefk(i,2,ml)*(t(m)-250.0)
     *   + coefk(i,3,ml)*(t(m)-250.0)**2
         fkg(i,m)=x1+(x2-x1)/(stp(ml)-stp(ml-1))
     *   *(pmb(m)-stp(ml-1))
        end if
      end do
      end do

      do i=1,ni
        do m=1,nv
          tau20(i,m)=k(i)*u(m)*fkg(i,m)
        end do
      end do

      return
      end
c *********************************************************************
      subroutine n2cont (amnt,twin,patm,temp,fac)
c     Parameterization of CKD_2.1 continuum over 2200 to 2500 cm-1 band
c     Fit inaccurate for amnt < 0.01 g/cm*2;
c          however optical depths < 0.001.
c     INPUT:
c     amnt = h2O layer amount (g/cm**2)
c     patm= pressure (atm)
c     temp = temperature (k)
c     OUTPUT:
c     twin = parameterized CKD_2.1 optical depth over band
c
      parameter (ncoef=8)
      real aa(ncoef)
      data aa/-12.23,0.9995,-0.003916,-3.364,
     *         6.360,5.881e-04,-1.001,10.18/
c
      ph2o = amnt*(8.314e+07*temp)/(fac*1.0e+05*18.01534*1.01325e+06)
c
          tau_log   = aa(1)              +
     *                aa(2) * log(amnt)  +
     *                aa(3) * temp       +
     *                aa(4) * patm       +
     *                aa(5) * ph2o       +
     *                aa(6) * amnt       +
     *                aa(7) * log(ph2o)  +
     *                aa(8) * patm**0.5

      twin = exp ( tau_log )
c
      return
      end
!===============================================================================      
	subroutine ck_dkfr_win_hyb(ib,iga, hka,tg)
        USE FUINPUT
 	parameter (maxig=5,ngas=6,maxjp=10)

	 real fkg(nv1x)
        common /band11/ hk11(3),c11h2o(3,19,3),c11ch4(3,19),c11n2o(3,19)
        common /band12/ hk12(5), c12o3(3,19,5), c12h2o(3,19)
        common /band13/ hk13(2), c13h2o(3,19,2)

	real tg(nvx)

	real tgall(nvx)

	integer,save ::ngv(14:16,6),ng(6)

	integer,save,dimension(maxjp,14:16) ::
     &    igs1,igs2,igs3,igs4,igs5,igs6
	real,save :: tgx(nvx,maxig,ngas),hkx(maxig,ngas)
	character*5 glab(6)
	data glab/'H2O','O3','CO2','N2O','CH4','CFC'/
c ( 14 = 1250 - 1100 cm**-1 )
c ( 15 = 1100 -  980 cm**-1 )
c ( 16 =  980 -  800 cm**-1 )

		ngv(14:16,1)  = (/5,1,2/) ! H20 KRATZ/Fu   /Fu
		ngv(14:16,2)  = (/1,5,1/) ! O3  KRATZ/Fu/KRATZ
		ngv(14:16,3)  = (/0,1,1/) ! CO2(thin)/KRATZ/KRATZ
		ngv(14:16,4)  = (/1,0,0/) ! N2O Fu   /Fu   /Fu
		ngv(14:16,5)  = (/1,0,0/) ! CH4 Fu   /Fu   /Fu
		ngv(14:16,6)  = (/1,1,1/) ! CFC KRATZ
! 5,5,2=12
!-------------------------------------------------------------------
! 
	if (iga == 1) then
	hkx=1.000
	tgx=0.000
	
	do kgas=1,ngas
	if( ngv(ib,kgas) == 0 ) cycle
! 	 print*, 'HYB',glab(kgas)
	do ig=1,ngv(ib,kgas)
	
	if(kgas ==1 ) then
	 if (ib == 15 ) then
	  call qki ( c12h2o, fkg )
          call qoph2o ( fkg, tgx(1,ig,1) )
	  hkx(ig,1 ) =1.0
	 elseif (ib == 16 ) then
	    call qki ( c13h2o(1,1,ig), fkg )
            call qoph2o ( fkg, tgx(1,ig,1)  )
	    hkx(ig,1 ) = hk13(ig)
	 elseif (ib == 14 ) then
	  call ck_dkfr_win_h2o(ib,ig, hkx(ig,1 ),tgx(1,ig,1) )
	 endif

	elseif (kgas ==2 ) then
	if (ib == 14 ) then
	 call ck_dkfr_win_o3_1k (ib,ig, hkx(ig,2 ),tgx(1,ig,2) )

	 elseif (ib == 15  ) then
          call qkio3 ( c12o3(1,1,ig), fkg )
          call qopo3i ( fkg, tgx(1,ig,2) )
	  hkx(ig,2 )= hk12(ig)

	 elseif ( ib == 16 ) then
	 call ck_dkfr_win_o3 (ib,ig, hkx(ig,2 ),tgx(1,ig,2) )
	endif

	elseif (kgas ==3 ) then
	call ck_dkfr_win_co2_thin(ib,ig, hkx(ig,3 ),tgx(1,ig,3) )

	elseif (kgas ==4 ) then
	 
          call qki ( c11n2o, fkg )
          call qopn2o ( fkg, tgx(1,ig,4) )
	  tgx(1:nv,ig,4) = tgx(1:nv,ig,4)/0.28*umn2o
	  hkx(ig,4 ) =1.0
	
!!	call ck_dkfr_win_n2o(ib,ig, hkx(ig,4 ),tgx(1,ig,4) )

	elseif (kgas ==5 ) then
	
	   call qki ( c11ch4, fkg )
           call qopch4( fkg, tgx(1,ig,5) )
	   tgx(1:nv,ig,5) = tgx(1:nv,ig,5)/1.6*umch4
	   hkx(ig,5 ) =1.0
           
!!	   call ck_dkfr_win_ch4(ib,ig, hkx(ig,5 ),tgx(1,ig,5) )

	elseif (kgas ==6 ) then
	call ck_dkfr_win_cfc(ib,ig, hkx(ig,6 ),tgx(1,ig,6) )
	else
	endif
!	print*, tgx(1:nv,ig,kgas)
	enddo
!	print*, hkx(1:ngv(ib,kgas),kgas )
	
	enddo
	
	kk=0
	sumhka1=0
	do kgas=1,ngas
	ng(kgas)=ngv(ib,kgas)
	if(ngv(ib,kgas) == 0 ) ng(kgas)=1
	enddo 
	 tgsumw=0
	 do ig1=1,ng(1)
	 do ig2=1,ng(2)
	 do ig3=1,ng(3)
	 do ig4=1,ng(4)
	 do ig5=1,ng(5)
	 do ig6=1,ng(6)
	 kk=kk+1
	igs1(kk,ib)=ig1
	igs2(kk,ib)=ig2
	igs3(kk,ib)=ig3
	igs4(kk,ib)=ig4
	igs5(kk,ib)=ig5
	igs6(kk,ib)=ig6

	hka1 =hkx(ig1,1)*hkx(ig2,2)*hkx(ig3,3)*
     &        hkx(ig4,4)*hkx(ig5,5)*hkx(ig6,6)
	sumhka1=sumhka1+hka1
	tgall=0.0
	
	tgall(1:nv) =  
     &            tgx(1:nv,ig1,1)+ tgx(1:nv,ig2,2) + tgx(1:nv,ig3,3)
     &          + tgx(1:nv,ig4,4)+ tgx(1:nv,ig5,5) + tgx(1:nv,ig6,6)
	tgsum=sum( tgall(1:nv))
	tgsumw=tgsumw+ sum( tgall(1:nv)*hka1 )

!	 print'(I5,6I2,6f8.5,2f10.5,2x,4f8.4)'
!     &       ,kk,ig1,ig2,ig3,ig4,ig5,ig6,
!     &         hkx(ig1,1),hkx(ig2,2),hkx(ig3,3),
!     &         hkx(ig4,4),hkx(ig5,5),hkx(ig6,6),
!     &		hka1,sumhka1,tgsum,tgsumw
	
	 enddo;enddo;enddo;enddo;enddo;enddo
	 
	endif ! iga =1
c----------------------------------------------------------------------

	hka = 	hkx(igs1(iga,ib),1)*
     &	      	hkx(igs2(iga,ib),2)*
     &	      	hkx(igs3(iga,ib),3)*
     &	      	hkx(igs4(iga,ib),4)*
     &	      	hkx(igs5(iga,ib),5)*
     &	      	hkx(igs6(iga,ib),6)

	tg(1:nv) =  
     &		tgx(1:nv,igs1(iga,ib),1)+
     &		tgx(1:nv,igs2(iga,ib),2)+
     &		tgx(1:nv,igs3(iga,ib),3)+
     &		tgx(1:nv,igs4(iga,ib),4)+
     &		tgx(1:nv,igs5(iga,ib),5)+
     &		tgx(1:nv,igs6(iga,ib),6)

	return
	end
c *********************************************************************
      subroutine ck_dkfr_win_co2_thin(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 11 = 1250 - 1100 cm**-1 )
c ( 12 = 1100 -  980 cm**-1 )
c ( 13 =  980 -  800 cm**-1 )
c 
c ig = Spectral Division
c BAND 13 has 1
c BAND 12 has 1
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
 
   
      real tg(nvx),u(nv1x)
 
!      common /umcon/ umco2, umch4, umn2o 


       do m=1,nv
!!	dz= gpt(pp(m+1),pp(m),pt(m+1),pt(m),ph(m+1),ph(m))
!!! CO2 UNITS
!       u(m)=pp(m)*1.01325e+06/(1.3805e-16*pt(m))*3.50e-04*
!     *          1.0e+05/6.023e+23*44.00995

       u(m)=pp(m)*1.0e+03/(1.3805e-16*pt(m))* umco2* 1.0e-06*dz(m)*
     *          1.0e+05/ 6.023e+23 *44.00995
	enddo

	if (ig >1) fi%ierror = 33!print*, ' co2thin ig>1'
	tg(1:nv)=0.0

	if  (ib == 15 ) then
	     hk =1 
             rk1=0.03479137
             do m=1,nv
             tg(m)=0.837764*rk1*(1.0000+3.4268e-02*(pt(m)-250.0)+
     &             3.7401e-04*(pt(m)-250.0)**2)*u(m)
            end do

	elseif (ib==16) then
	     hk=1.0
      	     rk1=0.01407591
      	     do m=1,nv
             tg(m)=0.865828*rk1*(1.0000+3.7154e-02*(pt(m)-250.0)+
     &             4.3205e-04*(pt(m)-250.0)**2)*u(m)
	     enddo
	endif



	return
	end
c *********************************************************************
      subroutine ck_dkfr_win_o3_1k(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 11 = 1250 - 1100 cm**-1 )
c 
c ig = Spectral Division
c BAND 11 has 1 
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
 
   
      real tg(nvx), u(nv1x)
 

c     o3 1100-1250 cm^{-1} BAND 11 

      hk=1.000000
      rk1=21.305812
	tg(1:nv)=0.0
	if ( ig > 1 ) fi%ierror = 34!print*, ' ig >1 o3 '
	if (ib == 14 ) then
       do m=1,nv
!!! O3 UNITS
	u(m) = 1.02 *(po(m)+po(m+1))*0.5 * (pp(m+1) - pp(m))

        tg(m)=0.778043*rk1*(1.0000+1.4500e-03*(pt(m)-250.0)+
     &           6.3265e-06*(pt(m)-250.0)**2)*u(m)
       enddo
	endif
      return
      end
c *********************************************************************
!================================================================
!================================================================
	subroutine ck_dkfr_win_all(ib,iga, hka,tg)
        USE FUINPUT
	parameter (maxig=5,ngas=6,maxjp=80)

!        common /atmos/ pp(nv1x), pt(nv1x), ph(nv1x), po(nv1x)
!        common /umcon/ umco2, umch4, umn2o 
	real tg(nvx)

	real tgall(nvx)

	integer,save ::ngvkr(14:16,6),ng(6)
	integer,save,dimension(maxjp,14:16) ::
     &    igs1,igs2,igs3,igs4,igs5,igs6
	real,save :: tgx(nvx,maxig,ngas),hkx(maxig,ngas)
	character*5 glab(6)
	data glab/'H2O','O3','CO2','N2O','CH4','CFC'/

		ngvkr(14:16,1)  = (/5,4,5/) ! H20 KRATZ
		ngvkr(14:16,2)  = (/2,5,1/) ! O3 KRATZ
		ngvkr(14:16,3)  = (/0,2,2/) ! CO2 KRATZ
		ngvkr(14:16,4)  = (/2,0,0/) ! N2O KRATZ
		ngvkr(14:16,5)  = (/4,0,0/) ! CH4 KRATZ
		ngvkr(14:16,6)  = (/1,1,1/) ! CFC KRATZ
!-------------------------------------------------------------------
! 
	if (iga == 1) then
	hkx=1.000
	tgx=0.000
	
	do kgas=1,ngas
	if( ngvkr(ib,kgas) == 0 ) cycle
! 	 print*, 'Kratz',glab(kgas)
	do ig=1,ngvkr(ib,kgas)
	
	if(kgas ==1 ) then
	call ck_dkfr_win_h2o(ib,ig, hkx(ig,1 ),tgx(1,ig,1) )
	elseif (kgas ==2 ) then
	call ck_dkfr_win_o3 (ib,ig, hkx(ig,2 ),tgx(1,ig,2) )
	elseif (kgas ==3 ) then
	call ck_dkfr_win_co2(ib,ig, hkx(ig,3 ),tgx(1,ig,3) )
	elseif (kgas ==4 ) then
	call ck_dkfr_win_n2o(ib,ig, hkx(ig,4 ),tgx(1,ig,4) )
	elseif (kgas ==5 ) then
	call ck_dkfr_win_ch4(ib,ig, hkx(ig,5 ),tgx(1,ig,5) )
	elseif (kgas ==6 ) then
	call ck_dkfr_win_cfc(ib,ig, hkx(ig,6 ),tgx(1,ig,6) )
	else
	endif
!	print*, tgx(1:nv,ig,kgas)
	enddo
!	print*, hkx(1:ngvkr(ib,kgas),kgas )
	
	enddo
	
	kk=0
	sumhka1=0
	do kgas=1,ngas
	ng(kgas)=ngvkr(ib,kgas)
	if(ngvkr(ib,kgas) == 0 ) ng(kgas)=1
	enddo 
	 tgsumw=0
	 do ig1=1,ng(1)
	 do ig2=1,ng(2)
	 do ig3=1,ng(3)
	 do ig4=1,ng(4)
	 do ig5=1,ng(5)
	 do ig6=1,ng(6)
	 kk=kk+1
	igs1(kk,ib)=ig1
	igs2(kk,ib)=ig2
	igs3(kk,ib)=ig3
	igs4(kk,ib)=ig4
	igs5(kk,ib)=ig5
	igs6(kk,ib)=ig6

	hka1 =hkx(ig1,1)*hkx(ig2,2)*hkx(ig3,3)*
     &        hkx(ig4,4)*hkx(ig5,5)*hkx(ig6,6)
	sumhka1=sumhka1+hka1
	tgall=0.0
	
	tgall(1:nv) =  
     &            tgx(1:nv,ig1,1)+ tgx(1:nv,ig2,2) + tgx(1:nv,ig3,3)
     &          + tgx(1:nv,ig4,4)+ tgx(1:nv,ig5,5) + tgx(1:nv,ig6,6)
	tgsum=sum( tgall(1:nv))
	tgsumw=tgsumw+ sum( tgall(1:nv)*hka1 )

!	 print'(I5,6I2,6f8.5,2f10.5,2x,4f8.4)'
!     &       ,kk,ig1,ig2,ig3,ig4,ig5,ig6,
!     &         hkx(ig1,1),hkx(ig2,2),hkx(ig3,3),
!     &         hkx(ig4,4),hkx(ig5,5),hkx(ig6,6),
!     &		hka1,sumhka1,tgsum,tgsumw
	
	 enddo;enddo;enddo;enddo;enddo;enddo
	 
	endif ! iga =1
c----------------------------------------------------------------------

	hka = 	hkx(igs1(iga,ib),1)*
     &	      	hkx(igs2(iga,ib),2)*
     &	      	hkx(igs3(iga,ib),3)*
     &	      	hkx(igs4(iga,ib),4)*
     &	      	hkx(igs5(iga,ib),5)*
     &	      	hkx(igs6(iga,ib),6)

	tg(1:nv) =  
     &		tgx(1:nv,igs1(iga,ib),1)+
     &		tgx(1:nv,igs2(iga,ib),2)+
     &		tgx(1:nv,igs3(iga,ib),3)+
     &		tgx(1:nv,igs4(iga,ib),4)+
     &		tgx(1:nv,igs5(iga,ib),5)+
     &		tgx(1:nv,igs6(iga,ib),6)

	return
	end
c *********************************************************************
      subroutine ck_dkfr_win_h2o(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 14 = 1250 - 1100 cm**-1 )
c ( 15 = 1100 -  980 cm**-1 )
c ( 16 =  980 -  800 cm**-1 )
c 
c ig = Spectral Division
c BAND 16 has 1:5 , 15 has 1:4 14 has 1:5
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
 
   
      real tg(nvx), u(nv1x),coefk(3,19)
 

	common /band13dkfr/ hk13h2o(5),c13h2o(5,3,19) 
	common /band12dkfr/ hk12h2o(4),c12h2o(4,3,19)
	common /band11dkfr/ hk11h2o(5),c11h2o(5,3,19)

	if(ib==16) then
	 ni=5
	 rk1=0.00207622908
	 rk=6.0
	 hk=hk13h2o(ig)
	 coefk(1:3,1:19)=c13h2o(ig,1:3,1:19)
		elseif( ib == 15) then
	 ni=4
	 rk1= 0.00238376099 
	 rk = 10.0
	 hk=hk12h2o(ig)
	 coefk(1:3,1:19)=c12h2o(ig,1:3,1:19)
		elseif( ib == 14) then
	 ni=5
	 rk1= 0.004186672 
	 rk= 9.0
	 hk=hk11h2o(ig)
	 coefk(1:3,1:19)=c11h2o(ig,1:3,1:19)
	else
	 fi%ierror = 35 !print*, ' bad band ib not 11,12,13'
	endif
	if(ig>ni)fi%ierror = 36 ! print*, ' BAD ig'

	do m=1,nv
	u(m) = 1.02 *(ph(m)+ph(m+1))*0.5 * (pp(m+1) - pp(m))
	enddo

	call stp_correction(ig,rk1,rk,coefk,u,tg)

      return
      end

c===============================================================


c *********************************************************************
      subroutine ck_dkfr_win_o3(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 14 = 1250 - 1100 cm**-1 )
c ( 15 = 1100 -  980 cm**-1 )
c ( 16 =  980 -  800 cm**-1 )
c 
c ig = Spectral Division
c BAND 11 has 1 , 12 has 5 , 11 has 1:2
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
 
   
      real tg(nvx), u(nv1x),coefk(3,19)
 

! OZONE
	common /band13dkfr_o3/ hk13o3(1),c13o3(1,3,19)
	common /band12dkfr_o3/ hk12o3(5),c12o3(5,3,19)
	common /band11dkfr_o3/ hk11o3(2),c11o3(2,3,19)

	if(ib==16) then
	 ni=1
	 rk1=1.0
	 rk=1.0
	 hk=hk13o3(1)
  	  do jp=1,19
	  coefk(1:3,jp)=c13o3(1,1:3,1)
	  enddo
		elseif( ib == 15) then
	 ni=5
	 rk1= 9.172445 
	 rk = 9.0
	 hk=hk12o3(ig)
	 coefk(1:3,1:19)=c12o3(ig,1:3,1:19)
		elseif( ib == 14) then
	  ni=2
	  rk1=11.0838052
	  rk=36.0
	  hk=hk11o3(ig)
	  coefk(1:3,1:19)=c11o3(ig,1:3,1:19)
	else
	fi%ierror = 37!print*, ' bad band ib not 14,15,16'
	endif

	if(ig>ni) fi%ierror = 38!print*, ' BAD ig'

        do m=1,nv
!!! O3 UNITS
	u(m) = 1.02 *(po(m)+po(m+1))*0.5 * (pp(m+1) - pp(m))
	enddo

	call stp_correction(ig,rk1,rk,coefk,u,tg)

	return
	end

c *********************************************************************
      subroutine ck_dkfr_win_ch4(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 14 = 1250 - 1100 cm**-1 )
c 
c ig = Spectral Division
c BAND  11 has 2
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
 
   
      real tg(nvx), u(nv1x),coefk(3,19)
 
!      common /umcon/ umco2, umch4, umn2o 
!CH4
      common /band11dkfr_ch4/ hk11ch4(4),c11ch4(4,3,19)

	if(ib==14) then

	  ni=4
	  rk1=4.289433
	  rk=8.0
	  hk=hk11ch4(ig)
	  coefk(1:3,1:19)=c11ch4(ig,1:3,1:19)
	elseif( ib == 15 .or. ib == 16) then
	  tg=0
	  return

	else
	fi%ierror = 39!print*, ' bad band ib not 11,12,13'
	endif

	if(ig>ni) fi%ierror = 40!print*, ' BAD ig'

        do m=1,nv
!	dz= gpt(pp(m+1),pp(m),pt(m+1),pt(m),ph(m+1),ph(m))

!!! CH4 UNITS
!!        uch4(m)=p(m)*1.01325e+06/(1.3805e-16*t(m))*1.75e-06*fac(m)*
!!     *          1.0e+05/6.023e+23*16.04303

        u(m)=pp(m)*1.0e+03/(1.3805e-16*pt(m))*umch4*1.0e-06*dz(m)*
     *          1.0e+05/6.023e+23*16.04303
	enddo

        call stp_correction(ig,rk1,rk,coefk,u,tg)

	return
	end
c *********************************************************************
      subroutine ck_dkfr_win_n2o(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 14 = 1250 - 1100 cm**-1 )
c 
c ig = Spectral Division
c BAND  11 has 2
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
 
   
      real tg(nvx),u(nv1x),coefk(3,19)
 
!      common /umcon/ umco2, umch4, umn2o 

!N2O
	common /band11dkfr_n2o/ hk11n2o(2),c11n2o(2,3,19)

	if(ib==14) then

	  ni=2
	  rk1= 15.071363
	  rk=23.0
	  hk=hk11n2o(ig)
	  coefk(1:3,1:19)=c11n2o(ig,1:3,1:19)

	elseif( ib == 15 .or. ib == 16) then
	  tg=0
	  return
	else
	  fi%ierror = 41!print*, ' bad band ib not 11,12,13'
	endif

	if(ig>ni) fi%ierror = 42!print*, ' BAD ig'

        do m=1,nv
!!	dz= gpt(pp(m+1),pp(m),pt(m+1),pt(m),ph(m+1),ph(m))
!!! N2O UNITS
!!!      un2o(m)=p(m)*1.01325e+06/(1.3805e-16*t(m))*3.10e-07*fac(m)*
!!!     *          1.0e+05/6.023e+23*44.0128

         u(m)=pp(m)*1.0e+03/(1.3805e-16*pt(m))*umn2o *1.0e-06*dz(m)*
     *          1.0e+05/6.023e+23*44.0128

	enddo

        call stp_correction(ig,rk1,rk,coefk,u,tg)

	return
	end

c *********************************************************************
      subroutine ck_dkfr_win_co2(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 14 = 1250 - 1100 cm**-1 )
c ( 15 = 1100 - 980 cm**-1 )
c 
c ig = Spectral Division
c BAND 13 has 3
c BAND 12 has 2
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
 
   
      real tg(nvx),u(nv1x),coefk(3,19)
 
!      common /umcon/ umco2, umch4, umn2o 

!CO2
	common /band13dkfr_co2/ hk13co2(2),c13co2(2,3,19)
	common /band12dkfr_co2/ hk12co2(2),c12co2(2,3,19)

	if(ib==16) then

	  ni=2
	  rk1= 0.0059926849
	  rk=60.
	  hk=hk13co2(ig)
	  coefk(1:3,1:19)=c13co2(ig,1:3,1:19)

	elseif(ib==15) then
	  ni=2
	  rk1=0.0130594055
	  rk=44.0
	  hk=hk12co2(ig)
	  coefk(1:3,1:19)=c12co2(ig,1:3,1:19)
	elseif( ib == 14) then
	  tg=0
	  return
	else
	fi%ierror = 43!print*, ' bad band ib not 11,12,13'
	endif

	if(ig>ni) fi%ierror = 44!print*, ' BAD ig'

       do m=1,nv
!!	dz= gpt(pp(m+1),pp(m),pt(m+1),pt(m),ph(m+1),ph(m))
!!! CO2 UNITS
!       u(m)=pp(m)*1.01325e+06/(1.3805e-16*pt(m))*3.50e-04*
!     *          1.0e+05/6.023e+23*44.00995

       u(m)=pp(m)*1.0e+03/(1.3805e-16*pt(m))* umco2* 1.0e-06*dz(m)*
     *          1.0e+05/ 6.023e+23 *44.00995
	enddo

        call stp_correction(ig,rk1,rk,coefk,u,tg)

	return
	end
c---------------------------------------------------------------
c===============================================================
      subroutine stp_correction(ig,rk1,rk,coefk,ug,tg)
      USE FUINPUT
 
   
      real tg(nvx),ug(nv1x),coefk(3,19)
 

   	real k(100),fkg(nv1x)
	
    	real stp(19)
	data stp / 0.251, 0.398, 0.631, 1.000, 1.58, 2.51, 
     *	             3.98, 6.31, 10.0, 15.8, 25.1, 39.8, 63.1,
     *	             100.0, 158.0, 251.0, 398.0, 631.0, 1000.0/

c - - - - - - - - - - - - - - - - - - -
	k(1)=rk1
        do iii=2,ig
        k(iii)=rk * k(iii-1)
!	print*,iii,k1,k(iii)
        end do

        do m=1,nv
!!	ug(m) = 1.02 *(ph(m)+ph(m+1))*0.5 * (pp(m+1) - pp(m))
        ml=1

        if(pp(m).lt.stp(1))then
         x1=coefk(1,1)+coefk(2,1)*(pt(m)-250.0)
     *   +coefk(3,1)*(pt(m)-250.0)**2
         fkg(m)=x1*pp(m)/stp(1)
        else if (pp(m).ge.stp(19)) then  !!!Org .gt.
         x1=coefk(1,18)+coefk(2,18)*(pt(m)-250.0)
     *   +coefk(3,18)*(pt(m)-250.0)**2
         x2=coefk(1,19)+coefk(2,19)*(pt(m)-250.0)
     *   +coefk(3,19)*(pt(m)-250.0)**2
         fkg(m)=x1+(x2-x1)/(stp(19)-stp(18))
     *  *(pp(m)-stp(18))
        else
         do while(pp(m).ge.stp(ml))
           ml=ml+1
         end do
         x1=coefk(1,ml-1)+coefk(2,ml-1)*(pt(m)-250.0)
     *   + coefk(3,ml-1)*(pt(m)-250.0)**2
         x2=coefk(1,ml)+coefk(2,ml)*(pt(m)-250.0)
     *   + coefk(3,ml)*(pt(m)-250.0)**2
         fkg(m)=x1+(x2-x1)/(stp(ml)-stp(ml-1))
     *   *(pp(m)-stp(ml-1))
        end if

          tg(m)=k(ig)*ug(m)*fkg(m)

      end do

!        do m=1,nv
!        tg(m)=k(ig)*ug(m)*fkg(m)
!	 print'(3i3,3f10.7)',ib,ig,m,k(ig),u(m),fkg(m)
!        end do
      return
      end
c---------------------------------------------------------------
	real function gpt(p1,p2,t1,t2,q1,q2)
	
	qbar=(q1+q2)*0.5
	tbar=(t1+t2)*0.5
	avt = tbar*(1.0+0.61*qbar)
	h = .001*29.3* avt
	gpt =  h * log(p1 / p2) 
	return
	end

c===============================================================
c *********************************************************************
      subroutine ck_dkfr_win_cfc(ib,ig, hk,tg)
c INPUT: 
c ib = FU BAND ID 
c ( 11 = 1250 - 1100 cm**-1 )
c ( 12 = 1100 -  980 cm**-1 )
c ( 13 =  980 -  800 cm**-1 )
c 
c ig = Spectral Division
c BAND 11 has 1:5 , 12 has 1:4 11 has 1:5
c
c OUTPUT:
c hk = Cumulative Probability Value
c tg(1:nv) = Optical Depth for level 
c-----------------------------------------------------------------------
      USE FUINPUT
   
      real tg(nvx)
 
!      common /cfcs/ cfc_conc(3)
	real cfcc(3,3,14:16),corc(3,14:16),rat(3,14:16), cfcmw(3)
	real uf(3),fk(3)

	data cfcmw/137.3685,120.9139,86.4689/

        data cfcc /	
     &			37.940  ,9.9010e-03 ,4.2008e-05 ,
     &   		1822.228,2.1404e-04 ,4.4274e-06 ,
     &			2698.829,-1.8124e-04,6.7193e-07 ,

     &			891.794, 1.9494e-04 ,7.0829e-06 ,
     & 			800.771, 1.3996e-03 ,1.7760e-06 ,
     &			417.009, 2.8908e-03 ,7.1388e-06 ,

     &			5611.777,1.2661e-03 ,3.5594e-06 ,
     & 			2250.491,8.7737e-04 ,5.8844e-06 ,
     &			1295.441,9.6513e-04 ,1.3128e-05 /
  
	data corc / 	1.110020 , 1.091854 , 1.111199 ,
     &			0.891109 , 0.869315 , 0.902223 ,
     &			0.277778 , 0.666667 , 0.555556 /

	data rat / 	1.0	, 1.0	    , 1.0      ,
     &			1.0	, 1.0	    , 1.0      ,
     &			0.277778,0.666667   , 0.555556 /


	if (ib < 14 .or. ib > 16 ) fi%ierror = 45!print*,

	hk=1
	ni=1
	if(ig>ni) fi%ierror = 46!print*, ' BAD ig'

	do m=1,nv
!!	dz= gpt(pp(m+1),pp(m),pt(m+1),pt(m),ph(m+1),ph(m))

	do ifc=1,3
	uf(ifc) =pp(m)*1.0e+03/(1.3805e-16*pt(m))*cfc_conc(ifc)*dz(m)*
     *          1.0e+05/6.023e+23*cfcmw(ifc)

	fk(ifc)=corc(ifc,ib)*rat(ifc,ib)* cfcc(1,ifc,ib)*
     *  (1.0000+ cfcc(2,ifc,ib)*(pt(m)-250.0)+
     *   cfcc(3,ifc,ib) *(pt(m)-250.0)**2)

	enddo

        tg(m)=fk(1)*uf(1) + fk(2)*uf(2) +fk(3)*uf(3)

      end do

	return
	end



c---------------------------------------------------------------
	block data dkfr13
	common /band13dkfr/ hk13h2o(5),c13h2o(5,3,19)

      data hk13h2o / 0.874946 , 0.028572 , 0.067003 ,
     &  0.021822 , 0.007657/

      data ( (c13h2o(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *0.00000,0.00013,0.00105,0.00509,0.01786,0.04702,
     *0.09765,0.18006,0.31177,0.51197,0.81269,1.25906,
     *1.90230,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *0.000E+00,5.375E-06,4.287E-05,2.076E-04,6.810E-04,1.616E-03,
     *3.120E-03,5.532E-03,9.357E-03,1.515E-02,2.361E-02,3.602E-02,
     *5.343E-02,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *0.000E+00,6.812E-08,5.406E-07,2.611E-06,8.206E-06,1.806E-05,
     *3.347E-05,5.794E-05,9.710E-05,1.560E-04,2.385E-04,3.588E-04,
     *5.217E-04/
      data ( (c13h2o(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *0.00011,0.00129,0.00602,0.02093,0.05523,0.09989,
     *0.15814,0.24711,0.37581,0.56387,0.83740,1.20265,
     *1.67662,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *4.513E-06,5.184E-05,2.485E-04,7.961E-04,1.801E-03,3.121E-03,
     *4.914E-03,7.687E-03,1.156E-02,1.708E-02,2.491E-02,3.588E-02,
     *4.880E-02,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *5.406E-08,6.447E-07,3.169E-06,9.547E-06,1.889E-05,3.294E-05,
     *5.286E-05,8.252E-05,1.228E-04,1.793E-04,2.563E-04,3.768E-04,
     *5.087E-04/
      data ( (c13h2o(3,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00014,0.00094,
     *0.00394,0.01208,0.02774,0.05169,0.08407,0.12949,
     *0.19531,0.29073,0.42682,0.61650,0.86541,1.17320,
     *1.52430,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,5.388E-06,3.800E-05,
     *1.634E-04,4.536E-04,9.603E-04,1.688E-03,2.688E-03,4.092E-03,
     *6.089E-03,8.962E-03,1.313E-02,1.890E-02,2.647E-02,3.556E-02,
     *4.414E-02,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,6.156E-08,4.769E-07,
     *2.089E-06,5.409E-06,1.083E-05,1.848E-05,2.926E-05,4.432E-05,
     *6.505E-05,9.451E-05,1.386E-04,1.993E-04,2.806E-04,3.767E-04,
     *4.511E-04/
      data ( (c13h2o(4,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00120,0.00128,0.00242,0.00553,0.01117,0.01998,
     *0.03244,0.04899,0.07138,0.10317,0.15001,0.21618,
     *0.30731,0.42906,0.57909,0.75300,0.92892,1.08421,
     *1.18233,
     *1.316E-04,1.392E-04,1.634E-04,2.889E-04,5.016E-04,7.824E-04,
     *1.160E-03,1.660E-03,2.346E-03,3.319E-03,4.714E-03,6.728E-03,
     *9.492E-03,1.318E-02,1.765E-02,2.217E-02,2.616E-02,2.976E-02,
     *3.187E-02,
     *2.578E-06,2.721E-06,2.733E-06,4.265E-06,6.791E-06,9.797E-06,
     *1.375E-05,1.903E-05,2.647E-05,3.687E-05,5.102E-05,7.200E-05,
     *1.007E-04,1.398E-04,1.872E-04,2.286E-04,2.576E-04,2.814E-04,
     *2.908E-04/
      data ( (c13h2o(5,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.77726,1.77579,1.77489,1.77406,1.77231,1.76948,
     *1.76735,1.76208,1.75493,1.74598,1.72806,1.69572,
     *1.63730,1.54934,1.42998,1.28142,1.10596,0.91165,
     *0.71913,
     *5.213E-02,5.209E-02,5.203E-02,5.197E-02,5.187E-02,5.172E-02,
     *5.157E-02,5.126E-02,5.082E-02,5.023E-02,4.936E-02,4.807E-02,
     *4.614E-02,4.337E-02,3.979E-02,3.564E-02,3.083E-02,2.559E-02,
     *2.038E-02,
     *5.478E-04,5.475E-04,5.464E-04,5.454E-04,5.440E-04,5.420E-04,
     *5.393E-04,5.352E-04,5.289E-04,5.206E-04,5.093E-04,4.933E-04,
     *4.725E-04,4.419E-04,4.043E-04,3.635E-04,3.168E-04,2.693E-04,
     *2.206E-04/

! OZONE
	common /band13dkfr_o3/ hk13o3(1),c13o3(1,3,19)

	data  hk13o3 / 1.0/

	data (c13o3(1,jt,1),jt=1,3)
     * /1.246705,2.6904e-02,2.7135e-04/

!CO2
	common /band13dkfr_co2/ hk13co2(2),c13co2(2,3,19)

	data hk13co2 / 0.972025, 0.027975 /

      data ( (c13co2(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00045,0.00099,0.00210,0.00438,0.00860,0.01503,
     *0.02430,0.03752,0.05664,0.08348,0.12152,0.17345,
     *0.24203,0.33027,0.45103,0.61785,0.85883,1.15553,
     *1.47206,
     *3.141E-05,6.461E-05,1.278E-04,2.514E-04,4.454E-04,7.285E-04,
     *1.132E-03,1.704E-03,2.511E-03,3.616E-03,5.135E-03,7.211E-03,
     *9.914E-03,1.338E-02,1.774E-02,2.311E-02,3.138E-02,4.135E-02,
     *5.177E-02,
     *5.347E-07,1.064E-06,2.035E-06,3.884E-06,6.516E-06,1.028E-05,
     *1.561E-05,2.321E-05,3.374E-05,4.788E-05,6.663E-05,9.232E-05,
     *1.255E-04,1.686E-04,2.194E-04,2.739E-04,3.639E-04,4.687E-04,
     *5.743E-04/
      data ( (c13co2(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.63918,1.63805,1.63609,1.63448,1.62868,1.62197,
     *1.61184,1.59748,1.57952,1.55626,1.52820,1.49593,
     *1.45372,1.40012,1.32752,1.21144,1.08034,0.91050,
     *0.72074,
     *5.840E-02,5.836E-02,5.828E-02,5.820E-02,5.797E-02,5.771E-02,
     *5.731E-02,5.681E-02,5.608E-02,5.522E-02,5.410E-02,5.275E-02,
     *5.111E-02,4.912E-02,4.651E-02,4.222E-02,3.826E-02,3.243E-02,
     *2.560E-02,
     *6.567E-04,6.563E-04,6.552E-04,6.540E-04,6.515E-04,6.483E-04,
     *6.436E-04,6.383E-04,6.294E-04,6.197E-04,6.057E-04,5.882E-04,
     *5.685E-04,5.452E-04,5.148E-04,4.644E-04,4.276E-04,3.641E-04,
     *2.865E-04/

	end

c===================================================================
	block data dkfr12

	common /band12dkfr/ hk12h2o(4),c12h2o(4,3,19)

	data hk12h2o /0.914825 , 0.045491 , 0.036386 , 0.003298/
    
      data ( (c12h2o(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *0.00009,0.00078,0.00452,0.01483,0.03426,0.06639,
     *0.11794,0.19817,0.32318,0.51457,0.80714,1.23416,
     *1.80137,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *3.087E-06,2.678E-05,1.455E-04,4.626E-04,1.070E-03,2.083E-03,
     *3.708E-03,6.252E-03,1.015E-02,1.610E-02,2.492E-02,3.792E-02,
     *5.561E-02,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *3.219E-08,2.975E-07,1.519E-06,4.734E-06,1.090E-05,2.135E-05,
     *3.804E-05,6.464E-05,1.046E-04,1.658E-04,2.524E-04,3.817E-04,
     *5.685E-04/
      data ( (c12h2o(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00005,0.00057,
     *0.00374,0.01246,0.02747,0.04804,0.07634,0.11725,
     *0.17851,0.26999,0.40956,0.60833,0.85850,1.14913,
     *1.39638,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,1.700E-06,1.995E-05,
     *1.177E-04,3.789E-04,8.418E-04,1.517E-03,2.429E-03,3.740E-03,
     *5.666E-03,8.523E-03,1.261E-02,1.852E-02,2.637E-02,3.494E-02,
     *4.316E-02,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,1.688E-08,2.206E-07,
     *1.194E-06,3.778E-06,8.425E-06,1.556E-05,2.510E-05,3.895E-05,
     *5.897E-05,8.877E-05,1.274E-04,1.836E-04,2.658E-04,3.456E-04,
     *4.354E-04/
      data ( (c12h2o(3,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.02572,0.02678,0.02958,0.03599,0.04565,0.06011,
     *0.08145,0.11230,0.15518,0.21712,0.30136,0.40518,
     *0.52890,0.66382,0.79336,0.90048,0.97465,1.01230,
     *0.98289,
     *1.362E-03,1.393E-03,1.450E-03,1.638E-03,1.924E-03,2.335E-03,
     *2.941E-03,3.848E-03,5.123E-03,6.871E-03,9.322E-03,1.241E-02,
     *1.614E-02,2.032E-02,2.429E-02,2.793E-02,3.071E-02,3.224E-02,
     *3.241E-02,
     *2.040E-05,2.072E-05,2.110E-05,2.297E-05,2.579E-05,2.946E-05,
     *3.465E-05,4.291E-05,5.505E-05,7.050E-05,9.342E-05,1.237E-04,
     *1.601E-04,2.030E-04,2.433E-04,2.842E-04,3.160E-04,3.324E-04,
     *3.490E-04/
      data ( (c12h2o(4,jt,jp), jp = 1, 19), jt = 1, 3)/
     *2.71173,2.70906,2.69995,2.68591,2.66765,2.64146,
     *2.59903,2.53778,2.45614,2.34626,2.21089,2.05945,
     *1.88700,1.69715,1.50251,1.30286,1.10293,0.90151,
     *0.71955,
     *8.568E-02,8.552E-02,8.527E-02,8.498E-02,8.437E-02,8.368E-02,
     *8.244E-02,8.084E-02,7.844E-02,7.541E-02,7.146E-02,6.686E-02,
     *6.154E-02,5.576E-02,4.970E-02,4.319E-02,3.651E-02,3.006E-02,
     *2.363E-02,
     *8.699E-04,8.665E-04,8.643E-04,8.640E-04,8.569E-04,8.513E-04,
     *8.400E-04,8.293E-04,8.071E-04,7.826E-04,7.463E-04,7.006E-04,
     *6.477E-04,5.909E-04,5.275E-04,4.576E-04,3.863E-04,3.228E-04,
     *2.508E-04/

! OZONE
	common /band12dkfr_o3/ hk12o3(5),c12o3(5,3,19)

	data  hk12o3 / 0.303817, 0.420361, 0.200442 , 0.059613, 0.015767/

      data ( (c12o3(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00979,0.02336,0.04790,0.09069,0.16051,0.27301,
     *0.43742,0.67745,1.04000,1.45151,2.05584,2.83048,
     *3.73868,4.69546,5.56884,6.26397,6.73334,7.07776,
     * 7.46791,
     * 2.488E-04, 4.436E-04, 7.564E-04, 1.272E-03, 2.068E-03, 3.314E-03,
     * 5.033E-03, 7.398E-03, 1.078E-02, 1.542E-02, 2.158E-02, 2.948E-02,
     * 3.868E-02, 4.834E-02, 5.633E-02, 6.131E-02, 6.424E-02, 6.715E-02,
     * 6.959E-02,
     * 2.181E-06, 2.691E-06, 3.966E-06, 6.009E-06, 8.725E-06, 1.186E-05,
     * 1.672E-05, 2.224E-05, 8.728E-06, 4.355E-05, 5.922E-05, 7.806E-05,
     * 9.862E-05, 1.250E-04, 1.489E-04, 1.626E-04, 1.971E-04, 2.556E-04,
     * 2.880E-04/
      data ( (c12o3(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.06620,0.08860,0.12406,0.17583,0.25003,0.35240,
     *0.50090,0.71273,1.04000,1.39262,1.89454,2.52711,
     *3.33143,4.34682,5.58255,6.97494,8.39649,9.69907,
     *10.73610,
     * 1.341E-03, 1.543E-03, 1.816E-03, 2.133E-03, 2.548E-03, 3.105E-03,
     * 3.869E-03, 4.897E-03, 6.356E-03, 8.401E-03, 1.094E-02, 1.395E-02,
     * 1.701E-02, 2.004E-02, 2.315E-02, 2.693E-02, 3.068E-02, 3.452E-02,
     * 3.773E-02,
     * 9.797E-06, 9.625E-06, 9.444E-06, 9.709E-06, 9.672E-06, 9.862E-06,
     * 9.825E-06, 9.816E-06,-1.221E-05, 1.395E-05, 1.855E-05, 2.357E-05,
     * 2.492E-05, 2.418E-05, 1.727E-05, 7.997E-06,-6.288E-06,-1.890E-05,
     *-4.795E-05/
      data ( (c12o3(3,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.25965,0.27977,0.30863,0.35051,0.40986,0.49103,
     *0.60846,0.77464,1.04000,1.32456,1.75906,2.30693,
     *2.92641,3.54370,4.06031,4.43662,4.65689,4.74587,
     * 4.77463,
     * 3.488E-03, 3.439E-03, 3.397E-03, 3.410E-03, 3.492E-03, 3.645E-03,
     * 3.881E-03, 4.122E-03, 4.333E-03, 4.246E-03, 3.837E-03, 3.322E-03,
     * 2.773E-03, 2.215E-03, 1.372E-03, 2.910E-04,-8.093E-04,-1.853E-03,
     *-2.843E-03,
     * 1.886E-05, 1.845E-05, 1.774E-05, 1.646E-05, 1.524E-05, 1.421E-05,
     * 1.372E-05, 1.383E-05,-8.053E-06, 1.462E-05, 1.117E-05, 6.956E-06,
     * 3.922E-06,-3.063E-07, 1.575E-06, 1.906E-06, 4.144E-06, 5.428E-06,
     * 1.528E-05/
      data ( (c12o3(4,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.56031,0.56833,0.58032,0.59986,0.63098,0.67687,
     *0.75268,0.86332,1.04000,1.16430,1.29948,1.37607,
     *1.37588,1.30907,1.19139,1.05733,0.93554,0.85314,
     * 0.79321,
     * 4.727E-03, 4.675E-03, 4.568E-03, 4.404E-03, 4.150E-03, 3.796E-03,
     * 3.303E-03, 2.660E-03, 1.944E-03, 1.260E-03, 7.381E-04, 2.411E-04,
     *-1.656E-04,-4.431E-04,-5.450E-04,-5.321E-04,-5.521E-04,-5.740E-04,
     *-5.761E-04,
     * 1.656E-06, 1.663E-06, 2.081E-06, 2.925E-06, 3.434E-06, 3.553E-06,
     * 3.359E-06, 3.138E-06,-2.039E-05,-3.812E-07, 6.906E-07, 6.906E-07,
     * 1.609E-06, 5.031E-07, 1.387E-06, 1.516E-06, 1.859E-06, 1.206E-06,
     * 4.594E-07/
      data ( (c12o3(5,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.37137,1.36422,1.35091,1.33042,1.29804,1.24585,
     *1.18554,1.10850,1.04000,0.87896,0.73774,0.59572,
     *0.46651,0.35956,0.27922,0.22430,0.18639,0.15853,
     * 0.13974,
     *-2.193E-03,-2.136E-03,-2.075E-03,-1.948E-03,-1.765E-03,-1.479E-03,
     *-1.287E-03,-1.134E-03,-9.195E-04,-7.087E-04,-5.296E-04,-3.786E-04,
     *-2.871E-04,-2.111E-04,-1.944E-04,-1.777E-04,-1.585E-04,-1.308E-04,
     *-1.138E-04,
     * 5.094E-07,-9.377E-08,-2.844E-07,-6.031E-07,-1.406E-06,-2.656E-06,
     *-2.278E-06,-1.478E-06,-2.396E-05,-1.088E-06,-2.844E-07, 1.656E-07,
     * 1.719E-07, 4.156E-07, 4.844E-07, 2.375E-07, 3.562E-07, 4.250E-07,
     * 4.250E-07/

!CO2
	common /band12dkfr_co2/ hk12co2(2),c12co2(2,3,19)

	data hk12co2/ 0.961324 , 0.038676/
	
      data ( (c12co2(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00012,0.00043,0.00111,0.00256,0.00577,0.01151,
     *0.01911,0.03057,0.04735,0.07152,0.10639,0.15660,
     *0.22750,0.32466,0.45192,0.62695,0.86151,1.15048,
     *1.45610,
     *8.013E-06,2.554E-05,6.049E-05,1.357E-04,2.728E-04,4.795E-04,
     *7.884E-04,1.237E-03,1.897E-03,2.829E-03,4.110E-03,5.950E-03,
     *8.587E-03,1.225E-02,1.695E-02,2.269E-02,3.052E-02,3.991E-02,
     *5.045E-02,
     *1.334E-07,4.066E-07,9.128E-07,2.027E-06,3.781E-06,6.025E-06,
     *9.991E-06,1.554E-05,2.375E-05,3.518E-05,5.013E-05,7.142E-05,
     *1.023E-04,1.458E-04,2.020E-04,2.619E-04,3.453E-04,4.411E-04,
     *5.584E-04/
      data ( (c12co2(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.46378,1.46528,1.46749,1.47035,1.47393,1.48012,
     *1.48753,1.49545,1.50304,1.50490,1.49706,1.47418,
     *1.43550,1.38056,1.31013,1.20658,1.07377,0.91729,
     *0.74149,
     *5.042E-02,5.045E-02,5.049E-02,5.053E-02,5.064E-02,5.071E-02,
     *5.086E-02,5.107E-02,5.123E-02,5.125E-02,5.093E-02,5.017E-02,
     *4.878E-02,4.666E-02,4.406E-02,4.049E-02,3.623E-02,3.330E-02,
     *2.516E-02,
     *5.531E-04,5.531E-04,5.533E-04,5.533E-04,5.547E-04,5.538E-04,
     *5.542E-04,5.563E-04,5.563E-04,5.561E-04,5.517E-04,5.439E-04,
     *5.280E-04,5.023E-04,4.711E-04,4.318E-04,3.889E-04,3.905E-04,
     *2.717E-04/
	end

c===================================================================
	block data dkfr11
	common /band11dkfr/ hk11h2o(5),c11h2o(5,3,19)

	data hk11h2o / 0.444198 , 0.385220 , 0.126262 ,
     & 0.037311 , 0.007009 /

      data ( (c11h2o(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *0.00000,0.00000,0.00008,0.00086,0.00363,0.01052,
     *0.03001,0.07842,0.18661,0.41396,0.76228,1.30036,
     *2.06106,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *0.000E+00,0.000E+00,1.562E-06,1.941E-05,8.462E-05,2.456E-04,
     *6.580E-04,1.644E-03,3.748E-03,7.797E-03,1.456E-02,2.503E-02,
     *3.915E-02,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *0.000E+00,0.000E+00,3.437E-09,1.378E-07,6.531E-07,2.066E-06,
     *5.325E-06,1.293E-05,2.824E-05,4.874E-05,9.226E-05,1.576E-04,
     *2.403E-04/
      data ( (c11h2o(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *0.00041,0.00157,0.00488,0.01337,0.03167,0.06774,
     *0.12891,0.22321,0.36117,0.55392,0.82533,1.21923,
     *1.76842,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *9.488E-06,3.591E-05,1.075E-04,2.814E-04,6.704E-04,1.390E-03,
     *2.508E-03,4.255E-03,6.841E-03,1.047E-02,1.544E-02,2.242E-02,
     *3.177E-02,
     *0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00,
     *7.219E-08,2.891E-07,8.313E-07,2.047E-06,5.128E-06,1.039E-05,
     *1.720E-05,2.786E-05,4.424E-05,6.838E-05,9.892E-05,1.392E-04,
     *1.899E-04/
      data ( (c11h2o(3,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00000,0.00000,0.00009,0.00042,0.00127,0.00332,
     *0.00786,0.01660,0.03295,0.05885,0.09495,0.14408,
     *0.21230,0.30777,0.43984,0.62226,0.86117,1.16200,
     *1.51320,
     *0.000E+00,0.000E+00,2.700E-06,1.196E-05,3.451E-05,8.425E-05,
     *1.880E-04,3.823E-04,7.209E-04,1.236E-03,1.947E-03,2.900E-03,
     *4.223E-03,6.060E-03,8.547E-03,1.193E-02,1.638E-02,2.238E-02,
     *2.939E-02,
     *0.000E+00,0.000E+00,2.813E-08,1.153E-07,3.172E-07,7.562E-07,
     *1.613E-06,3.231E-06,5.766E-06,9.300E-06,1.415E-05,2.055E-05,
     *2.922E-05,4.091E-05,5.612E-05,7.652E-05,1.039E-04,1.446E-04,
     *1.910E-04/
      data ( (c11h2o(4,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.01451,0.01494,0.01790,0.02372,0.03162,0.04273,
     *0.05752,0.07786,0.10566,0.14347,0.19622,0.27002,
     *0.36739,0.48830,0.63343,0.78459,0.92817,1.07507,
     *1.17461,
     *5.337E-04,5.444E-04,5.690E-04,6.828E-04,8.466E-04,1.056E-03,
     *1.346E-03,1.729E-03,2.245E-03,2.966E-03,3.963E-03,5.316E-03,
     *7.084E-03,9.453E-03,1.236E-02,1.552E-02,1.820E-02,2.039E-02,
     *2.194E-02,
     *6.587E-06,6.703E-06,6.375E-06,6.969E-06,8.166E-06,9.406E-06,
     *1.145E-05,1.382E-05,1.680E-05,2.156E-05,2.793E-05,3.610E-05,
     *4.620E-05,6.149E-05,8.040E-05,1.038E-04,1.208E-04,1.250E-04,
     *1.247E-04/
      data ( (c11h2o(5,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.82788,1.82672,1.82619,1.82564,1.82672,1.82556,
     *1.82564,1.82332,1.81516,1.80014,1.76685,1.70987,
     *1.63467,1.53211,1.40742,1.26146,1.09063,0.90847,
     *0.71754,
     *3.468E-02,3.466E-02,3.461E-02,3.460E-02,3.453E-02,3.442E-02,
     *3.431E-02,3.414E-02,3.385E-02,3.342E-02,3.274E-02,3.170E-02,
     *3.024E-02,2.829E-02,2.584E-02,2.293E-02,1.979E-02,1.677E-02,
     *1.326E-02,
     *2.034E-04,2.034E-04,2.029E-04,2.033E-04,2.011E-04,2.002E-04,
     *1.985E-04,1.964E-04,1.938E-04,1.885E-04,1.834E-04,1.780E-04,
     *1.678E-04,1.567E-04,1.400E-04,1.198E-04,1.020E-04,8.899E-05,
     *7.262E-05/


! OZONE
	common /band11dkfr_o3/ hk11o3(2),c11o3(2,3,19)

	data  hk11o3 / 0.973683, 0.026317/

      data ( (c11o3(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.11160,0.11694,0.12485,0.13708,0.15490,0.18035,
     *0.21504,0.26357,0.33159,0.42643,0.54808,0.69060,
     *0.84504,1.00000,1.14124,1.26234,1.36787,1.45446,
     *1.52682,
     *1.565E-03,1.554E-03,1.549E-03,1.550E-03,1.558E-03,1.566E-03,
     *1.591E-03,1.643E-03,1.749E-03,1.881E-03,1.968E-03,2.034E-03,
     *2.118E-03,2.236E-03,2.412E-03,2.523E-03,2.606E-03,2.693E-03,
     *2.761E-03,
     *8.206E-06,7.972E-06,8.069E-06,7.944E-06,7.925E-06,7.778E-06,
     *7.797E-06,7.200E-06,6.806E-06,7.622E-06,7.987E-06,8.516E-06,
     *8.937E-06,8.841E-06,9.491E-06,1.026E-05,1.036E-05,1.059E-05,
     *1.026E-05/
      data ( (c11o3(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.96365,1.95785,1.94512,1.92795,1.89909,1.85230,
     *1.77961,1.69148,1.60713,1.53430,1.43732,1.30709,
     *1.15729,1.00000,0.85407,0.72868,0.61960,0.52682,
     *0.45076,
     *8.716E-04,8.880E-04,9.111E-04,9.630E-04,1.046E-03,1.102E-03,
     *1.269E-03,1.354E-03,1.358E-03,1.239E-03,1.052E-03,9.302E-04,
     *8.200E-04,6.619E-04,4.876E-04,3.455E-04,2.603E-04,1.853E-04,
     *9.575E-05,
     *8.147E-06,6.481E-06,7.809E-06,7.037E-06,6.797E-06,5.803E-06,
     *6.141E-06,5.719E-06,5.050E-06,5.925E-06,4.594E-06,5.062E-06,
     *4.119E-06,3.866E-06,3.291E-06,2.494E-06,1.806E-06,1.712E-06,
     *1.619E-06/

! CH4
	common /band11dkfr_ch4/ hk11ch4(4),c11ch4(4,3,19)

	data hk11ch4/0.865020 , 0.060018 , 0.061265 , 0.013697 /

        data ( (c11ch4(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00175,0.00186,0.00236,0.00368,0.00585,0.00947,
     *0.01530,0.02524,0.04176,0.06855,0.10814,0.16470,
     *0.24366,0.35005,0.48602,0.66037,0.88063,1.15237,
     *1.48382,
     *5.966E-05,6.169E-05,5.980E-05,7.925E-05,1.093E-04,1.556E-04,
     *2.256E-04,3.337E-04,4.966E-04,7.509E-04,1.138E-03,1.722E-03,
     *2.543E-03,3.719E-03,5.353E-03,7.563E-03,1.047E-02,1.427E-02,
     *1.895E-02,
     *6.503E-07,6.559E-07,5.644E-07,6.750E-07,8.688E-07,1.097E-06,
     *1.459E-06,1.906E-06,2.516E-06,3.159E-06,4.234E-06,6.203E-06,
     *9.141E-06,1.401E-05,2.062E-05,2.791E-05,3.566E-05,4.864E-05,
     *6.190E-05/
      data ( (c11ch4(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.01050,0.01070,0.01170,0.01435,0.01842,0.02516,
     *0.03542,0.05029,0.06951,0.09404,0.12764,0.17138,
     *0.22980,0.31581,0.44617,0.62437,0.86205,1.18590,
     *1.61479,
     *2.913E-04,2.931E-04,2.859E-04,3.178E-04,3.660E-04,4.407E-04,
     *5.510E-04,6.879E-04,8.481E-04,1.047E-03,1.328E-03,1.753E-03,
     *2.388E-03,3.420E-03,4.829E-03,6.539E-03,8.871E-03,1.205E-02,
     *1.613E-02,
     *2.619E-06,2.622E-06,2.428E-06,2.456E-06,2.669E-06,2.844E-06,
     *3.250E-06,3.597E-06,3.722E-06,4.097E-06,4.534E-06,6.256E-06,
     *9.581E-06,1.288E-05,1.313E-05,1.214E-05,1.304E-05,1.223E-05,
     *1.058E-05/
      data ( (c11ch4(3,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.04628,0.04671,0.04856,0.05280,0.05843,0.06590,
     *0.07557,0.08820,0.10576,0.13018,0.16622,0.21913,
     *0.29579,0.40519,0.55081,0.72897,0.91824,1.09785,
     *1.24313,
     *7.931E-04,7.956E-04,7.733E-04,8.039E-04,8.394E-04,8.905E-04,
     *9.610E-04,1.052E-03,1.195E-03,1.413E-03,1.751E-03,2.267E-03,
     *3.048E-03,4.213E-03,5.740E-03,7.539E-03,9.501E-03,1.118E-02,
     *1.237E-02,
     *5.384E-06,5.391E-06,5.194E-06,5.266E-06,5.403E-06,5.488E-06,
     *5.744E-06,5.806E-06,5.884E-06,6.200E-06,6.344E-06,6.313E-06,
     *6.097E-06,6.766E-06,8.466E-06,8.053E-06,1.122E-05,1.293E-05,
     *1.237E-05/
      data ( (c11ch4(4,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.78344,1.77799,1.76799,1.75348,1.73023,1.69947,
     *1.65701,1.60388,1.54595,1.49093,1.45025,1.42143,
     *1.39841,1.36918,1.31127,1.20896,1.07866,0.93319,
     *0.78806,
     *1.787E-02,1.777E-02,1.764E-02,1.745E-02,1.714E-02,1.671E-02,
     *1.616E-02,1.548E-02,1.481E-02,1.426E-02,1.392E-02,1.389E-02,
     *1.394E-02,1.386E-02,1.330E-02,1.220E-02,1.076E-02,9.265E-03,
     *7.711E-03,
     *1.669E-05,1.618E-05,1.572E-05,1.508E-05,1.539E-05,1.365E-05,
     *1.323E-05,1.234E-05,1.162E-05,1.214E-05,1.118E-05,1.287E-05,
     *1.518E-05,1.561E-05,1.248E-05,1.177E-05,8.472E-06,6.391E-06,
     *2.587E-06/
!N2O

	common /band11dkfr_n2o/ hk11n2o(2),c11n2o(2,3,19)

	data hk11n2o/0.930764, 0.069236/

      data ( (c11n2o(1,jt,jp), jp = 1, 19), jt = 1, 3)/
     *0.00050,0.00114,0.00233,0.00476,0.00908,0.01587,
     *0.02595,0.04097,0.06286,0.09246,0.13543,0.19313,
     *0.27407,0.38342,0.51938,0.68028,0.88705,1.14064,
     *1.41256,
     *1.893E-05,2.733E-05,4.675E-05,7.575E-05,1.148E-04,1.660E-04,
     *2.398E-04,3.465E-04,5.000E-04,7.157E-04,9.908E-04,1.342E-03,
     *1.798E-03,2.432E-03,3.199E-03,3.929E-03,4.523E-03,5.232E-03,
     *5.988E-03,
     *2.587E-07,2.581E-07,4.000E-07,4.562E-07,5.750E-07,7.063E-07,
     *9.250E-07,1.194E-06,1.513E-06,2.750E-06,3.450E-06,4.894E-06,
     *6.013E-06,7.222E-06,8.538E-06,1.260E-05,1.383E-05,1.305E-05,
     *1.275E-05/
      data ( (c11n2o(2,jt,jp), jp = 1, 19), jt = 1, 3)/
     *1.65497,1.65363,1.65134,1.64740,1.64078,1.63262,
     *1.61814,1.58872,1.57649,1.54641,1.51493,1.47966,
     *1.43172,1.36766,1.29030,1.19705,1.07703,0.93204,
     *0.77538,
     *6.922E-03,6.915E-03,6.918E-03,6.918E-03,6.950E-03,6.960E-03,
     *6.978E-03,7.006E-03,6.963E-03,6.893E-03,6.772E-03,6.544E-03,
     *6.250E-03,5.907E-03,5.453E-03,5.034E-03,4.693E-03,4.293E-03,
     *3.837E-03,
     *3.273E-05,3.249E-05,3.229E-05,3.205E-05,3.246E-05,3.092E-05,
     *3.154E-05,3.730E-05,2.915E-05,2.854E-05,2.840E-05,2.623E-05,
     *2.578E-05,2.549E-05,2.447E-05,2.185E-05,2.198E-05,2.152E-05,
     *2.233E-05/
	end
