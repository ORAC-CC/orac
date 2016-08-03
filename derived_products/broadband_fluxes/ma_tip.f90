
        subroutine gascon_mt_parm ( ib )
!  *********************************************************
!  Parameterized Ma and Tipping (2002) continuum absorption.
!  Linear weighting with Planck function is used.
!  *********************************************************
      USE FUINPUT
      implicit none

      integer m,ib

      real a1(18), b1(18), a2(18), b2(18)
      real tm, pm, hm, em, eatm, pfatm, cs, cf, fkc,tgm,fnoto
!      real pp,pt,ph,po,dz
!      common /atmos/ pp(nv1x), pt(nv1x), ph(nv1x), po(nv1x)
!      common /thick/ dz(nvx)
       common /con/ tgm(nvx)

!    Cs = a1*T**(b1), i = 1, 18
        data a1/ 6*0.0, 2.342524E-12, 2.807346E-13, 9.912225E-15,     &
                       1.455137E-14, 1.059636E-9,  3.018152E-9,      &
                       2.555730E-9,  1.334979E-9,  6.189722E-10,     &
                       1.060054E-10, 1.790652E-11, 1.763452E-11 /
        data b1/ 6*0.0, -4.154496E+0, -3.462583E+0, -2.852329E+0,     &
                       -3.205255E+0, -5.301763E+0, -5.406081E+0,     &
                       -5.247754E+0, -4.991395E+0, -4.715502E+0,     &
                       -4.241449E+0, -3.772324E+0, -3.711288E+0 /

! Cf = a2*T**(b2), i = 1, 18
        data a2 / 6*0.0, 9.042257E-20, 1.471276E-16, 2.460784E-17,    &
                        8.594072E-24, 6.160071E-27, 6.443826E-23,    &
                        7.781344E-22, 1.838002E-21, 4.410925E-21,    &
                        7.179783E-21, 1.127213E-18, 3.178859E-15 /
        data b2 / 6*0.0, -1.843298E+0, -2.685093E+0, -2.334587E+0,    &
                        -1.307864E-1, 5.510285E-1,  -1.102701E+0,    &
                        -1.319608E+0, -1.229402E+0, -1.135313E+0,    &
                        -9.114466E-1, -1.498141E+0, -2.752571E+0 /

      do m=1,nv
      tgm(m)=0.0
      end do

      if( ib .le. 6 ) return

      fnoto = 7.341e21
      do m=1,nv
         tm = ( pt(m) + pt(m+1) ) * 0.5
         pm = ( pp(m) + pp(m+1) ) *0.5
         hm = ( ph(m) + ph(m+1) ) * 0.5
         em = pm * hm / ( 0.622 + 0.378 * hm )
         eatm = em / 1013.25
         pfatm = ( pm - em ) / 1013.25
         cs = a1(ib) * tm ** (b1(ib))
         cf = a2(ib) * tm ** (b2(ib))
         fkc = fnoto / tm * ( cs * eatm * eatm + cf * eatm * pfatm )
         tgm(m) = fkc * dz(m) * 100000.0
       end do

      return
      end
! Fu 04-29-03
