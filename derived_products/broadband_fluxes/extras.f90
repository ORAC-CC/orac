MODULE EXTRAS

implicit none

 CONTAINS
!===========================================================
  subroutine getatmosphere(stdatm,nlay,pl,tl,ql,o3,skint)
  implicit none
  character*(*)    stdatm
  integer nlay ,i,icl
  real skint,cld
  real ,dimension(*) :: pl,tl,ql,o3
  icl = index(stdatm,' ')-1
!  open(1,file= stdatm(1:icl),status='old')
  open(1,file= trim(stdatm),status='old',action='read' )
  read (1,*) skint,nlay

  do i=1,nlay+1
  read(1,*) pl(i),tl(i),ql(i),o3(i),cld		  
  enddo
  close(1)

!  pw=ql_to_pw(nlay+1,ql,pl)
!  print*,'Atmosphere: ',stdatm(1:icl)
!  print'(a8,f8.2)','PW(cm): ',pw
  return
  end subroutine getatmosphere

!=========================================================================
	real function tau_wp(idir,rin,radius,ipha)
! NEW (10-30-96)MINNIS VERSION OF VISIBLE OPTICAL DEPTH TO 
! WATER/ICE PATH RELATION

! IDIR= 1        TAU ----> WATER PATH
! IDIR=-1 WATER PATH ----> TAU
! RIN ( TAU if IDIR=1) ( LWP/IWP if IDIR=-1)
! RADIUS ( radius in um WATER droplets)
!         ( effective diameter in um for ICE crystals)
! IPHA  ( 1= WATER) ( 2=ICE)
        implicit none
        integer,intent(in) :: idir,ipha
        real,intent(in) :: rin,radius
        real veae,val,alr,qext
	real*4 iwpcoefs(3),qextcoefs(3)
!c OLD	data iwpcoefs/0.1473,0.2833,-2.594E-4/
	data iwpcoefs/2.453e-1,1.2196e-3,-3.4745e-6/ !NEW
	data qextcoefs/2.416, -0.1854,.0209/   
! ICE	
	if(ipha.eq.2)then
! OLD	veae= iwpcoefs(1)+
!     &        iwpcoefs(2)*radius +
!     &        iwpcoefs(3)*radius*radius
	veae= iwpcoefs(1)*radius +		&
              iwpcoefs(2)*radius *radius+	&
              iwpcoefs(3)*radius*radius*radius

	val=veae
	elseif(ipha.eq.1) then
! WATER
	 alr=alog(radius)
	 qext = qextcoefs(1) + qextcoefs(2)*alr +qextcoefs(3)*alr*alr
	 val =4.*radius/3./qext
	else
! OTHER
!	 print*, 'ipha ne 1or2'
	 tau_wp=-9999.
	 return
	endif

	if(idir.eq. 1)tau_wp=rin*val !TAU ----> WATER PATH
	if(idir.eq.-1)tau_wp=rin/val !WATER PATH ----> TAU

	return
	end function tau_wp   
!===================================================================
real function  sh_rh (tl, pl, sh) 
real  tl,pl,sh
real  es, ws
!	rh (0-100)
!	tl (K)
!	pl (mb)
!	q (g/g)
es = satvap (tl)
ws = 0.622 * es / (pl - es)
sh_rh= sh/ws * 100.
end function sh_rh
!===================================================================
real function  rh_sh (rh, tl, pl) 
real  rh,tl,pl
real  es, ws
!	rh (0-100)
!	tl (K)
!	pl (mb)
!	q (g/g)
      es = satvap (tl)
      ws = 0.622 * es / (pl - es)
      rh_sh = (rh/100.0) * ws
end function rh_sh
!================================================================      
      real function satvap(temp2)
       implicit none
       real ,intent(in):: temp2
       real temp,toot,toto,eilog
       real tsot,ewlog,ewlog2,ewlog3,ewlog4

      temp = temp2-273.155
      if (temp.lt.-20.) then   !!!! ice saturation
        toot = 273.16 / temp2
        toto = 1 / toot
        eilog = -9.09718 * (toot - 1) - 3.56654 * (log(toot) /  &
         log(10.)) + .876793 * (1 - toto) + (log(6.1071) / log(10.))
        satvap = 10 ** eilog
      else
        tsot = 373.16 / temp2
        ewlog = -7.90298 * (tsot - 1) + 5.02808 *	&
                  (log(tsot) / log(10.))
        ewlog2 = ewlog - 1.3816e-07 *			&
                  (10 ** (11.344 * (1 - (1 / tsot))) - 1)
        ewlog3 = ewlog2 + .0081328 *			&
                  (10 ** (-3.49149 * (tsot - 1)) - 1)
        ewlog4 = ewlog3 + (log(1013.246) / log(10.))
        satvap = 10 ** ewlog4
      end if

      return
      end function satvap

!=====================================================================
 real function ql_to_pw(nl,ql,pb)
 implicit none
 integer ,intent(in)::nl
 real ,intent(in):: ql(nl),pb(nl)
 real pw ,qlm
 integer i
! 	NL = # of model levels
!	pb = Pressure Profile (top to bottom in mb)
!	ql = Specific Humidity profile (in g/g)
! 	pw = precipitable water in cm
 pw = 0.0
 do  i=2,nl
  qlm = 0.5* (ql(i) + ql(i-1) )
  pw  =  pw+ 1.02*qlm*(pb(i)-pb(i-1))
 enddo
 ql_to_pw =pw
 return
 end function ql_to_pw
!----------------------------------------------------
!----------------------------------------------------
 subroutine aer_scale_hgt(nv,pp,h,aprof)
 implicit none
 integer,intent(in):: nv
 real,intent(in):: pp(nv+1),h
 real,intent(out):: aprof(nv)
 integer i
 real sum1,tot,z1,z2
 sum1=0
 do i=1,nv
 
 z1= 8.0* log( pp(nv+1) /pp(i))
 z2= 8.0* log( pp(nv+1) /pp(i+1))
 aprof(i)= exp(-z2/h)-exp(-z1/h) 
 sum1=sum1+aprof(i)
!       print'(4f7.2,2f10.4)',pp(i),z1,pp(i+1),z2,aprof(i),sum1
 enddo
!
 tot= sum(aprof(1:nv))   
 aprof = 100*(aprof/tot) !! aprof in %
!       print*,aprof(1:nv)
 return
 end subroutine aer_scale_hgt
!=============================================================
! the following subroutines and functions lifted from CERES_METEO
!-----------------------------------------------------------------

      SUBROUTINE new_sfc_alt (z0, p0, t0, q0, z1,                        & ! I
                                                  p1, t1, q1)              ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   new_sfc_alt
!
! !Description:
!    Routine ID -
!
!    Purpose - To estimate sub-regional meteorological values for a
!              selected near-surface pressure level given the existing
!              surface meteorological values.
!
! !Input Parameters:
!   z0 - MOA surface altitude (m)
!   p0 - MOA surface pressure (hPa)
!   t0 - MOA surface temperature (K)
!   q0 - MOA surface humidity (g/g)
!   z1 - SSF surface altitude (m)
!
! !Output Parameters:
!   p1 - Surface pressure level (hPa)
!   t1 - Surface temperature (K)
!   q1 - Surface humidity (g/g)
!
! !Team-Unique Header:
!
!  Internal Routines Called:
!    z1_p1
!    t0_t1
!    q0_q1
!
! !Revision History:
!  Version 2.0   August 15, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      REAL, INTENT (IN)  :: z0, p0, t0, q0, z1
      REAL, INTENT (OUT) :: p1, t1, q1
!
!
      p1 = z1_p1 (p0, t0, z0, z1)          !! NEW SURFACE PRESSURE
      t1 = t0_t1 (t0, z0, z1)              !! NEW SURFACE TEMPERATURE
      q1 = q0_q1 (z0, p0, t0, q0, z1)      !! NEW SURFACE MIXING RATIO
!
      RETURN
!
      END SUBROUTINE new_sfc_alt

!=============================================================================
      FUNCTION q0_q1 (z0, p0, t0, q0, z1) RESULT (F_Result)

! !F90
!
! !Name:
!   q0_q1
!
! !Description:
!    Routine ID -
!
!    Purpose - To calculate specific humidity at a sub-regional elevation
!              given the existing regional values of surface elevation,
!              pressure, temperature, and specific humidity
!
!
! !Input Parameters:
!   z0 - MOA surface geopotential height (m)
!   p0 - MOA surface pressure (hPa)
!   t0 - MOA surface air temperature (K)
!   q0 - MOA surface specific humidity (g/g)
!   z1 - SSF footprint altitude (m)
!
! !Output Parameters:
!   F_Result - Specific humidity corresponding to SSF Surface Altitude z1 (g/g)
!
! !Team-Unique Header:
!
!  Internal Routines Called:
!    sh_rh
!    t0_t1
!    z1_p1
!    rh_sh
!
! !Revision History:
!  Version 2.0   August 15, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      REAL, INTENT (IN) :: z0, p0, t0, q0, z1
!
      REAL :: F_Result, rh0, t1, p1
!
!
!      CALL sh_rh (rh0, t0, p0, q0)
      rh0= sh_rh(t0, p0, q0)
      t1  = t0_t1 (t0, z0, z1)
      p1  = z1_p1 (p0, t0, z0, z1)
!      CALL rh_sh (rh0, t1, p1, F_Result)
      F_Result = rh_sh (rh0, t1, p1)
!
      RETURN
!
      END FUNCTION q0_q1
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      FUNCTION z1_p1 (p0, t0, z0, z1) RESULT (F_Result)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   z1_p1
!
! !Description:
!    Routine ID -
!
!    Purpose - To determine the surface pressure (hPa) for a
!              sub-regional elevation (m), given the regional values
!              for pressure, temperature, and elevation
!
!
! !Input Parameters:
!   p0 - MOA surface pressure (hPa)
!   t0 - MOA surface air temperature (K)
!   z0 - MOA surface geopotential height (m)
!   z1 - SSF footprint altitude (m)
!
! !Output Parameters:
!   F_Result - Surface pressure (hPa) corresponding to SSF surface altitude
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 2.0   August 15, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      REAL, INTENT (IN) :: p0, t0, z0, z1
!
      REAL            :: F_Result, dz, expo, p1
      REAL, PARAMETER :: g=9.8, r=287, gamma=6.5*.001
!
!
      dz= z1-z0
      expo = g/(r*gamma)
      p1 = p0 *(( t0-gamma*dz)/t0)**expo
      F_Result = p1
!
      RETURN
!
      END FUNCTION z1_p1
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      FUNCTION p1_z1 (p0, t0, z0, p1) RESULT (F_Result)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   p1_z1
!
! !Description:
!    Routine ID -
!
!    Purpose - To determine the surface elevation (m) for a sub-regional
!              surface pressure (hPa), given the regional surface pressure,
!              air temperature, and geopotential height
!
!
! !Input Parameters:
!   p0 - MOA surface pressure (hPa)
!   t0 - MOA surface air temperature (K)
!   z0 - MOA surface geopotential height (m)
!   p1 - SSF surface Pressure (hPa)
!
! !Output Parameters:
!   F_Result - SSF elevation (m) coresponding to p1
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 2.0   August 15, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      REAL, INTENT (IN) :: p0, t0, z0, p1
!
      REAL            :: F_Result, exp2, z1
      REAL, PARAMETER :: g=9.8, r=287, gamma=6.5*.001
!
!
      exp2 = (r*gamma)/g
      z1 = (t0/gamma)* (1.0 -(p1/p0)**exp2)
      F_Result = z1+z0
!
      RETURN
!
      END FUNCTION p1_z1
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      FUNCTION t0_t1 (t0, z0, z1) RESULT (F_Result)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   t0_t1
!
! !Description:
!    Routine ID -
!
!    Purpose - To estimate the temperature (K) at footprint altitude
!
!
! !Input Parameters:
!   t0 - MOA surface air temperature (K)
!   z0 - MOA surface geopotential height (m)
!   z1 - SSF footprint altitude (m)
!
! !Output Parameters:
!   F_Result - Surface temperature at z1
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 2.0   August 15, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      REAL, INTENT (IN) :: t0, z0, z1
!
      REAL            :: F_Result, dz
      REAL, PARAMETER :: g=9.8, r=287, gamma=6.5*.001
!
!
      dz = z1-z0
      F_Result = t0 - dz*gamma
!
      RETURN
!
      END FUNCTION t0_t1
!



END MODULE EXTRAS
