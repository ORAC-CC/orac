!-------------------------------------------------------------------------------
! Name: compute_lts.F90
!
! Purpose:
! Compute LTS (lower troposphere stability) from input profile of pressure,
! and temperature. LTS is the difference in potential temperature between
! 700 hPa and the surface. The temperature at 700 hPa level is interpolated
! using linear slope method (T = mP+b). Surface values are taken to be those
! at the base level of the profile.
!
! Inputs:
! temperature (K)
! pressure (hPa)
!
! Output:
! lower troposphere stability (K)
!
! History:
! 2016/02/18, MC: Implementation
!
! $Id$
!
! Bugs:
! none.
!
!-------------------------------------------------------------------------------
subroutine compute_lts(nlm,P,T,LTS)

   implicit none
       integer, intent(in) :: &
         nlm   !Number of vertical layers.      (-).

   !meteorological profiles
   real, intent(in), dimension(nlm+1) :: &
    P   ,& !pressure profile at SAT. pixel            (hPa).
    T      !temperature profile at SAT. pixel           (K).

   !OUTPUT
   real, intent(out) :: LTS

   !local variables
   real :: m(1),TSFC(1),T700(1),PSFC(1)
   real :: thetaSFC(1),theta700(1)
   real, parameter :: k=0.286
   integer :: id1(1),id2(1)


   PSFC = P(nlm)
   TSFC = T(nlm)

   ! Interpolate temperature at 700 hPa level
   ! Check that the lowest layer pressure > 850
   if( maxval(P) .gt. 850.) then
      id1 = MINLOC(P,MASK=(P>600))
      id2 = MAXLOC(P,MASK=(P<800))
      m = (T(id2)-T(id1))/(P(id2)-P(id1))
      T700 = m*700.+T(id1)-m*P(id1)
      thetaSFC = TSFC*(1000./PSFC)**k
      theta700 = T700*(1000./700.)**k
      LTS = theta700(1) - thetaSFC(1)
       !print*,id1,id2,P(id1),P(id2),T(id1),T(id2),m,T700,TSFC,PSFC
       !print*,thetaSFC,theta700,LTS
   end if

   return
end subroutine compute_lts
