MODULE ICEDIRSFC
USE FUINPUT , only: mbsx,mccx,fi,u0
USE FUOUTPUT ,only: foscc
implicit none
real , dimension(mbsx,mccx) :: tau_uc,tau_co


 CONTAINS 
!============================================================
subroutine dircorrect 
integer ic,icc,ib,isc
real rt,taco,tauuc
real diftau(18) 

if (u0 <= 0.0 ) return



WAER: do isc  = 1,2

foscc(0,isc)%dirsfc(1:18) =  foscc(0,isc)%rswdir(fi%nv+1,1:18)
foscc(0,isc)%difsfc(1:18) =  foscc(0,isc)%rswdif(fi%nv+1,1:18)

 CLDCON : do icc =1 ,mccx

if ( fi%fc(icc)%cldfrac > 0 ) then

 diftau(1:10)  = tau_co(1,icc)  -tau_uc(1,icc)
 diftau(11:18) = tau_co(2:9,icc)-tau_uc(2:9,icc)
!print'(18f6.1)',diftau
  BAND: do ib = 1,18

   if ( foscc(icc,isc)%rswdir(1,ib) > 0 ) &
   rt =  foscc(icc,isc)%rswdir(fi%nv+1      ,ib)/&
         foscc(icc,isc)%rswdir(1,ib) ! Transmission Cloud,Gas,Rayleigh

   taco = 128.
   if ( rt > 1.0E-36 ) taco = -u0 * log( rt) ! Direct Transmission to Tau

   tauuc = taco - diftau(ib) ! subtract of difference between tau adjusted for forward scatter peak.
!print*,ib,rt,taco,diftau(ib),tauuc, exp(- tauuc/u0)
   foscc(icc,isc)%dirsfc(ib) =  foscc(icc,isc)%rswdir(1,ib) * exp(- tauuc/u0)
   foscc(icc,isc)%difsfc(ib) =  foscc(icc,isc)%rswfd(fi%nv+1,ib) -  foscc(icc,isc)%dirsfc(ib)
  enddo BAND

endif ! CLDFRAC >0

enddo CLDCON


enddo WAER

end subroutine dircorrect
END MODULE ICEDIRSFC
