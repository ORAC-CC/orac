MODULE CALIPSO_OUTPUT

USE FUINPUT, only : nv1x,fi
USE FUOUTPUT, only : fo
implicit none
real ,parameter,private  :: r4omit= -9999

!---------------------------------
TYPE PACKP_TYPE
sequence
 real d(nv1x)   !! DOWNWELLING
  real u(nv1x)   !! UPWELLNG
END TYPE PACKP_TYPE

TYPE FLXP_TYPE
sequence
 type(PACKP_TYPE) sw
 type(PACKP_TYPE) lw
 type(PACKP_TYPE) wn
END TYPE FLXP_TYPE

TYPE SKYP_TYPE
sequence
 integer nlevel
 integer sfclev
 real pp(nv1x)
 real zz(nv1x)
  real tt(nv1x)
  real hh(nv1x)
 TYPE (FLXP_TYPE) clr
 TYPE (FLXP_TYPE) prs
 TYPE (FLXP_TYPE) tot
 TYPE (FLXP_TYPE) tna
END TYPE SKYP_TYPE

TYPE (SKYP_TYPE) skyp
 CONTAINS
!==============================================================================
subroutine print_pack_sky
integer k

print*,'                     SHORTWAVE Down---------------    Shortwave Up------------------'
print*,' #  Presure  Height  Clear   Prist   Total  TotNOA    Clear   Prist   Total  TotNOA '
print*,'Lev  [hPa] [meters]   Down    Down    Down    Down     Up      Up      Up      UP   '
do k= 1,skyp%sfclev !!! skyp%nlevel

print'(i4,f8.2,f8.0,8f8.2)',k,skyp%pp(k),skyp%zz(k),&
		  skyp%clr%sw%d(k) ,&
 		  skyp%prs%sw%d(k) ,&
 		  skyp%tot%sw%d(k) ,&
		  skyp%tna%sw%d(k) ,&
		  skyp%clr%sw%u(k) ,&
 		  skyp%prs%sw%u(k) ,&
 		  skyp%tot%sw%u(k) ,&
		  skyp%tna%sw%u(k) 
enddo

!LW
print*, '                    LONGWAVE Down----------------    Longwave  Up------------------'
print*,' #  Presure  Height  Clear   Prist   Total  TotNOA    Clear   Prist   Total  TotNOA '
print*,'Lev  [hPa] [meters]   Down    Down    Down    Down     Up      Up      Up      UP   '
do k= 1,skyp%sfclev !!! skyp%nlevel
print'(i4,f8.2,f8.0,8f8.2)',k,skyp%pp(k),skyp%zz(k),&
		  skyp%clr%lw%d(k) ,&
 		  skyp%prs%lw%d(k) ,&
 		  skyp%tot%lw%d(k) ,&
		  skyp%tna%lw%d(k) ,&
		  skyp%clr%lw%u(k) ,&
 		  skyp%prs%lw%u(k) ,&
 		  skyp%tot%lw%u(k) ,&
		  skyp%tna%lw%u(k) 
enddo

!WN
print*, '                    WINDOW Down------------------    WINDOW  Up--------------------'
print*,' #  Presure  Height  Clear   Prist   Total  TotNOA    Clear   Prist   Total  TotNOA '
print*,'Lev  [hPa] [meters]   Down    Down    Down    Down     Up      Up      Up      UP   '

do k= 1,skyp%sfclev !!! skyp%nlevel

print'(i4,f8.2,f8.0,8f8.2)',k,skyp%pp(k),skyp%zz(k),&
		  skyp%clr%wn%d(k) ,&
 		  skyp%prs%wn%d(k) ,&
 		  skyp%tot%wn%d(k) ,&
		  skyp%tna%wn%d(k) ,&
		  skyp%clr%wn%u(k) ,&
 		  skyp%prs%wn%u(k) ,&
 		  skyp%tot%wn%u(k) ,&
		  skyp%tna%wn%u(k) 
enddo	

end subroutine print_pack_sky
!==============================================================================
subroutine pack_sky_init
skyp%nlevel = nv1x
skyp%sfclev = 0
skyp%pp = r4omit
skyp%zz = r4omit
skyp%tt = r4omit
skyp%hh = r4omit
skyp%clr%sw%d = r4omit
skyp%clr%lw%d = r4omit
skyp%clr%wn%d = r4omit

skyp%clr%sw%u = r4omit
skyp%clr%lw%u = r4omit
skyp%clr%wn%u = r4omit

skyp%tot%sw%d = r4omit
skyp%tot%lw%d = r4omit
skyp%tot%wn%d = r4omit

skyp%tot%sw%u = r4omit
skyp%tot%lw%u = r4omit
skyp%tot%wn%u = r4omit

skyp%prs%sw%d = r4omit
skyp%prs%lw%d = r4omit
skyp%prs%wn%d = r4omit

skyp%prs%sw%u = r4omit
skyp%prs%lw%u = r4omit
skyp%prs%wn%u = r4omit

skyp%tna%sw%d = r4omit
skyp%tna%lw%d = r4omit
skyp%tna%wn%d = r4omit

skyp%tna%sw%u = r4omit
skyp%tna%lw%u = r4omit
skyp%tna%wn%u = r4omit
end subroutine pack_sky_init
!==============================================================================
subroutine pack_sky
integer k ,kk
 call pack_sky_init


skyp%nlevel = nv1x
skyp%sfclev = fi%vd%nrep
do k=1,fi%vd%nrep
kk = fi%vo%ireport(k)
!print*,k,kk

skyp%pp      (k) =  fi%pp     (kk)
skyp%zz      (k) =  fi%zz     (kk)
skyp%tt      (k) =  fi%pt     (kk)
skyp%hh      (k) =  fi%ph     (kk)

skyp%clr%sw%d(k) =  fo(1)%fds (kk)
skyp%clr%lw%d(k) =  fo(1)%fdir(kk)
skyp%clr%wn%d(k) =  fo(1)%fdwn(kk)

skyp%clr%sw%u(k) =  fo(1)%fus( kk)
skyp%clr%lw%u(k) =  fo(1)%fuir(kk)
skyp%clr%wn%u(k) =  fo(1)%fuwn(kk)

skyp%tot%sw%d(k) =  fo(2)%fds (kk)
skyp%tot%lw%d(k) =  fo(2)%fdir(kk)
skyp%tot%wn%d(k) =  fo(2)%fdwn(kk)

skyp%tot%sw%u(k) =  fo(2)%fus (kk)
skyp%tot%lw%u(k) =  fo(2)%fuir(kk)
skyp%tot%wn%u(k) =  fo(2)%fuwn(kk)

skyp%prs%sw%d(k) =  fo(3)%fds (kk)
skyp%prs%lw%d(k) =  fo(3)%fdir(kk)
skyp%prs%wn%d(k) =  fo(3)%fdwn(kk)

skyp%prs%sw%u(k) =  fo(3)%fus (kk)
skyp%prs%lw%u(k) =  fo(3)%fuir(kk)
skyp%prs%wn%u(k) =  fo(3)%fuwn(kk)

skyp%tna%sw%d(k) =  fo(4)%fds (kk)
skyp%tna%lw%d(k) =  fo(4)%fdir(kk)
skyp%tna%wn%d(k) =  fo(4)%fdwn(kk)

skyp%tna%sw%u(k) =  fo(4)%fus (kk)
skyp%tna%lw%u(k) =  fo(4)%fuir(kk)
skyp%tna%wn%u(k) =  fo(4)%fuwn(kk)
enddo


end subroutine pack_sky

END MODULE CALIPSO_OUTPUT
