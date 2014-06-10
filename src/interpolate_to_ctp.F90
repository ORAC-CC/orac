! Name:
!
!
! Purpose:
!
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!20131122: MJ writes routine which return ctp FG including uncertainty information
!based on interpolation of brightness temperature.
! 20140123 CP puts in a condition to check minimum inversion level as false inversions over cold surfaces were causing mon_k values of 0 which subsequently crashed the program
!20140127 MJ changes how minimum from above is implemented and catches division by zero if temperature is constant and some cleanup.
!20140610 MJ catches another div. by zero.
!
! $Id$
!
! Bugs:
!
subroutine interpolate2ctp(SPixel,Ctrl,BT_o,BP_o,DBP_o)

  use ECP_Constants

  use Ctrl_def

  use SPixel_def

  implicit none

  type(SPixel_t), intent(inout) :: SPixel   
  type(Ctrl_t), intent(in)      :: Ctrl
  real            :: BT_o,BP_o,DBP_o,dx,dy,xd
  real            :: invert_t(SPixel%RTM%LW%Np),&
       & invert_p(SPixel%RTM%LW%Np),invert_h(SPixel%RTM%LW%Np)
  integer :: kspot,ik,mon_k,upper_index,lower_index
  integer :: min_prof_lev

  !set default values if anything goes wrong
  BP_o=Ctrl%X0(3)
  !FG does not need Error but AP does
  DBP_o=MDADErrPc
  
  !minimum level above which to look for inversion
  min_prof_lev=3

  !invert order of profiles
  do ik=1,SPixel%RTM%LW%Np

     invert_t(ik)=SPixel%RTM%LW%T(SPixel%RTM%LW%Np-ik+1)
     invert_p(ik)=SPixel%RTM%LW%P(SPixel%RTM%LW%Np-ik+1)
     invert_h(ik)=SPixel%RTM%LW%H(SPixel%RTM%LW%Np-ik+1)

  enddo


  !default value for inversion: TOA
  mon_k=SPixel%RTM%LW%Np
  !determine where temperature inversion is, counting from ground up
  do ik=1,SPixel%RTM%LW%Np-1

     if(invert_t(ik+1) .gt. invert_t(ik)) then
        mon_k=ik
        exit
     endif
  enddo
  !set mon_k at least to level three to avoid problems with cold surfaces
  mon_k=max(mon_k,min_prof_lev)

  !locate kspot (within profile up tp mon_k), with invert_t(kspot) <= bt_o < invert_t(kspot+1)
  !we interpolate between those two below then
  call locate_int(invert_t(1:mon_k),mon_k,BT_o,kspot)

  !If locate routine cannot find a pair of points in the temperature profile between which the BT falls.
  !In other words the BT is outside the temperature profile 
  !(or at least this monotonous stretch of it which is investigated).
  !1.) if kspot is too low (unlikely)
  if( kspot .eq. 0) then
#ifdef DEBUG
     write(*,*) 'WARNING: Locating kspot for FG/AP CTP FAILED'
     write(*,*) 'kspot',kspot
     write(*,*) 'mon_k',mon_k
     write(*,*) 'invert_t',invert_t(1:mon_k)
     write(*,*) 'BT',BT_o
#endif
     !if no interpolation possible set BP_o and DBP_o to hardcoded values to recover:
     BP_o=Ctrl%X0(3)
     !FG does not need Error but AP does
     DBP_o=MDADErrPc
     !2.) if point too high up just use highest point for extrapolation
     !(could inspect profile coming from top as well, but keep it simpe for the time being)
  elseif(kspot .eq. mon_k) then

     !if profile is mon. decresing everything is fine to determine gradient
     if(invert_t(mon_k-1)-invert_t(mon_k) .gt. ditherm3) then
        dx=invert_t(mon_k-1)-invert_t(mon_k)
        dy=invert_p(mon_k-1)-invert_p(mon_k)
        xd=BT_o-invert_t(mon_k)
        BP_o=invert_p(mon_k)+dy*xd/dx
        !if profile is isothermal just use this last pressure
     else
        BP_o=invert_p(mon_k)
     endif
     DBP_o=MDADErrPc

     !if extrapolation goes too far use just highest point
     if(BP_o .lt. Ctrl%Invpar%Xllim(3)) then
        !BP_o=invert_p(mon_k)
        BP_o=Ctrl%X0(3)
     endif
     BP_o=Ctrl%X0(3)
     DBP_o=MDADErrPc

#ifdef DEBUG
     write(*,*) 'WARNING: kspot eq mon_k: linearly extrapolating now'
     write(*,*) 'kspot, mon_k',kspot,mon_k
     write(*,*) 'invert_t(mon_k),invert_p(mon_k)',invert_t(mon_k),invert_p(mon_k)
     write(*,*) 'BT_o,BP_o',BT_o,BP_o
#endif

     !if everything good, do the actual interpolation
  else

     !set indices to bound interpolation
     upper_index=kspot+1
     lower_index=kspot

     !write(*,*) 'indices',upper_index,lower_index
     !call the polynomial interpolation (so far linear), return interpolated ctp and uncertainty estimate delctp
     if(invert_t(upper_index)-invert_t(lower_index) .gt. ditherm3) then
        call polint(invert_t(lower_index:upper_index),invert_p(lower_index:upper_index),&
             & upper_index-lower_index+1,BT_o,BP_o,DBP_o)
     else
        BP_o=invert_p(upper_index)
        DBP_o=MDADErrPc
     endif

  endif

  !last safety check, if pressure too low set to lower limit
  if(BP_o .lt. Ctrl%Invpar%Xllim(3)) then
     BP_o=Ctrl%Invpar%Xllim(3)
     DBP_o=MDADErrPc
  endif
  

end subroutine interpolate2ctp
