!-------------------------------------------------------------------------------
! Name: interpolate2ctp
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2013/11/22, MJ: Writes routine which return ctp FG including uncertainty
!    information based on interpolation of brightness temperature.
! 2014/01/23, CP: Puts in a condition to check minimum inversion level as false
!    inversions over cold surfaces were causing mon_k values of 0 which
!    subsequently crashed the program
! 2014/01/27, MJ: Changes how minimum from above is implemented and catches
!    division by zero if temperature is constant and some cleanup.
! 2014/06/10, MJ: Catches another div. by zero.
! 2014/08/01, GM: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine interpolate2ctp(SPixel,Ctrl,BT_o,BP_o,DBP_o)

   use Ctrl_def
   use ECP_Constants
   use SPixel_def

   implicit none

   type(SPixel_t), intent(inout) :: SPixel
   type(Ctrl_t),   intent(in)    :: Ctrl
   real,           intent(out)   :: BT_o
   real,           intent(out)   :: BP_o
   real,           intent(out)   :: DBP_o

   integer :: i
   integer :: mon_k,mon_k_trop
   integer :: upper_index,lower_index
   integer :: kspot
   integer :: min_prof_lev

   real    :: dx,dy,xd
   real    :: invert_t(SPixel%RTM%LW%Np)
   real    :: invert_p(SPixel%RTM%LW%Np)
   real    :: invert_h(SPixel%RTM%LW%Np)
   real    :: invert_temp(SPixel%RTM%LW%Np)

   ! set default values if anything goes wrong
   BP_o=Ctrl%X0(3)

   ! FG does not need Error but AP does
   DBP_o=MDADErrPc

   ! minimum level above which to look for inversion
   min_prof_lev=3

   ! invert order of profiles
   do i=1, SPixel%RTM%LW%Np
      invert_t(i)=SPixel%RTM%LW%T(SPixel%RTM%LW%Np-i+1)
      invert_p(i)=SPixel%RTM%LW%P(SPixel%RTM%LW%Np-i+1)
      invert_h(i)=SPixel%RTM%LW%H(SPixel%RTM%LW%Np-i+1)
   end do

   ! default value for inversion: TOA
   mon_k=SPixel%RTM%LW%Np
   mon_k_trop=mon_k

   ! determine where tropospheric temperature inversion is, counting from ground
   ! up
   do i=1,SPixel%RTM%LW%Np-1
      ! this catches when the tropopause is extrapolated
      if (i .ge.  SPixel%RTM%LW%Np-1) then
         mon_k_trop=SPixel%RTM%LW%Np-1
         exit
      end if

      ! > 700HP means you do not get trapped under a boundary layer inversion or
      ! polar inversion this might need to be changed when interpolate profile
      ! into stratosphere
      if ((invert_t(i+1) .gt. invert_t(i)) .and. invert_p(i) .lt. 700.) then
         mon_k_trop=i
         exit
      end if
   end do

   ! set mon_k at least to level three to avoid problems with cold surfaces
   mon_k_trop=max(mon_k_trop,min_prof_lev)

   ! locate kspot (within profile up tp mon_k), with invert_t(kspot) <= bt_o <
   ! invert_t(kspot+1) we interpolate between those two below then
   call locate_int(invert_t(1:mon_k_trop),mon_k_trop,BT_o,kspot)

   if (kspot .ne. 0 ) then
      mon_k=mon_k_trop
   end if

   if (kspot .eq. 0) then
      ! profile may have unusual inversions

      invert_temp(1:mon_k_trop)=invert_t(1:mon_k_trop)

      ! sort into ascending order
      call hpsort(mon_k_trop,invert_temp(1:mon_k_trop))

      ! try to find match in this sorted profile
      call locate_int(invert_temp(1:mon_k_trop),mon_k_trop,BT_o,kspot)

      if (kspot .ne. 0) then
         do i=1,mon_k_trop
            if (invert_temp(kspot) .eq. invert_t(i)) then
               mon_k=i
            end if
         end do
      end if
   end if

   ! If locate routine cannot find a pair of points in the temperature profile
   ! between which the BT falls. In other words the BT is outside the temperature
   ! profile (or at least this monotonous stretch of it which is investigated).

   ! If kspot is too low (unlikely)
   if ( kspot .eq. 0) then
      ! I think this is being set incorrectly over polar regions resulting in
      ! bad retrievals over snow

      ! If no interpolation possible set BP_o and DBP_o to hardcoded values to
      ! recover:
      if ( BT_o .gt. invert_t(1)) then
         ! most likely at the surface
         BP_o=invert_p(1)
      else
         BP_o=Ctrl%X0(3)
      end if

      if ( BP_o .lt. 20.0) then
         ! too warm - more likely surface
         BP_o=invert_p(1)
      end if

      DBP_o=MDADErrPc

   ! If point too high up just use highest point for extrapolation
   ! (could inspect profile coming from top as well, but keep it simple for the
   ! time being)
   else if (kspot .eq. mon_k) then
      ! profile is mon. decreasing everything is fine to determine gradient.
      if (invert_t(mon_k-1)-invert_t(mon_k) .gt. ditherm3) then
         dx=invert_t(mon_k-1)-invert_t(mon_k)
         dy=invert_p(mon_k-1)-invert_p(mon_k)
         xd=BT_o-invert_t(mon_k)
         BP_o=invert_p(mon_k)+dy*xd/dx
      ! else profile is isothermal just use this last pressure
      else
         BP_o=invert_p(mon_k)
      end if

      DBP_o=MDADErrPc

      ! if extrapolation goes too far use just highest point
      if (BP_o .lt. Ctrl%Invpar%Xllim(3)) then
         BP_o=Ctrl%X0(3)
      end if

      BP_o=Ctrl%X0(3)
      DBP_o=MDADErrPc

   ! if everything good, do the actual interpolation
   else
      !set indices to bound interpolation
      upper_index=kspot+1
      lower_index=kspot

      ! call the polynomial interpolation (so far linear), return interpolated
      ! ctp and uncertainty estimate delctp
      if (invert_t(upper_index)-invert_t(lower_index) .gt. ditherm3) then
         call polint(invert_t(lower_index:upper_index), &
                     invert_p(lower_index:upper_index), &
                     upper_index-lower_index+1,BT_o,BP_o,DBP_o)
      else
         BP_o=invert_p(upper_index)
         DBP_o=MDADErrPc
      end if
   end if

   ! last safety check, if pressure too low set to lower limit
   if (BP_o .lt. Ctrl%Invpar%Xllim(3)) then
      BP_o=Ctrl%Invpar%Xllim(3)
      DBP_o=MDADErrPc
   end if

end subroutine interpolate2ctp
