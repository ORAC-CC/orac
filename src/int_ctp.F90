!-------------------------------------------------------------------------------
! Name: int_ctp.F90
!
! Purpose:
! Given a brighness temperature (presumably at 11um), find a first guess for
! CTP by interpolating that BT within the RTM temperature profile.
!
! Description and Algorithm details:
! 1) Correct the temperature profile.
!    a) Skip past any surface inversion by searching for the lowest level at
!       which the temperature decreases with height.
!    b) Locate inversions within the boundary layer (being anywhere we don't
!       expect to find the troposphere; see 1(c)). These are levels with
!       temperature lower than that of the level above them.
!       i) If an inversion is found, locate the top of the inversion, being
!          the next level up at which the temperature decreases relative to
!          the previous.
!      ii) Consider two levels above that. This reflects the EUMETSAT method
!          this is based on, requiring that the inversion have some width.
!     iii) Overwrite all values between those points by linearly extrapolating
!          from the two levels just beneath the inversion. The purpose of this
!          is to preferentially select lower CTP as the 11um BT will be an
!          underestimate, which will tend to select smaller CTP and can place
!          clouds on the wrong side of an inversion.
!    c) Locate the tropopause. Following http://www.fas.org/spp/military/docops/
!       afwa/atmos-U2.htm, this defines the tropopause as the lowest level
!       between 500 and 30 hPa for which the lapse rate is less than 2 K km^-1
!       and remains below that level for at least 2 km.
!       i) The vertical height is approximated by the geopotential divided by
!          gravitational acceleration.
!      ii) The tropopause and all levels above it are overwritten by
!          extrapolating from the two levels just beneath the tropopause.
! 2) Interpolate the BT onto the corrected temperature profile.
!    a) If the BT is outside the range of the profile, return the pressure of
!       the level with min/max temperature (as appropriate) and flag as an
!       out-of-bounds result.
!    b) If considering a liquid phase retrieval (WAT), search from the bottom
!       of the profile up. Otherwise, search top down.
!    c) Search through the profile for the first pair of levels that bound the
!       requested temperature.
!    d) Linearly interpolate BT between those levels to find the CTP.
! 3) Check the value against the retrieval's upper and lower limits.
!
! Arguments:
! Name   Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! SPixel struct  In          Super pixel structure
! Ctrl   struct  In          Control structure
! BT     real    In          Brightness temperature to find in RTM profile
! CTP    real    Out         Interpolated cloud top pressure
! status int     Out         Indicates an out-of-bounds result
!
! History:
! 2015/02/06, AP: Initial version, derived from interpolate_to_ctp. Moved the
!    functionality of blmodification and extrap_into_tropopause here as this is
!    the only thing that uses them. Locating within temperature profile now
!    dependant on the cloud phase (WAT bottom up, everything else top down).
!
! Bugs:
! Doesn't return an estimate of the uncertainty.
!-------------------------------------------------------------------------------

subroutine Int_CTP(SPixel, Ctrl, BT, CTP, status)

   use Ctrl_m
   use Int_Routines_m
   use ORAC_Constants_m, only : g_wmo, XMDADBounds
   use planck_m
   use SAD_Chan_m

   implicit none

   type(SPixel_t), intent(inout) :: SPixel      ! Contains RTM profiles
   type(Ctrl_t),   intent(in)    :: Ctrl        ! Contains control information
   real,           intent(in)    :: BT          ! Brightness temp to interpol
   real,           intent(out)   :: CTP         ! Interpolated cloud top pre
   integer,        intent(out)   :: status      ! Indicates out-of-bounds

   real,    parameter :: min_tropopause = 30.0  ! Heighest p allowed for trop
   real,    parameter :: max_tropopause = 500.0 ! Lowest p allowed for trop
   integer, parameter :: depth          = 2     ! # layers added to inversions

   integer                          :: nz       ! Number of vertical levels
   integer                          :: k, l     ! Indexing variables
   integer, dimension(1)            :: k_tmax   ! Index of max temperature
   integer, dimension(1)            :: k_tmin   ! Index of min temperature
   integer                          :: k_int    ! Index of interpolated temp
   integer                          :: step     ! Direction of search
   real, dimension(SPixel%RTM%Np-1) :: t        ! Temperature profile
   real, dimension(SPixel%RTM%Np-1) :: p        ! Pressure profile
   real, dimension(SPixel%RTM%Np-1) :: h        ! Approx height profile
   real                             :: gradient ! For extrapolation


   status = 0

   ! When reading the following, remember that 1 is TOA and nz is the surface.
   ! Hence, t(k) < t(k-1) reads as "Is the temperature here less than the
   ! temperature in the level vertically above this level?"

   ! Short variable names
   nz = SPixel%RTM%Np-1
   t  = SPixel%RTM%T(1:nz)
   p  = SPixel%RTM%P(1:nz)
   ! Estimate vertical height with the geopotential divided by gravity
   h  = (0.001 / g_wmo) * SPixel%RTM%H(1:nz)


   !----------------------- CORRECT TEMPERATURE PROFILE ------------------------

   ! Search for temperature inversions within the troposphere, starting
   ! at the lowest *atmospheric* level we can extrapolate from.
   k = nz-2

   ! Ignore surface inversion (as there isn't an obvious way to remove them)
   do while (t(k-1) > t(k) .and. k > 1)
      k = k-1
   end do

   ! Search for inversions within the 'boundary layer'. This is defined as the
   ! region where we aren't looking for the tropopause.
   do while (p(k) > max_tropopause .and. k > 1)
      ! An inversion is a level which has temperature lower than that above it
      if (t(k-1) > t(k)) then
         ! Due to atmospheric transmission, the 11um BT will be an underestimate
         ! of the actual CTT. We compensate using the EUMETSAT procedure to
         ! extrapolate the lapse rate beneath the inversion through to two levels
         ! above the inversion. We could, theoretically, just use the Tac field
         ! from RTM%LW to correct the t profile, but that gives a very flat
         ! profile near the surface.

         ! Find top of inversion
         l = k-2
         do while (t(l) < t(l-1) .and. l > 1)
            l = l-1
         end do
         l = l - depth ! Add levels

         ! Extrapolate lapse rate from the two levels beneath the inversion
         gradient = (t(k+2) - t(k+1)) / (p(k+2) - p(k+1))
         t(l:k) = t(k+1) + gradient * (p(l:k) - p(k+1))

         ! Continue searching above where we have just corrected
         k = l-1
      else
         ! Continue to the next level up
         k = k-1
      end if
   end do

   ! Locate the tropopause
   do while (p(k) > min_tropopause .and. k > 1)
      ! The tropopause is defined as the lowest level with a lapse rate
      ! less than 2 K km^-1
      if ((t(k) - t(k-1)) / (h(k-1) - h(k)) < 2.) then
         ! Find the first level at least 2 km above the identified level
         l = k-1
         do while (h(l) - h(k) < 2. .and. l > 1)
            l = l-1
         end do

         ! We also require that the lapse rate remain this low in the 2km above
         ! the tropopause
         if ((t(k) - t(l)) / (h(l) - h(k)) < 2.) exit
      end if

      ! Continue to the next level up
      k = k-1
   end do

   ! Alter temperatures within the stratosphere by extrapolating the lapse
   ! rate immediately beneath the tropopause upwards
   gradient = (t(k+1) - t(k+2)) / (p(k+1) - p(k+2))
   t(1:k) = t(k+1) + gradient * (p(1:k) - p(k+1))


   !----------------- INTERPOLATE BT ONTO TEMPERATURE PROFILE ------------------

   ! Identify the range of temperature in the profile
   k_tmax = maxloc(t)
   k_tmin = minloc(t)

   if (BT >= t(k_tmax(1))) then
      ! If desired BT greater than profile maximum, return that pressure level
      CTP = p(k_tmax(1))
      status = XMDADBounds
   else if (BT <= t(k_tmin(1))) then
      ! If desired BT smaller than profile minimum, return that pressure level
      CTP = p(k_tmin(1))
      status = XMDADBounds
   else
      ! When inside the range of the temperature profile, search it for the
      ! first set of levels which bound the desired value. Select the direction
      ! of that search dependent on the cloud phase.
      if (Ctrl%Class == ClsCldWat) then
         ! Search from ground up (to trap under inversions)
         k_int = nz
         step  = -1
      else
         ! Search from TOA down (to sit on top of inversions)
         k_int = 1
         step  = 1
      end if

      ! Scan through profile until value bound by k_int and k_int+step
      ! (the conditions cover both increasing or decreasing temp)
      do while ((BT >= t(k_int) .and. BT > t(k_int+step)) .or. &
                (BT <= t(k_int) .and. BT < t(k_int+step)))
         k_int = k_int + step
      end do

      ! Interpolate
      CTP = p(k_int) + (BT - t(k_int)) * (p(k_int+step) - p(k_int)) / &
                       (t(k_int+step) - t(k_int))
   end if

   ! Check output against retrieval limits
   if (CTP < Ctrl%Invpar%XLLim(IPc)) then
      CTP = Ctrl%Invpar%XLLim(IPc)
      status = XMDADBounds
   else if (CTP > Ctrl%Invpar%XULim(IPc)) then
      CTP = Ctrl%Invpar%XULim(IPc)
      status = XMDADBounds
   end if

   ! Overwrite RTM profile with that extrapolated. (Later used to determine
   ! RTM_Pc%LW%B for the forward model.)
   SPixel%RTM%T(1:nz)=t

end subroutine Int_CTP
