module solar_position_m

   implicit none

   private

   public :: sun_pos_calc, &
             get_day_of_year


contains

subroutine get_day_of_year(day, month, year, dayj)
   implicit none

   real, intent(in)  :: day, month, year
   real, intent(out) :: dayj

   integer :: i
   integer :: leap_day

   if ( mod(year,400.) == 0 ) then
      leap_day = 1
   else if ( mod(year,100.) == 0 ) then
      leap_day = 0
   else if ( mod(year,4.) == 0 ) then
      leap_day = 1
   else
      leap_day = 0
   end if

   dayj = day
   do i = 1, nint(month)-1
      select case (i)
      case (1,3,5,7,8,10,12)
         dayj = dayj + 31
      case (4,6,9,11)
         dayj = dayj + 30
      case (2)
         dayj = dayj + 28 + leap_day
      end select
   end do

end subroutine get_day_of_year

subroutine sun_pos_calc(year, day, hour, lat, lon, el, az)

   use ieee_arithmetic, only : ieee_is_nan
   use system_utils_m
   implicit none
!$acc routine seq

   real, intent(in)  :: year
   real, intent(in)  :: day
   real, intent(in)  :: hour
   real, intent(in)  :: lat
   real, intent(in)  :: lon
   real, intent(out) :: el,az

   real :: ha
   real :: dec

   real :: twopi, pi, rad
   real :: delta, leap, jd, time
   real :: mnlon, mnanom, eclon, oblqec, num, den, ra
   real :: gmst, lmst, latrad

   real, parameter :: SMALL = 1.E-18



   data twopi,pi,rad/6.2831853,3.1415927,.017453293/

   delta = year - 1949.
   leap = aint(delta/4.)
   jd = 32916.5 + delta*365. + leap + day + hour/24.
   if (amod(year,100.) .eq. 0.0 .and. amod(year,400.) .ne. 0.0) jd = jd-1.

   time = jd - 51545.0

   mnlon = 280.460 + .9856474*time
   mnlon = mod(mnlon,360.)
   if (mnlon .lt. 0.) mnlon = mnlon + 360.

   mnanom = 357.528 + .9856003*time
   mnanom = mod(mnanom,360.)
   if (mnanom .lt. 0.) mnanom = mnanom + 360.
   mnanom = mnanom*rad

   eclon = mnlon + 1.915*sin(mnanom) + .020*sin(2.*mnanom)
   eclon = mod(eclon,360.)
   if (eclon .lt. 0.) eclon = eclon + 360.
   oblqec = 23.439 - .0000004*time
   eclon = eclon*rad
   oblqec = oblqec*rad

   num = cos(oblqec)*sin(eclon)
   den = cos(eclon)
   ra = atan(num/SIGN(MAX(ABS(den), SMALL), den))
   if (den .lt. 0) then
      ra = ra + pi
   else if (num .lt. 0) then
      ra = ra + twopi
   end if

   dec = asin(sin(oblqec)*sin(eclon))

   gmst = 6.697375 + .0657098242*time + hour

   gmst = mod(gmst,24.)
   if (gmst .lt. 0.) gmst = gmst + 24.

   lmst = gmst + lon/15.
   lmst = mod(lmst,24.)
   if (lmst .lt. 0.) lmst = lmst + 24.
   lmst = lmst*15.*rad

   ha = lmst - ra
   if (ha .lt. -pi) ha = ha + twopi
   if (ha .gt. pi) ha = ha - twopi
   latrad = lat*rad

   el = asin(sin(dec)*sin(latrad) + cos(dec)*cos(latrad)*cos(ha))
   az = asin(-cos(dec)*sin(ha)/SIGN(MAX(ABS(cos(el)), SMALL), cos(el)))

   if (sin(dec) - sin(el)*sin(latrad) .ge. 0.) then
      if (sin(az) .lt. 0.) az = az + twopi
   else
      az = pi - az
   end if

   el = 90 - el/rad
   az = az/rad

   if (az .lt. 0) then
      az = 360. + az
   endif

   if (ieee_is_nan(az) .and. lat .ge. -90. .and. lat .le. 90) then
     az = 270.
   endif

end subroutine


end module solar_position_m
