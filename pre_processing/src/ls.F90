! Name: ls.F90
!
!
! Purpose:
! This module contains the Julian Dates and the total accumulated leapseconds whic hwere intrudruced until and including this date.
! The provided data is taken from http://maia.usno.navy.mil/ser7/tai-utc.dat which provides an overview of the introduced leap seconds.
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
! 2012/06/18: Matthias Jerg writes first version.
!
! $Id$
!
! Bugs:
!
!none known

module leap_seconds

  use preproc_constants

  implicit none

  integer(kind=stint),parameter :: nleaps=39

  real(kind=sreal) :: leap_on_jdays(nleaps)

  integer(kind=stint) :: leapseconds(nleaps)

  data leap_on_jdays / 2437300.5,2437512.5,2437665.5,2438334.5,2438395.5,2438486.5,2438639.5,&
       & 2438761.5,2438820.5,2438942.5,2439004.5,2439126.5,2439887.5,&
       & 2441317.5,2441499.5,2441683.5,2442048.5,2442413.5,2442778.5,&
       & 2443144.5,2443509.5,2443874.5,2444239.5,2444786.5,2445151.5,&
       & 2445516.5,2446247.5,2447161.5,2447892.5,2448257.5,2448804.5,&
       2449169.5,2449534.5,2450083.5,2450630.5,2451179.5,2453736.5,2454832.5,2456109.5 /


  data leapseconds / 1.4228180, 1.3728180, 1.8458580, 1.9458580,3.2401300,3.3401300,&
       & 3.4401300,3.5401300,3.6401300,3.7401300,3.8401300, &
       & 4.3131700,4.2131700,10.0,11.0,12.0,13.0,14.0,15.0,16.0,&
       & 17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,&
       & 29.0,30.0,31.0,32.0,33.0,34.0,35.0 /

end module leap_seconds
