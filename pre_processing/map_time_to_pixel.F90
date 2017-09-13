!-------------------------------------------------------------------------------
! Name: map_time_to_pixel.F90
!
! Purpose:
! Map MODIS time data from EV frame to single imager pixels
!
! Description and Algorithm details:
! 1) A MODIS scan observes 10 along track pixels at once. The time at which ch30
!    (the edge of the focal plane) began collection is given by the
!    EV_start_time array (in seconds).
! 2) The start time for a 'field' of pixels in the along track direction is then
!    the start time for that block of 10 plus the number of 'cycles' that have
!    taken place (i.e. the number of across track pixels we are from the start).
! 3) For the centre of the focal place, this is,#
!       time = EV_start_time[x/10] + 333us * (y + 14),
!    where 333us is the time it takes to process one 'field' and 14 is the
!    number of field separating ch30 from the centre of the focal plane.
! 4) As the channels we use aren't all at the centre of the focal plane, this
!    value will be strictly wrong. However, as dealing with a different time for
!    each channel is far too complicated and the error is only significant for
!    ch31-32, this error is ignored.
! 5) The Julian date is then determined by dividing this number of seconds by
!    the number of seconds in a day.
!
! Arguments:
! Name              Type In/Out/Both Description
! ------------------------------------------------------------------------------
! along_track_ratio lint  in   Number of pixels collected in each scan (i.e. the
!                              ratio of length of latitude to EV_start_time)
! ixstart           lint  in   First pixel to read across track
! ixstop            lint  in   Last pixel to read across track
! iread_start       lint  in   First pixel to read along track
! iread_stop        lint  in   Last pixel to read along track
! iread_startt      lint  in   First pixel read along reduced resolution track
! iread_stopt       lint  in   Last pixel to read along reduced resolution track
! ttemp             dreal both Array into which pixel times will be written
! ttemp10           dreal in   Reduced resolution field of observation times
! refjulianday      dreal in   Julian day number for beginning of satellite time
!
! History:
! 2011/12/16, MJ: produces draft code
! 2012/06/13, MJ: reworks code and add ATBD formula
! 2013/09/06, AP: tidying
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine map_time_to_pixel(along_track_ratio,ixstart,ixstop,iread_start, &
     iread_stop,iread_startt,iread_stopt,ttemp,ttemp10,refjulianday)

   use imager_structures_m
   use preproc_constants_m

   implicit none

   integer(kind=lint), intent(in)  :: along_track_ratio
   integer(kind=lint), intent(in)  :: ixstart, ixstop
   integer(kind=lint), intent(in)  :: iread_start, iread_stop
   integer(kind=lint), intent(in)  :: iread_startt, iread_stopt
   real(kind=dreal),   intent(out) :: ttemp(ixstart:ixstop,iread_start:iread_stop)
   real(kind=dreal),   intent(in)  :: ttemp10(iread_startt:iread_stopt)
   real(kind=dreal),   intent(in)  :: refjulianday

   real(kind=dreal),   parameter :: tframe=333.333D-6

   integer(kind=lint), parameter :: f30=-14_lint

   integer(kind=lint) :: ix,jy,jyt

   do jy=iread_start,iread_stop
      !compute in which 10-line block the current pixel lies
      jyt = int((jy-1)/along_track_ratio)+1

      ! loop along the line and determine the time for ATBD3.0 formula.
      ! (page 3-23, http://modis.gsfc.nasa.gov/data/atbd/atbd_mod28_v3.pdf)
      ! MJ: Not sure this is really correct for the intended purpose
      ! AP: This gives the time for the 'ideal sensor' at the centre of the
      !     focal plane, for which the lat/lon are given. The channels we use
      !     are offset from that giving an error in this <5ms = 100m.
      do ix=ixstart,ixstop
         ttemp(ix,jy)=ttemp10(jyt)+tframe*real((ix-f30),kind=dreal)
      end do
   end do

   ! divide TAI seconds since 1/1/1993 by 86400 (every day has in the TAI system
   ! precisely this amount of seconds) and add to Julian date of 1/1/1993
   ! this gives the Julian date of the observation for each pixel.
   ttemp=refjulianday+ttemp/real(86400,kind=dreal)

end subroutine map_time_to_pixel
