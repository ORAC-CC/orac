! Name: create_time_for_pixel.F90
!
!
! Purpose:
! Compute time for each pixel from AVHRR start and end time of oribt
! 
! Description and Algorithm details:
! 1) Assume each across track row is observed at once and each has the same
!    duration.
! 2) Convert into a Julian date and output.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ixstart       lint   in   First pixel to read across track
! ixstop        lint   in   Last pixel to read across track
! iystart       lint   in   First pixel to read along track
! iystop        lint   in   Last pixel to read along track
! n_along_track lint   in   Number of pixels available in the 
! imager_time   struct both Summary of pixel observation time
! startepochs   lint   in   Start time for the orbit, in seconds
! endepochs     lint   in   End time for the orbit, in seconds
! refjulianday  dreal  in   Julian day number for reference date
!
! History:
! 2012/06/14: MJ produces draft code
! 2013/09/06: AP tidying
!
! $Id$
!
! Bugs:
! none known
!

subroutine create_time_for_pixel(ixstart,ixstop,iystart,iystop, &
     n_along_track,startepochs,endepochs,imager_time,refjulianday)

   use preproc_constants
   use imager_structures

   implicit none

   integer(kind=lint)  :: jy,ixstart,ixstop,iystart,iystop

   real(kind=dreal)    :: deltime

   real(kind=dreal)    :: temp(ixstart:ixstop,iystart:iystop)

   integer(kind=lint)  :: n_along_track
  
   integer(kind=lint)  :: startepochs,endepochs

   real(kind=dreal)    :: refjulianday

   type(imager_time_s) :: imager_time

   deltime=(endepochs-startepochs)/real(n_along_track,kind=dreal)

   do jy=iystart,iystop
      temp(:,jy)=real(startepochs,kind=dreal)+(jy-1)*deltime
   enddo

   !divide TAI seconds since 1/1/1970 by 86400 (every day has in the TAI system
   !precicesly this amount of seconds) and add to Julian date of 1/1/1970
   !this gives the Julian date of the obeservation for each pixel.
   temp=refjulianday+temp/real(86400,kind=dreal)

   imager_time%time=temp

end subroutine create_time_for_pixel
