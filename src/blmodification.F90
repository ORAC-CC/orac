!-------------------------------------------------------------------------------
! Name:
!    blmodification.F90
!
! Purpose:
!    Modifies temperature profile if a boundary layer inversion is found.
!
! Description:
!    From OCA ATBD
!
! Arguments:
!    Name   Type   In/Out/Both Description
!    SPixel struct Both Summarises the details of the pixel to be processed.
!
! Algorithm:
!    1) Loop from bottom of atmosphere up searching for a level that has lower
!       temperature than those around it whilst having pressure greater than
!       a specified limit.
!    2) If one is found, estimate the BL lapse rate from the two levels beneath
!       the inversion.
!    3) Overwrite the temperature profile within the inversion by extrapolting
!       from the level beneath it.
!
! History:
!    11/11/2014, C. Poulsen: First version
!    19/12/2014, C. Poulsen:  changed depth of boundary layer inversion from
!       1  -->2 as this seems to give better results. This is now consistent
!       with OCA(EUMETSAT) code.
!    30/01/2015, A. Povey: Tidying.
!
! Bugs:
!    Could use humidity subsidence inversion in the future
!
! $Id: Blmodification.F90 1963 2014-02-03 11:38:08Z acpovey $
!
!---------------------------------------------------------------------
subroutine Blmodification(SPixel)

   use SPixel_def

   implicit none

   !  Declare arguments
   type(SPixel_t), intent(inout) :: SPixel

   real               :: bllr               ! boundary layer lapse rate
   real,    parameter :: max_pressure=600.0 ! min allowed pressure
   integer            :: i,k,BLindexbottom,bltop
   integer, parameter :: depth1 = 2         ! min number of layers in inversion
   integer, parameter :: depth2 = 2         ! added to depth of inversion
   logical, parameter :: test = .false.     ! Activates printout


   BLindexbottom = 0

   ! Loop from bottom to top of atmosphere
   do i = SPixel%RTM%LW%Np-1,2,-1
      ! Check that we haven't left the boundary layer
      if (SPixel%RTM%LW%P(i) .le. max_pressure) exit

      ! Search for level where the one above is at least 1K greater to avoid
      ! small-scale surface inversions
      if (SPixel%RTM%LW%T(i-1) - SPixel%RTM%LW%T(i) .gt. 1.0) then
         if (SPixel%RTM%LW%T(i+1) .gt. SPixel%RTM%LW%T(i)) then
            BLindexbottom = i
            exit
         end if
      endif
   end do


   if (BLindexbottom .gt. 0) then
      ! Modify boundary layer temperature inversion profile

      ! Estimate the BL lapse rate from the two levels beneath the inversion
      bllr=(SPixel%RTM%LW%T(BLindexbottom+1)-SPixel%RTM%LW%T(BLindexbottom+2))/ &
         (SPixel%RTM%LW%P(BLindexbottom+1)-SPixel%RTM%LW%P(BLindexbottom+2))

      ! Search for top of boundary layer. Begin at -2 to force it to have
      ! some depth
      do k=BLindexbottom-depth1,1,-1
         if (SPixel%RTM%LW%T(k) .lt. SPixel%RTM%LW%T(k+1)) then
            bltop=k
            exit
         endif
      enddo

      ! print out new temperature profile
      if (test) then
         write(*,*)'bltop bottom',bltop,BLindexbottom
         write(*,*)'bl true'
         write(*,*)'SPixel%RTM%LW%P',SPixel%RTM%LW%P
         write(*,*)'SPixel%RTM%LW%T',SPixel%RTM%LW%T

         write(*,*),'press=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*)SPixel%RTM%LW%P(k),',$'

         enddo
         write(*,*)']'

         write(*,*),'told=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*)SPixel%RTM%LW%T(k),',$'

         enddo
         write(*,*)']'
      endif

      ! Increase depth of inversion
      bltop = bltop - depth2

      ! Modify temperature profile to replace profile where the inversion is
      do k=BLindexbottom,bltop,-1
         SPixel%RTM%LW%T(k) = SPixel%RTM%LW%T(BLindexbottom) + &
              bllr * (SPixel%RTM%LW%P(k) - SPixel%RTM%LW%P(BLindexbottom))
      enddo

      ! print out new temperature profile
      if (test) then
         write(*,*),'tnew=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*)SPixel%RTM%LW%T(k),',$'
         enddo
         write(*,*)']'
      endif

   endif ! bltrue

end subroutine Blmodification
