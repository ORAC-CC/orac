!BLmodification.F90
!
! Purpose:
!
!    modifies boundary layer inversion if found
!
! Algorithm:
!
!    From OCA ATBD
!
! History:
!    First version 11/11/2014 C. Poulsen
!
! Bugs
!    None known
!
!    Could use humidity subsidence inver sion in the future
!
! $Id: Blmodification.F90 1963 2014-02-03 11:38:08Z acpovey $
!
!---------------------------------------------------------------------
subroutine Blmodification(SPixel)
   
   use SPixel_def

   implicit none
   
   !  Declare arguments
   type(SPixel_t), intent(inout) :: SPixel

   real    :: bllr,max_pressure ! boundary layer lapse rate
   integer :: i,j,k,np,BLindexbottom,newindex,test,bltop,depth
   logical :: bltrue



   max_pressure=600.0
   test=0
   !
   !loop from bottom to top
   !
   BLtrue=.false.

   do i = SPixel%RTM%LW%Np-1,2,-1

      !this is required to avoid small scale surface inversions
      if (abs(SPixel%RTM%LW%T(i) - SPixel%RTM%LW%T(i-1)) .gt. 1.0) then
         !this is required to avoid surface inversions
         if (SPixel%RTM%LW%T(i) .lt. SPixel%RTM%LW%T(i-1)) then

            if (SPixel%RTM%LW%T(i+1) .gt. SPixel%RTM%LW%T(i)) then
               ! add in this check so do not reach the tropopause
               if (SPixel%RTM%LW%P(i) .gt. max_pressure) then

                  BLtrue = .true.
                  BLindexbottom=i
                  exit
                  !exit out of loop straight awawy to save time
               endif
            end if !t check
         endif
      end if

   end do


   if (BLtrue) then
      ! modify boundary layer temperature inversion profile

      !find index 2 layers below invesrion so as to estimate the lapse rate

      ! this might actually be +2
      newindex=BLindexbottom+2

      ! calculate lapse rate using 2 levels below
      bllr=(SPixel%RTM%LW%T(newindex-1)-SPixel%RTM%LW%T(newindex))/(SPixel%RTM%LW%P(newindex-1)-SPixel%RTM%LW%P(newindex))

      !      write(*,*)'bllr',bllr

      ! make a search for top of boundary layer inversion; start from bl bottom

      depth=1.0
      !  do i = SPixel%RTM%LW%Np-depth,1,-1
      !begin at -2 to force some depth to the feature
      do k=BLindexbottom-2,1,-1

         if (SPixel%RTM%LW%T(k) .lt. SPixel%RTM%LW%T(k+1)) then
            bltop=k

            ! exit out of loop to improve the speed
            exit
         endif

      enddo


      !print out new temperature profile
      !

      if (test .eq. 1) then
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

      !modify temperature profile to reaplce profile wher the inversion is
      bltop=bltop-depth

      do k=BLindexbottom ,bltop,-1


         SPixel%RTM%LW%T(k)=SPixel%RTM%LW%T(BLindexbottom)+bllr*(SPixel%RTM%LW%P(k)-SPixel%RTM%LW%P(BLindexbottom))

      enddo

      !print out new temperature profile
      !
      if (test .eq. 1) then
         write(*,*),'tnew=[ $'
         do k=1,SPixel%RTM%LW%Np
            write(*,*)SPixel%RTM%LW%T(k),',$'

         enddo
         write(*,*)']'
      endif


   endif ! bltrue


end subroutine Blmodification
