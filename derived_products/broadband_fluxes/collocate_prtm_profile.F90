!-------------------------------------------------------------------------------
! Name: collocate_prtm_profile.F90
!
! Purpose:
!
! Inputs:
!
! Output:
!
! History:
! xxxx/xx/xx, MC: Initial implementation
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine collocate_prtm_profile(lonval,latval,xdim,ydim,lonarr,latarr,lonid, &
                                  latid)

   implicit none

   ! Input arguments
   real, intent(in) :: lonval,latval
   integer, intent(in) :: xdim,ydim
   real, intent(in), dimension(xdim) :: lonarr
   real, intent(in), dimension(ydim) :: latarr

   ! Output arguments
   integer, intent(out) :: lonid,latid

   ! Local variables
   real, dimension(20) :: all_lonID,all_latID,all_dist
   integer :: i,j,ct,minid(1)

   all_lonID(:) = 0.0
   all_latID(:) = 0.0
   all_dist(:) = 900000.
   ct = 1
   do i = 1, xdim
      if (abs(lonval-lonarr(i)) .lt. 0.5) then
         do j = 1, ydim
            if (abs(latval-latarr(j)) .lt. 0.5) then
               all_lonID(ct) = i
               all_latID(ct) = j
               all_dist(ct) = sqrt( (lonval-lonarr(i))**2. + (latval-latarr(j))**2.)
               ct = ct+1
!              print*,i,j
            end if
         end do
      end if
   end do
!  print*,all_lonID(1:ct-1)
!  print*,all_latID(1:ct-1)
!  print*,all_dist(1:ct-1)
   minid = minloc(all_dist(1:ct))
   lonid = all_lonID(minid(1))
   latid = all_latID(minid(1))

end subroutine collocate_prtm_profile
