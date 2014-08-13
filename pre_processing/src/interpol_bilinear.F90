!-------------------------------------------------------------------------------
! Name: interpol_bilinear.F90
!
! Purpose:
! Does bilinear interpolation on regularly gridded data for a vector of locations
!
! Description and algorithm details
! For each point in the vector of output locations:
!  * The indices of the four points surrounding the desired point are located
!    using the bisection method. NOTE: This requires the axes of the gridded
!    data to be strictly in either increases or decreasing order by value.
!  * The data is then linearly interpolated in the x-direction at each of the
!    two surrounding y-vertices.
!  * The two intermediate x-interpolates are then linearly interpolated in the
!    y-direction
!
! Arguments:
! Name            Type     In/Out/Both Description
! ------------------------------------------------------------------------------
! xin             real        in       X-coordinate of the gridded data
! yin             real        in       Y-coordinate of the gridded data
! datain          real        in       The gridded data field to be interpolated
! xout            real        in       A vector of x-coordinates for the desired
!                                      values
! yout            real        in       A vector of y-coordinates for the desired
!                                      values
! dataout         real        out      On return, will contain the interpolated
!                                      values corresponding to xout & yout.
! missing         real   optional in   Specify the value which denotes missing
!                                      data in the input array. If missing values
!                                      are encountered, the code switches to
!                                      a nearest neighbour interpolate.
! History:
! 2013/03/15, GT: Written as a replacement for the old, very slow, version of
!   interpol_bilinear. This version is pretty much as fast as the nearest
!   neighbour interpolation (which also uses the bisection method).
! 2013/05/23, GT: Added the missing keyword.
! 2013/12/15, GM: Deal with cases when the x or y coordinates for the desired
!   values are outside the range of x or y coordinates of the gridded data. In
!   these cases the interpolated result is now bounded to the gridded data.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine interpol_bilinear(xin, yin, datin, xout, yout, datout, missing)

   use preproc_constants

   implicit none

   ! Arguments
   real(kind=sreal), dimension(:),   intent(in)  :: xin,yin
   real(kind=sreal), dimension(:,:), intent(in)  :: datin
   real(kind=sreal), dimension(:),   intent(in)  :: xout, yout
   real(kind=sreal), dimension(:),   intent(out) :: datout
   real(kind=sreal), optional,       intent(in)  :: missing
   ! Local variables
   real(kind=sreal)                              :: intxbot, intxtop
   integer(kind=lint)                            :: nxin, nyin, nout
   integer(kind=lint)                            :: out
   integer(kind=lint)                            :: ii, iibot, iitop, iiold
   integer(kind=lint)                            :: jj, jjbot, jjtop, jjold
   integer(kind=sint),dimension(4)               :: missmask

   nxin = size(xin)
   nyin = size(yin)

   nout = size(xout)

   do out=1,nout
      ! Are the x/y axis values increases or decreasing?
      if (xin(1).lt.xin(2)) then ! Increasing x
         iibot = 1
         iitop = nxin
      else ! Decreasing x
         iibot = nxin
         iitop = 1
      end if
      if (yin(1).lt.yin(2)) then ! Increasing y
         jjbot = 1
         jjtop = nyin
      else ! Decreasing y
         jjbot = nyin
         jjtop = 1
      end if
      iiold = iibot
      jjold = jjbot

      do
         ii = (iibot + iitop) / 2
         jj = (jjbot + jjtop) / 2
         if (xout(out) .gt. xin(ii)) iibot = ii
         if (xout(out) .lt. xin(ii)) iitop = ii
         if (yout(out) .gt. yin(jj)) jjbot = jj
         if (yout(out) .lt. yin(jj)) jjtop = jj
         if (ii .eq. iiold .and. jj .eq. jjold) exit
         iiold = ii
         jjold = jj
      end do
      ! After the above process, iibot/iitop jjbot/jjtop will be the coordinates
      ! either side of our value of interest, so we can easily do the linear
      ! interpolation...

      ! If the missing argument has been provided, we check our bracketing
      ! values for missing data. If we have missing data, then we revert
      ! to nearest neighbour. If all the surrounding values are missing
      ! then that's what we get back
      missmask=1
      if (present(missing)) then
         if (datin(iibot,jjbot).eq.missing) missmask(1) = 0
         if (datin(iitop,jjbot).eq.missing) missmask(2) = 0
         if (datin(iibot,jjtop).eq.missing) missmask(3) = 0
         if (datin(iitop,jjtop).eq.missing) missmask(4) = 0
      end if

      if (any(missmask.eq.0)) then
         if (missmask(4).eq.1) then
            datout(out) = datin(iitop,jjtop)
         else if (missmask(3).eq.1) then
            datout(out) = datin(iibot,jjtop)
         else if (missmask(2).eq.1) then
            datout(out) = datin(iitop,jjbot)
         else
            datout(out) = datin(iibot,jjbot)
         end if
      else
         ! Linearly interpolate in the x-direction
         if (iitop .eq. iibot) then
            intxbot = datin(iibot,jjbot)
            intxtop = datin(iibot,jjtop)
         else
            intxbot = datin(iibot,jjbot) + &
                 (xout(out)-xin(iibot)) * &
                 (datin(iitop,jjbot)-datin(iibot,jjbot))/(xin(iitop)-xin(iibot))
            intxtop = datin(iibot,jjtop) + &
                 (xout(out)-xin(iibot)) * &
                 (datin(iitop,jjtop)-datin(iibot,jjtop))/(xin(iitop)-xin(iibot))
         end if
         ! Now interpolate these intermediate values in the y-direction
         if (jjtop .eq. jjbot) then
            datout(out) = intxbot
         else
            datout(out) = intxbot + (yout(out)-yin(jjbot)) * &
                 (intxtop-intxbot) / (yin(jjtop)-yin(jjbot))
         end if
      end if
   end do

end subroutine interpol_bilinear
