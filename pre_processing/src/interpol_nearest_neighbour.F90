!-------------------------------------------------------------------------------
! Name: interpol_nearest_neighbour.F90
!
! Purpose:
! Does a nearest neighbour on regularly gridded data for a vector of locations
!
! Description and algorithm details
!
! Arguments:
! ------------------------------------------------------------------------------
! Name    Type     In/Out/Both Description
! xin     real        in       X-coordinate of the gridded data
! yin     real        in       Y-coordinate of the gridded data
! datain  real        in       The gridded data field to be interpolated
! xout    real        in       A vector of x-coordinates for the desired values
! yout    real        in       A vecotor of y-coordinates for the desired values
! dataout real        out      On return, will contain the interpolated
!                              values correspoinding to xout & yout.
!
! History:
! 2012/06/27, GT: Finished first version
! 2014/07/10, AP: Removed redundant code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine interpol_nearest_neighbour(xin, yin, datin, xout, yout, datout)

   use preproc_constants

   implicit none

   ! Arguments
   real(kind=sreal), dimension(:),   intent(in)  :: xin,yin
   real(kind=sreal), dimension(:,:), intent(in)  :: datin
   real(kind=sreal), dimension(:),   intent(in)  :: xout, yout
   real(kind=sreal), dimension(:),   intent(out) :: datout

   ! Local variables
   integer(kind=lint)                            :: nxin, nyin, nout, out

   integer(kind=lint)                            :: ii, jj, ii_old,jj_old
   integer(kind=lint)                            :: iibot,iitop,jjbot,jjtop

   nxin = size(xin)
   nyin = size(yin)

   nout = size(xout)

   ! For each desired point, find the nearest location in x & y
   ! directions and extract the corresponding data value.
   do out=1,nout
      iibot=1
      iitop=nxin
      jjbot=1
      jjtop=nyin
      ii_old=iibot
      jj_old=jjbot
      !MST I believe y axis is upside down...
      do
         ii=(iibot+iitop)/2
         jj=(jjbot+jjtop)/2
         if (xout(out) .gt. xin(ii)) iibot = ii
         if (xout(out) .lt. xin(ii)) iitop = ii
         if (yout(out) .gt. yin(jj)) jjtop = jj
         if (yout(out) .lt. yin(jj)) jjbot = jj
         if (ii .eq. ii_old .and. jj .eq. jj_old) exit
         ii_old = ii
         jj_old = jj
      end do
      datout(out) = datin(ii,jj)
   end do

end subroutine interpol_nearest_neighbour
