!-------------------------------------------------------------------------------
! Name: t_extrap_tp1.f90
!
! Purpose:
! subroutine of find_tropopause routine
! 
! Description and Algorithm details:
! Find tropopause in a temperature profile, cut profile above and 
! replace by T decreasing with given lapse rate
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! Z0     real    in   Corresponding altitudes / km
! T0     real    in   Temperature profile / K
! Ztp    real    in   pressure at the tropopause
! lr     real    in   lapse rate
! t2     real    both The returned temperature profile
! nlevs  integer in   Number of elements in Z0 and T0
!
! History:
! 2012/05/24, CP: Original code based on idl t_extrap_tp.pro
!
! $Id$
!
! Bugs:
! not tested bfull of bugs
!-------------------------------------------------------------------------------

subroutine t_extrap_tp1(z,t,ztp,lr,t2,nlevs)

   implicit none

   integer, intent(in)                      :: nlevs
   real,    intent(inout), dimension(nlevs) :: t2
   real,    intent(in),    dimension(nlevs) :: t,z
   real,    intent(in)                      :: ztp,lr

   real    :: ttp,min,tdiff,tdiff_store,sf
   integer :: nt,zdim,t2dim,ii,index,n


   if (ztp .eq. -999.) then 
      t2=t
   else
      sf=10000.   

      tdiff_store=999
      do ii=1,nlevs
         tdiff=abs(z(ii)/sf-ztp)
         if (tdiff .lt. tdiff_store) then
            tdiff_store=tdiff
            index=ii
         end if
      enddo
      ttp=t(index)

      ! generate T profile with given lapse rate, passing through TP
      t2=(((z/sf)-ztp)*(lr))+ttp	

      where((z/sf) .lt. ztp)	! insert actual T below TP
         t2=t
      end where

   end if

end subroutine  t_extrap_tp1
