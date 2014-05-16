! Name: extrap_into_tropopause.f90
!
!
! Purpose:
! find troppuaes given temperature and height profile and remove by extrapolating into stratosphere
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!	Z0	Corresponding altitudes / km
!	T0	Temperature profile / K
!
!
! Local variables:
! ztp : tropopause
!
!
!
! Name Type Description
!
!
! History:
! 2012/05/24:Original code C. Poulsen based on idl t_extrap_tp.pro
! 2014/03/14:CP debugged and tested
!
! $Id$
!
! Bugs:
!not tested bfull of bugs
!none known

subroutine extrap_into_tropopause(preproc_prtm,preproc_dims )
  
  use preproc_structures

  implicit none
  ! arguments
  type(preproc_prtm_s), intent(inout) :: preproc_prtm
  type(preproc_dims_s), intent(in)   :: preproc_dims 

  real ,allocatable ::  t(:),z(:),t2(:),t3(:)
  real ,allocatable ::  t2a(:)
  real ::  lr,ztp,ttp,min_t,maxz,ztp2
  integer  :: nt,zdim,t2adim,i,j,nlevs
 
  do j=preproc_dims%min_lat,preproc_dims%max_lat
     do i=preproc_dims%min_lon,preproc_dims%max_lon
        


        if (preproc_prtm%temperature(i,j,1) .gt. 0) then 



        nlevs=size(preproc_prtm%temperature(i,j,:))

        allocate(t(nlevs))
        allocate(t2(nlevs))
        allocate(t2a(nlevs))
        allocate(t3(nlevs))
        allocate(z(nlevs))
        
        
        t=preproc_prtm%temperature(i,j,:)
        z=preproc_prtm%phi_lev(i,j,:)
! this is atypical value for a moist adiabatic lapse rate (see wikipedia)
        lr=-5 !lapse rate K/km
        if (z(1) .le. z(2)) then
           write(*,*),'z must be in descending order !'
        end if
        
!
!find location of tropopause
!

        call find_tropopause(t,z,ztp,nlevs)
        
        if (ztp .eq. -999) then
           write(*,*),'tropopause not found !',ztp
        end if
        
!
!now extrapolate the tropopause into stratosphere
!
        call t_extrap_tp1(z,t,ztp,lr,t2,nlevs)

        ! minimum temperature allowed - 
        !below this weak lapse rate assumed to avoid non-zero KN and help convergence
        min_t=100. 
        where(t2 .gt. min_t)
           t2a=t2
        end where
        
        t2adim=size(t2a)
        zdim=size(z)
        
        
        if (t2adim .ne. zdim) then 
           maxz=maxval(z)
           lr=-50./(maxz-ztp2)
           call t_extrap_tp1(z,t2,ztp2,lr,t3)
           
           t2=t3
        end if
        
        
 
!
!assign new extrapolated profile to prtm structure
!
        
        
        preproc_prtm%temperature(i,j,:)=t2


        deallocate(t)
        deallocate(t2)
        deallocate(t2a)
        deallocate(t3)
        deallocate(z)
     endif
  enddo
enddo




end subroutine  extrap_into_tropopause
