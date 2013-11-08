! Name: compute_geopot_coordinate.F90
!
!
! Purpose:
! Compute geopotential vertical coorindate from pressure cooordinate
! 
!
! Description and Algorithm details:
!This code is based on details given in the following ECMWF documentation:
!http://www.ecmwf.int/research/ifsdocs/DYNAMICS/Chap2_Discretization4.html#961180

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
!2012/06/26: Matthias Jerg writes original code version.
! $Id$
!
! Bugs:
!
!none known

!---------------------------------------------------
!---------------------------------------------------
subroutine compute_geopot_coordinate(ecmwf_dims,geopot,spec_hum,temperature,avector,bvector,sp,&
     & phi_lev,phi_lay)
!---------------------------------------------------
!---------------------------------------------------

  use ecmwf_structures

  use preproc_constants

  implicit none

  type(ecmwf_dims_s) :: ecmwf_dims

  integer(kind=lint) :: ik,ikk

  real(kind=sreal) :: virt_temp,geopot,sp,p,pp1,logpp,&
       & spec_hum(ecmwf_dims%kdim_ec),temperature(ecmwf_dims%kdim_ec),&
       & r_ratio,sum_term(ecmwf_dims%kdim_ec),add_term(ecmwf_dims%kdim_ec),alpha

  real(kind=sreal) :: avector(1:ecmwf_dims%kdim_ec+1),&
       & bvector(1:ecmwf_dims%kdim_ec+1),&
       & phi_lev(1:ecmwf_dims%kdim_ec+1),phi_lay(1:ecmwf_dims%kdim_ec)

  r_ratio=r_water_vap/(r_dry_air-1.0_sreal)
  
  sum_term=0.0_sreal

  !OK, this is prob. more complicated than it needs to be but here we go:
  !compute the summation terms of the sum in (2.21) and necessary terms in (2.22) & (2.23)
  !from TOA down (index ikk represents cell centers and cell upper boundaries (wrt height))
  do ikk=1,ecmwf_dims%kdim_ec

     p=avector(ikk)+bvector(ikk)*sp !pressure at cell upper boundary
     pp1=avector(ikk+1)+bvector(ikk+1)*sp !pressure at cell lower boundary
    
     !logpp is logarithmic pressure difference, is then defined again on cell centers
     if(p .gt. dither) then 
        logpp=log(pp1/p)
        !TOA has zero pressure, therefore:
     else
        logpp=log(pp1)

     endif
    
     !virtual temperature at cell centers
     virt_temp=temperature(ikk)*(1.0_sreal+r_ratio*spec_hum(ikk))
     sum_term(ikk)=r_dry_air*virt_temp*logpp
     !alpha term used later to put gph on cell centers, s.b.
     alpha=1.0_sreal-p/(pp1-p)*logpp
     if(ikk .eq. 1)  alpha=log(2.0_sreal)
     !write(*,*) alpha

     !add_term dito to alpha term, s.b.
     add_term(ikk)=alpha*r_dry_air*virt_temp

  enddo

  !write(*,*) 

  !loop over levels from TOA to last one above surface and compute geopotential
  !loop from from TOA down (index ik represents cell centers and cell upper boundaries (wrt height))
  !according to (2.21)
  do ik=1,ecmwf_dims%kdim_ec
    
     phi_lev(ik)=geopot+sum(sum_term(ik:ecmwf_dims%kdim_ec))

     !sum(sum_term(ik:ecmwf_dims%kdim_ec)),geopot/g_wmo

  enddo

  !this is the lowest level=surface, it also has the surface pressure
  phi_lev(ecmwf_dims%kdim_ec+1)=geopot

  !loop again to use the level term( further down wrt height) and
  !add the add_term to get to the layer centers
  !as given in (2.22)
  do ik=1,ecmwf_dims%kdim_ec
  
     phi_lay(ik)=phi_lev(ik+1)+add_term(ik)

  enddo


!!$  write(*,*) phi_lev/g_wmo
!!$
!!$  write(*,*)
!!$
!!$  write(*,*)  phi_lay/g_wmo

  !write(*,*) size(phi_lay),size(phi_lev)

  !  stop

end subroutine compute_geopot_coordinate
