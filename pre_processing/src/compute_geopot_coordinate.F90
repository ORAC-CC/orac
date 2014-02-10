! Name: compute_geopot_coordinate.F90
!
!
! Purpose:
! Compute geopotential vertical coorindate from pressure cooordinate
! 
! Description and Algorithm details:
! This code is based on details given in the following ECMWF documentation:
! http://www.ecmwf.int/research/ifsdocs/DYNAMICS/Chap2_Discretization4.html#961180
!
! Arguments:
! Name         Type In/Out/Both Description
! ------------------------------------------------------------------------------
! preproc_prtm struct Both Pressure-level information for RTTOV.
! ecmwf_dims   struct In   Dimensions of the ECMWF data.
!
! History:
! 2012/06/26: MJ writes original code version.
! 2014/02/10: AP Improved summations. Added loop over lat,lon to move this
!                call outside of read_ecmwf_nc.
!
! $Id$
!
! Bugs:
! none known
!

subroutine compute_geopot_coordinate(preproc_prtm, preproc_dims, ecmwf_dims)

   implicit none

   type(preproc_prtm_s), intent(inout) :: preproc_prtm
   type(preproc_dims_s), intent(in)    :: preproc_dims
   type(ecmwf_dims_s),   intent(in)    :: ecmwf_dims
   
   integer                             :: ii,ij,ik
   real(kind=sreal)                    :: virt_temp,p,pp1,logpp,r_ratio,alpha,sp
   real(kind=sreal)                    :: sum_term,add_term

   r_ratio=r_water_vap/(r_dry_air-1.0_sreal)

   ! convert model levels to pressure levels.
   do ik=1,ecmwf_dims%kdim
      preproc_prtm%pressure(:,:,ik)=0.5*(avector(ik) + avector(ik+1) + &
           (bvector(ik)+bvector(ik+1))*exp(preproc_prtm%lnsp))
   enddo
   
   ! compute the summation terms of the sum in (2.21) and necessary terms in 
   ! (2.22) & (2.23) from TOA down (index ik represents cell centers and cell
   ! upper boundaries (wrt height))
   do ij=preproc_dims%min_lat,preproc_dims%max_lat
      do ii=preproc_dims%min_lon,preproc_dims%max_lon
         ! this is the lowest level=surface, it also has the surface pressure
         preproc_prtm%phi_lev(ii,ij,ecmwf_dims%kdim+1)= &
              preproc_prtm%geopot(ii,ij)
         sp=exp(preproc_prtm%lnsp(ii,ij))

         ! sum from TOA down according to (2.21)
         do ik=ecmwf_dims%kdim,1,-1
            
            p=avector(ik)+bvector(ik)*sp !pressure at cell upper boundary
            pp1=avector(ik+1)+bvector(ik+1)*sp !pressure at cell lower boundary

            !logpp is logarithmic pressure difference, defined on cell centers
            if(p .gt. dither) then 
               logpp=log(pp1/p)
            else
               !TOA has zero pressure, therefore:
               logpp=log(pp1)
            endif

            !virtual temperature at cell centers
            virt_temp=preproc_prtm%temperature(ii,ij,ik)*(1.0_sreal + &
                 r_ratio*preproc_prtm%spec_hum(ii,ij,ik))
            sum_term=r_dry_air*virt_temp*logpp
      
            !alpha term used later to put gph on cell centers, s.b.
            if(ik .eq. 1) then
               alpha=log(2.0_sreal)
            else
               alpha=1.0_sreal-p/(pp1-p)*logpp
            end if
            !add_term dito to alpha term, s.b.
            add_term=alpha*r_dry_air*virt_temp

            ! perform sum
            preproc_prtm%phi_lev(ii,ij,ik)=preproc_prtm%phi_lev(ii,ij,ik+1) + &
                 sum_term
            preproc_prtm%phi_lay(ii,ij,ik)=preproc_prtm%phi_lev(ii,ij,ik+1) +&
                 add_term
         enddo
      end do
   end do
   
end subroutine compute_geopot_coordinate
