!-------------------------------------------------------------------------------
! Name: compute_geopot_coordinate.F90
!
! Purpose:
! Compute geopotential vertical coordinate from pressure coordinate
!
! Description and Algorithm details:
! This code is based on details given in S2.2.1 of the following ECMWF documentation:
! https://www.ecmwf.int/sites/default/files/elibrary/2023/81369-ifs-documentation-cy48r1-part-iii-dynamics-and-numerical-procedures.pdf
!
! Arguments:
! Name         Type In/Out/Both Description
! ------------------------------------------------------------------------------
! preproc_prtm struct Both Pressure-level information for RTTOV.
! ecmwf_dims   struct In   Dimensions of the ECMWF data.
!
! History:
! 2012/06/26, MJ: writes original code version.
! 2014/02/10, AP: Improved summations. Added loop over lat,lon to move this call
!    outside of read_ecmwf_nc.
! 2014/05/08, AP: Updated to new ecmwf structure.
! 2019/05/23, GT: Added check for valid data in each grid cell before doing
!    calculation
! 2024/07/01, DH: Change indexing to use preproc_dims for all dimensions
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine compute_geopot_coordinate(preproc_prtm, preproc_dims, ecmwf)

   use preproc_constants_m
   use preproc_structures_m

   implicit none

   type(preproc_prtm_t), intent(inout) :: preproc_prtm
   type(preproc_dims_t), intent(in)    :: preproc_dims
   type(ecmwf_t),        intent(inout) :: ecmwf

   integer          :: ii,ij,ik
   real(kind=sreal) :: virt_temp,p,pp1,logpp,r_ratio,alpha,sp
   real(kind=sreal) :: sum_term,add_term

   r_ratio = r_water_vap / (r_dry_air - 1.0_sreal)

   ! compute the summation terms of the sum in (2.21) and necessary terms in
   ! (2.22) & (2.23) from TOA down (index ik represents cell centers and cell
   ! upper boundaries (wrt height))
   do ij = 1, preproc_dims%ydim
      do ii = 1, preproc_dims%xdim
         ! Check to see we have data for this particular pixel
         if (preproc_prtm%lnsp(ii,ij) .ne. sreal_fill_value) then
            ! this is the lowest level=surface, it also has the surface pressure
            preproc_prtm%phi_lev(ii,ij,ecmwf%kdim+1) = preproc_prtm%geopot(ii,ij)
            sp = exp(preproc_prtm%lnsp(ii,ij))

            !pressure at cell lower boundary
            pp1 = ecmwf%avec(ecmwf%kdim+1) + ecmwf%bvec(ecmwf%kdim+1) * sp

            ! sum from surface up according to (2.21)
            do ik = ecmwf%kdim, 1, -1
               !pressure at cell upper boundary
               p = ecmwf%avec(ik) + ecmwf%bvec(ik) * sp
               preproc_prtm%pressure(ii,ij,ik) = 0.5 * (p + pp1)

               !logpp is logarithmic pressure difference, defined on cell centers
               if (p .gt. dither) then
                  logpp = log(pp1/p)
               else
                  !TOA has zero pressure, therefore:
                  logpp = log(pp1)
               end if

               !virtual temperature at cell centers
               virt_temp = preproc_prtm%temperature(ii,ij,ik) * (1.0_sreal + &
                    r_ratio * preproc_prtm%spec_hum(ii,ij,ik))
               sum_term = r_dry_air * virt_temp * logpp

               !alpha term used later to put gph on cell centers, s.b.
               if (ik .eq. 1) then
                  alpha = log(2.0_sreal)
               else
                  alpha = 1.0_sreal - p / (pp1 - p) * logpp
               end if
               !add_term dito to alpha term, s.b.
               add_term = alpha*r_dry_air*virt_temp

               ! perform sum
               preproc_prtm%phi_lev(ii,ij,ik) = preproc_prtm%phi_lev(ii,ij,ik+1) + &
                    sum_term
               preproc_prtm%phi_lay(ii,ij,ik) = preproc_prtm%phi_lev(ii,ij,ik+1) + &
                    add_term

               pp1 = p
            end do
         end if
      end do
   end do
end subroutine compute_geopot_coordinate

subroutine compute_geopot_coordinate_gfs(preproc_prtm, preproc_dims, ecmwf)

   use preproc_constants_m
   use preproc_structures_m

   implicit none

   type(preproc_prtm_t), intent(inout) :: preproc_prtm
   type(preproc_dims_t), intent(in)    :: preproc_dims
   type(ecmwf_t),        intent(inout) :: ecmwf

   integer          :: ii, ij, ik, kp, kdim
   real(kind=sreal) :: virt_temp, p, pp1, logpp, r_ratio, alpha, sp
   real(kind=sreal) :: sum_term, add_term, p1, p2, logp1, logp2, logpt
   real(kind=sreal) :: t1, t2, q1, q2, o1, o2

   r_ratio = r_water_vap / (r_dry_air - 1.0_sreal)
   kdim = 137

   do ij = 1, preproc_dims%ydim
      do ii = 1, preproc_dims%xdim
         if (preproc_prtm%lnsp(ii,ij) .ne. sreal_fill_value) then
            ! Surface geopotential and surface pressure
            preproc_prtm%phi_lev(ii,ij,kdim+1) = preproc_prtm%geopot(ii,ij)
            sp = exp(preproc_prtm%lnsp(ii,ij))
            pp1 = ecmwf%avec(kdim+1) + ecmwf%bvec(kdim+1)*sp

            ! Loop from top to bottom level
            do ik = kdim, 1, -1
               ! Pressure computation
               p = ecmwf%avec(ik) + ecmwf%bvec(ik)*sp
               preproc_prtm%pressure(ii,ij,ik) = 0.5 * (p + pp1)
               ! Interpolation of temperature and spec_hum
               if (preproc_prtm%pressure(ii,ij,ik) < ecmwf%pressure(ii,ij,1)) then
                  preproc_prtm%temperature(ii,ij,ik) = ecmwf%temperature(ii,ij,1)
                  preproc_prtm%spec_hum(ii,ij,ik)    = ecmwf%spec_hum(ii,ij,1)
                  preproc_prtm%ozone(ii,ij,ik)    = ecmwf%ozone(ii,ij,1)
               else if (preproc_prtm%pressure(ii,ij,ik) > ecmwf%pressure(ii,ij,41)) then
                  preproc_prtm%temperature(ii,ij,ik) = ecmwf%temperature(ii,ij,41)
                  preproc_prtm%spec_hum(ii,ij,ik)    = ecmwf%spec_hum(ii,ij,41)
                  preproc_prtm%ozone(ii,ij,ik)    = ecmwf%ozone(ii,ij,41)
               else
                  do kp = 1, 40
                     if (preproc_prtm%pressure(ii,ij,ik) >= ecmwf%pressure(ii,ij,kp) .and. preproc_prtm%pressure(ii,ij,ik) <= ecmwf%pressure(ii,ij,kp+1)) then
                        p1 = ecmwf%pressure(ii,ij,kp)
                        p2 = ecmwf%pressure(ii,ij,kp+1)

                        logp1 = log(p1)
                        logp2 = log(p2)
                        logpt = log(preproc_prtm%pressure(ii,ij,ik))

                        t1 = ecmwf%temperature(ii,ij,kp)
                        t2 = ecmwf%temperature(ii,ij,kp+1)
                        q1 = ecmwf%spec_hum(ii,ij,kp)
                        q2 = ecmwf%spec_hum(ii,ij,kp+1)
                        o1 = ecmwf%ozone(ii,ij,kp)
                        o2 = ecmwf%ozone(ii,ij,kp+1)

                        preproc_prtm%temperature(ii,ij,ik) = t1 + (t2 - t1)*(logpt - logp1)/(logp2 - logp1)
                        preproc_prtm%spec_hum(ii,ij,ik) = q1 + (q2 - q1)*(logpt - logp1)/(logp2 - logp1)
                        preproc_prtm%ozone(ii,ij,ik) = o1 + (o2 - o1)*(logpt - logp1)/(logp2 - logp1)
                        exit
                     end if
                  end do
               end if

               ! Geopotential height computation
               if (p > dither) then
                  logpp = log(pp1 / p)
               else
                  logpp = log(pp1)
               end if

               virt_temp = preproc_prtm%temperature(ii,ij,ik) * &
                           (1.0_sreal + r_ratio * preproc_prtm%spec_hum(ii,ij,ik))
               sum_term = r_dry_air * virt_temp * logpp

               if (ik == 1) then
                  alpha = log(2.0_sreal)
               else
                  alpha = 1.0_sreal - p / (pp1 - p) * logpp
               end if
               add_term = alpha * r_dry_air * virt_temp

               preproc_prtm%phi_lev(ii,ij,ik) = preproc_prtm%phi_lev(ii,ij,ik+1) + sum_term
               preproc_prtm%phi_lay(ii,ij,ik) = preproc_prtm%phi_lev(ii,ij,ik+1) + add_term

               pp1 = p
            end do
         end if
      end do
   end do
end subroutine compute_geopot_coordinate_gfs