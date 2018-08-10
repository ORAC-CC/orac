

! CVS: $Id: cloud_correlate.F90,v 1.3 2005/11/22 22:32:22 norm Exp $
! CVS: $Name:  $

module bugs_cloud_correlate
   use kinds
   implicit none
   real(kind=dbl_kind), PARAMETER :: MIN_CF = 1.0e-6   !Minimum layer cloud fraction considered cloudy
   real(kind=dbl_kind), PARAMETER :: MIN_LC = 1.0      !Minimum correlation length in meters
                                                       !If L_c is smaller than this, random is assumed
   contains

   subroutine bugs_ctot(nlen, len_loc, nlm, pl2,tl, acld, l_c, c_tot)
      !Calculates the total cloud amount using the profile of cloud
      !fraction acld and the correlation length l_c
      integer (kind=int_kind), intent(in) ::  &
         nlen,               & ! Length of total domain
         len_loc,            & ! Length of sub domain
         nlm                   ! Number of layers
      real (kind=dbl_kind), intent(in), dimension(nlen) ::  &
         l_c                   ! Correlation length
      real(kind=dbl_kind), intent(in), dimension(nlen,nlm) ::   &
         tl,                 & ! Layer temperature
         acld                  ! Radiative cloud fraction
      real(kind=dbl_kind), intent(in), dimension(nlen,nlm+1) :: &
         pl2                   ! Level pressure
      real(kind=dbl_kind), intent(out), dimension(nlen) ::  &
         c_tot                 ! Total cloud amount

      ! Local variables
      integer(kind=int_kind) :: &
         i_lay_a, i_domain, ncloud, kcld, j   ! Indices
      integer(kind=int_kind), dimension(:), allocatable :: &
         i_cld                                      ! Indices of cloudy layers
      real(kind=dbl_kind) :: &
         z0,                 &
         dz,                 &
         alpha
      real(kind=dbl_kind), dimension(nlm) :: &
         z                     ! mid-layer heights estimated via hydrostatic equation
      real(kind=dbl_kind), dimension(:), allocatable :: &
         cloud,              &
         olap,               &
         cld_below             ! Amount of cloud in each layer that lies under clear
                               ! sky, i.e., that contributes to total cloudiness

      do i_domain = 1, nlen
         !Figure out the cloudy layers
         ncloud = count(acld(i_domain,:) > min_cf)
         if (ncloud == 0) then
            !No cloud
            c_tot(i_domain) = 0.
         else if (ncloud == 1) then
            !One cloudy layer
            c_tot(i_domain) = maxval(acld(i_domain,:))
         else
            if (maxval(acld(i_domain,:)) >= 1.) then
               c_tot(i_domain) = 1.0
            else
               !Multiple cloudy layers.
               ! Get the mid-layer heights
               ! Don't worry about actual surface elevation, since we only use dz's
               z0 = 0.
               do i_lay_a = nlm, 1, -1
                  dz = 29.286*log(pl2(i_domain,i_lay_a+1)/pl2(i_domain,i_lay_a))*tl(i_domain,i_lay_a)
                  z(i_lay_a) = z0 + dz/2.
                  z0 = z0 + dz
               end do

               !First figure out what layers the clouds are in
               allocate(i_cld(ncloud),cloud(ncloud),olap(ncloud-1), cld_below(ncloud))
               kcld = 0
               do i_lay_a = 1, nlm
                  if (acld(i_domain,i_lay_a) > min_cf) then
                     kcld = kcld+1
                    i_cld(kcld) = i_lay_a
                  end if
               end do
               !Second, condense the cloud fraction vector to remove any cloud-free layers
               !and set up the olap and cld_below vectors
               cloud(1) = acld(i_domain,i_cld(1))
               do  kcld = 2, ncloud
                  cloud(kcld) = acld(i_domain,i_cld(kcld))
                  !Overlap of this layer with prior
                  if (l_c(i_domain) < min_lc) then
                     !Pure random correlation between this pair of layers
                     olap(kcld-1) = cloud(kcld-1)*cloud(kcld)
                  else
                     dz=z(i_cld(kcld-1)) - z(i_cld(kcld))
                     alpha = exp(-dz/l_c(i_domain))
                     olap(kcld-1) = alpha*min(cloud(kcld-1),cloud(kcld)) + &
                                  (1. - alpha)*cloud(kcld-1)*cloud(kcld)
                  end if
               end do

               !Third, compute the total cloud amount
               cld_below(1) = cloud(1)
               cld_below(2) = cloud(2) - olap(1)
               do kcld = 3,ncloud
                  do j = kcld-1,1,-1
                     select case (kcld-j)
                        case(1)
                           cld_below(kcld) = cloud(kcld) - olap(kcld-1)
                        case(2:)
                           cld_below(kcld) = cld_below(kcld)*(1. - (cloud(j)-olap(j))/&
                                                                    (1. - cloud(j+1)))
                     end select
                  end do
               end do
               c_tot(i_domain) = sum(cld_below)
               deallocate(i_cld, cloud,olap, cld_below)
            end if
         end if
      end do
      return
   end subroutine bugs_ctot


   subroutine bugs_cloudfit(len, nlm, c_tot, acld, &
                            c_maximal, cf_max, cf_random)
      use kinds
      implicit none

      integer (kind=int_kind), intent(in) :: &
        len,  &       !Length of domain
        nlm           !Number of layers in each column
      real (kind=dbl_kind), intent(in), dimension(len) :: &
        c_tot         !Total cloud fraction computed by bugs_ctot
      real (kind=dbl_kind), intent(in), dimension(len,nlm) :: &
        acld          !Vector of cloud fractions for each column
      real (kind=dbl_kind), intent(out), dimension(len) :: &
        c_maximal     !Fraction of column which is maximally overlapped
      real (kind=dbl_kind), intent(out), dimension(len,nlm) :: &
        cf_max,    &  !Vector of layer cloud fractions inside the maximally-overlapped column
        cf_random     !Vector of layer cloud fractions outside the maximally-overlapped column


      !LOCAL VARIABLES
       integer(kind=int_kind) :: i, j, nc

       real(kind=dbl_kind) :: min_frac, max_frac
       real(kind=dbl_kind) :: prod, c_tot_max, cf_stacked, &
                              c_tot_calcd,tol,delta
       !real(kind=dbl_kind), dimension(nlm) :: cf_tmp
       integer(kind=int_kind) :: frac_set

       !Use these below to reproduce original gs_olap results
       !real(kind=dbl_kind) :: cf_stacked_thresh,  &
       !                       cf_stacked_min
       !integer(kind=int_kind) :: cf_stacked_min_set

      !Set tol
      tol = 1.0e-5
      do i = 1,len
         !For each column, get the max and min nonzero cloud fractions
         frac_set = 0
         min_frac = 1.
         max_frac = 0.
         nc = 0
         do j = 1, nlm
            if (acld(i,j) > min_cf) then
               frac_set = 1
               nc = nc + 1
               if (acld(i,j) < min_frac) then
                  min_frac = acld(i,j)
               end if
               if (acld(i,j) > max_frac) then
                  max_frac = acld(i,j)
               end if
            end if
         end do
         if (frac_set == 0) then
            !No nonzero cloud amount in column, clear sky
            min_frac = 0.
            max_frac = 0.
            c_maximal(i) = 0.
            cf_max(i,:) = 0.
            cf_random(i,:) = 0.
         else
            !Min_frac and max_frac determined for this column

            !Determine c_tot for purely random sky (maximum c_tot possible for this sky)
            prod = 1.
            do j = 1, nlm
               prod = prod*(1. - acld(i,j))
            end do
            c_tot_max = 1. - prod
            !Now determine the stacked (maximally overlapped) cloud fraction using c_tot to constrain
            !Check to see if the actual c_tot is less than c_tot_max by a nonnegligible amount
            !If so, run an iteration in which cf_stacked is incremented until c_tot_calcd
            !is just less than c_tot.  Note that cf_stacked is not allowed to exceed the maximum
            !layer cloud fraction, max_frac
            if (c_tot_max - c_tot(i) > tol) then
               cf_stacked = 0.
               c_tot_calcd = c_tot_max
               delta = max_frac/1000.
               do while (c_tot_calcd > c_tot(i) .and. cf_stacked <= max_frac)
                  !Compute c_tot_calcd for the given cf_stacked
                  prod = 1.
                  do j = 1, nlm
                     if (acld(i,j) > cf_stacked) then
                        prod = prod*(1. - acld(i,j))/(1. - cf_stacked)
                     end if
                  end do
                  c_tot_calcd = cf_stacked + (1 - cf_stacked)*(1. - prod)
                  cf_stacked = cf_stacked + delta
               end do
               if (cf_stacked > max_frac) then
                  !Reached end of iterations and c_tot_calcd is not less than c_tot
                  !Set cf_stacked to its maximum value.  This makes c_tot_calcd as small
                  !as possible
                  cf_stacked = max_frac
               else
                  !Good result, converged such that c_tot_calcd < c_tot before cf_stacked
                  !got too large.
                  !For good measure, back up half a step
                  cf_stacked = cf_stacked - delta/2.
               end if
            else
               !The actual c_tot is greater than or equal to c_tot_max, so set cf_stacked to
               !its minimum value.  This makes c_tot_calcd as large as possible
               !If only one cloud layer, must have cf_stacked = the cloud fraction in that
               !layer.  Otherwise (i.e., Nc > 1), we can have 0. overlap and cf_stacked = 0.
               if (nc == 1) then
                  cf_stacked = min_frac
               else
                  cf_stacked = 0.
               end if
            end if

            !To reproduce the original gsolap calcs
            !Comment between here and the next comment to use constrained gsolap
            ! cf_stacked_min = 1.
            ! cf_stacked_thresh = 0.00
            ! cf_stacked_min_set = 0

            ! do j = 1, nlm
            !    if (acld(i,j) < cf_stacked_min  .and.  acld(i,j) > cf_stacked_thresh) then
            !       cf_stacked_min = acld(i,j)
            !       cf_stacked_min_set = 1
            !    end if
            ! end do
            ! if (cf_stacked_min_set == 0) then
            !     cf_stacked = 0
            ! else
            !     cf_stacked = cf_stacked_min
            ! end if
            ! cf_stacked = 0. !Force random
            !End reproduce original gsolap calcs

            c_maximal(i) = cf_stacked
            do j = 1, nlm
               if (acld(i,j) >= cf_stacked) then
                  cf_max(i,j) = cf_stacked
                  cf_random(i,j) = acld(i,j) - cf_stacked
               else
                  cf_max(i,j) = acld(i,j)
                  cf_random(i,j) = 0.
               end if
            end do
            !If c_maximal is other than zero or 1, renormalize the cloud fractions
            !to the range [0. - 1.]
            if (c_maximal(i) > 0. .and. c_maximal(i) < 1.) then
               cf_max(i,:) = cf_max(i,:)/c_maximal(i)
               cf_random(i,:) = cf_random(i,:)/(1. - c_maximal(i))
            end if

            !Diagnostic calc - comment out if not debugging
            !prod = 1.
            !do j = 1, nlm
            !  if (acld(i,j) > cf_stacked) then
            !      prod = prod*(1. - acld(i,j))/(1. - cf_stacked)
            !   end if
            !end do
            !c_tot_calcd = cf_stacked + (1 - cf_stacked)*(1. - prod)
         end if
      end do
   end subroutine bugs_cloudfit

end module bugs_cloud_correlate
