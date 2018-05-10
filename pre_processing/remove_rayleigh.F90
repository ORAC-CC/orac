!-------------------------------------------------------------------------------
! Name: remove_rayleigh.F90
!
! Purpose:
! Removes the Rayleigh scattering component from a transmittance profile and a
! total transmittance from TOA to the surface.
! (Phillip Watts, personal communication, June, 2015.)
!
! History:
! 2015/06/30, GM: First version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module remove_rayleigh_m

   implicit none

   private

   public :: remove_rayleigh

contains

subroutine remove_rayleigh(n_channels, n_levels, lambda, satza, p, &
                           transmittance, transmittance_total)

   use preproc_constants_m

   implicit none

   integer, intent(in)    :: n_channels
   integer, intent(in)    :: n_levels
   real,    intent(in)    :: lambda(:)
   real(8), intent(in)    :: satza
   real(8), intent(in)    :: p(:)
   real(8), intent(inout) :: transmittance(:,:)
   real(8), intent(inout) :: transmittance_total(:)

   integer :: i, j
   real    :: p_0
   real    :: sec_vza
   real    :: tau_ray_0
   real    :: tau_ray_p

   p_0 = 1013.

   sec_vza = 1. / cos(satza * d2r)

   do i = 1, n_channels
      ! Rayleigh optical thickness for the atmosphere down to 1013
      ! hPa (Hansen and Travis, 1974)
      tau_ray_0 = .008569 * lambda(i)**(-4) * &
           (1. + .0113 * lambda(i)**(-2) + .00013 * lambda(i)**(-4))

      do j = 1, n_levels
         ! Pressure and path dependent Rayleigh optical thickness
         tau_ray_p = tau_ray_0 * p(j) / p_0 * &
              sec_vza

         ! Corrected level transmittances
         transmittance(j, i) = &
              transmittance(j, i) / exp(-tau_ray_p)
      end do

      ! Corrected total transmittances
      transmittance_total(i) = &
           transmittance_total(i) / exp(-tau_ray_p)
   end do

end subroutine remove_rayleigh

end module remove_rayleigh_m
