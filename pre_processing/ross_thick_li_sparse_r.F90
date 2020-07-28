!-------------------------------------------------------------------------------
! Name: ross_thick_li_sparse_r.F90
!
! Purpose:
! Module contining routines to calculate land BRDF parameters using the Ross-Thick
! Li-Sparse-Reciprocal bidirectional reflectance kernel but the structure is set
! so that other kernels may be added.
!
! Description and Algorithm details:
! Lucht, Wolfgang, Crystal Barker Schaaf, and Alan H. Strahler. 2000. "An
! Algorithm for the Retrieval of Albedo from Space Using Semiempirical BRDF
! Models." IEEE Transactions on Geoscience and Remote Sensing 38 (2) (March):
! 977–998. doi:10.1109/36.841980.
!
! Schaaf, Crystal B., Feng Gao, Alan H. Strahler, Wolfgang Lucht, Xiaowen Li,
! Trevor Tsang, Nicholas C. Strugnell, et al. 2002. "First Operational BRDF,
! Albedo Nadir Reflectance Products from MODIS." Remote Sensing of Environment
! 83: 135–148. doi:10.1016/S0034-4257(02)00091-3.
!
! Wanner, W., X. Li, and A. H. Strahler. 1995. "On the Derivation of Kernels for
! Kernel-Driven Models of Bidirectional Reflectance." Journal of Geophysical
! Research 100 (D10) (October): 21077–21089. doi:10.1029/95JD02371.
!
! History:
! 2014/08/10, GM: First version.
! 2014/08/20, OS: marked C-style comment as Fortran comment
! 2014/12/31, GM: Parallelized the main loops in the interface subroutine with
!    OpenMP.
! 2015/01/05, GM: Fixed a couple of bugs in the OpenMP parallelization that come
!    out when compiling with ifort.
! 2017/08/09, GM: Switch from the NR routine gauleg() to the GPL compatible
!    gauss_leg_quadx() for computing Gauss-Legendre quadrature.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module ross_thick_li_sparse_r_m

   use preproc_constants_m

   implicit none

   private

   public :: ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd

   ! Structure containing auxiliary values common to all BRDF kernels
   type brdf_aux_t
      real(kind=sreal) :: theta1
      real(kind=sreal) :: cos_theta1
      real(kind=sreal) :: sin_theta1
      real(kind=sreal) :: tan_theta1
      real(kind=sreal) :: tan_theta1_2

      real(kind=sreal) :: theta2
      real(kind=sreal) :: cos_theta2
      real(kind=sreal) :: sin_theta2
      real(kind=sreal) :: tan_theta2
      real(kind=sreal) :: tan_theta2_2

      real(kind=sreal) :: phi
      real(kind=sreal) :: cos_phi
      real(kind=sreal) :: sin_phi
      real(kind=sreal) :: sin_phi_2
   end type brdf_aux_t

   ! Structure containing auxiliary values based on brdf_aux_t values that are
   ! common to both the Li-Sparse-R and Li-Dense-R kernels.
   type kernel_aux_li_r_t
      real(kind=sreal) :: a
      real(kind=sreal) :: b
      real(kind=sreal) :: c
      real(kind=sreal) :: d
      real(kind=sreal) :: e
      real(kind=sreal) :: g

      real(kind=sreal) :: tan_theta_i_p
      real(kind=sreal) :: tan_theta_r_p

      real(kind=sreal) :: tan_theta_i_p_2
      real(kind=sreal) :: tan_theta_r_p_2

      real(kind=sreal) :: theta_i_p
      real(kind=sreal) :: theta_r_p

      real(kind=sreal) :: cos_theta_i_p
      real(kind=sreal) :: sin_theta_i_p

      real(kind=sreal) :: cos_theta_r_p
      real(kind=sreal) :: sin_theta_r_p

      real(kind=sreal) :: r
   end type kernel_aux_li_r_t

   ! Structure containing auxiliary values based on brdf_aux_t values that are
   ! common to both the Ross-Thin and Ross-Thick kernels.
   type kernel_aux_ross_t
      real(kind=sreal) :: a
      real(kind=sreal) :: b
      real(kind=sreal) :: c
   end type kernel_aux_ross_t

   ! Structure containing auxiliary values for the Ross-Thick_Li-Sparse_R kernel.
   type kernel_aux_ross_thick_li_sparse_r_t
      type(kernel_aux_ross_t) :: ross
      type(kernel_aux_li_r_t) :: li_r
   end type kernel_aux_ross_thick_li_sparse_r_t

contains

!-------------------------------------------------------------------------------
! Name: brdf_aux_calc()
!
! Purpose:
! Calculate auxiliary values common to all BRDF kernels.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name   Type       In/Out/Both Description
! aux    brdf_aux_t Out         Structure containing auxiliary values common to
!                               all BRDF kernels
! theta1 real       In          Incident zenith angle
! theta2 real       In          Viewing zenith angle
! phi    real       In          Relative azimuth angle
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine brdf_aux_calc(aux, theta1, theta2, phi)

   implicit none

   type(brdf_aux_t), intent(out) :: aux
   real(kind=sreal), intent(in)  :: theta1
   real(kind=sreal), intent(in)  :: theta2
   real(kind=sreal), intent(in)  :: phi

   aux%theta1       = theta1
   aux%cos_theta1   = cos(theta1)
   aux%sin_theta1   = sin(theta1)
!  aux%sin_theta1   = sqrt(1. - aux%cos_theta1 * aux%cos_theta1)
   aux%tan_theta1   = aux%sin_theta1 / aux%cos_theta1
   aux%tan_theta1_2 = aux%tan_theta1 * aux%tan_theta1

   aux%theta2       = theta2
   aux%cos_theta2   = cos(theta2)
   aux%sin_theta2   = sin(theta2)
!  aux%sin_theta2   = sqrt(1. - aux%cos_theta2 * aux%cos_theta2)
   aux%tan_theta2   = aux%sin_theta2 / aux%cos_theta2
   aux%tan_theta2_2 = aux%tan_theta2 * aux%tan_theta2

   aux%phi          = phi
   aux%cos_phi      = cos(phi)
   aux%sin_phi      = sin(phi)
   aux%sin_phi_2    = aux%sin_phi * aux%sin_phi

end subroutine


!-------------------------------------------------------------------------------
! Name: li_aux_calc()
!
! Purpose:
! Calculate auxiliary values based on brdf_aux_t values that are common to both
! the Li-Sparse-R and Li-Dense-R kernels.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name Type              In/Out/Both Description
! aux  brdf_aux_t        In          Structure containing auxiliary values
!                                    common to all BRDF kernels
! aux2 kernel_aux_li_r_t Out         Structure containing auxiliary values based
!                                    on brdf_aux_t values that are common to
!                                    both the Li-Sparse-R and Li-
!                                    Dense-R kernels.
! p    rseal(2)          In          Li parameters
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine li_aux_calc(aux, aux2, p)

   implicit none

   type(brdf_aux_t),        intent(in)  :: aux
   type(kernel_aux_li_r_t), intent(out) :: aux2
   real(kind=sreal),        intent(in)  :: p(2)

   aux2%tan_theta_i_p   = p(1) * aux%tan_theta1
   aux2%tan_theta_r_p   = p(1) * aux%tan_theta2

   aux2%tan_theta_i_p_2 = aux2%tan_theta_i_p * aux2%tan_theta_i_p
   aux2%tan_theta_r_p_2 = aux2%tan_theta_r_p * aux2%tan_theta_r_p

   aux2%theta_i_p       = atan(aux2%tan_theta_i_p)
   aux2%theta_r_p       = atan(aux2%tan_theta_r_p)

   aux2%cos_theta_i_p   = cos(aux2%theta_i_p)
   aux2%sin_theta_i_p   = sin(aux2%theta_i_p)

   aux2%cos_theta_r_p   = cos(aux2%theta_r_p)
   aux2%sin_theta_r_p   = sin(aux2%theta_r_p)

   aux2%a               = aux2%cos_theta_i_p * aux2%cos_theta_r_p
   aux2%b               = aux2%sin_theta_i_p * aux2%sin_theta_r_p

   aux2%c               = aux2%tan_theta_i_p_2 + aux2%tan_theta_r_p_2
   aux2%d               = aux2%tan_theta_i_p   * aux2%tan_theta_r_p * 2.
   aux2%e               = aux2%tan_theta_i_p_2 * aux2%tan_theta_r_p_2

   aux2%r               = 1. / aux2%cos_theta_i_p + 1. / aux2%cos_theta_r_p

   aux2%g               = p(2) / aux2%r

end subroutine


!-------------------------------------------------------------------------------
! Name: li_common()
!
! Purpose:
! To isolate computations common to both the Li-Sparse-R and Li-Dense-R kernels.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name Type              In/Out/Both Description
! aux  brdf_aux_t        In          Structure containing auxiliary values
!                                    common to all BRDF kernels
! aux2 kernel_aux_li_r_t In          Structure containing auxiliary values
!                                    based on brdf_aux_t values that are common
!                                    to both the Li-Sparse-R and Li-Dense-R
!                                    kernels.
! p    sreal(2)          In          Li parameters
! p_   sreal             Out         An important value
! q    sreal             Out         An important value
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine li_common(aux, aux2, p, p_, q)

   implicit none

   type(brdf_aux_t),        intent(in)  :: aux
   type(kernel_aux_li_r_t), intent(in)  :: aux2
   real(kind=sreal),        intent(in)  :: p(2)
   real(kind=sreal),        intent(out) :: p_
   real(kind=sreal),        intent(out) :: q

   real(kind=sreal) :: cos_ksi_p
   real(kind=sreal) :: d
   real(kind=sreal) :: h
   real(kind=sreal) :: cos_t

   cos_ksi_p = aux2%a + aux2%b * aux%cos_phi

!  p_        = (1. + cos_ksi_p) / aux2%cos_theta_r_p
   p_        = (1. + cos_ksi_p) / (aux2%cos_theta_i_p * aux2%cos_theta_r_p)

   d         = sqrt(aux2%c - aux2%d * aux%cos_phi)

   h         = sqrt(d * d + aux2%e * aux%sin_phi_2)

   cos_t     = aux2%g * h

   if (cos_t .gt. 1.) then ! overlap area = 0
     q = 1.
   else
     q = 1. - (acos(cos_t) - sqrt(1. - cos_t * cos_t) * cos_t) / PI
   end if

end subroutine


!-------------------------------------------------------------------------------
! Name: li_sparse_kernel()
!
! Purpose:
! Calculate the Li-Sparse kernel value.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name Type              In/Out/Both Description
! aux  brdf_aux_t        In          Structure containing auxiliary values
!                                    common to all BRDF kernels
! aux2 kernel_aux_li_r_t In          Structure containing auxiliary values
!                                    based on brdf_aux_t values that are common
!                                    to both the Li-Sparse-R and
!                                    Li-Dense-R kernels.
! p    sreal(2)          In          Li parameters
! K    sreal             Out         The kernel value
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine li_sparse_kernel(aux, aux2, p, K)

   implicit none

   type(brdf_aux_t),        intent(in)  :: aux
   type(kernel_aux_li_r_t), intent(in)  :: aux2
   real(kind=sreal),        intent(in)  :: p(2)
   real(kind=sreal),        intent(out) :: K

   real(kind=sreal) :: p_
   real(kind=sreal) :: q

   call li_common(aux, aux2, p, p_, q)

   K = .5 * p_ - q * aux2%r

end subroutine


!-------------------------------------------------------------------------------
! Name: ross_aux_calc()
!
! Purpose:
! Calculate auxiliary values based on brdf_aux_t values that are common to both
! the Ross-Thin and Ross-Thick kernels.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name Type              In/Out/Both Description
! aux  brdf_aux_t        In          Structure containing auxiliary values
!                                    common to all BRDF kernels
! aux2 kernel_aux_li_r_t Out         Structure containing auxiliary values
!                                    based on brdf_aux_t values that are common
!                                    to both the Ross-thin and Ross-thick
! p    sreal(0)          In          Ross parameters
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine ross_aux_calc(aux, aux2, p)

   implicit none

   type(brdf_aux_t),        intent(in)  :: aux
   type(kernel_aux_ross_t), intent(out) :: aux2
   real(kind=sreal),        intent(in)  :: p(1)

   aux2%a = aux%cos_theta1 * aux%cos_theta2
   aux2%b = aux%sin_theta1 * aux%sin_theta2
   aux2%c = aux%cos_theta1 + aux%cos_theta2

end subroutine


!-------------------------------------------------------------------------------
! Name: ross_thick_kernel()
!
! Purpose:
! Calculate the Ross-Thick kernel value.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name Type              In/Out/Both Description
! aux  brdf_aux_t        In          Structure containing auxiliary values
!                                    common to all BRDF kernels
! aux2 kernel_aux_li_r_t Out         Structure containing auxiliary values
!                                    based on brdf_aux_t values that are common
!                                    to both the Ross-thin and Ross-thick
! p    sreal(0)          In          Ross parameters
! K    sreal             Out         The kernel value
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine ross_thick_kernel(aux, aux2, p, K)

   use preproc_constants_m

   implicit none

   type(brdf_aux_t),        intent(in)  :: aux
   type(kernel_aux_ross_t), intent(in)  :: aux2
   real(kind=sreal),        intent(in)  :: p(1)
   real(kind=sreal),        intent(out) :: K

   real(kind=sreal) :: cos_ksi
   real(kind=sreal) :: ksi

   cos_ksi = aux2%a + aux2%b * aux%cos_phi

   ksi     = acos(cos_ksi)

   K = ((pi / 2. - ksi) * cos_ksi + sin(ksi)) / aux2%c - pi / 4.

end subroutine


!-------------------------------------------------------------------------------
! Name: ross_thick_li_sparse_r_aux_calc()
!
! Purpose:
! Calculate auxiliary values based on brdf_aux_t values for this kernel.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name   Type              In/Out/Both Description
! aux    brdf_aux_t        In          Structure containing auxiliary values
!                                      common to all BRDF kernels
! aux2   kernel_aux_li_r_t Out         Structure containing auxiliary values
!                                      based on brdf_aux_t values that are
!                                      common to the Ross-Thick_Li-Sparse_R
!                                      kernel.
! p_ross sreal(0)          In          Ross parameters
! p_li   sreal(2)          In          Li   parameters
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine ross_thick_li_sparse_r_aux_calc(aux, aux_kernel, p_ross, p_li_r)

   use preproc_constants_m

   implicit none

   type(brdf_aux_t),                          intent(in)  :: aux
   type(kernel_aux_ross_thick_li_sparse_r_t), intent(out) :: aux_kernel
   real(kind=sreal),                          intent(in)  :: p_ross(1)
   real(kind=sreal),                          intent(in)  :: p_li_r(2)

   call ross_aux_calc(aux, aux_kernel%ross, p_ross)
   call li_aux_calc  (aux, aux_kernel%li_r, p_li_r)

end subroutine


!-------------------------------------------------------------------------------
! Name: ross_thick_li_sparse_r_kernel()
!
! Purpose:
! Calculate the Ross-Thick_Li-Sparse_R kernel value.
!
! Description and Algorithm details:
! See file header.
!
! Arguments:
! Name   Type              In/Out/Both Description
! aux    brdf_aux_t        In          Structure containing auxiliary values
!                                      common to all BRDF kernels
! aux2   kernel_aux_li_r_t Out         Structure containing auxiliary values
!                                      based on brdf_aux_t values that are
!                                      common to the Ross-Thick_Li-Sparse_R
!                                      kernel.
! p_ross sreal(0)          In          Ross parameters
! p_li   sreal(2)          In          Li   parameters
! f      sreal(3)          In          Ross-Thick_Li-Sparse_R weights
! K      sreal             Out         The kernel value
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine ross_thick_li_sparse_r_kernel(aux, aux_kernel, p_ross, p_li_r, f, K)

   use preproc_constants_m

   implicit none

   type(brdf_aux_t),                             intent(in)  :: aux
   type(kernel_aux_ross_thick_li_sparse_r_t),    intent(in)  :: aux_kernel
   real(kind=sreal),                             intent(in)  :: p_ross(1)
   real(kind=sreal),                             intent(in)  :: p_li_r(2)
   real(kind=sreal),                             intent(in)  :: f(:)
   real(kind=sreal),                             intent(out) :: K

   real(kind=sreal) :: K_ross_thick
   real(kind=sreal) :: K_li_r_sparse

   call ross_thick_kernel(aux, aux_kernel%ross, p_ross, K_ross_thick)
   call li_sparse_kernel (aux, aux_kernel%li_r, p_li_r, k_li_r_sparse)

   K = f(1) + f(2) * k_ross_thick + f(3) * k_li_r_sparse

end subroutine


!-------------------------------------------------------------------------------
! Name: ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd()
!
! Purpose:
! Compute quantities used in the ORAC fast forward model.
!
! Description and Algorithm details:
! This is mostly just Gauss-Legendre integrations over hemispheres.
!
! Arguments:
! Name    Type    In/Out/Both Description
! n_bands integer In          Wavelength band index numbers
! solza   sreal   In          Array of solar zenith angles (in degrees)
! satza   sreal   In          Array of satellite zenith angles
! relaz   sreal   In          Array of relative azimuth angles (between sun and
!                             satellite)
! f(3)    sreal   In          Ross-Thick_Li-Sparse_R weights
! rho_0v  sreal   Out         The nbands x npoints output array of solar beam to
!                             satellite view reflectances
! rho_0d  sreal   Out         The nbands x npoints output array of solar beam to
!                             diffuse reflectances
! rho_dv  sreal   Out         The nbands x npoints output array of diffuse to
!                             satellite view reflectances
! rho_dd  sreal   Out         The nbands x npoints output array of diffuse to
!                             diffuse reflectances
!
! Local variables:
! Name Type Description
!-------------------------------------------------------------------------------
subroutine ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd(n_bands, solza, satza, &
   relaz, f, fill_value, rho_0v, rho_0d, rho_dv, rho_dd, verbose)

   use preproc_constants_m
   use gauss_leg_quad_m

   implicit none

   ! Input arguments
   integer,          intent(in)    :: n_bands
   real(kind=sreal), intent(in)    :: solza(:)
   real(kind=sreal), intent(in)    :: satza(:)
   real(kind=sreal), intent(in)    :: relaz(:)
   real(kind=sreal), intent(in)    :: f(:, :, :)
   real(kind=sreal), intent(in)    :: fill_value
   logical,          intent(in)    :: verbose

   ! Output arguments
   real(kind=sreal), intent(inout) :: rho_0v(:,:)
   real(kind=sreal), intent(inout) :: rho_0d(:,:)
   real(kind=sreal), intent(inout) :: rho_dv(:,:)
   real(kind=sreal), intent(inout) :: rho_dd(:,:)

   ! Local variables
   integer                       :: i
   integer                       :: j
   integer                       :: k
   integer                       :: l
   integer                       :: m
   integer                       :: n_points
   integer, parameter            :: n_quad_theta = 4
   integer, parameter            :: n_quad_phi   = 4

   real(kind=sreal), parameter   :: p_ross(1) = (/0./)
   real(kind=sreal), parameter   :: p_li_r(2) = (/1., 2./)

   real(kind=sreal), allocatable :: qx_theta(:)
   real(kind=sreal), allocatable :: qw_theta(:)

   real(kind=sreal), allocatable :: qx_phi(:)
   real(kind=sreal), allocatable :: qw_phi(:)

   real(kind=sreal), allocatable :: qx_cos_sin_qw_theta(:)

   real(kind=sreal)              :: a
   real(kind=sreal)              :: a2
   real(kind=sreal)              :: a3
   real(kind=sreal)              :: solza2
   real(kind=sreal)              :: satza2
   real(kind=sreal)              :: relaz2

   type(brdf_aux_t)                                        :: aux_brdf
   type(brdf_aux_t), allocatable                           :: aux_brdf2(:,:)
   type(brdf_aux_t), allocatable                           :: aux_brdf3(:,:,:)
   type(kernel_aux_ross_thick_li_sparse_r_t)               :: aux_kernel
   type(kernel_aux_ross_thick_li_sparse_r_t), allocatable  :: aux_kernel2(:,:)
   type(kernel_aux_ross_thick_li_sparse_r_t), allocatable  :: aux_kernel3(:,:,:)


   n_points = size(solza)


   !----------------------------------------------------------------------------
   ! Allocate arrays
   !----------------------------------------------------------------------------
   allocate(qx_theta(n_quad_theta))
   allocate(qw_theta(n_quad_theta))

   allocate(qx_phi  (n_quad_phi  ))
   allocate(qw_phi  (n_quad_phi  ))

   allocate(qx_cos_sin_qw_theta(n_quad_theta))


   !----------------------------------------------------------------------------
   ! Calculate Gauss-Legendre quadrature over zenith theta and azimuthal phi
   !----------------------------------------------------------------------------
   call gauss_leg_quadx(n_quad_theta, 0., pi / 2., qx_theta, qw_theta)
   call gauss_leg_quadx(n_quad_phi,   0., 2. * pi, qx_phi,   qw_phi)

   do i = 1, n_quad_theta
      qx_cos_sin_qw_theta(i) = cos(qx_theta(i)) * sin(qx_theta(i)) * qw_theta(i)
   end do


   !----------------------------------------------------------------------------
   if (verbose) print *, 'ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd(): computing rho_0v'
   !----------------------------------------------------------------------------
!$OMP PARALLEL PRIVATE(i, j, solza2, satza2, relaz2, aux_brdf, aux_kernel)
!$OMP DO SCHEDULE(GUIDED)
   do i = 1, n_points
      if (solza(i) .gt. maxsza_twi) then
            rho_0v(:, i) = fill_value
            cycle
      end if

      solza2 = solza(i) * d2r
      satza2 = satza(i) * d2r
      relaz2 = relaz(i) * d2r

      call brdf_aux_calc(aux_brdf, solza2, satza2, relaz2)
      call ross_thick_li_sparse_r_aux_calc(aux_brdf, aux_kernel, p_ross, p_li_r)

      do j = 1, n_bands
         if (any(f(:, j, i) .eq. fill_value)) then
            rho_0v(j, i) = fill_value
            cycle
         end if

         call ross_thick_li_sparse_r_kernel(aux_brdf, aux_kernel, p_ross, &
                                            p_li_r, f(:, j, i), rho_0v(j, i))
      end do
   end do
!$OMP END DO
!$OMP END PARALLEL

   !----------------------------------------------------------------------------
   if (verbose) print *, 'ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd(): computing rho_0d'
   !----------------------------------------------------------------------------
   rho_0d = 0.
!$OMP PARALLEL PRIVATE(i, j, k, l, a, a2, solza2, satza2, relaz2, aux_brdf2, aux_kernel2)
   allocate(aux_brdf2(n_quad_theta, n_quad_phi))
   allocate(aux_kernel2(n_quad_theta, n_quad_phi))
!$OMP DO SCHEDULE(GUIDED)
   do i = 1, n_points
      if (solza(i) .gt. maxsza_twi) then
            rho_0d(:, i) = fill_value
            cycle
      end if

      solza2 = solza(i) * d2r
      do j = 1, n_quad_theta
         satza2 = qx_theta(j)
         do k = 1, n_quad_phi
            relaz2 = qx_phi(k)
            call brdf_aux_calc(aux_brdf2(j,k), solza2, satza2, relaz2)
            call ross_thick_li_sparse_r_aux_calc(aux_brdf2(j,k), &
                                                 aux_kernel2(j,k), p_ross, &
                                                 p_li_r)
         end do
      end do

      do j = 1, n_bands
         if (any(f(:, j, i) .eq. fill_value)) then
            rho_0d(j, i) = fill_value
            cycle
         end if

         do k = 1, n_quad_theta
            a = 0.
            do l = 1, n_quad_phi
               call ross_thick_li_sparse_r_kernel(aux_brdf2(k,l), &
                                                  aux_kernel2(k,l), p_ross, &
                                                  p_li_r, f(:, j, i), a2)
               a = a + a2 * qw_phi(l)
            end do
            rho_0d(j, i) = rho_0d(j, i) + a * qx_cos_sin_qw_theta(k)
         end do

         rho_0d(j, i) = rho_0d(j, i) / pi
      end do
   end do
!$OMP END DO
   deallocate(aux_brdf2)
   deallocate(aux_kernel2)
!$OMP END PARALLEL

   !----------------------------------------------------------------------------
   if (verbose) print *, &
        'ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd(): computing rho_dv'
   !----------------------------------------------------------------------------
   rho_dv = 0.
!$OMP PARALLEL PRIVATE(i, j, k, l, a, a2, solza2, satza2, relaz2, aux_brdf2, aux_kernel2)
   allocate(aux_brdf2(n_quad_theta, n_quad_phi))
   allocate(aux_kernel2(n_quad_theta, n_quad_phi))
!$OMP DO SCHEDULE(GUIDED)
   do i = 1, n_points
      satza2 = satza(i) * d2r
      do j = 1, n_quad_theta
         solza2 = qx_theta(j)
         do k = 1, n_quad_phi
            relaz2 = qx_phi(k)
            call brdf_aux_calc(aux_brdf2(j,k), solza2, satza2, relaz2)
            call ross_thick_li_sparse_r_aux_calc(aux_brdf2(j,k), &
                                                 aux_kernel2(j,k), p_ross, &
                                                 p_li_r)
         end do
      end do

      do j = 1, n_bands
         if (any(f(:, j, i) .eq. fill_value)) then
            rho_dv(j, i) = fill_value
            cycle
         end if

         do k = 1, n_quad_theta
            a = 0.
            do l = 1, n_quad_phi
               call ross_thick_li_sparse_r_kernel(aux_brdf2(k,l), &
                                                  aux_kernel2(k,l), &
                                                  p_ross, p_li_r, f(:, j, i), a2)
               a = a + a2 * qw_phi(l)
            end do
            rho_dv(j, i) = rho_dv(j, i) + a * qx_cos_sin_qw_theta(k)
         end do

         rho_dv(j, i) = rho_dv(j, i) / pi
      end do
   end do
!$OMP END DO
   deallocate(aux_brdf2)
   deallocate(aux_kernel2)
!$OMP END PARALLEL

   !----------------------------------------------------------------------------
   if (verbose) print *, &
        'ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd(): computing rho_dd'
   !----------------------------------------------------------------------------
   allocate(aux_brdf3(n_quad_theta, n_quad_theta, n_quad_phi))
   allocate(aux_kernel3(n_quad_theta, n_quad_theta, n_quad_phi))

   do j = 1, n_quad_theta
      solza2 = qx_theta(j)
      do k = 1, n_quad_theta
         satza2 = qx_theta(k)
         do l = 1, n_quad_phi
            relaz2 = qx_phi(l)
            call brdf_aux_calc(aux_brdf3(j,k,l), solza2, satza2, relaz2)
            call ross_thick_li_sparse_r_aux_calc(aux_brdf3(j,k,l), &
                                                 aux_kernel3(j,k,l), &
                                                 p_ross, p_li_r)
         end do
      end do
   end do

   rho_dd = 0.
!$OMP PARALLEL PRIVATE(i, j, k, l, m, a, a2, a3)
!$OMP DO SCHEDULE(GUIDED)
   do i = 1, n_points
      do j = 1, n_bands
         if (any(f(:, j, i) .eq. fill_value)) then
            rho_dd(j, i) = fill_value
            cycle
         end if

         do k = 1, n_quad_theta
            a = 0.
            do l = 1, n_quad_theta
               a2 = 0.
               do m = 1, n_quad_phi
                  call ross_thick_li_sparse_r_kernel(aux_brdf3(k,l,m), &
                                                     aux_kernel3(k,l,m), &
                                                     p_ross, p_li_r, &
                                                     f(:, j, i), a3)
                  a2 = a2 + a3 * qw_phi(m)
               end do
               a = a + a2 * qx_cos_sin_qw_theta(l) / pi
            end do
            rho_dd(j, i) = rho_dd(j, i) + a * qx_cos_sin_qw_theta(k)
         end do

         rho_dd(j, i) = rho_dd(j, i) * 2.
      end do
   end do
!$OMP END DO
!$OMP END PARALLEL
   deallocate(aux_brdf3)
   deallocate(aux_kernel3)

   !----------------------------------------------------------------------------
   ! Deallocate arrays
   !----------------------------------------------------------------------------
   deallocate(qx_theta)
   deallocate(qw_theta)

   deallocate(qx_phi)
   deallocate(qw_phi)

   deallocate(qx_cos_sin_qw_theta)

end subroutine ross_thick_li_sparse_r_rho_0v_0d_dv_and_dd

end module ross_thick_li_sparse_r_m
