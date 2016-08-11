

! CVS:  $Id: bugsrad_planck.F90,v 1.1 2003/08/06 19:41:13 norm Exp $
! CVS:  $Name:  $

module bugsrad_planck

   use kinds
   implicit none

   integer, private, parameter :: mbir = 12  !Number of IR bands
   real (kind=dbl_kind), private, dimension(6, MBIR) ::  &
      b = reshape ((/ -25.889132,     0.75038381,     -0.87074567e-02,  0.50701144e-04, -0.14856755e-06,  0.17579587e-09, &     !Band 1    Coefficients for fitted polynomial
                       25.397471,    -0.59596460,      0.53117737e-02, -0.21681758e-04,  0.36630792e-07, -0.11541419e-10, &     !Band 2    which computes the blackbody flux emission
                       57.891546,    -1.4745788,       0.14577775e-01, -0.68637478e-04,  0.14707480e-06, -0.98862337e-10, &     !Band 3    as a function of temperature and band.
                       21.837317,    -0.63194381,      0.71338812e-02, -0.38569394e-04,  0.95685257e-07, -0.76188561e-10, &     !Band 4
                      0.83155466,    -0.15281669,      0.31020500e-02, -0.23768837e-04,  0.74605666e-07, -0.67494167e-10, &     !Band 5
                      -19.432674,     0.37744942,     -0.22166529e-02,  0.11663914e-05,  0.22128830e-07, -0.28943829e-10, &     !Band 6
                      -51.844021,      1.2280373,     -0.10600353e-01,  0.38135251e-04, -0.45111018e-07,  0.16679671e-10, &     !Band 7
                      -31.210771,     0.85737498,     -0.87947387e-02,  0.39416747e-04, -0.67469797e-07,  0.43711306e-10, &     !Band 8
                      -5.4417604,     0.28970317,     -0.44571665e-02,  0.26395273e-04, -0.52111967e-07,  0.37627129e-10, &     !Band 9
                       14.646543,    -0.25202253,      0.67234738e-03,  0.67552180e-05, -0.19815201e-07,  0.17221281e-10, &     !Band 10
                       12.218584,    -0.31591213,      0.26032011e-02, -0.58878366e-05,  0.73276694e-08, -0.38798834e-11, &     !Band 11
                       1.0183416,    -0.79710154e-01,  0.13753393e-02, -0.40247214e-05,  0.63186167e-08, -0.41250652e-11 /), &  !Band 12
                       (/ 6, mbir /))

   contains
      subroutine planck  &
         ( ncol,         &     ! Input:  Number of columns
           nlm,          &     ! Input:  Number of model layers
           nbir,         &     ! Input:  IR band number
           ts,           &     ! Input:  surface temperature, K
           tt,           &     ! Input:  atmospheric layer temperature, K
           bf   )              ! Output: blackbody emission, W/m^2

         integer (kind=int_kind), intent(in) ::   &
            ncol,   &
            nlm,    &
            nbir
         real (kind=dbl_kind), intent(in), dimension(:) :: &
            ts
         real (kind=dbl_kind), intent(in), dimension(:,:) :: &
            tt
         real (kind=dbl_kind), intent(out), dimension(:,:) :: &
            bf


         !Local variables
         integer (kind=int_kind) :: &
            i_lay     !Layer index
         real(kind=dbl_kind), dimension(NCOL) :: &
             tmp      !Tmp var to hold interface temperature


         !Blackbody emission at top-of-model
         bf(:,1) = b(1,nbir)+tt(:,1)*(b(2,nbir)+tt(:,1)*(b(3,nbir)+tt(:,1)*(b(4,nbir)+tt(:,1)*(b(5,nbir)+tt(:,1)*b(6,nbir)))))
         !Emission at remaining interfaces
         do i_lay = 2,nlm
              tmp(:) = 0.5*(tt(:,i_lay-1)+tt(:,i_lay))
              bf(:,i_lay)= b(1,nbir)+tmp(:)*(b(2,nbir)+tmp(:)*(b(3,nbir)+tmp(:)*(b(4,nbir)+tmp(:)*(b(5,nbir)+tmp(:)*b(6,nbir)))))
           !end do
         end do
         !Surface emission
         bf(:,nlm+1) = b(1,nbir)+ts(:)*(b(2,nbir)+ts(:)*(b(3,nbir)+ts(:)*(b(4,nbir)+ts(:)*(b(5,nbir)+ts(:)*b(6,nbir)))))
         return
      end subroutine planck

end module bugsrad_planck
