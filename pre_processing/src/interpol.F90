!-------------------------------------------------------------------------------
! Name: interpol.F90
!
! Purpose:
! Container for interpolation routines. To interpolate a field, call
! bilinear_coefs and then interp_field. The structure interpol passes information
! between them.
!
! Description and Algorithm details:
! The interpol structure contains the fields:
!   x0 integer Index of the row just below the desired point.
!   x1 integer Index of the row just above the desired point.
!   y0 integer Index of the column just left of the desired point.
!   y1 integer Index of the column just right of the desired point.
!   t  real    Fractional distance of desired point from y0.
!   u  real    Fractional distance of desired point from x0.
!
! Arguments:
! None
!
! History:
! 2014/05/23, GM: First version.
! 2014/08/05, AP: New bilinear interpolation routines.
!
! $Id$
!
! Bugs:
! With arrays, interp_field only checks if the first value is missing.
!-------------------------------------------------------------------------------

module interpol

   use preproc_constants, only: sreal, real_fill_value
   
   implicit none
   
   type interpol_s
      integer     :: x0, x1, y0, y1
      real(sreal) :: t, u
   end type interpol_s

   interface bilinear_coef
      module procedure bilinear_coef_reg_reg,  bilinear_coef_reg_irr, &
                       bilinear_coef_irr_reg,  bilinear_coef_irr_irr, &
                       bilinear_coef_reg_reg2, bilinear_coef_reg_irr2, &
                       bilinear_coef_irr_reg2, bilinear_coef_irr_irr2
   end interface bilinear_coef
   
   interface interp_field
      module procedure interp_field_0d, interp_field_1d, interp_field_2d
   end interface interp_field

contains

include 'bound_grid.F90'
   
include 'interpol_bilinear.F90'
!include 'interpol_nearest_neighbour.F90'

subroutine bilinear_coef_reg_reg(xstart, x_invdel, nx, ystart, y_invdel, ny, &
     xout, yout, interp, wrap)
   implicit none

   real(8),          intent(in)  :: xstart, x_invdel
   real(8),          intent(in)  :: ystart, y_invdel
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp
   logical,          intent(in)  :: wrap

   call bound_regular_grid(xstart, x_invdel, nx, xout, &
        interp%x0, interp%x1, interp%t, wrap)
   call bound_regular_grid(ystart, y_invdel, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_reg_reg

subroutine bilinear_coef_reg_irr(xstart, x_invdel, nx, ygrid, ny, &
     xout, yout, interp, wrap)
   implicit none

   real(8),          intent(in)  :: xstart, x_invdel
   real(sreal),      intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp
   logical,          intent(in)  :: wrap

   call bound_regular_grid(xstart, x_invdel, nx, xout, &
        interp%x0, interp%x1, interp%t, wrap)
   call bound_irregular_grid(ygrid, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_reg_irr

subroutine bilinear_coef_irr_reg(xgrid, nx, ystart, y_invdel, ny, &
     xout, yout, interp, wrap)
   implicit none

   real(sreal),      intent(in)  :: xgrid(:)
   real(8),          intent(in)  :: ystart, y_invdel
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp
   logical,          intent(in)  :: wrap

   call bound_irregular_grid(xgrid, nx, xout, &
        interp%x0, interp%x1, interp%t, wrap)
   call bound_regular_grid(ystart, y_invdel, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_irr_reg

subroutine bilinear_coef_irr_irr(xgrid, nx, ygrid, ny, &
     xout, yout, interp, wrap)
   implicit none

   real(sreal),      intent(in)  :: xgrid(:)
   real(sreal),      intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp
   logical,          intent(in)  :: wrap

   call bound_irregular_grid(xgrid, nx, xout, &
        interp%x0, interp%x1, interp%t, wrap)
   call bound_irregular_grid(ygrid, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_irr_irr

subroutine bilinear_coef_reg_reg2(xstart, x_invdel, nx, ystart, y_invdel, ny, &
     xout, yout, interp)
   implicit none

   real(8),          intent(in)  :: xstart, x_invdel
   real(8),          intent(in)  :: ystart, y_invdel
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp

   call bound_regular_grid(xstart, x_invdel, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_regular_grid(ystart, y_invdel, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_reg_reg2

subroutine bilinear_coef_reg_irr2(xstart, x_invdel, nx, ygrid, ny, &
     xout, yout, interp)
   implicit none

   real(8),          intent(in)  :: xstart, x_invdel
   real(sreal),      intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp

   call bound_regular_grid(xstart, x_invdel, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_irregular_grid(ygrid, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_reg_irr2

subroutine bilinear_coef_irr_reg2(xgrid, nx, ystart, y_invdel, ny, &
     xout, yout, interp)
   implicit none

   real(sreal),      intent(in)  :: xgrid(:)
   real(8),          intent(in)  :: ystart, y_invdel
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp

   call bound_irregular_grid(xgrid, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_regular_grid(ystart, y_invdel, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_irr_reg2

subroutine bilinear_coef_irr_irr2(xgrid, nx, ygrid, ny, &
     xout, yout, interp)
   implicit none

   real(sreal),      intent(in)  :: xgrid(:)
   real(sreal),      intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real(sreal),      intent(in)  :: xout, yout
   type(interpol_s), intent(out) :: interp

   call bound_irregular_grid(xgrid, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_irregular_grid(ygrid, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_irr_irr2

subroutine interp_field_0d(datin, datout, interp)

   implicit none

   real(sreal), target, intent(in)    :: datin(:,:)
   real(sreal),         intent(inout) :: datout
   type(interpol_s),    intent(in)    :: interp

   logical     :: miss(4)
   real(sreal) :: coef(3)
   real(sreal) :: bot_left, bot_rght, top_left, top_rght

   ! decide that interpolation to do dependent on the missing values
   miss = [datin(interp%x0, interp%y0) == real_fill_value, &
           datin(interp%x1, interp%y0) == real_fill_value, &
           datin(interp%x0, interp%y1) == real_fill_value, &
           datin(interp%x1, interp%y1) == real_fill_value]

   bot_left = datin(interp%x0, interp%y0)
   bot_rght = datin(interp%x1, interp%y0)
   top_left = datin(interp%x0, interp%y1)
   top_rght = datin(interp%x1, interp%y1)

   include "interp_field.inc"

end subroutine interp_field_0d

subroutine interp_field_1d(datin, datout, interp)

   implicit none

   real(sreal), target, intent(in)       :: datin(:,:,:)
   real(sreal),         intent(inout)    :: datout(:)
   type(interpol_s),    intent(in)       :: interp

   logical                            :: miss(4)
   real(sreal)                        :: coef(3)
   real(sreal), pointer, dimension(:) :: bot_left, bot_rght, top_left, top_rght

   ! decide interpolation to do dependent on the missing values
   miss = [datin(interp%x0, interp%y0, 1) == real_fill_value, &
           datin(interp%x1, interp%y0, 1) == real_fill_value, &
           datin(interp%x0, interp%y1, 1) == real_fill_value, &
           datin(interp%x1, interp%y1, 1) == real_fill_value]
   
   bot_left => datin(interp%x0, interp%y0, :)
   bot_rght => datin(interp%x1, interp%y0, :)
   top_left => datin(interp%x0, interp%y1, :)
   top_rght => datin(interp%x1, interp%y1, :)

   include "interp_field.inc"

end subroutine interp_field_1d

subroutine interp_field_2d(datin, datout, interp)

   implicit none

   real(8),     target, intent(in)         :: datin(:,:,:,:)
   real(8),             intent(inout)      :: datout(:,:)
   type(interpol_s),    intent(in)         :: interp

   logical                              :: miss(4)
   real(8)                              :: coef(3)
   real(8),     pointer, dimension(:,:) :: bot_left, bot_rght, top_left, top_rght

   ! decide interpolation to do dependent on the missing values
   miss = [datin(interp%x0, interp%y0, 1, 1) == real_fill_value, &
           datin(interp%x1, interp%y0, 1, 1) == real_fill_value, &
           datin(interp%x0, interp%y1, 1, 1) == real_fill_value, &
           datin(interp%x1, interp%y1, 1, 1) == real_fill_value]
   
   bot_left => datin(interp%x0, interp%y0, :, :)
   bot_rght => datin(interp%x1, interp%y0, :, :)
   top_left => datin(interp%x0, interp%y1, :, :)
   top_rght => datin(interp%x1, interp%y1, :, :)

   include "interp_field.inc"

end subroutine interp_field_2d

end module interpol
