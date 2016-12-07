!-------------------------------------------------------------------------------
! Name: interpol.F90
!
! Purpose:
! Container for interpolation routines. To interpolate a field, call
! bilinear_coefs and then interp_field. The structure interpol passes
! information between them.
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
! 2014/05/23, AP: First version.
! 2014/08/05, AP: New bilinear interpolation routines.
! 2014/09/28, GM: Added interp_field2.
!
! $Id$
!
! Bugs:
! With arrays, interp_field only checks if the first value is missing.
!-------------------------------------------------------------------------------

module interpol_m

   use common_constants_m, only: sreal, sreal_fill_value

   implicit none

   type interpol_t
      integer     :: x0, x1, y0, y1
      real        :: t, u
   end type interpol_t

   interface bilinear_coef
      module procedure bilinear_coef_reg_reg,  bilinear_coef_reg_irr, &
                       bilinear_coef_irr_reg,  bilinear_coef_irr_irr, &
                       bilinear_coef_reg_reg2, bilinear_coef_reg_irr2, &
                       bilinear_coef_irr_reg2, bilinear_coef_irr_irr2
   end interface bilinear_coef

   interface interp_field
      module procedure interp_field_0d, interp_field_1d, interp_field_2d
   end interface interp_field

   interface interp_field2
      module procedure interp_field2_1d, interp_field2_2d
   end interface interp_field2

contains

#include "bound_grid.F90"

subroutine bilinear_coef_reg_reg(xstart, x_invdel, nx, ystart, y_invdel, ny, &
     xout, yout, interp, wrap)
   implicit none

   real(8),          intent(in)  :: xstart, x_invdel
   real(8),          intent(in)  :: ystart, y_invdel
   integer,          intent(in)  :: nx, ny
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp
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
   real,             intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp
   logical,          intent(in)  :: wrap

   call bound_regular_grid(xstart, x_invdel, nx, xout, &
        interp%x0, interp%x1, interp%t, wrap)
   call bound_irregular_grid(ygrid, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_reg_irr

subroutine bilinear_coef_irr_reg(xgrid, nx, ystart, y_invdel, ny, &
     xout, yout, interp, wrap)
   implicit none

   real,             intent(in)  :: xgrid(:)
   real(8),          intent(in)  :: ystart, y_invdel
   integer,          intent(in)  :: nx, ny
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp
   logical,          intent(in)  :: wrap

   call bound_irregular_grid(xgrid, nx, xout, &
        interp%x0, interp%x1, interp%t, wrap)
   call bound_regular_grid(ystart, y_invdel, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_irr_reg

subroutine bilinear_coef_irr_irr(xgrid, nx, ygrid, ny, &
     xout, yout, interp, wrap)
   implicit none

   real,             intent(in)  :: xgrid(:)
   real,             intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp
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
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp

   call bound_regular_grid(xstart, x_invdel, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_regular_grid(ystart, y_invdel, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_reg_reg2

subroutine bilinear_coef_reg_irr2(xstart, x_invdel, nx, ygrid, ny, &
     xout, yout, interp)
   implicit none

   real(8),          intent(in)  :: xstart, x_invdel
   real,             intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp

   call bound_regular_grid(xstart, x_invdel, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_irregular_grid(ygrid, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_reg_irr2

subroutine bilinear_coef_irr_reg2(xgrid, nx, ystart, y_invdel, ny, &
     xout, yout, interp)
   implicit none

   real,             intent(in)  :: xgrid(:)
   real(8),          intent(in)  :: ystart, y_invdel
   integer,          intent(in)  :: nx, ny
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp

   call bound_irregular_grid(xgrid, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_regular_grid(ystart, y_invdel, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_irr_reg2

subroutine bilinear_coef_irr_irr2(xgrid, nx, ygrid, ny, &
     xout, yout, interp)
   implicit none

   real,             intent(in)  :: xgrid(:)
   real,             intent(in)  :: ygrid(:)
   integer,          intent(in)  :: nx, ny
   real,             intent(in)  :: xout, yout
   type(interpol_t), intent(out) :: interp

   call bound_irregular_grid(xgrid, nx, xout, &
        interp%x0, interp%x1, interp%t, .true.)
   call bound_irregular_grid(ygrid, ny, yout, &
        interp%y0, interp%y1, interp%u, .false.)

end subroutine bilinear_coef_irr_irr2

subroutine interp_field_0d(datin, datout, interp)

   implicit none

   real,        target, intent(in)    :: datin(:,:)
   real,                intent(inout) :: datout
   type(interpol_t),    intent(in)    :: interp

   logical     :: miss(4)
   real        :: coef(3)
   real        :: bot_left, bot_rght, top_left, top_rght

   ! decide that interpolation to do dependent on the missing values
   miss = [datin(interp%x0, interp%y0) == sreal_fill_value, &
           datin(interp%x1, interp%y0) == sreal_fill_value, &
           datin(interp%x0, interp%y1) == sreal_fill_value, &
           datin(interp%x1, interp%y1) == sreal_fill_value]

   bot_left = datin(interp%x0, interp%y0)
   bot_rght = datin(interp%x1, interp%y0)
   top_left = datin(interp%x0, interp%y1)
   top_rght = datin(interp%x1, interp%y1)

#include "interp_field.inc"

end subroutine interp_field_0d

subroutine interp_field_1d(datin, datout, interp)

   implicit none

   real,        target, intent(in)    :: datin(:,:,:)
   real,                intent(inout) :: datout(:)
   type(interpol_t),    intent(in)    :: interp

   logical                            :: miss(4)
   real                               :: coef(3)
   real,        pointer, dimension(:) :: bot_left, bot_rght, top_left, top_rght

   ! decide interpolation to do dependent on the missing values
   miss = [datin(interp%x0, interp%y0, 1) == sreal_fill_value, &
           datin(interp%x1, interp%y0, 1) == sreal_fill_value, &
           datin(interp%x0, interp%y1, 1) == sreal_fill_value, &
           datin(interp%x1, interp%y1, 1) == sreal_fill_value]

   bot_left => datin(interp%x0, interp%y0, :)
   bot_rght => datin(interp%x1, interp%y0, :)
   top_left => datin(interp%x0, interp%y1, :)
   top_rght => datin(interp%x1, interp%y1, :)

#include "interp_field.inc"

end subroutine interp_field_1d

subroutine interp_field_2d(datin, datout, interp)

   implicit none

   real,        target, intent(in)    :: datin(:,:,:,:)
   real,                intent(inout) :: datout(:,:)
   type(interpol_t),    intent(in)    :: interp

   logical                              :: miss(4)
   real                                 :: coef(3)
   real,        pointer, dimension(:,:) :: bot_left, bot_rght, top_left, top_rght

   ! decide interpolation to do dependent on the missing values
   miss = [datin(interp%x0, interp%y0, 1, 1) == sreal_fill_value, &
           datin(interp%x1, interp%y0, 1, 1) == sreal_fill_value, &
           datin(interp%x0, interp%y1, 1, 1) == sreal_fill_value, &
           datin(interp%x1, interp%y1, 1, 1) == sreal_fill_value]

   bot_left => datin(interp%x0, interp%y0, :, :)
   bot_rght => datin(interp%x1, interp%y0, :, :)
   top_left => datin(interp%x0, interp%y1, :, :)
   top_rght => datin(interp%x1, interp%y1, :, :)

#include "interp_field.inc"

end subroutine interp_field_2d

subroutine interp_field2_1d(datin, datout, interp)

   implicit none

   real,        target, intent(in)    :: datin(:,:,:)
   real,                intent(inout) :: datout(:)
   type(interpol_t),    intent(in)    :: interp

   logical                            :: miss(4)
   real                               :: coef(3)
   real,        pointer, dimension(:) :: bot_left, bot_rght, top_left, top_rght

   ! decide interpolation to do dependent on the missing values
   miss = [datin(1, interp%x0, interp%y0) == sreal_fill_value, &
           datin(1, interp%x1, interp%y0) == sreal_fill_value, &
           datin(1, interp%x0, interp%y1) == sreal_fill_value, &
           datin(1, interp%x1, interp%y1) == sreal_fill_value]

   bot_left => datin(:, interp%x0, interp%y0)
   bot_rght => datin(:, interp%x1, interp%y0)
   top_left => datin(:, interp%x0, interp%y1)
   top_rght => datin(:, interp%x1, interp%y1)

#include "interp_field.inc"

end subroutine interp_field2_1d

subroutine interp_field2_2d(datin, datout, interp)

   implicit none

   real,        target, intent(in)    :: datin(:,:,:,:)
   real,                intent(inout) :: datout(:,:)
   type(interpol_t),    intent(in)    :: interp

   logical                              :: miss(4)
   real                                 :: coef(3)
   real,        pointer, dimension(:,:) :: bot_left, bot_rght, top_left, top_rght

   ! decide interpolation to do dependent on the missing values
   miss = [datin(1, 1, interp%x0, interp%y0) == sreal_fill_value, &
           datin(1, 1, interp%x1, interp%y0) == sreal_fill_value, &
           datin(1, 1, interp%x0, interp%y1) == sreal_fill_value, &
           datin(1, 1, interp%x1, interp%y1) == sreal_fill_value]

   bot_left => datin(:, :, interp%x0, interp%y0)
   bot_rght => datin(:, :, interp%x1, interp%y0)
   top_left => datin(:, :, interp%x0, interp%y1)
   top_rght => datin(:, :, interp%x1, interp%y1)

#include "interp_field.inc"

end subroutine interp_field2_2d

end module interpol_m
