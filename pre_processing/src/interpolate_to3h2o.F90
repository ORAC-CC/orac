subroutine interpolate_to3h2o(nlay,nlev,phi_lay,phi_lev,tlay,tlev,o3lay,o3lev,h2olay,h2olev)

  use preproc_constants

  implicit none

  integer(kind=lint), intent(in) :: nlay,nlev
  real(kind=sreal), intent(out) :: phi_lay(nlay),phi_lev(nlev)
  real(kind=sreal), intent(out) :: tlay(nlay),tlev(nlev)
  real(kind=sreal), intent(out) :: o3lay(nlay),o3lev(nlev)
  real(kind=sreal), intent(out) :: h2olay(nlay),h2olev(nlev)

  integer(kind=lint) :: kdim

  real(kind=sreal) :: dx10,x0h,x1h

  !interpolate here
  do kdim=2,nlev-1
     
     !indexing is from top to bottom, while phi increases from bottom to top
     dx10=phi_lay(kdim-1)-phi_lay(kdim)
     x0h=(phi_lay(kdim-1)-phi_lev(kdim))/dx10
     x1h=(phi_lev(kdim)-phi_lay(kdim))/dx10

     tlev(kdim)=max(tlay(kdim)*x0h+tlay(kdim-1)*x1h,dither_more)
     !write(*,*) kdim,phi_lay(kdim-1),phi_lay(kdim),dx10,x0h,x1h,x0h+x1h,tlev(kdim)
     !pause
     o3lev(kdim)=max(o3lay(kdim)*x0h+o3lay(kdim-1)*x1h,dither_more)
     h2olev(kdim)=max(h2olay(kdim)*x0h+h2olay(kdim-1)*x1h,dither_more)

  enddo

  !extrpolate to first and last level
  !first (topmost)
  dx10=phi_lay(1)-phi_lay(2)
  x0h=(phi_lay(1)-phi_lev(1))/dx10
  x1h=(phi_lev(1)-phi_lay(2))/dx10
  tlev(1)=max(tlay(2)*x0h+tlay(1)*x1h,dither_more)
  o3lev(1)=max(o3lay(2)*x0h+o3lay(1)*x1h,dither_more)
  h2olev(1)=max(h2olay(2)*x0h+h2olay(1)*x1h,dither_more)
  !last (bottommost)
  dx10=phi_lay(nlay-1)-phi_lay(nlay)
  x0h=(phi_lay(nlay-1)-phi_lev(nlev))/dx10
  x1h=(phi_lev(nlev)-phi_lay(nlay))/dx10
  tlev(nlev)=max(tlay(nlay)*x0h+tlay(nlay-1)*x1h,dither_more)
  o3lev(nlev)=max(o3lay(nlay)*x0h+o3lay(nlay-1)*x1h,dither_more)
  h2olev(nlev)=max(h2olay(nlay)*x0h+h2olay(nlay-1)*x1h,dither_more)

end subroutine interpolate_to3h2o
