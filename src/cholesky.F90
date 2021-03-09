!-------------------------------------------------------------------------------
! Name: cholesky.F90
!
! Purpose:
! Module encapsulating the linear algebra for a Cholesky decomposition.
!
! History:
! 2015/01/18, GM: Original version.
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Cholesky_m

   implicit none

contains

#include "invert_cholesky.F90"
#include "solve_cholesky.F90"

end module Cholesky_m
