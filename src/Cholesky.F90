!-------------------------------------------------------------------------------
! Name: Cholesky.F90
!
! Purpose:
! Module encapsulating the linear algebra for a Cholesky decomposition.
!
! History:
! 2015/01/18, GM: Original version.
!
! $Id: Cholesky.F90 2856 2015-01-12 18:50:33Z acpovey $
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Cholesky_m

   implicit none

contains

#include "DecomposeCholesky.F90"
#include "InvertCholesky.F90"
#include "InvertCholeskyDC.F90"
#include "SolveCholesky.F90"
#include "SolveCholeskyDC.F90"
#include "SquareCholeskyDC.F90"

end module Cholesky_m
