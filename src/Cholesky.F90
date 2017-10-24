!-------------------------------------------------------------------------------
! Name: Cholesky.F90
!
! Purpose:
! Module encapsulating the linear algebra for a Cholesky decomposition.
!
! History:
! 2015/01/18, GM: Original version.
!
! $Id$
!
! Bugs:
! None known.
!---------------------------------------------------------------------

module Cholesky_m

   implicit none

contains

#include "InvertCholesky.F90"
#include "SolveCholesky.F90"

end module Cholesky_m
