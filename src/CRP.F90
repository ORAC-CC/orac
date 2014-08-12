! Name:
!    CRP_def
!
! Purpose:
!    Module defining array of Cloud Radiative Properties structures.
!
! Description:
!    Contains a type definition for the Cloud Radiative Properties structure.
!    The CRP structure is used in array form, 1 set of values per channel.
!    Hence the array is declared allocatable so that it can be matched to
!    the instrument on each execution.
!
!    CRP_t is the defined type.
!    CRP is the declared array of type CRP_t.
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name       Type    Description
!
! History:
!    2nd Aug 2000, Andy Smith : Original version.

!
! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------

module CRP_def

   implicit none

   ! Note Tau is optical depth,
   ! Reff is particle size (effective radius?)

   type CRP_t
      real     :: Em      ! Cloud emissivity
      real     :: ddEm(2) ! Gradients of Em wrt Tau and REff
      real     :: RD      ! Cloud reflectivity
      real     :: ddRD(2) ! Gradients of RD wrt Tau and REff
      real     :: TD      ! Cloud transmissivity
      real     :: ddTD(2) ! Gradients of TD wrt Tau and REff
   end type CRP_t

   ! Should the array declaration be left up to the code using the module?

   !type(CRP_t), dimension(:), allocatable :: CRP

end module CRP_def
