! Name:
!    CPL_def
!
! Purpose:
!    Module defining array of Cloud Pressure Level structures.
!
! Description:
!    Contains a type definition for the Cloud Pressure Level structure.
!    The CPL structure is used in array form, 1 set of values per channel.
!    Hence the array is declared allocatable so that it can be matched to
!    the instrument on each execution.
!
!    CPL_t is the defined type.
!    CPL is the declared array of type CPL_t.
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
!    1st Aug 2000, Andy Smith : Original version.
!
! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------

module CPL_def

   implicit none

   type CPL_t
      real    :: BC     ! Planck radiance at cloud level
      real    :: dBC    ! Gradient of BC wrt pressure
      real    :: RAC    ! Upward atmospheric radiance above cloud
      real    :: dRAC   ! Gradient of RAC wrt pressure
      real    :: RBC    ! Upward radiance at cloud base
      real    :: dRBC   ! Gradient of RBC wrt pressure
      real    :: RDown  ! Downward radiance at cloud top
      real    :: dRDown ! Gradient wrt pressure of RDown
      real    :: TranC  ! Transmission from cloud to space
      real    :: dTranC ! Gradient wrt pressure of TranC
   end type CPL_t

   type(CPL_t), dimension(:), allocatable :: CPL

end module CPL_def
