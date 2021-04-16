!-------------------------------------------------------------------------------
! Name: gzero.F90
!
! Purpose:
! Defines GZero_m module used to hold information about the nearest neighbour
! grid point array indices when interpolating Look-Up Tables in the ORAC.
!
! History:
! 2000/10/04, AS: original version
! 2000/11/16, AS: Added dX and X1 values as these are used by several routines
!    for each value of Tau, Re etc and it's easier to calculate them once only.
! 2000/12/01, AS: Renamed Sun Zen values to Sol Zen (iSuZ to iSoZ etc)
! 2011/05/04, AS: Extension to multiple instrument views. Values depending on
!    viewing geometry are now arrays (no of views).
! 2011/09/05, CA: Included next-nearest neighbours in Tau/Re for new
!    interpolation routines
! 2013/12/03, MJ: Makes LUTs more flexible wrt channel and properties.
! 2014/12/20, GM: Cleaned up code.
! 2014/05/22, GM: Add allocate and deallocate subroutines.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2015/01/19, GM: Put SetGZero.F90 into this module.
! 2017/01/17, GM: Eliminate the unnecessary indexing of the GZero parameters wrt
!    LUT type and changes related to similar indexing simplifications of the LUT
!    grid.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module GZero_m

   type GZero_t
      integer, pointer :: iT0(:), iT1(:)     ! Nearest neighbour indices for Tau value
      integer, pointer :: iTm1(:),iTp1(:)    ! Next-nearest neighbour indices for Tau value
      integer, pointer :: iR0(:), iR1(:)     ! Nearest neighbour indices for Re value
      integer, pointer :: iRm1(:), iRp1(:)   ! Next-nearest neighbour indices for Re value
      integer, pointer :: iSaZ0(:), iSaZ1(:) ! Nearest neighbour indices for Sat zen value
      integer, pointer :: iSoZ0(:), iSoZ1(:) ! Nearest neighbour indices for Sun zen value
      integer, pointer :: iRA0(:), iRA1(:)   ! Nearest neighbour indices for Rel Azi value
      integer, pointer :: iSaZSoZ0(:), &
                          iSaZSoZ1(:)        ! Nearest neighbour indices for Sat zen value

      real,    pointer :: dT(:)              ! Fraction of grid step in Tau from zero'th
                                             ! point to current Tau value
      real,    pointer :: dR(:)              ! Fraction of grid step to current Re
      real,    pointer :: dSaZ(:)            ! Fraction of grid step to current Sat zen
      real,    pointer :: dSoZ(:)            ! Fraction of grid step to current Sun zen
      real,    pointer :: dRA(:)             ! Fraction of grid step to current Rel Azi
      real,    pointer :: dSaZSoZ(:)         ! Fraction of grid step to current Sat zen

      real,    pointer :: T1(:)              ! 1.0 - dT (stored for frequent use)
      real,    pointer :: R1(:)              ! 1.0 - dR (stored for frequent use)
      real,    pointer :: Sa1(:)             ! 1.0 - dSaZ (stored for frequent use)
      real,    pointer :: So1(:)             ! 1.0 - dSuZ (stored for frequent use)
      real,    pointer :: Ra1(:)             ! 1.0 - dRA (stored for frequent use)
      real,    pointer :: SaSo1(:)           ! 1.0 - dSaSoZ (stored for frequent use)
   end type GZero_t

contains

subroutine Allocate_GZero(GZero, Ny)

   use ORAC_Constants_m

   implicit none

   type(GZero_t), intent(out) :: GZero
   integer,       intent(in)  :: Ny

   allocate(GZero%iT0(Ny))
   GZero%iT0=0
   allocate(GZero%iT1(Ny))
   GZero%iT1=0
   allocate(GZero%iTm1(Ny))
   GZero%iTm1=0
   allocate(GZero%iTp1(Ny))
   GZero%iTp1=0
   allocate(GZero%iR0(Ny))
   GZero%iR0=0
   allocate(GZero%iR1(Ny))
   GZero%iR1=0
   allocate(GZero%iRm1(Ny))
   GZero%iRm1=0
   allocate(GZero%iRp1(Ny))
   GZero%iRp1=0
   allocate(GZero%iSaZ0(Ny))
   GZero%iSaZ0=0
   allocate(GZero%iSaZ1(Ny))
   GZero%iSaZ1=0
   allocate(GZero%iSoZ0(Ny))
   GZero%iSoZ0=0
   allocate(GZero%iSoZ1(Ny))
   GZero%iSoZ1=0
   allocate(GZero%iRA0(Ny))
   GZero%iRA0=0
   allocate(GZero%iRA1(Ny))
   GZero%iRA1=0
   allocate(GZero%iSaZSoZ0(Ny))
   GZero%iSaZSoZ0=0
   allocate(GZero%iSaZSoZ1(Ny))
   GZero%iSaZSoZ1=0

   allocate(GZero%dT(Ny))
   GZero%dT=0.0
   allocate(GZero%dR(Ny))
   GZero%dR=0.0
   allocate(GZero%dSaZ(Ny))
   GZero%dSaZ=0.
   allocate(GZero%dSoZ(Ny))
   GZero%dSoZ=0.
   allocate(GZero%dRA(Ny))
   GZero%dRA=0.
   allocate(GZero%dSaZSoZ(Ny))
   GZero%dSaZSoZ=0.

   allocate(GZero%T1(Ny))
   GZero%T1=0.0
   allocate(GZero%R1(Ny))
   GZero%R1=0.0
   allocate(GZero%Sa1(Ny))
   GZero%Sa1=0.
   allocate(GZero%So1(Ny))
   GZero%So1=0.
   allocate(GZero%Ra1(Ny))
   GZero%Ra1=0.
   allocate(GZero%SaSo1(Ny))
   GZero%SaSo1=0.

end subroutine Allocate_GZero



subroutine Deallocate_GZero(GZero)

   implicit none

   type(GZero_t), intent(inout) :: GZero

   deallocate(GZero%iT0)
   deallocate(GZero%iT1)
   deallocate(GZero%iTm1)
   deallocate(GZero%iTp1)
   deallocate(GZero%iR0)
   deallocate(GZero%iR1)
   deallocate(GZero%iRm1)
   deallocate(GZero%iRp1)
   deallocate(GZero%iSaZ0)
   deallocate(GZero%iSaZ1)
   deallocate(GZero%iSoZ0)
   deallocate(GZero%iSoZ1)
   deallocate(GZero%iRA0)
   deallocate(GZero%iRA1)
   deallocate(GZero%iSaZSoZ0)
   deallocate(GZero%iSaZSoZ1)

   deallocate(GZero%dT)
   deallocate(GZero%dR)
   deallocate(GZero%dSaZ)
   deallocate(GZero%dSoZ)
   deallocate(GZero%dRA)
   deallocate(GZero%dSaZSoZ)

   deallocate(GZero%T1)
   deallocate(GZero%R1)
   deallocate(GZero%Sa1)
   deallocate(GZero%So1)
   deallocate(GZero%Ra1)
   deallocate(GZero%SaSo1)

end subroutine Deallocate_GZero


#include "set_gzero.F90"

end module GZero_m
