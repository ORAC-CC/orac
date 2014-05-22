!-------------------------------------------------------------------------------
! Name:
!    GZero
!
! Description:
!    Defines a structure used to hold information about the nearest neighbour
!    grid point array indices when interpolating Look-Up Tables in the ECP.
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!
! History:
!     4th Oct 2000, Andy Smith : original version
!    16th Nov 2000, Andy Smith :
!       Added dX and X1 values as these are used by several routines for each
!       value of Tau, Re etc and it's easier to calculate them once only.
!     1st Dec 2000, Andy Smith :
!       Renamed Sun Zen values to Sol Zen (iSuZ to iSoZ etc)
!     4th May 2011, Andy Smith:
!       Extension to multiple instrument views. Values depending on viewing
!       geometry are now arrays (no of views).
!     5th Sep 2011, Chris Arnold:
!       Included next-nearest neighbours in Tau/Re for new interpolation
!       routines
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    20th Dec 2014, Greg McGarragh:
!       Cleaned up code.
!    22nd May 2014, Greg McGarragh:
!       Add allocate and deallocate subroutines.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module GZero_def

   type GZero_t
      integer, pointer :: iT0(:,:), iT1(:,:)     ! Nearest neighbour indices for Tau value
      integer, pointer :: iTm1(:,:),iTp1(:,:)    ! Next-nearest neighbour indices for Tau value
      integer, pointer :: iR0(:,:), iR1(:,:)     ! Nearest neighbour indices for Re value
      integer, pointer :: iRm1(:,:), iRp1(:,:)   ! Next-nearest neighbour indices for Re value
      integer, pointer :: iSaZ0(:,:), iSaZ1(:,:) ! Nearest neighbour indices for Sat zen value
      integer, pointer :: iSoZ0(:,:), iSoZ1(:,:) ! Nearest neighbour indices for Sun zen value
      integer, pointer :: iRA0(:,:), iRA1(:,:)	 ! Nearest neighbour indices for Rel Azi value

      real,    pointer :: dT(:,:)                ! Fraction of grid step in Tau from zero'th
						 ! point to current Tau value
      real,    pointer :: dR(:,:)                ! Fraction of grid step to current Re
      real,    pointer :: dSaZ(:,:)              ! Fraction of grid step to current Sat zen
      real,    pointer :: dSoZ(:,:)              ! Fraction of grid step to current Sun zen
      real,    pointer :: dRA(:,:)               ! Fraction of grid step to current Rel Azi

      real,    pointer :: T1(:,:)                ! 1.0 - dT (stored for frequent use)
      real,    pointer :: R1(:,:)                ! 1.0 - dR (stored for frequent use)
      real,    pointer :: Sa1(:,:)               ! 1.0 - dSaZ (stored for frequent use)
      real,    pointer :: So1(:,:)               ! 1.0 - dSuZ (stored for frequent use)
      real,    pointer :: Ra1(:,:)               ! 1.0 - dRA (stored for frequent use)
   end type GZero_t

contains

subroutine Allocate_GZero(GZero, SPixel)

   use ECP_Constants
   use SPixel_def

   implicit none

   type(GZero_t) :: GZero
   type(SPixel_t) :: SPixel

   allocate(GZero%iT0(SPixel%Ind%Ny,MaxCRProps))
   GZero%iT0=0
   allocate(GZero%iT1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iT1=0
   allocate(GZero%iTm1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iTm1=0
   allocate(GZero%iTp1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iTp1=0
   allocate(GZero%iR0(SPixel%Ind%Ny,MaxCRProps))
   GZero%iR0=0
   allocate(GZero%iR1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iR1=0
   allocate(GZero%iRm1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iRm1=0
   allocate(GZero%iRp1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iRp1=0
   allocate(GZero%iSaZ0(SPixel%Ind%Ny,MaxCRProps))
   GZero%iSaZ0=0
   allocate(GZero%iSaZ1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iSaZ1=0
   allocate(GZero%iSoZ0(SPixel%Ind%Ny,MaxCRProps))
   GZero%iSoZ0=0
   allocate(GZero%iSoZ1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iSoZ1=0
   allocate(GZero%iRA0(SPixel%Ind%Ny,MaxCRProps))
   GZero%iRA0=0
   allocate(GZero%iRA1(SPixel%Ind%Ny,MaxCRProps))
   GZero%iRA1=0

   allocate(GZero%dT(SPixel%Ind%Ny,MaxCRProps))
   GZero%dT=0.0
   allocate(GZero%dR(SPixel%Ind%Ny,MaxCRProps))
   GZero%dR=0.0
   allocate(GZero%dSaZ(SPixel%Ind%Ny,MaxCRProps))
   GZero%dSaZ=0.
   allocate(GZero%dSoZ(SPixel%Ind%Ny,MaxCRProps))
   GZero%dSoZ=0.
   allocate(GZero%dRA(SPixel%Ind%Ny,MaxCRProps))
   GZero%dRA=0.

   allocate(GZero%T1(SPixel%Ind%Ny,MaxCRProps))
   GZero%T1=0.0
   allocate(GZero%R1(SPixel%Ind%Ny,MaxCRProps))
   GZero%R1=0.0
   allocate(GZero%Sa1(SPixel%Ind%Ny,MaxCRProps))
   GZero%Sa1=0.
   allocate(GZero%So1(SPixel%Ind%Ny,MaxCRProps))
   GZero%So1=0.
   allocate(GZero%Ra1(SPixel%Ind%Ny,MaxCRProps))
   GZero%Ra1=0.

end subroutine Allocate_GZero



subroutine Deallocate_GZero(GZero)

   implicit none

   type(GZero_t) :: GZero

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

   deallocate(GZero%dT)
   deallocate(GZero%dR)
   deallocate(GZero%dSaZ)
   deallocate(GZero%dSoZ)
   deallocate(GZero%dRA)

   deallocate(GZero%T1)
   deallocate(GZero%R1)
   deallocate(GZero%Sa1)
   deallocate(GZero%So1)
   deallocate(GZero%Ra1)

end subroutine Deallocate_GZero

end module GZero_def
