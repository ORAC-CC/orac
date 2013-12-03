! Name:
!    GZero
!
! Purpose:
!    Module containing "zero'th point" structure used in ECP LUT interpolation.
! 
! Description:
!    Defines a structure used to hold information about the nearest neighbour 
!    grid point array indices when interpolating Look-Up Tables in the ECP. 
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Name:
!    GZero
!
! Purpose:
!    Module containing "zero'th point" structure used in ECP LUT interpolation.
! 
! Description:
!    Defines a structure used to hold information about the nearest neighbour 
!    grid point array indices when interpolating Look-Up Tables in the ECP. 
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
!    To be completed once questions are answered
!
! History:
!    4th Oct 2000, Andy Smith : original version
!   16th Nov 2000, Andy Smith :
!      Added dX and X1 values as these are used by several rouitnes 
!      for each value of Tau, Re etc and it's easier to calculate them
!      once only.
!    1st Dec 2000, Andy Smith :
!      Renamed Sun Zen values to Sol Zen (iSuZ to iSoZ etc)
!    4th May 2011, Andy Smith:
!      Extension to multiple instrument views. Values depending on 
!      viewing geometry are now arrays (no of views). 
!    5th Sep 2011, Chris Arnold:
!      Included next-nearest neighbours in Tau/Re for new
!      interpolation routines
!20131203 MJ makes LUTs more flexible wrt channel and properties
!
! Bugs:
!    None known.
!
!---------------------------------------------------------------------

module GZero_def

   type GZero_t
      integer,pointer          :: iT0(:,:), iT1(:,:)		! Nearest neighbour indices for Tau value    
      integer,pointer	       :: iTm1(:,:),iTp1(:,:)		! Next-nearest neighbour indices for Tau value
      integer,pointer          :: iR0(:,:), iR1(:,:)		! Nearest neighbour indices for Re value    
      integer,pointer          :: iRm1(:,:), iRp1(:,:)		! Next-nearest neighbour indices for Re value
      integer, pointer :: iSaZ0(:,:), iSaZ1(:,:)	! Nearest neighbour indices for Sat zen value
      integer, pointer :: iSoZ0(:,:), iSoZ1(:,:)	! Nearest neighbour indices for Sun zen value
      integer, pointer :: iRA0(:,:), iRA1(:,:)	! Nearest neighbour indices for Rel Azi value
      real,pointer             :: dT(:,:)			! Fraction of grid step in Tau from zero'th
						! point to current Tau value
      real,pointer             :: dR(:,:)			! Fraction of grid step to current Re 
      real, pointer    :: dSaZ(:,:)		! Fraction of grid step to current Sat zen
      real, pointer    :: dSoZ(:,:)       	! Fraction of grid step to current Sun zen 
      real, pointer    :: dRA(:,:)        	! Fraction of grid step to current Rel Azi
      real,pointer             :: T1(:,:)            	! 1.0 - dT (stored for frequent use)
      real,pointer             :: R1(:,:)            	! 1.0 - dR (stored for frequent use)
      real, pointer    :: Sa1(:,:)        	! 1.0 - dSaZ (stored for frequent use)
      real, pointer    :: So1(:,:)        	! 1.0 - dSuZ (stored for frequent use)
      real, pointer    :: Ra1(:,:)        	! 1.0 - dRA (stored for frequent use)
   end type GZero_t

end module GZero_def
