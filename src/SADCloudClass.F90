! Name:
!    SAD_Cloud_Class
!
! Purpose:
!    Module containing Static Application Data Cloud Class structure
! 
! Description:
!    Defines a set of structures used to hold Static Application Data 
!    for Cloud Classes in the ECP. 
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
!    5th Sep 2000, Andy Smith : original version
!    1st Dec 2000, Andy Smith :
!       nSun renamed nSol for consistency with other code.
!    4th Jun 2001, Andy Smith:
!       Replaced separate variables for Tau, SxTau etc with arrays X and Sx.
!       The arrays are big enough to hold all 5 state variables even though
!       only 3 can be set using cloud class data. This is done for consistency 
!       and ease of use elsewhere.
!   13th Jun 2001, Andy Smith:
!       Water/ice flag values 0/1 replaced by values 1/2 (named constants 
!       IPhaseWat/Ice). Comment updated. 
!
!
! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------

module SAD_CloudClass_def

   use ECP_Constants
   
   type SAD_CloudClass_t
      integer        :: ID            ! Numeric identifier
      character(4)   :: Name          ! Name identifier
      real           :: X(MaxStateVar)          
                                      ! Mean state vector for this class, to be
                                      ! used as first-guess when
				      ! Ctrl%FG value = SelmSAD

      real           :: Sx(MaxStateVar) 
                                      ! Errors in state variables,
                                      ! used when Ctrl%AP value for the
				      ! appropriate variable = SelmSAD

      integer        :: WIFlg         ! Water(IPhaseWat)/Ice(IPhaseIce) Flag 
                                      ! for class
      integer        :: nTau          ! Number of LUT entries in Tau dimension
      integer        :: nRe           ! Number of LUT entries in Re dimension
      integer        :: nSat          ! No of LUT entries in sat zenith angle 
                                      ! dimension
      integer        :: nSol          ! Number of LUT entries in solar zenith
                                      ! angle dimension
      integer        :: nAzi          ! Number of LUT entries in relative
                                      ! azimuth angle dimension
      real           :: ULim(MaxStateVar)       
                                      ! Upper limits on state parameters for
                                      ! this class
      real           :: LLim(MaxStateVar)       
                                      ! Lower limits on state parameters for
                                      ! this class
   end type SAD_CloudClass_t

end module SAD_CloudClass_def
