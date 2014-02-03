! Name:
!    Check_Limits
!
! Description:
!    Checks upper and lower limits on all active state variables and sets 
!    the phase change flag. Each active variable is checked to ensure that 
!    it's current value has not gone outside the bounds for the phase (stored
!    in the SPixel Ulim and LLim arrays). Re (if active) and possibly Tc are 
!    checked for indications of a phase change.
!      
! Arguments:
!    Name        Type       In/Out/Both   Description
!    SPixel      struct     Both          Super-pixel structure (contains the
!                                         current active state variable info, 
!                                         plus current phase) 
!    X           real array Both          Contains the currently active state 
!                                         variables
!    phase_change logical   Both          Result of the phase change test
!    status      int        Out           ECP program status value.
!
! Algorithm:
!    For each state variable, check bounds:
!       if (variable > Max) variable = Max
!       if (variable < Min) variable = Min
!
!
! Local variables:
!    Name       Type     Description
!    Re_active  logical  Indicates whether Re is an active state variable.
!
! History:
!    26th Apr 2001, Andy Smith : original version
!    29th Jun 2001, Andy Smith :
!       Added "quick exit" on detection of phase change. If phase change 
!       occurs new limits will be set by Invert_Marquardt and the state 
!       vector will be checked again. Hence there's no point checking the 
!       remaining state variables with the current phase limits.
!     ********************** ECV work starts here ***********************
!    21st Mar 2011, Andy Smith:
!      Removed phase change functionality. Each state variable will now 
!      simply be bound by the defined upper and lower limits. 
!      Phase_change argument kept for now to avoid updating function interface.
!
! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------


Subroutine Check_Limits (Ctrl, X, SPixel, RTM_Pc, phase_change, status)

   use ECP_Constants
   use Ctrl_def
   use SPixel_def
   use RTM_Pc_def

   Implicit none

!  Argument declarations

   type(Ctrl_t), intent(in)           :: Ctrl
   real, intent(inout)                :: X(MaxStateVar)
   type(SPixel_t), intent(inout)      :: SPixel
   type(RTM_Pc_t), intent(in)         :: RTM_Pc
   logical, intent(inout)             :: phase_change
   integer, intent(inout)             :: status

!  Local variable declarations 

   integer    :: i         ! Loop counter

   phase_change = .false.  
!   write(*,*) 'before loop'
   do i=1,Spixel%Nx
!      write(*,*) 'in loop',i
         if (X(Spixel%X(i)) > SPixel%XULim(Spixel%X(i))) &
	    X(Spixel%X(i)) = SPixel%XULim(Spixel%X(i))

         if (X(Spixel%X(i)) < SPixel%XLLim(Spixel%X(i))) &
	    X(Spixel%X(i)) = SPixel%XLLim(Spixel%X(i))      

   end do
!   write(*,*) 'after loop'	 

End Subroutine Check_Limits 
