! Name:
!    X_AUX
!
! Purpose:
!    Sets up parts of the state vector, X, depending on auxiliary data.
!
! Description:
!
! Arguments:
!    Name       Type    In/Out    Description
!    status     int     out       Indicates success/failure of subroutine.
!
! Algorithm:
!
! Local variables:
!    Name       Type    Description
!
! History:
!    9 Feb 2000, Kevin Smith : Original version
!
! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------

subroutine X_AUX(SPixel, index, X, err, status)

   use ECP_Constants

   implicit none

!  Declare arguments

   integer        :: index
   real           :: X
   real           :: err
   integer        :: status

!  Declare local variables

   character(180) :: message

!  Parameters supported are Tau, Pc and f.

   select case (index)

!     Cloud optical depth, Tau.

      case (iTau)

!     Effective radius, Re.

      case (iRe)

!     Cloud pressure, Pc.

      case (iPc)

!     Cloud fraction, f.

      case (iFr)

!     Surface temperature, Ts.

      case (iTs)

!     Write message to log if AUX method not supported for state variable

      case default

         status = XAUXMeth
	 write(unit=message, fmt=*) 'X_AUX: Method not supported for state variable index: ', index
         call Write_log(Ctrl, trim(message), status)

   end select

end subroutine X_AUX
