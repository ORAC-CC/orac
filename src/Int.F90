! Name:
!    test_int_def
!
! Purpose:
!    Module containing interface definitions for ECP test programs.
!
! Description:
!    Module test_int_def contains a set of interface defintions for ECP
!    subroutines. Not all subroutines are included. These interface
!    definitions are required in order that passed-length arrays can
!    be used as subroutine arguments.
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
!    N/A
!
! History:
!    25th Aug 2000, Andy Smith : original version
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

module test_int_def
   ! This horrible piece of Fortran 90 seems to be necessary to allow 
   ! use of a passed-length array.
   ! The alternative is to put the data to be passed inside a module
   ! rather than making it an argument.
   
   interface
      Subroutine Interp3dLUT(chan, F, xGrid, yGrid, delx, dely, CurX, CurY, &
         FInt, dFdx, dFdy)

	 implicit none
         integer, intent(in)  :: chan       
	 real, dimension(:,:,:), intent(inout)  :: F      ! The 2-d  (ignoring chan) array 
                                        	     ! to be interpolated.
	 real, dimension(:), intent(inout)    :: xGrid  ! x-values for F
	 real, dimension(:), intent(inout)    :: yGrid  ! y-values for F
	 real, intent(in)   :: delx, dely   ! Grid step in x and y
	 real, intent(in)   :: CurX, CurY   ! The "current" x and y values, i.e. the 
                                	    ! point where we want to find the 
					    ! interpolated value.  
	 real, intent(out)  :: FInt	      ! Interpolated value of F at CurX, CurY
	 real, intent(out)  :: dFdx, dFdy   ! Gradients of F wrt x and y at CurX, CurY
      End Subroutine Interp3dLUT
   end interface
   
end module test_int_def
