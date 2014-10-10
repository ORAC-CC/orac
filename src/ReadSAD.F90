module Read_SAD_def

implicit none

contains

!-------------------------------------------------------------------------------
! Name:
!    Read_SAD
!
! Purpose:
!    Controlling subroutine for reading Static Application Data (SAD) files.
!
! Description:
!    This subroutine calls all the subroutines that read SAD files. It also
!    sets the sizes of all allocatable arrays of SAD data structures, and
!    carries out checking of the returned status from each subroutine called.
!
! Arguments:
!    Name       Type    In/Out/Both Description
!    Ctrl       struct  In          ECP control structure. Passed to all
!                                   subordinate routines.
!    SAD_CloudClass     Out         Cloud class information populated by
!               array of structs    Read_Cloud_Class_Config
!    SAD_Chan   array of structs    Instrument channel info. Populated by
!                       Out         Read_Chan
!    SAD_LUT    array of structs    Cloud Radiative Property Look Up Tables
!                       Out         Populated by Read_LUT.
!    status     int     Out         Status/error code. Set non-zero if an error
!                                   occurs in any of the subordinate routines.
!
! Algorithm:
!    if status ok, call function to read cloud class config file
!    if status ok, call function to read channel characterisation files
!    if status ok, call function to read Look-Up Tables
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!     6th Sep 2000, Andy Smith: Original version.
!    23rd Nov 2000, Andy Smith:
!       Fixed debugging output statements: only executed if status is 0.
!    25th Jun 2001, Andy Smith:
!       Tidied up. Header comments completed. Removed debug code.
!    **************** ECV work starts here *************************************
!    22nd Mar 2011, Andy Smith:
!       Removal of phase change. Only 1 cloud class required for each retrieval
!       run. SADCloudClass and SAD_LUT array dimensions changed.
!     5th Aug 2011, Caroline Poulsen:
!       Removed routine that read the cloudclass config file removed from
!       function name.
!     5th Dec 2011, Caroline Poulsen:
!        Removed instrument config file.
!    23rd May 2014, Greg McGarragh:
!       Cleaned up code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_SAD(Ctrl, SAD_Chan, SAD_LUT, status)

   use CTRL_def
   use ECP_Constants
   use SAD_Chan_def
   use SAD_LUT_def

   implicit none

   ! Argument declarations
   type(CTRL_t),                   intent(inout) :: Ctrl
   type(SAD_Chan_t), dimension(:), intent(inout) :: SAD_Chan
   type(SAD_LUT_t),                intent(inout) :: SAD_LUT
   integer,                        intent(inout) :: status

   ! Read channel sad files
   if (status == 0) call Read_SAD_Chan(Ctrl, SAD_Chan, status)
   write(*,*)'Read SAD channel information, status: ', status

   ! Read Look up tables
   if (status == 0) call Read_SAD_LUT (Ctrl, SAD_Chan, SAD_LUT)
   write(*,*)'Read SAD LUTs, status: ', 0

end subroutine Read_SAD

end module Read_SAD_def
