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
!    Name       Type    In/Out/Both    Description
!    Ctrl       struct  In             ECP control structure. Passed to all 
!                                      subordinate routines.
!    SAD_CloudClass     Out            Cloud class information populated by
!               array of structs       Read_Cloud_Class_Config
!    SAD_Chan   array of structs       Instrument channel info. Populated by
!                       Out            Read_Chan
!    SAD_LUT    array of structs       Cloud Radiative Property Look Up Tables
!                       Out            Populated by Read_LUT.
!    status     int     Out            Status/error code. Set non-zero if an
!                                      error occurs in any of the subordinate
!                                      routines.
!
! Algorithm:
!    if status ok, call function to read cloud class config file
!    if status ok, call function to read channel characterisation files
!    if status ok, call function to read Look-Up Tables
!
! Local variables:
!    Name       Type    Description
!    N/A
!
! History:
!    6th Sep 2000, Andy Smith : original version.
!   23rd Nov 2000, Andy Smith :
!      Fixed debuging output statements: only executed if status is 0
!   25th Jun 2001, Andy Smith :
!      Tidied up. Header comments completed. Removed debug code.
!    ***************** ECV work starts here *****************
!   22nd Mar 2011, Andy Smith:
!      Removal of phase change. Only 1 cloud class required for each retrieval 
!      run. SADCloudClass and SAD_LUT array dimensions changed. 
!   5th Aug 2011, Caroline Poulsen removed routine that read the cloudclass
!             config file removed from functionname
!   5th Dec 2011, removed instrument config file
! Bugs:
!    None known.
!
! $Id: ReadSAD.f90 74 2011-08-16 16:11:53Z capoulse $
!
!---------------------------------------------------------------------

subroutine Read_SAD(Ctrl, SAD_Chan, SAD_LUT, status)
   
   use ECP_Constants
   use CTRL_def
   use SAD_Chan_def
   use SAD_LUT_def
   use SAD_Routines_def

   implicit none
   
!  argument declarations 
   type(CTRL_t), intent(inout) :: Ctrl
   type(SAD_Chan_t), dimension(:), intent(inout) :: SAD_Chan
   type(SAD_LUT_t), intent(inout)  :: SAD_LUT
   integer, intent(inout)        :: status
   

   if (status == 0) call Read_Chan (Ctrl, SAD_Chan, status)      
   write(*,*)'read chan',status
!  Read Look up tables

   write(*,*)'read lut before',status
   if (status == 0) call Read_LUT (Ctrl, SAD_Chan, SAD_LUT,status)
   write(*,*)'read lut',status

end subroutine Read_SAD
