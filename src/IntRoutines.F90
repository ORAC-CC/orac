! Name:
!    Int_Routines_def
!
! Purpose:
!    Interface definition for ECP LUT Interpolation routines
!
! Description:
!    This module contains a set of interface defintions for ECP
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
!    10th Nov 2000, Andy Smith : original version
!    16th Nov 2000, Andy Smith : 
!       Adding more Int routines: Int_LUT_TauReSun.
!     1st Dec 2000, Andy Smith : 
!       Replaced "sun" in routine names with Sol
!    11th Jan 2001, Andy Smith : 
!       Chans argument removed from all routines. Redundant since 
!       interpolation is done over the entire passed array.
!    5th Sep 2011, Chris Arnold:
!       Added interfaces for spline/locate routines and updated
!	interfaces for Int_LUT routines
!    7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to interpolation routines IntLUT*.f90
!20131203 MJ makes LUTs more flexible wrt channel and properties
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

module Int_Routines_def

   interface
      Subroutine Int_LUT_TauRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr,status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def
	 use bcubic_def

	 implicit none

         type(CTRL_t), intent(in)    :: Ctrl
!	 integer, dimension(:), intent(in)      :: Chans
	 real, dimension(:,:,:), intent(in)     :: F 
	 type(GZero_t), intent(in)      :: GZero
	 type(LUT_Grid_t), intent(in)   :: Grid 
	 real, dimension(:), intent(inout)      :: FInt	      
	 real, dimension(:,:), intent(inout)    :: FGrads 
         integer          :: status,icrpr   
      End Subroutine Int_LUT_TauRe
   End interface


   Interface
      Subroutine Int_LUT_TauSolRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr, status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def
	 use bcubic_def

	 implicit none

         type(CTRL_t), intent(in)     :: Ctrl
!	 integer, dimension(:), intent(in)      :: Chans
	 real, dimension(:,:,:,:), intent(in)   :: F 
	 type(GZero_t), intent(in)      :: GZero               
	 type(LUT_Grid_t), intent(in)   :: Grid
	 real, dimension(:), intent(inout)      :: FInt	      
	 real, dimension(:,:), intent(inout)    :: FGrads
         integer          :: status,icrpr   
      End Subroutine Int_LUT_TauSolRe
   End interface

   Interface
      Subroutine Int_LUT_TauSatRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr,status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def
	 use bcubic_def

	 implicit none
 
         type(CTRL_t), intent(in)     :: Ctrl
!	 integer, dimension(:), intent(in)      :: Chans
	 real, dimension(:,:,:,:), intent(in)   :: F 
	 type(GZero_t), intent(in)      :: GZero               
	 type(LUT_Grid_t), intent(in)   :: Grid
	 real, dimension(:), intent(inout)      :: FInt	      
	 real, dimension(:,:), intent(inout)    :: FGrads 
         integer          :: status,icrpr
      End Subroutine Int_LUT_TauSatRe
   End interface

   Interface
      Subroutine Int_LUT_TauSatSolAziRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr,status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def
	 use bcubic_def

	 implicit none

         type(CTRL_t), intent(in)     :: Ctrl
!	 integer, dimension(:), intent(in)      	:: Chans
	 real, dimension(:,:,:,:,:,:), intent(in)	:: F 
	 type(GZero_t), intent(in)     :: GZero               
	 type(LUT_Grid_t), intent(in)   :: Grid
	 real, dimension(:), intent(inout)		:: FInt	      
	 real, dimension(:,:), intent(inout)		:: FGrads 
         integer          :: status, icrpr   
      End Subroutine Int_LUT_TauSatSolAziRe
   End interface

   Interface 
      Subroutine spline(x,y,y2)
         
         implicit none

         real, dimension(:), intent(in)		:: x,y
         real, dimension(:), intent(out)	:: y2

      End Subroutine spline
   End Interface

   Interface
      Function locate(xx,x)

         real, dimension(:), intent(in) :: xx
         real, intent(in) :: x
         integer :: locate

      End Function locate
   End Interface

end module Int_Routines_def
