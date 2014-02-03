!-------------------------------------------------------------------------------
! Name:
!    Int_Routines_def
!
! Purpose:
!    interface definition for ECP LUT Interpolation routines
!
! Description:
!    This module contains a set of interface definitions for ECP subroutines.
!    Not all subroutines are included. These interface definitions are required
!    in order that passed-length arrays can be used as subroutine arguments.
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
!    N/A
!
! History:
!    10th Nov 2000, Andy Smith : original version
!    16th Nov 2000, Andy Smith :
!       Adding more Int routines: Int_LUT_TauReSun.
!     1st Dec 2000, Andy Smith :
!       Replaced "sun" in routine names with Sol
!    11th Jan 2001, Andy Smith :
!       Chans argument removed from all routines. Redundant since interpolation
!       is done over the entire passed array.
!     5th Sep 2011, Chris Arnold:
!       Added interfaces for spline/locate routines and updated interfaces for
!       Int_LUT routines
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to interpolation routines IntLUT*.f90
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    16th Jan 2014, Greg McGarragh:
!       Added i_chan_to_ctrl_offset and i_chan_to_spixel_offset to subroutine
!       Int_LUT_TauSatRe.
!    20th Dec 2014, Greg McGarragh:
!       Cleaned up code.
!    24th Dec 2014, Greg McGarragh:
!       Some intent changes.
!
! Bugs:
!    None known
!
! $Id$
!
!---------------------------------------------------------------------

module Int_Routines_def

   interface
      subroutine Int_LUT_TauRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr, status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def

	 implicit none

	 real, dimension(:,:,:), intent(in)  :: F
	 type(LUT_Grid_t),       intent(in)  :: Grid
	 type(GZero_t),          intent(in)  :: GZero
         type(CTRL_t),           intent(in)  :: Ctrl
	 real, dimension(:),     intent(out) :: FInt
	 real, dimension(:,:),   intent(out) :: FGrads
         integer,                intent(in)  :: icrpr
         integer,                intent(out) :: status
      end subroutine Int_LUT_TauRe
   end interface


   interface
      subroutine Int_LUT_TauSolRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr, status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def

	 implicit none

	 real, dimension(:,:,:,:), intent(in)  :: F
	 type(LUT_Grid_t),         intent(in)  :: Grid
	 type(GZero_t),            intent(in)  :: GZero
         type(CTRL_t),             intent(in)  :: Ctrl
	 real, dimension(:),       intent(out) :: FInt
	 real, dimension(:,:),     intent(out) :: FGrads
         integer,                  intent(in)  :: icrpr
         integer,                  intent(out) :: status

      end subroutine Int_LUT_TauSolRe
   end interface

   interface
      subroutine Int_LUT_TauSatRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr, &
         i_chan_to_ctrl_offset, i_chan_to_spixel_offset, status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def

	 implicit none

	 real, dimension(:,:,:,:), intent(in)  :: F
	 type(LUT_Grid_t),         intent(in)  :: Grid
	 type(GZero_t),            intent(in)  :: GZero
         type(CTRL_t),             intent(in)  :: Ctrl
	 real, dimension(:),       intent(out) :: FInt
	 real, dimension(:,:),     intent(out) :: FGrads
         integer,                  intent(in)  :: icrpr
         integer,                  intent(in)  :: i_chan_to_ctrl_offset
         integer,                  intent(in)  :: i_chan_to_spixel_offset
         integer,                  intent(out) :: status
      end subroutine Int_LUT_TauSatRe
   end interface

   interface
      subroutine Int_LUT_TauSatSolAziRe(F, Grid, GZero, Ctrl, FInt, FGrads, icrpr, status)

         use CTRL_def
	 use GZero_def
	 use SAD_LUT_def

	 implicit none

	 real, dimension(:,:,:,:,:,:), intent(in)  :: F
	 type(LUT_Grid_t),             intent(in)  :: Grid
	 type(GZero_t),                intent(in)  :: GZero
         type(CTRL_t),                 intent(in)  :: Ctrl
	 real, dimension(:),           intent(out) :: FInt
	 real, dimension(:,:),         intent(out) :: FGrads
         integer,                      intent(in)  :: icrpr
         integer,                      intent(out) :: status
      end subroutine Int_LUT_TauSatSolAziRe
   end interface

   interface
      subroutine spline(x,y,y2)

         implicit none

         real, dimension(:), intent(in)  :: x,y
         real, dimension(:), intent(out) :: y2

      end subroutine spline
   end interface

   interface
      Function locate(xx,x)

         real, dimension(:), intent(in) :: xx
         real,               intent(in) :: x
         integer                        :: locate

      end Function locate
   end interface

end module Int_Routines_def
