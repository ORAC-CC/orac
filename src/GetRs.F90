!-------------------------------------------------------------------------------
! Name: GetRs.F90
!
! Purpose:
! Calculates the super pixel mean surface reflectivities (and covariances)
! for the solar channels according to the specified averaging method
! Ctrl%Resoln%Ameth
!
! Description and Algorithm details:
!
! Arguments:
! Name       Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl       struct       In          Control structure
! SPixel     alloc struct Both        Super pixel structure
! SPixel_b   real array   In          Super pixel array of b
! SPixel_Sb  real array   In          Super pixel array of Sb
! SPixel_b2  real array   In          Super pixel array of b for BRDF
! SPixel_Sb2 real array   In          Super pixel array of Sb for BRDF
! status     integer      Out         Error status
!
! History:
! 2000/12/04, KS: Original version
! 2001/01/16, KS: Re-drafted
! 2001/01/18, KS: Modified to take array of reflectances etc from SPixel%Surface
!    Changed name from Get_Surface_Rs to Get_Rs
! 2001/03/16, AS: Removed checking for invalid averaging method. Using named
!    constants for averaging method.
! 2001/06/15, AS: Changed error message string assignments/writes. Long message
!    strings were wrapped over two lines with only one set of  quotes around the
!    whole string (including the line continuation marker). Original code works
!    on DEC but not Linux.
!    **************** ECV work starts here *************************************
! 2011/02/23, AS: Cloud flags converted to real to match current ORAC data.
! 2011/03/30, AS: Removal of super-pixel averaging. Process single pixels at X0,
!    Y0, Removed other SPixel indices Xc, Yc, Xn, Yn etc. Removed selection of 
!    averaging method. SPixel_B and SPixel_Sb re-dimensioned to remove SPixel
!    size.
! 2012/01/23, CP: General tidyup remove data variable
! 2014/05/28, GM: Some cleanup.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2015/01/21, AP: Corrected array dimensions.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_Rs(Ctrl, SPixel, SPixel_b, SPixel_Sb, SPixel_b2, SPixel_Sb2, &
                  status)

   use CTRL_def
   use Data_def

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   real,           intent(in)    :: SPixel_b (SPixel%Ind%NSolar)
   real,           intent(in)    :: SPixel_Sb(SPixel%Ind%NSolar, SPixel%Ind%NSolar)
   real,           intent(in)    :: SPixel_b2 (SPixel%Ind%NSolar, MaxRho_XX)
   real,           intent(in)    :: SPixel_Sb2(SPixel%Ind%NSolar, SPixel%Ind%NSolar, MaxRho_XX)
   integer,        intent(out)   :: status

   ! Define local variables

   ! Set status to zero
   status = 0

   ! Check mask to see if pixel is 'good'
   if (SPixel%Mask == 1) then
      SPixel%Rs  = SPixel_b
      SPixel%SRs = SPixel_Sb

      if (Ctrl%RS%use_full_brdf) then
         SPixel%Rs2  = SPixel_b2
         SPixel%SRs2 = SPixel_Sb2
      end if

      if (SPixel%Surface%Flags == 1) then
         SPixel%Surface%Land = 1
         SPixel%Surface%Sea  = 0
      else
         SPixel%Surface%Land = 0
         SPixel%Surface%Sea  = 1
      end if
   else
      write(*,*) 'ERROR: Get_Rs(): Pixel contains bad data'
      status = GetRsCentPix
   end if

end subroutine Get_Rs
