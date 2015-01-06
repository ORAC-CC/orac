!-------------------------------------------------------------------------------
! Name:
!    Get_Rs
!
! Purpose:
!    Calculates the super pixel mean surface reflectivities (and covariances)
!    for the solar channels according to the specified averaging method
!    Ctrl%Resoln%Ameth
!
! Arguments:
!    Name      Type         In/Out/Both Description
!    Ctrl      struct       In          Control structure
!    SPixel    alloc struct Both        Super pixel structure
!    SPixel_b  real array   In          Super pixel array of b (using super
!                                       pixel surface flags)
!    SPixel_Sb real array   In          Super pixel array of Sb (using super
!                                       pixel surface flags)
!    status    integer      Out         Error status
!
! Algorithm:
!    Average method:
!       'All'     - average super pixel surface reflectance array values (and
!                   covariances) over all good pixels,
!       'Cloudy'  - average over good cloudy pixels,
!       'Central' - take values from the central pixel, if it is good.
!
! Local variables:
!    Name Type Description
!
! History:
!     4th Dec 2000, Kevin M. Smith: Original version
!    16th Jan 2001, Kevin M. Smith:
!       Re-drafted
!    18th Jan 2001, Kevin M. Smith:
!       Modified to take array of reflectances etc from SPixel%Surface
!       Changed name from Get_Surface_Rs to Get_Rs
!    16th Mar 2001, Andy Smith:
!       Removed checking for invalid averaging method.
!       Using named constants for averaging method.
!    15th Jun 2001, Andy Smith:
!       Changed error message string assignments/writes.
!       Long message strings were wrapped over two lines with only one set of
!       quotes around the whole string (including the line continuation marker).
!       Original code works on DEC but not Linux.
!    **************** ECV work starts here *************************************
!    23rd Feb 2011, Andy Smith:
!       Cloud flags converted to real to match current ORAC data.
!    30th Mar 2011, Andy Smith:
!       Removal of super-pixel averaging.
!       Process single pixels at X0, Y0,
!       Removed other SPixel indices Xc, Yc, Xn, Yn etc.
!       Removed selection of averaging method.
!       SPixel_B and SPixel_Sb re-dimensioned to remove SPixel size.
!    20th Jan 2012, Someone:
!       General tidyup remove data varaible
!    28th May 2014, McGarragh:
!       Some cleanup.
!     9th Sep 2014, Greg McGarragh:
!       Changes related to new BRDF support.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Get_Rs(Ctrl, SPixel, SPixel_b, SPixel_Sb, SPixel_b2, SPixel_Sb2, status)

   use CTRL_def
   use Data_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   real,           intent(in)    :: SPixel_b (Ctrl%Ind%NSolar)
   real,           intent(in)    :: SPixel_Sb(Ctrl%Ind%NSolar, Ctrl%Ind%NSolar)
   real,           intent(in)    :: SPixel_b2 (Ctrl%Ind%NSolar, MaxRho_XX)
   real,           intent(in)    :: SPixel_Sb2(Ctrl%Ind%NSolar, Ctrl%Ind%NSolar, MaxRho_XX)
   integer,        intent(out)   :: status

   ! Define local variables

   ! Set status to zero
   status = 0

   ! Check mask to see if pixel is 'good'
   if (SPixel%Mask == 1) then
      SPixel%Rs(:)    = SPixel_b(:)
      SPixel%SRs(:,:) = SPixel_Sb(:, :)

      if (Ctrl%RS%use_full_brdf) then
         SPixel%Rs2(:,:)    = SPixel_b2(:,:)
         SPixel%SRs2(:,:,:) = SPixel_Sb2(:, :,:)
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
