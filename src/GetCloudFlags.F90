!-------------------------------------------------------------------------------
! Name:
!    Get_CloudFlags
!
! Purpose:
!    Gets the cloud flags for the current super-pixel.
!
! Arguments:
!    Name     Type    In/Out Description
!    Ctrl     struct  In     Control structure
!    SPixel   struct  Both   Super-pixel structure
!    MSI_Data struct  In     Data structure. Contains the multi-spectral image
!                            measurements, location values, geometry etc for the
!                            current image segment, from which the current
!                            SPixel values will be extracted.
!    status   integer Out    Error status
!
! Algorithm:
!    For Central pixel averaging:
!      - zero the super-pixel cloud flags
!      - copy the central pixel flag from the MSI_Data array
!      - no. of cloudy pixels is 0 or 1, i.e. same as the flag value
!      - fraction of cloudy pixels is as above
!    For averaging of Cloudy or All pixels:
!      - Take cloud flags array from Data structure - apply mask to give good
!        pixels only.
!      - Cloud fraction method depends on Ctrl%Resoln%Ameth:
!        if 'all' - fraction taken from all good pixels,
!        if 'cloudy' - fraction is unity (or zero if no cloudy pixels)
!    NCloudy is always the number of cloudy pixels in the super pixel.
!
! Local variables:
!    Name Type Description
!
! History:
!    30th Nov 2000, Kevin M. Smith: Original version
!     4th Dec 2000, Kevin M. Smith:
!       Corrected 'average all' method.
!    19th Dec 2000, Kevin M. Smith:
!       Replaced Data_CloudFlags with Data structure.
!     8th Jan 2001, Kevin M. Smith:
!       Included quality control mask.
!    24th Jan 2001, Kevin M. Smith:
!       Moved allocations to ECP main on integration.
!    16th Mar 2001, Andy Smith:
!       Removed checking for invalid averaging method.
!       Using named constants for averaging method.
!     7th Aug 2001, Andy Smith:
!       Updates for image segmentation. Selection of values from the MSI Data
!       structure arrays now need to use a y value that refers to the image
!       segment currently held in memory rather than the whole image area.
!       X co-ords are unchanged since the segment is the full image width.
!       Renamed structure Data to MSI_Data since Data is a reserved word (hasn't
!       caused any problems so far but it might).
!       Added argument intent.
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Reapplying changes originally made in late 2001/2002.
!    29th Nov 2001, Andy Smith:
!       Changed fraction setting for cloudy method so that fraction is zero when
!       there are no cloudy pixels.
!       Applied Mask to central pixel calculation, for consistency with the All
!       and Cloudy cases.
!       Re-structured so that cloud flags aren't calculated for the whole SPixel
!       if only the central pixel is required.
!       Use of "float" intrinsic function replaced by real.
!    10th Dec 2001, Andy Smith:
!       Bug fix: Xc, Yc were declared as real, not integer.
!
! Bugs:
!   Legacy code for super pixeling.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Get_CloudFlags(Ctrl, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(Data_t),   intent(in)    :: MSI_Data
   integer,        intent(out)   :: status

   ! Define local variables

   character(180) :: message
   integer        :: Xc, Yc ! Centre co-ordinates of SPixel arrays

   ! Set status to zero
   status = 0

   if (Ctrl%Resoln%Ameth == AMethCentral) then
      ! 'Central pixel' method.
      ! Take the 'central' pixel as defined in SPixel
      ! structure and apply the mask to exclude out of range pixels.
      ! Clear the flags array and set the centre value only.

      Xc = SPixel%Loc%Xc-SPixel%Loc%X0+1
      Yc = SPixel%Loc%Yc-SPixel%Loc%Y0+1
      SPixel%Cloud%Flags = 0
      SPixel%Cloud%Flags(Xc, Yc) = &
         MSI_Data%CloudFlags(SPixel%Loc%Xc, SPixel%Loc%YSegc) * &
         SPixel%Mask(Xc, Yc)

      SPixel%Cloud%NCloudy  = SPixel%Cloud%Flags(Xc, Yc)
      SPixel%Cloud%Fraction = real(SPixel%Cloud%NCloudy)
   else
      ! Assign super-pixel cloud flags (exclude 'out of range' pixels)

      SPixel%Cloud%Flags(:,:) = &
         MSI_Data%CloudFlags(SPixel%Loc%X0:SPixel%Loc%Xn, &
         SPixel%Loc%YSeg0:SPixel%Loc%YSegn) * SPixel%Mask

      ! Calculate number of cloudy pixels in super-pixel (excluding 'out of
      ! range' pixels)
      SPixel%Cloud%NCloudy = sum( int(SPixel%Cloud%Flags(:,:) * SPixel%Mask) )

      ! Calculate cloud fraction according to averaging method
      if (Ctrl%Resoln%Ameth == AMethAll) then
         ! 'All' method (number of 'good' cloudy pixels / total number of 'good'
         ! pixels)

         if (SPixel%Cloud%NCloudy /= 0) then
            SPixel%Cloud%Fraction = float( SPixel%Cloud%NCloudy ) / &
                                    float( SPixel%NMask )
         else
            SPixel%Cloud%Fraction = 0.0
         end if
      else if (Ctrl%Resoln%Ameth == AMethCloudy) then
         ! 'Cloudy pixel' method (average is unity - by definition)

         if (SPixel%Cloud%NCloudy /= 0) then
            SPixel%Cloud%Fraction = 1.0
         else
            SPixel%Cloud%Fraction = 0.0
         end if
      end if
   end if

end subroutine Get_CloudFlags
