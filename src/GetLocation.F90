!-------------------------------------------------------------------------------
! Name:
!    Get_Location
!
! Purpose:
!    Calculates the average location for the current super-pixel according to
!    the required averaging method.
!
! Arguments:
!    Name     Type   In/Out Description
!    Ctrl     struct In     Control structure
!    SPixel   struct Both   Super-pixel structure
!    MSI_Data struct In     Data structure. Contains the multi-spectral image
!                           measurements, location values, geometry etc for the
!                           current image segment, from which the current SPixel
!                           values will be extracted.
!    status   integer Out   Error status
!
! Algorithm:
!    Determine which averaging method is required
!       'All'
!          Average geometry of all 'good' pixels in the super-pixel
!       'Cloudy'
!          Average all 'good' cloudy pixels as defined by SPixel%CloudFlags
!       'Central'
!          Take geometry from the 'central' pixel as defined by SPixel%Loc%Xc
!          and Yc
!    Average location output to SPixel%Loc
!
! Local variables:
!    Name Type Description
!
! History:
!     5th Dec, 2000, Kevin M. Smith: Original version
!    19th Dec, 2000, Kevin M. Smith:
!       Replaced Data_Location array with Data structure
!     8th Jan, 2001, Kevin M. Smith:
!       Included quality control mask
!    16th Mar 2001, Andy Smith:
!       Using named constants for averaging methods.
!       Removed check for invalid averaging method (now in ReadDriver)
!     7th Aug 2001, Andy Smith:
!       Updates for image segmentation. Selection of values from the MSI Data
!       structure arrays now need to use a y value that refers to the image
!       segment currently held in memory rather than the whole image area.
!       X co-ords are unchanged since the segment is the full image width.
!       Renamed structure Data to MSI_Data since Data is a reserved word (hasn't
!       caused any problems so far but it might).
!       Added argument intent.
!    **************** ECV work starts here *************************************
!    23rd Feb 2011, Andy Smith:
!       Cloud flags converted to real to match current ORAC data.
!
! Bugs:
!   Legacy code for super pixeling.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Get_Location(Ctrl, SPixel, MSI_Data, status)

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

   ! Initialise
   status  = 0

   ! Assign SPixel values according to averaging method

   select case (Ctrl%Resoln%Ameth)
   case (AMethAll)
      ! 'All' method

      SPixel%Loc%Lat = sum(MSI_Data%Location%Lat(SPixel%Loc%X0:SPixel%Loc%Xn, &
         SPixel%Loc%YSeg0:SPixel%Loc%YSegn) &
         * float(SPixel%Mask(:,:))) / float(SPixel%NMask)

      SPixel%Loc%Lon = sum(MSI_Data%Location%Lon(SPixel%Loc%X0:SPixel%Loc%Xn, &
         SPixel%Loc%YSeg0:SPixel%Loc%YSegn) &
         * float(SPixel%Mask(:,:))) / float(SPixel%NMask)

   case (AMethCloudy)
      ! 'Cloudy pixel' method (averages only the cloudy pixels - excludes
      ! 'out of range' pixels)

      SPixel%Loc%Lat = sum(MSI_Data%Location%Lat(SPixel%Loc%X0:SPixel%Loc%Xn, &
         SPixel%Loc%YSeg0:SPixel%Loc%YSegn) &
         * SPixel%Cloud%Flags) / float(SPixel%Cloud%NCloudy)

      SPixel%Loc%Lon = sum(MSI_Data%Location%Lon(SPixel%Loc%X0:SPixel%Loc%Xn, &
         SPixel%Loc%YSeg0:SPixel%Loc%YSegn) &
         * SPixel%Cloud%Flags) / float(SPixel%Cloud%NCloudy)

   case (AMethCentral)
      ! 'Central pixel' method (takes the 'central' pixel as defined in SPixel
      ! structure)

      SPixel%Loc%Lat = MSI_Data%Location%Lat(SPixel%Loc%Xc, SPixel%Loc%YSegc)
      SPixel%Loc%Lon = MSI_Data%Location%Lon(SPixel%Loc%Xc, SPixel%Loc%YSegc)

    end select

end subroutine Get_Location
