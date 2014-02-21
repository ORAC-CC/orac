! Name:
!    Read_Location
!
! Purpose:
!    Controls the reading of latitude and longitude data from ATSR-type files
!    into the Data_Location array.
!
! Arguments:
!   Name     Type          In/Out   Description
!   Ctrl     struct        Both     Control structure
!   NSegs    int           In       Number of image segments read in by
!                                   previous calls to this routine.
!   SegSize  int           In       Size of image segment in rows of pixels.
!   MSI_files_open Logical In       Indicates whether the MSI data file is
!                                   open (if not, open it).
!   lun      int           In/Out   File unit number set by this routine
!                                   when file is opened, passed back and
!                                   forth for subsequent reads.
!   MSI_Data struct        In/Out   Data structure: the MSI data part
!                                   of this struct is populated by this
!                                   routine, and is overwritten on
!                                   successive calls.
!   status   int           Out      Error status
!    
! Algorithm:
!   if (MSI files are not yet open)
!      Find a logical unit number to be used for the location data file
!      Open location file
!      If open error
!         Write error message to log file
!      else
!         allocate MSI location arrays in Data_MSI struct to segment size.
!
!   If (no error opening files)
!      Read header (not used further)
!      If read error
!         Write error message to screen and log file
!      Else
!         Do 1 to 2 (Read lat. and long.)
!            Read array of size defined by Ctrl structure
!            If read error
!                Write error message to screen and log file
!            End if
!    (Leave location file open for subsequent reads).
!
! Local variables:
!   Name   Type   Description
!   ios    int    I/O status, file operations
!   lun    int    File unit number
!   array  float  Temporary array (size 2, Xmax) used to read in a row of 
!                 lat, long values. 
!   row    int    Image segment row counter. Used to check for premature
!                 end of file if EOF detected during segment read.
!
! History:
!   22nd November, 2000, Kevin M. Smith : original version
!   23rd November, 2000, KMS: Added status to argument list
!   19th December, 2000, KMS: Replaced Data_Location array with Data structure
!   31st July 2001, Andy Smith:
!      Changes in preparation for image segmentation. Data is now stored in the
!      file in an order such that all information for each pixel is available
!      in the same place (rather than all lat values then all lon values).
!      Allows for easier access to all required data for a partial image.
!      Not using ReadFPArray at present.
!      No header in re-written data file (may be required again later).
!      New arguments MSI_files_open, lun (since the file now stays open for
!      repeated read operations)
!      File open depends on MSI_files_open flag
!      Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).
!      Added argument intent specifiers.
!   10th Aug 2001, Andy Smith:
!      Updated to handle image segments/super-pixels of any size. Requires
!      handling of end of file during read on the last segment. On earlier 
!      segments EOF is reported as an error.
!   22nd Aug 2001, Andy Smith:
!      Bug fix: added status check before reading arrays in.
!28th June 2011 Caroline poulsen remove reference to ATSR
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Read_Location(Ctrl, NSegs, SegSize, MSI_files_open, lun, &
   MSI_Data, status)

   use CTRL_def
   use ECP_Constants
   use Data_def

   implicit none

!  Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer, intent(in)         :: NSegs     ! Number of segments read so far
   integer, intent(in)         :: SegSize   ! Size of segment to read
   logical, intent(in)         :: MSI_files_open
   integer, intent(inout)      :: lun       ! Unit number for MSI file   
   type(Data_t), intent(inout) :: MSI_Data
   integer, intent(out)        :: status

!  Local variables

   integer        :: ios     ! I/O status from file operations
   character(180) :: message ! Error message to pass to Write_Log
   real           :: array(2, Ctrl%Ind%XMax)  
                             ! Holds 1 row of data from the file
   integer        :: row     ! Row number within image segment


!  On first call, the file is opened. It is then left open for all subsequent
!  calls.

   status = 0
   if (.not. MSI_files_open) then
!     Call Find_LUN to set suitable unit number for positional data file

      call Find_LUN(lun)

!     Open location data file

      open(unit=lun, file=Ctrl%Fid%Loc, form='unformatted', status='old', &
           iostat=ios)
      if (ios /= 0) then
	 status = LocFileOpenErr ! Return error code
	 write(unit=message, fmt=*) &
	    'Read_Location: Error opening file ', Ctrl%Fid%Loc 
	 call Write_Log(Ctrl, trim(message), status)
      else
!        Open successful. Allocate MSI_Data%Location%Lat and Lon array sizes

         allocate(MSI_Data%Location%Lat(Ctrl%Ind%Xmax, SegSize))
	 allocate(MSI_Data%Location%Lon(Ctrl%Ind%Xmax, SegSize))
         read(unit=lun, iostat=ios) ! Disregard header content
         if (ios /= 0) then
            status = LocFileReadHeadErr ! Return error code
            write(unit=message, fmt=*) &
              'Read_Location: Error reading header of file ', Ctrl%Fid%Loc
            call Write_Log(Ctrl, trim(message), status)
         end if
      end if
   end if

!  Read location data (latitudes and longitudes)
!  Read the angles in 1 row at a time, both angles for each x value.

   if (status == 0) then
      do row = 1, SegSize
	 read(unit=lun, iostat=ios) array 
	 if (ios /= 0) exit ! Drop out of the loop on read error

	 MSI_Data%Location%Lat(:,row) = array(1,:)       
	 MSI_Data%Location%Lon(:,row) = array(2,:)       
      end do        
	 
!     ios > 0 indicates a read error.
!     ios < 0 indicates end of record or end of file. EOF is ok if it's in
!     the right place (in order to allow any super-pixel size, we must
!     deal with EOF part way through reading a segment).

      if (ios > 0) then
	 status = LocFileReadDataErr ! Return error code
	 write(unit=message, fmt=*) &
	    'Read_Location: Error reading data in file ', Ctrl%Fid%Loc
	 call Write_Log(Ctrl, trim(message), status)

      else if (ios < 0) then

!        Check for premature end of file during read. Compare the current 
!        image row count to the total expected.

	 row = row + NSegs * SegSize
	 if (row /= Ctrl%Ind%YMax+1) then
	    status = LocFileEOFErr
	    write(unit=message, fmt=*) &
	       'Read_Location: End of file during read: file ', Ctrl%Fid%Loc
            call Write_Log(Ctrl, trim(message), status)
	 end if
      endif
   endif

!   Leave location data file open for subsequent reads.
end subroutine Read_Location
