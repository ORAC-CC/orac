! Name:
!   Read_CloudFlags
!
! Purpose:
!   Controls the reading of cloud flags values from ATSR-type files into
!   the Data_CloudFlags array.
!
! Arguments:
!   Name     Type           In/Out   Description
!   Ctrl     struct         Both     Control structure
!   NSegs    int            In       Number of image segments read in by 
!                                    previous calls to this routine.
!   SegSize  int           In        Number of rows of pixels in an image 
!                                    segment.
!   MSI_files_open Logical In        Indicates whether the MSI data file is
!                                    open (if not, open it).
!   lun      int           In/Out    File unit number set by this routine
!                                    when file is opened, passed back and
!                                    forth for subsequent reads.
!   MSI_Data struct         Both     Data structure: contains the cloud flag
!                                    array to be populated with data from the 
!                                    file. This is overwritten as successive
!                                    segments of data are read in.
!   status   int            Out      Error status                         
!       
! Algorithm:
!   if (MSI files are not yet open)
!      Find a logical unit number to be used for the cloud flag file
!      Open cloud flag file
!      If open error
!         Write error message to screen and log file
!      else
!        allocate MSI image segment array in Data_MSI struct.
!
!   If (no error opening files)
!       Read header (not used further)
!       If read error
!          Write error message to screen and log file
!       Else
!          Read byte array of size defined by Ctrl structure
!          If read error
!             Write error message to log file
!   Leave cloud flag file open for later reads
!
! Local variables:
!   Name     Type   Description
!   ios      int    I/O status, file operations
!   message  char   Error message to pass to Write_Log   
!   row      int    Number of last image row read by ReadByteArray.  
!
! History:
!   3rd November, 2000, Kevin M. Smith : original version
!   23rd November, 2000, KMS: Added status to argument list
!    3rd August 2001, Andy Smith:
!      Updates for handling image segmentation:
!      - new arguments MSI_files_open, lun (since the file now stays open for
!        repeated read operations)
!      - file open depends on MSI_files_open flag
!      Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).
!      Added argument intent specifiers.
!   10th Aug 2001, Andy Smith:
!      Updated to handle image segments/super-pixels of any size. Requires
!      handling of end of file during read on the last segment. On earlier 
!      segments EOF is reported as an error.
!   22nd Aug 2001, Andy Smith:
!      Bug fix: added status check before reading arrays in.
!    *************************** ECV work starts here *********************
!   23rd Feb 2011, Andy Smith:
!      Replaced call to ReadByteArray with ReadFPArray to cope with current 
!      preprocessed data files from ORAC. 
!    28th June 2011 Caroline poulsen remove reference to ATSR
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Read_CloudFlags_nc(Ctrl, NSegs, SegSize, MSI_files_open, lun, &
   MSI_Data, status)

   use CTRL_def
   use ECP_Constants
   use Data_def

   implicit none

!  Argument declarations

   type(CTRL_t), intent(in)      :: Ctrl
   integer, intent(in)           :: NSegs     ! Number of segments read so far
   integer, intent(in)           :: SegSize   ! Size of image segment in rows of
                                              ! pixels.
   logical, intent(in)           :: MSI_files_open
   integer, intent(inout)        :: lun       ! Unit number for MSI file   
   type(Data_t), intent(inout)   :: MSI_Data
   integer, intent(out)          :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
   integer        :: row       ! Number of final image row read by ReadByteArray
                               ! (in pixels, starting at first row of segment)


!  On first call, the file is opened. It is then left open for all subsequent
!  calls. 

   status = 0
   if (.not. MSI_files_open) then
!     Call Find_LUN to set suitable unit number for cloud flag file

      call Find_LUN(lun)

!     Open cloud flag file

      open(unit=lun, file=Ctrl%Fid%Cf, form='unformatted', status='old', &
           iostat=ios)
      if (ios /= 0) then
	 status = CfFileOpenErr ! Return error code
	 write(unit=message, fmt=*) 'Read_CloudFlags: Error opening file ', &
            Ctrl%Fid%Cf 
	 call Write_Log(Ctrl, trim(message), status)
      else
!        Open successful. Allocate MSI_Data%CloudFlags array size

         allocate(MSI_Data%CloudFlags(Ctrl%Ind%Xmax, SegSize))
         read(unit=lun, iostat=ios) ! Disregard header content
         if (ios /= 0) then
            status = CfFileReadHeadErr ! Return error code
            write(unit=message, fmt=*) &
	        'Read_CloudFlags: Error reading header of file ', Ctrl%Fid%Cf
            call Write_Log(Ctrl, trim(message), status)
         end if
      end if
   end if


!  Call generic three dimensional float array reading
!  subroutine - use in two dimensional mode

   if (status == 0) then
      call Read_FPArray(lun, Ctrl%Ind%Xmax, SegSize, 1, 1, 1, &
	 MSI_Data%CloudFlags, row, ios)

!     ios > 0 indicates a read error.
!     ios < 0 indicates end of record or end of file. EOF is ok if it's in
!     the right place (in order to allow any super-pixel size, we must
!     deal with EOF part way through reading a segment).

      if (ios > 0) then
	 status = CfFileReadDataErr ! Return error code
	 write(unit=message, fmt=*) &
	      'Read_CloudFlags: Error reading data in file ', Ctrl%Fid%Cf
	 call Write_Log(Ctrl, trim(message), status)

      else if (ios < 0) then
   
!        Check for premature end of file during read. Convert the last row read
!        by ReadFPArray from the current segment into an abolute value within
!        the image and check vs. image size.

	 row = row + (NSegs * SegSize)
	 if (row /= Ctrl%Ind%YMax+1) then
	    status = CfFileEOFErr
	    write(unit=message, fmt=*) &
		'Read_CloudFlags: End of file during read: file ', &
		Ctrl%Fid%CF
            call Write_Log(Ctrl, trim(message), status)
	 end if
      endif
   endif

!  MSI file is not closed. Leave open for subsequent read of the rest of the
!  image.

 end subroutine Read_CloudFlags_nc
