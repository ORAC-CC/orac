! Name:
!   Read_LSFlags
!
! Purpose:
!   Controls the reading of land/sea flags values from ATSR-type files into
!   the LSFlags array within the MSI_Data structure. Reading is done in 
!   segments, i.e. part of image rather than whole image.
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
!   MSI_Data struct        In/Out    Data structure: the MSI data part
!                                    of this struct is populated by this
!                                    routine, and is overwritten on
!                                    successive calls.
!   status   int            Out      Error status
!    
! Algorithm:
!   if (MSI files are not yet open)
!      Find a logical unit number to be used for the land/sea flag file
!      Open l/s flag file
!      If open error
!         Write error message to screen and log file
!      else
!         allocate LS flag segment array in Data_MSI struct.
!
!   If (no error opening files)
!      Read header (not used further)
!      If read error
!         Write error message to screen and log file
!      Else
!         Read byte array of size defined by Ctrl structure
!         If read error
!            Write error message to log file
!   (Leave LS flag file open for further reads)
!
! Local variables:
!   Name   Type   Description
!   ios    int    I/O status, file operations
!   message   char   Error message to pass to Write_Log   
!   row       int    Number of last image row read by ReadByteArray.  
!
! History:
!   3rd November, 2000, Kevin M. Smith : original version
!   23rd November, 2000, KMS: Added status to argument list
!   19th December, 2000, KMS: Replaced Data_LSFLags with Data structure
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
!   28th June 2011 Caroline poulsen remove reference to ATSR
! Bugs:
!   None known.
!
! $Id: ReadLSFlags.f90 182 2011-10-05 10:03:40Z carnold $
!
!------------------------------------------------------------------------------------
subroutine Read_LSFlags(Ctrl, NSegs, SegSize, MSI_files_open, lun, &
   MSI_Data, status)

   use CTRL_def
   use ECP_constants
   use Data_def

   implicit none

!  Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer, intent(in)         :: NSegs     ! Number of segments read so far
   integer, intent(in)         :: SegSize   ! Size of image segment in rows of
                                            ! pixels.
   logical, intent(in)         :: MSI_files_open
   integer, intent(inout)      :: lun       ! Unit number for MSI file   
   type(Data_t), intent(inout) :: MSI_Data
   integer, intent(out)        :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
   integer        :: row       ! Number of final image row read by ReadByteArray
                               ! (in pixels, starting at first row of segment)


!  On first call, the file is opened. It is then left open for all subsequent
!  calls.

   status = 0
   if (.not. MSI_files_open) then
!     Call Find_LUN to set suitable unit number for land/sea flag file

      call Find_LUN(lun)

!     Open cloud land/sea flag file

      open(unit=lun, file=Ctrl%Fid%Ls, form='unformatted', status='old', &
           iostat=ios)
      if (ios /= 0) then
	 status = LsFileOpenErr ! Return error code
	 write(unit=message, fmt=*) &
              'Read_LSFlags: Error opening file ', Ctrl%Fid%Ls 
	 call Write_Log(Ctrl, trim(message), status)
      else
!        Open successful. Allocate MSI_Data%LSFlags array size

         allocate(MSI_Data%LSFlags(Ctrl%Ind%Xmax, SegSize))
         read(unit=lun, iostat=ios) ! Disregard header content
         if (ios /= 0) then
            status = LsFileReadHeadErr ! Return error code
            write(unit=message, fmt=*) &
	        'Read_LSFlags: Error reading header of file ', Ctrl%Fid%Ls
            call Write_Log(Ctrl, trim(message), status)
         end if
      end if
   end if

!  Call Read_ByteArray (generic three dimensional byte array reading
!  subroutine) - use in two dimensional mode

   if (status == 0) then
      call Read_ByteArray(lun, Ctrl%Ind%Xmax, SegSize, 1, 1, &
	 MSI_Data%LSFlags, row, ios)

!     ios > 0 indicates a read error.
!     ios < 0 indicates end of record or end of file. EOF is ok if it's in
!     the right place (in order to allow any super-pixel size, we must
!     deal with EOF part way through reading a segment).

      if (ios > 0) then
	 status = LsFileReadDataErr ! Return error code
	 write(unit=message, fmt=*) &
	      'Read_LSFlags: Error reading data in file ', Ctrl%Fid%Ls
	 call Write_Log(Ctrl, trim(message), status)

      else if (ios < 0) then

!        Check for premature end of file during read. Convert the last row read
!        by ReadFPArray from the current segment into an abolute value within
!        the image and check vs. image size.

	 row = row + (NSegs * SegSize)
	 if (row /= Ctrl%Ind%YMax+1) then
	    status = LSFileEOFErr
	    write(unit=message, fmt=*) &
	       'Read_LSFlags: End of file during read: file ', Ctrl%Fid%LS
            call Write_Log(Ctrl, trim(message), status)
	 end if
      endif
   end if

!  MSI file is not closed. Leave open for subsequent read of the rest of the
!  image.

end subroutine Read_LSFlags
