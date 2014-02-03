! Name:
!    Read_Scanlines
!
! Purpose:
!    Controls the reading of scan lines
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
!         Do 1 to 2 (Read u and v scan.)
!            Read array of size defined by Ctrl structure
!            If read error
!                Write error message to screen and log file
!            End if
!    (Leave uv file open for subsequent reads).
!
! Local variables:
!   Name   Type   Description
!   ios    int    I/O status, file operations
!   lun    int    File unit number
!   array  float  Temporary array (size 2, Xmax) used to read in a row of 
!                 u and v scan values. 
!   row    int    Image segment row counter. Used to check for premature
!                 end of file if EOF detected during segment read.
!
! History:
!   28th July 2011 created file began with Readscanline file and changed to read scan lines
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Read_Scanlines(Ctrl, NSegs, SegSize, MSI_files_open, lun, &
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
   real          :: array(2, Ctrl%Ind%XMax)  
                             ! Holds 1 row of data from the file
   integer        :: row     ! Row number within image segment


!  On first call, the file is opened. It is then left open for all subsequent
!  calls.

   status = 0
   if (.not. MSI_files_open) then
!     Call Find_LUN to set suitable unit number for positional data file

      call Find_LUN(lun)

!     Open Scan data file

      open(unit=lun, file=Ctrl%Fid%uv, form='unformatted', status='old', &
           iostat=ios)

      if (ios /= 0) then
	 status = scanFileOpenErr ! Return error code
	 write(unit=message, fmt=*) &
	    'Read_Scanlines: Error opening file ', Ctrl%Fid%uv 
	 call Write_Log(Ctrl, trim(message), status)

      else
!        Open successful. Allocate MSI_Data%scan%uscan and vscan array sizes

         allocate(MSI_Data%Scan%uscan(Ctrl%Ind%Xmax, SegSize))
         allocate(MSI_Data%Scan%vscan(Ctrl%Ind%Xmax, SegSize))
         read(unit=lun, iostat=ios) ! Disregard header content

         if (ios /= 0) then
            status = scanFileReadHeadErr ! Return error code
            write(unit=message, fmt=*) &
              'Read_Scanlines: Error reading header of file ', Ctrl%Fid%uv
            call Write_Log(Ctrl, trim(message), status)

         end if
      end if
   end if

!  Read Scanlines data
!  Read the angles in 1 row at a time, both angles for each x value.

   if (status == 0) then
      do row = 1, SegSize
	 read(unit=lun, iostat=ios) array 

	 if (ios /= 0) exit ! Drop out of the loop on read error

	 MSI_Data%Scan%uscan(:,row) = array(1,:)       
	 MSI_Data%Scan%vscan(:,row) = array(2,:)

      end do        
	 
!     ios > 0 indicates a read error.
!     ios < 0 indicates end of record or end of file. EOF is ok if it's in
!     the right place (in order to allow any super-pixel size, we must
!     deal with EOF part way through reading a segment).

      if (ios > 0) then
	 status = scanFileReadDataErr ! Return error code
	 write(unit=message, fmt=*) &
	    'Read_Scanlines: Error reading data in file ', Ctrl%Fid%uv
	 call Write_Log(Ctrl, trim(message), status)

      else if (ios < 0) then

!        Check for premature end of file during read. Compare the current 
!        image row count to the total expected.

	 row = row + NSegs * SegSize
	 if (row /= Ctrl%Ind%YMax+1) then
	    status = scanFileEOFErr
	    write(unit=message, fmt=*) &
	       'Read_Scanlines: End of file during read: file ', Ctrl%Fid%uv
            call Write_Log(Ctrl, trim(message), status)
	 end if
      endif
   endif

!   Leave Scanlines data file open for subsequent reads.
end subroutine Read_Scanlines
