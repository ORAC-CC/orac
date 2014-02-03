! Name:
!    Read_ALB
!
! Purpose:
!    Controls the reading of Multi Spectral Image (ALB) values from ATSR-type
!    files into the DATA_ALB array.
!
! Arguments:
!   Name     Type          In/Out/Both   Description
!   Ctrl     struct        Both          Control structure (date is read in 
!                                        here).
!   NSegs    int           In            Number of image segments read in by 
!                                        previous calls to this routine.
!   SegSize  int           In            Number of rows of pixels in an image 
!                                        segment.
!   ALB_files_open Logical In            Indicates whether the ALB data file is
!                                        open (if not, open it).
!   lun      int           In/Out        File unit number set by this routine
!                                        when file is opened, passed back and
!                                        forth for subsequent reads.
!   MSI_Data struct        In/Out        Data structure: the ALB data part 
!                                        of this struct is populated by this
!                                        routine, and is overwritten on 
!                                        successive calls.
!   SAD_Chan struct array  Both          Instrument channel parameters. Updated
!                                        by this routine: solar constant is 
!                                        modified from annual average to value 
!                                        for the day of the ALB data.
!   status   int           Out           Error status          
!    
! Algorithm:
!   if (ALB files are not yet open)
!      Find a logical unit number to be used for the ALB file
!      Open ALB file
!      If open error
!         Write error message to screen and log file
!      else
!         allocate ALB image segment array in Data_ALB struct.
!   
!   If (no error opening files)
!      Read header (not used further)
!      If read error
!         Write error message to screen and log file
!      Else
!         Read ALB array of size defined by Ctrl structure
!         If read error
!            Write error message to log file
!         If end of file or end of record (io status < 0)
!            Check current segment no. vs. expected last segment in file
!            if current segment is not last, report error
!   Leave ALB file open for further reads
!
! Local variables:
!   Name      Type   Description
!   ios       int    I/O status, file operations
!   message   char   Error message to pass to Write_Log 
!   header    char   Header data from input file
!   day, month int   Day and month numbers extracted from header and used in 
!                    calculation of Ctrl%DOY.
!   mon       char   Month name extracted from header date string.
!   i         int    Counter for DOY calculation
!   row       int    Number of last image row read by ReadFPArray.
!
! History:
!   29th May, 2002, Caroline Poulsen : original version copied
!                                 from READ_MSI 
!   29th Oct, 2002, Caroline Poulsen : fixed bug too many arguments in
!      the header removed sad_chan
!   28th June 2011 Caroline poulsen remove reference to ATSR
!   13th Dec 2011 Caroline poulsen change format statement to make
!                g95 compatible 
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
Subroutine Read_ALB(Ctrl, NSegs, SegSize, ALB_files_open, lun, MSI_Data, status)
   
   use ECP_Constants
   use CTRL_def
   use Data_def
   use SAD_Chan_def

   implicit none

!  Argument declarations

   type(CTRL_t), intent(inout) :: Ctrl
   integer, intent(in)         :: NSegs     ! Number of segments read so far
   integer, intent(in)         :: SegSize   ! Size of image segment in rows of
                                            ! pixels.
   logical, intent(in)         :: ALB_files_open
   integer, intent(inout)      :: lun       ! Unit number for ALB file   
   type(Data_t), intent(inout) :: MSI_Data
   integer, intent(out)        :: status
   integer, dimension(6) :: test=(/1,2,3,4,5,6/)
!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
   character(24)  :: header    ! Input file header record
   integer        :: day, month ! Day and month numberss extracted from 
                               ! Date string when calculating DOY.
   character(3)   :: mon       ! Month extracted from date in file header.
   integer        :: row       ! Number of final image row read by ReadFPArray
                               ! (in pixels, starting at first row of segment)
   integer        :: i         ! Counter for DOY calculation


!  On first call, the file is opened. It is then left open for all subsequent
!  calls. 

   status = 0
   if (.not. ALB_files_open) then
!     Call Find_LUN to set suitable unit number for ALB file

      call Find_LUN(lun)

!     Open ALB file

      open(unit=lun, file=Ctrl%Fid%AUX, form='unformatted', status='old', &
           iostat=ios)
      write(*,*) 'alb 1 ',ios
      if (ios /= 0) then
	 status = ALBFileOpenErr ! Return error code
  write(unit=message, fmt=*) &
              'Read_ALB: Error opening file ', trim(adjustl(Ctrl%Fid%AUX))
	 call Write_Log(Ctrl, trim(message), status)
      else
!        Allocate Data%ALB structure size to match image segments to be read in.

         allocate(MSI_Data%ALB(Ctrl%Ind%Xmax, SegSize, 6))

!        Read file header and extract the date: use date to calculate day of
!        year. Date format is DD-MMM-YYYY HH:MM:SS.mmm

         read(unit=lun, iostat=ios) header
         if (ios == 0) then
	    read(header, '(a11,1x,a12)', iostat=ios) Ctrl%Date, Ctrl%Time
     write(*,*) 'alb 2 ',ios
	    read(unit=Ctrl%Date, fmt='(i2,1x,a3)') day, mon

!           months, days_in_month are defined in ECP_Constants.

	    month = index(months,mon) / 3 + 1
	    Ctrl%DOY = 0
	    do i=1,month-1
               Ctrl%DOY = Ctrl%DOY + days_in_month(i)
	    end do
	    Ctrl%DOY = Ctrl%DOY + day      	    

!           Calculate solar constant based on day of year using the mean
!           and amplitude of variation.

!            do i = 1, Ctrl%Ind%NSolar
!	       if (SAD_Chan(i)%Solar%Flag > 0) &
!        	  SAD_Chan(i)%Solar%F0 = SAD_Chan(i)%Solar%F0 + &
!	             (SAD_Chan(i)%Solar%F1 * cos(2 * Pi * Ctrl%DOY / 365.))
!	    end do

	 else
            status = ALBFileReadHeadErr ! Return error code
            write(unit=message, fmt=*) &
   	        'Read_ALB: Error reading header of file ', Ctrl%Fid%AUX
            call Write_Log(Ctrl, trim(message), status)
         endif

      end if
   end if

!  Read ALB file 

!  Call Read_FPArray (generic three dimensional float array reading
!  subroutine) to read enough rows of pixels to fill the segment array.

   if (status == 0) then

      call Read_IntArray(lun, Ctrl%Ind%Xmax, SegSize,  5, &
          5, test, MSI_Data%ALB(:,:,:), row, ios)
      write(*,*) 'alb 3 ',ios

!     ios > 0 indicates a read error.
!     ios < 0 indicates end of record or end of file. EOF is ok if it's in
!     the right place (in order to allow any super-pixel size, we must
!     deal with EOF part way through reading a segment).

      if (ios > 0) then
	 status = ALBFileReadDataErr ! Return error code
	 write(unit=message, fmt=*) &
	      'Read_ALB: Error reading data in file ', Ctrl%Fid%AUX
	 call Write_Log(Ctrl, trim(message), status)

      else if (ios < 0) then

!     Check for premature end of file during read. Convert the last row read
!     by ReadINTArray from the current segment into an abolute value within the 
!     image and check vs. image size.

	 row = row + (NSegs * SegSize)
	 if (row /= Ctrl%Ind%YMax+1) then
	    status = ALBFileEOFErr
	    write(unit=message, fmt=*) &
	       'Read_ALB: End of file during read: file ', Ctrl%Fid%AUX
            call Write_Log(Ctrl, trim(message), status)
	 end if
      endif
   endif


!  ALB file is not closed. Leave open for subsequent read of the rest of the
!  image.

end subroutine Read_ALB
