! Name:
!    Read_MSI
!
! Purpose:
!    Controls the reading of Multi Spectral Image (MSI) values from ATSR-type
!    files into the DATA_MSI array.
!
! Arguments:
!   Name     Type          In/Out/Both   Description
!   Ctrl     struct        Both          Control structure (date is read in 
!                                        here).
!   NSegs    int           In            Number of image segments read in by 
!                                        previous calls to this routine.
!   SegSize  int           In            Number of rows of pixels in an image 
!                                        segment.
!   MSI_files_open Logical In            Indicates whether the MSI data file is
!                                        open (if not, open it).
!   lun      int           In/Out        File unit number set by this routine
!                                        when file is opened, passed back and
!                                        forth for subsequent reads.
!   MSI_Data struct        In/Out        Data structure: the MSI data part 
!                                        of this struct is populated by this
!                                        routine, and is overwritten on 
!                                        successive calls.
!   SAD_Chan struct array  Both          Instrument channel parameters. Updated
!                                        by this routine: solar constant is 
!                                        modified from annual average to value 
!                                        for the day of the MSI data.
!   status   int           Out           Error status          
!    
! Algorithm:
!   if (MSI files are not yet open)
!      Find a logical unit number to be used for the MSI file
!      Open MSI file
!      If open error
!         Write error message to screen and log file
!      else
!         allocate MSI image segment array in Data_MSI struct.
!   
!   If (no error opening files)
!      Read header (not used further)
!      If read error
!         Write error message to screen and log file
!      Else
!         Read MSI array of size defined by Ctrl structure
!         If read error
!            Write error message to log file
!         If end of file or end of record (io status < 0)
!            Check current segment no. vs. expected last segment in file
!            if current segment is not last, report error
!   Leave MSI file open for further reads
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
!   ChanIdx   int    Array of channel indices, used to select data from the MSI 
!                    file. Differs from Ctrl%Ind%Y_Id because Y_Id can contain 
!                    repeated channel numbers if more than one view is selected.
!                    ChanIdx is an array index used to locate the channel in the
!                    MSI data. 
!
! History:
!   3rd November, 2000, Kevin M. Smith : original version
!   23rd November, 2000, KMS: Added status to argument list
!   19th December, 2000, KMS: Replaced Data_MSI array with data structure
!   15th January,  2001, KMS: Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
!   27th July 2001, Andy Smith:
!      Updates for handling image segmentation:
!      - new arguments MSI_files_open, lun (since the file now stays open for
!        repeated read operations)
!      - file open depends on MSI_files_open flag
!      Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).
!      Added argument intent specifiers.
!    10th Aug 2001, Andy Smith:
!      Updated to handle image segments/super-pixels of any size. Requires
!      handling of end of file during read on the last segment. On earlier 
!      segments EOF is reported as an error.
!      Added code to read the file header, extract date and convert the 
!      solar constant mean values in SAD_Chan to values for the current month
!      and day. (New argument SAD_Chan). Intent for Ctrl changed to inout.
!    22nd Aug 2001, Andy Smith:
!      Bug fix: added status check before ReadFPArray call.
!    23rd Aug 2001, Andy Smith:
!      Additional argument to ReadFPArray: Ctrl%Ind%NChans, no of instrument 
!      channels available, specifies size of input array of channel data (as
!      opposed to Ctrl%Ind%Ny which specifies the size of the output array).
!   18th May 2011, Andy Smith:
!      Extension to multiple instrument views. Number of "channels" available in 
!      the MSI file is increased by factor of NViews. Modify the NChans argument 
!      passed to ReadFPArray. 
!      Array of selected channel identifiers Y_Id must be modified before passing
!      to ReadFPArray, as it contains repeated Channel IDs if more than one view
!      is selected. Use Y_Id and ViewIdx to populate ChanIdx array. 
!  28th June 2011 Caroline poulsen remove reference to ATSR
!                  changed  Ctrl.Ind.Nchans to  Ctrl.Ind.Ny to remove dependance! on config file
!   25th Nov   2011, Caroline Poulsen add ChI(replace y_id) channel indice variable.!
! 2nd Dec 2011 changed Ninstviews to nviews
!   13th Dec 2011 Caroline poulsen change format statement to make
!                g95 compatible 
! 20/01/2012 C Poulsen remove write statement
! Bugs:
!   None known.
!
! $Id: ReadMSI.f90 182 2011-10-05 10:03:40Z carnold $
!
!------------------------------------------------------------------------------------
Subroutine Read_MSI(Ctrl, NSegs, SegSize, MSI_files_open, lun, MSI_Data, &
   SAD_Chan, status)
   
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
   logical, intent(in)         :: MSI_files_open
   integer, intent(inout)      :: lun       ! Unit number for MSI file   
   type(Data_t), intent(inout) :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(Ctrl%Ind%Ny)
   integer, intent(out)        :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   !MJ ORG character(FilenameLen+100) :: message   ! Error message to pass to Write_Log
   character(800) :: message   ! Error message to pass to Write_Log
   character(24)  :: header    ! Input file header record
   integer        :: day, month ! Day and month numberss extracted from 
                               ! Date string when calculating DOY.
   character(3)   :: mon       ! Month extracted from date in file header.
   integer        :: row       ! Number of final image row read by ReadFPArray
                               ! (in pixels, starting at first row of segment)
   integer        :: i         ! Counter for DOY calculation
   integer        :: ChanIdx(Ctrl%Ind%Ny) ! Array of channel indices within the MSI file


!  On first call, the file is opened. It is then left open for all subsequent
!  calls. 

   status = 0
   if (.not. MSI_files_open) then
!     Call Find_LUN to set suitable unit number for MSI file

      call Find_LUN(lun)

!     Open MSI file

      open(unit=lun, file=Ctrl%Fid%MSI, form='unformatted', status='old', &
           iostat=ios)

      if (ios /= 0) then
	 status = MSIFileOpenErr ! Return error code
	 write(unit=message, fmt=*) &
              'Read_MSI: Error opening file ', trim(adjustl(Ctrl%Fid%MSI))
	 call Write_Log(Ctrl, trim(message), status)
      else
!        Allocate Data%MSI structure size to match image segments to be read in.
         !write(*,*) Ctrl%Ind%Xmax,SegSize, Ctrl%Ind%Ny
         !pause
         allocate(MSI_Data%MSI(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%Ny))

!        Read file header and extract the date: use date to calculate day of
!        year. Date format is DD-MMM-YYYY HH:MM:SS.mmm

         read(unit=lun, iostat=ios) header
         if (ios == 0) then
	    read(header, '(a11,1x,a12)', iostat=ios) Ctrl%Date, Ctrl%Time
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

            do i = 1, Ctrl%Ind%Ny
	       if (SAD_Chan(i)%Solar%Flag > 0) &
        	  SAD_Chan(i)%Solar%F0 = SAD_Chan(i)%Solar%F0 + &
	             (SAD_Chan(i)%Solar%F1 * cos(2 * Pi * Ctrl%DOY / 365.))
	    end do

	 else
            status = MSIFileReadHeadErr ! Return error code
            write(unit=message, fmt=*) &
   	        'Read_MSI: Error reading header of file ', trim(adjustl(Ctrl%Fid%MSI))
            call Write_Log(Ctrl, trim(message), status)
         endif

      end if
   end if

!  Read MSI file 

!  Call Read_FPArray (generic three dimensional float array reading
!  subroutine) to read enough rows of pixels to fill the segment array.

   if (status == 0) then
!     Set the array indices used to select measurements from the MSI file.
      do i = 1, Ctrl%Ind%Ny
!         ChanIdx(i) = Ctrl%Ind%Y_Id(i)+ (Ctrl%Ind%NChans * (Ctrl%Ind%ViewIdx(i)-1))
         ChanIdx(i) =Ctrl%Ind%ChI(i)


      end do


      call Read_FPArray(lun, Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NChans*Ctrl%Ind%NViews, &
         Ctrl%Ind%Ny, ChanIdx, MSI_Data%MSI(:,:,:), row, ios)
write(*,*) 'read msi msi_data',MSI_Data%MSI(1, 1, :)	 
write(*,*) 'read msi msi_data',MSI_Data%MSI(2, 2, :)	 

!     ios > 0 indicates a read error.
!     ios < 0 indicates end of record or end of file. EOF is ok if it's in
!     the right place (in order to allow any super-pixel size, we must
!     deal with EOF part way through reading a segment).

      if (ios > 0) then
	 status = MSIFileReadDataErr ! Return error code
	 write(unit=message, fmt=*) &
	      'Read_MSI: Error reading data in file ', Ctrl%Fid%MSI
	 call Write_Log(Ctrl, trim(message), status)

      else if (ios < 0) then

!     Check for premature end of file during read. Convert the last row read
!     by ReadFPArray from the current segment into an abolute value within the 
!     image and check vs. image size.

	 row = row + (NSegs * SegSize)
	 if (row /= Ctrl%Ind%YMax+1) then
	    status = MSIFileEOFErr
	    write(unit=message, fmt=*) &
	       'Read_MSI: End of file during read: file ', Ctrl%Fid%MSI
            call Write_Log(Ctrl, trim(message), status)
	 end if
      endif
   endif

!  MSI file is not closed. Leave open for subsequent read of the rest of the
!  image.

end subroutine Read_MSI
