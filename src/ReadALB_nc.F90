!-------------------------------------------------------------------------------
! Name:
!    Read_ALB_nc
!
! Purpose:
!    Controls the reading of Multi Spectral Image (ALB) values from ATSR-type
!    files into the DATA_ALB array.
!
! Arguments:
!   Name     Type          In/Out/Both Description
!   Ctrl     struct        Both        Control structure (date is read in
!                                      here).
!   NSegs    int           In          Number of image segments read in by
!                                      previous calls to this routine.
!   SegSize  int           In          Number of rows of pixels in an image
!                                      segment.
!   ALB_files_open Logical In          Indicates whether the ALB data file is
!                                      open (if not, open it).
!   lun      int           Both        File unit number set by this routine
!                                      when file is opened, passed back and
!                                      forth for subsequent reads.
!   MSI_Data struct        Both        Data structure: the ALB data part
!                                      of this struct is populated by this
!                                      routine, and is overwritten on
!                                      successive calls.
!   SAD_Chan struct array  Both        Instrument channel parameters. Updated
!                                      by this routine: solar constant is
!                                      modified from annual average to value
!                                      for the day of the ALB data.
!   status   int           Out         Error status
!
! Algorithm:
!    If (ALB files are not yet open)
!       Find a logical unit number to be used for the ALB file
!       Open ALB file
!       If open error
!          Write error message to screen and log file
!       Else
!          Allocate ALB image segment array in Data_ALB struct.
!
!    If (no error opening files)
!       Read header (not used further)
!       If read error
!          Write error message to screen and log file
!       Else
!          Read ALB array of size defined by Ctrl structure
!          If read error
!             Write error message to log file
!          If end of file or end of record (io status < 0)
!             Check current segment no. vs. expected last segment in file
!             If current segment is not last, report error
!    Leave ALB file open for further reads
!
! Local variables:
!    Name Type Description
!
! History:
!    29th May 2002, Caroline Poulsen: Original version copied from READ_MSI.
!    29th Oct 2002, Caroline Poulsen: Fixed bug too many arguments in the header
!       removed sad_chan.
!    28th June 2011, Caroline Poulsen: Remove reference to ATSR
!    13th Dec 2011 Caroline poulsen change format statement to make g95
!       compatible
!    15/09/2012, CP: Initialise array
!    15/09/2012, CP: Changed to read from netcdf files
!    2013/xx/xx, MJ: Fixes bug with close of netcdf file.
!    2013/11/18, MJ: Cleans and debugs:Loop to read albedo is indexed with
!       ysolar as ysolar halds the channel indices as they are stored in the
!       preprocessing file.
!    2014/04/20, GM: Cleaned up the code.
!    2014/05/28, GM: Removed unused read of attribute 'Product_Date'.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_ALB_nc(Ctrl, NSegs, SegSize, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use SAD_Chan_def

   use netcdf

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer,      intent(in)    :: NSegs     ! Number of segments read so far
   integer,      intent(in)    :: SegSize   ! Size of image segment in rows of
                                            ! pixels.
   type(Data_t), intent(inout) :: MSI_Data
   integer,      intent(out)   :: status

   ! Local variables

   integer        :: ios     ! I/O status from file operations
   character(180) :: message ! Error message to pass to Write_Log

   ! NetCDF related
   integer        :: i
   integer        :: ncid
   integer(kind=nint), allocatable, dimension(:) :: alb_instr_ch_numbers

   ! On first call, the file is opened. It is then left open for all subsequent
   ! calls.

   status = 0

   ! Open ALB file
   write(*,*)'alb file: ', Ctrl%Fid%Aux
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%Aux)),mode = nf90_nowrite,ncid = ncid)
   if (ios /= 0) then
      status = ALBFileOpenErr ! Return error code
      write(unit=message, fmt=*) &
           'Read_ALB: Error opening file ', trim(adjustl(Ctrl%Fid%AUX))
      call Write_Log(Ctrl, trim(message), status)
   else
      ! Allocate Data%ALB structure
      allocate(MSI_Data%ALB(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NSolar))

      ! Read instrument channel indices from file
      allocate(alb_instr_ch_numbers(Ctrl%Ind%NSolar))
      alb_instr_ch_numbers = 0
      call nc_read_array_1d_int_to_int_orac(ncid,Ctrl%Ind%NSolar, &
         "alb_abs_ch_numbers",alb_instr_ch_numbers,0)

      ! Loop over channels and read if desired channel number is hit
      do i=1,Ctrl%Ind%NSolar
         write(*,*) 'Ctrl%Ind%ysolar(i): ',Ctrl%Ind%ysolar(i)
         call nc_read_array_3d_float_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,&
            Ctrl%Ind%ysolar(i), "alb_data",MSI_Data%ALB(:,:,i),0)
         write(*,*) 'Max/Min Alb: ',maxval(MSI_Data%ALB(:,:, i)),minval(MSI_Data%ALB(:,:, i))
      enddo

      deallocate(alb_instr_ch_numbers)
     end if

     ! Close alb input file
     ios=nf90_close(ncid)
     write(*,*)'Reading Albedo done, status, ios: ',status,ios

end subroutine Read_ALB_nc
