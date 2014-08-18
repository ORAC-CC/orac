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
!    28th Jun 2011, Caroline Poulsen: Remove reference to ATSR
!    13th Dec 2011, Caroline Poulsen: change format statement to make g95
!       compatible
!    15/09/2012, CP: Initialise array
!    15/09/2012, CP: Changed to read from netcdf files
!    2013/xx/xx, MJ: Fixes bug with close of netcdf file.
!    2013/11/18, MJ: Cleans and debugs:Loop to read albedo is indexed with
!       ysolar as ysolar halds the channel indices as they are stored in the
!       preprocessing file.
!    2014/04/20, GM: Cleaned up the code.
!    2014/05/28, GM: Removed unused read of attribute 'Product_Date'.
!    2014/08/15, AP: Switching to preprocessor NCDF routines.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_ALB_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use orac_ncdf
   use SAD_Chan_def

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer,      intent(in)    :: NSegs     ! Number of segments read so far
   integer,      intent(in)    :: SegSize   ! Size of image segment in rows of
                                            ! pixels.
   type(Data_t), intent(inout) :: MSI_Data
   logical,      intent(in)    :: verbose

   integer :: ncid
!   integer(kind=lint), allocatable, dimension(:) :: alb_instr_ch_numbers

   ! Open ALB file
   if (verbose) write(*,*) 'Albedo file: ', trim(Ctrl%Fid%Aux)
   call nc_open(ncid, Ctrl%Fid%Aux)

   ! Allocate Data%ALB structure
   allocate(MSI_Data%ALB(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NSolar))

      ! Read instrument channel indices from file
!   allocate(alb_instr_ch_numbers(Ctrl%Ind%NSolar))
!   call nc_read_array(ncid, "alb_abs_ch_numbers", alb_instr_ch_numbers, &
!        verbose)

   ! read solar channels from albedo field
   call nc_read_array(ncid, "alb_data", MSI_Data%ALB, verbose, &
        3, Ctrl%Ind%ysolar)
   
   if (verbose) write(*,*) 'Max/Min Alb: ', maxval(MSI_Data%ALB), &
        minval(MSI_Data%ALB)

!   deallocate(alb_instr_ch_numbers)

   ! Close alb input file
   if (nf90_close(ncid) /= NF90_NOERR) &
        stop 'ERROR: read_alb_nc(): Error closing file.'

     
end subroutine Read_ALB_nc
