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
! 2012/08/22 MJ uses original routine and implements reading of netcdf data.
! Bugs:
!   None known.
!
! $Id: ReadCloudFlags.f90 182 2011-10-05 10:03:40Z carnold $
!!
!-------------------------------------------------------------------------------
subroutine Read_Scanlines_nc(Ctrl, NSegs, SegSize, &
   MSI_Data, status)

   use CTRL_def
   use ECP_Constants
   use Data_def

   use netcdf

   implicit none

!  Argument declarations

   type(CTRL_t), intent(in)      :: Ctrl
   integer, intent(in)           :: NSegs     ! Number of segments read so far
   integer, intent(in)           :: SegSize   ! Size of image segment in rows of
                                              ! pixels.
   type(Data_t), intent(inout)   :: MSI_Data
   integer, intent(out)          :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
   integer        :: row       ! Number of final image row read by ReadByteArray
                               ! (in pixels, starting at first row of segment)

  !netcdf related
  integer :: ncid

   status = 0

   !     Open cloud flag file
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%uv)),mode = nf90_nowrite,ncid = ncid) 
   
   if (ios /= 0) then
      status = scanFileOpenErr ! Return error code
      write(unit=message, fmt=*) 'Read_Scanlines: Error opening file ', &
           & Ctrl%Fid%uv
      call Write_Log(Ctrl, trim(message), status)
   else
      !        Open successful. Allocate MSI_Data%scan%uscan and vscan array sizes
      allocate(MSI_Data%Scan%uscan(Ctrl%Ind%Xmax, SegSize))
      allocate(MSI_Data%Scan%vscan(Ctrl%Ind%Xmax, SegSize))
      
   end if

   if (status == 0) then
      call nc_read_array_2d_int_to_real_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,"uscan",&
           & MSI_Data%Scan%uscan,0)
      call nc_read_array_2d_int_to_real_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,"vscan",&
           & MSI_Data%Scan%vscan,0)
   endif

  !close  cflag input file
  ios=nf90_close(ncid)
  if (ios /= 0) then
     status = ScanFileCloseErr ! Return error code
     write(unit=message, fmt=*) &
          & 'Read_LSflag: Error closing file ', trim(adjustl(Ctrl%Fid%uv))
     call Write_Log(Ctrl, trim(message), status)
  endif
end subroutine Read_Scanlines_nc
