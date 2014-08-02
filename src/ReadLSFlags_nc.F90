!-------------------------------------------------------------------------------
! Name:
!    Read_LSFlags_nc
!
! Purpose:
!    Controls the reading of cloud flags values from ATSR-type files into the
!    Data_CloudFlags array.
!
! Arguments:
!    Name     Type   In/Out/Both Description
!    Ctrl     struct Both        Control structure
!    NSegs    int    In          Number of image segments read in by previous
!                                calls to this routine.
!    SegSize  int    In          Number of rows of pixels in an image segment.
!    MSI_Data struct Both        Data structure: contains the cloud flag array
!                                to be populated with data from the file. This
!                                is overwritten as successive segments of data
!                                are read in.
!    status   int    Out         Error status
!
! Algorithm:
!    if (MSI files are not yet open)
!       Find a logical unit number to be used for the cloud flag file
!       Open cloud flag file
!       If open error
!          Write error message to screen and log file
!       else
!         allocate MSI image segment array in Data_MSI struct.
!
!    If (no error opening files)
!        Read header (not used further)
!        If read error
!           Write error message to screen and log file
!        Else
!           Read byte array of size defined by Ctrl structure
!           If read error
!              Write error message to log file
!    Leave cloud flag file open for later reads
!
! Local variables:
!    Name Type Description
!
! History:
!     3rd Nov 2000, Kevin M. Smith: Original version
!    23rd Nov 2000, Kevin M. Smith:
!       Added status to argument list
!     3rd Aug 2001, Andy Smith:
!       Updates for handling image segmentation:
!       - new arguments MSI_files_open, lun (since the file now stays open for
!         repeated read operations)
!       - file open depends on MSI_files_open flag
!       Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!       caused any problems so far but it might).
!       Added argument intent specifiers.
!    10th Aug 2001, Andy Smith:
!       Updated to handle image segments/super-pixels of any size. Requires
!       handling of end of file during read on the last segment. On earlier
!       segments EOF is reported as an error.
!    22nd Aug 2001, Andy Smith:
!       Bug fix: added status check before reading arrays in.
!    **************** ECV work starts here *************************************
!    23rd Feb 2011, Andy Smith:
!       Replaced call to ReadByteArray with ReadFPArray to cope with current
!       preprocessed data files from ORAC.
!    28th Jun 2011, Caroline Poulsen:
!       Remove reference to ATSR
!    22nd Aug 2012, Matthias Jerg:
!       Uses original routine and implements reading of netcdf data.
!     2nd Aug 2014, Greg McGarragh:
!       Cleaned up the code.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_LSFlags_nc(Ctrl, NSegs, SegSize, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants

   use netcdf

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer,      intent(in)    :: NSegs    ! Number of segments read so far
   integer,      intent(in)    :: SegSize  ! Size of image segment in rows of
                                           ! pixels.
   type(Data_t), intent(inout) :: MSI_Data
   integer,      intent(out)   :: status

   ! Local variables

   integer        :: ios     ! I/O status from file operations
   character(256) :: message ! Error message to pass to Write_Log

   ! netcdf related
   integer :: ncid

   status = 0

   ! Open LSF file
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%LS)),mode = nf90_nowrite,ncid = ncid)

   if (ios /= 0) then
      status = LsFileOpenErr ! Return error code
      write(unit=message, fmt=*) 'Read_LandSeaFlags: Error opening file ', &
         Ctrl%Fid%LS
      call Write_Log(Ctrl, trim(message), status)
   else
      allocate(MSI_Data%LSFlags(Ctrl%Ind%Xmax, SegSize))
   end if

   if (status == 0) then
      call nc_read_array_2d_byte_to_byte_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize, &
         "lsflag", MSI_Data%LSFlags,0)
   endif

   ! Close LSF file
   ios = nf90_close(ncid)
   if (ios /= 0) then
      status = LsFileCloseErr ! Return error code
      write(unit=message, fmt=*) 'Read_LSflag: Error closing file ', &
         trim(adjustl(Ctrl%Fid%LS))
      call Write_Log(Ctrl, trim(message), status)
   endif

end subroutine Read_LSFlags_nc
