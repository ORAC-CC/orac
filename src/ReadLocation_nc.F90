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
!
! History:
! 2012/08/22 MJ uses original routine and implements reading of netcdf data.
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Read_Location_nc(Ctrl, NSegs, SegSize, &
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

  !netcdf related
  integer :: ncid

   status = 0

   !     Open geometry file
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%Loc)),mode = nf90_nowrite,ncid = ncid) 
   
   if (ios /= 0) then
      status = LocFileOpenErr ! Return error code
      write(unit=message, fmt=*) 'Read_Location: Error opening file ', &
           & Ctrl%Fid%Loc
      call Write_Log(Ctrl, trim(message), status)
   else
      !        Open successful. Allocate MSI_Data%CloudFlags array size
      
      !write(*,*) Ctrl%Ind%NViews
      
      allocate(MSI_Data%Location%Lat(Ctrl%Ind%Xmax, SegSize))
      allocate(MSI_Data%Location%Lon(Ctrl%Ind%Xmax, SegSize))
   end if

   if (status == 0) then
      call nc_read_array_2d_float_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,&
           & "lat",MSI_Data%Location%Lat,0)
      call nc_read_array_2d_float_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,&
           & "lon",MSI_Data%Location%Lon,0)
   endif
   
   !close  cflag input file
   ios=nf90_close(ncid)
   if (ios /= 0) then
      status = LocFileCloseErr ! Return error code
      write(unit=message, fmt=*) &
           & 'Read_Location: Error closing file ', trim(adjustl(Ctrl%Fid%Loc))
      call Write_Log(Ctrl, trim(message), status)
   endif
 end subroutine Read_Location_nc
