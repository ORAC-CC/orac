!-------------------------------------------------------------------------------
! Name:
!    Read_Location_nc
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
!    2012/08/22, MJ: Uses original routine and implements reading of netcdf
!       data.
!    2014/08/02, GM: Cleaned up the code.
!    2014/08/15, AP: Switching to preprocessor NCDF routines.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_Location_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   use CTRL_def
   use Data_def
   use orac_ncdf

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer,      intent(in)    :: NSegs    ! Number of segments read so far
   integer,      intent(in)    :: SegSize  ! Size of image segment in rows of
                                           ! pixels.
   type(Data_t), intent(inout) :: MSI_Data
   logical,      intent(in)    :: verbose

   integer :: ncid

   ! Open location file
   if (verbose) write(*,*) 'Location file: ', trim(Ctrl%Fid%Loc)
   call nc_open(ncid, Ctrl%Fid%Loc)

   allocate(MSI_Data%Location%Lat(Ctrl%Ind%Xmax, SegSize))
   allocate(MSI_Data%Location%Lon(Ctrl%Ind%Xmax, SegSize))

   call nc_read_array(ncid, "lat", MSI_Data%Location%Lat, verbose)
   call nc_read_array(ncid, "lon", MSI_Data%Location%Lon, verbose)

   ! Close location file
   if (nf90_close(ncid) /= NF90_NOERR) &
        stop 'ERROR: read_location_nc(): Error closing file.'
   

end subroutine Read_Location_nc
