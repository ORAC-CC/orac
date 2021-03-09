!-------------------------------------------------------------------------------
! Name: read_location.F90
!
! Purpose:
! Controls the reading of cloud flags values from ATSR-type files into the
! Data_CloudFlags array.
!
! Description and Algorithm details:
! if (MSI files are not yet open)
!    Find a logical unit number to be used for the cloud flag file
!    Open cloud flag file
!    If open error
!       Write error message to screen and log file
!    else
!      allocate MSI image segment array in Data_MSI struct.
!
! If (no error opening files)
!     Read header (not used further)
!     If read error
!        Write error message to screen and log file
!     Else
!        Read byte array of size defined by Ctrl structure
!        If read error
!           Write error message to log file
! Leave cloud flag file open for later reads
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct Both        Control structure
! MSI_Data struct Both        Data structure: contains the cloud flag array
!                             to be populated with data from the file. This
!                             is overwritten as successive segments of data
!                             are read in.
!
! History:
! 2012/08/22, MJ: Uses original routine and implements reading of netcdf data.
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/15, AP: Switching to preprocessor NCDF routines.
! 2014/01/30, AP: Remove NSegs, SegSize arguments.
! 2015/07/03, OS: Added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_Location(Ctrl, MSI_Data)

   use Ctrl_m
   use orac_ncdf_m

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   integer :: ncid

   ! Open location file
   if (Ctrl%verbose) write(*,*) 'Location file: ', trim(Ctrl%FID%Loc)
   call ncdf_open(ncid, Ctrl%FID%Loc, 'Read_Location()')

   allocate(MSI_Data%Location%Lat(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%Location%Lon(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))

   call ncdf_read_array(ncid, "lat", MSI_Data%Location%Lat, Ctrl%verbose)
   call ncdf_read_array(ncid, "lon", MSI_Data%Location%Lon, Ctrl%verbose)

   ! Close location file
   call ncdf_close(ncid, 'read_location()')

end subroutine Read_Location
