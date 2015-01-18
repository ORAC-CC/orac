!-------------------------------------------------------------------------------
! Name:
!    Read_Geometry_nc
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
!
! Algorithm:
!    if (MSI files are not yet open)
!       Find a logical unit number to be used for the cloud flag file
!       Open cloud flag file
!       If open error
!          Write error message to screen and log file
!       Else
!         Allocate MSI image segment array in Data_MSI struct.
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
!    2013/03/12, CP: Changed definition of relative azimuth angle
!    2013/05/21, GT: Undid previous change made by CP. The error was in th L1B
!       reading code in the preprocessing.
!    2014/08/02, GM: Cleaned up the code.
!    2014/08/15, AP: Switching to preprocessor NCDF routines.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_Geometry_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   use CTRL_def
   use ECP_Constants
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

   ! Open geometry file
   if (verbose) write(*,*) 'Geometry file: ', trim(Ctrl%Fid%Geo)
   call nc_open(ncid, Ctrl%Fid%Geo)

   allocate(MSI_Data%Geometry%Sol(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
   allocate(MSI_Data%Geometry%Sat(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
   allocate(MSI_Data%Geometry%Azi(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))

   call nc_read_array(ncid,"solzen",MSI_Data%Geometry%Sol,verbose)
   call nc_read_array(ncid,"satzen",MSI_Data%Geometry%Sat,verbose)
   call nc_read_array(ncid,"relazi",MSI_Data%Geometry%Azi,verbose)

   ! Close geometry file
   if (nf90_close(ncid) /= NF90_NOERR) then
      write(*,*) 'ERROR: read_geometry_nc(): Error closing file.'
      stop error_stop_code
   end if

end subroutine Read_Geometry_nc
