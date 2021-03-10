!-------------------------------------------------------------------------------
! Name: read_lsflags.F90
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
! 2000/11/03, KS: Original version
! 2000/11/23, KS: Added status to argument list
! 2001/08/03, AS: Updates for handling image segmentation:
!    - new arguments MSI_files_open, lun (since the file now stays open for
!      repeated read operations)
!    - file open depends on MSI_files_open flag
!    Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!    caused any problems so far but it might).
!    Added argument intent specifiers.
! 2001/08/10, AS: Updated to handle image segments/super-pixels of any size.
!    Requires handling of end of file during read on the last segment. On
!    earlier segments EOF is reported as an error.
! 2001/08/22, AS: Bug fix: added status check before reading arrays in.
!    **************** ECV work starts here *************************************
! 2011/02/23, AS: Replaced call to ReadByteArray with ReadFPArray to cope with
!    current preprocessed data files from ORAC.
! 2011/06/28, CP: Remove reference to ATSR
! 2012/08/22, MJ: Uses original routine and implements reading of netcdf data.
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/15, AP: Switching to preprocessor NCDF routines.
! 2014/10/24, OS: added variables lusflags, dem, and nisemask
! 2015/01/30, AP: Remove NSegs, SegSize arguments. Replace YSeg0 with Y0.
! 2015/07/03, OS: Added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_LSFlags(Ctrl, MSI_Data)

   use Ctrl_m
   use ORAC_Constants_m
   use orac_ncdf_m

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   integer :: ncid

   ! Open LSF file
   if (Ctrl%verbose) write(*,*) 'Land/sea flag file: ', trim(Ctrl%FID%LS)
   call ncdf_open(ncid, Ctrl%FID%LS, 'Read_LSFlags()')

   allocate(MSI_Data%LSFlags(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%lusflags(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%dem(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%nisemask(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))

   call ncdf_read_array(ncid, "lsflag", MSI_Data%LSFlags)
   call ncdf_read_array(ncid, "lusflag", MSI_Data%lusflags)
   call ncdf_read_array(ncid, "dem", MSI_Data%dem)
   call ncdf_read_array(ncid, "nisemask", MSI_Data%nisemask)

   ! Close LSF file
   call ncdf_close(ncid, 'read_lsflags()')

end subroutine Read_LSFlags
