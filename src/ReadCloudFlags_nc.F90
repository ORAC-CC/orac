!-------------------------------------------------------------------------------
! Name:
!   Read_CloudFlags_nc
!
! Purpose:
!    Controls the reading of cloud flags values from ATSR-type files into the
!    Data_CloudFlags array.
!
! Arguments:
!    Name     Type   In/Out/Both Description
!    Ctrl     struct Both        Control structure
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
!    28th June 2011, Caroline Poulsen:
!       Remove reference to ATSR.
!    22nd Aug 2012, Matthias Jerg:
!       Uses original routine and implements reading of netcdf data.
!    15th Sep 2012, Caroline Poulsen:
!       Initialise MSI_Data%CloudFlags.
!     2nd Aug 2014, Greg McGarragh:
!       Cleaned up the code.
!    15th Aug 2014, Adam Povey:
!       Switching to preprocessor NCDF routines.
!    24th Oct 2014, Oliver Sus:
!       Added new variables CldType, CloudMask, and CCCOT_pre
!    30th Jan 2015, Adam Povey:
!       Remove NSegs, SegSize arguments.
!     4th Feb 2015, Martin Stengel & Oliver Sus:
!       Process only cloudy pixels and one phase; flags to be outsourced; 
!       hard-wired cloud type values to be replaced by Pavolonis constants
!     3rd Jul 2015, OS:
!       Added cldmask_uncertainty
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_CloudFlags_nc(Ctrl, MSI_Data, verbose)

   use CTRL_def
   use ECP_Constants
   use orac_ncdf

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data
   logical,      intent(in)    :: verbose

   integer :: ncid, ierr

   ! Open cloud flag file
   if (verbose) write(*,*) 'Cloud flag file: ', trim(Ctrl%Fid%Cf)
   call nc_open(ncid, Ctrl%Fid%CF, ierr)

   allocate(MSI_Data%CloudFlags(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%cldtype(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%cloudmask(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%cloudmask_error(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%cccot_pre(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))

   call nc_read_array(ncid, "cflag", MSI_Data%CloudFlags, verbose)
   call nc_read_array(ncid, "cldtype", MSI_Data%cldtype, verbose)
   call nc_read_array(ncid, "cldmask", MSI_Data%cloudmask, verbose)
   call nc_read_array(ncid, "cldmask_uncertainty", MSI_Data%cloudmask_error, verbose)
   call nc_read_array(ncid, "cccot_pre", MSI_Data%cccot_pre, verbose)

   ! Pavolonis cloud type values:

   ! CLEAR_TYPE = 0
   ! PROB_CLEAR_TYPE = 1 !currently not used
   ! FOG_TYPE = 2
   ! WATER_TYPE = 3
   ! SUPERCOOLED_TYPE = 4
   ! OPAQUE_ICE_TYPE = 6
   ! CIRRUS_TYPE = 7
   ! OVERLAP_TYPE = 8

   if( Ctrl%process_cloudy_only ) then
   ! set clear-sky pixels to 0 to avoid their processing
      where( MSI_Data%CloudMask .eq. 0 )
         MSI_Data%CloudFlags = 0
      endwhere
      if( Ctrl%process_one_phase_only ) then
      ! set ice pixels to 0
         if( trim( Ctrl%CloudClass ) .eq. 'WAT' ) then
            where( MSI_Data%CldType .ge. 6 .and. MSI_Data%CldType .le. 8 )
               MSI_Data%CloudFlags = 0
            endwhere
         endif
      ! set liquid pixels to 0
         if( trim( Ctrl%CloudClass ) .eq. 'ICE' ) then
            where( MSI_Data%CldType .ge. 2 .and. MSI_Data%CldType .le. 4 )
               MSI_Data%CloudFlags = 0
            endwhere
         endif
      endif
   endif

   ! Close cloud flag file
   if (nf90_close(ncid) /= NF90_NOERR) then
      write(*,*) 'ERROR: read_cloudflags_nc(): Error closing file.'
      stop error_stop_code
   end if

end subroutine Read_CloudFlags_nc
