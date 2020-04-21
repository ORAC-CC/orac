!-------------------------------------------------------------------------------
! Name: ReadCloudFlags_nc.F90
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
!    Else
!      Allocate MSI image segment array in Data_MSI struct.
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
!    caused any problems so far but it might). Added argument intent specifiers.
! 2001/08/10, AS: Updated to handle image segments/super-pixels of any size.
!    Requires handling of end of file during read on the last segment. On
!    earlier segments EOF is reported as an error.
! 2001/08/22, AS: Bug fix: added status check before reading arrays in.
!    **************** ECV work starts here *************************************
! 2011/02/23, AS: Replaced call to ReadByteArray with ReadFPArray to cope with
!    current preprocessed data files from ORAC.
! 2011/06/28, CP: Remove reference to ATSR.
! 2012/08/22, MJ: Uses original routine and implements reading of netcdf data.
! 2012/09/15, CP: Initialise MSI_Data%CloudFlags.
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/15, AP: Switching to preprocessor NCDF routines.
! 2014/10/24, OS: Added new variables CldType, CloudMask, and CCCOT_pre
! 2015/01/30, AP: Remove NSegs, SegSize arguments.
! 2015/02/04, MS&OS: Process only cloudy pixels and one phase; flags to be
!    outsourced; hard-wired cloud type values to be replaced by Pavolonis
!    constants
! 2015/07/03, OS: Added cldmask_uncertainty
! 2015/07/10, OS: removed ierr argument to nc_open call
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2016/05/31, GT: Added Ctrl%process_aerosol_only code
! 2016/07/15, GT: Ctrl%process_aerosol_only now checks cloud mask in all views.
! 2017/06/21, OS: Added ANN phase variables.
! 2017/10/04, GM: Add the option to use ANN phase to select pixels to process.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_CloudFlags_nc(Ctrl, MSI_Data)

   use Ctrl_m
   use ORAC_Constants_m
   use orac_ncdf_m

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   integer :: ncid
   integer :: i

   ! Open cloud flag file
   if (Ctrl%verbose) write(*,*) 'Cloud flag file: ', trim(Ctrl%FID%Cf)
   call ncdf_open(ncid, Ctrl%FID%CF, 'Read_CloudFlags_nc()')

   allocate(MSI_Data%Type(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%cldtype(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NViews))
   allocate(MSI_Data%cldmask(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NViews))
   allocate(MSI_Data%cldmask_uncertainty(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, &
        Ctrl%Ind%NViews))
   allocate(MSI_Data%cccot_pre(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NViews))
   allocate(MSI_Data%ann_phase(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NViews))
   allocate(MSI_Data%ann_phase_uncertainty(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, &
        Ctrl%Ind%NViews))
   allocate(MSI_Data%cphcot(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NViews))

!  call ncdf_read_array(ncid, "cflag", MSI_Data%CloudFlags, Ctrl%verbose)
   call ncdf_read_array(ncid, "cldtype", MSI_Data%cldtype, Ctrl%verbose)
   call ncdf_read_array(ncid, "cldmask", MSI_Data%cldmask, Ctrl%verbose)
   call ncdf_read_array(ncid, "cldmask_uncertainty", &
        MSI_Data%cldmask_uncertainty, Ctrl%verbose)
   call ncdf_read_array(ncid, "cccot_pre", MSI_Data%cccot_pre, Ctrl%verbose)
   call ncdf_read_array(ncid, "ann_phase", MSI_Data%ann_phase, Ctrl%verbose)
   call ncdf_read_array(ncid, "ann_phase_uncertainty", &
        MSI_Data%ann_phase_uncertainty, Ctrl%verbose)
   call ncdf_read_array(ncid, "cphcot", MSI_Data%cphcot, Ctrl%verbose)

   ! Merge various particle type flags (once aerosol is in)
   if (Ctrl%use_ann_phase) then
      MSI_Data%Type = MSI_Data%ann_phase(:,:,1) ! Nadir
   else
      MSI_Data%Type = MSI_Data%cldtype(:,:,1) ! Nadir
   end if

   if (Ctrl%process_cloudy_only) then
      ! Invalidate clear-sky pixels to 0 to avoid their processing
      where (MSI_Data%cldmask(:,:,1) .eq. 0)
         MSI_Data%Type = byte_fill_value
      end where
   end if

   if (Ctrl%process_aerosol_only) then
      ! Invalidate cloudy pixels to 0 ot avoid their processing
      do i=1,Ctrl%Ind%NViews
         where (MSI_Data%cldmask(:,:,i) .ne. 0)
            MSI_Data%Type = byte_fill_value
         end where
      end do
   end if

   ! Close cloud flag file
   call ncdf_close(ncid, 'read_cloudflags_nc()')

end subroutine Read_CloudFlags_nc
