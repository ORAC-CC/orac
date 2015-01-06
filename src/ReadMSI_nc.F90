!-------------------------------------------------------------------------------
! Name:
!    Read_MSI_nc
!
! Purpose:
!    Controls the reading of Multi Spectral Image (MSI) values from ATSR-type
!    files into the DATA_MSI array.
!
! Arguments:
!    Name     Type          In/Out/Both Description
!    Ctrl     struct        Both        Control structure (date is read in
!                                       here).
!    NSegs    int           In          Number of image segments read in by
!                                       previous calls to this routine.
!    SegSize  int           In          Number of rows of pixels in an image
!                                       segment.
!    MSI_Data struct        Both        Data structure: the MSI data part of
!                                       this struct is populated by this
!                                       routine, and is overwritten on
!                                       successive calls.
!    SAD_Chan struct array  Both        Instrument channel parameters. Updated
!                                       by this routine: solar constant is
!                                       modified from annual average to value
!                                       for the day of the MSI data.
!
! Algorithm:
!    Ff (MSI files are not yet open)
!       Find a logical unit number to be used for the MSI file
!       Open MSI file
!       If open error
!          Write error message to screen and log file
!       else
!          allocate MSI image segment array in Data_MSI struct.
!
!    If (no error opening files)
!       Read header (not used further)
!       If read error
!          Write error message to screen and log file
!       Else
!          Read MSI array of size defined by Ctrl structure
!          If read error
!             Write error message to log file
!    Leave MSI file open for further reads
!
! Local variables:
!    Name Type Description
!
! History:
!     3rd Nov 2000, Kevin M. Smith:
!        Original version
!    23rd Nov 2000, Kevin M. Smith:
!        Added status to argument list
!    19th Dec 2000, Kevin M. Smith:
!        Replaced Data_MSI array with data structure
!    15th Jan 2001, Kevin M. Smith:
!        Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
!    27th Jul 2001, Andy Smith:
!       Updates for handling image segmentation:
!        - new arguments MSI_files_open, lun (since the file now stays open for
!          repeated read operations)
!        - file open depends on MSI_files_open flag
!       Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!       caused any problems so far but it might).
!       Added argument intent specifiers.
!    10th Aug 2001, Andy Smith:
!       Updated to handle image segments/super-pixels of any size. Requires
!       handling of end of file during read on the last segment. On earlier
!       segments EOF is reported as an error.
!       Added code to read the file header, extract date and convert the
!       solar constant mean values in SAD_Chan to values for the current month
!       and day. (New argument SAD_Chan). Intent for Ctrl changed to inout.
!    22nd Aug 2001, Andy Smith:
!       Bug fix: added status check before ReadFPArray call.
!    23rd Aug 2001, Andy Smith:
!       Additional argument to ReadFPArray: Ctrl%Ind%NChans, no of instrument
!       channels available, specifies size of input array of channel data (as
!       opposed to Ctrl%Ind%Ny which specifies the size of the output array).
!    18th May 2011, Andy Smith:
!       Extension to multiple instrument views. Number of "channels" available
!       in the MSI file is increased by factor of NViews. Modify the NChans
!       argument
!       passed to ReadFPArray.
!       Array of selected channel identifiers Y_Id must be modified before
!       passing to ReadFPArray, as it contains repeated Channel IDs if more than
!       one view is selected. Use Y_Id and ViewIdx to populate ChanIdx array.
!    28th Jun 2011, Caroline Poulsen:
!       Remove reference to ATSR
!       Changed  Ctrl.Ind.Nchans to  Ctrl.Ind.Ny to remove dependence! on config
!       file
!    25th Nov 2011, Caroline Poulsen:
!       Add ChI(replace y_id) channel indice variable.
!     2nd Dec 2011, Caroline Poulsen:
!       Changed Ninstviews to nviews
!    13th Dec 2011, Caroline Poulsen:
!       Change format statement to make g95 compatible
!    20/01/2012, CP: Remove write statement
!    2012/08/21, MJ: Uses original routine and implements reading of netcdf data.
!    2012/09/21, CP: Added channel index to y_id value
!    2012/11/03, MST and MJ: Hard code in values for avhrr
!    2013/11/18, MJ: Cleans and debugs
!    2014/01/31, MJ: Removed hardcoded parts for avhrr (obsolete)
!    2014/04/18, GM: Cleaned up the code.
!    2014/04/30, GM: Fixed a bug introduced by a previous change.
!    2014/08/15, AP: Switching to preprocessor NCDF routines.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_MSI_nc(Ctrl, NSegs, SegSize, MSI_Data, SAD_Chan, verbose)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use orac_ncdf
   use SAD_Chan_def

   implicit none

   ! Argument declarations

   type(CTRL_t),     intent(inout) :: Ctrl
   integer,          intent(in)    :: NSegs    ! Number of segments read so far
   integer,          intent(inout) :: SegSize  ! Size of image segment in rows of
                                               ! pixels.
   type(Data_t),     intent(inout) :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(Ctrl%Ind%Ny)
   logical,          intent(in)    :: verbose

   ! Local variables

   integer :: i          ! Counter for DOY calculation
   integer :: day, month ! Day and month numbers extracted from

   ! NetCDF related
   integer           :: ncid
   character(len=12) :: prod_date
!   integer(kind=lint), allocatable, dimension(:) :: msi_instr_ch_numbers

   ! Open MSI file
   if (verbose) write(*,*) 'Imagery file: ', trim(Ctrl%Fid%MSI)
   call nc_open(ncid, Ctrl%Fid%MSI)

   ! Read product date and time from netcdf global attributes
   if (nf90_get_att(ncid, NF90_GLOBAL, "Product_Date", prod_date) == &
        NF90_NOERR) then
      Ctrl%Date=trim(adjustl(prod_date(1:8)))
      Ctrl%Time=trim(adjustl(prod_date(9:12)))

      ! Get day and month as integers
      read(prod_date(7:8), '(I2)') day
      read(prod_date(5:6), '(I2)') month

      ! Compute DOY for present day
      Ctrl%DOY = 0
      do i=1,month-1
         Ctrl%DOY = Ctrl%DOY + days_in_month(i)
      end do
      Ctrl%DOY = Ctrl%DOY + day

      ! Calculate solar constant based on day of year using the mean and
      ! amplitude of variation.
      do i = 1, Ctrl%Ind%Ny
         if (SAD_Chan(i)%Solar%Flag > 0) &
              SAD_Chan(i)%Solar%F0 = SAD_Chan(i)%Solar%F0 + &
              (SAD_Chan(i)%Solar%F1 * cos(2 * Pi * Ctrl%DOY / 365.))
      end do
   end if

   ! Read MSI file

   ! Allocate Data%MSI structure size to match image segments to be used.
   allocate(MSI_Data%MSI(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%Ny))
   allocate(MSI_Data%time(Ctrl%Ind%Xmax, SegSize))

   call nc_read_array(ncid, "msi_data", MSI_Data%MSI, verbose, 3, Ctrl%Ind%ICh)
   call nc_read_array(ncid, "time_data", MSI_Data%time, verbose)

   ! Read instrument channel indices from file
!  allocate(msi_instr_ch_numbers(Ctrl%Ind%Navail))
!  call nc_read_array(ncid,"msi_instr_ch_numbers",msi_instr_ch_numbers,0)
!  deallocate(msi_instr_ch_numbers)

   ! Close MSI input file
   if (nf90_close(ncid) /= NF90_NOERR) then
      write(*,*) 'ERROR: read_msi_nc(): Error closing file.'
      stop error_stop_code
   end if

end subroutine Read_MSI_nc
