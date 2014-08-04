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
!    status   int           Out         Error status
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
!          If end of file or end of record (io status < 0)
!             Check current segment no. vs. expected last segment in file
!             if current segment is not last, report error
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
!    28th Jun 2011, Caroline poulsen:
!       Remove reference to ATSR
!       Changed  Ctrl.Ind.Nchans to  Ctrl.Ind.Ny to remove dependance! on config
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
!
! Bugs:
!    None known.
!
! $Id$
!
!------------------------------------------------------------------------------------

subroutine Read_MSI_nc(Ctrl, NSegs, SegSize, MSI_Data, SAD_Chan, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use SAD_Chan_def

   use netcdf

   implicit none

   ! Argument declarations

   type(CTRL_t),     intent(inout) :: Ctrl
   integer,          intent(in)    :: NSegs    ! Number of segments read so far
   integer,          intent(inout) :: SegSize  ! Size of image segment in rows of
                                               ! pixels.
   type(Data_t),     intent(inout) :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(Ctrl%Ind%Ny)
   integer, intent(out)            :: status

   ! Local variables

   integer         :: ios        ! I/O status from file operations
   character(2048) :: message    ! Error message to pass to Write_Log
   integer         :: day, month ! Day and month numberss extracted from
   integer         :: i          ! Counter for DOY calculation

   ! NetCDF related
   integer            :: ncid
   character(len=12)  :: prod_date
   integer(kind=nint), allocatable, dimension(:) :: msi_instr_ch_numbers

   ! On first call, the file is opened. It is then left open for all subsequent
   ! calls.

   status = 0

   ! Open MSI file
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%MSI)),mode = nf90_nowrite,ncid = ncid)

   if (ios /= NF90_NOERR) then
      status = MSIFileOpenErr
      write(unit=message, fmt=*) &
        'Read_MSI: Error opening file ', trim(adjustl(Ctrl%Fid%MSI))
      write(*,*) trim(message)
      call Write_Log(Ctrl, trim(message), status)
      stop
   else
      ! Read product date and time from netcdf global attributes
      ios=nf90_get_att(ncid, NF90_GLOBAL, "Product_Date", prod_date)

      if (ios == 0) then
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

      else
         status = MSIFileReadHeadErr
         write(unit=message, fmt=*) &
              'Read_MSI: Error reading header of file ', trim(adjustl(Ctrl%Fid%MSI))
         write(*,*) 'Read_MSI: Error reading header of file ', trim(adjustl(Ctrl%Fid%MSI))
         call Write_Log(Ctrl, trim(message), status)
         stop
      endif
   end if

   if (status == 0) then
      ! Read MSI file
      write(*,*) 'Start reading MSI input'

      ! Allocate Data%MSI structure size to match image segments to be used.
      allocate(MSI_Data%MSI(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%Ny))
      allocate(MSI_Data%time(Ctrl%Ind%Xmax, SegSize))

      ! Read instrument channel indices from file
      allocate(msi_instr_ch_numbers(Ctrl%Ind%Nyp))
      msi_instr_ch_numbers=0_nint
      call nc_read_array_1d_int_to_int_orac(ncid,Ctrl%Ind%Nyp,"msi_instr_ch_numbers",msi_instr_ch_numbers,0)

      do i=1,Ctrl%Ind%Ny
         write(*,*) 'Read data for channel in MSI file: ', &
            i,msi_instr_ch_numbers(Ctrl%Ind%Chi(i)),Ctrl%Ind%Chi(i),Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i))
         call nc_read_array_3d_float_orac &
            (ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,Ctrl%Ind%Chi(i),"msi_data",MSI_Data%MSI(:,:,i),0)
      enddo

      deallocate(msi_instr_ch_numbers)

      ! Read time data
      call nc_read_array_2d_double_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,"time_data",MSI_Data%time(:,:),0)

      write(*,*) 'Done reading MSI input'
   endif

   ! Close MSI input file
   ios=nf90_close(ncid)
   if (ios /= 0) then
      status = MSIFileCloseErr ! Return error code
      write(unit=message, fmt=*) &
         'Read_MSI: Error closing file ', trim(adjustl(Ctrl%Fid%MSI))
      write(*,*) trim(message)
      call Write_Log(Ctrl, trim(message), status)
   endif

end subroutine Read_MSI_nc
