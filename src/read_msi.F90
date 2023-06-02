!-------------------------------------------------------------------------------
! Name: read_msi.F90
!
! Purpose:
! Controls the reading of Multi Spectral Image (MSI) values from ATSR-type
! files into the DATA_MSI array.
!
! Description and Algorithm details:
! If (MSI files are not yet open)
!    Find a logical unit number to be used for the MSI file
!    Open MSI file
!    If open error
!       Write error message to screen and log file
!    else
!       allocate MSI image segment array in Data_MSI struct.
!
! If (no error opening files)
!    Read header (not used further)
!    If read error
!       Write error message to screen and log file
!    Else
!       Read MSI array of size defined by Ctrl structure
!       If read error
!          Write error message to log file
! Leave MSI file open for further reads
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       Both        Control structure (date is read in here).
! MSI_Data struct       Both        Data structure: the MSI data part of this
!                                   struct is populated by this routine, and
!                                   is overwritten on successive calls.
! SAD_Chan struct array Both        Instrument channel parameters. Updated
!                                   by this routine: solar constant is
!                                   modified from annual average to value for
!                                   the day of the MSI data.
!
! History:
! 2000/11/03, KS: Original version
! 2000/11/23, KS: Added status to argument list
! 2000/12/19, KS: Replaced Data_MSI array with data structure
! 2001/01/15, KS: Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
! 2001/07/27, AS: Updates for handling image segmentation:
!     - new arguments MSI_files_open, lun (since the file now stays open for
!       repeated read operations)
!     - file open depends on MSI_files_open flag
!    Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!    caused any problems so far but it might).
!    Added argument intent specifiers.
! 2001/08/10, AS: Updated to handle image segments/super-pixels of any size.
!    Requires  handling of end of file during read on the last segment. On
!    earlier segments EOF is reported as an error. Added code to read the file
!    header, extract date and convert the solar constant mean values in SAD_Chan
!    to values for the current month and day. (New argument SAD_Chan). Intent for
!    Ctrl changed to inout.
! 2001/08/22, AS: Bug fix: added status check before ReadFPArray call.
! 2001/08/23, AS: Additional argument to ReadFPArray: Ctrl%Ind%NChans, no of
!    instrument channels available, specifies size of input array of channel data
!    (as opposed to Ctrl%Ind%Ny which specifies the size of the output array).
! 2011/05/18, AS:
!    Extension to multiple instrument views. Number of "channels" available
!    in the MSI file is increased by factor of NViews. Modify the NChans
!    argument passed to ReadFPArray.
!    Array of selected channel identifiers Y_Id must be modified before
!    passing to ReadFPArray, as it contains repeated Channel IDs if more than
!    one view is selected. Use Y_Id and ViewIdx to populate ChanIdx array.
! 2011/06/28, CP: Remove reference to ATSR,  Changed  Ctrl.Ind.Nchans to
!    Ctrl.Ind.Ny to remove dependence! on config file
! 2011/11/25, CP: Add ChI(replace y_id) channel indice variable.
! 2011/12/02, CP: Changed Ninstviews to nviews
! 2011/12/13, CP: Change format statement to make g95 compatible
! 2012/01/20, CP: Remove write statement
! 2012/08/21, MJ: Uses original routine and implements reading of netcdf data.
! 2012/09/21, CP: Added channel index to y_id value
! 2012/11/03, MST and MJ: Hard code in values for avhrr
! 2013/11/18, MJ: Cleans and debugs
! 2014/01/31, MJ: Removed hardcoded parts for avhrr (obsolete)
! 2014/04/18, GM: Cleaned up the code.
! 2014/04/30, GM: Fixed a bug introduced by a previous change.
! 2014/08/15, AP: Switching to preprocessor NCDF routines.
! 2014/01/30, AP: Remove NSegs, SegSize arguments.
! 2015/02/04, GM: Changes related to the new missing channel, illumination, and
!    channel selection code.  In particular, setting of out-of-range
!    measurements to MissingXn is now done here.
! 2015/06/02, AP: Add read of view direction and pixel variance.
! 2015/07/03, OS: Added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
! 2015/08/26, AP: Correct name of date attribute.
! 2015/08/31, AP: Check if ViewIdx have valid values.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2021/10/12, ATP: Add new approximation for F0 variation throughout
!    the year. This is needed for new LUTs as F1 is not provided in
!    the netcdf LUTs.
!
! Bugs:
! The DOY calculation does not account for leap years. This produces fractional
! errors in f0 < 6e-4, which is of a similar order to the equation's accuracy.
! Also, older codes deduced DOY from the end of the orbit whilst this is now
! derived from the beginning, introducing a slight difference.
!-------------------------------------------------------------------------------

subroutine Read_MSI(Ctrl, MSI_Data, SAD_Chan)

   use Ctrl_m
   use ORAC_Constants_m
   use orac_ncdf_m
   use SAD_Chan_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),     intent(inout) :: Ctrl
   type(Data_t),     intent(inout) :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(:)

   ! Local variables
   integer           :: i, j, k
   integer           :: day, month

   ! NetCDF related
   integer           :: ncid
   character(len=12) :: prod_date

   ! Open MSI file
   if (Ctrl%verbose) write(*,*) 'Imagery file: ', trim(Ctrl%FID%MSI)
   call ncdf_open(ncid, Ctrl%FID%MSI, 'Read_MSI()')

   ! Read product date and time from netcdf global attributes
   if (nf90_get_att(ncid, NF90_GLOBAL, "Date_Created", prod_date) == &
        NF90_NOERR) then
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
         if (SAD_Chan(i)%Solar%Flag > 0) then
            if (len_trim(Ctrl%LUTClass) == 3) then
               ! Old approximation for F0 variation throughout the year.
               SAD_Chan(i)%Solar%F0 = SAD_Chan(i)%Solar%F0 + &
                       (SAD_Chan(i)%Solar%F1 * cos(2. * Pi * Ctrl%DOY / 365.))
            else
               ! New (better) approximation for F0 variation throughout the year.
               ! Note this approximation must be used for newluts because
               ! F1 is not supplied in netcdfs.
               SAD_Chan(i)%Solar%F0 = SAD_Chan(i)%Solar%F0 / &
                       (1. - 0.0167086 * cos((2.* Pi * (Ctrl%DOY - 4.)) / &
                               365.256363))**2
            end if
         end if
      end do
   end if

   ! Read MSI file

   ! Allocate Data%MSI structure size to match image segments to be used.
   allocate(MSI_Data%MSI(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%Ny))
   allocate(MSI_Data%time(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%cal_gain(Ctrl%Ind%Ny))

   call ncdf_read_array(ncid, "msi_data", MSI_Data%MSI, 3, Ctrl%Ind%ICh)
   call ncdf_read_array(ncid, "time_data", MSI_Data%time)
   call ncdf_read_array(ncid, "cal_data", MSI_Data%cal_gain)

   ! Read variance data, if requested
   if (Ctrl%EqMPN%SySelm == SelmMeas) then
      allocate(MSI_Data%SD(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%Ny))
      call ncdf_read_array(ncid, "sd_data", MSI_Data%SD, 3, Ctrl%Ind%ICh)
   end if

   ! Read channel view indices from file (all channels)
!  allocate(Ctrl%Ind%View_Id(Ctrl%Ind%Ny))
!  call ncdf_read_array(ncid, "msi_ch_view", Ctrl%Ind%View_Id, &
!                     1, Ctrl%Ind%ICh)
   if (minval(Ctrl%Ind%View_Id) < 1 .or. &
       maxval(Ctrl%Ind%View_Id) > Ctrl%Ind%NViews) then
      write(*,*) 'ERROR: Read_MSI(): Invalid view indexing in input files.'
      stop error_stop_code
   end if

   ! Close MSI input file
   call ncdf_close(ncid, 'Read_MSI()')

   ! Set values that are out of range to MissingXn
   do i = 1,Ctrl%Ind%Ny
      do j = 1,Ctrl%Ind%Ymax
         do k = 1,Ctrl%Ind%Xmax
            if (btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
               if (MSI_Data%MSI(k,j,i) < BTMin .or. &
                   MSI_Data%MSI(k,j,i) > BTMax) then
                  MSI_Data%MSI(k,j,i) = sreal_fill_value

                  if (Ctrl%EqMPN%SySelm == SelmMeas) &
                       MSI_Data%SD(k,j,i) = sreal_fill_value
               end if
            else
               if (MSI_Data%MSI(k,j,i) < RefMin .or. &
                   MSI_Data%MSI(k,j,i) > RefMax) then
                  MSI_Data%MSI(k,j,i) = sreal_fill_value

                  if (Ctrl%EqMPN%SySelm == SelmMeas) &
                       MSI_Data%SD(k,j,i) = sreal_fill_value
               end if
            end if
         end do
      end do
   end do

end subroutine Read_MSI
