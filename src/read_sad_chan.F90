!-------------------------------------------------------------------------------
! Name: read_sad_chan.F90
!
! Purpose:
! Reads Static Application Data files containing channel description for use
! by the ORAC.
!
! Description and Algorithm details:
! Reads a set of Static Application Data files containing channel description
! info for a given instrument (defined by the Ctrl struct) into the SAD_Chan
! array of structs.
!
! Description and Algorithm details:
! 1) For each channel described in the Ctrl struct
!     - construct the channel description file name
!     - open the file (report any error)
!     - read the file contents into SAD_Chan()
!     - if the thermal flag indicates thermal data present
!          populate the SAD_Chan()%thermal struct
!     - if the solar flag indicates solar data present
!          populate the SAD_Chan()%solar struct
!     - close the file
!
! Arguments:
! Name      Type   In/Out/Both     Description
! ------------------------------------------------------------------------------
! Ctrl      struct In              Control structure defined in Ctrl_m
! SAD_Chan  array of structs (out) Channel description info.
!
! History:
! 2000/08/23, AS: Original version
! 2001/01/09, AS: Added setting of NSolar, YSolar, NThermal, YThermal in
!    Ctrl%Ind. Ctrl%Ind%Y renamed Y_Id
! 2001/01/16, AS: Added code to set ThermalFirst to SolarLast+1 if no thermal
!    channels are selected.
! 2001/01/19, AS: comments updated
! 2001/01/22, AS: Solar%Rs now converted from percentage to a fraction.
!    Ctrl%Ind%YSolar, YThermal no longer set here (reverted to driver file).
!    Ctrl%Ind%NSolar, NThermal now checked here rather than set here.
! 2001/02/05, KS: Added calculation of hidden Ctrl%Ind%NMixed.
! 2001/02/12, KS: Added calculation of hidden Ctrl%Ind%MDAD_LW and
!    Ctrl%Ind%MDAD_SW, the indices of channels used in the MDAD method to
!    determine FG Pc, phase and Tau.
! 2001/05/17, AS: Added setting of measurement error covariance values Ctrl%Sy.
! 2001/05/22, AS: Change to Ctrl%Sy setting. Moved out of "if" blocks for the
!    thermal / solar channels into it's own "if... else". Previously, for mixed
!    channels the thermal value (from NEBT) could be overwritten by the solar.
! 2001/06/07, AS: Ensured that MDAD_LW and SW are zeroed. Homog and coreg noise
!    terms are squared after reading in. Solar values are also converted to
!    fractions (chan file values are %).
! 2001/06/25, AS: Removed old debug output.
! 2001/07/11, AS: Added code to read Solar%F1 and convert from annual mean solar
!    constant to a value for the day of year.
! 2001/08/14, AS: Moved code to convert Solar constant value from here to
!    Read_ATSR_MSI since the latter now reads the date from the MSI file header.
! 2012/09/20, ??: Added channel index to y_id value.
! 2013/11/14, MJ: Some cleanup and bugfixing: Fixed bug with selection of
!    MDAD_SW/LW: changed diff to abs(diff)
! 2014/02/04, MJ: Implements code for AVHRR to assign channel numbers for LUT
!    names.
! 2014/05/23, GM: Cleaned up code.
! 2014/07/01, AP: Added check to see if SAD file exists to clarify error message
! 2014/08/01, GM: Use refactored code into Find_MDAD_LW() and Find_MDAD_SW() to
!    use here and elsewhere.
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2015/01/13, AP: Remove First:Last channel indexing.
! 2015/01/19, GM: Use make_sad_chan_num().
! 2015/04/30, MS: Correct the chan_file definition for NOAA-7 and NOAA-9
! 2015/06/02, AP: Writing to Sy managed in GetMeasurements.
! 2015/07/14, AP: Replacing **2 with multiplication.
! 2015/08/21, AP: Generalised MS last fix.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2016/07/28, GM: Remove hidden setting of Ctrl%Ind%NMixed.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_SAD_Chan(Ctrl, SAD_Chan)

   use Ctrl_m
   use ORAC_Constants_m
   use sad_util_m

   implicit none

   ! Arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(:)

   ! Local variables

   character(len=FilenameLen) :: chan_file        ! Name of channel description file
   character(len=4)           :: chan_num         ! Channel number converted to a string
   integer                    :: c_lun            ! Unit number for channel desc file
   integer                    :: i, j             ! Loop counters
   integer                    :: ios              ! I/O status from file open/read
   character(len=FilenameLen) :: filename         ! File name as read from chan desc file
   integer                    :: NSolar, NThermal ! Local values used to check
                                                  ! whether selected channels match
                                                  ! totals indicated in driver file.
   logical :: file_exists

   NThermal = 0
   NSolar   = 0

   if (Ctrl%verbose) &
        write(*,*) 'Number of channels used, Ctrl%Ind%Ny: ', Ctrl%Ind%Ny

   call Find_LUN(c_lun)

   ! Loop over channels
   do i = 1, Ctrl%Ind%Ny
      ! Generate channel file name from Ctrl struct info. This sets the channel
      ! to be used in instrument notation for reading from SAD.

      call make_sad_chan_num(Ctrl, i, chan_num)
      if (Ctrl%verbose) &
           write(*,*) 'SAD Channel number: ', trim(adjustl(chan_num))

      chan_file = create_sad_filename(Ctrl, chan_num)
      if (Ctrl%verbose) write(*,*) 'chan_file read in: ', trim(adjustl(chan_file))

      ! Check if file exists
      inquire(file=chan_file, exist=file_exists)
      if (.not. file_exists) then
         write(*,*) 'ERROR: Read_SAD_Chan(): SAD channel file not found: ', &
                    trim(chan_file)
         stop error_stop_code
      end if

      ! Open the file
      open(unit=c_lun, file=chan_file, iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: Read_SAD_Chan(): Error opening SAD channel file: ', &
                    trim(chan_file)
         stop ChanFileOpenErr
      end if

      ! Read the file contents into the SAD_Chan(i) structure
      read(c_lun, *, err=999, iostat=ios) filename
      read(c_lun, '(A10)', err=999, iostat=ios) SAD_Chan(i)%Desc
      read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%FileID

      ! Check FileID vs. expected value (i.e. value in filename vs. value in ! file)
      if (trim(SAD_Chan(i)%FileID) /= trim(chan_num)) then
         write(*,*) 'ERROR: Read_SAD_Chan(): channel ID inconsistent with ' // &
                    'file name in: ', trim(chan_file)
         stop ChanFileDataErr
      end if

      read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%WvN
      read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Thermal%Flag
      if (Ctrl%verbose) &
           write(*,*) 'Specs (wavenumber and t-flag):', SAD_Chan(i)%WvN, &
                      SAD_Chan(i)%Thermal%Flag

      if (SAD_Chan(i)%Thermal%Flag > 0) then
         NThermal = NThermal + 1

         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Thermal%B1
         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Thermal%B2
         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Thermal%T1
         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Thermal%T2
         read(c_lun, *, err=999, iostat=ios) &
            (SAD_Chan(i)%Thermal%NeHomog(j), j = 1, MaxCloudType)
         read(c_lun, *, err=999, iostat=ios) &
            (SAD_Chan(i)%Thermal%NeCoreg(j), j = 1, MaxCloudType)

         do j = 1, MaxCloudType
            SAD_Chan(i)%Thermal%NeHomog(j) = &
               SAD_Chan(i)%Thermal%NeHomog(j) * SAD_Chan(i)%Thermal%NeHomog(j)
            SAD_Chan(i)%Thermal%NeCoreg(j) = &
               SAD_Chan(i)%Thermal%NeCoreg(j) * SAD_Chan(i)%Thermal%NeCoreg(j)
         end do

         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Thermal%NEBT
         SAD_Chan(i)%Thermal%NEBT = SAD_Chan(i)%Thermal%NEBT * &
                                    SAD_Chan(i)%Thermal%NEBT
      end if

      read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Solar%Flag
      if (Ctrl%verbose) write(*,*) 'Specs (s-flag): ', SAD_Chan(i)%Solar%Flag

      if (SAD_Chan(i)%Solar%Flag > 0) then
         NSolar = NSolar + 1

         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Solar%F0, &
            SAD_Chan(i)%Solar%F1
         read(c_lun, *, err=999, iostat=ios) &
            (SAD_Chan(i)%Solar%NeHomog(j), j = 1, MaxCloudType)
         read(c_lun, *, err=999, iostat=ios) &
            (SAD_Chan(i)%Solar%NeCoreg(j), j = 1, MaxCloudType)

         do j = 1, MaxCloudType
            SAD_Chan(i)%Solar%NeHomog(j) = 1e-4 * &
               SAD_Chan(i)%Solar%NeHomog(j) * SAD_Chan(i)%Solar%NeHomog(j)

            SAD_Chan(i)%Solar%NeCoreg(j) = 1e-4 * &
               SAD_Chan(i)%Solar%NeCoreg(j) * SAD_Chan(i)%Solar%NeCoreg(j)
         end do

         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Solar%NedR
         SAD_Chan(i)%Solar%NedR = SAD_Chan(i)%Solar%NedR / SAD_Chan(i)%Solar%F0
         SAD_Chan(i)%Solar%NedR = SAD_Chan(i)%Solar%NedR * SAD_Chan(i)%Solar%NedR

         ! Convert Rs from a percentage to a fraction
         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%Solar%Rs
         SAD_Chan(i)%Solar%Rs = SAD_Chan(i)%Solar%Rs / 100.0
      end if

999   if (ios /= 0) then
         write(*,*) 'ERROR: Read_SAD_Chan(): Error reading channel ' // &
                    'description file: ', trim(chan_file)
         stop ChanFileReadErr
      end if

      close(c_lun)
   end do

   ! Check the NSolar and NThermal totals vs. the driver file values
   if (Ctrl%verbose) &
        write(*,*) 'NSolar,Ctrl%Ind%NSolar: ', NSolar, Ctrl%Ind%NSolar
   if (NSolar /= Ctrl%Ind%NSolar) then
      write(*,*) 'ERROR: Read_SAD_Chan(): Error in NSolar value in driver file'
      stop DriverFileDataErr
   end if

   if (Ctrl%verbose) &
        write(*,*) 'NThermal,Ctrl%Ind%NThermal: ', NThermal, Ctrl%Ind%NThermal
   if (NThermal /= Ctrl%Ind%NThermal) then
      write(*,*) 'ERROR: Read_SAD_Chan(): Error in NThermal value in driver file'
      stop DriverFileDataErr
   end if

end subroutine Read_SAD_Chan
