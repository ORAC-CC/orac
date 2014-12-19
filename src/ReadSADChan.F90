!-------------------------------------------------------------------------------
! Name:
!    Read_SAD_Chan
!
! Purpose:
!    Reads Static Application Data files containing channel description for use
!    by the ECP.
!
! Description:
!    Reads a set of Static Application Data files containing channel description
!    info for a given instrument (defined by the Ctrl struct) into the SAD_Chan
!    array of structs. Also sets SolarFirst/Last, ThermalFirst/Last in Ctrl%Ind
!    structure
!
! Arguments:
!    Name      Type   In/Out/Both     Description
!    Ctrl      struct In              Control structure defined in Ctrl_def
!    SAD_Chan  array of structs (out) Channel description info.
!    status    int    out             Indicates success/failure of subroutine.
!
! Algorithm:
!    for each channel described in the Ctrl struct
!       check for errors on last iteration of loop (drop out if any occurred)
!       construct the channel description file name
!       open the file (report any error)
!       read the file contents into SAD_Chan()
!       if the thermal flag indicates thermal data present
!          populate the SAD_Chan()%thermal struct
!       if the solar flag indicates solar data present
!          populate the SAD_Chan()%solar struct
!       close the file
!
! Local variables:
!    Name Type Description
!
! History:
!    23rd Aug 2000, Andy Smith: Original version
!     9th Jan 2001, Andy Smith:
!       Added setting of NSolar, YSolar, NThermal, YThermal in Ctrl%Ind.
!       Ctrl%Ind%Y renamed Y_Id
!    16th Jan 2001, Andy Smith:
!       Added code to set ThermalFirst to SolarLast+1 if no thermal channels are
!       selected.
!    19th Jan 2001, Andy Smith: comments updated
!    22nd Jan 2001, Andy Smith:
!       Solar%Rs now converted from percentage to a fraction.
!       Ctrl%Ind%YSolar, YThermal no longer set here (reverted to driver file).
!       Ctrl%Ind%NSolar, NThermal now checked here rather than set here.
!     5th Feb 2001, Kevin Smith: Added calculation of hidden Ctrl%Ind%NMixed.
!    12th Feb 2001, Kevin Smith: Added calculation of hidden Ctrl%Ind%MDAD_LW
!       and Ctrl%Ind%MDAD_SW, the indices of channels used in the MDAD method
!       to determine FG Pc, phase and Tau.
!    17th May 2001, Andy Smith:
!       Added setting of measurement error covariance values Ctrl%Sy.
!    22nd May 2001, Andy Smith:
!       Change to Ctrl%Sy setting. Moved out of "if" blocks for the thermal /
!       solar channels into it's own "if... else". Previously, for mixed
!       channels the thermal value (from NEBT) could be overwritten by the
!       solar.
!     7th Jun 2001, Andy Smith:
!       Ensured that MDAD_LW and SW are zeroed.
!       Homog and coreg noise terms are squared after reading in. Solar values
!       are also converted to fractions (chan file values are %).
!    25th Jun 2001, Andy Smith:
!       Removed old debug output.
!    11th Jul 2001, Andy Smith:
!       Added code to read Solar%F1 and convert from annual mean solar constant
!       to a value for the day of year.
!    14th Aug 2001, Andy Smith:
!       Moved code to convert Solar constant value from here to Read_ATSR_MSI
!       since the latter now reads the date from the MSI file header.
!    20th Sep 2012, Somebody:
!       Added channel index to y_id value.
!    14th Nov 2013, MJ:
!       Some cleanup and bugfixing: Fixed bug with selection of MDAD_SW/LW:
!       changed diff to abs(diff)
!     4th Feb 2014, MJ:
!       Implements code for AVHRR to assign channel numbers for LUT names.
!    23rd May 2014, Greg McGarragh:
!       Cleaned up code.
!     1st Jul 2014, Adam Povey:
!       Added check to see if SAD file exists to clarify error message.
!     1st Aug 2014, Greg McGarragh:
!          Use refactored code into Find_MDAD_LW() and Find_MDAD_SW() to use
!          here and elsewhere.
!    19th Dec 2014, Adam Povey: YSolar and YThermal now contain the index of
!          solar/thermalchannels with respect to the channels actually processed,
!          rather than the  MSI file.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_SAD_Chan(Ctrl, SAD_Chan, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Arguments

   type(Ctrl_t),                   intent(inout) :: Ctrl
   type(SAD_Chan_t), dimension(:), intent(inout) :: SAD_Chan
   integer,                        intent(inout) :: status

   ! Local variables

   character(FilenameLen) :: chan_file        ! Name of channel description file
   character(4)           :: chan_num         ! Channel number converted to a string
   integer                :: c_lun            ! Unit number for channel desc file
   integer                :: i, j             ! Loop counters
   integer                :: ios              ! I/O status from file open/read
   integer                :: ThermalFirstSet  ! ThermalFirst setting flag
   character(FilenameLen) :: filename         ! File name as read from chan desc file
   character(2048)        :: message          ! Error message to pass to Write_Log
   integer                :: NSolar, NThermal ! Local values used to check
                                              ! whether selected channels match
					      ! totals indicated in driver file.
   logical                :: file_exists

   status = 0
   ThermalFirstSet = 0 ! ThermalFirst not set
   NThermal = 0
   NSolar = 0
   Ctrl%Sy = 0.

   write(*,*)'Number of channels used, Ctrl%Ind%Ny: ',Ctrl%Ind%Ny

   call Find_LUN(c_lun)

   ! Loop over channels
   do i=1, Ctrl%Ind%Ny
      ! Drop out of loop if an open or write error occurred last time round
      if (status /= 0) exit

      ! Generate channel file name from Ctrl struct info. This sets the channel
      ! to be used in instrument notation for reading from SAD.
      write(*,*) 'Ctrl%Inst%Name(1:5): ', trim(Ctrl%Inst%Name(1:5))
      if (Ctrl%Ind%Y_Id(i) < 10) then
         if (trim(Ctrl%Inst%Name(1:5)) .ne. 'AVHRR') then
            write(chan_num, '(a2,i1)') 'Ch',Ctrl%Ind%Y_Id(i)
         else
            select case (Ctrl%Ind%Y_Id(i))
            case (1)
               chan_num='Ch1'
            case (2)
               chan_num='Ch2'
            case (3)
               chan_num='Ch3a'
            case (4)
               chan_num='Ch3b'
            case (5)
               chan_num='Ch4'
            case (6)
               chan_num='Ch5'
            end select
         end if

      else
         write(chan_num, '(a2,i2)') 'Ch',Ctrl%Ind%Y_Id(i)
      end if

      write(*,*) 'chan_num read in: ', trim(adjustl(chan_num))

      chan_file = trim(Ctrl%SAD_Dir) // trim(Ctrl%Inst%Name) &
         & // '_' // trim(chan_num) // '.sad'
      write(*,*)'chan_file read in: ',trim(adjustl(chan_file))
      ! Open the file
      inquire(file=chan_file, exist=file_exists)
      if (.not. file_exists) then
         write(message, *) 'Read_Chan: SAD file not found.'
         call Write_Log(Ctrl, trim(message), status)
         ios=-1
      else
         open(unit=c_lun, file=chan_file, iostat=ios)
      end if
      if (ios /= 0) then
         status = ChanFileOpenErr
         write(message, *) 'Read_Chan: Error opening file ', trim(chan_file)
         call Write_Log(Ctrl, trim(message), status)
      else
         ! Read the file contents into the SAD_Chan(i) structure
         read(c_lun, *, err=999, iostat=ios) filename
         read(c_lun, '(A10)', err=999, iostat=ios) SAD_Chan(i)%Desc
         read(c_lun, *, err=999, iostat=ios) SAD_Chan(i)%FileID

         ! Check FileID vs. expected value (i.e. value in filename vs. value in
         ! file)
         if (trim(SAD_Chan(i)%FileID) /= trim(chan_num)) then
            status = ChanFileDataErr
            write(message, *) &
               'Read_Chan: channel ID inconsistent with file name in ', &
               chan_file
            call Write_Log(Ctrl, trim(message), status)
         end if

         if (status == 0) then
            read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%WvN
            read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Thermal%Flag

            write(*,*)'Specs (wavenumber and t-flag):', &
               SAD_Chan(i)%WvN,SAD_Chan(i)%Thermal%Flag

            if (SAD_Chan(i)%Thermal%Flag > 0) then
               if (ThermalFirstSet == 0) then
                  Ctrl%Ind%ThermalFirst = i
                  ThermalFirstSet = 1 ! ThermalFirst set
               end if

               NThermal = NThermal + 1


               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Thermal%B1
               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Thermal%B2
               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Thermal%T1
               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Thermal%T2
               read(c_lun, *, err=999, iostat=ios) &
                  (SAD_Chan(i)%Thermal%NeHomog(j), j=1,MaxCloudType)
               read(c_lun, *, err=999, iostat=ios) &
                  (SAD_Chan(i)%Thermal%NeCoreg(j), j=1,MaxCloudType)

               do j=1,MaxCloudType
	          SAD_Chan(i)%Thermal%NeHomog(j) = &
                     SAD_Chan(i)%Thermal%NeHomog(j) ** 2
                  SAD_Chan(i)%Thermal%NeCoreg(j) = &
                     SAD_Chan(i)%Thermal%NeCoreg(j) ** 2
               end do

               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Thermal%NEBT
               SAD_Chan(i)%Thermal%NEBT = SAD_Chan(i)%Thermal%NEBT ** 2
            end if

            read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Solar%Flag
            write(*,*)'Specs (s-flag): ',SAD_Chan(i)%Solar%Flag
            if (SAD_Chan(i)%Solar%Flag > 0) then
               Ctrl%Ind%SolarLast = i
               NSolar = NSolar + 1

               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Solar%F0, &
                  SAD_Chan(i)%Solar%F1
               read(c_lun, *, err=999, iostat=ios) &
                  (SAD_Chan(i)%Solar%NeHomog(j), j=1,MaxCloudType)
               read(c_lun, *, err=999, iostat=ios) &
                  (SAD_Chan(i)%Solar%NeCoreg(j), j=1,MaxCloudType)

               do j=1,MaxCloudType
                  SAD_Chan(i)%Solar%NeHomog(j) = &
                     (SAD_Chan(i)%Solar%NeHomog(j) / 100.0) ** 2

                  SAD_Chan(i)%Solar%NeCoreg(j) = &
                     (SAD_Chan(i)%Solar%NeCoreg(j) / 100.0) ** 2
               end do

               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Solar%NedR
               SAD_Chan(i)%Solar%NedR = &
                  (SAD_Chan(i)%Solar%NedR / SAD_Chan(i)%Solar%F0) ** 2

               ! Convert Rs from a percentage to a fraction
               read(c_lun, *, err=999, iostat=ios)SAD_Chan(i)%Solar%Rs
               SAD_Chan(i)%Solar%Rs = SAD_Chan(i)%Solar%Rs / 100.0

            end if

            ! Set the measurement error covariance for the channel
            ! Thermal/mixed use NEBT, Solar use NEdR
            if (SAD_Chan(i)%Thermal%Flag > 0) then
               Ctrl%Sy(i,i) = SAD_Chan(i)%Thermal%NEBT
            else
               Ctrl%Sy(i,i) = SAD_Chan(i)%Solar%NedR
            end if
         end if

999      if (ios /= 0) then
            status = ChanFileReadErr
            write(message, *) &
               'Read_Chan: Error reading channel description file ', &
               chan_file
            call Write_Log(Ctrl, trim(message), status)
         end if

         close(c_lun)
      end if
   end do

   ! Check the NSolar and NThermal totals vs. the driver file values
   write(*,*)'NSolar,Ctrl%Ind%NSolar: ',NSolar,Ctrl%Ind%NSolar
   if (NSolar /= Ctrl%Ind%NSolar) then
      status = DriverFileDataErr
      call Write_Log(Ctrl, &
         'Read_Chan: Error in NSolar value in driver file', status)
      stop
   end if

   write(*,*)'NThermal,Ctrl%Ind%NThermal: ',NThermal,Ctrl%Ind%NThermal
   if (NThermal /= Ctrl%Ind%NThermal) then
      status = DriverFileDataErr
      call Write_Log(Ctrl, &
         'Read_Chan: Error in NThermal value in driver file', status)
      stop
   end if

   ! If no thermal channels are selected, ThermalFirst is 0. However, some solar
   ! routines rely on ThermalFirst-1 to index the last purely solar channel.
   ! Make sure a value is set.
   if (ThermalFirstSet == 0) then
      Ctrl%Ind%ThermalFirst = Ctrl%Ind%SolarLast + 1
   end if

   ! Set SolarFirst and ThermalLast
   Ctrl%Ind%SolarFirst = 1
   Ctrl%Ind%ThermalLast = Ctrl%Ind%Ny

   write(*,*) 'First/Last channels wrt number of channels used in '// &
      & 'successive order (aka stored in MSI array)'
   write(*,*) 'SolarFirst/Last: ', Ctrl%Ind%SolarFirst, Ctrl%Ind%SolarLast
   write(*,*) 'ThermalFirst/Last: ', Ctrl%Ind%ThermalFirst, Ctrl%Ind%ThermalLast

   ! Number of channels with both solar and thermal components (needed in FM).
   Ctrl%Ind%NMixed = Ctrl%Ind%NSolar + Ctrl%Ind%NThermal - Ctrl%Ind%Ny

   ! Find indices of channels required for calculation of MDAD FG state parameters
   Ctrl%Ind%MDAD_LW = Find_MDAD_LW(Ctrl%Ind%Ny, SAD_Chan)
   Ctrl%Ind%MDAD_SW = Find_MDAD_SW(Ctrl%Ind%Ny, SAD_Chan)

end subroutine Read_SAD_Chan
