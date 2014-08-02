! Read_LUT. This file contains all the Read_LUT_xxx subroutines as well as the
! Read_LUT routine that calls them. Each routine has it's own header comment
! section.
!
! Header comments common to all Read_LUT_xxx subroutines are below.
!
! Arguments:
!    Name     Type   In/Out/Both Description
!    Ctrl     struct In          Control structure passed to Write_Log for log
!                                file name
!    l_lun    int    in          Unit number for LUT file
!    LUT_file string in          LUT file name
!    chan     int    in          Current channel number used for array indexing
!                                within the SAD_LUT struct, e.g.
!                                SAD_LUT%Wavelength for checking against LUT
!                                file data.
!    SAD_LUT  struct out         Structure to hold the values from the LUT file.
!                                N.B. this is one struct from the array used
!                                elsewhere, not the whole array.
!    status   int    out         Status value returned by all ECP routines.
!
! Algorithm:
!    Open the LUT file specified by LUT_file, using unit l_lun (if error, report
!      error)
!    Read the wavelength value into SAD_LUT%Wavelength(chan)
!    For each of the Grid parameters:
!       Read in the number of values
!       Read the specified number of values into SAD_LUT%Grid%<array>
!       Use the values read in to set the min and max for the parameter
!   (Note since Grid is not an array, it's values are overwritten by the values
!   from successive LUT files)
!
!   Read in the LUT array
!
!   If an error was found when checking no of grid parameter values report the
!      error
!
!   Note that this subroutine is passed one of type SAD_LUT, rather than the
!      whole arrays used above.
!
! Local variables:
!    Name Type Description
!
! $Id$
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! Name:
!    Read_LUT_Rd
!
! Purpose:
!    Reads Look Up Table values for diffuse reflectance from LUT files into
!    SAD_LUT struct.
!
! History:
!    13th Oct 2000, Andy Smith: original version
!    22/03/2013, Gareth Thomas: Added trim() to LUT_file in write(message,*)
!       statements (called on I/O error). Also added write(*,*) statements for
!       I/O errors
!    12/01/2014, Greg McGarragh: Fixed LUT index for SAD_LUT%Grid%Satzen.
!    16/01/2014, Greg McGarragh: Added initialization of SAD_LUT%table_use*
!       arrays.
!    23/01/2014, Greg McGarragh: Cleaned up code.
!
! Bugs:
!    None known
!
!-------------------------------------------------------------------------------

subroutine Read_LUT_Rd(Ctrl, l_lun, LUT_file, chan, SAD_LUT, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Argument declarations
   ! Note that SAD_LUT are arrays of structs in the calling routine, but scalar
   ! structs here.

   type(CTRL_t),    intent(in)    :: Ctrl
   integer,         intent(in)    :: l_lun    ! Unit number for LUT file
   character(*),    intent(in)    :: LUT_file ! Name of LUT file
   integer,         intent(in)    :: chan     ! Current channel number
   type(SAD_LUT_t), intent(inout) :: SAD_LUT  ! Single structs from the array
                                              ! used in the main program
   integer,         intent(inout) :: status

   ! Local variables

   integer        :: i,j,k   ! Loop counters
   integer        :: ios     ! I/O status from file operations
   character(180) :: message ! Error message to pass to Write_Log
   integer        :: nVals   ! No. of Tau/Re/Satzen etc values in file

   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)

   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTRd: Error opening file ', trim(LUT_file)
      write(message, *) 'Read_LUTRd: Error opening file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   else
      SAD_LUT%table_used_for_channel(chan, IRd) = .true.
      SAD_LUT%table_used_for_channel(chan, IRFd) = .true.

      SAD_LUT%table_uses_satzen(IRd) = .true.
      SAD_LUT%table_uses_solzen(IRd) = .false.
      SAD_LUT%table_uses_relazi(IRd) = .false.

      SAD_LUT%table_uses_satzen(IRFd) = .false.
      SAD_LUT%table_uses_solzen(IRFd) = .false.
      SAD_LUT%table_uses_relazi(IRFd) = .false.


      ! Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)


      ! Now get the tau values
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau(chan,IRd)
      SAD_LUT%Grid%dTau(chan,IRFd) = SAD_LUT%Grid%dTau(chan,IRd)
      SAD_LUT%Grid%nTau(chan,IRd)  = nVals ! Used for loop control later
      SAD_LUT%Grid%nTau(chan,IRFd) = SAD_LUT%Grid%nTau(chan,IRd)

      if (chan == Ctrl%Ind%SolarFirst) then
         if (.not.associated(SAD_LUT%Grid%Tau)) then
            allocate(SAD_LUT%Grid%Tau(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxtau,maxcrprops))
         endif
      endif
      SAD_LUT%Grid%Tau(chan,:,IRd)  = 0.0
      SAD_LUT%Grid%Tau(chan,:,IRFd) = 0.0

      read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(chan,i,IRd), i=1,nVals)
      SAD_LUT%Grid%Tau(chan,:,IRFd)  = SAD_LUT%Grid%Tau(chan,:,IRd)

      SAD_LUT%Grid%MinTau(chan,IRd)  = SAD_LUT%Grid%Tau(chan,1,IRd)
      SAD_LUT%Grid%MaxTau(chan,IRd)  = SAD_LUT%Grid%Tau(chan,nVals,IRd)

      SAD_LUT%Grid%MinTau(chan,IRFd) = SAD_LUT%Grid%MinTau(chan,IRd)
      SAD_LUT%Grid%MaxTau(chan,IRFd) = SAD_LUT%Grid%MaxTau(chan,IRd)


      ! Now get the satzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen(chan,IRd)
         SAD_LUT%Grid%dSatzen(chan,IRFd) = SAD_LUT%Grid%dSatzen(chan,IRd)
         SAD_LUT%Grid%nSatzen(chan,IRd)  = nVals
         SAD_LUT%Grid%nSatzen(chan,IRFd) = SAD_LUT%Grid%nSatzen(chan,IRd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Satzen)) then
               allocate(SAD_LUT%Grid%Satzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsatzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%satzen(chan,:,IRd)  = 0.0
         SAD_LUT%Grid%satzen(chan,:,IRFd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Satzen(chan,i,IRd), i=1,nVals)
         SAD_LUT%Grid%Satzen(chan,:,IRFd)  = SAD_LUT%Grid%Satzen(chan,:,IRd)

         SAD_LUT%Grid%MinSatzen(chan,IRd)  = SAD_LUT%Grid%Satzen(chan,1,IRd)
         SAD_LUT%Grid%MaxSatzen(chan,IRd)  = SAD_LUT%Grid%Satzen(chan,nVals,IRd)

         SAD_LUT%Grid%MinSatzen(chan,IRFd) = SAD_LUT%Grid%MinSatzen(chan,IRd)
         SAD_LUT%Grid%MaxSatzen(chan,IRFd) = SAD_LUT%Grid%MaxSatzen(chan,IRd)
      endif


      ! Now get the effective radius values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe(chan,IRd)
         SAD_LUT%Grid%dRe(chan,IRFd)=SAD_LUT%Grid%dSatzen(chan,IRd)
         SAD_LUT%Grid%nRe(chan,IRd) = nVals
         SAD_LUT%Grid%nRe(chan,IRFd)=SAD_LUT%Grid%nRe(chan,IRd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Re)) then
               allocate(SAD_LUT%Grid%Re(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxre,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Re(chan,:,IRd)  = 0.0
         SAD_LUT%Grid%Re(chan,:,IRFd) = 0.0

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(chan,i,IRd), i=1,nVals)
         SAD_LUT%Grid%Re(chan,:,IRFd)  = SAD_LUT%Grid%Re(chan,:,IRd)

         SAD_LUT%Grid%MinRe(chan,IRd)  = SAD_LUT%Grid%Re(chan,1,IRd)
         SAD_LUT%Grid%MaxRe(chan,IRd)  = SAD_LUT%Grid%Re(chan,nVals,IRd)

         SAD_LUT%Grid%MinRe(chan,IRFd) = SAD_LUT%Grid%MinRe(chan,IRd)
         SAD_LUT%Grid%MaxRe(chan,IRFd) = SAD_LUT%Grid%MaxRe(chan,IRd)
      endif


      ! Read in the Rd array
      if (chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Rd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, SAD_LUT%Grid%nmaxre))
         SAD_LUT%Rd = 0.0
      endif

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios) &
            (((SAD_LUT%Rd(chan, i, j, k), i=1, SAD_LUT%Grid%nTau(chan,IRd)), &
            j=1, SAD_LUT%Grid%nSatzen(chan,IRd)), k=1, SAD_LUT%Grid%nRe(chan,IRd))
      endif


      ! Read in the RFd array.
      if (chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Rfd(Ctrl%Ind%Ny, SAD_LUT%Grid%nmaxtau, SAD_LUT%Grid%nmaxre))
         SAD_LUT%Rfd=-10.0
      endif

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            ((SAD_LUT%RFd(chan, i, j), i=1, SAD_LUT%Grid%nTau(chan,IRFd)), &
            j=1, SAD_LUT%Grid%nRe(chan,IRFd))
      endif


      close(unit=l_lun)
   endif

999 if (ios /= 0) then
      status = LUTFileReadErr
      write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   endif

end subroutine Read_LUT_Rd


!-------------------------------------------------------------------------------
! Name:
!    Read_LUT_Td
!
! Purpose:
!    Reads Look Up Table values for diffuse transmission from LUT files into
!    SAD_LUT struct.
!
! History:
!    13th Oct 2000, Andy Smith: original version
!    22/03/2013, Gareth Thomas: Added trim() to LUT_file in write(message,*)
!       statements (called on I/O error). Also added write(*,*) statements for
!       I/O errors
!    16/01/2014, Greg McGarragh: Added initialization of SAD_LUT%table_use*
!       arrays.
!    23/01/2014, Greg McGarragh: Cleaned up code.
!
! Bugs:
!    None known
!
!-------------------------------------------------------------------------------

subroutine Read_LUT_Td(Ctrl, l_lun, LUT_file, chan, SAD_LUT, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Argument declarations
   ! Note that SAD_LUT are arrays of structs in the calling routine, but scalar
   ! structs here.

   type(CTRL_t),    intent(in)    :: Ctrl
   integer,         intent(in)    :: l_lun    ! Unit number for LUT file
   character(*),    intent(in)    :: LUT_file ! Name of LUT file
   integer,         intent(in)    :: chan     ! Current channel number
   type(SAD_LUT_t), intent(inout) :: SAD_LUT  ! Single structs from the array
                                              ! used in the main program
   integer,         intent(inout) :: status

   ! Local variables

   integer        :: i,j,k   ! Loop counters
   integer        :: ios     ! I/O status from file operations
   character(180) :: message ! Error message to pass to Write_Log
   integer        :: nVals   ! No. of Tau/Re/Satzen etc values in file

   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTTd: Error opening file ', trim(LUT_file)
      write(message, *) 'Read_LUTTd: Error opening file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   else
      SAD_LUT%table_used_for_channel(chan, ITd) = .true.
      SAD_LUT%table_used_for_channel(chan, ITFd) = .true.

      SAD_LUT%table_uses_satzen(ITd) = .true.
      SAD_LUT%table_uses_solzen(ITd) = .false.
      SAD_LUT%table_uses_relazi(ITd) = .false.

      SAD_LUT%table_uses_satzen(ITFd) = .false.
      SAD_LUT%table_uses_solzen(ITFd) = .false.
      SAD_LUT%table_uses_relazi(ITFd) = .false.


!     Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)


      ! Now get the tau values
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau(chan,ITd)
      SAD_LUT%Grid%dTau(chan,ITFd) = SAD_LUT%Grid%dTau(chan,ITd)
      SAD_LUT%Grid%nTau(chan,ITd)  = nVals! Used for loop control later
      SAD_LUT%Grid%nTau(chan,ITFd) = SAD_LUT%Grid%nTau(chan,ITd)

      if (chan == Ctrl%Ind%SolarFirst) then
         if (.not.associated(SAD_LUT%Grid%Tau)) then
            allocate(SAD_LUT%Grid%Tau(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxtau,maxcrprops))
         endif
      endif
      SAD_LUT%Grid%Tau(chan,:,ITd)  = 0.0
      SAD_LUT%Grid%Tau(chan,:,ITFd) = 0.0

      read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(chan,i,ITd), i=1,nVals)
      SAD_LUT%Grid%Tau(chan,:,ITFd)  = SAD_LUT%Grid%Tau(chan,:,ITd)

      SAD_LUT%Grid%MinTau(chan,ITd)  = SAD_LUT%Grid%Tau(chan,1,ITd)
      SAD_LUT%Grid%MaxTau(chan,ITd)  = SAD_LUT%Grid%Tau(chan,nVals,ITd)

      SAD_LUT%Grid%MinTau(chan,ITFd) = SAD_LUT%Grid%MinTau(chan,ITd)
      SAD_LUT%Grid%MaxTau(chan,ITFd) = SAD_LUT%Grid%MaxTau(chan,ITd)


      ! Now get the satzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen(chan,ITd)
         SAD_LUT%Grid%dSatzen(chan,ITFd) = SAD_LUT%Grid%dSatzen(chan,ITd)
         SAD_LUT%Grid%nSatzen(chan,ITd)  = nVals
         SAD_LUT%Grid%nSatzen(chan,ITFd) = SAD_LUT%Grid%nSatzen(chan,ITd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Satzen)) then
               allocate(SAD_LUT%Grid%Satzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsatzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Satzen(chan,:,ITd)  = 0.0
         SAD_LUT%Grid%Satzen(chan,:,ITFd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Satzen(chan,i,ITd), i=1,nVals)
         SAD_LUT%Grid%Satzen(chan,:,ITFd)  = SAD_LUT%Grid%Satzen(chan,:,ITd)

         SAD_LUT%Grid%MinSatzen(chan,ITd)  = SAD_LUT%Grid%Satzen(chan,1,ITd)
         SAD_LUT%Grid%MaxSatzen(chan,ITd)  = SAD_LUT%Grid%Satzen(chan,nVals,ITd)

         SAD_LUT%Grid%MinSatzen(chan,ITFd) = SAD_LUT%Grid%MinSatzen(chan,ITd)
         SAD_LUT%Grid%MaxSatzen(chan,ITFd) = SAD_LUT%Grid%MaxSatzen(chan,ITd)
      endif


      ! Now get the effective radius values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe(chan,ITd)
         SAD_LUT%Grid%dRe(chan,ITFd) = SAD_LUT%Grid%dRe(chan,ITd)
         SAD_LUT%Grid%nRe(chan,ITd)  = nVals
         SAD_LUT%Grid%nRe(chan,ITFd) = SAD_LUT%Grid%nRe(chan,ITd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Re)) then
               allocate(SAD_LUT%Grid%Re(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxre,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Re(chan,:,ITd)  = 0.0
         SAD_LUT%Grid%Re(chan,:,ITFd) = 0.0

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(chan,i,ITd), i=1,nVals)
         SAD_LUT%Grid%Re(chan,:,ITFd)  = SAD_LUT%Grid%Re(chan,:,ITd)

         SAD_LUT%Grid%MinRe(chan,ITd)  = SAD_LUT%Grid%Re(chan,1,ITd)
         SAD_LUT%Grid%MaxRe(chan,ITd)  = SAD_LUT%Grid%Re(chan,nVals,ITd)

         SAD_LUT%Grid%MinRe(chan,ITFd) = SAD_LUT%Grid%MinRe(chan,ITd)
         SAD_LUT%Grid%MaxRe(chan,ITFd) = SAD_LUT%Grid%MaxRe(chan,ITd)
      endif

      ! Read in the Td array
      if (chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Td(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, SAD_LUT%Grid%nmaxre))
         SAD_LUT%Td = 0.0
      endif

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            (((SAD_LUT%Td(chan, i, j, k), i=1, SAD_LUT%Grid%nTau(chan,ITd)), &
            j=1, SAD_LUT%Grid%nSatzen(chan,ITd)), k=1, SAD_LUT%Grid%nRe(chan,ITd))
      endif


      ! Read in the TFd array.
      if (chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Tfd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxre))
         SAD_LUT%Tfd = 0.0
      endif
      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            ((SAD_LUT%TFd(chan, i, j), i=1, SAD_LUT%Grid%nTau(chan,ITFd)), &
            j=1, SAD_LUT%Grid%nRe(chan,ITFd))
      endif


      close(unit=l_lun)
   endif
999 if (ios /= 0) then
      status = LUTFileReadErr
      write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   endif

end subroutine Read_LUT_Td


!-------------------------------------------------------------------------------
! Name:
!    Read_LUT_Rbd
!
! Purpose:
!    Reads Look Up Table values for bi-directional reflectance from LUT files
!    into SAD_LUT struct.
!
! History:
!     6th Oct 2000, Andy Smith : original version
!    17th Apr 2013, Caroline Poulsen: fixed bug in which nsolzen specified
!       before nsatzen in rbd read command
!    16th Jan 2014, Greg McGarragh: Added initialization of SAD_LUT%table_use*
!       arrays.
!    23th Jan 2014, Greg McGarragh: Cleaned up code.
!
! Bugs:
!    None known
!
!-------------------------------------------------------------------------------

subroutine Read_LUT_Rbd(Ctrl, l_lun, LUT_file, chan, SAD_LUT, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Argument declarations
   ! Note that SAD_LUT are arrays of structs in the calling routine, but scalar
   ! structs here.

   type(CTRL_t),    intent(in)    :: Ctrl
   integer,         intent(in)    :: l_lun    ! Unit number for LUT file
   character(*),    intent(in)    :: LUT_file ! Name of LUT file
   integer,         intent(in)    :: chan     ! Current channel number
   type(SAD_LUT_t), intent(inout) :: SAD_LUT  ! Single structs from the array
                                              ! used in the main program
   integer,         intent(inout) :: status

   ! Local variables

   integer        :: i,j,k,l,m ! Loop counters
   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
   integer        :: nVals     ! No. of Tau/Re/Satzen etc values in file

   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTRbd: Error opening file ', trim(LUT_file)
      write(message, *) 'Read_LUTRbd: Error opening file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   else
      SAD_LUT%table_used_for_channel(chan, IRBd) = .true.

      SAD_LUT%table_uses_satzen(IRBd) = .true.
      SAD_LUT%table_uses_solzen(IRBd) = .true.
      SAD_LUT%table_uses_relazi(IRBd) = .true.


      ! Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)


      ! Now get the tau values
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau(chan,IRBd)
      SAD_LUT%Grid%nTau(chan,IRBd) = nVals ! Used for loop control later

      if (chan == Ctrl%Ind%SolarFirst) then
         if (.not.associated(SAD_LUT%Grid%Tau)) then
            allocate(SAD_LUT%Grid%Satzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxtau,maxcrprops))
         endif
      endif
      read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(chan,i,IRBd), i=1,nVals)

      SAD_LUT%Grid%MinTau(chan,IRBd) = SAD_LUT%Grid%Tau(chan,1,IRBd)
      SAD_LUT%Grid%MaxTau(chan,IRBd) = SAD_LUT%Grid%Tau(chan,nVals,IRBd)


      ! Now get the satzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen(chan,IRBd)
         SAD_LUT%Grid%nSatzen(chan,IRBd) = nVals

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Satzen)) then
               allocate(SAD_LUT%Grid%Satzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsatzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%satzen(chan,:,IRBd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Satzen(chan,i,IRBd), i=1,nVals)

         SAD_LUT%Grid%MinSatzen(chan,IRBd) = SAD_LUT%Grid%Satzen(chan,1,IRBd)
         SAD_LUT%Grid%MaxSatzen(chan,IRBd) = SAD_LUT%Grid%Satzen(chan,nVals,IRBd)
      endif


      ! Now get the Solzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSolzen(chan,IRBd)
         SAD_LUT%Grid%nSolzen(chan,IRBd) = nVals

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Solzen)) then
               allocate(SAD_LUT%Grid%Solzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsolzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Solzen(chan,:,IRBd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Solzen(chan,i,IRBd), i=1,nVals)

         SAD_LUT%Grid%MinSolzen(chan,IRBd) = SAD_LUT%Grid%Solzen(chan,1,IRBd)
         SAD_LUT%Grid%MaxSolzen(chan,IRBd) = SAD_LUT%Grid%Solzen(chan,nVals,IRBd)
      endif


!     Now get the rel. azi. values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRelazi(chan,IRBd)
         SAD_LUT%Grid%nRelazi(chan,IRBd) = nVals

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Relazi)) then
               allocate(SAD_LUT%Grid%Relazi(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxrelazi,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Relazi(chan,:,IRBd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Relazi(chan,i,IRBd), i=1,nVals)

         SAD_LUT%Grid%MinRelazi(chan,IRBd) = SAD_LUT%Grid%Relazi(chan,1,IRBd)
         SAD_LUT%Grid%MaxRelazi(chan,IRBd) = SAD_LUT%Grid%Relazi(chan,nVals,IRBd)
      endif


      ! Now get the effective radius values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe(chan,IRBd)
         SAD_LUT%Grid%nRe(chan,IRBd) = nVals

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%re)) then
               allocate(SAD_LUT%Grid%Re(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxre,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Re(chan,:,IRBd) = 0.0

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(chan,i,IRBd), i=1,nVals)

         SAD_LUT%Grid%MinRe(chan,IRBd) = SAD_LUT%Grid%Re(chan,1,IRBd)
         SAD_LUT%Grid%MaxRe(chan,IRBd) = SAD_LUT%Grid%Re(chan,nVals,IRBd)
      endif


      ! Read in the RBD array.
      if (Ctrl%Ind%NSolar > 0 .and. chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Rbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
            SAD_LUT%Grid%NmaxSolzen, SAD_LUT%Grid%nmaxrelazi, SAD_LUT%Grid%nmaxre))
         SAD_LUT%Rbd = 0.0
      endif

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            (((((SAD_LUT%Rbd(chan, i, j, k, l, m), i=1, SAD_LUT%Grid%nTau(chan,IRBd)), &
            j=1, SAD_LUT%Grid%nSatzen(chan,IRBd)), k=1, SAD_LUT%Grid%nSolzen(chan,IRBd)), &
            l=1, SAD_LUT%Grid%nRelazi(chan,IRBd)), m=1, SAD_LUT%Grid%nRe(chan,IRBd))
      endif


      close(unit=l_lun)
   endif

999 if (ios /= 0) then
      status = LUTFileReadErr
      write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   endif

end subroutine Read_LUT_RBD


!-------------------------------------------------------------------------------
! Name:
!    Read_LUT_Tb
!
! Purpose:
!    Reads Look Up Table values for direct part of beam transmission from LUT
!    files into SAD_LUT struct.
!
! History:
!    6th Oct 2000, Andy Smith: original version
!    22/03/2013, Gareth Thomas: Added trim() to LUT_file in write(message,*)
!       statements (called on I/O error). Also added write(*,*) statements for
!       I/O errors
!    16/01/2014, Greg McGarragh: Added initialization of SAD_LUT%table_use*
!       arrays.
!    23/01/2014, Greg McGarragh: Cleaned up code.
!
! Bugs:
!    None known
!
!-------------------------------------------------------------------------------

subroutine Read_LUT_Tb(Ctrl, l_lun, LUT_file, chan, SAD_LUT, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Argument declarations
   ! Note that SAD_LUT are arrays of structs in the calling routine, but scalar
   ! structs here.

   type(CTRL_t),    intent(in)    :: Ctrl
   integer,         intent(in)    :: l_lun    ! Unit number for LUT file
   character(*),    intent(in)    :: LUT_file ! Name of LUT file
   integer,         intent(in)    :: chan     ! Current channel number
   type(SAD_LUT_t), intent(inout) :: SAD_LUT  ! Single structs from the array
                                              ! used in the main program
   integer,         intent(inout) :: status

   ! Local variables

   integer        :: i,j,k   ! Loop counters
   integer        :: ios     ! I/O status from file operations
   character(180) :: message ! Error message to pass to Write_Log
   integer        :: nVals   ! No. of Tau/Re/Satzen etc values in file

   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTTb: Error opening file ', trim(LUT_file)
      write(message, *) 'Read_LUTTb: Error opening file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   else
      write(*, *) 'Read_LUTTb:', trim(LUT_file)

      SAD_LUT%table_used_for_channel(chan, ITB) = .true.

      SAD_LUT%table_uses_satzen(ITB) = .false.
      SAD_LUT%table_uses_solzen(ITB) = .true.
      SAD_LUT%table_uses_relazi(ITB) = .false.


      ! Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)


      ! Now get the tau values
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau(chan,ITB)
      SAD_LUT%Grid%nTau(chan,ITB) = nVals ! Used for loop control later

      if (chan == Ctrl%Ind%SolarFirst) then
         if (.not.associated(SAD_LUT%Grid%Tau)) then
            allocate(SAD_LUT%Grid%Tau(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxtau,maxcrprops))
         endif
      endif
      SAD_LUT%Grid%Tau(chan,:,ITB) = 0.0

      read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(chan,i,ITB), i=1,nVals)

      SAD_LUT%Grid%MinTau(chan,ITB) = SAD_LUT%Grid%Tau(chan,1,ITB)
      SAD_LUT%Grid%MaxTau(chan,ITB) = SAD_LUT%Grid%Tau(chan,nVals,ITB)


      ! Now get the Solzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSolzen(chan,ITB)
         SAD_LUT%Grid%nSolzen(chan,ITB) = nVals

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Solzen)) then
               allocate(SAD_LUT%Grid%Solzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsolzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Solzen(chan,:,ITB) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Solzen(chan,i,ITB), i=1,nVals)

         SAD_LUT%Grid%MinSolzen(chan,ITB) = SAD_LUT%Grid%Solzen(chan,1,ITB)
         SAD_LUT%Grid%MaxSolzen(chan,ITB) = SAD_LUT%Grid%Solzen(chan,nVals,ITB)
      endif


      ! Now get the effective radius values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe(chan,ITB)
         SAD_LUT%Grid%nRe(chan,ITB) = nVals

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%re)) then
               allocate(SAD_LUT%Grid%Re(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxre,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Re(chan,:,ITB) = 0.0
         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(chan,i,ITB), i=1,nVals)
         SAD_LUT%Grid%MinRe(chan,ITB) = SAD_LUT%Grid%Re(chan,1,ITB)
         SAD_LUT%Grid%MaxRe(chan,ITB) = SAD_LUT%Grid%Re(chan,nVals,ITB)
      endif


      ! Read in the Tb array.
      if (Ctrl%Ind%NSolar > 0 .and. chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Tb(Ctrl%Ind%Ny, SAD_LUT%Grid%nmaxtau, SAD_LUT%Grid%nmaxsolzen, &
            SAD_LUT%Grid%nmaxre))
         SAD_LUT%Tb = 0.0
      endif

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            (((SAD_LUT%Tb(chan, i, j, k), i=1, SAD_LUT%Grid%nTau(chan,ITB)), &
            j=1, SAD_LUT%Grid%nSolzen(chan,ITB)), k=1, SAD_LUT%Grid%nRe(chan,ITB))
      endif


      close(unit=l_lun)
   endif

999 if (ios /= 0) then
      status = LUTFileReadErr
      write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   endif

end subroutine Read_LUT_Tb


!-------------------------------------------------------------------------------
! Name:
!    Read_LUT_Tbd
!
! Purpose:
!    Reads Look Up Table values for bi-directional reflectance from LUT files
!    into SAD_LUT struct.
!
! History:
!    6th Oct 2000, Andy Smith: original version
!    22/03/2013, Gareth Thomas: Added trim() to LUT_file in write(message,*)
!       statements (called on I/O error). Also added write(*,*) statements for
!       I/O errors
!    16/01/2014, Greg McGarragh: Added initialization of SAD_LUT%table_use*
!       arrays.
!    23/01/2014, Greg McGarragh: Cleaned up code.
!
! Bugs:
!    None known
!
!-------------------------------------------------------------------------------

subroutine Read_LUT_Tbd(Ctrl, l_lun, LUT_file, chan, SAD_LUT, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Argument declarations
   ! Note that SAD_LUT are arrays of structs in the calling routine, but scalar
   ! structs here.

   type(CTRL_t),    intent(in)    :: Ctrl
   integer,         intent(in)    :: l_lun    ! Unit number for LUT file
   character(*),    intent(in)    :: LUT_file ! Name of LUT file
   integer,         intent(in)    :: chan     ! Current channel number
   type(SAD_LUT_t), intent(inout) :: SAD_LUT  ! Single structs from the array
                                              ! used in the main program
   integer,         intent(inout) :: status

   ! Local variables

   integer        :: i,j,k,l,m ! Loop counters
   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
   integer        :: nVals     ! No. of Tau/Re/Satzen etc values in file

   ! Array to hold unwanted transmission values from LUT file. This array must
   ! be skipped over before reading TFbd. It seems easiest to use an implied do
   ! loop to read into this array and then throw it away. RANK AND SIZE OF ARRAY
   ! ASSUMED, BASED ON TD/TFD ETC

   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTTbd: Error opening file ', trim(LUT_file)
      write(message, *) 'Read_LUTTbd: Error opening file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   else
      SAD_LUT%table_used_for_channel(chan, ITBd)  = .true.
      SAD_LUT%table_used_for_channel(chan, ITFBd) = .true.

      SAD_LUT%table_uses_satzen(ITBd) = .true.
      SAD_LUT%table_uses_solzen(ITBd) = .true.
      SAD_LUT%table_uses_relazi(ITBd) = .true.

      SAD_LUT%table_uses_satzen(ITFBd) = .false.
      SAD_LUT%table_uses_solzen(ITFBd) = .true.
      SAD_LUT%table_uses_relazi(ITFBd) = .false.


      ! Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)


      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau(chan,ITBd)
      SAD_LUT%Grid%dTau(chan,ITFBd) = SAD_LUT%Grid%dTau(chan,ITBd)
      SAD_LUT%Grid%nTau(chan,ITBd)  = nVals ! Used for loop control later
      SAD_LUT%Grid%nTau(chan,ITFBd) = SAD_LUT%Grid%nTau(chan,ITBd)

      if (chan == Ctrl%Ind%SolarFirst) then
         if (.not.associated(SAD_LUT%Grid%Tau)) then
            allocate(SAD_LUT%Grid%Tau(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxtau,maxcrprops))
         endif
      endif
      SAD_LUT%Grid%Tau(chan,:,ITBd)  = 0.0
      SAD_LUT%Grid%Tau(chan,:,ITFBd) = 0.0

      read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(chan,i,ITBd), i=1,nVals)
      SAD_LUT%Grid%Tau(chan,:,ITFBd)  = SAD_LUT%Grid%Tau(chan,:,ITBd)

      SAD_LUT%Grid%MinTau(chan,ITBd)  = SAD_LUT%Grid%Tau(chan,1,ITBd)
      SAD_LUT%Grid%MinTau(chan,ITFBd) = SAD_LUT%Grid%MinTau(chan,ITBd)

      SAD_LUT%Grid%MaxTau(chan,ITBd)  = SAD_LUT%Grid%Tau(chan,nVals,ITBd)
      SAD_LUT%Grid%MaxTau(chan,ITFBd) = SAD_LUT%Grid%MaxTau(chan,ITBd)


      ! Now get the satzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen(chan,ITBd)
         SAD_LUT%Grid%dSatzen(chan,ITFBd) = SAD_LUT%Grid%dSatzen(chan,ITBd)
         SAD_LUT%Grid%nSatzen(chan,ITBd)  = nVals
         SAD_LUT%Grid%nSatzen(chan,ITFBd) = SAD_LUT%Grid%nSatzen(chan,ITBd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Satzen)) then
               allocate(SAD_LUT%Grid%Satzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsatzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%satzen(chan,:,ITBd)  = 0.0
         SAD_LUT%Grid%satzen(chan,:,ITFBd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Satzen(chan,i,ITBd), i=1,nVals)
         SAD_LUT%Grid%Satzen(chan,:,ITFBd)  = SAD_LUT%Grid%Satzen(chan,:,ITBd)

         SAD_LUT%Grid%MinSatzen(chan,ITBd)  = SAD_LUT%Grid%Satzen(chan,1,ITBd)
         SAD_LUT%Grid%MaxSatzen(chan,ITBd)  = SAD_LUT%Grid%Satzen(chan,nVals,ITBd)

         SAD_LUT%Grid%MinSatzen(chan,ITFBd) = SAD_LUT%Grid%MinSatzen(chan,ITBd)
         SAD_LUT%Grid%MaxSatzen(chan,ITFBd) = SAD_LUT%Grid%MaxSatzen(chan,ITBd)
      endif


      ! Now get the Solzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSolzen(chan,ITBd)
         SAD_LUT%Grid%dSolzen(chan,ITFBd)= SAD_LUT%Grid%dSolzen(chan,ITBd)
         SAD_LUT%Grid%nSolzen(chan,ITBd) = nVals
         SAD_LUT%Grid%nSolzen(chan,ITFBd)=SAD_LUT%Grid%nSolzen(chan,ITBd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Solzen)) then
               allocate(SAD_LUT%Grid%Solzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsolzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Solzen(chan,:,ITBd)  = 0.0
         SAD_LUT%Grid%Solzen(chan,:,ITFBd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Solzen(chan,i,ITBd), i=1,nVals)
         SAD_LUT%Grid%Solzen(chan,:,ITFBd)  = SAD_LUT%Grid%Solzen(chan,:,ITBd)

         SAD_LUT%Grid%MinSolzen(chan,ITBd)  = SAD_LUT%Grid%Solzen(chan,1,ITBd)
         SAD_LUT%Grid%MaxSolzen(chan,ITBd)  = SAD_LUT%Grid%Solzen(chan,nVals,ITBd)

         SAD_LUT%Grid%MinSolzen(chan,ITFBd) = SAD_LUT%Grid%MinSolzen(chan,ITBd)
         SAD_LUT%Grid%MaxSolzen(chan,ITFBd) = SAD_LUT%Grid%MaxSolzen(chan,ITBd)
      endif


      ! Now get the rel. azi. values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRelazi(chan,ITBd)
         SAD_LUT%Grid%dRelazi(chan,ITFBd) = SAD_LUT%Grid%dRelazi(chan,ITBd)
         SAD_LUT%Grid%nRelazi(chan,ITBd)  = nVals
         SAD_LUT%Grid%nRelazi(chan,ITFBd) = SAD_LUT%Grid%nRelazi(chan,ITBd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Relazi)) then
               allocate(SAD_LUT%Grid%Relazi(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxrelazi,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Relazi(chan,:,ITBd)  = 0.0
         SAD_LUT%Grid%Relazi(chan,:,ITFBd) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Relazi(chan,i,ITBd), i=1,nVals)
         SAD_LUT%Grid%Relazi(chan,:,ITFBd)  = SAD_LUT%Grid%Relazi(chan,:,ITBd)

         SAD_LUT%Grid%MinRelazi(chan,ITBd)  = SAD_LUT%Grid%Relazi(chan,1,ITBd)
         SAD_LUT%Grid%MaxRelazi(chan,ITBd)  = SAD_LUT%Grid%Relazi(chan,nVals,ITBd)

         SAD_LUT%Grid%MinRelazi(chan,ITFBd) = SAD_LUT%Grid%MinRelazi(chan,ITBd)
         SAD_LUT%Grid%MaxRelazi(chan,ITFBd) = SAD_LUT%Grid%MaxRelazi(chan,ITBd)
      endif


      ! Now get the effective radius values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe(chan,ITBd)
         SAD_LUT%Grid%dRe(chan,ITFBd) = SAD_LUT%Grid%dRe(chan,ITBd)
         SAD_LUT%Grid%nRe(chan,ITBd)  = nVals
         SAD_LUT%Grid%nRe(chan,ITFBd) = SAD_LUT%Grid%nRe(chan,ITBd)

         if (chan == Ctrl%Ind%SolarFirst) then
            if (.not.associated(SAD_LUT%Grid%Re)) then
               allocate(SAD_LUT%Grid%Re(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxre,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Re(chan,:,ITBd)  = 0.0
         SAD_LUT%Grid%Re(chan,:,ITFBd) = 0.0

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(chan,i,ITBd), i=1,nVals)
         SAD_LUT%Grid%Re(chan,:,ITFBd)  = SAD_LUT%Grid%Re(chan,:,ITBd)

         SAD_LUT%Grid%MinRe(chan,ITBd)  = SAD_LUT%Grid%Re(chan,1,ITBd)
         SAD_LUT%Grid%MaxRe(chan,ITBd)  = SAD_LUT%Grid%Re(chan,nVals,ITBd)

         SAD_LUT%Grid%MinRe(chan,ITFBd) = SAD_LUT%Grid%MinRe(chan,ITBd)
         SAD_LUT%Grid%MaxRe(chan,ITFBd) = SAD_LUT%Grid%MaxRe(chan,ITBd)
      endif

      ! Skip the Tbd array - we dont use this in the ECP so it isn't part of
      ! SAD_LUT.
      if (Ctrl%Ind%NSolar > 0) then
         if (chan == Ctrl%Ind%SolarFirst) then
            allocate(SAD_LUT%Tbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
               SAD_LUT%Grid%NmaxSolzen, SAD_LUT%Grid%nmaxrelazi, SAD_LUT%Grid%nmaxre))
            SAD_LUT%Tbd = 0.0
         endif
      endif

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            (((((SAD_LUT%Tbd(chan, i, j, k, l, m), i=1, SAD_LUT%Grid%nTau(chan,ITBd)), &
            j=1, SAD_LUT%Grid%nSatzen(chan,ITBd)), k=1, SAD_LUT%Grid%nSolzen(chan,ITBd)), &
            l=1, SAD_LUT%Grid%nRelazi(chan,ITBd)), m=1, SAD_LUT%Grid%nRe(chan,ITBd))
      endif


      ! Read in the TFbd array.
      if (Ctrl%Ind%NSolar > 0) then
         if (chan == Ctrl%Ind%SolarFirst) then
            allocate(SAD_LUT%Tfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%nmaxtau, SAD_LUT%Grid%nmaxsolzen, &
               SAD_LUT%Grid%nmaxre))
            SAD_LUT%Tfbd = 0.0
         endif
      endif

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            (((SAD_LUT%TFbd(chan, i, j, k), i=1, SAD_LUT%Grid%nTau(chan,ITFBd)), &
            j=1, SAD_LUT%Grid%nSolzen(chan,ITFBd)), k=1, SAD_LUT%Grid%nRe(chan,ITFBd))
      endif


      close(unit=l_lun)
   endif

999 if (ios /= 0) then
      status = LUTFileReadErr
      write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   endif

end subroutine Read_LUT_Tbd


!-------------------------------------------------------------------------------
! Name:
!    Read_LUT_Em
!
! Purpose:
!    Reads Look Up Table values for emissivity from LUT files into SAD_LUT
!    struct.
!
! History:
!    13th Oct 2000, Andy Smith : original version
!    11th Jun 2011, Caroline Poulsen: major rewrite, remove refrences to cloud
!       class changed the way arrays were allocated, removed references
!    13th Dec 2011, Caroline Poulsen: checked if array are aloocated with
!       associated command before allocating them.
!    20/09/2011, Caroline Poulsen: added channel index to y_id value
!    22/03/2013, Gareth Thomas: Added trim() to LUT_file in write(message,*)
!       statements (called on I/O error). Also added write(*,*) statements for
!       I/O errors
!    20131114, MJ: corrected zeroing of satzen when ref is allocated, some
!       cleanup
!    20131203, MJ: makes LUTs more flexible wrt channel and properties
!    16th Jan 2014, Greg McGarragh: Added initialization of SAD_LUT%table_use*
!       arrays.
!    23th Jan 2014, Greg McGarragh: Cleaned up code.
!
! Bugs:
!    None known
!
!-------------------------------------------------------------------------------

subroutine Read_LUT_Em(Ctrl, l_lun, LUT_file, chan, SAD_LUT, status)

   use Ctrl_def
   use ECP_Constants

   implicit none

   ! Argument declarations
   ! Note that SAD_LUT are arrays of structs in the calling routine, but scalar
   ! structs here.

   type(CTRL_t),    intent(in)    :: Ctrl
   integer,         intent(in)    :: l_lun    ! Unit number for LUT file
   character(*),    intent(in)    :: LUT_file ! Name of LUT file
   integer,         intent(in)    :: chan     ! Current channel number
   type(SAD_LUT_t), intent(inout) :: SAD_LUT  ! Single structs from the array
                                              ! used in the main program
   integer,         intent(inout) :: status

   ! Local variables

   integer        :: i,j,k   ! Loop counters
   integer        :: ios     ! I/O status from file operations
   character(180) :: message ! Error message to pass to Write_Log
   integer        :: nVals   ! No. of Tau/Re/Satzen etc values in file

   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTEm: Error opening file ', trim(LUT_file)
      write(message, *) 'Read_LUTEm: Error opening file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   else
      SAD_LUT%table_used_for_channel(chan, IEm) = .true.

      SAD_LUT%table_uses_satzen(IEm) = .true.
      SAD_LUT%table_uses_solzen(IEm) = .false.
      SAD_LUT%table_uses_relazi(IEm) = .false.


      ! Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios) SAD_LUT%Wavelength(chan)


      ! Now get the tau values
      read(l_lun, *, err=999, iostat=ios) nVals, SAD_LUT%Grid%dTau(chan,IEm)
      SAD_LUT%Grid%nTau(chan,IEm) = nVals ! Used for loop control later

      if (chan == Ctrl%Ind%ThermalFirst) then
         if (.not.associated(SAD_LUT%Grid%Tau)) then
            allocate(SAD_LUT%Grid%Tau(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxtau,maxcrprops))
         endif
      endif
      SAD_LUT%Grid%Tau(chan,:,IEm) = 0.0

      read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(chan,i,IEm), i=1,nVals)

      SAD_LUT%Grid%MinTau(chan,IEm) = SAD_LUT%Grid%Tau(chan,1,IEm)
      SAD_LUT%Grid%MaxTau(chan,IEm) = SAD_LUT%Grid%Tau(chan,nVals,IEm)


      ! Now get the satzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen(chan,IEm)
         SAD_LUT%Grid%nSatzen(chan,IEm) = nVals

         if (chan == Ctrl%Ind%ThermalFirst) then
            if (.not.associated(SAD_LUT%Grid%satzen)) then
               allocate(SAD_LUT%Grid%satzen(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxsatzen,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%satzen(chan,:,IEm) = 0.0

         read(l_lun, *, err=999, iostat=ios) (SAD_LUT%Grid%Satzen(chan,i,IEm), i=1,nVals)

         SAD_LUT%Grid%MinSatzen(chan,IEm) = SAD_LUT%Grid%Satzen(chan,1,IEm)
         SAD_LUT%Grid%MaxSatzen(chan,IEm) = SAD_LUT%Grid%Satzen(chan,nVals,IEm)
      endif


      ! Now get the effective radius values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe(chan,IEm)
	 SAD_LUT%Grid%nRe(chan,IEm) = nVals

         if (chan == Ctrl%Ind%ThermalFirst) then
            if (.not.associated(SAD_LUT%Grid%Re)) then
               allocate(SAD_LUT%Grid%Re(Ctrl%Ind%Ny,SAD_LUT%Grid%nmaxre,maxcrprops))
            endif
         endif
         SAD_LUT%Grid%Re(chan,:,IEm) = 0.0
         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(chan,i,IEm), i=1,nVals)
         SAD_LUT%Grid%MinRe(chan,IEm) = SAD_LUT%Grid%Re(chan,1,IEm)
         SAD_LUT%Grid%MaxRe(chan,IEm) = SAD_LUT%Grid%Re(chan,nVals,IEm)
      endif


      ! Read in the Em array
      if (Ctrl%Ind%NThermal > 0 .and. chan == Ctrl%Ind%ThermalFirst) then
         allocate(SAD_LUT%Em(Ctrl%Ind%Ny, SAD_LUT%Grid%Nmaxtau, SAD_LUT%Grid%nmaxsatzen, &
            SAD_LUT%Grid%nmaxre))
         SAD_LUT%Em = 0.0
      endif

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
            (((SAD_LUT%Em(chan, i, j, k), i=1, SAD_LUT%Grid%nTau(chan,IEm)), &
            j=1, SAD_LUT%Grid%nSatzen(chan,IEm)), k=1, SAD_LUT%Grid%nRe(chan,IEm))
      endif


      close(unit=l_lun)
   endif

999 if (ios /= 0) then
      status = LUTFileReadErr
      write(*,*)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
      stop
   endif

end subroutine Read_LUT_Em


!-------------------------------------------------------------------------------
! Name:
!    Read_LUT
!
! Purpose:
!    Controls the reading of Look Up Table values from files into the SAD_LUT
!    array of structs. There are seperate LUTs for different LUT values and
!    channel indices.
!
! Arguments:
!    Name     Type   In/Out/Both Description
!    Ctrl     struct In          Control structure passed to Write_Log for log
!                                file name
!    l_lun    int    in          Unit number for LUT file
!    LUT_file string in          LUT file name
!    chan     int    in          Current channel number used for array indexing
!                                within the SAD_LUT struct, e.g.
!                                SAD_LUT%Wavelength for checking against LUT
!                                file data.
!    SAD_LUT  struct out         Structure to hold the values from the LUT file.
!                                N.B. this is one struct from the array used
!                                elsewhere, not the whole array.
!    status   int    out         Status value returned by all ECP routines.

! Algorithm:
!    Find a logical unit number to be used for each LUT file in turn for each
!    cloud class
!       allocate the LUT arrays (To match no. of channels selected. All other
!          dimensions are set to the max. allowed)
!       for each channel
!          work out the Rd (and Rfd) LUT file name
!          call the Rd LUT file read function
!          work out the Td (and Tfd) LUT file name
!          call the Td LUT file read function
!          if (SAD_Chan solar flag is set)
!             work out the Rbd LUT file name
!             call the Rbd LUT file read function
!             work out the Tb LUT file name
!             call the Tb LUT file read function
!             work out the Tbd (and Tfbd) LUT file name
!             call the Tbd LUT file read function
!          if (SAD_Chan thermal flag is set)
!             work out the Em LUT file name
!             call the Em LUT file read function
!    N.B. it is possible for a channel to have both solar and thermal
!    attributes, hence two "if" statements rather than "if... else"
!
! Local variables:
!    Name Type Description
!
! History:
!    13th Oct 2000, Andy Smith: original version
!    23rd Nov 2000, Andy Smith:
!       Channel file names updated: using 'Ch' instead of 'CH'
!     9th Jan 2001, Andy Smith:
!       Emissivity files available. Read_LUT_EM call un-commented.
!       Added breakpoint output.
!       Ctrl%Ind%Y renamed Y_Id
!    12th Jan 2001, Andy Smith:
!       Arrays of LUT values (RBd etc) made allocatable. Allocate sizes here.
!    18th Jan 2001, Andy Smith:
!       Bug fix in array allocation. Rfd, TFd arrays must always be allocated
!       even if the choice of channels means they're unused, because they are
!       read from the same files as Rd, Td by the same routines.
!     9th Feb 2001, Andy Smith:
!       Using pre-defined constants (ECPConstants.f90) for breakpoint levels.
!     1st Mar 2001, Andy Smith:
!       LUT array values are now divided by 100 since values in files are
!       percentages and we require fractions later on.
!       (Temporary fix until files are re-written?)
!     7th Jun 2001, Andy Smith:
!       Debug log message removed from routine Read_LUT_Rbd
!    **************** ECV work starts here *************************************
!    22nd Mar 2011, Andy Smith:
!       Remove phase change, phase 2 only 1 cloud class per run.
!       SAD_LUT is also now reduced from dimension N cloud classes to 1.
!     6th Apr 2011, Andy Smith:
!       Removed two redundant breakpoint outputs now that only 1 cloud class.
!    3rd May 2011, Andy Smith:
!       Extension to multiple instrument views. Wavelength array is now allocated.
!       Added wavelength to breakpoint outputs to allow checking when >1 view
!       selected.
!    3rd May 2011, Caroline Poulsen: removed allocation of LUTs into individual
!      routine so ntau,nrensatzen etc could be read and used directly from LUT
!      files and are not replicated else where
!    16th Jan 2014, Greg McGarragh:
!       Added allocation of SAD_LUT%table_use* arrays.
!    23th Jan 2014, Greg McGarragh:
!       Cleaned up code.
!20140204: MJ implements code for AVHRR to assign channel numbers for LUT names.
!
! Bugs:
!   None known.
!
!-------------------------------------------------------------------------------

subroutine Read_LUT(Ctrl, SAD_Chan, SAD_LUT, status)

   use CTRL_def
   use SAD_Chan_def

   implicit none

   ! Argument declarations

   type(CTRL_t),                   intent(in)    :: Ctrl
   type(SAD_Chan_t), dimension(:), intent(in)    :: SAD_Chan
   type(SAD_LUT_t),                intent(inout) :: SAD_LUT
   integer,                        intent(inout) :: status

   ! Local variables

   integer                :: j       ! Array counters
   character(FilenameLen) :: LUT_file! Name of LUT file
   character(4)           :: chan_num! Channel number converted to a string
   integer                :: l_lun   ! Unit number for LUT file
#ifdef BKP
   integer                :: bkp_lun ! Unit number for breakpoint file
   integer                :: ios     ! I/O status returned by file open etc
#endif

!  Open breakpoint file if required.

#ifdef BKP
   if (Ctrl%Bkpl > 0) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
	   status='old',      &
	   position='append', &
	   iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
	 call Write_Log(Ctrl, 'Read_LUT: Error opening breakpoint file', status)
      else
         write(bkp_lun,*)'Read_LUT:'
      endif
   endif
#endif

   ! Call find lun here and pass l_lun to subroutines, rather than calling once
   ! per file to be read.

   call Find_LUN(l_lun)

   ! For each cloud class, construct the LUT filename from the instrument name,
   ! cloud class ID, variable name and channel number. Then call the appropriate
   ! LUT file read function. Just pass the current SAD_LUT struct, rather than
   ! the whole array.

   if (status == 0) then

      ! Allocate SAD_LUT%Grid%Tau the LUT arrays in SAD_LUT. All arrays are
      ! allocated big enough to hold the total number of channels selected, even
      ! though not all arrays hold both thermal and solar data. This makes it
      ! easier to keep track of where each channel's data is. All other
      ! dimensions (tau etc) are set to the max. possible size as nTau etc can
      ! vary with channel number.

      if (.not.associated(SAD_LUT%Wavelength)) &
         allocate(SAD_LUT%Wavelength(Ctrl%Ind%Ny))
      SAD_LUT%Wavelength = 0.0

      !allocate some arrays:
      allocate(SAD_LUT%Grid%MaxTau(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%MinTau(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%dTau(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%nTau(Ctrl%Ind%Ny,maxcrprops))
      SAD_LUT%Grid%nTau=SAD_LUT%Grid%nmaxtau
      allocate(SAD_LUT%Grid%MaxRe(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%MinRe(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%dRe(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%nRe(Ctrl%Ind%Ny,maxcrprops))
      SAD_LUT%Grid%nRe=SAD_LUT%Grid%nmaxre
      allocate(SAD_LUT%Grid%MaxSatzen(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%MinSatzen(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%dSatzen(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%nSatzen(Ctrl%Ind%Ny,maxcrprops))
      SAD_LUT%Grid%nSatzen=SAD_LUT%Grid%nmaxsatzen
      allocate(SAD_LUT%Grid%MaxSolzen(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%MinSolzen(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%dSolzen(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%nSolzen(Ctrl%Ind%Ny,maxcrprops))
      SAD_LUT%Grid%nSolzen=SAD_LUT%Grid%nmaxsolzen
      allocate(SAD_LUT%Grid%MaxRelazi(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%MinRelazi(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%dRelazi(Ctrl%Ind%Ny,maxcrprops))
      allocate(SAD_LUT%Grid%nRelazi(Ctrl%Ind%Ny,maxcrprops))
      SAD_LUT%Grid%nRelazi=SAD_LUT%Grid%nmaxrelazi

      allocate(SAD_LUT%table_used_for_channel(Ctrl%Ind%Ny, maxcrprops))

      SAD_LUT%table_used_for_channel = .false.

      SAD_LUT%table_uses_satzen = .false.
      SAD_LUT%table_uses_solzen = .false.
      SAD_LUT%table_uses_relazi = .false.

      do j=1, Ctrl%Ind%Ny
         if (Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) < 10) then
            if(trim(Ctrl%Inst%Name(1:5)) .ne. 'AVHRR') then
               write(chan_num, '(a2,i1)') 'Ch',Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j))
            else
               if(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) .eq. 1) then
                  chan_num='Ch1'
               elseif(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) .eq. 2) then
                  chan_num='Ch2'
               elseif(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) .eq. 3) then
                  chan_num='Ch3a'
               elseif(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) .eq. 4) then
                  chan_num='Ch3b'
               elseif(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) .eq. 5) then
                  chan_num='Ch4'
               elseif(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) .eq. 6) then
                  chan_num='Ch5'
               endif

            endif
         else
            write(chan_num, '(a2,i2)') 'Ch',Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j))
         end if
         write(*,*) 'Channel number',trim(adjustl(chan_num))


         ! Read Rd, Td files for all channels (solar and thermal)
         LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name) &
                    // '_' // trim(Ctrl%CloudClass%Name) // '_RD_' &
                    // trim(chan_num) // '.sad'

         call Read_LUT_RD(Ctrl, l_lun, LUT_file, j, SAD_LUT, status)
         write(*,*) 'LUT_RD read',status

         if (status == 0) then
            LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name) &
                       // '_' // trim(Ctrl%CloudClass%Name) // '_TD_' &
                       // trim(chan_num) // '.sad'

            call Read_LUT_TD(Ctrl, l_lun, LUT_file, j, SAD_LUT, status)
            write(*,*) 'LUT_TD read',status
         endif


         ! Read solar channel attributes (this is from readchan)
         if (SAD_Chan(j)%Solar%Flag > 0) then

            ! Generate the Rbd file name for this channel and cloud class, and
            ! call the Rbd read function.

            if (status == 0) then
               LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name) &
                          // '_' // trim(Ctrl%CloudClass%Name) // '_RBD_' &
                          // trim(chan_num) // '.sad'

               call Read_LUT_RBD(Ctrl, l_lun, LUT_file, j, SAD_LUT, status)
               write(*,*) 'LUT_RBD read',status
            endif

            ! Read the Tb data

            if (status == 0) then
               LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name) &
                          // '_' // trim(Ctrl%CloudClass%Name) // '_TB_' &
                          // trim(chan_num) // '.sad'

               call Read_LUT_Tb(Ctrl, l_lun, LUT_file, j, SAD_LUT, status)
               write(*,*) 'LUT_Tb read',status
            endif

            ! Read the Tbd/TFbd data (N.B. Tbd is not kept)

            if (status == 0) then
               LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name) &
                          // '_' // trim(Ctrl%CloudClass%Name) // '_TBD_' &
                          // trim(chan_num) // '.sad'

               call Read_LUT_Tbd(Ctrl, l_lun, LUT_file, j, SAD_LUT, status)
               write(*,*) 'LUT_Tbd read',status
            endif
         endif ! End of "solar" actions


         ! Read thermal channel attributes
         if (status == 0) then
            if (SAD_Chan(j)%Thermal%Flag > 0) then

               ! Read the Em data
               LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name) &
                          // '_' // trim(Ctrl%CloudClass%Name) // '_EM_' &
                          // trim(chan_num) // '.sad'

               call Read_LUT_Em(Ctrl, l_lun, LUT_file, j, SAD_LUT, status)
               write(*,*) 'LUT_Em read',status
            endif
         endif


         if (status /= 0) then
            write(*,*) 'Failed to process Lut for index',j
            stop
            exit ! Drop out if an open or read error occurred
         endif
      end do

#ifdef BKP
      if (Ctrl%Bkpl >= BkpL_Read_LUT_1) then
         ! Write out SAD_LUT Name and Wavelength
         write(bkp_lun, *)'Name in cloud class struct: ', &
              Ctrl%CloudClass%Name
      endif

      if (Ctrl%Bkpl >= BkpL_Read_LUT_2) then
         ! Write out SAD_LUT Grid substructs.
         write(bkp_lun,*)'Wavelengths: ',(SAD_LUT%Wavelength(i),i=1,Ctrl%Ind%Ny)

         write(bkp_lun,*)'Max, min, delta Tau:',SAD_LUT%Grid%MaxTau, &
              SAD_LUT%Grid%MinTau, SAD_LUT%Grid%dTau
         write(bkp_lun,'(a, 9(f6.3, 1x),/)') ' Tau: ', &
              (SAD_LUT%Grid%Tau(k), k=1,SAD_LUT%Grid%nTau)

         write(bkp_lun,*)'Max, min, delta Re:',SAD_LUT%Grid%MaxRe, &
              SAD_LUT%Grid%MinRe, SAD_LUT%Grid%dRe
         write(bkp_lun,'(a, 12(f7.1, 1x),/)') ' Re: ', &
              (SAD_LUT%Grid%Re(k), k=1,SAD_LUT%Grid%nRe)

         write(bkp_lun,*)'Max, min, delta SatZen:', &
              SAD_LUT%Grid%MaxSatZen, &
              SAD_LUT%Grid%MinSatZen, SAD_LUT%Grid%dSatZen
         write(bkp_lun,'(a, 10(f6.1, 1x),/)') ' SatZen: ', &
              (SAD_LUT%Grid%SatZen(k), k=1,SAD_LUT%Grid%nSatZen)

         write(bkp_lun,*)'Max, min, delta SolZen:',SAD_LUT%Grid%MaxSolZen, &
              SAD_LUT%Grid%MinSolZen, SAD_LUT%Grid%dSolZen
         write(bkp_lun,'(a, 10(f6.1, 1x),/)') ' SolZen: ', &
              (SAD_LUT%Grid%SolZen(k), k=1,SAD_LUT%Grid%nSolZen)

         write(bkp_lun,*)'Max, min, delta RelAzi:',SAD_LUT%Grid%MaxRelAzi, &
              SAD_LUT%Grid%MinRelAzi, SAD_LUT%Grid%dRelAzi
         write(bkp_lun,'(a, 11(f6.1, 1x),/)') ' RelAzi: ', &
              (SAD_LUT%Grid%RelAzi(k), k=1,SAD_LUT%Grid%nRelAzi)
      endif
#endif
   endif

   !Convert from percentage to fractional values
   if (status == 0) then
      SAD_LUT%Rd(:,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen, &
           & 1:SAD_LUT%Grid%nmaxRe) &
           & = SAD_LUT%Rd(:,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen, &
           & 1:SAD_LUT%Grid%nmaxRe) / 100.

      SAD_LUT%Td(1:Ctrl%Ind%Ny,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen,1:SAD_LUT%Grid%nmaxRe) = &
           & SAD_LUT%Td(1:Ctrl%Ind%Ny,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen,1:SAD_LUT%Grid%nmaxRe) / &
           & 100.

      SAD_LUT%Tfd(:,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxRe) &
           & = SAD_LUT%Tfd(:,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxRe) / 100.

      SAD_LUT%Rfd(:,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxRe) &
           & = SAD_LUT%Rfd(:,1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxRe) / 100.

      if (Ctrl%Ind%NSolar > 0) then
         SAD_LUT%Rbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,         &
              & 1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen,    &
              & 1:SAD_LUT%Grid%nmaxSolZen,1:SAD_LUT%Grid%nmaxRelAzi, &
              & 1:SAD_LUT%Grid%nmaxRe) =                             &
              & SAD_LUT%Rbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,  &
              & 1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen,    &
              & 1:SAD_LUT%Grid%nmaxSolZen,1:SAD_LUT%Grid%nmaxRelAzi, &
              & 1:SAD_LUT%Grid%nmaxRe) / 100.

         SAD_LUT%Tbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,         &
              & 1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen,    &
              & 1:SAD_LUT%Grid%nmaxSolZen,1:SAD_LUT%Grid%nmaxRelAzi, &
              & 1:SAD_LUT%Grid%nmaxRe) =                             &
              & SAD_LUT%Tbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,  &
              & 1:SAD_LUT%Grid%nmaxTau,1:SAD_LUT%Grid%nmaxSatZen,    &
              & 1:SAD_LUT%Grid%nmaxSolZen,1:SAD_LUT%Grid%nmaxRelAzi, &
              & 1:SAD_LUT%Grid%nmaxRe) / 100.

         SAD_LUT%Tb(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,          &
              & 1:SAD_LUT%Grid%nmaxTau, 1:SAD_LUT%Grid%nmaxSolZen,   &
              & 1:SAD_LUT%Grid%nmaxRe) =                             &
              & SAD_LUT%Tb(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,   &
              & 1:SAD_LUT%Grid%nmaxTau, 1:SAD_LUT%Grid%nmaxSolZen,   &
              & 1:SAD_LUT%Grid%nmaxRe) / 100.

         SAD_LUT%Tfbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,        &
              & 1:SAD_LUT%Grid%nmaxTau, 1:SAD_LUT%Grid%nmaxSolZen,   &
              & 1:SAD_LUT%Grid%nmaxRe) =                             &
              & SAD_LUT%Tfbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast, &
              & 1:SAD_LUT%Grid%nmaxTau, 1:SAD_LUT%Grid%nmaxSolZen,   &
              & 1:SAD_LUT%Grid%nmaxRe) / 100.
      endif

      if (Ctrl%Ind%NThermal > 0) then
         SAD_LUT%Em(Ctrl%Ind%ThermalFirst:Ctrl%Ind%ThermalLast,        &
              & 1:SAD_LUT%Grid%nmaxTau, 1:SAD_LUT%Grid%nmaxSatZen,     &
              & 1:SAD_LUT%Grid%nmaxRe) =                               &
              & SAD_LUT%Em(Ctrl%Ind%ThermalFirst:Ctrl%Ind%ThermalLast, &
              & 1:SAD_LUT%Grid%nmaxTau, 1:SAD_LUT%Grid%nmaxSatZen,     &
              & 1:SAD_LUT%Grid%nmaxRe) / 100.
      endif
   endif

#ifdef BKP
   if (Ctrl%Bkpl > 0) then
      write(bkp_lun, *) 'Read_LUT: end ----------'
      close(unit=bkp_lun)
   endif
#endif

end subroutine Read_LUT
