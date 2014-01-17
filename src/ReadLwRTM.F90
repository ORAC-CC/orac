! Name:
!    Read_LwRTM
!
! Purpose:
!    Reads the longwave Radiative Transfer Model (atmospheric) file and loads 
!    appropriate data arrays. 
!
! Arguments:
!    Name     Type           In/Out   Description
!    Ctrl     struct         Both     Control structure
!    RTM      alloc struct   Out      RTM structure
!    status   int            Out      Error status
!
! Algorithm:
!   Open RTM data file (report errors)
!   Read instrument name and check vs. name in Ctrl struct (report errors)
!   Read date
!   Read number of latitude and longitude points
!   Read number of pressure levels and number of thermal channels
!   Allocate size of Channel ID and Wave Number arrays
!   Read array of channel IDs
!   Read array of channel wavenumbers
!   Check channel IDs vs. requested Ids in Ctrl 
!      set up index array to handle selection of required channel data from
!      input data (since order of storage of different channel values in the 
!      file may not match the order in the ECP)
!   (report error if not all thermal chans requested are stored in the RTM file)
!   
!   Use no. of latitude, longitude, pressure levels from file plus no. of 
!   requested thermal channels to allocate RTM arrays
!
!   for each latitude band (1 to NLat) 
!      for each longitude (1 to NLon) 
!         read the latitude and longitude values
!         read the emissivity value for each channel
!         read the pressure dependent LW RTM data for all channels and 
!            pressure values at this lat & lon
!         use the channel index array set earlier to assign the RTM data to
!         the arrays in the RTM%LW struct
!   
!   close the RTM file
!   open the profile file (report errors)
!      read date (check vs. date from RTM file: report errors)
!      read no. of lat and lon points (check vs. RTM file values: report errors)
!      allocate RTM%LW pressure and temp. arrays
!
!      for each latitude
!         for each longitude
!            read lat, lon and no. of pressure levels
!            change sign of longitude value
!            check lat and lon agree with RTM data: report errors
!            check no. of pressure values agrees with RTM data: report errors
!            read in block of pressure and temp. values and assign to arrays
!            in RTM%LW
!            
!   close the profile file
! 
!   set up grid parameters for use in Get_LwRTM: 
!      lat and lon valus at corners of grid
!      min and max latitude and longitude values
!      step size on lat and lon grids (and reciprocals)  
!
! Local variables:
!   Name         Type        Description
!   ios          int         I/O status, file operations
!   lun          int         File unit number 
!   message      char        Error message to pass to Write_Log  
!   InstName     char        Instrument name
!   Date         char        Date
!   NChan        int         Number of channels in RTM
!   ChanID       alloc int   Channel identifiers
!   WvNumber  alloc real(8)  Central wavenumber
!   ilat         int         Loop counter over latitudes
!   ilon         int         Loop counter over longitudes
!   x            real(4)     dummy variable for reading latitude value
!   y            real(4)     dummy variable for reading longitude value
!   klat         int         Loop counter over latitudes
!   klon         int         Loop counter over longitudes
!   index        int         Index to channel
!   i,j,k        int         Loop counters
!   chan_found   int         Flag to indicate channel has been found
!   dummy        char        Dummy string
!   dummy_len    int         Length of dummy string (excluding end spaces)
!   buf(:,:,:) alloc real(5) Dummy array for read. Dimensions are 
!                            no. of channels, no. of pressure levels, 
!                            no. of parameters in LwRTM file (or PRTM file).
!   bufe(:)    alloc real(4) Dummy array for read, used for reading in 
!                            emissivity values.
!
! History:
!   5th December, 2000, Kevin M. Smith : original version
!  15th January,  2001, KMS : Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
!  17th January,  2001, KMS : Corrected indexing of RTM%LW%Lat and Lon from
!                             1-D to 2-D array
!  25th January,  2001, KMS : Corrected calculation of LatN and LonN in
!                             RTM%LW%Grid.
!  21st Feb 2001, Andy Smith: 
!      Added Tbc to LW structure. Previously missing from model data.
!   1st Mar 2001, Andy Smith: 
!     Removed allocation of R_Clear in LW RTM struct. R_Clear not available
!     from RTM data file.
!  30th Mar 2001, Andy Smith:
!     Added setting of new grid variables MaxLat, MinLat, MaxLon, MinLon.
!     Avoids repeated re-calculation in GetRTM. 
!  22nd Jun 2001, Andy Smith:
!     Updated header comments and added argument intent.
!  24th Oct 2001, Andy Smith:
!     Added deallocation of local allocatable arrays.
!     Removed change of sign on longitude values. Data should be supplied on a
!     grid with west -ve.
!  *** ECV work starts here ***
!   7th Feb 2011, Andy Smith:
!     Re-applying changes made in 2002. 
!     Converted to unformatted read:
!     - variables x, y, buf and bufe are declared as real(4) in order to match
!       the number of bytes used for reals in the RTM files (previously 8).
!     - Allocation of buffer array "buf" changed from 7 paramaters to 5, since
!       whole array writes to binary file mean that the level and channel 
!       indices are no longer present.
!     - Array dimensions in buf swapped round to make access more efficient.
!       Channel is now the first index, then pressure level, and parameter last.
!     Error checking improved: iostat values checked.
!     Added tests for allocation status before deallocation of local 
!     allocatable arrays (may not be allocated if errors detected before
!     allocation).
!     Date changed to character length 8 (YYYYMMDD) instead of 10.
!   (19th September 2002 Caroline Poulsen, bug found, changed the deltalat 
!    and deltalon
!   12th Decmber 2002 Caroline Poulsen now read geopotential height
!     from profile file)
!   15th Feb 2011, Andy Smith:
!     Character string "dummy" length changed from 10 to 8, otherwise read 
!     error occurs on prtm file. 
!   7th Nov 2011 C .poulsen tidied up comments but no aculacode change
!   20/01/2012 C. Poulsen changed reading of buf prtm array
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Read_LwRTM(Ctrl, RTM, status)

   use CTRL_def
   use RTM_def
   use ECP_Constants

   implicit none

!  Argument declarations

   type(CTRL_t), intent(in)  :: Ctrl
   type(RTM_t), intent(out)  :: RTM
   integer, intent(inout)      :: status

!  Local variables
!  Note on values read from the binary Lw and P RTM files. These files are
!  generated by RTTOV code, which is compiled with a flag to force reals to
!  become real(8). The parameter arrays read in via buf, and the lat, longs
!  etc are explicitly written as real(4) in order to reduce the file size.

   integer                :: ios
   integer                :: lun
   character(180)         :: message
   character(Instnamelen) :: InstName
   character(8)           :: Date
   integer                :: NChan   
   integer, allocatable   :: ChanID(:)
   real(4), allocatable   :: WvNumber(:)  
   integer                :: ilat
   integer                :: ilon
   real(4)                :: x
   real(4)                :: y
   integer                :: klat
   integer                :: klon
   integer, allocatable   :: index(:)   
   integer                :: i, j, k
   integer                :: chan_found
   character(8)           :: dummy
   integer                :: dummy_len
   real(4), allocatable   :: buf(:,:,:)
   real(4), allocatable   :: bufprtm(:,:)
   real(4), allocatable   :: bufe(:)

!  Call Find_LUN to set suitable unit number for LW RTM data file

   call Find_LUN(lun)

!  Open RTM data file


   open(unit=lun, file=Ctrl%Fid%LWRTM, form='unformatted', status='old', &
      iostat=ios)
   if (ios /= 0) then
      status = LwRTMRTMFileOpenErr ! Return error code
      write(unit=message, fmt=*) 'Read_LwRTM: Error opening RTM file ', trim(adjustl(Ctrl%Fid%LWRTM))
      call Write_Log(Ctrl, trim(message), status)
   else

!     Read RTM data file

!     Instrument
      read(unit=lun,iostat=ios) InstName
!      InstName='MODIS-AQUA'
!      ios=0
!      write(*,*) 'IOS',ios
      if (ios /= 0) then
         status = LwRTMRTMInstErr ! Return error code
         call Write_Log(Ctrl, 'Read_LwRTM: error reading instrument name', &
            status)
!         write(*,*) 'hier0'
!         write(*,*) InstName
!         write(*,*) Ctrl%Inst%Name
!         pause
   else

!         write(*,*) InstName
!         write(*,*) Ctrl%Inst%Name
!         pause
      !      InstName='MODIS-AQUA'
      InstName = adjustl(InstName)
         if (trim(adjustl(InstName)) /= trim(adjustl(Ctrl%Inst%Name))) then
            status = LwRTMRTMInstErr ! Return error code
            call Write_Log(Ctrl, &
               'Read_LwRTM: RTM file; header instrument disagrees with filename',&
               status)
!            write(*,*) 'hier1'
         end if
      end if

!      write(*,*) 'hier2'
!     Date
      
      

      if (status == 0) then      
         read(unit=lun, iostat=ios) Date
!         Date='20080620'
!         ios=0
!         write(*,*) Date
         if (ios /= 0) then
            status = LwRTMRTMDateErr ! Return error code
            call Write_Log(Ctrl, 'Read_LwRTM: error reading date', &
               status)
         end if   
!	 if (trim(Date) /= trim(Ctrl%Inst%Date)) then
!	     status = LwRTMRTMDateErr ! Return error code
!            write(unit=message, fmt=*) &
!                 'Read_LwRTM: RTM file; header date disagrees with filename' 
!            call Write_Log(Ctrl, trim(message), status)
!	 end if
      end if

!     For this set of reads, if read fails (non-zero iostat), set status
!     immendiately but report error later since the message is the same for
!     all cases.

!     Number of latitude and longitude points

      if (status == 0) then
         read(unit=lun, iostat=ios) RTM%LW%Grid%NLat, RTM%LW%Grid%NLon
         if (ios /= 0) status = LwRTMReadErr
      end if

!     Number of pressure levels and number of thermal channels

      if (status == 0) then 
         read(unit=lun, iostat=ios) RTM%LW%NP, NChan
         if (ios /= 0) status = LwRTMReadErr
      end if

!     Allocate size of ChanID and WvNumber
      if (status == 0) then 
         allocate(ChanID(NChan))
         allocate(WvNumber(NChan))

!        Read ChanID and WvNumber

         read(unit=lun, iostat=ios) ChanID
         if (ios /= 0) status = LwRTMReadErr
      end if
            
      if (status == 0) then 
         read(unit=lun, iostat=ios) WvNumber
         if (ios /= 0) status = LwRTMReadErr
      end if
	
!     Check that required thermal channels are present

      if (status == 0) then
!        Initialise counter

         chan_found = 0
	 allocate(index(Ctrl%Ind%NThermal))
	 index(:) = 0
	 k = 1

!        Loop over longwave instrument channels, checking that requested
!        channels are available in the RTM data and setting up the index array
!        to allow us to find the channels we want from the RTM file data (in
!        case order of storage is different from the order we specified our
!        selection).

         do i = Ctrl%Ind%ThermalFirst, Ctrl%Ind%ThermalLast
!           Loop over channels in RTM
            do j = 1, NChan
!              Signal that the required channel has been found by incrementing
!              chan_found and break out of the inner loop to start search for
!              next instrument channel

               if (Ctrl%Ind%Y_Id(i) == ChanID(j)) then
                  chan_found = chan_found + 1
		  index(k) = j
		  k = k+1
                  exit
               end if
            end do
         end do


         if (chan_found /= Ctrl%Ind%NThermal) then
            status = LwRTMChanErr ! Return error code
            write(unit=message, fmt=*) &
               'Read_LwRTM: RTM file; required instrument channels not found' 
            call Write_Log(Ctrl, trim(message), status)
         end if

      end if  ! End of "required thermal channels" status check
   
      if (status == 0) then 
!        Allocate sizes of arguments in call to Read_LwRTM

         allocate(RTM%LW%bs(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, Ctrl%Ind%NThermal))
         allocate(RTM%LW%ems(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, Ctrl%Ind%NThermal))
         allocate(RTM%LW%Tbc(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, Ctrl%Ind%NThermal, RTM%LW%NP))
         allocate(RTM%LW%Tac(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, Ctrl%Ind%NThermal, RTM%LW%NP))
         allocate(RTM%LW%Rac_up(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, Ctrl%Ind%NThermal, RTM%LW%NP))
         allocate(RTM%LW%Rac_dwn(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, Ctrl%Ind%NThermal, RTM%LW%NP))
         allocate(RTM%LW%Rbc_up(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, Ctrl%Ind%NThermal, RTM%LW%NP))
         allocate(RTM%LW%lat(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))
         allocate(RTM%LW%lon(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))   
         allocate(buf(NChan, RTM%LW%NP, 5))
         allocate(bufe(NChan))

!        Read data blocks (only read the required channels)

         do ilat = 1, RTM%LW%Grid%NLat
            do ilon = 1, RTM%LW%Grid%NLon
               read(unit=lun, iostat=ios) x, y
               if (ios /= 0) exit

               RTM%LW%lat(ilat, ilon) = x
               RTM%LW%lon(ilat, ilon) = y

!              Read one row of data: emissivity values in each channel

               read(unit=lun, iostat=ios) bufe
               if (ios /= 0) exit
               
               !              Write required channels into output array		  
               do i = 1, Ctrl%Ind%NThermal
                  RTM%LW%ems(ilat, ilon, i) = bufe(index(i))
               end do

!              Read block of data containing Tac, Tbc etc for all channels
!              and pressure levels at this lat and lon
!              In the binary RTM file data is written in whole-array blocks.
!              Each block contains the 5 RTM parameters written for all 
!              Lw channels and pressure levels as follows: 
!              Taubc(1:Nchans,1:Nlevs), Tau(1:Nchans,1:Nlevs), ...

               read(unit=lun, iostat=ios) buf(:,:,:)
               if (ios /= 0) exit

               !              Write required channels into output arrays

               do i = 1, Ctrl%Ind%NThermal
                  RTM%LW%Tbc(ilat, ilon, i, :) = buf(index(i),:, 1)
                  RTM%LW%Tac(ilat, ilon, i, :) = buf(index(i),:, 2)
                  RTM%LW%Rbc_up(ilat, ilon, i, :) = buf(index(i),:, 3)
                  RTM%LW%Rac_up(ilat, ilon, i, :) = buf(index(i),:, 4)
                  RTM%LW%Rac_dwn(ilat, ilon, i, :) = buf(index(i),:, 5)
               end do
            end do
            if (ios /= 0) exit
         end do
         if (ios /= 0) write(*,*)'Error in main read loop'
         if (ios /= 0) status = LwRTMReadErr
      
!        Deallocate arrays for reuse

         if (allocated(buf)) deallocate(buf)
         if (allocated(bufe)) deallocate(bufe)

      end if  ! End of status check before main data blocks are allocated and 
              ! read in.

!     Close RTM file and deallocate allocatable arrays if not required

      if (status == LwRTMReadErr) &
         call Write_Log(Ctrl, 'Read_LwRTM: error reading from Lw file', &
            status)

      close(lun)
      if (allocated(index)) deallocate(index)
      if (allocated(WvNumber)) deallocate(WvNumber)
      if (allocated(ChanID)) deallocate(ChanID)
   
   end if  ! End of check that ios is ok on opening Lw RTM file.

   if (status==0) write(*,*)'Read LW RTM data ok'

!  Open profile file

   if (status == 0) then
      open(unit=lun, file=Ctrl%Fid%PRTM, form='unformatted', status='old', &
           iostat=ios)

      if(ios /= 0) then
         status = LwRTMPFileOpenErr
         write(unit=message, fmt=*) &
              'Read_LwRTM: Error opening RTM profile file ', trim(adjustl(Ctrl%Fid%PRTM))
         call Write_Log(Ctrl, trim(message), status)
      else
         !        Read date from header

         read(unit=lun, iostat=ios) dummy
         if (ios /= 0) then
            status = LwRTMProfDateErr
            call Write_Log(Ctrl, 'Read_LwRTM: Error reading profile file', &
               status)
         else 
            dummy_len = len(trim(dummy))
            if (dummy(1:dummy_len) /= Date) then
               status = LwRTMProfDateErr
               write(unit=message, fmt=*) &
                  'Read_LwRTM: RTM profile file; header date disagrees with RTM filename' 
               call Write_Log(Ctrl, trim(message), status)
            end if
         end if

!        Read lat. and long. points
         
         if (status == 0) then
            read(unit=lun, iostat=ios) klat, klon
            if (ios /= 0) then
               status = LwRTMProfNLatErr
               call Write_Log(Ctrl, &
                    'Read_LwRTM: error reading lat and lon points from profile file', & 
                    status)
            else 
               if (klat /= RTM%LW%Grid%NLat) then
                  status = LwRTMProfNLatErr
                  write(unit=message, fmt=*) &
                       'Read_LwRTM: RTM profile file; no. latitude points disagrees with RTM file' 
                  call Write_Log(Ctrl, trim(message), status)
               end if
               if (klon /= RTM%LW%Grid%NLon) then
                  status = LwRTMProfNLonErr ! Return error code
                  write(unit=message, fmt=*) &
                     'Read_LwRTM: RTM profile file; no. of longitude points disagrees with RTM file' 
                  call Write_Log(Ctrl, trim(message), status)
               end if
            end if      
         end if

         if (status == 0) then
!           Allocate arrays

            
	    allocate(buf(1, RTM%LW%NP, 5))
	    allocate(bufprtm( RTM%LW%NP, 5))
	    bufprtm=0.
 	    buf=0. 
            allocate(RTM%LW%P(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))
            allocate(RTM%LW%T(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))
            allocate(RTM%LW%H(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))

!           Read profile

            do ilat = 1, RTM%LW%Grid%NLat
               do ilon = 1, RTM%LW%Grid%NLon
                  read(unit=lun, iostat=ios) x, y, i
                  if (ios /= 0) exit
                  if (abs( x - RTM%LW%lat(ilat, ilon) ) > 0.1) then
                     status = LwRTMProfErr
                     write(unit=message, fmt=*) &
                        'Read_LwRTM: RTM profile file; latitude disagrees (>.1o) with RTM file'
                     call Write_Log(Ctrl, trim(message), status)
		     exit
                  end if
   
                  if (abs( y - RTM%LW%lon(ilat, ilon) ) > 0.1) then
                     status = LwRTMProfErr
                     write(unit=message, fmt=*) &
                        'Read_LwRTM: RTM profile file; longitude disagrees (>.1o) with RTM file'
                     call Write_Log(Ctrl, trim(message), status)
		     exit
                  end if
   
                  if (i /= RTM%LW%NP) then
                     status = LwRTMProfErr
                     write(unit=message, fmt=*) &
                        'Read_LwRTM: RTM profile file; # p levels at ', ilat, ilon, ' not ', RTM%LW%NP
                     call Write_Log(Ctrl, trim(message), status)
                  end if
                  if (status /= 0) exit

		  bufprtm=buf(1,:,:)

                  read(unit=lun, iostat=ios) bufprtm!(1,:,:)
                  if (ios /= 0) exit

                  RTM%LW%P(ilat, ilon, :) = bufprtm( :, 1)

                  RTM%LW%T(ilat, ilon, :) = bufprtm( :, 2)

                  RTM%LW%H(ilat, ilon, :) = bufprtm( :, 5)

               end do
	       if (status /= 0 .or. ios /= 0) exit
           end do

!           Deal with read errors that occurred inside the do loop

            if (ios /= 0) then
               status = LwRTMProfReadErr
               call Write_Log(Ctrl, 'ReadLwRTM: error reading profile file', &
                  status)
            end if

            if (allocated(buf)) deallocate(buf)      
	    if (allocated(bufprtm)) deallocate(bufprtm)      	     
         end if

      end if
!      write(*,*) 'ios6',ios
!     Close profile file

      close(lun)
!      write(*,*) 'IN',status
      if (status==0) write(*,*)'Read PRTM data ok'     
      if (status == 0) then

!        Calculate grid parameters for use in Get_LwRTM
!        Corners of the grid

         RTM%LW%Grid%Lat0 = RTM%LW%Lat(1,1)
         RTM%LW%Grid%LatN = RTM%LW%Lat(RTM%LW%Grid%NLat,1)
         RTM%LW%Grid%Lon0 = RTM%LW%Lon(1,1)
         RTM%LW%Grid%LonN = RTM%LW%Lon(1,RTM%LW%Grid%NLon)
!        Grid spacing and inverse     
         RTM%LW%Grid%delta_Lat     =   ( RTM%LW%Grid%LatN - RTM%LW%Grid%Lat0 ) & 
                                          / (RTM%LW%Grid%NLat-1)
         RTM%LW%Grid%inv_delta_Lat = 1 / RTM%LW%Grid%delta_Lat
         RTM%LW%Grid%delta_Lon     =   ( RTM%LW%Grid%LonN - RTM%LW%Grid%Lon0 ) &
                                          / (RTM%LW%Grid%NLon-1)
         RTM%LW%Grid%inv_delta_Lon = 1 / RTM%LW%Grid%delta_Lon
   
!        Max and Min grid values

         RTM%LW%Grid%MinLat = min(RTM%LW%Grid%Lat0, RTM%LW%Grid%LatN)
         RTM%LW%Grid%MaxLat = max(RTM%LW%Grid%Lat0, RTM%LW%Grid%LatN)
         RTM%LW%Grid%MinLon = min(RTM%LW%Grid%Lon0, RTM%LW%Grid%LonN)
         RTM%LW%Grid%MaxLon = max(RTM%LW%Grid%Lon0, RTM%LW%Grid%LonN)
     
      end if

   end if

end subroutine Read_LwRTM
