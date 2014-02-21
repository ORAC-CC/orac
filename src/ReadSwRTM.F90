! Name:
!    Read_SwRTM
!
! Purpose:
!    Reads the shortwave Radiative Transfer Model (atmospheric) file and loads 
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
!   Read number of pressure levels and number of solar channels
!   Allocate size of Channel ID and Wave Number arrays
!   Read array of channel IDs
!   Read array of channel wavenumbers
!   Check channel IDs vs. requested Ids in Ctrl 
!      set up index array to handle selection of required channel data from
!      input data (since order of storage of different channel values in the 
!      file may not match the order in the ECP)
!   (report error if not all solar chans requested are stored in the RTM file)
!   
!   Use no. of latitude, longitude, pressure levels from file plus no. of 
!   requested solar channels to allocate RTM arrays
!
!   for each latitude band (1 to NLat) 
!      for each longitude (1 to NLon) 
!         read the latitude and longitude values
!         read the pressure dependent SW RTM data for all channels and 
!            pressure values at this lat & lon
!         use the channel index array set earlier to assign the RTM data to
!         the arrays in the RTM%SW struct
!   
!   close the RTM file
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
!   buf(:,:,:) alloc real(2) Dummy array for read. Dimensions are 
!                            no. of channels, no. of pressure levels, 
!                            no. of parameters in LwRTM file (or PRTM file).

!
! History:
!   5th December, 2000, Kevin M. Smith : original version
!  15th January,  2001, KMS : Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
!  17th January,  2001, KMS : Corrected indexing of RTM%SW%Lat and Lon from
!                             1-D to 2-D array
!  25th January,  2001, KMS : Corrected calculation of LatN and LonN in
!                             RTM%SW%Grid.
!  21st Feb 2001, Andy Smith: 
!      Added Tbc to SW structure. Previously missing from model data.
!   1st Mar 2001, Andy Smith: 
!     Removed allocation of R_Clear in SW RTM struct. R_Clear not available
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
!  22nd September Caroline Poulsen modified the lwrtm code to read in
!          the shortwave rtm which is now scene dependant and not fixed for each latitude band.
!    20th Jan 2012 general tidy up
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Read_SWRTM_nc(Ctrl, RTM, status)

   use CTRL_def
   use RTM_def
   use ECP_Constants

   implicit none

!  Argument declarations

   type(CTRL_t), intent(inout)  :: Ctrl
   type(RTM_t), intent(inout)  :: RTM
   integer, intent(inout)      :: status

!  Local variables
!  Note on values read from the binary Sw and P RTM files. These files are
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
   integer, allocatable   :: index(:)   
   integer                :: i, j, k
   integer                :: chan_found
   character(8)           :: dummy
   real(4), allocatable   :: buf(:,:,:)
 


!  Call Find_LUN to set suitable unit number for SW RTM data file

   call Find_LUN(lun)

!  Open RTM data file

   open(unit=lun, file=Ctrl%Fid%SWRTM, form='unformatted', status='old', &
      iostat=ios)
   if (ios /= 0) then
      status = SwRTMRTMFileOpenErr ! Return error code
      write(unit=message, fmt=*) 'Read_SwRTM: Error opening RTM file ', Ctrl%Fid%SWRTM 
      call Write_Log(Ctrl, trim(message), status)
   else

!     Read RTM data file

!     Instrument

      read(unit=lun,iostat=ios) InstName
      if (ios /= 0) then
         status = SwRTMRTMInstErr ! Return error code
         call Write_Log(Ctrl, 'Read_SwRTM: error reading instrument name', &
            status)
      else
         InstName = adjustl(InstName)
         if (trim(InstName) /= trim(Ctrl%Inst%Name)) then
            status = SwRTMRTMInstErr ! Return error code
            call Write_Log(Ctrl, &
               'Read_SwRTM: RTM file; header instrument disagrees with filename',&
               status)
         end if
      end if

!     Date

      if (status == 0) then      
         read(unit=lun, iostat=ios) Date
         if (ios /= 0) then
            status = SwRTMRTMDateErr ! Return error code
            call Write_Log(Ctrl, 'Read_SwRTM: error reading date', &
               status)
         end if   
!	 if (trim(Date) /= trim(Ctrl%Inst%Date)) then
!	     status = SwRTMRTMDateErr ! Return error code
!            write(unit=message, fmt=*) &
!                 'Read_SwRTM: RTM file; header date disagrees with filename' 
!            call Write_Log(Ctrl, trim(message), status)
!	 end if
      end if

!     For this set of reads, if read fails (non-zero iostat), set status
!     immendiately but report error later since the message is the same for
!     all cases.

!     Number of latitude and longitude points

      if (status == 0) then
         read(unit=lun, iostat=ios) RTM%SW%Grid%NLat, RTM%SW%Grid%NLon
         if (ios /= 0) status = SwRTMReadErr
      end if

!     Number of pressure levels and number of solar channels

      if (status == 0) then 
         read(unit=lun, iostat=ios) RTM%SW%NP, NChan
         if (ios /= 0) status = SwRTMReadErr
      end if

!     Allocate size of ChanID and WvNumber
      if (status == 0) then 
         allocate(ChanID(NChan))
         allocate(WvNumber(NChan))

!        Read ChanID and WvNumber

         read(unit=lun, iostat=ios) ChanID
         if (ios /= 0) status = SwRTMReadErr
      end if

      if (status == 0) then 
         read(unit=lun, iostat=ios) WvNumber
         if (ios /= 0) status = SwRTMReadErr
      end if

!     Check that required solar channels are present

      if (status == 0) then
         !        Initialise counter

         chan_found = 0
         allocate(index(Ctrl%Ind%NSolar))
         index(:) = 0
         k = 1

         !        Loop over shortwave instrument channels, checking that requested
         !        channels are available in the RTM data and setting up the index array
         !        to allow us to find the channels we want from the RTM file data (in
         !        case order of storage is different from the order we specified our
         !        selection).
         
         do i = Ctrl%Ind%SolarFirst, Ctrl%Ind%SolarLast
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
         if (chan_found /= Ctrl%Ind%NSolar) then
            status = SwRTMChanErr ! Return error code
            write(unit=message, fmt=*) &
                 'Read_SwRTM: RTM file; required instrument channels not found' 
            call Write_Log(Ctrl, trim(message), status)
         end if

      end if  ! End of "required solar channels" status check
   

      if (status == 0) then 
         !        Allocate sizes of arguments in call to Read_SwRTM
         
         allocate(RTM%SW%Tbc(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon, Ctrl%Ind%NSolar, RTM%SW%NP))
         allocate(RTM%SW%Tac(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon, Ctrl%Ind%NSolar, RTM%SW%NP))
         allocate(RTM%SW%lat(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))
         allocate(RTM%SW%lon(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))   
         
         allocate(buf(NChan,RTM%SW%NP,2))
!         write(*,*) 'after allocate'

         !         buf=0.00

         !        Read data blocks (only read the required channels)

         do ilat = 1, RTM%SW%Grid%NLat
            do ilon = 1, RTM%SW%Grid%NLon
               read(unit=lun, iostat=ios) x, y

               if (ios /= 0) exit
!               write(*,*) ilat,ilon,RTM%SW%Grid%NLat,RTM%SW%Grid%NLon

               RTM%SW%lat(ilat, ilon) = x
               RTM%SW%lon(ilat, ilon) = y


               !              Read block of data containing Tac, Tbc etc for all channels
               !              and pressure levels at this lat and lon
               !              In the binary RTM file data is written in whole-array blocks.
               !              Each block contains the 2 RTM parameters written for all 
               !              Sw channels and pressure levels as follows: 
               !              Taubc(1:Nchans,1:Nlevs), Tau(1:Nchans,1:Nlevs), ...
               
               read(unit=lun, iostat=ios) buf(:,:,:)
               if (ios /= 0) exit

               !              Write required channels into output arrays
               write(*,*)'index sw',index ,Ctrl%Ind%NSolar

               do i = 1, Ctrl%Ind%NSolar
                  RTM%SW%Tbc(ilat, ilon, i, :) = buf(index(i),:, 1)
                  RTM%SW%Tac(ilat, ilon, i, :) = buf(index(i),:, 2)
               end do
            end do
            if (ios /= 0) exit
         end do
         if (ios /= 0) write(*,*)'Error in main read loop'
         if (ios /= 0) status = SwRTMReadErr
      
!        Deallocate arrays for reuse

!MJORG         if (allocated(buf)) deallocate(buf)
!         write(*,*) allocated(buf)
         !         write(*,*) status
         deallocate(buf)

         
      end if  ! End of status check before main data blocks are allocated and 
              ! read in.

!     Close RTM file and deallocate allocatable arrays if not required

      if (status == SwRTMReadErr) &
         call Write_Log(Ctrl, 'Read_SwRTM: error reading from Sw file', &
            status)

      close(lun)
      if (allocated(index)) deallocate(index)
      if (allocated(WvNumber)) deallocate(WvNumber)
      if (allocated(ChanID)) deallocate(ChanID)
   
   end if  ! End of check that ios is ok on opening Sw RTM file.

   if (status==0) write(*,*)'Read SW RTM data ok'

      
     
   if (status == 0) then

!      write(*,*) 'CALC BEINM'

!        Calculate grid parameters for use in Get_SwRTM
!        Corners of the grid

         RTM%SW%Grid%Lat0 = RTM%SW%Lat(1,1)
         RTM%SW%Grid%LatN = RTM%SW%Lat(RTM%SW%Grid%NLat,1)
         RTM%SW%Grid%Lon0 = RTM%SW%Lon(1,1)
         RTM%SW%Grid%LonN = RTM%SW%Lon(1,RTM%SW%Grid%NLon)

!         write(*,*) 'den'
!         write(*,*) RTM%SW%Grid%Lat0, RTM%SW%Lat(1,1)
!         write(*,*) RTM%SW%Grid%LatN, RTM%SW%Lat(RTM%SW%Grid%NLat,1)
!         write(*,*) RTM%SW%Grid%Lon0, RTM%SW%Lon(1,1)
!         write(*,*) RTM%SW%Grid%LonN, RTM%SW%Lon(1,RTM%SW%Grid%NLon)

 
!        Grid spacing and inverse     
     
!         write(*,*) 'den oims',RTM%SW%Grid%NLat,RTM%SW%Grid%NLon


         RTM%SW%Grid%delta_Lat     =   ( RTM%SW%Grid%LatN - RTM%SW%Grid%Lat0 ) & 
                                          / (RTM%SW%Grid%NLat-1)
!         write(*,*) 'dlat',RTM%SW%Grid%delta_Lat
         if(RTM%SW%Grid%delta_Lat .lt. ditherm3) then
            RTM%SW%Grid%inv_delta_Lat=ditherm3        
         else
            RTM%SW%Grid%inv_delta_Lat = 1 / RTM%SW%Grid%delta_Lat
         endif

         RTM%SW%Grid%delta_Lon     =   ( RTM%SW%Grid%LonN - RTM%SW%Grid%Lon0 ) &
                                          / (RTM%SW%Grid%NLon-1)
!         write(*,*) 'dlon',RTM%SW%Grid%delta_Lon
         if(RTM%SW%Grid%delta_Lon .lt. ditherm3) then
            RTM%SW%Grid%inv_delta_Lon=ditherm3
         else
            RTM%SW%Grid%inv_delta_Lon = 1 / RTM%SW%Grid%delta_Lon
         endif

!         write(*,*) 'den oims after',RTM%SW%Grid%delta_Lat,RTM%SW%Grid%delta_Lon
   
!        Max and Min grid values

         RTM%SW%Grid%MinLat = min(RTM%SW%Grid%Lat0, RTM%SW%Grid%LatN)
         RTM%SW%Grid%MaxLat = max(RTM%SW%Grid%Lat0, RTM%SW%Grid%LatN)
         RTM%SW%Grid%MinLon = min(RTM%SW%Grid%Lon0, RTM%SW%Grid%LonN)
         RTM%SW%Grid%MaxLon = max(RTM%SW%Grid%Lon0, RTM%SW%Grid%LonN)
     
!         write(*,*) 'CALC END'

      end if

    end subroutine Read_SWRTM_nc
