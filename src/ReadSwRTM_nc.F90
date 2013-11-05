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
!   20120823 MJ uses initial file as template for netcdf read.
!   20120828 CP defined nchan and changed indicing of y_id
!   20121103 MJ changed loop over SW channels
!   2013 MJ irones out some bugs wrt old binary file implementation.

! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Read_SWRTM_nc(Ctrl, RTM, status)

   use CTRL_def
   use RTM_def
   use ECP_Constants

   use netcdf

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
 
   !netcdf related
   character(Instnamelen) :: platform
   character(Instnamelen) :: sensor
   character(len=12) :: prod_date_swrtm
   integer :: ncid,ik
   real(kind=sreal), allocatable, dimension(:,:) :: dummy1p1
   real(kind=sreal), allocatable, dimension(:,:,:) :: dummy1p2
   real(kind=sreal), allocatable, dimension(:) :: dummy1df
   integer(kind=nint), allocatable, dimension(:,:) :: dummy2dint
   real(kind=sreal), allocatable, dimension(:,:) :: dummy2df


!############################################
!SW FILE
!############################################

   !write(*,*) 'IN HERE'
   !pause

!  Open RTM data file
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%SWRTM)),mode = nf90_nowrite,ncid = ncid) 

   !"write(*,*) Ctrl%Fid%SWRTM
   !pause

   if (ios /= 0) then
      status = SwRTMRTMFileOpenErr ! Return error code
      write(unit=message, fmt=*) 'Read_SwRTM: Error opening RTM file ', Ctrl%Fid%SWRTM 
      call Write_Log(Ctrl, trim(message), status)
   else

!     Read RTM data file

      !     Instrument
      ios=nf90_get_att(ncid, NF90_GLOBAL, "Sensor_Name", sensor)
      ios=nf90_get_att(ncid, NF90_GLOBAL, "Platform", platform)
      if (ios /= 0) then
         status = SwRTMRTMInstErr ! Return error code
         call Write_Log(Ctrl, 'Read_SwRTM: error reading instrument name', &
              status)
         !         write(*,*) 'hier0'
         !         write(*,*) InstName
         !         write(*,*) Ctrl%Inst%Name
         !         pause
      else
         if (sensor =='AATSR') then
            instname=trim(adjustl(sensor))
         else
            instname=trim(adjustl(sensor))//'-'//trim(adjustl(platform))
         endif


         !MJ ORG instname=trim(adjustl(sensor))//'-'//trim(adjustl(platform))
         InstName = adjustl(InstName)
         if (trim(adjustl(InstName)) /= trim(adjustl(Ctrl%Inst%Name))) then
            status = SwRTMRTMInstErr ! Return error code
            call Write_Log(Ctrl, &
                 & 'Read_SwRTM: RTM file; header instrument disagrees with filename',&
                 & status)
         end if
      end if

      !write(*,*) sensor,platform
      !pause

!     Date
      if (status == 0) then 
         !Read product date and time from netcdf global attributes
         ios=nf90_get_att(ncid, NF90_GLOBAL, "Product_Date", prod_date_swrtm)     
         if (ios /= 0) then
            status = SwRTMRTMDateErr ! Return error code
            call Write_Log(Ctrl, 'Read_SwRTM: error reading date', &
                 status)
         end if
         !         if (trim(Date) /= trim(Ctrl%Inst%Date)) then
         !             status = LwRTMRTMDateErr ! Return error code
         !            write(unit=message, fmt=*) &
         !                 'Read_LwRTM: RTM file; header date disagrees with filename' 
         !            call Write_Log(Ctrl, trim(message), status)
         !         end if
      end if


      !     Allocate size of ChanID and WvNumber
      if (status == 0) then 
         allocate(ChanID(RTM%SW%NSWF))
         allocate(WvNumber(RTM%SW%NSWF))

!        Read ChanID and WvNumber
         call nc_read_array_1d_int_to_int_orac(ncid,RTM%SW%NSWF,"sw_channel_instr_ids",ChanID,0)
         call nc_read_array_1d_float_to_float_orac(ncid,RTM%SW%NSWF,"sw_channel_wvl",WvNumber,0)
      endif

      !write(*,*) chanid
      !write(*,*) WvNumber
      !pause

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
         Nchan=size(chanid)

         do i = Ctrl%Ind%SolarFirst, Ctrl%Ind%SolarLast
            !           Loop over channels in RTM
            do j = 1, RTM%SW%NSWF
!            do j = 1, NChan
               !              Signal that the required channel has been found by incrementing
               !              chan_found and break out of the inner loop to start search for
               !              next instrument channel
               if (Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)) == ChanID(j)) then
                  !if (Ctrl%Ind%Y_Id(i) == ChanID(j)) then
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

         allocate(dummy1p1(RTM%SW%Grid%NLatLon,1))
         dummy1p1=real_fill_value
         allocate(dummy1df(RTM%SW%Grid%NLatLon))
         dummy1df=real_fill_value
         allocate(dummy1p2(RTM%SW%Grid%NLatLon,1,1))
         dummy1p2=real_fill_value
         !read al wl related stuff here
         !write(*,*) Ctrl%Ind%ThermalFirst, Ctrl%Ind%ThermalLast,Ctrl%Ind%NThermal
         !pause
         do i = Ctrl%Ind%SolarFirst, Ctrl%Ind%SolarLast
            do j = 1,RTM%SW%NSWF
               !write(*,*) i,Ctrl%Ind%Y_Id(i),j,ChanID(j)
               !pause
               if (Ctrl%Ind%Y_Id(i) == ChanID(j)) then

                  do ik=1,RTM%SW%NP
                     call nc_read_array_1p2_float_orac(ncid,RTM%SW%Grid%NLatLon,j,ik,"tac_sw",dummy1p2,0)
                     dummy1df=dummy1p2(:,1,1)
                     RTM%SW%Tac(:,:,i-Ctrl%Ind%SolarFirst+1,ik)=&
                          & transpose(reshape(dummy1df,(/RTM%SW%Grid%NLon,RTM%SW%Grid%NLat/)))
                     
                     call nc_read_array_1p2_float_orac(ncid,RTM%SW%Grid%NLatLon,j,ik,"tbc_sw",dummy1p2,0)
                     dummy1df=dummy1p2(:,1,1)
                     RTM%SW%Tbc(:,:,i-Ctrl%Ind%SolarFirst+1,ik)=&
                          & transpose(reshape(dummy1df,(/RTM%SW%Grid%NLon,RTM%SW%Grid%NLat/)))

                  enddo

               end if
            end do
         end do
         
         deallocate(dummy1p1)
         deallocate(dummy1p2)
         deallocate(dummy1df)
         
         !write(*,*) RTM%SW%Tbc(5,5,1,:)
         !pause
         
      endif

         !close  swrtm input file
      ios=nf90_close(ncid)


      if (status == SwRTMReadErr) &
         call Write_Log(Ctrl, 'Read_SwRTM: error reading from Sw file', &
            status)

      if (allocated(index)) deallocate(index)
      if (allocated(WvNumber)) deallocate(WvNumber)
      if (allocated(ChanID)) deallocate(ChanID)
   
   end if  ! End of check that ios is ok on opening Sw RTM file.

   if (status==0) write(*,*)'Read SW RTM data ok'

      
     
   if (status == 0) then

!      write(*,*) 'CALC BEINM'

!        Calculate grid parameters for use in Get_SwRTM
!        Corners of the grid
      allocate(RTM%SW%lat(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon))
      allocate(RTM%SW%lon(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon))   
      RTM%SW%Lat=RTM%LW%Lat
      RTM%SW%Lon=RTM%LW%Lon


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

      !write(*,*) status
      !pause

    end subroutine Read_SWRTM_nc
