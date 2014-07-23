!-------------------------------------------------------------------------------
! Name:
!    Read_SwRTM
!
! Purpose:
!    Reads the shortwave Radiative Transfer Model (atmospheric) file and loads
!    appropriate data arrays.
!
! Arguments:
!    Name   Type         In/Out Description
!    Ctrl   struct       Both   Control structure
!    RTM    alloc struct Out    RTM structure
!    status int          Out    Error status
!
! Algorithm:
!    Open RTM data file (report errors)
!    Read instrument name and check vs. name in Ctrl struct (report errors)
!    Read date
!    Read number of latitude and longitude points
!    Read number of pressure levels and number of solar channels
!    Allocate size of Channel ID and Wave Number arrays
!    Read array of channel IDs
!    Read array of channel wavenumbers
!    Check channel IDs vs. requested Ids in Ctrl
!       set up index array to handle selection of required channel data from
!       input data (since order of storage of different channel values in the
!       file may not match the order in the ECP)
!    (report error if not all solar chans requested are stored in the RTM file)
!
!    Use no. of latitude, longitude, pressure levels from file plus no. of
!    requested solar channels to allocate RTM arrays
!
!    for each latitude band (1 to NLat)
!       for each longitude (1 to NLon)
!          read the latitude and longitude values
!          read the pressure dependent SW RTM data for all channels and
!             pressure values at this lat & lon
!          use the channel index array set earlier to assign the RTM data to the
!             arrays in the RTM%SW struct
!
!    close the RTM file
!
!    set up grid parameters for use in Get_LwRTM:
!       lat and lon valus at corners of grid
!       min and max latitude and longitude values
!       step size on lat and lon grids (and reciprocals)
!
! Local variables:
!    Name Type Description
!
! History:
!     5th Dec 2000, Kevin M. Smith:
!       Original version
!    15th Jan 2001, Kevin M. Smith:
!       Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
!    17th Jan 2001, Kevin M. Smith:
!       Corrected indexing of RTM%SW%Lat and Lon from 1-D to 2-D array
!    25th Jan 2001, Kevin M. Smith:
!       Corrected calculation of LatN and LonN in RTM%SW%Grid.
!    21st Feb 2001, Andy Smith:
!       Added Tbc to SW structure. Previously missing from model data.
!     1st Mar 2001, Andy Smith:
!       Removed allocation of R_Clear in SW RTM struct. R_Clear not available
!       from RTM data file.
!    30th Mar 2001, Andy Smith:
!       Added setting of new grid variables MaxLat, MinLat, MaxLon, MinLon.
!       Avoids repeated re-calculation in GetRTM.
!    22nd Jun 2001, Andy Smith:
!       Updated header comments and added argument intent.
!    24th Oct 2001, Andy Smith:
!       Added deallocation of local allocatable arrays.
!       Removed change of sign on longitude values. Data should be supplied on a
!       grid with west -ve.
!    *** ECV work starts here ***
!     7th Feb 2011, Andy Smith:
!       Re-applying changes made in 2002.
!       Converted to unformatted read:
!       - Variables x, y, buf and bufe are declared as real(4) in order to match
!         the number of bytes used for reals in the RTM files (previously 8).
!       - Allocation of buffer array "buf" changed from 7 paramaters to 5, since
!         whole array writes to binary file mean that the level and channel
!         indices are no longer present.
!       - Array dimensions in buf swapped round to make access more efficient.
!         Channel is now the first index, then pressure level, and parameter last.
!       Error checking improved: iostat values checked.
!       Added tests for allocation status before deallocation of local
!       allocatable arrays (may not be allocated if errors detected before
!       allocation).
!       Date changed to character length 8 (YYYYMMDD) instead of 10.
!    19th Sep 2002, Caroline Poulsen, bug found, changed the deltalat and
!       deltalon
!    12th Dec 2002, Caroline Poulsen now read geopotential height from profile
!       file)
!    15th Feb 2011, Andy Smith:
!       Character string "dummy" length changed from 10 to 8, otherwise read
!       error occurs on prtm file.
!    22nd Sep 2011, Caroline Poulsen: modified the lwrtm code to read in the
!       shortwave rtm which is now scene dependant and not fixed for each
!       latitude band.
!    20th Jan 2012:
!       General tidy up
!    2012/08/23, MJ: Uses initial file as template for netcdf read.
!    2012/08/28, CP: Defined nchan and changed indicing of y_id
!    2012/11/03, MJ: Changed loop over SW channels
!    2013/01/01, MJ: Irones out some bugs wrt old binary file implementation.
!    2014/04/18, GM: Made reading of NetCDF input more efficient by avoiding
!       inefficient access patterns and redundancy and cleaned up the code.
!    2014/05/28, GM: Removed unused read of attribute 'Product_Date'.
!    2014/07/23, AP: Commented out unused code for future deletion.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Read_SWRTM_nc(Ctrl, RTM, status)

   use CTRL_def
   use ECP_Constants
   use RTM_def

   use netcdf

   implicit none

!  Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   type(RTM_t),  intent(out)   :: RTM
   integer,      intent(inout) :: status

   ! Local variables

   ! Note on values read from the binary Lw and P RTM files. These files are
   ! generated by RTTOV code, which is compiled with a flag to force reals to
   ! become real(8). The parameter arrays read in via buf, and the lat/lons
   ! etc are explicitly written as real(4) in order to reduce the file size.

   integer                :: i, j, k
   integer                :: ios
   character(180)         :: message
   character(Instnamelen) :: InstName
   integer, allocatable   :: ChanID(:)
   real(4), allocatable   :: WvNumber(:)
   integer                :: chan_found
   integer, allocatable   :: index(:)

   ! NetCDF related
   character(Instnamelen) :: platform
   character(Instnamelen) :: sensor

   integer :: ik,ichan
   integer :: ncid

   real(kind=sreal), allocatable, dimension(:,:)   :: dummy1p1
   real(kind=sreal), allocatable, dimension(:,:,:) :: dummy1p2
   real(kind=sreal), allocatable, dimension(:)     :: dummy1df
   real(kind=sreal), allocatable, dimension(:,:)   :: dummy2df
   real(kind=sreal), allocatable, dimension(:,:,:) :: dummy3df


   !----------------------------------------------------------------------------
   ! Read SwRTM file
   !----------------------------------------------------------------------------
   if (status == 0) then
!     Open RTM data file
      ios = nf90_open(path=trim(adjustl(Ctrl%Fid%SWRTM)),mode = nf90_nowrite,ncid = ncid)

      if (ios /= 0) then
         write(unit=message, fmt=*) 'Read_SwRTM: Error opening SwRTM profile file ', &
               trim(adjustl(Ctrl%Fid%PRTM))
         write(*,*) message
         call Write_Log(Ctrl, trim(message), status)
         stop
      else

         ! Read instrument info
         ios=nf90_get_att(ncid, NF90_GLOBAL, "Sensor_Name", sensor)
         ios=nf90_get_att(ncid, NF90_GLOBAL, "Platform", platform)
         if (ios /= 0) then
            status = SwRTMRTMInstErr ! Return error code
            write(*,*) 'Read_SwRTM: error reading instrument name'
            call Write_Log(Ctrl, 'Read_SwRTM: error reading instrument name', &
                 status)
            stop
         else
            if (sensor =='AATSR') then
               instname=trim(adjustl(sensor))
            else
               instname=trim(adjustl(sensor))//'-'//trim(adjustl(platform))
            endif
            InstName = adjustl(InstName)
            if (trim(adjustl(InstName)) /= trim(adjustl(Ctrl%Inst%Name))) then
               status = SwRTMRTMInstErr ! Return error code
               write(*,*)  'Read_SwRTM: RTM file; header instrument disagrees with filename'
               call Write_Log(Ctrl, &
                    & 'Read_SwRTM: RTM file; header instrument disagrees with filename',&
                    & status)
               stop
            end if
         end if

         ! For this set of reads, if read fails (non-zero iostat), set status
         ! immendiately but report error later since the message is the same for
         ! all cases.

         ! Allocate size of ChanID and WvNumber
         if (status == 0) then
            allocate(ChanID(RTM%SW%NSWF))
            allocate(WvNumber(RTM%SW%NSWF))

            ! Read ChanID and WvNumber
            call nc_read_array_1d_int_to_int_orac(ncid,RTM%SW%NSWF,"sw_channel_instr_ids",ChanID,0)
            call nc_read_array_1d_float_to_float_orac(ncid,RTM%SW%NSWF,"sw_channel_wvl",WvNumber,0)
         endif

         write(*,*) 'SW channel instrument ids for RTM in SW preprocessing file',ChanID

         ! Check that required solar channels are present
         if (status == 0) then

            ! Loop over longwave instrument channels, checking that requested
            ! channels are available in the RTM data and setting up the index
            ! array to allow us to find the channels we want from the RTM file
            ! data (in case order of storage is different from the order we
            ! specified our selection).

            chan_found = 0
            allocate(index(Ctrl%Ind%NSolar))
            index(:) = 0
            k = 1

            ! This is the loop over the requested channels
            do i = 1,Ctrl%Ind%Ny
               ! Loop over channels in RTM
               do j = 1,RTM%SW%NSWF
                  ! Signal that the required channel has been found by incrementing
                  ! chan_found and break out of the inner loop to start search for
                  ! next instrument channel
                  if (Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)) == ChanID(j)) then
                     chan_found = chan_found + 1
                     index(k) = j
                     k = k+1
                     exit
                  end if
               end do
            end do

            if (chan_found /= Ctrl%Ind%NSolar) then
               status = LwRTMChanErr ! Return error code
               write(unit=message, fmt=*) &
                  'Read_LwRTM: RTM file; required instrument channels not found'
               write(*,*) trim(message)
               call Write_Log(Ctrl, trim(message), status)
               stop
            end if
         end if


         if (status == 0) then
            ! Allocate arrays

            allocate(RTM%SW%Tbc(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon, Ctrl%Ind%NSolar, RTM%SW%NP))
            allocate(RTM%SW%Tac(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon, Ctrl%Ind%NSolar, RTM%SW%NP))

            allocate(dummy2df(RTM%LW%Grid%NLatLon,RTM%LW%NP))

            ichan=0
            do i = 1,Ctrl%Ind%Ny
               do j = 1,RTM%SW%NSWF
                  if (Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)) == ChanID(j)) then
                     ichan=ichan+1

                     call nc_read_array_2d_float_to_float_orac2(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,j,"tac_sw",dummy2df,0)
                     RTM%SW%Tac(:,:,ichan,:)=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

                     call nc_read_array_2d_float_to_float_orac2(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,j,"tbc_sw",dummy2df,0)
                     RTM%SW%Tbc(:,:,ichan,:)=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

                  endif
               enddo
            enddo

            deallocate(dummy2df)

         endif

         ! Close SwRTM input file
         ios=nf90_close(ncid)

         if (allocated(index)) deallocate(index)
         if (allocated(WvNumber)) deallocate(WvNumber)
         if (allocated(ChanID)) deallocate(ChanID)

         if (status == SwRTMReadErr .or. ios .ne. 0) then
            write(*,*)'Read_SwRTM: error reading from Sw file'
            call Write_Log(Ctrl, 'Read_SwRTM: error reading from Sw file', status)
            stop
         endif
      end if

      if (status==0) write(*,*)'Read SW RTM data OK'

!      if (status == 0) then
!         allocate(RTM%SW%lat(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon))
!         allocate(RTM%SW%lon(RTM%SW%Grid%NLat, RTM%SW%Grid%NLon))
!         RTM%SW%Lat=RTM%LW%Lat
!         RTM%SW%Lon=RTM%LW%Lon
!
!         ! Calculate grid parameters for use in Get_SwRTM
!
!         ! Corners of the grid
!         RTM%SW%Grid%Lat0 = RTM%SW%Lat(1,1)
!         RTM%SW%Grid%LatN = RTM%SW%Lat(RTM%SW%Grid%NLat,1)
!         RTM%SW%Grid%Lon0 = RTM%SW%Lon(1,1)
!         RTM%SW%Grid%LonN = RTM%SW%Lon(1,RTM%SW%Grid%NLon)
!
!         RTM%SW%Grid%delta_Lat = (RTM%SW%Grid%LatN - RTM%SW%Grid%Lat0) &
!                                 / (RTM%SW%Grid%NLat-1)
!         if (RTM%SW%Grid%delta_Lat .lt. ditherm3) then
!            RTM%SW%Grid%inv_delta_Lat = ditherm3
!         else
!            RTM%SW%Grid%inv_delta_Lat = 1 / RTM%SW%Grid%delta_Lat
!         endif
!
!         RTM%SW%Grid%delta_Lon = (RTM%SW%Grid%LonN - RTM%SW%Grid%Lon0) &
!                                  / (RTM%SW%Grid%NLon-1)
!         if (RTM%SW%Grid%delta_Lon .lt. ditherm3) then
!            RTM%SW%Grid%inv_delta_Lon = ditherm3
!         else
!            RTM%SW%Grid%inv_delta_Lon = 1 / RTM%SW%Grid%delta_Lon
!         endif
!
!         ! Max and Min grid values
!         RTM%SW%Grid%MinLat = min(RTM%SW%Grid%Lat0, RTM%SW%Grid%LatN)
!         RTM%SW%Grid%MaxLat = max(RTM%SW%Grid%Lat0, RTM%SW%Grid%LatN)
!         RTM%SW%Grid%MinLon = min(RTM%SW%Grid%Lon0, RTM%SW%Grid%LonN)
!         RTM%SW%Grid%MaxLon = max(RTM%SW%Grid%Lon0, RTM%SW%Grid%LonN)
!      end if
   end if

end subroutine Read_SWRTM_nc
