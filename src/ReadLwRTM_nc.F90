!-------------------------------------------------------------------------------
! Name:
!    Read_LwRTM_nc
!
! Purpose:
!    Reads the longwave Radiative Transfer Model (atmospheric) file and loads
!    appropriate data arrays.
!
! Arguments:
!    Name   Type         In/Out Description
!    Ctrl   struct       Both   Control structure
!    RTM    alloc struct Out    RTM structure
!    status int          Out    Error status
!
! Algorithm:
!    open RTM data file (report errors)
!    read instrument name and check vs. name in Ctrl struct (report errors)
!    read date
!    read number of latitude and longitude points
!    read number of pressure levels and number of thermal channels
!    allocate size of Channel ID and Wave Number arrays
!    read array of channel IDs
!    read array of channel wavenumbers
!    check channel IDs vs. requested Ids in Ctrl
!       set up index array to handle selection of required channel data from
!       input data (since order of storage of different channel values in the
!       file may not match the order in the ECP)
!    (report error if not all thermal chans requested are stored in the RTM file)
!
!    Use no. of latitude, longitude, pressure levels from file plus no. of
!    requested thermal channels to allocate RTM arrays
!
!    for each latitude band (1 to NLat)
!       for each longitude (1 to NLon)
!          read the latitude and longitude values
!          read the emissivity value for each channel
!          read the pressure dependent LW RTM data for all channels and pressure
!             values at this lat & lon
!          use the channel index array set earlier to assign the RTM data to
!          the arrays in the RTM%LW struct
!
!    close the RTM file
!    open the profile file (report errors)
!       read date (check vs. date from RTM file: report errors)
!       read no. of lat and lon points (check vs. RTM file values: report errors)
!       allocate RTM%LW pressure and temp. arrays
!
!       for each latitude
!          for each longitude
!             read lat, lon and no. of pressure levels
!             change sign of longitude value
!             check lat and lon agree with RTM data: report errors
!             check no. of pressure values agrees with RTM data: report errors
!             read in block of pressure and temp. values and assign to arrays
!             in RTM%LW
!
!    close the profile file
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
!       Corrected indexing of RTM%LW%Lat and Lon from 1-D to 2-D array
!    25th Jan 2001, Kevin M. Smith:
!       Corrected calculation of LatN and LonN in RTM%LW%Grid.
!    21st Feb 2001, Andy Smith:
!       Added Tbc to LW structure. Previously missing from model data.
!     1st Mar 2001, Andy Smith:
!       Removed allocation of R_Clear in LW RTM struct. R_Clear not available
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
!    **************** ECV work starts here *************************************
!     7th Feb 2011, Andy Smith:
!       Re-applying changes made in 2002.
!       Converted to unformatted read:
!        - variables x, y, buf and bufe are declared as real(4) in order to
!          match the number of bytes used for reals in the RTM files (previously
!          8).
!        - Allocation of buffer array "buf" changed from 7 paramaters to 5,
!          since whole array writes to binary file mean that the level and
!          channel indices are no longer present.
!        - Array dimensions in buf swapped round to make access more efficient.
!          Channel is now the first index, then pressure level, and parameter
!          last.
!       Error checking improved: iostat values checked.
!       Added tests for allocation status before deallocation of local
!       allocatable arrays (may not be allocated if errors detected before
!       allocation).
!       Date changed to character length 8 (YYYYMMDD) instead of 10.
!    19th Sep 2002, Caroline Poulsen:
!       Bug found, changed the deltalat and deltalon
!    12th Dec 2002, Caroline Poulsen:
!       Now read geopotential height from profile file)
!    15th Feb 2011, Andy Smith:
!       Character string "dummy" length changed from 10 to 8, otherwise read
!       error occurs on prtm file.
!     7th Nov 2011, C Poulsen:
!       Tidied up comments but no aculacode change
!    2012/01/20, CP: changed reading of buf prtm array
!    2012/08/23, MJ: Uses initial file as template for netcdf read.
!    2012/08/24, MJ: Implements prtm file read
!    2012/09/20, CP: Assigned channel index to y_id
!    2012/10/04, CP: Added new sp and tskin variable
!    2012/11/03, MST: AVHRR chanid hardcoded
!    2012/11/14, CP: Changed layers to levels
!    2013/11/21, MJ: Fixed reading of rad. information wrt channels
!    2014/01/31, MJ: removed hardcoded parts for avhrr (obsolete)
!    2014/04/18, GM: Made reading of NetCDF input more efficient by avoiding
!       inefficient access patterns and redundancy and cleaned up the code.
!    2014/05/28, GM: Removed unused read of attribute 'Product_Date'.
!    2014/07/23, AP: Grid no longer assumed to defined points rather than the
!       cells centres (as is actually the case).
!    2014/08/01, AP: Remove unused counter fields.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_LwRTM_nc(Ctrl, RTM, status)

   use CTRL_def
   use ECP_Constants
   use RTM_def

   use netcdf

   implicit none

   ! Argument declarations

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
   character(256)         :: message
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
!  integer(kind=nint), allocatable, dimension(:,:)   :: dummy2dint
   real(kind=sreal),   allocatable, dimension(:,:)   :: dummy1p1
   real(kind=sreal),   allocatable, dimension(:,:,:) :: dummy1p2
   real(kind=sreal),   allocatable, dimension(:)     :: dummy1df
   real(kind=sreal),   allocatable, dimension(:,:)   :: dummy2df
   real(kind=sreal),   allocatable, dimension(:,:,:) :: dummy3df


   !----------------------------------------------------------------------------
   ! PRTM (meteorology) file
   !----------------------------------------------------------------------------

   ! Open PRTM file
   if (status == 0) then
      write(*,*) 'PRTM File',trim(adjustl(Ctrl%FID%PRTM))
      ios = nf90_open(path=trim(adjustl(Ctrl%FID%PRTM)),mode = nf90_nowrite, &
                      ncid = ncid)
      if (ios /= 0) then
         status = LwRTMPFileOpenErr
         write(unit=message, fmt=*) 'Read_LwRTM: Error opening PRTM profile file ', &
               trim(adjustl(Ctrl%Fid%PRTM))
         write(*,*) trim(message)
         call Write_Log(Ctrl, trim(message), status)
         stop
      else
         if (status == 0) then
            ! Allocate arrays
            allocate(RTM%LW%lat(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))
            allocate(RTM%LW%lon(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))

            allocate(RTM%LW%skint(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))
            allocate(RTM%LW%sp   (RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))

            allocate(RTM%LW%P(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))
            allocate(RTM%LW%T(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))
            allocate(RTM%LW%H(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))

            ! Read data into arrays

            allocate(dummy1df(RTM%LW%Grid%NLatLon))

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"lon_pw",dummy1df,0)
            RTM%LW%lon=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"lat_pw",dummy1df,0)
            RTM%LW%lat=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"skint_pw",dummy1df,0)
            RTM%LW%skint=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"explnsp_pw",dummy1df,0)
            RTM%LW%sp=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

            deallocate(dummy1df)

            allocate(dummy2df(RTM%LW%Grid%NLatLon,RTM%LW%NP))

            call nc_read_array_2d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,"tprofile_lev",dummy2df,0)
            RTM%LW%T=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

            call nc_read_array_2d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,"pprofile_lev",dummy2df,0)
            RTM%LW%P=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

            call nc_read_array_2d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,"gphprofile_lev",dummy2df,0)
            RTM%LW%H=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

            deallocate(dummy2df)

         end if

         ! Close PRTM input file
         ios=nf90_close(ncid)
         if (ios == 0) write(*,*) 'Read PRTM data OK'

      end if
   endif


   !----------------------------------------------------------------------------
   ! LwRTM file
   !----------------------------------------------------------------------------

   ! Open LwRTM data file
   if (status == 0) then
      write(*,*) 'file',trim(adjustl(Ctrl%FID%LWRTM))
      ios = nf90_open(path=trim(adjustl(Ctrl%Fid%LWRTM)),mode = nf90_nowrite,ncid = ncid)
      write(*,*) 'ios', ios
      if (ios /= NF90_NOERR) then
         status = LwRTMRTMFileOpenErr
         write(unit=message, fmt=*) 'Read_LwRTM: Error opening LwPRTM profile file ', &
               trim(adjustl(Ctrl%Fid%PRTM))
         write(*,*) message
         call Write_Log(Ctrl, trim(message), status)
         stop
         stop
      else

         ! Read instrument info
         ios=nf90_get_att(ncid, NF90_GLOBAL, "Sensor_Name", sensor)
         ios=nf90_get_att(ncid, NF90_GLOBAL, "Platform", platform)
         if (ios /= 0) then
            status = LwRTMRTMInstErr
            write(*,*) 'Read_LwRTM: error reading instrument name', status
            call Write_Log(Ctrl, 'Read_LwRTM: error reading instrument name', &
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
               status = LwRTMRTMInstErr
               write(*,*) 'Read_LwRTM: RTM file; header instrument disagrees with filename',&
                    & status
               call Write_Log(Ctrl, &
                    & 'Read_LwRTM: RTM file; header instrument disagrees with filename',&
                    & status)
               stop
            end if
         end if

         ! For this set of reads, if read fails (non-zero iostat), set status
         ! immendiately but report error later since the message is the same for
         ! all cases.

         ! Allocate size of ChanID and WvNumber
         if (status == 0) then
            allocate(ChanID(RTM%LW%NLWF))
            allocate(WvNumber(RTM%LW%NLWF))

            ! Read ChanID and WvNumber
            call nc_read_array_1d_int_to_int_orac(ncid,RTM%LW%NLWF,"lw_channel_instr_ids",ChanID,0)
            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%NLWF,"lw_channel_wvl",WvNumber,0)
         endif

         write(*,*) 'LW channel instrument ids for RTM in LW preprocessing file',ChanID

         ! Check that required thermal channels are present
         if (status == 0) then

            ! Loop over longwave instrument channels, checking that requested
            ! channels are available in the RTM data and setting up the index
            ! array to allow us to find the channels we want from the RTM file
            ! data (in case order of storage is different from the order we
            ! specified our selection).

            chan_found = 0
            allocate(index(Ctrl%Ind%NThermal))
            index = 0
            k     = 1

            ! This is the loop over the requested channels
            do i = 1,Ctrl%Ind%Ny
               ! Loop over channels in RTM
               do j = 1,RTM%LW%NLWF
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

            if (chan_found /= Ctrl%Ind%NThermal) then
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
            allocate(RTM%LW%ems(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal))
            allocate(RTM%LW%Tbc(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
            allocate(RTM%LW%Tac(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
            allocate(RTM%LW%Rac_up(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
            allocate(RTM%LW%Rac_dwn(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
            allocate(RTM%LW%Rbc_up(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal, RTM%LW%NP))

            ! Succeesive cell count in preprocessing

            ! Read data into arrays
            allocate(dummy1df(RTM%LW%Grid%NLatLon))
            allocate(dummy2df(RTM%LW%Grid%NLatLon,RTM%LW%NP))

            ichan=0
            do i = 1,Ctrl%Ind%Ny
               do j = 1,RTM%LW%NLWF
                  if (Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)) == ChanID(j)) then
                     ichan=ichan+1

                     call nc_read_array_1p1_float_orac(ncid,RTM%LW%Grid%NLatLon,j,"emiss_lw",dummy1df,0)
                     RTM%LW%ems(:,:,ichan)=reshape(dummy1df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon/), order = (/2,1/))

                     call nc_read_array_2d_float_to_float_orac2(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,j,"tac_lw",dummy2df,0)
                     RTM%LW%Tac(:,:,ichan,:)=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

                     call nc_read_array_2d_float_to_float_orac2(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,j,"tbc_lw",dummy2df,0)
                     RTM%LW%Tbc(:,:,ichan,:)=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

                     call nc_read_array_2d_float_to_float_orac2(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,j,"rbc_up_lw",dummy2df,0)
                     RTM%LW%Rbc_up(:,:,ichan,:)=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

                     call nc_read_array_2d_float_to_float_orac2(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,j,"rac_up_lw",dummy2df,0)
                     RTM%LW%Rac_up(:,:,ichan,:)=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))

                     call nc_read_array_2d_float_to_float_orac2(ncid,RTM%LW%Grid%NLatLon,RTM%LW%NP,j,"rac_down_lw",dummy2df,0)
                     RTM%LW%Rac_dwn(:,:,ichan,:)=reshape(dummy2df,(/RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,RTM%LW%NP/), order = (/3,2,1/))
                  endif
               enddo
            enddo

            deallocate(dummy1df)
            deallocate(dummy2df)
         endif

         if (allocated(WvNumber)) deallocate(WvNumber)
         if (allocated(ChanID))   deallocate(ChanID)
         if (allocated(index))    deallocate(index)

         ! Close LwRTM input file
         ios=nf90_close(ncid)

         if (status == LwRTMReadErr) &
            call Write_Log(Ctrl, 'Read_LWRTM: error reading from LW file', status)
      end if

      if (status == 0) write(*,*)'Read LW RTM data OK'

      if (status == 0) then
         ! Calculate grid parameters for use in Get_LwRTM

         ! Corners of the grid
         RTM%LW%Grid%Lat0 = RTM%LW%Lat(1,1)
         RTM%LW%Grid%LatN = RTM%LW%Lat(RTM%LW%Grid%NLat,1)
         RTM%LW%Grid%Lon0 = RTM%LW%Lon(1,1)
         RTM%LW%Grid%LonN = RTM%LW%Lon(1,RTM%LW%Grid%NLon)

         ! Grid spacing and inverse
         RTM%LW%Grid%delta_Lat = (RTM%LW%Grid%LatN - RTM%LW%Grid%Lat0) &
                                 / (RTM%LW%Grid%NLat-1)
         RTM%LW%Grid%inv_delta_Lat = 1. / RTM%LW%Grid%delta_Lat

         RTM%LW%Grid%delta_Lon = (RTM%LW%Grid%LonN - RTM%LW%Grid%Lon0) &
                                 / (RTM%LW%Grid%NLon-1)
         RTM%LW%Grid%inv_delta_Lon = 1. / RTM%LW%Grid%delta_Lon

         ! Max and Min grid values
         RTM%LW%Grid%MinLat = min(RTM%LW%Grid%Lat0-0.5*RTM%LW%Grid%delta_Lat, &
              RTM%LW%Grid%LatN+0.5*RTM%LW%Grid%delta_Lat)
         RTM%LW%Grid%MaxLat = max(RTM%LW%Grid%Lat0-0.5*RTM%LW%Grid%delta_Lat, &
              RTM%LW%Grid%LatN+0.5*RTM%LW%Grid%delta_Lat)
         RTM%LW%Grid%MinLon = min(RTM%LW%Grid%Lon0-0.5*RTM%LW%Grid%delta_Lon, &
              RTM%LW%Grid%LonN+0.5*RTM%LW%Grid%delta_Lon)
         RTM%LW%Grid%MaxLon = max(RTM%LW%Grid%Lon0-0.5*RTM%LW%Grid%delta_Lon, &
              RTM%LW%Grid%LonN+0.5*RTM%LW%Grid%delta_Lon)
      end if
   end if

end subroutine Read_LwRTM_nc
