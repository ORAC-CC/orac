!-------------------------------------------------------------------------------
! Name:
!    Read_LwRTM_nc
!
! Purpose:
!    Reads the longwave Radiative Transfer Model (atmospheric) file and loads
!    appropriate data arrays.
!
! Arguments:
!    Name    Type         In/Out/Both Description
!    Ctrl    struct       Both        Control structure
!    RTM     alloc struct Out         RTM structure
!    verbose logical      In          Print out progress information
!
! Algorithm:
! 1) Open the PRTM file.
! 2) Read wavelength-independent fields from the PRTM file.
! 3) Close the PRTM file.
! 4) Open the LWRTM file.
! 5) Read the instrument name and ensure it matches that expected.
! 6) Read the available channel numbers.
! 7) Read the necessary thermal channel information.
! 8) Close the LWRTM file.
! 9) Determine the RTM grid from the lat/lon fields.
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
!        - Allocation of buffer array "buf" changed from 7 parameters to 5,
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
!     7th Nov 2011, Caroline Poulsen:
!       Tidied up comments but no actual code change
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
!    2014/08/15, AP: Switching to preprocessor NCDF routines.
!    2014/09/18, AP: Update to RTTOV11 output arrays in the correct shape.
!    2014/09/28, GM: Updated to conform with a new arrangement of dimensions.
!    2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!       channels with respect to the channels actually processed, rather than the
!       MSI file.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_LwRTM_nc(Ctrl, RTM, verbose)

   use CTRL_def
   use ECP_Constants
   use orac_ncdf
   use RTM_def

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   type(RTM_t),  intent(out)   :: RTM
   logical,      intent(in)    :: verbose

   ! Local variables

   ! Note on values read from the binary Lw and P RTM files. These files are
   ! generated by RTTOV code, which is compiled with a flag to force reals to
   ! become real(8). The parameter arrays read in via buf, and the lat/lons
   ! etc are explicitly written as real(4) in order to reduce the file size.

   integer                  :: ncid, chan_found, i, j
   real(sreal), allocatable :: dummy1d(:)
   character(Instnamelen)   :: platform, sensor, instname
   integer, allocatable     :: index(:), ChanID(:)
!  real(4), allocatable     :: WvNumber(:)


   !----------------------------------------------------------------------------
   ! PRTM (meteorology) file
   !----------------------------------------------------------------------------

   ! Open PRTM file
   call nc_open(ncid, Ctrl%FID%PRTM)

   ! Allocate arrays
   allocate(RTM%LW%lat(RTM%LW%Grid%NLon, RTM%LW%Grid%NLat))
   allocate(RTM%LW%lon(RTM%LW%Grid%NLon, RTM%LW%Grid%NLat))

   allocate(RTM%LW%skint(RTM%LW%Grid%NLon, RTM%LW%Grid%NLat))
   allocate(RTM%LW%sp   (RTM%LW%Grid%NLon, RTM%LW%Grid%NLat))

   allocate(RTM%LW%P(RTM%LW%NP, RTM%LW%Grid%NLon, RTM%LW%Grid%NLat))
   allocate(RTM%LW%T(RTM%LW%NP, RTM%LW%Grid%NLon, RTM%LW%Grid%NLat))
   allocate(RTM%LW%H(RTM%LW%NP, RTM%LW%Grid%NLon, RTM%LW%Grid%NLat))

   ! Read data into arrays
   allocate(dummy1d(RTM%LW%Grid%NLon))
   call nc_read_array(ncid, "lon_rtm", dummy1d, verbose)
   do i=1,RTM%LW%Grid%NLon
      RTM%LW%lon(i,:) = dummy1d(i)
   end do
   deallocate(dummy1d)

   allocate(dummy1d(RTM%LW%Grid%NLat))
   call nc_read_array(ncid, "lat_rtm", dummy1d, verbose)
   do i=1,RTM%LW%Grid%NLat
      RTM%LW%lat(:,i) = dummy1d(i)
   end do
   deallocate(dummy1d)

   call nc_read_array(ncid, "skint_rtm", RTM%LW%skint, verbose)
   call nc_read_array(ncid, "explnsp_rtm", RTM%LW%sp, verbose)
   call nc_read_array(ncid, "pprofile_rtm", RTM%LW%P, verbose)
   call nc_read_array(ncid, "tprofile_rtm", RTM%LW%T, verbose)
   call nc_read_array(ncid, "hprofile_rtm", RTM%LW%H, verbose)

   ! Close PRTM input file
   if (nf90_close(ncid) /= NF90_NOERR) &
      stop 'ERROR: read_lwrtm_nc(): Error closing PRTM file.'


   !----------------------------------------------------------------------------
   ! LwRTM file
   !----------------------------------------------------------------------------

   ! Open LwRTM data file
   call nc_open(ncid, Ctrl%FID%LWRTM)

   ! Ensure instrument info matches the sensor being processed
   if (nf90_get_att(ncid, NF90_GLOBAL, "Sensor", sensor) /= NF90_NOERR .or.&
       nf90_get_att(ncid, NF90_GLOBAL, "Platform", platform) /= NF90_NOERR) &
      stop 'ERROR: read_lwrtm_nc(): Could not read global attributes.'
   if (sensor =='AATSR') then
      instname=trim(adjustl(sensor))
   else
      instname=trim(adjustl(sensor))//'-'//trim(adjustl(platform))
   end if
   if (trim(adjustl(instname)) /= trim(adjustl(Ctrl%Inst%Name))) &
      stop 'ERROR: read_lwrtm_nc(): Instrument in RTM header inconsistent'

   allocate(ChanID(RTM%LW%NLWF))
!  allocate(WvNumber(RTM%LW%NLWF))

   ! Read ChanID and WvNumber
   call nc_read_array(ncid, "lw_channel_instr_ids", ChanID, verbose)
!  call nc_read_array(ncid, "lw_channel_wvl", WvNumber, verbose)

   if (verbose) write(*,*) &
      'LW channel instrument ids for RTM in LW preprocessing file',ChanID

   ! Check that required thermal channels are present

   ! Loop over longwave instrument channels, checking that requested
   ! channels are available in the RTM data and setting up the index
   ! array to allow us to find the channels we want from the RTM file
   ! data (in case order of storage is different from the order we
   ! specified our selection).
   chan_found = 0
   allocate(index(Ctrl%Ind%NThermal))
   index = 0

   ! This is the loop over the requested channels
   do i = 1,Ctrl%Ind%Ny
      ! Loop over channels in RTM
      do j = 1,RTM%LW%NLWF
         ! Signal that the required channel has been found by incrementing
         ! chan_found and break out of the inner loop to start search for
         ! next instrument channel
         if (Ctrl%Ind%Y_Id(i) == ChanID(j)) then
            chan_found = chan_found + 1
            index(chan_found) = j
            exit
         end if
      end do
   end do

   if (chan_found /= Ctrl%Ind%NThermal) &
      stop 'ERROR: read_lwrtm_nc(): required instrument channels not found'

   ! Allocate arrays
   allocate(RTM%LW%Ems(Ctrl%Ind%NThermal,RTM%LW%Grid%NLon,RTM%LW%Grid%NLat))
   allocate(RTM%LW%Tac(Ctrl%Ind%NThermal,RTM%LW%NP,RTM%LW%Grid%NLon, &
      RTM%LW%Grid%NLat))
   allocate(RTM%LW%Tbc(Ctrl%Ind%NThermal,RTM%LW%NP,RTM%LW%Grid%NLon, &
      RTM%LW%Grid%NLat))
   allocate(RTM%LW%Rac_up(Ctrl%Ind%NThermal,RTM%LW%NP,RTM%LW%Grid%NLon, &
      RTM%LW%Grid%NLat))
   allocate(RTM%LW%Rac_dwn(Ctrl%Ind%NThermal,RTM%LW%NP,RTM%LW%Grid%NLon, &
      RTM%LW%Grid%NLat))
   allocate(RTM%LW%Rbc_up(Ctrl%Ind%NThermal,RTM%LW%NP,RTM%LW%Grid%NLon, &
      RTM%LW%Grid%NLat))

   ! Read data into arrays
   call nc_read_array(ncid, "emiss_lw", RTM%LW%Ems, verbose, 1, index)
   call nc_read_array(ncid, "tac_lw", RTM%LW%Tac, verbose, 1, index)
   call nc_read_array(ncid, "tbc_lw", RTM%LW%Tbc, verbose, 1, index)
   call nc_read_array(ncid, "rbc_up_lw", RTM%LW%Rbc_up, verbose, 1, index)
   call nc_read_array(ncid, "rac_up_lw", RTM%LW%Rac_up, verbose, 1, index)
   call nc_read_array(ncid, "rac_down_lw", RTM%LW%Rac_dwn, verbose, 1, index)

!  if (allocated(WvNumber)) deallocate(WvNumber)
   if (allocated(ChanID))   deallocate(ChanID)
   if (allocated(index))    deallocate(index)

   ! Close LwRTM input file
   if (nf90_close(ncid) /= NF90_NOERR) &
      stop 'ERROR: read_lwrtm_nc(): Error closing file.'

   ! Calculate grid parameters for use in Get_LwRTM
   ! Corners of the grid
   RTM%LW%Grid%Lat0 = real(RTM%LW%Lat(1,1), kind=8)
   RTM%LW%Grid%LatN = real(RTM%LW%Lat(1,RTM%LW%Grid%NLat), kind=8)
   RTM%LW%Grid%Lon0 = real(RTM%LW%Lon(1,1), kind=8)
   RTM%LW%Grid%LonN = real(RTM%LW%Lon(RTM%LW%Grid%NLon,1), kind=8)

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

   ! Does the grid wrap around the international date-line?
   RTM%LW%Grid%Wrap = RTM%LW%Grid%MinLon <= -180. .and. &
                      RTM%LW%Grid%MaxLon >=  180.

end subroutine Read_LwRTM_nc
