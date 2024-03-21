!-------------------------------------------------------------------------------
! Name: read_lwrtm.F90
!
! Purpose:
! Reads the longwave Radiative Transfer Model (atmospheric) file and loads
! appropriate data arrays.
!
! Description and Algorithm details:
! 1) Open the LWRTM file.
! 2) Read the instrument name and ensure it matches that expected.
! 3) Read the available channel numbers.
! 4) Read the necessary thermal channel information.
! 5) Close the LWRTM file.
! 6) Determine the RTM grid from the lat/lon fields.
!
! Arguments:
! Name    Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl    struct       Both        Control structure
! RTM     alloc struct Out         RTM structure
!
! History:
! 2000/12/05, KS: Original version
! 2001/01/15, KS: Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
! 2001/01/17, KS: Corrected indexing of RTM%LW%Lat and Lon from 1-D to 2-D array
! 2001/01/25, KS: Corrected calculation of LatN and LonN in RTM%Grid.
! 2001/02/21, AS: Added Tbc to LW structure. Previously missing from model data.
! 2001/03/01, AS: Removed allocation of R_Clear in LW RTM struct. R_Clear not
!    available from RTM data file.
! 2001/03/30, AS: Added setting of new grid variables MaxLat, MinLat, MaxLon,
!    MinLon. Avoids repeated re-calculation in GetRTM.
! 2001/06/22, AS: Updated header comments and added argument intent.
! 2001/10/24, AS: Added deallocation of local allocatable arrays. Removed change
!    of sign on longitude values. Data should be supplied on a grid with west -ve
!    **************** ECV work starts here *************************************
! 2011/02/07, AS: Re-applying changes made in 2002.
!    Converted to unformatted read:
!     - variables x, y, buf and bufe are declared as real(4) in order to
!       match the number of bytes used for reals in the RTM files (previously
!       8).
!     - Allocation of buffer array "buf" changed from 7 parameters to 5,
!       since whole array writes to binary file mean that the level and
!       channel indices are no longer present.
!     - Array dimensions in buf swapped round to make access more efficient.
!       Channel is now the first index, then pressure level, and parameter
!       last.
!    Error checking improved: iostat values checked.
!    Added tests for allocation status before deallocation of local
!    allocatable arrays (may not be allocated if errors detected before
!    allocation).
!    Date changed to character length 8 (YYYYMMDD) instead of 10.
! 2002/09/19, CP: Bug found, changed the deltalat and deltalon
! 2002/12/12, CP: Now read geopotential height from profile file)
! 2011/02/15, AS: Character string "dummy" length changed from 10 to 8,
!    otherwise read error occurs on prtm file.
! 2011/11/07, CP: Tidied up comments but no actual code change
! 2012/01/20, CP: changed reading of buf prtm array
! 2012/08/23, MJ: Uses initial file as template for netcdf read.
! 2012/08/24, MJ: Implements prtm file read
! 2012/09/20, CP: Assigned channel index to y_id
! 2012/10/04, CP: Added new sp and tskin variable
! 2012/11/03, MST: AVHRR chanid hardcoded
! 2012/11/14, CP: Changed layers to levels
! 2013/11/21, MJ: Fixed reading of rad. information wrt channels
! 2014/01/31, MJ: removed hardcoded parts for avhrr (obsolete)
! 2014/04/18, GM: Made reading of NetCDF input more efficient by avoiding
!    inefficient access patterns and redundancy and cleaned up the code.
! 2014/05/28, GM: Removed unused read of attribute 'Product_Date'.
! 2014/07/23, AP: Grid no longer assumed to defined points rather than the cells
!    centres (as is actually the case).
! 2014/08/01, AP: Remove unused counter fields.
! 2014/08/15, AP: Switching to preprocessor NCDF routines.
! 2014/09/18, AP: Update to RTTOV11 output arrays in the correct shape.
! 2014/09/28, GM: Updated to conform with a new arrangement of dimensions.
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2015/01/30, AP: Remove sp and skint and redundant. Use bottom of T and P
!    arrays in RTM instead.
! 2015/03/11, GM: Do not read wavelength dependent fields if NThermal is equal
!    to 0.
! 2015/04/27, AP: Moved PRTM code into its own routine.
! 2015/07/03, OS: added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
! 2015/08/08, CP: added in ATSR2 capability
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2017/06/21, OS: string name fix for METOP
! 2023/06/02, GT: Fix for Sentinel-3 platform string
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_LwRTM(Ctrl, RTM)

   use Ctrl_m
   use ORAC_Constants_m
   use orac_ncdf_m

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)  :: Ctrl
   type(RTM_t),  intent(out) :: RTM

   ! Local variables

   ! Note on values read from the binary Lw and P RTM files. These files are
   ! generated by RTTOV code, which is compiled with a flag to force reals to
   ! become real(8). The parameter arrays read in via buf, and the lat/lons
   ! etc are explicitly written as real(4) in order to reduce the file size.

   integer                    :: ncid, chan_found, i, j
   character(len=InstNameLen) :: platform, sensor, instname
   integer, allocatable       :: index(:), ChanID(:)
!  real(4), allocatable       :: WvNumber(:)


   ! Open LwRTM data file
   call ncdf_open(ncid, Ctrl%FID%LWRTM, 'Read_LwRTM()')

   ! Ensure instrument info matches the sensor being processed
   if (nf90_get_att(ncid, NF90_GLOBAL, "Sensor", sensor) /= NF90_NOERR .or.&
       nf90_get_att(ncid, NF90_GLOBAL, "Platform", platform) /= NF90_NOERR) then
      write(*,*) 'ERROR: Read_LwRTM(): Could not read global attributes: ', &
                 Ctrl%FID%LWRTM
      stop error_stop_code
   end if
   instname = trim(adjustl(sensor))//'-'//trim(adjustl(platform))

   if (trim(adjustl(instname)) /= trim(adjustl(Ctrl%InstName))) then
      write(*,*) 'ERROR: Read_LwRTM(): Instrument in LWRTM header inconsistent: ', &
                 trim(adjustl(instname)), ' /= ', trim(adjustl(Ctrl%InstName))
      stop error_stop_code
   end if

   allocate(ChanID(RTM%LW%NLWF))
!  allocate(WvNumber(RTM%LW%NLWF))

   ! Read ChanID and WvNumber
   call ncdf_read_array(ncid, "lw_channel_instr_ids", ChanID)
!  call ncdf_read_array(ncid, "lw_channel_wvl", WvNumber)

   if (Ctrl%verbose) write(*,*) &
      'LW channel instrument ids for RTM in LW preprocessing file: ', ChanID

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
   do i = 1, Ctrl%Ind%Ny
      ! Loop over channels in RTM
      do j = 1, RTM%LW%NLWF
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

   if (chan_found /= Ctrl%Ind%NThermal) then
      write(*,*) 'ERROR: Read_LwRTM(): Required instrument channels not ' // &
                 'found in: ', Ctrl%FID%LWRTM
      stop error_stop_code
   end if

   if (Ctrl%Ind%NThermal > 0) then
      ! Allocate arrays
      allocate(RTM%LW%Ems(Ctrl%Ind%NThermal,RTM%Grid%NLon,RTM%Grid%NLat))
      allocate(RTM%LW%Tac(Ctrl%Ind%NThermal,RTM%NP,RTM%Grid%NLon, &
         RTM%Grid%NLat))
      allocate(RTM%LW%Tbc(Ctrl%Ind%NThermal,RTM%NP,RTM%Grid%NLon, &
         RTM%Grid%NLat))
      allocate(RTM%LW%Rac_up(Ctrl%Ind%NThermal,RTM%NP,RTM%Grid%NLon, &
         RTM%Grid%NLat))
      allocate(RTM%LW%Rac_dwn(Ctrl%Ind%NThermal,RTM%NP,RTM%Grid%NLon, &
         RTM%Grid%NLat))
      allocate(RTM%LW%Rbc_up(Ctrl%Ind%NThermal,RTM%NP,RTM%Grid%NLon, &
         RTM%Grid%NLat))

      ! Read data into arrays
      call ncdf_read_array(ncid, "emiss_lw", RTM%LW%Ems, 1, index)
      call ncdf_read_array(ncid, "tac_lw", RTM%LW%Tac, 1, index)
      call ncdf_read_array(ncid, "tbc_lw", RTM%LW%Tbc, 1, index)
      call ncdf_read_array(ncid, "rbc_up_lw", RTM%LW%Rbc_up, 1, index)
      call ncdf_read_array(ncid, "rac_up_lw", RTM%LW%Rac_up, 1, index)
      call ncdf_read_array(ncid, "rac_down_lw", RTM%LW%Rac_dwn, 1, index)
   end if

!  if (allocated(WvNumber)) deallocate(WvNumber)
   if (allocated(ChanID))   deallocate(ChanID)
   if (allocated(index))    deallocate(index)

   ! Close LwRTM input file
   call ncdf_close(ncid, 'Read_LwRTM()')

end subroutine Read_LwRTM
