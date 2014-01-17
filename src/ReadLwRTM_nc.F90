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
!   message      char        Error message to pass to Write_Log  
!   InstName     char        Instrument name
!   Date         char        Date
!   ChanID       alloc int   Channel identifiers
!   WvNumber  alloc real(8)  Central wavenumber
!   index        int         Index to channel
!   i,j,k        int         Loop counters
!   chan_found   int         Flag to indicate channel has been found
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
!   20120823 MJ uses initial file as template for netcdf read.
!   20120824 MJ implements prtm file read
!   20120920 CP assigned channel index to y_id
!   20121004 CP added new sp and tskin variable
!   20121103 MST  AVHRR chanid hardcoded
!   20121114 CP changed layers to levels
! 20131121 MJ fixed reading of rad. information wrt channels
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Read_LwRTM_nc(Ctrl, RTM, status)

   use CTRL_def
   use RTM_def
   use ECP_Constants

   use netcdf

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
   character(256)         :: message
   character(Instnamelen) :: InstName
!  character(8)           :: Date
   integer, allocatable   :: ChanID(:)
   real(4), allocatable   :: WvNumber(:)  
   integer, allocatable   :: index(:)   
   integer                :: i, j, k
   integer                :: chan_found

   !netcdf related
   character(Instnamelen) :: platform
   character(Instnamelen) :: sensor
   character(len=12) :: prod_date_prtm,prod_date_lwrtm
   integer :: ncid,ik,ichan
   integer(kind=nint), allocatable, dimension(:) :: counter_lw,counter_pw,i_pw,j_pw
   real(kind=sreal), allocatable, dimension(:,:) :: dummy1p1
   real(kind=sreal), allocatable, dimension(:,:,:) :: dummy1p2
   real(kind=sreal), allocatable, dimension(:) :: dummy1df
   integer(kind=nint), allocatable, dimension(:,:) :: dummy2dint


!############################################
!METEO FILE
!############################################

!  Open profile file
   if (status == 0) then
      write(*,*) 'PRTM File',trim(adjustl(Ctrl%FID%PRTM))
      ios = nf90_open(path=trim(adjustl(Ctrl%FID%PRTM)),mode = nf90_nowrite,ncid = ncid) 
      if(ios /= 0) then
         status = LwRTMPFileOpenErr
         write(unit=message, fmt=*) &
              'Read_LwRTM: Error opening RTM profile file ', trim(adjustl(Ctrl%Fid%PRTM))
         call Write_Log(Ctrl, trim(message), status)
         write(*,*) 'Read_LwRTM: Error opening RTM profile file ', trim(adjustl(Ctrl%Fid%PRTM))
         stop
      else
         !        Read date from attributes
         ios=nf90_get_att(ncid, NF90_GLOBAL, "Product_Date", prod_date_prtm)     
         if (ios /= 0) then
            status = LwRTMProfDateErr
            call Write_Log(Ctrl, 'Read_LwRTM: Error reading profile file',   status)
            write(*,*) 'Read_LwRTM: Error reading profile file',status
            stop
         end if

         if (status == 0) then
            !           Allocate arrays

            allocate(RTM%LW%lat(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))
            allocate(RTM%LW%lon(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))   

            allocate(RTM%LW%skint(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))
            allocate(RTM%LW%sp(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon))   
            
            allocate(RTM%LW%P(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))
            allocate(RTM%LW%T(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))
            allocate(RTM%LW%H(RTM%LW%Grid%NLat, RTM%LW%Grid%NLon, RTM%LW%NP))
            
            !read cell indices of 1D arrays
            allocate(counter_pw(RTM%LW%Grid%NLatLon))
            allocate(i_pw(RTM%LW%Grid%NLatLon))
            allocate(j_pw(RTM%LW%Grid%NLatLon))
            !succeesive cell count in preprocessing
            call nc_read_array_1d_int_to_int_orac(ncid,RTM%LW%Grid%NLatLon,"counter_pw",counter_pw,0)

            !i is longitude in preprocessing
            call nc_read_array_1d_int_to_int_orac(ncid,RTM%LW%Grid%NLatLon,"i_pw",i_pw,0)
            !j is latitude in preprocessing
            call nc_read_array_1d_int_to_int_orac(ncid,RTM%LW%Grid%NLatLon,"j_pw",j_pw,0)
            
            allocate(dummy2dint(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon))
            dummy2dint=-1
            dummy2dint=transpose(reshape(counter_pw,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

            deallocate(dummy2dint)

            allocate(dummy1df(RTM%LW%Grid%NLatLon))
            dummy1df=real_fill_value

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"lon_pw",dummy1df,0)
            RTM%LW%lon=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))
            dummy1df=real_fill_value

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"lat_pw",dummy1df,0)
            RTM%LW%lat=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))
            dummy1df=real_fill_value

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"skint_pw",dummy1df,0)
            RTM%LW%skint=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))
            dummy1df=real_fill_value

            call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%Grid%NLatLon,"explnsp_pw",dummy1df,0)
            RTM%LW%sp=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))
            deallocate(dummy1df)

            allocate(dummy1p1(RTM%LW%Grid%NLatLon,1))
            dummy1p1=real_fill_value
            allocate(dummy1df(RTM%LW%Grid%NLatLon))
            dummy1df=real_fill_value            

            do ik=1,RTM%LW%NP
               call nc_read_array_1p1_float_orac(ncid,RTM%LW%Grid%NLatLon,ik,"tprofile_lev",dummy1p1,0)
               dummy1df=dummy1p1(:,1)
               RTM%LW%T(:,:,ik)=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

               call nc_read_array_1p1_float_orac(ncid,RTM%LW%Grid%NLatLon,ik,"pprofile_lev",dummy1p1,0)
               dummy1df=dummy1p1(:,1)
               RTM%LW%P(:,:,ik)=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

               call nc_read_array_1p1_float_orac(ncid,RTM%LW%Grid%NLatLon,ik,"gphprofile_lev",dummy1p1,0)
               dummy1df=dummy1p1(:,1)
               RTM%LW%H(:,:,ik)=transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))
            enddo

            deallocate(dummy1p1)
            deallocate(dummy1df)

         end if

      end if

      deallocate(counter_pw)
      deallocate(i_pw)
      deallocate(j_pw)
      !close  prtm input file
   ios=nf90_close(ncid)
   if (ios==0) write(*,*)'Read PRTM data ok'     
   

!############################################
!LW FILE
!############################################

   
!  Open RTM data file
   write(*,*) 'file',trim(adjustl(Ctrl%FID%LWRTM))
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%LWRTM)),mode = nf90_nowrite,ncid = ncid) 
   write(*,*) 'ios', ios
   if (ios /= NF90_NOERR) then
      status = LwRTMRTMFileOpenErr ! Return error code
      write(unit=message, fmt=*) 'Read_LwRTM: Error opening RTM file ', trim(adjustl(Ctrl%Fid%LWRTM))
      call Write_Log(Ctrl, trim(message), status)
      write(*,*) 'Read_LwRTM: Error opening RTM file ', trim(adjustl(Ctrl%Fid%LWRTM))
      stop
   else

      !     Read RTM data file
      
      !     Instrument
      ios=nf90_get_att(ncid, NF90_GLOBAL, "Sensor_Name", sensor)
      ios=nf90_get_att(ncid, NF90_GLOBAL, "Platform", platform)
      if (ios /= 0) then
         status = LwRTMRTMInstErr ! Return error code
         call Write_Log(Ctrl, 'Read_LwRTM: error reading instrument name', &
              status)
         write(*,*) 'Read_LwRTM: error reading instrument name', status
         stop
      else
         !MJ ORG instname=trim(adjustl(sensor))//'-'//trim(adjustl(platform))

         if (sensor =='AATSR') then
            instname=trim(adjustl(sensor))
         else
            instname=trim(adjustl(sensor))//'-'//trim(adjustl(platform))
            !         instname=trim(adjustl(sensor))//trim(adjustl(platform))
         endif

         InstName = adjustl(InstName)
         if (trim(adjustl(InstName)) /= trim(adjustl(Ctrl%Inst%Name))) then
            status = LwRTMRTMInstErr ! Return error code
            call Write_Log(Ctrl, &
                 & 'Read_LwRTM: RTM file; header instrument disagrees with filename',&
                 & status)
            write(*,*) 'Read_LwRTM: RTM file; header instrument disagrees with filename',&
                 & status
            stop
         end if
      end if
      
      if (status == 0) then 
         !Read product date and time from netcdf global attributes
         ios=nf90_get_att(ncid, NF90_GLOBAL, "Product_Date", prod_date_lwrtm)     
         if (ios /= 0) then
            status = LwRTMRTMDateErr ! Return error code
            call Write_Log(Ctrl, 'Read_LwRTM: error reading date', &
                 & status)
            write(*,*) 'Read_LwRTM: error reading date', &
                 & status
            stop
         end if
         !         if (trim(Date) /= trim(Ctrl%Inst%Date)) then
         !             status = LwRTMRTMDateErr ! Return error code
         !            write(unit=message, fmt=*) &
         !                 'Read_LwRTM: RTM file; header date disagrees with filename' 
         !            call Write_Log(Ctrl, trim(message), status)
         !         end if
      end if
      
      !     For this set of reads, if read fails (non-zero iostat), set status
      !     immendiately but report error later since the message is the same for
      !     all cases.
      
      !     Allocate size of ChanID and WvNumber
      if (status == 0) then 
         allocate(ChanID(RTM%LW%NLWF))
         allocate(WvNumber(RTM%LW%NLWF))

!        Read ChanID and WvNumber
         call nc_read_array_1d_int_to_int_orac(ncid,RTM%LW%NLWF,"lw_channel_instr_ids",ChanID,0)
         call nc_read_array_1d_float_to_float_orac(ncid,RTM%LW%NLWF,"lw_channel_wvl",WvNumber,0)
      endif



      !MST do some hardcoding here for AVHRR
      ! MJ ORG if(sensor .eq. 'AVHRR') ChanID(:)=(/3,4,5/)
      if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA15") ChanID(:)=(/3,4,5/)
      if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA16") ChanID(:)=(/3,4,5/)
      if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA17") ChanID(:)=(/4,5/)
      if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA18") ChanID(:)=(/3,4,5/)

      write(*,*) 'LW channel instrument ids for RTM in LW preprocessing file',ChanID

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

         !this is the loop over the requested channels
         !MJ ORG do i = Ctrl%Ind%ThermalFirst, Ctrl%Ind%ThermalLast
         do i = 1,Ctrl%Ind%Ny
            !           Loop over channels in RTM
            do j = 1,RTM%LW%NLWF
               !              Signal that the required channel has been found by incrementing
               !              chan_found and break out of the inner loop to start search for
               !              next instrument channel
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
            call Write_Log(Ctrl, trim(message), status)
            write(*,*) 'Read_LwRTM: RTM file; required instrument channels not found' 
            stop
         end if

      end if  ! End of "required thermal channels" status check
   
      if (status == 0) then 
!        Allocate sizes of arguments in call to Read_LwRTM
         allocate(RTM%LW%ems(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal))
         allocate(RTM%LW%Tbc(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
         allocate(RTM%LW%Tac(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
         allocate(RTM%LW%Rac_up(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
         allocate(RTM%LW%Rac_dwn(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal,RTM%LW%NP))
         allocate(RTM%LW%Rbc_up(RTM%LW%Grid%NLat,RTM%LW%Grid%NLon,Ctrl%Ind%NThermal, RTM%LW%NP))

         !read cell indices of 1D array
         allocate(counter_lw(RTM%LW%Grid%NLatLon))
         call nc_read_array_1d_int_to_int_orac(ncid,RTM%LW%Grid%NLatLon,"counter_lw",counter_lw,0)
         deallocate(counter_lw)
      
         !write(*,*) counter_lw

         allocate(dummy1p1(RTM%LW%Grid%NLatLon,1))
         dummy1p1=real_fill_value
         allocate(dummy1df(RTM%LW%Grid%NLatLon))
         dummy1df=real_fill_value
         allocate(dummy1p2(RTM%LW%Grid%NLatLon,1,1))
         dummy1p2=real_fill_value
         !read al wl related stuff here
         !write(*,*) Ctrl%Ind%ThermalFirst, Ctrl%Ind%ThermalLast,Ctrl%Ind%NThermal
         !pause
         !MJ ORG do i = Ctrl%Ind%ThermalFirst, Ctrl%Ind%ThermalLast
         ichan=0
         do i = 1,Ctrl%Ind%Ny
            do j = 1,RTM%LW%NLWF
               !write(*,*) i,Ctrl%Ind%Y_Id(i),j,ChanID(j)
               !pause
               if (Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)) == ChanID(j)) then
                  ichan=ichan+1

                  call nc_read_array_1p1_float_orac(ncid,RTM%LW%Grid%NLatLon,j,"emiss_lw",dummy1p1,0)
                  dummy1df=dummy1p1(:,1)
                  !MJ ORG RTM%LW%ems(:,:,i-Ctrl%Ind%ThermalFirst+1)=&
                  RTM%LW%ems(:,:,ichan)=&
                       & transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))
                  
                  do ik=1,RTM%LW%NP
                     call nc_read_array_1p2_float_orac(ncid,RTM%LW%Grid%NLatLon,j,ik,"tac_lw",dummy1p2,0)
                     dummy1df=dummy1p2(:,1,1)
                     !MJ ORG RTM%LW%Tac(:,:,i-Ctrl%Ind%ThermalFirst+1,ik)=&
                     RTM%LW%Tac(:,:,ichan,ik)=&
                          & transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))
                     
                     call nc_read_array_1p2_float_orac(ncid,RTM%LW%Grid%NLatLon,j,ik,"tbc_lw",dummy1p2,0)
                     dummy1df=dummy1p2(:,1,1)
                     !MJ ORG RTM%LW%Tbc(:,:,i-Ctrl%Ind%ThermalFirst+1,ik)=&
                     RTM%LW%Tbc(:,:,ichan,ik)=&
                          & transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

                     call nc_read_array_1p2_float_orac(ncid,RTM%LW%Grid%NLatLon,j,ik,"rbc_up_lw",dummy1p2,0)
                     dummy1df=dummy1p2(:,1,1)
                     !MJ ORG RTM%LW%Rbc_up(:,:,i-Ctrl%Ind%ThermalFirst+1,ik)=&
                     RTM%LW%Rbc_up(:,:,ichan,ik)=&
                          & transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

!                           write(*,*) 'rbc_up',RTM%LW%Rbc_up(:,:,i-Ctrl%Ind%ThermalFirst+1,ik)
!pause
                     call nc_read_array_1p2_float_orac(ncid,RTM%LW%Grid%NLatLon,j,ik,"rac_up_lw",dummy1p2,0)
                     dummy1df=dummy1p2(:,1,1)
                     !MJ ORG RTM%LW%Rac_up(:,:,i-Ctrl%Ind%ThermalFirst+1,ik)=&
                     RTM%LW%Rac_up(:,:,ichan,ik)=&
                          & transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

                     call nc_read_array_1p2_float_orac(ncid,RTM%LW%Grid%NLatLon,j,ik,"rac_down_lw",dummy1p2,0)
                     dummy1df=dummy1p2(:,1,1)
                     !MJ ORG RTM%LW%Rac_dwn(:,:,i-Ctrl%Ind%ThermalFirst+1,ik)=&
                     RTM%LW%Rac_dwn(:,:,ichan,ik)=&
                          & transpose(reshape(dummy1df,(/RTM%LW%Grid%NLon,RTM%LW%Grid%NLat/)))

                  enddo

                  write(*,*) 'max/min TBC 1', ichan,Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)),&
                       & maxval(RTM%LW%Tbc(:,:,ichan,1)),minval(RTM%LW%Tbc(:,:,ichan,1))
                  write(*,*) 'max/min TBC RTM%LW%NP', ichan,Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)),&
                       & maxval(RTM%LW%Tbc(:,:,ichan,RTM%LW%NP)),&
                       & minval(RTM%LW%Tbc(:,:,ichan,RTM%LW%NP))
                  write(*,*) 'max/min TAC 1', ichan,Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)),&
                       & maxval(RTM%LW%Tbc(:,:,ichan,1)),minval(RTM%LW%Tbc(:,:,ichan,1))
                  write(*,*) 'max/min TAC RTM%LW%NP', ichan,Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)),&
                       & maxval(RTM%LW%Tbc(:,:,ichan,RTM%LW%NP)),&
                       & minval(RTM%LW%Tbc(:,:,ichan,RTM%LW%NP))

               end if

            end do


         end do
         
         deallocate(dummy1p1)
         deallocate(dummy1p2)
         deallocate(dummy1df)

      endif

!     Close RTM file and deallocate allocatable arrays if not required

      if (status == LwRTMReadErr) &
           call Write_Log(Ctrl, 'Read_LwRTM: error reading from Lw file', &
            status)


      !close  lwrtm input file
      ios=nf90_close(ncid)

      if (allocated(index)) deallocate(index)
      if (allocated(WvNumber)) deallocate(WvNumber)
      if (allocated(ChanID)) deallocate(ChanID)
   
   end if  ! End of check that ios is ok on opening Lw RTM file.

   if (status==0) write(*,*)'Read LW RTM data ok'

!      write(*,*) 'IN',status
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

 end subroutine Read_LwRTM_nc
