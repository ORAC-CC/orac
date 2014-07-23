!-------------------------------------------------------------------------------
! Name:
!    Get_SPixel
!
! Purpose:
!    Controls the extraction of the current super-pixel data from the
!    'whole-image' data arrays.
!    Also calculates the surface to TOA slant path transmittances.
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    Ctrl     struct       In   Control structure
!    SAD_Chan struct       In   SAD channel structure
!    MSI_Data struct       In   Data structure. Contains the multi-spectral
!                               image measurements, location values, geometry
!                               etc for the current image segment, from which
!                               the current SPixel values will be extracted.
!    RTM      alloc struct In   RTM structure
!    SPixel   alloc struct Both Super-pixel structure
!    status   integer      Out  Error status. Note most problems identified in
!                               this routine do not lead to non-zero status
!                               values as they affect the super-pixel data only.
!                               Status is not currently passed to subordinate
!                               routines (local stat is used instead). It is
!                               assumed that no subordinate can identify a
!                               serious error condition.
!
! Algorithm:
!    Check data in each of the Data structure arrays
!    Flag pixels containing bad data by setting zeros in the super pixel mask.
!    Flag reasons by setting bits in the quality control value SPixel%QC
!       (non-zero QC is not fatal at this stage, either to the super-pixel or to
!       the ECP)
!    For each case below, if the spixel cannot be used, set QC flag bit
!    SPixNoProc: indicates the error is fatal for the SPixel, but not the ECP.
!    Call Get_Location
!    Call Get_Cloudflags
!    If averaging method 'central'
!       Check that central pixel does not contain bad data (abort current super
!       pixel if it does),
!    If averaging method 'all'
!       Calculate number of good pixels in super pixel (if below threshold
!       Ctrl%SPix%Threshold then abort current super pixel,
!    If averaging method 'cloudy'
!       Calculate number of good pixels in super pixel (if there are no good
!       cloudy pixels then abort current super pixel).
!    Call Get_illum
!    Call Get_Geometry
!    Call Get_RTM
!    Call Get_Measurements
!    Call Get_Surface
!    Call Get_X to set Apriori and First Guess
!    if there are solar channels in use for the SPixel:
!       - Calculate the surface to TOA slant path transmittances for use in
!         forward model (airmass factors  - SEC_o/v - taken from Get_Geometry).
!       - Set solar constant values for the SPixel (same for all SPixels with
!         solar channels in use? Move elsewhere?)
!
!    Note on error handling: it is assumed that any error identified at this
!    stage is fatal only for the current super-pixel. Hence status can be used
!    to flag a problem in a super-pixel. The calling routine can set status=0
!    after checking.
!
! Local variables:
!    Name Type Description
!
! History:
!    29th Nov 2000, Kevin M. Smith:
!       Original version
!    19th Dec 2000, Kevin M. Smith:
!       Replaced Data_... arrays with Data structure.
!    17th Jan 2001, Kevin M. Smith:
!       Added error checking on land/sea flags.
!    24th Jan 2001, Kevin M. Smith:
!       Moved allocations to ECP main on integration.
!       Moved calculation of central pixel coords to ECP.
!    26th Jan 2001, Kevin M. Smith:
!       Added calculations of solar and viewing slant path transmittances.
!     6th Mar 2001, Andy Smith:
!       Change to setting of Tsf_o,v values in SPixel%RTM. Tsf_o,v now appear in
!       the overall RTM struct rather than the RTM%LW and SW structs.
!       Added calculation of the Ref_clear values.
!     7th Mar 2001, Andy Smith:
!       Change in LW Tsf_o, v calculations.
!       Additional breakpoint output
!       GetRTM needs SAD_Chan as an argument.
!       Changed order of arguments (inputs first).
!    15th Mar 2001, Andy Smith:
!       Added code to set the central pixel absolute coordinates (if required)
!       and the top right-hand corner coordinates.
!       Amended solar zenith angle check. Previously disallowed data where SolZen
!       > 90 degrees. Do not check. SolZen may be treated differently in
!       different data sets when > 90.
!       Using pre-defined constant names for super-pixel averaging methods.
!       Rs divided by Sec_o to take account of solar angle as soon as Rs is set
!       by Get_Surface/Get_Rs. REF_Clear and dREF_Clear_dRs calculations updated.
!     4th Apr 2001, Andy Smith:
!       Removed brackets specifying array section from whole array assignments
!       where the whole array is used, as experience elsewhere seems to show
!       that "array = " works faster than "array(:) = ".
!    17th May 2001,  Andy Smith:
!       New argument SAD_Chan required by Get_Measurements.
!     5th June 2001,  Andy Smith:
!       Added call to Get_X to set a priori and first guess, plus new argument
!       SAD_CloudClass required by Get_X.
!    15th Jun 2001, Andy Smith:
!       Changed error message string assignments/writes.
!       Long message strings were wrapped over two lines with only one set of
!       quotes around the whole string (including the line continuation marker).
!       Original code works on DEC but not Linux.
!    10th Jul 2001, Andy Smith:
!       Attempt to rationalise error handling and SPixel checking. Status was
!       used to flag conditions that were fatal for the SPixel. Spixel%QC was
!       used to flag out of range values etc but these were not considered fatal.
!       Changing to use SPixel%QC for all flagging. As a result some
!       QC settings now indicate conditions are fatal for the SPixel. This will
!       simplify the main ECP loop where it checks whether a particular SPixel
!       should be processed and leaves status for flagging of "real" errors.
!       (Note most subordinates flag data problems via status, but no
!       subordinate currently detects any error that is fatal for the program).
!       Replaced Kevin's BITS routine for setting bit flags with the intrinsic
!       ibset.
!     3rd August 2001, Andy Smith:
!       Bug fix: ibset arguments were the wrong way round!
!       Updates for image segmentation. Selection of values from the MSI Data
!       structure arrays now need to use a y value that refers to the image
!       segment currently held in memory rather than the whole image area.
!       X co-ords are unchaged since the segment is the full image width.
!       Renamed structure Data to MSI_Data since Data is a reserved word (hasn't
!       caused any problems so far but it might).
!       Moved setting of SPixel Xc, Yc. Required before GetCloudFlags call if
!       averaging method is central.
!    18th Sept 2001, Andy Smith:
!       Removed Write_Log call when there are no cloudy pixels in the
!       super-pixel. Could lead to lots of unnecessary log output.
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-applying changes from late 2001/2002.
!       2nd Nov 2001, Andy Smith:
!       Added zeroing of first-guess and a priori state vectors if SPixel
!       QC flag indicates no processing. Otherwise these vectors retain the
!       value from the previous pixel and are output into the diag file with the
!       old values.
!    28th Nov 2001, Andy Smith:
!       Bug fix to range checking of MSI reflectances and brightness temps.
!       Moved the check on "stat" and call to WriteLog inside the channel loop.
!       Previously, a "bad" stat value in, say, channel 1, could be overwritten
!       by a "good" value from a higher channel.
!       Replaced constant values in range checks with named constants.
!       Updated checking of cloud flags and errors in pixels required for the
!       selected averaging method. Removed error logging for these situations:
!       errors are already logged during limit checking.
!     6th Dec 2001, Andy Smith:
!       Bug fix to check on no. of "good" pixels vs. Ctrl%SPix%Threshold.
!       Previously, the check was:
!       if (real(SPixel%NAverage/Ctrl%SPix%NPixel) < Ctrl%SPix%Threshold)
!       This takes the real value of the result of an integer divide and
!       compares to the real Threshold fraction. The integer divide returns
!       1 if NAverage = NPixel and 0 if NAverage < NPixel. Both values should be
!       converted to real before division.
!    14th Aug 2002, Caroline Poulsen:
!       Fractional error is assigned according to the neighbouring pixels when
!       resolution is 1.
!    23rd Dec 2002, Andy Smith:
!       Move the Get_Location call so that Lat-Lon infomration is available for
!       all pixels in the image, rather than just the cloudy ones.
!    23rd Feb 2011, Andy Smith:
!       MSI_Data%CloudFlags now an array of floats, to match current ORAC data.
!    16th Mar 2011, Andy Smith:
!       Added some extra breakpoint outputs: lat, lon etc.
!       Added MSI temp/reflectance value to message for out of range values
!       (done in Feb 2011).
!    22nd Mar 2011, Andy Smith:
!       Removal of phase change. Only 1 cloud class required for each retrieval
!       run. SADCloudClass array dimensions changed.
!    24th Mar 2011, Andy Smith:
!       Removal of super-pixelling, i.e. no averaging of flags etc needed.
!       Any super-pixelling required will now be done in pre-processing.
!       Resolution for the retrieval will be fixed at 1 pixel.
!       Remove calculation of Spixel%Loc%Xc and Xn, simplify calls to
!       subordinate functions - no need for range of pixel locations.
!       Removed calculation of Fracnext (used in XMDAD to set error).
!     7th Apr 2011, Andy Smith:
!       Removal of selection methods SAD and SDAD. GetSurface argument list
!       updated - SAD_Chan no longer needed.
!    20th Apr 2011, Andy Smith:
!       Extension to handle multiple instrument views. The viewing geometry
!       becomes a set of arrays, e.g. 1 value of sat. zen angle per view.
!       Extend checks on MSI_Data geometry values and all references to the
!       SPixel%Geom sub-structure.
!    20th May 2011, Andy Smith:
!       Multiple instrument views (2). Modified breakpoints outout to check for
!       0 solar channels present.
!     8th Jun 2011, Andy Smith:
!       Reduced/removed logging to improve performance. Tests show that writing
!       to the ASCII log file can extend total time to process an orbit from a
!       few minutes to several hours.
!       Enclosed in ifdef DEBUG than removed, for easier re-introduction.
!       Removed reference to "super-pixel" from log messages.
!     5th Aug 2011, Caroline Poulsen: Remove ref to cloudclass
!    29th Sep 2011, Caroline Poulsen: Updated log output to be more imformative
!       and give the channel number of the missing data
!    16th Feb 2012, Caroline Poulsen: Updated file to fix bug that produced
!       errors when night views were processed.
!    24th Jul 2013, Adam Povey: Fixed BKP code
!    30th Apr 2014, Greg McGarragh: Cleaned up the code.
!    17th June 2014, Caroline Poulsen modified code so retrieval performed if a single ir channel is missing
!
! Bugs:
!   Risk: changes from 2001/2 re-applied in Feb 2011 may be "contaminated" by
!   later changes made for development of aerosol retrieval etc, since the
!   source of changes is a copy of the code from 2006 (last recorded mod).
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Get_SPixel(Ctrl, conf, SAD_Chan, MSI_Data, RTM, SPixel, status)

   use config_s
   use CTRL_def
   use Data_def
   use ECP_Constants
   use RTM_def
   use SAD_Chan_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t), intent(in)      :: Ctrl
   type(config_struct)           :: conf
   type(SAD_Chan_t), intent(in)  :: SAD_Chan(Ctrl%Ind%Ny)
   type(Data_t), intent(in)      :: MSI_Data
   type(RTM_t), intent(in)       :: RTM
   type(SPixel_t), intent(inout) :: SPixel
   integer, intent(out)          :: status

!#define DEBUG

   ! Define local variables

   integer        :: i, j, view, irbad
   real           :: minsolzen
   integer        :: stat ! Local status value
#ifdef DEBUG
   character(180) :: message
#endif
#ifdef BKP
   integer :: bkp_lun   ! Unit number for breakpoint file
   integer :: ios       ! I/O status for breakpoint file
   integer :: StartChan ! First valid channel for pixel, used in breakpoints.
#endif

   ! Set status to zero
   status = 0

   ! Perform quality control and pixel error checking

   ! Initialise Mask
   SPixel%Mask = 1

   ! Initialise quality control flag
   SPixel%QC = 0

   ! Check for pixel values out of range. QC is set as information to accompany
   ! a retrieval. stat is reset to 0 after each test. The SPixel Mask is set to
   ! flag problems in particular pixels within the Super-Pixel. If any one pixel
   ! has a problem (flagged by a 0 value) this is ok, but if all pixels are
   ! flagged the super-pixel cannot be used (checked after these tests).

   ! Check Cloud flags (0 or 1)
   stat = 0
   call Check_FloatArray(1, 1, MSI_Data%CloudFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0), &
        & SPixel%Mask, CloudMax, CloudMin, stat)
   if (stat > 0) then
#ifdef DEBUG
      write(unit=message, fmt=*) &
           & 'Get_SPixel: WARNING - Found cloud flag out of range in pixel at:', &
           & SPixel%Loc%X0, SPixel%Loc%Y0
      !write(*,*) trim(message)
      call Write_log(Ctrl, trim(message), stat)
#endif
       SPixel%QC = ibset(SPixel%QC, SPixCloudFl)
   end if

   ! Land/Sea flags (0 or 1)
   stat = 0
   call Check_ByteArray(1, 1, &
        & MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0), SPixel%Mask, &
        & FlagMax, FlagMin, stat)
   if (stat > 0) then
#ifdef DEBUG
      write(unit=message, fmt=*) &
           & 'Get_SPixel: WARNING - Found land/sea flag out of range in pixel at:', &
           & SPixel%Loc%X0, SPixel%Loc%Y0
      write(*,*) trim(message)
      call Write_log(Ctrl, trim(message), stat)
#endif
      SPixel%QC = ibset(SPixel%QC, SPixLandFl)
    end if

!  Make this work if pixel is in daylight
!
!  Geometry - Solar zenith (between 0o and 90o)
!  stat = 0
!  call Check_FloatArray(Ctrl%Resoln%Space, Ctrl%Resoln%Space, &
!       MSI_Data%Geometry%Sol(SPixel%Loc%X0:SPixel%Loc%Xn, &
!       SPixel%Loc%YSeg0:SPixel%Loc%YSegn), SPixel%Mask, 90.0, 0.0, stat)
!
!  if (stat > 0) then
!     write(unit=message, fmt=*) 'Get_SPixel: WARNING - Found solar zenith angle &
!                                 out of range in super pixel starting at:', &
!                                 SPixel%Loc%X0, SPixel%Loc%Y0
!     write(*,*) trim(message)
!     call Write_log(Ctrl, trim(message), stat)
!     SPixel%QC = ibset(SPixel%QC, SPixSolZen)
!  end if

   ! Geometry - Satellite zenith (between 0o and 90o)
   stat = 0
   do view=1,Ctrl%Ind%Nviews
      call Check_FloatArray(1, 1, &
           & MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%YSeg0, view), SPixel%Mask, &
           & SatZenMax, SatZenMin, stat)
      if (stat > 0) then
#ifdef DEBUG
         write(unit=message, fmt=*) &
              & 'Get_SPixel: WARNING - Found satellite zenith angle out of range ' &
              & // 'in pixel at:', SPixel%Loc%X0, SPixel%Loc%Y0 , &
              & MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%YSeg0, view)
         write(*,*) trim(message)
         call Write_log(Ctrl, trim(message), stat)
#endif
         SPixel%QC = ibset(SPixel%QC, SPixSatZen)
      end if
   end do

   ! Geometry - Azimuth (between 0o and 180o)
   stat = 0
   do view=1,Ctrl%Ind%Nviews
      call Check_FloatArray(1, 1, &
           & MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%YSeg0, view), SPixel%Mask, &
           & RelAziMax, RelAziMin, stat)

      if (stat > 0) then
#ifdef DEBUG
         write(unit=message, fmt=*) &
              & 'Get_SPixel: WARNING - Found azimuth angle out of range in pixel at:', &
              & SPixel%Loc%X0, SPixel%Loc%Y0, &
              & MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%YSeg0, view)
         write(*,*) trim(message)
         call Write_log(Ctrl, trim(message), stat)
#endif
         SPixel%QC = ibset(SPixel%QC, SPixRelAzi)
      end if
   end do

   ! Location - Latitude (between -90o and 90o)
   stat = 0
   call Check_FloatArray(1, 1, &
        & MSI_Data%Location%Lat(SPixel%Loc%X0, SPixel%Loc%YSeg0), SPixel%Mask, &
        & LatMax, LatMin, stat)
   if (stat > 0) then
#ifdef DEBUG
      write(unit=message, fmt=*) &
           & 'Get_SPixel: WARNING - Found location latitude out of range in pixel at: ', &
           & SPixel%Loc%X0, SPixel%Loc%Y0
      write(*,*) trim(message)
      call Write_log(Ctrl, trim(message), stat)
#endif
      SPixel%QC = ibset(SPixel%QC, SPixLat)
   end if

   ! Location - Longitude (between -180o and 180o)
   stat = 0
   call Check_FloatArray(1, 1, &
        MSI_Data%Location%Lon(SPixel%Loc%X0, SPixel%Loc%YSeg0), SPixel%Mask, &
        & LonMax, LonMin, stat)
   if (stat > 0) then
#ifdef DEBUG
      write(unit=message, fmt=*) &
           & 'Get_SPixel: WARNING - Found location longitude out of range in pixel at: ', &
           & SPixel%Loc%X0, SPixel%Loc%Y0
      write(*,*) trim(message)
      call Write_log(Ctrl, trim(message), stat)
#endif
      SPixel%QC = ibset(SPixel%QC, SPixLon)
   end if

   ! MSI - Reflectances (between 0 and 1)
   !
   ! Loop over shortwave channels
   minsolzen=minval(MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%YSeg0, :))
   if (minsolzen < Ctrl%MaxSolzen) then
      stat = 0

      do i = 1,Ctrl%Ind%Nsolar
         if (conf%channel_mixed_flag_use(Ctrl%Ind%ysolar_msi(i)) .eq. 0) then
            call Check_FloatArray(1, 1, &
                 & MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0,Ctrl%Ind%ysolar_msi(i)), SPixel%Mask, &
                 & RefMax, RefMin, stat)
         endif
         if (stat > 0) then
#ifdef DEBUG
            write(unit=message, fmt=*) &
                 & 'Get_SPixel: WARNING - Found MSI reflectance out of range in pixel at: ', &
                 & SPixel%Loc%X0, SPixel%Loc%Y0, &
                 & MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0, Ctrl%Ind%ysolar_msi(i)),'ch:',Ctrl%Ind%ysolar_msi(i)
            write(*,*) trim(message)
            call Write_log(Ctrl, trim(message), stat)
#endif
            SPixel%QC = ibset(SPixel%QC, SPixRef)
         end if
      end do
   endif

   ! MSI - Temperatures (between 150.0K and 330.0K)
   stat = 0
irbad=0
   do i = 1,Ctrl%Ind%Nthermal
      call Check_FloatArray(1, 1, &
           & MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0, Ctrl%Ind%ythermal_msi(i)), SPixel%Mask, &
           & BTMax, BTMin, stat)
      if (stat > 0) then
	irbad =irbad+1

#ifdef DEBUG
         write(unit=message, fmt=*) &
               & 'Get_SPixel: WARNING - Found MSI temperature out of range in pixel at: ', &
               & SPixel%Loc%X0, SPixel%Loc%Y0, ' chan ',Ctrl%Ind%ythermal_msi(i), &
               & MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0, Ctrl%Ind%ythermal_msi(i))
         write(*,*) trim(message)
         call Write_log(Ctrl, trim(message), stat)
#endif
	if (irbad > 1) then

         	SPixel%QC = ibset(SPixel%QC, SPixTemp)
		SPixel%Mask=0
	else 
! its ok to have one missing ir channel
		stat=0
		SPixel%Mask=1
	end if

      end if
   end do

   stat = 0

   ! End of range checking. From here on any non-zero stat value is fatal for
   ! the super-pixel. Set the QC flag bits both for the individual error
   ! condition and to indicate no processing for the SPixel.

   ! Get_Location call moved here so that lat-lon info is available for every
   ! super-pixel. Leave the stat value - don't re-set to 0 after the call - if
   ! there was an error retrieving loc information we want to know.
   !
   ! Calculate NMask: the tests above have been setting 0's in the super-pixel
   ! mask where the data for a given pixel fails each check.
   !
   ! Removal of super-pixel averaging: Get_Location redundant. Just assign lat
   ! and lon values directly. QC setting redundant: Get_Location had no
   ! functionality to set stat non-0.

   if (stat == 0) then
      SPixel%NMask = SPixel%Mask
      SPixel%Loc%Lat = MSI_Data%Location%Lat(SPixel%Loc%X0, SPixel%Loc%YSeg0)
      SPixel%Loc%Lon = MSI_Data%Location%Lon(SPixel%Loc%X0, SPixel%Loc%YSeg0)
!     call Get_Location(Ctrl, SPixel, MSI_Data, stat)
!     if (stat /= 0) Spixel%QC = ibset(Spixel%QC, SPixLoc)
   end if

   ! If all Mask flags are 0 there are no good pixels in the current SPixel, do
   ! not process. Set QC flag and report to the log.


   if (SPixel%NMask == 0) then
      Spixel%QC = ibset(Spixel%QC, SPixAll)
      stat = SPixelInvalid ! Entire super-pixel is invalid
#ifdef DEBUG
      write(unit=message, fmt=*) &
           & 'Get_SPixel: NMask zero in pixel at:', SPixel%Loc%X0, SPixel%Loc%Y0
       write(*,*) trim(message)
       call Write_log(Ctrl, trim(message), stat)
#endif



stat=0
   else
      ! Get cloud flags before checking for cloudy method

      ! Removal of Super-pixel averaging: replace call to GetCloudFlags with a
      ! simple assignment. "Flags" is now 1 single flag.

!     call Get_CloudFlags(Ctrl, SPixel, MSI_Data, stat)

      SPixel%Cloud%Flags = MSI_Data%CloudFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0)
      Spixel%Cloud%Fraction = SPixel%Cloud%Flags


      if (SPixel%Cloud%Fraction == 0) then
         ! No cloud in SPixel. Don't process.

         Spixel%QC = ibset(Spixel%QC, SPixNoCloud)
         stat = SPixelCloudFrac
#ifdef DEBUG
         write(unit=message, fmt=*) &
            'Get_SPixel: zero cloud fraction in super pixel starting at:' &
            , SPixel%Loc%X0, SPixel%Loc%Y0
         write(*,*) trim(message)
         call Write_log(Ctrl, trim(message), stat)
#endif
      end if

      if (stat == 0) then
         ! Call 'Get_' subroutines. Use of stat here assumes that no subordinate
         ! routine returns a non-zero status value unless it is to flag a super-
         ! pixel data problem.

         if (stat == 0) then
	! write(*,*)'getspixl msi',MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0,:)
            call Get_illum(Ctrl, SPixel, MSI_Data, stat)
            if (stat /= 0) then
               !write(*,*) 'WARNING: Get_illum', stat
               Spixel%QC = ibset(Spixel%QC, SPixillum)
            endif
         end if



         if (stat == 0) then
            call Get_Geometry(Ctrl, SPixel, MSI_Data, stat)
            if (stat /= 0) then
               !write(*,*)  'WARNING: Get_Geometry', stat
               Spixel%QC = ibset(Spixel%QC, SPixGeom)
            endif
         end if

         if (stat == 0) then
            call Get_RTM(Ctrl, SAD_Chan, RTM, SPixel, stat)
            if (stat /= 0) then
               Spixel%QC = ibset(Spixel%QC, SPixRTM)
!               write(*,*)  'WARNING: Get_RTM', stat
            endif
         end if

         if (stat == 0) then
            call Get_Measurements(Ctrl, SAD_Chan, SPixel, MSI_Data, stat)
            if (stat /= 0) then
               write(*,*)  'WARNING: Get_Measurements', stat
               Spixel%QC = ibset(Spixel%QC, SPixMeas)
            endif
         end if


         ! Get surface parameters and reduce reflectance by solar angle effect.
         if (stat == 0 .and. SPixel%Ind%NSolar /= 0) then
            call Get_Surface(Ctrl, SPixel, MSI_Data, stat)
            if (stat /= 0) then
!               write(*,*)  'WARNING: Get_Surface', stat
               Spixel%QC = ibset(Spixel%QC, SPixSurf)
            else
               do i=1,Ctrl%Ind%NSolar
                   SPixel%Rs(i) = SPixel%Rs(i) &
                      / SPixel%Geom%SEC_o(SPixel%ViewIdx(i))
               end do
            end if
         end if

         if (stat == 0 .and. SPixel%Ind%NSolar == 0) then
            call Get_LSF(Ctrl, SPixel, MSI_Data, stat)
            if (stat .ne. 0 ) write(*,*) 'WARNING: Get_LSF', stat
         endif

         if (stat == 0) then
            call Get_X(Ctrl, SAD_Chan, SPixel, stat)
            if (stat /= 0) then
               write(*,*)  'WARNING: Get_X', stat
               Spixel%QC = ibset(Spixel%QC, SPixFGAP)
            endif
         end if


      end if ! End of "if stat" after cloud fraction check

      if (stat == 0 .and. SPixel%Ind%NSolar /= 0) then
         ! Set the solar constant for the solar channels used in this SPixel.

         do i=SPixel%Ind%SolarFirst,SPixel%Ind%SolarLast
            SPixel%f0(i) = SAD_Chan(i)%Solar%f0
         end do

         ! Calculate transmittances along the slant paths (solar and viewing)
         ! Pick up the "purely solar" channel values from the SW RTM, and the
         ! mixed solar and thermal channels from the LW RTM.

         if (SPixel%Ind%Ny-SPixel%Ind%NThermal > 0) then
            do i=1,SPixel%Ind%Ny-SPixel%Ind%NThermal
               SPixel%RTM%Tsf_o(i) = SPixel%RTM%SW%Tsf(i) &
                    & ** SPixel%Geom%SEC_o(Spixel%ViewIdx(i))

               SPixel%RTM%Tsf_v(i) = SPixel%RTM%SW%Tsf(i) &
                    & ** SPixel%Geom%SEC_v(Spixel%ViewIdx(i))
            end do
         end if


         ! LW Tsf array is of size NThermal, but we only want the mixed channels.
         ! The Tsf_o, v calculations differ for LW, because here the Tac value
         ! used to set Tsf is given at the view angle, rather than the nadir as in
         ! the SW channels RTM.
         if (SPixel%Ind%NThermal > 0) then
            j=1
            do i = SPixel%Ind%ThermalFirst, SPixel%Ind%SolarLast
               SPixel%RTM%Tsf_o(i) = SPixel%RTM%LW%Tsf(j) &
                  ** (SPixel%Geom%SEC_o(Spixel%ViewIdx(i)) / &
                     SPixel%Geom%SEC_v(Spixel%ViewIdx(i)))
               j = j+1
            end do

            SPixel%RTM%Tsf_v(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) = &
               SPixel%RTM%LW%Tsf(1:SPixel%Ind%NMixed)
         end if

         ! Calculate top of atmosphere reflectance for clear conditions
         ! (all arrays are sized NSolar).

         SPixel%RTM%REF_clear = SPixel%Rs * SPixel%RTM%Tsf_o * SPixel%RTM%Tsf_v

         ! Gradient of REF_clear w.r.t. surface albedo

         SPixel%RTM%dREF_clear_dRs = SPixel%RTM%Tsf_o * SPixel%RTM%Tsf_v
      end if ! End of stat check before solar channel actions
   end if ! End of "NMask 0" check

   ! If stat indicates a "super-pixel fatal" condition set the quality control
   ! flag bit to indicate no processing.

   if (stat /= 0) Spixel%QC = ibset(Spixel%QC, SPixNoProc)



   ! If the super-pixel will not be processed, zero the first guess and a priori
   ! state vectors for output into the diag file. Check the QC flag rather than
   ! stat in case future changes mean QC could be set to SPixNoProc earlier on.

   if (btest(Spixel%QC, SpixNoProc)) then
      SPixel%X0 = MissingXn
      Spixel%Xb = MissingXn
   end if

   ! Open breakpoint file if required, and write out reflectances and gradients.

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_Get_SPixel) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
         call Write_Log(Ctrl, 'Get_SPixel: Error opening breakpoint file', status)
      else
         write(bkp_lun,'(/,a)')'Get_SPixel:'
      end if

      write(bkp_lun,'(a,2(i4,1x))')' Location: ',SPixel%Loc%X0,SPixel%Loc%Y0
      write(bkp_lun,'(a,2(f7.1,1x))')' Lat, lon: ',SPixel%Loc%Lat,SPixel%Loc%Lon
      write(bkp_lun,'(a,i2)')' Land flag ',SPixel%Surface%Flags
      do view=1,SPixel%Ind%NViews
         write(bkp_lun,'(a,i3,3(a,f7.1))')' View ', view,' sat zen ',SPixel%Geom%SatZen(view),&
        '  sol zen ',SPixel%Geom%SolZen(view), ' rel azi ',SPixel%Geom%RelAzi(view)
      end do
      write(bkp_lun,'(a,i4)')' Status: ',status
!     write(bkp_lun,'(a,i4)')' QC flag: ',SPixel%QC
      write(bkp_lun,*)' QC flag: ',SPixel%QC

      write(bkp_lun,*)'Bit tests'
      if (btest(Spixel%QC,SPixCloudFl )) write(bkp_lun,*)'SPixCloudFl'
      if (btest(Spixel%QC, SPixLandFl )) write(bkp_lun,*)' SPixLandFl'
      if (btest(Spixel%QC,  SPixSolZen)) write(bkp_lun,*)' SPixSolZen'
      if (btest(Spixel%QC,SPixSatZen )) write(bkp_lun,*)'SPixSatZen'
      if (btest(Spixel%QC,SPixRelAzi )) write(bkp_lun,*)'SPixRelAzi'
      if (btest(Spixel%QC, SPixLat )) write(bkp_lun,*)' SPixLat'
      if (btest(Spixel%QC,SPixLon  )) write(bkp_lun,*)'SPixLon '
      if (btest(Spixel%QC, SPixRef  )) write(bkp_lun,*)' SPixRef '
      if (btest(Spixel%QC,  SPixTemp )) write(bkp_lun,*)' SPixTemp '
      if (btest(Spixel%QC,  SPixAll )) write(bkp_lun,*)' SPixAll '
      if (btest(Spixel%QC,SPixNoCloud )) write(bkp_lun,*)'SPixNoCloud'
      if (btest(Spixel%QC, SPixNoAvge)) write(bkp_lun,*)'SPixNoAvge'
      if (btest(Spixel%QC,SPixGeom  )) write(bkp_lun,*)'SPixGeom '
      if (btest(Spixel%QC,SPixillum  )) write(bkp_lun,*)'SPixillum '
      if (btest(Spixel%QC,  SPixLoc )) write(bkp_lun,*)' SPixLoc '
      if (btest(Spixel%QC, SPixRTM )) write(bkp_lun,*)'SPixRTM '
      if (btest(Spixel%QC,  SPixMeas )) write(bkp_lun,*)' SPixMeas '
      if (btest(Spixel%QC, SPixSurf )) write(bkp_lun,*)'SPixSurf '
      if (btest(Spixel%QC,  SPixFGAP)) write(bkp_lun,*)' SPixFGAP'
      if (btest(Spixel%QC, SpixNoProc )) write(bkp_lun,*)'SpixNoProc '

      do view=1,SPixel%Ind%NViews
         write(bkp_lun,'(a,i2,2(a,f9.4))')' View ',view,', Sec_o: ', &
            SPixel%Geom%SEC_o(view), ' Sec_v: ',SPixel%Geom%SEC_v(view)
      end do
      do i=1, SPixel%Ind%NSolar
         write(bkp_lun,'(2a,2(a,f9.4))') 'Channel: ', SAD_Chan(i)%Desc, &
            ' Tsf_o: ', SPixel%RTM%Tsf_o(i), &
            ' Tsf_v: ', SPixel%RTM%Tsf_v(i)
      end do

      if (SPixel%Ind%Ny-SPixel%Ind%NThermal > 0 .and. SPixel%Ind%NSolar /= 0) then
         write(bkp_lun,'(/a)') 'Purely solar channels Tsf and Rs'
         do i=1, (SPixel%Ind%Ny-SPixel%Ind%NThermal)
            write(bkp_lun,'(2a,2(a,f9.6))') 'Channel: ', SAD_Chan(i)%Desc, &
               ' Tsf:   ', SPixel%RTM%SW%Tsf(i), ' Rs:    ', SPixel%Rs(i)
         end do
      end if

      if (SPixel%Ind%NThermal > 0 .and. SPixel%Ind%NSolar /= 0) then
         write(bkp_lun,'(/a)') 'Mixed Solar/Thermal channels Tsf and Rs'
         do i=SPixel%Ind%ThermalFirst, SPixel%Ind%SolarLast
            write(bkp_lun,'(2a,2(a,f9.6))') 'Channel: ', &
               SAD_Chan(i)%Desc, ' Tsf:   ', &
               SPixel%RTM%LW%Tsf(i-(SPixel%Ind%Ny-SPixel%Ind%NThermal)), &
               ' Rs:    ', SPixel%Rs(i)
         end do
      end if

      write(bkp_lun,'(/)')
      if (SPixel%Ind%NSolar /= 0) then
         do i=SPixel%Ind%SolarFirst, SPixel%Ind%SolarLast
                  write(bkp_lun,'(2a,2(a,f9.4))') 'Channel: ', SAD_Chan(i)%Desc, &
               ' Ref_clear: ', SPixel%RTM%Ref_clear(i), &
               ' dRef_clear_dRs: ', SPixel%RTM%dRef_clear_dRs(i)
         end do
      end if

      write(bkp_lun,'(/)')
      write(bkp_lun,'(a)') 'Ym, view for all channels '
      if (SPixel%Ind%NSolar == 0) then
         StartChan = SPixel%Ind%ThermalFirst
      else
         StartChan = SPixel%Ind%SolarFirst
      end if
      do i=1,Spixel%Ind%Ny
!        write(bkp_lun,'(3a,f9.4,a,i)') 'Channel: ', SAD_Chan(i+StartChan-1)%Desc, &
!           ' : ', SPixel%Ym(i), ' view ', SPixel%ViewIdx(i)
      end do
      write(bkp_lun, '(a,/)') 'Get_SPixel: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine Get_SPixel
