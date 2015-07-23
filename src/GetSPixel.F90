!-------------------------------------------------------------------------------
! Name: GetSPixel.F90
!
! Purpose:
! Controls the extraction of the current super-pixel data from the
! 'whole-image' data arrays.
! Also calculates the surface to TOA slant path transmittances.
!
! Description and Algorithm details:
! Check data in each of the Data structure arrays
! Flag pixels containing bad data by setting zeros in the super pixel mask.
! Flag reasons by setting bits in the quality control value SPixel%QC
!    (non-zero QC is not fatal at this stage, either to the super-pixel or to
!    the ECP)
! For each case below, if the SPixel cannot be used, set QC flag bit
! SPixNoProc: indicates the error is fatal for the SPixel, but not the ECP.
! Call Get_Location
! Call Get_Cloudflags
! If averaging method 'central'
!    Check that central pixel does not contain bad data (abort current super
!    pixel if it does),
! If averaging method 'all'
!    Calculate number of good pixels in super pixel (if below threshold
!    Ctrl%SPix%Threshold then abort current super pixel,
! If averaging method 'cloudy'
!    Calculate number of good pixels in super pixel (if there are no good
!    cloudy pixels then abort current super pixel).
! Call Get_Illum
! Call Get_Geometry
! Call Get_RTM
! Call Get_Measurements
! Call Get_Surface
! Call Get_X to set a priori and First Guess
! if there are solar channels in use for the SPixel:
!    - Calculate the surface to TOA slant path transmittances for use in
!      forward model (airmass factors  - SEC_o/v - taken from Get_Geometry).
!    - Set solar constant values for the SPixel (same for all SPixels with
!      solar channels in use? Move elsewhere?)
!
! Note on error handling: it is assumed that any error identified at this
! stage is fatal only for the current super-pixel. Hence status can be used
! to flag a problem in a super-pixel. The calling routine can set status=0
! after checking.
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       In   Control structure
! SAD_Chan struct       In   SAD channel structure
! MSI_Data struct       In   Data structure. Contains the multi-spectral
!                            image measurements, location values, geometry
!                            etc for the current image segment, from which
!                            the current SPixel values will be extracted.
! RTM      alloc struct In   RTM structure
! SPixel   alloc struct Both Super-pixel structure
! status   integer      Out  Error status. Note most problems identified in
!                            this routine do not lead to non-zero status
!                            values as they affect the super-pixel data only.
!                            Status is not currently passed to subordinate
!                            routines (local stat is used instead). It is
!                            assumed that no subordinate can identify a
!                            serious error condition.
!
! History:
! 2000/11/29, KS: Original version
! 2000/12/19, KS: Replaced Data_... arrays with Data structure.
! 2001/01/17, KS: Added error checking on land/sea flags.
! 2001/01/24, KS: Moved allocations to ECP main on integration. Moved
!    calculation of central pixel coords to ECP.
! 2001/01/26, KS: Added calculations of solar and viewing slant path
!    transmittances.
! 2001/03/06, AS: Change to setting of Tsf_o,v values in SPixel%RTM. Tsf_o,v 
!    now appear in the overall RTM struct rather than the RTM%LW and SW structs.
!    Added calculation of the Ref_clear values.
! 2001/03/07, AS: Change in LW Tsf_o, v calculations. Additional breakpoint
!    output. GetRTM needs SAD_Chan as an argument. Changed order of arguments
!    (inputs first).
! 2001/03/15, AS: Added code to set the central pixel absolute coordinates (if
!    required) and the top right-hand corner coordinates. Amended solar zenith
!    angle check. Previously disallowed data where SolZen  > 90 degrees. Do not
!    check. SolZen may be treated differently in different data sets when > 90.
!    Using pre-defined constant names for super-pixel averaging methods. Rs
!    divided by Sec_o to take account of solar angle as soon as Rs is set by 
!    Get_Surface/Get_Rs. REF_Clear and dREF_Clear_dRs calculations updated.
! 2001/04/04, AS: Removed brackets specifying array section from whole array
!    assignments where the whole array is used, as experience elsewhere seems to
!    show that "array = " works faster than "array(:) = ".
! 2001/05/17, AS: New argument SAD_Chan required by Get_Measurements.
! 2001/06/05, AS: Added call to Get_X to set a priori and first guess, plus new 
!    argument SAD_CloudClass required by Get_X.
! 2001/06/15, AS: Changed error message string assignments/writes. Long message
!    strings were wrapped over two lines with only one set of quotes around the
!    whole string (including the line continuation marker). Original code works
!    on DEC but not Linux.
! 2001/07/10, AS: Attempt to rationalise error handling and SPixel checking.
!    Status was used to flag conditions that were fatal for the SPixel.
!    SPixel%QC was used to flag out of range values etc but these were not
!    considered fatal. Changing to use SPixel%QC for all flagging. As a result
!    some QC settings now indicate conditions are fatal for the SPixel. This
!    will  simplify the main ECP loop where it checks whether a particular
!    SPixel should be processed and leaves status for flagging of "real" errors.
!    (Note most subordinates flag data problems via status, but no  subordinate
!    currently detects any error that is fatal for the program). Replaced
!    Kevin's BITS routine for setting bit flags with the intrinsic ibset.
! 2001/08/03, AS:  Bug fix: ibset arguments were the wrong way round! Updates for
!    image segmentation. Selection of values from the MSI Data structure arrays
!    now need to use a y value that refers to the image segment currently held in
!    memory rather than the whole image area.  X co-ords are unchanged since the
!    segment is the full image width. Renamed structure Data to MSI_Data since
!    Data is a reserved word (hasn't caused any problems so far but it might).
!    Moved setting of SPixel Xc, Yc. Required before GetCloudFlags call if
!    averaging method is central.
! 2001/09/18, AS: Removed Write_Log call when there are no cloudy pixels in the
!    super-pixel. Could lead to lots of unnecessary log output.
!    **************** ECV work starts here *************************************
! 2011/02/21, AS: Re-applying changes from late 2001/2002.
! 2001/11/21, AS: Added zeroing of first-guess and a priori state vectors if
!    SPixel QC flag indicates no processing. Otherwise these vectors retain the
!    value from the previous pixel and are output into the diag file with the
!    old values.
! 2001/11/28, AS: Bug fix to range checking of MSI reflectances and brightness
!    temps. Moved the check on "stat" and call to WriteLog inside the channel
!    loop. Previously, a "bad" stat value in, say, channel 1, could be
!    overwritten  by a "good" value from a higher channel. Replaced constant
!    values in range checks with named constants. Updated checking of cloud
!    flags and errors in pixels required for the  selected averaging method.
!    Removed error logging for these situations: errors are already logged
!    during limit checking.
! 2001/12/06, AS: Bug fix to check on no. of "good" pixels vs.
!    Ctrl%SPix%Threshold. Previously, the check was:
!       if (real(SPixel%NAverage/Ctrl%SPix%NPixel) < Ctrl%SPix%Threshold)
!    This takes the real value of the result of an integer divide and compares
!    to the real Threshold fraction. The integer divide returns 1 if NAverage =
!    NPixel and 0 if NAverage < NPixel. Both values should be converted to real
!    before division.
! 2002/08/14, CP: Fractional error is assigned according to the neighbouring
!    pixels when resolution is 1.
! 2002/12/23, AS: Move the Get_Location call so that Lat-Lon information is
!    available for all pixels in the image, rather than just the cloudy ones.
! 2011/02/23, AS: MSI_Data%CloudFlags now an array of floats, to match current
!    ORAC data.
! 2011/03/16, AS: Added some extra breakpoint outputs: lat, lon etc. Added MSI
!    temp/reflectance value to message for out of range values (done in Feb 2011)
! 2011/03/22, AS: Removal of phase change. Only 1 cloud class required for each
!    retrieval run. SADCloudClass array dimensions changed.
! 2011/03/24, AS: Removal of super-pixelling, i.e. no averaging of flags etc
!    needed. Any super-pixelling required will now be done in pre-processing.
!    Resolution for the retrieval will be fixed at 1 pixel. Remove calculation
!    of SPixel%Loc%Xc and Xn, simplify calls to subordinate functions - no need
!    for range of pixel locations. Removed calculation of Fracnext (used in
!    XMDAD to set error).
! 2011/04/07, AS: Removal of selection methods SAD and SDAD. GetSurface argument
!    list updated - SAD_Chan no longer needed.
! 2011/04/20, AS: Extension to handle multiple instrument views. The viewing
!    geometry becomes a set of arrays, e.g. 1 value of sat. zen angle per view.
!    Extend checks on MSI_Data geometry values and all references to the
!    SPixel%Geom sub-structure.
! 2011/05/20, AS: Multiple instrument views (2). Modified breakpoints output to
!    check for 0 solar channels present.
! 2011/06/08, AS: Reduced/removed logging to improve performance. Tests show
!    that writing to the ASCII log file can extend total time to process an
!    orbit from a few minutes to several hours. Enclosed in ifdef DEBUG than
!    removed, for easier re-introduction. Removed reference to "super-pixel"
!    from log messages.
! 2011/08/05, CP: Remove ref to cloudclass
! 2011/09/29, CP: Updated log output to be more informative and give the channel
!    number of the missing data
! 2012/02/16, CP: Updated file to fix bug that produced errors when night views
!    were processed.
! 2013/07/24, AP: Fixed BKP code
! 2014/04/30, GM: Cleaned up the code.
! 2014/06/17, CP: modified code so retrieval performed if a single ir channel
!    is missing
! 2014/07/25, AP: Tidying code with check_value subroutine. Fixing bug that
!    meant SPixal wasn't necessarily set if pixel failed.
! 2014/08/01, GM: Checks for missing data are already performed when the
!    illumination condition is chosen and the result of the check is defined by
!    the illumination condition so the checks in this subroutine have been
!    disabled except to set QC.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2015/01/13, AP: Switching to Ctrl%Ind%Ch_Is rather than any(). Removing 
!    First:Last channel indexing.
! 2015/01/21, AP: Moved allocated of SPixel%RTM%... here.
! 2015/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/02/04, GM: Changes related to the new missing channel, illumination, and 
!    channel selection code.
! 2015/06/02, AP: Remove Ctrl argument from check_value.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_SPixel(Ctrl, SAD_Chan, MSI_Data, RTM, SPixel, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use Int_Routines_def, only : find_in_array
   use RTM_def
   use SAD_Chan_def

   implicit none

   ! Define arguments

   type(CTRL_t),        intent(in)    :: Ctrl
   type(SAD_Chan_t),    intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(Data_t),        intent(in)    :: MSI_Data
   type(RTM_t),         intent(in)    :: RTM
   type(SPixel_t),      intent(inout) :: SPixel
   integer,             intent(out)   :: status

   ! Define local variables

   integer           :: i
   integer           :: ictrl, ispix, itherm, isolar
   real              :: minsolzen
   integer           :: stat ! Local status value
   real, allocatable :: thermal(:)
#ifdef BKP
   integer :: bkp_lun   ! Unit number for breakpoint file
   integer :: ios       ! I/O status for breakpoint file
   integer :: StartChan ! First valid channel for pixel, used in breakpoints.
#endif

   ! Set status to zero
   stat   = 0
   status = 0

   ! Initialise Mask
   SPixel%Mask = 1

   ! Initialise quality control flag
   SPixel%QC = 0

   ! Check for pixel values out of range. QC is set as information to accompany
   ! a retrieval. The SPixel Mask is set to flag problems in particular pixels.

   ! Check Cloud flags (0 or 1)
   call check_value(MSI_Data%CloudFlags(SPixel%Loc%X0, SPixel%Loc%Y0), &
        CloudMax, CloudMin, SPixel, 'cloud flag', SPixCloudFl)

   ! Land/Sea flags (0 or 1)
   call check_value(MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%Y0), &
        FlagMax, FlagMin, SPixel, 'land/sea flag', SPixLandFl)

   !  Make this work if pixel is in daylight
   !  Geometry - Solar zenith (between 0o and 90o)
   !call check_value(MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%Y0), &
   !     90.0, 0.0, SPixel, 'solar zenith angle', SPixSolZen)

   ! Geometry - Satellite zenith (between 0o and 90o)
   call check_value(MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%Y0, :), &
        SatZenMax, SatZenMin, SPixel, 'satellite zenith angl', SPixSatZen)

   ! Geometry - Azimuth (between 0o and 180o)
   call check_value(MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%Y0, :), &
        RelAziMax, RelAziMin, SPixel, 'azimuth angle', SPixRelAzi)

   ! Location - Latitude (between -90o and 90o)
   call check_value(MSI_Data%Location%Lat(SPixel%Loc%X0, SPixel%Loc%Y0), &
        LatMax, LatMin, SPixel, 'location latitude', SPixLat)

   ! Location - Longitude (between -180o and 180o)
   call check_value(MSI_Data%Location%Lon(SPixel%Loc%X0, SPixel%Loc%Y0), &
        LonMax, LonMin, SPixel, 'location longitude', SPixLon)

   ! These checks for missing data are already performed when the illumination
   ! condition is chosen. The result of the check is defined by the illumination
   ! condition allowing finer control. So the zeroing of SPixel%Mask has been
   ! disabled (located in check_value.inc).  Now, an invalid condition (which
   ! includes cases with too much missing data) will result in a non-zero status
   ! from Get_Illum() below. For now these loops are still here to set SPixel%QC.

   ! MSI - Reflectances (between 0 and 1)
   minsolzen=minval(MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%Y0, :))
   if (minsolzen < Ctrl%MaxSolzen) then
      do i = 1,Ctrl%Ind%NSolar
         if (.not. btest(Ctrl%Ind%Ch_Is(Ctrl%Ind%YSolar(i)), ThermalBit)) &
            call check_value(MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, &
                             Ctrl%Ind%YSolar(i)), RefMax, RefMin, SPixel, &
                             'MSI reflectance', SPixRef)
      end do
   end if

   ! MSI - Temperatures (between 150.0K and 330.0K)
   allocate(thermal(Ctrl%Ind%NThermal))
   do i = 1,Ctrl%Ind%NThermal
      thermal(i) = MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, &
                                Ctrl%Ind%YThermal(i))
   end do
   call check_value(thermal, BTMax, BTMin, SPixel, 'MSI temperature', SPixTemp)
   deallocate(thermal)

   ! End of range checking. From here on any non-zero stat value is fatal for
   ! the super-pixel. Set the QC flag bits both for the individual error
   ! condition and to indicate no processing for the SPixel.

   ! Calculate NMask: the tests above have been setting 0's in the super-pixel
   ! mask where the data for a given pixel fails each check.

   SPixel%NMask = SPixel%Mask
   SPixel%Loc%Lat = MSI_Data%Location%Lat(SPixel%Loc%X0, SPixel%Loc%Y0)
   SPixel%Loc%Lon = MSI_Data%Location%Lon(SPixel%Loc%X0, SPixel%Loc%Y0)

   ! If all Mask flags are 0 there are no good pixels in the current SPixel, do
   ! not process and set QC flag.

   if (SPixel%NMask == 0) then
      SPixel%QC = ibset(SPixel%QC, SPixAll)
      stat = SPixelInvalid ! pixel is invalid
#ifdef DEBUG
      write(*, *) 'WARNING: Get_SPixel(): NMask zero in pixel at: ', &
                  SPixel%Loc%X0, SPixel%Loc%Y0
#endif

   else
      ! Get cloud flags before checking for cloudy method

      ! Removal of Super-pixel averaging: replace call to GetCloudFlags with a
      ! simple assignment. "Flags" is now 1 single flag.

      SPixel%Cloud%Flags = MSI_Data%CloudFlags(SPixel%Loc%X0, SPixel%Loc%Y0)
      SPixel%Cloud%Fraction = SPixel%Cloud%Flags


      if (SPixel%Cloud%Fraction == 0) then
         ! No cloud in SPixel. Don't process.

         SPixel%QC = ibset(SPixel%QC, SPixNoCloud)
         stat = SPixelCloudFrac
#ifdef DEBUG
         write(*, *) 'WARNING: Get_SPixel(): Zero cloud fraction in super ' // &
                     'pixel starting at:', SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      else
         ! Call 'Get_' subroutines. Use of stat here assumes that no subordinate
         ! routine returns a non-zero status value unless it is to flag a
         ! super-pixel data problem.
         if (stat == 0) then
            call Get_Illum(Ctrl, SPixel, MSI_Data, stat)
            if (stat /= 0) then
!              write(*,*) 'WARNING: Get_Illum()', stat
               SPixel%QC = ibset(SPixel%QC, SPixIllum)
            end if
         end if

         if (stat == 0) then
            call Get_Indexing(Ctrl, SAD_Chan, SPixel, MSI_Data, stat)
            if (stat /= 0) then
!              write(*,*) 'WARNING: Get_Indexing()', stat
               SPixel%QC = ibset(SPixel%QC, SPixIllum)
            end if
         end if

         if (stat == 0) then
            call Get_Geometry(Ctrl, SPixel, MSI_Data, stat)
            if (stat /= 0) then
!              write(*,*)  'WARNING: Get_Geometry()', stat
               SPixel%QC = ibset(SPixel%QC, SPixGeom)
            end if
         end if

         if (stat == 0 .and. SPixel%Ind%NSolar == 0) then
            call Get_LSF(Ctrl, SPixel, MSI_Data, stat)
            if (stat /= 0) then
!              write(*,*) 'WARNING: Get_LSF()', stat
               SPixel%QC = ibset(SPixel%QC, SPixLSF)
            end if
         end if

         if (stat == 0) then
            call Get_RTM(Ctrl, SAD_Chan, RTM, SPixel, stat)
            if (stat /= 0) then
!              write(*,*)  'WARNING: Get_RTM()', stat
               SPixel%QC = ibset(SPixel%QC, SPixRTM)
            end if
         end if

         if (stat == 0) then
            call Get_Measurements(Ctrl, SAD_Chan, SPixel, MSI_Data, stat)
            if (stat /= 0) then
!              write(*,*)  'WARNING: Get_Measurements()', stat
               SPixel%QC = ibset(SPixel%QC, SPixMeas)
            end if
         end if

         ! Get surface parameters and reduce reflectance by solar angle effect.
         if (stat == 0 .and. SPixel%Ind%NSolar /= 0) then
            call Get_Surface(Ctrl, SPixel, MSI_Data, stat)
            if (stat /= 0) then
!              write(*,*)  'WARNING: Get_Surface()', stat
               SPixel%QC = ibset(SPixel%QC, SPixSurf)
            else
               do i=1,SPixel%Ind%NSolar
                  SPixel%Rs(i) = SPixel%Rs(i) &
                       / SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))

                   if (Ctrl%RS%use_full_brdf) then
                      SPixel%Rs2(i,:) = SPixel%Rs2(i,:) &
                        / SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
                   end if
               end do
            end if
         end if

         if (stat == 0) then
            call Get_X(Ctrl, SAD_Chan, SPixel, stat)
            if (stat /= 0) then
!              write(*,*)  'WARNING: Get_X()', stat
               SPixel%QC = ibset(SPixel%QC, SPixFGAP)
            end if
         end if

      end if ! End of "if stat" after cloud fraction check

      if (stat == 0 .and. SPixel%Ind%NSolar > 0) then
         ! Set the solar constant for the solar channels used in this SPixel.
         deallocate(SPixel%f0)
         allocate(SPixel%f0(SPixel%Ind%NSolar))
         do i=1,SPixel%Ind%NSolar
            SPixel%f0(i) = &
                 SAD_Chan(SPixel%spixel_y_solar_to_ctrl_y_index(i))%Solar%f0
         end do

         ! Calculate transmittances along the slant paths (solar and viewing)
         ! Pick up the "purely solar" channel values from the SW RTM, and the
         ! mixed channels from the LW RTM.
         deallocate(SPixel%RTM%Tsf_o)
         allocate(SPixel%RTM%Tsf_o         (SPixel%Ind%NSolar))
         deallocate(SPixel%RTM%Tsf_v)
         allocate(SPixel%RTM%Tsf_v         (SPixel%Ind%NSolar))
         deallocate(SPixel%RTM%Ref_clear)
         allocate(SPixel%RTM%Ref_clear     (SPixel%Ind%NSolar))
         deallocate(SPixel%RTM%dRef_clear_dRs)
         allocate(SPixel%RTM%dRef_clear_dRs(SPixel%Ind%NSolar))

         do i=1, SPixel%Ind%NSolar
            ictrl = SPixel%spixel_y_solar_to_ctrl_y_index(i)
            ispix = SPixel%Ind%YSolar(i)
            isolar = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

            if (btest(Ctrl%Ind%Ch_Is(ictrl), ThermalBit)) then
               itherm = find_in_array(Ctrl%Ind%YThermal, ictrl)

               ! The Tsf_o, v calculations differ for mixed channels
               ! because the Tac value used to set Tsf is given at the view
               ! angle, rather than the nadir as in the SW channels RTM.
               SPixel%RTM%Tsf_o(i) = SPixel%RTM%LW%Tsf(itherm) &
                    ** (SPixel%Geom%SEC_o(SPixel%ViewIdx(ispix)) / &
                        SPixel%Geom%SEC_v(SPixel%ViewIdx(ispix)))

               SPixel%RTM%Tsf_v(i) = SPixel%RTM%LW%Tsf(itherm)
            else
               ! Purely solar channel
               SPixel%RTM%Tsf_o(i) = SPixel%RTM%SW%Tsf(isolar) &
                    ** SPixel%Geom%SEC_o(SPixel%ViewIdx(ispix))

               SPixel%RTM%Tsf_v(i) = SPixel%RTM%SW%Tsf(isolar) &
                    ** SPixel%Geom%SEC_v(SPixel%ViewIdx(ispix))
            end if
         end do

         ! Calculate top of atmosphere reflectance for clear conditions
         ! (all arrays are sized NSolar).

         SPixel%RTM%REF_clear = SPixel%Rs * SPixel%RTM%Tsf_o * SPixel%RTM%Tsf_v

         ! Gradient of REF_clear w.r.t. surface albedo

         SPixel%RTM%dREF_clear_dRs = SPixel%RTM%Tsf_o * SPixel%RTM%Tsf_v
      end if ! End of stat check before solar channel actions
   end if ! End of "NMask 0" check

   ! If stat indicates a "super-pixel fatal" condition set the quality
   ! control flag bit to indicate no processing.

   if (stat /= 0) SPixel%QC = ibset(SPixel%QC, SPixNoProc)


   ! If the super-pixel will not be processed, zero the first guess and a priori
   ! state vectors for output into the diag file. Check the QC flag rather than
   ! stat in case future changes mean QC could be set to SPixNoProc earlier on.

   if (btest(SPixel%QC, SPixNoProc)) then
      SPixel%X0 = MissingXn
      SPixel%Xb = MissingXn
   end if

   ! Open breakpoint file if required, and write out reflectances and gradients.
   ! Definitely not functional.
#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_Get_SPixel) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: Get_SPixel(): Error opening breakpoint file'
         stop BkpFileOpenErr
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
      if (btest(SPixel%QC, SPixCloudFl)) write(bkp_lun,*)'SPixCloudFl'
      if (btest(SPixel%QC, SPixLandFl)) write(bkp_lun,*)'SPixLandFl'
      if (btest(SPixel%QC, SPixSolZen)) write(bkp_lun,*)'SPixSolZen'
      if (btest(SPixel%QC, SPixSatZen)) write(bkp_lun,*)'SPixSatZen'
      if (btest(SPixel%QC, SPixRelAzi)) write(bkp_lun,*)'SPixRelAzi'
      if (btest(SPixel%QC, SPixLat)) write(bkp_lun,*)'SPixLat'
      if (btest(SPixel%QC, SPixLon)) write(bkp_lun,*)'SPixLon '
      if (btest(SPixel%QC, SPixRef)) write(bkp_lun,*)'SPixRef '
      if (btest(SPixel%QC, SPixTemp)) write(bkp_lun,*)'SPixTemp '
      if (btest(SPixel%QC, SPixAll)) write(bkp_lun,*)'SPixAll '
      if (btest(SPixel%QC, SPixNoCloud)) write(bkp_lun,*)'SPixNoCloud'
      if (btest(SPixel%QC, SPixNoAvge)) write(bkp_lun,*)'SPixNoAvge'
      if (btest(SPixel%QC, SPixIllum)) write(bkp_lun,*)'SPixIllum '
      if (btest(SPixel%QC, SPixIndexing)) write(bkp_lun,*)'SPixIndexing '
      if (btest(SPixel%QC, SPixGeom)) write(bkp_lun,*)'SPixGeom '
      if (btest(SPixel%QC, SPixLoc)) write(bkp_lun,*)'SPixLoc '
      if (btest(SPixel%QC, SPixRTM)) write(bkp_lun,*)'SPixRTM '
      if (btest(SPixel%QC, SPixMeas)) write(bkp_lun,*)'SPixMeas '
      if (btest(SPixel%QC, SPixSurf)) write(bkp_lun,*)'SPixSurf '
      if (btest(SPixel%QC, SPixFGAP)) write(bkp_lun,*)'SPixFGAP'
      if (btest(SPixel%QC, SPixNoProc)) write(bkp_lun,*)'SPixNoProc '

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
      do i=1,SPixel%Ind%Ny
!        write(bkp_lun,'(3a,f9.4,a,i)') 'Channel: ', SAD_Chan(i+StartChan-1)%Desc, &
!           ' : ', SPixel%Ym(i), ' view ', SPixel%ViewIdx(i)
      end do
      write(bkp_lun, '(a,/)') 'Get_SPixel: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine Get_SPixel
