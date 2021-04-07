!-------------------------------------------------------------------------------
! Name: get_spixel.F90
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
!    the ORAC)
! For each case below, if the SPixel cannot be used, set QC flag bit
! SPixNoProc: indicates the error is fatal for the SPixel, but not the ORAC.
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
! status   integer      Out  Error status.
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
! 2001/08/03, AS: Bug fix: ibset arguments were the wrong way round! Updates for
!    image segmentation. Selection of values from the MSI Data structure arrays
!    now need to use a y value that refers to the image segment currently held
!    in memory rather than the whole image area.  X co-ords are unchanged since
!    the segment is the full image width. Renamed structure Data to MSI_Data
!    since Data is a reserved word (hasn't caused any problems so far but it
!    might).
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
! 2014/06/17, CP: modified code so retrieval performed if a single ir channel is
!    missing
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
! 2015/07/27, AP: Replace SPixel%Cloudy with check of SPixel%Type. Remove
!    Get_LSF and SPixel%QC. Replace status checks with go to 99 in the event of
!    failure.
! 2015/08/19, AP: Make reading of RTM terms optional. Add alternative
!    Get_Surface for Swansea model.
! 2015/12/17, GM: Get rid of the secant of the solar zenith angle division of
!    surface reflectance.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Get_SPixel(Ctrl, SAD_Chan, SAD_LUT, MSI_Data, RTM, SPixel, status)

   use Ctrl_m
   use Data_m
   use Int_Routines_m, only : find_in_array
   use ORAC_Constants_m
   use RTM_m
   use SAD_Chan_m
   use SAD_LUT_m

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT(:)
   type(Data_t),     intent(in)    :: MSI_Data
   type(RTM_t),      intent(in)    :: RTM
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i
   integer :: ictrl, ispix, itherm, isolar

   status = 0

   SPixel%Type = MSI_Data%Type(SPixel%Loc%X0, SPixel%Loc%Y0)

   if (.not. any(Ctrl%Types_to_process(1:Ctrl%NTypes_to_process) == &
                 SPixel%Type)) then
      ! Incorrect particle type in SPixel. Don't process.
      status = SPixelType
#ifdef DEBUG
      write(*, *) 'WARNING: Get_SPixel(): Incorrect particle type in  ' // &
                  'pixel starting at:', SPixel%Loc%X0, SPixel%Loc%Y0
#endif
      go to 99 ! Skip further data reading
   end if

   ! Call 'Get_' subroutines. Non-zero stat flags fatal error for superpixel
   call Get_Indexing(Ctrl, SAD_Chan, SPixel, MSI_Data, status)
   if (status /= 0) go to 99 ! Skip further data reading

   call Get_Geometry(Ctrl, SPixel, MSI_Data, status)
   if (status /= 0) go to 99 ! Skip further data reading

   call Get_Location(Ctrl, SPixel, MSI_Data, status)
   if (status /= 0) go to 99 ! Skip further data reading

   call Get_LSF(Ctrl, SPixel, MSI_Data, status)
   if (status /= 0) go to 99 ! Skip further data reading

   call Get_Measurements(Ctrl, SAD_Chan, SPixel, MSI_Data, status)
   if (status /= 0) go to 99 ! Skip further data reading

   if (SPixel%Ind%NSolar > 0) then
      if (Ctrl%Approach == AppAerSw) then
         call Get_Surface_Swansea(Ctrl, SPixel, SAD_LUT(1), MSI_Data)
      else
         call Get_Surface(Ctrl, SAD_Chan, SPixel, MSI_Data, status)
         if (status /= 0) go to 99 ! Skip further data reading
      end if

      ! Set the solar constant for the solar channels used in this SPixel.
      deallocate(SPixel%f0)
      allocate(SPixel%f0(SPixel%Ind%NSolar))
      do i = 1, SPixel%Ind%NSolar
         SPixel%f0(i) = &
              SAD_Chan(SPixel%spixel_y_solar_to_ctrl_y_index(i))%Solar%f0
      end do
   end if

   if (Ctrl%RTMIntSelm == RTMIntMethNone) then
      ! Using an infinite extent cloud/aerosol layer
      if (SPixel%Ind%NSolar > 0) then
         deallocate(SPixel%RTM%Ref_clear)
         allocate(SPixel%RTM%Ref_clear     (SPixel%Ind%NSolar))
         deallocate(SPixel%RTM%dRef_clear_dRs)
         allocate(SPixel%RTM%dRef_clear_dRs(SPixel%Ind%NSolar))
         SPixel%RTM%Ref_clear      = 0.
         SPixel%RTM%dRef_clear_dRs = 0.
      end if
   else
      ! Using infinitesimally thin cloud/aerosol layer
      call Get_RTM(Ctrl, SAD_Chan, RTM, SPixel, status)
      if (status /= 0) go to 99 ! Skip further data reading

      ! Reduce reflectance by solar angle effect.
      if (SPixel%Ind%NSolar > 0) then
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

         do i = 1, SPixel%Ind%NSolar
            ictrl  = SPixel%spixel_y_solar_to_ctrl_y_index(i)
            ispix  = SPixel%Ind%YSolar(i)
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
         SPixel%RTM%Ref_clear = SPixel%Surface%Rs * SPixel%RTM%Tsf_o * &
                                SPixel%RTM%Tsf_v

         ! Gradient of Ref_clear w.r.t. surface albedo
         SPixel%RTM%dRef_clear_dRs = SPixel%RTM%Tsf_o * SPixel%RTM%Tsf_v
      end if ! End of NSolar > 0
   end if ! End of RTMIntMeth /= RTMIntMethNone

   call Get_X(Ctrl, SPixel, status)
!  if (status /= 0) go to 99 ! Skip further data reading

   ! If stat indicates a "super-pixel fatal" condition set the quality
   ! control flag bit to indicate no processing.
99 if (status /= 0) then
#ifdef DEBUG
     write(*,*) 'WARNING: Get_SPixel() error status', status
#endif
   end if

end subroutine Get_SPixel
