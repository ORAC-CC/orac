!-------------------------------------------------------------------------------
! Name:
!    FM
!
! Description:
!    Main forward model routine.
!    Calls subroutines Interpol_Thermal and Interpol_Solar to interpolate the
!    RTM quantities in SPixel%RTM onto the cloud pressure level. The thermal and
!    shortwave forward model calculations take place in subroutines FM_Thermal
!    and FM_Solar. The latter is only called during daytime conditions. At
!    twilight and nighttime only FM_Thermal is run. At twilight only the pure
!    thermal channels are used. At night all thermal channels are used. Daytime
!    retrievals can use all available channels. In this last case pure thermal
!    and pure solar channels are separated from the mixed channels during the
!    calculation of the measurement vector Y. For the mixed channels, Y is
!    calculated by summing contributions from reflectances (converted to
!    radiances using solar constants f0) and radiances. R2T is used to convert
!    the resulting total radiances to brightness temperatures. The gradients
!    dY_dX are treated in a similar way, using dT_dR from the previous call to
!    R2T. The results from the FMs are then combined into one measurement vector
!    and array of gradients.
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    Ctrl     struct       In          Control structure
!    SPixel   alloc struct In          Super pixel structure
!    SAD_Chan struct arr   In          Array of SAD_Chan structures
!    SAD_LUT  struct arr   In          Array of SAD_LUT structures
!    RTM_Pc   struct       Both        Array to hold RTM data interpolated to
!                                      current Pc value. Passed in because it
!                                      contains allocated arrays. Populated and
!                                      used locally
!    X        real arr     In          State vector
!    Y        real arr     Out         Calculated measurement vector
!    dY_dX    real arr     Out         Gradient in Y wrt state parameters and Rs
!    status   int          Out         Error status
!
! Algorithm:
!    Update state vector X.
!    Interpolate LW RTM data onto cloud pressure level. (all LW channels,
!       including those with thermal and solar components)
!    Set zeroth point grid info. For SAD_LUT CRP interpolation (call Set_GZero).
!    Run thermal forward model (all channels with thermal component).
!    If daytime:
!       Interpolate SW RTM data onto cloud pressure level ("purely" solar
!          channels only, no mixed channels).
!       Run the shortwave forward model (all channels with solar component).
!       Combine the thermal and shortwave calculated radiances and reflectance
!       vectors and write to measurement vector Y (similarly for gradients
!       dY_dX). (Mixed channels are summed in BT via a call to R2T).
!    If twilight:
!       Only write radiances from the purely thermal channels to Y (similarly
!       for dY_dX).
!    If nighttime:
!       Write all thermal channels (including mixed channels) to Y and dY_dX.
!
! Local variables:
!    Name Type Description
!
! History:
!     2nd Feb, 2001, Kevin M. Smith : original version (draft)
!     2nd Mar 2001, Andy Smith:
!       Updates to draft.
!     7th Mar 2001, Andy Smith:
!       New arrays in RTM_Pc for overall dTac_dPc and dTbc_dPc.
!       Adding breakpoint outputs.
!     8th Mar 2001, Andy Smith:
!       dB_dTs no longer required as an argument to FM_Thermal.
!    15th Mar 2001, Andy Smith:
!       Added ThF and ThL indices for RTM_Pc%LW arrays. Required because these
!       arrays are allocated to match the no. of thermal channels requested, but
!       in twilight not all of the requested thermal channels may be used.
!       Moved InterpolSolar call into the "daytime" section. Not required in
!       twilight or nighttime conditions.
!       Breakpoint SAD_Chan%Desc indexing changed: failed for night-time
!       conditions (needs checking)
!       Using SPixel%Ind%ThermalLast as the end of the channel range for
!       RTM_Pc%Tac and Tbc, instead of Ctrl%Ind%Ny.
!       Changed declaration of SAD_Chan to size Ctrl%Ind%Ny, not SPixel%Ind%Ny.
!       (Allows use of SPixel%Ind%ThermalFirst etc to select out channel ranges).
!       Changed declaration of CRP, dCRP to size Ctrl%Ind%Ny, not SPixel%Ind%Ny.
!    11th Apr 2001, Andy Smith:
!       f0 argument no longer required. Now part of SPixel.
!     4th May 2011, Andy Smith:
!       Extension to multiple instrument views. Values depending on viewing
!       geometry are now arrays (no of views).
!       Added SetGZero argument for no of channels, needed for array sizing.
!       In test for daytime/night conditions, add array index to Solzen. Assume
!       that the same illumination applies to all instrument views in a given
!       pixel, so we only need to test one element of the array.
!    19th May 2011, Andy Smith:
!       Multiple instrument views, part 2.
!       In twilight and night conditions the last element of dY_dX for each
!       channel can be NaN or some large value. Try initializing. It is not
!       clear why this has arisen now, but uninitialized values can be dangerous
!       anyway.
!    5th Sep 2011, Chris Arnold:
!       Now calls either linear *or* cubic-spline based RTM interpolation
!       schemes depending on RTMIntflag
!   13th Dec 2011, C Poulsen: Deallocated GZero array at end of routine
!   24th Jan 2012, C Poulsen: Changed array input into FM_Thermal and FM_Solar
!      to avoid no contiguous arrays.
!   15th Jun 2012, C Poulsen: Changed the way day was defined to use illum value
!   20120814, MJ: Removes bug in GZero allocation that made ORAC crash.
!   20120817, MJ: Fixed bug uninitialized variables which cause NaN crash.
!   20121024, CP: Notice a lot of this routine was modified without commenting
!      here!
!   20121024, CP: Removed hardwiring of variable indices as this caused a
!      segmentation fault as there were in fact 3 solar channels
!   20120920, CP: Assigned Y to explicit sizeY(1:SPixel%Ind%Ny) =
!      BT(1:SPixel%Ind%Ny)
!   2013XXXX, MJ: Makes some changes to merge code versions
!   20131125, MJ: Dynamically sets upper limit for CTP to highest pressure in
!      profile to avoid extrapolation problems.
!   20131206, MJ: Add deallocation statements to fix memory leaks.
!   20140115, GM: Ctrl%Invpar%XULim(3)=SPixel%RTM%LW%p(SPixel%RTM%LW%Np), from
!      20131125 should be SPixel%XULim(3)=SPixel%RTM%LW%p(SPixel%RTM%LW%Np) but
!      it would also be clearer to move it just out of FM() to just after
!      Set_Limits() is called which is what was done here.
!   20140117, MJ: Comments out usage of  some temp_* variables as they appear to
!      be unnecessary.
!   20140119, GM: Cleaned up code.
!   20140522, GM: Use allocate and deallocate subroutines for GZero.
!   20140528, GM: The sharing of CRP results for mixed channels from FMThermal
!      with FMSolar was causing problems that were hard to debug and gained
!      little in performance.  Now the Solar and Thermal forward model calls are
!      independent so that contents of CRP and d_CRP do not need to be passed
!      from the thermal call to the solar call.
!   20140715, CP: Changed illumination logic.
!   20141201, CP: added in cloud albedo
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine FM(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, X, Y, dY_dX, cloud_albedo,status)

   use Ctrl_def
   use ECP_Constants
   use GZero_def
   use Interpol_Routines_def
   use RTM_Pc_def
   use SAD_Chan_def
   use SAD_LUT_def
   use SPixel_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(in)    :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc
   real,             intent(in)    :: X(MaxStateVar)
   real,             intent(out)   :: Y(SPixel%Ind%Ny)
   real,              intent(out)   :: cloud_albedo(SPixel%Ind%NSolar)
   real,             intent(out)   :: dY_dX(SPixel%Ind%Ny,(MaxStateVar+1))
   integer,          intent(out)   :: status

   ! Declare local variables

   integer       :: i
   integer       :: ThF, ThL ! First, last thermal channel indices
   type(GZero_t) :: GZero
   real          :: CRP(SPixel%Ind%Ny, MaxCRProps)
!   real          :: d_cloud_albedo(SPixel%Ind%NySolar, 2))	
   real          :: d_CRP(SPixel%Ind%Ny, MaxCRProps, 2)
   real          :: BT(SPixel%Ind%NThermal)
   real          :: d_BT(SPixel%Ind%NThermal, MaxStateVar)
   real          :: Rad(SPixel%Ind%NThermal)
   real          :: d_Rad(SPixel%Ind%NThermal, MaxStateVar)
   real          :: Ref(SPixel%Ind%NSolar)
   real          :: d_Ref(SPixel%Ind%NSolar, MaxStateVar+1)
   real          :: Y_R(SPixel%Ind%NMixed)
   real          :: dT_dR(SPixel%Ind%NMixed)
#ifdef BKP
   integer       :: j
   integer       :: bkp_lun ! Unit number for breakpoint file
   integer       :: ios     ! I/O status for breakpoint file
#endif

   Y     = 0.0
   dy_dX = 0.0

   ! Call routine to interpolate RTM data to the cloud pressure level.
   ! Interpol_Thermal returns transmittances in the LW part of RTM_Pc.
   if (Ctrl%RTMIntflag .eq. RTMIntMethLinear) then
      call Interpol_Thermal(Ctrl, SPixel, X(iPc), &
              SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast), &
              RTM_Pc, status)
   else if (Ctrl%RTMIntflag .eq. RTMIntMethSpline) then
      call Interpol_Thermal_spline(Ctrl, SPixel, X(iPc), &
              SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast), &
              RTM_Pc, status)
   else
      status = RTMIntflagErr
      call Write_Log(Ctrl, 'FM.f90: RTM Interp thermal flag error:', status)
      write(*,*) 'FM.f90: RTM Interp thermal flag error:', status
   end if

   ! Call Set_GZero (results used in both FM_Thermal and FM_Solar).
   call Allocate_GZero(GZero, SPixel)

   if (status == 0) then
      call Set_GZero(X(iTau), X(iRe), Ctrl, SPixel, SAD_LUT, GZero, status)
   end if

   ! Combine long and short wave transmittance values (depending on whether it
   ! is daytime, twilight or nighttime).
   !
   ! Note: For channels that are both thermal and solar the transmittances are
   ! taken from the interpolated thermal RTM values.
   !
   ! 2014/05/28, GM: This was causing problems that were hard to debug and
   ! gained little in performance.  Now the Solar and Thermal forward model
   ! calls are independent so that contents of CRP and d_CRP do not need to be
   ! passed from the thermal call to the solar call.

   ! Assign long wave transmittances to the combined Tac and Tbc vectors.

   ! Use ThF and ThL to access the first and last required thermal channels from
   ! RTM_Pc%LW arrays, since these are always allocated to size
   ! Ctrl%Ind%NThermal, but not all thermal channels are used in all SPixels
   ! (hence SPixel%Ind%ThermalFirst may not equal Ctrl%Ind%ThermalFirst). The
   ! problem does not occur with solar channels as we always use either all
   ! requested solar channels or none at all.

   if (status == 0) then
      ! These next two lines exclude the mixed channel during twilight conditions
      ! from the treatment of the thermal channels in the LW array(r.h.s). On
      ! l.h.s this is done via getillum
      ThF = 1 + SPixel%Ind%ThermalFirst - Ctrl%Ind%ThermalFirst
      ThL = Ctrl%Ind%NThermal

      RTM_Pc%Tac(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
         RTM_Pc%LW%Tac(ThF:ThL)
      RTM_Pc%Tbc(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
         RTM_Pc%LW%Tbc(ThF:ThL)

      RTM_Pc%dTac_dPc(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
         RTM_Pc%LW%dTac_dPc(ThF:ThL)
      RTM_Pc%dTbc_dPc(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
         RTM_Pc%LW%dTbc_dPc(ThF:ThL)

      ! Call thermal forward model (required for day, twilight and night)
      CRP   = 0.0
      d_CRP = 0.0


      call FM_Thermal(Ctrl, SAD_LUT, SPixel, &
              SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast), &
              RTM_Pc, X, GZero, CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:), d_CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:,:), BT, d_BT, Rad, d_Rad, status)

      ! Daytime
      if ((SPixel%Illum(1) .ne. Inight .and. SPixel%Illum(1) .ne. Itwi ) .and. status == 0) then
         ! Call routine to interpolate RTM data to the cloud pressure level.
         ! Interpol_Solar populates the SW part of RTM_Pc.
         if (Ctrl%RTMIntflag .eq. RTMIntMethLinear) then
            call Interpol_Solar(Ctrl, SPixel, X(iPc), RTM_Pc, status)
         else if (Ctrl%RTMIntflag .eq. RTMIntMethSpline) then
            call Interpol_Solar_spline(Ctrl, SPixel, X(iPc), RTM_Pc, status)
         else
            status = RTMIntflagErr
            call Write_Log(Ctrl, 'FM.f90: RTM Interp solar flag error:', status)
            write(*,*) 'FM.f90: RTM Interp solar flag error:', status
         end if

         RTM_Pc%Tac(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
            RTM_Pc%SW%Tac(:)
         RTM_Pc%Tbc(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
            RTM_Pc%SW%Tbc(:)

         RTM_Pc%dTac_dPc(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
            RTM_Pc%SW%dTac_dPc(:)
         RTM_Pc%dTbc_dPc(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
            RTM_Pc%SW%dTbc_dPc(:)

         ! Call short wave forward model. Note that solar channels only are
         ! passed (including mixed channels).
         CRP   = 0.0
         d_CRP = 0.0

         Ref   = 0.0
         d_Ref = 0.0

         call FM_Solar(Ctrl, SAD_LUT, SPixel, RTM_Pc, X, GZero, CRP(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast,:), d_CRP(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast,:,:), &
                 Ref(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast), d_Ref(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast,:), status)

         cloud_albedo(:)=CRP(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast,IRD)
!        d_cloud_albedo(:,:)=d_CRP(:,IRd,:) 


         ! Combine the results from the LW and SW forward models
         if (status == 0) then
            ! Purely solar channels
            Y(1:(SPixel%Ind%ThermalFirst-1)) = Ref(1:(SPixel%Ind%ThermalFirst-1))

            dY_dX(1:(SPixel%Ind%ThermalFirst-1),:) = d_Ref(1:(SPixel%Ind%ThermalFirst-1),:)


            ! Purely thermal channels. Y array is of size Ny channels, whereas
            ! BT and d_BT hold only thermal channels. 1+SPixel%Ind%NMixed
            ! offsets the starting index by the number of mixed channels (s.b.)
            Y(SPixel%Ind%SolarLast+1:SPixel%Ind%Ny) = &
               BT(1+SPixel%Ind%NMixed:SPixel%Ind%NThermal)

            dY_dX(SPixel%Ind%SolarLast+1:SPixel%Ind%Ny,1:MaxStateVar) = &
               d_BT(1+SPixel%Ind%NMixed:SPixel%Ind%NThermal,:)

            ! Although there is no value w.r.t Rs for the thermal channels, the
            ! dY_dX contains space for it, so set it to 0.
            dY_dX(SPixel%Ind%SolarLast+1:SPixel%Ind%Ny, IRs) = 0

            ! Mixed channels - when there are mixed channels present loop over
            ! them.  The mixed channel measurements are in brightness temperature.
            ! Convert reflectances to radiances using the solar constant f0 and
            ! calculate total radiances. The total radiances are converted to
            ! brightness temperatures using R2T.

            if (SPixel%Ind%SolarLast >= SPixel%Ind%ThermalFirst) then

               ! Sum the radiances and the reflectances (converted to radiances
               ! using f0) to give total radiance for the current scene, Y_R.
               Y_R(:) =  Rad(1:SPixel%Ind%NMixed) + &
                  (SPixel%f0(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) * &
                  Ref(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast))

               ! Call R2T to convert the scene radiance Y_R to brightness
               ! temperature. Write the result into the appropriate part of the
               ! measurement vector Y. The gradient dT_dR calculated at the scene
               ! radiance is used later.
               call R2T( SPixel%Ind%NMixed,&
                  SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast),  &
                  Y_R(:), Y(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast), &
                  dT_dR(:), status)

               if (status == 0) then
                  ! The gradients in Y w.r.t. state vector X.  Use dT_dR
                  ! calculated in the previous call to R2T to convert dR_dX to
                  ! dT_dX (i.e. dY_dX). Write result into appropriate part of
                  ! dY_dX array.
                  do i=1,MaxStateVar

                     dY_dX(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,i) = &
                        (d_Rad(1:SPixel%Ind%NMixed,i) + &
                        (SPixel%f0(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) * &
                        d_Ref(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,i))) * &
                        dT_dR(:)

                  end do

                  ! Deal with the Rs terms
                  dY_dX(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,IRs) = &
                     SPixel%f0(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) * &
                     d_Ref(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,IRs) * &
                     dT_dR(:)

               end if ! status == 0 from R2T
            end if ! (SolarLast >= ThermalFirst)
         end if ! status == 0 from FM_Solar

      ! Twilight and nighttime
      else if ((SPixel%Illum(1) .eq.  ITwi .or. SPixel%Illum(1) .eq. INight) &
              .and. status == 0) then
         ! Solar channels are set to zeros as these are not used (replace
         ! previous SPixel values)
!        Y(1:(SPixel%Ind%ThermalFirst-1))       = 0.0
!        dY_dX(1:(SPixel%Ind%ThermalFirst-1),:) = 0.0

         ! All available thermal channels (not mixed channels at twilight)

!        Y(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast)                   = BT
!        dY_dX(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,1:MaxStateVar) = d_BT(:,:)

         ! The above is not necessary since Y and dY_dX are passed with size
         ! SPixel%Ind%Ny. Hence, for nighttime or twilight there are only
         ! thermal channels passed.
         Y(1:SPixel%Ind%Ny)      = BT(1:SPixel%Ind%Ny)
         dY_dX(:, 1:MaxStateVar) = d_BT
      end if
   end if

   call Deallocate_GZero(GZero)

   ! Open breakpoint file if required, and write our reflectances and gradients.

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_FM) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
	   status='old',      &
	   position='append', &
	   iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
	 call Write_Log(Ctrl, 'FM: Error opening breakpoint file', status)
      else
         write(bkp_lun,'(/,a)')'FM:'
      end if

      write(bkp_lun,'(2(a,f9.2))') ' SolZen(1): ',SPixel%Geom%Solzen(1), &
         '  Max Sol Zen: ', Ctrl%MaxSolzen

      do i=1, SPixel%Ind%NThermal
         write(bkp_lun,'(3a,f9.4,a,5f9.4)')'Channel: ', &
	    SAD_Chan(i+(SPixel%Ind%ThermalFirst-1))%Desc,  &
	    ' Rad ',Rad(i), ' dRad ',(d_Rad(i,j), j=1,MaxStateVar)
      end do
      do i=1, SPixel%Ind%NThermal
         write(bkp_lun,'(3a,f9.4,a,5f9.4)')'Channel: ', &
	    SAD_Chan(i+(SPixel%Ind%ThermalFirst-1))%Desc, &
	    ' BT  ',BT(i), ' dBT  ',(d_BT(i,j), j=1,MaxStateVar)
      end do
      write(bkp_lun,'(/)')

      if (SPixel%Geom%Solzen(1) < Ctrl%MaxSolzen) then
         do i=1, SPixel%Ind%NSolar
            write(bkp_lun,'(3a,f9.4,a,6f9.4)')'Channel: ', &
	       SAD_Chan(i)%Desc, ' Ref ',Ref(i), ' dRef ', &
	       (d_Ref(i,j),j=1,MaxStateVar+1)
	 end do
         write(bkp_lun,'(/)')

         do i=1, SPixel%Ind%Ny
            write(bkp_lun,'(3a,f9.4,a,6f9.4)') 'Channel: ', &
	       SAD_Chan(i)%Desc, ' Y: ',Y(i),' dY_dX: ',&
	       (dY_dX(i,j),j=1,MaxStateVar+1)
	 end do
      else

       do i=1, SPixel%Ind%NThermal
	    write(bkp_lun,'(3a,f9.4,a,6f9.4)') 'Channel: ', &
	       SAD_Chan(i+(SPixel%Ind%ThermalFirst-1))%Desc, ' Y: ',Y(i),&
	       ' dY_dX: ',(dY_dX(i,j),j=1,MaxStateVar+1)
	 end do
      end if
      write(bkp_lun,'(/)')

      write(bkp_lun, '(a,/)') 'FM: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine FM
