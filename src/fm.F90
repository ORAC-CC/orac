!-------------------------------------------------------------------------------
! Name: fm.F90
!
! Purpose:
! Main forward model routine.
! Calls subroutines Interpol_Thermal and Interpol_Solar to interpolate the
! RTM quantities in SPixel%RTM onto the cloud pressure level. The thermal and
! shortwave forward model calculations take place in subroutines FM_Thermal
! and FM_Solar. The latter is only called during daytime conditions. At
! twilight and nighttime only FM_Thermal is run. At twilight only the pure
! thermal channels are used. At night all thermal channels are used. Daytime
! retrievals can use all available channels. In this last case pure thermal
! and pure solar channels are separated from the mixed channels during the
! calculation of the measurement vector Y. For the mixed channels, Y is
! calculated by summing contributions from reflectances (converted to
! radiances using solar constants f0) and radiances. R2T is used to convert
! the resulting total radiances to brightness temperatures. The gradients
! dY_dX are treated in a similar way, using dT_dR from the previous call to
! R2T. The results from the FMs are then combined into one measurement vector
! and array of gradients.
!
! Description and Algorithm details:
! 1) Update state vector X.
! 2) Interpolate LW RTM data onto cloud pressure level. (all LW channels,
!       including those with thermal and solar components)
! 3) Set zeroth point grid info. For SAD_LUT CRP interpolation (call Set_GZero).
! 4) Run thermal forward model (all channels with thermal component).
! 5) If daytime:
!       Interpolate SW RTM data onto cloud pressure level ("purely" solar
!          channels only, no mixed channels).
!       Run the shortwave forward model (all channels with solar component).
!       Combine the thermal and shortwave calculated radiances and reflectance
!       vectors and write to measurement vector Y (similarly for gradients
!       dY_dX). (Mixed channels are summed in BT via a call to R2T).
! 6) If twilight:
!       Only write radiances from the purely thermal channels to Y (similarly
!       for dY_dX).
! 7) If nighttime:
!       Write all thermal channels (including mixed channels) to Y and dY_dX.
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       In          Control structure
! SPixel   alloc struct In          Super pixel structure
! SAD_Chan struct arr   In          Array of SAD_Chan structures for each
!                                   channel
! SAD_LUT  struct arr   In          Array of SAD_LUT structures for each layer.
! RTM_Pc   struct       Both        Array to hold RTM data for each layer.
!                                   Interpolated to current Pc value. Passed in
!                                   because it contains allocated arrays.
!                                   Populated and used locally.
! X        real arr     In          State vector
! Y        real arr     Out         Calculated measurement vector
! dY_dX    real arr     Out         Gradient in Y wrt state parameters and Rs
! status   int          Out         Error status
!
! History:
! 2001/02/02, KS: original version (draft)
! 2001/03/02, AS: Updates to draft.
! 2001/03/07, AS: New arrays in RTM_Pc for overall dTac_dPc and dTbc_dPc.
!    Adding breakpoint outputs.
! 2001/03/08, AS: dB_dTs no longer required as an argument to FM_Thermal.
! 2001/03/15, AS: Added ThF and ThL indices for RTM_Pc%LW arrays. Required
!    because these arrays are allocated to match the no. of thermal channels
!    requested, but in twilight not all of the requested thermal channels may be
!    used. Moved InterpolSolar call into the "daytime" section. Not required in
!    twilight or nighttime conditions. Breakpoint SAD_Chan%Desc indexing changed:
!    failed for night-time conditions (needs checking) Using
!    SPixel%Ind%ThermalLast as the end of the channel range for RTM_Pc%Tac and
!    Tbc, instead of Ctrl%Ind%Ny. Changed declaration of SAD_Chan to size
!    Ctrl%Ind%Ny, not SPixel%Ind%Ny. (Allows use of SPixel%Ind%ThermalFirst etc
!    to select out channel ranges). Changed declaration of CRP, dCRP to size
!    Ctrl%Ind%Ny, not SPixel%Ind%Ny.
! 2001/04/11, AS: f0 argument no longer required. Now part of SPixel.
! 2011/05/04, AS: Extension to multiple instrument views. Values depending on
!    requested, but in twilight not all of the requested thermal channels may be
!    viewing  geometry are now arrays (no of views). Added SetGZero argument for
!    no of channels, needed for array sizing. In test for daytime/night
!    conditions, add array index to Solzen. Assume that the same illumination
!    applies to all instrument views in a given pixel, so we only need to test
!    one element of the array.
! 2011/05/19, AS: Multiple instrument views, part 2. In twilight and night
!    conditions the last element of dY_dX for each channel can be NaN or some
!    large value. Try initializing. It is not  clear why this has arisen now,
!    but uninitialized values can be dangerous anyway.
! 2011/09/05, CA: Now calls either linear *or* cubic-spline based RTM
!    interpolation  schemes depending on RTMIntflag
! 2011/12/13, CP: Deallocated GZero array at end of routine
! 2012/01/14, CP: Changed array input into FM_Thermal and FM_Solar to avoid no
!    contiguous arrays.
! 2012/06/15, CP: Changed the way day was defined to use illum value
! 2012/08/14, MJ: Removes bug in GZero allocation that made ORAC crash.
! 2012/08/17, MJ: Fixed bug uninitialized variables which cause NaN crash.
! 2012/10/24, CP: Notice a lot of this routine was modified without commenting
!    here!
! 2012/10/24, CP: Removed hardwiring of variable indices as this caused a
!    segmentation fault as there were in fact 3 solar channels
! 2012/09/20, CP: Assigned Y to explicit sizeY(1:SPixel%Ind%Ny) =
!    BT(1:SPixel%Ind%Ny)
! 2013/05/08, MJ: Makes some changes to merge code versions
! 2013/11/25, MJ: Dynamically sets upper limit for CTP to highest pressure in
!    profile to avoid extrapolation problems.
! 2013/12/06, MJ: Add deallocation statements to fix memory leaks.
! 2014/01/15, GM: Ctrl%Invpar%XULim(3)=SPixel%RTM%LW%p(SPixel%RTM%LW%Np), from
!    20131125 should be SPixel%XULim(3)=SPixel%RTM%LW%p(SPixel%RTM%LW%Np) but
!    it would also be clearer to move it just out of FM() to just after
!    Set_Limits() is called which is what was done here.
! 2014/01/17, MJ: Comments out usage of  some temp_* variables as they appear to
!    be unnecessary.
! 2014/01/19, GM: Cleaned up code.
! 2014/05/22, GM: Use allocate and deallocate subroutines for GZero.
! 2014/05/28, GM: The sharing of CRP results for mixed channels from FMThermal
!    with FMSolar was causing problems that were hard to debug and gained
!    little in performance.  Now the Solar and Thermal forward model calls are
!    independent so that contents of CRP and d_CRP do not need to be passed
!    from the thermal call to the solar call.
! 2014/07/15, CP: Changed illumination logic.
! 2014/12/01, CP: Added in cloud albedo.
! 2015/01/07, AP: Eliminate write to RTM_Pc%Tac, Tbc. Now within models.
! 2015/01/12, CP: Bugfix to cloud albedo.
! 2015/01/12, AP: Replacing First:Last channel indexing with generic,
!    array-based indexing.
! 2015/01/21, AP: Finishing the previous commit.
! 2015/01/30, GM: Fixed a bug in the recent channel indexing changes.
! 2015/08/21, AP: Turn off thermal retrieval with aerosol approaches.
! 2015/10/21, GM: Remove cloud albedo output as it is now evaluated elsewhere.
! 2016/07/27, GM: Changes for the multilayer retrieval.
! 2017/03/16, GT: Changes for single-view aerosol retrieval mode.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine FM(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, X, Y, dY_dX, status)

   use Ctrl_m
   use GZero_m
   use Int_LUT_Routines_m
   use Interpol_Routines_m
   use ORAC_Constants_m
   use planck_m
   use RTM_Pc_m
   use SAD_Chan_m
   use SAD_LUT_m
   use SPixel_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(in)    :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT(:)
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc(:)
   real,             intent(in)    :: X(:)
   real,             intent(out)   :: Y(:)
   real,             intent(out)   :: dY_dX(:,:)
   integer,          intent(out)   :: status

   ! Declare local variables

   integer          :: i
   type(GZero_t)    :: GZero(2)
   real             :: BT(SPixel%Ind%NThermal)
   real             :: d_BT(SPixel%Ind%NThermal, MaxStateVar)
   real             :: Rad(SPixel%Ind%NThermal)
   real             :: d_Rad(SPixel%Ind%NThermal, MaxStateVar)
   real             :: Ref(SPixel%Ind%NSolar)
   real             :: d_Ref(SPixel%Ind%NSolar, MaxStateVar)
   real             :: Ref_0d(SPixel%Ind%NSolar)
   real             :: d_Ref_0d(SPixel%Ind%NSolar, MaxStateVar)
   real             :: Ref_dv(SPixel%Ind%NSolar)
   real             :: d_Ref_dv(SPixel%Ind%NSolar, MaxStateVar)
   real             :: Ref_dd(SPixel%Ind%NSolar)
   real             :: d_Ref_dd(SPixel%Ind%NSolar, MaxStateVar)
   real             :: d_Ref2(SPixel%Ind%NSolar, MaxStateVar)
   real             :: d_Ref_0d2(SPixel%Ind%NSolar, MaxStateVar)
   real             :: d_Ref_dv2(SPixel%Ind%NSolar, MaxStateVar)
   real             :: d_Ref_dd2(SPixel%Ind%NSolar, MaxStateVar)
   real             :: d_Ref_dRs2(SPixel%Ind%NSolar, MaxRho_XX)
   real             :: Y_R(SPixel%Ind%NMixed)
   real             :: T(SPixel%Ind%NMixed)
   real             :: dT_dR(SPixel%Ind%NMixed)
   integer          :: itherm(SPixel%Ind%NMixed)
   integer          :: isolar(SPixel%Ind%NMixed)
   type(SAD_Chan_t) :: SAD_therm(SPixel%Ind%NThermal)
   type(SAD_Chan_t) :: SAD_mixed(SPixel%Ind%NMixed)

   status = 0

   Y     = 0.0
   dy_dX = 0.0

   ! Call Set_GZero (results used in both FM_Thermal and FM_Solar).
   call Allocate_GZero(GZero(1), SPixel%Ind%Ny)
   call Set_GZero(X(ITau), X(IRe), Ctrl, SPixel, SAD_LUT(1), GZero(1), status)

   ! If the two layer retrieval is active call Set_GZero for the second layer.
   if (Ctrl%Approach == AppCld2L) then
      call Allocate_GZero(GZero(2), SPixel%Ind%Ny)
      call Set_GZero(X(ITau2), X(IRe2), Ctrl, SPixel, SAD_LUT(2), GZero(2), status)
   end if

   ! Evaluate long and short wave transmittance values (depending on whether it
   ! is daytime, twilight or nighttime).

   ! Call thermal forward model (required for day, twilight and night)
   ! ACP: Temporarily turn off Thermal entirely for aerosol retrieval
   if (SPixel%Ind%NThermal > 0 .and. status == 0 .and. &
        Ctrl%Approach /= AppAerOx .and. Ctrl%Approach /= AppAerSw .and. &
        Ctrl%Approach /= AppAerO1) then
      SAD_therm = SAD_Chan( &
           SPixel%spixel_y_thermal_to_ctrl_y_index(1:SPixel%Ind%NThermal))

      ! Call routine to interpolate RTM data to the cloud pressure level.
      ! Interpol_Thermal returns transmittances in the LW part of RTM_Pc.
      select case (Ctrl%RTMIntSelm)
      case (RTMIntMethLinear)
         call Interpol_Thermal(Ctrl, SPixel, X(IPc), &
                               SAD_therm, RTM_Pc(1), status)
         ! If the two layer retrieval is active call Interpol_Thermal for the
         ! second layer.
         if (Ctrl%Approach == AppCld2L) then
            call Interpol_Thermal(Ctrl, SPixel, X(IPc2), &
                                  SAD_therm, RTM_Pc(2), status)
         end if
      case (RTMIntMethSpline)
         call Interpol_Thermal_spline(Ctrl, SPixel, X(IPc), &
                                      SAD_therm, RTM_Pc(1), status)
         ! If the two layer retrieval is active call Interpol_Thermal for the
         ! second layer.
         if (Ctrl%Approach == AppCld2L) then
            call Interpol_Thermal_spline(Ctrl, SPixel, X(IPc2), &
                                         SAD_therm, RTM_Pc(2), status)
         end if
      case (RTMIntMethNone)
         write(*,*) 'ERROR: FM(): Thermal forward model requires RTTOV inputs.'
         stop RTMIntflagErr
      case default
         write(*,*) 'ERROR: FM(): Invalid value for Ctrl%RTMIntSelm: ', &
              Ctrl%LUTIntSelm
         status = RTMIntflagErr
         return
      end select

      ! Call thermal forward model (required for day, twilight and night)
      call FM_Thermal(Ctrl, SAD_LUT, SPixel, SAD_therm, RTM_Pc, X, GZero, BT, &
           d_BT, Rad, d_Rad, status)

      ! Copy results into output vectors
      Y(SPixel%Ind%YThermal) = BT
      dY_dX(SPixel%Ind%YThermal,:) = d_BT
   end if

   ! Call solar forward model
   if (SPixel%Ind%NSolar > 0 .and. status == 0) then
      ! Call routine to interpolate RTM data to the cloud pressure level.
      ! Interpol_Solar populates the SW part of RTM_Pc.
      select case (Ctrl%RTMIntSelm)
      case (RTMIntMethLinear)
         call Interpol_Solar(Ctrl, SPixel, X(IPc), RTM_Pc(1), status)
         if (Ctrl%Approach == AppCld2L) then
            call Interpol_Solar(Ctrl, SPixel, X(IPc2), RTM_Pc(2), status)
         end if
      case (RTMIntMethSpline)
         call Interpol_Solar_spline(Ctrl, SPixel, X(IPc), RTM_Pc(1), status)
         if (Ctrl%Approach == AppCld2L) then
            call Interpol_Solar_spline(Ctrl, SPixel, X(IPc2), RTM_Pc(2), status)
         end if
      case (RTMIntMethNone)
      case default
         write(*,*) 'ERROR: FM(): Invalid value for Ctrl%RTMIntSelm: ', &
              Ctrl%LUTIntSelm
         status = RTMIntflagErr
         return
      end select

      ! Call short wave forward model. Note that solar channels only are
      ! passed (including mixed channels).
      Ref   = 0.0
      d_Ref = 0.0
      if (Ctrl%Approach /= AppCld2L) then
         ! A single call for bidirectional reflectance is required when there
         ! is only one particle layer.
         call FM_Solar(Ctrl, SAD_LUT(1), SPixel, 0, RTM_Pc(1), RTM_Pc(1), X, &
              GZero(1), Ref, d_Ref, status)
      else
         ! When there are two layers the first call is to obtain BRDF
         ! parameters of the lower layer combined with the atmosphere and
         ! surface below.
         call FM_Solar(Ctrl, SAD_LUT(2), SPixel, 2, RTM_Pc(2), RTM_Pc(1), X, &
              GZero(2), Ref, d_Ref, status, Ref_0d=Ref_0d, d_Ref_0d=d_Ref_0d, &
              Ref_dv=Ref_dv, d_Ref_dv=d_Ref_dv, Ref_dd=Ref_dd, d_Ref_dd=d_Ref_dd)

         ! Bidirectional is returned sun normalized whereas BRDF parameters
         ! should not be sun normalized so we invert the normalization.
         if (Ctrl%i_equation_form == 1 .or. Ctrl%i_equation_form == 3) then
            Ref   = Ref   * SPixel%Geom%SEC_o(1)
            d_Ref = d_Ref * SPixel%Geom%SEC_o(1)
         end if

         ! Save the BRDF derivatives for propagation through the top layer.
         d_Ref2    = d_Ref
         d_Ref_0d2 = d_Ref_0d
         d_Ref_dv2 = d_Ref_dv
         d_Ref_dd2 = d_Ref_dd

         ! Call FM_Solar() again for the top layer where the BRDF parameters
         ! obtained from the first call are input as the surface BRDF parameters
         ! and our output is the bidirectional reflectance of the top layer and
         ! the associated derivatives w.r.t parameters in the top layer with all
         ! other derivatives (w.r.t parameters in the bottom layer and the
         ! surface) set to zero.
         call FM_Solar(Ctrl, SAD_LUT(1), SPixel, 1, RTM_Pc(1), RTM_Pc(2), X, &
              GZero(1), Ref, d_Ref, status,RsX=Ref,RsX_0d=Ref_0d,RsX_dv=Ref_dv, &
              RsX_dd=Ref_dd, d_Ref_dRs2=d_Ref_dRs2)

         ! Propagate derivatives w.r.t parameters in the bottom layer and the
         ! surface though the top layer using derivatives w.r.t to the input
         ! surface BRDF parameters of the second call to FM_Solar(), d_Ref_dRs2.
         call propagate_d_Ref_dRs2(SPixel%Nx, SPixel%X, d_Ref, d_Ref_dRs2, &
                                   d_Ref2, d_Ref_0d2, d_Ref_dv2, d_Ref_dd2)
         call propagate_d_Ref_dRs2(SPixel%NxI, SPixel%XI, d_Ref, d_Ref_dRs2, &
                                   d_Ref2, d_Ref_0d2, d_Ref_dv2, d_Ref_dd2)
         call propagate_d_Ref_dRs2(SPixel%NxJ, SPixel%XJ, d_Ref, d_Ref_dRs2, &
                                   d_Ref2, d_Ref_0d2, d_Ref_dv2, d_Ref_dd2)
      end if

      ! Copy results into output vectors (may overwrite mixed chs)
      Y(SPixel%Ind%YSolar) = Ref
      dY_dX(SPixel%Ind%YSolar,:) = d_Ref
   end if


   ! Mixed channels - when there are mixed channels present loop over
   ! them.  The mixed channel measurements are in brightness temperature
   ! Convert reflectances to radiances using the solar constant f0 and
   ! calculate total radiances. The total radiances are converted to
   ! brightness temperatures using R2T.
   if (SPixel%Ind%NMixed > 0 .and. status == 0) then
      itherm = SPixel%spixel_y_mixed_to_spixel_y_thermal(1:SPixel%Ind%NMixed)
      isolar = SPixel%spixel_y_mixed_to_spixel_y_solar(1:SPixel%Ind%NMixed)

      ! Sum the radiances and the reflectances (converted to radiances
      ! using f0) to give total radiance for the current scene, Y_R.
      Y_R = Rad(itherm) + SPixel%f0(isolar) * Ref(isolar)

      ! Call R2T to convert the scene radiance Y_R to brightness
      ! temperature. Write the result into the appropriate part of the
      ! measurement vector Y. The gradient dT_dR calculated at the scene
      ! radiance is used later.
      SAD_mixed = SAD_Chan(SPixel%spixel_y_to_ctrl_y_index(SPixel%Ind%YMixed))
      call R2T(SPixel%Ind%NMixed, SAD_mixed, Y_R, T, dT_dR, status)
      if (status == 0) then
         ! Write output into Y array
         Y(SPixel%Ind%YMixed) = T

         ! The gradients in Y w.r.t. state vector X.  Use dT_dR
         ! calculated in the previous call to R2T to convert dR_dX to
         ! dT_dX (i.e. dY_dX). Write result into appropriate part of
         ! dY_dX array.
         do i = 1, MaxStateVar
            dY_dX(SPixel%Ind%YMixed,i) = (d_Rad(itherm,i) + &
                 SPixel%f0(isolar) * d_Ref(isolar,i)) * dT_dR
         end do

         ! Deal with the Rs terms
!         dY_dX(SPixel%Ind%YMixed,IRs(1,1)) = &
!              SPixel%f0(isolar) * d_Ref(isolar,IRs(1,1)) * dT_dR

      end if ! status == 0 from R2T
   end if ! SPixel%Ind%NMixed > 0

   call Deallocate_GZero(GZero(1))
   if (Ctrl%Approach == AppCld2L) then
      call Deallocate_GZero(GZero(2))
   end if

end subroutine FM


subroutine propagate_d_Ref_dRs2(Nx, X, d_Ref, d_Ref_dRs2, d_Ref0, d_Ref_0d0, &
                                d_Ref_dv0, d_Ref_dd0)

   implicit none

   integer, intent(in)    :: Nx
   integer, intent(in)    :: X(:)
   real,    intent(inout) :: d_Ref(:,:)
   real,    intent(in)    :: d_Ref_dRs2(:,:)
   real,    intent(in)    :: d_Ref0(:,:)
   real,    intent(in)    :: d_Ref_0d0(:,:)
   real,    intent(in)    :: d_Ref_dv0(:,:)
   real,    intent(in)    :: d_Ref_dd0(:,:)

   integer :: i, ii

   do i = 1, Nx
      ii = X(i)

      d_Ref(:,ii) = d_Ref(:,ii) + d_Ref_dRs2(:,1) * d_Ref0(:,ii)
      d_Ref(:,ii) = d_Ref(:,ii) + d_Ref_dRs2(:,2) * d_Ref_0d0(:,ii)
      d_Ref(:,ii) = d_Ref(:,ii) + d_Ref_dRs2(:,3) * d_Ref_dv0(:,ii)
      d_Ref(:,ii) = d_Ref(:,ii) + d_Ref_dRs2(:,4) * d_Ref_dd0(:,ii)
   end do

end subroutine propagate_d_Ref_dRs2
