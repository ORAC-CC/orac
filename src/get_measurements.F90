!-------------------------------------------------------------------------------
! Name: get_measurements.F90
!
! Purpose:
! Gets the 'average' measurements for the current super-pixel, plus the error
! covariance.
!
! Description and Algorithm details:
! Determine which averaging method is required
!    'All'
!       Average measurements at the four corners of the super-pixel
!    'Cloudy'
!       Average all cloudy pixels as defined by SPixel%CloudFlags
!    'Central'
!       Take measurements from the 'central' pixel as defined by
!       SPixel%Loc%Xc and Yc
! Average measurements output to SPixel%Ym
!
! Allocate super-pixel error covariance Sy to correct number of channels
! if (Homogeneity noise requested)
!    for each channel, add the NeHomog noise from SAD_Chan to the appropriate
!     diagonal value in Sy
!     - in daytime the noise value is taken from the Solar sub-structure for
!       "pure" solar channels, the thermal sub-struct for thermal channels
!       and the sum of the two for mixed channels
!     - otherwise, only thermal noise is used
! if (Co-registration noise requested)
!    for each channel, add the NeCoreg noise from SAD_Chan to the appropriate
!    diagonal value in Sy
!    - same day/twi/night conditions apply as for NeHomog
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure
! SAD_Chan struct  In          Array of structs containing channel
!                              characteristics.
! SPixel   struct  Both        Super-pixel structure
! MSI_Data struct  In          Data structure. Contains the multi-spectral
!                              image measurements, location values, geometry
!                              etc for the current image segment, from which
!                              the current SPixel values will be extracted.
! status   integer Out         Error status
!
! History:
! 2000/11/29, KS: Original version
! 2000/12/04, KS: corrections to 'average all' method
! 2000/12/19, KS: Replaced Data_MSI array with Data structure
! 2001/01/08, KS: Included quality control mask
! 2001/01/11, KS: Added check for allowed solar zenith angles
! 2001/01/18, KS: Changed Sol_zen to Solzen in SPixel structure
! 2001/03/16, AS: Using named constants for averaging method and SPixel index
!    values to select thermal channels. Removed warning set if solar zenith
!    angle > Max in Ctrl file (only indicates non-daytime data). Removed check
!    on Ctrl%Resoln%Ameth: now done once only in ReadDriver.
! 2001/04/03, AS: Replaced loop over channels for AMethCentral case with a
!    whole-array assignment. Should be more efficient.
! 2001/05/17, AS: Added setting of the SPixel%Sy measurement error covariance
!    values. New argument SAD_Chan required for noise information.
! 2001/06/11, AS: Change to setting of Sy for mixed channels.
! 2001/06/25, AS: Header comments updated following recent changes to
!    functionality.
! 2001/08/07, AS: Updates for image segmentation. Selection of values from the
!    MSI Data structure arrays now need to use a y value that refers to the
!    image segment currently held in memory rather than the whole image area. X
!    co-ords are unchanged since the segment is the full image width. Renamed
!    structure Data to MSI_Data since Data is a reserved word (hasn't caused any
!    problems so far but it might).
! 2001/08/14, AS: Change to Sy setting. When homog and coreg noise are added for
!    purely solar channels, the values in the SAD_Chan arrays (which are
!    percentages) must be multiplied by the measurement values to convert them
!    to the reflectance errors. Thermal channel values are already stored as BT
!    error.
! 2001/09/21, AS: Memory leak fix. Now deallocates SPixel%Ym and Sy before each
!    allocation. It is assumed that an initial allocation was made (in ECP main)
!    otherwise the first deallocate will fail.
!    **************** ECV work starts here *************************************
! 2011/02/23, AS: Cloud flags converted to real to match current ORAC data.
! 2011/03/30, AS: Removal of super-pixel averaging. Process single pixels at X0,
!    Y0, removed other SPixel indices Xc, Yc, Xn, Yn etc.
! 2011/04/21, AS: Extension to handle multiple instrument views. New SPixel
!    array View_Idx, holds the view index values for active channels in this
!    pixel.
! 2012/06/15, CP: Added illum array
! 2014/05/21, GM: Cleaned up the code.
! 2012/07/08, CP: Changed ilumination logic.
! 2014/08/01, GM: The above change requires use of
!    SPixel%spixel_y_to_ctrl_y_index(:) to properly index the right channels
!    from MSI_Data%MSI(:,:,:) and cleanup.
! 2015/01/13, AP: Remove ThermalFirst,ThermalLast.
! 2015/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/08/19, AP: Sy may now be drawn from the MSI data files (SelmMeas), the
!    SAD files (SelmAux), or taken to be constant (SelmCtrl).
! 2021/10/12, ATP: Implement NEBT and NEDR that vary with the measurements.
!    This requires the new (netcdf) LUTs. When using netcdf LUTs,
!    Homog and Coreg noise are independent of LUT being used.
! 2023/06/13, AP: Add alternative calculation of uncertainty in mixed channels
!    that uses the observed radiance rather than the solar constant to convert
!    Solar%NeHomog/Coreg from reflectance into radiance. Additionally, add
!    Thermal%NeHomog/Coreg to IDay pixels as well.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#define USE_OLD_MIXED_UNCERTAINTY .false.
#define THERMAL_ALWAYS_HAS_HOMOG_COREG .true.

subroutine Get_Measurements(Ctrl, SAD_Chan, SPixel, MSI_Data, status)

   use Ctrl_m
   use Data_m
   use ORAC_Constants_m
   use planck_m
   use SAD_Chan_m

   implicit none

   ! Define arguments
   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status

   ! Define local variables
   integer :: i, ii, j, jj
   real    :: Rad(1)    ! Radiance calculated from noise equivalent brightness
                        ! temperature used in setting Sy for mixed thermal /
                        ! solar channels.
   real    :: dR_dT(1)  ! Gradient of Rad w.r.t temp.

   ! Define variables for computing reflectance varying NEDR
   real    :: Lx        ! Radiance measurement
   real    :: dLx       ! Radiance uncertainty

   ! Define variables for computing brightness temperature-varying NEBT.
   real    :: dR_dT0(1) ! Gradient of Rad w.r.t. reference temperature.
   real    :: dR_dTm(1) ! Gradient of Rad w.r.t. measured temperature.
   real    :: R0(1)     ! Radiance calculated reference temperature.
   real    :: Rm(1)     ! Radiance calculated measured temperature.

   ! Homogeneity (Homog) and Co-registration (Coreg) noise
   real    :: ThermalNeHomog   ! Homog noise for thermal channels
   real    :: SolarNeHomog     ! Homog noise for solar channels
   real    :: ThermalNeCoreg   ! Coreg noise for thermal channels
   real    :: SolarNeCoreg     ! Coreg noise for solar channels

   status = 0

   ! Reallocate to the appropriate size for the SPixel and Assign SPixel values.
   ! Use Ctrl indices for the Data array since this is populated for all
   ! requested channels (not just those that are valid for the current SPixel).

   deallocate(SPixel%Ym)
   allocate(SPixel%Ym(SPixel%Ind%Ny))
   deallocate(SPixel%ViewIdx)
   allocate(SPixel%ViewIdx(SPixel%Ind%Ny))

   do i = 1, SPixel%Ind%Ny
      ii = SPixel%spixel_y_to_ctrl_y_index(i)
      SPixel%Ym(i) = MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, ii)
      SPixel%ViewIdx(i) = Ctrl%Ind%View_Id(ii)
   end do
!  SPixel%Ind%NViews = maxval(SPixel%ViewIdx)

   ! Reallocate the measurement error covariance array to the appropriate size
   ! for the SPixel and set the values. Initial value is the same as Ctrl.

   deallocate(SPixel%Sy)
   allocate(SPixel%Sy(SPixel%Ind%Ny, SPixel%Ind%Ny))
   SPixel%Sy = 0.

   select case (Ctrl%EqMPN%SySelm)
   case(SelmMeas)
      ! Use Sy matrix read in from MSI file
      do i = 1, SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)
         ! Square value to a variance
         SPixel%Sy(i,i) = MSI_Data%SD(SPixel%Loc%X0, SPixel%Loc%Y0, ii) * &
                          MSI_Data%SD(SPixel%Loc%X0, SPixel%Loc%Y0, ii)
      end do
   case(SelmAux)
      ! Use values read from SAD_Chan files
      do i = 1, SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)
         if (SAD_Chan(ii)%Thermal%Flag > 0) then
            if (len_trim(Ctrl%LUTClass) == 3) then
               ! Old LUTs approach (fixed uncertainty)
               ! Note: NEBT is already squared when old LUT is read in.
               SPixel%Sy(i,i) = SAD_Chan(ii)%Thermal%NEBT
            else
               ! New LUTs approach (varying uncertainty)
               ! Note: NEBT is NOT squared when new LUT is read in.
               call T2R(1, SAD_Chan(ii:ii), SAD_Chan(ii:ii)%Thermal%T0, R0, dR_dT0, status)
               call T2R(1, SAD_Chan(ii:ii), SPixel%Ym(i:i), Rm, dR_dTm, status)
               ! Convert to variance by squaring uncertainty
               SPixel%Sy(i,i) = (SAD_Chan(ii)%Thermal%NEBT * &
                                 (dR_dT0(1) / dR_dTm(1))) ** 2
            end if
         else
            if (len_trim(Ctrl%LUTClass) == 3) then
               ! Old LUTs approach (fixed uncertainty)
               ! Note: NEDR is squared when old LUT is read in.
               SPixel%Sy(i,i) = SAD_Chan(ii)%Solar%NEDR
            else
               ! New LUTs approach (varying uncertainty)
               ! Convert reflectance to radiance
               j = SPixel%ViewIdx(i)
               Lx = (SPixel%Ym(i) * cos(SPixel%Geom%SolZen(j) * d2r) * &
                     SAD_Chan(ii)%Solar%F0) / Pi
               ! Compute uncertainty in terms of radiance
               if (SAD_Chan(ii)%Solar%SNR > 0.0) then
                  if (MSI_Data%cal_gain(ii) > 0.0) then
                     ! variance = (Lx / SNR)**2 + 2(gain / sqrt(12))**2
                     dLx = sqrt((Lx / SAD_Chan(ii)%Solar%SNR) ** 2 + &
                              MSI_Data%cal_gain(ii) ** 2 / 6.0)
                  else
                     ! Sensor does not report gain so switch to 2nd order version of below
                     dLx = Lx / SAD_Chan(ii)%Solar%SNR
                  end if
               else
                  ! variance = a**2 Lx**2 + b**2 Lx + c**2
                  dLx = sqrt(SAD_Chan(ii)%Solar%ru2(1) * Lx * Lx + &
                             SAD_Chan(ii)%Solar%ru2(2) * Lx + &
                             SAD_Chan(ii)%Solar%ru2(3))
               end if
               ! Convert radiance uncertainty to reflectance uncertainty
               ! then square it to convert to reflectance variance
               SPixel%Sy(i,i) = ((Pi * dLx) / &
                                (cos(SPixel%Geom%SolZen(j) * d2r) * &
                                 SAD_Chan(ii)%Solar%F0)) ** 2
            end if
         end if
      end do
   case(SelmCtrl)
      ! Use Sy matrix put in Ctrl by Read_Driver
      do i = 1, SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)
         do j = 1, SPixel%Ind%Ny
            jj = SPixel%spixel_y_to_ctrl_y_index(j)
            SPixel%Sy(i,j) = Ctrl%Sy(ii, jj)
         end do
      end do
   end select

   ! Add in the Homog and Coreg noise if the appropriate Ctrl flags are set
   ! (For mixed solar/thermal channels, the value in daytime is a sum of the
   ! thermal and solar contributions).

   if (Ctrl%EqMPN%Homog) then
      ! Set values for cloud homogeneity noise to account for errors due
      ! to the plane-parallel cloud assumption. Values are taken from
      ! Table 4 in Watts et al. (2011) and were originally calculated
      ! for a pre-launch SEVIRI using ASTR-2 data (Watts et al., 1998).
      ! Originally, 5 different values were set for each cloud type.
      ! However, as we do not classify clouds by type, only a single
      ! value is used. Currently, we take the maximum value reported
      ! in Table 4 of Watts et al. (2011) for thermal and solar channels,
      ! acknowledging that these values need revision in the future.
      ! In addition, these values are now set independent of the
      ! instrument as they are forward model errors. Legacy values for
      ! Homog can be found in the old *.sad files for each instrument.
      ThermalNeHomog = 0.50  ! K (absolute error)
      SolarNeHomog = 1.0 ! % of total signal (fractional error)

      do i = 1, SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)

         if (SPixel%Ym(i) .le. 0.0) then
            SPixel%Sy(i,i) = 1.e6
         end if

         ! Daylight
         if (SPixel%Illum == IDay .and. SAD_Chan(ii)%Solar%Flag /= 0) then
            if (SAD_Chan(ii)%Thermal%Flag /= 0) then
               ! Both solar and thermal => mixed

               ! Compute partial derivative of Planck function w.r.t.
               ! T, evaluated at the measured BT and wavelength of
               ! mixed channel and store result in dR_dT(1).
               call T2R(1, SAD_Chan(ii:ii), SPixel%Ym(i:i), Rad, dR_dT, status)
               if (USE_OLD_MIXED_UNCERTAINTY) Rad(1) = SAD_Chan(ii)%Solar%f0
               if (len_trim(Ctrl%LUTClass) == 3) then
                  ! Old LUTs approach reads homog from sad files.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       SAD_Chan(ii)%Solar%NeHomog(Ctrl%CloudType) * &
                       Rad(1) * Rad(1) / &
                       (dR_dT(1)*dR_dT(1))
               else
                  ! New LUTs approach uses homog value hard-coded above.
                  ! Add solar and thermal contributions of homog noise
                  ! in units of Kelvin^2 to Sy by multiplying
                  ! solar contribution by (F0/dR_dT)^2.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       ((SolarNeHomog/100.) * Rad(1) / dR_dT(1)) ** 2
               end if

            else
               ! Pure solar channel, just add the solar NedR contribution.
               if (len_trim(Ctrl%LUTClass) == 3) then
                  ! Old LUTs approach reads homog from sad files.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       SAD_Chan(ii)%Solar%NeHomog(Ctrl%CloudType) * &
                       SPixel%Ym(i) * SPixel%Ym(i)
               else
                  ! New LUTs approach uses homog value hard-coded above.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       ((SolarNeHomog/100.) * SPixel%Ym(i)) ** 2
               end if
            end if
         end if
         ! Night/twilight, only thermal channel info required
         if (SAD_Chan(ii)%Thermal%Flag /= 0 .and. &
              (THERMAL_ALWAYS_HAS_HOMOG_COREG .or. SPixel%Illum /= IDay .or. &
              SAD_Chan(ii)%Solar%Flag /= 0)) then
            ! Pure thermal channel, add the thermal contribution to
            ! Sy by squaring absolute uncertainties.
            if (len_trim(Ctrl%LUTClass) == 3) then
               ! Old LUTs approach reads homog from sad files.
               SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                    SAD_Chan(ii)%Thermal%NeHomog(Ctrl%CloudType)
            else
               ! New LUTs approach uses homog value hard-coded above.
               SPixel%Sy(i,i) = SPixel%Sy(i,i) + ThermalNeHomog ** 2
            end if
         end if
      end do
   end if ! End of Homog noise section

   if (Ctrl%EqMPN%Coreg) then
      ! Set values for co-registration noise to account for errors due
      ! to different IFOVs between channels. Currently, we take the
      ! maximum value reported in Table 4 of Watts et al. (2011)
      ! for thermal and solar channels. In the future, these values
      ! should vary based on channel and be reported as a pixel offset
      ! in metres or pixel fraction and then converted to a BT or
      ! reflectance by applying the associated pixel shifts as in
      ! Watts et al. (1998). This approach would allow Coreg noise to
      ! be scene and instrument dependent and would therefore not be
      ! considered a forward model error. Legacy values for Coreg can
      ! be found in the old *.sad files for each instrument.
      ThermalNeCoreg = 0.15  ! K (absolute error)
      SolarNeCoreg = 2.0 ! % of total signal (fractional error)

      do i = 1, SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)

         ! Daylight
         if (SPixel%Illum == IDay .and. SAD_Chan(ii)%Solar%Flag /= 0) then
            if (SAD_Chan(ii)%Thermal%Flag /= 0) then
               ! Both solar and thermal => mixed

               ! Compute partial derivative of Planck function w.r.t.
               ! T, evaluated at the measured BT and wavelength of
               ! mixed channel and store result in dR_dT(1).
               call T2R(1, SAD_Chan(ii:ii), SPixel%Ym(i:i), Rad, dR_dT, status)
               if (USE_OLD_MIXED_UNCERTAINTY) Rad(1) = SAD_Chan(ii)%Solar%f0
               if (len_trim(Ctrl%LUTClass) == 3) then
                  ! Old LUTs approach reads coreg from sad files.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       SAD_Chan(ii)%Solar%NeCoreg(Ctrl%CloudType) * &
                       Rad(1) * Rad(1) / &
                       (dR_dT(1)*dR_dT(1))
               else
                  ! New LUTs approach uses coreg value hard-coded above.
                  ! Add solar and thermal contributions of coreg noise
                  ! in units of Kelvin^2 to Sy by multiplying
                  ! solar contribution by (F0/dR_dT)^2.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       ((SolarNeCoreg/100.) * Rad(1) / dR_dT(1)) ** 2
               end if

            else
               ! Pure solar channel, just add the solar NedR contribution.
               if (len_trim(Ctrl%LUTClass) == 3) then
                  ! Old LUTs approach reads coreg from sad files.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       SAD_Chan(ii)%Solar%NeCoreg(Ctrl%CloudType) * &
                       SPixel%Ym(i) * SPixel%Ym(i)
               else
                  ! New LUTs approach uses coreg value hard-coded above.
                  SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                       ((SolarNeCoreg/100.) * SPixel%Ym(i)) ** 2
               end if
            end if
         end if
         ! Add thermal channel info
         if (SAD_Chan(ii)%Thermal%Flag /= 0 .and. &
              (THERMAL_ALWAYS_HAS_HOMOG_COREG .or. SPixel%Illum /= IDay .or. &
              SAD_Chan(ii)%Solar%Flag /= 0)) then
            ! Pure thermal channel, add the thermal contribution to
            ! Sy by squaring absolute uncertainties.
            if (len_trim(Ctrl%LUTClass) == 3) then
               ! Old LUTs approach reads coreg from sad files.
               SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                    SAD_Chan(ii)%Thermal%NeCoreg(Ctrl%CloudType)
            else
               ! New LUTs approach uses coreg value hard-coded above.
               SPixel%Sy(i,i) = SPixel%Sy(i,i) + ThermalNeCoreg ** 2
            end if
         end if
      end do
   end if ! End of Coreg noise section

end subroutine Get_Measurements
