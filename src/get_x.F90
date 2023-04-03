!-------------------------------------------------------------------------------
! Name: get_x.F90
!
! Purpose:
! Gets the first guess and a priori state vector values (X0 and Xb) for the
! current super pixel according to the methods specified by SPixel%FG and
! SPixel%AP.
!
! Description and Algorithm details:
! Set "first guess" phase to the value for the retrieval (phase is no longer
!    retrieved).
! Set cloud class depending on the FG phase
! For each state variable
! - Set a priori state vector and error covariance by the selected method
! - Set first guess state vector by the selected method
!   (if the FG method matches the AP, just copy the AP value to the FG)
! - Check both state vectors for internal consistency (i.e. check vs. the
!   limits for the cloud class)
! (Note that not all methods are supported for all variables).
!
! Note that when error values are associated with the a priori the values
! must be squared for use in the ORAC. It is assumed that the values obtained
! by each method are the rms errors.
!
! Arguments:
! Name           Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl           struct  In   Control structure
! SPixel         struct  Both Super-pixel structure (contains the phase and
!                             AP, FG arrays to be set).
! status         integer Out  Error status
!
! History:
! 2001/01/30, KS: Original version
! 2001/04/24, AS: Using named constants for allowed selection method values.
!    Moved test for valid selection method to Read_Driver. No need to test for
!    every super-pixel. Using SPixel%FGPhase instead of SPixel%X0(6) to store
!    phase. Added setting of SPixel%FGCloudClass.
! 2001/06/08, AS: Changed structure of first guess phase setting to take account
!    of cases when XMDAD setting is requested but unsuccessful.
!    Added scaling of Sx.
! 2001/06/11, AS: XScale must be squared where used with Sx since Sx contains
!    the square of the errors in X.
! 2001/07/06, AS: Update to comments. Added status check on SDAD a priori setting
! 2001/07/17, AS:  Using named constants for Ts errors when set by AUX method.
!    Errors now squared.
!    **************** ECV work starts here *************************************
! 2011/03/21, AS:  Removal of functionality to change phase during retrieval.
!    Phase will be initialised to the Ctrl value in the main function. Since
!    phase is now fixed for the retrieval only 1 cloud class is used, hence the
!    first guess cloud class is the 1 available class.
! 2011/03/22, AS:  Removal of phase change, phase 2. Only 1 cloud class rather
!    than an array of N cloud classes. Ctrl struct reorganised so that X0, Xb,
!    limits etc only refer to one fixed phase (N.B. state variable not updated,
!    so phase is still "retrieved").
! 2011/04/05, AS: Removed selection methods SAD and SDAD. Renamed SelmMDAD to
!    SelmMeas. Mopping up from removal of phase change. Removed setting of
!    SPixel%FGPhase as it is redundant.
! 2011/08/05, CP: Remove rf to sadcloudclass to simplify code. These values can
!    be read  from the driver file instead.
! 2012/06/20, CP: Added in sacura option
! 2012/10/02, CP: Set in active statearibaes error to a very low number i.e
!    assume they are well known and let information csignal go to active state
!    variables
! 2012/10/03, CP: Modified how Ctrl%sx is set added skint to be surface
!    temperature first guess
! 2013/02/26, CP: Added in option to process pixel if only one IR channel is
!    present
! 2014/01/15, GM: Fixed setting of SPixel%Sx for inactive state variables so
!    that Ctrl%Sx is not modified.
! 2014/08/01, GM: Cleaned up the code.
! 2015/01/27, AP: Switched first guess auxilliary surface temperature from
!    lowest T level to skint to be consistent with a priori. As rttov_driver.F90
!    sets these to be the same, it makes little difference other than tidiness.
! 2015/06/02, AP: Moved repeated code into subroutine.
! 2015/08/14, AP: Only set needed variables. Adding aerosol surface setup.
! 2015/08/20, GM: Fix SelmCtrl setting of SPixel%X0.
! 2016/01/02, AP: Ctrl%RS%diagonal_SRs produces a diagonal covariance matrix.
! 2016/10/21, AP: Add AppAerSw to SelmAux.
! 2022/01/27, GT: Added Cloud top pressure SelmAux selection method of
!    Get_State()
!
! Bugs:
! Does not facilitate correlation between surface reflectance terms.
!-------------------------------------------------------------------------------

subroutine Get_X(Ctrl, SPixel, MSI_Data, status)

   use Ctrl_m
   use ORAC_Constants_m
   use Data_m

   implicit none

   ! Declare arguments
   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status

   ! Local variables
   integer :: i

   status = 0

   ! Set all required state vector elements
   SPixel%Sx = 0.
   do i = 1, SPixel%Nx
      call Set_State(SPixel%X(i), Ctrl, SPixel, MSI_Data, status)
   end do
   do i = 1, SPixel%NXJ
      call Set_State(SPixel%XJ(i), Ctrl, SPixel, MSI_Data, status)
   end do
   do i = 1, SPixel%NXI
      call Set_State(SPixel%XI(i), Ctrl, SPixel, MSI_Data, status)
   end do

end subroutine Get_X


!-------------------------------------------------------------------------------
! Name: Set_State
!
! Purpose:
! Initialise state variable i.
!
! Algorithm:
! 1) Call Get_State twice.
! 2) Check limits.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/08/20, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Set_State(i, Ctrl, SPixel, MSI_Data, status)

   use Ctrl_m
   use ORAC_Constants_m
   use Data_m

   implicit none

   ! Declare arguments
   integer,          intent(in)    :: i
   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status


   ! Set a priori
   call Get_State(SPixel%AP(i), i, Ctrl, SPixel, MSI_Data, 0, &
                  SPixel%Xb(i), status, SPixel%Sx)

   ! Set first guess
   if (SPixel%FG(i) /= SelmCtrl .and. SPixel%FG(i) == SPixel%AP(i)) then
      SPixel%X0(i) = SPixel%Xb(i)
   else
      call Get_State(SPixel%FG(i), i, Ctrl, SPixel, MSI_Data, 1, &
                     SPixel%X0(i), status)
   end if

   ! Check for internal consistency with the cloud class limits. Set the a
   ! priori or first guess values equal to any limit they exceed.
   if (SPixel%Xb(i) > Ctrl%Invpar%xUlim(i)) then
      SPixel%Xb(i) = Ctrl%Invpar%xUlim(i)
   else if (SPixel%Xb(i) < Ctrl%Invpar%xLlim(i)) then
      SPixel%Xb(i) = Ctrl%Invpar%xLlim(i)
   end if

   if (SPixel%X0(i) > Ctrl%Invpar%xUlim(i)) then
      SPixel%X0(i) = Ctrl%Invpar%xUlim(i)
   else if (SPixel%X0(i) < Ctrl%Invpar%xLlim(i)) then
      SPixel%X0(i) = Ctrl%Invpar%xLlim(i)
   end if

end subroutine Set_State


!-------------------------------------------------------------------------------
! Name: Get_State
!
! Purpose:
! Determines the appropriate method by which to initialise state variable i.
!
! Algorithm:
! 1) Switch dependant on value of mode.
! 2) If that fails, use the Ctrl method.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/07/30, AP: Original version
! 2016/01/12, AP: Make treatment of solar_factor consistent with GetSurface.
! 2016/09/07, AP: Output covariance for all variables.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Get_State(mode, i, Ctrl, SPixel, MSI_Data, flag, X, status, Err)

   use Ctrl_m
   use ORAC_Constants_m
   use Data_m

   implicit none

   ! Declare arguments
   integer,          intent(in)    :: mode
   integer,          intent(in)    :: i
   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(in)    :: flag
   real,             intent(out)   :: X
   integer,          intent(out)   :: status
   real,   optional, intent(out)   :: Err(:,:)

   ! Local variables
   integer :: is, ic, irho, js, jc, start, finsh
   real    :: Scale2, err_temp


   Scale2 = Ctrl%Invpar%XScale(i) * Ctrl%Invpar%XScale(i)

   select case (mode)
   case (SelmMeas) ! Draw state from measurement vector
      call X_MDAD(Ctrl, SPixel, i, X, status, err_temp)
      if (status == XMDADBounds) then
#ifdef DEBUG
         write(*,*) 'WARNING: X_MDAD(): Out-of-bounds interpolation'
#endif
         status = 0
      else if (status /= 0) then
#ifdef DEBUG
         write(*,*) 'ERROR: X_MDAD(): Failed with status:',status
#endif
      end if
      if (status == 0 .and. present(Err)) &
           Err(i,i) = err_temp * err_temp * Scale2

   case (SelmAux) ! Draw from auxilliary input information
      if (i == ITs) then ! Surface temperature
         ! Error setting could be much more sophisticated. The current scheme
         ! takes no account of relative proportions of land/sea in the
         ! current SPixel.
         X = SPixel%RTM%T(SPixel%RTM%Np)
         if (present(Err)) then
            if (SPixel%Surface%Land) then
               Err(i,i) = AUXErrTsLand * AUXErrTsLand * Scale2
            else
               Err(i,i) = AUXErrTsSea * AUXErrTsSea * Scale2
            end if
         end if

      else if (i == IPc) then ! Cloud-top pressure
         X = MSI_Data%State%CTP(SPixel%Loc%X0, SPixel%Loc%Y0)
         ! Check the prior CTP value to make sure it is non-null and physically reasonable
         if (X .gt. MinPriorCTP) then
            ! Set the prior CTP error from the value in the file.
            if (present(Err)) Err(i,i) = MSI_Data%State%CTP_var(SPixel%Loc%X0, SPixel%Loc%Y0)
         else
            ! If the prior CTP isn't reasonable, set status non-zero, which will cause Ctrl
            ! value to be used (see below).
            status = 1
         end if

      else if (any(i == ISP)) then
         do is = 1, Ctrl%Ind%NViews
            X = SPixel%Surface%Sw_p(is)
            if (present(Err)) Err(ISP(is), ISP(is)) = SPixel%Surface%Sw_p_var(is)
         end do

      else ! Surface reflectance (BRDF only)
         search: do is = 1, SPixel%Ind%NSolar
            ic = SPixel%spixel_y_solar_to_ctrl_y_solar_index(is)

            if (Ctrl%Approach == AppAerSw) then
               if (i == ISs(ic)) then
                  X = SPixel%Surface%Sw_s(is)
                  if (present(Err)) &
                       Err(ISs(is), ISs(is)) = SPixel%Surface%Sw_s_var(is)
                  exit search
               end if
            else if (Ctrl%RS%use_full_brdf) then
               do irho = 1, MaxRho_XX
                  if (i == IRs(ic,irho)) then
                     X = SPixel%Surface%Rs2(is,irho)

                     ! Copy over (and scale) covariance matrix
                     if (present(Err)) then
                        if (Ctrl%RS%diagonal_SRs) then
                           start = is
                           finsh = is
                        else
                           start = 1
                           finsh = SPixel%Ind%NSolar
                        end if
                        do js = start, finsh
                           jc = SPixel%spixel_y_solar_to_ctrl_y_solar_index(js)
                           Err(IRs(jc,irho), IRs(ic,irho)) = &
                                SPixel%Surface%SRs2(js,is,irho)  * &
                                Ctrl%Invpar%XScale(IRs(ic,irho)) * &
                                Ctrl%Invpar%XScale(IRs(jc,irho))
                           Err(IRs(ic,irho), IRs(jc,irho)) = &
                                Err(IRs(jc,irho), IRs(ic,irho))
                        end do
                     end if
                     exit search
                  end if
               end do
            else if (i == IRs(ic,IRho_DD)) then
               X = SPixel%Surface%Rs(is)

               if (present(Err)) then
                  if (Ctrl%RS%diagonal_SRs) then
                     start = is
                     finsh = is
                  else
                     start = 1
                     finsh = SPixel%Ind%NSolar
                  end if
                  do js = start, finsh
                     jc = SPixel%spixel_y_solar_to_ctrl_y_solar_index(js)
                     Err(IRs(jc,IRho_DD), IRs(ic,IRho_DD)) = &
                          SPixel%Surface%SRs(js,is)  * &
                          Ctrl%Invpar%XScale(IRs(ic,IRho_DD)) * &
                          Ctrl%Invpar%XScale(IRs(jc,IRho_DD))
                     Err(IRs(ic,IRho_DD), IRs(jc,IRho_DD)) = &
                          Err(IRs(jc,IRho_DD), IRs(ic,IRho_DD))
                  end do
               end if
               exit search
            end if
         end do search
      end if
   case(SelmPrev)
      call X_SDAD(Ctrl, SPixel, i, X, status, err_temp)
      if (status == 0 .and. present(Err)) &
           Err(i,i) = err_temp * Scale2 ! SnSave is a variance
   end select

   ! Draw from Ctrl structure (e.g. driver file). Used if method other
   ! methods failed. Ctrl%Sx is squared after reading in. (SelmSAD of the
   ! aerosol code
   if (mode == SelmCtrl .or. status /= 0) then
      ! Use value from Ctrl structure
      if (flag /= 0) then
         X = Ctrl%X0(i)
      else
         X = Ctrl%Xb(i)
      end if
      ! If surface reflectance, correct for solar zenith angle
      if (Ctrl%Approach /= AppAerSw .and. Ctrl%RS%solar_factor) then
         do is = 1, SPixel%Ind%NSolar
            ic = SPixel%spixel_y_solar_to_ctrl_y_solar_index(is)

            if (any(i == IRs(ic,:))) X = X / &
                 SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(is)))
         end do
      end if

      if (present(Err)) then
         Err(i,i) = Ctrl%Sx(i) * Ctrl%Sx(i) * Scale2
      end if

      status = 0
   end if

end subroutine Get_State
