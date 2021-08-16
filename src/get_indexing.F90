!-------------------------------------------------------------------------------
! Name: get_indexing.F90
!
! Purpose:
! Determine which set of channels to use in retrieval and select a state vector.
!
! Description and Algorithm details:
! Find missing channels and channels not used due to illumination conditions.
!
! CLOUD:
! Each of the three categories: "tau" (optical thickness), "r_e" (effective
! radius), ir (cloud-top pressure, cloud fraction and surface temperature)
! have a set of channels that meet a channel requirement.  Find which of
! these channels are available in each category.  Note: This does not prevent
! other unrequired channels from being used.
!
! AEROSOL:
! Each solar channel is checked for other solar channels that have the same
! wavelength. If there are less than min_view available, the channels are
! ignored. Otherwise, the appropriate surface terms are added. For the Swansea
! model, each available view is checked to have at least min_wvl available or the
! channels are ignored.
!
! ALL:
! When a variable is desired, Add_to_State_Vector checks:
! 1) If it is listed in Ctrl%X (for this illumination condition), the variable
!    is added to the active state vector.
! 2) If it is listed in Ctrl%XJ, the variable is added to the variables that
!    aren't retrieved but are included in the Jacobian.
! 3) Otherwise, it is added to the inactive state vector so that it will be
!    included as a parameter error after the retrieval.
!
! Then, setup the rest of the SPixel indexes.
!
! The intention is for this algorithm to be written in a way that allows
! additional state variables to be added and additional categories.
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure
! SAD_Chan struct  In          Array of SAD channel structures
! SPixel   struct  Both        Super-pixel structure
! MSI_Data struct  In          Data structure. Contains the multi-spectral
!                              image measurements, location values, geometry
!                              etc for the current image segment, from which
!                              the current SPixel values will be extracted.
! status   integer Out         Error status
!
! History:
! 2015/02/04, GM: Original version.
! 2015/02/04, GM: Add support to use Ctrl%ReChans.  See description in Ctrl.F90
!    and default values set in ReadDriver.F90.
! 2015/03/11, GM: Remove check for missing r_e channels. It is valid not to have
!    any.
! 2015/07/30, AP: Move cloud indexing logic into its own routine.
! 2015/07/31, GM: Remove cloud legacy mode.
! 2015/08/17, AP: Add selection logics for aerosol retrievals.
! 2015/08/20, AP: Rather than set every state vector element, only sets those
!    with meaning in this retrieval. Added indexing logic for aerosol
!    retrievals. Replaced Find_MDAD with Find_Channel.
! 2016/01/27, GM: Add indexing for the new night cloud retrieval.
! 2016/07/27, GM: Add indexing for two layer cloud retieval.
! 2016/08/11, SP: Add logical flag for processing when using only 1 view from a
!                 multiangular sensor. Prevents post-processor problems.
! 2017/03/16, GT: Changes for single-view aerosol retrieval mode.
! 2017/07/05, AP: Add channels_used, variables_retrieved.
!
! Bugs:
! Assumes a single view for now.
!-------------------------------------------------------------------------------

subroutine Get_Indexing(Ctrl, SAD_Chan, SPixel, MSI_Data, status)

   use Ctrl_m
   use Data_m
   use ORAC_Constants_m
   use SAD_Chan_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i_view, i_chan
   logical :: is_not_used_or_missing(Ctrl%Ind%Ny)
   integer, dimension(MaxStateVar) :: X, XJ, XI


   status = 0

   ! Set these to zero as a default as some of the i/o requires them even when a
   ! retrieval is not performed.
   SPixel%Ind%Ny = 0
   SPixel%Ind%NSolar = 0
   SPixel%Ind%NThermal = 0
   SPixel%Ind%NMixed = 0

   SPixel%Nx  = 0
   SPixel%NXJ = 0
   SPixel%NXI = 0

   SPixel%channels_used = 0

   ! Initialise Approach and Class bits
   SPixel%variables_retrieved = (Ctrl%Approach-1) + 8*(Ctrl%Class-1)

   ! Force nighttime retrieval
   if (Ctrl%force_nighttime_retrieval) then
      SPixel%Illum = INight
   else
      ! If the views have different illumination conditions then skip this pixel
      do i_view = 1, Ctrl%Ind%NViews
         SPixel%Illum = MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%Y0, i_view)
         if ((i_view > 1) .and. (.not. Ctrl%all_channels_same_view)) then
            if (MSI_Data%Illum(SPixel%Loc%X0, SPixel%Loc%Y0, i_view-1) /= SPixel%Illum) then
               status = SPixelIndexing
               return
            end if
         end if
      end do
   end if

   ! Find which channels are either not used based on illumination conditions
   ! and/or are flagged as missing.
   is_not_used_or_missing = .false.

   if (SPixel%Illum .eq. IDay) then
      ! All channels are used
      do i_chan = 1, Ctrl%Ind%Ny
         if (MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, i_chan) == sreal_fill_value) then
            is_not_used_or_missing(i_chan) = .true.
         end if
      end do
   else if (SPixel%Illum .eq. ITwi)  then
      ! Only pure thermal channels (no mixed) are used
      do i_chan = 1, Ctrl%Ind%Ny
         if (btest(Ctrl%Ind%Ch_Is(i_chan), SolarBit) .or. &
             MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, i_chan) == sreal_fill_value) then
            is_not_used_or_missing(i_chan) = .true.
         end if
      end do
   else if (SPixel%Illum .eq. INight) then
      ! All thermal channels (including mixed) are used
      do i_chan = 1, Ctrl%Ind%Ny
         if (.not. btest(Ctrl%Ind%Ch_Is(i_chan), ThermalBit) .or. &
             MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, i_chan) == sreal_fill_value) then
            is_not_used_or_missing(i_chan) = .true.
         end if
      end do
   else
      status = SPixelIndexing
      return
   end if

   ! Select appropriate logic for channel selection
   select case (Ctrl%Approach)
   case (AppCld1L)
      if (.not. Ctrl%do_new_night_retrieval &
          .or. SPixel%Illum .eq. IDay .or. SPixel%Illum .eq. ITwi) then
         call cloud_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, &
                                   X, XJ, XI, status)
      else
         call cloud_indexing_logic_night(Ctrl, SPixel, is_not_used_or_missing, &
                                          X, XJ, XI, status)
      end if
   case (AppCld2L)
      call cloud_indexing_logic_two_layer(Ctrl, SPixel, is_not_used_or_missing, &
                                          X, XJ, XI, status)
   case (AppAerOx)
      call aer_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, &
                              X, XJ, XI, status)
   case (AppAerSw)
      call swan_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, &
                               X, XJ, XI, status)
   case (AppAerO1)
      call aer_indexing_logic_1view(Ctrl, SPixel, is_not_used_or_missing, &
                                    X, XJ, XI, status)
   end select

   if (status /= 0) then
      return
   end if
   ! Set up the active and inactive state variable indexes and associated counts
   deallocate(SPixel%X)
   allocate(SPixel%X(SPixel%Nx))
   SPixel%X = X(1:SPixel%Nx)

   deallocate(SPixel%XJ)
   if (SPixel%NXJ > 0) then
      allocate(SPixel%XJ(SPixel%NXJ))
      SPixel%XJ = XJ(1:SPixel%NXJ)
   else
      allocate(SPixel%XJ(1))
      SPixel%XJ = 0
   end if

   deallocate(SPixel%XI)
   if (SPixel%NXI > 0) then
      allocate(SPixel%XI(SPixel%NXI))
      SPixel%XI = XI(1:SPixel%NXI)
   else
      allocate(SPixel%XI(1))
      SPixel%XI = 0
   end if

   ! Set the illumination-dependent first guess and a priori
   SPixel%FG = Ctrl%FG(:,SPixel%Illum)
   SPixel%AP = Ctrl%AP(:,SPixel%Illum)

   ! Set up all the channel indexes
   call setup_indexes(Ctrl, SAD_Chan, SPixel, is_not_used_or_missing)

end subroutine Get_Indexing


!-------------------------------------------------------------------------------
! Name: setup_indexes
!
! Purpose:
! Determines channel indexing arrays, such as YSolar.
!
! Algorithm:
! Loop over channels, counting various features.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/02/04, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine setup_indexes(Ctrl, SAD_Chan, SPixel, is_not_used_or_missing)

   use Ctrl_m
   use SAD_Chan_m

   implicit none

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SPixel_t),   intent(inout) :: SPixel
   logical,          intent(in)    :: is_not_used_or_missing(:)

   integer :: i, j
   integer :: ii, i0, i1, i2, ii0, ii1

   SPixel%spixel_y_to_ctrl_y_index                 = 0
   SPixel%spixel_y_solar_to_ctrl_y_index           = 0
   SPixel%spixel_y_thermal_to_ctrl_y_index         = 0
   SPixel%spixel_y_solar_to_ctrl_y_solar_index     = 0
   SPixel%spixel_y_thermal_to_ctrl_y_thermal_index = 0
   SPixel%spixel_y_mixed_to_spixel_y_solar         = 0
   SPixel%spixel_y_mixed_to_spixel_y_thermal       = 0

   do i = 1, Ctrl%Ind%Ny
      if (.not. is_not_used_or_missing(i)) then
         SPixel%Ind%Ny = SPixel%Ind%Ny + 1

         if (btest(Ctrl%Ind%Ch_Is(i), SolarBit) .and. &
             SPixel%Illum .eq. IDay .and. &
             btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
            SPixel%Ind%NMixed   = SPixel%Ind%NMixed + 1
         end if

         if (btest(Ctrl%Ind%Ch_Is(i), SolarBit) .and. &
             SPixel%Illum .eq. IDay) then
            SPixel%Ind%NSolar   = SPixel%Ind%NSolar + 1
         end if

         if (btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
            SPixel%Ind%NThermal = SPixel%Ind%NThermal + 1
         end if
      end if
   end do

   if (SPixel%Ind%NSolar .gt. 0) then
      deallocate(SPixel%Ind%YSolar)
      allocate(SPixel%Ind%YSolar(SPixel%Ind%NSolar))
   end if
   if (SPixel%Ind%NThermal .gt. 0) then
      deallocate(SPixel%Ind%YThermal)
      allocate(SPixel%Ind%YThermal(SPixel%Ind%NThermal))
   end if
   if (SPixel%Ind%NMixed .gt. 0) then
      deallocate(SPixel%Ind%YMixed)
      allocate(SPixel%Ind%YMixed(SPixel%Ind%NMixed))
   end if

   ii  = 1
   i0  = 1
   i1  = 1
   i2  = 1
   ii0 = 0
   ii1 = 0
   do i = 1, Ctrl%Ind%Ny
      if (btest(Ctrl%Ind%Ch_Is(i), SolarBit)) then
         ii0 = ii0 + 1
      end if

      if (btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
         ii1 = ii1 + 1
      end if

      if (is_not_used_or_missing(i)) &
         cycle

      if (btest(Ctrl%Ind%Ch_Is(i), SolarBit) .and. &
          SPixel%Illum .eq. IDay .and. &
          btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
         ! Mixed channels out of those to be retrieved
         SPixel%Ind%YMixed(i2) = ii
         SPixel%spixel_y_mixed_to_spixel_y_solar(i2) = i0
         SPixel%spixel_y_mixed_to_spixel_y_thermal(i2) = i1
         i2 = i2 + 1
      end if

      if (btest(Ctrl%Ind%Ch_Is(i), SolarBit) .and. &
          SPixel%Illum .eq. IDay) then
         ! Solar channels out of those to be used
         SPixel%Ind%YSolar(i0) = ii
         SPixel%spixel_y_solar_to_ctrl_y_index(i0) = i
         SPixel%spixel_y_solar_to_ctrl_y_solar_index(i0) = ii0
         i0 = i0 + 1
      end if

      ! Thermal channels out of those to be used
      if (btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
         SPixel%Ind%YThermal(i1) = ii
         SPixel%spixel_y_thermal_to_ctrl_y_index(i1) = i
         SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(i1) = ii1
         i1 = i1 + 1
      end if

      ! Channels to be retrieved out of those in Ctrl%Ind%ICh
      SPixel%spixel_y_to_ctrl_y_index(ii) = i
      ii = ii + 1

      ! Flag use of channel (Bit 0 not used)
      SPixel%channels_used = ibset(SPixel%channels_used, Ctrl%Ind%Y_Id(i))
   end do


   ! Cross-reference the ss_terms flags
   if (associated(Ctrl%Ind%ss_terms)) then
      ii0 = 0
      ii1 = 0
      SPixel%spixel_y_solar_to_ss_terms = -1
      do i = 1, Ctrl%Ind%NSolar
         ii = Ctrl%Ind%YSolar(i)
         if (.not. is_not_used_or_missing(ii)) ii0 = ii0 + 1

         if (Ctrl%Ind%ss_terms(i)) then
            ii1 = ii1 + 1
            if (.not. is_not_used_or_missing(ii)) &
                 SPixel%spixel_y_solar_to_ss_terms(ii0) = ii1
         end if
      end do
   end if

   ! Cross-reference the rho_terms flags
   if (associated(Ctrl%Ind%rho_terms)) then
      ii0 = 0
      ii1 = 0
      SPixel%spixel_y_solar_to_rho_terms = -1
      do i = 1, Ctrl%Ind%NSolar
         ii = Ctrl%Ind%YSolar(i)
         if (.not. is_not_used_or_missing(ii)) ii0 = ii0 + 1

         do j = 1, MaxRho_XX
            if (Ctrl%Ind%rho_terms(i,j)) then
               ii1 = ii1 + 1
               if (.not. is_not_used_or_missing(ii)) &
                    SPixel%spixel_y_solar_to_rho_terms(ii0,j) = ii1
            end if
         end do
      end do
   end if

   ! Cross-reference the alb_terms flags
   if (associated(Ctrl%Ind%alb_terms)) then
      ii0 = 0
      ii1 = 0
      SPixel%spixel_y_solar_to_alb_terms = -1
      do i = 1, Ctrl%Ind%NSolar
         ii = Ctrl%Ind%YSolar(i)
         if (.not. is_not_used_or_missing(ii)) ii0 = ii0 + 1

         if (Ctrl%Ind%alb_terms(i)) then
            ii1 = ii1 + 1
            if (.not. is_not_used_or_missing(ii)) &
                 SPixel%spixel_y_solar_to_alb_terms(ii0) = ii1
         end if
      end do
   end if

   ! Cross-reference the cee_terms flags
   if (associated(Ctrl%Ind%cee_terms)) then
      ii0 = 0
      ii1 = 0
      SPixel%spixel_y_thermal_to_cee_terms = -1
      do i = 1, Ctrl%Ind%NThermal
         ii = Ctrl%Ind%YThermal(i)
         if (.not. is_not_used_or_missing(ii)) ii0 = ii0 + 1

         if (Ctrl%Ind%cee_terms(i)) then
            ii1 = ii1 + 1
            if (.not. is_not_used_or_missing(ii)) &
                 SPixel%spixel_y_thermal_to_cee_terms(ii0) = ii1
         end if
      end do
   end if


end subroutine setup_indexes


!-------------------------------------------------------------------------------
! Name: cloud_indexing_logic
!
! Purpose:
! Determines state vector elements required for a cloud retrieval.
!
! Algorithm:
! 1) Identify preferred (available) Re channel, invalidating all others.
! 2) Count number of channels available to retrieve Tau, Re, and IR values.
! 3) Add Tau, Re, Pc, Ts, and Fr to the state vector. These are active only if
!    sufficient channels are available. Appropriate BRDF terms are added but are
!    not active.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/07/30, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine cloud_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, &
                                X, XJ, XI, status)

   use Ctrl_m
   use Int_Routines_m, only : find_in_array
   use ORAC_Constants_m

   implicit none

   ! Define arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   logical,        intent(inout) :: is_not_used_or_missing(:)
   integer,        intent(out)   :: X(:)
   integer,        intent(out)   :: XJ(:)
   integer,        intent(out)   :: XI(:)
   integer,        intent(inout) :: status

   ! Define local variables

   integer :: ii_x, ii_xj, ii_xi
   integer :: min_rho
   integer :: i_chan, ii_chan, i_r_e_chan
   integer :: n_chans, n_tau_chans, n_r_e_chans, n_ir_chans, n_ir_chans2
   logical :: re_thermal

   integer, parameter :: min_tau_chans = 1
   integer, parameter :: min_r_e_chans = 1
   integer, parameter :: min_ir_chans  = 1
   integer, parameter :: min_x         = 1


   if (Ctrl%RTMIntSelm == RTMIntMethNone) then
      write(*,*) 'ERROR: Get_Indexing(): Cloud retrieval requires RTTOV inputs.'
      stop RTMIntflagErr
   end if

   ! If it is day time, Ctrl%ReChans is associated and any channels in
   ! Ctrl%ReChans are greater than zero and match any of the available r_e
   ! channels then pick the first one in Ctrl%ReChans that matches and set the
   ! rest of the Ctrl%ReChans that match to missing.  If Ctrl%ReChans is not
   ! associated it is assumed that all available r_e channels should be used.
   if (SPixel%Illum .eq. IDay .and. associated(Ctrl%ReChans)) then
      i_r_e_chan = 0
      do i_chan = 1, size(Ctrl%ReChans)
         ii_chan = find_in_array(Ctrl%r_e_chans, Ctrl%ReChans(i_chan))
         if (ii_chan .gt. 0) then
            ii_chan = find_in_array(Ctrl%Ind%Y_ID, Ctrl%ReChans(i_chan))
            if (ii_chan .gt. 0) then
               if (.not. is_not_used_or_missing(ii_chan)) then
                  i_r_e_chan = Ctrl%ReChans(i_chan)
                  exit
               end if
            end if
         end if
      end do

      if (i_r_e_chan .gt. 0) then
         do i_chan = 1, Ctrl%Ind%Ny
            if (any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%r_e_chans) .and. &
                    Ctrl%Ind%Y_ID(i_chan) .ne. i_r_e_chan) then
               is_not_used_or_missing(i_chan) = .true.
            end if
         end do
      end if
   end if

   ! Find the number of channels that fit the minimum requirement in each
   ! category.  The possible set of channels that fit the minimum requirement in
   ! each category are instrument specific and located in Ctrl and defined in
   ! ReadDriver.F90.
   n_chans     = 0
   n_tau_chans = 0
   n_r_e_chans = 0
   n_ir_chans  = 0
   re_thermal  = .false.
   do i_chan = 1, Ctrl%Ind%Ny
      if (.not. is_not_used_or_missing(i_chan)) then
         if (any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%tau_chans) .or. &
             any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%r_e_chans) .or. &
             any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%ir_chans))  &
            n_chans = n_chans + 1

         if (any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%tau_chans)) then
            n_tau_chans = n_tau_chans + 1
         end if
         if (any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%r_e_chans)) then
            n_r_e_chans = n_r_e_chans + 1
            ! Check if any thermal channels are used for Re
            if (btest(Ctrl%Ind%Ch_Is(i_chan), ThermalBit)) re_thermal = .true.
         end if
         if (any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%ir_chans))  then
            n_ir_chans  = n_ir_chans  + 1
         end if
      end if
   end do

   ! Select the active state variables
   ii_x  = 0
   ii_xj = 0
   ii_xi = 0
   n_ir_chans2 = n_ir_chans
   ! Retrieve Tau if sufficient appropriate channels are available
   call Add_to_State_Vector(Ctrl, SPixel%Illum, ITau, X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, &
        active = n_tau_chans .ge. min_tau_chans)
   ! Retrieve Re if sufficient appropriate channels are available (if thermal
   ! channels used, decrement the IR channel counter)
   if (re_thermal) then
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe, X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, &
           active = n_r_e_chans .ge. min_r_e_chans, ch_available = n_ir_chans2)
   else
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe, X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, &
           active = n_r_e_chans .ge. min_r_e_chans)
   end if
   ! Pc, Ts, Fr retreved for all illumination. n_ir_chans2 ensures there aren't
   ! more things retrieved than there are measurements
   call Add_to_State_Vector(Ctrl, SPixel%Illum, IPc, X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, &
        active = n_ir_chans2 .ge. min_ir_chans, ch_available = n_ir_chans2)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, ITs, X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, &
        active = n_ir_chans2 .ge. min_ir_chans, ch_available = n_ir_chans2)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, IFr, X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, &
        active = n_ir_chans2 .ge. min_ir_chans, ch_available = n_ir_chans2)

   ! Add BRDF terms to parameter vector
   if (Ctrl%RS%use_full_brdf) then
      min_rho = 1
   else
      min_rho = MaxRho_XX
   end if
   call Identify_BRDF_Terms(Ctrl, SPixel%Illum, 1, min_rho, &
        is_not_used_or_missing, X, ii_x, XJ, ii_xj, XI, ii_xi, &
        SPixel%variables_retrieved, .false.)

   ! If nothing is retrievable set the error status and return
   if (ii_x .lt. min_x) then
      status = SPixelIndexing
      return
   end if

   SPixel%Nx  = ii_x
   SPixel%NXJ = ii_xj
   SPixel%NXI = ii_xi

end subroutine cloud_indexing_logic


!-------------------------------------------------------------------------------
! Name: cloud_indexing_logic_new_night
!
! Purpose:
! Determines state vector elements required for a cloud retrieval at night.
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/12/31, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine cloud_indexing_logic_night(Ctrl, SPixel, is_not_used_or_missing, &
                                      X, XJ, XI, status)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Define arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   logical,        intent(inout) :: is_not_used_or_missing(:)
   integer,        intent(out)   :: X(:)
   integer,        intent(out)   :: XJ(:)
   integer,        intent(out)   :: XI(:)
   integer,        intent(inout) :: status

   ! Define local variables

   integer :: ii_x, ii_xj, ii_xi
   integer :: min_rho
   integer :: n_chans

   n_chans = count(.not. is_not_used_or_missing)

   if (n_chans .eq. 0) then
      status = SPixelIndexing
      return
   end if

   ! Select the active state variables
   ii_x  = 0
   ii_xj = 0
   ii_xi = 0

   if (n_chans .eq. 1) then
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITau, X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .false.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .false.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IPc,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITs,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .false.)
   else if (n_chans .eq. 2) then
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITau, X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .false.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .false.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IPc,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITs,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   else if (n_chans .eq. 3) then
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITau, X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IPc,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITs,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   else if (n_chans .ge. 4) then
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITau, X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, IPc,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
      call Add_to_State_Vector(Ctrl, SPixel%Illum, ITs,  X, ii_x, XJ, ii_xj, &
           XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   end if

   call Add_to_State_Vector(Ctrl, SPixel%Illum, IFr,  X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .false.)

   ! Add BRDF terms to parameter vector
   if (Ctrl%RS%use_full_brdf) then
      min_rho = 1
   else
      min_rho = MaxRho_XX
   end if
   call Identify_BRDF_Terms(Ctrl, SPixel%Illum, 1, min_rho, &
        is_not_used_or_missing, X, ii_x, XJ, ii_xj, XI, ii_xi, &
        SPixel%variables_retrieved, .false.)

   SPixel%Nx  = ii_x
   SPixel%NXJ = ii_xj
   SPixel%NXI = ii_xi

end subroutine cloud_indexing_logic_night


!-------------------------------------------------------------------------------
! Name: cloud_indexing_logic_two_layer
!
! Purpose:
! Determines state vector elements required for a cloud retrieval at night.
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/12/31, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine cloud_indexing_logic_two_layer(Ctrl, SPixel, is_not_used_or_missing, &
                                          X, XJ, XI, status)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   logical,        intent(inout) :: is_not_used_or_missing(:)
   integer,        intent(out)   :: X(:)
   integer,        intent(out)   :: XJ(:)
   integer,        intent(out)   :: XI(:)
   integer,        intent(inout) :: status

   ! Define local variables

   integer :: ii_x, ii_xj, ii_xi
   integer :: min_rho
   integer :: n_chans

   n_chans = count(.not. is_not_used_or_missing)

   if (n_chans .lt. Ctrl%Ind%Ny) then
      status = SPixelIndexing
      return
   end if

   ! Select the active state variables
   ii_x  = 0
   ii_xj = 0
   ii_xi = 0

   call Add_to_State_Vector(Ctrl, SPixel%Illum, ITau,  X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe,   X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, IPc,   X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, ITau2, X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, IRe2,  X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, IPc2,  X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .true.)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, ITs,   X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .true.)

   call Add_to_State_Vector(Ctrl, SPixel%Illum, IFr,   X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .false.)
   call Add_to_State_Vector(Ctrl, SPixel%Illum, IFr2,  X, ii_x, XJ, ii_xj, &
        XI, ii_xi, SPixel%variables_retrieved, active = .false.)

   ! Add BRDF terms to parameter vector
   if (Ctrl%RS%use_full_brdf) then
      min_rho = 1
   else
      min_rho = MaxRho_XX
   end if
   call Identify_BRDF_Terms(Ctrl, SPixel%Illum, 1, min_rho, &
        is_not_used_or_missing, X, ii_x, XJ, ii_xj, XI, ii_xi, &
        SPixel%variables_retrieved, .false.)

   SPixel%Nx  = ii_x
   SPixel%NXJ = ii_xj
   SPixel%NXI = ii_xi

end subroutine cloud_indexing_logic_two_layer


!-------------------------------------------------------------------------------
! Name: aer_indexing_logic
!
! Purpose:
! Determines state vector elements required for an Oxford surface retrieval.
!
! Algorithm:
! 1) Use same BRDF logic as the cloud retrieval.
! 2) If there are sufficient channels, add Tau and Re to the state vector.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/08/17, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine aer_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, &
                              X, XJ, XI, status)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   logical,          intent(inout) :: is_not_used_or_missing(:)
   integer,          intent(out)   :: X(:)
   integer,          intent(out)   :: XJ(:)
   integer,          intent(out)   :: XI(:)
   integer,          intent(inout) :: status

   ! Define local variables
   integer :: ii_x, ii_xj, ii_xi

   integer, parameter :: min_view = 2 ! AATSR chs at a minimum


   ! Daytime only
   if (SPixel%Illum /= IDay) then
      status = SPixelIndexing
      return
   end if

   ! Must have a least min_view valid observations to use a channel
   ii_x  = 0             ! Number of state vector elements set
   ii_xj = 0
   ii_xi = 0             ! Number of parameters set
   call Identify_BRDF_Terms(Ctrl, IDay, min_view, &
                            MaxRho_XX, & ! Only retrieve Rho_DD.
                            is_not_used_or_missing, &
                            X, ii_x, XJ, ii_xj, XI, ii_xi, &
                            SPixel%variables_retrieved, .true.)

   if (ii_x+2 > count(.not. is_not_used_or_missing)) then
      ! Insufficient wavelengths to perform retrieval
      status = SPixelIndexing
   else
      ! As we're OK for a retrieval, add elements to the state vector
      call Add_to_State_Vector(Ctrl, IDay, ITau, X, ii_x, XJ, ii_xj, XI, ii_xi, &
                               SPixel%variables_retrieved)
      call Add_to_State_Vector(Ctrl, IDay, IRe,  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                               SPixel%variables_retrieved)
      call Add_to_State_Vector(Ctrl, IDay, IFr,  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                               SPixel%variables_retrieved, active = .false.)

      SPixel%Nx  = ii_x
      SPixel%NXJ = ii_xj
      SPixel%NXI = ii_xi
   end if

end subroutine aer_indexing_logic


!-------------------------------------------------------------------------------
! Name: swan_indexing_logic
!
! Purpose:
! Determines state vector elements required for a Swansea surface retrieval.
!
! Algorithm:
! 1) Loop over solar channels that haven't been inspected already.
!    Count the number of subsequent, available channels that share that
!    wavelength. If there are fewer than min_view channels, invalidate them.
!    Otherwise, add the corresponding S terms to the state vector.
! 2) Loop over views. Count the number of subsequent, available channels that
!    share that view. If there are fewer than min_wvl, invalidate them.
!    Otherwise, add the corresponding P terms to the state vector.
! 3) If there are sufficient channels, add Tau and Re to the state vector.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/08/17, AP: Original version
!
! Bugs:
! The wvl and view checks are not actually independent, so the result could be
! different if their order is inverted.
!-------------------------------------------------------------------------------
subroutine swan_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, &
                              X, XJ, XI, status)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   logical,          intent(inout) :: is_not_used_or_missing(:)
   integer,          intent(out)   :: X(:)
   integer,          intent(out)   :: XJ(:)
   integer,          intent(out)   :: XI(:)
   integer,          intent(inout) :: status

   ! Define local variables
   integer :: ii_x, ii_xj, ii_xi
   integer :: i_view, i_solar, i_ctrl, j_solar, j_ctrl
   integer :: nch, ch(Ctrl%Ind%NSolar)
   logical :: checked(Ctrl%Ind%NSolar)

   integer, parameter :: min_view = 2 ! AATSR chs at a minimum
   integer, parameter :: min_wvn  = 2


   ! Daytime only
   if (SPixel%Illum /= IDay) then
      status = SPixelIndexing
      return
   end if

   ii_x  = 0             ! Number of state vector elements set
   ii_xj = 0
   ii_xi = 0             ! Number of parameters set

   checked = .false.

   ! Identify which valid solar channels are at this wavelength
   ! [Duplication of Identify_BRDF_Terms using ISS(:) rather than IRs(:,:)]
   do i_solar = 1, Ctrl%Ind%NSolar
      i_ctrl = Ctrl%Ind%YSolar(i_solar)

      ! Skip dead chs and wavelengths we've already dealt with
      if (checked(i_solar) .or. is_not_used_or_missing(i_ctrl)) cycle

      ! Make a list of channels that share this wavelength
      nch = 1
      ch(1) = i_solar
!     checked(i_solar) = .true.

      do j_solar = i_solar+1, Ctrl%Ind%NSolar
         j_ctrl = Ctrl%Ind%YSolar(j_solar)

         if (Ctrl%Ind%WvlIdx(i_ctrl) == Ctrl%Ind%WvlIdx(j_ctrl) .and. &
              .not. is_not_used_or_missing(i_ctrl)) then
            nch = nch+1
            ch(nch) = j_solar
            checked(j_solar) = .true.
         end if
      end do

      ! If too few valid views, ignore all views
      if (nch < min_view) then
         ! If insufficient views valid, invalidate all channels
         is_not_used_or_missing(Ctrl%Ind%YSolar(ch(1:nch))) = .true.
      else
         ! Add state vector elements for this wavenumber (NOTE: the first view
         ! found in the MSI file for this wavenumber is used)
         call Add_to_State_Vector(Ctrl, IDay, ISS(ch(1)), &
                                  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                                  SPixel%variables_retrieved)
      end if
   end do

   ! Sort through view dependant variables
   do i_view = 1, Ctrl%Ind%NViews
      nch = 0
      do i_solar = 1, Ctrl%Ind%NSolar
         i_ctrl = Ctrl%Ind%YSolar(i_solar)
         if (Ctrl%Ind%View_Id(i_ctrl) == i_view .and. &
              .not. is_not_used_or_missing(i_ctrl)) then
            nch = nch+1
            ch(nch) = i_solar
         end if
      end do

      if (nch < min_wvn) then
         is_not_used_or_missing(Ctrl%Ind%YSolar(ch(1:nch))) = .true.
      else
         call Add_to_State_Vector(Ctrl, IDay, ISP(i_view), &
                                  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                                  SPixel%variables_retrieved)
      end if
   end do

   if (ii_x+2 > count(.not. is_not_used_or_missing)) then ! Includes thermal
      ! Insufficient wavelengths to perform retrieval
      status = SPixelIndexing
   else
      ! As we're OK for a retrieval, add elements to the state vector
      call Add_to_State_Vector(Ctrl, IDay, ITau, X, ii_x, XJ, ii_xj, XI, ii_xi, &
                               SPixel%variables_retrieved)
      call Add_to_State_Vector(Ctrl, IDay, IRe,  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                               SPixel%variables_retrieved)
      call Add_to_State_Vector(Ctrl, IDay, IFr,  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                               SPixel%variables_retrieved, active = .false.)
      call Add_to_State_Vector(Ctrl, IDay, ISG,  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                               SPixel%variables_retrieved)

      SPixel%Nx  = ii_x
      SPixel%NXJ = ii_xj
      SPixel%NXI = ii_xi
   end if

end subroutine swan_indexing_logic

!-------------------------------------------------------------------------------
! Name: aer_indexing_logic_1view
!
! Purpose:
! Determines state vector elements required for a single view aerosol retrieval
!
! Algorithm:
! 1) Use same BRDF logic as the cloud retrieval.
! 2) If there are sufficient channels, add Tau and Re to the state vector.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2017/03/16, GT: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine aer_indexing_logic_1view(Ctrl, SPixel, is_not_used_or_missing, &
                                    X, XJ, XI, status)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SPixel_t),   intent(inout) :: SPixel
   logical,          intent(inout) :: is_not_used_or_missing(:)
   integer,          intent(out)   :: X(:)
   integer,          intent(out)   :: XJ(:)
   integer,          intent(out)   :: XI(:)
   integer,          intent(inout) :: status

   ! Define local variables
   integer :: ii_x, ii_xj, ii_xi

   integer, parameter :: min_view = 1 ! Only using a single view.

   ! Daytime only
   if (SPixel%Illum /= IDay) then
      status = SPixelIndexing
      return
   end if

   ! Must have a least min_view valid observations to use a channel
   ii_x  = 0             ! Number of state vector elements set
   ii_xj = 0
   ii_xi = 0             ! Number of parameters set
   call Identify_BRDF_Terms(Ctrl, IDay, min_view, &
                            MaxRho_XX, & ! Only retrieve Rho_DD.
                            is_not_used_or_missing, &
                            X, ii_x, XJ, ii_xj, XI, ii_xi, &
                            SPixel%variables_retrieved, .true.)

!   if (ii_x+2 > count(.not. is_not_used_or_missing)) then
!      ! Insufficient wavelengths to perform retrieval
!      status = SPixelIndexing
!   else
   ! As we're OK for a retrieval, add elements to the state vector
   call Add_to_State_Vector(Ctrl, IDay, ITau, X, ii_x, XJ, ii_xj, XI, ii_xi, &
                            SPixel%variables_retrieved)
   call Add_to_State_Vector(Ctrl, IDay, IRe,  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                            SPixel%variables_retrieved)
   call Add_to_State_Vector(Ctrl, IDay, IFr,  X, ii_x, XJ, ii_xj, XI, ii_xi, &
                            SPixel%variables_retrieved, active = .false.)

   SPixel%Nx  = ii_x
   SPixel%NXJ = ii_xj
   SPixel%NXI = ii_xi
 !  end if

end subroutine aer_indexing_logic_1view

!-------------------------------------------------------------------------------
! Name: Add_to_State_Vector
!
! Purpose:
! Include a term in either SPixel%X, XJ, or XI as dictated by the Ctrl structure.
!
! Algorithm:
! 1) If the index is in Ctrl%X and the active argument is either true or
!    not present, add the index to SPixel%X.
! 2) If not (1), then if the index is in Ctrl%XJ, add it to SPixel%XJ.
! 3) Otherwise, add it to SPixel%XI.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/08/17, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Add_to_State_Vector(Ctrl, illum, index, X, ii_x, XJ, ii_xj, &
                               XI, ii_xi, flag, active, ch_available)
   use Ctrl_m

   implicit none

   type(Ctrl_t),      intent(in)    :: Ctrl
   integer,           intent(in)    :: illum  ! Illumination condition to check
   integer,           intent(in)    :: index  ! State element to search for
   integer,           intent(inout) :: X(:)   ! Active state vector
   integer,           intent(inout) :: ii_x   ! Current position in X
   integer,           intent(inout) :: XJ(:)  ! Parameter vector
   integer,           intent(inout) :: ii_xj  ! Current position in XJ
   integer,           intent(inout) :: XI(:)  ! Inactive vector
   integer,           intent(inout) :: ii_xi  ! Current position in XI
   integer(dint),     intent(inout) :: flag
   logical, optional, intent(in)    :: active ! T: Add to X (default); F: To XI
   integer, optional, intent(inout) :: ch_available

   logical :: act
   integer :: index_tmp

   if (present(active)) then
      act = active
   else
      act = .true.
   end if

   if (act .and. any(Ctrl%X(1:Ctrl%Nx(illum),illum) == index)) then
      ! Retrieve this variable
      if (present(ch_available)) ch_available = ch_available-1
      ii_x = ii_x+1
      X(ii_x) = index

      ! Collapse the RhoXX dimension of IRs to keep the # of bits managable
      index_tmp = index
      do while (index_tmp > IRs(MaxNumSolar,1))
         index_tmp = index_tmp - MaxNumSolar
      end do
      ! Flag that we're retriving this variable (starting in bit 7)
      flag = ibset(flag, index_tmp + VarRetrievedBitOffset)

   else if (any(Ctrl%XJ(1:Ctrl%NXJ(illum),illum) == index)) then
      ! Include this variable in the Jacobian
      ii_xj = ii_xj+1
      XJ(ii_xj) = index

   else
      ! An inactive variable for which parameter error will be calculated
      ii_xi = ii_xi+1
      XI(ii_xi) = index
   end if

end subroutine Add_to_State_Vector


!-------------------------------------------------------------------------------
! Name: Identify_BRDF_Terms
!
! Purpose:
! Adds appropriate BRDF terms to the state vector.
!
! Algorithm:
! 1) Loop over solar channels that haven't been inspected already.
! 2) Count the number of subsequent, available channels that share that
!    wavelength. If there are fewer than min_view channels, invalidate them.
!    Otherwise, add the corresponding 1|4 BRDF terms to the state vector.
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/08/17, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Identify_BRDF_Terms(Ctrl, illum, min_view, min_rho, &
                               is_not_used_or_missing, &
                               X, ii_x, XJ, ii_xj, XI, ii_xi, flag, active)
   use Ctrl_m
   use ORAC_Constants_m, only : IRs, MaxRho_XX

   implicit none

   type(Ctrl_t),      intent(in)    :: Ctrl
   integer,           intent(in)    :: illum
   integer,           intent(in)    :: min_view
   integer,           intent(in)    :: min_rho
   logical,           intent(inout) :: is_not_used_or_missing(:)
   integer,           intent(inout) :: X(:)
   integer,           intent(inout) :: ii_x
   integer,           intent(inout) :: XJ(:)
   integer,           intent(inout) :: ii_xj
   integer,           intent(inout) :: XI(:)
   integer,           intent(inout) :: ii_xi
   integer(dint),     intent(inout) :: flag
   logical,           intent(in)    :: active

   integer :: i_solar, i_ctrl, j_solar, j_ctrl, j_rho
   integer :: nch, ch(Ctrl%Ind%NViews)
   logical :: checked(Ctrl%Ind%NSolar)


   checked = .false.

   ! Identify which valid solar channels are at this wavelength
   do i_solar = 1, Ctrl%Ind%NSolar
      i_ctrl = Ctrl%Ind%YSolar(i_solar)

      ! Skip dead chs and wavelengths we've already dealt with
      if (checked(i_solar) .or. is_not_used_or_missing(i_ctrl)) cycle

      ! Make a list of channels that share this wavelength
      nch = 1
      ch(1) = i_solar
!     checked(i_solar) = .true.

      do j_solar = i_solar+1, Ctrl%Ind%NSolar
         j_ctrl = Ctrl%Ind%YSolar(j_solar)

         if (Ctrl%Ind%WvlIdx(i_ctrl) == Ctrl%Ind%WvlIdx(j_ctrl) .and. &
              .not. is_not_used_or_missing(i_ctrl)) then
            nch = nch+1
            ch(nch) = j_solar
            checked(j_solar) = .true.
         end if
      end do

      ! If too few valid views, ignore all views
      if (nch < min_view) then
         ! If insufficient views valid, invalidate all channels
         is_not_used_or_missing(Ctrl%Ind%YSolar(ch(1:nch))) = .true.
      else
         ! Add state vector elements for this wavenumber (NOTE: the first view
         ! found in the MSI file for this wavenumber is used)
         do j_rho = min_rho, MaxRho_XX
            call Add_to_State_Vector(Ctrl, illum, IRs(ch(1),j_rho), &
                 X, ii_x, XJ, ii_xj, XI, ii_xi, flag, active=active)
         end do
      end if
   end do

end subroutine Identify_BRDF_Terms
