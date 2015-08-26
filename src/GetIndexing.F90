!-------------------------------------------------------------------------------
! Name: GetIndexing.F90
!
! Purpose:
! Determine which set of channels to use in retrieval.
!
! Description and Algorithm details:
! Find missing channels and channels not used due to illumination conditions.
!
! Each of the three categories: "tau" (optical thickness), "r_e" (effective
! radius), ir (cloud-top pressure, cloud fraction and surface temperature)
! have a set of channels that meet a channel requirement.  Find which of
! these channels are available in each category.  Note: This does not prevent
! other unrequired channels from being used.
!
! Loop through the required active state variables, either Ctrl%X_Dy,
! Ctrl%X_Tw or Ctrl%X_Ni, depending on illumination and if the
! channel requirement is met in its category, add it to the list of active
! state variables.
!
! Setup the rest of the SPixel indexes.
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
! 2015/02/04, GM: Add support to use CTRL%ReChans.  See description in Ctrl.F90
!    and default values set in ReadDriver.F90.
! 2015/03/11, GM: Remove check for missing r_e channels. It is valid not to
!    have any.
! 2015/07/30, AP: Move cloud indexing logic into its own routine.
! 2015/07/31, GM: Remove cloud legacy mode.
!
! $Id$
!
! Bugs:
! Assumes a single view for now.
!-------------------------------------------------------------------------------

subroutine Get_Indexing(Ctrl, SAD_Chan, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use Int_Routines_def, only : find_in_array
   use SAD_Chan_def

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i, ii, i_view, i_chan
   logical :: is_not_used_or_missing(Ctrl%Ind%Ny)
   integer :: X(MaxStateVar)

   ! Set status to zero
   status = 0

   ! Set these to zero as a default as some of the i/o requires them even when a
   ! retrieval is not performed.
   SPixel%Ind%Ny = 0
   SPixel%Ind%NSolar = 0
   SPixel%Ind%NThermal = 0
   SPixel%Ind%NMixed = 0

   SPixel%Nx = 0
   SPixel%NxI = 0

   ! If the views have different illumination conditions then skip this pixel
   do i_view = 1, Ctrl%Ind%NViews
      SPixel%illum(i_view) = MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%Y0, i_view)

      if (i_view > 1) then
         if (SPixel%illum(i_view - 1) /= SPixel%illum(i_view)) then
            status = SPixelIndexing
            return
         end if
      end if
   end do

   ! Find which channels are either not used based on illumination conditions
   ! and/or are flagged as missing.
   is_not_used_or_missing = .false.

   if (SPixel%Illum(1) .eq. IDay) then
      ! All channels are used
      do i_chan = 1, Ctrl%Ind%Ny
         if (MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, i_chan) == sreal_fill_value) then
            is_not_used_or_missing(i_chan) = .true.
         end if
      end do
   else if (SPixel%Illum(1) .eq. ITwi)  then
      ! Only pure thermal channels (no mixed) are used
      do i_chan = 1, Ctrl%Ind%Ny
         if (btest(Ctrl%Ind%Ch_Is(i_chan), SolarBit) .or. &
             MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%Y0, i_chan) == sreal_fill_value) then
            is_not_used_or_missing(i_chan) = .true.
         end if
      end do
   else if (SPixel%Illum(1) .eq. INight) then
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
   call cloud_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, X, status)
   if (status /= 0) return

   ! Set up the active and inactive state variable indexes and associated counts
   deallocate(SPixel%X)
   allocate(SPixel%X(SPixel%Nx))

   SPixel%NxI = MaxStateVar - SPixel%Nx
   deallocate(SPixel%XI)
   allocate(SPixel%XI(SPixel%NxI))

   SPixel%X = X(1:SPixel%Nx)

   ii = 0
   do i = 1, MaxStateVar
      if (.not. any(SPixel%X(1:SPixel%Nx) == i)) then
         ii = ii + 1
         SPixel%XI(ii) = i
      end if
   end do

   ! Set the illumination dependent first guess and a priori
   SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
   SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))

   ! Set up all the channel indexes
   call setup_indexes(Ctrl, SAD_Chan, SPixel, is_not_used_or_missing)

end subroutine Get_Indexing


subroutine setup_indexes(Ctrl, SAD_Chan, SPixel, is_not_used_or_missing)

   use CTRL_def
   use Int_Routines_def, only : find_in_array
   use SAD_Chan_def

   implicit none

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(SPixel_t),   intent(inout) :: SPixel
   logical,          intent(in)    :: is_not_used_or_missing(:)

   integer :: i
   integer :: ii, i0, i1, i2, ii0, ii1

   SPixel%Ind%Ny       = 0
   SPixel%Ind%NSolar   = 0
   SPixel%Ind%NThermal = 0
   SPixel%Ind%NMixed   = 0

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
             SPixel%illum(1) .eq. IDay .and. &
             btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
            SPixel%Ind%NMixed   = SPixel%Ind%NMixed + 1
         end if

         if (btest(Ctrl%Ind%Ch_Is(i), SolarBit) .and. &
             SPixel%illum(1) .eq. IDay) then
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
          SPixel%illum(1) .eq. IDay .and. &
          btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
         ! Mixed channels out of those to be retrieved
         SPixel%Ind%YMixed(i2) = ii
         SPixel%spixel_y_mixed_to_spixel_y_solar(i2) = i0
         SPixel%spixel_y_mixed_to_spixel_y_thermal(i2) = i1
         i2 = i2 + 1
      end if

      if (btest(Ctrl%Ind%Ch_Is(i), SolarBit) .and. &
          SPixel%illum(1) .eq. IDay) then
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
   end do

   ! Find channel closest to 0.67um (between 0.5 and 1.0um)
   SPixel%Ind%MDAD_SW = Find_Channel(14925., SPixel%Ind%Ny, SAD_Chan, &
        SPixel%spixel_y_to_ctrl_y_index, min=10000.0, max=20000.0)
   ! Find channel closest to 11.0um (greater than 4.0um)
   SPixel%Ind%MDAD_LW = Find_Channel(909.09, SPixel%Ind%Ny, SAD_Chan, &
        SPixel%spixel_y_to_ctrl_y_index, max=2500.0)

end subroutine setup_indexes


subroutine cloud_indexing_logic(Ctrl, SPixel, is_not_used_or_missing, X, status)

   use CTRL_def
   use ECP_Constants
   use Int_Routines_def, only : find_in_array

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   logical,        intent(inout) :: is_not_used_or_missing(:)
   integer,        intent(out)   :: X(:)
   integer,        intent(inout) :: status

   ! Define local variables

   integer :: i_chan, ii_chan, i_x, ii_x, i_r_e_chan
   integer :: n_chans, n_tau_chans, n_r_e_chans, n_ir_chans, &
              n_ir_chans2
   integer :: min_tau_chans, min_r_e_chans, min_ir_chans, min_x

   if (Ctrl%RTMIntSelm == RTMIntMethNone) then
      write(*,*) 'ERROR: Get_Indexing(): Cloud retrieval requires RTTOV inputs.'
      stop RTMIntFlagErr
   end if

   ! If it is day time, Ctrl%ReChans is associated and any channels in
   ! Ctrl%ReChans are greater than zero and match any of the available r_e
   ! channels then pick the first one in Ctrl%ReChans that matches and set the
   ! rest of the Ctrl%ReChans that match to missing.  If Ctrl%ReChans is not
   ! associated it is assumed that all available r_e channels should be used.
   if (SPixel%Illum(1) .eq. IDay .and. associated(Ctrl%ReChans)) then
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
         end if
         if (any(Ctrl%Ind%Y_ID(i_chan) .eq. Ctrl%ir_chans))  then
            n_ir_chans  = n_ir_chans  + 1
         end if
      end if
   end do

   ! Limits used in the active state variable selection loop
   min_tau_chans = 1
   min_r_e_chans = 1
   min_ir_chans  = 1

   ! Minimum number of active state variables to perform a retrieval
   min_x         = 1

   ! Select the active state variables
   ii_x = 0
   n_ir_chans2 = n_ir_chans
   if (SPixel%Illum(1) .eq. IDay) then
      do i_x = 1, Ctrl%Nx(IDay)
         if (Ctrl%X(i_x,IDay) .eq. ITau .and. n_tau_chans .ge. min_tau_chans) then
            ii_x = ii_x + 1
            X(ii_x) = ITau
         else if (Ctrl%X(i_x,IDay) .eq. IRe .and. n_r_e_chans .ge. min_r_e_chans) then
            ii_x = ii_x + 1
            X(ii_x) = IRe
         else if (Ctrl%X(i_x,IDay) .eq. IPc .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = IPc
         else if (Ctrl%X(i_x,IDay) .eq. ITs .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = ITs
         else if (Ctrl%X(i_x,IDay) .eq. IFr .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = IFr
         end if
      end do
   else if (SPixel%Illum(1) .eq. ITwi) then
      do i_x = 1, Ctrl%Nx(ITwi)
         if (Ctrl%X(i_x,ITwi) .eq. IPc .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = IPc
         else if (Ctrl%X(i_x,ITwi) .eq. ITs .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = ITs
         else if (Ctrl%X(i_x,ITwi) .eq. IFr .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = IFr
         end if
      end do
   else if (SPixel%Illum(1) .eq. INight) then
      do i_x = 1, Ctrl%Nx(INight)
         if (Ctrl%X(i_x,INight) .eq. IPc .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = IPc
         else if (Ctrl%X(i_x,INight) .eq. ITs .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = ITs
         else if (Ctrl%X(i_x,INight) .eq. IFr .and. n_ir_chans2 .ge. min_ir_chans) then
            ii_x = ii_x + 1
            n_ir_chans2 = n_ir_chans2 - 1
            X(ii_x) = IFr
         end if
      end do
   end if

   ! If nothing is retrievable set the error status and return
   if (ii_x .lt. min_x) then
      status = SPixelIndexing
      return
   end if

   SPixel%Nx = ii_x

end subroutine cloud_indexing_logic
