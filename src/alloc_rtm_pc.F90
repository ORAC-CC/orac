!-------------------------------------------------------------------------------
! Name: alloc_rtm_pc.F90
!
! Purpose:
! Allocate sizes of the RTM_Pc arrays prior to entering the PGM loop.
!
! Description and Algorithm details:
! 1) Allocates arrays in the LW sub-structure to size Ctrl%Ind%NThermal
! 2) Allocates arrays in the SW sub-structure to size set to hold no. of purely
!    solar channels (Ny - NThermal)
! 3) Allocates arrays in the main RTM_Pc structure to size Ctrl%Ind%NY
!
! Note: The SW struct is populated by Interpol_Solar, which deals with only
!    the purely solar channels for FM. The LW struct is populated for all
!    channels with a thermal component (i.e. mixed channels included) by
!    Interpol_Thermal. When the struct members are used later on by FM_Solar,
!    all values with a solar component are required, i.e. the mixed channels
!    must be included. Hence FM_Solar requires the overall structs in order to
!    avoid referencing both the SW and LW. (Similarly, FM_Thermal requires all
!    values with a thermal component, but these are all stored in the LW
!    sub-structure.)
!
! Arguments:
! Name   Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct In          Control structure
! RTM_Pc struct Out         RTM_Pc structure
!
! History:
! 2001/02/08, AS: Original version
! 2001/02/16, AS: SW array sizes changed to include only the purely solar
!    channels, not those with mixed solar and thermal components, i.e. size is
!    Ctrl%Ind%Ny-Ctrl%Ind%NThermal, not Ctrl%Ind%NSolar. Also added d_Tac_d_Pc
!    and d_Tbc_d_Pc to LW structure.
! 2001/02/20, AS: Renaming variables to remove excess underscores.
! 2001/02/26, AS: Added tests for NSolar > 0, NThermal > 0 before allocating SW
!    and LW arrays.
! 2001/03/07, AS: Added dTac_dPc, dTbc_dPc to the top-level struct. The LW and
!    SW values must be combined into these arrays for later use.
! 2012/01/30, MJ: Set RTM_Pc%Hc,RTM_Pc%Tc to fill value.
! 2012/11/03, MJ: Changed allocation of SW arrays.
! 2013/01/17, MJ: Added dTc_dPc and dHc_dPc.
! 2014/05/27, GM: Some cleanup.
! 2015/01/07, AP: Eliminate write to RTM_Pc%Tac, Tbc.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Alloc_RTM_Pc(Ctrl, RTM_Pc)

   use Ctrl_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)  :: Ctrl
   type(RTM_Pc_t), intent(out) :: RTM_Pc

   ! Allocate sizes of SW sub-structure arrays

   if (Ctrl%Ind%NSolar > 0) then
      allocate(RTM_Pc%SW%Tac(Ctrl%Ind%NSolar))
      allocate(RTM_Pc%SW%Tbc(Ctrl%Ind%NSolar))
      allocate(RTM_Pc%SW%dTac_dPc(Ctrl%Ind%NSolar))
      allocate(RTM_Pc%SW%dTbc_dPc(Ctrl%Ind%NSolar))
   end if

   ! Allocate sizes of LW sub-structure arrays

   if (Ctrl%Ind%NThermal > 0) then
      allocate(RTM_Pc%LW%Tac(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%Tbc(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%B(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%Rac_up(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%Rac_dwn(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%Rbc_up(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%dTac_dPc(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%dTbc_dPc(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%dB_dPc(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%dRac_up_dPc(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%dRac_dwn_dPc(Ctrl%Ind%NThermal))
      allocate(RTM_Pc%LW%dRbc_up_dPc(Ctrl%Ind%NThermal))
   end if

   ! Allocate sizes of the main structure arrays

   RTM_Pc%Hc=sreal_fill_value
   RTM_Pc%Tc=sreal_fill_value

   RTM_Pc%dHc_dPc=sreal_fill_value
   RTM_Pc%dTc_dPc=sreal_fill_value

end subroutine Alloc_RTM_Pc
