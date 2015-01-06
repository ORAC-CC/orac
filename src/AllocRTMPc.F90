!-------------------------------------------------------------------------------
! Name:
!    Alloc_RTM_Pc
!
! Purpose:
!    Allocate sizes of the RTM_Pc arrays prior to entering the PGM loop.
!
! Arguments:
!    Name   Type         In/Out/Both Description
!    Ctrl   struct       In          Control structure
!    RTM_Pc alloc struct In          RTM_Pc structure
!    status int          Out         Error status
!
! Algorithm:
!    Allocates arrays in the LW sub-structure to size Ctrl%Ind%NThermal
!    Allocates arrays in the SW sub-structure to size set to hold no. of purely
!    solar channels (Ny - NThermal)
!    Allocates arrays in the main RTM_Pc structure to size Ctrl%Ind%NY
!
!    Note: The SW struct is populated by Interpol_Solar, which deals with only
!    the purely solar channels for FM. The LW struct is populated for all
!    channels with a thermal component (i.e. mixed channels included) by
!    Interpol_Thermal. When the struct members are used later on by FM_Solar,
!    all values with a solar component are required, i.e. the mixed channels
!    must be included. Hence FM_Solar requires the overall structs in order to
!    avoid referencing both the SW and LW. (Similarly, FM_Thermal requires all
!    values with a thermal component, but these are all stored in the LW
!    sub-structure.)
!
! Local variables:
!    Name Type Description
!
! History:
!     8th Feb 2001, Andy Smith: Original version
!    16th Feb 2001, Andy Smith:
!       SW array sizes changed to include only the purely solar channels, not
!       those with mixed solar and thermal components, i.e. size is Ctrl%Ind%Ny-
!       Ctrl%Ind%NThermal, not Ctrl%Ind%NSolar.
!       Also added d_Tac_d_Pc and d_Tbc_d_Pc to LW structure.
!    20th Feb 2001, Andy Smith:
!       Renaming variables to remove excess underscores.
!    26th Feb 2001, Andy Smith :
!      Added tests for NSolar > 0, NThermal > 0 before allocating SW and LW
!      arrays.
!     7th Mar 2001, Andy Smith:
!      Added dTac_dPc, dTbc_dPc to the top-level struct. The LW and SW values
!      must be combined into these arrays for later use.
!    2012/01/30, MJ: Set RTM_Pc%Hc,RTM_Pc%Tc to fill value.
!    2012/11/03, MJ: Changed allocation of SW arrays.
!    2013/01/17, MJ: Added dTc_dPc and dHc_dPc.
!    2014/05/27, GM: Some cleanup.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Alloc_RTM_Pc(Ctrl, RTM_Pc)

   use Ctrl_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)  :: Ctrl
   type(RTM_Pc_t), intent(out) :: RTM_Pc

   ! Allocate sizes of SW sub-structure arrays

   if (Ctrl%Ind%Ny-Ctrl%Ind%NThermal > 0) then
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

   allocate(RTM_Pc%Tac(Ctrl%Ind%NY))
   allocate(RTM_Pc%Tbc(Ctrl%Ind%NY))
   allocate(RTM_Pc%dTac_dPc(Ctrl%Ind%Ny))
   allocate(RTM_Pc%dTbc_dPc(Ctrl%Ind%Ny))

   RTM_Pc%Hc=sreal_fill_value
   RTM_Pc%Tc=sreal_fill_value

   RTM_Pc%dHc_dPc=sreal_fill_value
   RTM_Pc%dTc_dPc=sreal_fill_value

end subroutine Alloc_RTM_Pc
