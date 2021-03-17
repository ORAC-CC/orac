!-------------------------------------------------------------------------------
! Name: rtm_pc.F90
!
! Purpose:
! Defines the RTM_Pc_m module. This structure holds radiance and long and
! short wave transmittance terms used in the forward model subroutines.
!
! History:
! 2001/02/02, KS: original version
! 2001/02/08, AS: All arrays made allocatable.
! 2001/02/16, AS: d_Tac_d_Pc, d_Tbc_d_Pc added to LW struct.
! 2001/02/20, AS: Renaming variables to remove excess underscores.
! 2001/03/07, AS: Added dTac_dPc, dTbc_dPc to the top-level struct. The LW and
!    SW values must be combined into these arrays for later use. Further
!    comments under Algorithm in AllocRTMPc.f90.
! 2001/05/11, AS: Added Tc parameter (temperature at Pc).
!    **************** ECV work starts here *************************************
! 2011/02/21, AS: Re-introducing changes made in late 2001/2002.
! 2002/12/13, CP: Added Hc parameter.
! 2002/12/23, AS: Added dHc_dPc parameter. Rate of change of geopotential ht
!    w.r.t pressure. Allows us to calculate an error on Hc for output.
! 2013/01/17, MJ: Adds dTc_dPc: rate of change of temperature w.r.t pressure.
!    Allows us to calculate an error on Tc for output.
! 2014/05/27, GM: Some cleanup.
! 2015/01/07, AP: Eliminate write to RTM_Pc%Tac, Tbc.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module RTM_Pc_m

   ! Some variables appear in both the LW and SW structures, and are duplicated
   ! in the top-level struct to give a set of values for all channels. Others
   ! appear in only the LW.

   ! Short-wave sub-structure
   type RTM_Pc_SW_t
      real, pointer     :: Tac(:)
      real, pointer     :: Tbc(:)
      real, pointer     :: dTac_dPc(:)
      real, pointer     :: dTbc_dPc(:)
   end type RTM_Pc_SW_t

   ! Long-wave sub-structure
   type RTM_Pc_LW_t
      real, pointer     :: Tac(:)          ! Channel transmittance from cloud level
                                           ! to space.
      real, pointer     :: Tbc(:)          ! Channel trans from surface to cloud
                                           ! level.
      real, pointer     :: B(:)            ! Planck radiance at brightness
                                           ! temperature Tb, channel dependent.
      real, pointer     :: Rac_up(:)       ! TOA radiance from atmos above cloud
      real, pointer     :: Rac_dwn(:)      ! Down radiance at cloud top from atmos
      real, pointer     :: Rbc_up(:)       ! Up radiance at cloud base.
      real, pointer     :: dTac_dPc(:)     ! Derivative wrt cloud pressure, Pc
      real, pointer     :: dTbc_dPc(:)     ! Derivative wrt cloud pressure, Pc
      real, pointer     :: dB_dPc(:)       ! Derivative wrt cloud pressure, Pc
      real, pointer     :: dRac_up_dPc(:)  ! Derivative wrt cloud pressure, Pc
      real, pointer     :: dRac_dwn_dPc(:) ! Derivative wrt cloud pressure, Pc
      real, pointer     :: dRbc_up_dPc(:)  ! Derivative wrt cloud pressure, Pc
   end type RTM_Pc_LW_t

   type RTM_Pc_t
      type(RTM_Pc_LW_t) :: LW              ! Long wave
      type(RTM_Pc_SW_t) :: SW              ! Short wave
      real              :: Tc              ! Temperature at Pc
      real              :: Hc              ! geopotentail height at Pc
      real              :: dHc_dPc         ! Rate of change of Hc w.r.t Pc
      real              :: dTc_dPc         ! Rate of change of Tc w.r.t Pc
   end type RTM_Pc_t

contains

#include "alloc_rtm_pc.F90"
#include "dealloc_rtm_pc.F90"

end module RTM_Pc_m
