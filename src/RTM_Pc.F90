!-------------------------------------------------------------------------------
! Name:
!    RTM_Pc
!
! Purpose:
!    Defines the RTM_Pc structure. This structure holds radiance and long and
!    short wave transmittance terms used in the forward model subroutines.
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!
! History:
!     2nd Feb 2001, Kevin M. Smith: original version
!     8th Feb 2001, Andy Smith:
!       All arrays made allocatable.
!    16th Feb 2001, Andy Smith:
!       d_Tac_d_Pc, d_Tbc_d_Pc added to LW struct.
!    20th Feb 2001, Andy Smith:
!       Renaming variables to remove excess underscores.
!     7th Mar 2001, Andy Smith:
!       Added dTac_dPc, dTbc_dPc to the top-level struct. The LW and SW values
!       must be combined into these arrays for later use. Further comments under
!       Algorithm in AllocRTMPc.f90.
!    11th May 2001, Andy Smith:
!       Added Tc parameter (temperature at Pc).
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    13th Dec 2002, Caroline Poulsen:
!       Added Hc parameter.
!    23rd Dec 2002, Andy Smith:
!       Added dHc_dPc parameter. Rate of change of geopotential ht w.r.t
!       pressure. Allows us to calculate an error on Hc for output.
!    2013/01/17, Matthias Jerg:
!       Adds dTc_dPc: rate of change of temperature
!       w.r.t pressure. Allows us to calculate an error on Tc for output.
!    27th May 2014, Greg McGarragh:
!       Some cleanup.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module RTM_Pc_def

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
      real, pointer     :: Tac(:)          ! Combined long and short wave transmittances
      real, pointer     :: Tbc(:)          ! Combined long and short wave transmittances
      real, pointer     :: dTac_dPc(:)     ! Gradient of Tac w.r.t cloud pressure
      real, pointer     :: dTbc_dPc(:)     ! Gradient of Tbc w.r.t cloud pressure
      real              :: Tc              ! Temperature at Pc
      real              :: Hc              ! geopotentail height at Pc
      real              :: dHc_dPc         ! Rate of change of Hc w.r.t Pc
      real              :: dTc_dPc         ! Rate of change of Hc w.r.t Pc
   end type RTM_Pc_t

contains

   include 'AllocRTMPc.F90'
   include 'DeallocRTMPc.F90'

end module RTM_Pc_def
