!-------------------------------------------------------------------------------
! Name: AllocSPixel.F90
!
! Purpose:
! Allocate sizes of the SPixel arrays prior to entering the PGM loop.
! Also assign the number of pressure levels in the SPixel%RTM%LW and SW
! structures, as these are the same as in the RTM structure and do not
! change across the image.
!
! Description and Algorithm details:
! 1) Allocate the quality control mask and cloud flag arrays using the spatial
!    resolution specified in Ctrl.
! 2) Allocate the LW RTM arrays in SPixel using the number of thermal channels
!    specified in Ctrl and the number of pressure levels from the RTM file.
! 3) Allocate the SPixel SW RTM arrays using the number of PURELY solar channels
!    specified in Ctrl (i.e. Ny - NThermal) and the no. of pressure levels from
!    the SW RTM file.
! 4) Additional arrays for overall transmittances and reflectances are allocated
!    using the number of solar (i.e. pure solar plus mixed solar/thermal)
!    channels from Ctrl.
!
! Arguments:
! Name   Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct In          Control structure
! RTM    struct In          RTM structure
! SPixel struct Both        SPixel structure
!
! History:
! 2001/01/31, KS: Original version
! 2001/02/16, AS: Changed SW allocations to use Ny-NThermal, not 
!    ThermalFirst-SolarFirst (for consistency with other routines).
! 2001/02/22, AS: Tsf parameters removed from LW RTM structure.
!    Added allocation of new parameter Tbc in LW struct.
! 2001/03/06, AS: Allocation of Tsf parameters changed. Tsf_o,v now appear in 
!    overall RTM struct. New arrays Rerf_Clear, dRef_clear_dRs added to RTM struct
! 2001/03/07, AS: New LW value dB_dTs.
! 2001/04/11, AS: Added solar constant f0
! 2001/06/25, AS: Completed header comments.
! 2001/09/24, AS: Added initial allocation of SPixel%Ym, Sy, X and XI. These 
!    quantities are reallocated for each super-pixel (unlike the others here) 
!    but require an initial allocation (otherwise the first deallocation fails).
! 2001/10/22, AS: Added initialisation of SPixel%Ind%Ny since this is output to 
!    the diagnostic file for each SPixel and may be output un-initialised if the 
!    first few SPixels are omitted from processing due to no cloud or errors. 
!    Also NThermal and NSolar.
!    **************** ECV work starts here *************************************
! 2011/02/21, AS: Re-applying changes from late 2001/2002.
! 2001/12/13, CP: added geopotential height
! 2011/03/23, AS: Removal of super-pixel averaging. No need to allocate mask and
!    cloud  or surface flags to Ctrl%Resoln%Space. Assume 1 pixel processed at a
!    time so only 1 flag needed.
! 2011/04/20, AS: Extension to handle multiple instrument views. The viewing 
!    geometry becomes a set of arrays, e.g. 1 value of sat. zen angle per view. 
!    Now allocated to number of views.
! 2011/09/22, CP: Remove sw%p as now the same as lw%p changed to this
!    allocate(SPixel%RTM%SW%P(RTM%LW%NP))
! 2012/06/15, CP: Changed illum definition
! 2012/11/03, CP: Bug fix: changed way shortwave variables were allocated.
!    Remove YmSav variable
! 2014/01/16, GM: Added allocation of SPixel%spixel_y_to_ctrl_y_index.
! 2014/05/27, GM: Some cleanup.
! 2014/08/01, GM: Added more SPixel to Ctrl map indexes.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2015/01/30, AP: Remove redundant fields.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Alloc_SPixel(Ctrl, RTM, SPixel)

   use Ctrl_def
   use ECP_Constants
   use RTM_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(RTM_t),    intent(in)    :: RTM
   type(SPixel_t), intent(out)   :: SPixel

   ! Declare local variables


   ! Allocate sizes of SPixel sub-structure arrays

   ! Quality control mask

!  allocate(SPixel%Mask(Ctrl%Resoln%Space, Ctrl%Resoln%Space))

   ! Get_CloudFlags arrays

!  allocate(SPixel%Cloud%Flags(Ctrl%Resoln%Space, Ctrl%Resoln%Space))

   ! Get_RTM arrays

   ! Short wave RTM parameters

   allocate(SPixel%RTM%SW%Tac        (Ctrl%Ind%NSolar, RTM%SW%NP))
   allocate(SPixel%RTM%SW%Tbc        (Ctrl%Ind%NSolar, RTM%SW%NP))
   allocate(SPixel%RTM%SW%Tsf        (Ctrl%Ind%NSolar))
   allocate(SPixel%RTM%SW%P          (RTM%LW%NP))

   ! Long wave RTM parameters

   allocate(SPixel%RTM%LW%Tac        (Ctrl%Ind%NThermal,RTM%LW%NP))
   allocate(SPixel%RTM%LW%Tbc        (Ctrl%Ind%NThermal,RTM%LW%NP))
   allocate(SPixel%RTM%LW%Tsf        (Ctrl%Ind%NThermal))
   allocate(SPixel%RTM%LW%Rac_up     (Ctrl%Ind%NThermal,RTM%LW%NP))
   allocate(SPixel%RTM%LW%Rac_dwn    (Ctrl%Ind%NThermal,RTM%LW%NP))
   allocate(SPixel%RTM%LW%Rbc_up     (Ctrl%Ind%NThermal,RTM%LW%NP))
   allocate(SPixel%RTM%LW%R_clear    (Ctrl%Ind%NThermal))
   allocate(SPixel%RTM%LW%dB_dTs     (Ctrl%Ind%NThermal))
   allocate(SPixel%RTM%LW%Ems        (Ctrl%Ind%NThermal))
   allocate(SPixel%RTM%LW%T          (RTM%LW%NP))
   allocate(SPixel%RTM%LW%H          (RTM%LW%NP))
   allocate(SPixel%RTM%LW%P          (RTM%LW%NP))

   ! Assign number of pressure levels

   SPixel%RTM%LW%Np = RTM%LW%Np
   SPixel%RTM%SW%Np = RTM%SW%Np

   ! Overall RTM transmittances and reflectances (Reallocated in GetSPixel)

   allocate(SPixel%RTM%Tsf_o         (Ctrl%Ind%NSolar))
   allocate(SPixel%RTM%Tsf_v         (Ctrl%Ind%NSolar))
   allocate(SPixel%RTM%Ref_clear     (Ctrl%Ind%NSolar))
   allocate(SPixel%RTM%dRef_clear_dRs(Ctrl%Ind%NSolar))

   ! Geometrical parameters

   allocate(SPixel%Geom%SolZen       (Ctrl%Ind%NViews))
   allocate(SPixel%Geom%SatZen       (Ctrl%Ind%NViews))
   allocate(SPixel%Geom%RelAzi       (Ctrl%Ind%NViews))
   allocate(SPixel%Geom%SEC_o        (Ctrl%Ind%NViews))
   allocate(SPixel%Geom%SEC_v        (Ctrl%Ind%NViews))

   ! Get_Surface arrays (Reallocated in GetSurface)

   allocate(SPixel%Rs                (Ctrl%Ind%NSolar))
   allocate(SPixel%SRs               (Ctrl%Ind%NSolar, Ctrl%Ind%NSolar))
   if (Ctrl%RS%use_full_brdf) then
      allocate(SPixel%Rs2            (Ctrl%Ind%NSolar, MaxRho_XX))
      allocate(SPixel%SRs2           (Ctrl%Ind%NSolar, Ctrl%Ind%NSolar, MaxRho_XX))
   end if

   !  Solar constant (Reallocated in GetSPixel)

   allocate(SPixel%f0                (Ctrl%Ind%NSolar))

   ! Initialise super-pixel active and inactive state vectors, measurements and
   ! errors. Allocate and initialise here in case the first (few) SPixel(s)
   ! aren't processed and these arrays remain unallocated.

   SPixel%Ind%Ny = 0
   SPixel%Ind%NSolar = 0
   SPixel%Ind%NThermal = 0
   SPixel%Ind%NMixed = 0

   allocate(SPixel%Illum(Ctrl%Ind%NViews))

   ! These reallocated in GetMeasurements
   allocate(SPixel%Ym(1))
   allocate(SPixel%Sy(1,1))
   allocate(SPixel%ViewIdx(1))
   SPixel%ViewIdx(1)=1

   ! These reallocated in GetIllum
   SPixel%Nx = 1
   allocate(SPixel%X(SPixel%Nx))
   SPixel%X  = 0
   SPixel%NxI = 1
   allocate(SPixel%XI(SPixel%NxI))
   SPixel%XI = 0
   allocate(SPixel%Ind%YSolar(Ctrl%Ind%NSolar))
   allocate(SPixel%Ind%YThermal(Ctrl%Ind%NThermal))
   allocate(SPixel%Ind%YMixed(Ctrl%Ind%NMixed))

   allocate(SPixel%spixel_y_to_ctrl_y_index(Ctrl%Ind%Ny))
   allocate(SPixel%spixel_y_solar_to_ctrl_y_index(Ctrl%Ind%Ny))
   allocate(SPixel%spixel_y_thermal_to_ctrl_y_index(Ctrl%Ind%Ny))
   allocate(SPixel%spixel_y_solar_to_ctrl_y_solar_index(Ctrl%Ind%Ny))
   allocate(SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(Ctrl%Ind%Ny))
   allocate(SPixel%spixel_y_mixed_to_spixel_y_solar(Ctrl%Ind%Ny))
   allocate(SPixel%spixel_y_mixed_to_spixel_y_thermal(Ctrl%Ind%Ny))

end subroutine Alloc_SPixel
