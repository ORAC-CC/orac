!-------------------------------------------------------------------------------
! Name:
!    Alloc_SPixel
!
! Purpose:
!    Allocate sizes of the SPixel arrays prior to entering the PGM loop.
!    Also assign the number of pressure levels in the SPixel%RTM%LW and SW
!    structures, as these are the same as in the RTM structure and do not
!    change across the image.
!
! Arguments:
!    Name   Type         In/Out/Both Description
!    Ctrl   struct       In          Control structure
!    RTM    alloc struct In          RTM structure
!    SPixel alloc struct Both        SPixel structure
!    status int          Out         Error status
!
! Algorithm:
!    Allocate the quality control mask and cloud flag arrays using the spatial
!    resolution specified in Ctrl.
!    Allocate the LW RTM arrays in SPixel using the number of thermal channels
!    specified in Ctrl and the number of pressure levels from the RTM file.
!    Allocate the SPixel SW RTM arrays using the number of PURELY solar channels
!    specified in Ctrl (i.e. Ny - NThermal) and the no. of pressure levels from
!    the SW RTM file.
!    Additional arrays for overall transmittances and reflectances are allocated
!    using the number of solar (i.e. pure solar plus mixed solar/thermal)
!    channels from Ctrl.
!
! Local variables:
!    Name Type Description
!
! History:
!    31th Jan, 2001, Kevin M. Smith: Original version
!    16th Feb 2001, Andy Smith:
!       Changed SW allocations to use Ny-NThermal, not ThermalFirst-SolarFirst
!       (for consistency with other routines).
!    22nd Feb 2001, Andy Smith:
!       Tsf parameters removed from LW RTM structure.
!       Added allocation of new parameter Tbc in LW struct.
!     6th Mar 2001, Andy Smith:
!       Allocation of Tsf parameters changed. Tsf_o,v now appear in overall RTM
!       struct. New arrays Rerf_Clear, dRef_clear_dRs added to RTM struct.
!     7th Mar 2001, Andy Smith:
!       New LW value dB_dTs.
!    11th Apr 2001, Andy Smith:
!       Added solar constant f0
!    25th Jun 2001, Andy Smith:
!       Completed header comments.
!    24th Sept 2001, Andy Smith:
!       Added initial allocation of SPixel%Ym, Sy, X and XI. These quantities
!       are reallocated for each super-pixel (unlike the others here) but
!       require an initial allocation (otherwise the first deallocation fails).
!    22nd Oct 2001, Andy Smith:
!       Added initialisation of SPixel%Ind%Ny since this is output to the
!       diagnostic file for each SPixel and may be output un-initialised if the
!       first few SPixels are omitted from processing due to no cloud or errors.
!       Also NThermal and NSolar.
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-applying changes from late 2001/2002.
!    13th December (2001?) Caroline Poulsen added geopotential height
!    23rd Mar 2011, Andy Smith:
!       Removal of super-pixel averaging. No need to allocate mask and cloud
!       or surface flags to Ctrl%Resoln%Space. Assume 1 pixel processed at a
!       time so only 1 flag needed.
!    20th Apr 2011, Andy Smith:
!       Extension to handle multiple instrument views. The viewing geometry
!       becomes a set of arrays, e.g. 1 value of sat. zen angle per view. Now
!       allocated to number of views.
!    22nd Sept 2011 Caroline Poulsen
!       Remove sw%p as now the same as lw%p changed to this
!       allocate(SPixel%RTM%SW%P(RTM%LW%NP))
!    15th June 2012, C. Poulsen:
!       Changed illum definition
!     3rd Nov 2012, Someone:
!       Bug fix: changed way shortwave variables were allocated
!       Remove YmSav variable
!    16th Jan 2014, Greg McGarragh:
!       Added allocation of SPixel%spixel_y_to_ctrl_y_index.
!    27th May 2014, Greg McGarragh:
!       Some cleanup.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Alloc_SPixel(Ctrl, RTM, SPixel, status)

   use Ctrl_def
   use RTM_def

   implicit none

   ! Declare arguments

   type(Ctrl_t), intent(in)    :: Ctrl
   type(RTM_t), intent(in)     :: RTM
   type(SPixel_t), intent(out) :: SPixel
   integer, intent(inout)      :: status

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
   allocate(SPixel%RTM%LW%Bs         (Ctrl%Ind%NThermal))
   allocate(SPixel%RTM%LW%dB_dTs     (Ctrl%Ind%NThermal))
   allocate(SPixel%RTM%LW%Ems        (Ctrl%Ind%NThermal))
   allocate(SPixel%RTM%LW%T          (RTM%LW%NP))
   allocate(SPixel%RTM%LW%H          (RTM%LW%NP))
   allocate(SPixel%RTM%LW%P          (RTM%LW%NP))

   ! Assign number of pressure levels

   SPixel%RTM%LW%Np = RTM%LW%Np
   SPixel%RTM%SW%Np = RTM%SW%Np

   ! Overall RTM transmittances and reflectances

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

   ! Get_Surface arrays

   allocate(SPixel%Rs                (Ctrl%Ind%NSolar))
   allocate(SPixel%SRs               (Ctrl%Ind%NSolar,   Ctrl%Ind%NSolar))

   !  Solar constant

   allocate(SPixel%f0                (Ctrl%Ind%NSolar))

   ! Initialise super-pixel active and inactive state vectors, measurements and
   ! errors. Allocate and initialise here in case the first (few) SPixel(s)
   ! aren't processed and these arrays remain unallocated. Write_Diag will try
   ! to write them out for all SPixels. Get_SPixel (or subordinate functions)
   ! will reallocate them.

   Spixel%Ind%Ny = 0
   Spixel%Ind%NSolar = 0
   Spixel%Ind%NThermal = 0

   allocate(SPixel%Illum(Ctrl%Ind%NViews))

   allocate(Spixel%Ym(1))

   allocate(SPixel%Sy(1,1))

   allocate(SPixel%ViewIdx(1))
   SPixel%ViewIdx(1)=1

   SPixel%Nx = MaxStateVar
   allocate(Spixel%X(SPixel%Nx))
   Spixel%X  = 0
   allocate(Spixel%XI(SPixel%Nx))
   Spixel%XI = 0

   allocate(SPixel%spixel_y_to_ctrl_y_index(Ctrl%Ind%Ny))

end subroutine Alloc_SPixel
