!-------------------------------------------------------------------------------
! Name:
!    Dealloc_SPixel
!
! Purpose:
!    Deallocates the SPixel arrays to ensure that memory is freed once it is no
!    longer required.
!
! Arguments:
!    Name   Type         In/Out/Both Description
!    Ctrl   struct       In          Control structure
!    SPixel alloc struct Both        SPixel structure
!    status int          Out         Error status
!
! Algorithm:
!    Deallocates each of the arrays that is allocated by Alloc_SPixel.
!    At present, no check is made on deallocation status. What action should be
!    taken if dealloc stat is non-zero? Checking and reporting an error on ever
!    deallocation would make a very long routine.
!
! Local variables:
!    Name Type Description
!
! History:
!    22nd Oct 2001, Andy Smith: Original version
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-applying changes from late 2001/2002.
!    12th Dec 2011, Caroline Poulsen:
!       Added geopotential height
!    30th Mar 2011, Andy Smith:
!       Removal of super-pixelling, i.e. no averaging of flags etc needed.
!       Any super-pixelling required will now be done in pre-processing.
!       Resolution for the retrieval will be fixed at 1 pixel.
!       No need to deallocate cloud or surface flags and mask.
!    22nd Sep 2011, Caroline Poulsen:
!       Remove sw%p as now the same aslw%p
!    13th Dec 2011, Caroline Poulsen:
!       Deallocated SPixel%Geom arrays SPixel%SWRTM%P SPixel%ViewIdx
!    16th Jan 2014, Greg McGarragh:
!       Added deallocation of SPixel%spixel_y_to_ctrl_y_index.
!    27th May 2014, Greg McGarragh:
!       Some cleanup.
!     1st Aug 2014, Greg McGarragh:
!       Added more SPixel to Ctrl map indexes.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Dealloc_SPixel(Ctrl, SPixel, status)

   use Ctrl_def

   implicit none

   ! Declare arguments

   type(Ctrl_t), intent(in)      :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   integer, intent(inout)        :: status

   ! Declare local variables

   ! Quality control mask

!  deallocate(SPixel%Mask)

   ! Get_CloudFlags arrays

!  deallocate(SPixel%Cloud%Flags)

   ! Get_RTM arrays

   ! Short wave RTM parameters

   deallocate(SPixel%RTM%SW%Tac)
   deallocate(SPixel%RTM%SW%Tbc)
   deallocate(SPixel%RTM%SW%Tsf)
   deallocate(SPixel%RTM%SW%P)

   ! Long wave RTM parameters

   deallocate(SPixel%RTM%LW%Tac)
   deallocate(SPixel%RTM%LW%Tbc)
   deallocate(SPixel%RTM%LW%Tsf)
   deallocate(SPixel%RTM%LW%Rac_up)
   deallocate(SPixel%RTM%LW%Rac_dwn)
   deallocate(SPixel%RTM%LW%Rbc_up)
   deallocate(SPixel%RTM%LW%R_clear)
   deallocate(SPixel%RTM%LW%Bs)
   deallocate(SPixel%RTM%LW%dB_dTs)
   deallocate(SPixel%RTM%LW%Ems)
   deallocate(SPixel%RTM%LW%T)
   deallocate(SPixel%RTM%LW%P)
   deallocate(SPixel%RTM%LW%H)

   ! Overall RTM transmittances and reflectances

   deallocate(SPixel%RTM%Tsf_o)
   deallocate(SPixel%RTM%Tsf_v)
   deallocate(SPixel%RTM%Ref_clear)
   deallocate(SPixel%RTM%dRef_clear_dRs)

   ! Geometrical parameters

   deallocate(SPixel%Geom%SolZen)
   deallocate(SPixel%Geom%SatZen)
   deallocate(SPixel%Geom%RelAzi)
   deallocate(SPixel%Geom%SEC_o)
   deallocate(SPixel%Geom%SEC_v)

   ! Get_Surface arrays

   deallocate(SPixel%Rs)
   deallocate(SPixel%SRs)

   !  Solar constant

   deallocate(SPixel%f0)

   ! Super-pixel active and inactive state vectors, measurements and errors.

   deallocate(SPixel%illum)
   deallocate(SPixel%Ym)
   deallocate(SPixel%Sy)
   deallocate(SPixel%ViewIdx)
   deallocate(SPixel%X)
   deallocate(SPixel%XI)
   deallocate(SPixel%spixel_y_to_ctrl_y_index)
   deallocate(SPixel%spixel_y_solar_to_ctrl_y_index)
   deallocate(SPixel%spixel_y_thermal_to_ctrl_y_index)
   deallocate(SPixel%spixel_y_solar_to_ctrl_y_solar_index)
   deallocate(SPixel%spixel_y_thermal_to_ctrl_y_thermal_index)

end subroutine Dealloc_SPixel
