!-------------------------------------------------------------------------------
! Name: dealloc_spixel.F90
!
! Purpose:
! Deallocates the SPixel arrays to ensure that memory is freed once it is no
! longer required.
!
! Description and Algorithm details:
! 1) Deallocates each of the arrays that is allocated by Alloc_SPixel.
!
! Arguments:
! Name   Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl   struct       In          Control structure
! SPixel alloc struct Both        SPixel structure
!
! History:
! 2001/10/22, AS: Original version
!    **************** ECV work starts here *************************************
! 2011/02/21, AS: Re-applying changes from late 2001/2002.
! 2011/12/12, CP: Added geopotential height
! 2011/03/11, AS: Removal of super-pixelling, i.e. no averaging of flags etc
!    needed. Any super-pixelling required will now be done in pre-processing.
!    Resolution for the retrieval will be fixed at 1 pixel. No need to
!    deallocate cloud or surface flags and mask.
! 2011/09/22, CP: Remove sw%p as now the same aslw%p
! 2011/12/12, CP: Deallocated SPixel%Geom arrays SPixel%SWRTM%P SPixel%ViewIdx
! 2014/01/16, GM: Added deallocation of SPixel%spixel_y_to_ctrl_y_index.
! 2014/05/27, GM: Some cleanup.
! 2014/08/01, GM: Added more SPixel to Ctrl map indexes.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2015/01/12, AP: Added YSolar,YThermal.
! 2015/01/20, GM: Added deallocation of spixel_y_mixed_to_spixel_y_solar and
!    spixel_y_mixed_to_spixel_y_thermal.
! 2015/01/30, AP: Remove redundant fields.
! 2015/06/02, AP: Add aerosol fields.
! 2015/08/19, AP: Make reading of RTM terms optional.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Dealloc_SPixel(Ctrl, SPixel)

   use Ctrl_m

   implicit none

   ! Declare arguments

   type(Ctrl_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel

   ! Get_RTM arrays
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) then
      deallocate(SPixel%RTM%P)
      deallocate(SPixel%RTM%T)
      deallocate(SPixel%RTM%H)

      ! Short wave RTM parameters
      deallocate(SPixel%RTM%SW%Tac)
      deallocate(SPixel%RTM%SW%Tbc)
      deallocate(SPixel%RTM%SW%Tsf)

      ! Long wave RTM parameters
      deallocate(SPixel%RTM%LW%Tac)
      deallocate(SPixel%RTM%LW%Tbc)
      deallocate(SPixel%RTM%LW%Tsf)
      deallocate(SPixel%RTM%LW%Rac_up)
      deallocate(SPixel%RTM%LW%Rac_dwn)
      deallocate(SPixel%RTM%LW%Rbc_up)
      deallocate(SPixel%RTM%LW%R_clear)
      deallocate(SPixel%RTM%LW%dB_dTs)
      deallocate(SPixel%RTM%LW%Ems)

      ! Overall RTM transmittances and reflectances
      deallocate(SPixel%RTM%Tsf_o)
      deallocate(SPixel%RTM%Tsf_v)
   end if
   deallocate(SPixel%RTM%Ref_clear)
   deallocate(SPixel%RTM%dRef_clear_dRs)

   ! Geometrical parameters

   deallocate(SPixel%Geom%SolZen)
   deallocate(SPixel%Geom%SatZen)
   deallocate(SPixel%Geom%RelAzi)
   deallocate(SPixel%Geom%SatAzi)
   deallocate(SPixel%Geom%SEC_o)
   deallocate(SPixel%Geom%SEC_v)

   ! Get_Surface arrays

   if (Ctrl%Approach == AppAerSw) then
      deallocate(SPixel%Surface%Sw_s)
      deallocate(SPixel%Surface%Sw_s_var)
      if (Ctrl%RS%read_full_brdf) then
         deallocate(SPixel%Surface%Sw_p)
         deallocate(SPixel%Surface%Sw_p_var)
      end if
   else
      deallocate(SPixel%Surface%Rs)
      deallocate(SPixel%Surface%SRs)
      if (Ctrl%RS%use_full_brdf) then
         deallocate(SPixel%Surface%Rs2)
         deallocate(SPixel%Surface%SRs2)
      end if
      deallocate(SPixel%Surface%Ratios)
   end if
   deallocate(SPixel%Surface%XIndex)

   !  Solar constant

   deallocate(SPixel%f0)

   ! Super-pixel active and inactive state vectors, measurements and errors.

   deallocate(SPixel%Ym)
   deallocate(SPixel%Sy)
   deallocate(SPixel%ViewIdx)
   deallocate(SPixel%X)
   deallocate(SPixel%XJ)
   deallocate(SPixel%XI)
   deallocate(SPixel%Ind%YSolar)
   deallocate(SPixel%Ind%YThermal)
   deallocate(SPixel%Ind%YMixed)
   deallocate(SPixel%spixel_y_to_ctrl_y_index)
   deallocate(SPixel%spixel_y_solar_to_ctrl_y_index)
   deallocate(SPixel%spixel_y_thermal_to_ctrl_y_index)
   deallocate(SPixel%spixel_y_solar_to_ctrl_y_solar_index)
   deallocate(SPixel%spixel_y_thermal_to_ctrl_y_thermal_index)
   deallocate(SPixel%spixel_y_mixed_to_spixel_y_solar)
   deallocate(SPixel%spixel_y_mixed_to_spixel_y_thermal)
   deallocate(SPixel%spixel_y_solar_to_rho_terms)
   deallocate(SPixel%spixel_y_solar_to_ss_terms)
   deallocate(SPixel%spixel_y_solar_to_alb_terms)
   deallocate(SPixel%spixel_y_thermal_to_cee_terms)

end subroutine Dealloc_SPixel
