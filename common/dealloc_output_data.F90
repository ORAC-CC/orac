!-------------------------------------------------------------------------------
! Name: dealloc_output_data.F90
!
! Purpose:
! The file contains a collection of subroutines which deallocates the
! array parts of the output variable types, stored within module output_routines.
!
! History:
! 2011/12/19, MJ: Creates initial file.
! 2012/01/05, CP: Added in channel information
! 2012/01/06, CP: Added in cwp
! 2012/01/15, CP: Added in chan definitions
! 2012/01/28, CP: Added in albedo
! 2012/08/21, MJ: Adds time variable
! 2013/12/16, GM: Add deallocation of output_data%ctt_error,
!    output_data%cth_error, and output_data_sec%ds and a bit cleanup.
! 2014/05/27, GM: Some cleanup.
! 2014/10/24, OS: Added deallocation of cldtype, cldmask, cccot_pre,
!    lusflag, dem, nisemask
! 2014/12/01, CP: Added in cloud albedo
! 2015/07/01, CP: Added in corrected cth
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: dealloc_output_data_primary
!
! Purpose:
! Deallocate storage for primary output file.
!
! Description and Algorithm details:
! 1) Deallocate all arrays.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! output_data struct Both        Contents of secondard output file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine dealloc_output_data_primary(output_data, do_phase_pavolonis, &
   do_cldmask_uncertainty, do_dem)

   implicit none

   type(output_data_primary), intent(inout) :: output_data
   logical,                    intent(in)   :: do_phase_pavolonis
   logical,                    intent(in)   :: do_cldmask_uncertainty
   logical,                    intent(in)   :: do_dem

   deallocate(output_data%vid_sol_zen)
   deallocate(output_data%vid_sat_zen)
   deallocate(output_data%vid_rel_azi)

   deallocate(output_data%vid_cloud_albedo)
   deallocate(output_data%vid_cloud_albedo_error)

   deallocate(output_data%time)

   deallocate(output_data%lon)
   deallocate(output_data%lat)

   deallocate(output_data%sol_zen)
   deallocate(output_data%sat_zen)
   deallocate(output_data%rel_azi)

   deallocate(output_data%cot)
   deallocate(output_data%cot_error)
   deallocate(output_data%ref)
   deallocate(output_data%ref_error)
   deallocate(output_data%ctp)
   deallocate(output_data%ctp_error)
   deallocate(output_data%cct)
   deallocate(output_data%cct_error)
   deallocate(output_data%stemp)
   deallocate(output_data%stemp_error)

   deallocate(output_data%ctt)
   deallocate(output_data%ctt_error)
   deallocate(output_data%cth)
   deallocate(output_data%cth_error)
   deallocate(output_data%cth_corrected)
   deallocate(output_data%cth_corrected_error)
   deallocate(output_data%cwp)
   deallocate(output_data%cwp_error)

   deallocate(output_data%cloud_albedo)
   deallocate(output_data%cloud_albedo_error)

   deallocate(output_data%convergence)

   deallocate(output_data%niter)

   deallocate(output_data%phase)
if (do_phase_pavolonis) then
   deallocate(output_data%phase_pavolonis)
end if
   deallocate(output_data%costja)
   deallocate(output_data%costjm)

   deallocate(output_data%lsflag)

   deallocate(output_data%qcflag)

   deallocate(output_data%illum)

   deallocate(output_data%cldtype)
   deallocate(output_data%cldmask)
if (do_cldmask_uncertainty) then
   deallocate(output_data%cldmask_uncertainty)
end if
   deallocate(output_data%cccot_pre)
   deallocate(output_data%lusflag)
if (do_dem) then
   deallocate(output_data%dem)
end if
   deallocate(output_data%nisemask)

end subroutine dealloc_output_data_primary


!-------------------------------------------------------------------------------
! Name: dealloc_output_data_secondary
!
! Purpose:
! Deallocate storage for secondary output file.
!
! Description and Algorithm details:
! 1) Deallocate all arrays.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! output_data struct Both        Contents of secondard output file.
! lcovar      logic  In          Flag indicating presence of covariance matrices
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine dealloc_output_data_secondary(output_data, do_covariance)

   implicit none

   logical,                     intent(in)    :: do_covariance

   type(output_data_secondary), intent(inout) :: output_data

   deallocate(output_data%vid_albedo)
   deallocate(output_data%vid_channels)
   deallocate(output_data%vid_y0)
   deallocate(output_data%vid_residuals)

   deallocate(output_data%albedo_scale)
   deallocate(output_data%albedo_offset)
   deallocate(output_data%albedo_vmin)
   deallocate(output_data%albedo_vmax)

   deallocate(output_data%channels_scale)
   deallocate(output_data%channels_offset)
   deallocate(output_data%channels_vmin)
   deallocate(output_data%channels_vmax)

   deallocate(output_data%y0_scale)
   deallocate(output_data%y0_offset)
   deallocate(output_data%y0_vmin)
   deallocate(output_data%y0_vmax)

   deallocate(output_data%residuals_scale)
   deallocate(output_data%residuals_offset)
   deallocate(output_data%residuals_vmin)
   deallocate(output_data%residuals_vmax)

   deallocate(output_data%scanline_u)
   deallocate(output_data%scanline_v)

   deallocate(output_data%cot_ap)
   deallocate(output_data%cot_fg)
   deallocate(output_data%ref_ap)
   deallocate(output_data%ref_fg)
   deallocate(output_data%ctp_ap)
   deallocate(output_data%ctp_fg)
   deallocate(output_data%stemp_ap)
   deallocate(output_data%stemp_fg)

   deallocate(output_data%albedo)
   deallocate(output_data%channels)
   deallocate(output_data%y0)
   deallocate(output_data%residuals)

   deallocate(output_data%ds)

   if (do_covariance) then
      deallocate(output_data%vid_covariance)
      deallocate(output_data%covariance)
   end if

end subroutine dealloc_output_data_secondary
