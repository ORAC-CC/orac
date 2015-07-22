!-------------------------------------------------------------------------------
! Name: dealloc_spixel_scan_out_pp.F90
!
! Purpose:
! The file contains a collection of three subroutines which deallocates the
! array parts of the output variable types.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2011/12/19, Matthias Jerg: creates initial file.
! 05/01/2012, Caroline Poulsen: add in reflectances and brightness
! 06/01/2012, Caroline Poulsen: add in cwp
! 15/01/2012, Caroline Poulsen: added in chans definition temperature
! 06/03/2012, Caroline Poulsen: Modified to create post processed netcdf
!    files with best phase selected
! 07/03/2012, Martin Stengel: added missing stemp_ap
! 2012/03/18, Caroline Poulsen: modified to add cloud flag
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/10/24, OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!    (currently deactivated), and nisemask
! 2014/11/20, OS: added deallocation of pavolonis phase
! 2014/11/26, OS: added deallocation of cloud_albedo
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine dealloc_spixel_scan_out_pp(spixel_scan_out)

   use common_constants
   use scanline_structure
   use vartypes_pp

   implicit none

   type(spixel_scanline_primary_output), intent(inout) :: spixel_scan_out

   deallocate(spixel_scan_out%vid_sol_zen)
   deallocate(spixel_scan_out%vid_sat_zen)
   deallocate(spixel_scan_out%vid_rel_azi)
   deallocate(spixel_scan_out%vid_cloud_albedo)

   deallocate(spixel_scan_out%time)
   deallocate(spixel_scan_out%lat)
   deallocate(spixel_scan_out%lon)
   deallocate(spixel_scan_out%sol_zen)
   deallocate(spixel_scan_out%sat_zen)
   deallocate(spixel_scan_out%rel_azi)
   deallocate(spixel_scan_out%cot)
   deallocate(spixel_scan_out%cot_error)
   deallocate(spixel_scan_out%ref)
   deallocate(spixel_scan_out%ref_error)
   deallocate(spixel_scan_out%ctp)
   deallocate(spixel_scan_out%ctp_error)
   deallocate(spixel_scan_out%cct)
   deallocate(spixel_scan_out%cct_error)
   deallocate(spixel_scan_out%stemp)
   deallocate(spixel_scan_out%stemp_error)
   deallocate(spixel_scan_out%ctt)
   deallocate(spixel_scan_out%ctt_error)
   deallocate(spixel_scan_out%cth)
   deallocate(spixel_scan_out%cth_error)
   deallocate(spixel_scan_out%cth_corrected)
   deallocate(spixel_scan_out%cth_corrected_error)
   deallocate(spixel_scan_out%cwp)
   deallocate(spixel_scan_out%cwp_error)
   deallocate(spixel_scan_out%cloud_albedo)
   deallocate(spixel_scan_out%convergence)
   deallocate(spixel_scan_out%niter)
   deallocate(spixel_scan_out%phase)
   deallocate(spixel_scan_out%phase_pavolonis)
   deallocate(spixel_scan_out%costja)
   deallocate(spixel_scan_out%costjm)
   deallocate(spixel_scan_out%lsflag)
   deallocate(spixel_scan_out%qcflag)
   deallocate(spixel_scan_out%illum)
   deallocate(spixel_scan_out%cldtype)
   deallocate(spixel_scan_out%cldmask)
   deallocate(spixel_scan_out%cccot)
   deallocate(spixel_scan_out%cccot_pre)
   deallocate(spixel_scan_out%lusflag)
!  deallocate(spixel_scan_out%dem)
   deallocate(spixel_scan_out%nisemask)

end subroutine dealloc_spixel_scan_out_pp


subroutine dealloc_spixel_scan_out_sec_pp(spixel_scan_out)

   use common_constants
   use scanline_structure
   use vartypes_pp

   implicit none

   type(spixel_scanline_secondary_output), intent(inout) :: spixel_scan_out

   logical :: lcovar = .false.

   deallocate(spixel_scan_out%vid_albedo)
   deallocate(spixel_scan_out%vid_channels)
   deallocate(spixel_scan_out%vid_y0)
   deallocate(spixel_scan_out%vid_residuals)

   deallocate(spixel_scan_out%albedo_scale)
   deallocate(spixel_scan_out%albedo_offset)
   deallocate(spixel_scan_out%albedo_vmin)
   deallocate(spixel_scan_out%albedo_vmax)

   deallocate(spixel_scan_out%channels_scale)
   deallocate(spixel_scan_out%channels_offset)
   deallocate(spixel_scan_out%channels_vmin)
   deallocate(spixel_scan_out%channels_vmax)

   deallocate(spixel_scan_out%y0_scale)
   deallocate(spixel_scan_out%y0_offset)
   deallocate(spixel_scan_out%y0_vmin)
   deallocate(spixel_scan_out%y0_vmax)

   deallocate(spixel_scan_out%residuals_scale)
   deallocate(spixel_scan_out%residuals_offset)
   deallocate(spixel_scan_out%residuals_vmin)
   deallocate(spixel_scan_out%residuals_vmax)

   deallocate(spixel_scan_out%scanline_u)
   deallocate(spixel_scan_out%scanline_v)

   deallocate(spixel_scan_out%cot_ap)
   deallocate(spixel_scan_out%cot_fg)
   deallocate(spixel_scan_out%ref_ap)
   deallocate(spixel_scan_out%ref_fg)
   deallocate(spixel_scan_out%ctp_ap)
   deallocate(spixel_scan_out%ctp_fg)
   deallocate(spixel_scan_out%stemp_ap)
   deallocate(spixel_scan_out%stemp_fg)

   deallocate(spixel_scan_out%albedo)
   deallocate(spixel_scan_out%channels)
   deallocate(spixel_scan_out%y0)
   deallocate(spixel_scan_out%residuals)

   deallocate(spixel_scan_out%ds)

   if (lcovar) then
      deallocate(spixel_scan_out%vid_covariance)
      deallocate(spixel_scan_out%covariance)
   end if

end subroutine dealloc_spixel_scan_out_sec_pp
