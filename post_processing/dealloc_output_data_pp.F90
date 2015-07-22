!-------------------------------------------------------------------------------
! Name: dealloc_output_data_pp.F90
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

subroutine dealloc_output_data_primary_pp(output_data)

   use common_constants
   use output_routines
   use postproc_constants

   implicit none

   type(output_data_primary_pp), intent(inout) :: output_data

   deallocate(output_data%vid_sol_zen)
   deallocate(output_data%vid_sat_zen)
   deallocate(output_data%vid_rel_azi)
   deallocate(output_data%vid_cloud_albedo)

   deallocate(output_data%time)
   deallocate(output_data%lat)
   deallocate(output_data%lon)
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
   deallocate(output_data%convergence)
   deallocate(output_data%niter)
   deallocate(output_data%phase)
   deallocate(output_data%phase_pavolonis)
   deallocate(output_data%costja)
   deallocate(output_data%costjm)
   deallocate(output_data%lsflag)
   deallocate(output_data%qcflag)
   deallocate(output_data%illum)
   deallocate(output_data%cldtype)
   deallocate(output_data%cldmask)
   deallocate(output_data%cccot)
   deallocate(output_data%cccot_pre)
   deallocate(output_data%lusflag)
!  deallocate(output_data%dem)
   deallocate(output_data%nisemask)

end subroutine dealloc_output_data_primary_pp


subroutine dealloc_output_data_secondary_pp(output_data)

   use common_constants
   use output_routines
   use postproc_constants

   implicit none

   type(output_data_secondary_pp), intent(inout) :: output_data

   logical :: lcovar = .false.

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

   if (lcovar) then
      deallocate(output_data%vid_covariance)
      deallocate(output_data%covariance)
   end if

end subroutine dealloc_output_data_secondary_pp
