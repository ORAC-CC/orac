!-------------------------------------------------------------------------------
! Name: dealloc_output_data.F90
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
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: creates initial file.
! 2012/01/05, Caroline Poulsen: added in channel information
! 2012/01/06, Caroline Poulsen: added in cwp
! 2012/01/15, Caroline Poulsen: added in chan definitions
! 2012/01/28, Caroline Poulsen: added in albedo
! 2012/08/21, Matthias Jerg: adds time variable
! 2013/12/16, Greg McGarragh: Add deallocation of output_data%ctt_error,
!    output_data%cth_error, and output_data_sec%ds and a bit cleanup.
! 2014/05/27, Greg McGarragh: Some cleanup.
!
! $Id: dealloc_output_data.F90 2311 2014-08-15 17:06:07Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine dealloc_output_data_primary(output_data)

   use ECP_Constants

   implicit none

   type(output_data_primary), intent(inout) :: output_data

   deallocate(output_data%vid_sol_zen)
   deallocate(output_data%vid_sat_zen)
   deallocate(output_data%vid_rel_azi)

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
   deallocate(output_data%cwp)
   deallocate(output_data%cwp_error)

   deallocate(output_data%convergence)

   deallocate(output_data%niter)

   deallocate(output_data%phase)

   deallocate(output_data%costja)
   deallocate(output_data%costjm)

   deallocate(output_data%lsflag)

   deallocate(output_data%qcflag)

   deallocate(output_data%illum)

end subroutine dealloc_output_data_primary


subroutine dealloc_output_data_secondary(output_data,lcovar)

   use ECP_Constants

   implicit none

   logical :: lcovar

   type(output_data_secondary) :: output_data

   deallocate(output_data%vid_albedo)
   deallocate(output_data%vid_channels)
   deallocate(output_data%vid_y0)
   deallocate(output_data%vid_residuals)

   deallocate(output_data%albedo_vmin)
   deallocate(output_data%albedo_vmax)
   deallocate(output_data%albedo_scale)
   deallocate(output_data%albedo_offset)

   deallocate(output_data%channels_scale)
   deallocate(output_data%channels_offset)
   deallocate(output_data%channels_vmin)
   deallocate(output_data%channels_vmax)

   deallocate(output_data%y0_vmin)
   deallocate(output_data%y0_vmax)
   deallocate(output_data%y0_scale)
   deallocate(output_data%y0_offset)

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

end subroutine dealloc_output_data_secondary
