!-------------------------------------------------------------------------------
! Name: dealloc_input_data.F90
!
! Purpose:
! File contains several subroutines to allocate and initialize structures and
! user defined variable types.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/03, MJ: Cleans out prototype code to prepare repository upload.
! 15/02/2012, CP: To do level 2 post processing
! 07/03/2012, MS: Added missing stemp_ap
! 07/03/2012, CP: Cleaned up
! 2012/03/18, CP: Modified to add cloud flag
! 2012/06/20, CP: Added albedo
! 2012/07/04, MJ: Fixed several data type bugs
! 2012/07/06, MJ: Extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/09/20, change phase from 2 to 1, changed arguments of
!    set_input_data_secondary added in channels
! 2014/09/29, CP: Added in MODIS variable names
! 2014/10/24, OS: Added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!    (currently deactivated), and nisemask; commented out (de)allocation of
!    variables for water within if condition iphase = 2 (never true for water)
! 2014/11/20, OS: Some minor editing
! 2014/11/26, CP: Added cloud_albedo
! 2015/01/26, CP: Added multi layer cloud IR only
! 2015/07/16, GM: Major cleanup.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine dealloc_input_data_primary_common(input_data)

   use postproc_constants

   implicit none

   type(input_data_primary), intent(inout) :: input_data

   deallocate(input_data%cot)
   deallocate(input_data%cot_uncertainty)
   deallocate(input_data%cer)
   deallocate(input_data%cer_uncertainty)
   deallocate(input_data%ctp)
   deallocate(input_data%ctp_uncertainty)
   deallocate(input_data%cct)
   deallocate(input_data%cct_uncertainty)
   deallocate(input_data%cc_total)
   deallocate(input_data%cc_total_uncertainty)
   deallocate(input_data%stemp)
   deallocate(input_data%stemp_uncertainty)
   deallocate(input_data%cth)
   deallocate(input_data%cth_uncertainty)
   deallocate(input_data%cth_corrected)
   deallocate(input_data%cth_corrected_uncertainty)
   deallocate(input_data%ctt)
   deallocate(input_data%ctt_uncertainty)
   deallocate(input_data%cwp)
   deallocate(input_data%cwp_uncertainty)
   deallocate(input_data%cloud_albedo)
   deallocate(input_data%cloud_albedo_uncertainty)
   deallocate(input_data%convergence)
   deallocate(input_data%niter)
   deallocate(input_data%costja)
   deallocate(input_data%costjm)
   deallocate(input_data%qcflag)

end subroutine dealloc_input_data_primary_common


subroutine dealloc_input_data_primary_all(input_data)

   use postproc_constants

   implicit none

   type(input_data_primary), intent(inout) :: input_data

   call dealloc_input_data_primary_common(input_data)

   deallocate(input_data%time)
   deallocate(input_data%lat)
   deallocate(input_data%lon)
   deallocate(input_data%solar_zenith_view_no1)
   deallocate(input_data%satellite_zenith_view_no1)
   deallocate(input_data%rel_azimuth_view_no1)
   deallocate(input_data%lsflag)
   deallocate(input_data%lusflag)
   deallocate(input_data%dem)
   deallocate(input_data%nisemask)
   deallocate(input_data%illum)
   deallocate(input_data%cldtype)
   deallocate(input_data%cldmask)
   deallocate(input_data%cldmask_uncertainty)
   deallocate(input_data%cccot_pre)
   deallocate(input_data%phase)

end subroutine dealloc_input_data_primary_all


subroutine dealloc_input_data_primary_class(input_data)

   use postproc_constants

   implicit none

   type(input_data_primary), intent(inout) :: input_data

   call dealloc_input_data_primary_common(input_data)

end subroutine dealloc_input_data_primary_class


subroutine dealloc_input_data_common(input_data)

   use postproc_constants

   implicit none

   type(input_data_secondary), intent(inout) :: input_data

   deallocate(input_data%ctp_ap)
   deallocate(input_data%ctp_fg)
   deallocate(input_data%cer_ap)
   deallocate(input_data%cer_fg)
   deallocate(input_data%cot_ap)
   deallocate(input_data%cot_fg)
   deallocate(input_data%stemp_ap)
   deallocate(input_data%stemp_fg)

   deallocate(input_data%y0)

   deallocate(input_data%residuals)

   deallocate(input_data%ds)

end subroutine dealloc_input_data_common


subroutine dealloc_input_data_secondary_all(input_data)

   use postproc_constants

   implicit none

   type(input_data_secondary), intent(inout) :: input_data

   call dealloc_input_data_common(input_data)

!  deallocate(input_data%scanline_u)
!  deallocate(input_data%scanline_v)

   deallocate(input_data%albedo)

   deallocate(input_data%channels)

end subroutine dealloc_input_data_secondary_all


subroutine dealloc_input_data_secondary_class(input_data)

   use postproc_constants

   implicit none

   type(input_data_secondary), intent(inout) :: input_data

   call dealloc_input_data_common(input_data)

end subroutine dealloc_input_data_secondary_class
