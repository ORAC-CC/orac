!-------------------------------------------------------------------------------
! Name: set_struct_pp.F90
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
! 2012/02/03, MJ: cleans out prototype code to prepare repository upload.
! 15/02/2012, CP: to do level 2 post processing
! 07/03/2012, MS: added missing stemp_ap
! 07/03/2012, CP: cleaned up
! 2012/03/18, CP: modified to add cloud flag
! 2012/06/20, CP: added albedo
! 2012/07/04, MJ fixed several data type bugs
! 2012/07/06, MJ extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/09/20, change phase from 2 to 1, changed arguments of
!    set_l2_input_struct_2d_secondary added in channels
! 2014/09/29, CP: added in MODIS variable names
! 2014/10/24, OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!    (currently deactivated), and nisemask; commented out (de)allocation of
!    variables for water within if condition iphase = 2 (never true for water)
! 2014/11/20, OS: some minor editing
! 2014/11/26, CP: added cloud_albedo
! 2015/01/26, CP: added multi layer cloud IR only
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine set_l2_input_struct_2d_primary_common(l2_input_2d_primary, &
                                                 xdim1km,ydim1km,indexing)

   use common_constants
   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary
   integer(kind=lint),               intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),         intent(in)    :: indexing

   allocate(l2_input_2d_primary%cot(xdim1km,ydim1km))
   l2_input_2d_primary%cot=sreal_fill_value
   allocate(l2_input_2d_primary%cot_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%cot_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%ref(xdim1km,ydim1km))
   l2_input_2d_primary%ref=sreal_fill_value
   allocate(l2_input_2d_primary%ref_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%ref_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%ctp(xdim1km,ydim1km))
   l2_input_2d_primary%ctp=sreal_fill_value
   allocate(l2_input_2d_primary%ctp_uncertainty(xdim1km,ydim1km))

   l2_input_2d_primary%ctp_uncertainty=sreal_fill_value
   allocate(l2_input_2d_primary%cct(xdim1km,ydim1km))
   l2_input_2d_primary%cct=sreal_fill_value
   allocate(l2_input_2d_primary%cct_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%cct_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%cc_total(xdim1km,ydim1km))
   l2_input_2d_primary%cc_total=sreal_fill_value
   allocate(l2_input_2d_primary%cc_total_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%cc_total_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%stemp_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%stemp_uncertainty=sreal_fill_value
   allocate(l2_input_2d_primary%stemp(xdim1km,ydim1km))
   l2_input_2d_primary%stemp=sreal_fill_value

   allocate(l2_input_2d_primary%cth(xdim1km,ydim1km))
   l2_input_2d_primary%cth=sreal_fill_value
   allocate(l2_input_2d_primary%cth_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%cth_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%cth_corrected(xdim1km,ydim1km))
   l2_input_2d_primary%cth_corrected=sreal_fill_value
   allocate(l2_input_2d_primary%cth_corrected_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%cth_corrected_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%ctt(xdim1km,ydim1km))
   l2_input_2d_primary%ctt=sreal_fill_value
   allocate(l2_input_2d_primary%ctt_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%ctt_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%cwp(xdim1km,ydim1km))
   l2_input_2d_primary%cwp=sreal_fill_value
   allocate(l2_input_2d_primary%cwp_uncertainty(xdim1km,ydim1km))
   l2_input_2d_primary%cwp_uncertainty=sreal_fill_value

   allocate(l2_input_2d_primary%cloud_albedo(xdim1km,ydim1km,indexing%NSolar))
   l2_input_2d_primary%cloud_albedo=sreal_fill_value

   allocate(l2_input_2d_primary%convergence(xdim1km,ydim1km))
   l2_input_2d_primary%convergence=byte_fill_value

   allocate(l2_input_2d_primary%niter(xdim1km,ydim1km))
   l2_input_2d_primary%niter=byte_fill_value

   allocate(l2_input_2d_primary%costja(xdim1km,ydim1km))
   l2_input_2d_primary%costja=sreal_fill_value
   allocate(l2_input_2d_primary%costjm(xdim1km,ydim1km))
   l2_input_2d_primary%costjm=sreal_fill_value

   allocate(l2_input_2d_primary%qcflag(xdim1km,ydim1km))
   l2_input_2d_primary%qcflag=sint_fill_value

end subroutine set_l2_input_struct_2d_primary_common


subroutine set_l2_input_struct_2d_primary_all(l2_input_2d_primary, &
                                              xdim1km,ydim1km,indexing)

   use common_constants
   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary
   integer(kind=lint),               intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),         intent(in)    :: indexing

   call set_l2_input_struct_2d_primary_common(l2_input_2d_primary,xdim1km, &
      ydim1km,indexing)

   allocate(l2_input_2d_primary%time(xdim1km,ydim1km))
   l2_input_2d_primary%time=dreal_fill_value

   allocate(l2_input_2d_primary%lat(xdim1km,ydim1km))
   l2_input_2d_primary%lat=sreal_fill_value
   allocate(l2_input_2d_primary%lon(xdim1km,ydim1km))
   l2_input_2d_primary%lon=sreal_fill_value

   allocate(l2_input_2d_primary%solar_zenith_view_no1(xdim1km,ydim1km))
   l2_input_2d_primary%solar_zenith_view_no1=sreal_fill_value
   allocate(l2_input_2d_primary%satellite_zenith_view_no1(xdim1km,ydim1km))
   l2_input_2d_primary%satellite_zenith_view_no1=sreal_fill_value
   allocate(l2_input_2d_primary%rel_azimuth_view_no1(xdim1km,ydim1km))
   l2_input_2d_primary%rel_azimuth_view_no1=sreal_fill_value

   allocate(l2_input_2d_primary%phase(xdim1km,ydim1km))
   l2_input_2d_primary%phase=byte_fill_value
   allocate(l2_input_2d_primary%phase_post(xdim1km,ydim1km))
   l2_input_2d_primary%phase_post=byte_fill_value

   allocate(l2_input_2d_primary%lsflag(xdim1km,ydim1km))
   l2_input_2d_primary%lsflag=byte_fill_value
   allocate(l2_input_2d_primary%illum(xdim1km,ydim1km))
   l2_input_2d_primary%illum=byte_fill_value

   allocate(l2_input_2d_primary%cccot(xdim1km,ydim1km))
   l2_input_2d_primary%cccot=sreal_fill_value
   allocate(l2_input_2d_primary%cccot_pre(xdim1km,ydim1km))
   l2_input_2d_primary%cccot_pre=sreal_fill_value

   allocate(l2_input_2d_primary%cldtype(xdim1km,ydim1km))
   l2_input_2d_primary%cldtype=byte_fill_value
   allocate(l2_input_2d_primary%cldmask(xdim1km,ydim1km))
   l2_input_2d_primary%cldmask=byte_fill_value
   allocate(l2_input_2d_primary%lusflag(xdim1km,ydim1km))
   l2_input_2d_primary%lusflag=byte_fill_value

   !allocate(l2_input_2d_primary%dem(xdim1km,ydim1km))
   !l2_input_2d_primary%dem=sint_fill_value

   allocate(l2_input_2d_primary%nisemask(xdim1km,ydim1km))
   l2_input_2d_primary%nisemask=byte_fill_value

end subroutine set_l2_input_struct_2d_primary_all


subroutine set_l2_input_struct_2d_primary_class(l2_input_2d_primary, &
                                                xdim1km,ydim1km,indexing)

   use common_constants
   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary
   integer(kind=lint),               intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),         intent(in)    :: indexing

   call set_l2_input_struct_2d_primary_common(l2_input_2d_primary,xdim1km, &
      ydim1km,indexing)

end subroutine set_l2_input_struct_2d_primary_class


subroutine set_l2_input_struct_2d_secondary_common(l2_input_2d_secondary, &
                                                   xdim1km,ydim1km,indexing)

   use common_constants
   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary
   integer(kind=lint),                 intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),           intent(in)    :: indexing

   allocate(l2_input_2d_secondary%cot_ap(xdim1km,ydim1km))
   l2_input_2d_secondary%cot_ap=sreal_fill_value
   allocate(l2_input_2d_secondary%cot_fg(xdim1km,ydim1km))
   l2_input_2d_secondary%cot_fg=sreal_fill_value

   allocate(l2_input_2d_secondary%ref_ap(xdim1km,ydim1km))
   l2_input_2d_secondary%ref_ap=sreal_fill_value
   allocate(l2_input_2d_secondary%ref_fg(xdim1km,ydim1km))
   l2_input_2d_secondary%ref_fg=sreal_fill_value

   allocate(l2_input_2d_secondary%ctp_ap(xdim1km,ydim1km))
   l2_input_2d_secondary%ctp_ap=sreal_fill_value
   allocate(l2_input_2d_secondary%ctp_fg(xdim1km,ydim1km))
   l2_input_2d_secondary%ctp_fg=sreal_fill_value

   allocate(l2_input_2d_secondary%stemp_fg(xdim1km,ydim1km))
   l2_input_2d_secondary%stemp_fg=sreal_fill_value
   allocate(l2_input_2d_secondary%stemp_ap(xdim1km,ydim1km))
   l2_input_2d_secondary%stemp_ap=sreal_fill_value

   allocate(l2_input_2d_secondary%y0(xdim1km,ydim1km,indexing%Ny))
   l2_input_2d_secondary%y0=sreal_fill_value

   allocate(l2_input_2d_secondary%residuals(xdim1km,ydim1km,indexing%Ny))
   l2_input_2d_secondary%residuals=sreal_fill_value

end subroutine set_l2_input_struct_2d_secondary_common


subroutine set_l2_input_struct_2d_secondary_all(l2_input_2d_secondary, &
                                                xdim1km,ydim1km,indexing)

   use common_constants
   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary
   integer(kind=lint),                 intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),           intent(in)    :: indexing

   call set_l2_input_struct_2d_secondary_common(l2_input_2d_secondary, &
                                                xdim1km,ydim1km,indexing)

   allocate(l2_input_2d_secondary%albedo(xdim1km,ydim1km,indexing%NSolar))
   l2_input_2d_secondary%albedo=sreal_fill_value

   allocate(l2_input_2d_secondary%channels(xdim1km,ydim1km,indexing%Ny))
   l2_input_2d_secondary%channels=sreal_fill_value

end subroutine set_l2_input_struct_2d_secondary_all


subroutine set_l2_input_struct_2d_secondary_class(l2_input_2d_secondary, &
                                                  xdim1km,ydim1km,indexing)

   use common_constants
   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary
   integer(kind=lint),                 intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),           intent(in)    :: indexing

   call set_l2_input_struct_2d_secondary_common(l2_input_2d_secondary, &
                                                xdim1km,ydim1km,indexing)

end subroutine set_l2_input_struct_2d_secondary_class


subroutine unset_l2_input_struct_2d_primary_common(l2_input_2d_primary)

   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary

   deallocate(l2_input_2d_primary%cot)
   deallocate(l2_input_2d_primary%cot_uncertainty)
   deallocate(l2_input_2d_primary%ref)
   deallocate(l2_input_2d_primary%ref_uncertainty)
   deallocate(l2_input_2d_primary%ctp)
   deallocate(l2_input_2d_primary%ctp_uncertainty)
   deallocate(l2_input_2d_primary%cct)
   deallocate(l2_input_2d_primary%cct_uncertainty)
   deallocate(l2_input_2d_primary%cc_total)
   deallocate(l2_input_2d_primary%cc_total_uncertainty)
   deallocate(l2_input_2d_primary%stemp)
   deallocate(l2_input_2d_primary%stemp_uncertainty)
   deallocate(l2_input_2d_primary%cth)
   deallocate(l2_input_2d_primary%cth_uncertainty)
   deallocate(l2_input_2d_primary%cth_corrected)
   deallocate(l2_input_2d_primary%cth_corrected_uncertainty)
   deallocate(l2_input_2d_primary%ctt)
   deallocate(l2_input_2d_primary%ctt_uncertainty)
   deallocate(l2_input_2d_primary%cwp)
   deallocate(l2_input_2d_primary%cwp_uncertainty)
   deallocate(l2_input_2d_primary%cloud_albedo)
   deallocate(l2_input_2d_primary%convergence)
   deallocate(l2_input_2d_primary%niter)
   deallocate(l2_input_2d_primary%costja)
   deallocate(l2_input_2d_primary%costjm)
   deallocate(l2_input_2d_primary%qcflag)

end subroutine unset_l2_input_struct_2d_primary_common


subroutine unset_l2_input_struct_2d_primary_all(l2_input_2d_primary)

   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary

   call unset_l2_input_struct_2d_primary_common(l2_input_2d_primary)

   deallocate(l2_input_2d_primary%time)
   deallocate(l2_input_2d_primary%lat)
   deallocate(l2_input_2d_primary%lon)
   deallocate(l2_input_2d_primary%solar_zenith_view_no1)
   deallocate(l2_input_2d_primary%satellite_zenith_view_no1)
   deallocate(l2_input_2d_primary%rel_azimuth_view_no1)

   deallocate(l2_input_2d_primary%phase)
   deallocate(l2_input_2d_primary%phase_post)

   deallocate(l2_input_2d_primary%lsflag)
   deallocate(l2_input_2d_primary%cldtype)
   deallocate(l2_input_2d_primary%illum)

   deallocate(l2_input_2d_primary%cccot)
   deallocate(l2_input_2d_primary%cccot_pre)

   deallocate(l2_input_2d_primary%cldmask)
   deallocate(l2_input_2d_primary%lusflag)
!  deallocate(l2_input_2d_primary%dem)
   deallocate(l2_input_2d_primary%nisemask)

end subroutine unset_l2_input_struct_2d_primary_all


subroutine unset_l2_input_struct_2d_primary_class(l2_input_2d_primary)

   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary

   call unset_l2_input_struct_2d_primary_common(l2_input_2d_primary)

end subroutine unset_l2_input_struct_2d_primary_class


subroutine unset_l2_input_struct_2d_common(l2_input_2d_secondary)

   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary

   deallocate(l2_input_2d_secondary%ctp_ap)
   deallocate(l2_input_2d_secondary%ctp_fg)
   deallocate(l2_input_2d_secondary%ref_ap)
   deallocate(l2_input_2d_secondary%ref_fg)
   deallocate(l2_input_2d_secondary%cot_ap)
   deallocate(l2_input_2d_secondary%cot_fg)
   deallocate(l2_input_2d_secondary%stemp_ap)
   deallocate(l2_input_2d_secondary%stemp_fg)

   deallocate(l2_input_2d_secondary%y0)

   deallocate(l2_input_2d_secondary%residuals)

end subroutine unset_l2_input_struct_2d_common


subroutine unset_l2_input_struct_2d_secondary_all(l2_input_2d_secondary)

   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary

   call unset_l2_input_struct_2d_common(l2_input_2d_secondary)

!  deallocate(l2_input_2d_primary%scanline_u)
!  deallocate(l2_input_2d_primary%scanline_v)

   deallocate(l2_input_2d_secondary%albedo)

   deallocate(l2_input_2d_secondary%channels)

end subroutine unset_l2_input_struct_2d_secondary_all


subroutine unset_l2_input_struct_2d_secondary_class(l2_input_2d_secondary)

   use structures_pp
   use vartypes_pp

   implicit none

   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary

   call unset_l2_input_struct_2d_common(l2_input_2d_secondary)

end subroutine unset_l2_input_struct_2d_secondary_class
