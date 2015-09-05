!-------------------------------------------------------------------------------
! Name: alloc_input_data.F90
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
!    alloc_input_data_secondary added in channels
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

subroutine alloc_input_data_primary_common(input_data,xdim1km,ydim1km,indexing)

   use common_constants
   use postproc_constants

   implicit none

   type(input_data_primary), intent(inout) :: input_data
   integer(kind=lint),       intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes), intent(in)    :: indexing

   allocate(input_data%cot(xdim1km,ydim1km))
   input_data%cot=sreal_fill_value
   allocate(input_data%cot_uncertainty(xdim1km,ydim1km))
   input_data%cot_uncertainty=sreal_fill_value

   allocate(input_data%ref(xdim1km,ydim1km))
   input_data%ref=sreal_fill_value
   allocate(input_data%ref_uncertainty(xdim1km,ydim1km))
   input_data%ref_uncertainty=sreal_fill_value

   allocate(input_data%ctp(xdim1km,ydim1km))
   input_data%ctp=sreal_fill_value
   allocate(input_data%ctp_uncertainty(xdim1km,ydim1km))

   input_data%ctp_uncertainty=sreal_fill_value
   allocate(input_data%cct(xdim1km,ydim1km))
   input_data%cct=sreal_fill_value
   allocate(input_data%cct_uncertainty(xdim1km,ydim1km))
   input_data%cct_uncertainty=sreal_fill_value

   allocate(input_data%cc_total(xdim1km,ydim1km))
   input_data%cc_total=sreal_fill_value
   allocate(input_data%cc_total_uncertainty(xdim1km,ydim1km))
   input_data%cc_total_uncertainty=sreal_fill_value

   allocate(input_data%stemp_uncertainty(xdim1km,ydim1km))
   input_data%stemp_uncertainty=sreal_fill_value
   allocate(input_data%stemp(xdim1km,ydim1km))
   input_data%stemp=sreal_fill_value

   allocate(input_data%cth(xdim1km,ydim1km))
   input_data%cth=sreal_fill_value
   allocate(input_data%cth_uncertainty(xdim1km,ydim1km))
   input_data%cth_uncertainty=sreal_fill_value

   allocate(input_data%cth_corrected(xdim1km,ydim1km))
   input_data%cth_corrected=sreal_fill_value
   allocate(input_data%cth_corrected_uncertainty(xdim1km,ydim1km))
   input_data%cth_corrected_uncertainty=sreal_fill_value

   allocate(input_data%ctt(xdim1km,ydim1km))
   input_data%ctt=sreal_fill_value
   allocate(input_data%ctt_uncertainty(xdim1km,ydim1km))
   input_data%ctt_uncertainty=sreal_fill_value

   allocate(input_data%cwp(xdim1km,ydim1km))
   input_data%cwp=sreal_fill_value
   allocate(input_data%cwp_uncertainty(xdim1km,ydim1km))
   input_data%cwp_uncertainty=sreal_fill_value

   allocate(input_data%cloud_albedo(xdim1km,ydim1km,indexing%NSolar))
   input_data%cloud_albedo=sreal_fill_value

   allocate(input_data%convergence(xdim1km,ydim1km))
   input_data%convergence=byte_fill_value

   allocate(input_data%niter(xdim1km,ydim1km))
   input_data%niter=byte_fill_value

   allocate(input_data%costja(xdim1km,ydim1km))
   input_data%costja=sreal_fill_value
   allocate(input_data%costjm(xdim1km,ydim1km))
   input_data%costjm=sreal_fill_value

   allocate(input_data%qcflag(xdim1km,ydim1km))
   input_data%qcflag=sint_fill_value

end subroutine alloc_input_data_primary_common


subroutine alloc_input_data_primary_all(input_data,xdim1km,ydim1km,indexing)

   use common_constants
   use postproc_constants

   implicit none

   type(input_data_primary), intent(inout) :: input_data
   integer(kind=lint),       intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes), intent(in)    :: indexing

   call alloc_input_data_primary_common(input_data,xdim1km,ydim1km,indexing)

   allocate(input_data%time(xdim1km,ydim1km))
   input_data%time=dreal_fill_value

   allocate(input_data%lat(xdim1km,ydim1km))
   input_data%lat=sreal_fill_value
   allocate(input_data%lon(xdim1km,ydim1km))
   input_data%lon=sreal_fill_value

   allocate(input_data%solar_zenith_view_no1(xdim1km,ydim1km))
   input_data%solar_zenith_view_no1=sreal_fill_value
   allocate(input_data%satellite_zenith_view_no1(xdim1km,ydim1km))
   input_data%satellite_zenith_view_no1=sreal_fill_value
   allocate(input_data%rel_azimuth_view_no1(xdim1km,ydim1km))
   input_data%rel_azimuth_view_no1=sreal_fill_value

   allocate(input_data%phase(xdim1km,ydim1km))
   input_data%phase=byte_fill_value

   allocate(input_data%lsflag(xdim1km,ydim1km))
   input_data%lsflag=byte_fill_value
   allocate(input_data%illum(xdim1km,ydim1km))
   input_data%illum=byte_fill_value

   allocate(input_data%cccot_pre(xdim1km,ydim1km))
   input_data%cccot_pre=sreal_fill_value
   allocate(input_data%cccot(xdim1km,ydim1km))
   input_data%cccot=sreal_fill_value

   allocate(input_data%cldtype(xdim1km,ydim1km))
   input_data%cldtype=byte_fill_value
   allocate(input_data%cldmask(xdim1km,ydim1km))
   input_data%cldmask=byte_fill_value
   allocate(input_data%lusflag(xdim1km,ydim1km))
   input_data%lusflag=byte_fill_value

   !allocate(input_data%dem(xdim1km,ydim1km))
   !input_data%dem=sint_fill_value

   allocate(input_data%nisemask(xdim1km,ydim1km))
   input_data%nisemask=byte_fill_value

end subroutine alloc_input_data_primary_all


subroutine alloc_input_data_primary_class(input_data,xdim1km,ydim1km,indexing)

   use common_constants
   use postproc_constants

   implicit none

   type(input_data_primary), intent(inout) :: input_data
   integer(kind=lint),       intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes), intent(in)    :: indexing

   call alloc_input_data_primary_common(input_data,xdim1km, ydim1km,indexing)

end subroutine alloc_input_data_primary_class


subroutine alloc_input_data_secondary_common(input_data,xdim1km,ydim1km,indexing)

   use common_constants
   use postproc_constants

   implicit none

   type(input_data_secondary), intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),   intent(in)    :: indexing

   allocate(input_data%cot_ap(xdim1km,ydim1km))
   input_data%cot_ap=sreal_fill_value
   allocate(input_data%cot_fg(xdim1km,ydim1km))
   input_data%cot_fg=sreal_fill_value

   allocate(input_data%ref_ap(xdim1km,ydim1km))
   input_data%ref_ap=sreal_fill_value
   allocate(input_data%ref_fg(xdim1km,ydim1km))
   input_data%ref_fg=sreal_fill_value

   allocate(input_data%ctp_ap(xdim1km,ydim1km))
   input_data%ctp_ap=sreal_fill_value
   allocate(input_data%ctp_fg(xdim1km,ydim1km))
   input_data%ctp_fg=sreal_fill_value

   allocate(input_data%stemp_fg(xdim1km,ydim1km))
   input_data%stemp_fg=sreal_fill_value
   allocate(input_data%stemp_ap(xdim1km,ydim1km))
   input_data%stemp_ap=sreal_fill_value

   allocate(input_data%y0(xdim1km,ydim1km,indexing%Ny))
   input_data%y0=sreal_fill_value

   allocate(input_data%residuals(xdim1km,ydim1km,indexing%Ny))
   input_data%residuals=sreal_fill_value

end subroutine alloc_input_data_secondary_common


subroutine alloc_input_data_secondary_all(input_data,xdim1km,ydim1km,indexing)

   use common_constants
   use postproc_constants

   implicit none

   type(input_data_secondary), intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),   intent(in)    :: indexing

   call alloc_input_data_secondary_common(input_data,xdim1km,ydim1km,indexing)

   allocate(input_data%albedo(xdim1km,ydim1km,indexing%NSolar))
   input_data%albedo=sreal_fill_value

   allocate(input_data%channels(xdim1km,ydim1km,indexing%Ny))
   input_data%channels=sreal_fill_value

end subroutine alloc_input_data_secondary_all


subroutine alloc_input_data_secondary_class(input_data,xdim1km,ydim1km,indexing)

   use common_constants
   use postproc_constants

   implicit none

   type(input_data_secondary), intent(inout) :: input_data
   integer(kind=lint),         intent(in)    :: xdim1km,ydim1km
   type(counts_and_indexes),   intent(in)    :: indexing

   call alloc_input_data_secondary_common(input_data,xdim1km,ydim1km,indexing)

end subroutine alloc_input_data_secondary_class
