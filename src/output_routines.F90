!-------------------------------------------------------------------------------
! Name:
!
! Purpose:
!
! Description:
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!    10th Nov 2000, Greg McGarragh : Original version
!
! Bugs:
!    None known.
!
! $Id: output_routines.F90 1963 2014-02-03 11:38:08Z acpovey $
!
!---------------------------------------------------------------------

module output_routines

   use ECP_Constants

   implicit none

   type output_data_primary
      integer                       :: vid_time

      integer                       :: vid_lat
      integer                       :: vid_lon

      integer,dimension(:), pointer :: vid_sol_zen
      integer,dimension(:), pointer :: vid_sat_zen
      integer,dimension(:), pointer :: vid_rel_azi

      integer                       :: vid_cot
      integer                       :: vid_coterror

      integer                       :: vid_ref
      integer                       :: vid_referror

      integer                       :: vid_ctp
      integer                       :: vid_ctperror

      integer                       :: vid_cct
      integer                       :: vid_ccterror

      integer                       :: vid_stemp
      integer                       :: vid_stemperror

      integer                       :: vid_cth
      integer                       :: vid_ctherror

      integer                       :: vid_ctt
      integer                       :: vid_ctterror

      integer                       :: vid_cwp
      integer                       :: vid_cwperror

      integer                       :: vid_convergence

      integer                       :: vid_niter

      integer                       :: vid_phase

      integer                       :: vid_costja
      integer                       :: vid_costjm

      integer                       :: vid_lsflag

      integer                       :: vid_qcflag

      integer                       :: vid_illum


      real(kind=dreal)              :: time_scale,time_offset,time_vmin,time_vmax

      real(kind=sreal)              :: lat_scale,lat_offset,lat_vmin,lat_vmax
      real(kind=sreal)              :: lon_scale,lon_offset,lon_vmin,lon_vmax

      real(kind=sreal)              :: sol_scale,sol_offset,sol_vmin,sol_vmax
      real(kind=sreal)              :: sat_scale,sat_offset,sat_vmin,sat_vmax
      real(kind=sreal)              :: azi_scale,azi_offset,azi_vmin,azi_vmax

      real(kind=sreal)              :: cot_scale,cot_offset
      integer(kind=sint)            :: cot_vmin,cot_vmax
      real(kind=sreal)              :: cot_error_scale,cot_error_offset
      integer(kind=sint)            :: cot_error_vmin,cot_error_vmax

      real(kind=sreal)              :: ref_scale,ref_offset
      integer(kind=sint)            :: ref_vmin,ref_vmax
      real(kind=sreal)              :: ref_error_scale,ref_error_offset
      integer(kind=sint)            :: ref_error_vmin,ref_error_vmax

      real(kind=sreal)              :: ctp_scale,ctp_offset
      integer(kind=sint)            :: ctp_vmin,ctp_vmax
      real(kind=sreal)              :: ctp_error_scale,ctp_error_offset
      integer(kind=sint)            :: ctp_error_vmin,ctp_error_vmax

      real(kind=sreal)              :: cct_scale,cct_offset
      integer(kind=sint)            :: cct_vmin,cct_vmax
      real                          :: cct_error_scale,cct_error_offset
      integer(kind=sint)            :: cct_error_vmin,cct_error_vmax

!     real(kind=sreal)              :: albedo_scale,albedo_offset
!     integer(kind=sint)            :: albedo_vmin,albedo_vmax
!     real(kind=sreal)              :: albedo_error_scale,albedo_error_offset
!     integer(kind=sint)            :: albedo_error_vmin,albedo_error_vmax

      real(kind=sreal)              :: stemp_scale,stemp_offset
      integer(kind=sint)            :: stemp_vmin,stemp_vmax
      real(kind=sreal)              :: stemp_error_scale,stemp_error_offset
      integer(kind=sint)            :: stemp_error_vmin,stemp_error_vmax

      real(kind=sreal)              :: cth_scale,cth_offset
      integer(kind=sint)            :: cth_vmin,cth_vmax
      real(kind=sreal)              :: cth_error_scale,cth_error_offset
      integer(kind=sint)            :: cth_error_vmin,cth_error_vmax

      real(kind=sreal)              :: ctt_scale,ctt_offset
      integer(kind=sint)            :: ctt_vmin,ctt_vmax
      real                          :: ctt_error_scale,ctt_error_offset
      integer(kind=sint)            :: ctt_error_vmin,ctt_error_vmax

      real(kind=sreal)              :: cwp_scale,cwp_offset
      integer(kind=sint)            :: cwp_vmin,cwp_vmax
      real                          :: cwp_error_scale,cwp_error_offset
      integer(kind=sint)            :: cwp_error_vmin,cwp_error_vmax

      integer(kind=byte)            :: convergence_scale,convergence_offset
      integer(kind=byte)            :: convergence_vmin,convergence_vmax

      integer(kind=byte)            :: niter_scale,niter_offset
      integer(kind=byte)            :: niter_vmin,niter_vmax

      integer(kind=byte)            :: phase_scale,phase_offset
      integer(kind=byte)            :: phase_vmin,phase_vmax

      real(kind=sreal)              :: costja_scale,costja_offset
      real(kind=sreal)              :: costja_vmin,costja_vmax

      real(kind=sreal)              :: costjm_scale,costjm_offset
      real(kind=sreal)              :: costjm_vmin,costjm_vmax

      integer(kind=byte)            :: lsflag_scale,lsflag_offset
      integer(kind=byte)            :: lsflag_vmin,lsflag_vmax

      integer(kind=sint)            :: qcflag_scale,qcflag_offset
      integer(kind=sint)            :: qcflag_vmin,qcflag_vmax

      integer(kind=byte)            :: illum_scale,illum_offset
      integer(kind=byte)            :: illum_vmin,illum_vmax


      real(kind=dreal),   dimension(:,:),   pointer :: time

      real(kind=sreal),   dimension(:,:),   pointer :: lat
      real(kind=sreal),   dimension(:,:),   pointer :: lon

      real(kind=sreal),   dimension(:,:,:), pointer :: sol_zen
      real(kind=sreal),   dimension(:,:,:), pointer :: sat_zen
      real(kind=sreal),   dimension(:,:,:), pointer :: rel_azi

      integer(kind=sint), dimension(:,:),   pointer :: cot
      integer(kind=sint), dimension(:,:),   pointer :: cot_error

      integer(kind=sint), dimension(:,:),   pointer :: ref
      integer(kind=sint), dimension(:,:),   pointer :: ref_error

      integer(kind=sint), dimension(:,:),   pointer :: ctp
      integer(kind=sint), dimension(:,:),   pointer :: ctp_error

      integer(kind=sint), dimension(:,:),   pointer :: cct
      integer(kind=sint), dimension(:,:),   pointer :: cct_error

!     integer(kind=sint), dimension(:,:),   pointer :: albedo
!     integer(kind=sint), dimension(:,:),   pointer :: albedo_error

      integer(kind=sint), dimension(:,:),   pointer :: stemp
      integer(kind=sint), dimension(:,:),   pointer :: stemp_error

      integer(kind=sint), dimension(:,:),   pointer :: ctt
      integer(kind=sint), dimension(:,:),   pointer :: ctt_error

      integer(kind=sint), dimension(:,:),   pointer :: cth
      integer(kind=sint), dimension(:,:),   pointer :: cth_error

      integer(kind=sint), dimension(:,:),   pointer :: cwp
      integer(kind=sint), dimension(:,:),   pointer :: cwp_error

      integer(kind=byte), dimension(:,:),   pointer :: convergence

      integer(kind=byte), dimension(:,:),   pointer :: niter

      integer(kind=byte), dimension(:,:),   pointer :: phase

      real(kind=sreal),   dimension(:,:),   pointer :: costja

      real(kind=sreal),   dimension(:,:),   pointer :: costjm

      integer(kind=byte), dimension(:,:),   pointer :: lsflag

      integer(kind=sint), dimension(:,:),   pointer :: qcflag

      integer(kind=byte), dimension(:,:),   pointer :: illum

   end type output_data_primary


   type output_data_secondary
      integer                          :: vid_scanline_u
      integer                          :: vid_scanline_v

      integer                          :: vid_cot_ap,vid_cot_fg
      integer                          :: vid_ref_ap,vid_ref_fg
      integer                          :: vid_ctp_ap,vid_ctp_fg
      integer                          :: vid_stemp_ap,vid_stemp_fg

      integer, dimension(:),   pointer :: vid_albedo

      integer, dimension(:),   pointer :: vid_channels

      integer, dimension(:),   pointer :: vid_y0

      integer, dimension(:),   pointer :: vid_residuals

      integer                          :: vid_ds

      integer, dimension(:,:), pointer :: vid_covariance


      integer(kind=lint)               :: scanline_u_scale,scanline_u_offset
      integer(kind=lint)               :: scanline_u_vmin,scanline_u_vmax

      integer(kind=lint)               :: scanline_v_scale,scanline_v_offset
      integer(kind=lint)               :: scanline_v_vmin,scanline_v_vmax

      real(kind=sreal)                 :: cot_ap_scale,cot_ap_offset
      integer(kind=sint)               :: cot_ap_vmin,cot_ap_vmax

      real(kind=sreal)                 :: cot_fg_scale,cot_fg_offset
      integer(kind=sint)               :: cot_fg_vmin,cot_fg_vmax

      real(kind=sreal)                 :: ref_ap_scale,ref_ap_offset
      integer(kind=sint)               :: ref_ap_vmin,ref_ap_vmax

      real(kind=sreal)                 :: ref_fg_scale,ref_fg_offset
      integer(kind=sint)               :: ref_fg_vmin,ref_fg_vmax

      real(kind=sreal)                 :: ctp_ap_scale,ctp_ap_offset
      integer(kind=sint)               :: ctp_ap_vmin,ctp_ap_vmax

      real(kind=sreal)                 :: ctp_fg_scale,ctp_fg_offset
      integer(kind=sint)               :: ctp_fg_vmin,ctp_fg_vmax

      real(kind=sreal)                 :: stemp_ap_offset,stemp_ap_scale
      integer(kind=sint)               :: stemp_ap_vmin,stemp_ap_vmax

      real(kind=sreal)                 :: stemp_fg_offset,stemp_fg_scale
      integer(kind=sint)               :: stemp_fg_vmin,stemp_fg_vmax

      real(kind=sreal),   dimension(:), pointer :: albedo_scale,albedo_offset
      integer(kind=sint), dimension(:), pointer :: albedo_vmin,albedo_vmax

      real(kind=sreal),   dimension(:), pointer :: channels_scale,channels_offset
      integer(kind=sint), dimension(:), pointer :: channels_vmin,channels_vmax

      real(kind=sreal),   dimension(:), pointer :: y0_scale,y0_offset
      integer(kind=sint), dimension(:), pointer :: y0_vmin,y0_vmax

      real(kind=sreal),   dimension(:), pointer :: residuals_scale,residuals_offset
      integer(kind=sint), dimension(:), pointer :: residuals_vmin,residuals_vmax

      real(kind=sreal)                          :: ds_offset, ds_scale
      integer(kind=sint)                        :: ds_vmin, ds_vmax

      real(kind=sreal)                          :: covariance_offset, covariance_scale
      integer(kind=sint)                        :: covariance_vmin, covariance_vmax


      integer(kind=lint), dimension(:,:),     pointer :: scanline_u, scanline_v

      integer(kind=sint), dimension(:,:),     pointer :: cot_ap,cot_fg
      integer(kind=sint), dimension(:,:),     pointer :: ref_ap,ref_fg
      integer(kind=sint), dimension(:,:),     pointer :: ctp_ap,ctp_fg
      integer(kind=sint), dimension(:,:),     pointer :: stemp_ap,stemp_fg

      integer(kind=sint), dimension(:,:,:),   pointer :: albedo
      integer(kind=sint), dimension(:,:,:),   pointer :: channels
      integer(kind=sint), dimension(:,:,:),   pointer :: y0
      integer(kind=sint), dimension(:,:,:),   pointer :: residuals

      integer(kind=sint), dimension(:,:),     pointer :: ds

      real(kind=sreal),   dimension(:,:,:,:), pointer :: covariance

   end type output_data_secondary

contains

include 'alloc_output_data.F90'
include 'dealloc_output_data.F90'

include 'def_vars_primary.F90'
include 'def_vars_secondary.F90'

include 'prepare_primary.F90'
include 'prepare_secondary.F90'

include 'write_primary.F90'
include 'write_secondary.F90'


subroutine prepare_short_packed_float(value_in, value_out, &
                                      scale_factor, add_offset, &
                                      fill_value_in, fill_value_out, &
                                      valid_min, valid_max, bound_max_value)

   use ECP_Constants

   implicit none

   real(kind=sreal),   intent(in)  :: value_in
   integer(kind=sint), intent(out) :: value_out
   real(kind=sreal),   intent(in)  :: scale_factor
   real(kind=sreal),   intent(in)  :: add_offset
   real(kind=sreal),   intent(in)  :: fill_value_in
   integer(kind=sint), intent(in)  :: fill_value_out
   integer(kind=sint), intent(in)  :: valid_min
   integer(kind=sint), intent(in)  :: valid_max
   integer(kind=sint), intent(in)  :: bound_max_value

   real(kind=sreal)                 :: temp

   temp = (value_in - add_offset) / scale_factor

   if (value_in .ne. sreal_fill_value) then
      if (temp .lt. real(valid_min,kind=sreal)) then
         value_out=sint_fill_value
      else if (temp .gt. real(valid_max,kind=sreal)) then
         value_out=bound_max_value
      else
         value_out=int(temp, kind=sint)
      end if
   else
      value_out=sint_fill_value
   end if

end subroutine prepare_short_packed_float


subroutine prepare_float_packed_float(value_in, value_out, &
                                      scale_factor, add_offset, &
                                      fill_value_in, fill_value_out, &
                                      valid_min, valid_max, bound_max_value)

   use ECP_Constants

   implicit none

   real(kind=sreal), intent(in)  :: value_in
   real(kind=sreal), intent(out) :: value_out
   real(kind=sreal), intent(in)  :: scale_factor
   real(kind=sreal), intent(in)  :: add_offset
   real(kind=sreal), intent(in)  :: fill_value_in
   real(kind=sreal), intent(in)  :: fill_value_out
   real(kind=sreal), intent(in)  :: valid_min
   real(kind=sreal), intent(in)  :: valid_max
   real(kind=sreal), intent(in)  :: bound_max_value

   real(kind=sreal)              :: temp

   temp = (value_in - add_offset) / scale_factor

   if (temp .ge. real(valid_min,kind=sreal) .and. &
       temp .le. real(valid_max,kind=sreal)) then
      value_out=temp
   else if (temp .lt. real(valid_min,kind=sreal)) then
      value_out=sreal_fill_value
   else if (temp .gt. real(valid_max,kind=sreal)) then
      value_out=bound_max_value
   end if

end subroutine prepare_float_packed_float

end module output_routines
