!-------------------------------------------------------------------------------
! Name: channel_structures.F90
!
! Purpose:
! Module to define variables types which hold the channel information
! PUT EVERYTHING IN HERE NECESSARY FOR CHANNEL INFORMATION!!
! PLEASE COMMENT SUFFICIENTLY TO CLEARLY EXPLAIN THE ROLE OF THE
! INDIVIDUAL INFORMATION!!
!
! History:
! 2012/06/01, MJ: writes initial version.
! 2012/06/18, GT: Made some changes for dual view indexing
! 2012/08/22, GT: Added nview (number of viewing geometries)
! 2014/10/15, GM: Added map_ids_abs_to_ref_band_land and
!    map_ids_abs_to_ref_band_sea and removed channel_proc_flag.
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/03/04, GM: Added map_ids_abs_to_snow_and_ice.
! 2016/04/08, SP: Added variables to cope with multiple view sensors.
! 2016/05/27, SP: Updates to enable RTTOV to work correctly with multi-views.
! 2016/07/01, GT: Added map_ids_sw_to_channel and map_ids_lw_to_channel.
! 2016/08/04, GM: Added map_ids_channel_to_sw and map_ids_channel_to_lw.
! 2017/07/05, AP: Add NAll to track the total number of channels.
! 2021/03/09, AP: Add radiance bias corrections.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module channel_structures_m

   use preproc_constants_m

   implicit none

   type channel_info_t

      ! Total number of channels to be handled in any way, even if not all are
      ! processed. Note that multiple views with the same channel are treated as
      ! separate channels by the code. Also, you cannot assume that
      ! channels_total=nchannels_sw + nchannels_lw as some channels (3.6 microns
      ! for eg) are both solar and thermal.
      ! So, for example, all channels and dual view for AATSR would mean:
      ! nchannels_total = 14 (7 wavelengths in 2 views)
      ! nchannels_sw    = 10 (first 5 wavelengths have a solar component)
      ! nchannels_lw    = 6  (last 3 wavelengths have a thermal component)
      integer(kind=lint) :: nchannels_total,nchannels_sw,nchannels_lw
      ! # of channels that exist on the instrument
      integer(kind=lint) :: all_nchannels_total
      ! Number of different viewing geometries
      integer(kind=lint) :: nviews

      ! channel ids (=numbers):
      ! wrt original instrument definition
      ! Note that these values may well repeat for multi-view instruments, like
      ! AATSR: (/ 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7 /)
      integer(kind=lint), dimension(:), pointer :: channel_ids_instr

      ! Wavelength (in micrometers) array wrt to absolute channel numbering
      ! i.e. For all AATSR channels in both views, this would be
      ! (/ 0.55, 0.67, 0.87, 1.6, 3.7, 11, 12, 0.55, 0.67, 0.87, 1.6, 3.7, 11, 12 /)
      real(kind=sreal), dimension(:), pointer :: channel_wl_abs

      ! Arrays linking sw and lw indexed arrays to channel indexed arrays useful
      ! when dealing with multiple view instruments, where sw and lw channels
      ! aren't contiguous in the channel listing
      integer(kind=lint), dimension(:), pointer :: map_ids_sw_to_channel
      integer(kind=lint), dimension(:), pointer :: map_ids_lw_to_channel
      integer(kind=lint), dimension(:), pointer :: map_ids_channel_to_sw
      integer(kind=lint), dimension(:), pointer :: map_ids_channel_to_lw

      ! Arrays containing the viewing geometry index for each channel
      integer(kind=lint), dimension(:), pointer :: channel_view_ids
      integer(kind=lint), dimension(:), pointer :: lw_view_ids
      integer(kind=lint), dimension(:), pointer :: sw_view_ids
      integer(kind=lint), dimension(:), pointer :: lw_rttov_viewone_id
      integer(kind=lint), dimension(:), pointer :: sw_rttov_viewone_id

      ! Arrays containing 0/1 flags to identify to which part (sw/lw) of the
      ! spectrum they are assigned. could be used to determine the number of
      ! channels used as well.
      integer(kind=lint), dimension(:), pointer :: channel_sw_flag
      integer(kind=lint), dimension(:), pointer :: channel_lw_flag

      ! Channel number wrt its position in the RTTOV coefficient file
      integer(kind=lint), dimension(:), pointer :: channel_ids_rttov_coef_sw
      integer(kind=lint), dimension(:), pointer :: channel_ids_rttov_coef_lw

      ! Map the channel ids to the ancillary reflectance input bands
      integer(kind=lint), dimension(:), pointer :: map_ids_abs_to_ref_band_land
      integer(kind=lint), dimension(:), pointer :: map_ids_abs_to_ref_band_sea

      ! Map the channel ids to the snow and ice albedo channels in
      !correct_for_ice_snow()
      integer(kind=lint), dimension(:), pointer :: map_ids_abs_to_snow_and_ice

      ! Uncertainty estimates, used by read_imager()
      real(kind=sreal),   dimension(:), pointer :: channel_fractional_uncertainty
      real(kind=sreal),   dimension(:), pointer :: channel_minimum_uncertainty
      real(kind=sreal),   dimension(:), pointer :: channel_fm_lnd_uncertainty
      real(kind=sreal),   dimension(:), pointer :: channel_fm_sea_uncertainty

      ! Bias correction for each channel
      real(kind=sreal),   dimension(:), pointer :: channel_absolute_bias
      real(kind=sreal),   dimension(:), pointer :: channel_relative_bias

   end type channel_info_t

contains

#include "allocate_channel_info.F90"
#include "deallocate_channel_info.F90"

end module channel_structures_m
