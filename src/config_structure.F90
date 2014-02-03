! Name:
!
!
! Purpose:
!
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!
!
! $Id$
!
! Bugs:
!
!
module config_s
  
  use ECP_Constants
  
  implicit none
  
  type config_struct
     
     integer(kind=nint) :: nx,ny,nc,nalb,nemis,nsolar,nthermal,nmixed,nsolar_use,nthermal_use,nmixed_use

     !total number of channels to be handled in any way, even if not all are
     !processed. Note that multiple views with the same channel are treated as
     !separate channels by the code. Also, you cannot assume that
     !channels_total=nchannels_sw + nchannels_lw as some channels (3.6 microns 
     !for eg) are both solar and thermal.
     !So, for example, all channels and dual view for AATSR would mean:
     ! nchannels_total = 14 (7 wavelengths in 2 views)
     ! nchannels_sw    = 10 (first 5 wavelengths have a solar component)
     ! nchannels_lw    = 6  (last 3 wavelengths have a thermal component)
     integer(kind=nint) :: nchannels_total,nchannels_sw,nchannels_lw
     ! Number of different viewing geometries
     integer(kind=nint) :: nviews

     !channel ids (=numbers):
     !wrt original instrument defintion
     !Note that these values may well repeat for multi-view instruments, like
     !AATSR: (/ 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7 /)
     integer(kind=nint), dimension(:), pointer ::  channel_ids_instr
     !wrt plain numbering 1,2,..... with regard to increasing wavelength and
     !then view
     integer(kind=nint), dimension(:), pointer ::  channel_ids_abs


     !channel number wrt its position in the RTTOV coefficient file
     integer(kind=nint), dimension(:), pointer ::  channel_ids_rttov_coef_sw, &
          channel_ids_rttov_coef_lw

     !wavelength (in micrometers) array wrt to absolute channel numbering
     ! i.e. For all AATSR channels in both views, this would be
     ! (/ 0.55, 0.67, 0.87, 1.6, 3.7, 11, 12, 0.55, 0.67, 0.87, 1.6, 3.7, 11, 12 /)
     real(kind=sreal), dimension(:), pointer ::  channel_wl_abs

     !arrays containing 0/1 flags to identify to which part (sw/lw) of the 
     !spectrum they are assigned. could be used to determine the number of
     !channels used as well.
     integer(kind=nint), dimension(:), pointer ::  channel_sw_flag
     integer(kind=nint), dimension(:), pointer ::  channel_lw_flag
     integer(kind=nint), dimension(:), pointer ::  channel_sw_flag_use
     integer(kind=nint), dimension(:), pointer ::  channel_lw_flag_use
     integer(kind=nint), dimension(:), pointer ::  channel_mixed_flag_use

     !arrays containing the viewing geometry index for each channel
     integer(kind=nint), dimension(:), pointer ::  channel_view_ids

     !array specifing if channel is processed
     !(I know we agreed to process all channels anyway, but it might be handy 
     !to still include this even if it set to "1")
     integer(kind=nint), dimension(:), pointer ::  channel_proc_flag

  end type config_struct


end module config_s
