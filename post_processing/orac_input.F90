!-------------------------------------------------------------------------------
! Name: orac_input.F90
!
! Purpose: F90 Module file which declares user defined variable type structures.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/03, MJ: cleans out prototype code to prepare repository upload.
! 2012/03/06, CP: modified to produce post processed files
! 2012/03/18, CP: modified to add cloud flag
! 2012/06/20, CP: modified to add albedo
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2014/09/20, CP: adds in extra channel variables
! 2014/09/29, CP: adds in variable names for MODIS
! 2014/10/24, OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM,
!    and nisemask
! 2014/12/02, CP: adds in cloud_albedo
! 2015/02/05, OS: deactivated use of postproc_constants to force consistency with
!    common_constants; changed nint to lint; added variable phase_post
! 2015/07/16, GM: Major cleanup and add associated routines to module.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module orac_input

   use common_constants

   implicit none


   type counts_and_indexes
      integer          :: NViews
      integer          :: Ny
      integer          :: NSolar
      integer          :: NThermal
      integer, pointer :: YSolar(:)
      integer, pointer :: Y_Id(:)
      integer, pointer :: Ch_Is(:)
      integer          :: Nx
   end type counts_and_indexes


   type input_data_primary

      real(kind=dreal),    dimension(:,:),   pointer :: time
      real(kind=sreal),    dimension(:,:),   pointer :: lat, lon
      real(kind=sreal),    dimension(:,:),   pointer :: solar_zenith_view_no1
      real(kind=sreal),    dimension(:,:),   pointer :: satellite_zenith_view_no1
      real(kind=sreal),    dimension(:,:),   pointer :: rel_azimuth_view_no1
      real(kind=sreal),    dimension(:,:),   pointer :: cot, cot_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: ref, ref_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: ctp, ctp_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: cct, cct_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: cc_total, cc_total_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: stemp, stemp_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: cth, cth_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: cth_corrected, cth_corrected_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: ctt, ctt_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: cwp, cwp_uncertainty
      real(kind=sreal),    dimension(:,:,:), pointer :: cloud_albedo, cloud_albedo_uncertainty

      integer(kind=byte),  dimension(:,:),   pointer :: convergence
      integer(kind=byte),  dimension(:,:),   pointer :: niter
      real(kind=sreal),    dimension(:,:),   pointer :: costja
      real(kind=sreal),    dimension(:,:),   pointer :: costjm
      character(len=512)                             :: qc_flag_meanings
      integer(kind=sint),  dimension(:,:),   pointer :: qcflag

      integer(kind=byte),  dimension(:,:),   pointer :: lsflag
      integer(kind=byte),  dimension(:,:),   pointer :: lusflag
      integer(kind=sint),  dimension(:,:),   pointer :: dem
      integer(kind=byte),  dimension(:,:),   pointer :: nisemask

      integer(kind=byte),  dimension(:,:),   pointer :: illum

      integer(kind=byte),  dimension(:,:),   pointer :: cldtype
      integer(kind=byte),  dimension(:,:),   pointer :: cldmask
      real(kind=sreal),    dimension(:,:),   pointer :: cldmask_uncertainty
      real(kind=sreal),    dimension(:,:),   pointer :: cccot_pre

      integer(kind=byte),  dimension(:,:),   pointer :: phase

   end type input_data_primary


   type input_data_secondary

!     integer(kind=lint), dimension(:,:),   pointer :: scanline_u
!     integer(kind=lint), dimension(:,:),   pointer :: scanline_v

      real(kind=sreal),   dimension(:,:),   pointer :: cot_ap,cot_fg
      real(kind=sreal),   dimension(:,:),   pointer :: ref_ap,ref_fg
      real(kind=sreal),   dimension(:,:),   pointer :: ctp_ap,ctp_fg
      real(kind=sreal),   dimension(:,:),   pointer :: stemp_ap,stemp_fg

      real(kind=sreal),   dimension(:,:,:), pointer :: albedo
      real(kind=sreal),   dimension(:,:,:), pointer :: channels
      real(kind=sreal),   dimension(:,:,:), pointer :: y0
      real(kind=sreal),   dimension(:,:,:), pointer :: residuals

      real(kind=sreal),   dimension(:,:),   pointer :: ds

   end type input_data_secondary

contains

#include "alloc_input_data.F90"
#include "dealloc_input_data.F90"

#include "read_input_dimensions.F90"

#include "read_input_primary.F90"
#include "read_input_secondary.F90"

end module orac_input
