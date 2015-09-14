!-------------------------------------------------------------------------------
! Name: postproc_utils.F90
!
! Purpose:
!
! History:
! 2015/09/14, GM: Original version
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module postproc_utils

   use common_constants

   implicit none

contains

subroutine copy_class_specific_inputs(i, j, primary2, primary1, secondary2, &
                                      secondary1, do_secondary)

   use orac_input
   use postproc_constants

   implicit none

   integer,                    intent(in)    :: i, j
   type(input_data_primary),   intent(inout) :: primary2
   type(input_data_primary),   intent(in)    :: primary1
   type(input_data_secondary), intent(inout) :: secondary2
   type(input_data_secondary), intent(in)    :: secondary1
   logical,                    intent(in)    :: do_secondary

   ! primary file
   primary2%cot(i,j)                  = primary1%cot(i,j)
   primary2%cot_uncertainty(i,j)      = primary1%cot_uncertainty(i,j)

   primary2%ref(i,j)                  = primary1%ref(i,j)
   primary2%ref_uncertainty(i,j)      = primary1%ref_uncertainty(i,j)

   primary2%ctp(i,j)                  = primary1%ctp(i,j)
   primary2%ctp_uncertainty(i,j)      = primary1%ctp_uncertainty(i,j)

   primary2%cc_total(i,j)             = primary1%cc_total(i,j)
   primary2%cc_total_uncertainty(i,j) = primary1%cc_total_uncertainty(i,j)

   primary2%stemp(i,j)                = primary1%stemp(i,j)
   primary2%stemp_uncertainty(i,j)    = primary1%stemp_uncertainty(i,j)

   primary2%cth(i,j)                  = primary1%cth(i,j)
   primary2%cth_uncertainty(i,j)      = primary1%cth_uncertainty(i,j)

   primary2%ctt(i,j)                  = primary1%ctt(i,j)
   primary2%ctt_uncertainty(i,j)      = primary1%ctt_uncertainty(i,j)

   primary2%cwp(i,j)                  = primary1%cwp(i,j)
   primary2%cwp_uncertainty(i,j)      = primary1%cwp_uncertainty(i,j)

   primary2%cloud_albedo(i,j,:)       = primary1%cloud_albedo(i,j,:)

   primary2%convergence(i,j)          = primary1%convergence(i,j)

   primary2%niter(i,j)                = primary1%niter(i,j)

!  primary2%phase(i,j)                = primary1%phase(i,j)

   primary2%costja(i,j)               = primary1%costja(i,j)
   primary2%costjm(i,j)               = primary1%costjm(i,j)

   primary2%qcflag(i,j)               = primary1%qcflag(i,j)

   ! secondary file
   if (do_secondary) then
      secondary2%cot_ap(i,j)      = secondary1%cot_ap(i,j)
      secondary2%cot_fg(i,j)      = secondary1%cot_fg(i,j)

      secondary2%ref_ap(i,j)      = secondary1%ref_ap(i,j)
      secondary2%ref_fg(i,j)      = secondary1%ref_fg(i,j)

      secondary2%ctp_ap(i,j)      = secondary1%ctp_ap(i,j)
      secondary2%ctp_fg(i,j)      = secondary1%ctp_fg(i,j)

      secondary2%stemp_ap(i,j)    = secondary1%stemp_ap(i,j)
      secondary2%stemp_fg(i,j)    = secondary1%stemp_fg(i,j)

      secondary2%y0(i,j,:)        = secondary1%y0(i,j,:)

      secondary2%residuals(i,j,:) = secondary1%residuals(i,j,:)

      secondary2%ds(i,j)          = secondary1%ds(i,j)
   end if

end subroutine copy_class_specific_inputs

end module postproc_utils
