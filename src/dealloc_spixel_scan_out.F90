! Name: dealloc_spixel_scan_out.F90
!
!
! Purpose:
! The file contains a collection of three subroutines which deallocates the array parts of the output variable types.
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
!2011/12/19: Matthias Jerg creates initial file.
!5/1/2012 Caroline Poulsen add in reflectances and brightness
!6/1/2012 Caroline Poulsen add in cwp
!15/1/2012 Caroline Poulsen added in chans definition
!28/1/2012 Caroline Poulsen added in albedo
!2012/08/21 Matthias Jerg adds time variable
! temperature 
!
! $Id$
!
! Bugs:
!
!none known

!----------------------------------------------
!----------------------------------------------
subroutine dealloc_spixel_scan_out(spixel_scan_out)
!----------------------------------------------
!----------------------------------------------

  use ECP_Constants

  use SPixel_def

  implicit none

  type(spixel_scanline_primary_output) :: spixel_scan_out

  deallocate(spixel_scan_out%vidsat_zen)
  deallocate(spixel_scan_out%vidsol_zen)
  deallocate(spixel_scan_out%vidrel_azi)
  deallocate(spixel_scan_out%time)
  deallocate(spixel_scan_out%lon)
  deallocate(spixel_scan_out%lat)
  deallocate(spixel_scan_out%sat_zen)
  deallocate(spixel_scan_out%sol_zen)
  deallocate(spixel_scan_out%rel_azi)
  deallocate(spixel_scan_out%cot)
  deallocate(spixel_scan_out%cot_error)
  deallocate(spixel_scan_out%ref)
  deallocate(spixel_scan_out%ref_error)
  deallocate(spixel_scan_out%ctp)
  deallocate(spixel_scan_out%ctp_error)
  deallocate(spixel_scan_out%cct)
  deallocate(spixel_scan_out%cct_error)
  deallocate(spixel_scan_out%stemp)
  deallocate(spixel_scan_out%stemp_error)
  deallocate(spixel_scan_out%ctt)
  deallocate(spixel_scan_out%cth)
  deallocate(spixel_scan_out%cwp)
  deallocate(spixel_scan_out%cwp_error)
  deallocate(spixel_scan_out%convergence)
  deallocate(spixel_scan_out%niter)
  deallocate(spixel_scan_out%pchange)
  deallocate(spixel_scan_out%costja)
  deallocate(spixel_scan_out%costjm)
  deallocate(spixel_scan_out%lsflag)
  deallocate(spixel_scan_out%qcflag)
  deallocate(spixel_scan_out%illum)



end subroutine dealloc_spixel_scan_out

!----------------------------------------------
!----------------------------------------------
subroutine dealloc_spixel_scan_out_sec( spixel_scan_out_sec,lcovar)
!----------------------------------------------
!----------------------------------------------

  use ECP_Constants

  use SPixel_def

  implicit none

  logical :: lcovar

  type(spixel_scanline_secondary_output) :: spixel_scan_out_sec

  deallocate(spixel_scan_out_sec%scanline_u)
  deallocate(spixel_scan_out_sec%scanline_v)
  deallocate(spixel_scan_out_sec%cot_ap)
  deallocate(spixel_scan_out_sec%cot_fg)
  deallocate(spixel_scan_out_sec%ref_ap)
  deallocate(spixel_scan_out_sec%ref_fg)
  deallocate(spixel_scan_out_sec%ctp_ap)
  deallocate(spixel_scan_out_sec%ctp_fg)

  deallocate(spixel_scan_out_sec%stemp_fg)

  deallocate(spixel_scan_out_sec%vidres)
  deallocate(spixel_scan_out_sec%vidchans)	
 deallocate(spixel_scan_out_sec%vidalb)	
  deallocate(spixel_scan_out_sec%vidy0)	
  deallocate(spixel_scan_out_sec%res_scale)
  deallocate(spixel_scan_out_sec%res_offset)
  deallocate(spixel_scan_out_sec%chans_scale)
  deallocate(spixel_scan_out_sec%chans_offset)

  deallocate(spixel_scan_out_sec%alb_scale)
  deallocate(spixel_scan_out_sec%alb_offset)

  deallocate(spixel_scan_out_sec%y0_scale)
  deallocate(spixel_scan_out_sec%y0_offset)
  deallocate(spixel_scan_out_sec%res_vmin)
  deallocate(spixel_scan_out_sec%res_vmax)
  deallocate(spixel_scan_out_sec%chans_vmin)
  deallocate(spixel_scan_out_sec%chans_vmax)

  deallocate(spixel_scan_out_sec%alb_vmin)
  deallocate(spixel_scan_out_sec%alb_vmax)

  deallocate(spixel_scan_out_sec%y0_vmin)
  deallocate(spixel_scan_out_sec%y0_vmax)
  deallocate(spixel_scan_out_sec%residuals)
  deallocate(spixel_scan_out_sec%channels)
  deallocate(spixel_scan_out_sec%albedo)
  deallocate(spixel_scan_out_sec%y0)
!  deallocate(spixel_scan_out_sec%yn)
  if(lcovar) then

     deallocate(spixel_scan_out_sec%vidcovar)
     deallocate(spixel_scan_out_sec%covariance)

  endif

end subroutine dealloc_spixel_scan_out_sec


!----------------------------------------------
!----------------------------------------------
subroutine dealloc_spixel_scan_in( spixel_scan_in)
!----------------------------------------------
!----------------------------------------------

  use ECP_Constants

  use SPixel_def

  implicit none

  type(spixel_scanline_input) :: spixel_scan_in

  deallocate(spixel_scan_in%vidinput)
  deallocate(spixel_scan_in%input_scale)
  deallocate(spixel_scan_in%input_offset)
  deallocate(spixel_scan_in%input_vmin)
  deallocate(spixel_scan_in%input_vmax)
  deallocate(spixel_scan_in%input)
  deallocate(spixel_scan_in%viderror)
  deallocate(spixel_scan_in%error_scale)
  deallocate(spixel_scan_in%error_offset)
  deallocate(spixel_scan_in%error_vmin)
  deallocate(spixel_scan_in%error_vmax)
  deallocate(spixel_scan_in%error)

end subroutine dealloc_spixel_scan_in
