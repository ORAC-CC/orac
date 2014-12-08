! Name: dealloc_spixel_scan_out_pp.F90
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
! temperature 
! 6/3/2012  Caroline Poulsen  Modified to create post processed 
!                    netcdf files with best phase selected
! 7/3/2012  Martin Stengel added missing stemp_ap
! 2012/03/18 Caroline Poulsen modified to add cloud flag
! 2012/07/06 MJ extensively overhauls and restructures the code
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
! 2014/10/24 OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!  (currently deactivated), and nisemask
! 2014/11/20 OS: added deallocation of pavolonis phase
! 2014/11/26 OS: added deallocation of cloud_albedo
!
! $Id$
!
! Bugs:
!
!none known

!----------------------------------------------
!----------------------------------------------
subroutine dealloc_spixel_scan_out_pp(spixel_scan_out)
  !----------------------------------------------
  !----------------------------------------------

  use vartypes_pp
  use common_constants

  use scanline_structure

  implicit none

  type(spixel_scanline_primary_output) :: spixel_scan_out

  write(*,*) 'start deallocation'

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
  deallocate(spixel_scan_out%cccot)
  deallocate(spixel_scan_out%cccot_pre)
  deallocate(spixel_scan_out%stemp)
  deallocate(spixel_scan_out%stemp_error)
  deallocate(spixel_scan_out%ctt)
  deallocate(spixel_scan_out%ctt_error)
  deallocate(spixel_scan_out%cth)
  deallocate(spixel_scan_out%cth_error)
  deallocate(spixel_scan_out%cwp)
  deallocate(spixel_scan_out%cloud_albedo)	
  deallocate(spixel_scan_out%cwp_error)
  deallocate(spixel_scan_out%convergence)
  deallocate(spixel_scan_out%niter)
  deallocate(spixel_scan_out%phase)
  deallocate(spixel_scan_out%phase_pavolonis)
  deallocate(spixel_scan_out%costja)
  deallocate(spixel_scan_out%costjm)

  write(*,*) 'start deallocation lsflag'

  deallocate(spixel_scan_out%lsflag)

  write(*,*) 'start deallocation lusflag'

  deallocate(spixel_scan_out%lusflag)

  write(*,*) 'start deallocation cldtype'

  deallocate(spixel_scan_out%cldtype)

  write(*,*) 'start deallocation qcflag'

  deallocate(spixel_scan_out%qcflag)

  write(*,*) 'start deallocation illum'

  deallocate(spixel_scan_out%illum)

  write(*,*) 'start deallocation cldmask'

  deallocate(spixel_scan_out%cldmask)

  !write(*,*) 'start deallocation dem'

  !deallocate(spixel_scan_out%dem)

  write(*,*) 'start deallocation nisemask'

  deallocate(spixel_scan_out%nisemask)


end subroutine dealloc_spixel_scan_out_pp

!!$!----------------------------------------------
!!$!----------------------------------------------
!!$subroutine dealloc_spixel_scan_out_sec_pp( spixel_scan_out_sec,lcovar)
!!$!----------------------------------------------
!!$!----------------------------------------------
!!$
!!$  use vartypes_pp
!!$
!!$  use SPixel_pp
!!$
!!$  implicit none
!!$
!!$  logical :: lcovar
!!$
!!$  type(spixel_scanline_secondary_output) :: spixel_scan_out_sec
!!$
!!$  deallocate(spixel_scan_out_sec%scanline_u)
!!$  deallocate(spixel_scan_out_sec%scanline_v)
!!$  deallocate(spixel_scan_out_sec%cot_ap)
!!$  deallocate(spixel_scan_out_sec%cot_fg)
!!$  deallocate(spixel_scan_out_sec%ref_ap)
!!$  deallocate(spixel_scan_out_sec%ref_fg)
!!$  deallocate(spixel_scan_out_sec%ctp_ap)
!!$  deallocate(spixel_scan_out_sec%ctp_fg)
!!$
!!$  deallocate(spixel_scan_out_sec%stemp_ap)
!!$  deallocate(spixel_scan_out_sec%stemp_fg)
!!$
!!$  deallocate(spixel_scan_out_sec%vidres)
!!$  deallocate(spixel_scan_out_sec%vidchans)	
!!$  deallocate(spixel_scan_out_sec%vidy0)	
!!$  deallocate(spixel_scan_out_sec%res_scale)
!!$  deallocate(spixel_scan_out_sec%res_offset)
!!$  deallocate(spixel_scan_out_sec%chans_scale)
!!$  deallocate(spixel_scan_out_sec%chans_offset)
!!$  deallocate(spixel_scan_out_sec%y0_scale)
!!$  deallocate(spixel_scan_out_sec%y0_offset)
!!$  deallocate(spixel_scan_out_sec%res_vmin)
!!$  deallocate(spixel_scan_out_sec%res_vmax)
!!$  deallocate(spixel_scan_out_sec%chans_vmin)
!!$  deallocate(spixel_scan_out_sec%chans_vmax)
!!$  deallocate(spixel_scan_out_sec%y0_vmin)
!!$  deallocate(spixel_scan_out_sec%y0_vmax)
!!$  deallocate(spixel_scan_out_sec%residuals)
!!$  deallocate(spixel_scan_out_sec%channels)
!!$  deallocate(spixel_scan_out_sec%y0)
!!$!  deallocate(spixel_scan_out_sec%yn)
!!$  if(lcovar) then
!!$
!!$     deallocate(spixel_scan_out_sec%vidcovar)
!!$     deallocate(spixel_scan_out_sec%covariance)
!!$
!!$  endif
!!$
!!$end subroutine dealloc_spixel_scan_out_sec_pp
!!$
!!$
!!$!----------------------------------------------
!!$!----------------------------------------------
!!$subroutine dealloc_spixel_scan_in( spixel_scan_in)
!!$!----------------------------------------------
!!$!----------------------------------------------
!!$
!!$  use vartypes_pp
!!$
!!$  use SPixel_pp
!!$
!!$  implicit none
!!$
!!$  type(spixel_scanline_input) :: spixel_scan_in
!!$
!!$  deallocate(spixel_scan_in%vidinput)
!!$  deallocate(spixel_scan_in%input_scale)
!!$  deallocate(spixel_scan_in%input_offset)
!!$  deallocate(spixel_scan_in%input_vmin)
!!$  deallocate(spixel_scan_in%input_vmax)
!!$  deallocate(spixel_scan_in%input)
!!$  deallocate(spixel_scan_in%viderror)
!!$  deallocate(spixel_scan_in%error_scale)
!!$  deallocate(spixel_scan_in%error_offset)
!!$  deallocate(spixel_scan_in%error_vmin)
!!$  deallocate(spixel_scan_in%error_vmax)
!!$  deallocate(spixel_scan_in%error)
!!$
!!$end subroutine dealloc_spixel_scan_in
