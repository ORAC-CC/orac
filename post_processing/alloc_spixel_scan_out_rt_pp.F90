! Name: alloc_spixel_scan_out_pp.F90
!
!
! Purpose:
! The file contains a collection of three subroutines which allocate the array parts of the output variable types.
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
!5/1/2012 Caroline Poulsen added in channel information
!6/1/2012 Caroline Poulsen added in cwp
!15/1/2012 Caroline Poulsen added in chan definitions
!6/3/2012  Caroline Poulsen  Modified to create post processed 
!                    netcdf files with best phase selected!
! 7/3/2012  Martin Stengel added missing stemp_ap
! 2012/07/06 MJ extensively overhauls and restructures the code
! 2015/02/07 CP changed to common constants
! $Id$
!
! Bugs:
!
!none known


!----------------------------------------------
!----------------------------------------------
subroutine alloc_spixel_scan_out_rt_pp( ixstart,ixstop,iystart,iystop,l2_input_2dice_refl_bt,spixel_scan_out_rt)
!----------------------------------------------
!----------------------------------------------

  use vartypes_pp

  use scanline_structure

  use structures_pp

  implicit none

  integer :: ixstart, ixstop,iystart, iystop, NViews

  type(l2_input_struct_2d_refl_bt) :: l2_input_2dice_refl_bt

  type(spixel_scanline_output_rt) :: spixel_scan_out_rt

  

end subroutine alloc_spixel_scan_out_rt_pp


!----------------------------------------------
!----------------------------------------------
subroutine dealloc_spixel_scan_out_rt_pp(spixel_scan_out_rt)
!----------------------------------------------
!----------------------------------------------

  use vartypes_pp

  use scanline_structure

  implicit none

  type(spixel_scanline_output_rt) :: spixel_scan_out_rt
  

end subroutine dealloc_spixel_scan_out_rt_pp










!!$
!!$


!!$!----------------------------------------------
!!$!----------------------------------------------
!!$subroutine alloc_spixel_scan_out_pp( ixstart,ixstop,NViews,spixel_scan_out)
!!$!----------------------------------------------
!!$!----------------------------------------------
!!$
!!$  use vartypes_pp
!!$
!!$  use SPixel_pp
!!$
!!$  implicit none
!!$
!!$  integer :: ixstart, ixstop, NViews
!!$
!!$  type(spixel_scanline_primary_output) :: spixel_scan_out
!!$
!!$
!!$!  allocate(spixel_scan_out%vidsat_zen)
!!$!  spixel_scan_out%vidsat_zen=0
!!$
!!$ ! allocate(spixel_scan_out%vidsol_zen)
!!$ ! spixel_scan_out%vidsol_zen=0
!!$
!!$ ! allocate(spixel_scan_out%vidrel_azi)
!!$ ! spixel_scan_out%vidrel_azi=0
!!$
!!$  allocate(spixel_scan_out%time(ixstart:ixstop))
!!$  spixel_scan_out%time(ixstart:ixstop)=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out%lon(ixstart:ixstop))
!!$  spixel_scan_out%lon(ixstart:ixstop)=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out%lat(ixstart:ixstop))
!!$  spixel_scan_out%lat(ixstart:ixstop)=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out%sat_zen(ixstart:ixstop))
!!$  spixel_scan_out%sat_zen(ixstart:ixstop)=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out%sol_zen(ixstart:ixstop))
!!$  spixel_scan_out%sol_zen(ixstart:ixstop)=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out%rel_azi(ixstart:ixstop))
!!$  spixel_scan_out%rel_azi(ixstart:ixstop)=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out%cot(ixstart:ixstop))
!!$  spixel_scan_out%cot(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$  allocate(spixel_scan_out%cot_error(ixstart:ixstop))
!!$  spixel_scan_out%cot_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%ref(ixstart:ixstop))
!!$  spixel_scan_out%ref(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$  allocate(spixel_scan_out%ref_error(ixstart:ixstop))
!!$  spixel_scan_out%ref_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%ctp(ixstart:ixstop))
!!$  spixel_scan_out%ctp(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$  allocate(spixel_scan_out%ctp_error(ixstart:ixstop))
!!$  spixel_scan_out%ctp_error(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$
!!$  allocate(spixel_scan_out%cct(ixstart:ixstop))
!!$  spixel_scan_out%cct(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$  allocate(spixel_scan_out%cct_error(ixstart:ixstop))
!!$  spixel_scan_out%cct_error(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$
!!$  allocate(spixel_scan_out%stemp(ixstart:ixstop))
!!$  spixel_scan_out%stemp(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$  allocate(spixel_scan_out%stemp_error(ixstart:ixstop))
!!$  spixel_scan_out%stemp_error(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$
!!$  allocate(spixel_scan_out%ctt(ixstart:ixstop))
!!$  spixel_scan_out%ctt(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$
!!$  allocate(spixel_scan_out%cth(ixstart:ixstop))
!!$  spixel_scan_out%cth(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$
!!$  allocate(spixel_scan_out%cwp(ixstart:ixstop))
!!$  spixel_scan_out%cwp(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$  allocate(spixel_scan_out%cwp_error(ixstart:ixstop))
!!$  spixel_scan_out%cwp_error(ixstart:ixstop)=spixel_scan_out%sint_fill_value
!!$
!!$
!!$
!!$  allocate(spixel_scan_out%convergence(ixstart:ixstop))
!!$  spixel_scan_out%convergence(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$  allocate(spixel_scan_out%niter(ixstart:ixstop))
!!$  spixel_scan_out%niter(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$  allocate(spixel_scan_out%pchange(ixstart:ixstop))
!!$  spixel_scan_out%pchange(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$  allocate(spixel_scan_out%costja(ixstart:ixstop))
!!$  spixel_scan_out%costja(ixstart:ixstop)=spixel_scan_out%sint_file_value
!!$
!!$  allocate(spixel_scan_out%costjm(ixstart:ixstop))
!!$  spixel_scan_out%costjm(ixstart:ixstop)=spixel_scan_out%sint_file_value
!!$
!!$  allocate(spixel_scan_out%lsflag(ixstart:ixstop))
!!$  spixel_scan_out%lsflag(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$
!!$  allocate(spixel_scan_out%cloudflag(ixstart:ixstop))
!!$  spixel_scan_out%cloudflag(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$  allocate(spixel_scan_out%qcflag(ixstart:ixstop))
!!$  spixel_scan_out%qcflag(ixstart:ixstop)=spixel_scan_out%sint_file_value
!!$
!!$  allocate(spixel_scan_out%illum(ixstart:ixstop))
!!$  spixel_scan_out%illum(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$end subroutine alloc_spixel_scan_out_pp
!!$
!!$!----------------------------------------------
!!$!----------------------------------------------
!!$subroutine alloc_spixel_scan_out_sec_pp( ixstart,ixstop,Ny,Nx,lcovar,spixel_scan_out_sec)
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
!!$  integer :: ixstart, ixstop,Ny,Nx
!!$
!!$  type(spixel_scanline_secondary_output) :: spixel_scan_out_sec
!!$
!!$  allocate(spixel_scan_out_sec%scanline_u(ixstart:ixstop))
!!$  spixel_scan_out_sec%scanline_u(ixstart:ixstop)=spixel_scan_out_sec%lsint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%scanline_v(ixstart:ixstop))
!!$  spixel_scan_out_sec%scanline_v(ixstart:ixstop)=spixel_scan_out_sec%lsint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%cot_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%cot_ap(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%cot_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%cot_fg(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%ref_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%ref_ap(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%ref_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%ref_fg(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$
!!$  allocate(spixel_scan_out_sec%ctp_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%ctp_ap(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%ctp_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%ctp_fg(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%stemp_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%stemp_ap(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%stemp_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%stemp_fg(ixstart:ixstop)=spixel_scan_out_sec%sint_file_value
!!$
!!$
!!$
!!$!write(*,*)'alloc ny',ny
!!$
!!$  allocate(spixel_scan_out_sec%vidres(1:Ny))
!!$  spixel_scan_out_sec%vidres=0
!!$
!!$
!!$  allocate(spixel_scan_out_sec%vidchans(1:Ny))
!!$  spixel_scan_out_sec%vidchans=0
!!$
!!$
!!$  allocate(spixel_scan_out_sec%vidy0(1:Ny))
!!$  spixel_scan_out_sec%vidy0=0
!!$
!!$  allocate(spixel_scan_out_sec%res_scale(1:Ny))
!!$  spixel_scan_out_sec%res_scale=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%res_offset(1:Ny))
!!$  spixel_scan_out_sec%res_offset=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%res_vmin(1:Ny))
!!$  spixel_scan_out_sec%res_vmin=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%res_vmax(1:Ny))
!!$  spixel_scan_out_sec%res_vmax=spixel_scan_out_sec%sint_file_value
!!$
!!$
!!$
!!$  allocate(spixel_scan_out_sec%chans_scale(1:Ny))
!!$  spixel_scan_out_sec%chans_scale=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%chans_offset(1:Ny))
!!$  spixel_scan_out_sec%chans_offset=dreal_fill_value
!!$
!!$
!!$  allocate(spixel_scan_out_sec%chans_vmin(1:Ny))
!!$  spixel_scan_out_sec%chans_vmin=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%chans_vmax(1:Ny))
!!$  spixel_scan_out_sec%chans_vmax=spixel_scan_out_sec%sint_file_value
!!$
!!$
!!$
!!$  allocate(spixel_scan_out_sec%y0_scale(1:Ny))
!!$  spixel_scan_out_sec%y0_scale=dreal_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%y0_offset(1:Ny))
!!$  spixel_scan_out_sec%y0_offset=dreal_fill_value
!!$
!!$
!!$  allocate(spixel_scan_out_sec%y0_vmin(1:Ny))
!!$  spixel_scan_out_sec%y0_vmin=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%y0_vmax(1:Ny))
!!$  spixel_scan_out_sec%y0_vmax=spixel_scan_out_sec%sint_file_value
!!$
!!$
!!$
!!$
!!$
!!$
!!$  allocate(spixel_scan_out_sec%residuals(ixstart:ixstop,1:Ny))
!!$  spixel_scan_out_sec%residuals(ixstart:ixstop,1:Ny)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%channels(ixstart:ixstop,1:Ny))
!!$  spixel_scan_out_sec%channels(ixstart:ixstop,1:Ny)=spixel_scan_out_sec%sint_file_value
!!$
!!$  allocate(spixel_scan_out_sec%y0(ixstart:ixstop,1:Ny))
!!$  spixel_scan_out_sec%y0(ixstart:ixstop,1:Ny)=spixel_scan_out_sec%sint_file_value
!!$
!!$  if(lcovar) then
!!$
!!$     allocate(spixel_scan_out_sec%vidcovar(Nx,Nx))
!!$     spixel_scan_out_sec%vidcovar=0
!!$
!!$     allocate(spixel_scan_out_sec%covariance(ixstart:ixstop,Nx,Nx))
!!$     spixel_scan_out_sec%covariance=dreal_fill_value
!!$
!!$  endif
!!$
!!$end subroutine alloc_spixel_scan_out_sec_pp
!!$
!!$
!!$!----------------------------------------------
!!$!----------------------------------------------
!!$subroutine alloc_spixel_scan_in( ixstart,ixstop,Ny,spixel_scan_in)
!!$!----------------------------------------------
!!$!----------------------------------------------
!!$
!!$  use vartypes_pp
!!$
!!$  use SPixel_pp
!!$
!!$  implicit none
!!$
!!$  integer :: ixstart, ixstop, Ny
!!$
!!$  type(spixel_scanline_input) :: spixel_scan_in
!!$
!!$  allocate(spixel_scan_in%vidinput(Ny))
!!$  spixel_scan_in%vidinput=0
!!$
!!$  allocate(spixel_scan_in%input_scale(Ny))
!!$  spixel_scan_in%input_scale=dreal_fill_value
!!$
!!$  allocate(spixel_scan_in%input_offset(Ny))
!!$  spixel_scan_in%input_offset=dreal_fill_value
!!$
!!$  allocate(spixel_scan_in%input_vmin(Ny))
!!$  spixel_scan_in%input_vmin=spixel_scan_in%sint_file_value
!!$
!!$  allocate(spixel_scan_in%input_vmax(Ny))
!!$  spixel_scan_in%input_vmax=spixel_scan_in%sint_file_value
!!$
!!$  allocate(spixel_scan_in%input(ixstart:ixstop,Ny))
!!$  spixel_scan_in%input(ixstart:ixstop,Ny)=spixel_scan_in%sint_file_value
!!$
!!$
!!$  allocate(spixel_scan_in%viderror(Ny))
!!$  spixel_scan_in%viderror=0
!!$
!!$  allocate(spixel_scan_in%error_scale(Ny))
!!$  spixel_scan_in%error_scale=dreal_fill_value
!!$
!!$  allocate(spixel_scan_in%error_offset(Ny))
!!$  spixel_scan_in%error_offset=dreal_fill_value
!!$
!!$  allocate(spixel_scan_in%error_vmin(Ny))
!!$  spixel_scan_in%error_vmin=spixel_scan_in%sint_file_value
!!$
!!$  allocate(spixel_scan_in%error_vmax(Ny))
!!$  spixel_scan_in%error_vmax=spixel_scan_in%sint_file_value
!!$
!!$  allocate(spixel_scan_in%error(ixstart:ixstop,Ny))
!!$  spixel_scan_in%error(ixstart:ixstop,Ny)=spixel_scan_in%sint_file_value
!!$
!!$end subroutine alloc_spixel_scan_in
