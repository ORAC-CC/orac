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
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
! 2014/10/24 OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!  (currently deactivated), and nisemask
! 2014/11/20 OS: added allocation of pavolonis phase
! 2014/11/26 Cp: added allocation of cloud_albedo
! 2015/02/05 OS: changed nint to lint

! $Id$
!
! Bugs:
!
!none known


!----------------------------------------------
!----------------------------------------------
subroutine alloc_spixel_scan_out_pp( ixstart,ixstop,iystart,iystop,NViews,spixel_scan_out)
!----------------------------------------------
!----------------------------------------------

  use vartypes_pp
  use common_constants

  use scanline_structure

  implicit none

  integer(kind=lint) :: ixstart, ixstop,iystart, iystop, NViews,nchan

  type(spixel_scanline_primary_output) :: spixel_scan_out
  
  nchan=2

  allocate(spixel_scan_out%vidcloud_albedo(Nchan))
  spixel_scan_out%vidcloud_albedo=0

  allocate(spixel_scan_out%vidsat_zen(NViews))
  spixel_scan_out%vidsat_zen=0



  allocate(spixel_scan_out%vidsol_zen(NViews))
  spixel_scan_out%vidsol_zen=0

  allocate(spixel_scan_out%vidrel_azi(NViews))
  spixel_scan_out%vidrel_azi=0
  
  allocate(spixel_scan_out%time(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%time(ixstart:ixstop,iystart:iystop)=real_fill_value

  allocate(spixel_scan_out%lon(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%lon(ixstart:ixstop,iystart:iystop)=real_fill_value

  allocate(spixel_scan_out%lat(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%lat(ixstart:ixstop,iystart:iystop)=real_fill_value

  allocate(spixel_scan_out%sat_zen(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%sat_zen(ixstart:ixstop,iystart:iystop)=real_fill_value

  allocate(spixel_scan_out%sol_zen(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%sol_zen(ixstart:ixstop,iystart:iystop)=real_fill_value

  allocate(spixel_scan_out%rel_azi(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%rel_azi(ixstart:ixstop,iystart:iystop)=real_fill_value

  allocate(spixel_scan_out%cot(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cot(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%cot_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cot_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%ref(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%ref(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%ref_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%ref_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%ctp(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%ctp(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%ctp_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%ctp_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%cct(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cct(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%cct_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cct_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%cccot(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cccot(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%cccot_pre(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cccot_pre(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%stemp(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%stemp(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%stemp_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%stemp_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%ctt(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%ctt(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%ctt_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%ctt_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%cth(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cth(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%cth_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cth_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value


  allocate(spixel_scan_out%cwp(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cwp(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value
  allocate(spixel_scan_out%cwp_error(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cwp_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%cloud_albedo(ixstart:ixstop,iystart:iystop,1:nchan))
  spixel_scan_out%cloud_albedo(ixstart:ixstop,iystart:iystop,1:nchan)=spixel_scan_out%int_fill_value


  !allocate(spixel_scan_out%cloudflag(ixstart:ixstop))
  !spixel_scan_out%cloudflag(ixstart:ixstop)=spixel_scan_out%byte_fill_value


  allocate(spixel_scan_out%convergence(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%convergence(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%niter(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%niter(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%phase(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%phase(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%phase_pavolonis(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%phase_pavolonis(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%costja(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%costja(ixstart:ixstop,iystart:iystop)=spixel_scan_out%real_fill_value

  allocate(spixel_scan_out%costjm(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%costjm(ixstart:ixstop,iystart:iystop)=spixel_scan_out%real_fill_value

  allocate(spixel_scan_out%lsflag(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%lsflag(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%lusflag(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%lusflag(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%qcflag(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%qcflag(ixstart:ixstop,iystart:iystop)=real_fill_value

  allocate(spixel_scan_out%illum(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%illum(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%cldtype(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cldtype(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  allocate(spixel_scan_out%cldmask(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%cldmask(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

  !allocate(spixel_scan_out%dem(ixstart:ixstop,iystart:iystop))
  !spixel_scan_out%dem(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

  allocate(spixel_scan_out%nisemask(ixstart:ixstop,iystart:iystop))
  spixel_scan_out%nisemask(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value


end subroutine alloc_spixel_scan_out_pp





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
!!$  spixel_scan_out%time(ixstart:ixstop)=real_fill_value
!!$
!!$  allocate(spixel_scan_out%lon(ixstart:ixstop))
!!$  spixel_scan_out%lon(ixstart:ixstop)=real_fill_value
!!$
!!$  allocate(spixel_scan_out%lat(ixstart:ixstop))
!!$  spixel_scan_out%lat(ixstart:ixstop)=real_fill_value
!!$
!!$  allocate(spixel_scan_out%sat_zen(ixstart:ixstop))
!!$  spixel_scan_out%sat_zen(ixstart:ixstop)=real_fill_value
!!$
!!$  allocate(spixel_scan_out%sol_zen(ixstart:ixstop))
!!$  spixel_scan_out%sol_zen(ixstart:ixstop)=real_fill_value
!!$
!!$  allocate(spixel_scan_out%rel_azi(ixstart:ixstop))
!!$  spixel_scan_out%rel_azi(ixstart:ixstop)=real_fill_value
!!$
!!$  allocate(spixel_scan_out%cot(ixstart:ixstop))
!!$  spixel_scan_out%cot(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$  allocate(spixel_scan_out%cot_error(ixstart:ixstop))
!!$  spixel_scan_out%cot_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%ref(ixstart:ixstop))
!!$  spixel_scan_out%ref(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$  allocate(spixel_scan_out%ref_error(ixstart:ixstop))
!!$  spixel_scan_out%ref_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%ctp(ixstart:ixstop))
!!$  spixel_scan_out%ctp(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$  allocate(spixel_scan_out%ctp_error(ixstart:ixstop))
!!$  spixel_scan_out%ctp_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%cct(ixstart:ixstop))
!!$  spixel_scan_out%cct(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$  allocate(spixel_scan_out%cct_error(ixstart:ixstop))
!!$  spixel_scan_out%cct_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%stemp(ixstart:ixstop))
!!$  spixel_scan_out%stemp(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$  allocate(spixel_scan_out%stemp_error(ixstart:ixstop))
!!$  spixel_scan_out%stemp_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%ctt(ixstart:ixstop))
!!$  spixel_scan_out%ctt(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%cth(ixstart:ixstop))
!!$  spixel_scan_out%cth(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%cwp(ixstart:ixstop))
!!$  spixel_scan_out%cwp(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$  allocate(spixel_scan_out%cwp_error(ixstart:ixstop))
!!$  spixel_scan_out%cwp_error(ixstart:ixstop)=spixel_scan_out%int_fill_value
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
!!$  spixel_scan_out%costja(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%costjm(ixstart:ixstop))
!!$  spixel_scan_out%costjm(ixstart:ixstop)=spixel_scan_out%int_fill_value
!!$
!!$  allocate(spixel_scan_out%lsflag(ixstart:ixstop))
!!$  spixel_scan_out%lsflag(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$
!!$  allocate(spixel_scan_out%cloudflag(ixstart:ixstop))
!!$  spixel_scan_out%cloudflag(ixstart:ixstop)=spixel_scan_out%byte_fill_value
!!$
!!$  allocate(spixel_scan_out%qcflag(ixstart:ixstop))
!!$  spixel_scan_out%qcflag(ixstart:ixstop)=spixel_scan_out%int_fill_value
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
!!$  spixel_scan_out_sec%scanline_u(ixstart:ixstop)=spixel_scan_out_sec%lint_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%scanline_v(ixstart:ixstop))
!!$  spixel_scan_out_sec%scanline_v(ixstart:ixstop)=spixel_scan_out_sec%lint_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%cot_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%cot_ap(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%cot_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%cot_fg(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%ref_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%ref_ap(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%ref_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%ref_fg(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
!!$
!!$
!!$  allocate(spixel_scan_out_sec%ctp_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%ctp_ap(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%ctp_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%ctp_fg(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%stemp_ap(ixstart:ixstop))
!!$  spixel_scan_out_sec%stemp_ap(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%stemp_fg(ixstart:ixstop))
!!$  spixel_scan_out_sec%stemp_fg(ixstart:ixstop)=spixel_scan_out_sec%int_fill_value
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
!!$  spixel_scan_out_sec%res_scale=real_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%res_offset(1:Ny))
!!$  spixel_scan_out_sec%res_offset=real_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%res_vmin(1:Ny))
!!$  spixel_scan_out_sec%res_vmin=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%res_vmax(1:Ny))
!!$  spixel_scan_out_sec%res_vmax=spixel_scan_out_sec%int_fill_value
!!$
!!$
!!$
!!$  allocate(spixel_scan_out_sec%chans_scale(1:Ny))
!!$  spixel_scan_out_sec%chans_scale=real_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%chans_offset(1:Ny))
!!$  spixel_scan_out_sec%chans_offset=real_fill_value
!!$
!!$
!!$  allocate(spixel_scan_out_sec%chans_vmin(1:Ny))
!!$  spixel_scan_out_sec%chans_vmin=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%chans_vmax(1:Ny))
!!$  spixel_scan_out_sec%chans_vmax=spixel_scan_out_sec%int_fill_value
!!$
!!$
!!$
!!$  allocate(spixel_scan_out_sec%y0_scale(1:Ny))
!!$  spixel_scan_out_sec%y0_scale=real_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%y0_offset(1:Ny))
!!$  spixel_scan_out_sec%y0_offset=real_fill_value
!!$
!!$
!!$  allocate(spixel_scan_out_sec%y0_vmin(1:Ny))
!!$  spixel_scan_out_sec%y0_vmin=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%y0_vmax(1:Ny))
!!$  spixel_scan_out_sec%y0_vmax=spixel_scan_out_sec%int_fill_value
!!$
!!$
!!$
!!$
!!$
!!$
!!$  allocate(spixel_scan_out_sec%residuals(ixstart:ixstop,1:Ny))
!!$  spixel_scan_out_sec%residuals(ixstart:ixstop,1:Ny)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%channels(ixstart:ixstop,1:Ny))
!!$  spixel_scan_out_sec%channels(ixstart:ixstop,1:Ny)=spixel_scan_out_sec%int_fill_value
!!$
!!$  allocate(spixel_scan_out_sec%y0(ixstart:ixstop,1:Ny))
!!$  spixel_scan_out_sec%y0(ixstart:ixstop,1:Ny)=spixel_scan_out_sec%int_fill_value
!!$
!!$  if(lcovar) then
!!$
!!$     allocate(spixel_scan_out_sec%vidcovar(Nx,Nx))
!!$     spixel_scan_out_sec%vidcovar=0
!!$
!!$     allocate(spixel_scan_out_sec%covariance(ixstart:ixstop,Nx,Nx))
!!$     spixel_scan_out_sec%covariance=real_fill_value
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
!!$  spixel_scan_in%input_scale=real_fill_value
!!$
!!$  allocate(spixel_scan_in%input_offset(Ny))
!!$  spixel_scan_in%input_offset=real_fill_value
!!$
!!$  allocate(spixel_scan_in%input_vmin(Ny))
!!$  spixel_scan_in%input_vmin=spixel_scan_in%int_fill_value
!!$
!!$  allocate(spixel_scan_in%input_vmax(Ny))
!!$  spixel_scan_in%input_vmax=spixel_scan_in%int_fill_value
!!$
!!$  allocate(spixel_scan_in%input(ixstart:ixstop,Ny))
!!$  spixel_scan_in%input(ixstart:ixstop,Ny)=spixel_scan_in%int_fill_value
!!$
!!$
!!$  allocate(spixel_scan_in%viderror(Ny))
!!$  spixel_scan_in%viderror=0
!!$
!!$  allocate(spixel_scan_in%error_scale(Ny))
!!$  spixel_scan_in%error_scale=real_fill_value
!!$
!!$  allocate(spixel_scan_in%error_offset(Ny))
!!$  spixel_scan_in%error_offset=real_fill_value
!!$
!!$  allocate(spixel_scan_in%error_vmin(Ny))
!!$  spixel_scan_in%error_vmin=spixel_scan_in%int_fill_value
!!$
!!$  allocate(spixel_scan_in%error_vmax(Ny))
!!$  spixel_scan_in%error_vmax=spixel_scan_in%int_fill_value
!!$
!!$  allocate(spixel_scan_in%error(ixstart:ixstop,Ny))
!!$  spixel_scan_in%error(ixstart:ixstop,Ny)=spixel_scan_in%int_fill_value
!!$
!!$end subroutine alloc_spixel_scan_in
