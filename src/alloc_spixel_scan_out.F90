!-------------------------------------------------------------------------------
! Name: alloc_spixel_scan_out.F90
!
! Purpose:
! The file contains a collection of three subroutines which allocate the array
! parts of the output variable types.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: creates initial file.
! 2012/01/05, Caroline Poulsen: added in channel information
! 2012/01/06, Caroline Poulsen: added in cwp
! 2012/01/15, Caroline Poulsen: added in chan definitions
! 2012/01/28, Caroline Poulsen: added in albedo
! 2012/07/08, Caroline Poulsen: fixed memory access error
! 2013/01/17, Matthias Jerg: Adds code to accommodate uncertainties of ctt and
!    cth.
! 2013/01/23, Caroline Poulsen: Changed illum from byte to int
! 2013/10/02, CP/GT: Added allocation statement for DOFS
! 2014/01/01, Greg McGarragh: Fixed the range in NY for initializations. Plus,
!    no need for explicit indexing in these cases anyway.
! 2014/05/27, Greg McGarragh: Some cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine alloc_spixel_scan_out(ixstart,ixstop,iystart,iystop,NViews, &
                                 spixel_scan_out)

   use ECP_Constants

   implicit none

   integer,                              intent(in)    :: ixstart
   integer,                              intent(in)    :: ixstop
   integer,                              intent(in)    :: iystart
   integer,                              intent(in)    :: iystop
   integer,                              intent(in)    :: NViews
   type(spixel_scanline_primary_output), intent(inout) :: spixel_scan_out


   allocate(spixel_scan_out%vidsat_zen(NViews))
   spixel_scan_out%vidsat_zen=0

   allocate(spixel_scan_out%vidsol_zen(NViews))
   spixel_scan_out%vidsol_zen=0

   allocate(spixel_scan_out%vidrel_azi(NViews))
   spixel_scan_out%vidrel_azi=0


   allocate(spixel_scan_out%time(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%time(ixstart:ixstop,iystart:iystop)=spixel_scan_out%real_fill_value


   allocate(spixel_scan_out%lon(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%lon(ixstart:ixstop,iystart:iystop)=spixel_scan_out%real_fill_value

   allocate(spixel_scan_out%lat(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%lat(ixstart:ixstop,iystart:iystop)=spixel_scan_out%real_fill_value


   allocate(spixel_scan_out%sat_zen(ixstart:ixstop,iystart:iystop,NViews))
   spixel_scan_out%sat_zen(ixstart:ixstop,iystart:iystop,NViews)=spixel_scan_out%real_fill_value

   allocate(spixel_scan_out%sol_zen(ixstart:ixstop,iystart:iystop,NViews))
   spixel_scan_out%sol_zen(ixstart:ixstop,iystart:iystop,NViews)=spixel_scan_out%real_fill_value

   allocate(spixel_scan_out%rel_azi(ixstart:ixstop,iystart:iystop,NViews))
   spixel_scan_out%rel_azi(ixstart:ixstop,iystart:iystop,NViews)=spixel_scan_out%real_fill_value


   allocate(spixel_scan_out%cot(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cot(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%ref(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ref(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%ctp(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctp(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%cct(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cct(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%stemp(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%stemp(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value


   allocate(spixel_scan_out%ctt(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctt(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%cth(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cth(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%cwp(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cwp(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value


   allocate(spixel_scan_out%cot_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cot_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%ref_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ref_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%ctp_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctp_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%cct_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cct_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%stemp_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%stemp_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value


   allocate(spixel_scan_out%ctt_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%ctt_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%cth_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cth_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%cwp_error(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%cwp_error(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value


   allocate(spixel_scan_out%convergence(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%convergence(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

   allocate(spixel_scan_out%niter(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%niter(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

   allocate(spixel_scan_out%pchange(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%pchange(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

   allocate(spixel_scan_out%costja(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%costja(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%costjm(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%costjm(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%lsflag(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%lsflag(ixstart:ixstop,iystart:iystop)=spixel_scan_out%byte_fill_value

   allocate(spixel_scan_out%qcflag(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%qcflag(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

   allocate(spixel_scan_out%illum(ixstart:ixstop,iystart:iystop))
   spixel_scan_out%illum(ixstart:ixstop,iystart:iystop)=spixel_scan_out%int_fill_value

end subroutine alloc_spixel_scan_out


subroutine alloc_spixel_scan_out_sec(ixstart,ixstop,iystart,iystop,Ny,Nx,lcovar, &
                                     spixel_scan_out_sec)

   use ECP_Constants

   implicit none

   integer,                                intent(in)    :: ixstart
   integer,                                intent(in)    :: ixstop
   integer,                                intent(in)    :: iystart
   integer,                                intent(in)    :: iystop
   integer,                                intent(in)    :: Ny
   integer,                                intent(in)    :: Nx
   logical,                                intent(inout) :: lcovar
   type(spixel_scanline_secondary_output), intent(inout) :: spixel_scan_out_sec


   allocate(spixel_scan_out_sec%vidres(Ny))
   spixel_scan_out_sec%vidres=0

   allocate(spixel_scan_out_sec%vidchans(Ny))
   spixel_scan_out_sec%vidchans=0

   allocate(spixel_scan_out_sec%vidalb(Ny))
   spixel_scan_out_sec%vidalb=0

   allocate(spixel_scan_out_sec%vidy0(Ny))
   spixel_scan_out_sec%vidy0=0


   allocate(spixel_scan_out_sec%res_scale(Ny))
   spixel_scan_out_sec%res_scale=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%res_offset(Ny))
   spixel_scan_out_sec%res_offset=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%res_vmin(Ny))
   spixel_scan_out_sec%res_vmin=spixel_scan_out_sec%int_fill_value
   allocate(spixel_scan_out_sec%res_vmax(Ny))
   spixel_scan_out_sec%res_vmax=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%chans_scale(Ny))
   spixel_scan_out_sec%chans_scale=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%chans_offset(Ny))
   spixel_scan_out_sec%chans_offset=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%chans_vmin(Ny))
   spixel_scan_out_sec%chans_vmin=spixel_scan_out_sec%int_fill_value
   allocate(spixel_scan_out_sec%chans_vmax(Ny))
   spixel_scan_out_sec%chans_vmax=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%alb_scale(Ny))
   spixel_scan_out_sec%alb_scale=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%alb_offset(Ny))
   spixel_scan_out_sec%alb_offset=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%alb_vmin(Ny))
   spixel_scan_out_sec%alb_vmin=spixel_scan_out_sec%int_fill_value
   allocate(spixel_scan_out_sec%alb_vmax(Ny))
   spixel_scan_out_sec%alb_vmax=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%y0_scale(Ny))
   spixel_scan_out_sec%y0_scale=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%y0_offset(Ny))
   spixel_scan_out_sec%y0_offset=spixel_scan_out_sec%real_fill_value
   allocate(spixel_scan_out_sec%y0_vmin(Ny))
   spixel_scan_out_sec%y0_vmin=spixel_scan_out_sec%int_fill_value
   allocate(spixel_scan_out_sec%y0_vmax(Ny))
   spixel_scan_out_sec%y0_vmax=spixel_scan_out_sec%int_fill_value


   allocate(spixel_scan_out_sec%scanline_u(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%scanline_u(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%lint_fill_value

   allocate(spixel_scan_out_sec%scanline_v(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%scanline_v(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%lint_fill_value


   allocate(spixel_scan_out_sec%cot_ap(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%cot_ap(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value
   allocate(spixel_scan_out_sec%cot_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%cot_fg(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%ref_ap(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%ref_ap(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value
   allocate(spixel_scan_out_sec%ref_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%ref_fg(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%ctp_ap(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%ctp_ap(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value
   allocate(spixel_scan_out_sec%ctp_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%ctp_fg(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%stemp_fg(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%stemp_fg(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value


   allocate(spixel_scan_out_sec%residuals(ixstart:ixstop,iystart:iystop,Ny))
   spixel_scan_out_sec%residuals=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%channels(ixstart:ixstop,iystart:iystop,Ny))
   spixel_scan_out_sec%channels=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%albedo(ixstart:ixstop,iystart:iystop,Ny))
   spixel_scan_out_sec%albedo=spixel_scan_out_sec%int_fill_value

   allocate(spixel_scan_out_sec%y0(ixstart:ixstop,iystart:iystop,Ny))
   spixel_scan_out_sec%y0=spixel_scan_out_sec%int_fill_value

   if (lcovar) then
      allocate(spixel_scan_out_sec%vidcovar(Nx,Nx))
      spixel_scan_out_sec%vidcovar=0

      allocate(spixel_scan_out_sec%covariance(ixstart:ixstop,iystart:iystop,Nx,Nx))
      spixel_scan_out_sec%covariance=spixel_scan_out_sec%real_fill_value
   end if

   allocate(spixel_scan_out_sec%ds(ixstart:ixstop,iystart:iystop))
   spixel_scan_out_sec%ds(ixstart:ixstop,iystart:iystop)=spixel_scan_out_sec%int_fill_value

end subroutine alloc_spixel_scan_out_sec


subroutine alloc_spixel_scan_in(ixstart,ixstop,Ny,spixel_scan_in)

   use ECP_Constants

   implicit none

   integer,                     intent(in)    :: ixstart
   integer,                     intent(in)    :: ixstop
   integer,                     intent(in)    :: Ny
   type(spixel_scanline_input), intent(inout) :: spixel_scan_in


   allocate(spixel_scan_in%vidinput(Ny))
   spixel_scan_in%vidinput=0
   allocate(spixel_scan_in%viderror(Ny))
   spixel_scan_in%viderror=0

   allocate(spixel_scan_in%input_scale(Ny))
   spixel_scan_in%input_scale=spixel_scan_in%real_fill_value
   allocate(spixel_scan_in%input_offset(Ny))
   spixel_scan_in%input_offset=spixel_scan_in%real_fill_value
   allocate(spixel_scan_in%input_vmin(Ny))
   spixel_scan_in%input_vmin=spixel_scan_in%int_fill_value
   allocate(spixel_scan_in%input_vmax(Ny))
   spixel_scan_in%input_vmax=spixel_scan_in%int_fill_value

   allocate(spixel_scan_in%error_scale(Ny))
   spixel_scan_in%error_scale=spixel_scan_in%real_fill_value
   allocate(spixel_scan_in%error_offset(Ny))
   spixel_scan_in%error_offset=spixel_scan_in%real_fill_value
   allocate(spixel_scan_in%error_vmin(Ny))
   spixel_scan_in%error_vmin=spixel_scan_in%int_fill_value
   allocate(spixel_scan_in%error_vmax(Ny))
   spixel_scan_in%error_vmax=spixel_scan_in%int_fill_value

   allocate(spixel_scan_in%input(ixstart:ixstop,Ny))
   spixel_scan_in%input=spixel_scan_in%int_fill_value

   allocate(spixel_scan_in%error(ixstart:ixstop,Ny))
   spixel_scan_in%error=spixel_scan_in%int_fill_value

end subroutine alloc_spixel_scan_in
