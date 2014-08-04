!-------------------------------------------------------------------------------
! Name: prepare_primary.F90
!
! Purpose:
! Map internal representation of variables to output representation by applying
! scale and offset where necessary.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Return value:
! Name Type Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: Creates initial version
! 2012/01/06, Matthias Jerg: Added in cwp
! 2012/01/16, Caroline Poulsen: Bug fix, changed how offset applied
! 2012/06/16, Caroline Poulsen: Change illum arry size
! 2012/07/17, MJ: Fixes bug in CWP write.
! 2012/08/10, CP: Fixed bug in illum read 2d array instead of 3d
! 2012/11/03, MST: Converted height to km
! 2012/11/03, CP: Changed log cot to COT using 10*
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2013/xx/xx, MJ: Make various modifications to define range of variables more
!    thoroughly.
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_primary(Ctrl, conv, i, j, MSI_Data, RTM_Pc, SPixel, Diag, &
                           spixel_scan_out, status)

   use CTRL_def
   use Diag_def
   use Data_def
   use RTM_Pc_def
   use SPixel_def

   implicit none

   type(CTRL_t),                         intent(in)    :: Ctrl
   integer,                              intent(in)    :: conv
   integer,                              intent(in)    :: i, j
   type(Data_t),                         intent(in)    :: MSI_Data
   type(RTM_Pc_t),                       intent(in)    :: RTM_Pc
   type(SPixel_t),                       intent(in)    :: SPixel
   type(Diag_t),                         intent(in)    :: Diag
   type(spixel_scanline_primary_output), intent(inout) :: spixel_scan_out
   integer,                              intent(inout) :: status

   integer          :: iviews
   real(kind=sreal) :: dummyreal, dummyreal_store, &
                       minvalue=100000.0,maxvalue=-100000.0

   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   spixel_scan_out%time(i,j)=MSI_Data%time(SPixel%Loc%X0, SPixel%Loc%YSeg0)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   spixel_scan_out%illum(i,j)=MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%YSeg0,1)

   !----------------------------------------------------------------------------
   ! lon/lat
   !----------------------------------------------------------------------------
   spixel_scan_out%lon(i,j)=SPixel%Loc%Lon/spixel_scan_out%lon_scale
   spixel_scan_out%lat(i,j)=SPixel%Loc%Lat/spixel_scan_out%lat_scale

   !----------------------------------------------------------------------------
   ! sat_zen, sol_zen, rel_azi
   !----------------------------------------------------------------------------
   do iviews=1,Ctrl%Ind%NViews
      spixel_scan_out%sat_zen(i,j,iviews)=MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%YSeg0,iviews)
      spixel_scan_out%sol_zen(i,j,iviews)=MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%YSeg0,iviews)
      spixel_scan_out%rel_azi(i,j,iviews)=MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%YSeg0,iviews)
   enddo

   !-------------------------------------------------------------------------------
   ! State variables and covariance diagonal
   !-------------------------------------------------------------------------------

   !-------------------------------------------------------------------------------
   ! cot
   !-------------------------------------------------------------------------------
   dummyreal=(10.0**SPixel%Xn(1)-spixel_scan_out%cot_offset)/spixel_scan_out%cot_scale

   minvalue=min(SPixel%Xn(1),minvalue)
   maxvalue=max(SPixel%Xn(1),maxvalue)

   if (dummyreal .ge. real(spixel_scan_out%cot_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%cot_vmax,kind=sreal)) then
      spixel_scan_out%cot(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .lt. real(spixel_scan_out%cot_vmin,kind=sreal)) then
      spixel_scan_out%cot(i,j)=spixel_scan_out%int_fill_value
   elseif (dummyreal .gt. real(spixel_scan_out%cot_vmax,kind=sreal)) then
      spixel_scan_out%cot(i,j)=spixel_scan_out%cot_vmax
   endif

   dummyreal=(sqrt(SPixel%Sn(1,1))-spixel_scan_out%cot_error_offset)/spixel_scan_out%cot_error_scale
   if (dummyreal .ge. real(spixel_scan_out%cot_error_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%cot_error_vmax,kind=sreal)) then
      spixel_scan_out%cot_error(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .lt. real(spixel_scan_out%cot_error_vmin,kind=sreal)) then
      spixel_scan_out%cot_error(i,j)=spixel_scan_out%int_fill_value
   elseif (dummyreal .gt. real(spixel_scan_out%cot_error_vmax,kind=sreal)) then
      spixel_scan_out%cot_error(i,j)=spixel_scan_out%cot_error_vmax
   endif

   !-------------------------------------------------------------------------------
   ! ref
   !-------------------------------------------------------------------------------
   dummyreal=(SPixel%Xn(2)-spixel_scan_out%ref_offset)/spixel_scan_out%ref_scale
   if (dummyreal .ge. real(spixel_scan_out%ref_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%ref_vmax,kind=sreal)) then
      spixel_scan_out%ref(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%ref_vmax,kind=sreal)) then
      spixel_scan_out%ref(i,j)=spixel_scan_out%ref_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%ref_vmin,kind=sreal)) then
      spixel_scan_out%ref(i,j)=spixel_scan_out%int_fill_value
   endif

   dummyreal=(sqrt(SPixel%Sn(2,2))-spixel_scan_out%ref_error_offset)/spixel_scan_out%ref_error_scale
   if (dummyreal .ge. real(spixel_scan_out%ref_error_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%ref_error_vmax,kind=sreal)) then
      spixel_scan_out%ref_error(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%ref_error_vmax,kind=sreal)) then
      spixel_scan_out%ref_error(i,j)=spixel_scan_out%ref_error_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%ref_error_vmin,kind=sreal)) then
      spixel_scan_out%ref_error(i,j)=spixel_scan_out%int_fill_value
   endif

   !-------------------------------------------------------------------------------
   ! ctp
   !-------------------------------------------------------------------------------
   dummyreal=(SPixel%Xn(3)-spixel_scan_out%ctp_offset)/spixel_scan_out%ctp_scale
   if (dummyreal .ge. real(spixel_scan_out%ctp_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%ctp_vmax,kind=sreal)) then
      spixel_scan_out%ctp(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%ctp_vmax,kind=sreal)) then
      spixel_scan_out%ctp(i,j)=spixel_scan_out%ctp_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%ctp_vmin,kind=sreal)) then
      spixel_scan_out%ctp(i,j)=spixel_scan_out%int_fill_value
   endif

   dummyreal_store=(sqrt(SPixel%Sn(3,3))-spixel_scan_out%ctp_error_offset)/spixel_scan_out%ctp_error_scale

   ! If ctp_error is good compute cth_error and ctt_error
   if (dummyreal_store .ge. real(spixel_scan_out%ctp_error_vmin,kind=sreal) .and. &
       dummyreal_store .le. real(spixel_scan_out%ctp_error_vmax,kind=sreal)) then

      spixel_scan_out%ctp_error(i,j)=int(dummyreal_store, kind=sint)

      dummyreal=abs(RTM_Pc%dHc_dPc/10./1000.)*dummyreal_store
      dummyreal=(dummyreal-spixel_scan_out%cth_error_offset)/spixel_scan_out%cth_error_scale

      if (dummyreal .ge. real(spixel_scan_out%cth_error_vmin,kind=sreal) .and. &
          dummyreal .le. real(spixel_scan_out%cth_error_vmax,kind=sreal)) then
         spixel_scan_out%cth_error(i,j)=int(dummyreal, kind=sint)
      elseif (dummyreal .gt. real(spixel_scan_out%cth_error_vmax,kind=sreal)) then
         spixel_scan_out%cth_error(i,j)=spixel_scan_out%cth_error_vmax
      elseif (dummyreal .lt. real(spixel_scan_out%cth_error_vmin,kind=sreal)) then
         spixel_scan_out%cth_error(i,j)=spixel_scan_out%int_fill_value
      endif

      dummyreal=abs(RTM_Pc%dTc_dPc)*dummyreal_store
      dummyreal=(dummyreal-spixel_scan_out%ctt_error_offset)/spixel_scan_out%ctt_error_scale

      if (dummyreal .ge. real(spixel_scan_out%ctt_error_vmin,kind=sreal) .and. &
          dummyreal .le. real(spixel_scan_out%ctt_error_vmax,kind=sreal)) then
         spixel_scan_out%ctt_error(i,j)=int(dummyreal, kind=sint)
      elseif (dummyreal .gt. real(spixel_scan_out%ctt_error_vmax,kind=sreal)) then
         spixel_scan_out%ctt_error(i,j)=spixel_scan_out%ctt_error_vmax
      elseif (dummyreal .lt. real(spixel_scan_out%ctt_error_vmin,kind=sreal)) then
         spixel_scan_out%ctt_error(i,j)=spixel_scan_out%int_fill_value
      endif

   elseif (dummyreal_store .gt. real(spixel_scan_out%ctp_error_vmax,kind=sreal)) then

      spixel_scan_out%ctp_error(i,j)=spixel_scan_out%ctp_error_vmax
      spixel_scan_out%cth_error(i,j)=spixel_scan_out%cth_error_vmax
      spixel_scan_out%ctt_error(i,j)=spixel_scan_out%ctt_error_vmax

   elseif (dummyreal_store .lt. real(spixel_scan_out%ctp_error_vmin,kind=sreal)) then
      spixel_scan_out%ctp_error(i,j)=spixel_scan_out%int_fill_value
      spixel_scan_out%cth_error(i,j)=spixel_scan_out%int_fill_value
      spixel_scan_out%ctt_error(i,j)=spixel_scan_out%int_fill_value

   endif

   !-------------------------------------------------------------------------------
   ! cct
   !-------------------------------------------------------------------------------
   dummyreal=(SPixel%Xn(4)-spixel_scan_out%cct_offset)/spixel_scan_out%cct_scale
   if (dummyreal .ge. real(spixel_scan_out%cct_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%cct_vmax,kind=sreal)) then
      spixel_scan_out%cct(i,j)=int(dummyreal, kind=sint)
   else
      spixel_scan_out%cct(i,j)=spixel_scan_out%int_fill_value
   endif

   dummyreal=(sqrt(SPixel%Sn(4,4))-spixel_scan_out%cct_error_offset)/spixel_scan_out%cct_error_scale
   if (dummyreal .ge. real(spixel_scan_out%cct_error_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%cct_error_vmax,kind=sreal)) then
      spixel_scan_out%cct_error(i,j)=int(dummyreal, kind=sint)
   else
      spixel_scan_out%cct_error(i,j)=spixel_scan_out%int_fill_value
   endif

   !-------------------------------------------------------------------------------
   ! stemp
   !-------------------------------------------------------------------------------
   dummyreal=(SPixel%Xn(5)-spixel_scan_out%stemp_offset)/spixel_scan_out%stemp_scale
   if (dummyreal .ge. real(spixel_scan_out%stemp_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%stemp_vmax,kind=sreal)) then
      spixel_scan_out%stemp(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%stemp_vmax,kind=sreal)) then
      spixel_scan_out%stemp(i,j)=spixel_scan_out%stemp_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%stemp_vmin,kind=sreal)) then
      spixel_scan_out%stemp(i,j)=spixel_scan_out%int_fill_value
   endif

   dummyreal=(sqrt(SPixel%Sn(5,5))-spixel_scan_out%stemp_error_offset)/spixel_scan_out%stemp_error_scale
   if (dummyreal .ge. real(spixel_scan_out%stemp_error_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%stemp_error_vmax,kind=sreal)) then
      spixel_scan_out%stemp_error(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%stemp_error_vmax,kind=sreal)) then
      spixel_scan_out%stemp_error(i,j)=spixel_scan_out%stemp_error_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%stemp_error_vmin,kind=sreal)) then
      spixel_scan_out%stemp_error(i,j)=spixel_scan_out%int_fill_value
   endif


   !-------------------------------------------------------------------------------
   ! No real state variables from here
   !-------------------------------------------------------------------------------

   !-------------------------------------------------------------------------------
   ! cth
   !-------------------------------------------------------------------------------
   dummyreal=RTM_Pc%Hc/10./1000. ! now it's in km
   dummyreal=(dummyreal-spixel_scan_out%cth_offset)/spixel_scan_out%cth_scale
   if (dummyreal .ge. real(spixel_scan_out%cth_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%cth_vmax,kind=sreal)) then
      spixel_scan_out%cth(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%cth_vmax,kind=sreal)) then
      spixel_scan_out%cth(i,j)=spixel_scan_out%cth_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%cth_vmin,kind=sreal)) then
      spixel_scan_out%cth(i,j)=spixel_scan_out%int_fill_value
   endif

   !-------------------------------------------------------------------------------
   ! ctt
   !-------------------------------------------------------------------------------
   dummyreal=(RTM_Pc%Tc-spixel_scan_out%ctt_offset)/spixel_scan_out%ctt_scale
   if (dummyreal .ge. real(spixel_scan_out%ctt_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%ctt_vmax,kind=sreal)) then
      spixel_scan_out%ctt(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%ctt_vmax,kind=sreal)) then
      spixel_scan_out%ctt(i,j)=spixel_scan_out%ctt_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%ctt_vmin,kind=sreal)) then
      spixel_scan_out%ctt(i,j)=spixel_scan_out%int_fill_value
   endif

   !-------------------------------------------------------------------------------
   ! cwp
   !-------------------------------------------------------------------------------
   dummyreal=(SPixel%CWP-spixel_scan_out%cwp_offset)/spixel_scan_out%cwp_scale
   if (dummyreal .ge. real(spixel_scan_out%cwp_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%cwp_vmax,kind=sreal)) then
      spixel_scan_out%cwp(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%cwp_vmax,kind=sreal)) then
      spixel_scan_out%cwp(i,j)=spixel_scan_out%cwp_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%cwp_vmin,kind=sreal)) then
      spixel_scan_out%cwp(i,j)=spixel_scan_out%int_fill_value
   endif

   dummyreal=(sqrt(SPixel%CWP_error)-spixel_scan_out%cwp_error_offset)/spixel_scan_out%cwp_error_scale
   if (dummyreal .ge. real(spixel_scan_out%cwp_error_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%cwp_error_vmax,kind=sreal)) then
      spixel_scan_out%cwp_error(i,j)=int(dummyreal, kind=sint)
   elseif (dummyreal .gt. real(spixel_scan_out%cwp_error_vmax,kind=sreal)) then
      spixel_scan_out%cwp_error(i,j)=spixel_scan_out%cwp_error_vmax
   elseif (dummyreal .lt. real(spixel_scan_out%cwp_error_vmin,kind=sreal)) then
      spixel_scan_out%cwp_error(i,j)=spixel_scan_out%int_fill_value
   endif

   !-------------------------------------------------------------------------------
   ! conv, niter, pchange
   !-------------------------------------------------------------------------------
   spixel_scan_out%convergence(i,j)=int(conv,kind=byte)

   if (conv .eq. 0 ) spixel_scan_out%niter(i,j)=int(Diag%Iterations,kind=byte)
   if (conv .eq. 1 ) spixel_scan_out%niter(i,j)=int(spixel_scan_out%byte_fill_value,kind=byte)

   spixel_scan_out%pchange(i,j)=int(1,kind=byte)

   !-------------------------------------------------------------------------------
   ! costjm
   !-------------------------------------------------------------------------------
   dummyreal=real((Diag%Jm-spixel_scan_out%costjm_offset)/spixel_scan_out%costjm_scale,kind=sreal)
   if (dummyreal .ge. real(spixel_scan_out%costjm_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%costjm_vmax,kind=sreal)) then
      spixel_scan_out%costjm(i,j)=real(dummyreal,kind=sreal)
   else
      spixel_scan_out%costjm(i,j)=spixel_scan_out%real_fill_value
   endif

   !-------------------------------------------------------------------------------
   ! costja
   !-------------------------------------------------------------------------------
   dummyreal=real((Diag%Ja-spixel_scan_out%costja_offset)/spixel_scan_out%costja_scale,kind=sreal)
   if (dummyreal .ge. real(spixel_scan_out%costja_vmin,kind=sreal) .and. &
       dummyreal .le. real(spixel_scan_out%costja_vmax,kind=sreal)) then
      spixel_scan_out%costja(i,j)=real(dummyreal,kind=sreal)
   else
      spixel_scan_out%costja(i,j)=spixel_scan_out%real_fill_value
   endif

   !-------------------------------------------------------------------------------
   ! lsflag
   !-------------------------------------------------------------------------------
   spixel_scan_out%lsflag(i,j)=int(MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0), kind=byte)

   !-------------------------------------------------------------------------------
   ! qcflag
   !-------------------------------------------------------------------------------
   spixel_scan_out%qcflag(i,j)=int(Diag%QCFlag,kind=sint)

end subroutine prepare_primary
