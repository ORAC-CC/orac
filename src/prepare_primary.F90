! Name: prepare_primary.inc
!
!
! Purpose:
! Map internal representation of variables to output representation by applying scale and offset where necessary.
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
!2011/12/19: Matthias Jerg creates initial output for main ooutput variables.
!2012/01/06: Matthias Jerg added in cwp
!2012/01/16: Caroline Poulsen bug fix: changed how offset applied
!2012/06/16: Caroline Poulsen change illum arry size
! 2012/07/17 MJ fixes bug in CWP write.
! 2012/08/10 CP fixed bug in illum read 2d array instead of 3d
! 2012/11/03 MST converted height to km
! 2012/11/03 Cp changed log cot to COT using 10*
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
!2013 MJ make various modifications to define range of variables more thoroughly.
!2014/06/13, GM: Put the code into a subroutine.
!
! $Id$
!
! Bugs:
!
!none known

subroutine prepare_primary(Ctrl, conv, i, j, MSI_Data, RTM_Pc, SPixel, Diag, spixel_scan_out, status)

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

   integer           :: iviews
   real(kind=sreal)  :: dummyreal, dummyreal_store, &
                        minvalue=100000.0,maxvalue=-100000.0

               !-------------------------------------
               !-------------------------------------
               !TIME: this needs to be implemented once preprocessing is in fortran
               !-------------------------------------
               !-------------------------------------
               spixel_scan_out%time(i,j)=MSI_Data%time(SPixel%Loc%X0, SPixel%Loc%YSeg0)




               !-------------------------------------
               !-------------------------------------
               !illumination: 
               !-------------------------------------
               !-------------------------------------
                 spixel_scan_out%illum(i,j)=MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%YSeg0,1)


               !-------------------------------------
               !-------------------------------------
               !GEOLOCATION
               !-------------------------------------
               !-------------------------------------
               spixel_scan_out%lon(i,j)=SPixel%Loc%Lon/spixel_scan_out%lon_scale
               spixel_scan_out%lat(i,j)=SPixel%Loc%Lat/spixel_scan_out%lat_scale

               !-------------------------------------
               !-------------------------------------
               !ANGLES
               !-------------------------------------
               !-------------------------------------
               do iviews=1,Ctrl%Ind%NViews

                  spixel_scan_out%sat_zen(i,j,iviews)=MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%YSeg0,iviews)
                  spixel_scan_out%sol_zen(i,j,iviews)=MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%YSeg0,iviews)
                  spixel_scan_out%rel_azi(i,j,iviews)=MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%YSeg0,iviews)

               enddo

!               write(*,*) '1',MSI_Data%Geometry%Sat(SPixel%Loc%X0, SPixel%Loc%YSeg0, :)
!               write(*,*) '2',MSI_Data%Geometry%Sol(SPixel%Loc%X0, SPixel%Loc%YSeg0, :)
!               write(*,*) '3',MSI_Data%Geometry%Azi(SPixel%Loc%X0, SPixel%Loc%YSeg0, :)

!               write(*,*) 'write vals'

!               write(*,*) 'write cot'
               
               !-------------------------------------
               !-------------------------------------
               !STATE VARIABLES and COVARIANCE DIAGONAL
               !-------------------------------------
               !-------------------------------------



               !-------------------------------------
               !COT
               !-------------------------------------
               dummyreal=(10.0**SPixel%Xn(1)-spixel_scan_out%cot_offset)/spixel_scan_out%cot_scale

!print*,dummyreal
!print*,'lonlatdat',i,j
!print*,'asu',i,j,spixel_scan_out%lon(i,j),spixel_scan_out%lat(i,j),dummyreal

               minvalue=min(SPixel%Xn(1),minvalue)
               maxvalue=max(SPixel%Xn(1),maxvalue)
!               write(*,*) SPixel%Xn(1)
               if( dummyreal .ge. real(spixel_scan_out%cot_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cot_vmax,kind=sreal)) then
                  spixel_scan_out%cot(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .lt. real(spixel_scan_out%cot_vmin,kind=sreal)) then
	spixel_scan_out%cot(i,j)=spixel_scan_out%int_fill_value
	!                  spixel_scan_out%cot(i,j)=spixel_scan_out%cot_vmin
	elseif(dummyreal .gt. real(spixel_scan_out%cot_vmax,kind=sreal)) then
	  spixel_scan_out%cot(i,j)=spixel_scan_out%cot_vmax
               endif

!!$               if(SPixel%Xn(1) .lt. MissingXn ) then
!!$                  spixel_scan_out%cot(i,j)=int(SPixel%Xn(1)/spixel_scan_out%cot_scale-spixel_scan_out%cot_offset, kind=sint)
!!$                  if(spixel_scan_out%cot(i,j) .gt. spixel_scan_out%cot_vmax .or. &
!!$                       & spixel_scan_out%cot(i,j) .lt. spixel_scan_out%cot_vmin) then
!!$                     spixel_scan_out%cot(i,j)=spixel_scan_out%int_fill_value
!!$                  endif
!!$               else
!!$                  spixel_scan_out%cot(i,j)=spixel_scan_out%int_fill_value
!!$               endif


                  
!               write(*,*) 'write cot_error'

!               write(*,*) SPixel%Sn(1,1)

               dummyreal=(sqrt(SPixel%Sn(1,1))-spixel_scan_out%cot_error_offset)/spixel_scan_out%cot_error_scale
!               write(*,*) 'cot',SPixel%Xn(1)
!               write(*,*) 'cot dummyreal', dummyreal
               if( dummyreal .ge. real(spixel_scan_out%cot_error_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cot_error_vmax,kind=sreal)) then
                  spixel_scan_out%cot_error(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .lt. real(spixel_scan_out%cot_error_vmin,kind=sreal)) then
                  spixel_scan_out%cot_error(i,j)=spixel_scan_out%int_fill_value
                  !MJ spixel_scan_out%cot_error(i,j)=spixel_scan_out%cot_error_vmin
               elseif(dummyreal .gt. real(spixel_scan_out%cot_error_vmax,kind=sreal)) then
                  !MJ spixel_scan_out%cot_error(i,j)=spixel_scan_out%int_fill_value
                  spixel_scan_out%cot_error(i,j)=spixel_scan_out%cot_error_vmax
               endif

!               write(*,*) 'write ref'


               !-------------------------------------
               !REFF
               !-------------------------------------
               dummyreal=(SPixel%Xn(2)-spixel_scan_out%ref_offset)/spixel_scan_out%ref_scale
!               write(*,*) SPixel%Xn(1)
               if( dummyreal .ge. real(spixel_scan_out%ref_vmin,kind=sreal) .and. &
                   & dummyreal .le. real(spixel_scan_out%ref_vmax,kind=sreal)) then
                  spixel_scan_out%ref(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%ref_vmax,kind=sreal)) then
                  !MJ spixel_scan_out%ref(i,j)=spixel_scan_out%int_fill_value
	spixel_scan_out%ref(i,j)=spixel_scan_out%ref_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%ref_vmin,kind=sreal)) then
                 spixel_scan_out%ref(i,j)=spixel_scan_out%int_fill_value
	!MJ	spixel_scan_out%ref(i,j)=spixel_scan_out%ref_vmin
               endif

               dummyreal=(sqrt(SPixel%Sn(2,2))-spixel_scan_out%ref_error_offset)/spixel_scan_out%ref_error_scale
               if( dummyreal .ge. real(spixel_scan_out%ref_error_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%ref_error_vmax,kind=sreal)) then
                  spixel_scan_out%ref_error(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%ref_error_vmax,kind=sreal)) then
                  !MJ spixel_scan_out%ref_error(i,j)=spixel_scan_out%int_fill_value
                 spixel_scan_out%ref_error(i,j)=spixel_scan_out%ref_error_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%ref_error_vmin,kind=sreal)) then
                 spixel_scan_out%ref_error(i,j)=spixel_scan_out%int_fill_value
                 !MJ spixel_scan_out%ref_error(i,j)=spixel_scan_out%ref_error_vmin
               endif

!               write(*,*) 'write ctp'

               !-------------------------------------
               !CTP
               !-------------------------------------
               dummyreal=(SPixel%Xn(3)-spixel_scan_out%ctp_offset)/spixel_scan_out%ctp_scale
               if( dummyreal .ge. real(spixel_scan_out%ctp_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%ctp_vmax,kind=sreal)) then
                  spixel_scan_out%ctp(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%ctp_vmax,kind=sreal)) then
                  !MJ spixel_scan_out%ctp(i,j)=spixel_scan_out%int_fill_value
                  spixel_scan_out%ctp(i,j)=spixel_scan_out%ctp_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%ctp_vmin,kind=sreal)) then
                  spixel_scan_out%ctp(i,j)=spixel_scan_out%int_fill_value
                  !MJ spixel_scan_out%ctp(i,j)=spixel_scan_out%ctp_vmin
               endif

	!print*,'ctp and ctperror',SPixel%Illum,SPixel%Xn(3),sqrt(SPixel%Sn(3,3))

               dummyreal_store=(sqrt(SPixel%Sn(3,3))-spixel_scan_out%ctp_error_offset)/spixel_scan_out%ctp_error_scale
               if( dummyreal_store .ge. real(spixel_scan_out%ctp_error_vmin,kind=sreal) .and. &
                    & dummyreal_store .le. real(spixel_scan_out%ctp_error_vmax,kind=sreal)) then
                  spixel_scan_out%ctp_error(i,j)=int(dummyreal_store, kind=sint)

	!if del_ctp good compute del_cth,del_ctt

	!del_cth
               dummyreal=abs(RTM_Pc%dHc_dPc/10./1000.)*dummyreal_store
               dummyreal=(dummyreal-spixel_scan_out%cth_error_offset)/spixel_scan_out%cth_error_scale
               if( dummyreal .ge. real(spixel_scan_out%cth_error_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cth_error_vmax,kind=sreal)) then
                  spixel_scan_out%cth_error(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%cth_error_vmax,kind=sreal)) then
                  !MJ spixel_scan_out%cth_error(i,j)=spixel_scan_out%int_fill_value
	spixel_scan_out%cth_error(i,j)=spixel_scan_out%cth_error_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%cth_error_vmin,kind=sreal)) then
                 spixel_scan_out%cth_error(i,j)=spixel_scan_out%int_fill_value
	!MJ spixel_scan_out%cth_error(i,j)=spixel_scan_out%cth_error_vmin
               endif

	!del_ctt
               dummyreal=abs(RTM_Pc%dTc_dPc)*dummyreal_store
               dummyreal=(dummyreal-spixel_scan_out%ctt_error_offset)/spixel_scan_out%ctt_error_scale

               if( dummyreal .ge. real(spixel_scan_out%ctt_error_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%ctt_error_vmax,kind=sreal)) then
                  spixel_scan_out%ctt_error(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%ctt_error_vmax,kind=sreal)) then
                   !MJ spixel_scan_out%ctt_error(i,j)=spixel_scan_out%int_fill_value
	spixel_scan_out%ctt_error(i,j)=spixel_scan_out%ctt_error_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%ctt_error_vmin,kind=sreal)) then
                spixel_scan_out%ctt_error(i,j)=spixel_scan_out%int_fill_value
	!MJ spixel_scan_out%ctt_error(i,j)=spixel_scan_out%ctt_error_vmin
               endif

               elseif(dummyreal_store .gt. real(spixel_scan_out%ctp_error_vmax,kind=sreal)) then
	!MJ                  spixel_scan_out%ctp_error(i,j)=spixel_scan_out%int_fill_value
	!MJ                  spixel_scan_out%cth_error(i,j)=spixel_scan_out%int_fill_value
	!MJ                  spixel_scan_out%ctt_error(i,j)=spixel_scan_out%int_fill_value

                  spixel_scan_out%ctp_error(i,j)=spixel_scan_out%ctp_error_vmax
                  spixel_scan_out%cth_error(i,j)=spixel_scan_out%cth_error_vmax
                  spixel_scan_out%ctt_error(i,j)=spixel_scan_out%ctt_error_vmax
               elseif(dummyreal_store .lt. real(spixel_scan_out%ctp_error_vmin,kind=sreal)) then
	                  spixel_scan_out%ctp_error(i,j)=spixel_scan_out%int_fill_value
	                  spixel_scan_out%cth_error(i,j)=spixel_scan_out%int_fill_value
	                  spixel_scan_out%ctt_error(i,j)=spixel_scan_out%int_fill_value

                  !spixel_scan_out%ctp_error(i,j)=spixel_scan_out%ctp_error_vmin
                  !spixel_scan_out%cth_error(i,j)=spixel_scan_out%cth_error_vmin
                  !spixel_scan_out%ctt_error(i,j)=spixel_scan_out%ctt_error_vmin

               endif

!               write(*,*) 'write cct'


               !-------------------------------------
               !CCT
               !-------------------------------------
               dummyreal=(SPixel%Xn(4)-spixel_scan_out%cct_offset)/spixel_scan_out%cct_scale
               if( dummyreal .ge. real(spixel_scan_out%cct_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cct_vmax,kind=sreal)) then
                  spixel_scan_out%cct(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out%cct(i,j)=spixel_scan_out%int_fill_value
               endif


               dummyreal=(sqrt(SPixel%Sn(4,4))-spixel_scan_out%cct_error_offset)/spixel_scan_out%cct_error_scale
               if( dummyreal .ge. real(spixel_scan_out%cct_error_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cct_error_vmax,kind=sreal)) then
                  spixel_scan_out%cct_error(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out%cct_error(i,j)=spixel_scan_out%int_fill_value
               endif


!               write(*,*) 'write stemp'

               !-------------------------------------
               !STEMP
               !-------------------------------------
               dummyreal=(SPixel%Xn(5)-spixel_scan_out%stemp_offset)/spixel_scan_out%stemp_scale
               if( dummyreal .ge. real(spixel_scan_out%stemp_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%stemp_vmax,kind=sreal)) then
                  spixel_scan_out%stemp(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%stemp_vmax,kind=sreal)) then
                  !MJ spixel_scan_out%stemp(i,j)=spixel_scan_out%int_fill_value
                  spixel_scan_out%stemp(i,j)=spixel_scan_out%stemp_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%stemp_vmin,kind=sreal)) then
                  spixel_scan_out%stemp(i,j)=spixel_scan_out%int_fill_value
                  !MJ spixel_scan_out%stemp(i,j)=spixel_scan_out%stemp_vmin
               endif


               dummyreal=(sqrt(SPixel%Sn(5,5))-spixel_scan_out%stemp_error_offset)/spixel_scan_out%stemp_error_scale
               if( dummyreal .ge. real(spixel_scan_out%stemp_error_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%stemp_error_vmax,kind=sreal)) then
                  spixel_scan_out%stemp_error(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%stemp_error_vmax,kind=sreal)) then
                  spixel_scan_out%stemp_error(i,j)=spixel_scan_out%stemp_error_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%stemp_error_vmin,kind=sreal)) then
                  !MJspixel_scan_out%stemp_error(i,j)=spixel_scan_out%stemp_error_vmin
	spixel_scan_out%stemp_error(i,j)=spixel_scan_out%int_fill_value
               endif

!                    write(*,*) 'write cth',spixel_scan_out%cth_scale
               !-------------------------------------
               !NO REAL STATE VARIABLES=>NO COVARIANCE FROM HERE
               !-------------------------------------
               !-------------------------------------
               !CTH
               !-------------------------------------
!                    write(*,*) RTM_Pc%Hc
!MST seems the height is at this point not in km, thus must be converted to
! that first!
               dummyreal=RTM_Pc%Hc/10./1000. ! now it's in km
               dummyreal=(dummyreal-spixel_scan_out%cth_offset)/spixel_scan_out%cth_scale
!                    write(*,*) 'dummyreal',dummyreal
!                    write(*,*) 'bfore if'
               if( dummyreal .ge. real(spixel_scan_out%cth_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cth_vmax,kind=sreal)) then
!               write(*,*) 'i',i                                                               
                  spixel_scan_out%cth(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%cth_vmax,kind=sreal)) then
!               write(*,*) 'i2',i                                                               
                  spixel_scan_out%cth(i,j)=spixel_scan_out%cth_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%cth_vmin,kind=sreal)) then
!               write(*,*) 'i2',i                                                               
                  spixel_scan_out%cth(i,j)=spixel_scan_out%int_fill_value
               endif







!print*,'ctp',spixel_scan_out%ctp(i,j)
!print*,'cth',spixel_scan_out%cth(i,j),RTM_Pc%Hc

!               write(*,*) 'write ctt'


               !-------------------------------------
               !CTT
               !-------------------------------------
               dummyreal=(RTM_Pc%Tc-spixel_scan_out%ctt_offset)/spixel_scan_out%ctt_scale
               if( dummyreal .ge. real(spixel_scan_out%ctt_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%ctt_vmax,kind=sreal)) then
                  spixel_scan_out%ctt(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%ctt_vmax,kind=sreal)) then
                  spixel_scan_out%ctt(i,j)=spixel_scan_out%ctt_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%ctt_vmin,kind=sreal)) then
                  spixel_scan_out%ctt(i,j)=spixel_scan_out%int_fill_value
               endif


!               write(*,*) 'write cwp'


               !-------------------------------------
               !CWP
               !-------------------------------------
               dummyreal=(SPixel%CWP-spixel_scan_out%cwp_offset)/spixel_scan_out%cwp_scale
!               write(*,*) SPixel%Xn(1)
               if( dummyreal .ge. real(spixel_scan_out%cwp_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cwp_vmax,kind=sreal)) then
                  spixel_scan_out%cwp(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%cwp_vmax,kind=sreal)) then
                  spixel_scan_out%cwp(i,j)=spixel_scan_out%cwp_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%cwp_vmin,kind=sreal)) then
                  spixel_scan_out%cwp(i,j)=spixel_scan_out%int_fill_value
               endif

               dummyreal=(sqrt(SPixel%CWP_error)-spixel_scan_out%cwp_error_offset)/spixel_scan_out%cwp_error_scale
               if( dummyreal .ge. real(spixel_scan_out%cwp_error_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%cwp_error_vmax,kind=sreal)) then
                  spixel_scan_out%cwp_error(i,j)=int(dummyreal, kind=sint)
               elseif(dummyreal .gt. real(spixel_scan_out%cwp_error_vmax,kind=sreal)) then
                  spixel_scan_out%cwp_error(i,j)=spixel_scan_out%cwp_error_vmax
               elseif(dummyreal .lt. real(spixel_scan_out%cwp_error_vmin,kind=sreal)) then
                  spixel_scan_out%cwp_error(i,j)=spixel_scan_out%int_fill_value
               endif



!               write(*,*) 'write covegence'


               !-------------------------------------
               !CONVERGENCE, NO OF ITERATIONS, NO OF PHASE CHANGES
               !-------------------------------------
               spixel_scan_out%convergence(i,j)=int(conv,kind=byte)

               if(conv .eq. 0 ) spixel_scan_out%niter(i,j)=int(Diag%Iterations,kind=byte)
               if(conv .eq. 1 ) spixel_scan_out%niter(i,j)=int(spixel_scan_out%byte_fill_value,kind=byte)

!MJOLD               spixel_scan_out%pchange(i,j)=int(Diag%PhaseChanges,kind=byte)
               !dummy statement to set everything to liquid:
               spixel_scan_out%pchange(i,j)=int(1,kind=byte)  

               !-------------------------------------
               !COST DUE TO MEASUREMENTS
               !-------------------------------------
               dummyreal=real((Diag%Jm-spixel_scan_out%costjm_offset)/spixel_scan_out%costjm_scale,kind=sreal)
               if( dummyreal .ge. real(spixel_scan_out%costjm_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%costjm_vmax,kind=sreal)) then
                  spixel_scan_out%costjm(i,j)=real(dummyreal,kind=sreal)!int(dummyreal, kind=sint)
               else
                  spixel_scan_out%costjm(i,j)=spixel_scan_out%real_fill_value!spixel_scan_out%int_fill_value
               endif


!               write(*,*) 'write costja',spixel_scan_out%costjm(i,j)


               !-------------------------------------
               !COST DUE TO A PRIORI
               !-------------------------------------
               dummyreal=real((Diag%Ja-spixel_scan_out%costja_offset)/spixel_scan_out%costja_scale,kind=sreal)
               if( dummyreal .ge. real(spixel_scan_out%costja_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out%costja_vmax,kind=sreal)) then
                  spixel_scan_out%costja(i,j)=real(dummyreal,kind=sreal) !int(dummyreal, kind=sint)
               else
                  spixel_scan_out%costja(i,j)=spixel_scan_out%real_fill_value!spixel_scan_out%int_fill_value
               endif

!               write(*,*) 'write costja',spixel_scan_out%costja(i,j)
!               write(*,*) 'write lsflag'

               !-------------------------------------
               !LAND-SEA FLAG
               !-------------------------------------
               spixel_scan_out%lsflag(i,j)=int(MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0), kind=byte)

!               write(*,*) 'write qcflag'

               !-------------------------------------
               !QUALITY FLAG
               !-------------------------------------
               spixel_scan_out%qcflag(i,j)=int(Diag%QCFlag,kind=sint)

end subroutine prepare_primary
