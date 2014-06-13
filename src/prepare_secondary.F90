
!Original version Matthias Jerg
! 5/1/2012 Caroline Poulsen add in reflectances and brightness temperature
! 15/1/2012 Caroline Poulsen changed how offset was applied
! 20/09/2012 Caroline Poulsen remove scaling factor from albedo
! 20/09/2012 Caroline Poulsen chanaged how svan value is set
! 29/05/2013 Gareth Thomas added degrees of freedom for signal
! 30/01/2014 Greg McGarragh fixed writing of the residuals and first guess in
!    the case of nighttime pixels.
! 13/06/2014, Greg McGarragh: Put the code into a subroutine.

subroutine prepare_secondary(Ctrl, lcovar, i, j, MSI_Data, SPixel, Diag, spixel_scan_out, spixel_scan_out_sec, status)

   use CTRL_def
   use Diag_def
   use Data_def
   use SPixel_def

   implicit none

   type(CTRL_t),                           intent(in)    :: Ctrl
   logical,                                intent(in)    :: lcovar
   integer,                                intent(in)    :: i, j
   type(Data_t),                           intent(in)    :: MSI_Data
   type(SPixel_t),                         intent(in)    :: SPixel
   type(Diag_t),                           intent(in)    :: Diag
   type(spixel_scanline_primary_output),   intent(inout) :: spixel_scan_out
   type(spixel_scanline_secondary_output), intent(inout) :: spixel_scan_out_sec
   integer,                                intent(inout) :: status

   integer            :: ii
   integer            :: js,is
   integer            :: iinput
   real(kind=sreal)   :: dummyreal

                spixel_scan_out_sec%scanline_u(i,j)=i
!int(MSI_Data%Scan%uscan(SPixel%Loc%X0, SPixel%Loc%YSeg0))
                spixel_scan_out_sec%scanline_v(i,j)=j
!int(MSI_Data%Scan%vscan(SPixel%Loc%X0, SPixel%Loc%YSeg0))

!write(*,*)'scan', spixel_scan_out_sec%scanline_u(i,j), spixel_scan_out_sec%scanline_v(i,j)

               dummyreal=(SPixel%Xb(1)-spixel_scan_out_sec%cot_ap_offset)/spixel_scan_out_sec%cot_ap_scale


               if( dummyreal .ge. real(spixel_scan_out_sec%cot_ap_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%cot_ap_vmax,kind=sreal)) then
                  spixel_scan_out_sec%cot_ap(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%cot_ap(i,j)=spixel_scan_out_sec%int_fill_value
               endif

               dummyreal=(SPixel%X0(1)-spixel_scan_out_sec%cot_fg_offset)/spixel_scan_out_sec%cot_fg_scale
               if( dummyreal .ge. real(spixel_scan_out_sec%cot_fg_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%cot_fg_vmax,kind=sreal)) then
                  spixel_scan_out_sec%cot_fg(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%cot_fg(i,j)=spixel_scan_out_sec%int_fill_value
               endif


               dummyreal=(SPixel%Xb(2)-spixel_scan_out_sec%ref_ap_offset)/spixel_scan_out_sec%ref_ap_scale

               if( dummyreal .ge. real(spixel_scan_out_sec%ref_ap_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ref_ap_vmax,kind=sreal)) then
                  spixel_scan_out_sec%ref_ap(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ref_ap(i,j)=spixel_scan_out_sec%int_fill_value
               endif

               dummyreal=(SPixel%X0(2)-spixel_scan_out_sec%ref_fg_offset)/spixel_scan_out_sec%ref_fg_scale

               if( dummyreal .ge. real(spixel_scan_out_sec%ref_fg_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ref_fg_vmax,kind=sreal)) then
                  spixel_scan_out_sec%ref_fg(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ref_fg(i,j)=spixel_scan_out_sec%int_fill_value
               endif




               dummyreal=(SPixel%Xb(3)-spixel_scan_out_sec%ctp_ap_offset)/spixel_scan_out_sec%ctp_ap_scale

               if( dummyreal .ge. real(spixel_scan_out_sec%ctp_ap_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ctp_ap_vmax,kind=sreal)) then
                  spixel_scan_out_sec%ctp_ap(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ctp_ap(i,j)=spixel_scan_out_sec%int_fill_value
               endif

               dummyreal=(SPixel%X0(3)-spixel_scan_out_sec%ctp_fg_offset)/spixel_scan_out_sec%ctp_fg_scale

               if( dummyreal .ge. real(spixel_scan_out_sec%ctp_fg_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ctp_fg_vmax,kind=sreal)) then
                  spixel_scan_out_sec%ctp_fg(i,j)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ctp_fg(i,j)=spixel_scan_out_sec%int_fill_value
               endif



                dummyreal=(SPixel%X0(5)-spixel_scan_out_sec%stemp_fg_offset)/spixel_scan_out_sec%stemp_fg_scale


            if( dummyreal .ge. real(spixel_scan_out_sec%stemp_fg_vmin,kind=sreal) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%stemp_fg_vmax,kind=sreal)) then
                  spixel_scan_out_sec%stemp_fg(i,j)=int(dummyreal, kind=sint)

               else
                  spixel_scan_out_sec%stemp_fg(i,j)=spixel_scan_out_sec%int_fill_value
               endif


!
!residualsy
!

               do iinput=1,SPixel%Ind%NY
                  ii = SPixel%spixel_y_to_ctrl_y_index(iinput)
                  dummyreal=(Diag%YmFit(iinput)-spixel_scan_out_sec%res_offset(ii))/&
	                       & spixel_scan_out_sec%res_scale(ii)
                  if( dummyreal .ge. real(spixel_scan_out_sec%res_vmin(ii),kind=sreal) .and. &
                       & dummyreal .le. real(spixel_scan_out_sec%res_vmax(ii),kind=sreal)) then
                     spixel_scan_out_sec%residuals(i,j,ii)=int(dummyreal, kind=sint)

                  else
                     spixel_scan_out_sec%residuals(i,j,ii)=spixel_scan_out_sec%int_fill_value
                  endif
               enddo


!
!  firstguess forward modelled radiance
!
               do iinput=1,SPixel%Ind%NY
                  ii = SPixel%spixel_y_to_ctrl_y_index(iinput)
                  dummyreal=(Diag%Y0(iinput)-spixel_scan_out_sec%y0_offset(ii))/&
                       & spixel_scan_out_sec%y0_scale(ii)
                  if( dummyreal .ge. real(spixel_scan_out_sec%y0_vmin(ii),kind=sreal) .and. &
                       & dummyreal .le. real(spixel_scan_out_sec%y0_vmax(ii),kind=sreal)) then
                     spixel_scan_out_sec%y0(i,j,ii)=int(dummyreal, kind=sint)
                  else
                     spixel_scan_out_sec%y0(i,j,ii)=spixel_scan_out_sec%int_fill_value
                  endif
              enddo




!
! add channel reflectance and brightness temperature information
!
!write(*,*)'Ctrl%Ind%Ny',Ctrl%Ind%Ny
!write(*,*)'prepare secmsi+data',MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0,:) 
               do iinput=1,Ctrl%Ind%Ny!SPixel%Ind%NY!Ctrl%Ind%Ny

                  dummyreal=(MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0, iinput) -spixel_scan_out_sec%chans_offset(iinput))/&
                       & spixel_scan_out_sec%chans_scale(iinput)
                  if( dummyreal .ge. real(spixel_scan_out_sec%chans_vmin(iinput),kind=sreal) .and. &
                       		& dummyreal .le. real(spixel_scan_out_sec%chans_vmax(iinput),kind=sreal)) then
                       		spixel_scan_out_sec%channels(i,j,iinput)=int(dummyreal, kind=sint)
!write(*,*)'spixel_scan_out_sec%channels(i,j,iinput)',spixel_scan_out_sec%channels(i,j,iinput)
          	       else
                     spixel_scan_out_sec%channels(i,j,iinput)=spixel_scan_out_sec%int_fill_value

!write(*,*)'spixel_scan_out_sec%channels(i,j,iinput)',spixel_scan_out_sec%channels(i,j,iinput)
                  endif

               enddo


               if(lcovar) then

                  do is=1,SPixel%Nx
                     do js=1,SPixel%Nx
                        spixel_scan_out_sec%covariance(i,j,is,js)=real(SPixel%Sn(is,js),kind=sreal)
                        if((spixel_scan_out_sec%covariance(i,j,is,js) .lt. spixel_scan_out%real_fill_value_lat_lon) .or. &
                             & (spixel_scan_out_sec%covariance(i,j,is,js) .gt. abs(spixel_scan_out%real_fill_value_lat_lon))) then
                           spixel_scan_out_sec%covariance(i,j,is,js)=spixel_scan_out%real_fill_value_lat_lon
                        endif
                     enddo
                  enddo
               endif



!
! add albedo
!

               do iinput=1,Ctrl%Ind%Nsolar

                  dummyreal=((MSI_Data%ALB(SPixel%Loc%X0,SPixel%Loc%YSeg0,iinput)-&
				  & spixel_scan_out_sec%alb_offset(iinput)))/&
                       & spixel_scan_out_sec%alb_scale(iinput)



                  if( dummyreal .ge. real(spixel_scan_out_sec%alb_vmin(iinput),kind=sreal) .and. &
                       & dummyreal .le. real(spixel_scan_out_sec%alb_vmax(iinput),kind=sreal)) then
                     spixel_scan_out_sec%albedo(i,j,iinput)=int(dummyreal, kind=sint)

          else
                     spixel_scan_out_sec%albedo(i,j,iinput)=spixel_scan_out_sec%int_fill_value
                  endif

               enddo

!
! Degrees of freedom for signal
!
	    dummyreal = 0.0
	    do iinput=1,SPixel%Nx
	       dummyreal = dummyreal + Diag%AK(iinput,iinput)
	    enddo
            dummyreal = (dummyreal-spixel_scan_out_sec%ds_offset)/ &
                      & spixel_scan_out_sec%ds_scale
            if( dummyreal .ge. real(spixel_scan_out_sec%ds_vmin,kind=sreal) .and. &
              & dummyreal .le. real(spixel_scan_out_sec%ds_vmax,kind=sreal)) then
               spixel_scan_out_sec%ds(i,j)=int(dummyreal, kind=sint)
            else
               spixel_scan_out_sec%ds(i,j)=spixel_scan_out_sec%int_fill_value
            endif

end subroutine prepare_secondary
