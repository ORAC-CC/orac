
!Original version Matthias Jerg
! 5/1/2012 Caroline Poulsen add in reflectances and brightness temperature
! 15/1/2012 Caroline Poulsen changed how offset was applied
!2015/02/07 CP: changed to common constants and tidied up string reading of instrument

                spixel_scan_out_sec%scanline_u(i)=int(l2_input_2d%scanline_u(j,i))
                spixel_scan_out_sec%scanline_v(i)=int(l2_input_2d%scanline_v(j,i))

               dummyreal=(l2_input_2d%cot_ap(j,i)-spixel_scan_out_sec%cot_ap_offset)/spixel_scan_out_sec%cot_ap_scale
!               write(*,*) l2_input_2d%Xn(1)

               if( dummyreal .ge. real(spixel_scan_out_sec%cot_ap_vmin) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%cot_ap_vmax)) then
                  spixel_scan_out_sec%cot_ap(i)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%cot_ap(i)=spixel_scan_out_sec%int_fill_value
               endif

               dummyreal=(l2_input_2d%cot_fg(j,i)-spixel_scan_out_sec%cot_fg_offset)/spixel_scan_out_sec%cot_fg_scale
               if( dummyreal .ge. real(spixel_scan_out_sec%cot_fg_vmin) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%cot_fg_vmax)) then
                  spixel_scan_out_sec%cot_fg(i)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%cot_fg(i)=spixel_scan_out_sec%int_fill_value
               endif


               dummyreal=(l2_input_2d%ref_ap(j,i)-spixel_scan_out_sec%ref_ap_offset)/spixel_scan_out_sec%ref_ap_scale
!               write(*,*) l2_input_2d%ref_ap(j,i)
               if( dummyreal .ge. real(spixel_scan_out_sec%ref_ap_vmin) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ref_ap_vmax)) then
                  spixel_scan_out_sec%ref_ap(i)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ref_ap(i)=spixel_scan_out_sec%int_fill_value
               endif

               dummyreal=(l2_input_2d%ref_fg(j,i)-spixel_scan_out_sec%ref_fg_offset)/spixel_scan_out_sec%ref_fg_scale
!               write(*,*) l2_input_2d%ref_fg(j,i)
               if( dummyreal .ge. real(spixel_scan_out_sec%ref_fg_vmin) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ref_fg_vmax)) then
                  spixel_scan_out_sec%ref_fg(i)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ref_fg(i)=spixel_scan_out_sec%int_fill_value
               endif


!               write(*,*) Spixel%Xb(3),Spixel%X0(3)

               dummyreal=(l2_input_2d%ctp_ap(j,i)-spixel_scan_out_sec%ctp_ap_offset)/spixel_scan_out_sec%ctp_ap_scale
!               write(*,*) l2_input_2d%ctp_ap(j,i)
               if( dummyreal .ge. real(spixel_scan_out_sec%ctp_ap_vmin) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ctp_ap_vmax)) then
                  spixel_scan_out_sec%ctp_ap(i)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ctp_ap(i)=spixel_scan_out_sec%int_fill_value
               endif

               dummyreal=(l2_input_2d%ctp_fg(j,i)-spixel_scan_out_sec%ctp_fg_offset)/spixel_scan_out_sec%ctp_fg_scale
!               write(*,*) l2_input_2d%ctp_fg(j,i)
               if( dummyreal .ge. real(spixel_scan_out_sec%ctp_fg_vmin) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%ctp_fg_vmax)) then
                  spixel_scan_out_sec%ctp_fg(i)=int(dummyreal, kind=sint)
               else
                  spixel_scan_out_sec%ctp_fg(i)=spixel_scan_out_sec%int_fill_value
               endif



                dummyreal=(l2_input_2d%stemp_fg(j,i)-spixel_scan_out_sec%stemp_fg_offset)/spixel_scan_out_sec%stemp_fg_scale


            if( dummyreal .ge. real(spixel_scan_out_sec%stemp_fg_vmin) .and. &
                    & dummyreal .le. real(spixel_scan_out_sec%stemp_fg_vmax)) then
                  spixel_scan_out_sec%stemp_fg(i)=int(dummyreal, kind=sint)

               else
                  spixel_scan_out_sec%stemp_fg(i)=spixel_scan_out_sec%int_fill_value
               endif


!
!residuals
!
               do iinput=1,Ny
                  if (iinput .eq. 1) then
                     resin=l2_input_2d%reflectance_residual_in_channel_no_2(j,i)
                  else if (iinput .eq. 2) then
                     resin=l2_input_2d%reflectance_residual_in_channel_no_3(j,i)
                     
                  else if (iinput .eq. 3) then

 if  (ch_3a_3b_available(1) .eq. 0) then
          resin=l2_input_2d%reflectance_residual_in_channel_no_4(j,i)
   endif else begin
  	  resin=l2_input_2d%brightness_temperature_residual_in_channel_no_5(j,i)
   endelse

                  else if (iinput .eq. 4) then
                     resin=l2_input_2d%brightness_temperature_residual_in_channel_no_6(j,i)
                  else if (iinput .eq. 5) then
                     resin=l2_input_2d%brightness_temperature_residual_in_channel_no_7(j,i)
                  endif
               
	   if(resin .gt. 1.0E-12) then
                  dummyreal=(resin-spixel_scan_out_sec%res_offset(iinput))/&
                       & spixel_scan_out_sec%res_scale(iinput)
	   else
	     dummyreal=real_fill_value
	       endif	     


                  if( dummyreal .ge. real(spixel_scan_out_sec%res_vmin(iinput)) .and. &
                       & dummyreal .le. real(spixel_scan_out_sec%res_vmax(iinput))) then
                     spixel_scan_out_sec%residuals(i,iinput)=int(dummyreal, kind=sint)
                  else
                     spixel_scan_out_sec%residuals(i,iinput)=spixel_scan_out_sec%int_fill_value
                  endif

               enddo

!
!  firstguess forward modelled radiance
!
               do iinput=1,Ny


                  if (iinput .eq. 1) then
                     resin=l2_input_2d%firstguess_reflectance_in_channel_no_1(j,i)
                  else if (iinput .eq. 2) then
                     resin=l2_input_2d%firstguess_reflectance_in_channel_no_2(j,i)
                     
                  else if (iinput .eq. 3) then
                     resin=l2_input_2d%firstguess_reflectance_in_channel_no_3(j,i)
                  else if (iinput .eq. 4) then
                     resin=l2_input_2d%firstguess_brightness_temperature_in_channel_no_4(j,i)
                  else if (iinput .eq. 5) then
                     resin=l2_input_2d%firstguess_brightness_temperature_in_channel_no_5(j,i)
                  endif
		    !write(*,*) resin,spixel_scan_out_sec%y0_scale(iinput)
		    if(resin .gt. 1.0E-12) then
		      dummyreal=(resin-spixel_scan_out_sec%y0_offset(iinput))/&
		      & spixel_scan_out_sec%y0_scale(iinput)
		    else
		      dummyreal=real_fill_value
		      endif

                  if( dummyreal .ge. real(spixel_scan_out_sec%y0_vmin(iinput)) .and. &
                       & dummyreal .le. real(spixel_scan_out_sec%y0_vmax(iinput))) then
                     spixel_scan_out_sec%y0(i,iinput)=int(dummyreal, kind=sint)
                  else
                     spixel_scan_out_sec%y0(i,iinput)=spixel_scan_out_sec%int_fill_value
                  endif

              enddo



!
! add channel reflectance and brightness temperature information
!

               do iinput=1,Ny

                  if (iinput .eq. 1) then
                     resin=l2_input_2d%reflectance_in_channel_no_1(j,i)
                  else if (iinput .eq. 2) then
                     resin=l2_input_2d%reflectance_in_channel_no_2(j,i)
                     
                  else if (iinput .eq. 3) then
                     resin=l2_input_2d%reflectance_in_channel_no_3(j,i)
                  else if (iinput .eq. 4) then
                     resin=l2_input_2d%brightness_temperature_in_channel_no_4(j,i)
                  else if (iinput .eq. 5) then
                     resin=l2_input_2d%brightness_temperature_in_channel_no_5(j,i) 
                  endif

	    if(resin .gt. 1.0E-12) then
                  dummyreal=(resin -spixel_scan_out_sec%chans_offset(iinput))/&
                       & spixel_scan_out_sec%chans_scale(iinput)
	    else
	      dummyreal=real_fill_value
		endif	      

                  if( dummyreal .ge. real(spixel_scan_out_sec%chans_vmin(iinput)) .and. &
                       & dummyreal .le. real(spixel_scan_out_sec%chans_vmax(iinput))) then
                     spixel_scan_out_sec%channels(i,iinput)=int(dummyreal, kind=sint)

          else
                     spixel_scan_out_sec%channels(i,iinput)=spixel_scan_out_sec%int_fill_value
                  endif

               enddo


!               if(lcovar) then

!                  do is=1,Nx
!                     do js=1,Nx
!                        spixel_scan_out_sec%covariance(i,is,js)=real(Spixel%Sn(is,js),kind=sreal)
!                        if((spixel_scan_out_sec%covariance(i,is,js) .lt. spixel_scan_out%real_fill_value_lat_lon) .or. &
!                             & (spixel_scan_out_sec%covariance(i,is,js) .gt. abs(spixel_scan_out%real_fill_value_lat_lon))) then
!                           spixel_scan_out_sec%covariance(i,is,js)=spixel_scan_out%real_fill_value_lat_lon
!                        endif
!                     enddo
!                  enddo
!               endif
