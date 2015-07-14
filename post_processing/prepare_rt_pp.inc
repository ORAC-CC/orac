
! Name: prepare_rt_pp.inc..f90
!
!
! Purpose:
! 
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
!Original version Matthias Jerg
! 5/1/2012 Caroline Poulsen add in reflectances and brightness temperature
! 15/1/2012 Caroline Poulsen changed how offset was applied
!2012/07/09 MJ overhauls routine.
! 2014/09/20 CP changed channel numbering
!2015/02/07 CP: changed to common constants and tidied up string reading of instrument
! $Id$
!
! Bugs:
! not a bug but this code assumes 5 channels of instrument data
! may not work for avhrr/modis needs to be checked
!none known

!
!residuals
!
do iinput=1,Ny

write(*,*)' prepare_rt may be instrument dependant this is for AATSR',Ny
   if (iinput .eq. 1) then
      resin=l2_input_2d%reflectance_residual_in_channel_no_2(j,i)
   else if (iinput .eq. 2) then
      resin=l2_input_2d%reflectance_residual_in_channel_no_3(j,i)
      
   else if (iinput .eq. 3) then


   if  (ch_3a_3b_available(1) .eq. 1) then
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

