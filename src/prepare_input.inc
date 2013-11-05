
               do iinput=1,Ctrl%Ind%Ny

                  dummyreal=MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0,iinput)/&
                       & spixel_scan_in%input_scale(iinput)-&
                       & spixel_scan_in%input_offset(iinput)
                  if( dummyreal .ge. real(spixel_scan_in%input_vmin(iinput)) .and. &
                       & dummyreal .le. real(spixel_scan_in%input_vmax(iinput))) then
                     spixel_scan_in%input(i,iinput)=int(dummyreal, kind=sint)
                  else
                     spixel_scan_in%input(i,iinput)=spixel_scan_out%int_fill_value
                  endif


                  dummyreal=Diag%YError(iinput)/&
                       & spixel_scan_in%error_scale(iinput)-&
                       & spixel_scan_in%error_offset(iinput)
                  if( dummyreal .ge. real(spixel_scan_in%error_vmin(iinput)) .and. &
                       & dummyreal .le. real(spixel_scan_in%error_vmax(iinput))) then
                     spixel_scan_in%error(i,iinput)=int(dummyreal, kind=sint)
                  else
                     spixel_scan_in%error(i,iinput)=spixel_scan_out%int_fill_value
                  endif

               enddo


