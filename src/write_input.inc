         do iinput=1,Ctrl%Ind%Ny

            write(input_num,"(i4)") iinput

            input_dummy='input_'//trim(adjustl(input_num))

!            write(*,*) trim(adjustl(input_dummy)),iinput

            CALL nc_write_L2_short(ncid_input,trim(adjustl(input_dummy)),spixel_scan_in%vidinput(iinput),&
                 spixel_scan_in%input(:,iinput),ixstart,ixstop,j,wo)

            CALL nc_write_L2_short(ncid_input,trim(adjustl(input_dummy)),spixel_scan_in%viderror(iinput),&
                 spixel_scan_in%error(:,iinput),ixstart,ixstop,j,wo)
            
         enddo
