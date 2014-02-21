! Name: def_vars_input.inc
!
!
! Purpose:
! Definition of L1 input data which is written to output file again.
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
!2011/12/19: Matthias Jerg creates initial version of the code.
!
! $Id$
!
! Bugs:
!
!none known


!===============================
!MSI DATA
!===============================



      do iinput=1,Ctrl%Ind%Ny

         if(index(Ctrl%Inst%Name,'MODIS') .ge. 1) then

            if(Ctrl%Ind%Y_Id(iinput) .eq. 1) then
               modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))(:)='645.5nm_central_wavelength'
            elseif(Ctrl%Ind%Y_Id(iinput) .eq. 2) then
               modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))(:)='856.5nm_central_wavelength'
            elseif(Ctrl%Ind%Y_Id(iinput) .eq. 3) then
               modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))(:)='1629.1nm_central_wavelength'
            elseif(Ctrl%Ind%Y_Id(iinput) .eq. 4) then
               modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))(:)='3.785um_central_wavelength'
            elseif(Ctrl%Ind%Y_Id(iinput) .eq. 5) then
               modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))(:)='11.017um_central_wavelength'
            elseif(Ctrl%Ind%Y_Id(iinput) .eq. 6) then
               modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))(:)='12.032um_central_wavelength'
            endif

         endif

!!$         write(*,*) '1'
!!$         write(*,*) modis_input%modis_channel_names(iinput)
!!$         write(*,*) '2'
!!$         pause

         write(input_num,"(i4)") Ctrl%Ind%Y_Id(iinput)

!         write(*,*) input_num
!         pause

         do i=1,Ctrl%Ind%Nsolar
            if(iinput .eq. Ctrl%Ind%Ysolar(i)) then
               spixel_scan_in%input_scale(iinput)=0.0001
               spixel_scan_in%input_offset(iinput)=0.00
               spixel_scan_in%input_vmin(iinput)=0
               spixel_scan_in%input_vmax(iinput)=10000

               input_dummy='reflectance_in_channel_no_'//trim(adjustl(input_num))


               s_input_dummy=trim(adjustl('reflectance_in_channel_no_'//trim(adjustl(input_num))//':'//&
                    & trim(adjustl(modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))))))

               CALL nc_defdata_short_no_units(ncid_input, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_in%vidinput(iinput), trim(adjustl(s_input_dummy)), &
                    & trim(adjustl(s_input_dummy)), spixel_scan_in%int_fill_value,&
                    & spixel_scan_in%input_scale(iinput),spixel_scan_in%input_offset(iinput),&
                    & spixel_scan_in%input_vmin(iinput),spixel_scan_in%input_vmax(iinput), wo)

            endif
         enddo


         do i=1,Ctrl%Ind%Nthermal
            if(iinput .eq. Ctrl%Ind%Ythermal(i)) then
               spixel_scan_in%input_scale(iinput)=0.1
               spixel_scan_in%input_offset(iinput)=0.00
               spixel_scan_in%input_vmin(iinput)=0
               spixel_scan_in%input_vmax(iinput)=10000

               input_dummy='brightness_temperature_in_channel_no_'//trim(adjustl(input_num))

               s_input_dummy=trim(adjustl('brightness_temperature_in_channel_no_'//trim(adjustl(input_num))//':'//&
                    & trim(adjustl(modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))))))

               CALL nc_defdata_short(ncid_input, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_in%vidinput(iinput), trim(adjustl(s_input_dummy)), &
                    & trim(adjustl(s_input_dummy)), 'kelvin', spixel_scan_in%int_fill_value,&
                    & spixel_scan_in%input_scale(iinput),spixel_scan_in%input_offset(iinput),&
                    & spixel_scan_in%input_vmin(iinput),spixel_scan_in%input_vmax(iinput), wo)


            endif
         enddo

         
         !         write(*,*) trim(adjustl(input_dummy)),iinput
         !      write(*,*) ncid_primary

      enddo



!===============================
!MSI ERROR
!===============================
      do iinput=1,Ctrl%Ind%Ny

!         write(input_num,"(i4)") iinput
         write(input_num,"(i4)") Ctrl%Ind%Y_Id(iinput)

         do i=1,Ctrl%Ind%Nsolar
            if(iinput .eq. Ctrl%Ind%Ysolar(i)) then
               spixel_scan_in%error_scale(iinput)=0.0001
               spixel_scan_in%error_offset(iinput)=0.00
               spixel_scan_in%error_vmin(iinput)=0
               spixel_scan_in%error_vmax(iinput)=10000

        
               input_dummy='reflectance_error_in_channel_no_'//trim(adjustl(input_num))

               s_input_dummy=trim(adjustl('reflectance_error_in_channel_no_'//trim(adjustl(input_num))//':'//&
                    & trim(adjustl(modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))))))

               CALL nc_defdata_short_no_units(ncid_input, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_in%viderror(iinput), trim(adjustl(s_input_dummy)), &
                    & trim(adjustl(s_input_dummy)), spixel_scan_in%int_fill_value,&
                    & spixel_scan_in%error_scale(iinput),spixel_scan_in%error_offset(iinput),&
                    & spixel_scan_in%error_vmin(iinput),spixel_scan_in%error_vmax(iinput), wo)

            endif
         enddo


         do i=1,Ctrl%Ind%Nthermal
            if(iinput .eq. Ctrl%Ind%Ythermal(i)) then
               spixel_scan_in%error_scale(iinput)=0.1
               spixel_scan_in%error_offset(iinput)=0.00
               spixel_scan_in%error_vmin(iinput)=0
               spixel_scan_in%error_vmax(iinput)=10000

               input_dummy='brightness_temperature_error_in_channel_no_'//trim(adjustl(input_num))

               s_input_dummy=trim(adjustl('brightness_temperature_error_in_channel_no_'//trim(adjustl(input_num))//':'//&
                    & trim(adjustl(modis_input%modis_channel_names(Ctrl%Ind%Y_Id(iinput))))))


               CALL nc_defdata_short(ncid_input, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_in%viderror(iinput), trim(adjustl(s_input_dummy)), &
                    & trim(adjustl(s_input_dummy)), 'kelvin', spixel_scan_in%int_fill_value,&
                    & spixel_scan_in%error_scale(iinput),spixel_scan_in%error_offset(iinput),&
                    & spixel_scan_in%error_vmin(iinput),spixel_scan_in%error_vmax(iinput), wo)


            endif
         enddo

         
         !         write(*,*) trim(adjustl(input_dummy)),iinput
         !      write(*,*) ncid_primary

      enddo
