! Name: def_vars_secondary_pp.inc
!
!
! Purpose:
! Define secondary output variables for netcdf. Variable type, scale, offset, fill value and /or range are defined and
! applied to the variable definition. Variable names are also defined.
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
!2011/12/19: Matthias Jerg creates initial output for main output variables.
!2012/1/5 C. Poulsen add channel info
!2012/1/15 C. Poulsen modified scales and offsets for residuals
!2012/07/09 MJ overhauls routine.
! $Id$
!
! Bugs:
!
!none known



!===============================
!RESIDUALS
!===============================
  write(*,*) 'setting up residual'

!      do iinput=1,Ny

         do i=1,Nsolar
            iinput=i
!            write(input_num,"(i4)") i

        input_num= Chan_Id(i)
!            if(iinput .eq. Ctrl%Ind%Ysolar(i)) then
               spixel_scan_out_rt%res_scale(iinput)=0.00001 !0.00001
               spixel_scan_out_rt%res_offset(iinput)=0.001 !0.0001
               spixel_scan_out_rt%res_vmin(iinput)=-30000
               spixel_scan_out_rt%res_vmax(iinput)=30000
  
               input_dummy='reflectance_residual_in_channel_no_'//trim(adjustl(input_num))


               CALL nc_defdata_short_no_units(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_rt%vidres(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), spixel_scan_out_rt%int_fill_value,&
                    & spixel_scan_out_rt%res_scale(iinput),spixel_scan_out_rt%res_offset(iinput),&
                    & spixel_scan_out_rt%res_vmin(iinput),spixel_scan_out_rt%res_vmax(iinput), wo,ierr)


                if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr
!            endif
         enddo


         do i=1,Nthermal

            input_num=chan_id(3+i)
!            write(input_num,"(i4)") i+3
            iinput=3+i

!            if(iinput .eq. Ctrl%Ind%Ythermal(i)) then
               spixel_scan_out_rt%res_scale(iinput)=0.0001!0.001
               spixel_scan_out_rt%res_offset(iinput)=0.00001
               spixel_scan_out_rt%res_vmin(iinput)=-30000
               spixel_scan_out_rt%res_vmax(iinput)=30000

               input_dummy='brightness_temperature_residual_in_channel_no_'//trim(adjustl(input_num))

               CALL nc_defdata_short(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_rt%vidres(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), 'kelvin', spixel_scan_in%int_fill_value,&
                    & spixel_scan_out_rt%res_scale(iinput),spixel_scan_out_rt%res_offset(iinput),&
                    & spixel_scan_out_rt%res_vmin(iinput),spixel_scan_out_rt%res_vmax(iinput), wo,ierr)
            if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

!            endif
         enddo

!      enddo


!===============================
!REFLECTANCES AND bRIGHTNESS TEMPERATURE
!===============================
write(*,*) 'setting up reflectances and bt'

!      do iinput=1,Ny


 !        write(input_num,"(i4)") Ctrl%Ind%Y_Id(iinput)

         do i=1,Nsolar
            iinput=i
            input_num=chan_id(i)            
!write(input_num,"(i4)") i
!           if(iinput .eq. Ctrl%Ind%Ysolar(i)) then
               spixel_scan_out_rt%chans_scale(iinput)=0.0001
               spixel_scan_out_rt%chans_offset(iinput)=0.00
               spixel_scan_out_rt%chans_vmin(iinput)=0
               spixel_scan_out_rt%chans_vmax(iinput)=10000

               input_num=chan_id(i)     
               input_dummy='reflectance_in_channel_no_'//trim(adjustl(input_num))

               CALL nc_defdata_short_no_units(ncid_rtondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_rt%vidchans(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), spixel_scan_out_rt%int_fill_value,&
                    & spixel_scan_out_rt%chans_scale(iinput),spixel_scan_out_rt%chans_offset(iinput),&
                    & spixel_scan_out_rt%chans_vmin(iinput),spixel_scan_out_rt%chans_vmax(iinput), wo,ierr)

            if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr
 !           endif
         enddo


         do i=1,Nthermal
            
            iinput=3+i
            input_num=chan_id(3+i)
!write(input_num,"(i4)") i+3
 !           if(iinput .eq. Ctrl%Ind%Ythermal(i)) then
               spixel_scan_out_rt%chans_scale(iinput)=.1
               spixel_scan_out_rt%chans_offset(iinput)=0.00
               spixel_scan_out_rt%chans_vmin(iinput)=0
               spixel_scan_out_rt%chans_vmax(iinput)=10000

               input_dummy='brightness_temperature_in_channel_no_'//trim(adjustl(input_num))

               CALL nc_defdata_short(ncid_secondary, dims_var, trim(adjustl(input_dummy)),&
                    & spixel_scan_out_rt%vidchans(iinput), trim(adjustl(input_dummy)), &
                    & trim(adjustl(input_dummy)), 'kelvin', spixel_scan_in%int_fill_value,&
                    & spixel_scan_out_rt%chans_scale(iinput),spixel_scan_out_rt%chans_offset(iinput),&
                    & spixel_scan_out_rt%chans_vmin(iinput),spixel_scan_out_rt%chans_vmax(iinput), wo,ierr)
            if(ierr .ne. 0 ) status=SecondaryFileDefinitionErr

 !           endif
         enddo


 !     enddo
