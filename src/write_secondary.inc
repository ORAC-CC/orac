! Name: write_secondary.inc
!
!
! Purpose:
! Actual writing of the secondary output data to the netcdf file is carried out.
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
!5/1/2012 Caroline Poulsen add in reflectances and brightness temperature
!5/1/2012 Caroline Poulsen add in albedo
!24/01/2013 Caroline Poulsen changed how input_dummy is set input_dummy now has name matching channel number

!write_secondary.inc! $Id$

! Bugs:
!
!none known


         CALL nc_write_L2_long(ncid_secondary,'scanline_u',spixel_scan_out_sec%vidscanline_u,&
	       & spixel_scan_out_sec%scanline_u(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_long(ncid_secondary,'scanline_v',spixel_scan_out_sec%vidscanline_v,&
	        & spixel_scan_out_sec%scanline_v(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_secondary,'cot_ap',spixel_scan_out_sec%vidcotap,&
	& spixel_scan_out_sec%cot_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_secondary,'cot_fg',spixel_scan_out_sec%vidcotfg,&
	& spixel_scan_out_sec%cot_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid_secondary,'ref_ap',spixel_scan_out_sec%vidrefap,&
	& spixel_scan_out_sec%ref_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_secondary,'ref_fg',spixel_scan_out_sec%vidreffg,&
	& spixel_scan_out_sec%ref_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid_secondary,'ctp_ap',spixel_scan_out_sec%vidctpap,&
	& spixel_scan_out_sec%ctp_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid_secondary,'ctp_fg',spixel_scan_out_sec%vidctpfg,&
	& spixel_scan_out_sec%ctp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid_secondary,'stemp_fg',spixel_scan_out_sec%vidstempfg,&
	& spixel_scan_out_sec%stemp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         do iinput=1,Ctrl%Ind%Ny


	write(input_num,"(i4)")Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
                input_dummy='residuals_'//trim(adjustl(input_num))
	write(*,*) 'outside',trim(adjustl(input_dummy)),spixel_scan_out_sec%vidres(iinput)
	   CALL nc_write_L2_short(ncid_secondary,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidres(iinput),&
	  & spixel_scan_out_sec%residuals(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)

         enddo




!
!forward modelled radiances
!

           do iinput=1,Ctrl%Ind%Ny

 !             write(input_num,"(i4)") iinput
 	       write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
              input_dummy='y0_'//trim(adjustl(input_num))

               CALL nc_write_L2_short(ncid_secondary,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidy0(iinput),&
		      & spixel_scan_out_sec%y0(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
              
           enddo

!
!channels
!
wo=1
         do iinput=1,Ctrl%Ind%Ny

            write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
            input_dummy='channels_'//trim(adjustl(input_num))

            CALL nc_write_L2_short(ncid_secondary,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidchans(iinput),&
		   & spixel_scan_out_sec%channels(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)

         enddo


!
!albedo
!

         do iinput=1,Ctrl%Ind%Nsolar
	 write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
!            write(input_num,"(i4)") iinput

            input_dummy='albedo_'//trim(adjustl(input_num))

            CALL nc_write_L2_short(ncid_secondary,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidalb(iinput),&
		   & spixel_scan_out_sec%albedo(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
            
         enddo



         if(lcovar) then

            do is=1,SPixel%Nx
               do js=1,SPixel%Nx

                  write(input_num1,"(i4)") is
                  write(input_num2,"(i4)") js
                  input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))
                  CALL nc_write_L2_float(ncid_secondary,input_dummy,spixel_scan_out_sec%vidcovar(is,js),&
		 & spixel_scan_out_sec%covariance(:,:,is,js),ixstart,ixstop,iystart,iystop,wo,ierr)
               enddo
            enddo
         endif
!
! Degrees of freedom for signal
!
	 CALL nc_write_L2_short(ncid_secondary,'degrees_of_freedom_signal',spixel_scan_out_sec%vidds,&
	       & spixel_scan_out_sec%ds(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!		 ierr=1
     if(ierr .ne. 0 ) then
       status=SecondaryFileWriteErr
	
       write(*,*) 'write_secondary.inc: netcdf secondary file write error:', status
       call Write_Log(Ctrl,'write_primary.inc: netcdf secondary file write error:', status)
       stop

       endif
