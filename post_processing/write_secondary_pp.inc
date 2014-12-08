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

!write_secondary.inc! $Id$

! Bugs:
!
!none known




         CALL nc_write_L2_long(ncid_secondary,'scanline_u',spixel_scan_out_sec%vidscanline_u,&
              spixel_scan_out_sec%scanline_u(:),ixstart,ixstop,j,wo,ierr)


       CALL nc_write_L2_long(ncid_secondary,'scanline_v',spixel_scan_out_sec%vidscanline_v,&
              spixel_scan_out_sec%scanline_v(:),ixstart,ixstop,j,wo,ierr)
   
       CALL nc_write_L2_short(ncid_secondary,'cot_ap',spixel_scan_out_sec%vidcotap,&
              spixel_scan_out_sec%cot_ap(:),ixstart,ixstop,j,wo,ierr)

  

         CALL nc_write_L2_short(ncid_secondary,'cot_fg',spixel_scan_out_sec%vidcotfg,&
              spixel_scan_out_sec%cot_fg(:),ixstart,ixstop,j,wo,ierr)


         CALL nc_write_L2_short(ncid_secondary,'ref_ap',spixel_scan_out_sec%vidrefap,&
              spixel_scan_out_sec%ref_ap(:),ixstart,ixstop,j,wo,ierr)

         CALL nc_write_L2_short(ncid_secondary,'ref_fg',spixel_scan_out_sec%vidreffg,&
              spixel_scan_out_sec%ref_fg(:),ixstart,ixstop,j,wo,ierr)


         CALL nc_write_L2_short(ncid_secondary,'ctp_ap',spixel_scan_out_sec%vidctpap,&
              spixel_scan_out_sec%ctp_ap(:),ixstart,ixstop,j,wo,ierr)

         CALL nc_write_L2_short(ncid_secondary,'ctp_fg',spixel_scan_out_sec%vidctpfg,&
              spixel_scan_out_sec%ctp_fg(:),ixstart,ixstop,j,wo,ierr)


         CALL nc_write_L2_short(ncid_secondary,'stemp_fg',spixel_scan_out_sec%vidstempfg,&
              spixel_scan_out_sec%stemp_fg(:),ixstart,ixstop,j,wo,ierr)


         do iinput=1,Ny

            write(input_num,"(i4)") iinput
!            input_num=chan_id(iinput)
            input_dummy='residuals_'//trim(adjustl(input_num))

!            write(*,*) trim(adjustl(input_dummy)),iinput

            CALL nc_write_L2_short(ncid_secondary,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidres(iinput),&
                 spixel_scan_out_sec%residuals(:,iinput),ixstart,ixstop,j,wo,ierr)

         enddo


!
!forward modelled radiances
!

           do iinput=1,Ny

              write(input_num,"(i4)") iinput
  
              input_dummy='y0_'//trim(adjustl(input_num))

               CALL nc_write_L2_short(ncid_secondary,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidy0(iinput),&
                   spixel_scan_out_sec%y0(:,iinput),ixstart,ixstop,j,wo,ierr)
              
           enddo

!
!channels
!

         do iinput=1,Ny

            write(input_num,"(i4)") iinput

            input_dummy='channels_'//trim(adjustl(input_num))


            CALL nc_write_L2_short(ncid_secondary,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidchans(iinput),&
                 spixel_scan_out_sec%channels(:,iinput),ixstart,ixstop,j,wo,ierr)
            
         enddo

!         if(lcovar) then
!
!            do is=1,Nx
!               do js=1,Nx
!                  !                  write(*,*) is,js
!                  write(input_num1,"(i4)") is
!                  write(input_num2,"(i4)") js
!                  input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))
!                  CALL nc_write_L2_float(ncid_secondary,input_dummy,spixel_scan_out_sec%vidcovar(is,js),&
!                       & spixel_scan_out_sec%covariance(:,is,js),ixstart,ixstop,j,wo,ierr)
!               enddo
!            enddo
!         endif

!		 ierr=1
     if(ierr .ne. 0 ) then
       status=SecondaryFileWriteErr
	
       write(*,*) 'write_secondary.inc: netcdf secondary file write error:', status

       stop

       endif
