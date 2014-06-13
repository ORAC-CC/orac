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
!13/06/2014, Greg McGarragh: Put the code into a subroutine.

!write_secondary.inc! $Id$

! Bugs:
!
!none known

subroutine write_secondary(Ctrl, lcovar, SPixel, ncid, ixstart, ixstop, iystart, iystop, spixel_scan_out_sec, status)

   use CTRL_def
   use SPixel_def

   implicit none

   type(CTRL_t),                           intent(in)    :: Ctrl
   logical,                                intent(in)    :: lcovar
   type(SPixel_t),                         intent(in)    :: SPixel
   integer,                                intent(in)    :: ncid
   integer,                                intent(in)    :: ixstart
   integer,                                intent(in)    :: ixstop
   integer,                                intent(in)    :: iystart
   integer,                                intent(in)    :: iystop
   type(spixel_scanline_secondary_output), intent(inout) :: spixel_scan_out_sec
   integer,                                intent(inout) :: status

   character(len=20)  :: input_num,input_num1,input_num2
   character(len=500) :: input_dummy
   integer            :: ierr
   integer            :: js,is
   integer            :: iinput
   integer            :: wo = 0

         CALL nc_write_L2_long(ncid,'scanline_u',spixel_scan_out_sec%vidscanline_u,&
	       & spixel_scan_out_sec%scanline_u(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_long(ncid,'scanline_v',spixel_scan_out_sec%vidscanline_v,&
	        & spixel_scan_out_sec%scanline_v(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'cot_ap',spixel_scan_out_sec%vidcotap,&
	& spixel_scan_out_sec%cot_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'cot_fg',spixel_scan_out_sec%vidcotfg,&
	& spixel_scan_out_sec%cot_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid,'ref_ap',spixel_scan_out_sec%vidrefap,&
	& spixel_scan_out_sec%ref_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'ref_fg',spixel_scan_out_sec%vidreffg,&
	& spixel_scan_out_sec%ref_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid,'ctp_ap',spixel_scan_out_sec%vidctpap,&
	& spixel_scan_out_sec%ctp_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         CALL nc_write_L2_short(ncid,'ctp_fg',spixel_scan_out_sec%vidctpfg,&
	& spixel_scan_out_sec%ctp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)


         CALL nc_write_L2_short(ncid,'stemp_fg',spixel_scan_out_sec%vidstempfg,&
	& spixel_scan_out_sec%stemp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

         do iinput=1,Ctrl%Ind%Ny


	write(input_num,"(i4)")Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
                input_dummy='residuals_'//trim(adjustl(input_num))
	write(*,*) 'outside',trim(adjustl(input_dummy)),spixel_scan_out_sec%vidres(iinput)
	   CALL nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidres(iinput),&
	  & spixel_scan_out_sec%residuals(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)

         enddo




!
!forward modelled radiances
!

           do iinput=1,Ctrl%Ind%Ny

 !             write(input_num,"(i4)") iinput
 	       write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
              input_dummy='y0_'//trim(adjustl(input_num))

               CALL nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidy0(iinput),&
		      & spixel_scan_out_sec%y0(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
              
           enddo

!
!channels
!
wo=1
         do iinput=1,Ctrl%Ind%Ny

            write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
            input_dummy='channels_'//trim(adjustl(input_num))

            CALL nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidchans(iinput),&
		   & spixel_scan_out_sec%channels(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)

         enddo


!
!albedo
!

         do iinput=1,Ctrl%Ind%Nsolar
	 write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
!            write(input_num,"(i4)") iinput

            input_dummy='albedo_'//trim(adjustl(input_num))

            CALL nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidalb(iinput),&
		   & spixel_scan_out_sec%albedo(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
            
         enddo



         if(lcovar) then

            do is=1,SPixel%Nx
               do js=1,SPixel%Nx

                  write(input_num1,"(i4)") is
                  write(input_num2,"(i4)") js
                  input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))
                  CALL nc_write_L2_float(ncid,input_dummy,spixel_scan_out_sec%vidcovar(is,js),&
		 & spixel_scan_out_sec%covariance(:,:,is,js),ixstart,ixstop,iystart,iystop,wo,ierr)
               enddo
            enddo
         endif
!
! Degrees of freedom for signal
!
	 CALL nc_write_L2_short(ncid,'degrees_of_freedom_signal',spixel_scan_out_sec%vidds,&
	       & spixel_scan_out_sec%ds(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

!		 ierr=1
     if(ierr .ne. 0 ) then
       status=SecondaryFileWriteErr
	
       write(*,*) 'write_secondary.inc: netcdf secondary file write error:', status
       call Write_Log(Ctrl,'write_primary.inc: netcdf secondary file write error:', status)
       stop

       endif

end subroutine write_secondary
