!-------------------------------------------------------------------------------
! Name: write_secondary.F90
!
! Purpose:
! Actual writing of the secondary output data to the netcdf file is carried out.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Return value:
! Name Type Description
!
! Local variables:
! Name Type Description
!
! History:
! 2011/12/19, Matthias Jerg: Creates initial output for main output variables.
! 2012/01/05, Caroline Poulsen: Add in reflectances and brightness temperature
! 2012/01/05, Caroline Poulsen: Add in albedo
! 2013/01/24, Caroline Poulsen: Changed how input_dummy is set input_dummy now
!    has name matching channel number
! 2014/06/13, Greg McGarragh: Put the code into a subroutine.
! 2014/06/13, Greg McGarragh: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_secondary(Ctrl, lcovar, SPixel, ncid, ixstart, ixstop, &
                           iystart, iystop, spixel_scan_out_sec, status)

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

   call nc_write_L2_long(ncid,'scanline_u',spixel_scan_out_sec%vidscanline_u,&
           spixel_scan_out_sec%scanline_u(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_long(ncid,'scanline_v',spixel_scan_out_sec%vidscanline_v,&
           spixel_scan_out_sec%scanline_v(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'cot_ap',spixel_scan_out_sec%vidcotap,&
           spixel_scan_out_sec%cot_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'cot_fg',spixel_scan_out_sec%vidcotfg,&
           spixel_scan_out_sec%cot_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'ref_ap',spixel_scan_out_sec%vidrefap,&
           spixel_scan_out_sec%ref_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
    call nc_write_L2_short(ncid,'ref_fg',spixel_scan_out_sec%vidreffg,&
           spixel_scan_out_sec%ref_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'ctp_ap',spixel_scan_out_sec%vidctpap,&
           spixel_scan_out_sec%ctp_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_L2_short(ncid,'ctp_fg',spixel_scan_out_sec%vidctpfg,&
           spixel_scan_out_sec%ctp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_L2_short(ncid,'stemp_fg',spixel_scan_out_sec%vidstempfg,&
           spixel_scan_out_sec%stemp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   do iinput=1,Ctrl%Ind%Ny
	write(input_num,"(i4)")Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
                input_dummy='residuals_'//trim(adjustl(input_num))

	call nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidres(iinput),&
	        spixel_scan_out_sec%residuals(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   ! forward modelled radiances
   do iinput=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='y0_'//trim(adjustl(input_num))

      call nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidy0(iinput),&
              spixel_scan_out_sec%y0(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   ! channels
   do iinput=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='channels_'//trim(adjustl(input_num))

      call nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidchans(iinput),&
              spixel_scan_out_sec%channels(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   ! albedo
   do iinput=1,Ctrl%Ind%Nsolar
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))

      input_dummy='albedo_'//trim(adjustl(input_num))

      call nc_write_L2_short(ncid,trim(adjustl(input_dummy)),spixel_scan_out_sec%vidalb(iinput),&
         spixel_scan_out_sec%albedo(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   if (lcovar) then
      do is=1,SPixel%Nx
         do js=1,SPixel%Nx
            write(input_num1,"(i4)") is
            write(input_num2,"(i4)") js
            input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))
            call nc_write_L2_float(ncid,input_dummy,spixel_scan_out_sec%vidcovar(is,js),&
                    spixel_scan_out_sec%covariance(:,:,is,js),ixstart,ixstop,iystart,iystop,wo,ierr)
         end do
      end do
   end if

   ! degrees of freedom for signal
   call nc_write_L2_short(ncid,'degrees_of_freedom_signal',spixel_scan_out_sec%vidds,&
	   spixel_scan_out_sec%ds(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   if (ierr .ne. 0 ) then
      status=SecondaryFileWriteErr
      write(*,*) 'write_secondary.inc: netcdf secondary file write error: ', status
      call Write_Log(Ctrl,'write_primary.inc: netcdf secondary file write error: ', status)
      stop
   end if

end subroutine write_secondary
