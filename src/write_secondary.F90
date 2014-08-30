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
                           iystart, iystop, output_data, status)

   use CTRL_def
   use nc_utils
   use SPixel_def

   implicit none

   type(CTRL_t),                intent(in)    :: Ctrl
   logical,                     intent(in)    :: lcovar
   type(SPixel_t),              intent(in)    :: SPixel
   integer,                     intent(in)    :: ncid
   integer,                     intent(in)    :: ixstart
   integer,                     intent(in)    :: ixstop
   integer,                     intent(in)    :: iystart
   integer,                     intent(in)    :: iystop
   type(output_data_secondary), intent(inout) :: output_data
   integer,                     intent(inout) :: status

   character(len=20)  :: input_num,input_num1,input_num2
   character(len=500) :: input_dummy
   integer            :: ierr
   integer            :: js,is
   integer            :: iinput
   integer            :: wo = 0

   call nc_write_long(ncid,'scanline_u',output_data%vid_scanline_u,&
           output_data%scanline_u(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_long(ncid,'scanline_v',output_data%vid_scanline_v,&
           output_data%scanline_v(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_short(ncid,'cot_ap',output_data%vid_cot_ap,&
           output_data%cot_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_short(ncid,'cot_fg',output_data%vid_cot_fg,&
           output_data%cot_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_short(ncid,'ref_ap',output_data%vid_ref_ap,&
           output_data%ref_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_short(ncid,'ref_fg',output_data%vid_ref_fg,&
           output_data%ref_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_short(ncid,'ctp_ap',output_data%vid_ctp_ap,&
           output_data%ctp_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_short(ncid,'ctp_fg',output_data%vid_ctp_fg,&
           output_data%ctp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   call nc_write_short(ncid,'stemp_fg',output_data%vid_stemp_fg,&
           output_data%stemp_fg(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)
   call nc_write_short(ncid,'stemp_ap',output_data%vid_stemp_fg,&
           output_data%stemp_ap(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   do iinput=1,Ctrl%Ind%Nsolar
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_short(ncid,trim(adjustl(input_dummy)),output_data%vid_albedo(iinput),&
         output_data%albedo(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   do iinput=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_short(ncid,trim(adjustl(input_dummy)),output_data%vid_channels(iinput),&
              output_data%channels(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   do iinput=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='firstguess_radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_short(ncid,trim(adjustl(input_dummy)),output_data%vid_y0(iinput),&
              output_data%y0(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   do iinput=1,Ctrl%Ind%Ny
	write(input_num,"(i4)")Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
        input_dummy='radiance_residual_in_channel_no_'//trim(adjustl(input_num))

	call nc_write_short(ncid,trim(adjustl(input_dummy)),output_data%vid_residuals(iinput),&
	        output_data%residuals(:,:,iinput),ixstart,ixstop,iystart,iystop,wo,ierr)
   end do

   call nc_write_short(ncid,'degrees_of_freedom_signal',output_data%vid_ds,&
	   output_data%ds(:,:),ixstart,ixstop,iystart,iystop,wo,ierr)

   if (lcovar) then
      do is=1,SPixel%Nx
         do js=1,SPixel%Nx
            write(input_num1,"(i4)") is
            write(input_num2,"(i4)") js
            input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))
            call nc_write_float(ncid,input_dummy,output_data%vid_covariance(is,js),&
                    output_data%covariance(:,:,is,js),ixstart,ixstop,iystart,iystop,wo,ierr)
         end do
      end do
   end if

   if (ierr .ne. 0 ) then
      status=SecondaryFileWriteErr
      write(*,*) 'write_secondary.inc: netcdf secondary file write error: ', status
      call Write_Log(Ctrl,'write_primary.inc: netcdf secondary file write error: ', status)
      stop
   end if

end subroutine write_secondary
