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
! 2014/09/01, Greg McGarragh: Start using the common/orac_ncdf.F90 write_array
!    interface.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_secondary(Ctrl, lcovar, SPixel, ncid, ixstart, ixstop, &
                           iystart, iystop, output_data, status)

   use CTRL_def
   use orac_ncdf
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
   integer            :: js,is
   integer            :: iinput

   status = 0

   call nc_write_array(ncid,'scanline_u',output_data%vid_scanline_u,&
           output_data%scanline_u(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'scanline_v',output_data%vid_scanline_v,&
           output_data%scanline_v(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'cot_ap',output_data%vid_cot_ap,&
           output_data%cot_ap(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'cot_fg',output_data%vid_cot_fg,&
           output_data%cot_fg(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'ref_ap',output_data%vid_ref_ap,&
           output_data%ref_ap(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'ref_fg',output_data%vid_ref_fg,&
           output_data%ref_fg(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'ctp_ap',output_data%vid_ctp_ap,&
           output_data%ctp_ap(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'ctp_fg',output_data%vid_ctp_fg,&
           output_data%ctp_fg(:,:),ixstart,ixstop,iystart,iystop)

   call nc_write_array(ncid,'stemp_fg',output_data%vid_stemp_fg,&
           output_data%stemp_fg(:,:),ixstart,ixstop,iystart,iystop)
   call nc_write_array(ncid,'stemp_ap',output_data%vid_stemp_fg,&
           output_data%stemp_ap(:,:),ixstart,ixstop,iystart,iystop)

   do iinput=1,Ctrl%Ind%Nsolar
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)),output_data%vid_albedo(iinput),&
         output_data%albedo(:,:,iinput),ixstart,ixstop,iystart,iystop)
   end do

   do iinput=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)),output_data%vid_channels(iinput),&
              output_data%channels(:,:,iinput),ixstart,ixstop,iystart,iystop)
   end do

   do iinput=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
      input_dummy='firstguess_radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)),output_data%vid_y0(iinput),&
              output_data%y0(:,:,iinput),ixstart,ixstop,iystart,iystop)
   end do

   do iinput=1,Ctrl%Ind%Ny
	write(input_num,"(i4)")Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(iinput))
        input_dummy='radiance_residual_in_channel_no_'//trim(adjustl(input_num))

	call nc_write_array(ncid,trim(adjustl(input_dummy)),output_data%vid_residuals(iinput),&
	        output_data%residuals(:,:,iinput),ixstart,ixstop,iystart,iystop)
   end do

   call nc_write_array(ncid,'degrees_of_freedom_signal',output_data%vid_ds,&
	   output_data%ds(:,:),ixstart,ixstop,iystart,iystop)

   if (lcovar) then
      do is=1,SPixel%Nx
         do js=1,SPixel%Nx
            write(input_num1,"(i4)") is
            write(input_num2,"(i4)") js
            input_dummy='covariance_matrix_element_'//trim(adjustl(input_num1))//trim(adjustl(input_num2))
            call nc_write_array(ncid,input_dummy,output_data%vid_covariance(is,js),&
                    output_data%covariance(:,:,is,js),ixstart,ixstop,iystart,iystop)
         end do
      end do
   end if

end subroutine write_secondary
