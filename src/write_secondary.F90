!-------------------------------------------------------------------------------
! Name: write_secondary.F90
!
! Purpose:
! Actual writing of the secondary output data to the netcdf file is carried out.
!
! Description and Algorithm details:
! Call nc_write_array many time.
!
! Arguments:
! Name        Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl        struct  In          Control parameters for the retrieval
! lcovar      logical In          Include covariance matrices in output
! SPixel      struct  In          Details of retrieved pixel
! ncid        integer In          File ID for open output file
! ixstart     integer In          Starting index on first dimension of output
! ixstop      integer In          Ending index on first dimension of output
! iystart     integer In          Starting index on second dimension of output
! iystop      integer In          Ending index on second dimension of output
! output_data struct  Both        Data to be written to output
!
! History:
! 2011/12/19, MJ: Creates initial output for main output variables.
! 2012/01/05, CP: Add in reflectances and brightness temperature
! 2012/01/05, CP: Add in albedo
! 2013/01/24, CP: Changed how input_dummy is set input_dummy now
!    has name matching channel number
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/09/01, GM: Start using the common/orac_ncdf.F90 write_array
!    interface.
! 2014/09/17, GM: Bug fix, forgot to offset y dimension of output.
! 2014/12/19, AP: YSolar and YThermal now contain the index of
!    solar/thermal channels with respect to the channels actually processed,
!    rather than the MSI file.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_secondary(Ctrl, lcovar, SPixel, ncid, ixstart, ixstop, &
                           iystart, iystop, output_data)

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

   character(len=32)  :: input_num,input_num1,input_num2
   character(len=512) :: input_dummy
   integer            :: i, j
   integer            :: n_x
   integer            :: n_y

   n_x = ixstop - ixstart + 1
   n_y = iystop - iystart + 1

   call nc_write_array(ncid,'scanline_u',output_data%vid_scanline_u,&
           output_data%scanline_u(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'scanline_v',output_data%vid_scanline_v,&
           output_data%scanline_v(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'cot_ap',output_data%vid_cot_ap,&
           output_data%cot_ap(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'cot_fg',output_data%vid_cot_fg,&
           output_data%cot_fg(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ref_ap',output_data%vid_ref_ap,&
           output_data%ref_ap(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ref_fg',output_data%vid_ref_fg,&
           output_data%ref_fg(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'ctp_ap',output_data%vid_ctp_ap,&
           output_data%ctp_ap(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'ctp_fg',output_data%vid_ctp_fg,&
           output_data%ctp_fg(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   call nc_write_array(ncid,'stemp_fg',output_data%vid_stemp_fg,&
           output_data%stemp_fg(ixstart:,iystart:),1,1,n_x,1,1,n_y)
   call nc_write_array(ncid,'stemp_ap',output_data%vid_stemp_ap,&
           output_data%stemp_ap(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   do i=1,Ctrl%Ind%NSolar
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(i)
      input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_albedo(i),output_data%albedo(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)
   end do

   do i=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(i)
      input_dummy='radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_channels(i),output_data%channels(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)
   end do

   do i=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(i)
      input_dummy='firstguess_radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_y0(i),output_data%y0(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)
   end do

   do i=1,Ctrl%Ind%Ny
      write(input_num,"(i4)") Ctrl%Ind%Y_Id(i)
        input_dummy='radiance_residual_in_channel_no_'//trim(adjustl(input_num))

        call nc_write_array(ncid,trim(adjustl(input_dummy)), &
                output_data%vid_residuals(i),output_data%residuals(ixstart:,:,i), &
                1,1,n_x,1,1,n_y)
   end do

   call nc_write_array(ncid,'degrees_of_freedom_signal',output_data%vid_ds,&
           output_data%ds(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   if (lcovar) then
      do i=1,SPixel%Nx
         do j=1,SPixel%Nx
            write(input_num1,"(i4)") i
            write(input_num2,"(i4)") j
            input_dummy='covariance_matrix_element_' // &
                 trim(adjustl(input_num1))//trim(adjustl(input_num2))
            call nc_write_array(ncid,input_dummy,output_data%vid_covariance(i,j),&
                    output_data%covariance(ixstart:,:,i,j),1,1,n_x,1,1,n_y)
         end do
      end do
   end if

end subroutine write_secondary
