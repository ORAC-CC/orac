!-------------------------------------------------------------------------------
! Name: write_output_secondary.F90
!
! Purpose:
! Actual writing of the secondary output data to the netcdf file is carried out.
!
! Description and Algorithm details:
! Call ncdf_write_array many time.
!
! Arguments:
! Name        Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! ncid        integer In          File ID for open output file
! ind         struct  In          Channel indexing information
! output_data struct  Both        Data to be written to output
!
! History:
! 2011/12/19, MJ: Creates initial output for main output variables.
! 2012/01/05, CP: Add in reflectances and brightness temperature
! 2012/01/05, CP: Add in albedo
! 2013/01/24, CP: Changed how input_dummy is set input_dummy now has name
!    matching channel number
! 2014/06/13, GM: Put the code into a subroutine.
! 2014/06/13, GM: Cleaned up the code.
! 2014/09/01, GM: Start using the common/orac_ncdf.F90 write_array interface.
! 2014/09/17, GM: Bug fix, forgot to offset y dimension of output.
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2015/09/06, GM: Move into common/ from src/ and changes related to sharing
!    with post_processing/.
! 2015/12/28, AP: Add output fields for aerosol retrievals.
! 2016/01/06, AP: Wrap do_* flags into output_flags structure.
! 2016/03/04, AP: Homogenisation of I/O modules.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_output_secondary(ncid, ind, output_data)

   use orac_ncdf_m

   implicit none

   integer,                       intent(in)    :: ncid
   type(common_indices_t),        intent(in)    :: ind
   type(output_data_secondary_t), intent(inout) :: output_data

   character(len=32)  :: input_num, input_num1, input_num2
   character(len=512) :: input_dummy
   integer            :: i, j

   call ncdf_write_array(ncid,'scanline_u', output_data%vid_scanline_u, &
        output_data%scanline_u(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'scanline_v', output_data%vid_scanline_v, &
        output_data%scanline_v(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

if (ind%flags%do_aerosol) then
   call ncdf_write_array(ncid,'aot550_ap', output_data%vid_aot550_ap, &
        output_data%aot550_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'aot550_fg', output_data%vid_aot550_fg, &
        output_data%aot550_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'aer_ap', output_data%vid_aer_ap, &
        output_data%aer_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'aer_fg', output_data%vid_aer_fg, &
        output_data%aer_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
end if

if (ind%flags%do_rho) then
   do i = 1, ind%Nrho
      call ncdf_write_array(ncid,'rho_ap', output_data%vid_rho_ap(i), &
           output_data%rho_ap(ind%X0:,ind%Y0:, i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'rho_fg', output_data%vid_rho_fg(i), &
           output_data%rho_fg(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do
end if

if (ind%flags%do_swansea) then
   do i = 1, ind%Nss
      call ncdf_write_array(ncid,'swansea_s_ap', &
           output_data%vid_swansea_s_ap(i), &
           output_data%swansea_s_ap(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'swansea_s_fg', &
           output_data%vid_swansea_s_fg(i), &
           output_data%swansea_s_fg(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do

   do i = 1, ind%NViews
      call ncdf_write_array(ncid,'swansea_p_ap', &
           output_data%vid_swansea_p_ap(i), &
           output_data%swansea_p_ap(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
      call ncdf_write_array(ncid,'swansea_p_fg', &
           output_data%vid_swansea_p_fg(i), &
           output_data%swansea_p_fg(ind%X0:,ind%Y0:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do
end if

if (ind%flags%do_cloud) then
   call ncdf_write_array(ncid,'cot_ap', output_data%vid_cot_ap, &
        output_data%cot_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cot_fg', output_data%vid_cot_fg, &
        output_data%cot_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cer_ap', output_data%vid_cer_ap, &
        output_data%cer_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cer_fg', output_data%vid_cer_fg, &
        output_data%cer_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctp_ap', output_data%vid_ctp_ap, &
        output_data%ctp_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctp_fg', output_data%vid_ctp_fg, &
        output_data%ctp_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'stemp_fg', output_data%vid_stemp_fg, &
        output_data%stemp_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'stemp_ap', output_data%vid_stemp_ap, &
        output_data%stemp_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   do i = 1, ind%NSolar
      write(input_num,"(i4)") ind%Y_Id(i)
      input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))

      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_albedo(i), output_data%albedo(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do
end if

if (ind%flags%do_cloud_layer_2) then
   call ncdf_write_array(ncid,'cot2_ap', output_data%vid_cot2_ap, &
        output_data%cot2_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cot2_fg', output_data%vid_cot2_fg, &
        output_data%cot2_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'cer2_ap', output_data%vid_cer2_ap, &
        output_data%cer2_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'cer2_fg', output_data%vid_cer2_fg, &
        output_data%cer2_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

   call ncdf_write_array(ncid,'ctp2_ap', output_data%vid_ctp2_ap, &
        output_data%ctp2_ap(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
   call ncdf_write_array(ncid,'ctp2_fg', output_data%vid_ctp2_fg, &
        output_data%ctp2_fg(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
end if

   do i = 1, ind%Ny
      write(input_num,"(i4)") ind%Y_Id(i)
      input_dummy='radiance_in_channel_no_'//trim(adjustl(input_num))

      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_channels(i), output_data%channels(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do

   do i = 1, ind%Ny
      write(input_num,"(i4)") ind%Y_Id(i)
      input_dummy='firstguess_radiance_in_channel_no_'//trim(adjustl(input_num))

      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_y0(i), output_data%y0(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do

   do i = 1, ind%Ny
      write(input_num,"(i4)") ind%Y_Id(i)
      input_dummy='radiance_residual_in_channel_no_'//trim(adjustl(input_num))

      call ncdf_write_array(ncid, trim(adjustl(input_dummy)), &
           output_data%vid_residuals(i), output_data%residuals(ind%X0:,:,i), &
           1, 1, ind%Xdim, 1, 1, ind%Ydim)
   end do

   call ncdf_write_array(ncid,'degrees_of_freedom_signal', output_data%vid_ds, &
        output_data%ds(ind%X0:,ind%Y0:), 1, 1, ind%Xdim, 1, 1, ind%Ydim)

if (ind%flags%do_covariance) then
   do i = 1, ind%Nx
      do j = 1, ind%Nx
         write(input_num1,"(i4)") i
         write(input_num2,"(i4)") j
         input_dummy='covariance_matrix_element_' // &
              trim(adjustl(input_num1))//trim(adjustl(input_num2))
         call ncdf_write_array(ncid, input_dummy, &
              output_data%vid_covariance(i,j), &
              output_data%covariance(ind%X0:,:,i,j), 1, 1, ind%Xdim, 1, 1, ind%Ydim)
      end do
   end do
end if

end subroutine write_output_secondary
