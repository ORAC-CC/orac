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
! History:
! 2011/12/19, Matthias Jerg: creates initial output for main output variables.
! 2012/01/05, Caroline Poulsen: add in reflectances and brightness temperature
! 2015/07/16, Greg McGarragh: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_secondary_pp(ncid, ixstart, ixstop, iystart, iystop, indexing, &
                              output_data, global_atts)

   use global_attributes
   use orac_ncdf
   use scanline_structure
   use structures_pp
   use vartypes_pp

   implicit none

   integer,                                intent(in) :: ncid
   integer,                                intent(in) :: ixstart, ixstop, &
                                                         iystart, iystop
   type(counts_and_indexes),               intent(in) :: indexing
   type(spixel_scanline_secondary_output), intent(in) :: output_data
   type(global_attributes_s),              intent(in) :: global_atts

   character(len=32)  :: input_num,input_num1,input_num2
   character(len=512) :: input_dummy
   logical            :: lcovar = .false.
   integer            :: i, j
   integer            :: n_x, n_y

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

   do i=1,indexing%NSolar
      write(input_num,"(i4)") indexing%Y_Id(i)
      input_dummy='albedo_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_albedo(i),output_data%albedo(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)
   end do

   do i=1,indexing%Ny
      write(input_num,"(i4)") indexing%Y_Id(i)
      input_dummy='radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_channels(i),output_data%channels(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)
   end do

   do i=1,indexing%Ny
      write(input_num,"(i4)") indexing%Y_Id(i)
      input_dummy='firstguess_radiance_in_channel_no_'//trim(adjustl(input_num))

      call nc_write_array(ncid,trim(adjustl(input_dummy)), &
              output_data%vid_y0(i),output_data%y0(ixstart:,:,i), &
              1,1,n_x,1,1,n_y)
   end do

   do i=1,indexing%Ny
	write(input_num,"(i4)") indexing%Y_Id(i)
        input_dummy='radiance_residual_in_channel_no_'//trim(adjustl(input_num))

	call nc_write_array(ncid,trim(adjustl(input_dummy)), &
                output_data%vid_residuals(i),output_data%residuals(ixstart:,:,i), &
                1,1,n_x,1,1,n_y)
   end do

   call nc_write_array(ncid,'degrees_of_freedom_signal',output_data%vid_ds,&
	   output_data%ds(ixstart:,iystart:),1,1,n_x,1,1,n_y)

   if (lcovar) then
      do i=1,indexing%Nx
         do j=1,indexing%Nx
            write(input_num1,"(i4)") i
            write(input_num2,"(i4)") j
            input_dummy='covariance_matrix_element_' // &
                 trim(adjustl(input_num1))//trim(adjustl(input_num2))
            call nc_write_array(ncid,input_dummy,output_data%vid_covariance(i,j),&
                    output_data%covariance(ixstart:,:,i,j),1,1,n_x,1,1,n_y)
         end do
      end do
   end if

end subroutine write_secondary_pp
