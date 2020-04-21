!-------------------------------------------------------------------------------
! Name: write_ir_rttov.F90
!
! Purpose:
! Copy contents of RTTOV structures into ORAC structures.
!
! Description and Algorithm details:
! 1) Loop over channels and levels, copying contents of transmission and
!    radiance into preproc_lwrtm
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! netcdf_info    struct in   Summary of NCDF file properties.
! idim           int    in   X (lon) array index of grid cell
! jdim           int    in   Y (lat) array index of grid call
! nlev           int    in   # of vertical levels used
! emissivity     struct in   RTTOV-derived surface emissivity
! transmission   struct in   RTTOV-derived atmospheric transmission
! radiance       struct in   RTTOV-derived atmospheric radiance
! radiance2      struct in   RTTOV-derived secondary atmospheric radiances
! write_flag     logic  in   T: Write values; F: Write fill-values
!
! History:
! 2014/09/11, AP: First version.
! 2014/09/28, GM: Updated to conform with a new arrangement of dimensions.
! 2016/04/09, SP: Write one channel at a time, to facilitate multiple views.
! 2017/02/25, SP: Update to RTTOV v12.1 (ExtWork)
!
! Bugs:
! - Shouldn't upwelling radiance be assigned to the level above it's layer?
!-------------------------------------------------------------------------------

subroutine write_ir_rttov(netcdf_info, idim, jdim, nlev, emissivity, &
     transmission, radiance, radiance2, write_flag, chan_num, rttov_num)

   use netcdf_output_m, only: netcdf_output_info_t
   use orac_ncdf_m
   use parkind1, only: jpim
   use preproc_constants_m, only: lint, sreal, sreal_fill_value
   use preproc_structures_m, only: preproc_dims_t
   use rttov_types, only: rttov_emissivity, rttov_transmission, &
      rttov_radiance, rttov_radiance2

   implicit none

   type(netcdf_output_info_t), intent(in) :: netcdf_info
   integer(lint),              intent(in) :: idim, jdim
   integer(jpim),              intent(in) :: nlev
   type(rttov_emissivity),     intent(in) :: emissivity(:)
   type(rttov_transmission),   intent(in) :: transmission
   type(rttov_radiance),       intent(in) :: radiance
   type(rttov_radiance2),      intent(in) :: radiance2
   logical,                    intent(in) :: write_flag
   integer,                    intent(in) :: chan_num
   integer,                    intent(in) :: rttov_num

   real(sreal), dimension(1,1,1)      :: dummy_emis
   real(sreal), dimension(1,nlev,1,1) :: dummy_tac, dummy_tbc
   real(sreal), dimension(1,nlev,1,1) :: dummy_rbc_up
   real(sreal), dimension(1,nlev,1,1) :: dummy_rac_up, dummy_rac_down

   if (write_flag) then
      ! Emissivity
      dummy_emis(:,1,1) = emissivity(rttov_num)%emis_out

      ! Calculate required above/below cloud transmittances and radiances

      ! Transmission from level to TOA
      dummy_tac(1,:,1,1) = transmission%tau_levels(:,rttov_num)
      ! Transmission from surface to level
      dummy_tbc(1,:,1,1) = transmission%tau_total(rttov_num) / &
           transmission%tau_levels(:,rttov_num)

      ! Translate layers onto levels

      ! Upwelling radiance from surface to level
      dummy_rbc_up(1,2:,1,1) = &
           (radiance%clear(rttov_num) - radiance2%up(:,rttov_num)) / &
           transmission%tau_levels(2:,rttov_num)
      ! Upwelling radiance from level
      dummy_rac_up(1,2:,1,1) = radiance2%up(:,rttov_num)
      ! Downwelling radiance from level
      dummy_rac_down(1,2:,1,1) = radiance2%down(:,rttov_num)

      ! Set top level to be top layer
      dummy_rbc_up(1,1,1,1) = dummy_rbc_up(1,2,1,1)
      dummy_rac_up(1,1,1,1) = dummy_rac_up(1,2,1,1)
      dummy_rac_down(1,1,1,1) = dummy_rac_down(1,2,1,1) * &
         transmission%tau_levels(2,rttov_num) / &
         transmission%tau_levels(1,rttov_num)
   else
      ! Write fill values
      dummy_emis = sreal_fill_value
      dummy_tac = sreal_fill_value
      dummy_tbc = sreal_fill_value
      dummy_rbc_up = sreal_fill_value
      dummy_rac_up = sreal_fill_value
      dummy_rac_down = sreal_fill_value
   end if

   ! Write outputs
   call ncdf_write_array(netcdf_info%ncid_lwrtm, 'emiss_lw', &
                       netcdf_info%vid_emiss_lw, dummy_emis, &
                       1, chan_num, 1, &
                       1, idim, 1, 1, jdim, 1)
   call ncdf_write_array(netcdf_info%ncid_lwrtm, 'tac_lw', &
                       netcdf_info%vid_tac_lw, dummy_tac, &
                       1, chan_num, 1, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call ncdf_write_array(netcdf_info%ncid_lwrtm, 'tbc_lw', &
                       netcdf_info%vid_tbc_lw, dummy_tbc, &
                       1, chan_num, 1, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call ncdf_write_array(netcdf_info%ncid_lwrtm, 'rbc_up_lw', &
                       netcdf_info%vid_rbc_up_lw, dummy_rbc_up, &
                       1, chan_num, 1, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call ncdf_write_array(netcdf_info%ncid_lwrtm, 'rac_up_lw', &
                       netcdf_info%vid_rac_up_lw, dummy_rac_up, &
                       1, chan_num, 1, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call ncdf_write_array(netcdf_info%ncid_lwrtm, 'rac_down_lw', &
                       netcdf_info%vid_rac_down_lw, dummy_rac_down, &
                       1, chan_num, 1, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)

end subroutine write_ir_rttov
