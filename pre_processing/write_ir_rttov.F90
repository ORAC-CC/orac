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
! preproc_dims   struct in   Summary of preprocessing grid definitions
! i              int    in   X (lon) array index of grid cell
! j              int    in   Y (lat) array index of grid call
! nchan          int    in   # of channels used
! nlevels        int    in   # of vertical levels used
! emissivity     struct in   RTTOV-derived surface emissivity
! transmission   struct in   RTTOV-derived atmospheric transmission
! radiance       struct in   RTTOV-derived atmospheric radiance
! radiance2      struct in   RTTOV-derived secondary atmospheric radiances
! write_flag     logic  in   T: Write values; F: Write fill-values
!
! History:
! 2014/09/11, AP: First version.
! 2014/09/28, GM: Updated to conform with a new arrangement of dimensions.
!
! $Id$
!
! Bugs:
! - Shouldn't upwelling radiance be assigned to the level above it's layer?
!-------------------------------------------------------------------------------

subroutine write_ir_rttov(netcdf_info, preproc_dims, idim, jdim, nchan, nlev, &
     emissivity, transmission, radiance, radiance2, write_flag)

   use netcdf_output_m, only: netcdf_output_info_t
   use orac_ncdf_m
   use parkind1, only: jpim
   use preproc_constants_m, only: lint, sreal, sreal_fill_value
   use preproc_structures_m, only: preproc_dims_t
   use rttov_types, only: rttov_emissivity, transmission_type, &
      radiance_type, radiance2_type

   implicit none

   type(netcdf_output_info_t), intent(in) :: netcdf_info
   type(preproc_dims_t),       intent(in) :: preproc_dims
   integer(lint),              intent(in) :: idim, jdim, nchan
   integer(jpim),              intent(in) :: nlev
   type(rttov_emissivity),     intent(in) :: emissivity(:)
   type(transmission_type),    intent(in) :: transmission
   type(radiance_type),        intent(in) :: radiance
   type(radiance2_type),       intent(in) :: radiance2
   logical,                    intent(in) :: write_flag

   integer                                :: jch
   real(sreal), dimension(nchan,1,1)      :: dummy_emis
   real(sreal), dimension(nchan,nlev,1,1) :: dummy_tac, dummy_tbc
   real(sreal), dimension(nchan,nlev,1,1) :: dummy_rbc_up
   real(sreal), dimension(nchan,nlev,1,1) :: dummy_rac_up, dummy_rac_down

   if (write_flag) then
      ! Calculate required above/below cloud radiances and transmittances
      ! TOA emissivity
      dummy_emis(:,1,1) = emissivity%emis_out
      do jch=1,nchan
         ! Transmission from level to TOA
         dummy_tac(jch,:,1,1) = transmission%tau_levels(:,jch)
         ! Transmission from surface to level
         dummy_tbc(jch,:,1,1) = transmission%tau_total(jch) / &
            transmission%tau_levels(:,jch)

         ! Translate layers onto levels

         ! Upwelling radiance from surface to level
         dummy_rbc_up(jch,2:,1,1) = &
            (radiance%clear(jch) - radiance2%up(:,jch)) / &
            transmission%tau_levels(2:,jch)
         ! Upwelling radiance from level
         dummy_rac_up(jch,2:,1,1) = radiance2%up(:,jch)
         ! Downwelling radiance from level
         dummy_rac_down(jch,2:,1,1) = radiance2%down(:,jch)
      end do

      ! Set top level to be top layer
      dummy_rbc_up(:,1,1,1) = dummy_rbc_up(:,2,1,1)
      dummy_rac_up(:,1,1,1) = dummy_rac_up(:,2,1,1)
      dummy_rac_down(:,1,1,1) = dummy_rac_down(:,2,1,1) * &
         transmission%tau_levels(2,:) / transmission%tau_levels(1,:)
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
   call nc_write_array(netcdf_info%ncid_lwrtm, 'emiss_lw', &
                       netcdf_info%vid_emiss_lw, dummy_emis, &
                       1, 1, nchan, &
                       1, idim, 1, 1, jdim, 1)
   call nc_write_array(netcdf_info%ncid_lwrtm, 'tac_lw', &
                       netcdf_info%vid_tac_lw, dummy_tac, &
                       1, 1, nchan, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call nc_write_array(netcdf_info%ncid_lwrtm, 'tbc_lw', &
                       netcdf_info%vid_tbc_lw, dummy_tbc, &
                       1, 1, nchan, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call nc_write_array(netcdf_info%ncid_lwrtm, 'rbc_up_lw', &
                       netcdf_info%vid_rbc_up_lw, dummy_rbc_up, &
                       1, 1, nchan, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call nc_write_array(netcdf_info%ncid_lwrtm, 'rac_up_lw', &
                       netcdf_info%vid_rac_up_lw, dummy_rac_up, &
                       1, 1, nchan, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call nc_write_array(netcdf_info%ncid_lwrtm, 'rac_down_lw', &
                       netcdf_info%vid_rac_down_lw, dummy_rac_down, &
                       1, 1, nchan, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)

end subroutine write_ir_rttov
