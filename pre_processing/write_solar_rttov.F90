!-------------------------------------------------------------------------------
! Name: write_solar_rttov.F90
!
! Purpose:
! Copy contents of RTTOV structures into ORAC structures.
!
! Description and Algorithm details:
! 1) Loop over channels and levels, copying contents of transmission and
!    radiance into preproc_swrtm.
!    Note that this uses the tausun_XXX_path1 element of the transmission
!    structure. That is the surface-to-satellite output for the solar channels.
!    The 3.7um channel will also have valid values in the tau_XXX field, which
!    will have been derived used the thermal coefficients, which is different
!    to the value we take here.
!
! Arguments:
! Name           Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! netcdf_info    struct in   Summary of NCDF file properties.
! coefs          struct in   rttov_coefs structure
! idim           int    in   X (lon) array index of grid cell
! jdim           int    in   Y (lat) array index of grid call
! nlev           int    in   # of vertical levels used
! satza          real   in   Satellite zenith angle
! transmission   struct in   RTTOV-derived atmospheric transmission
! write_flag     logic  in   T: Write values; F: Write fill-values
!
! History:
! 2014/09/11, AP: First version.
! 2014/09/28, GM: Updated to conform with a new arrangement of dimensions.
! 2014/11/28, GM: A previous fix to the correction of transmittance for mass
!    path (amf_recip) in rttov_driver() was incorrect. The relevant code then
!    got moved into here.  The correct fix has now been implemented.
! 2016/04/09, SP: Write one channel at a time, to facilitate multiple views.
! 2016/04/30, SP: Fixed bug in multi-view processing
! 2017/02/25, SP: Update to RTTOV v12.1 (ExtWork)
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_solar_rttov(netcdf_info, coefs, idim, jdim, nlev, satza, &
     transmission, write_flag, chan_num, rttov_num)

   use netcdf_output_m, only: netcdf_output_info_t
   use orac_ncdf_m
   use parkind1, only: jpim, jprb
   use preproc_constants_m, only: lint, sreal, d2r, sreal_fill_value
   use preproc_structures_m, only: preproc_dims_t
   use rttov_types, only: rttov_emissivity, rttov_transmission, &
        rttov_coefs

   implicit none

   type(netcdf_output_info_t), intent(in) :: netcdf_info
   type(rttov_coefs),          intent(in) :: coefs
   integer(lint),              intent(in) :: idim, jdim
   integer(jpim),              intent(in) :: nlev
   real(jprb),                 intent(in) :: satza
   type(rttov_transmission),   intent(in) :: transmission
   logical,                    intent(in) :: write_flag
   integer,                    intent(in) :: chan_num
   integer,                    intent(in) :: rttov_num

   real(sreal)                        :: amf_recip
   real(sreal), dimension(1,nlev,1,1) :: dummy_tac, dummy_tbc

   if (write_flag) then
      ! The reciprocal of the air mass factor
      amf_recip = cos(satza*d2r)

      ! Calculate required above/below cloud transmittances
      ! Identify which transmittance to output from the channel type
      ! (see p.113 of RTTOV v 11 Users Guide)
      if (coefs%coef%ss_val_chn(rttov_num) == 2) then
         ! Transmission from level to TOA
         dummy_tac(1,:,1,1) = &
              transmission%tausun_levels_path1(:,rttov_num)**amf_recip
         ! Transmission from surface to level
         dummy_tbc(1,:,1,1) = &
              transmission%tausun_total_path1(rttov_num)**amf_recip &
              / dummy_tac(1,:,1,1)
      else
         ! Transmission from level to TOA
         dummy_tac(1,:,1,1) = transmission%tau_levels(:,rttov_num)**amf_recip
         ! Transmission from surface to level
         dummy_tbc(1,:,1,1) = transmission%tau_total(rttov_num)**amf_recip &
              / dummy_tac(1,:,1,1)
      end if
   else
      ! Write fill values
      dummy_tac = sreal_fill_value
      dummy_tbc = sreal_fill_value
   end if

   ! Write outputs
   call ncdf_write_array(netcdf_info%ncid_swrtm, "tac_sw", &
                       netcdf_info%vid_tac_sw, dummy_tac, &
                       1, chan_num, 1, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)
   call ncdf_write_array(netcdf_info%ncid_swrtm, "tbc_sw", &
                       netcdf_info%vid_tbc_sw, dummy_tbc, &
                       1, chan_num, 1, 1, 1, nlev, &
                       1, idim, 1, 1, jdim, 1)

end subroutine write_solar_rttov
