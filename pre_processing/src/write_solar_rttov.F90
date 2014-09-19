!-------------------------------------------------------------------------------
! Name: call_rtm_solar_rttov.F90
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
! 2012/05/22, CP: Initial version first version extracted form
!             code by M. Jerg
! 2012/07/04, CP: remove nview dependance
! 2012/17/07, CP: complete rewrite
! 2012/29/07, CP: algorithm rewrite
! 2012/08/10, CP: algorithm debug
! 2012/08/14, CP: fixed bug in taubc
! 2013/12/11, GM: Significant code clean up.
! 2014/07/10, AP: Slight tidying. Removed errorstatus as not used.
! 2014/09/11, AP: Moved NCDF write routines here. Removed print statements.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine write_solar_rttov(netcdf_info, preproc_dims, coefs, idim, jdim, &
     nchan, nlev, satza, emissivity, transmission, radiance, radiance2, &
     write_flag)

   use netcdf_output, only: netcdf_output_info_s
   use orac_ncdf
   use parkind1, only: jpim, jprb
   use preproc_constants, only: lint, sreal, d2r, sreal_fill_value
   use preproc_structures, only: preproc_dims_s
   use rttov_types, only: rttov_emissivity, transmission_type, &
        radiance_type, radiance2_type, rttov_coefs

   implicit none

   type(netcdf_output_info_s), intent(in) :: netcdf_info
   type(preproc_dims_s),       intent(in) :: preproc_dims
   type(rttov_coefs),          intent(in) :: coefs
   integer(lint),              intent(in) :: idim, jdim, nchan
   integer(jpim),              intent(in) :: nlev
   real(jprb),                 intent(in) :: satza
   type(rttov_emissivity),     intent(in) :: emissivity(:)
   type(transmission_type),    intent(in) :: transmission
   type(radiance_type),        intent(in) :: radiance
   type(radiance2_type),       intent(in) :: radiance2
   logical,                    intent(in) :: write_flag

   integer                                 :: jch
   real(sreal)                             :: amf
   real(sreal), dimension(nchan)           :: tau_total
   real(sreal), dimension(1,1,nchan,nlev)  :: dummy_tac, dummy_tbc

   if (write_flag) then
      ! Equivalent to effective_2way_za
      amf = cos(satza*d2r)
      amf = amf / (amf + 1.)

      ! Calculate required above/below cloud transmittances
      do jch=1,nchan
         ! Identify which transmittance to output from the channel type
         ! (see p.113 of RTTOV v 11 Users Guide)
         if (coefs%coef%ss_val_chn(jch) == 2) then
            ! Transmission from level to TOA
            dummy_tac(1,1,jch,:) = transmission%tausun_levels_path1(:,jch)**amf
            ! Transmission from surface to level
            dummy_tbc(1,1,jch,:) = transmission%tausun_total_path1(jch)**amf &
                 / dummy_tac(1,1,jch,:)
         else
            ! Transmission from level to TOA
            dummy_tac(1,1,jch,:) = transmission%tau_levels(:,jch)**amf
            ! Transmission from surface to level
            dummy_tbc(1,1,jch,:) = transmission%tau_total(jch)**amf &
                 / dummy_tac(1,1,jch,:)
         end if
      end do
   else
      ! Write fill values
      dummy_tac = sreal_fill_value
      dummy_tbc = sreal_fill_value
   end if

   ! Write outputs
   call nc_write_array(netcdf_info%ncid_swrtm, 'tac_sw', &
        netcdf_info%vid_tac_sw, dummy_tac, &
        1, idim, 1, 1, jdim, 1, &
        1, 1, nchan, 1, 1, nlev)
   call nc_write_array(netcdf_info%ncid_swrtm, 'tbc_sw', &
        netcdf_info%vid_tbc_sw, dummy_tbc, &
        1, idim, 1, 1, jdim, 1, &
        1, 1, nchan, 1, 1, nlev)

end subroutine write_solar_rttov
