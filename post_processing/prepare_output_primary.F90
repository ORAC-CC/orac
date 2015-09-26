!-------------------------------------------------------------------------------
! Name: prepare_output_primary.F90
!
! Purpose:
! Map internal representation of variables to output representation by applying
! scale and offset where necessary.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2011/12/19, Matthias Jerg creates initial output for main ooutput variables.
! 2012/01/06, Matthias Jerg added in cwp
! 2012/01/16, Caroline Poulsen bug fix: changed how offset applied
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2012/07/17, MJ: fixes bug in CWP write.
! 2012/07/31, MJ: fixes bug in CTY write.
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and
!    cth
! 2014/01/22, MJ fixes FP overflow with COT.
! 2014/07/08, CP added more illumination options
! 2014/10/24, OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
!    (currently deactivated), and nisemask
! 2014/11/20, OS: added Pavolonis cloud phase variable, which is here classified
!    through Pavolonis cloud types
! 2014/11/20, CP: added cloud albedo
! 2015/02/07, CP: changed to common constants
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine prepare_output_primary(i, j, indexing, input_data, output_data)

   use orac_input
   use orac_ncdf
   use orac_output

   implicit none

   integer,                   intent(in)    :: i, j
   type(counts_and_indexes),  intent(in)    :: indexing
   type(input_data_primary),  intent(in)    :: input_data
   type(output_data_primary), intent(inout) :: output_data

   integer            :: k
   integer(kind=sint) :: temp_short_ctp_error
   real(kind=sreal)   :: temp_real, temp_real_cot, temp_real_ctp_error


   !----------------------------------------------------------------------------
   ! time
   !----------------------------------------------------------------------------
   output_data%time(i,j)=input_data%time(i,j)

   !----------------------------------------------------------------------------
   ! lat, lon
   !----------------------------------------------------------------------------
   output_data%lon(i,j)=input_data%Lon(i,j)/output_data%lon_scale
   output_data%lat(i,j)=input_data%Lat(i,j)/output_data%lat_scale

   !----------------------------------------------------------------------------
   ! sol_zen, sat_zen, rel_azi
   !----------------------------------------------------------------------------
   do k=1,indexing%NViews
      output_data%sat_zen(i,j,k)=input_data%satellite_zenith_view_no1(i,j)
      output_data%sol_zen(i,j,k)=input_data%solar_zenith_view_no1(i,j)
      output_data%rel_azi(i,j,k)=input_data%rel_azimuth_view_no1(i,j)
   end do

   !----------------------------------------------------------------------------
   ! cot, cot_error
   !----------------------------------------------------------------------------
   ! write microphysical values only if pixel is "day"
   if (input_data%illum(i,j) .eq. 1_byte .or.&
       input_data%illum(i,j) .eq. 4_byte .or. &
       input_data%illum(i,j) .eq. 5_byte .or.&
       input_data%illum(i,j) .eq. 6_byte .or. &
       input_data%illum(i,j) .eq. 7_byte .or.&
       input_data%illum(i,j) .eq. 8_byte .or. &
       input_data%illum(i,j) .eq. 9_byte ) then

      if (input_data%cot(i,j) .eq. sreal_fill_value) then
         temp_real_cot = sreal_fill_value
      else
         temp_real_cot = input_data%cot(i,j)
      end if
      call prepare_short_packed_float( &
              temp_real_cot, output_data%cot(i,j), &
              output_data%cot_scale, output_data%cot_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%cot_vmin, output_data%cot_vmax, &
              output_data%cot_vmax)

      if (input_data%cot_uncertainty(i,j) .eq. sreal_fill_value) then
         temp_real = sreal_fill_value
      else
         temp_real = input_data%cot_uncertainty(i,j)
      end if
      call prepare_short_packed_float( &
              temp_real, output_data%cot_error(i,j), &
              output_data%cot_error_scale, output_data%cot_error_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%cot_error_vmin, output_data%cot_error_vmax, &
              output_data%cot_error_vmax)

      !--------------------------------------------------------------------------
      ! ref, ref_error
      !--------------------------------------------------------------------------
      if (input_data%ref(i,j) .eq. sreal_fill_value) then
         temp_real = sreal_fill_value
      else
         temp_real = input_data%ref(i,j)
      end if
      call prepare_short_packed_float( &
              temp_real, output_data%ref(i,j), &
              output_data%ref_scale, output_data%ref_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%ref_vmin, output_data%ref_vmax, &
              output_data%ref_vmax)

      if (input_data%ref_uncertainty(i,j) .eq. sreal_fill_value) then
         temp_real = sreal_fill_value
      else
         temp_real = input_data%ref_uncertainty(i,j)
      end if
      call prepare_short_packed_float( &
              temp_real, output_data%ref_error(i,j), &
              output_data%ref_error_scale, output_data%ref_error_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%ref_error_vmin, output_data%ref_error_vmax, &
              output_data%ref_error_vmax)

      !-------------------------------------------------------------------------
      ! cwp, cwp_error
      !-------------------------------------------------------------------------
      if (input_data%cwp(i,j) .eq. sreal_fill_value) then
         temp_real = sreal_fill_value
      else
         temp_real = input_data%cwp(i,j)
      end if
      call prepare_short_packed_float( &
              temp_real, output_data%cwp(i,j), &
              output_data%cwp_scale, output_data%cwp_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%cwp_vmin, output_data%cwp_vmax, &
              output_data%cwp_vmax)

      if (input_data%cwp_uncertainty(i,j) .eq. sreal_fill_value) then
         temp_real = sreal_fill_value
      else
         temp_real = input_data%cwp_uncertainty(i,j)
      end if
      call prepare_short_packed_float( &
              temp_real, output_data%cwp_error(i,j), &
              output_data%cwp_error_scale, output_data%cwp_error_offset, &
              sreal_fill_value, sint_fill_value, &
              output_data%cwp_error_vmin, output_data%cwp_error_vmax, &
              output_data%cwp_error_vmax)
   end if

   !----------------------------------------------------------------------------
   ! ctp, ctp_error
   !----------------------------------------------------------------------------
   if (input_data%ctp(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%ctp(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%ctp(i,j), &
           output_data%ctp_scale, output_data%ctp_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_vmin, output_data%ctp_vmax, &
           output_data%ctp_vmax)

   if (input_data%ctp_uncertainty(i,j) .eq. sreal_fill_value) then
      temp_real_ctp_error=sreal_fill_value
   else
      temp_real_ctp_error = input_data%ctp_uncertainty(i,j)
      temp_short_ctp_error = ( int(temp_real_ctp_error, kind=sint) - &
                               output_data%ctp_error_scale &
                             ) / output_data%ctp_error_scale
   end if
   call prepare_short_packed_float( &
           temp_real_ctp_error, output_data%ctp_error(i,j), &
           output_data%ctp_error_scale, output_data%ctp_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctp_error_vmin, output_data%ctp_error_vmax, &
           output_data%ctp_error_vmax)

   !----------------------------------------------------------------------------
   ! cct, cct_error
   !----------------------------------------------------------------------------
   if (input_data%cc_total(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%cc_total(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cct(i,j), &
           output_data%cct_scale, output_data%cct_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cct_vmin, output_data%cct_vmax, &
           sint_fill_value)

   if (input_data%cc_total_uncertainty(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%cc_total_uncertainty(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cct_error(i,j), &
           output_data%cct_error_scale, output_data%cct_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cct_error_vmin, output_data%cct_error_vmax, &
           sint_fill_value)

   !----------------------------------------------------------------------------
   ! stemp, stemp_error
   !----------------------------------------------------------------------------
   if (input_data%stemp(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%stemp(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%stemp(i,j), &
           output_data%stemp_scale, output_data%stemp_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_vmin, output_data%stemp_vmax, &
           output_data%stemp_vmax)

   if (input_data%stemp_uncertainty(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%stemp_uncertainty(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%stemp_error(i,j), &
           output_data%stemp_error_scale, output_data%stemp_error_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%stemp_error_vmin, output_data%stemp_error_vmax, &
           output_data%stemp_error_vmax)

   !----------------------------------------------------------------------------
   ! cth, cth_error
   !----------------------------------------------------------------------------
   if (input_data%cth(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%cth(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cth(i,j), &
           output_data%cth_scale, output_data%cth_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cth_vmin, output_data%cth_vmax, &
           output_data%cth_vmax)

   if (input_data%cth_uncertainty(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%cth_uncertainty(i,j)
   end if
   call prepare_short_packed_float( &
        temp_real, output_data%cth_error(i,j), &
        output_data%cth_error_scale, output_data%cth_error_offset, &
        sreal_fill_value, sint_fill_value, &
        output_data%cth_error_vmin, output_data%cth_error_vmax, &
        output_data%cth_error_vmax)

   !----------------------------------------------------------------------------
   ! cth_corrected, cth_corrected_error
   !----------------------------------------------------------------------------
#ifdef CRAP
   if (input_data%cth_corrected(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%cth_corrected(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%cth_corrected(i,j), &
           output_data%cth_scale, output_data%cth_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cth_vmin, output_data%cth_vmax, &
           output_data%cth_vmax)

   if (input_data%cth_corrected_uncertainty(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%cth_corrected_uncertainty(i,j)
   end if
   call prepare_short_packed_float( &
        temp_real, output_data%cth_corrected_error(i,j), &
        output_data%cth_error_scale, output_data%cth_error_offset, &
        sreal_fill_value, sint_fill_value, &
        output_data%cth_error_vmin, output_data%cth_error_vmax, &
        output_data%cth_error_vmax)
#endif
   !----------------------------------------------------------------------------
   ! ctt, ctt_error
   !----------------------------------------------------------------------------
   if (input_data%ctt(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%ctt(i,j)
   end if
   call prepare_short_packed_float( &
           temp_real, output_data%ctt(i,j), &
           output_data%ctt_scale, output_data%ctt_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%ctt_vmin, output_data%ctt_vmax, &
           output_data%ctt_vmax)

   if (input_data%ctt_uncertainty(i,j) .eq. sreal_fill_value) then
      temp_real = sreal_fill_value
   else
      temp_real = input_data%ctt_uncertainty(i,j)
   end if
   call prepare_short_packed_float( &
        temp_real, output_data%ctt_error(i,j), &
        output_data%ctt_error_scale, output_data%ctt_error_offset, &
        sreal_fill_value, sint_fill_value, &
        output_data%ctt_error_vmin, output_data%ctt_error_vmax, &
        output_data%ctt_error_vmax)

   !----------------------------------------------------------------------------
   ! cloud_albedo
   !----------------------------------------------------------------------------
   do k=1,indexing%NSolar
      call prepare_short_packed_float( &
           input_data%cloud_albedo(i,j,k), output_data%cloud_albedo(i,j,k), &
           output_data%cloud_albedo_scale, output_data%cloud_albedo_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cloud_albedo_vmin, output_data%cloud_albedo_vmax, &
           sint_fill_value)
   end do

   !----------------------------------------------------------------------------
   ! convergence, niter
   !----------------------------------------------------------------------------
   output_data%convergence(i,j)=input_data%convergence(i,j)

   if (input_data%convergence(i,j) .eq. 0 ) output_data%niter(i,j)=input_data%niter(i,j)
   if (input_data%convergence(i,j) .eq. 1 ) output_data%niter(i,j)=input_data%niter(i,j)

   !----------------------------------------------------------------------------
   ! costja
   !----------------------------------------------------------------------------
   temp_real=input_data%costja(i,j)
   call prepare_float_packed_float( &
           temp_real, output_data%costja(i,j), &
           output_data%costja_scale, output_data%costja_offset, &
           sreal_fill_value, sreal_fill_value, &
           output_data%costja_vmin, output_data%costja_vmax, &
           sreal_fill_value)

   !----------------------------------------------------------------------------
   ! costjm
   !----------------------------------------------------------------------------
   temp_real=input_data%costjm(i,j)
   call prepare_float_packed_float( &
           temp_real, output_data%costjm(i,j), &
           output_data%costjm_scale, output_data%costjm_offset, &
           sreal_fill_value, sreal_fill_value, &
           output_data%costjm_vmin, output_data%costjm_vmax, &
           sreal_fill_value)

   !----------------------------------------------------------------------------
   ! qcflag
   !----------------------------------------------------------------------------
   output_data%qcflag(i,j)=input_data%QCFlag(i,j)

   !----------------------------------------------------------------------------
   ! lsflag
   !----------------------------------------------------------------------------
   output_data%lsflag(i,j)=input_data%LSFlag(i,j)

   !----------------------------------------------------------------------------
   ! lusflag
   !----------------------------------------------------------------------------
   output_data%lusflag(i,j)=input_data%lusflag(i,j)

   !----------------------------------------------------------------------------
   ! dem
   !----------------------------------------------------------------------------
   output_data%dem(i,j)=input_data%dem(i,j)

   !----------------------------------------------------------------------------
   ! nisemask
   !----------------------------------------------------------------------------
   output_data%nisemask(i,j)=input_data%nisemask(i,j)

   !----------------------------------------------------------------------------
   ! illum
   !----------------------------------------------------------------------------
   output_data%illum(i,j)=input_data%illum(i,j)

   !----------------------------------------------------------------------------
   ! cldtype
   !----------------------------------------------------------------------------
   output_data%cldtype(i,j)=input_data%cldtype(i,j)

   !----------------------------------------------------------------------------
   ! cldmask
   !----------------------------------------------------------------------------
   output_data%cldmask(i,j)=input_data%cldmask(i,j)

   !----------------------------------------------------------------------------
   ! cccot_pre
   !----------------------------------------------------------------------------
   call prepare_short_packed_float( &
           input_data%cccot_pre(i,j), output_data%cccot_pre(i,j), &
           output_data%cccot_pre_scale, output_data%cccot_pre_offset, &
           sreal_fill_value, sint_fill_value, &
           output_data%cccot_pre_vmin, output_data%cccot_pre_vmax, &
           sint_fill_value)

   !----------------------------------------------------------------------------
   ! phase
   !----------------------------------------------------------------------------
   if (input_data%phase(i,j) .eq. byte_fill_value) then
      output_data%phase(i,j) = byte_fill_value
   else
      output_data%phase(i,j) = input_data%phase(i,j)
   end if

   if (     input_data%cldtype(i,j) .eq. 0) then
      output_data%phase_pavolonis(i,j) = 0 ! phase = clear
   else if (input_data%cldtype(i,j) .lt. 5 &
      .and. input_data%cldtype(i,j) .gt. 1) then
      output_data%phase_pavolonis(i,j) = 1 ! phase = water
   else if (input_data%cldtype(i,j) .gt. 5) then
      output_data%phase_pavolonis(i,j) = 2 ! phase = ice
   else
      output_data%phase_pavolonis(i,j) = byte_fill_value ! for all
      ! other values (should not occur)
   end if

end subroutine prepare_output_primary
