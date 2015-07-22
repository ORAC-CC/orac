!-------------------------------------------------------------------------------
! Name: read_inter_file.F90
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for
! different attribute/variable type combinations.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/20, CP: creates initial file.
! 2012/03/18, CP: modified to add cloud flag
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/06/04, MJ: changes routine names to "*_pp" to avoid confusion when
!    building libraries.
! 2014/10/24, OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
! 2014/11/26, CP: added cloud_albedo
!    (currently deactivated), and nisemask; commented out reading of variables
!    for water within if condition iphase = 2 (never true for water)
! 2014/12/02, CP: reads in global and source attributes from file
! 2015/01/26, CP: add in ml from IR only option changed to common constants
! 2015/07/06, OS: changed some nf90_attributes to their naming convention in
!    common/orac_ncdf.F90; changed cct to cc_total
! 2015/07/16, GM: Major cleanup and made use of the NetCDF interface in the
!    common library.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_inter_file_common(ncid,l2_input_primary,xdim,ydim,indexing, &
   global_atts,verbose)

   use global_attributes
   use netcdf
   use orac_ncdf
   use structures_pp
   use vartypes_pp

   implicit none

   integer,                          intent(in)    :: ncid
   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_primary
   integer(kind=lint),               intent(in)    :: xdim,ydim
   type(counts_and_indexes),         intent(in)    :: indexing
   type(global_attributes_s),        intent(in)    :: global_atts
   logical,                          intent(in)    :: verbose

   integer            :: i
   character(len=32)  :: input_num
   character(len=512) :: input_dummy

   call nc_read_packed_array(ncid, "cot", l2_input_primary%cot, verbose)
   call nc_read_packed_array(ncid, "cot_uncertainty", l2_input_primary%cot_uncertainty, verbose)

   call nc_read_packed_array(ncid, "ref", l2_input_primary%ref, verbose)
   call nc_read_packed_array(ncid, "ref_uncertainty", l2_input_primary%ref_uncertainty, verbose)

   call nc_read_packed_array(ncid, "ctp", l2_input_primary%ctp, verbose)
   call nc_read_packed_array(ncid, "ctp_uncertainty", l2_input_primary%ctp_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cc_total", l2_input_primary%cc_total, verbose)
   call nc_read_packed_array(ncid, "cc_total_uncertainty", l2_input_primary%cc_total_uncertainty, verbose)

   call nc_read_packed_array(ncid, "stemp", l2_input_primary%stemp, verbose)
   call nc_read_packed_array(ncid, "stemp_uncertainty", l2_input_primary%stemp_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cth", l2_input_primary%cth, verbose)
   call nc_read_packed_array(ncid, "cth_uncertainty", l2_input_primary%cth_uncertainty, verbose)

   call nc_read_packed_array(ncid, "ctt", l2_input_primary%ctt, verbose)
   call nc_read_packed_array(ncid, "ctt_uncertainty", l2_input_primary%ctt_uncertainty, verbose)

   call nc_read_packed_array(ncid, "cwp", l2_input_primary%cwp, verbose)
   call nc_read_packed_array(ncid, "cwp_uncertainty", l2_input_primary%cwp_uncertainty, verbose)

   call nc_read_array(ncid, "convergence", l2_input_primary%convergence, verbose)

   call nc_read_array(ncid, "niter", l2_input_primary%niter, verbose)

   do i=1,indexing%NSolar
      if (btest(indexing%Ch_Is(i), SolarBit)) then
         write(input_num,"(i4)") indexing%Y_Id(i)
         input_dummy='cloud_albedo_in_channel_no_'//trim(adjustl(input_num))
         call nc_read_packed_array(ncid, input_dummy, l2_input_primary%cloud_albedo(:,:,i), verbose)
      endif
   end do

   call nc_read_array(ncid, "costja", l2_input_primary%costja, verbose)
   call nc_read_array(ncid, "costjm", l2_input_primary%costjm, verbose)

   call nc_read_array(ncid, "qcflag", l2_input_primary%qcflag, verbose)
   where(l2_input_primary%qcflag .eq. sint_fill_value) l2_input_primary%qcflag = -1

end subroutine read_inter_file_common


subroutine read_inter_file_all(fname,l2_input_2d_primary,xdim,ydim,indexing, &
                               global_atts,source_atts,verbose)

   use global_attributes
   use netcdf
   use orac_ncdf
   use source_attributes
   use structures_pp
   use vartypes_pp

   implicit none

   character(len=path_length),       intent(in)    :: fname
   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary
   integer(kind=lint),               intent(in)    :: xdim,ydim
   type(counts_and_indexes),         intent(in)    :: indexing
   type(global_attributes_s),        intent(inout) :: global_atts
   type(source_attributes_s),        intent(inout) :: source_atts
   logical,                          intent(in)    :: verbose

   integer :: ncid

   write(*,*) 'Opening primary input file: ', trim(fname)
   call nc_open(ncid,fname)

   call nc_get_common_attributes(ncid, global_atts, source_atts)

   call read_inter_file_common(ncid, l2_input_2d_primary, xdim, ydim, indexing, &
      global_atts, verbose)

   call nc_read_array(ncid, "time", l2_input_2d_primary%time, verbose)

   call nc_read_array(ncid, "lon", l2_input_2d_primary%lon, verbose)
   call nc_read_array(ncid, "lat", l2_input_2d_primary%lat, verbose)

   call nc_read_array(ncid, "solar_zenith_view_no1", l2_input_2d_primary%solar_zenith_view_no1, verbose)
   call nc_read_array(ncid, "satellite_zenith_view_no1", l2_input_2d_primary%satellite_zenith_view_no1, verbose)
   call nc_read_array(ncid, "rel_azimuth_view_no1", l2_input_2d_primary%rel_azimuth_view_no1, verbose)

!  call nc_read_array(ncid, "phase", l2_input_2d_primary%phase, verbose)

   call nc_read_array(ncid, "lsflag", l2_input_2d_primary%lsflag, verbose)

   call nc_read_array(ncid, "illum", l2_input_2d_primary%illum, verbose)

   call nc_read_array(ncid, "cldtype", l2_input_2d_primary%cldtype, verbose)

   call nc_read_array(ncid, "cldmask", l2_input_2d_primary%cldmask, verbose)

   call nc_read_array(ncid, "cccot_pre", l2_input_2d_primary%cccot_pre, verbose)

   call nc_read_array(ncid, "lusflag", l2_input_2d_primary%lusflag, verbose)

!  call nc_read_array(ncid, "dem", l2_input_2d_primary%dem, verbose)
!  where(l2_input_2d_primary%qcflag .eq. sint_fill_value) l2_input_2d_primary%qcflags = -1

   call nc_read_array(ncid, "nisemask", l2_input_2d_primary%nisemask, verbose)

   write(*,*) 'Closing primary input file.'
   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_inter_file_all


subroutine read_inter_file_class(fname,l2_input_2d_primary,xdim,ydim,indexing, &
   global_atts,verbose)

   use global_attributes
   use netcdf
   use orac_ncdf
   use structures_pp
   use vartypes_pp

   implicit none

   character(len=path_length),       intent(in)    :: fname
   type(l2_input_struct_2d_primary), intent(inout) :: l2_input_2d_primary
   integer(kind=lint),               intent(in)    :: xdim,ydim
   type(counts_and_indexes),         intent(in)    :: indexing
   type(global_attributes_s),        intent(in)    :: global_atts
   logical,                          intent(in)    :: verbose

   integer :: ncid

   call nc_open(ncid,fname)

   call read_inter_file_common(ncid, l2_input_2d_primary, xdim, ydim, indexing, &
      global_atts, verbose)

   if (nf90_close(ncid) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

end subroutine read_inter_file_class
