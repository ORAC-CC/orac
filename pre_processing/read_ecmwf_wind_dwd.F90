!-------------------------------------------------------------------------------
! Name: read_ecmwf_wind_dwd.F90
!
! Purpose:
! Read surface wind components and lat/lon from single ECMWF file in NetCDF format.
! Successor to read_ecmwf_wind_nc.F90.
!
! Description and Algorithm details:
! 1) Set a|bvector values (set in interpolation).
! 2) Process file.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in   Full path to a ECMWF NCDF file to read.
! ecmwf      struct both Structure summarising contents of ECMWF files.
!
! History:
! 2014/12/17, OS: First version.
! 2015/07/03, OS: Added error status variable to nc_open call.
! 2015/07/10, OS: Undo previous commit.
! 2015/11/17, OS: Added snow_depth and sea_ice_cover reading.
! 2016/02/03, GM: Call ecmwf_wind_init() to be consistent with the other wind
!    readers.
!
! $Id: read_ecmwf_wind_nc.F90 2746 2014-11-21 10:12:39Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_dwd(ecmwf_path, ecmwf)

   use preproc_constants_m

   implicit none

   character(len=*), intent(in)    :: ecmwf_path
   type(ecmwf_t),    intent(inout) :: ecmwf

   call ecmwf_wind_init(ecmwf)

   allocate(ecmwf%avec(61))
   allocate(ecmwf%bvec(61))
   ecmwf%avec = avec
   ecmwf%bvec = bvec

   ! loop over given files (order not necessarily known)
   call read_ecmwf_wind_file_dwd(ecmwf_path,ecmwf)

end subroutine read_ecmwf_wind_dwd

!-------------------------------------------------------------------------------
! Name: read_ecmwf_wind_file
!
! Purpose:
! Read surface wind components and lat/lon from ECMWF file.
! Successor to read_ecmwf_wind_nc.F90.
!
! Description and Algorithm details:
! 1) Open file.
! 2) Search for desired dimensions. If size unknown, save. Otherwise, check
!    it is consistent with the previous value.
! 3) Search for desired variables. If not yet stored, allocate ecmwf structure
!    and store array.
! 4) Close file.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path string in   Full path to a ECMWF NCDF file to read.
! ecmwf      struct both Structure summarising contents of ECMWF files.
!
! History:
! 2014/05/07, AP: First version.
! 2014/11/04, AP: Added skin_temp reading.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_file_dwd(ecmwf_path,ecmwf)

   use orac_ncdf_m
   use preproc_constants_m

   implicit none

   character(len=*), intent(in)    :: ecmwf_path
   type(ecmwf_t),    intent(inout) :: ecmwf

   real, allocatable               :: val(:,:,:,:)
   integer                         :: fid,i,ndim,nvar,size
   character(len=var_length)       :: name
   logical                         :: verbose = .false.

   ! open file
   call nc_open(fid,ecmwf_path)

   ! check field dimensions for consistency
   if (nf90_inquire(fid,ndim,nvar) .ne. 0) &
        call h_e_e('wind_dwd', 'Bad inquire.')
   do i=1,ndim
      if (nf90_inquire_dimension(fid,i,name,size) .ne. 0) &
           call h_e_e('wind_dwd', 'Bad dimension.')
      if (name .eq. 'lon') then
         if (ecmwf%xdim .eq. 0) then
            ecmwf%xdim=size
         else
            if (ecmwf%xdim .ne. size) call h_e_e('wind_dwd', 'Inconsistent lon.')
         end if
      else if (name .eq. 'lat') then
         if (ecmwf%ydim .eq. 0) then
            ecmwf%ydim=size
         else
            if (ecmwf%ydim .ne. size) call h_e_e('wind_dwd', 'Inconsistent lat.')
         end if
         ! the vertical coordinate is inconsistently between gpam and ggam
      else if (name .eq. 'nhym') then
         if (ecmwf%kdim .eq. 0) then
            ecmwf%kdim=size
         else
            if (ecmwf%kdim .ne. size) &
                 call h_e_e('wind_dwd', 'Inconsistent vertical.')
         end if
      end if
   end do

   ! read wind fields and geolocation from files
   do i=1,nvar
      if (nf90_inquire_variable(fid,i,name) .ne. 0) &
           call h_e_e('wind_dwd', 'Bad variable.')
      select case (name)
      case('lon')
         if (.not.associated(ecmwf%lon)) then
            allocate(ecmwf%lon(ecmwf%xdim))
            call nc_read_array(fid,name,ecmwf%lon,verbose)
         end if
      case('lat')
         if (.not.associated(ecmwf%lat)) then
            allocate(ecmwf%lat(ecmwf%ydim))
            call nc_read_array(fid,name,ecmwf%lat,verbose)
         end if
      case('U10M')
         if (.not.associated(ecmwf%u10)) then
            allocate(ecmwf%u10(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call nc_read_array(fid,name,val,verbose)
            ecmwf%u10=val(:,:,1,1)
            deallocate(val)
         end if
      case('V10M')
         if (.not.associated(ecmwf%v10)) then
            allocate(ecmwf%v10(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call nc_read_array(fid,name,val,verbose)
            ecmwf%v10=val(:,:,1,1)
            deallocate(val)
         end if
      case('SKT')
         if (.not.associated(ecmwf%skin_temp)) then
            allocate(ecmwf%skin_temp(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call nc_read_array(fid,name,val,verbose)
            ecmwf%skin_temp=val(:,:,1,1)
            deallocate(val)
         end if
      case('SD','sd')
         if (.not.associated(ecmwf%snow_depth)) then
            allocate(ecmwf%snow_depth(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call nc_read_array(fid,name,val,verbose)
            ecmwf%snow_depth=val(:,:,1,1)
            deallocate(val)
         end if
      case('CI','ci')
         if (.not.associated(ecmwf%sea_ice_cover)) then
            allocate(ecmwf%sea_ice_cover(ecmwf%xdim,ecmwf%ydim))
            allocate(val(ecmwf%xdim,ecmwf%ydim,1,1))
            call nc_read_array(fid,name,val,verbose)
            ecmwf%sea_ice_cover=val(:,:,1,1)
            deallocate(val)
         end if
      end select
   end do

   if (nf90_close(fid) .ne. NF90_NOERR) &
        call h_e_e('wind_dwd', 'File could not close.')

end subroutine read_ecmwf_wind_file_dwd
