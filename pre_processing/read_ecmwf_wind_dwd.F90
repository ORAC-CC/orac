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
!
! $Id: read_ecmwf_wind_nc.F90 2746 2014-11-21 10:12:39Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_ecmwf_wind_dwd(ecmwf_path, ecmwf)

   use preproc_constants

   implicit none

   character(len=path_length), intent(in)    :: ecmwf_path
   type(ecmwf_s),              intent(inout) :: ecmwf

   ! initialise
   ecmwf%xdim=0
   ecmwf%ydim=0
   ecmwf%kdim=0
   allocate(ecmwf%avec(61))
   allocate(ecmwf%bvec(61))
   ecmwf%avec=[0.000000,     2.000000E+01, 3.842534E+01, 6.364780E+01, &
               9.563696E+01, 1.344833E+02, 1.805844E+02, 2.347791E+02, &
               2.984958E+02, 3.739719E+02, 4.646182E+02, 5.756511E+02, &
               7.132180E+02, 8.836604E+02, 1.094835E+03, 1.356475E+03, &
               1.680640E+03, 2.082274E+03, 2.579889E+03, 3.196422E+03, &
               3.960292E+03, 4.906707E+03, 6.018020E+03, 7.306633E+03, &
               8.765055E+03, 1.037612E+04, 1.207745E+04, 1.377532E+04, &
               1.537980E+04, 1.681947E+04, 1.804518E+04, 1.902770E+04, &
               1.975511E+04, 2.022220E+04, 2.042986E+04, 2.038448E+04, &
               2.009740E+04, 1.958433E+04, 1.886475E+04, 1.796136E+04, &
               1.689947E+04, 1.570645E+04, 1.441112E+04, 1.304322E+04, &
               1.163276E+04, 1.020950E+04, 8.802355E+03, 7.438805E+03, &
               6.144316E+03, 4.941777E+03, 3.850913E+03, 2.887697E+03, &
               2.063780E+03, 1.385913E+03, 8.553618E+02, 4.673335E+02, &
               2.103939E+02, 6.588924E+01, 7.367743,     0.000000,     &
               0.000000 ]
   ecmwf%bvec=[0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
               0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
               0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
               0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
               0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
               0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
               7.5823496E-05, 4.6139490E-04, 1.8151561E-03, 5.0811172E-03, &
               1.1142910E-02, 2.0677876E-02, 3.4121163E-02, 5.1690407E-02, &
               7.3533833E-02, 9.9674702E-02, 0.1300225,     0.1643843,     &
               0.2024759,     0.2439331,     0.2883230,     0.3351549,     &
               0.3838921,     0.4339629,     0.4847715,     0.5357099,     &
               0.5861684,     0.6355475,     0.6832686,     0.7287858,     &
               0.7715966,     0.8112534,     0.8473749,     0.8796569,     &
               0.9078839,     0.9319403,     0.9518215,     0.9676452,     &
               0.9796627,     0.9882701,     0.9940194,     0.9976301,     &
               1.0000000 ]
   nullify(ecmwf%lon)
   nullify(ecmwf%lat)
   nullify(ecmwf%u10)
   nullify(ecmwf%v10)
   nullify(ecmwf%skin_temp)

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

   use orac_ncdf
   use preproc_constants

   implicit none

   character(len=*), intent(in)    :: ecmwf_path
   type(ecmwf_s),    intent(inout) :: ecmwf

   real, allocatable               :: val(:,:,:,:)
   integer                         :: fid,i,ndim,nvar,size
   character(len=var_length)       :: name
   logical                         :: verbose = .false.

   ! open file
   call nc_open(fid,ecmwf_path)

   ! check field dimensions for consistency
   if (nf90_inquire(fid,ndim,nvar) .ne. 0) &
        stop 'ERROR: read_ecmwf_wind(): Bad inquire.'
   do i=1,ndim
      if (nf90_inquire_dimension(fid,i,name,size) .ne. 0) &
           stop 'ERROR: read_ecmwf_wind(): Bad dimension.'
      if (name .eq. 'lon') then
         if (ecmwf%xdim .eq. 0) then
            ecmwf%xdim=size
         else
            if (ecmwf%xdim .ne. size) &
                 stop 'ERROR: read_ecmwf_wind(): Inconsistent lon.'
         end if
      else if (name .eq. 'lat') then
         if (ecmwf%ydim .eq. 0) then
            ecmwf%ydim=size
         else
            if (ecmwf%ydim .ne. size) &
                 stop 'ERROR: read_ecmwf_wind(): Inconsistent lat.'
         end if
         ! the vertical coordinate is inconsistently between gpam and ggam
      else if (name .eq. 'nhym') then
         if (ecmwf%kdim .eq. 0) then
            ecmwf%kdim=size
         else
            if (ecmwf%kdim .ne. size) &
                 stop 'ERROR: read_ecmwf_wind(): Inconsistent vertical.'
         end if
      end if
   end do

   ! read wind fields and geolocation from files
   do i=1,nvar
      if (nf90_inquire_variable(fid,i,name) .ne. 0) &
           stop 'ERROR: read_ecmwf_wind(): Bad variable.'
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
      end select
   end do

   if (nf90_close(fid) .ne. NF90_NOERR) &
        stop 'ERROR: read_ecmwf_wind(): File could not close.'

end subroutine read_ecmwf_wind_file_dwd
