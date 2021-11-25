!-------------------------------------------------------------------------------
! Name: select_modis_emiss_file.F90
!
! Purpose:
! Select the matching emissivity file.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! cyear                string in Year, as a 4 character string
! cmonth               string in Month, as a 2 character string
! camel_emis_path      string in Folder containing CAMEL monthly average emissivity
! camel_emis_path_file string out Desired CAMEL monthly average emissivity file
!
! History:
! 2018/03/22, SRP: Initial version, based on existing select_modis_emiss_file.F90
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine select_camel_emiss_file(cyear, cmonth, camel_emis_path, &
     camel_emis_path_file)

   use preproc_structures_m

   implicit none

   character(len=*), intent(in)  :: cyear
   character(len=*), intent(in)  :: cmonth
   character(len=*), intent(in)  :: camel_emis_path
   character(len=*), intent(out) :: camel_emis_path_file

   logical                       :: camel_emis_file_exist
   character(len=7)              :: camel_emis_file_read

   camel_emis_path_file = trim(adjustl(camel_emis_path))// &
      '/CAM5K30EM_emis_'// &
      trim(adjustl(cyear))// &
      trim(adjustl(cmonth))//'_V002.nc'

   ! Check that the defined file exists and is readable
   inquire(file=trim(camel_emis_path_file), exist=camel_emis_file_exist, &
      read=camel_emis_file_read)
   if (.not.camel_emis_file_exist) then
      camel_emis_path_file = trim(adjustl(camel_emis_path))// &
      '/CAM5K30EM_emis_'// &
      trim(adjustl(cyear))// &
      trim(adjustl(cmonth))//'_V001.nc'
       inquire(file=trim(camel_emis_path_file), exist=camel_emis_file_exist, &
          read=camel_emis_file_read)
       if (.not.camel_emis_file_exist) then
          write(*,*) 'ERROR: select_camel_emiss_file(): camel surface ' // &
                     'emissivity file does not exist, filename: ', &
                     trim(camel_emis_path_file)
          stop error_stop_code
       endif
   endif
   if (trim(camel_emis_file_read).eq.'NO') then
      write(*,*) 'ERROR: select_camel_emiss_file(): camel surface ' // &
                 'emissivity file exists but is not readable, filename: ', &
                 trim(camel_emis_path_file)
      stop error_stop_code
   end if

end subroutine select_camel_emiss_file
