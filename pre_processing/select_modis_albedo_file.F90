!-------------------------------------------------------------------------------
! Name: select_modis_albedo_file.F90
!
! Purpose:
! Populates the surface albedo part of the surface structure, using MODIS MCD43C
! spectral albedo over the land the cox_munk ocean surface reflectance model
! over the sea.
!
! Description and Algorithm details:
!
! Arguments:
! Name                 Type       In/Out/Both Description
! ------------------------------------------------------------------------------
! cyear                character  in          Year, as a 4 character string
! cdoy                 character  in          DOY,  as a 3 character string
! modis_surf_path      character  in          Path to directory with the
!                                             MCD43C3.A* files
! modis_surf_path_file character  out         Path the the required MCD43C3.A*
!                                             file
!
! History:
! 2012/08/06, CP: Original version
! 2012/08/16, GT: Removed unused variable "mcd_date".
! 2012/08/06, CP: Added in file name extension for 2009 and changed how path is
!    defined i.e year is no longer required in the path name.
! 2012/02/25, CP: Added in 2010 and 2007 data.
! 2013/03/07, GT: Reverted change made by CP on 2012/08/06, and added code to
!    check that the MODIS file exists and is readable.
! 2014/04/21, GM: Cleaned up the code.
! 2014/05/26, MJ: Added "FAILED" to error output.
! 2014/08/10, GM: Changes related to new BRDF support.
! 2014/11/26, CP: Added in 2002-2006 support.
! 2014/12/09, GM: Added support for 2013, better error checking, and cleaned up
!    changes made by CP.
! 2015/03/12, GM: Completed support for 2002 to 2014.
! 2015/07/05, CP: Bug fix for 2007 support
! 2015/08/05, CP: Added in use of climatology and fixed some bugs in file names
! 2015/09/27, GM: The above change assumed that the year was in the path which
!    we should not do.  Reverted this and a little cleanup.
! 2015/10/06, GM: Make use of subroutine match_file() in the common/system_utils
!    module.  No more need to hard-wire processing dates.
! 2016/09/15, SP: Allow use of collection 6 MODIS data. Default is collection 5.
! 2018/05/24, SP: MODIS Collection 6 data is daily, so check for a daily file
!                 first. Then fall back to 8-day C6, then 8-day C5.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine select_modis_albedo_file(cyear, cdoy, modis_surf_path, &
                                    include_full_brdf, modis_surf_path_file)

   use preproc_structures_m
   use system_utils_m

   implicit none

   character(len=*), intent(in)  :: cyear
   character(len=*), intent(in)  :: cdoy
   character(len=*), intent(in)  :: modis_surf_path
   logical,          intent(in)  :: include_full_brdf
   character(len=*), intent(out) :: modis_surf_path_file

   integer                                       :: nv
   integer                                       :: doy
   integer(kind=sint), allocatable, dimension(:) :: dates
   integer(kind=sint), allocatable, dimension(:) :: date_diffs
   integer                                       :: pos(1)
   character(len=3)                              :: mcd_date_s
   integer                                       :: iyear
   character(len=date_length)                    :: cyear2
   character(len=7)                              :: prefix
   character(len=path_length)                    :: regex
   character(len=path_length)                    :: file_name
   logical                                       :: modis_surf_file_exist
   character(len=7)                              :: modis_surf_file_read

   nv = 46

   allocate(dates(nv))

   dates = (/  1,  9, 17, 25, 33, 41, 49, 57, 65, 73, 81, 89, 97,105,113,121, &
             129,137,145,153,161,169,177,185,193,201,209,217,225,233,241,249, &
             257,265,273,281,289,297,305,313,321,329,337,345,353,361/)

   read(cyear, *) iyear

   if (iyear .lt. 2002) then
      cyear2 = 'XXXX'
   else
      cyear2 = cyear
   end if

   if (include_full_brdf) then
      prefix = 'MCD43C1'
   else
      prefix = 'MCD43C3'
   end if

   regex = prefix//'\.A'//trim(adjustl(cyear2))//trim(adjustl(cdoy))// &
           '\.006\..............'//'\.hdf$'

   if (match_file(trim(modis_surf_path), trim(regex), file_name) .ne. 0) then

      ! Find the closest data
      allocate(date_diffs(nv))
      read(cdoy, *) doy
      date_diffs = abs(dates-doy)

      pos = minloc(date_diffs)

      write(mcd_date_s, '(I0.3)') dates(pos(1))

      ! Search for the file with an unknown processing date
      regex = prefix//'\.A'//trim(adjustl(cyear2))//trim(adjustl(mcd_date_s))// &
           '\.061\..............'//'\.hdf$'
          if (match_file(trim(modis_surf_path), trim(regex), file_name) .ne. 0) then
          regex = prefix//'\.A'//trim(adjustl(cyear2))//trim(adjustl(mcd_date_s))// &
               '\.006\..............'//'\.hdf$'
          if (match_file(trim(modis_surf_path), trim(regex), file_name) .ne. 0) then
             regex = prefix//'\.A'//trim(adjustl(cyear2))//trim(adjustl(mcd_date_s))// &
                  '\.005\..............'//'\.hdf$'
             if (match_file(trim(modis_surf_path), trim(regex), file_name) .ne. 0) then
                write(*,*) 'ERROR: select_modis_albedo_file(): Unable to locate ' // &
                     'MODIS albedo file: ', trim(modis_surf_path)//'/'//trim(regex)
                stop error_stop_code
             end if
          end if
      end if
      deallocate(date_diffs)
   end if

   modis_surf_path_file = trim(modis_surf_path)//'/'//trim(file_name)

   ! Check that the defined file exists and is readable
   inquire(file=trim(modis_surf_path_file), exist=modis_surf_file_exist, &
      read=modis_surf_file_read)
   if (.not.modis_surf_file_exist) then
      write(*,*) 'ERROR: select_modis_albedo_file(): MODIS surface albedo ' // &
                 'file does not exist, filename: ', trim(modis_surf_path_file)
      stop error_stop_code
   else if (trim(modis_surf_file_read).eq.'NO') then
      write(*,*) 'ERROR: select_modis_albedo_file(): MODIS surface albedo ' // &
                 'file exists but is not readable, filename: ', &
                 trim(modis_surf_path_file)
      stop error_stop_code
   end if

   deallocate(dates)
end subroutine select_modis_albedo_file
