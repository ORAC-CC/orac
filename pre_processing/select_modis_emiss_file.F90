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
! cyear           string in Year, as a 4 character string
! cdoy            string in DOY,  as a 2 character string
! emiss_surf_path string in Folder containing MODIS monthly average emissivity
! emiss_surf_path string in Desired MODIS monthly average emissivity file
!
! History:
! 2012/08/06, CP: Original version
! 2012/08/16, GT: Extensive rewrite. The code now checks if we're looking at
!    data for a leap year, and sets the day-of-year numbers (which are for the
!    first of each month) accordingly. Date arrays are also no-longer dynamic.
!    Also changed the way the appropriate day-of-year number is selected, so that
!    it is always the closest smaller number to the actual date.
! 2012/08/20, MJ: fixes bug (variable type from int to logical) in
!    inquire statement reads files downloaded from
!    http://cimss.ssec.wisc.edu/iremis/download.php
! 2013/06/27, MJ: Implements file independent checking for leap year
! 2013/11/01, GM: Fixed leap year check to include all cases.  This
!    code is now 2100 ready:)
! 2013/11/01, GM: Cleaned up code.
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/05/26, MJ: Fixed error with data type of year for mod-operation.
! 2014/11/26, CP: Modified for change in file versioning numbering prior to
!    2007.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine select_modis_emiss_file(cyear,cdoy,cimss_emis_path, &
     cimss_emis_path_file)

   use preproc_structures

   implicit none

   character(len=date_length), intent(in)  :: cyear
   character(len=date_length), intent(in)  :: cdoy
   character(len=path_length), intent(in)  :: cimss_emis_path
   character(len=path_length), intent(out) :: cimss_emis_path_file

   integer(kind=sint)                 :: year
   integer(kind=sint)                 :: doy
   integer                            :: pos
   logical                            :: isleapyear
   integer(kind=sint), dimension(12)  :: dates,newdates
   character(len=3),    dimension(12) :: dates_s
   character(len=3)                   :: emis_date_s
   logical                            :: cimss_emis_file_exist
   character(len=7)                   :: cimss_emis_file_read

   read(cyear, *) year

   isleapyear=.false.

   if ((mod(year,  4_sint) .eq. 0 .and. mod(year,100_sint) .ne. 0) .or. &
        mod(year,400_sint) .eq. 0) isleapyear=.true.

   if (isleapyear) then
      dates(1)=1
      dates(2)=32
      dates(3)=61
      dates(4)=92
      dates(5)=122
      dates(6)=153
      dates(7)=183
      dates(8)=214
      dates(9)=245
      dates(10)=275
      dates(11)=306
      dates(12)=336

      dates_s(1)='001'
      dates_s(2)='032'
      dates_s(3)='061'
      dates_s(4)='092'
      dates_s(5)='122'
      dates_s(6)='153'
      dates_s(7)='183'
      dates_s(8)='214'
      dates_s(9)='245'
      dates_s(10)='275'
      dates_s(11)='306'
      dates_s(12)='336'
   else
      dates(1)=1
      dates(2)=32
      dates(3)=60
      dates(4)=91
      dates(5)=121
      dates(6)=152
      dates(7)=182
      dates(8)=213
      dates(9)=244
      dates(10)=274
      dates(11)=305
      dates(12)=335

      dates_s(1)='001'
      dates_s(2)='032'
      dates_s(3)='060'
      dates_s(4)='091'
      dates_s(5)='121'
      dates_s(6)='152'
      dates_s(7)='182'
      dates_s(8)='213'
      dates_s(9)='244'
      dates_s(10)='274'
      dates_s(11)='305'
      dates_s(12)='335'
   end if

   read(cdoy, *) doy
   newdates=dates-doy

   pos=count(newdates .le. 0)

   emis_date_s=dates_s(pos)

   ! files earlier than 2007 do not have the version number in the name
   if ( year .le. 2007) then
      cimss_emis_path_file=trim(adjustl(cimss_emis_path))// &
         '/global_emis_inf10_monthFilled_MYD11C3.A'// &
         trim(adjustl(cyear))// &
         trim(adjustl(emis_date_s))//'.nc'
   else
      cimss_emis_path_file=trim(adjustl(cimss_emis_path))// &
         '/global_emis_inf10_monthFilled_MYD11C3.A'// &
         trim(adjustl(cyear))// &
         trim(adjustl(emis_date_s))//'.041'//'.nc'
   endif

   ! Check that the defined file exists and is readable
   inquire(file=trim(cimss_emis_path_file), exist=cimss_emis_file_exist, &
      read=cimss_emis_file_read)
   if (.not.cimss_emis_file_exist) then
      write(*,*) 'ERROR: select_modis_emiss_file(): CIMSS surface emissivity ' // &
               & 'file does not exist, filename: ', trim(cimss_emis_path_file)
      stop error_stop_code
   else if (trim(cimss_emis_file_read).eq.'NO') then
      write(*,*) 'ERROR: select_modis_emiss_file(): CIMSS surface emissivity ' // &
               & 'file exists but is not readable, filename: ', trim(cimss_emis_path_file)
      stop error_stop_code
   end if

end subroutine select_modis_emiss_file
