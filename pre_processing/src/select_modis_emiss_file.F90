!-------------------------------------------------------------------------------
! Name: select_modis_emiss_file.F90
!
! Purpose:
! Select the matching emissivity file.
!
! Description and algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! cyear           string  in   Year, as a 4 character string
! doy             integer in   Day of year
! emiss_surf_path string  in   Folder containing MODIS monthly average emissivity
! emiss_surf_path string  in   Desired MODIS monthly average emissivity file
!
! History:
! 2012/08/06, CP: Original version
! 2012/08/16, GT: Extensive rewrite. The code now checks if we're looking at
!   data for a leap year, and sets the day-of-year numbers (which are for the
!   first of each month) accordingly. Date arrays are also no-longer dynamic.
!   Also changed the way the appropriate day-of-year number is selected, so that
!   it is always the closest smaller number to the actual date.
! 2012/08/20, MJ: fixes bug (variable type from int to logical) in
!   inquire statement reads files downloaded from
!   http://cimss.ssec.wisc.edu/iremis/download.php
! 2013/06/27, MJ: Implements file independent checking for leap year
! 2013/11/01, GM: Fixed leap year check to include all cases.  This
!   code is now 2100 ready:)
! 2013/11/01, GM: Cleaned up code.
! 2014/04/21, GM: Added logical option assume_full_path.
! 2014/05/26, MJ: fixed error with datatype of year for mod-operation.

! $Id$
!
!-------------------------------------------------------------------------------
subroutine select_modis_emiss_file(cyear,doy,emiss_surf_path, &
     emiss_surf_path_file)

   use preproc_structures

   implicit none

   character(len=datelength), intent(in)  :: cyear
   integer(kind=stint),       intent(in)  :: doy
   character(len=pathlength), intent(in)  :: emiss_surf_path
   character(len=pathlength), intent(out) :: emiss_surf_path_file

   integer(kind=stint)                :: year
   integer                            :: pos
   logical                            :: isleapyear
   integer(kind=stint), dimension(12) :: dates,newdates
   character(len=3),    dimension(12) :: dates_s
   character(len=3)                   :: emis_date_s

   read(cyear, *) year

   isleapyear=.false.

   if ((mod(year,  4_stint) .eq. 0 .and. mod(year,100_stint) .ne. 0) .or. &
        mod(year,400_stint) .eq. 0) isleapyear=.true.

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

   newdates=dates-doy

   pos=count(newdates .le. 0)

   emis_date_s=dates_s(pos)

   emiss_surf_path_file=trim(adjustl(emiss_surf_path))// &
        '/global_emis_inf10_monthFilled_MYD11C3.A'// &
        trim(adjustl(cyear))// &
        trim(adjustl(emis_date_s))//'.041'//'.nc'



end subroutine select_modis_emiss_file
