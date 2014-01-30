! Name: preproc_constants.f90
!
!
! Purpose:
! Define here data types, string lengths, constants etc.
! 
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
! 2011/12/09: MJ produces draft code with basic data types and lengths
! 2012/04/19: GT Added value of pi.
! 20131127 MJ adds parameters for netcdf4 compression
! 20140123 MJ switches nc4 shuffling off.
! 20140130 MJ removes parameters for chunking.
!
! $Id$
!
! Bugs:
! none known
!

module preproc_constants

  implicit none
  
  integer, parameter :: pathlength=1024
  integer, parameter :: filelength=512
  integer, parameter :: varlength=64
  integer, parameter :: flaglength=1
  integer, parameter :: sensorlength=5
  integer, parameter :: platformlength=8
  integer, parameter :: pixellength=10
  integer, parameter :: datelength=12
  integer, parameter :: unitlength=75
  integer, parameter :: errlength=100
  integer, parameter :: attribute_length=75
  integer, parameter :: description_length=2048
  integer, parameter :: uuid_length=36

  integer, parameter :: sint=1
  integer, parameter :: stint=2
  integer, parameter :: lint=4
  
  integer, parameter :: sreal=4
  integer, parameter :: dreal=8
  
  real(kind=sreal), parameter :: real_fill_value=-999.0, filter_thres=-500.0, &
       filter_micro=0.00,dither=1.0E-3,dither_more=1.0E-7

  real(kind=sreal), parameter :: pa2hpa=100.0

  real(kind=sreal), parameter :: maxsza_day=80.0,maxsza_twi=110.0

  real(kind=dreal), parameter :: double_fill_value=-999.0

  integer(kind=lint) :: long_int_fill_value=-999

  integer(kind=sint), parameter :: byte_fill_value=-1

  real(kind=sreal), parameter :: pi = 3.1459265
  real(kind=sreal), parameter :: d2r = 0.017453292   ! Pi/180.0
  
  !specific gas constants of dry air and water vapor
  real(kind=sreal), parameter :: r_dry_air=287.05,r_water_vap=461.51
  !WMO defined gravity constant which is constant  for all latitude and heights
  !used to convert from gph (m^2/s^2) to height (m) by h=gph/g_wmo
  real(kind=sreal), parameter :: g_wmo=9.80665

  !some netcdf4 related parameters
  !compression levels for variables of different type (0:none,9:maximum)
  integer(kind=lint), parameter :: compress_level_float=0
  integer(kind=lint), parameter :: compress_level_double=0
  integer(kind=lint), parameter :: compress_level_lint=0
  integer(kind=lint), parameter :: compress_level_nint=0
  integer(kind=lint), parameter :: compress_level_stint=0
  integer(kind=lint), parameter :: compress_level_byte=0
  integer(kind=lint), parameter :: compress_level_stint_flag=0


  !turn on shuffling to improve compression
  logical, parameter :: shuffle_float=.FALSE.
  logical, parameter :: shuffle_double=.FALSE.
  logical, parameter :: shuffle_lint=.FALSE.
  logical, parameter :: shuffle_nint=.FALSE.
  logical, parameter :: shuffle_stint=.FALSE.
  logical, parameter :: shuffle_byte=.FALSE.
  logical, parameter :: shuffle_stint_flag=.FALSE.



end module preproc_constants
