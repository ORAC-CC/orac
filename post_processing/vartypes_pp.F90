!
! Name: vartypes_pp.f90 
!
!
! Purpose: F90 Module file which declares variable types and some parameters.
! 
! 
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
!2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
!2012/03/06 Caroline Poulsen modied to produce post processed files
! 2012/07/06 MJ extensively overhauls and restructures the code
! 2014/04/01 Matthias Jerg: fixes some problems/cleanup with illumination
! 2014/11/10 OS: very minor edit
! 2014/12/03 CP: removed variables duplicated in common_constants
! 2015/02/05 OS: some cleanup; changed nint to lint

! $Id$
!
! Bugs:
!
!none known

module vartypes_pp

use common_constants


  implicit none

   integer, parameter :: PrimaryFileOpenErr = 1400
   integer, parameter :: SecondaryFileOpenErr = 1401
   integer, parameter :: PrimaryFileDefinitionErr = 1402
   integer, parameter :: SecondaryFileDefinitionErr = 1403
   integer, parameter :: PrimaryFileWriteErr = 1404
   integer, parameter :: SecondaryFileWriteErr = 1405
   integer, parameter :: PrimaryFileCloseErr = 1406
   integer, parameter :: SecondaryFileCloseErr = 1407



  integer, parameter :: currentlength=512
  integer, parameter :: cpathlength=1024
  integer, parameter :: filelength=512!128
  integer, parameter :: commandlength=64
  integer, parameter :: cfilelength=32
  integer, parameter :: paramlength=150!16
  integer, parameter :: dummylength=128
  integer, parameter :: varlength=150

  integer, parameter :: inlength=2
  integer, parameter :: uuid_length=36
  integer, parameter :: l3_outputpath_and_file_length=512
  integer, parameter :: description_length=2048
  integer, parameter :: MaxStateVar = 5  ! Max. number of state variables

  real(kind=sreal), parameter :: real_fill_value=-999.0, filter_thres=-500.0, filter_micro=0.00, dither=1.0E-5
  real(kind=sreal), parameter :: norm_factor=1.0E-3,dither3=1.0E-3, reduce_cost=99.0

  integer(kind=sint), parameter :: short_int_fill_value=-99

  real(kind=dreal), parameter :: double_fill_value=-999.0

  integer(kind=lint), parameter :: n_features=10,n_error_features=1,n_variables=10

  integer(kind=lint), parameter :: n_hist_phase=2,n_hist_cot=7,n_hist_ctp=8

  integer(kind=lint), parameter :: n_cot_bins=11,n_ctp_bins=10, n_ctt_bins=12

  integer(kind=lint) :: n_remedian_base, n_remedian_exponent

  real(kind=sreal), parameter :: e_radius=6371.0

  real(kind=sreal), parameter :: min_range=12.0

  real(kind=sreal), parameter :: ctp_high=440.0, ctp_middle=680.0

end module vartypes_pp
