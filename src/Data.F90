!-------------------------------------------------------------------------------
! Name:
!    Data
!
! Purpose:
!    Module defining auxiliary and image data structures/types.
!
! Arguments:
!    Name  Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!
! History:
!    19th Dec 2000, Kevin M. Smith: Original version
!    **************** ECV work starts here *************************************
!    22nd Feb 2011, Andy Smith:
!       Re-applying changes made in late 2001/2002 detailed in comments below.
!    27th May 2002, Caroline Poulsen:
!       Added in albedo ALB.
!    xxth Jul 2002, Caroline Poulsen:
!       Changed the definition of alb to kind.
!    23rd Feb 2011, Andy Smith:
!       Changed CloudFlags to real(4) to match current ORAC data.
!    14th Apr 2011, Andy Smith:
!       Extension to handle multiple views. Geometry struct extended to take
!       Sat, Sol and Azi data from >1 view (forward,, nadir etc).
!    28th Jul 2011, Caroline Poulsen:
!       Added in variable for scan line file
!    15/06/2012, Caroline Poulsen: added in variable for illum
!    10/08/2012, Caroline Poulsen: remove illum data type
!    22/08/2012, MJ: includes time in MSI structure
!    20/09/2012, CP: changed albedo to real 8
!    XX/XX/2013, MJ: explicitly defines LSFLag byte array type
!    27/05/2014, GM: Some cleanup.
!    2014/09/09, GM: Changes related to new BRDF support.
!    2014/09/17, GM: Added Nullify_Data()
!    2014/10/24, OS: added variables cldtype, cloudmask, cccot_pre, lusflags,
!       dem, and nisemask
!    2015/01/18, GM: Put all related Read*() subroutines into this module.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module Data_def

   use ECP_Constants

   private

   public :: Geometry_t, &
             Location_t, &
             Scan_t, &
             Data_t, &
             Dealloc_Data, &
             Read_Data_nc

   type Geometry_t
      real(4), pointer :: Sol(:,:,:)
      real(4), pointer :: Sat(:,:,:)
      real(4), pointer :: Azi(:,:,:)
   end type Geometry_t

   type Location_t
      real(4), pointer :: Lat(:,:)
      real(4), pointer :: Lon(:,:)
   end type Location_t

   type Scan_t
      real(4), pointer :: uscan(:,:)
      real(4), pointer :: vscan(:,:)
   end type Scan_t

   type Data_t
      real(4), pointer            :: ALB(:,:,:)
      real(4), pointer            :: rho_0v(:,:,:)
      real(4), pointer            :: rho_0d(:,:,:)
      real(4), pointer            :: rho_dv(:,:,:)
      real(4), pointer            :: rho_dd(:,:,:)
      real(4), pointer            :: CloudFlags(:,:)
      integer(kind=byte), pointer :: cldtype(:,:)
      integer(kind=byte), pointer :: cloudmask(:,:)
      real(kind=sreal), pointer   :: cccot_pre(:,:)
      type(Geometry_t)            :: Geometry
      type(Location_t)            :: Location
      integer(kind=byte), pointer :: LSFlags(:,:)
      integer(kind=byte), pointer :: lusflags(:,:)
      integer(kind=sint), pointer :: dem(:,:)
      integer(kind=byte), pointer :: nisemask(:,:)
      real(8),            pointer :: time(:,:)
      real(4),            pointer :: MSI(:,:,:)
      type(Scan_t)                :: Scan
      integer, pointer            :: illum(:,:,:)
   end type Data_t

contains

#include "NullifyData.F90"
#include "DeallocData.F90"

#include "ReadALB_nc.F90"
#include "ReadCloudFlags_nc.F90"
#include "ReadGeometry_nc.F90"
#include "ReadIllum_nc.F90"
#include "ReadLSFlags_nc.F90"
#include "ReadLocation_nc.F90"
#include "ReadMSI_nc.F90"
#include "ReadScanLines_nc.F90"

#include "ReadData_nc.F90"

end module Data_def
