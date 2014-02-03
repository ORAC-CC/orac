! Name:
!    Data
!
! Purpose:
!    Module defining auxiliary and image data structures/types.
!
! Description:
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name       Type    Description
!
! History:
!    19 December 2000: Kevin M. Smith original version
!    ****************** ECV work starts here ************************
!    22nd Feb 2011, Andy Smith:
!      Re-applying changes made in late 2001/2002 detailed in commments below.
!    27 May 2002: Caroline Poulsen added in albedo ALB
!    39 July 2002 changed the definition of alb to kind
!   23rd Feb 2011, Andy Smith:
!      Changed CloudFlags to real(4) to match current ORAC data. 
!   14th Apr 2011, Andy Smith:
!      Extension to handle multiple views. Geometry struct extended to 
!      take Sat, Sol and Azi data from >1 view (forward,, nadir etc).
!   28th Jul 2011, Caroline poulsen: added in variable for scan line file
!   15/06/2012, Caroline poulsen: added in variable for illum
!   10/08/2012, Caroline poulsen: remove illum data type
! 2012/08/22, MJ includes time in MSI structure
! 2012/09/20, CP changed albedo to real 8
! 2013 MJ explicitly defines LSFLag byte array type

! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------
module Data_def

   use ECP_Constants

   implicit none
   
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
      real, pointer :: uscan(:,:)
      real, pointer :: vscan(:,:)
   end type Scan_t

   type Data_t
      real(4), pointer :: MSI(:,:,:)
      real(8), pointer :: time(:,:)
      type(Geometry_t) :: Geometry
      type(Location_t) :: Location
      type(Scan_t) :: Scan
      real(4), pointer :: CloudFlags(:,:)
      !      byte,    pointer :: LSFlags(:,:)
      integer(kind=byte), pointer :: LSFlags(:,:)
      real(4), pointer :: ALB(:,:,:)
      integer, pointer :: illum(:,:,:)
   end type Data_t



end module Data_def
