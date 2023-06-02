!-------------------------------------------------------------------------------
! Name: data.F90
!
! Purpose:
! Module defining auxiliary and image data structures/types.
!
! History:
! 2000/12/19, KS: Original version
!    **************** ECV work starts here *************************************
! 2001/02/22, AS: Re-applying changes made in late 2001/2002 detailed in
!    comments below.
! 2002/05/27, CP: Added in albedo ALB.
! 2002/07/xx, CP: Changed the definition of alb to kind.
! 2011/02/23, AS: Changed CloudFlags to real(4) to match current ORAC data.
! 2011/04/14, AS: Extension to handle multiple views. Geometry struct extended
!    to take Sat, Sol and Azi data from >1 view (forward,, nadir etc).
! 2011/07/28, CP: Added in variable for scan line file
! 2012/06/15, CP: added in variable for illum
! 2012/08/10, CP: remove illum data type
! 2012/08/22, MJ: includes time in MSI structure
! 2012/09/20, CP: changed albedo to real 8
! 2013/XX/XX, MJ: explicitly defines LSFLag byte array type
! 2014/05/27, GM: Some cleanup.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/09/17, GM: Added Nullify_Data()
! 2014/10/24, OS: added variables cldtype, cldmask, cccot_pre, lusflags, dem,
!    and nisemask
! 2015/01/18, GM: Put all related Read*() subroutines into this module.
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2015/02/04, GM: Changes related to the new missing channel, illumination, and
!    channel selection code.
! 2015/04/28, AP: Added fields for surface uncertainty and correlation.
! 2015/07/03, OS: Added cldmask_uncertainty
! 2015/07/27, AP: Replace CloudFlag with Type.
! 2016/04/28, AP: Add multiple views.
! 2017/06/21, OS: Added ANN phase variables.
! 2018/06/08, SP: Add satellite azimuth angle to output.
! 2022/01/27, GT: Add State type, to hold a priori/first guess state parameters
!    read from auxiliary files. So far, it only holds CTP.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module Data_m

   use ORAC_Constants_m

   private

   public :: Geometry_t, &
             Location_t, &
             Scan_t, &
             State_t, &
             Data_t, &
             Dealloc_Data, &
             Read_Data

   type Geometry_t
      real, pointer :: Sol(:,:,:)
      real, pointer :: Sat(:,:,:)
      real, pointer :: Azi(:,:,:)
      real, pointer :: Saz(:,:,:)
   end type Geometry_t

   type Location_t
      real, pointer :: Lat(:,:)
      real, pointer :: Lon(:,:)
   end type Location_t

   type State_t
      real, pointer :: CTP(:,:)
      real, pointer :: CTP_var(:,:)
   end type State_t

   type Data_t
      real,               pointer :: ALB(:,:,:)
      real,               pointer :: rho_0v(:,:,:)
      real,               pointer :: rho_0d(:,:,:)
      real,               pointer :: rho_dv(:,:,:)
      real,               pointer :: rho_dd(:,:,:)
      integer(byte),      pointer :: Type(:,:)
      integer(byte),      pointer :: cldtype(:,:,:)
      integer(byte),      pointer :: cldmask(:,:,:)
      real,               pointer :: cldmask_uncertainty(:,:,:)
      real,               pointer :: cccot_pre(:,:,:)
      integer(byte),      pointer :: ann_phase(:,:,:)
      real,               pointer :: ann_phase_uncertainty(:,:,:)
      real,               pointer :: cphcot(:,:,:)
      type(Geometry_t)            :: Geometry
      type(Location_t)            :: Location
      type(State_t)               :: State
      integer(byte),      pointer :: LSFlags(:,:)
      integer(byte),      pointer :: lusflags(:,:)
      integer(sint),      pointer :: dem(:,:)
      integer(byte),      pointer :: nisemask(:,:)
      real(dreal),        pointer :: time(:,:)
      real,               pointer :: MSI(:,:,:)
      real,               pointer :: cal_gain(:)
      integer,            pointer :: illum(:,:,:)

      ! SelmAux surface reflectance uncertainty terms
      real,               pointer :: SD(:,:,:)     ! Std deviation in MSI data
      real,               pointer :: rho_dd_cor(:,:,:,:) ! Correlation matrix for
                                                   ! surface reflectance
      real,               pointer :: rho_dd_unc(:,:,:) ! Uncertainty in surface
                                                   ! reflect 3rd dim covers all
                                                   ! visible chs
      real,               pointer :: svd_unc(:)    ! Uncertainty due to differing
                                                   ! filter bandshapes
      real,               pointer :: veg_unc(:)    ! Uncertainty over vegetated
                                                   ! surfaces
      real,               pointer :: bare_unc(:)   ! Uncertainty over bare surf
      real,               pointer :: snow_unc(:)   ! Uncertainty over snow
   end type Data_t

contains

#include "nullify_data.F90"
#include "dealloc_data.F90"

#include "read_alb.F90"
#include "read_cloudflags.F90"
#include "read_geometry.F90"
#include "read_lsflags.F90"
#include "read_location.F90"
#include "read_msi.F90"
#include "read_ctp.F90"

#include "sabotage_inputs.F90"

#include "determine_illum.F90"

#include "read_data.F90"

end module Data_m
