!-------------------------------------------------------------------------------
! Name: attribute_structures.F90
!
! Purpose:
! Define variables types which hold the source attribute data.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2014/12/01, CP: Created a source attribute module using source attributes as
!    a template
!
! $Id: source_attributes.F90 2355 2014-09-09 23:16:38Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module source_attributes

   use common_constants

   implicit none

   type source_attributes_s

      character(len=attribute_length_long) :: albedo_file
      character(len=attribute_length_long) :: brdf_file
      character(len=attribute_length_long) :: emissivity_file
      character(len=attribute_length_long) :: usgs_file
      character(len=attribute_length_long) :: snow_file
      character(len=attribute_length_long) :: sea_ice_file
      character(len=attribute_length_long) :: level1b_file
      character(len=attribute_length_long) :: geo_file

   end type source_attributes_s

end module source_attributes
