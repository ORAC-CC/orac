! Name: modis2oraclsflag.f90
!
!
! Purpose:
! Map Land/Sea Flag to ORAC values
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
! 2011/12/15: MJ produces draft code which maps the MODIS flags to the ORAC definitions (approximately).
! 2013/09/03: AP tidying
!
! $Id$
!
! Bugs:
!
!none known
!-------------------------------------------------------
!-------------------------------------------------------
subroutine modis2oraclsflag(temp)
!-------------------------------------------------------
!-------------------------------------------------------

  use preproc_constants

  implicit none

  integer(kind=sint) :: temp

!!$  MODIS DEFINITIONS:
!!$     DN values:
!!$                0:      Shallow Ocean (Ocean <5k from coast OR <50m deep).
!!$                1:      Land (not anything else).
!!$                2:      Ocean Coastlines and Lake Shorelines.
!!$                3:      Shallow Inland Water (Inland Water < 5km from shore
!!$                                OR < 50m deep).
!!$                4:      Ephemeral (intermittent) Water.
!!$                5:      Deep Inland Water (Inland water > 5km from shoreline
!!$                                AND > 50m deep).
!!$                6:      Moderate or Continental Ocean (Ocean > 5km from coast
!!$                                AND > 50m deep AND < 500m deep).
!!$                7:      Deep Ocean (Ocean > 500m deep).
!!$
!!$                SDS Attributes:
!!$                Attribute Name        Format            Example
!!$                --------------        ------            -------
!!$
!!$                _FillValue            uint8             221
!!$


  lsflag_mapping:  select  case(temp)

  case(0,5,6,7) !water 0
     
     temp=0

  case(1,2,3,4) !land 1

     temp=1
   
  case(-1) !fill value

     temp=1

  case default !default land

     temp=1

  end select lsflag_mapping


end subroutine modis2oraclsflag
