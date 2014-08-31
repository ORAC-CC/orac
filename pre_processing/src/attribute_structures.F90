!-------------------------------------------------------------------------------
! Name: attribute_structures.F90
!
! Purpose:
! Define variables types which hold the attribute input data.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2012/02/13, MJ: creates initial structure.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module attribute_structures

   use preproc_constants

   implicit none

   type script_arguments_s
      character(len=attribute_length)      :: project
      character(len=attribute_length)      :: cncver
      character(len=attribute_length)      :: ccon
      character(len=attribute_length)      :: cinst
      character(len=attribute_length)      :: l2cproc
      character(len=attribute_length)      :: l2cprocver
      character(len=attribute_length)      :: uuid_tag
      character(len=attribute_length)      :: contact
      character(len=attribute_length)      :: website
      character(len=attribute_length)      :: exec_time
      character(len=attribute_length_long) :: reference
      character(len=attribute_length)      :: history
      character(len=attribute_length)      :: summary
      character(len=attribute_length)      :: keywords
      character(len=attribute_length)      :: comment
      character(len=attribute_length)      :: license
      character(len=attribute_length)      :: file_version
   end type script_arguments_s

end module attribute_structures
