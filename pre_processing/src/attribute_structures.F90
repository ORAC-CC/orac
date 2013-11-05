! Name: attribute_structures.F90
!
!
! Purpose:
! Define variables types which hold the attribute input data.
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
! 2012/02/13: MJ creates initial structure.
!
! $Id$
!
! Bugs:
! none known
!

module attribute_structures

  use preproc_constants

  implicit none
  
  type script_arguments_s   
     character(len=attribute_length) :: exec_time,cncver,ccon,cinst,l2cproc, &
          l2cprocver,contact,website,project,file_version
     character(len=description_length) :: reference,history,summary,keywords, &
          comment,license
     character(len=uuid_length) :: uuid_tag

  end type script_arguments_s


end module attribute_structures
