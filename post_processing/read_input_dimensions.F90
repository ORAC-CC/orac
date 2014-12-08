!2014/06/04 MJ changes routine names to "*_pp" to avoid confusion when building libraries.
!2014/12/03 CP added in common_constants should eventually remove vartypes_pp
!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_input_dimensions(fname,xdim,ydim,wo)
!--------------------------------------------------
  use netcdf

  use vartypes_pp
  use common_constants  
  use structures_pp

  implicit none
 INTEGER,INTENT(IN) :: wo
  integer :: ivar,idim,ndim,nvar,nattr,dummyint,iphase
  integer :: ncid,ierr,ny=5,nx=5
  character(len=cpathlength) :: fname,name
  integer (kind=nint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
  character(len=varlength), allocatable :: dname(:)
  
  character (len=varlength), allocatable, dimension(:) ::  available_names(:)
  INTEGER(kind=nint),INTENT(OUT) ::  xdim,ydim

  call nc_open_pp(ncid,fname,ierr,wo)
  call nc_info_pp(ncid,ndim,nvar,nattr,wo)
  
  
  allocate(dimids(ndim))
  dimids=0
  allocate(dname(ndim))
  dname=''
  allocate(dimlength(ndim))
  dimlength=0
  allocate(varids(nvar))
  varids=0
  allocate(attrids(nattr))
  attrids=0


  allocate(available_names(nvar))
  available_names=''
 
  name='across_track'
  call nc_dim_id_pp(ncid,trim(adjustl(name)),dimids(1),wo)
  call nc_dim_length_pp(ncid,name,dimids(1),dummyint,wo)
  dimlength(1)=dummyint
  xdim=int(dimlength(1),kind=nint)
  
  name='along_track'
  call nc_dim_id_pp(ncid,trim(adjustl(name)),dimids(2),wo)
  call nc_dim_length_pp(ncid,name,dimids(2),dummyint,wo)
  dimlength(2)=dummyint
  ydim=int(dimlength(2),kind=nint)

  !close  input file
  ierr=nf90_close(ncid)

end SUBROUTINE read_input_dimensions
