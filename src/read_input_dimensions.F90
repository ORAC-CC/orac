!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_input_dimensions_msi(fname_msi,fname_geo,xdim,ydim,cdim,vdim,wo)
!--------------------------------------------------
  
  use netcdf

  use ECP_Constants
  
  implicit none
 INTEGER,INTENT(IN) :: wo
  integer :: ivar,idim,ndim,nvar,nattr,dummyint
  integer :: ncid,ierr,ny=5,nx=5
  character(len=FilenameLen) :: fname_msi,fname_geo,name
  integer (kind=nint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
  character(len=varlength), allocatable :: dname(:)
  
  character (len=varlength), allocatable, dimension(:) ::  available_names(:)
  INTEGER(kind=nint),INTENT(OUT) ::  xdim,ydim,cdim,vdim

  !open msi file
  call nc_open(ncid,fname_msi,ierr,wo)
  call nc_info(ncid,ndim,nvar,nattr,wo)
  
  
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
 
  name='nx_msi'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),wo)
  call nc_dim_length(ncid,name,dimids(1),dummyint,wo)
  dimlength(1)=dummyint
  xdim=int(dimlength(1),kind=nint)
  
  name='ny_msi'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),wo)
  call nc_dim_length(ncid,name,dimids(2),dummyint,wo)
  dimlength(2)=dummyint
  ydim=int(dimlength(2),kind=nint)

  name='nc_msi'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(3),wo)
  call nc_dim_length(ncid,name,dimids(3),dummyint,wo)
  dimlength(3)=dummyint
  cdim=int(dimlength(3),kind=nint)

  deallocate(dimids)
  deallocate(dname)
  deallocate(dimlength)
  deallocate(varids)
  deallocate(attrids)
  deallocate(available_names)

  !close  input file
  ierr=nf90_close(ncid)

  !open geo file
  call nc_open(ncid,fname_geo,ierr,wo)
  call nc_info(ncid,ndim,nvar,nattr,wo)
  
  
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
 
  name='nx_geo'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),wo)
  call nc_dim_length(ncid,name,dimids(1),dummyint,wo)
  dimlength(1)=dummyint
  !xdim=int(dimlength(1),kind=nint)
  
  name='ny_geo'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),wo)
  call nc_dim_length(ncid,name,dimids(2),dummyint,wo)
  dimlength(2)=dummyint
  !ydim=int(dimlength(2),kind=nint)

  name='nv_geo'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(3),wo)
  call nc_dim_length(ncid,name,dimids(3),dummyint,wo)
  dimlength(3)=dummyint
  vdim=int(dimlength(3),kind=nint)

  deallocate(dimids)
  deallocate(dname)
  deallocate(dimlength)
  deallocate(varids)
  deallocate(attrids)
  deallocate(available_names)


  !close  input file
  ierr=nf90_close(ncid)


end SUBROUTINE read_input_dimensions_msi


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_input_dimensions_lwrtm(Ctrl,fname,xydim,xdim,ydim,levdim,laydim,channeldim,viewdim,wo)
!--------------------------------------------------
  
  use netcdf

  use ECP_Constants

  use CTRL_def
  
  implicit none
 INTEGER,INTENT(IN) :: wo
  integer :: ivar,idim,ndim,nvar,nattr,dummyint
  integer :: ncid,ierr,ny=5,nx=5
  character(len=FilenameLen) :: fname,name
  integer (kind=nint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
  character(len=varlength), allocatable :: dname(:)
  
  character (len=varlength), allocatable, dimension(:) ::  available_names(:)
  INTEGER(kind=nint) ::  xydim,xdim,ydim,levdim,laydim,channeldim,viewdim

  type(CTRL_t) :: Ctrl

  !open file
!  write(*,*) 'this open',trim(adjustl(fname))
!  write(*,*) 'in lwrtm', Ctrl%Ind%Y_Id      
  call nc_open(ncid,fname,ierr,wo)
  call nc_info(ncid,ndim,nvar,nattr,wo)
  
  
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

  name='nlon_x_nlat_lwrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),wo)
  call nc_dim_length(ncid,name,dimids(1),dummyint,wo)
  dimlength(1)=dummyint
  xydim=int(dimlength(1),kind=nint)
  
  name='nlon_lwrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),wo)
  call nc_dim_length(ncid,name,dimids(2),dummyint,wo)
  dimlength(2)=dummyint
  xdim=int(dimlength(2),kind=nint)
  
  name='nlat_lwrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(3),wo)
  call nc_dim_length(ncid,name,dimids(3),dummyint,wo)
  dimlength(3)=dummyint
  ydim=int(dimlength(3),kind=nint)

  name='nlayers_lwrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(4),wo)
  call nc_dim_length(ncid,name,dimids(4),dummyint,wo)
  dimlength(4)=dummyint
  laydim=int(dimlength(4),kind=nint)

  name='nlevels_lwrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(5),wo)
  call nc_dim_length(ncid,name,dimids(5),dummyint,wo)
  dimlength(5)=dummyint
  levdim=int(dimlength(5),kind=nint)

  name='nlw_channels'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(6),wo)
  call nc_dim_length(ncid,name,dimids(6),dummyint,wo)
  dimlength(6)=dummyint
  channeldim=int(dimlength(6),kind=nint)


  name='nviews'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(7),wo)
  call nc_dim_length(ncid,name,dimids(7),dummyint,wo)
  dimlength(7)=dummyint
  viewdim=int(dimlength(7),kind=nint)
  

  deallocate(dimids)
  deallocate(dname)
  deallocate(dimlength)
  deallocate(varids)
  deallocate(attrids)
  deallocate(available_names)

  !close  input file
  ierr=nf90_close(ncid)

  write(*,*) 'in lwrtm end', Ctrl%Ind%Y_Id
  !stop

end SUBROUTINE read_input_dimensions_lwrtm


!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_input_dimensions_swrtm(fname,xydim,xdim,ydim,levdim,laydim,channeldim,viewdim,wo)
!--------------------------------------------------
  
  use netcdf

  use ECP_Constants
  
  implicit none
 INTEGER,INTENT(IN) :: wo
  integer :: ivar,idim,ndim,nvar,nattr,dummyint
  integer :: ncid,ierr,ny=5,nx=5
  character(len=FilenameLen) :: fname,name
  integer (kind=nint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
  character(len=varlength), allocatable :: dname(:)
  
  character (len=varlength), allocatable, dimension(:) ::  available_names(:)
  INTEGER(kind=nint),INTENT(OUT) ::  xydim,xdim,ydim,levdim,laydim,channeldim,viewdim

  !open file
  !write(*,*) 'this open'
  call nc_open(ncid,fname,ierr,wo)
  call nc_info(ncid,ndim,nvar,nattr,wo)
  
  
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

  name='nlon_x_nlat_swrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),wo)
  call nc_dim_length(ncid,name,dimids(1),dummyint,wo)
  dimlength(1)=dummyint
  xydim=int(dimlength(1),kind=nint)
  
  name='nlon_swrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),wo)
  call nc_dim_length(ncid,name,dimids(2),dummyint,wo)
  dimlength(2)=dummyint
  xdim=int(dimlength(2),kind=nint)
  
  name='nlat_swrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(3),wo)
  call nc_dim_length(ncid,name,dimids(3),dummyint,wo)
  dimlength(3)=dummyint
  ydim=int(dimlength(3),kind=nint)

  name='nlayers_swrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(4),wo)
  call nc_dim_length(ncid,name,dimids(4),dummyint,wo)
  dimlength(4)=dummyint
  laydim=int(dimlength(4),kind=nint)

  name='nlevels_swrtm'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(5),wo)
  call nc_dim_length(ncid,name,dimids(5),dummyint,wo)
  dimlength(5)=dummyint
  levdim=int(dimlength(5),kind=nint)

  name='nsw_channels'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(6),wo)
  call nc_dim_length(ncid,name,dimids(6),dummyint,wo)
  dimlength(6)=dummyint
  channeldim=int(dimlength(6),kind=nint)


  name='nviews'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(7),wo)
  call nc_dim_length(ncid,name,dimids(7),dummyint,wo)
  dimlength(7)=dummyint
  viewdim=int(dimlength(7),kind=nint)
  

  deallocate(dimids)
  deallocate(dname)
  deallocate(dimlength)
  deallocate(varids)
  deallocate(attrids)
  deallocate(available_names)

  !close  input file
  ierr=nf90_close(ncid)

  
end SUBROUTINE read_input_dimensions_swrtm
