! 2013/11/14 MJ initial file version

subroutine read_config_file(Ctrl,conf)
  
  use netcdf

  use ECP_Constants

  use CTRL_def

  use config_s

  implicit none

  type(CTRL_t) :: Ctrl
  type(config_struct) :: conf

  integer :: ncid,ierr,wo
  integer :: ndim,nvar,nattr,dummyint
  integer(kind=nint), allocatable, dimension(:) :: msi_instr_ch_numbers
  integer (kind=nint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
  INTEGER(kind=nint) ::  xdim,ydim,cdim,vdim,albdim,emisdim
  character(len=varlength), allocatable :: dname(:)
  character(len=FilenameLen) :: name

  wo=0

  !open config file for reading
  !open msi file
  call nc_open(ncid,Ctrl%FID%CONFIG,ierr,wo)
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

 
  name='nx_conf'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),wo)
  call nc_dim_length(ncid,name,dimids(1),dummyint,wo)
  dimlength(1)=dummyint
  xdim=int(dimlength(1),kind=nint)
  conf%nx=xdim

  name='ny_conf'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),wo)
  call nc_dim_length(ncid,name,dimids(2),dummyint,wo)
  dimlength(2)=dummyint
  ydim=int(dimlength(2),kind=nint)
  conf%ny=ydim

  name='nc_conf'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(3),wo)
  call nc_dim_length(ncid,name,dimids(3),dummyint,wo)
  dimlength(3)=dummyint
  cdim=int(dimlength(3),kind=nint)
  conf%nc=cdim

  name='nc_alb'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(4),wo)
  call nc_dim_length(ncid,name,dimids(4),dummyint,wo)
  dimlength(4)=dummyint
  albdim=int(dimlength(4),kind=nint)
  conf%nalb=albdim

  name='nc_emis'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(5),wo)
  call nc_dim_length(ncid,name,dimids(5),dummyint,wo)
  dimlength(5)=dummyint
  emisdim=int(dimlength(5),kind=nint)
  conf%nemis=emisdim

  deallocate(dimids)
  deallocate(dname)
  deallocate(dimlength)
  deallocate(varids)
  deallocate(attrids)

  write(*,*)   xdim,ydim,cdim,albdim,emisdim

  allocate(conf%channel_ids_instr(conf%nc))
  conf%channel_ids_instr=-1_nint
  call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_instr_ch_numbers",conf%channel_ids_instr,0)
  write(*,*) 'msi channel numbers instr',conf%channel_ids_instr
  !write(*,*)  'msi channel numbers y_id',Ctrl%Ind%Y_Id
  !deallocate(msi_instr_ch_numbers)

  allocate(conf%channel_ids_abs(conf%nc))
  conf%channel_ids_abs=-1_nint
  call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_abs_ch_numbers",conf%channel_ids_abs,0)
  write(*,*) 'msi channel numbers file',conf%channel_ids_abs


  allocate(conf%channel_sw_flag(conf%nc))
  conf%channel_sw_flag=-1_nint
  call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_ch_swflag",conf%channel_sw_flag,0)
  write(*,*) 'sw flag',conf%channel_sw_flag


  allocate(conf%channel_lw_flag(conf%nc))
  conf%channel_lw_flag=-1_nint
  call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_ch_lwflag",conf%channel_lw_flag,0)
  write(*,*) 'lw flag',conf%channel_lw_flag


  !close  input file
  ierr=nf90_close(ncid)

end subroutine read_config_file
