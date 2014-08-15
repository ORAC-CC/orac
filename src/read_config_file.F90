!-------------------------------------------------------------------------------
! Name: read_config_file.F90
!
! Purpose:
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2013/11/14, MJ: Initial version
! 2014/08/02, GM: Cleaned up the code.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_config_file(Ctrl, conf)

   use config_def
   use CTRL_def
   use ECP_Constants
   use netcdf

   implicit none

   type(Ctrl_t),        intent(in)    :: Ctrl
   type(config_struct), intent(inout) :: conf

   integer                                     :: ncid,ierr,wo
   integer                                     :: ndim,nvar,nattr,dummyint
   integer(kind=lint), allocatable             :: dimids(:), varids(:), &
                                                  attrids(:), dimlength(:)
   integer(kind=lint)                          :: xdim,ydim,cdim,albdim,emisdim
   character(len=NetcdfVarLength), allocatable :: dname(:)
   character(len=FilenameLen)                  :: name

   wo=0

   ! Open config file for reading
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
   xdim=int(dimlength(1),kind=lint)
   conf%nx=xdim

   name='ny_conf'
   call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),wo)
   call nc_dim_length(ncid,name,dimids(2),dummyint,wo)
   dimlength(2)=dummyint
   ydim=int(dimlength(2),kind=lint)
   conf%ny=ydim

   name='nc_conf'
   call nc_dim_id(ncid,trim(adjustl(name)),dimids(3),wo)
   call nc_dim_length(ncid,name,dimids(3),dummyint,wo)
   dimlength(3)=dummyint
   cdim=int(dimlength(3),kind=lint)
   conf%nc=cdim

   name='nc_alb'
   call nc_dim_id(ncid,trim(adjustl(name)),dimids(4),wo)
   call nc_dim_length(ncid,name,dimids(4),dummyint,wo)
   dimlength(4)=dummyint
   albdim=int(dimlength(4),kind=lint)
   conf%nalb=albdim

   name='nc_emis'
   call nc_dim_id(ncid,trim(adjustl(name)),dimids(5),wo)
   call nc_dim_length(ncid,name,dimids(5),dummyint,wo)
   dimlength(5)=dummyint
   emisdim=int(dimlength(5),kind=lint)
   conf%nemis=emisdim

   deallocate(dimids)
   deallocate(dname)
   deallocate(dimlength)
   deallocate(varids)
   deallocate(attrids)

   allocate(conf%channel_ids_instr(conf%nc))
   conf%channel_ids_instr=-1_lint
   call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_instr_ch_numbers",conf%channel_ids_instr,0)
   write(*,*) 'msi channel numbers instr: ',conf%channel_ids_instr

   allocate(conf%channel_ids_abs(conf%nc))
   conf%channel_ids_abs=-1_lint
   call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_abs_ch_numbers",conf%channel_ids_abs,0)
   write(*,*) 'msi channel numbers file: ',conf%channel_ids_abs

   allocate(conf%channel_sw_flag(conf%nc))
   conf%channel_sw_flag=-1_lint
   call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_ch_swflag",conf%channel_sw_flag,0)
   write(*,*) 'sw flag: ',conf%channel_sw_flag

   allocate(conf%channel_lw_flag(conf%nc))
   conf%channel_lw_flag=-1_lint
   call nc_read_array_1d_int_to_int_orac(ncid,conf%nc,"msi_ch_lwflag",conf%channel_lw_flag,0)
   write(*,*) 'lw flag: ',conf%channel_lw_flag

   ! Close config file
   ierr=nf90_close(ncid)

end subroutine read_config_file
