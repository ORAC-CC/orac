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
! History:
! 2013/11/14, MJ: Initial version
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/07, AP: Replaced with preprocessor's NCDF routines.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_config_file(Ctrl, conf)

   use config_def
   use CTRL_def
   use orac_ncdf

   implicit none

   type(Ctrl_t),        intent(in)    :: Ctrl
   type(config_struct), intent(inout) :: conf
   logical                            :: verbose = .true.

   integer                            :: ncid

   ! Open config file for reading
   call nc_open(ncid, Ctrl%FID%CONFIG)

!  conf%nx = nc_dim_length(ncid, 'nx_conf', verbose)
!  conf%ny = nc_dim_length(ncid, 'ny_conf', verbose)
   conf%nc = nc_dim_length(ncid, 'nc_conf', verbose)
!  conf%nalb = nc_dim_length(ncid, 'nc_alb', verbose)
!  conf%nemis = nc_dim_length(ncid, 'nc_emis', verbose)

   allocate(conf%channel_ids_instr(conf%nc))
   call nc_read_array(ncid, "msi_instr_ch_numbers", conf%channel_ids_instr, &
        verbose)
   if (verbose) write(*,*) 'msi channel numbers instr: ',conf%channel_ids_instr

   allocate(conf%channel_ids_abs(conf%nc))
   call nc_read_array(ncid, "msi_abs_ch_numbers", conf%channel_ids_abs, verbose)
   if (verbose) write(*,*) 'msi channel numbers file: ',conf%channel_ids_abs

   allocate(conf%channel_sw_flag(conf%nc))
   call nc_read_array(ncid, "msi_ch_swflag", conf%channel_sw_flag, verbose)
   if (verbose) write(*,*) 'sw flag: ',conf%channel_sw_flag

   allocate(conf%channel_lw_flag(conf%nc))
   call nc_read_array(ncid, "msi_ch_lwflag", conf%channel_lw_flag, verbose)
   if (verbose) write(*,*) 'lw flag: ',conf%channel_lw_flag

   ! Close config file
   if (nf90_close(ncid) .ne. NF90_NOERR) &
      stop 'ERROR: read_config_file(): Failure to close file.'

end subroutine read_config_file
