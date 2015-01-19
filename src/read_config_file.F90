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
! 2012/12/01, CP: Read in global and source attributes
! 2012/12/01, GM: Use the code in the common library to read the global
!    attributes.
! 2014/12/19, AP: Eliminated conf structure. Flags now simply variables.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_config_file(Ctrl, channel_ids_instr, channel_sw_flag, &
     channel_lw_flag, global_atts, source_atts, verbose)

   use CTRL_def
   use ECP_constants, only : DriverFileIncompat
   use orac_ncdf
   use global_attributes
   use source_attributes

   implicit none

   type(Ctrl_t),                       intent(in)    :: Ctrl
   integer, allocatable, dimension(:), intent(inout) :: channel_ids_instr
   integer, allocatable, dimension(:), intent(inout) :: channel_sw_flag
   integer, allocatable, dimension(:), intent(inout) :: channel_lw_flag
   type(global_attributes_s),          intent(inout) :: global_atts
   type(source_attributes_s),          intent(inout) :: source_atts
   logical,                            intent(in)    :: verbose

   integer :: ncid

   ! Open config file for reading
   call nc_open(ncid, Ctrl%FID%Config)

   if (Ctrl%Ind%Navail /= nc_dim_length(ncid, 'nc_conf', verbose)) then
      write(*,*) 'ERROR: read_config_file(): Driver incompatible with preprocessor files.'
      stop DriverFileIncompat
   end if

   allocate(channel_ids_instr(Ctrl%Ind%Navail))
   call nc_read_array(ncid, "msi_instr_ch_numbers", channel_ids_instr, verbose)
   if (verbose) write(*,*) 'msi channel numbers instr: ',channel_ids_instr

   allocate(channel_sw_flag(Ctrl%Ind%Navail))
   call nc_read_array(ncid, "msi_ch_swflag", channel_sw_flag, verbose)
   if (verbose) write(*,*) 'sw flag: ',channel_sw_flag

   allocate(channel_lw_flag(Ctrl%Ind%Navail))
   call nc_read_array(ncid, "msi_ch_lwflag", channel_lw_flag, verbose)
   if (verbose) write(*,*) 'lw flag: ',channel_lw_flag

   ! Read global attributes
   call nc_get_common_attributes(ncid, global_atts, source_atts)

   if (nf90_close(ncid) /= NF90_NOERR) then
      write(*,*) 'ERROR: read_config_file(): Error closing file.'
      stop error_stop_code
   end if

end subroutine read_config_file
