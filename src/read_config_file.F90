!-------------------------------------------------------------------------------
! Name: read_config_file.F90
!
! Purpose:
! Read contents of the configuration file, used to allocating data arrays.
!
! Description and Algorithm details:
! Use ncdf_read_array a few times.
!
! Arguments:
! Name              Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl              struct    In          Control structure
! channel_ids_instr int array Both        ID number for channels input
! channel_sw_flag   int array Both        Flags channels with shortwave component
! channel_lw_flag   int array Both        Flags channels with longwave component
! global_atts       struct    Both        Global attributes for output files
! source_atts       struct    Both        Attributes for output files specifying
!                                         input files
!
! History:
! 2013/11/14, MJ: Initial version
! 2014/08/02, GM: Cleaned up the code.
! 2014/08/07, AP: Replaced with preprocessor's NCDF routines.
! 2012/12/01, CP: Read in global and source attributes
! 2012/12/01, GM: Use the code in the common library to read the global
!    attributes.
! 2014/12/19, AP: Eliminated conf structure. Flags now simply variables.
! 2015/07/03, OS: added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
! 2015/08/02, AP: Output channel wavelengths to identify multiple views.
! 2017/07/05, AP: Add channels_used.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_config_file(Ctrl, channel_ids_instr, channel_sw_flag, &
     channel_lw_flag, channel_wvl, channel_view, global_atts, source_atts, &
     nall)

   use Ctrl_m
   use global_attributes_m
   use ORAC_Constants_m, only : DriverFileIncompat
   use orac_ncdf_m
   use source_attributes_m

   implicit none

   type(Ctrl_t),                       intent(in)    :: Ctrl
   integer, allocatable, dimension(:), intent(inout) :: channel_ids_instr
   integer, allocatable, dimension(:), intent(inout) :: channel_sw_flag
   integer, allocatable, dimension(:), intent(inout) :: channel_lw_flag
   real,    allocatable, dimension(:), intent(inout) :: channel_wvl
   integer, allocatable, dimension(:), intent(inout) :: channel_view
   type(global_attributes_t),          intent(inout) :: global_atts
   type(source_attributes_t),          intent(inout) :: source_atts
   integer,                            intent(out)   :: nall

   integer :: ncid

   ! Open config file for reading
   call ncdf_open(ncid, Ctrl%FID%Config, 'read_config_file()')

   if (Ctrl%Ind%Navail /= ncdf_dim_length(ncid, 'nc_conf', 'read_config_file()', Ctrl%verbose)) then
      write(*,*) 'ERROR: read_config_file(): Driver incompatible with ', &
           'preprocessor files.'
      stop DriverFileIncompat
   end if

   allocate(channel_ids_instr(Ctrl%Ind%Navail))
   call ncdf_read_array(ncid, "msi_instr_ch_numbers", channel_ids_instr, &
                      Ctrl%verbose)
   if (Ctrl%verbose) write(*,*) 'msi channel numbers instr: ',channel_ids_instr

   allocate(channel_sw_flag(Ctrl%Ind%Navail))
   call ncdf_read_array(ncid, "msi_ch_swflag", channel_sw_flag, Ctrl%verbose)
   if (Ctrl%verbose) write(*,*) 'sw flag: ',channel_sw_flag

   allocate(channel_lw_flag(Ctrl%Ind%Navail))
   call ncdf_read_array(ncid, "msi_ch_lwflag", channel_lw_flag, Ctrl%verbose)
   if (Ctrl%verbose) write(*,*) 'lw flag: ',channel_lw_flag

   allocate(channel_wvl(Ctrl%Ind%Navail))
   call ncdf_read_array(ncid, "msi_abs_ch_wl", channel_wvl, Ctrl%verbose)

   allocate(channel_view(Ctrl%Ind%Navail))
   call ncdf_read_array(ncid, "msi_ch_view", channel_view, Ctrl%verbose)

   if (nf90_get_att(ncid, NF90_GLOBAL, 'all_nchannels_total', &
        nall) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_get_att(), name: all_nchannels_total'
      stop error_stop_code
   end if

   ! Read global attributes
   call ncdf_get_common_attributes(ncid, global_atts, source_atts)

   call ncdf_close(ncid, 'read_config_file()')

end subroutine read_config_file
