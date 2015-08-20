!-------------------------------------------------------------------------------
! Name: ReadALB_nc.F90
!
! Purpose:
! Controls the reading of Multi Spectral Image (ALB) values from ATSR-type
! files into the DATA_ALB array.
!
! Description and Algorithm details:
! If (ALB files are not yet open)
!    Find a logical unit number to be used for the ALB file
!    Open ALB file
!    If open error
!       Write error message to screen and log file
!    Else
!       Allocate ALB image segment array in Data_ALB struct.
!
! If (no error opening files)
!    Read header (not used further)
!    If read error
!       Write error message to screen and log file
!    Else
!       Read ALB array of size defined by Ctrl structure
!       If read error
!          Write error message to log file
! Leave ALB file open for further reads
!
! Arguments:
! Name     Type          In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct        Both        Control structure (date is read in
!                                    here).
! MSI_Data struct        Both        Data structure: the ALB data part
!                                    of this struct is populated by this
!                                    routine, and is overwritten on
!                                    successive calls.
! verbose  logical       In          Prints log information to screen.
!
! History:
! 2002/05/29, CP: Original version copied from READ_MSI.
! 2002/10/29, CP: Fixed bug too many arguments in the header
!    removed sad_chan.
! 2011/06/28, CP: Remove reference to ATSR
! 2011/12/13, CP: change format statement to make g95
!    compatible
! 2012/09/15, CP: Initialise array
! 2012/09/15, CP: Changed to read from netcdf files
! 2013/xx/xx, MJ: Fixes bug with close of netcdf file.
! 2013/11/18, MJ: Cleans and debugs:Loop to read albedo is indexed with
!    ysolar as ysolar halds the channel indices as they are stored in the
!    preprocessing file.
! 2014/04/20, GM: Cleaned up the code.
! 2014/05/28, GM: Removed unused read of attribute 'Product_Date'.
! 2014/08/15, AP: Switching to preprocessor NCDF routines.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2014/01/30, AP: Remove NSegs, SegSize arguments.
! 2015/04/28, AP: Adding read of surface uncertainties and correlations.
! 2015/07/03, OS: Added error status variable to nc_open call
! 2015/07/10, OS: undo previous commit
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_ALB_nc(Ctrl, MSI_Data, verbose)

   use CTRL_def
   use ECP_Constants
   use orac_ncdf
   use SAD_Chan_def

   implicit none

   ! Argument declarations
   type(CTRL_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data
   logical,      intent(in)    :: verbose

   integer                     :: ncid, i, j
   integer(kind=lint)          :: NAlb
   integer(kind=lint), allocatable, dimension(:) :: alb_instr_ch_numbers, subs

   ! Open ALB file
   if (verbose) write(*,*) 'Albedo file: ', trim(Ctrl%Fid%Alb)
   call nc_open(ncid, Ctrl%Fid%Alb)

   ! Read instrument channel indices from file
   NAlb = nc_dim_length(ncid, 'nc_alb', verbose)
   allocate(alb_instr_ch_numbers(NAlb))
   call nc_read_array(ncid, "alb_abs_ch_numbers", alb_instr_ch_numbers, verbose)

   ! Find the subscripts Ctrl%Ind%ysolar within alb_abs_ch_numbers
   allocate(subs(Ctrl%Ind%NSolar))
   do i=1,Ctrl%Ind%NSolar
      do j=1,NAlb
         if (alb_instr_ch_numbers(j) == Ctrl%Ind%ICh(Ctrl%Ind%YSolar(i))) then
            subs(i) = j
            exit
         end if
      end do
   end do

   ! Allocate Data%ALB structure
   allocate(MSI_Data%ALB(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))

   ! Read solar channels from albedo field
   call nc_read_array(ncid, "alb_data", MSI_Data%ALB, verbose, 3, subs)

   if (verbose) write(*,*) 'Max/Min Alb: ', maxval(MSI_Data%ALB), &
      minval(MSI_Data%ALB)

   if (Ctrl%RS%use_full_brdf) then
      ! Allocate Data%rho_xx structures
      allocate(MSI_Data%rho_0v(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))
      allocate(MSI_Data%rho_0d(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))
      allocate(MSI_Data%rho_dv(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))
      allocate(MSI_Data%rho_dd(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))

      call nc_read_array(ncid, "rho_0v_data", MSI_Data%rho_0v, verbose, 3, subs)
      call nc_read_array(ncid, "rho_0d_data", MSI_Data%rho_0d, verbose, 3, subs)
      call nc_read_array(ncid, "rho_dv_data", MSI_Data%rho_dv, verbose, 3, subs)
      call nc_read_array(ncid, "rho_dd_data", MSI_Data%rho_dd, verbose, 3, subs)
   end if

   deallocate(alb_instr_ch_numbers)
   deallocate(subs)

   ! Close alb input file
   if (nf90_close(ncid) /= NF90_NOERR) then
      write(*,*) 'ERROR: read_alb_nc(): Error closing file.'
      stop error_stop_code
   end if

end subroutine Read_ALB_nc
