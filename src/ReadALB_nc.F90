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
!
! History:
! 2002/05/29, CP: Original version copied from READ_MSI.
! 2002/10/29, CP: Fixed bug too many arguments in the header removed sad_chan.
! 2011/06/28, CP: Remove reference to ATSR
! 2011/12/13, CP: change format statement to make g95 compatible
! 2012/09/15, CP: Initialise array
! 2012/09/15, CP: Changed to read from netcdf files
! 2013/xx/xx, MJ: Fixes bug with close of netcdf file.
! 2013/11/18, MJ: Cleans and debugs:Loop to read albedo is indexed with ysolar
!    as ysolar halds the channel indices as they are stored in the preprocessing
!    file.
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
! 2015/08/10, AP: Additional surface uncs.
! 2015/08/31, AP: Read channels described by a correlation value rather than
!    assuming them from the channel ordering.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_ALB_nc(Ctrl, MSI_Data)

   use Ctrl_m
   use ORAC_Constants_m
   use orac_ncdf_m
   use SAD_Chan_m

   implicit none

   ! Argument declarations
   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   integer                                           :: ncid, i, j, k, ind
   integer(kind=lint)                                :: NAlb, NCor
   integer(kind=lint), allocatable, dimension(:)     :: alb_instr_ch_numbers, subs
   integer(kind=lint), allocatable, dimension(:,:)   :: cor_ch_numbers
   real(kind=sreal),   allocatable, dimension(:,:,:) :: cor_temp

   ! Open ALB file
   if (Ctrl%verbose) write(*,*) 'Albedo file: ', trim(Ctrl%FID%Alb)
   call ncdf_open(ncid, Ctrl%FID%Alb, 'Read_ALB_nc()')

   ! Read instrument channel indices from file
   NAlb = ncdf_dim_length(ncid, 'nc_alb', 'Read_ALB_nc()', Ctrl%verbose)
   allocate(alb_instr_ch_numbers(NAlb))
   call ncdf_read_array(ncid, "alb_abs_ch_numbers", alb_instr_ch_numbers, Ctrl%verbose)

   ! Find the subscripts Ctrl%Ind%ysolar within alb_abs_ch_numbers
   allocate(subs(Ctrl%Ind%NSolar))
   do i = 1, Ctrl%Ind%NSolar
      do j = 1, NAlb
         if (alb_instr_ch_numbers(j) == Ctrl%Ind%ICh(Ctrl%Ind%YSolar(i))) then
            subs(i) = j
            exit
         end if
      end do
   end do

   ! Allocate Data%ALB structure
   allocate(MSI_Data%ALB(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))

   ! Read solar channels from albedo field
   call ncdf_read_array(ncid, "alb_data", MSI_Data%ALB, Ctrl%verbose, 3, subs)

   if (Ctrl%verbose) write(*,*) 'Max/Min Alb: ', maxval(MSI_Data%ALB), &
      minval(MSI_Data%ALB)

   if (Ctrl%RS%read_full_brdf) then
      ! Allocate Data%rho_xx structures
      allocate(MSI_Data%rho_0v(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))
      allocate(MSI_Data%rho_0d(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))
      allocate(MSI_Data%rho_dv(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))
      allocate(MSI_Data%rho_dd(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))

      call ncdf_read_array(ncid, "rho_0v_data", MSI_Data%rho_0v, Ctrl%verbose, 3, subs)
      call ncdf_read_array(ncid, "rho_0d_data", MSI_Data%rho_0d, Ctrl%verbose, 3, subs)
      call ncdf_read_array(ncid, "rho_dv_data", MSI_Data%rho_dv, Ctrl%verbose, 3, subs)
      call ncdf_read_array(ncid, "rho_dd_data", MSI_Data%rho_dd, Ctrl%verbose, 3, subs)
   end if


   ! Correlations for aerosol retrieval
   if (Ctrl%RS%SRsSelm == SelmAux) then
      ! Read surface reflectance uncertainties
      allocate(MSI_Data%rho_dd_unc(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NSolar))
      call ncdf_read_array(ncid, "rho_dd_unc", MSI_Data%rho_dd_unc, Ctrl%verbose, 3, subs)

      ! Read additional uncertainty terms (in RhoDD over land)
      allocate(MSI_Data%svd_unc(Ctrl%Ind%NSolar))
      call ncdf_read_array(ncid, "svd_unc", MSI_Data%svd_unc, Ctrl%verbose, 1, subs)
      allocate(MSI_Data%veg_unc(Ctrl%Ind%NSolar))
      call ncdf_read_array(ncid, "veg_unc", MSI_Data%veg_unc, Ctrl%verbose, 1, subs)
      allocate(MSI_Data%bare_unc(Ctrl%Ind%NSolar))
      call ncdf_read_array(ncid, "bare_unc", MSI_Data%bare_unc, Ctrl%verbose, 1, subs)
      allocate(MSI_Data%snow_unc(Ctrl%Ind%NSolar))
      call ncdf_read_array(ncid, "snow_unc", MSI_Data%snow_unc, Ctrl%verbose, 1, subs)

      ! Read surface reflectance correlations. The third dimension of this is
      ! every permutation of the Solar channels, so some work is necessary to
      ! reorder from the file to ORAC. To be efficient, the correlations were
      ! stored once only, in the order 12, 13, 14, ..., 23, 24, ..., ... We'll
      ! make a correlation matrix for ORAC.

      ! Read in the correlation data to a temporary array
      NCor = ncdf_dim_length(ncid, 'nc_corr', 'Read_ALB_nc()', Ctrl%verbose)
      allocate(cor_ch_numbers(2, NCor))
      call ncdf_read_array(ncid, "cor_abs_ch_numbers", cor_ch_numbers, Ctrl%verbose)
      allocate(cor_temp(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, NCor))
      call ncdf_read_array(ncid, "cor_data", cor_temp, Ctrl%verbose)

      allocate(MSI_Data%rho_dd_cor(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, &
                                   Ctrl%Ind%NSolar, Ctrl%Ind%NSolar))

      ! Loop over available correlations
      do ind = 1, NCor
         ! Identify if the indices of this correlation correspond to solar chs
         i = 0
         j = 0
         do k = 1, Ctrl%Ind%NSolar
            if (cor_ch_numbers(1,ind) == Ctrl%Ind%ICh(Ctrl%Ind%YSolar(k))) then
               i = k
            end if
            if (cor_ch_numbers(2,ind) == Ctrl%Ind%ICh(Ctrl%Ind%YSolar(k))) then
               j = k
            end if

            ! If both indices found, copy correlation
            if (i /= 0 .and. j /= 0) then
               MSI_Data%rho_dd_cor(:,:,i,j) = cor_temp(:,:,ind)
               MSI_Data%rho_dd_cor(:,:,j,i) = cor_temp(:,:,ind)
               exit
            end if
         end do
      end do

      ! Clean up
      deallocate(cor_ch_numbers)
      deallocate(cor_temp)
   end if

   deallocate(alb_instr_ch_numbers)
   deallocate(subs)

   ! Close alb input file
   call ncdf_close(ncid, 'read_alb_nc()')

end subroutine Read_ALB_nc
