!-------------------------------------------------------------------------------
! Name: GetSurface.F90
!
! Purpose:
! Passes surface reflectances for the solar channels and current super-pixel
! array according to the method defined by Ctrl%Rs%Flag to Get_Rs.
! Get_Rs calculates the mean surface reflectance of the super pixel.
! Also calculates the covariances for the solar channels.
!
! Description and Algorithm details:
! Depending on the method specified for setting surface reflectance
! (Ctrl%Rs%Flag):
! - Ctrl
!      Ctrl: Get B values from Ctrl struct
!             Set Sb using Ctrl values
!      Calculate an overall B value for the super-pixel in each channel,
!         using the land/sea flags to pick out whether each pixel in the
!         super-pixel is land or sea and combining the results for the two
!         surface types.
!      Similarly, calculate on-diagonal covariance terms Sb
!      Calculate off-diagonal Sb terms using the correlation between channels
!         Ctrl%Surface%Cb
!      Set SPixel values for total number of land and sea pixels
!      Call Get_Rs to average reflectance values over the super-pixel
!
! - MDAD
!      Not supported
!
! - AUX
!      Use albedo value read from Aux file.
!
! Arguments:
! Name      Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl      struct       In          Control structure
! SPixel    alloc struct Both        Super-pixel structure
! MSI_Data  struct       In          Data structure. Contains the multi-
!                                    spectral image measurements, location
!                                    values, geometry etc for the current
!                                    image segment, from which the current
!                                    SPixel values will be extracted.
! status    integer      Out         Error status
!
! History:
! 2000/12/01, KS: Original version
! 2000/12/04, KS: Included Get_Surface_Rs subroutine
! 2000/12/19, KS: Replaced Data_LSFlags array with Data structure
! 2001/01/16, KS: Re-drafted
! 2001/01/24, KS: Moved allocations to ECP main on integration
! 2001/02/08, KS: Corrected error in calculation of b
! 2001/03/16, AS: Removed checking for invalid "GetSurface" method.
!    Using named constants for selection method.
! 2001/04/03, AS: Replaced loop over channels for setting b in the Ctrl case
!    with a whole array assignment. Similar for Sb in both Ctrl and SAD cases.
! 2001/06/25, AS: Completed header comments.
! 2001/08/07, AS: Updates for image segmentation. Selection of values from the
!    MSI Data  structure arrays now need to use a y value that refers to the
!    image segment currently held in memory rather than the whole image area. X
!    co-ords are unchanged since the segment is the full image width. Renamed
!    structure Data to MSI_Data since Data is a reserved word (hasn't caused any
!    problems so far but it might). Added argument intent.
! 2001/10/22, AS: Updated comments.
!    **************** ECV work starts here *************************************
! 2011/02/22, AS: Re-introducing changes made in late 2001/2002.
! 2002/05/29, CP: Changed the routine to be able to read auxiliary albedo
!    information)
! 2011/03/10, AS: plus later albedo changes made by Oxford.
!    Replaced Ctrl%Ind values in array size declarations by SPixel equivalents
!    Updated loops for b, Sb setting to use SPixel ind as well - not done in
!    ORAC code)
! 2011/03/30, AS: Removal of super-pixel averaging. Process single pixels at X0,
!    Y0, removed other SPixel indices Xc, Yc, Xn, Yn etc. SPixel_B and SPixel_Sb
!    re-dimensioned to remove SPixel size.
! 2011/04/05, AS: Removed selection methods SAD and SDAD. SelmMDAD renamed
!    SelmMeas. SAD_Chan struct no longer needed here: argument list updated.
!    Bug fix from super-pixel removal: calc of spixel_b was still using index j
!    (pixel position with super-pixel) when indexing the MSI_Data ALB array.
! 2011/04/26, AS: Extension to handle multiple instrument views. Commented out
!    setting of unused variable solar_factor.
! 2011/05/19, AS: Multiple views (2). Commented out setting of qc1, qc2 as these
!    are not used and the assignment refers specifically to channels 5 and 6 of
!    the albedo data - may change in future and may be specific to one instrument
! 2011/06/08, AS:  Removed calls to Write_Log to improve performance
!    (add ifdef DEBUG).
! 2011/12/08, CP: Changed y_id to chi.
! 2011/12/13, CP: Added byte 2 unsigned integer routine to make g95 compatible.
! 2012/01/27, CA: Bugfix: added call to byte to uint routine for AUX method
!    (ln 349)
! 2012/05/01, CP: added in the solar factor remove
!    Factor 1 for multiplication
! 2012/09/09, CP: changed indexing on albedo data set  have to be really
!    Careful here as aatsr preprocessing reads out albedo for 55 channel.
! 2012/09/21, CP: removed albedo scaling factor as netcdf value doe not need
!    scaling
! 2013/10/01, GT: Bug-fix: Changed Y_ID to ChI when indexing albedo data
!    (from MSI_Data%ALB). Y_ID gives the Channel ID numbers, rather than
!    their index numbers within the data arrays.
! 2013/11/19, MJ: Changes channel indices to ysolar_msi as this gives the
!    indices of the solar channels as stored in the MSI array.
! 2014/04/19, GM: Remove incorrect use of b1_to_ui1() for implicit conversion.
! 2014/04/20, GM: Cleaned up the code.
! 2014/08/01, GM: Use of SPixel%spixel_*_y_to_ctrl_y_index(:) to properly
!    index the right channels from the surface fields.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2015/01/15, AP: Facilitate channel indexing in arbitrary order.
! 2015/01/15, AP: The preprocessed MODIS albedo data was always rejected for
!    Ch5 of AATSR. There is no reason to do so. This has been removed.
! 2015/01/20, AP: Tidying redundant code.
! 2015/01/21, AP: Move allocations of reflectance arrays here.
! 2014/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/07/27, AP: Remove SPixel%Mask and Get_Rs. Consolidate setting LSFlag.
!
! $Id$
!
! Bugs:
! AS, Mar 2011. The Aux method albedo code includes specific handling for
! channel 5, which presumably assumes that we are processing ATSR data and
! makes this code harder to generalise to other instruments.
!-------------------------------------------------------------------------------

subroutine Get_Surface(Ctrl, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   type(Data_t),   intent(in)    :: MSI_Data
   integer,        intent(out)   :: status

   ! Define local variables

   real           :: b( Ctrl%Ind%NSolar, 2)
   real           :: Sb(Ctrl%Ind%NSolar, 2)
   real           :: SPixel_b(  SPixel%Ind%NSolar)
   real           :: SPixel_Sb( SPixel%Ind%NSolar, SPixel%Ind%NSolar)
   real           :: SPixel_b2( SPixel%Ind%NSolar, MaxRho_XX)
   real           :: SPixel_Sb2(SPixel%Ind%NSolar, SPixel%Ind%NSolar, MaxRho_XX)
   integer        :: i, ii, ic, j, jj, iflag
!  integer        :: qc1,qc2 ! N.B. qc vars are set but not used
   real           :: solar_factor

   ! Initialise output
   status     = 0
   SPixel_b   = 0.0
   SPixel_Sb  = 0.0
   SPixel_b2  = 0.0
   SPixel_Sb2 = 0.0

   ! Reallocate surface reflectances to appropriate length
   deallocate(SPixel%Surface%Rs)
   allocate(SPixel%Surface%Rs(SPixel%Ind%NSolar))
   deallocate(SPixel%Surface%SRs)
   allocate(SPixel%Surface%SRs(SPixel%Ind%NSolar, SPixel%Ind%NSolar))
   if (Ctrl%RS%use_full_brdf) then
      deallocate(SPixel%Surface%Rs2)
      allocate(SPixel%Surface%Rs2(SPixel%Ind%NSolar, MaxRho_XX))
      deallocate(SPixel%Surface%SRs2)
      allocate(SPixel%Surface%SRs2(SPixel%Ind%NSolar, SPixel%Ind%NSolar, MaxRho_XX))
   end if

   ! Get the surface reflectances etc for the super pixel array according to
   ! the specified method

   ! Ctrl initialization method
   if (Ctrl%Rs%Flag == SelmCtrl) then
      ! Get the surface flag super pixel array (only if the Ctrl or SAD method
      ! is set).
      !
      ! Note: The QC mask is applied later in all calculations involving the
      ! surface flags
      !
      ! Note: setting by these methods could be done higher up in ECP, as the
      ! assignment only needs to be done once for each run rather than once per
      !      super-pixel.


      ! Loop over channels to assign b, Sb. Cb is taken straight from Ctrl%Rs%Cb
      ! in both methods.
      !
      ! Note: This cannot be done in the main loop because the off-diagonal
      ! calculation of the covariance requires element i+1 to NSolar (which
      ! would not have been calculated).

      Sb = Ctrl%Rs%Sb ! Note this is a single value filling an array
      b  = Ctrl%Rs%b

      if (SPixel%Surface%Land) then
         iflag = 2
      else
         iflag = 1
      end if
      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

         ! Calculate b
         SPixel_b(i) = Ctrl%Rs%b(ii,iflag)

         ! Calculate Sb

         ! On diagonals
         SPixel_Sb(i,i) = b(ii,iflag) * b(ii,iflag) * Sb(ii,iflag) * Sb(ii,iflag)

         ! Off diagonals (loop over half of the matrix, and swap indices for
         ! other half, sea then land)
         do j = i+1, SPixel%Ind%NSolar
            jj = SPixel%spixel_y_solar_to_ctrl_y_solar_index(j)

	    SPixel_Sb(i,j) = Ctrl%Rs%Cb * b(ii,iflag) * b(jj,iflag) * &
                             Sb(ii,iflag) * Sb(jj,iflag)

	    SPixel_Sb(j,i) = SPixel_Sb(i,j)
         end do
      end do

   ! Meas method (not supported in ECP delivery)
   else if (Ctrl%Rs%Flag == SelmMeas) then

   ! AUX method: use Albedo data to set Rs
   else if (Ctrl%Rs%Flag == SelmAux) then

      Sb = Ctrl%Rs%Sb ! Note this is a single value filling an array

      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)
         ic = SPixel%spixel_y_solar_to_ctrl_y_index(i)

         ! ACP: No idea why this is here. CP added it in R595. Not in aerosol.
         solar_factor = 1. / cos(SPixel%Geom%solzen(1) * d2r)

         SPixel_b(i) = MSI_Data%ALB(SPixel%Loc%X0, SPixel%Loc%Y0, ii) / &
                       solar_factor

         if (Ctrl%RS%use_full_brdf) then
            SPixel_b2(i,IRho_0V) = MSI_Data%rho_0v(SPixel%Loc%X0, &
                                   SPixel%Loc%Y0, ii) / solar_factor
            SPixel_b2(i,IRho_0D) = MSI_Data%rho_0d(SPixel%Loc%X0, &
                                   SPixel%Loc%Y0, ii) / solar_factor
            SPixel_b2(i,IRho_DV) = MSI_Data%rho_dv(SPixel%Loc%X0, &
                                   SPixel%Loc%Y0, ii) / solar_factor
            SPixel_b2(i,IRho_DD) = MSI_Data%rho_dd(SPixel%Loc%X0, &
                                   SPixel%Loc%Y0, ii) / solar_factor
         end if

         ! If sea and then sun glint replace by Ctrl value, otherwise if land,
         ! check for missing values and replace by Ctrl value
         !
         ! N.B. assumes either all channel albedos are present, or none are.

         if (.not. SPixel%Surface%Land) then
            if (SPixel_b(i) .ge. 1.0) then
#ifdef DEBUG
               write(*, *) 'WARNING: Get_Surface(): Sunglint region predicted ' // &
                    'over ocean in pixel at: ', SPixel%Loc%X0, SPixel%Loc%Y0
#endif
               status = SPixelSurfglint
            end if

         else if (SPixel_b(i) == 0.0) then ! missing land
            SPixel_b(i) = Ctrl%Rs%b(ii,2)
            if (Ctrl%RS%use_full_brdf) &
                 SPixel_b2(i,IRho_DD) = Ctrl%Rs%b(ii,2)
         else if (SPixel_b(i) >  1.0) then ! odd value for land
            SPixel_b(i) = Ctrl%Rs%b(ii,2)
            if (Ctrl%RS%use_full_brdf) &
                 SPixel_b2(i,IRho_DD) = Ctrl%Rs%b(ii,2)
         ! Comment out this section to install loose qc
!        else if (( qc1 == 1) .and. (qc2 >= 9 )) then
            ! Use the default value MODIS not reliable
!           SPixel_b(i) = Ctrl%Rs%b(ii,2)
!        else if (( qc1 > 1)) then
            ! Use the default value MODIS not reliable
!           SPixel_b(i) = Ctrl%Rs%b(ii,2)
         end if
      end do

      ! Calculate Sb
      if (SPixel%Surface%Land) then
         iflag = 2
      else
         iflag = 1
      end if
      if (status == 0) then
         do i = 1, SPixel%Ind%NSolar
            ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

            ! On diagonals
            SPixel_Sb(i,i) = SPixel_b(i) * SPixel_b(i) * &
                             Sb(ii,iflag) * Sb(ii,iflag)

            if (Ctrl%RS%use_full_brdf) &
               SPixel_Sb2(i,i,:) = SPixel_Sb(i,i)

            ! Calculate SPixel_Sb

            ! Off diagonals (loop over half of the matrix, and swap indices for
            ! other half)
            do j = i+1, SPixel%Ind%NSolar
               jj = SPixel%spixel_y_solar_to_ctrl_y_solar_index(j)

               SPixel_Sb(i,j) = Ctrl%Rs%Cb * SPixel_b(i) * SPixel_b(j) * &
                                Sb(ii,iflag) * Sb(jj,iflag)

               if (Ctrl%RS%use_full_brdf) &
                  SPixel_Sb2(i,j,:) = SPixel_Sb(i,j)

               SPixel_Sb(j,i) = SPixel_Sb(i,j)

               if (Ctrl%RS%use_full_brdf) &
                  SPixel_Sb2(j,i,:) = SPixel_Sb2(i,j,:)
            end do
         end do
      end if
   end if

   if (status == 0) then
      SPixel%Surface%Rs  = SPixel_b
      SPixel%Surface%SRs = SPixel_Sb

      if (Ctrl%RS%use_full_brdf) then
         SPixel%Surface%Rs2  = SPixel_b2
         SPixel%Surface%SRs2 = SPixel_Sb2
      end if
   end if

end subroutine Get_Surface
