!-------------------------------------------------------------------------------
! Name: GetSurface.F90
!
! Purpose:
! Passes surface reflectances for the solar channels and current super-pixel
! array according to the method defined by Ctrl%RS%RsSelm.
! Also calculates the covariances for the solar channels.
!
! Description and Algorithm details:
! Depending on the method specified for setting surface reflectance:
! - Ctrl) Get B values from Ctrl struct, which are a function of surface type.
!      Assume a constant fractional uncertainty on and correlation between those.
!
! - MDAD) Not supported
!
! - AUX) Draw BRDF values from preprocessor files.
!      Set uncertainty based on SRsSelm:
!      - Ctrl) Constant fractional uncertainty on and correlation between terms.
!      - AUX) Draw uncertainty on and correlation between terms from preprocessor
!           files. In addition, if the NDVI and NDSI can be calculated, add
!           uncertainties due to natural variability in the surface over 16 days.
!           Then add uncertainties due to differences in the channel bandpass
!           and temporal sampling of this instrument relative to MODIS.
!
! Then, form the XIndex and Ratios arrays. These have the same shape as the Rs
! array. The intention is that Rs is set from the state vector by,
!    Rs = X(XIndex) * Ratios
! Thus, an element of XIndex indicates which term of the state vector this Rs
! term is proportional to and the corresponding element of Ratios gives the
! constant of proportionality. (These facilitate the VariableSurf and FixedSurf
! modes of the aerosol retrieval. For traditional cloud processing, XIndex=IRs
! and Ratios=1 as the terms are assumed constant.)
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
! 2015/08/19, AP: Extensive overhaul: Uncertainties and correlations can now be
!    drawn from auxiliary files; The XIndex array maps elements of the state
!    vector onto the Rs array.
! 2015/01/06, AP: Fix a minor channel indexing bug when searching for solar chs.
!    As aerosol preprocessing does not calculate Rho_DV, remove check for its
!    validity. Add check for missing values in auxiliary uncertainties.
! 2015/01/07, AP: solar_factor didn't consider multiple views.
!
! $Id$
!
! Bugs:
! SelmCtrl only sets IRho_DD.
! SRs2 is only set for IRho_DD as it isn't clear where uncertainty estimates for
!    the other terms would come from.
! Invalid surface reflectances are ignored (for backwards compatibility).
!-------------------------------------------------------------------------------

subroutine Get_Surface(Ctrl, SAD_Chan, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use SAD_Chan_def

   implicit none

   ! Define arguments
   type(CTRL_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status

   ! Define local variables
   integer :: i, ii, j, jj
   integer :: i_surf, i_rho, j_rho, i_wvl
   logical :: surf_unc
   real    :: ndvi, ndsi
   real    :: solar_factor
   real    :: correl(SPixel%Ind%NSolar, SPixel%Ind%NSolar)
   integer :: nch, min_rho
   integer, dimension(SPixel%Ind%NSolar)            :: i_s, i_c
   real,    dimension(SPixel%Ind%NSolar)            :: uncertainty
   real,    dimension(SPixel%Ind%NSolar, MaxRho_XX) :: uncertainty2, additional
   real,    dimension(Ctrl%Ind%NSolar, MaxSurf)     :: Rs, frac_error


   status      = 0
   uncertainty = 0.
   uncertainty2= 0.
   additional  = 0.
   correl      = 0.

   ! Reallocate surface reflectances to appropriate length
   deallocate(SPixel%Surface%Rs)
   allocate(SPixel%Surface%Rs(SPixel%Ind%NSolar))
   deallocate(SPixel%Surface%SRs)
   allocate(SPixel%Surface%SRs(SPixel%Ind%NSolar, SPixel%Ind%NSolar))
   SPixel%Surface%Rs = 0.
   SPixel%Surface%SRs = 0.
   if (Ctrl%RS%use_full_brdf) then
      deallocate(SPixel%Surface%Rs2)
      allocate(SPixel%Surface%Rs2(SPixel%Ind%NSolar, MaxRho_XX))
      deallocate(SPixel%Surface%SRs2)
      allocate(SPixel%Surface%SRs2(SPixel%Ind%NSolar, SPixel%Ind%NSolar, MaxRho_XX))
      SPixel%Surface%Rs2 = 0.
      SPixel%Surface%SRs2 = 0.
   end if

   ! Properties may be a function of surface type
   if (SPixel%Surface%Land) then
      i_surf = ILand
   else
      i_surf = ISea
   end if

   ! These are copied into temporary arrays in case we ever make Ctrl%Rs terms
   ! arrays rather than scalars
   Rs         = Ctrl%RS%b
   frac_error = Ctrl%RS%Sb


   ! Get the surface reflectances
   select case (Ctrl%RS%RsSelm)
   case(SelmCtrl) ! Use values specified in driver file (only white sky albedo)
      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

         ! Use constant surface reflectances
         SPixel%Surface%Rs(i) = Rs(ii,i_surf)
         ! Uncertainty is a constant fraction of the reflectance
         uncertainty(i) = Rs(ii,i_surf) * frac_error(ii,i_surf)
      end do

      ! Constant correlation between channels
      correl = Ctrl%RS%Cb

   case(SelmAux) ! Use values in preprocessor files
      ! Determine vegetation and snow indexes outside of loop
      if (Ctrl%Rs%SRsSelm == SelmAux .and. SPixel%Surface%Land) &
           surf_unc = Calculate_ND(SAD_Chan, SPixel, ndvi) == 0 .and. &
                      Calculate_ND(SAD_Chan, SPixel, ndsi, snow=.true.) == 0

      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

         ! ACP: No idea why this is here. CP added it in R595. Not in aerosol.
         solar_factor = 1. / cos(SPixel%Geom%solzen(SPixel%ViewIdx(SPixel%Ind%YSolar(i))) * d2r)

         ! Copy surface reflectances from MSI files
         SPixel%Surface%Rs(i) = MSI_Data%ALB(SPixel%Loc%X0, SPixel%Loc%Y0, ii) /&
                                solar_factor
         if (Ctrl%RS%use_full_brdf) then
            SPixel%Surface%Rs2(i,IRho_0V) = MSI_Data%rho_0v(SPixel%Loc%X0, &
                                            SPixel%Loc%Y0, ii) / solar_factor
            SPixel%Surface%Rs2(i,IRho_0D) = MSI_Data%rho_0d(SPixel%Loc%X0, &
                                            SPixel%Loc%Y0, ii) / solar_factor
            SPixel%Surface%Rs2(i,IRho_DV) = MSI_Data%rho_dv(SPixel%Loc%X0, &
                                            SPixel%Loc%Y0, ii) / solar_factor
            SPixel%Surface%Rs2(i,IRho_DD) = MSI_Data%rho_dd(SPixel%Loc%X0, &
                                            SPixel%Loc%Y0, ii) / solar_factor
         end if

         ! Check for invalid values
         if (SPixel%Surface%Rs(i) < RhoMin .or. SPixel%Surface%Rs(i) > RhoMax) then
#ifdef DEBUG
            write(*, *) 'WARNING: Get_Surface(): Invalid surface property ' // &
                 'in pixel at: ', SPixel%Loc%X0, SPixel%Loc%Y0
#endif
!           SPixel%Surface%Rs(i) = Rs(ii,i_surf)
            status = SPixelSurfglint
            return
         end if
         if (Ctrl%RS%use_full_brdf) then
!           if (any(SPixel%Surface%Rs2(i,:) < RhoMin .or. &
!                   SPixel%Surface%Rs2(i,:) > RhoMax)) then
            ! TEMPORARY measure until either the aerosol preprocessing outputs
            ! Rho_DV or i_equation_form == 0.
            if ( SPixel%Surface%Rs2(i,IRho_0V) < RhoMin .or. &
                 SPixel%Surface%Rs2(i,IRho_0D) < RhoMin .or. &
!                SPixel%Surface%Rs2(i,IRho_DV) < RhoMin .or. &
                 SPixel%Surface%Rs2(i,IRho_DD) < RhoMin .or. &
                 SPixel%Surface%Rs2(i,IRho_0V) > RhoMax .or. &
                 SPixel%Surface%Rs2(i,IRho_0D) > RhoMax .or. &
!                SPixel%Surface%Rs2(i,IRho_DV) > RhoMax .or. &
                 SPixel%Surface%Rs2(i,IRho_DD) > RhoMax) then
#ifdef DEBUG
               write(*, *) 'WARNING: Get_Surface(): Invalid surface property ' // &
                    'in pixel at: ', SPixel%Loc%X0, SPixel%Loc%Y0
#endif
!              SPixel%Surface%Rs2(i,:) = Rs(ii,i_surf)
               status = SPixelSurfglint
               return
            end if
         end if

         ! Fetch uncertainty information
         select case (Ctrl%RS%SRsSelm)
         case(SelmCtrl)
            ! Uncertainty is a constant fraction of the reflectance
            uncertainty(i) = SPixel%Surface%Rs(i) * frac_error(ii,i_surf)
            if (Ctrl%RS%use_full_brdf) &
                 uncertainty2(i,:) = uncertainty(i)
!                uncertainty2(i,:) = SPixel%Surface%Rs2(i,:) * frac_error(ii,i_surf)
            ! Constant correlation between channels
            correl = Ctrl%RS%Cb
         case(SelmAux)
            ! Uncertainty and correlations drawn from MSI files
            uncertainty2(i,IRho_DD) = MSI_Data%rho_dd_unc(SPixel%Loc%X0, &
                                                          SPixel%Loc%Y0, ii)
            do j = 1, SPixel%Ind%NSolar
               correl(:,j) = MSI_Data%rho_dd_cor(SPixel%Loc%X0, &
                    SPixel%Loc%Y0, SPixel%Ind%YSolar, SPixel%Ind%YSolar(j))
            end do

            if (uncertainty2(i,IRho_DD) < RhoErrMin .or. &
                uncertainty2(i,IRho_DD) > RhoErrMax .or. &
                any(correl < CorrMin) .or. any(correl > CorrMax)) then
               write(*,*) 'WARNING: Get_Surface(): Invalid surface uncertainty '// &
                    'in pixel at: ', SPixel%Loc%X0, SPixel%Loc%Y0
               status = SPixelSurfErr
               return
            end if

            ! Use vegetation and snow indexes to assign surface variablity unc.
            ! See http://eodg.atm.ox.ac.uk/eodg/mphys_reports/2009_Ward.pdf
            if (surf_unc) then
               if (ndsi >= 0.4) then
                  additional(i,IRho_DD) = MSI_Data%snow_unc(ii) * &
                                          MSI_Data%snow_unc(ii)
               else
                  if (ndvi >= 0.3) then
                     additional(i,IRho_DD) = MSI_Data%veg_unc(ii) * &
                                             MSI_Data%veg_unc(ii)
                  else
                     additional(i,IRho_DD) = MSI_Data%bare_unc(ii) * &
                                             MSI_Data%bare_unc(ii)
                  end if
               end if
            end if

            ! From aerosol's Invert_Marquadt, labelled TEMPORARY TEST
            additional(i,IRho_DD) = additional(i,IRho_DD) + &
                 0.04*SPixel%Surface%Rs2(i,IRho_DD)*SPixel%Surface%Rs2(i,IRho_DD)

            ! Over land, include SVD/temporal uncertainties
            if (SPixel%Surface%Land) &
                 additional(i,IRho_DD) = additional(i,IRho_DD) + &
                            MSI_Data%svd_unc(ii) * MSI_Data%svd_unc(ii)

         end select

      end do
   end select


   ! Form covariance matrices
   do i = 1, SPixel%Ind%NSolar
      ! On-diagonal variance
      SPixel%Surface%SRs(i,i) = uncertainty(i) * uncertainty(i)

      ! Off-diagonal covariance
      do j = i+1, SPixel%Ind%NSolar
         SPixel%Surface%SRs(i,j) = correl(i,j) * uncertainty(i) * uncertainty(j)
         SPixel%Surface%SRs(j,i) = SPixel%Surface%SRs(i,j)
      end do
   end do

   if (Ctrl%RS%use_full_brdf) then
      do i_rho = 1, MaxRho_XX
         do i = 1, SPixel%Ind%NSolar
            ! On-diagonal variance
            SPixel%Surface%SRs2(i,i,i_rho) = additional(i,i_rho) + &
                 uncertainty2(i,i_rho) * uncertainty2(i,i_rho)

            ! Off-diagonal covariance
            do j = i+1, SPixel%Ind%NSolar
               SPixel%Surface%SRs2(i,j,i_rho) = correl(i,j) * &
                    uncertainty2(i,i_rho) * uncertainty2(j,i_rho)
               SPixel%Surface%SRs2(j,i,i_rho) = SPixel%Surface%SRs2(i,j,i_rho)
            end do
         end do
      end do
   end if

   ! Set up Ratios and XIndex arrays.
   deallocate(SPixel%Surface%XIndex)
   allocate(SPixel%Surface%XIndex(SPixel%Ind%NSolar, MaxRho_XX))
   SPixel%Surface%XIndex = 0
   deallocate(SPixel%Surface%Ratios)
   allocate(SPixel%Surface%Ratios(SPixel%Ind%NSolar, MaxRho_XX))

   do i_wvl = 1, Ctrl%Ind%NWvl
      ! Identify which valid solar channels are at this wavelength
      nch = 0
      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_index(i)
         if (Ctrl%Ind%WvlIdx(ii) == i_wvl) then
            nch      = nch+1
            i_s(nch) = i
            i_c(nch) = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)
         end if
      end do
      if (nch == 0) cycle

      if (Ctrl%RS%use_full_brdf) then
         min_rho = 1
      else
         min_rho = MaxRho_XX
      end if
      do i_rho = min_rho, MaxRho_XX
         if (any(SPixel%X  == IRs(i_c(1),i_rho)) .or. &
             any(SPixel%XJ == IRs(i_c(1),i_rho)) .or. &
             any(SPixel%XI == IRs(i_c(1),i_rho))) then
            ! Value is retrieved or is a parameter
            SPixel%Surface%XIndex(i_s(1:nch),i_rho) = IRs(i_c(1),i_rho)
            SPixel%Surface%Ratios(i_s(1:nch),i_rho) = 1.
         else
            ! Is another parameter for this channel retrieved? Analogue to
            ! VariableSurface in aerosol code.
            do j_rho = min_rho, MaxRho_XX
               if (any(SPixel%X == IRs(i_c(1),j_rho))) then
                  SPixel%Surface%XIndex(i_s(1:nch),i_rho) = IRs(i_c(1),j_rho)
                  SPixel%Surface%Ratios(i_s(1:nch),i_rho) = &
                       SPixel%Surface%Rs2(i_s(1:nch),i_rho) / &
                       SPixel%Surface%Rs2(i_s(1),j_rho)
                  go to 10
               end if
            end do
            ! No parameters for this wavelength retrieved. Therefore, find the
            ! *first* parameter that is and associate to that. Analogue to
            ! FixedSurface in aerosol code.
            do j = 1, Ctrl%Ind%NSolar
               do j_rho = min_rho, MaxRho_XX
                  if (any(SPixel%X == IRs(j,j_rho))) then
                     SPixel%Surface%XIndex(i_s(1:nch),i_rho) = IRs(j,j_rho)
                     SPixel%Surface%Ratios(i_s(1:nch),i_rho) = &
                          SPixel%Surface%Rs2(i_s(1:nch),i_rho) / &
                          SPixel%Surface%Rs2(j,j_rho)
                     goto 10
                  end if
               end do
            end do
            ! Should never get here
            write(*,*) 'WARNING: Get_Surface(): No surface terms in state ' //&
                 'vector at ', SPixel%Loc%X0, SPixel%Loc%Y0
10       end if
      end do
   end do

end subroutine Get_Surface
