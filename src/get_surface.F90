!-------------------------------------------------------------------------------
! Name: get_surface.F90
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
! 2011/06/08, AS:  Removed calls to Write_Log to improve performance (add ifdef
!    DEBUG).
! 2011/12/08, CP: Changed y_id to chi.
! 2011/12/13, CP: Added byte 2 unsigned integer routine to make g95 compatible.
! 2012/01/27, CA: Bugfix: added call to byte to uint routine for AUX method
!    (ln 349)
! 2012/05/01, CP: added in the solar factor, remove factor 1 for multiplication
! 2012/09/09, CP: changed indexing on albedo data set  have to be really
!    Careful here as aatsr preprocessing reads out albedo for 55 channel.
! 2012/09/21, CP: removed albedo scaling factor as netcdf value doe not need
!    scaling
! 2013/10/01, GT: Bug-fix: Changed Y_ID to ChI when indexing albedo data (from
!    MSI_Data%ALB). Y_ID gives the Channel ID numbers, rather than their index
!    numbers within the data arrays.
! 2013/11/19, MJ: Changes channel indices to ysolar_msi as this gives the
!    indices of the solar channels as stored in the MSI array.
! 2014/04/19, GM: Remove incorrect use of b1_to_ui1() for implicit conversion.
! 2014/04/20, GM: Cleaned up the code.
! 2014/08/01, GM: Use of SPixel%spixel_*_y_to_ctrl_y_index(:) to properly index
!    the right channels from the surface fields.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file.
! 2015/01/15, AP: Facilitate channel indexing in arbitrary order.
! 2015/01/15, AP: The preprocessed MODIS albedo data was always rejected for Ch5
!    of AATSR. There is no reason to do so. This has been removed.
! 2015/01/20, AP: Tidying redundant code.
! 2015/01/21, AP: Move allocations of reflectance arrays here.
! 2014/01/30, AP: Replace YSeg0 with Y0 as superpixeling removed.
! 2015/07/27, AP: Remove SPixel%Mask and Get_Rs. Consolidate setting LSFlag.
! 2015/08/19, AP: Extensive overhaul: Uncertainties and correlations can now be
!    drawn from auxiliary files; The XIndex array maps elements of the state
!    vector onto the Rs array.
! 2015/12/22, AP: Rename SRsSelm==SelmCtrl to SelmMeas as it is dependent on the
!    measurement. Add a SelmCtrl that draws a constant value from Ctrl.
! 2015/12/29, AP: Add_fractional increases the uncertainty in surface
!    reflectance by a fraction (set by Ctrl%RS%Sb) of the current value.
! 2015/01/06, AP: Fix a minor channel indexing bug when searching for solar chs.
!    As aerosol preprocessing does not calculate Rho_DV, remove check for its
!    validity. Add check for missing values in auxiliary uncertainties.
! 2015/01/07, AP: Solar_factor didn't consider multiple views.
! 2016/01/16, CP: Added in default values for suface if missing in climatology.
! 2016/02/02, GM: Make use of Ctrl%allow_a_default_surface.
!
! Bugs:
! SelmCtrl only sets IRho_DD.
! SRs2 is only set for IRho_DD as it isn't clear where uncertainty estimates for
!    the other terms would come from.
! Invalid surface reflectances are ignored (for backwards compatibility).
!-------------------------------------------------------------------------------

subroutine Get_Surface(Ctrl, SAD_Chan, SPixel, MSI_Data, status)

   use Ctrl_m
   use Data_m
   use ORAC_Constants_m
   use SAD_Chan_m

   implicit none

   ! Define arguments
   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status

   ! Define local variables
   integer :: i, ii, j
   integer :: i_surf, i_rho, j_rho, i_wvl
   logical :: surf_unc
   real    :: ndvi, ndsi
   real    :: solar_factor
   real    :: correl(SPixel%Ind%NSolar, SPixel%Ind%NSolar)
   integer :: nch, min_rho
   integer, dimension(SPixel%Ind%NSolar)            :: i_s, i_c
   real,    dimension(SPixel%Ind%NSolar)            :: uncertainty
   real,    dimension(SPixel%Ind%NSolar, MaxRho_XX) :: uncertainty2, additional
   real,    dimension(Ctrl%Ind%NSolar, MaxSurf)     :: Rs, frac_uncertainty


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
   Rs               = Ctrl%RS%b
   frac_uncertainty = Ctrl%RS%Sb


   ! Get the surface reflectances
   select case (Ctrl%RS%RsSelm)
   case(SelmCtrl) ! Use values specified in driver file (only white sky albedo)
      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

         ! Use constant surface reflectances
         SPixel%Surface%Rs(i) = Rs(ii,i_surf)
         ! Uncertainty is a constant fraction of the reflectance
         uncertainty(i) = Rs(ii,i_surf) * frac_uncertainty(ii,i_surf)
      end do

      ! Constant correlation between channels
      correl = Ctrl%RS%Cb

   case(SelmAux) ! Use values in preprocessor files
      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

         if (Ctrl%RS%solar_factor) then
            ! This affects d_REF(:,IRs) in FMSolar
            solar_factor = SPixel%Geom%SEC_o(SPixel%ViewIdx(SPixel%Ind%YSolar(i)))
         else
            ! Used by old Oxford surface model with i_equation_form == 2
            solar_factor = 1.
         end if

         ! Copy surface reflectances from MSI files
         SPixel%Surface%Rs(i) = MSI_Data%ALB(SPixel%Loc%X0, SPixel%Loc%Y0, ii) / &
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
            if (Ctrl%RS%allow_a_default_surface) then
               SPixel%Surface%Rs(i) = Rs(ii,i_surf)
            else
               status = SPixelSurfglint
               return
            end if
         end if
         if (Ctrl%RS%use_full_brdf) then
           if (any(SPixel%Surface%Rs2(i,:) < RhoMin .or. &
                   SPixel%Surface%Rs2(i,:) > RhoMax)) then
#ifdef DEBUG
               write(*, *) 'WARNING: Get_Surface(): Invalid surface ' // &
                    'property in pixel at: ', SPixel%Loc%X0, SPixel%Loc%Y0
#endif
               if (Ctrl%RS%allow_a_default_surface) then
                  SPixel%Surface%Rs2(i,:) = Rs(ii,i_surf)
               else
                  status = SPixelSurfglint
                  return
               end if
            end if
         end if
      end do

      ! Determine vegetation and snow indexes outside of loop
      if (Ctrl%RS%SRsSelm == SelmAux .and. SPixel%Surface%Land) then
         surf_unc = Calculate_ND(SAD_Chan, SPixel, ndvi) == 0 .and. &
                    Calculate_ND(SAD_Chan, SPixel, ndsi, snow=.true.) == 0
      else
         surf_unc = .false.
      end if

      do i = 1, SPixel%Ind%NSolar
         ii = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i)

         ! Fetch uncertainty information
         select case (Ctrl%RS%SRsSelm)
         case(SelmCtrl)
            ! Uncertainty is a constant drawn from the driver file
            uncertainty(i) = Ctrl%Sx(IRs(ii,IRho_DD))
            if (Ctrl%RS%use_full_brdf) then
               uncertainty2(i,:) = Ctrl%Sx(IRs(ii,:))

               ! A "temporary test" from the aerosol code
               if (Ctrl%RS%add_fractional) additional(i,IRho_DD) = &
                    frac_uncertainty(ii,i_surf) * frac_uncertainty(ii,i_surf) * &
                    SPixel%Surface%Rs2(i,IRho_DD) * SPixel%Surface%Rs2(i,IRho_DD)
            end if

            ! Constant correlation between channels
            correl = Ctrl%RS%Cb
         case(SelmMeas)
            ! Uncertainty is a constant fraction of the reflectance
            uncertainty(i) = SPixel%Surface%Rs(i) * frac_uncertainty(ii,i_surf)
            if (Ctrl%RS%use_full_brdf) &
                 uncertainty2(i,:) = uncertainty(i)
!                uncertainty2(i,:) = SPixel%Surface%Rs2(i,:) * frac_uncertainty(ii,i_surf)
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
               write(*,*) 'WARNING: Get_Surface(): Invalid surface '// &
                    'uncertainty in pixel at: ', SPixel%Loc%X0, SPixel%Loc%Y0
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

            ! Over land, include SVD/temporal uncertainties
            if (SPixel%Surface%Land) &
                 additional(i,IRho_DD) = additional(i,IRho_DD) + &
                 MSI_Data%svd_unc(ii) * MSI_Data%svd_unc(ii)

            ! A "temporary test" from the aerosol code
            if (Ctrl%RS%add_fractional) &
                 additional(i,IRho_DD) = additional(i,IRho_DD) + &
                 frac_uncertainty(ii,i_surf) * frac_uncertainty(ii,i_surf) * &
                 SPixel%Surface%Rs2(i,IRho_DD) * SPixel%Surface%Rs2(i,IRho_DD)
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


!-------------------------------------------------------------------------------
! Name: Get_Surface_Swansea
!
! Purpose:
! Sets XIndex array for Swansea model retrievals.
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2015/08/17, AP: Original version
! 2016/09/23, AP: Add Meas and Aux options for first guess selection.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Get_Surface_Swansea(Ctrl, SPixel, SAD_LUT, MSI_Data)

   use Ctrl_m
   use Data_m
   use GZero_m
   use FM_Routines_m, only: Set_CRP_Solar
   use Int_LUT_Routines_m, only: MaxCRPParams
   use ORAC_Constants_m
   use SAD_LUT_m

   implicit none

   ! Define arguments

   type(Ctrl_t),    intent(in)    :: Ctrl
   type(SPixel_t),  intent(inout) :: SPixel
   type(SAD_LUT_t), intent(in)    :: SAD_LUT
   type(Data_t),    intent(in)    :: MSI_Data

   ! Define local variables
   integer :: i_solar, i_ctrl, j_solar, j_ctrl, i_surf, i_csol, i_view
   integer :: nch, i_s(SPixel%Ind%NSolar)
   logical :: checked(SPixel%Ind%NSolar)

   type(GZero_t) :: GZero
   integer       :: status
   real          :: Sw_g, Sw_s, Sw_s_var
   real          :: rho_0v, rho_dd, rs_unc, denom, tmp
   real          :: CRP(SPixel%Ind%NSolar, MaxCRProps)
   real          :: d_CRP(SPixel%Ind%NSolar, MaxCRProps, MaxCRPParams)

   checked = .false.

   ! Reallocate surface reflectances to appropriate length
   deallocate(SPixel%Surface%Sw_s)
   allocate(SPixel%Surface%Sw_s(SPixel%Ind%NSolar))
   deallocate(SPixel%Surface%Sw_s_var)
   allocate(SPixel%Surface%Sw_s_var(SPixel%Ind%NSolar))
   SPixel%Surface%Sw_s = 0.
   SPixel%Surface%Sw_s_var = 0.
   if (Ctrl%RS%read_full_brdf) then
      deallocate(SPixel%Surface%Sw_p)
      allocate(SPixel%Surface%Sw_p(Ctrl%Ind%NViews))
      deallocate(SPixel%Surface%Sw_p_var)
      allocate(SPixel%Surface%Sw_p_var(Ctrl%Ind%NViews))
      SPixel%Surface%Sw_p = 0
      SPixel%Surface%Sw_p_var = 0
   end if
   deallocate(SPixel%Surface%XIndex)
   allocate(SPixel%Surface%XIndex(SPixel%Ind%NSolar, MaxSwan_X))
   SPixel%Surface%XIndex = 0

   if (Ctrl%RS%RsSelm == SelmMeas) then
      ! Interpolate LUTs using a priori
      call Allocate_GZero(GZero, SPixel%Ind%Ny)
      call Set_GZero(Ctrl%RS%SS_Xb(ITau), Ctrl%RS%SS_Xb(IRe), Ctrl, SPixel, &
           SAD_LUT, GZero, status)
      call Set_CRP_Solar(Ctrl, SPixel%Ind, &
           SPixel%spixel_y_solar_to_ctrl_y_index, &
           GZero, SAD_LUT, CRP, d_CRP, status)
      call Deallocate_GZero(GZero)
   end if

   if (SPixel%Surface%Land) then
      i_surf = ILand
   else
      i_surf = ISea
   end if


   ! Copy over indices for view-dependant P parameter
   SPixel%Surface%XIndex(:, ISwan_P) = ISP(SPixel%ViewIdx)

   ! ----- Estimate s parameter -----
   do i_solar = 1, SPixel%Ind%NSolar
      ! Skip wavelengths we've already dealt with
      if (checked(i_solar)) cycle

      i_ctrl = SPixel%spixel_y_solar_to_ctrl_y_index(i_solar)
      i_csol = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i_solar)
      i_view = SPixel%ViewIdx(SPixel%Ind%YSolar(i_solar))

      ! Make a list of channels that share this wavelength
      nch = 1
      i_s(1) = i_solar
!     checked(i_solar) = .true.

      do j_solar = i_solar+1, SPixel%Ind%NSolar
         j_ctrl = SPixel%spixel_y_solar_to_ctrl_y_index(j_solar)

         if (Ctrl%Ind%WvlIdx(i_ctrl) == Ctrl%Ind%WvlIdx(j_ctrl)) then
            nch = nch+1
            i_s(nch) = j_solar
            checked(j_solar) = .true.
         end if
      end do

      ! For each wavelength, copy over index for S parameter
      SPixel%Surface%XIndex(i_s(1:nch), ISwan_S) = ISS(i_csol)


      select case (Ctrl%RS%RsSelm)
      case(SelmCtrl)
         ! Use constant values for s and its uncertainty
         Sw_s = Ctrl%RS%b(i_csol, i_surf)

         Sw_s_var = Ctrl%Sx(ISS(i_csol))

         if (Ctrl%RS%add_fractional) then
            ! Consistent with the routine above, Sb is a fractional uncertainty
            tmp = Ctrl%RS%b(i_csol, i_surf) * Ctrl%RS%Sb(i_csol, i_surf)
            Sw_s_var = Sw_s_var + tmp * tmp
         end if

      case(SelmMeas)
         ! Invert nadir radiance for S parameter
         call Effective_Rho_Calc(SPixel%Ym(i_s(1)), SPixel%Sy(i_s(1),i_s(1)), &
              CRP(i_s(1),:), d_CRP(i_s(1),:,:), Ctrl%RS%SS_Xb(ISP(1)), &
              Ctrl%RS%SS_Xb(ISG), Ctrl%RS%SS_Sx(ITau), Ctrl%RS%SS_Sx(IRe), &
              Ctrl%RS%SS_Sx(ISP(i_view)), Ctrl%RS%SS_Sx(ISG), &
              Sw_s, Sw_s_var)

         select case (Ctrl%RS%SRsSelm)
         case(SelmCtrl)
            ! Use constant uncertainty
            Sw_s_var = Ctrl%Sx(ISS(i_csol))
            if (Ctrl%RS%add_fractional) then
               tmp = Sw_s * Ctrl%RS%Sb(i_csol, i_surf)
               Sw_s_var = Sw_s_var + tmp * tmp
            end if
         !case(SelmMeas) ! Use the uncertainty propagated from a priori
         !case(SelmAux) ! There's nowhere from which this would come
         end select

      case(SelmAux)
         ! Deduce S parameter from albedo in preprocessor files

         ! There are a lot of possible methods here. There are 4 BRDF terms per
         ! pixel (0D, DD, and 2 0V) with 4 unknowns, but the eventual result
         ! isn't bounded to [0,1] and so doesn't help much. The easiest approach
         ! is to use rho_0v = p * s and assume p, but that is very sensitive to
         ! the p assumed and easily gives s > 1. When the BRDF terms are
         ! available, we can use rho_DD = g * s / (1 - (1-g) * s) to find s
         ! and rho_0v to find p. (There is an expression using rho_0D for g,
         ! but it tends to be inconsistent across the channels.)

         if (Ctrl%RS%read_full_brdf) then
            rho_dd = MSI_Data%rho_dd(SPixel%Loc%X0, SPixel%Loc%Y0, i_csol)
         else
            rho_dd = MSI_Data%ALB(SPixel%Loc%X0, SPixel%Loc%Y0, i_csol)
         end if

         Sw_g = Ctrl%RS%SS_Xb(ISG)
         denom = Sw_g + (1.0 - Sw_g) * rho_dd
         Sw_s = rho_dd / denom

         if (Ctrl%RS%SRsSelm == SelmCtrl) then
            ! Use constant uncertainty
            Sw_s_var = Ctrl%Sx(ISS(i_csol))

            if (Ctrl%RS%add_fractional) then
               tmp = Sw_s * Ctrl%RS%Sb(i_csol, i_surf)
               Sw_s_var = Sw_s_var + tmp * tmp
            end if
         else
            ! Use uncertainty propagated through the equations above
            if (Ctrl%RS%SRsSelm == SelmMeas) then
               ! Uncertainty is a constant fraction of the reflectance
               rs_unc = rho_dd * Ctrl%Rs%Sb(i_csol, i_surf)
            else ! (Ctrl%RS%SRsSelm == SelmAux)
               rs_unc = MSI_Data%rho_dd_unc(SPixel%Loc%X0, SPixel%Loc%Y0, i_csol)
               ! ACP: Perhaps snow/veg/bare terms needed here?
            end if

            tmp = rho_dd * (1. - rho_dd) * Ctrl%Sx(ISG)
            Sw_s_var = (Sw_g * Sw_g * rs_unc * rs_unc + tmp * tmp) / &
                 (denom * denom * denom * denom)
         end if
      end select

      SPixel%Surface%Sw_s(i_s(1:nch)) = Sw_s
      SPixel%Surface%Sw_s_var(i_s(1:nch)) = Sw_s_var
   end do

   ! ----- Estimate p parameter -----
   if (Ctrl%RS%read_full_brdf .and. Ctrl%RS%RSSelm == SelmAux) then
      checked = .false.
      do i_solar = 1, SPixel%Ind%NSolar
         ! Skip wavelengths we've already dealt with
         if (checked(i_solar)) cycle

         i_csol = SPixel%spixel_y_solar_to_ctrl_y_solar_index(i_solar)
         i_view = SPixel%ViewIdx(SPixel%Ind%YSolar(i_solar))

         ! Find channels that share this view
         do j_solar = i_solar+1, SPixel%Ind%NSolar
            if (i_view == SPixel%ViewIdx(SPixel%Ind%YSolar(j_solar))) then
               checked(j_solar) = .true.
            end if
         end do

         rho_0v = MSI_Data%rho_0v(SPixel%Loc%X0, SPixel%Loc%Y0, i_csol)
         SPixel%Surface%Sw_p(i_view) = rho_0v / SPixel%Surface%Sw_s(i_solar)

         if (Ctrl%RS%SRsSelm == SelmCtrl) then
            SPixel%Surface%Sw_p_var(i_view) = Ctrl%Sx(ISP(i_view))
         else
            if (Ctrl%RS%SRsSelm == SelmMeas) then
               rs_unc = rho_0v * Ctrl%Rs%Sb(i_csol, i_surf)
            else
               ! MSI_Data%rho_0v_unc doesn't exist, so we approximate
               rs_unc = MSI_Data%rho_dd_unc(SPixel%Loc%X0, SPixel%Loc%Y0, i_csol)
            end if

            tmp = SPixel%Surface%Sw_s(i_solar) * SPixel%Surface%Sw_s(i_solar)
            SPixel%Surface%Sw_p_var(i_view) = &
                 (tmp * rs_unc * rs_unc + &
                 rho_0v * rho_0v * SPixel%Surface%Sw_s_var(i_solar) * &
                 SPixel%Surface%Sw_s_var(i_solar)) / (tmp * tmp)
         end if
      end do
   end if

end subroutine Get_Surface_Swansea


subroutine Effective_Rho_Calc(Y, Sy, CRP, d_CRP, Sw_p, Sw_g, Sx_t, Sx_r, Sx_p, &
     Sx_g, Sw_s, var)

! According to the Lambertian SU forward model,
!    R = R_0v + \frac{ T_dv * Rs }{ 1 - R_dd * Rs }.
! Thus,
!    Rs = \frac{ R - R_0v }{ T_dv + R_dd * (R - R_0v) }
!       = s * [p + D * (g - p) + g * (1-g) * s / (1 - (1-g) * s)]
! So,
!    Rs [1 - (1-g) * s] = s * [(1 - (1-g) * s)(p + D * (g - p)) + g * (1-g) * s]
! Factorising in s,
!    0 = s^2 (g - p)(1 - D)(1 - g) + s * [p * (1-D) + Rs * (1-g) + D * g] - Rs
! Solving the quadratic, noting that D, g, and Rs fall in the range [0,1] and we
! must satisfy s>0
!    s = [-B + sqrt(B^2 + 4A * Rs)] / 2A    if g > p
!      = Rs / (g + Rs * (1-g))              if g = p
!      = [B +- sqrt(B^2 - 4|A| * Rs) / 2|A| if g < p
! where
!    A = (g - p)(1 - D)(1 - g)
!    B = p * (1-D) + Rs * (1-g) + D * g
! This routine evaluates these equations and their derivatives. See p.679 of
! ACP's labbook for more details.

   use FM_Routines_m, only: IR_0v, IT_dv, IR_dd, IT_00, IT_0d

   implicit none

   real, intent(in)  :: Y         ! Observed TOA reflectance
   real, intent(in)  :: Sy        ! Variance in TOA reflectance
   real, intent(in)  :: CRP(:)    ! LUT-interpolated aerosol radiative properties
   real, intent(in)  :: d_CRP(:,:)! Derivatives of CRP wrt aot and aer
   real, intent(in)  :: Sw_p      ! Swansea view parameter
   real, intent(in)  :: Sw_g      ! Swansea gamma parameter
   real, intent(in)  :: Sx_t      ! A priori variance in tau
   real, intent(in)  :: Sx_r      ! A priori variance in re
   real, intent(in)  :: Sx_p      ! A priori variance in Swansea p
   real, intent(in)  :: Sx_g      ! A priori variance in Swansea gamma
   real, intent(out) :: Sw_s      ! Estimated Swansea wavelength parameter
   real, intent(out) :: var       ! Variance in effective surface reflectance

   real :: dif, sum, T_all, direct, minus_g, root
   real :: rho, rho_v, diffuse, diffuse_v, b, b_v, a, a_v
   real :: Drho_Dy, Drho_Dx(2), Dd_Dx(2), Db_Dd, Db_Dg, Da_Dp, Da_Dd, Da_Dg, Ds_Db, Ds_Da, Ds_Drho

   ! Calculate the effective surface reflectance
   dif = Y - CRP(IR_0v)
   sum = CRP(IT_dv) + CRP(IR_dd) * dif
   rho = dif / sum

   ! Calculate it's variance
   Drho_Dy = CRP(IT_dv) / (sum * sum)
   Drho_Dx = -(d_CRP(IR_0v,:) * CRP(IT_dv) + d_CRP(IT_dv,:) * dif + &
        d_CRP(IR_dd,:) * dif * dif) / (sum * sum)
   rho_v = sy * Drho_Dy * Drho_Dy + Sx_t * Drho_Dx(1) * Drho_Dx(1) + &
           Sx_r * Drho_Dx(2) * Drho_Dx(2)

   T_all = CRP(IT_00) + CRP(IT_0d)
   diffuse = CRP(IT_0d) / T_all
   direct  = 1.0 - diffuse
   Dd_Dx = (d_CRP(IT_0d,:) * direct - d_CRP(IT_00,:) * diffuse) / T_all
   diffuse_v = Sx_t * Dd_Dx(1) * Dd_Dx(1) + &
               Sx_r * Dd_Dx(2) * Dd_Dx(2)

   minus_g = 1.0 - Sw_g
   b = direct * Sw_p + minus_g * rho + Sw_g * diffuse
   Db_Dd = Sw_g - Sw_p
   Db_Dg = diffuse - rho
   b_v = Sx_p * direct * direct + rho_v * minus_g * minus_g + &
         diffuse_v * Db_Dd * Db_Dd + Sx_g * Db_Dg * Db_Dg

   ! Estimate s parameter
   if (abs(Sw_p - Sw_g) < 1e-4) then
      ! Solve linear equation
      Sw_s = rho / b
      var = (rho_v + b_v * Sw_s * Sw_s) / (b * b)
   else
      ! Solve quadratic equation
      a = (Sw_p - Sw_g) * direct * minus_g
      Da_Dp = direct * minus_g
      Da_Dd = minus_g * (Sw_g - Sw_p)
      Da_Dg = direct * (2.0 * Sw_g - 1.0 - Sw_p)
      a_v = Sx_p * Da_Dp * Da_Dp + diffuse_v * Da_Dd * Da_Dd + &
            Sx_g * Da_Dg * Da_Dg

      root = sqrt(b * b - 4.0 * a * rho)
      Sw_s = (b - root) / (2.0 * a)
      Ds_Db = (1.0 - b / root) / (2.0 * a)
      Ds_Da = (2.0 * a * rho / root - root - b) / (2.0 * a * a)
      Ds_Drho = 1.0 / root
      var = a_v * Ds_Da * Ds_Da + b_v * Ds_Db * Ds_Db + rho_v * Ds_Drho * Ds_Drho
   end if

end subroutine Effective_Rho_Calc
