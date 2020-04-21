!-------------------------------------------------------------------------------
! Name: orac_input.F90
!
! Purpose: F90 Module file which declares user defined variable type structures.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2012/02/03, MJ: Cleans out prototype code to prepare repository upload.
! 2012/03/06, CP: Modified to produce post processed files
! 2012/03/18, CP: Modified to add cloud flag
! 2012/06/20, CP: Modified to add albedo
! 2012/07/06, MJ: Extensively overhauls and restructures the code
! 2014/09/20, CP: Adds in extra channel variables
! 2014/09/29, CP: Adds in variable names for MODIS
! 2014/10/24, OS: Added variables cccot_pre, lusflag, cldtype, cloudmask, DEM,
!    and nisemask
! 2014/12/02, CP: Adds in cloud_albedo
! 2015/02/05, OS: Deactivated use of postproc_constants to force consistency
!    with common_constants; changed nint to lint; added variable phase_post
! 2015/07/16, GM: Major cleanup and add associated routines to module.
! 2015/09/07, GM: Add cldmask_uncertainty.
! 2015/10/22, GM: Add cloud albedo uncertainty.
! 2016/01/27, GM: Add cee and cee_uncertainty.
! 2016/01/28, GM: Add ctp and ctt corrected and corrected_uncertianty.
! 2016/03/02, AP: Homogenisation of I/O modules.
! 2016/04/28, AP: Add multiple views.
! 2016/06/06, SP: New variable for bayesian selection without huge memory usage.
! 2016/07/19, AP: Reduce rho and swansea_s to only contain terms that were
!    retrieved. This is indicated by the rho|ss_terms array (and Nrho|Nss).
! 2017/01/09, CP: ML additions.
! 2017/06/22, OS: Added phase variables.
! 2017/07/05, AP: Add channels_used, variables_retrieved. New QC.
! 2018/06/08, SP: Add satellite azimuth angle to output.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module orac_input_m

   use orac_indexing_m

   implicit none

   type, extends(common_indices_t) :: input_indices_t
      character(len=7) :: LUTClass

      integer, pointer :: loop_to_main_index(:)
      integer, pointer :: ysolar_loop_to_main_index(:)
      integer, pointer :: ythermal_loop_to_main_index(:)
      integer, pointer :: view_loop_to_main_index(:)
      integer, pointer :: rho_loop_to_rho_main_index(:)
      integer, pointer :: ss_loop_to_ss_main_index(:)
      integer, pointer :: alb_loop_to_alb_main_index(:)
      integer, pointer :: cee_loop_to_cee_main_index(:)
      logical          :: read_optional_channel_field(MaxNumMeas) = .false.
      logical          :: read_optional_view_field(MaxNumViews)   = .false.
!     logical, pointer :: best_infile(:,:)
   end type input_indices_t


   type input_data_primary_t
      real(sreal),   pointer :: aot550(:,:)
      real(sreal),   pointer :: aot550_uncertainty(:,:)
      real(sreal),   pointer :: aot870(:,:)
      real(sreal),   pointer :: aot870_uncertainty(:,:)
      real(sreal),   pointer :: aer(:,:)
      real(sreal),   pointer :: aer_uncertainty(:,:)

      real(sreal),   pointer :: rho(:,:,:)
      real(sreal),   pointer :: rho_uncertainty(:,:,:)

      real(sreal),   pointer :: swansea_s(:,:,:)
      real(sreal),   pointer :: swansea_s_uncertainty(:,:,:)
      real(sreal),   pointer :: swansea_p(:,:,:)
      real(sreal),   pointer :: swansea_p_uncertainty(:,:,:)
      real(sreal),   pointer :: diffuse_frac(:,:,:)
      real(sreal),   pointer :: diffuse_frac_uncertainty(:,:,:)

      real(sreal),   pointer :: cot(:,:)
      real(sreal),   pointer :: cot_uncertainty(:,:)
      real(sreal),   pointer :: cer(:,:)
      real(sreal),   pointer :: cer_uncertainty(:,:)
      real(sreal),   pointer :: ctp(:,:)
      real(sreal),   pointer :: ctp_uncertainty(:,:)
      real(sreal),   pointer :: ctp_corrected(:,:)
      real(sreal),   pointer :: ctp_corrected_uncertainty(:,:)
      real(sreal),   pointer :: cc_total(:,:)
      real(sreal),   pointer :: cc_total_uncertainty(:,:)
      real(sreal),   pointer :: stemp(:,:)
      real(sreal),   pointer :: stemp_uncertainty(:,:)
      real(sreal),   pointer :: cth(:,:)
      real(sreal),   pointer :: cth_uncertainty(:,:)
      real(sreal),   pointer :: cth_corrected(:,:)
      real(sreal),   pointer :: cth_corrected_uncertainty(:,:)
      real(sreal),   pointer :: ctt(:,:)
      real(sreal),   pointer :: ctt_uncertainty(:,:)
      real(sreal),   pointer :: ctt_corrected(:,:)
      real(sreal),   pointer :: ctt_corrected_uncertainty(:,:)
      real(sreal),   pointer :: cwp(:,:)
      real(sreal),   pointer :: cwp_uncertainty(:,:)

      real(sreal),   pointer :: cot2(:,:)
      real(sreal),   pointer :: cot2_uncertainty(:,:)
      real(sreal),   pointer :: cer2(:,:)
      real(sreal),   pointer :: cer2_uncertainty(:,:)
      real(sreal),   pointer :: ctp2(:,:)
      real(sreal),   pointer :: ctp2_uncertainty(:,:)
      real(sreal),   pointer :: cth2(:,:)
      real(sreal),   pointer :: cth2_uncertainty(:,:)
      real(sreal),   pointer :: ctt2(:,:)
      real(sreal),   pointer :: ctt2_uncertainty(:,:)
      real(sreal),   pointer :: cwp2(:,:)
      real(sreal),   pointer :: cwp2_uncertainty(:,:)

      real(sreal),   pointer :: cloud_albedo(:,:,:)
      real(sreal),   pointer :: cloud_albedo_uncertainty(:,:,:)
      real(sreal),   pointer :: cee(:,:,:)
      real(sreal),   pointer :: cee_uncertainty(:,:,:)
      real(sreal),   pointer :: cccot_pre(:,:,:)

      real(dreal),   pointer :: time(:,:)
      real(sreal),   pointer :: lat(:,:)
      real(sreal),   pointer :: lon(:,:)
      real(sreal),   pointer :: sol_zen(:,:,:)
      real(sreal),   pointer :: sat_zen(:,:,:)
      real(sreal),   pointer :: rel_azi(:,:,:)
      real(sreal),   pointer :: sat_azi(:,:,:)

      integer(byte), pointer :: niter(:,:)
      real(sreal),   pointer :: costja(:,:)
      real(sreal),   pointer :: costjm(:,:)
      integer(lint), pointer :: qcflag(:,:)
      integer(dint), pointer :: channels_used(:,:)
      integer(dint), pointer :: variables_retrieved(:,:)
      character(len=attribute_length_long) :: qc_flag_masks
      character(len=attribute_length_long) :: qc_flag_meanings
      character(len=attribute_length_long) :: ch_flag_masks
      character(len=attribute_length_long) :: ch_flag_meanings
      character(len=attribute_length_long) :: vr_flag_masks
      character(len=attribute_length_long) :: vr_flag_meanings

      integer(byte), pointer :: lsflag(:,:)
      integer(byte), pointer :: lusflag(:,:)
      integer(sint), pointer :: dem(:,:)

      integer(byte), pointer :: illum(:,:)

      integer(byte), pointer :: cldtype(:,:,:)
      integer(byte), pointer :: cldmask(:,:,:)
      real(sreal),   pointer :: cldmask_uncertainty(:,:,:)

      integer(byte), pointer :: ann_phase(:,:,:)
      real(sreal),   pointer :: ann_phase_uncertainty(:,:,:)
      real(sreal),   pointer :: cphcot(:,:,:)

      integer(byte), pointer :: phase(:,:)
      integer(byte), pointer :: phase_pavolonis(:,:)
   end type input_data_primary_t


   type input_data_secondary_t
      real(sreal), pointer :: aot550_ap(:,:)
      real(sreal), pointer :: aot550_fg(:,:)
      real(sreal), pointer :: aer_ap(:,:)
      real(sreal), pointer :: aer_fg(:,:)

      real(sreal), pointer :: rho_ap(:,:,:)
      real(sreal), pointer :: rho_fg(:,:,:)

      real(sreal), pointer :: swansea_s_ap(:,:,:)
      real(sreal), pointer :: swansea_s_fg(:,:,:)
      real(sreal), pointer :: swansea_p_ap(:,:,:)
      real(sreal), pointer :: swansea_p_fg(:,:,:)

      real(sreal), pointer :: cot_ap(:,:)
      real(sreal), pointer :: cot_fg(:,:)
      real(sreal), pointer :: cer_ap(:,:)
      real(sreal), pointer :: cer_fg(:,:)
      real(sreal), pointer :: ctp_ap(:,:)
      real(sreal), pointer :: ctp_fg(:,:)
      real(sreal), pointer :: stemp_fg(:,:)
      real(sreal), pointer :: stemp_ap(:,:)
      real(sreal), pointer :: albedo(:,:,:)

      real(sreal), pointer :: cot2_ap(:,:)
      real(sreal), pointer :: cot2_fg(:,:)
      real(sreal), pointer :: cer2_ap(:,:)
      real(sreal), pointer :: cer2_fg(:,:)
      real(sreal), pointer :: ctp2_ap(:,:)
      real(sreal), pointer :: ctp2_fg(:,:)

      real(sreal), pointer :: channels(:,:,:)
      real(sreal), pointer :: y0(:,:,:)
      real(sreal), pointer :: residuals(:,:,:)

      real(sreal), pointer :: ds(:,:)
   end type input_data_secondary_t

contains

subroutine determine_channel_indexing(fname, indexing, verbose)

   use orac_ncdf_m

   implicit none

   character(len=*),      intent(in)    :: fname
   type(input_indices_t), intent(inout) :: indexing
   logical,               intent(in)    :: verbose

   integer       :: ncid, i_ch, i0, i1, ierr
   integer       :: do_flags
   integer(byte) :: rho_flags(indexing%Ny)

   call ncdf_open(ncid, fname, 'determine_channel_indexing()')

   ! Read attributes
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'LUT_class', indexing%LUTClass)
   if (ierr /= NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions(), ', trim(nf90_strerror(ierr)), &
           ', name: LUT_class'
      stop error_stop_code
   end if
   ierr = nf90_get_att(ncid, NF90_GLOBAL, 'do_flags', do_flags)
   if (ierr /= NF90_NOERR) then
      write(*,*) 'ERROR: read_input_dimensions(), ', trim(nf90_strerror(ierr)), &
           ', name: do_flags'
      stop error_stop_code
   end if

   call set_common_file_flags_from_bitmask(do_flags, indexing%flags)

   ! Read channel indexing information
   allocate(indexing%Y_Id(   indexing%Ny))
   allocate(indexing%View_Id(indexing%NViews))
   allocate(indexing%Ch_Is(  indexing%Ny))
   call ncdf_read_array(ncid, "y_id",    indexing%Y_Id,    verbose)
   call ncdf_read_array(ncid, "view_id", indexing%View_Id, verbose)
   call ncdf_read_array(ncid, "ch_is",   indexing%Ch_Is,   verbose)
   call ncdf_read_array(ncid, "rho_flags", rho_flags, verbose)

   call ncdf_close(ncid, 'determine_channel_indexing()')

   ! Allocate and form indexing arrays
   i0 = 0
   i1 = 0
   do i_ch = 1, indexing%Ny
      if (btest(indexing%Ch_Is(i_ch), SolarBit)) i0 = i0 + 1
      if (btest(indexing%Ch_Is(i_ch), ThermalBit)) i1 = i1 + 1
   end do
   indexing%NSolar = i0
   indexing%NThermal = i1
   allocate(indexing%YSolar(indexing%NSolar))
   allocate(indexing%YThermal(indexing%NThermal))

   i0 = 0
   i1 = 0
   do i_ch = 1, indexing%Ny
      if (btest(indexing%Ch_Is(i_ch), SolarBit)) then
         i0 = i0 + 1
         indexing%YSolar(i0) = i_ch
      end if
      if (btest(indexing%Ch_Is(i_ch), ThermalBit)) then
         i1 = i1 + 1
         indexing%YThermal(i1) = i_ch
      end if
   end do

   ! Allocate and form rho_terms array
   allocate(indexing%rho_terms(indexing%NSolar, MaxRho_XX))
   allocate(indexing%ss_terms(indexing%NSolar))
   allocate(indexing%alb_terms(indexing%NSolar))
   allocate(indexing%cee_terms(indexing%NThermal))
   call set_terms_from_bitmask(rho_flags, indexing%common_indices_t)

end subroutine determine_channel_indexing


subroutine cross_reference_indexing(n, loop_ind, main_ind)

   implicit none

   integer,               intent(in)    :: n
   type(input_indices_t), intent(inout) :: loop_ind(:)
   type(input_indices_t), intent(inout) :: main_ind

   integer :: i0, i1, i2, i_ch, i_file, j_ch, k_rho
   integer, dimension(MaxNumMeas) :: Y_Id, View_Id, Ch_Is, YSolar, YThermal

   ! Ensure files all have the same grid
   if (any(loop_ind(2:n)%Xdim /= loop_ind(1)%Xdim) .or. &
       any(loop_ind(2:n)%Ydim /= loop_ind(1)%Ydim)) then
      write(*,*) 'ERROR: Inconsistent horizontal grids between input files.'
      stop error_stop_code
   end if
   main_ind%Xdim = loop_ind(1)%Xdim
   main_ind%Ydim = loop_ind(1)%Ydim

   ! Activate all necessary output flags
   main_ind%flags%do_cloud           = any(loop_ind(1:n)%flags%do_cloud)
   main_ind%flags%do_cloud_layer_2   = any(loop_ind(1:n)%flags%do_cloud_layer_2)
   main_ind%flags%do_aerosol         = any(loop_ind(1:n)%flags%do_aerosol)
   main_ind%flags%do_rho             = any(loop_ind(1:n)%flags%do_rho)
   main_ind%flags%do_swansea         = any(loop_ind(1:n)%flags%do_swansea)
   main_ind%flags%do_indexing        = any(loop_ind(1:n)%flags%do_indexing)
   main_ind%flags%do_phase_pavolonis = &
                                  any(loop_ind(1:n)%flags%do_phase_pavolonis)
   main_ind%flags%do_cldmask         = any(loop_ind(1:n)%flags%do_cldmask)
   main_ind%flags%do_cldmask_uncertainty = &
                                  any(loop_ind(1:n)%flags%do_cldmask_uncertainty)
   main_ind%flags%do_phase           = any(loop_ind(1:n)%flags%do_phase)
   main_ind%flags%do_covariance      = any(loop_ind(1:n)%flags%do_covariance)
   main_ind%flags%do_ann_phase  = any(loop_ind(1:n)%flags%do_ann_phase)
   main_ind%flags%do_ann_phase_uncertainty = &
                                  any(loop_ind(1:n)%flags%do_ann_phase_uncertainty)

   ! Identify all available channels
   i0 = 0
   i1 = 0
   i2 = 0
   do i_ch = 1, MaxNumMeas
      file_loop: do i_file = 1, n
         do j_ch = 1, loop_ind(i_file)%Ny
            if (loop_ind(i_file)%Y_Id(j_ch) == i_ch) then
               i0 = i0 + 1
               Y_Id(i0) = i_ch
               Ch_Is(i0) = loop_ind(i_file)%Ch_Is(j_ch)
               loop_ind(i_file)%read_optional_channel_field(j_ch) = .true.
               if (btest(Ch_Is(i0), SolarBit)) then
                  i1 = i1 + 1
                  YSolar(i1) = i0
               end if
               if (btest(Ch_Is(i0), ThermalBit)) then
                  i2 = i2 + 1
                  YThermal(i2) = i0
               end if
               exit file_loop
            end if
         end do
      end do file_loop
   end do

   ! Allocate main channel indexing arrays
   main_ind%Ny       = i0
   main_ind%NSolar   = i1
   main_ind%NThermal = i2
   allocate(main_ind%Y_Id(main_ind%Ny))
   allocate(main_ind%Ch_Is(main_ind%Ny))
   allocate(main_ind%YSolar(main_ind%NSolar))
   allocate(main_ind%YThermal(main_ind%NThermal))
   main_ind%Y_Id     = Y_Id(1:i0)
   main_ind%Ch_Is    = Ch_Is(1:i0)
   main_ind%YSolar   = YSolar(1:i1)
   main_ind%YThermal = YThermal(1:i2)

   ! Allocate channel cross-referencing arrays
   do i_file = 1, n
      allocate(loop_ind(i_file)%loop_to_main_index(loop_ind(i_file)%Ny))
      do j_ch = 1, loop_ind(i_file)%Ny
         do i_ch = 1, main_ind%Ny
            if (loop_ind(i_file)%Y_Id(j_ch) == main_ind%Y_Id(i_ch)) then
               loop_ind(i_file)%loop_to_main_index(j_ch) = i_ch
               exit
            end if
         end do
      end do

      allocate(loop_ind(i_file)%ysolar_loop_to_main_index( &
           loop_ind(i_file)%NSolar))
      do j_ch = 1, loop_ind(i_file)%NSolar
         do i_ch = 1, main_ind%NSolar
            if (loop_ind(i_file)%Y_Id(loop_ind(i_file)%YSolar(j_ch)) == &
                main_ind%Y_Id(main_ind%YSolar(i_ch))) then
               loop_ind(i_file)%ysolar_loop_to_main_index(j_ch) = i_ch
            end if
         end do
      end do

      allocate(loop_ind(i_file)%ythermal_loop_to_main_index( &
           loop_ind(i_file)%NThermal))
      do j_ch = 1, loop_ind(i_file)%NThermal
         do i_ch = 1, main_ind%NThermal
            if (loop_ind(i_file)%Y_Id(loop_ind(i_file)%YThermal(j_ch)) == &
                main_ind%Y_Id(main_ind%YThermal(i_ch))) then
               loop_ind(i_file)%ythermal_loop_to_main_index(j_ch) = i_ch
            end if
         end do
      end do
   end do

   ! Identify all available views
   i0 = 0
   do i_ch = 1, MaxNumViews
      view_loop: do i_file = 1, n
         do j_ch = 1, loop_ind(i_file)%NViews
            if (loop_ind(i_file)%View_Id(j_ch) == i_ch) then
               i0 = i0 + 1
               View_Id(i0) = i_ch
               loop_ind(i_file)%read_optional_view_field(j_ch) = .true.
               exit view_loop
            end if
         end do
      end do view_loop
   end do

   ! Allocate main view indexing array
   main_ind%NViews = i0
   allocate(main_ind%View_Id(main_ind%NViews))
   main_ind%View_Id = View_Id(1:i0)

   ! Allocate view cross-referencing array
   do i_file = 1, n
      allocate(loop_ind(i_file)%view_loop_to_main_index( &
           loop_ind(i_file)%NViews))
      do j_ch = 1, loop_ind(i_file)%NViews
         do i_ch = 1, main_ind%NViews
            if (loop_ind(i_file)%View_Id(j_ch) == main_ind%View_Id(i_ch)) then
               loop_ind(i_file)%view_loop_to_main_index(j_ch) = i_ch
            end if
         end do
      end do
   end do

   ! Allocate state vector terms
   main_ind%Nx = maxval(loop_ind%Nx)
   if (main_ind%flags%do_rho) then
      ! Identify which rho terms are retrieved in any input
      allocate(main_ind%rho_terms(main_ind%NSolar, MaxRho_XX))
      main_ind%rho_terms = .false.
      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_rho) then
            do j_ch = 1, loop_ind(i_file)%NSolar
               i_ch = loop_ind(i_file)%ysolar_loop_to_main_index(j_ch)
               main_ind%rho_terms(i_ch,:) = main_ind%rho_terms(i_ch,:) .or. &
                    loop_ind(i_file)%rho_terms(j_ch,:)
            end do
         end if
      end do
      main_ind%Nrho = count(main_ind%rho_terms)

      ! Identify where each file's rho terms are in the main structure
      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_rho) then
            allocate(loop_ind(i_file)%rho_loop_to_rho_main_index( &
                 loop_ind(i_file)%Nrho))

            i0 = 0
            i1 = 0
            do i_ch = 1, main_ind%NSolar
               ! Search for the inverse of ysolar_loop_to_main_index
               ch_search_rho: do j_ch = 1, loop_ind(i_file)%NSolar
                  if (loop_ind(i_file)%Y_Id(loop_ind(i_file)%YSolar(j_ch)) == &
                       main_ind%Y_Id(main_ind%YSolar(i_ch))) exit ch_search_rho
               end do ch_search_rho

               do k_rho = 1, MaxRho_XX
                  if (main_ind%rho_terms(i_ch,k_rho)) i0 = i0 + 1

                  if (j_ch <= loop_ind(i_file)%NSolar) then
                     if (loop_ind(i_file)%rho_terms(j_ch,k_rho)) then
                        i1 = i1 + 1
                        loop_ind(i_file)%rho_loop_to_rho_main_index(i1) = i0
                     end if
                  end if
               end do
            end do
         end if
      end do
   end if

   ! As above, but for ss_terms
   if (main_ind%flags%do_swansea) then
      allocate(main_ind%ss_terms(main_ind%NSolar))
      main_ind%ss_terms = .false.
      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_swansea) then
            do j_ch = 1, loop_ind(i_file)%NSolar
               i_ch = loop_ind(i_file)%ysolar_loop_to_main_index(j_ch)
               main_ind%ss_terms(i_ch) = main_ind%ss_terms(i_ch) .or. &
                    loop_ind(i_file)%ss_terms(j_ch)
            end do
         end if
      end do
      main_ind%Nss = count(main_ind%ss_terms)

      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_swansea) then
            allocate(loop_ind(i_file)%ss_loop_to_ss_main_index( &
                 loop_ind(i_file)%Nss))

            i0 = 0
            i1 = 0
            do i_ch = 1, main_ind%NSolar
               ch_search_ss: do j_ch = 1, loop_ind(i_file)%NSolar
                  if (loop_ind(i_file)%Y_Id(loop_ind(i_file)%YSolar(j_ch)) == &
                       main_ind%Y_Id(main_ind%YSolar(i_ch))) exit ch_search_ss
               end do ch_search_ss

               if (main_ind%ss_terms(i_ch)) i0 = i0 + 1

               if (j_ch <= loop_ind(i_file)%NSolar) then
                  if (loop_ind(i_file)%ss_terms(j_ch)) then
                     i1 = i1 + 1
                     loop_ind(i_file)%ss_loop_to_ss_main_index(i1) = i0
                  end if
               end if
            end do
         end if
      end do
   end if

   ! As above, but for alb_terms
   if (main_ind%flags%do_cloud) then
      allocate(main_ind%alb_terms(main_ind%NSolar))
      main_ind%alb_terms = .false.
      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_cloud) then
            do j_ch = 1, loop_ind(i_file)%NSolar
               i_ch = loop_ind(i_file)%ysolar_loop_to_main_index(j_ch)
               main_ind%alb_terms(i_ch) = main_ind%alb_terms(i_ch) .or. &
                    loop_ind(i_file)%alb_terms(j_ch)
            end do
         end if
      end do
      main_ind%Nalb = count(main_ind%alb_terms)

      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_cloud) then
            allocate(loop_ind(i_file)%alb_loop_to_alb_main_index( &
                 loop_ind(i_file)%Nalb))

            i0 = 0
            i1 = 0
            do i_ch = 1, main_ind%NSolar
               ch_search_alb: do j_ch = 1, loop_ind(i_file)%NSolar
                  if (loop_ind(i_file)%Y_Id(loop_ind(i_file)%YSolar(j_ch)) == &
                       main_ind%Y_Id(main_ind%YSolar(i_ch))) exit ch_search_alb
               end do ch_search_alb

               if (main_ind%alb_terms(i_ch)) i0 = i0 + 1

               if (j_ch <= loop_ind(i_file)%NSolar) then
                  if (loop_ind(i_file)%alb_terms(j_ch)) then
                     i1 = i1 + 1
                     loop_ind(i_file)%alb_loop_to_alb_main_index(i1) = i0
                  end if
               end if
            end do
         end if
      end do

      ! And then the cee_terms
      allocate(main_ind%cee_terms(main_ind%NThermal))
      main_ind%cee_terms = .false.
      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_cloud) then
            do j_ch = 1, loop_ind(i_file)%NThermal
               i_ch = loop_ind(i_file)%ythermal_loop_to_main_index(j_ch)
               main_ind%cee_terms(i_ch) = main_ind%cee_terms(i_ch) .or. &
                    loop_ind(i_file)%cee_terms(j_ch)
            end do
         end if
      end do
      main_ind%Ncee = count(main_ind%cee_terms)

      do i_file = 1, n
         if (loop_ind(i_file)%flags%do_cloud) then
            allocate(loop_ind(i_file)%cee_loop_to_cee_main_index( &
                 loop_ind(i_file)%Ncee))

            i0 = 0
            i1 = 0
            do i_ch = 1, main_ind%NThermal
               ch_search_cee: do j_ch = 1, loop_ind(i_file)%NThermal
                  if (loop_ind(i_file)%Y_Id(loop_ind(i_file)%YThermal(j_ch)) == &
                       main_ind%Y_Id(main_ind%YThermal(i_ch))) exit ch_search_cee
               end do ch_search_cee

               if (main_ind%cee_terms(i_ch)) i0 = i0 + 1

               if (j_ch <= loop_ind(i_file)%NThermal) then
                  if (loop_ind(i_file)%cee_terms(j_ch)) then
                     i1 = i1 + 1
                     loop_ind(i_file)%cee_loop_to_cee_main_index(i1) = i0
                  end if
               end if
            end do
         end if
      end do
   end if

end subroutine cross_reference_indexing


subroutine nullify_indexing(indexing)

   implicit none

   type(input_indices_t), intent(inout) :: indexing

   call nullify_common_indices(indexing%common_indices_t)

   nullify(indexing%loop_to_main_index)
   nullify(indexing%ysolar_loop_to_main_index)
   nullify(indexing%ythermal_loop_to_main_index)
   nullify(indexing%view_loop_to_main_index)
   nullify(indexing%rho_loop_to_rho_main_index)
   nullify(indexing%ss_loop_to_ss_main_index)
   nullify(indexing%alb_loop_to_alb_main_index)
   nullify(indexing%cee_loop_to_cee_main_index)
!  nullify(indexing%best_infile)

end subroutine nullify_indexing


subroutine dealloc_input_indices(indexing)

   implicit none

   type(input_indices_t), intent(inout) :: indexing

   call dealloc_common_indices(indexing%common_indices_t)

   if (associated(indexing%loop_to_main_index)) &
      deallocate(indexing%loop_to_main_index)
   if (associated(indexing%ysolar_loop_to_main_index)) &
      deallocate(indexing%ysolar_loop_to_main_index)
   if (associated(indexing%ythermal_loop_to_main_index)) &
      deallocate(indexing%ythermal_loop_to_main_index)
   if (associated(indexing%view_loop_to_main_index)) &
      deallocate(indexing%view_loop_to_main_index)
   if (associated(indexing%rho_loop_to_rho_main_index)) &
      deallocate(indexing%rho_loop_to_rho_main_index)
   if (associated(indexing%ss_loop_to_ss_main_index)) &
      deallocate(indexing%ss_loop_to_ss_main_index)
   if (associated(indexing%alb_loop_to_alb_main_index)) &
      deallocate(indexing%alb_loop_to_alb_main_index)
   if (associated(indexing%cee_loop_to_cee_main_index)) &
      deallocate(indexing%cee_loop_to_cee_main_index)
!  if (associated(indexing%best_infile)) &
!     deallocate(indexing%best_inffile)

end subroutine dealloc_input_indices


#include "alloc_input_data.F90"
#include "dealloc_input_data.F90"

#include "read_input_dimensions.F90"

#include "read_input_primary.F90"
#include "read_input_secondary.F90"

end module orac_input_m
