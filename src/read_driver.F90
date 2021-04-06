!-------------------------------------------------------------------------------
! Name: read_driver.F90
!
! Purpose:
! Stores values required by driver file. This code is intended to replace
! the idl write_idriver.pro
!
! Description and Algorithm details:
! Reads the values from the "driver" file used to set run-time options into
! the Ctrl structure. Settings beyond the typical can be overridden in the
! driver using lines such as,
!    Ctrl%Run_ID = ABCD
! The variable to change is identified before an = sign (with structure
! references expressed by % or .) and its value is after the = sign. #
! denotes a comment. Arrays should be delimited with commas, then semicolons
! though whitespace, then commas is acceptable.
!
! Arguments:
! Name        Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl        struct  Out         Control struct defined in Ctrl_m
! global_atts struct  Both        Attributes for NCDF files
! source_atts struct  Both        Description of file inputs for NCDF files
!
! History:
! 2012/05/15, CP: created original file to replace ReadDriver
! 2012/06/08, CP: fixed memory leaks
! 2012/07/13, MJ: implements option to read drifile path from command line
! 2012/07/13, CP: changed ref_solar_sea nd ref_solar_land to reals rather than
!    integers.
! 2012/07/26, MJ: makes some small changes, paths passed to ORAC via driver
!    file must not have trailing forward slashes ("/"). Those are explicitly
!    added in this routine where necessary.
! 2012/07/29, CP: fixed bug in twilight/night state variable numbering
! 2012/08/10, CP: modified state parameter indives for night
! 2012/08/22, MJ: makes adaptions to read netcdf files
! 2012/09/15, CP: removed double allocation of viewidx
! 2012/10/01, CP: changed active variables at night to be ctp fraction and
!    surface temperature, changed how first guess of CTP calculated at night
!    i.e matching temperature profile changed a priori errors of state vector
! 2012/10/01, CP: added in default sx category as was not being reset after
!    each pixel was processed
! 2012/10/01, MJ: changed definition of AVHRR SW variables
! 2012/11/22, CP: fixed bug in index_ir AATSR definition
! 2013/03/18, CP: modified upper limits of ice and water effective radius
! 2013/03/18, CP: added in more comments for clarity
! 2013/06/27, MJ: implemented reading of path to driver file either from
!    environment variable or passed from outside
! 2013/10/02, CP: added comments for GT added aerosol classes for Bayesian
!    cloud id
! 2013/11/14, MJ: rewrote most parts referring to setting and reading channel
!    indices. Added reading of config file different driver file necessary now.
! 2013/11/14, MJ: changes lower and upper limits for ctp.
! 2013/11/18, MJ: Several additional changes, introduces ysolar_msi and
!    ythermal_msi
! 2013/11/25, MJ: initialized previously uninitialised Ctrl%Run_ID
! 2014/01/12, GM: Small fixes involving Ctrl%Ind%NAvail
! 2014/01/15, GM: No need for Ctrl%defaultSx any more.
! 2014/01/15, GM: Added the ability to read the driver file contents from
!    standard input indicated by drifile .eq. '-'.
! 2014/01/16, GM: Use Selm* constants to set FG and AP instead of numbers.
! 2014/01/16, GM: Cleaned up code.
! 2014/01/31, MJ: Adds code for default surface reflection for avhrr
!    (=modis for the time being)
! 2014/06/04, MJ: introduced "WRAPPER" for c-preprocessor and associated
!    variables
! 2014/09/17, GM: Use the DiFlag* constants instead of integer values.
! 2014/12/01, CP: added in global and source attribute read capability
! 2014/12/01, OS: increased maximum acceptable retrieval cost from 10 to 100,
!    as otherwise ~30% of converged pixels are lost in l2tol3 processing
! 2014/12/17, AP: Converted read statements to parse_driver statements.
!    Permit optional overrides of default behaviour from driver.
! 2014/12/19, AP: Tidying. Cleaning the management of channel indexing.
! 2014/12/29, GM: Fixed a bug in the channel indexing changes above.
! 2015/01/15, AP: Adding Ctrl%Ind%Ch_Is. Revised setting of Ctrl%RS%B.
! 2015/01/19, GM: Added error handling for parsing the driver file.
! 2015/01/20, GM: Bug fix: any() was being used on uninitialized data.
! 2015/01/28, AP: Use consistent FG and AP. Turn off retrieval of phase.
!    Increase a priori uncertainty on CTP to equal that of other variables.
! 2015/01/30, AP: Allow warm start coordinates to be specified.
! 2015/02/04, GM: Add sabotage_inputs flag and retrieval channel requirements
!    arrays.
! 2015/02/04, GM: Add initialization of ReChans array.
! 2015/02/04, OS: drifile is passed as call argument for WRAPPER
! 2015/02/24, GM: Some small fixes to driver file error handling.
! 2015/03/10, GM: Added Ctrl%RS%use_full_brdf as a driver option.
! 2015/03/11, GM: Added Ctrl%ReChans as a driver option.
! 2015/03/11, GM: Added an error check for unrecognised instruments.
! 2015/05/25, GM: Get rid of filename Diag and flags Diagl. Neither was being
!    used and have been rotting.
! 2015/06/02, AP: Additions for aerosol code.
! 2015/07/22, AP: Use parse_user_text for arguments which set variables that
!    are set using ECP_Constants parameters within the code.
! 2015/07/29, GM: Removed unused ash CloudClasses.
! 2015/07/29, GM: Added missing initializations of Ctrl%RS%B for MODIS.
! 2015/07/31, GM: Changed a priori values and uncertainties for EYJ.
! 2015/08/10, AP: Consolidate X_Dy, X_Tw, X_Ni into one variable.
! 2015/08/18, AP: Added optional argument specifying the ID# for each channel
!    such that CHXX style indexing can be used.
! 2015/08/19, AP: Extensive tidying using the switch function. Tidying of Ctrl.
!    Eliminated XI as not meaningful.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2015/08/08, CP: Added in ATSR-2 capability
! 2015/11/17, OS: Added prob_opaque_ice_type to CldIce types to process
! 2015/11/18, GM: Add setting of Ctrl%Ind%Y_Id_legacy.
! 2015/11/27, CP: Modified setting so clear pixels are processed when not cloud
!    only.
! 2015/12/17, OS: MaxSolZen decreased from 80. to 75. to remove low quality
!    microphysical retrievals.
! 2016/01/27, GM: Pass through the driver file twice.  Once for mandatory and
!    code path controlling arguments.  Then a second time for optional arguments
!    dependent on the path controlling arguments since they control default
!    values.
! 2016/01/27, GM: Changes for the new night cloud retrieval.
! 2016/04/12, SP: Updated to support Himawari/AHI.
! 2016/05/17, SP: Updated to support SuomiNPP/VIIRS.
! 2016/05/18, SP: Added sanity check to ensure sensor is supported. Prevents
!    segfault if Ctrl INDEXING CHANNELS if-statements don't succeed
! 2016/05/31, GT: Added Ctrl%process_aerosol_only flag
! 2016/06/06, GM: Set Ctrl%RS%solar_factor and Ctrl%get_T_dv_from_T_0d based on
!    Ctrl%i_equation_form.  Change default value of Ctrl%i_equation_form to 3.
! 2016/06/05, SP: Updated to support Sentinel-3/SLSTR.
! 2016/07/19, AP: Reduce rho and swansea_s to only contain terms that were
!    retrieved. This is indicated by the rho|ss_terms array (and Nrho|Nss).
! 2016/07/20, WJ: Add check for instrument for setting max stemp value and set
!    max to 400K (instead of 320K) for cases other than ATSR2/AATSR.
! 2016/07/27, GM: Changes and additions for the multilayer retrieval.
! 2016/08/05, GM: Add optional second command line argument: A file to dump the
!    entire Ctrl structure as a driver file. If it is not given nothing is
!    dumped. If it is '-' the dump is to standard output. Anything else is the
!    name of a file to dump to. If this argument is given then execution
!    terminates after the dump.
! 2016/08/11, SP: Add logical flag for processing when using only 1 view from a
!                 multiangular sensor. Prevents post-processor problems.
! 2017/01/19, CP: Add in revised uncertainty for ML case.
! 2017/03/16, GT: Changes for single-view aerosol retrieval mode.
! 2017/06/21, OS: Deactivated dumpfile and new driver format option for WRAPPER
! 2017/07/12, AP: New QC.
! 2017/09/14, GM: In ML cases postfix both the upper and lower layer LUT class
!    names separated by an underscore.
! 2017/10/04, GM: Add the option to use ANN phase to select pixels to process.
! 2017/12/13, GT: Changed SLSTR instrument name string to SLSTR-Sentinel3a
!                 (in preparation for Sentinel-3b)
! 2017/12/14, MST: the lower limit of tskin was set to 250.K which was too high
!     in particular for Greenland and Antartica. Have set it to 200.0K.
!     Comparisons of lw fluxes to CERES confirm that this was necessary.
! 2018/09/30, SRP: Delete old driver read routines.
! 2019/08/14, SP: Add Fengyun4A support.
! 2021/04/06, AP: New LUT names.
!
! Bugs:
! None known.
!
! IMPORTANT NOTE:
! If a new type of LUT i.e aerosol is added then new default values will have
! to be added to this routine
!-------------------------------------------------------------------------------

module read_driver_m
   implicit none

   interface switch_app
      module procedure &
           switch_app_logic, switch_app_byte,  switch_app_sint, &
           switch_app_lint, switch_app_sreal, switch_app_dreal, switch_app_char
   end interface switch_app

   interface switch_cls
      module procedure &
           switch_cls_logic, switch_cls_byte,  switch_cls_sint, &
           switch_cls_lint, switch_cls_sreal, switch_cls_dreal, switch_cls_char
   end interface switch_cls

contains

#ifdef WRAPPER
subroutine Read_Driver(Ctrl, global_atts, source_atts, drifile)
#else
subroutine Read_Driver(Ctrl, global_atts, source_atts)
#endif

   use, intrinsic :: iso_fortran_env, only : input_unit, output_unit

   use constants_cloud_typing_pavolonis_m
   use Ctrl_m
   use global_attributes_m
   use ORAC_Constants_m
   use read_utils_m
   use source_attributes_m
   use read_ctrl_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),              intent(out)   :: Ctrl
   type(global_attributes_t), intent(inout) :: global_atts
   type(source_attributes_t), intent(inout) :: source_atts
#ifdef WRAPPER
   character(len=*),          intent(inout) :: drifile
#else
   character(len=FilenameLen)               :: drifile
#endif

   ! Local variables
   integer                            :: i, ii, i0, i1, i2, j
   integer                            :: ios
   integer                            :: dri_lun, dump_lun
   character(len=FilenameLen)         :: root_filename
   character(len=FilenameLen)         :: outname, line
   logical                            :: file_exists
   integer, allocatable, dimension(:) :: channel_ids_instr
   integer, allocatable, dimension(:) :: channel_sw_flag, channel_lw_flag
   real,    allocatable, dimension(:) :: channel_wvl
   integer, allocatable, dimension(:) :: channel_view
   integer                            :: Nx_Dy, Nx_Tw, Nx_Ni
   integer                            :: NXJ_Dy, NXJ_Tw, NXJ_Ni
   integer, dimension(MaxStateVar)    :: X_Dy, X_Tw, X_Ni
   integer, dimension(MaxStateVar)    :: XJ_Dy, XJ_Tw, XJ_Ni
   integer                            :: a     ! Short name for Ctrl%Approach
   integer                            :: c, c2 ! Short name for Ctrl%Class
   real                               :: wvl_threshold
   integer                            :: size
   character, allocatable             :: buffer(:)
   character(len=FilenameLen)         :: dumpfile


   call Nullify_Ctrl(Ctrl)


   ! Initialise some important variables before first driver file read
   Ctrl%verbose                = .true.
   Ctrl%Approach               = -1
   Ctrl%Class                  = -1
   Ctrl%Class2                 = -1
   Ctrl%use_ann_phase          = .true.
   Ctrl%do_new_night_retrieval = .true.
   Ctrl%do_CTX_correction      = .true.
   Ctrl%process_cloudy_only    = .true.
   Ctrl%process_aerosol_only   = .false.


   !----------------------------------------------------------------------------
   ! Locate the driver file
   !----------------------------------------------------------------------------
#ifndef WRAPPER
   if (command_argument_count() >= 1) then
      drifile = ''
      call get_command_argument(1, drifile)
   else
      call get_environment_variable("ORAC_TEXTIN", drifile)
   end if

   dumpfile = ''
   if (command_argument_count() == 2) then
      call get_command_argument(2, dumpfile)
   end if
#else
   dumpfile = ''
#endif

   ! If drifile is '-' read the file from standard input otherwise read drifile
   if (drifile == '-') then
      dri_lun = input_unit
   else
      ! Check drifile exists
      inquire(file=drifile, exist=file_exists)
      if (.not. file_exists) then
         write(*,*) 'ERROR: Read_Driver(): Driver file pointed to by ' // &
                    'ORAC_DRIVER does not exist: ', trim(drifile)
         stop DriverFileNotFound
      end if

      ! Open the driver file
      call find_lun(dri_lun)
      open(unit=dri_lun, file=drifile, iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: Read_Driver(): Unable to open driver file: ', &
                    trim(drifile)
         stop DriverFileOpenErr
      end if
   end if

   ! Identify version of driver file
   read(dri_lun, '(A)', iostat=ios) line
   if (ios /= 0) then
      write(*,*) 'ERROR: Read_Driver(): Unable to read driver file: ', &
                 trim(drifile)
      stop DriverFileOpenErr
   end if
   rewind dri_lun


   !----------------------------------------------------------------------------
   ! Read the driver file
   !----------------------------------------------------------------------------
   close(dri_lun)
   call read_ctrl(drifile, Ctrl)

   ! Set filenames
   root_filename   = trim(Ctrl%FID%Data_Dir)//'/'//trim(Ctrl%FID%Filename)
   Ctrl%FID%Config = trim(root_filename)//'.config.nc'
   Ctrl%FID%MSI    = trim(root_filename)//'.msi.nc'
   Ctrl%FID%LWRTM  = trim(root_filename)//'.lwrtm.nc'
   Ctrl%FID%SWRTM  = trim(root_filename)//'.swrtm.nc'
   Ctrl%FID%PRTM   = trim(root_filename)//'.prtm.nc'
   Ctrl%FID%LS     = trim(root_filename)//'.lsf.nc'
   Ctrl%FID%CF     = trim(root_filename)//'.clf.nc'
   Ctrl%FID%Geo    = trim(root_filename)//'.geo.nc'
   Ctrl%FID%Loc    = trim(root_filename)//'.loc.nc'
   Ctrl%FID%Alb    = trim(root_filename)//'.alb.nc'

   ! Read channel related info
   call read_config_file(Ctrl, channel_ids_instr, channel_sw_flag, &
     channel_lw_flag, channel_wvl, channel_view, global_atts, source_atts, &
     Ctrl%Ind%NAll)

   ! Read dimensions of preprocessing swath files
   call read_input_dimensions_msi(Ctrl%FID%MSI, Ctrl%FID%Geo, &
      Ctrl%Ind%Xmax, Ctrl%Ind%YMax, Ctrl%Ind%NViews, Ctrl%verbose)

   ! Determine the number of channels to be used.
   Ctrl%Ind%Ny       = count(Ctrl%Ind%channel_proc_flag == 1)
   Ctrl%Ind%NSolar   = count(channel_sw_flag == 1 .and. Ctrl%Ind%channel_proc_flag == 1)
   Ctrl%Ind%NThermal = count(channel_lw_flag == 1 .and. Ctrl%Ind%channel_proc_flag == 1)
   Ctrl%Ind%NMixed   = count(channel_sw_flag == 1 .and. channel_lw_flag == 1 &
                             .and. Ctrl%Ind%channel_proc_flag == 1)

   ! Produce channel indexing arrays
   allocate(Ctrl%Ind%ICh(Ctrl%Ind%Ny))
   allocate(Ctrl%Ind%Y_ID(Ctrl%Ind%Ny))
   allocate(Ctrl%Ind%Ch_Is(Ctrl%Ind%Ny))
   allocate(Ctrl%Ind%View_Id(Ctrl%Ind%Ny))
   Ctrl%Ind%Ch_Is = 0
   if (Ctrl%Ind%NSolar > 0)   allocate(Ctrl%Ind%YSolar(Ctrl%Ind%NSolar))
   if (Ctrl%Ind%NThermal > 0) allocate(Ctrl%Ind%YThermal(Ctrl%Ind%NThermal))
   if (Ctrl%Ind%NMixed > 0)   allocate(Ctrl%Ind%YMixed(Ctrl%Ind%NMixed))
   ii = 0
   i0 = 0
   i1 = 0
   i2 = 0
   do i = 1, Ctrl%Ind%NAvail
      ! Identify processing channels WITH RESPECT TO THE PREPROC FILE
      if (Ctrl%Ind%channel_proc_flag(i) == 1) then
         ii = ii+1
         Ctrl%Ind%ICh(ii) = i ! Fortran array index for channel
         Ctrl%Ind%Y_ID(ii) = channel_ids_instr(i) ! Instrument channel number
         Ctrl%Ind%View_Id(ii) = channel_view(i)   ! Channel view number

         ! Identify solar and thermal channels WITH RESPECT TO Ctrl%IND%ICH
         if (channel_sw_flag(i) == 1) then
            i0 = i0+1
            Ctrl%Ind%YSolar(i0) = ii
            Ctrl%Ind%Ch_Is(ii) = ibset(Ctrl%Ind%Ch_Is(ii), SolarBit)
         end if
         if (channel_lw_flag(i) == 1) then
            i1 = i1+1
            Ctrl%Ind%YThermal(i1) = ii
            Ctrl%Ind%Ch_Is(ii) = ibset(Ctrl%Ind%Ch_Is(ii), ThermalBit)
         end if
         if (channel_sw_flag(i) == 1 .and. channel_lw_flag(i) == 1) then
            i2 = i2+1
            Ctrl%Ind%YMixed(i2) = ii
!            Ctrl%Ind%Ch_Is(ii) = ibset(Ctrl%Ind%Ch_Is(ii), MixedBit)
         end if
      end if
   end do

   ! Check if all selected channels are from the same view
   if (all(Ctrl%Ind%View_Id .eq. Ctrl%Ind%View_Id(1))) then
      Ctrl%all_channels_same_view = .true.
   else
      Ctrl%all_channels_same_view = .false.
   end if


   ! Identify which channels are multiple views of the same wavelength
   allocate(Ctrl%Ind%WvlIdx(Ctrl%Ind%Ny))
   Ctrl%Ind%WvlIdx = 0
   ii = 0
   do i = 1, Ctrl%Ind%Ny
      if (Ctrl%Ind%WvlIdx(i) == 0) then
         ii = ii+1
         Ctrl%Ind%WvlIdx(i) = ii
         do j = i+1, Ctrl%Ind%Ny
            if (channel_wvl(Ctrl%Ind%ICh(i)) == channel_wvl(Ctrl%Ind%ICh(j))) &
                 Ctrl%Ind%WvlIdx(j) = ii
         end do
      end if
   end do
   Ctrl%Ind%NWvl = ii

   if (Ctrl%Approach == -1) then
      ! Approach not set, so deduce from LUTClass
      if (Ctrl%LUTClass(1:3) == 'WAT' .or. &
           Ctrl%LUTClass(1:12) == 'liquid-water') then
         Ctrl%Approach = AppCld1L
         Ctrl%Class    = ClsCldWat
      else if (Ctrl%LUTClass(1:3) == 'ICE' .or. &
           Ctrl%LUTClass(1:9) == 'water-ice') then
         Ctrl%Approach = AppCld1L
         Ctrl%Class    = ClsCldIce
      else if (Ctrl%LUTClass(1:3) == 'ASO' .or. &
           Ctrl%LUTClass(1:3) == 'EYJ' .or. &
           Ctrl%LUTClass(1:12) == 'volcanic-ash') then
         Ctrl%Approach = AppCld1L
         Ctrl%Class    = ClsAshEyj
      else if (Ctrl%LUTClass(1:1) == 'A' .or. &
           Ctrl%LUTClass(1:7) == 'aerosol') then
         Ctrl%Approach = AppAerOx
         Ctrl%Class    = ClsAerOx
      else
         write(*,*) 'ERROR: Read_Driver(): Cannot determine retrieval '// &
                    'approach from LUTClass. Please set Ctrl%Approach.'
         stop error_stop_code
      end if
   end if

   if (Ctrl%Class == -1) then
      ! Class not set, so deduce it from the LUTClass and/or Approach
      if (Ctrl%Approach == AppCld1L .and. (Ctrl%LUTClass(1:3) == 'WAT' .or. &
           Ctrl%LUTClass(1:12) == 'liquid-water')) then
         Ctrl%Class = ClsCldWat
      else if (Ctrl%Approach == AppCld1L .and. (Ctrl%LUTClass(1:3) == 'ICE' .or. &
           Ctrl%LUTClass(1:9) == 'water-ice')) then
         Ctrl%Class = ClsCldIce
      else if (Ctrl%Approach == AppCld1L .and. (Ctrl%LUTClass(1:3) == 'ASO' .or. &
           Ctrl%LUTClass(1:3) == 'EYJ' .or. &
           Ctrl%LUTClass(1:12) == 'volcanic-ash')) then
         Ctrl%Class = ClsAshEyj
      else if (Ctrl%Approach == AppCld2L) then
         Ctrl%Class  = ClsCldIce
         Ctrl%Class2 = ClsCldWat
      else if (Ctrl%Approach == AppAerOx) then
         Ctrl%Class = ClsAerOx
      else if (Ctrl%Approach == AppAerSw) then
         Ctrl%Class = ClsAerSw
      else if (Ctrl%Approach == AppAerO1) then
         Ctrl%Class = ClsAerOx
      else
         write(*,*) 'ERROR: Read_Driver(): Invalid Ctrl%Approach:', Ctrl%Approach
         stop error_stop_code
      end if
   end if

   Ctrl%LUTClassLayers = trim(Ctrl%LUTClass)
   if (Ctrl%Approach == AppCld2L) then
      Ctrl%LUTClassLayers = trim(Ctrl%LUTClassLayers)//'_'//trim(Ctrl%LUTClass2)
   end if


   ! Output filenames
   outname = trim(Ctrl%FID%Out_Dir)//'/'//trim(Ctrl%FID%Filename)// &
            trim(Ctrl%LUTClassLayers)
   Ctrl%FID%L2_primary   = trim(outname)//'.primary.nc'
   Ctrl%FID%L2_secondary = trim(outname)//'.secondary.nc'


   ! Use a short name for Ctrl%Approach
   a = Ctrl%Approach

   ! Use a short name for Ctrl%Class
   c  = Ctrl%Class
   if (Ctrl%Approach /= AppCld2L) then
      c2 = Ctrl%Class
   else
      c2 = Ctrl%Class2
   end if


   !----------------------------------------------------------------------------
   ! Set default values of the Ctrl structure
   !----------------------------------------------------------------------------
   Ctrl%Run_ID = 'none'

   !----------------------- Ctrl%RS -----------------------
   Ctrl%RS%RsSelm         = switch_app(a, Default=SelmAux)
   Ctrl%RS%SRsSelm        = switch_app(a, Default=SelmMeas, Aer=SelmCtrl)
   Ctrl%RS%use_full_brdf  = switch_cls(c, Default=.true.,   AerSw=.false.)
   Ctrl%RS%read_full_brdf = switch_cls(c, Default=.true.,   AerSw=.false.)
   Ctrl%RS%Cb             = switch_app(a, Default=0.2,      AerOx=0.4, &
                                       AerSw=0.4, AerO1=0.98)
   Ctrl%RS%add_fractional = switch_app(a, Default=.false.,  AerOx=.true.)
   Ctrl%RS%diagonal_SRs   = switch_app(a, Default=.false.,  AerOx=.true., &
                                       AerO1=.false.)

   ! Select assumed surface reflectance based on wavelength
   Ctrl%RS%allow_a_default_surface = .true.

   wvl_threshold = 0.05
   if (Ctrl%Ind%NSolar > 0) allocate(Ctrl%RS%B(Ctrl%Ind%NSolar, MaxSurf))
   do i = 1, Ctrl%Ind%NSolar
      ii = Ctrl%Ind%ICh(Ctrl%Ind%YSolar(i))

      if (abs(channel_wvl(ii) - 0.55) < wvl_threshold) then      ! AATSR Ch1
         Ctrl%RS%B(i,ISea)  = switch_app(a, Default=0.05, Aer=0.01)
         Ctrl%RS%B(i,ILand) = switch_app(a, Default=0.15, Aer=0.20)
      else if (abs(channel_wvl(ii) - 0.67) < wvl_threshold) then ! AATSR Ch2
         Ctrl%RS%B(i,ISea)  = switch_app(a, Default=0.02, Aer=0.005)
         Ctrl%RS%B(i,ILand) = switch_app(a, Default=0.10, Aer=0.15)
      else if (abs(channel_wvl(ii) - 0.87) < wvl_threshold) then ! AATSR Ch3
         Ctrl%RS%B(i,ISea)  = switch_app(a, Default=0.01, Aer=0.0001)
         Ctrl%RS%B(i,ILand) = switch_app(a, Default=0.01, Aer=0.10)
      else if (abs(channel_wvl(ii) - 1.6) < wvl_threshold) then  ! AATSR Ch4
         Ctrl%RS%B(i,ISea)  = switch_app(a, Default=0.01, Aer=0.00)
         Ctrl%RS%B(i,ILand) = switch_app(a, Default=0.01, Aer=0.01)
      else ! No opinion on channel
         Ctrl%RS%B(i,:)     = 0.0
      end if
   end do

   if (Ctrl%Ind%NSolar > 0) then
      allocate(Ctrl%RS%Sb(Ctrl%Ind%NSolar, MaxSurf))
      Ctrl%RS%Sb(:,ISea)  = switch_app(a, Default=0.2, AerOx=0.0)
      Ctrl%RS%Sb(:,ILand) = switch_app(a, Default=0.2, AerOx=2e-4)
   end if

   !----------------------- Ctrl%EqMPN --------------------
   Ctrl%EqMPN%SySelm = switch_app(a, Default=SelmAux)
   Ctrl%EqMPN%Homog  = switch_app(a, Default=.true.,  Aer=.false.)
   Ctrl%EqMPN%Coreg  = switch_app(a, Default=.true.,  Aer=.false.)

   !----------------------- Ctrl%Invpar -------------------
   Ctrl%Invpar%ConvTest           = switch_app(a, Default=.false., Aer=.true.)
   Ctrl%Invpar%MqStart            = switch_app(a, Default=0.001)
   Ctrl%Invpar%MqStep             = switch_app(a, Default=10.0)
   Ctrl%Invpar%MaxIter            = switch_app(a, Default=40,      Aer=25)
   Ctrl%Invpar%Ccj                = switch_app(a, Default=0.05,    AerSw=0.001)
   Ctrl%Invpar%Pc_dmz             = switch_app(a, Default=50.0)
   Ctrl%Invpar%always_take_GN     = switch_app(a, Default=.false., AerOx=.true.)
   Ctrl%Invpar%dont_iter_convtest = switch_app(a, Default=.false., Aer=.true.)
   Ctrl%Invpar%disable_Ss         = switch_app(a, Default=.false., Aer=.true.)

   ! Scale used to avoid pivots in matrix inversion. Terms that a mode shouldn't
   ! touch are zeroed to
   ! NOTES: 1) Aerosol LUTs use log Re while cloud uses linear Re.
   ! 2) For the Swansea retrieval, IRho_DD is equivalent to ISS.
   Ctrl%Invpar%XScale(ITau)           = switch_cls(c, Default=10.0, AerSw=1.0)
   Ctrl%Invpar%XScale(IRe)            = switch_cls(c, Default=1.0,  AerOx=10.0)
   Ctrl%Invpar%XScale(IPc)            = switch_cls(c, Default=1.0)
   Ctrl%Invpar%XScale(IFr)            = switch_cls(c, Default=1000.0)
   Ctrl%Invpar%XScale(ITau2)          = switch_cls(c2,Default=10.0, AerSw=1.0)
   Ctrl%Invpar%XScale(IRe2)           = switch_cls(c2,Default=1.0,  AerOx=10.0)
   Ctrl%Invpar%XScale(IPc2)           = switch_cls(c2,Default=1.0)
   Ctrl%Invpar%XScale(IFr2)           = switch_cls(c2,Default=1000.0)
   Ctrl%Invpar%XScale(ITs)            = switch_app(a, Default=1.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_0V)) = switch_app(a, Default=1.0,  AerOx=1000.0, &
                                                                    AerO1=100.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_0D)) = switch_app(a, Default=1.0,  AerOx=1000.0, &
                                                                    AerO1=100.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_DV)) = switch_app(a, Default=1.0,  AerOx=1000.0, &
                                                                    AerO1=100.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_DD)) = switch_app(a, Default=1.0,  AerOx=1000.0, &
                                                                    AerO1=100.0)
   Ctrl%Invpar%XScale(ISP)            = switch_app(a, Default=1.0)
   Ctrl%Invpar%XScale(ISG)            = switch_app(a, Default=1.0)
   ! Lower limit
   Ctrl%Invpar%XLLim(ITau)            = switch_cls(c, Default=-3.0, Aer=-2.0)
   Ctrl%Invpar%XLLim(IRe)             = switch_cls(c, Default=0.1,  Aer=-2.0, &
                                                                    AshEyj=0.01)
   Ctrl%Invpar%XLLim(IPc)             = switch_cls(c, Default=10.0)
   Ctrl%Invpar%XLLim(IFr)             = switch_cls(c, Default=0.0)
   Ctrl%Invpar%XLLim(ITau2)           = switch_cls(c2,Default=-3.0)
   Ctrl%Invpar%XLLim(IRe2)            = switch_cls(c2,Default=0.1)
   Ctrl%Invpar%XLLim(IPc2)            = switch_cls(c2,Default=10.0)
   Ctrl%Invpar%XLLim(IFr2)            = switch_cls(c2,Default=0.0)
   Ctrl%Invpar%XLLim(ITs)             = switch_app(a, Default=200.)
   Ctrl%Invpar%XLLim(IRs(:,IRho_0V))  = switch_app(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(IRs(:,IRho_0D))  = switch_app(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(IRs(:,IRho_DV))  = switch_app(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(IRs(:,IRho_DD))  = switch_app(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(ISP)             = switch_app(a, Default=0.00001)
   Ctrl%Invpar%XLLim(ISG)             = switch_app(a, Default=0.00001)
   ! Upper limit
   Ctrl%Invpar%XULim(ITau)            = switch_cls(c, Default=2.408, Aer=0.7)
   Ctrl%Invpar%XULim(IRe)             = switch_cls(c, Default=1.0,   AshEyj=20.0, &
                                                                     CldWat=35.0, &
                                                                     CldIce=100.0)
   Ctrl%Invpar%XULim(IPc)             = switch_cls(c, Default=1200.0)
   Ctrl%Invpar%XULim(IFr)             = switch_cls(c, Default=1.0)
   Ctrl%Invpar%XULim(ITau2)           = switch_cls(c2,Default=2.408, Aer=0.7)
   Ctrl%Invpar%XULim(IRe2)            = switch_cls(c2,Default=1.0,   AshEyj=20.0, &
                                                                     CldWat=35.0, &
                                                                     CldIce=100.0)
   Ctrl%Invpar%XULim(IPc2)            = switch_cls(c2,Default=1200.0)
   Ctrl%Invpar%XULim(IFr2)            = switch_cls(c2,Default=1.0)
   if ((Ctrl%InstName(1:5) .eq. 'AATSR') .or. (Ctrl%InstName(1:5) .eq. 'ATSR2')) then
      Ctrl%Invpar%XULim(ITs)          = switch_app(a, Default=320.0)
   else
      Ctrl%Invpar%XULim(ITs)          = switch_app(a, Default=400.0)
   end if
   Ctrl%Invpar%XULim(IRs(:,IRho_0V))  = switch_app(a, Default=1.0)
   Ctrl%Invpar%XULim(IRs(:,IRho_0D))  = switch_app(a, Default=1.0)
   Ctrl%Invpar%XULim(IRs(:,IRho_DV))  = switch_app(a, Default=1.0)
   Ctrl%Invpar%XULim(IRs(:,IRho_DD))  = switch_app(a, Default=1.0)
   Ctrl%Invpar%XULim(ISP)             = switch_app(a, Default=100.0)
   Ctrl%Invpar%XULim(ISG)             = switch_app(a, Default=1.0)

   !----------------------- Ctrl%QC -----------------------
   Ctrl%QC%MaxJ         = switch_app(a, Default=100.0, Aer=3.0)
   Ctrl%QC%MaxDoFN      = switch_app(a, Default=2.0)
   Ctrl%QC%MaxElevation = switch_app(a, Default=1500.0)

   !------------------- Ctrl START/END POINT --------------
   ! Process entire file
   Ctrl%Ind%X0 = 0
   Ctrl%Ind%X1 = 0
   Ctrl%Ind%Y0 = 0
   Ctrl%Ind%Y1 = 0

   !------------ Ctrl ILLUMINATION CONDITIONS -------------
   Ctrl%MaxSolZen = 75. ! Maximum solar zenith angle
   Ctrl%MaxSatZen = 90. ! Maximum satellite zenith angle
   Ctrl%MinRelAzi = 0.  ! Used to remove sunglint (0 = no test)
   Ctrl%Sunset    = 90. ! Used to identify twilight conditions

   !----------------------- Ctrl SWITCHES -----------------
   Ctrl%i_equation_form  = switch_cls(c, Default=3, AerOx=1, AerSw=0, AerBR=1)
   Ctrl%LUTIntSelm       = switch_app(a, Default=LUTIntMethLinear)
   Ctrl%RTMIntSelm       = switch_app(a, Default=RTMIntMethLinear, Aer=RTMIntMethNone)
   Ctrl%CloudType        = switch_app(a, Default=1,                Aer=2)
   Ctrl%Max_SDAD         = 10.0
   Ctrl%sabotage_inputs  = .false.
   Ctrl%surfaces_to_skip = 0_byte
   Ctrl%second_aot_ch    = 3 ! Assuming AATSR

   ! Set cloud types to process depending on requested LUT
   Ctrl%Types_to_process = byte_fill_value
   if (Ctrl%use_ann_phase .and. Ctrl%Approach == AppCld1L .and. &
       Ctrl%Class == ClsCldWat) then
      Ctrl%NTypes_to_process   = 1
      Ctrl%Types_to_process(1) = LIQUID

   else if (Ctrl%use_ann_phase .and. Ctrl%Approach == AppCld1L .and. &
            Ctrl%Class == ClsCldIce) then
      Ctrl%NTypes_to_process   = 1
      Ctrl%Types_to_process(1) = ICE

   else if (.not. Ctrl%use_ann_phase .and. Ctrl%Approach == AppCld1L .and. &
            Ctrl%Class == ClsCldWat) then
      Ctrl%NTypes_to_process   = 3
      Ctrl%Types_to_process(1) = FOG_TYPE
      Ctrl%Types_to_process(2) = WATER_TYPE
      Ctrl%Types_to_process(3) = SUPERCOOLED_TYPE
      if (.not. Ctrl%process_cloudy_only) then
         Ctrl%NTypes_to_process   = 5
         Ctrl%Types_to_process(4) = CLEAR_TYPE
         Ctrl%Types_to_process(5) = PROB_CLEAR_TYPE
      end if

   else if (.not. Ctrl%use_ann_phase .and. Ctrl%Approach == AppCld1L .and. &
            Ctrl%Class == ClsCldIce) then
      Ctrl%NTypes_to_process   = 4
      Ctrl%Types_to_process(1) = OPAQUE_ICE_TYPE
      Ctrl%Types_to_process(2) = CIRRUS_TYPE
      Ctrl%Types_to_process(3) = OVERLAP_TYPE
      Ctrl%Types_to_process(4) = PROB_OPAQUE_ICE_TYPE
      if (.not. Ctrl%process_cloudy_only) then
         Ctrl%NTypes_to_process   = 6
         Ctrl%Types_to_process(5) = CLEAR_TYPE
         Ctrl%Types_to_process(6) = PROB_CLEAR_TYPE
      end if

   else if (Ctrl%use_ann_phase .and. Ctrl%Approach == AppCld2L) then
         if (Ctrl%verbose) write(*,*) 'WARNING: Read_Driver(): '// &
              'Ctrl%use_ann_phase=true with Ctrl%Approach=AppCld2L '// &
              'processes all pixels.'
         Ctrl%NTypes_to_process    = 11
         Ctrl%Types_to_process(1)  = CLEAR_TYPE
         Ctrl%Types_to_process(2)  = SWITCHED_TO_WATER_TYPE
         Ctrl%Types_to_process(3)  = FOG_TYPE
         Ctrl%Types_to_process(4)  = WATER_TYPE
         Ctrl%Types_to_process(5)  = SUPERCOOLED_TYPE
         Ctrl%Types_to_process(6)  = SWITCHED_TO_ICE_TYPE
         Ctrl%Types_to_process(7)  = OPAQUE_ICE_TYPE
         Ctrl%Types_to_process(8)  = CIRRUS_TYPE
         Ctrl%Types_to_process(9)  = OVERLAP_TYPE
         Ctrl%Types_to_process(10) = PROB_OPAQUE_ICE_TYPE
         Ctrl%Types_to_process(11) = PROB_CLEAR_TYPE

   else if (.not. Ctrl%use_ann_phase .and. Ctrl%Approach == AppCld2L) then
      Ctrl%NTypes_to_process   = 1
      Ctrl%Types_to_process(1) = OVERLAP_TYPE

   else
      ! Accept everything
      Ctrl%NTypes_to_process   = MaxTypes
      Ctrl%Types_to_process    = [(i-1, i = 1, MaxTypes)]
   end if

   !---------------- Ctrl INDEXING CHANNELS ---------------
   ! See Ctrl.F90 for descriptions of the variables initialized below.
   Ctrl%Ind%Y_Id_legacy = 0

   if (Ctrl%InstName(1:5) == 'AATSR' .or. Ctrl%InstName(1:5) == 'ATSR2') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 4
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 6
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 7

      allocate(Ctrl%ReChans(2))
      Ctrl%ReChans = (/ 5, 4 /)

      allocate(Ctrl%tau_chans(3))
      Ctrl%tau_chans = (/ 1, 2, 3 /)
      allocate(Ctrl%r_e_chans(2))
      Ctrl%r_e_chans = (/ 4, 5 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 5, 6, 7 /)
   else if (Ctrl%InstName(1:3) == 'ABI') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 7
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 14
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 15

      allocate(Ctrl%ReChans(3))
      Ctrl%ReChans = (/ 7, 5, 6 /)

      allocate(Ctrl%tau_chans(3))
      Ctrl%tau_chans = (/ 1, 2, 3 /)
      allocate(Ctrl%r_e_chans(3))
      Ctrl%r_e_chans = (/ 5, 6, 7 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 7, 14, 15 /)
   else if (Ctrl%InstName(1:4) == 'AGRI') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 7
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 11
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 12

      allocate(Ctrl%ReChans(3))
      Ctrl%ReChans = (/ 5, 7, 6 /)

      allocate(Ctrl%tau_chans(3))
      Ctrl%tau_chans = (/ 2, 3, 1 /)
      allocate(Ctrl%r_e_chans(3))
      Ctrl%r_e_chans = (/ 5, 7, 6 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 7, 11, 12 /)
   else if (Ctrl%InstName(1:3) == 'AHI') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 4
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 7
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 14
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 15

      allocate(Ctrl%ReChans(3))
      Ctrl%ReChans = (/ 7, 5, 6 /)

      allocate(Ctrl%tau_chans(4))
      Ctrl%tau_chans = (/ 3, 4, 1, 2 /)
      allocate(Ctrl%r_e_chans(3))
      Ctrl%r_e_chans = (/ 5, 6, 7 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 7, 14, 15 /)
   else if (Ctrl%InstName(1:5) == 'AVHRR') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 1
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 4
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 6

      allocate(Ctrl%ReChans(2))
      if (Ctrl%InstName(7:12) == 'NOAA17') then
         Ctrl%ReChans = (/ 3, 4 /)
      else
         Ctrl%ReChans = (/ 4, 3 /)
      end if

      allocate(Ctrl%tau_chans(2))
      Ctrl%tau_chans = (/ 1, 2 /)
      allocate(Ctrl%r_e_chans(2))
      Ctrl%r_e_chans = (/ 3, 4 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 4, 5, 6 /)
   else if (Ctrl%InstName(1:5) == 'MODIS') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 1
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 6
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 20
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 31
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 32

      allocate(Ctrl%ReChans(4))
      Ctrl%ReChans = (/ 20, 6, 7, 5 /)

      allocate(Ctrl%tau_chans(4))
      Ctrl%tau_chans = (/ 1, 2, 3, 4 /)
      allocate(Ctrl%r_e_chans(4))
      Ctrl%r_e_chans = (/ 5, 6, 7, 20 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 20, 31, 32 /)
   else if (Ctrl%InstName(1:6) == 'SEVIRI') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 1
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 4
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 9
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 10

      allocate(Ctrl%ReChans(2))
      Ctrl%ReChans = (/ 4, 3 /)

      allocate(Ctrl%tau_chans(2))
      Ctrl%tau_chans = (/ 1, 2 /)
      allocate(Ctrl%r_e_chans(2))
      Ctrl%r_e_chans = (/ 3, 4 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 4, 9, 10 /)
   else if (Ctrl%InstName(1:6) == 'VIIRSM') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 7
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 10
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 12
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 15
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 16

      allocate(Ctrl%ReChans(5))
      Ctrl%ReChans = (/ 12, 10, 8, 9, 11 /)

      allocate(Ctrl%tau_chans(4))
      Ctrl%tau_chans = (/ 5, 7, 3, 4 /)
      allocate(Ctrl%r_e_chans(4))
      Ctrl%r_e_chans = (/ 8, 10, 11, 12 /)
      allocate(Ctrl%ir_chans(4))
      Ctrl%ir_chans  = (/ 12, 15, 16, 14 /)
   else if (Ctrl%InstName(1:6) == 'VIIRSI') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 1
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 4
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 5

      allocate(Ctrl%ReChans(2))
      Ctrl%ReChans = (/ 3, 4 /)

      allocate(Ctrl%tau_chans(2))
      Ctrl%tau_chans = (/ 2, 3 /)
      allocate(Ctrl%r_e_chans(2))
      Ctrl%r_e_chans = (/ 3, 4 /)
      allocate(Ctrl%ir_chans(2))
      Ctrl%ir_chans  = (/ 4, 5 /)
   else if (Ctrl%InstName(1:16) == 'SLSTR-Sentinel-3') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 7
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 8
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 9

      allocate(Ctrl%ReChans(3))
      Ctrl%ReChans = (/ 7, 5, 6 /)

      allocate(Ctrl%tau_chans(3))
      Ctrl%tau_chans = (/ 1, 2, 3 /)
      allocate(Ctrl%r_e_chans(3))
      Ctrl%r_e_chans = (/ 5, 6, 7 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 7, 8, 9 /)
   else
      write(*,*) 'ERROR: Read_Driver(): Unrecognised sensor/platform: ', &
                   trim(Ctrl%InstName)
      stop error_stop_code
   end if

   !---------------- Ctrl STATE VECTOR SELM ---------------
   ! Select the manner by which the a priori (AP) and first guess (FG) values
   ! are set before the retrieval.
   ! SelmCtrl) Use constant values prescribed in this routine.
   ! SelmAux)  Use values from some manner of auxiliary data file.
   ! SelmMeas) Deduce values from the measurement vector (see XMDAD).

   ! Currently these are identical for all illuminations. To be different, make
   ! three copies of the line and replace : with IDay, ITwi, and INight.
   Ctrl%AP(ITau,:)           = switch_cls(c, Default=SelmCtrl)
   Ctrl%AP(IRe,:)            = switch_cls(c, Default=SelmCtrl)
   Ctrl%AP(IPc,:)            = switch_cls(c, Default=SelmCtrl)
   Ctrl%AP(IFr,:)            = switch_cls(c, Default=SelmMeas)
   Ctrl%AP(ITau2,:)          = switch_cls(c2,Default=SelmCtrl)
   Ctrl%AP(IRe2,:)           = switch_cls(c2,Default=SelmCtrl)
   Ctrl%AP(IPc2,:)           = switch_cls(c2,Default=SelmCtrl)
   Ctrl%AP(IFr2,:)           = switch_cls(c2,Default=SelmCtrl)
   Ctrl%AP(ITs,:)            = switch_app(a, Default=SelmAux)
   Ctrl%AP(IRs(:,IRho_0V),:) = switch_app(a, Default=SelmAux, AerSw=SelmCtrl)
   Ctrl%AP(IRs(:,IRho_0D),:) = switch_app(a, Default=SelmAux)
   Ctrl%AP(IRs(:,IRho_DV),:) = switch_app(a, Default=SelmAux)
   Ctrl%AP(IRs(:,IRho_DD),:) = switch_app(a, Default=SelmAux)
   Ctrl%AP(ISP,:)            = switch_app(a, Default=SelmCtrl)
   Ctrl%AP(ISG,:)            = switch_app(a, Default=SelmCtrl)

   ! NOTES:
   ! 1) The aerosol code used SelmSAD for Tau and Re, which drew the values from
   !    a separate driver file. This must now be managed by the calling script
   !    setting Ctrl%XB and X0, as that knows what the LUT is.
   ! 2) Fr uses SelmMeas so the error is set to MDADErrF. Could be tidier.
   Ctrl%FG(ITau,:)           = switch_cls(c, Default=SelmCtrl)
   Ctrl%FG(IRe,:)            = switch_cls(c, Default=SelmCtrl)
   Ctrl%FG(IPc,:)            = switch_cls(c, Default=SelmMeas)
   Ctrl%FG(IFr,:)            = switch_cls(c ,Default=SelmCtrl)
   Ctrl%FG(ITau2,:)          = switch_cls(c2,Default=SelmCtrl)
   Ctrl%FG(IRe2,:)           = switch_cls(c2,Default=SelmCtrl)
   Ctrl%FG(IPc2,:)           = switch_cls(c2,Default=SelmCtrl)
   Ctrl%FG(IFr2,:)           = switch_cls(c2,Default=SelmCtrl)
   Ctrl%FG(ITs,:)            = switch_app(a, Default=SelmAux)
   Ctrl%FG(IRs(:,IRho_0V),:) = switch_app(a, Default=SelmAux, AerSw=SelmCtrl)
   Ctrl%FG(IRs(:,IRho_0D),:) = switch_app(a, Default=SelmAux)
   Ctrl%FG(IRs(:,IRho_DV),:) = switch_app(a, Default=SelmAux)
   Ctrl%FG(IRs(:,IRho_DD),:) = switch_app(a, Default=SelmAux)
   Ctrl%FG(ISP,:)            = switch_app(a, Default=SelmCtrl)
   Ctrl%FG(ISG,:)            = switch_app(a, Default=SelmCtrl)
   ! 3) Not sure why Fr is now SelmCtrl.

   !----------- Ctrl PRESCRIBED STATE VECTORS -------------
   ! A priori values
   if (Ctrl%Approach /= AppCld2L) then
      Ctrl%XB(ITau)        = switch_cls(c, Default=0.8,   AerOx=-1.5,  AerSw=-0.3, &
                                                          AshEyj=0.18)
      Ctrl%XB(IRe)         = switch_cls(c, Default=-0.07, AshEyj=0.7, &
                                                          CldWat=12.,  CldIce=30.)
      Ctrl%XB(IPc)         = switch_cls(c, Default=900.,  AshEyj=600., CldIce=400.)
      Ctrl%XB(IFr)         = switch_cls(c, Default=1.0)
      Ctrl%XB(ITau2)       = Ctrl%XB(ITau)
      Ctrl%XB(IRe2)        = Ctrl%XB(IRe)
      Ctrl%XB(IPc2)        = Ctrl%XB(IPc)
      Ctrl%XB(IFr2)        = Ctrl%XB(IFr)
   else
      Ctrl%XB(ITau)        = switch_cls(c, Default=0.1)
      Ctrl%XB(IRe)         = switch_cls(c, Default=30.0, AshEyj=0.7, CldWat=12.)
      Ctrl%XB(IPc)         = switch_cls(c, Default=245.0)
      Ctrl%XB(IFr)         = switch_cls(c, Default=1.0)
      Ctrl%XB(ITau2)       = switch_cls(c2,Default=0.8)
      Ctrl%XB(IRe2)        = switch_cls(c2,Default=12.0, AshEyj=0.7, CldIce=30.)
      Ctrl%XB(IPc2)        = switch_cls(c2,Default=800.0)
      Ctrl%XB(IFr2)        = switch_cls(c2,Default=1.0)
   end if
   Ctrl%XB(ITs)            = switch_app(a, Default=300.0)
   Ctrl%XB(IRs(:,IRho_0V)) = switch_app(a, Default=0.01,  AerSw=0.1)
   Ctrl%XB(IRs(:,IRho_0D)) = switch_app(a, Default=0.01)
   Ctrl%XB(IRs(:,IRho_DV)) = switch_app(a, Default=0.01)
   Ctrl%XB(IRs(:,IRho_DD)) = switch_app(a, Default=0.01)
   Ctrl%XB(ISP)            = switch_app(a, Default=0.3)
   Ctrl%XB(ISG)            = switch_app(a, Default=0.3)

   ! First guess values
   if (Ctrl%Approach /= AppCld2L) then
      Ctrl%X0(ITau)        = switch_cls(c, Default=0.8,   AerOx=-1.5,  AerSw=-0.3, &
                                        AshEyj=0.18)
      Ctrl%X0(IRe)         = switch_cls(c, Default=-0.07, AshEyj=0.7, &
                                        CldWat=12.,  CldIce=30.)
      Ctrl%X0(IPc)         = switch_cls(c, Default=900.,  AshEyj=600., CldIce=400.)
      Ctrl%X0(IFr)         = switch_cls(c, Default=1.0)
      Ctrl%X0(ITau2)       = Ctrl%X0(ITau)
      Ctrl%X0(IRe2)        = Ctrl%X0(IRe)
      Ctrl%X0(IPc2)        = Ctrl%X0(IPc)
      Ctrl%X0(IFr2)        = Ctrl%X0(IFr)
   else
      Ctrl%X0(ITau)        = switch_cls(c, Default=0.1)
      Ctrl%X0(IRe)         = switch_cls(c, Default=30.0, AshEyj=0.7, CldWat=12.)
      Ctrl%X0(IPc)         = switch_cls(c, Default=245.0)
      Ctrl%X0(IFr)         = switch_cls(c, Default=1.0)
      Ctrl%X0(ITau2)       = switch_cls(c2,Default=0.8)
      Ctrl%X0(IRe2)        = switch_cls(c2,Default=12.0, AshEyj=0.7, CldIce=30.)
      Ctrl%X0(IPc2)        = switch_cls(c2,Default=800.0)
      Ctrl%X0(IFr2)        = switch_cls(c2,Default=1.0)
   end if
   Ctrl%X0(ITs)            = switch_app(a, Default=300.0)
   Ctrl%X0(IRs(:,IRho_0V)) = switch_app(a, Default=0.01,  AerSw=0.5)
   Ctrl%X0(IRs(:,IRho_0D)) = switch_app(a, Default=0.01)
   Ctrl%X0(IRs(:,IRho_DV)) = switch_app(a, Default=0.01)
   Ctrl%X0(IRs(:,IRho_DD)) = switch_app(a, Default=0.01)
   Ctrl%X0(ISP)            = switch_app(a, Default=0.3)
   Ctrl%X0(ISG)            = switch_app(a, Default=0.3)

   ! A priori uncertainty
   if (Ctrl%Approach /= AppCld2L) then
      Ctrl%Sx(ITau)        = switch_cls(c, Default=1.0e+08, Aer=2.0)
      Ctrl%Sx(IRe)         = switch_cls(c, Default=1.0e+08, Aer=0.5)
      Ctrl%Sx(IPc)         = switch_cls(c, Default=1.0e+08)
      Ctrl%Sx(IFr)         = switch_cls(c, Default=1.0e+08)
      Ctrl%Sx(ITau2)       = switch_cls(c2,Default=1.0e+08, Aer=2.0)
      Ctrl%Sx(IRe2)        = switch_cls(c2,Default=1.0e+08, Aer=0.5)
      Ctrl%Sx(IPc2)        = switch_cls(c2,Default=1.0e+08)
      Ctrl%Sx(IFr2)        = switch_cls(c2,Default=1.0e+08)
      Ctrl%Sx(ITs)         = switch_app(a, Default=1.0e+08)
   else
      Ctrl%Sx(ITau)        = switch_cls(c, Default=1.0e+08, Aer=2.0)
      Ctrl%Sx(IRe)         = switch_cls(c, Default=1.0e+08, Aer=0.5)
      Ctrl%Sx(IPc)         = switch_cls(c, Default=200.0)
      Ctrl%Sx(IFr)         = switch_cls(c, Default=1.0e+08)
      Ctrl%Sx(ITau2)       = switch_cls(c2,Default=1.0e+08, Aer=2.0)
      Ctrl%Sx(IRe2)        = switch_cls(c2,Default=4.0, Aer=0.5)
      Ctrl%Sx(IPc2)        = switch_cls(c2,Default=200.0)
      Ctrl%Sx(IFr2)        = switch_cls(c2,Default=1.0e+08)
      Ctrl%Sx(ITs)         = switch_app(a, Default=1.0e+08)
   end if

   Ctrl%Sx(IRs(:,IRho_0V)) = switch_app(a, Default=1.0e+08, AerSw=1.0)
   Ctrl%Sx(IRs(:,IRho_0D)) = switch_app(a, Default=1.0e+08)
   Ctrl%Sx(IRs(:,IRho_DV)) = switch_app(a, Default=1.0e+08)
   Ctrl%Sx(IRs(:,IRho_DD)) = switch_app(a, Default=1.0e+08, AerOx=0.05, AerO1=0.01)
   Ctrl%Sx(ISP(1))         = switch_app(a, Default=1.0e+08, AerSw=0.01)
   Ctrl%Sx(ISP(2:))        = switch_app(a, Default=1.0e+08, AerSw=0.5)
   Ctrl%Sx(ISG)            = switch_app(a, Default=0.1)
   ! NOTE: The nadir P value doesn't really need to be retrieved.

   ! Measurement covariance
   allocate(Ctrl%Sy(Ctrl%Ind%Ny,Ctrl%Ind%Ny))
   Ctrl%Sy = 0.
   do i = 1, Ctrl%Ind%Ny
      if (btest(Ctrl%Ind%Ch_Is(i), ThermalBit)) then
         Ctrl%Sy(i,i) = 2.0
      else
         Ctrl%Sy(i,i) = 0.05
      end if
   end do

   ! Using SelmMeas with the Swansea retrieval assumes some atmospheric state to
   ! algebratically invert the radiances and find first guesses for the S
   ! parameters. In the absence of a better idea, these are the a priori.
   Ctrl%RS%SS_Xb = Ctrl%Xb
   Ctrl%RS%SS_Sx = Ctrl%Sx

   !------------- Ctrl STATE VECTOR INDEXING --------------
   ! These arrays specify which variables should be retrieved in each
   ! illumination condition (by the index of that variable; see ORAC_Constants_m).
   X_Dy  = sint_fill_value
   X_Tw  = sint_fill_value
   X_Ni  = sint_fill_value
   XJ_Dy = sint_fill_value
   XJ_Tw = sint_fill_value
   XJ_Ni = sint_fill_value
   if (Ctrl%Approach == AppAerOx) then
      ! Retrieve optical depth, effective radius, and white sky albedo in all
      ! channels (it'll work out which duplicate views later). No night/twilight.
      Nx_Dy   = 2
      X_Dy(1) = ITau
      X_Dy(2) = IRe
      do i = 1, Ctrl%Ind%NSolar
         ! Only accept the first view
         ! ACP: This avoids outputting every view, while keeping the ability to
         ! retrieve whatever BRDF terms we like. When we get around to nviews>2,
         ! this approach should be reassessed.
         if (Ctrl%Ind%View_Id(Ctrl%Ind%YSolar(i)) == 1) then
            Nx_Dy = Nx_Dy+1
            X_Dy(Nx_Dy) = IRs(i,IRho_DD)
         end if
      end do
      Nx_Tw = 0
      Nx_Ni = 0

      ! No terms added to the Jacobian
      NXJ_Dy = 0
      NXJ_Tw = 0
      NXJ_Ni = 0
   else if (Ctrl%Approach == AppAerSw) then
      ! Retrieve optical depth, effective radius, and Swansea parameters in all
      ! channels (it'll work out which duplicate views later). No night/twilight.
      Nx_Dy   = 2
      X_Dy(1) = ITau
      X_Dy(2) = IRe
      do i = 1, Ctrl%Ind%NSolar
         ! Only accept the first view
         if (Ctrl%Ind%View_Id(Ctrl%Ind%YSolar(i)) == 1) then
            Nx_Dy = Nx_Dy+1
            X_Dy(Nx_Dy) = ISS(i)
         end if
      end do
      do i = 1, Ctrl%Ind%NViews
         Nx_Dy = Nx_Dy+1
         X_Dy(Nx_Dy) = ISP(i)
      end do
      Nx_Tw = 0
      Nx_Ni = 0

      ! No terms added to the Jacobian
      NXJ_Dy = 0
      NXJ_Tw = 0
      NXJ_Ni = 0
   else if (Ctrl%Approach == AppAerO1) then
      ! Retrieve optical depth, effective radius, and white sky albedo in 1st
      ! channel (albedo in other channels will scale with this value).
      ! No night/twilight.
      Nx_Dy   = 2
      X_Dy(1) = ITau
      X_Dy(2) = IRe
      do i = 1, Ctrl%Ind%NSolar
         ! Only accept the first view
         ! ACP: This avoids outputting every view, while keeping the ability to
         ! retrieve whatever BRDF terms we like. When we get around to nviews>2,
         ! this approach should be reassessed.
         if (Ctrl%Ind%View_Id(Ctrl%Ind%YSolar(i)) == 1) then
            Nx_Dy = Nx_Dy+1
            X_Dy(Nx_Dy) = IRs(i,IRho_DD)
         end if
      end do
      Nx_Tw = 0
      Nx_Ni = 0

      ! No terms added to the Jacobian
      NXJ_Dy = 0
      NXJ_Tw = 0
      NXJ_Ni = 0
   else if (Ctrl%Approach == AppCld1L) then
      ! By day, retrieve optical depth, effective radius, cloud top pressure and
      ! surface temperature.
      Nx_Dy    = 4
      X_Dy(1)  = ITau
      X_Dy(2)  = IRe
      X_Dy(3)  = IPc
      X_Dy(4)  = ITs

      ! Include white sky albedo in Jacobian.
      NXJ_Dy   = 0
      do i = 1, Ctrl%Ind%NSolar
         NXJ_Dy = NXJ_Dy+1
         XJ_Dy(NXJ_Dy) = IRs(i,IRho_DD)
      end do

      ! In twilight retrieve cloud top pressure and surface. Include white sky
      ! albedo in Jacobian.
      Nx_Tw   = 2
      X_Tw(1) = IPc
      X_Tw(2) = ITs
      NXJ_Tw  = NXJ_Dy
      XJ_Tw   = XJ_Dy

      if (.not. Ctrl%do_new_night_retrieval) then
         ! At night retrieve cloud top pressure and surface. Include white sky
         ! albedo in Jacobian.
         Nx_Ni   = 2
         X_Ni(1) = IPc
         X_Ni(2) = ITs
      else
         ! At night retrieve optical depth, effective radius, cloud top pressure
         ! and surface temperature.
         Nx_Ni    = 4
         X_Ni(1)  = ITau
         X_Ni(2)  = IRe
         X_Ni(3)  = IPc
         X_Ni(4)  = ITs
      end if

      NXJ_Ni  = NXJ_Dy
      XJ_Ni   = XJ_Dy
   else if (Ctrl%Approach == AppCld2L) then
      ! By day, retrieve optical depth, effective radius, cloud top pressure and
      ! surface temperature.
      Nx_Dy    = 7
      X_Dy(1)  = ITau
      X_Dy(2)  = IRe
      X_Dy(3)  = IPc
      X_Dy(4)  = ITau2
      X_Dy(5)  = IRe2
      X_Dy(6)  = IPc2
      X_Dy(7)  = ITs

      ! Include white sky albedo in Jacobian.
      NXJ_Dy   = 0
      do i = 1, Ctrl%Ind%NSolar
         NXJ_Dy = NXJ_Dy+1
         XJ_Dy(NXJ_Dy) = IRs(i,IRho_DD)
      end do

      ! In twilight retrieve cloud top pressure and surface. Include white sky
      ! albedo in Jacobian.
      Nx_Tw   = 3
      X_Tw(1) = IPc
      X_Tw(2) = IPc2
      X_Tw(3) = ITs
      NXJ_Tw  = NXJ_Dy
      XJ_Tw   = XJ_Dy

      if (.not. Ctrl%do_new_night_retrieval) then
         ! At night retrieve cloud top pressure and surface. Include white sky
         ! albedo in Jacobian.
         Nx_Ni   = 3
         X_Ni(1) = IPc
         X_Ni(2) = IPc2
         X_Ni(3) = ITs
      else
         ! At night retrieve optical depth, effective radius, cloud top pressure
         ! and surface temperature.
         Nx_Ni    = 7
         X_Ni(1)  = ITau
         X_Ni(2)  = IRe
         X_Ni(3)  = IPc
         X_Ni(4)  = ITau2
         X_Ni(5)  = IRe2
         X_Ni(6)  = IPc2
         X_Ni(7)  = ITs
      end if

      NXJ_Ni  = NXJ_Dy
      XJ_Ni   = XJ_Dy
   else
      write(*,*) 'ERROR: Read_Driver(): Invalid Ctrl%Approach: ', Ctrl%Approach
      stop error_stop_code
   end if

   Ctrl%CTP_correction_limit = 100.


   !----------------------------------------------------------------------------
   ! Consider optional lines of driver file
   !----------------------------------------------------------------------------
   ! Copy individual illumination arrays into main block
   Ctrl%Nx(IDay)     = Nx_Dy
   Ctrl%Nx(ITwi)     = Nx_Tw
   Ctrl%Nx(INight)   = Nx_Ni
   Ctrl%X(:,IDay)    = X_Dy
   Ctrl%X(:,ITwi)    = X_Tw
   Ctrl%X(:,INight)  = X_Ni

   Ctrl%NXJ(IDay)    = NXJ_Dy
   Ctrl%NXJ(ITwi)    = NXJ_Tw
   Ctrl%NXJ(INight)  = NXJ_Ni
   Ctrl%XJ(:,IDay)   = XJ_Dy
   Ctrl%XJ(:,ITwi)   = XJ_Tw
   Ctrl%XJ(:,INight) = XJ_Ni

   call read_ctrl(drifile, Ctrl)


   ! ---------------------------------------------------------------------------
   ! Things that have to be after the optional lines
   ! ---------------------------------------------------------------------------

   if (Ctrl%RS%use_full_brdf) Ctrl%RS%read_full_brdf = .true.
   if (.not. Ctrl%RS%use_full_brdf .and. Ctrl%Approach == AppCld1L) Ctrl%RS%read_full_brdf = .false.

   if (Ctrl%RS%use_full_brdf .and. Ctrl%i_equation_form == 0) then
      write(*,*) 'ERROR: Read_Driver(): Ctrl%RS%use_full_brdf = true cannot ' // &
                 'be used with i_equation_form = 0'
      stop error_stop_code
   end if

   if (.not. Ctrl%RS%use_full_brdf .and. Ctrl%i_equation_form > 0) then
      Ctrl%i_equation_form = 0
!     write(*,*) 'ERROR: Read_Driver(): Ctrl%RS%use_full_brdf = false cannot ' // &
!                'be used with i_equation_form > 0'
!     stop error_stop_code
   end if

   ! Whether or not to multiply surface reflectance terms by cos(theta_0)
   ! depends on Ctrl%i_equation_form.
   select case (Ctrl%i_equation_form)
   case(0)
      Ctrl%RS%solar_factor = .false.
   case(1)
      Ctrl%RS%solar_factor = .false.
   case(2)
      Ctrl%RS%solar_factor = .true.
   case(3)
      Ctrl%RS%solar_factor = .false.
   case(4)
      Ctrl%RS%solar_factor = .true.
   case default
      write(*,*) 'ERROR: Read_Driver(): Invalid Ctrl%i_equation_form: ', &
                 Ctrl%i_equation_form
      stop error_stop_code
   end select

   ! T_dv (or Td using the old naming convention) is not correctly produced in
   ! either RAL's or GT's LUT code.  It should approach zero as the optical
   ! thickness approaches zero as scattering is required to scatter diffuse
   ! radiation into the satellite viewing angle beam.  In the current LUTs it
   ! approaches unity instead.  Analogously, T_0d (or Tfbd using the old naming
   ! convention) must also approach zero as the optical thickness approaches
   ! zero since scattering is required to scatter radiation out of the beam to
   ! produce diffuse radiation.  In this case the LUTs are correct and since
   ! T_dv(theta) = T_0d(theta) we can obtain T_dv from T_0d by using the
   ! satellite viewing angle instead of the solar zenith angle.
   !
   ! Interestingly, equations 1 and 2 use a simplification that breaks down as
   ! optical thickness approaches zero and the incorrect values for T_dv provide
   ! a compensating effect and is therefore required as is.  This is very likely
   ! the reason this problem went unnoticed until the reciprocity-obeying form
   ! (equations 3 and 4) were introduced.
   if (Ctrl%i_equation_form == 3 .or. Ctrl%i_equation_form == 4) then
      Ctrl%get_T_dv_from_T_0d = .true.
   else
      Ctrl%get_T_dv_from_T_0d = .false.
   end if

   ! Identify which surface fields are in use
   Ctrl%Ind%Nss = 0
   Ctrl%Ind%Nrho = 0
   if (Ctrl%Approach == AppAerSw) then
      if (Ctrl%Ind%NSolar > 0) allocate(Ctrl%Ind%ss_terms(Ctrl%Ind%NSolar))

      ! Count number of surface terms retrieved
      do i = 1, Ctrl%Ind%NSolar
         if (any(Ctrl%X == ISS(i))) then
            Ctrl%Ind%Nss = Ctrl%Ind%Nss + 1
            Ctrl%Ind%ss_terms(i) = .true.
         else
            Ctrl%Ind%ss_terms(i) = .false.
         end if
      end do
   else
      ! As above, but for Rho variables
      if (Ctrl%Ind%NSolar > 0) &
           allocate(Ctrl%Ind%rho_terms(Ctrl%Ind%NSolar, MaxRho_XX))

      do j = 1, MaxRho_XX
         do i = 1, Ctrl%Ind%NSolar
            if (any(Ctrl%X == IRs(i,j))) then
               Ctrl%Ind%Nrho = Ctrl%Ind%Nrho + 1
               Ctrl%Ind%rho_terms(i,j) = .true.
            else
               Ctrl%Ind%rho_terms(i,j) = .false.
            end if
         end do
      end do
   end if

   if (Ctrl%Approach == AppCld1L .or. Ctrl%Approach == AppCld2L) then
      ! Output albedo in all solar channels
      Ctrl%Ind%Nalb = Ctrl%Ind%NSolar
      if (Ctrl%Ind%NSolar > 0) then
         allocate(Ctrl%Ind%alb_terms(Ctrl%Ind%NSolar))
         Ctrl%Ind%alb_terms = .true.
      end if

      ! Output cee in all thermal channels
      Ctrl%Ind%Ncee = Ctrl%Ind%NThermal
      if (Ctrl%Ind%NThermal > 0) then
         allocate(Ctrl%Ind%cee_terms(Ctrl%Ind%NThermal))
         Ctrl%Ind%cee_terms = .true.
      end if
   end if

   if (Ctrl%verbose) then
      write(*,*) 'Driver file: ',       trim(drifile)
      write(*,*) 'Input directory: ',   trim(Ctrl%FID%Data_Dir)
      write(*,*) 'Input filename: ',    trim(Ctrl%FID%Filename)
      write(*,*) 'Output directory: ',  trim(Ctrl%FID%Out_Dir)
      write(*,*) 'LUT directory: ',     trim(Ctrl%FID%SAD_Dir)
      write(*,*) 'Ctrl%FID%Data_Dir: ', trim(Ctrl%FID%Data_Dir)
      write(*,*) 'Ctrl%FID%Out_Dir: ',  trim(Ctrl%FID%Out_Dir)
      write(*,*) 'Ctrl%FID%SAD_Dir: ',  trim(Ctrl%FID%SAD_Dir)
      write(*,*) 'Ctrl%FID%Config: ',   trim(Ctrl%FID%Config)
      write(*,*) 'Ctrl%InstName: ',     trim(Ctrl%InstName)
      write(*,*) 'Number of channels expected in preproc files: ',Ctrl%Ind%NAvail
      write(*,*) 'channel flag from driver: ', Ctrl%Ind%channel_proc_flag
      write(*,*) 'Ny,NSolar,NThermal,NMixed: ', Ctrl%Ind%Ny, Ctrl%Ind%NSolar, &
           Ctrl%Ind%NThermal, Ctrl%Ind%NMixed
      write(*,*) 'Ctrl%Ind%ICh: ',          Ctrl%Ind%ICh
      write(*,*) 'Ctrl%Ind%Y_ID: ',         Ctrl%Ind%Y_ID
      if (Ctrl%Ind%NSolar > 0) &
           write(*,*) 'Ctrl%Ind%YSolar: ',       Ctrl%Ind%YSolar
      if (Ctrl%Ind%NThermal > 0) &
           write(*,*) 'Ctrl%Ind%YThermal: ',     Ctrl%Ind%YThermal
      if (Ctrl%Ind%NMixed > 0) &
           write(*,*) 'Ctrl%Ind%YMixed: ',       Ctrl%Ind%YMixed
      write(*,*) 'Ctrl%LUTClass: ',         trim(Ctrl%LUTClass)
      write(*,*) 'Ctrl%FID%L2_primary: ',   trim(Ctrl%FID%L2_primary)
      write(*,*) 'Ctrl%FID%L2_secondary: ', trim(Ctrl%FID%L2_secondary)
   end if


   !----------------------------------------------------------------------------
   ! Now do some input checks
   !----------------------------------------------------------------------------

   ! Check that the first-guess methods for all variables are legal in ORAC
   ! and supported. Not all legal values are supported for all variables.
   ! N.B. not all supported methods can be used in all conditions and this is
   ! NOT CHECKED here.

   do j = 1, MaxIllum ! loop over day, twi, night values for FG
      do i = 1, MaxStateVar
         select case (Ctrl%FG(i,j))
         case (SelmCtrl)
            continue

         case (SelmMeas)
            if (i == IRe .or. i == ITs) then
               write(*,*) 'ERROR: Read_Driver(): MDAD method not supported ' // &
                    'for setting first guess Re, Ts'
               stop FGMethErr
            end if

         case (SelmAux)
            if (i /= ITs .and. .not. any(i == ISP) .and. .not. any(i == IRs)) then
               write(*,*) 'ERROR: Read_Driver(): AUX method ONLY supported ' // &
                    'for setting first guess Ts, SP and Rs'
               stop FGMethErr
            end if
            if (any(i == ISP) .and. .not. Ctrl%RS%read_full_brdf) then
               write(*,*) 'ERROR: Read_Driver(): Setting SP by the AUX ' // &
                    'method requires Ctrl%RS%read_full_brdf = .true.'
            end if

         case default
            write(*,*) 'ERROR: Read_Driver(): Invalid method for ' // &
                 'first-guess state variable: ', i
            stop FGMethErr
         end select
      end do
   end do

   ! Check validity of a priori selection options. Not all legal values are
   ! supported for all variables.
   do j = 1, MaxIllum
      do i = 1, MaxStateVar
         select case (Ctrl%AP(i,j))
         case (SelmCtrl)
            continue

         case (SelmMeas)
            if (i == IRe .or. i == ITs) then
               write(*,*) 'ERROR: Read_Driver(): MDAD method not supported ' // &
                    'for setting a priori Re, Ts'
               stop APMethErr
            end if

         case (SelmAux)
            if (i /= ITs .and. .not. any(i == ISP) .and. .not. any(i == IRs)) then
               write(*,*) 'ERROR: Read_Driver(): AUX method ONLY supported ' // &
                    'for setting a priori Ts, Rs, and S'
               stop APMethErr
            end if
            if (any(i == ISP) .and. .not. Ctrl%RS%read_full_brdf) then
               write(*,*) 'ERROR: Read_Driver(): Setting SP by the AUX ' // &
                    'method requires Ctrl%RS%read_full_brdf = .true.'
            end if

         case default
            write(*,*) 'ERROR: Read_Driver(): Invalid method for a priori ' // &
                 'state variable ', i
            stop APMethErr
         end select
      end do
   end do

   ! Check validity of surface reflectance flag
   select case (Ctrl%RS%RsSelm)
   case (SelmCtrl)
      if (Ctrl%RS%use_full_brdf .and. Ctrl%Approach /= AppAerSw) then
         write(*,*) 'ERROR: Read_Driver(): Setting surface reflectance by '//  &
              'Ctrl method assumes a Lambertian surface and cannot be used '// &
              'alongside the full BRDF in cloud mode.'
         stop GetSurfaceMeth
      end if
      if (Ctrl%RS%SRsSelm /= SelmCtrl .and. Ctrl%RS%SRsSelm /= SelmMeas) then
         write(*,*) 'ERROR: Read_Driver(): Surface reflectance uncertainty '// &
              'must be set by Ctrl or Meas method when surface reflectance is.'
         stop GetSurfaceMeth
      end if
   case (SelmAux)
      if (Ctrl%RS%SRsSelm /= SelmCtrl .and. Ctrl%RS%SRsSelm /= SelmAux .and. &
           Ctrl%RS%SRsSelm /= SelmMeas) then
         write(*,*) 'ERROR: Read_Driver(): Surface reflectance uncertainty '// &
              ' method not supported.'
         stop GetSurfaceMeth
      end if
      if (Ctrl%RS%SRsSelm == SelmAux .and. .not. Ctrl%RS%use_full_brdf .and. &
           Ctrl%Approach /= AppAerSw) then
         write(*,*) 'ERROR: Read_Driver(): Full BRDF required with '//&
              'auxilliary surface uncertainties in cloud mode.'
         stop GetSurfaceMeth
      end if
      if (Ctrl%RS%SRsSelm == SelmCtrl .and. Ctrl%RS%add_fractional .and. &
           .not. Ctrl%RS%use_full_brdf .and. Ctrl%Approach /= AppAerSw) then
         write(*,*) 'ERROR: Read_Driver(): add_fractional is not currently '//&
              'functional for the Lambertian surface with SRsSelm == Ctrl.'
         stop GetSurfaceMeth
      end if
      if (Ctrl%RS%SRsSelm == SelmMeas .and. Ctrl%RS%add_fractional) then
         write(*,*) 'WARNING: Read_Driver(): add_fractional is ignored '//&
              'with SRsSelm == Meas.'
      end if
   case (SelmMeas)
      if (Ctrl%Approach /= AppAerSw) then
         write(*,*) 'ERROR: Read_Driver(): Surface reflectance method not '// &
              'supported in cloud mode.'
         stop GetSurfaceMeth
      else if (Ctrl%RS%SRsSelm == SelmAux) then
         write(*,*) 'ERROR: Read_Driver(): Surface reflectance uncertainty '// &
              'must be set by Ctrl or Meas method when surface reflectance is.'
         stop GetSurfaceMeth
      end if
   case default
      write(*,*) 'ERROR: Read_Driver(): Invalid surface reflectance method'
      stop GetSurfaceMeth
   end select

   ! For now, AerSw approach does not allow for non-Lambertian surface
   if (Ctrl%Class == ClsAerSw .and. Ctrl%RS%use_full_brdf) then
      write(*,*) 'ERROR: Read_Driver(): Use of the Swansea surface '// &
           'reflectance model with full BRDF equations supported via '// &
           'Ctrl%Class = ClsAerBR'
      stop GetSurfaceMeth
   end if

   ! The upper layer is not allowed to be below the lower layer.
   if (Ctrl%Approach == AppCld2L) then
      if (Ctrl%XB(IPc) .gt. Ctrl%XB(IPc2)) then
         write(*,*) 'ERROR: Read_Driver(): Ctrl%XB(IPc) cannot be greater than '// &
                    'Ctrl%XB(IPc2): ', Ctrl%XB(IPc), ' > ', Ctrl%XB(IPc2)
         stop error_stop_code
      end if
      if (Ctrl%X0(IPc) .gt. Ctrl%X0(IPc2)) then
         write(*,*) 'ERROR: Read_Driver(): Ctrl%X0(IPc) cannot be greater than '// &
                    'Ctrl%X0(IPc2): ', Ctrl%X0(IPc), ' > ', Ctrl%X0(IPc2)
         stop error_stop_code
      end if
   end if


   !----------------------------------------------------------------------------
   ! Clean up
   !----------------------------------------------------------------------------
   deallocate(channel_ids_instr)
   deallocate(channel_sw_flag)
   deallocate(channel_lw_flag)
   deallocate(channel_wvl)
   deallocate(channel_view)


   !----------------------------------------------------------------------------
   ! Dump Ctrl as a driver file.
   !----------------------------------------------------------------------------
   if (dumpfile /= '') then
      ! For first run set length argument to 0 for a dry run to get the size
      ! required for the buffer.
      size = print_ctrl(Ctrl, buffer, 0)

      allocate(buffer(size))

      ! For the second run the size is > 0 indicating a full run that will print
      ! to buffer.
      size = print_ctrl(Ctrl, buffer, size)

      if (dumpfile == '-') then
         ! Standard output
         dump_lun = output_unit
      else
         ! Output to file
         call find_lun(dump_lun)
         open(unit=dump_lun, file=dumpfile, status='replace', iostat=ios)
         if (ios /= 0) then
            write(*,*) 'ERROR: Read_Driver(): Unable to open dump file: ', &
                       trim(dumpfile)
            stop error_stop_code
         end if
      end if

      ! Write the driver file
      write(dump_lun, '(*(A))') '# ORAC Driver File'
      write(dump_lun, '(*(A))') buffer(1:size-1)

      if (dumpfile /= '-') then
         close(dump_lun)
      end if

      deallocate(buffer)

      call Dealloc_Ctrl(Ctrl)

      stop
   end if

end subroutine Read_Driver


!-------------------------------------------------------------------------------
! Name: switch
!
! Purpose:
! Character-efficient wrapper for switch case statements in Read_Driver.
! If additional values for Ctrl%Approach are added, they should be included here.
!
! Algorithm:
! If argument for current mode is set, return that value. If not but generic
! argument for current mode (i.e. Cld or Aer) is set, return that value.
! Otherwise, return default value.
!
! Arguments:
! Name    Type In/Out/Both Description
! ------------------------------------------------------------------------------
! a       int In           MANDATORY ARGUMENT. Value of Ctrl%Approach.
! Default any In           MANDATORY ARGUMENT. Value to return if the argument
!                          of this mode is not set.
! Cld     any In           Value for CldWat and CldIce approaches.
! CldWat  any In           Value for CldWat approach.
! CldIce  any In           Value for CldIce approach.
! Aer     any In           Value for AerOx and AerSw approaches.
! AerOx   any In           Value for AerOx approach.
! AerSw   any In           Value for AerSw approach.
! AshEyj  any In           Value for AshEyj approach.
! out     any Out          Return value.
!
! History:
! 2015/08/19, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
#define SWITCH_TYPE logical
#define SWITCH_NAME_APP switch_app_logic
#define SWITCH_NAME_CLS switch_cls_logic
#define SWITCH_FILL .false.
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME_APP
#undef SWITCH_NAME_CLS
#undef SWITCH_FILL

#define SWITCH_TYPE integer(kind=byte)
#define SWITCH_NAME_APP switch_app_byte
#define SWITCH_NAME_CLS switch_cls_byte
#define SWITCH_FILL byte_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME_APP
#undef SWITCH_NAME_CLS
#undef SWITCH_FILL

#define SWITCH_TYPE integer(kind=sint)
#define SWITCH_NAME_APP switch_app_sint
#define SWITCH_NAME_CLS switch_cls_sint
#define SWITCH_FILL sint_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME_APP
#undef SWITCH_NAME_CLS
#undef SWITCH_FILL

#define SWITCH_TYPE integer(kind=lint)
#define SWITCH_NAME_APP switch_app_lint
#define SWITCH_NAME_CLS switch_cls_lint
#define SWITCH_FILL lint_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME_APP
#undef SWITCH_NAME_CLS
#undef SWITCH_FILL

#define SWITCH_TYPE real(kind=sreal)
#define SWITCH_NAME_APP switch_app_sreal
#define SWITCH_NAME_CLS switch_cls_sreal
#define SWITCH_FILL sreal_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME_APP
#undef SWITCH_NAME_CLS
#undef SWITCH_FILL

#define SWITCH_TYPE real(kind=dreal)
#define SWITCH_NAME_APP switch_app_dreal
#define SWITCH_NAME_CLS switch_cls_dreal
#define SWITCH_FILL dreal_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME_APP
#undef SWITCH_NAME_CLS
#undef SWITCH_FILL

#define SWITCH_TYPE character(len=FilenameLen)
#define SWITCH_NAME_APP switch_app_char
#define SWITCH_NAME_CLS switch_cls_char
#define SWITCH_FILL ''
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME_APP
#undef SWITCH_NAME_CLS
#undef SWITCH_FILL

end module read_driver_m
