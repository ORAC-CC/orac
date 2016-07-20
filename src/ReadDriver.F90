!-------------------------------------------------------------------------------
! Name: ReadDriver.F90
!
! Purpose:
! Stores values required by driver file. This code is intended to replace
! the idl write_idriver.pro
!
! Description and Algorithm details:
! Reads the values from the "driver" file used to set run-time options into
! the Ctrl structure. Settings beyond the typical can be overriden in the
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
! 2012/05/15, CP: created original file to reapce ReadDriver
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
!    i.e matching temperature profile changed a priori errors of state vecto
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
! 2016/06/06, GM: Set Ctrl%RS%solar_factor and Ctrl%get_T_dv_from_T_0d based on
!    Ctrl%i_equation_form.  Change default value of Ctrl%i_equation_form to 3.
! 2016/06/05, SP: Updated to support Sentinel-3/SLSTR.
!
! $Id$
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

   interface switch
      module procedure &
           switch_logic, switch_byte,  switch_sint, switch_lint, &
           switch_sreal, switch_dreal, switch_char
   end interface switch
contains

#ifdef WRAPPER
subroutine Read_Driver(Ctrl, global_atts, source_atts, drifile)
#else
subroutine Read_Driver(Ctrl, global_atts, source_atts)
#endif

   use, intrinsic :: iso_fortran_env, only : input_unit

   use constants_cloud_typing_pavolonis_m
   use Ctrl_m
   use ECP_constants_m
   use global_attributes_m
   use parse_user_m
   use read_ctrl_m
   use read_utils_m
   use source_attributes_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),              intent(out)   :: Ctrl
   type(global_attributes_t), intent(inout) :: global_atts
   type(source_attributes_t), intent(inout) :: source_atts
#ifdef WRAPPER
   character(*),              intent(inout) :: drifile
#else
   character(FilenameLen)                   :: drifile
#endif

   ! Local variables
   integer                            :: i,ii,i0,i1,i2,j
   integer                            :: ios
   integer                            :: dri_lun
   character(FilenameLen)             :: root_filename
   character(FilenameLen)             :: outname, line
   logical                            :: file_exists
   integer, allocatable, dimension(:) :: channel_ids_instr
   integer, allocatable, dimension(:) :: channel_sw_flag, channel_lw_flag
   real,    allocatable, dimension(:) :: channel_wvl
   integer                            :: Nx_Dy, Nx_Tw, Nx_Ni
   integer                            :: NXJ_Dy, NXJ_Tw, NXJ_Ni
   integer, dimension(MaxStateVar)    :: X_Dy, X_Tw, X_Ni
   integer, dimension(MaxStateVar)    :: XJ_Dy, XJ_Tw, XJ_Ni
   integer                            :: a ! Short name for Ctrl%Approach
   real                               :: wvl_threshold
   logical                            :: new_driver_format


   call Nullify_Ctrl(Ctrl)


   ! Initialise some important variables
   Ctrl%verbose                = .true.
   Ctrl%Approach               = -1
   Ctrl%do_new_night_retrieval = .true.
   Ctrl%do_CTX_correction      = .true.


   !----------------------------------------------------------------------------
   ! Locate the driver file
   !----------------------------------------------------------------------------
#ifndef WRAPPER
   if (command_argument_count() == 1) then
      drifile = ''
      call get_command_argument(1, drifile)
   else
      call get_environment_variable("ORAC_TEXTIN", drifile)
   end if
#endif

   ! If drifile is '-' read the file from standard input otherwise read drifile
   if (drifile == '-') then
      dri_lun = input_unit
   else
      ! Check drifile exists
      inquire(file=drifile, exist=file_exists)
      if (.not. file_exists) then
         write(*,*) 'ERROR: ReadDriver(): Driver file pointed to by ' // &
                    'ORAC_DRIVER does not exist: ', trim(drifile)
         stop DriverFileNotFound
      end if

      ! Open the driver file
      call find_lun(dri_lun)
      open(unit=dri_lun, file=drifile, iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: ReadDriver(): Unable to open driver file: ', &
                    trim(drifile)
         stop DriverFileOpenErr
      end if
   end if

   ! Identify version of driver file
   read(dri_lun, '(A)', iostat=ios) line
   if (ios /= 0) then
      write(*,*) 'ERROR: ReadDriver(): Unable to read driver file: ', &
                 trim(drifile)
      stop DriverFileOpenErr
   end if
   rewind dri_lun

   new_driver_format = line(1:18) .eq. '# ORAC Driver File'

   !----------------------------------------------------------------------------
   ! Read the driver file
   !----------------------------------------------------------------------------
   if (new_driver_format) then
      close(dri_lun)
      call read_ctrl(drifile, Ctrl)
   else
      call old_driver_first_read(dri_lun, Ctrl)
   end if

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

   ! Output filenames
   outname=trim(Ctrl%FID%Out_Dir)//'/'//trim(Ctrl%FID%Filename)//trim(Ctrl%LUTClass)
   Ctrl%FID%L2_primary   = trim(outname)//'.primary.nc'
   Ctrl%FID%L2_secondary = trim(outname)//'.secondary.nc'
   Ctrl%FID%BkP          = trim(outname)//'.bkp'

   ! Read channel related info
   call read_config_file(Ctrl, channel_ids_instr, channel_sw_flag, &
     channel_lw_flag, channel_wvl, global_atts, source_atts)

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
   Ctrl%Ind%Ch_Is = 0
   allocate(Ctrl%Ind%YSolar(Ctrl%Ind%NSolar))
   allocate(Ctrl%Ind%YThermal(Ctrl%Ind%NThermal))
   allocate(Ctrl%Ind%YMixed(Ctrl%Ind%NMixed))
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
      if (Ctrl%LUTClass(1:3) == 'WAT') then
         Ctrl%Approach = CldWat
      else if (Ctrl%LUTClass(1:3) == 'ICE') then
         Ctrl%Approach = CldIce
      else if (Ctrl%LUTClass(1:3) == 'EYJ') then
         Ctrl%Approach = AshEyj
      else if (Ctrl%LUTClass(1:1) == 'A') then
         Ctrl%Approach = AerOx
      else
         write(*,*) 'ERROR: Read_Driver(): Cannot determine retrieval '// &
              'approach from LUTClass. Please set Ctrl%APPROACH.'
         stop error_stop_code
      end if
   end if

   ! Use a short name for Ctrl%Approach
   a = Ctrl%Approach

   !----------------------------------------------------------------------------
   ! Set default values of the Ctrl structure
   !----------------------------------------------------------------------------
   Ctrl%Run_ID = 'none'

   !----------------------- Ctrl%RS -----------------------
   Ctrl%RS%RsSelm         = switch(a, Default=SelmAux)
   Ctrl%RS%SRsSelm        = switch(a, Default=SelmMeas, Aer=SelmCtrl)
   Ctrl%RS%use_full_brdf  = switch(a, Default=.true.,   AerSw=.false.)
   Ctrl%RS%Cb             = switch(a, Default=0.2,      Aer=0.4)
   Ctrl%RS%add_fractional = switch(a, Default=.false.,  AerOx=.true.)
   Ctrl%RS%diagonal_SRs   = switch(a, Default=.false.,  AerOx=.true.)

   ! Select assumed surface reflectance based on wavelength
   Ctrl%RS%allow_a_default_surface = .true.

   wvl_threshold = 0.05
   allocate(Ctrl%RS%B(Ctrl%Ind%NSolar, MaxSurf))
   do i = 1, Ctrl%Ind%NSolar
      ii = Ctrl%Ind%ICh(Ctrl%Ind%YSolar(i))

      if (abs(channel_wvl(ii) - 0.55) < wvl_threshold) then      ! AATSR Ch1
         Ctrl%RS%B(i,ISea)  = switch(a, Default=0.05, Aer=0.01)
         Ctrl%RS%B(i,ILand) = switch(a, Default=0.15, Aer=0.20)
      else if (abs(channel_wvl(ii) - 0.67) < wvl_threshold) then ! AATSR Ch2
         Ctrl%RS%B(i,ISea)  = switch(a, Default=0.02, Aer=0.005)
         Ctrl%RS%B(i,ILand) = switch(a, Default=0.10, Aer=0.15)
      else if (abs(channel_wvl(ii) - 0.87) < wvl_threshold) then ! AATSR Ch3
         Ctrl%RS%B(i,ISea)  = switch(a, Default=0.01, Aer=0.0001)
         Ctrl%RS%B(i,ILand) = switch(a, Default=0.01, Aer=0.10)
      else if (abs(channel_wvl(ii) - 1.6) < wvl_threshold) then  ! AATSR Ch4
         Ctrl%RS%B(i,ISea)  = switch(a, Default=0.01, Aer=0.00)
         Ctrl%RS%B(i,ILand) = switch(a, Default=0.01, Aer=0.01)
      else ! No opinion on channel
         Ctrl%RS%B(i,:)     = 0.0
      end if
   end do

   allocate(Ctrl%RS%Sb(Ctrl%Ind%NSolar, MaxSurf))
   Ctrl%RS%Sb(:,ISea)  = switch(a, Default=0.2, AerOx=0.0)
   Ctrl%RS%Sb(:,ILand) = switch(a, Default=0.2, AerOx=2e-4)

   !----------------------- Ctrl%EqMPN --------------------
   Ctrl%EqMPN%SySelm = switch(a, Default=SelmAux)
   Ctrl%EqMPN%Homog  = switch(a, Default=.true.,  Aer=.false.)
   Ctrl%EqMPN%Coreg  = switch(a, Default=.true.,  Aer=.false.)

   !----------------------- Ctrl%Invpar -------------------
   Ctrl%Invpar%ConvTest           = switch(a, Default=.false., Aer=.true.)
   Ctrl%Invpar%MqStart            = switch(a, Default=0.001)
   Ctrl%Invpar%MqStep             = switch(a, Default=10.0)
   Ctrl%Invpar%MaxIter            = switch(a, Default=40,      Aer=25)
   Ctrl%Invpar%Ccj                = switch(a, Default=0.05,    AerSw=0.001)
   Ctrl%Invpar%always_take_GN     = switch(a, Default=.false., AerOx=.true.)
   Ctrl%Invpar%dont_iter_convtest = switch(a, Default=.false., Aer=.true.)
   Ctrl%Invpar%disable_Ss         = switch(a, Default=.false., Aer=.true.)

   ! Scale used to avoid pivots in matrix inversion. Terms that a mode shouldn't
   ! touch are zeroed to
   ! NOTES: 1) Aerosol LUTs use log Re while cloud uses linear Re.
   ! 2) For the Swansea retrieval, IRho_DD is equivalent to ISS.
   Ctrl%Invpar%XScale(ITau)           = switch(a, Default=10.0, AerSw=1.0)
   Ctrl%Invpar%XScale(IRe)            = switch(a, Default=1.0,  AerOx=10.0)
   Ctrl%Invpar%XScale(IPc)            = switch(a, Default=1.0)
   Ctrl%Invpar%XScale(IFr)            = switch(a, Default=1000.0)
   Ctrl%Invpar%XScale(ITs)            = switch(a, Default=1.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_0V)) = switch(a, Default=1.0,  AerOx=1000.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_0D)) = switch(a, Default=1.0,  AerOx=1000.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_DV)) = switch(a, Default=1.0,  AerOx=1000.0)
   Ctrl%Invpar%XScale(IRs(:,IRho_DD)) = switch(a, Default=1.0,  AerOx=1000.0)
   Ctrl%Invpar%XScale(ISP)            = switch(a, Default=1.0)
   ! Lower limit
   Ctrl%Invpar%XLLim(ITau)            = switch(a, Default=-3.0, Aer=-2.0)
   Ctrl%Invpar%XLLim(IRe)             = switch(a, Default=0.1,  Aer=-2.0, &
                                                                AshEyj=0.01)
   Ctrl%Invpar%XLLim(IPc)             = switch(a, Default=10.0)
   Ctrl%Invpar%XLLim(IFr)             = switch(a, Default=0.0)
   Ctrl%Invpar%XLLim(ITs)             = switch(a, Default=250.0)
   Ctrl%Invpar%XLLim(IRs(:,IRho_0V))  = switch(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(IRs(:,IRho_0D))  = switch(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(IRs(:,IRho_DV))  = switch(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(IRs(:,IRho_DD))  = switch(a, Default=0.00001, Cld=0.)
   Ctrl%Invpar%XLLim(ISP)             = switch(a, Default=0.00001)
   ! Upper limit
   Ctrl%Invpar%XULim(ITau)            = switch(a, Default=2.408, Aer=0.7)
   Ctrl%Invpar%XULim(IRe)             = switch(a, Default=1.0,   AshEyj=20.0, &
                                                                 CldWat=35.0, &
                                                                 CldIce=100.0)
   Ctrl%Invpar%XULim(IPc)             = switch(a, Default=1200.0)
   Ctrl%Invpar%XULim(IFr)             = switch(a, Default=1.0)
   if ((Ctrl%InstName(1:5) .eq. 'AATSR') .or. (Ctrl%InstName(1:5) .eq. 'ATSR2')) then
      Ctrl%Invpar%XULim(ITs)             = switch(a, Default=320.0)
   else
      Ctrl%Invpar%XULim(ITs)             = switch(a, Default=400.0)
   endif
   Ctrl%Invpar%XULim(IRs(:,IRho_0V))  = switch(a, Default=1.0,   AerSw=100.0)
   Ctrl%Invpar%XULim(IRs(:,IRho_0D))  = switch(a, Default=1.0)
   Ctrl%Invpar%XULim(IRs(:,IRho_DV))  = switch(a, Default=1.0)
   Ctrl%Invpar%XULim(IRs(:,IRho_DD))  = switch(a, Default=1.0)
   Ctrl%Invpar%XULim(ISP)             = switch(a, Default=100.0)

   !----------------------- Ctrl%QC -----------------------
   Ctrl%QC%MaxJ                 = switch(a, Default=100.0, Aer=4.0)
   Ctrl%QC%MaxS(ITau)           = switch(a, Default=0.08)
   Ctrl%QC%MaxS(IRe)            = switch(a, Default=3.0)
   Ctrl%QC%MaxS(IPc)            = switch(a, Default=200.0)
   Ctrl%QC%MaxS(IFr)            = switch(a, Default=0.2)
   Ctrl%QC%MaxS(ITs)            = switch(a, Default=2.0)
   Ctrl%QC%MaxS(IRs(:,IRho_0V)) = switch(a, Default=0.2,   AerSw=10.0)
   Ctrl%QC%MaxS(IRs(:,IRho_0D)) = switch(a, Default=0.2) ! No idea of a sensible
   Ctrl%QC%MaxS(IRs(:,IRho_DV)) = switch(a, Default=0.2) ! value for these
   Ctrl%QC%MaxS(IRs(:,IRho_DD)) = switch(a, Default=0.2)
   Ctrl%QC%MaxS(ISP)            = switch(a, Default=10.0)

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
   Ctrl%i_equation_form = switch(a, Default=3, AerOx=1, AerSw=0)
   Ctrl%LUTIntSelm      = switch(a, Default=LUTIntMethLinear)
   Ctrl%RTMIntSelm      = switch(a, Default=RTMIntMethLinear, Aer=RTMIntMethNone)
   Ctrl%CloudType       = switch(a, Default=1,                Aer=2)
   Ctrl%Bkpl                = 3
   Ctrl%Max_SDAD            = 10.0
   Ctrl%sabotage_inputs     = .false.
   Ctrl%process_cloudy_only = .false.
   Ctrl%surfaces_to_skip    = 0_byte
   Ctrl%second_aot_ch       = 3 ! Assuming AATSR

   ! Set cloud types to process depending on requested LUT
   Ctrl%Types_to_process = byte_fill_value
   if (Ctrl%Approach == CldWat) then
      Ctrl%NTypes_to_process   = 3
      Ctrl%Types_to_process(1) = FOG_TYPE
      Ctrl%Types_to_process(2) = WATER_TYPE
      Ctrl%Types_to_process(3) = SUPERCOOLED_TYPE
      if (.NOT. Ctrl%process_cloudy_only) then
         Ctrl%NTypes_to_process   = 5
         Ctrl%Types_to_process(4) = CLEAR_TYPE
         Ctrl%Types_to_process(5) = PROB_CLEAR_TYPE
      end if
   else if (Ctrl%Approach == CldIce) then
      Ctrl%NTypes_to_process   = 4
      Ctrl%Types_to_process(1) = OPAQUE_ICE_TYPE
      Ctrl%Types_to_process(2) = CIRRUS_TYPE
      Ctrl%Types_to_process(3) = OVERLAP_TYPE
      Ctrl%Types_to_process(4) = PROB_OPAQUE_ICE_TYPE
      if (.NOT. Ctrl%process_cloudy_only) then
         Ctrl%NTypes_to_process   = 6
         Ctrl%Types_to_process(5) = CLEAR_TYPE
         Ctrl%Types_to_process(6) = PROB_CLEAR_TYPE
      end if
   else
      ! Accept everything
      Ctrl%NTypes_to_process   = MaxTypes
      Ctrl%Types_to_process    = [(i-1, i = 1, MaxTypes)]
   end if

   !---------------- Ctrl INDEXING CHANNELS ---------------
   ! See Ctrl.F90 for descriptions of the variables initialized below.
   Ctrl%Ind%Y_Id_legacy = 0

   if (Ctrl%InstName(1:5) .eq. 'AATSR' .or. Ctrl%InstName(1:5) .eq. 'ATSR2') then
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
   else if (Ctrl%InstName(1:3) .eq. 'AHI') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 4
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 7
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 14
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 15

      allocate(Ctrl%ReChans(3))
      Ctrl%ReChans = (/ 7, 5, 6 /)

      allocate(Ctrl%tau_chans(4))
      Ctrl%tau_chans = (/ 1, 2, 3, 4 /)
      allocate(Ctrl%r_e_chans(3))
      Ctrl%r_e_chans = (/ 5, 6, 7 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 7, 14, 15 /)
   else if (Ctrl%InstName(1:5) .eq. 'AVHRR') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 1
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 4
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 6

      allocate(Ctrl%ReChans(2))
      if (Ctrl%InstName(7:12) .eq. 'NOAA17') then
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
   else if (Ctrl%InstName(1:5) .eq. 'MODIS') then
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
   else if (Ctrl%InstName(1:6) .eq. 'SEVIRI') then
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
   else if (Ctrl%InstName(1:5) .eq. 'VIIRS') then
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
   else if (Ctrl%InstName(1:10) .eq. 'SLSTR-Sen3') then
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_6x) = 2
      Ctrl%Ind%Y_Id_legacy(I_legacy_0_8x) = 3
      Ctrl%Ind%Y_Id_legacy(I_legacy_1_6x) = 5
      Ctrl%Ind%Y_Id_legacy(I_legacy_3_xx) = 7
      Ctrl%Ind%Y_Id_legacy(I_legacy_11_x) = 8
      Ctrl%Ind%Y_Id_legacy(I_legacy_12_x) = 9

      allocate(Ctrl%ReChans(4))
      Ctrl%ReChans = (/ 7, 5, 6, 4 /)

      allocate(Ctrl%tau_chans(3))
      Ctrl%tau_chans = (/ 1, 2, 3 /)
      allocate(Ctrl%r_e_chans(4))
      Ctrl%r_e_chans = (/ 4, 5, 6, 7 /)
      allocate(Ctrl%ir_chans(3))
      Ctrl%ir_chans  = (/ 7, 8, 9 /)
   else
      write(*,*)"Unrecognised sensor/platform:",trim(Ctrl%InstName)
      stop
   end if

   !---------------- Ctrl STATE VECTOR SELM ---------------
   ! Select the manner by which the a priori (AP) and first guess (FG) values
   ! are set before the retrieval.
   ! SelmCtrl) Use constant values prescribed in this routine.
   ! SelmAux)  Use values from some manner of auxiliary data file.
   ! SelmMeas) Deduce values from the measurement vector (see XMDAD).

   ! Currently these are identical for all illuminations. To be different, make
   ! three copies of the line and replace : with IDay, ITwi, and INight.
   Ctrl%AP(ITau,:)           = switch(a, Default=SelmCtrl)
   Ctrl%AP(IRe,:)            = switch(a, Default=SelmCtrl)
   Ctrl%AP(IPc,:)            = switch(a, Default=SelmCtrl)
   Ctrl%AP(IFr,:)            = switch(a, Default=SelmMeas)
   Ctrl%AP(ITs,:)            = switch(a, Default=SelmAux)
   Ctrl%AP(IRs(:,IRho_0V),:) = switch(a, Default=SelmAux, AerSw=SelmCtrl)
   Ctrl%AP(IRs(:,IRho_0D),:) = switch(a, Default=SelmAux)
   Ctrl%AP(IRs(:,IRho_DV),:) = switch(a, Default=SelmAux)
   Ctrl%AP(IRs(:,IRho_DD),:) = switch(a, Default=SelmAux)
   Ctrl%AP(ISP,:)            = switch(a, Default=SelmCtrl)
   ! NOTES: 1) The aerosol code used SelmSAD for Tau and Re, which drew the
   !    values from a separate driver file. This must now be managed by the
   !    calling script setting Ctrl%XB and X0, as that knows what the LUT is.
   ! 2) Fr uses SelmMeas so the error is set to MDADErrF. Could be tidier.

   Ctrl%FG(ITau,:)           = switch(a, Default=SelmCtrl)
   Ctrl%FG(IRe,:)            = switch(a, Default=SelmCtrl)
   Ctrl%FG(IPc,:)            = switch(a, Default=SelmMeas)
   Ctrl%FG(IFr,:)            = switch(a, Default=SelmCtrl)
   Ctrl%FG(ITs,:)            = switch(a, Default=SelmAux)
   Ctrl%FG(IRs(:,IRho_0V),:) = switch(a, Default=SelmAux, AerSw=SelmCtrl)
   Ctrl%FG(IRs(:,IRho_0D),:) = switch(a, Default=SelmAux)
   Ctrl%FG(IRs(:,IRho_DV),:) = switch(a, Default=SelmAux)
   Ctrl%FG(IRs(:,IRho_DD),:) = switch(a, Default=SelmAux)
   Ctrl%FG(ISP,:)            = switch(a, Default=SelmCtrl)
   ! 3) Not sure why Fr is now SelmCtrl.

   !----------- Ctrl PRESCRIBED STATE VECTORS -------------
   ! A priori values
   Ctrl%XB(ITau)           = switch(a, Default=0.8,   AerOx=-1.5,  AerSw=-0.3, &
                                                      AshEyj=0.18)
   Ctrl%XB(IRe)            = switch(a, Default=-0.07, AshEyj=0.7, &
                                                      CldWat=12.,  CldIce=30.)
   Ctrl%XB(IPc)            = switch(a, Default=900.,  AshEyj=600., CldIce=400.)
   Ctrl%XB(IFr)            = switch(a, Default=1.0)
   Ctrl%XB(ITs)            = switch(a, Default=300.0)
   Ctrl%XB(IRs(:,IRho_0V)) = switch(a, Default=0.01,  AerSw=0.1)
   Ctrl%XB(IRs(:,IRho_0D)) = switch(a, Default=0.01)
   Ctrl%XB(IRs(:,IRho_DV)) = switch(a, Default=0.01)
   Ctrl%XB(IRs(:,IRho_DD)) = switch(a, Default=0.01)
   Ctrl%XB(ISP)            = switch(a, Default=0.3)
   ! First guess values
   Ctrl%X0(ITau)           = switch(a, Default=0.8,   AerOx=-1.5,  AerSw=-0.3, &
                                                      AshEyj=0.18)
   Ctrl%X0(IRe)            = switch(a, Default=-0.07, AshEyj=0.7, &
                                                      CldWat=12.,  CldIce=30.)
   Ctrl%X0(IPc)            = switch(a, Default=900.,  AshEyj=600., CldIce=400.)
   Ctrl%X0(IFr)            = switch(a, Default=1.0)
   Ctrl%X0(ITs)            = switch(a, Default=300.0)
   Ctrl%X0(IRs(:,IRho_0V)) = switch(a, Default=0.01,  AerSw=0.5)
   Ctrl%X0(IRs(:,IRho_0D)) = switch(a, Default=0.01)
   Ctrl%X0(IRs(:,IRho_DV)) = switch(a, Default=0.01)
   Ctrl%X0(IRs(:,IRho_DD)) = switch(a, Default=0.01)
   Ctrl%X0(ISP)            = switch(a, Default=0.3)
   ! A priori uncertainty
   Ctrl%Sx(ITau)           = switch(a, Default=1.0e+08, Aer=2.0)
   Ctrl%Sx(IRe)            = switch(a, Default=1.0e+08, Aer=0.5)
   Ctrl%Sx(IPc)            = switch(a, Default=1.0e+08)
   Ctrl%Sx(IFr)            = switch(a, Default=1.0e+08)
   Ctrl%Sx(ITs)            = switch(a, Default=1.0e+08)
   Ctrl%Sx(IRs(:,IRho_0V)) = switch(a, Default=1.0e+08, AerSw=1.0)
   Ctrl%Sx(IRs(:,IRho_0D)) = switch(a, Default=1.0e+08)
   Ctrl%Sx(IRs(:,IRho_DV)) = switch(a, Default=1.0e+08)
   Ctrl%Sx(IRs(:,IRho_DD)) = switch(a, Default=1.0e+08, AerOx=0.05)
   Ctrl%Sx(ISP(1))         = switch(a, Default=1.0e+08, AerSw=0.01)
   Ctrl%Sx(ISP(2:))        = switch(a, Default=1.0e+08, AerSw=0.5)
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

   !------------- Ctrl STATE VECTOR INDEXING --------------
   ! These arrays specify which variables should be retrieved in each
   ! illumination condition (by the index of that variable; see ECP_constants_m).
   X_Dy  = sint_fill_value
   X_Tw  = sint_fill_value
   X_Ni  = sint_fill_value
   XJ_Dy = sint_fill_value
   XJ_Tw = sint_fill_value
   XJ_Ni = sint_fill_value
   if (Ctrl%Approach == AerOx) then
      ! Retrieve optical depth, effective radius, and white sky albedo in all
      ! channels (it'll work out which duplicate views later). No night/twilight.
      Nx_Dy   = 2
      X_Dy(1) = ITau
      X_Dy(2) = IRe
      do i = 1, Ctrl%Ind%NSolar
         Nx_Dy = Nx_Dy+1
         X_Dy(Nx_Dy) = IRs(i,IRho_DD)
      end do
      Nx_Tw = 0
      Nx_Ni = 0

      ! No terms added to the Jacobian
      NXJ_Dy = 0
      NXJ_Tw = 0
      NXJ_Ni = 0
   else if (Ctrl%Approach == AerSw) then
      ! Retrieve optical depth, effective radius, and Swansea parameters in all
      ! channels (it'll work out which duplicate views later). No night/twilight.
      Nx_Dy   = 2
      X_Dy(1) = ITau
      X_Dy(2) = IRe
      do i = 1, Ctrl%Ind%NSolar
         Nx_Dy = Nx_Dy+1
         X_Dy(Nx_Dy) = ISS(i)
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
   else
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
         NXJ_Ni  = NXJ_Dy
         XJ_Ni   = XJ_Dy
      else
         ! At night retrieve optical depth, effective radius, cloud top pressure
         ! and surface temperature.
         Nx_Ni    = 4
         X_Ni(1)  = ITau
         X_Ni(2)  = IRe
         X_Ni(3)  = IPc
         X_Ni(4)  = ITs
         NXJ_Ni   = NXJ_Dy
         XJ_Ni    = XJ_Dy
      end if
   end if

   Ctrl%CTP_correction_limit = 100.


   !----------------------------------------------------------------------------
   ! Consider optional lines of driver file
   !----------------------------------------------------------------------------

   if (new_driver_format) then
      call read_ctrl(drifile, Ctrl)
   else
      call old_driver_second_read(dri_lun, Ctrl, Nx_Dy, Nx_Tw, Nx_Ni, NXJ_Dy, &
           NXJ_Tw, NXJ_Ni, X_Dy, X_Tw, X_Ni, XJ_Dy, XJ_Tw, XJ_Ni)
      if (drifile /= '-') then
         close(unit=dri_lun)
      end if
   end if


   ! ---------------------------------------------------------------------------
   ! Things that have to be after the optional lines
   ! ---------------------------------------------------------------------------

   if (Ctrl%RS%use_full_brdf .and. Ctrl%i_equation_form == 0) then
      write(*,*) 'ERROR: ReadDriver(): Ctrl%RS%use_full_brdf = true cannot ' // &
                 'be used with i_equation_form = 0'
      stop error_stop_code
   end if

   if (.not. Ctrl%RS%use_full_brdf .and. Ctrl%i_equation_form > 0) then
      Ctrl%i_equation_form = 0
!     write(*,*) 'ERROR: ReadDriver(): Ctrl%RS%use_full_brdf = false cannot ' // &
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
      write(*,*) 'ERROR: ReadDriver(): Invalid Ctrl%i_equation_form: ', &
                 Ctrl%i_equation_form
      stop error_stop_code
   end select

   ! T_dv (or TD using the old naming convention) is not correctly produced in
   ! either RAL's or GT's LUT code.  It should approach zero as the optical
   ! thickness approaches zero as scattering is required to scatter diffuse
   ! radiation into the satellite viewing angle beam.  In the current LUTs it
   ! approaches unity instead.  Analogously, T_0d (or TFBD using the old naming
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
   if (Ctrl%i_equation_form .eq. 3 .or. Ctrl%i_equation_form .eq. 4) then
      Ctrl%get_T_dv_from_T_0d = .true.
   else
      Ctrl%get_T_dv_from_T_0d = .false.
   end if

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
      write(*,*) 'Ctrl%Ind%YSolar: ',       Ctrl%Ind%YSolar
      write(*,*) 'Ctrl%Ind%YThermal: ',     Ctrl%Ind%YThermal
      write(*,*) 'Ctrl%Ind%YMixed: ',       Ctrl%Ind%YMixed
      write(*,*) 'Ctrl%LUTClass: ',         trim(Ctrl%LUTClass)
      write(*,*) 'Ctrl%FID%L2_primary: ',   trim(Ctrl%FID%L2_primary)
      write(*,*) 'Ctrl%FID%L2_secondary: ', trim(Ctrl%FID%L2_secondary)
      write(*,*) 'Ctrl%FID%BkP: ',          trim(Ctrl%FID%BkP)
   end if

   !----------------------------------------------------------------------------
   ! Now do some checks
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
            if (i /= ITs .and. .not. any(i == IRs)) then
               write(*,*) 'ERROR: Read_Driver(): AUX method ONLY supported ' // &
                    'for setting first guess Ts and Rs'
               stop FGMethErr
            end if

         case default
            write(*,*) 'ERROR: Read_Driver(): Invalid method ', &
                 'for first-guess state variable ',i
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
            if (i /= ITs .and. .not. any(i == IRs)) then
               write(*,*) 'ERROR: Read_Driver(): AUX method ONLY supported ' // &
                    'for setting a priori Ts and Rs'
               stop APMethErr
            end if

         case default
            write(*,*) 'ERROR: Read_Driver(): Invalid method for a priori ' // &
                 'state variable ',i
            stop APMethErr
         end select
      end do
   end do

   ! Check validity of surface reflectance flag
   select case (Ctrl%RS%RsSelm)
   case (SelmCtrl)
      if (Ctrl%RS%use_full_brdf) then
         write(*,*) 'ERROR: Read_Driver(): Setting surface reflectance by '//  &
              'Ctrl method assumes a Lambertian surface and cannot be used '// &
              'alongside the full BRDF.'
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
         write(*,*) 'ERROR: Read_Driver(): surface reflectance uncertainty '// &
              ' method not supported.'
         stop GetSurfaceMeth
      end if
      if (Ctrl%RS%SRsSelm == SelmAux .and. .not. Ctrl%RS%use_full_brdf) then
         write(*,*) 'ERROR: Read_Driver(): Full BRDF required with '//&
              'auxilliary surface uncertainties.'
         stop GetSurfaceMeth
      end if
      if (Ctrl%RS%SRsSelm == SelmCtrl .and. Ctrl%RS%add_fractional .and. &
           .not. Ctrl%RS%use_full_brdf) then
         write(*,*) 'ERROR: Read_Driver(): add_fractional is not currently '//&
              'functional for the Lambertian surface with SRsSelm == Ctrl.'
         stop GetSurfaceMeth
      end if
      if (Ctrl%RS%SRsSelm == SelmMeas .and. Ctrl%RS%add_fractional) then
         write(*,*) 'WARNING: Read_Driver(): add_fractional is ignored '//&
              'with SRsSelm == Meas.'
      end if
   case (SelmMeas)
      write(*,*) 'ERROR: Read_Driver(): surface reflectance method not supported'
      stop GetSurfaceMeth
   case default
      write(*,*) 'ERROR: Read_Driver(): invalid surface reflectance method'
      stop GetSurfaceMeth
   end select

   ! For now, AerSw approach does not allow for non-Lambertian surface
   if (Ctrl%Approach == AerSw .and. Ctrl%RS%use_full_brdf) then
      write(*,*) 'ERROR: Read_Driver(): Use of the Swansea surface '// &
           'reflectance model and full BRDF equations not supported'
      stop GetSurfaceMeth
   end if

   ! Clean up

   deallocate(channel_ids_instr)
   deallocate(channel_sw_flag)
   deallocate(channel_lw_flag)
   deallocate(channel_wvl)

end subroutine Read_Driver


! handle_parse_error (h_p_e)
subroutine h_p_e(label)

   use ECP_constants_m

   implicit none

   character(len=*), intent(in) :: label

   write(*,*) 'ERROR: ReadDriver(): Error parsing value for: ',trim(label)

   stop error_stop_code

end subroutine h_p_e


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
#define SWITCH_NAME switch_logic
#define SWITCH_FILL .false.
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME
#undef SWITCH_FILL

#define SWITCH_TYPE integer(kind=byte)
#define SWITCH_NAME switch_byte
#define SWITCH_FILL byte_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME
#undef SWITCH_FILL

#define SWITCH_TYPE integer(kind=sint)
#define SWITCH_NAME switch_sint
#define SWITCH_FILL sint_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME
#undef SWITCH_FILL

#define SWITCH_TYPE integer(kind=lint)
#define SWITCH_NAME switch_lint
#define SWITCH_FILL lint_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME
#undef SWITCH_FILL

#define SWITCH_TYPE real(kind=sreal)
#define SWITCH_NAME switch_sreal
#define SWITCH_FILL sreal_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME
#undef SWITCH_FILL

#define SWITCH_TYPE real(kind=dreal)
#define SWITCH_NAME switch_dreal
#define SWITCH_FILL dreal_fill_value
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME
#undef SWITCH_FILL

#define SWITCH_TYPE character(len=FilenameLen)
#define SWITCH_NAME switch_char
#define SWITCH_FILL ''
#include "switch.inc"
#undef SWITCH_TYPE
#undef SWITCH_NAME
#undef SWITCH_FILL

subroutine old_driver_first_read(dri_lun, Ctrl)

   use Ctrl_m
   use parse_user_m

   implicit none

   integer,      intent(in)    :: dri_lun
   type(Ctrl_t), intent(inout) :: Ctrl

   character(FilenameLen) :: line, label
   integer                :: i

   ! Read folder paths
   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%FID%Data_Dir) /= 0) call h_p_e('Ctrl%FID%Data_Dir')

   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%FID%Filename) /= 0) call h_p_e('Ctrl%FID%Filename')

   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%FID%Out_Dir) /= 0) call h_p_e('Ctrl%FID%Out_Dir')

   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%FID%SAD_Dir) /= 0) call h_p_e('Ctrl%FID%SAD_Dir')

   ! Read name of instrument
   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%InstName) /= 0) call h_p_e('Ctrl%InstName')

   ! Number of channels in preprocessing file
   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%Ind%NAvail) /= 0) &
      call h_p_e('number of channels expected in preproc files')

   ! Read processing flag from driver
   allocate(Ctrl%Ind%channel_proc_flag(Ctrl%Ind%NAvail))
   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%Ind%channel_proc_flag) /= 0) call h_p_e('channel flags')
   if (sum(Ctrl%Ind%channel_proc_flag) < 1 .or. &
       sum(Ctrl%Ind%channel_proc_flag) > Ctrl%Ind%NAvail .or. &
       any(Ctrl%Ind%channel_proc_flag /= 0 .and. Ctrl%Ind%channel_proc_flag /= 1)) then
      write(*,*) 'ERROR: ReadDriver(): channel flag from driver wrong: ', &
                 Ctrl%Ind%channel_proc_flag
      stop DriverFileIncompat
   end if

   ! Read in cloud class (aka phase of no aerosols processed)
   if (parse_driver(dri_lun, line) /= 0 .or. &
       parse_string(line, Ctrl%LUTClass) /= 0) &
      call h_p_e('Ctrl%LUTClass')

   do while (parse_driver(dri_lun, line, label) == 0)
      call clean_driver_label(label)
      select case (label)
      case('CTRL%APPROACH')
         if (parse_user_text(line, Ctrl%Approach) /= 0) call h_p_e(label)
      case('CTRL%DO_NEW_NIGHT_RETRIEVAL')
         if (parse_string(line, Ctrl%do_new_night_retrieval) /= 0) call h_p_e(label)
      case('CTRL%DO_CTX_CORRECTION')
         if (parse_string(line, Ctrl%do_CTX_correction) /= 0) call h_p_e(label)
      case('CTRL%VERBOSE')
         if (parse_string(line, Ctrl%verbose)           /= 0) call h_p_e(label)
      case default
         cycle
      end select
   end do

   ! Done with first pass through driver file.  Move file position to the end of
   ! the mandatory arguments which is the beginning of the optional arguments.
   rewind dri_lun
   do i = 1, 8
      read(dri_lun, *)
   end do

end subroutine old_driver_first_read

subroutine old_driver_second_read(dri_lun, Ctrl, Nx_Dy, Nx_Tw, Nx_Ni, NXJ_Dy, &
   NXJ_Tw, NXJ_Ni, X_Dy, X_Tw, X_Ni, XJ_Dy, XJ_Tw, XJ_Ni)

   use Ctrl_m
   use parse_user_m

   implicit none

   integer,      intent(in)                            :: dri_lun
   type(Ctrl_t), intent(inout)                         :: Ctrl
   integer,      intent(inout)                         :: Nx_Dy, Nx_Tw, Nx_Ni
   integer,      intent(inout)                         :: NXJ_Dy, NXJ_Tw, NXJ_Ni
   integer,      intent(inout), dimension(MaxStateVar) :: X_Dy, X_Tw, X_Ni
   integer,      intent(inout), dimension(MaxStateVar) :: XJ_Dy, XJ_Tw, XJ_Ni

   character(FilenameLen) :: line, label
   integer, allocatable   :: solar_ids(:)

   ! Array temporary needed for human-readable solar channel indexing
   allocate(solar_ids(Ctrl%Ind%NSolar))
   solar_ids = Ctrl%Ind%Y_ID(Ctrl%Ind%YSolar)

   ! Process optional lines of driver file
   do while (parse_driver(dri_lun, line, label) == 0)
      call clean_driver_label(label)
      select case (label)
      case('CTRL%FID%DATA_DIR','Ctrl%DATA_DIR')
         if (parse_string(line, Ctrl%FID%Data_Dir)     /= 0) call h_p_e(label)
      case('CTRL%FID%OUT_DIR','Ctrl%OUT_DIR')
         if (parse_string(line, Ctrl%FID%Out_Dir)      /= 0) call h_p_e(label)
      case('CTRL%FID%SAD_DIR','Ctrl%SAD_DIR')
         if (parse_string(line, Ctrl%FID%SAD_Dir)      /= 0) call h_p_e(label)
      case('CTRL%FID%MSI')
         if (parse_string(line, Ctrl%FID%MSI)          /= 0) call h_p_e(label)
      case('CTRL%FID%LWRTM')
         if (parse_string(line, Ctrl%FID%LWRTM)        /= 0) call h_p_e(label)
      case('CTRL%FID%SWRTM')
         if (parse_string(line, Ctrl%FID%SWRTM)        /= 0) call h_p_e(label)
      case('CTRL%FID%PRTM')
         if (parse_string(line, Ctrl%FID%PRTM)         /= 0) call h_p_e(label)
      case('CTRL%FID%LS')
         if (parse_string(line, Ctrl%FID%LS)           /= 0) call h_p_e(label)
      case('CTRL%FID%CF')
         if (parse_string(line, Ctrl%FID%CF)           /= 0) call h_p_e(label)
      case('CTRL%FID%GEO')
         if (parse_string(line, Ctrl%FID%Geo)          /= 0) call h_p_e(label)
      case('CTRL%FID%LOC')
         if (parse_string(line, Ctrl%FID%Loc)          /= 0) call h_p_e(label)
      case('CTRL%FID%ALB')
         if (parse_string(line, Ctrl%FID%Alb)          /= 0) call h_p_e(label)
      case('CTRL%FID%BKP')
         if (parse_string(line, Ctrl%FID%BkP)          /= 0) call h_p_e(label)
      case('CTRL%FID%L2_PRIMARY')
         if (parse_string(line, Ctrl%FID%L2_primary)   /= 0) call h_p_e(label)
      case('CTRL%FID%L2_SECONDARY')
         if (parse_string(line, Ctrl%FID%L2_secondary) /= 0) call h_p_e(label)
      case('CTRL%RUN_ID')
         if (parse_string(line, Ctrl%Run_ID)           /= 0) call h_p_e(label)
      case('CTRL%RS%RSSELM','Ctrl%RS%FLAG')
         if (parse_user_text(line, Ctrl%RS%RsSelm)     /= 0) call h_p_e(label)
      case('CTRL%RS%SRSSELM')
         if (parse_user_text(line, Ctrl%RS%SRsSelm)    /= 0) call h_p_e(label)
      case('CTRL%RS%USE_FULL_BRDF')
         if (parse_string(line, Ctrl%RS%use_full_brdf) /= 0) call h_p_e(label)
      case('CTRL%RS%ALLOW_A_DEFAULT_SURFACE')
         if (parse_string(line, Ctrl%RS%allow_a_default_surface) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%RS%B')
         if (parse_string(line, Ctrl%RS%B)             /= 0) call h_p_e(label)
      case('CTRL%RS%SB')
         if (parse_string(line, Ctrl%RS%Sb)            /= 0) call h_p_e(label)
      case('CTRL%RS%CB')
         if (parse_string(line, Ctrl%RS%Cb)            /= 0) call h_p_e(label)
      case('CTRL%RS%ADD_FRACTIONAL')
         if (parse_string(line, Ctrl%RS%add_fractional)/= 0) call h_p_e(label)
      case('CTRL%RS%DIAGONAL_SRS')
         if (parse_string(line, Ctrl%RS%diagonal_SRs)  /= 0) call h_p_e(label)
      case('CTRL%RS%SOLAR_FACTOR')
         if (parse_string(line, Ctrl%RS%solar_factor)  /= 0) call h_p_e(label)
      case('CTRL%EQMPN%SYSELM')
         if (parse_user_text(line, Ctrl%EqMPN%SySelm)  /= 0) call h_p_e(label)
      case('CTRL%EQMPN%HOMOG')
         if (parse_string(line, Ctrl%EqMPN%Homog)      /= 0) call h_p_e(label)
      case('CTRL%EQMPN%COREG')
         if (parse_string(line, Ctrl%EqMPN%Coreg)      /= 0) call h_p_e(label)
      case('CTRL%INVPAR%CONVTEST')
         if (parse_string(line, Ctrl%Invpar%ConvTest)  /= 0) call h_p_e(label)
      case('CTRL%INVPAR%MQSTART')
         if (parse_string(line, Ctrl%Invpar%MqStart)   /= 0) call h_p_e(label)
      case('CTRL%INVPAR%MQSTEP')
         if (parse_string(line, Ctrl%Invpar%MqStep)    /= 0) call h_p_e(label)
      case('CTRL%INVPAR%MAXITER')
         if (parse_string(line, Ctrl%Invpar%MaxIter)   /= 0) call h_p_e(label)
      case('CTRL%INVPAR%CCJ')
         if (parse_string(line, Ctrl%Invpar%Ccj)       /= 0) call h_p_e(label)
      case('CTRL%INVPAR%XSCALE')
         if (parse_string(line, Ctrl%Invpar%XScale)    /= 0) call h_p_e(label)
      case('CTRL%INVPAR%XLLIM')
         if (parse_string(line, Ctrl%Invpar%XLLim)     /= 0) call h_p_e(label)
      case('CTRL%INVPAR%XULIM')
         if (parse_string(line, Ctrl%Invpar%XULim)     /= 0) call h_p_e(label)
      case('CTRL%INVPAR%ALWAYS_TAKE_GN')
         if (parse_string(line, Ctrl%Invpar%always_take_GN) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%INVPAR%DONT_ITER_CONVTEST')
         if (parse_string(line, Ctrl%Invpar%dont_iter_convtest) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%INVPAR%DISABLE_SS')
         if (parse_string(line, Ctrl%Invpar%disable_Ss)/= 0) call h_p_e(label)
      case('CTRL%QC%MAXJ')
         if (parse_string(line, Ctrl%QC%MaxJ)          /= 0) call h_p_e(label)
      case('CTRL%QC%MAXS')
         if (parse_string(line, Ctrl%QC%MaxS)          /= 0) call h_p_e(label)
      case('CTRL%IND%X0')
         if (parse_string(line, Ctrl%Ind%X0)           /= 0) call h_p_e(label)
      case('CTRL%IND%X1')
         if (parse_string(line, Ctrl%Ind%X1)           /= 0) call h_p_e(label)
      case('CTRL%IND%Y0')
         if (parse_string(line, Ctrl%Ind%Y0)           /= 0) call h_p_e(label)
      case('CTRL%IND%Y1')
         if (parse_string(line, Ctrl%Ind%Y1)           /= 0) call h_p_e(label)
      case('CTRL%MAXSOLZEN')
         if (parse_string(line, Ctrl%MaxSolZen)        /= 0) call h_p_e(label)
      case('CTRL%MAXSATZEN')
         if (parse_string(line, Ctrl%MaxSatZen)        /= 0) call h_p_e(label)
      case('CTRL%MINRELAZI')
         if (parse_string(line, Ctrl%MinRelAzi)        /= 0) call h_p_e(label)
      case('CTRL%SUNSET')
         if (parse_string(line, Ctrl%Sunset)           /= 0) call h_p_e(label)
      case('CTRL%I_EQUATION_FORM')
         if (parse_string(line, Ctrl%i_equation_form)  /= 0) call h_p_e(label)
      case('CTRL%LUTINTSELM','Ctrl%LUTINTFLAG')
         if (parse_user_text(line, Ctrl%LUTIntSelm)    /= 0) call h_p_e(label)
      case('CTRL%RTMINTSELM','Ctrl%RTMINTFLAG')
         if (parse_user_text(line, Ctrl%RTMIntSelm)    /= 0) call h_p_e(label)
      case('CTRL%CLOUDTYPE')
         if (parse_user_text(line, Ctrl%CloudType)     /= 0) call h_p_e(label)
      case('CTRL%BKPL')
         if (parse_user_text(line, Ctrl%Bkpl)          /= 0) call h_p_e(label)
      case('CTRL%MAX_SDAD')
         if (parse_string(line, Ctrl%Max_SDAD)         /= 0) call h_p_e(label)
      case('CTRL%SABOTAGE_INPUTS')
         if (parse_string(line, Ctrl%sabotage_inputs)  /= 0) call h_p_e(label)
      case('CTRL%PROCESS_CLOUDY_ONLY')
         if (parse_string(line, Ctrl%process_cloudy_only) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%NTYPES_TO_PROCESS')
         if (parse_string(line, Ctrl%NTypes_to_process)/= 0) call h_p_e(label)
      case('CTRL%TYPES_TO_PROCESS')
         if (parse_string(line, Ctrl%Types_to_process, Ctrl%NTypes_to_process) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%SURFACES_TO_SKIP')
         if (parse_user_text(line, Ctrl%surfaces_to_skip) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%SECOND_AOT_CH')
         if (parse_user_text(line, Ctrl%second_aot_ch) /= 0) call h_p_e(label)
      case('CTRL%RECHANS')
         if (parse_user_text(line, Ctrl%ReChans)       /= 0) call h_p_e(label)
      case('CTRL%AP')
         if (parse_user_text(line, Ctrl%AP)            /= 0) call h_p_e(label)
      case('CTRL%FG')
         if (parse_user_text(line, Ctrl%FG)            /= 0) call h_p_e(label)
      case('CTRL%XB')
         if (parse_string(line, Ctrl%XB)               /= 0) call h_p_e(label)
      case('CTRL%X0')
         if (parse_string(line, Ctrl%X0)               /= 0) call h_p_e(label)
      case('CTRL%SX')
         if (parse_string(line, Ctrl%Sx)               /= 0) call h_p_e(label)
      case('CTRL%SY')
         if (parse_string(line, Ctrl%Sy)               /= 0) call h_p_e(label)
      case('NX_DY','Ctrl%NX_DY','Ctrl%IND%NX_DY')
         if (parse_string(line, NX_DY)                 /= 0) call h_p_e(label)
      case('X_DY','Ctrl%X_DY','Ctrl%IND%X_DY')
         if (parse_user_text(line, X_DY, NX_DY, solar_ids) &
                                                       /= 0) call h_p_e(label)
      case('NX_TW','Ctrl%NX_TW','Ctrl%IND%NX_TW')
         if (parse_string(line, NX_TW)                 /= 0) call h_p_e(label)
      case('X_TW','Ctrl%X_TW','Ctrl%IND%X_TW')
         if (parse_user_text(line, X_TW, NX_TW, solar_ids) &
                                                       /= 0) call h_p_e(label)
      case('NX_NI','Ctrl%NX_NI','Ctrl%IND%NX_NI')
         if (parse_string(line, NX_NI)                 /= 0) call h_p_e(label)
      case('X_NI','Ctrl%X_NI','Ctrl%IND%X_NI')
         if (parse_user_text(line, X_NI, NX_NI, solar_ids) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%NXJ_DY')
         if (parse_string(line, NXJ_DY)                /= 0) call h_p_e(label)
      case('CTRL%XJ_DY')
         if (parse_user_text(line, XJ_DY, NXJ_DY, solar_ids) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%NXJ_TW')
         if (parse_string(line, NXJ_TW)                /= 0) call h_p_e(label)
      case('CTRL%XJ_TW')
         if (parse_user_text(line, XJ_TW, NXJ_TW, solar_ids) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%NXJ_NI')
         if (parse_string(line, NXJ_NI)                /= 0) call h_p_e(label)
      case('CTRL%XJ_NI')
         if (parse_user_text(line, XJ_NI, NXJ_NI, solar_ids) &
                                                       /= 0) call h_p_e(label)
      case('CTRL%APPROACH', &
           'CTRL%DO_NEW_NIGHT_RETRIEVAL', &
           'CTRL%DO_CTX_CORRECTION', &
           'CTRL%VERBOSE')
         cycle ! These arguments have already been parsed in the first pass
               ! through the driver file.
      case default
         write(*,*) 'ERROR: ReadDriver(): Unknown option: ',trim(label)
         stop error_stop_code
      end select
   end do

   deallocate(solar_ids)

end subroutine old_driver_second_read

end module read_driver_m
