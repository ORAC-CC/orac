!-------------------------------------------------------------------------------
! Name: 
!    Read_Driver
!
! Purpose:
!     Stores values required by driver file. This code is intended to replace
!     the idl write_idriver.pro
!
! Description:
!    Reads the values from the "driver" file used to set run-time options into
!    the CTRL structure. Settings beyond the typical can be overriden in the
!    driver using lines such as,
!       Ctrl%Run_ID = ABCD
!    The variable to change is identified before an = sign (with structure
!    references expressed by % or .) and its value is after the = sign. #
!    denotes a comment. Arrays should be delimited with commas, then semicolons
!    though whitespace, then commas is acceptable.
!
! Arguments:
!    Name    Type    In/Out/Both Description
!    Ctrl    Ctrl_t  out         Control struct defined in CTRL_def
!    message string  inout       Error message returned to calling routine
!    status  int     out         Status returned to calling function
!
! Algorithm:
!    N/A
!
! Local variables:
!    Name Type Description
!    N/A
!
! History:
!    15th May 2012, Caroline Poulsen: created original file to reapce ReadDriver
!     8th Jul 2012, Caroline Poulsen: fixed memory leaks
!    2012/07/13, MJ: implements option to read drifile path from command line
!    2012/07/13, Caroline Poulsen: changed ref_solar_sea nd ref_solar_land to
!       reals rather than integers.
!    2012/07/26, MJ: makes some small changes, paths passed to ORAC via driver
!       file must not have trailing forward slashes ("/"). Those are explicitly
!       added in this routine where necessary.
!    2012/07/29, Caroline Poulsen: fixed bug in twilight/night state variable
!       numbering
!    2012/08/10, Caroline Poulsen: modified state parameter indives for night
!    2012/08/22, MJ: makes adaptions to read netcdf files
!    2012/09/15, CP: removed double allocation of viewidx
!    2012/10/01, CP: changed active variables at night to be ctp fraction and
!       surface temperature, changed how first guess of CTP calculated at night
!       i.e matching temperature profile changed a priori errors of state vecto
!    2012/10/01, CP: added in default sx category as was not being reset after
!       each pixel was processed
!    2012/10/01, MJ: changed definition of AVHRR SW variables
!    2012/11/22, CP: fixed bug in index_ir AATSR definition
!    2013/03/18, CP: modified upper limits of ice and water effective radius
!    2013/03/18, CP: added in more comments for clarity
!    2013/06/27, MJ: implemented reading of path to driver file either from
!       environment variable or passed from outside
!    2013/10/02, CP: added comments for GT added aerosol classes for Bayesian
!       cloud id
!    2013/11/14, MJ: rewrote most parts referring to setting and reading channel
!       indices. Added reading of config file different driver file necessary
!       now.
!    2013/11/14, MJ: changes lower and upper limits for ctp.
!    2013/11/18, MJ: Several additional changes, introduces ysolar_msi and
!       ythermal_msi
!    2013/11/25, MJ: initialized previously uninitialised Ctrl%Run_ID
!    2014/01/12, GM: Small fixes involving Ctrl%Ind%NAvail
!    2014/01/15, GM: No need for Ctrl%defaultSx any more.
!    2014/01/15, GM: Added the ability to read the driver file contents from
!       standard input indicated by drifile .eq. '-'.
!    2014/01/16, GM: Use Selm* constants to set FG and AP instead of numbers.
!    2014/01/16, GM: Cleaned up code.
!    2014/01/31, MJ: Adds code for default surface reflection for avhrr
!       (=modis for the time being)
!    2014/06/04, MJ: introduced "WRAPPER" for c-preprocessor and associated
!       variables
!    2014/09/17, GM: Use the DiFlag* constants instead of integer values.
!    2014/12/01, CP: added in global and source attribute read capability
!    2014/12/01, OS: increased maximum acceptable retrieval cost from 10 to 100,
!       as otherwise ~30% of converged pixels are lost in l2tol3 processing
!    2014/12/17, AP: Converted read statements to parse_driver statements.
!       Permit optional overrides of default behaviour from driver.
!    2014/12/19, AP: Tidying. Cleaning the management of channel indexing.
!    2014/12/29, GM: Fixed a bug in the channel indexing changes above.
!
! Bugs:
!    NViews should be changed for dual view
!    Not quite working for AVHRR
!
! IMPORTANT NOTE:
!    If a new type of LUT i.e aerosol is added then new default values will have
!    to be added to this routine
!
! $Id$
!
!-------------------------------------------------------------------------------
module orac_io
contains
   
subroutine Read_Driver(Ctrl, global_atts, source_atts, verbose)

   use, intrinsic :: iso_fortran_env, only : input_unit

   use CTRL_def
   use ECP_constants
   use global_attributes
   use parsing
   use source_attributes

   implicit none

   ! Argument declarations
   type(CTRL_t),              intent(out)   :: Ctrl
   type(global_attributes_s), intent(inout) :: global_atts
   type(source_attributes_s), intent(inout) :: source_atts
   logical,                   intent(in)    :: verbose

   ! Local variables
   character(FilenameLen)             :: drifile
   integer                            :: i,ii,i0,i1,i2,j
   integer                            :: ios
   integer                            :: dri_lun
   character(FilenameLen)             :: input_path, input_filename, scratch_dir
   character(FilenameLen)             :: lut_dir, root_filename
   character(FilenameLen)             :: outname, line, label
   logical                            :: file_exists
   real,    allocatable, dimension(:) :: solar_store_sea, solar_store_land
   real,    allocatable, dimension(:) :: ref_solar_sea, ref_solar_land
   integer, allocatable, dimension(:) :: channel_ids_instr, channel_proc_flag
   integer, allocatable, dimension(:) :: channel_sw_flag, channel_lw_flag


   !----------------------------------------------------------------------------
   ! Locate the driver file
   !----------------------------------------------------------------------------
#ifndef WRAPPER
   if (command_argument_count() == 1) then
#else
   if (.false.)
#endif
      drifile = ''
      call get_command_argument(1, drifile)
   else
      call get_environment_variable("ORAC_TEXTIN", drifile)
   end if

   ! If drifile is '-' read the file from standard input otherwise read drifile
   if (drifile == '-') then
      dri_lun = input_unit
   else
      ! Check drifile exists
      inquire(file=drifile, exist=file_exists)
      if (.not. file_exists) then
         write(*,*)' Driver file pointed to by ORAC_DRIVER does not exist'
         stop DriverFileNotFound
      end if
      
      if (verbose) write(*,*) 'Driver file: ',trim(drifile)
      
      ! Open the driver file
      call find_lun(dri_lun)
      open(unit=dri_lun, file=drifile, iostat=ios)
      if (ios /= 0) then
         write(*,*)' Unable to open driver file: ',trim(drifile)
         stop DriverFileOpenErr
      end if
   end if


   !----------------------------------------------------------------------------
   ! Read the driver file
   !----------------------------------------------------------------------------
   ! Read folder paths
   if (parse_driver(dri_lun, line) == 0) call parse_string(line, input_path)
   if (verbose) write(*,*) 'Input directory: ',trim(input_path)

   if (parse_driver(dri_lun, line) == 0) call parse_string(line, input_filename)
   if (verbose) write(*,*) 'Input filename: ',trim(input_filename)

   if (parse_driver(dri_lun, line) == 0) call parse_string(line, scratch_dir)
   if (verbose) write(*,*) 'Output directory: ',trim(scratch_dir)

   if (parse_driver(dri_lun, line) == 0) call parse_string(line, lut_dir)
   if (verbose) write(*,*) 'LUT directory: ',trim(lut_dir)

   ! Set filenames
   Ctrl%Data_Dir = trim(scratch_dir)//'/'
   Ctrl%Out_Dir  = trim(scratch_dir)//'/'
   Ctrl%SAD_Dir  = trim(lut_dir)//'/'
   if (verbose) then
      write(*,*) 'Ctrl%Data_Dir: ',trim(Ctrl%Data_Dir)
      write(*,*) 'Ctrl%out_Dir: ',trim(Ctrl%out_Dir)
      write(*,*) 'Ctrl%SAD_Dir: ',trim(Ctrl%SAD_Dir)
   end if
   
   root_filename   = trim(input_path)//'/'//trim(input_filename)
   Ctrl%FID%MSI    = trim(root_filename)//'.msi.nc'
   Ctrl%FID%LWRTM  = trim(root_filename)//'.lwrtm.nc'
   Ctrl%FID%SWRTM  = trim(root_filename)//'.swrtm.nc'
   Ctrl%FID%PRTM   = trim(root_filename)//'.prtm.nc'
   Ctrl%FID%LS     = trim(root_filename)//'.lsf.nc'
   Ctrl%FID%CF     = trim(root_filename)//'.clf.nc'
   Ctrl%FID%Geo    = trim(root_filename)//'.geo.nc'
   Ctrl%FID%Loc    = trim(root_filename)//'.loc.nc'
   Ctrl%FID%uv     = trim(root_filename)//'.uv.nc'
   Ctrl%FID%Alb    = trim(root_filename)//'.alb.nc'
   Ctrl%FID%Config = trim(root_filename)//'.config.nc'
   if (verbose) write(*,*) 'Ctrl%FID%Config: ',trim(Ctrl%FID%Config)

   ! Read name of instrument
   if (parse_driver(dri_lun, line) == 0) call parse_string(line, Ctrl%Inst%Name)
   write(*,*) 'Ctrl%Inst%Name: ',trim(Ctrl%Inst%Name)

   ! Number of channels in preprocessing file
   ! (this is actually not really necessary as we have that in the config file)
   if (parse_driver(dri_lun, line) == 0) call parse_string(line, Ctrl%Ind%NAvail)
   if (verbose) write(*,*) &
        'Number of channels expected in preproc files: ',Ctrl%Ind%NAvail

   ! Read channel related info
   call read_config_file(Ctrl, channel_ids_instr, channel_sw_flag, &
     channel_lw_flag, global_atts, source_atts, verbose)

   ! Read processing flag from driver
   allocate(channel_proc_flag(Ctrl%Ind%Navail))
   if (parse_driver(dri_lun, line) == 0) &
        call parse_string(line, channel_proc_flag)
   if (sum(channel_proc_flag) < 1 .or. &
       sum(channel_proc_flag) > Ctrl%Ind%Navail .or. &
       any(channel_proc_flag /= 0 .and. channel_proc_flag /= 1)) then
      write(*,*) 'ERROR: channel flag from driver wrong: ',channel_proc_flag
      stop DriverFileIncompat
   end if
   if (verbose) write(*,*) 'channel flag from driver: ',channel_proc_flag

   ! Determine the number of channels to be used.
   Ctrl%Ind%Ny       = count(channel_proc_flag == 1)
   Ctrl%Ind%NSolar   = count(channel_sw_flag == 1 .and. channel_proc_flag == 1)
   Ctrl%Ind%NThermal = count(channel_lw_flag == 1 .and. channel_proc_flag == 1)
   Ctrl%Ind%NMixed   = count(channel_sw_flag == 1 .and. channel_lw_flag == 1 &
                             .and. channel_proc_flag == 1)
   if (verbose) write(*,*) 'Ny,NSolar,NThermal,NMixed: ',Ctrl%Ind%Ny, &
        Ctrl%Ind%NSolar,Ctrl%Ind%NThermal,Ctrl%Ind%NMixed

   ! Produce channel indexing arrays
   allocate(Ctrl%Ind%ICh(Ctrl%Ind%Ny))
   allocate(Ctrl%Ind%Y_ID(Ctrl%Ind%Ny))
   allocate(Ctrl%Ind%YSolar(Ctrl%Ind%NSolar))
   allocate(Ctrl%Ind%YThermal(Ctrl%Ind%NThermal))
   allocate(Ctrl%Ind%YMixed(Ctrl%Ind%NMixed))
   ii = 0
   i0 = 0
   i1 = 0
   i2 = 0
   do i=1,Ctrl%Ind%Navail
      ! Identify processing channels WITH RESPECT TO THE PREPROC FILE
      if (channel_proc_flag(i) == 1) then
         ii = ii+1
         Ctrl%Ind%ICh(ii) = i ! Fortran array index for channel
         Ctrl%Ind%Y_ID(ii) = channel_ids_instr(i) ! Instrument channel number

         ! Identify solar and thermal channels WITH RESPECT TO CTRL%IND%ICH
         if (channel_sw_flag(i) == 1) then
            i0 = i0+1
            Ctrl%Ind%YSolar(i0) = ii
         end if
         if (channel_lw_flag(i) == 1) then
            i1 = i1+1
            Ctrl%Ind%YThermal(i1) = ii
         end if
         if (channel_sw_flag(i) == 1 .and. channel_lw_flag(i) == 1) then
            i2 = i2+1
            Ctrl%Ind%YMixed(i2) = ii
         end if
      end if
   end do
   if (verbose) then
      write(*,*) 'Ctrl%Ind%ICh: ',Ctrl%Ind%ICh
      write(*,*) 'Ctrl%Ind%Y_ID: ',Ctrl%Ind%Y_ID
      write(*,*) 'Ctrl%Ind%YSolar: ',Ctrl%Ind%YSolar
      write(*,*) 'Ctrl%Ind%YThermal: ',Ctrl%Ind%YThermal
      write(*,*) 'Ctrl%Ind%YMixed: ',Ctrl%Ind%YMixed
   end if

   ! Read in cloud class (aka phase of no aerosols processed)
   if (parse_driver(dri_lun, line) == 0) &
        call parse_string(line, Ctrl%CloudClass)
   if (verbose) write(*,*)'Ctrl%CloudClass: ',trim(Ctrl%CloudClass)


   !----------------------------------------------------------------------------
   ! Set the rest of the Ctrl structure
   !----------------------------------------------------------------------------

   outname=trim(scratch_dir)//'/'//trim(input_filename)//trim(Ctrl%CloudClass)
   Ctrl%FID%L2_primary   = trim(outname)//'.primary.nc'
   Ctrl%FID%L2_secondary = trim(outname)//'.secondary.nc'
   Ctrl%FID%Log          = trim(outname)//'.log'
   Ctrl%FID%Diag         = trim(outname)//'.diag'
   Ctrl%FID%BkP          = trim(outname)//'bkp'
   if (verbose) then
      write(*,*) 'Ctrl%FID%L2_primary: ', trim(Ctrl%FID%L2_primary)
      write(*,*) 'Ctrl%FID%L2_secondary: ', trim(Ctrl%FID%L2_secondary)
      write(*,*) 'Ctrl%FID%Log: ',trim(Ctrl%FID%Log)
      write(*,*) 'Ctrl%FID%Diag: ',trim(Ctrl%FID%Diag)
      write(*,*) 'Ctrl%FID%BkP: ',trim(Ctrl%FID%BkP)
   end if

   ! The level of breakpoint output when the code is compiled with breakpoint
   ! option Ctrl%Bkpl
   Ctrl%Bkpl=3

   ! Set diagnostic flags
   Ctrl%Diagl(DiFlagQC)   = 1
   Ctrl%Diagl(DiFlagIter) = 1
   Ctrl%Diagl(DiFlagPhCh) = 1
   Ctrl%Diagl(DiFlagCost) = 1
   Ctrl%Diagl(DiFlagSt1)  = 1
   Ctrl%Diagl(DiFlagSs1)  = 1
   Ctrl%Diagl(DiFlagSt2)  = 0
   Ctrl%Diagl(DiFlagSs2 ) = 0
   Ctrl%Diagl(DiFlagYFit) = 1
   Ctrl%Diagl(DiFlagXFit) = 1
   Ctrl%Diagl(DiFlagAP)   = 1
   Ctrl%Diagl(DiFlagFG)   = 1
   Ctrl%Diagl(DiFlagSx)   = 0
   Ctrl%Diagl(DiFlagSy)   = 0

   Ctrl%RTMIntflag = RTMIntMethLinear
   Ctrl%LUTIntflag = LUTIntMethLinear

   Ctrl%MaxSatZen  = 90 ! max satellite zenith angle
   Ctrl%MaxSolZen  = 80 ! max solar zenith angle > 90 = night image
   Ctrl%Sunset     = 90 ! used to set twilight option


   ! Set stuff in Ctrl%Ind
   Ctrl%Ind%Ws = 0 ! warm start option i.e enables user to start partway through
                   ! a scene


   ! For each of the day, twilight, night active state variable arrays, read the
   ! array and set up the corresponding inactive array.

   ! Day options
   Ctrl%Ind%Nx_Dy = 5   ! number of active state variables

   Ctrl%Ind%X_Dy(1) = ITau ! indices of state parameters
   Ctrl%Ind%X_Dy(2) = IRe
   Ctrl%Ind%X_Dy(3) = IPc
   Ctrl%Ind%X_Dy(4) = IFr
   Ctrl%Ind%X_Dy(5) = ITs

   ! Twilight options
   Ctrl%Ind%Nx_Tw = 3   ! number of active state variables

   Ctrl%Ind%X_Tw(1) = IPc ! indices of state parameters
   Ctrl%Ind%X_Tw(2) = IFr
   Ctrl%Ind%X_Tw(3) = ITs


   ! Night options
   Ctrl%Ind%Nx_Ni = 3   ! number of active state variables

   Ctrl%Ind%X_Ni(1) = IPc ! indices of state parameters
   Ctrl%Ind%X_Ni(2) = IFr
   Ctrl%Ind%X_Ni(3) = ITs

   ! Force single view (for the time being)
   Ctrl%Ind%NViews=1
   allocate(Ctrl%Ind%Viewidx(Ctrl%Ind%Ny))
   Ctrl%Ind%Viewidx = 1

   Ctrl%CloudType     = 1 ! use this to select which coreg/homog errors to use

   ! Set a priori options (Tau,Re,Pc,F,Ts)

   ! Day
   Ctrl%AP(ITau,IDay) = SelmCtrl
   Ctrl%AP(IRe,IDay)  = SelmCtrl
   Ctrl%AP(IPc,IDay)  = SelmCtrl
   Ctrl%AP(IFr,IDay)  = SelmMeas
   Ctrl%AP(ITs,IDay)  = SelmAux

   ! Twilight
   Ctrl%AP(ITau,ITwi) = SelmCtrl
   Ctrl%AP(IRe,ITwi)  = SelmCtrl
   Ctrl%AP(IPc,ITwi)  = SelmCtrl
   Ctrl%AP(IFr,ITwi)  = SelmMeas
   Ctrl%AP(ITs,ITwi)  = SelmAux

   ! Night
   Ctrl%AP(ITau,INight) = SelmCtrl
   Ctrl%AP(IRe,INight)  = SelmCtrl
   Ctrl%AP(IPc,INight)  = SelmCtrl
   Ctrl%AP(IFr,INight)  = SelmMeas
   Ctrl%AP(ITs,INight)  = SelmAux


   ! Set first guess options (Tau,Re,Pc,F,Ts)
   ! What do the constants mean:
   ! SelmCtrl: Static a priori variable i.e does not change
   ! SelmMeas: Use a dynamically chosen first guess dependant on measurements
   ! SelmAux: Use a value from an external auxiliary file
   ! Ctrl%FG(state variable, time of day)

   ! Day
   Ctrl%FG(ITau,IDay) = SelmCtrl
   Ctrl%FG(IRe,IDay)  = SelmCtrl
   Ctrl%FG(IPc,IDay)  = SelmMeas ! from ir profile
   Ctrl%FG(IFr,IDay)  = SelmCtrl
   Ctrl%FG(ITs,IDay)  = SelmAux  ! from auxiliary file

   ! Twilight
   Ctrl%FG(ITau,ITwi) = SelmCtrl
   Ctrl%FG(IRe,ITwi)  = SelmCtrl
   Ctrl%FG(IPc,ITwi)  = SelmMeas ! from ir profile
   Ctrl%FG(IFr,ITwi)  = SelmCtrl
   Ctrl%FG(ITs,ITwi)  = SelmAux  ! from auxiliary file

   ! Night
   Ctrl%FG(ITau,INight) = SelmCtrl
   Ctrl%FG(IRe,INight)  = SelmCtrl
   Ctrl%FG(IPc,INight)  = SelmMeas ! from ir profile
   Ctrl%FG(IFr,INight)  = SelmCtrl
   Ctrl%FG(ITs,INight)  = SelmAux  ! from auxiliary file


   ! Set default a priori and first guess values. Quite often these values have a
   ! very high uncertainty
   select case (trim(Ctrl%CloudClass))
   case('WAT')
      Ctrl%XB(ITau) = 0.8
      Ctrl%XB(IRe) = 12.
      Ctrl%XB(IPc) = 900.
      Ctrl%XB(IFr) = 1.
      Ctrl%XB(ITs) = 300.0
      
      Ctrl%X0(ITau) = 0.8
      Ctrl%X0(IRe) = 12.
      Ctrl%X0(IPc) = 700.
      Ctrl%X0(IFr) = 1.
      Ctrl%X0(ITs) = 300.
   case('ICE')
      Ctrl%XB(ITau) = 0.8
      Ctrl%XB(IRe) = 30.
      Ctrl%XB(IPc) = 400.
      Ctrl%XB(IFr) = 1.
      Ctrl%XB(ITs) = 300.0
      
      Ctrl%X0(ITau) = 0.8
      Ctrl%X0(IRe) = 30.
      Ctrl%X0(IPc) = 400.
      Ctrl%X0(IFr) = 1.
      Ctrl%X0(ITs) = 300.
   case('EYJ')
      Ctrl%XB(ITau) = 0.8
      Ctrl%XB(IRe) = 0.5
      Ctrl%XB(IPc) = 1000.
      Ctrl%XB(IFr) = 1.
      Ctrl%XB(ITs) = 300.
      
      Ctrl%X0(ITau) = 0.8
      Ctrl%X0(IRe) = 0.5
      Ctrl%X0(IPc) = 1000.
      Ctrl%X0(IFr) = 1.
      Ctrl%X0(ITs) = 300.
   case('MAR')
      Ctrl%XB(ITau) = 0.1
      Ctrl%XB(IRe) = 1.8
      Ctrl%XB(IPc) = 1000.
      Ctrl%XB(IFr) = 1.
      Ctrl%XB(ITs) = 300.
      
      Ctrl%X0(ITau) = 0.1
      Ctrl%X0(IRe) = 1.8
      Ctrl%X0(IPc) = 1000.
      Ctrl%X0(IFr) = 1.
      Ctrl%X0(ITs) = 300.
   case('DES')
      Ctrl%XB(ITau) = 0.0
      Ctrl%XB(IRe) = 1.4
      Ctrl%XB(IPc) = 1000.
      Ctrl%XB(IFr) = 1.
      Ctrl%XB(ITs) = 300.
      
      Ctrl%X0(ITau) = 0.0
      Ctrl%X0(IRe) = 1.4
      Ctrl%X0(IPc) = 1000.
      Ctrl%X0(IFr) = 1.
      Ctrl%X0(ITs) = 300.
   case default
      write(*,*) 'ERROR: ReadDriver(): Unsupported cloud/aerosol class.'
      stop BadCloudClass
   end select



   ! may want to put these in Get_Illum routine at some point
   ! Set default a priori error covariance
   if ((trim(Ctrl%CloudClass) .eq. 'EYJ' ) .or. &
       (trim(Ctrl%CloudClass) .eq. 'MAR' ) .or. &
       (trim(Ctrl%CloudClass) .eq. 'DES' ) ) then
      Ctrl%Sx(ITau) = 1.0e+01 ! optical depth
      Ctrl%Sx(IRe) = 1.0e-01 ! effective radii
      Ctrl%Sx(IPc) = 1.0e+06 ! ctp
      Ctrl%Sx(IFr) = 1.0e-10 ! fraction
      Ctrl%Sx(ITs) = 1.0e+00 ! surface temperature
   else
      Ctrl%Sx(ITau) = 1.0e+08 ! optical depth
      Ctrl%Sx(IRe) = 1.0e+08 ! effective radii
      Ctrl%Sx(IPc) = 1.0e+06 ! ctp
      Ctrl%Sx(IFr) = 1.0e-10 ! fraction
      Ctrl%Sx(ITs) = 1.0e+00 ! surface temperature
   end if

   Ctrl%Max_SDAD = 10 ! No. of pixels where state is valid for SDAD setting


   allocate(Ctrl%Sy(Ctrl%Ind%Ny,Ctrl%Ind%Ny))


   ! Set Ctrl%RS

   ! Look at the channel numbers and determine what combination of vis/mixed/ir
   ! channels. This is instrument dependant so if introducing a new instrument
   ! channel info needs to be stored here
   allocate(solar_store_sea(Ctrl%Ind%Navail))
   allocate(solar_store_land(Ctrl%Ind%Navail))

   ! Set some default arrays for surface reflection
   if ((trim(Ctrl%Inst%Name) .eq. trim('MODIS-AQUA')) .or.&
       (trim(Ctrl%Inst%Name) .eq. trim('MODIS-TERRA')) .or. &
       !this is a fudge but probably a harmless one:
       (trim(Ctrl%Inst%Name(1:5)) .eq. trim('AVHRR')) ) then
      solar_store_sea(1)  = 2.0
      solar_store_sea(2)  = 1.0
      solar_store_sea(3)  = 0.0
      solar_store_sea(4)  = 0.0
      solar_store_sea(5)  = 0.0
      solar_store_sea(6)  = 1.0

      solar_store_land(1) = 10.0
      solar_store_land(2) = 1.0
      solar_store_land(3) = 0.0
      solar_store_land(4) = 0.0
      solar_store_land(5) = 0.0
      solar_store_land(6) = 1.0
   else if (Ctrl%Inst%Name .eq. 'AATSR') then
      solar_store_sea(1)  = 5.
      solar_store_sea(2)  = 2.0
      solar_store_sea(3)  = 1.0
      solar_store_sea(4)  = 1.0

      solar_store_land(1) = 15.0
      solar_store_land(2) = 10.0
      solar_store_land(3) = 1.0
      solar_store_land(4) = 1.0
   end if

   ! Set default surface reflectance values for channels used. This just maps
   ! the values from all channels in the file to the ones which will be actually
   ! used
   Ctrl%RS%Flag = SelmAux ! Selection method

   Ctrl%RS%use_full_brdf = .true.

   allocate(ref_solar_sea(Ctrl%Ind%Ny))
   allocate(ref_solar_land(Ctrl%Ind%Ny))
   ii=1
   do i=1,Ctrl%Ind%Navail
      if (channel_proc_flag(i) .eq. 1) then
         ref_solar_sea(ii)=solar_store_sea(i)
         ref_solar_land(ii)=solar_store_land(i)
         ii=ii+1
      end if
   end do
   deallocate(solar_store_sea)
   deallocate(solar_store_land)

   allocate(Ctrl%RS%B(Ctrl%Ind%Nsolar,2))
   Ctrl%RS%B=0.0
   ! valid for 0.67/0.87/1.6 channels
   Ctrl%RS%B(Ctrl%Ind%Ysolar,1) = ref_solar_sea(Ctrl%Ind%Ysolar)/100.0
   Ctrl%RS%B(Ctrl%Ind%Ysolar,2) = ref_solar_land(Ctrl%Ind%Ysolar)/100.0
   deallocate(ref_solar_sea)
   deallocate(ref_solar_land)

   Ctrl%RS%Sb            = 20.0/100.0 ! Percentage error in surface reflectance
   Ctrl%RS%Cb            = 0.2        ! Correlation between surface reflectance


   ! Set Ctrl%EqMPN

   Ctrl%EqMPN%Rs         = 1          ! Flag to use EqMPN from Rs errors
   Ctrl%EqMPN%TH         = 0          ! Flag to use EqMPN from T/H(z) errors
   Ctrl%EqMPN%Homog      = 1          ! Flag to use EqMPN from homog errors
   Ctrl%EqMPN%Coreg      = 1          ! Flag to use EqMPN from coReg errors


   ! Set Ctrl%Invpar

   Ctrl%Invpar%MqStart   = 0.001      ! Marquardt: starting parameter
   Ctrl%Invpar%MqStep    = 10.0       ! step parameter
   Ctrl%Invpar%MaxIter   = 40         ! Maximum # of iterations
   Ctrl%Invpar%MaxPhase  = 3          ! Maximum # of phase changes
   Ctrl%Invpar%Ccj       = 0.05       ! Cost convergence criteria

   Ctrl%Invpar%XScale(ITau) = 10.0       ! Scaling parameters (Tau,Re,Pc,F,Ts)
   Ctrl%Invpar%XScale(IRe) = 1.0
   Ctrl%Invpar%XScale(IPc) = 1.0
   Ctrl%Invpar%XScale(IFr) = 1000.0
   Ctrl%Invpar%XScale(ITs) = 1.0

   Ctrl%Invpar%XLLim(ITau)  = -3.0

   if ((trim(Ctrl%CloudClass) .eq. 'EYJ' ) .or. &
       (trim(Ctrl%CloudClass) .eq. 'MAR' ) .or. &
       (trim(Ctrl%CloudClass) .eq. 'DES' ) ) then
      Ctrl%Invpar%XLLim(IRe) = 0.01
   else
      Ctrl%Invpar%XLLim(IRe) = 0.1
   end if
   Ctrl%Invpar%XLLim(IPc)  = 10.0
   Ctrl%Invpar%XLLim(IFr)  = 1.0
   Ctrl%Invpar%XLLim(ITs)  = 250.0

   ! Upper limit on state Ctrl.Invpar.XULim
   Ctrl%Invpar%XULim(ITau) = 2.408
   if (trim(Ctrl%CloudClass) .eq. 'WAT') then
      Ctrl%Invpar%XULim(IRe) = 35.0
   else if (trim(Ctrl%CloudClass) .eq. 'ICE') then
      Ctrl%Invpar%XULim(IRe) = 100.0
   else if ((trim(Ctrl%CloudClass) .eq. 'EYJ' ) .or. &
            (trim(Ctrl%CloudClass) .eq. 'MAR' ) .or. &
            (trim(Ctrl%CloudClass) .eq. 'DES' ) ) then
      Ctrl%Invpar%XULim(IRe) = 20.0
   end if

   Ctrl%Invpar%XULim(IPc) = 1200.0
   Ctrl%Invpar%XULim(IFr) = 1.0
   Ctrl%Invpar%XULim(ITs) = 320.0


   ! Set Ctrl%QC

   ! Maximum acceptable retrieval cost
   Ctrl%QC%MaxJ = 100.0

   ! Maximum acceptable retrieval errors
   Ctrl%QC%MaxS(ITau) = 0.08
   Ctrl%QC%MaxS(IRe) = 3.0
   Ctrl%QC%MaxS(IPc) = 200.
   Ctrl%QC%MaxS(IFr) = 0.2
   Ctrl%QC%MaxS(ITs) = 2.0

   
   !----------------------------------------------------------------------------
   ! Consider optional lines of driver file
   !----------------------------------------------------------------------------
   do while (parse_driver(dri_lun, line, label) == 0)
      call clean_driver_label(label)
      select case (label)
      case('CTRL%DATA_DIR')
         call parse_string(line, Ctrl%Data_Dir)
      case('CTRL%OUT_DIR')
         call parse_string(line, Ctrl%Out_Dir)
      case('CTRL%SAD_DIR')
         call parse_string(line, Ctrl%SAD_Dir)
      case('CTRL%FID%MSI')
         call parse_string(line, Ctrl%FID%MSI)
       case('CTRL%FID%LWRTM')
        call parse_string(line, Ctrl%FID%LWRTM)
      case('CTRL%FID%SWRTM')
         call parse_string(line, Ctrl%FID%SWRTM)
      case('CTRL%FID%PRTM')
         call parse_string(line, Ctrl%FID%PRTM)
      case('CTRL%FID%LS')
         call parse_string(line, Ctrl%FID%LS)
      case('CTRL%FID%CF')
         call parse_string(line, Ctrl%FID%CF)
      case('CTRL%FID%GEO')
         call parse_string(line, Ctrl%FID%Geo)
      case('CTRL%FID%LOC')
         call parse_string(line, Ctrl%FID%Loc)
      case('CTRL%FID%UV')
         call parse_string(line, Ctrl%FID%uv)
      case('CTRL%FID%ALB')
         call parse_string(line, Ctrl%FID%Alb)
      case('CTRL%FID%L2_PRIMARY')
         call parse_string(line, Ctrl%FID%L2_primary)
      case('CTRL%FID%L2_SECONDARY')
         call parse_string(line, Ctrl%FID%L2_secondary)
      case('CTRL%FID%LOG')
         call parse_string(line, Ctrl%FID%Log)
      case('CTRL%FID%DIAG')
         call parse_string(line, Ctrl%FID%Diag)
      case('CTRL%FID%BKP')
         call parse_string(line, Ctrl%FID%BkP)
      case('CTRL%BKPL')
         call parse_string(line, Ctrl%Bkpl)
      case('CTRL%DIAGL')
         call parse_string(line, Ctrl%Diagl)
      case('CTRL%RTMINTFLAG')
         call parse_string(line, Ctrl%RTMIntflag)
      case('CTRL%LUTINTFLAG')
         call parse_string(line, Ctrl%LUTIntflag)
      case('CTRL%MAXSATZEN')
         call parse_string(line, Ctrl%MaxSatZen)
      case('CTRL%MAXSOLZEN')
         call parse_string(line, Ctrl%MaxSolZen)
      case('CTRL%SUNSET')
         call parse_string(line, Ctrl%Sunset)
      case('CTRL%IND%WS')
         call parse_string(line, Ctrl%Ind%Ws)
      case('CTRL%IND%NX_DY')
         call parse_string(line, Ctrl%Ind%NX_DY)
      case('CTRL%IND%X_DY')
         call parse_string(line, Ctrl%Ind%X_DY)
      case('CTRL%IND%NX_TW')
         call parse_string(line, Ctrl%Ind%NX_TW)
      case('CTRL%IND%X_TW')
         call parse_string(line, Ctrl%Ind%X_TW)
     case('CTRL%IND%NX_NI')
         call parse_string(line, Ctrl%Ind%NX_NI)
      case('CTRL%IND%X_NI')
         call parse_string(line, Ctrl%Ind%X_NI)
      case('CTRL%IND%NVIEWS')
         call parse_string(line, Ctrl%Ind%NViews)
      case('CTRL%IND%VIEWIDX')
         call parse_string(line, Ctrl%Ind%Viewidx)
       case('CTRL%AP')
         call parse_string(line, Ctrl%AP)
      case('CTRL%FG')
         call parse_string(line, Ctrl%FG)
      case('CTRL%XB')
         call parse_string(line, Ctrl%XB)
      case('CTRL%X0')
         call parse_string(line, Ctrl%X0)
      case('CTRL%SX')
         call parse_string(line, Ctrl%Sx)
      case('CTRL%RS%FLAG')
         call parse_string(line, Ctrl%RS%Flag)
       case('CTRL%RS%SB')
         call parse_string(line, Ctrl%RS%Sb)
      case('CTRL%RS%CB')
         call parse_string(line, Ctrl%RS%Cb)
      case('CTRL%EQMPN%RS')
         call parse_string(line, Ctrl%EqMPN%Rs)
      case('CTRL%EQMPN%TH')
         call parse_string(line, Ctrl%EqMPN%TH)
      case('CTRL%EQMPN%HOMOG')
         call parse_string(line, Ctrl%EqMPN%Homog)
      case('CTRL%EQMPN%COREG')
         call parse_string(line, Ctrl%EqMPN%Coreg)
      case('CTRL%INVPAR%MQSTART')
         call parse_string(line, Ctrl%Invpar%MqStart)
      case('CTRL%INVPAR%MQSTEP')
         call parse_string(line, Ctrl%Invpar%MqStep)
      case('CTRL%INVPAR%MAXITER')
         call parse_string(line, Ctrl%Invpar%MaxIter)
      case('CTRL%INVPAR%MAXPHASE')
         call parse_string(line, Ctrl%Invpar%MaxPhase)
      case('CTRL%INVPAR%CCJ')
         call parse_string(line, Ctrl%Invpar%Ccj)
      case('CTRL%INVPAR%XSCALE')
         call parse_string(line, Ctrl%Invpar%XScale)
      case('CTRL%INVPAR%XLLIM')
         call parse_string(line, Ctrl%Invpar%XLLim)
      case('CTRL%INVPAR%XULIM')
         call parse_string(line, Ctrl%Invpar%XULim)
      case('CTRL%QC%MAXJ')
         call parse_string(line, Ctrl%QC%MaxJ)
      case('CTRL%QC%MAXS')
         call parse_string(line, Ctrl%QC%MaxS)
      case default
         print*,'ERROR: ReadDriver(): Unknown option ',trim(label)
      end select
   end do

   ! --------------------------------------------------------------------------
   ! Things that had to be moved to after the optional lines
   ! -------------------------------------------------------------------------

   ! Sort out inactive elements of day/night/twilight arrays
   Ctrl%Ind%NxI_Dy = MaxStateVar - Ctrl%Ind%Nx_Dy
   Ctrl%Ind%NxI_Tw = MaxStateVar - Ctrl%Ind%Nx_Tw
   Ctrl%Ind%NxI_Ni = MaxStateVar - Ctrl%Ind%Nx_Ni

   ! Set inactive state variables
   i0 = 0
   i1 = 0
   i2 = 0
   do i=1, MaxStateVar
      if (.not. any(Ctrl%Ind%X_Dy == i)) then
         i0 = i0+1
         Ctrl%Ind%XI_Dy(i0) = i
      end if
      if (.not. any(Ctrl%Ind%X_Tw == i)) then
         i1 = i1+1
         Ctrl%Ind%XI_Tw(i1) = i
      end if
      if (.not. any(Ctrl%Ind%X_Ni == i)) then
         i2 = i2+1
         Ctrl%Ind%XI_Ni(i2) = i
      end if
   end do
      
   !----------------------------------------------------------------------------
   ! Now do some checks
   !----------------------------------------------------------------------------
   ! Check that the first-guess methods for all variables are legal in ORAC
   ! and supported. Not all legal values are supported for all variables.
   ! N.B. not all supported methods can be used in all conditions and this is
   ! NOT CHECKED here.
   
   do j=1,3 ! loop over day, twi, night values for FG
      do i=1, MaxStateVar
         select case (Ctrl%Fg(i,j))
         case (SelmCtrl)
            continue
            
         case (SelmMeas)
            if (i == IRe .or. i == ITs) then
               write(*,*) 'Read_Driver: MDAD method not supported ' // &
                    'for setting first guess Re, Ts'
               stop FGMethErr
            end if
            
         case (SelmAux)
            if (i /= ITs) then
               write(*,*) 'Read_Driver: AUX method ONLY supported ' // &
                    'for setting first guess Ts'
               stop FGMethErr
            end if
            
         case default
            write(*,*) 'Read_Driver: Invalid method ', &
                 'for first-guess state variable ',i
            stop FGMethErr
         end select
      end do
   end do

   ! Check validity of a priori selection options. Not all legal values are
   ! supported for all variables.
   do j=1, 3
      do i=1, MaxStateVar
         select case (Ctrl%Ap(i,j))
         case (SelmCtrl)
            continue
            
         case (SelmMeas)
            if (i == IRe .or. i == ITs) then
               write(*,*) 'Read_Driver: MDAD method not supported for ' //&
                    'setting a priori Re, Ts'
               stop APMethErr
            end if
            
         case (SelmAux)
            if (i /= ITs) then
               write(*,*) 'Read_Driver: AUX method ONLY supported ' // &
                    'for setting a priori Ts'
               stop APMethErr
            end if
            
         case default
            write(*,*) &
                 'Read_Driver: Invalid method for a priori state variable ',i
            stop APMethErr
         end select
      end do
   end do

   ! Check validity of surface reflectance flag
   select case (Ctrl%RS%Flag)
   case (SelmCtrl, SelmAux)
      continue
   case (SelmMeas)
      write(*,*) 'Read_Driver: surface reflectance method not supported'
      stop GetSurfaceMeth
   case default
      write(*,*) 'Read_Driver: invalid surface reflectance method'
      stop GetSurfaceMeth
   end select

   
   ! Clean up
   if (drifile /= '-') then
      close(unit=dri_lun)
   end if

   deallocate(channel_ids_instr)
   deallocate(channel_sw_flag)
   deallocate(channel_lw_flag)

end subroutine Read_Driver

#include "read_config_file.F90"

end module orac_io
