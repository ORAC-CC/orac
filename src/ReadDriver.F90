!-------------------------------------------------------------------------------
! Name:
!   Read_Driver
!
! Purpose:
!    Stores values required by driver file. This code is intended to replace the
!    idl write_idriver.pro
!
! Description:
!   Reads the values from the "driver" file used to set run-time options into
!   the CTRL structure.
!
! Arguments:
!   Name    Type    In/Out/Both Description
!   Ctrl    Ctrl_t  out         Control struct defined in CTRL_def
!   message string  inout       Error message returned to calling routine
!   status  int     out         Status returned to calling function
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
!    2012/07/29, Caroline Poulsen: fixed bug in twilight/night statevariable
!       numbering
!    2012/08/10, Caroline Poulsen: modified state parameter indives for night
!    2012/08/22, MJ: makes adaptions to read netcdf files
!    2012/09/15, CP: removed double allocation of vieidx
!    2012/10/01, CP: changed active variables at night to be ctp fraction and
!       surface temperature, changed how first guess of CTP calculated at night
!       i.e matching temperature profile changed apriori errors of state vecto
!    2012/10/01, CP: added in default sx category as was not being resset after
!       each pixel was processed
!    2012/10/01, MJ: changed definition of AVHRR SW variables
!    2012/11/22, CP: fixed bug in index_ir AATSR defintion
!    2013/03/18, CP: modified upper limits of ice and water effective radius
!    2013/03/18, CP: added in more comments for clarity
!    2013/06/27, MJ: implemented reading of path to driver file either from
!       environment variable or passed from outside
!    2013/10/02, CP: added comments for GT added aerosl classes for Bayesian
!       cloud id
!    2013/11/14, MJ: rewrote most parts refering to setting and reading channel
!       indices. Added reading of config file different driver file necessary
!       now.
!    2013/11/14, MJ: changes lower and upper limits for ctp.
!    2013/11/18, MJ: Several additional changes, introduces ysolar_msi and
!       ythermal_msi
!    2013/11/25, MJ: initialized previously unitialized  Ctrl%Run_ID
!    2014/01/12, GM: Small fixes involving Ctrl%Ind%NAvail
!    2014/01/15, GM: No need for Ctrl%defaultSx anymore.
!    2014/01/15, GM: Added the ability to read the driver file contents from
!       standard input indicated by drifile .eq. '-'.
!    2014/01/16, GM: Use Selm* constants to set FG and AP instead of numbers.
!    2014/01/16, GM: Cleaned up code.
!    2014/01/31, MJ: Adds code for default surface reflection for avhrr
!                    (=modis for the time being)
!
! Bugs:
!    NViews should be changed for dual view
!    Not quite working for AVHRR
!
! IMPORTANT NOTE:
!   If a new type of LUT i.e aerosol is added then new default values will have
!   to be added to this routine
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_Driver(Ctrl, conf, message, drifile, status)

   use, intrinsic :: iso_fortran_env, only : input_unit

   use config_s
   use CTRL_def
   use ECP_constants

   implicit none

   ! Argument declarations
   type(CTRL_t),           intent(out)   :: Ctrl
   type(config_struct),    intent(out)   :: conf
   character(*),           intent(out)   :: message
   character(FilenameLen), intent(inout) :: drifile
   integer,                intent(out)   :: status

   ! Local variables
   integer                         :: i,ii,j,jj,k
   integer                         :: ios      ! Iostat value from file open, read etc.
   integer                         :: dri_lun  ! Unit number for driver file
   character(FilenameLen)          :: input_path,input_filename,scratch_dir,lut_dir
   character(FilenameLen)          :: suffix,outname
   logical                         :: file_exists, found
   real, allocatable, dimension(:) :: solar_store_sea,solar_store_land
   real, allocatable, dimension(:) :: ref_solar_sea,ref_solar_land


   status = 0

   !----------------------------------------------------------------------------
   ! Open the driver file
   !----------------------------------------------------------------------------

   ! If there is nothing in drifile because it was not passed, get it as env.
   ! variable
   if (len_trim(adjustl(drifile)) .eq. 0 ) then
      call get_environment_variable("ORAC_TEXTIN",drifile)
   endif

   ! If drifile is not '-' the check that the drifile exists
   if (drifile .ne. '-') then
      inquire(file=drifile, exist=file_exists)
      if (.not. file_exists) then
         status = DriverFileNotFound
         message = 'Read_Driver: Driver file not found'
         write(*,*)' Driver file pointed to by ORAC_DRIVER does not exist'
      endif
   endif

   write(*,*)'driver file: ',trim(drifile)

   ! If drifile is '-' read the file from standard input otherwise read drifile
   if (status == 0) then
      if (drifile .eq. '-') then
         dri_lun = input_unit
      else
         call Find_Lun(dri_lun)
         open(unit=dri_lun, file=drifile, iostat=ios)
         if (ios /= 0) then
            status = DriverFileOpenErr
            message = 'Read_Driver: unable to open driver file'
            write(*,*)' Unable to open driver file: ',trim(drifile)
         endif
      endif
   endif


   !----------------------------------------------------------------------------
   ! Read the driver file and the config file
   !----------------------------------------------------------------------------

   read(dri_lun, *, err=999, iostat=ios) input_path
   write(*,*)'input directory: ',trim(adjustl(input_path))

   read(dri_lun, *, err=999, iostat=ios) input_filename
   write(*,*)'input filename: ',trim(adjustl(input_filename))

   Ctrl%fid%input_filename=trim(adjustl(input_path))//'/'//trim(adjustl(input_filename))

   read(dri_lun, *, err=999, iostat=ios) scratch_dir
   write(*,*)'output directory: ', trim(adjustl(scratch_dir))

   read(dri_lun, *, err=999, iostat=ios) lut_dir
   write(*,*) 'lut_dir: ',trim(adjustl(lut_dir))

   read(dri_lun, *, err=999, iostat=ios) Ctrl%Inst%Name
   write(*,*)'Ctrl%Inst%Name: ',trim(adjustl(Ctrl%Inst%Name))

   ! Number of channels in preprocessing file
   ! (this is actually not really necessary as we have that in the config file)
   read(dri_lun, *, err=999, iostat=ios) Ctrl%Ind%NAvail
   write(*,*) 'Number of available channels in preproc files: ',Ctrl%Ind%NAvail

   suffix='.config.nc'
   Ctrl%FID%CONFIG=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   Ctrl%FID%CONFIG=trim(adjustl(Ctrl%FID%CONFIG))
   write(*,*) 'Ctrl%FID%CONFIG: ',Ctrl%FID%CONFIG

   ! Read config file in order to set all channel related info
   call read_config_file(Ctrl,conf)
   ! Check if input ok
   if (Ctrl%Ind%NAvail .ne. conf%nc) then
      write(*,*) 'ERROR: Ctrl%Ind%NAvail .ne. conf%nc: Problem with file or driver!', &
                 Ctrl%Ind%NAvail,conf%nc
      stop
   endif

   ! Read processing flag from driver
   allocate(conf%channel_proc_flag(conf%nc))
   read(dri_lun, *, err=999, iostat=ios)(conf%channel_proc_flag(i),i=1,conf%nc)
   if (sum(conf%channel_proc_flag) .lt. 1 .or. sum(conf%channel_proc_flag) .gt. conf%nc .or. &
      .not. &
      (any(conf%channel_proc_flag .eq. 0) .or. any(conf%channel_proc_flag .eq. 1)))  then
      write(*,*) 'ERROR: processing flag from driver wrong: ',conf%channel_proc_flag
      stop
   endif
   write(*,*) 'processing flag from driver: ',conf%channel_proc_flag

   ! Determine the number of channels to be used. Some of the follolwing is not
   ! (yet) used leave in there for potential later use for the time being
   Ctrl%Ind%NChans=sum(conf%channel_proc_flag)
   write(*,*) 'Ctrl%Ind%NChan: ',Ctrl%Ind%NChans

   ! These are the indices wrt the position of the channels in the preproc file
   ! which are to be used. Determine them from the indices in the file and proc.
   ! flag.
   allocate(Ctrl%Ind%Chi(Ctrl%Ind%NChans))
   Ctrl%Ind%Chi=0
   ii=0
   do i=1,conf%nc
      if (conf%channel_proc_flag(i) .eq. 1 ) then
         ii=ii+1
         Ctrl%Ind%Chi(ii)=conf%channel_ids_abs(i)
      endif
   enddo
   write(*,*) 'CHI: ',Ctrl%Ind%Chi

   ! These are the channels available in the file wrt numbering in the instrument
   allocate(Ctrl%Ind%Y_Id(conf%nc))
   Ctrl%Ind%y_id=conf%channel_ids_instr
   write(*,*) 'Y_ID: ',Ctrl%Ind%Y_Id

   ! Determine number of channels in lw,sw, and mixed in preproc_file
   conf%nsolar=0
   conf%nthermal=0
   conf%nmixed=0

   conf%nsolar_use=0
   conf%nthermal_use=0
   conf%nmixed_use=0

   conf%nsolar=sum(conf%channel_sw_flag)
   conf%nthermal=sum(conf%channel_lw_flag)

   allocate(conf%channel_sw_flag_use(Ctrl%Ind%NChans))
   conf%channel_sw_flag_use=0
   allocate(conf%channel_lw_flag_use(Ctrl%Ind%NChans))
   conf%channel_lw_flag_use=0
   allocate(conf%channel_mixed_flag_use(Ctrl%Ind%NChans))
   conf%channel_mixed_flag_use=0

   ii=0
   do i=1,conf%nc
      if (conf%channel_sw_flag(i) .eq. 1 .and. conf%channel_lw_flag(i) .eq. 1) then
         conf%nmixed=conf%nmixed+1
      endif

      ! These treat only the channels actually used
      if (conf%channel_proc_flag(i) .eq. 1) then
         ii=ii+1
         if (conf%channel_sw_flag(i) .eq. 1 ) then
            conf%nsolar_use=conf%nsolar_use+1
            conf%channel_sw_flag_use(ii)=1
         endif
         if (conf%channel_lw_flag(i) .eq. 1) then
            conf%nthermal_use=conf%nthermal_use+1
            conf%channel_lw_flag_use(ii)=1
         endif
         if (conf%channel_sw_flag(i) .eq. 1 .and. conf%channel_lw_flag(i) .eq. 1) then
            conf%nmixed_use=conf%nmixed_use+1
            conf%channel_mixed_flag_use(ii)=1
         endif
      endif
   enddo

   write(*,*) 'Flags solar, thermal and mixed of used channels:'
   write(*,*) 'conf%nsolar,conf%nthermal: ', &
              conf%nsolar,conf%nthermal
   write(*,*) 'conf%nsolar_use,conf%channel_sw_flag_use: ', &
              conf%nsolar_use,conf%channel_sw_flag_use
   write(*,*) 'conf%nthermal_use,conf%channel_lw_flag_use: ', &
              conf%nthermal_use,conf%channel_lw_flag_use
   write(*,*) 'conf%nmixed_use,conf%channel_mixed_flag_use: ', &
              conf%nmixed_use,conf%channel_mixed_flag_use

   ! Read in cloud class (aka phase of no aerosols processed)
   read(dri_lun, *, err=999, iostat=ios) Ctrl%CloudClass%Name
   write(*,*)'Ctrl%CloudClass%Name: ',trim(adjustl(Ctrl%CloudClass%Name))


   !----------------------------------------------------------------------------
   ! Set the rest of the Ctrl structure
   !----------------------------------------------------------------------------

   ! Set filenames

   Ctrl%Run_ID=trim(adjustl(scratch_dir))
   write(*,*)'Ctrl%Run_ID: ',trim(adjustl(Ctrl%Run_ID))

   Ctrl%Data_Dir=trim(adjustl(scratch_dir))//'/'
   write(*,*)'Ctrl%Data_Dir: ',trim(adjustl(Ctrl%Data_Dir))

   Ctrl%Out_Dir=trim(adjustl(scratch_dir))//'/'
   write(*,*)'Ctrl%out_Dir: ',trim(adjustl(Ctrl%out_Dir))

   Ctrl%SAD_Dir=trim(adjustl(lut_dir))//'/'
   write(*,*)'Ctrl%SAD_Dir: ',trim(adjustl(Ctrl%SAD_Dir))

   suffix='.msi.nc'
   Ctrl%FID%MSI=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   Ctrl%FID%MSI=trim(adjustl(Ctrl%FID%MSI))
   write(*,*)'Ctrl%FID%MSI: ',trim(adjustl(Ctrl%FID%MSI))
   suffix='.lwrtm.nc'
   Ctrl%FID%LWRTM=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   write(*,*) 'Ctrl%FID%LWRTM: ',trim(adjustl(Ctrl%FID%LWRTM))
   suffix='.swrtm.nc'
   Ctrl%FID%SWRTM=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   suffix='.prtm.nc'
   Ctrl%FID%PRTM=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   suffix='.lsf.nc'
   Ctrl%FID%LS=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   suffix='.clf.nc'
   Ctrl%FID%CF=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   suffix='.geo.nc'
   Ctrl%FID%Geo=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   suffix='.loc.nc'
   Ctrl%FID%Loc=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   suffix='.uv.nc'
   Ctrl%FID%uv=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   suffix='.alb.nc'
   Ctrl%FID%Aux=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))

   outname=trim(adjustl(scratch_dir))//'/'//trim(adjustl(input_filename))//&
      trim(adjustl(Ctrl%CloudClass%Name))
   Ctrl%FID%L2_primary_outputpath_and_file=trim(adjustl(outname))//'.primary.nc'
   write(*,*) 'Ctrl%FID%L2_primary_outputpath_and_file: ', &
              Ctrl%FID%L2_primary_outputpath_and_file
   Ctrl%FID%L2_secondary_outputpath_and_file=trim(adjustl(outname))//'.secondary.nc'
   write(*,*) 'Ctrl%FID%L2_secondary_outputpath_and_file: ', &
              Ctrl%FID%L2_secondary_outputpath_and_file

   Ctrl%FID%Log=trim(adjustl(outname))//'.log'
   write(*,*)'Ctrl%FID%Log: ',trim(adjustl(Ctrl%FID%Log))
   Ctrl%FID%Diag=trim(adjustl(outname))//'.diag'
   write(*,*)'Ctrl%FID%Diag: ',trim(adjustl(Ctrl%FID%Diag))
   Ctrl%FID%BkP=trim(adjustl(outname))//'bkp'
   write(*,*)'Ctrl%FID%BkP: ',trim(adjustl(Ctrl%FID%BkP))


   ! The level of breakpoint output when the code is comiled with breakpoint
   ! option Ctrl%Bkpl
   Ctrl%Bkpl=2

   ! Set diagnostic flags
   Ctrl%Diagl(1)  = 1
   Ctrl%Diagl(2)  = 1
   Ctrl%Diagl(3)  = 1
   Ctrl%Diagl(4)  = 1
   Ctrl%Diagl(5)  = 1
   Ctrl%Diagl(6)  = 1
   Ctrl%Diagl(7)  = 0
   Ctrl%Diagl(8)  = 0
   Ctrl%Diagl(9)  = 1
   Ctrl%Diagl(10) = 1
   Ctrl%Diagl(11) = 1
   Ctrl%Diagl(12) = 1
   Ctrl%Diagl(13) = 0
   Ctrl%Diagl(14) = 0

   Ctrl%RTMIntflag = RTMIntMethLinear
   Ctrl%LUTIntflag = LUTIntMethLinear

   Ctrl%MaxSatZen  = 90 ! max satellite zenith angle
   Ctrl%MaxSolZen  = 80 ! max solar zenith angle > 90 = night image
   Ctrl%Sunset     = 90 ! used to set twilight option


   ! Set stuff in Ctrl%Ind

   Ctrl%Ind%Ny = Ctrl%Ind%NChans

   Ctrl%Ind%Ws = 0 ! warm start option i.e enables user to start partway through
                   ! a scene

   ! This stores for the used channels the number of channels, where mixed
   ! channels are included in both on the l.h.s.
   Ctrl%Ind%Nsolar=conf%nsolar_use
   Ctrl%Ind%Nthermal=conf%nthermal_use

   ! Use to assign to measurement indices
   allocate(Ctrl%Ind%Ysolar(Ctrl%Ind%Nsolar))
   Ctrl%Ind%Ysolar=-1
   allocate(Ctrl%Ind%Ythermal(Ctrl%Ind%Nthermal))
   Ctrl%Ind%Ythermal=-1

   allocate(Ctrl%Ind%Ysolar_msi(Ctrl%Ind%Nsolar))
   Ctrl%Ind%Ysolar_msi=-1
   allocate(Ctrl%Ind%Ythermal_msi(Ctrl%Ind%Nthermal))
   Ctrl%Ind%Ythermal_msi=-1

   ii=0
   jj=0
   do i=1,Ctrl%Ind%NChans
      if (conf%channel_sw_flag_use(i) .eq. 1 ) then
         ii=ii+1
         Ctrl%Ind%Ysolar(ii)=Ctrl%Ind%Chi(i) ! these are the indices wrt the preproc file
         Ctrl%Ind%Ysolar_msi(ii)=i           ! these are the indices wrt the order in the MSI array
      endif
      if (conf%channel_lw_flag_use(i) .eq. 1) then
         jj=jj+1
         Ctrl%Ind%Ythermal(jj)=Ctrl%Ind%Chi(i)! these are the indices wrt the preproc file
         Ctrl%Ind%Ythermal_msi(jj)=i          ! these are the indices wrt the order in the MSI array
      endif
   enddo

   write(*,*) 'Ctrl%Ind%Ysolar/msi: ',Ctrl%Ind%Ysolar,Ctrl%Ind%Ysolar_msi
   write(*,*) 'Ctrl%Ind%Ythermal/msi: ',Ctrl%Ind%Ythermal,Ctrl%Ind%Ythermal_msi


   ! For each of the day, twilight, night active state variable arrays, read the
   ! array and set up the corresponding inactive array.

   ! Day options
   Ctrl%Ind%Nx_Dy = 5   ! number of active state variables

   Ctrl%Ind%X_Dy(1) = 1 ! indices of state parameters
   Ctrl%Ind%X_Dy(2) = 2
   Ctrl%Ind%X_Dy(3) = 3
   Ctrl%Ind%X_Dy(4) = 4
   Ctrl%Ind%X_Dy(5) = 5

   Ctrl%Ind%NxI_Dy = MaxStateVar - Ctrl%Ind%Nx_Dy

   ! Set active and inactive state variables
   k = 0
   do i=1, MaxStateVar
      found = .false.
      do j=1, Ctrl%Ind%Nx_Dy ! Look for the variable index in the active array
         if (Ctrl%Ind%X_Dy(j) == i) then
            found = .true.
            exit
         endif
      end do
      if (.not. found) then ! Not found in active set
         k = k + 1          ! Add to inactive array
         Ctrl%Ind%XI_Dy(k) = i
      endif
      if (k == Ctrl%Ind%NxI_Dy) exit ! Correct number of values found
   end do

   ! Twilight options
   Ctrl%Ind%Nx_Tw = 3   ! number of active state variables

   Ctrl%Ind%X_Tw(1) = 3 ! indices of state parameters
   Ctrl%Ind%X_Tw(2) = 4
   Ctrl%Ind%X_Tw(3) = 5

   Ctrl%Ind%NxI_Tw = MaxStateVar - Ctrl%Ind%Nx_Tw

   k = 0
   do i=1, MaxStateVar
      found = .false.
      do j=1, Ctrl%Ind%Nx_Tw ! Look for the variable index in the active array
         if (Ctrl%Ind%X_Tw(j) == i) then
            found = .true.
            exit
         endif
      end do
      if (.not. found) then ! Not found in active set
         k = k + 1          ! Add to inactive array
         Ctrl%Ind%XI_Tw(k) = i
      endif
      if (k == Ctrl%Ind%NxI_Tw) exit ! Correct number of values found
   end do

   ! Night options
   Ctrl%Ind%Nx_Ni = 3   ! number of active state variables

   Ctrl%Ind%X_Ni(1) = 3 ! indices of state parameters
   Ctrl%Ind%X_Ni(2) = 4
   Ctrl%Ind%X_Ni(3) = 5

   Ctrl%Ind%NxI_Ni = MaxStateVar - Ctrl%Ind%Nx_Ni

   k = 0
   do i=1, MaxStateVar
      found = .false.
      do j=1, Ctrl%Ind%Nx_Ni ! Look for the variable index in the active array
         if (Ctrl%Ind%X_Ni(j) == i) then
            found = .true.
            exit
         endif
      end do
      if (.not. found) then ! Not found in active set
         k = k + 1          ! Add to inactive array
         Ctrl%Ind%XI_Ni(k) = i
      endif
      if (k == Ctrl%Ind%NxI_Ni) exit ! Correct number of values found
   end do

   Ctrl%Ind%NViews=1
   allocate(Ctrl%Ind%viewidx(Ctrl%Ind%nchans))

   do i=1,Ctrl%Ind%nchans
      Ctrl%Ind%Viewidx(i)=1
   end do


   Ctrl%CloudClass%Id = 1


   Ctrl%CloudType     = 1 ! use this to select which coreg/homog errors to use


   ! Set a priori options (Tau,Re,Pc,F,Ts)

   ! Day
   Ctrl%AP(1,1)=SelmCtrl
   Ctrl%AP(2,1)=SelmCtrl
   Ctrl%AP(3,1)=SelmCtrl
   Ctrl%AP(4,1)=SelmMeas
   Ctrl%AP(5,1)=SelmAux

   ! Twilight
   Ctrl%AP(1,2)=SelmCtrl
   Ctrl%AP(2,2)=SelmCtrl
   Ctrl%AP(3,2)=SelmCtrl
   Ctrl%AP(4,2)=SelmMeas
   Ctrl%AP(5,2)=SelmAux

   ! Night
   Ctrl%AP(1,3)=SelmCtrl
   Ctrl%AP(2,3)=SelmCtrl
   Ctrl%AP(3,3)=SelmCtrl
   Ctrl%AP(4,3)=SelmMeas
   Ctrl%AP(5,3)=SelmAux


   ! Set first guess options (Tau,Re,Pc,F,Ts)
   ! What do the constants mean:
   ! SelmCtrl: Static apriori variable i.e does not change
   ! SelmMeas: Use a dynamically chosen first guess dependant on measurements
   ! SelmAux: Use a value from an external auxillary file
   ! Ctrl%FG(statevariable, time of day)

   ! Day
   Ctrl%FG(1,1) = SelmCtrl
   Ctrl%FG(2,1) = SelmCtrl
   Ctrl%FG(3,1) = SelmMeas ! from ir profile
   Ctrl%FG(4,1) = SelmCtrl
   Ctrl%FG(5,1) = SelmAux  ! from auxillary file

   ! Twilight
   Ctrl%FG(1,2) = SelmCtrl
   Ctrl%FG(2,2) = SelmCtrl
   Ctrl%FG(3,2) = SelmMeas ! from ir profile
   Ctrl%FG(4,2) = SelmCtrl
   Ctrl%FG(5,2) = SelmAux  ! from auxillary file

   ! Night
   Ctrl%FG(1,3) = SelmCtrl
   Ctrl%FG(2,3) = SelmCtrl
   Ctrl%FG(3,3) = SelmMeas ! from ir profile
   Ctrl%FG(4,3) = SelmCtrl
   Ctrl%FG(5,3) = SelmAux  ! from auxillary file


   ! Set default a priori values. Quite often these values with a very high
   ! uncertainty
   if (trim(Ctrl%CloudClass%Name) .eq. 'WAT') then
      Ctrl%XB(1) = 0.8
      Ctrl%XB(2) = 12.
      Ctrl%XB(3) = 700.
      Ctrl%XB(4) = 1.
      Ctrl%XB(5) = 300.0
   else if (trim(Ctrl%CloudClass%Name) .eq. 'ICE') then
      Ctrl%XB(1) = 0.8
      Ctrl%XB(2) = 30.
      Ctrl%XB(3) = 400.
      Ctrl%XB(4) = 1.
      Ctrl%XB(5) = 300.0
   else if (trim(Ctrl%CloudClass%Name) .eq. 'EYJ') then
      Ctrl%XB(1) = 0.8
      Ctrl%XB(2) = 0.5
      Ctrl%XB(3) = 1000.
      Ctrl%XB(4) = 1.
      Ctrl%XB(5) = 300.
   else if (trim(Ctrl%CloudClass%Name) .eq. 'MAR') then
      Ctrl%XB(1) = 0.1
      Ctrl%XB(2) = 1.8
      Ctrl%XB(3) = 1000.
      Ctrl%XB(4) = 1.
      Ctrl%XB(5) = 300.
   else if (trim(Ctrl%CloudClass%Name) .eq. 'DES') then
      Ctrl%XB(1) = 0.0
      Ctrl%XB(2) = 1.4
      Ctrl%XB(3) = 1000.
      Ctrl%XB(4) = 1.
      Ctrl%XB(5) = 300.
   endif


   ! Set default first guess default values
   if (trim(Ctrl%CloudClass%Name) .eq. 'WAT') then
      Ctrl%X0(1) = 0.8
      Ctrl%X0(2) = 12.
      Ctrl%X0(3) = 700.
      Ctrl%X0(4) = 1.
      Ctrl%X0(5) = 300.
   else if (trim(Ctrl%CloudClass%Name) .eq. 'ICE') then
      Ctrl%X0(1) = 0.8
      Ctrl%X0(2) = 30.
      Ctrl%X0(3) = 400.
      Ctrl%X0(4) = 1.
      Ctrl%X0(5) = 300.

   ! For a small selection of aerosol types
   else if (trim(Ctrl%CloudClass%Name) .eq. 'EYJ') then
      Ctrl%X0(1) = 0.8
      Ctrl%X0(2) = 0.5
      Ctrl%X0(3) = 1000.
      Ctrl%X0(4) = 1.
      Ctrl%X0(5) = 300.
   else if (trim(Ctrl%CloudClass%Name) .eq. 'MAR') then
      Ctrl%X0(1) = 0.1
      Ctrl%X0(2) = 1.8
      Ctrl%X0(3) = 1000.
      Ctrl%X0(4) = 1.
      Ctrl%X0(5) = 300.
   else if (trim(Ctrl%CloudClass%Name) .eq. 'DES') then
      Ctrl%X0(1) = 0.0
      Ctrl%X0(2) = 1.4
      Ctrl%X0(3) = 1000.
      Ctrl%X0(4) = 1.
      Ctrl%X0(5) = 300.
   endif

   ! Set default a priori error covariance
   if ((trim(Ctrl%CloudClass%Name) .eq. 'EYJ' ) .or. &
       (trim(Ctrl%CloudClass%Name) .eq. 'MAR' ) .or. &
       (trim(Ctrl%CloudClass%Name) .eq. 'DES' ) ) then
      Ctrl%Sx(1) = 1.0e+01 ! optical depth
      Ctrl%Sx(2) = 1.0e-01 ! effective radii
      Ctrl%Sx(3) = 1.0e+06 ! ctp
      Ctrl%Sx(4) = 1.0e-10 ! fraction
      Ctrl%Sx(5) = 1.0e+00 ! surface temperature
   else
      Ctrl%Sx(1) = 1.0e+08 ! optical depth
      Ctrl%Sx(2) = 1.0e+08 ! effective radii
      Ctrl%Sx(3) = 1.0e+06 ! ctp
      Ctrl%Sx(4) = 1.0e-10 ! fraction
      Ctrl%Sx(5) = 1.0e+00 ! surface temperature
   endif

   Ctrl%Max_SDAD = 10 ! No. of pixels where state is valid for SDAD setting


   allocate(Ctrl%Sy(Ctrl%Ind%Ny,Ctrl%Ind%Ny))


   ! Set Ctrl%RS

   ! Look at the channel numbers and detremine what combination of vis/mixed/ir
   ! channels. This is instrument dependant so if introducing a new instrument
   ! channel info needs to be stored here
   allocate(solar_store_sea(conf%nc))
   allocate(solar_store_land(conf%nc))

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
   endif

   ! Set default surface reflectance values for channels used. This just maps
   ! the values from all channels in the file to the ones which will be actually
   ! used
   Ctrl%RS%Flag  = 3  ! Surface Ref: Flag (1-Ctrl 3-Aux) (1-Ctrl 3-Aux)

   allocate(ref_solar_sea(Ctrl%Ind%NChans))
   ref_solar_sea=0.0
   allocate(ref_solar_land(Ctrl%Ind%NChans))
   ref_solar_land=0.0

   ii=1
   do i=1,conf%nc
      if (conf%channel_proc_flag(i) .eq. 1) then
         ref_solar_sea(ii)=solar_store_sea(i)
         ref_solar_land(ii)=solar_store_land(i)
         ii=ii+1
      endif
   enddo
   deallocate(solar_store_sea)
   deallocate(solar_store_land)

   allocate(Ctrl%RS%B(Ctrl%Ind%Nsolar,2))
   Ctrl%RS%B=0.0
   Ctrl%RS%B(1:Ctrl%Ind%Nsolar,1) = ref_solar_sea(1:Ctrl%Ind%Nsolar)/100.0 ! valid for 0.67/0.87/1.6 channels
   Ctrl%RS%B(1:Ctrl%Ind%Nsolar,2) = ref_solar_land(1:Ctrl%Ind%Nsolar)/100.0
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

   Ctrl%Invpar%Mqstart   = 0.001      ! Marquardt: starting parameter
   Ctrl%Invpar%Mqstep    = 10.0       ! step parameter
   Ctrl%Invpar%Maxiter   = 40         ! Maximum # of iterations
   Ctrl%Invpar%Maxphase  = 3          ! Maximum # of phase changes
   Ctrl%Invpar%Ccj       = 0.05       ! Cost convergence criteria

   Ctrl%Invpar%Xscale(1) = 10.0       ! Scaling parameters (Tau,Re,Pc,F,Ts)
   Ctrl%Invpar%Xscale(2) = 1.0
   Ctrl%Invpar%Xscale(3) = 1.0
   Ctrl%Invpar%Xscale(4) = 1000.0
   Ctrl%Invpar%Xscale(5) = 1.0

   Ctrl%Invpar%Xllim(1)  = -3.0

   if ((trim(Ctrl%CloudClass%Name) .eq. 'EYJ' ) .or. &
       (trim(Ctrl%CloudClass%Name) .eq. 'MAR' ) .or. &
       (trim(Ctrl%CloudClass%Name) .eq. 'DES' ) ) then
      Ctrl%Invpar%Xllim(2) = 0.01
   else
      Ctrl%Invpar%Xllim(2) = 0.1
   endif
   Ctrl%Invpar%Xllim(3)  = 10.0
   Ctrl%Invpar%Xllim(4)  = 1.0
   Ctrl%Invpar%Xllim(5)  = 250.0

   ! Upper limit on state Ctrl.Invpar.Xulim
   Ctrl%Invpar%Xulim(1) = 2.408
   if (trim(Ctrl%CloudClass%Name) .eq. 'WAT') then
      Ctrl%Invpar%Xulim(2) = 35.0
   else if (trim(Ctrl%CloudClass%Name) .eq. 'ICE') then
      Ctrl%Invpar%Xulim(2) = 100.0
   else if ((trim(Ctrl%CloudClass%Name) .eq. 'EYJ' ) .or. &
            (trim(Ctrl%CloudClass%Name) .eq. 'MAR' ) .or. &
            (trim(Ctrl%CloudClass%Name) .eq. 'DES' ) ) then
      Ctrl%Invpar%Xulim(2) = 20.0
   endif

   Ctrl%Invpar%Xulim(3) = 1200.0
   Ctrl%Invpar%Xulim(4) = 1.0
   Ctrl%Invpar%Xulim(5) = 320.0


   ! Set Ctrl%QC

   ! Maximum acceptable retrieval cost
   Ctrl%QC%MaxJ = 10.0

   ! Maximum acceptable retrieval errors
   Ctrl%QC%MaxS(1) = 0.08
   Ctrl%QC%MaxS(2) = 3.0
   Ctrl%QC%MaxS(3) = 200.
   Ctrl%QC%MaxS(4) = 0.2
   Ctrl%QC%MaxS(5) = 2.0


   !----------------------------------------------------------------------------
   ! Now do some checks
   !----------------------------------------------------------------------------
   if (status == 0) then
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
                  status = FGMethErr
                  message = 'Read_Driver: MDAD method not supported ' // &
                            'for setting first guess Re, Ts'
               endif

            case (SelmAux)
               if (i /= ITs) then
                  status = FGMethErr
                  message = 'Read_Driver: AUX method ONLY supported ' // &
                            'for setting first guess Ts'
               endif

            case default
               status = FGMethErr
               write(unit=message, fmt=*) 'Read_Driver: Invalid method ', &
                                          'for first-guess state variable ',i
            end select
         end do
      end do
   endif ! end of status check

   if (status == 0) then
      ! Check validity of a priori selection options. Not all legal values are
      ! supported for all variables.

      do j=1, 3
         do i=1, MaxStateVar
            select case (Ctrl%Ap(i,j))
            case (SelmCtrl)
               continue

            case (SelmMeas)
               if (i == IRe .or. i == ITs) then
                  status = APMethErr
                  message = 'Read_Driver: MDAD method not supported for ' //&
                            'setting a priori Re, Ts'
               endif

            case (SelmAux)
               if (i /= ITs) then
                  status = APMethErr
                  message = 'Read_Driver: AUX method ONLY supported ' // &
                            'for setting a priori Ts'
               endif

            case default
               status = APMethErr
               write(unit=message, fmt=*) &
                  'Read_Driver: Invalid method for a priori state variable ',i
            end select
         end do
      end do
   endif ! end of status check

   if (status == 0) then
      ! Check validity of surface reflectance flag

      select case (Ctrl%RS%Flag)
      case (SelmCtrl, SelmAux)
         continue
      case (SelmMeas)
         status = GetSurfaceMeth
         message = 'Read_Driver: surface reflectance method not supported'
      case default
         status = GetSurfaceMeth
         message = 'Read_Driver: invalid surface reflectance method'
      end select
      ! Warning - status value may be lost if a read error occurs later on in
      ! this routine.
   endif ! end of status==0 check

999 if (ios /= 0) then          ! Don't write error messages out here.
      status = DriverFileReadErr ! Log file not open yet.
      message = 'Read_Driver: error reading driver file'
   endif

   if (drifile .ne. '-') then
      close(unit=dri_lun)
   endif

end subroutine Read_Driver
