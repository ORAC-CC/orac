! Name:
!   Read_Driver
!
! Purpose:
! Stores values required by driver file. This code is intended to
!  replace the idl write_idriver.pro
!
! Description:
!   Reads the values from the "driver" file used to set run-time options
!   into the CTRL structure.
!
! Arguments:
!   Name       Type    In/Out/Both    Description
!   Ctrl       Ctrl_t  out            Control struct defined in CTRL_def
!   message    string  inout          Error message returned to calling routine
!   status     int     out            Status returned to calling function
!
! Algorithm:
!
! History:
!
!   15th May  2012, Caroline Poulsen created original file to reapce ReadDriver
!   8th July  2012, Caroline Poulsen fixed memory leaks
!  2012/07/13 MJ implements option to read drifile path from command line
!  13th July  2012, Caroline Poulsen changed ref_solar_sea nd ref_solar_land to! reals rather than integers.
! 2012/07/26 MJ makes some small changes, paths passed to ORAC via driver file must not have trailing 
!                     forward slashes ("/"). Those are explicitly added in this routine where necessary. 
!29th July  2012, Caroline Poulsen fixed bug in twilight/night statevariable numbering
!10th Aug  2012, Caroline Poulsen modified state parameter indives for night
!20120822 MJ makes adaptions to read netcdf files 
! 2012/09/15 CP removed double allocation of vieidx
! 2012/10/01 CP changed active variables at night to be ctp fraction and surface temperature, changed how first guess of 
!               CTp calculated at night i.e matching temperature profile changed apriori errors of state vecto
! 2012/10/01 CP added in default sx category as was not being resset after each pixel was processed
! 2012/10/01  MJ changed definition of AVHRR SW variables
! 2012/11/22  CP fixed bug in index_ir AATSR defintion
! 2013/03/18  CP modified upper limits of ice and water effective radius
! 2013/03/18  CP added in more comments for clarity
! 2013/06/27 MJ implemented reading of path to driver file either from environme
!nt variable or passed from outside
! 2013/10/02 CP added comments for GT added aerosl classes for Bayesian cloud id
! 2013/11/14 MJ rewrote most parts refering to setting and reading channel indices. added reading of config file
!                    Different driver file necessary now.
!2013/11/14 MJ changes lower and upper limits for ctp.
!20131118 Several additional changes, introduces ysolar_msi and ythermal_msi
!20131125 MJ initialized previously unitialized  Ctrl%Run_ID
! Bugs:
! nviews should be changed for dual view
! not quiteworking for AVHRR
!
!IMPORTANT NOTE:
! if a new type of LUT i.e aerosol is added then new default values will
! have to be added to this routine
!   None known.
!
!---------------------------------------------------------------------
subroutine Read_Driver (Ctrl, conf,message, drifile,status)

   use ECP_constants
   use CTRL_def
   use config_s

   implicit none

   type(CTRL_t) :: Ctrl
   type(config_struct) :: conf
   integer, intent(inout) :: status
   character(*), intent(inout) :: message 
                                    ! Error message returned to calling routine

!  Local variables

   integer              :: dri_lun  ! Unit number for driver file
   integer              :: ios      ! Iostat value from file open, read etc.
   integer              :: i, ii,j,jj, jin,k ,nsolar,nthermal,jcount ! Counters for loops in reads etc
   integer, dimension(2)           :: rangev  ! specifies size of granule
!   real, dimension(4)           :: solar_store_sea,solar_store_land

   character(2048)       :: DriFile,root,scratch_dir,outname,lut_dir  ! Driver file name and path
   character(2048) :: input_filename,input_path
   character(len=12) :: suffix
   logical              :: file_exists ! Result of inquire on driver file  
   integer,  allocatable            :: index_solar(:),index_ir(:),index_mixed(:)
   real,  allocatable, dimension(:)  :: solar_store_sea,solar_store_land
   integer,   allocatable            ::   pos_solar(:),pos_thermal(:),pos_mixed(:)
   real, allocatable, dimension(:)  ::  ref_solar_sea,ref_solar_land
   integer :: nindex_solar,nindex_ir,nindex_mixed,navail
!   external getenv      ! Declare getenv function on DEC UNIX systems
                        ! Remove for use on Linux system with Absoft compiler
   logical              :: found     ! Used when checking state vectors

!  Translate the environment variable for the driver file

   !if there is nothing in drifile because it was not passed, get it as env. variable
   if(len_trim(adjustl(drifile)) .eq. 0 ) then

      call get_environment_variable("ORAC_TEXTIN",drifile)
      !call getenv('ORAC_TEXTIN',DriFile)
      write(*,*) 'file_exists',DriFile

   endif


   inquire(file=DriFile, exist=file_exists)
   if (.not. file_exists) then
      status = DriverFileNotFound
      message = 'Read_Driver: Driver file not found'
      write(*,*)' Driver file pointed to by ORAC_DRIVER does not exist'
   end if

   write(*,*)' driver: ',trim(DriFile),status
   
   !
   !two options here can either read a file or from a script
   !if environment variable not set read default driver
   !

   if (status == 0) then     
      call Find_Lun(dri_lun)
      open(unit=dri_lun, file=DriFile, iostat=ios)
      if (ios /= 0) then
         status = DriverFileOpenErr
         message = 'Read_Driver: unable to open driver file'
         write(*,*)' Unable to open driver file: ',trim(DriFile)
      end if
   end if

   ! read the driver file
   !  Read the data, filename, instrument,chanell id, phase to process

   read(dri_lun, *, err=999, iostat=ios) input_path
   write(*,*)'input directory:',trim(adjustl(input_path))
   read(dri_lun, *, err=999, iostat=ios) input_filename
   write(*,*)'input filename:',trim(adjustl(input_filename))
   Ctrl%fid%input_filename=trim(adjustl(input_path))//'/'//trim(adjustl(input_filename))
   write(*,*)' Ctrl%fid%input_filename',  trim(adjustl(Ctrl%fid%input_filename))
   
   read(dri_lun, *, err=999, iostat=ios)   scratch_dir
   write(*,*)'output directory:', trim(adjustl(scratch_dir))
   
   read(dri_lun, *, err=999, iostat=ios)   lut_dir
   write(*,*) 'lut_dir:',trim(adjustl(lut_dir))
   
   read(dri_lun, *, err=999, iostat=ios) Ctrl%Inst%Name
   write(*,*)'Ctrl%Inst%Name',trim(adjustl(Ctrl%Inst%Name))
   !MJ ORGread(dri_lun, *, err=999, iostat=ios) Ctrl%Ind%NChans ,Navail!to actually use

   !number of channels in preprocessing file
   !(this is actually not really necessary as we have that in the config file)
   read(dri_lun, *, err=999, iostat=ios) Navail
   

   write(*,*)  'Number of available channels in preproc files',Navail

   suffix='.config.nc'
   Ctrl%FID%CONFIG=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   Ctrl%FID%CONFIG=trim(adjustl(Ctrl%FID%CONFIG))   
   !write(*,*) 'Ctrl%FID%CONFIG',Ctrl%FID%CONFIG
   !read config file in order to set all channel related info
   call read_config_file(Ctrl,conf)
   !check if input ok
   if(navail .ne. conf%nc) then
      write(*,*) 'ERROR: conf%nc .ne. navail: Problem with file or driver!)',navail,conf%nc
      stop
   endif

   !read processing flag from driver
   allocate(conf%channel_proc_flag(conf%nc))
   read(dri_lun, *, err=999, iostat=ios)(conf%channel_proc_flag(i),i=1,conf%nc)  
   if(sum(conf%channel_proc_flag) .lt. 1 .or. sum(conf%channel_proc_flag) .gt. conf%nc .or. &
        & any(conf%channel_proc_flag .lt. 0) .or. any(conf%channel_proc_flag .gt. 1))  then

      write(*,*) 'ERROR: processing flag from driver wrong:',conf%channel_proc_flag
      stop

   endif
   write(*,*) 'processing flag from driver:',conf%channel_proc_flag

   !determine the number of channels to be used
   !some of the follolwing is not (yet) used
   !leave in there for potential later use for the time being
   Ctrl%Ind%NChans=sum(conf%channel_proc_flag)
   allocate(ref_solar_sea(Ctrl%Ind%NChans))
   ref_solar_sea=0.00
   allocate(ref_solar_land(Ctrl%Ind%NChans))
   ref_solar_land=0.00
   write(*,*) 'Ctrl%Ind%NChan',Ctrl%Ind%NChans 

   !these are the indices wrt the position of the channels in the preproc file
   !which are to be used.
   !determine them from the indices in the file and proc. flag
   allocate(Ctrl%Ind%Chi(Ctrl%Ind%NChans))
   Ctrl%Ind%Chi=0 
   ii=0
   do i=1,conf%nc
      if(conf%channel_proc_flag(i) .eq. 1 ) then
         ii=ii+1
         Ctrl%Ind%Chi(ii)=conf%channel_ids_abs(i)
      endif
   enddo

   allocate(conf%channel_sw_flag_use(Ctrl%Ind%NChans))
   conf%channel_sw_flag_use=0
   allocate(conf%channel_lw_flag_use(Ctrl%Ind%NChans))
   conf%channel_lw_flag_use=0
   allocate(conf%channel_mixed_flag_use(Ctrl%Ind%NChans))
   conf%channel_mixed_flag_use=0

   !these are the channels available in the file wrt 
   !numbering in the instrument
   allocate(Ctrl%Ind%y_id(conf%nc))
   Ctrl%Ind%y_id=conf%channel_ids_instr

   write(*,*) 'CHI',Ctrl%Ind%Chi
   write(*,*) 'Y_ID',Ctrl%Ind%y_id 

   !determine number of channels in lw,sw, and mixed in preproc_file
   conf%nsolar=0
   conf%nthermal=0
   conf%nmixed=0

   conf%nsolar_use=0
   conf%nthermal_use=0
   conf%nmixed_use=0


   conf%nsolar=sum(conf%channel_sw_flag)
   conf%nthermal=sum(conf%channel_lw_flag)
   ii=0
   do i=1,conf%nc

      if(conf%channel_sw_flag(i) .eq. 1 .and. conf%channel_lw_flag(i) .eq. 1) then
         conf%nmixed=conf%nmixed+1
      endif

      !these treats only the channels actually used
      if(conf%channel_proc_flag(i) .eq. 1) then
         ii=ii+1
         if(conf%channel_sw_flag(i) .eq. 1 ) then
            conf%nsolar_use=conf%nsolar_use+1   
            conf%channel_sw_flag_use(ii)=1
         endif
         if(conf%channel_lw_flag(i) .eq. 1) then
            conf%nthermal_use=conf%nthermal_use+1         
            conf%channel_lw_flag_use(ii)=1
         endif
         if(conf%channel_sw_flag(i) .eq. 1 .and. conf%channel_lw_flag(i) .eq. 1) then
            conf%nmixed_use=conf%nmixed_use+1         
            conf%channel_mixed_flag_use(ii)=1
         endif
      endif
   enddo

   write(*,*) 'Flags solar,thermal and mixed of used channels'
   write(*,*) conf%nsolar, conf%nthermal
   write(*,*) conf%nsolar_use,conf%channel_sw_flag_use
   write(*,*) conf%nthermal_use,conf%channel_lw_flag_use
   write(*,*) conf%nmixed_use,conf%channel_mixed_flag_use

   !read in cloud class (aka phase of no aerosols processed)
   read(dri_lun, *, err=999, iostat=ios) Ctrl%CloudClass%Name
   write(*,*)'Ctrl%CloudClass%Name',trim(adjustl(Ctrl%CloudClass%Name))

   ! look at the channel numbers and detremine what combination of
   !  vis/mixed/ir channels this is instrument dependant so if
   ! introducing a new instrument channel info needs to be stored here
   
   write(*,*)'Ctrl%Inst%Name',trim(adjustl(Ctrl%Inst%Name))

   allocate(solar_store_sea(conf%nc))
   allocate(solar_store_land(conf%nc))

   !set some default arrays for surface reflection   
   if ((trim(Ctrl%Inst%Name) .eq. trim('MODIS-AQUA')) .or.&
        & (trim(Ctrl%Inst%Name) .eq. trim('MODIS-TERRA')) ) then
      !
      !modis solar channels 1-19,26 mixed 20-23 ir 24-36
      !
      !default reflectance values:


      !
      ! these variables are default value of surface reflectance
      !            
      solar_store_sea(1)=2.00
      solar_store_sea(2)=1.00
      solar_store_sea(3)=0.00
      solar_store_sea(4)=0.00
      solar_store_sea(5)=0.00
      solar_store_sea(6)=1.00

      solar_store_land(1)=10.0
      solar_store_land(2)=1.0
      solar_store_land(3)=0.0
      solar_store_land(4)=0.0
      solar_store_land(5)=0.0
      solar_store_land(6)=1.0


!!$      allocate(index_solar(20))
!!$      allocate(index_mixed(5))
!!$      allocate(index_ir(13))
!!$      
!!$      
!!$      index_solar=0
!!$      index_mixed=0
!!$      index_ir=0
!!$
!!$
!!$!
!!$!index_solar/mixed/ir contains the modis channels which are solar/mixed/ir
!!$!this uses modis channel numbering
!!$      
!!$      index_solar(1)=1
!!$      index_solar(2)=2
!!$      index_solar(3)=3
!!$      index_solar(4)=4
!!$      index_solar(5)=5
!!$      index_solar(6)=6
!!$      index_solar(7)=7
!!$      index_solar(8)=8
!!$      index_solar(9)=9
!!$      index_solar(10)=10
!!$      index_solar(11)=11
!!$      index_solar(12)=12
!!$      index_solar(13)=13
!!$      index_solar(14)=14
!!$      index_solar(15)=15
!!$      index_solar(16)=16
!!$      index_solar(17)=17
!!$      index_solar(18)=18
!!$      index_solar(19)=19
!!$      index_solar(20)=26
!!$      !(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,26/)
!!$
!!$      !index_solar=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,26/)
!!$      index_mixed(1)=20
!!$      index_mixed(2)=21
!!$      index_mixed(3)=22
!!$      index_mixed(4)=23
!!$      index_mixed(5)=24
!!$      !index_mixed=(/20,21,22,23,24/)
!!$      index_ir(1)=24
!!$      index_ir(2)=25
!!$      index_ir(3)=26
!!$      index_ir(4)=27
!!$      index_ir(5)=28
!!$      index_ir(6)=29
!!$      index_ir(7)=30
!!$      index_ir(8)=31
!!$      index_ir(9)=32
!!$      index_ir(10)=33
!!$      index_ir(11)=34
!!$      index_ir(12)=35
!!$      index_ir(13)=36
      !index_ir=(/24,25,26,27,28,29,30,31,32,33,34,35,36/)
      !rangev=(/1354,2030/)
      !???rangev=(/1354,1/)???
   end if

   !
!!$   !avhrr solar channels solar 1,2 3a mixed 3b ir 4,5
!!$   !
!!$   if (Ctrl%Inst%Name .eq. 'AVHRR-NOAA15') then
!!$      
!!$      allocate(index_solar(2))
!!$      allocate(solar_store_sea(2))
!!$      allocate(solar_store_land(2))
!!$      allocate(index_mixed(1))
!!$      allocate(index_ir(2))
!!$      
!!$      index_solar=0
!!$      solar_store_sea=0.
!!$      
!!$      solar_store_land=0.
!!$      print*,'shape',shape(solar_store_sea)
!!$      print*,'shape',shape(solar_store_land)
!!$      index_mixed=0
!!$      index_ir=0
!!$
!!$
!!$!
!!$!designate which avhrr channels are vis/mixed or ir note this may chane depending on if 1.6 of 3.7 retrived.
!!$!      
!!$      index_solar(1)=1
!!$      index_solar(2)=2
!!$      !index_solar(3)=3
!!$      !index_solar=(/1,2,3/) !3a
!!$      
!!$      index_mixed(1)=3
!!$      !index_mixed=(/3/) !3b
!!$      
!!$      index_ir(1)=4
!!$      index_ir(2)=5
!!$      !index_ir=(/4,5/)
!!$   end if
!!$   
!!$   
!!$   !
!!$   !avhrr solar channels solar 1,2 3a mixed 3b ir 4,5
!!$   !watch oout for time later as other channerls were tuirned on
!!$   if (Ctrl%Inst%Name .eq. 'AVHRR-NOAA16') then
!!$
!!$      write(*,*) 'in here for n16',Ctrl%Inst%Name
!!$      
!!$      allocate(index_solar(2))
!!$      allocate(solar_store_sea(2))
!!$      allocate(solar_store_land(2))
!!$      allocate(index_mixed(1))
!!$      allocate(index_ir(2))
!!$      
!!$      index_solar=0
!!$      solar_store_sea=0.
!!$      
!!$      solar_store_land=0.
!!$      print*,'shape',shape(solar_store_sea)
!!$      print*,'shape',shape(solar_store_land)
!!$      index_mixed=0
!!$      index_ir=0
!!$      
!!$      index_solar(1)=1
!!$      index_solar(2)=2
!!$      !index_solar(3)=3
!!$      !index_solar=(/1,2,3/) !3a
!!$      
!!$      index_mixed(1)=3
!!$      !index_mixed(1)=4
!!$      !index_mixed=(/3/) !3b
!!$      
!!$      index_ir(1)=4
!!$      index_ir(2)=5
!!$      !index_ir(1)=5
!!$      !index_ir(2)=6
!!$
!!$      !index_ir=(/4,5/)
!!$   end if
!!$   
!!$   !
!!$   !avhrr solar channels solar 1,2 3a mixed 3b ir 4,5
!!$   !this one has channel 3a turned on which is a purely solar channel
!!$   if (Ctrl%Inst%Name .eq. 'AVHRR-NOAA17') then
!!$      
!!$      allocate(index_solar(3))
!!$      allocate(solar_store_sea(3))
!!$      allocate(solar_store_land(3))
!!$      allocate(index_mixed(1))
!!$      allocate(index_ir(2))
!!$      
!!$      index_solar=0
!!$      solar_store_sea=0.
!!$      solar_store_land=0.
!!$      index_mixed=0
!!$      index_ir=0
!!$      
!!$      index_solar(1)=1
!!$      index_solar(2)=2
!!$      index_solar(3)=3
!!$      !index_solar=(/1,2,3/) !3a
!!$      
!!$      !this one doe not exist on NOAA17
!!$      index_mixed(1)=-1
!!$      !index_mixed=(/3/) !3b
!!$      
!!$      index_ir(1)=4
!!$      index_ir(2)=5
!!$      !index_ir=(/4,5/)
!!$   end if
!!$!
!!$   !avhrr solar channels solar 1,2 3a mixed 3b ir 4,5
!!$   !
!!$   if (Ctrl%Inst%Name .eq. 'AVHRR-NOAA18') then
!!$      
!!$      allocate(index_solar(2))
!!$      allocate(solar_store_sea(2))
!!$      allocate(solar_store_land(2))
!!$      allocate(index_mixed(1))
!!$      allocate(index_ir(2))
!!$      
!!$      index_solar=0
!!$      solar_store_sea=0.
!!$      
!!$      solar_store_land=0.
!!$      print*,'shape',shape(solar_store_sea)
!!$      print*,'shape',shape(solar_store_land)
!!$      index_mixed=0
!!$      index_ir=0
!!$      
!!$      index_solar(1)=1
!!$      index_solar(2)=2
!!$      !index_solar(3)=3
!!$      !index_solar=(/1,2,3/) !3a
!!$      
!!$      index_mixed(1)=3
!!$      !index_mixed=(/3/) !3b
!!$      
!!$      index_ir(1)=4
!!$      index_ir(2)=5
!!$      !index_ir=(/4,5/)
!!$   end if
!!$
!!$
!!$   !
!!$   !aatsr solar channels 1-4 nir 5 ir 6,7 (nadir view)
!!$   !
   if (Ctrl%Inst%Name .eq. 'AATSR') then
!!$      
!!$      allocate(index_solar(4))
!!$      allocate(solar_store_sea(4))
!!$      allocate(solar_store_land(4))
!!$      allocate(index_mixed(1))
!!$      allocate(index_ir(2))
!!$      
!!$      index_solar=0
!!$      solar_store_sea=0.0
!!$      
!!$      solar_store_land=0.0
!!$      index_mixed=0
!!$      index_ir=0
!!$
!!$      index_solar(1)=1 !55
!!$      index_solar(2)=2 !67
!!$      index_solar(3)=3 !87
!!$      index_solar(4)=4 !1.6
!!$
!!$      index_mixed(1)=5 !3.7
!!$      index_ir(1)=6 ! 11
!!$      index_ir(2)=7 ! 12
!!$      
!!$
!!$      !index_solar=(/1,2,3,4/)
!!$      !index_mixed=(/5/)
!!$      !index_ir=(/6,7/)
!!$      
!!$! store default value of surface reflecatnce thses can be used whn the value is missing from auxillary file
!!$
      solar_store_sea(1)=5.
      solar_store_sea(2)=2.0
      solar_store_sea(3)=1.0
      solar_store_sea(4)= 1.0
      
      solar_store_land(1)=15.0
      solar_store_land(2)=10.0
      solar_store_land(3)=1.0
      solar_store_land(4)=1.0
      
   end if

!!$   write(*,*) solar_store_land
!!$   write(*,*)'index_solar',index_solar
!!$   write(*,*)'size(index_solar)',size(index_solar)

   !
   !count the number of POTENTIAL channels that could be used.
   !
   
!!$   nindex_solar=size(index_solar)
!!$   nindex_mixed=size(index_mixed)
!!$   nindex_ir=size(index_ir)



   !
   !this value should be how many luts etc to read in
   !
   !Ny= number of measurements which will be used 
   Ctrl%Ind%Ny=Ctrl%Ind%NChans
   allocate(Ctrl%Sy(Ctrl%Ind%Ny,Ctrl%Ind%Ny))

!!$  allocate(pos_solar(nindex_solar+1))
!!$  allocate(ref_solar_sea(nindex_solar+1))
!!$  ref_solar_sea=0.0
!!$  allocate(ref_solar_land(nindex_solar+1))                
!!$  ref_solar_land=0.0
!!$  allocate(pos_thermal(nindex_ir+1))
!!$  allocate(pos_mixed(nindex_mixed))
!!$  
!!$ 
!!$  pos_solar=0
!!$  pos_thermal=0
!!$  pos_mixed=0
!!$  Nsolar=0
!!$  Nthermal=0
!!$  write(*,*)' nindex_solar',nindex_solar
!!$  write(*,*)'Ctrl%Ind%nchans aa',Ctrl%Ind%nchans
  !
   !set default surface reflectance values for channels used
   !this just maps the values from all channels in the file to the ones which will be actually used
  !
   
  ii=1
  do i=1,conf%nc
     if(conf%channel_proc_flag(i) .eq. 1) then
        ref_solar_sea(ii)=solar_store_sea(i)
        ref_solar_land(ii)=solar_store_land(i) 
        ii=ii+1
     endif
  enddo
!!$
!!$  do i=1,nindex_solar
!!$     do j=1,Ctrl%Ind%nchans 
!!$        jin=Ctrl%Ind%Chi(j)
!!$
!!$        if  (Ctrl%Ind%Y_id(jin) .eq. index_solar(i)) then
!!$           nsolar=nsolar+1
!!$           pos_solar(i)=jin
!!$           ref_solar_sea(i)=solar_store_sea(j)
!!$           ref_solar_land(i)=solar_store_land(j)        
!!$        end if
!!$        
!!$     end do
!!$  end do
!!$  
  write(*,*)'here Ctrl%Ind%Y_ida',Ctrl%Ind%Y_id
!!$  !thermal channels count channels and assign index
!!$  do i=1,nindex_ir
!!$     do j=1,Ctrl%Ind%nchans 
!!$        jin=Ctrl%Ind%Chi(j)
!!$        if  (Ctrl%Ind%Y_id(jin) .eq. index_ir(i)) then
!!$           nthermal=nthermal+1
!!$           pos_thermal(i)=jin
!!$        end if
!!$     end do
!!$  end do
!!$
!!$  !mixed channels count channels and assign index
!!$  do i=1,nindex_mixed
!!$     do j=1,Ctrl%Ind%nchans 
!!$        
!!$        jin=Ctrl%Ind%Chi(j)
!!$        if  (Ctrl%Ind%Y_id(jin) .eq. index_mixed(i)) then
!!$           nsolar=nsolar+1
!!$           nthermal=nthermal+1
!!$           pos_solar(i)=j
!!$           pos_thermal(i)=jin
!!$        end if
!!$     end do
!!$  end do
  

  !this stores for the used channels the number of channels 
  !where mixed channels are included in both on the l.h.s.
  Ctrl%Ind%Nsolar=conf%nsolar_use!+conf%nmixed_use
  Ctrl%Ind%Nthermal=conf%nthermal_use!+conf%nmixed_use
  write(*,*) 'Nsolar, nthermal to be used',Ctrl%Ind%Nsolar,Ctrl%Ind%Nthermal

  !
  ! use above  indices to assign to measurement indices
  !
!!$  write(*,*) 'this test'
!!$  write(*,*) Ctrl%Ind%Nsolar
!!$  write(*,*) Ctrl%Ind%Nthermal
!!$  write(*,*) Ctrl%Ind%NChans 
!!$  write(*,*) conf%channel_sw_flag_use
!!$  write(*,*) conf%channel_lw_flag_use
!!$  write(*,*)  Ctrl%Ind%Chi


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
     if(conf%channel_sw_flag_use(i) .eq. 1 ) then
        ii=ii+1
        Ctrl%Ind%Ysolar(ii)=Ctrl%Ind%Chi(i) !these are the indices wrt the preproc file
        Ctrl%Ind%Ysolar_msi(ii)=i !these are the indices wrt the order in the MSI array
     endif
     if(conf%channel_lw_flag_use(i) .eq. 1) then
        jj=jj+1
        Ctrl%Ind%Ythermal(jj)=Ctrl%Ind%Chi(i)!these are the indices wrt the preproc file
        Ctrl%Ind%Ythermal_msi(jj)=i !these are the indices wrt the order in the MSI array
     endif
  enddo

  write(*,*)  'Ctrl%Ind%Ysolar/msi',Ctrl%Ind%Ysolar,Ctrl%Ind%Ysolar_msi
  write(*,*) 'Ctrl%Ind%Ythermal/msi',Ctrl%Ind%Ythermal,Ctrl%Ind%Ythermal_msi
!!$  stop
!!$  jcount=1
!!$  do i=1,nindex_solar
!!$     if (pos_solar(i) .gt. 0) then
!!$        Ctrl%Ind%Ysolar(jcount)=pos_solar(i)
!!$        jcount=jcount+1
!!$     end if
!!$  end do
  
  

  !Ctrl%Ind%Ythermal=conf%channel_sw_flag_use
!!$  jcount=1
!!$  do i=1,nindex_ir
!!$     if (pos_thermal(i) .gt. 0) then
!!$        Ctrl%Ind%Ythermal(jcount)=pos_thermal(i)
!!$        jcount=jcount+1
!!$     end if
!!$  end do
!!$   



   !Setup some directories:
   !
   !strip off root filename
   !
!!$   if ((Ctrl%Inst%Name .eq. 'MODIS-MYD') .or. (Ctrl%Inst%Name .eq. 'MODIS-MOD'))  then
!!$      
!!$      root=Ctrl%fid%input_filename(:)
!!$      write(*,*)'rootCtrl%fid%input_filename(', Ctrl%fid%input_filename(:)
!!$      root=trim(root)
!!$   end if
!!$   
!!$   root=trim(adjustl(input_path))
!!$
!!$   write(*,*)'root',root
   !
   !assign directories
   !
  
  Ctrl%Run_ID=trim(adjustl(scratch_dir))
   Ctrl%Data_Dir=trim(adjustl(scratch_dir))//'/'
   Ctrl%out_Dir=trim(adjustl(scratch_dir))//'/'
   
   Ctrl%SAD_Dir=trim(adjustl(lut_dir))//'/'
   write(*,*)'Ctrl%SAD_Dir',trim(adjustl(Ctrl%SAD_Dir))
   
   suffix='.msi.nc'
   Ctrl%FID%MSI=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   Ctrl%FID%MSI=trim(adjustl(Ctrl%FID%MSI))

   write(*,*)'Ctrl%FID%MSI',trim(adjustl(Ctrl%FID%MSI))
  
 
   suffix='.lwrtm.nc'
   Ctrl%FID%LWRTM=trim(adjustl(Ctrl%fid%input_filename))//trim(adjustl(suffix))
   write(*,*) 'Ctrl%FID%LWRTM',trim(adjustl(Ctrl%FID%LWRTM))
      

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
        & trim(adjustl(Ctrl%CloudClass%Name))
   write(*,*) trim(adjustl(outname))
   
   Ctrl%FID%L2_primary_outputpath_and_file=trim(adjustl(outname))//'.primary.nc'
   Ctrl%FID%L2_secondary_outputpath_and_file=trim(adjustl(outname))//'.secondary.nc'
  
   write(*,*) Ctrl%FID%L2_primary_outputpath_and_file
   
   Ctrl%FID%Log=trim(adjustl(outname))//'.log'
   Ctrl%FID%Diag=trim(adjustl(outname))//'.diag'
   Ctrl%FID%BkP=trim(adjustl(outname))//'bkp'
   
   write(*,*)'log file',trim(adjustl(Ctrl%FID%Log))
   

   !
   !Ctrl%Diag  values set the diagnostic output level Theses values can be changed but they are mostly static
   !
   Ctrl%Diagl(1)=1
   Ctrl%Diagl(2)=1
   Ctrl%Diagl(3)=1
   Ctrl%Diagl(4)=1
   Ctrl%Diagl(5)=1
   Ctrl%Diagl(6)=1
   Ctrl%Diagl(7)=0
   Ctrl%Diagl(8)=0
   Ctrl%Diagl(9)=1
   Ctrl%Diagl(10)=1
   Ctrl%Diagl(11)=1
   Ctrl%Diagl(12)=1
   Ctrl%Diagl(13)=0
   Ctrl%Diagl(14)=0


   ! Another diagnostic level sets the level of breakpoint out put
   ! when the code is comiled with breakpoint option Ctrl%Bkpl
   Ctrl%Bkpl=2
   
   
!   Ctrl%MaxSatZen=80 !this was used for the first CCI processing
   Ctrl%MaxSatZen=90 ! max satellite zenith angle
   Ctrl%MaxSolZen=80 ! max solar zenith angle > 90 = night image
   Ctrl%Sunset=90 ! used to set twilight option
   Ctrl%Ind%Ws=0 ! warm start option i.e enables user to start partway through a scene
   Ctrl%LUTIntflag=0 ! 0 =linear 1 = bicubic interpolation

   Ctrl%RTMIntflag=0 ! 0 =linear 1 = bicubic interpolation

   !        For each of the day, twilight, night active state variable arrays,
   !        read the array and set up the corresponding inactive array.


   !
   !Day options
   !
   Ctrl%Ind%Nx_Dy=5  ! number of active state variables
   Ctrl%Ind%X_Dy(1)=1
   Ctrl%Ind%X_Dy(2)=2
   Ctrl%Ind%X_Dy(3)=3
   Ctrl%Ind%X_Dy(4)=4
   Ctrl%Ind%X_Dy(5)=5
   

   Ctrl%Ind%NxI_Dy = MaxStateVar - Ctrl%Ind%Nx_Dy

   !
   !set active and inactive state variables
   !
   !Day:
   k = 0
   do i=1, MaxStateVar
      found = .false.
      do j=1, Ctrl%Ind%Nx_Dy ! Look for the variable index in the active array
         if (Ctrl%Ind%X_Dy(j) == i) then
            found = .true.
            exit
         end if
      end do
      if (.not. found) then     ! Not found in active set
         k = k + 1                   ! Add to inactive array
         Ctrl%Ind%XI_Dy(k) = i
      end if
      if (k == Ctrl%Ind%NxI_Dy) exit  ! Correct number of values found 
   end do

   !
   !twilight options
   !   
   Ctrl%Ind%Nx_Tw=3
   
   Ctrl%Ind%X_Tw(1)=3
   Ctrl%Ind%X_Tw(2)=4
   Ctrl%Ind%X_Tw(3)=5
   !  Ctrl%Ind%X_Tw(4)=4
   !  Ctrl%Ind%X_Tw(5)=5
   !  Ctrl%Ind%X_Tw=(/1, 0, 3, 4, 5/)
   Ctrl%Ind%NxI_Tw = MaxStateVar - Ctrl%Ind%Nx_Tw
   
   k = 0
   do i=1, MaxStateVar
      found = .false.
      do j=1, Ctrl%Ind%Nx_Tw ! Look for the variable index in the active array
         if (Ctrl%Ind%X_Tw(j) == i) then
            found = .true.
            exit
               end if
     end do
            if (.not. found) then     ! Not found in active set
        k = k + 1
        Ctrl%Ind%XI_Tw(k) = i
     end if
     if (k == Ctrl%Ind%NxI_Tw) exit  ! Correct number of values found 
  end do

  !
  !night options
  ! 
  Ctrl%Ind%Nx_Ni=3
  !indices of state parameters
  Ctrl%Ind%X_Ni(1)=3
  Ctrl%Ind%X_Ni(2)=4
  Ctrl%Ind%X_Ni(3)=5
  !  Ctrl%Ind%X_Ni(4)=4
  !  Ctrl%Ind%X_Ni(5)=5
  !Ctrl%Ind%X_Ni=(/1,0, 3, 4, 5/)
  Ctrl%Ind%NxI_Ni = MaxStateVar - Ctrl%Ind%Nx_Ni
  
  k = 0
  do i=1, MaxStateVar
     found = .false.
     do j=1, Ctrl%Ind%Nx_Ni ! Look for the variable index in the active array
        if (Ctrl%Ind%X_Ni(j) == i) then
           found = .true.
           exit
        end if
     end do
     if (.not. found) then     ! Not found in active set
        k = k + 1
        Ctrl%Ind%XI_Ni(k) = i
     end if
     if (k == Ctrl%Ind%NxI_Ni) exit  ! Correct number of values found 
  end do
  

  Ctrl%Ind%NViews=1
  allocate(Ctrl%Ind%viewidx(Ctrl%Ind%nchans))
  
  do i=1,Ctrl%Ind%nchans 
     Ctrl%Ind%Viewidx(i)=1
  end do

!!$  write(*,*) Ctrl%Ind%Chi
!!$  write(*,*) Ctrl%Ind%Nsolar
!!$  write(*,*) Ctrl%Ind%Nthermal
!!$  write(*,*) Ctrl%Ind%Ythermal
!!$  write(*,*) Ctrl%Ind%Ysolar
!!$  write(*,*) 'STOPSTOPSTOP'
!!$  stop

  Ctrl%CloudClass%Id=1
  
  ! set first guess options
  !(Tau,Re,Pc,F,Ts)
  ! what do the numbers mean
  ! 1: static apriori variable i.e does not change
  ! 2: use a dynamically chosen first guess dependant on measurements
  ! 3: use a value from an external auxillary file
  ! Ctrl%FG(statevariable,time of day)
  !
  !day
  Ctrl%FG(1,1)=1
  Ctrl%FG(2,1)=1
  Ctrl%FG(3,1)=2 !from ir profile=2
  Ctrl%FG(4,1)=1
  Ctrl%FG(5,1)=3 ! from auxillary file
  !twilight
  Ctrl%FG(1,2)=1
  Ctrl%FG(2,2)=1
  Ctrl%FG(3,2)=2
  Ctrl%FG(4,2)=1
  Ctrl%FG(5,2)=3
  !night
  Ctrl%FG(1,3)=1
  Ctrl%FG(2,3)=1
  Ctrl%FG(3,3)=2
  Ctrl%FG(4,3)=1
  Ctrl%FG(5,3)=3

  ! first guess default values for water i,e if option 1 is selected above
  if  (trim(Ctrl%CloudClass%Name) .eq. 'WAT') then
     Ctrl%X0(1)=0.8
     Ctrl%X0(2)=12.
     Ctrl%X0(3)=700.
     Ctrl%X0(4)=1.
     Ctrl%X0(5)=300.
  end if
  
  ! first guess default values for icei,e if option 1 is selected above
  if  (trim(Ctrl%CloudClass%Name) .eq. 'ICE') then
     Ctrl%X0(1)=0.8
     Ctrl%X0(2)=30.
     Ctrl%X0(3)=400.
     Ctrl%X0(4)=1.
     Ctrl%X0(5)=300.
  end if

  ! first guess default values for a small selection of aerosol types
  if (trim(Ctrl%CloudClass%Name) .eq. 'EYJ') then
     Ctrl%X0(1)=0.8
     Ctrl%X0(2)=0.5
     Ctrl%X0(3)=1000.
     Ctrl%X0(4)=1.
     Ctrl%X0(5)=300.
  end if
  
  if (trim(Ctrl%CloudClass%Name) .eq. 'MAR') then
     Ctrl%X0(1)=0.1
     Ctrl%X0(2)=1.8
     Ctrl%X0(3)=1000.
     Ctrl%X0(4)=1.
     Ctrl%X0(5)=300.
  end if

  if (trim(Ctrl%CloudClass%Name) .eq. 'DES') then
     Ctrl%X0(1)=0.0
     Ctrl%X0(2)=1.4
     Ctrl%X0(3)=1000.
     Ctrl%X0(4)=1.
     Ctrl%X0(5)=300.
  end if

!  write(*,*)'here g'
  !A priori options (Tau,Re,Pc,F,Ts)
  Ctrl%AP(1,1)=1
  Ctrl%AP(2,1)=1
  Ctrl%AP(3,1)=1
  Ctrl%AP(4,1)=2
  Ctrl%AP(5,1)=3
  !twilight
  Ctrl%AP(1,2)=1
  Ctrl%AP(2,2)=1
  Ctrl%AP(3,2)=1
  Ctrl%AP(4,2)=2
  Ctrl%AP(5,2)=3
  !night
  Ctrl%AP(1,3)=1
  Ctrl%AP(2,3)=1
  Ctrl%AP(3,3)=1
  Ctrl%AP(4,3)=2
  Ctrl%AP(5,3)=3

  ! set default apriori values (i.e if option 1 selected above) quite often these values with a very high uncertainty

  if  (trim(Ctrl%CloudClass%Name) .eq. 'WAT') then
     Ctrl%XB(1)=0.8
     Ctrl%XB(2)=12. 
     Ctrl%XB(3)=700.
     Ctrl%XB(4)=1.
     Ctrl%XB(5)=300.0

  end if
  
  if  (trim(Ctrl%CloudClass%Name) .eq. 'ICE') then
     Ctrl%XB(1)=0.8
     Ctrl%XB(2)=30. 
     Ctrl%XB(3)=400.
     Ctrl%XB(4)=1.
     Ctrl%XB(5)=300.0
  endif

  if  (trim(Ctrl%CloudClass%Name) .eq. 'EYJ') then
     Ctrl%XB(1)=0.8
     Ctrl%XB(2)=0.5
     Ctrl%XB(3)=1000.
     Ctrl%XB(4)=1.
     Ctrl%XB(5)=300.
  end if
  
  if  (trim(Ctrl%CloudClass%Name) .eq. 'MAR') then
     Ctrl%XB(1)=0.1
     Ctrl%XB(2)=1.8
     Ctrl%XB(3)=1000.
     Ctrl%XB(4)=1.
     Ctrl%XB(5)=300.
  end if

  if (trim(Ctrl%CloudClass%Name) .eq. 'DES') then
     Ctrl%XB(1)=0.0
     Ctrl%XB(2)=1.4
     Ctrl%XB(3)=1000.
     Ctrl%XB(4)=1.
     Ctrl%XB(5)=300.
  end if


  Ctrl%Max_SDAD=10 !No. of pixels where state is valid for SDAD setting
  Ctrl%RS%Flag=3 ! Surface Ref: Flag (1-Ctrl 3-Aux) (1-Ctrl 3-Aux) 
  !Ctrl%RS%Flag=1 ! Surface Ref: Flag (1-Ctrl 3-Aux) (1-Ctrl 3-Aux) 
  Ctrl%defaultSX(1)=1.0e+08 ! optical depth
  Ctrl%defaultSX(2)=1.0e+08 ! effective radii
  Ctrl%defaultSX(3)=1.0e+06 ! ctp
  Ctrl%defaultSX(4)=1.0e-10 ! fraction
  Ctrl%defaultSX(5)=1.0e+00 ! surface temperature
  Ctrl%SX= Ctrl%defaultSX ! optical depth
  

  if ( (trim(Ctrl%CloudClass%Name) .eq. 'EYJ' ) .or. &
     & (trim(Ctrl%CloudClass%Name) .eq. 'MAR' ) .or. & 
     & (trim(Ctrl%CloudClass%Name) .eq. 'DES' ) ) then
     Ctrl%defaultSX(1)=1.0e+01 ! optical depth
     Ctrl%defaultSX(2)=1.0e-01 ! effective radii
     Ctrl%defaultSX(3)=1.0e+06 ! ctp
     Ctrl%defaultSX(4)=1.0e-10 ! fraction
     Ctrl%defaultSX(5)=1.0e+00 ! surface temperature
     Ctrl%SX= Ctrl%defaultSX
  end if

  
  allocate(Ctrl%RS%B(Ctrl%Ind%Nsolar,2))
  Ctrl%RS%B=0.00  
  Ctrl%RS%B(1:Ctrl%Ind%Nsolar,1)=ref_solar_sea(1:Ctrl%Ind%Nsolar)/100.0 !valid for 67/87/1.6 channels  
  Ctrl%RS%B(1:Ctrl%Ind%Nsolar,2) = ref_solar_land(1:Ctrl%Ind%Nsolar)/100.0   
  deallocate(ref_solar_sea)
  deallocate(ref_solar_land)
  deallocate(solar_store_sea)
  deallocate(solar_store_land)
  
  Ctrl%RS%Sb  = 20.0/100.0   !Percentage error in surface reflectance  
  Ctrl%RS%Cb =  0.2    !Correlation between    surface reflectance
  
  Ctrl%Eqmpn%Rs = 1 ! Flag to use Eqmpn from Rs errors 
  Ctrl%Eqmpn%TH = 0 ! Flag to use Eqmpn from T/H(z) errors   
  Ctrl%Eqmpn%Homog = 1 ! Flag to use Eqmpn from homog errors
  Ctrl%Eqmpn%Coreg = 1 ! Flag to use Eqmpn from coReg errors 

  Ctrl%CloudType = 1 ! use this to select which coreg/homog errors to use
  Ctrl%Invpar%Mqstart =  0.001 ! Marquardt: starting parameter     
  Ctrl%Invpar%Mqstep =  10.0  ! step parameter       
  Ctrl%Invpar%Maxiter =  40 ! Maximum # of iterations   
  Ctrl%Invpar%Maxphase = 3   ! Maximum # of phase changes  
  Ctrl%Invpar%Ccj =   0.05  ! Cost convergence criteria    
  
  Ctrl%Invpar%Xscale(1)= 10.0 ! Scaling parameters (Tau,Re,Pc,F,Ts) 
  Ctrl%Invpar%Xscale(2)=1.0
  Ctrl%Invpar%Xscale(3)=1.0
  Ctrl%Invpar%Xscale(4)=1000.0
  Ctrl%Invpar%Xscale(5)=1.0

  Ctrl%Invpar%Xllim(1) =-3.0
  if ( (trim(Ctrl%CloudClass%Name) .eq. 'EYJ' ) .or. &
     & (trim(Ctrl%CloudClass%Name) .eq. 'MAR' ) .or. & 
     & (trim(Ctrl%CloudClass%Name) .eq. 'DES' ) ) then
     Ctrl%Invpar%Xllim(2) = 0.01
  else
     Ctrl%Invpar%Xllim(2) = 0.1
  endif
  !MJ ORG Ctrl%Invpar%Xllim(3)= 100.0
  Ctrl%Invpar%Xllim(3)= 10.0
  Ctrl%Invpar%Xllim(4)= 1.0
  Ctrl%Invpar%Xllim(5)= 250.0

! Upper limit on state Ctrl.Invpar.Xulim
  Ctrl%Invpar%Xulim(1)=2.408
  if  (trim(Ctrl%CloudClass%Name) .eq. 'ICE') then
     Ctrl%Invpar%Xulim(2)=100.0
  endif

  if  (trim(Ctrl%CloudClass%Name) .eq. 'WAT') then
     Ctrl%Invpar%Xulim(2)=35.0
  endif

  if ( (trim(Ctrl%CloudClass%Name) .eq. 'EYJ' ) .or. &
     & (trim(Ctrl%CloudClass%Name) .eq. 'MAR' ) .or. & 
     & (trim(Ctrl%CloudClass%Name) .eq. 'DES' ) ) then
     Ctrl%Invpar%Xulim(2)=20.0
  endif

  Ctrl%Invpar%Xulim(3)=1200.0
  Ctrl%Invpar%Xulim(4)=1.0
  Ctrl%Invpar%Xulim(5)=320.0
! Maximum acceptable retrieval cost   
  Ctrl%QC%MaxJ = 10.0

!  Maximum acceptable retrieval errors    
  Ctrl%QC%MaxS(1)=0.08
  Ctrl%QC%MaxS(2)=3.0
  Ctrl%QC%MaxS(3)=200.
  Ctrl%QC%MaxS(4)=0.2
  Ctrl%QC%MaxS(5)=2.0

  
  if (status == 0) then
     !Now do some checks
     !        Check that the first-guess methods for all variables are legal in ORAC
     !        and supported. Not all legal values are supported for all variables.
     !        N.B. not all supported methods can be used in all conditions and this
     !        is NOT CHECKED here.
     
!     write(*,*)'here n',Ctrl%Fg(:,1)
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
              end if

           case (SelmAux)
              if (i /= ITs) then
                 status = FGMethErr
                 message = 'Read_Driver: AUX method ONLY supported ' // &
                      'for setting first guess Ts'
              end if
              
           case default
              status = FGMethErr
              write(unit=message, fmt=*) 'Read_Driver: Invalid method ', &
                   'for first-guess state variable ',i
           end select
        end do
     end do
  end if ! end of status check
!  write(*,*)'here o'
  if (status == 0) then
     !        Check validity of a priori selection options. Not all legal values 
     !        are supported for all variables.
     
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
              end if
              
           case (SelmAux)
              if (i /= ITs) then
                 status = APMethErr
                 message = 'Read_Driver: AUX method ONLY supported ' // &
                      'for setting a priori Ts'
              end if
              
           case default
              status = APMethErr
              write(unit=message, fmt=*) &
                   'Read_Driver: Invalid method for a priori state variable ',i
           end select
        end do
     end do
  end if ! end of status check
  ! write(*,*)'here p Ctrl%RS%Flag',Ctrl%RS%Flag	 
  if (status == 0) then
     
     !        Check validity of surface reflectance flag

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
     !        Warning - status value may be lost if a read error occurs
     !        later on in this routine.
  end if ! end of status==0 check          
  


!  write(*,*)'here q'
  
999 if (ios /= 0) then             ! Don't write error messages out here.
     status = DriverFileReadErr  ! Log file not open yet.
     message = 'Read_Driver: error reading driver file'
  end if
  
  close(unit=dri_lun)
  


end subroutine Read_Driver
