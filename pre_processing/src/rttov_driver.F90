! Name: rttov_driver.f90
!
!
! Purpose:
! Create environment within all RTTOV related code is contained.
! 
!
! Description and Algorithm details:
! 1) set up what hardware is required i.e which instrument
! 2) setup flags required
! 3) initialise options structure
! 4) begin loop over sw and lw
! 5) set up coefficients
! 6) Allocate arrays
! 7) read coefficeints
! 8) setup rttov included emissivity atlas-need month input here
! 9) allocate profile
! 10) allocate radiance structure
! 11) allocate transmission structure
! 12) retrieve emissivity values from atlas
! 13) read angular information
! 14) perform rttov direct calculation
! 15) modify output for sw and lw so correct inputs for ORAC solar 
!     channels need to be modified  to incorporate an airmass correction
! 16) output the data
! 17) end loop over coefficients i.e lw and sw!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/03/27: Matthias Jerg provides initial implementation based on the example
!               program example_fw of  Annex X of the RTTOV V10.2 user guide V1.3.
!2012/05/23: Matthias Jerg fixes bug with AVHRR setup.
!2012/06/20: Caroline Poulsen removed emissivity
!               implemented new calls to call_rtm_ir and call_rtm_solar
!2012/07/29: Caroline Poulsen improved readability added in algorithm description,
!               added in month variable required for emissivity
!2012/08/02: MJ implements writing of RTTOV output to netcdf file.
!2012/08/06 CP added in solazi angle
!2012/08/10 CP modified how rttov_direct is called
!2012/08/24 MJ rearranged filtering and calling of rtm
!2012/08/24 MJ implemented writing of fill values in RTTOV output if not a good
!               preproc pixel
!2012/08/28 MJ initialized emissivity_out properly
!2012/08/28 CP general tidy up of code formatting.
!              fixed bug where prtm lat and longitude were switched around.
!2012/08/29 CP fix bug that gave unrealistic values of profile longitudes
!               readded _lw _sw to filter_array, commented out printouts
!               used profile error status to skip rttov calculations if appropri!ate
!2012/08/31 CP changed position of emissivity caluculation to speed up code
!2012/09/13 CP changed AATSR coefficient file naming
!2012/09/13 CP changed criterai for processing of coefficients and tidied up file
!2012/10/04 CP fixed bug and wrote skin temp and surface pressure to output prtm file 
!2012/10/24 CP added extra level to profile in define_preproc_grid so had to
!               change code to read surface info into profile
!2012/11/14 CP converted output to levels from layers
!2012/11/14 CP read in surface pressure from badc netcdf files for AATSR 
!               instrument cleaned up code
!2013/03/08 Gareth Thomas Changed the (dummy) values of surface/2m Q, O3, as well
!               as CTP to profiles(1)%nlayers profile value as profiles(1)%nlevels
!               is past the end of the arrays.
!              Added check on pressure profile against the RTTOV "pmax" parameter
!               to prevent RTTOV throwing an error. Pressures greater than pmax
!               are set to pmax in the RTTOV profile structure.
! 2013/03/14 CP added in land sea mask
! 2013/03/19 GT Tidied up some debugging write statements
! 2013/07/25 MJ tidies up some preprocessor statements
! 2013/10/29 CP removed redundant variables nlevs and nlayers
! 2013/10/31 MJ added another bugfix for levels/layers
! 2013/11/08 GM added missing call to rttov_alloc_auxrad() in deallocate mode
!
! $Id$
!
! Bugs:
!2012/11/14 CP atsr ir coefficients are in reverse order a temporary code fudge
!               has been introduced to read the coefficient files correctly
!2013.10.14 CP bug fix changed number of lay/level in rttov_alloc_rad and in the assignment of profiles and preproc_lwrtm%plevels to  preproc_lwrtm%players
!none known

subroutine rttov_driver(coef_path,emiss_path,sensor,platform,preproc_dims, & 
     &preproc_geoloc,preproc_geo,&
     & preproc_prtm,preproc_lwrtm,preproc_swrtm,imager_angles, &
     & netcdf_info,channel_info,month,ecmwf_dims)

  use netcdf
  use preproc_constants
  use preproc_structures
  use imager_structures
  use netcdf_structures
  use channel_structures
  use ecmwf_structures
  use ieee_arithmetic

  Use rttov_const, Only : &
       & sensor_id_mw, &
       & sensor_id_ir, &
       & sensor_id_hi, &
       & sensor_id_po, &  
       & errorstatus_success,&
       & errorstatus_warning,&
       & errorstatus_fatal ,&
       & q_mixratio_to_ppmv,&
       & o3_mixratio_to_ppmv, &
       & pmax, pmin
  
  
  Use rttov_types, Only :   &
       & rttov_options,     &
       & rttov_coefs,       &
       & profile_Type,      &
       & transmission_Type, &
       & radiance_Type,     &
       & radiance_aux,     &
       & rttov_chanprof

  use rttov_coef_io_mod

  Use parkind1, Only : jpim, jprb, jplm
  !
  Implicit None
  !

!#define DEBUG

#ifdef _OPENMP
!#define rttov_direct rttov_parallel_direct
#include "rttov_parallel_direct.interface"
#include "rttov_calcrad.interface"
#else
#include "rttov_direct.interface"
#include "rttov_calcrad.interface"
#endif

#include "rttov_setup.interface"
#include "rttov_dealloc_coefs.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_alloc_auxrad.interface"
#include "rttov_alloc_transmission.interface"
#include "rttov_alloc_prof.interface"

#include "rttov_read_coefs.interface"
#include "rttov_atlas_setup.interface"
#include "rttov_get_emis.interface"
#include "rttov_deallocate_atlas.interface"

#ifdef DEBUG
#include "rttov_print_profile.interface"
#include "rttov_user_profile_checkinput.interface"
#include "rttov_errorreport.interface"
#endif

  ! One profile per call and one sensor only for this simple example!!!
  Integer (Kind=jpim) :: nprof = 1         ! Number of profiles per call
  Integer(Kind=jpim) :: nrttovid = 1       ! Number of sensor coeff files to read
  ! RTTOV_errorhandling interface
  !====================
  Integer(Kind=jpim) :: Err_Unit     ! Logical error unit (<0 for default)
  Integer(Kind=jpim) :: verbosity_level ! (<0 for default)
  ! RTTOV_setup interface
  !====================
  Integer(Kind=jpim)               :: setup_errorstatus   ! setup return code
  Integer(Kind=jpim), Allocatable  :: instrument(:,:) ! platform id, sat id and sensor id
  Type(rttov_options)              :: opts            ! Options structure
  Type(rttov_coefs), Allocatable   :: coefs(:)        ! Coefficients structure


  ! RTTOV interface
  !====================
  Integer(Kind=jpim)    :: rttov_errorstatus ! rttov error return code
  Integer(Kind=jpim)    :: nchannels,ichan
  Type(rttov_chanprof), Allocatable :: chanprof(:)
  Type(profile_Type), Allocatable   :: profiles(:)
  Logical(Kind=jplm), Allocatable   :: calcemis(:)
  Real(Kind=jprb), Allocatable      :: emissivity_in (:)
  Real(Kind=jprb), Allocatable      :: emissivity_out (:)
  Type(transmission_Type)           :: transmission ! transmittances and layer optical depths
  Type(radiance_Type)               :: radiance
  Type(radiance_aux)                :: auxrad
  Integer(Kind=jpim) :: alloc_status(20)
  Integer(Kind=jpim) :: errorstatus,test_write
  Real(Kind=jprb), Allocatable :: emissivity(:)
  real(kind=jprb),    allocatable :: emis_std(:)
  integer(kind=jpim), allocatable :: emis_flag(:)

  Character (len=80) :: errMessage
  Character (len=11) :: NameOfRoutine = 'example_fwd'

  ! variables for input
  !====================
  Integer(Kind=jpim), Parameter :: mxchn = 9000 ! max number of channels
  Integer(Kind=jpim) :: input_chan(mxchn)
  Real(Kind=jprb)    :: input_ems(mxchn)
  Real(Kind=jprb)    :: zenith
  Real(Kind=jprb)    :: azimut
  Real(Kind=jprb)    :: lat
  Real(Kind=jprb)    :: zerht
  Real(Kind=jprb)    :: sunzang
  Real(Kind=jprb)    :: sunazang
  Integer(Kind=jpim) :: nlevels,ilevel
  Integer(Kind=jpim) :: ivch, ich
  Integer(Kind=jpim) :: asw      ! allocate or deallocate switch
  Real(Kind=jprb)    :: ems_val,diff
  Integer(Kind=jpim), Allocatable :: nchan(:) ! number of channels per profile
  Integer(Kind=jpim) :: isurf
  Integer(Kind=jpim) :: nwater
  logical(Kind=jplm) :: addrefrac
  logical(Kind=jplm) :: addsolar
  logical(Kind=jplm) :: addaerosl
  logical(Kind=jplm) :: addclouds
  logical(Kind=jplm) :: all_channels
  Logical(Kind=jplm) :: addinterp ! switch for the interpolator
  ! printing arrays
  Real(Kind=jprb), Allocatable :: pr_radcld(:)
  Real(Kind=jprb), Allocatable :: pr_trans(:)
  Real(Kind=jprb), Allocatable :: pr_emis(:)
  Real(Kind=jprb), Allocatable :: pr_trans_lev(:,:)
  Character (len=3) :: cref
  Character (len=3) :: caer
  Character (len=3) :: ccld
  Character (len=3) :: csun
  ! loop variables
  Integer(Kind=jpim) :: j, jch,ii
  Integer(Kind=jpim) :: np, nch
  Integer(Kind=jpim) :: ilev, nprint,test_rttov_flag
  Integer            :: ios


  integer(kind=lint) :: idim,jdim,kchar,kdim,icoeffs,imonth,iangle
  integer(kind=stint) :: iplatformnumber,month

  character(len=sensorlength) :: sensor
  character(len=platformlength) :: platform, platformnumber,dummy
  character(len=pathlength) :: coef_path,emiss_path,path
  character(len=filelength) :: file_coef,coeffile
  character(len=filelength) :: form_coef
  
  type(preproc_dims_s) :: preproc_dims
  type(preproc_geoloc_s) :: preproc_geoloc
  type(preproc_geo_s) :: preproc_geo
  type(preproc_prtm_s) :: preproc_prtm
  type(preproc_lwrtm_s) :: preproc_lwrtm
  type(preproc_swrtm_s) :: preproc_swrtm
  type(imager_angles_s) :: imager_angles
  type(netcdf_info_s) :: netcdf_info
  type(channel_info_s) :: channel_info
  ! ECMWF_setup interface
  !====================
  type(ecmwf_dims_s) :: ecmwf_dims

   LOGICAL(KIND=jplm) :: addcosmic! switch for adding temp of cosmic background

  real :: amf,za,sza,lza
  integer :: lu
  character*100 :: text
  logical :: apply_reg_limits

  integer(kind=lint) :: ierr

  integer(kind=lint) ::  pixel_counter_lw,pixel_counter_sw,pixel_counter_pw,dummy_lint_1d(1)

  real(kind=sreal) :: dummy_real_1d(1),dummy_real_2d(preproc_dims%kdim_pre,1)
  
  real(kind=sreal), allocatable, dimension(:,:,:) :: dummy_real_3d

  real(kind=sreal), allocatable, dimension(:,:,:,:) :: dummy_real_4d
  real(kind=sreal), allocatable, dimension(:,:) :: dummy_real_2dems

  integer(kind=lint), allocatable, dimension(:) :: dummy_lint_1dveca,dummy_lint_1dvecb
  real(kind=sreal), allocatable, dimension(:) :: dummy_real_1dveca

  real(kind=sreal), allocatable, dimension(:,:) ::  dummy_real_2dveca
  integer(kind=lint) :: maxlat,minlat,maxlon,minlon
#ifdef _OPENMP
  integer :: OMP_GET_max_THREADS, OMP_GET_NUM_THREADS
  integer(Kind=jpim) :: nthreads,nompthreads
#endif



  errorstatus     = 0_jpim
  alloc_status(:) = 0_jpim
  allocate (instrument(3,nrttovid),stat= alloc_status(1))

  !how many threads are available?
#ifdef _OPENMP
  nompthreads=OMP_get_max_threads()
  write(*,*) 'RTTOV running on: ', nompthreads, 'threads'
#endif
  !if( nompthreads .ge. 4)  nompthreads=4
  !nompthreads=int(max(1,nompthreads),kind=jpim)
  !call OMP_set_num_threads(nompthreads)
  !nompthreads = OMP_GET_NUM_THREADS()



!
! which hardware do we have?
! AVHRR
!
  if(trim(sensor) .eq. 'AVHRR') then
     !Get instrument type
     instrument(3,nrttovid)=5_jpim
     !Get satellite number
     !Look for numbers in name
     platformnumber='        '
     dummy='        '
     do kchar=1,len(platform)
        if(ichar(platform(kchar:kchar)) .ge. 48 .and. &
             & ichar(platform(kchar:kchar)) .le. 57) then
           platformnumber(kchar:kchar)=platform(kchar:kchar)
        endif
     enddo

     !assign this characters as integers to variable
     dummy=platformnumber
     write(platformnumber, '(i2)')  instrument(2,nrttovid)
     platformnumber=dummy
     write(*,*) platformnumber,platform,instrument
     
     !     read(trim(adjustl(platformnumber)), '(i2)')  instrument(2,nrttovid)

     !which satellite is it flying on
     if(index(trim(platform),'noaa') .ge. 1) then
        instrument(1,nrttovid)=1 !NOAA
        
     elseif(index(trim(platform),'metop') .ge. 1) then
        instrument(1,nrttovid)=10 !metop
     endif
!
! MODIS
!
  elseif(trim(sensor) .eq. 'MODIS') then
     instrument(3,nrttovid)=13
     instrument(1,nrttovid)=9 !EOS Aqua or Terra
     if(trim(platform) .eq. 'TERRA') instrument(2,nrttovid)=1
     if(trim(platform) .eq. 'AQUA')  instrument(2,nrttovid)=2
     !(A)ATSR
  elseif(trim(sensor) .eq. 'ATSR' .or. trim(sensor) .eq. 'AATSR'  ) then
     instrument(3,nrttovid)=14
     !instrument(1,nrttovid)=???
     !instrument(2,nrttovid)=???
  endif
  write(*,*) sensor,' ',platform,' ',instrument(:,nrttovid)
  
!
! Set flags 
!
  write(*,*) 'Set flags for options structure'
  addrefrac=.true. !false. !.True.! include refraction in path calc
  addsolar=.true. !.False.    ! Do not include reflected solar
  addaerosl=.False.   ! Don't include aerosol effects
  addclouds=.False.   ! Don;t include cloud effects
  all_channels=.True. ! Read all channels into memory from coef file
  addinterp = .True.  ! Allow interpolation of input profile
  cref = 'YES'
  caer = ' NO'
  ccld = ' NO'
  csun = ' NO'


!
! Initialise options structure
! (seems to be complete (what about IR scattering/solar?), 
! many flags sets implicitly as default
!
  opts % addrefrac = addrefrac
  opts % addinterp = addinterp
  opts % addsolar   = addsolar
  opts % addclouds = addclouds
  opts % addaerosl = addaerosl
  opts % ozone_data = .True.       ! we have an ozone profile
  opts % co2_data   = .False.      ! we do not have profiles
  opts % n2o_data   = .False.      ! for any other constituents
  opts % ch4_data   = .False.      !
  opts % co_data    = .False.      !
  opts % apply_reg_limits=.false. !we do  NOT move our profiles into the RTTOV range
!  
! Initialise error management with default value for
! the error unit number and all error message output
!
  Err_unit = -1
  !MJ ORG verbosity_level = 3_jpim
  verbosity_level = 1_jpim
  verbosity_level = 3_jpim

  Call rttov_errorhandling(Err_unit, verbosity_level)
!
! zero the counters
!
  pixel_counter_pw=0
  pixel_counter_sw=0
  pixel_counter_lw=0


!write some static information to the files:
!LW
!channel id abs and instr lw:               
  allocate(dummy_lint_1dveca(sum(channel_info%channel_lw_flag)))
  dummy_lint_1dveca=long_int_fill_value
  allocate(dummy_lint_1dvecb(sum(channel_info%channel_lw_flag)))
  dummy_lint_1dvecb=long_int_fill_value
  allocate(dummy_real_1dveca(sum(channel_info%channel_lw_flag)))
  dummy_real_1dveca=real_fill_value
  do icoeffs=1,channel_info%nchannels_total
     if(channel_info%channel_lw_flag(icoeffs) .eq. 1 ) then
        pixel_counter_lw=pixel_counter_lw+1
        dummy_lint_1dveca(pixel_counter_lw)=channel_info%channel_ids_abs(icoeffs)
        dummy_lint_1dvecb(pixel_counter_lw)=channel_info%channel_ids_instr(icoeffs)
        dummy_real_1dveca(pixel_counter_lw)=channel_info%channel_wl_abs(icoeffs)
     endif
  enddo
 
  netcdf_info%start_1d(1)=1
  netcdf_info%counter_1d(1)=sum(channel_info%channel_lw_flag)
  netcdf_info%stride_1d(1)=1
  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, netcdf_info%channels_id_lw, &
       & dummy_lint_1dveca,&
       & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
  if (ierr.NE.NF90_NOERR) stop 'err write lw index abs'                 

  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
       & netcdf_info%channels_id_instr_lw,dummy_lint_1dvecb,&
       & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
  if (ierr.NE.NF90_NOERR) stop 'err write lw index instr'                    
  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
       & netcdf_info%wvn_id_lw,dummy_real_1dveca,&
       & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
  if (ierr.NE.NF90_NOERR) stop 'err write lw wl'                    
  deallocate(dummy_lint_1dveca)
  deallocate(dummy_lint_1dvecb)
  deallocate(dummy_real_1dveca)

!SW
!channel id abs and instr sw:               
!
  allocate(dummy_lint_1dveca(sum(channel_info%channel_sw_flag)))
  dummy_lint_1dveca=long_int_fill_value
  allocate(dummy_lint_1dvecb(sum(channel_info%channel_sw_flag)))
  dummy_lint_1dvecb=long_int_fill_value
  allocate(dummy_real_1dveca(sum(channel_info%channel_sw_flag)))
  dummy_real_1dveca=real_fill_value
  do icoeffs=1,channel_info%nchannels_total
     if(channel_info%channel_sw_flag(icoeffs) .eq. 1 ) then
        pixel_counter_sw=pixel_counter_sw+1
        dummy_lint_1dveca(pixel_counter_sw)=channel_info%channel_ids_abs(icoeffs)
        dummy_lint_1dvecb(pixel_counter_sw)=channel_info%channel_ids_instr(icoeffs)
        dummy_real_1dveca(pixel_counter_sw)=channel_info%channel_wl_abs(icoeffs)
     endif
  enddo

  netcdf_info%start_1d(1)=1
  netcdf_info%counter_1d(1)=sum(channel_info%channel_sw_flag)
  netcdf_info%stride_1d(1)=1
  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
       & netcdf_info%channels_id_sw,dummy_lint_1dveca,&
       & netcdf_info%start_1d, netcdf_info%counter_1d, &
       & netcdf_info%stride_1d)
  if (ierr.NE.NF90_NOERR) stop 'err write sw index abs'                 
  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
       & netcdf_info%channels_id_instr_sw,dummy_lint_1dvecb,&
       & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
  if (ierr.NE.NF90_NOERR) stop 'err write sw index instr' 
  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
       &netcdf_info%wvn_id_sw,dummy_real_1dveca,&
       & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
  if (ierr.NE.NF90_NOERR) stop 'err write sw wl'                                       
  deallocate(dummy_lint_1dveca)
  deallocate(dummy_lint_1dvecb)
  deallocate(dummy_real_1dveca)

!
! zero the counters again
!
  pixel_counter_pw=0
  pixel_counter_sw=0
  pixel_counter_lw=0
  
!
!loop over lw and sw calculations
! 
  do icoeffs=1,2 !do first LW and then SW calculations
     
     allocate (coefs(nrttovid),stat= alloc_status(1))
     If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem allocation error for coefs")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
     End If


     write(*,*) 'set nchan'
     Allocate (nchan(nprof))
     nchan(:) = size(channel_info%channel_ids_rttov_coef_lw)
     write(*,*)'platformnumber', platformnumber
     write(*,*) 'nchan',nchan


!        
! set up coefficients
! pick instrument
! AVHRR
!
     if(instrument(3,nrttovid) .eq. 5) then
! NOAA:
        if(instrument(1,nrttovid) .eq. 1) then
       !pick satellite directly through instrument(2,nrttovid)
       !first LW
           if(icoeffs .eq. 1) then
              coeffile='rtcoef_noaa_'//trim(adjustl(platformnumber))//'_avhrr.v10.dat'
          !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_noaa_'//trim(adjustl(platformnumber))//'_avhrr_visnir_v1.v10.dat'
           endif
! METOP
        elseif(instrument(1,nrttovid) .eq. 10) then
           if(icoeffs .eq. 1) then
              coeffile='rtcoef_metop_'//trim(adjustl(platformnumber))//'_avhrr.v10.dat'
          !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_metop_'//trim(adjustl(platformnumber))//'_avhrr_visnir_v1.v10.dat'
           endif
        endif
          
! MODIS
     elseif(instrument(3,nrttovid) .eq. 13) then
! TERRA
        if(instrument(2,nrttovid) .eq. 1) then
       !first LW
           if(icoeffs .eq. 1) then           
              coeffile='rtcoef_eos_2_modis.v10.dat'
          !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_eos_2_modis_visnir_v1.v10.dat'
           endif
! AQUA
        elseif(instrument(2,nrttovid) .eq. 2) then
           if(icoeffs .eq. 1) then           
              coeffile='rtcoef_eos_2_modis.v10.dat'
          !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_eos_2_modis_visnir_v1.v10.dat'
           endif
        endif

! AATSR
     elseif(instrument(3,nrttovid) .eq. 14) then
         !first LW
           if(icoeffs .eq. 1) then           
              coeffile='rtcoef_envisat_1_atsr.dat'
          !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_envisat_1_atsr_visnir-spp_v10.dat'
           endif      
     endif
 
!
!set channel info for coeff files
!
    
     write(*,*)'finished setting coef files icoeffs',icoeffs
! LW
     if(icoeffs .eq. 1 ) then
        nchan(nprof)=size(channel_info%channel_ids_rttov_coef_lw)
        input_chan(1:nchan(nprof))= channel_info%channel_ids_rttov_coef_lw(:)

! SW
     elseif(icoeffs .eq. 2 ) then
        nchan(nprof)=size(channel_info%channel_ids_rttov_coef_sw)
        input_chan(1:nchan(nprof))= channel_info%channel_ids_rttov_coef_sw(:)
    endif

     nchannels = nchan(nprof)! Number of valid channels to compute radiances
!
! Allocate and Pack channels and emmissivity arrays
!
     Allocate(chanprof(nchan(nprof)))! Note these array sizes nchan can vary per  profile
     Allocate(emissivity(nchan(nprof))) ! but for this example assume 1  profile/call with same channels
     Allocate(emis_std(nchan(nprof)))
     Allocate(emis_flag(nchan(nprof)))
     Allocate(emissivity_out(nchan(nprof)))
     emissivity_out=real_fill_value
     Allocate(calcemis(nchannels))
     Allocate(emissivity_in(nchannels))

!
!loop over channels
!

     nch = 0_jpim
     Do j = 1 , nprof
        DO jch = 1,nchan(j)
           nch = nch + 1_jpim
           chanprof(nch)%prof = j
           ! set this up for RTTOV, as for the actual RTM the numbering is again
           ! different and has to start at "1" regardless of the instrument
           ! channel numbering
           chanprof(nch)%chan = jch
        End Do
     End Do
!
!Nb note that the ATSR coefficient files have the channels in reverse order no idea why!
!
     if(trim(sensor) .eq. 'ATSR' .or. trim(sensor) .eq. 'AATSR'  ) then
        chanprof(1)%chan = 3
        chanprof(2)%chan = 2
        chanprof(3)%chan = 1
     endif


! setup of coefficients
!
     write(*,*) 'Set up coefficients'
     write(*,*) 'coeffile', coeffile  
     write(*,*) trim(adjustl(coef_path))//'/'//trim(adjustl(coeffile))
     form_coef="formatted"
     write(*,*) 'Read coefficients'
     write(*,*) 'coef file',trim(adjustl(coef_path))//'/'//trim(adjustl(coeffile))
     write(*,*) 'read_coefs start',input_chan(1:nchannels)
     
!
!read coefficients
!
     call rttov_read_coefs(errorstatus,coefs(nrttovid),opts, &
          & channels=input_chan(1:nchannels),form_coef=form_coef,&
          & file_coef=trim(adjustl(coef_path))//'/'//trim(adjustl(coeffile)))
     write(*,*) 'errorstatus coeffs',errorstatus
     
     Call rttov_errorhandling(Err_unit, verbosity_level)
     write(*,*) 'read_coefs end'

!
!initialise coefficients
!          
     write(*,*) 'Init coefficients'
     write(*,*) 'Init coefs',errorstatus
     call rttov_init_coefs(errorstatus,opts,coefs(nrttovid))
     write(*,*) 'errorstatus init coeffs',errorstatus
     Call rttov_errorhandling(Err_unit, verbosity_level)

!
! set up emissivity atlas
!
     imonth=month
     write(*,*) 'Set up atlas'
     if(icoeffs .le. 2 ) then
        call rttov_atlas_setup(errorstatus,imonth,coefs(nrttovid)%coef, &
	           & path=emiss_path)
        write(*,*) 'errorstatus rttov atlas',errorstatus
        Call rttov_errorhandling(Err_unit, verbosity_level)
     endif
!
! allocate profile
! number of profile levels
! assign here the number of layers from grib 
! as we have there the variables defined.
!
     asw=1 !allocate it!
     write(*,*) 'Set up profile',preproc_dims%kdim_pre
     allocate(profiles(nprof),stat=alloc_status(1))
  
     test_rttov_flag=0
     if (test_rttov_flag .eq. 0) then

        
        call rttov_alloc_prof(errorstatus,nprof,profiles, &
             & preproc_dims%kdim_pre, &
             & opts,asw,coefs=coefs(nrttovid),init=.true._jplm)
      
        write(*,*) 'errorstatus rttov_prof ',errorstatus
        Call rttov_errorhandling(Err_unit, verbosity_level)
        !CP 2/10/200 add in surface layer
        profiles(1)%nlevels=preproc_dims%kdim_pre
        profiles(1)%nlayers=profiles(1)%nlevels-1
      
     else

        call rttov_alloc_prof(errorstatus,nprof,profiles, &
             & preproc_dims%kdim_pre,&
             & opts,asw,coefs=coefs(nrttovid),init=.true._jplm)

     	Call rttov_errorhandling(Err_unit, verbosity_level)
	preproc_dims%kdim_pre=43
 	profiles(1)%nlevels=43
     	profiles(1)%nlayers=profiles(1)%nlevels-1

!
!setup test code for rttov to compare with idl
!
!	call test_rttov(test_rttov_flag,profiles,preproc_dims)

     end if

!
! allocate radiance structure
!
     asw=1 !allocate it!
     write(*,*) 'Set up radiance',preproc_dims%kdim_pre,  profiles(1)%nlevels
     call rttov_alloc_rad(errorstatus,nchannels,radiance, &
                          & preproc_dims%kdim_pre-1,asw) 
     !MJ ORG & preproc_dims%kdim_pre,asw) 
     write(*,*) 'errorstatus alloc rad',errorstatus, preproc_dims%kdim_pre
     Call rttov_errorhandling(Err_unit, verbosity_level)
  
     call rttov_alloc_auxrad( errorstatus,auxrad,  profiles(1)%nlevels, &
                          &  nchannels,asw)

! 
! allocate transmission structure
!
     write(*,*) 'Set up transmission',preproc_dims%kdim_pre

     call rttov_alloc_transmission(errorstatus,transmission, &
     	  & preproc_dims%kdim_pre-1,nchannels,asw,init=.true._jplm)
     write(*,*) 'errorstatus alloc transmission',errorstatus
     Call rttov_errorhandling(Err_unit, verbosity_level)

!
! Loop over preprocessing (interpolated) pixels
     pixel_counter_pw=0
     pixel_counter_sw=0
     pixel_counter_lw=0!

!     write(*,*) 'Loop over preproc pixels lat jdim', &
!          & preproc_dims%preproc_min_lat,preproc_dims%preproc_max_lat 
!    write(*,*) 'Loop over preproc pixels lon idim', &
!         & preproc_dims%preproc_min_lon,preproc_dims%preproc_max_lon
     

     maxlat= preproc_dims%preproc_max_lat
     maxlon= preproc_dims%preproc_max_lon

     minlat= preproc_dims%preproc_min_lat
     minlon= preproc_dims%preproc_min_lon


     do jdim=minlat,maxlat
        do idim=minlon,maxlon
  
           ! now assign the preprocessing grid data to the RTTOV structures
              

           !profiles of p,t,o3,q and convert to units required by RTTOV
           !              write(*,*) 'set profiles'
           profiles(1)%id='standard'
           !              write(*,*) 'set p'
           !convert from Pa to hPa


!          write(*,*) 'set t'
           profiles(1)%t(1:preproc_dims%kdim_pre)= &
                & preproc_prtm%temperature(idim,jdim,:)


!           write(*,*) 'set q'
           !convert from kg/kg to ppmv by multiplying with:
           !dry air molecule weight(28.9644)/molcule weight of gas
           !(e.g. o3  47.9982)*1.0E6
           !write(*,*)'spechum',preproc_prtm%spec_hum(idim,jdim,:)
           
           profiles(1)%q(1:preproc_dims%kdim_pre)= &
                & preproc_prtm%spec_hum(idim,jdim,:)*q_mixratio_to_ppmv
           
!            write(*,*)'q', profiles(1)%q(:)
           ! write(*,*) 'set o3'
           profiles(1)%o3(1:preproc_dims%kdim_pre)= &
                & preproc_prtm%ozone(idim,jdim,:)*o3_mixratio_to_ppmv
!           write(*,*)'ozone',preproc_prtm%ozone(idim,jdim,:)
           !2m/10m variables
          
           if (trim(sensor) .eq. 'ATSR' .or. trim(sensor) .eq. 'AATSR'  ) then
              ! surface pressure is read from BADC netcdf files
              profiles(1)%s2m%p=preproc_prtm%surface_pressure(idim,jdim)/pa2hpa
              profiles(1)%s2m%t=preproc_prtm%temp2(idim,jdim)
              profiles(1)%p(1:preproc_dims%kdim_pre)= &
                   preproc_prtm%pressure(idim,jdim,:)/pa2hpa
           else
              profiles(1)%s2m%p=exp(preproc_prtm%lnsp(idim,jdim))/pa2hpa
              profiles(1)%s2m%t=preproc_prtm%temp2(idim,jdim)
              profiles(1)%p(1:preproc_dims%kdim_pre)= &
                   preproc_prtm%pressure(idim,jdim,:)/pa2hpa
           end if

           ! Check the pressure values against the RTTOV max pressure
           ! value and round down if required
           where(profiles(1)%p .gt. pmax) profiles(1)%p = pmax
           if (profiles(1)%s2m%p .gt. pmax) profiles(1)%s2m%p = pmax

           profiles(1)%s2m%u=preproc_prtm%u10(idim,jdim)
           profiles(1)%s2m%v=preproc_prtm%v10(idim,jdim)
            
           !This is a dummy as we don't have these variables
           profiles(1)%s2m%q=profiles(1)%q(profiles(1)%nlevels-1)
           profiles(1)%s2m%o=profiles(1)%o3(profiles(1)%nlevels-1)
           profiles(1)%s2m%wfetc=100000.0
           profiles(1)%ctp=profiles(1)%p(profiles(1)%nlevels-1)
           profiles(1)%cfraction=0.00
           !skin variables
           profiles(1)%skin%t=preproc_prtm%skin_temp(idim,jdim)

           ! Add surface values to the profile. rttov documentataion states
           ! that 'The user profile lowest level should be equal or below
           ! the surface pressure'
           profiles(1)%p(preproc_dims%kdim_pre)=profiles(1)%s2m%p
           profiles(1)%t(preproc_dims%kdim_pre)=profiles(1)%skin%t
           profiles(1)%q(preproc_dims%kdim_pre)=profiles(1)%q(preproc_dims%kdim_pre-1)
           !profiles(1)%s2m%q
           profiles(1)%o3(preproc_dims%kdim_pre)=profiles(1)%o3(preproc_dims%kdim_pre-1)
           
           test_write=0
           if (test_write .eq. 1) then       
              write(*,*)'pressure', profiles(1)%p(:)
              write(*,*)'temp', profiles(1)%t(:)
              ! write(*,*)'humidity', profiles(1)%q(:) 
              write(*,*)'ozone', profiles(1)%o3(:)
              ! write(*,*) '2mp',profiles(1)%s2m%p
              ! write(*,*) '2mt',profiles(1)%s2m%t
              write(*,*) '2mq',profiles(1)%s2m%q
              ! write(*,*) '2mu',profiles(1)%s2m%u
              ! write(*,*) '2mv',profiles(1)%s2m%v
              write(*,*) 'CTP',profiles(1)%ctp
              ! write(*,*) opts % addclouds
              write(*,*) 'skin temp ',profiles(1)%skin%t
              write(*,*) 'temp at 2m',profiles(1)%s2m%t
              write(*,*) 'surface pressure',profiles(1)%s2m%p
              write(*,*) 'sst ', preproc_prtm%sst(idim,jdim)
           endif

           !Hardwire surface types
           profiles(1) % skin % surftype  = 0
           profiles(1) % skin % watertype = 1
           profiles(1)%elevation=filter_micro
           !              write(*,*)'filter_micro',filter_micro
           
           profiles(1)%latitude=preproc_geoloc%latitude(jdim)

           if(preproc_geoloc%longitude(idim) .gt. 180.00) then
              profiles(1)%longitude=preproc_geoloc%longitude(idim)-360.0
           else
              profiles(1)%longitude=preproc_geoloc%longitude(idim)
           endif
           
           
           !todo set up emissivities
           !handle snow fraction with poor man's approach
           !write(*,*) preproc_prtm%snow_albedo(idim,jdim),'preproc_prtm%snow_albedo(idim,jdim)'
           if(preproc_prtm%snow_albedo(idim,jdim) .gt. filter_micro) &
                & profiles(1)%snow_frac=1.0
           !
           !lw rtm
           !
           

           if(icoeffs .le. 2) then
              
              !              
              ! angles for different views
              !            
              
              do  iangle=1,imager_angles%nviews
                 
                 profiles(1)%zenangle=preproc_geo%satza(idim,jdim,iangle)
                 profiles(1)%azangle=preproc_geo%solazi(idim,jdim,iangle)
                 profiles(1)%sunzenangle=preproc_geo%solza(idim,jdim,iangle)
                 profiles(1)%sunazangle=preproc_geo%relazi(idim,jdim,iangle)

                 if(profiles(1)%sunzenangle .gt. maxsza_day .and. icoeffs .eq. 2) cycle
                 if(profiles(1)%sunzenangle .eq. -999.0) cycle

!               if (profiles(1)%zenangle .gt. -999.0) then
!                  write(*,*)'iidm,jdim',idim,jdim        
!                  write(*,*) 'profiles(1)%zenangle',profiles(1)%zenangle
!                  write(*,*) 'profiles(1)%sunzenangle',profiles(1)%sunzenangle
!                  write(*,*) 'profiles(1)%azangle',profiles(1)%azangle
!                  write(*,*) 'profiles(1)%sunazangle',profiles(1)%sunazangle
!              endif
                 
                 !
                 !check profile first before do anything else
                 !
               
                 call rttov_user_profile_checkinput(opts, profiles(1), &
                      & coefs(nrttovid),errorstatus)
!                 if (errorstatus.ne.0) then
!                    write(*,*) 'O3  = ',minval(profiles(1)%o3), maxval(profiles(1)%o3)
!                    write(*,*) 'WV  = ',minval(profiles(1)%q), maxval(profiles(1)%q)
!                    write(*,*) 'profile WV  = ',(profiles(1)%q)
                 !   pause
!                 endif

                 Call rttov_errorhandling(Err_unit, verbosity_level)
                 
!only write profile check outin testing phase
!                 open(100,file='profile.check',status='replace')
!                 call rttov_print_profile(profiles(1),lu=100,text='profile_test')
!                 close(100)

		 if( errorstatus .lt. 4 .and. iangle .eq. 1 .and. &
        & icoeffs .eq. 1 .and. profiles(1)%sunazangle .gt. -999. ) then
      
      !
      !----------------------------
      ! Retrieve values from atlas
      !----------------------------
      !
      
      call rttov_get_emis(                &
           errorstatus,                    &
           chanprof,               &
           profiles,               &
           coefs(nrttovid)%coef,             &
           emissivity = emissivity,&
           emis_std = emis_std,    &
           emis_flag = emis_flag   )
      !                 write(*,*) 'emissivity',emissivity
      
      Call rttov_errorhandling(Err_unit, verbosity_level)
      !                 
      ! if returned emissivity is spurious (water surface) then calculate emissivity
      ! if it is not (land surface) then use it.
      !
      if(minval(emissivity) .le. dither) then
         calcemis=.true.
         emissivity=filter_micro
         emissivity_in=emissivity
      else
         calcemis=.false.
         emissivity_in=emissivity
         emissivity_out=emissivity
      endif
      !if SW, we set emissivity to 0 and do not compute it?
      !              elseif(icoeffs .eq. 2 ) then
      ! this is SW so no emissivity required
      !                 calcemis=.false.
      !                 emissivity_in=filter_micro
      !                 emissivity=filter_micro
   endif ! loop over icoeffs
   
   !              write(*,*) 'emissivity_in',emissivity_in,calcemis
   
   
   !
   ! if this is a good preprocessing pixel do the RT computation
   !LW
   if(icoeffs .eq. 1 .and. preproc_dims%filter_array_lw(idim,jdim) & 
        & .eq. 1 .and. errorstatus .eq. 0) then
 
#ifdef _OPENMP
      !write(*,*) 'using parallel RTTOV'
      call rttov_parallel_direct(errorstatus,chanprof,opts,profiles,coefs(nrttovid),&
           & calcemis,emissivity_in,emissivity_out,&
           & transmission,radiance,nthreads=nompthreads)
#else

!write(*,*)'emissivity_in',emissivity_in
      !write(*,*) 'using serial RTTOV'
      call rttov_direct(errorstatus,chanprof,opts,profiles, &
           & coefs(nrttovid),calcemis,emissivity_in,emissivity_out,&
           & transmission,radiance)

#endif
      if (errorstatus .ne. 0) then 
         write(*,*) 'errorstatus rttov direct',errorstatus
      endif
      !
      !calculate radiance
      !
      !addcosmic = (coefs(nrttovid)%coef%id_sensor == sensor_id_mw .OR. &
      !     & coefs(nrttovid)%coef%id_sensor == sensor_id_po)
      !auxrad%cosmic=0
      !auxrad%skin=0
      !auxrad%surfair=0
      !auxrad%air=0
      ! do not need this
      !      call rttov_calcrad(addcosmic, chanprof, profiles,coefs(nrttovid)%coef,&
      !           & auxrad%cosmic, auxrad%skin, auxrad%surfair, auxrad%air)!out

      Call rttov_errorhandling(Err_unit, verbosity_level)
      
      
      !
      !calculate lw values
      !

      call  call_rtm_ir_rttov(errorstatus,transmission,radiance, &
           & imager_angles,channel_info,preproc_lwrtm)
      
      !SW
   elseif(icoeffs .eq. 2 .and. preproc_dims%filter_array_sw(idim,jdim) &
        & .eq. 1 .and. errorstatus .eq. 0) then
      !emissivity_out=filter_micro
 
#ifdef _OPENMP
      !write(*,*) 'using parallel RTTOV'
      call rttov_parallel_direct(errorstatus,chanprof,opts,profiles,coefs(nrttovid),&
           & calcemis,emissivity_in,emissivity_out,&
           & transmission,radiance,nthreads=nompthreads)
#else
      !write(*,*) 'using serial RTTOV'
      call rttov_direct(errorstatus,chanprof,opts,profiles,coefs(nrttovid), &
           & calcemis,emissivity_in,emissivity_out,&
           & transmission,radiance)
#endif

      !calculate radiance
      !addcosmic = (coefs(nrttovid)%coef%id_sensor == sensor_id_mw .OR. &
      !     & coefs(nrttovid)%coef%id_sensor == sensor_id_po)
      !auxrad%cosmic=0
      !auxrad%skin=0
      !auxrad%surfair=0
      !auxrad%air=0
      !call rttov_calcrad(addcosmic, chanprof, profiles,coefs(nrttovid)%coef,&
      !     & auxrad%cosmic, auxrad%skin, auxrad%surfair, auxrad%air) !out
      
      Call rttov_errorhandling(Err_unit, verbosity_level)
      
      
      sza=profiles(1)%zenangle
      lza=profiles(1)%sunzenangle
      !
      !get airmass factor
      !
      call effective_2way_za(sza,lza,amf,za)
      !profiles(1)%zenangle=za
      
      !
      !calculate shortwave values
      !
 !     write(*,*)' call solar'
      
      call  call_rtm_solar_rttov(errorstatus,transmission, &
           &  imager_angles,channel_info,preproc_swrtm,amf,za)
      

   endif !loop over icoeffs
   !radiances units are (mw/cm-1/ster/sq.m) and degK
   !
   !convert brightness temperatute to radiance
   !
   ! convert radiance to brightmess
   ! call rttov_calcbt(chanprof, coeffs, rad)
   
   
enddo !loop over views




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!now write out data to netcdf files
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


if(icoeffs.eq.1) then

   ! write the meteo data
   pixel_counter_pw=pixel_counter_pw+1
   
   ! 1D variables
   netcdf_info%start_1d(1)=pixel_counter_pw
   netcdf_info%counter_1d(1)=1
   netcdf_info%stride_1d(1)=1
   ! counter
   dummy_lint_1d(1)=pixel_counter_pw
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
        &  netcdf_info%counterid_pw,dummy_lint_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d, &
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write iindex rttov'
   
   ! iindex
   dummy_lint_1d(1)=idim-preproc_dims%preproc_min_lon+1
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, & 
        & netcdf_info%iid_pw,dummy_lint_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        &  netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write iindex rttov'
   
   ! jindex
   dummy_lint_1d(1)=jdim-preproc_dims%preproc_min_lat+1
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
        & netcdf_info%jid_pw,dummy_lint_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write jindex rttov'
   
   
   ! longitude
   dummy_real_1d(1)=profiles(1)%longitude
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm,&
        & netcdf_info%lonid_pw,dummy_real_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lon rttov'
   
   
   ! latitude
   dummy_real_1d(1)=profiles(1)%latitude
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm,&
        & netcdf_info%latid_pw,dummy_real_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lat rttov'
   
   
   
   
   ! satzen
   dummy_real_1d(1)=profiles(1)%zenangle
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm,&!
        & netcdf_info%satzenid_pw,dummy_real_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lon rttov'
   
   
   ! solzen
   dummy_real_1d(1)=profiles(1)%sunzenangle
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm,&
        & netcdf_info%solzenid_pw,dummy_real_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write solzen rttov'



   ! skin temp
   dummy_real_1d(1)=profiles(1)%skin%t
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm,&
        & netcdf_info%skintid_pw,dummy_real_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write skin temp rttov'



   ! exp(lnsp)
   dummy_real_1d(1)=profiles(1)%s2m%p
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm,&
        & netcdf_info%lnspid_pw,dummy_real_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lnsp rttov'
   


   ! (lsf)
   dummy_real_1d(1)= preproc_prtm%land_sea_mask(idim,jdim)
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm,&
        & netcdf_info%lsfid_pw,dummy_real_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lsf rttov'
   


   !
   ! 2d variables
   !
   
   netcdf_info%start_2d(1)=1
   netcdf_info%counter_2d(1)=profiles(1)%nlevels
   netcdf_info%stride_2d(1)=1                 
   
   netcdf_info%start_2d(2)=pixel_counter_pw
   netcdf_info%counter_2d(2)=1
   netcdf_info%stride_2d(2)=1                 
   
   
   !	      	 dummy_real_2d=real_fill_value
   !   write(*,*) 'shape prof', shape(profiles(1)%p(:))
   !   write(*,*) 'netcdf_info%counter_2d',netcdf_info%counter_2d
   !   write(*,*) 'dummy_real_2d(:,1)',shape(dummy_real_2d(:,1)) 
   ! write(*,*) profiles(1)%p(:)
   !write(*,*) netcdf_info%counter_2d
!
!
!pressure
!

   dummy_real_2d(:,1)=profiles(1)%p(:)
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
        & netcdf_info%pprofile_lev_id_pw,dummy_real_2d,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,&
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write pprof'
   
   ! temperature (at layer centers of preprocessing profile, interfaces for RTTOV)
   dummy_real_2d=real_fill_value
   
   dummy_real_2d(:,1)=profiles(1)%t(:)
 
   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
        & netcdf_info%tprofile_lev_id_pw,dummy_real_2d,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,&
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write pprof'
   

   !geopotential height profile (at lever centers of preprocessing profile, interfaces for RTTOV)

   dummy_real_2d=real_fill_value
   dummy_real_2d(:,1)=preproc_prtm%phi_lev(idim,jdim,:)

   ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
        & netcdf_info%hprofile_lev_id_pw,dummy_real_2d,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,& 
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write pot prof'
   
   !write the lw information
   !                 write(*,*) 'lw'
   pixel_counter_lw=pixel_counter_lw+1
   
   
   ! 1D variables
   ! counter                 
   
   netcdf_info%start_1d(1)=pixel_counter_lw
   netcdf_info%counter_1d(1)=1
   netcdf_info%stride_1d(1)=1
   dummy_lint_1d(1)=pixel_counter_lw
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm,&
        & netcdf_info%counterid_lw,dummy_lint_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d, &
        &	netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write iindex lw rttov'
   
   
   ! 2D variables
   
   allocate(dummy_real_2dems(size(channel_info%channel_ids_rttov_coef_lw),1))
   !                 write(*,*) 'lw1a'
   dummy_real_2dems=real_fill_value
   netcdf_info%start_2d(1)=1
   netcdf_info%counter_2d(1)=channel_info%nchannels_lw
   netcdf_info%stride_2d(1)=1
   netcdf_info%start_2d(2)=pixel_counter_lw
   netcdf_info%counter_2d(2)=1
   netcdf_info%stride_2d(2)=1
   
   ! emissivity
   
   !write(*,*) shape(emissivity_out),icoeffs
   !write(*,*) 'lw1ba'
   !write(*,*) emissivity_out
   !write(*,*) 'lw1bc'
   
   ! pressure profile (at lever centers of preprocessing profile, interfaces for RTTOV)
   ! write out emissivity
   dummy_real_2dems(:,1)=emissivity_out(:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_2dems(:,1)=real_fill_value
   !write(*,*) 'lw1bd'
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &	
        & netcdf_info%emiss_id_lw,dummy_real_2dems,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,&
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write iindex lw rttov'
   !write(*,*) 'lw1c'
   deallocate(dummy_real_2dems)
   
   
   ! write out lwrtm angles
   
   allocate(dummy_real_2dveca(imager_angles%nviews,1))
   dummy_real_2dveca=real_fill_value
   netcdf_info%start_2d(1)=1
   netcdf_info%counter_2d(1)=imager_angles%nviews
   netcdf_info%stride_2d(1)=1
   netcdf_info%start_2d(2)=pixel_counter_lw
   netcdf_info%counter_2d(2)=1
   netcdf_info%stride_2d(2)=1
   !solazi
   dummy_real_2dveca(:,1)=preproc_geo%solza(idim,jdim,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0  ) &
        & dummy_real_2dveca(:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &	 
        & netcdf_info%solzaid_lw,dummy_real_2dveca,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,&
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write lw solazi angles'                    
   !satazi
   
   dummy_real_2dveca(:,1)=preproc_geo%satza(idim,jdim,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_2dveca(:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &	   
        & netcdf_info%satzaid_lw,dummy_real_2dveca,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,&
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write lw satazi angles'                    
   
   !write(*,*) 'lw4'
   !relazi
   dummy_real_2dveca(:,1)=preproc_geo%relazi(idim,jdim,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0  ) &
        & dummy_real_2dveca(:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm,&
        & netcdf_info%relaziid_lw,dummy_real_2dveca,&
        & netcdf_info%start_2d, netcdf_info%counter_2d, &
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write lw relazi angles'                    
   
   deallocate(dummy_real_2dveca)
   
   !write out lwrtm 3d variables
   allocate(dummy_real_3d(size(channel_info%channel_ids_rttov_coef_lw),&
        & preproc_dims%kdim_pre,1))
   dummy_real_3d=real_fill_value
   
   netcdf_info%start_3d(1)=1
   netcdf_info%counter_3d(1)=channel_info%nchannels_lw
   netcdf_info%stride_3d(1)=1                 
   
   netcdf_info%start_3d(2)=1
   netcdf_info%counter_3d(2)=profiles(1)%nlevels
   netcdf_info%stride_3d(2)=1                 
   
   netcdf_info%start_3d(3)=pixel_counter_lw
   netcdf_info%counter_3d(3)=1
   netcdf_info%stride_3d(3)=1                 
   
  !tac
!   write(*,*) 'lw5 tauac',preproc_lwrtm%tauac(:,:)

   dummy_real_3d(:,:,1)=preproc_lwrtm%tauac(:,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_3d(:,:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm,&
        & netcdf_info%tac_id_lw,dummy_real_3d,&
        & netcdf_info%start_3d, netcdf_info%counter_3d, &
        & netcdf_info%stride_3d)
   if (ierr.NE.NF90_NOERR) stop 'err write tac lw'                 
   
   
   !tbc
   !if( preproc_lwrtm%taubc(1,1) .gt. -999) then
   !write(*,*) 'lw6 taubc',preproc_lwrtm%taubc(:,:)
!   write(*,*) 'lw6 shape dummy_real',shape( dummy_real_3d(:,:,1))
!   write(*,*) 'lw6 shape taubc',shape(preproc_lwrtm%taubc(:,:))

   !endif
   dummy_real_3d(:,:,1)=preproc_lwrtm%taubc(:,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_3d(:,:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm,&
        & netcdf_info%tbc_id_lw,dummy_real_3d,&
        & netcdf_info%start_3d, netcdf_info%counter_3d,&
        & netcdf_info%stride_3d)
   if (ierr.NE.NF90_NOERR) stop 'err write tbc lw'                 
   
   !radbc_up
   !if (preproc_lwrtm%radbc(1,1) .gt. -999) then
   !                 write(*,*) 'lw7radbc',preproc_lwrtm%radbc(:,:)
   
   !endif
   dummy_real_3d(:,:,1)=preproc_lwrtm%radbc(:,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_3d(:,:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm,&
        & netcdf_info%rbc_up_id_lw,dummy_real_3d,&
        & netcdf_info%start_3d, netcdf_info%counter_3d, &
        & netcdf_info%stride_3d)
   if (ierr.NE.NF90_NOERR) stop 'err write rabc_up lw'                 
   
   !rac_up
   !                 write(*,*) 'lw8'
   dummy_real_3d(:,:,1)=preproc_lwrtm%radiance_up(:,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_3d(:,:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
        & netcdf_info%rac_up_id_lw,dummy_real_3d,&
        & netcdf_info%start_3d, netcdf_info%counter_3d,&
        & netcdf_info%stride_3d)
   if (ierr.NE.NF90_NOERR) stop 'err write rabc_up lw'                 
   
   !rac_down
   !                 write(*,*) 'lw9'
   dummy_real_3d(:,:,1)=preproc_lwrtm%radiance_down(:,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_3d(:,:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm,&
        & netcdf_info%rac_down_id_lw,dummy_real_3d,&
        & netcdf_info%start_3d, netcdf_info%counter_3d,&
        & netcdf_info%stride_3d)
   if (ierr.NE.NF90_NOERR) stop 'err write rac_down lw'                 
   
   
   deallocate(dummy_real_3d)
   
   
   !write the sw information
elseif(icoeffs.eq.2) then

   pixel_counter_sw=pixel_counter_sw+1
   
   ! 1D variables
   netcdf_info%start_1d(1)=pixel_counter_sw
   netcdf_info%counter_1d(1)=1
   netcdf_info%stride_1d(1)=1
   ! counter
   dummy_lint_1d(1)=pixel_counter_sw
   ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
        & netcdf_info%counterid_sw,dummy_lint_1d,&
        & netcdf_info%start_1d, netcdf_info%counter_1d,&
        & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write iindex sw rttov'
   
   !angles
   allocate(dummy_real_2dveca(imager_angles%nviews,1))
   dummy_real_2dveca=real_fill_value
   netcdf_info%start_2d(1)=1
   netcdf_info%counter_2d(1)=imager_angles%nviews
   netcdf_info%stride_2d(1)=1
   netcdf_info%start_2d(2)=pixel_counter_sw
   netcdf_info%counter_2d(2)=1
   netcdf_info%stride_2d(2)=1
   !solazi
   dummy_real_2dveca(:,1)=preproc_geo%solza(idim,jdim,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_2dveca(:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm,&
        & netcdf_info%solzaid_sw,dummy_real_2dveca,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,&
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write sw solazi angles'                    
   !satazi
   dummy_real_2dveca(:,1)=preproc_geo%satza(idim,jdim,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
        & dummy_real_2dveca(:,1)=real_fill_value	
   ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm,&
        & netcdf_info%satzaid_sw,dummy_real_2dveca,&
        & netcdf_info%start_2d, netcdf_info%counter_2d,&
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write sw satazi angles'                    
   
   !relazi
   dummy_real_2dveca(:,1)=preproc_geo%relazi(idim,jdim,:)
   if(preproc_dims%filter_array_lw(idim,jdim) .eq. 0 )&	
        & dummy_real_2dveca(:,1)=real_fill_value
   ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
        & netcdf_info%relaziid_sw,dummy_real_2dveca,&
        & netcdf_info%start_2d, netcdf_info%counter_2d, &
        & netcdf_info%stride_2d)
   if (ierr.NE.NF90_NOERR) stop 'err write sw relazi angles'                    
   
   deallocate(dummy_real_2dveca)
   
   
   
   allocate(dummy_real_3d(size(channel_info%channel_ids_rttov_coef_sw), &
        & preproc_dims%kdim_pre,1))
   dummy_real_3d=real_fill_value
   
   netcdf_info%start_3d(1)=1
   netcdf_info%counter_3d(1)=channel_info%nchannels_sw
   netcdf_info%stride_3d(1)=1                 
   
   netcdf_info%start_3d(2)=1
   netcdf_info%counter_3d(2)=profiles(1)%nlevels
   netcdf_info%stride_3d(2)=1                 
   
   netcdf_info%start_3d(3)=pixel_counter_sw
   netcdf_info%counter_3d(3)=1
   netcdf_info%stride_3d(3)=1                 


   !tac
                 dummy_real_3d(:,:,1)=preproc_swrtm%tauac(:,:)
                 if(preproc_dims%filter_array_sw(idim,jdim) .eq. 0 ) &
                      & dummy_real_3d(:,:,1)=real_fill_value
                 ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm,&
		      & netcdf_info%tac_id_sw,dummy_real_3d,&
                      & netcdf_info%start_3d, netcdf_info%counter_3d,&
		      & netcdf_info%stride_3d)
                 if (ierr.NE.NF90_NOERR) stop 'err write tac sw'                 
                 
   !tbc
                 dummy_real_3d(:,:,1)=preproc_swrtm%taubc(:,:)
                 if(preproc_dims%filter_array_sw(idim,jdim) .eq. 0 ) &
                      & dummy_real_3d(:,:,1)=real_fill_value
                 ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm,&
		      & netcdf_info%tbc_id_sw,dummy_real_3d,&
                      & netcdf_info%start_3d, netcdf_info%counter_3d,&
		      & netcdf_info%stride_3d)
                 if (ierr.NE.NF90_NOERR) stop 'err write tbc sw'                 


                 deallocate(dummy_real_3d)
                 
              endif ! else if
           endif  ! icoeefs le 2            

           !deallocate arrays
           !
        enddo ! loop over i
     enddo ! loop over j
     !
     !
     !
     
!     write(*,*) 'pixel_counter_sw,pixel_counter_lw',pixel_counter_sw, &
!          & pixel_counter_lw,pixel_counter_pw
     

     !-----------------------
     ! Deallocate rttov data i.e. asw=0 (use nlevls)
     !-----------------------
     write(*,*) 'Deallocate things now',size(radiance%up(:,1))
     asw=0 !deallocate now
     call rttov_alloc_rad(errorstatus,nchannels,radiance,preproc_dims%kdim_pre,asw)
     write(*,*) 'errorstatus dealloc rad',errorstatus

     call rttov_alloc_auxrad(errorstatus,auxrad,profiles(1)%nlevels,nchannels,asw)

     write(*,*) 'Deallocate trans',size(transmission%tau_levels(1,:))
     asw=0
     call rttov_alloc_transmission(errorstatus,transmission,preproc_dims%kdim_pre-1,nchannels,asw,init=.true._jplm)
     write(*,*) 'errorstatus dealloc tran',errorstatus

     !write(*,*) 'Deallocate prof'
     asw=0
     call rttov_alloc_prof(errorstatus,nprof,profiles,preproc_dims%kdim_pre,opts,asw,coefs=coefs(nrttovid),init=.true._jplm)
     write(*,*) 'errorstatus dealloc prof',errorstatus

     !-----------------------
     ! Deallocate atlas data
     !-----------------------
     !write(*,*) 'Deallocate atlas'
     call rttov_deallocate_atlas(coefs(nrttovid)%coef)
     write(*,*) 'errorstatus dealloc atlas',errorstatus

     !write(*,*) 'Deallocate coefs'
     call rttov_dealloc_coefs(errorstatus,coefs(nrttovid))
     write(*,*) 'errorstatus dealloc coefs',errorstatus
 
     
     ! write(*,*) 'Deallocate remaining'
     deallocate(emissivity_in)
     deallocate(emissivity_out)
     deallocate(emissivity)
     deallocate(calcemis)
     deallocate(emis_std)
     deallocate(emis_flag)

     deallocate(nchan)
     deallocate(chanprof)
     deallocate (coefs)

  enddo ! loop over coefficients
  deallocate (instrument)
 
end subroutine rttov_driver
