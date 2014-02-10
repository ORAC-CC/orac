! Name: rttov_driver.f90
!
!
! Purpose:
! Create environment within which all RTTOV related code is contained.
!
! Description and Algorithm details (rough):
! 1) Set up what hardware is required i.e which instrument.
! 2) Initialise options structure.
! 3) Begin loop over sw and lw.
! 4)    Set up coefficients.
! 5)    Allocate channel and emissivity arrays.
! 6)    Setup RTTOV included emissivity atlas.
! 7)    Allocate RTTOV profiles.
! 8)    Allocate RTTOV radiance structure.
! 9)    Allocate RTTOV transmission structure.
! 10)   Begin loop over pixels.
! 11)      Initialize RTTOV profiles.
! 12)      Begin loop over views.
! 13)         Retrieve emissivity atlas.
! 14)         Perform RTTOV direct calculation.
! 15)         Modify output to incorporate an airmass correction.
! 16)      End loop over views.
! 17)      Output the data.
! 18)   End loop over pixels.
! 19) End loop over coefficients i.e lw and sw.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
!
!
! History:
! 2012/03/27, Matthias Jerg: provides initial implementation based on the
!                 example program example_fw of  Annex X of the RTTOV V10.2
!                 user guide V1.3.
! 2012/05/23, Matthias Jerg: fixes bug with AVHRR setup.
! 2012/06/20, Caroline Poulsen: removed emissivity, implemented new calls to
!                 call_rtm_ir and call_rtm_solar
! 2012/07/29, Caroline Poulsen: improved readability added in algorithm
!                 description, added in month variable required for emissivity
! 2012/08/02, MJ: implements writing of RTTOV output to netcdf file.
! 2012/08/06, CP: added in solazi angle
! 2012/08/10, CP: modified how RTTOV_direct is called
! 2012/08/24, MJ: rearranged filtering and calling of rtm
! 2012/08/24, MJ: implemented writing of fill values in RTTOV output if not a
!                 good preproc pixel
! 2012/08/28, MJ: initialized emissivity_out properly
! 2012/08/28, CP: general tidy up of code formatting.
!                 fixed bug where prtm lat and longitude were switched around.
! 2012/08/29, CP: fix bug that gave unrealistic values of profile longitudes
!                 readded _lw _sw to filter_array, commented out printouts
!                 used profile error status to skip RTTOV calculations if
!                 appropriate
! 2012/08/31, CP: changed position of emissivity caluculation to speed up code
! 2012/09/13, CP: changed AATSR coefficient file naming
! 2012/09/13, CP: changed criterai for processing of coefficients and tidied up
!                 file
! 2012/10/04, CP: fixed bug and wrote skin temp and surface pressure to output
!                 prtm file
! 2012/10/24, CP: added extra level to profile in define_preproc_grid so had to
!                 change code to read surface info into profile
! 2012/11/14, CP: converted output to levels from layers
! 2012/11/14, CP: read in surface pressure from badc netcdf files for AATSR
!                 instrument cleaned up code
! 2013/03/08, GT: Changed the (dummy) values of surface/2m Q, O3, as well
!                 as CTP to profiles(1)%nlayers profile value as profiles(1)%
!                 nlevels is past the end of the arrays.
!                 Added check on pressure profile against the RTTOV "pmax"
!                 parameter to prevent RTTOV throwing an error. Pressures
!                 greater than pmax are set to pmax in the RTTOV profile structure.
! 2013/03/14, CP: added in land sea mask
! 2013/03/19, GT: Tidied up some debugging write statements
! 2013/07/25, MJ: tidies up some preprocessor statements
! 2013/10/29, CP: removed redundant variables nlevs and nlayers
! 2013/10/31, MJ: added another bugfix for levels/layers
! 2013/11/08, GM: added missing call to rttov_alloc_auxrad() in deallocate mode
! 2013/12/11, GM: Significant code clean up.
! 2014/01/15, GM: Removed some unnecessary RTTOV error handling calls.
! 2014/02/04, MJ: implements if to set verbosity of RTTOV based on "verbose"
!                  variable
! 2014/02/10, AP: variable renaming
!
! $Id$
!
! Bugs:
! 2012/11/14, CP: atsr ir coefficients are in reverse order a temporary code
!                 fudge has been introduced to read the coefficient files
!                 correctly
! 2013/10/14, CP: bug fix changed number of lay/level in rttov_alloc_rad and in
!                 the assignment of profiles and preproc_lwrtm%plevels to
!                 preproc_lwrtm%players

subroutine rttov_driver(coef_path,emiss_path,sensor,platform,preproc_dims, &
                        preproc_geoloc,preproc_geo,preproc_prtm,preproc_lwrtm, &
                        preproc_swrtm,imager_angles,netcdf_info,channel_info, &
                        month,ecmwf_dims,verbose)

   use netcdf

!  use ieee_arithmetic

   use preproc_constants
   use preproc_structures
   use channel_structures
   use ecmwf_structures
   use imager_structures
   use netcdf_structures

   use parkind1, only : jpim, &
                        jprb, &
                        jplm

   use rttov_const, only : q_mixratio_to_ppmv, &
                           o3_mixratio_to_ppmv, &
                           pmax, pmin

   use rttov_types, only : profile_type, &
                           radiance_aux, &
                           radiance_type, &
                           rttov_chanprof, &
                           rttov_coefs, &
                           rttov_options, &
                           transmission_type

   use rttov_coef_io_mod


   Implicit None


!#define DEBUG

#ifdef _OPENMP
#include "rttov_parallel_direct.interface"
#else
#include "rttov_direct.interface"
#endif

#include "rttov_calcrad.interface"
#include "rttov_setup.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_alloc_auxrad.interface"
#include "rttov_alloc_transmission.interface"
#include "rttov_alloc_prof.interface"
#include "rttov_dealloc_coefs.interface"

#include "rttov_atlas_setup.interface"
#include "rttov_get_emis.interface"
#include "rttov_read_coefs.interface"
#include "rttov_deallocate_atlas.interface"

#ifdef DEBUG
#include "rttov_print_profile.interface"
#include "rttov_user_profile_checkinput.interface"
#include "rttov_errorreport.interface"
#endif

   ! Arguments
   character(len=pathlength)         :: coef_path
   character(len=pathlength)         :: emiss_path
   character(len=sensorlength)       :: sensor
   character(len=platformlength)     :: platform
   type(preproc_dims_s)              :: preproc_dims
   type(preproc_geoloc_s)            :: preproc_geoloc
   type(preproc_geo_s)               :: preproc_geo
   type(preproc_prtm_s)              :: preproc_prtm
   type(preproc_lwrtm_s)             :: preproc_lwrtm
   type(preproc_swrtm_s)             :: preproc_swrtm
   type(imager_angles_s)             :: imager_angles
   type(netcdf_info_s)               :: netcdf_info
   type(channel_info_s)              :: channel_info
   integer(kind=stint)               :: month
   type(ecmwf_dims_s)                :: ecmwf_dims

   ! Loop variables
   integer(kind=jpim)                :: j
   integer(kind=jpim)                :: jch
   integer(kind=jpim)                :: nch
   integer(kind=lint)                :: idim
   integer(kind=lint)                :: jdim
   integer(kind=lint)                :: kchar
   integer(kind=lint)                :: icoeffs
   integer(kind=lint)                :: imonth
   integer(kind=lint)                :: iangle

   integer(kind=lint)                :: ierr

   ! One profile per call and one sensor
!  integer(kind=jpim)                :: nprof    = 1    ! Number of profiles per call
   integer(kind=jpim)                :: iprof    = 1
   integer(kind=jpim)                :: nrttovid = 1    ! Number of sensor coeff files to read
   integer(kind=jpim)                :: irttovid = 1

   character(len=platformlength)     :: platform_id_string

   integer(kind=jpim)                :: test_rttov_flag

   real                              :: sza,lza,amf,za


   ! RTTOV interface
   character(len=filelength)         :: coeffile

   integer(kind=jpim)                :: asw             ! allocate or deallocate switch

   integer(kind=jpim)                :: errorstatus

   integer(kind=jpim)                :: err_unit        ! (<0 for default)
   integer(kind=jpim)                :: verbosity_level ! (<0 for default)

   integer(kind=jpim)                :: nchannels

   integer(kind=jpim), allocatable   :: nchan(:)        ! number of channels per profile

   integer(kind=jpim), parameter     :: mxchn = 9000    ! max number of channels
   integer(kind=jpim)                :: input_chan(mxchn)

   integer(kind=jpim), allocatable   :: instrument(:,:) ! platform id, sat id, and sensor id
   type(rttov_options)               :: opts            ! options structure
   type(rttov_coefs),  allocatable   :: coefs(:)        ! coefficients structure
   type(rttov_chanprof), allocatable :: chanprof(:)
   real(kind=jprb),      allocatable :: emissivity(:)
   real(kind=jprb),      allocatable :: emis_std(:)
   integer(kind=jpim),   allocatable :: emis_flag(:)
   logical(kind=jplm),   allocatable :: calcemis(:)
   real(kind=jprb),      allocatable :: emissivity_in(:)
   real(kind=jprb),      allocatable :: emissivity_out(:)
   type(profile_type),   allocatable :: profiles(:)
   type(radiance_type)               :: radiance
   type(radiance_aux)                :: auxrad
   type(transmission_type)           :: transmission    ! transmittances and layer optical depths


   ! Output related variables
   integer(kind=lint)                                :: pixel_counter_lw
   integer(kind=lint)                                :: pixel_counter_sw
   integer(kind=lint)                                :: pixel_counter_pw

   integer(kind=lint)                                :: dummy_lint_1d(1)

   real(kind=sreal)                                  :: dummy_real_1d(1)
   real(kind=sreal)                                  :: dummy_real_2d(preproc_dims%kdim,1)

   real(kind=sreal),   allocatable, dimension(:,:,:) :: dummy_real_3d

   real(kind=sreal),   allocatable, dimension(:,:)   :: dummy_real_2dems

   integer(kind=lint), allocatable, dimension(:)     :: dummy_lint_1dveca
   integer(kind=lint), allocatable, dimension(:)     :: dummy_lint_1dvecb
   real(kind=sreal),   allocatable, dimension(:)     :: dummy_real_1dveca

   real(kind=sreal),   allocatable, dimension(:,:)   :: dummy_real_2dveca

   logical :: verbose

   ! Openmp variables
#ifdef _OPENMP
   integer(kind=jpim)                                :: nompthreads
   integer                                           :: omp_get_max_threads
   integer                                           :: omp_get_num_threads
#endif

   write(*,*) '---------- Entering rttov_driver() ----------'

   write(*,*) 'sensor: ', sensor
   write(*,*) 'platform: ', platform


   errorstatus = 0_jpim


   ! Check how many threads are available?
#ifdef _OPENMP
   nompthreads=omp_get_max_threads()
   write(*,*) 'RTTOV running on: ', nompthreads, 'threads'
#endif

   write(*,*) 'Set the RTTOV instrument'

   allocate(instrument(3,nrttovid))

   instrument(:,irttovid) = -999
   platform_id_string=''

   if (trim(sensor) .eq. 'AVHRR') then
      !set the instrument id
      instrument(3,irttovid)=5

      ! Check which platform the instrument is on
      !set the platform id
      if (index(trim(platform),'noaa') .ge. 1) then
         instrument(1,irttovid)=1  ! NOAA
      elseif (index(trim(platform),'metop') .ge. 1) then
         instrument(1,irttovid)=10 ! MetOp
      endif

      ! Extract sat ID string from platform string
      do kchar=1,len(platform)
         if (ichar(platform(kchar:kchar)) .ge. 48 .and. &
            ichar(platform(kchar:kchar)) .le. 57) then
            platform_id_string(kchar:kchar)=platform(kchar:kchar)
         endif
      enddo
      platform_id_string=adjustl(platform_id_string)
      !set the satellite id
      read(platform_id_string, '(i2)') instrument(2,irttovid)
   elseif (trim(sensor) .eq. 'MODIS') then
      instrument(3,irttovid)=13
      instrument(1,irttovid)=9 ! EOS Aqua or Terra

      if (trim(platform) .eq. 'TERRA') instrument(2,irttovid)=1
      if (trim(platform) .eq. 'AQUA')  instrument(2,irttovid)=2

   elseif (trim(sensor) .eq. 'ATSR' .or. trim(sensor) .eq. 'AATSR') then
      instrument(3,irttovid)=14
      instrument(1,irttovid)=11
     instrument(2,irttovid)=1

   endif


   write(*,*) 'instrument(:,irttovid): ', instrument(:,irttovid)


   ! Initialise options structure, includes only relevent flags, others are set
   ! set to defaults, check main/rttov_types.F90 for default values
   opts % addinterp        = .true.  ! Allow interpolation of input profile
   opts % addsolar         = .true.  ! Do not include reflected solar
   opts % addaerosl        = .false. ! Don't include aerosol effects
   opts % addclouds        = .false. ! Don't include cloud effects
   opts % addrefrac        = .true.  ! Include refraction in path calc
   opts % apply_reg_limits = .false. ! we do NOT move our profiles into the
                                     ! RTTOV range
   opts % ozone_data       = .true.  ! we have an ozone profile
   opts % co2_data         = .false. ! we do not have CO2 profiles
   opts % n2o_data         = .false.
   opts % co_data          = .false.
   opts % ch4_data         = .false.


   ! Initialise error management with default value for the error unit number
   ! and all error message output
   err_unit = -1
   if(verbose) then
      !everything
      verbosity_level = 3_jpim
      !only FATAL messages
   else
      verbosity_level = 1_jpim
   endif

   call rttov_errorhandling(err_unit, verbosity_level)


   ! Zero the counters
   pixel_counter_pw=0
   pixel_counter_sw=0
   pixel_counter_lw=0


   write(*,*) 'Write some static information to the output files'

   ! LW, channel id abs and instr lw:
   allocate(dummy_lint_1dveca(sum(channel_info%channel_lw_flag)))
   dummy_lint_1dveca=long_int_fill_value
   allocate(dummy_lint_1dvecb(sum(channel_info%channel_lw_flag)))
   dummy_lint_1dvecb=long_int_fill_value
   allocate(dummy_real_1dveca(sum(channel_info%channel_lw_flag)))
   dummy_real_1dveca=real_fill_value
   do icoeffs=1,channel_info%nchannels_total
      if (channel_info%channel_lw_flag(icoeffs) .eq. 1 ) then
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
          & dummy_lint_1dveca, &
          & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lw index abs'
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
          & netcdf_info%channels_id_instr_lw,dummy_lint_1dvecb, &
          & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lw index instr'
   ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
          & netcdf_info%wvn_id_lw,dummy_real_1dveca, &
          & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write lw wl'
   deallocate(dummy_lint_1dveca)
   deallocate(dummy_lint_1dvecb)
   deallocate(dummy_real_1dveca)

   ! SW, channel id abs and instr sw:
   allocate(dummy_lint_1dveca(sum(channel_info%channel_sw_flag)))
   dummy_lint_1dveca=long_int_fill_value
   allocate(dummy_lint_1dvecb(sum(channel_info%channel_sw_flag)))
   dummy_lint_1dvecb=long_int_fill_value
   allocate(dummy_real_1dveca(sum(channel_info%channel_sw_flag)))
   dummy_real_1dveca=real_fill_value
   do icoeffs=1,channel_info%nchannels_total
      if (channel_info%channel_sw_flag(icoeffs) .eq. 1 ) then
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
          & netcdf_info%channels_id_sw,dummy_lint_1dveca, &
          & netcdf_info%start_1d, netcdf_info%counter_1d, &
          & netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write sw index abs'
   ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
          & netcdf_info%channels_id_instr_sw,dummy_lint_1dvecb, &
          & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write sw index instr'
   ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
          & netcdf_info%wvn_id_sw,dummy_real_1dveca, &
          & netcdf_info%start_1d, netcdf_info%counter_1d, netcdf_info%stride_1d)
   if (ierr.NE.NF90_NOERR) stop 'err write sw wl'
   deallocate(dummy_lint_1dveca)
   deallocate(dummy_lint_1dvecb)
   deallocate(dummy_real_1dveca)


   ! Zero the counters
   pixel_counter_pw=0
   pixel_counter_sw=0
   pixel_counter_lw=0


   write(*,*) 'Loop over lw and sw calculations'

   do icoeffs=1,2 ! First do LW and then do SW

      write(*,*) 'icoeffs: ', icoeffs


      allocate(nchan(iprof))
      nchan(:) = size(channel_info%channel_ids_RTTOV_coef_lw)

      write(*,*) 'nchan: ', nchan


      write(*,*) 'Set up coefficients'

      allocate(coefs(nRTTOVid))

      ! Set coeff filename

      ! AVHRR
      if (instrument(3,iRTTOVid) .eq. 5) then
         ! NOAA
         if (instrument(1,irttovid) .eq. 1) then
            ! pick satellite directly through instrument(2,irttovid)
            ! LW
            if (icoeffs .eq. 1) then
               coeffile='rtcoef_noaa_'//trim(adjustl(platform_id_string))// &
                        '_avhrr.v10.dat'
            ! SW
            elseif (icoeffs .eq. 2) then
               coeffile='rtcoef_noaa_'//trim(adjustl(platform_id_string))// &
                        '_avhrr_visnir_v1.v10.dat'
            endif

         ! METOP
         elseif (instrument(1,irttovid) .eq. 10) then
	    ! LW
            if (icoeffs .eq. 1) then
               coeffile='rtcoef_metop_'//trim(adjustl(platform_id_string))// &
                        '_avhrr.v10.dat'
            ! SW
            elseif (icoeffs .eq. 2) then
               coeffile='rtcoef_metop_'//trim(adjustl(platform_id_string))// &
                        '_avhrr_visnir_v1.v10.dat'
           endif
         endif

      ! MODIS
      elseif (instrument(3,irttovid) .eq. 13) then
         ! TERRA
         if (instrument(2,irttovid) .eq. 1) then
            ! LW
            if (icoeffs .eq. 1) then
               coeffile='rtcoef_eos_2_modis.v10.dat'
            ! SW
            elseif (icoeffs .eq. 2) then
               coeffile='rtcoef_eos_2_modis_visnir_v1.v10.dat'
            endif
         ! AQUA
         elseif (instrument(2,irttovid) .eq. 2) then
            ! LW
            if (icoeffs .eq. 1) then
               coeffile='rtcoef_eos_2_modis.v10.dat'
           ! SW
            elseif (icoeffs .eq. 2) then
               coeffile='rtcoef_eos_2_modis_visnir_v1.v10.dat'
            endif
         endif

      ! AATSR
      elseif (instrument(3,irttovid) .eq. 14) then
         ! LW
         if (icoeffs .eq. 1) then
            coeffile='rtcoef_envisat_1_atsr.dat'
         ! SW
         elseif (icoeffs .eq. 2) then
            coeffile='rtcoef_envisat_1_atsr_visnir-spp_v10.dat'
         endif
      endif

      ! Set channel info for coeff files

      ! LW
      if (icoeffs .eq. 1 ) then
         nchan(iprof)=size(channel_info%channel_ids_rttov_coef_lw)
         input_chan(1:nchan(iprof))= channel_info%channel_ids_rttov_coef_lw(:)

      ! SW
      elseif (icoeffs .eq. 2 ) then
         nchan(iprof)=size(channel_info%channel_ids_rttov_coef_sw)
         input_chan(1:nchan(iprof))= channel_info%channel_ids_rttov_coef_sw(:)
      endif

      nchannels = nchan(iprof) ! Number of valid channels to compute radiances


      write(*,*) 'Read coefficients'

      write(*,*) 'input_chan(1:nchannels): ', input_chan(1:nchannels)

      write(*,*) 'coef file: ',trim(adjustl(coef_path))//'/'//trim(adjustl(coeffile))

      call rttov_read_coefs(errorstatus,coefs(irttovid),opts, &
           & channels=input_chan(1:nchannels),form_coef="formatted", &
           & file_coef=trim(adjustl(coef_path))//'/'//trim(adjustl(coeffile)))
      write(*,*) 'errorstatus rttov_read_coefs: ', errorstatus


      write(*,*) 'Initialize coefficients'

      call rttov_init_coefs(errorstatus,opts,coefs(irttovid))
      write(*,*) 'errorstatus rttov_init_coefs: ', errorstatus


      write(*,*) 'Allocate channel and emmissivity arrays'

      allocate(chanprof(nchan(iprof)))
      allocate(emissivity(nchan(iprof)))
      allocate(emis_std(nchan(iprof)))
      allocate(emis_flag(nchan(iprof)))
      allocate(calcemis(nchannels))
      allocate(emissivity_in(nchannels))
      allocate(emissivity_out(nchan(iprof)))
      emissivity_out=real_fill_value


      write(*,*) 'Set up chanprof array'

      nch = 0_jpim
      do j = 1, iprof
         do jch = 1, nchan(j)
            nch = nch + 1_jpim
            chanprof(nch)%prof = j
            ! set this up for RTTOV, as for the actual RTM the numbering is
            ! again different and has to start at "1" regardless of the
            ! instrument channel numbering
            chanprof(nch)%chan = jch
         end do
      end do

      ! Note that the ATSR coefficient files have the channels in reverse order
      if (trim(sensor) .eq. 'ATSR' .or. trim(sensor) .eq. 'AATSR') then
         chanprof(1)%chan = 3
         chanprof(2)%chan = 2
         chanprof(3)%chan = 1
      endif


      write(*,*) 'Set up emissivity atlas'

      imonth=month
      write(*,*) 'Set up atlas'
      if (icoeffs .le. 2 ) then
         call rttov_atlas_setup(errorstatus,imonth,coefs(irttovid)%coef, &
	      & path=emiss_path)
         write(*,*) 'errorstatus rttov_atlas_setup: ', errorstatus
      endif


      ! Allocate RTTOV structures

      asw=1 ! Tells RTTOV to allocate memory

      write(*,*) 'Allocate RTTOV profile structure: ', preproc_dims%kdim

      allocate(profiles(iprof))

      test_rttov_flag=0
      if (test_rttov_flag .eq. 0) then

         call rttov_alloc_prof(errorstatus,iprof,profiles, &
              & preproc_dims%kdim,opts,asw,coefs=coefs(irttovid), &
              & init=.true._jplm)
         write(*,*) 'errorstatus rttov_alloc_prof: ',errorstatus

         profiles(1)%nlevels=preproc_dims%kdim
         profiles(1)%nlayers=profiles(1)%nlevels-1

      else

         call rttov_alloc_prof(errorstatus,iprof,profiles, &
              & preproc_dims%kdim,opts,asw,coefs=coefs(irttovid), &
              & init=.true._jplm)
         write(*,*) 'errorstatus rttov_alloc_prof: ',errorstatus

	 preproc_dims%kdim=43
 	 profiles(1)%nlevels=43
     	 profiles(1)%nlayers=profiles(1)%nlevels-1

         ! Setup test code for RTTOV to compare with idl
!        call test_rttov(test_rttov_flag,profiles,preproc_dims)

      end if


      write(*,*) 'Allocate RTTOV radiance structure: ', preproc_dims%kdim, profiles(1)%nlevels

      call rttov_alloc_rad(errorstatus,nchannels,radiance, &
           & preproc_dims%kdim-1,asw)
      write(*,*) 'errorstatus rttov_alloc_rad: ', errorstatus, preproc_dims%kdim

      call rttov_alloc_auxrad(errorstatus,auxrad, profiles(1)%nlevels, &
           & nchannels,asw)
      write(*,*) 'errorstatus rttov_alloc_auxrad: ', errorstatus, preproc_dims%kdim


      write(*,*) 'Allocate RTTOV rtransmission structure: ', preproc_dims%kdim, profiles(1)%nlevels

      call rttov_alloc_transmission(errorstatus,transmission, &
      	   & preproc_dims%kdim-1, nchannels,asw,init=.true._jplm)
      write(*,*) 'errorstatus rttov_alloc_transmission: ', errorstatus


      write(*,*) 'Loop over preprocessing (interpolated) pixels'

      pixel_counter_pw=0
      pixel_counter_sw=0
      pixel_counter_lw=0!


      do jdim=preproc_dims%min_lat,preproc_dims%max_lat
         do idim=preproc_dims%min_lon,preproc_dims%max_lon

            ! Now assign the preprocessing grid data to the RTTOV structures

            ! profiles of p,t,o3,q and convert to units required by RTTOV
            !              write(*,*) 'set profiles'
            profiles(1)%id='standard'
            !              write(*,*) 'set p'
            ! convert from Pa to hPa

!           write(*,*) 'setting t'
            profiles(1)%t(1:preproc_dims%kdim)= &
               preproc_prtm%temperature(idim,jdim,:)
!           write(*,*)'temperature: ', preproc_prtm%temperature(idim,jdim,:)

!           write(*,*) 'setting q'
            ! convert from kg/kg to ppmv by multiplying with dry air molecule
            ! weight(28.9644)/molcule weight of gas (e.g. o3  47.9982)*1.0E6
            profiles(1)%q(1:preproc_dims%kdim)= &
               preproc_prtm%spec_hum(idim,jdim,:)*q_mixratio_to_ppmv
!           write(*,*)'spechum: ', preproc_prtm%spec_hum(idim,jdim,:)

!           write(*,*) 'setting o3'
            profiles(1)%o3(1:preproc_dims%kdim)= &
               preproc_prtm%ozone(idim,jdim,:)*o3_mixratio_to_ppmv
!           write(*,*)'ozone: ', preproc_prtm%ozone(idim,jdim,:)

            if (trim(sensor) .eq. 'ATSR' .or. trim(sensor) .eq. 'AATSR'  ) then
               ! surface pressure is read from BADC netcdf files
               profiles(1)%s2m%p=preproc_prtm%surface_pressure(idim,jdim)/pa2hpa
               profiles(1)%s2m%t=preproc_prtm%temp2(idim,jdim)
               profiles(1)%p(1:preproc_dims%kdim)= &
                  preproc_prtm%pressure(idim,jdim,:)/pa2hpa
            else
               profiles(1)%s2m%p=exp(preproc_prtm%lnsp(idim,jdim))/pa2hpa
               profiles(1)%s2m%t=preproc_prtm%temp2(idim,jdim)
               profiles(1)%p(1:preproc_dims%kdim)= &
                  preproc_prtm%pressure(idim,jdim,:)/pa2hpa
            end if

            ! Check the pressure values against the RTTOV max pressure value and
            ! round down if required
            where(profiles(1)%p .gt. pmax) profiles(1)%p = pmax
            if (profiles(1)%s2m%p .gt. pmax) profiles(1)%s2m%p = pmax

            profiles(1)%s2m%u=preproc_prtm%u10(idim,jdim)
            profiles(1)%s2m%v=preproc_prtm%v10(idim,jdim)

            ! This is a dummy as we don't have these variables
            profiles(1)%s2m%q=profiles(1)%q(profiles(1)%nlevels-1)
            profiles(1)%s2m%o=profiles(1)%o3(profiles(1)%nlevels-1)
            profiles(1)%s2m%wfetc=100000.0
            profiles(1)%ctp=profiles(1)%p(profiles(1)%nlevels-1)
            profiles(1)%cfraction=0.00
            ! skin variables
            profiles(1)%skin%t=preproc_prtm%skin_temp(idim,jdim)

            ! Add surface values to the profile. rttov documentataion states
            ! that 'The user profile lowest level should be equal or below
            ! the surface pressure'
            profiles(1)%p(preproc_dims%kdim)=profiles(1)%s2m%p
            profiles(1)%t(preproc_dims%kdim)=profiles(1)%skin%t
            profiles(1)%q(preproc_dims%kdim)=profiles(1)%q(preproc_dims%kdim-1)
!           profiles(1)%s2m%q
            profiles(1)%o3(preproc_dims%kdim)=profiles(1)%o3(preproc_dims%kdim-1)

            if (.false.) then
               write(*,*) 'pressure:  ', profiles(1)%p(:)
               write(*,*) 'temp:      ', profiles(1)%t(:)
               write(*,*) 'humidity:  ', profiles(1)%q(:)
               write(*,*) 'ozone:     ', profiles(1)%o3(:)
               write(*,*) 's2m%p:     ', profiles(1)%s2m%p
               write(*,*) 's2m%t:     ', profiles(1)%s2m%t
               write(*,*) 's2m%q:     ', profiles(1)%s2m%q
               write(*,*) 's2m%u:     ', profiles(1)%s2m%u
               write(*,*) 's2m%v:     ', profiles(1)%s2m%v
               write(*,*) 'CTP:       ', profiles(1)%ctp
               write(*,*) 'skin temp: ', profiles(1)%skin%t
               write(*,*) 'sst:       ', preproc_prtm%sst(idim,jdim)
            endif

            ! Hardwire surface types
            profiles(1)%skin%surftype  = 0
            profiles(1)%skin%watertype = 1
            profiles(1)%elevation=filter_micro

            profiles(1)%latitude=preproc_geoloc%latitude(jdim)

            if (preproc_geoloc%longitude(idim) .gt. 180.00) then
               profiles(1)%longitude=preproc_geoloc%longitude(idim)-360.0
            else
               profiles(1)%longitude=preproc_geoloc%longitude(idim)
            endif


            ! Handle snow fraction with poor man's approach
            if (preproc_prtm%snow_albedo(idim,jdim) .gt. filter_micro) &
               profiles(1)%snow_frac=1.0


            if (icoeffs .le. 2) then

               ! Loop over views
               do  iangle=1,imager_angles%nviews

                  profiles(1)%zenangle=preproc_geo%satza(idim,jdim,iangle)
                  profiles(1)%azangle=preproc_geo%solazi(idim,jdim,iangle)
                  profiles(1)%sunzenangle=preproc_geo%solza(idim,jdim,iangle)
                  profiles(1)%sunazangle=preproc_geo%relazi(idim,jdim,iangle)

                  if (profiles(1)%sunzenangle .gt. maxsza_day .and. icoeffs .eq. 2) cycle
                  if (profiles(1)%sunzenangle .eq. -999.0) cycle

                  ! Check profile first before do anything else
                  call rttov_user_profile_checkinput(opts, profiles(1), &
                       & coefs(irttovid),errorstatus)
                  if (errorstatus .eq. errorstatus_fatal) then
                     write(*,*) 'errorstatus rttov_user_profile_checkinput: ', errorstatus
                  endif

                  ! Only write profile check outin testing phase
!                 open(100,file='profile.check',status='replace')
!                 call rttov_print_profile(profiles(1),lu=100,text='profile_test')
!                 close(100)

                  if (errorstatus .lt. 4 .and. iangle .eq. 1 .and. &
                      icoeffs .eq. 1 .and. profiles(1)%sunazangle .gt. -999.) then

                     ! Retrieve values from atlas
                     call rttov_get_emis(errorstatus, &
                                         chanprof, &
                                         profiles, &
                                         coefs(irttovid)%coef, &
                                         emissivity = emissivity, &
                                         emis_std = emis_std, &
                                         emis_flag = emis_flag)
                     if (errorstatus .ne. 0) then
                        write(*,*) 'errorstatus rttov_get_emis: ', errorstatus
                     endif

                     ! If returned emissivity is spurious (water surface) then
                     ! calculate emissivity. If it is not (land surface) then
                     ! use it.
                     if (minval(emissivity) .le. dither) then
                        calcemis=.true.
                        emissivity=filter_micro
                        emissivity_in=emissivity
                     else
                        calcemis=.false.
                        emissivity_in=emissivity
                        emissivity_out=emissivity
                     endif
!                 else
!                    calcemis=.false.
!                    emissivity_in=filter_micro
!                    emissivity=filter_micro
                  endif

                  ! If this is a good preprocessing pixel do the RT computation.

                  ! LW
                  if (icoeffs .eq. 1 .and. preproc_dims%filter_array_lw(idim,jdim) &
                      .eq. 1 .and. errorstatus .eq. 0) then
#ifdef _OPENMP
                  call rttov_parallel_direct(errorstatus,chanprof,opts,profiles, &
                       & coefs(irttovid), calcemis,emissivity_in,emissivity_out, &
                       & transmission,radiance,nthreads=nompthreads)
#else
                  call rttov_direct(errorstatus,chanprof,opts,profiles, &
                       & coefs(irttovid),calcemis,emissivity_in,emissivity_out, &
                       & transmission,radiance)
#endif
                     if (errorstatus .ne. 0) then
                        write(*,*) 'errorstatus rttov_direct: ', errorstatus
                     endif

                     ! Calculate lw values
                     call call_rtm_ir_rttov(errorstatus,transmission,radiance, &
                          & imager_angles,channel_info,preproc_lwrtm)
                     if (errorstatus .ne. 0) then
                        write(*,*) 'errorstatus call_rtm_ir_rttov: ', errorstatus
                     endif

                  ! SW
                  elseif (icoeffs .eq. 2 .and. preproc_dims%filter_array_sw(idim,jdim) &
                          .eq. 1 .and. errorstatus .eq. 0) then
#ifdef _OPENMP
                     call rttov_parallel_direct(errorstatus,chanprof,opts, &
                          & profiles,coefs(irttovid),calcemis,emissivity_in, &
                          & emissivity_out,transmission,radiance, &
                          & nthreads=nompthreads)
#else
                     call rttov_direct(errorstatus,chanprof,opts,profiles, &
                          & coefs(irttovid),calcemis,emissivity_in, &
                          & emissivity_out, transmission,radiance)
#endif
                     if (errorstatus .ne. 0) then
                        write(*,*) 'errorstatus rttov_direct: ', errorstatus
                     endif

                     sza=profiles(1)%zenangle
                     lza=profiles(1)%sunzenangle

                     ! Get airmass factor
                     call effective_2way_za(sza,lza,amf,za)

                     ! Calculate sw values
                     call call_rtm_solar_rttov(errorstatus,transmission, &
                          & imager_angles,channel_info,preproc_swrtm,amf,za)

                  endif
               enddo ! Loop over views


               ! Now write out data to netcdf files
               if (icoeffs.eq.1) then

                  ! Write the meteo data
                  pixel_counter_pw=pixel_counter_pw+1


                  ! 1D variables
                  netcdf_info%start_1d(1)=pixel_counter_pw
                  netcdf_info%counter_1d(1)=1
                  netcdf_info%stride_1d(1)=1

                  ! counter
                  dummy_lint_1d(1)=pixel_counter_pw
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%counterid_pw,dummy_lint_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write iindex rttov'

                  ! iindex
                  dummy_lint_1d(1)=idim-preproc_dims%min_lon+1
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%iid_pw,dummy_lint_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write iindex rttov'

                  ! jindex
                  dummy_lint_1d(1)=jdim-preproc_dims%min_lat+1
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%jid_pw,dummy_lint_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write jindex rttov'

                  ! longitude
                  dummy_real_1d(1)=profiles(1)%longitude
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%lonid_pw,dummy_real_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lon rttov'

                  ! latitude
                  dummy_real_1d(1)=profiles(1)%latitude
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%latid_pw,dummy_real_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lat rttov'

                  ! satzen
                  dummy_real_1d(1)=profiles(1)%zenangle
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &!
                         & netcdf_info%satzenid_pw,dummy_real_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lon rttov'

                  ! solzen
                  dummy_real_1d(1)=profiles(1)%sunzenangle
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%solzenid_pw,dummy_real_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write solzen rttov'

                  ! skin temp
                  dummy_real_1d(1)=profiles(1)%skin%t
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%skintid_pw,dummy_real_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write skin temp rttov'

                  ! exp(lnsp)
                  dummy_real_1d(1)=profiles(1)%s2m%p
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%lnspid_pw,dummy_real_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lnsp rttov'

                  ! (lsf)
                  dummy_real_1d(1)= preproc_prtm%land_sea_mask(idim,jdim)
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%lsfid_pw,dummy_real_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lsf rttov'


                  ! 2d variables

                  netcdf_info%start_2d(1)=1
                  netcdf_info%counter_2d(1)=profiles(1)%nlevels
                  netcdf_info%stride_2d(1)=1

                  netcdf_info%start_2d(2)=pixel_counter_pw
                  netcdf_info%counter_2d(2)=1
                  netcdf_info%stride_2d(2)=1

                  ! Pressure profile (at level centers of preprocessing profile,
                  ! interfaces for RTTOV)
                  dummy_real_2d(:,1)=profiles(1)%p(:)
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%pprofile_lev_id_pw,dummy_real_2d, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write pprof'

                  ! Temperature profile (at layer centers of preprocessing profile,
                  ! interfaces for RTTOV)
                  dummy_real_2d=real_fill_value
                  dummy_real_2d(:,1)=profiles(1)%t(:)
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%tprofile_lev_id_pw,dummy_real_2d, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write pprof'


                  ! Geopotential height profile (at lever centers of preprocessing
                  ! profile, interfaces for RTTOV)
                  dummy_real_2d=real_fill_value
                  dummy_real_2d(:,1)=preproc_prtm%phi_lev(idim,jdim,:)
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_prtm, &
                         & netcdf_info%hprofile_lev_id_pw,dummy_real_2d, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write pot prof'


                  ! Write the lw information
                  pixel_counter_lw=pixel_counter_lw+1


                  ! 1D variables

                  ! counter
                  netcdf_info%start_1d(1)=pixel_counter_lw
                  netcdf_info%counter_1d(1)=1
                  netcdf_info%stride_1d(1)=1
                  dummy_lint_1d(1)=pixel_counter_lw
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%counterid_lw,dummy_lint_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write iindex lw rttov'


                  ! 2D variables

                  allocate(dummy_real_2dems(size(channel_info%channel_ids_rttov_coef_lw),1))

                  dummy_real_2dems=real_fill_value

                  netcdf_info%start_2d(1)=1
                  netcdf_info%counter_2d(1)=channel_info%nchannels_lw
                  netcdf_info%stride_2d(1)=1
                  netcdf_info%start_2d(2)=pixel_counter_lw
                  netcdf_info%counter_2d(2)=1
                  netcdf_info%stride_2d(2)=1

                  ! emissivity
                  dummy_real_2dems(:,1)=emissivity_out(:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_2dems(:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%emiss_id_lw,dummy_real_2dems, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write iindex lw rttov'

                  deallocate(dummy_real_2dems)


                  ! Write out lwrtm angles

                  allocate(dummy_real_2dveca(imager_angles%nviews,1))

                  dummy_real_2dveca=real_fill_value

                  netcdf_info%start_2d(1)=1
                  netcdf_info%counter_2d(1)=imager_angles%nviews
                  netcdf_info%stride_2d(1)=1
                  netcdf_info%start_2d(2)=pixel_counter_lw
                  netcdf_info%counter_2d(2)=1
                  netcdf_info%stride_2d(2)=1

                  ! solazi
                  dummy_real_2dveca(:,1)=preproc_geo%solza(idim,jdim,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0  ) &
                     & dummy_real_2dveca(:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%solzaid_lw,dummy_real_2dveca, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lw solazi angles'

                  ! satazi
                  dummy_real_2dveca(:,1)=preproc_geo%satza(idim,jdim,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_2dveca(:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%satzaid_lw,dummy_real_2dveca, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lw satazi angles'

                  ! relazi
                  dummy_real_2dveca(:,1)=preproc_geo%relazi(idim,jdim,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0  ) &
                     & dummy_real_2dveca(:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%relaziid_lw,dummy_real_2dveca, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write lw relazi angles'

                  deallocate(dummy_real_2dveca)


                  ! Write out lwrtm 3d variables
                  allocate(dummy_real_3d(size(channel_info%channel_ids_rttov_coef_lw), &
                     & preproc_dims%kdim,1))

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

                  ! tac
                  dummy_real_3d(:,:,1)=preproc_lwrtm%tauac(:,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_3d(:,:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%tac_id_lw,dummy_real_3d, &
                         & netcdf_info%start_3d, netcdf_info%counter_3d, &
                         & netcdf_info%stride_3d)
                  if (ierr.NE.NF90_NOERR) stop 'err write tac lw'

                  ! tbc
                  dummy_real_3d(:,:,1)=preproc_lwrtm%taubc(:,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_3d(:,:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%tbc_id_lw,dummy_real_3d, &
                         & netcdf_info%start_3d, netcdf_info%counter_3d, &
                         & netcdf_info%stride_3d)
                  if (ierr.NE.NF90_NOERR) stop 'err write tbc lw'

                  ! radbc_up
                  dummy_real_3d(:,:,1)=preproc_lwrtm%radbc(:,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_3d(:,:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%rbc_up_id_lw,dummy_real_3d, &
                         & netcdf_info%start_3d, netcdf_info%counter_3d, &
                         & netcdf_info%stride_3d)
                  if (ierr.NE.NF90_NOERR) stop 'err write rabc_up lw'

                  ! rac_up
                  dummy_real_3d(:,:,1)=preproc_lwrtm%radiance_up(:,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_3d(:,:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%rac_up_id_lw,dummy_real_3d, &
                         & netcdf_info%start_3d, netcdf_info%counter_3d, &
                         & netcdf_info%stride_3d)
                  if (ierr.NE.NF90_NOERR) stop 'err write rabc_up lw'

                  ! rac_down
                  dummy_real_3d(:,:,1)=preproc_lwrtm%radiance_down(:,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_3d(:,:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_lwrtm, &
                         & netcdf_info%rac_down_id_lw,dummy_real_3d, &
                         & netcdf_info%start_3d, netcdf_info%counter_3d, &
                         & netcdf_info%stride_3d)
                  if (ierr.NE.NF90_NOERR) stop 'err write rac_down lw'

                  deallocate(dummy_real_3d)


               ! Write the sw information
               elseif (icoeffs.eq.2) then

                  pixel_counter_sw=pixel_counter_sw+1

                  ! 1D variables
                  netcdf_info%start_1d(1)=pixel_counter_sw
                  netcdf_info%counter_1d(1)=1
                  netcdf_info%stride_1d(1)=1

                  ! counter
                  dummy_lint_1d(1)=pixel_counter_sw
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
                         & netcdf_info%counterid_sw,dummy_lint_1d, &
                         & netcdf_info%start_1d, netcdf_info%counter_1d, &
                         & netcdf_info%stride_1d)
                  if (ierr.NE.NF90_NOERR) stop 'err write iindex sw rttov'

                  ! Write out swrtm angles
                  allocate(dummy_real_2dveca(imager_angles%nviews,1))

                  dummy_real_2dveca=real_fill_value

                  netcdf_info%start_2d(1)=1
                  netcdf_info%counter_2d(1)=imager_angles%nviews
                  netcdf_info%stride_2d(1)=1
                  netcdf_info%start_2d(2)=pixel_counter_sw
                  netcdf_info%counter_2d(2)=1
                  netcdf_info%stride_2d(2)=1

                  ! solazi
                  dummy_real_2dveca(:,1)=preproc_geo%solza(idim,jdim,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_2dveca(:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
                         & netcdf_info%solzaid_sw,dummy_real_2dveca, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write sw solazi angles'

                  ! satazi
                  dummy_real_2dveca(:,1)=preproc_geo%satza(idim,jdim,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 ) &
                     & dummy_real_2dveca(:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
                         & netcdf_info%satzaid_sw,dummy_real_2dveca, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write sw satazi angles'

                  ! relazi
                  dummy_real_2dveca(:,1)=preproc_geo%relazi(idim,jdim,:)
                  if (preproc_dims%filter_array_lw(idim,jdim) .eq. 0 )&
                     & dummy_real_2dveca(:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
                         & netcdf_info%relaziid_sw,dummy_real_2dveca, &
                         & netcdf_info%start_2d, netcdf_info%counter_2d, &
                         & netcdf_info%stride_2d)
                  if (ierr.NE.NF90_NOERR) stop 'err write sw relazi angles'

                  deallocate(dummy_real_2dveca)



                  allocate(dummy_real_3d(size(channel_info%channel_ids_rttov_coef_sw), &
                  & preproc_dims%kdim,1))

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

                  ! tac
                  dummy_real_3d(:,:,1)=preproc_swrtm%tauac(:,:)
                  if (preproc_dims%filter_array_sw(idim,jdim) .eq. 0 ) &
                     & dummy_real_3d(:,:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
		         & netcdf_info%tac_id_sw,dummy_real_3d, &
                         & netcdf_info%start_3d, netcdf_info%counter_3d, &
		         & netcdf_info%stride_3d)
                  if (ierr.NE.NF90_NOERR) stop 'err write tac sw'

                  ! tbc
                  dummy_real_3d(:,:,1)=preproc_swrtm%taubc(:,:)
                  if (preproc_dims%filter_array_sw(idim,jdim) .eq. 0 ) &
                     & dummy_real_3d(:,:,1)=real_fill_value
                  ierr = NF90_PUT_VAR(netcdf_info%ncid_swrtm, &
		         & netcdf_info%tbc_id_sw,dummy_real_3d, &
                         & netcdf_info%start_3d, netcdf_info%counter_3d, &
		         & netcdf_info%stride_3d)
                  if (ierr.NE.NF90_NOERR) stop 'err write tbc sw'

                  deallocate(dummy_real_3d)

               endif
            endif  ! icoeffs le 2
         enddo ! Loop over i
      enddo ! Loop over j


      write(*,*) 'Deallocate RTTOV strunctures'

      asw=0 ! Tells RTTOV to deallocate memory


      call rttov_alloc_transmission(errorstatus,transmission,preproc_dims%kdim-1,nchannels,asw,init=.true._jplm)
      write(*,*) 'errorstatus dealloc tran: ', errorstatus

      call rttov_alloc_auxrad(errorstatus,auxrad,profiles(1)%nlevels,nchannels,asw)
      write(*,*) 'errorstatus dealloc tran: ', errorstatus

      call rttov_alloc_rad(errorstatus,nchannels,radiance,preproc_dims%kdim,asw)
      write(*,*) 'errorstatus dealloc rad: ', errorstatus

      call rttov_alloc_prof(errorstatus,iprof,profiles,preproc_dims%kdim,opts,asw,coefs=coefs(irttovid),init=.true._jplm)
      write(*,*) 'errorstatus dealloc prof: ', errorstatus

      call rttov_deallocate_atlas(coefs(irttovid)%coef)
      write(*,*) 'errorstatus dealloc atlas: ',errorstatus

      call rttov_dealloc_coefs(errorstatus,coefs(irttovid))
      write(*,*) 'errorstatus dealloc coefs: ',errorstatus


      write(*,*) 'Deallocate other arrays'

      deallocate(profiles)

      deallocate(emissivity_out)
      deallocate(emissivity_in)
      deallocate(calcemis)
      deallocate(emis_flag)
      deallocate(emis_std)
      deallocate(emissivity)
      deallocate(chanprof)

      deallocate(coefs)

      deallocate(nchan)

  enddo ! Loop over coefficients

  deallocate(instrument)

end subroutine rttov_driver
