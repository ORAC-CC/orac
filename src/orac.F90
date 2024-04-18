!*******************************************************************************
!
! Copyright (C) 2000-2018, RAL Space, Science and Technology Facilities Council
! Copyright (C) 2000-2018, University of Oxford
! Copyright (C) 2011-2018, Deutscher Wetterdienst
!
! This file is part of the Optimal Retrieval of Aerosol and Cloud (ORAC).
!
! ORAC is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! ORAC is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE. See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! ORAC. If not, see <http://www.gnu.org/licenses/>.
!
!*******************************************************************************


!-------------------------------------------------------------------------------
! Name: orac.F90
!
! Purpose:
! Main program for the Enhanced Cloud Processor prototype. Calls subordinate
! functions to read in data and process.
!
! Description and Algorithm details:
!  Data Preparation Section: reads in driver file and all SAD files but not
!  the image data (which is read in segments)
!  - read driver file
!  - open log file
!  - open breakpoint file if required
!  - open output and diagnostic files
!  - allocate SAD arrays and read in SAD files
!  - read RTM data files
!  - allocate super-pixel structure SPixel, and the RTM_Pc structure to hold
!    RTM data interpolated to a given pressure level Pc.
!
!  Product generation section:
!  - initialise super-pixel values (e.g. phase used for first guess state
!    vector setting, saved state and error XnSav and SnSav used for first
!    guess setting if method is SDAD).
!  - modify user selected image area so that coordinates fall on exact
!    super-pixel boundaries (and therefore exact image segment boundaries)
!    and write log message if values are changed
!  - read in image segments up to the user's selected starting point
!  - main loop: for each row of super-pixels from user's starting y to end y:
!    - convert the y location within the image to y loc within the segment
!      (SPixel%Loc%Yseg0)
!    - if (row number corresponds to a new segment)
!      - read in the next segment from the MSI, cloud flags files etc
!    - if (first row in the image)
!      - write control structure to the output and diagnostic files
!    - for each x location within the row (xstart to xstop in steps of the
!      SPixel size)
!      - get the current super-pixel values (measurements, cloud flags, geom
!        etc)
!      - check SPixel quality control flag: if flag indicates the SPixel
!        should not be processed:
!        - set output state vector and error arrays to indicate missing data
!      - else (process the SPixel)
!        - call Invert_Marquardt to calculate the current state vector
!        - update overall statistics totals
!        - write out the retrieved state vector and errors, plus diagnostics
!    - end of x loop
!  - end of y loop
!  - write out overall statistics
!  - close files
!
! Arguments:
! See Wiki.
!
! History:
! 2000/08/02, AS: Original version (in development)
! 2001/07/11, AS: Preparing main program for integration with other ECP routines.
! 2001/08/15, AS: First fully working version of ECP. Includes image segmentation
!    Added status checks at start of main loop and after ReadSatData calls.
! 2001/08/23, AS: Added status check around overall statistics output.
! 2001/09/21, AS:  Added initial allocation of SPixel%Ym and Sy. These are
!    deallocated each time Get_SPixel is called (in Get_Measurements). Also took
!    the opportunity to put an "if" before part of the final stats output to
!    avoid divide by zero errors, and replace tabs put in by the Nedit
!    auto-indent feature with spaces.
! 2001/09/24, AS: Moved initial allocation of SPixel%Ym, Sy, X and XI to
!    AllocSPixel.  Makes the main program more readable and avoids
!    re-coding test harnesses.
! 2001/10/22, AS: Added calls to deallocate routines for SPixel, RTM_Pc, RTM and
!    the MSI_Data structure. The alloc arrays in these structures *should* be
!    released automatically at the end of execution. These routines deallocate
!    explicitly in case the automatic dealloc isn't done.
! 2001/10/23, AS: Removed conversion of solar channel reflectances from % to
!    fraction. MSI files should now be written with fractional values. New
!    logical variable to track alloc state of SAD_LUT internal arrays.
!    **************** ECV work starts here *************************************
! 2011/03/09, AS: Re-applying changes from late 2001/2, MSI_luns now dimension
!    6 to allow for albedo data.
! 2011/03/22, AS: Removal of phase change. Only 1 cloud class required for each
!    retrieval run. SADCloudClass and SAD_LUT array dimensions changed.
! 2011/03/23, AS: Added output of latitude and longitude to output file.
! 2011/03/30, AS: Removal of super-pixel averaging. All super-pixelling will now
!    be done in pre-processing. The SPixel structure used here now refers to a
!    single pixel from the input files. Removed the modification of image (x,y)
!    processing ranges to whole number of super-pixels. Remove use of
!    Ctrl%Resoln%Space.
! 2011/04/08, AS: Simplification of selection methods for first guess, a priori,
!    limits etc. InvertMarquardt no longer requires SAD_CloudClass argument.
!    Removed setting of SPixel%Phase: redundant following removal of phase
!    change functionality.
! 2011/05/11, AS: Extension to multiple viewing angles. Elements of the Ctrl
!    struct are now pointers to arrays rather than fixed-size arrays, so Ctrl
!    cannot be written in a single operation. Removed the writes of Ctrl to the
!    diag and out file. It is planned to remove Ctrl from the out file anyway.
! 2011/06/08, CP: Removed diagnostic structure. Added extra output file residual,
!    quality indicators, a priori and first guess also input structure MSI_luns
!    now dimension 7 to allow for scanline data.
! 2011/06/08, AS: Tidied up log output. Use ORAC not ECP in log file. Trim run
!    ID string. Added Write_Log call at end, to get finish time.
! 2011/09/22, CP: remove sw%p as now the same as lw%p
! 2011/10/08, CP: added CWP to output
! 2011/11/08, CP: added y0 and sx to output
! 2011/12/08, MJ: added code to accommodate netcdfoutput
! 2011/12/12, CP: remove relenlog to be compatible with f95
! 2011/12/19, MJ: cleaned up netcdf output, introduced error reporting and added
!    file headers.
! 2012/01/05, CP: removed binary output files
! 2012/01/15, CP: added missing for ymfit
! 2012/03/27, MJ: changes netcdf write to 2D arrays
! 2012/07/13, MJ: implements option to read drifile path from command line
! 2012/08/14, MJ: irons out bug that made ORAC crash with gfortran
! 2012/08/14, MJ: changes scaling of CWP output
! 2012/08/22, MJ: includes time in MSI structure and writes it to primary netcdf
!    file
! 2012/08/22, MJ: makes adaptions to read netcdf files, start and end indices
!    of area to be processed determined by preprocessing file contents and not
!    hardwired any more.
! 2012/08/27, MJ: better implements time variable in output.
! 2012/11/01, MJ: implements OpenMP parallelization of along-track loop.
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt & cth
! 2013/12/05, MJ: initializes Diag%AK=sreal_fill_value
! 2013/12/10, MJ: initializes ymfit and y0 with MissingXn
! 2014/01/12, GM: Added some missing deallocates.
! 2014/01/28, GM: Cleaned up code.
! 2014/01/29, GM: Some OpenMP fixes. Ctrl is actually shared.  No need to make
!    it private. Also many variables set to be 'privatefirst' should just be
!    'private', i.e. they do not need to enter the parallel loop initialised.
!    Finally status_line was not needed. Status is private within the loop.
! 2014/02/10, MJ: Put the correct boundaries lat/lon for adaptive processing
!    back in.
! 2014/06/04, MJ: Introduced "WRAPPER" for c-preprocessor and associated
!    variables.
! 2014/06/12, GM: OpenMP functions should be declared by the omp_lib module, not
!    explicitly.
! 2014/06/13, GM: Put NetCDF output related includes into subroutines.
! 2014/06/15, GM: Set CTH and CTT values to missing in the case when a retrieval
!    is not possible.
! 2014/08/18, AP: Updating to preprocessor's NCDF routines.
! 2014/12/01, CP: added new global and source attributes
! 2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!    channels with respect to the channels actually processed, rather than the
!    MSI file. Eliminate conf structure.
! 2014/01/30, AP: Read surface level of RTTOV files. Allow warm start
!    coordinates to be specified in the driver file. Remove SegSize.
! 2015/02/04, OS: drifile is passed as call argument for WRAPPER
! 2015/05/25, GM: Some cleanup involving Diag.
! 2015/07/31, AP: Rejig Diag for longer, variable state vector.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2015/11/17, OS: Minor edit.
! 2015/12/30, AP: Move creation of NCDF files to after the main processing loop.
! 2016/03/04, AP: Homogenisation of I/O modules.
! 2016/07/27, GM: Changes for the multilayer retrieval.
! 2017/03/16, GT: Changes for single-view aerosol retrieval mode.
! 2017/06/21, OS: Added ann phase logical flags, which default to true
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

#ifndef WRAPPER
program orac
#else
subroutine orac(mytask,ntasks,lower_bound,upper_bound,drifile)
#endif

   ! Modules used by this program.

   use Ctrl_m
   use Data_m
   use Diag_m
   use Inversion_m
   use omp_lib
   use ORAC_Constants_m
   use orac_indexing_m
   use orac_ncdf_m
   use orac_output_m
   use prepare_output_m
   use read_driver_m
   use Read_SAD_m
   use read_utils_m
   use RTM_m
   use RTM_Pc_m
   use SAD_Chan_m
   use SAD_LUT_m
   use SPixel_m
   use SPixel_routines_m
   use global_attributes_m
   use source_attributes_m

   ! Local variable declarations

   implicit none

   type(global_attributes_t) :: global_atts
   type(source_attributes_t) :: source_atts
   type(Ctrl_t)              :: Ctrl
   type(Data_t)              :: MSI_Data
   type(Diag_t)              :: Diag ! Diagnostic struct returned by Invert_Marquardt
   type(RTM_t)               :: RTM
   type(RTM_Pc_t)            :: RTM_Pc(2)
   type(SAD_Chan_t), allocatable, dimension(:) :: SAD_Chan
   type(SAD_LUT_t)           :: SAD_LUT(2)
   type(SPixel_t)            :: SPixel

   integer            :: i, j, m
   integer            :: status ! Status value returned from subroutines

   integer            :: xstep  ! Pixels to skip when processing
   integer            :: ystep

   integer            :: TotPix   = 0   ! Total number of SPixels processed
   integer            :: TotMissed= 0   ! Number of SPixels left unprocessed
   integer            :: TotConv  = 0   ! Number of successful inversions
   integer            :: TotMaxJ  = 0   ! Number of inversions with cost > MaxQC
   integer            :: AvIter   = 0   ! Average no. of iterations per successful
                                        ! retrieval
   real               :: AvJ      = 0.0 ! Average cost per successful retrieval

   ! netcdf related variables:
   integer :: ncid_primary, ncid_secondary, dims_var(3), ch_var(1)

   ! Additional types for the scanline output for netcdf are defined
   type(output_data_primary_t)   :: output_data_1
   type(output_data_secondary_t) :: output_data_2

   ! OpenMP related variables
#ifdef _OPENMP
   integer :: n_threads, thread_num
#endif
   integer :: bitmask

   ! Some more variables for OpenMP implementation
   integer, allocatable, dimension(:) :: totpix_line, totmissed_line
   integer, allocatable, dimension(:) :: totconv_line, totmaxj_line
   integer, allocatable, dimension(:) :: aviter_line
   real,    allocatable, dimension(:) :: avj_line

#ifdef USE_TIMING
   ! This is for timing the different parts of the code on AIX IBM PWR7 at ECMWF
   integer(kind=lint) :: m0,m1,m2,m3,mclock
   real(kind=dreal)   :: r0,r1,r2,r3,rtc
   real(kind=dreal)   :: cpu_secs,real_secs
#endif
#ifdef USE_ADAPTIVE_PROCESSING
   logical            :: lhres
   real(kind=sreal)   :: range_lat(2),range_lon(2)
#endif

   ! This is for the wrapper
#ifdef WRAPPER
   character(len=FilenameLen) :: drifile
   integer :: mytask,ntasks,lower_bound,upper_bound
#endif

#ifdef USE_TIMING
   ! Initialize timing
   cpu_secs=0._dreal
   real_secs=0._dreal
   r0=rtc()
   write(*,110) r0
   m0=mclock()
   write(*,111) m0

110 format(1x,'TIMING: INITIALIZED:',1x,d15.5,1x,'r')
111 format(1x,'TIMING: INITIALIZED:',1x,i10,1x,'m')
112 format(1x,'TIMING: Lead in took:',1x,d15.5,1x,'cpu_secs and',1x,d15.5,1x,'real_secs')
113 format(1x,'TIMING: Along-track loop took:',1x,d15.5,1x,'cpu_secs and',1x,d15.5,1x,'real_secs')
114 format(1x,'TIMING: Lead out took:',1x,d15.5,1x,'cpu_secs and',1x,d15.5,1x,'real_secs')
115 format(1x,'TIMING: Ratio cpu_secs/real_secs:',1x,d15.5)
#endif

   status = 0


   !----------------------------------------------------------------------------
   ! Program initialization section
   !----------------------------------------------------------------------------

   ! Read Ctrl struct from driver file
#ifdef WRAPPER
   call Read_Driver(Ctrl, global_atts, source_atts, drifile)
#else
   call Read_Driver(Ctrl, global_atts, source_atts)
#endif

   ! Set output fields to be produced
   Ctrl%Ind%flags%do_aerosol             = Ctrl%Approach == AppAerOx .or. &
                                           Ctrl%Approach == AppAerSw .or. &
                                           Ctrl%Approach == AppAerO1
   Ctrl%Ind%flags%do_cloud               = Ctrl%Approach == AppCld1L .or. &
                                           Ctrl%Approach == AppCld2L
   Ctrl%Ind%flags%do_cloud_layer_2       = Ctrl%Approach == AppCld2L
   Ctrl%Ind%flags%do_rho                 = Ctrl%Approach == AppAerOx .or. &
                                           Ctrl%Approach == AppAerO1
   Ctrl%Ind%flags%do_swansea             = Ctrl%Approach == AppAerSw
   Ctrl%Ind%flags%do_indexing            = .true.
   Ctrl%Ind%flags%do_phase_pavolonis     = .false.
   Ctrl%Ind%flags%do_cldmask             = .true.
   Ctrl%Ind%flags%do_cldmask_uncertainty = .true.
   Ctrl%Ind%flags%do_ann_phase           = .true.
   Ctrl%Ind%flags%do_ann_phase_uncertainty = .true.
   Ctrl%Ind%flags%do_phase               = .false.
   Ctrl%Ind%flags%do_covariance          = .false.
   Ctrl%Ind%flags%do_meas_error          = .true.

   ! Set the size of the SAD_Chan and Cloud Class arrays based on the Ctrl
   ! parameters and read the SAD values.
   allocate(SAD_Chan(Ctrl%Ind%Ny))

   call Read_SAD(Ctrl, SAD_Chan, SAD_LUT)

   ! Make read in rttov data in one go, no more segment reads
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) then
      call read_input_dimensions_rtm(Ctrl%FID%PRTM, Ctrl%FID%LWRTM, &
           Ctrl%FID%SWRTM, RTM%Grid%NLon, RTM%Grid%NLat, RTM%NP, &
           RTM%LW%NLWF, RTM%SW%NSWF, Ctrl%Ind%NSolar, Ctrl%verbose)

      call Read_PRTM(Ctrl, RTM)
      if (Ctrl%Ind%NThermal > 0) &
           call Read_LwRTM(Ctrl, RTM)
      if (Ctrl%Ind%NSolar > 0) &
           call Read_SwRTM(Ctrl, RTM)
   end if

   !----------------------------------------------------------------------------
   ! Product generation section
   !----------------------------------------------------------------------------

   ! Loop over required super pixel X0,Y0 coordinates

   ! Set the start/stop positions
   if (Ctrl%Ind%X0 < 1 .or. Ctrl%Ind%X0 > Ctrl%Ind%Xmax) &
        Ctrl%Ind%X0 = 1
   if (Ctrl%Ind%X1 < 1 .or. Ctrl%Ind%X1 > Ctrl%Ind%Xmax) &
        Ctrl%Ind%X1 = Ctrl%Ind%Xmax
   if (Ctrl%Ind%Y0 < 1 .or. Ctrl%Ind%Y0 > Ctrl%Ind%Ymax) &
        Ctrl%Ind%Y0 = 1
   if (Ctrl%Ind%Y1 < 1 .or. Ctrl%Ind%Y1 > Ctrl%Ind%Ymax) &
        Ctrl%Ind%Y1 = Ctrl%Ind%YMax

   Ctrl%Ind%Xdim = Ctrl%Ind%X1 - Ctrl%Ind%X0 + 1
   Ctrl%Ind%Ydim = Ctrl%Ind%Y1 - Ctrl%Ind%Y0 + 1

   if (Ctrl%verbose) then
      write(*,*) 'Start line: ', Ctrl%Ind%Y0
      write(*,*) 'Stop line: ', Ctrl%Ind%Y1
      write(*,*) 'Total number of lines: ', Ctrl%Ind%Ydim
   end if


   ! Read all the swath data
   call Read_Data(Ctrl, MSI_Data, SAD_Chan)

   xstep = 1
   ystep = 1
#ifdef USE_ADAPTIVE_PROCESSING
   ! Adaptive processing:
   lhres = .false. ! "high" resolution flag
   if (Ctrl%InstName(1:5) .eq. 'MODIS') then

      ! Set special range
      range_lat(1) = 42.0
      range_lat(2) = 53.0

      range_lon(1) = 0.0
      range_lon(2) = 18.0

      ! Look if any pixel in current granule is in special range
      lhres=any(MSI_Data%Location%Lat .ge. range_lat(1) .and. &
                MSI_Data%Location%Lat .le. range_lat(2) .and. &
                MSI_Data%Location%Lon .ge. range_lon(1) .and. &
                MSI_Data%Location%Lon .le. range_lon(2))

      ! If yes, do higher resolution processing there.
      if (lhres) then
         xstep = 1
         ystep = 1
      ! Otherwise process reduced amount of pixels to speed things up
      else
         xstep = 2
         ystep = 2
      end if
   else
      lhres = .true.

      xstep = 1
      ystep = 1
   end if

   write(*,*) 'Adaptive processing: ',lhres,xstep,ystep
#endif

   ! Allocate output arrays
   call alloc_output_data_primary(Ctrl%Ind%common_indices_t, Ctrl%Invpar%MaxIter, &
                                  output_data_1)
   call alloc_output_data_secondary(Ctrl%Ind%common_indices_t, output_data_2)

   ! Set i, the counter for the image x dimension, for the first row processed.
   i = Ctrl%Ind%X0


   ! This is to make things easier for OpenMP
   allocate(totpix_line(Ctrl%Ind%Y0:Ctrl%Ind%Y1))
   totpix_line=0
   allocate(totmissed_line(Ctrl%Ind%Y0:Ctrl%Ind%Y1))
   totmissed_line=0
   allocate(totconv_line(Ctrl%Ind%Y0:Ctrl%Ind%Y1))
   totconv_line=0
   allocate(totmaxj_line(Ctrl%Ind%Y0:Ctrl%Ind%Y1))
   totmaxj_line=0
   allocate(aviter_line(Ctrl%Ind%Y0:Ctrl%Ind%Y1))
   aviter_line=0
   allocate(avj_line(Ctrl%Ind%Y0:Ctrl%Ind%Y1))
   avj_line=0.0

#ifdef USE_TIMING
   m1=mclock()
   write(*,111) m1
   cpu_secs=(m1-m0)*0.01
   r1=rtc()
   write(*,110) r1
   real_secs=(r1-r0) ! * 0.001
   write(*,112) cpu_secs,real_secs
   cpu_secs=cpu_secs/real_secs
   write(*,115) cpu_secs
#endif

#ifdef _OPENMP
   ! Along track loop is parallelized with OpenMP
   n_threads = omp_get_max_threads()
   if (Ctrl%verbose) &
        write(*,*) 'ORAC along-track loop now running on', n_threads, 'threads'

   ! Start OMP section by spawning the threads
   !$OMP PARALLEL &
   !$OMP PRIVATE(i,j,m,thread_num,RTM_Pc,SPixel) &
   !$OMP PRIVATE(Diag) &
   !$OMP FIRSTPRIVATE(status)

   thread_num = omp_get_thread_num()
   !$OMP CRITICAL
   if (Ctrl%verbose) write(*,*) 'Thread ', thread_num+1, 'is active'
   !$OMP END CRITICAL
#endif

   ! Allocate structures required in the main loop
   call Alloc_SPixel(Ctrl, RTM, SPixel)

   if (Ctrl%RTMIntSelm == RTMIntMethNone) then
      RTM_Pc%Hc      = sreal_fill_value
      RTM_Pc%Tc      = sreal_fill_value
      RTM_Pc%dHc_dPc = sreal_fill_value
      RTM_Pc%dTc_dPc = sreal_fill_value
   else
      call Alloc_RTM_Pc(Ctrl, RTM_Pc(1))
      if (Ctrl%Approach == AppCld2L) then
         call Alloc_RTM_Pc(Ctrl, RTM_Pc(2))
      end if

      ! Number of pressure levels in SPixel will not change from here on
      SPixel%RTM%Np = RTM%Np
      SPixel%RTM%NP = RTM%Np
   end if

   ! Initialise values required before main loop begins
   SPixel%XnSav = MissingXn
   SPixel%SnSav = MissingSn
   SPixel%Loc%LastX0 = 0
   SPixel%Loc%LastY0 = 0

   ! Start OMP parallel loop for along track direction.
   !$OMP DO SCHEDULE(GUIDED)
   do j = Ctrl%Ind%Y0,Ctrl%Ind%Y1,ystep

!     write(*,*) 'thread,Ctrl%Ind%Y0,Ctrl%Ind%Y1,iy: ', thread_num,Ctrl%Ind%Y0,Ctrl%Ind%Y1,j

      ! Set the location of the pixel within the image (Y0)
      SPixel%Loc%Y0 = j

      do i = Ctrl%Ind%X0,Ctrl%Ind%X1,xstep
         SPixel%Loc%X0 = i

         call Zero_Diag(Ctrl, Diag)

         TotPix_line(j) = TotPix_line(j)+1

         ! Set up the super-pixel data values.
         call Get_SPixel(Ctrl, SAD_Chan, SAD_LUT, MSI_Data, RTM, SPixel, status)

         ! Nothing wrong so do the inversion.
         if (status == 0) &
            call Invert_Marquardt(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, &
                                  Diag, status)

         if (status == 0) then
            call Set_Diag(Ctrl, SPixel, MSI_Data, Diag)

            ! Calculate the Cloud water path CWP
            call Calc_CWP(Ctrl, SPixel)

            ! Set values required for overall statistics 1st bit test on QC
            ! flag determines whether convergence occurred.
            if (Diag%Converged) then
               TotConv_line(j) = TotConv_line(j)+1
               AvIter_line(j)  = AvIter_line(j) + Diag%Iterations
               AvJ_line(j) = AvJ_line(j) + Diag%Jm + Diag%Ja
            end if
            if (btest(Diag%QCFlag, CostBit)) then
               TotMaxJ_line(j) = TotMaxJ_line(j)+1
            end if
         else
            ! Retrieval suffered fatal error
            TotMissed_line(j) = TotMissed_line(j) + 1

            SPixel%variables_retrieved       = status
            SPixel%X0                        = MissingXn
            SPixel%Xb                        = MissingXn
            SPixel%Xn                        = MissingXn
            SPixel%Sn                        = MissingSn
            SPixel%CWP                       = MissingXn
            SPixel%CWP_uncertainty           = MissingSn
            SPixel%CWP2                      = MissingXn
            SPixel%CWP2_uncertainty          = MissingSn
            SPixel%CTP_corrected             = MissingXn
            SPixel%CTP_corrected_uncertainty = MissingSn
            SPixel%CTH_corrected             = MissingXn
            SPixel%CTH_corrected_uncertainty = MissingSn
            SPixel%CTT_corrected             = MissingXn
            SPixel%CTT_corrected_uncertainty = MissingSn

            ! These are not filled as they are FM related products but
            ! they are actually output so we fill them here for lack of
            ! better place.
            RTM_Pc%Hc        = MissingXn
            RTM_Pc%dHc_dPc   = MissingSn
            RTM_Pc%Tc        = MissingXn
            RTM_Pc%dTc_dPc   = MissingSn
         end if

         ! Copy output to spixel_scan_out structures
         call prepare_output_primary(Ctrl, i, j, MSI_Data, SPixel, RTM_Pc, &
                                     Diag, output_data_1)

         call prepare_output_secondary(Ctrl, i, j, MSI_Data, SPixel, Diag, &
                                       output_data_2)

      end do ! End of super-pixel X loop

   end do ! End of super-pixel Y loop

   !$OMP END DO

   call Dealloc_SPixel(Ctrl, SPixel)
   if (Ctrl%RTMIntSelm /= RTMIntMethNone) then
      call Dealloc_RTM_Pc(Ctrl, RTM_Pc(1))
      if (Ctrl%Approach == AppCld2L) then
         call Dealloc_RTM_Pc(Ctrl, RTM_Pc(2))
      end if
   end if

   !$OMP END PARALLEL

#ifdef USE_TIMING
   m2=mclock()
   write(*,111) m2
   cpu_secs=(m2-m1)*0.01
   r2=rtc()
   write(*,110) r2
   real_secs=(r2-r1) ! * 0.001
   write(*,113) cpu_secs,real_secs
   cpu_secs=cpu_secs/real_secs
   write(*,115) cpu_secs
#endif

   ! Open the netcdf output files
   if (Ctrl%verbose) write(*,*) 'path1: ',trim(Ctrl%FID%L2_primary)
   if (Ctrl%Ind%flags%do_indexing) then
      ! Write channel indexing, LUT class, and Ctrl%Ind%flags to output

      output_data_1%y_id  = Ctrl%Ind%Y_Id
      output_data_1%ch_is = Ctrl%Ind%Ch_Is

      call make_bitmask_from_terms( &
           Ctrl%Ind%common_indices_t, output_data_1%rho_flags)

      call make_bitmask_from_common_file_flags(Ctrl%Ind%flags, bitmask)

      ! Form a list of all the views available in this file
      ! (forseeing the eventual desire to process arbitrary views)
      m = 0
      do i=1,MaxNumViews
         do j=1,Ctrl%Ind%Ny
            if (Ctrl%Ind%View_Id(j) == i) then
               m = m + 1
               output_data_1%view_id(m) = i
               exit
            end if
         end do
      end do

      call ncdf_create(Ctrl%FID%L2_primary, ncid_primary, Ctrl%Ind%Xdim, &
           Ctrl%Ind%Ydim, Ctrl%Ind%NViews, dims_var, 1, global_atts,  &
           source_atts, Ctrl%Ind%Ny, ch_var, Ctrl%Nx(IDay), &
           Ctrl%LUTClassLayers, Ctrl%use_ann_phase, bitmask)
   else
      call ncdf_create(Ctrl%FID%L2_primary, ncid_primary, Ctrl%Ind%Xdim, &
           Ctrl%Ind%Ydim, Ctrl%Ind%NViews, dims_var, 1, global_atts, source_atts)
   end if


   if (Ctrl%verbose) write(*,*) 'path2: ',trim(Ctrl%FID%L2_secondary)
   call ncdf_create(Ctrl%FID%L2_secondary, ncid_secondary, Ctrl%Ind%Xdim, &
        Ctrl%Ind%Ydim, Ctrl%Ind%NViews, dims_var, 2, global_atts, source_atts)

   ! Create NetCDF files and variables
   call build_flag_masks(Ctrl, output_data_1)
   call def_output_primary(ncid_primary, dims_var, output_data_1, &
        Ctrl%Ind%common_indices_t, deflate_level, shuffle_flag, &
        Ctrl%verbose, ch_var)
   call def_output_secondary(ncid_secondary, dims_var, output_data_2, &
        Ctrl%Ind%common_indices_t, deflate_level, shuffle_flag, Ctrl%verbose)

   ! Write output from spixel_scan_out structures NetCDF files
   call write_output_primary(ncid_primary, Ctrl%Ind%common_indices_t, &
        output_data_1)
   call write_output_secondary(ncid_secondary, Ctrl%Ind%common_indices_t, &
        output_data_2)

   TotPix    = sum(totpix_line)
   Totmissed = sum(totmissed_line)
   Totconv   = sum(totconv_line)
   aviter    = sum(aviter_line)
   avj       = sum(avj_line)
   totmaxj   = sum(totmaxj_line)

   if (Ctrl%verbose) then
      write(*,*)' Total super-pixels processed          ',TotPix
      write(*,*)' Total skipped due to 0 cloud or error ',TotMissed
      write(*,*)' No. of retrievals converged           ',TotConv
      if (TotConv > 0) then
         write(*,*)' Avge no. of iter per conv.            ',&
              float(AvIter)/float(TotConv)
         write(*,*)' Avge cost per conv                    ',&
              AvJ / float(TotConv)
         write(*,*)' No. of retrieval costs > max          ',TotMaxJ
      end if
   end if

   ! Deallocate some vectors for openMP
   deallocate(totpix_line)
   deallocate(totmissed_line)
   deallocate(totconv_line)
   deallocate(totmaxj_line)
   deallocate(aviter_line)
   deallocate(avj_line)


   !----------------------------------------------------------------------------
   ! Deallocate any allocatable arrays that have been set and close the output.
   !----------------------------------------------------------------------------

   ! SAD_LUT is an allocatable arrays of structs, each struct containing
   ! allocatable arrays. Hence call a routine to dealloc the internal arrays
   ! before deallocating the array of structs.

   deallocate(SAD_Chan)
   call Dealloc_SAD_LUT(Ctrl, SAD_LUT(1))
   if (Ctrl%Approach == AppCld2L) then
      call Dealloc_SAD_LUT(Ctrl, SAD_LUT(2))
   end if

   if (Ctrl%RTMIntSelm /= RTMIntMethNone) call Dealloc_RTM(Ctrl, RTM)

   call Dealloc_Data(Ctrl, MSI_Data)

   call dealloc_output_data_primary(output_data_1)
   call dealloc_output_data_secondary(output_data_2)

   call Dealloc_Ctrl(Ctrl)

   ! Close netcdf output files
   call ncdf_close(ncid_primary, 'orac(primary)')
   call ncdf_close(ncid_secondary, 'orac(secondary)')

   if (Ctrl%verbose) write(*,*) 'ORAC is ending successfully'

#ifdef USE_TIMING
   m3=mclock()
   write(*,111) m3
   cpu_secs=(m3-m2)*0.01
   r3=rtc()
   write(*,110) r3
   real_secs=(r3-r2) ! * 0.001
   write(*,114) cpu_secs,real_secs
   cpu_secs=cpu_secs/real_secs
   write(*,115) cpu_secs
#endif

#ifdef WRAPPER
end subroutine orac
#else
end program orac
#endif
