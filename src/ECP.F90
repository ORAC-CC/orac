!-------------------------------------------------------------------------------
! Name:
!   ECP
!
! Description:
!   Main program for the Enhanced Cloud Processor prototype. Calls subordinate
!   functions to read in data and process.
!
!   License/Copyright
!   Copyright 2011, RAL Space, Science and Technology Facilities Council and
!   University of Oxford.
!
!   This file and the associated documentation and source code files are part of
!   ORAC.
!
!   ORAC is free software: you can redistribute it and/or modify it under the
!   terms of the GNU General Public License as published by the Free Software
!   Foundation, either version 3 of the License, or (at your option) any later
!   version.
!
!   ORAC is distributed in the hope that it will be useful, but WITHOUT ANY
!   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
!   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
!   details.
!
!   You should have received a copy of the GNU General Public License along with
!   ORAC. If not, see http://www.gnu.org/licenses/
!
! Arguments:
!    Name Type In/Out/Both Description
!    N/A
!
! Algorithm:
!    Data Preparation Section: reads in driver file and all SAD files but not
!    the image data (which is read in segments)
!    - read driver file
!    - open log file
!    - open breakpoint file if required
!    - open output and diagnostic files
!    - allocate SAD arrays and read in SAD files
!    - read RTM data files
!    - allocate super-pixel structure SPixel, and the RTM_Pc structure to hold
!      RTM data interpolated to a given pressure level Pc.
!
!    Product generation section:
!    - initialise super-pixel values (e.g. phase used for first guess state
!      vector setting, saved state and error XnSav and SnSav used for first
!      guess setting if method is SDAD).
!    - modify user selected image area so that coordinates fall on exact
!      super-pixel boundaries (and therefore exact image segment boundaries)
!      and write log message if values are changed
!    - read in image segments up to the user's selected starting point
!    - main loop: for each row of super-pixels from user's starting y to end y:
!      - convert the y location within the image to y loc within the segment
!        (SPixel%Loc%Yseg0)
!      - if (row number corresponds to a new segment)
!        - read in the next segment from the MSI, cloud flags files etc
!      - if (first row in the image)
!        - write control structure to the output and diagnostic files
!      - for each x location within the row (xstart to xstop in steps of the
!        SPixel size)
!        - get the current super-pixel values (measurements, cloud flags, geom
!          etc)
!        - check SPixel quality control flag: if flag indicates the SPixel
!          should not be processed:
!          - set output state vector and error arrays to indicate missing data
!        - else (process the SPixel)
!          - call Invert_Marquardt to calculate the current state vector
!          - update overall statistics totals
!          - write out the retrieved state vector and errors, plus diagnostics
!      - end of x loop
!    - end of y loop
!    - write out overall statistics
!    - close files
!
! Local variables:
!    Name Type Description
!    Do we really need a list here that is the same as the "Local variable
!    declarations" section below, descriptions and all, and is subject to rot
!    unlike the declaration section below.
!
! History:
!     2nd Aug 2000, Andy Smith: Original version (in development)
!    11th July 2001, Andy Smith:
!       Preparing main program for integration with other ECP routines.
!    15th Aug 2001, Andy Smith:
!       First fully working version of ECP. Includes image segmentation.
!    15th Aug 2001, Andy Smith:
!       Added status checks at start of main loop and after ReadSatData calls.
!    23rd Aug 2001, Andy Smith:
!       Added status check around overall statistics output.
!    21st Sep 2001, Andy Smith:
!       Added initial allocation of SPixel%Ym and Sy. These are deallocated
!       each time Get_SPixel is called (in Get_Measurements). Also took the
!       opportunity to put an "if" before part of the final stats output to
!       avoid divide by zero errors, and replace tabs put in by the Nedit
!       auto-indent feature with spaces.
!    24th Sep 2001, Andy Smith:
!       Moved initial allocation of SPixel%Ym, Sy, X and XI to AllocSPixel.
!       Makes the main program more readable and avoids re-coding test
!       harnesses.
!    22nd Oct 2001, Andy Smith:
!       Added calls to deallocate routines for SPixel, RTM_Pc, RTM and the
!       MSI_Data structure. The alloc arrays in these structures *should* be
!       released automatically at the end of execution. These routines
!       deallocate explicitly in case the automatic dealloc isn't done.
!    24th Oct 2001, Andy Smith:
!       Removed conversion of solar channel reflectances from % to fraction.
!       MSI files should now be written with fractional values.
!       New logical variable to track alloc state of SAD_LUT internal arrays.
!    **************** ECV work starts here *************************************
!     9th Mar 2011, Andy Smith:
!       Re-applying changes from late 2001/2, MSI_luns now dimension 6 to allow
!       for albedo data.
!    22nd Mar 2011, Andy Smith:
!       Removal of phase change. Only 1 cloud class required for each retrieval
!       run. SADCloudClass and SAD_LUT array dimensions changed.
!    23rd Mar 2011, Andy Smith:
!       Added output of latitude and longitude to output file.
!    30th Mar 2011, Andy Smith:
!       Removal of super-pixel averaging. All super-pixelling will now be done
!       in pre-processing. The SPixel structure used here now refers to a single
!       pixel from the input files.
!       Removed the modification of image (x,y) processing ranges to whole number
!       of super-pixels. Remove use of Ctrl%Resoln%Space.
!     8th Apr 2011, Andy Smith:
!       Simplification of selection methods for first guess, a priori, limits etc.
!       InvertMarquardt no longer requires SAD_CloudClass argument.
!       Removed setting of SPixel%Phase: redundant following removal of phase
!       change functionality.
!    11th May 2011, Andy Smith:
!       Extension to multiple viewing angles. Elements of the Ctrl struct are
!       now pointers to arrays rather than fixed-size arrays, so Ctrl cannot be
!       written in a single operation. Removed the writes of Ctrl to the diag
!       and out file. It is planned to remove Ctrl from the out file anyway.
!     8th Jun 2011, Caroline Poulsen:
!       removed diagnostic structure
!       added extra output file residual, quality indicators, apriori and first
!       guess also input structure MSI_luns now dimension 7 to allow for
!       scanline data.
!     8th Jun 2011, Andy Smith:
!       Tidied up log output. Use ORAC not ECP in log file. Trim run ID string.
!       Added Write_Log call at end, to get finish time.
!    22nd Sep 2011, Caroline Poulsen: remove sw%p as now the same as lw%p
!     8th Oct 2011, Caroline Poulsen: added CWP to output
!     8th Nov 2011, Caroline Poulsen: added y0 and sx to output
!     8th Dec 2011, Matthias Jerg: added code to accommodate netcdfoutput
!    13th Dec 2011, caroline poulsen: remove relenlog to be compatible with f95
!    19th Dec 2011, Matthias Jerg: cleaned up netcdf output, introduced error
!       reporting and added file headers.
!     5th Jan 2012, Caroline poulsen: removed binary output files
!    15th Jan 2012, Caroline poulsen: added missing for ymfit
!    2012/03/27, MJ: changes netcdf write to 2D arrays
!    2012/07/13, MJ: implements option to read drifile path from command line
!    2012/08/14, MJ: irons out bug that made ORAC crash with gfortran
!    2012/08/14, MJ: changes scaling of CWP output
!    2012/08/22, MJ: includes time in MSI structure and writes it to primary
!       netcdf file
!    2012/08/22, MJ: makes adaptions to read netcdf files, start and end indices
!       of area to be processed determined by preprocessing file contents and
!       not hardwired any more.
!    2012/08/27, MJ: better implements time variable in output.
!    2012/11/01, MJ: implements openMP parallelization of along-track loop.
!    2013/01/17, Matthias Jerg: Adds code to accommodate uncertainties of ctt
!       and cth
!    2013/12/05, MJ: initializes Diag%AK=real_fill_value
!    2013/12/10, MJ: initializes ymfit and y0 with missingxn
!    2014/01/12, Greg McGarragh: Added some missing deallocates.
!    2014/01/28, Greg McGarragh: Cleaned up code.
!    2014/01/29, Greg McGarragh: Some OpenMP fixes. Ctrl is actually shared.
!       No need to make it private. Also many variables set to be 'privatefirst'
!       should just be 'private', i.e. they do not need to enter the parallel
!       loop initialised. Finally status_line was not needed. Status is private
!       within the loop.
!    2014/02/10, Matthias Jerg: Put the correct boundaries lat/lon for adaptive
!       processing back in.
!    2014/06/04, Matthias Jerg: Introduced "WRAPPER" for c-preprocessor and
!       associated variables.
!    2014/06/12, Greg McGarragh: OpenMP functions should be declared by the
!       omp_lib module, not explicitly.
!    2014/06/13, Greg McGarragh: Put NetCDF output related includes into
!       subroutines.
!    2014/06/15, Greg McGarragh: Set CTH and CTT values to missing in the case
!       when a retrieval is not possible.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

#ifndef WRAPPER
program ECP
#else
subroutine ECP(mytask,ntasks,lower_bound,upper_bound,drifile)
#endif

   ! Modules used by this program.

   use config_def
   use CTRL_def
   use Data_def
   use Diag_def
   use ECP_Constants
   use omp_lib
   use output_routines
   use Read_SAD_def
   use RTM_def
   use RTM_Pc_def
   use SAD_Chan_def
   use SAD_LUT_def
   use SPixel_def

   use netcdf

   ! Local variable declarations

   implicit none

   type(config_struct) :: conf
   type(CTRL_t)        :: Ctrl
   type(Data_t)        :: MSI_Data
   type(Diag_t)        :: Diag       ! Diagnostic struct returned by Invert_Marquardt
   type(RTM_t)         :: RTM
   type(RTM_Pc_t)      :: RTM_Pc
   type(SAD_Chan_t), allocatable, dimension(:) :: SAD_Chan
   type(SAD_LUT_t)     :: SAD_LUT
   type(SPixel_t)      :: SPixel

   integer             :: i, j, jj, m
   integer             :: ios        ! I/O status value from file operations
   integer             :: status = 0 ! Status value returned from subroutines
   integer             :: nargs
   character(len=FilenameLen) &
                       :: drifile
   character(180)      :: message    ! Error message string returned by Read_Driver
   integer             :: log_lun    ! Logical Unit Number for log file
   integer             :: diag_lun   ! Logical unit number for diagnostics file

   character(8)        :: date       ! date returned from Date And Time function
   character(10)       :: time       ! time returned from Date And Time function
   character(24)       :: time_str   ! string to hold date + time

   integer             :: ixstart,ixstop,xstep
                                     ! First and last super-pixel X locations
   integer             :: iystart,iystop,ystep
                                     ! First and last super-pixel Y locations

   integer             :: conv       ! convergence flag

   integer             :: NSegs      ! Number of image segments processed
   integer             :: SegSize    ! Number of rows of pixels in image segment.

   integer             :: TotPix   = 0   ! Total number of SPixels processed
   integer             :: TotMissed= 0   ! Number of SPixels left unprocessed
   integer             :: TotConv  = 0   ! Number of successful inversions
   integer             :: TotMaxJ  = 0   ! Number of inversions with cost > MaxQC
   integer             :: AvIter   = 0   ! Average no. of iterations per successful
                                         ! retrieval
   integer             :: AvPhCh   = 0   ! Average no. of phase changes per successful
                                         ! retrieval
   real                :: AvJ      = 0.0 ! Average cost per successful retrieval

   logical             :: RTM_Pc_alloc  = .false. ! Indicates Alloc_RTM_Pc called
                                                  ! and ran successfully.
   logical             :: RTM_alloc     = .false. ! Indicates Read_RTM called and ran
                                                  ! successfully, arrays are allocated.
   logical             :: SAD_LUT_alloc = .false. ! Indicates Read_LUT called and
                                                  ! ran successfully, arrays are allocated.
   logical             :: SPixel_alloc  = .false. ! Indicates that Alloc_SPixel
                                                  ! has been called and ran successfully.

   ! netcdf related variables:
   integer :: ncid_primary,ncid_secondary,dims_var(2), wo = 0

   ! Write full covariance matrix to secondary output file, so far hardwired
   logical :: lcovar = .FALSE.

   ! Additional types for the scanline output for netcdf are defined
   type(spixel_scanline_input)            :: spixel_scan_in
   type(spixel_scanline_primary_output)   :: spixel_scan_out
   type(spixel_scanline_secondary_output) :: spixel_scan_out_sec

   ! Some netcdf related indices and labels
   integer :: ierr
   integer :: iviews,iinput

   ! Variables to avoid out of bounds contents
   real(kind=sreal) :: dummyreal

   ! OpenMP related variables
   integer :: nthreads,thread_id

   ! Some more variables for OpenMP implementation
   integer, allocatable, dimension(:) :: totpix_line,totmissed_line,totconv_line, &
                                         totmaxj_line
   integer, allocatable, dimension(:) :: aviter_line,avphch_line
   real,    allocatable, dimension(:) :: avj_line

#ifdef USE_TIMING
   ! This is for timing the different parts of the code on AIX IBM PWR7 at ECMWF
   integer(kind=nint) :: m0,m1,m2,m3,mclock
   real(kind=dreal)   :: r0,r1,r2,r3,rtc
   real(kind=dreal)   :: cpu_secs,real_secs
#endif
#ifdef BKP
   integer            :: bkp_lun ! Unit number for breakpoint file
#endif
#ifdef USE_ADAPTIVE_PROCESSING
   logical            :: lhres
   real(kind=sreal)   :: range_lat(2),range_lon(2)
#endif

   ! This is for the wrapper
#ifdef WRAPPER
   integer :: mytask,ntasks,lower_bound,upper_bound
#endif
   !include "sigtrap.F90"

#ifdef USE_TIMING
   ! Initialize timing
   cpu_secs=0_dreal
   real_secs=0_dreal
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

   !----------------------------------------------------------------------------
   ! Product generation section
   !----------------------------------------------------------------------------

   ! Look if path to driver file was given on the command line. If yes, then
   ! read it, if not leave it to the Read_Driver routine to deal with it


#ifndef WRAPPER
   nargs=command_argument_count()
#else
   nargs=-1
#endif

   write(*,*) 'inside preproc',nargs

   if (nargs .eq. 1 ) then
      drifile=''
      call get_command_argument(1,drifile)
   endif

   ! Read Ctrl struct from driver file
   call Read_Driver(Ctrl, conf,message,nargs, drifile,status)

   ! Read dimensions of preprocessing swath files first:
   call read_input_dimensions_msi(Ctrl%Fid%MSI,Ctrl%FID%Geo, &
      Ctrl%Ind%Xmax,Ctrl%Ind%YMax,Ctrl%Ind%Nyp,Ctrl%Ind%NInstViews,0)


   ! Now set the corners of the domain based on what's in the input files
   Ctrl%Ind%X0 = 1
   Ctrl%Ind%Y0 = 1
   Ctrl%Ind%X1 = Ctrl%Ind%Xmax
   Ctrl%Ind%Y1 = Ctrl%Ind%YMax

   Ctrl%Ind%Xstart = 1
   Ctrl%Ind%Ystart = 1
   Ctrl%Resoln%SegSize = Ctrl%Ind%YMax


   ! Open the log file specified in Ctrl
   call find_lun(log_lun)
   open(unit=log_lun, file=Ctrl%FID%Log, status='replace', iostat=ios)
   write(*,*) log_lun,ios,trim(adjustl(Ctrl%FID%Log))

   if (ios == 0) then
      call Date_and_Time (date=date, time=time)
      time_str = date // ' ' // time(1:2) // ':' // time(3:4) // ':' // time(5:6)
      write(log_lun, *)' ORAC '
      write(log_lun, *)' Start time: ', time_str
!     write(log_lun, *)' Run ID: ',Ctrl%Run_ID
   else
      write(*,*)' Error opening log file'
      write(*,*) Ctrl%FID%Log
      stop
   end if
   close(unit=log_lun)

   ! Handle any error status returned by Read_Driver
   ! (special case - ReadDriver can't report it's own errors because it has to
   ! read the log file name from the driver file before the log file can be used).
   if (status /= 0) then
      call Write_Log(Ctrl, message, status)
   end if

#ifdef BKP
   ! Clear the breakpoint file (if breakpoints required)
   if (status == 0 .and. Ctrl%Bkpl > 0) then
      call find_lun(bkp_lun)
      open(unit=bkp_lun, file=Ctrl%FID%Bkp, status='replace', iostat=ios)

      if (ios == 0) then
         write(bkp_lun, *)' ORAC breakpoint output'
         write(bkp_lun, *)' Start time: ', time_str
!        write(bkp_lun, *)' Run ID: ',Ctrl%Run_ID
         write(bkp_lun, *)
      else
         status = BkpFileOpenErr
         call Write_Log(Ctrl, 'Main: Error opening breakpoint file', status)
      end if
      close(unit=bkp_lun)
   end if
#endif

   ! Open the output and diagnostic files
   call find_lun(diag_lun)
   open(unit=diag_lun, file=Ctrl%FID%Diag, form='Unformatted', &
        status='replace', iostat=ios, err=999)
   if (ios /= 0) then
      write(*,*)' Error opening log file'
      write(*,*) Ctrl%FID%Log
      stop
   end if


   ! Set the size of the SAD_Chan and Cloud Class arrays based on the Ctrl
   ! parameters and read the SAD values.
   if (status == 0) then
      allocate(SAD_Chan(Ctrl%Ind%Ny))

      call Read_SAD(Ctrl, SAD_Chan, SAD_LUT, status)
      if (status == 0) SAD_LUT_Alloc = .true.
      write(*,*) 'Reading SAF files done (status)',status
   end if


   ! Make read in rttov data in one go, no more segment reads
   if (status == 0) then
      call read_input_dimensions_lwrtm(Ctrl,Ctrl%Fid%LWRTM,&
         RTM%LW%Grid%NLatLon,RTM%LW%Grid%NLon, RTM%LW%Grid%NLat,&
         RTM%LW%NP,RTM%LW%NPLAY,&
         RTM%LW%NLWF,RTM%LW%NV,0)

      call read_input_dimensions_swrtm(Ctrl%Fid%SWRTM,&
         RTM%SW%Grid%NLatLon,RTM%SW%Grid%NLon, RTM%SW%Grid%NLat,&
         RTM%SW%NP,RTM%SW%NPLAY,&
         RTM%SW%NSWF,RTM%SW%NV,0)

      RTM%LW%NP=RTM%LW%NPLAY
      RTM%SW%NP=RTM%SW%NPLAY

      call Read_RTMData_nc(Ctrl, RTM, status)
      if (status == 0) then
         RTM_Alloc = .true.
      endif
   end if


   !----------------------------------------------------------------------------
   ! Product generation section
   !----------------------------------------------------------------------------

   if (status == 0) then

      ! Loop over required super pixel X0,Y0 coordinates
      !
      ! Determine start and stop values for the loop counters depending on whether
      ! a "warm start" is required. Modify start and stop x values so that only
      ! whole super-pixels are processed. A SPixel of size 3 with a left-hand
      ! corner at 511 will go outside of an image of size 512. Also, if the
      ! starting pixel is not a whole number of Super-Pixels from the first line
      ! in the image data, some SPixels in the selected area will cross boundaries
      ! between image segments.

      if (Ctrl%Ind%Ws == 0) then
         ixstart = Ctrl%Ind%X0
         iystart = Ctrl%Ind%Y0
      else
         ! Warm start. Use Xstart, YStart
         ixstart = Ctrl%Ind%Xstart
         iystart = Ctrl%Ind%Ystart
      end if

      ! Set the stop values
      ixstop = Ctrl%Ind%X1
      iystop = Ctrl%Ind%Y1

      write(*,*) 'Start line: ', iystart
      write(*,*) 'Stop line: ', iystop
      write(*,*) 'Total number of lines: ', (iystop - iystart) + 1


      SegSize = Ctrl%Resoln%SegSize
      select case (mod(iystart,SegSize))
      ! 0 = last row of segment; 1 = 1st row of segment
      case (0, 1)
         NSegs = iystart / SegSize
      ! Mid-segment
      case default
         NSegs = (iystart / SegSize) + 1
      end select


      ! Read all the swath data
      call Read_SatData_nc(Ctrl, NSegs, SegSize, MSI_Data, SAD_Chan,status)


      xstep = 1
      ystep = 1
#ifdef USE_ADAPTIVE_PROCESSING
      ! Adaptive processing:
      lhres = .false. ! "high" resolution flag
      if (index(trim(adjustl(Ctrl%Inst%Name)),'MODIS') .ge. 1) then

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
         endif

         write(*,*) 'Adaptive processing: ',lhres,xstep,ystep
      else
         lhres = .true.

         xstep = 1
         ystep = 1

         write(*,*) 'Adaptive processing: ',lhres,xstep,ystep
      endif
#endif

      ! Open the netcdf output files
      if (status == 0) then
         write(*,*) 'path1: ',trim(adjustl(Ctrl%FID%L2_primary_outputpath_and_file))
         call nc_create_global_l2(Ctrl,adjustl(Ctrl%FID%L2_primary_outputpath_and_file),&
            ncid_primary, ixstop-ixstart+1, iystop-iystart+1, dims_var, wo,1,status)
         write(*,*) 'path2: ',trim(adjustl(Ctrl%FID%L2_secondary_outputpath_and_file))
         call nc_create_global_l2(Ctrl,adjustl(Ctrl%FID%L2_secondary_outputpath_and_file),&
           ncid_secondary, ixstop-ixstart+1, iystop-iystart+1, dims_var, wo,2, status)

         ! Allocate output arrays
         call alloc_spixel_scan_out(ixstart,ixstop,iystart,iystop,Ctrl%Ind%NViews, &
                                    spixel_scan_out)
         call alloc_spixel_scan_out_sec(ixstart,ixstop,iystart,iystop,Ctrl%Ind%Ny, &
                                        MaxStateVar,lcovar,spixel_scan_out_sec)

         ! Create NetCDF files and variables
         call def_vars_primary(Ctrl, ncid_primary, dims_var, spixel_scan_out, &
                               status)
         call def_vars_secondary(Ctrl, conf, lcovar, ncid_secondary, dims_var, &
                                 spixel_scan_in, spixel_scan_out_sec, status)
      endif


      ! Set i, the counter for the image x dimension, for the first row processed.
      i = ixstart


      ! This is to make things easier for OpenMP
      allocate(totpix_line(iystart:iystop))
      totpix_line=0
      allocate(totmissed_line(iystart:iystop))
      totmissed_line=0
      allocate(totconv_line(iystart:iystop))
      totconv_line=0
      allocate(totmaxj_line(iystart:iystop))
      totmaxj_line=0
      allocate(aviter_line(iystart:iystop))
      aviter_line=0
      allocate(avphch_line(iystart:iystop))
      avphch_line=0
      allocate(avj_line(iystart:iystop))
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

      ! Along track loop is parallelized with openMP
      nthreads = omp_get_max_threads()
      write(*,*) 'ORAC along-track loop now running on', nthreads, 'threads'

      ! Start OMP section by spawning the threads
      !$OMP PARALLEL &
      !$OMP PRIVATE(i,j,jj,m,iviews,iinput,thread_id,RTM_Pc,SPixel,SPixel_Alloc,RTM_Pc_Alloc,Diag,conv,dummyreal) &
      !$OMP FIRSTPRIVATE(status)
      thread_id = omp_get_thread_num()
      print *, 'Thread ', thread_id+1, 'is active'


      !  Allocate sizes of SPixel sub-structure arrays
      call Alloc_RTM_Pc(Ctrl, RTM_Pc, status)
      if (status == 0) RTM_Pc_Alloc = .true.

      call Alloc_SPixel(Ctrl, RTM, SPixel, status)
      if (status == 0) SPixel_Alloc = .true.


      ! Set RTM pressure values in SPixel (will not change from here on)
      SPixel%RTM%LW%Np = RTM%LW%Np
      SPixel%RTM%SW%NP = RTM%SW%Np


      ! Initialise values required before main loop begins, e.g. first guess
      ! phase which may be required for SDAD first guess/a priori setting in the
      ! first SPixel when no retrieved data is available.
      SPixel%XnSav = Ctrl%Xb
      SPixel%SnSav = 0
      do m=1,MaxStateVar
         SPixel%SnSav(m,m) = Ctrl%Sx(m) ** 2
      end do
      SPixel%Loc%LastX0 = 1
      SPixel%Loc%LastX0 = 1


      ! Set the counter for the image y dimension to the first row to process
      i = ixstart

      ! Start OMP parallel loop for along track direction.
      !$OMP DO SCHEDULE(GUIDED)
      do j = iystart,iystop,ystep

!        write(*,*) 'thread,iystart,iystop,iy: ', thread_id,iystart,iystop,j

         ! Set the location of the pixel within the image (Y0) and within the
         ! current image segment (YSeg0).
         SPixel%Loc%Y0 = j

         if (mod(SPixel%Loc%Y0, SegSize) == 0) then
            SPixel%Loc%YSeg0 = SegSize
         else
            SPixel%Loc%YSeg0 = SPixel%Loc%Y0 - &
               ((SPixel%Loc%Y0/SegSize) * SegSize)
         end if


         ! The X loop is unbounded. This allows for the changing X limits required
         ! on warm start: for the first row of SPixels processed the X range is
         ! Ctrl%Ind%Xstart to ixstop; for the remainder it is X0 to ixstop (in
         ! both cases the limits used are modified to whole numbers of SPixels).
         do
            !write(*,*) '(i,j)',i,j

            SPixel%Loc%X0 = i

            Diag%YmFit = MissingXn
            Diag%Y0    = MissingXn
            Diag%AK    = real_fill_value

!           TotPix = TotPix+1
            TotPix_line(j) = TotPix_line(j)+1

            ! Set up the super-pixel data values.
            if (status == 0) then
               call Get_SPixel(Ctrl, conf, SAD_Chan, MSI_Data, RTM, SPixel, status)
            endif

            if (status == 0) then

               ! If the super-pixel cannot be processed, zero the outputs and
               ! diag struct.

               if (btest(SPixel%QC, SPixNoProc)) then
!                 TotMissed = TotMissed+1
                  Totmissed_line(j) = Totmissed_line(j)+1

                  SPixel%Xn = MissingXn
                  SPixel%Sn = MissingSn

                  ! These are not filled as they are FM related products but
                  ! they are actually output so we fill them here for lack of
                  ! better place.
                  RTM_Pc%Hc      = MissingXn
                  RTM_Pc%dHc_dPc = MissingXn
                  RTM_Pc%Tc      = MissingXn
                  RTM_Pc%dTc_dPc = MissingXn

                  call Zero_Diag(Ctrl, Diag, status)
               else
                  Diag%AK = 0

                  ! No indication that the SPixel should not be processed, do the
                  ! inversion.
                  Call Invert_Marquardt(Ctrl, SPixel, SAD_Chan, SAD_LUT, &
                                        RTM_Pc, Diag, status)

                  ! Set values required for overall statistics 1st bit test on QC
                  ! flag determines whether convergence occurred.
                  if (status == 0) then
                     if (.not. btest(Diag%QCFlag,MaxStateVar+1)) then

!                       TotConv = TotConv+1
                        TotConv_line(j) = TotConv_line(j)+1
!                       AvIter  = AvIter + Diag%Iterations
                        AvIter_line(j)  = AvIter_line(j) + Diag%Iterations
                        if (Diag%PhaseChanges >= 0) then
!                          AvPhCh = AvPhCh + Diag%PhaseChanges
                           AvPhCh_line(j) = AvPhCh_line(j) + Diag%PhaseChanges
                        else
!                          AvPhCh = AvPhCh + Ctrl%InvPar%MaxPhase
                           AvPhCh_line(j) = AvPhCh_line(j) + Ctrl%InvPar%MaxPhase
                        end if
!                       AvJ = AvJ + Diag%Jm + Diag%Ja
                        AvJ_line(j) = AvJ_line(j) + Diag%Jm + Diag%Ja
                     end if
                     if (btest(Diag%QCFlag,MaxStateVar+2)) then
!                       TotMaxJ = TotMaxJ+1
                        TotMaxJ_line(j) = TotMaxJ_line(j)+1
                     end if
                  else
                     Diag%YmFit= MissingXn
                     SPixel%Xn = MissingXn
                     SPixel%Sn = MissingSn
                     call Zero_Diag(Ctrl, Diag, status)
                  end if
               end if ! btest if closes

               ! Calculate the Cloud water path CWP
               call Calc_CWP(Ctrl, SPixel, status)

               ! Write the outputs

               conv=1
               if (.not. btest(Diag%QCFlag,MaxStateVar+1)) then
                  conv=0
               end if

               ! Copy output to spixel_scan_out structures
               call prepare_primary(Ctrl, conv, i, j, MSI_Data, RTM_Pc, SPixel, &
                                    Diag, spixel_scan_out, status)
               call prepare_secondary(Ctrl, lcovar, i, j, MSI_Data, SPixel, Diag, &
                                      spixel_scan_out, spixel_scan_out_sec, status)

            end if ! End of status check after Get_SPixel

            i = i + xstep

            if (i > ixstop) exit

         end do ! End of super-pixel X loop

         i = Ctrl%Ind%X0
      end do    ! End of super-pixel Y loop

      !$OMP END DO

      if (SPixel_alloc) call Dealloc_SPixel(Ctrl, SPixel, status)

      if (RTM_Pc_alloc) call Dealloc_RTM_Pc(Ctrl, RTM_Pc, status)

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

      status = 0

      if (status == 0) then
         ! Write output from spixel_scan_out structures NetCDF files
         call write_primary(Ctrl, ncid_primary, ixstart, ixstop, iystart, iystop, &
                            spixel_scan_out, status)
         call write_secondary(Ctrl, lcovar, SPixel, ncid_secondary, ixstart, ixstop, &
                              iystart, iystop, spixel_scan_out_sec, status)
      endif


      if (status == 0) then
         TotPix    = sum(totpix_line)
         Totmissed = sum(totmissed_line)
         Totconv   = sum(totconv_line)
         aviter    = sum(aviter_line)
         avphch    = sum(avphch_line)
         avj       = sum(avj_line)
         totmaxj   = sum(totmaxj_line)

         write(*,*)' Total super-pixels processed          ',TotPix
         write(*,*)' Total skipped due to 0 cloud or error ',TotMissed
         write(*,*)' No. of retrievals converged           ',TotConv
         if (TotConv > 0) then
            write(*,*)' Avge no. of iter per conv.            ',&
               float(AvIter)/float(TotConv)
            write(*,*)' Avge no. of phase ch per conv.        ',&
               float(AvPhCh)/float(TotConv)
            write(*,*)' Avge cost per conv                    ',&
               AvJ / float(TotConv)
            write(*,*)' No. of retrieval costs > max          ',TotMaxJ
         end if
      end if

   end if  ! End of status check at start of product generation section


   ! Deallocate some vectors for openMP
   deallocate(totpix_line)
   deallocate(totmissed_line)
   deallocate(totconv_line)
   deallocate(totmaxj_line)
   deallocate(aviter_line)
   deallocate(avphch_line)
   deallocate(avj_line)


   !----------------------------------------------------------------------------
   ! Deallocate any allocatable arrays that have been set, close the output and
   ! diagnostics files.
   !----------------------------------------------------------------------------

   ! SAD_LUT is an allocatable arrays of structs, each struct containing
   ! allocatable arrays. Hence call a routine to dealloc the internal arrays
   ! before deallocating the array of structs.

   if (allocated(SAD_Chan)) deallocate(SAD_Chan)
!  if (allocated(SAD_CloudClass)) deallocate(SAD_CloudClass)
   if (SAD_LUT_Alloc) call Dealloc_SAD_LUT(Ctrl, SAD_LUT, status)
   if (RTM_alloc)     call Dealloc_RTM(Ctrl, RTM, status)

   call Dealloc_Data(Ctrl, MSI_Data, status)

   if (status == 0) then
      call dealloc_spixel_scan_out(spixel_scan_out)
      call dealloc_spixel_scan_out_sec(spixel_scan_out_sec,lcovar)
   endif

   call Dealloc_Ctrl(Ctrl, status)

   deallocate(conf%channel_ids_instr)
   deallocate(conf%channel_ids_abs)
   deallocate(conf%channel_sw_flag)
   deallocate(conf%channel_lw_flag)
   deallocate(conf%channel_proc_flag)
   deallocate(conf%channel_sw_flag_use)
   deallocate(conf%channel_lw_flag_use)
   deallocate(conf%channel_mixed_flag_use)


   close(unit=diag_lun)

   ! Close netcdf output files
   if (status == 0) then
      call nc_close(ncid_primary, &
                    trim(adjustl(Ctrl%FID%L2_primary_outputpath_and_file)),wo,ierr)

      if (ierr .ne. 0) then
         status=PrimaryFileCloseErr

         write(*,*) 'nc_close.F90: netcdf primary file close error:', status
         call Write_Log(Ctrl,'nc_close.F90: netcdf primary file close error:', status)
         stop
      endif

      call nc_close(ncid_secondary, &
                    trim(adjustl(Ctrl%FID%L2_secondary_outputpath_and_file)),wo,ierr)

      if (ierr .ne. 0) then
         status=SecondaryFileCloseErr

         write(*,*) 'nc_close.F90: netcdf secondary file close error:', status
         call Write_Log(Ctrl,'nc_close.F90: netcdf secondary file close error:', status)
         stop
      endif
   endif

999 if (ios /= 0) then
       status = OutFileOpenErr
       call Write_Log(Ctrl,'Error opening output or diagnostic file',status)
   end if


   call Write_Log(Ctrl, 'ORAC ending', status)

   write(*,*)'Ending with status ',status

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
end subroutine ECP
#else
end program ECP
#endif
