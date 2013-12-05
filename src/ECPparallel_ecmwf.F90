! Name:
!   ECP
!
! Description:
!   Main program for the Enhanced Cloud Processor prototype. Calls subordinate 
!   functions to read in data and process.
!
!   License/Copyright
!   Copyright 2011, RAL Space, Science and Technology Facilities Council and University
!   of Oxford. 
!
!   This file and the associated documentation and source code files are part of ORAC.
!
!   ORAC is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   ORAC is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with ORAC. If not, see http://www.gnu.org/licenses/
!
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Algorithm:
!    Data Preparation Section: reads in driver file and all SAD files but not
!    the image data (which is read in segments)
!    - Read driver file
!    - open log file
!    - open breakpoint file if required
!    - open output and diagnostic files 
!    - allocate SAD arrays and read in SAD files
!    - read RTM data files
!    - allocate super-pixel structure SPixel, and the RTM_Pc structure to 
!        hold RTM data interpolated to a given pressure level Pc.
!
!    Product generation section
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
!           - set output state vector and error arrays to indicate missing data
!        - else (process the SPixel)
!           - call Invert_Marquardt to calculate the current state vector
!           - update overall statistics totals 
!           - write out the retrieved state vector and errors, plus diagnostics 
!      - end of x loop
!    - end of y loop
!    - write out overall statistics  
!    - close files    
!
! Local variables:
!    Name       Type    Description
!    Ctrl       struct  ECP control structure read from driver file
!    SAD_CloudClass     Array of structures each containing info on a cloud
!                       class (means and limit on state vectors etc)
!    SAD_Chan   array of structs  Contains info on each instrument channel.
!    SAD_LUT    array of structs  Cloud radiative properties for each class in
!                       SAD_CloudClass, stored as look-up tables.
!    RTM        struct  Radiative Transfer Model data.
!    RTM_Pc     struct  RTM data interpolated to a given cloud pressure, Pc.
!    Spixel     struct  Data for the current super-pixel.
!    MSI_Data   struct  Multi-spectral image data, cloud flags, land sea flags
!                       and all other data associated with the image.
!    Diag       struct  Diagnostic information produced by the inversion
!                       process.
!    message    string  Used for error messages to be written to the log file.
!    status     int     Error/status value returned by subordinate routines.
!    ios        int     Fortran I/O status value from file operations.
!    log_lun    int     Logical Unit Number for log file
!    bkp_lun    int     Logical Unit Number for breakpoint file
!    out_lun    int     Logical unit number for output file
!    diag_lun   int     Logical unit number for diagnostics file
!    MSI_luns   int array  Logical unit numbers for multi-spectral
!                       image files (measurements, cloud flags etc)
!    MSI_Files_open logical   Flag indiacting whether the MSI files have been
!                       opened (files are opened on first call to ReadSatData
!                       and left open for reading of image segments).
!    date       string  Date returned by intrinsic function, for logging.
!    time       string  Time returned by intrinsic function, for logging.
!    time_str   string  Combined date and time.
!    ixstart    int     "Lower left" x co-ordinate for image area to be
!                       processed: either user's X0 or Xstart depending whether
!                       warm start is selected, modified to whole no. of 
!                       SPixels from start of image.
!    ixstop     int     "Upper right" image x, modified to whole SPixels.
!    iystart    int     "Lower left" image y: from Y0 or Ystart depending on 
!                       warm start option, modified to whole SPixels.
!    iystop     int     "Upper right" y value, modified to whole SPixels.
!    i, j, m    int     Loop counters
!    SegSize    int     Size of image segment in rows of pixels (converted from
!                       Ctrl value which is rows of SPixels).
!    NSegs      int     Counter for number of image segments read in, or, at
!                       start of image, number of segments to read past to get
!                       to the required starting row for processing.
!    TotPix     int     Total number of super-pixels processed.
!    TotMissed  int     Total number of super-pixels omitted from processing
!                       due to zero cloud etc.
!    TotConv    int     Total number of super-pixels where processing reached
!                       convergence.
!    TotMaxJ    int     Total number of super-pixels where convergence resulted
!                       in a cost greater than the quality control max value.
!    AvIter     int     Average number of iterations per successful convergence.
!    AvPhCh     int     Average number of phase changes per successful 
!                       convergence.
!    AvJ        int     Average cost per successful convergence.
!    SPixel_alloc       Indicates that Alloc_SPixel has been called and ran
!               logical successfully, i.e. the allocatable arrays in SPixel
!                       have been allocated and therefore need to be 
!                       deallocated on exit.
!    RTM_Pc_alloc       As with SPixel_alloc but for RTM_Pc arrays.
!               logical
!    RTM_alloc          As with SPixel_alloc but for RTM arrays.
!               logical
!
! History:
!     2nd Aug 2000, Andy Smith : Original version (in development)
!    11th July 2001, Andy Smith : 
!       Preparing main program for integration with other ECP routines. 
!    15th Aug 2001, Andy Smith:
!       First fully working version of ECP. Includes image segmentation.
!    15th Aug 2001, Andy Smith:
!       Added status checks at start of main loop and after ReadSatData calls.
!    23rd Aug 2001, Andy Smith:
!       Added status check around overall statistics output.
!    21st Sept 2001, Andy Smith:
!       Added initial allocation of SPixel%Ym and Sy. These are deallocated 
!       each time Get_SPixel is called (in Get_Measurements). Also took the 
!       opportunity to put an "if" before part of the final stats output to
!       avoid divide by zero errors, and replace tabs put in by the Nedit
!       auto-indent feature with spaces.
!    24th Sept 2001, Andy Smith:
!       Moved initial allocation of SPixel%Ym, Sy, X and XI to AllocSPixel.
!       Makes the main program more readable and avoids re-coding test
!       harnesses.
!    22nd Oct 2001, Andy Smith:
!       Added calls to deallocate routines for SPixel, RTM_Pc, RTM and the
!       MSI_Data structure. The alloc arrays in these structures *should* 
!       be released automatically at the end of execution. These routines 
!       deallocate explicitly in case the automatic dealloc isn't done. 
!    24th Oct 2001, Andy Smith:
!       Removed conversion of solar channel reflectances from % to fraction.
!       MSI files should now be written with fractional values.
!       New logical variable to track alloc state of SAD_LUT internal arrays.
!     ***************************** ECV work starts here *****************
!    9th Mar 2011, Andy Smith: re-applying changes from late 2001/2,
!      MSI_luns now dimension 6 to allow for albedo data. 
!   22nd Mar 2011, Andy Smith:
!      Removal of phase change. Only 1 cloud class required for each retrieval 
!      run. SADCloudClass and SAD_LUT array dimensions changed. 
!   23rd Mar 2011, Andy Smith:
!      Added output of latitude and longitude to output file. 
!   30th Mar 2011, Andy Smith:
!      Removal of super-pixel averaging. All super-pixelling will now be done
!      in pre-processing. The Spixel structure used here now refers to a single
!      pixel from the input files. 
!      Removed the modification of image (x,y) processing ranges to whole number 
!      of super-pixels. Remove use of Ctrl%Resoln%Space. 
!   8th Apr 2011, Andy Smith:
!     Simplification of selection methods for first guess, a priori, limits etc.
!     InvertMarquardt no longer requires SAD_CloudClass argument. 
!     Removed setting of SPixel%Phase: redundant following removal of phase change
!     functionality. 
!  11th May 2011, Andy Smith:
!     Extension to multiple viewing angles. Elements of the Ctrl struct are now 
!     pointers to arrays rather than fixed-size arrays, so Ctrl cannot be written 
!     in a single operation. Removed the writes of Ctrl to the diag and out file. 
!     It is planned to remove Ctrl from the out file anyway.
! 8th June 2011, Caroline Poulsen:
!     removed diagnostic structure
!     added extra output file residual, quality indicators, apriori and first guess also input structure
!      MSI_luns now dimension 7 to allow for scanline data. 
!
!  8th Jun 2011, Andy Smith:
!     Tidied up log output. Use ORAC not ECP in log file. Trim run ID string. 
!     Added Write_Log call at end, to get finish time. 
!  22nd Sept 2011 Caroline Poulsen remove sw%p as now the same as lw%p
!  8th Oct 2011 Caroline Poulsen added CWP to output 
!  8th Nov 2011 Caroline Poulsen added y0 and sx to output 
!  8th December 2011 Matthias Jerg added code to accommodate netcdfoutput
!  13th December 2011 caroline poulsen remove relenlog to be compatible
! with  f95
!  19th December 2011 Matthias Jerg cleaned up netcdf output, introduced error reporting and added file headers.
!  5th/Jan 2012 Caroline poulsen removed binary output files
!  15th/Jan 2012 Caroline poulsen added missing for ymfit
!  2012/03/27 MJ changes netcdf write to 2D arrays
!  2012/07/13 MJ implements option to read drifile path from command line
!  2012/08/14 MJ irons out bug that made ORAC crash with gfortran
!  2012/08/14 MJ changes scaling of CWP output
!  2012/08/22, MJ includes time in MSI structure and writes it to primary netcdf file
!20120822 MJ makes adaptions to read netcdf files, start and end indices of area to be processed 
!                  determined by preprocessing file contents and not hardwired any more.
!20120827 MJ better implements time variable in output.
!2012 November: MJ implements openMP parallelization of along-track loop.
! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
!!
! Bugs:
!   None known
!
! $Id: ECP.f90 182 2011-10-05 10:03:40Z carnold $
!
!---------------------------------------------------------------------


Program ECP

!  Modules used by this program. 

   use ECP_Constants
   use CTRL_def
    use SAD_Chan_def
   use SAD_LUT_def
   use RTM_def
   use RTM_Pc_def
   use Data_def
   use SPixel_def
   use Diag_def
   use ECP_Routines_def    ! Defines subroutine interfaces for ReadSAD etc
   use input_structures
   use config_s

   use netcdf

!  Local variable declarations

   implicit none

   type(CTRL_t)     :: Ctrl
    type(SAD_Chan_t), dimension(:), allocatable :: SAD_Chan
   type(SAD_LUT_t)  :: SAD_LUT
   type(RTM_t)      :: RTM
   type(RTM_Pc_t)   :: RTM_Pc
   type(SPixel_t)   :: SPixel
   type(Data_t)     :: MSI_Data
   type(config_struct) :: conf
   type(Diag_t)     :: Diag     ! Diagnostic struct returned by Invert_Marquardt
   character(180)   :: message  ! Error message string returned by Read_Driver
   integer          :: status = 0 ! Status value returned from subroutines
   integer          :: ios      ! I/O status value from file operations
   integer          :: log_lun  ! Logical Unit Number for log file
   integer          :: diag_lun ! Logical unit number for diagnostics file
! commented unused but declared variables
!    integer          :: bkp_lun  ! Logical Unit Number for breakpoint file
!   integer          :: out_lun  ! Logical unit number for output file
!    integer          :: res_lun ! Logical unit number for residuals file
!   integer          :: apfg_lun ! Logical unit number for fg and ap file
!    integer          :: qc_lun ! Logical unit number for qc file
!   integer          :: geoout_lun ! Logical unit number for geo/angles file
!    integer          :: runinfo_lun ! Logical unit number for run information file
!   integer          :: scan_lun ! Logical unit number for run information file
!   integer          :: input_lun ! Logical unit number for input file

   integer          :: MSI_luns(7) ! Logical unit numbers for multi-spectral   
                                ! image files (measurements, cloud flags etc)
   logical          :: MSI_Files_open
                                ! Flag indiacting whether the MSI files are 
                                ! open for reading (set false at start of
                                ! program: ReadSatData will set it true).
   character(8)     :: date     ! date returned from Date And Time function
   character(10)    :: time     ! time returned from Date And Time function
   character(24)    :: time_str ! string to hold date + time
   integer          :: ixstart, ixstop,xstep ! First and last super-pixel X locations
   integer          :: iystart, iystop,ystep ! First and last super-pixel Y locations
   logical :: lhres
   real(kind=sreal) :: range_lat(2),range_lon(2)
   integer          :: i, j, m,ipix,jline,jj        ! Loop counter   
   integer          :: conv     ! convergence flag
   integer          :: SegSize     ! Number of rows of pixels in image segment.
   integer          :: NSegs       ! Number of image segments processed
   integer          :: TotPix=0    ! Total number of SPixels processed
   integer          :: TotMissed=0 ! Number of SPixels left unprocessed
   integer          :: TotConv=0   ! Number of successful inversions
   integer          :: TotMaxJ=0   ! Number of inversions with cost > MaxQC
   integer          :: AvIter=0    ! Average no. of iterations per successful
                                   ! retrieval
   integer          :: AvPhCh=0    ! Average no. of phase changes per successful
                                   ! retrieval
   real             :: AvJ=0.0       ! Average cost per successful retrieval

   !some more variables for openMP implementation
   integer, allocatable, dimension(:) :: totpix_line,totmissed_line,totconv_line,totmaxj_line,status_line
   integer, allocatable, dimension(:) :: aviter_line, avphch_line
   real,allocatable, dimension(:) :: avj_line



   logical          :: SPixel_alloc = .false. ! Indicates that Alloc_SPixel
                                   ! has been called and ran successfully.
   logical          :: RTM_Pc_alloc = .false. ! Indicates Alloc_RTM_Pc called
                                   ! and ran successfully.
   logical          :: RTM_alloc = .false. ! Indicates Read_RTM called and ran 
                                   ! successfully, arrays are allocated.
   logical          :: SAD_LUT_alloc = .false. ! Indicates Read_LUT called and 
                                   ! ran successfully, arrays are allocated.

   !netcdf related variables:
   !file ids,dimensions array,debugging flag
!   INTEGER :: ncid_primary,ncid_secondary,ncid_input, dims_var(2), wo
   INTEGER :: ncid_primary,ncid_secondary, dims_var(2), wo
   !write full covariance matrix to secondary output file, so far hardwired
   logical :: lcovar=.FALSE.
   !additional types for the scanline output for netcdf are defined
   type(spixel_scanline_primary_output) :: spixel_scan_out
   type(spixel_scanline_secondary_output) :: spixel_scan_out_sec
   type(spixel_scanline_input) :: spixel_scan_in
!   type(modis_input_d) :: modis_input
   integer :: modispos
   !some netcdf related indices and labels
   integer :: iinput,iviews !,iread_start, iread_stop
   integer :: js,is
   character(len=20) :: input_num,input_num1,input_num2
   character(len=500) :: input_dummy,s_input_dummy
   !variables to avoid out of bounds contents
   real(kind=sreal) :: dummyreal, dummyreal_store,&
        & minvalue=100000.0,maxvalue=-100000.0,dummyrealmax
   integer :: ierr, nargs
   character(len=2048) :: drifile
   integer :: nthreads,OMP_GET_NUM_THREADS,tid,omp_get_thread_num

   !this is for timing the different parts of the code on AIX IBM PWR7 at ECMWF
   integer(kind=nint) :: m0,m1,m2,m3,mclock
   real(kind=dreal) :: r0,r1,r2,r3,rtc
   real(kind=dreal) :: cpu_secs,real_secs
   CHARACTER(LEN=15) :: comp_nthreads
   INTEGER :: Statomp,omp_nthreads

   include "sigtrap.F90"
#define DEBUG

   !initialize timing
110 format(1x,'TIMING: INITIALIZED:',1x,d15.5,1x,'r')
111 format(1x,'TIMING: INITIALIZED:',1x,i10,1x,'m')
112 format(1x,'TIMING: Lead in took:',1x,d15.5,1x,'cpu_secs and',1x,d15.5,1x,'real_secs')
113 format(1x,'TIMING: Along-track loop took:',1x,d15.5,1x,'cpu_secs and',1x,d15.5,1x,'real_secs')
114 format(1x,'TIMING: Lead out took:',1x,d15.5,1x,'cpu_secs and',1x,d15.5,1x,'real_secs')
115 format(1x,'TIMING: Ratio cpu_secs/real_secs:',1x,d15.5)

!!$   cpu_secs=0_dreal
!!$   real_secs=0_dreal
!!$   r0=rtc()
!!$   write(*,110) r0
!!$   m0=mclock()
!!$   write(*,111) m0



   ! get number of threads
   omp_nthreads=1
   CALL GET_ENVIRONMENT_VARIABLE(NAME="OMP_NUM_THREADS",VALUE=comp_nthreads,STATUS=Statomp)
   IF (Statomp /=0) THEN
      PRINT *, "Environment variable 'OMP_NUM_THREADS' not set!"
   else
      read(comp_nthreads, '(i3)') omp_nthreads
      write(*,*) omp_nthreads,'threads available'
   END IF




! -------------------------------------------------------------------
! ------------------ Data Preparation functions
! -------------------------------------------------------------------

   !look if path to driver file was given on the command line:
   nargs=command_argument_count()
   drifile=''

   
   !if yes, then read it, if not leave it to the Read_Driver routine to deal with it
   if(nargs .eq. 1 ) then

      call get_command_argument(1,drifile)

   endif

!  Read Ctrl struct from driver file   

   call Read_Driver(Ctrl, conf,message, drifile,status)

   !read dimensions of preprocessing swath files first:
   call read_input_dimensions_msi(Ctrl%Fid%MSI,Ctrl%FID%Geo,&
        & Ctrl%Ind%Xmax,Ctrl%Ind%YMax,Ctrl%Ind%Nyp,Ctrl%Ind%NInstViews,0)

   write(*,*) 'after read_input_dims',Ctrl%Ind%Y_Id
   !Now set the corners of the domain based on what's in the input files
   Ctrl%Ind%X0=1
   Ctrl%Ind%Y0=1
   Ctrl%Ind%X1=Ctrl%Ind%Xmax
   Ctrl%Ind%Y1=Ctrl%Ind%YMax
   Ctrl%Ind%Xstart=1
   Ctrl%Ind%Ystart=1
   Ctrl%Resoln%SegSize=Ctrl%Ind%YMax

   
!   write(*,*) Ctrl%FID%Log
!   stop

!  Open the log file specified in Ctrl, and close again (gets rid of old copy).
!  On the Linux machine, a write is needed to clear the file (on a DEC machine
!  this wasn't necessary).
! AS exctended recl to avoid annoying line breaks Feb 2011

   call find_lun(log_lun)
   open(unit=log_lun, file=Ctrl%FID%Log, status='replace', iostat=ios)
   write(*,*) log_lun,ios,trim(adjustl(Ctrl%FID%Log))
!   pause
!   open(unit=log_lun, file=Ctrl%FID%Log, status='replace', iostat=ios,recl=ECPlogReclen)


   if (ios == 0) then
      call Date_and_Time (date=date, time=time)
      time_str = date // ' ' // time(1:2) // ':' // time(3:4) // &
         ':' // time(5:6)        
      write(log_lun, *)' ORAC '
      write(log_lun, *)' Start time: ', time_str
      write(log_lun, *)' Run ID: ',Ctrl%Run_ID
   else
      write(*,*)' Error opening log file'
      write(*,*) Ctrl%FID%Log
   end if
   close(unit=log_lun)

!  Handle any error status returned by Read_Driver
!  (special case - ReadDriver can't report it's own errors because it has to
!  read the log file name from the driver file before the log file can be used).

   if (status /= 0) then
      call Write_Log(Ctrl, message, status)
   end if

!  Clear the breakpoint file (if breakpoints required)

#ifdef BKP
   if (status == 0 .and. Ctrl%Bkpl > 0) then
      call find_lun(bkp_lun)
      open(unit=bkp_lun, file=Ctrl%FID%Bkp, status='replace', iostat=ios)

      if (ios == 0) then
         write(bkp_lun, *)' ORAC breakpoint output'
         write(bkp_lun, *)' Start time: ', time_str
         write(bkp_lun, *)' Run ID: ',Ctrl%Run_ID
         write(bkp_lun, *)
      else
         status = BkpFileOpenErr
         call Write_Log(Ctrl, 'Main: Error opening breakpoint file', status)
      end if   
      close(unit=bkp_lun)      
   end if 
#endif

!  Open the output and diagnostic files
   
   call find_lun(diag_lun)
   open(unit=diag_lun, file=Ctrl%FID%Diag, form='Unformatted', &
      status='replace', iostat=ios, err=999)
   write(*,*) 'diag',ios
!  Set the size of the SAD_Chan and Cloud Class arrays based on the Ctrl 
!  parameters and read the SAD values.

   if (status == 0) then      
      allocate(SAD_Chan(Ctrl%Ind%Ny))
      
      call Read_SAD(Ctrl, SAD_Chan, SAD_LUT, status)
      if (status == 0) SAD_LUT_Alloc = .true.
      write(*,*) 'Rading SAF files done (status)',status
      
   end if

   if (status == 0) then
      !make here new netcdf read for all the rttov data
      !read everything in one go, no more segment reads
      call read_input_dimensions_lwrtm(Ctrl,Ctrl%Fid%LWRTM,&
           & RTM%LW%Grid%NLatLon,RTM%LW%Grid%NLon, RTM%LW%Grid%NLat,&
           & RTM%LW%NP,RTM%LW%NPLAY,&
           & RTM%LW%NLWF,RTM%LW%NV,0)

      call read_input_dimensions_swrtm(Ctrl%Fid%SWRTM,&
           & RTM%SW%Grid%NLatLon,RTM%SW%Grid%NLon, RTM%SW%Grid%NLat,&
           & RTM%SW%NP,RTM%SW%NPLAY,&
           & RTM%SW%NSWF,RTM%SW%NV,0)

      !this will probably change as we need to rethink the vert. coordinate!!!???!!!
      RTM%LW%NP=RTM%LW%NPLAY
      RTM%SW%NP=RTM%SW%NPLAY

      call Read_RTMData_nc(Ctrl, RTM, status)      
      if (status == 0) then
         RTM_Alloc = .true.
      endif
      
   end if

! -------------------------------------------------------------------
! ------------------ End of Data Preparation section
! -------------------------------------------------------------------



! -------------------------------------------------------------------
! --------------------- Product generation section 
! -------------------------------------------------------------------

   if (status == 0) then


!     Flag used by Read_Sat_Data and subordinates to determine whether the 
!     image files need to be opened. Required for image segmentation:
!     Read_Sat_Data can be called many times as the image loop executes.

      !MSI_files_open = .false.
      NSegs          = 0

!     Loop over required super pixel X0,Y0 coordinates
!     Determine start and stop values for the loop counters depending on whether 
!     a "warm start" is required. Modify start and stop x values so that only 
!     whole super-pixels are processed. A SPixel of size 3 with a left-hand  
!     corner at 511 will go outside of an image of size 512. Also, if the
!     starting pixel is not a whole number of Super-Pixels from the first line 
!     in the image data, some SPixels in the selected area will cross boundaries 
!     between image segments. 

      if (Ctrl%Ind%Ws > 0) then ! Warm start. Use Xstart, YStart
         ixstart = Ctrl%Ind%Xstart
         iystart = Ctrl%Ind%Ystart          
      else 
         ixstart = Ctrl%Ind%X0
         iystart = Ctrl%Ind%Y0 
      end if

      !iystart=1440
      !iystop=1454

!     Set the stop values 

      ixstop = Ctrl%Ind%X1 
      iystop = Ctrl%Ind%Y1 
      
!IMPORTANT!!!
!--------------------
!--------------------
! Segment approach from now on (netcdf input) obsolete,
! files are all read in in one go
! Code remains untouched as far as it is useful
!--------------------
!--------------------
!Original comment:
!     Read past the first few image segments if user selection doesn't start
!     at the first segment in the image data. The main loop will read a new
!     segment when its y loop counter reaches the first row of a segment.
!     So if iystart is at row 1 of a seg: read all preceding segs.
!     If iystart is at the end of a seg, read all preceding plus the one
!     containing iystart (same calculation as for row 1 but this time the 
!     integer divide goes exactly, no truncation)
!     Otherwise, read all preceding segs plus the one containing iystart (same
!     condition as above but the int divide truncates so we add 1).

      SegSize = Ctrl%Resoln%SegSize
      select case (mod(iystart,SegSize))
!     0 = last row of segment; 1 = 1st row of segment    
      case (0, 1)
         NSegs = iystart / SegSize 
!     Mid-segment
      case default      
         NSegs = (iystart / SegSize) + 1
      end select

      !new netcdf read for all the swath data
      !read everything in one go, no more segment reads
      call Read_SatData_nc(Ctrl, NSegs, SegSize, &
           & MSI_Data, SAD_Chan,status)

      !look if we are processing modis:
      lhres=.false. !"high" resolution flag
      if(index(trim(adjustl(Ctrl%Inst%Name)),'MODIS') .ge. 1 ) then

         !set special range 
!         range_lat(1)=42.0
!         range_lat(2)=53.0

!         range_lon(1)=0.0
!         range_lon(2)=18.0

         !look if any pixel in current granule is in special range
!         lhres=any(MSI_Data%Location%Lat .ge. range_lat(1) .and. &
!              & MSI_Data%Location%Lat .le. range_lat(2) .and. &
!              & MSI_Data%Location%Lon .ge. range_lon(1) .and. &
!              & MSI_Data%Location%Lon .le. range_lon(2))

         lhres=.true.
         !if yes, do higher resolution processing there.
!         if(lhres) then
            xstep=1
            ystep=1
            !otherwise process reduced amount of pixels to speed things up
!         else
!            xstep=2
!            ystep=2
!         endif
!ACP: why in God's name is this automatic? DO NOT COMMIT
         write(*,*) 'MODIS Adaptive processing',lhres,xstep,ystep

      else
         
         lhres=.true.
         xstep=1
         ystep=1
         write(*,*) 'Stepsize',lhres,xstep,ystep

      endif
      

!!$      do j = 1, NSegs
!!$         call Read_SatData(Ctrl, NSegs, SegSize, MSI_files_open, MSI_luns, &
!!$            MSI_Data, SAD_Chan, status)
!!$          if (status /= 0) exit
!!$      end do

      !open  the netcdf output files
      if (status == 0) then
!open  the netcdf output files	
         call nc_create_global_l2(Ctrl,trim(adjustl(Ctrl%FID%L2_primary_outputpath_and_file)),&
        	      & ncid_primary,  ixstop-ixstart+1, iystop-iystart+1, dims_var, wo,1,status)
         call nc_create_global_l2(Ctrl,trim(adjustl(Ctrl%FID%L2_secondary_outputpath_and_file)),&
              & ncid_secondary,  ixstop-ixstart+1, iystop-iystart+1, dims_var, wo,2, status)
!      write(*,*)  trim(adjustl(Ctrl%FID%L2_primary_outputpath_and_file))
!      pause
              
         !allocate output arrays
         call alloc_spixel_scan_out( ixstart,ixstop,iystart,iystop,Ctrl%Ind%NViews,spixel_scan_out)
         call alloc_spixel_scan_out_sec( ixstart,ixstop,iystart,iystop,Ctrl%Ind%Ny,SPixel%Nx,lcovar,spixel_scan_out_sec)      
        
 !include definition of variables files
!         write(*,*) 'before defs'
         include "def_vars_primary.inc"
         include "def_vars_secondary.inc"
!         write(*,*) 'after defs'
!         pause            
 	
      endif

!     Set i, the counter for the image x dimension, for the first row processed.
      i = ixstart

      !This is to make things easier for OpenMP
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
      allocate(status_line(iystart:iystop))
      status_line=0

!!$      m1=mclock()
!!$      write(*,111) m1
!!$      cpu_secs=(m1-m0)*0.01
!!$      r1=rtc()
!!$      write(*,110) r1
!!$      real_secs=(r1-r0)!*0.001
!!$      write(*,112) cpu_secs,real_secs
!!$      cpu_secs=cpu_secs/real_secs
!!$      write(*,115) cpu_secs

      write(*,*) 'Nr of lines to process: ',(iystop - iystart)+1,iystart,iystop 
      write(*,*) 'Starting with line #', iystart
      !Along track loop is parallelized with openMP
      !OMP PRIVATE(j,iviews,iinput,tid,m,SPixel_Alloc,RTM_Pc_Alloc) &

      !Start OMP section by spawning the threads
      !$OMP PARALLEL &
      !$OMP PRIVATE(jj,j,iviews,iinput,tid,m,SPixel_Alloc,RTM_Pc_Alloc) &
      !$OMP FIRSTPRIVATE(status,iystart,iystop,dummyreal,conv,i,Diag,Ctrl,RTM_Pc,Spixel)
      TID = OMP_GET_THREAD_NUM()
      if(tid .eq. 0) then
         NTHREADS = OMP_GET_NUM_THREADS()
         write(*,*) 'ORAC along-track loop now running on', nthreads, 'threads'
      endif
      PRINT *, 'Thread ', TID+1, 'is active'


      !  Allocate sizes of SPixel sub-structure arrays
      call Alloc_SPixel(Ctrl, RTM, SPixel, status)
      if (status == 0) SPixel_Alloc = .true.
      call Alloc_RTM_Pc(Ctrl, RTM_Pc, status)
      !      write(*,*) 'after alloc_rtm_pc'
      !      pause
      if (status == 0) RTM_Pc_Alloc = .true.
         
      !     Set RTM pressure values in SPixel (will not change from here on)
         
      SPixel%RTM%SW%NP = RTM%SW%NP
         
      !     Initialise values required before main loop begins, e.g. first guess phase
      !     which may be required for SDAD first guess/a priori setting in the first 
      !     SPixel when no retrieved data is available.
      
      Spixel%XnSav = Ctrl%Xb
      SPixel%SnSav = 0
      do m=1,MaxStateVar
         SPixel%SnSav(m,m) = Ctrl%Sx(m) ** 2
      end do
      SPixel%Loc%LastX0 = 1
      SPixel%Loc%LastX0 = 1
      
      !this was used for avhrr

      !this is for MODIS


      !Start OMP parallel loop for along track direction.
      !this gets the parameters from the env. variable from the surrouding script
      !OMP DO SCHEDULE(GUIDED)
      !OMP DO SCHEDULE(DYNAMIC,125)
      !OMP DO SCHEDULE(DYNAMIC,180)
      !OMP DO SCHEDULE(RUNTIME)
      !$OMP DO SCHEDULE(DYNAMIC,500)
      do j = iystart, iystop,ystep
      

      !MJ TESTdo j = iystop,iystart,-1
         !write(*,*) 't_id,line',tid+1,jj,iystop-iystart+1
         status_line(j)=status

   !write(*,*) 'processing line: ',j

         !MJ ORG if (status /= 0) exit
!        Set the location of the pixel within the image (Y0) and within 
!        the current image segment (YSeg0). 

         Spixel%Loc%Y0    = j
!         Spixel%Loc%YSeg0 = Spixel%Loc%Y0 - (NSegs * SegSize)          
         if (mod(Spixel%Loc%Y0, SegSize) == 0) then
            Spixel%Loc%YSeg0 = SegSize
         else 
            Spixel%Loc%YSeg0 = Spixel%Loc%Y0 - &
               ((Spixel%Loc%Y0/SegSize) * SegSize)
         end if

!        Read in the next segment of MSI data when j=1, SegSize+1, 
!        (2*SegSize)+1, ...
!        SegSize (local) is the number of rows of pixels in an image segment. 
!        Ctrl%Resoln%SegSize is the no of rows of SPixels, which are of size 
!        Ctrl%Resoln%Space (now size 1, Mar 2011). 

!!$         if (mod(j,SegSize) == 1) then
!!$            call Read_SatData(Ctrl, NSegs, SegSize, MSI_files_open, MSI_luns, &
!!$               MSI_Data, SAD_Chan, status)
!!$
!!$
!!$            NSegs = NSegs + 1
!!$
!!$         end if

         !MJ ORG if (status /= 0) exit

!        Read_SatData makes the last update to the Ctrl structure by setting
!        the date and day of year after the first read from the MSI file.
!        On first execution of the loop write out Ctrl.
!        AS, May 2011, whole-struct write of Ctrl no longer possible due to 
!        presence of pointers. Comment out for now. Ctrl to be removed from 
!        output files? 

!        The X loop is unbounded. This allows for the changing X limits required
!        on warm start: for the first row of SPixels processed the X range is
!        Ctrl%Ind%Xstart to ixstop; for the remainder it is X0 to ixstop (in 
!        both cases the limits used are modified to whole numbers of SPixels).
!MJ         write(*,*) 'Processing line:',j
         !if(mod(j,50) .eq. 0) write(*,*) 'Processing line:',j,'on thread:', tid+1
!         if(j .gt. 1) stop
         do 
            SPixel%Loc%X0 = i
	    !write(*,*) 'Processing pixel:',i
            !MJ ORG TotPix = TotPix+1
            TotPix_line(j) = TotPix_line(j)+1
!           Set up the super-pixel data values.                       

            !write(*,*) 'testtest0'

!MST
           Spixel%QC=0

           !write(*,*) 'testtest1'	


           !write(*,*) 'status1',status
           if (status_line(j) == 0) call Get_SPixel(Ctrl, conf,SAD_Chan, &
                & MSI_Data, RTM, SPixel, status)
           status_line(j)=status
           !write(*,*) 'main qc',Spixel%QC,status
           !Spixel%QC=0
            !write(*,*) 'status2',status
           if (status_line(j) == 0) then
              !              If the super-pixel cannot be processed, zero the outputs and
              !              diag struct.
              
              !write(*,*) 'status3',status
              
              if (btest(Spixel%QC, SpixNoProc)) then
                 !MJ ORG TotMissed = TotMissed+1
                 Totmissed_line(j) = Totmissed_line(j)+1
                 
                 
                 Spixel%Xn = MissingXn
                 Spixel%Sn = MissingSn
                 
                 !write(*,*) 'status3a',status
                 
                 call Zero_Diag(Ctrl, Diag, status)
 
                 !write(*,*) 'status3b',status
                 
              else
                 
                 !write(*,*) 'status4',status
                 !                 No indication that the SPixel should not be processed,
                 !                 do the inversion.
                 !write(*,*) 'before marqw'
                 Call Invert_Marquardt(Ctrl, SPixel, SAD_Chan, SAD_LUT,  &
                      & RTM_Pc, Diag, status)
                 status_line(j)=status
                 !write(*,*) 'after marqw'
                 
                 !                 Set values required for overall statistics
                 !                 1st bit test on QC Flag determines whether convergence 
                 !                 occurred.
                 
                 !                  Spixel%Xn = MissingXn
                 !write(*,*) 'marq status',status,Diag%QCFlag,MaxStateVar
                 !                  pause
                 if (status_line(j) == 0) then
                    if (.not. btest(Diag%QCFlag,MaxStateVar+1)) then
                       
                       !MJ ORG                        TotConv = TotConv+1
                       TotConv_line(j) = TotConv_line(j)+1
                       !MJORG                        AvIter  = AvIter + Diag%Iterations
                       AvIter_line(j)  = AvIter_line(j) + Diag%Iterations
                       if (Diag%PhaseChanges >= 0) then 
                          !MJ ORG                           AvPhCh = AvPhCh + Diag%PhaseChanges
                          AvPhCh_line(j) = AvPhCh_line(j) + Diag%PhaseChanges
                       else
                          !MJ ORG                           AvPhCh = AvPhCh + Ctrl%InvPar%MaxPhase                     
                          AvPhCh_line(j) = AvPhCh_line(j) + Ctrl%InvPar%MaxPhase                     
                       end if
                       !MJ ORG                        AvJ = AvJ + Diag%Jm + Diag%Ja
                       AvJ_line(j) = AvJ_line(j) + Diag%Jm + Diag%Ja
                    end if
                    if (btest(Diag%QCFlag,MaxStateVar+2)) then
                       !MJ ORG                        TotMaxJ = TotMaxJ+1
                       TotMaxJ_line(j) = TotMaxJ_line(j)+1
                    end if
                 else
                    Diag%YmFit= MissingXn
                    Spixel%Xn = MissingXn
                    Spixel%Sn = MissingSn
                    call Zero_Diag(Ctrl, Diag, status)
                    status_line(j)=status
                    
                 end if
                 
              end if!btest if closes

              !calculate the Cloud water path CWP
!
              !write(*,*) 'before cwp',status
              call Calc_CWP(Ctrl,SPixel, status)
              status_line(j)=status
              !write(*,*) 'after cwp',status
              

              
              !              Write the outputs
              
              conv=1
              if (.not. btest(Diag%QCFlag,MaxStateVar+1)) then
                 conv=0
              end if


              ! write out the quality indicators               
              !reset
              !                    Spixel%Xn = MissingXn
              
              !include variable preparation files
              !write(*,*) 'before preps', RTM_Pc%Hc,RTM_Pc%Tc,Spixel%CWP,Spixel%CWP_error
              include "prepare_primary.inc"
              !write(*,*) 'after preps prim'
              !include "prepare_secondary.inc"
              !write(*,*) 'after preps sec'
              !               pause 
              
           end if  ! End of status check after Get_SPixel

            !MJ ORG i = i + 1
            i = i + xstep
            
            if (i > ixstop) exit

         end do ! End of super-pixel X loop

         i = Ctrl%Ind%X0         
      end do    ! End of super-pixel Y loop 
      !$OMP END DO 

      if (SPixel_alloc)   call Dealloc_SPixel(Ctrl, SPixel, status)

      if (RTM_Pc_alloc)   call Dealloc_RTM_Pc(Ctrl, RTM_Pc, status)
      !$OMP END PARALLEL
!!$      m2=mclock()
!!$      write(*,111) m2
!!$      cpu_secs=(m2-m1)*0.01
!!$      r2=rtc()
!!$      write(*,110) r2
!!$      real_secs=(r2-r1)!*0.001
!!$      write(*,113) cpu_secs,real_secs
!!$      cpu_secs=cpu_secs/real_secs
!!$      write(*,115) cpu_secs

      status=0

      if (status == 0) then	
	
      !include netcdf write files
      !         write(*,*) 'before writes'
         include "write_primary.inc"
         !quite some problems with loops over channels in secondary:
         !include "write_secondary.inc"
      !         write(*,*) 'after writes'
      !         pause
      endif

      if (status == 0) then
         TotPix=sum(totpix_line)
         Totmissed=sum(totmissed_line)
         Totconv=sum(totconv_line)
         aviter=sum(aviter_line)
         avphch=sum(avphch_line)
         avj=sum(avj_line)
         totmaxj=sum(totmaxj_line)


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

!     Close the multi spectral image files.

      end if       ! End of status check at start of product generation section
      !deallocate some vectors for openMP
      deallocate(totpix_line)
      deallocate(totmissed_line)
      deallocate(totconv_line)
      deallocate(totmaxj_line)
      deallocate(aviter_line)
      deallocate(avphch_line)
      deallocate(avj_line)
      deallocate(status_line)

   
! -------------------------------------------------------------------
! ------------------ End of Product generation section 
! -------------------------------------------------------------------

!  Deallocate any allocatable arrays that have been set, 
!  close the output and diagnostics files.
!  SAD_LUT is an allocatable arrays of structs, each struct containing
!  allocatable arrays. Hence call a routine to dealloc the internal arrays
!  before deallocating the array of structs.

   if (allocated(SAD_Chan)) deallocate(SAD_Chan) 
!   if (allocated(SAD_CloudClass)) deallocate(SAD_CloudClass)
!   if (allocated(SAD_LUT)) then
      if (SAD_LUT_Alloc) call Dealloc_SAD_LUT(Ctrl, SAD_LUT, status)
!      deallocate(SAD_LUT)
!   end if
      if (RTM_alloc)      call Dealloc_RTM(Ctrl, RTM, status)

      !if (MSI_files_open) call Dealloc_Data(Ctrl, MSI_Data, status)
      call Dealloc_Data(Ctrl, MSI_Data, status)

      if (status == 0) then
         call dealloc_spixel_scan_out(spixel_scan_out)
         call dealloc_spixel_scan_out_sec(spixel_scan_out_sec,lcovar)
      endif

      call Dealloc_Ctrl(Ctrl, status)
  !  call Dealloc_GZero(Ctrl, status)        

      deallocate(conf%channel_ids_instr)
      deallocate(conf%channel_ids_abs)
      deallocate(conf%channel_sw_flag)
      deallocate(conf%channel_lw_flag)
      deallocate(conf%channel_proc_flag)




      close(unit=diag_lun)

      !close netcdf output files
      if (status == 0) then
         CALL nc_close(ncid_primary,trim(adjustl(Ctrl%FID%L2_primary_outputpath_and_file)),wo,ierr)

         if(ierr .ne. 0) then 

            status=PrimaryFileCloseErr

            write(*,*) 'nc_close.F90: netcdf primary file close error:', status
            call Write_Log(Ctrl,'nc_close.F90: netcdf primary file close error:', status)
            stop

         endif

         CALL nc_close(ncid_secondary,trim(adjustl(Ctrl%FID%L2_secondary_outputpath_and_file)),wo,ierr)

         if(ierr .ne. 0) then 

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

!!$   m3=mclock()
!!$   write(*,111) m3
!!$   cpu_secs=(m3-m2)*0.01
!!$   r3=rtc()
!!$   write(*,110) r3
!!$   real_secs=(r3-r2)!*0.001
!!$   write(*,114) cpu_secs,real_secs
!!$   cpu_secs=cpu_secs/real_secs
!!$   write(*,115) cpu_secs

 End Program ECP

