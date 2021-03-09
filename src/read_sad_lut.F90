!-------------------------------------------------------------------------------
! Name: read_sad_lut.F90
!
! Purpose:
! Module of various routines used to read ORAC Look-Up Tables,
! supporting the subroutine Read_SAD_LUT() defined at the end.
!
! History:
! 2014/10/10, GM: Original version.
! 2015/03/18, OS: Needed to add "/" for LUT file path
! 2016/05/02, AP: Add routines to read 1D BextRat tables.
! 2017/01/17, GM: Eliminate the unnecessary indexing of the LUT grid wrt LUT
!    type and channel.
! 2017/01/18, GM: Add checks to make sure that all the LUTs loaded have
!    grids consistent in dimension size, spacing and vertex values.
! 2017/03/16, GT: Changes for single-view aerosol retrieval mode.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Name: grid_dimension_read
!
! Purpose:
! Read a single grid dimension.
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine grid_dimension_read(filename, n_name, d_name, v_name, lun, nMax, &
                               n, d, Min, Max, x)

   implicit none

   ! Argument declarations
   character(len=*), intent(in)    :: filename
   character(len=*), intent(in)    :: n_name
   character(len=*), intent(in)    :: d_name
   character(len=*), intent(in)    :: v_name
   integer,          intent(in)    :: lun
   integer,          intent(in)    :: nMax
   integer,          intent(out)   :: n
   real,             intent(out)   :: d
   real,             intent(out)   :: Max
   real,             intent(out)   :: Min
   real,             intent(inout) :: x(:)

   ! Local variables
   integer :: i
   integer :: iostat
   integer :: n2
   real    :: d2
   real    :: x2(NMax)

   read(lun, *, iostat=iostat) n2, d2
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: grid_dimension_read(): Error reading ', trim(n_name), &
                 ' and ', trim(d_name), ' from SAD LUT file: ', trim(filename)
      stop LUTFileReadErr
   end if

   if (n2 .gt. NMax) then
      write(*,*) 'ERROR: grid_dimension_read(): LUT grid dimension size for ', &
                 trim(v_name), ' is greater than maximum size allowed'
      stop LUTFileReadErr
   end if

   read(lun, *, iostat=iostat) (x2(i), i = 1, n2)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: grid_dimension_read(): Error reading ', trim(v_name), &
                 ' from SAD LUT file: ', trim(filename)
      stop LUTFileReadErr
   end if

   if (n .gt. 0) then
      ! This dimension has already been read from a previous LUT
      if (n2 .ne. n) then
         write(*,*) 'ERROR: grid_dimension_read(): LUT grid dimension size for ', &
                    trim(v_name), ' is inconsistent between LUT files'
         stop LUTFileReadErr
      end if

      if (d2 .ne. d) then
         write(*,*) 'ERROR: grid_dimension_read(): LUT grid dimension spacing for ', &
                    trim(v_name), ' is inconsistent between LUT files'
         stop LUTFileReadErr
      end if

      if (any(x2(1:n)  .ne. x(1:n))) then
         write(*,*) 'ERROR: grid_dimension_read(): LUT grid dimension vertices for ', &
                    trim(v_name), ' are inconsistent between LUT files'
         stop LUTFileReadErr
      end if
   else
      ! This is the first time this dimension has been encountered
      n   = n2
      d   = d2
      x   = x2
      Min = x2(1)
      Max = x2(n2)
   end if

end subroutine grid_dimension_read


!-------------------------------------------------------------------------------
! Name: read_grid_dimensions
!
! Purpose:
! Read all required grid dimensions
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine read_grid_dimensions(filename, lun, SAD_LUT, has_sol_zen, &
                                has_sat_zen, has_rel_azi)

   implicit none

   ! Argument declarations
   character(len=*), intent(in)    :: filename
   integer,          intent(in)    :: lun
   type(SAD_LUT_t),  intent(inout) :: SAD_LUT
   logical,          intent(in)    :: has_sol_zen
   logical,          intent(in)    :: has_sat_zen
   logical,          intent(in)    :: has_rel_azi

   ! Read the Tau dimension
   call grid_dimension_read(filename, 'nTau', 'dTau', 'Tau', lun, &
                            SAD_LUT%Grid%NMaxTau, SAD_LUT%Grid%nTau, &
                            SAD_LUT%Grid%dTau, SAD_LUT%Grid%MinTau, &
                            SAD_LUT%Grid%MaxTau, SAD_LUT%Grid%Tau)

   if (has_sat_zen) then
      ! Read the Satzen dimension
      call grid_dimension_read(filename, 'nSatzen', 'dSatzen', 'Satzen', lun, &
                               SAD_LUT%Grid%NMaxSatZen, SAD_LUT%Grid%nSatzen, &
                               SAD_LUT%Grid%dSatzen, SAD_LUT%Grid%MinSatzen, &
                               SAD_LUT%Grid%MaxSatzen, SAD_LUT%Grid%Satzen)
   end if

   if (has_sol_zen) then
      ! Read the Solzen dimension
      call grid_dimension_read(filename, 'nSolzen', 'dSolzen', 'Solzen', lun, &
                               SAD_LUT%Grid%NMaxSolZen, SAD_LUT%Grid%nSolzen, &
                               SAD_LUT%Grid%dSolzen, SAD_LUT%Grid%MinSolzen, &
                               SAD_LUT%Grid%MaxSolzen, SAD_LUT%Grid%Solzen)
   end if

   if (has_rel_azi) then
      ! Read the Relazi dimension
      call grid_dimension_read(filename, 'nRelazi', 'dRelazi', 'Relazi', lun, &
                               SAD_LUT%Grid%NMaxRelAzi, SAD_LUT%Grid%nRelazi, &
                               SAD_LUT%Grid%dRelazi, SAD_LUT%Grid%MinRelazi, &
                               SAD_LUT%Grid%MaxRelazi, SAD_LUT%Grid%Relazi)
   end if

   ! Read the Re dimension
   call grid_dimension_read(filename, 'nRe', 'dRe', 'Re', lun, &
                            SAD_LUT%Grid%NMaxRe, SAD_LUT%Grid%nRe, &
                            SAD_LUT%Grid%dRe, SAD_LUT%Grid%MinRe, &
                            SAD_LUT%Grid%MaxRe, SAD_LUT%Grid%Re)

end subroutine read_grid_dimensions


!-------------------------------------------------------------------------------
! Name: read_values_2d
!
! Purpose:
! Read 2d LUT values
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine read_values_2d(filename, v_name, lun, i_chan, i_lut, &
                          n_i, n_j, values)

   implicit none

   ! Argument declarations
   character(len=*), intent(in)    :: filename
   character(len=*), intent(in)    :: v_name
   integer,          intent(in)    :: lun
   integer,          intent(in)    :: i_chan
   integer,          intent(in)    :: i_lut
   integer,          intent(in)    :: n_i
   integer,          intent(in)    :: n_j
   real,             intent(inout) :: values(:,:,:)

   ! Local variables
   integer :: i, j
   integer :: iostat

   read(lun, *, iostat=iostat) ((values(i_chan, i, j), &
      i = 1, n_i), j = 1, n_j)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: read_values_2d(): Error reading ', v_name, &
         ' from file: ', trim(v_name), trim(filename)
      stop LUTFileReadErr
   end if

end subroutine read_values_2d


!-------------------------------------------------------------------------------
! Read 3d LUT values
!-------------------------------------------------------------------------------
subroutine read_values_3d(filename, v_name, lun, i_chan, i_lut, &
                          n_i, n_j, n_k, values)

   implicit none

   character(len=*), intent(in)    :: filename
   character(len=*), intent(in)    :: v_name
   integer,          intent(in)    :: lun
   integer,          intent(in)    :: i_chan
   integer,          intent(in)    :: i_lut
   integer,          intent(in)    :: n_i
   integer,          intent(in)    :: n_j
   integer,          intent(in)    :: n_k
   real,             intent(inout) :: values(:,:,:,:)

   ! Local variables
   integer :: i, j, k
   integer :: iostat

   read(lun, *, iostat=iostat) (((values(i_chan, i, j, k), &
      i = 1, n_i), j = 1, n_j), &
      k = 1, n_k)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: read_values_3d(): Error reading ', v_name, &
         ' from file: ', trim(v_name), trim(filename)
      stop LUTFileReadErr
   end if

end subroutine read_values_3d


!-------------------------------------------------------------------------------
! Name: read_values_5d
!
! Purpose:
! Read 5d LUT values
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine read_values_5d(filename, v_name, lun, i_chan, i_lut, &
                          n_i, n_j, n_k, n_l, n_m, values)

   implicit none

   ! Argument declarations
   character(len=*), intent(in)    :: filename
   character(len=*), intent(in)    :: v_name
   integer,          intent(in)    :: lun
   integer,          intent(in)    :: i_chan
   integer,          intent(in)    :: i_lut
   integer,          intent(in)    :: n_i
   integer,          intent(in)    :: n_j
   integer,          intent(in)    :: n_k
   integer,          intent(in)    :: n_l
   integer,          intent(in)    :: n_m
   real,             intent(inout) :: values(:,:,:,:,:,:)

   ! Local variables
   integer :: i, j, k, l, m
   integer :: iostat

   read(lun, *, iostat=iostat) (((((values(i_chan, i, j, k, l, m), &
      i = 1, n_i), j = 1, n_j), &
      k = 1, n_k), l = 1, n_l), &
      m = 1, n_m)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: read_values_5d(): Error reading ', v_name, &
         ' from file: ', trim(v_name), trim(filename), lun
      stop LUTFileReadErr
   end if

end subroutine read_values_5d


!-------------------------------------------------------------------------------
! Name: Read_LUT_rat
!
! Purpose:
! Read a BextRat LUT
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2016/05/03, AP: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Read_LUT_rat(Ctrl, LUT_file, SAD_LUT, i_chan, i_lut, name, values)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),     intent(in)    :: Ctrl
   character(len=*), intent(in)    :: LUT_file
   type(SAD_LUT_t),  intent(inout) :: SAD_LUT
   integer,          intent(in)    :: i_chan
   integer,          intent(in)    :: i_lut
   character(len=*), intent(in)    :: name
   real,             intent(inout) :: values(:,:)

   ! Local variables
   real    :: tmp_values(1, SAD_LUT%Grid%NMaxTau, SAD_LUT%Grid%NMaxRe)
   integer :: lun
   integer :: iostat

   call Find_LUN(lun)
   open(unit = lun, file = LUT_file, status = 'old', iostat = iostat)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: Read_LUT_ch(): Error opening file: ', trim(LUT_file)
      stop LUTFileOpenErr
   end if

   ! Read the grid dimensions (temporarily 2D)
   call read_grid_dimensions(LUT_file, lun, SAD_LUT, .false., .false., .false.)

   ! Read in the i_lut array (temporarily 2D)
   call read_values_2d(LUT_file, name, lun, i_chan, i_lut, SAD_LUT%Grid%nTau, &
                       SAD_LUT%Grid%nRe, tmp_values)

   values(i_chan,1:SAD_LUT%Grid%nRe) = tmp_values(1,1,1:SAD_LUT%Grid%nRe)

   close(unit = lun)

end subroutine Read_LUT_rat

!-------------------------------------------------------------------------------
! Name: Read_LUT
!
! Purpose:
! Read an LUT over effective radius and optical depth
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Read_LUT(Ctrl, LUT_file, i_chan, SAD_LUT, i_lut, name, values)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),     intent(in)    :: Ctrl
   character(len=*), intent(in)    :: LUT_file
   integer,          intent(in)    :: i_chan
   type(SAD_LUT_t),  intent(inout) :: SAD_LUT
   integer,          intent(in)    :: i_lut
   character(len=*), intent(in)    :: name
   real,             intent(inout) :: values(:,:,:)

   ! Local variables
   integer :: lun
   integer :: iostat

   call Find_LUN(lun)
   open(unit = lun, file = LUT_file, status = 'old', iostat = iostat)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: Read_LUT(): Error opening file: ', trim(LUT_file)
      stop LUTFileOpenErr
   end if

   ! Read Wavelength
   read(lun, *, iostat=iostat) SAD_LUT%Wavelength(i_chan)

   ! Read the grid dimensions
   call read_grid_dimensions(LUT_file, lun, SAD_LUT, .false., .false., .false.)

   ! Read in the i_lut array
   call read_values_2d(LUT_file, name, lun, i_chan, i_lut, SAD_LUT%Grid%nTau, &
                       SAD_LUT%Grid%nRe, values)

   close(unit = lun)

end subroutine Read_LUT


!-------------------------------------------------------------------------------
! Name: Read_LUT_sat
!
! Purpose:
! Read an LUT that has a variable solar zenith angle
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Read_LUT_sat(Ctrl, LUT_file, i_chan, SAD_LUT, i_lut, name, &
                        values, i_lut2, name2, values2)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),     intent(in)              :: Ctrl
   character(len=*), intent(in)              :: LUT_file
   integer,          intent(in)              :: i_chan
   type(SAD_LUT_t),  intent(inout)           :: SAD_LUT
   integer,          intent(in)              :: i_lut
   character(len=*), intent(in)              :: name
   real,             intent(inout)           :: values(:,:,:,:)
   integer,          intent(in),    optional :: i_lut2
   character(len=*), intent(in),    optional :: name2
   real,             intent(inout), optional :: values2(:,:,:)

   ! Local variables
   integer :: lun
   integer :: iostat

   call Find_LUN(lun)
   open(unit = lun, file = LUT_file, status = 'old', iostat = iostat)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: Read_LUT_sat(): Error opening file: ', trim(LUT_file)
      stop LUTFileOpenErr
   end if

   ! Read Wavelength
   read(lun, *, iostat=iostat) SAD_LUT%Wavelength(i_chan)

   ! Read the grid dimensions
   call read_grid_dimensions(LUT_file, lun, SAD_LUT, .false., .true., .false.)

   ! Read in the i_lut array
   call read_values_3d(LUT_file, name, lun, i_chan, i_lut, &
           SAD_LUT%Grid%nTau, SAD_LUT%Grid%nSatzen, SAD_LUT%Grid%nRe, values)

   if (present(i_lut2)) then
      ! Read in the i_lut2 array.
      call read_values_2d(LUT_file, name2, lun, i_chan, i_lut2, &
              SAD_LUT%Grid%nTau, SAD_LUT%Grid%nRe, values2)
   end if

   close(unit = lun)

end subroutine Read_LUT_sat


!-------------------------------------------------------------------------------
! Name: Read_LUT_sol
!
! Purpose:
! Read an LUT that has a variable solar zenith angle
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Read_LUT_sol(Ctrl, LUT_file, i_chan, SAD_LUT, i_lut, name, &
                        values, i_lut2, name2, values2)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),     intent(in)              :: Ctrl
   character(len=*), intent(in)              :: LUT_file
   integer,          intent(in)              :: i_chan
   type(SAD_LUT_t),  intent(inout)           :: SAD_LUT
   integer,          intent(in)              :: i_lut
   character(len=*), intent(in)              :: name
   real,             intent(inout)           :: values(:,:,:,:)
   integer,          intent(in),    optional :: i_lut2
   character(len=*), intent(in),    optional :: name2
   real,             intent(inout), optional :: values2(:,:,:)

   ! Local variables
   integer :: lun
   integer :: iostat

   call Find_LUN(lun)
   open(unit = lun, file = LUT_file, status = 'old', iostat = iostat)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: Read_LUT_sol(): Error opening file: ', trim(LUT_file)
      stop LUTFileOpenErr
   end if

   ! Read Wavelength
   read(lun, *, iostat=iostat) SAD_LUT%Wavelength(i_chan)

   ! Read the grid dimensions
   call read_grid_dimensions(LUT_file, lun, SAD_LUT, .true., .false., .false.)

   ! Read in the i_lut array
   call read_values_3d(LUT_file, name, lun, i_chan, i_lut, &
           SAD_LUT%Grid%nTau, SAD_LUT%Grid%nSolzen, SAD_LUT%Grid%nRe, values)

   if (present(i_lut2)) then
      ! Read in the i_lut2 array.
      call read_values_2d(LUT_file, name2, lun, i_chan, i_lut2, &
              SAD_LUT%Grid%nTau, SAD_LUT%Grid%nRe, values2)
      end if

   close(unit = lun)

end subroutine Read_LUT_sol


!-------------------------------------------------------------------------------
! Name: Read_LUT_both
!
! Purpose:
! Read an LUT that has a variable solar and satellite zenith angle, in which
! case relative azimuth angle also matters.
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Read_LUT_both(Ctrl, LUT_file, i_chan, SAD_LUT, i_lut, name, &
                         values, i_lut2, name2, values2)

   use Ctrl_m
   use ORAC_Constants_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),     intent(in)              :: Ctrl
   character(len=*), intent(in)              :: LUT_file
   integer,          intent(in)              :: i_chan
   type(SAD_LUT_t),  intent(inout)           :: SAD_LUT
   integer,          intent(in)              :: i_lut
   character(len=*), intent(in)              :: name
   real,             intent(inout)           :: values(:,:,:,:,:,:)
   integer,          intent(in),    optional :: i_lut2
   character(len=*), intent(in),    optional :: name2
   real,             intent(inout), optional :: values2(:,:,:,:)

   ! Local variables
   integer :: lun
   integer :: iostat

   call Find_LUN(lun)
   open(unit = lun, file = LUT_file, status = 'old', iostat = iostat)
   if (iostat .ne. 0) then
      write(*,*) 'ERROR: Read_LUT_both(): Error opening file: ', trim(LUT_file)
      stop LUTFileOpenErr
   end if

   ! Read Wavelength
   read(lun, *, iostat=iostat) SAD_LUT%Wavelength(i_chan)

   ! Read the grid dimensions
   call read_grid_dimensions(LUT_file, lun, SAD_LUT, .true., .true., .true.)

   ! Read in the i_lut array
   call read_values_5d(LUT_file, name, lun, i_chan, i_lut, &
           SAD_LUT%Grid%nTau, SAD_LUT%Grid%nSatzen, SAD_LUT%Grid%nSolzen, &
           SAD_LUT%Grid%nRelazi, SAD_LUT%Grid%nRe, values)

   if (present(i_lut2)) then
      ! Read in the i_lut2 array.
      call read_values_3d(LUT_file, name2, lun, i_chan, i_lut2, &
              SAD_LUT%Grid%nTau, SAD_LUT%Grid%nSolzen, SAD_LUT%Grid%nRe, values2)
      end if

   close(unit = lun)

end subroutine Read_LUT_both


!-------------------------------------------------------------------------------
! Name: Read_SAD_LUT
!
! Purpose:
! Reads the required SAD LUTs.
!
! Algorithm:
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct In          Control structure
! SAD_Chan struct out         SAD_Chan structure filled from a read using
!                             Read_SAD_Chan()
! SAD_LUT  struct out         Structure to hold the values from the LUT
!                             files.
! i_layer  int    In          Layer index.
!
! History:
! 2000/10/13, AS: Original version
! 2000/11/23, AS: Channel file names updated: using 'Ch' instead of 'CH'
! 2001/01/09, AS: Emissivity files available. Read_LUT_EM call un-commented.
!    Added breakpoint output. Ctrl%Ind%Y renamed Y_Id
! 2001/01/12, AS: Arrays of LUT values (Rbd etc) made allocatable. Allocate
!    sizes here.
! 2001/01/18, AS: Bug fix in array allocation. Rfd, Tfd arrays must always be
!    allocated even if the choice of channels means they're unused, because they
!    are read from the same files as Rd, Td by the same routines.
! 2001/02/09, AS: Using pre-defined constants (ECPConstants.f90) for breakpoint
!    levels.
! 2001/03/01, AS: LUT array values are now divided by 100 since values in files
!    are percentages and we require fractions later on.
!    (Temporary fix until files are re-written?)
! 2001/06/07, AS: Debug log message removed from routine Read_LUT_Rbd
! **************** ECV work starts here *************************************
! 2011/03/22, AS: Remove phase change, phase 2 only 1 cloud class per run.
!    SAD_LUT is also now reduced from dimension N cloud classes to 1.
! 2011/04/06, AS: Removed two redundant breakpoint outputs now that only 1
!    cloud class.
! 2011/05/03, AS: Extension to multiple instrument views. Wavelength array is
!    now allocated. Added wavelength to breakpoint outputs to allow checking
!    when >1 view selected.
! 2011/05/03, CP: removed allocation of LUTs into individual
!    routine so ntau,nrensatzen etc could be read and used directly from LUT
!    files and are not replicated else where
! 2014/01/16, GM: Added allocation of SAD_LUT%table_use* arrays.
! 2014/01/23, GM: Cleaned up code.
! 2014/02/04, MJ: Implements code for AVHRR to assign channel numbers for LUT
!    names.
! 2014/09/20, GM: Use a subroutine to create the LUT filenames.
! 2014/10/10, GM: Use the new LUT read code.
! 2014/12/19, AP: YSolar and YThermal now contain the index of
!    solar/thermal channels with respect to the channels actually processed,
!    rather than the MSI file.
! 2014/12/29, GM: Fixed a bug in the channel indexing changes above.
! 2015/01/09, CP: Added Rfbd.
! 2015/01/19, GM: Use make_sad_chan_num().
! 2015/08/21, AP: Generalised MS NOAA7/9 fix, moving create_lut_filename into
!    SAD_Chan_m and renaming it create_sad_filename.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2015/10/19, GM: Added support to read the Bext LUT for Ctrl%do_CTP_correction.
! 2016/07/27, GM: Read LUTs for layer 2 when the multilayer retrieval is active.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
subroutine Read_SAD_LUT(Ctrl, SAD_Chan, SAD_LUT, i_layer)

   use Ctrl_m
   use SAD_Chan_m
   use sad_util_m

   implicit none

   ! Argument declarations

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(:)
   type(SAD_LUT_t),  intent(inout) :: SAD_LUT
   integer,          intent(in)    :: i_layer

   ! Local variables

   integer                    :: i        ! Array counters
   character(len=FilenameLen) :: LUT_file ! Name of LUT file
   character(len=4)           :: chan_num ! Channel number converted to a string
   real, allocatable          :: tmp(:)   ! Array for flipping RelAzi

   ! For each cloud class, construct the LUT filename from the instrument name,
   ! cloud class ID, variable name and channel number. Then call the appropriate
   ! LUT file read function. Just pass the current SAD_LUT struct, rather than
   ! the whole array.
   call Alloc_SAD_LUT(Ctrl, SAD_LUT)

   ! We initialize these to zero to identify during the reading process if these
   ! dimensions have been read yet as not all LUTs have all dimensions.
   SAD_LUT%Grid%nTau    = 0
   SAD_LUT%Grid%nRe     = 0
   SAD_LUT%Grid%nSatzen = 0
   SAD_LUT%Grid%nSolzen = 0
   SAD_LUT%Grid%nRelazi = 0

   do i = 1, Ctrl%Ind%Ny
      ! Generate channel file name from Ctrl struct info. This sets the channel
      ! to be used in instrument notation for reading from SAD.
      call make_sad_chan_num(Ctrl, i, chan_num)
      if (Ctrl%verbose) &
           write(*,*) 'SAD Channel number: ', trim(adjustl(chan_num))

      ! Read the Rd and Rfd LUTs from the Rd file for all channels (solar and
      ! thermal)
      LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'RD')
      call Read_LUT_sat(Ctrl, LUT_file, i, SAD_LUT, IRd, "Rd", SAD_LUT%Rd, &
                        i_lut2 = IRfd, name2 = "Rfd", values2 = SAD_LUT%Rfd)

      ! Read the Td and Tfd LUTs from the Td file for all channels (solar and
      ! thermal)
      LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'TD')
      call Read_LUT_sat(Ctrl, LUT_file, i, SAD_LUT, ITd, "Td", SAD_LUT%Td, &
                        i_lut2 = ITfd, name2 = "Tfd", values2 = SAD_LUT%Tfd)

      ! Read solar channel LUTs
      if (SAD_Chan(i)%Solar%Flag > 0) then
         ! Read the Rbd LUT from the Rbd files
         LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'RBD')
         call Read_LUT_both(Ctrl, LUT_file, i, SAD_LUT, IRbd, "Rbd", &
                            SAD_LUT%Rbd, i_lut2 = IRfbd, name2 = "Rfbd", &
                            values2 = SAD_LUT%Rfbd)

         ! Read the Tb file LUT from the Tb files
         LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'TB')
         call Read_LUT_sol(Ctrl, LUT_file, i, SAD_LUT, ITb, "Tb", &
                           SAD_LUT%Tb)

         ! Read the Tbd and Tfbd LUTs from the Tbd files
         LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'TBD')
         call Read_LUT_both(Ctrl, LUT_file, i, SAD_LUT, ITbd, "Tbd", &
                            SAD_LUT%Tbd, i_lut2 = ITfbd, name2 = "Tfbd", &
                            values2 = SAD_LUT%Tfbd)
      end if

      ! Read thermal channel LUTs
      if (SAD_Chan(i)%Thermal%Flag > 0) then
         ! Read the Em file
         LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'EM')
         call Read_LUT_sat(Ctrl, LUT_file, i, SAD_LUT, IEm, "Em", SAD_LUT%Em)

         if (Ctrl%do_CTX_correction .and. Ctrl%Class .eq. ClsCldIce) then
            ! Read the Bext file
            LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'Bext')
            call Read_LUT(Ctrl, LUT_file, i, SAD_LUT, IBext, "Bext", SAD_LUT%Bext)
         end if
      end if
   end do

   ! Read AOD conversion table
   if (Ctrl%Approach == AppAerOx .or. Ctrl%Approach == AppAerSw .or. &
       Ctrl%Approach == AppAerO1) then
      call make_sad_chan_num(Ctrl, Ctrl%second_aot_ch(1), chan_num)
      LUT_File = create_sad_filename(Ctrl, chan_num, i_layer, 'BextRat')
      call Read_LUT_rat(Ctrl, LUT_File, SAD_LUT, 1, IBextRat, "BextRat", &
           SAD_LUT%BextRat)
   end if

   ! Convert from percentage to fractional values
   SAD_LUT%Rd  = SAD_LUT%Rd  / 100.
   SAD_LUT%Td  = SAD_LUT%Td  / 100.
   SAD_LUT%Tfd = SAD_LUT%Tfd / 100.
   SAD_LUT%Rfd = SAD_LUT%Rfd / 100.

   if (Ctrl%Ind%NSolar > 0) then
      SAD_LUT%Rbd  = SAD_LUT%Rbd  / 100.
      SAD_LUT%RFbd = SAD_LUT%Rfbd / 100.
      SAD_LUT%Tbd  = SAD_LUT%Tbd  / 100.
      SAD_LUT%Tb   = SAD_LUT%Tb   / 100.
      SAD_LUT%Tfbd = SAD_LUT%Tfbd / 100.
   end if

   if (Ctrl%Ind%NThermal > 0) then
      SAD_LUT%Em = SAD_LUT%Em / 100.
   end if

   ! Invert Relazi axis
   allocate(tmp(SAD_LUT%Grid%nRelazi))
   do i = 1, SAD_LUT%Grid%nRelazi
      tmp(i) = SAD_LUT%Grid%Relazi(SAD_LUT%Grid%nRelazi - i + 1)
   end do
   SAD_LUT%Grid%Relazi(1:SAD_LUT%Grid%nRelazi) = tmp
   deallocate(tmp)

end subroutine Read_SAD_LUT
