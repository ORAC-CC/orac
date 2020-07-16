!-------------------------------------------------------------------------------
! Name: read_aatsr_l1b.F90
!
! Purpose:
! Read AATSR level L1b file using a C function. This routine passes a set of C
! pointers to that function which point to the appropriate sections of
! imager_measurements%data or other imager structures.
!
! Description and Algorithm details:
! 1) Allocate pointers as required by the views and channels requested.
! 2) Call C function.
! 3) Correct data returned to correspond to the desired formatting. These include
!    converting elevation angle to zenith angles and percentage radiances into
!    fractional radiances.
! 4) Apply the drift corrections dictated by the drift table.
! 5) Parse the land/sea and cloud flags into the ORAC format.
!
! Arguments:
! Name                 Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! l1b_file             string in   Full path to level 1B data
! drift_file           string in   Full path to the AATSR calibration file
! imager_geolocation   struct both Summary of pixel positions
! imager_measurements  struct both Satellite observations
! imager_angles        struct both Summary of sun/satellite viewing angles
! imager_flags         struct both Summary of land/sea/ice flags
! imager_time          struct both Summary of pixel observation time
! channel_info         struct in   Summary of channel information
! verbose              logic  in   T: print status information; F: don't
!
! History:
! 2012/06/22, GT: First version
! 2012/07/29, CP: added header and changed filename to lower case
! 2012/08/01, GT: Bug fix: solzen, satzen, solazi and relazi were
!    all being stored in imager_angles%solzen!
! 2012/08/20, MJ: fixes several programming errors
! 2012/08/21, GT: Further syntax fixes
! 2012/08/28, GT: Changed C calls to use iso_c_binding. Temporarily commented
!    out calibration correction code until it can be updated.
! 2012/09/12, GT: Changed size of arrays that took output of C data
! 2012/09/12, GT: changed day array structure write
! 2012/09/12, GT: assigned time to imager_time structure
! 2012/09/13, GT: Reactivated calibration correction code.
! 2012/09/14, GT: Bug fix in indexing of nday array. Bug fix to
!    read_aatsr_beam_ctof90 interface: fixed length strings are not supported by
!    iso_c_binding and must be defined as
!      character(kind=c_char), intent(inout) :: var(*)
!    not
!      character(kind=c_char,len=X), intent(inout) :: var
! 2012/11/29, CP: changed type and initialise channels_short
! 2013/09/02, AP: Removed startyi, endye.
! 2013/09/10, AP: tidying
! 2013/10/07, AP: Complete rewrite. Rather than allocate arrays for the C
!    function to read into, this now passes a set of pointers. This relies on
!    imager_measurements%data being real(sreal) as that exactly corresponds to
!    c_float. The drift corrections have also been altered: aatsr_corrections
!    now simply returns a few constants rather than passing arrays around.
! 2013/10/10, MJ: fixed small bug
! 2013/10/17, GM: Hoist loop invariant call to aatsr_read_drift_table() out
!    of loop.
! 2014/01/27, MJ: data type corrections
! 2014/04/25, GM: Use the "is_lut_drift_corrected" flag from read_aatsr_orbit()
!    to determine if the LUT based drift correction has already been applied to
!    the data as in the 3rd reprocessing (V2.1) data.
! 2014/06/30, GM: Apply 12um nonlinearity brightness temperature correction.
! 2015/01/15, AP: Eliminate channel_ids_abs.
! 2015/09/15, CP: Adapted to read ATSR-2 data. NB a bug currently exists such
!    that ATSR-2 drift correction idicator does not work so the code currently
!    assumes we  are using the latest calibrated version of AATSR data v 2.1/3.0
! 2015/11/15, CP: Corrects implementataion of 12um non linearity correction.
! 2015/11/26, GM: Translate AATSR time to Julian time.
! 2016/04/07, SP: Changed channel indexing for 2nd view, now channels in view 2
!                 start with channel 8. This simplifies things (no multiple Ch1s)
!
! Bugs:
!   Currently ATSR-2 needs to be hardwired to true drift correction as the
!   drift flag does not work.
!-------------------------------------------------------------------------------

subroutine read_aatsr_l1b(l1b_file, drift_file, imager_geolocation, &
     imager_measurements, imager_angles, imager_flags, imager_time, &
     channel_info, sensor, verbose)

   use iso_c_binding ! technically Fortran 2003
   use aatsr_corrections_m
   use channel_structures_m
   use imager_structures_m
   use preproc_constants_m

   implicit none

   interface
     subroutine read_aatsr_orbit(l1b_file, verb, nch, ch, view, &
           nx, ny, startx, starty, stat, lat, lon, &
           nsza, niza, nsaz, niaz, nraz, nflg, nqul, nday, &
           nch1, nch2, nch3, nch4, nch5, nch6, nch7, &
           fsza, fiza, fsaz, fiaz, fraz, fflg, fqul, fday, &
           fch1, fch2, fch3, fch4, fch5, fch6, fch7, &
           start_date, gc1_file, vc1_file, is_lut_drift_corrected) &
           bind(C, name='read_aatsr_orbit')
         use iso_c_binding ! technically Fortran 2003
         use preproc_constants_m

         implicit none

         character(kind=c_char), dimension(path_length) :: l1b_file
         character(kind=c_char), dimension(30)   :: start_date
         character(kind=c_char), dimension(62)   :: gc1_file, vc1_file
         integer(kind=c_short)                   :: nch, stat
         integer(kind=c_short), dimension(nch)   :: ch, view
         integer(kind=c_long)                    :: nx, ny, startx, starty
         integer(kind=c_short), dimension(nx,ny) :: nflg, nqul, fflg, fqul
         real(kind=c_double), dimension(ny)      :: nday, fday
         logical(kind=c_bool)                    :: verb, is_lut_drift_corrected
         type(c_ptr) :: lat, lon
         type(c_ptr) :: nsza, niza, nsaz, niaz, nraz
         type(c_ptr) :: nch1, nch2, nch3, nch4, nch5, nch6, nch7
         type(c_ptr) :: fsza, fiza, fsaz, fiaz, fraz
         type(c_ptr) :: fch1, fch2, fch3, fch4, fch5, fch6, fch7
      end subroutine read_aatsr_orbit
   end interface

   ! Fortran variables
   character(len=*),            intent(in)    :: l1b_file, drift_file
   type(imager_geolocation_t),  intent(inout) :: imager_geolocation
   type(imager_measurements_t), intent(inout) :: imager_measurements
   type(imager_angles_t),       intent(inout) :: imager_angles
   type(imager_flags_t),        intent(inout) :: imager_flags
   type(imager_time_t),         intent(inout) :: imager_time
   type(channel_info_t),        intent(in)    :: channel_info
   character(len=*),            intent(in)    :: sensor
   logical,                     intent(in)    :: verbose

   integer                        :: i, ii, j, jj, status
   integer(kind=byte)             :: view_selection
   real(kind=sreal), dimension(4) :: A
   type(aatsr_drift_lut_t)        :: lut
   real(kind=dreal)               :: new_drift, old_drift, drift_var

   ! C variables
   logical(kind=c_bool)                    :: verb, is_lut_drift_corrected
   integer(kind=c_short)                   :: nch, stat, temp
   integer(kind=c_long)                    :: nx, ny, startx, starty
   character(kind=c_char, len=path_length) :: l1b_file_c
   character(kind=c_char, len=30)          :: start_date
   character(kind=c_char, len=62)          :: gc1_file, vc1_file
   real(kind=c_double),   allocatable, dimension(:)   :: nday, fday
   integer(kind=c_short), allocatable, dimension(:)   :: ch, view
   integer(kind=c_short), allocatable, dimension(:,:) :: nflg, fflg
   integer(kind=c_short), allocatable, dimension(:,:) :: nqul, fqul
   type(c_ptr) :: lat, lon
   type(c_ptr) :: nsza, niza, nsaz, niaz, nraz
   type(c_ptr) :: nch1, nch2, nch3, nch4, nch5, nch6, nch7
   type(c_ptr) :: fsza, fiza, fsaz, fiaz, fraz
   type(c_ptr) :: fch1, fch2, fch3, fch4, fch5, fch6, fch7

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_aatsr_l1b()'

   ! initialise all the pointers
   lat  = c_null_ptr
   lon  = c_null_ptr
   nsza = c_null_ptr
   niza = c_null_ptr
   nsaz = c_null_ptr
   niaz = c_null_ptr
   nraz = c_null_ptr
   nch1 = c_null_ptr
   nch2 = c_null_ptr
   nch3 = c_null_ptr
   nch4 = c_null_ptr
   nch5 = c_null_ptr
   nch6 = c_null_ptr
   nch7 = c_null_ptr
   fsza = c_null_ptr
   fiza = c_null_ptr
   fsaz = c_null_ptr
   fiaz = c_null_ptr
   fraz = c_null_ptr
   fch1 = c_null_ptr
   fch2 = c_null_ptr
   fch3 = c_null_ptr
   fch4 = c_null_ptr
   fch5 = c_null_ptr
   fch6 = c_null_ptr
   fch7 = c_null_ptr

   ! copy function arguments into dummy variables
   nch = int(channel_info%nchannels_total, c_short)
   allocate(ch(nch))
   allocate(view(nch))
   ch = int(channel_info%channel_ids_instr, c_short)
   view = int(channel_info%channel_view_ids, c_short)

   nx = int(imager_geolocation%nx, c_long)
   startx = int(imager_geolocation%startx, c_long)
   ny = int(imager_geolocation%ny, c_long)
   starty = int(imager_geolocation%starty, c_long)
   stat = 0
   view_selection = 0
   verb = verbose
   if (any(view.eq.1)) view_selection = view_selection + 1
   if (any(view.eq.2)) view_selection = view_selection + 2

   ! assign geolocation and angle pointers
   lat = c_loc(imager_geolocation%latitude(startx,1))
   lon = c_loc(imager_geolocation%longitude(startx,1))
   if (iand(view_selection, 1_byte) .gt. 0) then
      allocate(nflg(startx:imager_geolocation%endx,1:ny))
      allocate(nqul(startx:imager_geolocation%endx,1:ny))
      allocate(nday(1:ny))
      nsza = c_loc(imager_angles%solzen(startx,1,1))
      niza = c_loc(imager_angles%satzen(startx,1,1))
      nsaz = c_loc(imager_angles%solazi(startx,1,1))
      niaz = c_loc(imager_angles%satazi(startx,1,1))
      nraz = c_loc(imager_angles%relazi(startx,1,1))
   else ! you have to pass something in Fortran
      allocate(nflg(1,1))
      allocate(nqul(1,1))
      allocate(nday(1))
   end if
   if (iand(view_selection, 2_byte) .gt. 0) then
      allocate(fflg(startx:imager_geolocation%endx,1:ny))
      allocate(fqul(startx:imager_geolocation%endx,1:ny))
      allocate(fday(1:ny))
      fsza = c_loc(imager_angles%solzen(startx,1,2))
      fiza = c_loc(imager_angles%satzen(startx,1,2))
      fsaz = c_loc(imager_angles%solazi(startx,1,2))
      fiaz = c_loc(imager_angles%satazi(startx,1,2))
      fraz = c_loc(imager_angles%relazi(startx,1,2))
   else
      allocate(fflg(1,1))
      allocate(fqul(1,1))
      allocate(fday(1))
   end if

   ! assign write pointers for required channels and views. Fortran pointers
   ! required for aatsr_apply_corrections as uncertain of last index
   do i = 1, nch
      if (view(i) .eq. 1) then
         select case (ch(i))
         case (1)
            nch1 = c_loc(imager_measurements%data(startx,1,i))
         case (2)
            nch2 = c_loc(imager_measurements%data(startx,1,i))
         case (3)
            nch3 = c_loc(imager_measurements%data(startx,1,i))
         case (4)
            nch4 = c_loc(imager_measurements%data(startx,1,i))
         case (5)
            nch5 = c_loc(imager_measurements%data(startx,1,i))
         case (6)
            nch6 = c_loc(imager_measurements%data(startx,1,i))
         case (7)
            nch7 = c_loc(imager_measurements%data(startx,1,i))
         case default
            write(*,*) 'ERROR: read_aatsr_l1b(): Channel ', ch(i), ', view ', &
                       view(i), ' not defined for AATSR.'
            stop error_stop_code
         end select
      else if (view(i) .eq. 2) then
         ch(i) = ch(i) - 7
         select case (ch(i))
         case (1)
            fch1 = c_loc(imager_measurements%data(startx,1,i))
         case (2)
            fch2 = c_loc(imager_measurements%data(startx,1,i))
         case (3)
            fch3 = c_loc(imager_measurements%data(startx,1,i))
         case (4)
            fch4 = c_loc(imager_measurements%data(startx,1,i))
         case (5)
            fch5 = c_loc(imager_measurements%data(startx,1,i))
         case (6)
            fch6 = c_loc(imager_measurements%data(startx,1,i))
         case (7)
            fch7 = c_loc(imager_measurements%data(startx,1,i))
         case default
            write(*,*) 'ERROR: read_aatsr_l1b(): Channel ', ch(i), ', view ', &
                       view(i), ' not defined for AATSR.'
            stop error_stop_code
         end select
      else
         write(*,*) 'ERROR: read_aatsr_l1b(): View ', view(i), ' not defined ' // &
                    'for AATSR.'
         stop error_stop_code
      end if
   end do

   ! read data using C routine (correct starts to zero offset)
   startx = startx-1
   starty = starty-1
   l1b_file_c = trim(l1b_file)//C_NULL_CHAR
   if (verbose) write(*,*) 'Calling C function READ_AATSR_ORBIT with file ', &
        trim(l1b_file)
   call read_aatsr_orbit(l1b_file_c, verb, nch, ch, view, &
        nx, ny, startx, starty, stat, lat, lon, &
        nsza, niza, nsaz, niaz, nraz, nflg, nqul, nday, &
        nch1, nch2, nch3, nch4, nch5, nch6, nch7, &
        fsza, fiza, fsaz, fiaz, fraz, fflg, fqul, fday, &
        fch1, fch2, fch3, fch4, fch5, fch6, fch7, &
        start_date, gc1_file, vc1_file, is_lut_drift_corrected)

   if (verbose) write(*,*) 'C function returned with status ', stat
   if (verbose) write(*,*) 'is_lut_drift_corrected: ', is_lut_drift_corrected

   ! temporary for now
   if (trim(adjustl(sensor)) .eq. 'ATSR2') then
      is_lut_drift_corrected = .true.
   end if

   if (.not. is_lut_drift_corrected) then
      if (verbose) write(*,*) 'calling read drift file ', stat
      call aatsr_read_drift_table(drift_file, lut, status)
      if (verbose) &
           write(*,*) 'finish drift table read returned with status ', stat
   end if

if (.not. is_lut_drift_corrected) then
   ! apply corrections
   if (verbose) write(*,*) 'apply calibration corrections'

   do i = 1, channel_info%nchannels_total
      j = int(ch(i))

      ! SW drift correction
      if (j.le.4) then
         ! AATSR L1B reflectances are stored as percentage values, so scale to
         ! the fractional value used by ORAC
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i)*0.01

         ! determine if non-linearity correction has been applied
         if (j .eq. 4 .and. gc1_file .eq. &
             'ATS_GC1_AXVIEC20020123_073430_20020101_000000_20200101_000000') then
            ! this correction acts on the voltage, which is -4.25*radiance
            A = pi/1.553 * (/ 1.0, -4.25, 18.0625, -76.765625 /) * &
                 (/ -0.000027, -0.1093, 0.009393, 0.001013 /)
            ! evaluate:
            ! pi*(A(1) + A(2)*volts + A(3)*volts**2 + A(4)*volts**3) / 1.553
            imager_measurements%data(:,:,i) = A(1) + &
                 imager_measurements%data(:,:,i)*(A(2) + &
                 imager_measurements%data(:,:,i)*(A(3) + &
                 imager_measurements%data(:,:,i)* A(4)))
         end if

         ! determine drift correction to remove from data
         if (.not. is_lut_drift_corrected .and. status.eq.0) then
            ! drift = old correction / new correction
            call aatsr_drift_correction(start_date, vc1_file, lut, j, &
                 new_drift, old_drift, drift_var)
            if (verbose) write(*,*) 'Corrections - new_drift:', new_drift, &
                 'old_drift:', old_drift, 'drift_var:', drift_var
            imager_measurements%data(:,:,i) = &
                 (old_drift/new_drift) * imager_measurements%data(:,:,i)
         end if
      end if ! j.le.4
   end do ! channel info%nchannels_total
end if ! drift corrected

   ! This correction need to be applied to AATSR version 2.1/3.0
   ! NB might need to remove this in future versions
   if (trim(adjustl(sensor)) .eq. 'AATSR') then
      do i = 1, channel_info%nchannels_total
         j = int(ch(i))
         ! 12um nonlinearity_correction
         if (j .eq. 7) then
            do ii = imager_geolocation%startx, imager_geolocation%endx
               do jj = 1, imager_geolocation%ny
                  imager_measurements%data(ii,jj,i) = &
                       imager_measurements%data(ii,jj,i) - &
                       aatsr_12um_nonlinearity_correction( &
                       imager_measurements%data(ii,jj,i))
               end do
            end do
         end if
      end do
   end if

if (is_lut_drift_corrected) then
   do i = 1, channel_info%nchannels_total
      j = int(ch(i))
      if (j.le.4) then
         ! AATSR L1B reflectances are stored as percentage values, so scale to
         ! the fractional value used by ORAC
         imager_measurements%data(:,:,i) = imager_measurements%data(:,:,i)*0.01
      end if
   end do
end if

   ! copy time values into rows from nadir (which we're presumably viewing) and
   ! translate to Julian time
   do i = 1, imager_geolocation%ny
      imager_time%time(:,i) = nday(i) + 2451544.5
   end do

   ! translate flags (THIS USED TO BE RATHER MORE COMPLICATED)
   if (iand(view_selection, 1_byte) .gt. 0_byte) then
      do i = imager_geolocation%startx, imager_geolocation%endx
         do j = 1, imager_geolocation%ny
            imager_flags%lsflag(i,j) = iand(nflg(i,j), 1_c_short)
            temp = iand(nflg(i,j), 2_c_short)
            if (temp .gt. 0) then
               imager_flags%cflag(i,j,1) = 1
            else
               imager_flags%cflag(i,j,1) = 0
            end if
         end do
      end do
   end if
   if (iand(view_selection, 2_byte) .gt. 0_byte) then
      do i = imager_geolocation%startx, imager_geolocation%endx
         do j = 1, imager_geolocation%ny
            temp = iand(fflg(i,j), 2_c_short)
            if (temp .gt. 0) then
               imager_flags%cflag(i,j,2) = 1
            else
               imager_flags%cflag(i,j,2) = 0
            end if
         end do
      end do
   end if

   if (verbose) write(*,*) 'finished read_aatsr_l1b deallocate'

   deallocate(ch)
   deallocate(view)
   deallocate(nflg)
   deallocate(nqul)
   deallocate(nday)
   deallocate(fflg)
   deallocate(fqul)
   deallocate(fday)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_aatsr_l1b()'

end subroutine read_aatsr_l1b
