!-------------------------------------------------------------------------------
! Name: aatsr_corrections.F90
!
! Purpose:
! Determines the shortwave long-term drift corrections to AATSR data using the
! look-up tables produced by Dave Smith (RAL Space).
! Note that all of the routines contained in this file are heavily based
! on Dave Smith's own drift correction IDL code.
!
! Description and Algorithm details:
! 1) Determine the date of this measurement from the string start_date.
! 2) Determine how the measurement was processed from the string vc1_file.
! 3) Determine if an exponential or thin film correction was applied to the
!    data and save that value to old_drift.
! 4) Interpolate the look-up table given in lut to the current measurement.
!    Return the required value in new_drift.
!
! Arguments:
! Name       Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! start_date string In   Start date/time string from L1B file
! vc1_table  string In   Name of the calibration file applied
! lut        struct In   Correction look-up table read by aatsr_read_drift_table
! chan       int    In   Channel number to consider
! new_drift  dreal  Out  Value by which radiances should be divided to account
!                        for detector drift
! old_drift  dreal  Out  Value by which radiances should be multiplied to remove
!                        any previous correction for detector drift
! drift_var  dreal  Out  The variance of the drift correction, which is needed
!                        to update the uncertainty estimate
!
! History:
! 2013/10/07, AP: Original
! 2013/10/11, GM: Fixed format specification for the read of seconds from the
!    string sdate in aatsr_corrections.F90: aatsr_corrections().
! 2013/10/11, GM: Changed the comparison lut%julday.eq.Tn to
!    lut%julday(1:lut%n).eq.Tn so that the comparison does go past the length of
!    the lut (lut%n) to the static size of lut%julday.
! 2014/01/27, MJ: data type corrections
! 2014/06/30, GM: Apply 12um nonlinearity brightness temperature correction.
! 2015/11/20, GM: Minor bug fix to real constants.  For example: 26_dreal
!    results in an 8 byte integer so that 26_dreal/60_dreal will be zero.  It
!    should be 26._dreal to get the intended result.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module aatsr_corrections_m

   use preproc_constants_m

   implicit none

   type aatsr_drift_lut_t
      integer(kind=lint)                  :: n
      integer(kind=sint), dimension(4000) :: year, month, day
      integer(kind=sint), dimension(4000) :: hour, minute, second
      real(kind=dreal), dimension(4000)   :: julday
      real(kind=sreal), dimension(4,4000) :: ch
      real(kind=sreal), dimension(4,4000) :: er
   end type aatsr_drift_lut_t

   ! Month strings used in the LUT files
   character(len=3), dimension(12), parameter :: monthname &
      = (/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
           'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /)

   ! Nonlinearity correction LUT
   integer, parameter :: N_NONLINEARITY_CORRECTION_LUT = 121

   real(sreal), dimension(N_NONLINEARITY_CORRECTION_LUT), parameter :: &
      nonlinearity_correction_T_scene = (/ &
         200.000, 201.000, 202.000, 203.000, 204.000, 205.000, 206.000, 207.000, &
         208.000, 209.000, 210.000, 211.000, 212.000, 213.000, 214.000, 215.000, &
         216.000, 217.000, 218.000, 219.000, 220.000, 221.000, 222.000, 223.000, &
         224.000, 225.000, 226.000, 227.000, 228.000, 229.000, 230.000, 231.000, &
         232.000, 233.000, 234.000, 235.000, 236.000, 237.000, 238.000, 239.000, &
         240.000, 241.000, 242.000, 243.000, 244.000, 245.000, 246.000, 247.000, &
         248.000, 249.000, 250.000, 251.000, 252.000, 253.000, 254.000, 255.000, &
         256.000, 257.000, 258.000, 259.000, 260.000, 261.000, 262.000, 263.000, &
         264.000, 265.000, 266.000, 267.000, 268.000, 269.000, 270.000, 271.000, &
         272.000, 273.000, 274.000, 275.000, 276.000, 277.000, 278.000, 279.000, &
         280.000, 281.000, 282.000, 283.000, 284.000, 285.000, 286.000, 287.000, &
         288.000, 289.000, 290.000, 291.000, 292.000, 293.000, 294.000, 295.000, &
         296.000, 297.000, 298.000, 299.000, 300.000, 301.000, 302.000, 303.000, &
         304.000, 305.000, 306.000, 307.000, 308.000, 309.000, 310.000, 311.000, &
         312.000, 313.000, 314.000, 315.000, 316.000, 317.000, 318.000, 319.000, &
         320.000/)

   real(sreal), dimension(N_NONLINEARITY_CORRECTION_LUT), parameter :: &
      nonlinearity_correction_delta_BT = (/ &
         0.654,  0.637,  0.621,  0.605,  0.588,  0.573,  0.557,  0.541, &
         0.526,  0.511,  0.496,  0.482,  0.467,  0.453,  0.439,  0.425, &
         0.411,  0.398,  0.385,  0.372,  0.359,  0.346,  0.334,  0.322, &
         0.310,  0.298,  0.286,  0.275,  0.264,  0.253,  0.242,  0.231, &
         0.221,  0.211,  0.201,  0.191,  0.181,  0.172,  0.163,  0.154, &
         0.145,  0.137,  0.128,  0.120,  0.112,  0.104,  0.097,  0.090, &
         0.082,  0.076,  0.069,  0.062,  0.056,  0.050,  0.044,  0.038, &
         0.033,  0.027,  0.022,  0.018,  0.013,  0.008,  0.004,  0.000, &
        -0.004, -0.008, -0.011, -0.014, -0.017, -0.020, -0.023, -0.025, &
        -0.027, -0.030, -0.031, -0.033, -0.034, -0.036, -0.037, -0.037, &
        -0.038, -0.038, -0.039, -0.039, -0.038, -0.038, -0.037, -0.037, &
        -0.036, -0.034, -0.033, -0.031, -0.030, -0.027, -0.025, -0.023, &
        -0.020, -0.017, -0.014, -0.011, -0.008, -0.004,  0.000,  0.004, &
         0.008,  0.013,  0.018,  0.022,  0.027,  0.033,  0.038,  0.044, &
         0.050,  0.056,  0.062,  0.069,  0.076,  0.082,  0.090,  0.097, &
         0.104/)

   real(sreal), dimension(N_NONLINEARITY_CORRECTION_LUT), parameter :: &
      nonlinearity_correction_u_delta_BT_2 = (/ &
         0.079,  0.077,  0.075,  0.073,  0.070,  0.068,  0.066,  0.064, &
         0.062,  0.061,  0.059,  0.057,  0.055,  0.053,  0.052,  0.050, &
         0.048,  0.047,  0.045,  0.044,  0.042,  0.041,  0.040,  0.038, &
         0.037,  0.036,  0.035,  0.034,  0.033,  0.032,  0.031,  0.030, &
         0.029,  0.028,  0.027,  0.027,  0.026,  0.026,  0.025,  0.025, &
         0.024,  0.024,  0.023,  0.023,  0.023,  0.022,  0.022,  0.022, &
         0.022,  0.022,  0.021,  0.021,  0.021,  0.021,  0.021,  0.021, &
         0.021,  0.021,  0.021,  0.021,  0.021,  0.020,  0.020,  0.020, &
         0.020,  0.020,  0.020,  0.020,  0.020,  0.020,  0.019,  0.019, &
         0.019,  0.019,  0.019,  0.019,  0.018,  0.018,  0.018,  0.018, &
         0.018,  0.017,  0.017,  0.017,  0.017,  0.017,  0.017,  0.017, &
         0.016,  0.016,  0.016,  0.016,  0.017,  0.017,  0.017,  0.017, &
         0.018,  0.018,  0.019,  0.019,  0.020,  0.020,  0.021,  0.022, &
         0.023,  0.024,  0.025,  0.026,  0.027,  0.028,  0.029,  0.031, &
         0.032,  0.033,  0.035,  0.036,  0.038,  0.039,  0.041,  0.043, &
         0.044/)

contains

subroutine aatsr_drift_correction(start_date, vc1_file, lut, chan, new_drift, &
     old_drift, drift_var)

   use calender_m

   implicit none

   character(len=*),        intent(in)  :: start_date
   character(len=*),        intent(in)  :: vc1_file
   type(aatsr_drift_lut_t), intent(in)  :: lut
   integer,                 intent(in)  :: chan
   real(dreal),             intent(out) :: new_drift
   real(dreal),             intent(out) :: old_drift
   real(dreal),             intent(out) :: drift_var

   character(len=30)  :: sdate
   real(kind=dreal)   :: T0, T1, T2, T3, T4, Tn, Tvc, dT, second
   integer(kind=sint) :: year, month, day, hour, minute
   integer(sint)      :: vc_year, vc_month, vc_day, vc_hour
   integer(sint)      :: vc_minute, vc_second
   integer(sint)      :: stat, ilow

   ! Yearly rates for exponential correction
   real(kind=sreal), parameter, dimension(4) :: K = &
        (/ 0.034, 0.021, 0.013, 0.002 /)
   ! Thin film drift model coefficients
   real(kind=sreal), dimension(3,2) :: A
   A(1,:)=(/ 0.083, 1.5868E-3 /)
   A(2,:)=(/ 0.056, 1.2374E-3 /)
   A(3,:)=(/ 0.041, 9.6111E-4 /)

   ! define various dates on which drift correction changed
   call GREG2JD(2002_sint, 3_sint,   1_sint, T0) ! Envisat launch date
   call GREG2JD(2005_sint, 11_sint, 29_sint, T1)
   T1 = T1 + (13._dreal + (20._dreal + 26._dreal/60._dreal)/60._dreal)/24._dreal
   call GREG2JD(2006_sint, 12_sint, 18_sint, T2)
   call GREG2JD(2010_sint,  4_sint,  4_sint, T3)
   call GREG2JD(2010_sint,  7_sint, 13_sint, T4)

   ! determine this record's date
   sdate = adjustl(start_date)
   read(sdate(8:11), '(I4)') year
   month=1_sint
   do while (monthname(month).ne.sdate(4:6))
      month=month+1
   end do
   read(sdate(1:2), '(I2)') day
   read(sdate(13:14), '(I2)') hour
   read(sdate(16:17), '(I2)') minute
   read(sdate(19:27), '(F9.6)') second
   call GREG2JD(year, month, day, Tn)
   Tn = Tn + (hour + (minute + second/60._dreal)/60._dreal)/24._dreal

   ! Check that the measurement lies within the time-span covered by the
   ! correction table
   if (Tn.lt.lut%julday(1)) then
      write(*,*) 'aatsr_corrections: WARNING: Acquisition before ' // &
           'start time of correction LUT. No correction made.', Tn, &
           lut%julday(1), lut%julday(lut%n)
      stat = -1
      return
   else if (Tn.gt.lut%julday(lut%n)) then
      write(*,*) 'aatsr_corrections: WARNING: Acquisition after end ' // &
           'time of correction LUT. No correction made.', Tn, &
           lut%julday(1), lut%julday(lut%n)
      stat = -1
      return
   end if

   ! Extract the date of the visual calibration file used in the L1B processing
   ! Used to determine which corrections were originally applied
   read(vc1_file(15:18), '(i4)') vc_year
   read(vc1_file(19:20), '(i2)') vc_month
   read(vc1_file(21:22), '(i2)') vc_day
   read(vc1_file(24:25), '(i2)') vc_hour
   read(vc1_file(26:27), '(i2)') vc_minute
   read(vc1_file(28:29), '(i2)') vc_second
   call GREG2JD(vc_year, vc_month, vc_day, Tvc)
   Tvc = Tvc + (vc_hour + (vc_minute + vc_second/60._dreal)/60._dreal)/24._dreal

   ! Identify which processing period we're in for the correction to REMOVE
   if ((Tvc.lt.T1) .or. ((Tvc.ge.T3) .and. (Tvc.lt.T4))) then
      ! No drift correction applied
      old_drift = 1.0
   else if ((chan.eq.4) .or. ((Tvc.ge.T1) .and. (Tvc.lt.T2))) then
      ! Exponential drift correction applied
      old_drift = exp(K(chan)*(Tn-T0)/365._dreal)
   else
      ! Thin film drift correction applied
      old_drift = sin(A(chan,2)*(Tn-T0))
      old_drift = 1.0 + A(chan,1)*old_drift*old_drift
   end if

   ! Check if we are, by some fluke, right on one of the LUT times
   if (any(lut%julday(1:lut%n).eq.Tn)) then
      ilow = 1
      do while (lut%julday(ilow).ne.Tn)
         ilow = ilow + 1
      end do ! assume it'll work because of any() above

      ! if so, add the drift correction recorded in that table
      new_drift = lut%ch(chan,ilow)
      drift_var = lut%er(chan,ilow)*lut%er(chan,ilow)
   else
      ! Find the index of the LUT time, just below the acquisition time
      ilow = count(lut%julday(1:lut%n).lt.Tn)

      ! interpolate the drift table to the current time
      dT = (Tn-lut%julday(ilow)) / (lut%julday(ilow+1)-lut%julday(ilow))
      new_drift = lut%ch(chan,ilow) + dT*(lut%ch(chan,ilow+1)-lut%ch(chan,ilow))
      ! this assumes that lut%julday are equally spaced
      drift_var = (lut%er(chan,ilow)*lut%er(chan,ilow) + &
           lut%er(chan,ilow+1)*lut%er(chan,ilow+1)) * dT*dT
   end if

end subroutine aatsr_drift_correction

!-------------------------------------------------------------------------------
! Name: aatsr_read_drift_table.F90
!
! Purpose:
! Reads the drift table LUT file produced by Dave Smith. Both files with
! and without uncertainty estimates are supported.
!
! Description and algorithm details
! 1) Ensure file exists.
! 2) Find available file unit and open file.
! 3) Skip the header, whose last line is identified by a #.
! 4) Read lines until a status error occurs. Parse the line using known
!    character positions. If the line is over 100 characters long, it contains
!    uncertainty information.
!
! Arguments:
! Name        Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! drift_table string In  Path to the correction look-up table
! lut         struct Out Correction look-up table read by aatsr_read_drift_table
! stat        sint  Out 0: Returned normally; Otherwise: Error
!
! History:
! 2013/10/07, AP: Original
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine aatsr_read_drift_table(drift_table, lut, stat)

   use calender_m
   use preproc_constants_m

   implicit none

   character(len=*),        intent(in)    :: drift_table
   integer,                 intent(out)   :: stat
   type(aatsr_drift_lut_t), intent(inout) :: lut

   integer            :: i, lun
   character(len=256) :: line
   logical            :: file_exists, no_lun, lun_exists, lun_used

   ! Does the drift correction LUT exist?
   inquire(file=drift_table, exist=file_exists)
   if (.not. file_exists) then
      stat = -1
      write(*,*) 'ERROR: aatsr_read_drift_table(): Could not find AATSR ' // &
           'drift correction LUT, filename: ', trim(drift_table)
      return
   end if

   ! Get a file descriptor...
   lun=19
   no_lun = .true.
   do while (no_lun)
      lun=lun+1
      inquire(unit=lun, exist=lun_exists, opened=lun_used)
      if (lun_exists .and. (.not. lun_used)) then
         no_lun = .false.
      else
         if (lun .ge. 100) then
            write(*,*) 'ERROR: aatsr_read_drift_table(): No LUN available. ' // &
                 'No correction applied!'
            stat = -2
            return
         end if
      end if
   end do

   ! Open the file...
   open(unit=lun, file=drift_table, iostat=stat)
   if (stat.ne.0) then
      write(*,*) 'ERROR: aatsr_read_drift_table(): Unable to open AATSR ' // &
           'drift correction LUT, filename: ', trim(drift_table)
      return
   end if

   ! Read through the header
   ! The last line of the header is marked with a "#" as its first character
   line = ' '
   do while (line(1:1).ne.'#' .and. stat.eq.0)
      read(lun, '(a200)', iostat=stat) line
      line = adjustl(line)
   end do

   ! Read data
   ! Assume a fixed format per line:
   ! Version with uncertainty estimates
   ! #         Date                   560nm                659nm                870nm               1600nm
   ! 0  01-MAR-2002 00:00:00   1.00000    0.01090   1.00000    0.01002   1.00000    0.00979   1.00000    0.01228
   ! Version without uncertainty estimates
   ! #         Date            0.56um    0.66um    0.87um     1.6um
   ! 0  01-MAR-2002 00:00:00   1.00000   1.00000   1.00000   1.00000
   i = 0
   do
      read(lun, '(a200)', iostat=stat) line
      if (stat /= 0) exit

      i=i+1

      ! Convert the day and time values into a Julian day:
      read(line(9:10), '(i2)') lut%day(i)
      lut%month(i)=1
      do while (monthname(lut%month(i)).ne.line(12:14))
         lut%month(i)=lut%month(i)+1
      end do
      read(line(16:19), '(i4)') lut%year(i)
      read(line(21:22), '(i2)') lut%hour(i)
      read(line(24:25), '(i2)') lut%minute(i)
      read(line(27:28), '(i2)') lut%second(i)
      call GREG2JD(lut%year(i), lut%month(i), lut%day(i), lut%julday(i))
      lut%julday(i) = lut%julday(i) + (real(lut%hour(i), dreal) + &
           (real(lut%minute(i), dreal) + (real(lut%second(i), dreal) &
           / 60._dreal))/60._dreal)/24._dreal

      ! There are two different formats of drift file: one includes
      ! uncertainties on the drift correction, one doesn't
      if (len(line).gt.100) then
         read(line(31:38),   '(f8.5)') lut%ch(1,i)
         read(line(52:59),   '(f8.5)') lut%ch(2,i)
         read(line(73:80),   '(f8.5)') lut%ch(3,i)
         read(line(94:101),  '(f8.5)') lut%ch(4,i)

         read(line(42:49),   '(f8.5)') lut%er(1,i)
         read(line(63:70),   '(f8.5)') lut%er(2,i)
         read(line(84:91),   '(f8.5)') lut%er(3,i)
         read(line(105:112), '(f8.5)') lut%er(4,i)
      else
         read(line(31:38),   '(f8.5)') lut%ch(1,i)
         read(line(41:48),   '(f8.5)') lut%ch(2,i)
         read(line(51:58),   '(f8.5)') lut%ch(3,i)
         read(line(60:68),   '(f8.5)') lut%ch(4,i)
         lut%er(1,i) = 0.0
         lut%er(2,i) = 0.0
         lut%er(3,i) = 0.0
         lut%er(4,i) = 0.0
      end if
   end do
   close(unit=lun)
   if (stat.ge.0) then
      write(*,*) 'ERROR: aatsr_read_drift_table(): Problem reading AATSR ' // &
           'drift correction LUT, filename: ', trim(drift_table), ', line: ', i
      stop error_stop_code
   else
      stat = 0 ! let's have a normal output if it works
   end if

   lut%n = i

end subroutine aatsr_read_drift_table

!-------------------------------------------------------------------------------
! Name: aatsr_12um_nonlinearity_correction
!
! Purpose:
! Return the AATSR 12um nonlinearity brightness temperature correction.
!
! Description and Algorithm details:
! Dave Smith: Empirical Nonlinearity Correction for 12um Channel, RAL Space
! AATSR Technical note, Doc No: PO-TN-RAL-AT-0562, Issue: 1.1, Date: 27-Feb-2014.
! Needs to be applied to version 2.1/3.0 of aatsr.
!
! Arguments:
! Name    Type  In/Out/Both Description
! ------------------------------------------------------------------------------
! T       dreal In          Original scene temperature
!
! Return value:
! Name    Type  Description
! delta_T dreal Brightness temperature correction
!
! History:
! 2014/06/16, GM: Original implementation.
! 2014/07/25, GM: Fixed out of bounds indexing bug.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

function aatsr_12um_nonlinearity_correction(T, u_delta_BT_2) result(delta_BT)

   implicit none

   real(kind=sreal), intent(in)            :: T
   real(kind=sreal)                        :: delta_BT
   real(kind=sreal), intent(out), optional :: u_delta_BT_2

   integer          :: i
   real(kind=sreal) :: a

   i = int(T - nonlinearity_correction_T_scene(1)) + 1
   i = max(i, 1)
   i = min(i, N_NONLINEARITY_CORRECTION_LUT - 1)

   a = (T - nonlinearity_correction_T_scene(i)) / &
       (nonlinearity_correction_T_scene(i + 1) - &
        nonlinearity_correction_T_scene(i))

   delta_BT = (1. - a) * nonlinearity_correction_delta_BT(i) + &
                    a  * nonlinearity_correction_delta_BT(i + 1)

   if (present(u_delta_BT_2)) then
      u_delta_BT_2 = (1. - a) * nonlinearity_correction_u_delta_BT_2(i) + &
                           a  * nonlinearity_correction_u_delta_BT_2(i + 1)
   end if

end function aatsr_12um_nonlinearity_correction

end module aatsr_corrections_m
