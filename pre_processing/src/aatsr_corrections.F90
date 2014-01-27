! Name: aatsr_corrections.F90
!
! Purpose:
! Determines the shortwave long-term drift corrections to AATSR data using the
! look-up tables produced by Dave Smith (RAL Space).
! Note that all of the routines contained in this file are heavily based
! on Dave Smith's own drift correction IDL code.
!
! Description and algorithm details
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
! Local variables:
! Name Type Description
!
!
! History:
! 2013/10/07: AP Original
! 2013/10/11: GM Fixed format specification for the read of seconds from the
!                string sdate in aatsr_corrections.F90: aatsr_corrections().
! 2013/10/11: GM Changed the comparison lut%julday.eq.Tn to lut%julday(1:lut%n)
!                .eq.Tn so that the comparison does go past the length of the
!                lut (lut%n) to the static size of lut%julday.
!20140127:: MJ datatype corrections
!
! $Id$
!
! Bugs:
! none known
!

subroutine aatsr_corrections(start_date, vc1_file, lut, chan, new_drift, old_drift, drift_var)

   use aatsr_drift_structure

   implicit none

   character(len=28), intent(in) :: start_date
   character(len=28)             :: sdate
   character(len=62), intent(in) :: vc1_file
   type(aatsr_drift_lut)         :: lut
   real(dreal), intent(out)      :: new_drift, old_drift, drift_var
   real(kind=dreal)                   :: T0, T1, T2, T3, T4, Tn, Tvc, dT, second
   integer(kind=stint)                :: year, month, day, hour, minute
   integer(stint)                :: vc_year, vc_month, vc_day, vc_hour
   integer(stint)                :: vc_minute, vc_second
   integer(stint)                :: chan, stat, ilow

   ! Yearly rates for exponential correction
   real(kind=sreal), parameter, dimension(4) :: K = &
        (/ 0.034, 0.021, 0.013, 0.002 /) 
   ! Thin film drift model coefficients
   real(kind=sreal), dimension(3,2) :: A 
   A(1,:)=(/ 0.083, 1.5868E-3 /)
   A(2,:)=(/ 0.056, 1.2374E-3 /)
   A(3,:)=(/ 0.041, 9.6111E-4 /)

   ! define various dates on which drift correction changed
   call GREG2JD(2002_stint, 3_stint, 1_stint, T0) ! Envisat launch date
   call GREG2JD(2005_stint, 11_stint, 29_stint, T1)
   T1 = T1 + (13_dreal + (20_dreal + 26_dreal/60_dreal)/60_dreal)/24_dreal
   call GREG2JD(2006_stint, 12_stint, 18_stint, T2)
   call GREG2JD(2010_stint,  4_stint,  4_stint, T3)
   call GREG2JD(2010_stint,  7_stint, 13_stint, T4)

   ! determine this record's date
   sdate = adjustl(start_date)
   read(sdate(8:11), '(I4)') year
   month=1_stint
   do while (monthname(month).ne.sdate(4:6)) 
      month=month+1
   end do
   read(sdate(1:2), '(I2)') day
   read(sdate(13:14), '(I2)') hour
   read(sdate(16:17), '(I2)') minute
   read(sdate(19:), '(F9.6)') second
   call GREG2JD(year, month, day, Tn)
   Tn = Tn + (hour + (minute + second/60_dreal)/60_dreal)/24_dreal

   ! Check that the measurement lies within the time-span covered by the
   ! correction table
   if (Tn.lt.lut%julday(1)) then
      write(*,*) 'aatsr_corrections: WARNING: Aquisition before '// &
           & 'start time of correction LUT. No correction made.',Tn, &
           & lut%julday(1),lut%julday(lut%n)
      stat = -1
      return
   else if (Tn.gt.lut%julday(lut%n)) then
      write(*,*) 'aatsr_corrections: WARNING: Aquisition after end '// &
           & 'time of correction LUT. No correction made.',Tn, &
           & lut%julday(1),lut%julday(lut%n)
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
   Tvc = Tvc + (vc_hour + (vc_minute + vc_second/60_dreal)/60_dreal)/24_dreal

   ! Identify which processing period we're in for the correction to REMOVE
   if ((Tvc.lt.T1) .or. ((Tvc.ge.T3) .and. (Tvc.lt.T4))) then
      ! No drift correction applied
      old_drift = 1.0 
   else if ((chan.eq.4) .or. ((Tvc.ge.T1) .and. (Tvc.lt.T2))) then
      ! Exponential drift correction applied
      old_drift = exp(K(chan)*(Tn-T0)/365_dreal) 
   else
      ! Thin film drift correction applied
      old_drift = sin(A(chan,2)*(Tn-T0))
      old_drift = 1.0 + A(chan,1)*old_drift*old_drift
   end if

   ! Check if we are, by some fluke, right on one of the LUT times
   if (any(lut%julday(1:lut%n).eq.Tn)) then
      ilow = 1
      do while(lut%julday(ilow).ne.Tn)
         ilow = ilow + 1
      end do ! assume it'll work because of any() above

      ! if so, add the drift correction recorded in that table
      new_drift = lut%ch(chan,ilow)
      drift_var = lut%er(chan,ilow)*lut%er(chan,ilow)
   else
      ! Find the index of the LUT time, just below the aquisition time
      ilow = count(lut%julday(1:lut%n).lt.Tn)

      ! interpolate the drift table to the current time
      dT = (Tn-lut%julday(ilow)) / (lut%julday(ilow+1)-lut%julday(ilow))
      new_drift = lut%ch(chan,ilow) + dT*(lut%ch(chan,ilow+1)-lut%ch(chan,ilow))
      ! this assumes that lut%julday are equally spaced
      drift_var = (lut%er(chan,ilow)*lut%er(chan,ilow) + &
           lut%er(chan,ilow+1)*lut%er(chan,ilow+1)) * dT*dT
   end if
   
end subroutine aatsr_corrections

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
! stat        stint  Out 0: Returned normally; Otherwise: Error
!
! Local variables:
! Name Type Description
!
!
! History:
! 2013/10/07: AP Original
!
! Bugs:
! none known
!

subroutine aatsr_read_drift_table(drift_table, lut, stat)
   use preproc_constants
   use aatsr_drift_structure
   use date_type_structures

   implicit none

   integer                   :: stat, lun, i
   character(len=pathlength) :: drift_table
   character(len=200)        :: line
   type(aatsr_drift_lut)     :: lut
   logical                   :: file_exists, no_lun, lun_exists, lun_used

   ! Does the drift correction LUT exist?
   inquire(file=drift_table, exist=file_exists)
   if (.not. file_exists) then
      stat = -1
      write(*,*) 'aatsr_read_drift_table: WARNING: Could not find AATSR '// &
           & 'drift correction LUT. No correction applied!'
      write(*,*) '   Filename: ',trim(drift_table)
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
            write(*,*) 'aatsr_read_drift_table: No LUN available. No '// &
                 & 'correction applied!'
            stat = -2
            return
         end if
      endif
   end do

   ! Open the file...
   open(unit=lun, file=drift_table, iostat=stat)
   if (stat.ne.0) then
      write(*,*) 'aatsr_read_drift_table: WARNING: Unable to open AATSR '// &
           & 'drift correction LUT. No correction applied!'
      write(*,*) '   Filename: ',trim(drift_table)
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
      lut%julday(i) = lut%julday(i) + (real(lut%hour(i),dreal) + &
           (real(lut%minute(i),dreal) + (real(lut%second(i),dreal) &
           / 60_dreal))/60_dreal)/24_dreal
      
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
         read(line(31:38), '(f8.5)') lut%ch(1,i)
         read(line(41:48), '(f8.5)') lut%ch(2,i)
         read(line(51:58), '(f8.5)') lut%ch(3,i)
         read(line(60:68), '(f8.5)') lut%ch(4,i)
         lut%er(1,i) = 0.0
         lut%er(2,i) = 0.0
         lut%er(3,i) = 0.0
         lut%er(4,i) = 0.0
      end if
   end do
   close(unit=lun)
   if (stat.ge.0) then
      write(*,'(A, i4)') 'aatsr_read_drift_table: WARNING: Error reading '// &
           & 'AATSR drift correction LUT at line ',i
      write(*,*) '   Filename: ',trim(drift_table)
   else
      stat = 0 ! let's have a normal output if it works
   end if

   lut%n = i
   
end subroutine aatsr_read_drift_table
