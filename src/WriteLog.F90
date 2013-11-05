! Name:
!    Write_Log
!
! Purpose:
!    Writes out log and error information to the ECP log file.
!
! Description:
!    The function is passed a string containing a log message or error
!    message. The log file is opened, the string written to the file and
!    the log is closed again.
!
! Arguments:
!    Name       Type    In/Out/Both   Description
!    Ctrl       Struct  In            Control structure, contains log file name
!    message    string  In            Log or error message
!    status     int     In            status value associated with error 
!
! Algorithm:
!    Call FORTRAN library functions to get the system time and convert to ASCII
!    Call function to find a free logical unit number
!    Open log file named in Ctrl%FID%Log using LUN found above
!    Write system time to log file
!    Write message to log file
!    If status indicates error (i.e. non-zero), write status value to log file
!    Close log file
!
!
! Local variables:
!    Name       Type    Description
!    log_lun    int     Logical UnitNumber for log file
!    ios        int     I/O status from file operations
!    stime      int     FORTRAN lib function 
!    time       int     System time returned by stime function
!    ctime      char    FORTRAN lib function 
!    time_str   string  ASCII time string returned by ctime function
!        
! References:
!    UNIX man pages: man 3f ctime
!
! History:
!    3rd Oct 2000, Andy Smith : original version
!   23rd Feb 2011, Andy Smith:
!      Using record length to prevent annoying line breaks in middle of text.
!    8th Jun 2011, Andy Smith:
!      Fixed a typo in comment while trying temporary updates.  
! 2013 MJ turns off extensive writing to log file in order to accelerate code.

! Bugs:
!    None known.
!
!---------------------------------------------------------------------

subroutine Write_Log (Ctrl, message, status)

   use CTRL_def

   implicit none
   
!  argument declarations 
   type(CTRL_t), intent(in) :: Ctrl
   character(*), intent(in) :: message
   integer, intent(in)      :: status ! note intent IN unlike all other routines

!  Local variables
   integer     :: log_lun      ! Logical UnitNumber for log file
   integer     :: ios          ! I/O status from file operations
!   integer     :: stime, time  ! stime function, int to hold the result
   character(24)  :: ctime, time_str ! ctime function, string to hold the result
!   external ctime, time     ! Declare FORTRAN library functions as external
   character(8) :: date
   character(10) :: time

!  Generate a time string to precede the entry in the log file

!   stime = time()
!   time_str = ctime(stime)

   call Date_and_Time (date=date, time=time)
   time_str = date // ' ' // time(1:2) // ':' // time(3:4) // ':' // time(5:6)  

!  Open the log file and write the message, with status value if it is an 
!  error message.

   call Find_Lun(log_lun)
   open(unit=log_lun,      & 
        file=Ctrl%FID%Log, &
        status='old',      &
        position='append', &
        iostat=ios,recl=ECPlogReclen)
! AS extended recl to avoid annoying line breaks Feb 2011
        
   write(log_lun, '(/,a24)')time_str

   if (status == 0) then
      write(log_lun, *) message
   else
      write(log_lun, *) message
      write(log_lun, *) 'Status = ',status
   end if      
   close(unit=log_lun)   

end subroutine Write_Log
