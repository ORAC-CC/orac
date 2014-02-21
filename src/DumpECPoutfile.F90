! Name: DumpECPoutfile
!
! Program to dump some of the contents of an ECP output file to stdout. 
! Reads the ECP binary output file
! Outputs the retrieved state for each pixel. 
!
! History:
!    01-Jun-2011, Andy Smith:
!       Commented out read and write of lat, lon following update to main ECP source.
!       Using trim() on out file name. 
!
! $Id$
!

Program DumpECPoutfile

   use ECP_Constants
   use CTRL_def

   implicit none

   type(CTRL_t)     :: Ctrl
   integer          :: lun     ! Logical unit number of ECP output file to be read
   integer          :: ios=0   ! Status returned from file operations
   integer          :: status=0 ! Status from ReadDriver
   character(180)   :: message  ! Error message string returned by Read_Driver
   real             :: X(MaxStateVar)        
                            ! state vector values
   real             :: Sx(MaxStateVar)        
                            ! state vector errors (square root of diagonal)
   real             :: lat, lon
   integer          :: m    ! counter

!  Read Ctrl struct from driver file, to find name of output file to be read

   call Read_Driver(Ctrl, message, status)
   if (status /= 0) then
      write(*,*) message
   end if

   call find_lun(lun)

   write(*,*) trim(Ctrl%FID%Out)
   open(unit=lun, file=Ctrl%FID%Out, form='Unformatted', status='old', &
      iostat=ios, err=999)
   write(*,*)' file open'

! Read the Ctrl struct at the start of the file  
! AS, May 2011, skip this now. Removed Ctrl from out file since it now contains 
! pointers and can't be read in a single operation, plus its removal was planned anyway.
!   read(lun, iostat=ios, err=999) Ctrl 

!  Read each retrieved state variable in turn
!  The stored Sx is actually just the square root of each diagonal term
   write(*,*)' read X'
   do
!      read(lun, iostat=ios, err=999, end=998) lat, lon
      read(lun, iostat=ios, err=999, end=998) X, Sx
!      write(*,*) lat, lon
      write(*,*) X
   end do


999   if (ios /= 0) then
         write(*,*)' Error opening/reading file, ios ',ios
      end if
998 close(lun)

End program DumpECPoutfile
