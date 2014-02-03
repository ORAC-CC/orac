! Name:
!    Read_Inst_Config
!
! Purpose:
!    Reads the instrument configuration file for the ECP.
!
! Description:
!    Reads in paramaters describing the instrument, number of channels,
!    image size etc from a configuration file.
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    Ctrl       Ctrl_t  Both           Control struct defined in CTRL_def
!    status     int     out            Status returned to calling function
!
! Algorithm:
!    Open inst config file (check and report errors)
!    Read parameters for the selected instrument (Ctrl%Inst%ID) 
!    Validate selected channels in Ctrl vs. parameters from config file   
!
! Local variables:
!    Name       Type    Description
!    c_lun      int     Unit number assigned to config file
!    ios        int     iostat value used for file open and read
!    NInst      int     No. of sets of instrument params in config file
!    InstID     int     Instrument identifier read from file
!    Name       char    String containing instrument name from file
!    NChans     int     No. of channels read from file
!    Functional int     Array of channel flags read from file
!    TotalFun   int     Sum of channels from Functional with flag value 1
!    i, j       int     Loop counters
!
! History:
!    22nd Aug 2000, Andy Smith : original version
!     9th Jan 2001, Andy Smith : 
!        Added setting of Ctrl variables Ctrl%Ind%XMax and YMax
!        Ctrl%Ind%Y renamed Y_Id
!    19th Jan 2001, Andy Smith : comments updated
!    14th Aug 2001, Andy Smith: 
!       Added error checking on user's selected image co-ordinate limits
!       vs. max image size.
!    21st Aug 2001, Andy Smith:
!       Bug fix to image co-ordinate error checking: more brackets needed in 
!       .and. and .or. clauses, plus X1, Y1 should always be checked regardless
!       of the warm start flag. Re-structured the if into 3 ifs in order to 
!       simplify the logic.
!    23rd Aug 2001, Andy Smith:
!       Added setting of Ctrl%Ind%NChans, number of instrument channels 
!       available in data. Required for reading of the Multi-Spectral Image 
!       file.
!   ****************************** ECV work starts here ************************
!    17th Feb 2011, Andy Smith:
!       Remove setting of Ctrl XMax, YMax. Now done from driver file to allow 
!       pre-processing to lower resolution rather than use of SPixel averaging.
!    22nd Feb 2011, Andy Smith:
!       Bug fix in X, Y limit checks. Y0 was checked against XMax, not YMax.
!    18th May 2011, Andy Smith:
!       Extension to multiple instrument views. Added number of available views
!       to instrument config file. The users's selected number of "channels" 
!       Ctrl%Ind%Ny may exceed the number of unique channels in the 
!       inst config file. Modified the limit check.
!   20/09/2012 CP added channel index to y_id value
! Bugs:
!    None known
!
! $Id$
!
!---------------------------------------------------------------------

subroutine Read_Inst_Config (Ctrl, status)

   use CTRL_def

   implicit none
   
!  argument declarations 
!  Note if the arguments are changed the interface definition in
!  SADRoutines.f90 must be updated to match.

   type(CTRL_t), intent(inout) :: Ctrl
   integer, intent(out)        :: status
   
!  Local variables   
   integer        :: c_lun      ! Unit number for config file
   integer        :: ios        ! Iostat value from file open, read etc.
   integer        :: NInst      ! No. of instruments described in config file
   integer        :: i, j       ! Loop counter
   integer        :: InstID     ! Instrument identifier read from file
   character(InstNameLen) :: Name 
                                ! Inst name read from file
   integer        :: XMax, YMax ! Max. no of pixels in x/y direction, read from
                                ! file
   integer        :: NViews     ! Number of viewing angles for inst (read from file)
   integer        :: NChans     ! Number of channels for inst (read from file)
   integer        :: Functional(MaxNumMeas) 
                                ! Array of functional channel flags 
   integer        :: TotalFun   ! Sum of Functional channels with flag value 1
   character(FilenameLen) :: Filename

   status = 0

!  open the instrument config file

   Filename = trim(Ctrl%SAD_Dir) // '/' // ICFileName
   call find_lun(c_lun)
   open(unit=c_lun, file=Filename, iostat=ios)
   if (ios /= 0) then
      status = ICFileOpenErr
      call Write_Log(Ctrl, 'Read_Inst_Config: Error opening file ', status)
   else
   
!     Open successful, read the file contents
   
      read(c_lun, *, err=999, iostat=ios)       ! Skip comment line
      read(c_lun, *, err=999, iostat=ios) NInst 
      
!     Check Inst ID in Ctrl struct vs. NInst value
      
      if (Ctrl%Inst%ID > NInst) then
	 status = InstIDInvalid          
         call Write_Log(Ctrl, &
            'Read_Inst_Config: Error reading inst config file ', status)
      end if      
      
      if (status == 0) then
         read(c_lun, *, err=999, iostat=ios)       ! Skip comment line
	 
!        Loop to read Inst parameters. Drops out when the Inst Id matches the
!        value in the Ctrl struct
	 
         do i=1, NInst
	    read(c_lun, *, err=999, iostat=ios) &
	       InstID, Name, NViews, NChans, XMax, YMax, (Functional(j), j=1,MaxNumMeas)
	    if (InstID == Ctrl%Inst%ID) exit 
	 end do
	 
!        Store no. of instrument channels and views available in Ctrl.

         Ctrl%Ind%NChans = NChans
         Ctrl%Ind%NInstViews = NViews

!        Check validity of Ctrl values.
	 
         TotalFun = 0
         do i=1, MaxNumMeas
	    if (Functional(i) == 1) TotalFun = TotalFun + 1	    
	 end do

!        Compare values read from inst config file with values in Ctrl struct
!        TotalFun > NChans checks the config file is not corrupted

!	 if (Ctrl%Ind%Ny > NChans .or. TotalFun > NChans) then
	 if (Ctrl%Ind%Ny > (NChans * NViews) .or. TotalFun > NChans) then
	    status = CtrlDataInvalid	    
            call Write_Log(Ctrl, &
	       'Read_Inst_Config: invalid data in Ctrl struct: ' // &
	       'requested no. of channels too great, ' // &
	       'requested pixel ranges incorrect, ' // &
	       'or error in Inst Config file channel data', &
	       status)
	 end if
	 
!        Check that requested channel indices are available

         if (status == 0) then
	    do i=1,Ctrl%Ind%Ny
	       if (status /=0) exit
	       if (Functional(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)) /= 1) then
		  write(*, *) 'Read_Inst_Config: invalid channel requested'
		  status = CtrlDataInvalid
                  call Write_Log(Ctrl, &
		     'Read_Inst_Config: invalid channel requested', status)
	       end if
	    end do
         end if	
	 
!        Set the XMax, YMax values in Ctrl using the values from the file.
!        (Removed 17th Feb 2011, AS. Now set in ReadDriver - this limit 
!        checking should move to ReadDriver too?)
!        Check user's selected start and end co-ordinates vs the image min
!        and max.

         if (status == 0) then
!	    Ctrl%Ind%XMax = XMax
!	    Ctrl%Ind%YMax = YMax
	    if (Ctrl%Ind%Ws == 1) then
	       if (Ctrl%Ind%Xstart < 1 .or. Ctrl%Ind%Xstart > Ctrl%Ind%XMax .or.&
	           Ctrl%Ind%Ystart < 1 .or. Ctrl%Ind%Ystart > Ctrl%Ind%YMax) &
        	  status = DriverFileDataErr
            end if
	       
            if (Ctrl%Ind%X0 < 1 .or. Ctrl%Ind%X0 > Ctrl%Ind%XMax .or. &
                Ctrl%Ind%Y0 < 1 .or. Ctrl%Ind%Y0 > Ctrl%Ind%YMax) &
	       status = DriverFileDataErr
            if (Ctrl%Ind%X1 < 1 .or. Ctrl%Ind%X1 > Ctrl%Ind%XMax .or. &
	        Ctrl%Ind%Y1 < 1 .or. Ctrl%Ind%Y1 > Ctrl%Ind%YMax) &
		  status = DriverFileDataErr
		
            if (status /= 0) then
	       call Write_Log(Ctrl, &
	          'Read_Inst_Config: selected co-ordinate outside image range',&
		   status)
	    end if	       
         end if	     	  
      end if  

999   if (ios /= 0) then
	 status = ICFileReadErr 
         call Write_Log(Ctrl, &
	    'Read_Inst_Config: Error reading inst config file ', status)
      end if
      
      close(unit=c_lun)
   end if    
   
end subroutine Read_Inst_Config
