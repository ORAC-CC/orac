! Read_LUT. This file contains all the Read_LUT_xxx subroutines as well as
! the Read_LUT routine that calls them. Each routine has it's own header
! comment section.


! Name:
!    Read_LUT_Em
!
! Purpose:
!    Reads Look Up Table values for emissivity from LUT files 
!    into SAD_LUT struct.
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    Ctrl       struct  In           Control structure passed to Write_Log for
!                                    log file name
!    l_lun      int     in           Unit number for LUT file
!    LUT_file   string  in           LUT file name
!    chan       int     in           Current channel number used for array
!                                    indexing within the SAD_LUT struct, e.g.
!                                    SAD_LUT%Wavelength.
!                                    for checking against LUT file data.
!    SAD_LUT    struct  out          Structure to hold the values from the LUT
!                                    file. N.B. this is one struct from the
!                                    array used elsewhere, not the whole array.
!    status     int     out          Status value returned by all ECP routines.
!
! Algorithm:
!    Open the LUT file specified by LUT_file, using unit l_lun
!    (if error, report error)
!    read the wavelength value into SAD_LUT%Wavelength(chan)
!    for each of the Grid parameters Tau, Satzen, Re:
!       read in the number of values
!       read the specified number of values into SAD_LUT%Grid%<array>
!       use the values read in to set the min and max for the parameter
!   (Note since Grid is not an array, it's values are overwritten by the
!   values from successive LUT files)
!
!   read in the Em array
!
!   if an error was found when checking no of grid parameter values 
!      report the error
!    
!
!   Note that this subroutine is passed one of type SAD_LUT, rather than the whole arrays used above. 
!
! Local variables:
!    Name       Type    Description
!    ios        int     I/O status from file operations
!    message    string  Message to be o/p by Write_Log function
!    nVals      int     No. of parameter values to read, from LUT file
!    i,j,k,l,m  int     Counters
!
! History:
!    13th Oct 2000, Andy Smith : original version
!    11th June 2011, Caroline Poulsen : major rewrite, remove refrences to cloud class
!                     changed the way arrays were allocated, removed
!                      references 
!    13th December Caroline Poulsen : checked if array are aloocated
!                      with associated command before allocating them.
!    20/09/2011 CP added channel index to y_id value
!    22/03/2013 Gareth Thomas : Added trim() to LUT_file in write(message,*)
!                      statements (called on I/O error). Also added write(*,*)
!                      statements for I/O errors
! 20131114 MJ corrected zeroing of satzen when ref is allocated, some cleanup
! Bugs:
!    None known
!
! $Id: ReadLUT.f90 74 2011-08-16 16:11:53Z capoulse $
!
!---------------------------------------------------------------------

Subroutine Read_LUT_Em (Ctrl, l_lun, LUT_file, chan, &
   SAD_LUT, status)

   use ECP_Constants
   use Ctrl_def
    use SAD_LUT_def
   
   implicit none
   
!  argument declarations
!  Note that  SAD_LUT are arrays of structs in the calling
!  routine, but scalar structs here. 

   
   type(CTRL_t), intent(in)        :: Ctrl
   integer, intent(in)             :: l_lun   !    Unit number for LUT file
   character(*)                    :: LUT_file!    Name of LUT file
   integer                         :: chan    !    Current channel number
   type(SAD_LUT_t), intent(inout)  :: SAD_LUT !    Single structs from the array
                                              !    used in the main program
   integer, intent(inout)            :: status
   
!  Local variables
   
   integer        :: ios     !    I/O status from file operations
   character(180) :: message !    Error message to pass to Write_Log
   integer        :: nVals   !    No. of Tau/Re/Satzen etc values in file
   integer        :: i,j,k,l,m  ! Loop counters



   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)	    
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTEm: Error opening file ', trim(LUT_file) 
      write(message, *) 'Read_LUTEm: Error opening file ', trim(LUT_file) 
      call Write_Log(Ctrl, trim(message), status)
   else
      

      !     Read the file contents into the SAD_LUT structure
      read(l_lun, *, err=999, iostat=ios) SAD_LUT%Wavelength(chan)
      read(l_lun, *, err=999, iostat=ios) nVals, SAD_LUT%Grid%dTau

      !Used for loop control later
      SAD_LUT%Grid%nTau = nVals               


      !Read in Tau values and set min and max
      if (chan == Ctrl%Ind%ThermalFirst) then
         if (.not.associated(SAD_LUT%Grid%tau)) allocate(SAD_LUT%Grid%tau(SAD_LUT%Grid%nTau) )
         SAD_LUT%Grid%tau=0.00
      end if
         
      read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(i), i=1,nVals)

      SAD_LUT%Grid%MinTau = SAD_LUT%Grid%Tau(1)
      SAD_LUT%Grid%MaxTau = SAD_LUT%Grid%Tau(nVals)


      !Now get the satzen values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen
         SAD_LUT%Grid%nSatzen = nVals
         
         !Read in values and set min and max
         if (chan == Ctrl%Ind%ThermalFirst) then 
            if (.not.associated(SAD_LUT%Grid%satzen)) allocate(SAD_LUT%Grid%satzen(SAD_LUT%Grid%nsatzen) )
            SAD_LUT%Grid%satzen=0.00
         end if
         read(l_lun, *, err=999, iostat=ios) &
              & (SAD_LUT%Grid%Satzen(i), i=1,nVals)
         SAD_LUT%Grid%MinSatzen = SAD_LUT%Grid%Satzen(1)
         SAD_LUT%Grid%MaxSatzen = SAD_LUT%Grid%Satzen(nVals)	    
         
      end if

      !Now get the effective radius values
      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe
	 SAD_LUT%Grid%nRe = nVals
         
         !           Read in values and set min and max
         if (chan == Ctrl%Ind%ThermalFirst) then 
            if (.not.associated(SAD_LUT%Grid%Re)) allocate(SAD_LUT%Grid%Re(SAD_LUT%Grid%nre) )
            !MJ ORG SAD_LUT%Grid%satzen=0.00
            SAD_LUT%Grid%Re=0.00
         end if
         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(i), i=1,nVals)
         SAD_LUT%Grid%MinRe = SAD_LUT%Grid%Re(1)
         SAD_LUT%Grid%MaxRe = SAD_LUT%Grid%Re(nVals)	    
         
      end if

      !Read in the Em array 
      if (Ctrl%Ind%NThermal > 0 .and. chan == Ctrl%Ind%ThermalFirst) then
         allocate(SAD_LUT%Em(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%NSatzen, &
              & SAD_LUT%Grid%nre))
         SAD_LUT%Em=0.00
      end if

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
              & (((SAD_LUT%Em(chan, i, j, k), i=1, SAD_LUT%Grid%nTau), &
              & j=1, SAD_LUT%Grid%nSatzen), k=1, SAD_LUT%Grid%nRe)
      end if

      close(unit=l_lun)
   end if
   
999 if (ios /= 0) then
      status = LUTFileReadErr 
      write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
   end if
 end Subroutine Read_LUT_Em


! Name:
!    Read_LUT_Rbd
!
! Purpose:
!    Reads Look Up Table values for bi-directional reflectance from LUT files 
!    into SAD_LUT struct.
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    Ctrl       struct  In           Control structure passed to Write_Log for
!                                    log file name
!    l_lun      int     in           Unit number for LUT file
!    LUT_file   string  in           LUT file name
!    chan       int     in           Current channel number used for array
!                                    indexing within the SAD_LUT struct, e.g.
!                                    SAD_LUT%Wavelength.
!    SAD_LUT    struct  out          Structure to hold the values from the LUT
!                                    file. N.B. this is one struct from the
!                                    array used elsewhere, not the whole array.
!    status     int     out          Status value returned by all ECP routines.
!
! Algorithm:
!    Open the LUT file specified by LUT_file, using unit l_lun
!    (if error, report error)
!    read the wavelength value into SAD_LUT%Wavelength(chan)
!    for each of the Grid parameters Tau, Satzen, Solzen, Relazi, Re:
!       read in the number of values
!       read the specified number of values into SAD_LUT%Grid%<array>
!       use the values read in to set the min and max for the parameter
!   (Note since Grid is not an array, it's values are overwritten by the
!   values from successive LUT files)
!
!   read in the Rbd array
!
!   if an error was found when checking no of grid parameter values 
!      report the error
!    
!
!   Note that this subroutine is passed a single struct of type SAD_LUT, rather than the whole arrays used above. 
!
! Local variables:
!    Name       Type    Description
!    ios        int     I/O status from file operations
!    message    string  Message to be o/p by Write_Log function
!    nVals      int     No. of parameter values to read, from LUT file
!    i,j,k,l,m  int     Counters
!
! History:
!    6th Oct 2000, Andy Smith : original version
!    17th Apr 2013, Caroline Poulsen : fixed bug in which nsolzen specified
!       before nsatzen in rbd read command
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

Subroutine Read_LUT_Rbd (Ctrl, l_lun, LUT_file, chan, &
   SAD_LUT, status)

   use ECP_Constants
   use Ctrl_def
   use SAD_LUT_def
   
   implicit none
   
!  argument declarations
!  Note that SAD_LUT are arrays of structs in the calling
!  routine, but scalar structs here. 
   
   type(CTRL_t), intent(in)        :: Ctrl
   integer, intent(in)             :: l_lun   !    Unit number for LUT file
   character(*)                    :: LUT_file!    Name of LUT file
   integer                         :: chan    !    Current channel number
    type(SAD_LUT_t), intent(inout)  :: SAD_LUT !    Single structs from the array
                                              !    used in the main program
   integer, intent(inout)            :: status
   
!  Local variables
   
   integer        :: ios     !    I/O status from file operations
   character(180) :: message !    Error message to pass to Write_Log
   integer        :: nVals   !    No. of Tau/Re/Satzen etc values in file
   integer        :: i,j,k,l,m  ! Loop counters


   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)	    
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTRbd: Error opening file ', trim(LUT_file) 
      write(message, *) 'Read_LUTRbd: Error opening file ', trim(LUT_file) 
      call Write_Log(Ctrl, trim(message), status)
   else
!     Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau
      SAD_LUT%Grid%nTau = nVals               !    Used for loop control later

    
!        Read in Tau values and set min and max

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(i), i=1,nVals)
	 SAD_LUT%Grid%MinTau = SAD_LUT%Grid%Tau(1)
	 SAD_LUT%Grid%MaxTau = SAD_LUT%Grid%Tau(nVals)
   

!     Now get the satzen values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen
	 SAD_LUT%Grid%nSatzen = nVals


         if (chan == Ctrl%Ind%SolarFirst) then 
             if (.not.associated(SAD_LUT%Grid%Satzen)) allocate(SAD_LUT%Grid%Satzen(SAD_LUT%Grid%nsatzen) )
             SAD_LUT%Grid%Satzen=0.00
             end if

!           Read in values and set min and max
        
            read(l_lun, *, err=999, iostat=ios) &
	       (SAD_LUT%Grid%Satzen(i), i=1,nVals)
	    SAD_LUT%Grid%MinSatzen = SAD_LUT%Grid%Satzen(1)
	    SAD_LUT%Grid%MaxSatzen = SAD_LUT%Grid%Satzen(nVals)	   
      end if
      
!     Now get the Solzen values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSolzen
	 SAD_LUT%Grid%nSolzen = nVals
          !           Read in values and set min and max
         if (chan == Ctrl%Ind%SolarFirst) then 
              if (.not.associated(SAD_LUT%Grid%Solzen)) allocate(SAD_LUT%Grid%Solzen(SAD_LUT%Grid%nsolzen))
              SAD_LUT%Grid%Solzen=0.00
             end if
         read(l_lun, *, err=999, iostat=ios) &
              (SAD_LUT%Grid%Solzen(i), i=1,nVals)
         SAD_LUT%Grid%MinSolzen = SAD_LUT%Grid%Solzen(1)
         SAD_LUT%Grid%MaxSolzen = SAD_LUT%Grid%Solzen(nVals)	    
	 
      end if
      
!     Now get the rel. azi. values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRelazi
	 SAD_LUT%Grid%nRelazi = nVals
 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
               if (.not.associated(SAD_LUT%Grid%Relazi)) allocate(SAD_LUT%Grid%Relazi(SAD_LUT%Grid%nrelazi) )
               SAD_LUT%Grid%Relazi=0.00
               end if

            read(l_lun, *, err=999, iostat=ios)  &
	       (SAD_LUT%Grid%Relazi(i), i=1,nVals)
	    SAD_LUT%Grid%MinRelazi = SAD_LUT%Grid%Relazi(1)
	    SAD_LUT%Grid%MaxRelazi = SAD_LUT%Grid%Relazi(nVals)	    
      end if
      
!     Now get the effective radius values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe
	 SAD_LUT%Grid%nRe = nVals
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                 if (.not.associated(SAD_LUT%Grid%re)) allocate(SAD_LUT%Grid%Re(SAD_LUT%Grid%nre) )
                 SAD_LUT%Grid%Re=0.00
               end if

            read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(i), i=1,nVals)
	    SAD_LUT%Grid%MinRe = SAD_LUT%Grid%Re(1)
	    SAD_LUT%Grid%MaxRe = SAD_LUT%Grid%Re(nVals)	    
	 
      end if
  
!     Read in the RBD array.
      if (Ctrl%Ind%NSolar > 0 .and. chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Rbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%NSatzen, &
              SAD_LUT%Grid%NSolzen, SAD_LUT%Grid%nrelazi, SAD_LUT%Grid%nre))
         SAD_LUT%Rbd=0.00
      end if
      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
	    (((((SAD_LUT%Rbd(chan, i, j, k, l, m), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nSatzen), k=1, SAD_LUT%Grid%nSolzen),     &
	    l=1, SAD_LUT%Grid%nRelazi), m=1, SAD_LUT%Grid%nRe)
      end if

      close(unit=l_lun)
   end if

999   if (ios /= 0) then
	 status = LUTFileReadErr 
	 write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 call Write_Log(Ctrl, trim(message), status)
      end if
end Subroutine Read_LUT_RBD 


! Name:
!    Read_LUT_Rd
!
! Purpose:
!    Reads Look Up Table values for diffuse reflectance from LUT files 
!    into SAD_LUT struct.
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    Ctrl       struct  In           Control structure passed to Write_Log for
!                                    log file name
!    l_lun      int     in           Unit number for LUT file
!    LUT_file   string  in           LUT file name
!    chan       int     in           Current channel number used for array
!                                    indexing within the SAD_LUT struct, e.g.
!                                    SAD_LUT%Wavelength.
!    SAD_LUT    struct  out          Structure to hold the values from the LUT
!                                    file. N.B. this is one struct from the
!                                    array used elsewhere, not the whole array.
!    status     int     out          Status value returned by all ECP routines.
!
! Algorithm:
!    Open the LUT file specified by LUT_file, using unit l_lun
!    (if error, report error)
!    read the wavelength value into SAD_LUT%Wavelength(chan)
!    for each of the Grid parameters Tau, Satzen, Solzen, Relazi, Re:
!       read in the number of values
!       read the specified number of values into SAD_LUT%Grid%<array>
!       use the values read in to set the min and max for the parameter
!   (Note since Grid is not an array, it's values are overwritten by the
!   values from successive LUT files)
!
!   read in the Rd array
!   read in the RFd array
!
!   if an error was found when checking no of grid parameter values 
!      report the error
!    
!
!   Note that this subroutine is passed a single struct 
!   of type SAD_LUT, rather than the whole arrays used above. 
!
! Local variables:
!    Name       Type    Description
!    ios        int     I/O status from file operations
!    message    string  Message to be o/p by Write_Log function
!    nVals      int     No. of parameter values to read, from LUT file
!    i,j,k,l,m  int     Counters
!
! History:
!    13th Oct 2000, Andy Smith : original version
!    22/03/2013 Gareth Thomas : Added trim() to LUT_file in write(message,*)
!                      statements (called on I/O error). Also added write(*,*)
!                      statements for I/O errors
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

Subroutine Read_LUT_Rd (Ctrl, l_lun, LUT_file, chan, &
   SAD_LUT, status)

   use ECP_Constants
   use Ctrl_def
    use SAD_LUT_def
   
   implicit none
   
!  argument declarations
!  Note  SAD_LUT are arrays of structs in the calling
!  routine, but scalar structs here. 
   
   type(CTRL_t), intent(in)        :: Ctrl
   integer, intent(in)             :: l_lun   !    Unit number for LUT file
   character(*)                    :: LUT_file!    Name of LUT file
   integer                         :: chan    !    Current channel number
    type(SAD_LUT_t), intent(inout)  :: SAD_LUT !    Single structs from the array
                                              !    used in the main program
   integer, intent(inout)            :: status
   
!  Local variables
   
   integer        :: ios     !    I/O status from file operations
   character(180) :: message !    Error message to pass to Write_Log
   integer        :: nVals   !    No. of Tau/Re/Satzen etc values in file
   integer        :: i,j,k,l,m  ! Loop counters


   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)	    

   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTRd: Error opening file ', trim(LUT_file)
      write(message, *) 'Read_LUTRd: Error opening file ', trim(LUT_file)
      call Write_Log(Ctrl, trim(message), status)
   else
!     Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)

      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau

      SAD_LUT%Grid%nTau = nVals              !    Used for loop control later

!        Read in Tau values and set min and max
         if (chan == Ctrl%Ind%SolarFirst) then 
             if (.not.associated(SAD_LUT%Grid%tau)) allocate(SAD_LUT%Grid%tau(SAD_LUT%Grid%nTau) )
             SAD_LUT%Grid%tau=0.00
            end if

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(i), i=1,nVals)
	 SAD_LUT%Grid%MinTau = SAD_LUT%Grid%Tau(1)
	 SAD_LUT%Grid%MaxTau = SAD_LUT%Grid%Tau(nVals)


!     Now get the satzen values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen
	 SAD_LUT%Grid%nSatzen = nVals

!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%Satzen)) allocate(SAD_LUT%Grid%Satzen(SAD_LUT%Grid%nsatzen) )
                SAD_LUT%Grid%Satzen=0.00
               end if
            read(l_lun, *, err=999, iostat=ios) &
	       (SAD_LUT%Grid%Satzen(i), i=1,nVals)
	    SAD_LUT%Grid%MinSatzen = SAD_LUT%Grid%Satzen(1)
	    SAD_LUT%Grid%MaxSatzen = SAD_LUT%Grid%Satzen(nVals)	    
	 
      end if

!     Now get the effective radius values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe
	 SAD_LUT%Grid%nRe = nVals

!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                 if (.not.associated(SAD_LUT%Grid%Re)) allocate(SAD_LUT%Grid%Re(SAD_LUT%Grid%nre) )
                 SAD_LUT%Grid%Re=0.00
               end if
            read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(i), i=1,nVals)

	    SAD_LUT%Grid%MinRe = SAD_LUT%Grid%Re(1)
	    SAD_LUT%Grid%MaxRe = SAD_LUT%Grid%Re(nVals)	    

      end if
!HERE
!     Read in the Rd array 

      
      if (chan == Ctrl%Ind%SolarFirst) then 
         allocate(SAD_LUT%Rd(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%NSatzen, SAD_LUT%Grid%nre))
         SAD_LUT%Rd=0.00
         end if

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios) &
	    (((SAD_LUT%Rd(chan, i, j, k), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nSatzen), k=1, SAD_LUT%Grid%nRe)
      end if

!     Read in the RFd array.

      if (chan == Ctrl%Ind%SolarFirst) then 
         allocate(SAD_LUT%Rfd(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%nre))
         SAD_LUT%Rfd=0.00
         end if

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
	    ((SAD_LUT%RFd(chan, i, j), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nRe)
      end if


      close(unit=l_lun)
   end if

999   if (ios /= 0) then
	 status = LUTFileReadErr 
	 write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 call Write_Log(Ctrl, trim(message), status)
      end if

end Subroutine Read_LUT_Rd 


! Name:
!    Read_LUT_Tb
!
! Purpose:
!    Reads Look Up Table values for direct part of beam transmission from LUT 
!    files into SAD_LUT struct.
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    Ctrl       struct  In           Control structure passed to Write_Log for
!                                    log file name
!    l_lun      int     in           Unit number for LUT file
!    LUT_file   string  in           LUT file name
!    chan       int     in           Current channel number used for array
!                                    indexing within the SAD_LUT struct, e.g.
!                                    SAD_LUT%Wavelength.
!    SAD_LUT    struct  out          Structure to hold the values from the LUT
!                                    file. N.B. this is one struct from the
!                                    array used elsewhere, not the whole array.
!    status     int     out          Status value returned by all ECP routines.
!
! Algorithm:
!    Open the LUT file specified by LUT_file, using unit l_lun
!    (if error, report error)
!    read the wavelength value into SAD_LUT%Wavelength(chan)
!    for each of the Grid parameters Tau, Satzen, Solzen, Relazi, Re:
!       read in the number of values
!       read the specified number of values into SAD_LUT%Grid%<array>
!       use the values read in to set the min and max for the parameter
!   (Note since Grid is not an array, it's values are overwritten by the
!   values from successive LUT files)
!
!   read in the Tb array
!
!   if an error was found when checking no of grid parameter values 
!      report the error
!    
!
!   Note that this subroutine is passed a single struct of
!   type SAD_LUT, rather than the whole arrays used above. 
!
! Local variables:
!    Name       Type    Description
!    ios        int     I/O status from file operations
!    message    string  Message to be o/p by Write_Log function
!    nVals      int     No. of parameter values to read, from LUT file
!    i,j,k      int     Counters
!
! History:
!    6th Oct 2000, Andy Smith : original version
!    22/03/2013 Gareth Thomas : Added trim() to LUT_file in write(message,*)
!                      statements (called on I/O error). Also added write(*,*)
!                      statements for I/O errors
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

Subroutine Read_LUT_Tb (Ctrl, l_lun, LUT_file, chan, &
   SAD_LUT, status)

   use ECP_Constants
   use Ctrl_def
    use SAD_LUT_def
   
   implicit none
   
!  argument declarations
!  Note  SAD_LUT are arrays of structs in the calling
!  routine, but scalar structs here. 
   
   type(CTRL_t), intent(in)        :: Ctrl
   integer, intent(in)             :: l_lun   !    Unit number for LUT file
   character(*)                    :: LUT_file!    Name of LUT file
   integer                         :: chan    !    Current channel number
    type(SAD_LUT_t), intent(inout)  :: SAD_LUT !    Single structs from the array
                                              !    used in the main program
   integer, intent(inout)            :: status
   
!  Local variables
   
   integer        :: ios     !    I/O status from file operations
   character(180) :: message !    Error message to pass to Write_Log
   integer        :: nVals   !    No. of Tau/Re/Satzen etc values in file
   integer        :: i,j,k   !    Loop counters


   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)	    
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTTb: Error opening file ', trim(LUT_file) 
      write(message, *) 'Read_LUTTb: Error opening file ', trim(LUT_file) 
      call Write_Log(Ctrl, trim(message), status)
   else
!     Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau
      SAD_LUT%Grid%nTau = nVals                !    Used for loop control later

     
      !        Read in Tau values and set min and max
      if (chan == Ctrl%Ind%SolarFirst) then 
          if (.not.associated(SAD_LUT%Grid%tau)) allocate(SAD_LUT%Grid%Tau(SAD_LUT%Grid%ntau) )
          SAD_LUT%Grid%tau=0.00
         end if

     read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(i), i=1,nVals)
      SAD_LUT%Grid%MinTau = SAD_LUT%Grid%Tau(1)
      SAD_LUT%Grid%MaxTau = SAD_LUT%Grid%Tau(nVals)
      
 
!     Now get the Solzen values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSolzen
	 SAD_LUT%Grid%nSolzen = nVals
	 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%Solzen)) allocate(SAD_LUT%Grid%Solzen(SAD_LUT%Grid%nsolzen) )
                SAD_LUT%Grid%Solzen=0.00
            end if
            read(l_lun, *, err=999, iostat=ios) &
	       (SAD_LUT%Grid%Solzen(i), i=1,nVals)
	    SAD_LUT%Grid%MinSolzen = SAD_LUT%Grid%Solzen(1)
	    SAD_LUT%Grid%MaxSolzen = SAD_LUT%Grid%Solzen(nVals)	    
	
      end if
      
!     Now get the effective radius values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe
	 SAD_LUT%Grid%nRe = nVals
	 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%re)) allocate(SAD_LUT%Grid%Re(SAD_LUT%Grid%nre) )
                SAD_LUT%Grid%re=0.00
               end if
            read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(i), i=1,nVals)
	    SAD_LUT%Grid%MinRe = SAD_LUT%Grid%Re(1)
	    SAD_LUT%Grid%MaxRe = SAD_LUT%Grid%Re(nVals)	    
	 
      end if
      
!     Read in the Tb array.
   if (Ctrl%Ind%NSolar > 0 .and. chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Tb(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%NSolzen, &
              SAD_LUT%Grid%nre))
         SAD_LUT%Tb=0.00
      end if
      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
	    (((SAD_LUT%Tb(chan, i, j, k), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nSolzen), k=1, SAD_LUT%Grid%nRe) 
      end if


      close(unit=l_lun)
   end if
999   if (ios /= 0) then
	 status = LUTFileReadErr 
	 write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 call Write_Log(Ctrl, trim(message), status)
      end if
end Subroutine Read_LUT_Tb


! Name:
!    Read_LUT_Tbd
!
! Purpose:
!    Reads Look Up Table values for bi-directional reflectance from LUT files 
!    into SAD_LUT struct.
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    Ctrl       struct  In           Control structure passed to Write_Log for
!                                    log file name
!    l_lun      int     in           Unit number for LUT file
!    LUT_file   string  in           LUT file name
!    chan       int     in           Current channel number used for array
!                                    indexing within the SAD_LUT struct, e.g.
!                                    SAD_LUT%Wavelength.
!    SAD_LUT    struct  out          Structure to hold the values from the LUT
!                                    file. N.B. this is one struct from the
!                                    array used elsewhere, not the whole array.
!    status     int     out          Status value returned by all ECP routines.
!
! Algorithm:
!    Open the LUT file specified by LUT_file, using unit l_lun
!    (if error, report error)
!    read the wavelength value into SAD_LUT%Wavelength(chan)
!    for each of the Grid parameters Tau, Satzen, Solzen, Relazi, Re:
!       read in the number of values
!       read the specified number of values into SAD_LUT%Grid%<array>
!       use the values read in to set the min and max for the parameter
!   (Note since Grid is not an array, it's values are overwritten by the
!   values from successive LUT files)
!
!   read in the Tbd array
!
!   if an error was found when checking no of grid parameter values 
!      report the error
!    
!
!   Note that this subroutine is passed a single struct
!   of type SAD_LUT, rather than the whole arrays used above. 
!
! Local variables:
!    Name       Type    Description
!    ios        int     I/O status from file operations
!    message    string  Message to be o/p by Write_Log function
!    nVals      int     No. of parameter values to read, from LUT file
!    i,j,k,l,m  int     Counters
!
! History:
!    6th Oct 2000, Andy Smith : original version
!    22/03/2013 Gareth Thomas : Added trim() to LUT_file in write(message,*)
!                      statements (called on I/O error). Also added write(*,*)
!                      statements for I/O errors
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

Subroutine Read_LUT_Tbd (Ctrl, l_lun, LUT_file, chan, &
   SAD_LUT, status)

   use ECP_Constants
   use Ctrl_def
    use SAD_LUT_def
   
   implicit none
   
!  argument declarations
!  Note that SAD_LUT are arrays of structs in the calling
!  routine, but scalar structs here. 
   
   type(CTRL_t), intent(in)        :: Ctrl
   integer, intent(in)             :: l_lun   !    Unit number for LUT file
   character(*)                    :: LUT_file!    Name of LUT file
   integer                         :: chan    !    Current channel number
    type(SAD_LUT_t), intent(inout)  :: SAD_LUT !    Single structs from the array
                                              !    used in the main program
   integer, intent(inout)            :: status
   
!  Local variables
   
   integer        :: ios     !    I/O status from file operations
   character(180) :: message !    Error message to pass to Write_Log
   integer        :: nVals   !    No. of Tau/Re/Satzen etc values in file
   integer        :: i,j,k,l,m  ! Loop counters
 
			     !    Array to hold unwanted transmission values
				! from LUT file. This array must be skipped over
				! before reading TFbd. It seems easiest to use
				! an implied do loop to read into this array and
				! then throw it away.
				! RANK AND SIZE OF ARRAY ASSUMED, BASED ON
				! TD/TFD ETC


   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)	    
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTTbd: Error opening file ', trim(LUT_file) 
      write(message, *) 'Read_LUTTbd: Error opening file ', trim(LUT_file) 
      call Write_Log(Ctrl, trim(message), status)
   else
!     Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau
      SAD_LUT%Grid%nTau = nVals                !    Used for loop control later

     
!        Read in Tau values and set min and max
         if (chan == Ctrl%Ind%SolarFirst) then
             if (.not.associated(SAD_LUT%Grid%tau)) allocate(SAD_LUT%Grid%Tau(SAD_LUT%Grid%ntau) )
             SAD_LUT%Grid%Tau=0.00
            endif

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(i), i=1,nVals)
	 SAD_LUT%Grid%MinTau = SAD_LUT%Grid%Tau(1)
	 SAD_LUT%Grid%MaxTau = SAD_LUT%Grid%Tau(nVals)
 

!     Now get the satzen values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen
	 SAD_LUT%Grid%nSatzen = nVals
	 
         !           Read in values and set min and max
         if (chan == Ctrl%Ind%SolarFirst) then 
             if (.not.associated(SAD_LUT%Grid%Satzen)) allocate(SAD_LUT%Grid%Satzen(SAD_LUT%Grid%nsatzen) )
             SAD_LUT%Grid%Satzen=0.00
            end if

         read(l_lun, *, err=999, iostat=ios) &
              (SAD_LUT%Grid%Satzen(i), i=1,nVals)
         SAD_LUT%Grid%MinSatzen = SAD_LUT%Grid%Satzen(1)
         SAD_LUT%Grid%MaxSatzen = SAD_LUT%Grid%Satzen(nVals)	    
     
      end if
      
!     Now get the Solzen values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSolzen
	 SAD_LUT%Grid%nSolzen = nVals
	 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%Solzen)) allocate(SAD_LUT%Grid%Solzen(SAD_LUT%Grid%nsolzen) )
                SAD_LUT%Grid%Solzen=0.00
               end if

            read(l_lun, *, err=999, iostat=ios) &
	       (SAD_LUT%Grid%Solzen(i), i=1,nVals)
	    SAD_LUT%Grid%MinSolzen = SAD_LUT%Grid%Solzen(1)
	    SAD_LUT%Grid%MaxSolzen = SAD_LUT%Grid%Solzen(nVals)	    
	 
      end if
      
!     Now get the rel. azi. values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRelazi
	 SAD_LUT%Grid%nRelazi = nVals
	 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%Relazi)) allocate(SAD_LUT%Grid%Relazi(SAD_LUT%Grid%nrelazi) )
                SAD_LUT%Grid%Relazi=0.00
               end if

           read(l_lun, *, err=999, iostat=ios)  &
	       (SAD_LUT%Grid%Relazi(i), i=1,nVals)
	    SAD_LUT%Grid%MinRelazi = SAD_LUT%Grid%Relazi(1)
	    SAD_LUT%Grid%MaxRelazi = SAD_LUT%Grid%Relazi(nVals)	    

      end if
      
!     Now get the effective radius values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe
	 SAD_LUT%Grid%nRe = nVals
	 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%Re)) allocate(SAD_LUT%Grid%Re(SAD_LUT%Grid%nre) )
                SAD_LUT%Grid%Re=0.00
               end if

            read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(i), i=1,nVals)
	    SAD_LUT%Grid%MinRe = SAD_LUT%Grid%Re(1)
	    SAD_LUT%Grid%MaxRe = SAD_LUT%Grid%Re(nVals)	    
	 
      end if
      
!     Skip the Tbd array - we dont use this in the ECP so it isn't part of
!     SAD_LUT. 

      if (Ctrl%Ind%NSolar > 0) then
         if (chan == Ctrl%Ind%SolarFirst) then 
            allocate(SAD_LUT%Tbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%NSatzen, &
              SAD_LUT%Grid%NSolzen, SAD_LUT%Grid%nrelazi, SAD_LUT%Grid%nre))
            SAD_LUT%Tbd=0.00
            end if

      end if

      if (status == 0) then
       read(l_lun, LUTArrayForm, err=999, iostat=ios) &
	    (((((SAD_LUT%Tbd(chan, i, j, k, l, m), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nSatzen), k=1, SAD_LUT%Grid%nSolzen),     &
	    l=1, SAD_LUT%Grid%nRelazi), m=1, SAD_LUT%Grid%nRe)

      end if

!     Read in the TFbd array.


      if (Ctrl%Ind%NSolar > 0) then
         if (chan == Ctrl%Ind%SolarFirst) then 
            allocate(SAD_LUT%Tfbd(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%NSolzen, &
              SAD_LUT%Grid%nre))
            SAD_LUT%Tfbd=0.00
            end if
      end if

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
	    (((SAD_LUT%TFbd(chan, i, j, k), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nSolzen), k=1, SAD_LUT%Grid%nRe)
      end if



      close(unit=l_lun)
   end if
999   if (ios /= 0) then
	 status = LUTFileReadErr 
	 write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 call Write_Log(Ctrl, trim(message), status)
      end if
end Subroutine Read_LUT_Tbd 


! Name:
!    Read_LUT_Td
!
! Purpose:
!    Reads Look Up Table values for diffuse transmission from LUT files 
!    into SAD_LUT struct.
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    Ctrl       struct  In           Control structure passed to Write_Log for
!                                    log file name
!    l_lun      int     in           Unit number for LUT file
!    LUT_file   string  in           LUT file name
!    chan       int     in           Current channel number used for array
!                                    indexing within the SAD_LUT struct, e.g.
!                                    SAD_LUT%Wavelength.
!    SAD_LUT    struct  out          Structure to hold the values from the LUT
!                                    file. N.B. this is one struct from the
!                                    array used elsewhere, not the whole array.
!    status     int     out          Status value returned by all ECP routines.
!
! Algorithm:
!    Open the LUT file specified by LUT_file, using unit l_lun
!    (if error, report error)
!    read the wavelength value into SAD_LUT%Wavelength(chan)
!    for each of the Grid parameters Tau, Satzen, Solzen, Relazi, Re:
!       read in the number of values

!       read the specified number of values into SAD_LUT%Grid%<array>
!       use the values read in to set the min and max for the parameter
!   (Note since Grid is not an array, it's values are overwritten by the
!   values from successive LUT files)
!
!   read in the Td array
!   read in the TFd array
!
!   if an error was found when checking no of grid parameter values 
!      report the error
!    
!
!   Note that this subroutine is passed a single struct
!   of type SAD_LUT, rather than the whole arrays used above. 
!
! Local variables:
!    Name       Type    Description
!    ios        int     I/O status from file operations
!    message    string  Message to be o/p by Write_Log function
!    nVals      int     No. of parameter values to read, from LUT file
!    i,j,k,l,m  int     Counters
!
! History:
!    13th Oct 2000, Andy Smith : original version
!    22/03/2013 Gareth Thomas : Added trim() to LUT_file in write(message,*)
!                      statements (called on I/O error). Also added write(*,*)
!                      statements for I/O errors
!
! Bugs:
!    None known
!
!---------------------------------------------------------------------

Subroutine Read_LUT_Td (Ctrl, l_lun, LUT_file, chan, &
   SAD_LUT, status)

   use ECP_Constants
   use Ctrl_def
    use SAD_LUT_def
   
   implicit none
   
!  argument declarations
!  Note  SAD_LUT are arrays of structs in the calling
!  routine, but scalar structs here. 
   
   type(CTRL_t), intent(in)        :: Ctrl
   integer, intent(in)             :: l_lun   !    Unit number for LUT file
   character(*)                    :: LUT_file!    Name of LUT file
   integer                         :: chan    !    Current channel number
   type(SAD_LUT_t), intent(inout)  :: SAD_LUT !    Single structs from the array
                                              !    used in the main program
   integer, intent(inout)            :: status
   
!  Local variables
   
   integer        :: ios     !    I/O status from file operations
   character(180) :: message !    Error message to pass to Write_Log
   integer        :: nVals   !    No. of Tau/Re/Satzen etc values in file
   integer        :: i,j,k,l,m  ! Loop counters


   open(unit=l_lun, file=LUT_file, status = 'old', iostat=ios)	    
   if (ios /= 0) then
      status = LUTFileOpenErr
      write(*, *) 'Read_LUTTd: Error opening file ', trim(LUT_file) 
      write(message, *) 'Read_LUTTd: Error opening file ', trim(LUT_file) 
      call Write_Log(Ctrl, trim(message), status)
   else
!     Read the file contents into the SAD_LUT structure

      read(l_lun, *, err=999, iostat=ios)SAD_LUT%Wavelength(chan)
      read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dTau
      SAD_LUT%Grid%nTau = nVals                !    Used for loop control later

!        Read in Tau values and set min and max
         if (chan == Ctrl%Ind%SolarFirst) then 
               if (.not.associated(SAD_LUT%Grid%tau)) allocate(SAD_LUT%Grid%Tau(SAD_LUT%Grid%ntau) )
               SAD_LUT%Grid%Tau=0.00
            end if

         read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Tau(i), i=1,nVals)
	 SAD_LUT%Grid%MinTau = SAD_LUT%Grid%Tau(1)
	 SAD_LUT%Grid%MaxTau = SAD_LUT%Grid%Tau(nVals)
   
!     Now get the satzen values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dSatzen
	 SAD_LUT%Grid%nSatzen = nVals
	 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%Satzen)) allocate(SAD_LUT%Grid%Satzen(SAD_LUT%Grid%nsatzen) )
                SAD_LUT%Grid%Satzen=0.00
            end if


            read(l_lun, *, err=999, iostat=ios) &
	       (SAD_LUT%Grid%Satzen(i), i=1,nVals)
	    SAD_LUT%Grid%MinSatzen = SAD_LUT%Grid%Satzen(1)
	    SAD_LUT%Grid%MaxSatzen = SAD_LUT%Grid%Satzen(nVals)	    
	 
      end if
      
!     Now get the effective radius values

      if (status == 0) then
         read(l_lun, *, err=999, iostat=ios)nVals, SAD_LUT%Grid%dRe
	 SAD_LUT%Grid%nRe = nVals
	 
!           Read in values and set min and max
            if (chan == Ctrl%Ind%SolarFirst) then 
                if (.not.associated(SAD_LUT%Grid%Re)) allocate(SAD_LUT%Grid%Re(SAD_LUT%Grid%nre) )
                SAD_LUT%Grid%Re=0.00
               end if

            read(l_lun, *, err=999, iostat=ios)(SAD_LUT%Grid%Re(i), i=1,nVals)
	    SAD_LUT%Grid%MinRe = SAD_LUT%Grid%Re(1)
	    SAD_LUT%Grid%MaxRe = SAD_LUT%Grid%Re(nVals)	    
	
      end if

      if (chan == Ctrl%Ind%SolarFirst) then
         allocate(SAD_LUT%Td(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%NSatzen, SAD_LUT%Grid%nre))      
         SAD_LUT%Td=0.00
         end if

!     Read in the Td array 

      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
	    (((SAD_LUT%Td(chan, i, j, k), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nSatzen), k=1, SAD_LUT%Grid%nRe)
      end if

!     Read in the TFd array.

      if (chan == Ctrl%Ind%SolarFirst) then 
         allocate(SAD_LUT%Tfd(Ctrl%Ind%Ny, SAD_LUT%Grid%Ntau, SAD_LUT%Grid%nre))
         SAD_LUT%Tfd=0.00
         end if
      if (status == 0) then
         read(l_lun, LUTArrayForm, err=999, iostat=ios) &
	    ((SAD_LUT%TFd(chan, i, j), i=1, SAD_LUT%Grid%nTau), &
	    j=1, SAD_LUT%Grid%nRe)
      end if


      close(unit=l_lun)
   end if
999   if (ios /= 0) then
	 status = LUTFileReadErr 
	 write(*, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 write(message, *)'Read_LUT: Error reading LUT file ', trim(LUT_file)
	 call Write_Log(Ctrl, trim(message), status)
      end if
end Subroutine Read_LUT_Td 


! Name:
!    Read_LUT
!
! Purpose:
!    Controls the reading of Look Up Table values from files into the SAD_LUT 
!    array of structs. There are seperate LUTs for different LUT values and 
!    channel indices.
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    Ctrl       struct  In           Control structure passed to Write_Log for
!                                    log file name
!    SAD_Chan   Array of structs    In
!                                    Channel data: required for solar/thermal
!                                    flags.
!    SAD_LUT    struct  out          
!                                    Structures to hold the values from the LUT
!                                    files. 
!    status     int     out          Status value returned by all ECP routines.
!
!
! Algorithm:
!    Find a logical unit number to be used for each LUT file in turn
!    for each cloud class
!       allocate the LUT arrays (To match no. of channels selected. All other
!          dimensions are set to the max. allowed)
!       for each channel
!          work out the Rd (and Rfd) LUT file name
!          call the Rd LUT file read function
!          work out the Td (and Tfd) LUT file name
!          call the Td LUT file read function
!          if (SAD_Chan solar flag is set)
!             work out the Rbd LUT file name
!             call the Rbd LUT file read function
!             work out the Tb LUT file name
!             call the Tb LUT file read function
!             work out the Tbd (and Tfbd) LUT file name
!             call the Tbd LUT file read function
!          if (SAD_Chan thermal flag is set)
!             work out the Em LUT file name
!             call the Em LUT file read function
!    N.B. it is possible for a channel to have both solar and thermal 
!    attributes, hence two "if" statements rather than "if... else"
!
!
! Local variables:
!    Name       Type    Description
!    LUT_file   string  Name of LUT file to be read
!    chan_num   string  Channel number converted to string for use in LUT_file
!    l_lun      int     Unit number used for all LUT files
!    i,j,k      int     Counters
!    bkp_lun    int     Unit number for breakpoint file
!    ios        int     I/O status returned by file open etc
!
! History:
!   13th Oct 2000, Andy Smith : original version
!   23rd Nov 2000, Andy Smith :
!      Channel file names updated: using 'Ch' instead of 'CH'
!    9th Jan 2001, Andy Smith :
!      Emissivity files available. Read_LUT_EM call un-commented.
!      Added breakpoint output.
!      Ctrl%Ind%Y renamed Y_Id
!   12th Jan 2001, Andy Smith :
!      Arrays of LUT values (RBd etc) made allocatable. Allocate sizes here.
!   18th Jan 2001, Andy Smith :
!      Bug fix in array allocation. Rfd, TFd arrays must always be allocated
!      even if the choice of channels means they're unused, because they are
!      read from the same files as Rd, Td by the same routines.
!    9th Feb 2001, Andy Smith :
!      Using pre-defined constants (ECPConstants.f90) for breakpoint levels.
!    1st Mar 2001, Andy Smith :
!      LUT array values are now divided by 100 since values in files are 
!      percentages and we require fractions later on.
!      (Temporary fix until files are re-written?)
!    7th Jun 2001, Andy Smith:
!      Debug log message removed from routine Read_LUT_Rbd
!   ******************************** ECV work starts here ********************
!   22nd Mar 2011, Andy Smith:
!      Remove phase change, phase 2 only 1 cloud class per run. 
!      SAD_LUT is also now reduced from dimension N cloud classes to 1. 
!    6th Apr 2011, Andy Smith:
!      Removed two redundant breakpoint outputs now that only 1 cloud class.
!   3rd May 2011, Andy Smith:
!      Extension to multiple instrument views. Wavelength array is now allocated.
!      Added wavelength to breakpoint outputs to allow checking when >1 view 
!      selected.  
!   3rd May 2011, Caroline Poulsen: removed allocation of LUTs into individual
!                    routine so ntau,nrensatzen etc could be read and used directly
!                    from LUT files and are not replicated else where
! Bugs:
!   None known.
!
!
!---------------------------------------------------------------------

Subroutine Read_LUT (Ctrl, SAD_Chan, SAD_LUT, status)

   use CTRL_def
   use SAD_Chan_def
    use SAD_LUT_def
!   use LUT_Routines_def
   
   implicit none
   
!  argument declarations 
   
   type(CTRL_t), intent(inout)                      :: Ctrl
   type(SAD_Chan_t), dimension(:), intent(in)       :: SAD_Chan
   type(SAD_LUT_t), intent(inout)                   :: SAD_LUT
   integer, intent(inout)                             :: status
   
!  Local variables

   integer                :: i, j    !    Array counters
   character(FilenameLen) :: LUT_file!    Name of LUT file
   character(4)           :: chan_num!    Channel number converted to a string
   integer                :: l_lun   !    Unit number for LUT file
   integer                :: bkp_lun !    Unit number for breakpoint file
   integer                :: ios     !    I/O status returned by file open etc
   integer                :: k       !    Array counter


!  Open breakpoint file if required. 

#ifdef BKP
   if (Ctrl%Bkpl > 0) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      & 
           file=Ctrl%FID%Bkp, &
	   status='old',      &
	   position='append', &
	   iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
	 call Write_Log(Ctrl, 'Read_LUT: Error opening breakpoint file', status)
      else
         write(bkp_lun,*)'Read_LUT:'
      end if
   end if
#endif

!  Call find lun here and pass l_lun to subroutines, rather than 
!  calling once per file to be read. 

   call Find_LUN(l_lun)

!  For each cloud class, construct the LUT filename from the 
!  instrument name, cloud class ID, variable name and channel number.
!  Then call the appropriate LUT file read function. Just pass the current 
!  SAD_LUT struct, rather than the whole array. 

   if (status == 0) then

!        AllocateSAD_LUT%Grid%Tau the LUT arrays in SAD_LUT. All arrays are allocated big  
!        enough to hold the total number of channels selected, even though 
!        not all arrays hold both thermal and solar data. This makes it easier
!        to keep track of where each channel's data is. All other dimensions
!        (tau etc) are set to the max. possible size as nTau etc can vary with
!        channel number.

!      allocate(SAD_LUT%Wavelength(Ctrl%Ind%Ny))
      if (.not.associated(SAD_LUT%Wavelength)) allocate(SAD_LUT%Wavelength(Ctrl%Ind%Ny))
      SAD_LUT%Wavelength=0.00

      do j=1, Ctrl%Ind%Ny
         if (status /= 0) exit ! Drop out if an open or read error occurred

         if (Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) < 10) then 
            write(chan_num, '(a2,i1)') 'Ch',Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) 
         else
            write(chan_num, '(a2,i2)') 'Ch',Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(j)) 
         end if


!           Read Rd, Td files for all channels (solar and thermal)

         LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name)   &
                     // '_' // trim(Ctrl%CloudClass%Name) // '_RD_'   & 
                     // trim(chan_num) // '.sad'       

 !        write(*,*) 'before rd'
         call Read_LUT_RD(Ctrl, l_lun, LUT_file, j, &
              SAD_LUT, status)

         if (status == 0) then
            LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name)   &
                        // '_' // trim(Ctrl%CloudClass%Name) // '_TD_'   & 
                        // trim(chan_num) // '.sad'       

!            write(*,*) 'before td'
            call Read_LUT_TD(Ctrl, l_lun, LUT_file, j, &
               SAD_LUT, status)	 
         end if


!           Read solar channel attributes

         if (SAD_Chan(j)%Solar%Flag > 0) then	 

!              Generate the Rbd file name for this channel and cloud class, and
!              call the Rbd read function. 

            if (status == 0) then
               LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name)   &
                           // '_' // trim(Ctrl%CloudClass%Name) // '_RBD_'   & 
                           // trim(chan_num) // '.sad'       

!               write(*,*) 'before rbd'
               call Read_LUT_RBD(Ctrl, l_lun, LUT_file, j, &
                  SAD_LUT, status)	 

        end if

!              Read the Tb data

            if (status == 0) then
               LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name)  &
                           // '_' // trim(Ctrl%CloudClass%Name) // '_TB_'   & 
                           // trim(chan_num) // '.sad'       

!               write(*,*) 'before tb'
               call Read_LUT_Tb(Ctrl, l_lun, LUT_file, j, & 
                  SAD_LUT, status)	 	 



            end if

!              Read the Tbd/TFbd data (N.B. Tbd is not kept)

            if (status == 0) then
               LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name)  &
                           // '_' // trim(Ctrl%CloudClass%Name) // '_TBD_'  & 
                           // trim(chan_num) // '.sad'       

!               write(*,*) 'before tbd'
               call Read_LUT_Tbd(Ctrl, l_lun, LUT_file, j, &
                  SAD_LUT, status)	 	 

            end if
         end if   !    End of "solar" actions


!           Read thermal channel attributes

            if (status == 0) then
               if (SAD_Chan(j)%Thermal%Flag > 0) then

!                 Read the Em data

                  LUT_file = trim(Ctrl%SAD_Dir) // '/' // trim(Ctrl%Inst%Name)  &
                           // '_' // trim(Ctrl%CloudClass%Name) // '_EM_'  & 
                           // trim(chan_num) // '.sad'       

!                  write(*,*) 'before em'
                  call Read_LUT_Em(Ctrl, l_lun, LUT_file, j, &
                     SAD_LUT, status)
!                  write(*,*) 'after em'
               end if	 	 
            end if         
         end do

#ifdef BKP
      if (Ctrl%Bkpl >= BkpL_Read_LUT_1) then
!           Write out SAD_LUT Name and Wavelength 
         write(bkp_lun, *)'Name in cloud class struct: ', &
            Ctrl%CloudClass%Name 
      end if	
         
      if (Ctrl%Bkpl >= BkpL_Read_LUT_2) then
!           Write out SAD_LUT Grid substructs.
         write(bkp_lun,*)'Wavelengths: ',(SAD_LUT%Wavelength(i),i=1,Ctrl%Ind%Ny)

         write(bkp_lun,*)'Max, min, delta Tau:',SAD_LUT%Grid%MaxTau, &
            SAD_LUT%Grid%MinTau, SAD_LUT%Grid%dTau
         write(bkp_lun,'(a, 9(f6.3, 1x),/)') ' Tau: ', &
            (SAD_LUT%Grid%Tau(k), k=1,SAD_LUT%Grid%nTau)

         write(bkp_lun,*)'Max, min, delta Re:',SAD_LUT%Grid%MaxRe, &
            SAD_LUT%Grid%MinRe, SAD_LUT%Grid%dRe
         write(bkp_lun,'(a, 12(f7.1, 1x),/)') ' Re: ', &
            (SAD_LUT%Grid%Re(k), k=1,SAD_LUT%Grid%nRe)
         
         write(bkp_lun,*)'Max, min, delta SatZen:', &
            SAD_LUT%Grid%MaxSatZen, &
            SAD_LUT%Grid%MinSatZen, SAD_LUT%Grid%dSatZen
         write(bkp_lun,'(a, 10(f6.1, 1x),/)') ' SatZen: ', &
            (SAD_LUT%Grid%SatZen(k), k=1,SAD_LUT%Grid%nSatZen)
         
         write(bkp_lun,*)'Max, min, delta SolZen:',SAD_LUT%Grid%MaxSolZen, &
            SAD_LUT%Grid%MinSolZen, SAD_LUT%Grid%dSolZen
         write(bkp_lun,'(a, 10(f6.1, 1x),/)') ' SolZen: ', &
            (SAD_LUT%Grid%SolZen(k), k=1,SAD_LUT%Grid%nSolZen)
         
         write(bkp_lun,*)'Max, min, delta RelAzi:',SAD_LUT%Grid%MaxRelAzi, &
            SAD_LUT%Grid%MinRelAzi, SAD_LUT%Grid%dRelAzi
         write(bkp_lun,'(a, 11(f6.1, 1x),/)') ' RelAzi: ', &
            (SAD_LUT%Grid%RelAzi(k), k=1,SAD_LUT%Grid%nRelAzi)
         
      end if
#endif

   end if
   
!  Convert from percentage to fractional values

   if (status == 0) then
!      do i=1, Ctrl%CloudClass%N
!      write(*,*) 'rd'
      SAD_LUT%Rd(:,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen,   &
           & 1:SAD_LUT%Grid%nRe) &
           & = SAD_LUT%Rd(:,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen, &
           & 1:SAD_LUT%Grid%nRe) / 100.

!      write(*,*) 'td',Ctrl%Ind%Ny
!      write(*,*) 'SAD_LUT%Td A',minval(SAD_LUT%Td),maxval(SAD_LUT%Td)
!      write(*,*) Ctrl%Ind%Ny,SAD_LUT%Grid%nTau,SAD_LUT%Grid%nSatZen,SAD_LUT%Grid%nRe
!      pause
      SAD_LUT%Td(1:Ctrl%Ind%Ny,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen,1:SAD_LUT%Grid%nRe)= & 
           & SAD_LUT%Td(1:Ctrl%Ind%Ny,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen, 1:SAD_LUT%Grid%nRe)/ &
           & 100.
!      write(*,*) 'SAD_LUT%Td B',SAD_LUT%Td
!      pause
!      write(*,*) 'tfd'
      SAD_LUT%Tfd(:,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nRe) & 
      = SAD_LUT%Tfd(:,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nRe) / 100.
      
!      write(*,*) 'rfd'
      SAD_LUT%Rfd(:,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nRe) & 
      = SAD_LUT%Rfd(:,1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nRe) / 100.
      
      if (Ctrl%Ind%NSolar > 0) then 
!         write(*,*) 'rbd'
         SAD_LUT%Rbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast, &
            1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen,   &
            1:SAD_LUT%Grid%nSolZen,1:SAD_LUT%Grid%nRelAzi,&
            1:SAD_LUT%Grid%nRe) = &
            SAD_LUT%Rbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,&
            1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen,   &
            1:SAD_LUT%Grid%nSolZen,1:SAD_LUT%Grid%nRelAzi,&
            1:SAD_LUT%Grid%nRe) / 100.
  
!         write(*,*) 'tbd'
         SAD_LUT%Tbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast, &
            1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen,   &
            1:SAD_LUT%Grid%nSolZen,1:SAD_LUT%Grid%nRelAzi,&
            1:SAD_LUT%Grid%nRe) = &
            SAD_LUT%Tbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,&
            1:SAD_LUT%Grid%nTau,1:SAD_LUT%Grid%nSatZen,   &
            1:SAD_LUT%Grid%nSolZen,1:SAD_LUT%Grid%nRelAzi,&
            1:SAD_LUT%Grid%nRe) / 100.

!         write(*,*) 'tb'
         SAD_LUT%Tb(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast, &
            1:SAD_LUT%Grid%nTau, 1:SAD_LUT%Grid%nSolZen,  &
            1:SAD_LUT%Grid%nRe) = &
            SAD_LUT%Tb(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,&
            1:SAD_LUT%Grid%nTau, 1:SAD_LUT%Grid%nSolZen, &
            1:SAD_LUT%Grid%nRe) / 100.

!         write(*,*) 'tfbd'
         SAD_LUT%Tfbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast, &
            1:SAD_LUT%Grid%nTau, 1:SAD_LUT%Grid%nSolZen,  &
            1:SAD_LUT%Grid%nRe) = &
            SAD_LUT%Tfbd(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast,&
            1:SAD_LUT%Grid%nTau, 1:SAD_LUT%Grid%nSolZen, &
            1:SAD_LUT%Grid%nRe) / 100.
      end if
      if (Ctrl%Ind%NThermal > 0) then
!         write(*,*) 'em'
         SAD_LUT%Em(Ctrl%Ind%ThermalFirst:Ctrl%Ind%ThermalLast, &
            1:SAD_LUT%Grid%nTau, 1:SAD_LUT%Grid%nSatZen,  &
            1:SAD_LUT%Grid%nRe) = &
            SAD_LUT%Em(Ctrl%Ind%ThermalFirst:Ctrl%Ind%ThermalLast, &
            1:SAD_LUT%Grid%nTau, 1:SAD_LUT%Grid%nSatZen,  &
            1:SAD_LUT%Grid%nRe) / 100.
      end if
!      end do
   end if   
   
#ifdef BKP
   if (Ctrl%Bkpl > 0) then
      write(bkp_lun, *) 'Read_LUT: end ----------'
      close(unit=bkp_lun)
   end if   
#endif
 
end Subroutine Read_LUT 
