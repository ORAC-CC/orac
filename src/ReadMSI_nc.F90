! Name:
!    Read_MSI
!
! Purpose:
!    Controls the reading of Multi Spectral Image (MSI) values from ATSR-type
!    files into the DATA_MSI array.
!
! Arguments:
!   Name     Type          In/Out/Both   Description
!   Ctrl     struct        Both          Control structure (date is read in 
!                                        here).
!   NSegs    int           In            Number of image segments read in by 
!                                        previous calls to this routine.
!   SegSize  int           In            Number of rows of pixels in an image 
!                                        segment.
!   MSI_files_open Logical In            Indicates whether the MSI data file is
!                                        open (if not, open it).
!   lun      int           In/Out        File unit number set by this routine
!                                        when file is opened, passed back and
!                                        forth for subsequent reads.
!   MSI_Data struct        In/Out        Data structure: the MSI data part 
!                                        of this struct is populated by this
!                                        routine, and is overwritten on 
!                                        successive calls.
!   SAD_Chan struct array  Both          Instrument channel parameters. Updated
!                                        by this routine: solar constant is 
!                                        modified from annual average to value 
!                                        for the day of the MSI data.
!   status   int           Out           Error status          
!    
! Algorithm:
!   if (MSI files are not yet open)
!      Find a logical unit number to be used for the MSI file
!      Open MSI file
!      If open error
!         Write error message to screen and log file
!      else
!         allocate MSI image segment array in Data_MSI struct.
!   
!   If (no error opening files)
!      Read header (not used further)
!      If read error
!         Write error message to screen and log file
!      Else
!         Read MSI array of size defined by Ctrl structure
!         If read error
!            Write error message to log file
!         If end of file or end of record (io status < 0)
!            Check current segment no. vs. expected last segment in file
!            if current segment is not last, report error
!   Leave MSI file open for further reads
!
! Local variables:
!   Name      Type   Description
!   ios       int    I/O status, file operations
!   message   char   Error message to pass to Write_Log 
!   day, month int   Day and month numbers extracted from header and used in 
!                    calculation of Ctrl%DOY.
!   i         int    Counter for DOY calculation
!
! History:
!   3rd November, 2000, Kevin M. Smith : original version
!   23rd November, 2000, KMS: Added status to argument list
!   19th December, 2000, KMS: Replaced Data_MSI array with data structure
!   15th January,  2001, KMS: Changed Ctrl%Ind%Y to Ctrl%Ind%Y_Id
!   27th July 2001, Andy Smith:
!      Updates for handling image segmentation:
!      - new arguments MSI_files_open, lun (since the file now stays open for
!        repeated read operations)
!      - file open depends on MSI_files_open flag
!      Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).
!      Added argument intent specifiers.
!    10th Aug 2001, Andy Smith:
!      Updated to handle image segments/super-pixels of any size. Requires
!      handling of end of file during read on the last segment. On earlier 
!      segments EOF is reported as an error.
!      Added code to read the file header, extract date and convert the 
!      solar constant mean values in SAD_Chan to values for the current month
!      and day. (New argument SAD_Chan). Intent for Ctrl changed to inout.
!    22nd Aug 2001, Andy Smith:
!      Bug fix: added status check before ReadFPArray call.
!    23rd Aug 2001, Andy Smith:
!      Additional argument to ReadFPArray: Ctrl%Ind%NChans, no of instrument 
!      channels available, specifies size of input array of channel data (as
!      opposed to Ctrl%Ind%Ny which specifies the size of the output array).
!   18th May 2011, Andy Smith:
!      Extension to multiple instrument views. Number of "channels" available in 
!      the MSI file is increased by factor of NViews. Modify the NChans argument 
!      passed to ReadFPArray. 
!      Array of selected channel identifiers Y_Id must be modified before passing
!      to ReadFPArray, as it contains repeated Channel IDs if more than one view
!      is selected. Use Y_Id and ViewIdx to populate ChanIdx array. 
!  28th June 2011 Caroline poulsen remove reference to ATSR
!                  changed  Ctrl.Ind.Nchans to  Ctrl.Ind.Ny to remove dependance! on config file
!   25th Nov   2011, Caroline Poulsen add ChI(replace y_id) channel indice variable.!
! 2nd Dec 2011 changed Ninstviews to nviews
!   13th Dec 2011 Caroline poulsen change format statement to make
!                g95 compatible 
! 20/01/2012 C Poulsen remove write statement
! 2012/08/21 MJ uses original routine and implements reading of netcdf data.
! 2012/09/21  CP added channel index to y_id value
! 2012/11/03  MST and MJ  hard code in values for avhrr
! 20131118 MJ cleans and debugs

! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
Subroutine Read_MSI_nc(Ctrl, NSegs, SegSize, MSI_Data, &
     & SAD_Chan, status)
   
  use ECP_Constants
  use CTRL_def
  use Data_def
  use SAD_Chan_def
  
  use netcdf
  
  implicit none
   
  !  Argument declarations
   
  type(CTRL_t), intent(inout) :: Ctrl
  integer, intent(in)         :: NSegs     ! Number of segments read so far
  integer, intent(inout)         :: SegSize   ! Size of image segment in rows of
  ! pixels.
  type(Data_t), intent(inout) :: MSI_Data
  type(SAD_Chan_t), intent(inout) :: SAD_Chan(Ctrl%Ind%Ny)
  integer, intent(out)        :: status
  
  !  Local variables
  
  integer        :: ios       ! I/O status from file operations
  !MJ ORG character(FilenameLen+100) :: message   ! Error message to pass to Write_Log
  character(2048) :: message   ! Error message to pass to Write_Log
  integer        :: day, month ! Day and month numberss extracted from 
  ! Date string when calculating DOY.
  integer        :: i         ! Counter for DOY calculation

  !netcdf related
  integer :: ncid
  character(len=12) :: prod_date
  INTEGER(kind=nint) ::  ii
  integer(kind=nint), allocatable, dimension(:) :: msi_instr_ch_numbers

  !  On first call, the file is opened. It is then left open for all subsequent
  !  calls. 
  
  status = 0
  !     Open MSI file
  ios = nf90_open(path=trim(adjustl(Ctrl%Fid%MSI)),mode = nf90_nowrite,ncid = ncid) 

  if (ios /= NF90_NOERR) then
     status = MSIFileOpenErr ! Return error code
     write(unit=message, fmt=*) &
          & 'Read_MSI: Error opening file ', trim(adjustl(Ctrl%Fid%MSI))
     call Write_Log(Ctrl, trim(message), status)
     write(*,*) 'Read_MSI: Error opening file ', trim(adjustl(Ctrl%Fid%MSI))
     stop
  else
     
     !Read product date and time from netcdf global attributes
     ios=nf90_get_att(ncid, NF90_GLOBAL, "Product_Date", prod_date)

     if (ios == 0) then
        Ctrl%Date=trim(adjustl(prod_date(1:8)))
        Ctrl%Time=trim(adjustl(prod_date(9:12)))
        !get day and month as integers
        read(prod_date(7:8), '(I2)') day
        read(prod_date(5:6), '(I2)') month
        !day=trim(adjustl(prod_date(7:8)))
        !mon=trim(adjustl(prod_date(5:6)))
        !        write(*,*) Ctrl%Date
        !write(*,*) Ctrl%Time
        !write(*,*) day,month

        !compute DOY for present day
        Ctrl%DOY = 0
        do i=1,month-1
           Ctrl%DOY = Ctrl%DOY + days_in_month(i)
        end do
        Ctrl%DOY = Ctrl%DOY + day                  

        !           Calculate solar constant based on day of year using the mean
        !           and amplitude of variation.
        
        do i = 1, Ctrl%Ind%Ny
           if (SAD_Chan(i)%Solar%Flag > 0) &
                & SAD_Chan(i)%Solar%F0 = SAD_Chan(i)%Solar%F0 + &
                & (SAD_Chan(i)%Solar%F1 * cos(2 * Pi * Ctrl%DOY / 365.))
        end do
        
     else
        status = MSIFileReadHeadErr ! Return error code
        write(unit=message, fmt=*) &
             'Read_MSI: Error reading header of file ', trim(adjustl(Ctrl%Fid%MSI))
        call Write_Log(Ctrl, trim(message), status)
        write(*,*) 'Read_MSI: Error reading header of file ', trim(adjustl(Ctrl%Fid%MSI))
        stop
     endif
     
  end if

  if (status == 0) then

     !  Read MSI file 
     !read instrument channel numbers stored in the file
     !Allocate Data%MSI structure size to match image segments to be used.
     allocate(MSI_Data%MSI(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%Ny))
     allocate(MSI_Data%time(Ctrl%Ind%Xmax, SegSize))

     !read instrument channel indices from file
     write(*,*) 'Ctrl%Ind%Nyp',Ctrl%Ind%Nyp
     !stop
     allocate(msi_instr_ch_numbers(Ctrl%Ind%Nyp)) 
     msi_instr_ch_numbers=0_nint
     call nc_read_array_1d_int_to_int_orac(ncid,Ctrl%Ind%Nyp,"msi_instr_ch_numbers",msi_instr_ch_numbers,0)
     write(*,*) 'msi channel numbers',msi_instr_ch_numbers
     write(*,*)  'msi channel numbers y_id',Ctrl%Ind%Y_Id

     !loop over channels and read if desired channel number is hit
     !manually change of id, only needed here for avhrr-noaa18!!
     if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA15") then
        Ctrl%Ind%Y_Id(1)=1
        Ctrl%Ind%Y_Id(2)=2
        Ctrl%Ind%Y_Id(3)=4
        Ctrl%Ind%Y_Id(4)=5
        Ctrl%Ind%Y_Id(5)=6
     endif
     if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA16") then
        Ctrl%Ind%Y_Id(1)=1
        Ctrl%Ind%Y_Id(2)=2
        Ctrl%Ind%Y_Id(3)=4
        Ctrl%Ind%Y_Id(4)=5
        Ctrl%Ind%Y_Id(5)=6
     endif
     if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA17") then
        !here channel 3a is on
        Ctrl%Ind%Y_Id(1)=1
        Ctrl%Ind%Y_Id(2)=2
        Ctrl%Ind%Y_Id(3)=3
        Ctrl%Ind%Y_Id(4)=5
        Ctrl%Ind%Y_Id(5)=6
     endif

     if(trim(Ctrl%Inst%Name) .eq. "AVHRR-NOAA18") then
        Ctrl%Ind%Y_Id(1)=1
        Ctrl%Ind%Y_Id(2)=2
        Ctrl%Ind%Y_Id(3)=4
        Ctrl%Ind%Y_Id(4)=5
        Ctrl%Ind%Y_Id(5)=6
     endif

     print*,'Number of channels to read in',Ctrl%Ind%Ny
     print*,'chi',Ctrl%Ind%Chi
     print*,'msi ch',msi_instr_ch_numbers
     print*, 'y_id ',Ctrl%Ind%Y_Id
     do ii=1,Ctrl%Ind%Ny

        print*,'Read channel in MSI file', &
             & ii,msi_instr_ch_numbers(Ctrl%Ind%Chi(ii)),Ctrl%Ind%Chi(ii),Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(ii))
        call nc_read_array_3d_float_orac &
             & (ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,Ctrl%Ind%Chi(ii),"msi_data",MSI_Data%MSI(:,:,ii),1)
        write(*,*) 'max/min MSI', maxval(MSI_Data%MSI(:,:,ii)),minval(MSI_Data%MSI(:,:,ii))
     enddo

     

!!$     print*,maxval(MSI_Data%MSI(:,:,1))
!!$     print*,maxval(MSI_Data%MSI(:,:,2))
!!$     print*,maxval(MSI_Data%MSI(:,:,3))
!!$     print*,maxval(MSI_Data%MSI(:,:,4))
!!$     print*,maxval(MSI_Data%MSI(:,:,5))
!!$
!!$
!!$     print*,'-----------'
!!$     print*,(MSI_Data%MSI(1:3,1,1))
!!$     print*,(MSI_Data%MSI(1:3,1,2))
!!$     print*,(MSI_Data%MSI(1:3,1,3))
!!$     print*,(MSI_Data%MSI(1:3,1,4))
!!$     print*,(MSI_Data%MSI(1:3,1,5))
!!$
!!$     print*,shape(MSI_Data%MSI)

     deallocate(msi_instr_ch_numbers)
     write(*,*) 'Done reading MSI input'         
     
     !read time data
     call nc_read_array_2d_double_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,"time_data",MSI_Data%time(:,:),0)
     
  endif

  !close  msi input file
  ios=nf90_close(ncid)
  if (ios /= 0) then
     status = MSIFileCloseErr ! Return error code
     write(unit=message, fmt=*) &
          & 'Read_MSI: Error closing file ', trim(adjustl(Ctrl%Fid%MSI))
     call Write_Log(Ctrl, trim(message), status)
  endif
end subroutine Read_MSI_nc
