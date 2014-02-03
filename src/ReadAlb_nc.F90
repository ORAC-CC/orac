! Name:
!    Read_ALB
!
! Purpose:
!    Controls the reading of Multi Spectral Image (ALB) values from ATSR-type
!    files into the DATA_ALB array.
!
! Arguments:
!   Name     Type          In/Out/Both   Description
!   Ctrl     struct        Both          Control structure (date is read in 
!                                        here).
!   NSegs    int           In            Number of image segments read in by 
!                                        previous calls to this routine.
!   SegSize  int           In            Number of rows of pixels in an image 
!                                        segment.
!   ALB_files_open Logical In            Indicates whether the ALB data file is
!                                        open (if not, open it).
!   lun      int           In/Out        File unit number set by this routine
!                                        when file is opened, passed back and
!                                        forth for subsequent reads.
!   MSI_Data struct        In/Out        Data structure: the ALB data part 
!                                        of this struct is populated by this
!                                        routine, and is overwritten on 
!                                        successive calls.
!   SAD_Chan struct array  Both          Instrument channel parameters. Updated
!                                        by this routine: solar constant is 
!                                        modified from annual average to value 
!                                        for the day of the ALB data.
!   status   int           Out           Error status          
!    
! Algorithm:
!   if (ALB files are not yet open)
!      Find a logical unit number to be used for the ALB file
!      Open ALB file
!      If open error
!         Write error message to screen and log file
!      else
!         allocate ALB image segment array in Data_ALB struct.
!   
!   If (no error opening files)
!      Read header (not used further)
!      If read error
!         Write error message to screen and log file
!      Else
!         Read ALB array of size defined by Ctrl structure
!         If read error
!            Write error message to log file
!         If end of file or end of record (io status < 0)
!            Check current segment no. vs. expected last segment in file
!            if current segment is not last, report error
!   Leave ALB file open for further reads
!
! Local variables:
!   Name      Type   Description
!   ios       int    I/O status, file operations
!   message   char   Error message to pass to Write_Log 
!   i         int    Counter for DOY calculation
!
! History:
!   29th May, 2002, Caroline Poulsen : original version copied
!                                 from READ_MSI 
!   29th Oct, 2002, Caroline Poulsen : fixed bug too many arguments in
!      the header removed sad_chan
!   28th June 2011 Caroline poulsen remove reference to ATSR
!   13th Dec 2011 Caroline poulsen change format statement to make
!                g95 compatible 
!   15/09/2012 CP initialise array
!   15/09/2012 CP changed to read from netcdf files
! 2013 Matthias Jerg fixes bug with close of netcdf file.
! 20131118 MJ cleans and debugs:Loop to read albedo is indexed with ysolar as ysolar halds the channel
!indices as they are stored in the preprocessing file.
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
Subroutine Read_ALB_nc(Ctrl, NSegs, SegSize, MSI_Data, status)
   
   use ECP_Constants
   use CTRL_def
   use Data_def
   use SAD_Chan_def

   use netcdf

   implicit none

!  Argument declarations

   type(CTRL_t), intent(inout) :: Ctrl
   integer, intent(in)         :: NSegs     ! Number of segments read so far
   integer, intent(in)         :: SegSize   ! Size of image segment in rows of
                                            ! pixels.
   
   type(Data_t), intent(inout) :: MSI_Data
   integer, intent(out)        :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log

   !netcdf related
   integer :: ncid
   character(len=12) :: prod_date
  INTEGER(kind=nint) ::  ii
  integer(kind=nint), allocatable, dimension(:) :: alb_instr_ch_numbers
!  On first call, the file is opened. It is then left open for all subsequent
!  calls. 

   status = 0
!     Open ALB file
   write(*,*)'alb file',Ctrl%Fid%Aux
   ios = nf90_open(path=trim(adjustl(Ctrl%Fid%Aux)),mode = nf90_nowrite,ncid = ncid) 
   write(*,*)'alb ios',ios
   if (ios /= 0) then
      status = ALBFileOpenErr ! Return error code
      write(unit=message, fmt=*) &
           'Read_ALB: Error opening file ', trim(adjustl(Ctrl%Fid%AUX))
      call Write_Log(Ctrl, trim(message), status)
   else
      !        Allocate Data%ALB structure size to match image segments to be read in.
      !MJ ORGallocate(MSI_Data%ALB(Ctrl%Ind%Xmax, SegSize, 4))
      allocate(MSI_Data%ALB(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NSolar))
      
      !read instrument channel indices from file
      allocate(alb_instr_ch_numbers(Ctrl%Ind%NSolar))
      alb_instr_ch_numbers=0_nint
      call nc_read_array_1d_int_to_int_orac(ncid,Ctrl%Ind%NSolar,"alb_abs_ch_numbers",alb_instr_ch_numbers,0)
      
      !loop over channels and read if desired channel number is hit
      ios=nf90_get_att(ncid, NF90_GLOBAL, "Product_Date", prod_date)

      do ii=1,Ctrl%Ind%NSolar !MJORG!4!Ctrl%Ind%Ny
         write(*,*) 'Ctrl%Ind%ysolar(ii)',Ctrl%Ind%ysolar(ii)
         call nc_read_array_3d_float_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,Ctrl%Ind%ysolar(ii),"alb_data",MSI_Data%ALB(:,:,ii),0)
!         call nc_read_array_3d_float_orac(ncid,Ctrl%Ind%Xmax,Ctrl%Resoln%SegSize,ii,"alb_data",MSI_Data%ALB(:,:,ii),0)

         write(*,*) 'Max/Min Alb',maxval(MSI_Data%ALB(:,:, ii)),minval(MSI_Data%ALB(:,:, ii))
      enddo
      
      !write(*,*) 'alb_data',MSI_Data%ALB(1, 1, :)	 
      !    write(*,*) 'alb_data',MSI_Data%ALB(1, 1, 1)	 
      !    write(*,*) 'alb_data',MSI_Data%ALB(300, 300, :)

      deallocate(alb_instr_ch_numbers)
      !write(*,*) 'Done reading ALB input' 

     end if

     !close  alb input file
     ios=nf90_close(ncid)
     write(*,*)'Reading Albedo done (status)',status,ios

   end subroutine Read_ALB_nc

