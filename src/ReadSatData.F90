! Name:
!   Read_SatData
!
! Purpose:
!   Controls the reading of satellite image, cloud flag, land/sea flag and
!   geometry data.
!   
!
! Arguments:
!   Name      Type           In/Out   Description
!   Ctrl      struct         Both     Control structure
!   NSegs     int            In       Number of image segments read in by
!                                     previous calls to this routine.
!   SegSize   int            In       Size of an image segment in rows of
!                                     pixels.
!   MSI_files_open   Logical Both     Indicates whether the MSI etc files are
!                                     open.
!   MSI_luns  int array      Both     Logical unit numbers for the MSI and 
!                                     associated files.
!   MSI_Data  alloc struct   Out      Data structure (multi-spectral image plus
!                                     associated geometry, cloud flags etc)
!   SAD_Chan struct array    Both     Instrument channel parameters. Updated
!                                     by Read_MSI: solar constant is 
!                                     modified from annual average to value 
!                                     for the day of the MSI data.
!   status    int            Out      Error status
!    
! Algorithm:
!   Determine current instrument
!       Call instrument-specific MSI reading subroutine
!       Call instrument-specific cloud flag reading subroutine
!       Call instrument-specific land/sea reading subroutine
!       Call instrument-specific geometry reading subroutine
!       Call instrument-specific location reading subroutine
!       Set MSI_files_open to true so that read routines don't open files
!          after the first call.
!
! Local variables:
!   Name        Type    Description	
!   (none)
!
! History:
!   16th November, 2000, Kevin M. Smith : original version
!   23rd November, 2000, KMS: added status to argument list
!   23rd November, 2000, Andy Smith: 
!      Checking status after each subroutine call.
!   19th December, 2000, KMS: Replaced Data_... arrays with Data structure
!    2nd Aug 2001, Andy Smith:
!      Updates for image segmentation: new arguments MSI_files_open, MSI_luns.
!      Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).
!      Added argument intent specifiers.
!   10th Aug 2001, Andy Smith:
!      Added new arguments NSegs, SegSize, used to check whether end of 
!      file during a segment read is valid. Intent for Ctrl changed to inout
!      because Read_MSI reads the date from the MSI file header.
!      ************** ECV work starts here ********************
!    9th Mar 2011, Andy Smith:
!     Re-applying changes made in late 2001/2. Add reading of albedo file.
!     (27th may 20012, Caroline Poulsen added in read alb] )
!     (29th october 2002 Caroline Poulsen fixed but . not %)
!     Added check on name string to allow AATSR data. 
!    28th June 2011 Caroline Poulsen remove reference to ATSR
!    28th July 2011 Caroline Poulsen added in scan line routine
!    1st aug 2011 add extra value to msi_luns to accept uv data
!    15 Feb 2012  only read in albedo for scenes with day light componenet.
!    18 June 2012 calculate illumination data.
! Bugs:
!   None known.
!
! $Id: ReadSatData.f90 80 2011-08-16 16:13:01Z capoulse $
!
!------------------------------------------------------------------------------------
Subroutine Read_SatData(Ctrl, NSegs, SegSize, MSI_files_open, MSI_luns, &
   MSI_Data, SAD_Chan, status)

   use CTRL_def
   use Data_def
   use SAD_Chan_def

    implicit none

! Define arguments

   type(CTRL_t), intent(inout) :: Ctrl
   integer, intent(in)         :: NSegs
   integer, intent(in)         :: SegSize
   logical, intent(inout)      :: MSI_files_open
   integer, intent(inout)      :: MSI_luns(7)
   type(Data_t), intent(inout) :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(Ctrl%Ind%Ny)
   integer, intent(out)        :: status
   real         :: minsolzen
! Define local variables (currently none)


! Call appropriate satellite data reading routines
! (Sections to be added as reading routines become available
!  

! For any ATSR-type instrument:
!       Read MSI
!       Read cloud flags
!       Read land/sea flags
!       Read geometry
!       Read location data
!       Read scanline data
!       Calculate illumination data
!       Set MSI_files_open logical: on first call this is false, so the 
!       read routines will open the files. Set true so that no file open is 
!       attempted on subsequent calls.

 
   call Read_MSI(Ctrl, NSegs, SegSize, MSI_files_open, MSI_luns(1), &
           & MSI_Data, SAD_Chan, status)
      write(*,*) 'status readmsi data 1',status
      if (status == 0) call Read_CloudFlags(Ctrl, NSegs, SegSize, &
           & MSI_files_open, MSI_luns(2), MSI_Data, status)
      write(*,*) 'status readcf data 2',status
      if (status == 0) call Read_LSFlags(Ctrl, NSegs, SegSize, &
           & MSI_files_open, MSI_luns(3), MSI_Data, status)
      write(*,*) 'status readlsf data 4',status,MSI_files_open

      if (status == 0) call Read_Geometry(Ctrl, NSegs, SegSize, &
           & MSI_files_open, MSI_luns(4), MSI_Data, status)

      write(*,*) 'status readgeom data 3',status,MSI_files_open
      if (status == 0) call Read_Location(Ctrl, NSegs, SegSize, &
           & MSI_files_open, MSI_luns(5), MSI_Data, status)
      write(*,*) 'status readloc data 5',status,MSI_files_open

      if (status == 0) call Read_illum(Ctrl, NSegs, SegSize, &
           & MSI_files_open, MSI_luns(7), MSI_Data, status)
      write(*,*) 'status readillum data 7',status,MSI_files_open
      minsolzen=minval(MSI_Data%Geometry%Sol(:,:, :)) 

      write(*,*)'minsolzen',minsolzen,Ctrl%MaxSolzen

      if (minsolzen < Ctrl%MaxSolzen) then
        
         if (Ctrl%RS%Flag == SelmAux) then
            if (status == 0) call Read_Alb(Ctrl, NSegs, SegSize, &
                 MSI_files_open, MSI_luns(6), MSI_Data, status)
            write(*,*)'status readsat data 6',status,MSI_files_open
         endif
      endif

      if (status == 0) call Read_Scanlines(Ctrl, NSegs, SegSize, &
           MSI_files_open, MSI_luns(7), MSI_Data, status)
      write(*,*) 'status readsat data 7',status

      MSI_files_open = .true.
 

end subroutine Read_SatData
