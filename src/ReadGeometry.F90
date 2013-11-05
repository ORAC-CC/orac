! Name:
!   Read_Geometry
!
! Purpose:
!   Controls the reading of geometric data from ATSR-type files into the
!   Data_Geometry array.
!
! Arguments:
!   Name     Type          In/Out/Both   Description
!   Ctrl     struct        Both          Control structure
!   NSegs    int           In            Number of image segments read in by 
!                                        previous calls to this routine.
!   SegSize  int           In            Size of image segment in rows of pixels.
!   MSI_files_open Logical In            Indicates whether the MSI data file is
!                                        open (if not, open it).
!   lun      int           In/Out        File unit number set by this routine
!                                        when file is opened, passed back and
!                                        forth for subsequent reads.
!   MSI_Data struct        In/Out        Data structure: the Geometry data part 
!                                        of this struct is populated by this
!                                        routine, and is overwritten on 
!                                        successive calls.
!   status   integer       Out           Error status         
!    
! Algorithm:
!   if (MSI files are not yet open)
!      Find a logical unit number to be used for the geometric data file
!      Open geom. file
!      If open error
!         Write error message to screen and log file
!      else
!         allocate geometry arrays in Data_MSI struct to size of image segment
!   
!   If (no error opening files)
!      Read header (not used further)
!      If read error
!         Write error message to screen and log file
!      Else
!         For the number of rows of pixels in the image segment
!            read a row of (sat, sol, azi) values (at all x for the current y)
!               into a temporary array (size 3 by Nx)
!            load the row values into the MSI_Data Sat, Sol and Azi arrays
!            If read error
!               Write error message to log file
!   (Leave geom. file open for later reads)
!
! Local variables:
!   Name   Type       Description
!   ios    int        I/O status, file operations
!   lun    int        File unit number
!   array  real array Temporary array used to read in one row of values from
!                     the data file. Size (3, XMax) since there are 3 angle
!                     values at each x location within the image.
!   row    int        Image segment row counter. Used to check for premature
!                     end of file if EOF detected during segment read.
!
! History:
!   3rd November, 2000, Kevin M. Smith : original version
!   23rd November, 2000, KMS: Added status to argument list
!   19th December, 2000, KMS: Replaced Data_Geometry_... arrays with Data
!                             structure
!   31st July 2001, Andy Smith:
!      Changes to cope with image segmentation. 
!      - Data storage changed from whole arrays of sol, sat, azi for all x,y 
!        to all three values for each x,y.
!      - new arguments MSI_files_open, lun (since the file now stays open for
!        repeated read operations)
!      - file open depends on MSI_files_open flag
!      Structure Data renamed MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).
!      Added argument intent specifiers.
!   10th Aug 2001, Andy Smith:
!      Updated to handle image segments/super-pixels of any size. Requires
!      handling of end of file during read on the last segment. On earlier 
!      segments EOF is reported as an error.
!   22nd Aug 2001, Andy Smith:
!      Bug fix: added status check before reading arrays in.
!   14th Apr 2011, Andy Smith:
!      Extension to handle multiple views. Geometry struct extended to 
!      take Sat, Sol and Azi data from >1 view (forward,, nadir etc).
!      Extra dimension in array allocations. Extended read loop to cover views. 
!   20th May 2011, Andy Smith:
!      Extension to multiple views, part 2. Use number of views from the inst 
!      config file to set the input array size. The geo file should always contain
!      data for all views but the user might select between 1 and the max number!   28th June 2011 Caroline poulsen remove reference to ATSR
!   28th Nov 2011 Caroline poulsen ninstviews to nviews
!
! Bugs:
!   None known.
!
! $Id: ReadGeometry.f90 182 2011-10-05 10:03:40Z carnold $
!
!------------------------------------------------------------------------------------
subroutine Read_Geometry(Ctrl, NSegs, SegSize, MSI_files_open, lun, &
   MSI_Data, status)

   use CTRL_def
   use ECP_Constants
   use Data_def

   implicit none

!  Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer, intent(in)         :: NSegs     ! Number of segments read so far
   integer, intent(in)         :: SegSize   ! Size of segment to read
   logical, intent(in)         :: MSI_files_open
   integer, intent(inout)      :: lun       ! Unit number for MSI file   
   type(Data_t), intent(inout) :: MSI_Data
   integer, intent(out)        :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(800) :: message   ! Error message to pass to Write_Log
   real           :: array(3*Ctrl%Ind%NViews, Ctrl%Ind%XMax)  
                               ! Holds 1 row of data from the file
   integer        :: row, view


!  On first call, the file is opened. It is then left open for all subsequent
!  calls.

   status = 0
   if (.not. MSI_files_open) then
      !     Call Find_LUN to set suitable unit number for geometric data file

      call Find_LUN(lun)

!     Open geometric data file
      !write(*,*) status
      !pause
      open(unit=lun, file=Ctrl%Fid%Geo, form='unformatted', status='old', &
           & iostat=ios)
      !write(*,*) 'this open',Ctrl%Fid%Geo,ios
      if (ios /= 0) then
         status = GeomFileOpenErr ! Return error code
         write(unit=message, fmt=*) &
              & 'Read_Geometry: Error opening file ', trim(adjustl(Ctrl%Fid%Geo))
         call Write_Log(Ctrl, trim(adjustl(message)), status)
      else
         !        Open successful. Allocate MSI_Data%Geometry and temporary array sizes

         allocate(MSI_Data%Geometry%Sol(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
         allocate(MSI_Data%Geometry%Sat(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
         allocate(MSI_Data%Geometry%Azi(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
         read(unit=lun, iostat=ios) ! Disregard header content
         !write(*,*) 'this header', ios
         if (ios /= 0) then
            status = GeomFileReadHeadErr ! Return error code
            write(unit=message, fmt=*) &
	  	'Read_Geometry: Error reading header of file ', Ctrl%Fid%Geo
            call Write_Log(Ctrl, trim(message), status)
         end if
      end if
   end if
   
   !  Read geometric data file (solar zenith, sat. zenith and azimuth data)
   !  Read the angles in 1 row at a time, al 3 angles for each x value.
   
   !write(*,*) status, ios
   !pause
   if (status == 0) then
      do row=1,SegSize 
         read(unit=lun, iostat=ios) array 
         if (ios /= 0) exit ! Drop out of the loop on read error
         
         do view = 1, Ctrl%Ind%NViews
            MSI_Data%Geometry%Sol(:,row, view) = array(((view-1)*3)+1,:)
            MSI_Data%Geometry%Sat(:,row, view) = array(((view-1)*3)+2,:)
            MSI_Data%Geometry%Azi(:,row, view) = array(((view-1)*3)+3,:)
         end do
      end do
      
      
      
      
      !     ios > 0 indicates a read error.
      !     ios < 0 indicates end of record or end of file. EOF is ok if it's in
      !     the right place (in order to allow any super-pixel size, we must
      !     deal with EOF part way through reading a segment).
      
      if (ios > 0) then
         status = GeomFileReadDataErr ! Return error code
         write(unit=message, fmt=*) &
              & 'Read_Geometry: Error reading data in file ', Ctrl%Fid%Geo
         call Write_Log(Ctrl, trim(message), status)

      else if (ios < 0) then
   
         !        Check for premature end of file during read. Compare the current image
         !        row count to the total expected.

         row = row + NSegs * SegSize
         if (row /= Ctrl%Ind%YMax+1) then
            status = GeomFileEOFErr
            write(unit=message, fmt=*) &
                 & 'Read_Geometry: End of file during read: file ', Ctrl%Fid%Geo
            call Write_Log(Ctrl, trim(message), status)
         end if
      end if
   end if

   !  Geometry file is not closed. Leave open for subsequent read of the rest 
   !  of the image.

 end subroutine Read_Geometry
