! Name:
!   Read_illum
!
! Purpose:
!   Controls the reading of illumination data
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
!
! Local variables:
!   Name   Type       Description
!   ios    int        I/O status, file operations
!   lun    int        File unit number
!
! History:
!  18/06/2012 C. Poulsen original version
!  8/07/2012 C. Poulsen initialised ios value
!  14/06/2014 C. Poulsen modified to include scenarios where a single visible of a single Ir channel are missing
!
! Bugs:
!
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Read_Illum(Ctrl, NSegs, SegSize, MSI_files_open, lun, &
   MSI_Data, status)

   use CTRL_def
   use ECP_Constants
   use Data_def

   implicit none

!  Argument declarations

   type(CTRL_t), intent(inout)    :: Ctrl
   integer, intent(in)         :: NSegs     ! Number of segments read so far
   integer, intent(in)         :: SegSize   ! Size of segment to read
   logical, intent(in)         :: MSI_files_open
   integer, intent(inout)      :: lun       ! Unit number for MSI file   
   type(Data_t), intent(inout) :: MSI_Data
   integer, intent(out)        :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
   integer        :: view,i,j,ic,nsbad,ntbad,nref,it
   integer ::refch1,refch2,missing_vis,missing_ir
   real            :: minrad
 
   status=0

   if ((trim(Ctrl%Inst%Name) .eq. trim('MODIS-AQUA')) &
        & .or. (trim(Ctrl%Inst%Name) .eq. trim('MODIS-TERRA')) .or. &
        & (trim(Ctrl%Inst%Name) .eq. trim('AVHRR-NOAA18'))) then
      minrad=0.0   
   end if
   

   if (trim(Ctrl%Inst%Name) .eq. trim('AATSR'))  then
      minrad=0.00 !could be 0.001  
   end if
   
   allocate(MSI_Data%illum(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
   
   write(*,*)'Ctrl%Ind%Nsolar',Ctrl%Ind%Nsolar
   
   !  First ensure that the illumination is consistent in all iews
   ! loop over observations in y direction
   
   write(*,*)'read illum SegSize',SegSize
   
   !
   !loop over all channels and set values to zero where missing
   !Make sure that enough channels are present
   !
   ! identify channels that are needed to do effective radius retrieval

   if ((trim(Ctrl%inst%name) .eq. trim('MODIS-AQUA')) .or. &
        & (trim(Ctrl%Inst%Name) .eq. trim('MODIS-TERRA'))) then
      refch1=6
      refch2=19
   endif


   if   (trim(Ctrl%inst%name) .eq. 'AATSR') then
      refch1=4
      refch2=5
   endif
   
   
   do i = 1,Ctrl%Ind%Xmax
      do j = 1,Ctrl%Ind%Ymax
         do view = 1,Ctrl%Ind%NViews
            nref=0
            nsbad=0	
            do ic=1,Ctrl%Ind%NSolar	
               
               if ((Ctrl%Ind%Y_id(ic) .eq. refch1) .or. (Ctrl%Ind%Y_id(ic) .eq. refch2)) then
! count number of bad effectve radius channels
                  if (MSI_Data%MSI(i, j, &
                       & Ctrl%Ind%Chi(ic)) .le. minrad) then 
                     nref=nref+1
                     MSI_Data%MSI(i,j,Ctrl%Ind%Chi(ic))=0.0

                  end if
               end if
               
!count number of good visible reflectance channels i.e have value greater than minimum

               if (ic .le. 2) then
                  if (MSI_Data%MSI(i, j, &
                       Ctrl%Ind%Chi(ic)) .le. minrad) then 
                     nsbad=nsbad+1.0
		     missing_vis=ic	
                     MSI_Data%MSI(i,j,Ctrl%Ind%Chi(ic))=0.0
                     !if (ic .le. 2) then
                     !write(*,*)'after',MSI_Data%MSI(i, j,:)
                     !end if
                  end if
               end if
               
               
            end do !nsolar

            ntbad=0
            ! check all other channels
            ! count number of good thermal channels
		it =1
            do ic=Ctrl%Ind%NSolar+1,Ctrl%Ind%Navail	
            write(*,*)  'msi check1', MSI_Data%MSI(i, j,Ctrl%Ind%Chi(ic)),minrad
               if (MSI_Data%MSI(i, j, &
                    Ctrl%Ind%Chi(ic)) .le. minrad) then 
                  ntbad=ntbad+1.0
		  missing_ir=it		
                  MSI_Data%MSI(i,j,Ctrl%Ind%Chi(ic))=0.0
               end if
               it=it+1
            end do !nthermal
            
            !
            !make sure enough channels are present
            !
            
            
        write(*,*)'ntbad nsbad nref',ntbad, nsbad,nref    
            
            if ((nsbad  .eq. 0) .and.&
                 & (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen) .and. (nref  .eq. 0)  ) then
               MSI_Data%Illum(i,j,view) = IDay
            else if ((nsbad .eq. 1)   .and. (nref  .eq. 0) .and.&
                 & (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen))  then
		if (missing_vis .eq. 1 )  then 
			MSI_Data%Illum(i,j,view) = IDaysinglevisfirst
		endif
		if (missing_vis .eq. 2 )  then
			 MSI_Data%Illum(i,j,view) = IDaysinglevissecond
		endif

	    else if ((nsbad .eq. 0)   .and. (nref  .eq. 0) .and.&
                 & (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen) .and. ntbad .eq. 1)  then
		if (missing_ir .eq. 1 )  then 
			MSI_Data%Illum(i,j,view) = IDaysingleirfirst
		endif

		if (missing_ir .eq. 2 )  then
			 MSI_Data%Illum(i,j,view) = IDaysingleirsecond
		endif

            else if ((nsbad .ge. 0) .and. (nsbad .le. 3) &
                 & .and. (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%MaxSolzen) &
                 & .and. (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%Sunset)) then
               MSI_Data%Illum(i,j,view) = ITwi
            else
               MSI_Data%Illum(i,j,view) = INight
            end if

 write(*,*)'msi illum',MSI_Data%Illum(i,j,view),missing_ir,missing_vis 
!!!!!!!!!!!!!!!!!!!now caculate values for retrieval



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         end do !view
      end do !y
   end do !x

   write(*,*)'illum d'
!
   !these next lines are actually not applicable at the moment
!
   ios=0
   if (ios > 0) then
      status = illumFileReadDataErr ! Return error code
      write(unit=message, fmt=*) &
           'Read_illum: Error reading data in file ', Ctrl%Fid%illum
      call Write_Log(Ctrl, trim(message), status)

   else if (ios < 0) then
      
   end if



 end subroutine Read_illum
