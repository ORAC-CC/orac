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
!   array  real array Temporary array used to read in one row of values from
!                     the data file. Size (3, XMax) since there are 3 angle
!                     values at each x location within the image.
!   row    int        Image segment row counter. Used to check for premature
!                     end of file if EOF detected during segment read.
!
! History:
!  18/06/2012 C. Poulsen original version
!  8/07/2012 C. Poulsen initialised ios value
!  01/10/2012 C. Poulsen introduced new class i.e a day measurement with no effective radius channel
!             i.e 1.6 or 3.7 the pixel will not be processed in this case. So will make the code run faster
!   2013 MJ implements code for MODIS and AVHRR processing
!   24 Jul 2013 APovey: added MODIS-TERRA and AQUA as valid instruments
! 20131118 MJ fixes a number of problems with this subroutine:refch2 for modis is corrected from 19 to 20. ysolar_msi and ythermal_mis is now used in indexing the MSI array, as this gives the indices of the channels as they are stored in the MSI array.
!
! Bugs:
!
!
! $Id: ReadIllum.f90 182 2011-10-05 10:03:40Z cpoulsen $
!
!------------------------------------------------------------------------------------
subroutine Read_Illum_nc(Ctrl, NSegs, SegSize,&
   MSI_Data, status)

   use CTRL_def
   use ECP_Constants
   use Data_def

   implicit none

!  Argument declarations

   type(CTRL_t), intent(inout)    :: Ctrl
   integer, intent(in)         :: NSegs     ! Number of segments read so far
   integer, intent(in)         :: SegSize   ! Size of segment to read
   type(Data_t), intent(inout) :: MSI_Data
   integer, intent(out)        :: status

!  Local variables

   integer        :: ios       ! I/O status from file operations
   character(180) :: message   ! Error message to pass to Write_Log
!   real           :: array(3*Ctrl%Ind%NViews, Ctrl%Ind%XMax)  
                               ! Holds 1 row of data from the file
   integer        :: view,i,j,ic,nsbad,ntbad,nref!, icnew,jcount,navail,nsolar,nthermal,row
!   integer        ::  ii,jj,jin
   integer        ::refch1,refch2
   real            :: minrad

   status=0

   allocate(MSI_Data%illum(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))


   !set channel number in instrument notation which can be used
   !for effective radius retrieval (could be made dynmic but set
   !static for each instrument here).
   !also set minimum radiance
   if ((trim(Ctrl%inst%name) .eq. trim('MODIS-AQUA')) .or. &
        & (trim(Ctrl%Inst%Name) .eq. trim('MODIS-TERRA'))) then
      minrad=0.00
      refch1=6
      refch2=20
      write(*,*) 'went in here'
   endif


   if   (trim(Ctrl%inst%name) .eq. 'AATSR') then
      minrad=0.00
      refch1=4
      refch2=5
   endif
   !
   !loop over all channels and set values to zero where missing
   !Make sure that enough channels are present
   !  First ensure that the illumination is consistent in all iews
   ! loop over observations in y direction
   
!!$   write(*,*) '1'
!!$   write(*,*) minval(MSI_Data%MSI(:,:,1))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,2))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,3))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,4))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,5))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,6))

   do i = 1,Ctrl%Ind%Xmax
      do j = 1,Ctrl%Resoln%SegSize
         do view = 1,Ctrl%Ind%NViews
            nref=0
            nsbad=0
            do ic=1,Ctrl%Ind%Nsolar
               
               !check the ref channels
               if ((Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .eq. refch1) .or. (Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .eq. refch2)) then
                  if (MSI_Data%MSI(i, j,Ctrl%Ind%ysolar_msi(ic)) .le. minrad) then 
                     nref=nref+1
                     MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic))=0.0
                  end if
               end if
               
               !MJ ORG if (ic .le. 2) then
               !check the tau channels
               if ((Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .ne. refch1) .and. (Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .ne. refch2)) then
                  if (MSI_Data%MSI(i, j,Ctrl%Ind%ysolar_msi(ic)) .le. minrad) then 
                     nsbad=nsbad+1
                     MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic))=0.0
                  end if
               end if

            end do !nsolar

            ntbad=0
            ! check the cloud top property channels
            do ic=1,Ctrl%Ind%Nthermal
               if (MSI_Data%MSI(i, j,Ctrl%Ind%ythermal_msi(ic)) .le. minrad) then
                  ntbad=ntbad+1
                  MSI_Data%MSI(i,j,Ctrl%Ind%ythermal_msi(ic))=0.0
               end if
            end do

            !
            !make sure enough channels are present
            !
            !Day: information in solar channels for tau are there AND info in solar channels for ref are there
            !AND solar zenith angle is OK
            if ((nsbad  .le. 0) .and.&
                 & (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen) .and. (nref  .eq. 0)  ) then
               MSI_Data%Illum(i,j,view) = IDay
            !Twilight: if in more than one but less than 3 solar channels there is no information
            !AND if solar zenith is larger than maximum but less than 90 deg. (sunset)
            else if ((nsbad .gt. 1) .and. (nsbad .le. 3) &
                 & .and. (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%MaxSolzen) &
                 & .and. (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%Sunset)) then
               MSI_Data%Illum(i,j,view) = ITwi
            !Solar tau channels are present but no ref channel valid
            else if ((nsbad .eq. 0) .and. (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen) .and. (nref  .gt. 0) ) then
               MSI_Data%Illum(i,j,view) = IDaynore !but no effective radius channel i.e no 1.6 of 3.7 channel
               !MJ ORGMSI_Data%MSI(i, j,:)=0.0
            else
               MSI_Data%Illum(i,j,view) = INight
            end if


!MJ ORG
!!$            if ((nsbad  .le. 1) .and.&
!!$                 & (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen) .and. (nref  .eq. 0)  ) then
!!$               MSI_Data%Illum(i,j,view) = IDay
!!$            else if ((nsbad .ge. 0) .and. (nsbad .le. 3) &
!!$                 & .and. (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%MaxSolzen) &
!!$                 & .and. (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%Sunset)) then
!!$               MSI_Data%Illum(i,j,view) = ITwi
!!$            else
!!$               MSI_Data%Illum(i,j,view) = INight
!!$            end if


!!!!!!!!!!!!!!!!!!!now caculate values for retrieval



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !MJ write(*,*) 'nref,nsbad,ntbad',nref,nsbad,ntbad            


         end do !view
      end do !y
   end do !x

!!$   write(*,*)'minmax thermal channels 1',&
!!$        & minval(MSI_Data%MSI(:,:,Ctrl%Ind%ythermal_msi(1):Ctrl%Ind%ythermal_msi(Ctrl%Ind%Nthermal))),&
!!$        & maxval(MSI_Data%MSI(:,:,Ctrl%Ind%ythermal_msi(1):Ctrl%Ind%ythermal_msi(Ctrl%Ind%Nthermal)))
   !stop
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

!!$   write(*,*) '2'
!!$   write(*,*) minval(MSI_Data%MSI(:,:,1))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,2))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,3))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,4))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,5))
!!$   write(*,*) minval(MSI_Data%MSI(:,:,6))
!!$
!!$   stop

 end subroutine Read_Illum_nc
