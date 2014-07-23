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
!20140131 MJ adds code for setting of AVHRR refch
!20140401 MJ rewrites routine partly to robustly set illumination
!20140403 Initialize illumination with fill value
!20140703 CP added in options for when only a single IR channel is present replace 0.o with missingvalue
!
! Bugs:
!
!
! $Id$
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
   integer        ::refch1,refch2,missing_vis,missing_ir,it
   real            :: minrad

   status=0

   allocate(MSI_Data%illum(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
   MSI_Data%illum=byte_fill_value

   !set channel number in instrument notation which can be used
   !for effective radius retrieval (could be made dynmic but set
   !static for each instrument here).
   !also set minimum radiance
   if ((trim(Ctrl%inst%name) .eq. trim('MODIS-AQUA')) .or. &
        & (trim(Ctrl%Inst%Name) .eq. trim('MODIS-TERRA'))) then
      minrad=0.00
      refch1=6
      refch2=20
   endif

   if   (trim(Ctrl%inst%name(1:5)) .eq. 'AVHRR') then
      minrad=0.00
      refch1=3
      refch2=4
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
!   write(*,*) minval(MSI_Data%MSI(:,:,:))
! write(*,*)'Ctrl%Ind%ysolar_msi',Ctrl%Ind%ysolar_msi(:)
!write(*,*)'Ctrl%Ind%ythermal_msi',Ctrl%Ind%ythermal_msi(:)
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
	missing_ir=0	
	missing_vis=0	
            do ic=1,Ctrl%Ind%Nsolar
               
               !check the ref channels
               if ((Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .eq. refch1) .or. (Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .eq. refch2)) then
                  if (MSI_Data%MSI(i, j,Ctrl%Ind%ysolar_msi(ic)) .le. minrad) then 
                     nref=nref+1
                     MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic))=MissingXn !0.0
                  end if
               end if
               
               !check the tau channels
               if ((Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .ne. refch1) .and. (Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .ne. refch2)) then
                  if (MSI_Data%MSI(i, j,Ctrl%Ind%ysolar_msi(ic)) .le. minrad) then 
                     nsbad=nsbad+1
	missing_vis=ic
                     MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic))=MissingXn !0.0
                  end if
               end if

	enddo


	it =1
            ntbad=0
            ! check the cloud top property channels
! check pure Ir channels
            do ic=1,Ctrl%Ind%Nthermal
               if (MSI_Data%MSI(i, j,Ctrl%Ind%ythermal_msi(ic)) .le. minrad) then
                  ntbad=ntbad+1
 missing_ir=it	
                  MSI_Data%MSI(i,j,Ctrl%Ind%ythermal_msi(ic))=MissingXn 
	
               end if
it=it+1
            end do

            !Determine now illumination conditions
            !based on solar illumination and amount of available channels
            !all sw (component) channels are there
            if(nref .eq. 0) then
               
               !sun high enough in the sky
               if(MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen .and. ntbad .eq. 0 .and. nsbad .eq. 0) then
                  
                  MSI_Data%Illum(i,j,view) = IDay

                  !sun close to sunset

		else if ((nsbad .eq. 1)   .and. (nref  .eq. 0) .and.&
                 & (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen))  then

		if (missing_vis .eq. 1 )  then 
			MSI_Data%Illum(i,j,view) = IDaysinglevisfirst
		endif
		if (missing_vis .eq. 2 )  then
			 MSI_Data%Illum(i,j,view) = IDaysinglevissecond
		endif
		
!
! a single ir channel missing good retrieval still possible
!


	    else if ((nsbad .eq. 0)   .and. (nref  .eq. 0) .and.&
                 & (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolzen) .and. ntbad .eq. 1)  then
		if (missing_ir .eq. 1 )  then 
			MSI_Data%Illum(i,j,view) = IDaysingleirfirst
		endif

		if (missing_ir .eq. 2 )  then
			 MSI_Data%Illum(i,j,view) = IDaysingleirsecond
		endif

		if (missing_ir .eq. 3 )  then
			 MSI_Data%Illum(i,j,view) = IDaysingleirthird
		endif

               else if((MSI_Data%Geometry%Sol(i, j, 1) .ge. Ctrl%MaxSolzen) &
                    & .and. (MSI_Data%Geometry%Sol(i, j, 1) .le. Ctrl%Sunset)) then
                  
                  MSI_Data%Illum(i,j,view) = ITwi

                  !sun below horizon
               elseif(( MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset)) then

                  MSI_Data%Illum(i,j,view) = INight

               endif
               
               !some solar channels gone only do night retrieval
            elseif(nsbad .gt. 0 .or. nref .gt. 0) then

               if((MSI_Data%Geometry%Sol(i, j, 1) .ge. Ctrl%MaxSolzen) &
                    & .and. (MSI_Data%Geometry%Sol(i, j, 1) .le. Ctrl%Sunset)) then

                  MSI_Data%Illum(i,j,view) = ITwi

                  !sun below horizon
               elseif(( MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset)) then

                  MSI_Data%Illum(i,j,view) = INight

                if (missing_ir .eq. 1 )  then 
			MSI_Data%Illum(i,j,view) = INightsingleirfirst
		endif

		if (missing_ir .eq. 2 )  then
			 MSI_Data%Illum(i,j,view) = INightsingleirsecond
		endif

		if (missing_ir .eq. 3 )  then
			 MSI_Data%Illum(i,j,view) = INightsingleirthird
		endif

               endif

            endif



!if (MSI_Data%MSI(i,j,4)  .lt. 1 .or. MSI_Data%MSI(i,j,5) .lt. 1) then
!   write(*,*) MSI_Data%MSI(i,j,:)
!            write(*,*) 'illum',nref,nsbad,ntbad,MSI_Data%Illum(i,j,view)
!	    write(*,*) 'illum missing',missing_ir,missing_vis
!endif
            !STOP


         end do !view
 
!if (MSI_Data%MSI(i,j,4)  .lt. 1 .or. MSI_Data%MSI(i,j,5) .lt. 1) then
!   write(*,*) 'aftewards'
!            write(*,*) 'nref nsbad ntbad illum status',nref,nsbad,ntbad,MSI_Data%Illum(i,j,1),status
!endif

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

!                write(*,*) 'illum msi_data', MSI_Data%Illum(:,:,1)

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
