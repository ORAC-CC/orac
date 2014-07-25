! Name:
!    Get_illum
!
! Purpose:
!    Determines the illumination of a scene by asseseing the visible and ir
!         channel values
!
! Arguments:
!    Name     Type         In/Out   Description
!    Ctrl     struct       In       Control structure
!    SPixel   struct       Both     Super-pixel structure
!    MSI_Data struct       In       Data structure. Contains the multi-spectral
!                                   image measurements, location values,
!                                   geometry etc for the current image segment,
!                                   from which the current SPixel values will 
!                                   be extracted.
!    status   integer      Out      Error status
!    
! Algorithm:
!
! Local variables:
!    Name      Type   Description
!    message   char   Message to pass to log file
!
! History:
!   15th June 2012, Caroline Poulsen: original version
!   17th July 2012, Caroline Poulsen: changed value 1 to nviews
!   16th Jan 2014, Greg McGarragh: Added initialization of
!      SPixel%spixel_y_to_ctrl_y_index.
!   11th June 2014 CP added in different illumination options that
!    maybe the result of occasional missing channels.
!
! Bugs:
!   Warning At the moment only one view is specified
!   Warning this routine has far to much dependance on heriatge channel selection
!   should be re wriiten if many more channels are used.   
!
! $Id$
!
!------------------------------------------------------------------------------
subroutine Get_illum(Ctrl, SPixel, MSI_Data, status)

    use CTRL_def
    use SPixel_def
    use Data_def
    use ECP_Constants

    implicit none

!   Define arguments

    type(CTRL_t), intent(in)      :: Ctrl
    type(SPixel_t), intent(inout) :: SPixel
    type(Data_t), intent(inout)      :: MSI_Data
    integer, intent(out)          :: status

!   Define local variables

!    character(180) :: message ! stapel: var not used
    integer        :: view,i,j!,nsbad,minrad,nviews,ic,nn
    integer        :: Illum(Ctrl%Ind%Ny,Ctrl%Ind%NViews)

    !   Set status to zero

    status = 0

!   Determine whether super pixel geometry corresponds to day, twilight or night 
!   and set up SPixel%Ind values accordingly. The ThermalFirst, ThermalLast etc
!   indices in SPixel%Ind are set relative to the Ctrl%Ind%Y array. Hence the
!   first thermal channel in Y which is to be used in the current super-pixel
!   is Ctrl%Ind%Y(SPixel%Ind%ThermalFirst).



!
!set illumination to night if no solar channels are present this enables an IR only retrieval during the day
!

	if (Ctrl%Ind%Nsolar .eq. 0 ) then 
	SPixel%Illum(1) = INight
	MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%YSeg0, view)= INight
	endif

!
!
!
if (MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0,4)  .lt. 1 .or. MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0,5) .lt. 1) then
!   write(*,*) 'illum ir missing ',MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0,:)
!write(*,*) 'illum value', SPixel%Illum(1)
endif


    do view=1,Ctrl%Ind%NViews
    SPixel%illum(view) = MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%YSeg0, view)



       !Daylight
       if (SPixel%illum(view)  .eq. 1) then 
          SPixel%Illum(view) = IDay

          !      These are a straight copy from the Ctrl values (i.e. all channels are allowed)
          SPixel%Ind%Ny = Ctrl%Ind%Ny
          SPixel%Ind%NSolar = Ctrl%Ind%NSolar
          SPixel%Ind%NThermal = Ctrl%Ind%NThermal
          SPixel%Ind%NMixed = Ctrl%Ind%NMixed !comes from readchan.f90
          SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
          SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast
          SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast
          
          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW
          SPixel%Ind%MDAD_SW = Ctrl%Ind%MDAD_SW

! This variable SPixel%spixel_y_to_ctrl_y_index(i) contains the channel information that should be used.
          do i = 1, Ctrl%Ind%Ny
             SPixel%spixel_y_to_ctrl_y_index(i) = i
          enddo

          SPixel%Nx = Ctrl%Ind%Nx_Dy
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)
          
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
          
          SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
          SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))



       	else if (SPixel%illum(view)  .eq. IDaysinglevisfirst) then 
          SPixel%Illum(view) = IDaysinglevisfirst
          
          !   a single visible channel is missing
          SPixel%Ind%Ny = Ctrl%Ind%Ny-1
          SPixel%Ind%NSolar = Ctrl%Ind%NSolar-1
          SPixel%Ind%NThermal = Ctrl%Ind%NThermal
          SPixel%Ind%NMixed = Ctrl%Ind%NMixed !comes from readchan.f90
!must select which one is missing
          SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst+1
          SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast

          SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast
          
          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW
          SPixel%Ind%MDAD_SW = Ctrl%Ind%MDAD_SW-1

          do i = 1, Ctrl%Ind%Ny-1
             SPixel%spixel_y_to_ctrl_y_index(i) =  Ctrl%Ind%SolarFirst +i
          enddo

          SPixel%Nx = Ctrl%Ind%Nx_Dy
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)
          
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
          
          SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
          SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))



          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
          
          SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
          SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))






       	else if (SPixel%illum(view)  .eq. IDaysinglevissecond) then 
          SPixel%Illum(view) = IDaysinglevissecond
          
          !   a single visible channel is missing
          SPixel%Ind%Ny = Ctrl%Ind%Ny-1
          SPixel%Ind%NSolar = Ctrl%Ind%NSolar-1
          SPixel%Ind%NThermal = Ctrl%Ind%NThermal
          SPixel%Ind%NMixed = Ctrl%Ind%NMixed !comes from readchan.f90
!must select which one is missing
          SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
          SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast-1

          SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast
          
          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW
          SPixel%Ind%MDAD_SW = Ctrl%Ind%MDAD_SW-1

          do i = 1, Ctrl%Ind%Ny-1
             SPixel%spixel_y_to_ctrl_y_index(i) =  Ctrl%Ind%SolarFirst +i
          enddo

          SPixel%Nx = Ctrl%Ind%Nx_Dy
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)
          
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
          
          SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
          SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))



          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
          
          SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
          SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))



       	else if (SPixel%illum(view)  .eq. IDaysingleirfirst) then 
          SPixel%Illum(view) = IDaysingleirfirst
          
          !   a single ir channel is missing
          SPixel%Ind%Ny = Ctrl%Ind%Ny-1
          SPixel%Ind%NSolar = Ctrl%Ind%NSolar
          SPixel%Ind%NThermal = Ctrl%Ind%NThermal-1
          SPixel%Ind%NMixed = Ctrl%Ind%NMixed !comes from readchan.f90
!must select which one is missing
          SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
          SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast

          SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst+1
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast
          
          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW-1 !(Ctrl%Ind%ThermalFirst+1:Ctrl%Ind%ThermalLast)
          SPixel%Ind%MDAD_SW = Ctrl%Ind%MDAD_SW 

          do i = 1, Ctrl%Ind%Ny-1
             if (i .le. Ctrl%Ind%NSolar ) then 
		 SPixel%spixel_y_to_ctrl_y_index(i) = i
	     endif
! skip the first ir channel
	     if (i .ge. Ctrl%Ind%ThermalFirst ) then
		  SPixel%spixel_y_to_ctrl_y_index(i) = Ctrl%Ind%ThermalFirst+1
	     endif	
          end do

          SPixel%Nx = Ctrl%Ind%Nx_Dy
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)
          
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
!currently assume  complete state vector          
          SPixel%FG = Ctrl%FG(:,IDay)
          SPixel%AP = Ctrl%AP(:,IDay)


       	else if (SPixel%illum(view)  .eq. IDaysingleirsecond) then 
          SPixel%Illum(view) = IDaysingleirsecond
          
          !   a single ir channel is missing
          SPixel%Ind%Ny = Ctrl%Ind%Ny-1
          SPixel%Ind%NSolar = Ctrl%Ind%NSolar
          SPixel%Ind%NThermal = Ctrl%Ind%NThermal-1
          SPixel%Ind%NMixed = Ctrl%Ind%NMixed !comes from readchan.f90
!must select which one is missing
          SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
          SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast

          SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast-1
 !
!does not work at the moment
!         index=[Ctrl%Ind%ThermalFirst,Ctrl%Ind%ThermalLast]
          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW-1
          SPixel%Ind%MDAD_SW = Ctrl%Ind%MDAD_SW

          do i = 1, Ctrl%Ind%Ny-1
             if (i .le. Ctrl%Ind%NSolar ) then 
		 SPixel%spixel_y_to_ctrl_y_index(i) = i
	     endif



!but not the first
	     if (i .eq. Ctrl%Ind%ThermalFirst ) then 
		 SPixel%spixel_y_to_ctrl_y_index(i) = Ctrl%Ind%ThermalFirst
	     endif
! skip the second ir channel 
	     if (i .eq. Ctrl%Ind%ThermalFirst+1 ) then 
		 SPixel%spixel_y_to_ctrl_y_index(i) = Ctrl%Ind%ThermalFirst+2
	     endif

          enddo
!	write(*,*)'irsingle third channel missing',SPixel%spixel_y_to_ctrl_y_index
          SPixel%Nx = Ctrl%Ind%Nx_Dy
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)
          
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
!currently assume  complete state vector                    
          SPixel%FG = Ctrl%FG(:,IDay)
          SPixel%AP = Ctrl%AP(:,IDay)





! could be thrid channel if 3.7 present genrally 12um chhannel is the first channel to saturate
       	else if (SPixel%illum(view)  .eq. IDaysingleirthird) then 

!write(*,*)'IDaysingleirthird',IDaysingleirthird
          SPixel%Illum(view) = IDaysingleirthird
          
          !   a single ir channel is missing
          SPixel%Ind%Ny = Ctrl%Ind%Ny-1
          SPixel%Ind%NSolar = Ctrl%Ind%NSolar
          SPixel%Ind%NThermal = Ctrl%Ind%NThermal-1
          SPixel%Ind%NMixed = Ctrl%Ind%NMixed !comes from readchan.f90
!must select which one is missing
          SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
          SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast

          SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast-1

          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW -1!(Ctrl%Ind%ThermalFirst:Ctrl%Ind%ThermalLast-1)
          SPixel%Ind%MDAD_SW = Ctrl%Ind%MDAD_SW
!write(*,*)'get illum Ctrl%Ind%MDAD_SW',Ctrl%Ind%MDAD_SW
!write(*,*)'get illum Ctrl%Ind%MDAD_LW',Ctrl%Ind%MDAD_LW


          do i = 1, Ctrl%Ind%Ny-1
             if (i .le. Ctrl%Ind%NSolar ) then 
		 SPixel%spixel_y_to_ctrl_y_index(i) = i
	     endif

!thermal channels
	     if (i .eq. Ctrl%Ind%ThermalFirst ) then 
		 SPixel%spixel_y_to_ctrl_y_index(i) = Ctrl%Ind%ThermalFirst
	     endif

	     if (i .eq. Ctrl%Ind%ThermalFirst+1 ) then 
		 SPixel%spixel_y_to_ctrl_y_index(i) = Ctrl%Ind%ThermalFirst+1
	     endif

	
          enddo

          SPixel%Nx = Ctrl%Ind%Nx_Dy
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)
          
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)
          
!currently assume  complete state vector                    
          SPixel%FG = Ctrl%FG(:,IDay)
          SPixel%AP = Ctrl%AP(:,IDay)

          !Twilight          
       else if  (SPixel%illum(view)  .eq. 2)  then
          
          SPixel%Illum(view) = ITwi
          
          !      Only pure thermal channels are allowed (i.e. mixed channels are excluded)   
          
          SPixel%Ind%Ny = Ctrl%Ind%Ny-Ctrl%Ind%NSolar
          SPixel%Ind%NSolar = 0
          SPixel%Ind%NThermal = SPixel%Ind%Ny
          SPixel%Ind%NMixed = 0
          SPixel%Ind%SolarFirst = 0
          SPixel%Ind%SolarLast = 0
          SPixel%Ind%ThermalFirst = Ctrl%Ind%SolarLast+1
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast
          
          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW - Ctrl%Ind%NSolar
          SPixel%Ind%MDAD_SW = 0

          do i = 1, Ctrl%Ind%Ny-Ctrl%Ind%NSolar
             SPixel%spixel_y_to_ctrl_y_index(i) = Ctrl%Ind%SolarLast + i
          enddo

          SPixel%Nx = Ctrl%Ind%Nx_Tw
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Tw(1:Ctrl%Ind%Nx_Tw)
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Tw(1:Ctrl%Ind%NxI_Tw)
          
          SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
          SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))

! add in night only options with missing channels !!TO BE COMPLETED!!
 else if (SPixel%illum(view)  .eq. Inightsingleirfirst) then 
status=1

else if (SPixel%illum(view)  .eq. Inightsingleirsecond) then 
status=1

else if (SPixel%illum(view)  .eq. Inightsingleirthird) then 
status=1

         
       else

          !nighttime
          SPixel%Illum(1) = INight

          !      Channels with a thermal component are allowed (i.e. mixed channels are included)
          SPixel%Ind%Ny = Ctrl%Ind%NThermal
          SPixel%Ind%NSolar = 0
          SPixel%Ind%NThermal = SPixel%Ind%Ny
          SPixel%Ind%NMixed = 0
          SPixel%Ind%SolarFirst = 0
          SPixel%Ind%SolarLast = 0
          SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
          SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast
          
          SPixel%Ind%MDAD_LW = Ctrl%Ind%MDAD_LW - Ctrl%Ind%NSolar 
          SPixel%Ind%MDAD_SW = 0

          do i = 1, Ctrl%Ind%NThermal
             SPixel%spixel_y_to_ctrl_y_index(i) = Ctrl%Ind%ThermalFirst + i - 1
          enddo

! make changes to state vector if necessary

          SPixel%Nx = Ctrl%Ind%Nx_Ni
          deallocate(SPixel%X)
          allocate(SPixel%X(SPixel%Nx))
          SPixel%X = Ctrl%Ind%X_Ni(1:Ctrl%Ind%Nx_Ni)
          
          SPixel%NxI = MaxStateVar - SPixel%Nx
          deallocate(SPixel%XI)
          allocate(SPixel%XI(SPixel%NxI))
          SPixel%XI = Ctrl%Ind%XI_Ni(1:Ctrl%Ind%NxI_Ni)
          

          SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
          SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))
       end if

       if (Ctrl%Ind%NViews > 1) then
          if (view > 1 .and. Illum(j,view) /= Illum(j,view)) then
             status = SPixelillum
          end if
       end if
    
    end do ! views

    !   Set the first guess and a priori state variable setting methods according 
    !   to the illumination conditions. 








end subroutine Get_Illum
