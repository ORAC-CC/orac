!-------------------------------------------------------------------------------
! Name:
!    Get_Illum
!
! Purpose:
!    Determines the illumination of a scene by asseseing the visible and ir
!    channel values
!
! Arguments:
!    Name     Type    In/Out/Both Description
!    Ctrl     struct  In          Control structure
!    SPixel   struct  Both        Super-pixel structure
!    MSI_Data struct  In          Data structure. Contains the multi-spectral
!                                 image measurements, location values, geometry
!                                 etc for the current image segment, from which
!                                 the current SPixel values will be extracted.
!    status   integer Out         Error status
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    15th Jun 2012, Caroline Poulsen: Original version
!    17th Jul 2012, Caroline Poulsen: Changed value 1 to nviews
!    16th Jan 2014, Greg McGarragh: Added initialization of
!       SPixel%spixel_y_to_ctrl_y_index.
!    11th Jun 2014, Caroline Poulsen: Added in different illumination options
!       that maybe the result of occasional missing channels.
!     1st Aug 2014, Greg McGarragh: Fixes and refactoring related to the above
!       change and cleanup.
!
! Bugs:
!    Warning At the moment only one view is specified.
!    Warning this routine has far to much dependance on heriatge channel
!    selection should be re wriiten if many more channels are used.
!
! $Id$
!
!-------------------------------------------------------------------------------

module Get_Illum_m

   implicit none

   private

   public :: Get_Illum

contains

subroutine Get_Illum(Ctrl, SAD_Chan, SPixel, MSI_Data, status)

   use CTRL_def
   use Data_def
   use ECP_Constants
   use SAD_Chan_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(SPixel_t),   intent(inout) :: SPixel
   type(Data_t),     intent(in)    :: MSI_Data
   integer,          intent(out)   :: status

   ! Define local variables

   integer :: i_view,i,ii
   integer :: i_missing(MaxNumMeas)

   ! Set status to zero
   status = 0

   ! Determine whether super pixel geometry corresponds to day, twilight or
   ! night and set up SPixel%Ind values accordingly. The ThermalFirst,
   ! ThermalLast etc indices in SPixel%Ind are set relative to the Ctrl%Ind%Y
   ! array. Hence the first thermal channel in Y which is to be used in the
   ! current super-pixel is Ctrl%Ind%Y(SPixel%Ind%ThermalFirst).

   do i_view=1,Ctrl%Ind%NViews
      SPixel%illum(i_view) = MSI_Data%illum(SPixel%Loc%X0, SPixel%Loc%YSeg0, i_view)

      ! Daylight
      if (SPixel%illum(i_view) .eq. IDay) then
         ! Daylight

         ! These are a straight copy from the Ctrl values (i.e. all channels are allowed)
         SPixel%Ind%Ny = Ctrl%Ind%Ny
         SPixel%Ind%NSolar = Ctrl%Ind%NSolar
         SPixel%Ind%NThermal = Ctrl%Ind%NThermal
         SPixel%Ind%NMixed = Ctrl%Ind%NMixed
         SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
         SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast
         SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast

         SPixel%Nx = Ctrl%Ind%Nx_Dy
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)

         i_missing = -1

         SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
         SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))

      else if (SPixel%illum(i_view) .eq. IDayMissingSingleVisFirst) then
         status = 1 ! Disable this configuration

         ! The first visible channel is missing
         SPixel%Ind%Ny = Ctrl%Ind%Ny-1
         SPixel%Ind%NSolar = Ctrl%Ind%NSolar-1
         SPixel%Ind%NThermal = Ctrl%Ind%NThermal
         SPixel%Ind%NMixed = Ctrl%Ind%NMixed
         SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst+1
         SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast
         SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast

         SPixel%Nx = Ctrl%Ind%Nx_Dy
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)

         i_missing = -1
         i_missing(1) =  1

         SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
         SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))

      else if (SPixel%illum(i_view) .eq. IDayMissingSingleVisSecond) then
         status = 1 ! Disable this configuration

         ! The second visible channel is missing
         SPixel%Ind%Ny = Ctrl%Ind%Ny-1
         SPixel%Ind%NSolar = Ctrl%Ind%NSolar-1
         SPixel%Ind%NThermal = Ctrl%Ind%NThermal
         SPixel%Ind%NMixed = Ctrl%Ind%NMixed
         SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
         SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast-1
         SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast

         SPixel%Nx = Ctrl%Ind%Nx_Dy
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)

         i_missing = -1
         i_missing(1) =  2

         SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
         SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))

      else if (SPixel%illum(i_view) .eq. IDayMissingSingleIRFirst) then
         status = 1 ! Disable this configuration

         ! The first ir channel is missing
         SPixel%Ind%Ny = Ctrl%Ind%Ny-1
         SPixel%Ind%NSolar = Ctrl%Ind%NSolar
         SPixel%Ind%NThermal = Ctrl%Ind%NThermal-1
         SPixel%Ind%NMixed = Ctrl%Ind%NMixed
         SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
         SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast
         SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst+1
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast

         SPixel%Nx = Ctrl%Ind%Nx_Dy
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)

         i_missing = -1
         i_missing(1) =  Ctrl%Ind%ThermalFirst

         SPixel%FG = Ctrl%FG(:,IDay)
         SPixel%AP = Ctrl%AP(:,IDay)

      else if (SPixel%illum(i_view) .eq. IDayMissingSingleIRSecond) then
         status = 1 ! Disable this configuration

         !  The second ir channel is missing
         SPixel%Ind%Ny = Ctrl%Ind%Ny-1
         SPixel%Ind%NSolar = Ctrl%Ind%NSolar
         SPixel%Ind%NThermal = Ctrl%Ind%NThermal-1
         SPixel%Ind%NMixed = Ctrl%Ind%NMixed
         SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
         SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast
         SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast-1

         SPixel%Nx = Ctrl%Ind%Nx_Dy
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)

         i_missing = -1
         i_missing(1) =  Ctrl%Ind%ThermalFirst + 1

         SPixel%FG = Ctrl%FG(:,IDay)
         SPixel%AP = Ctrl%AP(:,IDay)

      else if (SPixel%illum(i_view) .eq. IDayMissingSingleIRThird) then
         ! The third ir channel is missing
         SPixel%Ind%Ny = Ctrl%Ind%Ny-1
         SPixel%Ind%NSolar = Ctrl%Ind%NSolar
         SPixel%Ind%NThermal = Ctrl%Ind%NThermal-1
         SPixel%Ind%NMixed = Ctrl%Ind%NMixed
         SPixel%Ind%SolarFirst = Ctrl%Ind%SolarFirst
         SPixel%Ind%SolarLast = Ctrl%Ind%SolarLast
         SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast-1

         SPixel%Nx = Ctrl%Ind%Nx_Dy
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Dy(1:Ctrl%Ind%NX_Dy)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Dy(1:Ctrl%Ind%NXI_Dy)

         i_missing = -1
         i_missing(1) =  Ctrl%Ind%ThermalFirst + 2

         SPixel%FG = Ctrl%FG(:,IDay)
         SPixel%AP = Ctrl%AP(:,IDay)

      else if  (SPixel%illum(i_view) .eq. ITwi)  then
         ! Twilight

         ! Only pure thermal channels are allowed (i.e. mixed channels are excluded)

         SPixel%Ind%Ny = Ctrl%Ind%Ny-Ctrl%Ind%NSolar
         SPixel%Ind%NSolar = 0
         SPixel%Ind%NThermal = SPixel%Ind%Ny
         SPixel%Ind%NMixed = 0
         SPixel%Ind%SolarFirst = 0
         SPixel%Ind%SolarLast = 0
         SPixel%Ind%ThermalFirst = Ctrl%Ind%SolarLast+1
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast

         SPixel%Nx = Ctrl%Ind%Nx_Tw
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Tw(1:Ctrl%Ind%Nx_Tw)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Tw(1:Ctrl%Ind%NxI_Tw)

         ii = 1
         i_missing = -1
         do i = 1, Ctrl%Ind%NSolar
            i_missing(ii) = i
            ii = ii + 1
         enddo

         SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
         SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))

      else if (SPixel%illum(i_view) .eq. INightMissingSingleIRFirst) then
          status = 1 ! Disable this configuration

      else if (SPixel%illum(i_view) .eq. INightMissingSingleIRSecond) then
          status = 1 ! Disable this configuration

      else if (SPixel%illum(i_view) .eq. INightMissingSingleIRThird) then
          status = 1 ! Disable this configuration

      else if (SPixel%illum(i_view) .eq. INight) then
         ! Night

         ! Channels with a thermal component are allowed (i.e. mixed channels are included)
         SPixel%Ind%Ny = Ctrl%Ind%NThermal
         SPixel%Ind%NSolar = 0
         SPixel%Ind%NThermal = SPixel%Ind%Ny
         SPixel%Ind%NMixed = 0
         SPixel%Ind%SolarFirst = 0
         SPixel%Ind%SolarLast = 0
         SPixel%Ind%ThermalFirst = Ctrl%Ind%ThermalFirst
         SPixel%Ind%ThermalLast = Ctrl%Ind%ThermalLast

         SPixel%Nx = Ctrl%Ind%Nx_Ni
         deallocate(SPixel%X)
         allocate(SPixel%X(SPixel%Nx))
         SPixel%X = Ctrl%Ind%X_Ni(1:Ctrl%Ind%Nx_Ni)

         SPixel%NxI = MaxStateVar - SPixel%Nx
         deallocate(SPixel%XI)
         allocate(SPixel%XI(SPixel%NxI))
         SPixel%XI = Ctrl%Ind%XI_Ni(1:Ctrl%Ind%NxI_Ni)

         ii = 1
         i_missing = -1
         do i = 1, Ctrl%Ind%NSolar - Ctrl%Ind%NMixed
            i_missing(ii) = i
            ii = ii + 1
         enddo

         SPixel%FG = Ctrl%FG(:,SPixel%Illum(1))
         SPixel%AP = Ctrl%AP(:,SPixel%Illum(1))
      else
          status = 1
      end if

      if (status .eq. 0) then
         call setup_indexes(Ctrl, SPixel, i_missing)

         SPixel%Ind%MDAD_SW = Find_MDAD_SW(SPixel%Ind%Ny, SAD_Chan, &
            SPixel%spixel_y_to_ctrl_y_index)
         SPixel%Ind%MDAD_LW = Find_MDAD_LW(SPixel%Ind%Ny, SAD_Chan, &
            SPixel%spixel_y_to_ctrl_y_index)
      endif

      if (i_view > 1 .and. SPixel%Illum(i_view - 1) /= SPixel%Illum(i_view)) then
         status = SPixelIllum
      end if
   end do

end subroutine Get_Illum


subroutine setup_indexes(Ctrl, SPixel, i_missing)

   use CTRL_def
   use SPixel_def

   implicit none

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel
   integer,        intent(in)    :: i_missing(:)

   integer :: i
   integer :: ii

   SPixel%spixel_y_to_ctrl_y_index                 = -1
   SPixel%spixel_y_solar_to_ctrl_y_index           = -1
   SPixel%spixel_y_thermal_to_ctrl_y_index         = -1
   SPixel%spixel_y_solar_to_ctrl_y_solar_index     = -1
   SPixel%spixel_y_thermal_to_ctrl_y_thermal_index = -1

   ii = 1
   do i = 1, Ctrl%Ind%Ny
      if (any(i .eq. i_missing)) &
         cycle
      SPixel%spixel_y_to_ctrl_y_index(ii) = i
      ii = ii + 1
   enddo

   if (SPixel%Ind%NSolar .gt. 0) then
      ii = 1
      do i = 1, Ctrl%Ind%NSolar
         if (any(i .eq. i_missing)) &
            cycle
         SPixel%spixel_y_solar_to_ctrl_y_index(ii) = i
         ii = ii + 1
      enddo
   endif

   if (SPixel%Ind%NThermal .gt. 0) then
      ii = 1
      do i = Ctrl%Ind%ThermalFirst, Ctrl%Ind%Ny
         if (any(i .eq. i_missing)) &
            cycle
         SPixel%spixel_y_thermal_to_ctrl_y_index(ii) = i
         ii = ii + 1
      enddo
   endif

   if (SPixel%Ind%NSolar .gt. 0) then
      ii = 1
      do i = 1, Ctrl%Ind%NSolar
         if (any(i .eq. i_missing)) &
            cycle
         SPixel%spixel_y_solar_to_ctrl_y_solar_index(ii) = i
         ii = ii + 1
      enddo
   endif

   if (SPixel%Ind%NThermal .gt. 0) then
      ii = 1
      do i = Ctrl%Ind%ThermalFirst, Ctrl%Ind%Ny
         if (any(i .eq. i_missing)) &
            cycle
         SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(ii) = i - Ctrl%Ind%ThermalFirst + 1
         ii = ii + 1
      enddo
   endif

end subroutine setup_indexes

end module Get_Illum_m
