!-------------------------------------------------------------------------------
! Name: determine_illum.F90
!
! Purpose:
! Set the illumination condition for each pixel.
!
! Description and Algorithm details:
! This considers only the solar zenith angle. The conditions are:
!    Day:      solzen < MaxSolZen
!    Twilight: MaxSolZen < solzen < Sunset
!    Night:    Sunset < solzen
! where MaxSolZen and Sunset are constants set in the Ctrl structure.
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct  In          Control structure
! MSI_Data struct  In          Data structure. Contains the multi-spectral
!                              image measurements, location values, geometry
!                              etc for the current image segment, from which
!                              the current SPixel values will be extracted.
!
! History:
! 2015/02/04, GM: Original version.
! 2015/08/04, GM: Check for solar zenith angle < SolZenMin.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Determine_Illum(Ctrl, MSI_Data)

   use Ctrl_m
   use Int_Routines_m, only : find_in_array
   use ORAC_Constants_m

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   ! Local variables

   integer :: i, j, k

   allocate(MSI_Data%illum(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NViews))
   MSI_Data%illum=byte_fill_value

   do i = 1,Ctrl%Ind%Xmax
      do j = 1,Ctrl%Ind%Ymax
         do k = 1,Ctrl%Ind%nviews

            ! Daytime
            if (MSI_Data%Geometry%Sol(i, j, k) .ge. SolZenMin .and. &
                MSI_Data%Geometry%Sol(i, j, k) .lt. Ctrl%MaxSolZen) then
               MSI_Data%Illum(i,j,k) = IDay

            ! Twilight
            else if (MSI_Data%Geometry%Sol(i, j, k) .ge. Ctrl%MaxSolZen .and. &
                     MSI_Data%Geometry%Sol(i, j, k) .le. Ctrl%Sunset) then
               MSI_Data%Illum(i,j,k) = ITwi

            ! Night time
            else if (MSI_Data%Geometry%Sol(i, j, k) .gt. Ctrl%Sunset) then
               ! Night time, all lw channels
               MSI_Data%Illum(i,j,k) = INight

            end if
         end do
      end do
   end do

end subroutine Determine_Illum
