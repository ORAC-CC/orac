!-------------------------------------------------------------------------------
! Name:
!    Determine_Illum
!
! Purpose:
!
! Arguments:
!    Name     Type    In/Out/Both Description
!    Ctrl     struct  In          Control structure
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
!     4th Feb 2015, Greg McGarragh:
!       Original version.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Determine_Illum(Ctrl, MSI_Data, verbose)

   use CTRL_def
   use ECP_Constants
   use Int_Routines_def, only : find_in_array

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data
   logical,      intent(in)    :: verbose

   ! Local variables

   integer :: i, j, k

   allocate(MSI_Data%illum(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax, Ctrl%Ind%NViews))
   MSI_Data%illum=byte_fill_value

   do i = 1,Ctrl%Ind%Xmax
      do j = 1,Ctrl%Ind%Ymax
         do k = 1,Ctrl%Ind%nviews

            ! Daytime
            if (MSI_Data%Geometry%Sol(i, j, k) .lt. Ctrl%MaxSolZen) then
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
