! Name:
!    Nullify_Data
!
! Purpose:
!
! Arguments:
!    Name Type In/Out/Both Description
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    24th Oct 2001, Greg McGarragh:Original version
!
! Bugs:
!    None known.
!
! $Id: NullifyData.F90 2356 2014-09-10 20:42:15Z gmcgarragh $
!
!-------------------------------------------------------------------------------

subroutine Nullify_Data(Ctrl, MSI_Data)

   use Ctrl_def

   implicit none

   ! Declare arguments
   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   nullify(MSI_Data%ALB)

   if (Ctrl%RS%use_full_brdf) then
      nullify(MSI_Data%rho_0v)
      nullify(MSI_Data%rho_0d)
      nullify(MSI_Data%rho_dv)
      nullify(MSI_Data%rho_dd)
   endif

   nullify(MSI_Data%CloudFlags)

   nullify(MSI_Data%Geometry%Sol)
   nullify(MSI_Data%Geometry%Sat)
   nullify(MSI_Data%Geometry%Azi)

   nullify(MSI_Data%Location%Lat)
   nullify(MSI_Data%Location%Lon)

   nullify(MSI_Data%LSFlags)

   nullify(MSI_Data%time)

   nullify(MSI_Data%MSI)

   nullify(MSI_Data%Scan%uscan)
   nullify(MSI_Data%Scan%vscan)

   nullify(MSI_Data%illum)

end subroutine Nullify_Data
